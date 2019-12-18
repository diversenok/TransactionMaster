unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.Graphics,
  NtUtils.Exceptions, Vcl.ComCtrls, VclEx.ListView, Vcl.AppEvnts, Vcl.ExtCtrls,
  NtUiLib.HysteresisList, NtUtils.Objects.Snapshots;

type
  TFormMain = class(TForm)
    lvActiveTmTx: TListViewEx;
    lblActive: TLabel;
    btnNewTmTx: TButton;
    lvHandles: TListViewEx;
    lblProcesses: TLabel;
    ApplicationEvents: TApplicationEvents;
    btnTransact: TButton;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnNewTmTxClick(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure lvActiveTmTxDblClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvHandlesDblClick(Sender: TObject);
  private
    ActiveTransctions: THysteresisList<TGuid>;
    Consumers: THysteresisList<TSystemHandleEntry>;
    TmTxTypeIndex: NativeUInt;
    IsFirstUpdate: Boolean;
    procedure ForceTimer;
    procedure FillTransactionInfo(const Item: TGuid; Index: Integer);
    procedure AddMissingProcessIcons;
    procedure OnActiveAddStart(const Item: TGuid; Index: Integer);
    procedure OnActiveAddFinish(const Item: TGuid; Index: Integer);
    procedure OnActiveRemoveStart(const Item: TGuid; Index: Integer);
    procedure OnActiveRemoveFinish(const Item: TGuid; Index: Integer);
    procedure OnConsumerAddStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure OnConsumerAddFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure OnConsumerRemoveStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure OnConsumerRemoveFinish(const Item: TSystemHandleEntry; Index: Integer);
  end;

var
  FormMain: TFormMain;

implementation

uses
  Ntapi.nttmapi, NtUiLib.Exceptions, NtUtils.Transactions, NtUtils.Objects,
  Ntapi.ntobapi, NtUtils.Access, DelphiUtils.Strings, DelphiUtils.Arrays,
  NtUtils.Processes, NtUtils.Processes.Snapshots, NtUiLib.Icons,
  TransactionInfo;

{$R *.dfm}

function CompareGuids(const A, B: TGuid): Boolean;
begin
  Result := (A = B);
end;

function CompareHandleEntries(const A, B: TSystemHandleEntry): Boolean;
begin
  Result := (A.UniqueProcessId = B.UniqueProcessId) and
    (A.HandleValue = B.HandleValue) and (A.GrantedAccess = B.GrantedAccess);
end;

procedure TFormMain.AddMissingProcessIcons;
var
  Processes: TArray<TProcessEntry>;
  FileName: String;
  Entry: PProcessEntry;
  i: Integer;
begin
  if not NtxEnumerateProcesses(Processes).IsSuccess then
    SetLength(Processes, 0);

  for i := 0 to Consumers.Count - 1 do
    if hdAddStart in Consumers[i].BelongsToDelta then
    begin
      Entry := NtxFindProcessById(Processes, Consumers[i].Data.UniqueProcessId);
      FileName := NtxTryQueryImageProcessById(Consumers[i].Data.UniqueProcessId);

      if Assigned(Entry) then
        lvHandles.Items[i].Cell[0] := Entry.ImageName
      else if FileName <> '' then
        lvHandles.Items[i].Cell[0] := ExtractFileName(FileName)
      else
        lvHandles.Items[i].Cell[0] := 'Unknown';

      lvHandles.Items[i].ImageIndex := TProcessIcons.GetIcon(FileName);
    end;
end;

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  ShowNtxException(Self.Handle, E);
end;

procedure TFormMain.btnNewTmTxClick(Sender: TObject);
var
  hxTransaction: IHandle;
  Description: String;
begin
  Description := 'TM Experiment #' + IntToStr(Random(1000));

  if InputQuery('Create a new transaction', 'Description (optional):',
    Description) then
  begin
    NtxCreateTransaction(hxTransaction, Description).RaiseOnError;
    TFormInfo.CreateDlg(Self, hxTransaction);
    ForceTimer;
  end;
end;

procedure TFormMain.FillTransactionInfo(const Item: TGuid; Index: Integer);
var
  hxTransaction: IHandle;
  Properties: TTransactionProperties;
  Info: TObjectBasicInformaion;
begin
  // TODO: show error messages as hints

  if NtxOpenTransactionById(hxTransaction, Item,
    TRANSACTION_QUERY_INFORMATION).IsSuccess then
    with lvActiveTmTx.Items[Index] do
    begin
      // Get transaction description
      if NtxQueryPropertiesTransaction(hxTransaction.Value,
        Properties).IsSuccess then
        Cell[1] := Properties.Description;

      // Get handle count (excluding ours)
      if  NtxQueryBasicInfoObject(hxTransaction.Value, Info).IsSuccess then
        Cell[2] := IntToStr(Info.HandleCount - 1);
    end;
end;

procedure TFormMain.ForceTimer;
begin
  UpdateTimer.Enabled := False;
  UpdateTimerTimer(Self);
  UpdateTimer.Enabled := True;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ActiveTransctions.Free;
  Consumers.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  ObjTypes: TArray<TObjectTypeInfo>;
  i: Integer;
begin
  ActiveTransctions := THysteresisList<TGuid>.Create(CompareGuids, 3);
  ActiveTransctions.OnAddStart := OnActiveAddStart;
  ActiveTransctions.OnAddFinish := OnActiveAddFinish;
  ActiveTransctions.OnRemoveStart := OnActiveRemoveStart;
  ActiveTransctions.OnRemoveFinish := OnActiveRemoveFinish;

  Consumers := THysteresisList<TSystemHandleEntry>.Create(CompareHandleEntries,
    3);
  Consumers.OnAddStart := OnConsumerAddStart;
  Consumers.OnAddFinish := OnConsumerAddFinish;
  Consumers.OnRemoveStart := OnConsumerRemoveStart;
  Consumers.OnRemoveFinish := OnConsumerRemoveFinish;

  lvHandles.SmallImages := TProcessIcons.ImageList;

  // Try to obtain an index of a transaction type
  if NtxEnumerateTypes(ObjTypes).IsSuccess then
    for i := 0 to High(ObjTypes) do
      if ObjTypes[i].TypeName = 'TmTx' then
      begin
        TmTxTypeIndex := ObjTypes[i].Other.TypeIndex;
        Break;
      end;

  // Type enumeration should not fail, but if it does, at least fall back to
  // a hardcoded value...
  if TmTxTypeIndex = 0 then
    TmTxTypeIndex := 39;

  IsFirstUpdate := True;
  UpdateTimerTimer(Sender);
end;

procedure TFormMain.lvActiveTmTxDblClick(Sender: TObject);
var
  hxTransaction: IHandle;
begin
  if not Assigned(lvActiveTmTx.Selected) then
    Exit;

  NtxOpenTransactionById(hxTransaction, ActiveTransctions[
    lvActiveTmTx.Selected.Index].Data, MAXIMUM_ALLOWED).RaiseOnError;

  TFormInfo.CreateDlg(Self, hxTransaction);
end;

procedure TFormMain.lvHandlesDblClick(Sender: TObject);
begin
  if not Assigned(lvHandles.Selected) then
    Exit;

  // TODO: show process information dialog
end;

procedure TFormMain.OnActiveAddFinish(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items[Index].ColorEnabled := False;
end;

procedure TFormMain.OnActiveAddStart(const Item: TGuid; Index: Integer);
begin
  with lvActiveTmTx.Items.Add do
  begin
    Caption := Item.ToString;
    FillTransactionInfo(Item, Index);

    if not IsFirstUpdate then
      Color := clLime;
  end;
end;

procedure TFormMain.OnActiveRemoveFinish(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items.Delete(Index);
end;

procedure TFormMain.OnActiveRemoveStart(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items[Index].Selected := False;
  lvActiveTmTx.Items[Index].Color := clRed;
end;

procedure TFormMain.OnConsumerAddFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items[Index].ColorEnabled := False;
end;

procedure TFormMain.OnConsumerAddStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  with lvHandles.Items.Add do
  begin
    Cell[1] := IntToStr(Item.UniqueProcessId);
    Cell[2] := IntToHexEx(Item.HandleValue);
    Cell[3] := FormatAccess(Item.GrantedAccess, @TmTxAccessType);

    if not IsFirstUpdate then
      Color := clLime;
  end;
end;

procedure TFormMain.OnConsumerRemoveFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items.Delete(Index);
end;

procedure TFormMain.OnConsumerRemoveStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items[Index].Selected := False;
  lvHandles.Items[Index].Color := clRed;
end;

procedure TFormMain.UpdateTimerTimer(Sender: TObject);
var
  Guids: TArray<TGuid>;
  Handles: TArray<TSystemHandleEntry>;
  i: Integer;
begin
  if WindowState = wsMinimized then
    Exit;

  // Snapshot active transactions
  if not NtxEnumerateTransactions(Guids).IsSuccess then
    SetLength(Guids, 0);

  lvActiveTmTx.Items.BeginUpdate;
  begin
    // Process the snapshot
    ActiveTransctions.Update(Guids);

    // Update info in changable columns for alive items
   for i := 0 to ActiveTransctions.Count - 1 do
     if ActiveTransctions[i].State <> hisDeleted then
       FillTransactionInfo(ActiveTransctions[i].Data, i);
  end;
  lvActiveTmTx.Items.EndUpdate;

  // Snapshot system handles, filter transactions only
  if NtxEnumerateHandles(Handles).IsSuccess then
    TArrayHelper.Filter<TSystemHandleEntry>(Handles, FilterByType,
      TmTxTypeIndex)
  else
    SetLength(Handles, 0);

  lvHandles.Items.BeginUpdate;
  begin
    // Process the snapshot
    Consumers.Update(Handles);

    // If new items arrived, update their process names and icons
    if Consumers.AddStartDelta > 0 then
      AddMissingProcessIcons;
  end;
  lvHandles.Items.EndUpdate;

  IsFirstUpdate := False;
end;

end.
