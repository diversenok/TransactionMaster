unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.Graphics,
  NtUtils.Exceptions, Vcl.ComCtrls, VclEx.ListView, Vcl.AppEvnts, Vcl.ExtCtrls,
  NtUiLib.HysteresisList, NtUtils.Objects.Snapshots, DelphiUtils.Events,
  NtUtils.Processes.Snapshots;

type
  TFormMain = class(TForm)
    appEvents: TApplicationEvents;
    btnNewTmTx: TButton;
    btnTransact: TButton;
    lblActive: TLabel;
    lblProcesses: TLabel;
    lvActiveTmTx: TListViewEx;
    lvHandles: TListViewEx;
    timerUpdate: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure appEventsException(Sender: TObject; E: Exception);
    procedure btnNewTmTxClick(Sender: TObject);
    procedure lvActiveTmTxDblClick(Sender: TObject);
    procedure lvHandlesDblClick(Sender: TObject);
    procedure timerUpdateTick(Sender: TObject);
  private
    ActiveTransctions: THysteresisList<TGuid>;
    Consumers: THysteresisList<TSystemHandleEntry>;
    TmTxTypeIndex: NativeUInt;
    IsFirstUpdate: Boolean;
    FOnHandleSnapshotting: TEvent<TArray<TSystemHandleEntry>>;
    FOnTmTxEnumeration: TEvent<TArray<TGuid>>;
    FOnProcessSnapshotting: TEvent<TArray<TProcessEntry>>;
    FOnShutdown: TEvent<TObject>;
    procedure FillTransactionInfo(const Item: TGuid; Index: Integer);
    procedure AddMissingProcessIcons;
    procedure AtActiveAddStart(const Item: TGuid; Index: Integer);
    procedure AtActiveAddFinish(const Item: TGuid; Index: Integer);
    procedure AtActiveRemoveStart(const Item: TGuid; Index: Integer);
    procedure AtActiveRemoveFinish(const Item: TGuid; Index: Integer);
    procedure AtConsumerAddStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtConsumerAddFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtConsumerRemoveStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtConsumerRemoveFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtTmTxEnumeration(const Transactions: TArray<TGuid>);
    procedure AtHandleSnapshot(const Handles: TArray<TSystemHandleEntry>);
  public
    procedure ForceTimerUpdate;
    property OnHandleSnapshotting: TEvent<TArray<TSystemHandleEntry>> read FOnHandleSnapshotting;
    property OnTmTxEnumeration: TEvent<TArray<TGuid>> read FOnTmTxEnumeration;
    property OnProcessSnapshotting: TEvent<TArray<TProcessEntry>> read FOnProcessSnapshotting;
    property OnShutdown: TEvent<TObject> read FOnShutdown;
  end;

var
  FormMain: TFormMain;

function CompareHandleEntries(const A, B: TSystemHandleEntry): Boolean;

implementation

uses
  Ntapi.nttmapi, NtUiLib.Exceptions, NtUtils.Transactions, NtUtils.Objects,
  Ntapi.ntobapi, NtUtils.Access, DelphiUtils.Strings, DelphiUtils.Arrays,
  NtUtils.Processes, NtUiLib.Icons,
  TransactionInfo, ProcessInfo;

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

procedure TFormMain.appEventsException(Sender: TObject; E: Exception);
begin
  ShowNtxException(Self.Handle, E);
end;

procedure TFormMain.AtActiveAddFinish(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items[Index].ColorEnabled := False;
end;

procedure TFormMain.AtActiveAddStart(const Item: TGuid; Index: Integer);
begin
  with lvActiveTmTx.Items.Add do
  begin
    Caption := Item.ToString;
    FillTransactionInfo(Item, Index);

    if not IsFirstUpdate then
      Color := clLime;
  end;
end;

procedure TFormMain.AtActiveRemoveFinish(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items.Delete(Index);
end;

procedure TFormMain.AtActiveRemoveStart(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items[Index].Selected := False;
  lvActiveTmTx.Items[Index].Color := clRed;
end;

procedure TFormMain.AtConsumerAddFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items[Index].ColorEnabled := False;
end;

procedure TFormMain.AtConsumerAddStart(const Item: TSystemHandleEntry;
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

procedure TFormMain.AtConsumerRemoveFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items.Delete(Index);
end;

procedure TFormMain.AtConsumerRemoveStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items[Index].Selected := False;
  lvHandles.Items[Index].Color := clRed;
end;

procedure TFormMain.AtHandleSnapshot(const Handles: TArray<TSystemHandleEntry>);
begin
  lvHandles.Items.BeginUpdate;
  begin
    // Process the snapshot
    Consumers.Update(Handles);

    // If new items arrived, update their process names and icons
    if Consumers.AddStartDelta > 0 then
      AddMissingProcessIcons;
  end;
  lvHandles.Items.EndUpdate;
end;

procedure TFormMain.AtTmTxEnumeration(const Transactions: TArray<TGuid>);
var
  i: Integer;
begin
  lvActiveTmTx.Items.BeginUpdate;
  begin
    // Process the snapshot
    ActiveTransctions.Update(Transactions);

    // Update info in changable columns for alive items
   for i := 0 to ActiveTransctions.Count - 1 do
     if ActiveTransctions[i].State <> hisDeleted then
       FillTransactionInfo(ActiveTransctions[i].Data, i);
  end;
  lvActiveTmTx.Items.EndUpdate;
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
    TFormTmTxInfo.CreateDlg(hxTransaction);
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

procedure TFormMain.ForceTimerUpdate;
begin
  timerUpdate.Enabled := False;
  timerUpdateTick(Self);
  timerUpdate.Enabled := True;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OnHandleSnapshotting.Unsubscribe(AtHandleSnapshot);
  OnTmTxEnumeration.Unsubscribe(AtTmTxEnumeration);
  ActiveTransctions.Free;
  Consumers.Free;
  FOnShutdown.Invoke(Self);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  ObjTypes: TArray<TObjectTypeInfo>;
  i: Integer;
begin
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

  // Bind process icons
  lvHandles.SmallImages := TProcessIcons.ImageList;

  // Subscibe for active transactions change
  ActiveTransctions := THysteresisList<TGuid>.Create(CompareGuids, 3);
  ActiveTransctions.OnAddStart := AtActiveAddStart;
  ActiveTransctions.OnAddFinish := AtActiveAddFinish;
  ActiveTransctions.OnRemoveStart := AtActiveRemoveStart;
  ActiveTransctions.OnRemoveFinish := AtActiveRemoveFinish;

  // Subscribe for transaction consumers change
  Consumers := THysteresisList<TSystemHandleEntry>.Create(CompareHandleEntries,
    3);
  Consumers.OnAddStart := AtConsumerAddStart;
  Consumers.OnAddFinish := AtConsumerAddFinish;
  Consumers.OnRemoveStart := AtConsumerRemoveStart;
  Consumers.OnRemoveFinish := AtConsumerRemoveFinish;

  // Subscribe for snapshots
  OnTmTxEnumeration.Subscribe(AtTmTxEnumeration);
  OnHandleSnapshotting.Subscribe(AtHandleSnapshot);

  // Force the first timer tick
  IsFirstUpdate := True;
  timerUpdateTick(Sender);
end;

procedure TFormMain.lvActiveTmTxDblClick(Sender: TObject);
var
  hxTransaction: IHandle;
begin
  if not Assigned(lvActiveTmTx.Selected) then
    Exit;

  NtxOpenTransactionById(hxTransaction, ActiveTransctions[
    lvActiveTmTx.Selected.Index].Data, MAXIMUM_ALLOWED).RaiseOnError;

  TFormTmTxInfo.CreateDlg(hxTransaction);
end;

procedure TFormMain.lvHandlesDblClick(Sender: TObject);
begin
  if not Assigned(lvHandles.Selected) then
    Exit;

  TFormProcessInfo.CreateDlg(Consumers[lvHandles.Selected.Index].Data.
    UniqueProcessId);
end;

procedure TFormMain.timerUpdateTick(Sender: TObject);
var
  Guids: TArray<TGuid>;
  Handles: TArray<TSystemHandleEntry>;
  Processes: TArray<TProcessEntry>;
begin
  if WindowState = wsMinimized then
    Exit;

  try
    // Snapshot active transactions
    if OnTmTxEnumeration.Count > 0 then
    begin
      if not NtxEnumerateTransactions(Guids).IsSuccess then
        SetLength(Guids, 0);

      OnTmTxEnumeration.Invoke(Guids);
    end;

    // Snapshot system handles
    if OnHandleSnapshotting.Count > 0 then
    begin
      if not NtxEnumerateHandles(Handles).IsSuccess then
        SetLength(Handles, 0);

      // Filter transactions only
      TArrayHelper.Filter<TSystemHandleEntry>(Handles, FilterByType,
        TmTxTypeIndex);

      OnHandleSnapshotting.Invoke(Handles);
    end;

    // Snapshot processes
    if OnProcessSnapshotting.Count > 0 then
    begin
      if not NtxEnumerateProcesses(Processes).IsSuccess then
        SetLength(Processes, 0);

      OnProcessSnapshotting.Invoke(Processes);
    end;

    IsFirstUpdate := False;
  except
    // Exceptions here mean a serious bug.
    // At least stop spamming messages every second...
    timerUpdate.Enabled := False;
    raise;
  end;
end;

end.
