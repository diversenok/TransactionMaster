unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.Graphics,
  NtUtils.Exceptions, Vcl.ComCtrls, VclEx.ListView, Vcl.AppEvnts, Vcl.ExtCtrls,
  NtUiLib.HysteresisList;

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
  private
    ActiveTransctions: THysteresisList<TGuid>;
    procedure ForceTimer;
    procedure FillTransactionInfo(const Item: TGuid; Index: Integer);
    procedure OnActiveAddStart(const Item: TGuid; Index: Integer);
    procedure OnActiveAddFinish(const Item: TGuid; Index: Integer);
    procedure OnActiveRemoveStart(const Item: TGuid; Index: Integer);
    procedure OnActiveRemoveFinish(const Item: TGuid; Index: Integer);
  end;

var
  FormMain: TFormMain;

implementation

uses
  Ntapi.nttmapi, NtUiLib.Exceptions, NtUtils.Transactions, NtUtils.Objects,
  Ntapi.ntobapi, TransactionInfo;

{$R *.dfm}

function CompareGuids(const A, B: TGuid): Boolean;
begin
  Result := (A = B);
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
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ActiveTransctions := THysteresisList<TGuid>.Create(CompareGuids, 3);
  ActiveTransctions.OnAddStart := OnActiveAddStart;
  ActiveTransctions.OnAddFinish := OnActiveAddFinish;
  ActiveTransctions.OnRemoveStart := OnActiveRemoveStart;
  ActiveTransctions.OnRemoveFinish := OnActiveRemoveFinish;
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

procedure TFormMain.OnActiveAddFinish(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items[Index].ColorEnabled := False;
end;

procedure TFormMain.OnActiveAddStart(const Item: TGuid; Index: Integer);
begin
  with lvActiveTmTx.Items.Add do
  begin
    Caption := Item.ToString;
    Color := clLime;
    FillTransactionInfo(Item, Index);
  end;
end;

procedure TFormMain.OnActiveRemoveFinish(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items.Delete(Index);
end;

procedure TFormMain.OnActiveRemoveStart(const Item: TGuid; Index: Integer);
begin
  lvActiveTmTx.Items[Index].Color := clRed;
end;

procedure TFormMain.UpdateTimerTimer(Sender: TObject);
var
  Guids: TArray<TGuid>;
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
end;

end.
