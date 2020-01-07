unit TransactionInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.Graphics, VclEx.ListView, NtUtils.Objects,
  NtUtils.Objects.Snapshots, NtUiLib.HysteresisList,
  NtUtils.Processes.Snapshots;

type
  TFormTmTxInfo = class(TForm)
    btnClose: TButton;
    btnCommit: TButton;
    btnRollback: TButton;
    btnSendHandle: TButton;
    cmCloseHandle: TMenuItem;
    cmInspect: TMenuItem;
    lblUsedIn: TLabel;
    lvConsumers: TListViewEx;
    lvInfo: TListViewEx;
    pageControl: TPageControl;
    popupProcess: TPopupMenu;
    tabConsumers: TTabSheet;
    tabGeneral: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCommitClick(Sender: TObject);
    procedure btnRollbackClick(Sender: TObject);
    procedure btnSendHandleClick(Sender: TObject);
    procedure cmCloseHandleClick(Sender: TObject);
    procedure lvProcessInspect(Sender: TObject);
  private
    hxTranscation: IHandle;
    Consumers: THysteresisList<TSystemHandleEntry>;
    IsFirstUpdate: Boolean;
    procedure UpdateProperties;
    procedure UpdateBasicInfo;
    procedure AtConsumerAddStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtConsumerAddFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtConsumerRemoveStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtConsumerRemoveFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtHandleSnapshot(const Handles: TArray<TSystemHandleEntry>);
    procedure AtNewProcessArrive(const Processes: TArray<TProcessEntry>);
    procedure AtShutdown(const Sender: TObject);
  public
    constructor CreateDlg(Transcation: IHandle);
  end;

implementation

uses
  NtUtils.Transactions, Ntapi.nttmapi, Ntapi.ntobapi, NtUtils.Access,
  DelphiUtils.Strings, NtUtils.Exceptions,
  System.UITypes, NtUiLib.Icons, NtUtils.Processes, Ntapi.ntpsapi,
  ProcessList, MainForm, ProcessInfo;

{$R *.dfm}

const
  COMMIT_ROLLBACK_WARNING = 'Commiting or rolling back a transaction ' +
    'invalidates all file handles opened for this transaction. Programs that' +
    'use this transaction might misbehave. Continue anyway?';

procedure TFormTmTxInfo.AtConsumerAddFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvConsumers.Items[Index].ColorEnabled := False;
end;

procedure TFormTmTxInfo.AtConsumerAddStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  with lvConsumers.Items.Add do
  begin
    Cell[1] := IntToStr(Item.UniqueProcessId);
    Cell[2] := IntToHexEx(Item.HandleValue);
    Cell[3] := FormatAccess(Item.GrantedAccess, @TmTxAccessType);

    if not IsFirstUpdate then
      Color := clLime;
  end;
end;

procedure TFormTmTxInfo.AtConsumerRemoveFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvConsumers.Items.Delete(Index);
end;

procedure TFormTmTxInfo.AtConsumerRemoveStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvConsumers.Items[Index].Selected := False;
  lvConsumers.Items[Index].Color := clRed;
end;

procedure TFormTmTxInfo.AtHandleSnapshot(const Handles: TArray<TSystemHandleEntry>);
var
  FilteredHandles: TArray<TSystemHandleEntry>;
begin
  // Filter on handle to our transaction
  FilteredHandles := Copy(Handles, Low(Handles), Length(Handles));
  NtxFilterHandlesByHandle(FilteredHandles, hxTranscation.Value);

  lvConsumers.Items.BeginUpdate;
  begin
    // Process the snapshot
    Consumers.Update(FilteredHandles);

    // If new items arrived, update their process names and icons
    // Note: this event will automatically unsunscribe after update
    if Consumers.AddStartDelta > 0 then
      FormMain.OnProcessSnapshotting.Subscribe(AtNewProcessArrive);
  end;
  lvConsumers.Items.EndUpdate;

  lvInfo.Items.BeginUpdate;
  begin
    UpdateProperties;
    UpdateBasicInfo;
  end;
  lvInfo.Items.EndUpdate;

  IsFirstUpdate := False;
end;

procedure TFormTmTxInfo.AtNewProcessArrive(
  const Processes: TArray<TProcessEntry>);
var
  FileName: String;
  Entry: PProcessEntry;
  i: Integer;
begin
  for i := 0 to Consumers.Count - 1 do
    if hdAddStart in Consumers[i].BelongsToDelta then
    begin
      Entry := NtxFindProcessById(Processes, Consumers[i].Data.UniqueProcessId);
      FileName := NtxTryQueryImageProcessById(Consumers[i].Data.UniqueProcessId);

      if Assigned(Entry) then
        lvConsumers.Items[i].Cell[0] := Entry.ImageName
      else if FileName <> '' then
        lvConsumers.Items[i].Cell[0] := ExtractFileName(FileName)
      else
        lvConsumers.Items[i].Cell[0] := 'Unknown';

      lvConsumers.Items[i].ImageIndex := TProcessIcons.GetIcon(FileName);
    end;

  // Update only once
  FormMain.OnProcessSnapshotting.Unsubscribe(AtNewProcessArrive);
end;

procedure TFormTmTxInfo.AtShutdown(const Sender: TObject);
begin
  Close;
end;

procedure TFormTmTxInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormTmTxInfo.btnCommitClick(Sender: TObject);
begin
  if TaskMessageDlg('Commit the transaction?', COMMIT_ROLLBACK_WARNING,
    mtWarning, mbYesNoCancel, -1) = mrYes then
  begin
    NtxCommitTransaction(hxTranscation.Value).RaiseOnError;
    FormMain.ForceTimerUpdate;
  end;
end;

procedure TFormTmTxInfo.btnRollbackClick(Sender: TObject);
begin
  if TaskMessageDlg('Rollback the transaction?', COMMIT_ROLLBACK_WARNING,
    mtWarning, mbYesNoCancel, -1) = mrYes then
  begin
    NtxRollbackTransaction(hxTranscation.Value).RaiseOnError;
    FormMain.ForceTimerUpdate;
  end;
end;

procedure TFormTmTxInfo.btnSendHandleClick(Sender: TObject);
var
  hxProcess: IHandle;
  hNewHandle: THandle;
begin
  NtxOpenProcess(hxProcess, TFormProcessList.Pick(Self).Process.ProcessId,
    PROCESS_DUP_HANDLE).RaiseOnError;

  NtxDuplicateObjectTo(hxProcess.Value, hxTranscation.Value,
    hNewHandle).RaiseOnError;

  FormMain.ForceTimerUpdate;;
end;

procedure TFormTmTxInfo.cmCloseHandleClick(Sender: TObject);
const
  CONFIRMATION_TEXT = 'Do you really want to close this handle in ';
var
  Item: THysteresisItem<TSystemHandleEntry>;
  hxProcess: IHandle;
begin
  if not Assigned(lvConsumers.Selected) then
    Exit;

  Item := Consumers[lvConsumers.Selected.Index];

  // The handles was already closed
  if Item.State = hisDeleted then
    Exit;

  // Don't allow closing handles in the current process
  if Item.Data.UniqueProcessId = NtCurrentProcessId then
    Exit;

  if TaskMessageDlg('Close this handle?', CONFIRMATION_TEXT +
    lvConsumers.Selected.Caption + '?', mtConfirmation,
    mbYesNoCancel, -1) = mrYes then
  begin
    NtxOpenProcess(hxProcess, Item.Data.UniqueProcessId, PROCESS_DUP_HANDLE).
      RaiseOnError;

    NtxCloseRemoteHandle(hxProcess.Value, Item.Data.HandleValue).RaiseOnError;
    FormMain.ForceTimerUpdate;;
  end;
end;

constructor TFormTmTxInfo.CreateDlg(Transcation: IHandle);
begin
  hxTranscation := Transcation;
  inherited Create(FormMain);
  Show;
end;

procedure TFormTmTxInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormMain.OnHandleSnapshotting.Unsubscribe(AtHandleSnapshot);
  FormMain.OnShutdown.Unsubscribe(AtShutdown);
  Consumers.Free;
  Action := caFree;
end;

procedure TFormTmTxInfo.FormCreate(Sender: TObject);
var
  TrInfo: TTransactionBasicInformation;
begin
  // Bind process icons
  lvConsumers.SmallImages := TProcessIcons.ImageList;

  // Subscribe for transaction consumers change
  Consumers := THysteresisList<TSystemHandleEntry>.Create(CompareHandleEntries,
    3);
  Consumers.OnAddStart := AtConsumerAddStart;
  Consumers.OnAddFinish := AtConsumerAddFinish;
  Consumers.OnRemoveStart := AtConsumerRemoveStart;
  Consumers.OnRemoveFinish := AtConsumerRemoveFinish;

  // Subscribe for snapshots and shutdown
  FormMain.OnHandleSnapshotting.Subscribe(AtHandleSnapshot);
  FormMain.OnShutdown.Subscribe(AtShutdown);

  // Query static info about the transaction
  lvInfo.Items.BeginUpdate;
  begin
    if NtxTransaction.Query<TTransactionBasicInformation>(hxTranscation.Value,
      TransactionBasicInformation, TrInfo).IsSuccess then
      lvInfo.Items[0].Cell[1] := TrInfo.TransactionId.ToString;

    lvInfo.Items[3].Cell[1] := IntToHexEx(hxTranscation.Value);
  end;
  lvInfo.Items.EndUpdate;

  IsFirstUpdate := True;
  FormMain.ForceTimerUpdate;
end;

procedure TFormTmTxInfo.lvProcessInspect(Sender: TObject);
begin
  if not Assigned(lvConsumers.Selected) then
    Exit;

  TFormProcessInfo.CreateDlg(Consumers[lvConsumers.Selected.Index].Data
    .UniqueProcessId);
end;

procedure TFormTmTxInfo.UpdateBasicInfo;
var
  Info: TObjectBasicInformaion;
begin
  if NtxQueryBasicInfoObject(hxTranscation.Value, Info).IsSuccess then
  begin
    lvInfo.Items[4].Cell[1] := FormatAccessPrefixed(Info.GrantedAccess,
      @TmTxAccessType);

    lvInfo.Items[5].Cell[1] := IntToStr(Info.PointerCount);
    lvInfo.Items[6].Cell[1] := IntToStr(Info.HandleCount);
  end;
end;

procedure TFormTmTxInfo.UpdateProperties;
var
  Props: TTransactionProperties;
begin
  if NtxQueryPropertiesTransaction(hxTranscation.Value, Props).IsSuccess then
  begin
    lvInfo.Items[1].Cell[1] := Props.Description;
    lvInfo.Items[2].Cell[1] := PrettifyCamelCaseEnum(
      TypeInfo(TTransactionOutcome), Integer(Props.Outcome),
      'TransactionOutcome');
  end;
end;

end.
