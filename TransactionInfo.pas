unit TransactionInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Menus, Vcl.ExtCtrls, Vcl.Graphics, VclEx.ListView, NtUtils.Objects,
  NtUtils.Objects.Snapshots, NtUiLib.HysteresisList;

type
  TFormInfo = class(TForm)
    lvInfo: TListViewEx;
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabConsumers: TTabSheet;
    lvConsumers: TListViewEx;
    lblUsedIn: TLabel;
    btnCommit: TButton;
    btnRollback: TButton;
    btnClose: TButton;
    btnSendHandle: TButton;
    ProcessPopup: TPopupMenu;
    cmCloseHandle: TMenuItem;
    cmInspect: TMenuItem;
    UpdateTimer: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCommitClick(Sender: TObject);
    procedure btnRollbackClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnSendHandleClick(Sender: TObject);
    procedure cmCloseHandleClick(Sender: TObject);
    procedure lvProcessInspect(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    hxTranscation: IHandle;
    Consumers: THysteresisList<TSystemHandleEntry>;
    procedure UpdateProperties;
    procedure UpdateBasicInfo;
    procedure AddMissingProcessIcons;
    procedure ForceUpdate;
    procedure OnConsumerAddStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure OnConsumerAddFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure OnConsumerRemoveStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure OnConsumerRemoveFinish(const Item: TSystemHandleEntry; Index: Integer);
  public
    constructor CreateDlg(AOwner: TComponent; Transcation: IHandle);
  end;

implementation

uses
  NtUtils.Transactions, Ntapi.nttmapi, Ntapi.ntobapi, NtUtils.Access,
  NtUtils.Processes.Snapshots, DelphiUtils.Strings, NtUtils.Exceptions,
  System.UITypes, NtUiLib.Icons, NtUtils.Processes, Ntapi.ntpsapi,
  ProcessList;

{$R *.dfm}

const
  COMMIT_ROLLBACK_WARNING = 'Commiting or rolling back a transaction ' +
    'invalidates all file handles opened for this transaction. Programs that' +
    'use this transaction might misbehave. Continue anyway?';

function CompareHandleEntries(const A, B: TSystemHandleEntry): Boolean;
begin
  Result := (A.UniqueProcessId = B.UniqueProcessId) and
    (A.HandleValue = B.HandleValue) and (A.GrantedAccess = B.GrantedAccess);
end;

procedure TFormInfo.AddMissingProcessIcons;
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
        lvConsumers.Items[i].Cell[0] := Entry.ImageName
      else if FileName <> '' then
        lvConsumers.Items[i].Cell[0] := ExtractFileName(FileName)
      else
        lvConsumers.Items[i].Cell[0] := 'Unknown';

      lvConsumers.Items[i].ImageIndex := TProcessIcons.GetIcon(FileName);
    end;
end;

procedure TFormInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormInfo.btnCommitClick(Sender: TObject);
begin
  if TaskMessageDlg('Commit the transaction?', COMMIT_ROLLBACK_WARNING,
    mtWarning, mbYesNoCancel, -1) = mrYes then
  begin
    NtxCommitTransaction(hxTranscation.Value).RaiseOnError;
    ForceUpdate;
  end;
end;

procedure TFormInfo.btnRollbackClick(Sender: TObject);
begin
  if TaskMessageDlg('Rollback the transaction?', COMMIT_ROLLBACK_WARNING,
    mtWarning, mbYesNoCancel, -1) = mrYes then
  begin
    NtxRollbackTransaction(hxTranscation.Value).RaiseOnError;
    ForceUpdate;
  end;
end;

procedure TFormInfo.btnSendHandleClick(Sender: TObject);
var
  hxProcess: IHandle;
  hNewHandle: THandle;
begin
  NtxOpenProcess(hxProcess, TFormProcessList.Pick(Self).Process.ProcessId,
    PROCESS_DUP_HANDLE).RaiseOnError;

  NtxDuplicateObjectTo(hxProcess.Value, hxTranscation.Value,
    hNewHandle).RaiseOnError;

  ForceUpdate;
end;

procedure TFormInfo.cmCloseHandleClick(Sender: TObject);
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
    lvConsumers.Selected.Selected := False;
    ForceUpdate;
  end;
end;

constructor TFormInfo.CreateDlg(AOwner: TComponent; Transcation: IHandle);
begin
  hxTranscation := Transcation;
  inherited Create(AOwner);
  Show;
end;

procedure TFormInfo.ForceUpdate;
begin
  UpdateTimer.Enabled := False;
  UpdateTimerTimer(Self);
  UpdateTimer.Enabled := True;
end;

procedure TFormInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  Consumers.Free;
end;

procedure TFormInfo.FormCreate(Sender: TObject);
var
  TrInfo: TTransactionBasicInformation;
begin
  Consumers := THysteresisList<TSystemHandleEntry>.Create(CompareHandleEntries,
    3);

  Consumers.OnAddStart := OnConsumerAddStart;
  Consumers.OnAddFinish := OnConsumerAddFinish;
  Consumers.OnRemoveStart := OnConsumerRemoveStart;
  Consumers.OnRemoveFinish := OnConsumerRemoveFinish;

  lvConsumers.SmallImages := TProcessIcons.ImageList;

  lvInfo.Items.BeginUpdate;
  begin
    if NtxTransaction.Query<TTransactionBasicInformation>(hxTranscation.Value,
      TransactionBasicInformation, TrInfo).IsSuccess then
      lvInfo.Items[0].Cell[1] := TrInfo.TransactionId.ToString;

    lvInfo.Items[3].Cell[1] := IntToHexEx(hxTranscation.Value);
  end;
  lvInfo.Items.EndUpdate;

  UpdateTimerTimer(Sender);
end;

procedure TFormInfo.lvProcessInspect(Sender: TObject);
begin
  if not Assigned(lvConsumers.Selected) then
    Exit;

  // TODO: show process information dialog
end;

procedure TFormInfo.OnConsumerAddFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvConsumers.Items[Index].ColorEnabled := False;
end;

procedure TFormInfo.OnConsumerAddStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  with lvConsumers.Items.Add do
  begin
    Cell[1] := IntToStr(Item.UniqueProcessId);
    Cell[2] := IntToHexEx(Item.HandleValue);
    Cell[3] := FormatAccess(Item.GrantedAccess, @TmTxAccessType);
    Color := clLime;
  end;
end;

procedure TFormInfo.OnConsumerRemoveFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvConsumers.Items.Delete(Index);
end;

procedure TFormInfo.OnConsumerRemoveStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvConsumers.Items[Index].Color := clRed;
end;

procedure TFormInfo.UpdateBasicInfo;
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

procedure TFormInfo.UpdateProperties;
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

procedure TFormInfo.UpdateTimerTimer(Sender: TObject);
var
  Handles: TArray<TSystemHandleEntry>;
begin
  if WindowState = wsMinimized then
    Exit;

  // Snapshot and filter system handles
  if NtxEnumerateHandles(Handles).IsSuccess then
    NtxFilterHandlesByHandle(Handles, hxTranscation.Value)
  else
    SetLength(Handles, 0);

  lvConsumers.Items.BeginUpdate;
  begin
    // Process the snapshot
    Consumers.Update(Handles);

    // If new items arrived, update their process names and icons
    if Consumers.AddStartDelta > 0 then
      AddMissingProcessIcons;
  end;
  lvConsumers.Items.EndUpdate;

  lvInfo.Items.BeginUpdate;
  begin
    UpdateProperties;
    UpdateBasicInfo;
  end;
  lvInfo.Items.EndUpdate;
end;

end.
