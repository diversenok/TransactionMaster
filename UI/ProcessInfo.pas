unit ProcessInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, VclEx.ListView, Vcl.Menus, NtUiLib.HysteresisList,
  NtUtils.Objects.Snapshots, NtUtils.Objects, NtUtils.Processes.Snapshots,
  NtUtils.Exceptions, TmTxTrackerUtils;

type
  TFormProcessInfo = class(TForm)
    btnClose: TButton;
    btnSetFuture: TButton;
    cbFutureTmTx: TComboBox;
    cmAssign: TMenuItem;
    cmAssignNone: TMenuItem;
    cmCloseHandle: TMenuItem;
    cmIncludeExisting: TMenuItem;
    cmInspect: TMenuItem;
    cmRefreshFuture: TMenuItem;
    cmResume: TMenuItem;
    cmSendTo: TMenuItem;
    cmSeparator: TMenuItem;
    cmSuspend: TMenuItem;
    gbTracking: TGroupBox;
    lblTrackingState: TLabel;
    lvHandles: TListViewEx;
    lvThreads: TListViewEx;
    pageControl: TPageControl;
    popupFuture: TPopupMenu;
    popupHandle: TPopupMenu;
    popupThread: TPopupMenu;
    tabHandles: TTabSheet;
    tabThreads: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSetFutureClick(Sender: TObject);
    procedure cmAssignClick(Sender: TObject);
    procedure cmCloseHandleClick(Sender: TObject);
    procedure cmHandleInspect(Sender: TObject);
    procedure cmRefreshFutureClick(Sender: TObject);
    procedure cmResumeClick(Sender: TObject);
    procedure cmSendToClick(Sender: TObject);
    procedure cmSuspendClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    PID: NativeUInt;
    hxProcessRead: IHandle;
    TmTxTracker: TTmTxTracker;
    hxThreads: TArray<IHandle>;
    Transactions: THysteresisList<TSystemHandleEntry>;
    Threads: THysteresisList<TSystemThreadInformation>;
    IsFirstUpdate: Boolean;
    procedure AtTmTxAddStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtTmTxAddFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtTmTxRemoveStart(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtTmTxRemoveFinish(const Item: TSystemHandleEntry; Index: Integer);
    procedure AtThreadAddStart(const Item: TSystemThreadInformation; Index: Integer);
    procedure AtThreadAddFinish(const Item: TSystemThreadInformation; Index: Integer);
    procedure AtThreadRemoveStart(const Item: TSystemThreadInformation; Index: Integer);
    procedure AtThreadRemoveFinish(const Item: TSystemThreadInformation; Index: Integer);
    procedure AtHandleSnapshot(const Handles: TArray<TSystemHandleEntry>);
    procedure AtProcessSnapshot(const Processes: TArray<TProcessEntry>);
    procedure AtShutdown(const Sender: TObject);
    function FindTmTxTrackerDll: TNtxStatus;
  public
    constructor CreateDlg(ProcessID: NativeUInt);
  end;

implementation

uses
  MainForm, TransactionInfo, Ntapi.nttmapi, Ntapi.ntpsapi, Ntapi.ntkeapi,
  NtUtils.Access, NtUtils.Processes, NtUtils.Threads, NtUtils.Transactions,
  NtUtils.Transactions.Remote, NtUtils.WinUser, DelphiUtils.Strings,
  DelphiUtils.Arrays, System.UiTypes, NtUtils.Exceptions.Report,
  NtUiLib.Icons, ProcessList;

{$R *.dfm}

function CompareThreadEntries(const A, B: TSystemThreadInformation): Boolean;
begin
  // Thread IDs might be reused, compare creation time as well
  Result := (A.ClientId.UniqueThread = B.ClientId.UniqueThread) and
    (A.CreateTime.QuadPart = B.CreateTime.QuadPart);
end;

procedure TFormProcessInfo.AtHandleSnapshot(
  const Handles: TArray<TSystemHandleEntry>);
var
  FilteredHandles: TArray<TSystemHandleEntry>;
  hxProcess, hxTransaction: IHandle;
  Properties: TTransactionProperties;
  i: Integer;
begin
  // We are interested only in one process
  FilteredHandles := Copy(Handles, Low(Handles), Length(Handles));
  TArrayHelper.Filter<TSystemHandleEntry>(FilteredHandles, FilterByProcess,
    PID);

  lvHandles.Items.BeginUpdate;
  begin
    Transactions.Update(FilteredHandles);

    // Query descriptions for new transactions
    // TODO: Open transactions for query only
    if (Transactions.AddStartDelta > 0) and NtxOpenProcess(
      hxProcess, PID, PROCESS_DUP_HANDLE).IsSuccess then
      for i := 0 to Transactions.Count - 1 do
        if (hdAddStart in Transactions[i].BelongsToDelta) and
          NtxDuplicateObjectFrom(hxProcess.Handle,
          Transactions[i].Data.HandleValue, hxTransaction).IsSuccess and
          NtxQueryPropertiesTransaction(hxTransaction.Handle,
          Properties).IsSuccess then
          begin
            lvHandles.Items[i].Cell[2] := Properties.Description;

            // Update caption of the sub-menu and future transaction as well.
            // Note: the first item is "No transaction", skip it
            cmAssign.Items[i + 1].Caption := lvHandles.Items[i].Cell[0] +
              ' (' + Properties.Description + ')';
            cbFutureTmTx.Items[i + 1] := cmAssign.Items[i + 1].Caption;
          end;
  end;
  lvHandles.Items.EndUpdate;
end;

procedure TFormProcessInfo.AtProcessSnapshot(
  const Processes: TArray<TProcessEntry>);
var
  Entry: PProcessEntry;
  ActiveThreads: TArray<TSystemThreadInformation>;
  HandleValue: NativeUInt;
  i: Integer;
begin
  Entry := NtxFindProcessById(Processes, PID);

  if Assigned(Entry) then
  begin
    ActiveThreads := Entry.Threads;

    // Update the caption and the icon
    if IsFirstUpdate then
    begin
      Caption := Entry.ImageName + ' [' + IntToStr(PID) + '] Properties';
      TProcessIcons.ImageList.GetIcon(TProcessIcons.GetIconByPid(PID), Icon);
    end;
  end
  else
    SetLength(ActiveThreads, 0);

  lvThreads.Items.BeginUpdate;
  begin
    Threads.Update(ActiveThreads);

    // Highlight threads based on their state
    for i := 0 to Threads.Count - 1 do
     if (Threads[i].State = hisExisting) or (IsFirstUpdate and
      (Threads[i].State = hisNew)) then
     begin
       if Threads[i].Data.WaitReason = Suspended then
         lvThreads.Items[i].Color := $AAAAAA // Suspended threads are gray
       else if UsrxIsGuiThread(Threads[i].Data.ClientId.UniqueThread) then
         lvThreads.Items[i].Color := $77FFFF // GUI threads are yellow
       else
         lvThreads.Items[i].ColorEnabled := False;
     end;

    // Update threads' current transactions
    if Assigned(hxProcessRead) then
      for i := 0 to Threads.Count - 1 do
        if (Threads[i].State <> hisDeleted) and Assigned(hxThreads[i]) then
          if RtlxGetTransactionThread(hxProcessRead.Handle,
            hxThreads[i].Handle, HandleValue).IsSuccess then
          begin
            if HandleValue = 0 then
              lvThreads.Items[i].Cell[2] := 'No'
            else
              lvThreads.Items[i].Cell[2] := IntToHexEx(HandleValue)
          end
          else
            lvThreads.Items[i].Cell[2] := 'Unknown';
  end;
  lvThreads.Items.EndUpdate;

  IsFirstUpdate := False;
end;

procedure TFormProcessInfo.AtShutdown(const Sender: TObject);
begin
  Close;
end;

procedure TFormProcessInfo.AtThreadAddFinish(
  const Item: TSystemThreadInformation; Index: Integer);
begin
  lvThreads.Items[Index].ColorEnabled := False;
end;

procedure TFormProcessInfo.AtThreadAddStart(
  const Item: TSystemThreadInformation; Index: Integer);
begin
  with lvThreads.Items.Add do
  begin
    Cell[0] := IntToStr(Item.ClientId.UniqueThread);
    Cell[1] := DateTimeToStr(Item.CreateTime.ToDateTime);
    Cell[2] := 'Unknown';

    // Obtain a handle for the thread for future use to query transactions
    SetLength(hxThreads, Length(hxThreads) + 1);
    NtxOpenThread(hxThreads[High(hxThreads)], Item.ClientId.UniqueThread,
      THREAD_QUERY_LIMITED_INFORMATION);

    if not IsFirstUpdate then
      Color := clLime;
  end;
end;

procedure TFormProcessInfo.AtThreadRemoveFinish(
  const Item: TSystemThreadInformation; Index: Integer);
begin
  lvThreads.Items.Delete(Index);
  Delete(hxThreads, Index, 1);
end;

procedure TFormProcessInfo.AtThreadRemoveStart(
  const Item: TSystemThreadInformation; Index: Integer);
begin
  lvThreads.Items[Index].Selected := False;
  lvThreads.Items[Index].Color := clRed;
end;

procedure TFormProcessInfo.AtTmTxAddFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items[Index].ColorEnabled := False;
end;

procedure TFormProcessInfo.AtTmTxAddStart(const Item: TSystemHandleEntry;
  Index: Integer);
var
  Menu: TMenuItem;
begin
  with lvHandles.Items.Add do
  begin
    Cell[0] := IntToHexEx(Item.HandleValue);
    Cell[1] := FormatAccess(Item.GrantedAccess, @TmTxAccessType);
    Cell[2] := 'Unknown';

    // Add corresponging sub-menu
    Menu := TMenuItem.Create(cmAssign);
    Menu.Caption := Cell[0];
    Menu.Tag := Item.HandleValue;
    Menu.OnClick := cmAssignClick;
    cmAssign.Add(Menu);

    // Add an item for future transactions
    cbFutureTmTx.Items.Add(Cell[0]);

    if not IsFirstUpdate then
      Color := clLime;
  end;
end;

procedure TFormProcessInfo.AtTmTxRemoveFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items.Delete(Index);

  // The first item is "No transaction", skip it
  cmAssign.Delete(Index + 1);
  cbFutureTmTx.Items.Delete(Index + 1);
end;

procedure TFormProcessInfo.AtTmTxRemoveStart(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items[Index].Selected := False;
  lvHandles.Items[Index].Color := clRed;
end;

procedure TFormProcessInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormProcessInfo.btnSetFutureClick(Sender: TObject);
var
  hxProcess: IHandle;
  Value: NativeUInt;
begin
  if not Assigned(hxProcessRead) then
    Exit;

  try
    // We need a thread-tracking DLL to proceed
    if not Assigned(TmTxTracker.DllBase) then
    begin
      // Inject it
      NtxOpenProcess(hxProcess, PID, PROCESS_INJECT_DLL).RaiseOnError;
      InjectTmTxTracker(hxProcess.Handle).RaiseOnError;

      // Find its address
      FindTmTxTrackerDll.RaiseOnError;
    end;

    // Prepare the remote handle value
    if cbFutureTmTx.ItemIndex = 0 then
      Value := 0
    else
      Value := Transactions.Items[cbFutureTmTx.ItemIndex - 1].Data.HandleValue;

    // Set it
    NtxOpenProcess(hxProcess, PID, PROCESS_SET_FUTURE_TRANASCTION).RaiseOnError;
    SetFutureTransaction(hxProcess.Handle, TmTxTracker, Value).RaiseOnError;

    if cmIncludeExisting.Checked then
    begin
      // Include existing threads as well
      NtxOpenProcess(hxProcess, PID, PROCESS_SET_PROCESS_TRANSACTION).RaiseOnError;
      RtlxSetTransactionProcess(hxProcess.Handle, Value).RaiseOnError;
      FormMain.ForceTimerUpdate;
    end;
  finally
    cmRefreshFutureClick(cmRefreshFuture);
  end;
end;

procedure TFormProcessInfo.cmAssignClick(Sender: TObject);
var
  hxProcess, hxThread: IHandle;
  HandleValue: NativeUInt;
  i: Integer;
begin
  if (lvThreads.SelCount = 0) or not (Sender is TMenuItem) then
    Exit;

  HandleValue := (Sender as TMenuItem).Tag;
  NtxOpenProcess(hxProcess, PID, PROCESS_SET_THREAD_TRANSACTION).RaiseOnError;

  for i := 0 to Threads.Count - 1 do
    if lvThreads.Items[i].Selected and (Threads[i].State <> hisDeleted) then
    begin
      // TODO: Add abort-continue buttons to the error messages

      // Open the target thread
      NtxOpenThread(hxThread, Threads[i].Data.ClientId.UniqueThread,
        THREAD_SET_TRANSACTION or THREAD_SUSPEND_RESUME).RaiseOnError;

      // Suspend it to prevent race conditions
      NtxSuspendThread(hxThread.Handle).RaiseOnError;
      try
        // Assign transaction
        RtlxSetTransactionThread(hxProcess.Handle, hxThread.Handle,
          HandleValue).RaiseOnError;
      finally
        NtxResumeThread(hxThread.Handle);
      end;
    end;

  FormMain.ForceTimerUpdate;
end;

procedure TFormProcessInfo.cmCloseHandleClick(Sender: TObject);
const
  CONFIRMATION_TEXT = 'If the program tries to use it, it might misbehave.';
var
  hxProcess: IHandle;
begin
  if Assigned(lvHandles.Selected) and (TaskMessageDlg('Close this handle?',
    CONFIRMATION_TEXT, mtConfirmation, mbYesNoCancel, -1) = mrYes) then
  begin
    NtxOpenProcess(hxProcess, PID, PROCESS_DUP_HANDLE).RaiseOnError;

    NtxCloseRemoteHandle(hxProcess.Handle,
      Transactions[lvHandles.Selected.Index].Data.HandleValue).RaiseOnError;
  end;
end;

procedure TFormProcessInfo.cmHandleInspect(Sender: TObject);
var
  hxProcess, hxTransaction: IHandle;
begin
  if not Assigned(lvHandles.Selected) then
    Exit;

  // TODO: Duplicate with maximum allowed access
  NtxOpenProcess(hxProcess, PID, PROCESS_DUP_HANDLE).RaiseOnError;
  NtxDuplicateObjectFrom(hxProcess.Handle, Transactions[lvHandles.Selected.
    Index].Data.HandleValue, hxTransaction).RaiseOnError;

  TFormTmTxInfo.CreateDlg(hxTransaction);
end;

procedure TFormProcessInfo.cmRefreshFutureClick(Sender: TObject);
var
  Value: NativeUInt;
  Status: TNtxStatus;
  i: Integer;
begin
  if not Assigned(hxProcessRead) then
  begin
    cbFutureTmTx.ItemIndex := -1;
    Exit;
  end;

  if Assigned(TmTxTracker.DllBase) then
  begin
    // Read the transaction handle
    Status := GetFutureTransaction(hxProcessRead.Handle, TmTxTracker, Value);

    // Explicitly throw exceptions on user actions
    if Sender = cmRefreshFuture then
      Status.RaiseOnError
    else if not Status.IsSuccess then
    begin
      cbFutureTmTx.ItemIndex := -1;
      Exit;
    end;
  end
  else
    Value := 0; // No tracking => no future transactions

  if Value = 0 then
  begin
    // Use "No transaction" item
    cbFutureTmTx.ItemIndex := 0;
    Exit;
  end;

  // Find the corresponding item
  for i := 0 to Transactions.Count - 1 do
    if Transactions[i].Data.HandleValue = Value then
    begin
      cbFutureTmTx.ItemIndex := i + 1;
      Exit;
    end;

  cbFutureTmTx.ItemIndex := -1;
end;

procedure TFormProcessInfo.cmResumeClick(Sender: TObject);
var
  hxThread: IHandle;
  i: Integer;
begin
  for i := 0 to Threads.Count - 1 do
    if lvThreads.Items[i].Selected and (Threads[i].State <> hisDeleted) then
    begin
      NtxOpenThread(hxThread, Threads[i].Data.ClientId.UniqueThread,
        THREAD_SUSPEND_RESUME).RaiseOnError;

      NtxResumeThread(hxThread.Handle).RaiseOnError;
    end;
end;

procedure TFormProcessInfo.cmSendToClick(Sender: TObject);
var
  TargetPID: NativeUInt;
  hxSource, hxTarget: IHandle;
  NewTargetHandle: THandle;
begin
  if not Assigned(lvHandles.Selected) then
    Exit;

  // Open source process
  NtxOpenProcess(hxSource, PID, PROCESS_DUP_HANDLE).RaiseOnError;

  // Pick and open the target process
  TargetPID := TFormProcessList.Pick(FormMain).Process.ProcessId;
  NtxOpenProcess(hxTarget, TargetPID, PROCESS_DUP_HANDLE).RaiseOnError;

  // Send the handle
  NtxDuplicateObject(hxSource.Handle, Transactions[lvHandles.Selected.Index].
    Data.HandleValue, hxTarget.Handle, NewTargetHandle, 0, 0,
    DUPLICATE_SAME_ACCESS).RaiseOnError;

  // Show info
  TFormProcessInfo.CreateDlg(TargetPID);
end;

procedure TFormProcessInfo.cmSuspendClick(Sender: TObject);
var
  hxThread: IHandle;
  i: Integer;
begin
  for i := 0 to Threads.Count - 1 do
    if lvThreads.Items[i].Selected and (Threads[i].State <> hisDeleted) then
    begin
      NtxOpenThread(hxThread, Threads[i].Data.ClientId.UniqueThread,
        THREAD_SUSPEND_RESUME).RaiseOnError;

      NtxSuspendThread(hxThread.Handle).RaiseOnError;
    end;
end;

constructor TFormProcessInfo.CreateDlg(ProcessID: NativeUInt);
begin
  PID := ProcessID;
  inherited Create(FormMain);
  Show;
end;

function TFormProcessInfo.FindTmTxTrackerDll: TNtxStatus;
begin
  cbFutureTmTx.Enabled := False;
  btnSetFuture.Enabled := False;

  // Find the tracker dll
  Result := FindTmTxTracker(hxProcessRead.Handle, TmTxTracker);

  if not Result.IsSuccess then
  begin
    lblTrackingState.Caption := 'Unknown: can''t enumerate loaded modules.';
    lblTrackingState.Hint := NtxVerboseStatusMessage(Result);
  end
  else
  begin
    cbFutureTmTx.Enabled := True;
    btnSetFuture.Enabled := True;

    if Assigned(TmTxTracker.DllBase) then
      lblTrackingState.Caption := 'Tracking DLL is loaded.'
    else
      lblTrackingState.Caption := 'Tracking DLL is not loaded.';
  end;
end;

procedure TFormProcessInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Transactions.Free;
  Threads.Free;
  FormMain.OnProcessSnapshotting.Unsubscribe(AtProcessSnapshot);
  FormMain.OnHandleSnapshotting.Unsubscribe(AtHandleSnapshot);
  FormMain.OnShutdown.Unsubscribe(AtShutdown);
  Action := caFree;
end;

procedure TFormProcessInfo.FormCreate(Sender: TObject);
var
  Status: TNtxStatus;
begin
  // Open the process for read access
  Status := NtxOpenProcess(hxProcessRead, PID, PROCESS_ENUMERATE_MODULES or
    PROCESS_GET_THREAD_TRANSACTION);

  if not Status.IsSuccess then
    lblTrackingState.Hint := NtxVerboseStatusMessage(Status);

  // Subscribe for handle list changes
  Transactions := THysteresisList<TSystemHandleEntry>.Create(
    CompareHandleEntries, 3);
  Transactions.OnAddStart := AtTmTxAddStart;
  Transactions.OnAddFinish := AtTmTxAddFinish;
  Transactions.OnRemoveStart := AtTmTxRemoveStart;
  Transactions.OnRemoveFinish := AtTmTxRemoveFinish;

  // Subscribe for thread list changes
  Threads := THysteresisList<TSystemThreadInformation>.Create(
    CompareThreadEntries, 3);
  Threads.OnAddStart := AtThreadAddStart;
  Threads.OnAddFinish := AtThreadAddFinish;
  Threads.OnRemoveStart := AtThreadRemoveStart;
  Threads.OnRemoveFinish := AtThreadRemoveFinish;

  // Subscribe snapshotting and shutdown
  FormMain.OnHandleSnapshotting.Subscribe(AtHandleSnapshot);
  FormMain.OnProcessSnapshotting.Subscribe(AtProcessSnapshot);
  FormMain.OnShutdown.Subscribe(AtShutdown);

  IsFirstUpdate := True;
  FormMain.ForceTimerUpdate;

  // Update future thread information
  if Assigned(hxProcessRead) then
    if FindTmTxTrackerDll.IsSuccess then
      cmRefreshFutureClick(Self);
end;

procedure TFormProcessInfo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_ESCAPE then
    Close;
end;

end.
