unit ProcessInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, VclEx.ListView, Vcl.Menus, NtUiLib.HysteresisList,
  NtUtils.Objects.Snapshots, NtUtils.Objects, Winapi.WinNt,
  NtUtils.Processes.Snapshots;

type
  TFormProcessInfo = class(TForm)
    btnClose: TButton;
    cmAssign: TMenuItem;
    cmAssignNone: TMenuItem;
    cmCloseHandle: TMenuItem;
    cmInspect: TMenuItem;
    cmResume: TMenuItem;
    cmSuspend: TMenuItem;
    lvHandles: TListViewEx;
    lvThreads: TListViewEx;
    pageControl: TPageControl;
    popupHandle: TPopupMenu;
    popupThread: TPopupMenu;
    tabHandles: TTabSheet;
    tabThreads: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure cmAssignClick(Sender: TObject);
    procedure cmCloseHandleClick(Sender: TObject);
    procedure cmHandleInspect(Sender: TObject);
    procedure cmResumeClick(Sender: TObject);
    procedure cmSuspendClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    PID: NativeUInt;
    hxProcessVmRead: IHandle;
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
  public
    constructor CreateDlg(ProcessID: NativeUInt);
  end;

implementation

uses
  MainForm, TransactionInfo, Ntapi.nttmapi, Ntapi.ntpsapi, Ntapi.ntkeapi,
  NtUtils.Access, NtUtils.Processes, NtUtils.Threads, NtUtils.Exceptions,
  NtUtils.Transactions, NtUtils.Transactions.Remote, NtUtils.WinUser,
  DelphiUtils.Strings, DelphiUtils.Arrays;

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
          NtxDuplicateObjectFrom(hxProcess.Value,
          Transactions[i].Data.HandleValue, hxTransaction).IsSuccess and
          NtxQueryPropertiesTransaction(hxTransaction.Value,
          Properties).IsSuccess then
          begin
            lvHandles.Items[i].Cell[2] := Properties.Description;

            // Update caption of the sub-menu as well. Note: the first
            // sub-menu is "No transaction", skip it
            cmAssign.Items[i + 1].Caption := lvHandles.Items[i].Cell[0] +
              ' (' + Properties.Description + ')';
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
    ActiveThreads := Entry.Threads
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
    if Assigned(hxProcessVmRead) then
      for i := 0 to Threads.Count - 1 do
        if (Threads[i].State <> hisDeleted) and Assigned(hxThreads[i]) then
          if RtlxGetTransactionThread(hxProcessVmRead.Value, hxThreads[i].Value,
            HandleValue).IsSuccess then
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

    if not IsFirstUpdate then
      Color := clLime;
  end;
end;

procedure TFormProcessInfo.AtTmTxRemoveFinish(const Item: TSystemHandleEntry;
  Index: Integer);
begin
  lvHandles.Items.Delete(Index);
  cmAssign.Delete(Index + 1); // The first item is "No transaction", skip it
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
      // Open the target thread
      NtxOpenThread(hxThread, Threads[i].Data.ClientId.UniqueThread,
        THREAD_QUERY_LIMITED_INFORMATION or THREAD_SUSPEND_RESUME).RaiseOnError;

      // Suspend it to prevent race conditions
      NtxSuspendThread(hxThread.Value).RaiseOnError;
      try
        // Assign transaction
        RtlxSetTransactionThread(hxProcess.Value, hxThread.Value,
          HandleValue).RaiseOnError;
      finally
        NtxResumeThread(hxThread.Value);
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

    NtxCloseRemoteHandle(hxProcess.Value,
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
  NtxDuplicateObjectFrom(hxProcess.Value, Transactions[lvHandles.Selected.Index]
    .Data.HandleValue, hxTransaction).RaiseOnError;

  TFormTmTxInfo.CreateDlg(hxTransaction);
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

      NtxResumeThread(hxThread.Value).RaiseOnError;
    end;
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

      NtxSuspendThread(hxThread.Value).RaiseOnError;
    end;
end;

constructor TFormProcessInfo.CreateDlg(ProcessID: NativeUInt);
begin
  PID := ProcessID;
  NtxOpenProcess(hxProcessVmRead, PID, PROCESS_GET_THREAD_TRANSACTION);
  inherited Create(FormMain);
  Show;
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
begin
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
end;

procedure TFormProcessInfo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_ESCAPE then
    Close;
end;

end.
