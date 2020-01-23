unit ProcessTransact;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, NtUtils.Objects, NtUtils.Exceptions;

type
  TFormTransact = class(TForm)
    btnBrowse: TButton;
    btnCancel: TButton;
    btnContinue: TButton;
    btnProcessSelect: TButton;
    cbThreadsFuture: TCheckBox;
    dlgOpen: TOpenDialog;
    lblDescription: TLabel;
    lblExecutable: TLabel;
    lblParameters: TLabel;
    lblProcess: TLabel;
    Pages: TPageControl;
    tabExistingProcess: TTabSheet;
    tabNewProcess: TTabSheet;
    tbDescription: TEdit;
    tbExecutable: TEdit;
    tbParameters: TEdit;
    tbProcess: TButtonedEdit;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnContinueClick(Sender: TObject);
    procedure btnProcessSelectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    PID: NativeUInt;
    function CreateSuspendedProcess(out hxProcess: IHandle;
      out hxThread: IHandle): TNtxStatus;
    function OpenExistingProcess(out hxProcess: IHandle): TNtxStatus;
    function TransactProcess(hProcess, hTransaction: THandle): TNtxStatus;
  end;

implementation

uses
  ProcessList, ProcessInfo, TmTxTrackerUtils, Winapi.ShlwApi, Winapi.WinNt,
  Ntapi.ntstatus, NtUtils.Processes, NtUiLib.Icons, NtUtils.Transactions.Remote,
  NtUtils.Exec, NtUtils.Exec.Win32, NtUtils.Transactions, NtUtils.Threads;

{$R *.dfm}

procedure TFormTransact.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute(Handle) then
    tbExecutable.Text := dlgOpen.FileName;
end;

procedure TFormTransact.btnContinueClick(Sender: TObject);
var
  hxProcess, hxThread, hxTransaction: IHandle;
  Result: TNtxStatus;
begin
  // Create a transaction
  NtxCreateTransaction(hxTransaction, tbDescription.Text).RaiseOnError;

  if Pages.ActivePage = tabNewProcess then
    CreateSuspendedProcess(hxProcess, hxThread).RaiseOnError
  else
    OpenExistingProcess(hxProcess).RaiseOnError;

  Result := TransactProcess(hxProcess.Handle, hxTransaction.Handle);

  if Pages.ActivePage = tabNewProcess then
  begin
    // We have a suspended process. Resume it on success or terminate on failure
    if Result.IsSuccess then
      NtxResumeThread(hxThread.Handle)
    else
      NtxTerminateProcess(hxProcess.Handle, STATUS_DLL_INIT_FAILED);
  end;

  Result.RaiseOnError;

  // Show process info
  TFormProcessInfo.CreateDlg(PID);
  Close;
end;

procedure TFormTransact.btnProcessSelectClick(Sender: TObject);
begin
  with TFormProcessList.Pick(Self) do
  begin
    PID := Process.ProcessId;
    tbProcess.Text := ImageName + ' [' + IntToStr(PID) + ']';
    tbProcess.LeftButton.ImageIndex := TProcessIcons.GetIconByPid(PID);
  end;
end;

function TFormTransact.CreateSuspendedProcess(out hxProcess: IHandle;
  out hxThread: IHandle): TNtxStatus;
var
  ProcessInfo: TProcessInfo;
  ParamProvider: TDefaultExecProvider;
begin
  ParamProvider := TDefaultExecProvider.Create;
  ParamProvider.UseParams := [ppParameters, ppCreateSuspended];
  ParamProvider.strApplication := tbExecutable.Text;
  ParamProvider.strParameters := tbParameters.Text;
  ParamProvider.bCreateSuspended := True;

  Result := TExecCreateProcessAsUser.Execute(ParamProvider, ProcessInfo);

  if Result.IsSuccess then
  begin
    hxProcess := ProcessInfo.hxProcess;
    hxThread := ProcessInfo.hxThread;
    PID := ProcessInfo.ClientId.UniqueProcess;
  end;
end;

procedure TFormTransact.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormTransact.FormCreate(Sender: TObject);
begin
  tbProcess.Images := TProcessIcons.ImageList;
  tbDescription.Text := 'TM Experiment #' + IntToStr(Random(1000));
  SHAutoComplete(tbExecutable.Handle, SHACF_FILESYS_ONLY);
end;

function TFormTransact.OpenExistingProcess(out hxProcess: IHandle): TNtxStatus;
var
  AccessMask: TAccessMask;
begin
  AccessMask := PROCESS_SET_PROCESS_TRANSACTION or PROCESS_DUP_HANDLE;

  if cbThreadsFuture.Checked then
    AccessMask := AccessMask or PROCESS_INJECT_DLL or
      PROCESS_SET_FUTURE_TRANASCTION;

  Result := NtxOpenProcess(hxProcess, PID, AccessMask);
end;

function TFormTransact.TransactProcess(hProcess, hTransaction: THandle):
  TNtxStatus;
var
  hRemoteHandle: THandle;
  TmTxTracker: TTmTxTracker;
begin
  // Duplicate the handle to the process
  Result := NtxDuplicateObjectTo(hProcess, hTransaction, hRemoteHandle);

  if not Result.IsSuccess then
    Exit;

  // Set future transactions
  if cbThreadsFuture.Checked then
  begin
    // Inject thread-tracking dll
    Result := InjectTmTxTracker(hProcess);

    if not Result.IsSuccess then
      Exit;

    // Find its base address
    Result := FindTmTxTracker(hProcess, TmTxTracker);

    if not Result.IsSuccess then
      Exit;

    if not Assigned(TmTxTracker.DllBase) then
    begin
      Result.Location := '[Module enumeration]';
      Result.Status := STATUS_DLL_NOT_FOUND;
      Exit;
    end;

    // Notify the dll about future transaction
    Result := SetFutureTransaction(hProcess, TmTxTracker, hRemoteHandle);

    if not Result.IsSuccess then
      Exit;
  end;

  // Set a transaction to existing threads
  Result := RtlxSetTransactionProcess(hProcess, hRemoteHandle);
end;

end.
