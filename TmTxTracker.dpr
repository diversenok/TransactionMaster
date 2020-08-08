library TmTxTracker;

{$R *.res}
{$WEAKLINKRTTI ON}

uses
  DelphiApi.Reflection in 'NtUtilsUI\NtUtils\Headers\DelphiApi.Reflection.pas',
  Ntapi.ntdef in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntdef.pas',
  Ntapi.ntexapi in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntexapi.pas',
  Ntapi.ntkeapi in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntkeapi.pas',
  Ntapi.ntmmapi in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntmmapi.pas',
  Ntapi.ntpebteb in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntpebteb.pas',
  Ntapi.ntrtl in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntrtl.pas',
  Ntapi.ntseapi in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntseapi.pas',
  Ntapi.ntstatus in 'NtUtilsUI\NtUtils\Headers\Ntapi.ntstatus.pas',
  Ntapi.nttmapi in 'NtUtilsUI\NtUtils\Headers\Ntapi.nttmapi.pas',
  NtUtils.Version in 'NtUtilsUI\NtUtils\Headers\NtUtils.Version.pas',
  Winapi.NtSecApi in 'NtUtilsUI\NtUtils\Headers\Winapi.NtSecApi.pas',
  Winapi.WinBase in 'NtUtilsUI\NtUtils\Headers\Winapi.WinBase.pas',
  Winapi.WinNt in 'NtUtilsUI\NtUtils\Headers\Winapi.WinNt.pas',
  NtUtils.SysUtils in 'NtUtilsUI\NtUtils\NtUtils.SysUtils.pas';

var
  FutureTransaction: THandle;

{$IFDEF Debug}
function ReportState(Reason: Integer): NTSTATUS;
var
  TidStr: String;
begin
  Result := STATUS_SUCCESS;

  case Reason of
    DLL_PROCESS_ATTACH:
      OutputDebugStringW('TmTxTracker: attached to process');

    DLL_PROCESS_DETACH:
      OutputDebugStringW('TmTxTracker: detached from process');

    DLL_THREAD_ATTACH, DLL_THREAD_DETACH:
      begin
        TidStr := RtlxIntToStr(NtCurrentTeb.ClientId.UniqueThread);

        case Reason of
          DLL_THREAD_ATTACH:
            OutputDebugStringW(PWideChar('TmTxTracker: attached to thread ' +
              TidStr));

          DLL_THREAD_DETACH:
            OutputDebugStringW(PWideChar('TmTxTracker: detached from thread ' +
              TidStr));
        end;
      end;
  end;
end;
{$ENDIF}

procedure DllMain(Reason: Integer);
begin
  if (Reason = DLL_THREAD_ATTACH) and (FutureTransaction <> 0) then
    RtlSetCurrentTransaction(FutureTransaction);

  {$IFDEF Debug} ReportState(Reason); {$ENDIF}
end;

exports
  FutureTransaction, DllMain;

begin
  DllProc := DllMain;
  DllMain(DLL_PROCESS_ATTACH);
end.
