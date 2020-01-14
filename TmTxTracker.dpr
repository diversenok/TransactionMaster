library TmTxTracker;

{$R *.res}
{$WEAKLINKRTTI ON}

uses
  Ntapi.ntdef in 'NtUtils\Headers\Ntapi.ntdef.pas',
  Ntapi.ntexapi in 'NtUtils\Headers\Ntapi.ntexapi.pas',
  Ntapi.ntkeapi in 'NtUtils\Headers\Ntapi.ntkeapi.pas',
  Ntapi.ntmmapi in 'NtUtils\Headers\Ntapi.ntmmapi.pas',
  Ntapi.ntpebteb in 'NtUtils\Headers\Ntapi.ntpebteb.pas',
  Ntapi.ntrtl in 'NtUtils\Headers\Ntapi.ntrtl.pas',
  Ntapi.ntstatus in 'NtUtils\Headers\Ntapi.ntstatus.pas',
  Ntapi.nttmapi in 'NtUtils\Headers\Ntapi.nttmapi.pas',
  Winapi.NtSecApi in 'NtUtils\Headers\Winapi.NtSecApi.pas',
  Winapi.WinBase in 'NtUtils\Headers\Winapi.WinBase.pas',
  Winapi.WinNt in 'NtUtils\Headers\Winapi.WinNt.pas';

var
  hTransaction: THandle = 0;

{$IFDEF Debug}
function ReportState(Reason: Integer): NTSTATUS;
var
  TidStr: UNICODE_STRING;
  Buffer: array [0..10] of WideChar;
begin
  Result := STATUS_SUCCESS;

  case Reason of
    DLL_PROCESS_ATTACH:
      OutputDebugStringW('TmTxTracker: attached to process');

    DLL_PROCESS_DETACH:
      OutputDebugStringW('TmTxTracker: detached from process');

    DLL_THREAD_ATTACH, DLL_THREAD_DETACH:
      begin
        TidStr.Length := 0;
        TidStr.MaximumLength := SizeOf(Buffer);
        TidStr.Buffer := PWideChar(@Buffer);
        FillChar(Buffer, SizeOf(Buffer), 0);

        Result := RtlIntegerToUnicodeString(Cardinal(
          NtCurrentTeb.ClientId.UniqueThread), 10, TidStr);

        case Reason of
          DLL_THREAD_ATTACH:
            OutputDebugStringW(PWideChar('TmTxTracker: attached to thread ' +
              TidStr.ToString));

          DLL_THREAD_DETACH:
            OutputDebugStringW(PWideChar('TmTxTracker: detached from thread ' +
              TidStr.ToString));
        end;
      end;
  end;
end;
{$ENDIF}

procedure DllMain(Reason: Integer);
begin
  if (Reason = DLL_THREAD_ATTACH) and (hTransaction <> 0) then
    RtlSetCurrentTransaction(hTransaction);

  {$IFDEF Debug} ReportState(Reason); {$ENDIF}
end;

function SetFutureTranasction(TransactionHandle: THandle): NTSTATUS; stdcall;
begin
  hTransaction := TransactionHandle;
  Result := STATUS_SUCCESS;

  {$IFDEF Debug} OutputDebugStringW('Setting future transaction'); {$ENDIF}
end;

function GetFutureTranasction(Reserved: NativeUInt): THandle; stdcall;
begin
  Result := hTransaction;
end;

exports
  SetFutureTranasction, GetFutureTranasction;

begin
  DllProc := DllMain;
  DllMain(DLL_PROCESS_ATTACH);
end.
