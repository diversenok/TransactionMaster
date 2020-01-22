unit TmTxTrackerUtils;

interface

uses
  Winapi.WinNt, Ntapi.ntpsapi, NtUtils.Exceptions, NtUtils.Ldr,
  NtUtils.Shellcode.Dll, NtUtils.Processes.Modules;

const
  PROCESS_ENUMERATE_MODULES = PROCESS_ENUMERATE_MODULES;
  PROCESS_INJECT_DLL = PROCESS_INJECT_DLL;
  PROCESS_SET_FUTURE_TRANASCTION = PROCESS_VM_WRITE;

  TRACKER_DLL_32 = 'TmTxTracker32.dll';
  TRACKER_DLL_64 = 'TmTxTracker64.dll';

  TRACKER_VAR = 'FutureTransaction';
  TRACKER_TIMEOUT = 1000 * MILLISEC;

type
  TTmTxTracker = record
    DllBase: Pointer;
    WoW64: Boolean;
  end;

function FindTmTxTracker(hProcess: THandle; out Instance: TTmTxTracker):
  TNtxStatus;

function InjectTmTxTracker(hProcess: THandle): TNtxStatus;

function GetFutureTransaction(hProcess: THandle; Instance: TTmTxTracker;
  out Value: NativeUInt): TNtxStatus;

function SetFutureTransaction(hProcess: THandle; Instance: TTmTxTracker;
  Value: NativeUInt): TNtxStatus;

implementation

uses
  System.SysUtils, Ntapi.ntstatus, NtUtils.Sections, NtUtils.ImageHlp,
  NtUtils.Files, NtUtils.Processes, NtUtils.Processes.Memory;

type
  TTrackerDll = record
    Initialized: Boolean;
    Status: TNtxStatus;
    NtPath: String;
    HandleVA: Cardinal;
  end;
  PTrackerDll = ^TTrackerDll;

var
  TrackerDll32, TrackerDll64: TTrackerDll;

function IsX86(WoW64: Boolean): Boolean;
begin
  {$IFDEF Win32}
  // If we run on 32-bit Windows, use the 32 bit DLL
  if not RtlIsWoW64 then
    Result := True;
  {$ELSE}
  Result := WoW64;
  {$ENDIF}
end;

function GetDllInfo(WoW64: Boolean): PTrackerDll;
begin
  if IsX86(WoW64) then
    Result := @TrackerDll32
  else
    Result := @TrackerDll64;
end;

function GetDllLocation(WoW64: Boolean): String;
begin
  if IsX86(WoW64) then
    Result := ExtractFilePath(ParamStr(0)) + TRACKER_DLL_32
  else
    Result := ExtractFilePath(ParamStr(0)) + TRACKER_DLL_64;
end;

function InitializeDllInfo(WoW64: Boolean): TNtxStatus;
var
  Info: PTrackerDll;
  hxSection: IHandle;
  MappedMemory: IMemory;
  Entries: TArray<TExportEntry>;
  pEntry: PExportEntry;
begin
  Info := GetDllInfo(WoW64);

  if Info.Initialized then
    Exit(Info.Status);

  Info.Initialized := True;
  Info.Status := RtlxDosPathToNtPath(GetDllLocation(WoW64), Info.NtPath);

  if not Info.Status.IsSuccess then
    Exit(Info.Status);

  // Map the DLL
  Info.Status := RtlxMapReadonlyFile(hxSection, Info.NtPath, MappedMemory);

  if not Info.Status.IsSuccess then
    Exit(Info.Status);

  // Parse export
  Info.Status := RtlxEnumerateExportImage(MappedMemory.Address,
    MappedMemory.Size, False, Entries);

  pEntry := RtlxFindExportedName(Entries, TRACKER_VAR);

  if Assigned(pEntry) and pEntry.ValidAddress then
  begin
    Info.HandleVA := pEntry.VirtualAddress;
    Info.Status.Status := STATUS_SUCCESS;
  end
  else
  begin
    Info.Status.Location := 'InitializeTrackerInfo';
    Info.Status.Status := STATUS_PROCEDURE_NOT_FOUND;
  end;

  Result := Info.Status;
end;

function FindTmTxTracker(hProcess: THandle; out Instance: TTmTxTracker)
  : TNtxStatus;
var
  Modules: TArray<TModuleEntry>;
  i: Integer;
begin
  Result := NtxEnumerateModulesProcess(hProcess, Modules, @Instance.WoW64);

  if not Result.IsSuccess then
    Exit;

  Instance.DllBase := nil;

  for i := 0 to High(Modules) do
    if (Modules[i].BaseDllName = TRACKER_DLL_64) or
      (Modules[i].BaseDllName = TRACKER_DLL_32) then
    begin
      Instance.DllBase := Modules[i].DllBase;
      Break;
    end;
end;

function InjectTmTxTracker(hProcess: THandle): TNtxStatus;
var
  WoW64: Boolean;
begin
  Result := NtxQueryIsWoW64Process(hProcess, WoW64);

  if Result.IsSuccess then
    Result := RtlxInjectDllProcessEx(hProcess, GetDllLocation(WoW64));
end;

function GetFutureTransaction(hProcess: THandle; Instance: TTmTxTracker;
  out Value: NativeUInt): TNtxStatus;
begin
  Result := InitializeDllInfo(Instance.WoW64);

  if Result.IsSuccess then
    Result := NtxMemory.Read(hProcess, Pointer(NativeUInt(Instance.DllBase) +
      GetDllInfo(Instance.WoW64).HandleVA), Value);
end;

function SetFutureTransaction(hProcess: THandle; Instance: TTmTxTracker;
  Value: NativeUInt): TNtxStatus;
begin
  Result := InitializeDllInfo(Instance.WoW64);

  if Result.IsSuccess then
    Result := NtxMemory.Write(hProcess, Pointer(NativeUInt(Instance.DllBase) +
      GetDllInfo(Instance.WoW64).HandleVA), Value);
end;

end.
