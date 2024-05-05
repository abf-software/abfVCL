{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}

{$IfDef abfVCLTrial}

//==============================================================================
// Private types
//==============================================================================
type
  TLoadLibrary = function(lpLibFileName: LPCSTR): HMODULE; stdcall;
  TGetProcAddress = function(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
  TFreeLibrary = function(hLibModule: HMODULE): BOOL; stdcall;

  TGetWindowModuleFileName = function(hwnd: HWND; pszFileName: LPWSTR;
    cchFileNameMax: UINT): UINT; stdcall;

  PInjectionRecord = ^TInjectionRecord;
  TInjectionRecord = record
    pLoadLibrary: TLoadLibrary;
    pGetProcAddress: TGetProcAddress;
    pFreeLibrary: TFreeLibrary;
    User32ModuleName: array[0..10] of Char; //Length('user32.dll#0')
    GWMFN_ProcName: array[0..24] of Char;   //Length('GetWindowModuleFileNameW#0')
    GWMFN_Result: DWORD;
    GWMFN_Buf: array[0..High(Smallint)] of WideChar;
    AWindowHandle: THandle;
  end;

//==============================================================================
// Private variables
//==============================================================================

var
  _TAList: TList = nil;
  _TABList: TList = nil;

//==============================================================================
// Private routines
//==============================================================================

function ShellExecuteA(hWnd: HWND; Operation, FileName, Parameters, Directory: PChar;
  ShowCmd: Integer): HINST; stdcall; external 'shell32.dll' name 'ShellExecuteA';

//------------------------------------------------------------------------------

function _InjectionFunc(AParam: Cardinal): Bool; stdcall;
var
  TempLibHandle: THandle;
  GWMFN: TGetWindowModuleFileName;
begin
  Result := True;

  with PInjectionRecord(AParam)^ do
  begin
    TempLibHandle := pLoadLibrary(@User32ModuleName);
    if TempLibHandle <> 0 then
    begin
      @GWMFN := pGetProcAddress(TempLibHandle, @GWMFN_ProcName);
      if Assigned(@GWMFN) then
        GWMFN_Result := GWMFN(AWindowHandle, @GWMFN_Buf[0], Length(GWMFN_Buf));

      pFreeLibrary(TempLibHandle);
    end;
  end;
end;

procedure _DummyProc; stdcall;
begin
end;

//------------------------------------------------------------------------------

function _LowerCase(AWS: Pointer): DWORD;
var
  WS: WideString;
  WSLen: DWORD;
  S: string;
begin
  WS := WideString(PWideChar(AWS));
  WSLen := Length(WS);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CharLowerBuffW(AWS, WSLen)
  else
    begin
      S := WideString(PWideChar(AWS));
      Result := CharLowerBuff(Pointer(S), Length(S));
      WS := S;
      Move(WS[1], AWS^, WSLen);
    end;
end;

//------------------------------------------------------------------------------

function _SetDebugPrivilege(APID: Cardinal): Bool;
var
  hToken, hProcess: THandle;
  tp, prev_tp: TTokenPrivileges;
  Len: DWORD;
begin
  Result := False;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, APID);
    if hProcess = 0 then
      RaiseLastWin32Error;
    try
      if not OpenProcessToken(hProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
        hToken) then Exit;
    finally
      CloseHandle(hProcess);
    end;
    try
      if not LookupPrivilegeValue('', 'SeDebugPrivilege',
        tp.Privileges[0].Luid) then Exit;
      tp.PrivilegeCount := 1;
      tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      if not AdjustTokenPrivileges(hToken, False, tp, Sizeof(prev_tp),
        prev_tp, Len) then Exit;
      Result := True;
    finally
      CloseHandle(hToken);
    end;
  end;
end;

//------------------------------------------------------------------------------

function _GWMFN(AHandle: HWND): WideString;
var
  TempLibHandle: THandle;
  TempProcessHandle, TempThreadHandle: THandle;
  TempProcessID, TempThreadID: DWORD;
  TempProcSize: DWORD;
  TempProc: Pointer;
  TempLocalData: PInjectionRecord;
  TempData: PInjectionRecord;
  c: Cardinal;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
    GetMem(TempLocalData, SizeOf(TempLocalData^));
    try
      FillChar(TempLocalData^, SizeOf(TempLocalData^), 0);
      
      with TempLocalData^ do
      begin
        GetWindowModuleFileName(AHandle, @GWMFN_Buf[0], Length(GWMFN_Buf));

        Result := PWideChar(@GWMFN_Buf);
      end;
    finally
      FreeMem(TempLocalData)
    end;

    Exit;
  end;

  _SetDebugPrivilege(GetCurrentProcessID);
  GetWindowThreadProcessId(AHandle, @TempProcessID);

  TempProcessHandle := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_VM_OPERATION or
	 	PROCESS_VM_WRITE or PROCESS_VM_READ, False, TempProcessID);
  if TempProcessHandle = 0 then Exit;

  TempProc := nil;
  TempData := nil;
  try
  // allocate memory
    TempProcSize := Cardinal(@_DummyProc) - Cardinal(@_InjectionFunc);
	  TempProc := VirtualAllocEx(TempProcessHandle, nil, TempProcSize, MEM_COMMIT,
      PAGE_EXECUTE_READWRITE);
    if not Assigned(TempProc) then Exit;

    TempData := VirtualAllocEx(TempProcessHandle, nil, SizeOf(TInjectionRecord),
      MEM_COMMIT, PAGE_READWRITE);
    if not Assigned(TempData) then Exit;

    // copy function there
    if not WriteProcessMemory(TempProcessHandle, TempProc, @_InjectionFunc,
      TempProcSize, c) then Exit;

    GetMem(TempLocalData, SizeOf(TempLocalData^));
    try
      FillChar(TempLocalData^, SizeOf(TempLocalData^), 0);

      with TempLocalData^ do
      begin
        User32ModuleName := 'user32.dll';
        GWMFN_ProcName := 'GetWindowModuleFileNameW';
        AWindowHandle := AHandle;

        TempLibHandle := LoadLibrary('kernel32.dll');
        try
          @pLoadLibrary := GetProcAddress(TempLibHandle, 'LoadLibraryA');
          @pGetProcAddress := GetProcAddress(TempLibHandle, 'GetProcAddress');
          @pFreeLibrary := GetProcAddress(TempLibHandle, 'FreeLibrary');
        finally
          FreeLibrary(TempLibHandle);
        end;
      end;

      if not WriteProcessMemory(TempProcessHandle, TempData, TempLocalData,
        SizeOf(TInjectionRecord), c) then Exit;

      TempThreadHandle := CreateRemoteThread(TempProcessHandle, nil, 0, TempProc,
        TempData, 0, TempThreadID);
      try
        if WaitForSingleObject(TempThreadHandle, 3000) = WAIT_OBJECT_0 then
        begin
        // this might just have worked, pick up the result!
          if not ReadProcessMemory(TempProcessHandle, TempData, TempLocalData,
            SizeOf(TInjectionRecord), c) then Exit;

          Result := PWideChar(@TempLocalData^.GWMFN_Buf);
        end;
      finally
        CloseHandle(TempThreadHandle);
      end;
    finally
      FreeMem(TempLocalData);
    end;
  finally
    if Assigned(TempProc) then VirtualFreeEx(TempProcessHandle, TempProc, 0,
      MEM_RELEASE);
    if Assigned(TempData) then VirtualFreeEx(TempProcessHandle, TempData, 0,
      MEM_RELEASE);
    CloseHandle(TempProcessHandle);
  end;
end;

//------------------------------------------------------------------------------

function _X(const S: string; B: Byte): string;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    Result[i] := Char(Byte(Result[i]) xor B);
end;

//------------------------------------------------------------------------------

function _AllocWideString(const AWideStr: WideString): Pointer;
var
  BufSize: Integer;
begin
  BufSize := Length(AWideStr) * 2;
  GetMem(Result, BufSize + 2);
  FillChar(Result^, BufSize + 2, 0);
  Move(AWideStr[1], Result^, BufSize);
  _LowerCase(Result);
end;

//------------------------------------------------------------------------------

function _ETW(AHandle: HWND; lParam: LPARAM): Bool; stdcall;
const
  CN1 = 'Builder';
  CN0 = 'TApplication';
var
  TempBuf: PChar;
begin
  Result := True;

  GetMem(TempBuf, MAX_PATH);
  try
    FillChar(TempBuf^, MAX_PATH, 0);
    if GetClassName(AHandle, TempBuf, MAX_PATH) > 0 then
    begin
      if string(TempBuf) = CN0 then
        _TAList.Add(_AllocWideString(_GWMFN(AHandle))) else
      if string(TempBuf) = (Copy(CN0, 1, 4) + CN1) then
        _TABList.Add(_AllocWideString(_GWMFN(AHandle)));
    end;
  finally
    FreeMem(TempBuf);
  end;
end;

{$EndIf abfVCLTrial}
//==============================================================================
// Public routines
//==============================================================================

function _ASK: Boolean;
{$IfDef abfVCLTrial}
const
  {$IfDef D2_ONLY} M = 'vcl20';  {$EndIf}
  {$IfDef D3_ONLY} M = 'vcl30';  {$EndIf}
  {$IfDef D4_ONLY} M = 'vcl40';  {$EndIf}
  {$IfDef D5_ONLY} M = 'vcl50';  {$EndIf}
  {$IfDef D6_ONLY} M = 'vcl60';  {$EndIf}
  {$IfDef D7_ONLY} M = 'vcl70';  {$EndIf}
  {$IfDef D8_ONLY} M = 'vcl80';  {$EndIf}
  {$IfDef D9_ONLY} M = 'vcl90';  {$EndIf}
  {$IfDef D10_ONLY}M = 'vcl100'; {$EndIf}
  {$IfDef D11_ONLY}M = 'vcl100'; {$EndIf}
  {$IfDef C1_ONLY} M = 'vcl';    {$EndIf}
  {$IfDef C3_ONLY} M = 'vcl30';  {$EndIf}
  {$IfDef C4_ONLY} M = 'vcl40';  {$EndIf}
  {$IfDef C5_ONLY} M = 'vcl50';  {$EndIf}
  {$IfDef C6_ONLY} M = 'vcl60';  {$EndIf}
  {$IfDef C10_ONLY}M = 'vcl100'; {$EndIf}
  {$IfDef C11_ONLY}M = 'vcl100'; {$EndIf}
var
  TempTAB: WideString;
  TempTA: WideString;
  i, a, TempPos: Integer;
{$EndIf abfVCLTrial}
begin
{$IfDef abfVCLTrial}
  Result := False;

  _TAList := TList.Create;
  try
    _TABList := TList.Create;
    try
      EnumWindows(@_ETW, 0);

      for i := 0 to _TABList.Count - 1 do
      begin
        TempTAB := PWideChar(_TABList[i]);

        for a := 0 to _TAList.Count - 1 do
        begin
          TempTA := PWideChar(_TAList[a]);

          if TempTAB = TempTA then
          begin
            repeat
              TempPos := Pos('\', TempTAB);
              Delete(TempTAB, 1, TempPos);
            until TempPos < 1;

            TempPos := Pos('.', TempTAB);
            Delete(TempTAB, TempPos, MaxInt);

            Result := TempTAB = M;

            if Result then Exit;
          end;
        end;
      end;
    finally
      while _TABList.Count > 0 do
      begin
        FreeMem(_TABList[0]);
        _TABList.Delete(0);
      end;
      _TABList.Free;
    end;
  finally
    while _TAList.Count > 0 do
    begin
      FreeMem(_TAList[0]);
      _TAList.Delete(0);
    end;

    _TAList.Free;
  end;
{$Else abfVCLTrial}
  Result := True;
{$EndIf abfVCLTrial}
end;

//------------------------------------------------------------------------------
// Hack message with halt.

procedure _TADA;
begin
{$IfDef abfVCLTrial}
// 'This application uses a HACKED version of the ABF software, Inc. product.' + CRLF +
// 'STOP using it! It is ILLEGAL! Register it! Do you want to register it right now?';
  if MessageBoxEx(0,
    PChar(_X('Iutn=|mmqt~|itrs=hnxn=|=U\^VXY=kxontrs=r{=iux=\_[=nr{ij|ox1=Ts~3=moryh~i3', 29) + #13#10 +
          _X('NIRM=hntsz=ti<=Ti=tn=TQQXZ\Q<=Oxztnixo=ti<=Yr=drh=j|si=ir=oxztnixo=ti=otzui=srj"', 29)),
    PChar(_X('Tpmroi|si', 29)), //Important
    MB_YESNO or MB_ICONSTOP, 0) = IDYES then
      ShellExecuteA(0, PChar(_X('hwbi', 7)),
        PChar(_X('ossw=((ppp)fea*cbq)dhj', 7)),  //http://www.abf-dev.com
        nil, nil, SW_SHOW);
  Halt;
{$EndIf abfVCLTrial}
end;

//------------------------------------------------------------------------------
// Default hack check

procedure _GlobalInit;
begin
{$IfDef abfVCLTrial}
  if not _ASK then _TADA;
{$EndIf abfVCLTrial}
end;
