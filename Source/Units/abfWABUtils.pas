{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfWABUtils;

{$I abf.inc}

interface

uses
  Windows, Classes, WabApi, WabDefs, WabIab;

{*******************************************************************************
  Units WabDefs, WabApi, WabIab, WabTags, WabCode are used in the abfWAB
product. These unints are part of the Windows Address Book (WAB) functions
interfaces, taken from <http://delphi-jedi.org> site. You can get more
information about these units and the Project JEDI at <http://delphi-jedi.org>
*******************************************************************************}

{$IFDEF C3}
(*$HPPEMIT 'typedef System::DelphiInterface<IMAPITable> _di_IMAPITable;'*)
(*$HPPEMIT 'typedef System::DelphiInterface<IMailUser> _di_IMailUser;'*)
(*$HPPEMIT 'typedef System::DelphiInterface<IDistList> _di_IDistList;'*)
(*$HPPEMIT 'typedef System::DelphiInterface<IABContainer> _di_IABContainer;'*)
(*$HPPEMIT 'typedef System::DelphiInterface<IAddrBook> _di_IAddrBook;'*)
(*$HPPEMIT 'typedef System::DelphiInterface<IMAPIAdviseSink> _di_IMAPIAdviseSink;'*)
(*$HPPEMIT 'typedef System::DelphiInterface<IWABObject> _di_IWabObject;'*)
(*$HPPEMIT 'typedef LPWABOPEN TWABOpen;'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfNewSBinary(LPSBinary &P, unsigned Size);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfNewSBinary2(const _di_IWabObject AWabObject, LPSBinary &P, unsigned Size);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfFreeSBinary(LPSBinary &P);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfFreeSBinary2(const _di_IWabObject AWabObject, LPSBinary &P);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfMoveSBinary(const LPSBinary Source, LPSBinary &Dest);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfMoveSBinary2(const _di_IWabObject AWabObject, const LPSBinary Source, LPSBinary &Dest);'*)
(*$HPPEMIT 'extern PACKAGE AnsiString __fastcall abfEntryIDToString(LPSBinary P);'*)
(*$HPPEMIT 'extern PACKAGE LPSBinary __fastcall abfStringToEntryID(AnsiString IdString);'*)
(*$HPPEMIT 'extern PACKAGE bool __fastcall abfIsEmptyEntryID(const LPSBinary P);'*)
(*$HPPEMIT 'extern PACKAGE bool __fastcall abfIfPropExists(const LPSPropValue PropValue);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfCopyWABStringsArray(const LPSPropValue PropValue);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfFreeMVbinArray(const _di_IWabObject AWabObject, LPENTRYLIST &AArray);'*)
(*$HPPEMIT 'extern PACKAGE bool __fastcall abfReplaceMVbinArray(const _di_IWabObject AWabObject, const LPENTRYLIST ASrcArray, LPENTRYLIST &ADestArray);'*)
(*$HPPEMIT 'extern PACKAGE bool __fastcall abfIsEqualSBinaryValues(const LPSBinary AValue1, const LPSBinary AValue2);'*)
(*$HPPEMIT 'extern PACKAGE bool __fastcall abfIfSBinaryExistsInArray(const LPSBinary AValue, const LPENTRYLIST AArray);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfFreeSRowSet(const _di_IWabObject AWabObject, LPSRowSet &P);'*)
(*$HPPEMIT 'extern PACKAGE void __fastcall abfFreePAdrList(const _di_IWabObject AWabObject, LPADRLIST &P);'*)
{$ENDIF}

  procedure abfNewSBinary(var P: PSBinary; Size: ULONG);
  {$IfDef C3}{$EXTERNALSYM abfNewSBinary}{$EndIf C3}
  procedure abfNewSBinary2(const AWabObject: IWabObject;
    var P: PSBinary; Size: ULONG);
  {$IfDef C3}{$EXTERNALSYM abfNewSBinary2}{$EndIf C3}
  procedure abfFreeSBinary(var P: PSBinary);
  {$IfDef C3}{$EXTERNALSYM abfFreeSBinary}{$EndIf C3}
  procedure abfFreeSBinary2(const AWabObject: IWabObject; var P: PSBinary);
  {$IfDef C3}{$EXTERNALSYM abfFreeSBinary2}{$EndIf C3}  
  procedure abfMoveSBinary(const Source: PSBinary; var Dest: PSBinary);
  {$IfDef C3}{$EXTERNALSYM abfMoveSBinary}{$EndIf C3}
  procedure abfMoveSBinary2(const AWabObject: IWabObject;
    const Source: PSBinary; var Dest: PSBinary);
  {$IfDef C3}{$EXTERNALSYM abfMoveSBinary2}{$EndIf C3}
  function abfEntryIDToString(P: PSBinary): string;
  {$IfDef C3}{$EXTERNALSYM abfEntryIDToString}{$EndIf C3}
  function abfStringToEntryID(IdString: string): PSBinary;
  {$IfDef C3}{$EXTERNALSYM abfStringToEntryID}{$EndIf C3}
  function abfIsEmptyEntryID(const P: PSBinary): Boolean;
  {$IfDef C3}{$EXTERNALSYM abfIsEmptyEntryID}{$EndIf C3}
  function abfIfWABPropExists(const PropValue: TSPropValue): Boolean;
  {$IfDef C3}{$EXTERNALSYM abfIfWABPropExists}{$EndIf C3}
  procedure abfCopyWABStringsArray(const PropValue: TSPropValue;
    const AStrings: TStrings);
  {$IfDef C3}{$EXTERNALSYM abfCopyWABStringsArray}{$EndIf C3}
  procedure abfFreeMVbinArray(const AWabObject: IWabObject;
    var AArray: PSBinaryArray);
  {$IfDef C3}{$EXTERNALSYM abfFreeMVbinArray}{$EndIf C3}
  function abfReplaceMVbinArray(const AWabObject: IWabObject;
    const ASrcArray: TSBinaryArray; var ADestArray: TSBinaryArray): Boolean;
  {$IfDef C3}{$EXTERNALSYM abfReplaceMVbinArray}{$EndIf C3}
  function abfIsEqualSBinaryValues(const AValue1, AValue2: PSBinary): Boolean;
  {$IfDef C3}{$EXTERNALSYM abfIsEqualSBinaryValues}{$EndIf C3}
  function abfIfSBinaryExistsInArray(const AValue: PSBinary;
    const AArray: TSBinaryArray): Boolean;
  {$IfDef C3}{$EXTERNALSYM abfIfSBinaryExistsInArray}{$EndIf C3}

  function abfCombineMVbinArrays(const AWabObject: IWabObject;
    const AArray1, AArray2: TSBinaryArray;
    var AResultArray: TSBinaryArray): Boolean;

  function abfGetWABVersion(var Version: string): Boolean;
  function abfGetWabDllPath(var WABDllPath: string): Boolean; overload;
  function abfGetWabDllPath(var WABDllPath: WideString): Boolean; overload;
  function abfGetLastUserIdentityGUID(var GUIDString: string): Boolean;
  function abfIsOutlookSharedMode: Boolean;
  function abfSetOutlookSharedMode(Value: Boolean): Boolean;
  function abfGetRefCountWABDll: DWORD;
  function abfIncRefCountWABDll: DWORD;
  function abfDecRefCountWABDll: DWORD;
  procedure abfFreeSRowSet(const AWabObject: IWabObject; var P: PSRowSet);
  {$IfDef C3}{$EXTERNALSYM abfFreeSRowSet}{$EndIf C3}
  procedure abfFreePAdrList(const AWabObject: IWabObject; var P: PAdrList);
  {$IfDef C3}{$EXTERNALSYM abfFreePAdrList}{$EndIf C3}

{******************************************************************************}
implementation
{******************************************************************************}

uses
  SysUtils, abfSysUtils,
  abfClasses, abfWABConsts;

//==============================================================================
// Registry keys
//==============================================================================
const
  WAB_SHARED_DLLS       = 'Software\Microsoft\Windows\CurrentVersion\SharedDLLs'; //HKEY_LOCAL_MACHINE
  WAB_VERSION_PATH      = 'Software\Microsoft\WAB\Version Info';                  //HKEY_LOCAL_MACHINE
  WAB_VERSION_KEY       = 'Current';
  WAB_USEOUTLOOK_PATH   = 'Software\Microsoft\WAB\WAB4';                          //HKEY_CURRENT_USER
  WAB_USEOUTLOOK_KEY    = 'UseOutlook';
  WAB_OUTLOOK_DLL_PATH  = 'Software\Microsoft\WAB\OutlwabDLLPath';                //HKEY_LOCAL_MACHINE
  WAB_OUTLOOK_DLL_NAME  = 'OUTLWAB.DLL';
  WAB_LAST_USER_ID_PATH = 'Identities';                                           //HKEY_CURRENT_USER
  WAB_LAST_USER_ID_KEY  = 'Last User ID';

//==============================================================================
// Private types
//==============================================================================

type
  PFakePCharArr = ^TFakePCharArr;
  TFakePCharArr = array[0..0] of PChar;

//==============================================================================
// Public routines
//==============================================================================

procedure abfNewSBinary(var P: PSBinary; Size: ULONG);
begin
  New(P);
  P.cb := Size;
  GetMem(P.lpb, P.cb);
end;

//------------------------------------------------------------------------------

procedure abfNewSBinary2(const AWabObject: IWabObject;
  var P: PSBinary; Size: ULONG);
begin
  if not Assigned(AWabObject) then Exit;

  AWabObject.AllocateBuffer(SizeOf(P^), Pointer(P));
  ZeroMemory(P, SizeOf(P^));

  if Size <= 0 then Exit;

  P.cb := Size;
  AWabObject.AllocateMore(P.cb, Pointer(P), P.lpb);
end;

//------------------------------------------------------------------------------

procedure abfFreeSBinary(var P: PSBinary);
begin
  if Assigned(P) then
  begin
    if not IsBadWritePtr(P.lpb, P.cb) then
    try
      FreeMem(P.lpb, P.cb);
    except
    end;

    if not IsBadWritePtr(P, SizeOf(P)) then
    try
      Dispose(P);
    except
    end;
    P := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure abfFreeSBinary2(const AWabObject: IWabObject; var P: PSBinary);
begin
  if not Assigned(AWabObject) or (P = nil) then Exit;

  if AWabObject.FreeBuffer(P) <> S_OK then
    abfFreeSBinary(P);
  P := nil;
end;

//------------------------------------------------------------------------------

procedure abfMoveSBinary(const Source: PSBinary; var Dest: PSBinary);
begin
  if not Assigned(Source) then Exit;

  abfFreeSBinary(Dest);
  abfNewSBinary(Dest, Source.cb);

  CopyMemory(Dest.lpb, Source.lpb, Dest.cb);
end;

//------------------------------------------------------------------------------

procedure abfMoveSBinary2(const AWabObject: IWabObject;
  const Source: PSBinary; var Dest: PSBinary);
begin
  if not Assigned(AWabObject) then Exit;
  if not Assigned(Source) then Exit;

  abfFreeSBinary2(AWabObject, Dest);
  abfNewSBinary2(AWabObject, Dest, Source.cb);

  CopyMemory(Dest.lpb, Source.lpb, Dest.cb);
end;

//------------------------------------------------------------------------------

procedure abfFreeSRowSet(const AWabObject: IWabObject; var P: PSRowSet);
var
  i: Integer;
begin
  if not Assigned(AWabObject) or (P = nil) then Exit;

  for i := 0 to P^.cRows - 1 do
    AWabObject.FreeBuffer(P^.aRow[i].lpProps);
  AWabObject.FreeBuffer(P);
  P := nil;
end;

//------------------------------------------------------------------------------

procedure abfFreePAdrList(const AWabObject: IWabObject; var P: PAdrList);
var
  i: Integer;
begin
  if not Assigned(AWabObject) or (P = nil) then Exit;
  for i := 0 to P^.cEntries - 1 do
  begin
    if @(P^.aEntries[i]) <> nil then
      AWabObject.FreeBuffer(P^.aEntries[i].rgPropVals);
  end;
  AWabObject.FreeBuffer(P);
  P := nil;
end;

//------------------------------------------------------------------------------

function abfEntryIDToString(P: PSBinary): string;
var
  PByte: ^Byte;
  i: Integer;
begin
  Result := '';
  PByte := P.lpb;
  for i := 0 to P.cb - 1 do
  begin
    Result := Result + IntToHex(PByte^, 2);
    PByte := Pointer(Cardinal(PByte) + 1);
  end;
end;

//------------------------------------------------------------------------------

function abfStringToEntryID(IdString: string): PSBinary;
var
  i, Len: Integer;
  PByte: ^Byte;
begin
  Len := Length(IdString);
  if (Len mod 2) = 1 then
  begin
    IdString := IdString + '0';
    Inc(Len);
  end;
  abfNewSBinary(Result, Len div 2);
  PByte := Result.lpb;
  i := 1;
  while i < Len  do
  begin
    PByte^ := StrToInt('$' + IdString[i] + IdString[i + 1]);
    PByte := Pointer(Cardinal(PByte) + 1);
    Inc(i, 2);
  end;
end;

//------------------------------------------------------------------------------

function abfIsEmptyEntryID(const P: PSBinary): Boolean;
begin
  Result := (P = nil) or (P.lpb = nil) or (P.cb = 0);
end;

//------------------------------------------------------------------------------

function abfIfWABPropExists(const PropValue: TSPropValue): Boolean;
begin
  Result := PROP_TYPE(PropValue.ulPropTag) <> PT_ERROR;
end;

//------------------------------------------------------------------------------

procedure abfCopyWABStringsArray(const PropValue: TSPropValue;
  const AStrings: TStrings);
var
  PTempArr: PFakePCharArr;
  i: Integer;
begin
  if abfIfWABPropExists(PropValue) then
  begin
    PTempArr := Pointer(PropValue.Value.MVszA.lppszA);
    for i := 0 to PropValue.Value.MVszA.cValues - 1 do
      AStrings.Add(PTempArr[i]);
  end;
end;

//------------------------------------------------------------------------------

procedure abfFreeMVbinArray(const AWabObject: IWabObject;
  var AArray: PSBinaryArray);
begin
  if not Assigned(AWabObject) then Exit;
  if not Assigned(AArray) then Exit;

  AWabObject.FreeBuffer(AArray.lpbin);
  AWabObject.FreeBuffer(AArray);
end;

//------------------------------------------------------------------------------

function abfReplaceMVbinArray(const AWabObject: IWabObject;
  const ASrcArray: TSBinaryArray; var ADestArray: TSBinaryArray): Boolean;
var
  PSB: PSBinary;
  P: Pointer;
  i: Integer;
begin
  Result := False;
  if not Assigned(AWabObject) then Exit;

  AWabObject.FreeBuffer(ADestArray.lpbin);
  ADestArray.lpbin := nil;

  ADestArray.cValues := ASrcArray.cValues;
  if ADestArray.cValues = 0 then
  begin
    Result := True;
    Exit;
  end;

  AWabObject.AllocateBuffer(ADestArray.cValues * SizeOf(TSBinary),
    Pointer(ADestArray.lpbin));
  CopyMemory(ADestArray.lpbin, ASrcArray.lpbin,
    ADestArray.cValues * SizeOf(TSBinary));
  for i := 0 to ADestArray.cValues do
  begin
    PSB := Pointer(Cardinal(ADestArray.lpbin) + (Cardinal(i) * SizeOf(TSBinary)));
    P := PSB.lpb;
    PSB.lpb := nil;
    if PSB.cb = 0 then Continue;

    AWabObject.AllocateMore(PSB.cb, ADestArray.lpbin, PSB.lpb);
    CopyMemory(PSB.lpb, P, PSB.cb);
  end;

  Result := True;
end;

//------------------------------------------------------------------------------

function abfIsEqualSBinaryValues(const AValue1, AValue2: PSBinary): Boolean;
begin
  Result := False;

  if AValue1 = AValue2 then
  begin
    Result := True;
    Exit;
  end;

  if not (Assigned(AValue1) and Assigned(AValue2)) then Exit;

  Result := (AValue1.cb = AValue2.cb)
    and CompareMem(AValue1.lpb, AValue2.lpb, AValue1.cb);
end;

//------------------------------------------------------------------------------

function abfIfSBinaryExistsInArray(const AValue: PSBinary;
  const AArray: TSBinaryArray): Boolean;
var
  i: Integer;
  PSB: PSBinary;
begin
  Result := False;
  if not Assigned(AValue) or IsBadWritePtr(AValue, SizeOf(TSBinary)) then Exit;

  for i := 0 to AArray.cValues - 1 do
  begin
    PSB := Pointer(Cardinal(AArray.lpbin) + (Cardinal(i) * SizeOf(TSBinary)));
    if abfIsEqualSBinaryValues(AValue, PSB) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------

function abfCombineMVbinArrays(const AWabObject: IWabObject;
  const AArray1, AArray2: TSBinaryArray;
  var AResultArray: TSBinaryArray): Boolean;
var
  i, a: Integer;
  SaveCount: ULONG;
  PSB1, PSB2: PSBinary;
  TempArr: TSBinaryArray;
begin
  Result := False;

  if (not Assigned(AArray2.lpbin)) or (AArray2.cValues = 0) then
  begin
    Result := abfReplaceMVbinArray(AWabObject, AArray1, AResultArray);
    Exit;
  end;

  if (not Assigned(AArray1.lpbin)) or (AArray1.cValues = 0) then
  begin
    Result := abfReplaceMVbinArray(AWabObject, AArray2, AResultArray);
    Exit;
  end;

  TempArr.cValues := AArray1.cValues + AArray2.cValues;
  a := 0;

  AWabObject.AllocateBuffer(TempArr.cValues * SizeOf(TSBinary),
    Pointer(TempArr.lpbin));
  try
    ZeroMemory(TempArr.lpbin, TempArr.cValues * SizeOf(TSBinary));
    for i := 0 to AArray1.cValues - 1 do
    begin
      PSB1 := Pointer(Cardinal(AArray1.lpbin) + (Cardinal(i) * SizeOf(TSBinary)));
      PSB2 := Pointer(Cardinal(TempArr.lpbin) + (Cardinal(a) * SizeOf(TSBinary)));
      if not abfIfSBinaryExistsInArray(PSB1, AArray2) then
      begin
        PSB2.cb := PSB1.cb;
        if PSB2.cb > 0 then
        begin
          AWabObject.AllocateMore(PSB2.cb, Pointer(TempArr.lpbin), PSB2.lpb);
          CopyMemory(PSB2.lpb, PSB1.lpb, PSB2.cb);
        end;
        Inc(a);
      end;
    end;

    if a > 0 then
    begin
      SaveCount := TempArr.cValues;
      TempArr.cValues := a - 1;
      Result := abfReplaceMVbinArray(AWabObject, TempArr, AResultArray);
      TempArr.cValues := SaveCount;
    end;
  finally
    AWabObject.FreeBuffer(TempArr.lpbin);
  end;
end;

//------------------------------------------------------------------------------

function abfGetWABVersion(var Version: string): Boolean;
var
  WABDllPath: string;
  WABFileInfo: TabfFileVersionInfo;
begin
  Version := SabfWAB_UnknownVersion;
  Result := abfGetWabDllPath(WABDllPath);
  if Result then
  begin
    WABFileInfo := TabfFileVersionInfo.Create(WABDllPath);
    try
      Version := WABFileInfo.FileVersion;
    finally
      WABFileInfo.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

function abfGetWabDllPath(var WABDllPath: string): Boolean;
var
  CharCount: Integer;
  TempStr: PChar;
  Buf: array[0..MAX_PATH + 2] of Char;
begin
  Result := GetWabDllPath(WABDllPath);
  if Result and FileExists(WABDllPath) then Exit;

  CharCount := SearchPath(nil, PChar(WAB_DLL_NAME), nil, SizeOf(Buf),
    @Buf, TempStr);

  Result := (CharCount > 0) and (CharCount <= Length(Buf));
  if Result then
    WABDllPath := Buf
  else
    WABDllPath := '';
end;

//------------------------------------------------------------------------------

function abfGetWabDllPath(var WABDllPath: WideString): Boolean;
var
  S: string;
  CharCount: Integer;
  TempStr: PWideChar;
  Buf: array[0..MAX_PATH + 2] of WideChar;
begin
  S := WABDllPath;

  if not IsWinNT then
  begin
    Result := abfGetWabDllPath(S);
    WABDllPath := S;

    Exit;
  end;

  Result := GetWabDllPath(S);
  if Result and FileExists(S) then
  begin
    WABDllPath := S;
    Exit;
  end;

  CharCount := SearchPathW(nil, PWideChar(WideString(WAB_DLL_NAME)), nil, SizeOf(Buf),
    @Buf, TempStr);

  Result := (CharCount > 0) and (CharCount <= Length(Buf));
  if Result then
    WABDllPath := Buf
  else
    WABDllPath := '';
end;

//------------------------------------------------------------------------------

function abfGetLastUserIdentityGUID(var GUIDString: string): Boolean;
var
  Key: HKEY;
  Buffer: array[Word] of Char;
  BufSize: DWORD;
begin
  Result := False;
  if RegOpenKeyEx(HKEY_CURRENT_USER,
    WAB_LAST_USER_ID_PATH, 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    BufSize := Sizeof(Buffer);
    if RegQueryValueEx(Key, PChar(WAB_LAST_USER_ID_KEY), nil, nil, @Buffer,
      @BufSize) = ERROR_SUCCESS then
    begin
      GUIDString := Buffer;
      Result := True;
    end;
    RegCloseKey(Key);
  end;
end;

//------------------------------------------------------------------------------

function abfIsOutlookSharedMode: Boolean;
var
  Key: HKEY;
  Buffer, BufSize: DWORD;
begin
  Result := False;

  if RegOpenKeyEx(HKEY_CURRENT_USER,
    WAB_USEOUTLOOK_PATH, 0, KEY_READ, Key) = ERROR_SUCCESS then
  try
    BufSize := SizeOf(Buffer);
    RegQueryValueEx(Key, WAB_USEOUTLOOK_KEY, nil, nil, @Buffer, @BufSize);
    Result := (BufSize = 4) and (Buffer = 1);
  finally
    RegCloseKey(Key);
  end;
end;

//------------------------------------------------------------------------------

function abfSetOutlookSharedMode(Value: Boolean): Boolean;

  //-------------------------------------

  function _GetOUTLWABPath: string;
  var
    Key: HKEY;
    BufSize: DWORD;
    TempStr: PChar;
    Buf: array[0..MAX_PATH] of Char;
  begin
    Result := '';
    if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
      WAB_OUTLOOK_DLL_PATH, 0, KEY_READ, Key) = ERROR_SUCCESS then
    try
      BufSize := SizeOf(Buf);
      if RegQueryValueEx(Key, PChar(''), nil, nil, @Buf, @BufSize) = ERROR_SUCCESS then
        if (BufSize > 0) and (BufSize <= Sizeof(Buf)) and FileExists(PChar(@Buf)) then
          Result := Trim(PChar(@Buf));
    finally
      RegCloseKey(Key);
    end;

    if Result <> '' then Exit;

    BufSize := SearchPath(nil, PChar(WAB_OUTLOOK_DLL_NAME), nil, Sizeof(Buf),
      @Buf, TempStr);

    if (BufSize > 0) and (BufSize <= Sizeof(Buf)) then
      Result := Trim(PChar(@Buf));
  end;

  //-------------------------------------

var
  Key: HKEY;
  Buffer: DWORD;
begin
  Result := abfIsOutlookSharedMode;

  if RegOpenKeyEx(HKEY_CURRENT_USER,
    WAB_USEOUTLOOK_PATH, 0, KEY_WRITE, Key) = ERROR_SUCCESS then
  try
    Buffer := Cardinal(Value);

    if Value then
    begin
      if Value <> Result then
      begin

        Result := _GetOUTLWABPath <> '';

        if not Result then Exit;

        Result := RegSetValueEx(Key, WAB_USEOUTLOOK_KEY, 0, REG_DWORD, @Buffer,
          SizeOf(Buffer)) = ERROR_SUCCESS;
      end;
    end else
      Result := RegSetValueEx(Key, WAB_USEOUTLOOK_KEY, 0, REG_DWORD, @Buffer,
        SizeOf(Buffer)) = ERROR_SUCCESS;
  finally
    RegCloseKey(Key);
  end;
end;

//------------------------------------------------------------------------------

function abfGetRefCountWABDll: DWORD;
var
  WabDllPath: string;
  Key: HKEY;
  Buffer, BufSize: DWORD;
begin
  Result := 0;
  if not GetWABDllPath(WabDllPath) then Exit;
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
    WAB_SHARED_DLLS, 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    BufSize := Sizeof(Buffer);
    if RegQueryValueEx(Key, PChar(WabDllPath), nil, nil, @Buffer,
      @BufSize) = ERROR_SUCCESS then Result := Buffer;
    RegCloseKey(Key);
  end;
end;

//------------------------------------------------------------------------------

function abfIncRefCountWABDll: DWORD;
var
  WabDllPath: string;
  Key: HKEY;
  Buffer, BufSize: DWORD;
begin
  Result := abfGetRefCountWABDll;
  if not GetWABDllPath(WabDllPath) then Exit;
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
    WAB_SHARED_DLLS, 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    BufSize := Sizeof(Buffer);
    Buffer := Result + 1;
    if RegSetValueEx(Key, Pchar(WabDllPath), 0, REG_DWORD,
      @Buffer, BufSize) = ERROR_SUCCESS then  Result := Buffer;
    RegCloseKey(Key);
  end;
end;

//------------------------------------------------------------------------------

function abfDecRefCountWABDll: DWORD;
var
  WabDllPath: string;
  Key: HKEY;
  Buffer, BufSize: DWORD;
begin
  Result := abfGetRefCountWABDll;

  if not GetWABDllPath(WabDllPath) then Exit;
  if Result = 0 then Exit;

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
    WAB_SHARED_DLLS, 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    BufSize := Sizeof(Buffer);
    Buffer := Result - 1;
    if RegSetValueEx(Key, Pchar(WabDllPath), 0, REG_DWORD,
      @Buffer, BufSize) = ERROR_SUCCESS then  Result := Buffer;
    RegCloseKey(Key);
  end;
end;

//------------------------------------------------------------------------------

end{unit abfWABUnits}.
