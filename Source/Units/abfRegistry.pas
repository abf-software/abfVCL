{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfRegistry;

{$I abf.inc}

interface

uses
  Windows, Classes, Registry, SysUtils, System.SysConst;

const
(*
#define REG_NONE                    ( 0 )   // No value type
#define REG_SZ                      ( 1 )   // Unicode nul terminated string
#define REG_EXPAND_SZ               ( 2 )   // Unicode nul terminated string
                                            // (with environment variable references)
#define REG_BINARY                  ( 3 )   // Free form binary
#define REG_DWORD                   ( 4 )   // 32-bit number
#define REG_DWORD_LITTLE_ENDIAN     ( 4 )   // 32-bit number (same as REG_DWORD)
#define REG_DWORD_BIG_ENDIAN        ( 5 )   // 32-bit number
#define REG_LINK                    ( 6 )   // Symbolic Link (unicode)
#define REG_MULTI_SZ                ( 7 )   // Multiple Unicode strings
#define REG_RESOURCE_LIST           ( 8 )   // Resource list in the resource map
#define REG_FULL_RESOURCE_DESCRIPTOR ( 9 )  // Resource list in the hardware description
#define REG_RESOURCE_REQUIREMENTS_LIST ( 10 )
#define REG_QWORD                   ( 11 )  // 64-bit number
#define REG_QWORD_LITTLE_ENDIAN     ( 11 )  // 64-bit number (same as REG_QWORD)

#define HKEY_PERFORMANCE_TEXT       (( HKEY ) (ULONG_PTR)((LONG)0x80000050) )
#define HKEY_PERFORMANCE_NLSTEXT    (( HKEY ) (ULONG_PTR)((LONG)0x80000060) )
*)

  {$EXTERNALSYM REG_QWORD}
  REG_QWORD                = 11;         // 64-bit number
  {$EXTERNALSYM REG_QWORD_LITTLE_ENDIAN}
  REG_QWORD_LITTLE_ENDIAN  = REG_QWORD;  // 64-bit number (same as REG_QWORD)


  {$EXTERNALSYM HKEY_PERFORMANCE_TEXT}
  HKEY_PERFORMANCE_TEXT    = DWORD($80000050);
  {$EXTERNALSYM HKEY_PERFORMANCE_NLSTEXT}
  HKEY_PERFORMANCE_NLSTEXT = DWORD($80000060);


  SregNone                     = 'REG_NONE';                       // No value type
  SregSZ                       = 'REG_SZ';                         // Unicode nul terminated string
  SregExpandSZ                 = 'REG_EXPAND_SZ';                  // Unicode nul terminated string
                                                                   // (with environment variable references)
  SregBinary                   = 'REG_BINARY';                     // Free form binary
  SregDWord                    = 'REG_DWORD';                      // 32-bit number
  SregDWordLittleEndian        = 'REG_DWORD_LITTLE_ENDIAN';        // 32-bit number (same as REG_DWORD)
  SregDWordBigEndian           = 'REG_DWORD_BIG_ENDIAN';           // 32-bit number
  SregLink                     = 'REG_LINK';                       // Symbolic Link (unicode)
  SregMultiSZ                  = 'REG_MULTI_SZ';                   // Multiple Unicode strings
  SregResourceList             = 'REG_RESOURCE_LIST';              // Resource list in the resource map
  SregFullResourceDescriptor   = 'REG_FULL_RESOURCE_DESCRIPTOR';   // Resource list in the hardware description
  SregResourceRequirementsList = 'REG_RESOURCE_REQUIREMENTS_LIST';
  SregQWord                    = 'REG_QWORD';                      // 64-bit number
  SregQWordLittleEndian        = 'REG_QWORD_LITTLE_ENDIAN';        // 64-bit number (same as REG_QWORD)

type
  TabfRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary,
    rdDWordBigEndian, rdLink, rdMultiSZ, rdResourceList, rdFullResourceDescriptor,
    rdResourceRequirementsList, rdQWord);

  TabfDataTypeSet = set of TabfRegDataType;

const
  cAllValueType = [rdString..High(TabfRegDataType)];
{$IFDEF D9}
resourcestring
  SRegGetDataFailed = 'Registry Data Failed %s';
{$ENDIF}

type
  TabfRegRootKey = (rrkClassesRoot, rrkCurrentUser, rrkLocalMachine, rrkUsers,
    rrkCurrentConfig, rrkDynData);

//------------------------------------------------------------------------------
// Specifies the details about auto execution. You can have an application
// executed once at the next logon (aekMachineRunOnce, aekUserRunOnce) or at
// each logon (aekMachineRun, aekUserRun). The Machine and User specify for
// 'all users' or the 'current user only' respectively. Note that except for
// aekMachineRun and aekMachineRunOnce, all registered executables execute
// asynchronously. The aekServiceRun and aekServiceRunOnce exist to simulate NT
// services on Windows 9x and are probably of not much use. You can use the
// abfRegisterAutoExec and abfUnregisterAutoExec function to add/remove the
// entry to/from the registry.
  TabfAutoExecKind = (aekMachineRun, aekMachineRunOnce, aekUserRun,
    aekUserRunOnce, aekServiceRun, aekServiceRunOnce);

//------------------------------------------------------------------------------
// _REG_CHANGE_DATA structure used
// by abfRegFindFirstChange/abfRegFindNextChange functions

   LPREG_CHANGE_DATA = ^REG_CHANGE_DATA;
  _REG_CHANGE_DATA = record
     hKey: HKEY;
     bWatchSubtree: BOOL;
     dwNotifyFilter: DWORD;
   end;
   REG_CHANGE_DATA = _REG_CHANGE_DATA;

   
//==============================================================================
// TabfRegistry
//==============================================================================

  TabfRegDataInfo = record
    RegData: TabfRegDataType;
    DataSize: Integer;
  end;

  TabfRegistry = class(TRegistry)
  protected
    function RegDataTypeToDataType(ARegDataType: TabfRegDataType): Cardinal;
  public
    function GetDataInfoEx(const AName: string;
      var AValue: TabfRegDataInfo): Boolean;
    function GetDataTypeEx(const AName: string): TabfRegDataType;
    function GetDataEx(const AName: string; ABuffer: Pointer;
      ABufSize: DWORD; var ARegData: TabfRegDataType): DWORD;
    procedure PutDataEx(const AName: string; ABuffer: Pointer; ABufSize: DWORD;
      ARegData: TabfRegDataType);
    function ReadBoolDef(const AName: string; ADefault: Boolean): Boolean;
    function ReadBytes(const AName: string): TBytes;
    function ReadBytesDef(const AName: string; ADefault: TBytes): TBytes;
    procedure WriteBytes(const AName: string; AValue: TBytes);
    procedure WriteExpandString(const AName: string;
      const AValue: WideString); reintroduce;
    function ReadInt64(const AName: string): Int64;
    function ReadInt64Def(const AName: string; ADefault: Int64): Int64;
    procedure WriteInt64(const AName: string; AValue: Int64);
    function ReadIntegerDef(const AName: string; ADefault: Integer): Integer;
    function ReadMultiString(const AName: string): WideString;
    function ReadMultiStringDef(const AName: string;
      const ADefault: WideString): WideString;
    procedure WriteMultiString(const AName: string; const AValue: WideString);
    function ReadString(const AName: string): WideString; reintroduce;
    function ReadStringDef(const AName: string;
      const ADefault: WideString): WideString;
    procedure WriteString(const AName: string;
      const AValue: WideString); reintroduce;
    function ReadWideString(const AName: string): WideString;
    function ReadWideStringDef(const AName: string;
      const ADefault: WideString): WideString;
    procedure WriteWideString(const AName: string; const AValue: WideString);
  end;


//==============================================================================
// Registry routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns the registry type and position of value in the registry string
function abfRegExtractValueName(const RegLine: string;
  var RegType: TabfRegDataType; var Index: Integer): string;

//------------------------------------------------------------------------------
// Converts Win32 registry data type into TabfRegDataType
function abfDataTypeToRegDataType(DataType: Cardinal): TabfRegDataType;

//------------------------------------------------------------------------------
// Return the registry data type as string
function abfRegTypeToString(const RegType: TabfRegDataType): string;

//------------------------------------------------------------------------------
// Return the registry value data as string
function abfRegDataAsString(Root: HKEY; const Key, Name: string): string; overload;

//------------------------------------------------------------------------------
// Return the registry value data as string
function abfRegDataAsString(Root: HKEY; const Key, Name: string;
  DWORDAsHex: Boolean; var DataType: TabfRegDataType): string; overload;

//------------------------------------------------------------------------------
// Return the registry value data as string
function abfRegDataAsString(const AReg: TabfRegistry; Root: HKEY;
  const Key, Name: string; DWORDAsHex: Boolean;
  var DataType: TabfRegDataType): string; overload;

//------------------------------------------------------------------------------
// Return the registry value data as string
function abfRegDataAsString(const AReg: TabfRegistry; Root: HKEY;
  const Key, Name: string): string; overload;

//------------------------------------------------------------------------------
// Save registry key to list; return false if not saved.
function abfRegSaveKey(const List: TStrings; Root: HKEY; const Key: string;
  IncludeSubfolders: Boolean): Boolean;

//------------------------------------------------------------------------------
// Save registry key to list; return false if not saved.
// Extended version of abfRegSaveKey
function abfRegSaveKeyEx(const List: TStrings; Root: HKEY; const Key: string;
  IncludeSubfolders: Boolean; const ValueTypes: TabfDataTypeSet;
  const IncludeKeys, ExcludeKeys, IncludeValues, ExcludeValues: TStrings): Boolean;

//------------------------------------------------------------------------------
// Restore registry key from list
function abfRegRestoreKey(const List: TStrings): Boolean;

//------------------------------------------------------------------------------
// Creates a registry key with a default value
function abfRegCreateKeyA(ARootKey: HKEY; const AKey: AnsiString;
  const ADefaultValue: AnsiString = ''): LongInt;
function abfRegCreateKey(ARootKey: HKEY; const AKey: string;
  const ADefaultValue: string = ''): LongInt;
function abfRegCreateKeyW(ARootKey: HKEY; const AKey: WideString;
  const ADefaultValue: WideString = ''): LongInt;

//------------------------------------------------------------------------------
// Deletes a registry value
function abfRegDeleteValueA(ARootKey: HKEY; const AKey, AName: AnsiString): Boolean;
function abfRegDeleteValue(ARootKey: HKEY; const AKey, AName: string): Boolean;
function abfRegDeleteValueW(ARootKey: HKEY; const AKey, AName: WideString): Boolean;

//------------------------------------------------------------------------------
// Deletes a registry key
function abfRegDeleteKeyA(ARootKey: HKEY; const AKey: AnsiString): Boolean;
function abfRegDeleteKey(ARootKey: HKEY; const AKey: string): Boolean;
function abfRegDeleteKeyW(ARootKey: HKEY; const AKey: WideString): Boolean;

//------------------------------------------------------------------------------
// Tests if a specified key exists
function abfRegKeyExistsA(ARootKey: HKEY; const AKey: AnsiString): Boolean;
function abfRegKeyExists(ARootKey: HKEY; const AKey: string): Boolean;
function abfRegKeyExistsW(ARootKey: HKEY; const AKey: WideString): Boolean;

//------------------------------------------------------------------------------
// Tests if a specified value exists
function abfRegValueExistsA(ARootKey: HKEY; const AKey, AName: AnsiString): Boolean;
function abfRegValueExists(ARootKey: HKEY; const AKey, AName: string): Boolean;
function abfRegValueExistsW(ARootKey: HKEY; const AKey, AName: WideString): Boolean;

//------------------------------------------------------------------------------
// Reads a boolean value from registry
function abfRegReadBoolA(ARootKey: HKEY; const AKey, AName: AnsiString): Boolean;
function abfRegReadBool(ARootKey: HKEY; const AKey, AName: string): Boolean;
function abfRegReadBoolW(ARootKey: HKEY; const AKey, AName: WideString): Boolean;

//------------------------------------------------------------------------------
// Reads a boolean value from registry. If not exists, returns a default value.
function abfRegReadBoolDefA(ARootKey: HKEY; const AKey, AName: AnsiString;
  ADefaultValue: Boolean): Boolean;
function abfRegReadBoolDef(ARootKey: HKEY; const AKey, AName: string;
  ADefaultValue: Boolean): Boolean;
function abfRegReadBoolDefW(ARootKey: HKEY; const AKey, AName: WideString;
  ADefaultValue: Boolean): Boolean;

//------------------------------------------------------------------------------
// Reads a float value from registry
function abfRegReadFloatA(ARootKey: HKEY; const AKey, AName: AnsiString): Double;
function abfRegReadFloat(ARootKey: HKEY; const AKey, AName: string): Double;
function abfRegReadFloatW(ARootKey: HKEY; const AKey, AName: WideString): Double;

//------------------------------------------------------------------------------
// Reads a float value from registry. If not exists, returns a default value.
function abfRegReadFloatDefA(ARootKey: HKEY; const AKey, AName: AnsiString;
  ADefaultValue: Double): Double;
function abfRegReadFloatDef(ARootKey: HKEY; const AKey, AName: string;
  ADefaultValue: Double): Double;
function abfRegReadFloatDefW(ARootKey: HKEY; const AKey, AName: WideString;
  ADefaultValue: Double): Double;

//------------------------------------------------------------------------------
// Reads a integer value from registry
function abfRegReadIntegerA(ARootKey: HKEY; const AKey, AName: AnsiString): Integer;
function abfRegReadInteger(ARootKey: HKEY; const AKey, AName: string): Integer;
function abfRegReadIntegerW(ARootKey: HKEY; const AKey, AName: WideString): Integer;

//------------------------------------------------------------------------------
// Reads a integer value from registry. If not exists, returns a default value.
function abfRegReadIntegerDefA(ARootKey: HKEY; const AKey, AName: AnsiString;
  ADefaultValue: Integer): Integer;
function abfRegReadIntegerDef(ARootKey: HKEY; const AKey, AName: string;
  ADefaultValue: Integer): Integer;
function abfRegReadIntegerDefW(ARootKey: HKEY; const AKey, AName: WideString;
  ADefaultValue: Integer): Integer;

//------------------------------------------------------------------------------
// Reads a string value from registry
function abfRegReadStringA(ARootKey: HKEY; const AKey, AName: AnsiString): AnsiString;
function abfRegReadString(ARootKey: HKEY; const AKey, AName: string): string;
function abfRegReadStringW(ARootKey: HKEY; const AKey, AName: WideString): WideString;

//------------------------------------------------------------------------------
// Reads a string value from registry. If not exists, returns a default value.
function abfRegReadStringDefA(ARootKey: HKEY; const AKey, AName,
  ADefaultValue: AnsiString): AnsiString;
function abfRegReadStringDef(ARootKey: HKEY; const AKey, AName,
  ADefaultValue: string): string;
function abfRegReadStringDefW(ARootKey: HKEY; const AKey, AName,
  ADefaultValue: WideString): WideString;

//------------------------------------------------------------------------------
// Writes a boolean value to registry
procedure abfRegWriteBoolA(ARootKey: HKEY; const AKey, AName: AnsiString;
  AValue: Boolean);
procedure abfRegWriteBool(ARootKey: HKEY; const AKey, AName: string;
  AValue: Boolean);
procedure abfRegWriteBoolW(ARootKey: HKEY; const AKey, AName: WideString;
  AValue: Boolean);

//------------------------------------------------------------------------------
// Writes a float value to registry
procedure abfRegWriteFloatA(ARootKey: HKEY; const AKey, AName: AnsiString;
  AValue: Double);
procedure abfRegWriteFloat(ARootKey: HKEY; const AKey, AName: string;
  AValue: Double);
procedure abfRegWriteFloatW(ARootKey: HKEY; const AKey, AName: WideString;
  AValue: Double);

//------------------------------------------------------------------------------
// Writes a integer value to registry
procedure abfRegWriteIntegerA(ARootKey: HKEY; const AKey, AName: AnsiString;
  AValue: Integer);
procedure abfRegWriteInteger(ARootKey: HKEY; const AKey, AName: string;
  AValue: Integer);
procedure abfRegWriteIntegerW(ARootKey: HKEY; const AKey, AName: WideString;
  AValue: Integer);

//------------------------------------------------------------------------------
// Writes an AnsiString value to registry
procedure abfRegWriteAnsiStringA(ARootKey: HKEY; const AKey, AName: AnsiString;
  const AValue: AnsiString);
procedure abfRegWriteAnsiString(ARootKey: HKEY; const AKey, AName: string;
  const AValue: AnsiString);
procedure abfRegWriteAnsiStringW(ARootKey: HKEY; const AKey, AName: WideString;
  const AValue: AnsiString);

//------------------------------------------------------------------------------
// Writes a WideString value to registry
procedure abfRegWriteWideStringA(ARootKey: HKEY; const AKey, AName: AnsiString;
  const AValue: WideString);
procedure abfRegWriteWideString(ARootKey: HKEY; const AKey, AName: string;
  const AValue: WideString);
procedure abfRegWriteWideStringW(ARootKey: HKEY; const AKey, AName: WideString;
  const AValue: WideString);

//------------------------------------------------------------------------------
// Writes an expand string value to registry
procedure abfRegWriteExpandStringA(ARootKey: HKEY; const AKey, AName: AnsiString;
  const AValue: AnsiString);
procedure abfRegWriteExpandString(ARootKey: HKEY; const AKey, AName: string;
  const AValue: AnsiString);
procedure abfRegWriteExpandStringW(ARootKey: HKEY; const AKey, AName: WideString;
  const AValue: AnsiString);

//------------------------------------------------------------------------------
// abfRegFindFirstChangeNotification
//
//  PURPOSE:      This function begins a registry change notification
//                cycle. It will never return a handle that is already
//                signaled due to interim change notifications.
//
//                After a call to abfRegFindFirstChange, a call to
//                abfRegFindNextChangeNotification ensures that interim
//                notifications are not missed.
//
//                Since this routine allocates resources, it is
//                important that each call is matched up with a
//                corresponding abfRegFindCloseChangeNotification
//                call to free the resources.
//
//  PARAMETERS:   hKey - handle to the key to watch
//
//                bWatchSubtree - indicates whether or not to receive
//                notifications for changes in subkeys
//
//                dwNotifyFilter - identifies which changes should cause
//                notifications to occur
//
//                lprcd - pointer to a REG_CHANGE_DATA structure used
//                to cycle through the changes
//
//  RETURN VALUE: If the function succeeds, a wait able handle is
//                returned. If it fails, INVALID_HANDLE_VALUE is returned
//                and a call to GetLastError identifies why it failed.
function abfRegFindFirstChangeNotification(hKey: HKEY; bWatchSubtree: Boolean;
  dwNotifyFilter: DWORD; lprcd: LPREG_CHANGE_DATA): THandle;

//------------------------------------------------------------------------------
// abfRegFindNextChangeNotification
//
//  PURPOSE:      This function queues the next registry change
//                notification in a cycle. It must be preceded by a
//                call to abfRegFindFirstChangeNotification.
//
//                After calling this function, the handle returned by
//                abfRegFindFirstChangeNotification can be waited on again
//                to get the next change notification. Using these functions
//                in this manner, all interim change notifications will
//                be caught.
//
//  PARAMETERS:   lprcd - pointer to the same REG_CHANGE_DATA structure
//                that was initialized with abfRegFindFirstChangeNotification
//
//  RETURN VALUE: If the function succeeds, it returns TRUE. If it
//                fails, it returns FALSE and a call to GetLastError
//                identifies why it failed.
function abfRegFindNextChangeNotification(hChange: THandle;
  lprcd: LPREG_CHANGE_DATA): Boolean;

//------------------------------------------------------------------------------
//  abfRegFindCloseChangeNotification
//
//  PURPOSE:      This procedure frees the resources allocated by a call
//                to abfRegFindFirstChangeNotification.
//
//  PARAMETERS:   hChange - the waitable handle returned from
//                abfRegFindFirstChangeNotification
procedure abfRegFindCloseChangeNotification(hChange: THandle);

//------------------------------------------------------------------------------
// Register the file specified by Path in the system registry so that it will
// be automatically executed by the system at the next logon.
function abfRegisterAutoExec(AExecKind: TabfAutoExecKind;
  const ACmdLine: WideString): Boolean;

//------------------------------------------------------------------------------
// Unregister the file specified by Path in the system registry so that it will
// not be automatically executed any more.
function abfUnregisterAutoExec(AExecKind: TabfAutoExecKind;
  const ACmdLine: WideString): Boolean;


//******************************************************************************
implementation
//******************************************************************************

uses
  {$IfDef D6} RTLConsts, {$Else} Consts, {$EndIf D6}
// ABF VCL
  abfVCLConsts, abfSysUtils, abfStrUtils;

//==============================================================================
// Private types
//==============================================================================

type
  THackRegistry = class(TRegistry);

//==============================================================================
// Private consts
//==============================================================================

const
  cDefaultValue = '<<default>>';

//==============================================================================
// Private variables
//==============================================================================

var
  _Registry            : TabfRegistry = nil;
  _RegistryValueName   : TStringList  = nil;
  _RegSaveDataTypes    : TabfDataTypeSet;
  _RegSaveIcludeValues : TStrings     = nil;
  _RegSaveExcludeValues: TStrings     = nil;
  _RegSaveIcludeKeys   : TStrings     = nil;
  _RegSaveExcludeKeys  : TStrings     = nil;
  _RegSaveList         : TStrings     = nil;


//==============================================================================
// Private routines
//==============================================================================

//------------------------------------------------------------------------------
// Raise an exception on reading a value

procedure _ReadError(const Name: string);
begin
  raise ERegistryException.CreateResFmt(@SInvalidRegType, [Name]);
end;

//------------------------------------------------------------------------------
// Save data from registry
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Read from registry and return string as name=value

function _RegSaveValueLine(const Name: string): string;
var
  Buf: Pointer;
  BufSize: Integer;
  DataType: TabfRegDataType;
begin
  if Name <> '' then Result := Name else Result := cDefaultValue;
  Result := Result + '=';

// Read wrong value name
  if not _Registry.ValueExists(Name) then
  begin
    Result := '';
    Exit;
  end;

// Get data size
  BufSize := _Registry.GetDataSize(Name) + 10;
  GetMem(Buf, BufSize);
// Read data to buffer
  try
    FillChar(Buf^, BufSize, 0);
    BufSize := _Registry.GetDataEx(Name, Buf, BufSize, DataType);
    Result := Result + IntToHex(Integer(DataType), 1) + ';';

    if DataType = rdInteger then
      Result := Result + IntToStr(Integer(Buf^))
    else
      Result := Result + abfBinaryToString(Buf^, BufSize);
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Open key and getting list of value name; for each value
// name calling _RegSaveValueLine

function _RegSaveKey(const Key: string): Boolean;
var
  i: Integer;
begin
  _Registry.CloseKey;
  Result := _Registry.OpenKeyReadOnly(Key);
  if not Result then Exit;

  _RegSaveList.Add('[' + IntToStr(Int64(_Registry.RootKey)) + ';' + Key + ']');

//  _RegSaveList.Add(_RegSaveValueLine(''));
  _Registry.GetValueNames(_RegistryValueName);
  for i := 0 to _RegistryValueName.Count - 1 do
    _RegSaveList.Add(_RegSaveValueLine(_RegistryValueName[i]));
end;

//------------------------------------------------------------------------------
// Extended version of _RegSaveKey. Before calling _RegSaveValueLine
// value name compare with Mask, Include and Exclude list

function _RegSaveKeyEx(const Key: string): Boolean;
var
  i: Integer;
  S: string;
begin
  _Registry.CloseKey;
  Result := _Registry.OpenKeyReadOnly(Key);
  if not Result then Exit;

//  _RegSaveList.Add(_RegSaveValueLine(''));
  _Registry.GetValueNames(_RegistryValueName);

// Remove values are not matched ValueTypes
  for i := _RegistryValueName.Count - 1 downto 0 do
    if not (_Registry.GetDataTypeEx(_RegistryValueName[i]) in _RegSaveDataTypes) then
      _RegistryValueName.Delete(i);

// Remove values are not present in IncludeValues
  if Assigned(_RegSaveIcludeValues) then
    for i := _RegistryValueName.Count - 1 downto 0 do
      if _RegSaveIcludeValues.IndexOf(_RegistryValueName[i]) < 0 then
        _RegistryValueName.Delete(i);

// Remove values are present in ExcludeValues
  if Assigned(_RegSaveExcludeValues) then
    for i := _RegistryValueName.Count - 1 downto 0 do
      if _RegSaveExcludeValues.IndexOf(_RegistryValueName[i]) > -1 then
        _RegistryValueName.Delete(i);

  if _RegistryValueName.Count < 1 then Exit;

  _RegSaveList.Add('[' + IntToStr(Int64(_Registry.RootKey)) + ';' + Key + ']');

  for i := 0 to _RegistryValueName.Count - 1 do
  begin
    S := _RegSaveValueLine(_RegistryValueName[i]);
    if S = '' then Continue;
    _RegSaveList.Add(S);
  end;
end;

//------------------------------------------------------------------------------

function _RegSaveEnumKeys(Root: HKEY; const Key: string;
  IncludeSubfolders: Boolean): Boolean;
var
  i: Integer;
  S: string;
  Subfolders: TStringList;
begin
  _Registry.RootKey := Root;
  Result := _RegSaveKey(Key);

  if not Result then Exit;
  if not IncludeSubfolders then Exit;

  Subfolders := TStringList.Create;
  try
    _Registry.GetKeyNames(Subfolders);
    if Key <> '' then S := abfAddSlash(Key) else S := '';
    
    for i := 0 to Subfolders.Count - 1 do
    begin
      if Subfolders[i] = '' then Continue;
      Result := _RegSaveEnumKeys(Root, S + Subfolders[i], IncludeSubfolders);
//      if not Result then Exit;
    end;
  finally
    Subfolders.Free;
  end;
end;

//------------------------------------------------------------------------------

function _RegSaveEnumKeysEx(Root: HKEY; const Key: string;
  IncludeSubfolders: Boolean): Boolean;
var
  i: Integer;
  S: string;
  Subfolders: TStringList;
begin
  _Registry.RootKey := Root;

  Result := _RegSaveKeyEx(Key);

  if not Result then Exit;
  if not IncludeSubfolders then Exit;

  Subfolders := TStringList.Create;
  try
    _Registry.GetKeyNames(Subfolders);
    //save include subkey
    if Assigned(_RegSaveIcludeKeys) then
      for i := Subfolders.Count - 1 downto 0 do
        if _RegSaveIcludeKeys.IndexOf(Subfolders[i]) < 0 then
          Subfolders.Delete(i);

    //don't save exclude sub key
    if Assigned(_RegSaveExcludeKeys) then
      for i := Subfolders.Count - 1 downto 0 do
        if _RegSaveExcludeKeys.IndexOf(Subfolders[i]) > -1 then
          Subfolders.Delete(i);

    if Key <> '' then S := abfAddSlash(Key) else S := '';

    for i := 0 to Subfolders.Count - 1 do
    begin
      if Subfolders[i] = '' then Continue;
      Result := _RegSaveEnumKeysEx(Root, S + Subfolders[i], IncludeSubfolders);
      //if not Result then Exit;
    end;
  finally
    Subfolders.Free;
  end;
end;


//------------------------------------------------------------------------------
// Load data into registry
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Save a value into regsitry. Used for compatibility only.

procedure _RegRestoreOldData(const Name, EncodeValue: string);
var
  S: string;
  p: Pointer;
  PSize, DataType, Len: Integer;

  //-------------------------------------

  function _GetDataType(AType: Integer): TRegDataType;
  begin
    Result := Registry.rdInteger;
    case AType of
      1: Result := Registry.rdString;
      2: Result := Registry.rdExpandString;
      3: Result := Registry.rdBinary;
    end;
  end;

  //-------------------------------------

begin
  S := EncodeValue;
// Detect value type
  DataType := StrToIntDef(Copy(S, 1, 1), 3);
  if (DataType < 0) or (DataType > 3) then DataType := 3;

  Delete(S, 1, 2);

  if DataType = 0 then
  begin
    _Registry.WriteInteger(Name, StrToInt(S));
    Exit;
  end;

  Len := Length(S);
  GetMem(p, Len);
  try
    PSize := abfStringToBinary(S, p^, Len);
    if PSize > 0 then _Registry.PutData(Name, p, PSize, _GetDataType(DataType));
  finally
    FreeMem(P);
  end;
end;

//------------------------------------------------------------------------------
// Save a value into regsitry

procedure _RegRestoreNewData(const Name, EncodedValue: string);
var
  S: string;
  P: Pointer;
  PSize, Len, i, k: Integer;
  DataType: TabfRegDataType;
begin
  S := EncodedValue;
  i := Pos(';', S);
  if i < 1 then Exit;

// Detect value type
  k := StrToIntDef('$' + Copy(S, 1, i - 1), Integer(rdBinary));
  if (k < Integer(Low(TabfRegDataType))) or (k > Integer(High(TabfRegDataType))) then
    k := Integer(rdBinary);
  DataType := TabfRegDataType(k);

  Delete(S, 1, i);

  if DataType = rdInteger then
  begin
    try
      _Registry.WriteInteger(Name, StrToIntDef(S, 0));
    except
    end;
    Exit;
  end;

  Len := Length(S);
  GetMem(P, Len);
  try
    PSize := abfStringToBinary(S, P^, Len);
    if PSize > 0 then
    try
      _Registry.PutDataEx(Name, P, PSize, DataType);
    except
    end;
  finally
    FreeMem(P);
  end;
end;


//==============================================================================
// TabfRegistry
//==============================================================================

function TabfRegistry.RegDataTypeToDataType(
  ARegDataType: TabfRegDataType): Cardinal;
begin
  case ARegDataType of
    rdInteger: Result := REG_DWORD;
    rdBinary : Result := REG_BINARY
  else
    Result := Integer(ARegDataType);
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.GetDataInfoEx(const AName: string;
  var AValue: TabfRegDataInfo): Boolean;
var
  DataType: Integer;
begin
  FillChar(AValue, SizeOf(TabfRegDataInfo), 0);

  if IsWinNT then
  begin
    Result := RegQueryValueExW(CurrentKey, PWideChar(WideString(AName)), nil,
      @DataType, nil, @AValue.DataSize) = ERROR_SUCCESS;
  end else
  begin
    Result := RegQueryValueExA(CurrentKey, PAnsiChar(AName), nil, @DataType,
      nil, @AValue.DataSize) = ERROR_SUCCESS;
  end;

  if Result then
    AValue.RegData := abfDataTypeToRegDataType(DataType);
end;

//------------------------------------------------------------------------------

function TabfRegistry.GetDataTypeEx(const AName: string): TabfRegDataType;
var
  Info: TabfRegDataInfo;
begin
  if GetDataInfoEx(AName, Info) then
    Result := Info.RegData
  else
    Result := rdUnknown;
end;

//------------------------------------------------------------------------------

function TabfRegistry.GetDataEx(const AName: string; ABuffer: Pointer;
  ABufSize: DWORD; var ARegData: TabfRegDataType): DWORD;
var
  DataType: Cardinal;
begin
  if IsWinNT then
  begin
    if RegQueryValueExW(CurrentKey, PWideChar(WideString(AName)), nil,
      @DataType, PByte(ABuffer), @ABufSize) <> ERROR_SUCCESS then
      raise ERegistryException.CreateResFmt(@SRegGetDataFailed, [AName]);
  end else
  begin
    if RegQueryValueExA(CurrentKey, PAnsiChar(AName), nil, @DataType,
      PByte(ABuffer), @ABufSize) <> ERROR_SUCCESS then
      raise ERegistryException.CreateResFmt(@SRegGetDataFailed, [AName]);
  end;

  ARegData := abfDataTypeToRegDataType(DataType);
  Result := ABufSize;
end;

//------------------------------------------------------------------------------

procedure TabfRegistry.PutDataEx(const AName: string; ABuffer: Pointer;
  ABufSize: DWORD; ARegData: TabfRegDataType);
var
  DataType: Cardinal;
begin
  DataType := RegDataTypeToDataType(ARegData);

  if IsWinNT then
  begin
    if RegSetValueExW(CurrentKey, PWideChar(WideString(AName)), 0, DataType ,
      ABuffer, ABufSize) <> ERROR_SUCCESS
    then
      raise ERegistryException.CreateResFmt(@SRegSetDataFailed, [AName]);
  end else
  begin
    if RegSetValueExA(CurrentKey, PAnsiChar(AName), 0, DataType , ABuffer,
      ABufSize) <> ERROR_SUCCESS
    then
      raise ERegistryException.CreateResFmt(@SRegSetDataFailed, [AName]);
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadBoolDef(const AName: string;
  ADefault: Boolean): Boolean;
begin
  Result := ADefault;
  if not ValueExists(AName) then Exit;
  try
    Result := ReadBool(AName);
  except
    Result := ADefault;  
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadBytes(const AName: string): TBytes;
var
  TempInfo: TabfRegDataInfo;
begin
  Result := nil;

  if GetDataInfoEx(AName, TempInfo) then
  begin
    if (TempInfo.RegData <> abfRegistry.rdBinary) and
      (TempInfo.RegData <> abfRegistry.rdUnknown) then
      _ReadError(AName);

    SetLength(Result, TempInfo.DataSize);
    GetDataEx(AName, @Result[0], TempInfo.DataSize, TempInfo.RegData);
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadBytesDef(const AName: string;
  ADefault: TBytes): TBytes;
begin
  Result := Copy(ADefault);
  if not ValueExists(AName) then Exit;
  try
    Result := ReadBytes(AName);
  except
    Result := Copy(ADefault);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRegistry.WriteBytes(const AName: string; AValue: TBytes);
begin
  PutDataEx(AName, @AValue[0], Length(AValue), rdBinary);
end;

//------------------------------------------------------------------------------

procedure TabfRegistry.WriteExpandString(const AName: string;
  const AValue: WideString);
begin
  if IsWinNT then
  begin
    PutDataEx(AName, PWideChar(AValue), (Length(AValue) + 1) * 2,
      rdExpandString);
  end else
    inherited WriteExpandString(AName, AValue);
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadInt64(const AName: string): Int64;
var
  RegData: TabfRegDataType;
begin
  GetDataEx(AName, @Result, SizeOf(Int64), RegData);
  if RegData <> rdQWord then _ReadError(AName);
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadInt64Def(const AName: string; ADefault: Int64): Int64;
var
  RegData: TabfRegDataType;
begin
  Result := ADefault;
  if not ValueExists(AName) then Exit;
  try
    GetDataEx(AName, @Result, SizeOf(Int64), RegData);
    if RegData <> rdQWord then _ReadError(AName);
  except
    Result := ADefault;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRegistry.WriteInt64(const AName: string; AValue: Int64);
begin
  PutDataEx(AName, @AValue, SizeOf(Int64), rdQWord);
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadIntegerDef(const AName: string;
  ADefault: Integer): Integer;
begin
  Result := ADefault;
  if not ValueExists(AName) then Exit;
  try
    Result := ReadInteger(AName);
  except
    Result := ADefault;
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadMultiString(const AName: string): WideString;
var
  TempInfo: TabfRegDataInfo;
  TempData: TBytes;
begin
  Result := '';

  if GetDataInfoEx(AName, TempInfo) then
  begin
    if TempInfo.RegData <> abfRegistry.rdMultiSZ then
      _ReadError(AName);

    SetLength(TempData, TempInfo.DataSize);
    GetDataEx(AName, @TempData[0], TempInfo.DataSize, TempInfo.RegData);

    if IsWinNT then
      Result := abfBytesToWStr(TempData)
    else
      Result := abfBytesToStr(TempData);
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadMultiStringDef(const AName: string;
  const ADefault: WideString): WideString;
begin
  Result := ADefault;
  if not ValueExists(AName) then Exit;
  try
    Result := ReadMultiString(AName);
  except
    Result := ADefault;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRegistry.WriteMultiString(const AName: string;
  const AValue: WideString);
var
  TempS: AnsiString;
begin
  if IsWinNT then
  begin
    PutDataEx(AName, @AValue[1], Length(AValue) * 2, rdMultiSZ);
  end else
  begin
    TempS := AValue;
    PutDataEx(AName, @TempS[1], Length(TempS), rdMultiSZ);
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadString(const AName: string): WideString;
var
  TempInfo: TabfRegDataInfo;
  TempData: TBytes;
begin
  Result := '';

  if GetDataInfoEx(AName, TempInfo) then
  begin
    if (TempInfo.RegData <> abfRegistry.rdString) and
      (TempInfo.RegData <> abfRegistry.rdExpandString) then
      _ReadError(AName);

    SetLength(TempData, TempInfo.DataSize);
    GetDataEx(AName, @TempData[0], TempInfo.DataSize, TempInfo.RegData);

    if IsWinNT then
      Result := abfBytesToWStr(TempData)
    else
      Result := abfBytesToStr(TempData);
  end;
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadStringDef(const AName: string;
  const ADefault: WideString): WideString;
begin
  Result := ADefault;
  if not ValueExists(AName) then Exit;
  try
    Result := ReadString(AName);
  except
    Result := ADefault;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRegistry.WriteString(const AName: string;
  const AValue: WideString);
begin
  if IsWinNT then
  begin
    PutDataEx(AName, PWideChar(AValue), (Length(AValue) + 1) * 2, rdString);
  end else
    inherited WriteString(AName, AValue);
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadWideString(const AName: string): WideString;
var
  Data: TBytes;
  Size: Integer;
begin
  Size := GetDataSize(AName);
  if Size > 0 then
  begin
    SetLength(Data, Size);
    if ReadBinaryData(AName, Data[0], Size) <> Size then _ReadError(AName);
    Result := abfBytesToWStr(Data);
  end else
    Result := '';
end;

//------------------------------------------------------------------------------

function TabfRegistry.ReadWideStringDef(const AName: string;
  const ADefault: WideString): WideString;
var
  Data: TBytes;
  Size: Integer;
begin
  Result := ADefault;
  if not ValueExists(AName) then Exit;
  try
    Size := GetDataSize(AName);
    if Size > 0 then
    begin
      SetLength(Data, Size);
      if ReadBinaryData(AName, Data[0], Size) <> Size then _ReadError(AName);
      Result := abfBytesToWStr(Data);
    end else
      Result := ADefault;
  except
    Result := ADefault;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRegistry.WriteWideString(const AName: string;
  const AValue: WideString);
var
  Data: TBytes;
begin
  try
    Data := abfWStrToBytes(AValue, True);
    WriteBinaryData(AName, Data[0], Length(Data));
  except
  end;
end;


//==============================================================================
// Registry routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns the registry type and position of value in the registry string

function abfRegExtractValueName(const RegLine: string;
  var RegType: TabfRegDataType; var Index: Integer): string;
const
  cNum = ['0'..'9', 'a'..'f'];
  cDataTypeDelim = [';', ':'];
var
  Len: Integer;
  RegTypeChar: Char;
begin
  Result := '';
  Index := 0;

  Len := Length(RegLine);
  if Len < 5 then Exit;

  Index := Pos('=', RegLine);
  if (Index < 1) or (Index > Len - 3) then Exit; // wrong line

// fix for present in values name char '='
  RegTypeChar := RegLine[Index + 1];
  while (not (RegTypeChar in cNum)) and
    (not (RegLine[Index + 2] in cDataTypeDelim)) do
  begin
    Index := abfPosEx('=', RegLine, Index);
    if Index < 1 then Break;
    RegTypeChar := RegLine[Index + 1];
  end; {while }

  if Index < 1 then Exit; // wrong line

  Result := Copy(RegLine, 1, Index - 1);

// determinate registry data type
  Len := StrToInt('$' + RegTypeChar);
  if Len > Integer(High(TabfRegDataType)) then RegType := rdUnknown
  else RegType := TabfRegDataType(Len);
end;

//------------------------------------------------------------------------------
// Converts Win32 registry data type into TabfRegDataType

function abfDataTypeToRegDataType(DataType: Cardinal): TabfRegDataType;
begin
  Result := rdUnknown;
  case DataType of
    REG_DWORD : Result := rdInteger;
    REG_BINARY: Result := rdBinary
  else
    if DataType <= Cardinal(High(TabfRegDataType)) then
      Result := TabfRegDataType(DataType);
  end;
end;

//------------------------------------------------------------------------------
// Returns name of the registry type as string

function abfRegTypeToString(const RegType: TabfRegDataType): string;
begin
  Result := SregNone;
  case RegType of
    rdString                  : Result := SregSZ;
    rdExpandString            : Result := SregExpandSZ;
    rdInteger                 : Result := SregDword;
    rdBinary                  : Result := SregBinary;
    rdDWordBigEndian          : Result := SregDwordBigEndian;
    rdLink                    : Result := SregLink;
    rdMultiSZ                 : Result := SregMultiSZ;
    rdResourceList            : Result := SregResourceList;
    rdFullResourceDescriptor  : Result := SregFullResourceDescriptor;
    rdResourceRequirementsList: Result := SregResourceRequirementsList;
    rdQWord                   : Result := SregQWord;
  end;
end;

//------------------------------------------------------------------------------
// Returns the registry value data as string

function abfRegDataAsString(Root: HKEY; const Key, Name: string): string; overload;
var
  DataType: TabfRegDataType;
begin
  Result := abfRegDataAsString(Root, Key, Name, True, DataType);
end;

//------------------------------------------------------------------------------
// Returns the registry value data as string

function abfRegDataAsString(Root: HKEY; const Key, Name: string;
  DWORDAsHex: Boolean; var DataType: TabfRegDataType): string;
var
  Reg: TabfRegistry;
begin
  Reg := TabfRegistry.Create;
  try
    Result := abfRegDataAsString(Reg, Root, Key, Name, DWORDAsHex, DataType);
  finally
    Reg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Returns the registry value data as string

function abfRegDataAsString(const AReg: TabfRegistry; Root: HKEY;
  const Key, Name: string): string;
var
  DataType: TabfRegDataType;
begin
  Result := '';
  if not Assigned(AReg) then Exit;

  Result := abfRegDataAsString(AReg, Root, Key, Name, True, DataType);
end;

//------------------------------------------------------------------------------
// Returns registry key value as string

function abfRegDataAsString(const AReg: TabfRegistry; Root: HKEY;
  const Key, Name: string; DWORDAsHex: Boolean;
  var DataType: TabfRegDataType): string;
var
  P: Pointer;
  CurRootKey: HKEY;
  CurKey, S: string;
  Size: Integer;
begin
  Result := '';
  if not Assigned(AReg) then Exit;

  CurKey := AReg.CurrentPath;
  CurRootKey := AReg.RootKey;
  AReg.CloseKey;
  AReg.RootKey := Root;
  if not AReg.OpenKeyReadOnly(Key) then Exit;

  Size := AReg.GetDataSize(Name);
  GetMem(P, Size);
  try
    AReg.GetDataEx(Name, P, Size, DataType);
    case DataType of
      rdString, rdExpandString: Result := PChar(P);
      rdMultiSZ:
        begin
          SetLength(S, Size);
          Move(P^, S[1], Size - 2);
          abfReplaceChars(S, #0, #32);
          Result := S;
        end;
      rdInteger, rdDWordBigEndian:
        begin
          if DWORDAsHex then Result := IntToHex(DWORD(P^), 8)
          else Result := IntToStr(DWORD(P^));
        end
      else Result := abfBinaryToString(P^, Size);
    end;
  finally
    FreeMem(P);
  end;

  AReg.CloseKey;
  if CurKey = '' then Exit;
  AReg.RootKey := CurRootKey;
  if AReg.OpenKey(CurKey, False) then Exit;
  AReg.OpenKeyReadOnly(CurKey);
end;

//------------------------------------------------------------------------------
// Save registry key to list; return false if not saved.

function abfRegSaveKey(const List: TStrings; Root: HKEY; const Key: string;
  IncludeSubfolders: Boolean): Boolean;
begin
  Result := False;
  if not Assigned(List) then Exit;

  _RegSaveList := List;
  Result := _RegSaveEnumKeys(Root, Key, IncludeSubfolders);
end;

//------------------------------------------------------------------------------
// Save registry key to list; return false if not saved.
// Extended version of abfRegSaveKey

function abfRegSaveKeyEx(const List: TStrings; Root: HKEY; const Key: string;
  IncludeSubfolders: Boolean; const ValueTypes: TabfDataTypeSet;
  const IncludeKeys, ExcludeKeys, IncludeValues, ExcludeValues: TStrings): Boolean;
begin
  Result := False;
  if not Assigned(List) then Exit;
  _RegSaveList := List;
  _RegSaveIcludeKeys    := IncludeKeys;
  _RegSaveExcludeKeys   := ExcludeKeys;
  _RegSaveIcludeValues  := IncludeValues;
  _RegSaveExcludeValues := ExcludeValues;
  _RegSaveDataTypes     := ValueTypes;

  Result := _RegSaveEnumKeysEx(Root, Key, IncludeSubfolders);
end;

//------------------------------------------------------------------------------
// Restore registry key from list

function abfRegRestoreKey(const List: TStrings): Boolean;
var
  i, j, Len: Integer;
  S, Key, Name, Value: string;
  SavedAccess: LongWord;
  RegType: TabfRegDataType;
  Root: HKEY;
begin
  SavedAccess := _Registry.Access;
  _Registry.Access := KEY_ALL_ACCESS;
  try
    Result := True;

    for i := 0 to List.Count - 1 do
    begin
      S := Trim(List[i]);
      if S = '' then Continue;

      Len := Length(S);

    // Detect registry key, Root and open this key
      if (S[1] = '[') and (S[Len] = ']') then
      begin
        j := Pos(';', S);
        if j > 2 then
        begin
        // Root is found
          Root := StrToInt64Def(Copy(S, 2, j - 2), 0);
          if Root = 0 then //wrong root
          begin
             j := 1;
             Root := HKEY_CURRENT_USER;
          end;
        end else
        begin
        // Root is not found
          Root := HKEY_CURRENT_USER;
          j := 1;
        end;
        Key := Copy(S, j + 1, Len - j - 1);
        _Registry.CloseKey;
        _Registry.RootKey := Root;
        Result := _Registry.OpenKey(Key, True);
        Continue; // Key is selected, continue reading values
      end;{if (S[1] = '[')...}

      if not Result then Continue;

      Name := abfRegExtractValueName(S, RegType, j);
      if j < 1 then Continue; // wrong line
      Value := Copy(S, j + 1, MaxInt);
      if Name = cDefaultValue then Name := '';

    // Fix for old format
      if Value[2] = ':' then _RegRestoreOldData(Name, Value)
      else _RegRestoreNewData(Name, Value);
    end;

    Result := True;
  finally
    _Registry.Access := SavedAccess;
  end;
end;

//------------------------------------------------------------------------------
// Creates registry key with a default value

function abfRegCreateKeyA(ARootKey: HKEY; const AKey: AnsiString;
  const ADefaultValue: AnsiString = ''): LongInt;
begin
  Result := RegSetValueA(ARootKey, PAnsiChar(AKey), REG_SZ,
    PAnsiChar(ADefaultValue), Length(ADefaultValue) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

function abfRegCreateKey(ARootKey: HKEY; const AKey: string;
  const ADefaultValue: string = ''): LongInt;
begin
  Result := abfRegCreateKeyA(ARootKey, AKey, ADefaultValue);
end;

//------------------------------------------------------------------------------

function abfRegCreateKeyW(ARootKey: HKEY; const AKey: WideString;
  const ADefaultValue: WideString = ''): LongInt;
begin
  if not IsWinNT then
  begin
    Result := abfRegCreateKeyA(ARootKey, AKey, ADefaultValue);
    Exit;
  end;

  Result := RegSetValueW(ARootKey, PWideChar(AKey), REG_SZ,
    PWideChar(ADefaultValue), Length(ADefaultValue) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

function abfRegDeleteValueA(ARootKey: HKEY; const AKey, AName: AnsiString): Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) then Result := WinReg.DeleteValue(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegDeleteValue(ARootKey: HKEY; const AKey, AName: string): Boolean;
begin
  Result := abfRegDeleteValueA(ARootKey, AKey, AName);
end;

//------------------------------------------------------------------------------

function abfRegDeleteValueW(ARootKey: HKEY; const AKey, AName: WideString): Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) then Result := WinReg.DeleteValue(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Deletes a registry key

function abfRegDeleteKeyA(ARootKey: HKEY; const AKey: AnsiString): Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.keyExists(AKey) then
    begin
      WinReg.CloseKey;
      Result := WinReg.DeleteKey(AKey);
    end;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegDeleteKey(ARootKey: HKEY; const AKey: string): Boolean;
begin
  Result := abfRegDeleteKeyA(ARootKey, AKey);
end;

//------------------------------------------------------------------------------

function abfRegDeleteKeyW(ARootKey: HKEY; const AKey: WideString): Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.keyExists(AKey) then
    begin
      WinReg.CloseKey;
      Result := WinReg.DeleteKey(AKey);
    end;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Tests if a specified key exists

function abfRegKeyExistsA(ARootKey: HKEY; const AKey: AnsiString): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    Result := WinReg.KeyExists(AKey);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegKeyExists(ARootKey: HKEY; const AKey: string): Boolean;
begin
  Result := abfRegKeyExistsA(ARootKey, AKey);
end;

//------------------------------------------------------------------------------

function abfRegKeyExistsW(ARootKey: HKEY; const AKey: WideString): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    Result := WinReg.KeyExists(AKey);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Tests if a specified value exists

function abfRegValueExistsA(ARootKey: HKEY; const AKey, AName: AnsiString): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    Result := WinReg.OpenKeyReadOnly(AKey);

    if Result then
      Result := WinReg.ValueExists(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegValueExists(ARootKey: HKEY; const AKey, AName: string): Boolean;
begin
  Result := abfRegValueExistsA(ARootKey, AKey, AName);
end;

//------------------------------------------------------------------------------

function abfRegValueExistsW(ARootKey: HKEY; const AKey, AName: WideString): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    Result := WinReg.OpenKeyReadOnly(AKey);

    if Result then
      Result := WinReg.ValueExists(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a boolean value from registry

function abfRegReadBoolA(ARootKey: HKEY; const AKey, AName: AnsiString): Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    Result := WinReg.ReadBool(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadBool(ARootKey: HKEY; const AKey, AName: string): Boolean;
begin
  Result := abfRegReadBoolA(ARootKey, AKey, AName);
end;

//------------------------------------------------------------------------------

function abfRegReadBoolW(ARootKey: HKEY; const AKey, AName: WideString): Boolean;
var
  WinReg: TRegistry;
begin
  Result := False;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    Result := WinReg.ReadBool(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a boolean value from registry. If not exists, returns a default value.

function abfRegReadBoolDefA(ARootKey: HKEY; const AKey, AName: AnsiString;
  ADefaultValue: Boolean): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) and WinReg.ValueExists(AName) then
      Result := WinReg.ReadBool(AName)
    else
      Result := ADefaultValue;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadBoolDef(ARootKey: HKEY; const AKey, AName: string;
  ADefaultValue: Boolean): Boolean;
begin
  Result := abfRegReadBoolDefA(ARootKey, AKey, AName, ADefaultValue);
end;

//------------------------------------------------------------------------------

function abfRegReadBoolDefW(ARootKey: HKEY; const AKey, AName: WideString;
  ADefaultValue: Boolean): Boolean;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) and WinReg.ValueExists(AName) then
      Result := WinReg.ReadBool(AName)
    else
      Result := ADefaultValue;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a float value from registry

function abfRegReadFloatA(ARootKey: HKEY; const AKey, AName: AnsiString): Double;
var
  WinReg: TRegistry;
begin
  Result := 0;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    Result := WinReg.ReadFloat(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadFloat(ARootKey: HKEY; const AKey, AName: string): Double;
begin
  Result := abfRegReadFloatA(ARootKey, AKey, AName);
end;

//------------------------------------------------------------------------------

function abfRegReadFloatW(ARootKey: HKEY; const AKey, AName: WideString): Double;
var
  WinReg: TRegistry;
begin
  Result := 0;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    Result := WinReg.ReadFloat(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a float value from registry. If not exists, returns a default value.

function abfRegReadFloatDefA(ARootKey: HKEY; const AKey, AName: AnsiString;
  ADefaultValue: Double): Double;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) and WinReg.ValueExists(AName) then
      Result := WinReg.ReadFloat(AName)
    else
      Result := ADefaultValue;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadFloatDef(ARootKey: HKEY; const AKey, AName: string;
  ADefaultValue: Double): Double;
begin
  Result := abfRegReadFloatDefA(ARootKey, AKey, AName, ADefaultValue);
end;  

//------------------------------------------------------------------------------

function abfRegReadFloatDefW(ARootKey: HKEY; const AKey, AName: WideString;
  ADefaultValue: Double): Double;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) and WinReg.ValueExists(AName) then
      Result := WinReg.ReadFloat(AName)
    else
      Result := ADefaultValue;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a integer value from registry

function abfRegReadIntegerA(ARootKey: HKEY; const AKey, AName: AnsiString): Integer;
var
  WinReg: TRegistry;
begin
  Result := 0;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    Result := WinReg.ReadInteger(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadInteger(ARootKey: HKEY; const AKey, AName: string): Integer;
begin
  Result := abfRegReadIntegerA(ARootKey, AKey, AName);
end;

//------------------------------------------------------------------------------

function abfRegReadIntegerW(ARootKey: HKEY; const AKey, AName: WideString): Integer;
var
  WinReg: TRegistry;
begin
  Result := 0;
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    Result := WinReg.ReadInteger(AName);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a integer value from registry. If not exists, returns a default value.

function abfRegReadIntegerDefA(ARootKey: HKEY; const AKey, AName: AnsiString;
  ADefaultValue: Integer): Integer;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) and WinReg.ValueExists(AName) then
      Result := WinReg.ReadInteger(AName)
    else
      Result := ADefaultValue;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadIntegerDef(ARootKey: HKEY; const AKey, AName: string;
  ADefaultValue: Integer): Integer;
begin
  Result := abfRegReadIntegerDefA(ARootKey, AKey, AName, ADefaultValue);
end;

//------------------------------------------------------------------------------

function abfRegReadIntegerDefW(ARootKey: HKEY; const AKey, AName: WideString;
  ADefaultValue: Integer): Integer;
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if WinReg.OpenKey(AKey, False) and WinReg.ValueExists(AName) then
      Result := WinReg.ReadInteger(AName)
    else
      Result := ADefaultValue;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a string value from registry

function abfRegReadStringA(ARootKey: HKEY; const AKey, AName: AnsiString): AnsiString;
var
  WinReg: TabfRegistry;
  RDT: TabfRegDataType;
begin
  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);

    RDT := WinReg.GetDataTypeEx(AName);
    if RDT in [rdString, rdExpandString] then
      Result := WinReg.ReadString(AName)
    else if RDT in [rdBinary] then
      Result := WinReg.ReadWideString(AName)
    else
      _ReadError(AName)
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadString(ARootKey: HKEY; const AKey, AName: string): string;
begin
  Result := abfRegReadStringA(ARootKey, AKey, AName);
end;

//------------------------------------------------------------------------------

function abfRegReadStringW(ARootKey: HKEY; const AKey, AName: WideString): WideString;
var
  WinReg: TabfRegistry;
  RDT: TabfRegDataType;
begin
  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, False) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);

    RDT := WinReg.GetDataTypeEx(AName);
    if RDT in [rdString, rdExpandString] then
      Result := WinReg.ReadString(AName)
    else if RDT in [rdBinary] then
      Result := WinReg.ReadWideString(AName)
    else
      _ReadError(AName)
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Reads a string value from registry. If not exists, returns a default value.

function abfRegReadStringDefA(ARootKey: HKEY; const AKey, AName,
  ADefaultValue: AnsiString): AnsiString;
var
  WinReg: TabfRegistry;
  RDT: TabfRegDataType;
begin
  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    try
      if not WinReg.OpenKey(AKey, False) then Abort;
      
      RDT := WinReg.GetDataTypeEx(AName);
      if RDT in [rdString, rdExpandString] then
        Result := WinReg.ReadString(AName)
      else if RDT in [rdBinary] then
        Result := WinReg.ReadWideString(AName)
      else
        Abort;
    except
      Result := ADefaultValue;
    end;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfRegReadStringDef(ARootKey: HKEY; const AKey, AName,
  ADefaultValue: string): string;
begin
  Result := abfRegReadStringDefA(ARootKey, AKey, AName, ADefaultValue);
end;

//------------------------------------------------------------------------------

function abfRegReadStringDefW(ARootKey: HKEY; const AKey, AName,
  ADefaultValue: WideString): WideString;
var
  WinReg: TabfRegistry;
  RDT: TabfRegDataType;
begin
  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    try
      if not WinReg.OpenKey(AKey, False) then Abort;
      
      RDT := WinReg.GetDataTypeEx(AName);
      if RDT in [rdString, rdExpandString] then
        Result := WinReg.ReadString(AName)
      else if RDT in [rdBinary] then
        Result := WinReg.ReadWideString(AName)
      else
        Abort;
    except
      Result := ADefaultValue;
    end;
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Writes a boolean value to registry.

procedure abfRegWriteBoolA(ARootKey: HKEY; const AKey, AName: AnsiString;
  AValue: Boolean);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteBool(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfRegWriteBool(ARootKey: HKEY; const AKey, AName: string;
  AValue: Boolean);
begin
  abfRegWriteBoolA(ARootKey, AKey, AName, AValue);
end;

//------------------------------------------------------------------------------

procedure abfRegWriteBoolW(ARootKey: HKEY; const AKey, AName: WideString;
  AValue: Boolean);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteBool(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Writes a float value to registry
procedure abfRegWriteFloatA(ARootKey: HKEY; const AKey, AName: AnsiString;
  AValue: Double);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey:= ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteFloat(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfRegWriteFloat(ARootKey: HKEY; const AKey, AName: string;
  AValue: Double);
begin
  abfRegWriteFloatA(ARootKey, AKey, AName, AValue);
end;

//------------------------------------------------------------------------------

procedure abfRegWriteFloatW(ARootKey: HKEY; const AKey, AName: WideString;
  AValue: Double);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey:= ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteFloat(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Writes a integer value to registry

procedure abfRegWriteIntegerA(ARootKey: HKEY; const AKey, AName: AnsiString;
  AValue: Integer);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteInteger(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfRegWriteInteger(ARootKey: HKEY; const AKey, AName: string;
  AValue: Integer);
begin
  abfRegWriteIntegerA(ARootKey, AKey, AName, AValue);
end;

//------------------------------------------------------------------------------

procedure abfRegWriteIntegerW(ARootKey: HKEY; const AKey, AName: WideString;
  AValue: Integer);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteInteger(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Writes an AnsiString value to registry

procedure abfRegWriteAnsiStringA(ARootKey: HKEY; const AKey, AName: AnsiString;
  const AValue: AnsiString);
var
  WinReg: TabfRegistry;
begin
  if Pos('%', AValue) > 0 then
  begin
    abfRegWriteExpandStringA(ARootKey, AKey, AName, AValue);
    Exit;
  end;

  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteString(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfRegWriteAnsiString(ARootKey: HKEY; const AKey, AName: string;
  const AValue: AnsiString);
begin
  abfRegWriteAnsiStringA(ARootKey, AKey, AName, AValue);
end;

//------------------------------------------------------------------------------

procedure abfRegWriteAnsiStringW(ARootKey: HKEY; const AKey, AName: WideString;
  const AValue: AnsiString);
var
  WinReg: TabfRegistry;
begin
  if Pos('%', AValue) > 0 then
  begin
    abfRegWriteExpandStringW(ARootKey, AKey, AName, AValue);
    Exit;
  end;

  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteString(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Writes a WideString value to registry

procedure abfRegWriteWideStringA(ARootKey: HKEY; const AKey, AName: AnsiString;
  const AValue: WideString);
var
  WinReg: TabfRegistry;
begin
  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteWideString(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfRegWriteWideString(ARootKey: HKEY; const AKey, AName: string;
  const AValue: WideString);
begin
  abfRegWriteWideStringA(ARootKey, AKey, AName, AValue);
end;

//------------------------------------------------------------------------------

procedure abfRegWriteWideStringW(ARootKey: HKEY; const AKey, AName: WideString;
  const AValue: WideString);
var
  WinReg: TabfRegistry;
begin
  WinReg := TabfRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteWideString(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Writes an expand string value to registry

procedure abfRegWriteExpandStringA(ARootKey: HKEY; const AKey, AName: AnsiString;
  const AValue: AnsiString);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteExpandString(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfRegWriteExpandString(ARootKey: HKEY; const AKey, AName: string;
  const AValue: AnsiString);
begin
  abfRegWriteExpandStringA(ARootKey, AKey, AName, AValue);
end;  

//------------------------------------------------------------------------------

procedure abfRegWriteExpandStringW(ARootKey: HKEY; const AKey, AName: WideString;
  const AValue: AnsiString);
var
  WinReg: TRegistry;
begin
  WinReg := TRegistry.Create;
  try
    WinReg.RootKey := ARootKey;
    if not WinReg.OpenKey(AKey, True) then
      raise ERegistryException.CreateFmt(SabfRegistry_UnableToOpenKey, [AKey]);
    WinReg.WriteExpandString(AName, AValue);
  finally
    WinReg.Free;
  end;
end;

//------------------------------------------------------------------------------
// abfRegFindFirstChangeNotification
//
//  PURPOSE:      This function begins a registry change notification
//                cycle. It will never return a handle that is already
//                signaled due to interim change notifications.
//
//                After a call to abfRegFindFirstChange, a call to
//                abfRegFindNextChangeNotification ensures that interim
//                notifications are not missed.
//
//                Since this routine allocates resources, it is
//                important that each call is matched up with a
//                corresponding abfRegFindCloseChangeNotification
//                call to free the resources.
//
//  PARAMETERS:   hKey - handle to the key to watch
//
//                bWatchSubtree - indicates whether or not to receive
//                notifications for changes in subkeys
//
//                dwNotifyFilter - identifies which changes should cause
//                notifications to occur
//
//                lprcd - pointer to a REG_CHANGE_DATA structure used
//                to cycle through the changes
//
//  RETURN VALUE: If the function succeeds, a wait able handle is
//                returned. If it fails, INVALID_HANDLE_VALUE is returned
//                and a call to GetLastError identifies why it failed.

function abfRegFindFirstChangeNotification(hKey: HKEY; bWatchSubtree: Boolean;
  dwNotifyFilter: DWORD; lprcd: LPREG_CHANGE_DATA): THandle;
var
  Res: LongInt;
begin
  lprcd.hKey := hKey;
  lprcd.bWatchSubtree := bWatchSubtree;
  lprcd.dwNotifyFilter := dwNotifyFilter;

// Create event to be signaled when changes occur
  Result := CreateEvent(nil, True, False, nil);

// Request registry change notifications
  with lprcd^ do
    Res := RegNotifyChangeKeyValue(hKey, bWatchSubtree, dwNotifyFilter,
      Result, True);

  if Res <> ERROR_SUCCESS then
  begin
     SetLastError(Res);
     Result := INVALID_HANDLE_VALUE;
     Exit;
  end;

// It is possible that this key handle has been used to receive registry
// notifications already. Thus, you will wait with a timeout of zero to clear
// interim notifications that might have occurred
  if WaitForSingleObject(Result, 0) = WAIT_OBJECT_0 then
  begin
  // There were some interim changes; they are cleared now, but you must call
  // the API again to request future notifications
    with lprcd^ do
      Res := RegNotifyChangeKeyValue(hKey, bWatchSubtree, dwNotifyFilter,
        Result, True);

    if Res <> ERROR_SUCCESS then
    begin
      SetLastError(Res);
      Result := INVALID_HANDLE_VALUE;
    end;
  end;
end;{function abfRegFindFirstChangeNotification}

//------------------------------------------------------------------------------
// abfRegFindNextChangeNotification
//
//  PURPOSE:      This function queues the next registry change
//                notification in a cycle. It must be preceded by a
//                call to abfRegFindFirstChangeNotification.
//
//                After calling this function, the handle returned by
//                abfRegFindFirstChangeNotification can be waited on again
//                to get the next change notification. Using these functions
//                in this manner, all interim change notifications will
//                be caught.
//
//  PARAMETERS:   lprcd - pointer to the same REG_CHANGE_DATA structure
//                that was initialized with abfRegFindFirstChangeNotification
//
//  RETURN VALUE: If the function succeeds, it returns TRUE. If it
//                fails, it returns FALSE and a call to GetLastError
//                identifies why it failed.

function abfRegFindNextChangeNotification(hChange: THandle;
  lprcd: LPREG_CHANGE_DATA): Boolean;
var
  Res: LongInt;
begin
  Result := False;

  if hChange = INVALID_HANDLE_VALUE then Exit;

// Reset the event so the handle can be waited on again
  if not ResetEvent(hChange) then Exit;

// If you call this function, you want to catch interim changes, so simply
// call the API again.
  with lprcd^ do
    Res := RegNotifyChangeKeyValue(hKey, bWatchSubtree, dwNotifyFilter,
      hChange, True);

  if Res = ERROR_SUCCESS then
    Result := True
  else
    SetLastError(Res);
end;

//------------------------------------------------------------------------------
//  abfRegFindCloseChangeNotification
//
//  PURPOSE:      This procedure frees the resources allocated by a call
//                to abfRegFindFirstChangeNotification.
//
//  PARAMETERS:   hChange - the waitable handle returned from
//                abfRegFindFirstChangeNotification

procedure abfRegFindCloseChangeNotification(hChange: THandle);
begin
// free event
  if hChange <> INVALID_HANDLE_VALUE then CloseHandle(hChange);
end;

//------------------------------------------------------------------------------

procedure _GetAutoexecKeyAndPath(AExecKind: TabfAutoExecKind; var AKey: HKEY;
  var ARegPath: string);
begin
  AKey := HKEY_CURRENT_USER;
  if AExecKind in [aekMachineRun, aekMachineRunOnce, aekServiceRun,
    aekServiceRunOnce] then AKey := HKEY_LOCAL_MACHINE;
  ARegPath := 'Software\Microsoft\Windows\CurrentVersion\'; // don't localize
  case AExecKind of
    aekMachineRun, aekUserRun        : ARegPath := ARegPath + 'Run';
    aekMachineRunOnce, aekUserRunOnce: ARegPath := ARegPath + 'RunOnce';
    aekServiceRun                    : ARegPath := ARegPath + 'RunServices';
    aekServiceRunOnce                : ARegPath := ARegPath + 'RunServicesOnce';
  end;
end;

//------------------------------------------------------------------------------
// Registers the file specified by Path in the system registry so that will be
// automatically executed at the next system start.
// Date: 03/14/2001

function abfRegisterAutoExec(AExecKind: TabfAutoExecKind;
  const ACmdLine: WideString): Boolean;
var
  Key: HKEY;
  RegPath: string;
begin
  Result := False;
  if (AExecKind in [aekServiceRun, aekServiceRunOnce]) and IsWinNT then Exit;
  _GetAutoexecKeyAndPath(AExecKind, Key, RegPath);
  try
    abfRegWriteAnsiStringA(Key, RegPath, abfExtractFileNameA(ACmdLine),
      abfGetShortPathNameW(ACmdLine));
    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------
// Unregisters the file specified by Path in the system registry so that it will
// not be automatically executed any more.
// Date: 03/14/2001

function abfUnregisterAutoExec(AExecKind: TabfAutoExecKind;
  const ACmdLine: WideString): Boolean;
var
  Key: HKEY;
  RegPath: string;
  S: string;
begin
  Result := False;
  if (AExecKind in [aekServiceRun, aekServiceRunOnce]) and IsWinNT then Exit;
  _GetAutoexecKeyAndPath(AExecKind, Key, RegPath);
  with TRegistry.Create do
  try
    RootKey := Key;
    if OpenKey(RegPath, False) then
    begin
      S := abfExtractFileNameA(ACmdLine);
      if not ValueExists(S) then Result := True else Result := DeleteValue(S);
    end;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure _DoInitialization;
begin
  _Registry := TabfRegistry.Create;
  _RegistryValueName := TStringList.Create;
end;

//------------------------------------------------------------------------------

procedure _DoFinalization;
begin
  FreeAndNil(_RegistryValueName);
  FreeAndNil(_Registry);
end;

{******************************************************************************}
initialization
{******************************************************************************}
  _DoInitialization;

{******************************************************************************}
finalization
{******************************************************************************}
  _DoFinalization;

end.
