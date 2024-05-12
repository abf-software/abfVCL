{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfSysUtils;

{$I abf.inc}

interface

uses
  Windows, Classes, SysUtils, {$IFDEF D9} WideStrings,{$ENDIF}
// ABF VCL
  abfClasses;

//==============================================================================
// OS version
//==============================================================================

const
  VER_BUILDNUMBER = $00000004;
  {$EXTERNALSYM VER_BUILDNUMBER}
  VER_MAJORVERSION = $00000002;
  {$EXTERNALSYM VER_MAJORVERSION}
  VER_MINORVERSION = $00000001;
  {$EXTERNALSYM VER_MINORVERSION}
  VER_PLATFORMID = $00000008;
  {$EXTERNALSYM VER_PLATFORMID}
  VER_SERVICEPACKMAJOR = $00000020;
  {$EXTERNALSYM VER_SERVICEPACKMAJOR}
  VER_SERVICEPACKMINOR = $00000010;
  {$EXTERNALSYM VER_SERVICEPACKMINOR}
  VER_SUITENAME = $00000040;
  {$EXTERNALSYM VER_SUITENAME}
  VER_PRODUCT_TYPE = $00000080;
  {$EXTERNALSYM VER_PRODUCT_TYPE}

  VER_NT_WORKSTATION = $0000001;
  {$EXTERNALSYM VER_NT_WORKSTATION}
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  {$EXTERNALSYM VER_NT_DOMAIN_CONTROLLER}
  VER_NT_SERVER = $0000003;
  {$EXTERNALSYM VER_NT_SERVER}

  PROCESSOR_ARCHITECTURE_INTEL            = 0;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_INTEL}
  PROCESSOR_ARCHITECTURE_MIPS             = 1;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_MIPS}
  PROCESSOR_ARCHITECTURE_ALPHA            = 2;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_ALPHA}
  PROCESSOR_ARCHITECTURE_PPC              = 3;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_PPC}
  PROCESSOR_ARCHITECTURE_SHX              = 4;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_SHX}
  PROCESSOR_ARCHITECTURE_ARM              = 5;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_ARM}
  PROCESSOR_ARCHITECTURE_IA64             = 6;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA64}
  PROCESSOR_ARCHITECTURE_ALPHA64          = 7;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_ALPHA64}
  PROCESSOR_ARCHITECTURE_MSIL             = 8;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_MSIL}
  PROCESSOR_ARCHITECTURE_AMD64            = 9;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_AMD64}
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64    = 10;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA32_ON_WIN64}

  PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_UNKNOWN}

  {$EXTERNALSYM SM_SERVERR2}
  SM_SERVERR2 = 89;

type
  POSVersionInfoExA = ^TOSVersionInfoExA;
  POSVersionInfoExW = ^TOSVersionInfoExW;
  POSVersionInfoEx = POSVersionInfoExW;
  _OSVERSIONINFOEXA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance AnsiString for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved:BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOEXA}
  _OSVERSIONINFOEXW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar; { Maintenance UnicodeString for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved:BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOEXW}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXW;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  TOSVersionInfoExW = _OSVERSIONINFOEXW;
  TOSVersionInfoEx = TOSVersionInfoExW;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  {$EXTERNALSYM OSVERSIONINFOEXA}
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  {$EXTERNALSYM OSVERSIONINFOEXW}
  OSVERSIONINFOEX = OSVERSIONINFOEXW;
  {$EXTERNALSYM OSVERSIONINFOEX}
  LPOSVERSIONINFOEXA = POSVERSIONINFOEXA;
  {$EXTERNALSYM LPOSVERSIONINFOEXA}
  LPOSVERSIONINFOEXW = POSVERSIONINFOEXW;
  {$EXTERNALSYM LPOSVERSIONINFOEXW}
  LPOSVERSIONINFOEX = LPOSVERSIONINFOEXW;
  {$EXTERNALSYM LPOSVERSIONINFOEX}
  RTL_OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  {$EXTERNALSYM RTL_OSVERSIONINFOEXW}
  PRTL_OSVERSIONINFOEXW = POSVERSIONINFOEXW;
  {$EXTERNALSYM PRTL_OSVERSIONINFOEXW}


  TabfOSVersionId = (viUnknown, viWinNT3, viWinNT4, viWin2000, viWinXP,
    viWin2003, viWin2003R2, viWinVista, viWin2008, viWin7, viWin2008R2,
    viWin8, viWin2012, viWin8_1, viWin2012R2, viWin10, viWin2016);

  TabfOSArchitecture = (arUnknown, arX86, arX64);
  TabfOSPlatform = (pfUnknown, pfWindows);
  TabfOSPlatformType = (ptUnknown, ptWorkstation, ptDomainController, ptServer);


  TabfOSVersion = class
  private
    FArchitecture: TabfOSArchitecture;
    FBuild: Integer;
    FId: TabfOSVersionId;
    FMajor: Integer;
    FMinor: Integer;
    FPlatform: TabfOSPlatform;
    FPlatformType: TabfOSPlatformType;
    FServicePackMajor: Integer;
    FServicePackMinor: Integer;
    procedure Clear;
    function GetName: WideString;
  public
    constructor Create;

    procedure Refresh;

    function Check(AMajor: Integer): Boolean; overload;
    function Check(AMajor, AMinor: Integer): Boolean; overload;
    function Check(AMajor, AMinor, AServicePackMajor: Integer): Boolean; overload;

    function ToString: WideString;

    property Architecture: TabfOSArchitecture read FArchitecture;
    property Build: Integer read FBuild;
    property Major: Integer read FMajor;
    property Minor: Integer read FMinor;
    property Name: WideString read GetName;
    property Platform: TabfOSPlatform read FPlatform;
    property ServicePackMajor: Integer read FServicePackMajor;
    property ServicePackMinor: Integer read FServicePackMinor;
  end;

var
  OSVersion: TabfOSVersion = nil;

const
  IsWin9x    : Boolean = False;
  IsWin95    : Boolean = False;
  IsWin95OSR2: Boolean = False;
  IsWin98    : Boolean = False;
  IsWin98SE  : Boolean = False;
  IsWinME    : Boolean = False;

  IsWin95OSR2orHigher: Boolean = False;
  IsWin98orHigher    : Boolean = False;
  IsWin98SEorHigher  : Boolean = False;

  IsWinNT  : Boolean = False;
  IsWow64  : Boolean = False;
  Is32bitOS: Boolean = False;
  Is64bitOS: Boolean = False;


//==============================================================================
// Exceptions
//==============================================================================

//------------------------------------------------------------------------------
// Returns error code depending on an exception type
function abfExceptionToResult(const E: Exception): HRESULT;

//==============================================================================
// Math, calculation, coordinates utilities
//==============================================================================

function Max(A, B: Integer): Integer;
function Min(A, B: Integer): Integer;

//------------------------------------------------------------------------------
// Raise base to an integral power. Very fast. Same as Borland's routine
function IntPower(Base: Extended; Exponent: Integer): Extended register;

//------------------------------------------------------------------------------
// Raise base to any power. For fractional exponents, or |exponents| > MaxInt,
// base must be > 0
function Power(Base, Exponent: Extended): Extended;

//------------------------------------------------------------------------------
// Use this function to align the InnerRect rect relative to the OuterRect rect
// depending on the HorzAlign and VertAlign parameters
function abfAlignRectInRect(const InnerRect, OuterRect: TRect;
  HorzAlign: TabfHorzAlign; VertAlign: TabfVertAlign): TRect;

//==============================================================================
// System utilities
//==============================================================================

//------------------------------------------------------------------------------
// Loads the specified module into the address space of the calling process
// Note: the parameters should be the same as for LoadLibrary and LoadLibraryEx
function abfLoadLibraryExA(const AFileName: AnsiString;
  AFileHandle: THandle; AFlags: DWORD): HMODULE; stdcall;
function abfLoadLibraryEx(const AFileName: string;
  AFileHandle: THandle; AFlags: DWORD): HMODULE; stdcall;
function abfLoadLibraryExW(const AFileName: WideString;
  AFileHandle: THandle; AFlags: DWORD): HMODULE; stdcall;
function abfLoadLibraryA(const AFileName: AnsiString): HMODULE; stdcall;
function abfLoadLibrary(const AFileName: string): HMODULE; stdcall;
function abfLoadLibraryW(const AFileName: WideString): HMODULE; stdcall;

//------------------------------------------------------------------------------
// abfSafeLoadLibrary calls abfLoadLibrary, disabling normal Win32 error message
// popup dialogs if the requested file can't be loaded. abfSafeLoadLibrary also
// preserves the current FPU control word (precision, exception masks) across
// the abfLoadLibrary call (in case the DLL you're loading hammers
// the FPU control word in its initialization, as many MS DLLs do)
function abfSafeLoadLibraryA(const AFileName: AnsiString;
  AErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE; stdcall;
function abfSafeLoadLibrary(const AFileName: string;
  AErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE; stdcall;
function abfSafeLoadLibraryW(const AFileName: WideString;
  AErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE; stdcall;

//------------------------------------------------------------------------------
// Returns enviroment variable value by name
function abfGetEnvVarA(const AEnvName: AnsiString): AnsiString;
function abfGetEnvVar(const AEnvName: string): string;
function abfGetEnvVarW(const AEnvName: WideString): WideString;

//------------------------------------------------------------------------------
// Expands string that contains any number of %VariableName% environment
// variables with its values
function abfExpandEnvStrA(const AStr: AnsiString): AnsiString;
function abfExpandEnvStr(const AStr: string): string;
function abfExpandEnvStrW(const AStr: WideString): WideString;

//------------------------------------------------------------------------------
// Retrieves the NetBIOS name of the local computer.
function abfGetComputerNameA: AnsiString;
function abfGetComputerName: string;
function abfGetComputerNameW: WideString;

//------------------------------------------------------------------------------
// Retrieves the name of the user associated with the current thread.
function abfGetUserNameA: AnsiString;
function abfGetUserName: string;
function abfGetUserNameW: WideString;

//------------------------------------------------------------------------------
// Returns workgroup name if network is set
function abfGetWorkgroupNameA: AnsiString;
function abfGetWorkgroupName: string;
function abfGetWorkgroupNameW: WideString;

//------------------------------------------------------------------------------
// Returns a rect of current working area (desktop without the taskbar)
function abfGetWorkAreaRect: TRect;

//------------------------------------------------------------------------------
// Returns current state for shift keys (Shift, Ctrl, Alt)
function abfGetShiftState: TShiftState;

//------------------------------------------------------------------------------
// Returns a name of Pressed/Released key by lParam of KeyboardMessage
function abfGetKeyTextA(lParam: LongInt): AnsiString;
function abfGetKeyText(lParam: LongInt): string;
function abfGetKeyTextW(lParam: LongInt): WideString;

//------------------------------------------------------------------------------
// Returns a name value ($00xx1234) for the given keyboard layout.
function abfGetLayoutID(KL: HKL): LongWord;

//------------------------------------------------------------------------------
// Returns a string value for the specified param of the specified locale.
function abfGetLocaleInfoStringA(const ALocID : LCID;
  const ALocType : LCTYPE): AnsiString;
function abfGetLocaleInfoString(const ALocID : LCID;
  const ALocType : LCTYPE): string;
function abfGetLocaleInfoStringW(const ALocID : LCID;
  const ALocType : LCTYPE): WideString;

//------------------------------------------------------------------------------
// Returns a list of supported keyboard layouts. Items of AList will contain
// names of layouts, Objects contain HKL values for each layout.
// Note: The AList will not be cleared before using.
procedure abfGetKeyboardLayoutList(AList: TStrings);

//------------------------------------------------------------------------------
// Determines is CapsLock on or off.
function abfIsCapsLock: Boolean;

//------------------------------------------------------------------------------
// Determines is NumLock on or off.
function abfIsNumLock: Boolean;

//------------------------------------------------------------------------------
// Determines is ScrollLock on or off.
function abfIsScrollLock: Boolean;

//------------------------------------------------------------------------------
// Determines is screen saver running or not.
function abfIsScreenSaverRunning: Boolean;

//------------------------------------------------------------------------------
// Outputs sound thru the Speaker under Win9x.
procedure abfSound(Frequency: Cardinal);

//------------------------------------------------------------------------------
// Turn off the Speaker sound under Win9x.
procedure abfNoSound;

//------------------------------------------------------------------------------
// Outputs sound thru the Speaker under any windows version.
procedure abfSpeakerBeep(Frequency, Duration: Cardinal);

//------------------------------------------------------------------------------
// Retrieves information about the current system. If the function is called
// from a 64-bit application or called on Windows 2000 and lower,
// it is equivalent to the GetSystemInfo function.
procedure abfGetNativeSystemInfo(var lpSystemInformation: TSystemInfo); stdcall;

//------------------------------------------------------------------------------
// Returns version of shell32.dll
function abfGetShellVersion: Integer;


//==============================================================================
// Comparison and conversion utilities
//==============================================================================

//------------------------------------------------------------------------------
// Performs a comparison of two TabfCustomVersion variables.
function abfCompareVersion(AValue1, AValue2: TabfCustomVersion): Integer;

//------------------------------------------------------------------------------
// Converts a byte value to the binary string (01010111)
function abfByteToBin(Value: Byte): string;

//------------------------------------------------------------------------------
// Converts an integer value to the binary string (01101001). The Digits
// parameter specifies lengts of resulting string.
function abfIntToBin(Value, Digits: Integer): string;

//------------------------------------------------------------------------------
// Converts a binary string (01101001) to the integer value.
function abfBinToInt(const Value: string): Integer;

//------------------------------------------------------------------------------
// Converts an integer value to the octal string (1277).
function abfIntToOct(Value, Digits: Integer): string;

//------------------------------------------------------------------------------
// Converts an octal string (1277) to the integer value.
function abfOctToInt(const Value: string): Integer;

//------------------------------------------------------------------------------
// Converts an integer value to the roman string (XIV).
function abfIntToRoman(Value: Integer): string;

//------------------------------------------------------------------------------
// Converts a roman string (XIV) to the integer value.
function abfRomanToInt(const Value: string): Integer;

//------------------------------------------------------------------------------
// Converts a boolean value to a string.
function abfBooleanToStr(B: Boolean): string;

//------------------------------------------------------------------------------
// Converts a string value to a boolean.
function abfStrToBoolean(const S: string): Boolean;

//------------------------------------------------------------------------------
// Converts a TabfCustomVersion value to the string (x.x.x.x) value.
function abfVersionToStr(AVersion: TabfCustomVersion): string;

//------------------------------------------------------------------------------
// Converts a string version value (x.x.x.x) to a TabfCustomVersion value.
function abfStrToVersion(const AVersionString: string;
  AVersion: TabfCustomVersion): Boolean;

//------------------------------------------------------------------------------
// Converts TPoint to the string (X and Y coord separated with ',' ) value
function abfPointToStr(const P: TPoint): string;

//------------------------------------------------------------------------------
// Converts string (X and Y coord separated with ',' ) to the TPoint value
function abfStrToPoint(const S: string): TPoint;

//------------------------------------------------------------------------------
// Converts TRect to the string (L,T,B,R) value
function abfRectToStr(const R: TRect): string;

//------------------------------------------------------------------------------
// Converts string (L,T,B,R) to the TRect value
function abfStrToRect(const S: string): TRect;

//------------------------------------------------------------------------------
// Converts string of specified format to the date value.
function abfStrToDate(S, Format: string): TDateTime;

//------------------------------------------------------------------------------
// Converts string of specified format to the time value.
function abfStrToTime(S, Format: string): TDateTime;

//------------------------------------------------------------------------------
// Converts the binary data to "FFDDAA00" hex string.
function abfBinaryToString(var Buffer; Size: Integer): AnsiString;

//------------------------------------------------------------------------------
// Converts "FFDDAA00" hex string to the binary data. Result is a number of
// bytes are converted. If some error occurs result is less then 0. Any
// non-hexadecimal characters are removed from string before the conversion.
function abfStringToBinary(const S: AnsiString;
  var Buffer; Size: Integer): Integer;

//------------------------------------------------------------------------------
// Performs a binary comparison of two TBytes arrays.
function abfSameBytes(AValue1, AValue2: TBytes): Boolean;

//------------------------------------------------------------------------------
// Converts DWORD to TBytes array.
function abfDWORDToBytes(AValue: DWORD): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to DWORD.
function abfBytesToDWORD(const AValue: TBytes): DWORD;

//------------------------------------------------------------------------------
// Creates a globally unique identifier.
function abfCreateGUID: TGUID;

//------------------------------------------------------------------------------
// Converts TGUID to TBytes array.
function abfGUIDToBytes(AValue: TGUID): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to TGUID.
function abfBytesToGUID(const AValue: TBytes): TGUID;

//------------------------------------------------------------------------------
// Converts "ffddaa00" hex string to TBytes array.
function abfHexToBytes(const AHex: AnsiString): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to "ffddaa00" hex string.
function abfBytesToHex(const ABytes: TBytes): AnsiString;

//------------------------------------------------------------------------------
// Converts Int64 to TBytes array.
function abfInt64ToBytes(AValue: Int64): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to Int64.
function abfBytesToInt64(const AValue: TBytes): Int64;

//------------------------------------------------------------------------------
// Converts AnsiString to TBytes array.
function abfStrToBytes(const AStr: AnsiString;
  ANullTerminated: Boolean = True): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to AnsiString.
function abfBytesToStr(const ABytes: TBytes): AnsiString;

//------------------------------------------------------------------------------
// Converts AnsiString to "ffddaa00" hex string.
function abfStrToHex(const AStr: AnsiString;
  ANullTerminated: Boolean = True): AnsiString;

//------------------------------------------------------------------------------
// Converts "ffddaa00" hex string to AnsiString.
function abfHexToStr(const AHex: AnsiString): AnsiString;

//------------------------------------------------------------------------------
// Creates a globally unique identifier and puts it into Intel processor
// byte order.
function abfCreateUID: AnsiString;

//------------------------------------------------------------------------------
// Performs a string comparison of two UID strings.
function abfSameUID(const AValue1, AValue2: AnsiString): Boolean;

//------------------------------------------------------------------------------
// Checks is AValue a valid UID string.
function abfIsValidUID(const AValue: AnsiString): Boolean;

//------------------------------------------------------------------------------
// Checks is AValues a valid array of UID string.
function abfIsValidUIDs(const AValues: AnsiString): Boolean;

//------------------------------------------------------------------------------
// Converts UID string to TBytes array.
function abfUIDToBytes(const AValue: AnsiString): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to UID string.
function abfBytesToUID(const AValue: TBytes): AnsiString;

//------------------------------------------------------------------------------
// Converts WideString to TBytes array.
function abfWStrToBytes(const AStr: WideString;
  ANullTerminated: Boolean = True): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to WideString.
function abfBytesToWStr(const ABytes: TBytes): WideString;

//------------------------------------------------------------------------------
// Converts WideString to "ffddaa00" hex string.
function abfWStrToHex(const AStr: WideString;
  ANullTerminated: Boolean = True): AnsiString;

//------------------------------------------------------------------------------
// Converts "ffddaa00" hex string to WideString.
function abfHexToWStr(const AHex: AnsiString): WideString;

//------------------------------------------------------------------------------
// Converts Word to TBytes array.
function abfWordToBytes(AValue: Word): TBytes;

//------------------------------------------------------------------------------
// Converts TBytes array to Word.
function abfBytesToWord(const AValue: TBytes): Word;


//==============================================================================
// Memory, records, pointer utilites
//==============================================================================

//------------------------------------------------------------------------------
// Frees an object reference and replaces the reference with nil.
{$IfNDef D5}
procedure FreeAndNil(var Obj);
{$EndIf D5}

//------------------------------------------------------------------------------
// Releases memory allocated for a dynamic variable and assign nil.
procedure abfDispose(var P);

//------------------------------------------------------------------------------
// Fills the record with zeros and set the size value into the first DWORD
procedure abfInitRecord(var Rec; Size: Integer);


//------------------------------------------------------------------------------
// Creates, frees and copies an array of const.
// Based on code written by Rudy Velthuis (rvelthuis@gmx.de).

type
  TabfConstArray = array of TVarRec;

//------------------------------------------------------------------------------
// Copies a TVarRec and its contents. If the content is referenced
// the value will be copied to a new location and the reference
// updated.
function abfCopyVarRec(const AItem: TVarRec): TVarRec;

//------------------------------------------------------------------------------
// Creates a TabfConstArray out of the values given. Uses abfCopyVarRec
// to make copies of the original elements.
function abfCreateConstArray(const AElements: array of const): TabfConstArray;

//------------------------------------------------------------------------------
// TVarRecs created by abfCopyVarRec must be finalized with this function.
// You should not use it on other TVarRecs.
procedure abfFinalizeVarRec(var AItem: TVarRec);

//------------------------------------------------------------------------------
// A TabfConstArray contains TVarRecs that must be finalized. This function
// does that for all items in the array.
procedure abfFinalizeConstArray(var AArr: TabfConstArray);


//==============================================================================
// File utilities
//==============================================================================

const
{$IfNDef D6}
  PathDelim  = '\';
  DriveDelim = ':';
  PathSep    = ';';
{$EndIf D6}

  SabfSlash = '\';
  SabfPathPack = '...';
  SAppPath: WideString = '';  // Value will be assigned on initialization

  faNormal       = $00000080; // FILE_ATTRIBUTE_NORMAL              0x00000080
  faTemporary    = $00000100; // FILE_ATTRIBUTE_TEMPORARY           0x00000100
  faSparse       = $00000200; // FILE_ATTRIBUTE_SPARSE_FILE         0x00000200
  faReparsePoint = $00000400; // FILE_ATTRIBUTE_REPARSE_POINT       0x00000400
  faCompressed   = $00000800; // FILE_ATTRIBUTE_COMPRESSED          0x00000800
  faOffline      = $00001000; // FILE_ATTRIBUTE_OFFLINE             0x00001000
  faNotIndexed   = $00002000; // FILE_ATTRIBUTE_NOT_CONTENT_INDEXED 0x00002000
  faEncrypted    = $00004000; // FILE_ATTRIBUTE_ENCRYPTED           0x00004000

  faAnyFiles     = faAnyFile or faNormal;
  faOnlyFiles    = faAnyFiles and not (faDirectory or faVolumeID);
  faDirsAndFiles = faAnyFiles and not (faVolumeID);

type
  TabfSearchRecA = record
    Time: Integer;
    Size: Int64;
    Attr: Integer;
    Name: AnsiString;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindDataA;
  end;

  TabfSearchRec = TabfSearchRecA;

  TabfSearchRecW = record
    Time: Integer;
    Size: Int64;
    Attr: Integer;
    Name: WideString;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindDataW;
  end;

  TabfForEachFileCallBackA = procedure(const Path: AnsiString;
    const SearchRec: TabfSearchRec; var Continue: Boolean);
  TabfForEachFileCallBackW = procedure(const Path: WideString;
    const SearchRec: TabfSearchRecW; var Continue: Boolean);
  TabfForEachFileCallBack = TabfForEachFileCallBackA;

  TabfForEachFileCallBackMethodA = procedure(const Path: AnsiString;
    const SearchRec: TabfSearchRec; var Continue: Boolean) of object;
  TabfForEachFileCallBackMethodW = procedure(const Path: WideString;
    const SearchRec: TabfSearchRecW; var Continue: Boolean) of object;
  TabfForEachFileCallBackMethod = TabfForEachFileCallBackMethodA;


//------------------------------------------------------------------------------
// Retrieves fully-qualified path for the file that contains
// the specified module that the current process owns.
// Date: 11/02/2007

function abfGetModuleFileNameA(AModule: HMODULE): AnsiString;
function abfGetModuleFileName(AModule: HMODULE): string;
function abfGetModuleFileNameW(AModule: HMODULE): WideString;

//------------------------------------------------------------------------------
// Retrieves file system type for the specified drive
// Date: 07/28/2008

type
  TabfFileSystem = (afsUnknown, afsFAT12, afsFAT16, afsFAT32, afsNTFS);

function abfGetDriveFileSystemA(ADrive: AnsiChar): TabfFileSystem;
function abfGetDriveFileSystem(ADrive: Char): TabfFileSystem;
function abfGetDriveFileSystemW(ADrive: WideChar): TabfFileSystem;

//------------------------------------------------------------------------------
// Returns drive type for the specified root path name
// Date: 07/28/2008

function abfGetDriveTypeA(ARootPathName: AnsiString): Cardinal;
function abfGetDriveType(ARootPathName: string): Cardinal;
function abfGetDriveTypeW(ARootPathName: WideString): Cardinal;

//------------------------------------------------------------------------------
// Gets a value indicating whether a drive is ready.
// Date: 07/28/2008

function abfIsDriveReadyA(ARootPathName: AnsiString): Boolean;
function abfIsDriveReady(ARootPathName: string): Boolean;
function abfIsDriveReadyW(ARootPathName: WideString): Boolean;

//------------------------------------------------------------------------------
// Retrieves information about the amount of space that is available on a disk volume.
function abfGetDiskFreeSpaceA(ARootPathName: AnsiString): Int64;
function abfGetDiskFreeSpace(ARootPathName: string): Int64;
function abfGetDiskFreeSpaceW(ARootPathName: WideString): Int64;

//------------------------------------------------------------------------------
// Retrieves information about maximum media size for the specified drive.
function abfGetDriveMaxMediaSizeA(ADrive: AnsiString): Int64;
function abfGetDriveMaxMediaSize(ADrive: string): Int64;
function abfGetDriveMaxMediaSizeW(ADrive: WideString): Int64;

//==============================================================================
// Name routines
//==============================================================================

//------------------------------------------------------------------------------
// Checks is FileName a valid file name string.
function abfIsValidFileNameA(const FileName: AnsiString): Boolean;
function abfIsValidFileName(const FileName: string): Boolean;
function abfIsValidFileNameW(const FileName: WideString): Boolean;

//------------------------------------------------------------------------------
// Adds a slash to the end of the Path string if it is not present
function abfAddSlashA(const Path: AnsiString): AnsiString;
function abfAddSlash(const Path: string): string;
function abfAddSlashW(const Path: WideString): WideString;

//------------------------------------------------------------------------------
// Adds a slash to the begin of the Path string if it is not present.
function abfAddSlashBeforeA(const Path: AnsiString): AnsiString;
function abfAddSlashBefore(const Path: string): string;
function abfAddSlashBeforeW(const Path: WideString): WideString;

//------------------------------------------------------------------------------
// Removes a slash from the end of the Path string
function abfRemoveSlashA(const Path: AnsiString): AnsiString;
function abfRemoveSlash(const Path: string): string;
function abfRemoveSlashW(const Path: WideString): WideString;

//------------------------------------------------------------------------------
// Replaces all doubled slashes with one slash.
function abfRemoveExtraSlashA(const FileName: AnsiString): AnsiString;
function abfRemoveExtraSlash(const FileName: string): string;
function abfRemoveExtraSlashW(const FileName: WideString): WideString;

//------------------------------------------------------------------------------
// Returns the drive portion of a file name.
function abfExtractFileDriveA(const FileName: AnsiString): AnsiString;
function abfExtractFileDrive(const FileName: string): string;
function abfExtractFileDriveW(const FileName: WideString): WideString;

//------------------------------------------------------------------------------
// Returns the drive and directory portions of a file name. Same to
// ExtractFilePath but supports '/'.
function abfExtractFilePathA(const FileName: AnsiString): AnsiString;
function abfExtractFilePath(const FileName: string): string;
function abfExtractFilePathW(const FileName: WideString): WideString;

//------------------------------------------------------------------------------
// Extracts the name and extension parts of a file name. Same to ExtractFileName
// but supports '/'.
function abfExtractFileNameA(const FileName: AnsiString): AnsiString;
function abfExtractFileName(const FileName: string): string;
function abfExtractFileNameW(const FileName: WideString): WideString;

//------------------------------------------------------------------------------
// Returns the name of the file without extension.
function abfExtractFileNameOnlyA(const FileName: AnsiString): AnsiString;
function abfExtractFileNameOnly(const FileName: string): string;
function abfExtractFileNameOnlyW(const FileName: WideString): WideString;

//------------------------------------------------------------------------------
// Changes the extension of a file name. Same to ChangeFileExt
// but supports '/'.
function abfChangeFileExtA(const FileName, Extension: AnsiString): AnsiString;
function abfChangeFileExt(const FileName, Extension: string): string;
function abfChangeFileExtW(const FileName, Extension: WideString): WideString;

//------------------------------------------------------------------------------
// Extracts the extension part of a file name. Same to ExtractFileExt
// but supports '/'.
function abfExtractFileExtA(const FileName: AnsiString): AnsiString;
function abfExtractFileExt(const FileName: string): string;
function abfExtractFileExtW(const FileName: WideString): WideString;

//------------------------------------------------------------------------------
// Returns the name given from the FileName and matched the Mask. Supports only
// simple masks 'C:\AA\*.pas', '?.*', etc. Doesn't support masks such a 'p*.c*'
// Example:
//   FileName = 'D:\Copy\autoexec.bat'
//   Mask = 'C:\*.bak'
//   Result = 'C:\autoexec.bak' }
function abfGetFileNameMatchedMaskA(const FileName, Mask: AnsiString): AnsiString;
function abfGetFileNameMatchedMask(const FileName, Mask: string): string;
function abfGetFileNameMatchedMaskW(const FileName, Mask: WideString): WideString;

//------------------------------------------------------------------------------
// Packs given path string to fit the MaxLength.
// Example: "C:\ABF\...\some.txt"
function abfPackPathStringA(const Path: AnsiString;
  MaxLength: Integer): AnsiString;
function abfPackPathString(const Path: string; MaxLength: Integer): string;
function abfPackPathStringW(const Path: WideString;
  MaxLength: Integer): WideString;

//------------------------------------------------------------------------------
// Retrieves the short path form of the specified path.
function abfGetShortPathNameA(const ALongPathName: AnsiString): AnsiString;
function abfGetShortPathName(const ALongPathName: string): string;
function abfGetShortPathNameW(const ALongPathName: WideString): WideString;

//------------------------------------------------------------------------------
// Retrieves the long path form of the specified path.
function abfGetLongPathNameA(const AShortPathName: AnsiString): AnsiString;
function abfGetLongPathName(const AShortPathName: string): string;
function abfGetLongPathNameW(const AShortPathName: WideString): WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the system directory used by WOW64.
// This directory is not present on 32-bit Windows.
function abfGetSystemWow64DirectoryA: AnsiString;
function abfGetSystemWow64Directory: ansistring;
function abfGetSystemWow64DirectoryW: WideString;

//------------------------------------------------------------------------------
// Determines whether the specified process is running under WOW64.
// Date: 10/06/2010
function abfIsWow64Process(AProcessHandle: THandle): Boolean;

//------------------------------------------------------------------------------
// Determines whether the current process is running under WOW64.
// Date: 10/06/2010
function abfIsWow64: Boolean;

//------------------------------------------------------------------------------
// Determines whether a file is an executable (.exe) file, and if so, 
// which subsystem runs the executable file.
// Date: 06/12/2013

const
  SCS_64BIT_BINARY = 6;
  {$EXTERNALSYM SCS_64BIT_BINARY}

function abfGetBinaryTypeA(const AAppName: AnsiString;
  var ABinType: DWORD): Boolean;
function abfGetBinaryType(const AAppName: string;
  var ABinType: DWORD): Boolean;
function abfGetBinaryTypeW(const AAppName: WideString;
  var ABinType: DWORD): Boolean;


//==============================================================================
// Command line routines
//==============================================================================

//------------------------------------------------------------------------------
// Parses command line parameters for current process to string list.
// Leading '/',  '-' are removed.
procedure abfCommandLineToStringsA(const AList: TabfAnsiStrings);
procedure abfCommandLineToStrings(const AList: TStrings);
procedure abfCommandLineToStringsW(const AList: TabfWideStrings);

//------------------------------------------------------------------------------
// Returns the command line string for current process.
function abfGetCommandLineA: AnsiString;
function abfGetCommandLine: string;
function abfGetCommandLineW: WideString;

//------------------------------------------------------------------------------
//Returns a specified parameter from command line.
function abfParamStrA(AIndex: Integer): AnsiString;
function abfParamStr(AIndex: Integer): string;
function abfParamStrW(AIndex: Integer): WideString;

//------------------------------------------------------------------------------
// Parses command line parameters to string list.
// Leading '/' and '-' are removed.
procedure abfParseCommandLineA(const ACommandLine: AnsiString;
  const AList: TabfAnsiStrings);
procedure abfParseCommandLine(const ACommandLine: string;
  const AList: TStrings);
procedure abfParseCommandLineW(const ACommandLine: WideString;
  const AList: TabfWideStrings);


//==============================================================================
// Paths, directory managing
//==============================================================================

//------------------------------------------------------------------------------
// Creates directory.
function abfCreateDirectoryA(const ADirName: AnsiString): Boolean;
function abfCreateDirectory(const ADirName: string): Boolean;
function abfCreateDirectoryW(const ADirName: WideString): Boolean;

//------------------------------------------------------------------------------
// Adds a directory to the search path used to locate DLLs for the application.
function abfSetDllDirectoryA(ADirName: PAnsiChar): Boolean;
function abfSetDllDirectory(ADirName: PAnsiChar): Boolean;
function abfSetDllDirectoryW(ADirName: PWideChar): Boolean;

//------------------------------------------------------------------------------
// Checks directory existing.
function abfDirectoryExistsA(const ADirName: AnsiString): Boolean;
function abfDirectoryExists(const ADirName: string): Boolean;
function abfDirectoryExistsW(const ADirName: WideString): Boolean;

//------------------------------------------------------------------------------
// Creates all the directories along a directory path if they do not already
// exist. You can specify an optional ACreatedDirList param to receive the list
// of all created directory names, if you don't need this information set the
// ACreatedDirList parameter to nil.
procedure abfForceDirectoriesExA(ADirName: AnsiString;
  ACreatedDirList: TabfAnsiStrings);
procedure abfForceDirectoriesEx(ADirName: string; ACreatedDirList: TStrings);
procedure abfForceDirectoriesExW(ADirName: WideString;
  ACreatedDirList: TabfWideStrings);

//------------------------------------------------------------------------------
// Creates all the directories along a directory path if they do not already
// exist.
procedure abfForceDirectoriesA(ADirName: AnsiString);
procedure abfForceDirectories(ADirName: string);
procedure abfForceDirectoriesW(ADirName: WideString);

//------------------------------------------------------------------------------
// Retrieves the path of the current directory.
function abfGetCurrentDirectoryA: AnsiString;
function abfGetCurrentDirectory: string;
function abfGetCurrentDirectoryW: WideString;

//------------------------------------------------------------------------------
// Change the path of the current directory.
function abfSetCurrentDirectoryA(const APath: AnsiString): Boolean;
function abfSetCurrentDirectory(const APath: string): Boolean;
function abfSetCurrentDirectoryW(const APath: WideString): Boolean;

//------------------------------------------------------------------------------
// Retrieves the path of the Windows directory.
function abfGetWindowsDirA: AnsiString;
function abfGetWindowsDir: string;
function abfGetWindowsDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the Windows system directory.
function abfGetSystemDirA: AnsiString;
function abfGetSystemDir: string;
function abfGetSystemDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the TEMP directory.
function abfGetTempDirA: AnsiString;
function abfGetTempDir: string;
function abfGetTempDirW: WideString;

//------------------------------------------------------------------------------
// Returns an unique name for newly created temporary directory.
function abfGetUniqueTempDirA(const APrefix: AnsiString): AnsiString;
function abfGetUniqueTempDir(const APrefix: string): string;
function abfGetUniqueTempDirW(const APrefix: WideString): WideString;

//------------------------------------------------------------------------------
// Returns an unique name for newly created temporary file.
function abfGetUniqueTempFileNameA(const APrefix: AnsiString): AnsiString;
function abfGetUniqueTempFileName(const APrefix: string): string;
function abfGetUniqueTempFileNameW(const APrefix: WideString): WideString;

//------------------------------------------------------------------------------
// Removes directory. If AWithContent parameter is True then this function
// removes directory with its content.
function abfRemoveDirectoryA(const ADirName: AnsiString;
  AWithContent: Boolean = True): Boolean;
function abfRemoveDirectory(const ADirName: string;
  AWithContent: Boolean = True): Boolean;
function abfRemoveDirectoryW(const ADirName: WideString;
  AWithContent: Boolean = True): Boolean;


//==============================================================================
// Relative names
//==============================================================================

//------------------------------------------------------------------------------
// Concatenates specified RelativePath relative with the given BasePath.
function abfConcatRelativePathA(const BasePath,
  RelativePath: AnsiString): AnsiString;
function abfConcatRelativePath(const BasePath, RelativePath: string): string;
function abfConcatRelativePathW(const BasePath,
  RelativePath: WideString): WideString;

//------------------------------------------------------------------------------
// Returns the full path name for a relative file name.
function abfExpandFileNameA(const AFileName: AnsiString): AnsiString;
function abfExpandFileName(const AFileName: string): string;
function abfExpandFileNameW(const AFileName: WideString): WideString;

//------------------------------------------------------------------------------
// Expands relative path from BasePath.
function abfExpandRelativePathExA(const BasePath,
  RelativePath: AnsiString): AnsiString;
function abfExpandRelativePathEx(const BasePath, RelativePath: string): string;
function abfExpandRelativePathExW(const BasePath,
  RelativePath: WideString): WideString;

//------------------------------------------------------------------------------
// Expands relative path from the starting directory of the application.
function abfExpandRelativePathA(const RelativePath: AnsiString): AnsiString;
function abfExpandRelativePath(const RelativePath: string): string;
function abfExpandRelativePathW(const RelativePath: WideString): WideString;

//------------------------------------------------------------------------------
// Expands relative path from the curent directory, if path doesn't exists,
// expands it from the starting directory of the application
function abfSmartExpandRelativePathA(const RelativePath: AnsiString): AnsiString;
function abfSmartExpandRelativePath(const RelativePath: string): string;
function abfSmartExpandRelativePathW(const RelativePath: WideString): WideString;

//------------------------------------------------------------------------------
// Expands relative file name from BasePath.
function abfExpandRelativeFileNameExA(const BasePath,
  RelativeFileName: AnsiString): AnsiString;
function abfExpandRelativeFileNameEx(const BasePath,
  RelativeFileName: string): string;
function abfExpandRelativeFileNameExW(const BasePath,
  RelativeFileName: WideString): WideString;

//------------------------------------------------------------------------------
// Expands relative file name from the starting directory of the application
function abfExpandRelativeFileNameA(const RelativeFileName: AnsiString): AnsiString;
function abfExpandRelativeFileName(const RelativeFileName: string): string;
function abfExpandRelativeFileNameW(const RelativeFileName: WideString): WideString;

//------------------------------------------------------------------------------
// Expands relative file name from the curent directory, if path doesn't exists,
// expands file name from the starting directory of the application.
function abfSmartExpandRelativeFileNameA(const RelativeFileName: AnsiString): AnsiString;
function abfSmartExpandRelativeFileName(const RelativeFileName: string): string;
function abfSmartExpandRelativeFileNameW(const RelativeFileName: WideString): WideString;


//==============================================================================
// File managing
//==============================================================================

//------------------------------------------------------------------------------
// Checks file existing.
function abfFileExistsA(FileName: AnsiString): Boolean;
function abfFileExists(FileName: string): Boolean;
function abfFileExistsW(FileName: WideString): Boolean;

//------------------------------------------------------------------------------
// Use this function to get a size of the file specified by the FileName
// parameter. If function returns True the FileSize parameter contains the size
// of file (in bytes). If function returns False - some error occurred and the
// FileSize parameter contains a wrong information. Use GetLastError API
// function to determine the error.
function abfGetFileSizeA(const FileName: AnsiString;
  var FileSize: Int64): Boolean;
function abfGetFileSize(const FileName: string;
  var FileSize: Int64): Boolean;
function abfGetFileSizeW(const FileName: WideString;
  var FileSize: Int64): Boolean;

//------------------------------------------------------------------------------
// Copies the SrcFile file to the DestFile file.
function abfCopyFileA(SrcFile, DstFile: AnsiString): Boolean;
function abfCopyFile(SrcFile, DstFile: string): Boolean;
function abfCopyFileW(SrcFile, DstFile: WideString): Boolean;

//------------------------------------------------------------------------------
// Moves an existing file or a directory, including its children.
//
// This function will move (rename) either a file or a directory (including
// its children) either in the same directory or across directories.
// The one caveat is that the MoveFile function will fail on directory moves
// when the destination is on a different volume.
// If a file is moved across volumes, MoveFile does not move the security
// descriptor with the file. The file will be assigned the default security
// descriptor in the destination directory.
function abfRenameFileA(OldName, NewName: AnsiString): Boolean;
function abfRenameFile(OldName, NewName: string): Boolean;
function abfRenameFileW(OldName, NewName: WideString): Boolean;

//------------------------------------------------------------------------------
// Copies the SrcFile file to the DestFile file block by block. The BlockSize
// parameter specifies a size of copying block. If you specify BlockSize as 0 -
// CabfCopyFileExBlockSize bytes blocks will be used. Use OnProgress to specify
// an event that will be called with Sender parameter after the copying of each
// block. You can terminate procedure routine by setting Terminate parameter of
// the OnProgress event True. If function returns False - some error occurred
// and file wasn't copyed properly. Use GetLastError API function to determine
// the error.
const
  CabfCopyFileExBlockSize = 65536;

type
  TabfCopyFileProgressEvent = procedure(Sender: TObject;
    CopiedSize, TotalSize: Int64; var Terminate: Boolean);

function abfCopyFileExA(const SrcFile, DstFile: AnsiString; BlockSize: LongWord;
  OnProgress: TabfCopyFileProgressEvent; Sender: TObject): Boolean;
function abfCopyFileEx(const SrcFile, DstFile: string; BlockSize: LongWord;
  OnProgress: TabfCopyFileProgressEvent; Sender: TObject): Boolean;
function abfCopyFileExW(const SrcFile, DstFile: WideString; BlockSize: LongWord;
  OnProgress: TabfCopyFileProgressEvent; Sender: TObject): Boolean;

//------------------------------------------------------------------------------
// Deletes the AFileName file.
function abfDeleteFileA(AFileName: AnsiString): Boolean;
function abfDeleteFile(AFileName: string): Boolean;
function abfDeleteFileW(AFileName: WideString): Boolean;

//------------------------------------------------------------------------------
// Creates a new file.
function abfFileCreateA(const FileName: AnsiString): Integer; overload;
function abfFileCreate(const FileName: string): Integer; overload;
function abfFileCreateW(const FileName: WideString): Integer; overload;
function abfFileCreateA(const FileName: AnsiString;
  Rights: Integer): Integer; overload;
function abfFileCreate(const FileName: string;
  Rights: Integer): Integer; overload;
function abfFileCreateW(const FileName: WideString;
  Rights: Integer): Integer; overload;

//------------------------------------------------------------------------------
//Opens a specified file using a specified access mode.
function abfFileOpenA(const FileName: AnsiString; Mode: LongWord): Integer;
function abfFileOpen(const FileName: string; Mode: LongWord): Integer;
function abfFileOpenW(const FileName: WideString; Mode: LongWord): Integer;

//------------------------------------------------------------------------------
// Runs/Executes file with possibility of waiting the end of the file execution.
function abfRunExA(const FileName: AnsiString; Wait: Boolean;
  ShowWindow: Boolean = True): DWORD;
function abfRunEx(const FileName: string; Wait: Boolean;
  ShowWindow: Boolean = True): DWORD;
function abfRunExW(const FileName: WideString; Wait: Boolean;
  ShowWindow: Boolean = True): DWORD;

//------------------------------------------------------------------------------
// Runs/Executes file.
function abfRunA(const FileName: AnsiString;
  ShowWindow: Boolean = True): DWORD;
function abfRun(const FileName: string;
  ShowWindow: Boolean = True): DWORD;
function abfRunW(const FileName: WideString;
  ShowWindow: Boolean = True): DWORD;

//------------------------------------------------------------------------------
// Executes console apps and gets its output.
function abfRunAndGetOutputA(const AFileName: AnsiString;
  const ACurrentDir: AnsiString; out AOutput: AnsiString): DWORD;
function abfRunAndGetOutput(const AFileName: string;
  const ACurrentDir: string; out AOutput: AnsiString): DWORD;
function abfRunAndGetOutputW(const AFileName: WideString;
  const ACurrentDir: WideString; out AOutput: AnsiString): DWORD;

//------------------------------------------------------------------------------
// Removes file in any way. If the file are locked by the system, it will be
// deleted at once it become unlocked.
procedure abfAsyncDeleteFileA(const FileName: AnsiString);
procedure abfAsyncDeleteFile(const FileName: string);
procedure abfAsyncDeleteFileW(const FileName: WideString);

//------------------------------------------------------------------------------
// Replaces DstName with SrcName file in any way. If the file DstName are locked
// by the system, it will be replaced at once it become unlocked.
procedure abfAsyncReplaceFileA(const SrcName, DstName: AnsiString);
procedure abfAsyncReplaceFile(const SrcName, DstName: string);
procedure abfAsyncReplaceFileW(const SrcName, DstName: WideString);

//------------------------------------------------------------------------------
// Replaces DstName with SrcName file in any way. If the file DstName are locked
// by the system, it will be replaced at once it become unlocked. After the
// replacing runs the file with CmdLnParams.
procedure abfAsyncReplaceFileAndRunA(const SrcName, DstName,
  CmdLnParams: AnsiString);
procedure abfAsyncReplaceFileAndRun(const SrcName, DstName,
  CmdLnParams: string);
procedure abfAsyncReplaceFileAndRunW(const SrcName, DstName,
  CmdLnParams: WideString);

//------------------------------------------------------------------------------
// abfFindFirst searches the directory given by Path for the first entry that
// matches the filename given by Path and the attributes given by Attr. The
// result is returned in the search record given by SearchRec. The return
// value is zero if the function was successful. Otherwise the return value
// is a system error code. After calling abfFindFirst, always call abfFindClose.
// abfFindFirst is typically used with abfFindNext and abfFindClose as follows:
//
//   Result := abfFindFirst(Path, Attr, SearchRec);
//   while Result = 0 do
//   begin
//     ProcessSearchRec(SearchRec);
//     Result := abfFindNext(SearchRec);
//   end;
//   abfFindClose(SearchRec);
//
// where ProcessSearchRec represents user-defined code that processes the
// information in a search record.

function abfFindFirstA(const Path: AnsiString; Attr: Integer;
  var F: TabfSearchRecA): Integer;
function abfFindFirst(const Path: string; Attr: Integer;
  var F: TabfSearchRec): Integer;
function abfFindFirstW(const Path: WideString; Attr: Integer;
  var F: TabfSearchRecW): Integer;

//------------------------------------------------------------------------------
// abfFindNext returs the next entry that matches the name and attributes
// specified in a previous call to abfFindFirst. The search record must be one
// that was passed to abfFindFirst. The return value is zero if the function was
// successful. Otherwise the return value is a system error code. }

function abfFindNextA(var F: TabfSearchRecA): Integer;
function abfFindNext(var F: TabfSearchRec): Integer;
function abfFindNextW(var F: TabfSearchRecW): Integer;

//------------------------------------------------------------------------------
// abfFindClose terminates a abfFindFirst/abfFindNext sequence and frees memory
// and system resources allocated by abfFindFirst.
// Every abfFindFirst/abfFindNext must end with a call to abfFindClose. }

procedure abfFindCloseA(var F: TabfSearchRecA);
procedure abfFindClose(var F: TabfSearchRec);
procedure abfFindCloseW(var F: TabfSearchRecW);

//------------------------------------------------------------------------------
// The abfFindFirstChangeNotification function creates a change
// notification handle and sets up initial change notification filter
// conditions. A wait on a notification handle succeeds when a change matching
// the filter conditions occurs in the specified directory or subtree.
// However, the function does not indicate the change that satisfied
// the wait condition.

function abfFindFirstChangeNotificationA(const APathName: AnsiString;
  AWatchSubtree: BOOL; ANotifyFilter: DWORD): THandle;
function abfFindFirstChangeNotification(const APathName: string;
  AWatchSubtree: BOOL; ANotifyFilter: DWORD): THandle;
function abfFindFirstChangeNotificationW(const APathName: WideString;
  AWatchSubtree: BOOL; ANotifyFilter: DWORD): THandle;


//------------------------------------------------------------------------------
// Process CallBack procedure for each files matched the PathWithMask and given
// attributes.
function abfForEachFileA(const PathWithMask: AnsiString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackA): Integer; overload;
function abfForEachFile(const PathWithMask: string; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBack): Integer; overload;
function abfForEachFileW(const PathWithMask: WideString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackW): Integer; overload;

function abfForEachFileA(const PathWithMask: AnsiString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackMethodA): Integer; overload;
function abfForEachFile(const PathWithMask: string; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackMethod): Integer; overload;
function abfForEachFileW(const PathWithMask: WideString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackMethodW): Integer; overload;

//------------------------------------------------------------------------------
// Converts a TDateTime value to the TFileTime value.
function abfDateTimeToFileTime(DateTime: TDateTime): TFileTime;

//------------------------------------------------------------------------------
// Converts a TFileTime value to the TDateTime value.
function abfFileTimeToDateTime(FileTime: TFileTime): TDateTime;

//------------------------------------------------------------------------------
// Retrieves a set of FAT file system attributes
// for a specified file or directory.
function abfGetFileAttrA(AFileName: AnsiString): DWORD;
function abfGetFileAttr(AFileName: string): DWORD;
function abfGetFileAttrW(AFileName: WideString): DWORD;

//------------------------------------------------------------------------------
// Sets the attributes for a file or directory.
function abfSetFileAttrA(AFileName: AnsiString; AFileAttributes: DWORD): Boolean;
function abfSetFileAttr(AFileName: string; AFileAttributes: DWORD): Boolean;
function abfSetFileAttrW(AFileName: WideString; AFileAttributes: DWORD): Boolean;

//------------------------------------------------------------------------------
// Modifies attributes of a specified file.
function abfModifyFileAttrA(const FileName: AnsiString;
  RemoveAttr, AddAttr: Integer): Integer;
function abfModifyFileAttr(const FileName: string;
  RemoveAttr, AddAttr: Integer): Integer;
function abfModifyFileAttrW(const FileName: WideString;
  RemoveAttr, AddAttr: Integer): Integer;

//------------------------------------------------------------------------------
// Returns date and time for the specified file
function abfGetFileDateTimeA(const FileName: AnsiString): TDateTime;
function abfGetFileDateTime(const FileName: string): TDateTime;
function abfGetFileDateTimeW(const FileName: WideString): TDateTime;

//------------------------------------------------------------------------------
// Sets date and time for the specified file
procedure abfSetFileDateTimeA(const FileName: AnsiString;
  const DateTime: TDateTime);
procedure abfSetFileDateTime(const FileName: string; const DateTime: TDateTime);
procedure abfSetFileDateTimeW(const FileName: WideString;
  const DateTime: TDateTime);

//------------------------------------------------------------------------------
// Returns the date/time that the specified file was last accessed.
function abfGetFileLastAccessA(const FileName: AnsiString): TDateTime;
function abfGetFileLastAccess(const FileName: string): TDateTime;
function abfGetFileLastAccessW(const FileName: WideString): TDateTime;

//------------------------------------------------------------------------------
// Returns the date/time that the specified file was created.
function abfGetFileCreationA(const FileName: AnsiString): TDateTime;
function abfGetFileCreation(const FileName: string): TDateTime;
function abfGetFileCreationW(const FileName: WideString): TDateTime;

//------------------------------------------------------------------------------
// Returns the date/time that the specified file was last written to.
function abfGetFileLastWriteA(const FileName: AnsiString): TDateTime;
function abfGetFileLastWrite(const FileName: string): TDateTime;
function abfGetFileLastWriteW(const FileName: WideString): TDateTime;

//------------------------------------------------------------------------------
// Sets the specified file's last-access timestamp
function abfSetFileLastAccessA(const FileName: AnsiString;
  const DateTime: TDateTime): Boolean;
function abfSetFileLastAccess(const FileName: string;
  const DateTime: TDateTime): Boolean;
function abfSetFileLastAccessW(const FileName: WideString;
  const DateTime: TDateTime): Boolean;

//------------------------------------------------------------------------------
// Sets the specified file's creation timestamp
function abfSetFileCreationA(const FileName: AnsiString;
  const DateTime: TDateTime): Boolean;
function abfSetFileCreation(const FileName: string;
  const DateTime: TDateTime): Boolean;
function abfSetFileCreationW(const FileName: WideString;
  const DateTime: TDateTime): Boolean;

//------------------------------------------------------------------------------
// Sets the specified file's last-write timestamp
function abfSetFileLastWriteA(const FileName: AnsiString;
  const DateTime: TDateTime): Boolean;
function abfSetFileLastWrite(const FileName: string;
  const DateTime: TDateTime): Boolean;
function abfSetFileLastWriteW(const FileName: WideString;
  const DateTime: TDateTime): Boolean;

//------------------------------------------------------------------------------
// Returns a build's date and time of the specified application. Based on
// Alexander Kramarenko's <www.akhome.da.ru> routines.
// Note: Normally works only with files created by Borland compilers.
function abfGetFileBuildDateTimeA(FileName: AnsiString;
  var BuildTime: TDateTime): Boolean;
function abfGetFileBuildDateTime(FileName: string;
  var BuildTime: TDateTime): Boolean;
function abfGetFileBuildDateTimeW(FileName: WideString;
  var BuildTime: TDateTime): Boolean;

//==============================================================================
// Resource routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns file description from VERSIONINFO
function abfGetFileDescriptionA(const AFileName: AnsiString): AnsiString;
function abfGetFileDescription(const AFileName: string): string;
function abfGetFileDescriptionW(const AFileName: WideString): WideString;

//------------------------------------------------------------------------------
// Returns resource string for specified language from module instance
function abfLoadStringEx(AInstance: HMODULE; AID: DWORD;
  ALangID: DWORD): WideString;

//------------------------------------------------------------------------------
// Returns resource string from module instance
function abfLoadString(AInstance: HMODULE; AID: DWORD): WideString;



//==============================================================================
// Borland, Delphi/C++Builder utilities
//==============================================================================

//------------------------------------------------------------------------------
// Same to the RegisterClass function, but doesn't generates exception if the
// class is already registered.
procedure abfRegisterClass(AClass: TPersistentClass);

//------------------------------------------------------------------------------
// Same to the RegisterClasses function, but doesn't generates exception if some
// of classese are already registered.
procedure abfRegisterClasses(AClasses: array of TPersistentClass);

//==============================================================================
// Trial and debug routines
//==============================================================================

var
  abfLogFileName: WideString = ''; // Value will be assigned on initialization
{$IfDef abfVCLDebug}
  abfTraceIndent: string = ''; // Usually contains spaces, but can be a preffix.
{$EndIf abfVCLDebug}

//------------------------------------------------------------------------------
// Checks is the version trial. Shows trial message if it is.
procedure abfCheckTrialVersion;

//------------------------------------------------------------------------------
// Writes S to the log file
procedure abfTrace(const S: string);

//------------------------------------------------------------------------------
// Writes S to the log file and changes indent. If Indent > 0 ident will be
// increased by Indent spaces. If Indent < 0 ident will be decreased by Indent
// chars.
procedure abfTraceEx(const S: string; Indent: Integer);

//------------------------------------------------------------------------------
// Writes S to the log file with the 'Error: ' prefix
procedure abfTraceError(const S: string);


{******************************************************************************}
implementation
{******************************************************************************}

uses
{$IfDef abfVCLTrial}
  ShellAPI, abfVclConsts,
{$EndIf abfVCLTrial}
  abfConsts, abfStrUtils, abfRegistry;

{$I abf_init.inc}

//==============================================================================
// OS version
//==============================================================================

constructor TabfOSVersion.Create;
begin
  inherited Create;

  Refresh;
end;

//------------------------------------------------------------------------------

procedure TabfOSVersion.Clear;
begin
  FArchitecture := arUnknown;
  FBuild := 0;
  FMajor := 0;
  FMinor := 0;
  FId := viUnknown;
  FPlatform := pfUnknown;
  FPlatformType := ptUnknown;
  FServicePackMajor := 0;
  FServicePackMinor := 0;
end;

//------------------------------------------------------------------------------

function TabfOSVersion.GetName;
begin
  Result := '';
//  SVersion32 = '32-bit Edition';
//  SVersion64 = '64-bit Edition';
//  SWindows = 'Windows';
//  SWindowsVista = 'Windows Vista';
//  SWindowsServer2008 = 'Windows Server 2008';
//  SWindows7 = 'Windows 7';
//  SWindowsServer2008R2 = 'Windows Server 2008 R2';
//  SWindows2000 = 'Windows 2000';
//  SWindowsXP = 'Windows XP';
//  SWindowsServer2003 = 'Windows Server 2003';
//  SWindowsServer2003R2 = 'Windows Server 2003 R2';
//  SWindowsServer2012 = 'Windows Server 2012';
//  SWindows8 = 'Windows 8';
end;

//------------------------------------------------------------------------------

var
  _GetVersionExW: function(
    lpVersionInfo: POSVersionInfoExW): BOOL; stdcall = nil;

procedure TabfOSVersion.Refresh;
var
  SysInfo: TSystemInfo;
  VerInfo: TOSVersionInfoExW;
begin
  Clear;

  ZeroMemory(@VerInfo, SizeOf(VerInfo));
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);

  if Assigned(@_GetVersionExW) then
  begin
    if not _GetVersionExW(@VerInfo) then
    begin
      VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfoW);

      if not _GetVersionExW(@VerInfo) then Exit;
    end;
  end else
    Exit;

  if not (VerInfo.dwPlatformId in [VER_PLATFORM_WIN32_NT]) then Exit;

  FPlatform := pfWindows;
  FMajor := VerInfo.dwMajorVersion;
  FMinor := VerInfo.dwMinorVersion;
  FBuild := VerInfo.dwBuildNumber;
  FServicePackMajor := VerInfo.wServicePackMajor;
  FServicePackMinor := VerInfo.wServicePackMinor;

  ZeroMemory(@SysInfo, SizeOf(SysInfo));
  abfGetNativeSystemInfo(SysInfo);

  case SysInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_AMD64, PROCESSOR_ARCHITECTURE_IA64:
      FArchitecture := arX64;
    PROCESSOR_ARCHITECTURE_INTEL:
      FArchitecture := arX86;
  else
    FArchitecture := arUnknown;
  end;

  case VerInfo.dwPlatformId of
    VER_PLATFORM_WIN32_NT:
      begin
        case VerInfo.wProductType of
          VER_NT_WORKSTATION:       FPlatformType := ptWorkstation;
          VER_NT_DOMAIN_CONTROLLER: FPlatformType := ptDomainController;
          VER_NT_SERVER:            FPlatformType := ptServer;
        end;

        case FMajor of
          3: FId := viWinNT3;
          4: FId := viWinNT4;
          5: case FMinor of
               0: FId := viWin2000;
               1: FId := viWinXP;
               2: begin
                    if (FPlatformType = ptWorkstation) and
                       (FArchitecture = arX64) then
                      FId := viWinXP
                    else
                      begin
                        if GetSystemMetrics(SM_SERVERR2) = 0 then
                          FId := viWin2003
                        else
                          FId := viWin2003R2;
                      end;
                  end;
             end;
          6: case FMinor of
               0: if FPlatformType = ptWorkstation then
                    FId := viWinVista
                  else
                    FId := viWin2008;
               1: if FPlatformType = ptWorkstation then
                    FId := viWin7
                  else
                    FId := viWin2008R2;
               2: if FPlatformType = ptWorkstation then
                    FId := viWin8
                  else
                    FId := viWin2012;
               3: if FPlatformType = ptWorkstation then
                    FId := viWin8_1
                  else
                    FId := viWin2012R2;
             end;
         10: case FMinor of
               0: if FPlatformType = ptWorkstation then
                    FId := viWin10
                  else
                    FId := viWin2016;
             end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

function TabfOSVersion.Check(AMajor: Integer): Boolean;
begin
  Result := Major >= AMajor;
end;

//------------------------------------------------------------------------------

function TabfOSVersion.Check(AMajor, AMinor: Integer): Boolean;
begin
  Result := (Major > AMajor) or ((Major = AMajor) and (Minor >= AMinor));
end;

//------------------------------------------------------------------------------

function TabfOSVersion.Check(
  AMajor, AMinor, AServicePackMajor: Integer): Boolean;
begin
  Result := (Major > AMajor) or ((Major = AMajor) and (Minor > AMinor)) or
    ((Major = AMajor) and (Minor = AMinor)
    and (ServicePackMajor >= AServicePackMajor));
end;

//------------------------------------------------------------------------------

function TabfOSVersion.ToString: WideString;
//resourcestring
//  SVersionStr = '%s (Version %d.%d, Build %d, %5:s)';
//  SSPVersionStr = '%s Service Pack %4:d (Version %1:d.%2:d, Build %3:d, %5:s)';
//const
//  CVersionStr: array[Boolean] of PResStringRec = (@SVersionStr, @SSPVersionStr);
//  CEditionStr: array[Boolean] of PResStringRec = (@SVersion32, @SVersion64);

begin
  Result := '';
//  Result := Format(LoadResString(CVersionStr[ServicePackMajor <> 0]),
//    [Name, Major, Minor, Build, ServicePackMajor,
//    LoadResString(CEditionStr[FArchitecture = arIntelX64])]);
end;


//==============================================================================
// Exceptions
//==============================================================================

//------------------------------------------------------------------------------
// Returns error code depending on an exception type

function abfExceptionToResult(const E: Exception): HRESULT;
begin
  if E is EAccessViolation then
    Result := E_POINTER  else
  if E is EInvalidPointer then
    Result := E_POINTER  else
  if E is EOutOfMemory then
    Result := E_OUTOFMEMORY  else
  if E is EAbort then
    Result := E_FAIL
  else
    Result := E_UNEXPECTED
end;

//==============================================================================
// Math, calculation, coordinates utilities
//==============================================================================

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

//------------------------------------------------------------------------------

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

//------------------------------------------------------------------------------
// Raise base to an integral power. Very fast. Same as Borland's routine

function IntPower(Base: Extended; Exponent: Integer): Extended;
{$IFDEF WIN32}
asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
{$ENDIF}
{$IFDEF WIN64}
begin

//end;
{$ENDIF}
end;

//------------------------------------------------------------------------------
// Raise base to any power. For fractional exponents, or |exponents| > MaxInt,
// base must be > 0. 

function Power(Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then
    Result := 1.0               { n**0 = 1 }
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0               { 0**n = 0, n > 0 }
  else if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := IntPower(Base, Integer(Trunc(Exponent)))
  else
    Result := Exp(Exponent * Ln(Base))
end;

//------------------------------------------------------------------------------
// Use this function to align the InnerRect rect relative to the OuterRect rect
// depending on the HorzAlign and VertAlign prameters.
// 12/05/2001

function abfAlignRectInRect(const InnerRect, OuterRect: TRect;
  HorzAlign: TabfHorzAlign; VertAlign: TabfVertAlign): TRect;
var
  InnerWidth, InnerHeight: Integer;
begin
  InnerWidth  := InnerRect.Right - InnerRect.Left;
  InnerHeight := InnerRect.Bottom - InnerRect.Top;
  case HorzAlign of
    haLeft  : Result.Left := OuterRect.Left;
    haCenter: Result.Left := OuterRect.Left +
      (OuterRect.Right - OuterRect.Left - InnerWidth) div 2;
    haRight : Result.Left := OuterRect.Right - InnerWidth;
  end;
  case VertAlign of
    vaTop   : Result.Top := OuterRect.Top;
    vaCenter: Result.Top := OuterRect.Top +
      (OuterRect.Bottom - OuterRect.Top - InnerHeight) div 2;
    vaBottom: Result.Top := OuterRect.Bottom - InnerHeight;
  end;
  Result.Right  := Result.Left + InnerWidth;
  Result.Bottom := Result.Top + InnerHeight;
end;


//==============================================================================
// System utilities
//==============================================================================

//------------------------------------------------------------------------------
// Loads the specified module into the address space of the calling process
// Note: the parameters should be the same as for LoadLibrary and LoadLibraryEx

var
  _LoadLibraryExA: function (lpFileName: PAnsiChar; hFile: THandle;
      dwFlags: DWORD): HMODULE; stdcall = nil;

function abfLoadLibraryExA(const AFileName: AnsiString;
  AFileHandle: THandle; AFlags: DWORD): HMODULE; stdcall;
begin
  Result := 0;
  if not Assigned(@_LoadLibraryExA) then Exit;

  Result := _LoadLibraryExA(PAnsiChar(AFileName), AFileHandle, AFlags);
end;

//------------------------------------------------------------------------------

function abfLoadLibraryEx(const AFileName: string;
  AFileHandle: THandle; AFlags: DWORD): HMODULE; stdcall;
begin
  Result := abfLoadLibraryExA(AFileName, AFileHandle, AFlags);
end;

//------------------------------------------------------------------------------

var
  _LoadLibraryExW: function (lpFileName: PWideChar; hFile: THandle;
      dwFlags: DWORD): HMODULE; stdcall = nil;

function abfLoadLibraryExW(const AFileName: WideString;
  AFileHandle: THandle; AFlags: DWORD): HMODULE; stdcall;
begin
  if not IsWinNT then
  begin
    Result := abfLoadLibraryExA(AnsiString(AFileName), AFileHandle, AFlags);

    Exit;
  end;

  Result := 0;
  if not Assigned(@_LoadLibraryExW) then Exit;

  Result := _LoadLibraryExW(PWideChar(AFileName), AFileHandle, AFlags);
end;

//------------------------------------------------------------------------------

var
  _LoadLibraryA: function (lpFileName: PAnsiChar): HMODULE; stdcall = nil;

function abfLoadLibraryA(const AFileName: AnsiString): HMODULE; stdcall;
begin
  Result := 0;
  if not Assigned(@_LoadLibraryA) then Exit;

  Result := _LoadLibraryA(PAnsiChar(AFileName));
end;

//------------------------------------------------------------------------------

function abfLoadLibrary(const AFileName: string): HMODULE; stdcall;
begin
  Result := abfLoadLibraryA(AFileName);
end;

//------------------------------------------------------------------------------

var
  _LoadLibraryW: function (lpFileName: PWideChar): HMODULE; stdcall = nil;

function abfLoadLibraryW(const AFileName: WideString): HMODULE; stdcall;
begin
  if not IsWinNT then
  begin
    Result := abfLoadLibraryA(AnsiString(AFileName));

    Exit;
  end;

  Result := 0;
  if not Assigned(@_LoadLibraryW) then Exit;

  Result := _LoadLibraryW(PWideChar(AFileName));
end;


//------------------------------------------------------------------------------
// abfSafeLoadLibrary calls abfLoadLibrary, disabling normal Win32 error message
// popup dialogs if the requested file can't be loaded. abfSafeLoadLibrary also
// preserves the current FPU control word (precision, exception masks) across
// the abfLoadLibrary call (in case the DLL you're loading hammers
// the FPU control word in its initialization, as many MS DLLs do)

function abfSafeLoadLibraryA(const AFileName: AnsiString;
  AErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE; stdcall;
var
  OldMode: UINT;
  FPUControlWord: Word;
begin
  OldMode := SetErrorMode(AErrorMode);
  try
    {$IFDEF WIN32}
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := abfLoadLibraryA(AFilename);
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
    {$ELSE}
    try
      Result := abfLoadLibraryA(AFilename);
    finally
    end;
    {$ENDIF}
  finally
    SetErrorMode(OldMode);
  end;
end;

//------------------------------------------------------------------------------

function abfSafeLoadLibrary(const AFileName: string;
  AErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE; stdcall;
begin
  Result := abfSafeLoadLibraryA(AFileName, AErrorMode);
end;

//------------------------------------------------------------------------------

function abfSafeLoadLibraryW(const AFileName: WideString;
  AErrorMode: UINT = SEM_NOOPENFILEERRORBOX): HMODULE; stdcall;
var
  OldMode: UINT;
  FPUControlWord: Word;
begin
  OldMode := SetErrorMode(AErrorMode);
  try
    {$IFDEF WIN32}
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := abfLoadLibraryW(AFilename);
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
    {$ENDIF}
    {$IFDEF WIN64}
    try
      Result := abfLoadLibraryW(AFilename);
    finally
    end;
    {$ENDIF}
  finally
    SetErrorMode(OldMode);
  end;
end;


//------------------------------------------------------------------------------
// Returns enviroment variable value by name
// Date: 08/12/2000
// Unicode version added 06/28/2007

function abfGetEnvVarA(const AEnvName: AnsiString): AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  Result := '';

  BufLen := 32767;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    GetEnvironmentVariableA(PAnsiChar(AEnvName), Buf, BufLen);
    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetEnvVar(const AEnvName: string): string;
begin
  Result := abfGetEnvVarA(AEnvName);
end;

//------------------------------------------------------------------------------

function abfGetEnvVarW(const AEnvName: WideString): WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetEnvVarA(AEnvName);
    Exit;
  end;

  Result := '';

  BufLen := 32767;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    GetEnvironmentVariableW(PWideChar(AEnvName), Buf, BufLen);
    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Expands string that contains any number of %VariableName% environment
// variables with its values
// Date: 06/28/2007

function abfExpandEnvStrA(const AStr: AnsiString): AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  Result := AStr;

  BufLen := ExpandEnvironmentStringsA(PAnsiChar(AStr), nil, 0);
  if BufLen = 0 then Exit;

  GetMem(Buf, (BufLen + 2) * SizeOf(Buf^));
  try
    if ExpandEnvironmentStringsA(PAnsiChar(AStr), Buf, BufLen) <> 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfExpandEnvStr(const AStr: string): string;
begin
  Result := abfExpandEnvStrA(AStr);
end;

//------------------------------------------------------------------------------

function abfExpandEnvStrW(const AStr: WideString): WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfExpandEnvStrA(AStr);
    Exit;
  end;

  Result := AStr;

  BufLen := ExpandEnvironmentStringsW(PWideChar(AStr), nil, 0);
  if BufLen = 0 then Exit;

  GetMem(Buf, (BufLen + 2) * SizeOf(Buf^));
  try
    if ExpandEnvironmentStringsW(PWideChar(AStr), Buf, BufLen) <> 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Retrieves the NetBIOS name of the local computer.
// Date: 07/28/2008

function abfGetComputerNameA: AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  Result := '';
  BufLen := MAX_PATH + 1;
  Buf := GetMemory(BufLen * SizeOf(Buf^));
  try
    ZeroMemory(Buf, BufLen * SizeOf(Buf^));
    if not GetComputerNameA(Buf, BufLen) then Exit;
    Result := Buf;
  finally
    if Assigned(Buf) then FreeMemory(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetComputerName: string;
begin
  Result := abfGetComputerNameA;
end;

//------------------------------------------------------------------------------

function abfGetComputerNameW: WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetComputerNameA;
    Exit;
  end;

  Result := '';
  BufLen := MAX_PATH + 1;
  Buf := GetMemory(BufLen * SizeOf(Buf^));
  try
    ZeroMemory(Buf, BufLen * SizeOf(Buf^));
    if not GetComputerNameW(Buf, BufLen) then Exit;
    Result := Buf;
  finally
    if Assigned(Buf) then FreeMemory(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Retrieves the name of the user associated with the current thread.
// Date: 07/28/2008

function abfGetUserNameA: AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  Result := '';
  BufLen := MAX_PATH + 1;
  Buf := GetMemory(BufLen * SizeOf(Buf^));
  try
    ZeroMemory(Buf, BufLen * SizeOf(Buf^));
    if not GetUserNameA(Buf, BufLen) then Exit;
    Result := Buf;
  finally
    if Assigned(Buf) then FreeMemory(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetUserName: string;
begin
  Result := abfGetUserNameA;
end;

//------------------------------------------------------------------------------

function abfGetUserNameW: WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetUserNameA;
    Exit;
  end;

  Result := '';
  BufLen := MAX_PATH + 1;
  Buf := GetMemory(BufLen * SizeOf(Buf^));
  try
    ZeroMemory(Buf, BufLen * SizeOf(Buf^));
    if not GetUserNameW(Buf, BufLen) then Exit;
    Result := Buf;
  finally
    if Assigned(Buf) then FreeMemory(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Returns workgroup name if network is set.
// Date: 06/28/2007

function abfGetWorkgroupNameA: AnsiString;
begin
  Result := '';

  if IsWinNT then
    Result := abfGetEnvVarA('USERDOMAIN')
  else
// Read registry for Win9x
  if IsWin9x then
    Result := abfRegReadStringDef(HKEY_LOCAL_MACHINE,
      'System\CurrentControlSet\Services\VxD\VNETSUP', 'Workgroup', '');

// Make upper case
  Result := AnsiUpperCase(Result);
end;

//------------------------------------------------------------------------------

function abfGetWorkgroupName: string;
begin
  Result := abfGetWorkgroupNameA;
end;

//------------------------------------------------------------------------------

function abfGetWorkgroupNameW: WideString;
begin
  Result := '';

  if IsWinNT then
    Result := abfGetEnvVarW(WideString('USERDOMAIN'))
  else
// Read registry for Win9x
  if IsWin9x then
    Result := abfRegReadStringDef(HKEY_LOCAL_MACHINE,
      'System\CurrentControlSet\Services\VxD\VNETSUP', 'Workgroup', '');

// Make upper case
  Result := WideUpperCase(Result);
end;

//------------------------------------------------------------------------------
// Returns a rect of current working area (desktop without the taskbar)
// Date: 04/01/2000

function abfGetWorkAreaRect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
end;

//------------------------------------------------------------------------------
// Returns current state for shift keys (Shift, Ctrl, Alt).
// Date: 07/18/2000

function abfGetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT  ) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU   ) < 0 then Include(Result, ssAlt);
end;


//------------------------------------------------------------------------------
// Returns a name of Pressed/Released key by lParam of KeyboardMessage
// Date: 08/12/2000
// Unicode version added 06/28/2007

function abfGetKeyTextA(lParam: LongInt): AnsiString;
var
  Buf: array[0..255] of AnsiChar;
begin
  GetKeyNameTextA(lParam, @Buf, Length(Buf));
  Result := Buf;
end;

//------------------------------------------------------------------------------

function abfGetKeyText(lParam: LongInt): string;
begin
  Result := abfGetKeyTextA(lParam);
end;

//------------------------------------------------------------------------------

function abfGetKeyTextW(lParam: LongInt): WideString;
var
  Buf: array[0..255] of WideChar;
begin
  GetKeyNameTextW(lParam, @Buf, Length(Buf));
  Result := Buf;
end;

//------------------------------------------------------------------------------

const
  SReg_KeyboardLayoutsFmt = 'System\CurrentControlSet\Control\Keyboard Layouts\%.8x'; // do not localize
  SReg_KeyboardLayoutID   = 'layout id';   // do not localize
  SReg_KeyboardLayoutText = 'layout text'; // do not localize

//------------------------------------------------------------------------------
// Returns a name value ($00xx1234) for the given keyboard layout.
// Date: 08/12/2000

function abfGetLayoutID(KL: HKL): LongWord;
var
  CurentID: Cardinal;
  i, ValueSize: Integer;
  S: string;
  Key: HKey;
  KeyName: array[0..63] of Char;
  Value: array[0..255] of Char;
begin
  Result := LoWord(KL);
  if (KL and $F0000000) = 0 then Exit;
  S := Format('%.4x', [HiWord(KL) and $0FFF]); // Hi word without 1 byte
// Search thru registry for LayoutID same to HiWord of KL 
  for i := 1 to $ff do
  begin
    CurentID := LongWord(i shl 16) or Result; // $00xx1234
    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, StrFmt(KeyName, SReg_KeyboardLayoutsFmt,
      [CurentID]), 0, KEY_READ, Key) = ERROR_SUCCESS then
      try
        ValueSize := SizeOf(Value);
        if RegQueryValueEx(Key, SReg_KeyboardLayoutID, nil, nil, @Value,
          @ValueSize) = ERROR_SUCCESS then
          if Value = S then
          begin
            Result := CurentID;
            Exit;
          end;
      finally
        RegCloseKey(Key);
      end;
  end;{for i := 1 to $ff do}
end;

//------------------------------------------------------------------------------
// Returns a string value for the specified param of the specified locale.
// Date: 11/04/2007

function abfGetLocaleInfoStringA(const ALocID : LCID;
  const ALocType : LCTYPE): AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  Result := '';

  BufLen := GetLocaleInfoA(ALocID, ALocType, nil, 0);
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    if GetLocaleInfoA(ALocID, ALocType, Buf, BufLen) = 0 then Exit;

    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetLocaleInfoString(const ALocID : LCID;
  const ALocType : LCTYPE): string;
begin
  Result := abfGetLocaleInfoStringA(ALocID, ALocType);
end;

//------------------------------------------------------------------------------

function abfGetLocaleInfoStringW(const ALocID : LCID;
  const ALocType : LCTYPE): WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetLocaleInfoStringA(ALocID, ALocType);
    Exit;
  end;

  Result := '';

  BufLen := GetLocaleInfoW(ALocID, ALocType, nil, 0);
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    if GetLocaleInfoW(ALocID, ALocType, Buf, BufLen) = 0 then Exit;

    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Returns a list of supported keyboard layouts. Items of AList will contain
// names of layouts, Objects contain HKL values for each layout.
// Note: The AList will not be cleared before using.
// Date: 08/12/2000

procedure abfGetKeyboardLayoutList(AList: TStrings);
var
  Dummy: Pointer;
  Key: HKey;
  KeyName: array[0..63] of Char;
  Value: array[0..255] of Char;
  TotalKbLayout, ValueSize: Integer;

  //-------------------------------------

  procedure _FillTheList;
  var
    i, KbLayoutCount: Integer;
    KbListBuff: Pointer;
    CurrentKL: HKL;
    CurrentID: LongWord;
  begin
    GetMem(KbListBuff, TotalKbLayout * SizeOf(HKL));
    try
      KbLayoutCount := GetKeyboardLayoutList(TotalKbLayout, KbListBuff^);
      for i := 0 to KbLayoutCount - 1 do
      begin
        CurrentKL := HKL(Pointer(Integer(KbListBuff) + i * SizeOf(HKL))^);
        CurrentID := abfGetLayoutID(CurrentKL); // Translate KL to ID
        if RegOpenKeyEx(HKEY_LOCAL_MACHINE, StrFmt(KeyName,
          SReg_KeyboardLayoutsFmt, [CurrentID]), 0, KEY_READ,
          Key) = ERROR_SUCCESS then
        try
          ValueSize := SizeOf(Value);
          if RegQueryValueEx(Key, SReg_KeyboardLayoutText, nil, nil, @Value,
            @ValueSize) = ERROR_SUCCESS then
            AList.AddObject(Value, TObject(CurrentKL));
        finally
          RegCloseKey(Key);
        end;
      end;{for i := 0 to KbLayoutCount - 1 do}
    finally
      FreeMem(KbListBuff, TotalKbLayout * SizeOf(HKL));
    end;
  end;

  //-------------------------------------

begin
  if not Assigned(AList) then Exit;
  Dummy := nil;
  TotalKbLayout := GetKeyboardLayoutList(0, Dummy^);
  if TotalKbLayout > 0 then _FillTheList;
end;

//------------------------------------------------------------------------------
// Determines is CapsLock on or off.
// Date: 08/12/2000

function abfIsCapsLock: Boolean;
begin
  Result := ((GetKeyState(VK_CAPITAL) and 1) = 1);
end;

//------------------------------------------------------------------------------
// Determines is NumLock on or off.
// Date: 08/12/2000

function abfIsNumLock: Boolean;
begin
  Result := ((GetKeyState(VK_NUMLOCK) and 1) = 1);
end;

//------------------------------------------------------------------------------
// Determines is ScrollLock on or off.
// Date: 08/12/2000

function abfIsScrollLock: Boolean;
begin
  Result := ((GetKeyState(VK_SCROLL) and 1) = 1);
end;

 //------------------------------------------------------------------------------
// Determines is screen saver running or not.
// Date: 10/12/2000

function abfIsScreenSaverRunning: Boolean;
var
  Flag: BOOL;
  Dummy: DWORD;
begin
  Dummy := 0;
  SystemParametersInfo(SPI_GETSCREENSAVERRUNNING, Dummy, @Flag, 0);
  Result := Flag;
end;

//------------------------------------------------------------------------------

procedure _SetPort(Address: Word; Value: Byte); register;
asm
  xchg eax,edx
  out dx,al
end;

//------------------------------------------------------------------------------

function _GetPort(Address: Word): Byte; register;
asm
  mov edx,eax
  in  al,dx
end;

//------------------------------------------------------------------------------
// Outputs sound thru the Speaker under Win9x.

procedure abfSound(Frequency: Cardinal);
var
  B: Word;
begin
  if Frequency > 18 then
  begin
    Frequency := Word(1193181 div LongInt(Frequency));
    B := _GetPort($61);
    if (B and 3) = 0 then
    begin
      _SetPort($61, B or 3);
      _SetPort($43, $B6);
    end;
    _SetPort($42, Lo(Frequency));
    _SetPort($42, Hi(Frequency));
  end;
end;

//------------------------------------------------------------------------------
// Turn off the Speaker sound under Win9x.

procedure abfNoSound;
begin
  _SetPort($61, _GetPort($61) and $fc);
end;

//------------------------------------------------------------------------------
// Outputs sound thru the Speaker under any windows version.

procedure abfSpeakerBeep(Frequency, Duration: Cardinal);
begin
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS: begin
      abfSound(Frequency);
      Sleep(Duration);
      abfNoSound;
    end;
    VER_PLATFORM_WIN32_NT: Windows.Beep(Frequency, Duration)
  end;
end;

//------------------------------------------------------------------------------
// Retrieves information about the current system. If the function is called
// from a 64-bit application or called on Windows 2000 and lower,
// it is equivalent to the GetSystemInfo function.
// Date: 06/27/2013
var
  _GetNativeSystemInfo: procedure(var lpSystemInformation: TSystemInfo); stdcall = nil;

procedure abfGetNativeSystemInfo(var lpSystemInformation: TSystemInfo); stdcall;
begin
  if Assigned(@_GetNativeSystemInfo) then
    _GetNativeSystemInfo(lpSystemInformation)
  else
    GetSystemInfo(lpSystemInformation);
end;

//------------------------------------------------------------------------------
// Returns version of shell32.dll (Based on JEDI stuff)
// Date: 05/27/2001

var
  _ShellVersion: Integer = 0;

function abfGetShellVersion: Integer;
const
  SShellFileName = 'shell32.dll';
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  if _ShellVersion = 0 then
  begin
    InfoSize := GetFileVersionInfoSize(SShellFileName, Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(SShellFileName, Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
            _ShellVersion := FI.dwFileVersionMS;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;
  Result := _ShellVersion;
end;

//==============================================================================
// Conversion utilities
//==============================================================================

//------------------------------------------------------------------------------
// Performs a comparison of two TabfCustomVersion variables.

function abfCompareVersion(AValue1, AValue2: TabfCustomVersion): Integer;
begin
  if not (Assigned(AValue1) and Assigned(AValue2)) then
    Result := 0
  else if not Assigned(AValue1) then
    Result := -1
  else if not Assigned(AValue2) then
    Result := 1
  else
    begin
      if AValue1.Major < AValue2.Major then
        Result := -1
      else if AValue1.Major > AValue2.Major then
        Result := 1
      else
        begin
          if AValue1.Minor < AValue2.Minor then
            Result := -1
          else if AValue1.Minor > AValue2.Minor then
            Result := 1
          else
            begin
              if AValue1.Release < AValue2.Release then
                Result := -1
              else if AValue1.Release > AValue2.Release then
                Result := 1
              else
                begin
                  if AValue1.Build = AValue2.Build then
                    Result := 0
                  else if AValue1.Build < AValue2.Build then
                    Result := -1
                  else 
                    Result := 1;
                end;
            end;
        end;
    end;
end;

//------------------------------------------------------------------------------
// Converts a byte value to the binary string (01010111)

function abfByteToBin(Value: Byte): string;
const
  cBit: array[Boolean] of Char = ('0', '1');
begin
  Result := cBit[Value and 128 = 128] +
    cBit[Value and 64 = 64] +
    cBit[Value and 32 = 32] +
    cBit[Value and 16 = 16] +
    cBit[Value and 8 = 8] +
    cBit[Value and 4 = 4] +
    cBit[Value and 2 = 2] +
    cBit[Value and 1 = 1];
end;

//------------------------------------------------------------------------------
// Converts an integer value to the binary string (01101001). The Digits
// parameter specifies lengts of resulting string.

function abfIntToBin(Value, Digits: Integer): string;
var
  i, j: integer;
  Bytes: array[1..4] of Byte;
begin
  Move(Value, Bytes, SizeOf(Bytes));
  Result := abfByteToBin(Bytes[4]) + abfByteToBin(Bytes[3]) +
    abfByteToBin(Bytes[2]) + abfByteToBin(Bytes[1]);

// Remove leading zeroz
  i := Pos('1', Result);
  if i < 1 then
    Result := '0'
  else
    Delete(Result, 1, i - 1);

// Add leading zeros
  if (Digits > 0) then
  begin
    j := Length(Result) - Digits;
    for i := j + 1 to 0 do
      Result := '0' + Result;
  end;
end;

//------------------------------------------------------------------------------
// Converts a binary string (01101001) to the integer value.

function abfBinToInt(const Value: string): Integer;
var
  i, j: Integer;
begin
  Result := 0;
  j := Length(Value) + 1;
  if j <= 1 then Exit;

  i := 1;
  repeat
   dec(j);
   if Value[j] = '1' then Result := Result + i else
   if Value[j] <> '0' then
     raise EConvertError.CreateFmt('%s is not valid Binary value', [Value]);
   i := i shl 1;
  until j <= 1;
end;

//------------------------------------------------------------------------------
// Converts an integer value to the octal string (1277).

function abfIntToOct(Value, Digits: Integer): string;
var
  i, j, p: Integer;
begin
  Result := '';

  i := Abs(Value);
  j := 0;
  while True do
  begin
    p := Round(Power(8, j));
    if p > i then Break;
    Result := IntToStr((i div p) mod 8) + Result;
    Inc(j);
  end;

// Add leading zeros
  if (Digits > 0) then
  begin
    j := Length(Result) - Digits;
    for i := j + 1 to 0 do
      Result := '0' + Result;
  end;
end;

//------------------------------------------------------------------------------
// Converts an octal string (1277) to the integer value.

function abfOctToInt(const Value: string): Integer;
var
  i, Len: Integer;
begin
  Result := 0;
  Len := Length(Value);
  if Len < 1 then Exit;

  for i := 0 to Len - 1 do
  begin
    if not (Value[Len - i] in ['0'..'7']) then
      raise EConvertError.CreateFmt('%s is not valid Octal value', [Value]);
    Result := Result + StrToInt(Value[Len - i]) * Round(Power(8, i));
  end;
end;

//------------------------------------------------------------------------------
// Converts an integer value to the Roman string (XIV).

function abfIntToRoman(Value: Integer): string;
label
  _500, _400, _100, _90, _50, _40, _10, _9, _5, _4, _1, _End;
var
  Negative: Boolean;
begin
  Result := '';
  Negative := Value < 0;
  if Negative then Value := - Value;

// Count thousands
  while Value >= 1000 do
  begin
    Dec(Value, 1000);
    Result := Result + 'M';
  end;

  if Value < 900 then goto _500 else
  begin
    Dec(Value, 900);
    Result := Result + 'CM';
  end;
  goto _90;

_400:
  if Value < 400 then goto _100 else
  begin
    Dec(Value, 400);
    Result := Result + 'CD';
  end;
  goto _90;

_500:
  if Value < 500 then goto _400 else
  begin
    Dec(Value, 500);
    Result := Result + 'D';
  end;

_100:
// Count hundreds
  while Value >= 100 do
  begin
    Dec(Value, 100);
    Result := Result + 'C';
  end;

_90:
  if Value < 90 then goto _50 else
  begin
    Dec(Value, 90);
    Result := Result + 'XC';
  end;
  goto _9;

_40:
  if Value < 40 then goto _10 else
  begin
    Dec(Value, 40);
    Result := Result + 'XL';
  end;
  goto _9;

_50:
  if Value < 50 then goto _40 else
  begin
    Dec(Value, 50);
    Result := Result + 'L';
  end;

_10:
// Count tens
  while Value >= 10 do
  begin
    Dec(Value, 10);
    Result := Result + 'X';
  end;

_9:
  if Value < 9 then goto _5 else Result := Result + 'IX';
  goto _End;

_4:
  if Value < 4 then goto _1 else Result := Result + 'IV';
  goto _End;

_5:
  if Value < 5 then goto _4 else
  begin
    Dec(Value, 5);
    Result := Result + 'V';
  end;
  goto _1;

_1:
  while Value >= 1 do begin
    Dec(Value);
    Result := Result + 'I';
  end;

_End:

  if Negative then Result := '-' + Result;

end;{function abfIntToRoman}

//------------------------------------------------------------------------------
// Converts a Roman string (XIV) to the integer value.

function abfRomanToInt(const Value: string): Integer;
var
  S: string;
  i, N, PN: Integer;
  Negative: Boolean;
begin
  Result := 0;
  S := AnsiUpperCase(Value);
  S := abfRemoveCharSet([' '], S);
  Negative := (Length(S) > 0) and (S[1] = '-');
  if Negative then Delete(S, 1, 1);

  N := 10000;
  for i := 1 to Length(S) do
  begin
    PN := N;
    case S[i] of
      'C': N := 100;
      'D': N := 500;
      'I': N := 1;
      'L': N := 50;
      'M': N := 1000;
      'V': N := 5;
      'X': N := 10;
    else
      raise EConvertError.CreateFmt('%s is not valid Roman value', [Value]);
    end;
    if N <= PN then Inc(Result, N) else Inc(Result, N - (PN shl 1));
  end;

  if Negative then Result := -Result;
end;

//------------------------------------------------------------------------------
// Converts a boolean value to a string.
// Date: 10/16/2000

function abfBooleanToStr(B: Boolean): string;
begin
  Result := CabfBooleanValues[B];
end;

//------------------------------------------------------------------------------
// Converts a string value to a boolean.
// Date: 10/16/2000

function abfStrToBoolean(const S: string): Boolean;
begin
  Result := (CompareText(S, SabfTrue) = 0);
end;

//------------------------------------------------------------------------------
// Converts a TabfCustomVersion value to the string (x.x.x.x) value.
// Date: 06/14/2014

function abfVersionToStr(AVersion: TabfCustomVersion): string;
begin
  Result := '';
  if not (AVersion is TabfCustomVersion) then Exit;

  with AVersion do
    Result := Format('%u.%u.%u.%u', [Major, Minor, Release, Build]);
end;

//------------------------------------------------------------------------------
// Converts a string version value (x.x.x.x) to a TabfCustomVersion value.
// Date: 06/14/2014

type
  THackVersion = class(TabfCustomVersion);

function abfStrToVersion(const AVersionString: string;
  AVersion: TabfCustomVersion): Boolean;
var
  S: string;
  i, p: Integer;
  Arr: array[0..3] of Word;
begin
  Result := False;
  if not (AVersion is TabfCustomVersion) then Exit;

  try
    THackVersion(AVersion).Clear;

    S := AVersionString;
    FillChar(Arr, SizeOf(Arr), 0);

    for i := 0 to 3 do
    begin
      p := Pos('.', S);
      if p <= 0 then
      begin
        Arr[i] := StrToInt(S);
        Break;
      end;
      Arr[i] := StrToInt(Copy(S, 1, p - 1));
      Delete(S, 1, p);
    end;

    THackVersion(AVersion).SetVersion(Arr[0], Arr[1], Arr[2], Arr[3]);

    Result := True;
  except
  end;
end;

//------------------------------------------------------------------------------
// Converts TPoint to the string (X and Y coord separated with ',' ) value
// Date: 01/10/2001

function abfPointToStr(const P: TPoint): string;
begin
  Result := IntToStr(P.X) + ',' + IntToStr(P.Y);
end;

//------------------------------------------------------------------------------
// Converts string (X and Y coord separated with ',' ) to the TPoint value
// Date: 01/10/2001

function abfStrToPoint(const S: string): TPoint;
var
  i: Integer;
begin
  i := Pos(',', S);
  if i < 1 then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;
  Result.X := StrToIntDef(Trim(Copy(S, 1, i - 1)), 0);
  Result.Y := StrToIntDef(Trim(Copy(S, i + 1, MaxInt)), 0);
end;

//------------------------------------------------------------------------------
// Converts TRect to the string (L,T,R,B) value
// Date: 04/20/2001

function abfRectToStr(const R: TRect): string;
begin
  Result := abfPointToStr(R.TopLeft) + ',' + abfPointToStr(R.BottomRight);
end;

//------------------------------------------------------------------------------
// Converts string (L,T,R,B) to the TRect value
// Date: 01/10/2001

function abfStrToRect(const S: string): TRect;
var
  i: Integer;
begin
  i := Pos(',', S);
  i := abfPosEx(',', S, i + 1);
  Result.TopLeft     := abfStrToPoint(Copy(S, 1, i - 1));
  Result.BottomRight := abfStrToPoint(Copy(S, i + 1, MaxInt));
end;

//------------------------------------------------------------------------------
// Converts string of specified format to the date value.

function abfStrToDate(S, Format: string): TDateTime;
var
  i: Integer;
  SaveFormat: string;
begin
// Fix separators
  abfReplaceChars(S, '/', {$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator);
  abfReplaceChars(S, '\', {$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator);
  abfReplaceChars(S, ' ', {$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator);
  abfReplaceChars(S, ':', {$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator);
  abfReplaceChars(S, '-', {$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator);

// Fix the case when there is no separator in format
  i := Length(Format);
  if (Pos({$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator, Format) < 1) and (Pos({$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator, S) < 1) then
    while i > 1 do
    begin
      if Format[i] <> Format[i - 1] then
      begin
        Insert({$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator, Format, i);
        Insert({$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator, S, i);
      end;
      Dec(i);
    end;

// Temporary change format and convert
  SaveFormat := {$IFDEF DFMT}FormatSettings.{$ENDIF}ShortDateFormat;
  try
    {$IFDEF DFMT}FormatSettings.{$ENDIF}ShortDateFormat := Format;
    Result := StrToDate(S);
  finally
    {$IFDEF DFMT}FormatSettings.{$ENDIF}ShortDateFormat := SaveFormat;
  end;
end;

//------------------------------------------------------------------------------
// Converts string of specified format to the time value.

function abfStrToTime(S, Format: string): TDateTime;
var
  i: Integer;
  SaveFormat: string;
begin
// Fix separators
  abfReplaceChars(S, '/', {$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator);
  abfReplaceChars(S, '\', {$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator);
  abfReplaceChars(S, ' ', {$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator);
  abfReplaceChars(S, ':', {$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator);
  abfReplaceChars(S, '-', {$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator);

// Fix the case when there is no separator in format
  i := Length(Format);
  if (Pos({$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator, Format) < 1) and (Pos({$IFDEF DFMT}FormatSettings.{$ENDIF}DateSeparator, S) < 1) then
    while i > 1 do
    begin
      if Format[i] <> Format[i - 1] then
      begin
        Insert({$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator, Format, i);
        Insert({$IFDEF DFMT}FormatSettings.{$ENDIF}TimeSeparator, S, i);
      end;
      Dec(i);
    end;

// Temporary change format and convert
  SaveFormat := {$IFDEF DFMT}FormatSettings.{$ENDIF}ShortTimeFormat;
  try
    {$IFDEF DFMT}FormatSettings.{$ENDIF}ShortTimeFormat := Format;
    Result := StrToTime(S);
  finally
    {$IFDEF DFMT}FormatSettings.{$ENDIF}ShortTimeFormat := SaveFormat;
  end;
end;

//------------------------------------------------------------------------------
// Converts the binary data to "FFDDAA00" hex string.

function abfBinaryToString(var Buffer; Size: Integer): AnsiString;
var
  i: Integer;
  BufArr: PByteArray;
  SByte: AnsiString;
begin
  Result := '';
  if Size <= 0 then Exit;

  BufArr := @Buffer;
  SetLength(Result, 2 * Size);

  for i := 0 to Size - 1 do
  begin
    SByte := IntToHex(BufArr[i], 2);
    Result[i * 2 + 1] := SByte[1];
    Result[i * 2 + 2] := SByte[2];
  end;
end;

//------------------------------------------------------------------------------
// Converts "FFDDAA00" hex string to the binary data. Result is a number of
// bytes are converted. If some error occurs result is less then 0. Any
// non-hexadecimal characters are removed from string before the conversion.

function abfStringToBinary(const S: AnsiString;
  var Buffer; Size: Integer): Integer;
const
  cHexChars: set of AnsiChar =
    ['0'..'9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'];
var
  i, Addr: Integer;
  Data: AnsiString;
begin
  Result := -1;
  if not Assigned(@Buffer) or (Size <= 0) then Exit;

  Addr := Integer(@Buffer);
  Data := abfLeaveCharsetA(cHexChars, S);

  Result := Min(Size, Length(Data) div 2);
  try
    for i := 0 to Result - 1 do
      PByte(Addr + i)^ := StrToInt('$' + Copy(Data, i * 2 + 1, 2));
  except
    Result := -1;
  end;
end;

//------------------------------------------------------------------------------
// Performs a binary comparison of two TBytes arrays.

function abfSameBytes(AValue1, AValue2: TBytes): Boolean;
var
  TempLen: Integer;
begin
  TempLen := Length(AValue1);
  Result := (TempLen = Length(AValue2))
    and (CompareMem(@AValue1[0], @AValue2[0], TempLen));
end;

//------------------------------------------------------------------------------
// Converts DWORD to TBytes array.

function abfDWORDToBytes(AValue: DWORD): TBytes;
begin
  SetLength(Result, SizeOf(AValue));
  Move(AValue, Result[0], SizeOf(AValue));
end;

//------------------------------------------------------------------------------
// Converts TBytes array to DWORD.

function abfBytesToDWORD(const AValue: TBytes): DWORD;
begin
  Result := 0;
  if Length(AValue) <> SizeOf(Result) then
    raise EConvertError.Create('Value is not a valid DWORD');

  Move(AValue[0], Result, SizeOf(Result));
end;

//------------------------------------------------------------------------------
// Creates a globally unique identifier.

function abfCreateGUID: TGUID;
begin
  CreateGUID(Result);
end;

//------------------------------------------------------------------------------
// Converts TGUID to TBytes array.

function abfGUIDToBytes(AValue: TGUID): TBytes;
begin
  SetLength(Result, SizeOf(AValue));
  Move(AValue, Result[0], SizeOf(AValue));
end;

//------------------------------------------------------------------------------
// Converts TBytes array to TGUID.

function abfBytesToGUID(const AValue: TBytes): TGUID;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(AValue) <> SizeOf(Result) then
    raise EConvertError.Create('Value is not a valid TGUID');

  Move(AValue[0], Result, SizeOf(Result));
end;

//------------------------------------------------------------------------------
// Converts "ffddaa00" hex string to TBytes array.

function abfHexToBytes(const AHex: AnsiString): TBytes;
var
  i, n: Integer;
begin
  n := Length(AHex) div 2;
  SetLength(Result, n);
  for i := 0 to n - 1 do
  begin
    //!!HexToBin(@AHex[(i * 2) + 1], @Result[i], 1);
  end;
end;

//------------------------------------------------------------------------------
// Converts TBytes array to "ffddaa00" hex string.

function abfBytesToHex(const ABytes: TBytes): AnsiString;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Length(ABytes) - 1 do
    Result := Result + IntToHex(ABytes[i], 2);

  Result := LowerCase(Result);
end;

//------------------------------------------------------------------------------
// Converts Int64 to TBytes array.

function abfInt64ToBytes(AValue: Int64): TBytes;
begin
  SetLength(Result, SizeOf(AValue));
  Move(AValue, Result[0], SizeOf(AValue));
end;

//------------------------------------------------------------------------------
// Converts TBytes array to Int64.

function abfBytesToInt64(const AValue: TBytes): Int64;
begin
  Result := 0;
  if Length(AValue) <> SizeOf(Result) then
    raise EConvertError.Create('Value is not a valid Int64');

  Move(AValue[0], Result, SizeOf(Result));
end;

//------------------------------------------------------------------------------
// Converts AnsiString to TBytes array.

function abfStrToBytes(const AStr: AnsiString;
  ANullTerminated: Boolean = True): TBytes;
var
  TempLen: Integer;
begin
  Result := nil;
  try
    TempLen := Length(AStr);

    if ANullTerminated then
      Inc(TempLen);

    SetLength(Result, TempLen);

    Move(AStr[1], Result[0], TempLen);
  except
    Result := nil;
  end;
end;

//------------------------------------------------------------------------------
// Converts TBytes array to AnsiString.

function abfBytesToStr(const ABytes: TBytes): AnsiString;
var
  TempLen: Integer;
begin
  Result := '';
  if not Assigned(ABytes) then Exit;
  try
    TempLen := Length(ABytes);
    if TempLen > 0 then
    begin
      SetLength(Result, TempLen);
      Move(ABytes[0], Result[1], TempLen);
      if Result[TempLen] = #0 then
        SetLength(Result, TempLen - 1);
    end else
      Result := '';
  except
    Result := '';
  end;
end;

//------------------------------------------------------------------------------
// Converts AnsiString to "ffddaa00" hex string.

function abfStrToHex(const AStr: AnsiString;
  ANullTerminated: Boolean): AnsiString;
begin
  Result := abfBytesToHex(abfStrToBytes(AStr, ANullTerminated));
end;

//------------------------------------------------------------------------------
// Converts "ffddaa00" hex string to AnsiString.

function abfHexToStr(const AHex: AnsiString): AnsiString;
begin
  Result := abfBytesToStr(abfHexToBytes(AHex));
end;

//------------------------------------------------------------------------------
// Creates a globally unique identifier and puts it into Intel processor
// byte order.

function abfCreateUID: AnsiString;
var
  i: Integer;
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := LowerCase(IntToHex(GUID.D1, 8));
  Result := Result + LowerCase(IntToHex(GUID.D2, 4));
  Result := Result + LowerCase(IntToHex(GUID.D3, 4));
  for i := 0 to 7 do
    Result := Result + LowerCase(IntToHex(GUID.D4[i], 2));
end;

//------------------------------------------------------------------------------
// Performs a string comparison of two UID strings.

function abfSameUID(const AValue1, AValue2: AnsiString): Boolean;
begin
  Result := abfIsValidUID(AValue1) and abfIsValidUID(AValue2)
    and SameText(AValue1, AValue2);
end;

//------------------------------------------------------------------------------
// Checks is AValue a valid UID string.

function abfIsValidUID(const AValue: AnsiString): Boolean;
var
  TempData: TBytes;
begin
  Result := False;
  try
    TempData := abfHexToBytes(AValue);

    Result := (Length(TempData) = 16)
      and SameText(AValue, abfBytesToHex(TempData));
  except
  end;
end;

//------------------------------------------------------------------------------
// Checks is AValues a valid array of UID string.

function abfIsValidUIDs(const AValues: AnsiString): Boolean;
var
  TempData: TBytes;
begin
  Result := False;
  try
    TempData := abfHexToBytes(AValues);

    Result := ((Length(TempData) mod 16) = 0)
      and SameText(AValues, abfBytesToHex(TempData));
  except
  end;
end;

//------------------------------------------------------------------------------
// Converts UID string to TBytes array.

function abfUIDToBytes(const AValue: AnsiString): TBytes;
var
  TempData: TBytes;
begin
  TempData := abfHexToBytes(AValue);

  if (Length(TempData) mod 16) <> 0 then
    raise EConvertError.Create('Value is not a valid UID');

  Result := TempData;
end;

//------------------------------------------------------------------------------
// Converts TBytes array to UID string.

function abfBytesToUID(const AValue: TBytes): AnsiString;
begin
  if (Length(AValue) mod 16) <> 0 then
    raise EConvertError.Create('Value is not a valid UID');

  Result := abfBytesToHex(AValue);
end;

//------------------------------------------------------------------------------
// Converts WideString to TBytes array.

function abfWStrToBytes(const AStr: WideString;
  ANullTerminated: Boolean = True): TBytes;
var
  TempLen: Integer;
begin
  Result := nil;
  try
    TempLen := Length(AStr);

    if ANullTerminated then
      Inc(TempLen);

    TempLen := TempLen * 2;
    SetLength(Result, TempLen);
    Move(AStr[1], Result[0], TempLen);
  except
    Result := nil;
  end;
end;

//------------------------------------------------------------------------------
// Converts TBytes array to WideString.

function abfBytesToWStr(const ABytes: TBytes): WideString;
var
  TempLen: Integer;
begin
  Result := '';
  if not Assigned(ABytes) then Exit;
  try
    TempLen := Length(ABytes) div 2;
    if TempLen > 0 then
    begin
      SetLength(Result, TempLen);
      Move(ABytes[0], Result[1], TempLen * 2);
      if Result[TempLen] = #0 then
        SetLength(Result, TempLen - 1);
    end else
      Result := '';
  except
    Result := '';
  end;
end;

//------------------------------------------------------------------------------
// Converts WideString to "ffddaa00" hex string.

function abfWStrToHex(const AStr: WideString;
  ANullTerminated: Boolean): AnsiString;
begin
  Result := abfBytesToHex(abfWStrToBytes(AStr, ANullTerminated));
end;

//------------------------------------------------------------------------------
// Converts "ffddaa00" hex string to WideString.

function abfHexToWStr(const AHex: AnsiString): WideString;
begin
  Result := abfBytesToWStr(abfHexToBytes(AHex));
end;

//------------------------------------------------------------------------------
// Converts Word to TBytes array.

function abfWordToBytes(AValue: Word): TBytes;
begin
  SetLength(Result, SizeOf(AValue));
  Move(AValue, Result[0], SizeOf(AValue));
end;

//------------------------------------------------------------------------------
// Converts TBytes array to Word.

function abfBytesToWord(const AValue: TBytes): Word;
begin
  Result := 0;
  if Length(AValue) <> SizeOf(Result) then
    raise EConvertError.Create('Value is not a valid Word');

  Move(AValue[0], Result, SizeOf(Result));
end;


//==============================================================================
// Memory, records, pointer utilites
//==============================================================================

//------------------------------------------------------------------------------
// Frees an object reference and replaces the reference with nil.

{$IfNDef D5}
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;
{$EndIf D5}

//------------------------------------------------------------------------------
// Releases memory allocated for a dynamic variable and assign nil.
// Date: 08/07/2008

procedure abfDispose(var P);
var
  TempP: Pointer;
begin
  TempP := Pointer(P);
  Pointer(P) := nil;
  if Assigned(TempP) then
    Dispose(TempP);
end;

//------------------------------------------------------------------------------
// Fills the record with zeros and set the size value into the first DWORD
// Date: 02/23/2000

procedure abfInitRecord(var Rec; Size: Integer);
begin
  FillChar(Rec, Size, 0);
  DWORD(Rec) := Size;
end;

//------------------------------------------------------------------------------
// Copies a TVarRec and its contents. If the content is referenced
// the value will be copied to a new location and the reference
// updated.
// Date: 08/01/2008

function abfCopyVarRec(const AItem: TVarRec): TVarRec;
var
  W: WideString;
begin
  // Copy entire TVarRec first
  Result := AItem;

  // Now handle special cases
  case AItem.VType of
    vtExtended:
      begin
        New(Result.VExtended);
        Result.VExtended^ := AItem.VExtended^;
      end;
    vtString:
      begin
        New(Result.VString);
        Result.VString^ := AItem.VString^;
      end;
    vtPChar:
      Result.VPChar := StrNew(AItem.VPChar);
    // there is no StrNew for PWideChar
    vtPWideChar:
      begin
        W := AItem.VPWideChar;
        GetMem(Result.VPWideChar, 
               (Length(W) + 1) * SizeOf(WideChar));
        Move(PWideChar(W)^, Result.VPWideChar^, 
             (Length(W) + 1) * SizeOf(WideChar));
      end;
    // a little trickier: casting to AnsiString will ensure
    // reference counting is done properly
    vtAnsiString:
      begin
        // nil out first, so no attempt to decrement
        // reference count
        Result.VAnsiString := nil;
        AnsiString(Result.VAnsiString) := AnsiString(AItem.VAnsiString);
      end;
    vtCurrency:
      begin
        New(Result.VCurrency);
        Result.VCurrency^ := AItem.VCurrency^;
      end;
    vtVariant:
      begin
        New(Result.VVariant);
        Result.VVariant^ := AItem.VVariant^;
      end;
    // casting ensures proper reference counting
    vtInterface:
      begin
        Result.VInterface := nil;
        IInterface(Result.VInterface) := IInterface(AItem.VInterface);
      end;
    // casting ensures a proper copy is created
    vtWideString:
      begin
        Result.VWideString := nil;
        WideString(Result.VWideString) := WideString(AItem.VWideString);
      end;
    vtInt64:
      begin
        New(Result.VInt64);
        Result.VInt64^ := AItem.VInt64^;
      end;
    // VPointer and VObject don't have proper copy semantics so it
    // is impossible to write generic code that copies the contents
  end;
end;

//------------------------------------------------------------------------------
// Creates a TabfConstArray out of the values given. Uses abfCopyVarRec
// to make copies of the original elements.
// Date: 08/01/2008

function abfCreateConstArray(const AElements: array of const): TabfConstArray;
var
  i: Integer;
begin
  SetLength(Result, Length(AElements));
  for i := Low(AElements) to High(AElements) do
    Result[i] := abfCopyVarRec(AElements[i]);
end;

//------------------------------------------------------------------------------
// TVarRecs created by abfCopyVarRec must be finalized with this function.
// You should not use it on other TVarRecs.
// Date: 08/01/2008

procedure abfFinalizeVarRec(var AItem: TVarRec);
begin
  case AItem.VType of
    vtExtended: Dispose(AItem.VExtended);
    vtString: Dispose(AItem.VString);
    vtPChar: StrDispose(AItem.VPChar);
    vtPWideChar: FreeMem(AItem.VPWideChar);
    vtAnsiString: AnsiString(AItem.VAnsiString) := '';
    vtCurrency: Dispose(AItem.VCurrency);
    vtVariant: Dispose(AItem.VVariant);
    vtInterface: IInterface(AItem.VInterface) := nil;
    vtWideString: WideString(AItem.VWideString) := '';
    vtInt64: Dispose(AItem.VInt64);
  end;
  AItem.VInteger := 0;
end;

//------------------------------------------------------------------------------
// A TabfConstArray contains TVarRecs that must be finalized. This function
// does that for all items in the array.
// Date: 08/01/2008

procedure abfFinalizeConstArray(var AArr: TabfConstArray);
var
  i: Integer;
begin
  if not Assigned(AArr) then Exit;
  
  for i := Low(AArr) to High(AArr) do
    abfFinalizeVarRec(AArr[I]);
  Finalize(AArr);
  AArr := nil;
end;


//==============================================================================
// File utilities
//==============================================================================

//------------------------------------------------------------------------------
// Retrieves fully-qualified path for the file that contains
// the specified module that the current process owns.
// Date: 11/02/2007

function abfGetModuleFileNameA(AModule: HMODULE): AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  Result := '';

  BufLen := MAX_PATH + 1;
  GetMem(Buf, BufLen  * SizeOf(Buf^));
  try
    if GetModuleFileNameA(AModule, Buf, BufLen) > 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetModuleFileName(AModule: HMODULE): string;
begin
  Result := abfGetModuleFileNameA(AModule);
end;

//------------------------------------------------------------------------------

function abfGetModuleFileNameW(AModule: HMODULE): WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetModuleFileNameA(AModule);
    Exit;
  end;

  Result := '';

  BufLen := MAX_PATH + 1;
  GetMem(Buf, BufLen  * SizeOf(Buf^));
  try
    if GetModuleFileNameW(AModule, Buf, BufLen) > 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Retrieves file system type for the specified drive
// Date: 07/28/2008

function _GetFileSystem(const AFileSystemName: WideString): TabfFileSystem;
begin
  Result := afsUnknown;
  if AFileSystemName = 'FAT12' then
    Result := afsFAT12
  else if AFileSystemName = 'FAT16' then
    Result := afsFAT16
  else if AFileSystemName = 'FAT32' then
    Result := afsFAT32
  else if AFileSystemName = 'NTFS' then
    Result := afsNTFS;
end;

//------------------------------------------------------------------------------

function abfGetDriveFileSystemA(ADrive: AnsiChar): TabfFileSystem;
var
  VolumeName, FileSystemName: array [0..MAX_PATH] of AnsiChar;
  VolumeSerialNo: DWORD;
  MaxComponentLength, FileSystemFlags: Cardinal;
begin
  Result := afsUnknown;

  if not GetVolumeInformationA(PAnsiChar(AnsiString(ADrive) + ':\'), VolumeName,
    MAX_PATH, @VolumeSerialNo, MaxComponentLength, FileSystemFlags,
    FileSystemName, MAX_PATH ) then
    Exit;

  Result := _GetFileSystem(FileSystemName);
end;

//------------------------------------------------------------------------------

function abfGetDriveFileSystem(ADrive: Char): TabfFileSystem;
begin
  Result := abfGetDriveFileSystem(ADrive);
end;

//------------------------------------------------------------------------------

function abfGetDriveFileSystemW( ADrive: WideChar ): TabfFileSystem;
var
  VolumeName, FileSystemName: array [ 0..MAX_PATH ] of WideChar;
  VolumeSerialNo: DWORD;
  MaxComponentLength, FileSystemFlags: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfGetDriveFileSystem(Char(ADrive));
    Exit;
  end;

  Result := afsUnknown;

  if not GetVolumeInformationW(PWideChar(WideString(ADrive) + ':\'), VolumeName,
    MAX_PATH, @VolumeSerialNo, MaxComponentLength, FileSystemFlags,
    FileSystemName, MAX_PATH ) then
    Exit;

  Result := _GetFileSystem(FileSystemName);
end;

//------------------------------------------------------------------------------
// Returns drive type for the specified root path name
// Date: 07/28/2008

function abfGetDriveTypeA(ARootPathName: AnsiString): Cardinal;
begin
  ARootPathName := Trim(ARootPathName);
  if (Length(ARootPathName) > 2) and (Copy(ARootPathName, 1, 2 ) = '\\') then
    Result := DRIVE_REMOTE
  else
    Result := GetDriveTypeA(PAnsiChar(ARootPathName));
end;

//------------------------------------------------------------------------------

function abfGetDriveType(ARootPathName: string): Cardinal;
begin
  Result := abfGetDriveTypeA(ARootPathName);
end;

//------------------------------------------------------------------------------

function abfGetDriveTypeW(ARootPathName: WideString): Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfGetDriveTypeA(ARootPathName);
    Exit;
  end;

  ARootPathName := Trim(ARootPathName);
  if (Length(ARootPathName) > 2) and (Copy(ARootPathName, 1, 2 ) = '\\') then
    Result := DRIVE_REMOTE
  else
    Result := GetDriveTypeW(PWideChar(ARootPathName));
end;

//------------------------------------------------------------------------------
// Gets a value indicating whether a drive is ready.
// Date: 07/28/2008

function abfIsDriveReadyA(ARootPathName: AnsiString): Boolean;
var
  OldErrorMode: Cardinal;
  DW1, DW2: DWORD;
  Len: Integer;
begin
  Result := False;
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    ARootPathName := abfExtractFileDriveA(ARootPathName);

    Len := Length(ARootPathName);
    if Len < 1 then Exit;

    if Len = 1 then ARootPathName := ARootPathName + ':\';

    if ARootPathName[Len] = ':' then
      ARootPathName := abfAddSlashA(ARootPathName);

    Result := GetVolumeInformationA(PAnsiChar(ARootPathName), nil, 0, nil,
      DW1, DW2, nil, 0);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------

function abfIsDriveReady(ARootPathName: string): Boolean;
begin
  Result := abfIsDriveReadyA(ARootPathName);
end;

//------------------------------------------------------------------------------

function abfIsDriveReadyW(ARootPathName: WideString): Boolean;
var
  OldErrorMode: Cardinal;
  DW1, DW2: DWORD;
  Len: Integer;  
begin
  if not IsWinNT then
  begin
    Result := abfIsDriveReadyA(ARootPathName);
    Exit;
  end;

  Result := False;
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    ARootPathName := abfExtractFileDriveW(ARootPathName);

    Len := Length(ARootPathName);
    if Len < 1 then Exit;

    if Len = 1 then ARootPathName := ARootPathName + ':\';

    if ARootPathName[Len] = ':' then
      ARootPathName := abfAddSlashA(ARootPathName);

    Result := GetVolumeInformationW(PWideChar(ARootPathName), nil, 0, nil,
      DW1, DW2, nil, 0);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------
// Retrieves information about the amount of space that is available on a disk volume.
// Date: 07/28/2008

function abfGetDiskFreeSpaceA(ARootPathName: AnsiString): Int64;
var
  lpTotalNumberOfBytes: Int64;
  OldErrorMode: Cardinal;
begin
  Result := 0;

  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    ARootPathName := abfAddSlashA(Trim(ARootPathName));

    GetDiskFreeSpaceExA(PAnsiChar(ARootPathName), Result, lpTotalNumberOfBytes, nil);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------

function abfGetDiskFreeSpace(ARootPathName: string): Int64;
begin
  Result := abfGetDiskFreeSpaceA(ARootPathName);
end;

//------------------------------------------------------------------------------

function abfGetDiskFreeSpaceW(ARootPathName: WideString): Int64;
var
  lpTotalNumberOfBytes: Int64;
  OldErrorMode: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfGetDiskFreeSpaceA(ARootPathName);
    Exit;
  end;

  Result := 0;

  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    ARootPathName := abfAddSlashW(Trim(ARootPathName));

    GetDiskFreeSpaceExW(PWideChar(ARootPathName), Result, lpTotalNumberOfBytes, nil);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------
// Retrieves information about maximum media size for the specified drive.
// Date: 07/28/2008

type
 _MEDIA_TYPE = (
    Unknown,                // Format is unknown
    F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
    F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
    F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
    F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
    F3_720_512,             // 3.5",  720KB,  512 bytes/sector
    F5_360_512,             // 5.25", 360KB,  512 bytes/sector
    F5_320_512,             // 5.25", 320KB,  512 bytes/sector
    F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
    F5_180_512,             // 5.25", 180KB,  512 bytes/sector
    F5_160_512,             // 5.25", 160KB,  512 bytes/sector
    RemovableMedia,         // Removable media other than floppy
    FixedMedia,             // Fixed hard disk media
    F3_120M_512,            // 3.5",  120M Floppy
    F3_640_512,             // 3.5" , 640KB,  512 bytes/sector
    F5_640_512,             // 5.25", 640KB,  512 bytes/sector
    F5_720_512,             // 5.25", 720KB,  512 bytes/sector
    F3_1Pt2_512,            // 3.5" , 1.2Mb,  512 bytes/sector
    F3_1Pt23_1024,          // 3.5" , 1.23Mb, 1024 bytes/sector
    F5_1Pt23_1024,          // 5.25", 1.23MB, 1024 bytes/sector
    F3_128Mb_512,           // 3.5",  MO 128Mb   512 bytes/sector
    F3_230Mb_512,           // 3.5",  MO 230Mb   512 bytes/sector
    F8_256_128,             // 8",    256KB,  128 bytes/sector
    F3_200Mb_512,           // 3.5",  200M Floppy (HiFD)
    F3_240M_512,            // 3.5",  240Mb Floppy (HiFD)
    F3_32M_512 );           // 3.5",  32Mb Floppy
  {$EXTERNALSYM _MEDIA_TYPE}
  MEDIA_TYPE = _MEDIA_TYPE;
  {$EXTERNALSYM MEDIA_TYPE}
  PMEDIA_TYPE = ^MEDIA_TYPE;
  {$EXTERNALSYM PMEDIA_TYPE}
  TMediaType = MEDIA_TYPE;
  PMediaType = PMEDIA_TYPE;

  PDOS_DPB = ^DOS_DPB;
  {$EXTERNALSYM PDOS_DPB}
  _DOS_DPB = record
    specialFunc: BYTE;                //
    devType: BYTE;                    //
    devAttr: WORD;                    //
    cCyl: WORD;                       // number of cylinders
    mediaType: BYTE;                  //
    cbSec: WORD;                      // Bytes per sector
    secPerClus: BYTE;                 // Sectors per cluster
    cSecRes: WORD;                    // Reserved sectors
    cFAT: BYTE;                       // FATs
    cDir: WORD;                       // Root Directory Entries
    cSec: WORD;                       // Total number of sectors in image
    bMedia: BYTE;                     // Media descriptor
    secPerFAT: WORD;                  // Sectors per FAT
    secPerTrack: WORD;                // Sectors per track
    cHead: WORD;                      // Heads
    cSecHidden: DWORD;                // Hidden sectors
    cTotalSectors: DWORD;             // Total sectors, if cbSec is zero
    reserved: array [ 0..6 ] of BYTE; //
  end;
  {$EXTERNALSYM _DOS_DPB}
  DOS_DPB = _DOS_DPB;
  {$EXTERNALSYM DOS_DPB}
  TDOSDPB = DOS_DPB;
  PDOSDPB = PDOS_DPB;

  PDISK_GEOMETRY = ^DISK_GEOMETRY;
  {$EXTERNALSYM PDISK_GEOMETRY}
  _DISK_GEOMETRY = record
    Cylinders: LARGE_INTEGER;
    MediaType: MEDIA_TYPE;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
  end;
  {$EXTERNALSYM _DISK_GEOMETRY}
  DISK_GEOMETRY = _DISK_GEOMETRY;
  {$EXTERNALSYM DISK_GEOMETRY}
  TDiskGeometry = DISK_GEOMETRY;
  PDiskGeometry = PDISK_GEOMETRY;

  PDIOC_REGISTERS = ^_DIOC_REGISTERS;
  _DIOC_REGISTERS = record
    reg_EBX: DWORD;
    reg_EDX: DWORD;
    reg_ECX: DWORD;
    reg_EAX: DWORD;
    reg_EDI: DWORD;
    reg_ESI: DWORD;
    reg_Flags: DWORD;
  end;
  {$EXTERNALSYM _DIOC_REGISTERS}
  DIOC_REGISTERS = _DIOC_REGISTERS;
  {$EXTERNALSYM DIOC_REGISTERS}
  TDIOCRegisters = _DIOC_REGISTERS;
  PDIOCRegisters = PDIOC_REGISTERS;


function abfGetDriveMaxMediaSizeA(ADrive: AnsiString): Int64;
const
  IOCTL_DISK_GET_MEDIA_TYPES    = $70C00;
  CARRY_FLAG                    = $00;
  VWIN32_DIOC_DOS_IOCTL         = $01;
  Kb                            = 1024;
  Mb                            = 1024 * Kb;
var
  hDevice, OutBuffSize: Cardinal;
  DOSDPB: TDOSDPB;
  Reg: TDIOCRegisters;
begin
  if IsWinNT then
  begin
    Result := abfGetDriveMaxMediaSizeW(ADrive);
    Exit;
  end;

  Result := 0;
  ADrive := AnsiUpperCase(Trim(ADrive));
  if ADrive = '' then Exit;
  if not (ADrive[1] in ['A'..'Z']) then Exit;

{$IFDEF D9}
  hDevice := CreateFileW(PChar( '\\.\VWIN32'), 0, 0, nil, 0,
    FILE_FLAG_DELETE_ON_CLOSE, 0 );
{$ELSE}
  hDevice := CreateFileA(PChar( '\\.\VWIN32'), 0, 0, nil, 0,
    FILE_FLAG_DELETE_ON_CLOSE, 0 );
{$ENDIF}

  if hDevice = INVALID_HANDLE_VALUE then Exit;
  try
    DOSDPB.specialFunc := 0;            // return default type; do not hit disk

    reg.reg_EBX   := Ord(ADrive[1]) - Ord('A') + 1; // BL = drive number (1-based)
    reg.reg_EDX   := DWORD( @DOSDPB );              // DS:EDX -> DPB
    reg.reg_ECX   := $0860;                         // CX = Get DPB
    reg.reg_EAX   := $440D;                         // AX = Ioctl
    reg.reg_Flags := CARRY_FLAG;                    // assume failure

    if not DeviceIoControl(
      hDevice,
      VWIN32_DIOC_DOS_IOCTL,
      @reg, SizeOf( reg ),
      @reg, SizeOf( reg ),
      OutBuffSize,
      nil
    ) then Exit;

    case DOSDPB.devType of
      0: Result := 360 * Kb; // 5.25 360K floppy
      1: Result := 1214720;  // 5.25 1.2MB floppy
      2: Result := 720 * Kb; // 3.5  720K floppy
      3: Result := 256 * Kb; // 8" low-density floppy
      4: Result := 360 * Kb; // 8" high-density floppy
      7: Result := 1457664;  // 3.5  1.44MB floppy
      9: Result := 2915328;  // 3.5  2.88MB floppy
    end;
  finally
    CloseHandle( hDevice );
  end;
end;

//------------------------------------------------------------------------------

function abfGetDriveMaxMediaSize(ADrive: string): Int64;
begin
  Result := abfGetDriveMaxMediaSizeA(ADrive);
end;

//------------------------------------------------------------------------------

function abfGetDriveMaxMediaSizeW(ADrive: WideString): Int64;
const
  IOCTL_DISK_GET_MEDIA_TYPES    = $70C00;
  CARRY_FLAG                    = $00;
  VWIN32_DIOC_DOS_IOCTL         = $01;
  Kb                            = 1024;
  Mb                            = 1024 * Kb;
var
  hDevice, OutBuffSize: Cardinal;
  i: Integer;
  DiskGeometry: array [ 0..20 ] of TDiskGeometry;
  tmpResult: Int64;
begin
  if not IsWinNT then
  begin
    Result := abfGetDriveMaxMediaSizeA(ADrive);
    Exit;
  end;

  Result := 0;
  ADrive := Trim(ADrive);
  if ADrive = '' then Exit;

  hDevice := CreateFileW(PWideChar( '\\.\' + WideUpperCase(ADrive)),
    0, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE then Exit;
  try
    if not DeviceIoControl(
             hDevice,
             IOCTL_DISK_GET_MEDIA_TYPES,
             nil, 0,
             @DiskGeometry, SizeOf(DiskGeometry),
             OutBuffSize,
             nil
           ) or (OutBuffSize = 0) then Exit;

    for i := 0 to (OutBuffSize div SizeOf(DISK_GEOMETRY)) - 1 do begin
      case DiskGeometry[i].MediaType of
        F5_1Pt2_512  : tmpResult := 1214720; // 1.2 * Mb;
        F3_1Pt44_512 : tmpResult := 1457664; // 1.44 * Mb;
        F3_2Pt88_512 : tmpResult := 2915328; // 2.88 * Mb;
        F3_20Pt8_512 : tmpResult := 20 * Mb; // 20.8 * Mb;
        F3_720_512   : tmpResult := 720 * Kb;
        F5_360_512   : tmpResult := 360 * Kb;
        F5_320_512   : tmpResult := 320 * Kb;
        F5_320_1024  : tmpResult := 320 * Kb;
        F5_180_512   : tmpResult := 180 * Kb;
        F5_160_512   : tmpResult := 160 * Kb;
        F3_120M_512  : tmpResult := 120 * Mb;
        F3_640_512   : tmpResult := 640 * Kb;
        F5_640_512   : tmpResult := 640 * Kb;
        F5_720_512   : tmpResult := 720 * Kb;
        F3_1Pt2_512  : tmpResult := 1214720; // 1.2 * Mb;
        F3_1Pt23_1024: tmpResult := 1245088; // 1.23 * Mb;
        F5_1Pt23_1024: tmpResult := 1245088; // 1.23 * Mb;
        F3_128Mb_512 : tmpResult := 128 * Mb;
        F3_230Mb_512 : tmpResult := 230 * Mb;
        F8_256_128   : tmpResult := 256 * Kb;
        F3_200Mb_512 : tmpResult := 200 * Mb;
        F3_240M_512  : tmpResult := 240 * Mb;
        F3_32M_512   : tmpResult := 32 * Mb;
      else
        tmpResult := -1;
      end;
      if tmpResult > Result then Result := tmpResult;
    end;
  finally
    CloseHandle(hDevice);
  end;
end;


//==============================================================================
// Name routines
//==============================================================================

//------------------------------------------------------------------------------
// Checks is FileName a valid file name string.
// Date: 11/09/2000
// Unicode version added 06/13/2007

function abfIsValidFileNameA(const FileName: AnsiString): Boolean;
var
  Handle: THandle;
begin
  Result := False;
  if FileName = '' then Exit;

  if not abfFileExistsA(FileName) then
  begin
    Handle := Integer(CreateFileA(PAnsiChar(FileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
    if Handle = INVALID_HANDLE_VALUE then Exit;

    FileClose(Handle);
    DeleteFile(PAnsiChar(FileName));
  end;
  Result := True;
end;

//------------------------------------------------------------------------------

function abfIsValidFileName(const FileName: string): Boolean;
begin
  Result := abfIsValidFileNameA(FileName);
end;

//------------------------------------------------------------------------------

function abfIsValidFileNameW(const FileName: WideString): Boolean;
var
  Handle: THandle;
begin
  if not IsWinNT then
  begin
    Result := abfIsValidFileNameA(FileName);
    Exit;
  end;

  Result := False;
  if FileName = '' then Exit;

  if not abfFileExistsA(FileName) then
  begin
    Handle := Integer(CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
    if Handle = INVALID_HANDLE_VALUE then Exit;

    FileClose(Handle);
    DeleteFileW(PWideChar(FileName));
  end;
  Result := True;
end;

//------------------------------------------------------------------------------
// Adds a slash to the end of the Path string if it is not present
// Date: 02/23/2000
// Unicode version added 06/12/2007

function abfAddSlashA(const Path: AnsiString): AnsiString;
begin
  Result := abfAddCharA(Path, AnsiChar(SabfSlash));
end;

//------------------------------------------------------------------------------

function abfAddSlash(const Path: string): string;
begin
  Result := abfAddSlashA(Path);
end;

//------------------------------------------------------------------------------

function abfAddSlashW(const Path: WideString): WideString;
begin
  Result := abfAddCharW(Path, WideChar(SabfSlash));
end;

//------------------------------------------------------------------------------
// Adds a slash to the begin of the Path string if it is not present.
// Date: 02/23/2000

function abfAddSlashBeforeA(const Path: AnsiString): AnsiString;
begin
  Result := abfAddCharBeforeA(Path, SabfSlash);
end;

//------------------------------------------------------------------------------

function abfAddSlashBefore(const Path: string): string;
begin
  Result := abfAddSlashBeforeA(Path);
end;

//------------------------------------------------------------------------------

function abfAddSlashBeforeW(const Path: WideString): WideString;
begin
  Result := abfAddCharBeforeW(Path, SabfSlash);
end;

//------------------------------------------------------------------------------
// Removes a slash from the end of the Path string
// Date: 02/23/2000
// Unicode version added 06/12/2007

function abfRemoveSlashA(const Path: AnsiString): AnsiString;
var
  Len: Integer;
begin
  Result := Path;
  repeat
    Len := Length(Result);
    if (Len > 0) and (Result[Len] = SabfSlash) then
      Delete(Result, Len, 1)
    else
      Break;
  until False;
end;

//------------------------------------------------------------------------------

function abfRemoveSlash(const Path: string): string;
begin
  Result := abfRemoveSlashA(Path);
end;

//------------------------------------------------------------------------------

function abfRemoveSlashW(const Path: WideString): WideString;
var
  Len: Integer;
begin
  Result := Path;
  repeat
    Len := Length(Result);
    if (Len > 0) and (Result[Len] = SabfSlash) then
      Delete(Result, Len, 1)
    else
      Break;
  until False;
end;

//------------------------------------------------------------------------------
// Replaces all doubled slashes with one slash.
// Date: 02/23/2000
// Unicode version added 06/12/2007

function abfRemoveExtraSlashA(const FileName: AnsiString): AnsiString;
begin
  Result := FileName;
  abfReplaceSubStringsA(Result, SabfSlash + SabfSlash, SabfSlash);
end;

//------------------------------------------------------------------------------

function abfRemoveExtraSlash(const FileName: string): string;
begin
  Result := abfRemoveExtraSlashA(FileName);
end;

//------------------------------------------------------------------------------

function abfRemoveExtraSlashW(const FileName: WideString): WideString;
begin
  Result := FileName;
  abfReplaceSubStringsW(Result, SabfSlash + SabfSlash, SabfSlash);
end;

//------------------------------------------------------------------------------
// Returns the drive portion of a file name.
// Date: 03/11/2008

function abfExtractFileDriveA(const FileName: AnsiString): AnsiString;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = PathDelim) and
    (FileName[2] = PathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = PathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = PathDelim then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;

//------------------------------------------------------------------------------

function abfExtractFileDrive(const FileName: string): string;
begin
  Result := abfExtractFileDriveA(FileName);
end;

//------------------------------------------------------------------------------

function abfExtractFileDriveW(const FileName: WideString): WideString;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = PathDelim) and
    (FileName[2] = PathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = PathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = PathDelim then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;

//------------------------------------------------------------------------------
// Returns the drive and directory portions of a file name. Same to
// ExtractFilePath but supports '/'.
// Date: 09/15/2000
// Unicode version added 06/12/2007

function abfExtractFilePathA(const FileName: AnsiString): AnsiString;
var
  i: Integer;
begin
  i := LastDelimiter('\:/', FileName);
  Result := Copy(FileName, 1, i);
end;

//------------------------------------------------------------------------------

function abfExtractFilePath(const FileName: string): string;
begin
  Result := abfExtractFilePathA(FileName);
end;

//------------------------------------------------------------------------------

function abfExtractFilePathW(const FileName: WideString): WideString;
var
  i: Integer;
begin
  i := LastDelimiter('\:/', FileName);
  Result := Copy(FileName, 1, i);
end;

//------------------------------------------------------------------------------
// Extracts the name and extension parts of a file name. Same to ExtractFileName
// but supports '/'.
// Date: 09/15/2000
// Unicode version added 06/12/2007

function abfExtractFileNameA(const FileName: AnsiString): AnsiString;
var
  i: Integer;
begin
  i := LastDelimiter('\:/', FileName);
  Result := Copy(FileName, i + 1, MaxInt);
end;

//------------------------------------------------------------------------------

function abfExtractFileName(const FileName: string): string;
begin
  Result := abfExtractFileNameA(FileName);
end;

//------------------------------------------------------------------------------

function abfExtractFileNameW(const FileName: WideString): WideString;
var
  i: Integer;
begin
  i := LastDelimiter('\:/', FileName);
  Result := Copy(FileName, i + 1, MaxInt);
end;

//------------------------------------------------------------------------------
// Returns the name of the file without extension.
// Date: 02/23/2000
// Unicode version added 06/25/2007

function abfExtractFileNameOnlyA(const FileName: AnsiString): AnsiString;
begin
  Result := abfChangeFileExtA(abfExtractFileNameA(FileName), '');
end;

//------------------------------------------------------------------------------

function abfExtractFileNameOnly(const FileName: string): string;
begin
  Result := abfExtractFileNameOnlyA(FileName);
end;

//------------------------------------------------------------------------------

function abfExtractFileNameOnlyW(const FileName: WideString): WideString;
begin
  Result := abfChangeFileExtW(abfExtractFileNameW(FileName), '');
end;

//------------------------------------------------------------------------------
// Changes the extension of a file name. Same to ChangeFileExt
// but supports '/'.
// Date: 06/13/2007

function abfChangeFileExtA(const FileName, Extension: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := LastDelimiter('.\:/', Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

//------------------------------------------------------------------------------

function abfChangeFileExt(const FileName, Extension: string): string;
begin
  Result := abfChangeFileExtA(FileName, Extension);
end;

//------------------------------------------------------------------------------

function abfChangeFileExtW(const FileName, Extension: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('.\:/', Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

//------------------------------------------------------------------------------
// Extracts the extension part of a file name. Same to ExtractFileExt
// but supports '/'.
// Date: 06/12/2007

function abfExtractFileExtA(const FileName: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := LastDelimiter('.\:/', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

//------------------------------------------------------------------------------

function abfExtractFileExt(const FileName: string): string;
begin
  Result := abfExtractFileExtA(FileName);
end;

//------------------------------------------------------------------------------

function abfExtractFileExtW(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiter('.\:/', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

//------------------------------------------------------------------------------
// Returns the name given from the FileName and matched the Mask. Supports only
// simple masks 'C:\AA\*.pas', '?.*', etc. Doesn't support masks such a 'p*.c*'
// Example:
//   FileName = 'D:\Copy\autoexec.bat'
//   Mask = 'C:\*.bak'
//   Result = 'C:\autoexec.bak' }
// Date: 04/01/2000
// Unicode version added 07/02/2007

function abfGetFileNameMatchedMaskA(const FileName, Mask: AnsiString): AnsiString;
var
  MaskPath, MaskName, MaskExt: AnsiString;
begin
  if Trim(Mask) = '' then
  begin
    Result := FileName;
    Exit;
  end;
  MaskPath := abfExtractFilePathA(Mask);
  MaskName := abfExtractFileNameOnlyA(Mask);
  MaskExt  := abfExtractFileExtA(Mask);
// Determine file path
  if MaskPath <> '' then Result := MaskPath
  else Result := abfExtractFilePathA(FileName);
// Determine file name
  if (MaskName <> '') and (Pos('*', MaskName) = 0) and
    (Pos('?', MaskName) = 0) then Result := Result + MaskName
  else Result := Result + abfExtractFileNameOnlyA(FileName);
// Determine file extension
  if (MaskExt <> '') and (Pos('*', MaskExt) = 0) and
    (Pos('?', MaskExt) = 0) then Result := Result + MaskExt
  else Result := Result + abfExtractFileExtA(FileName);
end;

//------------------------------------------------------------------------------

function abfGetFileNameMatchedMask(const FileName, Mask: string): string;
begin
  Result := abfGetFileNameMatchedMaskA(FileName, Mask);
end;

//------------------------------------------------------------------------------

function abfGetFileNameMatchedMaskW(const FileName, Mask: WideString): WideString;
var
  MaskPath, MaskName, MaskExt: WideString;
begin
  if Trim(Mask) = '' then
  begin
    Result := FileName;
    Exit;
  end;
  MaskPath := abfExtractFilePathW(Mask);
  MaskName := abfExtractFileNameOnlyW(Mask);
  MaskExt  := abfExtractFileExtW(Mask);
// Determine file path
  if MaskPath <> '' then Result := MaskPath
  else Result := abfExtractFilePathW(FileName);
// Determine file name
  if (MaskName <> '') and (Pos('*', MaskName) = 0) and
    (Pos('?', MaskName) = 0) then Result := Result + MaskName
  else Result := Result + abfExtractFileNameOnlyW(FileName);
// Determine file extension
  if (MaskExt <> '') and (Pos('*', MaskExt) = 0) and
    (Pos('?', MaskExt) = 0) then Result := Result + MaskExt
  else Result := Result + abfExtractFileExtW(FileName);
end;

//------------------------------------------------------------------------------
// Packs given path string to fit the MaxLength.
// Example: "C:\ABF\...\some.txt"
// Date: 09/21/2000
// Unicode version added 07/02/2007

function abfPackPathStringA(const Path: AnsiString;
  MaxLength: Integer): AnsiString;

  //-------------------------------------

  procedure _FixTheEnd;
  begin
    if Length(Result) > MaxLength then
    begin
      Delete(Result, MaxLength - Length(SabfPathPack), MaxInt);
      Result := Result + SabfPathPack;
    end;
  end;{Internal procedure _FixTheEnd}

  //-------------------------------------

var
  i, Len, Limit, PrevSlash, LastSlash: Integer;
  EndPart: AnsiString;
begin
	Result := Path;
  Len := Length(Result);
  if Len <= MaxLength then Exit;
// Get EndPart of the path
  LastSlash := abfBackPosA(SabfSlash, Result);
  EndPart := Copy(Result, LastSlash, MaxInt);
// Search for last allowed slash from beginning
  PrevSlash := Pos(SabfSlash, Result);
  i := abfPosExA(SabfSlash, Result, PrevSlash + 1);
  Limit := MaxLength - (Len - LastSlash) - Length(SabfPathPack);
  while (i > 0) and (i < Limit) do
  begin
    PrevSlash := i;
    i := abfPosExA(SabfSlash, Result, i + 1);
  end;
// Path can not be packed
  if PrevSlash <= 0 then
  begin
   _FixTheEnd;
    Exit;
  end;
// Pack the path
  Delete(Result, PrevSlash + 1, MaxInt);
  Result := Result + SabfPathPack + EndPart;
  _FixTheEnd;
end;

//------------------------------------------------------------------------------

function abfPackPathString(const Path: string; MaxLength: Integer): string;
begin
	Result := abfPackPathStringA(Path, MaxLength);
end;

//------------------------------------------------------------------------------

function abfPackPathStringW(const Path: WideString;
  MaxLength: Integer): WideString;

  //-------------------------------------

  procedure _FixTheEnd;
  begin
    if Length(Result) > MaxLength then
    begin
      Delete(Result, MaxLength - Length(SabfPathPack), MaxInt);
      Result := Result + SabfPathPack;
    end;
  end;{Internal procedure _FixTheEnd}

  //-------------------------------------

var
  i, Len, Limit, PrevSlash, LastSlash: Integer;
  EndPart: WideString;
begin
	Result := Path;
  Len := Length(Result);
  if Len <= MaxLength then Exit;
// Get EndPart of the path
  LastSlash := abfBackPosW(SabfSlash, Result);
  EndPart := Copy(Result, LastSlash, MaxInt);
// Search for last allowed slash from beginning
  PrevSlash := Pos(SabfSlash, Result);
  i := abfPosExW(SabfSlash, Result, PrevSlash + 1);
  Limit := MaxLength - (Len - LastSlash) - Length(SabfPathPack);
  while (i > 0) and (i < Limit) do
  begin
    PrevSlash := i;
    i := abfPosExW(SabfSlash, Result, i + 1);
  end;
// Path can not be packed
  if PrevSlash <= 0 then
  begin
   _FixTheEnd;
    Exit;
  end;
// Pack the path
  Delete(Result, PrevSlash + 1, MaxInt);
  Result := Result + SabfPathPack + EndPart;
  _FixTheEnd;
end;

//------------------------------------------------------------------------------
// Retrieves the short path form of the specified path.
// Date: 08/22/2007

function abfGetShortPathNameA(const ALongPathName: AnsiString): AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  if IsWinNT then
  begin
    Result := abfGetShortPathNameW(ALongPathName);
    Exit;
  end;

  Result := ALongPathName;
  BufLen := MAX_PATH;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    FillChar(Buf^, BufLen * SizeOf(Buf^), 0);
    if GetShortPathNameA(PAnsiChar(ALongPathName), Buf, BufLen) > 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetShortPathName(const ALongPathName: string): string;
begin
  Result := abfGetShortPathNameA(ALongPathName);
end;

//------------------------------------------------------------------------------

function abfGetShortPathNameW(const ALongPathName: WideString): WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetShortPathNameA(ALongPathName);
    Exit;
  end;

  Result := ALongPathName;
  BufLen := 32767;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    FillChar(Buf^, BufLen * SizeOf(Buf^), 0);
    if GetShortPathNameW(PWideChar(ALongPathName), Buf, BufLen) > 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Retrieves the long path form of the specified path.
// Date: 08/22/2007

var
  _GetLongPathNameA: function (lpszShortPath: PAnsiChar; lpszLongPath: PAnsiChar;
      cchBuffer: DWORD): DWORD; stdcall = nil;

function abfGetLongPathNameA(const AShortPathName: AnsiString): AnsiString;
begin
  if IsWinNT then
  begin
    Result := abfGetLongPathNameW(AShortPathName);
    Exit;
  end;

  Result := AShortPathName;
  if not Assigned(@_GetLongPathNameA) then Exit;

  SetLength(Result, MAX_PATH + 1);
  SetLength(Result, _GetLongPathNameA(@Result[1], @Result[1], MAX_PATH));
end;

//------------------------------------------------------------------------------

function abfGetLongPathName(const AShortPathName: string): string;
begin
  Result := abfGetLongPathNameA(AShortPathName);
end;

//------------------------------------------------------------------------------

var
  _GetLongPathNameW: function (lpszShortPath: PWideChar; lpszLongPath: PWideChar;
      cchBuffer: DWORD): DWORD; stdcall = nil;

function abfGetLongPathNameW(const AShortPathName: WideString): WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetLongPathNameA(AShortPathName);
    Exit;
  end;

  Result := AShortPathName;
  if not Assigned(@_GetLongPathNameW) then Exit;

  BufLen := 32767;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    FillChar(Buf^, BufLen * SizeOf(Buf^), 0);
    if _GetLongPathNameW(PWideChar(AShortPathName), Buf, BufLen) > 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;


//------------------------------------------------------------------------------
// Retrieves the path of the system directory used by WOW64.
// This directory is not present on 32-bit Windows.
// Date: 10/06/2010

var
  _GetSystemWow64DirectoryA: function (lpBuffer: PAnsiChar;
    uSize: DWORD): DWORD; stdcall = nil;

function abfGetSystemWow64DirectoryA: AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  Result := '';
  if not Assigned(@_GetSystemWow64DirectoryA) then Exit;

  BufLen := 32767;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    FillChar(Buf^, BufLen * SizeOf(Buf^), 0);
    if _GetSystemWow64DirectoryA(Buf, BufLen) > 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetSystemWow64Directory: AnsiString;
begin
  Result := abfGetSystemWow64DirectoryA;
end;

//------------------------------------------------------------------------------

var
  _GetSystemWow64DirectoryW: function (lpBuffer: PWideChar;
    uSize: DWORD): DWORD; stdcall = nil;

function abfGetSystemWow64DirectoryW: WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  Result := '';
  if not Assigned(@_GetSystemWow64DirectoryW) then Exit;

  BufLen := 32767;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    FillChar(Buf^, BufLen * SizeOf(Buf^), 0);
    if _GetSystemWow64DirectoryW(Buf, BufLen) > 0 then
      Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;


//------------------------------------------------------------------------------
// Determines whether the specified process is running under WOW64.
// Date: 10/06/2010

var
  _IsWow64Process: function (hProcess: THandle;
    Wow64Process: PBOOL): BOOL; stdcall = nil;

function abfIsWow64Process(AProcessHandle: THandle): Boolean;
var
  TempBool: BOOL;
begin
  Result := False;
  if not Assigned(@_IsWow64Process) then Exit;

  TempBool := False;
  if _IsWow64Process(AProcessHandle, @TempBool) and TempBool then
    Result := True;
end;


//------------------------------------------------------------------------------
// Determines whether the current process is running under WOW64.
// Date: 10/06/2010

function abfIsWow64: Boolean;
begin
  Result := abfIsWow64Process(GetCurrentProcess);
end;


//------------------------------------------------------------------------------
// Determines whether a file is an executable (.exe) file, and if so, 
// which subsystem runs the executable file.
// Date: 06/12/2013

function abfGetBinaryTypeA(const AAppName: AnsiString;
  var ABinType: DWORD): Boolean;
begin
  Result := False;
  if not OSVersion.Check(5, 1) then Exit;

  Result := GetBinaryTypeA(PAnsiChar(AAppName), ABinType);
end;

//------------------------------------------------------------------------------

function abfGetBinaryType(const AAppName: string;
  var ABinType: DWORD): Boolean;
begin
  Result := abfGetBinaryTypeA(AAppName, ABinType);
end;

//------------------------------------------------------------------------------

function abfGetBinaryTypeW(const AAppName: WideString;
  var ABinType: DWORD): Boolean;
begin
  Result := False;
  if not OSVersion.Check(5, 1) then Exit;

  Result := GetBinaryTypeW(PWideChar(AAppName), ABinType);
end;


//==============================================================================
// Command line routines.
//==============================================================================

function _GetNextParamStrA(P: PAnsiChar; var Param: AnsiString): PAnsiChar;
var
  i, Len: Integer;
  Start, S, Q: PAnsiChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      P := CharNextA(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNextA(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNextA(P);
        Inc(Len, Q - P);
        P := Q;
      end;
      if P[0] <> #0 then
        P := CharNextA(P);
    end
    else
    begin
      Q := CharNextA(P);
      Inc(Len, Q - P);
      P := Q;
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNextA(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNextA(P);
        while P < Q do
        begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then P := CharNextA(P);
    end
    else
    begin
      Q := CharNextA(P);
      while P < Q do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result := P;
end;

//------------------------------------------------------------------------------

function _GetNextParamStrW(P: PWideChar; var Param: WideString): PWideChar;
var
  i, Len: Integer;
  Start, S, Q: PWideChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      P := CharNextW(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNextW(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNextW(P);
        Inc(Len, Q - P);
        P := Q;
      end;
      if P[0] <> #0 then
        P := CharNextW(P);
    end
    else
    begin
      Q := CharNextW(P);
      Inc(Len, Q - P);
      P := Q;
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNextW(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNextW(P);
        while P < Q do
        begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then P := CharNextW(P);
    end
    else
    begin
      Q := CharNextW(P);
      while P < Q do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result := P;
end;

//------------------------------------------------------------------------------
// Parses command line parameters to string list.
// Leading '/' and '-' are removed.
// Date: 03/10/2001
// Unicode version added 07/02/2007

procedure abfCommandLineToStringsA(const AList: TabfAnsiStrings);
begin
  abfParseCommandLineA(abfGetCommandLineA, AList);
end;

//------------------------------------------------------------------------------

procedure abfCommandLineToStrings(const AList: TStrings);
begin
  abfCommandLineToStringsA(AList);
end;

//------------------------------------------------------------------------------

procedure abfCommandLineToStringsW(const AList: TabfWideStrings);
begin
  abfParseCommandLineW(abfGetCommandLineW, AList);
end;

//------------------------------------------------------------------------------
// Returns the command line string for current process.
// Date: 12/07/2007

function abfGetCommandLineA: AnsiString;
begin
  Result := GetCommandLineA;
end;

//------------------------------------------------------------------------------

function abfGetCommandLine: string;
begin
  Result := abfGetCommandLineA;
end;

//------------------------------------------------------------------------------

function abfGetCommandLineW: WideString;
begin
  if not IsWinNT then
  begin
    Result := abfGetCommandLineA;
    Exit;
  end;

  Result := GetCommandLineW;
end;

//------------------------------------------------------------------------------
// Returns a specified parameter from command line.
// Date: 06/12/2007
// Unicode version added 06/25/2007

function abfParamStrA(AIndex: Integer): AnsiString;
var
  P: PAnsiChar;
  Buffer: array[0..MAX_PATH] of AnsiChar;
begin
  Result := '';
  if AIndex = 0 then
  begin
    if GetModuleFileNameA(0, Buffer, SizeOf(Buffer)) > 0 then
      Result := PAnsiChar(@Buffer);
  end else
  begin
    P := GetCommandLineA;
    while True do
    begin
      P := _GetNextParamStrA(P, Result);
      if (AIndex = 0) or (Result = '') then Break;
      Dec(AIndex);
    end;
  end;
end;

//------------------------------------------------------------------------------

function abfParamStr(AIndex: Integer): string;
begin
  Result := abfParamStrA(AIndex);
end;

//------------------------------------------------------------------------------

function abfParamStrW(AIndex: Integer): WideString;
var
  P: PWideChar;
  Buffer: array[0..MAX_PATH] of WideChar;
begin
  if not IsWinNT then
  begin
    Result := abfParamStrA(AIndex);
    Exit;
  end;

  Result := '';
  if AIndex = 0 then
  begin
    if GetModuleFileNameW(0, Buffer, SizeOf(Buffer)) > 0 then
      Result := PWideChar(@Buffer);
  end else
  begin
    P := GetCommandLineW;
    while True do
    begin
      P := _GetNextParamStrW(P, Result);
      if (AIndex = 0) or (Result = '') then Break;
      Dec(AIndex);
    end;
  end;
end;

//------------------------------------------------------------------------------
// Parses command line parameters to string list.
// Leading '/' and '-' are removed.
// Date: 03/10/2001
// Unicode version added 07/02/2007

procedure abfParseCommandLineA(const ACommandLine: AnsiString;
  const AList: TabfAnsiStrings);
var
  PAC: PAnsiChar;
  S: AnsiString;
begin
  AList.BeginUpdate;
  try
    AList.Clear;

    PAC := PAnsiChar(ACommandLine);
    while True do
    begin
      PAC := _GetNextParamStrA(PAC, S);
      if S = '' then Break;

      if S[1] in [Char('-'), Char('/')] then Delete(S, 1, 1);

      AList.Add(S);
    end;
  finally
    AList.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure abfParseCommandLine(const ACommandLine: string;
  const AList: TStrings);
begin
  abfParseCommandLineA(ACommandLine, AList);
end;

//------------------------------------------------------------------------------

procedure abfParseCommandLineW(const ACommandLine: WideString;
  const AList: TabfWideStrings);
{$IFDEF D9}
var
  PWC: PWideChar;
  WS: WideString;
{$ENDIF}
begin
{$IFDEF D9}
  AList.BeginUpdate;
  try
    AList.Clear;

    PWC := PWideChar(ACommandLine);
    while True do
    begin
      PWC := _GetNextParamStrW(PWC, WS);
      if WS = '' then Break;

      if WS[1] in [WideChar('-'), WideChar('/')] then Delete(WS, 1, 1);

      AList.Add(WS);
    end;
  finally
    AList.EndUpdate;
  end;
{$ELSE}
  abfParseCommandLineA(ACommandLine, AList);
{$ENDIF}
end;


//==============================================================================
// Paths, directory managing.
//==============================================================================

//------------------------------------------------------------------------------
// Creates directory.
// Date: 07/02/2007

function abfCreateDirectoryA(const ADirName: AnsiString): Boolean;
var
  OldErrorMode: Cardinal;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := CreateDirectoryA(PAnsiChar(ADirName), nil);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------

function abfCreateDirectory(const ADirName: string): Boolean;
begin
  Result := abfCreateDirectoryA(ADirName);
end;

//------------------------------------------------------------------------------

function abfCreateDirectoryW(const ADirName: WideString): Boolean;
var
  OldErrorMode: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfCreateDirectoryA(ADirName);
    Exit;
  end;

  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := CreateDirectoryW(PWideChar(ADirName), nil);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------
// Adds a directory to the search path used to locate DLLs for the application.
// Date: 10/01/2016

var
  _SetDllDirectoryA: function (lpPathName: PAnsiChar): BOOL; stdcall = nil;

function abfSetDllDirectoryA(ADirName: PAnsiChar): Boolean;
begin
  Result := False;
  if not Assigned(@_SetDllDirectoryA) then Exit;

  Result := _SetDllDirectoryA(ADirName);
end;

//------------------------------------------------------------------------------

function abfSetDllDirectory(ADirName: PAnsiChar): Boolean;
begin
  Result := abfSetDllDirectoryA(ADirName);
end;

//------------------------------------------------------------------------------

var
  _SetDllDirectoryW: function (lpPathName: PWideChar): BOOL; stdcall = nil;

function abfSetDllDirectoryW(ADirName: PWideChar): Boolean;
begin
  Result := False;
  if not Assigned(@_SetDllDirectoryW) then Exit;

  Result := _SetDllDirectoryW(ADirName);
end;

//------------------------------------------------------------------------------
// Checks directory existing.
// Date: 12/28/2000
// Unicode version added 06/12/2007

function abfDirectoryExistsA(const ADirName: AnsiString): Boolean;
var
  Code: Integer;
begin
  Result := False;
  if ADirName = '' then Exit;

  Code := abfGetFileAttrA(abfRemoveSlashA(ADirName));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

//------------------------------------------------------------------------------

function abfDirectoryExists(const ADirName: string): Boolean;
begin
  Result := abfDirectoryExistsA(ADirName);
end;

//------------------------------------------------------------------------------

function abfDirectoryExistsW(const ADirName: WideString): Boolean;
var
  Code: Integer;
begin
  if not IsWinNT then
  begin
    Result := abfDirectoryExistsA(string(ADirName));
    Exit;
  end;

  Result := False;
  if ADirName = '' then Exit;

  Code := abfGetFileAttrW(abfRemoveSlashW(ADirName));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

//------------------------------------------------------------------------------
// Creates all the directories along a directory path if they do not already
// exist. You can specify an optional CreatedDirList param to receive the list
// of all created directory names, if you don't need this information set the
// CreatedDirList parameter to nil.
// Date: 11/28/2001
// Unicode version added 07/02/2007

procedure abfForceDirectoriesExA(ADirName: AnsiString;
  ACreatedDirList: TabfAnsiStrings);
var
  Path: AnsiString;
begin
  if ADirName = '' then Exit;
  ADirName := abfRemoveSlashA(ADirName);
  Path := abfExtractFilePathA(ADirName);
  if (Length(ADirName) < 3) or abfDirectoryExistsA(ADirName) or
    (Path = ADirName) then Exit;
  abfForceDirectoriesExA(Path, ACreatedDirList);
  if abfCreateDirectoryA(ADirName) then
    if Assigned(ACreatedDirList) then ACreatedDirList.Add(ADirName);
end;

//------------------------------------------------------------------------------

procedure abfForceDirectoriesEx(ADirName: string; ACreatedDirList: TStrings);
begin
  abfForceDirectoriesExA(ADirName, ACreatedDirList);
end;

//------------------------------------------------------------------------------


procedure abfForceDirectoriesExW(ADirName: WideString;
  ACreatedDirList: TabfWideStrings);
{$IFDEF D9}
var
  Path: WideString;
{$ENDIF}
begin
{$IFDEF D9}
  if ADirName = '' then Exit;
  ADirName := abfRemoveSlashW(ADirName);
  Path := abfExtractFilePathW(ADirName);
  if (Length(ADirName) < 3) or abfDirectoryExistsW(ADirName) or
    (Path = ADirName) then Exit;
  abfForceDirectoriesExW(Path, ACreatedDirList);
  if abfCreateDirectoryW(ADirName) then
    if Assigned(ACreatedDirList) then ACreatedDirList.Add(ADirName);
{$ELSE}
  abfForceDirectoriesExA(ADirName, ACreatedDirList);
{$ENDIF}
end;


//------------------------------------------------------------------------------
// Creates all the directories along a directory path if they do not already
// exist.
// Date: 06/09/2001
// Unicode version added 07/02/2007

procedure abfForceDirectoriesA(ADirName: AnsiString);
begin
  abfForceDirectoriesExA(ADirName, nil);
end;

//------------------------------------------------------------------------------

procedure abfForceDirectories(ADirName: string);
begin
  abfForceDirectoriesA(ADirName);
end;

//------------------------------------------------------------------------------

procedure abfForceDirectoriesW(ADirName: WideString);
begin
  abfForceDirectoriesExW(ADirName, nil);
end;

//------------------------------------------------------------------------------
// Retrieves the path of the current directory.
// Date: 04/10/2007
// Unicode version added 06/12/2007

function abfGetCurrentDirectoryA: AnsiString;
var
  Buf: array[0..MAX_PATH] of AnsiChar;
  CharCount: Integer;
begin
  CharCount := GetCurrentDirectoryA(Length(Buf), @Buf);
  if (CharCount > 0) and (CharCount <= Length(Buf)) then
    Result := Buf
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function abfGetCurrentDirectory: string;
begin
  Result := abfGetCurrentDirectoryA;
end;

//------------------------------------------------------------------------------

function abfGetCurrentDirectoryW: WideString;
var
  Buf: array[0..MAX_PATH] of WideChar;
  CharCount: Integer;
begin
  if not IsWinNT then
  begin
    Result := abfGetCurrentDirectoryA;

    Exit;
  end;

  CharCount := GetCurrentDirectoryW(Length(Buf), @Buf);
  if (CharCount > 0) and (CharCount <= Length(Buf)) then
    Result := Buf
  else
    Result := '';
end;

//------------------------------------------------------------------------------
// Change the path of the current directory.
// Date: 04/10/2007
// Unicode version added 06/12/2007

function abfSetCurrentDirectoryA(const APath: AnsiString): Boolean;
begin
  Result := SetCurrentDirectoryA(PAnsiChar(APath));
end;

//------------------------------------------------------------------------------

function abfSetCurrentDirectory(const APath: string): Boolean;
begin
  Result := abfSetCurrentDirectoryA(APath);
end;

//------------------------------------------------------------------------------

function abfSetCurrentDirectoryW(const APath: WideString): Boolean;
begin
  if not IsWinNT then
  begin
    Result := abfSetCurrentDirectoryA(APath);
    Exit;
  end;

  Result := SetCurrentDirectoryW(PWideChar(APath));
end;

//------------------------------------------------------------------------------
// Retrieves the path of the Windows directory.
// Date: 09/01/2000
// Unicode version added 06/12/2007

function abfGetWindowsDirA: AnsiString;
var
  Buffer: array[0..MAX_PATH] of AnsiChar;
begin
  GetWindowsDirectoryA(Buffer, SizeOf(Buffer));
  Result := AnsiString(Buffer);
end;

//------------------------------------------------------------------------------

function abfGetWindowsDir: string;
begin
  Result := abfGetWindowsDirA;
end;

//------------------------------------------------------------------------------

function abfGetWindowsDirW: WideString;
var
  Buffer: array[0..MAX_PATH] of WideChar;
begin
  if not IsWinNT then
  begin
    Result := abfGetWindowsDirA;
    Exit;
  end;

  GetWindowsDirectoryW(Buffer, SizeOf(Buffer));
  Result := WideString(Buffer);
end;

//------------------------------------------------------------------------------
// Retrieves the path of the Windows system directory.
// Date: 09/01/2000
// Unicode version added 06/12/2007

function abfGetSystemDirA: AnsiString;
var
  Buffer: array[0..MAX_PATH] of AnsiChar;
begin
  GetSystemDirectoryA(Buffer, SizeOf(Buffer));
  Result := AnsiString(Buffer);
end;

//------------------------------------------------------------------------------

function abfGetSystemDir: string;
begin
  Result := abfGetSystemDirA;
end;

//------------------------------------------------------------------------------

function abfGetSystemDirW: WideString;
var
  Buffer: array[0..MAX_PATH] of WideChar;
begin
  if not IsWinNT then
  begin
    Result := abfGetSystemDirA;
    Exit;
  end;

  GetSystemDirectoryW(Buffer, SizeOf(Buffer));
  Result := WideString(Buffer);
end;

//------------------------------------------------------------------------------
// Retrieves the path of the TEMP directory.
// Date: 10/13/2000
// Unicode version added 06/12/2007

function abfGetTempDirA: AnsiString;
var
  Buf: PAnsiChar;
  BufLen: DWORD;
begin
  BufLen := MAX_PATH;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    BufLen := GetTempPathA(BufLen, Buf);

    if BufLen > MAX_PATH then
    begin
      FreeMem(Buf);
      GetMem(Buf, BufLen * SizeOf(Buf^));
      GetTempPathA(BufLen, Buf);
    end;

    Result := AnsiString(Buf);
  finally
    FreeMem(Buf)
  end;
end;

//------------------------------------------------------------------------------

function abfGetTempDir: string;
begin
  Result := abfGetTempDirA;
end;

//------------------------------------------------------------------------------

function abfGetTempDirW: WideString;
var
  Buf: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetTempDirA;
    Exit;
  end;

  BufLen := MAX_PATH;
  GetMem(Buf, BufLen * SizeOf(Buf^));
  try
    BufLen := GetTempPathW(BufLen, Buf);

    if BufLen > MAX_PATH then
    begin
      FreeMem(Buf);
      GetMem(Buf, BufLen * SizeOf(Buf^));
      GetTempPathW(BufLen, Buf);
    end;

    Result := WideString(Buf);
  finally
    FreeMem(Buf)
  end;
end;

//------------------------------------------------------------------------------
// Returns an unique name for newly created temporary directory.
// Date: 03/04/2008

function abfGetUniqueTempDirA(const APrefix: AnsiString): AnsiString;
begin
  repeat
    Result := abfGetLongPathNameA(abfGetUniqueTempFileNameA(APrefix));
    abfDeleteFileA(Result);
    Result := abfChangeFileExtA(Result, '');
  until not abfDirectoryExistsA(Result);

  abfForceDirectoriesA(Result);

  if not abfDirectoryExistsA(Result) then
    Result := '';
end;

//------------------------------------------------------------------------------

function abfGetUniqueTempDir(const APrefix: string): string;
begin
  Result := abfGetUniqueTempDirA(APrefix);
end;

//------------------------------------------------------------------------------

function abfGetUniqueTempDirW(const APrefix: WideString): WideString;
begin
  if not IsWinNT then
  begin
    Result := abfGetUniqueTempDirA(APrefix);
    Exit;  
  end;

  repeat
    Result := abfGetLongPathNameW(abfGetUniqueTempFileNameW(APrefix));
    abfDeleteFileW(Result);
    Result := abfChangeFileExtW(Result, '');
  until not abfDirectoryExistsW(Result);

  abfForceDirectoriesW(Result);

  if not abfDirectoryExistsW(Result) then
    Result := '';
end;

//------------------------------------------------------------------------------
// Returns an unique name for newly created temporary file.
// Date: 10/13/2000
// Unicode version added 03/10/2007

function abfGetUniqueTempFileNameA(const APrefix: AnsiString): AnsiString;
var
  Buffer: array[0..MAX_PATH + 2] of AnsiChar;
begin
  GetTempFileNameA(PAnsiChar(abfGetTempDirA), PAnsiChar(APrefix), 0, Buffer);
  Result := AnsiString(Buffer);
end;

//------------------------------------------------------------------------------

function abfGetUniqueTempFileName(const APrefix: string): string;
begin
  Result := abfGetUniqueTempFileNameA(APrefix);
end;

//------------------------------------------------------------------------------

function abfGetUniqueTempFileNameW(const APrefix: WideString): WideString;
var
  Buffer: array[0..MAX_PATH + 2] of WideChar;
begin
  if not IsWinNT then
  begin
    Result := abfGetUniqueTempFileNameA(APrefix);
    Exit;
  end;

  GetTempFileNameW(PWideChar(abfGetTempDirW), PWideChar(APrefix), 0, Buffer);
  Result := WideString(Buffer);
end;

//------------------------------------------------------------------------------
// Removes directory.
// Date: 07/29/2008

function abfRemoveDirectoryA(const ADirName: AnsiString;
  AWithContent: Boolean = True): Boolean;
var
  OldErrorMode: Cardinal;
  FindData: TWin32FindDataA;
  SearchHandle: THandle;
  s: AnsiString;
begin
  Result := False;
  try
    OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      if AWithContent then
      begin
        FindData.dwFileAttributes := faAnyFile;
        SearchHandle := FindFirstFileA(PAnsiChar(abfAddSlashA(ADirName) + '*.*'),
          FindData);

        if SearchHandle <> INVALID_HANDLE_VALUE then
        try
          Result := True;
          repeat
            s := FindData.cFileName;

            if ( ( FindData.dwFileAttributes and faDirectory ) = faDirectory ) then
            begin
              if ( s = '' ) or ( s = '.' ) or ( s = '..' ) then Continue;
              Result := abfRemoveDirectoryA(abfAddSlashA(abfAddSlashA(ADirName) + s),
                AWithContent);
            end
            else
              Result := DeleteFileA(PAnsiChar(abfAddSlashA(ADirName) + s));

          until not (FindNextFileA( SearchHandle, FindData ) and Result);
        finally
          Windows.FindClose(SearchHandle);
        end;
      end;

      Result := RemoveDirectoryA(PAnsiChar(ADirName));
    finally
      if GetLastError = ERROR_NO_MORE_FILES then SetLastError(0);
      SetErrorMode(OldErrorMode);
    end;
  except
  end;
end;

//------------------------------------------------------------------------------

function abfRemoveDirectory(const ADirName: string;
  AWithContent: Boolean = True): Boolean;
begin
  Result := abfRemoveDirectoryA(ADirName, AWithContent);
end;

//------------------------------------------------------------------------------

function abfRemoveDirectoryW(const ADirName: WideString;
  AWithContent: Boolean = True): Boolean;
var
  OldErrorMode: Cardinal;
  FindData: TWin32FindDataW;
  SearchHandle: THandle;
  s: WideString;
begin
  if not IsWinNT then
  begin
    Result := abfRemoveDirectoryA(ADirName, AWithContent);
    Exit;
  end;

  Result := False;
  try
    OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      if AWithContent then
      begin
        FindData.dwFileAttributes := faAnyFile;
        SearchHandle := FindFirstFileW(PWideChar(abfAddSlashW(ADirName) + '*.*'),
          FindData);

        if SearchHandle <> INVALID_HANDLE_VALUE then
        try
          Result := True;
          repeat
            s := FindData.cFileName;

            if ( ( FindData.dwFileAttributes and faDirectory ) = faDirectory ) then
            begin
              if ( s = '' ) or ( s = '.' ) or ( s = '..' ) then Continue;
              Result := abfRemoveDirectoryW(abfAddSlashW(abfAddSlashW(ADirName) + s),
                AWithContent);
            end
            else
              Result := DeleteFileW(PWideChar(abfAddSlashW(ADirName) + s));

          until not (FindNextFileW( SearchHandle, FindData ) and Result);
        finally
          Windows.FindClose(SearchHandle);
        end;
      end;

      Result := RemoveDirectoryW(PWideChar(ADirName));
    finally
      if GetLastError = ERROR_NO_MORE_FILES then SetLastError(0);
      SetErrorMode(OldErrorMode);
    end;
  except
  end;
end;

//==============================================================================
// Relative names
//==============================================================================

//------------------------------------------------------------------------------
// Concatenates specified RelativePath relative with the given BasePath.
// Date: 04/01/2000
// Unicode version added 07/09/2007

function abfConcatRelativePathA(const BasePath,
  RelativePath: AnsiString): AnsiString;
begin
  Result := RelativePath;
  if Length(abfExtractFileDriveA(Result)) > 0 then Exit;
  Result := abfRemoveExtraSlashA(abfAddSlashA(BasePath) + Result);
end;

//------------------------------------------------------------------------------

function abfConcatRelativePath(const BasePath, RelativePath: string): string;
begin
  Result := abfConcatRelativePathA(BasePath, RelativePath);
end;

//------------------------------------------------------------------------------

function abfConcatRelativePathW(const BasePath,
  RelativePath: WideString): WideString;
begin
  Result := RelativePath;
  if Length(abfExtractFileDriveW(Result)) > 0 then Exit;
  Result := abfRemoveExtraSlashW(abfAddSlashW(BasePath) + Result);
end;

//------------------------------------------------------------------------------
// Returns the full path name for a relative file name.
// Date: 07/24/2007

function abfExpandFileNameA(const AFileName: AnsiString): AnsiString;
var
  Buf, P: PAnsiChar;
  BufLen: DWORD;
begin
  BufLen := 4096;
  GetMem(Buf, (BufLen + 1) * SizeOf(Buf^));
  try
    FillChar(Buf^, (BufLen + 1) * SizeOf(Buf^), 0);
    GetFullPathNameA(PAnsiChar(AFileName), BufLen, Buf, P);

    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfExpandFileName(const AFileName: string): string;
begin
  Result := abfExpandFileNameA(AFileName);
end;

//------------------------------------------------------------------------------

function abfExpandFileNameW(const AFileName: WideString): WideString;
var
  Buf, P: PWideChar;
  BufLen: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfExpandFileNameA(AFileName);
    Exit;
  end;

  BufLen := 4096;
  GetMem(Buf, (BufLen + 1) * SizeOf(Buf^));
  try
    FillChar(Buf^, (BufLen + 1) * SizeOf(Buf^), 0);
    GetFullPathNameW(PWideChar(AFileName), BufLen, Buf, P);

    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------
// Expands relative path from BasePath.
// Date: 04/01/2000
// Unicode version added 07/09/2007

function abfExpandRelativePathExA(const BasePath,
  RelativePath: AnsiString): AnsiString;
var
  SaveDir: AnsiString;
begin
  if RelativePath = '' then
  begin
    if BasePath <> '' then Result := abfAddSlashA(BasePath);
    Exit;
  end;
  Result := '';
  SaveDir := abfGetCurrentDirectoryA;
  try
    abfSetCurrentDirectoryA(BasePath);
    Result := abfExpandFileNameA(RelativePath);
//    Result := ExpandUNCFileName(RelativePath);
  finally
    abfSetCurrentDirectoryA(SaveDir);
  end;
end;

//------------------------------------------------------------------------------

function abfExpandRelativePathEx(const BasePath, RelativePath: string): string;
begin
  Result := abfExpandRelativePathExA(BasePath, RelativePath);
end;

//------------------------------------------------------------------------------

function abfExpandRelativePathExW(const BasePath,
  RelativePath: WideString): WideString;
var
  SaveDir: WideString;
begin
  if RelativePath = '' then
  begin
    if BasePath <> '' then Result := abfAddSlashW(BasePath);
    Exit;
  end;
  Result := '';
  SaveDir := abfGetCurrentDirectoryW;
  try
    abfSetCurrentDirectoryW(BasePath);
    Result := abfExpandFileNameW(RelativePath);
//    Result := ExpandUNCFileName(RelativePath);
  finally
    abfSetCurrentDirectoryA(SaveDir);
  end;
end;

//------------------------------------------------------------------------------
// Expands relative path from the starting directory of the application.
// Date: 04/01/2000
// Unicode version added 07/09/2007

function abfExpandRelativePathA(const RelativePath: AnsiString): AnsiString;
begin
  Result := abfExpandRelativePathExA(SAppPath, RelativePath);
end;

//------------------------------------------------------------------------------

function abfExpandRelativePath(const RelativePath: string): string;
begin
  Result := abfExpandRelativePathA(RelativePath);
end;

//------------------------------------------------------------------------------

function abfExpandRelativePathW(const RelativePath: WideString): WideString;
begin
  Result := abfExpandRelativePathExW(SAppPath, RelativePath);
end;

//------------------------------------------------------------------------------
// Expands relative path from the curent directory, if path doesn't exists,
// expands it from the starting directory of the application
// Date: 04/01/2000
// Unicode version added 07/09/2007

function abfSmartExpandRelativePathA(const RelativePath: AnsiString): AnsiString;
begin
  Result := abfExpandRelativePathExA(abfGetCurrentDirectoryA, RelativePath);
  if abfDirectoryExistsA(Result) then Exit;
  Result := abfExpandRelativePathA(RelativePath);
end;

//------------------------------------------------------------------------------

function abfSmartExpandRelativePath(const RelativePath: string): string;
begin
  Result := abfSmartExpandRelativePathA(RelativePath);
end;

//------------------------------------------------------------------------------

function abfSmartExpandRelativePathW(const RelativePath: WideString): WideString;
begin
  Result := abfExpandRelativePathExW(abfGetCurrentDirectoryW, RelativePath);
  if abfDirectoryExistsW(Result) then Exit;
  Result := abfExpandRelativePathW(RelativePath);
end;

//------------------------------------------------------------------------------
// Expands relative file name from BasePath.
// Date: 04/01/2000
// Unicode version added 07/09/2007

function abfExpandRelativeFileNameExA(const BasePath,
  RelativeFileName: AnsiString): ansistring;
var
  TempFileName: AnsiString;
begin
  TempFileName := abfExtractFileNameA(RelativeFileName);
  Result := abfExpandRelativePathExA(BasePath,
    abfExtractFilePathA(RelativeFileName)) + TempFileName;
end;

//------------------------------------------------------------------------------

function abfExpandRelativeFileNameEx(const BasePath,
  RelativeFileName: string): string;
begin
  Result := abfExpandRelativeFileNameExA(BasePath, RelativeFileName);
end;

//------------------------------------------------------------------------------

function abfExpandRelativeFileNameExW(const BasePath,
  RelativeFileName: WideString): WideString;
var
  TempFileName: WideString;
begin
  TempFileName := abfExtractFileNameW(RelativeFileName);
  Result := abfExpandRelativePathExW(BasePath,
    abfExtractFilePathW(RelativeFileName)) + TempFileName;
end;

//------------------------------------------------------------------------------
// Expands relative file name from the starting directory of the application
// Date: 04/01/2000
// Unicode version added 07/09/2007

function abfExpandRelativeFileNameA(const RelativeFileName: AnsiString): AnsiString;
begin
  Result := abfExpandRelativeFileNameExA(SAppPath, RelativeFileName);
end;

//------------------------------------------------------------------------------

function abfExpandRelativeFileName(const RelativeFileName: string): string;
begin
  Result := abfExpandRelativeFileNameA(RelativeFileName);
end;

//------------------------------------------------------------------------------

function abfExpandRelativeFileNameW(const RelativeFileName: WideString): WideString;
begin
  Result := abfExpandRelativeFileNameExW(SAppPath, RelativeFileName);
end;

//------------------------------------------------------------------------------
// Expands relative file name from the curent directory, if path doesn't exists,
// expands file name from the starting directory of the application.
// Date: 04/01/2000
// Unicode version added 07/09/2007

function abfSmartExpandRelativeFileNameA(const RelativeFileName: AnsiString): AnsiString;
begin
  Result := abfExpandRelativeFileNameExA(abfGetCurrentDirectoryA,
    RelativeFileName);
  if abfFileExistsA(Result) then Exit;
  Result := abfExpandRelativeFileNameA(RelativeFileName);
end;

//------------------------------------------------------------------------------

function abfSmartExpandRelativeFileName(const RelativeFileName: string): string;
begin
  Result := abfSmartExpandRelativeFileNameA(RelativeFileName);
end;

//------------------------------------------------------------------------------

function abfSmartExpandRelativeFileNameW(const RelativeFileName: WideString): WideString;
begin
  Result := abfExpandRelativeFileNameExW(abfGetCurrentDirectoryW,
    RelativeFileName);
  if abfFileExistsW(Result) then Exit;
  Result := abfExpandRelativeFileNameW(RelativeFileName);
end;

//==============================================================================
// File managing.
//==============================================================================

//------------------------------------------------------------------------------
// Checks file existing.
// Unicode version added 03/10/2007

function abfFileExistsA(FileName: AnsiString): Boolean;
var
  Code: Integer;
begin
  Code := abfGetFileAttrA(FileName);
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code = 0);
end;

//------------------------------------------------------------------------------

function abfFileExists(FileName: string): Boolean;
begin
  Result := abfFileExistsA(FileName);
end;

//------------------------------------------------------------------------------

function abfFileExistsW(FileName: WideString): Boolean;
var
  Code: Integer;
begin
  if not IsWinNT then
  begin
    Result := abfFileExistsA(string(FileName));
    Exit;
  end;

  Code := abfGetFileAttrW(FileName);
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code = 0);
end;

//------------------------------------------------------------------------------
// Use this function to get a size of the file specified by the FileName
// parameter. If function returns True the FileSize parameter contains the size
// of file (in bytes). If function returns False - some error occurred and the
// FileSize parameter contains a wrong information. Use GetLastError API
// function to determine the error.
// Date: 07/02/2007
// Unicode version added 08/22/2007

function abfGetFileSizeA(const FileName: AnsiString;
  var FileSize: Int64): Boolean;
var
  Handle: THandle;
  Error: DWORD;
begin
  Result := False;
  FileSize := -1;
  Handle := abfFileOpenA(FileName, fmOpenRead or fmShareDenyNone);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    Int64Rec(FileSize).Lo := GetFileSize(Handle, @Int64Rec(FileSize).Hi);
    Error := GetLastError;
    if (Int64Rec(FileSize).Lo = $FFFFFFFF) and (Error <> NO_ERROR) then
    begin
      Int64Rec(FileSize).Hi := $FFFFFFFF;
      SetLastError(Error); // Set the error value back
    end else
      Result := True;
  finally
    FileClose(Handle);
  end;
end;

//------------------------------------------------------------------------------

function abfGetFileSize(const FileName: string;
  var FileSize: Int64): Boolean;
begin
  Result := abfGetFileSizeA(FileName, FileSize);
end;

//------------------------------------------------------------------------------

function abfGetFileSizeW(const FileName: WideString;
  var FileSize: Int64): Boolean;
var
  Handle: THandle;
  Error: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileSizeA(FileName, FileSize);
    Exit;
  end;

  Result := False;
  FileSize := -1;
  Handle := abfFileOpenW(FileName, fmOpenRead or fmShareDenyNone);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    Int64Rec(FileSize).Lo := GetFileSize(Handle, @Int64Rec(FileSize).Hi);
    Error := GetLastError;
    if (Int64Rec(FileSize).Lo = $FFFFFFFF) and (Error <> NO_ERROR) then
    begin
      Int64Rec(FileSize).Hi := $FFFFFFFF;
      SetLastError(Error); // Set the error value back
    end else
      Result := True;
  finally
    FileClose(Handle);
  end;
end;

//------------------------------------------------------------------------------
// Copies the SrcFile file to the DestFile file.
// Date: 02/23/2000
// Unicode version added 03/10/2007

function abfCopyFileA(SrcFile, DstFile: AnsiString): Boolean;
begin
  if IsWinNT then
  begin
    Result := abfCopyFileW(SrcFile, DstFile);
    Exit;
  end;

  SrcFile := abfTrimA(SrcFile, [' ', '"']);
  DstFile := abfTrimA(DstFile, [' ', '"']);

  Result := CopyFileA(PAnsiChar(SrcFile), PAnsiChar(DstFile), False);
end;

//------------------------------------------------------------------------------

function abfCopyFile(SrcFile, DstFile: string): Boolean;
begin
  Result := abfCopyFileA(SrcFile, DstFile);
end;

//------------------------------------------------------------------------------

function abfCopyFileW(SrcFile, DstFile: WideString): Boolean;
begin
  if not IsWinNT then
  begin
    Result := abfCopyFileA(SrcFile, DstFile);
    Exit;
  end;

  SrcFile := abfTrimW(SrcFile, ' "');
  DstFile := abfTrimW(DstFile, ' "');

  Result := CopyFileW(PWideChar(SrcFile), PWideChar(DstFile), False);
end;

//------------------------------------------------------------------------------
// Moves an existing file or a directory, including its children.
//
// This function will move (rename) either a file or a directory (including
// its children) either in the same directory or across directories.
// The one caveat is that the MoveFile function will fail on directory moves
// when the destination is on a different volume.
// If a file is moved across volumes, MoveFile does not move the security
// descriptor with the file. The file will be assigned the default security
// descriptor in the destination directory.
// Date: 08/27/2007

function abfRenameFileA(OldName, NewName: AnsiString): Boolean;
begin
  if IsWinNT then
  begin
    Result := abfRenameFileW(OldName, NewName);
    Exit;
  end;

  OldName := abfTrimA(OldName, [' ', '"']);
  NewName := abfTrimA(NewName, [' ', '"']);

  Result := MoveFileA(PAnsiChar(OldName), PAnsiChar(NewName));
  if Result then Exit;

  Result := abfCopyFileA(OldName, NewName);
  if not Result then Exit;

  abfDeleteFileA(OldName);
end;

//------------------------------------------------------------------------------

function abfRenameFile(OldName, NewName: string): Boolean;
begin
  Result := abfRenameFileA(OldName, NewName);
end;

//------------------------------------------------------------------------------

function abfRenameFileW(OldName, NewName: WideString): Boolean;
begin
  if not IsWinNT then
  begin
    Result := abfRenameFileA(OldName, NewName);
    Exit;
  end;

  OldName := abfTrimW(OldName, ' "');
  NewName := abfTrimW(NewName, ' "');

  Result := MoveFileW(PWideChar(OldName), PWideChar(NewName));
  if Result then Exit;

  Result := abfCopyFileW(OldName, NewName);
  if not Result then Exit;

  abfDeleteFileW(OldName);
end;

//------------------------------------------------------------------------------
// Copies the SrcFile file to the DestFile file block by block. The BlockSize
// parameter specifies a size of copying block. If you specify BlockSize as 0 -
// CabfCopyFileExBlockSize bytes blocks will be used. Use OnProgress to specify
// an event that will be called with Sender parameter after the copying of each
// block. You can terminate procedure routine by setting Terminate parameter of
// the OnProgress event True. If function returns False - some error occurred
// and file wasn't copyed properly. Use GetLastError API function to determine
// the error.
// Date: 09/18/2000
// Unicode version added 08/22/2007

function abfCopyFileExA(const SrcFile, DstFile: AnsiString; BlockSize: LongWord;
  OnProgress: TabfCopyFileProgressEvent; Sender: TObject): Boolean;
var
  SrcHandle, DstHandle: THandle;

  //-------------------------------------

  procedure _Copy;
  var
    CopiedSize, TotalSize: Int64;
    BytesRead, BytesWrite: Int64;
    Block: Pointer;
    Terminate: Boolean;
  begin
    CopiedSize := 0;
    TotalSize  := 0;
  // Get TotalSize if OnProgress assigned
    if Assigned(OnProgress) then
      if not abfGetFileSizeA(SrcFile, TotalSize) then TotalSize := 0;
  // Alocate a Block buffer
    GetMem(Block, BlockSize);
    try
      Terminate := False;
    // Copy block by block
      repeat
      // Read block
        BytesRead := FileRead(SrcHandle, Block^, BlockSize);
      // Check for the end of file
        if BytesRead <= 0 then Break;
      // Write block
        BytesWrite := FileWrite(DstHandle, Block^, BytesRead);
        if BytesWrite <> BytesRead then Exit; // The error occured
        Inc(CopiedSize, BytesRead);
      // Call the event if it is specified
        if Assigned(OnProgress) then
        begin
          OnProgress(Sender, CopiedSize, TotalSize, Terminate);
          if Terminate then
          begin
            SetLastError(ERROR_CANCELLED);
            Exit;
          end;
        end;{if Assigned(OnProgress) then}
      until False;
    // Copy operation is successful
      Result := True;
    finally
      FreeMem(Block, BlockSize);
    end;
  end;{Internal procedure _Copy}

  //-------------------------------------

begin
  Result := False;
// Fix BlockSize
  if BlockSize = 0 then BlockSize := CabfCopyFileExBlockSize;
// Open Src file, create Dst file and provide the copy operation
  SrcHandle := abfFileOpenA(SrcFile, fmOpenRead or fmShareDenyNone);
  if SrcHandle <> INVALID_HANDLE_VALUE then
  try
     DstHandle := abfFileCreateA(DstFile);
     if DstHandle <> INVALID_HANDLE_VALUE then
     try
       _Copy;
     finally
       FileClose(DstHandle);
     end;
  finally
    FileClose(SrcHandle);
  end;
end;

//------------------------------------------------------------------------------

function abfCopyFileEx(const SrcFile, DstFile: string; BlockSize: LongWord;
  OnProgress: TabfCopyFileProgressEvent; Sender: TObject): Boolean;
begin
  Result := abfCopyFileExA(SrcFile, DstFile, BlockSize, OnProgress, Sender);
end;

//------------------------------------------------------------------------------

function abfCopyFileExW(const SrcFile, DstFile: WideString; BlockSize: LongWord;
  OnProgress: TabfCopyFileProgressEvent; Sender: TObject): Boolean;
var
  SrcHandle, DstHandle: THandle;

  //-------------------------------------

  procedure _Copy;
  var
    CopiedSize, TotalSize: Int64;
    BytesRead, BytesWrite: Int64;
    Block: Pointer;
    Terminate: Boolean;
  begin
    CopiedSize := 0;
    TotalSize  := 0;
  // Get TotalSize if OnProgress assigned
    if Assigned(OnProgress) then
      if not abfGetFileSizeW(SrcFile, TotalSize) then TotalSize := 0;
  // Alocate a Block buffer
    GetMem(Block, BlockSize);
    try
      Terminate := False;
    // Copy block by block
      repeat
      // Read block
        BytesRead := FileRead(SrcHandle, Block^, BlockSize);
      // Check for the end of file
        if BytesRead <= 0 then Break;
      // Write block
        BytesWrite := FileWrite(DstHandle, Block^, BytesRead);
        if BytesWrite <> BytesRead then Exit; // The error occured
        Inc(CopiedSize, BytesRead);
      // Call the event if it is specified
        if Assigned(OnProgress) then
        begin
          OnProgress(Sender, CopiedSize, TotalSize, Terminate);
          if Terminate then
          begin
            SetLastError(ERROR_CANCELLED);
            Exit;
          end;
        end;{if Assigned(OnProgress) then}
      until False;
    // Copy operation is successful
      Result := True;
    finally
      FreeMem(Block, BlockSize);
    end;
  end;{Internal procedure _Copy}

  //-------------------------------------

begin
  if not IsWinNT then
  begin
    Result := abfCopyFileExA(SrcFile, DstFile, BlockSize, OnProgress, Sender);
    Exit;
  end;

  Result := False;
// Fix BlockSize
  if BlockSize = 0 then BlockSize := CabfCopyFileExBlockSize;
// Open Src file, create Dst file and provide the copy operation
  SrcHandle := abfFileOpenW(SrcFile, fmOpenRead or fmShareDenyNone);
  if SrcHandle <> INVALID_HANDLE_VALUE then
  try
     DstHandle := abfFileCreateW(DstFile);
     if DstHandle <> INVALID_HANDLE_VALUE then
     try
       _Copy;
     finally
       FileClose(DstHandle);
     end;
  finally
    FileClose(SrcHandle);
  end;
end;

//------------------------------------------------------------------------------
// Deletes the AFileName file.
// Date: 07/06/2007

function abfDeleteFileA(AFileName: AnsiString): Boolean;
begin
  AFileName := abfTrimA(AFileName, [' ', '"']);

  Result := Windows.DeleteFileA(PAnsiChar(AFileName));
end;

//------------------------------------------------------------------------------

function abfDeleteFile(AFileName: string): Boolean;
begin
  Result := abfDeleteFileA(AFileName);
end;

//------------------------------------------------------------------------------

function abfDeleteFileW(AFileName: WideString): Boolean;
begin
  if not IsWinNT then
  begin
    Result := abfDeleteFileA(AFileName);
    Exit;
  end;

  AFileName := abfTrimW(AFileName, ' "');

  Result := Windows.DeleteFileW(PWideChar(AFileName));
end;

//------------------------------------------------------------------------------
// Creates a new file.
// Date: 07/24/2007

function abfFileCreateA(const FileName: AnsiString): Integer;
begin
  Result := Integer(CreateFileA(PAnsiChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;

//------------------------------------------------------------------------------

function abfFileCreate(const FileName: string): Integer;
begin
  Result := abfFileCreateA(FileName);
end;

//------------------------------------------------------------------------------

function abfFileCreateW(const FileName: WideString): Integer;
begin
  if not IsWinNT then
  begin
    Result := abfFileCreateA(FileName);
    Exit;
  end;

  Result := Integer(CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;

//------------------------------------------------------------------------------

function abfFileCreateA(const FileName: AnsiString; Rights: Integer): Integer;
begin
  Result := abfFileCreateA(FileName);
end;

//------------------------------------------------------------------------------

function abfFileCreate(const FileName: string; Rights: Integer): Integer;
begin
  Result := abfFileCreateA(FileName, Rights);
end;

//------------------------------------------------------------------------------

function abfFileCreateW(const FileName: WideString; Rights: Integer): Integer;
begin
  Result := abfFileCreateW(FileName);
end;

//------------------------------------------------------------------------------
// Opens a specified file using a specified access mode.
// Date: 07/24/2007

function abfFileOpenA(const FileName: AnsiString; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := -1;
  if ((Mode and 3) <= fmOpenReadWrite) and
    ((Mode and $F0) <= fmShareDenyNone) then
    Result := Integer(CreateFileA(PAnsiChar(FileName), AccessMode[Mode and 3],
      ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0));
end;

//------------------------------------------------------------------------------

function abfFileOpen(const FileName: string; Mode: LongWord): Integer;
begin
  Result := abfFileOpenA(FileName, Mode);
end;

//------------------------------------------------------------------------------

function abfFileOpenW(const FileName: WideString; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  if not IsWinNT then
  begin
    Result := abfFileOpenA(FileName, Mode);
    Exit;
  end;

  Result := -1;
  if ((Mode and 3) <= fmOpenReadWrite) and
    ((Mode and $F0) <= fmShareDenyNone) then
    Result := Integer(CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
      ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0));
end;

//------------------------------------------------------------------------------
// Runs/Executes file with possibility of waiting the end of the file execution.
// Date: 10/16/2000
// Unicode version added 06/13/2007

function abfRunExA(const FileName: AnsiString; Wait: Boolean;
  ShowWindow: Boolean = True): DWORD;
const
  TempFileNameLen = 32768;
var
{$IfDef D9}
  si: TStartupInfoA;
{$Else}
  si: TStartupInfo;
{$EndIf}
  pi: TProcessInformation;
  TempFileName: PAnsiChar;
begin
  Result := 0;

  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  if ShowWindow then
    si.wShowWindow := SW_SHOWNORMAL
  else
    si.wShowWindow := SW_HIDE;

  GetMem(TempFileName, (TempFileNameLen + 1) * SizeOf(TempFileName^));
  try
    FillChar(TempFileName^, (TempFileNameLen + 1) * SizeOf(TempFileName^), 0);
    Move(PAnsiChar(FileName)^, TempFileName^, Length(FileName));

    if CreateProcessA(nil, TempFileName, nil, nil, False,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then
    begin
      if Wait then
      begin
        WaitForSingleObject(pi.hProcess, INFINITE);
        GetExitCodeProcess(pi.hProcess, Result);
        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
      end;
    end;
  finally
    FreeMem(TempFileName);
  end;
end;

//------------------------------------------------------------------------------

function abfRunEx(const FileName: string; Wait: Boolean;
  ShowWindow: Boolean = True): DWORD;
begin
  Result := abfRunExA(FileName, Wait, ShowWindow);
end;

//------------------------------------------------------------------------------

function abfRunExW(const FileName: WideString; Wait: Boolean;
  ShowWindow: Boolean = True): DWORD;
const
  TempFileNameLen = 32768;
var
{$IfDef D9}
  si: TStartupInfoW;
{$Else}
  si: TStartupInfo;
{$EndIf}
  pi: TProcessInformation;
  TempFileName: PWideChar;
begin
  if not IsWinNT then
  begin
    Result := abfRunExA(FileName, Wait, ShowWindow);
    Exit;
  end;

  Result := 0;

  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOWNORMAL;
  if not ShowWindow then
    si.wShowWindow := SW_HIDE;

  GetMem(TempFileName, (TempFileNameLen + 1) * SizeOf(TempFileName^));
  try
    FillChar(TempFileName^, (TempFileNameLen + 1) * SizeOf(TempFileName^), 0);
    Move(PWideChar(FileName)^, TempFileName^,
      Length(FileName) * SizeOf(TempFileName^));

    if CreateProcessW(nil, TempFileName, nil, nil, False,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, si, pi) then
    begin
      if Wait then
      begin
        WaitForSingleObject(pi.hProcess, INFINITE);
        GetExitCodeProcess(pi.hProcess, Result);
        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
      end;
    end;
  finally
    FreeMem(TempFileName);
  end;
end;

//------------------------------------------------------------------------------
// Runs/Executes file.
// Date: 10/16/2000
// Unicode version added 06/13/2007

function abfRunA(const FileName: AnsiString;
  ShowWindow: Boolean = True): DWORD;
begin
  Result := abfRunExA(FileName, False, ShowWindow);
end;

//------------------------------------------------------------------------------

function abfRun(const FileName: string;
  ShowWindow: Boolean = True): DWORD;
begin
  Result := abfRunA(FileName, ShowWindow);
end;

//------------------------------------------------------------------------------

function abfRunW(const FileName: WideString;
  ShowWindow: Boolean = True): DWORD;
begin
  Result := abfRunExW(FileName, False, ShowWindow);
end;

//------------------------------------------------------------------------------
// Executes console apps and gets its output.
// Date: 04/07/2008

function abfRunAndGetOutputA(const AFileName: AnsiString;
  const ACurrentDir: AnsiString; out AOutput: AnsiString): DWORD;
const
  TempFileNameLen = 32768;
var
  sa: TSecurityAttributes;
{$IfDef D9}
  si: TStartupInfoA;
{$Else}
  si: TStartupInfo;
{$EndIf}
  pi: TProcessInformation;
  cstdin, wstdin, rstdout, cstdout: THandle;
  BytesCount, BytesAvail: DWORD;
  TempFileName: PAnsiChar;
  TempCurrDir: PAnsiChar;
  TempBuf: PAnsiChar;
  TempExitCode: DWORD;
begin
  Result := 0;
  AOutput := '';

  FillChar(sa, SizeOf(sa), 0);
  with sa do
  begin
    nLength := SizeOf(sa);
    bInheritHandle := True;
  end;

  if not CreatePipe(cstdin, wstdin, @sa, 0) then Exit;
  try
    if not CreatePipe(rstdout, cstdout, @sa, 0) then Exit;
    try
      FillChar(si, SizeOf(si), 0);
      with si do
      begin
        cb := SizeOf(si);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        wShowWindow := SW_HIDE;
        hStdOutput := cstdout;
        hStdError := cstdout;
        hStdInput := cstdin;
      end;

      GetMem(TempFileName, (TempFileNameLen + 1) * SizeOf(TempFileName^));
      try
        FillChar(TempFileName^, (TempFileNameLen + 1) * SizeOf(TempFileName^), 0);
        Move(PAnsiChar(AFileName)^, TempFileName^, Length(AFileName));

        if Trim(ACurrentDir) <> '' then
          TempCurrDir := PAnsiChar(ACurrentDir)
        else
          TempCurrDir := nil;

        if CreateProcessA(nil, TempFileName, nil, nil, True,
          CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, TempCurrDir,
          si, pi) then
        begin
          try
            while GetExitCodeProcess(pi.hProcess, TempExitCode) and
              (TempExitCode = STILL_ACTIVE) do
              Sleep(1);

            WaitForSingleObject(pi.hProcess,INFINITE);
            GetExitCodeProcess(pi.hProcess, Result);
          finally
            CloseHandle(pi.hThread);
            CloseHandle(pi.hProcess);
          end;

          GetMem(TempBuf, 256);
          try
            repeat
              if PeekNamedPipe(rstdout, nil, 0, nil, @BytesAvail, nil)
                and (BytesAvail > 0) then
              begin
                if not ReadFile(rstdout, TempBuf^, 255, BytesCount, nil) then
                  Break;

                if BytesCount > 0 then
                begin
                  TempBuf[BytesCount] := #0;
                  AOutput := AOutput + TempBuf;
                end;
              end else
                Break;
            until False;
          finally
            FreeMem(TempBuf);
          end;
        end;
      finally
        FreeMem(TempFileName);
      end;
    finally
      CloseHandle(rstdout);
      CloseHandle(cstdout);
    end;
  finally
    CloseHandle(cstdin);
    CloseHandle(wstdin);
  end;
end;

//------------------------------------------------------------------------------

function abfRunAndGetOutput(const AFileName: string;
  const ACurrentDir: string; out AOutput: AnsiString): DWORD;
begin
  Result := abfRunAndGetOutputA(AFileName, ACurrentDir, AOutput);
end;

//------------------------------------------------------------------------------

function abfRunAndGetOutputW(const AFileName: WideString;
  const ACurrentDir: WideString; out AOutput: AnsiString): DWORD;
const
  TempFileNameLen = 32768;
var
  sa: TSecurityAttributes;
{$IfDef D9}
  si: TStartupInfoW;
{$Else}
  si: TStartupInfo;
{$EndIf}
  pi: TProcessInformation;
  cstdin, wstdin, rstdout, cstdout: THandle;
  BytesCount, BytesAvail: DWORD;
  TempFileName: PWideChar;
  TempCurrDir: PWideChar;
  TempBuf: PAnsiChar;
  TempExitCode: DWORD;
begin
  if not IsWinNT then
  begin
    Result := abfRunAndGetOutputA(AFileName, ACurrentDir, AOutput);
    Exit;
  end;

  Result := 0;
  AOutput := '';

  FillChar(sa, SizeOf(sa), 0);
  with sa do
  begin
    nLength := SizeOf(sa);
    bInheritHandle := True;
  end;

  if not CreatePipe(cstdin, wstdin, @sa, 0) then Exit;
  try
    if not CreatePipe(rstdout, cstdout, @sa, 0) then Exit;
    try
      FillChar(si, SizeOf(si), 0);
      with si do
      begin
        cb := SizeOf(si);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        wShowWindow := SW_HIDE;
        hStdOutput := cstdout;
        hStdError := cstdout;
        hStdInput := cstdin;
      end;

      GetMem(TempFileName, (TempFileNameLen + 1) * SizeOf(TempFileName^));
      try
        FillChar(TempFileName^, (TempFileNameLen + 1) * SizeOf(TempFileName^), 0);
        Move(PWideChar(AFileName)^, TempFileName^,
          Length(AFileName) * SizeOf(TempFileName^));

        if Trim(ACurrentDir) <> '' then
          TempCurrDir := PWideChar(ACurrentDir)
        else
          TempCurrDir := nil;

        if CreateProcessW(nil, TempFileName, nil, nil, True,
          CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, TempCurrDir,
          si, pi) then
        begin
          try
            while GetExitCodeProcess(pi.hProcess, TempExitCode) and
              (TempExitCode = STILL_ACTIVE) do
              Sleep(1);

            WaitForSingleObject(pi.hProcess,INFINITE);
            GetExitCodeProcess(pi.hProcess, Result);
          finally
            CloseHandle(pi.hThread);
            CloseHandle(pi.hProcess);
          end;

          GetMem(TempBuf, 256);
          try
            repeat
              if PeekNamedPipe(rstdout, nil, 0, nil, @BytesAvail, nil)
                and (BytesAvail > 0) then
              begin
                if not ReadFile(rstdout, TempBuf^, 255, BytesCount, nil) then
                  Break;

                if BytesCount > 0 then
                begin
                  TempBuf[BytesCount] := #0;
                  AOutput := AOutput + TempBuf;
                end;
              end else
                Break;
            until False;
          finally
            FreeMem(TempBuf);
          end;
        end;
      finally
        FreeMem(TempFileName);
      end;
    finally
      CloseHandle(rstdout);
      CloseHandle(cstdout);
    end;
  finally
    CloseHandle(cstdin);
    CloseHandle(wstdin);
  end;
end;

//------------------------------------------------------------------------------
// Removes file in any way. If the file are locked by the system, it will be
// deleted at once it become unlocked.
// Date: 10/16/2007

procedure abfAsyncDeleteFileA(const FileName: AnsiString);
var
  BatFile: Text;
  BatFileName, QuotedFileName: AnsiString;
  TempFileName: AnsiString;
begin
// Try simply delete
  if abfDeleteFileA(FileName) then Exit;
// Remove file using bat file
  QuotedFileName := abfEncloseStringA(FileName, '"');

  repeat
    TempFileName := abfGetUniqueTempFileNameA(SabfCompanyShortName);
    BatFileName := abfChangeFileExtA(TempFileName, SabfExt_Bat);

    if abfFileExistsA(BatFileName) then
      abfDeleteFileA(TempFileName)
    else
    begin
      abfRenameFileA(TempFileName, BatFileName);
      Break;
    end;
  until False;

  Assign(BatFile, BatFileName);
  Rewrite(BatFile);
  WriteLn(BatFile, '@ECHO OFF');
  WriteLn(BatFile, ':REPEAT');
//  WriteLn(BatFile, 'attrib -H -R ' + QuotedFileName); // remove attributes
  WriteLn(BatFile, 'del ' + QuotedFileName);          // delete file
  WriteLn(BatFile, 'if exist ' + QuotedFileName + ' goto REPEAT');
  WriteLn(BatFile, 'del "' + BatFileName + '"');
  Close(BatFile);
// Execute bat file
  abfRunA(BatFileName, False);
end;

//------------------------------------------------------------------------------

procedure abfAsyncDeleteFile(const FileName: string);
begin
  abfAsyncDeleteFile(FileName);
end;

//------------------------------------------------------------------------------

procedure abfAsyncDeleteFileW(const FileName: WideString);
var
  BatFile: Text;
  BatFileName: WideString;
  TempAnsiFileName: AnsiString;
  TempWideFileName: WideString;
begin
  if not IsWinNT then
  begin
    abfAsyncDeleteFileA(FileName);
    Exit;
  end;

// Try simply delete
  if abfDeleteFileW(FileName) then Exit;
// Remove file using bat file

//Check FileName for unicode filename
  TempAnsiFileName := FileName;
  TempWideFileName := TempAnsiFileName;

  if FileName <> TempWideFileName then
    // unicode filename
    TempAnsiFileName := abfGetShortPathNameW(FileName)
  else
    // non-unicode filename
    TempAnsiFileName := abfEncloseStringW(FileName, '"');

  repeat
    TempWideFileName := abfGetUniqueTempFileNameW(SabfCompanyShortName);
    BatFileName := abfChangeFileExtW(TempWideFileName, SabfExt_Bat);

    if abfFileExistsW(BatFileName) then
      abfDeleteFileW(TempWideFileName)
    else
    begin
      abfRenameFileW(TempWideFileName, BatFileName);
      Break;
    end;
  until False;

  Assign(BatFile, BatFileName);
  Rewrite(BatFile);
  WriteLn(BatFile, '@ECHO OFF');
  WriteLn(BatFile, ':REPEAT');
//  WriteLn(BatFile, 'attrib -H -R ' + QuotedFileName);   // remove attributes
  WriteLn(BatFile, 'del ' + TempAnsiFileName);    // delete file
  WriteLn(BatFile, 'if exist ' + TempAnsiFileName + ' goto REPEAT');
  WriteLn(BatFile, 'del ' + string(abfGetShortPathNameW(BatFileName)));
  Close(BatFile);
// Execute bat file
  abfRunW(BatFileName, False);
end;

//------------------------------------------------------------------------------
// Replaces DstName with SrcName file in any way. If the file DstName are locked
// by the system, it will be replaced at once it become unlocked.
// Date: 10/16/2007

procedure abfAsyncReplaceFileA(const SrcName, DstName: AnsiString);
var
  BatFile: Text;
  BatFileName, QuotedSrcName, QuotedDstName: AnsiString;
begin
// Try simply copy
  if abfCopyFileA(SrcName, DstName) then Exit;
// Remove file using bat file
  QuotedSrcName := abfEncloseStringA(SrcName, '"');
  QuotedDstName := abfEncloseStringA(DstName, '"');
  BatFileName := abfChangeFileExtA(abfGetUniqueTempFileNameA(SabfCompanyShortName),
    SabfExt_Bat);
  Assign(BatFile, BatFileName);
  Rewrite(BatFile);
  WriteLn(BatFile, ':REPEAT');
//  WriteLn(BatFile, 'attrib -H -R ' + QuotedDstName); // remove attributes
  WriteLn(BatFile, 'del ' + QuotedDstName);          // delete file
  WriteLn(BatFile, 'if exist ' + QuotedDstName + ' goto REPEAT');
  WriteLn(BatFile, 'copy ' + QuotedSrcName + ' ' + QuotedDstName);
  WriteLn(BatFile, 'del "' + BatFileName + '"');     // delete bat file
  Close(BatFile);
// Execute bat file
  abfRunA(BatFileName, False);
end;

//------------------------------------------------------------------------------

procedure abfAsyncReplaceFile(const SrcName, DstName: string);
begin
  abfAsyncReplaceFileA(SrcName, DstName);
end;

//------------------------------------------------------------------------------

procedure abfAsyncReplaceFileW(const SrcName, DstName: WideString);
var
  BatFile: Text;
  BatFileName, QuotedSrcName, QuotedDstName: WideString;
begin
// Try simply copy
  if abfCopyFileW(SrcName, DstName) then Exit;
// Remove file using bat file
  QuotedSrcName := abfEncloseStringW(SrcName, '"');
  QuotedDstName := abfEncloseStringW(DstName, '"');
  BatFileName := abfChangeFileExtW(abfGetUniqueTempFileNameW(SabfCompanyShortName),
    SabfExt_Bat);
  Assign(BatFile, BatFileName);
  Rewrite(BatFile);
  WriteLn(BatFile, ':REPEAT');
//  WriteLn(BatFile, 'attrib -H -R ' + QuotedDstName); // remove attributes
  WriteLn(BatFile, 'del ' + string(QuotedDstName));          // delete file
  WriteLn(BatFile, 'if exist ' + string(QuotedDstName) + ' goto REPEAT');
  WriteLn(BatFile, 'copy ' + string(QuotedSrcName) + ' ' + string(QuotedDstName));
  WriteLn(BatFile, 'del "' + string(BatFileName) + '"');     // delete bat file
  Close(BatFile);
// Execute bat file
  abfRunW(BatFileName, False);
end;

//------------------------------------------------------------------------------
// Replaces DstName with SrcName file in any way. If the file DstName are locked
// by the system, it will be replaced at once it become unlocked. After the
// replacing runs the file.
// Date: 03/28/2000
// Unicode version added 10/16/2007

procedure abfAsyncReplaceFileAndRunA(const SrcName, DstName,
  CmdLnParams: AnsiString);
var
  BatFile: Text;
  BatFileName, QuotedSrcName, QuotedDstName: AnsiString;
begin
// Try simply copy and run
  if abfCopyFileA(SrcName, DstName) then
  begin
    abfRunA(DstName, False);
    Exit;
  end;
// Remove file using a bat file
  QuotedSrcName := abfEncloseStringA(SrcName, '"');
  QuotedDstName := abfEncloseStringA(DstName, '"');
  BatFileName := abfChangeFileExtA(abfGetUniqueTempFileNameA(SabfCompanyShortName),
    SabfExt_Bat);
  Assign(BatFile, BatFileName);
  Rewrite(BatFile);
  WriteLn(BatFile, ':REPEAT');
//  WriteLn(BatFile, 'attrib -H -R ' + QuotedDstName); // remove attributes
  WriteLn(BatFile, 'del ' + QuotedDstName);          // delete file
  WriteLn(BatFile, 'if exist ' + QuotedDstName + ' goto REPEAT');
  WriteLn(BatFile, 'copy ' + QuotedSrcName + ' ' + QuotedDstName);
  if CmdLnParams <> '' then QuotedDstName := QuotedDstName + ' ' + CmdLnParams;
  WriteLn(BatFile, QuotedDstName);
  WriteLn(BatFile, 'del "' + BatFileName + '"');     // delete bat file
  Close(BatFile);
// Execute bat file
  abfRunA(BatFileName, False);
end;

//------------------------------------------------------------------------------

procedure abfAsyncReplaceFileAndRun(const SrcName, DstName,
  CmdLnParams: string);
begin
  abfAsyncReplaceFileAndRunA(SrcName, DstName, CmdLnParams);
end;

//------------------------------------------------------------------------------

procedure abfAsyncReplaceFileAndRunW(const SrcName, DstName,
  CmdLnParams: WideString);
var
  BatFile: Text;
  BatFileName, QuotedSrcName, QuotedDstName: WideString;
begin
// Try simply copy and run
  if abfCopyFileW(SrcName, DstName) then
  begin
    abfRunW(DstName, False);
    Exit;
  end;
// Remove file using a bat file
  QuotedSrcName := abfEncloseStringW(SrcName, '"');
  QuotedDstName := abfEncloseStringW(DstName, '"');
  BatFileName := abfChangeFileExtW(abfGetUniqueTempFileNameW(SabfCompanyShortName),
    SabfExt_Bat);
  Assign(BatFile, BatFileName);
  Rewrite(BatFile);
  WriteLn(BatFile, ':REPEAT');
//  WriteLn(BatFile, 'attrib -H -R ' + QuotedDstName); // remove attributes
  WriteLn(BatFile, 'del ' + string(QuotedDstName));          // delete file
  WriteLn(BatFile, 'if exist ' + string(QuotedDstName) + ' goto REPEAT');
  WriteLn(BatFile, 'copy ' + string(QuotedSrcName) + ' ' + string(QuotedDstName));
  if CmdLnParams <> '' then QuotedDstName := QuotedDstName + ' ' + CmdLnParams;
  WriteLn(BatFile, string(QuotedDstName));
  WriteLn(BatFile, 'del "' + string(BatFileName) + '"');     // delete bat file
  Close(BatFile);
// Execute bat file
  abfRunW(BatFileName, False);
end;

//------------------------------------------------------------------------------

function _FindMatchingFileA(var F: TabfSearchRecA): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFileA(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi,
      LongRec(Time).Lo);
    Size := FindData.nFileSizeLow or Int64(FindData.nFileSizeHigh) shl 32;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

//------------------------------------------------------------------------------

function _FindMatchingFileW(var F: TabfSearchRecW): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFileW(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi,
      LongRec(Time).Lo);
    Size := FindData.nFileSizeLow or Int64(FindData.nFileSizeHigh) shl 32;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

//------------------------------------------------------------------------------

procedure _CopyWideToAnsiSearchRec(ASrc: TabfSearchRecW; var ADest: TabfSearchRecA);
var
  TempS: AnsiString;
begin
  FillChar(ADest, SizeOf(ADest), 0);

  with ADest, ADest.FindData do
  begin
    Time        := ASrc.Time;
    Size        := ASrc.Size;
    Attr        := ASrc.Attr;
    Name        := AnsiString(ASrc.Name);
    ExcludeAttr := ASrc.ExcludeAttr;
    FindHandle  := ASrc.FindHandle;
  // FindData
    dwFileAttributes   := ASrc.FindData.dwFileAttributes;
    ftCreationTime     := ASrc.FindData.ftCreationTime;
    ftLastAccessTime   := ASrc.FindData.ftLastAccessTime;
    ftLastWriteTime    := ASrc.FindData.ftLastWriteTime;
    nFileSizeHigh      := ASrc.FindData.nFileSizeHigh;
    nFileSizeLow       := ASrc.FindData.nFileSizeLow;
    dwReserved0        := ASrc.FindData.dwReserved0;
    dwReserved1        := ASrc.FindData.dwReserved1;

    TempS := AnsiString(WideString(PWideChar(@ASrc.FindData.cFileName)));
    Move(PAnsiChar(TempS)^, cFileName[0], Min(Length(TempS), Length(cFileName) - 1));

    TempS := AnsiString(WideString(PWideChar(@ASrc.FindData.cAlternateFileName)));
    Move(PAnsiChar(TempS)^, cAlternateFileName[0], Min(Length(TempS),
      Length(cAlternateFileName) - 1));
  end;
end;

//------------------------------------------------------------------------------

procedure _CopyAnsiToWideSearchRec(ASrc: TabfSearchRecA; var ADest: TabfSearchRecW);
var
  TempS: WideString;
begin
  FillChar(ADest, SizeOf(ADest), 0);

  with ADest, ADest.FindData do
  begin
    Time        := ASrc.Time;
    Size        := ASrc.Size;
    Attr        := ASrc.Attr;
    Name        := WideString(ASrc.Name);
    ExcludeAttr := ASrc.ExcludeAttr;
    FindHandle  := ASrc.FindHandle;
  // FindData
    dwFileAttributes   := ASrc.FindData.dwFileAttributes;
    ftCreationTime     := ASrc.FindData.ftCreationTime;
    ftLastAccessTime   := ASrc.FindData.ftLastAccessTime;
    ftLastWriteTime    := ASrc.FindData.ftLastWriteTime;
    nFileSizeHigh      := ASrc.FindData.nFileSizeHigh;
    nFileSizeLow       := ASrc.FindData.nFileSizeLow;
    dwReserved0        := ASrc.FindData.dwReserved0;
    dwReserved1        := ASrc.FindData.dwReserved1;

    TempS := WideString(string(PChar(@ASrc.FindData.cFileName)));
    Move(PWideChar(TempS)^, cFileName[0],
      Min(Length(TempS), Length(cFileName) - 1) * 2);

    TempS := WideString(string(PChar(@ASrc.FindData.cAlternateFileName)));
    Move(PWideChar(TempS)^, cAlternateFileName[0], Min(Length(TempS),
      Length(cAlternateFileName) - 1) * 2);
  end;
end;

//------------------------------------------------------------------------------
// abfFindFirst searches the directory given by Path for the first entry that
// matches the filename given by Path and the attributes given by Attr. The
// result is returned in the search record given by SearchRec. The return
// value is zero if the function was successful. Otherwise the return value
// is a system error code. After calling abfFindFirst, always call abfFindClose.
// abfFindFirst is typically used with abfFindNext and abfFindClose as follows:
//
//   Result := abfFindFirst(Path, Attr, SearchRec);
//   while Result = 0 do
//   begin
//     ProcessSearchRec(SearchRec);
//     Result := abfFindNext(SearchRec);
//   end;
//   abfFindClose(SearchRec);
//
// where ProcessSearchRec represents user-defined code that processes the
// information in a search record.
//
// Date: 08/21/2007

function abfFindFirstA(const Path: AnsiString; Attr: Integer;
  var F: TabfSearchRecA): Integer;
const
  faSpecial = faHidden or faSysFile or faDirectory;
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFileA(PAnsiChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := _FindMatchingFileA(F);
    if Result <> 0 then abfFindCloseA(F);
  end else
    Result := GetLastError;
end;

//------------------------------------------------------------------------------

function abfFindFirst(const Path: string; Attr: Integer;
  var F: TabfSearchRec): Integer;
begin
  Result := abfFindFirstA(Path, Attr, F);
end;

//------------------------------------------------------------------------------

function abfFindFirstW(const Path: WideString; Attr: Integer;
  var F: TabfSearchRecW): Integer;
const
  faSpecial = faHidden or faSysFile or faDirectory;
var
  TempRec: TabfSearchRecA;
begin
  if not IsWinNT then
  begin
    _CopyWideToAnsiSearchRec(F, TempRec);
    try
      Result := abfFindFirstA(Path, Attr, TempRec);
    finally
      _CopyAnsiToWideSearchRec(TempRec, F);
    end;
    Exit;
  end;

  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFileW(PWideChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := _FindMatchingFileW(F);
    if Result <> 0 then abfFindCloseW(F);
  end else
    Result := GetLastError;
end;

//------------------------------------------------------------------------------
// abfFindNext returs the next entry that matches the name and attributes
// specified in a previous call to abfFindFirst. The search record must be one
// that was passed to abfFindFirst. The return value is zero if the function was
// successful. Otherwise the return value is a system error code.
// Date: 08/21/2007

function abfFindNextA(var F: TabfSearchRecA): Integer;
begin
  if FindNextFileA(F.FindHandle, F.FindData) then
    Result := _FindMatchingFileA(F) else
    Result := GetLastError;
end;

//------------------------------------------------------------------------------

function abfFindNext(var F: TabfSearchRec): Integer;
begin
  Result := abfFindNextA(F);
end;

//------------------------------------------------------------------------------

function abfFindNextW(var F: TabfSearchRecW): Integer;
var
  TempRec: TabfSearchRecA;
begin
  if not IsWinNT then
  begin
    _CopyWideToAnsiSearchRec(F, TempRec);
    try
      Result := abfFindNextA(TempRec);
    finally
      _CopyAnsiToWideSearchRec(TempRec, F);
    end;
    Exit;
  end;

  if FindNextFileW(F.FindHandle, F.FindData) then
    Result := _FindMatchingFileW(F) else
    Result := GetLastError;
end;

//------------------------------------------------------------------------------
// abfFindClose terminates a abfFindFirst/abfFindNext sequence and frees memory
// and system resources allocated by abfFindFirst.
// Every abfFindFirst/abfFindNext must end with a call to abfFindClose.
// Date: 08/21/2007

procedure abfFindCloseA(var F: TabfSearchRecA);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

//------------------------------------------------------------------------------

procedure abfFindClose(var F: TabfSearchRec);
begin
  abfFindCloseA(F);
end;

//------------------------------------------------------------------------------

procedure abfFindCloseW(var F: TabfSearchRecW);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

//------------------------------------------------------------------------------
// The abfFindFirstChangeNotification function creates a change
// notification handle and sets up initial change notification filter
// conditions. A wait on a notification handle succeeds when a change matching
// the filter conditions occurs in the specified directory or subtree.
// However, the function does not indicate the change that satisfied
// the wait condition.
// Date: 11/06/2007

function abfFindFirstChangeNotificationA(const APathName: AnsiString;
  AWatchSubtree: BOOL; ANotifyFilter: DWORD): THandle;
begin
  Result := FindFirstChangeNotificationA(PAnsiChar(APathName),
    AWatchSubtree, ANotifyFilter);
end;

//------------------------------------------------------------------------------

function abfFindFirstChangeNotification(const APathName: string;
  AWatchSubtree: BOOL; ANotifyFilter: DWORD): THandle;
begin
  Result := abfFindFirstChangeNotificationA(APathName, AWatchSubtree,
    ANotifyFilter);
end;

//------------------------------------------------------------------------------

function abfFindFirstChangeNotificationW(const APathName: WideString;
  AWatchSubtree: BOOL; ANotifyFilter: DWORD): THandle;
begin
  if not IsWinNT then
  begin
    Result := abfFindFirstChangeNotificationA(APathName, AWatchSubtree,
      ANotifyFilter);
    Exit;
  end;

  Result := FindFirstChangeNotificationW(PWideChar(APathName),
    AWatchSubtree, ANotifyFilter);
end;

//------------------------------------------------------------------------------
// Process CallBack procedure for each files matched the PathWithMask and given
// attributes.
// Date: 02/11/2010

function abfForEachFileA(const PathWithMask: AnsiString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackA): Integer;

  function _Iterate(const Path: AnsiString): Integer;
  var
    SearchRec: TabfSearchRecA;
    R: Integer;
    APath, AFile: AnsiString;
    AContinue: Boolean;
    FirstR: Integer;
  begin
    Result := 0;
    APath := abfAddSlashA(abfExtractFilePathA(Path));
    AFile := abfExtractFileNameA(Path);

  // Search and process files
    FirstR := abfFindFirstA(Path, FileAttr, SearchRec);
    R := FirstR;
    try
      AContinue := True;
      while (R = 0) and AContinue do
      begin
        if (SearchRec.Attr and FileAttr <> 0) or
          ((SearchRec.Attr = 0) and (FileAttr and faOnlyFiles <> 0)) then
        begin
          CallBack(APath, SearchRec, AContinue);
          Inc(Result);
        end;
        R := abfFindNextA(SearchRec);
      end;
    finally
      if FirstR = 0  then abfFindCloseA(SearchRec);
    end;

  // Process sub-dirs if needed
    if not ProcessSubDirs then Exit;

    FirstR := abfFindFirstA(APath + '*.*', faDirectory or (FileAttr and $0000000F),
      SearchRec);
    R := FirstR;
    try
      while R = 0 do
      begin
        if (SearchRec.Name[1] <> '.') and (SearchRec.Attr and faDirectory <> 0) then
          Inc(Result, _Iterate(abfAddSlashA(APath + SearchRec.Name) + AFile));
        R := abfFindNextA(SearchRec);
      end;
    finally
      if FirstR = 0 then abfFindCloseA(SearchRec);
    end;
  end;

begin
  if Assigned(CallBack) then Result := _Iterate(PathWithMask)
  else Result := 0;
end;

//------------------------------------------------------------------------------

function abfForEachFile(const PathWithMask: string; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBack): Integer;
begin
  Result := abfForEachFileA(PathWithMask, FileAttr, ProcessSubDirs,
    CallBack);
end;

//------------------------------------------------------------------------------

function abfForEachFileW(const PathWithMask: WideString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackW): Integer;

  function _Iterate(const Path: WideString): Integer;
  var
    SearchRec: TabfSearchRecW;
    R: Integer;
    APath, AFile: WideString;
    AContinue: Boolean;
    FirstR: Integer;
  begin
    Result := 0;
    APath := abfAddSlashW(abfExtractFilePathW(Path));
    AFile := abfExtractFileNameW(Path);

  // Search and process files
    FirstR := abfFindFirstW(Path, FileAttr, SearchRec);
    R := FirstR;
    try
      AContinue := True;
      while (R = 0) and AContinue do
      begin
        if (SearchRec.Attr and FileAttr <> 0) or
          ((SearchRec.Attr = 0) and (FileAttr and faOnlyFiles <> 0)) then
        begin
          CallBack(APath, SearchRec, AContinue);
          Inc(Result);
        end;
        R := abfFindNextW(SearchRec);
      end;
    finally
      if FirstR = 0  then abfFindCloseW(SearchRec);
    end;

  // Process sub-dirs if needed
    if not ProcessSubDirs then Exit;

    FirstR := abfFindFirstW(APath + '*.*', faDirectory or (FileAttr and $0000000F),
      SearchRec);
    R := FirstR;
    try
      while R = 0 do
      begin
        if (SearchRec.Name[1] <> '.') and (SearchRec.Attr and faDirectory <> 0) then
          Inc(Result, _Iterate(abfAddSlashW(APath + SearchRec.Name) + AFile));
        R := abfFindNextW(SearchRec);
      end;
    finally
      if FirstR = 0 then abfFindCloseW(SearchRec);
    end;
  end;

begin
  if Assigned(CallBack) then Result := _Iterate(PathWithMask)
  else Result := 0;
end;

//------------------------------------------------------------------------------

function abfForEachFileA(const PathWithMask: AnsiString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackMethodA): Integer;

  function _Iterate(const Path: AnsiString): Integer;
  var
    SearchRec: TabfSearchRecA;
    R: Integer;
    APath, AFile: AnsiString;
    AContinue: Boolean;
    FirstR: Integer;
  begin
    Result := 0;
    APath := abfAddSlashA(abfExtractFilePathA(Path));
    AFile := abfExtractFileNameA(Path);

  // Search and process files
    FirstR := abfFindFirstA(Path, FileAttr, SearchRec);
    R := FirstR;
    try
      AContinue := True;
      while (R = 0) and AContinue do
      begin
        if (SearchRec.Attr and FileAttr <> 0) or
          ((SearchRec.Attr = 0) and (FileAttr and faOnlyFiles <> 0)) then
        begin
          CallBack(APath, SearchRec, AContinue);
          Inc(Result);
        end;
        R := abfFindNextA(SearchRec);
      end;
    finally
      if FirstR = 0  then abfFindCloseA(SearchRec);
    end;

  // Process sub-dirs if needed
    if not ProcessSubDirs then Exit;

    FirstR := abfFindFirstA(APath + '*.*', faDirectory or (FileAttr and $0000000F),
      SearchRec);
    R := FirstR;
    try
      while R = 0 do
      begin
        if (SearchRec.Name[1] <> '.') and (SearchRec.Attr and faDirectory <> 0) then
          Inc(Result, _Iterate(abfAddSlashA(APath + SearchRec.Name) + AFile));
        R := abfFindNextA(SearchRec);
      end;
    finally
      if FirstR = 0 then abfFindCloseA(SearchRec);
    end;
  end;

begin
  if Assigned(CallBack) then Result := _Iterate(PathWithMask)
  else Result := 0;
end;

//------------------------------------------------------------------------------

function abfForEachFile(const PathWithMask: string; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackMethod): Integer;
begin
  Result := abfForEachFileA(PathWithMask, FileAttr, ProcessSubDirs,
    CallBack);
end;

//------------------------------------------------------------------------------

function abfForEachFileW(const PathWithMask: WideString; FileAttr: Integer;
  ProcessSubDirs: Boolean; CallBack: TabfForEachFileCallBackMethodW): Integer;

  function _Iterate(const Path: WideString): Integer;
  var
    SearchRec: TabfSearchRecW;
    R: Integer;
    APath, AFile: WideString;
    AContinue: Boolean;
    FirstR: Integer;
  begin
    Result := 0;
    APath := abfAddSlashW(abfExtractFilePathW(Path));
    AFile := abfExtractFileNameW(Path);

  // Search and process files
    FirstR := abfFindFirstW(Path, FileAttr, SearchRec);
    R := FirstR;
    try
      AContinue := True;
      while (R = 0) and AContinue do
      begin
        if (SearchRec.Attr and FileAttr <> 0) or
          ((SearchRec.Attr = 0) and (FileAttr and faOnlyFiles <> 0)) then
        begin
          CallBack(APath, SearchRec, AContinue);
          Inc(Result);
        end;
        R := abfFindNextW(SearchRec);
      end;
    finally
      if FirstR = 0  then abfFindCloseW(SearchRec);
    end;

  // Process sub-dirs if needed
    if not ProcessSubDirs then Exit;

    FirstR := abfFindFirstW(APath + '*.*', faDirectory or (FileAttr and $0000000F),
      SearchRec);
    R := FirstR;
    try
      while R = 0 do
      begin
        if (SearchRec.Name[1] <> '.') and (SearchRec.Attr and faDirectory <> 0) then
          Inc(Result, _Iterate(abfAddSlashW(APath + SearchRec.Name) + AFile));
        R := abfFindNextW(SearchRec);
      end;
    finally
      if FirstR = 0 then abfFindCloseW(SearchRec);
    end;
  end;

begin
  if Assigned(CallBack) then Result := _Iterate(PathWithMask)
  else Result := 0;
end;

//------------------------------------------------------------------------------
// Converts a TDateTime value to the TFileTime value.
// Date: 06/17/2000

function abfDateTimeToFileTime(DateTime: TDateTime): TFileTime;
var
  ST: TSystemTime;
  LT: TFileTime;
begin
  FillChar(Result, SizeOf(Result), 0);

  DateTimeToSystemTime(DateTime, ST);
  SystemTimeToFileTime(ST, LT);
  LocalFileTimeToFileTime(LT, LT);
  Result := LT;
end;

//------------------------------------------------------------------------------
// Converts a TFileTime value to the TDateTime value.
// Date: 09/01/2000

function abfFileTimeToDateTime(FileTime: TFileTime): TDateTime;
var
  ST: TSystemTime;
  LT: TFileTime;
begin
  FileTimeToLocalFileTime(FileTime, LT);
  FileTimeToSystemTime(LT, ST);
  Result := SystemTimeToDateTime(ST);
end;

//------------------------------------------------------------------------------
// Retrieves a set of FAT file system attributes
// for a specified file or directory.
// Date: 07/28/2008

function abfGetFileAttrA(AFileName: AnsiString): DWORD;
var
  OldErrorMode: Cardinal;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    AFileName := abfTrimA(AFileName, [' ', '"']);
    Result := GetFileAttributesA(PAnsiChar(AFileName));
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------

function abfGetFileAttr(AFileName: string): DWORD;
begin
  Result := abfGetFileAttrA(AFileName);
end;

//------------------------------------------------------------------------------

function abfGetFileAttrW(AFileName: WideString): DWORD;
var
  OldErrorMode: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileAttrA(AFileName);
    Exit;
  end;

  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    AFileName := abfTrimW(AFileName, ' "');
    Result := GetFileAttributesW(PWideChar(AFileName));
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------
// Sets the attributes for a file or directory.
// Date: 07/28/2008

function abfSetFileAttrA(AFileName: AnsiString; AFileAttributes: DWORD): Boolean;
var
  OldErrorMode: Cardinal;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    AFileName := abfTrimA(AFileName, [' ', '"']);
    Result := SetFileAttributesA(PAnsiChar(AFileName), AFileAttributes);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------

function abfSetFileAttr(AFileName: string; AFileAttributes: DWORD): Boolean;
begin
  Result := abfSetFileAttrA(AFileName, AFileAttributes);
end;

//------------------------------------------------------------------------------

function abfSetFileAttrW(AFileName: WideString; AFileAttributes: DWORD): Boolean;
var
  OldErrorMode: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfSetFileAttrA(AFileName, AFileAttributes);
    Exit;
  end;

  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    AFileName := abfTrimW(AFileName, ' "');
    Result := SetFileAttributesW(PWideChar(AFileName), AFileAttributes);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------
// Modifies attributes of a specified file.
// Date: 02/23/2000
// Unicode version added 03/15/2008

function abfModifyFileAttrA(const FileName: AnsiString;
  RemoveAttr, AddAttr: Integer): Integer;
var
  FileAttr: Integer;
begin
  Result := 0;
  FileAttr := abfGetFileAttrA(FileName);
  FileAttr := (FileAttr and (not RemoveAttr)) or AddAttr;
  if not abfSetFileAttrA(FileName, FileAttr) then
    Result := GetLastError;
end;

//------------------------------------------------------------------------------

function abfModifyFileAttr(const FileName: string;
  RemoveAttr, AddAttr: Integer): Integer;
begin
  Result := abfModifyFileAttrA(FileName, RemoveAttr, AddAttr);
end;

//------------------------------------------------------------------------------

function abfModifyFileAttrW(const FileName: WideString;
  RemoveAttr, AddAttr: Integer): Integer;
var
  FileAttr: Integer;
begin
  if not IsWinNT then
  begin
    Result := abfModifyFileAttrA(FileName, RemoveAttr, AddAttr);
    Exit;
  end;

  Result := 0;
  FileAttr := abfGetFileAttrW(FileName);
  FileAttr := (FileAttr and (not RemoveAttr)) or AddAttr;
  if not abfSetFileAttrW(FileName, FileAttr) then
    Result := GetLastError;
end;

//------------------------------------------------------------------------------
// Returns date and time for the specified file
// Date: 02/23/2000
// Unicode version added 08/22/2007

function abfGetFileDateTimeA(const FileName: AnsiString): TDateTime;
var
  Handle: THandle;
begin
  Result := Now;
  Handle := abfFileOpenA(FileName, fmOpenWrite or fmShareDenyNone);
  try
    if Handle > 0 then Result := FileDateToDateTime(FileGetDate(Handle));
  finally
    FileClose(Handle)
  end;
end;

//------------------------------------------------------------------------------

function abfGetFileDateTime(const FileName: string): TDateTime;
begin
  Result := abfGetFileDateTimeA(FileName);
end;

//------------------------------------------------------------------------------

function abfGetFileDateTimeW(const FileName: WideString): TDateTime;
var
  Handle: THandle;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileDateTimeA(FileName);
    Exit;
  end;

  Result := Now;
  Handle := abfFileOpenW(FileName, fmOpenWrite or fmShareDenyNone);
  try
    if Handle > 0 then Result := FileDateToDateTime(FileGetDate(Handle));
  finally
    FileClose(Handle)
  end;
end;

//------------------------------------------------------------------------------
// Sets date and time for the specified file
// Date: 02/23/2000
// Unicode version added 08/22/2007

procedure abfSetFileDateTimeA(const FileName: AnsiString;
  const DateTime: TDateTime);
var
  Handle: THandle;
begin
  Handle := abfFileOpenA(FileName, fmOpenWrite or fmShareDenyNone);
  try
    if Handle > 0 then FileSetDate(Handle, DateTimeToFileDate(DateTime));
  finally
    FileClose(Handle)
  end;
end;

//------------------------------------------------------------------------------

procedure abfSetFileDateTime(const FileName: string; const DateTime: TDateTime);
begin
  abfSetFileDateTimeA(FileName, DateTime);
end;

//------------------------------------------------------------------------------

procedure abfSetFileDateTimeW(const FileName: WideString;
  const DateTime: TDateTime);
var
  Handle: THandle;
begin
  if not IsWinNT then
  begin
    abfSetFileDateTimeA(FileName, DateTime);
    Exit;
  end;

  Handle := abfFileOpenW(FileName, fmOpenWrite or fmShareDenyNone);
  try
    if Handle > 0 then FileSetDate(Handle, DateTimeToFileDate(DateTime));
  finally
    FileClose(Handle)
  end;
end;

//------------------------------------------------------------------------------
// Returns the date/time that the specified file was last accessed.
// Date: 09/01/2000
// Unicode version added 08/22/2007

function abfGetFileLastAccessA(const FileName: AnsiString): TDateTime;
var
  Handle: THandle;
  FindData: TWin32FindDataA;
begin
  Double(Result) := 0.0;
  Handle := FindFirstFileA(PAnsiChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := abfFileTimeToDateTime(FindData.ftLastAccessTime);
  end else
    RaiseLastWin32Error;
end;

//------------------------------------------------------------------------------

function abfGetFileLastAccess(const FileName: string): TDateTime;
begin
  Result := abfGetFileLastAccessA(FileName);
end;

//------------------------------------------------------------------------------

function abfGetFileLastAccessW(const FileName: WideString): TDateTime;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileLastAccessA(FileName);
    Exit;
  end;

  Double(Result) := 0.0;
  Handle := FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := abfFileTimeToDateTime(FindData.ftLastAccessTime);
  end else
    RaiseLastWin32Error;
end;

//------------------------------------------------------------------------------
// Returns the date/time that the specified file was created.
// Date: 09/01/2000
// Unicode version added 08/22/2007

function abfGetFileCreationA(const FileName: AnsiString): TDateTime;
var
  Handle: THandle;
  FindData: TWin32FindDataA;
begin
  Double(Result) := 0.0;
  Handle := FindFirstFileA(PAnsiChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := abfFileTimeToDateTime(FindData.ftCreationTime);
  end else
    RaiseLastWin32Error;
end;

//------------------------------------------------------------------------------

function abfGetFileCreation(const FileName: string): TDateTime;
begin
  Result := abfGetFileCreationA(FileName);
end;

//------------------------------------------------------------------------------

function abfGetFileCreationW(const FileName: WideString): TDateTime;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileCreationA(FileName);
    Exit;
  end;

  Double(Result) := 0.0;
  Handle := FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := abfFileTimeToDateTime(FindData.ftCreationTime);
  end else
    RaiseLastWin32Error;
end;

//------------------------------------------------------------------------------
// Returns the date/time that the specified file was last written to.
// Date: 09/01/2000
// Unicode version added 08/22/2007

function abfGetFileLastWriteA(const FileName: AnsiString): TDateTime;
var
  Handle: THandle;
  FindData: TWin32FindDataA;
begin
  Double(Result) := 0.0;
  Handle := FindFirstFileA(PAnsiChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := abfFileTimeToDateTime(FindData.ftLastWriteTime);
  end else
    RaiseLastWin32Error;
end;

//------------------------------------------------------------------------------

function abfGetFileLastWrite(const FileName: string): TDateTime;
begin
  Result := abfGetFileLastWriteA(FileName);
end;

//------------------------------------------------------------------------------

function abfGetFileLastWriteW(const FileName: WideString): TDateTime;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileLastWriteA(FileName);
    Exit;
  end;

  Double(Result) := 0.0;
  Handle := FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := abfFileTimeToDateTime(FindData.ftLastWriteTime);
  end else
    RaiseLastWin32Error;
end;

//------------------------------------------------------------------------------
// Sets the specified file's last-access timestamp
// Date: 09/01/2000
// Unicode version added 08/22/2007

function abfSetFileLastAccessA(const FileName: AnsiString;
  const DateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  Attr: Cardinal;
begin
  Result := False;
  Attr := 0;
  if IsWinNT then
    Attr := FILE_FLAG_BACKUP_SEMANTICS;
  Handle := CreateFileA(PAnsiChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, Attr, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit;
  try
    FileTime := abfDateTimeToFileTime(DateTime);
    Result := SetFileTime(Handle, nil, @FileTime, nil);
  finally
    CloseHandle(Handle);
  end;
end;

//------------------------------------------------------------------------------

function abfSetFileLastAccess(const FileName: string;
  const DateTime: TDateTime): Boolean;
begin
  Result := abfSetFileLastAccessA(FileName, DateTime);
end;

//------------------------------------------------------------------------------

function abfSetFileLastAccessW(const FileName: WideString;
  const DateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  Attr: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfSetFileLastAccessA(FileName, DateTime);
    Exit;
  end;

  Result := False;
  Attr := FILE_FLAG_BACKUP_SEMANTICS;
  Handle := CreateFileW(PWideChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, Attr, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit;
  try
    FileTime := abfDateTimeToFileTime(DateTime);
    Result := SetFileTime(Handle, nil, @FileTime, nil);
  finally
    CloseHandle(Handle);
  end;
end;

//------------------------------------------------------------------------------
// Sets the specified file's creation timestamp
// Date: 09/01/2000
// Unicode version added 08/22/2007

function abfSetFileCreationA(const FileName: AnsiString;
  const DateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  Attr: Cardinal;
begin
  Result := False;
  Attr := 0;
  if IsWinNT then
    Attr := FILE_FLAG_BACKUP_SEMANTICS;
  Handle := CreateFileA(PAnsiChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, Attr, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit;
  try
    FileTime := abfDateTimeToFileTime(DateTime);
    Result := SetFileTime(Handle, @FileTime, nil, nil);
  finally
    CloseHandle(Handle);
  end;
end;

//------------------------------------------------------------------------------

function abfSetFileCreation(const FileName: string;
  const DateTime: TDateTime): Boolean;
begin
  Result := abfSetFileCreationA(FileName, DateTime);
end;

//------------------------------------------------------------------------------

function abfSetFileCreationW(const FileName: WideString;
  const DateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  Attr: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfSetFileCreationA(FileName, DateTime);
    Exit;
  end;

  Result := False;
  Attr := FILE_FLAG_BACKUP_SEMANTICS;
  Handle := CreateFileW(PWideChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, Attr, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit;
  try
    FileTime := abfDateTimeToFileTime(DateTime);
    Result := SetFileTime(Handle, @FileTime, nil, nil);
  finally
    CloseHandle(Handle);
  end;
end;

//------------------------------------------------------------------------------
// Sets the specified file's last-write timestamp
// Date: 09/01/2000
// Unicode version added 08/22/2007

function abfSetFileLastWriteA(const FileName: AnsiString;
  const DateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  Attr: Cardinal;
begin
  Result := False;
  Attr := 0;
  if IsWinNT then
    Attr := FILE_FLAG_BACKUP_SEMANTICS;
  Handle := CreateFileA(PAnsiChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, Attr, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit;
  try
    FileTime := abfDateTimeToFileTime(DateTime);
    Result := SetFileTime(Handle, nil, nil, @FileTime);
  finally
    CloseHandle(Handle);
  end;
end;

//------------------------------------------------------------------------------

function abfSetFileLastWrite(const FileName: string;
  const DateTime: TDateTime): Boolean;
begin
  Result := abfSetFileLastWriteA(FileName, DateTime);
end;

//------------------------------------------------------------------------------

function abfSetFileLastWriteW(const FileName: WideString;
  const DateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FileTime: TFileTime;
  Attr: Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfSetFileLastWriteA(FileName, DateTime);
    Exit;
  end;

  Result := False;
  Attr := FILE_FLAG_BACKUP_SEMANTICS;
  Handle := CreateFileW(PWideChar(FileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
    OPEN_EXISTING, Attr, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit;
  try
    FileTime := abfDateTimeToFileTime(DateTime);
    Result := SetFileTime(Handle, nil, nil, @FileTime);
  finally
    CloseHandle(Handle);
  end;
end;

//------------------------------------------------------------------------------
// Returns a build's date and time of the specified application. Based on
// Alexander Kramarenko's <www.akhome.da.ru> routines.
// Note: Normally works only with files created by Borland compilers.
// Date: 09/07/2000
// Unicode version added 08/22/2007

function _GetFileBuildDateTimeW(FileHandle: HFile;
  var BuildTime: TDateTime): Boolean;
type
  TImageDosHeader = packed record
    e_magic: array[0..1] of Char; // MZ signature
    e_cblp: Word;                 // bytes in last page
    e_cp: Word;                   // page count
    e_crlc: Word;                 // Relocations
    e_cparhdr: Word;              // Descriptor size in paragraphs
    e_minalloc: Word;             // Minimum extra paragraphsneeded
    e_maxalloc: Word;             // Maximum extra paragraphsneeded
    e_ss: Word;                   // SS value
    e_sp: Word;                   // SP value
    e_csum: Word;                 // CheckSum
    e_ip: Word;                   // IP value
    e_cs: Word;                   // CS value
    e_lfarlc: Word;               // Reloc Table address
    e_ovno: Word;                 // Overlay count
    e_res: array[0..3] of Word;   // Reserved
    e_oemid: Word;                // OEM identifier (for e_oeminfo)
    e_oeminfo: Word;              // OEM information; e_oemid specific
    e_res2: array [0..9] of Word; // Reserved
    e_lfanew: LongInt;            // New header address
  end;

  PImageResourceDirectory = ^TImageResourceDirectory;
  TImageResourceDirectory = packed record
    Characteristics: LongWord;
    TimeDateStamp: LongWord;
    MajorVersion: Word;
    MinorVersion: Word;
    NumberOfNamedEntries: Word;
    NumberOfIdEntries: Word;
//  IMAGE_RESOURCE_DIRECTORY_ENTRY DirectoryEntries[];
  end;
var
  ImageDosHeader: TImageDosHeader;
  Signature: Cardinal;
  ImageFileHeader:TImageFileHeader;
  ImageOptionalHeader: TImageOptionalHeader;
  ImageSectionHeader: TImageSectionHeader;
  ImageResourceDirectory: TImageResourceDirectory;
  Temp: DWORD;
  i: Integer;
begin
  Result := False;

  ReadFile(FileHandle, ImageDosHeader, SizeOf(ImageDosHeader), Temp, nil);
// Check MZ signature
  if CompareStr(ImageDosHeader.e_magic, 'MZ') <> 0 then Exit;
  SetFilePointer(FileHandle, ImageDosHeader.e_lfanew, nil, FILE_BEGIN);
  ReadFile(FileHandle, Signature, SizeOf(Signature), Temp, nil);
  ReadFile(FileHandle, ImageFileHeader, SizeOf(ImageFileHeader), Temp, nil);
  ReadFile(FileHandle, ImageOptionalHeader, SizeOf(ImageOptionalHeader),
    Temp, nil);
  for i := 0 to ImageFileHeader.NumberOfSections - 1 do
  try
    ReadFile(FileHandle, ImageSectionHeader, SizeOf(ImageSectionHeader),
      Temp, nil);
    if StrComp(@ImageSectionHeader.Name, '.rsrc') <> 0 then Continue;

    SetFilePointer(FileHandle, ImageSectionHeader.PointerToRawData,
      nil, FILE_BEGIN);
    ReadFile(FileHandle, ImageResourceDirectory, SizeOf(ImageResourceDirectory),
      Temp, nil);

    BuildTime := FileDateToDateTime(ImageResourceDirectory.TimeDateStamp);
    Result := True;
    Break;
  except
  end;{for i := 0 to ...}
end;

//------------------------------------------------------------------------------

function abfGetFileBuildDateTimeA(FileName: AnsiString;
  var BuildTime: TDateTime): Boolean;
var
  hExeFile: HFile;
begin
  Result := False;
  if not abfFileExistsA(FileName) then Exit;
  hExeFile := CreateFileA(PAnsiChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  try
    Result := _GetFileBuildDateTimeW(hExeFile, BuildTime);
  finally
    FileClose(hExeFile);
  end;
end;

//------------------------------------------------------------------------------

function abfGetFileBuildDateTime(FileName: string;
  var BuildTime: TDateTime): Boolean;
begin
  Result := abfGetFileBuildDateTimeA(FileName, BuildTime);
end;

//------------------------------------------------------------------------------

function abfGetFileBuildDateTimeW(FileName: WideString;
  var BuildTime: TDateTime): Boolean;
var
  hExeFile: HFile;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileBuildDateTimeA(FileName, BuildTime);
    Exit;
  end;

  Result := False;
  if not abfFileExistsW(FileName) then Exit;
  hExeFile := CreateFileW(PWideChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  try
    Result := _GetFileBuildDateTimeW(hExeFile, BuildTime);
  finally
    FileClose(hExeFile);
  end;
end;


//==============================================================================
// Resource routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns file description from VERSIONINFO
// 06/25/2007

function abfGetFileDescriptionA(const AFileName: AnsiString): AnsiString;
type
  PLangAndCodePage = ^TLangAndCodePage;
  TLangAndCodePage = packed record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
var
  TempWnd: DWORD;
  TempBufSize: DWORD;
  TempBuf: Pointer;
  TempTrans: PLangAndCodePage;
  TempLen: DWORD;
  TempFileDesc: AnsiString;
  TempValue: Pointer;
begin
  Result := '';

  TempBufSize := GetFileVersionInfoSizeA(PAnsiChar(AFileName), TempWnd);
  if TempBufSize < 1 then Exit;

  GetMem(TempBuf, TempBufSize);
  try
    if not GetFileVersionInfoA(PAnsiChar(AFileName), TempWnd, TempBufSize,
      TempBuf) then Exit;

    if not VerQueryValueA(TempBuf, PAnsiChar('\VarFileInfo\Translation'), Pointer(TempTrans),
      TempLen) then Exit;

    TempFileDesc := Format('\StringFileInfo\%04x%04x\FileDescription',
      [TempTrans^.wLanguage, TempTrans^.wCodePage]);

    if not VerQueryValueA(TempBuf, PAnsiChar(TempFileDesc), TempValue,
      TempLen) then Exit;

    Result := PAnsiChar(TempValue);
  finally
    FreeMem(TempBuf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetFileDescription(const AFileName: string): string;
begin
  Result := abfGetFileDescriptionA(AFileName);
end;

//------------------------------------------------------------------------------

function abfGetFileDescriptionW(const AFileName: WideString): WideString;
type
  PLangAndCodePage = ^TLangAndCodePage;
  TLangAndCodePage = packed record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
var
  TempWnd: DWORD;
  TempBufSize: DWORD;
  TempBuf: Pointer;
  TempTrans: PLangAndCodePage;
  TempLen: DWORD;
  TempFileDesc: WideString;
  TempValue: Pointer;
begin
  if not IsWinNT then
  begin
    Result := abfGetFileDescriptionA(AFileName);
    Exit;
  end;

  Result := '';

  TempBufSize := GetFileVersionInfoSizeW(PWideChar(AFileName), TempWnd);
  if TempBufSize < 1 then Exit;

  GetMem(TempBuf, TempBufSize);
  try
    if not GetFileVersionInfoW(PWideChar(AFileName), TempWnd, TempBufSize,
      TempBuf) then Exit;

    if not VerQueryValueW(TempBuf, PWideChar(WideString('\VarFileInfo\Translation')),
      Pointer(TempTrans), TempLen) then Exit;

    TempFileDesc := Format('\StringFileInfo\%.4x%.4x\FileDescription',
      [TempTrans^.wLanguage, TempTrans^.wCodePage]);

    if not VerQueryValueW(TempBuf, PWideChar(TempFileDesc), TempValue,
      TempLen) then Exit;

    Result := PWideChar(TempValue);
  finally
    FreeMem(TempBuf);
  end;
end;

//------------------------------------------------------------------------------
// Returns resource string for specified language from module instance
// Date: 05/14/2010

function abfLoadStringEx(AInstance: HMODULE; AID: DWORD;
  ALangID: DWORD): WideString;
var
  TempRSRC: HRSRC;
  TempGLOBAL: HGLOBAL;
  TempTable: Pointer;
  TempID: Integer;
  TablePos: PWordArray;
  i: Integer;
begin
  Result := '';
  try
    //Convert the string ID into a block number
    TempRSRC := FindResourceEx(AInstance, RT_STRING,
      MAKEINTRESOURCE((AID shr 4) + 1), ALangID);
    if TempRSRC = 0 then Exit;

    TempGLOBAL := LoadResource(AInstance, TempRSRC);
    if TempGLOBAL = 0 then Exit;
    try
      TempTable := LockResource(TempGLOBAL);
      if not Assigned(TempTable) then Exit;
      try
        TablePos := TempTable;
        TempID := AID mod 16;

        //now walk the string table
        for i := 0 to TempID - 1 do
{$IFDEF WIN32}
          Inc(DWORD(TablePos), SizeOf(TablePos^[0]) * (TablePos^[0] + 1));
{$ENDIF}
{$IFDEF WIN64}
          Inc(NativeInt(TablePos), SizeOf(TablePos^[0]) * (TablePos^[0] + 1));
{$ENDIF}

        SetLength(Result, TablePos^[0]);
        Move(TablePos^[1], Result[1], SizeOf(TablePos^[0]) * TablePos^[0]);
      finally
        UnlockResource(DWORD(TempTable));
      end;
    finally
      FreeResource(TempGLOBAL);
    end;
  except
  end;
end;

//------------------------------------------------------------------------------
// Returns resource string from module instance
// Date: 05/14/2010

function abfLoadString(AInstance: HMODULE; AID: DWORD): WideString;
begin
  Result := abfLoadStringEx(AInstance, AID, 0);
end;


//==============================================================================
// Borland, Delphi/C++Builder utilities
//==============================================================================

//------------------------------------------------------------------------------
// Same to the RegisterClass function, but doesn't generates exception if the
// class is already registered.
// Date: 09/23/2001

procedure abfRegisterClass(AClass: TPersistentClass);
begin
  if not Assigned(AClass) then Exit;
  try
    if GetClass(AClass.ClassName) = nil then RegisterClass(AClass);
  except
  end;
end;

//------------------------------------------------------------------------------
// Same to the RegisterClasses function, but doesn't generates exception if some
// of classese are already registered.
// Date: 09/23/2001

procedure abfRegisterClasses(AClasses: array of TPersistentClass);
var
  i: Integer;
begin
  for i := Low(AClasses) to High(AClasses) do
    abfRegisterClass(AClasses[i]);
end;

//==============================================================================
// Trial and debug routines
//==============================================================================

//------------------------------------------------------------------------------
// Checks is the version trial. Shows trial message if it is.

var
  _CheckTrialCalled: Boolean = False;

procedure abfCheckTrialVersion;
begin
//  if _CheckTrialCalled then Exit;
  _CheckTrialCalled := True;
{$IfDef abfVCLTrial}
  if _ASK then Exit;
  if MessageBoxEx(0, PChar(SabfMsgTrial), PChar(SabfWarning),
    MB_YESNO or MB_ICONWARNING, 0) = IDYES then
    ShellExecute(0, 'open', PChar('http://' + SabfWeb_Register), nil, nil,
      SW_SHOW);
  Halt;
{$EndIf abfVCLTrial}
end;

//------------------------------------------------------------------------------
// Writes S to the log file
// Date: 08/15/2000

procedure abfTrace(const S: string);
{$IfDef abfVCLDebug}
var
  F: Text;
{$EndIf abfVCLDebug}
begin
{$IfDef abfVCLDebug}
  if not abfFileExistsW(abfLogFileName) then Exit;
  try
    AssignFile(F, abfLogFileName);
    try
      Append(F);
      WriteLn(F, Format('%u: %s%s', [GetTickCount, abfTraceIndent, S]));
    finally
      CloseFile(F);
    end;
  except
  end;
{$EndIf abfVCLDebug}
end;

//------------------------------------------------------------------------------
// Writes S to the log file and changes indent. If Indent > 0 indent will be
// increased by Indent spaces. If Indent < 0 indent will be decreased by Indent
// chars.
// Date: 08/15/2000

procedure abfTraceEx(const S: string; Indent: Integer);
{$IfDef abfVCLDebug}
var
  i: Integer;
{$EndIf abfVCLDebug}
begin
{$IfDef abfVCLDebug}
// Remove spaces if need
  if Indent < 0 then SetLength(abfTraceIndent, Length(abfTraceIndent) + Indent);
  abfTrace(S);
// Add spaces if need
  for i := 1 to Indent do abfTraceIndent := abfTraceIndent + ' ';
{$EndIf abfVCLDebug}
end;

//------------------------------------------------------------------------------
// Writes S to the log file with the 'Error: ' prefix
// Date: 08/15/2000

procedure abfTraceError(const S: string);
begin
{$IfDef abfVCLDebug}
  abfTrace('Error: ' + S);
{$EndIf abfVCLDebug}
end;


//==============================================================================
// Routines for initialization/finalization
//==============================================================================

procedure _OSVersionInit;
var
  S: string;
begin
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        S := Trim(Win32CSDVersion);

        case Win32MinorVersion of
          0: if (S = 'B') or (S = 'C') then
               IsWin95OSR2 := True
             else
               IsWin95 := True;
          10: if S = 'A' then
                IsWin98SE := True
              else
                IsWin98 := True;
          90: IsWinME := True;
        end;

        IsWin9x := True;
        Is32bitOS := True;
        IsWin98SEorHigher := IsWin98SE or IsWinME;
        IsWin98orHigher := IsWin98 or IsWin98SEorHigher;
        IsWin95OSR2orHigher := IsWin95OSR2 or IsWin98orHigher;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        IsWinNT := True;
        IsWow64 := abfIsWow64;
        Is32bitOS := OSVersion.Architecture = arX86;
        Is64bitOS := OSVersion.Architecture = arX64;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure _DebugInit;
begin
  abfLogFileName := abfParamStrW(0);
  if abfLogFileName = '' then Exit;
  abfLogFileName := abfChangeFileExtW(abfLogFileName, SabfExt_Log);
{$IfDef abfVCLDebug}
  if not abfFileExistsW(abfLogFileName) then
    FileClose(abfFileCreateW(abfLogFileName));
  abfTrace(Format('** Application started at %s *******************',
    [DateTimeToStr(Now)]));
{$EndIf abfVCLDebug}
end;

//------------------------------------------------------------------------------

procedure _DebugDone;
begin
{$IfDef abfVCLDebug}
  abfTraceEx(Format('** Application finished at %s ******************',
    [DateTimeToStr(Now)]), -255);
{$EndIf abfVCLDebug}
end;

//------------------------------------------------------------------------------

procedure _Kernel32Init;
var
  _LibHandle: THandle;
begin
  _LibHandle := GetModuleHandle(kernel32);
  if _LibHandle <> 0 then
  begin
    @_GetLongPathNameA         := GetProcAddress(_LibHandle,
      'GetLongPathNameA');
    @_GetLongPathNameW         := GetProcAddress(_LibHandle,
      'GetLongPathNameW');
    @_GetNativeSystemInfo      := GetProcAddress(_LibHandle,
      'GetNativeSystemInfo');
    @_GetSystemWow64DirectoryA := GetProcAddress(_LibHandle,
      'GetSystemWow64DirectoryA');
    @_GetSystemWow64DirectoryW := GetProcAddress(_LibHandle,
      'GetSystemWow64DirectoryW');
    @_GetVersionExW            := GetProcAddress(_LibHandle, 'GetVersionExW');
    @_IsWow64Process           := GetProcAddress(_LibHandle, 'IsWow64Process');
    @_LoadLibraryA             := GetProcAddress(_LibHandle, 'LoadLibraryA');
    @_LoadLibraryW             := GetProcAddress(_LibHandle, 'LoadLibraryW');
    @_LoadLibraryExA           := GetProcAddress(_LibHandle, 'LoadLibraryExA');
    @_LoadLibraryExW           := GetProcAddress(_LibHandle, 'LoadLibraryExW');
    @_SetDllDirectoryA         := GetProcAddress(_LibHandle,
      'SetDllDirectoryA');
    @_SetDllDirectoryW         := GetProcAddress(_LibHandle,
      'SetDllDirectoryW');
  end;
end;

//------------------------------------------------------------------------------

procedure _Kernel32Done;
begin
  @_GetLongPathNameA         := nil;
  @_GetLongPathNameW         := nil;
  @_GetNativeSystemInfo      := nil;
  @_GetSystemWow64DirectoryA := nil;
  @_GetSystemWow64DirectoryW := nil;
  @_GetVersionExW            := nil;
  @_IsWow64Process           := nil;
  @_LoadLibraryA             := nil;
  @_LoadLibraryW             := nil;
  @_LoadLibraryExA           := nil;
  @_LoadLibraryExW           := nil;
  @_SetDllDirectoryA         := nil;
  @_SetDllDirectoryW         := nil;
end;

//------------------------------------------------------------------------------

procedure _DoInitialization;
begin
  _Kernel32Init;

  OSVersion := TabfOSVersion.Create;
  _OSVersionInit;

  _DebugInit;

  SAppPath := abfExtractFilePathW(abfParamStrW(0));
end;

//------------------------------------------------------------------------------

procedure _DoFinalization;
begin
  _DebugDone;

  FreeAndNil(OSVersion);

  _Kernel32Done;
end;

{******************************************************************************}
initialization
{******************************************************************************}
  _DoInitialization;

{******************************************************************************}
finalization
{******************************************************************************}
  _DoFinalization;

end{unit abfSysUtils}.
