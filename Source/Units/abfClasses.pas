{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfClasses;

{$I abf.inc}

interface

uses
  Windows, Classes, Messages, SysUtils, {$IFDEF D9}Widestrings,{$ENDIF}
// ABF VCL
  abfConsts;

type
//==============================================================================
// Misc types
//==============================================================================
  TabfChar        = {$IFDEF D9}WideChar       {$ELSE}Char{$ENDIF};
  PabfChar        = {$IFDEF D9}PWideChar      {$ELSE}PChar{$ENDIF};
  TabfWideString  = {$IFDEF D9}WideString     {$ELSE}string{$ENDIF};
  TabfAnsiStrings = TStrings;
  TabfWideStrings = {$IFDEF D9}TWideStrings   {$ELSE}TStrings{$ENDIF};

  TabfIDEVer = (iv_D2, iv_C1, iv_D3, iv_C3, iv_D4, iv_C4, iv_D5, iv_C5,
    iv_D6, iv_C6, iv_D7, iv_D8, iv_D9, iv_D10, iv_C10, iv_D11, iv_C11);
  TabfIDEVers = set of TabfIDEVer;
  TabfRunTimeState  = (rtsNormal, rtsSelected, rtsActive, rtsDisabled);
  TabfRunTimeStates = set of TabfRunTimeState;
  TabfHorzAlign = (haLeft, haCenter, haRight);
  TabfVertAlign = (vaTop, vaCenter, vaBottom);
  TabfDirection = (diHorizontal, diVertical);

  TabfCallBack       = procedure;
  TabfObjectCallBack = procedure of object;
  TabfFreeNotifyProc = procedure(Instance: TComponent) of object;
  TabfMessageEvent = procedure(Sender: TObject; var Msg: TMessage;
    var Handled: Boolean) of object;

  TabfCallbackThunk = packed record
    POPEDX: Byte;
    MOVEAX: Byte;
    SelfPtr: Pointer;
    PUSHEAX: Byte;
    PUSHEDX: Byte;
    JMP: Byte;
    JmpOffset: Integer;
  end;

{$IfNDef D6}
  IInterface = IUnknown;
{$EndIf D6}

//==============================================================================
// EabfException
//==============================================================================

  EabfException = class(Exception);


//==============================================================================
// TabfFileStream
//==============================================================================
// TabfFileStream enables applications to read from and write to a file
// with unicode filename.

  TabfFileStream = class(THandleStream)
  private
    FFileName: WideString;
  public
    constructor Create(const AFileName: WideString; AMode: Word); overload;
    constructor Create(const AFileName: WideString;
      AMode: Word; ARights: Cardinal); overload;
    destructor Destroy; override;
    property FileName: Widestring read FFileName;
  end;


//==============================================================================
// TabfMemoryStream
//==============================================================================
// TabfMemoryStream is a stream that stores its data in dynamic memory.
// The enhanced version of TMemoryStream which allows to load into or save from
// the memory buffer the entire contents of a file with unicode filename.

  TabfMemoryStream = class(TMemoryStream)
  public
    procedure LoadFromFile(const AFileName: WideString); reintroduce; virtual;
    procedure SaveToFile(const AFileName: WideString); reintroduce; virtual;
  end;


//==============================================================================
// TabfResourceStream
//==============================================================================
// TabfResourceStream is a memory stream that provides access to the compiled
// resources in an application.
// The enhanced version of TResourceStream which allows to save  the entire
// contents of the resource stream to a file with unicode filename.

  TabfResourceStream = class(TResourceStream)
  public
    procedure SaveToFile(const AFileName: WideString); reintroduce; virtual;
  end;


//==============================================================================
// TabfAnsiStringList
//==============================================================================
// TabfAnsiStringList maintains a list of ANSI strings.

  TabfAnsiStringList = class(TStringList)
  public
    procedure LoadFromFile(const AFileName: WideString); reintroduce; virtual;
    procedure SaveToFile(const AFileName: WideString); reintroduce; virtual;
  end;


//==============================================================================
// TabfWideStringList
//==============================================================================
// TabfWideStringList maintains a list of wide strings or ANSI strings
// depending on compiler version.

{$IFDEF D9}
  TabfWideStringList = class(TWideStringList)
  public
    procedure LoadFromFile(const AFileName: WideString); reintroduce; virtual;
    procedure SaveToFile(const AFileName: WideString); reintroduce; virtual;
  end;
{$ELSE}
  TabfWideStringList = class(TabfAnsiStringList);
{$ENDIF}

//==============================================================================
// TabfAnsiStringListEx
//==============================================================================
// Extended string list. Names now trimmed from right, Values trimmed from left
// and cut by ','. Added Parameters property (part of Values after the ','
// delimiter).

  TabfAnsiStringListEx = class(TabfAnsiStringList)
  private
    function  GetName(Index: Integer): AnsiString;
    procedure SetName(Index: Integer; const Value: AnsiString);
    function  GetValue(const Name: AnsiString): AnsiString;
    procedure SetValue(const Name, Value: AnsiString);
    function  GetParameter(const Name: AnsiString): AnsiString;
    procedure SetParameter(const Name, Value: AnsiString);
  public
    class function CreateItem(const Name, Value,
      Parameter: AnsiString): AnsiString; // virtual;
    class procedure ParseItem(const S: AnsiString; var Name, Value,
      Parameter: AnsiString); // virtual;
    function ExtractName(const S: AnsiString): AnsiString; virtual;
    function ExtractValue(const S: AnsiString): AnsiString; virtual;
    function ExtractParameter(const S: AnsiString): AnsiString; virtual;
    property Names[Index: Integer]: AnsiString read GetName write SetName;
    property Values[const Name: AnsiString]: AnsiString read GetValue write SetValue;
    property Parameters[const Name: AnsiString]: AnsiString read GetParameter
      write SetParameter;
  end;


//==============================================================================
// TabfLinkedComponent
//==============================================================================
// Use TabfLinkedComponent component in classes that are not descendants of
// TComponent but needs to respond standard Delphi notification routine. Very
// useful for persistents that contain information about some component which
// can be removed from the form or simply destroyed.

  TabfLinkedComponent = class(TComponent)
  protected
    FNotifier: TabfFreeNotifyProc;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor CreateInderect(AOwner: TComponent;
      ANotifier: TabfFreeNotifyProc); virtual;
    property Notifier: TabfFreeNotifyProc read FNotifier write FNotifier;
  end;


//==============================================================================
// TabfChannel
//==============================================================================
// A class for easy data exchanging between applications and/or dll. Based on
// Alexander Kramarenko <http://www.akhome.da.ru> TChannel object. Simply
// specify the Name of chanel, pointer to Data structure and the Data size. If
// the channel with specified name doesn't exist it will be created, otherwise
// the instance will be connected to the existed channel.

  EabfChannel = class(EabfException);

  TabfChannel = class(TObject)
  protected
    FName: AnsiString;
    FMemory: Pointer;
    FObject: Integer;
    FSize: Integer;
    procedure Close; virtual;
  public
    constructor Create(const Name: AnsiString; var Data; Size: Integer); virtual;
    destructor Destroy; override;
    procedure Clean; virtual;
  end;


//==============================================================================
// TabfCustomVersion
//==============================================================================
// TabfCustomVersion is a class that enables you to store the version
// number of a file or a product

  TabfCustomVersion = class
  private
    FMajor: Word;
    FMinor: Word;
    FRelease: Word;
    FBuild: Word;
  protected
    procedure Assign(AVersion: TabfCustomVersion); virtual;
    procedure Clear; virtual;
    function CompareWith(AVersion: TabfCustomVersion): Integer;
    function GetText: AnsiString; virtual;
    procedure SetVersion(AMajor: Word; AMinor: Word = 0; ARelease: Word = 0;
      ABuild: Word = 0); virtual;
  public
    constructor Create; overload;
    constructor Create(AMajor: Word; AMinor: Word = 0; ARelease: Word = 0;
      ABuild: Word = 0); overload;
    constructor Create(const AVersionString: AnsiString); overload;
    property Major: Word read FMajor;
    property Minor: Word read FMinor;
    property Release: Word read FRelease;
    property Build: Word read FBuild;
    property Text: AnsiString read GetText;
  end;


//==============================================================================
// TabfVersion
//==============================================================================
// TabfVersion is a class that enables you to store the version number of a file
// or a product

  TabfVersion = class(TabfCustomVersion)
  public
    procedure Assign(AVersion: TabfVersion); reintroduce; virtual;
    procedure Clear; override;
    function CompareWith(AVersion: TabfVersion): Integer; reintroduce;
    procedure SetVersion(AMajor: Word; AMinor: Word = 0; ARelease: Word = 0;
      ABuild: Word = 0); override;
  end;


//==============================================================================
// TabfFileVersionInfo
//==============================================================================
// TabfFileVersionInfo is a class that enables you to read the version
// information from an executable file (exe, dll etc.). Besides allowing read
// standard version key such as ProductName and FileVersion this class also
// allows you to read custom keys through the UserKeys property. Based on
// TJclFileVersionInfo class from JCL <http://www.delphi-jedi.org>

  TabfFileVersionFlag = (fvfDebug, fvfInfoInferred, fvfPatched, fvfPreRelease,
    fvfPrivateBuild, fvfSpecialBuild);
  TabfFileVersionFlags = set of TabfFileVersionFlag;

  EabfFileVersionInfo = class(EabfException);
  TabfFileVersion = class(TabfCustomVersion);

  TabfFileVersionInfo = class(TObject)
    FBinFileVersion: TabfFileVersion;
    FBinProductVersion: TabfFileVersion;
    FBuffer: AnsiString;
    FFileFlags: TabfFileVersionFlags;
    FFileName: WideString;
    FFixedBuffer: PVSFixedFileInfo;
    FLanguageIndex: Integer;
    FLanguages: TStrings;
    procedure InitBinVersions;
    procedure InitFileFlags;
  protected
    procedure GetVersionInfo;
    procedure ExtractLanguageIds;
  // Properties Get/Set
    function GetFileOS: LongWord;
    function GetFileSubType: LongWord;
    function GetFileType: LongWord;
    function GetLanguageCount: Integer;
    function GetLanguageIds(Index: Integer): AnsiString;
    function GetLanguageNames(Index: Integer): WideString;
    procedure SetLanguageIndex(A: Integer);
    function GetUserKey(const Key: WideString): WideString;
    function GetVersionKeyValue(Index: Integer): WideString;
  public
    constructor Create(const AFileName: WideString); overload; virtual;
    constructor Create(const AInstanceHandle: HINST); overload; virtual;
    constructor CreateFromBlock(const Buffer: AnsiString; Dummy: Integer);
    destructor Destroy; override;
    class function VersionInfoExists(const AFileName: WideString): Boolean; overload;
    class function VersionInfoExists(const AInstanceHandle: HINST): Boolean; overload;
  // Properties
    property BinFileVersion: TabfFileVersion read FBinFileVersion;
    property BinProductVersion: TabfFileVersion read FBinProductVersion;
    property Comments: WideString index 1 read GetVersionKeyValue;
    property CompanyName: WideString index 2 read GetVersionKeyValue;
    property FileDescription: WideString index 3 read GetVersionKeyValue;
    property FileFlags: TabfFileVersionFlags read FFileFlags;
    property FileOS: LongWord read GetFileOS;
    property FileSubType: LongWord read GetFileSubType;
    property FileType: LongWord read GetFileType;
    property FileVersion: WideString index 4 read GetVersionKeyValue;
    property InternalName: WideString index 5 read GetVersionKeyValue;
    property LanguageCount: Integer read GetLanguageCount;
    property LanguageIndex: Integer read FLanguageIndex write SetLanguageIndex;
    property LanguageIds[Index: Integer]: AnsiString read GetLanguageIds;
    property LanguageNames[Index: Integer]: WideString read GetLanguageNames;
    property LegalCopyright: WideString index 6 read GetVersionKeyValue;
    property LegalTradeMarks: WideString index 7 read GetVersionKeyValue;
    property OriginalFilename: WideString index 8 read GetVersionKeyValue;
    property ProductName: WideString index 9 read GetVersionKeyValue;
    property ProductVersion: WideString index 10 read GetVersionKeyValue;
    property SpecialBuild: WideString index 11 read GetVersionKeyValue;
    property PrivateBuild: WideString index 12 read GetVersionKeyValue;
    property UserKeys[const Key: WideString]: WideString read GetUserKey;
  end;


//==============================================================================
// TabfThread
//==============================================================================
// Enhanced thread, Terminate property True after execution. Also has Finished
// property

  TabfThread = class(TThread)
  private
    FOnFinish: TNotifyEvent;
  protected
    FFinished: Boolean;
    procedure Execute; override;
    procedure Finish; virtual;
    procedure DoFinish; dynamic;
  public
    property Terminated;
    property Finished: Boolean read FFinished;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;


//==============================================================================
// TabfEventThread
//==============================================================================
// A thread that have OnExecute event.

  TabfEventThread = class(TabfThread)
  private
    FOnExecute: TNotifyEvent;
    FSynchronized: Boolean;
  protected
    procedure Execute; override;
    procedure DoExecute; virtual;
  public
  // Properties
    property Synchronized: Boolean read FSynchronized write FSynchronized;
  // Events
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;


//==============================================================================
// TabfTimerThread
//==============================================================================
// A thread for creating timers.

  TabfTimerThread = class(TabfEventThread)
  private
    FInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
  protected
    FStopHandle: THandle;
    procedure Execute; override;
    procedure Stop; virtual;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
  // Properties
    property Interval: Cardinal read FInterval write SetInterval;
    property StopHandle: THandle read FStopHandle;
  end;


//==============================================================================
// TabfFolderMonitorThread
//==============================================================================
// Tread class for folder monitoring.

  TabfFolderMonitorThread = class(TabfThread)
  private
    FMutex, FWaitHandle: THandle;
    FDirectory: WideString;
    FWatchSubTree: Boolean;
    FWaitChanged : Boolean;
    FNotifyOptionFlags: DWORD;
    FChangeEvent: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(ChangeEvent: TThreadMethod); virtual;
    destructor Destroy; override;
    procedure SetDirectoryOptions(ADir: WideString; AWatchSubTree: Boolean;
      ANotifyOptionFlags: DWORD);
  // Properties
    property Mutex: THandle read FMutex;
    property Terminated;
    property WaitHandle: THandle read FWaitHandle;
    property ChangeEvent: TThreadMethod read FChangeEvent write FChangeEvent;
  end;


//==============================================================================
// TabfRegistryMonitorThread
//==============================================================================
// Tread class for registry monitoring.

  TabfRegistryMonitorThread = class(TabfThread)
  private
    FMutex, FWaitHandle, FWaitRootHandle: THandle;
    FRegistryKey: AnsiString;
    FRootKey: HKEY;
    FKey: HKEY;
    FKeyExists: Boolean;
    FWatchSubTree: Boolean;
    FWaitChanged : Boolean;
    FNotifyOptionFlags: DWORD;
    FChangeEvent: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(ChangeEvent: TThreadMethod); virtual;
    destructor Destroy; override;
    procedure SetRegistryKeyOptions(ARootKey: HKEY; AKey: AnsiString;
      AWatchSubTree: Boolean; ANotifyOptionFlags: DWORD);
  // Properties
    property Mutex: THandle read FMutex;
    property Terminated;
    property WaitHandle: THandle read FWaitHandle;
    property ChangeEvent: TThreadMethod read FChangeEvent write FChangeEvent;
  end;


//==============================================================================
// TabfCustomCopyThread
//==============================================================================
// Prototype of threads that provides a "Copy" operation asynchronously to the
// main VCL thread. SrcName specifies a source name of copying file/data.
// DstName specifies a destination name of copying file/data. You should
// implement Copy and CalcTotalSize methods in descendants. Copy method provides
// a main copying routine. CalcTotalSize is used to determine a TotalSize of the
// copying data. Use DoProgress or FireProgressEvent periodically in the Copy
// method. Setting of the Error property to value different then 0, terminates
// the thread and sets the ErrorMessage property. Use ErrorMessage to get a
// readable information about the error.

  TabfCopyThreadErrorEvent = procedure(Sender: TObject; ErrorCode: LongWord;
    const ErrorMessage: AnsiString) of object;

  TabfCopyThreadProgressEvent = procedure(Sender: TObject; CopiedSize,
    TotalSize: Int64; var Terminate: Boolean) of object;

  TabfCustomCopyThread = class(TabfThread)
  private
    FErrorCode: LongWord;
    FOnError: TabfCopyThreadErrorEvent;
    FOnProgress: TabfCopyThreadProgressEvent;
  protected
    FSrcName: WideString;
    FDstName: WideString;
    FCopiedSize: Int64;
    FTotalSize: Int64;
    FExtraData: Pointer;
  // Main routines
    procedure CalcTotalSize; virtual; abstract; // Should set FTotalSize value.
    procedure Copy; virtual; abstract; // Main routine, override this.
  // Event Handlers
    procedure FireErrorEvent; virtual;
    procedure DoError(ErrorCode: LongWord; const ErrorMessage: AnsiString); dynamic;
    procedure FireProgressEvent; virtual;
    procedure DoProgress(CopiedSize, TotalSize: LongWord); dynamic;
  // Properties Get/Set
    procedure SetSrcName(const A: WideString); virtual;
    procedure SetDstName(const A: WideString); virtual;
    procedure SetErrorCode(A: LongWord); virtual;
    function GetErrorMessage: AnsiString; virtual;
  // Properties
    property ErrorCode: LongWord read FErrorCode write SetErrorCode;
    property ErrorMessage: AnsiString read GetErrorMessage;
  // Events
    property OnError: TabfCopyThreadErrorEvent read FOnError write FOnError;
    property OnProgress: TabfCopyThreadProgressEvent read FOnProgress
      write FOnProgress;
  public
    constructor Create(const ASrcName, ADstName: WideString; CreateSuspended,
      AFreeOnTerminate: Boolean; AOnError: TabfCopyThreadErrorEvent;
      AOnProgress: TabfCopyThreadProgressEvent; AOnFinish: TNotifyEvent;
      const ExtraData: Pointer); virtual;
  // Main routines
    procedure Execute; override; // Override the Copy method, not Execute.
    procedure Cancel; virtual;
  // Properties
    property DstName: WideString read FDstName write SetDstName;
    property SrcName: WideString read FSrcName write SetSrcName;
    property TotalSize: Int64 read FTotalSize;
    property CopiedSize: Int64 read FCopiedSize;
    property Terminated;
  end;{TabfCustomCopyThread = class(TThread)}

  TabfCopyThreadClass = class of TabfCustomCopyThread;

//==============================================================================
// TabfLocalCopyThread
//==============================================================================
// Provides asynchronous local file copying.

  TabfLocalCopyThread = class(TabfCustomCopyThread)
  protected
    procedure CalcTotalSize; override;
    procedure Copy; override;
  public
  // Properties
    property DstName;
    property SrcName;
    property TotalSize;
    property CopiedSize;
    property ErrorCode;
    property ErrorMessage;
  // Events
    property OnError;
    property OnProgress;
  end;

//==============================================================================
// Auxiliary consts
//==============================================================================

const
  CabfIDERegPaths: array[TabfIDEVer] of AnsiString = (
    SabfRegSoftwareBorland + SabfDelphi  + '\2.0',
    SabfRegSoftwareBorland + SabfBuilder + '\1.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\3.0',
    SabfRegSoftwareBorland + SabfBuilder + '\3.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\4.0',
    SabfRegSoftwareBorland + SabfBuilder + '\4.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\5.0',
    SabfRegSoftwareBorland + SabfBuilder + '\5.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\6.0',
    SabfRegSoftwareBorland + SabfBuilder + '\6.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\7.0',
    SabfRegSoftwareBorland + SabfBDS     + '\1.0',
    SabfRegSoftwareBorland + SabfBDS     + '\3.0',
    SabfRegSoftwareBorland + SabfBDS     + '\4.0',
    SabfRegSoftwareBorland + SabfBDS     + '\4.0',
    SabfRegSoftwareBorland + SabfBDS     + '\5.0',
    SabfRegSoftwareBorland + SabfBDS     + '\5.0'
  );

  CabfCallbackThunk: TabfCallbackThunk = (
    POPEDX:    $5A;
    MOVEAX:    $B8;
    SelfPtr:   nil;        // Pointer to Self
    PUSHEAX:   $50;
    PUSHEDX:   $52;
    JMP:       $E9;
    JmpOffset: $00000000); // Offset to callback method


//==============================================================================
// Misc functions
//==============================================================================

function abfFillCallbackThunk(var ACallbackThunk: TabfCallbackThunk;
  AObject: TObject; AMethod: TabfObjectCallBack): Boolean;


{******************************************************************************}
implementation
{******************************************************************************}

uses
// ABF VCL
  abfVclConsts, abfSysUtils, abfRegistry;


//==============================================================================
// TabfFileStream
//==============================================================================
// TabfFileStream enables applications to read from and write to a file
// with unicode filename.

constructor TabfFileStream.Create(const AFileName: WideString; AMode: Word);
begin
  Create(AFileName, AMode, 0);
end;

//------------------------------------------------------------------------------

constructor TabfFileStream.Create(const AFileName: WideString; AMode: Word;
  ARights: Cardinal);
begin
  if AMode = fmCreate then
  begin
    inherited Create(abfFileCreateW(AFileName, ARights));
    if Handle < 0 then
      raise EFCreateError.CreateResFmt(@SabfFCreateErrorEx,
        [abfExpandFileNameW(AFileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
    inherited Create(abfFileOpenW(AFileName, AMode));
    if Handle < 0 then
      raise EFOpenError.CreateResFmt(@SabfFOpenErrorEx,
        [abfExpandFileNameW(AFileName), SysErrorMessage(GetLastError)]);
  end;

  FFileName := AFileName;
end;

//------------------------------------------------------------------------------

destructor TabfFileStream.Destroy;
begin
  if Handle >= 0 then FileClose(Handle);
  inherited Destroy;
end;


//==============================================================================
// TabfMemoryStream
//==============================================================================
// TabfMemoryStream is a stream that stores its data in dynamic memory.
// The enhanced version of TMemoryStream which allows to load into or save from
// the memory buffer the entire contents of a file with unicode filename.

procedure TabfMemoryStream.LoadFromFile(const AFileName: WideString);
var
  Stream: TStream;
begin
  Stream := TabfFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfMemoryStream.SaveToFile(const AFileName: WideString);
var
  Stream: TStream;
begin
  Stream := TabfFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//==============================================================================
// TabfResourceStream
//==============================================================================
// TabfResourceStream is a memory stream that provides access to the compiled
// resources in an application.
// The enhanced version of TResourceStream which allows to save  the entire
// contents of the resource stream to a file with unicode filename.

procedure TabfResourceStream.SaveToFile(const AFileName: WideString);
var
  Stream: TStream;
begin
  Stream := TabfFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//==============================================================================
// TabfAnsiStringList
//==============================================================================
// TabfAnsiStringList maintains a list of ANSI strings.

procedure TabfAnsiStringList.LoadFromFile(const AFileName: WideString);
var
  Stream: TStream;
begin
  Stream := TabfFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAnsiStringList.SaveToFile(const AFileName: WideString);
var
  Stream: TStream;
begin
  Stream := TabfFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//==============================================================================
// TabfWideStringList
//==============================================================================
// TabfWideStringList maintains a list of wide strings or ANSI strings
// depending on compiler version.

{$IFDEF D9}
procedure TabfWideStringList.LoadFromFile(const AFileName: WideString);
var
  Stream: TStream;
begin
  Stream := TabfFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWideStringList.SaveToFile(const AFileName: WideString);
var
  Stream: TStream;
begin
  Stream := TabfFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}


//==============================================================================
// TabfAnsiStringListEx
//==============================================================================
// Extended string list. Names now trimmed from right, Values trimmed from left
// and cut by ','. Added Parameters property (part of Values after the ','
// delimiter).
// Date: 09/01/2000

class function TabfAnsiStringListEx.CreateItem(const Name, Value,
  Parameter: AnsiString): AnsiString;
begin
  Result := Name + '=' + Value;
  if Parameter <> '' then Result := Result  + ',' + Parameter;
end;

//------------------------------------------------------------------------------

class procedure TabfAnsiStringListEx.ParseItem(const S: AnsiString; var Name, Value,
  Parameter: AnsiString);
var
  i: Integer;
begin
  Value := '';
  Parameter := '';
// Get Name
  Name := Trim(S);
  i := Pos('=', Name);
  if i <= 0 then Exit;
  Value := Name;
  System.Delete(Name, i, MaxInt);
  Name := Trim(Name);
// Get Value
  System.Delete(Value, 1, i);
  Value := Trim(Value);
  i := Pos(',', Value);
  if i <= 0 then Exit;
  Parameter := Value;
  System.Delete(Value, i, MaxInt);
  Value := Trim(Value);
// Get Parameters
  System.Delete(Parameter, 1, i);
  Parameter := Trim(Parameter);
end;

//------------------------------------------------------------------------------

function TabfAnsiStringListEx.ExtractName(const S: AnsiString): AnsiString;
var
  S1, S2: AnsiString;
begin
  ParseItem(S, Result, S1, S2);
end;

//------------------------------------------------------------------------------

function TabfAnsiStringListEx.ExtractValue(const S: AnsiString): AnsiString;
var
  S1, S2: AnsiString;
begin
  ParseItem(S, S1, Result, S2);
end;

//------------------------------------------------------------------------------

function TabfAnsiStringListEx.ExtractParameter(const S: AnsiString): AnsiString;
var
  S1, S2: AnsiString;
begin
  ParseItem(S, S1, S2, Result);
end;

//------------------------------------------------------------------------------

function TabfAnsiStringListEx.GetName(Index: Integer): AnsiString;
begin
  Result := ExtractName(Get(Index));
end;

//------------------------------------------------------------------------------

procedure TabfAnsiStringListEx.SetName(Index: Integer; const Value: AnsiString);
var
  S, N, V, P: AnsiString;
begin
  S := Get(Index);
  ParseItem(S, N, V, P);
  Put(Index, CreateItem(Value, V, P));
end;

//------------------------------------------------------------------------------

function TabfAnsiStringListEx.GetValue(const Name: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  i := IndexOfName(Name);
  if i < 0 then Exit;
  Result := ExtractValue(Get(i));
end;

//------------------------------------------------------------------------------

procedure TabfAnsiStringListEx.SetValue(const Name, Value: AnsiString);
var
  i: Integer;
begin
  i := IndexOfName(Name);
  if i < 0 then i := Add(Name); // Add new item
  Put(i, CreateItem(Name, Value, Parameters[Name]));
end;

//------------------------------------------------------------------------------

function TabfAnsiStringListEx.GetParameter(const Name: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  i := IndexOfName(Name);
  if i < 0 then Exit;
  Result := ExtractParameter(Get(i));
end;

//------------------------------------------------------------------------------

procedure TabfAnsiStringListEx.SetParameter(const Name, Value: AnsiString);
var
  i: Integer;
begin
  i := IndexOfName(Name);
  if i < 0 then i := Add(Name); // Add new item
  Put(i, CreateItem(Name, Values[Name], Value));
end;


//==============================================================================
// TabfLinkedComponent
//==============================================================================
// Use TabfLinkedComponent component in classes that are not descendants of
// TComponent but needs to respond standard Delphi notification routine. Very
// useful for persistents that contain information about some component which
// can be removed from the form or simply destroyed.
// Date: 02/23/2000

constructor TabfLinkedComponent.CreateInderect(AOwner: TComponent;
  ANotifier: TabfFreeNotifyProc);
begin
  FNotifier := ANotifier;
  Create(AOwner);
end;

//------------------------------------------------------------------------------

procedure TabfLinkedComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = OpRemove) and (Assigned(FNotifier)) then
    FNotifier(AComponent);
end;


//==============================================================================
// TabfChannel
//==============================================================================
// A class for easy data exchanging between applications and/or dll. Based on
// Alexander Kramarenko <http://www.akhome.da.ru> TChannel object. Simply
// specify the Name of chanel, pointer to Data structure and the Data size. If
// the channel with specified name doesn't exist it will be created, otherwise
// the instance will be connected to the existed channel.
// Date: 08/15/2000

constructor TabfChannel.Create(const Name: AnsiString; var Data; Size: Integer);
begin
  Pointer(Data) := nil;
  FName := Name;
  FSize := Size;

// Crate a file-mapping object
  FObject := CreateFileMappingA($FFFFFFFF, nil, PAGE_READWRITE, 0,
    FSize, PAnsiChar(FName));
  if DWORD(FObject) = INVALID_HANDLE_VALUE then
    raise EabfChannel.Create(SabfChannel_CreationFailed);

// Maps a view of a file into the address space
  FMemory := MapViewOfFile(FObject, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if GetLastError <> ERROR_ALREADY_EXISTS then Clean;
  if FMemory = nil then
  begin
    CloseHandle(FObject);
    raise EabfChannel.Create(SabfChannel_MappingFailed);
  end;

// Assign a mapped memory to the Data 
  Pointer(Data) := FMemory;
end;

//------------------------------------------------------------------------------

destructor TabfChannel.Destroy;
begin
  inherited Destroy;
  Close;
end;

//------------------------------------------------------------------------------

procedure TabfChannel.Clean;
begin
  if FMemory = nil then Exit;
  FillChar(FMemory^, FSize, 0);
end;

//------------------------------------------------------------------------------

procedure TabfChannel.Close;
begin
  UnMapViewOfFile(FMemory);
  CloseHandle(FObject);
  FName := '';
end;

//==============================================================================
// TabfCustomVersion
//==============================================================================
// TabfCustomVersion is a class that enables you to store the version
// number of a file or a product
// Date: 04/02/2014

constructor TabfCustomVersion.Create;
begin
  inherited Create;

  FMajor := 0;
  FMinor := 0;
  FRelease := 0;
  FBuild := 0;
end;

//------------------------------------------------------------------------------

constructor TabfCustomVersion.Create(AMajor: Word; AMinor: Word;
  ARelease: Word; ABuild: Word);
begin
  Create;

  SetVersion(AMajor, AMinor, ARelease, ABuild);
end;

//------------------------------------------------------------------------------

constructor TabfCustomVersion.Create(const AVersionString: AnsiString);
begin
  Create;

  abfStrToVersion(AVersionString, Self);
end;

//------------------------------------------------------------------------------

procedure TabfCustomVersion.Assign(AVersion: TabfCustomVersion);
begin
  if AVersion is TabfCustomVersion then
    with AVersion do
      Self.SetVersion(Major, Minor, Release, Build);
end;

//------------------------------------------------------------------------------

procedure TabfCustomVersion.Clear;
begin
  SetVersion(0, 0, 0, 0);
end;

//------------------------------------------------------------------------------

function TabfCustomVersion.CompareWith(AVersion: TabfCustomVersion): Integer;
begin
  Result := abfCompareVersion(Self, AVersion);
end;

//------------------------------------------------------------------------------

function TabfCustomVersion.GetText: AnsiString;
begin
  Result := Format('%u.%u.%u.%u', [FMajor, FMinor, FRelease, FBuild]);
end;

//------------------------------------------------------------------------------

procedure TabfCustomVersion.SetVersion(AMajor: Word; AMinor: Word;
  ARelease: Word; ABuild: Word);
begin
  FMajor   := AMajor;
  FMinor   := AMinor;
  FRelease := ARelease;
  FBuild   := ABuild;
end;


//==============================================================================
// TabfVersion
//==============================================================================
// TabfVersion is a class that enables you to store the version number of a file
// or a product
// Date: 04/02/2014

procedure TabfVersion.Assign(AVersion: TabfVersion);
begin
  inherited Assign(AVersion);
end;

//------------------------------------------------------------------------------

procedure TabfVersion.Clear;
begin
  inherited Clear;
end;

//------------------------------------------------------------------------------

function TabfVersion.CompareWith(AVersion: TabfVersion): Integer;
begin
  Result := inherited CompareWith(AVersion);
end;

//------------------------------------------------------------------------------

procedure TabfVersion.SetVersion(AMajor: Word; AMinor: Word; ARelease: Word;
  ABuild: Word);
begin
  inherited SetVersion(AMajor, AMinor, ARelease, ABuild);
end;


//==============================================================================
// TabfFileVersionInfo
//==============================================================================
// TabfFileVersionInfo is a class that enables you to read the version
// information from an executable file (exe, dll etc.). Besides allowing read
// standard version key such as ProductName and FileVersion this class also
// allows you to read custom keys through the UserKeys property. Based on
// TJclFileVersionInfo class from JCL <http://www.delphi-jedi.org>
// Date: 09/07/2000

const
  cVerFixedInfo:   WideString = '\';
  cVerTranslation: WideString = '\VarFileInfo\Translation';
  cVerStringInfo:  WideString = '\StringFileInfo\';

  cVerKeyNames: array [1..12] of WideString = (
    'Comments',
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'LegalCopyright',
    'LegalTradeMarks',
    'OriginalFilename',
    'ProductName',
    'ProductVersion',
    'SpecialBuild',
    'PrivateBuild');

//------------------------------------------------------------------------------

type
  PLangIdRec = ^TLangIdRec;
  TLangIdRec = packed record
    case Integer of
    0: (
      LangId: Word;
      CodePage: Word);
    1: (
      Pair: LongWord);
  end;

//------------------------------------------------------------------------------

constructor TabfFileVersionInfo.Create(const AFileName: WideString);
var
  Size: UINT;
begin
  FBinFileVersion := TabfFileVersion.Create;
  FBinProductVersion := TabfFileVersion.Create;
  FFileName := AFileName;
  FFileFlags := [];
  FLanguages := TStringList.Create;
  GetVersionInfo;
  if IsWinNT then
    Win32Check(VerQueryValueW(Pointer(FBuffer), PWideChar(cVerFixedInfo),
      Pointer(FFixedBuffer), Size))
  else
    Win32Check(VerQueryValue(Pointer(FBuffer), PAnsiChar(AnsiString(cVerFixedInfo)),
      Pointer(FFixedBuffer), Size));
  if Size < SizeOf(TVSFixedFileInfo) then RaiseLastWin32Error;
  ExtractLanguageIds;
  InitBinVersions;
  InitFileFlags;
end;

//------------------------------------------------------------------------------

constructor TabfFileVersionInfo.Create(const AInstanceHandle: HINST);
begin
  Create(abfGetModuleFileNameW(AInstanceHandle));
end;

//------------------------------------------------------------------------------

constructor TabfFileVersionInfo.CreateFromBlock(const Buffer: AnsiString;
  Dummy: Integer);
var
  Size: UINT;
begin
  FBinFileVersion := TabfFileVersion.Create;
  FBinProductVersion := TabfFileVersion.Create;
  FFileName := '';
  FFileFlags := [];
  FLanguages := TStringList.Create;
  FBuffer := Buffer;
  if IsWinNT then
    Win32Check(VerQueryValueW(PAnsiChar(FBuffer), PWideChar(cVerFixedInfo),
      Pointer(FFixedBuffer), Size))
  else
    Win32Check(VerQueryValue(PAnsiChar(FBuffer), PAnsiChar(AnsiString(cVerFixedInfo)),
      Pointer(FFixedBuffer), Size));
  if Size < SizeOf(TVSFixedFileInfo) then RaiseLastWin32Error;
  ExtractLanguageIds;
  InitBinVersions;
  InitFileFlags;
end;

//------------------------------------------------------------------------------

destructor TabfFileVersionInfo.Destroy;
begin
  FreeAndNil(FLanguages);
  FreeAndNil(FBinProductVersion);
  FreeAndNil(FBinFileVersion);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

class function TabfFileVersionInfo.VersionInfoExists(
  const AFileName: WideString): Boolean;
var
  Handle: DWORD;  
begin
  if IsWinNT then
    Result := GetFileVersionInfoSizeW(PWideChar(AFileName), Handle) > 0
  else
    Result := GetFileVersionInfoSize(PAnsiChar(AnsiString(AFileName)), Handle) > 0;
end;

//------------------------------------------------------------------------------

class function TabfFileVersionInfo.VersionInfoExists(
  const AInstanceHandle: HINST): Boolean;
begin
  Result := VersionInfoExists(abfGetModuleFileNameW(AInstanceHandle));
end;

//------------------------------------------------------------------------------

procedure TabfFileVersionInfo.InitBinVersions;
begin
  if Assigned(FFixedBuffer) then
    with FFixedBuffer^ do
    begin
      if Assigned(FBinFileVersion) then
        FBinFileVersion.SetVersion(
          HiWord(dwFileVersionMS),
          LoWord(dwFileVersionMS),
          HiWord(dwFileVersionLS),
          LoWord(dwFileVersionLS)
        );

      if Assigned(FBinProductVersion) then
        FBinProductVersion.SetVersion(
          HiWord(dwProductVersionMS),
          LoWord(dwProductVersionMS),
          HiWord(dwProductVersionLS),
          LoWord(dwProductVersionLS)
        );
    end
  else
    begin
      if Assigned(FBinFileVersion) then
        FBinFileVersion.Clear;
      if Assigned(FBinProductVersion) then
        FBinProductVersion.Clear;
    end;
end;

//------------------------------------------------------------------------------

procedure TabfFileVersionInfo.InitFileFlags;
var
  F: LongWord;
begin
  F := FFixedBuffer^.dwFileFlags and FFixedBuffer^.dwFileFlagsMask;
  if (F and VS_FF_DEBUG       ) <> 0 then Include(FFileFlags, fvfDebug);
  if (F and VS_FF_INFOINFERRED) <> 0 then Include(FFileFlags, fvfInfoInferred);
  if (F and VS_FF_PATCHED     ) <> 0 then Include(FFileFlags, fvfPatched);
  if (F and VS_FF_PRERELEASE  ) <> 0 then Include(FFileFlags, fvfPreRelease);
  if (F and VS_FF_PRIVATEBUILD) <> 0 then Include(FFileFlags, fvfPrivateBuild);
  if (F and VS_FF_SPECIALBUILD) <> 0 then Include(FFileFlags, fvfSpecialBuild);
end;

//------------------------------------------------------------------------------

procedure TabfFileVersionInfo.GetVersionInfo;
var
  Size: LongWord;
  Handle: DWORD;
begin
  if IsWinNT then
    Size := GetFileVersionInfoSizeW(PWideChar(FFileName), Handle)
  else
    Size := GetFileVersionInfoSize(PAnsiChar(AnsiString(FFileName)), Handle);
  if Size = 0 then raise EabfFileVersionInfo.Create(SabfFileVersionInfo_NoInfo);
  SetLength(FBuffer, Size);
  if IsWinNT then
    Win32Check(GetFileVersionInfoW(PWideChar(FFileName), Handle, Size,
      PAnsiChar(FBuffer)))
  else
    Win32Check(GetFileVersionInfo(PAnsiChar(AnsiString(FFileName)), Handle, Size,
      PAnsiChar(FBuffer)));
end;

//------------------------------------------------------------------------------

procedure TabfFileVersionInfo.ExtractLanguageIds;
var
  i: Integer;
  Translation: PLongInt;
  Lang: TLangIdRec;
  Size: UINT;
  Res: BOOL;
begin
  if IsWinNT then
    Res := VerQueryValueW(PAnsiChar(FBuffer), PWideChar(cVerTranslation),
      Pointer(Translation), Size)
  else
    Res := VerQueryValue(PAnsiChar(FBuffer), PAnsiChar(AnsiString(cVerTranslation)),
      Pointer(Translation), Size);

  if Res then
    for i := 0 to (Size div 4) - 1 do
    begin
      Lang := PLangIdRec(Longint(Translation) + (I * 4))^;
      FLanguages.AddObject(Format('%.4x%.4x', [Lang.LangId,
        Lang.CodePage]), TObject(Lang.Pair));
    end;
// If no translations or neutral lang ID then English (United States)
  if (FLanguages.Count = 0) then FLanguages.Add('040904E4');
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetFileOS: LongWord;
begin
  Result := FFixedBuffer^.dwFileOS;
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetFileSubType: LongWord;
begin
  Result := FFixedBuffer^.dwFileSubType;
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetFileType: LongWord;
begin
  Result := FFixedBuffer^.dwFileType;
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetLanguageCount: Integer;
begin
  Result := FLanguages.Count;
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetLanguageIds(Index: Integer): AnsiString;
begin
  Result := FLanguages[Index];
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetLanguageNames(Index: Integer): WideString;
var
  D: LongWord;
  Buf: Pointer;
  BufSize: DWORD;
begin
  D := Integer(Flanguages.Objects[Index]);

  BufSize := (MAX_PATH + 1) * 2;
  GetMem(Buf, BufSize);
  try
    if IsWinNT then
    begin
      VerLanguageNameW(LoWord(D), PWideChar(Buf), BufSize);
      Result := WideString(PWideChar(Buf));
    end else
    begin
      VerLanguageName(LoWord(D), PAnsiChar(Buf), BufSize);
      Result := AnsiString(PAnsiChar(Buf));
    end;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetUserKey(const Key: WideString): WideString;
var
  WS: WideString;
  P: Pointer;
  Size: UINT;
begin
  Result := '';

  WS := cVerStringInfo + FLanguages[0] + '\' + Key;

  if IsWinNT then
  begin
    if VerQueryValueW(PAnsiChar(FBuffer), PWideChar(WS), P, Size) then
      Result := WideString(PWideChar(P));
  end else
  begin
    if VerQueryValue(PAnsiChar(FBuffer), PAnsiChar(AnsiString(WS)), P, Size) then
      Result := AnsiString(PAnsiChar(P));
  end;
end;

//------------------------------------------------------------------------------

function TabfFileVersionInfo.GetVersionKeyValue(Index: Integer): WideString;
var
  WS: WideString;
  P: Pointer;
  Size: UINT;
begin
  Result := '';

  WS := cVerStringInfo + FLanguages[0] + '\' + cVerKeyNames[Index];

  if IsWinNT then
  begin
    if VerQueryValueW(PAnsiChar(FBuffer), PWideChar(WS), P, Size) then
      Result := WideString(PWideChar(P));
  end else
  begin
    if VerQueryValue(PAnsiChar(FBuffer), PAnsiChar(AnsiString(WS)), P, Size) then
      Result := AnsiString(PAnsiChar(P));
  end;
end;

//------------------------------------------------------------------------------

procedure TabfFileVersionInfo.SetLanguageIndex(A: Integer);
begin
  if (A >= 0) and (A < FLanguages.Count) then FLanguageIndex := A
  else raise EabfFileVersionInfo.Create(SabfFileVersionInfo_WrongLngIndex);
end;


//==============================================================================
// TabfThread
//==============================================================================
// Enhanced thread, Terminate property True after execution. Also has Finished
// property

procedure TabfThread.Execute;
begin
//  Finish;
end;

//------------------------------------------------------------------------------

procedure TabfThread.Finish;
begin
  Terminate;
  FFinished := True;
  if Assigned(OnFinish) then Synchronize(DoFinish);
end;

//------------------------------------------------------------------------------

procedure TabfThread.DoFinish;
begin
  if Assigned(OnFinish) then OnFinish(Self);
end;


//==============================================================================
// TabfEventThread
//==============================================================================
// A thread that have OnExecute event.

procedure TabfEventThread.Execute;
begin
  if Terminated then Exit;

  if Synchronized then
    Synchronize(DoExecute)
  else
    DoExecute;

  inherited Execute;
end;

//------------------------------------------------------------------------------

procedure TabfEventThread.DoExecute;
begin
  if Terminated then Exit;

  if Assigned(OnExecute) then OnExecute(Self);
end;


//==============================================================================
// TabfTimerThread
//==============================================================================
// A thread for the timer.

constructor TabfTimerThread.Create(CreateSuspended: Boolean);
begin
  FInterval := 1000;
  inherited Create(CreateSuspended);
end;

//------------------------------------------------------------------------------

destructor TabfTimerThread.Destroy;
begin
  try
    OnExecute := nil;
    Stop;
  except
  end;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfTimerThread.Execute;
begin
  if Terminated then Exit;

// Create an event if the first execution
  if FStopHandle = 0 then
    FStopHandle := CreateEvent(nil, False, False, nil);

// Repeat as timer
  while not Terminated do
    if WaitForSingleObject(FStopHandle, FInterval) = WAIT_TIMEOUT then
      if not Terminated then
        inherited Execute;
end;

//------------------------------------------------------------------------------

procedure TabfTimerThread.Stop;
begin
  try
    if StopHandle <> 0 then SetEvent(StopHandle);
    Resume;
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTimerThread.SetInterval(Value: Cardinal);
var
  RegBool: Boolean;
begin
  if Interval = Value then Exit;

  try
    RegBool := Suspended;
    FInterval := Value;
    if StopHandle <> 0 then Stop;
    Suspended := RegBool;
  except
  end;
end;


//==============================================================================
// TabfFolderMonitorThread
//==============================================================================
// Tread class for folder monitoring.

var
  CSFolderMonitor: TRTLCriticalSection;

constructor TabfFolderMonitorThread.Create(ChangeEvent: TThreadMethod);
begin
  FreeOnTerminate := True;
  FChangeEvent := ChangeEvent;
  FMutex := CreateMutex(nil, True, nil);
// Mutex is used to wake up the thread as it waits for any change notifications.
  WaitForSingleObject(Mutex, INFINITE); // Grab the mutex.
  FWaitChanged := False;
  inherited Create(True);
end;

//------------------------------------------------------------------------------

destructor TabfFolderMonitorThread.Destroy;
begin
  if WaitHandle <> ERROR_INVALID_HANDLE then
    FindCloseChangeNotification(WaitHandle);
  CloseHandle(Mutex);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfFolderMonitorThread.Execute;
var
  Obj: DWORD;
  Handles: array[0..1] of DWORD;
begin
  EnterCriticalSection(CSFolderMonitor);
  try
  // Right boolean casting is needed
    FWaitHandle := abfFindFirstChangeNotificationW(FDirectory,
      LongBool(Integer(FWatchSubTree)), FNotifyOptionFlags);
  finally
    LeaveCriticalSection(CSFolderMonitor);
  end;

  if FWaitHandle = ERROR_INVALID_HANDLE then Exit;

  while not Terminated do
  begin
    Handles[0] := WaitHandle;
    Handles[1] := Mutex;
    Obj := WaitForMultipleObjects(2, @Handles, False, INFINITE);
    case Obj of
      WAIT_OBJECT_0:
        begin
          if Assigned(FChangeEvent) then Synchronize(FChangeEvent);
          FindNextChangeNotification(WaitHandle);
        end;
      WAIT_OBJECT_0 + 1: ReleaseMutex(Mutex);
      WAIT_FAILED: Exit;
    end;

    EnterCriticalSection(CSFolderMonitor);
    try
      if FWaitChanged then
      begin
      // Right boolean casting is needed
        FWaitHandle := abfFindFirstChangeNotificationW(FDirectory,
          LongBool(Integer(FWatchSubTree)), FNotifyOptionFlags);
        FWaitChanged := False;
      end;
    finally
      LeaveCriticalSection(CSFolderMonitor);
    end;
  end;{while not Terminated do}

end;{procedure TabfFolderMonitorThread.Execute}

//------------------------------------------------------------------------------

procedure TabfFolderMonitorThread.SetDirectoryOptions(ADir: WideString;
  AWatchSubTree: Boolean; ANotifyOptionFlags: DWORD);
begin
  EnterCriticalSection(CSFolderMonitor);
  try
    FDirectory := ADir;
    if Trim(FDirectory) = '' then FDirectory := abfGetCurrentDirectoryW;
    FWatchSubTree := AWatchSubTree;
    FNotifyOptionFlags := ANotifyOptionFlags;
  // Release the current notification handle
    FindCloseChangeNotification(WaitHandle);
    FWaitChanged := True;
  finally
    LeaveCriticalSection(CSFolderMonitor);
  end;
end;


//==============================================================================
// TabfRegistryMonitorThread
//==============================================================================
// Tread class for registry monitoring.

var
  CSRegistryMonitor: TRTLCriticalSection;

constructor TabfRegistryMonitorThread.Create(ChangeEvent: TThreadMethod);
begin
  FreeOnTerminate := True;
  FChangeEvent := ChangeEvent;
  FMutex := CreateMutex(nil, True, nil);
  FWaitHandle := INVALID_HANDLE_VALUE;
  FWaitRootHandle := INVALID_HANDLE_VALUE;
  FKeyExists := False;
// Mutex is used to wake up the thread as it waits for any change notifications.
  WaitForSingleObject(Mutex, INFINITE); // Grab the mutex.
  FWaitChanged := False;
  inherited Create(True);
end;

//------------------------------------------------------------------------------

destructor TabfRegistryMonitorThread.Destroy;
begin
  abfRegFindCloseChangeNotification(WaitHandle);
  abfRegFindCloseChangeNotification(FWaitRootHandle);
  RegCloseKey(FKey);
  CloseHandle(Mutex);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfRegistryMonitorThread.Execute;
var
  Obj: DWORD;
  Handles: array[0..2] of DWORD;
  HandlesCount: Cardinal;
  Res: LongInt;
  TempKey: HKEY;
  rcd, rcd2: REG_CHANGE_DATA;
begin
  Res := RegOpenKeyEx(FRootKey, PAnsiChar(FRegistryKey), 0, KEY_READ, FKey);

  FKeyExists := (Res = ERROR_SUCCESS);

  EnterCriticalSection(CSRegistryMonitor);
  try
    FWaitHandle := abfRegFindFirstChangeNotification(FKey, FWatchSubTree,
      FNotifyOptionFlags, @rcd);
    FWaitRootHandle := abfRegFindFirstChangeNotification(FRootKey, True,
      REG_NOTIFY_CHANGE_NAME, @rcd2);
  finally
    LeaveCriticalSection(CSRegistryMonitor);
  end;

  while not Terminated do
  begin
    HandlesCount := 1;

    if FWaitRootHandle <> INVALID_HANDLE_VALUE then Inc(HandlesCount);
    if FWaitHandle <> INVALID_HANDLE_VALUE then Inc(HandlesCount);

    Handles[0] := Mutex;
    Handles[1] := FWaitRootHandle;
    Handles[2] := WaitHandle;
    Obj := WaitForMultipleObjects(HandlesCount, @Handles, False, INFINITE);
    case Obj of
      WAIT_OBJECT_0: ReleaseMutex(Mutex);
      WAIT_OBJECT_0 + 1:
        begin
          Res := RegOpenKeyEx(FRootKey, PAnsiChar(FRegistryKey), 0, KEY_READ,
            TempKey);
          if (Res <> ERROR_SUCCESS) then
          begin
            if FKeyExists and Assigned(FChangeEvent) then
            try
              Synchronize(FChangeEvent);
            except
            end;
            FKeyExists := False;
          end else
          begin
            if not FKeyExists then
            begin
              abfRegFindCloseChangeNotification(FWaitHandle);
              FKey := TempKey;
              FWaitHandle := abfRegFindFirstChangeNotification(FKey,
                FWatchSubTree, FNotifyOptionFlags, @rcd);
            end;
            if not FKeyExists and Assigned(FChangeEvent) then
            try
              Synchronize(FChangeEvent);
            except
            end;
            FKeyExists := True;
          end;
          abfRegFindNextChangeNotification(FWaitRootHandle, @rcd2);
        end;
      WAIT_OBJECT_0 + 2:
        begin
          if Assigned(FChangeEvent) then
          try
            Synchronize(FChangeEvent);
          except
          end;
          abfRegFindNextChangeNotification(WaitHandle, @rcd);
        end;
      WAIT_FAILED: Exit;
    end;

    if Terminated then Exit;

    EnterCriticalSection(CSRegistryMonitor);
    try
      if FWaitChanged then
      begin
        Res := RegOpenKeyEx(FRootKey, PAnsiChar(FRegistryKey), 0, KEY_READ, FKey);
        FKeyExists := Res = ERROR_SUCCESS;
        if FKeyExists then
          FWaitHandle := abfRegFindFirstChangeNotification(FKey, FWatchSubTree,
            FNotifyOptionFlags, @rcd);
        FWaitChanged := False;
      end;
    finally
      LeaveCriticalSection(CSRegistryMonitor);
    end;
  end;{while not Terminated do}

end;{procedure TabfRegistryMonitorThread.Execute}

//------------------------------------------------------------------------------

procedure TabfRegistryMonitorThread.SetRegistryKeyOptions(ARootKey: HKEY;
  AKey: AnsiString; AWatchSubTree: Boolean; ANotifyOptionFlags: DWORD);
begin
  EnterCriticalSection(CSRegistryMonitor);
  try
    abfRegFindCloseChangeNotification(FWaitHandle);
    abfRegFindCloseChangeNotification(FWaitRootHandle);
    RegCloseKey(FKey);
    FRootKey := ARootKey;
    FRegistryKey := AKey;
    FWatchSubTree := AWatchSubTree;
    FNotifyOptionFlags := ANotifyOptionFlags;
  // Release the current notification handle
    FWaitChanged := True;
  finally
    LeaveCriticalSection(CSRegistryMonitor);
  end;
end;


//==============================================================================
// TabfCustomCopyThread
//==============================================================================
// Prototype of threads that provides a "Copy" operation asynchronously to the
// main VCL thread. SrcName specifies a source name of copying file/data.
// DstName specifies a destination name of copying file/data. You should
// implement Copy and CalcTotalSize methods in descendants. Copy method provides
// a main copying routine. CalcTotalSize is used to determine a TotalSize of the
// copying data. Use DoProgress or FireProgressEvent periodically in the Copy
// method. Setting of the Error property to value different then 0, terminates
// the thread and sets the ErrorMessage property. Use ErrorMessage to get a
// readable information about the error.
// 08/30/2003

constructor TabfCustomCopyThread.Create(const ASrcName, ADstName: WideString;
  CreateSuspended, AFreeOnTerminate: Boolean;
  AOnError: TabfCopyThreadErrorEvent; AOnProgress: TabfCopyThreadProgressEvent;
  AOnFinish: TNotifyEvent;  const ExtraData: Pointer);
begin
  FTotalSize := 0;
  FCopiedSize := 0;
  FErrorCode := 0;
  SrcName  := ASrcName;
  DstName  := ADstName;
  FreeOnTerminate := AFreeOnTerminate;
  OnError    := AOnError;
  OnProgress := AOnProgress;
  OnFinish   := AOnFinish;
  FExtraData := ExtraData;
  inherited Create(CreateSuspended);
end;

//------------------------------------------------------------------------------
// Cancels a copying process.

procedure TabfCustomCopyThread.Cancel;
begin
  Terminate;
  if Suspended then Resume;
end;

//------------------------------------------------------------------------------
// Determines the sequence of operations. In descendants you should override the
// Copy method instead of Execute.

procedure TabfCustomCopyThread.Execute;
begin
  try
    if Terminated then Exit;
  // Get size of the data, CalcTotalSize should be implemented in descendants
    CalcTotalSize;
    if Terminated then Exit;
  // Progress 0%
    Synchronize(FireProgressEvent);
    if Terminated then Exit;
  // Process main routine, the Copy method should be implemented in descendants
    Copy;
    if Terminated then Exit;
  // Progress 100%
    Synchronize(FireProgressEvent);
  finally
    Finish;
  end;
end;

//------------------------------------------------------------------------------
// Pass this function to the Synchronize method to fire the OnError event
// synchronously to the main VCL thread.

procedure TabfCustomCopyThread.FireErrorEvent;
begin
  if Assigned(OnError) then
    DoError(ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------

procedure TabfCustomCopyThread.DoError(ErrorCode: LongWord;
  const ErrorMessage: AnsiString);
begin
  if Assigned(OnError) then OnError(Self, ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------
// Pass this function to the Synchronize method to fire the OnProgress event
// synchronously to the main VCL thread.

procedure TabfCustomCopyThread.FireProgressEvent;
begin
  if Assigned(OnProgress) then
    DoProgress(CopiedSize, TotalSize);
end;

//------------------------------------------------------------------------------

procedure TabfCustomCopyThread.DoProgress(CopiedSize, TotalSize: LongWord);
var
  ATerminate: Boolean;
begin
  ATerminate := False;
  if Assigned(OnProgress) then
    OnProgress(Self, CopiedSize, TotalSize, ATerminate);
  if ATerminate then Cancel;
end;

//------------------------------------------------------------------------------
// Simply assigns SrcName value. Override if you need catch a value changing.

procedure TabfCustomCopyThread.SetSrcName(const A: WideString);
begin
  FSrcName := A;
end;

//------------------------------------------------------------------------------
// Simply assigns DstName value. Override if you need catch a value changing.

procedure TabfCustomCopyThread.SetDstName(const A: WideString);
begin
  FDstName := A;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCopyThread.SetErrorCode(A: LongWord);
begin
  FErrorCode := A;
  ReturnValue := FErrorCode;
  if ErrorCode = 0 then Exit;
  if Terminated then Exit;

// Fire the OnError event and terminate
  Synchronize(FireErrorEvent);
  Cancel;
//  Terminate;
end;

//------------------------------------------------------------------------------
// Returns a system error message associated with the ErrorCode value. Override
// if you need associate own messages with the ErrorCode value.

function TabfCustomCopyThread.GetErrorMessage: AnsiString;
begin
  Result := '';
  if ErrorCode = 0 then Exit;
  Result := SysErrorMessage(ErrorCode);
end;


//==============================================================================
// TabfLocalCopyThread
//==============================================================================
// Provides asynchronous local file copying.
// 10/15/2000

//------------------------------------------------------------------------------
// Determines a size of the SrcName file.

procedure TabfLocalCopyThread.CalcTotalSize;
begin
  if not abfGetFileSizeW(SrcName, FTotalSize) then
  begin
    FTotalSize := 0;
    ErrorCode := GetLastError;
  end;
end;

//------------------------------------------------------------------------------
// Copies a SrcName file to the DstName file by blocks.

procedure _LocalCopyProgressEvent(Sender: TObject; ACopiedSize,
  ATotalSize: Int64; var ATerminate: Boolean);
begin
  if (Sender is TabfLocalCopyThread) then
    with TabfLocalCopyThread(Sender) do
    begin
      FCopiedSize := ACopiedSize;
      FTotalSize  := ATotalSize;
      Synchronize(FireProgressEvent);
      ATerminate := Terminated;
    end;
end;

procedure TabfLocalCopyThread.Copy;
begin
  abfForceDirectoriesW(abfExtractFilePathW(DstName));

  if abfCopyFileExW(SrcName, DstName, 0, _LocalCopyProgressEvent, Self) then
    FCopiedSize := TotalSize
  else
    ErrorCode := GetLastError;
end;

//==============================================================================
// Misc functions
//==============================================================================

function abfFillCallbackThunk(var ACallbackThunk: TabfCallbackThunk;
  AObject: TObject; AMethod: TabfObjectCallBack): Boolean;
begin
  Result := False;
  if IsBadCodePtr(AObject) or IsBadCodePtr(@AMethod) then Exit;

  ACallbackThunk := CabfCallbackThunk;
  ACallbackThunk.SelfPtr := AObject;
  ACallbackThunk.JmpOffset := Integer(@AMethod) -
    Integer(@ACallbackThunk.JMP) - 5;

  Result := True;
end;


//==============================================================================
// Routines for initialization/finalization.
//==============================================================================

procedure _DoInitialization;
begin
  InitializeCriticalSection(CSFolderMonitor);
  InitializeCriticalSection(CSRegistryMonitor);
end;

//------------------------------------------------------------------------------

procedure _DoFinalization;
begin
  DeleteCriticalSection(CSFolderMonitor);
  DeleteCriticalSection(CSRegistryMonitor);
end;

{******************************************************************************}
initialization
{******************************************************************************}

  _DoInitialization;

{******************************************************************************}
finalization
{******************************************************************************}

  _DoFinalization;

end{unit abfClasses}.

