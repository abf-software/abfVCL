{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComponents;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Menus, ShellApi,
  Forms,
// ABF VCL
  abfClasses, abfSysUtils, abfVclUtils, abfRegistry;

const
  WM_TRAYICONNOTIFY = WM_USER + 211;

  {$EXTERNALSYM NIN_SELECT}
  NIN_SELECT      = WM_USER + 0;
  {$EXTERNALSYM NINF_KEY}
  NINF_KEY        =  $1;
  {$EXTERNALSYM NIN_KEYSELECT}
  NIN_KEYSELECT   = NIN_SELECT or NINF_KEY;

  {$EXTERNALSYM NIN_BALLOONSHOW}
  NIN_BALLOONSHOW       = WM_USER + 2;
  {$EXTERNALSYM NIN_BALLOONHIDE}
  NIN_BALLOONHIDE       = WM_USER + 3;
  {$EXTERNALSYM NIN_BALLOONTIMEOUT}
  NIN_BALLOONTIMEOUT    = WM_USER + 4;
  {$EXTERNALSYM NIN_BALLOONUSERCLICK}
  NIN_BALLOONUSERCLICK  = WM_USER + 5;

  {$EXTERNALSYM NIM_ADD}
  NIM_ADD         = $00000000;
  {$EXTERNALSYM NIM_MODIFY}
  NIM_MODIFY      = $00000001;
  {$EXTERNALSYM NIM_DELETE}
  NIM_DELETE      = $00000002;
  {$EXTERNALSYM NIM_SETFOCUS}
  NIM_SETFOCUS    = $00000003;
  {$EXTERNALSYM NIM_SETVERSION}
  NIM_SETVERSION  = $00000004;

  {$EXTERNALSYM NIF_MESSAGE}
  NIF_MESSAGE     = $00000001;
  {$EXTERNALSYM NIF_ICON}
  NIF_ICON        = $00000002;
  {$EXTERNALSYM NIF_TIP}
  NIF_TIP         = $00000004;
  {$EXTERNALSYM NIF_STATE}
  NIF_STATE       = $00000008;
  {$EXTERNALSYM NIF_INFO}
  NIF_INFO        = $00000010;
  {$EXTERNALSYM NIF_GUID}
  NIF_GUID        = $00000020;

  {$EXTERNALSYM NIS_HIDDEN}
  NIS_HIDDEN      = $00000001;
  {$EXTERNALSYM NIS_SHAREDICON}
  NIS_SHAREDICON  = $00000002;

// Notify Icon Infotip flags
// Icon flags are mutually exclusive and take only the lowest 2 bits

  {$EXTERNALSYM NIIF_NONE}
  NIIF_NONE       = $00000000;
  {$EXTERNALSYM NIIF_INFO}
  NIIF_INFO       = $00000001;
  {$EXTERNALSYM NIIF_WARNING}
  NIIF_WARNING    = $00000002;
  {$EXTERNALSYM NIIF_ERROR}
  NIIF_ERROR      = $00000003;
  {$EXTERNALSYM NIIF_USER}
  NIIF_USER       = $00000004;
  {$EXTERNALSYM NIIF_ICON_MASK}
  NIIF_ICON_MASK  = $0000000F;
  {$EXTERNALSYM NIIF_NOSOUND}
  NIIF_NOSOUND    = $00000010;

{$IfNDef D3}
  PROCESS_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $FFF);
{$EndIf D3}

type

//==============================================================================
// TabfComponent
//==============================================================================
// Base class of all non-visual TabfXXX components.

  TabfComponent = class(TComponent)
  private
    FAbout: string;
    FParentHandle: THandle;
  protected
    function GetParentHandle: THandle; virtual;
    procedure SetParentHandle(AValue: THandle); virtual;
    property About: string read FAbout write FAbout stored False;
  public
    constructor Create(AOwner: TComponent); override;
    property ParentHandle: THandle read GetParentHandle
      write SetParentHandle default 0;
  end;


//==============================================================================
// TabfCustomThreadComponent
//==============================================================================
// TabfCustomThreadComponent is a base type of all components that has own
// thread.

  TabfThreadMethodEx = procedure(Data: Pointer) of object;

  TabfCustomThreadComponent = class(TabfComponent)
  private
    FPriority: TThreadPriority;
    FReturnValue: Integer;
    FSuspended, FSynchronized: Boolean;
    FOnExecute, FOnException, FOnTerminate, FOnFinish: TNotifyEvent;
    procedure InternalSynchronizeEx;
  // Properties Get/Set
    function  GetHandle: THandle;
    function  GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    function  GetReturnValue: Integer;
    procedure SetReturnValue(Value: Integer);
    function  GetSuspended: Boolean;
    procedure SetSuspended(Value: Boolean);
    function  GetSynchronized: Boolean;
    procedure SetSynchronized(Value: Boolean);
    function  GetTerminated: Boolean;
    function  GetThreadID: THandle;
    function  GetOnTerminate: TNotifyEvent;
    procedure SetOnTerminate(Value: TNotifyEvent);
    function GetOnFinish: TNotifyEvent;
    procedure SetOnFinish(const Value: TNotifyEvent);
  protected
    FThread: TabfEventThread;
    FSyncMethod: TabfThreadMethodEx;
    FSyncData: Pointer;
    procedure Loaded; override;
    procedure DoExecute(Sender: TObject); virtual;
    procedure DoException(Data: Pointer); virtual;
  // Events
    property OnException: TNotifyEvent read FOnException write FOnException;
    property OnTerminate: TNotifyEvent read GetOnTerminate write SetOnTerminate;
    property OnFinish: TNotifyEvent read GetOnFinish write SetOnFinish;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateThread; virtual;
  // Execution routines
    procedure Execute; virtual;
    procedure Synchronize(Method: TThreadMethod); virtual;
    procedure SynchronizeEx(Method: TabfThreadMethodEx; Data: Pointer); virtual;
  // Thread routines
    procedure Suspend;
    procedure Resume;
    procedure Terminate;
    procedure TerminateHard;
    function TerminateWaitFor: Integer;
    function WaitFor: Integer;
  // Properties
    property Handle: THandle read GetHandle;
    property Priority: TThreadPriority read GetPriority write SetPriority
      default tpNormal;
    property ReturnValue: Integer read GetReturnValue write SetReturnValue
      default 0;
    property Suspended: Boolean read GetSuspended write SetSuspended
      default True;
    property Synchronized: Boolean read GetSynchronized write SetSynchronized
      default False;
    property Terminated: Boolean read GetTerminated;
    property Thread: TabfEventThread read FThread;
    property ThreadID: THandle read GetThreadID;
  // Events
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;{TabfCustomThreadComponent = class(TabfComponent)}


//==============================================================================
// TabfThreadComponent
//==============================================================================
// TabfThreadComponent is an incapsulation of the tread for easy usage.

  TabfThreadComponent = class(TabfCustomThreadComponent)
  published
  // Properties
    property About;
    property Priority;
    property ReturnValue;
    property Suspended;
    property Synchronized;
  // Events
    property OnException;
    property OnExecute;
    property OnTerminate;
    property OnFinish;
  end;


//==============================================================================
// TabfCustomThreadTimer
//==============================================================================
// TabfCustomThreadTimer is a base type of all threaded timers.

  TabfCustomThreadTimer = class(TabfCustomThreadComponent)
  private
    FInterval: Cardinal;
    function  GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function  GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function  GetOnTimer: TNotifyEvent;
    procedure SetOnTimer(Value: TNotifyEvent);
    function  GetThread: TabfTimerThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateThread; override;
  // Properties
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    property Enabled: Boolean read GetEnabled write SetEnabled default False;
    property Thread: TabfTimerThread read GetThread;
  // Events
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
  end;


//==============================================================================
// TabfThreadTimer
//==============================================================================
// TabfThreadTimer is a treaded timer component. Same to the TTimer, but uses
// thread, so it is more precision.

  TabfThreadTimer = class(TabfCustomThreadTimer)
  published
  // Properties
    property About;
    property Interval;
    property Priority;
    property Synchronized;
  // Events
    property OnTimer;
  // Properties
    property Enabled; // Should be the last
  end;


//==============================================================================
// TabfCustomTimer
//==============================================================================
// TabfCustomTimer is a base type of all components that contains internal
// timer.

  TabfCustomTimer = class(TabfComponent)
  private
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(A: Boolean);
    procedure SetInterval(A: Cardinal);
    procedure SetOnTimer(A: TNotifyEvent);
  protected
    FHandle: THandle;
    procedure WndProc(var Message: TMessage); virtual;
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property Handle: THandle read FHandle;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
  // Events
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;{TabfCustomTimer = class(TabfComponent)}


//==============================================================================
// TabfCustomWndProcHook
//==============================================================================
// Use TabfCustomWndProcHook component or its descendants to hook a WndProc of
// the control specified in the WinControl property. If no WinControl specified,
// the form where the component is placed will be hooked. Also you can use
// the Handle property to hook any window by its handle.

  TabfCustomWndProcHook = class(TabfComponent)
  private
    FOnChangeHandle: TNotifyEvent;
    FOnChangeWinControl: TNotifyEvent;
    FOnMessageAfter: TabfMessageEvent;
    FOnMessageBefore: TabfMessageEvent;
  protected
    FActive: Boolean;
    FAutoDrag: Boolean;
    FHandle: THandle;
    FWinControl: TWinControl;
    FOldWndProc, FNewWndProc: Pointer;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  // Hook routines
    procedure Hook; virtual;
    procedure UnHook; virtual;
    procedure WndProc(var Message: TMessage); virtual;
  // Properties Get/Set
    procedure SetActive(A: Boolean); virtual;
    function  GetHandle: THandle; virtual;
    procedure SetHandle(A: THandle); virtual;
    function  GetWinControl: TWinControl; virtual;
    procedure SetWinControl(const A: TWinControl); virtual;
  // Properties
    property AutoDrag: Boolean read FAutoDrag write FAutoDrag default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property Active: Boolean read FActive write SetActive default False;
    property Handle: THandle read GetHandle write SetHandle;
    property WinControl: TWinControl read GetWinControl write SetWinControl;
  // Events
    property OnChangeHandle: TNotifyEvent read FOnChangeHandle
      write FOnChangeHandle;
    property OnChangeWinControl: TNotifyEvent read FOnChangeWinControl
      write FOnChangeWinControl;
    property OnMessageAfter: TabfMessageEvent read FOnMessageAfter
      write FOnMessageAfter;
    property OnMessageBefore: TabfMessageEvent read FOnMessageBefore
      write FOnMessageBefore;
  end;{TabfCustomWndProcHook = class(TabfComponent)}


//==============================================================================
// TabfWndProcHook
//==============================================================================
// Descendant of the TabfCustomWndProcHook component. Has published properties,
// so it can be used at design-time.

  TabfWndProcHook = class(TabfCustomWndProcHook)
  published
  // Properties
    property About;
    property Active;
    property AutoDrag;
    property WinControl;
  // Events
    property OnChangeHandle;
    property OnChangeWinControl;
    property OnMessageAfter;
    property OnMessageBefore;
  end;


//==============================================================================
//  TabfFileOperation
//==============================================================================
// TabfFileOperation is a component that provides an easy way to perform
// different operations with files. Allows to copy, move, delete and other file
// operations.

  TabfFileOperationType = (fotCopy, fotDelete, fotMove, fotRename);
  TabfFileOperationFlag = (fofAllowUndo, fofFilesOnly, fofNoConfirmation,
    fofNoConfirmMkDir, fofNoErrorUI, fofRenameCollision, fofSilent,
    fofSimpleProgress);
  TabfFileOperationFlags = set of TabfFileOperationFlag;
  TabfFileOperationEvent = procedure(Sender: TObject; const S: string;
    var Handled: Boolean) of object;

  TabfFileOperation = class(TabfComponent)
  private
    FFileList: TabfWideStrings;
    FDestFolder: TabfWideString;
    FProgressTitle: TabfWideString;
    FOperation: TabfFileOperationType;
    FOptions: TabfFileOperationFlags;
    FOnError: TabfFileOperationEvent;
    procedure SetFileList(const A: TabfWideStrings);
  protected
    function ExecuteOperation(
      OperationType: TabfFileOperationType): Boolean; virtual;
    function GetSystemError(ErrorCode: Integer): TabfWideString; virtual;
    function Error(const S: TabfWideString): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Copy: Boolean;
    function Delete: Boolean;
    function Execute: Boolean; virtual;
    function Move: Boolean;
    function Rename: Boolean;
  published
  // Properties
    property About;
    property FileList: TabfWideStrings read FFileList write SetFileList;
    property DestFolder: TabfWideString read FDestFolder write FDestFolder;
    property ProgressTitle: TabfWideString read FProgressTitle write FProgressTitle;
    property Operation: TabfFileOperationType read FOperation write FOperation
      default fotCopy;
    property Options: TabfFileOperationFlags read FOptions write FOptions
      default [fofAllowUndo, fofFilesOnly, fofRenameCollision];
  // Events
    property OnError: TabfFileOperationEvent read FOnError write FOnError;
  end;{TabfFileOperation = class(TabfComponent)}

  
//==============================================================================
// TabfFileAssociation
//==============================================================================
// Component allows read and write file association data.

  TabfFileAssociation = class(TabfComponent)
  private
    FExtension: string;
    FExtDescription: string;
    FFileDescription: string;
    FIconFile: string;
    FIconIndex: Integer;
    FOpenCommand: TFileName;
    FOpenCommandDescription: string;
    FOpenNewCommand: TFileName;
    FOpenNewCommandDescription: string;
    FEditCommand: TFileName;
    FEditCommandDescription: string;
    FPrintCommand: TFileName;
    FPrintCommandDescription: string;
    FPrintToCommand: TFileName;
    FPrintToCommandDescription: string;
  // Properties Get/Set
    procedure SetExtension(const Value: string);
    procedure SetExtDescription(const Value: string);
    procedure SetFileDescription(const Value: string);
    procedure SetIcon(const Value: TIcon);
    procedure SetIconFile(const Value: string);
    procedure SetIconIndex(const Value: Integer);
    procedure SetOpenCommand(const Value: TFileName);
    procedure SetOpenCommandDescription(const Value: string);
    procedure SetOpenNewCommand(const Value: TFileName);
    procedure SetOpenNewCommandDescription(const Value: string);
    procedure SetEditCommand(const Value: TFileName);
    procedure SetEditCommandDescription(const Value: string);
    procedure SetPrintCommand(const Value: TFileName);
    procedure SetPrintCommandDescription(const Value: string);
    procedure SetPrintToCommand(const Value: TFileName);
    procedure SetPrintToCommandDescription(const Value: string);
  protected
    FLargeIcon: TIcon;
    FSmallIcon: TIcon;
    FReadOnly: Boolean;
    function ChangingAllowed: Boolean; virtual;
    procedure FixExtension; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ReadAssociation: Boolean; overload; virtual;
    function ReadAssociation(const AExtension: string): Boolean; overload; virtual; 
    function WriteAssociation: Boolean; virtual;
    function DeleteAssociation: Boolean; virtual;
    procedure UpdateAssociation; virtual;
  published
    property About;
    property Extension: string read FExtension write SetExtension;
    property ExtDescription: string read FExtDescription write SetExtDescription;
    property FileDescription: string read FFileDescription write SetFileDescription;
    property IconFile: string read FIconFile write SetIconFile;
    property IconIndex: Integer read FIconIndex write SetIconIndex default 0;
    property LargeIcon: TIcon read FLargeIcon write SetIcon;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default True;
    property SmallIcon: TIcon read FSmallIcon write SetIcon;
    property OpenCommand: TFileName read FOpenCommand write SetOpenCommand;
    property OpenCommandDescription: string read FOpenCommandDescription
      write SetOpenCommandDescription;
    property OpenNewCommand: TFileName read FOpenNewCommand write SetOpenNewCommand;
    property OpenNewCommandDescription: string read FOpenNewCommandDescription
      write SetOpenNewCommandDescription;
    property EditCommand: TFileName read FEditCommand write SetEditCommand;
    property EditCommandDescription: string read FEditCommandDescription
      write SetEditCommandDescription;
    property PrintCommand: TFileName read FPrintCommand write SetPrintCommand;
    property PrintCommandDescription: string read FPrintCommandDescription
      write SetPrintCommandDescription;
    property PrintToCommand: TFileName read FPrintToCommand write SetPrintToCommand;
    property PrintToCommandDescription: string read FPrintToCommandDescription
      write SetPrintToCommandDescription;
  end;


//==============================================================================
// TabfCustomFileStorage
//==============================================================================
// A prototype of components that can store a file data into the *.dfm resource
// of the form where the component is placed.

  EabfFileStorage = class(EabfException);

  TabfFileStorageOnFileOperation = procedure(Sender: TObject;
    Successful: Boolean) of object;
  TabfFileStorageOnConfirmOverride = procedure(Sender: TObject;
    var OverrideFile: Boolean) of object;

  TabfCustomFileStorage = class(TabfComponent)
  private
    FOnLoad: TabfFileStorageOnFileOperation;
    FOnSave: TabfFileStorageOnFileOperation;
    FOnConfirmOverride: TabfFileStorageOnConfirmOverride;
  // Properties Get/Set
    procedure SetSaveName(const Value: TFileName);
  protected
    FFileDataStream: TStream;
    FAutoSave: Boolean;
    FDataSize: LongWord;
    FFileName: TFileName;
    FSaveName: TFileName;
    procedure Loaded; override;
  // File operations
    function LoadFrom(const AFileName: string): Boolean; virtual;
    function SaveTo(const AFileName: string): Boolean; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
  // Streaming routines
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadFileData(Stream: TStream); virtual;
    procedure WriteFileData(Stream: TStream); virtual;
  // Properties Get/Set
    procedure SetAutoSave(Value: Boolean); virtual;
    procedure SetFileName(const Value: TFileName); virtual;
    function  GetDataSize: LongWord; virtual;
    procedure SetDataSize(Value: LongWord); virtual;
  // Properties
    property SaveName: TFileName read FSaveName write SetSaveName;
    property AutoSave: Boolean read FAutoSave write SetAutoSave default False;
  // Events
    property OnLoad: TabfFileStorageOnFileOperation read FOnLoad write FOnLoad;
    property OnSave: TabfFileStorageOnFileOperation read FOnSave write FOnSave;
    property OnConfirmOverride: TabfFileStorageOnConfirmOverride
      read FOnConfirmOverride write FOnConfirmOverride;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  // File operations
    procedure Clear; virtual;
  // Properties
    property DataSize: LongWord read GetDataSize write SetDataSize stored False;
    property FileName: TFileName read FFileName write SetFileName;
  end;{TabfCustomFileStorage = class(TabfComponent)}


//==============================================================================
// TabfFileStorage
//==============================================================================
// A component that can store a file into a *.dfm resource of the form where
// the component is placed.

  TabfFileStorage = class(TabfCustomFileStorage)
  public
    function LoadFrom(const AFileName: string): Boolean; override;
    function SaveTo(const AFileName: string): Boolean; override;
    procedure Load; override;
    procedure Save; override;
  published
  // Properties
    property About;
    property SaveName;
    property FileName;
    property DataSize;
    property AutoSave; // Should be read last
  // Events
    property OnLoad;
    property OnSave;
    property OnConfirmOverride;
  end;


//==============================================================================
// TabfStartButtonProperties
//==============================================================================
// Component that allows to get or set some parameters of the Window's Start
// Button

  TabfStartButtonProperties = class(TabfComponent)
  private
    FHeight: Integer;
    FTop: Integer;
    FWidth: Integer;
    FLeft: Integer;
  // Properties Get/Set
    function  GetBoundsRect: TRect;
    procedure SetBoundsRect(const A: TRect);
    function  GetLeft: Integer;
    procedure SetLeft(A: Integer);
    function  GetTop: Integer;
    procedure SetTop(A: Integer);
    function  GetWidth: Integer;
    procedure SetWidth(A: Integer);
    function  GetHeight: Integer;
    procedure SetHeight(A: Integer);
    function  GetCaption: string;
    procedure SetCaption(const A: string);
    function  GetEnabled: Boolean;
    procedure SetEnabled(A: Boolean);
    function  GetVisible: Boolean;
    procedure SetVisible(A: Boolean);
  protected
    FHandle: THandle;
    procedure UpdateBounds; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property Handle: THandle read FHandle;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Caption: string read GetCaption write SetCaption;
  published
    property About;
    property ButtonLeft: Integer read GetLeft write SetLeft;
    property ButtonTop: Integer read GetTop write SetTop;
    property ButtonWidth: Integer read GetWidth write SetWidth;
    property ButtonHeight: Integer read GetHeight write SetHeight;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;{TabfStartButtonProperties = class(TabfComponent)}


//==============================================================================
// TabfColorPicker
//==============================================================================
// Allows to get a color value of the pixel under cursor.

  TabfColorPicker = class(TabfCustomTimer)
  private
    FCursor: TCursor;
  protected
    procedure Timer; override;
    procedure UpdateCursor; virtual;
    function  GetColor: TColor; virtual;
    procedure SetCursor(A: TCursor); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Color: TColor read GetColor;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
  published
    property About;
    property Enabled;
    property Interval default 100;
    property OnTimer;
  end;{TabfColorPicker = class(TabfCustomTimer)}


//==============================================================================
// TabfTrayIcon
//==============================================================================
// Component for working with the system tray. Allows to add or remove the icon,
// provides icon messages support. Has an engine to create the icon animated.


  TabfNotifyIconDataAVer4  = packed record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..63] of AnsiChar;
  end;
  {$EXTERNALSYM TabfNotifyIconDataAVer4}
  PabfNotifyIconDataAVer4 = ^TabfNotifyIconDataAVer4;
  {$EXTERNALSYM PabfNotifyIconDataAVer4}

  TabfNotifyIconDataWVer4  = packed record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..63] of WideChar;
  end;
  {$EXTERNALSYM TabfNotifyIconDataWVer4}
  PabfNotifyIconDataWVer4 = ^TabfNotifyIconDataWVer4;
  {$EXTERNALSYM PabfNotifyIconDataWVer4}

  TabfNotifyIconDataAVer5  = packed record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of AnsiChar;
    case Integer of
      0: (uTimeout: UINT;
          szInfoTitle: array [0..63] of AnsiChar;
          dwInfoFlags: DWORD);
      1: (uVersion: UINT);
  end;
  {$EXTERNALSYM TabfNotifyIconDataAVer5}
  PabfNotifyIconDataAVer5 = ^TabfNotifyIconDataAVer5;
  {$EXTERNALSYM PabfNotifyIconDataAVer5}

  TabfNotifyIconDataWVer5  = packed record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of WideChar;
    case Integer of
      0: (uTimeout: UINT;
          szInfoTitle: array [0..63] of WideChar;
          dwInfoFlags: DWORD);
      1: (uVersion: UINT);
  end;
  {$EXTERNALSYM TabfNotifyIconDataWVer5}
  PabfNotifyIconDataWVer5 = ^TabfNotifyIconDataWVer5;
  {$EXTERNALSYM PabfNotifyIconDataWVer5}

  TabfNotifyIconDataAVer6  = packed record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of AnsiChar;
    case Integer of
      0: (uTimeout: UINT;
          szInfoTitle: array [0..63] of AnsiChar;
          dwInfoFlags: DWORD;
          guidItem: TGUID);
      1: (uVersion: UINT);
  end;
  {$EXTERNALSYM TabfNotifyIconDataAVer6}
  PabfNotifyIconDataAVer6 = ^TabfNotifyIconDataAVer6;
  {$EXTERNALSYM PabfNotifyIconDataAVer6}

  TabfNotifyIconDataWVer6  = packed record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of WideChar;
    case Integer of
      0: (uTimeout: UINT;
          szInfoTitle: array [0..63] of WideChar;
          dwInfoFlags: DWORD;
          guidItem: TGUID);
      1: (uVersion: UINT);
  end;
  {$EXTERNALSYM TabfNotifyIconDataWVer6}
  PabfNotifyIconDataWVer6 = ^TabfNotifyIconDataWVer6;
  {$EXTERNALSYM PabfNotifyIconDataWVer6}

  EabfTrayIcon = class(EabfException);

  TabfTrayIconInfoType = (tiiNone, tiiInfo, tiiWarning, tiiError, tiiUser);

  TabfTrayIcon = class(TabfCustomTimer)
  private
    FEnabled: Boolean;
    FHint: WideString;
    FIcon: TIcon;
    FIconFlags: Integer;
    FIconID: Integer;
    FImageList: TImageList;
    FImageIndex: TImageIndex;
    FImageListChangeLink: TChangeLink;
    FMinimizeOnStart: Boolean;
    FMinimizeToTray: Boolean;
    FNewAppProc: Pointer;
    FNotifyIconData: Pointer;
    FPopupMenu: TPopupMenu;
    FPopupByLeft: Boolean;
    FPressed: Boolean;
    FOldAppProc: Pointer;
    FShowHint: Boolean;
    FVisible: Boolean;
    FWasHidden: Boolean;
  // Event fields
    FOnMinimize: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPopupMenu: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnCycleIcons: TNotifyEvent;
    FVisibleOnDesigning: Boolean;
    FInfoNoSound: Boolean;
    FInfoText: WideString;
    FInfoTimeOut: Integer;
    FInfoTitle: WideString;
    FInfoType: TabfTrayIconInfoType;
    FOnBalloonTimeout: TNotifyEvent;
    FOnBalloonShow: TNotifyEvent;
    FOnBalloonClick: TNotifyEvent;
  // Misc
    function InsertToTray: Boolean;
    function DeleteFromTray: Boolean;
    procedure SetIconByImageIndex;
    function  IsIconStored: Boolean;
    procedure DummyOnTimer(ASender: TObject);
    procedure ImageListChange(ASender: TObject);
  // Properties Get/Set
    function  GetCycleIcons: Boolean;
    procedure SetCycleIcons(AValue: Boolean);
    function  GetCycleInterval: Cardinal;
    procedure SetCycleInterval(AValue: Cardinal);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHint(const AValue: WideString);
    procedure SetIcon(const AValue: TIcon);
    procedure SetIconID(AValue: Integer);
    procedure SetImageList(const AValue: TImageList);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetShowHint(AValue: Boolean);
    procedure SetPopupMenu(const AValue: TPopupMenu);
    procedure SetVisible(AValue: Boolean);
    procedure SetVisibleOnDesigning(AValue: Boolean);
    procedure SetInfoNoSound(AValue: Boolean);
    procedure SetInfoText(const AValue: WideString);
    procedure SetInfoTitle(const AValue: WideString);
    procedure SetInfoTimeOut(AValue: Integer);
    procedure SetInfoType(AValue: TabfTrayIconInfoType);
  protected
    function CallShellNotifyIcon(AMessage: DWORD): Boolean; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    procedure Timer; override;
    procedure WndProc(var Message: TMessage); override;
  // Taskbar routines
    function PrepareNotifyIconData: Boolean; virtual;
  // Hook routines
    procedure HookApp;
    procedure UnhookApp;
    procedure HookAppProc(var Message: TMessage); virtual;
  // Events handlers
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); dynamic;
    procedure Click;  dynamic;
    procedure DblClick; dynamic;
    procedure DoMinimize; dynamic;
    property NewAppProc: Pointer read FNewAppProc;
    property OldAppProc: Pointer read FOldAppProc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Actions
    procedure Update; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure ShowPopup; virtual;
    procedure ShowMainForm; virtual;
    procedure HideMainForm; virtual;
  // Extended info
    function ShowCustomInfo(AType: TabfTrayIconInfoType; const ATitle,
      AText: WideString): Boolean; virtual;
    function ShowInfo: Boolean; virtual;
    function ShowBalloon: Boolean; virtual; // Alias
  published
  // Properties
    property About;
    property CycleIcons: Boolean read GetCycleIcons write SetCycleIcons
      default False;
    property CycleInterval: Cardinal read GetCycleInterval
      write SetCycleInterval default 200;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Hint: WideString read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property ImageList: TImageList read FImageList write SetImageList;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex
      default 0;
    property IconID: Integer read FIconID write SetIconID default 0;
    property MinimizeToTray: Boolean read FMinimizeToTray write FMinimizeToTray
      default False;
    property MinimizeOnStart: Boolean read FMinimizeOnStart
      write FMinimizeOnStart default False; // Is MainForm minimized on startup
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property PopupByLeft: Boolean read FPopupByLeft write FPopupByLeft
      default False; // Is popup menu shown by left click
    property ShowHint: Boolean read FShowHint write SetShowHint default True;
    property Visible: Boolean read FVisible write SetVisible default False;
    property VisibleOnDesigning: Boolean read FVisibleOnDesigning
      write SetVisibleOnDesigning default False;
    // Extended info
    property InfoNoSound: Boolean read FInfoNoSound write SetInfoNoSound
      default False;
    property InfoText: WideString read FInfoText write SetInfoText;
    property InfoTitle: WideString read FInfoTitle write SetInfoTitle;
    property InfoTimeout: Integer read FInfoTimeOut write SetInfoTimeOut
      default 10000;
    property InfoType: TabfTrayIconInfoType read FInfoType write SetInfoType
      default tiiNone;
  // Events
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnPopupMenu: TNotifyEvent read FOnPopupMenu write FOnPopupMenu;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnCycleIcons: TNotifyEvent read FOnCycleIcons write FOnCycleIcons;
    property OnBalloonShow: TNotifyEvent read FOnBalloonShow
      write FOnBalloonShow;
    property OnBalloonTimeout: TNotifyEvent read FOnBalloonTimeout
      write FOnBalloonTimeout;
    property OnBalloonClick: TNotifyEvent read FOnBalloonClick
      write FOnBalloonClick;
  end;{TabfTrayIcon = class(TabfCustomTimer)}


//==============================================================================
// TabfOneInstance
//==============================================================================
// Component allows easily create applications that have only one running copy
// at the same time.  Simple drop TabfOneInstance onto any form of application
// or create it at run-time and your can prevent the second application start.
// Applications are separated by Application.Title or Application.ExeName

  TabfOneInstancePerformCheckingType = (pcGlobal, pcForCurrentUserOnly);

  TabfOneInstanceShowMessage = procedure(Sender: TObject;
    var Terminate: Boolean) of object;

  TabfOneInstance = class(TabfComponent)
  private
    FActivatePrevInstance: Boolean;
    FPerformChecking: TabfOneInstancePerformCheckingType;
    FExeNameAsIdentifier: Boolean;
    FShowMessage: Boolean;
    FOnShowMessage: TabfOneInstanceShowMessage;
  protected
    FMessage: WideString;
    FIdentifier: WideString;
    FTerminateApplication: Boolean;
    procedure Loaded; override;
    procedure DoActivatePrevInstance; dynamic;
    procedure DoShowMessage; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    function AlreadyRun: Boolean; virtual;
    function Check: Boolean; virtual;
  published
  // Properties
    property About;
    property ActivatePrevInstance: Boolean read FActivatePrevInstance
      write FActivatePrevInstance default True;
    property ExeNameAsIdentifier: Boolean read FExeNameAsIdentifier
      write FExeNameAsIdentifier default False;
    property Message: WideString read FMessage write FMessage;
    property Identifier: WideString read FIdentifier write FIdentifier;
    property PerformChecking: TabfOneInstancePerformCheckingType
      read FPerformChecking write FPerformChecking default pcGlobal;
    property ShowMessage: Boolean read FShowMessage write FShowMessage
      default False;
  // Events
    property OnShowMessage: TabfOneInstanceShowMessage read FOnShowMessage
      write FOnShowMessage;
  end;{TabfOneInstance = class(TabfComponent)}


//==============================================================================
// TabfAutoRun
//==============================================================================
// Component allows create applications that automatically run at the system
// start-up. You can register and unregister autorun execution for any
// application using this component.

  TabfAutoRun = class(TabfComponent)
  private
    FAutoRun: Boolean;
    FFileName: TFileName;
    FKind: TabfAutoExecKind;
    procedure SetAutoRun(A: Boolean);
    procedure SetFileName(const A: TFileName);
    procedure SetKind(A: TabfAutoExecKind);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Register; virtual;
    procedure UnRegister; virtual;
  published
    property About;
    property AutoRun: Boolean read FAutoRun write SetAutoRun default False;
    property FileName: TFileName read FFileName write SetFileName;
    property Kind: TabfAutoExecKind read FKind write SetKind
      default aekMachineRun;
  end;{TabfAutoRun = class(TabfComponent)}


//==============================================================================
//  TabfShutdown
//==============================================================================
// Use TabfShutdown to precess PowerOff, Shutdown, Reboot, Logoff, Suspend or
// Hibernate action. The Force property determines forced suspension. If True,
// the Execute function sends a PBT_APMSUSPEND message to each application and
// driver, then immediately suspends operation. If False, sends a
// PBT_APMQUERYSUSPEND message to each application to request permission to
// suspend operation.

  TabfShutdownActionType = (aatPowerOff, aatShutdown, aatReboot, aatLogOff,
    aatSuspend, aatHibernate);

  TabfQueryShutdownEvent = procedure(Sender: TObject;
    var CanShutdown: Boolean) of object;

  TabfShutdown = class(TabfComponent)
  private
    FActionType: TabfShutdownActionType;
    FForce: Boolean;
    FOnQueryShutdown: TabfQueryShutdownEvent;
  protected
    procedure DoQueryShutdown(var CanShutdown: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; virtual;
  published
  // Properties
    property About;
    property ActionType: TabfShutdownActionType read FActionType
      write FActionType default aatPowerOff;
    property Force: Boolean read FForce write FForce default False;
  // Events
    property OnQueryShutdown: TabfQueryShutdownEvent read FOnQueryShutdown
      write FOnQueryShutdown;
  end;


//==============================================================================
// TabfWav
//==============================================================================
// Component for playing standard Wave (*.WAV) files or resources. Can play
// sounds form file on disk or from resource. Has ability store wave data
// in the *.dfm file.

  TabfWavPlayFrom = (pfFile, pfResource, pfDFM, pfSystemEvent);

  TabfWav = class(TabfCustomFileStorage)
  private
    FAsyncPlay: Boolean;
    FLooped: Boolean;
    FNoDefaultSound: Boolean;
    FNoStopSound: Boolean;
    FPlayFrom: TabfWavPlayFrom;
    FResourceName: string;
    FOnPlayAfter: TNotifyEvent;
    FOnPlayBefore: TNotifyEvent;
    FNoWaitIfBusy: Boolean;
    FEventAlias: string;
  // Properties stored flags
    function IsEventAliasStored: Boolean;
    function IsFileNameStored: Boolean;
    function IsResourceNameStored: Boolean;
  protected
    FPlaying: Boolean;
    FSoundPointer: PChar;
    FSoundOptions: DWORD;
    FSoundModule: HMODULE;
    procedure UpdateSoundOptions; virtual;
  // Properties Get/Set
    procedure SetAsyncPlay(Value: Boolean); virtual;
    procedure SetEventAlias(const Value: string); virtual;
    procedure SetFileName(const Value: TFileName); override;
    procedure SetLooped(Value: Boolean); virtual;
    procedure SetNoWaitIfBusy(Value: Boolean); virtual;
    procedure SetPlayFrom(Value: TabfWavPlayFrom); virtual;
    procedure SetResourceName(const Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  // Wave routines
    procedure Play; virtual;
    procedure Stop; virtual;
  // Properties
    property Playing: Boolean read FPlaying;
  published
  // Properties
    property About;
    property AsyncPlay: Boolean read FAsyncPlay write SetAsyncPlay default True;
    property DataSize;
    property EventAlias: string read FEventAlias write SetEventAlias
      stored IsEventAliasStored;
    property FileName: TFileName read FFileName write SetFileName
      stored IsFileNameStored;
    property Looped: Boolean read FLooped write SetLooped default False;
    property NoDefaultSound: Boolean read FNoDefaultSound write FNoDefaultSound
      default True;
    property NoStopSound: Boolean read FNoStopSound write FNoStopSound
      default False;
    property NoWaitIfBusy: Boolean read FNoWaitIfBusy write SetNoWaitIfBusy
      default True;
    property PlayFrom: TabfWavPlayFrom read FPlayFrom write SetPlayFrom
      default Low(TabfWavPlayFrom);
    property ResourceName: string read FResourceName write SetResourceName
      stored IsResourceNameStored;
  // Events
    property OnPlayAfter: TNotifyEvent read FOnPlayAfter write FOnPlayAfter;
    property OnPlayBefore: TNotifyEvent read FOnPlayBefore write FOnPlayBefore;
  end;{TabfWav = class(TabfCustomFileStorage)}


//==============================================================================
// TabfCustomFolderMonitor
//==============================================================================
// Prototype of folder changes notifier

  TFolderMonitorFilter = (fmfFileNameChange, fmfDirNameChange,
    fmfAttributeChange, fmfSizeChange, fmfWriteChange, fmfSecurityChange);
  TFolderMonitorFilters = set of TFolderMonitorFilter;

  TabfCustomFolderMonitor = class(TabfComponent)
  private
    FActive: Boolean;
    FFolder: string;
    FFilters: TFolderMonitorFilters;
    FWatchSubTree: Boolean;
    FOnChange: TThreadMethod;
    function GetNotifyOptionFlags: DWORD;
  // Properties Get/Set
    procedure SetActive(Value: Boolean);
    procedure SetFilters(Value: TFolderMonitorFilters);
    procedure SetFolder(const Value: string);
    procedure SetWatchSubTree(Value: Boolean);
    procedure SetOnChange(Value: TThreadMethod);
  protected
    FThread: TabfFolderMonitorThread;
    procedure Loaded; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Change; virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
  // Properties
    property Active: Boolean read FActive write SetActive default False;
    property NotifyFilters: TFolderMonitorFilters read FFilters write SetFilters;
    property Folder: string read FFolder write SetFolder;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
  // Events
    property OnChange: TThreadMethod read FOnChange write SetOnChange;
  end;


//==============================================================================
// TabfFolderMonitor
//==============================================================================
// Folder changes notifier

  TabfFolderMonitor = class(TabfCustomFolderMonitor)
  published
  // Properties
    property About;
    property Active;
    property Folder;
    property NotifyFilters;
    property WatchSubTree;
  // Events
    property OnChange;
  end;


//==============================================================================
// TabfCustomRegistryMonitor
//==============================================================================
// Prototype of registry changes notifier

  TabfRegistryMonitorFilter = (rmfSubTreeChange, rmfAttributeChange,
    rmfValueChange, rmfSecurityChange);
  TabfRegistryMonitorFilters = set of TabfRegistryMonitorFilter;

  TabfCustomRegistryMonitor = class(TabfComponent)
  private
    FActive: Boolean;
    FFilters: TabfRegistryMonitorFilters;
    FWatchSubTree: Boolean;
    FOnChange: TThreadMethod;
    function GetNotifyOptionFlags: DWORD;
    function GetRootKey: HKEY;
  // Properties Get/Set
    procedure SetActive(Value: Boolean);
    procedure SetFilters(Value: TabfRegistryMonitorFilters);
    procedure SetRegistryKey(const Value: string);
    procedure SetRootKey(Value: TabfRegRootKey);
    procedure SetWatchSubTree(Value: Boolean);
    procedure SetOnChange(Value: TThreadMethod);
  protected
    FThread: TabfRegistryMonitorThread;
    FRegistryKey: string;
    FRootKey: TabfRegRootKey;
    procedure Loaded; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Change; virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
  // Properties
    property Active: Boolean read FActive write SetActive default False;
    property NotifyFilters: TabfRegistryMonitorFilters read FFilters write SetFilters;
    property RegistryKey: string read FRegistryKey write SetRegistryKey;
    property RootKey: TabfRegRootKey read FRootKey write SetRootKey default rrkCurrentUser;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
  // Events
    property OnChange: TThreadMethod read FOnChange write SetOnChange;
  end;


//==============================================================================
// TabfFolderMonitor
//==============================================================================
// Folder changes notifier

  TabfRegistryMonitor = class(TabfCustomRegistryMonitor)
  published
  // Properties
    property About;
    property Active;
    property RegistryKey;
    property RootKey;
    property NotifyFilters;
    property WatchSubTree;
  // Events
    property OnChange;
  end;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  Consts, Registry, MMSystem, ShlObj,
// ABF VCL
  abfConsts, abfVclConsts, abfStrUtils, abfGraphics;

//==============================================================================
// Private variables
//==============================================================================

var
  WM_TaskbarRestart: UINT;

//==============================================================================
// TabfComponent
//==============================================================================
// Base class of all non-visual TabfXXX components.

constructor TabfComponent.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  FParentHandle := 0;
end;

//------------------------------------------------------------------------------

function TabfComponent.GetParentHandle: THandle;
begin
  Result := FParentHandle;
  if (Result = 0) and Assigned(Owner) then
  begin
    if (Owner is TWinControl) then Result := TWinControl(Owner).Handle else
    if (Owner is TApplication) then Result := TApplication(Owner).Handle;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfComponent.SetParentHandle(AValue: THandle);
begin
  FParentHandle := AValue;
end;

//==============================================================================
// TabfCustomThreadComponent
//==============================================================================
// TabfCustomThreadComponent is a base type of all components that has own
// thread.
// 03/10/2001
{ TabfCustomThreadComponent }

type
  THackThread = class(TThread);

//------------------------------------------------------------------------------

constructor TabfCustomThreadComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPriority := tpNormal;
  FReturnValue := 0;
  FSuspended := True;
  if not (csDesigning in ComponentState) then
  begin
    CreateThread;
    Thread.OnExecute := DoExecute;
  end;
end;

//------------------------------------------------------------------------------

destructor TabfCustomThreadComponent.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    Thread.OnExecute := nil;
    Thread.Terminate;
    if Thread.Suspended then Thread.Resume;
    Thread.Free;
  end;
  FThread := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.CreateThread;
begin
  if (csDesigning in ComponentState) then Exit;
  FThread := TabfEventThread.Create(FSuspended);
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.Execute;
begin
  if (csDesigning in ComponentState) then Exit;
  TerminateHard;
  Thread.Resume;
end;

//------------------------------------------------------------------------------
// Executes a method call within the main VCL thread.

procedure TabfCustomThreadComponent.Synchronize(Method: TThreadMethod);
begin
  if (csDesigning in ComponentState) then Exit;
  THackThread(Thread).Synchronize(Method);
end;

//------------------------------------------------------------------------------
// Executes a method call within the main VCL thread. Data passed to the Method
// as parameter.

procedure TabfCustomThreadComponent.SynchronizeEx(Method: TabfThreadMethodEx;
  Data: Pointer);
begin
  if (csDesigning in ComponentState) then Exit;
  if Assigned(FSyncMethod) then Exit; // Already do something
  FSyncMethod := Method;
  FSyncData   := Data;
  try
    THackThread(FThread).Synchronize(InternalSynchronizeEx);
  finally
    FSyncMethod := nil;
    FSyncData := nil;
  end;
end;


//==============================================================================
// Thread routines

procedure TabfCustomThreadComponent.Suspend;
begin
  if (csDesigning in ComponentState) then Exit;
  Thread.Suspend;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.Resume;
begin
  if (csDesigning in ComponentState) then Exit;
  Thread.Resume;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.Terminate;
begin
  if (csDesigning in ComponentState) then Exit;
  Thread.Terminate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.TerminateHard;
var
  T: TabfEventThread;
begin
  if (csDesigning in ComponentState) then Exit;

// Create new tread
  T := TabfEventThread.Create(True);
  try
    T.Priority    := Priority;
    T.OnExecute   := DoExecute;
    T.OnTerminate := OnTerminate;
  except
    T.Free;
    raise;
  end;

// Terminate previous tread
  TerminateThread(Thread.Handle, 0);
{$IfDef D6}
  Thread.FreeOnTerminate := True;
  Thread.Terminate;
{$Else D6}
  Thread.Free;
{$EndIf D6}

// Assign new tread
  FThread := T;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.TerminateWaitFor: Integer;
begin
  Result := 0;
  if (csDesigning in ComponentState) then Exit;
  Terminate;
  Result := WaitFor;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.WaitFor: Integer;
begin
  Result := 0;
  if (csDesigning in ComponentState) then Exit;
  Result := Thread.WaitFor;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then Exit;
  SetSuspended(FSuspended);
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.DoExecute(Sender: TObject);
begin
  try
    if Assigned(OnExecute) then OnExecute(Self);
  except
    SynchronizeEx(DoException, ExceptObject);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.DoException(Data: Pointer);
var
  Msg: string;
  Sender: TObject;
begin
  Sender := TObject(Data);
  if Assigned(OnException) then OnException(Sender) else
  begin
    Msg := Format(SabfTreadComponent_Exception, [Name, Sender.ClassName,
      Exception(Sender).Message]);
    MessageBoxEx(Application.Handle, PChar(Msg), PChar(Application.Title),
      MB_ICONERROR or MB_SETFOREGROUND or MB_APPLMODAL, 0);
  end;
end;

//------------------------------------------------------------------------------
// Calls SyncMethod and passes SyncData as parameter

procedure TabfCustomThreadComponent.InternalSynchronizeEx;
begin
  if (csDesigning in ComponentState) then Exit;
  FSyncMethod(FSyncData);
end;


//==============================================================================
// Properties Get/Set

function TabfCustomThreadComponent.GetHandle: THandle;
begin
  Result := 0;
  if (csDesigning in ComponentState) then Exit;
  Result := Thread.Handle;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetPriority: TThreadPriority;
begin
  if not (csDesigning in ComponentState) then Result := Thread.Priority
  else Result := FPriority;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.SetPriority(Value: TThreadPriority);
begin
  if not (csDesigning in ComponentState) then Thread.Priority := Value
  else FPriority := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetReturnValue: Integer;
begin
  if not (csDesigning in ComponentState) then
    Result := THackThread(Thread).ReturnValue
  else
    Result := FReturnValue;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.SetReturnValue(Value: Integer);
begin
  if not (csDesigning in ComponentState) then
    THackThread(Thread).ReturnValue := Value
  else
    FReturnValue := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetSuspended: Boolean;
begin
  if not (csDesigning in ComponentState) then Result := Thread.Suspended
  else Result := FSuspended;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.SetSuspended(Value: Boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (csLoading in ComponentState) then FSuspended := Value
    else Thread.Suspended := Value;
  end else
    FSuspended := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetSynchronized: Boolean;
begin
  if not (csDesigning in ComponentState) then Result := Thread.Synchronized
  else Result := FSynchronized;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.SetSynchronized(Value: Boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (csLoading in ComponentState) then FSynchronized := Value
    else Thread.Synchronized := Value;
  end else
    FSynchronized := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetTerminated: Boolean;
begin
  Result := True;
  if (csDesigning in ComponentState) then Exit;
  Result := THackThread(Thread).Terminated;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetThreadID: THandle;
begin
  Result := 0;
  if (csDesigning in ComponentState) then Exit;
  Result := Thread.ThreadID;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetOnTerminate: TNotifyEvent;
begin
  if not (csDesigning in ComponentState) then Result := Thread.OnTerminate
  else Result := FOnTerminate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.SetOnTerminate(Value: TNotifyEvent);
begin
  if not (csDesigning in ComponentState) then Thread.OnTerminate := Value
  else FOnTerminate := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadComponent.GetOnFinish: TNotifyEvent;
begin
  if not (csDesigning in ComponentState) then Result := Thread.OnFinish
  else Result := FOnFinish;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadComponent.SetOnFinish(const Value: TNotifyEvent);
begin
  if not (csDesigning in ComponentState) then Thread.OnFinish := Value
  else FOnFinish := Value;
end;


//==============================================================================
// TabfCustomThreadTimer
//==============================================================================
// TabfCustomThreadTimer is a base type of all threaded timers.
// 03/13/2001
{ TabfCustomThreadTimer }

constructor TabfCustomThreadTimer.Create(AOwner: TComponent);
begin
  FInterval := 1000;
  inherited Create(AOwner);
end;

//------------------------------------------------------------------------------

destructor TabfCustomThreadTimer.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadTimer.CreateThread;
begin
  if (csDesigning in ComponentState) then Exit;
  FThread := TabfTimerThread.Create(FSuspended);
end;

//------------------------------------------------------------------------------

function TabfCustomThreadTimer.GetEnabled: Boolean;
begin
  Result := not Suspended;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadTimer.SetEnabled(Value: Boolean);
begin
  Suspended := not Value;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadTimer.GetInterval: Cardinal;
begin
  if not (csDesigning in ComponentState) then Result := Thread.Interval
  else Result := FInterval;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadTimer.SetInterval(Value: Cardinal);
begin
  if not (csDesigning in ComponentState) then Thread.Interval := Value
  else FInterval := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomThreadTimer.GetThread: TabfTimerThread;
begin
  Result := TabfTimerThread(FThread);
end;

//------------------------------------------------------------------------------

function TabfCustomThreadTimer.GetOnTimer: TNotifyEvent;
begin
  Result := OnExecute;
end;

//------------------------------------------------------------------------------

procedure TabfCustomThreadTimer.SetOnTimer(Value: TNotifyEvent);
begin
  OnExecute := Value;
end;


//==============================================================================
// TabfCustomTimer
//==============================================================================
// TabfCustomTimer is a base type of all components that contains internal
// timer.
// Date: 04/01/2000
{ TabfCustomTimer }

constructor TabfCustomTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  FHandle := AllocateHWnd(WndProc);
end;

//------------------------------------------------------------------------------

destructor TabfCustomTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  DeallocateHWnd(FHandle);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomTimer.WndProc(var Message: TMessage);
begin
  with Message do
    if Msg = WM_TIMER then
      try
        Timer;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

//------------------------------------------------------------------------------

procedure TabfCustomTimer.UpdateTimer;
begin
  KillTimer(FHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
end;

//------------------------------------------------------------------------------

procedure TabfCustomTimer.SetEnabled(A: Boolean);
begin
  if FEnabled = A then Exit;
  FEnabled := A;
  UpdateTimer;
end;

//------------------------------------------------------------------------------

procedure TabfCustomTimer.SetInterval(A: Cardinal);
begin
  if FInterval = A then Exit;
  FInterval := A;
  UpdateTimer;
end;

//------------------------------------------------------------------------------

procedure TabfCustomTimer.SetOnTimer(A: TNotifyEvent);
begin
  FOnTimer := A;
  UpdateTimer;
end;

//------------------------------------------------------------------------------

procedure TabfCustomTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;


//==============================================================================
// TabfWndProcHookList
//==============================================================================
// List of TabfCustomWndProcHook and its descendants. Used internally for right
// multi-hooking of the same WinControl.
// Date: 06/17/2000

type
  TabfWndProcHookList = class(TList)
  protected
    function GetDefaultWndProc(AHandle: THandle): Pointer;
    procedure ListByHandle(AList: TList; AHandle: THandle);
  public
    procedure Hook(const AHook: TabfCustomWndProcHook);
    procedure UnHook(const AHook: TabfCustomWndProcHook);
  end;

var
  _WndProcHookList: TabfWndProcHookList;

//==============================================================================
// TabfWndProcHookList
//==============================================================================
{TabfWndProcHookList}

function TabfWndProcHookList.GetDefaultWndProc(AHandle: THandle): Pointer;
var
  i: Integer;
begin
  Result := Pointer(GetWindowLong(AHandle, GWL_WNDPROC));
  for i := 0 to Count - 1 do
    with TabfCustomWndProcHook(Items[i]) do
      if Active and (Handle = AHandle) and Assigned(FOldWndProc) then
      begin
        Result := FOldWndProc;
        Break;
      end;
end;

//------------------------------------------------------------------------------

procedure TabfWndProcHookList.ListByHandle(AList: TList; AHandle: THandle);
var
  i: Integer;
begin
  if not Assigned(AList) then Exit;
  AList.Clear;
  for i := 0 to Count - 1 do
    with TabfCustomWndProcHook(Items[i]) do
      if Active and (Handle = AHandle) then AList.Add(Items[i])
end;

//------------------------------------------------------------------------------

procedure TabfWndProcHookList.Hook(const AHook: TabfCustomWndProcHook);
var
  DefaultProc: Pointer;
  List: TList;
  i: Integer;
  AHandle: THandle;
begin
  AHandle := AHook.Handle;
  if (AHandle = INVALID_HANDLE_VALUE) or (AHandle = 0) then Exit;
  DefaultProc := GetDefaultWndProc(AHandle);
  AHook.FActive := True;
  List := TList.Create;
  try
    ListByHandle(List, AHandle);
  // Don't check List.Count because AHook should be in the List in any way
  {!!!}
  // Normalize the List
    TabfCustomWndProcHook(List[0]).FOldWndProc := DefaultProc;
    for i := 1 to List.Count - 1 do
      TabfCustomWndProcHook(List[i]).FOldWndProc :=
        TabfCustomWndProcHook(List[i - 1]).FNewWndProc;
  // Set proc of the last item to the window
    SetWindowLong(AHandle, GWL_WNDPROC,
      LongInt(TabfCustomWndProcHook(List[List.Count - 1]).FNewWndProc));
  finally
    List.Free;
  end;
end;{procedure TabfWndProcHookList.Hook}

//------------------------------------------------------------------------------

procedure TabfWndProcHookList.UnHook(const AHook: TabfCustomWndProcHook);
var
  DefaultProc: Pointer;
  List: TList;
  i: Integer;
  AHandle: THandle;
begin
  AHandle := AHook.Handle;
  if (AHandle = INVALID_HANDLE_VALUE) or (AHandle = 0) then Exit;
  DefaultProc := GetDefaultWndProc(AHandle);
  AHook.FActive := False;
  List := TList.Create;
  try
    ListByHandle(List, AHandle);
  // If there are no items in List, just set the default proc to the window
    if List.Count < 1 then
    begin
      SetWindowLong(AHandle, GWL_WNDPROC, LongInt(DefaultProc));
      Exit;
    end;
  // Normalize the List
    TabfCustomWndProcHook(List[0]).FOldWndProc := DefaultProc;
    for i := 1 to List.Count - 1 do
      TabfCustomWndProcHook(List[i]).FOldWndProc :=
        TabfCustomWndProcHook(List[i - 1]).FNewWndProc;
  // Set proc of the last item to the window
    SetWindowLong(AHandle, GWL_WNDPROC,
      LongInt(TabfCustomWndProcHook(List[List.Count - 1]).FNewWndProc));
  finally
    List.Free;
  end;
end;{procedure TabfWndProcHookList.UnHook}


//==============================================================================
// TabfCustomWndProcHook
//==============================================================================
// Use TabfCustomWndProcHook component or its descendants to hook a WndProc of
// the control specified in the WinControl property. If no WinControl specified,
// the form where the component is placed will be hooked.
// Date: 06/17/2000
{ TabfCustomWndProcHook }

constructor TabfCustomWndProcHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FAutoDrag := False;
  FNewWndProc := nil;
  FOldWndProc := nil;
// Create the list if it was not used before
  if not Assigned(_WndProcHookList) then
    _WndProcHookList := TabfWndProcHookList.Create;
// Add self to the list
  _WndProcHookList.Add(Self);
end;

//------------------------------------------------------------------------------

destructor TabfCustomWndProcHook.Destroy;
begin
  Active := False;
  _WndProcHookList.Remove(Self);
  inherited Destroy;
// Free the list if it is empty
  if _WndProcHookList.Count < 1 then FreeAndNil(_WndProcHookList);
end;

//------------------------------------------------------------------------------

procedure TabfCustomWndProcHook.Loaded;
begin
  inherited Loaded;
  if Active then Hook;
end;

//------------------------------------------------------------------------------

procedure TabfCustomWndProcHook.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = WinControl then WinControl := nil else
    if AComponent = Self then Active := False;
  end;
end;

//==============================================================================
// Hook routines

procedure TabfCustomWndProcHook.Hook;
begin
  if csDesigning in ComponentState then
  begin
    FActive := True;
    Exit;
  end;
  if not Assigned(FNewWndProc) then FNewWndProc := MakeObjectInstance(WndProc);
  _WndProcHookList.Hook(Self);
  FActive := True;
end;

//------------------------------------------------------------------------------

procedure TabfCustomWndProcHook.UnHook;
begin
  if csDesigning in ComponentState then
  begin
    FActive := False;
    Exit;
  end;
  _WndProcHookList.UnHook(Self);
  if Assigned(FNewWndProc) then FreeObjectInstance(FNewWndProc);
  FNewWndProc := nil;
  FOldWndProc := nil;
  FActive := False;
end;

//------------------------------------------------------------------------------

procedure TabfCustomWndProcHook.WndProc(var Message: TMessage);
var
  Handled: Boolean;

  procedure _PerformMessages;
  begin
    with Message do
    begin
      case Msg of
        WM_LButtonDown:
          if AutoDrag and IsWindow(Handle) then
          begin
            ReleaseCapture;
            SendMessage(Handle, WM_SysCommand, SC_Move or htCaption, 0);
            Exit;
          end;
      end;{case Msg of}

    // Call previous message proc
      Result := CallWindowProc(FOldWndProc, Handle, Msg, wParam, lParam);
    end;{with Message do}
  end;{Internal procedure _PerformMessages}

begin
  if (Handle = INVALID_HANDLE_VALUE) or (Handle = 0) then Exit;
  Handled := False;
  if Assigned(FOnMessageBefore) then FOnMessageBefore(Self, Message, Handled);
  if not Handled then _PerformMessages;
  if Assigned(FOnMessageAfter) then FOnMessageAfter(Self, Message, Handled);
end;{procedure TabfCustomWndProcHook.WndProc}


//==============================================================================
// Properties Get/Set

procedure TabfCustomWndProcHook.SetActive(A: Boolean);
begin
  if FActive = A then Exit;
  if A then Hook else UnHook;
end;

//------------------------------------------------------------------------------

function TabfCustomWndProcHook.GetHandle: THandle;
var
  WC: TWinControl;
begin
  if (FHandle <> 0) and (FHandle <> INVALID_HANDLE_VALUE) then
  begin
    Result := FHandle;
    Exit;
  end;
  WC := WinControl;
  if Assigned(WC) and (WC.HandleAllocated or
    ([csReading, csDestroying] * WC.ComponentState = [])) then
    Result := WC.Handle
  else
    Result := INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------

procedure TabfCustomWndProcHook.SetHandle(A: THandle);
var
  OldValue: Boolean;
begin
  if A = FHandle then Exit;
  OldValue := Active;
  try
    Active := False;
    FHandle := A;
  finally
    Active := OldValue;
  end;
  if Assigned(FOnChangeHandle) then FOnChangeHandle(Self);
end;

//------------------------------------------------------------------------------

function TabfCustomWndProcHook.GetWinControl: TWinControl;
begin
  Result := nil;
  if Assigned(FWinControl) then Result := FWinControl else
  if Assigned(Owner) and (Owner is TWinControl) then
    Result := TWinControl(Owner);
end;

//------------------------------------------------------------------------------

procedure TabfCustomWndProcHook.SetWinControl(const A: TWinControl);
var
  OldValue: Boolean;
begin
  if FWinControl = A then Exit;
  OldValue := Active;
  try
    Active := False;
    FWinControl := A;
    if Assigned(WinControl) and (WinControl <> Owner) then
      WinControl.FreeNotification(Self);
  finally
    Active := OldValue;
  end;
  if Assigned(FOnChangeWinControl) then FOnChangeWinControl(Self);
end;


//==============================================================================
//  TabfFileOperation
//==============================================================================
// TabfFileOperation is a component that provides an easy way to perform
// different operations with files. Allows to copy, move, delete and other file
// operations.
// Date: 07/07/2007

constructor TabfFileOperation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperation := fotCopy;
  FOptions := [fofAllowUndo, fofFilesOnly, fofRenameCollision];
  FFileList := TabfWideStringList.Create;
end;

//------------------------------------------------------------------------------

destructor TabfFileOperation.Destroy;
begin
  FreeAndNil(FFileList);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfFileOperation.Copy: Boolean;
begin
  Result := ExecuteOperation(fotCopy);
end;

//------------------------------------------------------------------------------

function TabfFileOperation.Delete: Boolean;
begin
  Result := ExecuteOperation(fotDelete);
end;

//------------------------------------------------------------------------------

function TabfFileOperation.Execute: Boolean;
begin
  Result := ExecuteOperation(Operation);
end;

//------------------------------------------------------------------------------

function TabfFileOperation.Move: Boolean;
begin
  Result := ExecuteOperation(fotMove);
end;

//------------------------------------------------------------------------------

function TabfFileOperation.Rename: Boolean;
begin
  Result := ExecuteOperation(fotRename);
end;

//------------------------------------------------------------------------------

function TabfFileOperation.ExecuteOperation(
  OperationType: TabfFileOperationType): Boolean;
var
  _SHFOS: TSHFileOpStruct;
  SHFOS: TSHFileOpStruct absolute _SHFOS;
  SHFOSW: TSHFileOpStructW absolute _SHFOS;
  MultipleFiles: Boolean;
  SrcStr, DstStr, ErrStr, SrcName, DstName: TabfWideString;
  TempSeparator: TabfChar;
  TempSrcStr, TempDstStr: string;
  TempSrcStrW, TempDstStrW: WideString;  
  i, Position, ARes: Integer;
begin
  if FFileList.Count = 0 then
  begin
    Result := True;
    Exit;
  end;

{$IFDEF D9}
  TempSeparator := FFileList.NameValueSeparator;
{$ELSE}  
  TempSeparator := '=';
{$ENDIF}

  FillChar(_SHFOS, SizeOf(_SHFOS), 0);
  with _SHFOS do
  try
    Wnd := ParentHandle;

    case OperationType of
      fotCopy  : wFunc := FO_COPY;
      fotDelete: wFunc := FO_DELETE;
      fotMove  : wFunc := FO_MOVE;
      fotRename: wFunc := FO_RENAME;
    end;

  // Prepare fFlags
    MultipleFiles := (Pos(TempSeparator, FFileList.Text) > 0);
    fFlags := 0;
    if fofAllowUndo in FOptions       then fFlags := fFlags or FOF_ALLOWUNDO;
    if fofFilesOnly in FOptions       then fFlags := fFlags or FOF_FILESONLY;
    if fofNoConfirmation in FOptions  then fFlags := fFlags or FOF_NOCONFIRMATION;
    if fofNoConfirmMkDir in FOptions  then fFlags := fFlags or FOF_NOCONFIRMMKDIR;
    if fofNoErrorUI in FOptions       then fFlags := fFlags or FOF_NOERRORUI;
    if fofRenameCollision in FOptions then fFlags := fFlags or FOF_RENAMEONCOLLISION;
    if fofSilent in FOptions          then fFlags := fFlags or FOF_SILENT;
    if fofSimpleProgress in FOptions  then fFlags := fFlags or FOF_SIMPLEPROGRESS;
    if MultipleFiles                  then fFlags := fFlags or FOF_MULTIDESTFILES;

  // Set other fields
    fAnyOperationsAborted := False;
    hNameMappings := nil;

    ErrStr := '';

  // Prepare Source and if it is needed Destination strings
    SrcStr := '';
    DstStr := '';
    with FFileList do
      for i := 0 to Count - 1 do
      begin
        if MultipleFiles then
        begin
          Position := Pos(TempSeparator, Strings[i]);
          if Position <= 0 then Position := MaxInt - 1;

        {$IFDEF D9}
          SrcName := abfTrimW(System.Copy(Strings[i], 1, Position - 1), ' "');
        {$ELSE}
          SrcName := abfTrim(System.Copy(Strings[i], 1, Position - 1),
            [' ', '"']);
        {$ENDIF}

          SrcStr := SrcStr + SrcName + TabfChar(#0);

        {$IFDEF D9}
          DstName := abfTrimW(System.Copy(Strings[i], Position + 1, MaxInt),
            ' "');
          DstName := abfGetFileNameMatchedMaskW(SrcName,
            abfConcatRelativePath(DestFolder, DstName));
        {$ELSE}
          DstName := abfTrim(System.Copy(Strings[i], Position + 1, MaxInt),
            [' ', '"']);
          DstName := abfGetFileNameMatchedMask(SrcName,
            abfConcatRelativePath(DestFolder, DstName));
        {$ENDIF}

          DstStr := DstStr + DstName + TabfChar(#0);
        end else
          SrcStr := SrcStr + Strings[i] + TabfChar(#0);
      end;{for i := 0 to Count - 1 do}


    if not MultipleFiles then
      DstStr := DestFolder;

    SrcStr := SrcStr + TabfChar(#0);
    DstStr := DstStr + TabfChar(#0);

    if IsWinNT then
    begin
      lpszProgressTitle := Pointer(WideString(FProgressTitle));

      TempSrcStrW := SrcStr;
      TempDstStrW := DstStr;

      pFrom := Pointer(TempSrcStrW);
      pTo := Pointer(TempDstStrW);

      ARes := SHFileOperationW(SHFOSW);
    end else
    begin
      lpszProgressTitle := Pointer(string(FProgressTitle));

      TempSrcStr := SrcStr;
      TempDstStr := DstStr;

      pFrom := Pointer(TempSrcStr);
      pTo := Pointer(TempDstStr);

      ARes := SHFileOperation(SHFOS);
    end;

    Result := (ARes = 0) and (not fAnyOperationsAborted);

    try
      if (ARes <> 0) and (ARes <> ERROR_NO_MORE_FILES) then
      begin
        ErrStr := GetSystemError(ARes);
        Error(ErrStr);
      end;
    finally
      if Assigned(hNameMappings) then
        ShFreeNameMappings(Cardinal(hNameMappings));
    end;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

function TabfFileOperation.GetSystemError(ErrorCode: Integer): TabfWideString;
const
  MinBufLen = 1024;
var
  Buf: Pointer;
begin
  Result := '';
  if ErrorCode = 0 then Exit;

  Buf := nil;
  try
    if IsWinNT then
    begin
      FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER,
        nil, ErrorCode, 0, Buf, MinBufLen, nil);
      Result := TabfWideString(PWideChar(Buf));
    end else
    begin
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER,
        nil, ErrorCode, 0, Buf, MinBufLen, nil);
      Result := TabfWideString(PChar(Buf));
    end;
  finally
    if Assigned(Buf) then
      LocalFree(Cardinal(Buf));
  end;
end;

//------------------------------------------------------------------------------

function TabfFileOperation.Error(const S: TabfWideString): Boolean;
begin
  Result := False;
  if Assigned(FOnError) then FOnError(Self, S, Result);
end;

//------------------------------------------------------------------------------

procedure TabfFileOperation.SetFileList(const A: TabfWideStrings);
begin
  FFileList.Assign(A);
end;


//==============================================================================
// TabfFileAssociation
//==============================================================================
// Component allows read and write file association data.
{ TabfFileAssociation }

constructor TabfFileAssociation.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FLargeIcon := TIcon.Create;
  FSmallIcon := TIcon.Create;
  FReadOnly := True;
  FIconIndex := 0
end;

//------------------------------------------------------------------------------

destructor TabfFileAssociation.Destroy;
begin
  FreeAndNil(FSmallIcon);
  FreeAndNil(FLargeIcon);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfFileAssociation.ReadAssociation: Boolean;
var
  i, j: Integer;
  DefaultIcon: Boolean;
  LH, SH: hIcon;
  AKey: string;
  Reg: TRegistry;
begin
  Result := False;
  if [csLoading, csDestroying] * ComponentState <> [] then Exit;

  FExtDescription := '';
  FFileDescription := '';
  FIconFile := '';
  FIconIndex := 0;
  FLargeIcon.Assign(nil);
  FSmallIcon.Assign(nil);
  FOpenCommand := '';
  FOpenCommandDescription := '';
  FOpenNewCommand := '';
  FOpenNewCommandDescription := '';
  FEditCommand := '';
  FEditCommandDescription := '';
  FPrintCommand := '';
  FPrintCommandDescription := '';
  FPrintToCommand := '';
  FPrintToCommandDescription := '';

  if (FExtension = '') or (FExtension = '.') then Exit;

// Use default icon if there is no data
  DefaultIcon := True;

  Reg := TRegistry.Create;
  with Reg do
  try
{$IfDef D5}
    Access := KEY_READ;
{$EndIf D5}

  // Open HKCR key for current Extension
    RootKey := HKEY_CLASSES_ROOT;
    if not OpenKey(abfAddSlashBefore(FExtension), False) then Exit;

  // Read a description of default extension processor ("Word.Document.8")
    FExtDescription := ReadString('');
    if ExtDescription = '' then Exit;

  // Select between "inkey" and "as predefined file" extensions
    if not OpenKey(abfAddSlashBefore(ExtDescription), False) then
    begin
    // Load data for "predefined file"
      CloseKey;
    // Opens key as relative
      if not OpenKey(ExtDescription, False) then Exit;
    end;

  // Save key and set Result
    AKey := abfAddSlash(abfAddSlashBefore(CurrentPath));
    Result := True;

  // Read a description of file type ("Word document").
    FFileDescription := ReadString('');

  // Read icons
    if OpenKey('DefaultIcon', False) then
    begin
      FIconFile := ReadString('');

      if IconFile <> '' then
      begin
        i := abfBackPos(',', IconFile);
        if i <> 0 then
        begin
        // Check for 2 numbers after file name (.txt under WinXP)
          j := abfBackPosEx(',', IconFile, i - 1);
          if j > 0 then
          begin
{ TODO -oKARPOLAN : Make proper check for comma in the middle of file name }
            Delete(FIconFile, i, MaxInt);
            i := j;
          end;

        // Get IconIndex
          FIconIndex := StrToIntDef(Copy(IconFile, i + 1, MaxInt), -$FFFF);

        // If icon index is valide, remove it from IconFile string
          if IconIndex <> -$FFFF then
            Delete(FIconFile, i, MaxInt)
          else
            FIconIndex := 0;
        end;

      // Get icons
        i := ExtractIconEx(PChar(IconFile), IconIndex, LH, SH, 1);
        if i > 0 then
        begin
          DefaultIcon := False;
          LargeIcon.Handle := LH;
          SmallIcon.Handle := SH;
        end;
      end;
    end;

    CloseKey;

  // Read OpenCommand
    if OpenKey(AKey + 'shell\open', False) then
      FOpenCommandDescription := ReadString('');
    CloseKey;
    if OpenKey(AKey + 'shell\open\command', False) then
      FOpenCommand := ReadString('');
    CloseKey;

  // Read OpenNewCommand
    if OpenKey(AKey + 'shell\opennew', False) then
      FOpenNewCommandDescription := ReadString('');
    CloseKey;
    if OpenKey(AKey + 'shell\opennew\command', False) then
      FOpenNewCommand := ReadString('');
    CloseKey;

  // Read EditCommand
    if OpenKey(AKey + 'shell\edit', False) then
      FEditCommandDescription := ReadString('');
    CloseKey;
    if OpenKey(AKey + 'shell\edit\command', False) then
      FEditCommand := ReadString('');
    CloseKey;

  // Read PrintCommand
    if OpenKey(AKey + 'shell\print', False) then
      FPrintCommandDescription := ReadString('');
    CloseKey;
    if OpenKey(AKey + 'shell\print\command', False) then
      FPrintCommand := ReadString('');
    CloseKey;

  // Read PrintToCommand
    if OpenKey(AKey + 'shell\printto', False) then
      FPrintToCommandDescription := ReadString('');
    CloseKey;
    if OpenKey(AKey + 'shell\printto\command', False) then
      FPrintToCommand := ReadString('');
    CloseKey;

  finally
    if DefaultIcon then
    begin
      i := 0;
      if AnsiCompareText(Extension, SabfExt_Exe) = 0 then i := 2;
      abfGetShell32Icon(i, FLargeIcon, FSmallIcon);
    end;

    Free;
  end;
end;{function TabfFileAssociation.ReadAssociation}

//------------------------------------------------------------------------------

function TabfFileAssociation.ReadAssociation(const AExtension: string): Boolean;
begin
  Extension := AExtension;
  Result := ReadAssociation;
end;

//------------------------------------------------------------------------------

function TabfFileAssociation.WriteAssociation: Boolean;
var
  Reg: TRegistry;
  AKey: string;
begin
  Result := False;

  if [csLoading, csDestroying] * ComponentState <> [] then Exit;
  if ReadOnly then Exit;
  if (FExtension = '') or (FExtension = '.') then Exit;

  FixExtension;
  if ExtDescription = '' then
  begin
    FExtDescription := Extension + 'file';
    Delete(FExtDescription, 1, 1);
  end;

  Reg := TRegistry.Create;
  with Reg do
  try
{$IfDef D5}
    Access := KEY_ALL_ACCESS;
{$EndIf D5}

    RootKey := HKEY_CLASSES_ROOT;

    if not OpenKey(abfAddSlashBefore(Extension), True) then Exit;
    WriteString('', ExtDescription);

    CloseKey;

    AKey := abfAddSlash(abfAddSlashBefore(ExtDescription));

    if not OpenKey(AKey, True) then Exit;
    WriteString('', FileDescription);

    if IconFile <> '' then
    begin
      if not OpenKey('DefaultIcon', True) then Exit;
      WriteString('', IconFile + ',' + IntToStr(IconIndex));
    end;

    CloseKey;

    if OpenCommand <> '' then
    begin
      if not OpenKey(AKey + 'shell\open', True) then Exit;
      WriteString('', OpenCommandDescription);
      CloseKey;

      if not OpenKey(AKey + 'shell\open\command', True) then Exit;
      WriteString('', OpenCommand);
      CloseKey;
    end;

    if OpenNewCommand <> '' then
    begin
      if not OpenKey(AKey + 'shell\opennew', True) then Exit;
      WriteString('', OpenNewCommandDescription);
      CloseKey;

      if not OpenKey(AKey + 'shell\opennew\command', True) then Exit;
      WriteString('', OpenNewCommand);
      CloseKey;
    end;

    if EditCommand <> '' then
    begin
      if not OpenKey(AKey + 'shell\edit', True) then Exit;
      WriteString('', EditCommandDescription);
      CloseKey;

      if not OpenKey(AKey + 'shell\edit\command', True) then Exit;
      WriteString('', EditCommand);
      CloseKey;
    end;

    if PrintCommand <> '' then
    begin
      if not OpenKey(AKey + 'shell\print', True) then Exit;
      WriteString('', PrintCommandDescription);
      CloseKey;

      if not OpenKey(AKey + 'shell\print\command', True) then Exit;
      WriteString('', PrintCommand);
      CloseKey;
    end;

    if PrintToCommand <> '' then
    begin
      if not OpenKey(AKey + 'shell\printto', True) then Exit;
      WriteString('', PrintToCommandDescription);
      CloseKey;

      if not OpenKey(AKey + 'shell\printto\command', True) then Exit;
      WriteString('', PrintToCommand);
      CloseKey;
    end;

    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
    Result := True;
  finally
    Reg.Free;
  end;
end;{function TabfFileAssociation.WriteAssociation}

//------------------------------------------------------------------------------
{$HINTS OFF}

function TabfFileAssociation.DeleteAssociation: Boolean;
var
  Reg: TRegistry;
  TempKeyName: AnsiString;
begin
  Result := False;

  if [csLoading, csDestroying] * ComponentState <> [] then Exit;
  if ReadOnly then Exit;

  FixExtension;

  if (FExtension = '') or (FExtension = '.') then Exit;  

  Reg := TRegistry.Create;
  with Reg do
  try
{$IfDef D5}
    Access := KEY_ALL_ACCESS;
{$EndIf D5}
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKey(abfAddSlashBefore(Extension), False) then
    begin
      TempKeyName := Trim(ReadString(''));
      CloseKey;
    end else
    begin
      Result := True;
      Exit;
    end;

    if TempKeyName <> '' then
      DeleteKey(abfAddSlashBefore(TempKeyName));

    Result := DeleteKey(abfAddSlashBefore(Extension));

    if Result then
    begin
      RootKey := HKEY_CURRENT_USER;
      DeleteKey('\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\' +
        Extension);
    end;

    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  finally
    Free;
  end;

  ReadAssociation;
end;

{$HINTS ON}
//------------------------------------------------------------------------------

procedure TabfFileAssociation.UpdateAssociation;
begin
  if [csLoading, csDestroying] * ComponentState <> [] then Exit;
  if ReadOnly then Exit;

{  if (csDesigning in ComponentState) then
  begin
    if not ReadOnly then
    begin
      if not WriteAssociation then
        Application.MessageBox(PChar(SabfRegistry_CannotWrite),
          PChar(SabfError), MB_ICONERROR or MB_OK);
    end else
      Application.MessageBox(PChar(SabfProperty_ReadOnlyMode),
        PChar(SabfWarning), MB_ICONWARNING or MB_OK);
    Exit;
  end;{}

  if not (csDesigning in ComponentState) then
  try
    WriteAssociation;
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfFileAssociation.ChangingAllowed: Boolean;
begin
  Result := (not ReadOnly) or (csDesigning in ComponentState);
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.FixExtension;
begin
  if (FExtension <> '') and (FExtension[1] <> '.') then
    Insert('.', FExtension, 1);
end;


//==============================================================================
// Properties Get/Set

procedure TabfFileAssociation.SetExtension(const Value: string);
begin
  if Extension = Value then Exit;
  FExtension := Value;
  FixExtension;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetExtDescription(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if ExtDescription = Value then Exit;
  FExtDescription := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetFileDescription(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if FileDescription = Value then Exit;
  FFileDescription := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetIcon(const Value: TIcon);
begin
  // Do nothing, icons are readonly
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetIconFile(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if IconFile = Value then Exit;
  FIconFile := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetIconIndex(const Value: Integer);
begin
  if not ChangingAllowed then Exit;
  if IconIndex = Value then Exit;
  FIconIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetOpenCommand(const Value: TFileName);
begin
  if not ChangingAllowed then Exit;
  if OpenCommand = Value then Exit;
  FOpenCommand := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetOpenCommandDescription(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if OpenCommandDescription = Value then Exit;
  FOpenCommandDescription := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetOpenNewCommand(const Value: TFileName);
begin
  if not ChangingAllowed then Exit;
  if OpenNewCommand = Value then Exit;
  FOpenNewCommand := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetOpenNewCommandDescription(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if OpenNewCommandDescription = Value then Exit;
  FOpenNewCommandDescription := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetEditCommand(const Value: TFileName);
begin
  if not ChangingAllowed then Exit;
  if EditCommand = Value then Exit;
  FEditCommand := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetEditCommandDescription(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if EditCommandDescription = Value then Exit;
  FEditCommandDescription := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetPrintCommand(const Value: TFileName);
begin
  if not ChangingAllowed then Exit;
  if PrintCommand = Value then Exit;
  FPrintCommand := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetPrintCommandDescription(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if PrintCommandDescription = Value then Exit;
  FPrintCommandDescription := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetPrintToCommand(const Value: TFileName);
begin
  if not ChangingAllowed then Exit;
  if PrintToCommand = Value then Exit;
  FPrintToCommand := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFileAssociation.SetPrintToCommandDescription(const Value: string);
begin
  if not ChangingAllowed then Exit;
  if FPrintToCommandDescription = Value then Exit;
  FPrintToCommandDescription := Value;
end;


//==============================================================================
// TabfCustomFileStorage
//==============================================================================
// A prototype of components that can store a file data into the *.dfm resource
// of the form where the component is placed.
// Date:  11/08/2000
{ TabfCustomFileStorage }

constructor TabfCustomFileStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileDataStream := TMemoryStream.Create;
  FDataSize := 0;
end;

//------------------------------------------------------------------------------

destructor TabfCustomFileStorage.Destroy;
begin
  FFileDataStream.Free;
  FFileDataStream := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.Loaded;
begin
  inherited Loaded;
// Save file if AutoSave is True and run-time is now
  if AutoSave and not (csDesigning in ComponentState) then Save;
end;


//==============================================================================
// File operations

function TabfCustomFileStorage.LoadFrom(const AFileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  try
    Clear;
  // Check is the file exist }
    if not FileExists(AFileName) then Exit;
  // Load data from the specified file using streams
    try
      FS := TFileStream.Create(AFileName, fmOpenRead);
      try
        FDataSize := FS.Size;
      {$IfNDef D3}
        TMemoryStream(FFileDataStream).SetSize(DataSize);
      {$Else D3}
        FFileDataStream.Size := DataSize;
      {$EndIf D3}
        FS.Seek(0, soFromBeginning);
        FFileDataStream.Seek(0, soFromBeginning);
        FFileDataStream.CopyFrom(FS, 0);
      finally
        FS.Free;
      end;
      Result := True;
    except
    // Rase own exception
      raise EabfFileStorage.CreateFmt(SabfFileStorage_CannotLoad, [AFileName]);
    end;{try..except}
  finally
    if Assigned(OnLoad) then OnLoad(Self, Result);
  end;{try..finally}
end;{function TabfCustomFileStorage.LoadFrom}

//------------------------------------------------------------------------------

function TabfCustomFileStorage.SaveTo(const AFileName: string): Boolean;
var
  OverrideFile: Boolean;
  FS: TFileStream;
begin
  Result := False;
  OverrideFile := True;
  try
  // Check is the file exist or should be overrided
    if FileExists(AFileName) then
    begin
      if Assigned(OnConfirmOverride) then OnConfirmOverride(Self, OverrideFile);
      if not OverrideFile then Exit; // Do not override
    end;
  // Save data to the specified file using streams
    try
      FS := TFileStream.Create(AFileName, fmCreate);
      try
        FFileDataStream.Seek(0, soFromBeginning);
        FS.Seek(0, soFromBeginning);
        FS.CopyFrom(FFileDataStream, 0);
      finally
        FS.Free;
      end;
      Result := True;
    except
    // Rase own exception
      raise EabfFileStorage.CreateFmt(SabfFileStorage_CannotSave, [AFileName]);
    end;{try..except}
  finally
    if Assigned(OnSave) then OnSave(Self, Result);
  end;{try..finally}
end;{function TabfCustomFileStorage.SaveTo}

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.Load;
begin
  LoadFrom(FileName);
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.Save;
begin
  SaveTo(SaveName);
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.Clear;
begin
  FDataSize := 0;
{$IfNDef D3}
  TMemoryStream(FFileDataStream).SetSize(DataSize);
{$Else D3}
  FFileDataStream.Size := DataSize;
{$EndIf D3}
end;


//==============================================================================
// Streaming routines

procedure TabfCustomFileStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('FileData', ReadFileData, WriteFileData, True);
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.ReadFileData(Stream: TStream);
begin
  Stream.Read(FDataSize, SizeOf(FDataSize));
  if DataSize <= 0 then Exit;
  {$IfNDef D3}
    TMemoryStream(FFileDataStream).SetSize(DataSize);
  {$Else D3}
    FFileDataStream.Size := DataSize;
  {$EndIf D3}
  FFileDataStream.CopyFrom(Stream, DataSize);
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.WriteFileData(Stream: TStream);
begin
  Stream.Write(FDataSize, SizeOf(FDataSize));
  if DataSize <= 0 then Exit;
  FFileDataStream.Seek(0, soFromBeginning);
  Stream.CopyFrom(FFileDataStream, DataSize);
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomFileStorage.SetAutoSave(Value: Boolean);
begin
  if Value and (csDesigning in ComponentState) and not abfIsValidFileName(SaveName) then
  begin
    Application.MessageBox(PChar(SabfFileStorage_AutoSaveWhenNoSaveName),
      PChar(SabfWarning), MB_ICONWARNING or MB_OK);
    FAutoSave := False;
    Exit;
  end;
  FAutoSave := Value;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
// If the FileName is set in the designer then load file
  if (csDesigning in ComponentState) then Load;
end;

//------------------------------------------------------------------------------

function TabfCustomFileStorage.GetDataSize: LongWord;
begin
  Result := FDataSize;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.SetDataSize(Value: LongWord);
begin
  // Do nothing, this class doesn't allow change data size.
end;

//------------------------------------------------------------------------------

procedure TabfCustomFileStorage.SetSaveName(const Value: TFileName);
begin
  if FSaveName = Value then Exit;
  FSaveName := Value;
// Reset AutoSave if wrong SaveName
  if (csDesigning in ComponentState) and (not abfIsValidFileName(SaveName)) then
  begin
//    MessageBeep(0);
    AutoSave := False;
  end
end;


//==============================================================================
// TabfFileStorage
//==============================================================================
// A Components that can store a file into a *.dfm resource of the form where
// the component is placed.
{ TabfFileStorage }

function TabfFileStorage.LoadFrom(const AFileName: string): Boolean;
begin
  Result := inherited LoadFrom(AFileName);
end;

//------------------------------------------------------------------------------

function TabfFileStorage.SaveTo(const AFileName: string): Boolean;
begin
  Result := inherited SaveTo(AFileName);
end;

//------------------------------------------------------------------------------

procedure TabfFileStorage.Load;
begin
  inherited Load;
end;

//------------------------------------------------------------------------------

procedure TabfFileStorage.Save;
begin
  inherited Save;
end;


//==============================================================================
// TabfStartButtonProperties
//==============================================================================
// Component that allows to get or set some parameters of the Window's Start
// Button
// Date: 04/01/2000
{ TabfStartButtonProperties }

constructor TabfStartButtonProperties.Create(AOwner: TComponent);
const
  SButton  = 'Button';
  STaskBar = 'Shell_TrayWnd';
begin
  inherited Create(AOwner);
  FHandle := FindWindowEx(FindWindow(STaskBar, nil), 0, SButton, nil);
  UpdateBounds;
end;

//------------------------------------------------------------------------------

destructor TabfStartButtonProperties.Destroy;
begin
  FHandle := 0;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.UpdateBounds;
var
  WindowPlacement: TWindowPlacement;
begin
  WindowPlacement.Length := SizeOf(WindowPlacement);
  GetWindowPlacement(Handle, @WindowPlacement);
  with WindowPlacement.rcNormalPosition do
  begin
    FLeft   := Left;
    FTop    := Top;
    FWidth  := Right;
    FHeight := Bottom;
  end
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  if (ALeft = FLeft) and (ATop = FTop) and (AWidth = FWidth) and
    (AHeight = FHeight) then Exit;
  SetWindowPos(FHandle, 0, ALeft, ATop, AWidth, AHeight,
    SWP_NOZORDER or SWP_NOACTIVATE)
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetBoundsRect: TRect;
begin
  Result.Left   := FLeft;
  Result.Top    := FTop;
  Result.Right  := FLeft + FWidth;
  Result.Bottom := FTop + FHeight;
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetBoundsRect(const A: TRect);
begin
  with A do SetBounds(Left, Top, Right - Left, Bottom - Top);
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetLeft: Integer;
begin
  UpdateBounds;
  Result := FLeft;
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetLeft(A: Integer);
begin
  SetBounds(A, FTop, FWidth, FHeight);
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetTop: Integer;
begin
  UpdateBounds;
  Result := FTop;
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetTop(A: Integer);
begin
  SetBounds(FLeft, A, FWidth, FHeight);
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetWidth: Integer;
begin
  UpdateBounds;
  Result := FWidth;
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetWidth(A: Integer);
begin
  SetBounds(FLeft, FTop, A, FHeight);
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetHeight: Integer;
begin
  UpdateBounds;
  Result := FHeight;
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetHeight(A: Integer);
begin
  SetBounds(FLeft, FTop, FWidth, A);
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetCaption: string;
var
  Buffer: array[0..255] of Char;
begin
  GetWindowText(Handle, Buffer, SizeOf(Buffer));
  Result := Buffer;
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetCaption(const A: string);
begin
  SetWindowText(Handle, PChar(A));
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetEnabled: Boolean;
begin
  Result := IsWindowEnabled(Handle);
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetEnabled(A: Boolean);
begin
  EnableWindow(Handle, A);
end;

//------------------------------------------------------------------------------

function TabfStartButtonProperties.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

//------------------------------------------------------------------------------

procedure TabfStartButtonProperties.SetVisible(A: Boolean);
var
  Flag: Integer;
begin
  if A then Flag := SW_SHOW else Flag := SW_HIDE;
  ShowWindow(Handle, Flag);
end;


//==============================================================================
// TabfColorPicker
//==============================================================================
// Allows to get a color value of the pixel under cursor.
// Date: 04/01/2000
{ TabfColorPicker }

constructor TabfColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Interval := 100;
  FCursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TabfColorPicker.Timer;
begin
  inherited Timer;
  UpdateCursor;
end;

//------------------------------------------------------------------------------

procedure TabfColorPicker.UpdateCursor;
begin
  if FCursor = crDefault then Exit;
  SetCursor(Screen.Cursors[FCursor]);
end;

//------------------------------------------------------------------------------

function TabfColorPicker.GetColor: TColor;
begin
  Result := abfGetColorUnderCursor;
end;

//------------------------------------------------------------------------------
procedure TabfColorPicker.SetCursor(A: TCursor);
begin
  if FCursor = A then Exit;
  FCursor := A;
  UpdateCursor;
end;


//==============================================================================
// TabfTrayIcon
//==============================================================================
// Component for working with the system tray. Allows to add or remove the icon,
// provides icon messages support. Has an engine to create the icon animated.
// Date: 04/18/2001
{ TabfTrayIcon }

constructor TabfTrayIcon.Create(AOwner : TComponent);
begin
  FNotifyIconData := nil;
  FOldAppProc := nil;
  FNewAppProc := nil;
  FIcon := TIcon.Create;
  FImageIndex := 0;
  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := ImageListChange;
  FIconID := 0;
  FIconFlags := NIF_TIP or NIF_MESSAGE;
  FEnabled := True;
  FShowHint := True;
  FVisible := False;
  FWasHidden := False;
  FVisibleOnDesigning := False;
  FInfoNoSound := False;
  FInfoTimeout := 10000;
  FInfoType := tiiNone;

  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then OnTimer := DummyOnTimer;
  CycleIcons := False;
  CycleInterval := 200;

  HookApp;
end;

//------------------------------------------------------------------------------

destructor TabfTrayIcon.Destroy;
begin
  Visible := False;
  UnhookApp;
  inherited Destroy;
  FreeMem(FNotifyIconData);
  FNotifyIconData := nil;
  FreeAndNil(FImageListChangeLink);
  FreeAndNil(FIcon);
end;

//------------------------------------------------------------------------------
// Actions

procedure TabfTrayIcon.Update;
begin
  if not PrepareNotifyIconData then Exit;
  CallShellNotifyIcon(NIM_MODIFY);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.Show;
begin
  FIconFlags := FIconFlags or NIF_ICON;
  if not InsertToTray then Exit;
  if not (csDesigning in ComponentState) then FVisible := True
  else FVisibleOnDesigning := True;
  if Assigned(FOnShow) then FOnShow(Self);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.Hide;
begin
  FIconFlags := FIconFlags and not NIF_ICON;
  if not DeleteFromTray then Exit;
  if not (csDesigning in ComponentState) then FVisible := False
  else FVisibleOnDesigning := False;
  if Assigned(FOnHide) then FOnHide(Self);
// If owner form is minimized or hiden - restore it
  if MinimizeToTray then ShowMainForm;
  if Assigned(Owner) and (Owner is TWinControl) and
    (not TWinControl(Owner).Visible) then TWinControl(Owner).Visible := True;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.ShowPopup;
var
  APoint: TPoint;
begin
  if (not Assigned(PopupMenu)) or (not PopupMenu.AutoPopup) then Exit;
  GetCursorPos(APoint);

// Fix for showing popup whan it is already shown
  Application.ProcessMessages;
  if Assigned(Owner) and (Owner is TWinControl) then
    SetForegroundWindow(TWinControl(Owner).Handle);

// Show popup menu
  PopupMenu.PopupComponent := Self;
  PopupMenu.Popup(APoint.X, APoint.Y);

// Post some message to currently foreground window
//  PostMessage(FHandle, WM_NULL, 0, 0);
  if Assigned(Owner) and (Owner is TWinControl) then
    PostMessage(TWinControl(Owner).Handle, WM_NULL, 0, 0);{}

  if Assigned(FOnPopupMenu) then FOnPopupMenu(Self);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.ShowMainForm;
begin
  if FWasHidden then
  begin
    abfMainFormShow;
    FWasHidden := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.HideMainForm;
begin
  FWasHidden := FWasHidden or (Assigned(Application) and
    ((IsWindow(Application.Handle) and (not IsWindowVisible(Application.Handle))) or
    (Assigned(Application.MainForm) and IsWindow(Application.MainForm.Handle) and
    (not IsWindowVisible(Application.MainForm.Handle)))));

  abfMainFormHide;
end;

//------------------------------------------------------------------------------
// Extended info

function TabfTrayIcon.ShowCustomInfo(AType: TabfTrayIconInfoType;
  const ATitle, AText: WideString): Boolean;
begin
  Result := False;
  if not PrepareNotifyIconData then Exit;
  if abfGetShellVersion < $00050000 then Exit;

// Fill extended info members
  if IsWinNT then
    with PabfNotifyIconDataWVer5(FNotifyIconData)^ do
    begin
      dwInfoFlags := DWORD(AType);
      if FInfoNoSound then
        dwInfoFlags := dwInfoFlags or NIIF_NOSOUND;
      uFlags      := NIF_INFO;
      uTimeout    := InfoTimeOut;
      Move(PWideChar(AText)^, szInfo, Min((SizeOf(szInfo) - 1),
        Length(AText) * 2));
      Move(PWideChar(ATitle)^, szInfoTitle, Min((SizeOf(szInfoTitle) - 1),
        Length(ATitle) * 2));
    end
  else
    with PabfNotifyIconDataAVer5(FNotifyIconData)^ do
    begin
      dwInfoFlags := DWORD(AType);
      if FInfoNoSound then
        dwInfoFlags := dwInfoFlags or NIIF_NOSOUND;
      uFlags      := NIF_INFO;
      uTimeout    := InfoTimeout;
      Move(PChar(string(AText))^, szInfo, Min((SizeOf(szInfo) - 1),
        Length(AText)));
      Move(PChar(string(ATitle))^, szInfoTitle, Min((SizeOf(szInfoTitle) - 1),
        Length(ATitle)));
    end;

// Show a balloon
  Result := CallShellNotifyIcon(NIM_MODIFY);
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.ShowInfo: Boolean;
begin
  Result := ShowCustomInfo(FInfoType, FInfoTitle, FInfoText);
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.ShowBalloon: Boolean;
begin
  Result := ShowInfo;
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.CallShellNotifyIcon(AMessage: DWORD): Boolean;
begin
  Result := False;
  if not Assigned(FNotifyIconData) then Exit;

  if IsWinNT then
    Result := Shell_NotifyIconW(AMessage, FNotifyIconData)
  else
    Result := Shell_NotifyIcon(AMessage, FNotifyIconData);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = PopupMenu) then PopupMenu := nil else
    if (AComponent = ImageList) then ImageList := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.Loaded;
begin
  inherited Loaded;
  if (MinimizeOnStart) and not (csDesigning in ComponentState) then
  begin
    Application.ShowMainForm := False;
    DoMinimize;
  end;
  if (Visible and not (csDesigning in ComponentState)) or
    (VisibleOnDesigning and (csDesigning in ComponentState)) then Show;
end;
//------------------------------------------------------------------------------

procedure TabfTrayIcon.Timer;
begin
  if Assigned(ImageList) then
  begin
    if (ImageIndex < ImageList.Count - 1) then ImageIndex := ImageIndex + 1
    else ImageIndex := 0;
  end;
  if Assigned(FOnCycleIcons) then FOnCycleIcons(Self);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.WndProc(var Message: TMessage);
var
  APoint: TPoint;
begin
  with Message do
    if Msg <> WM_TRAYICONNOTIFY then
    begin
      case Msg of
        WM_QUERYENDSESSION: Result := 1; // For right ShutDown
      else
      // Show icon again if tray window is recreated (case cannot be used)
        if Msg = WM_TaskbarRestart then
          if Visible then
          begin
            Hide;
            Show;
          end;
        inherited WndProc(Message);
      end;
      Exit;
    end;{if Msg <> WM_TRAYICONNOTIFY then}

  if (csDesigning in ComponentState) then Exit;
  if not Enabled then Exit;

// pocess WM_TRAYICONNOTIFY message
  GetCursorPos(APoint);
  with Message do
    case LParam of
      NIN_BALLOONSHOW: begin // Balloon appear
        if Assigned(FOnBalloonShow) then FOnBalloonShow(Self);
      end;
      NIN_BALLOONTIMEOUT: begin // Balloon time is over
        if Assigned(FOnBalloonTimeout) then FOnBalloonTimeout(Self);
      end;
      NIN_BALLOONUSERCLICK: begin // Click on balloon
        if Assigned(FOnBalloonClick) then FOnBalloonClick(Self);
      end;

      WM_MOUSEMOVE: begin              // Mouse move
        GetCursorPos(APoint);
        MouseMove(abfGetShiftState, APoint.X, APoint.Y);
      end;
      WM_LBUTTONDOWN: begin            // Left button down
        MouseDown(mbLeft, abfGetShiftState + [ssLeft], APoint.X, APoint.Y);
        FPressed := True;
      end;
      WM_LBUTTONUP: begin              // Left button up
        MouseUp(mbLeft, abfGetShiftState  + [ssLeft], APoint.X, APoint.Y);
        if FPressed then Click;
        if FPopupByLeft then ShowPopup;
        FPressed := False
      end;
      WM_LBUTTONDBLCLK: begin          // Left button double click
        DblClick;
      end;
      WM_RBUTTONDOWN: begin            // Right button down
        MouseDown(mbRight, abfGetShiftState + [ssRight], APoint.X, APoint.Y);
      end;
      WM_RBUTTONUP: begin              // Right button up
        MouseUp(mbRight, abfGetShiftState + [ssRight], APoint.X, APoint.Y);
        ShowPopup;
      end;
      WM_MBUTTONDOWN: begin            // Middle button down
        MouseDown(mbMiddle, abfGetShiftState + [ssMiddle], APoint.X, APoint.Y);
      end;
      WM_MBUTTONUP: begin              // Middle button up
        MouseUp(mbMiddle, abfGetShiftState + [ssMiddle], APoint.X, APoint.Y);
      end;
   else
     inherited WndProc(Message);
   end;{case LParam of}
end;{procedure TabfTrayIcon.WndProc}


//==============================================================================
// Taskbar Routines

function TabfTrayIcon.PrepareNotifyIconData: Boolean;
var
  TempShellVer: Integer;

  //-------------------------------------

  procedure _AllocNID;
  var
    TempSize: Integer;
  begin
    if Assigned(FNotifyIconData) then
      FreeMem(FNotifyIconData);
    TempSize := 0;

    if TempShellVer < $00050000 then
    begin
      if IsWinNT then
        TempSize := SizeOf(TabfNotifyIconDataWVer4)
      else
        TempSize := SizeOf(TabfNotifyIconDataAVer4);
    end else
    if (TempShellVer >= $00050000) and (TempShellVer < $00060000) then
    begin
      if IsWinNT then
        TempSize := SizeOf(TabfNotifyIconDataWVer5)
      else
        TempSize := SizeOf(TabfNotifyIconDataAVer5);
    end else
    if (TempShellVer >= $00060000) then
    begin
      if IsWinNT then
        TempSize := SizeOf(TabfNotifyIconDataWVer6)
      else
        TempSize := SizeOf(TabfNotifyIconDataAVer6);
    end;

    if TempSize < 4 then Exit;

    GetMem(FNotifyIconData, TempSize);
    FillChar(FNotifyIconData^, TempSize, 0);
    PDWORD(FNotifyIconData)^ := TempSize;
  end;

  //-------------------------------------

  procedure _PrepareNID4;
  begin
    if IsWinNT then
    begin
      with PabfNotifyIconDataWVer4(FNotifyIconData)^ do
      begin
        hWnd             := Handle;
        uID              := IconID;
        uFlags           := FIconFlags;
        uCallbackMessage := WM_TRAYICONNOTIFY;
        hIcon            := FIcon.Handle;
        if ShowHint then
          Move(PWideChar(FHint)^, szTip, Min((SizeOf(szTip) - 1),
          Length(FHint) * 2));
      end
    end else
    begin
      with PabfNotifyIconDataAVer4(FNotifyIconData)^ do
      begin
        hWnd             := Handle;
        uID              := IconID;
        uFlags           := FIconFlags;
        uCallbackMessage := WM_TRAYICONNOTIFY;
        hIcon            := FIcon.Handle;
        if ShowHint then
          Move(PChar(string(FHint))^, szTip, Min((SizeOf(szTip) - 1),
          Length(FHint)));
      end
    end;
  end;

  //-------------------------------------

  procedure _PrepareNID5;
  begin
    if TempShellVer < $00050000 then Exit;

    if IsWinNT then
    begin
      with PabfNotifyIconDataWVer5(FNotifyIconData)^ do
      begin
        if ShowHint then
          Move(PWideChar(FHint)^, szTip, Min((SizeOf(szTip) - 1),
          Length(FHint) * 2));
      end
    end else
    begin
      with PabfNotifyIconDataAVer5(FNotifyIconData)^ do
      begin
        if ShowHint then
          Move(PChar(string(FHint))^, szTip, Min((SizeOf(szTip) - 1),
          Length(FHint)));
      end
    end;
  end;

  //-------------------------------------

  procedure _PrepareNID6;
  begin
    if TempShellVer < $00060000 then Exit;
  end;

  //-------------------------------------

begin
  Result := False;
  TempShellVer := abfGetShellVersion;

  if TempShellVer < $00040000 then Exit;
  Result := True;

  _AllocNID;

  _PrepareNID4;
  _PrepareNID5;
  _PrepareNID6;
end;{function TabfTrayIcon.PrepareNotifyIconData}


//==============================================================================
// Hook routines

procedure TabfTrayIcon.HookApp;
begin
  if (csDesigning in ComponentState) then Exit;
  FOldAppProc := Pointer(GetWindowLong(Application.Handle, GWL_WNDPROC));
  FNewAppProc := MakeObjectInstance(HookAppProc);
  SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(FNewAppProc));
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.UnhookApp;
begin
  if Assigned(FOldAppProc) then
    SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(FOldAppProc));
  if Assigned(FNewAppProc) then FreeObjectInstance(FNewAppProc);
  FNewAppProc := nil;
  FOldAppProc := nil;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.HookAppProc(var Message: TMessage);
begin
  with Message do
  begin
    if not (csDesigning in ComponentState) then;
      case Msg of
        WM_SIZE: begin
          if (WParam = SIZE_MINIMIZED) and MinimizeToTray then DoMinimize;
        end;
      end;{case Msg of}
  // Process messages to Old procedure
    Result := CallWindowProc(OldAppProc, Application.Handle, Msg, WParam, LParam);
  end;
end;


//==============================================================================
// Events handlers

procedure TabfTrayIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y : Integer);
begin
  if Assigned(Owner) and (Owner is TWinControl) then
    SetForegroundWindow(TWinControl(Owner).Handle);
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.Click;
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.DblClick;
var
  i: Integer;
begin
  if Assigned(PopupMenu) and PopupMenu.AutoPopup and (not PopupByLeft) then
    for i := 0 to PopupMenu.Items.Count - 1 do
      if PopupMenu.Items[i].Default then
      begin
        PopupMenu.Items[i].Click;
        if Assigned(FOnDblClick) then FOnDblClick(Self);
      // Last mouse up is lost, so set foreground window here
        if Assigned(Owner) and (Owner is TWinControl) then
          SetForegroundWindow(TWinControl(Owner).Handle);
        Exit;
      end;
  if (csDesigning in ComponentState) then Exit;

// If there is no Default menu item and MinimizeToTray, restore or minimize
// the application
  if MinimizeToTray and Assigned(Owner) and (Owner is TWinControl) then
    if TWinControl(Owner).Visible then HideMainForm else ShowMainForm;

// Process custom event
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.DoMinimize;
begin
  if (csDesigning in ComponentState) then Exit;
  HideMainForm;
  Visible := True;
  if Assigned(FOnMinimize) then FOnMinimize(Self);
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.InsertToTray: Boolean;
begin
  Result := False;
  try
    if not PrepareNotifyIconData then Exit;
    Result := CallShellNotifyIcon(NIM_ADD);
  except
    raise EabfTrayIcon.CreateFmt(SabfTrayIcon_ErrInsertTo, [Name]);
  end;
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.DeleteFromTray: Boolean;
begin
  Result := False;
  try
    if not PrepareNotifyIconData then Exit;
    Result := CallShellNotifyIcon(NIM_DELETE);
  except
    raise EabfTrayIcon.CreateFmt(SabfTrayIcon_ErrDeleteFrom, [Name]);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetIconByImageIndex;
begin
  if not Assigned(FImageList) then Exit;
  FIcon.Assign(nil);
  FImageList.GetIcon(FImageIndex, FIcon);
  Update;
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.IsIconStored: Boolean;
begin
  Result := (ImageList = nil);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.DummyOnTimer(ASender: TObject);
begin
  // Dummy OnTimer event for rigth Timer work
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.ImageListChange(ASender: TObject);
begin
  SetIconByImageIndex;
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.GetCycleIcons: Boolean;
begin
  Result := inherited Enabled;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetCycleIcons(AValue: Boolean);
begin
  inherited Enabled := AValue;
end;

//------------------------------------------------------------------------------

function TabfTrayIcon.GetCycleInterval: Cardinal;
begin
  Result := inherited Interval;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetCycleInterval(AValue: Cardinal);
begin
  inherited Interval := AValue;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetHint(const AValue: WideString);
begin
  if FHint = AValue then Exit;
  FHint := AValue;
  Update;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetIcon(const AValue: TIcon);
begin
  FIcon.Assign(AValue);
  Update;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetIconID(AValue: Integer);
var
  RegBool: Boolean;
begin
  if FIconID = AValue then Exit;
  RegBool := Visible;
  Visible := False;
  FIconID := AValue;
  Visible := RegBool;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetImageList(const AValue: TImageList);
begin
  if ImageList = AValue then Exit;
  if Assigned(ImageList) then ImageList.UnRegisterChanges(FImageListChangeLink);
  FImageList := AValue;
  if Assigned(ImageList) then
  begin
    ImageList.FreeNotification(Self);
    ImageList.RegisterChanges(FImageListChangeLink);
  end;
  SetIconByImageIndex;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetImageIndex(AValue: TImageIndex);
begin
  FImageIndex := AValue;
  SetIconByImageIndex;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetShowHint(AValue: Boolean);
begin
  if FShowHint = AValue then Exit;
  FShowHint := AValue;
  Update;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetPopupMenu(const AValue: TPopupMenu);
begin
  if PopupMenu = AValue then Exit;;
  FPopupMenu := AValue;
  if Assigned(PopupMenu) then PopupMenu.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  if (csDesigning in ComponentState) then Exit;
  if FVisible then Show else Hide;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetVisibleOnDesigning(AValue: Boolean);
begin
  if FVisibleOnDesigning = AValue then Exit;
  FVisibleOnDesigning := AValue;
  if not (csDesigning in ComponentState) then Exit;
  if FVisibleOnDesigning then Show else Hide;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetInfoNoSound(AValue: Boolean);
begin
  FInfoNoSound := AValue;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetInfoText(const AValue: WideString);
begin
  FInfoText := AValue;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetInfoTitle(const AValue: WideString);
begin
  FInfoTitle := AValue;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetInfoTimeOut(AValue: Integer);
begin
  FInfoTimeOut := AValue;
end;

//------------------------------------------------------------------------------

procedure TabfTrayIcon.SetInfoType(AValue: TabfTrayIconInfoType);
begin
  FInfoType := AValue;
end;


//==============================================================================
// TabfOneInstance
//==============================================================================
// Component allows easily create applications that have only one running copy
// at the same time.  Simple drop TabfOneInstance onto any form of application
// or create it at run-time and your can prevent the second application start.
// Applications are separated by Application.Title or Application.ExeName
// Date: 03/10/2003
{ TabfOneInstance }

var
  _OneInstanceMutex: THandle;

//------------------------------------------------------------------------------

constructor TabfOneInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivatePrevInstance := True;
  FPerformChecking := pcGlobal;
  FShowMessage := False;
  FExeNameAsIdentifier := False;
  FTerminateApplication := False;
end;

//------------------------------------------------------------------------------
// Returns True if previous instance of program is already run

function TabfOneInstance.AlreadyRun: Boolean;
const
  SSubClass = '.OneInstance.'; // do not localize
var
  Mutex: THandle;
  Flag: DWORD;
  WS: WideString;
begin
  Result := False;
  if csDesigning in ComponentState then Exit;

  Result := True;

  if PerformChecking = pcForCurrentUserOnly then
    WS := abfGetUserNameW + '.'
  else
    WS := '';

// Generate Mutex name
  if ExeNameAsIdentifier then
    WS := WS + WideUpperCase(abfGetShortPathNameW(abfParamStrW(0)))
  else
    WS := WS + Application.Title;
    
  WS := WS + Identifier + SSubClass;

// Back slashes are disallowed
  abfReplaceCharsW(WS, '\', '/');

  if IsWinNT then
    Mutex := CreateMutexW(nil, True, PWideChar(WS + 'CriticalSection'))  // don't localize
  else
    Mutex := CreateMutex(nil, True, PAnsiChar(AnsiString(WS) + 'CriticalSection')); // don't localize

  if (GetLastError <> 0) or (Mutex = 0) then Exit;

  if IsWinNT then
    _OneInstanceMutex := CreateMutexW(nil, False, PWideChar(WS + 'Default'))  // don't localize
  else
    _OneInstanceMutex := CreateMutex(nil, False, PAnsiChar(AnsiString(WS) + 'Default')); // don't localize

  Flag := WaitForSingleObject(_OneInstanceMutex, 0);
  Result := (Flag = WAIT_TIMEOUT);

  ReleaseMutex(Mutex);
  CloseHandle(Mutex);
end;

//------------------------------------------------------------------------------
// Verifies the previous instance, activate it is exists. Returns False if there
// is no prev instance. Returns True if prev instance exists and the current
// application should be closed.

function TabfOneInstance.Check: Boolean;
var
  Save: Boolean;
begin
  Result := False;
  if not AlreadyRun then Exit;

  FTerminateApplication := True;
  Save := Application.ShowMainForm;
  Application.ShowMainForm := False;

  if ShowMessage then DoShowMessage;
  if ActivatePrevInstance then DoActivatePrevInstance;

  if FTerminateApplication then Application.Terminate
  else Application.ShowMainForm := Save;

  Result := True;
end;

//------------------------------------------------------------------------------

procedure TabfOneInstance.Loaded;
begin
  inherited Loaded;
  Check;
end;

//------------------------------------------------------------------------------

procedure TabfOneInstance.DoActivatePrevInstance;
begin
{  if ExeNameAsIdentifier then
    ActivatePrevInstance(ExeName)
  else{}
    abfActivatePrevInstance;
end;

//------------------------------------------------------------------------------

procedure TabfOneInstance.DoShowMessage;
var
  WS: WideString;
begin
// Process custom message procedure
  if Assigned(FOnShowMessage) then
  begin
    FOnShowMessage(Self, FTerminateApplication);
    Exit;
  end;

// Prepare message string
  if Message <> '' then
    WS := Message
  else
{$IFDEF D6}
    WS := WideFormat(SabfOneInstance_AppAlreadyRun, [Application.Title]);
{$ELSE}
    WS := Format(SabfOneInstance_AppAlreadyRun, [Application.Title]);
{$ENDIF D6}

// Show message
  if IsWinNT then
    MessageBoxExW(Application.Handle, PWideChar(WS),
      PWideChar(WideString(Application.Title)), MB_ICONWARNING or MB_OK,
      LANG_NEUTRAL)
  else
    MessageBoxEx(Application.Handle, PChar(string(WS)),
      PChar(Application.Title), MB_ICONWARNING or MB_OK, LANG_NEUTRAL);
end;


//==============================================================================
// TabfAutoRun
//==============================================================================
// Component allows create applications that automatically run at the system
// start-up. You can register and unregister autorun execution for any
// application using this component.
// Date: 06/15/2000
{ TabfAutoRun }

constructor TabfAutoRun.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoRun := False;
  FKind := aekMachineRun;
  if not (csDesigning in ComponentState) then
    FFileName := Application.ExeName;
end;

//------------------------------------------------------------------------------

procedure TabfAutoRun.Register;
begin
  if (csDesigning in ComponentState) then Exit;
  UnRegister;
  if not AutoRun then Exit;
  abfRegisterAutoExec(FKind, FFileName);
end;

//------------------------------------------------------------------------------

procedure TabfAutoRun.UnRegister;
begin
  if (csDesigning in ComponentState) then Exit;
  abfUnregisterAutoExec(FKind, FFileName)
end;

//------------------------------------------------------------------------------

procedure TabfAutoRun.Loaded;
begin
  inherited Loaded;
//  if not (csDesigning in ComponentState) and (FFileName = '') then
//    FFileName := Application.ExeName;
  Register;
end;

//------------------------------------------------------------------------------

procedure TabfAutoRun.SetAutoRun(A: Boolean);
begin
  if FAutoRun = A then Exit;
  UnRegister;
  FAutoRun := A;
  Register;
end;

//------------------------------------------------------------------------------

procedure TabfAutoRun.SetFileName(const A: TFileName);
begin
  if FFileName = A then Exit;
  UnRegister;
  FFileName := A;
  Register;
end;

//------------------------------------------------------------------------------

procedure TabfAutoRun.SetKind(A: TabfAutoExecKind);
begin
  if FKind = A then Exit;
  UnRegister;
  FKind := A;
  Register;
end;


//==============================================================================
//  TabfShutdown
//==============================================================================
// Use TabfShutdown to precess PowerOff, Shutdown, Reboot, Logoff, Suspend or
// Hibernate action. The Force property determines forced suspension. If True,
// the Execute function sends a PBT_APMSUSPEND message to each application and
// driver, then immediately suspends operation. If False, sends a
// PBT_APMQUERYSUSPEND message to each application to request permission to
// suspend operation.
// Date: 07/30/2000
{ TabfShutdown }

constructor TabfShutdown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActionType := aatPowerOff;
  FForce := False;
end;

//------------------------------------------------------------------------------

procedure TabfShutdown.DoQueryShutdown(var CanShutdown: Boolean);
begin
  if Assigned(FOnQueryShutdown) then FOnQueryShutdown(Self, CanShutdown);
end;

//------------------------------------------------------------------------------

function TabfShutdown.Execute: Boolean;
var
  hToken, hProcess: THandle;
  tp, prev_tp: TTokenPrivileges;
  Len, Flags: DWORD;
  CanShutdown: Boolean;
begin
  Result := False;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, True, GetCurrentProcessID);
    try
      if not OpenProcessToken(hProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
        {$IfNDef D3}@{$EndIf}hToken) then Exit;
    finally
      CloseHandle(hProcess);
    end;
    try
      if not LookupPrivilegeValue('', 'SeShutdownPrivilege',
        tp.Privileges[0].Luid) then Exit;
      tp.PrivilegeCount := 1;
      tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      if not AdjustTokenPrivileges(hToken, False, tp, SizeOf(prev_tp),
        prev_tp, Len) then Exit;
    finally
      CloseHandle(hToken);
    end;
  end;
  CanShutdown := True;
  DoQueryShutdown(CanShutdown);
  if not CanShutdown then Exit;
  if FForce then Flags := EWX_FORCE else Flags := 0;
  case FActionType of
    aatPowerOff: Result := ExitWindowsEx(Flags or EWX_POWEROFF, 0);
    aatShutdown: Result := ExitWindowsEx(Flags or EWX_SHUTDOWN, 0);
    aatReboot: Result := ExitWindowsEx(Flags or EWX_REBOOT, 0);
    aatLogoff: Result := ExitWindowsEx(Flags or EWX_LOGOFF, 0);
    aatSuspend: Result := SetSystemPowerState(True, FForce);
    aatHibernate: Result := SetSystemPowerState(False, FForce);
  end;
end;{function TabfShutdown.Execute}


//==============================================================================
// TabfWav
//==============================================================================
// Component for playing of standard Wave (*.WAV) files (or resources). Can play
// sounds form file on a disk or from resource. Has ability Load wave in the
// *.dfm file.
{ TabfWav }

constructor TabfWav.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncPlay      := True;
  FLooped         := False;
  FEventAlias     := '.Default';
  FFileName       := inherited FileName;
  FNoDefaultSound := True;
  FNoStopSound    := False;
  FNoWaitIfBusy   := True;
  FPlayFrom       := Low(TabfWavPlayFrom);
  FResourceName   := 'WAVE1';
  FPlaying := False;
end;

//------------------------------------------------------------------------------

destructor TabfWav.Destroy;
begin
  Stop;
  inherited Destroy;
end;


//==============================================================================
// Wave routines

procedure TabfWav.UpdateSoundOptions;
begin
  FSoundModule  := 0;
  FSoundOptions := 0;
  if AsyncPlay      then FSoundOptions := FSoundOptions or SND_ASYNC;
  if Looped         then FSoundOptions := FSoundOptions or SND_LOOP;
  if NoDefaultSound then FSoundOptions := FSoundOptions or SND_NODEFAULT;
  if NoStopSound    then FSoundOptions := FSoundOptions or SND_NOSTOP;
  if NoWaitIfBusy   then FSoundOptions := FSoundOptions or SND_NOWAIT;
  case PlayFrom of
    pfFile: begin
      FSoundOptions := FSoundOptions or SND_FILENAME;
//      FSoundPointer := Pointer(FileName);
      FSoundPointer := PChar(FileName);
    end;
    pfResource: begin
      FSoundModule  := hInstance;
      FSoundOptions := FSoundOptions or SND_RESOURCE;
//      FSoundPointer := Pointer(ResourceName);
      FSoundPointer := PChar(ResourceName);
    end;
    pfDFM: begin
      FSoundOptions := FSoundOptions or SND_MEMORY;
      FSoundPointer := TMemoryStream(FFileDataStream).Memory;
    end;
    pfSystemEvent: begin
      FSoundOptions := FSoundOptions or SND_ALIAS{ or SND_APPLICATION};
//      FSoundPointer := Pointer(EventAlias);
      FSoundPointer := PChar(EventAlias);
    end;
  end;{case PlayFrom of}
end;

//------------------------------------------------------------------------------

procedure TabfWav.Play;
begin
  UpdateSoundOptions;
  FPlaying := True;
  try
    if Assigned(OnPlayBefore) then OnPlayBefore(Self);
    PlaySound(FSoundPointer, FSoundModule, FSoundOptions);
    if Assigned(OnPlayAfter) then OnPlayAfter(Self);
  finally
    FPlaying := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWav.Stop;
begin
  FPlaying := False;
  PlaySound(nil, 0, 0);
end;


//==============================================================================
// Properties Get/Set

procedure TabfWav.SetAsyncPlay(Value: Boolean);
begin
  if FAsyncPlay = Value then Exit;
  FAsyncPlay := Value;
// If playing now - terminate
  if FAsyncPlay and FPlaying then Stop;
end;

//------------------------------------------------------------------------------

procedure TabfWav.SetEventAlias(const Value: string);
begin
  if FEventAlias = Value then Exit;
  FEventAlias := Value;
  if (PlayFrom <> pfSystemEvent) then Exit;
// Stop old sound and play new one
  if FPlaying then Play else Stop;
end;

//------------------------------------------------------------------------------
// Override to prevent storing file into the *.dfm resource when the PlayFrom
// propery is not pfDFM.

procedure TabfWav.SetFileName(const Value: TFileName);
begin
  if FFileName = Value then Exit;
  FFileName := Value;
  if PlayFrom = pfDFM then inherited SetFileName(FFileName);
  if (PlayFrom <> pfDFM) then Exit;
// Stop old sound and play new one
  if FPlaying then Play else Stop;
end;

//------------------------------------------------------------------------------

procedure TabfWav.SetLooped(Value: Boolean);
begin
  if FLooped = Value then Exit;
  FLooped := Value;
  if not FPlaying then Exit;
// If playing now - terminate
  Stop;
// If looped - play again
  if FLooped then Play;
end;

//------------------------------------------------------------------------------

procedure TabfWav.SetNoWaitIfBusy(Value: Boolean);
begin
  if FNoWaitIfBusy = Value then Exit;;
  FNoWaitIfBusy := Value;
  if (not FNoWaitIfBusy) or (not FPlaying) then Exit;
// Terminate old sound and try to play again
  Stop;
  Play;
end;

//------------------------------------------------------------------------------
// If PlayMode is pfDFM then load file to the *.dfm otherwise clear storage.

procedure TabfWav.SetPlayFrom(Value: TabfWavPlayFrom);
begin
  if FPlayFrom = Value then Exit;
  FPlayFrom := Value;
  if PlayFrom <> pfDFM then Clear else Load;
end;

//------------------------------------------------------------------------------

procedure TabfWav.SetResourceName(const Value: string);
begin
  if FResourceName = Value then Exit;
  FResourceName := Value;
  if (PlayFrom <> pfResource) or not FPlaying then Exit;
// Stop the old sound and play the new one
  if FPlaying then Play else Stop;
end;


//==============================================================================
// Properties stored flags

function TabfWav.IsEventAliasStored: Boolean;
begin
  Result := (PlayFrom = pfSystemEvent);
end;

//------------------------------------------------------------------------------

function TabfWav.IsFileNameStored: Boolean;
begin
  Result := (PlayFrom = pfFile);
end;

//------------------------------------------------------------------------------

function TabfWav.IsResourceNameStored: Boolean;
begin
  Result := (PlayFrom = pfResource);
end;


//==============================================================================
// TabfCustomFolderMonitor
//==============================================================================
// Prototype of folder changes notifier
{ TabfCustomFolderMonitor }

constructor TabfCustomFolderMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FFolder := 'C:\'; //  Don't localize
  FWatchSubTree := True;
  FFilters := [fmfFilenameChange, fmfDirNameChange];
end;

//------------------------------------------------------------------------------

destructor TabfCustomFolderMonitor.Destroy;
begin
  Stop;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFolderMonitor.Start;
begin
  Stop;
  if (csDesigning in ComponentState) or (csReading in ComponentState) then Exit;
  if not (Active and Assigned(FOnChange)) then Exit;
  FThread := TabfFolderMonitorThread.Create(OnChange);
  if not FThread.Terminated then
  begin
    FThread.SetDirectoryOptions(Folder, WatchSubTree, GetNotifyOptionFlags);
    FThread.Resume;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFolderMonitor.Stop;
var
  T: TabfFolderMonitorThread;
begin
  if (ComponentState * [csDesigning, csReading]) <> [] then Exit;
  if not Assigned(FThread) then Exit;
  T := FThread;
  FThread := nil;
  T.Terminate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFolderMonitor.Change;
begin
  if Active then Start else Stop;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFolderMonitor.Loaded;
begin
  inherited Loaded;
  SetActive(Active);
end;

//------------------------------------------------------------------------------

function TabfCustomFolderMonitor.GetNotifyOptionFlags: DWORD;
begin
  Result := 0;
  if fmfFileNameChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if fmfDirNameChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if fmfSizeChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if fmfAttributeChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if fmfWriteChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if fmfSecurityChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomFolderMonitor.SetActive(Value: Boolean);
begin
  FActive := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFolderMonitor.SetFilters(Value: TFolderMonitorFilters);
begin
  if FFilters = Value then Exit;
  FFilters := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFolderMonitor.SetFolder(const Value: string);
begin
  if FFolder = Value then Exit;
  FFolder := Value;
  Change;
end;

//------------------------------------------------------------------------------


procedure TabfCustomFolderMonitor.SetWatchSubTree(Value: Boolean);
begin
  if FWatchSubTree = Value then Exit;
  FWatchSubTree := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomFolderMonitor.SetOnChange(Value: TThreadMethod);
begin
  FOnChange := Value;
  Change;
end;


//==============================================================================
// TabfCustomRegistryMonitor
//==============================================================================
// Prototype of registry changes notifier
{ TabfCustomRegistryMonitor }

constructor TabfCustomRegistryMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FRegistryKey := '';
  FRootKey := rrkCurrentUser;
  FWatchSubTree := True;
  FFilters := [rmfSubTreeChange, rmfValueChange];
end;

//------------------------------------------------------------------------------

destructor TabfCustomRegistryMonitor.Destroy;
begin
  Stop;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.Start;
begin
  Stop;
  if (ComponentState * [csDesigning, csReading, csDestroying]) <> [] then Exit;
  if not (Active and Assigned(FOnChange)) then Exit;
  FThread := TabfRegistryMonitorThread.Create(FOnChange);
  if not FThread.Terminated then
  begin
    FThread.SetRegistryKeyOptions(GetRootKey, FRegistryKey, FWatchSubTree,
      GetNotifyOptionFlags);
    FThread.Resume;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.Stop;
var
  T: TabfRegistryMonitorThread;
begin
  if (ComponentState * [csDesigning, csReading]) <> [] then Exit;
  if not Assigned(FThread) then Exit;
  T := FThread;
  FThread := nil;
  T.Terminate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.Change;
begin
  if Active then Start else Stop;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.Loaded;
begin
  inherited Loaded;
  SetActive(Active);
end;

//------------------------------------------------------------------------------

function TabfCustomRegistryMonitor.GetNotifyOptionFlags: DWORD;
begin
  Result := 0;
  if rmfSubTreeChange in FFilters then
    Result := Result or REG_NOTIFY_CHANGE_NAME;
  if rmfAttributeChange in FFilters then
    Result := Result or REG_NOTIFY_CHANGE_ATTRIBUTES;
  if rmfValueChange in FFilters then
    Result := Result or REG_NOTIFY_CHANGE_LAST_SET;
  if rmfSecurityChange in FFilters then
    Result := Result or REG_NOTIFY_CHANGE_SECURITY;
end;

//------------------------------------------------------------------------------

function TabfCustomRegistryMonitor.GetRootKey: HKEY;
const
  cHKEYs: array[TabfRegRootKey] of HKEY = (
    HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE,
    HKEY_USERS, HKEY_CURRENT_CONFIG, HKEY_DYN_DATA);
begin
  Result := cHKEYs[FRootKey];
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomRegistryMonitor.SetActive(Value: Boolean);
begin
  FActive := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.SetFilters(Value: TabfRegistryMonitorFilters);
begin
  if FFilters = Value then Exit;
  FFilters := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.SetRegistryKey(const Value: string);
begin
  if FRegistryKey = Value then Exit;
  FRegistryKey := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.SetRootKey(Value: TabfRegRootKey);
begin
  if FRootKey = Value then Exit;
  FRootKey := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.SetWatchSubTree(Value: Boolean);
begin
  if FWatchSubTree = Value then Exit;
  FWatchSubTree := Value;
  Change;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRegistryMonitor.SetOnChange(Value: TThreadMethod);
begin
  FOnChange := Value;
  Change;
end;


{******************************************************************************}
initialization
{******************************************************************************}
  _OneInstanceMutex := 0;

//------------------------------------------------------------------------------
// With Microsoft Internet Explorer 4.0 and later, the shell notifies
// applications that the taskbar has been created. When the taskbar is created,
// it registers a message with the "TaskbarCreated" string and then broadcasts
// this message to all top-level windows. When your taskbar application receives
// this message, it should assume that any taskbar icons it added have been
// removed and add them again.
  WM_TaskbarRestart := RegisterWindowMessage(PChar('TaskbarCreated'));

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfComponent, TabfCustomThreadComponent, TabfCustomTimer,
    TabfCustomThreadTimer, TabfCustomWndProcHook, TabfCustomFileStorage,
    TabfAutoRun, TabfOneInstance, TabfShutdown, TabfTrayIcon, TabfWndProcHook,
    TabfFileStorage, TabfWav, TabfThreadComponent, TabfThreadTimer,
    TabfFileOperation, TabfFileAssociation, TabfCustomFolderMonitor,
    TabfFolderMonitor, TabfCustomRegistryMonitor, TabfRegistryMonitor,
    TabfStartButtonProperties, TabfColorPicker]);{}

{******************************************************************************}
finalization
{******************************************************************************}
  if Assigned(_WndProcHookList) then FreeAndNil(_WndProcHookList);
  if LongBool(_OneInstanceMutex) then
  begin
    ReleaseMutex(_OneInstanceMutex);
    CloseHandle(_OneInstanceMutex);
  end;

end{unit abfComponents}.
