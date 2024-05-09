{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDialogs;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  {$IfDef D3}ShlObj, ActiveX,{$Else D3}ShlObj2, Ole2,{$EndIf D3}
  Windows, Messages, SysUtils, Dialogs, Forms, Classes, Controls, Graphics,
  FileCtrl, StdCtrls, ExtCtrls,
// ABF VCL  
  abfClasses, abfComponents;


const
  {$IfDef D4}{$EXTERNALSYM BIF_BROWSEINCLUDEURLS}{$EndIf D4}
  BIF_BROWSEINCLUDEURLS  = $0080;
  {$IfDef D4}{$EXTERNALSYM BIF_BROWSEINCLUDEFILES}{$EndIf D4}
  BIF_BROWSEINCLUDEFILES = $4000;
  {$IfDef D4}{$EXTERNALSYM BIF_NEWDIALOGSTYLE}{$EndIf D4}
  BIF_NEWDIALOGSTYLE     = $0040;
  {$IfDef D4}{$EXTERNALSYM BIF_SHAREABLE}{$EndIf D4}
  BIF_SHAREABLE          = $8000;
{$IfNDef D4}
  BIF_EDITBOX            = $0010;
{$EndIf D4}

// SHRunFileDlg Flags
  {$IfDef D4}{$EXTERNALSYM RFF_NOBROWSE}{$EndIf D4}
  RFF_NOBROWSE                = $01; // Removes the browse button.
  {$IfDef D4}{$EXTERNALSYM RFF_NODEFAULT}{$EndIf D4}
  RFF_NODEFAULT               = $02; // No default item selected.
  {$IfDef D4}{$EXTERNALSYM RFF_CALCDIRECTORY}{$EndIf D4}
  RFF_CALCDIRECTORY           = $04; // Calculates the working directory from the file name.
  {$IfDef D4}{$EXTERNALSYM RFF_NOLABEL}{$EndIf D4}
  RFF_NOLABEL                 = $08; // Removes the edit box label.
  {$IfDef D4}{$EXTERNALSYM RFF_NOSEPARATEMEM}{$EndIf D4}
  RFF_NOSEPARATEMEM           = $20; // Removes the Separate Memory Space check box (Windows NT only).

// SHObjectProperties Flags
  {$IfDef D4}{$EXTERNALSYM OPF_PRINTERNAME}{$EndIf D4}
  OPF_PRINTERNAME             = $01;
  {$IfDef D4}{$EXTERNALSYM OPF_PATHNAME}{$EndIf D4}
  OPF_PATHNAME                = $02;


type

//==============================================================================
// TabfCustomDlg
//==============================================================================
// TabfCustomDlg is a prototype of all abfXXX dialogs.

  TabfCustomDlg = class(TabfComponent)
  private
    FCaption: string;
    FIcon: TIcon;
    FImageList: TImageList;
    FText: string;
    procedure UpdateIcon;
  protected
    FImageIndex: TImageIndex;
    function GetOwnerHandle: THandle;
  // Properties Get/Set
    procedure SetIcon(const A: TIcon); virtual;
    procedure SetImageList(const A: TImageList); virtual;
    procedure SetImageIndex(A: TImageIndex); virtual;
  // Properties
    property Caption: string read FCaption write FCaption;
    property Icon: TIcon read FIcon write SetIcon;
    property ImageList: TImageList read FImageList write SetImageList;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex
      default 0;
    property Text: string read FText write FText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; virtual;
  end;{TabfCustomDlg = class(TComponent)}


//==============================================================================
// TabfWinAboutDlg
//==============================================================================
// Shows standard Windows about dialog.

  TabfWinAboutDlg = class(TabfCustomDlg)
  private
    FAppTitle: string;
  public
    function Execute: Boolean; override;
  published
    property About;
    property AppTitle: string read FAppTitle write FAppTitle;
    property Caption;
    property Icon;
    property ImageList;
    property ImageIndex;
    property Text;
  end;{TabfWinAboutDlg = class(TabfCustomDlg)}


//==============================================================================
// TabfSelectDirDlg
//==============================================================================
// Shows select directory dialog.

  TabfSelectDirDlg = class(TabfCustomDlg)
  private
    FHelpCtx: Integer;
    FDirectory: string;
    FOptions: TSelectDirOpts;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property About;
    property Directory: string read FDirectory write FDirectory;
    property HelpCtx: Integer read FHelpCtx write FHelpCtx default 0;
    property Options: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate, sdPerformCreate];
  end;{TabfSelectDirDlg = class(TabfCustomDlg)}


//==============================================================================
// TabfBrowseFolderDlg
//==============================================================================
// Shows Windows 'Browse for Folder' dialog. Has abbility to show a "Create..."
// button that provides creating directory routines.

  EabfBrowseFolderDlg = class(EabfException);

  TabfBrowseFolderDlg = class;

  TabfBrowseFolderDlgRootDir = (sdrNone, sdrRecycleBin, sdrControlPanel,
    sdrDesktop, sdrDesktopDir, sdrDrives, sdrFonts, sdrNetHood, sdrNetwork,
    sdrPersonal, sdrPrinters, sdrPrograms, sdrRecent, sdrSendTo, sdrStartMenu,
    sdrStartUp, sdrTemplates, sdrDesktopExpanded);

  TabfBrowseFolderDlgOption = (sdoComputers, sdoPrinters, sdoOnlyDomains,
    sdoReturnAncest, sdoOnlyDirs, sdoShowStatus, sdoIncludeFiles,
    sdoIncludeUrls, sdoShowEditBox, sdoNewDialogStyle, sdoShareable);
  TabfBrowseFolderDlgOptions = set of TabfBrowseFolderDlgOption;

	TabfBFDlgInitEvent = procedure(Sender: TabfBrowseFolderDlg;
    DialogHandle: THandle) of object;
  // for backward capability
	TabfBrowseFolderDlgInitializationEvent = TabfBFDlgInitEvent;

	TabfBFDlgChangedEvent = procedure(Sender: TabfBrowseFolderDlg;
    DialogHandle: THandle; const ItemIDList: PItemIDList;
    const Folder: string) of object;
  // for backward capability
	TabfBrowseFolderDlgChangedEvent = TabfBFDlgChangedEvent;

  TabfBrowseFolderDlg = class(TabfCustomDlg)
  private
    FRestart: Boolean; // Used to prevent dialog closing when new folder were created
    FCreateButtonVisible: Boolean;
    FCreateButtonEnabled: Boolean;
    FOptions: TabfBrowseFolderDlgOptions;
    FOwnerHandle: THandle;
    FRootDir: TabfBrowseFolderDlgRootDir;
    FPosition: TRect; // Used for saving and restoring a position of dialog.
    FOnSelectionChanged: TabfBFDlgChangedEvent;
    FOnInitialization: TabfBFDlgInitEvent;
    FStatusTextAsPath: Boolean;
    FStatusText: string;
    function GetOptions: Integer;
    function GetRootID: PItemIDList;
    procedure ResetPosition;
    procedure SavePosition;
    procedure UpdatePosition;
    function OldStyleDialog: Boolean;
  // Properties Get/Set 
    procedure SetCreateButtonEnabled(A: Boolean);
    procedure SetCreateButtonVisible(A: Boolean);
    function GetItem: string;
    procedure SetItem(const A: string);
    procedure SetSelectedFolder(const A: string);
    procedure SetStatusText(const A: string);
    function IsStatusTextStored: Boolean;
  protected
    FHandle: THandle;
    FDisplayName, FFolder, FSelectedFolder: string;
    FValidPath: Boolean;
    FTreeViewHandle, FBtnOKHandle, FBtnCancelHandle, FBtnCreateHandle: THandle;
    procedure Loaded; override;
    procedure ResetHandles; virtual;
    procedure CreateCreateButton; virtual;
    procedure DestroyCreateButton; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  // Properties
		property DisplayName: string read FDisplayName;
    property Handle: THandle read FHandle;
		property ImageIndex: TImageIndex read FImageIndex;
    property OwnerHandle: THandle read FOwnerHandle write FOwnerHandle;
		property SelectedFolder: string read FSelectedFolder
      write SetSelectedFolder;
    // for backward capability
    property Item: string read GetItem write SetItem stored False;
  published
  // Properties
    property About;
    property Caption;
    property CreateButtonVisible: Boolean read FCreateButtonVisible
      write SetCreateButtonVisible default True;
    property CreateButtonEnabled: Boolean read FCreateButtonEnabled
      write SetCreateButtonEnabled default True;
    property Folder: string read FFolder write FFolder;
    property Options: TabfBrowseFolderDlgOptions read FOptions write FOptions
      default [sdoOnlyDirs, sdoShowStatus, sdoNewDialogStyle];
    property RootDir: TabfBrowseFolderDlgRootDir read FRootDir write FRootDir
      default sdrNone;
    property Text;
    property StatusText: string read FStatusText write SetStatusText
      stored IsStatusTextStored;
    property StatusTextAsPath: Boolean read FStatusTextAsPath
      write FStatusTextAsPath default True;
  // Events
{$IfNDef C1_ONLY}
    property OnInitialization: TabfBFDlgInitEvent read FOnInitialization
      write FOnInitialization;
    property OnSelectionChanged: TabfBFDlgChangedEvent
      read FOnSelectionChanged write FOnSelectionChanged;
{$EndIf C1_ONLY}
  end;{TabfBrowseFolderDlg = class(TabfCustomDlg)}


//==============================================================================
// TabfCplDlg
//==============================================================================
// Shows any CPL dialog present in the Control Panel of the system or the
// Control Panel itself. Also you can run own Cpl applets using the TabfCplDlg
// component. Use cplCustom type, CplName property to specify a library file
// and the CplApplet property to specify a number of applet in the library.
// Date: 07/30/2000

  TabfCplType = (
    cplDefault, // Shows the Control Panel.
    cplCustom,  // Name of Library are taken from the CplName property
    cplAccessibility, cplAddRemoveHardware, cplAddRemoveProgram,
    cplAdministrativeTools, cplDateTime, cplDisplay, cplFax, cplFolderOptions,
    cplFonts, cplGameControllers, cplInternetOptions, cplKeyboard, cplMouse,
    cplModem, cplMultimedia, cplNetwork, cplODBC, cplPasswords, cplPhone,
    cplPowerOptions, cplPrinters, cplRegionalOptions, cplScanners,
    cplScheduledTasks, cplSound, cplSystem, cplUsers);

  TabfCplDlg = class(TabfCustomDlg)
  private
    FCplApplet: Word;
    FCplName: TFileName;
    FCplType: TabfCplType;
    FPageNumber: Word;
    procedure SetPageNumber(A: Word);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property About;
    property CplName: TFileName read FCplName write FCplName;
    property CplType: TabfCplType read FCplType write FCplType default cplDefault;
    property CplApplet: Word read FCplApplet write FCplApplet default 0;
    property PageNumber: Word read FPageNumber write SetPageNumber default 0;
  end;{TabfCplDlg = class(TabfCustomDlg)}


//==============================================================================
// TabfRunDlg
//==============================================================================
// Implementation of the Run dialog.

  TabfRunDlgOption = (rdoNoBrowse, rdoNoDefault, rdoCalcDirectory, rdoNoLabel,
    rdoNoSeparateMem);
  TabfRunDlgOptions = set of TabfRunDlgOption;

  TabfRunDlg = class(TabfCustomDlg)
  private
    FDirectory: string;
    FOptions: TabfRunDlgOptions;
    function GetOptions: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property About;
    property Caption;
    property Icon;
    property ImageList;
    property ImageIndex;
    property Directory: string read FDirectory write FDirectory;
    property Options: TabfRunDlgOptions read FOptions write FOptions default [];
    property Text;
  end;


//==============================================================================
// TabfOpenWithDlg
//==============================================================================
// "Open With..." dialog.

  TabfOpenWithDlg = class(TabfCustomDlg)
  private
    FFileName: TFileName;
  public
    function Execute: Boolean; override;
  published
    property About;
    property FileName: TFileName read FFileName write FFileName;
  end;


//==============================================================================
// TabfObjectPropertiesDlg
//==============================================================================
// Implementation of "Object Properties" dialog.

  TabfObjectPropertiesDlgType = (pdtFile, pdtPrinter);

  TabfObjectPropertiesDlg = class(TabfCustomDlg)
  private
    FFileType: TabfObjectPropertiesDlgType;
    FFileName: TFileName;
  public
    constructor Create(AOwner: TComponent);override;
    function Execute: Boolean; override;
  published
    property About;
    property FileName: TFileName read FFileName write FFileName;
    property FileType: TabfObjectPropertiesDlgType read FFileType write FFileType
      default pdtFile;
  end;


//==============================================================================
// TabfPickIconDlg
//==============================================================================
// Implementation of "Change Icon" dialog.

  TabfPickIconDlg = class(TabfCustomDlg)
  private
    FFileName: TFileName;
    FIconIndex: Integer;
    procedure SetFileName(const Value: TFileName);
    procedure SetIconIndex(Value: Integer);
    function  GetLargeIcon: TIcon;
    procedure SetLargeIcon(const Value: TIcon);
    procedure SetSmallIcon(const Value: TIcon);
    function IsFileNameStored: Boolean;
  protected
    FSmallIcon: TIcon;
    procedure UpdateIcons; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
  published
    property About;
    property FileName: TFileName read FFileName write SetFileName
      stored IsFileNameStored;
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property LargeIcon: TIcon read GetLargeIcon write SetLargeIcon stored False;
    property SmallIcon: TIcon read FSmallIcon write SetSmallIcon stored False;
  end;


//==============================================================================
// TabfFindDlg
//==============================================================================
// Find files/computer dialog.

  TabfFindDlgKind = (fdkFiles, fdkComputer);

  TabfFindDlg = class(TabfCustomDlg)
  private
    FDirectory: string;
    FKind: TabfFindDlgKind;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
    function FindComputer: Boolean; virtual;
    function FindFiles: Boolean; virtual;
  published
    property About;
    property Directory: string read FDirectory write FDirectory;
    property Kind: TabfFindDlgKind read FKind write FKind default fdkFiles;
  end;


//==============================================================================
// TabfCreateShortcutDlg
//==============================================================================
// Create shortcut shell dialog. Runs wizard for creating shortacat in a folder
// is specified by Directory property.

  TabfCreateShortcutDlg = class(TabfCustomDlg)
  private
    FDirectory: string;
  public
    function Execute: Boolean; override;
  published
    property About;
    property Directory: string read FDirectory write FDirectory;
  end;


//==============================================================================
// TabfAddToFavoritesDlg
//==============================================================================
// Shows "Add to Favorites" dialog

  TabfAddToFavoritesDlg = class(TabfCustomDlg)
  protected
    FFileName: TFileName;
    FURL: string;
    FTitle: string;
  public
    function Execute: Boolean; override;
  // Properties
    property FileName: TFileName read FFileName;
  published
    property About;
    property Title: string read FTitle write FTitle;
    property URL: string read FURL write FURL;
  end;


//==============================================================================
// TabfOrganizeFavoritesDlg
//==============================================================================
// Shows "Organize Favorites" dialog

  TabfOrganizeFavoritesDlg = class(TabfCustomDlg)
  protected
    FFolder: string;
  public
    function Execute: Boolean; override;
  published
    property About;
    property Folder: string read FFolder write FFolder;
  end;


//==============================================================================
//  TabfMessageForm
//==============================================================================
// Class of the message dialog form.
// Date: 04/01/2000

  TabfMessageForm = class(TForm)
  protected
    lbMessage: TLabel;
    procedure HelpButtonClick(Sender: TObject); virtual;
    procedure CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WriteToClipBoard(const Text: string);
    function GetFormText: string;
  public
  {$IfNDef D4}
    constructor CreateNew(AOwner: TComponent
      {$IfDef Builder}; Dummy: Integer{$EndIf});
  {$Else D4}
    constructor CreateNew(AOwner: TComponent
    {$IfDef Builder}; Dummy: Integer = 1{$EndIf}); reintroduce;
  {$EndIf D4}
  end;


//==============================================================================
// DialogBox routines
//==============================================================================

//------------------------------------------------------------------------------
// Creates a message dialog form that can be used several times in application.
// abfCreateMessageDialog returns a dialog of the type specified by the DlgType
// parameter and with the buttons indicated by the Buttons parameter.
// DefButtonIndex specifies a defult button of the dialog. Other parameters
// influence on dilog view.
function abfCreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: LongInt;
  DlgColor: TColor; const DlgFont: TFont; const DlgCaption: string;
  const DlgIcon: TGraphic): TForm;

//------------------------------------------------------------------------------
// Brings up a message box and obtain the user's response. The message box
// displays the value of the Msg parameter. Use the DlgType parameter to
// indicate the purpose of the dialog. Use the Buttons parameter to indicate
// what buttons should appear in the message box. DefButtonIndex specifies a
// defult button of the dialog. Use the HelpCtx parameter to specify the context
// ID for the help topic. Other parameters influence on dilog view. The dialog
// appears in the center of screen if MasterForm is nil, otherwise dialog
// appears in the center of MasterForm but fully fit to the screen working area.
// Returns the value of the button the user selected.
function abfMessageDlgEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: LongInt;
  DlgColor: TColor; const DlgFont: TFont; const DlgCaption: string;
  const DlgIcon: TGraphic; const MasterForm: TForm): Integer;

//------------------------------------------------------------------------------
// Same to the abfMessageDlgEx function but has a less number of parameters
function abfMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: LongInt;
  const DlgCaption: string; const DlgIcon: TGraphic;
  const MasterForm: TForm): Integer;

//------------------------------------------------------------------------------
// Same to the abfMessageDlg function but allows set a dilog position and
// specify the help file
function abfMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: Longint;
  const DlgCaption: string; const DlgIcon: TGraphic; X, Y: Integer;
  const HelpFileName: string): Integer;

//------------------------------------------------------------------------------
// Same to the abfMessageDlg function but allows set a dilog position.
function abfMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: Longint;
  const DlgCaption: string; const DlgIcon: TGraphic; X, Y: Integer): Integer;

//------------------------------------------------------------------------------
// Same to the standard InputQuery function but more powerful.
function abfInputQueryEx(const ACaption, APrompt: string;
  DefButtonIndex, HelpCtx: LongInt; DlgColor: TColor;
  const DlgFont: TFont; const MasterForm: TForm; var Value: string): Boolean;

//------------------------------------------------------------------------------
// Same to the abfInputQueryEx function but has less parameters.
function abfInputQuery(const ACaption, APrompt: string;
  DefButtonIndex, HelpCtx: LongInt; const MasterForm: TForm;
  var Value: string): Boolean;

//------------------------------------------------------------------------------
// Same to the standard InputBox function but uses our message routines.
function abfInputBox(const ACaption, APrompt, ADefault: string;
  DefButtonIndex, HelpCtx: LongInt; const MasterForm: TForm): string;


//==============================================================================
// Shell Routines
//==============================================================================

//------------------------------------------------------------------------------
// RunFileDlg is probably not as useful, but still warants mentioning since it
// is amazingly flexible. It is the dialog that you see when launching
// Applications from the Start/Run menu.
//
// Owner - identifies the window that owns the dialog box.
// Icon - is the handle of the icon that will be displayed in the dialog. if it
//   is NULL, the default icon will be used.
// Directory - points to a string that specifies the working directory.
// Title - points to a string to be placed in the title bar of the dialog
//   box. if it is NULL, the default title is used.
// Description - points to a stringthat is displayed in the dialog, briefly
//   informing the user what to do. if it is NULL, the default description is
//   used.
// Flags - is a set of bit flags that specify other properties of the dialog.
//
// A nice feature of this dialog is that it allows you to control which
// Applications the user may run. When the user selects the OK button, your
// parent window is sent a notification with details of the program that is
// about to be started. The notification is in the form of a WM_NOTIFY message
// with the notification code set to RFN_VALIDATE (-510) and the lParam pointing
// to an NM_RUNFILEDLG. The return Value determines whether the Application
// will be run or not.
type
  TSHRunFileDlg = function(Owner: HWND; Icon: HICON; Directory, Title,
    Description: PWideChar; Flags: LongInt): LongInt; stdcall;

var
  SHRunFileDlg: TSHRunFileDlg;

//------------------------------------------------------------------------------
// PickIconDlg. Obviously enough, it provides an interface with which the user
// can select an icon. It's used by the file type editor when selecting the
// icon to associate with a particular file type. It's also used in the
// shortcut properties dialog when changing the icon.
//   Owner - identifies the window that owns the dialog box.
//   FileName - points to a buffer containing the initial filename. When the
//     function returns, this buffer will contain the new file name.
//   BufferSize - specifies the size, in characters, of the buffer.
//   IconIndex - points to a variable containing the zero based offset of the
//     icon. When the function returns, the variable will be set to the new icon
//     index.
// if the user selects an icon, the return Value is TRUE. It is FALSE if the
// user chooses the Cancel button, or the Close command on the System menu.
type
  TSHPickIconDlg = function(Owner: HWND; FileName: LPWSTR;
    var BufferSize: DWORD; var IconIndex: DWORD): Boolean; stdcall;

var
  SHPickIconDlg: TSHPickIconDlg;

//------------------------------------------------------------------------------
// SHFindFiles and SHFindComputer functions give you access to the two system
// Find dialogs on the Start menu. Typically you would specify NULL for both
// parameters, which would start the dialogs as if the user had selected them
// from the Start menu. However, if you want to search for files rooted at a
// particular folder (as if the user had selected Find from the folder's context
// menu), you should set the Root parameter to the pidl of the folder. This only
// works for the SHFindFiles function though, the parameter is ignored by
// SHFindComputer. You can also open a previously saved search by specifying the
// pidl of the file (a .fnd file) in the SavedSearch parameter. Once again, this
// parameter is only supported with the SHFindFiles function it appears to be
// ignored by SHFindComputer.
// For both functions, the dialog is started in a separate thread so the
// function call will return almost immediately. The return Value is TRUE if
// the thread was started successfully or FALSE if there was an error of some
// sort. If you have specified a Value for the Root parameter, it is your
// responsibility to free it when the function returns. The SavedSearch
// parameter, on the other hand, is usually freed by the system you should only
// attempt to free it if the function returns FALSE.
type
  TSHFindComputer = function(Root, SavedSearch: PItemIDList): Boolean; stdcall;
  TSHFindFiles = function(Root, SavedSearch: PItemIDList): Boolean; stdcall;

var
  SHFindComputer: TSHFindComputer;
  SHFindFiles: TSHFindFiles;

//------------------------------------------------------------------------------
// SHObjectProperties is what you would use to display the properties dialog for
// a file or folder. It can also be used to display the properties for a printer
// object.
//   Owner - identifies the window that owns the dialog.
//   Name - points to a string containing the path name or the printer name
//     whose properties will be displayed.
//   Flags - specifies the type of name contained in Name.
//   Parameters - points to a string containing the name of the page that will
//     initially be selected. if Parameters is NULL, the first page on the
//     property sheet will be selected.
// if the function succeeds, the return Value is TRUE. If the function fails,
// the return Value is FALSE. To get extended error information, call
// GetLastError. Note that this dialog is actually modeless, so when the
// function returns the dialog will probably still be open. There is no way of
// knowing when the user has closed the dialog. You can quite easily accomplish
// the same thing with a call to the documented function ShellExecuteEx,
// specifying "properties" for the Verb parameter. This doesn't appear to work
// for printer names though.
type
  TSHObjectProperties = function(Owner: HWND; Flags: LongInt;
    Name, Parameters: LPWSTR): Boolean; stdcall;

var
  SHObjectProperties: TSHObjectProperties;


{******************************************************************************}
implementation
{******************************************************************************}

uses
  Consts, ShellApi,
// ABF VCL
  abfConsts, abfStrUtils, abfGraphics, abfSysUtils, abfShlUtils,
  abfVclConsts, abfVclUtils, abfIniFiles, abfDialogsConsts;

const
  SShellDll   = 'shell32.dll';
  SShDocVwDll = 'shdocvw.dll';

{$I abf_init.inc}


//==============================================================================
// Internal routines

{$IfNDef D5}

function SafeLoadLibrary(const FileName: string): HMODULE;
var
  ErrorMode: UINT;
  OldMode: UINT;
  FPUControlWord: Word;
begin
  ErrorMode := SEM_NOOPENFILEERRORBOX;
  OldMode := SetErrorMode(ErrorMode);
  try
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(FileName));
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;

{$EndIf D5}


//==============================================================================
//  TabfMessageForm
//==============================================================================
// Class of the message dialog form.
// Date: 05/30/2003

{$IfNDef D4}
constructor TabfMessageForm.CreateNew(AOwner: TComponent
  {$IfDef Builder}; Dummy: Integer{$EndIf});
{$Else D4}
constructor TabfMessageForm.CreateNew(AOwner: TComponent
  {$IfDef Builder}; Dummy: Integer = 1{$EndIf Builder});
{$EndIf D4}
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner{$IfDef Builder}, 0{$EndIf});
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
end;

//------------------------------------------------------------------------------

procedure TabfMessageForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

//------------------------------------------------------------------------------
// Place to clipbord the text representation of dialog on "Ctrl+C"

procedure TabfMessageForm.CustomKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('C')) then
  begin
    Beep;
    WriteToClipBoard(GetFormText);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfMessageForm.WriteToClipBoard(const Text: string);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  if not OpenClipBoard(0) then raise Exception.Create(SCannotOpenClipboard);

  try
    Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Length(Text) + 1);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(PChar(Text)^, DataPtr^, Length(Text) + 1);
        EmptyClipBoard;
        SetClipboardData(CF_TEXT, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    CloseClipBoard;
  end;
end;

//------------------------------------------------------------------------------

function TabfMessageForm.GetFormText: string;
var
  DividerLine, ButtonCaptions: string;
  i: Integer;
begin
  DividerLine := StringOfChar('-', 27) + CRLF;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TButton then
      ButtonCaptions := ButtonCaptions + TButton(Components[i]).Caption +
        StringOfChar(' ', 3);
  ButtonCaptions := abfRemoveCharSet(['&'], ButtonCaptions);
  Result := Format('%s%s%s%s%s%s%s%s%s%s', [DividerLine, Caption, CRLF,
    DividerLine, lbMessage.Caption, CRLF, DividerLine, ButtonCaptions,
    CRLF, DividerLine]);
end;


//==============================================================================
// DialogBox routines
//==============================================================================

var

{$IfNDef D3}
  Captions: array[TMsgDlgType] of PChar = (SMsgDlgWarning, SMsgDlgError,
    SMsgDlgInformation, SMsgDlgConfirm, nil);
  ButtonCaptions: array[TMsgDlgBtn] of PChar = (SMsgDlgYes, SMsgDlgNo,
    SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort, SMsgDlgRetry, SMsgDlgIgnore,
    SMsgDlgAll, SMsgDlgHelp);
{$Else D3}
  Captions: array[TMsgDlgType] of Pointer = (@SMsgDlgWarning, @SMsgDlgError,
    @SMsgDlgInformation, @SMsgDlgConfirm, nil);
  ButtonCaptions: array[TMsgDlgBtn] of Pointer = (@SMsgDlgYes, @SMsgDlgNo,
    @SMsgDlgOK, @SMsgDlgCancel, @SMsgDlgAbort, @SMsgDlgRetry, @SMsgDlgIgnore,
    @SMsgDlgAll, @SMsgDlgNoToAll, @SMsgDlgYesToAll, @SMsgDlgHelp{$IFDEF D9},@SMsgDlgHelpNone{$ENDIF});
{$EndIf D3}
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonNames: array[TMsgDlgBtn] of string = ('Yes', 'No', 'OK', 'Cancel',
    'Abort', 'Retry', 'Ignore', 'All',{$IfDef D3}'NoToAll', 'YesToAll',{$EndIf}
    'Help'{$IFDEF D9}, ''{$ENDIF});
  ModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll,{$IfDef D3}mrNoToAll, mrYesToAll,{$EndIf}
    0, 0);

var
  ButtonWidths: array[TMsgDlgBtn] of Integer;  // initialized to zero

//------------------------------------------------------------------------------
// Creates a message dialog form that can be used several times in application.
// abfCreateMessageDialog returns a dialog of the type specified by the DlgType
// parameter and with the buttons indicated by the Buttons parameter.
// DefButtonIndex specifies a defult button of the dialog. Other parameters
// influence on dilog view.
// Date: 05/30/2003

function abfCreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: LongInt;
  DlgColor: TColor; const DlgFont: TFont; const DlgCaption: string;
  const DlgIcon: TGraphic): TForm;
const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
var
  DialogUnits: TPoint;
  i, HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
  ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
  IconTextWidth, IconTextHeight, X, ALeft, BtnIndex: Integer;
  B, DefaultButton, CancelButton: TMsgDlgBtn;
  IconID: PChar;
  TextRect: TRect;
begin
  Result := TabfMessageForm.CreateNew(Application{$IfDef Builder}, 0{$EndIf});
  with Result do
  begin
    HelpContext := HelpCtx;
    Color := DlgColor;
    if Assigned(DlgFont) then Font := DlgFont;
{$IfDef D4}
    BiDiMode := Application.BiDiMode;
{$EndIf D4}
    BorderStyle := bsDialog;

  // Key routines for "Ctrl+C"  
    KeyPreview := True;
    OnKeyDown := TabfMessageForm(Result).CustomKeyDown;

  // Calculate button metrics depending on Font and Dilog units
    Canvas.Font := Font;
    DialogUnits := abfGetAverageCharSize(Canvas);
    HorzMargin := MulDiv(mcHorzMargin, DialogUnits.X, 4);
    VertMargin := MulDiv(mcVertMargin, DialogUnits.Y, 8);
    HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
    VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if B in Buttons then
      begin
        if ButtonWidths[B] = 0 then
        begin
          TextRect := Rect(0, 0, 0, 0);
          Windows.DrawText(Canvas.Handle,
            PChar({$IfDef D3}LoadResString{$EndIf}(ButtonCaptions[B])), -1,
            TextRect,
{$IfDef D4}
            DrawTextBiDiModeFlagsReadingOnly or
{$EndIf D4}
            DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
          with TextRect do
            ButtonWidths[B] := Right - Left + 8;
        end;
        if ButtonWidths[B] > ButtonWidth then ButtonWidth := ButtonWidths[B];
      end;{if B in Buttons then}
    end;
    ButtonHeight := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
    ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);

  // Calculate Text rect size
    SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
    DrawText(Canvas.Handle, PChar(Msg), Length(Msg) + 1, TextRect,
{$IfDef D4}
      DrawTextBiDiModeFlagsReadingOnly or
{$EndIf D4}
      DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK);

  // Calculate icon place size
    IconID := nil;
    IconTextWidth := TextRect.Right;
    IconTextHeight := TextRect.Bottom;
    if Assigned(DlgIcon) and not DlgIcon.Empty then
    begin
      Inc(IconTextWidth, Max(DlgIcon.Width, 32) + HorzSpacing);
      if IconTextHeight < DlgIcon.Height then IconTextHeight := DlgIcon.Height;
    end else
    begin // Old style, like in standard MessageBox
      IconID := IconIDs[DlgType];
      if IconID <> nil then
      begin
        Inc(IconTextWidth, 32 + HorzSpacing);
        if IconTextHeight < 32 then IconTextHeight := 32;
      end;
    end;

  // Calculate button place size
    ButtonCount := 0;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then Inc(ButtonCount);
    ButtonGroupWidth := 0;
    if ButtonCount <> 0 then
      ButtonGroupWidth := ButtonWidth * ButtonCount +
        ButtonSpacing * (ButtonCount - 1);

  // Set form size and position
    ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
    ClientHeight := IconTextHeight + ButtonHeight + VertSpacing +
      VertMargin * 2;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);

  // Select caption text
    if DlgCaption <> '' then Caption := DlgCaption else
    if DlgType <> mtCustom then
{$IfNDef D3}
      Caption := string(Captions[DlgType])
{$Else D3}
      Caption := LoadResString(Captions[DlgType])
{$EndIf D3}
    else
      Caption := Application.Title;

  // Create image for dilog icon
    if (Assigned(DlgIcon) and not DlgIcon.Empty) or (IconID <> nil) then
      with TImage.Create(Result) do
      begin
        Name := 'Image';
        Parent := Result;
        if (Assigned(DlgIcon) and not DlgIcon.Empty) then Picture.Assign(DlgIcon)
        else Picture.Icon.Handle := LoadIcon(0, IconID);
        AutoSize := True; // Resize to size of graphic
        SetBounds(HorzMargin, VertMargin, Width, Height);
      end;

  // Create a lable to show the message
    TabfMessageForm(Result).lbMessage := TLabel.Create(Result);
    with TabfMessageForm(Result).lbMessage do
    begin
      Name := 'Message';
      Transparent := True;
      Parent := Result;
      WordWrap := True;
      Caption := Msg;
      BoundsRect := TextRect;
{$IfDef D4}
      BiDiMode := Result.BiDiMode;
{$EndIf D4}
      ALeft := IconTextWidth - TextRect.Right + HorzMargin;
{$IfDef D4}
      if UseRightToLeftAlignment then
        ALeft := Result.ClientWidth - ALeft - Width;
{$EndIf D4}
      SetBounds(ALeft, VertMargin, TextRect.Right, TextRect.Bottom);
    end;{with TLabel.Create(Result) do}

  // Determine a default button
    if mbOk in Buttons then DefaultButton := mbOk else
    if mbYes in Buttons then DefaultButton := mbYes
    else DefaultButton := mbRetry;

  // Determine a cancel button
    if mbCancel in Buttons then CancelButton := mbCancel else
    if mbNo in Buttons then CancelButton := mbNo
    else CancelButton := mbOk;

  // Create button(s)
    X := (ClientWidth - ButtonGroupWidth) div 2;
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if B in Buttons then
        with TButton.Create(Result) do
        begin
          Name := ButtonNames[B];
          Parent := Result;
{$IfNDef D3}
          Caption := string(ButtonCaptions[B]);
{$Else D3}
          Caption := LoadResString(ButtonCaptions[B]);
{$EndIf D3}
          ModalResult := ModalResults[B];
          if B = DefaultButton then Default := True;
          if B = CancelButton then Cancel := True;
          SetBounds(X, IconTextHeight + VertMargin + VertSpacing,
            ButtonWidth, ButtonHeight);
          Inc(X, ButtonWidth + ButtonSpacing);
          if B = mbHelp then OnClick := TabfMessageForm(Result).HelpButtonClick;
        end;

  // Set default button if specified
    if DefButtonIndex <= 0 then Exit;
    BtnIndex := 1;
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TButton) then
        if BtnIndex = DefButtonIndex then
        begin
          ActiveControl := TWinControl(Controls[i]);
          TButton(Controls[i]).Default := True; // Can be remarked
          Exit;
        end else
          Inc(BtnIndex);

  end;{with Result do}
end;{function abfCreateMessageDialog}

//------------------------------------------------------------------------------
// Brings up a message box and obtain the user's response. The message box
// displays the value of the Msg parameter. Use the DlgType parameter to
// indicate the purpose of the dialog. Use the Buttons parameter to indicate
// what buttons should appear in the message box. DefButtonIndex specifies a
// defult button of the dialog. Use the HelpCtx parameter to specify the context
// ID for the help topic. Other parameters influence on dilog view. The dialog
// appears in the center of screen if MasterForm is nil, otherwise dialog
// appears in the center of MasterForm but fully fit to the screen working area.
// Returns the value of the button the user selected.
// Date: 06/24/2002

function abfMessageDlgEx(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: LongInt;
  DlgColor: TColor; const DlgFont: TFont; const DlgCaption: string;
  const DlgIcon: TGraphic; const MasterForm: TForm): Integer;
var
  frmMessageBox: TForm;
begin
  frmMessageBox := abfCreateMessageDialog(Msg, DlgType, Buttons, DefButtonIndex,
    HelpCtx, DlgColor, DlgFont, DlgCaption, DlgIcon);
  with frmMessageBox do
  try
  // Center form relatively to the MasterForm
    abfFormCenterForm(frmMessageBox, MasterForm);

  // Make form "non-sinkable" by Alt+Tab
//    if Assigned(MasterForm) and MasterForm.HandleAllocated then
//      ParentWindow := MasterForm.Handle;

  // Show form and return get result
    Result := ShowModal;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Same to the abfMessageDlgEx function but has a less number of parameters
// Date: 09/01/2000

function abfMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: LongInt;
  const DlgCaption: string; const DlgIcon: TGraphic;
  const MasterForm: TForm): Integer;
begin
  Result := abfMessageDlgEx(Msg, DlgType, Buttons,
    DefButtonIndex, HelpCtx, clBtnFace, nil, DlgCaption, DlgIcon, MasterForm);
end;

//------------------------------------------------------------------------------
// Same to the abfMessageDlg function but allows set a dilog position and
// specify the help file
// Date: 09/01/2000

function abfMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: Longint;
  const DlgCaption: string; const DlgIcon: TGraphic; X, Y: Integer;
  const HelpFileName: string): Integer;
var
  frmMessageBox: TForm;
begin
  frmMessageBox := abfCreateMessageDialog(Msg, DlgType, Buttons, DefButtonIndex,
    HelpCtx, clBtnFace, nil, DlgCaption, DlgIcon);
  with frmMessageBox do
  try
{$IfDef D3}
    HelpFile := HelpFileName;
{$EndIf D3}
    if X >= 0 then Left := X;
    if Y >= 0 then Top := Y;
    if (Y < 0) and (X < 0) then Position := poScreenCenter;
    Result := ShowModal;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Same to the abfMessageDlg function but allows set a dilog position.
// Date: 09/01/2000

function abfMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefButtonIndex, HelpCtx: Longint;
  const DlgCaption: string; const DlgIcon: TGraphic; X, Y: Integer): Integer;
begin
  Result := abfMessageDlgPosHelp(Msg, DlgType, Buttons, DefButtonIndex, HelpCtx,
    DlgCaption, DlgIcon, X, Y, '');
end;

//------------------------------------------------------------------------------
// Same to the standard InputQuery function but more powerful.
// Date: 06/24/2002

function abfInputQueryEx(const ACaption, APrompt: string;
  DefButtonIndex, HelpCtx: LongInt; DlgColor: TColor;
  const DlgFont: TFont; const MasterForm: TForm; var Value: string): Boolean;
var
  frmQuery: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;

  frmQuery := TForm.Create(Application);
  with frmQuery do
  try
    HelpContext := HelpCtx;
    Color := DlgColor;
    if Assigned(DlgFont) then Font := DlgFont;
{$IfDef D4}
    BiDiMode := Application.BiDiMode;
{$EndIf D4}

    Canvas.Font := Font;
    DialogUnits := abfGetAverageCharSize(Canvas);
    BorderStyle := bsDialog;
    Caption := ACaption;
    ClientWidth := MulDiv(180, DialogUnits.X, 4);
    ClientHeight := MulDiv(63, DialogUnits.Y, 8);
//    Position := poScreenCenter;

  // Lable
    Prompt := TLabel.Create(frmQuery);
    with Prompt do
    begin
      Name := 'Label';
      Parent := frmQuery;
      AutoSize := True;
      Left := MulDiv(8, DialogUnits.X, 4);
      Top := MulDiv(8, DialogUnits.Y, 8);
      Caption := APrompt;
    end;

  // Edit
    Edit := TEdit.Create(frmQuery);
    with Edit do
    begin
      Name := 'Edit';
      Parent := frmQuery;
      Left := Prompt.Left;
      Top := MulDiv(19, DialogUnits.Y, 8);
      Width := MulDiv(164, DialogUnits.X, 4);
      MaxLength := 255;
      Text := Value;
      SelectAll;
    end;

    ButtonTop := MulDiv(41, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(50, DialogUnits.X, 4);
    ButtonHeight := MulDiv(14, DialogUnits.Y, 8);

  // Button OK
    with TButton.Create(frmQuery) do
    begin
      Name := ButtonNames[mbOK];
      Parent := frmQuery;
{$IfNDef D3}
      Caption := string(ButtonCaptions[mbOK]);
{$Else D3}
      Caption := LoadResString(ButtonCaptions[mbOK]);
{$EndIf D3}
      ModalResult := mrOk;
      if DefButtonIndex <= 1 then Default := True;
      SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;

  // Button Cancel
    with TButton.Create(frmQuery) do
    begin
      Name := ButtonNames[mbCancel];
      Parent := frmQuery;
{$IfNDef D3}
      Caption := string(ButtonCaptions[mbCancel]);
{$Else D3}
      Caption := LoadResString(ButtonCaptions[mbCancel]);
{$EndIf D3}
      ModalResult := mrCancel;
      Cancel := True;
      if DefButtonIndex > 1 then Default := True;
      SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;

  // Center form relatively to the MasterForm
    abfFormCenterForm(frmQuery, MasterForm);

  // Make form "non-sinkable" by Alt+Tab
//    if Assigned(MasterForm) and MasterForm.HandleAllocated then
//      ParentWindow := MasterForm.Handle;

  // Show form and return get result
    if ShowModal = mrOk then
    begin
      Value  := Edit.Text;
      Result := True;
    end;
  finally
    frmQuery.Free;
  end;
end;{function abfInputQueryEx}

//------------------------------------------------------------------------------
// Same to the abfInputQueryEx function but has less parameters.
// Date: 09/20/2000

function abfInputQuery(const ACaption, APrompt: string;
  DefButtonIndex, HelpCtx: LongInt; const MasterForm: TForm;
  var Value: string): Boolean;
begin
  Result := abfInputQueryEx(ACaption, APrompt, DefButtonIndex, HelpCtx,
   clBtnFace, nil, MasterForm, Value);
end;

//------------------------------------------------------------------------------
// Same to the standard InputBox function but uses our message routines.
// Date: 09/20/2000

function abfInputBox(const ACaption, APrompt, ADefault: string;
  DefButtonIndex, HelpCtx: LongInt; const MasterForm: TForm): string;
begin
  Result := ADefault;
  abfInputQuery(ACaption, APrompt, DefButtonIndex, HelpCtx, MasterForm, Result);
end;


//==============================================================================
// TabfCustomDlg
//==============================================================================
// TabfCustomDlg is a prototype of all abfXXX dialogs.
// Date: 04/01/2000
{ TabfCustomDlg }

constructor TabfCustomDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FIcon := TIcon.Create;
  FImageIndex := 0;
end;

//------------------------------------------------------------------------------

destructor TabfCustomDlg.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfCustomDlg.Execute: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------
// Determine current owner handle

function TabfCustomDlg.GetOwnerHandle: THandle;
begin
  Result := 0;
  if Assigned(Owner) then
  begin
    if (Owner is TWinControl) then Result := TWinControl(Owner).Handle else
    if (Owner is TApplication) then Result := TApplication(Owner).Handle;
  end;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomDlg.SetIcon(const A: TIcon);
begin
  FIcon.Assign(A);
end;

//------------------------------------------------------------------------------

procedure TabfCustomDlg.SetImageList(const A: TImageList);
begin
  FImageList := A;
  UpdateIcon;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDlg.SetImageIndex(A: TImageIndex);
begin
  if FImageIndex = A then Exit;
  FImageIndex := A;
  UpdateIcon;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDlg.UpdateIcon;
begin
  if not Assigned(FImageList) then Exit;
  FIcon.Assign(nil);
  FImageList.GetIcon(FImageIndex, FIcon);
end;


//==============================================================================
// TabfWinAboutDlg
//==============================================================================
// Shows standard Windows about dialog.
// Date: 04/01/2000
{ TabfWinAboutDlg }

function TabfWinAboutDlg.Execute: Boolean;
var
  hIcon: THandle;
  Title, Msg: string;
begin
// Prepare icon handle
  hIcon := 0;
  if not FIcon.Empty then hIcon := Icon.Handle;
// Prepare title and message
  Msg := Text;
  Title := Caption;
  if AppTitle <> '' then Title := Title + '#' + AppTitle;
// Show dialog
  Result := Boolean(ShellAbout(GetOwnerHandle, PChar(Title), PChar(Msg), hIcon));
end;


//==============================================================================
// TabfSelectDirDlg
//==============================================================================
// Shows select directory dialog.
// Date: 04/01/2000
{ TabfSelectDirDlg }

constructor TabfSelectDirDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [sdAllowCreate, sdPerformCreate];
end;

//------------------------------------------------------------------------------

function TabfSelectDirDlg.Execute: Boolean;
var
  RegStr: string;
begin
  Result := False;
  RegStr := Directory;
  if not SelectDirectory(RegStr, Options, HelpCtx) then Exit;
  Result := True;
  Directory := RegStr;
end;


//==============================================================================
// TabfBrowseFolderDlg
//==============================================================================
// Shows Windows 'Browse for Folder' dialog.
// Date: 05/28/2001
{ TabfBrowseFolderDlg }

const
  CabfSelectDirDlgCSIDL: array[TabfBrowseFolderDlgRootDir] of Integer = (
    0, CSIDL_BITBUCKET, CSIDL_CONTROLS, CSIDL_DESKTOP, CSIDL_DESKTOPDIRECTORY,
    CSIDL_DRIVES, CSIDL_FONTS, CSIDL_NETHOOD, CSIDL_NETWORK, CSIDL_PERSONAL,
    CSIDL_PRINTERS, CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU,
    CSIDL_STARTUP, CSIDL_TEMPLATES, -1);
  CabfSelectDirDlgBIF: array[TabfBrowseFolderDlgOption] of Integer = (
    BIF_BROWSEFORCOMPUTER, BIF_BROWSEFORPRINTER, BIF_DONTGOBELOWDOMAIN,
    BIF_RETURNFSANCESTORS, BIF_RETURNONLYFSDIRS, BIF_STATUSTEXT,
    BIF_BROWSEINCLUDEFILES, BIF_BROWSEINCLUDEURLS, BIF_EDITBOX,
    BIF_NEWDIALOGSTYLE, BIF_SHAREABLE);

  BTN_CREATE_ID = 255;
  cMaxPathDisplayLength = 50;

//------------------------------------------------------------------------------

function _CreateButtonWndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  Dialog: TabfBrowseFolderDlg;
	S: string;
begin
	if (Msg = WM_COMMAND) and (Lo(wParam) = BTN_CREATE_ID) then
	begin
  	Dialog := TabfBrowseFolderDlg(GetWindowLong(Wnd, GWL_USERDATA));
    S := '';
    if abfInputQuery(SabfBrowseFolderDlg_DlgFolderNameCaption,
      SabfBrowseFolderDlg_DlgFolderNamePrompt, 0, 0, nil, S) then
    begin
      if S <> '' then
      begin
        Dialog.FRestart := True;
        Dialog.SavePosition;
        Dialog.FSelectedFolder := abfAddSlash(Dialog.SelectedFolder) + S;
        ForceDirectories(Dialog.SelectedFolder);
      // Click button to select
        SendMessage(Dialog.FBtnOkHandle, BM_CLICK, 0, 0);
      end;
    end;
		Result := 0;
	end else
    Result := DefDlgProc(Wnd, Msg, wParam, lParam);
end;{function _CreateButtonWndProc}

//------------------------------------------------------------------------------

function _BrowseFolderDlgCallBack(Wnd: HWND; Msg: UINT; Param,
  Data: LPARAM):Integer; stdcall;
var
  Dialog: TabfBrowseFolderDlg;

  //-------------------------------------

  function _EnumChildProc(ChildWnd: HWND; EnumParam: LPARAM): Boolean; stdcall;
  var
    ClsName: array[0..255] of Char;
  begin
    with TabfBrowseFolderDlg(EnumParam) do
    begin
      GetClassName(ChildWnd, ClsName, SizeOf(ClsName));
      if StrIComp(ClsName, 'SYSTREEVIEW32') = 0 then
        FTreeViewHandle := ChildWnd
      else
      if StrIComp(ClsName, 'BUTTON') = 0 then
        if FBtnOKHandle = INVALID_HANDLE_VALUE then FBtnOKHandle := ChildWnd else
        if FBtnCancelHandle = INVALID_HANDLE_VALUE then FBtnCancelHandle := ChildWnd
    end;
    Result := True;
  end;{Internal function _EnumChildProc}

  //-------------------------------------

  procedure _CheckIsFolderValid;
  begin
    with Dialog do
    begin
      FValidPath := abfDirectoryExists(SelectedFolder);
      if (sdoReturnAncest in Options) then
        EnableWindow(FBtnOkHandle, FValidPath)
      else
        EnableWindow(FBtnOkHandle, True);
      if IsWindow(FBtnCreateHandle) then
        EnableWindow(FBtnCreateHandle, FValidPath and CreateButtonEnabled);
    end;
  end;{Internal _CheckIsFolderValid}

  //-------------------------------------

var
	TempPath: array[0..MAX_PATH] of Char;
	Path: string;
	SHFolder: IShellFolder;
  Display: TStrRet;
begin
  Result := 0;
  Dialog := TabfBrowseFolderDlg(Data);
  with Dialog do
    case Msg of

      BFFM_INITIALIZED:
        begin
          FHandle := Wnd;
          UpdatePosition;

        // Change caption and status text if specified
          if Caption <> '' then SetWindowText(Handle, PChar(Caption));
          StatusText := StatusText;

        // Create "CreateButton" id needed
          if OldStyleDialog then
          begin
      			SetWindowLong(Handle, GWL_USERDATA, Data);
            EnumChildWindows(Handle, @_EnumChildProc, Data);
            CreateCreateButton;
           	SetWindowLong(Handle, GWL_WNDPROC, LongInt(@_CreateButtonWndProc));
          end;

        // Set selection
    			if abfDirectoryExists(Folder) then SelectedFolder := Folder;

        // Check the path and enable/disable buttons
          _CheckIsFolderValid;

{$IfNDef C1_ONLY}
        // Fire event if assigned
  		   	if Assigned(OnInitialization) then OnInitialization(Dialog, Wnd);
{$EndIf C1_ONLY}
        end;

      BFFM_SELCHANGED:
        begin

        // Update Item property
          SHGetPathFromIDList(PItemIDList(Param), TempPath);
          FSelectedFolder := StrPas(TempPath);

        // Update staus text, set current path if path should be shown
          if StatusTextAsPath then
          begin
            Path := abfPackPathString(FSelectedFolder, cMaxPathDisplayLength);
            if Path = '' then
              if SHGetDeskTopFolder(SHFolder) = NOERROR then
              begin
                SHFolder.GetDisplayNameOf(PItemIDList(Param), SHGDN_FORPARSING,
                  Display);
                if Display.uType = STRRET_CSTR then Path := StrPas(Display.cStr);
              end;
            StatusText := Path;
          end else
            StatusText := StatusText;

        // Check the path and enable/disable buttons
          _CheckIsFolderValid;

{$IfNDef C1_ONLY}
        // Fire event if assigned
          if Assigned(OnSelectionChanged) then
            OnSelectionChanged(Dialog, Wnd, PItemIDList(Param), SelectedFolder);
{$EndIf C1_ONLY}
        end;{case of BFFM_SELCHANGED}

    end;{case Msg of}
end;{function _BrowseFolderDlgCallBack}

//------------------------------------------------------------------------------

constructor TabfBrowseFolderDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FDisplayName, MAX_PATH);
	SetLength(FFolder, MAX_PATH);
  FFolder := '';
  FCreateButtonVisible := True;
  FCreateButtonEnabled := True;
  FStatusTextAsPath := True;
  Options := [sdoOnlyDirs, sdoShowStatus, sdoNewDialogStyle];
  RootDir := sdrNone;
  ResetPosition;
  ResetHandles;
end;

//------------------------------------------------------------------------------

function TabfBrowseFolderDlg.Execute: Boolean;
var
  iList: PItemIDList;
  BrowseInfo: TBrowseInfo;
	TempPath: array[0..MAX_PATH] of Char;

  //-------------------------------------

  procedure _InitBrowseInfo;
  var
    S: string;
  begin
    S := Text;
    FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
    with BrowseInfo do
    begin
      pszDisplayName := PChar(FDisplayName);
      lpszTitle := PChar(S);
      ulFlags := GetOptions;
      lpfn := @_BrowseFolderDlgCallBack;
      lParam := Integer(Self);
      if IsWindow(OwnerHandle) then
        hwndOwner := OwnerHandle
      else
        hwndOwner := GetOwnerHandle;
      pidlRoot := GetRootID;
    end;
  end;{Internal procedure _InitBrowseInfo}

  //-------------------------------------

begin
  Result := False;

  if abfGetShellVersion < $00040000 then
    raise EabfBrowseFolderDlg.CreateFmt(SabfIncompatibleShell, [ClassName]);

	iList := nil;
	try
    _InitBrowseInfo;

  // Start dialog "loop"
		FSelectedFolder := FFolder;
		repeat
			FRestart := False;
			FFolder := FSelectedFolder;
      try
     	  iList := SHBrowseForFolder(BrowseInfo);
      finally
        DestroyCreateButton; // Destroy a "Create button"
        ResetHandles; // Reset all handles to prevent wrong window catching
      end;
      if FRestart and not (csDesigning in ComponentState) then
        if not _ASK then _TADA;
		until not FRestart;

  // Analyse the result
    if iList <> nil then
    begin
      Result := True;
      SHGetPathFromIDList(iList, TempPath);
      FFolder := StrPas(TempPath);
      FSelectedFolder := FFolder;
			FImageIndex := BrowseInfo.iImage;
    end;

	finally
    ResetPosition; // Drop saved position
		CoTaskMemFree(iList);
		CoTaskMemFree(BrowseInfo.pidlRoot);
	end;
end;{function TabfBrowseFolderDlg.Execute}

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.Loaded;
begin
  inherited Loaded;
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.ResetHandles;
begin
  FHandle          := INVALID_HANDLE_VALUE;
  FTreeViewHandle  := INVALID_HANDLE_VALUE;
  FBtnOKHandle     := INVALID_HANDLE_VALUE;
  FBtnCancelHandle := INVALID_HANDLE_VALUE;
  FBtnCreateHandle := INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.CreateCreateButton;
var
  Left, hFont: Integer;
  R: TRect;
begin
// Check is the button can be created
  if not (IsWindow(Handle) and IsWindow(FTreeViewHandle) and
    IsWindow(FBtnOKHandle)) then Exit;

// Check is button needed or already exists
  if (not OldStyleDialog) or IsWindow(FBtnCreateHandle) then Exit;

// Get left side of TreeView
  GetWindowRect(FTreeViewHandle, R);
  Left := R.Left;

// Get rect of OK button and move it to the left side of the TreeView
  GetWindowRect(FBtnOKHandle, R);
  OffsetRect(R, -(R.Left - Left), 0);
  ScreenToClient(Handle, R.TopLeft);
  ScreenToClient(Handle, R.BottomRight);

// Create button
  with R do
    FBtnCreateHandle := CreateWindow('BUTTON',
      PChar(SabfBrowseFolderDlg_CreateButtonCaption), WS_CHILD or
      WS_CLIPSIBLINGS or WS_VISIBLE or WS_TABSTOP or BS_PUSHBUTTON, Left, Top,
      Right - Left, Bottom - Top, Handle, BTN_CREATE_ID, HInstance, nil);

// Apply OK button font to new button
  hFont := SendMessage(FBtnOKHandle, WM_GETFONT, 0, 0);
  SendMessage(FBtnCreateHandle, WM_SETFONT, hFont, 0);

// Update Enabled and Visible states
  CreateButtonEnabled := CreateButtonEnabled;
  CreateButtonVisible := CreateButtonVisible;

// Change Z-Order of controls inside the dialog
  SetWindowPos(FBtnOkHandle, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOACTIVATE or
    SWP_NOSIZE or SWP_NOMOVE);
  SetWindowPos(FBtnCancelHandle, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOACTIVATE or
    SWP_NOSIZE or SWP_NOMOVE);
end;{procedure TabfBrowseFolderDlg.CreateCreateButton}

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.DestroyCreateButton;
begin
  if IsWindow(FBtnCreateHandle) then DestroyWindow(FBtnCreateHandle);
  FBtnCreateHandle := INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------

function TabfBrowseFolderDlg.GetOptions: Integer;
var
  Opt: TabfBrowseFolderDlgOption;
  ShellVersion: Integer;
begin
  Result := 0;
  ShellVersion := abfGetShellVersion;
  for Opt := Low(Opt) to High(Opt) do
    if Opt in Options then
    begin
    // Skip unsupported options
      if (ShellVersion < $00040071) and
        (Opt in [sdoShowEditBox, sdoIncludeFiles]) then Continue;
      if (ShellVersion < $00050000) and
        (Opt in [sdoIncludeUrls, sdoNewDialogStyle, sdoShareable]) then Continue;
    // Add option flag
      Result := Result or CabfSelectDirDlgBIF[Opt];
    end;
end;

//------------------------------------------------------------------------------

function TabfBrowseFolderDlg.GetRootID: PItemIDList;
begin
  Result := nil;
  if FRootDir = sdrDesktopExpanded then Exit;
  SHGetSpecialFolderLocation(Application.Handle,
    CabfSelectDirDlgCSIDL[FRootDir], Result);
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.ResetPosition;
begin
  FillChar(FPosition, SizeOf(FPosition), 0);
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.SavePosition;
begin
  if IsWindow(Handle) then GetWindowRect(Handle, FPosition);
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.UpdatePosition;
var
  R: TRect;
begin
  if not IsWindow(Handle) then Exit;
  with FPosition do
    if (Top = 0) and (Left = 0) and (Right = 0) and (Bottom = 0) then
    begin
    // If poosition is empty, center the dialog.
      GetWindowRect(Handle, R);
      R := abfAlignRectInRect(R, abfGetWorkAreaRect, haCenter, vaCenter);
    end else
      R := FPosition;
// Set new position only for dilog of old style
  if OldStyleDialog then
    SetWindowPos(Handle, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or
      SWP_NOSIZE or SWP_NOZORDER);
end;

//------------------------------------------------------------------------------

function TabfBrowseFolderDlg.OldStyleDialog: Boolean;
begin
  Result := not ((abfGetShellVersion >= $00050000) and
    (sdoNewDialogStyle in Options));
end;


//==============================================================================
//  Properties Get/Set

procedure TabfBrowseFolderDlg.SetCreateButtonEnabled(A: Boolean);
begin
  FCreateButtonEnabled := A;
  if IsWindow(FBtnCreateHandle) then
    EnableWindow(FBtnCreateHandle, CreateButtonEnabled);
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.SetCreateButtonVisible(A: Boolean);
const
  cFlags: array[Boolean] of DWORD = (SW_HIDE, SW_SHOW);
begin
  FCreateButtonVisible := A;
  if IsWindow(FBtnCreateHandle) then
    ShowWindow(FBtnCreateHandle, cFlags[CreateButtonVisible]);
end;

//------------------------------------------------------------------------------
// For backward capability

function TabfBrowseFolderDlg.GetItem: string;
begin
  Result := Folder;
end;

//------------------------------------------------------------------------------
// For backward capability

procedure TabfBrowseFolderDlg.SetItem(const A: string);
begin
  Folder := A;
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.SetSelectedFolder(const A: string);
begin
  if IsWindow(Handle) then
    SendMessage(Handle, BFFM_SETSELECTION, Integer(LongBool(True)),
      LongInt(PChar(A)));
end;

//------------------------------------------------------------------------------

procedure TabfBrowseFolderDlg.SetStatusText(const A: string);
begin
  FStatusText := A;
  if IsWindow(Handle) then
    SendMessage(Handle, BFFM_SETSTATUSTEXT, 0, LongInt(PChar(FStatusText)));
end;

//------------------------------------------------------------------------------

function TabfBrowseFolderDlg.IsStatusTextStored: Boolean;
begin
  Result := not StatusTextAsPath;
end;

//==============================================================================
// TabfCplDlg
//==============================================================================
// Shows any CPL dialog present in the Control Panel of the system or the
// Control Panel itself. Also you can run own Cpl applets using the TabfCplDlg
// component. Use cplCustom type, CplName property to specify a library file
// and the CplApplet property to specify a number of applet in the library.
// Date: 07/30/2000
{ TabfCplDlg }

const
  CabfCplCmds: array [TabfCplType] of string = (
    '',
    '',
    'access.cpl,,%d',
// 'sysdm.cpl add new hardware' // Windows 9x
    'hdwwiz.cpl,,%d',
    'appwiz.cpl,,%d',
    'admintools',
    'timedate.cpl,,%d',
    'desk.cpl,,%d',
    'fax.cpl,,%d',
    'folders',
    'fonts',
    'joy.cpl,,%d',
    'inetcpl.cpl,,%d',
//    'keyboard', // Windows 95 only
    'main.cpl,keyboard,%d',
    'main.cpl,,%d',
    'modem.cpl', // !!! Don't use PageNumber for modem.cpl !!!
    'mmsys.cpl,,%d',
//    'netcpl.cpl,,%d' // Windows 9x
    'ncpa.cpl,,%d',
    'odbccp32.cpl,,%d',
//    'password.cpl,,%d' // Windows 9x
    'userpasswords',
    'telephon.cpl,,%d',
//    'main.cpl,power,%d',  // Windows 95 only
    'powercfg.cpl,,%d',
    'printers',
    'intl.cpl,,%d',
    'sticpl.cpl,,%d',
    'schedtasks',
    'mmsys.cpl,sounds,%d',
    'sysdm.cpl,,%d',
    'userpasswords'
  );

//------------------------------------------------------------------------------

constructor TabfCplDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCplType := cplDefault;
  FCplApplet := 0;
  FPageNumber := 0;
end;

//------------------------------------------------------------------------------

function TabfCplDlg.Execute: Boolean;
var
  CmdLine: string;
begin
  CmdLine := CabfCplCmds[CplType];

// Make some fix-ups
  case CplType of
    cplCustom: CmdLine := CplName + ',@' + IntToStr(CplApplet) + ',%d';
    cplAddRemoveHardware: begin
      if not IsWinNT then CmdLine := 'sysdm.cpl add new hardware';
    end;
    cplKeyboard: begin
      if IsWin95 or IsWin95OSR2 then CmdLine := 'keyboard';
    end;
    cplNetwork: begin
      if not IsWinNT then CmdLine := 'netcpl.cpl,,%d';
    end;
    cplPasswords: begin
      if not IsWinNT then CmdLine := 'password.cpl,,%d';
    end;
    cplPowerOptions: begin
      if IsWin95 or IsWin95OSR2 then CmdLine := 'main.cpl,power,%d';
    end;
  end;{case CplType of}

// Apply the page number if it is possible
  if Pos('%', CmdLine) > 0 then CmdLine := Format(CmdLine, [PageNumber]);

// Execute
  CmdLine := 'control.exe ' + CmdLine;
  Result := WinExec(PAnsiChar(AnsiString(CmdLine)), SW_ShowNormal) > 31;
end;

//------------------------------------------------------------------------------

procedure TabfCplDlg.SetPageNumber(A: Word);
begin
  if FPageNumber = A then Exit;
  FPageNumber := A;
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
end;


//==============================================================================
// TabfRunDlg
//==============================================================================
// Implementation of the Run dialog.
// 05/28/2001
{ TabfRunDlg }

const
  CabfRunDlgOptionsRFF: array[TabfRunDlgOption] of Integer = (
    RFF_NOBROWSE, RFF_NODEFAULT, RFF_CALCDIRECTORY, RFF_NOLABEL,
    RFF_NOSEPARATEMEM);

//------------------------------------------------------------------------------

constructor TabfRunDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
end;

//------------------------------------------------------------------------------

function TabfRunDlg.Execute: Boolean;
var
{$IFDef D3}
  D, C, T: WideString;
{$Else D3}
  D, C, T: PWideChar;

  procedure _CreateWideStrings(const S: string; var P: PWideChar);
  var
    Len: Integer;
  begin
    P := nil;
    Len := Length(S) * 2;
    if Len <= 0 then Exit;
    Inc(Len, 2);
    P := AllocMem(Len);
    StringToWideChar(S, P, Len);
  end;
{$EndIf D3}

begin
  Result := True;
  if IsWinNT then
  begin // Need convert to WideStrings
{$IfDef D3}
    D := Directory;
    C := Caption;
    T := Text;
    SHRunFileDlg(GetOwnerHandle, Icon.Handle, Pointer(D), Pointer(c),
      Pointer(T), GetOptions);
{$Else D3}
    _CreateWideStrings(Directory, D);
    _CreateWideStrings(Caption, C);
    _CreateWideStrings(Text, T);
    SHRunFileDlg(GetOwnerHandle, Icon.Handle, D, C, T, GetOptions);
    if D <> nil then FreeMem(D);
    if C <> nil then FreeMem(c);
    if T <> nil then FreeMem(T);
{$EndIf D3}
  end else
    SHRunFileDlg(GetOwnerHandle, Icon.Handle, Pointer(Directory),
      Pointer(Caption), Pointer(Text), GetOptions)
end;

//------------------------------------------------------------------------------

function TabfRunDlg.GetOptions: Integer;
var
  Opt: TabfRunDlgOption;
begin
  Result := 0;
  for Opt := Low(Opt) to High(Opt) do
    if Opt in Options then
      Result := Result or CabfRunDlgOptionsRFF[Opt];
  if (rdoNoBrowse in Options) and not (csDesigning in ComponentState) then
    if not _ASK then _TADA;
end;


//==============================================================================
// TabfOpenWithDlg
//==============================================================================
// "Open With..." dialog.
// 05/29/2001
{ TabfOpenWithDlg }

function TabfOpenWithDlg.Execute: Boolean;
var
  CmdLine: string;
begin
  CmdLine :='rundll32.exe shell32.dll,OpenAs_RunDLL ' + FileName;
  Result := WinExec(PAnsiChar(AnsiString(CmdLine)), 0) > 31;
end;


//==============================================================================
// TabfObjectPropertiesDlg
//==============================================================================
// Implementation of "ObjectProperties" dialog.
{ TabfObjectPropertiesDlg }

constructor TabfObjectPropertiesDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileType := pdtFile;
end;

//------------------------------------------------------------------------------

function TabfObjectPropertiesDlg.Execute: Boolean;
const
  cFlags: array[TabfObjectPropertiesDlgType] of Integer = (
    OPF_PATHNAME, OPF_PRINTERNAME);
var
{$IFDef D3}
  AFileName: WideString;
{$Else D3}
  AFileName: PWideChar;

  procedure _CreateWideStrings(const S: string; var P: PWideChar);
  var
    Len: Integer;
  begin
    P := nil;
    Len := Length(S) * 2;
    if Len <= 0 then Exit;
    Inc(Len, 2);
    P := AllocMem(Len);
    StringToWideChar(S, P, Len);
  end;
{$EndIf D3}

begin
  if IsWinNT then
  begin
{$IfDef D3}
    AFileName := FileName;
    Result := SHObjectProperties(GetOwnerHandle, cFlags[FileType],
      PWideChar(AFileName), nil);
{$Else D3}
    _CreateWideStrings(FileName, AFileName);
    Result := SHObjectProperties(GetOwnerHandle, cFlags[FileType],
      AFileName, nil);
    if AFileName <> nil then FreeMem(AFileName);
{$EndIf D3}
  end else
    Result := SHObjectProperties(GetOwnerHandle, cFlags[FileType],
      PWideChar(PChar(FFileName + #0)), nil);
end;


//==============================================================================
// TabfPickIconDlg
//==============================================================================
// Implementation of "Change Icon" dialog.
{ TabfPickIconDlg }

constructor TabfPickIconDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSmallIcon := TIcon.Create;
  FIconIndex := 0;
  FFileName := abfAddSlash(abfGetSystemDir) + SShellDll;
end;

//------------------------------------------------------------------------------

destructor TabfPickIconDlg.Destroy;
begin
  FreeAndNil(FSmallIcon);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfPickIconDlg.Execute: Boolean;
var
  i, Len: DWORD;
{$IFDef D3}
  AFileName: WideString;
{$Else D3}
  AFileName: PWideChar;
{$EndIf D3}
begin
  Result := False;
  i := IconIndex;
  Len := Length(FileName);

  if IsWinNT then
  begin  // Prepare WideChars buffer of MAX_PATH length and place FileName there
{$IfDef D3}
    AFileName := FileName + #0;
    SetLength(AFileName, MAX_PATH * 2 + 2);
    if not SHPickIconDlg(GetOwnerHandle, PWideChar(AFileName), Len, i) then Exit;
    FFileName := PWideChar(AFileName);
{$Else D3}
    AFileName := AllocMem(MAX_PATH * 2 + 2);
    try
      StringToWideChar(FileName, AFileName, MAX_PATH);
      if not SHPickIconDlg(GetOwnerHandle, AFileName, Len, i) then Exit;
      FFileName := WideCharToString(AFileName);
    finally
      FreeMem(AFileName);
    end;
{$EndIf D3}
  end else
  begin
    FFileName := FFileName + #0;
    SetLength(FFileName, MAX_PATH + 1);
    if not SHPickIconDlg(GetOwnerHandle, PWideChar(PChar(FFileName)), Len, i) then Exit;
  end;

// Apply changes
  IconIndex := i;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TabfPickIconDlg.UpdateIcons;
var
  iLarge, iSmall: HICON;
  Result: Integer;
begin
  if (IconIndex > 4) and not (csDesigning in ComponentState) then
    if not _ASK then _TADA;
  LargeIcon.Handle := 0;
  SmallIcon.Handle := 0;
  Result := ExtractIconEx(PChar(FileName), IconIndex, iLarge, iSmall, 1);
  if Result <= 1 then Exit;
  LargeIcon.Handle := iLarge;
  SmallIcon.Handle := iSmall;
end;

//------------------------------------------------------------------------------

procedure TabfPickIconDlg.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  UpdateIcons;
end;

//------------------------------------------------------------------------------

procedure TabfPickIconDlg.SetIconIndex(Value: Integer);
begin
  FIconIndex := Value;
  UpdateIcons;
end;

//------------------------------------------------------------------------------

function TabfPickIconDlg.GetLargeIcon: TIcon;
begin
  Result := Icon;
end;

//------------------------------------------------------------------------------

procedure TabfPickIconDlg.SetLargeIcon(const Value: TIcon);
begin
  Icon := Value;
end;

//------------------------------------------------------------------------------

procedure TabfPickIconDlg.SetSmallIcon(const Value: TIcon);
begin
  FSmallIcon.Assign(Value);
end;

//------------------------------------------------------------------------------
// Don't save dafault FileName, because some system has not C: drive as system.

function TabfPickIconDlg.IsFileNameStored: Boolean;
begin
  Result := (FileName <> (abfAddSlash(abfGetSystemDir) + SShellDll));
end;



//==============================================================================
// TabfFindDlg
//==============================================================================
// Find files/computer dialog.
// 05/29/2001
{ TabfFindDlg }

constructor TabfFindDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKind := fdkFiles;
end;

//------------------------------------------------------------------------------

function TabfFindDlg.Execute: Boolean;
begin
  if Kind = fdkFiles then
    Result := FindFiles
  else
    Result := FindComputer;
end;

//------------------------------------------------------------------------------

function TabfFindDlg.FindComputer: Boolean;
begin
  Result := SHFindComputer(nil, nil);
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
end;

//------------------------------------------------------------------------------
{$HINTS OFF}

function TabfFindDlg.FindFiles: Boolean;
var
  Malloc: IMalloc;
  Info: TShellExecuteInfo;
begin
  Result := False;
  SHGetMalloc(Malloc);
  try
    ZeroMemory(@Info, SizeOf(Info));
    with Info do
    begin
       cbSize      := SizeOf(Info);
       fMask       := SEE_MASK_ICON or SEE_MASK_DOENVSUBST or
         SEE_MASK_FLAG_NO_UI;
       hIcon       := Icon.Handle;
       lpVerb      := 'find';
       lpDirectory := Pointer(Directory);
    end;
    Result := ShellExecuteEx(@Info);
  finally
    Malloc.{$IfDef D3}_Release{$Else}Release{$EndIf};
    Malloc := nil;
  end;
end;

{$HINTS ON}

//==============================================================================
// TabfCreateShortcutDlg
//==============================================================================
// Create shortcut shell dialog.
{ TabfCreateShortcutDlg }

function TabfCreateShortcutDlg.Execute: Boolean;
begin
  Result := WinExec(PAnsiChar(AnsiString('rundll32 appwiz.cpl,NewLinkHere ' +
    abfAddSlash(Directory))), SW_SHOWNORMAL) > 31;{}
end;


//==============================================================================
// TabfAddToFavoritesDlg
//==============================================================================
// Shows "Add to Favorites" dialog
{ TabfAddToFavoritesDlg }

type
  TDoAddToFavDlg = function(Wnd: HWND; Path: PChar; PathLen: Integer;
    Title: PChar; TitleLen: Integer; PIDL: PItemIDList): Integer; stdcall;

//------------------------------------------------------------------------------

function TabfAddToFavoritesDlg.Execute: Boolean;
{var
  ShellUIHelper: IShellUIHelper;
  U: WideString;
  T: OleVariant;
begin
  Result := False;

  ShellUIHelper := CoShellUIHelper.Create;
  try
    U := URL;
    T := Title;
    try
      ShellUIHelper.AddFavorite(U, T);
    except
    end;
    Result := True;
  finally
    ShellUIHelper := nil;
  end;
end;{}
var
  Lib: THandle;
  P, T: string;
  PIDL: PItemIDList;
  DoAddToFavDlg: TDoAddToFavDlg;
begin
  Result := False;
  FFileName := '';

  PIDL := nil;

  Lib := SafeLoadLibrary(SShDocVwDll);
  try
    if Lib <= HINSTANCE_ERROR then Exit;

  // Get function
    DoAddToFavDlg := TDoAddToFavDlg(GetProcAddress(Lib, 'DoAddToFavDlg'));
    if not Assigned(DoAddToFavDlg) then Exit;

  // Prepare parameters
    SHGetSpecialFolderLocation(0, CSIDL_FAVORITES, PIDL);
    T := Title + #0#0;
    SetLength(P, MAX_PATH);
    SetLength(T, MAX_PATH);

  // Show dialog
    if DoAddToFavDlg(GetOwnerHandle, PChar(P), Length(P),
      PChar(T), Length(T), PIDL) = 0 then Exit;

  // Return FileName
    FFileName := PChar(P);

  // Create *.URL file as INI file
    abfIniWriteString(PChar(P), 'InternetShortcut', 'URL', URL);

    Result := True;
  finally
    if Lib <> 0 then FreeLibrary(Lib);
    CoTaskMemFree(PIDL)
  end;
end;


//==============================================================================
// TabfOrganizeFavoritesDlg
//==============================================================================
// Shows "Organize Favorites" dialog
{ TabfOrganizeFavoritesDlg }

type
  TDoOrganizeFavDlg = function(Wnd: HWND; Str: PChar): Integer; stdcall;

//------------------------------------------------------------------------------

function TabfOrganizeFavoritesDlg.Execute: Boolean;
{var
  ShellUIHelper: IShellUIHelper;
  varIn, varOut: OleVariant;
begin
  Result := False;

  ShellUIHelper := CoShellUIHelper.Create;
  try
    try
      ShellUIHelper.ShowBrowserUI('OrganizeFavorites', varIn);
    except
    end;
    Result := True;
  finally
    ShellUIHelper := nil;
  end;
end;{}
var
  Lib: THandle;
  S: string;
  DoOrganizeFavDlg: TDoOrganizeFavDlg;
begin
  Result := False;

  Lib := SafeLoadLibrary(SShDocVwDll);
  try
    if Lib <= HINSTANCE_ERROR then Exit;

  // Get function
    DoOrganizeFavDlg := TDoOrganizeFavDlg(GetProcAddress(Lib, 'DoOrganizeFavDlg'));
    if not Assigned(DoOrganizeFavDlg) then Exit;

    S := abfConcatRelativePath(abfGetFavoritesDir, Folder);

  // Show dialog
    DoOrganizeFavDlg(GetOwnerHandle, PChar(S));

    Result := True;
  finally
    if Lib <> 0 then FreeLibrary(Lib);
  end;
end;

//------------------------------------------------------------------------------

var
  ShellDll: THandle = 0;

{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

  ShellDll := LoadLibrary(SShellDll);
  if ShellDll <> 0 then
  begin
    SHRunFileDlg       := GetProcAddress(ShellDll, PChar(61));
    SHPickIconDlg      := GetProcAddress(ShellDll, PChar(62));
    SHFindFiles        := GetProcAddress(ShellDll, PChar(90));
    SHFindComputer     := GetProcAddress(ShellDll, PChar(91));
    SHObjectProperties := GetProcAddress(ShellDll, PChar(178));
  end;
  CoInitialize(nil);

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfCustomDlg, TabfWinAboutDlg, TabfSelectDirDlg,
    TabfBrowseFolderDlg, TabfCplDlg, TabfRunDlg, TabfOpenWithDlg,
    TabfObjectPropertiesDlg, TabfPickIconDlg, TabfFindDlg,
    TabfCreateShortcutDlg, TabfAddToFavoritesDlg, TabfOrganizeFavoritesDlg]);{}


{******************************************************************************}
finalization
{******************************************************************************}

  if ShellDll <> 0 then FreeLibrary(ShellDll);

end{unit abfDialogs}.


