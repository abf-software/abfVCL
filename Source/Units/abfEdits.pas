{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEdits;

{$I abf.inc}

interface

uses
  Windows, Messages, Graphics, SysUtils, Classes, Controls, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, Forms, Menus, Dialogs,
{$IfDef D3}
  ExtDlgs,
{$EndIf D3}
// ABF VCL
  abfDialogs,
  abfComponents, abfControls, abfEditsConsts;

type

//==============================================================================
// TabfIntegerEdit
//==============================================================================
// Integer value editor. Supports Dec, Hex, Bin, Oct, and even Roman formats.

  TabfNumberFormat = (nfDec, nfHex, nfBin, nfOct, nfRoman);

  TabfIntegerEdit = class(TabfEdit)
  private
    FDigits: Integer;
    FNumberFormat: TabfNumberFormat;
    FMin: Integer;
    FMax: Integer;
    FPrevValue: Integer;
    FPrevText: string;
    FPrevSelStart: Integer;
    FLastChar: Char;
    FChanging: Boolean;
    procedure SetDigits(Value: Integer);
    procedure SetNumberFormat(Value: TabfNumberFormat);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
  // Messages
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  protected
    FValue: Integer;
    FAllowedChars: string;
    procedure CreateWnd; override;
    procedure Change; override;
    procedure UpdateValue; virtual;
  // Properties Get/Set
    procedure SetValue(A: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Digits: Integer read FDigits write SetDigits default 0;
    property NumberFormat: TabfNumberFormat read FNumberFormat
      write SetNumberFormat default nfDec;
    property Min: Integer read FMin write SetMin default Low(Integer);
    property Max: Integer read FMax write SetMax default High(Integer);
    property Value: Integer read FValue write SetValue default 0;
  end;


//==============================================================================
// TabfCustomAdvancedEdit
//==============================================================================
// Prototype of all editors with attached controls

  TabfEditorButtonAlignment = (baRight, baLeft);

  TabfCustomAdvancedEdit = class(TabfCustomEdit)
  private
    function IsButtonWidthStored: Boolean;
  // Messages
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    FButtonControl: TControl;
    FOldButtonControlWndProc: TWndMethod; // Saved ButtonControl's WndProc
    FButtonPanel: TCustomControl;
    FButtonAlignment: TabfEditorButtonAlignment;
    FButtonInside: Boolean;
    FButtonVisible: Boolean;
    FButtonWidth: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure UpdateNonClientArea; override;
    procedure UpdateButton; virtual;
  // ButtonControl hook routines
    procedure HookButtonControl; virtual;
    procedure UnhookButtonControl; virtual;
    procedure ButtonControlWndProc(var Message: TMessage); virtual;
  // Properties Get/Set
    procedure SetButtonAlignment(Value: TabfEditorButtonAlignment); virtual;
    procedure SetButtonInside(Value: Boolean); virtual;
    procedure SetButtonVisible(Value: Boolean); virtual;
    procedure SetButtonWidth(Value: Integer); virtual;
    function GetLeft: Integer; virtual;
    procedure SetLeft(Value: Integer); virtual;
    function GetWidth: Integer; virtual;
    procedure SetWidth(Value: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  // Properties
    property ButtonControl: TControl read FButtonControl;
  published
  // Properties, don't change the order of properties !!!
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth
      stored IsButtonWidthStored;
    property Left: Integer read GetLeft write SetLeft;
    property Width: Integer read GetWidth write SetWidth;
    property ButtonVisible: Boolean read FButtonVisible
      write SetButtonVisible default True;
    property ButtonInside: Boolean read FButtonInside write
      SetButtonInside default True;
    property ButtonAlignment: TabfEditorButtonAlignment read FButtonAlignment
      write SetButtonAlignment default baRight;
  end;


//==============================================================================
// TabfAdvancedEdit
//==============================================================================
// Edit that can have a button or other control is attached

  TabfAdvancedEdit = class(TabfCustomAdvancedEdit)
  protected
    FButtonControlParent: TWinControl;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    procedure WriteState(Writer: TWriter); override;
  // Properties Get/Set
    procedure SetButtonControl(const Value: TControl); virtual;
  published
    property About;
    property ButtonControl: TControl read FButtonControl write SetButtonControl;
  // TabfEdit parts
    property Alignment;
    property Flat;
    property FocusBorder;
  // TEdit parts
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IfDef D3}
    property DragMode;
    property ImeMode;
    property ImeName;
    property OnEndDrag;
    property OnStartDrag;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
{$IfDef D6}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
{$EndIf D6}
  end;


//==============================================================================
// TabfCustomButtonEdit
//==============================================================================
// Prototype of editor with attached SpeedButton

  TabfCustomButtonEdit = class(TabfCustomAdvancedEdit)
  private
{$IfDef D4}
    function GetButtonAction: TBasicAction;
    procedure SetButtonAction(const Value: TBasicAction);
{$EndIf D4}
    function GetButtonFlat: Boolean;
    procedure SetButtonFlat(Value: Boolean);
    function GetButtonFont: TFont;
    procedure SetButtonFont(const Value: TFont);
    function GetButtonGlyph: TBitmap;
    procedure SetButtonGlyph(const Value: TBitmap);
    function GetButtonLayout: TButtonLayout;
    procedure SetButtonLayout(Value: TButtonLayout);
    function GetButtonMargin: Integer;
    procedure SetButtonMargin(Value: Integer);
    function GetButtonNumGlyphs: TNumGlyphs;
    procedure SetButtonNumGlyphs(Value: TNumGlyphs);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
    function GetButtonPopupMenu: TPopupMenu;
    procedure SetButtonPopupMenu(const Value: TPopupMenu);
    function GetButtonShowHint: Boolean;
    procedure SetButtonShowHint(Value: Boolean);
    function GetButtonSpacing: Integer;
    procedure SetButtonSpacing(Value: Integer);
    function GetButtonTransparent: Boolean;
    procedure SetButtonTransparent(Value: Boolean);
    function GetOnButtonClick: TNotifyEvent;
    procedure SetOnButtonClick(Value: TNotifyEvent);
    function GetOnButtonDblClick: TNotifyEvent;
    procedure SetOnButtonDblClick(Value: TNotifyEvent);
    function GetOnButtonMouseDown: TMouseEvent;
    procedure SetOnButtonMouseDown(Value: TMouseEvent);
    function GetOnButtonMouseMove: TMouseMoveEvent;
    procedure SetOnButtonMouseMove(Value: TMouseMoveEvent);
    function GetOnButtonMouseUp: TMouseEvent;
    procedure SetOnButtonMouseUp(Value: TMouseEvent);
  protected
    function GetButton: TSpeedButton; virtual;
  // Events
    property OnButtonClick: TNotifyEvent read GetOnButtonClick
      write SetOnButtonClick;
    property OnButtonDblClick: TNotifyEvent read GetOnButtonDblClick
      write SetOnButtonDblClick;
    property OnButtonMouseDown: TMouseEvent read GetOnButtonMouseDown
      write SetOnButtonMouseDown;
    property OnButtonMouseMove: TMouseMoveEvent read GetOnButtonMouseMove
      write SetOnButtonMouseMove;
    property OnButtonMouseUp: TMouseEvent read GetOnButtonMouseUp
      write SetOnButtonMouseUp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property Button: TSpeedButton read GetButton;
    // SpeedButton wrappers
{$IfDef D4}
    property ButtonAction: TBasicAction read GetButtonAction
      write SetButtonAction;
{$EndIf D4}      
    property ButtonFlat: Boolean read GetButtonFlat write SetButtonFlat
      default False;
    property ButtonFont: TFont read GetButtonFont write SetButtonFont;
    property ButtonGlyph: TBitmap read GetButtonGlyph write SetButtonGlyph;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonLayout: TButtonLayout read GetButtonLayout
      write SetButtonLayout default blGlyphLeft;
    property ButtonMargin: Integer read GetButtonMargin write SetButtonMargin
      default -1;
    property ButtonNumGlyphs: TNumGlyphs read GetButtonNumGlyphs
      write SetButtonNumGlyphs default 1;
    property ButtonPopupMenu: TPopupMenu read GetButtonPopupMenu
      write SetButtonPopupMenu;
    property ButtonShowHint: Boolean read GetButtonShowHint
      write SetButtonShowHint  default False;
    property ButtonSpacing: Integer read GetButtonSpacing write SetButtonSpacing
      default 4;
    property ButtonTransparent: Boolean read GetButtonTransparent
      write SetButtonTransparent default False;
  end;


//==============================================================================
// TabfButtonEdit
//==============================================================================
// Editor with attached SpeedButton

  TabfButtonEdit = class(TabfCustomButtonEdit)
  published
    property About;
{$IfDef D4}
    property ButtonAction;
{$EndIf D4}
    property ButtonFlat;
    property ButtonFont;
    property ButtonGlyph;
    property ButtonHint;
    property ButtonLayout;
    property ButtonMargin;
    property ButtonNumGlyphs;
    property ButtonPopupMenu;
    property ButtonShowHint;
    property ButtonSpacing;
    property ButtonTransparent;
    property OnButtonClick;
    property OnButtonDblClick;
    property OnButtonMouseDown;
    property OnButtonMouseMove;
    property OnButtonMouseUp;
  // TabfEdit parts
    property Alignment;
    property Flat;
    property FocusBorder;
  // TEdit parts
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IfDef D3}
    property DragMode;
    property ImeMode;
    property ImeName;
    property OnEndDrag;
    property OnStartDrag;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
{$IfDef D6}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
{$EndIf D6}
  end;


//==============================================================================
// TabfUpDownEdit
//==============================================================================
// UpDown (spin) editor

  TabfUpDownEdit = class(TabfCustomAdvancedEdit)
  private
    FIsHex: Boolean;
    FUpDownChanging: TUDChangingEvent;
    FUpdating, FInitialazed: Boolean;
  // Properties Get/Set
    procedure SetIsHex(Value: Boolean);
    function GetIncrement: Integer;
    procedure SetIncrement(Value: Integer);
    function GetMin: SmallInt;
    procedure SetMin(Value: SmallInt);
    function GetMax: SmallInt;
    procedure SetMax(Value: SmallInt);
    function GetOrientation: TUDOrientation;
    procedure SetOrientation(Value: TUDOrientation);
    function GetPosition: SmallInt;
    procedure SetPosition(Value: SmallInt);
    function GetThousands: Boolean;
    procedure SetThousands(Value: Boolean);
{$IfDef D5}
    function GetOnUpDownChangingEx: TUDChangingEventEx;
    procedure SetOnUpDownChangingEx(Value: TUDChangingEventEx);
{$EndIf D5}
    function GetOnUpDownClick: TUDClickEvent;
    procedure SetOnUpDownClick(Value: TUDClickEvent);
  // Messages
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure UpdateButton; override;
  // Events
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;
    procedure UpDownChanging(Sender: TObject;
      var AllowChange: Boolean); virtual;
  // Properties Get/Set
    function GetUpDown: TUpDown; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property UpDown: TUpDown read GetUpDown;
  published
    property ButtonInside default False;
    property IsHex: Boolean read FIsHex write SetIsHex default False;
    property Increment: Integer read GetIncrement write SetIncrement default 1;
    property Min: SmallInt read GetMin write SetMin;
    property Max: SmallInt read GetMax write SetMax;
    property Orientation: TUDOrientation read GetOrientation
      write SetOrientation default udVertical;
    property Position: SmallInt read GetPosition write SetPosition;
    property Thousands: Boolean read GetThousands write SetThousands
     default True;
    property OnUpDownChanging: TUDChangingEvent read FUpDownChanging
      write FUpDownChanging;
{$IfDef D5}
    property OnUpDownChangingEx: TUDChangingEventEx read GetOnUpDownChangingEx
      write SetOnUpDownChangingEx;
{$EndIf D5}
    property OnUpDownClick: TUDClickEvent read GetOnUpDownClick
      write SetOnUpDownClick;
  // TabfEdit parts
    property Alignment;
    property Flat;
    property FocusBorder;
  // TEdit parts
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IfDef D3}
    property DragMode;
    property ImeMode;
    property ImeName;
    property OnEndDrag;
    property OnStartDrag;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
{$IfDef D6}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
{$EndIf D6}
  end;


//==============================================================================
// TabfFileNameEdit
//==============================================================================
// File name editor with button that opens an associated dialog

  TabfFileDialogKind = (dkOpen, dkSave, dkOpenPicture, dkSavePicture);

  TabfFileNameEdit = class(TabfCustomButtonEdit)
  private
    FDialog: TOpenDialog;
    FDialogKind: TabfFileDialogKind;
    FDialogDefaultExt: string;
    FDialogFilter: string;
    FDialogFilterIndex: Integer;
    FDialogInitialDir: string;
    FDialogHelpContext: THelpContext;
    FDialogOptions: TOpenOptions;
    FDialogTitle: string;
    FOnDialogClose: TNotifyEvent;
    FOnDialogCanClose: TCloseQueryEvent;
    FOnDialogShow: TNotifyEvent;
    FOnDialogTypeChange: TNotifyEvent;
    FOnDialogFolderChange: TNotifyEvent;
    FOnDialogSelectionChange: TNotifyEvent;
    FOnButtonClick: TNotifyEvent;
{$IfDef D5}
    FOnDialogIncludeItem: TIncludeItemEvent;
{$EndIf D5}
{$IfDef D6}
    FDialogOptionsEx: TOpenOptionsEx;
{$EndIf D6}
  // Properties Get/Set
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure OnButtonClickEvent(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowDialog: Boolean; virtual;
  published
    property About;
    property Dialog: TOpenDialog read FDialog write FDialog;
    property DialogDefaultExt: string read FDialogDefaultExt
      write FDialogDefaultExt;
    property FileName: TFileName read GetFileName write SetFileName;
    property DialogFilter: string read FDialogFilter write FDialogFilter;
    property DialogFilterIndex: Integer read FDialogFilterIndex
      write FDialogFilterIndex default 1;
    property DialogInitialDir: string read FDialogInitialDir
      write FDialogInitialDir;
    property DialogHelpContext: THelpContext read FDialogHelpContext
      write FDialogHelpContext default 0;
    property DialogKind: TabfFileDialogKind read FDialogKind write FDialogKind
      default dkOpen;
    property DialogOptions: TOpenOptions read FDialogOptions
      write FDialogOptions default [ofHideReadOnly{$IfDef D4}, ofEnableSizing{$EndIf}];
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property OnDialogClose: TNotifyEvent read FOnDialogClose
      write FOnDialogClose;
    property OnDialogCanClose: TCloseQueryEvent read FOnDialogCanClose
      write FOnDialogCanClose;
    property OnDialogFolderChange: TNotifyEvent read FOnDialogFolderChange
      write FOnDialogFolderChange;
    property OnDialogSelectionChange: TNotifyEvent
      read FOnDialogSelectionChange write FOnDialogSelectionChange;
    property OnDialogShow: TNotifyEvent read FOnDialogShow write FOnDialogShow;
    property OnDialogTypeChange: TNotifyEvent read FOnDialogTypeChange
      write FOnDialogTypeChange;
{$IfDef D5}
    property OnDialogIncludeItem: TIncludeItemEvent read FOnDialogIncludeItem
      write FOnDialogIncludeItem;
{$EndIf D5}
    property OnButtonClick: TNotifyEvent read FOnButtonClick
      write FOnButtonClick;
{$IfDef D6}
    property DialogOptionsEx: TOpenOptionsEx read FDialogOptionsEx
      write FDialogOptionsEx default [];
{$EndIf D6}
  // TabfButtonEdit parts
    property ButtonFlat;
    property ButtonFont;
    property ButtonGlyph;
    property ButtonHint;
    property ButtonLayout;
    property ButtonMargin;
    property ButtonNumGlyphs;
    property ButtonPopupMenu;
    property ButtonShowHint;
    property ButtonSpacing;
    property ButtonTransparent;
    property OnButtonDblClick;
    property OnButtonMouseDown;
    property OnButtonMouseMove;
    property OnButtonMouseUp;
  // TabfEdit parts
    property Alignment;
    property Flat;
    property FocusBorder;
  // TEdit parts
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IfDef D3}
    property DragMode;
    property ImeMode;
    property ImeName;
    property OnEndDrag;
    property OnStartDrag;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
{$IfDef D6}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
{$EndIf D6}
  end;


//==============================================================================
// TabfDirectoryNameEdit
//==============================================================================
// Directory name editor with button that opens an associated dialog

  TabfDirectoryNameEdit = class(TabfCustomButtonEdit)
  private
    FDialogCreateButtonEnabled: Boolean;
    FDialogStatusTextAsPath: Boolean;
    FDialogCreateButtonVisible: Boolean;
    FDialogStatusText: string;
    FDialogCaption: string;
    FDialogText: string;
    FDialog: TabfBrowseFolderDlg;
    FDialogOptions: TabfBrowseFolderDlgOptions;
    FDialogRootDir: TabfBrowseFolderDlgRootDir;
    FOnDialogSelectionChanged: TabfBFDlgChangedEvent;
    FOnDialogInitialization: TabfBFDlgInitEvent;
    FOnButtonClick: TNotifyEvent;
  // Properties Get/Set
    function GetDirectory: string;
    procedure SetDirectory(const Value: string);
    function IsDialogStatusTextStored: Boolean;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure OnButtonClickEvent(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowDialog: Boolean; virtual;
  published
    property About;
    property Directory: string read GetDirectory write SetDirectory;
    property DialogCaption: string read FDialogCaption write FDialogCaption;
    property DialogCreateButtonVisible: Boolean read FDialogCreateButtonVisible
      write FDialogCreateButtonVisible default True;
    property DialogCreateButtonEnabled: Boolean read FDialogCreateButtonEnabled
      write FDialogCreateButtonEnabled default True;
    property DialogText: string read FDialogText write FDialogText;
    property DialogStatusText: string read FDialogStatusText
      write FDialogStatusText stored IsDialogStatusTextStored;
    property DialogStatusTextAsPath: Boolean read FDialogStatusTextAsPath
      write FDialogStatusTextAsPath default True;
    property Dialog: TabfBrowseFolderDlg read FDialog write FDialog;
    property DialogOptions: TabfBrowseFolderDlgOptions read FDialogOptions
      write FDialogOptions
      default [sdoOnlyDirs, sdoShowStatus, sdoNewDialogStyle];
    property DialogRootDir: TabfBrowseFolderDlgRootDir read FDialogRootDir
      write FDialogRootDir default sdrNone;
  // Events
  {$IfNDef C1_ONLY}
    property OnDialogInitialization: TabfBFDlgInitEvent
      read FOnDialogInitialization  write FOnDialogInitialization;
    property OnDialogSelectionChanged: TabfBFDlgChangedEvent
      read FOnDialogSelectionChanged write FOnDialogSelectionChanged;
  {$EndIf C1_ONLY}
    property OnButtonClick: TNotifyEvent read FOnButtonClick
      write FOnButtonClick;
  // TabfButtonEdit parts
    property ButtonFlat;
    property ButtonFont;
    property ButtonGlyph;
    property ButtonHint;
    property ButtonLayout;
    property ButtonMargin;
    property ButtonNumGlyphs;
    property ButtonPopupMenu;
    property ButtonShowHint;
    property ButtonSpacing;
    property ButtonTransparent;
    property OnButtonDblClick;
    property OnButtonMouseDown;
    property OnButtonMouseMove;
    property OnButtonMouseUp;
  // TabfEdit parts
    property Alignment;
    property Flat;
    property FocusBorder;
  // TEdit parts
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IfDef D3}
    property DragMode;
    property ImeMode;
    property ImeName;
    property OnEndDrag;
    property OnStartDrag;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
{$IfDef D6}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
{$EndIf D6}
  end;


//==============================================================================
// Default glyphs. Loaded from resources in the design time package

var
  ABF_GLYPH_FILE: TBitmap;
  ABF_GLYPH_FOLDER: TBitmap;

{******************************************************************************}
implementation
{******************************************************************************}
uses
  FileCtrl, CommCtrl,
// ABF VCL
  abfSysUtils, abfTypInfo;

{$I abf_init.inc}

type
  THackControl = class(TControl);
  THackWinControl = class(TWinControl);
  TabfHackCustomEdit = class(TabfCustomEdit);

//==============================================================================
// TabfEditButtonPanel
//==============================================================================
// Button container control

  TabfEditButtonPanel = class(TCustomControl)
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  end;

//------------------------------------------------------------------------------
{ TabfEditButtonPanel }

procedure TabfEditButtonPanel.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  Message.Result := HTTRANSPARENT;
end;


//==============================================================================
// TabfIntegerEdit
//==============================================================================
// Integer value editor. Supports Dec, Hex, Bin, Oct, and even Roman formats.
{ TabfIntegerEdit }

constructor TabfIntegerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];

  if not (csDesigning in ComponentState) then _GlobalInit;

  FDigits := 0;
  FNumberFormat := nfDec;
  FMin := Low(Integer);
  FMax := High(Integer);
  FValue := 0;

  FPrevText := '';
  FPrevValue := FValue;
end;

//------------------------------------------------------------------------------

procedure TabfIntegerEdit.CreateWnd;
begin
  inherited CreateWnd;
  UpdateValue;
end;

//------------------------------------------------------------------------------

procedure TabfIntegerEdit.Change;
var
  i: Integer;
  S: string;
begin
  if ([csLoading, csReading, csDestroying] * ComponentState <> []) then Exit;
  if not HandleAllocated then Exit;

  if FChanging then Exit;
  FChanging := True;
  try

  // Remove leading zeros
    if (Digits < 1) and (Pos(FLastChar, FAllowedChars) > 0) and
      ((Length(Text) > 1) and (Text[1] = '0') or
        (Length(Text) > 2) and (Text[1] = '-') and (Text[2] = '0')) then
    begin
      S := Text;
      if Text[1] = '-' then i := 2 else i := 1;
      while (Length(S) > i) and (S[i] = '0') do Delete(S, i, 1);
      Text := S;

    // Move selection to the end of text
      if FLastChar <> #0 then
      begin
        SelStart := Length(S);
        SelLength := 0;
      end;
    end;

  // Get value
    if Text = '' then FValue := 0 else
    try
      case NumberFormat of
        nfDec: begin
          if Text <> '-' then
            FValue := StrToInt(Text)
          else
            FValue := 0;
        end;
        nfHex: FValue := StrToInt('$' + Text);
        nfBin: FValue := abfBinToInt(Text);
        nfOct: FValue := abfOctToInt(Text);
        nfRoman: FValue := abfRomanToInt(Text);
      end;
    except
      Text := FPrevText;
      FValue := FPrevValue;
      SelStart := FPrevSelStart;
    end;

    FPrevText := Text;
    FPrevValue := Value;

  finally
    inherited Change;
    FChanging := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIntegerEdit.UpdateValue;
begin
//  if FChanging then Exit;
//  if not HandleAllocated then Exit;

  case NumberFormat of
    nfDec: begin
      FAllowedChars := '0123456789';
      if (Min < 0) or (Max < 0) then FAllowedChars := FAllowedChars + '-';
      Text := IntToStr(Value);
    end;
    nfHex: begin
      FAllowedChars := '0123456789abcdefABCDEF';
      Text := IntToHex(Value, Digits);
    end;
    nfBin: begin
      FAllowedChars := '01';
      Text := abfIntToBin(Value, Digits);
    end;
    nfOct: begin
      FAllowedChars := '01234567';
      Text := abfIntToOct(Value, Digits);
    end;
    nfRoman: begin
      FAllowedChars := 'IVXCLXMivxclxm';
      Text := abfIntToRoman(Value);
    end;
  end;
end;


//==============================================================================
// Properties Get/Set

procedure TabfIntegerEdit.SetValue(A: Integer);
var
  AMin: Integer;
begin
  AMin := Min;
  if NumberFormat in [nfBin, nfHex, nfOct] then AMin := 0;
  if A < AMin then A := AMin;
  if A > Max then A := Max;
  FValue := A;
  UpdateValue;
end;

//------------------------------------------------------------------------------

procedure TabfIntegerEdit.SetDigits(Value: Integer);
begin
  if Digits = Value then Exit;
  if Value < 0 then Value := 0;
  FDigits := Value;
  UpdateValue;
end;

//------------------------------------------------------------------------------

procedure TabfIntegerEdit.SetNumberFormat(Value: TabfNumberFormat);
begin
  if NumberFormat = Value then Exit;
  FNumberFormat := Value;
  UpdateValue;
end;

//------------------------------------------------------------------------------

procedure TabfIntegerEdit.SetMin(Value: Integer);
begin
  if Min = Value then Exit;
  FMin := Value;
  UpdateValue;
end;

//------------------------------------------------------------------------------

procedure TabfIntegerEdit.SetMax(Value: Integer);
begin
  if Max = Value then Exit;
  FMax := Value;
  UpdateValue;
end;


//==============================================================================
// Messages

procedure TabfIntegerEdit.WMChar(var Message: TWMChar);
begin
  FPrevText     := Text;
  FPrevValue    := Value;
  FPrevSelStart := SelStart;
  FLastChar := #0;

  if not (Chr(Message.CharCode) in [#0..#31]) then
  begin
    if Pos(Chr(Message.CharCode), FAllowedChars) < 1 then Exit;
    FLastChar := Chr(Message.CharCode);
  end;

  inherited;
end;


//==============================================================================
// TabfCustomAdvancedEdit
//==============================================================================
// Prototype of all editors with attached controls
{ TabfCustomAdvancedEdit }

constructor TabfCustomAdvancedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;

  FButtonPanel := TabfEditButtonPanel.Create(Self);
  THackWinControl(FButtonPanel).Color := clBtnFace;
  FButtonPanel.Visible := True;
  FButtonPanel.Parent := Self;

  ButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FButtonAlignment := baRight;
  FButtonInside := True;
  FButtonVisible := True;
end;

//------------------------------------------------------------------------------

destructor TabfCustomAdvancedEdit.Destroy;
begin
  inherited Destroy;
//  FreeAndNil(FButtonPanel);
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateButton;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
// Must be MultiLine to support a formatting rectangle when button is inside
  Params.Style := Params.Style or ES_MULTILINE;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then _GlobalInit;
  UpdateButton;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  UpdateButton;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.Loaded;
begin
  inherited Loaded;
  UpdateButton;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.UpdateNonClientArea;
begin
  inherited UpdateNonClientArea;

  if not (Flat and Assigned(ButtonControl) and ButtonInside and
    ButtonVisible) then Exit;

  ButtonControl.Repaint;
end;

//------------------------------------------------------------------------------
// Updates position and size of ButtonControl

procedure TabfCustomAdvancedEdit.UpdateButton;
var
  L: Integer;
  R: TRect;
begin
  if ComponentState * [csLoading, csReading, csDestroying] <> [] then Exit;

  if not HandleAllocated then Exit;
  if not (Assigned(Parent) and Parent.HandleAllocated) then Exit;

// Set button's properties
  if Assigned(ButtonControl) then
  begin
    ButtonControl.Visible := ButtonVisible and Visible;
    ButtonControl.Enabled := Enabled;
  end;
  FButtonPanel.Visible := ButtonInside and ButtonVisible;

  if ButtonInside then
  begin
  // Parent is FButtonPanel, ButtonControl is inside the editor
    if ButtonAlignment = baRight then L := ClientWidth - ButtonWidth
    else L := 0;
    FButtonPanel.SetBounds(L, 0, ButtonWidth, ClientHeight);

  // Resize buttonControl to fit all area of FButtonPanel
    if Assigned(ButtonControl) then
    begin
      ButtonControl.Parent := FButtonPanel;
      ButtonControl.SetBounds(0, 0, FButtonPanel.Width, FButtonPanel.Height);
    end;

  // Set formatting rectangle
    if ButtonVisible then
    begin
      if ButtonAlignment = baRight then
        R := Rect(3, 0, ClientWidth - ButtonWidth - 2, ClientHeight + 1)
      else
        R := Rect(ButtonWidth + 2, 0, ClientWidth - 3, ClientHeight + 1);
{$IFDEF WIN32}
      SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R))
{$ELSE}
      SendMessage(Handle, EM_SETRECTNP, 0, NativeInt(@R))
{$ENDIF}
    end else
      SendMessage(Handle, EM_SETRECTNP, 0, 0);

  end else
  if ButtonVisible then
  begin
  // Parent is same, ButtonControl is near the editor
    if ButtonAlignment = baRight then L := Left + Width - ButtonWidth
    else L := Left;

    if Assigned(ButtonControl) then
    begin
      ButtonControl.Parent := Parent;
      ButtonControl.SetBounds(L, Top, ButtonWidth, Height);
    end;

  // Reset formatting rectangle
    SendMessage(Handle, EM_SETRECTNP, 0, 0);
  end;

// Set visibility of FButtonPanel and ButtonContol at design time
  if not (csDesigning in ComponentState) then Exit;

  with THackWinControl(FButtonPanel) do
  begin
    if ButtonVisible then
      ControlStyle := ControlStyle - [csNoDesignVisible]
    else
      ControlStyle := ControlStyle + [csNoDesignVisible];
    RecreateWnd;
  end;

  if Assigned(ButtonControl) then
    with THackControl(ButtonControl) do
    begin
      if ButtonVisible then
        ControlStyle := ControlStyle - [csNoDesignVisible]
      else
        ControlStyle := ControlStyle + [csNoDesignVisible];
    // Apply ControlStyle changes
      if ButtonControl is TWinControl then
        THackWinControl(ButtonControl).RecreateWnd
      else
        Update;
    end;

end;{procedure TabfCustomAdvancedEdit.UpdateButton}


//==============================================================================
// ButtonControl hook routines

procedure TabfCustomAdvancedEdit.HookButtonControl;
begin
  if (not Assigned(ButtonControl)) or Assigned(FOldButtonControlWndProc) then
    Exit;
{$IfDef D3}
  FOldButtonControlWndProc := ButtonControl.WindowProc;
  ButtonControl.WindowProc := ButtonControlWndProc;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.UnhookButtonControl;
begin
  if not (Assigned(ButtonControl) and Assigned(FOldButtonControlWndProc)) then
    Exit;
{$IfDef D3}
  ButtonControl.WindowProc := FOldButtonControlWndProc;
{$EndIf D3}
  FOldButtonControlWndProc := nil; // Indication that ButtonControl is unhooked
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.ButtonControlWndProc(var Message: TMessage);
begin
  if not (Assigned(ButtonControl) and Assigned(FOldButtonControlWndProc)) then
    Exit;

  FOldButtonControlWndProc(Message);

  case Message.Msg of
    WM_NCHITTEST: begin
      FMouseInControl := True;
      UpdateNonClientArea;
    end;
    CM_ENTER, CM_EXIT, CM_MOUSEENTER, CM_MOUSELEAVE: begin
       PostMessage(Handle, Message.Msg, Message.WParam, Message.LParam);
    end;
//    WM_KILLFOCUS, WM_SETFOCUS:
  end;

end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomAdvancedEdit.SetButtonAlignment(
  Value: TabfEditorButtonAlignment);
var
  L: Integer;
begin
  if ButtonAlignment = Value then Exit;
  L := Left;
  FButtonAlignment := Value;
  Left := L;
  UpdateButton;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.SetButtonInside(Value: Boolean);
var
  L, W: Integer;
begin
  if ButtonInside = Value then Exit;
  L := Left;
  W := Width;
  FButtonInside := Value;
  Left := L;
  Width := W;
  UpdateButton;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.SetButtonVisible(Value: Boolean);
var
  L, W: Integer;
begin
  if ButtonVisible = Value then Exit;
  L := Left;
  W := Width;
  FButtonVisible := Value;
  Left := L;
  Width := W;
  UpdateButton;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.SetButtonWidth(Value: Integer);
var
  L, W: Integer;
begin
  if ButtonWidth = Value then Exit;
  L := Left;
  W := Width;
  FButtonWidth := Value;
  Left := L;
  Width := W;
  UpdateButton;
  Repaint;
end;

//------------------------------------------------------------------------------

function TabfCustomAdvancedEdit.IsButtonWidthStored: Boolean;
begin
  Result := ButtonWidth <> GetSystemMetrics(SM_CXVSCROLL);
end;

//------------------------------------------------------------------------------

function TabfCustomAdvancedEdit.GetLeft: Integer;
begin
  Result := inherited Left;
  if {(ComponentState * [csLoading, csReading] = []) and}
    (not ButtonInside) and ButtonVisible and (ButtonAlignment = baLeft) then
    Result := Result - ButtonWidth;{}
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.SetLeft(Value: Integer);
begin
  if {(ComponentState * [csLoading, csReading] = []) and}
    (not ButtonInside) and ButtonVisible and (ButtonAlignment = baLeft) then
    Value := Value + ButtonWidth;
  inherited Left := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomAdvancedEdit.GetWidth: Integer;
begin
  Result := inherited Width;
  if {(ComponentState * [csLoading, csReading] = []) and} (not ButtonInside) and
    ButtonVisible then
    Result := Result + ButtonWidth;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.SetWidth(Value: Integer);
begin
  if {(ComponentState * [csLoading, csReading] = []) and} (not ButtonInside) and
    ButtonVisible then
    Value := Value - ButtonWidth;
  inherited Width := Value;
end;


//==============================================================================
// Messages

procedure TabfCustomAdvancedEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateButton;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  UpdateButton;
end;

//------------------------------------------------------------------------------

procedure TabfCustomAdvancedEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateButton;
end;


//==============================================================================
// TabfAdvancedEdit
//==============================================================================
// Edit that can have a button or other control is attached
{ TabfAdvancedEdit }

procedure TabfAdvancedEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = ButtonControl then
    begin
      UnhookButtonControl;
      FButtonControl := nil;
      UpdateButton;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAdvancedEdit.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);

// Read ButtonControl manually if it is inside the ButtonPanel
  if ButtonInside and Assigned(ButtonControl) then
    Reader.ReadComponent(ButtonControl);
end;

//------------------------------------------------------------------------------

procedure TabfAdvancedEdit.WriteState(Writer: TWriter);
begin
  inherited WriteState(Writer);

// Write ButtonControl manually if it is inside the ButtonPanel
  if ButtonInside and Assigned(ButtonControl) then
    Writer.WriteComponent(ButtonControl);
end;


//==============================================================================
// Properties Get/Set

procedure TabfAdvancedEdit.SetButtonControl(const Value: TControl);
var
  i: Integer;
begin
  if (ButtonControl = Value) or (Value = Self) then Exit;

// Validate incoming value
  if Assigned(Value) then
  begin
  // Disallow inserting of TabfAdvancedEdit
    if (Value is TabfAdvancedEdit) then Exit;

  // Check isn't this control already a ButtonControl of other abfButtonEdit
    if Assigned(Owner) then
      for i := 0 to Owner.ComponentCount - 1 do
        if Owner.Components[i] is TabfAdvancedEdit then
          if TabfAdvancedEdit(Owner.Components[i]).ButtonControl = Value then
          begin
            if csDesigning in ComponentState then
              raise EabfEdit.CreateFmt(SabfButtonEdit_ControlIsAlreadyUsed,
                [Value.Name, Owner.Components[i].Name]);
            Exit;
          end;
  end;

// Update internal flag  
  FButtonVisible := ButtonVisible;

// Remove previous control
  if Assigned(ButtonControl) then
    ButtonControl.Parent := FButtonControlParent;
  UnhookButtonControl;

// Insert new control and save its parent
  FButtonControl := Value;
  if Assigned(FButtonControl) then
  begin
    FButtonControlParent := FButtonControl.Parent;
    ButtonVisible := FButtonVisible;
  end;
  HookButtonControl;

// Resize new control
  UpdateButton;
end;


//==============================================================================
// TabfCustomButtonEdit
//==============================================================================
// Prototype of editor with attached SpeedButton
{ TabfCustomButtonEdit }

constructor TabfCustomButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonControl := TSpeedButton.Create(Self);
  ButtonTransparent := False;

  HookButtonControl;
end;

//------------------------------------------------------------------------------

destructor TabfCustomButtonEdit.Destroy;
begin
  UnhookButtonControl;
  inherited Destroy;
end;


//==============================================================================
// Properties Get/Set

function TabfCustomButtonEdit.GetButton: TSpeedButton;
begin
  Result := TSpeedButton(ButtonControl);
end;

//------------------------------------------------------------------------------
{$IfDef D4}

function TabfCustomButtonEdit.GetButtonAction: TBasicAction;
begin
  Result := Button.Action;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonAction(const Value: TBasicAction);
begin
  Button.Action := Value;
end;

{$EndIf D4}
//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonFlat: Boolean;
begin
{$IfDef D3}
  Result := Button.Flat;
{$Else}
  Result := False;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonFlat(Value: Boolean);
begin
{$IfDef D3}
  Button.Flat := Value;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonFont: TFont;
begin
  Result := Button.Font;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonFont(const Value: TFont);
begin
  Button.Font := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonGlyph: TBitmap;
begin
  Result := Button.Glyph;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonGlyph(const Value: TBitmap);
begin
  Button.Glyph := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonHint: string;
begin
  Result := Button.Hint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonHint(const Value: string);
begin
  Button.Hint := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonLayout: TButtonLayout;
begin
  Result := Button.Layout;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonLayout(Value: TButtonLayout);
begin
  Button.Layout := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonMargin: Integer;
begin
  Result := Button.Margin;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonMargin(Value: Integer);
begin
  Button.Margin := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonNumGlyphs: TNumGlyphs;
begin
  Result := Button.NumGlyphs;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonNumGlyphs(Value: TNumGlyphs);
begin
  Button.NumGlyphs := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonPopupMenu: TPopupMenu;
begin
  Result := THackControl(Button).PopupMenu;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonPopupMenu(const Value: TPopupMenu);
begin
  THackControl(Button).PopupMenu := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonShowHint: Boolean;
begin
  Result := Button.ShowHint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonShowHint(Value: Boolean);
begin
  Button.ShowHint := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonSpacing: Integer;
begin
  Result := Button.Spacing;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonSpacing(Value: Integer);
begin
  Button.Spacing := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetButtonTransparent: Boolean;
begin
{$IfDef D4}
  Result := Button.Transparent;
{$Else D4}
  Result := False;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetButtonTransparent(Value: Boolean);
begin
{$IfDef D4}
  Button.Transparent := Value;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetOnButtonClick: TNotifyEvent;
begin
  Result := Button.OnClick;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetOnButtonClick(Value: TNotifyEvent);
begin
  Button.OnClick := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetOnButtonDblClick: TNotifyEvent;
begin
  Result := Button.OnDblClick;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetOnButtonDblClick(Value: TNotifyEvent);
begin
  Button.OnDblClick := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetOnButtonMouseDown: TMouseEvent;
begin
  Result := Button.OnMouseDown;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetOnButtonMouseDown(Value: TMouseEvent);
begin
  Button.OnMouseDown := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetOnButtonMouseMove: TMouseMoveEvent;
begin
  Result := Button.OnMouseMove;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetOnButtonMouseMove(Value: TMouseMoveEvent);
begin
  Button.OnMouseMove := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomButtonEdit.GetOnButtonMouseUp: TMouseEvent;
begin
  Result := Button.OnMouseUp;
end;

//------------------------------------------------------------------------------

procedure TabfCustomButtonEdit.SetOnButtonMouseUp(Value: TMouseEvent);
begin
  Button.OnMouseUp := Value;
end;


//==============================================================================
// TabfUpDownEdit
//==============================================================================
// UpDown (spin) editor
{ TabfUpDownEdit }

constructor TabfUpDownEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then _GlobalInit;

  FButtonControl := TUpDown.Create(Self);
  UpDown.Visible := True;
  UpDown.OnChanging := UpDownChanging;

  FButtonInside := False;
  FIsHex := False;
  Orientation := udVertical;
  Thousands := True;

  HookButtonControl;
end;

//------------------------------------------------------------------------------

destructor TabfUpDownEdit.Destroy;
begin
  UnhookButtonControl;

// Remove association
  UpdateButton;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.UpdateButton;
var
  Wnd: THandle;
  i: Integer;
begin
  inherited UpdateButton;

  if ComponentState * [csLoading, csReading, csDestroying] <> [] then Exit;
  if not Assigned(UpDown) then Exit;
  if not (Assigned(Parent) and Parent.HandleAllocated) then Exit;

  if FUpdating then Exit;
  FUpdating := True;
  try
  // Save width
    i := Width;

    if UpDown.HandleAllocated then
      if HandleAllocated then
      begin
        Wnd := SendMessage(UpDown.Handle, UDM_GETBUDDY, 0, 0);
        if Wnd = Handle then Exit;

        SendMessage(UpDown.Handle, UDM_SETBUDDY, Handle, 0);
      end else
      begin
        SendMessage(UpDown.Handle, UDM_SETBUDDY, 0, 0);
        UpDown.Associate := nil;
      end;

  // Update IsHex
    if UpDown.HandleAllocated then
      if IsHex then
        SendMessage(UpDown.Handle, UDM_SETBASE, 16, 0)
      else
        SendMessage(UpDown.Handle, UDM_SETBASE, 10, 0);

  // Restore width if changed
    if i <> Width then Width := i;

    UpDown.Repaint;
  finally
    FUpdating := False;
  end;

end;{procedure TabfUpDownEdit.UpdateButton}


//==============================================================================
// Events

procedure TabfUpDownEdit.Change;
var
  Value: Integer;
begin
  inherited Change;

  if not IsHex then
  begin
    if (Text = '') or (Text = '-') then Exit;
    Value := StrToIntDef(Text, UpDown.Position);
    if Value > Max then Value := Max;
    if Value < Min then Value := Min;
    Text := IntToStr(Value)
  end;
end;

//------------------------------------------------------------------------------
// Validate keyboard input

procedure TabfUpDownEdit.KeyPress(var Key: Char);
begin
  if IsHex then
  begin
    if not (Key in [#0..#31, '0'..'9', 'a'..'f', 'A'..'F', 'x', 'X']) then
      Key := #0;
  end else
  begin
    if not (Key in [#0..#31, '0'..'9'{, DecimalSeparator}, '-']) then
      Key := #0;
  end;

  inherited KeyPress(Key);
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.UpDownChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if Assigned(FUpDownChanging) then FUpDownChanging(Sender, AllowChange);

  if AllowChange then
    Text := IntToStr(UpDown.Position);{}
end;


//==============================================================================
// Properties Get/Set

function TabfUpDownEdit.GetUpDown: TUpDown;
begin
  Result := TUpDown(ButtonControl);
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetIsHex(Value: Boolean);
begin
  if IsHex = Value then Exit;
  FIsHex := Value;
  UpdateButton;
end;

//------------------------------------------------------------------------------

function TabfUpDownEdit.GetIncrement: Integer;
begin
  Result := UpDown.Increment;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetIncrement(Value: Integer);
begin
  UpDown.Increment := Value;
end;

//------------------------------------------------------------------------------


function TabfUpDownEdit.GetMin: SmallInt;
begin
  Result := UpDown.Min;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetMin(Value: SmallInt);
begin
  UpDown.Min := Value;
end;

//------------------------------------------------------------------------------

function TabfUpDownEdit.GetMax: SmallInt;
begin
  Result := UpDown.Max;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetMax(Value: SmallInt);
begin
  UpDown.Max := Value;
end;

//------------------------------------------------------------------------------

function TabfUpDownEdit.GetOrientation: TUDOrientation;
begin
  Result := UpDown.Orientation;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetOrientation(Value: TUDOrientation);
begin
  UpDown.Orientation := Value;
end;


//------------------------------------------------------------------------------

function TabfUpDownEdit.GetPosition: SmallInt;
begin
  Result := UpDown.Position;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetPosition(Value: SmallInt);
begin
  UpDown.Position := Value;
end;

//------------------------------------------------------------------------------

function TabfUpDownEdit.GetThousands: Boolean;
begin
  Result := UpDown.Thousands;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetThousands(Value: Boolean);
begin
  UpDown.Thousands := Value;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

function TabfUpDownEdit.GetOnUpDownChangingEx: TUDChangingEventEx;
begin
  Result := UpDown.OnChangingEx;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetOnUpDownChangingEx(Value: TUDChangingEventEx);
begin
  UpDown.OnChangingEx := Value;
end;

{$EndIf D5}
//------------------------------------------------------------------------------

function TabfUpDownEdit.GetOnUpDownClick: TUDClickEvent;
begin
  Result := UpDown.OnClick;
end;

//------------------------------------------------------------------------------

procedure TabfUpDownEdit.SetOnUpDownClick(Value: TUDClickEvent);
begin
  UpDown.OnClick := Value;
end;

//==============================================================================
// Messages

procedure TabfUpDownEdit.WMPaint(var Message: TWMPaint);
begin
  inherited;

  if FInitialazed then Exit;
  FInitialazed := True;

// Update UpDown because it can become invisible if the parent is not a form
  UpDown.Hide;
  if ButtonVisible and Visible then UpDown.Show;
  UpdateButton; // Don't remove! It ipdates association
end;


//==============================================================================
// TabfFileNameEdit
//==============================================================================
// File name editor with button that opens an associated dialog
{ TabfFileNameEdit }

constructor TabfFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDialogFilterIndex := 1;
  FDialogOptions := [ofHideReadOnly{$IfDef D4}, ofEnableSizing{$EndIf}];
  FDialogHelpContext := 0;
  FDialogKind := dkOpen;
  Button.OnClick := OnButtonClickEvent;
  if (csDesigning in ComponentState) then
    ButtonGlyph := ABF_GLYPH_FILE;
end;

//------------------------------------------------------------------------------

destructor TabfFileNameEdit.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
{$HINTS OFF}

function TabfFileNameEdit.ShowDialog: Boolean;
var
  Dlg: TOpenDialog;

  //-------------------------------------

  procedure _SetFileName;
  var
    S: string;
  begin
    S := FileName;
  // Last slash workaround
    if (Length(S) > 0) and (S[Length(S)] in ['/', '\']) then
    begin
//      Dlg.InitialDir := ExtractFilePath(S);
      Dlg.InitialDir := S;
      Dlg.FileName := '';
    end else
      Dlg.FileName := S;
  end;{Internal procedure _SetFileName}

  //-------------------------------------

begin
  Result := False;

// Open linked dialog if it is specified
  if Assigned(Dialog) then
  begin
    Dlg := Dialog;
    _SetFileName;
    Result := Dialog.Execute;
    if Result then
      FileName := Dialog.FileName;
    Exit;
  end;

  case DialogKind of
    dkOpen: Dlg := TOpenDialog.Create(Self);
    dkSave: Dlg := TSaveDialog.Create(Self);
{$IfDef D3}
    dkOpenPicture: Dlg := TOpenPictureDialog.Create(Self);
    dkSavePicture: Dlg := TSavePictureDialog.Create(Self);
{$Else D3}
  else
    Dlg := TOpenDialog.Create(Self);
{$EndIf D3}
  end;

  try
  // Set properties
    Dlg.Options     := DialogOptions;
    Dlg.DefaultExt  := DialogDefaultExt;
    Dlg.Filter      := DialogFilter;
    Dlg.FilterIndex := DialogFilterIndex;
    Dlg.InitialDir  := DialogInitialDir;
    Dlg.HelpContext := DialogHelpContext;
    Dlg.Options     := DialogOptions;
    Dlg.Title       := DialogTitle;
{$IfDef D6}
    Dlg.OptionsEx     := DialogOptionsEx;
{$EndIf D6}

  // Set events
{$IfDef D3}
    Dlg.OnClose           := OnDialogClose;
    Dlg.OnFolderChange    := OnDialogFolderChange;
    Dlg.OnSelectionChange := OnDialogSelectionChange;
    Dlg.OnShow            := OnDialogShow;
    Dlg.OnTypeChange      := OnDialogTypeChange;
{$EndIf D3}
{$IfDef D4}
    Dlg.OnCanClose        := OnDialogCanClose;
{$EndIf D4}
{$IfDef D5}
    Dlg.OnIncludeItem     := OnDialogIncludeItem;
{$EndIf D5}

    _SetFileName;
  // Show dialog
    Result := Dlg.Execute;
    if Result then
      FileName := Dlg.FileName;
  finally
    FreeAndNil(Dlg)
  end;

end;{function TabfFileNameEdit.ShowDialog}

{$HINTS ON}
//------------------------------------------------------------------------------

procedure TabfFileNameEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Dialog then Dialog := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfFileNameEdit.OnButtonClickEvent(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
  ShowDialog;
end;


//==============================================================================
// Properties Get/Set

function TabfFileNameEdit.GetFileName: TFileName;
begin
  Result := Text;
end;

//------------------------------------------------------------------------------

procedure TabfFileNameEdit.SetFileName(const Value: TFileName);
begin
  Text := Value;
end;


//==============================================================================
// TabfDirectoryNameEdit
//==============================================================================
// Directory name editor with button that opens an associated dialog
{ TabfDirectoryNameEdit }

constructor TabfDirectoryNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDialogCreateButtonVisible := True;
  FDialogCreateButtonEnabled := True;
  FDialogStatusTextAsPath := True;

  FDialogOptions := [sdoOnlyDirs, sdoShowStatus, sdoNewDialogStyle];
  FDialogRootDir := sdrNone;

  Button.OnClick := OnButtonClickEvent;
  if (csDesigning in ComponentState) then
    ButtonGlyph := ABF_GLYPH_FOLDER;
end;

//------------------------------------------------------------------------------

destructor TabfDirectoryNameEdit.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
{$HINTS OFF}

function TabfDirectoryNameEdit.ShowDialog: Boolean;
var
  Dlg: TabfBrowseFolderDlg;
begin
  Result := False;


// Open linked dialog if it is specified
  if Assigned(Dialog) then
  begin
    Dialog.Folder := Directory;
    Result := Dialog.Execute;
    if Result then
      Directory := Dialog.Folder;
    Exit;
  end;

  Dlg := TabfBrowseFolderDlg.Create(Self);
  try
  // Set properties
    Dlg.Caption             := DialogCaption;
    Dlg.CreateButtonVisible := DialogCreateButtonVisible;
    Dlg.CreateButtonEnabled := DialogCreateButtonEnabled;
    Dlg.Folder              := Directory;
    Dlg.Options             := DialogOptions;
    Dlg.RootDir             := DialogRootDir;
    Dlg.StatusText          := DialogStatusText;
    Dlg.StatusTextAsPath    := DialogStatusTextAsPath;
    Dlg.Text                := DialogText;
  // Events
{$IfNDef C1_ONLY}
    Dlg.OnInitialization   := OnDialogInitialization;
    Dlg.OnSelectionChanged := OnDialogSelectionChanged;
{$EndIf C1_ONLY}

  // Show dialog
    Result := Dlg.Execute;
    if Result then
      Directory := Dlg.Folder;
  finally
    FreeAndNil(Dlg)
  end;
end;{function TabfDirectoryNameEdit.ShowDialog}

{$HINTS ON}
//------------------------------------------------------------------------------

procedure TabfDirectoryNameEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Dialog then Dialog := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfDirectoryNameEdit.OnButtonClickEvent(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
  ShowDialog;
end;


//==============================================================================
// Properties Get/Set

function TabfDirectoryNameEdit.GetDirectory: string;
begin
  Result := Text;
end;

//------------------------------------------------------------------------------

procedure TabfDirectoryNameEdit.SetDirectory(const Value: string);
begin
  Text := Value;
end;

//------------------------------------------------------------------------------

function TabfDirectoryNameEdit.IsDialogStatusTextStored: Boolean;
begin
  Result := not DialogStatusTextAsPath;
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}


  ABF_GLYPH_FILE   := TBitmap.Create;
  ABF_GLYPH_FOLDER := TBitmap.Create;


{******************************************************************************}
finalization
{******************************************************************************}

  FreeAndNil(ABF_GLYPH_FILE);
  FreeAndNil(ABF_GLYPH_FOLDER);

end{unit abfEdits}.
