{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfControls;

{$I abf.inc}

interface

uses
{$IfDef D7}
  Themes,
{$EndIf D7}
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Classes, Controls, SysUtils, Messages, Graphics, Forms,
  ExtCtrls, StdCtrls, ComCtrls,
// ABF VCL
  abfClasses, abfSysUtils, abfComponents, abfPropertyDesc;

type

//==============================================================================
// TabfCustomControl
//==============================================================================
// Base class of all ABF custom controls. Provides About propery.

  TabfCustomControl = class(TCustomControl)
  private
    FAbout: string;
  protected
    property About: string read FAbout write FAbout stored False;
  end;


//==============================================================================
// TabfGraphicControl
//==============================================================================
// Base class of all ABF graphic controls. Provides About propery.

  TabfGraphicControl = class(TGraphicControl)
  private
    FAbout: string;
  protected
    property About: string read FAbout write FAbout stored False;
  end;


//==============================================================================
// TabfCustomActivityIndicator
//==============================================================================
// Prototype of activity indicators.

  TabfCustomActivityIndicator = class(TabfCustomControl)
  private
    FAnimate: Boolean;
    FFrameDelay: Word;
    FFrameIndex: Integer;
    FTimer: TTimer;
    FFrameList: TImageList;
    FFrameCount: Integer;
    FFrameSize: Integer;
    FFrameBitmap: TBitmap;
    FLoadedFrames: Boolean;
    procedure TimerExpired(Sender: TObject);
    procedure SetAnimate(AValue: Boolean);
    procedure SetFrameDelay(AValue: Word);
    procedure SetFrameList(AValue: TImageList);
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure ReloadFrames; virtual;
    procedure DrawFrame; virtual;
    procedure Paint; override;
    procedure Resize; override;
    property Animate: Boolean read FAnimate write SetAnimate default False;
    property FrameDelay: Word read FFrameDelay write SetFrameDelay default 50;
    property FrameList: TImageList read FFrameList write SetFrameList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartAnimation;
    procedure StopAnimation;
  published
    property Height stored False;
    property Width stored False;
  end;


//==============================================================================
// TabfCustomActivityIndicator
//==============================================================================

  TabfActivityIndicator = class(TabfCustomActivityIndicator)
  published
    property Anchors;
    property Animate;
    property FrameDelay;
    property FrameList;
  end;


//==============================================================================
// TabfCustomEdit
//==============================================================================
// Prototype of enhanced edits. Supports Flat style and Alignment.
// Note: PasswordChar is working when Alignment = taLeftJustify only!

  EabfEdit = class(EabfException);

  TabfCustomEdit = class(TCustomEdit)
  private
    FAbout: string;
    FAlignment: TAlignment;
    FFlat: Boolean;
    FFocusBorder: Boolean;
    procedure DrawControlBorder(DC: HDC);
    function IsControlBorderVisible: Boolean;
  // Color swapping
    procedure RestoreColors;
    procedure SaveColors;
  // Messages
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
  // Flags and temporary storages
    FMouseInControl: Boolean;
    FPrevColor: TColor;
    FPrevParentColor: Boolean;
//    FPainting: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;
    procedure DrawFlat(DC: HDC); virtual;
    procedure UpdateNonClientArea; virtual;
  // Properties Get/Set
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetFlat(Value: Boolean); virtual;
    procedure SetFocusBorder(Value: Boolean); virtual;
  // Properies
    property About: string read FAbout write FAbout stored False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update; override;
  // Properties
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FocusBorder: Boolean read FFocusBorder write SetFocusBorder
      default True;
  end;{TabfCustomEdit = class(TCustomEdit)}


//==============================================================================
// TabfEdit
//==============================================================================
// Enhanced edit control.  Supports Flat style and Alignment.
// Note: PasswordChar is working when Alignment = taLeftJustify only!

  TabfEdit = class(TabfCustomEdit)
  published
    property About;
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
  end;{TabfEdit = class(TabfCustomEdit)}


//==============================================================================
// TabfCustomComboBox
//==============================================================================
// A prototype of combobox controls with enhanced abilities.

  TabfComboBoxListAlign = (clsRigth, claLeft);

  TabfCustomComboBox = class(TCustomComboBox)
  private
    FAbout: string;
    FFlat: Boolean;
    FEtched: Boolean;
    FFocusBorder: Boolean;
    FItemHeightChanging: Boolean;
    FListAlign: TabfComboBoxListAlign;
    FListWidth: Integer;
    FImageListAsIcon: Boolean;
{$IfNDef D6}
    FLastTime: Cardinal;
    FAutoComplete: Boolean;
    FAutoDropDown: Boolean;
    FFilter: string;
    FOnSelect: TNotifyEvent;
{$EndIf D6}
  // Drawing routines
    procedure DrawButtonFlat(DC: HDC);
    procedure DrawButtonBorder(DC: HDC);
    procedure DrawControlBorder(DC: HDC);
    function IsControlBorderVisible: Boolean;
    function IsButtonBorderVisible: Boolean;
  // Notifying routines
    procedure ImageListChange(Sender: TObject);
  // Color swapping
    procedure SaveColors;
    procedure RestoreColors;
  // Properties Get/Set
    function GetListWidth: Integer;
    procedure SetListWidth(Value: Integer);
    procedure SetImageListAsIcon(Value: Boolean);
  // Messages routines
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
{$IfDef D3}
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
{$EndIf D3}
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
  protected
    FImageIcon: TIcon;
    FImageListChangeLink: TChangeLink;
    FImageList: TImageList;
  // Flags and temporary storages
    FMouseInControl: Boolean;
    FPrevColor: TColor;
    FPrevParentColor: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure UpdateItemHeight; virtual;
{$IfNDef D6}
    function SelectItem(const AnItem: string): Boolean;
    procedure KeyPress(var Key: Char); override;
    procedure Select; dynamic;
{$EndIf D6}
  // Button routines
    function GetButtonWidth: Integer; virtual;
    function GetButtonRect: TRect; virtual;
  // Drawing routines
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); override;
    procedure DrawFlat(DC: HDC); virtual;
    procedure UpdateNonClientArea; virtual;
  // Properties Get/Set
    procedure SetEtched(A: Boolean); virtual;
    procedure SetFlat(A: Boolean); virtual;
    procedure SetFocusBorder(A: Boolean); virtual;
    procedure SetImageList(const A: TImageList); virtual;
    function GetMinItemHeight: Integer; virtual;
  // Properties
    property About: string read FAbout write FAbout stored False;
{$IfNDef D6}
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
{$EndIf D6}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property Etched: Boolean read FEtched write SetEtched default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FocusBorder: Boolean read FFocusBorder write SetFocusBorder
      default True;
    property ImageList: TImageList read FImageList write SetImageList;
    property ImageListAsIcon: Boolean read FImageListAsIcon
      write SetImageListAsIcon default True;
    property ItemIndex default -1;
    property ListWidth: Integer read GetListWidth write SetListWidth default 0;
//    property ListAlign: TabfComboBoxListAlign read FListAlign write FListAlign
//      default clsRigth;
    property MinItemHeight: Integer read GetMinItemHeight;
{$IfDef D6}
    property AutoComplete default True;
    property AutoDropDown default False;
    property BevelKind default bkNone;
{$Else D6}
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete
      default True;
    property AutoDropDown: Boolean read FAutoDropDown write FAutoDropDown
      default False;
{$EndIf D6}
  end;{TabfCustomComboBox = class(TCustomComboBox)}


//==============================================================================
// TabfComboBox
//==============================================================================
// An enhanced combobox control with Flat, Eatched and FocusBorder abilities.

  TabfComboBox = class(TabfCustomComboBox)
  published
  // Properties
    property About;
    property AutoComplete;
    property AutoDropDown;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Etched;
    property Flat;
    property FocusBorder;
    property Font;
    property ImageList;
    property ImageListAsIcon;
    property ItemHeight;
    property ItemIndex;
    property ListWidth;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Style; // Must be streamed before Items
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
{$IfDef D3}
    property ImeMode;
    property ImeName;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$EndIf D4}
{$IfDef D6}
    property Action;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
{$EndIf D6}
  // Events
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property OnSelect;
{$IfDef D4}
    property OnEndDock;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
{$IfDef D6}
    property OnCloseUp;
{$EndIf D6}
  // Properties
    property Items; // Must be streamed after OnMeasureItem
  end;{TabfComboBox = class(TabfCustomComboBox)}


//==============================================================================
// TabfCustomImageListBox
//==============================================================================
// Prototype of listbox with imageges.

  TabfImageLayout = StdCtrls.TTextLayout;

  TabfCustomImageListBox = class(TCustomListBox)
  private
    FAbout: string;
    FImageLayout: TabfImageLayout;
    FImageIndexByObject: Boolean;
    FMargin: Integer;
    FSpacing: Integer;
    FHideSelection: Boolean;
    FImageListAsIcon: Boolean;
  // Notifying routines
    procedure ImageListChange(Sender: TObject);
  // Properties Get/Set
    procedure SetHideSelection(Value: Boolean);
    procedure SetImageIndexByObject(Value: Boolean);
    procedure SetImageLayout(Value: TabfImageLayout);
    procedure SetImageList(const Value: TImageList);
    procedure SetImageListAsIcon(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure SetSpacing(Value: Integer);
  // Messages
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    FImageList: TImageList;
    FImageListChangeLink: TChangeLink;
    FImageIcon: TIcon;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetRealImageIndex(AItemIndex: Integer): Integer; virtual;
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
  // Properties
    property About: string read FAbout write FAbout stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property HideSelection: Boolean read FHideSelection write SetHideSelection
      default False;
    property ImageIndexByObject: Boolean read FImageIndexByObject
      write SetImageIndexByObject default False;
    property ImageLayout: TabfImageLayout read FImageLayout
      write SetImageLayout default tlCenter;
    property ImageList: TImageList read FImageList write SetImageList;
    property ImageListAsIcon: Boolean read FImageListAsIcon
      write SetImageListAsIcon default True;
    property Margin: Integer read FMargin write SetMargin default 2;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
  end;


//==============================================================================
// TabfImageListBox
//==============================================================================
// Enhanced listbox with imageges. Has only one column and wraps text of
// each item

  TabfImageListBox = class(TabfCustomImageListBox)
  published
  // Properties
    property About;
    property HideSelection;
    property ImageList;
    property ImageIndexByObject;
    property ImageLayout;
    property ImageListAsIcon;
    property Margin;
    property Spacing;
    // Inherited
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
//    property IntegralHeight;
//    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
{$IfDef D4}
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$EndIf D4}
{$IfDef D6}
    property AutoComplete;
    property ScrollWidth;
{$EndIf D6}
  // Events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IfDef D4}
    property OnEndDock;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
  end;


//==============================================================================
// TabfCustomImage
//==============================================================================
// Prototype of enhanced image controls.

  TabfCustomImage = class(TImage)
  private
    FAbout: string;
    FOldPictureChanged: TNotifyEvent;
    FCaptionWhenEmpty: Boolean;
    FCaptionVisible: Boolean;
    FImageIndex: TImageIndex;
    FImageListChangeLink: TChangeLink;
    FImageListAsIcon: Boolean;
{$IfNDef D6}
    FProportional: Boolean;
{$EndIf D6}
    function IsPictureStored: Boolean;
  // Properties Get/Set
    procedure SetCaptionVisible(Value: Boolean);
    procedure SetCaptionWhenEmpty(Value: Boolean);
    procedure SetImageList(const Value: TImageList);
    procedure SetImageListAsIcon(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
{$IfNDef D6}
    procedure SetProportional(Value: Boolean);
{$EndIf D6}
  // Messages
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    FImageList: TImageList;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function DestRect: TRect; virtual;
    procedure SetPictureFromImageList; virtual;
    procedure PictureChanged(Sender: TObject); virtual;
    procedure ImageListChange(Sender: TObject); virtual;
  // Properties Get/Set
    function GetRealCanvas: TCanvas; virtual;
  // Properties
    property About: string read FAbout write FAbout stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  // Properties
    property Caption;
    property CaptionVisible: Boolean read FCaptionVisible
      write SetCaptionVisible default False;
    property CaptionWhenEmpty: Boolean read FCaptionWhenEmpty
      write SetCaptionWhenEmpty default True;
    property Font;
    property ImageList: TImageList read FImageList write SetImageList;
    property ImageListAsIcon: Boolean read FImageListAsIcon
      write SetImageListAsIcon default True;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex
      default 0;
    property ParentFont;
    property RealCanvas: TCanvas read GetRealCanvas;
{$IfNDef D6}
    property Proportional: Boolean read FProportional write SetProportional
      default False;
{$EndIf D6}
  published
    property Picture stored IsPictureStored;
  end;{TabfCustomImage = class(TImage)}


//==============================================================================
// TabfImage
//==============================================================================
// Enhanced image controls.

  TabfImage = class(TabfCustomImage)
  published
  // Properies
    property About;
    property Caption;
    property CaptionVisible;
    property CaptionWhenEmpty;
    property Font;
    property ImageList;
    property ImageListAsIcon;
    property ImageIndex;
    property ParentFont;
{$IfNDef D6}
    property Proportional;
{$EndIf D6}
  end;


//==============================================================================
// TabfCustomGroupBox
//==============================================================================
// Prototype of enhanced group box controls. Can have a check box in caption.

  TabfCustomGroupBox = class(TCustomGroupBox)
  private
    FAbout: string;
    FOnCheckBoxClick: TNotifyEvent;
    FAutoDisableControls: Boolean;
  // Properties Get/Set
    procedure SetAutoDisableControls(Value: Boolean);
    function GetCheckBoxChecked: Boolean;
    procedure SetCheckBoxChecked(Value: Boolean);
    function GetCheckBoxVisible: Boolean;
    procedure SetCheckBoxVisible(Value: Boolean);
  // Messages
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    FCheckBox: TCheckBox;
    procedure Paint; override;
    function GetCheckBoxSize: TSize; virtual;
    procedure UpdateCheckBox; virtual;
    procedure UpdateControls; virtual;
    procedure CheckBoxClickEvent(Sender: TObject); virtual;
  // Properties
    property About: string read FAbout write FAbout stored False;
  // Events
    property OnCheckBoxClick: TNotifyEvent read FOnCheckBoxClick
      write FOnCheckBoxClick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update; override;
  // Properties
    property AutoDisableControls: Boolean read FAutoDisableControls
      write SetAutoDisableControls default True;
    property CheckBox: TCheckBox read FCheckBox;
    property CheckBoxChecked: Boolean read GetCheckBoxChecked
      write SetCheckBoxChecked default True;
    property CheckBoxVisible: Boolean read GetCheckBoxVisible
      write SetCheckBoxVisible default True;
  end;


//==============================================================================
// TabfGroupBox
//==============================================================================
// Enhanced group box control. Can have a check box in caption.

  TabfGroupBox = class(TabfCustomGroupBox)
  published
  // Properties
    property About;
    property Align;
    property AutoDisableControls;
    property Caption;
    property Color;
    property CheckBoxChecked;
    property CheckBoxVisible;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
{$EndIf D4}
  // Events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnCheckBoxClick;
{$IfDef D4}
    property OnGetSiteInfo;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnStartDock;
    property OnUnDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
  end;


//==============================================================================
// TabfCustomScrollBar
//==============================================================================
// Prototype of transparent sctroll bar

  TabfCustomScrollBar = class(TabfGraphicControl)
  private
    FAddSize: Integer;
    FAutoSize: Boolean;
    FHideButtons: Boolean;
    FKind: TScrollBarKind;
    FMargin: Integer;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FOnChange: TNotifyEvent;
  // Properties Get/Set
    procedure SetKind(Value: TScrollBarKind);
    procedure SetHideButtons(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    function  GetTimerInterval: Cardinal;
    procedure SetTimerInterval(Value: Cardinal);
    function  GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
  // Messages
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    FTimer: TabfCustomTimer;
    FButtonsDown: Boolean;
    FActiveButtonDown: Boolean;
    procedure Loaded; override;
    procedure Paint; override;
    procedure InitAutoSize; virtual;
    function GetButtonRect(First: Boolean): TRect; virtual;
    procedure DrawButton(First: Boolean; Down: Boolean); virtual;
    function DoPosition: Boolean; virtual;
    procedure OnTimer(Sender: TObject); virtual;
  // Event handlers
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  // Properties Get/Set
    procedure SetAutoSize(Value: Boolean);{$IfDef D6}reintroduce;{$EndIf}virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property HideButtons: Boolean read FHideButtons write SetHideButtons
      default True;
    property Kind: TScrollBarKind read FKind write SetKind;
    property Margin: Integer read FMargin write SetMargin default 0;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property Position: Integer read FPosition write SetPosition;
    property TimerInterval: Cardinal read GetTimerInterval
      write SetTimerInterval default 100;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Align;
    property Color;
    property Hint;
    property ShowHint;
    property Visible;
    property ParentShowHint;
  // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;{TabfCustomScrollBar = class(TGraphicControl)}


//==============================================================================
// TabfScrollBar
//==============================================================================
// Transparent sctroll bar

  TabfScrollBar = class(TabfCustomScrollBar)
  published
  // Properties
    property About;
    property Align;
    property AutoSize;
    property Color;
    property HideButtons;
    property Hint;
    property Margin;
    property Max;
    property Min;
    property Kind;
    property Position;
    property ParentShowHint;
    property ShowHint;
    property TimerInterval;
    property Transparent;
    property Visible;
  // Events
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;



//==============================================================================
// TabfCustomDatePanel
//==============================================================================
// Prototype of complex panel for input or output a Date.

  TabfDatePanelLayout = (dplSystemDefault, dplDMY, dplMDY, dplYMD, dplYDM);

  TabfCustomDatePanel = class(TCustomPanel)
  private
    FAbout: string;
    FControlsSpace: Integer;
    FMonth: Integer;
    FEditDay: Boolean;
    FEditMonth: Boolean;
    FEditYear: Boolean;
    FAlreadyInitControls: Boolean;
    FIgnoreOnChange: Boolean;
    FIgnoreUpdateControls: Boolean;
    FOnChange: TNotifyEvent;
    FOnChangeDate: TNotifyEvent;
    FOnChangeYear: TNotifyEvent;
    FOnChangeDay: TNotifyEvent;
    FOnChangeMonth: TNotifyEvent;
    FOnClickMonth: TNotifyEvent;
    FOnKeyPressDay: TKeyPressEvent;
    FOnKeyPressYear: TKeyPressEvent;
  // Properties Get/Set
    function GetDay: Integer;
    procedure SetDay(Value: Integer);
    procedure SetMonth(Value: Integer);
    function GetYear: Integer;
    procedure SetYear(Value: Integer);
    function GetEtched: Boolean;
    procedure SetEtched(Value: Boolean);
    function GetFocusBorder: Boolean;
    procedure SetFlat(Value: Boolean);
    function GetFlat: Boolean;
    procedure SetFocusBorder(Value: Boolean);
    function GetMonthName: string;
    function GetMonthNames: TStrings;
    procedure SetMonthNames(const Value: TStrings);
    procedure SetEditDay(Value: Boolean);
    procedure SetEditMonth(Value: Boolean);
    procedure SetEditYear(Value: Boolean);
  // Messages Routine
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure CMEnabledChanged(var Message: TWMWinIniChange); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    FControlsLayout: TabfDatePanelLayout;
    FInternalControlsLayout: TabfDatePanelLayout;
  // Components
    FMonthEdit: TabfComboBox;
    FYearEdit: TabfEdit;
    FDayEdit: TabfEdit;
    FDayUpDown: TUpDown;
    FYearUpDown: TUpDown;
    procedure Loaded; override;
    procedure Resize; override;
    procedure InitMonthNames; virtual;
    procedure InitControls; virtual;
    procedure UpdateControls; virtual;
    procedure UpdateWidths; virtual;
    procedure HideShowControl(Control: TWinControl; AVisible: Boolean);
    procedure ValidateDay; virtual;
  // Event handlers
    procedure DoChange; virtual;
    procedure DoChangeDay(Sender: TObject); virtual;
    procedure DoChangeMonth(Sender: TObject); virtual;
    procedure DoChangeYear(Sender: TObject); virtual;
    procedure DoClickMonth(Sender: TObject); virtual;
    procedure DoKeyPressDay(Sender: TObject; var Key: Char); virtual;
    procedure DoKeyPressYear(Sender: TObject; var Key: Char); virtual;
  // Properties Get/Set
    procedure SetControlsLayout(Value: TabfDatePanelLayout); virtual;
    procedure SetControlsSpace(Value: Integer); virtual;
    function GetDate: TDate; virtual;
    procedure SetDate(Value: TDate); virtual;
  // Properies
    property About: string read FAbout write FAbout stored False;
    property Etched: Boolean read GetEtched write SetEtched default False;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property FocusBorder: Boolean read GetFocusBorder write SetFocusBorder
      default True;
    property MonthNames: TStrings read GetMonthNames write SetMonthNames;
  // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeDate: TNotifyEvent read FOnChangeDate write FOnChangeDate;
    property OnChangeDay: TNotifyEvent read FOnChangeDay write FOnChangeDay;
    property OnChangeMonth: TNotifyEvent read FOnChangeMonth write
      FOnChangeMonth;
    property OnChangeYear: TNotifyEvent read FOnChangeYear write FOnChangeYear;
    property OnClickMonth: TNotifyEvent read FOnClickMonth write
      FOnClickMonth;
    property OnKeyPressDay: TKeyPressEvent read FOnKeyPressDay
      write FOnKeyPressDay;
    property OnKeyPressYear: TKeyPressEvent read FOnKeyPressYear
      write FOnKeyPressYear;
  public
    constructor Create(AOwner: TComponent); override;
  // Properies
    property ControlsLayout: TabfDatePanelLayout read FControlsLayout
      write SetControlsLayout default dplSystemDefault;
    property ControlsSpace: Integer read FControlsSpace write SetControlsSpace
      default 8;
    property Date: TDate read GetDate write SetDate;
    property Day: Integer read GetDay write SetDay stored False;
    property Month: Integer read FMonth write SetMonth stored False;
    property Year: Integer read GetYear write SetYear stored False;
    property MonthName: string read GetMonthName;
    property EditDay: Boolean read FEditDay write SetEditDay default True;
    property EditMonth: Boolean read FEditMonth write SetEditMonth default True;
    property EditYear: Boolean read FEditYear write SetEditYear default True;
  // Control aliases
    property DayEdit: TabfEdit read FDayEdit;
    property DayUpDown: TUpDown read FDayUpDown;
    property MonthEdit: TabfComboBox read FMonthEdit;
    property YearEdit: TabfEdit read FYearEdit;
    property YearUpDown: TUpDown read FYearUpDown;
  end;{TabfCustomDatePanel = Class(TPanel)}


//==============================================================================
// TabfDatePanel
//==============================================================================
// Useful complex panel for input or output a Date or a parts of Date.

  TabfDatePanel = class(TabfCustomDatePanel)
  published
    property About;
    property Align;
    property BorderWidth;
    property Color;
    property Ctl3D;
    property Date;
    property EditDay;
    property EditMonth;
    property EditYear;
    property Etched;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property FocusBorder;
    property FullRepaint;
    property Font;
    property Locked;
    property MonthNames;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
{$EndIf D4}
  // Must be last !!!
    property ControlsSpace;
    property ControlsLayout;
  // Events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnChange;
    property OnChangeDate;
    property OnChangeDay;
    property OnChangeMonth;
    property OnChangeYear;
    property OnClickMonth;
    property OnKeyPressDay;
    property OnKeyPressYear;
{$IfDef D4}
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
  end;{TabfDatePanel = class(TabfCustomDatePanel)}


{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfGraphics, abfStrUtils;

type
  THackWinControl = class(TWinControl);



{ Support Methods }

procedure DrawParentImage(Control: TControl; DC: HDC; InvalidateParent: Boolean = False);
var
  SaveIndex: Integer;
  P: TPoint;
begin
  if Control.Parent = nil then
    Exit;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, P);

  SetViewportOrgEx(DC, P.X - Control.Left, P.Y - Control.Top, nil);
  IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);

  Control.Parent.Perform(WM_ERASEBKGND, DC, 0);
  Control.Parent.Perform(WM_PRINTCLIENT, DC, prf_Client);

  RestoreDC(DC, SaveIndex);

  if InvalidateParent then
  begin
    if not (Control.Parent is TCustomControl) and not (Control.Parent is TCustomForm) and
       not (csDesigning in Control.ComponentState) then
    begin
      Control.Parent.Invalidate;
    end;
  end;
end;

//==============================================================================
// TabfCustomActivityIndicator
//==============================================================================
// Prototype of activity indicators.

constructor TabfCustomActivityIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FFrameSize := 32;
  Height := FFrameSize;
  Width := FFrameSize;

  FFrameDelay := 50;
  FFrameList := nil;

  FFrameBitmap := TBitmap.Create;
  FFrameBitmap.PixelFormat := pf32bit;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := FFrameDelay;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerExpired;

  FLoadedFrames := False;
end;

//------------------------------------------------------------------------------

destructor TabfCustomActivityIndicator.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FFrameBitmap);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.ReloadFrames;
begin
  if FFrameList = nil then Exit;

  FFrameSize := FFrameList.Height;
  Height := FFrameSize;
  Width := FFrameSize;
  FFrameCount := FFrameList.Count;
  FFrameBitmap.SetSize(FFrameSize, FFrameSize);

  FLoadedFrames := True;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if (Parent <> nil) and Parent.DoubleBuffered then
    PerformEraseBackground(Self, Msg.DC);
  DrawParentImage(Self, Msg.DC, True);
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.DrawFrame;
begin
  if FFrameList = nil then Exit;

  if (FFrameSize <= 0) or not FLoadedFrames then
    Exit;

  if (Parent <> nil) and Parent.DoubleBuffered then
    PerformEraseBackground(Self, FFrameBitmap.Canvas.Handle);
  DrawParentImage(Self, FFrameBitmap.Canvas.Handle);

  if FAnimate then
    FFrameList.Draw(FFrameBitmap.Canvas, 0, 0, FFrameIndex);

  Canvas.Draw(0, 0, FFrameBitmap);
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.Paint;
begin
  inherited;

  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end
  else
    DrawFrame;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.TimerExpired(Sender: TObject);
begin
  try
    FTimer.Interval := FFrameDelay;
    if FFrameIndex >= FFrameCount then
      FFrameIndex := 0;

    DrawFrame;

    Inc(FFrameIndex);
    if FFrameIndex = FFrameCount then
      FFrameIndex := 0;
  except
    Animate := False;
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.StartAnimation;
begin
  Animate := True;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.StopAnimation;
begin
  Animate := False;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.SetAnimate(AValue: Boolean);
begin
  FAnimate := AValue;

  if FAnimate then
  begin
    FFrameIndex := 0;
    if not FLoadedFrames then
      ReloadFrames
  end
  else
    DrawFrame;

  FTimer.Enabled := FAnimate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.SetFrameDelay(AValue: Word);
begin
  if FFrameDelay <> AValue then
  begin
    FFrameDelay := AValue;
    FTimer.Interval := FFrameDelay;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.SetFrameList(AValue: TImageList);
begin
  FFrameList := AValue;

  ReloadFrames;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActivityIndicator.Resize;
begin
  inherited;
  SetBounds(Left, Top, FFrameSize, FFrameSize);
end;


//==============================================================================
// TabfCustomEdit
//==============================================================================
// Prototype of enhanced edits.
{ TabfCustomEdit }

constructor TabfCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taLeftJustify;
  FFlat := False;
  FFocusBorder := True;
  FMouseInControl := False;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.Update;
begin
  inherited;
  UpdateNonClientArea;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER),
     (ES_RIGHT, ES_LEFT, ES_CENTER));
var
  Flag: DWORD;
begin
// Ctl3D Alway must be True !!!
  Ctl3D := True;

  inherited CreateParams(Params);

// If not LeftJustify then use multiline editor for Win95 and NT4
  Flag := 0;
  if not (IsWin98orHigher or OSVersion.Check(5)) then
    if not ((Alignment = taLeftJustify)
      {$IfDef D4}and (not UseRightToLeftAlignment){$EndIf}) then
      Flag := ES_MULTILINE;

  Params.Style := Params.Style or Flag or WS_CLIPCHILDREN
    or Alignments[{$IfDef D4}UseRightToLeftAlignment{$Else}False{$EndIf}, Alignment];
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.CreateWnd;
begin
  inherited CreateWnd;
//  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.Change;
begin
  inherited Change;
//  UpdateNonClientArea;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.KeyPress(var Key: Char);
var
  Flag: DWORD;
begin
  Flag := GetWindowLong(Handle, GWL_STYLE);

// Remove enter and escape in multiline editor
  if (Flag and ES_MULTILINE) = ES_MULTILINE then
    if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
    begin
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then
      begin
        inherited KeyPress(Key);
        Key := #0;
        Exit;
      end;
    end;

  inherited KeyPress(Key);
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.DrawFlat(DC: HDC);
begin
// Draw border
  DrawControlBorder(DC);
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.UpdateNonClientArea;
var
  DC: HDC;
begin
  if not Flat then Exit;

//  if FPainting then Exit;
//  FPainting := True;
  DC := GetWindowDC(Handle);
  try
    DrawFlat(DC);
  finally
    ReleaseDC(Handle, DC);
//    FPainting := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.DrawControlBorder(DC: HDC);
var
  R: TRect;
  ParentColor: TColor;
begin
  if [csLoading, csReading, csDestroying] * ComponentState <> [] then Exit;

  ParentColor := clBtnFace;
  if (not IsControlBorderVisible) and Assigned(Parent) and
    (Parent is TWinControl) then
    ParentColor := TWinControl(Parent).Brush.Color;

//  R := ClientRect;

{  GetWindowRect(Handle, R);
  OffsetRect(R, - R.Left, - R.Top);{}

  R := Rect(0, 0, Width, Height);

  if IsControlBorderVisible then
  begin
    DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
    abfFrameRect(DC, R, ParentColor);
    InflateRect(R, -1, -1);
    abfFrameRect(DC, R, clWindow);
  end else
  begin
    abfFrameRect(DC, R, ParentColor);
    InflateRect(R, -1, -1);
    abfFrameRect(DC, R, ParentColor);
    InflateRect(R, -1, -1);
    abfFrameRect(DC, R, clWindow);
  end;
end;

//------------------------------------------------------------------------------

function TabfCustomEdit.IsControlBorderVisible: Boolean;
begin
  Result := FMouseInControl or (Screen.ActiveControl = Self);
  Result := Result and FocusBorder;
end;


//==============================================================================
// Color swapping

procedure TabfCustomEdit.SaveColors;
begin
  FPrevParentColor := inherited ParentColor;
  FPrevColor := inherited Color;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.RestoreColors;
begin
  inherited Color := FPrevColor;
  inherited ParentColor := FPrevParentColor;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomEdit.SetAlignment(Value: TAlignment);
begin
  if Alignment = Value then Exit;
  FAlignment := Value;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.SetFlat(Value: Boolean);
var
  OldValue: Boolean;
begin
  if FFlat = Value then Exit;

  OldValue := Enabled;
  try
    Enabled := False;
    Enabled := True;
    FFlat := Value;
//    Ctl3D := not FFlat; // Don't use Ctl3D, Edit become smaller
  finally
    Enabled := OldValue;
  end;

  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.SetFocusBorder(Value: Boolean);
begin
  if FFocusBorder = Value then Exit;
  FFocusBorder := Value;
  Invalidate;
end;


//==============================================================================
// Messages

procedure TabfCustomEdit.WMPaint(var Message: TWMPaint);
begin
  inherited;

// Update non-client area
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  UpdateNonClientArea;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Flat then Exit;

  if Enabled then
  begin
    RestoreColors;
  end else
  begin
    SaveColors;
    inherited ParentColor := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  UpdateNonClientArea;

// Apply auto select for non-standard Alignment
  if AutoSelect and (Alignment <> taLeftJustify) and
    not (csLButtonDown in ControlState) then SelectAll;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  UpdateNonClientArea;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    UpdateNonClientArea;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    UpdateNonClientArea;
  end;
end;



//==============================================================================
// TabfCustomComboBox
//==============================================================================
// A prototype of combobox controls with enhanced abilities.
// Date: 10/18/2001
{ TabfCustomComboBox }

function _GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight + 2;
end;

//------------------------------------------------------------------------------

constructor TabfCustomComboBox.Create(AOwner: TComponent);
begin
  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := ImageListChange;

  FImageIcon := TIcon.Create;

  inherited Create(AOwner);

  FImageListAsIcon := True;
  FFlat := False;
  FFocusBorder := True;
  FEtched := False;
  FListWidth := 0;
  FListAlign := clsRigth;
  FMouseInControl := False;
{$IfNDef D6}
  FLastTime := 0;
  FAutoComplete := True;
  FAutoDropDown := False;
{$EndIf D6}

// Save previous values
  FPrevColor := inherited Color;
  FPrevParentColor := inherited ParentColor;
end;

//------------------------------------------------------------------------------

destructor TabfCustomComboBox.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FImageIcon);
  FreeAndNil(FImageListChangeLink);
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not Assigned(ImageList) then Exit;
  if (Params.Style and CBS_OWNERDRAWVARIABLE) = 0 then
    Params.Style := Params.Style or CBS_OWNERDRAWFIXED;
  UpdateItemHeight;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.Loaded;
begin
  inherited Loaded;
  UpdateItemHeight;
  Invalidate;
end;

//------------------------------------------------------------------------------
// Catches the ImageList removing.

procedure TabfCustomComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = ImageList) then ImageList := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.UpdateItemHeight;
var
  H: Integer;
begin
  H := MinItemHeight;
  FItemHeightChanging := True;
  try
    inherited ItemHeight := H;
  finally
    FItemHeightChanging := False;
  end;
  if HandleAllocated then SendMessage(Handle, CB_SETITEMHEIGHT, 0, H);
end;

//------------------------------------------------------------------------------
{$IfNDef D6}

function TabfCustomComboBox.SelectItem(const AnItem: string): Boolean;
var
  Idx: Integer;
  ValueChange: Boolean;
begin
  if Length(AnItem) = 0 then
  begin
    Result := False;
    ItemIndex := -1;
    Change;
    Exit;
  end;

  Idx := SendMessage(Handle, CB_FINDSTRING, -1, LongInt(PChar(AnItem)));
  Result := (Idx <> CB_ERR);
  if not Result then Exit;

  ValueChange := Idx <> ItemIndex;
  SendMessage(Handle, CB_SETCURSEL, Idx, 0);
  if (Style in [csDropDown, csSimple]) then
  begin
    Text := AnItem + Copy(Items[Idx], Length(AnItem) + 1, MaxInt);
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Length(AnItem),
      Length(Text)));
  end else
  begin
    ItemIndex := Idx;
    FFilter := AnItem;
  end;

  if ValueChange then
  begin
    Click;
    Select;
  end;
end;{function TabfCustomComboBox.SelectItem}

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.KeyPress(var Key: Char);

  //-------------------------------------

  function HasSelectedText(var StartPos, EndPos: DWORD): Boolean;
  begin
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Result := EndPos > StartPos;
  end;

  //-------------------------------------

  procedure DeleteSelectedText;
  var
    StartPos, EndPos: DWORD;
    OldText: String;
  begin
    OldText := Text;
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Delete(OldText, StartPos + 1, EndPos - StartPos);
    SendMessage(Handle, CB_SETCURSEL, -1, 0);
    Text := OldText;
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(StartPos, StartPos));
  end;

  //-------------------------------------

var
  StartPos: DWORD;
  EndPos: DWORD;
  OldText: String;
  SaveText: String;
begin
  inherited;

  if not AutoComplete then exit;
  if Style in [csDropDown, csSimple] then
    FFilter := Text
  else
  begin
   if GetTickCount - FLastTime >= 500 then
      FFilter := '';
    FLastTime := GetTickCount;
  end;
  case Ord(Key) of
    VK_ESCAPE: exit;
    VK_TAB:
      if FAutoDropDown and DroppedDown then
        DroppedDown := False;
    VK_BACK:
      begin
        if HasSelectedText(StartPos, EndPos) then
          DeleteSelectedText
        else
          if (Style in [csDropDown, csSimple]) and (Length(Text) > 0) then
          begin
            SaveText := Text;
            OldText := Copy(SaveText, 1, StartPos - 1);
            SendMessage(Handle, CB_SETCURSEL, -1, 0);
            Text := OldText + Copy(SaveText, EndPos + 1, MaxInt);
            SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(StartPos - 1, StartPos - 1));
            FFilter := Text;
          end
          else
            Delete(FFilter, Length(FFilter), 1);
        Key := #0;
        Change;
      end;
  else
    if FAutoDropDown and not DroppedDown then
      DroppedDown := True;
    if HasSelectedText(StartPos, EndPos) then
    begin
      if SelectItem(Copy(FFilter, 1, StartPos) + Key) then
        Key := #0
    end
    else
      if SelectItem(FFilter + Key) then
        Key := #0;
  end;

end;{procedure TabfCustomComboBox.KeyPress}

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.Select;
begin
  if Assigned(FOnSelect) then FOnSelect(Self) else Change;
end;

{$EndIf D6}


//==============================================================================
// Button routines

function TabfCustomComboBox.GetButtonWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
end;

//------------------------------------------------------------------------------

function TabfCustomComboBox.GetButtonRect: TRect;
begin
  Result := ClientRect;
  Result.Left := Result.Right - GetButtonWidth - 4;
  InflateRect(Result, -2, -2);
end;


//==============================================================================
// Drawing routines

procedure TabfCustomComboBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  X, Y, TH, IH: Integer;
begin
{$IfDef D4}
  TControlCanvas(Canvas).UpdateTextFlags;
{$EndIf D4}

// Do custom draw if OnDrawItem the event is specified
  if Assigned(OnDrawItem) then
  begin
    OnDrawItem(Self, Index, ARect, State);
    Exit;
  end;

// Default draw routine
  if not Assigned(ImageList) then
  begin
    Canvas.FillRect(ARect);
    Canvas.TextOut(ARect.Left + 2, ARect.Top, Items[Index]);
    Exit;
  end;

// Draw with image
  Canvas.FillRect(ARect);
  TH := Canvas.TextHeight(Items[Index]);
  IH := ImageList.Height;
  X := ARect.Left + 2;

// Draw image
  if (Index < ImageList.Count) then
  begin
    Y := ARect.Top + (Max(IH, TH) - IH) div 2;

    if ImageListAsIcon then
    begin
      ImageList.GetIcon(Index, FImageIcon);
      Canvas.Draw(X, Y, FImageIcon);
    end else
      ImageList.Draw(Canvas, X, Y, Index);
  end;

// Output the text
  Inc(X, ImageList.Width + 2);
  Y := ARect.Top + (Max(IH, TH) - TH) div 2;
  Canvas.TextOut(X, Y, Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.DrawFlat(DC: HDC);
begin
// Draw button if it is needed
  DrawButtonFlat(DC);
// Draw border
  DrawControlBorder(DC);
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.UpdateNonClientArea;
var
  DC: HDC;
begin
  if not Flat then Exit;

//  if FPainting then Exit;
//  FPainting := True;
  DC := GetWindowDC(Handle);
  try
    DrawFlat(DC)
  finally
    ReleaseDC(Handle, DC);
//    FPainting := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.DrawButtonFlat(DC: HDC);
var
  R: TRect;
begin
  if (Style = csSimple) then Exit;
  R := GetButtonRect;
  DrawFrameControl(DC, R, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT);
  DrawButtonBorder(DC);
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.DrawControlBorder(DC: HDC);
var
  R: TRect;
  ParentColor: TColor;
begin
  ParentColor := clBtnFace;
  if (not IsControlBorderVisible) and Assigned(Parent) and
    (Parent is TWinControl) then ParentColor := TWinControl(Parent).Brush.Color;
  R := ClientRect;
  if IsControlBorderVisible then
  begin
    DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
    abfFrameRect(DC, R, ParentColor);
    InflateRect(R, -1, -1);
    abfFrameRect(DC, R, clWindow);
  end else
  begin
    abfFrameRect(DC, R, ParentColor);
    InflateRect(R, -1, -1);
    abfFrameRect(DC, R, ParentColor);
    InflateRect(R, -1, -1);
    abfFrameRect(DC, R, clWindow);
  end;
end;{procedure TabfCustomComboBox.DrawControlBorder}

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.DrawButtonBorder(DC: HDC);
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED, EDGE_ETCHED);
var
  R: TRect;
begin
  if (Style = csSimple) then Exit;
  R := GetButtonRect;
// Protect button face from painting
  InflateRect(R, -2, -2);
  with R do
    ExcludeClipRect(DC, Left, Top, Right, Bottom);
  InflateRect(R, 2, 2);
// Draw border depend on border visibility
  if not IsButtonBorderVisible then
  begin
    abfFillRect(DC, R, clBtnFace);
    abfFrameRect(DC, R, clBtnHighLight{clWindow});
  end else
    DrawEdge(DC, R, Edge[Etched], BF_RECT or Flags[DroppedDown]);
// Protect button border from painting
  with R do
    ExcludeClipRect(DC, Left, Top, Right, Bottom);{}
end;{procedure TabfCustomComboBox.DrawButtonBorder}

//------------------------------------------------------------------------------

function TabfCustomComboBox.IsControlBorderVisible: Boolean;
begin
  Result := FMouseInControl or (Screen.ActiveControl = Self);
  Result := Result and FocusBorder;
end;

//------------------------------------------------------------------------------

function TabfCustomComboBox.IsButtonBorderVisible: Boolean;
begin
  Result := FMouseInControl or (Screen.ActiveControl = Self);
end;


//==============================================================================
// Notifying routines

procedure TabfCustomComboBox.ImageListChange(Sender: TObject);
begin
  UpdateItemHeight;
  Invalidate;
end;


//==============================================================================
// Color swapping

procedure TabfCustomComboBox.SaveColors;
begin
  FPrevParentColor := inherited ParentColor;
  FPrevColor := inherited Color;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.RestoreColors;
begin
  inherited Color := FPrevColor;
  inherited ParentColor := FPrevParentColor;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomComboBox.SetEtched(A: Boolean);
begin
  if FEtched = A then Exit;
  FEtched := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.SetFlat(A: Boolean);
var
  OldValue: Boolean;
begin
  if FFlat = A then Exit;
  OldValue := Enabled;
  try
    Enabled := False;
    Enabled := True;
    FFlat := A;
    Ctl3D := not FFlat;
  finally
    Enabled := OldValue;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.SetFocusBorder(A: Boolean);
begin
  if FFocusBorder = A then Exit;
  FFocusBorder := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.SetImageList(const A: TImageList);
begin
  if ImageList = A then Exit;
  if Assigned(ImageList) then ImageList.UnRegisterChanges(FImageListChangeLink);
  FImageList := A;
  if Assigned(ImageList) then
  begin
    ImageList.FreeNotification(Self);
    ImageList.RegisterChanges(FImageListChangeLink);
  end;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.SetImageListAsIcon(Value: Boolean);
begin
  if ImageListAsIcon = Value then Exit;
  FImageListAsIcon := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TabfCustomComboBox.GetListWidth: Integer;
begin
  if (csDesigning in ComponentState) then
  begin
    Result := FListWidth;
    Exit;
  end;
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.SetListWidth(Value: Integer);
begin
  if (csDesigning in ComponentState) then
  begin
    FListWidth := Value;
    Exit;
  end;
  SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

//------------------------------------------------------------------------------

function TabfCustomComboBox.GetMinItemHeight: Integer;
begin
  Result := Max(_GetItemHeight(Font), 7);
  if Assigned(ImageList) then
    Result := Max(ImageList.Height, Result);
end;


//==============================================================================
// Messages routines

procedure TabfCustomComboBox.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  if not Flat then
  begin
    inherited;
    Exit;
  end;

// Draw as Flat
  if Message.DC = 0 then
    DC := BeginPaint(Handle, PS)
  else
    DC := Message.DC;
  try
    PaintWindow(DC);
    DrawFlat(DC);
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.WMNCPaint(var Message: TMessage);
begin
  inherited;
  UpdateNonClientArea;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Flat then Exit;
  if Enabled then
  begin
    RestoreColors;
  end else
  begin
    SaveColors;
    inherited ParentColor := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  UpdateNonClientArea;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  UpdateNonClientArea;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    UpdateNonClientArea;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    UpdateNonClientArea;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CMWinIniChange(var Message: TWMWinIniChange);
begin
  inherited;
// Update window when some of non-client metrics are changed (Size of button)
  if Message.Unused = SPI_SETNONCLIENTMETRICS then RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateItemHeight;
end;

//------------------------------------------------------------------------------

{$IfDef D3}
procedure TabfCustomComboBox.CMRecreateWnd(var Message: TMessage);
begin
  if not FItemHeightChanging then inherited;
end;
{$EndIf D3}

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CNCommand(var Message: TWMCommand);
var
  R: TRect;
begin
{$IfNDef D6}
  if Message.NotifyCode = CBN_SELCHANGE then
    Select;
{$EndIf D6 }

  inherited;
  if not (Message.NotifyCode in [CBN_CLOSEUP, CBN_DROPDOWN]) then Exit;

//  UpdateNonClientArea
  R := GetClientRect;
  R.Left := R.Right - GetButtonWidth - 2;
  InvalidateRect(Handle, @R, False);
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CNDrawItem(var Message: TWMDrawItem);
begin
  inherited;
  if not Flat then Exit;
// Request for non-client updating, Update Non-client area on each item drawing,
// it flickers enough, but on Windows 2000 only that way is properly working
  UpdateNonClientArea;
//  PostMessage(Handle, WM_NCPAINT, 0, 0); /// Best way, but Windows 2000 :((
end;

//------------------------------------------------------------------------------

procedure TabfCustomComboBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  inherited;
  if not Flat then Exit;
// Request for non-client updating, Update Non-client area on each item drawing,
// it flickers enough, but on Windows 2000 only that way is properly working
//  UpdateNonClientArea;
//  PostMessage(Handle, WM_NCPAINT, 0, 0); /// Best way, but Windows 2000 :((
end;


//==============================================================================
// TabfCustomImageListBox
//==============================================================================
// Prototype of listbox with imageges. Has only one column and wraps text of
// each item
{ TabfCustomImageListBox }

constructor TabfCustomImageListBox.Create(AOwner: TComponent);
begin
  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := ImageListChange;
  FImageIcon := TIcon.Create;

  inherited Create(AOwner);

  Style := lbOwnerDrawVariable;
  FHideSelection := False;
  FImageIndexByObject := False;
  FImageLayout := tlCenter;
  FImageListAsIcon := True;
  FMargin := 2;
  FSpacing := 4;
end;

//------------------------------------------------------------------------------

destructor TabfCustomImageListBox.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FImageIcon);
  FreeAndNil(FImageListChangeLink);
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = ImageList) then ImageList := nil;
  end;
end;

//------------------------------------------------------------------------------

function TabfCustomImageListBox.GetRealImageIndex(AItemIndex: Integer): Integer;
begin
  Result := AItemIndex;
  if not ImageIndexByObject then Exit;

  if (AItemIndex >= 0) and (AItemIndex < {$IfNDef D6}Items.{$EndIf}Count) then
  try
    Result := Integer(Items.Objects[AItemIndex]);
    Exit;
  except
  end;

  Result := -1;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  X, Y, TH, IH, ImageIndex: Integer;
  Flags: DWORD;
  R: TRect;
  Data: string;
begin
  R := ARect;

{$IfDef D4}
  TControlCanvas(Canvas).UpdateTextFlags;
{$EndIf D4}

// Do custom draw if OnDrawItem the event is specified
  if Assigned(OnDrawItem) then
  begin
    OnDrawItem(Self, Index, R, State);
    Exit;
  end;

  if Index >= {$IfNDef D6}Items.{$EndIf}Count then Exit;

// Change canvas properties if needed. Default values are set on CN_DRAWITEM
  if (odSelected in State) and (not HideSelection) then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText
  end;

// Fill background
  Canvas.FillRect(R);

// Set DT_WORDBREAK flag to add word wrapping
  Flags := {$IfDef D4}DrawTextBiDiModeFlags{$EndIf D4}(DT_WORDBREAK or DT_VCENTER or DT_NOPREFIX);

// Get text to be written
  Data := '';
{$IfDef D6}
  if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
    Data := DoGetData(Index)
  else
{$EndIf D6}
    Data := Items[Index];

// Adjust item rectangle
{$IfDef D4}
  if UseRightToLeftAlignment then Dec(R.Right, 2) else
{$EndIf D4}
  Inc(R.Left, 2);
  InflateRect(R, -Margin, -Margin);

// Default draw routine
  if not Assigned(ImageList) then
  begin
    DrawText(Canvas.Handle, PChar(Data), Length(Data), R, Flags);
    Exit;
  end;

// Draw with image
  TH := R.Bottom - R.Top;
  IH := ImageList.Height;
  X := R.Left;
  Y := R.Top;

// Draw image
  ImageIndex := GetRealImageIndex(Index);

  if (ImageIndex < ImageList.Count) and (ImageIndex >= 0) then
  begin
    case ImageLayout of
      tlTop: Y := R.Top;
      tlCenter: Y := R.Top + (TH - IH) div 2;
      tlBottom: Y := R.Bottom - ImageList.Height;
    end;

    if ImageListAsIcon then
    begin
      ImageList.GetIcon(ImageIndex, FImageIcon);
      Canvas.Draw(X, Y, FImageIcon);
    end else
      ImageList.Draw(Canvas, X, Y, ImageIndex);
  end;

// Draw text
  R := Rect(R.Left + ImageList.Width + Spacing, R.Top, R.Right, R.Bottom);
  DrawText(Canvas.Handle, PChar(Data), Length(Data), R, Flags);

// Draw focus rect if needed
  if (odFocused in State) and (not HideSelection) then
    DrawFocusRect(Canvas.Handle, ARect);
end;{procedure TabfCustomImageListBox.DrawItem}

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.MeasureItem(Index: Integer;
  var Height: Integer);
var
  Data: string;
  R: TRect;
  Flags: DWORD;
begin
  if Assigned(OnMeasureItem) then
  begin
    OnMeasureItem(Self, Index, Height);
    Exit;
  end;

  if Index >= {$IfNDef D6}Items.{$EndIf}Count then Exit;

// Get current item
  Data := '';
{$IfDef D6}
  if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
    Data := DoGetData(Index)
  else
{$EndIf D6}
    Data := Items[Index];

// Prepare flags
  Flags := DT_CALCRECT or
    {$IfDef D4}DrawTextBiDiModeFlags{$EndIf}(DT_WORDBREAK or DT_VCENTER or DT_NOPREFIX);

// Prepare rect
  if Assigned(ImageList) then
    R := Rect(0, 0, Width - ImageList.Width - Spacing - (2 * Margin), Height)
  else
    R := Rect(0, 0, Width, Height);

// Get height of wrapped item text, without draw
  Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(Data), Length(Data), R, Flags);

// Set item height
  if Assigned(ImageList) then
    Height := 2 * Margin + Max(R.Bottom, ImageList.Height)
  else
    Height := 2 * Margin + R.Bottom;
end;


//==============================================================================
// Notifying routines

procedure TabfCustomImageListBox.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomImageListBox.SetHideSelection(Value: Boolean);
begin
  if HideSelection = Value then Exit;
  FHideSelection := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.SetImageIndexByObject(Value: Boolean);
begin
  if FImageIndexByObject = Value then Exit;
  FImageIndexByObject := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.SetImageLayout(Value: TabfImageLayout);
begin
  if FImageLayout = Value then Exit;
  FImageLayout := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.SetImageList(const Value: TImageList);
begin
  if ImageList = Value then Exit;
  if Assigned(ImageList) then ImageList.UnRegisterChanges(FImageListChangeLink);
  FImageList := Value;
  if Assigned(ImageList) then
  begin
    ImageList.FreeNotification(Self);
    ImageList.RegisterChanges(FImageListChangeLink);
  end;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.SetImageListAsIcon(Value: Boolean);
begin
  if ImageListAsIcon = Value then Exit;
  FImageListAsIcon := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.SetMargin(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMargin = Value then Exit;
  FMargin := Value;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.SetSpacing(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSpacing = Value then Exit;
  FSpacing := Value;
  RecreateWnd;
end;


//==============================================================================
// Messages

procedure TabfCustomImageListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
{$IfDef D5}
    State := TOwnerDrawState(LongRec(itemState).Lo);
{$Else D5}
    State := TOwnerDrawState(Byte(LongRec(itemState).Lo));
{$EndIf D5}
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;

    if Integer(itemID) >= 0 then DrawItem(itemID, rcItem, State)
    else Canvas.FillRect(rcItem);

    Canvas.Handle := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImageListBox.CMWinIniChange(var Message: TWMWinIniChange);
begin
  inherited;
  RecreateWnd;
end;


//==============================================================================
// TabfCustomImage
//==============================================================================
// Enhanced image control.

{$HINTS OFF}
type
  TPrivateHackGraphicControl = class(TControl)
  private
  // Private section must be same as TGraphicControl We need private fields.
    FCanvas: TCanvas;
  end;

{$IfDef D3 }
  TPrivateHackImage = class(TGraphicControl)
  private
  // Private section must be same as TImage. We need private fields.
    FPicture: TPicture;
    FOnProgress: TProgressEvent;
    FAutoSize: Boolean;
    FStretch: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FDrawing: Boolean;
  end;
{$EndIf D3}
{$HINTS ON}

//------------------------------------------------------------------------------
{ TabfCustomImage }

constructor TabfCustomImage.Create(AOwner: TComponent);
begin
  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := ImageListChange;

  inherited Create(AOwner);

  FCaptionVisible   := False;
  FCaptionWhenEmpty := True;
  FImageIndex := 0;
  FImageListAsIcon := True;

// Save old and assign new Picture.OnChange
  FOldPictureChanged := Picture.OnChange;
  Picture.OnChange := PictureChanged;
end;

//------------------------------------------------------------------------------

destructor TabfCustomImage.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FImageListChangeLink);
end;

//------------------------------------------------------------------------------

procedure TabfCustomImage.Paint;
var
  R: TRect;
{$IfNDef D6}
  {$IfDef D3}
  Save: Boolean;
  {$EndIf D3}
{$EndIf D6}
begin
{$IfDef D6}
  inherited Paint;
{$Else D6}

// If proportional draw new style otherwise drawinherited
  if Proportional then
  begin

  // Draw as inherited but with new DestRect (non safe :(()
    if csDesigning in ComponentState then
      with RealCanvas do
      begin
        Pen.Style := psDash;
        Brush.Style := bsClear;
        Rectangle(0, 0, Width, Height);
      end;

  {$IfDef D3}
    Save := TPrivateHackImage(Self).FDrawing;
    TPrivateHackImage(Self).FDrawing := True;
    try
  {$EndIf D3}
      with RealCanvas do
        StretchDraw(DestRect, Picture.Graphic);
  {$IfDef D3}
    finally
      TPrivateHackImage(Self).FDrawing := Save;
    end;
  {$EndIf D3}
  end else
    inherited Paint;

{$EndIf D6}

// Draw caption if it is needed
  if (Caption <> '') and (CaptionVisible or
    (CaptionWhenEmpty and
      (not Assigned(Picture.Graphic) or Picture.Graphic.Empty))) then
    begin
      RealCanvas.Font := Font;

    // Get text rect
      FillChar(R, SizeOf(R), 0);
{$IfDef D3}
      R.BottomRight := TPoint(RealCanvas.TextExtent(Caption));
{$Else D3}
      R.Right  := RealCanvas.TextWidth(Caption);
      R.Bottom := RealCanvas.TextHeight(Caption);
{$EndIf D3}
      R := abfAlignRectInRect(R, ClientRect, haCenter, vaCenter);

    // Output text
      SetBkMode(RealCanvas.Handle, Windows.TRANSPARENT);
      RealCanvas.TextOut(R.Left, R.Top, Caption);
    end;

end;{procedure TabfCustomImage.Paint}

//------------------------------------------------------------------------------

procedure TabfCustomImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = ImageList) then ImageList := nil;
  end;
end;

//------------------------------------------------------------------------------

function TabfCustomImage.DestRect: TRect;
{$IfNDef D6}
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
{$EndIf D6}
begin
{$IfDef D6}
  Result := inherited DestRect;
{$Else D6}
  w := Picture.Width;
  h := Picture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end else
    begin
      w := cw;
      h := ch;
    end;
  end;{if Stretch or ...}

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
{$EndIf D6}
end;{function TabfCustomImage.DestRect}

//------------------------------------------------------------------------------
// Set picture's graphic from image list using the current image index

procedure TabfCustomImage.SetPictureFromImageList;
var
  Bitmap: TBitmap;
  Icon: TIcon;
begin
  if not Assigned(ImageList) then Exit;
  if (ImageIndex < 0) or (ImageIndex >= ImageList.Count) then Exit;

  if ImageListAsIcon then
  begin
  // Copy bitmap form the image list to picture as icon
    Icon := TIcon.Create;
    try
      ImageList.GetIcon(ImageIndex, Icon);
      Picture.Assign(Icon);
    finally
      Icon.Free;
    end;
  end else
  begin
  // Copy bitmap form the image list to picture as bitmap
    Bitmap := TBitmap.Create;
    try
      ImageList.GetBitmap(ImageIndex, Bitmap);
      Picture.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;

  Update;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImage.PictureChanged(Sender: TObject);
{$IfNDef D6}
  {$IfDef D3}
var
  G: TGraphic;
  {$EndIf D3}
{$EndIf D6}
begin
{$IfDef D6}
  if Assigned(FOldPictureChanged) then FOldPictureChanged(Self);
{$Else D6}
  {$IfDef D3}

// Delphi 3 - 5
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
    SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := Transparent;
    if (not G.Transparent) and Stretch and (not Proportional) then
      ControlStyle := ControlStyle + [csOpaque]
    else  // picture might not cover entire clientrect
      ControlStyle := ControlStyle - [csOpaque];

    if DoPaletteChange and TPrivateHackImage(Self).FDrawing then
      Update;
  end else
    ControlStyle := ControlStyle - [csOpaque];

  if not TPrivateHackImage(Self).FDrawing then
    Invalidate;

  {$Else D3}

// Delphi 2
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
    SetBounds(Left, Top, Picture.Width, Picture.Height);
  if (Picture.Graphic is TBitmap) and (Picture.Width >= Width) and
    (Picture.Height >= Height) then
    ControlStyle := ControlStyle + [csOpaque] else
    ControlStyle := ControlStyle - [csOpaque];
  if Proportional then
    ControlStyle := ControlStyle - [csOpaque];
  Invalidate;

  {$EndIf D3}
{$EndIf D6}
end;{procedure TabfCustomImage.PictureChanged}

//------------------------------------------------------------------------------

procedure TabfCustomImage.ImageListChange(Sender: TObject);
begin
  SetPictureFromImageList;
end;

//------------------------------------------------------------------------------

function TabfCustomImage.IsPictureStored: Boolean;
begin
  Result := not (Assigned(ImageList) and (ImageIndex >= 0) and
    (ImageIndex < ImageList.Count));
end;


//==============================================================================
// Properties Get/Set

function TabfCustomImage.GetRealCanvas: TCanvas;
begin
  Result := TPrivateHackGraphicControl(Self).FCanvas;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImage.SetCaptionVisible(Value: Boolean);
begin
  if CaptionVisible = Value then Exit;
  FCaptionVisible := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImage.SetCaptionWhenEmpty(Value: Boolean);
begin
  if CaptionWhenEmpty = Value then Exit;
  FCaptionWhenEmpty := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImage.SetImageList(const Value: TImageList);
begin
  if ImageList = Value then Exit;

  if Assigned(ImageList) then
    ImageList.UnRegisterChanges(FImageListChangeLink);

  FImageList := Value;
  if Assigned(ImageList) then
  begin
    ImageList.FreeNotification(Self);
    ImageList.RegisterChanges(FImageListChangeLink);
  end;

  SetPictureFromImageList;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImage.SetImageListAsIcon(Value: Boolean);
begin
  FImageListAsIcon := Value;
  SetPictureFromImageList;
end;

//------------------------------------------------------------------------------

procedure TabfCustomImage.SetImageIndex(Value: TImageIndex);
begin
  FImageIndex := Value;
  SetPictureFromImageList;
end;

//------------------------------------------------------------------------------
{$IfNDef D6}

procedure TabfCustomImage.SetProportional(Value: Boolean);
begin
  if FProportional = Value then Exit;
  FProportional := Value;
  if Assigned(Picture.OnChange)	then Picture.OnChange(Self);
end;

{$EndIf D6}

//==============================================================================
// Messages

procedure TabfCustomImage.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;



//==============================================================================
// TabfCustomGroupBox
//==============================================================================
// Prototype of enhanced group box controls. Can have a check box in caption.
{ TabfCustomGroupBox }

const
  cCheckBoxSize = 13;
  cCheckMargin = 5;

//------------------------------------------------------------------------------

constructor TabfCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  FCheckBox := TCheckBox.Create(Self);
  with CheckBox do
  begin
    Parent := Self;
    Visible := True;
    if (csDesigning in ComponentState) then
      ControlStyle := ControlStyle + [csNoDesignVisible];
    ParentFont := True;
    Checked := True;
    OnClick := CheckBoxClickEvent;
  end;
  FAutoDisableControls := True;
end;

//------------------------------------------------------------------------------

procedure TabfCustomGroupBox.Update;
begin
  inherited;
  UpdateCheckBox;
end;

//------------------------------------------------------------------------------

procedure TabfCustomGroupBox.Paint;
begin
  inherited Paint;
  UpdateCheckBox;
end;

//------------------------------------------------------------------------------

function TabfCustomGroupBox.GetCheckBoxSize: TSize;
begin
  Result.cx := cCheckBoxSize;
  Result.cy := cCheckBoxSize;
end;

//------------------------------------------------------------------------------
// Resizes CheckBox and synchronizes control captions

procedure TabfCustomGroupBox.UpdateCheckBox;
var
  W, H: Integer;
  SizeText, SizeCheck: TSize;
{$IfDef D7}
  R: TRect;
{$EndIf D7}
begin
  if not CheckBox.HandleAllocated then Exit;

  CheckBox.Caption := Text;

// Calculate and apply new size
  Canvas.Font := Font;
{$IfDef D3}
  SizeText  := Canvas.TextExtent(Caption);
{$Else D3}
  SizeText.cx := Canvas.TextWidth(Caption);
  SizeText.cy := Canvas.TextHeight(Caption);
{$EndIf D3}
  SizeCheck := GetCheckBoxSize;

  W := SizeText.cx + cCheckMargin + SizeCheck.cx + 1;
  H := Max(SizeText.cy, SizeCheck.cy);

  CheckBox.SetBounds(8, 0, W, H);

{$IfDef D7}
// Fill background under the CheckBox
  R := CheckBox.BoundsRect;
  with ThemeServices do
    if ThemesEnabled and Assigned(Parent) then
    begin
      DrawParentBackground(Handle,Canvas.Handle, nil, False, @R);
    end else
    begin
     Canvas.Brush.Color := Color;
     Canvas.Brush.Style := bsSolid;
     Canvas.FillRect(R);
    end;
{$EndIf D7}
end;

//------------------------------------------------------------------------------
// Updates states of child controls

procedure TabfCustomGroupBox.UpdateControls;
var
  i: Integer;
  Flag: Boolean;
begin
  if not AutoDisableControls then Exit;

  Flag := Enabled;
  if CheckBoxVisible then
    Flag := CheckBoxChecked and Enabled;

  for i := 0 to ControlCount - 1 do
    if Controls[i] <> CheckBox then
      Controls[i].Enabled := Flag;
    
  Invalidate;
end;

//------------------------------------------------------------------------------
// Click event for CheckBox

procedure TabfCustomGroupBox.CheckBoxClickEvent(Sender: TObject);
begin
  UpdateControls;
  if Assigned(OnCheckBoxClick) then OnCheckBoxClick(Self);
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomGroupBox.SetAutoDisableControls(Value: Boolean);
begin
  FAutoDisableControls := Value;
  UpdateControls;
end;

//------------------------------------------------------------------------------

function TabfCustomGroupBox.GetCheckBoxChecked: Boolean;
begin
  Result := CheckBox.Checked;
end;

//------------------------------------------------------------------------------

procedure TabfCustomGroupBox.SetCheckBoxChecked(Value: Boolean);
begin
  CheckBox.Checked := Value;
  UpdateControls;
end;

//------------------------------------------------------------------------------

function TabfCustomGroupBox.GetCheckBoxVisible: Boolean;
begin
  Result := CheckBox.Visible;
end;

//------------------------------------------------------------------------------

procedure TabfCustomGroupBox.SetCheckBoxVisible(Value: Boolean);
begin
  CheckBox.Visible := Value;

  if (csDesigning in ComponentState) then
  begin
    if CheckBoxVisible then
      CheckBox.ControlStyle := CheckBox.ControlStyle - [csNoDesignVisible]
    else
      CheckBox.ControlStyle := CheckBox.ControlStyle + [csNoDesignVisible];
    RecreateWnd;
  end;

  UpdateCheckBox;
  Invalidate;
end;


//==============================================================================
// Messages

procedure TabfCustomGroupBox.CMTextChanged(var Message: TMessage);
begin
  UpdateCheckBox;
  inherited;
end;


//==============================================================================
// TabfCustomScrollBar
//==============================================================================
// Prototype of transparent sctroll bar
{ TabfCustomScrollBar }

constructor TabfCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TabfCustomTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := False;
  TimerInterval := 100;
  
  FAutoSize := True;
  FHideButtons := True;
  FKind := sbVertical;
  FMargin := 0;
  FMax := 100;

  Height := 100;
  Width := 100;
  Color := clBtnFace;
  Transparent := False;

  InitAutoSize;
end;

//------------------------------------------------------------------------------

destructor TabfCustomScrollBar.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.Loaded;
begin
  inherited Loaded;
  Kind := FKind;
  AutoSize := FAutoSize;
  InitAutoSize;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.Paint;
var
  StateUp, StateDown: Boolean;
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;

  if not Transparent then Canvas.FillRect(ClientRect);

  if (csDesigning in ComponentState) then
  begin
    DrawButton(True, False);
    DrawButton(False, False);
    Exit;
  end;

  StateUp   := False;
  StateDown := False;
  if MouseCapture then
    if FButtonsDown then StateUp := True else StateDown := True;
    
// Update needed button
  if (not FHideButtons) or (Position > Min) then DrawButton(True, StateUp);
  if (not FHideButtons) or (Position < Max) then DrawButton(False, StateDown);
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.InitAutoSize;
begin
  if not AutoSize then Exit;

  if Kind = sbHorizontal then
  begin
    FAddSize := GetSystemMetrics(SM_CXHSCROLL);
    Height := GetSystemMetrics(SM_CYHSCROLL);
  end else
  begin
    Width := GetSystemMetrics(SM_CXVSCROLL);
    FAddSize := GetSystemMetrics(SM_CYVSCROLL);
  end;
end;

//------------------------------------------------------------------------------
// Returns size rect of button

function TabfCustomScrollBar.GetButtonRect(First: Boolean): TRect;

  //-------------------------------------

  function _CalcAddSize(Factor: Integer): Integer;
  begin
    Dec(Factor, 2 * Margin);
    if Factor <= 0 then Result := 0
    else
    if 2 * FAddSize < Factor then Result := FAddSize
    else Result := Factor div 2;
    if Result > 0 then Inc(Result, Margin);
  end;

  //-------------------------------------

var
  i: Integer;
begin
  if Kind = sbHorizontal then i := _CalcAddSize(Width)
  else i := _CalcAddSize(Height);

  if i = 0 then
  begin
    SetRectEmpty(Result);
    Exit;
  end;

  if First then
  begin
    if Kind = sbHorizontal then
      SetRect(Result, Margin, 0, i, Height)
    else
      SetRect(Result, 0, Margin, Width, i);
  end else
  begin
    if Kind = sbHorizontal then
      SetRect(Result, Width - i, 0, Width - Margin, Height)
    else
      SetRect(Result, 0, Height - i, Width, Height - Margin);
  end;
end;{function TabfCustomScrollBar.GetButtonRect}

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.DrawButton(First: Boolean; Down: Boolean);
const
  cButtons: array[Boolean, Boolean] of UINT = (
    (DFCS_SCROLLDOWN, DFCS_SCROLLUP), (DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT));
var
  Flag: UINT;
begin
  Flag := cButtons[Kind = sbHorizontal, First];
  if Down then Flag := Flag or DFCS_PUSHED;
  DrawFrameControl(Canvas.Handle, GetButtonRect(First), DFC_SCROLL, Flag);
end;

//------------------------------------------------------------------------------

function TabfCustomScrollBar.DoPosition: Boolean;
begin
  Result := True;
  if FButtonsDown then
  begin
    if Position > Min then Position := Position - 1 else Result := False;
  end else
  if Position < Max then Position := Position + 1 else Result := False;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  P.X := X;
  P.Y := Y;
  if MouseCapture then
    if PtInRect(GetButtonRect(FButtonsDown), P) <> FActiveButtonDown then
    begin
      FActiveButtonDown := not FActiveButtonDown;
      if (Position > Min) and (Position < Max) then
        DrawButton(FButtonsDown, FActiveButtonDown);
    end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  P.X := X;
  P.Y := Y;

  if Button = mbLeft then
  begin
    if PtInRect(GetButtonRect(True), P) and (Position > 0) then
      FButtonsDown := True
    else
    if PtInRect(GetButtonRect(False), P) and (Position < Max) then
      FButtonsDown := False
    else
      Exit;

    FActiveButtonDown := True;
    DrawButton(FButtonsDown, True);

  // Start timer
    FTimer.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  P.X := X;
  P.Y := Y;

  if Button = mbLeft then
  begin
  // Stop timer
    FTimer.Enabled := False;

    if PtInRect(GetButtonRect(FButtonsDown), P) then DoPosition;
    if (Position > Min) and (Position < Max) or (not HideButtons) then
      DrawButton(FButtonsDown, False);
  end;
end;

//------------------------------------------------------------------------------
// Repeat timer event

procedure TabfCustomScrollBar.OnTimer(Sender: TObject);
var
  P: TPoint;
begin
  inherited;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if PtInRect(GetButtonRect(FButtonsDown), P) then DoPosition;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomScrollBar.SetAutoSize(Value: Boolean);
begin
  if AutoSize = Value then Exit;
  FAutoSize := Value;
  if (csLoading in ComponentState) then Exit;
  InitAutoSize;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetHideButtons(Value: Boolean);
begin
  if HideButtons = Value then Exit;
  FHideButtons := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetKind(Value: TScrollBarKind);
begin
  if Kind = Value then Exit;
  FKind := Value;
  if (csLoading in ComponentState) then Exit;
  SetBounds(Left, Top, Height, Width);
  InitAutoSize;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetMargin(Value: Integer);
begin
  if Margin = Value then Exit;
  FMargin := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetMax(Value: Integer);
begin
  if Max = Value then Exit;
  FMax := Value;
  if Min > Max then Min := Max;
  if Position > Max then Position := Max;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetMin(Value: Integer);
begin
  if Min = Value then Exit;
  FMin := Value;
  if Min > Max then Min := Max;
  if Position < Min then Position := Min;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetPosition(Value: Integer);
//var
//  OldPosition: Integer;
begin
  if (Value < Min) then Value := Min;
  if (Value > Max) then Value := Max;

  if (Position = Value) then Exit;

//  OldPosition := Position;
  FPosition := Value;
  if Assigned(FOnChange) then FOnChange(Self);
{  if not ((Position > Min) and (OldPosition > Min) and
    (Position < Max) and (OldPosition < Max)) then }Invalidate;
end;

//------------------------------------------------------------------------------

function TabfCustomScrollBar.GetTimerInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetTimerInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomScrollBar.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.SetTransparent(Value: Boolean);
begin
  if Transparent = Value then Exit;
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  Invalidate;
  Update;
end;


//==============================================================================
// Messages

procedure TabfCustomScrollBar.CMWinIniChange(var Message: TWMWinIniChange);
begin
  inherited;
// Update window when some of non-client metrics are changed (Size of button)
  if Message.Unused = SPI_SETNONCLIENTMETRICS then InitAutoSize;
end;

//------------------------------------------------------------------------------

procedure TabfCustomScrollBar.WMSize(var Message: TWMSize);
begin
  inherited;
  InitAutoSize;
end;


//==============================================================================
// TabfCustomDatePanel
//==============================================================================
// Useful complex panel for input or output a Date or a part of Date values.
{ TabfCustomDatePanel}

constructor TabfCustomDatePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
// We don't need parent controls and caption
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption];
  Caption := '';
  BevelOuter := bvNone;
  BevelInner := bvNone;
  FullRepaint := False;
  FControlsSpace := 8;
  FControlsLayout := dplSystemDefault;
  FInternalControlsLayout := dplDMY;
  Width  := 200;
  Height := Font.Size + 5; // Will be resize to a MonthEdit's height
  FEditDay   := True;
  FEditMonth := True;
  FEditYear  := True;
  FAlreadyInitControls := False;
  FIgnoreOnChange := True;
// Create sub-controls
  FDayEdit := TabfEdit.Create(Self);
  FDayUpDown := TUpDown.Create(Self);
  FMonthEdit := TabfComboBox.Create(Self);
  FYearEdit := TabfEdit.Create(Self);
  FYearUpDown := TUpDown.Create(Self);
// Set now as default date
  Date := Now;
// Default values for properties
  Etched := False;
  Flat := False;
  FocusBorder := True;
end;{constructor TabfCustomDatePanel.Create}

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.Loaded;
begin
  FIgnoreOnChange := True;
  try
    InitControls; // Routine executes only one time...
    inherited Loaded;
    UpdateControls;
  finally
    FIgnoreOnChange := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.Resize;
begin
  inherited Resize;
  InitControls; // Routine executes only one time...
  UpdateControls;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.InitMonthNames;
var
  i: Integer;
begin
  if (csDesigning in ComponentState) then Exit;

// Fill MonthEdit.Items to 12 items.
  with MonthEdit.Items do
  begin
    if Count >= 12 then Exit; // Already enough
    BeginUpdate;
    try
      for i := Count + 1 to 12 do
        Add(LongMonthNames[i]);
    finally
      EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Inits controls. Executes only one time, after first call ignore flag is set.

procedure TabfCustomDatePanel.InitControls;
begin
  if FAlreadyInitControls then Exit;
  FAlreadyInitControls := True; // Set ignore flag

// Apply sub-control properties
  with DayEdit do
  begin
    Parent := Self;
    MaxLength := 2;
    Width := 10;
  end;

  with DayUpDown do
  begin
    Parent := Self;
    Min := 1;
    Max := 31;
    Thousands := False;
  end;

  with MonthEdit do
  begin
    Parent := Self;
    Style := csDropDownList;
    DropDownCount := 12;
  end;

  with YearEdit do
  begin
    Parent := Self;
    MaxLength := 4;
    Width := 10;
  end;

  with YearUpDown do
  begin
    Parent := Self;
    Min := 0;
    Max := 9999;
    Thousands := False;
  end;

// Update control layout
  SetControlsLayout(ControlsLayout);

// Set month names if empty
  InitMonthNames;

// Set associations
  DayUpDown .Associate := DayEdit;
  YearUpDown.Associate := YearEdit;

// Assign events
  if not (csDesigning in ComponentState) then
  begin
    MonthEdit.ItemIndex  := FMonth - 1;
    DayEdit  .OnChange   := DoChangeDay;
    DayEdit  .OnKeyPress := DoKeyPressDay;
    MonthEdit.OnChange   := DoChangeMonth;
    MonthEdit.OnClick    := DoClickMonth;
    YearEdit .OnChange   := DoChangeYear;
    YearEdit .OnKeyPress := DoKeyPressYear;
  end;

// Resize controls depending on font size
  UpdateWidths;
end;{procedure TabfCustomDatePanel.InitControls}

//------------------------------------------------------------------------------
// Alligns and resize controls. Also resizes panel if width or height too low.

procedure TabfCustomDatePanel.UpdateControls;
var
  AWidth, AHeight, MinAllowedWidth, CurrentPoint: Integer;
begin
  if FIgnoreUpdateControls then Exit;

  InitControls; // Don't remove, it fixes sizes for non System ControlLayout

// Check size
  MinAllowedWidth := GetSystemMetrics(SM_CXVSCROLL) + 20 + 2 * BorderWidth;
  AWidth := Width;
  if EditDay then
    AWidth := AWidth - (DayEdit.Width + DayUpDown.Width + ControlsSpace);
  if EditYear then
    AWidth := AWidth - (YearEdit.Width + YearUpDown.Width + ControlsSpace);
  if AWidth < MinAllowedWidth then
  begin
    AWidth := MinAllowedWidth;
    if EditDay then
      AWidth := AWidth + (DayEdit.Width + DayUpDown.Width + ControlsSpace);
    if EditYear then
      AWidth := AWidth + (YearEdit.Width + YearUpDown.Width + ControlsSpace);
    Width := AWidth + 2 * BorderWidth;
  // Recursive call
    UpdateControls;
    Exit;
  end;

// Check for minimum height
  AHeight := Height - 2 * BorderWidth;
  if MonthEdit.HandleAllocated or (not MonthEdit.Visible) then
    if AHeight < MonthEdit.Height then
    begin
      AHeight := MonthEdit.Height;
      Height := AHeight + 2 * BorderWidth;
    end;

  case FInternalControlsLayout of
    dplDMY: begin
    // DayEdit part
      if EditDay then
      begin
        DayEdit.SetBounds(BorderWidth, BorderWidth, DayEdit.Width, AHeight);
        DayUpDown.SetBounds(DayEdit.Left + DayEdit.Width, BorderWidth,
          DayUpDown.Width, AHeight);
      end;
    // YearEdit part
      if EditYear then
      begin
        YearUpDown.SetBounds(Width - BorderWidth - YearUpDown.Width,
          BorderWidth, YearUpDown.Width, AHeight);
        YearEdit.SetBounds(YearUpDown.Left - YearEdit.Width, BorderWidth,
          YearEdit.Width, AHeight);
      end;
    // MonthEdit part
      if EditDay then
        CurrentPoint := DayUpDown.Left + DayUpDown.Width + ControlsSpace
      else
        CurrentPoint := BorderWidth;
      if EditYear then
        AWidth := YearEdit.Left - ControlsSpace - MonthEdit.Left
      else
        AWidth := Width - BorderWidth - MonthEdit.Left;
      MonthEdit.SetBounds(CurrentPoint, (Height - MonthEdit.Height) div 2,
        AWidth, MonthEdit.Height);

      DayEdit  .TabOrder := 1;
      MonthEdit.TabOrder := 2;
      YearEdit .TabOrder := 3;
    end;{case of dplDMY}

    dplMDY: begin
      CurrentPoint := Width - BorderWidth;
    // YearEdit part
      if EditYear then
      begin
        YearUpDown.SetBounds(CurrentPoint - YearUpDown.Width, BorderWidth,
          YearUpDown.Width, AHeight);
        YearEdit.SetBounds(YearUpDown.Left - YearEdit.Width, BorderWidth,
          YearEdit.Width, AHeight);
        CurrentPoint := YearEdit.Left - ControlsSpace;
      end;
    // DayEdit part
      if EditDay then
      begin
        DayUpDown.SetBounds(CurrentPoint - DayUpDown.Width, BorderWidth,
          DayUpDown.Width, AHeight);
        DayEdit.SetBounds(DayUpDown.Left - DayEdit.Width, BorderWidth,
          DayEdit.Width, AHeight);
      end;
    // MonthEdit part
      if EditDay then
        AWidth := DayEdit.Left - ControlsSpace - MonthEdit.Left
      else
      if EditYear then
        AWidth := YearEdit.Left - ControlsSpace - MonthEdit.Left
      else
        AWidth := Width - BorderWidth - MonthEdit.Left ;
      MonthEdit.SetBounds(BorderWidth, (Height - MonthEdit.Height) div 2,
        AWidth, MonthEdit.Height);

      MonthEdit.TabOrder := 1;
      DayEdit  .TabOrder := 2;
      YearEdit .TabOrder := 3;
    end;

    dplYMD: begin
    // YearEdit part
      if EditYear then
      begin
        YearEdit.SetBounds(BorderWidth, BorderWidth, YearEdit.Width, AHeight);
        YearUpDown.SetBounds(YearEdit.Left + YearEdit.Width, BorderWidth,
          YearUpDown.Width, AHeight);
      end;
    // DayEdit part
      if EditDay then
      begin
        DayUpDown.SetBounds(Width - BorderWidth - DayUpDown.Width,
          BorderWidth, DayUpDown.Width, AHeight);
        DayEdit.SetBounds(DayUpDown.Left - DayEdit.Width, BorderWidth,
          DayEdit.Width, AHeight);
      end;
    // MonthEdit part
      if EditYear then
        CurrentPoint := YearUpDown.Left + YearUpDown.Width + ControlsSpace
      else
        CurrentPoint := BorderWidth;
      if EditDay then
        AWidth := DayEdit.Left - ControlsSpace - MonthEdit.Left
      else
        AWidth := Width - BorderWidth - MonthEdit.Left ;
      MonthEdit.SetBounds(CurrentPoint, (Height - MonthEdit.Height) div 2,
        AWidth, MonthEdit.Height);

      YearEdit .TabOrder := 1;
      MonthEdit.TabOrder := 2;
      DayEdit  .TabOrder := 3;
    end;

    dplYDM: begin
      CurrentPoint := BorderWidth;
    // YearEdit part
      if EditYear then
      begin
        YearEdit.SetBounds(CurrentPoint, BorderWidth,
          YearEdit.Width, AHeight);
        YearUpDown.SetBounds(CurrentPoint + YearEdit.Width, BorderWidth,
          YearUpDown.Width, AHeight);
        CurrentPoint := YearUpDown.Left + YearUpDown.Width + ControlsSpace;
      end;
    // DayEdit part
      if EditDay then
      begin
        DayEdit.SetBounds(CurrentPoint, BorderWidth,
          DayEdit.Width, AHeight);
        DayUpDown.SetBounds(CurrentPoint + DayEdit.Width, BorderWidth,
          DayUpDown.Width, AHeight);
      end;
    // MonthEdit part
      if EditDay then
        CurrentPoint := DayUpDown.Left  + DayUpDown.Width + ControlsSpace
      else
      if EditYear then
        CurrentPoint := YearUpDown.Left  + YearUpDown.Width + ControlsSpace
      else
        CurrentPoint := BorderWidth;
      MonthEdit.SetBounds(CurrentPoint, (Height - MonthEdit.Height) div 2,
        Width - BorderWidth - MonthEdit.Left, MonthEdit.Height);

      YearEdit .TabOrder := 1;
      DayEdit  .TabOrder := 2;
      MonthEdit.TabOrder := 3;
    end;
  end;
end;{procedure TabfCustomDatePanel.UpdateControls}

//------------------------------------------------------------------------------
// Updates width of edits depending on current font size

procedure TabfCustomDatePanel.UpdateWidths;
begin
  Canvas.Font := Font;
  DayEdit .Width := Canvas.TextWidth('88'  ) + 10;
  YearEdit.Width := Canvas.TextWidth('8888') + 10;
  
  DayUpDown .Width := GetSystemMetrics(SM_CXVSCROLL);
  YearUpDown.Width := DayUpDown.Width;
end;

//------------------------------------------------------------------------------
// Simply hides or show specified control. Changing Visible at design time
// doesn't hide a control. So the control is recreated with csNoDesignVisible
// flag is set.

procedure TabfCustomDatePanel.HideShowControl(Control: TWinControl;
  AVisible: Boolean);
begin
  with THackWinControl(Control) do
  begin
    Visible := AVisible;
    Enabled := AVisible;
    if not (csDesigning in ComponentState) then Exit;
  // At design time - change style and recreate control
    if AVisible then
      ControlStyle := ControlStyle - [csNoDesignVisible]
    else
      ControlStyle := ControlStyle + [csNoDesignVisible];
    RecreateWnd;
  end;
end;

//------------------------------------------------------------------------------
// Checks is the day value allowed.

procedure TabfCustomDatePanel.ValidateDay;
var
  Value: Integer;
begin
  with DayEdit do
  begin
    Value := StrToIntDef(Text, DayUpDown.Position);
    DayUpDown.Max := 31;
    if Value > 31 then Value := 31;

    if (Month in [4, 6, 9, 11]) then
    begin
      DayUpDown.Max := 30;
      if (Value > 30) then Value := 30;
    end else
    if (Month = 2) then
      if IsLeapYear(Year) then
      begin // 366 days in year
        DayUpDown.Max := 29;
        if Value > 29 then Value := 29;
      end else
      begin // 365 days in year
        DayUpDown.Max := 28;
        if Value > 28 then Value := 28;
      end;

//    DayUpDown.Position := Value;
    Text := IntToStr(Value);
  end;
end;


//==============================================================================
// Event handlers

procedure TabfCustomDatePanel.DoChange;
begin
  if FIgnoreOnChange then Exit;
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.DoChangeDay(Sender: TObject);
begin
  ValidateDay;

  if FIgnoreOnChange then Exit;

  if Assigned(OnChangeDay) then OnChangeDay(Sender);
  DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.DoChangeMonth(Sender: TObject);
begin
  if MonthEdit.HandleAllocated then
    FMonth := MonthEdit.ItemIndex + 1;

  if FIgnoreOnChange then Exit;

  ValidateDay;
  if Assigned(OnChangeMonth) then OnChangeMonth(Sender);
  DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.DoChangeYear(Sender: TObject);
begin
  ValidateDay;

  if FIgnoreOnChange then Exit;

  if Assigned(OnChangeYear) then OnChangeYear(Sender);
  DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.DoClickMonth(Sender: TObject);
begin
  ValidateDay;

  if Assigned(OnClickMonth) then OnClickMonth(Sender);
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.DoKeyPressDay(Sender: TObject; var Key: Char);
begin
  if (csLoading in ComponentState) then Exit;
  if Assigned(OnKeyPressDay) then OnKeyPressDay(Sender, Key);
  if (Key in [#1..#31, '0'..'9']) then Exit;
  Key := #0;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.DoKeyPressYear(Sender: TObject; var Key: Char);
begin
  if (csLoading in ComponentState) then Exit;
  if Assigned(OnKeyPressYear) then OnKeyPressYear(Sender, Key);
  if (Key in [#1..#31, '0'..'9']) then Exit;
  Key := #0;
end;


//==============================================================================// Properties Get/Set
// Properties Get/Set

procedure TabfCustomDatePanel.SetControlsLayout(Value: TabfDatePanelLayout);
var
  S: string;
begin
// Don't check previous value!!! Needed for auto changing when dplSystemDefault
  FControlsLayout := Value;

// Select FInternalControlsLayout depending on current system settings
  if ControlsLayout = dplSystemDefault then
  begin
    FInternalControlsLayout := dplDMY;
    S := abfLeaveCharset(['d', 'm', 'y'], AnsiLowerCase(ShortDateFormat));
    abfReplaceSubStrings(S, 'dd', 'd');
    abfReplaceSubStrings(S, 'mm', 'm');
    abfReplaceSubStrings(S, 'yy', 'y');
    abfReplaceSubStrings(S, 'yy', 'y'); // for yyyy
    if S = 'mdy' then FInternalControlsLayout := dplMDY;
    if S = 'ymd' then FInternalControlsLayout := dplYMD;
    if S = 'ydm' then FInternalControlsLayout := dplYDM;
  end else
    FInternalControlsLayout := ControlsLayout;

  UpdateControls;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetControlsSpace(Value: Integer);
begin
  if ControlsSpace = Value then Exit;
  FControlsSpace := Value;
  UpdateControls;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetDate: TDate;
begin
  Result := Now;
  try
    Result := EncodeDate(Year, Month, Day);
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetDate(Value: TDate);
var
  Y, M, D: Word;
  Flag: Boolean;
begin
  DecodeDate(Value, Y, M, D);

  Flag := FIgnoreOnChange;
  FIgnoreOnChange := True;
  try
    Day   := D;
    Year  := Y;
    Month := M;
  finally
    FIgnoreOnChange := Flag;
  end;
  DoChange;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetDay: Integer;
begin
  Result := DayUpDown.Position;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetDay(Value: Integer);
begin
  DayUpDown.Position := Value;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetMonth(Value: Integer);
begin
  FMonth := Value;
  if (csDesigning in ComponentState) then Exit;

  if MonthEdit.HandleAllocated then
    MonthEdit.ItemIndex := FMonth - 1;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetYear: Integer;
begin
  Result := YearUpDown.Position;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetYear(Value: Integer);
begin
  YearUpDown.Position := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetEtched: Boolean;
begin
  Result := MonthEdit.Etched;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetEtched(Value: Boolean);
begin
  MonthEdit.Etched := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetFlat: Boolean;
begin
  Result := DayEdit.Flat;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetFlat(Value: Boolean);
begin
  DayEdit  .Flat := Value;
  MonthEdit.Flat := Value;
  YearEdit .Flat := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetFocusBorder: Boolean;
begin
  Result := DayEdit.FocusBorder;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetFocusBorder(Value: Boolean);
begin
  DayEdit  .FocusBorder := Value;
  MonthEdit.FocusBorder := Value;
  YearEdit .FocusBorder := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetMonthName: string;
begin
  Result := '';
  if MonthEdit.HandleAllocated then Result := MonthEdit.Text;
end;

//------------------------------------------------------------------------------

function TabfCustomDatePanel.GetMonthNames: TStrings;
begin
  Result := MonthEdit.Items;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetMonthNames(const Value: TStrings);
begin
  MonthEdit.Items := Value;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetEditDay(Value: Boolean);
begin
  if EditDay = Value then Exit;
  FEditDay := Value;
  FIgnoreUpdateControls := True;
  try
    HideShowControl(DayEdit, Value);
    HideShowControl(DayUpDown, Value);
  finally
    FIgnoreUpdateControls := False;
  end;
  UpdateControls;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetEditMonth(Value: Boolean);
begin
  if EditMonth = Value then Exit;
  FEditMonth := Value;
  FIgnoreUpdateControls := True;
  try
    HideShowControl(MonthEdit, Value);
  finally
    FIgnoreUpdateControls := False;
  end;
  UpdateControls;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.SetEditYear(Value: Boolean);
begin
  if EditYear = Value then Exit;
  FEditYear := Value;
  FIgnoreUpdateControls := True;
  try
    HideShowControl(YearEdit, Value);
    HideShowControl(YearUpDown, Value);
  finally
    FIgnoreUpdateControls := False;
  end;
  UpdateControls;
end;


//==============================================================================
// Messages Routine

//------------------------------------------------------------------------------
// Enables or disables internal controls, also changes its color.

procedure TabfCustomDatePanel.CMEnabledChanged(var Message: TWMWinIniChange);
var
  cColors: array[Boolean] of TColor;
begin
  inherited;
  cColors[False] := Color{clBtnFace};
  cColors[True]  := clWindow;
  DayEdit   .Enabled := Enabled;
  DayUpDown .Enabled := Enabled;
  MonthEdit .Enabled := Enabled;
  YearUpDown.Enabled := Enabled;
  YearEdit  .Enabled := Enabled;
  DayEdit  .Color := cColors[Enabled];
  MonthEdit.Color := cColors[Enabled];
  YearEdit .Color := cColors[Enabled];
end;

//------------------------------------------------------------------------------
// Updates ControlsLayout if system settings is changing.

procedure TabfCustomDatePanel.CMWinIniChange(var Message: TWMWinIniChange);
begin
  inherited;
  UpdateWidths;
  SetControlsLayout(ControlsLayout);
end;

//------------------------------------------------------------------------------
// Resize edits depending on new font size

procedure TabfCustomDatePanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateWidths;
  UpdateControls;
end;

//------------------------------------------------------------------------------

procedure TabfCustomDatePanel.WMPaint(var Message: TWMPaint);
begin
  Caption := '';
  UpdateControls; // Don't move to Paint method !!!
  inherited;
end;


{******************************************************************************}
initialization
{******************************************************************************}

// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfCustomComboBox, TabfComboBox, TabfCustomComboBox,
    TabfComboBox, TabfCustomImageListBox, TabfImageListBox, TabfCustomImage,
    TabfImage, TabfCustomGroupBox, TabfGroupBox, TabfCustomScrollBar,
    TabfScrollBar, TabfCustomDatePanel, TabfDatePanel]);

// Used as subclasses
  abfRegisterClasses([TPanel, TUpDown]);

end{unit abfControls}.
