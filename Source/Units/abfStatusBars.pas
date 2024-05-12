{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfStatusBars;

{$I abf.inc}
{.$Define abfStatusBar_NoDoubleBuffer}
{.$Define abfStatusBar_NoSynchronize}

interface

uses
{$IfDef D7}
  Themes,
{$EndIf D7}
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Menus, Graphics, Controls, Forms,
  ComCtrls,
// ABF VCL  
  abfClasses, abfComponents;

{$IfNDef D3}
const
  CM_SYSFONTCHANGED = CM_BASE + 53;
{$EndIf D3}
{$IfNDef D4}
const
  SB_SETBKCOLOR  = $2001;
{$EndIf D4}

type

//==============================================================================
// Forward declaration
//==============================================================================

  TabfStatusBar = class;
  TabfStatusPanel = class;

//==============================================================================
// TabfProgressBarStyle
//==============================================================================

  TabfProgressType = (aptPercent, aptCount);

  TabfProgressBarStyle = class(TPersistent)
  private
    FCaption: string;
    FMaxValue: Integer;
    FMinValue: Integer;
    FOwner: TabfStatusPanel;
    FProgress: Integer;
    FProgressType: TabfProgressType;
    FShowCaption: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetMaxValue(Value: Integer);
    procedure SetMinValue(Value: Integer);
    procedure SetProgress(Value: Integer);
    procedure SetProgressType(Value: TabfProgressType);
    procedure SetShowCaption(Value: Boolean);
  protected
    constructor Create(AOwner: TabfStatusPanel);
  published
    property Caption: string read FCaption write SetCaption;
    property MaxValue: Integer read FMaxValue write SetMaxValue default 100;
    property MinValue: Integer read FMinValue write SetMinValue default 0;
    property Progress: Integer read FProgress write SetProgress default 0;
    property ProgressType: TabfProgressType read FProgressType
      write SetProgressType default aptPercent;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption
      default True;
  end;


//==============================================================================
// TabfTextStyle
//==============================================================================

  TabfScrollEffect = (seNone, seFromLeftToRight, seFromRightToLeft,
    seChangeableDirection);
  TabfTextLayout = (tlGlyphLeft, tlGlyphRight);

  TabfTextStyle = class(TPersistent)
  private
    FAlignment: TAlignment;
    FGlyphIndex: Integer;
    FInternalScrollEffect: TabfScrollEffect;
    FLayout: TabfTextLayout;
    FOwner: TabfStatusPanel;
    FScrollEffect: TabfScrollEffect;
    FShowText: Boolean;
    FSpacing: Integer;
    FText: string;
    FTextPosition: Integer;
    FTextWidth: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetGlyphIndex(Value: Integer);
    procedure SetLayout(Value: TabfTextLayout);
    procedure SetText(const Value: string);
    procedure SetScrollEffect(Value: TabfScrollEffect);
    procedure SetShowText(Value: Boolean);
    procedure SetSpacing(Value: Integer);
  protected
    constructor Create(AOwner: TabfStatusPanel);
  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex
      default -1;
    property Layout: TabfTextLayout read FLayout write SetLayout
      default tlGlyphLeft;
    property ScrollEffect: TabfScrollEffect read FScrollEffect
      write SetScrollEffect default seNone;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property Text: string read FText write SetText;
  end;


//==============================================================================
// TabfLockKeyStyle
//==============================================================================

  TabfLockKeyType = (lktNumLock, lktCapsLock, lktScrollLock);

  TabfLockKeyStyle = class(TPersistent)
  private
    FCaption: string;
    FColorOff: TColor;
    FColorOn: TColor;
    FKeyLockStateOn: Boolean;
    FLockKeyType: TabfLockKeyType;
    FOwner: TabfStatusPanel;
    procedure ChangeLockState(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetColorOff(Value: TColor);
    procedure SetColorOn(Value: TColor);
    procedure SetLockKeyType(Value: TabfLockKeyType);
  protected
    constructor Create(AOwner: TabfStatusPanel);
  published
    property ColorOff: TColor read FColorOff write SetColorOff
      default clGrayText;
    property ColorOn: TColor read FColorOn write SetColorOn
      default clWindowText;
    property Caption: string read FCaption write SetCaption;
    property LockKeyType: TabfLockKeyType read FLockKeyType
      write SetLockKeyType default lktNumLock;
  end;


//==============================================================================
// TabfStatusPanel
//==============================================================================

  TabfStatusPanelStyle = (apsEmpty, apsOwnerDraw, apsText, apsProgressBar,
    apsLockKey, apsAppHint);
  TabfStatusPanelBevel = (apbNone, apbLowered, apbRaised);
  EabfStatusPanelError = class(EabfException);

  TabfStatusPanel = class(TCollectionItem)
  private
{$IfDef D4}
    FBiDiMode: TBiDiMode;
{$EndIf D4}
    FBevel: TabfStatusPanelBevel;
    FBrushColor: TColor;
    FBrushStyle: TBrushStyle;
    FFont: TFont;
    FHint: string;
    FLastRect: TRect;
    FLockKeyStyle: TabfLockKeyStyle;
    FName: string;
    FParentBiDiMode: Boolean;
    FParentColor: Boolean;
    FParentFont: Boolean;
    FPopupMenu: TPopupMenu;
    FProgressBarStyle: TabfProgressBarStyle;
    FPanelStyle: TabfStatusPanelStyle;
    FTextStyle: TabfTextStyle;
    FVisible: Boolean;
    FUpdateNeeded: Boolean;
    procedure FontChanged(Sender: TObject);
  // Properties Get/Set
    function  GetStatusBar: TabfStatusBar;
    procedure SetBevel(Value: TabfStatusPanelBevel);
    procedure SetBrushColor(Value: TColor);
    procedure SetBrushStyle(Value: TBrushStyle);
    procedure SetFont(Value: TFont);
    procedure SetName(const Value: string);
    procedure SetPanelStyle(Value: TabfStatusPanelStyle);
    procedure SetParentColor(Value: Boolean);
    procedure SetParentFont(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    function  GetProgress: Integer;
    procedure SetProgress(Value: Integer);
    function  GetText: string;
    procedure SetText(const Value: string);
    procedure SetVisible(Value: Boolean);
    function  GetWidth: Integer;
    procedure SetWidth(Value: Integer);
{$IfDef D4}
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    function IsBiDiModeStored: Boolean;
{$EndIf D4}
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
  protected
    FBitmapBuffer: TBitmap;
    FWidth: Integer;
    function DoScroll: Boolean; virtual;
    function GetDisplayName: string;{$IfDef D3}override;{$Else}virtual;{$EndIf}
    function IsUniqueName(const TestName: string): Boolean; dynamic;
  // Properties 
    property StatusBar: TabfStatusBar read GetStatusBar;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetRealWidth: Integer; dynamic;
{$IfDef D4}
    procedure ParentBiDiModeChanged;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
{$EndIf D4}
    property BitmapBuffer: TBitmap read FBitmapBuffer;
  published
    property Bevel: TabfStatusPanelBevel read FBevel write SetBevel
      default apbLowered;
    property BrushColor: TColor read FBrushColor write SetBrushColor
      stored IsColorStored;
    property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle
      default bsSolid;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Hint: string read FHint write FHint;
    property LockKeyStyle: TabfLockKeyStyle read FLockKeyStyle
      write FLockKeyStyle;
    property Name: string read FName write SetName;
    property PanelStyle: TabfStatusPanelStyle read FPanelStyle
      write SetPanelStyle default apsText;
    property ParentColor: Boolean read FParentColor write SetParentColor
      default True;
    property ParentFont: Boolean read FParentFont write SetParentFont
      default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Progress: Integer read GetProgress write SetProgress;
    property ProgressBarStyle: TabfProgressBarStyle read FProgressBarStyle
      write FProgressBarStyle;
    property Text: string read GetText write SetText;
    property TextStyle: TabfTextStyle read FTextStyle write FTextStyle;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read GetWidth write SetWidth;
{$IfDef D4}
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode
      stored IsBiDiModeStored;
    property ParentBiDiMode: Boolean read FParentBiDiMode
      write SetParentBiDiMode default True;
{$EndIf D4}
  end;


//==============================================================================
// TabfStatusPanels
//==============================================================================

  TabfStatusPanels = class(TCollection)
  private
    FStatusBar: TabfStatusBar;
    function GetItem(Index: Integer): TabfStatusPanel;
    procedure SetItem(Index: Integer; Value: TabfStatusPanel);
  protected
    function GetOwner: TPersistent;{$IfDef D3}override;{$Else}virtual;{$EndIf}
    procedure SetItemName(Item: TCollectionItem);{$IfDef D3}override;{$Else}virtual;{$EndIf}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(StatusBar: TabfStatusBar);
  // Properties
    property Items[Index: Integer]: TabfStatusPanel read GetItem
      write SetItem; default;
  end;

//==============================================================================
// TabfCustomStatusBar
//==============================================================================

{$IfNDef D4}
  TabfCustomStatusBar = class(TWinControl)
  private
    FDoubleBuffered: Boolean;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
  end;
{$Else D4}
  TabfCustomStatusBar = class(TWinControl);
{$EndIf D4}


//==============================================================================
// TabfStatusBar
//==============================================================================
// TabfStatusBar is a powerful realization of the Windows styled status-bar.
// Provides support of icons, scrolling text, progress bars, lock key
// indicators, and other effects. You can easily show any statistic or dynamic
// information using this component.

  TabfDrawPanelEvent = procedure(StatusBar: TabfStatusBar;
    Panel: TabfStatusPanel; const Rect: TRect) of object;

  TabfPanelClickNotifyEvent = procedure(Panel: TabfStatusPanel;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TabfStatusBar = class(TabfCustomStatusBar)
  private
    FAbout: string;
    FCanvas: TCanvas;
    FCurrentPanel: TabfStatusPanel;
    FGlyphs: TCustomImageList;
    FGlyphsChangeLink: TChangeLink;
    FLock: TRTLCriticalSection;
    FPanels: TabfStatusPanels;
    FSimpleText: string;
    FSimplePanel: Boolean;
    FSizeGrip: Boolean;
    FTimerThread: TThread;
    FUseSystemFont: Boolean;
    FAutoHint: Boolean;
    FOnDrawPanel: TabfDrawPanelEvent;
    FOnHint: TNotifyEvent;
    FOnPanelClick: TabfPanelClickNotifyEvent;
    FOnPanelDblClick: TabfPanelClickNotifyEvent;
{$IfNDef D4}
    FOnResize: TNotifyEvent;
{$EndIf D4}
    procedure GlyphsChange(Sender: TObject);
    procedure SyncToSystemFont;
    procedure UpdatePanel(Index: Integer; Repaint: Boolean);
    procedure UpdatePanels(UpdateRects, UpdateText: Boolean);
    procedure UpdateSimpleText;
{$IfDef D4}
    procedure DoRightToLeftAlignment(var Str: string; AAlignment: TAlignment;
      ARTLAlignment: Boolean);
{$EndIf D4}
  // Properties Get/Set
    procedure SetGlyphs(const Value: TCustomImageList);
    procedure SetPanels(const Value: TabfStatusPanels);
    function  GetPanelValue(const PanelName: string): TabfStatusPanel;
    procedure SetPanelValue(const PanelName: string;
      const Value: TabfStatusPanel);
    procedure SetSimplePanel(Value: Boolean);
    procedure SetSimpleText(const Value: string);
    procedure SetSizeGrip(Value: Boolean);
    procedure SetUseSystemFont(Value: Boolean);
    function IsFontStored: Boolean;
  // Messages routines
{$IfDef D4}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
{$EndIf D4}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMWinIniChange(var Message: TMessage); message CM_WININICHANGE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
{$IfNDef D4}
    procedure Resize; dynamic;
{$EndIf D4}
{$IfDef D5}
    procedure ChangeScale(M, D: Integer); override;
{$EndIf D5}
    function  GetPopupMenu: TPopupMenu; override;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    procedure DrawPanel(Panel: TabfStatusPanel; const Rect: TRect); virtual;
    procedure PaintLockKey(Panel: TabfStatusPanel); virtual;
    procedure PaintProgressBar(Panel: TabfStatusPanel); virtual;
    procedure PaintText(Panel: TabfStatusPanel); virtual;
    procedure CheckLockKeyPanels; virtual;
    procedure CheckScrollPanels; virtual;
    function  DoHint: Boolean; virtual;
    procedure DoScrollPanels; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PanelByName(const PanelName: string): TabfStatusPanel; virtual;
    function PanelAtPos(Point: TPoint): TabfStatusPanel; virtual;
{$IfDef D4}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure FlipChildren(AllLevels: Boolean); override;
{$EndIf D4}
  // Properties
    property Canvas: TCanvas read FCanvas;
    property PanelsValues[const PanelName: string]: TabfStatusPanel
      read GetPanelValue write SetPanelValue; default;
  published
  // Properties
    property About: string read FAbout write FAbout stored False;
    property AutoHint: Boolean read FAutoHint write FAutoHint default False;
    property Align default alBottom;
    property Color default clBtnFace;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font stored IsFontStored;
    property Glyphs: TCustomImageList read FGlyphs write SetGlyphs;
    property Panels: TabfStatusPanels read FPanels write SetPanels;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SimplePanel: Boolean read FSimplePanel write SetSimplePanel;
    property SimpleText: string read FSimpleText write SetSimpleText;
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont
      default True;
    property Visible;
{$IfDef D4}
    property Action;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property DragKind;
    property Constraints;
    property ParentBiDiMode;
{$EndIf D4}
  // Events 
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDrawPanel: TabfDrawPanelEvent read FOnDrawPanel
      write FOnDrawPanel;
    property OnPanelClick: TabfPanelClickNotifyEvent read FOnPanelClick
      write FOnPanelClick;
    property OnPanelDblClick: TabfPanelClickNotifyEvent read FOnPanelDblClick
      write  FOnPanelDblClick;
{$IfDef D4}
    property OnResize;
    property OnStartDock;
    property OnEndDock;
{$Else D4}
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
{$EndIf D4}
    property OnStartDrag;
  end;{TabfStatusBar = class(TabfCustomStatusBar)}


//==============================================================================
// TabfStatusBarInserter
//==============================================================================
// TabfStatusBarInserter is a non-visual component that allows inserting of any
// control (panels, buttons, images, etc.) into any panel of the TStatusBar or
// TabfStatusBar control. You can create status-bars not only static using this
// component.

  TabfStatusBarInserter = class(TabfComponent)
  private
    procedure SetPanelStyle;
    procedure RestorePanelStyle;
    procedure ResetResizingFlags;
  // Properties Get/Set
    procedure SetControl(const A: TControl);
    procedure SetStatusBar(const A: TStatusBar);
    procedure SetAbfStatusBar(const A: TabfStatusBar);
    procedure SetPanelIndex(A: Integer);
  protected
    FNoResizeX, FNoResizeY: Boolean;
    FControl: TControl;
    FControlBoundsRect: TRect;
    FControlParent: TWinControl;
    FControlVisible: Boolean;
    FOldOnDrawPanel: TDrawPanelEvent;
    FOldOnAbfDrawPanel: TabfDrawPanelEvent;
    FStatusBar: TStatusBar;
    FPanelIndex: Integer;
    FPanelStyle: TStatusPanelStyle;
    FabfPanelStyle: TabfStatusPanelStyle;
    FabfStatusBar: TabfStatusBar;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure OnDrawPanel(AStatusBar: TStatusBar; APanel: TStatusPanel;
      const ARect: TRect); virtual;
    procedure OnAbfDrawPanel(AStatusBar: TabfStatusBar; APanel: TabfStatusPanel;
      const ARect: TRect); virtual;
  // Control manipulation
    procedure InsertControl; virtual;
    procedure RemoveControl; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh; virtual;
  published
    property About;
    property StatusBar: TStatusBar read FStatusBar write SetStatusBar;
    property abfStatusBar: TabfStatusBar read FabfStatusBar
      write SetabfStatusBar;
    property Control: TControl read FControl write SetControl;
    property PanelIndex: Integer read FPanelIndex write SetPanelIndex default 0;
  end;{TabfStatusBarInserter = class(TComponent)}


{******************************************************************************}
implementation
{******************************************************************************}

uses
{$IfDef D4}
  StdActns,
{$EndIf D4}
  Consts, CommCtrl,
  abfSysUtils, 
  abfStatusBarsConsts;


{$I abf_init.inc}

//==============================================================================
// Internal types
//==============================================================================

type

//==============================================================================
// THackImageList
//==============================================================================

  THackImageList = class(TCustomImageList);

//==============================================================================
// TTimerThread
//==============================================================================
// Date: 05/01/2000

  TTimerThread = class(TThread)
  private
    FOwner: TabfStatusBar;
    FInterval: Cardinal;
    FException: Exception;
    procedure HandleException;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TabfStatusBar; Enabled: Boolean);
  end;

//------------------------------------------------------------------------------
{ TTimerThread }

constructor TTimerThread.Create(AOwner: TabfStatusBar; Enabled: Boolean);
begin
  inherited Create(not Enabled);
  FOwner := AOwner;
  FInterval := 30;
  FreeOnTerminate := True;
  Priority := tpNormal;
end;

//------------------------------------------------------------------------------

procedure TTimerThread.HandleException;
begin
  if not (FException is EAbort) then
  begin
    if Assigned(Application.OnException) then
      Application.OnException(Self, FException)
    else
      Application.ShowException(FException);
  end;
end;

//------------------------------------------------------------------------------

procedure TTimerThread.Execute;

  function _ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwner = nil);
  end;

begin
  repeat
    if not _ThreadClosed and (SleepEx(FInterval, False) = 0) then
    try
      if not _ThreadClosed then
{$IfNDef abfStatusBar_NoSynchronize}
        Synchronize
{$EndIf abfStatusBar_NoSynchronize}
        (FOwner.DoScrollPanels);
    except
      on E: Exception do
      begin
        FException := E;
        HandleException;
      end;
    end;
  until Terminated;
end;


//==============================================================================
// TabfProgressBarStyle
//==============================================================================
// Date: 05/01/2000
{ TabfProgressBarStyle }

constructor TabfProgressBarStyle.Create(AOwner: TabfStatusPanel);
begin
  FMaxValue := 100;
  FMinValue := 0;
  FOwner := AOwner;
  FProgress := 0;
  FCaption := SabfStatusBars_PercentCaption;
  FProgressType := aptPercent;
  FShowCaption := True;
end;

//------------------------------------------------------------------------------

procedure TabfProgressBarStyle.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfProgressBarStyle.SetMaxValue(Value: Integer);
begin
  if FMaxValue <> Value then
  begin
    if Value <= FMinValue then
      raise EInvalidOperation.CreateFmt(SabfStatusBars_OutOfRange,
        [FMinValue + 1, MaxInt]);
    FMaxValue := Value;
    if FProgress > FMaxValue then FProgress := FMaxValue;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfProgressBarStyle.SetMinValue(Value: Integer);
begin
  if FMaxValue <> Value then
  begin
    if Value >= FMaxValue then
      raise EInvalidOperation.CreateFmt(SabfStatusBars_OutOfRange,
        [-MaxInt, FMaxValue - 1]);
    FMinValue := Value;
    if FProgress < FMinValue then FProgress := FMinValue;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfProgressBarStyle.SetProgress(Value: Integer);
begin
  if Value < FMinValue then Value := FMinValue;
  if Value > FMaxValue then Value := FMaxValue;

  if Progress = Value then Exit;
  FProgress := Value;

  if FOwner.StatusBar.HandleAllocated then
    InvalidateRect(FOwner.StatusBar.Handle, @FOwner.FLastRect, False);
// FOwner.Changed(False); // Flickers too much!
end;

//------------------------------------------------------------------------------

procedure TabfProgressBarStyle.SetProgressType(Value: TabfProgressType);
begin
  if (FProgressType <> Value) then
  begin
    FProgressType := Value;
    case FProgressType of
      aptPercent: FCaption := SabfStatusBars_PercentCaption;
      aptCount: FCaption := SabfStatusBars_CountCaption;
    end;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfProgressBarStyle.SetShowCaption(Value: Boolean);
begin
  if (FShowCaption <> Value) then
  begin
    FShowCaption := Value;
    FOwner.Changed(False);
  end;
end;


//==============================================================================
// TabfTextStyle
//==============================================================================
// Date: 05/01/2000
{ TabfTextStyle }

constructor TabfTextStyle.Create(AOwner: TabfStatusPanel);
begin
  FAlignment := taLeftJustify;
  FGlyphIndex := -1;
  FInternalScrollEffect := seNone;
  FLayout := tlGlyphLeft;
  FOwner := AOwner;
  FScrollEffect := seNone;
  FShowText := True;
  FSpacing := 2;
  FTextPosition := 0;
  FTextWidth := 0;
end;

//------------------------------------------------------------------------------

procedure TabfTextStyle.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTextStyle.SetGlyphIndex(Value: Integer);
begin
  if FGlyphIndex <> Value then
  begin
    FGlyphIndex := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTextStyle.SetLayout(Value: TabfTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTextStyle.SetScrollEffect(Value: TabfScrollEffect);
begin
  if FScrollEffect <> Value then
  begin
    FScrollEffect := Value;
    case FScrollEffect of
      seNone:
        begin
          FTextPosition := 0;
          FInternalScrollEffect := Value;
        end;
      seFromLeftToRight,
      seFromRightToLeft:
        if FInternalScrollEffect <> Value then
        begin
          FTextPosition := FOwner.FWidth + 4;
          FInternalScrollEffect := Value;
        end;
      seChangeableDirection:
        if FInternalScrollEffect = seNone then
        begin
          FTextPosition := FOwner.FWidth + 4;
          FInternalScrollEffect := seFromRightToLeft;
        end;
    end;{case FScrollEffect of}
    if (FOwner.StatusBar <> nil)
      and (not (csLoading in FOwner.StatusBar.ComponentState)) then
      FOwner.StatusBar.CheckScrollPanels;
  end;{if FScrollEffect <> Value then}
end;

//------------------------------------------------------------------------------

procedure TabfTextStyle.SetShowText(Value: Boolean);
begin
  if FShowText <> Value then
  begin
    FShowText := Value;
    if FScrollEffect = seNone then FOwner.Changed(False)
    else if (FOwner.StatusBar <> nil)
     and (not (csLoading in FOwner.StatusBar.ComponentState)) then
      FOwner.StatusBar.CheckScrollPanels;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTextStyle.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfTextStyle.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    FOwner.Changed(False);
  end;
end;


//==============================================================================
// TabfLockKeyStyle
//==============================================================================
// Date: 05/01/2000
{ TabfLockKeyStyle }

constructor TabfLockKeyStyle.Create(AOwner: TabfStatusPanel);
begin
  FCaption := SabfStatusBars_NumLockCaption;
  FColorOff := clGrayText;
  FColorOn := clWindowText;
  FKeyLockStateOn := False;
  FLockKeyType := lktNumLock;
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

procedure TabfLockKeyStyle.ChangeLockState(Value: Boolean);
begin
  if FKeyLockStateOn <> Value then
  begin
    FKeyLockStateOn := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfLockKeyStyle.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfLockKeyStyle.SetColorOff(Value: TColor);
begin
  if FColorOff <> Value then
  begin
    FColorOff := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfLockKeyStyle.SetColorOn(Value: TColor);
begin
  if FColorOn <> Value then
  begin
    FColorOn := Value;
    FOwner.Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfLockKeyStyle.SetLockKeyType(Value: TabfLockKeyType);
begin
  if FLockKeyType <> Value then
  begin
    FLockKeyType := Value;
    case FLockKeyType of
      lktNumLock   : FCaption := SabfStatusBars_NumLockCaption;
      lktCapsLock  : FCaption := SabfStatusBars_CapsLockCaption;
      lktScrollLock: FCaption := SabfStatusBars_ScrollLockCaption;
    end;
    FOwner.Changed(False);
  end;
end;


//==============================================================================
// TabfStatusPanel
//==============================================================================
// Date: 05/01/2000
{ TabfStatusPanel }

constructor TabfStatusPanel.Create(Collection: TCollection);
begin
  FBitmapBuffer := TBitmap.Create;
  inherited Create(Collection);
  FBevel := apbLowered;
  FBrushStyle := bsSolid;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FLockKeyStyle := TabfLockKeyStyle.Create(Self);
  FPanelStyle := apsText;
  FParentBiDiMode := True;
  FProgressBarStyle := TabfProgressBarStyle.Create(Self);
  FTextStyle := TabfTextStyle.Create(Self);
  FVisible := True;
  FWidth := 50;
  ParentColor := True;
  ParentFont := True;
{$IfDef D4}
  ParentBiDiModeChanged;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

destructor TabfStatusPanel.Destroy;
begin
  FTextStyle.Free;
  FProgressBarStyle.Free;
  FLockKeyStyle.Free;
  FFont.Free;
  inherited Destroy;
  FreeAndNil(FBitmapBuffer);
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.Assign(Source: TPersistent);
begin
  if Source is TabfStatusPanel then
    with TabfStatusPanel(Source) do
    begin
      Self.Bevel := Bevel;
      Self.BrushColor := BrushColor;
      Self.BrushStyle := BrushStyle;
      Self.Font.Assign(Font);
      Self.Hint := Hint;
      FParentColor := ParentColor;
      FParentFont := ParentFont;
      Self.Name := Name;
      Self.PanelStyle := PanelStyle;
      Self.PopupMenu := PopupMenu;
      Self.Visible := Visible;
      Self.Width := Width;
      with Self.LockKeyStyle do
      begin
        FLockKeyType := LockKeyStyle.LockKeyType;
        Caption := LockKeyStyle.Caption;
        ColorOff := LockKeyStyle.ColorOff;
        ColorOn := LockKeyStyle.ColorOn;
      end;
      with Self.ProgressBarStyle do
      begin
        FProgressType := ProgressBarStyle.ProgressType;
        Caption := ProgressBarStyle.Caption;
        ShowCaption := ProgressBarStyle.ShowCaption;
        MaxValue := ProgressBarStyle.MaxValue;
        MinValue := ProgressBarStyle.MinValue;
        Progress := ProgressBarStyle.Progress;
      end;
      with Self.TextStyle do
      begin
        Alignment := TextStyle.Alignment;
        GlyphIndex := TextStyle.GlyphIndex;
        Layout := TextStyle.Layout;
        Spacing := TextStyle.Spacing;
        Text := TextStyle.Text;
        ScrollEffect := TextStyle.ScrollEffect; // don't change order!
      end;
    end
  else inherited Assign(Source);
end;

//------------------------------------------------------------------------------
// Returns real value of Width property independently of Visible state.

function TabfStatusPanel.GetRealWidth: Integer;
begin
  Result := FWidth;
end;

//------------------------------------------------------------------------------
{$IfDef D4}

procedure TabfStatusPanel.ParentBiDiModeChanged;
begin
  if FParentBiDiMode then
  begin
    if StatusBar <> nil then
    begin
      BiDiMode := StatusBar.BiDiMode;
      FParentBiDiMode := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

{$EndIf D4}
//------------------------------------------------------------------------------

function TabfStatusPanel.DoScroll: Boolean;
begin
  Result := False;
  if (PanelStyle <> apsText) or (not FTextStyle.FShowText) then Exit;

  case FTextStyle.FInternalScrollEffect of
    seNone:
      Exit; // Doesn't request repaint
    seFromLeftToRight:
      begin
        Inc(FTextStyle.FTextPosition);
        if FTextStyle.FTextPosition > (FWidth + 4) then
        begin
          if FTextStyle.FScrollEffect = seChangeableDirection then
            FTextStyle.FInternalScrollEffect := seFromRightToLeft
          else
            FTextStyle.FTextPosition := -1 * FTextStyle.FTextWidth - 4;
        end;
      end;
    seFromRightToLeft:
      begin
        Dec(FTextStyle.FTextPosition);
        if FTextStyle.FTextPosition < -1 * FTextStyle.FTextWidth - 4 then
        begin
          if FTextStyle.FScrollEffect = seChangeableDirection then
            FTextStyle.FInternalScrollEffect := seFromLeftToRight
          else
            FTextStyle.FTextPosition := (FWidth + 4);
        end;
      end;
  end;
  Result := True;
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.GetDisplayName: string;
begin
  Result := FName;
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.IsUniqueName(const TestName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Collection.Count - 1 do
  if (TabfStatusPanel(Collection.Items[i]).FName = TestName) and
    (Collection.Items[i] <> Self) then Exit;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.FontChanged(Sender: TObject);
begin
  ParentFont := False;
  Changed(False);
end;


//==============================================================================
// Properties Get/Set

function TabfStatusPanel.GetStatusBar: TabfStatusBar;
begin
  if Collection is TabfStatusPanels then
    Result := TabfStatusPanels(Collection).FStatusBar
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetBevel(Value: TabfStatusPanelBevel);
begin
  if FBevel <> Value then
  begin
    FBevel := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetBrushColor(Value: TColor);
begin
  if FBrushColor <> Value then
  begin
    FBrushColor := Value;
    FParentColor := False;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetBrushStyle(Value: TBrushStyle);
begin
  if FBrushStyle <> Value then
  begin
    FBrushStyle := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetFont(Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetName(const Value: string);
var
  TempValue: string;
begin
  TempValue := Trim(Value);
  if FName <> TempValue then
  begin
    if (TempValue = '') or (not IsValidIdent(TempValue)) then
      raise EabfStatusPanelError.CreateFmt(SabfStatusBars_NotValidPanelName,
        [TempValue]);
    if not IsUniqueName(TempValue) then
      raise EabfStatusPanelError.CreateFmt(
        SabfStatusBars_PanelNameAlreadyExists, [TempValue]);
    FName := TempValue;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetPanelStyle(Value: TabfStatusPanelStyle);
begin
  if FPanelStyle <> Value then
  begin
    FPanelStyle := Value;
    Changed(False);
    if (StatusBar <> nil) and (not (csLoading in StatusBar.ComponentState)) then
      StatusBar.CheckScrollPanels;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetParentColor(Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      if StatusBar <> nil then FBrushColor := StatusBar.Color
      else FBrushColor := clBtnFace;
    end;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetParentFont(Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    FFont.OnChange := nil;
    try
      if FParentFont and (StatusBar <> nil) then FFont.Assign(StatusBar.Font);
    finally
      FFont.OnChange := FontChanged;
      Changed(False);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetPopupMenu(Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if (StatusBar <> nil) and (FPopupMenu <> nil) then
      FPopupMenu.FreeNotification(StatusBar);
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.GetProgress: Integer;
begin
  Result := FProgressBarStyle.Progress;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetProgress(Value: Integer);
begin
  FProgressBarStyle.Progress := Value;
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.GetText: string;
begin
  Result := FTextStyle.Text;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetText(const Value: string);
begin
  FTextStyle.Text := Value;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetVisible(Value: Boolean);
begin
  if Visible = Value then Exit;
  FVisible := Value;
  Changed(True);
end;

//------------------------------------------------------------------------------
// Returns 0 for Visible = False, except for writing state - then returns FWidth

function TabfStatusPanel.GetWidth: Integer;
begin
  if Visible or (csWriting in StatusBar.ComponentState) then
    Result := FWidth
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetWidth(Value: Integer);
begin
  if FWidth <> Value then // Check FWidth !!! Width depends on Visible.
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

//------------------------------------------------------------------------------
{$IfDef D4}

procedure TabfStatusPanel.SetBiDiMode(Value: TBiDiMode);
begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanel.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    ParentBiDiModeChanged;
  end;
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.IsBiDiModeStored: Boolean;
begin
  Result := not FParentBiDiMode;
end;

{$EndIf D4}
//------------------------------------------------------------------------------

function TabfStatusPanel.IsColorStored: Boolean;
begin
  Result := not FParentColor;
end;

//------------------------------------------------------------------------------

function TabfStatusPanel.IsFontStored: Boolean;
begin
  Result := not FParentFont;
end;


//==============================================================================
// TabfStatusPanels
//==============================================================================
// Date: 05/01/2000
{ TabfStatusPanels }

constructor TabfStatusPanels.Create(StatusBar: TabfStatusBar);
begin
  inherited Create(TabfStatusPanel);
  FStatusBar := StatusBar;
end;

//------------------------------------------------------------------------------

function TabfStatusPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanels.SetItemName(Item: TCollectionItem);
var
  ClassTemp: string;
  TempName: string;
  i: Integer;
begin
  with TabfStatusPanel(Item) do
    if (Name = '') then
    begin
      i := 0;
      ClassTemp := Copy(ClassName, 2, Length(ClassName) - 1);
      repeat
        Inc(i);
        TempName := ClassTemp + IntToStr(i);
      until IsUniqueName(TempName);
      Name := TempName;
    end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanels.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    FStatusBar.UpdatePanel(Item.Index, False)
  else
    FStatusBar.UpdatePanels(True, False);
end;

//------------------------------------------------------------------------------

function TabfStatusPanels.GetItem(Index: Integer): TabfStatusPanel;
begin
  Result := TabfStatusPanel(inherited GetItem(Index));
end;

//------------------------------------------------------------------------------

procedure TabfStatusPanels.SetItem(Index: Integer; Value: TabfStatusPanel);
begin
  inherited SetItem(Index, Value);
end;


//==============================================================================
// TabfCustomStatusBar
//==============================================================================
// Date: 05/01/2000
{ TabfCustomStatusBar }

{$IfNDef D4}

procedure TabfCustomStatusBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if not FDoubleBuffered or
    (TMessage(Message).wParam = TMessage(Message).lParam) then
    FillRect(Message.DC, ClientRect, Brush.Handle);
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TabfCustomStatusBar.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
    if ControlCount = 0 then inherited else PaintHandler(Message)
  else
    try
      DC := GetDC(0);
      MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right,
        ClientRect.Bottom);
      ReleaseDC(0, DC);
      MemDC := CreateCompatibleDC(0);
      OldBitmap := SelectObject(MemDC, MemBitmap);
      try
        DC := BeginPaint(Handle, PS);
        Perform(WM_ERASEBKGND, MemDC, MemDC);
        Message.DC := MemDC;
        WMPaint(Message);
        Message.DC := 0;
        BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom,
          MemDC, 0, 0, SRCCOPY);
        EndPaint(Handle, PS);
      finally
        SelectObject(MemDC, OldBitmap);
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
    except
    end;
end;

{$EndIf D4}


//==============================================================================
// TabfStatusBar
//==============================================================================
// TabfStatusBar is a powerful realization of the Windows styled status-bar.
// Provides support of icons, scrolling text, progress bars, lock key
// indicators, and other effects. You can easily show any statistic or dynamic
// information using this component.
// Date: 13/04/2001
{ TabfStatusBar }

constructor TabfStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  InitializeCriticalSection(FLock);
  Align := alBottom;
  Color := clBtnFace;
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
  FCurrentPanel := nil;
  FGlyphsChangeLink := TChangeLink.Create;
  FGlyphsChangeLink.OnChange := GlyphsChange;
  FPanels := TabfStatusPanels.Create(Self);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FSizeGrip := True;
  FUseSystemFont := True;
  Height := 19;
  ParentFont := False;
  DoubleBuffered := True;
{$IfDef abfStatusBar_NoDoubleBuffer}
  DoubleBuffered := False;
{$EndIf abfStatusBar_NoDoubleBuffer}
  SyncToSystemFont;
  FTimerThread := nil;
end;

//------------------------------------------------------------------------------

destructor TabfStatusBar.Destroy;
begin
  DoubleBuffered := False;
  if Assigned(FTimerThread) then
  begin
    TTimerThread(FTimerThread).FOwner := nil;
    while FTimerThread.Suspended do FTimerThread.Resume;
    FTimerThread.Terminate;
  end;
  FreeAndNil(FPanels);
  FreeAndNil(FCanvas);
  FreeAndNil(FGlyphsChangeLink);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfStatusBar.PanelByName(const PanelName: string): TabfStatusPanel;
var i: Integer;
begin
  Result := nil;
  for i:= 0 to FPanels.Count - 1 do
  if AnsiCompareText(FPanels[i].FName, PanelName) = 0 then
  begin
    Result := FPanels[i];
    Exit;
  end;
  if Result = nil then
    raise EabfStatusPanelError.CreateFmt(SabfStatusBars_PanelNotFound,
      [PanelName]);
end;

//------------------------------------------------------------------------------

function TabfStatusBar.PanelAtPos(Point: TPoint): TabfStatusPanel;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FPanels.Count - 1 do
  if PtInRect(FPanels[i].FLastRect, Point) then
  begin
    Result := FPanels[i];
    Break;
  end;
end;

//------------------------------------------------------------------------------
{$IfDef D4}

function TabfStatusBar.ExecuteAction(Action: TBasicAction): Boolean;
var
  i: Integer;
begin
  if AutoHint and not (csDesigning in ComponentState) and
    (Action is THintAction) and not DoHint then
  begin
    if SimplePanel or (Panels.Count = 0) then
      SimpleText := THintAction(Action).Hint
    else
      for i := 0 to Panels.Count - 1 do
        if Panels[i].PanelStyle = apsAppHint then
          Panels[i].TextStyle.Text := THintAction(Action).Hint;
    Result := True;
  end else
    Result := inherited ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.FlipChildren(AllLevels: Boolean);
var
  Loop, FirstWidth, LastWidth: Integer;
  APanels: TabfStatusPanels;
begin
  if HandleAllocated and (not SimplePanel) and (Panels.Count > 0) then
  begin
  // Get the true width of the last panel
    LastWidth := ClientWidth;
    FirstWidth := Panels[0].Width;
    for Loop := 0 to Panels.Count - 2 do Dec(LastWidth, Panels[Loop].Width);

  // Flip 'em
    APanels := TabfStatusPanels.Create(Self);
    try
      for Loop := 0 to Panels.Count - 1 do with APanels.Add do
        Assign(Self.Panels[Loop]);
      for Loop := 0 to Panels.Count - 1 do
        Panels[Loop].Assign(APanels[Panels.Count - Loop - 1]);
    finally
      APanels.Free;
    end;

  // Set the width of the last panel
    if Panels.Count > 1 then
    begin
      Panels[Panels.Count-1].Width := FirstWidth;
      Panels[0].Width := LastWidth;
    end;

    UpdatePanels(True, True);
  end;
end;

{$EndIf D4}
//------------------------------------------------------------------------------

procedure TabfStatusBar.CreateParams(var Params: TCreateParams);
const
  GripStyles: array[Boolean] of DWORD = (CCS_TOP, SBARS_SIZEGRIP);
begin
{$IfDef D3}
  InitCommonControl(ICC_BAR_CLASSES);
{$Else D3}
  InitCommonControls;
{$EndIf D3}
  inherited CreateParams(Params);
  CreateSubClass(Params, STATUSCLASSNAME);
  with Params do
  begin
{$IfDef D4}
    Style := Style or GripStyles[FSizeGrip and (Parent is TCustomForm) and
      (TCustomForm(Parent).BorderStyle in [bsSizeable, bsSizeToolWin])];
{$Else D4}
    Style := Style or GripStyles[FSizeGrip and (Parent is TForm) and
      (TForm(Parent).BorderStyle in [bsSizeable, bsSizeToolWin])];
{$EndIf D4}
    WindowClass.style := WindowClass.style and not CS_HREDRAW;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CreateWnd;
var
  i: Integer;
begin
  inherited CreateWnd;

  SendMessage(Handle, SB_SETBKCOLOR, 0, ColorToRGB(Color));

  for i:= 0 to Panels.Count - 1 do
    if Panels[i].ParentColor then Panels[i].FBrushColor := Color;

  UpdatePanels(True, False);

  if FSimplePanel then
  begin
    if FSimpleText <> '' then
{$IFDEF WIN32}
      SendMessage(Handle, SB_SETTEXT, 255, Integer(PChar(FSimpleText)));
{$ENDIF}
{$IFDEF WIN64}
      SendMessage(Handle, SB_SETTEXT, 255, NativeInt(PChar(FSimpleText)));
{$ENDIF}

    SendMessage(Handle, SB_SIMPLE, 1, 0);
  end;

  SetTimer(Handle, 0, 100, nil);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.DestroyWnd;
begin
  KillTimer(Handle, 0);
  inherited DestroyWnd;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.Loaded;
begin
  inherited Loaded;
  CheckLockKeyPanels;
  CheckScrollPanels;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FGlyphs then Glyphs := nil else
    if AComponent is TPopupMenu then
    begin
      for i := 0 to FPanels.Count-1 do
      if FPanels[i].PopupMenu = AComponent then FPanels[i].PopupMenu := nil;
    end;
  end
end;

//------------------------------------------------------------------------------
{$IfNDef D4}

procedure TabfStatusBar.Resize;
begin
  if Assigned(FOnResize) then FOnResize(Self);
end;

{$EndIf D4}
//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfStatusBar.ChangeScale(M, D: Integer);
begin
  if UseSystemFont then ScalingFlags := [sfTop];
  inherited ChangeScale(M, D);
end;

{$EndIf D5}
//------------------------------------------------------------------------------

function TabfStatusBar.GetPopupMenu: TPopupMenu;
begin
  if Assigned(FCurrentPanel) and Assigned(FCurrentPanel.PopupMenu) then
  begin
    Result := FCurrentPanel.PopupMenu;
{$IfDef D4}
    if Result <> nil then Result.BiDiMode := FCurrentPanel.BiDiMode;
{$EndIf D4}
  end else
    Result := PopupMenu;
end;


//------------------------------------------------------------------------------

procedure TabfStatusBar.Lock;
begin
  EnterCriticalSection(FLock);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.DrawPanel(Panel: TabfStatusPanel; const Rect: TRect);
begin
  if Panel.PanelStyle = apsOwnerDraw then
  begin
{$IFDEF WIN32}
    if not Boolean(SendMessage(Handle, SB_GETRECT, Panel.Index,
      Integer(@Panel.FLastRect))) then
      Panel.FLastRect := Rect
    else
      InflateRect(Panel.FLastRect, -1, -1);
{$ELSE}
    if not Boolean(SendMessage(Handle, SB_GETRECT, Panel.Index,
      NativeInt(@Panel.FLastRect))) then
      Panel.FLastRect := Rect
    else
      InflateRect(Panel.FLastRect, -1, -1);
{$ENDIF}

  {$IfDef D4}
    if Panel.UseRightToLeftReading then
      FCanvas.TextFlags := FCanvas.TextFlags or ETO_RTLREADING
    else
      FCanvas.TextFlags := FCanvas.TextFlags and (not ETO_RTLREADING);
  {$EndIf D4}
    if Assigned(FOnDrawPanel) then
      FOnDrawPanel(Self, Panel, Panel.FLastRect)
    else
    begin
  {$IfDef D7}
      if not ThemeServices.ThemesEnabled then
  {$EndIf D7}
      FCanvas.FillRect(Panel.FLastRect);
    end

  end else
  begin
{$IFDEF WIN32}
    if Boolean(SendMessage(Handle, SB_GETRECT, Panel.Index,
      Integer(@Panel.FLastRect))) then
{$ELSE}
    if Boolean(SendMessage(Handle, SB_GETRECT, Panel.Index,
      NativeInt(@Panel.FLastRect))) then
{$ENDIF}
    begin
    {$IfDef D4}
      if Panel.UseRightToLeftReading then
        FCanvas.TextFlags := FCanvas.TextFlags or ETO_RTLREADING
      else
        FCanvas.TextFlags := FCanvas.TextFlags and (not ETO_RTLREADING);
    {$EndIf D4}
      InflateRect(Panel.FLastRect, -1, -1);
      case Panel.PanelStyle of
        apsLockKey         : PaintLockKey(Panel);
        apsText, apsAppHint: PaintText(Panel);
        apsProgressBar     : PaintProgressBar(Panel);
      end;
    end;
  end;
end;{procedure TabfStatusBar.DrawPanel}

//------------------------------------------------------------------------------

procedure TabfStatusBar.PaintLockKey(Panel: TabfStatusPanel);
var
  OffsetLeft, OffsetTop, CaptionWidth: Integer;
begin
  with Panel.BitmapBuffer.Canvas, Panel.LockKeyStyle, Panel.FLastRect do
  begin
    if (Right - Left) >= 0 then Panel.BitmapBuffer.Width := Right - Left
    else Exit;

    if (Bottom - Top) >= 0 then Panel.BitmapBuffer.Height := Bottom - Top
    else Exit;

    Brush.Assign(FCanvas.Brush);
    SetBKColor(Handle, ColorToRGB(Self.Color));

    with Panel.FLastRect do
      FillRect(Bounds(0, 0, Right - Left, Bottom - Top));

    Panel.BitmapBuffer.Canvas.Font.Assign(Panel.Font);

    if FKeyLockStateOn then Panel.BitmapBuffer.Canvas.Font.Color := ColorOn
    else Panel.BitmapBuffer.Canvas.Font.Color := ColorOff;

    Brush.Style := bsClear;
    OffsetTop := (Panel.BitmapBuffer.Height -
      Panel.BitmapBuffer.Canvas.TextHeight(FCaption) + 1) div 2;
    CaptionWidth := Panel.BitmapBuffer.Canvas.TextWidth(FCaption);
    OffsetLeft := (Panel.Width - CaptionWidth - 3) div 2;
    TextRect(Bounds(0, 0, Panel.BitmapBuffer.Width, Panel.BitmapBuffer.Height),
      OffsetLeft, OffsetTop, FCaption);
  end;

{$IfDef D7}
  if ThemeServices.ThemesEnabled then
  begin
    Panel.BitmapBuffer.TransparentColor := Color;
    Panel.BitmapBuffer.Transparent := True;
  end;
{$EndIf D7}

  with Panel.FLastRect do
    if Assigned(FCanvas) then FCanvas.Draw(Left, Top, Panel.BitmapBuffer);
end;{procedure TabfStatusBar.PaintLockKey}

//------------------------------------------------------------------------------

procedure TabfStatusBar.PaintProgressBar(Panel: TabfStatusPanel);
var
  ProgressRect: TRect;
  TempProgress: Integer;
  TempStr: string;
begin
  ProgressRect := Panel.FLastRect;

  with Panel, Panel.ProgressBarStyle, Panel.FLastRect do
  begin
    TempProgress := ((Progress - MinValue) * 100) div (MaxValue - MinValue);
    ProgressRect.Right := Left + LongInt(Trunc((Width - 4) * TempProgress * 0.01));
    try
      case ProgressType of
        aptPercent: TempStr := Format(FCaption, [TempProgress]);
        aptCount: TempStr := Format(FCaption, [Progress - MinValue,
          MaxValue - MinValue]);
      else
        TempStr := '';
      end;
    except
      TempStr := '';
    end;
  end;

  with Panel.BitmapBuffer, Panel.BitmapBuffer.Canvas, Panel.FLastRect do
  begin
    if (Right - Left) >= 0 then Width  := Right - Left
    else Exit;
    if (Bottom - Top) >= 0 then Height := Bottom - Top
    else Exit;

    Brush.Style := bsSolid;
    Brush.Color := Self.Color;

    with Panel.FLastRect do
      FillRect(Bounds(ProgressRect.Right - ProgressRect.Left, 0,
        Right - Left, Bottom - Top));

    Brush.Color := Panel.BrushColor;
    Brush.Style := Panel.BrushStyle;

    with ProgressRect do
      FillRect(Bounds(0, 0, Right - Left, Bottom - Top));

//    SetBKColor(Handle, ColorToRGB(Self.Color));
    if Panel.FProgressBarStyle.FShowCaption then
    begin
      Brush.Style := bsClear;
      Font := Panel.Font;
      TextRect(Bounds(0, 0, Width, Height),
        (Panel.Width - TextWidth(TempStr) - 3) div 2,
        (Height - TextHeight(TempStr) + 1) div 2, TempStr);
    end;
  end;

{$IfDef D7}
  if ThemeServices.ThemesEnabled then
  begin
    Panel.BitmapBuffer.TransparentColor := Color;
    Panel.BitmapBuffer.Transparent := True;
  end;
{$EndIf D7}

  with Panel.FLastRect do
    if Assigned(FCanvas) then FCanvas.Draw(Left, Top, Panel.BitmapBuffer);
end;{procedure TabfStatusBar.PaintProgressBar}

//------------------------------------------------------------------------------

procedure TabfStatusBar.PaintText(Panel: TabfStatusPanel);
var
  TextLeft: Integer;
  TextTop: Integer;
begin
  with Panel.BitmapBuffer.Canvas, Panel.TextStyle, Panel.FLastRect do
  begin
    if (Right - Left) >= 0 then Panel.BitmapBuffer.Width  := Right - Left
    else Exit;
    if (Bottom - Top) >= 0 then Panel.BitmapBuffer.Height := Bottom - Top
    else Exit;
    Brush.Assign(FCanvas.Brush);
    SetBKColor(Handle, ColorToRGB(Self.Color));

    with Panel.FLastRect do
      FillRect(Bounds(0, 0, Right - Left, Bottom - Top));

    if FShowText then
    begin
      Panel.BitmapBuffer.Canvas.Font.Assign(Panel.Font);
      Brush.Style := bsClear;
      TextTop := (Panel.BitmapBuffer.Height -
        Panel.BitmapBuffer.Canvas.TextHeight(FText) + 1) div 2;

      if Assigned(FGlyphs) and (FGlyphIndex > -1) and
        (GlyphIndex <= (FGlyphs.Count - 1)) then
      begin
        FTextWidth := Panel.BitmapBuffer.Canvas.TextWidth(FText) +
          THackImageList(FGlyphs).Width + FSpacing;

        if FScrollEffect = seNone then
        begin
          if Alignment = taCenter then
            TextLeft := (Panel.BitmapBuffer.Width - FTextWidth) div 2
          else
          if Alignment = taLeftJustify then TextLeft := 2
          else TextLeft := Panel.BitmapBuffer.Width - FTextWidth - 2;
        end else TextLeft := 0;
        if FLayout = tlGlyphLeft then
        begin
          FGlyphs.Draw(Panel.BitmapBuffer.Canvas, TextLeft + FTextPosition,
            (Panel.BitmapBuffer.Height - THackImageList(FGlyphs).Height + 1) div 2,
            FGlyphIndex);
          TextRect(Bounds(0, 0, Panel.BitmapBuffer.Width, Panel.BitmapBuffer.Height),
            TextLeft + FTextPosition + THackImageList(FGlyphs).Width +
            FSpacing, TextTop, FText);
        end else
        begin
          TextRect(Bounds(0, 0, Panel.BitmapBuffer.Width, Panel.BitmapBuffer.Height),
            TextLeft + FTextPosition, TextTop, FText);
          FGlyphs.Draw(Panel.BitmapBuffer.Canvas, TextLeft + FTextPosition +
            FTextWidth - THackImageList(FGlyphs).Width,
            (Panel.BitmapBuffer.Height - THackImageList(FGlyphs).Height + 1) div 2,
            FGlyphIndex);
        end;

      end else
      begin
        FTextWidth := Panel.BitmapBuffer.Canvas.TextWidth(FText);

        if FScrollEffect = seNone then
        begin
          if Alignment = taCenter then
            TextLeft := (Panel.BitmapBuffer.Width - FTextWidth) div 2
          else
          if Alignment = taLeftJustify then TextLeft := 2
          else TextLeft := Panel.BitmapBuffer.Width - FTextWidth - 2;
        end else
          TextLeft := 0;

        TextRect(Bounds(0, 0, Panel.BitmapBuffer.Width, Panel.BitmapBuffer.Height),
          TextLeft + FTextPosition, TextTop, FText);
      end;
    end;
  end;{with Panel.BitmapBuffer.Canvas, Panel.TextStyle, Panel.FLastRect do}

{$IfDef D7}
  if ThemeServices.ThemesEnabled then
  begin
    Panel.BitmapBuffer.TransparentColor := Color;
    Panel.BitmapBuffer.Transparent := True;
  end;
{$EndIf D7}

  with Panel.FLastRect do
    if Assigned(FCanvas) then FCanvas.Draw(Left, Top, Panel.BitmapBuffer);
end;{procedure TabfStatusBar.PaintText}


//------------------------------------------------------------------------------

procedure TabfStatusBar.CheckLockKeyPanels;
var
  i: Integer;
begin
  for i := 0 to Panels.Count - 1 do
  if Panels[i].PanelStyle = apsLockKey then
    with Panels[i].LockKeyStyle do
    begin
      case LockKeyType of
        lktNumLock   : ChangeLockState(GetKeyState(VK_NUMLOCK) = 1);
        lktCapsLock  : ChangeLockState(GetKeyState(VK_CAPITAL) = 1);
        lktScrollLock: ChangeLockState(GetKeyState(VK_SCROLL) = 1);
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CheckScrollPanels;
var
  i: Integer;
begin
  if not Assigned(FTimerThread) then
  begin
    for i := 0 to Panels.Count - 1 do
    if (Panels[i].FPanelStyle = apsText) and
      (Panels[i].FTextStyle.FScrollEffect <> seNone) and
      Panels[i].FTextStyle.FShowText then
    begin
      FTimerThread := TTimerThread.Create(Self, True);
      Exit;
    end
  end else
  begin
    i := 0;
    while (i < Panels.Count) and ((Panels[i].FPanelStyle <> apsText) or
      (Panels[i].FTextStyle.FScrollEffect = seNone) or
      (not Panels[i].FTextStyle.FShowText)) do Inc(i);
    if i = Panels.Count then
    begin
      TTimerThread(FTimerThread).FOwner := nil;
      while FTimerThread.Suspended do
        FTimerThread.Resume;
      FTimerThread.Terminate;
      FTimerThread := nil;
      Invalidate;
    end
  end;
end;{procedure TabfStatusBar.CheckScrollPanels}

//------------------------------------------------------------------------------

function TabfStatusBar.DoHint: Boolean;
begin
  if Assigned(FOnHint) then
  begin
    FOnHint(Self);
    Result := True;
  end else
    Result := False;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.DoScrollPanels;
var
  i: Integer;
begin
//  Lock;
//  try
    for i := 0 to Panels.Count - 1 do
      if Panels[i].DoScroll then
        InvalidateRect(Handle, @Panels[i].FLastRect, False);
//  finally
//    Unlock;
//  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FCurrentPanel := PanelAtPos(Point(X, Y));
  if Assigned(FCurrentPanel) and Assigned(FOnPanelDblClick) and
    (ssDouble in Shift) then
    FOnPanelDblClick(FCurrentPanel, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FCurrentPanel := PanelAtPos(Point(X, Y));
  if Assigned(FCurrentPanel) and Assigned(FOnPanelClick) then
    FOnPanelClick(FCurrentPanel, Button, Shift, X, Y);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.MouseMove(Shift: TShiftState; X, Y: Integer);
{$IfDef D3}
var
  OldPanel: TabfStatusPanel;
{$EndIf D3}
begin
{$IfDef D3}
  if ShowHint then
  begin
    OldPanel := FCurrentPanel;
    FCurrentPanel := PanelAtPos(Point(X, Y));
    if OldPanel <> FCurrentPanel then Application.CancelHint;
  end;
{$EndIf D3}
  inherited;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.GlyphsChange(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.SyncToSystemFont;
var
  NonClientMetrics: TNonClientMetrics;
  i: Integer;
begin
  if FUseSystemFont then
  begin
    NonClientMetrics.cbSize := sizeof(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
      Font.Handle := CreateFontIndirect(NonClientMetrics.lfStatusFont);
    for i := 0 to Panels.Count - 1 do
    if Panels[i].ParentFont then
    begin
      Panels[i].Font.OnChange := nil;
      try
        Panels[i].Font.Assign(Font);
      finally
        Panels[i].Font.OnChange := Panels[i].FontChanged;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.UpdatePanel(Index: Integer; Repaint: Boolean);
var
  Flags: Integer;
  S: string;
begin
  if not HandleAllocated then Exit;
  with Panels[Index] do
  begin
    if not Repaint then
    begin
      FUpdateNeeded := True;
{$IFDEF WIN32}
      SendMessage(Handle, SB_GETRECT, Index, Integer(@FLastRect));
      InvalidateRect(Handle, @FLastRect, True);
{$ENDIF}
{$IFDEF WIN64}
      SendMessage(Handle, SB_GETRECT, Index, NativeInt(@FLastRect));
      InvalidateRect(Handle, @FLastRect, True);
{$ENDIF}
      Exit;
    end else
    if not FUpdateNeeded then Exit;

    FUpdateNeeded := False;
    Flags := 0;
    case Bevel of
      apbNone: Flags := SBT_NOBORDERS;
      apbRaised: Flags := SBT_POPOUT;
    end;

{$IfDef D4}
    if UseRightToLeftReading then Flags := Flags or SBT_RTLREADING;
{$EndIf D4}
    Flags := Flags or SBT_OWNERDRAW; // ownerdraw forever!!!
    S := TextStyle.Text;
{$IfDef D4}
    if UseRightToLeftAlignment then
      DoRightToLeftAlignment(S, TextStyle.Alignment, UseRightToLeftAlignment)
    else
      case TextStyle.Alignment of
        taCenter: Insert(#9, S, 1);
        taRightJustify: Insert(#9#9, S, 1);
      end;
{$EndIf D4}

{$IFDEF WIN32}
    SendMessage(Handle, SB_SETTEXT, Index or Flags, Integer(PChar(S)));
{$ENDIF}
{$IFDEF WIN64}
    SendMessage(Handle, SB_SETTEXT, Index or Flags, NativeInt(PChar(S)));
{$ENDIF}

  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.UpdatePanels(UpdateRects, UpdateText: Boolean);
const
  MaxPanelCount = 128;
var
  i, Count, PanelPos: Integer;
  PanelEdges: array[0..MaxPanelCount - 1] of Integer;
begin
  if not HandleAllocated then Exit;

  Count := Panels.Count;
  if UpdateRects then
  begin
    if Count > MaxPanelCount then Count := MaxPanelCount;
    if Count = 0 then
    begin
      PanelEdges[0] := -1;
{$IFDEF WIN32}
      SendMessage(Handle, SB_SETPARTS, 1, Integer(@PanelEdges));
      SendMessage(Handle, SB_SETTEXT, 0, Integer(PChar('')));
{$ENDIF}
{$IFDEF WIN64}
      SendMessage(Handle, SB_SETPARTS, 1, NativeInt(@PanelEdges));
      SendMessage(Handle, SB_SETTEXT, 0, NativeInt(PChar('')));
{$ENDIF}
    end else
    begin
      PanelPos := 0;
      for i := 0 to Count - 2 do
      begin
        Inc(PanelPos, Panels[i].Width);
        PanelEdges[i] := PanelPos;
      end;
      PanelEdges[Count - 1] := -1;
{$IFDEF WIN32}
      SendMessage(Handle, SB_SETPARTS, Count, Integer(@PanelEdges));
{$ENDIF}
{$IFDEF WIN64}
      SendMessage(Handle, SB_SETPARTS, Count, NativeInt(@PanelEdges));
{$ENDIF}
    end;
  end;

//{$IfDef D3}
//  FCanvas.Lock;
//  try
//{$EndIf}
  for i := 0 to Count - 1 do
    UpdatePanel(i, UpdateText);
//{$IfDef D3}
//  finally
//    FCanvas.UnLock;
//  end;
//{$EndIf}
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.UpdateSimpleText;
{$IfDef D4}
const
  RTLReading: array[Boolean] of Longint = (0, SBT_RTLREADING);
begin
  DoRightToLeftAlignment(FSimpleText, taLeftJustify, UseRightToLeftAlignment);
{$Else D4}
begin
{$EndIf D4}
  if not HandleAllocated then Exit;
  FCanvas.Brush.Color := Color;
  FCanvas.Brush.Style := bsSolid;
{$IFDEF WIN32}
  SendMessage(Handle, SB_SETTEXT, 255
   {$IfDef D4}or RTLREADING[UseRightToLeftReading]{$EndIf},
   Integer(PChar(FSimpleText)));
{$ENDIF}
{$IFDEF WIN64}
  SendMessage(Handle, SB_SETTEXT, 255
   {$IfDef D4}or RTLREADING[UseRightToLeftReading]{$EndIf},
   NativeInt(PChar(FSimpleText)));
{$ENDIF}
end;

//------------------------------------------------------------------------------
{$IfDef D4}

procedure TabfStatusBar.DoRightToLeftAlignment(var Str: string;
  AAlignment: TAlignment; ARTLAlignment: Boolean);
begin
  if ARTLAlignment then ChangeBiDiModeAlignment(AAlignment);
  case AAlignment of
    taCenter: Insert(#9, Str, 1);
    taRightJustify: Insert(#9#9, Str, 1);
  end;
end;

{$EndIf D4}

//==============================================================================
// Properties Get/Set

procedure TabfStatusBar.SetGlyphs(const Value: TCustomImageList);
begin
  if FGlyphs <> nil then
    FGlyphs.UnRegisterChanges(FGlyphsChangeLink);
  FGlyphs := Value;
  if FGlyphs <> nil then
  begin
    FGlyphs.RegisterChanges(FGlyphsChangeLink);
    FGlyphs.FreeNotification(Self);
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.SetPanels(const Value: TabfStatusPanels);
begin
  FPanels.Assign(Value);
end;

//------------------------------------------------------------------------------

function TabfStatusBar.GetPanelValue(const PanelName: string): TabfStatusPanel;
begin
  Result := PanelByName(PanelName);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.SetPanelValue(const PanelName: string; const Value: TabfStatusPanel);
begin
  PanelByName(PanelName).Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.SetSimplePanel(Value: Boolean);
begin
  if FSimplePanel <> Value then
  begin
    FSimplePanel := Value;
    if HandleAllocated then
      SendMessage(Handle, SB_SIMPLE, Ord(FSimplePanel), 0);
    if FSimplePanel then UpdateSimpleText else UpdatePanels(True, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.SetSimpleText(const Value: string);
begin
  if FSimpleText <> Value then
  begin
    FSimpleText := Value;
    if FSimplePanel then UpdateSimpleText;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.SetSizeGrip(Value: Boolean);
begin
  if FSizeGrip <> Value then
  begin
    FSizeGrip := Value;
    RecreateWnd;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.SetUseSystemFont(Value: Boolean);
begin
  if FUseSystemFont <> Value then
  begin
    FUseSystemFont := Value;
    if Value then
    begin
      if ParentFont then ParentFont := False;
      SyncToSystemFont;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TabfStatusBar.IsFontStored: Boolean;
begin
  Result := not FUseSystemFont and not ParentFont
    {$IfDef D3} and not DesktopFont{$EndIf};
end;

//==============================================================================
// Messages routines

{$IfDef D4}

procedure TabfStatusBar.CMBiDiModeChanged(var Message: TMessage);
var
  i: Integer;
begin
  inherited;
  if not HandleAllocated then Exit;
  if not SimplePanel then
  begin
    for i := 0 to Panels.Count - 1 do
      if Panels[i].ParentBiDiMode then Panels[i].ParentBiDiModeChanged;
    UpdatePanels(True, True);
  end else
    UpdateSimpleText;
end;

{$EndIf D4}

//------------------------------------------------------------------------------

procedure TabfStatusBar.CMColorChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CMHintShow(var Message: TMessage);
var
  HintInfo: ^THintInfo;
begin
  inherited;
{$IFDEF WIN32}
  Integer(HintInfo) := Message.LParam;
{$ENDIF}
{$IFDEF WIN64}
  NativeInt(HintInfo) := Message.LParam;
{$ENDIF}
{$IfNDef D3}
  HintInfo^.HintPos := ClientToScreen(HintInfo^.CursorPos);
  HintInfo^.HintPos.y := HintInfo^.HintPos.y + HintInfo^.CursorRect.Bottom -
    HintInfo^.CursorRect.Top;
{$Else D3}
  if not Assigned(FCurrentPanel) then
    FCurrentPanel := PanelAtPos(HintInfo^.CursorPos);
  if Assigned(FCurrentPanel) and (FCurrentPanel.Hint <> '') then
  begin
    if not (csDesigning in ComponentState) then if not _ASK then _TADA;
    HintInfo^.HintStr := FCurrentPanel.Hint
  end else
    HintInfo^.HintStr := Hint;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CMParentColorChanged(var Message: TMessage);
var i: Integer;
begin
  inherited;
  if ParentColor then
  for i:= 0 to Panels.Count - 1 do
  if Panels[i].ParentColor then
  begin
    Panels[i].FBrushColor := Brush.Color;
    Panels[i].Changed(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CMParentFontChanged(var Message: TMessage);
var i: Integer;
begin
  inherited;
  if ParentFont then
  begin
    if FUseSystemFont then FUseSystemFont := False;
    for i := 0 to Panels.Count - 1 do
      if Panels[i].ParentFont then
      begin
        Panels[i].Font.OnChange := nil;
        try
          Panels[i].Font.Assign(Font);
        finally
          Panels[i].Font.OnChange := Panels[i].FontChanged;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CMSysColorChange(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  inherited;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CMSysFontChanged(var Message: TMessage);
begin
  inherited;
  SyncToSystemFont;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CMWinIniChange(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  inherited;
  if (Message.WParam = 0) or (Message.WParam = SPI_SETNONCLIENTMETRICS) then
    SyncToSystemFont;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.CNDrawItem(var Message: TWMDrawItem);
var
  SaveIndex: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    SaveIndex := SaveDC(hDC);
    with FCanvas do
    try
      Lock;
      try
        Handle := hDC;
        Font.Assign(Panels[itemID].Font);
        Brush.Color := Panels[itemID].BrushColor;
        Brush.Style := Panels[itemID].BrushStyle;
//        SetBKColor(Handle, ColorToRGB(Color));
        DrawPanel(Panels[itemID], rcItem);
      finally
        Handle := 0;
        Unlock;
      end;
    finally
      RestoreDC(hDC, SaveIndex);
    end;
  end;
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.WMGetTextLength(var Message: TWMGetTextLength);
begin
  Message.Result := Length(FSimpleText);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.WMEraseBkGnd(var Message: TWMEraseBkGnd);
{$IfDef D7}
var
  Details: TThemedElementDetails;
{$EndIf D7}
begin

{$IfDef D7}
  if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tsStatusRoot);
    ThemeServices.DrawElement(Message.DC, Details, ClientRect, nil);
    Message.Result := 1;
    Exit;
  end else
{$EndIf D7}
    inherited;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.WMPaint(var Message: TWMPaint);
begin
  inherited;
  UpdatePanels(False, True);
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.WMSize(var Message: TWMSize);
begin
// Eat WM_SIZE message to prevent control from doing alignment
  if not (csLoading in ComponentState) then Resize;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBar.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if (Message.TimerID = 0) and (not (csReading in ComponentState)) then
    CheckLockKeyPanels;
end;


//==============================================================================
// TabfStatusBarInserter
//==============================================================================
// TabfStatusBarInserter is a non-visual component that allows inserting of any
// control (panels, buttons, images, etc.) into any panel of the TStatusBar or
// TabfStatusBar control. You can create status-bars not only static using this
// component.
// Date: 05/01/2000
{ TabfStatusBarInserter }

constructor TabfStatusBarInserter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FPanelIndex := 0;
  ResetResizingFlags;
end;

//------------------------------------------------------------------------------

destructor TabfStatusBarInserter.Destroy;
begin
  if Assigned(FControl) then
  begin
    FControl.Parent := FControlParent;
    FControl.BoundsRect := FControlBoundsRect;
  end;
  if Assigned(FStatusBar) then FStatusBar.OnDrawPanel := FOldOnDrawPanel;
  if Assigned(FabfStatusBar) then
    FabfStatusBar.OnDrawPanel := FOldOnAbfDrawPanel;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.Refresh;
begin
  if Assigned(FStatusBar   ) then FStatusBar   .Refresh;
  if Assigned(FabfStatusBar) then FabfStatusBar.Refresh;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = Self then
    begin
    // Set events back
      StatusBar    := nil;
      abfStatusBar := nil;
    end else
    if AComponent = FControl      then Control      := nil else
    if AComponent = FStatusBar    then StatusBar    := nil else
    if AComponent = FabfStatusBar then abfStatusBar := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.OnDrawPanel(AStatusBar: TStatusBar;
  APanel: TStatusPanel; const ARect: TRect);
var
  AWidth, AHeight: Integer;
begin
  if (FPanelIndex <> APanel.Index) or (not Assigned(FControl)) then
  begin
    if Assigned(FOldOnDrawPanel) then
      FOldOnDrawPanel(AStatusBar, APanel, ARect);
    Exit;
  end;

// Update control bounds
  with ARect do
  begin
    AWidth  := Right - Left;
    AHeight := Bottom - Top;
    if (APanel.Index = AStatusBar.Panels.Count - 1) and AStatusBar.SizeGrip then
      AWidth := AWidth - 13;
    if FControl.Left <> Left then FControl.Left := Left;
    if FControl.Top  <> Top  then FControl.Top  := Top;
    if not FNoResizeX then FControl.Width  := AWidth;
    if not FNoResizeY then FControl.Height := AHeight;
    if (FControl.Width  <> AWidth ) then FNoResizeX := True;
    if (FControl.Height <> AHeight) then FNoResizeY := True;
    if (FControl.Width <= 0) or (FControl.Height <= 0) then ResetResizingFlags;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.OnAbfDrawPanel(AStatusBar: TabfStatusBar;
  APanel: TabfStatusPanel; const ARect: TRect);
var
  AWidth, AHeight: Integer;
begin
  if (FPanelIndex <> APanel.Index) or (not Assigned(FControl)) then
  begin
    if Assigned(FOldOnAbfDrawPanel) then
      FOldOnAbfDrawPanel(AStatusBar, APanel, ARect);
    Exit;
  end;

// Update control bounds
  with ARect do
  begin
    AWidth  := Right - Left;
    AHeight := Bottom - Top;
    if (APanel.Index = AStatusBar.Panels.Count - 1) and AStatusBar.SizeGrip then
      AWidth := AWidth - 13;
    if FControl.Left <> Left then FControl.Left := Left;
    if FControl.Top  <> Top  then FControl.Top  := Top;
    if not FNoResizeX then FControl.Width  := AWidth;
    if not FNoResizeY then FControl.Height := AHeight;
    if (FControl.Width  <> AWidth ) then FNoResizeX := True;
    if (FControl.Height <> AHeight) then FNoResizeY := True;
    if (FControl.Width <= 0) or (FControl.Height <= 0) then ResetResizingFlags;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.InsertControl;
begin
  if not Assigned(FControl) then Exit;
  ResetResizingFlags;
  FControlVisible := FControl.Visible;
  FControl.FreeNotification(Self);
  FControlParent := FControl.Parent;
  if (FControlParent is TStatusBar) or (FControlParent is TabfStatusBar) then
    FControlParent := FControlParent.Parent;
  FControlBoundsRect := FControl.BoundsRect;
  if Assigned(FStatusBar   ) then FControl.Parent := FStatusBar;
  if Assigned(FabfStatusBar) then FControl.Parent := FabfStatusBar;
  SetPanelStyle;
  FControl.Visible := True;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.RemoveControl;
begin
  if not Assigned(FControl) then Exit;
  FControl.Visible := FControlVisible;
  if (csDestroying in FControl.ComponentState) then Exit;
  FControl.Parent := FControlParent;
  FControl.BoundsRect := FControlBoundsRect;
  ResetResizingFlags;
  RestorePanelStyle;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.SetPanelStyle;
begin
// TStatusBar routine
  if Assigned(FStatusBar) and Assigned(FControl) then
  begin
    if FPanelIndex >= FStatusBar.Panels.Count then Exit;
    FPanelStyle := FStatusBar.Panels[FPanelIndex].Style;
    FStatusBar.Panels[FPanelIndex].Style := psOwnerDraw;
  end;

// TabfStatusBar routine
  if Assigned(FabfStatusBar) and Assigned(FControl) then
  begin
    if FPanelIndex >= FabfStatusBar.Panels.Count then Exit;
    FabfPanelStyle := FabfStatusBar.Panels[FPanelIndex].PanelStyle;
    FabfStatusBar.Panels[FPanelIndex].PanelStyle := apsOwnerDraw;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.RestorePanelStyle;
begin
// TStatusBar routine
  if Assigned(FStatusBar) and (FStatusBar.Panels.Count > FPanelIndex) then
    FStatusBar.Panels[FPanelIndex].Style := FPanelStyle;
// TabfStatusBar routine
  if Assigned(FabfStatusBar) and (FabfStatusBar.Panels.Count > FPanelIndex) then
    FabfStatusBar.Panels[FPanelIndex].PanelStyle := FabfPanelStyle;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.ResetResizingFlags;
begin
  FNoResizeX := False;
  FNoResizeY := False;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.SetControl(const A: TControl);
var
  i: Integer;
begin
// Check incomming parameter
  if FControl = A then Exit;
  if not Assigned(A) or (A is TForm) or
    (A is TStatusBar) or (A is TabfStatusBar) then
  begin
    RemoveControl;
    FControl := nil;
    Exit;
  end;
  RemoveControl;

// Remove same control from other TabfStatusBarInserter components
  for i := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[i] is TabfStatusBarInserter) then
      with TabfStatusBarInserter(Owner.Components[i]) do
        if Control = A then Control := nil;

// Assign new control
  FControl := A;
  if Assigned(FControl) then FControl.FreeNotification(Self);
  InsertControl;
  Refresh;
end;{procedure TabfStatusBarInserter.SetControl}

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.SetStatusBar(const A: TStatusBar);
begin
  if FStatusBar = A then Exit;

// Unhook current status bar
  if Assigned(FStatusBar) then
  begin
    FStatusBar.OnDrawPanel := FOldOnDrawPanel;
    RemoveControl;
  end;
  
// Assign new status bar and hook its OnDrawPanel event
  FStatusBar := A;
  if Assigned(FStatusBar) then
  begin
    abfStatusBar := nil;
    ResetResizingFlags;
    FOldOnDrawPanel := FStatusBar.OnDrawPanel;
    FStatusBar.OnDrawPanel := OnDrawPanel;
    FStatusBar.FreeNotification(Self);
    InsertControl;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.SetAbfStatusBar(const A: TabfStatusBar);
begin
  if FabfStatusBar = A then Exit;
  
// Unhook current status bar
  if Assigned(FabfStatusBar) then
  begin
    FabfStatusBar.OnDrawPanel := FOldOnAbfDrawPanel;
    RemoveControl;
  end;

// Assign new status bar and hook its OnDrawPanel event
  FabfStatusBar := A;
  if Assigned(FabfStatusBar) then
  begin
    StatusBar := nil;
    ResetResizingFlags;
    FOldOnAbfDrawPanel := FabfStatusBar.OnDrawPanel;
    FabfStatusBar.OnDrawPanel := OnAbfDrawPanel;
    FabfStatusBar.FreeNotification(Self);
    InsertControl;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfStatusBarInserter.SetPanelIndex(A: Integer);
begin
  if A < 0 then A := 0;
  if PanelIndex = A then Exit;
  FPanelIndex := A;
  ResetResizingFlags;
  SetPanelStyle;
  Refresh;
  if (PanelIndex > 3) and not (csDesigning in ComponentState) then
    if not _ASK then _TADA;
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfStatusBar, TabfStatusBarInserter]);{}

end{unit abfStatusBars}.

