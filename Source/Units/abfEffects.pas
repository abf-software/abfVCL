{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEffects;

{$I abf.inc}
{$Define UseThreadTimer}

interface

uses
{$IfDef D4}
  ActnList, ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ExtCtrls, Forms,
  abfClasses, abfGraphics,
  abfComponents, abfControls;

type

  TabfFormEffect = class;
  TabfCustomCredits = class;
  EabfCredits = class(EabfException);
  EabfFormEffect = class(EabfException);

//==============================================================================
// GraphicFill routines
//==============================================================================

//------------------------------------------------------------------------------
// GraphicFill render procedure type. GraphicFill renders should fill the Rect
// area on the Canvas with specified Colors. Use UserData to send customized
// data to the GraphicFill render procedure .

  TabfGraphicFillProc = procedure(const Canvas: TCanvas; const Rect: TRect;
    ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// TabfGraphicFill class used as a description of registerd GraphicFill routines
// in the abfGraphicFillList.

  TabfGraphicFill = class
  public
    Name: string;
    Proc: TabfGraphicFillProc;
    SizeDepended: Boolean;
    NeedBitmap: Boolean;
  end;


//==============================================================================
// TabfFormEffectRolling
//==============================================================================
// Class for the Rolling property of the TabfFormEffect component.

  TabfFormEffectRollingDelay = 1..10;

  TabfFormEffectRolling = class(TPersistent)
  private
    FOnMinMax: Boolean;
    FAnimate: Boolean;
    FHeight: Integer;
    FDelay: TabfFormEffectRollingDelay;
    procedure SetHeight(A: Integer);
  protected
    FOwner: TabfFormEffect;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TabfFormEffect); virtual;
    property Owner: TabfFormEffect read FOwner;
  published
    property Animate: Boolean read FAnimate write FAnimate default True;
    property Height: Integer read FHeight write SetHeight default 0;
    property OnMinMax: Boolean read FOnMinMax write FOnMinMax default True;
    property Delay: TabfFormEffectRollingDelay read FDelay write FDelay
      default 3;
  end;


//==============================================================================
// TabfFormEffectShaking
//==============================================================================
// Class for the Shaking property of the TabfFormEffect component.

  TabfFormEffectShaking = class(TPersistent)
  private
    FAmplitude: Cardinal;
    FInterval: Cardinal;
    FTime: Cardinal;
    procedure SetInterval(A: Cardinal);
    procedure SetTime(A: Cardinal);
  protected
    FOwner: TabfFormEffect;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TabfFormEffect); virtual;
    property Owner: TabfFormEffect read FOwner;
  published
    property Amplitude: Cardinal read FAmplitude write FAmplitude default 10;
    property Interval: Cardinal read FInterval write SetInterval default 25;
    property Time: Cardinal read FTime write SetTime default 500;
  end;

  
//==============================================================================
// TabfFormEffectAlphaBlending
//==============================================================================
// Class realizes AlphaBlending effect.

  TabfAlphaBlendingEffect = (abeFade, abeAppear);

  TabfFormEffectAlphaBlending = class(TPersistent)
  private
    FBeginTime: Cardinal;
    FCycled: Boolean;
    FInvertOnCycle: Boolean;
    FEffect: TabfAlphaBlendingEffect;
    FTime: Cardinal;
    FAlpha: Byte;
{$IfDef D6}
    FSaveAlpha: Byte;
    FSaveBlend: Boolean;
{$EndIf D6}
    FMinBlend: Byte;
    FMaxBlend: Byte;
    FAsync: Boolean;
    FOnBegin: TNotifyEvent;
    FOnEnd: TNotifyEvent;
  // Properties Get/Set
    procedure SetAlpha(Value: Byte);
    procedure SetEffect(Value: TabfAlphaBlendingEffect);
    procedure SetTime(Value: Cardinal);
  protected
    FOwner: TabfFormEffect;
    FThread: TabfThreadComponent;
    FExecuting: Boolean;
    FTerminated: Boolean;
    function GetOwner: TPersistent; override;
    procedure OnExecute(Sender: TObject);
  public
    constructor Create(AOwner: TabfFormEffect); virtual;
    destructor Destroy; override;
    procedure Execute;
    procedure Terminate;
  // Properties
    property Owner: TabfFormEffect read FOwner;
  // Events
    property OnBegin: TNotifyEvent read FOnBegin write FOnBegin;
    property OnEnd: TNotifyEvent read FOnEnd write FOnEnd;
  published
  // Properties
    property Async: Boolean read FAsync write FAsync default True;
    property Alpha: Byte read FAlpha write SetAlpha default 255;
    property Cycled: Boolean read FCycled write FCycled default False;
    property InvertOnCycle: Boolean read FInvertOnCycle write FInvertOnCycle
      default True;
    property MinAlpha: Byte read FMinBlend write FMinBlend default 100;
    property MaxAlpha: Byte read FMaxBlend write FMaxBlend default 255;
    property Effect: TabfAlphaBlendingEffect read FEffect write SetEffect
      default abeFade;
    property Time: Cardinal read FTime write SetTime default 1000;
  end;


//==============================================================================
// TabfFormEffect
//==============================================================================
// Use TabfFormEffect component or its descendants to hook a WndProc of the
// form where the component is placed. Provides additional features as AutoDrag,
// Rolling and Shaking effect. Rolling can be manual or Minimize/Maximize
// depended.

  TabfFormEffect = class(TabfCustomWndProcHook)
  private
    FOnRolling: TNotifyEvent;
    FOnUnRolling: TNotifyEvent;
    FOnShakingEnd: TNotifyEvent;
    FOnShakingBegin: TNotifyEvent;
    function GetOnAlphaBlendingBegin: TNotifyEvent;
    procedure SetOnAlphaBlendingBegin(const Value: TNotifyEvent);
    function GetOnAlphaBlendingEnd: TNotifyEvent;
    procedure SetOnAlphaBlendingEnd(const Value: TNotifyEvent);
  protected
    FOldHeight: Integer;
    FOldPos: TPoint;
    FAlphaBlending: TabfFormEffectAlphaBlending;
    FRolled: Boolean;
    FRolling: TabfFormEffectRolling;
    FShaked: Boolean;
    FShaking: TabfFormEffectShaking;
  // Hook routines
    procedure WndProc(var Message: TMessage); override;
  // Event handlers
    procedure DoRolling; dynamic;
    procedure DoUnRolling; dynamic;
    procedure DoShakingBegin; dynamic;
    procedure DoShakingEnd; dynamic;
  // Peoperties Get/Set
    procedure SetAlphaBlending(const Value: TabfFormEffectAlphaBlending);
    procedure SetRolled(Value: Boolean);
    procedure SetRolling(const Value: TabfFormEffectRolling);
    procedure SetShaked(Value: Boolean);
    procedure SetShaking(const Value: TabfFormEffectShaking);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetRolling; virtual;
    procedure ResetShaking; virtual;
    procedure AlphaBlendExecute; virtual;
    procedure AlphaBlendTerminate; virtual;
    procedure Roll(Up: Boolean); virtual;
    procedure Shake; virtual;
  // Properties
    property Rolled: Boolean read FRolled write SetRolled;
    property Shaked: Boolean read FShaked write SetShaked;
  published
  // Properties
    property About;
    property Active default True;
    property AutoDrag default True;
    property AlphaBlending: TabfFormEffectAlphaBlending read FAlphaBlending
      write SetAlphaBlending;
    property Rolling: TabfFormEffectRolling read FRolling write SetRolling;
    property Shaking: TabfFormEffectShaking read FShaking write SetShaking;
  // Events
    property OnMessageAfter;
    property OnMessageBefore;
    property OnAlphaBlendingBegin: TNotifyEvent read GetOnAlphaBlendingBegin
      write SetOnAlphaBlendingBegin;
    property OnAlphaBlendingEnd: TNotifyEvent read GetOnAlphaBlendingEnd
      write SetOnAlphaBlendingEnd;
    property OnRolling: TNotifyEvent read FOnRolling write FOnRolling;
    property OnUnRolling: TNotifyEvent read FOnUnRolling write FOnUnRolling;
    property OnShakingBegin: TNotifyEvent read FOnShakingBegin
      write FOnShakingBegin;
    property OnShakingEnd: TNotifyEvent read FOnShakingEnd
      write FOnShakingEnd;
  end;{TabfFormEffect = class(TabfCustomWndProcHook)}


//==============================================================================
// TabfCustomBackGround
//==============================================================================
// Prototype of graphic control for creating filled backgrounds.

  TabfBackGroundFillType = string;

  TabfCustomBackGround = class(TabfGraphicControl)
  private
    FFillColorLight: TColor;
    FFillColorDark: TColor;
    FFillCached: Boolean;
    FParentHookAllowed: Boolean;
    procedure FillBitmapChange(Sender: TObject);
  // Properties Get/Set
    procedure SetFillBitmap(const Value: TBitmap);
    function GetFillColorNormal: TColor;
  // Messages routines
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
  protected
    FNewParentWndProc, FOldParentWndProc: Pointer;
    FFillTypeIndex: Integer;
    FGraphicFill: TabfGraphicFill;
    FFillBitmap: TBitmap;
    FFillCacheBitmap: TBitmap;
    FParentHooked, FNeedRedraw: Boolean;
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
  // Parent hook routines
    procedure HookParent; virtual;
    procedure UnHookParent; virtual;
    procedure HookParentWndProc(var Message: TMessage); virtual;
  // Properties Get/Set
    procedure SetFillCached(Value: Boolean); virtual;
    procedure SetFillColorDark(Value: TColor); virtual;
    procedure SetFillColorLight(Value: TColor); virtual;
    procedure SetFillColorNormal(Value: TColor); virtual;
    function GetFillType: TabfBackGroundFillType; virtual;
    procedure SetFillType(const Value: TabfBackGroundFillType); virtual;
    procedure SetParentHookAllowed(Value: Boolean); virtual;
  // Properties
    property FillCached: Boolean read FFillCached write SetFillCached
      default False;
    property FillCacheBitmap: TBitmap read FFillCacheBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  // Fill routines
    procedure DrawFill(const ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure DrawFillCache(const ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure DrawFillCacheRect(const ACanvas: TCanvas;
      const ACanvasRect, ACacheRect: TRect); virtual;
    procedure FreeFillCache; virtual;
    procedure SaveFillCache; virtual;
  // Properties
    property Align default alClient;
    property FillBitmap: TBitmap read FFillBitmap write SetFillBitmap;
    property FillColorDark: TColor read FFillColorDark write SetFillColorDark
      default clBtnShadow;
    property FillColorLight: TColor read FFillColorLight
      write SetFillColorLight default clBtnHighLight;
    property FillColorNormal: TColor read GetFillColorNormal
      write SetFillColorNormal default clBtnFace;
    property FillType: TabfBackGroundFillType read GetFillType
      write SetFillType;
    property ParentHookAllowed: Boolean read FParentHookAllowed
      write SetParentHookAllowed default True; 
  end;{TabfCustomBackGround = class(TGraphicControl)}


//==============================================================================
// TabfBackGround
//==============================================================================
// Graphic control for creating filled backgrounds.

  TabfBackGround = class(TabfCustomBackGround)
  public
    property FillCacheBitmap;
  published
  // Properties
    property About;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FillBitmap;
    property FillCached;
    property FillColorDark;
    property FillColorLight;
    property FillColorNormal;
    property FillType;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  // Events 
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


//==============================================================================
// TabfCreditSection
//==============================================================================
// Item of Credits collection

  TabfCreditTextEffect = (cteNone, cteRaised, cteLowered);

  TabfCreditSection = class(TCollectionItem)
  private
    FHeader: string;
    FColCount: Integer;
    FTextExtent: TSize;
    FParentBodyFont: Boolean;
    FParentHeaderFont: Boolean;
    FColSpace: Integer;
    FParentColSpace: Boolean;
    FHeaderTextEffect: TabfCreditTextEffect;
    FBodyTextEffect: TabfCreditTextEffect;
    procedure OnChangeEvent(Sender: TObject);
  // Properties Get/Set
    procedure SetBody(const Value: TStrings);
    procedure SetHeader(const Value: string);
    procedure SetBodyFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetBodyTextEffect(Value: TabfCreditTextEffect);
    procedure SetHeaderTextEffect(Value: TabfCreditTextEffect);
    procedure SetColCount(Value: Integer);
    procedure SetColSpace(Value: Integer);
    procedure SetParentColSpace(Value: Boolean);
    procedure SetParentBodyFont(Value: Boolean);
    procedure SetParentHeaderFont(Value: Boolean);
    function IsColSpaceStored: Boolean;
    function IsBodyFontStored: Boolean;
    function IsHeaderFontStored: Boolean;
  protected
    FBody: TStrings;
    FBodyFont: TFont;
    FHeaderFont: TFont;
    procedure RenderText(const ACanvas: TCanvas; const ATop, AWidth: Integer);
    function TextExtent(const ACanvas: TCanvas): TSize;
  // Properties Get/Set
    function GetBodyItems(Index: Integer): string;
    procedure SetBodyItems(Index: Integer; const Value: string);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property BodyItems[Index: Integer]: string read GetBodyItems
      write SetBodyItems; default;
  published
    property Body: TStrings read FBody write SetBody;
    property BodyFont: TFont read FBodyFont write SetBodyFont
      stored IsBodyFontStored;
    property BodyTextEffect: TabfCreditTextEffect read FBodyTextEffect
      write SetBodyTextEffect default cteRaised;
    property ColCount: Integer read FColCount write SetColCount default 1;
    property ColSpace: Integer read FColSpace write SetColSpace
      stored IsColSpaceStored;
    property Header: string read FHeader write SetHeader;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont
      stored IsHeaderFontStored;
    property HeaderTextEffect: TabfCreditTextEffect read FHeaderTextEffect
      write SetHeaderTextEffect default cteRaised;
    property ParentColSpace: Boolean read FParentColSpace
      write SetParentColSpace default True;
    property ParentBodyFont: Boolean read FParentBodyFont
      write SetParentBodyFont default True;
    property ParentHeaderFont: Boolean read FParentHeaderFont
      write SetParentHeaderFont default True;
  end;{TabfCreditSection = class(TCollectionItem)}


//==============================================================================
// TabfCreditSections
//==============================================================================
// Credits collection

  TabfCreditSections = class(TCollection)
  private
    FHeaderFont: TFont;
    FBodyFont: TFont;
    FColSpace: Integer;
    FHeight: Integer;
    FWidth: Integer;
    FSectionSpace: Integer;
    procedure OnChangeEvent(Sender: TObject);
  // Properties Get/Set
    procedure SetBodyFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetColSpace(Value: Integer);
    procedure SetSectionSpace(Value: Integer);
    function IsBodyFontStored: Boolean;
    function IsHeaderFontStored: Boolean;
  protected
    FOwner: TPersistent;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  // Properties Get/Set
    function GetItems(Index: Integer): TabfCreditSection;
    procedure SetItems(Index: Integer; const Value: TabfCreditSection);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add: TabfCreditSection;
    procedure RenderText(const ACanvas: TCanvas; const ATop, AWidth: Integer);
    function TextExtent(const ACanvas: TCanvas): TSize;
    function Owner: TabfCustomCredits; reintroduce;
  public
    property BodyFont: TFont read FBodyFont write SetBodyFont
      stored IsBodyFontStored;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont
      stored IsHeaderFontStored;
    property ColSpace: Integer read FColSpace write SetColSpace default 0;
    property SectionSpace: Integer read FSectionSpace write SetSectionSpace
      default 0;
    property Items[Index: Integer]: TabfCreditSection read GetItems
      write SetItems; default;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;


//==============================================================================
// TabfCustomCredits
//==============================================================================
// Prototype of credits rendering control

  TabfCustomCredits = class(TabfCustomBackGround)
  private
    FPathFallFrom: Integer;
    FPathEnd: Integer;
    FPathDirection: TabfDirection;
    FPathBegin: Integer;
    FPathRiseTo: Integer;
    FCreditsPosition: Integer;
    FNeedSetPathProps: Boolean;
    FBackward: Boolean;
    FCycled: Boolean;
    FOnTimer: TNotifyEvent;
    FOnCreditsEnd: TNotifyEvent;
    procedure CreditsChange;
  // Properties Get/Set
    function  GetMaxPosition: Integer;
    function  GetMinPosition: Integer;
    function  GetCreditsBodyFont: TFont;
    procedure SetCreditsBodyFont(const Value: TFont);
    function  GetCreditsHeaderFont: TFont;
    procedure SetCreditsHeaderFont(const Value: TFont);
    function  GetCreditsSectionSpace: Integer;
    procedure SetCreditsSectionSpace(Value: Integer);
    procedure SetPathBegin(Value: Integer);
    procedure SetPathDirection(Value: TabfDirection);
    procedure SetPathEnd(Value: Integer);
    function  GetPathReverseOrder: Boolean;
    procedure SetPathFallFrom(Value: Integer);
    procedure SetPathRiseTo(Value: Integer);
    function  GetTimerEnabled: Boolean;
    procedure SetTimerEnabled(Value: Boolean);
    function  GetTimerInterval: Cardinal;
    procedure SetTimerInterval(Value: Cardinal);
  // Messages routines
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    FBufferBitmap, FCreditsBitmap: TBitmap;
    FCredits: TabfCreditSections;
{$IfDef UseThreadTimer}
    FTimer: TabfThreadTimer;
{$Else UseThreadTimer}
    FTimer: TabfCustomTimer;
{$EndIf UseThreadTimer}
    procedure Loaded; override;
    procedure ResetCredits; virtual;
    procedure UpdateSize; virtual;
    procedure UpdatePath; virtual;
    procedure OnTimerEvent(Sender: TObject); virtual;
    procedure DoCreditsEnd; dynamic;
  // Properties Get/Set
    procedure SetCredits(const Value: TabfCreditSections);
    procedure SetCreditsPosition(Value: Integer);
  // Properties
    property FillCached default True;
    property PathDirection: TabfDirection read FPathDirection
      write SetPathDirection default diVertical;
  // Events
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnCreditsEnd: TNotifyEvent read FOnCreditsEnd write FOnCreditsEnd;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawCredits; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Next; virtual;
    procedure Prev; virtual;
  // Properties
    property Backward: Boolean read FBackward write FBackward default False;
    property Credits: TabfCreditSections read FCredits write SetCredits;
    property CreditsBodyFont: TFont read GetCreditsBodyFont
      write SetCreditsBodyFont;
    property CreditsHeaderFont: TFont read GetCreditsHeaderFont
      write SetCreditsHeaderFont;
    property CreditsSectionSpace: Integer read GetCreditsSectionSpace
      write SetCreditsSectionSpace default 0;
    property CreditsPosition: Integer read FCreditsPosition
      write SetCreditsPosition;
    property Cycled: Boolean read FCycled write FCycled default True;
    property MaxPosition: Integer read GetMaxPosition;
    property MinPosition: Integer read GetMinPosition;
    property PathBegin: Integer read FPathBegin write SetPathBegin default 0;
    property PathEnd: Integer read FPathEnd write SetPathEnd default 0;
    property PathFallFrom: Integer read FPathFallFrom
      write SetPathFallFrom default 0;
    property PathReverseOrder: Boolean read GetPathReverseOrder;
    property PathRiseTo: Integer read FPathRiseTo
      write SetPathRiseTo default 0;
    property TimerEnabled: Boolean read GetTimerEnabled
      write SetTimerEnabled default True;
    property TimerInterval: Cardinal read GetTimerInterval
      write SetTimerInterval;
  end;{TabfCustomCredits = class(TabfCustomBackGround)}


//==============================================================================
// TabfCredits
//==============================================================================
// Credits rendering control

  TabfCredits = class(TabfCustomCredits)
  published
  // Properties
    property About;
    property Backward;
    property DragCursor;
    property Credits;
    property CreditsBodyFont;
    property CreditsHeaderFont;
    property CreditsSectionSpace;
    property CreditsPosition;
    property Cycled;
    property DragMode;
    property Enabled;
    property FillBitmap;
    property FillColorDark;
    property FillColorLight;
    property FillColorNormal;
    property FillType;
    property ParentShowHint;
    property PathBegin;  // Do not change order of PathXXX properties
    property PathEnd;
    property PathRiseTo;
    property PathFallFrom;
    property PopupMenu;
    property ShowHint;
    property TimerEnabled;
    property TimerInterval;
    property Visible;
  // Events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnTimer;
    property OnCreditsEnd;
  end;


//==============================================================================
// TabfMovablePanel
//==============================================================================
// Panel with movement ability.

  TabfMoveDirection = (drUp, drDown, drLeft, drRight);

  TabfMovablePanel = class(TPanel)
  private
    FAbout: string;
    FAutoBack: Boolean;
    FAutoRepeat: Boolean;
    FMoveDirection: TabfMoveDirection;
    FMoveFullTime: Cardinal;
    FMoveLength: Integer;
    FOnDoMove: TNotifyEvent;
    FOnUndoMove: TNotifyEvent;
    FOnMoveEnd: TNotifyEvent;
  protected
    FRealDirection: TabfMoveDirection; // Unchangeable in run-time flag
    FMovingNow, FToDirection: Boolean;
    FBeginTop, FBeginLeft: Integer;
    FTimeOfMoveBeginning: Cardinal;
{$IfDef UseThreadTimer}
    FTimer: TabfThreadTimer;
{$Else UseThreadTimer}
    FTimer: TabfCustomTimer;
{$EndIf UseThreadTimer}
    procedure Loaded; override;
  // Timer routines
    procedure PrepareDirection(AToDirection: Boolean); virtual;
    procedure OnTimerEvent(Sender: TObject); virtual;
  // Properties Get/Set
    function GetTimerInterval: Cardinal;
    procedure SetTimerInterval(Value: Cardinal); virtual;
    function GetMoved: Boolean;
    procedure SetMoveLength(Value: Integer); virtual;
    procedure SetMoveFullTime(Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Movement routines
    procedure InverseDirection; virtual;
    procedure DoMove; virtual;
    procedure UndoMove; virtual;
    procedure SetBeginPosition(X, Y: Integer); virtual;
  // Properties
    property Moved: Boolean read GetMoved;
    property BeginLeft: Integer read FBeginLeft;
    property BeginTop: Integer read FBeginTop;
  published
  // Properties
    property About: string read FAbout write FAbout stored False;
    property AutoBack: Boolean read FAutoBack write FAutoBack default False;
    property AutoRepeat: Boolean read FAutoRepeat write FAutoRepeat
      default False;
    property MoveDirection: TabfMoveDirection read FMoveDirection
      write FMoveDirection default drDown;
    property MoveFullTime: Cardinal read FMoveFullTime write SetMoveFullTime
      default 1000;
    property MoveLength: Integer read FMoveLength write SetMoveLength;
    property TimerInterval: Cardinal read GetTimerInterval write SetTimerInterval
      default 50;
  // Events
    property OnDoMove: TNotifyEvent read FOnDoMove write FOnDoMove;
    property OnUndoMove: TNotifyEvent read FOnUndoMove write FOnUndoMove;
    property OnMoveEnd: TNotifyEvent read FOnMoveEnd write FOnMoveEnd;
  end;{TabfMovablePanel = class(TPanel)}


//==============================================================================
// TabfMagnifier
//==============================================================================
// Magnignifying control. Works with desktop or any specified window.

{$IfNDef D4}

  PCursorInfo = ^TCursorInfo;
  tagCURSORINFO = packed record
    cbSize: DWORD;
    flags: DWORD;
    hCursor: HCURSOR;
    ptScreenPos: TPoint;
  end;
  TCursorInfo = tagCURSORINFO;
  
{$EndIf D4}

  TabfMagnifier = class(TabfGraphicControl)
  private
    FPainting: Boolean;
    FShowCursor: Boolean;
    FLargeCursor: Boolean;
  // Properties Get/Set
    procedure SetSourceWindow(Value: THandle);
    function GetTimerEnabled: Boolean;
    procedure SetTimerEnabled(Value: Boolean);
    function GetTimerInterval: Cardinal;
    procedure SetTimerInterval(Value: Cardinal);
    procedure SetZoomCoef(Value: Double);
  protected
    FTimer: TabfCustomTimer;
    FBuffer, FCursor, FDoubleCursor: TBitmap;
    FCursorInfo: TCursorInfo;
    FIconInfo: TIconInfo;
    FCursorPos, FInternalCursorPos: TPoint;
    FZoomCoef: Double;
    FSourceWindow: THandle;
    FSourceWindowRect: TRect;
    FDesktopCanvas: TCanvas;
    procedure OnTimer(Sender: TObject); virtual;
    procedure UpdateMagnifier; virtual;
    function PrepareCursor: Boolean; virtual;
    function DrawCursor(const ACanvas: TCanvas; const APos: TPoint;
      DoubleSize: Boolean): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property About;
    property Align;
    property Color;
    property Cursor;
    property ShowHint;
    property ParentShowHint;
    property ZoomCoef: Double read FZoomCoef write SetZoomCoef;
    property SourceWindow: THandle read FSourceWindow write SetSourceWindow;
    property ShowCursor: Boolean read FShowCursor write FShowCursor
      default True;
    property LargeCursor: Boolean read FLargeCursor write FLargeCursor
      default True;
    property TimerInterval: Cardinal read GetTimerInterval
      write SetTimerInterval default 200;
    property TimerEnabled: Boolean read GetTimerEnabled write SetTimerEnabled
      default True;
    property Visible;
{$IfDef D4}
    property Anchors;
    property Constraints;
{$EndIf D4}
  end;{TabfMagnifier = class(TabfGraphicControl)}


//==============================================================================
// GraphicFill routines
//==============================================================================

//==============================================================================
// GraphicFill routines
//==============================================================================
// Don't localize

const
  SabfGraphicFill_ColorFill     = '(ColorFill)';
  SabfGraphicFill_None          = '(None)';
  SabfGraphicFill_BitmapCenter  = '(BitmapCenter)';
  SabfGraphicFill_BitmapStretch = '(BitmapStretch)';
  SabfGraphicFill_BitmapTile    = '(BitmapTile)';
  SabfGraphicFill_GradientHorz  = 'GradientHorz';
  SabfGraphicFill_GradientVert  = 'GradientVert';
  SabfGraphicFill_GradientDiag1 = 'GradientDiag1';
  SabfGraphicFill_GradientDiag2 = 'GradientDiag2';
  SabfGraphicFill_Adobe         = 'Adobe';
  SabfGraphicFill_Alcatraz      = 'Alcatraz';
  SabfGraphicFill_ElPaso        = 'ElPaso';
  SabfGraphicFill_Hanoi         = 'Hanoi';
  SabfGraphicFill_Harvard       = 'Harvard';
  SabfGraphicFill_Holes         = 'Holes';
  SabfGraphicFill_Hollywood     = 'Hollywood';
  SabfGraphicFill_Honolulu      = 'Honolulu';
  SabfGraphicFill_London        = 'London';
  SabfGraphicFill_Manhattan     = 'Manhattan';
  SabfGraphicFill_Minsk         = 'Minsk';
  SabfGraphicFill_Orleans       = 'Orleans';
  SabfGraphicFill_Oshawa        = 'Oshawa';
  SabfGraphicFill_Philadelphia  = 'Philadelphia';
  SabfGraphicFill_Pittsburg     = 'Pittsburg';
  SabfGraphicFill_Sahara        = 'Sahara';

var
  abfGraphicFillList: TStrings; // List of registered GraphicFills

//------------------------------------------------------------------------------
// Registers GraphicFill with given Name.
procedure abfRegisterGraphicFill(const AName: string;
  AProc: TabfGraphicFillProc; ASizeDepended, ANeedBitmap: Boolean);

//------------------------------------------------------------------------------
// Returns GraphicFill procedure by Name
function abfGetGraphicFillProc(const AName: string): TabfGraphicFillProc;

//------------------------------------------------------------------------------
// Draws one Sand Grain on Canvas at the (X, Y) point with given colors.
procedure abfDrawGrain(const Canvas: TCanvas; X, Y: Integer;
  ColorLight, ColorDark: TColor);

//------------------------------------------------------------------------------
// Renders a sand on the Canvas in the given Rect with specified Colors. Specify
// Integer(GrainCount) as a caunt of sand grains should be drawn.
procedure abfDrawSand(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; GrainCount: Pointer);

//------------------------------------------------------------------------------
// Draws a hole (like a bubble) on the Canvas in the given Rect with specified
// Colors. UserData is not used.
procedure abfDrawHole(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws a steel brick (with 4 bolts) on the Canvas in the ARect area with
// specified colors. Use Integer(BoltSize) to specify the size of bolts
// (from 1 to 256, 2 by default)
procedure abfDrawSteelBrick(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; BoltSize: Pointer);

//------------------------------------------------------------------------------
// Draws two vertical boards on the Canvas in the Rect area.
procedure abfDrawVertBoards(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
  
//------------------------------------------------------------------------------
// Draws an Adobe styled (Bubbles on a the sand) fill on the Canvas in the ARect
// area with specified colors. Use Integer(HoleSize) to specify the size of
// holes (Bubbles) (from 1 to 256, 7 by default).
procedure abfGraphicFill_Adobe(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; HoleSize: Pointer);

//------------------------------------------------------------------------------
// Draws a steel bricks (each with 4 bolts) and vertical grid fill on the Canvas
// in the ARect area with specifiedcolors. Use Integer(BoltSize) to specify
// the size of bolts (from 1 to 256, 2 by default)
procedure abfGraphicFill_Alcatraz(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; BoltSize: Pointer);

//------------------------------------------------------------------------------
// Fills ARect area of the Canvas with ColorNormal and draws TBitmap(FillBitmap)
// in the center of ARect.
procedure abfGraphicFill_BitmapCenter(const ACanvas: TCanvas;
  const ARect: TRect; ColorNormal, ColorLight, ColorDark: TColor;
  FillBitmap: Pointer);

//------------------------------------------------------------------------------
// Stretches TBitmap(FillBitmap) to the ARect area of Canvas.
procedure abfGraphicFill_BitmapStretch(const ACanvas: TCanvas;
  const ARect: TRect; ColorNormal, ColorLight, ColorDark: TColor;
  FillBitmap: Pointer);

//------------------------------------------------------------------------------
// Fills the ARect area of Canvas by tiling of TBitmap(FillBitmap)
procedure abfGraphicFill_BitmapTile(const ACanvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; FillBitmap: Pointer);

//------------------------------------------------------------------------------
// Simply fills the ARect area of Canvas with ColorNormal
procedure abfGraphicFill_ColorFill(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws horizontal gradient in ARect area of the Canvas. ColorLight on left,
// ColorDark on right.
procedure abfGraphicFill_GradientHorz(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws vertival gradient in ARect area of the Canvas. ColorLight on top,
// ColorDark on bottom.
procedure abfGraphicFill_GradientVert(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws "ElPaso" (Boards with some points) fill on the Canvas in the ARect area
// with specified Colors.
procedure abfGraphicFill_ElPaso(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws "Hanoi" (Wood engraving boards) fill on the Canvas in the ARect area
// with specified Colors.
procedure abfGraphicFill_Hanoi(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws "school copy-book's paper" fill on the Canvas in the ARect area with
// specified Colors. Use Integer(ShadowSize) to specify a size of shadow
// (from 1 to 2048, 3 by default).
procedure abfGraphicFill_Harvard(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; ShadowSize: Pointer);

//------------------------------------------------------------------------------
// Draws Holes (Bubbles) fill on the Canvas in the ARect area with specified
// Colors. Use Integer(HoleSize) to specify the size of holes (Bubbles)
// (from 1 to 256, 7 by default).
procedure abfGraphicFill_Holes(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; HoleSize: Pointer);

//------------------------------------------------------------------------------
// Draws Chess Board fill on the Canvas in the ARect area with specified Colors.
procedure abfGraphicFill_Hollywood(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws X lines fill on the Canvas in the ARect area with specified Colors.
procedure abfGraphicFill_Honolulu(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws "Old Bricks" fill on the Canvas in the ARect area with specified
// Colors.
procedure abfGraphicFill_London(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws "New Bricks" fill on the Canvas in the ARect area with specified
// Colors.
procedure abfGraphicFill_Manhattan(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws Vertical Light/Dark bords fill on the Canvas in the ARect area with
// specified Colors.
procedure abfGraphicFill_Minsk(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Does not fill any part of Canvas.
procedure abfGraphicFill_None(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws Holes(Bubbles) with Vertical grid fill on the Canvas in the ARect area
// with specified Colors. Use Integer(HoleSize) to specify the size of holes
// (Bubbles) (from 1 to 256, 7 by default).
procedure abfGraphicFill_Orleans(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; HoleSize: Pointer);

//------------------------------------------------------------------------------
// Draws Vertical grid under the sand fill on the Canvas in the ARect area with
// specified Colors.
procedure abfGraphicFill_Oshawa(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

//------------------------------------------------------------------------------
// Draws "paper with shadow" styled fill on the Canvas in the ARect area with
// specified colors. Use Integer(ShadowSize) to specify a size of shadow
// (from 1 to 2048, 7 by default).
procedure abfGraphicFill_Philadelphia(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; ShadowSize: Pointer);

//------------------------------------------------------------------------------
// Draws a steel bricks (each with 4 bolts) fill on the Canvas in the ARect
// area with specified colors. Use Integer(BoltSize) to specify the size
// of bolts (from 1 to 256, 2 by default)
procedure abfGraphicFill_Pittsburg(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; BoltSize: Pointer);

//------------------------------------------------------------------------------
// Draws a Sahara sand fill on the Canvas in the ARect area with specified
// colors
procedure abfGraphicFill_Sahara(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);

{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfSysUtils, abfVclUtils,
  abfConsts,
  abfEffectsConsts;

{$I abf_init.inc}

//==============================================================================
// GraphicFill routines
//==============================================================================

const
  cFillSize = 30;
  cFillHalfSize = cFillSize div 2;
  cFillQuarterSize = cFillSize div 4;

//------------------------------------------------------------------------------
// Registers GraphicFill with given Name. Set SizeDepended True if Fill is
// Rect depended (Philadelphia, Harvard, etc.).

procedure abfRegisterGraphicFill(const AName: string;
  AProc: TabfGraphicFillProc; ASizeDepended, ANeedBitmap: Boolean);
var
  GraphicFill: TabfGraphicFill;
begin
  if not Assigned(AProc) then Exit;
  GraphicFill := TabfGraphicFill.Create;
  with GraphicFill do
  begin
    Name := AName;
    Proc := AProc;
    SizeDepended := ASizeDepended;
    NeedBitmap := ANeedBitmap;
  end;
  abfGraphicFillList.AddObject(AName, GraphicFill);
end;

//------------------------------------------------------------------------------
// Returns GraphicFill procedure by Name

function abfGetGraphicFillProc(const AName: string): TabfGraphicFillProc;
var
  i: Integer;
begin
  i := abfGraphicFillList.IndexOf(AName);
  if i >= 0 then
    Result := TabfGraphicFill(abfGraphicFillList.Objects[i]).Proc
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
// Clones the SrcRect area of the Src canvas in the DstRect area of the Dst
// canvas.

procedure _CloneFill(const Dst, Src: TCanvas; const DstRect, SrcRect: TRect);
var
  X, Y, Width, Height: Integer;
begin
  with Dst, DstRect do
  begin
    IntersectClipRect(Handle, Left, Top, Right, Bottom);
    X := Left;
    Y := Top;
    Width  := SrcRect.Right - SrcRect.Left;
    Height := SrcRect.Bottom - SrcRect.Top;
    while (Y < Bottom) do
    begin
      BitBlt(Handle, X, Y, Width, Height,
        Src.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY);
      Inc(X, Width);
      if (X > Right) then
      begin
        X := Left;
        Inc(Y, Height);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Clones the SrcRect area of the Src canvas in the DstRect area of the Dst
// canvas randomly.

procedure _RandomFill(const Dst, Src: TCanvas; const DstRect, SrcRect: TRect);
var
  i, X, Y, Width, Height, AllowedWidth, AllowedHeight: Integer;
begin
  with Dst, DstRect do
  begin
    IntersectClipRect(Handle, Left, Top, Right, Bottom);
    Width  := SrcRect.Right - SrcRect.Left;
    Height := SrcRect.Bottom - SrcRect.Top;
    AllowedWidth  := Right - Left;
    AllowedHeight := Bottom - Top;
    for i := 0 to (AllowedWidth * AllowedHeight div 225) do
    begin
      X := Left + Random(AllowedWidth);
      Y := Top  + Random(AllowedHeight);
      BitBlt(Handle, X, Y, Width, Height,
        Src.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY);
    end;
  end;
end;

//------------------------------------------------------------------------------
// Draws one Sand Grain on Canvas at the (X, Y) point with given colors.

procedure abfDrawGrain(const Canvas: TCanvas; X, Y: Integer;
  ColorLight, ColorDark: TColor);
begin
  with Canvas do
  begin
    MoveTo(X , Y);
    Pen.Color := ColorLight;
    LineTo(X + 1, Y + 1);
    Pen.Color := ColorDark;
    LineTo(X + 2, Y + 2);
  end;
end;

//------------------------------------------------------------------------------
// Renders a sand on the Canvas in the given Rect with specified Colors. Specify
// Integer(GrainCount) as a caunt of sand grains should be drawn.

procedure abfDrawSand(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; GrainCount: Pointer);
var
  i, X, Y, Width, Height: Integer;
begin
  with Rect do
  begin
    Width  := Right - Left - 1;
    Height := Bottom - Top - 1;
    for i := 1 to Integer(GrainCount) do
    begin
      X := Left + Random(Width);
      Y := Top + Random(Height);
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
      abfDrawGrain(Canvas, X, Y, ColorLight, ColorDark);
    end;
  end;
end;

//------------------------------------------------------------------------------
// Draws a hole (like a bubble) on the Canvas in the given Rect with specified
// Colors. UserData is not used.

procedure abfDrawHole(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
begin
  with Canvas, Rect do
  begin
  // BotomRight light shadow
    Pen.Color   := ColorLight;
    Brush.Style := bsClear;
    Ellipse(2 + Left, 2 + Top, Right , Bottom);
  // Normal space between Hole and BotomRight light shadow
    Pen.Color   := ColorNormal;
    Brush.Color := ColorNormal;
    Brush.Style := bsSolid;
    Ellipse(1 + Left, 1 + Top, Right - 1, Bottom - 1);
  // Hole itself
    Pen.Color   := ColorLight;
    Brush.Color := ColorDark;
    Ellipse(Left, Top, Right - 2, Bottom - 2);
  end;
end;

//------------------------------------------------------------------------------
// Draws a steel brick (with 4 bolts) on the Canvas in the ARect area with
// specified colors. Use Integer(BoltSize) to specify the size of bolts
// (from 1 to 256, 2 by default)

procedure abfDrawSteelBrick(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; BoltSize: Pointer);
var
  BS: Integer;
begin
  BS := Integer(BoltSize);
  if (BS < 1) or (BS > 256) then BS := 2;
  with Canvas, Rect do
  begin
  // Fill all area
    Brush.Color := ColorNormal;
    Canvas.FillRect(Rect);
  // TopLeft light edge
    Pen.Color := ColorLight;
    MoveTo(Right - 2, Top);
    LineTo(Left, Top);
    LineTo(Left, Bottom - 1);
  // BottomRight dark edge
    Pen.Color := ColorDark;
    MoveTo(Top + 1, Bottom - 1);
    LineTo(Right - 1, Bottom - 1);
    LineTo(Right - 1, Top);
  // 4 Bolts dark part
    Brush.Color := ColorDark;
    Ellipse(Left + 4, Top + 4, Left + 4 + BS, Top + 4 + BS);
    Ellipse(Left + 4, Bottom - 2 - BS, Left + 4 + BS, Bottom - 2);
    Ellipse(Right - 2 - BS, Top + 4, Right - 2, Top + 4 + BS);
    Ellipse(Right - 2 - BS, Bottom - 2 - BS, Right - 2, Bottom - 2);
  // 4 Bolts light part
    Pen  .Color := ColorLight;
    Brush.Color := ColorNormal;
    Ellipse(Left + 3, Top + 3, Left + 3 + BS, Top + 3 + BS);
    Ellipse(Left + 3, Bottom - 3 - BS, Left + 3 + BS, Bottom - 3);
    Ellipse(Right - 3 - BS, Top + 3, Right - 3, Top + 3 + BS);
    Ellipse(Right - 3 - BS, Bottom - 3 - BS, Right - 3, Bottom - 3);
  end;
end;

//------------------------------------------------------------------------------
// Draws two vertical boards on the Canvas in the Rect area.

procedure abfDrawVertBoards(const Canvas: TCanvas; const Rect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  HalfWidth: Integer;
begin
  with Canvas, Rect do
  begin
    HalfWidth := Left + (Right - Left) div 2;
  // Light part
    Pen.Color := ColorLight;
    MoveTo(Left + 1, Top);
    LineTo(Left + 1, Bottom);
    MoveTo(HalfWidth + 1, Top);
    LineTo(HalfWidth + 1, Bottom);
  // Dark part
    Pen.Color := ColorDark;
    MoveTo(Left, Top);
    LineTo(Left, Bottom);
    MoveTo(HalfWidth, Top);
    LineTo(HalfWidth, Bottom);
    MoveTo(Right, Top);
    LineTo(Right, Bottom);
  end;
end;

//------------------------------------------------------------------------------
// Draws an Adobe styled (Bubbles on a the sand) fill on the Canvas in the ARect
// area with specified colors. Use Integer(HoleSize) to specify the size of
// holes (Bubbles) (from 1 to 256, 7 by default).

procedure abfGraphicFill_Adobe(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; HoleSize: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
  HS: Integer;
begin
  HS := Integer(HoleSize);
  if (HS < 1) or (HS > 256) then HS := 7;
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for Sand cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      abfDrawSand(Canvas, R, ColorNormal, ColorLight, ColorDark, Pointer(100));
    end;
  // Fill Canvas by cloning of Bitmap with Sand
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  // Prepare Bitmap for Hole cloning
    with Bitmap do
    begin
      R := Rect(0, 0, HS, HS);
      Width  := HS;
      Height := HS;
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      abfDrawHole(Canvas, R, ColorNormal, ColorLight, ColorDark, nil);
    end;
  // Fill Canvas by cloning of Bitmap with Hole
    _RandomFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Adobe}

//------------------------------------------------------------------------------
// Draws a steel bricks (each with 4 bolts) and vertical grid fill on the Canvas
// in the ARect area with specifiedcolors. Use Integer(BoltSize) to specify
// the size of bolts (from 1 to 256, 2 by default)

procedure abfGraphicFill_Alcatraz(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; BoltSize: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      abfDrawSteelBrick(Canvas, R, ColorNormal, ColorLight, ColorDark, BoltSize);
      with Bitmap.Canvas do
      begin
      // Light part
        Pen.Color := ColorLight;
        MoveTo(7, 0);
        LineTo(7, cFillSize);
      // Normal part
        Brush.Color := ColorNormal;
        FillRect(Rect(8, 0, 11, cFillSize));
      // Dark part
        Pen  .Color := ColorDark;
        Brush.Color := ColorDark;
        MoveTo(6, 0);
        LineTo(6, cFillSize);
        MoveTo(11, 0);
        LineTo(11, cFillSize);
        FillRect(Rect(21, 0, 24, cFillSize));
      end;{with Bitmap.Canvas do}
    end;{with Bitmap do}
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Alcatraz}

//------------------------------------------------------------------------------
// Fills ARect area of the Canvas with ColorNormal and draws TBitmap(FillBitmap)
// in the center of ARect.

procedure abfGraphicFill_BitmapCenter(const ACanvas: TCanvas;
  const ARect: TRect; ColorNormal, ColorLight, ColorDark: TColor;
  FillBitmap: Pointer);
var
  X, Y: Integer;
begin
  ACanvas.Brush.Color := ColorNormal;
  if not Assigned(TBitmap(FillBitmap)) or TBitmap(FillBitmap).Empty then
  begin
    ACanvas.FillRect(ARect);
    Exit;
  end;

  with ARect, TBitmap(FillBitmap) do
  begin
    X := Left + (((Right - Left) - Width) div 2);
    Y := Top + (((Bottom - Top) - Height) div 2);
    if X >= 0 then // Equal fixes 1 pixel difference
    begin
    // Vertical empty space
      ACanvas.FillRect(Rect(Left, Top, X + 1, Bottom));
      ACanvas.FillRect(Rect(Right - X - 1, Top, Right, Bottom));
    end;
    if Y >= 0 then // Equal fixes 1 pixel difference
    begin
    // Horizontal empty space
      ACanvas.FillRect(Rect(Left, Top, Right, Y + 1));
      ACanvas.FillRect(Rect(Left, Bottom - Y - 1, Right, Bottom));
    end;
    ACanvas.Draw(X, Y, TBitmap(FillBitmap));
  end;
end;

//------------------------------------------------------------------------------
// Stretches TBitmap(FillBitmap) to the ARect area of Canvas.

procedure abfGraphicFill_BitmapStretch(const ACanvas: TCanvas;
  const ARect: TRect; ColorNormal, ColorLight, ColorDark: TColor;
  FillBitmap: Pointer);
begin
  ACanvas.Brush.Color := ColorNormal;
  if not Assigned(TBitmap(FillBitmap)) or TBitmap(FillBitmap).Empty then
  begin
    ACanvas.FillRect(ARect);
    Exit;
  end;
  
  with ARect, TBitmap(FillBitmap) do
    StretchBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
      Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
end;

//------------------------------------------------------------------------------
// Fills the ARect area of Canvas by tiling of TBitmap(FillBitmap)

procedure abfGraphicFill_BitmapTile(const ACanvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; FillBitmap: Pointer);
begin
  ACanvas.Brush.Color := ColorNormal;
  if not Assigned(TBitmap(FillBitmap)) or TBitmap(FillBitmap).Empty then
  begin
    ACanvas.FillRect(ARect);
    Exit;
  end;
  with TBitmap(FillBitmap) do
    _CloneFill(ACanvas, Canvas, ARect, Rect(0, 0, Width, Height));
end;

//------------------------------------------------------------------------------
// Simply fills the ARect area of Canvas with ColorNormal

procedure abfGraphicFill_ColorFill(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
begin
  Canvas.Brush.Color := ColorNormal;
  Canvas.FillRect(ARect);
end;

//------------------------------------------------------------------------------
// Draws horizontal gradient in ARect area of the Canvas. ColorLight on left,
// ColorDark on right.

procedure abfGraphicFill_GradientHorz(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
begin
  abfDrawGradient(Canvas, ARect, ColorLight, ColorDark, True);
end;

//------------------------------------------------------------------------------
// Draws vertival gradient in ARect area of the Canvas. ColorLight on top,
// ColorDark on bottom.

procedure abfGraphicFill_GradientVert(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
begin
  abfDrawGradient(Canvas, ARect, ColorLight, ColorDark, False);
end;

//------------------------------------------------------------------------------
// Draws "ElPaso" (Boards with some points) fill on the Canvas in the ARect area
// with specified Colors.

procedure abfGraphicFill_ElPaso(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
  i: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      for i := 0 to 4 do
      begin
        abfDrawGrain(Bitmap.Canvas, Random(cFillSize), Random(cFillSize),
          ColorLight, ColorDark);
        abfDrawGrain(Bitmap.Canvas, Random(cFillSize), Random(cFillSize),
          ColorDark, ColorLight);
      end;
      abfDrawVertBoards(Bitmap.Canvas, R, ColorNormal, ColorLight,
        ColorDark, nil);
    end;
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_ElPaso}

//------------------------------------------------------------------------------
// Draws "Hanoi" (Wood engraving boards) fill on the Canvas in the ARect area
// with specified Colors.

procedure abfGraphicFill_Hanoi(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
const
  cMaxPoints = 94;
  cPoints: array[0..cMaxPoints] of array[0..1] of Integer = (
    (0,0),   (9,0),   (1,10),  (2,11),  (3,12),  (4,13),  (4,14),  (4,15),
    (4,16),  (4,17),  (4,18),  (4,19),  (4,20),  (4,21),  (4,22),  (4,23),
    (3,24),  (2,25),  (1,26),  (0,27),  (0,28),  (0,29),  (0,30),  (0,27),
    (1,2),   (2,3),   (3,4),   (4,5),   (5,6),   (6,7),   (8,7),   (9,7),
    (10,6),  (11,5),  (12,4),  (13,3),  (14,2),  (15,1),  (16,0),  (17,0),
    (18,0),  (18,1),  (17,2),  (16,4),  (15,5),  (15,6),  (16,7),  (17,8),
    (18,9),  (19,10), (19,11), (19,12), (18,13), (17,14), (16,15), (16,16),
    (15,18), (14,20), (13,21), (13,22), (13,23), (13,24), (13,25), (13,26),
    (14,27), (15,28), (16,28), (16,27), (16,26), (17,25), (18,24), (19,23),
    (19,22), (19,21), (19,20), (19,19), (19,18), (20,17), (20,16), (21,15),
    (22,14), (23,13), (23,12), (23,11), (23,10), (23,9),  (23,8),  (23,7),
    (23,6),  (24,5),  (25,4),  (26,3),  (27,2),  (28,1),  (29,0));
var
  Bitmap: TBitmap;
  R: TRect;
  i: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      for i := 0 to cMaxPoints do
        abfDrawGrain(Bitmap.Canvas, cPoints[i][0], cPoints[i][1],
          ColorLight, ColorDark);
      abfDrawVertBoards(Bitmap.Canvas, R, ColorNormal, ColorLight,
        ColorDark, nil);
    end;
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Hanoi}

//------------------------------------------------------------------------------
// Draws "school copy-book's paper" fill on the Canvas in the ARect area with
// specified Colors. Use Integer(ShadowSize) to specify a size of shadow
// (from 1 to 2048, 3 by default).

procedure abfGraphicFill_Harvard(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; ShadowSize: Pointer);
var
  SS, X, Y, HoleSpace, HoleSize, Width, Height: Integer;
begin
  SS := Integer(ShadowSize);
  if (SS < 1) or (SS > 2048) then SS := 3;
  abfDrawWideBorder(Canvas, ARect, ColorNormal, 5);
  with Canvas, ARect do
  begin
    Width  := Right - Left;
    Height := Bottom - Top;
  // Corners
    Brush.Color := ColorNormal;
    FillRect(Rect(Left + 5, Height - 5 - SS, Left + 5 + SS, Height - 5));
    FillRect(Rect(Width - 5 - SS, Top + 5, Width - 5, Top + 5 + SS));
  // Sahdow
    Brush.Color := ColorDark;
    FillRect(Rect(Left + 5 + SS, Height - 5 - SS, Width - 5, Height - 5));
    FillRect(Rect(Width - 5 - SS, Top + 5 + SS, Width - 5, Height - 5));
  // "Paper"
    Pen  .Color := clBlack;
    Brush.Color := ColorLight;
    Rectangle(Left + 5, Top + 5, Width - 5 - SS, Height - 5 - SS);
  // Paper lines
    X := Left + 8;
    Y := Top + 40;
    while (Y < Height - 20 - SS) do
    begin
    // Blue horizontal lines
      Pen.Color := clBlue;
      MoveTo(X, Y);
      LineTo(Width - 7 - SS, Y);
    // Red vertical line
      Pen.Color := clRed;
      MoveTo(Left + Width div 8, Top + 7);
      LineTo(Left + Width div 8, Height - 7 - SS);
      Y := Y + 20;
    end;
  // 4 Holes
    HoleSpace := (Height - 10 - SS) div 5;
    HoleSize := Width div (16 * 4);
    X := Left + (Width div 16);
    Y := Top + 5 + (HoleSpace div 2);
    while (Y < Height - 5 - SS) do
    begin
      Brush.Color := ColorNormal;
      Pen.Color := ColorDark;
      Ellipse(X - 1, Y - 1, X + HoleSize, Y + HoleSize);
      Pen.Color := ColorLight;
      Ellipse(X, Y, X + HoleSize, Y + HoleSize);
      Y := Y + HoleSpace;
    end;
  end;{with Canvas, ARect do}
end;{procedure abfGraphicFill_Harvard}

//------------------------------------------------------------------------------
// Draws Holes (Bubbles) fill on the Canvas in the ARect area with specified
// Colors. Use Integer(HoleSize) to specify the size of holes (Bubbles)
// (from 1 to 256, 7 by default).

procedure abfGraphicFill_Holes(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; HoleSize: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
  HS: Integer;
begin
  HS := Integer(HoleSize);
  if (HS < 1) or (HS > 256) then HS := 7;
  Bitmap := TBitmap.Create;
  try
  // Prepare Bitmap for Hole cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := HS;
      Height := HS;
      R := Rect(0, 0, HS, HS);
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      abfDrawHole(Canvas, R, ColorNormal, ColorLight, ColorDark, nil);
    end;
  // Fill Canvas by cloning of Bitmap with Hole
    Canvas.Brush.Color := ColorNormal;
    Canvas.FillRect(ARect);
    _RandomFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Holes}

//------------------------------------------------------------------------------
// Draws Chess Board fill on the Canvas in the ARect area with specified Colors.

procedure abfGraphicFill_Hollywood(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
  // Prepare Bitmap with 4 cell of Chess Board
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      R := Rect(0, 0, cFillSize, cFillSize);
      with Canvas do
      begin
      // Dark cells
        Brush.Color := ColorNormal;
        Pen.Color := ColorLight;
        Rectangle(0, 0, cFillHalfSize, cFillHalfSize);
        Rectangle(cFillHalfSize, cFillHalfSize, cFillSize, cFillSize);
      // Light cells
        Brush.Color := ColorLight;
        Pen.Color := ColorNormal;
        Rectangle(0, cFillHalfSize, cFillHalfSize, cFillSize);
        Rectangle(cFillHalfSize, 0, cFillSize, cFillHalfSize);
      end;
    end;{with Bitmap do}
  // Fill Canvas by cloning of Bitmap with 4 cell of Chess Board
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
//    _RandomFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
// Draws X lines fill on the Canvas in the ARect area with specified Colors.

procedure abfGraphicFill_Honolulu(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
  // Prepare Bitmap with X lines
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      R := Rect(0, 0, cFillSize, cFillSize);
      with Canvas do
      begin
        Brush.Color := ColorNormal;
        FillRect(R);
        Pen.Color := ColorLight;
        MoveTo(0, cFillSize - 1);
        LineTo(cFillSize, -1);
        Pen.Color := ColorDark;
        MoveTo(0, 0);
        LineTo(cFillSize, cFillSize);
        MoveTo(0, cFillSize);
        LineTo(cFillSize, 0);
      end;
    end;{with Bitmap do}
  // Fill Canvas by cloning of Bitmap with X lines
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Honolulu}

//------------------------------------------------------------------------------
// Draws "Old Bricks" fill on the Canvas in the ARect area with specified
// Colors.

procedure abfGraphicFill_London(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      with Canvas do
      begin
        Brush.Color := ColorNormal;
        FillRect(R);
        abfDrawSand(Canvas, R, ColorNormal, ColorLight, ColorDark,
          Pointer(150));
      // Draw bricks edges
        Pen.Color := ColorLight;
        MoveTo(0, 0);
        LineTo(cFillSize, 0);
        Lineto(cFillSize, cFillHalfSize);
        LineTo(0, cFillHalfSize);
        LineTo(0, 0);
        MoveTo(cFillHalfSize, cFillHalfSize);
        LineTo(cFillHalfSize, cFillSize);
      end;
    end;{with Bitmap do}
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_London}

//------------------------------------------------------------------------------
// Draws "New Bricks" fill on the Canvas in the ARect area with specified
// Colors.

procedure abfGraphicFill_Manhattan(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
  i, Width, Height: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      with Canvas do
      begin
        Brush.Color := ColorNormal;
        FillRect(R);
      // Draw bricks edges
        Pen.Color := ColorLight;
        MoveTo(0, 0);
        LineTo(cFillSize, 0);
        Lineto(cFillSize, cFillHalfSize);
        LineTo(0, cFillHalfSize);
        LineTo(0, 0);
        MoveTo(cFillHalfSize, cFillHalfSize);
        LineTo(cFillHalfSize, cFillSize);
      end;
    end;{with Bitmap do}
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  // Draw some grains
    with ARect do
    begin
      Width  := Right - Left - 2; // - 2 for grain size
      Height := Bottom - Top - 2; // - 2 for grain size
      for i := 1 to (Right * Bottom div 225) div 3 do
      begin
        abfDrawGrain(Canvas, Left + Random(Width), Top + Random(Height),
          ColorLight, ColorDark);
        abfDrawGrain(Canvas, Left + Random(Width), Top + Random(Height),
          ColorDark, ColorLight);
      end;
    end;{with ARect do}
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Manhattan}

//------------------------------------------------------------------------------
// Draws Vertical Light/Dark bords fill on the Canvas in the ARect area with
// specified Colors.

procedure abfGraphicFill_Minsk(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      with Canvas do
      begin
      // Normal part
        Brush.Color := ColorNormal;
        FillRect(R);
      // Light part
        Pen.Color := ColorLight;
        Brush.Color := ColorLight;
        Rectangle(cFillQuarterSize, 0, cFillHalfSize, cFillSize);
        Rectangle(cFillHalfSize + cFillQuarterSize, 0, cFillSize, cFillSize);
      // Dark edges
        Pen.Color := ColorDark;
        MoveTo(cFillHalfSize - 1, 0);
        LineTo(cFillHalfSize - 1, cFillSize);
        MoveTo(cFillSize - 1, 0);
        LineTo(cFillSize - 1, cFillSize);
      end;
    end;{with Bitmap do}
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Minsk}

//------------------------------------------------------------------------------
// Does not fill any part of Canvas.

procedure abfGraphicFill_None(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
begin
  // Do nothing
end;

//------------------------------------------------------------------------------
// Draws Holes(Bubbles) with Vertical grid fill on the Canvas in the ARect area
// with specified Colors. Use Integer(HoleSize) to specify the size of holes
// (Bubbles) (from 1 to 256, 7 by default).

procedure abfGraphicFill_Orleans(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; HoleSize: Pointer);
const
  cDarkX : array[1..4] of Integer = (0, 14, 15, 29);
  cLightX: array[1..4] of Integer = (1, 13, 16, 28);
var
  Bitmap: TBitmap;
  R: TRect;
  i, HS, X: Integer;
begin
  HS := Integer(HoleSize);
  if (HS < 1) or (HS > 256) then HS := 7;
  Bitmap := TBitmap.Create;
  try
  // Prepare Bitmap with Hole for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := HS;
      Height := HS;
      R := Rect(0, 0, HS, HS);
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      abfDrawHole(Canvas, R, ColorNormal, ColorLight, ColorDark, nil);
    end;
  // Fill Canvas by cloning of Bitmap with Hole
    Canvas.Brush.Color := ColorNormal;
    Canvas.FillRect(ARect);
    _RandomFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
// Draw grid
  with Canvas, ARect do
  begin
    X := Left;
    while (X < Right) do
    begin
    // Dark lines
      Pen.Color := ColorDark;
      for i := Low(cDarkX) to High(cDarkX) do
      begin
        MoveTo(X + cDarkX[i], Top);
        LineTo(X + cDarkX[i], Bottom);
      end;
    // Light lines
      Pen.Color := ColorLight;
      for i := Low(cLightX) to High(cLightX) do
      begin
        MoveTo(X + cLightX[i], Top);
        LineTo(X + cLightX[i], Bottom);
      end;
      Inc(X, cFillSize);
    end;
  end;{with ARect do}
end;{procedure abfGraphicFill_Orleans}

//------------------------------------------------------------------------------
// Draws Vertical grid under the sand fill on the Canvas in the ARect area with
// specified Colors.

procedure abfGraphicFill_Oshawa(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Draw grid
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      abfDrawVertBoards(Bitmap.Canvas, R, ColorNormal, ColorLight,
        ColorDark, nil);
      abfDrawSand(Canvas, R, ColorNormal, ColorLight,
        ColorDark, Pointer(200))
    end;{with Bitmap do}
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Oshawa}

//------------------------------------------------------------------------------
// Draws "paper with shadow" styled fill on the Canvas in the ARect area with
// specified colors. Use Integer(ShadowSize) to specify a size of shadow
// (from 1 to 2048, 7 by default).

procedure abfGraphicFill_Philadelphia(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; ShadowSize: Pointer);
var
  SS, Width, Height: Integer;
begin
  SS := Integer(ShadowSize);
  if (SS < 1) or (SS > 2048) then SS := 7;
  abfDrawWideBorder(Canvas, ARect, ColorNormal, 5);
  with Canvas, ARect do
  begin
    Width  := Right - Left;
    Height := Bottom - Top;
  // Corners
    Brush.Color := ColorNormal;
    FillRect(Rect(Left + 5, Height - 5 - SS, Left + 5 + SS, Height - 5));
    FillRect(Rect(Width - 5 - SS, Top + 5, Width - 5, Top + 5 + SS));
  // Sahdow
    Brush.Color := ColorDark;
    FillRect(Rect(Left + 5 + SS, Height - 5 - SS, Width - 5, Height - 5));
    FillRect(Rect(Width - 5 - SS, Top + 5 + SS, Width - 5, Height - 5));
  // "Paper"
    Pen  .Color := clBlack;
    Brush.Color := ColorLight;
    Rectangle(Left + 5, Top + 5, Width - 5 - SS, Height - 5 - SS);
  end;
end;{procedure abfGraphicFill_Philadelphia}

//------------------------------------------------------------------------------
// Draws a steel bricks (each with 4 bolts) fill on the Canvas in the ARect
// area with specified colors. Use Integer(BoltSize) to specify the size
// of bolts (from 1 to 256, 2 by default)

procedure abfGraphicFill_Pittsburg(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; BoltSize: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      abfDrawSteelBrick(Canvas, R, ColorNormal, ColorLight, ColorDark, BoltSize);
    end;
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Pittsburg}

//------------------------------------------------------------------------------
// Draws a Sahara sand fill on the Canvas in the ARect area with specified
// colors

procedure abfGraphicFill_Sahara(const Canvas: TCanvas; const ARect: TRect;
  ColorNormal, ColorLight, ColorDark: TColor; UserData: Pointer);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    R := Rect(0, 0, cFillSize, cFillSize);
  // Prepare Bitmap for cloning
    with Bitmap do
    begin
      PixelFormat := pf24bit; // pf32bit;
      Width  := cFillSize;
      Height := cFillSize;
      Canvas.Brush.Color := ColorNormal;
      Canvas.FillRect(R);
      abfDrawSand(Canvas, R, ColorNormal, ColorLight, ColorDark, Pointer(150));
    end;
  // Fill Canvas by cloning of Bitmap
    _CloneFill(Canvas, Bitmap.Canvas, ARect, R);
    _RandomFill(Canvas, Bitmap.Canvas, ARect, R);
  finally
    Bitmap.Free;
  end;
end;{procedure abfGraphicFill_Sahara}

//------------------------------------------------------------------------------
// Creates abfGraphicFillList and registers default set of GraphicFills

procedure _RegisterGraphicFills;
begin
  abfGraphicFillList := TStringList.Create;
  TStringList(abfGraphicFillList).Sorted := True;
// Register defaiult set of GraphicFills
  abfRegisterGraphicFill(SabfGraphicFill_ColorFill    , abfGraphicFill_ColorFill    , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_None         , abfGraphicFill_None         , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_BitmapCenter , abfGraphicFill_BitmapCenter , True , True );
  abfRegisterGraphicFill(SabfGraphicFill_BitmapStretch, abfGraphicFill_BitmapStretch, True , True );
  abfRegisterGraphicFill(SabfGraphicFill_BitmapTile   , abfGraphicFill_BitmapTile   , False, True );
  abfRegisterGraphicFill(SabfGraphicFill_GradientHorz , abfGraphicFill_GradientHorz , True , False);
  abfRegisterGraphicFill(SabfGraphicFill_GradientVert , abfGraphicFill_GradientVert , True , False);
  abfRegisterGraphicFill(SabfGraphicFill_Adobe        , abfGraphicFill_Adobe        , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Alcatraz     , abfGraphicFill_Alcatraz     , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_ElPaso       , abfGraphicFill_ElPaso       , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Hanoi        , abfGraphicFill_Hanoi        , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Harvard      , abfGraphicFill_Harvard      , True , False);
  abfRegisterGraphicFill(SabfGraphicFill_Holes        , abfGraphicFill_Holes        , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Hollywood    , abfGraphicFill_Hollywood    , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Honolulu     , abfGraphicFill_Honolulu     , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_London       , abfGraphicFill_London       , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Manhattan    , abfGraphicFill_Manhattan    , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Minsk        , abfGraphicFill_Minsk        , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Orleans      , abfGraphicFill_Orleans      , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Oshawa       , abfGraphicFill_Oshawa       , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Philadelphia , abfGraphicFill_Philadelphia , True , False);
  abfRegisterGraphicFill(SabfGraphicFill_Pittsburg    , abfGraphicFill_Pittsburg    , False, False);
  abfRegisterGraphicFill(SabfGraphicFill_Sahara       , abfGraphicFill_Sahara       , False, False);
end;

//------------------------------------------------------------------------------
// Frees GraphicFills routines

procedure _UnregisterGraphicFills;
var
  i: Integer;
begin
  if not Assigned(abfGraphicFillList) then Exit;
  for i := 0 to abfGraphicFillList.Count - 1 do
    abfGraphicFillList.Objects[i].Free;
  FreeAndNil(abfGraphicFillList);
end;


//==============================================================================
// TabfFormEffectRolling
//==============================================================================
// Class for the Rolling property of the TabfFormEffect component.
{ TabfFormEffectRolling }

constructor TabfFormEffectRolling.Create(AOwner: TabfFormEffect);
begin
  FOwner := AOwner;
  if not Assigned(FOwner) then
    raise EabfFormEffect.Create(SabfEffects_WrongOwner);
  inherited Create;
  FAnimate := True;
  FHeight := 0;
  FOnMinMax := True;
  FDelay := 3;
end;

//------------------------------------------------------------------------------

function TabfFormEffectRolling.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffectRolling.SetHeight(A: Integer);
begin
  if Height = A then Exit;
  FHeight := A;
  Owner.ResetRolling;
end;


//==============================================================================
// TabfFormEffectShaking
//==============================================================================
// Class for the Shaking property of the TabfFormEffect component.
{ TabfFormEffectShaking }

constructor TabfFormEffectShaking.Create(AOwner: TabfFormEffect);
begin
  FOwner := AOwner;
  if not Assigned(FOwner) then
    raise EabfFormEffect.Create(SabfEffects_WrongOwner);
  inherited Create;
  FAmplitude := 10;
  FInterval := 25;
  FTime := 500;
end;

//------------------------------------------------------------------------------

function TabfFormEffectShaking.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffectShaking.SetInterval(A: Cardinal);
begin
  if FInterval = A then Exit;
  FInterval := A;
  Owner.ResetShaking;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffectShaking.SetTime(A: Cardinal);
begin
  if FTime = A then Exit;
  FTime := A;
  Owner.ResetShaking;
end;

//==============================================================================
// TabfFormEffectShaking
//==============================================================================
// Class for the AlphaBlending property of the TabfFormEffect component.
{ TabfFormEffectAlphaBlending }

constructor TabfFormEffectAlphaBlending.Create(AOwner: TabfFormEffect);
begin
  FOwner := AOwner;
  if not Assigned(FOwner) then
    raise EabfFormEffect.Create(SabfEffects_WrongOwner);

  inherited Create;

  FThread := TabfThreadComponent.Create(nil);
  FThread.OnExecute := OnExecute;

  FAsync := True;
{$IfDef D6}
  FSaveAlpha := TForm(Owner.Owner).AlphaBlendValue;
  FSaveBlend := TForm(Owner.Owner).AlphaBlend;
  FAlpha := FSaveAlpha;
{$Else D6}
  FAlpha := 255;
{$EndIf D6}
  FInvertOnCycle := True;
  FMinBlend := 100;
  FMaxBlend := 255;
  FEffect   := abeFade;
  FTime     := 1000;
  FCycled   := False;
end;

//------------------------------------------------------------------------------

destructor TabfFormEffectAlphaBlending.Destroy;
begin
  Cycled := False;
  Terminate;
  FreeAndNil(FThread);
{$IfDef D6}
  if Assigned(Owner.Owner) and
    not (csDestroying in TForm(Owner.Owner).ComponentState) then
    with TForm(Owner.Owner) do
    begin
      AlphaBlendValue := FSaveAlpha;
      AlphaBlend      := FSaveBlend;
    end;
{$EndIf D6}
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// Starts effect

procedure TabfFormEffectAlphaBlending.Execute;
begin
  if csDesigning in Owner.ComponentState then Exit;

  Terminate;

// Do nothing in old Windows systems
  if not OSVersion.Check(5) then Exit;

  FTerminated := False;
  FExecuting := True;

  if Async then
    FThread.Execute
  else
    OnExecute(Self);
end;

//------------------------------------------------------------------------------
// Terminates effect

procedure TabfFormEffectAlphaBlending.Terminate;
begin
  FTerminated := True;
  if Async then FThread.Terminate;
end;

//------------------------------------------------------------------------------

function TabfFormEffectAlphaBlending.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------
// AlphaBlending effect's core routine

procedure TabfFormEffectAlphaBlending.OnExecute(Sender: TObject);
var
  T: Cardinal;
begin
// Do nothing in old Windows systems
  if not OSVersion.Check(5) then Exit;

  repeat
    FBeginTime := GetTickCount;
    T := GetTickCount;

    if FTerminated or Application.Terminated then Break;

  // Fire event
    if Assigned(OnBegin) then OnBegin(Owner);

    while T < FBeginTime + Time do
    begin
      if FTerminated or Application.Terminated then Break;

      case Effect of
        abeFade: Alpha := Round(MaxAlpha - (MaxAlpha - MinAlpha) *
          Max(1, (T - FBeginTime)) / Time);
        abeAppear: Alpha := Round(MinAlpha + (MaxAlpha - MinAlpha) *
          Max(1, (T - FBeginTime)) / Time);
      end;

      T := GetTickCount;
    end;

    if FTerminated or Application.Terminated then Break;

  // Set edge values
    case Effect of
      abeFade: Alpha := MinAlpha;
      abeAppear: Alpha := MaxAlpha;
    end;

  // Switch to inverted effect if it is needed
    if Cycled and InvertOnCycle then
//      if (Effect in [abeAppear, abeFade]) then
        if Effect = abeAppear then
          Effect := abeFade
        else
          Effect := abeAppear;

    if FTerminated or Application.Terminated then Break;

  // Process messages if synchronized
    if not Async then
      Application.ProcessMessages;

  // Fire event
    if Assigned(OnEnd) then OnEnd(Owner);

  until not Cycled;

  FExecuting := False;
end;{procedure TabfFormEffectAlphaBlending.OnExecute}


//==============================================================================
// Properties Get/Set

procedure TabfFormEffectAlphaBlending.SetAlpha(Value: Byte);
{$IfNDef D6}
var
  AStyle: Integer;
{$EndIf D6}
begin
  if Alpha = Value then Exit;
  FAlpha := Value;

// Do nothing in old Windows systems
  if not OSVersion.Check(5) then Exit;

// Change form transparency
  if Assigned(Owner.Owner) and TForm(Owner.Owner).HandleAllocated and
    not (csDestroying in TForm(Owner.Owner).ComponentState) then
    with TForm(Owner.Owner) do
    begin
{$IfDef D6}
      AlphaBlendValue := Alpha;
      AlphaBlend := True;
{$Else D6}
    // Make the form layered
      AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
      if (AStyle and WS_EX_LAYERED) = 0 then
        SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);

    // Set Alpha value
      abfSetLayeredWindowAttributes(Handle, 0, Alpha, LWA_ALPHA);
{$EndIf D6}
    end;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffectAlphaBlending.SetEffect(Value: TabfAlphaBlendingEffect);
begin
  if Effect = Value then Exit;
  FEffect := Value;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffectAlphaBlending.SetTime(Value: Cardinal);
begin
  if Time = Value then Exit;
  if Value > 60000 then Value := 60000;
  FTime := Value;
end;


//==============================================================================
// TabfFormEffect
//==============================================================================
// Use TabfFormEffect component or its descendants to hook a WndProc of the
// form where the component is placed. Provides additional features as AutoDrag,
// Rolling and Shaking effect. Rolling can be manual or Minimize/Maximize
// depended.
{ TabfFormEffect }

constructor TabfFormEffect.Create(AOwner: TComponent);
var
  i: Integer;
begin
// Check for the owner class
  if not (AOwner is TForm) then
    raise EabfFormEffect.CreateFmt(SabfFormEffect_WrongOwner, [ClassName]);

// Check for same object existing
  with AOwner do
    for i := 0 to ComponentCount - 1 do
      if (Components[i] is TabfFormEffect) then
        raise EabfFormEffect.CreateFmt(SabfFormEffect_AlreadyExists,
          [Self.ClassName]);

// Continue creating
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FActive := True;
  FAutoDrag := True;

  FAlphaBlending := TabfFormEffectAlphaBlending.Create(Self);

  FRolling := TabfFormEffectRolling.Create(Self);
  ResetRolling;

  FShaking := TabfFormEffectShaking.Create(Self);
  ResetShaking;
end;

//------------------------------------------------------------------------------

destructor TabfFormEffect.Destroy;
begin
  FreeAndNil(FAlphaBlending);
  FreeAndNil(FRolling);
  FreeAndNil(FShaking);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.ResetRolling;
begin
  FOldHeight := 0;
  FRolled := False;
//  Roll(False);
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.ResetShaking;
begin
  FShaked := False;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.AlphaBlendExecute;
begin
  AlphaBlending.Execute;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.AlphaBlendTerminate;
begin
  AlphaBlending.Terminate;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.Roll(Up: Boolean);
const
  cDelta = 3;
  cDelay = 5;

  //-------------------------------------

  procedure _Roll(HeightFrom, HeightTo: Integer);
  var
    OldAutoScroll: Boolean;
    i, Distance, Delta: Integer;
    Delay: Cardinal;
  begin
    Distance := HeightTo - HeightFrom;
    with TForm(Owner) do
    begin
      OldAutoScroll := AutoScroll;
      AutoScroll := False;
      try
        if Rolling.Animate then
        begin
          Delta := Round(Distance / (Rolling.Delay * cDelta));
          if Delta = 0 then
            if Distance > 0 then Delta := 2 else Delta := -2;
          Delay := Rolling.Delay * cDelay;
          i := 0;
          while Abs(i) < Abs(Distance) do
          begin
            if Rolled <> Up then Exit; // Unrolled during rolling
            ClientHeight := HeightFrom + i;
            i := i + Delta;
            if Up then Sleep(Delay) else abfDelay(Delay);
          end;
        end;{if RollingAnimate then}
        ClientHeight := HeightTo;
      finally
        AutoScroll := OldAutoScroll;
      end;
    end;{with TForm(Owner) do}
  end;{Internal procedure _Roll}

  //-------------------------------------

begin
  FRolled := Up;
  if (csDesigning in ComponentState) then Exit;
  if Up then
  begin
    if FOldHeight = 0 then FOldHeight := TForm(Owner).ClientHeight;
    _Roll(FOldHeight, Rolling.Height);
    DoRolling;
  end else
  begin
    _Roll(Rolling.Height, FOldHeight);
    DoUnRolling;
  end;
end;{procedure TabfFormEffect.Roll}

//------------------------------------------------------------------------------

procedure TabfFormEffect.Shake;

  //-------------------------------------

  procedure _Shake;
  var
    ATime: Cardinal;
    DeltaX, DeltaY: Integer;
  begin
    with (Owner as TControl) do
    begin
      FOldPos := Point(Left, Top);
      try
        ATime := GetTickCount;
        repeat
          if Shaked = False then Exit; // Terminated during effect
          DeltaX := Random(Shaking.Amplitude) + 1;
          DeltaY := Random(Shaking.Amplitude) + 1;
          SetBounds(FOldPos.X + DeltaX, FOldPos.Y + DeltaY, Width, Height);
          abfDelay(Shaking.Interval);
        until Cardinal(Abs(GetTickCount - ATime)) >= Shaking.Time;
      finally
        SetBounds(FOldPos.X, FOldPos.Y, Width, Height);
      end;
      Shaked := False;
    end;
  end;{Internal procedure _Shake}

  //-------------------------------------

begin
  if (csDesigning in ComponentState) then Exit;
  if not _ASK then _TADA;
  FShaked := True;
  DoShakingBegin;
  _Shake;
  DoShakingEnd;
  FShaked := False;
end;


//==============================================================================
// Hook routines

procedure TabfFormEffect.WndProc(var Message: TMessage);
begin
// Catch Minimizing and Maximizing
  if Rolling.OnMinMax and not (IsIconic(Handle) or IsZoomed(Handle)) then
    with Message do
    begin
    // Minimize routine
      if (Msg = WM_SYSCOMMAND) and (wParam = SC_MINIMIZE) and not Rolled then
      begin
        Roll(True);
        Exit;
      end;
    // Maximize routine
      if (Msg = WM_SYSCOMMAND) and (wParam = SC_MAXIMIZE) and Rolled then
      begin
       if not (csDesigning in ComponentState) then if not _ASK then _TADA;
        Roll(False);
        Exit;
      end;
    end;{with Message do}
  inherited WndProc(Message);
end;


//==============================================================================
// Event handlers

procedure TabfFormEffect.DoRolling;
begin
  if Assigned(FOnRolling) then FOnRolling(Self);
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.DoUnRolling;
begin
  if Assigned(FOnUnRolling) then FOnUnRolling(Self);
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.DoShakingBegin;
begin
  if Assigned(FOnShakingBegin) then FOnShakingBegin(Self);
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.DoShakingEnd;
begin
  if Assigned(FOnShakingEnd) then FOnShakingEnd(Self);
end;


//==============================================================================
// Peoperties Get/Set

procedure TabfFormEffect.SetAlphaBlending(
  const Value: TabfFormEffectAlphaBlending);
begin
  FAlphaBlending.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.SetRolled(Value: Boolean);
begin
  Roll(FRolled)
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.SetRolling(const Value: TabfFormEffectRolling);
begin
  FRolling.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.SetShaked(Value: Boolean);
begin
  if Shaked = Value then Exit;
  FShaked := Value;
  if Shaked then Shake;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.SetShaking(const Value: TabfFormEffectShaking);
begin
  FShaking.Assign(Value);
end;

//------------------------------------------------------------------------------

function TabfFormEffect.GetOnAlphaBlendingBegin: TNotifyEvent;
begin
  Result := FAlphaBlending.FOnBegin;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.SetOnAlphaBlendingBegin(const Value: TNotifyEvent);
begin
  FAlphaBlending.FOnBegin := Value;
end;

//------------------------------------------------------------------------------

function TabfFormEffect.GetOnAlphaBlendingEnd: TNotifyEvent;
begin
  Result := FAlphaBlending.FOnEnd;
end;

//------------------------------------------------------------------------------

procedure TabfFormEffect.SetOnAlphaBlendingEnd(const Value: TNotifyEvent);
begin
  FAlphaBlending.FOnEnd := Value;
end;


//==============================================================================
// TabfCustomBackGround
//==============================================================================
// Prototype of graphic control for creating filled backgrounds.
{ TabfCustomBackGround }

const
  cFillCacheDelta = 64; // Delta of enlarging FillCacheBitmap size.

//------------------------------------------------------------------------------

constructor TabfCustomBackGround.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];

  FFillBitmap := TBitmap.Create;
  FFillBitmap.OnChange := FillBitmapChange;
  FNewParentWndProc := MakeObjectInstance(HookParentWndProc);
  FFillColorLight := clBtnHighLight;
  FFillColorDark := clBtnShadow;
  FFillTypeIndex := 0;
  FillType := SabfGraphicFill_ColorFill;
  FParentHookAllowed := True;
  Align := alClient;
  SendToBack;
  FNeedRedraw := True;
end;

//------------------------------------------------------------------------------

destructor TabfCustomBackGround.Destroy;
begin
  FFillBitmap.OnChange := nil;
  UnHookParent;
  FreeObjectInstance(FNewParentWndProc);
  FreeFillCache;
  inherited Destroy;
  FreeAndNil(FFillBitmap);
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.Paint;
begin
  if not FillCached then DrawFill(Canvas, ClientRect) else
  begin
    if FNeedRedraw then SaveFillCache;
    DrawFillCache(Canvas, ClientRect);
  end;
  FNeedRedraw := False;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
// Check the size. If it is larger then FillCacheBitmap size set FNeedRedraw flag
  if FillCached and ((AWidth > FFillCacheBitmap.Width) or
    (AHeight > FFillCacheBitmap.Height) or
    (Assigned(FGraphicFill) and FGraphicFill.SizeDepended)) then
    FNeedRedraw := True;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.Loaded;
begin
  inherited Loaded;
end;

//------------------------------------------------------------------------------
// Hooks parent WndProc if it is not Hooked by other TabfCustomBackGround
// control.

procedure TabfCustomBackGround.SetParent(AParent : TWinControl);
var
  i: Integer;
begin
  UnHookParent;
  inherited SetParent(AParent);
  if Assigned(AParent) then
    with AParent do
    begin
      for i := 0 to ControlCount - 1 do
        if (Controls[i] is TabfCustomBackGround) and (Controls[i] <> Self) and
          TabfCustomBackGround(Controls[i]).FParentHooked then Exit;
    end;
  HookParent;
end;


//==============================================================================
// Fill routines

//------------------------------------------------------------------------------
// Renders fill on the Canvas

procedure TabfCustomBackGround.DrawFill(const ACanvas: TCanvas;
  const ARect: TRect);
begin
  if Assigned(FGraphicFill) then
    if FGraphicFill.NeedBitmap then
      FGraphicFill.Proc(ACanvas, ARect, FillColorNormal, FillColorLight,
        FillColorDark, FillBitmap)
    else
      FGraphicFill.Proc(ACanvas, ARect, FillColorNormal, FillColorLight,
        FillColorDark, nil);
end;

//------------------------------------------------------------------------------
// Copies saved FillCache to the Canvas

procedure TabfCustomBackGround.DrawFillCache(const ACanvas: TCanvas;
  const ARect: TRect);
begin
  if not Assigned(FFillCacheBitmap) or FFillCacheBitmap.Empty then Exit;

  with ARect do
    BitBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
      FFillCacheBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

//------------------------------------------------------------------------------
// Copies part of saved FillCache to the Canvas

procedure TabfCustomBackGround.DrawFillCacheRect(const ACanvas: TCanvas;
  const ACanvasRect, ACacheRect: TRect);
begin
  if not Assigned(FFillCacheBitmap) or FFillCacheBitmap.Empty then Exit;

  StretchBlt(ACanvas.Handle, ACanvasRect.Left, ACanvasRect.Top,
    ACanvasRect.Right - ACanvasRect.Left, ACanvasRect.Bottom - ACanvasRect.Top,
    FFillCacheBitmap.Canvas.Handle, ACacheRect.Left, ACacheRect.Top,
    ACacheRect.Right - ACacheRect.Left, ACacheRect.Bottom - ACacheRect.Top,
    SRCCOPY);
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.FreeFillCache;
begin
  if Assigned(FFillCacheBitmap) then FreeAndNil(FFillCacheBitmap);
end;

//------------------------------------------------------------------------------
// Resizes FFillCacheBitmap and copies curent fill to the FFillCacheBitmap canvas

procedure TabfCustomBackGround.SaveFillCache;
begin
// Check Cache bitmap. Create if is not assigned
  if not Assigned(FFillCacheBitmap) then
  begin
    FFillCacheBitmap := TBitmap.Create;
    FFillCacheBitmap.PixelFormat := pf24bit;
  end;

// Check Cache bitmap size, Enlarge if needed
  if Assigned(FGraphicFill) and FGraphicFill.SizeDepended then
  begin
    FFillCacheBitmap.Width  := Width;
    FFillCacheBitmap.Height := Height;
  end else
  begin
    if FFillCacheBitmap.Width < Width then
      FFillCacheBitmap.Width := ((Width div cFillCacheDelta) + 1) *
        cFillCacheDelta;
    if FFillCacheBitmap.Height < Height then
      FFillCacheBitmap.Height := ((Height div cFillCacheDelta) + 1) *
        cFillCacheDelta;
    if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  end;
  
// Render fill on the Cache bitmap's canvas
  with FFillCacheBitmap do
    DrawFill(FFillCacheBitmap.Canvas, Rect(0, 0, Width, Height));
end;


//==============================================================================
// Parent hook routines

procedure TabfCustomBackGround.HookParent;
begin
  if (csDesigning in ComponentState) then Exit;
  if FParentHooked or (not ParentHookAllowed) then Exit;

  if not Assigned(Parent) or (not Parent.HandleAllocated) then Exit;
  FOldParentWndProc := Pointer(GetWindowLong(Parent.Handle, GWL_WNDPROC));
  SetWindowLong(Parent.Handle, GWL_WNDPROC, LongInt(FNewParentWndProc));
  FParentHooked := True;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.UnHookParent;
begin
  if (csDesigning in ComponentState) then Exit;
  if not FParentHooked then Exit;

  if not Assigned(Parent) or (FOldParentWndProc = nil) or
    (not Parent.HandleAllocated) then Exit;
  SetWindowLong(Parent.Handle, GWL_WNDPROC, LongInt(FOldParentWndProc));
  FOldParentWndProc := nil;
  FParentHooked := False;
end;

//------------------------------------------------------------------------------
// Turns off EraseBackGround routines if TabfCustomBackGround visible

procedure TabfCustomBackGround.HookParentWndProc(var Message : TMessage);
begin
  with Message do
  begin
    if (Msg = WM_ERASEBKGND) and Visible then
    begin
      Result := 1;
      Exit;
    end;
    Result := CallWindowProc(FOldParentWndProc, Parent.Handle, Msg,
      WParam, LParam);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.FillBitmapChange(Sender: TObject);
begin
  FNeedRedraw := True;
  Invalidate;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomBackGround.SetFillBitmap(const Value: TBitmap);
begin
  FFillBitmap.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.SetFillCached(Value: Boolean);
begin
  if FillCached = Value then Exit;
  FFillCached := Value;
  if FillCached then SaveFillCache else FreeFillCache;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.SetFillColorDark(Value: TColor);
begin
  if FillColorDark = Value then Exit;
  FFillColorDark := Value;
  FNeedRedraw := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.SetFillColorLight(Value: TColor);
begin
  if FillColorLight = Value then Exit;
  FFillColorLight := Value;
  FNeedRedraw := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TabfCustomBackGround.GetFillColorNormal: TColor;
begin
  Result := Color;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.SetFillColorNormal(Value: TColor);
begin
  if FillColorNormal = Value then Exit;
  Color := Value;
  FNeedRedraw := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TabfCustomBackGround.GetFillType: TabfBackGroundFillType;
begin
  Result := abfGraphicFillList[FFillTypeIndex];
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.SetFillType(const Value: TabfBackGroundFillType);
var
  i: Integer;
begin
  if TStringList(abfGraphicFillList).Find(Value, i) and
    (i <> FFillTypeIndex) then
  begin
    FFillTypeIndex := i;
    FGraphicFill := TabfGraphicFill(abfGraphicFillList.Objects[i]);
    FNeedRedraw := True;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.SetParentHookAllowed(Value: Boolean);
begin
  if ParentHookAllowed = Value then Exit;
  FParentHookAllowed := Value;
  if FParentHookAllowed then HookParent else UnhookParent;
end;


//==============================================================================
// Messages routines

procedure TabfCustomBackGround.CMColorChanged(var Message: TMessage);
begin
  inherited;
  FNeedRedraw := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomBackGround.CMSysColorChange(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  inherited;
  FNeedRedraw := True;
  Invalidate;
end;


//==============================================================================
// Tools
//==============================================================================

type
  PRGB24 = ^TRGB24;
  TRGB24 = packed record
    B, G, R: byte;
  end;
  PRGB24Array = ^TRGB24Array;
  TRGB24Array = packed array[0..MaxInt div SizeOf(TRGB24) - 1] of TRGB24;

//------------------------------------------------------------------------------

function _AddSizeY(var size1: TSize; const size2: TSize): TSize;
begin
  with Result do
  begin
    cx := Max(size1.cx, size2.cx);
    cy := size1.cy + size2.cy;
  end;
  size1 := Result;
end;

//------------------------------------------------------------------------------

function _TextToStrings(const S: string): TStrings;
begin
  Result := TStringList.Create;
  Result.Text := S;
end;


//==============================================================================
// TabfCreditSection
//==============================================================================
// Item of Credits collection
{ TabfCreditSection }

constructor TabfCreditSection.Create(ACollection: TCollection);
begin
  if not (ACollection is TabfCreditSections) then
    raise EabfCredits.Create(SabfEffects_WrongOwner);

  FBodyFont := TFont.Create;
  FBodyFont.Assign(TabfCreditSections(ACollection).BodyFont);
  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(TabfCreditSections(ACollection).HeaderFont);
  FBody := TStringList.Create;

  inherited Create(ACollection);

  FColCount := 1;
  FColSpace := TabfCreditSections(Collection).ColSpace;

  TStringList(FBody).OnChange := OnChangeEvent;
  FBodyFont.OnChange := OnChangeEvent;
  FHeaderFont.OnChange := OnChangeEvent;

  FParentColSpace   := True;
  FParentBodyFont   := True;
  FParentHeaderFont := True;
  FBodyTextEffect   := cteRaised;
  FHeaderTextEffect := cteRaised;
end;

//------------------------------------------------------------------------------

destructor TabfCreditSection.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBodyFont);
  FreeAndNil(FHeaderFont);
  FreeAndNil(FBody);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.Assign(Source: TPersistent);
begin
  if Source is TabfCreditSection then
  begin
    FBody      .Assign(TabfCreditSection(Source).Body);
    FBodyFont  .Assign(TabfCreditSection(Source).BodyFont);
    FHeaderFont.Assign(TabfCreditSection(Source).HeaderFont);
    FColCount         := TabfCreditSection(Source).ColCount;;
    FHeader           := TabfCreditSection(Source).Header;
    FParentBodyFont   := TabfCreditSection(Source).ParentBodyFont;
    FParentHeaderFont := TabfCreditSection(Source).ParentHeaderFont;
  end else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.RenderText(const ACanvas: TCanvas;
  const ATop, AWidth: Integer);
var
  cw: Integer;

  //-------------------------------------

  procedure _TextOut(const X, Y: Integer; Text: string;
    TextEffect: TabfCreditTextEffect);
  var
    SaveColor: TColor;
  begin
    with ACanvas do
    begin
      if TextEffect <> cteNone then
      begin
      // Save color
        SaveColor := Font.Color;
      // Bottom-right part
        case TextEffect of
          cteRaised : Font.Color := clWhite;
          cteLowered: Font.Color := clBlack;
        end;
      // Top-left part
        TextOut(x, y, Text);
        case TextEffect of
          cteRaised : Font.Color := clBlack;
          cteLowered: Font.Color := clWhite;
        end;
        TextOut(x + 2, y + 2, Text);
      // Restore color
        Font.Color := SaveColor;
      end;
      TextOut(x + 1, y + 1, Text);
    end;
  end;

  //-------------------------------------

  procedure _CenterText(const X, Y, AWidth: Integer; Text: string;
    TextEffect: TabfCreditTextEffect);
  var
    rx: Integer;
  begin
    rx := Round((AWidth - ACanvas.TextWidth(Text)) / 2);
    _TextOut(X + rx, Y, Text, TextEffect);
  end;

  //-------------------------------------

  procedure _DrawColumn(const X, Y, Column: Integer);
  var
    i, k, CurrentY, LineHeight: Integer;
  begin
    if Body.Count = 0 then Exit;
    LineHeight := ACanvas.TextHeight('Wj'{Body[0]});
    CurrentY := Y;
    for i := 0 to Body.Count - 1 do
      if (i mod ColCount) = Column then
      begin
        with _TextToStrings(Body[i]) do
        try
          for k := 0 to Count - 1 do
          begin
            _CenterText(X, CurrentY, cw, Strings[k], BodyTextEffect);
            CurrentY := CurrentY + LineHeight;
          end;
        finally
          Free; // _TextToStrings
        end;
      end;
  end;{Internal procedure DrawColumn}

  //-------------------------------------

  function _ColumnWidth: Integer;
  begin
    Result := AWidth;
    if ColCount <> 0 then 
      Result := (AWidth - ColSpace * (ColCount - 1)) div ColCount;
  end;

  //-------------------------------------

var
  i, h, cs: Integer;
begin
  cw := _ColumnWidth;
  cs := ColSpace;
  h := 0;
  if Header <> '' then
  begin
    ACanvas.Font := {TabfCreditSections(Collection).}HeaderFont;
    _CenterText(0, ATop, AWidth, Header, HeaderTextEffect);
    h := ACanvas.TextHeight(Header);
  end;
  ACanvas.Font := {TabfCreditSections(Collection).}BodyFont;
  for i := 0 to ColCount - 1 do
    _DrawColumn((cw + cs) * i, ATop + h, i);
end;{procedure TabfCreditSection.RenderText}

//------------------------------------------------------------------------------

function TabfCreditSection.TextExtent(const ACanvas: TCanvas): TSize;

  //-------------------------------------

  function _TextExtent(const S: string): TSize;
  var
    i: Integer;
  begin
    FillChar(Result, SizeOf(TSize), 0);
    with _TextToStrings(S) do
    try
      for i := 0 to Count - 1 do
        _AddSizeY(Result, ACanvas.TextExtent(Strings[i]));
      Inc(Result.cx, 2); // shadow size   
    finally
      Free; // _TextToStrings
    end;
  end;

  //-------------------------------------

  function _ColumnExtent(const Column: Integer): TSize;
  var
    i: Integer;
  begin
    FillChar(Result, SizeOf(TSize), 0);
    for i := 0 to Body.Count - 1 do
      if (i mod ColCount) = Column then
        _AddSizeY(Result, _TextExtent(Body[i]));
  end;

  //-------------------------------------

var
  i: Integer;
  MaxColCX, MaxColCY: Integer;
  CSize: TSize;
begin
  if (FTextExtent.cx <> 0) or (FTextExtent.cy <> 0) then
    Result := FTextExtent
  else
    begin
      FillChar(Result, SizeOf(TSize), 0);
      MaxColCX := 0;
      MaxColCY := 0;
      if Header <> '' then
      begin
        ACanvas.Font := {(Collection as TabfCreditSections).}HeaderFont;
        Result := _TextExtent(Header);
      end;
      ACanvas.Font := {(Collection as TabfCreditSections).}BodyFont;
      for i := 0 to ColCount - 1 do
      begin
        CSize := _ColumnExtent(i);
        MaxColCX := Max(CSize.cx, MaxColCX);
        MaxColCY := Max(CSize.cy, MaxColCY);
      end;
      i := ColSpace * (ColCount - 1);
      CSize.cy := MaxColCY;
      CSize.cx := MaxColCX * ColCount + i;
      _AddSizeY(Result, CSize);
      FTextExtent := Result;
    end;
end;{function TabfCreditSection.TextExtent}

//------------------------------------------------------------------------------

procedure TabfCreditSection.OnChangeEvent(Sender: TObject);
begin
  Changed(False);
end;

//------------------------------------------------------------------------------

function TabfCreditSection.IsColSpaceStored: Boolean;
begin
  Result := not ParentColSpace;
end;

//------------------------------------------------------------------------------

function TabfCreditSection.IsBodyFontStored: Boolean;
begin
  Result := not ParentBodyFont;
end;

//------------------------------------------------------------------------------

function TabfCreditSection.IsHeaderFontStored: Boolean;
begin
  Result := not ParentHeaderFont;
end;


//==============================================================================
// Properties Get/Set

function TabfCreditSection.GetBodyItems(Index: Integer): string;
begin
  Result := Body[Index];
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetBodyItems(Index: Integer; const Value: string);
begin
  Body[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetBody(const Value: TStrings);
begin
  FBody.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetHeader(const Value: string);
begin
  if Value = FHeader then Exit;
  FHeader := Value;
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetBodyFont(const Value: TFont);
begin
  if BodyFont = Value then Exit;
  FBodyFont.Assign(Value);
  FParentBodyFont := (Value = TabfCreditSections(Collection).BodyFont);
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetHeaderFont(const Value: TFont);
begin
  if HeaderFont = Value then Exit;
  FHeaderFont.Assign(Value);
  FParentHeaderFont := (Value = TabfCreditSections(Collection).HeaderFont);
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetBodyTextEffect(Value: TabfCreditTextEffect);
begin
  if BodyTextEffect = Value then Exit;
  FBodyTextEffect := Value;
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetHeaderTextEffect(Value: TabfCreditTextEffect);
begin
  if HeaderTextEffect = Value then Exit;
  FHeaderTextEffect := Value;
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetColCount(Value: Integer);
begin
  if (FColCount = Value) and (Value <= 0) then Exit;
  FColCount := Value;
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetColSpace(Value: Integer);
begin
  if ColSpace = Value then Exit;
  FColSpace := Value;
  FParentColSpace := (ColSpace = TabfCreditSections(Collection).ColSpace);
  Changed(False);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetParentColSpace(Value: Boolean);
begin
  if ParentColSpace = Value then Exit;
  FParentColSpace := Value;
  if ParentColSpace then
    ColSpace := TabfCreditSections(Collection).ColSpace;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetParentBodyFont(Value: Boolean);
begin
  if ParentBodyFont = Value then Exit;
  FParentBodyFont := Value;
  if ParentBodyFont then
    BodyFont := TabfCreditSections(Collection).BodyFont;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSection.SetParentHeaderFont(Value: Boolean);
begin
  if ParentHeaderFont = Value then Exit;
  FParentHeaderFont := Value;
  if ParentHeaderFont then
    HeaderFont := TabfCreditSections(Collection).HeaderFont;
end;


//==============================================================================
// TabfCreditSections
//==============================================================================
// Credits collection
{ TabfCreditSections }

constructor TabfCreditSections.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create(TabfCreditSection);
  FColSpace := 0;
  FBodyFont := TFont.Create;
  FBodyFont.Name := 'Arial';
  FBodyFont.Size := 14;
  FBodyFont.Color := clBlue;

  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(Owner.Font);
  FHeaderFont.Name := FBodyFont.Name;
  FHeaderFont.Size := 14;
  FHeaderFont.Color := clRed;

  FBodyFont.OnChange := OnChangeEvent;
  FHeaderFont.OnChange := OnChangeEvent;
end;

//------------------------------------------------------------------------------

destructor TabfCreditSections.Destroy;
begin
  FreeAndNil(FHeaderFont);
  FreeAndNil(FBodyFont);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TabfCreditSections then
    begin
      ColSpace   := TabfCreditSections(Source).ColSpace;
      HeaderFont := TabfCreditSections(Source).HeaderFont;
      BodyFont   := TabfCreditSections(Source).BodyFont;
    end;
    inherited;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

function TabfCreditSections.Add: TabfCreditSection;
begin
  Result := TabfCreditSection(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.RenderText(const ACanvas: TCanvas;
  const ATop, AWidth: Integer);
var
  i, Y : Integer;
begin
  Y := ATop;
  for i := 0 to Count - 1 do
  begin
    Items[i].RenderText(ACanvas, Y, AWidth);
    Y := Y + Items[i].TextExtent(ACanvas).cy;
    Inc(Y, SectionSpace);
  end;
  FWidth  := AWidth;
  FHeight := Y;
end;

//------------------------------------------------------------------------------

function TabfCreditSections.TextExtent(const ACanvas: TCanvas): TSize;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(TSize), 0);
  for i := 0 to Count - 1 do
  begin
    _AddSizeY(Result, Items[i].TextExtent(ACanvas));
    Inc(Result.cy, SectionSpace);
  end;
end;

//------------------------------------------------------------------------------

function TabfCreditSections.Owner: TabfCustomCredits;
begin
  Result := TabfCustomCredits(FOwner);
end;

//------------------------------------------------------------------------------

function TabfCreditSections.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.Update(Item: TCollectionItem);

  //-------------------------------------

  procedure _UpdateItem(AItem: TabfCreditSection);
  begin
    with AItem do
    begin
      FillChar(FTextExtent, SizeOf(TSize), 0);
      if ParentColSpace then FColSpace := Self.ColSpace;
      if ParentBodyFont then FBodyFont.Assign(Self.BodyFont);
      if ParentHeaderFont then FHeaderFont.Assign(Self.HeaderFont);
    end;
  end;{Internal _UpdateItem}

  //-------------------------------------

var
  i: Integer;
begin
  inherited Update(Item);
  if Assigned(Item) then _UpdateItem(TabfCreditSection(Item)) else
    for i := 0 to Count - 1 do
      _UpdateItem(TabfCreditSection(Items[i]));
  Owner.CreditsChange;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.OnChangeEvent(Sender: TObject);
begin
  Changed;
end;


//==============================================================================
// Properties Get/Set

function TabfCreditSections.GetItems(Index: Integer): TabfCreditSection;
begin
  Result := TabfCreditSection(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.SetItems(Index: Integer;
  const Value: TabfCreditSection);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.SetBodyFont(const Value: TFont);
begin
  FBodyFont.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.SetColSpace(Value: Integer);
begin
  if FColSpace = Value then Exit;
  FColSpace := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TabfCreditSections.SetSectionSpace(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSectionSpace = Value then Exit;
  FSectionSpace := Value;
  Changed;
end;

//------------------------------------------------------------------------------

function TabfCreditSections.IsBodyFontStored: Boolean;
begin
  Result := BodyFont.Handle <> Owner.Font.Handle;
end;

//------------------------------------------------------------------------------

function TabfCreditSections.IsHeaderFontStored: Boolean;
begin
  Result := HeaderFont.Handle <> Owner.Font.Handle;
end;


//==============================================================================
// TabfCustomCredits
//==============================================================================
// Prototype of credits rendering control
{ TabfCustomCredits }

type
{$IfDef UseThreadTimer}
  THackTimer = class(TabfThreadTimer);
{$Else UseThreadTimer}
  THackTimer = class(TabfCustomTimer);
{$EndIf UseThreadTimer}

//------------------------------------------------------------------------------

constructor TabfCustomCredits.Create(AOwner: TComponent);
begin
// Buffer bitmap
  FBufferBitmap := TBitmap.Create;
  FBufferBitmap.PixelFormat := pf24bit;
// Text bitmap
  FCreditsBitmap := TBitmap.Create;
  FCreditsBitmap.PixelFormat := pf24bit;
  inherited Create(AOwner);
  FCredits := TabfCreditSections.Create(Self);
{$IfDef UseThreadTimer}
  FTimer := TabfThreadTimer.Create(Self);
  FTimer.Priority := tpNormal{tpLower};
  FTimer.Synchronized := True;
{$Else UseThreadTimer}
  FTimer := TabfCustomTimer.Create(Self);
{$EndIf UseThreadTimer}
  THackTimer(FTimer).OnTimer := OnTimerEvent;
  FillCached := True; // Turn on using FFillCacheBitmap
  FCycled := True;
  FPathDirection := diVertical;
  TimerEnabled   := True;
  TimerInterval  := 20;
  FNeedSetPathProps := True;
end;

//------------------------------------------------------------------------------

destructor TabfCustomCredits.Destroy;
begin
  THackTimer(FTimer).Enabled := False;
  THackTimer(FTimer).OnTimer := nil;
  inherited Destroy;
  FreeAndNil(FCreditsBitmap);
  FreeAndNil(FBufferBitmap);
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.Paint;
begin
  if FNeedRedraw then
  begin
  // Prepare FFillCacheBitmap and copy it canvas to FBufferBitmap
    inherited Paint;
    UpdateSize;
    with FFillCacheBitmap do
      BitBlt(FBufferBitmap.Canvas.Handle, 0, 0, Width, Height,
        Canvas.Handle, 0, 0, SRCCOPY);
    ResetCredits;
    Exit;
  end;
// Render text on the FFillCacheBitmap
  DrawCredits;
  inherited Paint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.DrawCredits;

  //-------------------------------------

  function _GetTransparency(APos: Integer): Integer;
  begin
    Result := 0;
    if PathReverseOrder then
    begin
      if APos > PathRiseTo then
        Result := 100 - Round(Abs(100 * (PathBegin - APos) /
          (PathBegin - PathRiseTo)));
      if APos < PathFallFrom then
        Result := 100 - Round(Abs(100 * Abs(APos - PathEnd) /
          (PathFallFrom - PathEnd)));
    end else
    begin
      if APos < PathRiseTo then
        Result := 100 - Round(Abs(100 * (APos - PathBegin) /
          (PathRiseTo - PathBegin)));
      if APos > PathFallFrom then
        Result := 100 - Round(Abs(100 * (PathEnd - APos) /
          (PathEnd - PathFallFrom)));
    end;
  end;{Internal function _GetTransparency}

  //-------------------------------------

  procedure _DrawVertical;
  var
    X, Y, XB, XE, SY, Transparency, H, CH: Integer;
    P: TabfARGB;
    TargetLine, BufferLine, CreditLine: PRGB24Array;
  begin
    P := TabfARGB(abfColorToARGB(Color)); // Transparent Color
    XB := Max(0, (Width - FCreditsBitmap.Width) div 2);
    XE := Min(XB + FCreditsBitmap.Width - 1, Width);
    H  := Min(Max(PathBegin, PathEnd), Height - 1); // Height of Credit ViewPort
    CH := FCreditsBitmap.Height - 1;   // Height of Credit bitmap
    for Y := Max(Min(PathBegin, PathEnd), 0) to
      Min(Max(PathBegin, PathEnd), Height - 1) do
    begin
      if PathReverseOrder then SY := Y - (H - CreditsPosition)
      else SY := Y - CreditsPosition;
      if (SY < 0) or (SY > CH) then Continue; // Skip area outside Credit bitmap
      Transparency := _GetTransparency(Y);
      TargetLine := FFillCacheBitmap.ScanLine[Y];
      BufferLine := FBufferBitmap.ScanLine[Y];
      CreditLine := FCreditsBitmap.ScanLine[SY];
      for X := XB to XE do
        with CreditLine[X - XB] do
        begin
        // If Color of Credits pixel is same to control Color (P) don't copy it
          if ((P.R = R) and (P.G = G) and (P.B = B)) then Continue;
        // Copy pixel from Credits with current transparency
          TargetLine[X].R := Round((Transparency * BufferLine[X].R +
            (100 - Transparency) * R) * 0.01);
          TargetLine[X].G := Round((Transparency * BufferLine[X].G +
            (100 - Transparency) * G) * 0.01);
          TargetLine[X].B := Round((Transparency * BufferLine[X].B +
            (100 - Transparency) * B) * 0.01);
        end;{with CreditLine[X - XB] do}
    end;{for Y := B to E do}
  end;{Internal procedure _DrawVertical}

  //-------------------------------------

  procedure _DrawHorizontal;
  begin
    // ToDo Horizontal scrolling
    // E := Min(Max(PathBegin, PathEnd), CreditsPosition + Credits.Width);
  end;{Internal procedure _DrawHorizontal}

  //-------------------------------------

begin
  with FFillCacheBitmap do
    BitBlt(Canvas.Handle, 0, 0, Self.Width, Self.Height,
      FBufferBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  if PathDirection = diVertical then _DrawVertical else _DrawHorizontal;
end;{procedure TabfCustomCredits.DrawCredits}

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateSize;
// Don't remove from here - it prevents of PathEnd and PathBegin loosing
  if FNeedSetPathProps and (Width > 0) and (Height > 0)then
  begin
    UpdatePath;
    FPathBegin     := Height;
    FPathEnd       := 0;
    FPathRiseTo    := Height - 50;
    FPathFallFrom  := 50;
    FNeedSetPathProps := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.Next;
begin
  CreditsPosition := CreditsPosition + 1;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.Prev;
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  CreditsPosition := CreditsPosition - 1;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
  begin
    ResetCredits;
    Paint;
  end;
{  if FNeedSetPathProps and (Width > 0) and (Height > 0)then
  begin
    FPathBegin     := Height;
    FPathEnd       := 0;
    FPathRiseTo    := Height - 50;
    FPathFallFrom  := 50;
    FNeedSetPathProps := False;
  end;}
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.ResetCredits;
begin
// Draw FBufferBitmap on the FFillCacheBitmap when position out the range
  if (CreditsPosition <= MinPosition) or (CreditsPosition >= MaxPosition) then
    with FFillCacheBitmap do
      BitBlt(Canvas.Handle, 0, 0, Width, Height,
        FBufferBitmap.Canvas.Handle, 0, 0, SRCCOPY);
// Make FCreditsBitmap empty when position reaches the Max value
  if CreditsPosition >= MaxPosition then
  begin
    FCreditsBitmap.Width  := 0;
    FCreditsBitmap.Height := 0;
    Exit;
  end;
// Resize FCreditsBitmap and render credits text
  with FCreditsBitmap do
  begin
    //with Credits.TextExtent(Canvas) do
    begin
      Width  := Credits.TextExtent(Canvas).cx;
      Height := Credits.TextExtent(Canvas).cy + 10;
    end;
    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Brush.Style := bsClear;
    Credits.RenderText(Canvas, 0, Width);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.UpdateSize;
begin
  if not (Assigned(FFillCacheBitmap) and Assigned(FBufferBitmap)) then Exit;
  FBufferBitmap.Width  := FFillCacheBitmap.Width;
  FBufferBitmap.Height := FFillCacheBitmap.Height;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.UpdatePath;
begin
  PathBegin := PathBegin;
  PathEnd   := PathEnd;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.OnTimerEvent(Sender: TObject);
begin
  if (csDesigning in ComponentState) then Exit;

  if Backward then Prev else Next;
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.DoCreditsEnd;
begin
  if Assigned(FOnCreditsEnd) then FOnCreditsEnd(Self);
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.CreditsChange;
begin
  ResetCredits;
  Invalidate;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomCredits.SetCredits(const Value: TabfCreditSections);
begin
  FCredits.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetCreditsPosition(Value: Integer);
var
  MinPos, MaxPos: Integer;
begin
  MinPos := MinPosition;
  MaxPos := MaxPosition;
// Constrain incoming Value
  if not Cycled then
  begin
    if Value > MaxPos then Value := MaxPos else
    if Value < MinPos then Value := MinPos;
  end else
  begin
    if Value > MaxPos then Value := MinPos else
    if Value < MinPos then Value := MaxPos;
  end;
// Apply new value if it is not same as previous
  if CreditsPosition = Value then Exit;
  FCreditsPosition := Value;
// Check is some routines should be stoped/frees
  if not Cycled then
  begin
    if ((Value = MinPos) and Backward) or
      ((Value = MaxPos) and not Backward) then TimerEnabled := False;
    if ((Value = MinPos) or (Value = MaxPos)) then
    begin
      ResetCredits;
      DoCreditsEnd;
    end;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetMaxPosition: Integer;
begin
  Result := Abs(PathEnd - PathBegin);
  if PathDirection = diVertical then
    Inc(Result, Credits.Height)
  else
    Inc(Result, Credits.Width);
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetMinPosition: Integer;
begin
  Result := 0;
{  Result := Min(PathBegin, PathEnd);
  if not Assigned(Credits) then Exit;
  if PathDirection = diVertical then
    Dec(Result, Credits.Height)
  else
    Dec(Result, Credits.Width);}
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetCreditsBodyFont: TFont;
begin
  Result := Credits.BodyFont;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetCreditsBodyFont(const Value: TFont);
begin
  Credits.BodyFont := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetCreditsHeaderFont: TFont;
begin
  Result := Credits.HeaderFont;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetCreditsHeaderFont(const Value: TFont);
begin
  Credits.HeaderFont := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetCreditsSectionSpace: Integer;
begin
  Result := Credits.SectionSpace;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetCreditsSectionSpace(Value: Integer);
begin
  Credits.SectionSpace := Value;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetPathBegin(Value: Integer);
var
  MaxValue: Integer;
begin
  if PathDirection = diVertical then MaxValue := Height else MaxValue := Width;
  if Value > MaxValue then Value := MaxValue else
  if Value < 0 then Value := 0;
  if PathBegin = Value then Exit;
  FPathBegin := Value;
  UpdatePath;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetPathDirection(Value: TabfDirection);
begin
  if PathDirection = Value then Exit;
  FPathDirection := Value;
  CreditsPosition := 0; // Reset Position
  UpdatePath;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetPathEnd(Value: Integer);
var
  MaxValue: Integer;
begin
  if PathDirection = diVertical then MaxValue := Height else MaxValue := Width;
  if Value > MaxValue then Value := MaxValue else
  if Value < 0 then Value := 0;
  if PathEnd = Value then Exit;
  FPathEnd := Value;
  UpdatePath;
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetPathReverseOrder: Boolean;
begin
  Result := (PathBegin > PathEnd);
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetPathFallFrom(Value: Integer);
begin
  if Value > Max(PathBegin, PathEnd) then Value := Max(PathBegin, PathEnd) else
  if Value < Min(PathBegin, PathEnd) then Value := Min(PathBegin, PathEnd);
  if PathFallFrom = Value then Exit;
  FPathFallFrom := Value;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetPathRiseTo(Value: Integer);
begin
  if Value > Max(PathBegin, PathEnd) then Value := Max(PathBegin, PathEnd) else
  if Value < Min(PathBegin, PathEnd) then Value := Min(PathBegin, PathEnd);
  if PathRiseTo = Value then Exit;
  FPathRiseTo := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetTimerEnabled: Boolean;
begin
  Result := THackTimer(FTimer).Enabled;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetTimerEnabled(Value: Boolean);
begin
  if (not Cycled) and (((CreditsPosition >= MaxPosition) and not Backward) or
     ((CreditsPosition <= MinPosition) and Backward)) then Value := False;
  THackTimer(FTimer).Enabled := Value;
end;

//------------------------------------------------------------------------------

function TabfCustomCredits.GetTimerInterval: Cardinal;
begin
  Result := THackTimer(FTimer).Interval;
end;

//------------------------------------------------------------------------------

procedure TabfCustomCredits.SetTimerInterval(Value: Cardinal);
begin
  THackTimer(FTimer).Interval := Value;
end;


//==============================================================================
// Messages routines

procedure TabfCustomCredits.CMFontChanged(var Message: TMessage);
begin
  ResetCredits;
  inherited;
end;


//==============================================================================
// TabfMovablePanel
//==============================================================================
// Panel with movement ability.
{ TabfMovablePanel }

constructor TabfMovablePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
{$IfDef D3}
  FullRepaint := False;
{$EndIf D3}
{$IfDef D4}
  DoubleBuffered := True;
{$EndIf D4}

// Timer routines
{$IfDef UseThreadTimer}
  FTimer := TabfThreadTimer.Create(Self);
//  FTimer.Priority := tpLower;
  FTimer.Synchronized := True;
{$Else UseThreadTimer}
  FTimer := TabfCustomTimer.Create(Self);
{$EndIf UseThreadTimer}
  THackTimer(FTimer).Enabled  := False;
  THackTimer(FTimer).OnTimer := OnTimerEvent;

// Preset properties
  FAutoBack      := False;
  FAutoRepeat    := False;
  FMoveDirection := drDown;
  FMoveFullTime  := 1000;
  TimerInterval  := 50;

// Save initial values
  FRealDirection := MoveDirection;
  FBeginTop  := Top;
  FBeginLeft := Left;
  FToDirection := False;
end;{constructor TabfMovablePanel.Create}

//------------------------------------------------------------------------------

destructor TabfMovablePanel.Destroy;
begin
  FTimer.OnTimer := nil;
  inherited Destroy;
end;


//==============================================================================
// Movement routines

procedure TabfMovablePanel.InverseDirection;
begin
  PrepareDirection(not FToDirection);
end;

//------------------------------------------------------------------------------

procedure TabfMovablePanel.DoMove;
begin
  PrepareDirection(True);
  FMovingNow := True;
  if Assigned(FOnDoMove) then FOnDoMove(Self);
  THackTimer(FTimer).Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TabfMovablePanel.UnDoMove;
begin
  PrepareDirection(False);
  FMovingNow := True;
  if Assigned(FOnUndoMove) then FOnUndoMove(Self);
  THackTimer(FTimer).Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TabfMovablePanel.SetBeginPosition(X, Y: Integer);
begin
  FBeginLeft := X;
  FBeginTop  := Y;
end;

//------------------------------------------------------------------------------

procedure TabfMovablePanel.Loaded;
begin
  inherited Loaded;
// Save initial values
  FRealDirection := MoveDirection;
  FBeginTop  := Top;
  FBeginLeft := Left;
end;


//==============================================================================
// Timer routine

procedure TabfMovablePanel.PrepareDirection(AToDirection: Boolean);
var
  DeltaTime, CurrentTime: Cardinal;
begin
  if not FMovingNow then
  begin
    FToDirection         := AToDirection;
    FTimeOfMoveBeginning := GetTickCount;
    Exit;
  end;

  if FToDirection = AToDirection then Exit;

  CurrentTime := GetTickCount;
  DeltaTime := CurrentTime - FTimeOfMoveBeginning;
  FTimeOfMoveBeginning := CurrentTime - MoveFullTime + DeltaTime;
  FToDirection := AToDirection;
end;

//------------------------------------------------------------------------------
{$WARNINGS OFF}

procedure TabfMovablePanel.OnTimerEvent(Sender: TObject);
var
  CurrentMoveTime: Cardinal;
  CurrentMoveLength: Integer;

  //-------------------------------------

  procedure _MakeMove;
  begin
    CurrentMoveLength :=
      (MoveLength * (CurrentMoveTime - FTimeOfMoveBeginning)) div MoveFullTime;
    if FToDirection then
    begin
      case FRealDirection of
        drUp  : Top  := FBeginTop  - CurrentMoveLength;
        drDown: Top  := FBeginTop  + CurrentMoveLength;
        drLeft: Left := FBeginLeft - CurrentMoveLength;
        else    Left := FBeginLeft + CurrentMoveLength;
      end;
    end else
    begin
      case FRealDirection of
        drUp   : Top  := (FBeginTop  - FMoveLength) + CurrentMoveLength;
        drDown : Top  := (FBeginTop  + FMoveLength) - CurrentMoveLength;
        drLeft : Left := (FBeginLeft - FMoveLength) + CurrentMoveLength;
        else     Left := (FBeginLeft + FMoveLength) - CurrentMoveLength;
      end;
    end;
  end;{Internal procedure _MakeMove}

  //-------------------------------------

begin
  CurrentMoveTime := GetTickCount;
  if CurrentMoveTime <= (FTimeOfMoveBeginning + MoveFullTime) then
  begin
    _MakeMove;
    Invalidate;
    Exit; // That's all for now
  end;

  CurrentMoveTime := FTimeOfMoveBeginning + MoveFullTime;
  _MakeMove;
  Invalidate;

  THackTimer(FTimer).Enabled := False;
  FMovingNow := False;
  if Assigned(FOnMoveEnd) then FOnMoveEnd(Self);

  if AutoRepeat or (AutoBack and FToDirection) then
    if not FToDirection then DoMove else UnDoMove;
end;{procedure TabfMovablePanel.OnTimerEvent}

{$WARNINGS ON}

//==============================================================================
// Properties Get/Set

function TabfMovablePanel.GetTimerInterval: Cardinal;
begin
  Result := THackTimer(FTimer).Interval;
end;

//------------------------------------------------------------------------------

procedure TabfMovablePanel.SetTimerInterval(Value: Cardinal);
begin
  THackTimer(FTimer).Interval := Value;
end;

//------------------------------------------------------------------------------

function TabfMovablePanel.GetMoved: Boolean;
begin
  Result := (Left <> BeginLeft) or (Top <> BeginTop);
end;

//------------------------------------------------------------------------------

procedure TabfMovablePanel.SetMoveLength(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FMoveLength := Value
end;

//------------------------------------------------------------------------------

procedure TabfMovablePanel.SetMoveFullTime(Value: Cardinal);
begin
  if Value <= 0 then Value := 1;
  FMoveFullTime := Value;
end;


//==============================================================================
// TabfMagnifier
//==============================================================================
// Magnignifying control. Works with desktop or any specified window.
{ TabfMagnifier }

{$IfDef D3_ONLY}
function GetCursorInfo(var pci: TCursorInfo): BOOL; stdcall; external user32 name 'GetCursorInfo';
{$EndIf D3_ONLY}

//------------------------------------------------------------------------------
// Copies rectangular area of the desktop to the given context by StretchBlt,
// so image will be stretched the specified client rect.

function _CopyRectFromDesktop(const DC: HDC; const ClientRect,
  DesktopRect: TRect): Boolean;
var
  DesktopWND: HWND;
  DesktopDC: HDC;
begin
  Result := False;

  DesktopWND := GetDesktopWindow;
  DesktopDC := GetDC(DesktopWND);
  if DesktopDC = 0 then Exit;

  with ClientRect do
  try
//    SendMessage(DesktopWND, WM_PAINT, DesktopDC, 0);
    Result := StretchBlt(DC, Left, Top, Right - Left, Bottom - Top,
      DesktopDC, DesktopRect.Left, DesktopRect.Top,
      (DesktopRect.Right  - DesktopRect.Left),
      (DesktopRect.Bottom - DesktopRect.Top ),
      SRCCOPY);
  finally
    ReleaseDC(DesktopWND, DesktopDC);
  end;
end;

//------------------------------------------------------------------------------

constructor TabfMagnifier.Create(AOwner: TComponent);
begin
  FBuffer := TBitmap.Create;
  FCursor := TBitmap.Create;
  FDoubleCursor := TBitmap.Create;
  FillChar(FCursorInfo, SizeOf(FCursorInfo), 0);
  FillChar(FIconInfo, SizeOf(FIconInfo), 0);

// Create desktop canvas to retrieve informations
  FDesktopCanvas := TCanvas.Create;
  FDesktopCanvas.Handle := GetDC(HWND_DESKTOP);

  inherited Create(AOwner);
  Width := 150;
  Height := 100;

  FTimer := TabfCustomTimer.Create(Self);
  FTimer.OnTimer := OnTimer;

  FZoomCoef     := 2.0;
  FSourceWindow := 0;
  FShowCursor   := True;
  FLargeCursor  := True;
  TimerInterval := 200;
  TimerEnabled  := True;
end;

//------------------------------------------------------------------------------

destructor TabfMagnifier.Destroy;
begin
  inherited Destroy;

// Free desktop canvas
  ReleaseDC(HWND_DESKTOP, FDesktopCanvas.Handle);
  FDesktopCanvas.Free;

  FreeAndNil(FBuffer);
  FreeAndNil(FCursor);
  FreeAndNil(FDoubleCursor);
end;

//------------------------------------------------------------------------------

procedure TabfMagnifier.Paint;
begin
  if FPainting then Exit;
  
  FPainting := True;
  try
    if FBuffer.Empty then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
    end else
      Canvas.Draw(0, 0, FBuffer);
  finally
    FPainting := False;
  end;
end;

//------------------------------------------------------------------------------
// On timer event

procedure TabfMagnifier.OnTimer(Sender: TObject);
begin
  if not Visible then Exit;
  
  UpdateMagnifier;
  Paint;
end;

//------------------------------------------------------------------------------
// Draws on the buffer bitmap part of desktop around cursor depending on size of
// magnifier control and current zoom. Also calucaltes InternalCursorPos.

procedure TabfMagnifier.UpdateMagnifier;
var
  DstRect, SrcRect: TRect;
  i: Integer;
begin
  if not Visible then Exit;
  if not GetCursorPos(FCursorPos) then Exit;

{$IfDef abfVCLTrial}
  if not _ASK then _TADA;
{$EndIf abfVCLTrial}

// Resize buffer
  FBuffer.Width  := Width;
  FBuffer.Height := Height;

// Get null based rect
  DstRect := ClientRect;
  OffsetRect(DstRect, -DstRect.Left, -DstRect.Top);

// Calc a rectangle on the desktop that should be shown
  SrcRect := Rect(0, 0, Round(FBuffer.Width / ZoomCoef),
    Round(FBuffer.Height / ZoomCoef));
  OffsetRect(SrcRect, FCursorPos.X - (SrcRect.Right div 2),
    FCursorPos.Y - (SrcRect.Bottom div 2));

// Get FSourceWindowRect to align image or know the real size
  if FSourceWindow = 0 then
  begin
  // Detect monitor under cursor and get FSourceWindowRect as a monitor rect.
{$IfDef D4}
    for i := Screen.MonitorCount - 1 downto 0 do
      with Screen.Monitors[i] do
      begin
        FSourceWindowRect := Bounds(Left, Top, Width, Height);
        if PtInRect(FSourceWindowRect, FCursorPos) then Break;
      end;
{$Else D4}
    FSourceWindowRect := Bounds(0, 0, Screen.Width, Screen.Height);
{$EndIf D4}
  end else
  begin
    if IsIconic(FSourceWindow) then Exit;
    if not GetWindowRect(FSourceWindow, FSourceWindowRect) then Exit;
  end;

// Align SrcRect using FSourceWindowRect
  if SrcRect.Right  > FSourceWindowRect.Right  then OffsetRect(SrcRect, FSourceWindowRect.Right - SrcRect.Right, 0);
  if SrcRect.Bottom > FSourceWindowRect.Bottom then OffsetRect(SrcRect, 0, FSourceWindowRect.Bottom - SrcRect.Bottom);
  if SrcRect.Left   < FSourceWindowRect.Left   then OffsetRect(SrcRect, FSourceWindowRect.Left - SrcRect.Left, 0);
  if SrcRect.Top    < FSourceWindowRect.Top    then OffsetRect(SrcRect, 0, FSourceWindowRect.Top - SrcRect.Top);{}

// Draw part of desktop to buffer
  try
    FBuffer.Canvas.CopyRect(DstRect, FDesktopCanvas, SrcRect);
  //  _CopyRectFromDesktop(FBuffer.Canvas.Handle, DstRect, SrcRect);
  except
  end;

  if not ShowCursor then Exit;

// Calculate internal cursor position
  if ((SrcRect.Right - SrcRect.Left) = 0) or
    ((SrcRect.Bottom - SrcRect.Top) = 0) then
  begin
    FInternalCursorPos := Point(-1000, -1000);
    Exit;
  end;
  FInternalCursorPos.X := Round((FCursorPos.X - SrcRect.Left) *
    (DstRect.Right - DstRect.Left) / (SrcRect.Right - SrcRect.Left));
  FInternalCursorPos.Y := Round((FCursorPos.Y - SrcRect.Top) *
    (DstRect.Bottom - DstRect.Top) / (SrcRect.Bottom - SrcRect.Top));

// Get cursor image at FCursor bitmap
  DrawCursor(FBuffer.Canvas, FInternalCursorPos, LargeCursor);
end;

//------------------------------------------------------------------------------
// Updates FCursor bitmap and saves FCursorInfo.

function TabfMagnifier.PrepareCursor: Boolean;
begin
  Result := False;

  with FCursor do
  begin
  // Prepare rect
    Width  := GetSystemMetrics(SM_CXCURSOR);
    Height := GetSystemMetrics(SM_CYCURSOR);
    Canvas.Brush.Color := $FF00FF;
    Canvas.FillRect(Rect(0, 0, Width, Height));

  // Get current cursor
    FCursorInfo.cbSize := SizeOf(FCursorInfo);
{$IfDef C3_ONLY}
    FCursorInfo.hCursor := GetCursor;
    if FCursorInfo.hCursor = 0 then Exit;
{$Else C3_ONLY}
    if not GetCursorInfo(FCursorInfo) then Exit;
{$EndIf C3_ONLY}

  // Get current cursor info. we must free some objects manually !!!
    if not GetIconInfo(FCursorInfo.hCursor, FIconInfo) then Exit;
    try
    // Draw cursor image to bitmap
      Result := DrawIconEx(Canvas.Handle, 0, 0, FCursorInfo.hCursor, 0, 0, 0, 0,
        DI_NORMAL or DI_COMPAT or DI_DEFAULTSIZE);
    finally
      DeleteObject(FIconInfo.hbmColor);
      DeleteObject(FIconInfo.hbmMask);
    end;
  end;
end;

//------------------------------------------------------------------------------
// Draws cursor image on the given canvas. Hotspot will be in specified
// position. Cursor is twice big if DoubleSize parameter is True

function TabfMagnifier.DrawCursor(const ACanvas: TCanvas; const APos: TPoint;
  DoubleSize: Boolean): Boolean;
var
  X, Y, W, H: Integer;
  SrcBmp: TBitmap;
begin
  Result := PrepareCursor;
  if not Result then Exit;

  SrcBmp := FCursor;
  X := APos.X;
  Y := APos.Y;
  W := FCursor.Width;
  H := FCursor.Height;
  Dec(X, FIconInfo.xHotspot);
  Dec(Y, FIconInfo.yHotspot);

  if DoubleSize then
  begin
  // Double the size and hotspot offset
    Dec(X, FIconInfo.xHotspot);
    Dec(Y, FIconInfo.yHotspot);
    W := W * 2;
    H := H * 2;
  // Stretch draw to DoubleCursor buffer
    FDoubleCursor.Width := W;
    FDoubleCursor.Height := H;
    FDoubleCursor.Canvas.Brush.Color := $FF00FF;
    FDoubleCursor.Canvas.FillRect(Rect(0, 0, W, H));
    StretchBlt(FDoubleCursor.Canvas.Handle, 0, 0, W, H,
      FCursor.Canvas.Handle, 0, 0, FCursor.Width, FCursor.Height, SRCCOPY);
  // Use a big cursor bitmap
    SrcBmp := FDoubleCursor;
  end;

// Draw transparent image of cursor
  SrcBmp.Transparent := True;
  ACanvas.Draw(X, Y, SrcBmp);

  Result := True;
end;


//==============================================================================
// Properties Get/Set

procedure TabfMagnifier.SetSourceWindow(Value: THandle);
begin
  FSourceWindow := Value;
  Paint;
end;

//------------------------------------------------------------------------------

function TabfMagnifier.GetTimerEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

//------------------------------------------------------------------------------

procedure TabfMagnifier.SetTimerEnabled(Value: Boolean);
begin
  FTimer.Enabled := Value;
// Reset buffer if timer is off.  
  if not Value then
  begin
    FBuffer.Width := 0;
    Paint;
  end;
end;

//------------------------------------------------------------------------------

function TabfMagnifier.GetTimerInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

//------------------------------------------------------------------------------

procedure TabfMagnifier.SetTimerInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

//------------------------------------------------------------------------------

procedure TabfMagnifier.SetZoomCoef(Value: Double);
begin
  if Value < 0.0 then Value := -Value;
  if Value < 0.001 then Value := 1;
  FZoomCoef := Value;
  Paint;
end;



{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

  Randomize;
  _RegisterGraphicFills;

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfCustomBackGround, TabfCustomCredits, TabfFormEffect,
    TabfBackGround, TabfCredits, TabfMovablePanel, TabfMagnifier]);{}

{******************************************************************************}
finalization
{******************************************************************************}
  _UnregisterGraphicFills;

end{unit abfEffects}.
