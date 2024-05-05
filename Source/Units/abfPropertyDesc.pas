{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfPropertyDesc;

{$I abf.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, StdCtrls, TypInfo,
  abfClasses;

type

//==============================================================================
// TabfPropertyDesc
//==============================================================================
// TabfPropertyDesc is a prototype of all property descriptors.

  TabfPropertyDesc = class(TPersistent)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    FCallBack: TabfObjectCallBack;
  protected
    procedure SetOnChange(A: TNotifyEvent); virtual;
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Invalidate; virtual;
  // Properties
    property Owner: TPersistent read FOwner;
    property CallBack: TabfObjectCallBack read FCallBack;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

//==============================================================================
// TabfMarginsDesc
//==============================================================================
// Descriptor for margins properties

  TabfMarginsUnits = (muPixels, muInches, muMillimeters, muCentimeters);

  TabfMarginsDesc = class(TabfPropertyDesc)
  private
    FLeft: Double;
    FTop: Double;
    FRight: Double;
    FBottom: Double;
    FUnits: TabfMarginsUnits;
  // Properties Get/Set
    procedure SetLeft(Value: Double);
    procedure SetTop(Value: Double);
    procedure SetRight(Value: Double);
    procedure SetBottom(Value: Double);
    procedure SetUnits(Value: TabfMarginsUnits);
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack; ALeft, ATop, ARight, ABottom: Double;
      AUnits: TabfMarginsUnits);
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Double read FLeft write SetLeft;
    property Top: Double read FTop write SetTop;
    property Right: Double read FRight write SetRight;
    property Bottom: Double read FBottom write SetBottom;
    property Units: TabfMarginsUnits read FUnits write SetUnits
      default muPixels;
  end;

//==============================================================================
// TabfTextDecorationDesc
//==============================================================================
// Descriptor for text decorations.

  TabfTextDecorationDesc = class(TabfPropertyDesc)
  private
    FColor: TColor;
    FStyle: TFontStyles;
    procedure SetColor(A: TColor);
    procedure SetStyle(A: TFontStyles);
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack; AColor: TColor; AStyle: TFontStyles);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor;
    property Style: TFontStyles read FStyle write SetStyle; // no default !!!
  end;

//==============================================================================
// TabfColorDecorationDesc
//==============================================================================
// Descriptor for color decorations.

  TabfColorDecorationDesc = class(TabfPropertyDesc)
  private
    FColor: TColor;
    FVisible: Boolean;
    procedure SetColor(A: TColor);
    procedure SetVisible(A: Boolean);
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack; AColor: TColor; AVisible: Boolean);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor  read FColor write SetColor;
    property Visible: Boolean read FVisible write SetVisible;
  end;

//==============================================================================
// TabfBorderDecorationDesc
//==============================================================================
// Descriptor for Border, Shadow or other decorations.

  TabfBorderDecorationDesc = class(TabfPropertyDesc)
  private
    FColor: TColor;
    FVisible: Boolean;
    FSizeX: Integer;
    FSizeY: Integer;
    procedure SetColor(A: TColor);
    procedure SetVisible(A: Boolean);
    procedure SetSizeX(A: Integer);
    procedure SetSizeY(A: Integer);
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack; AVisible: Boolean; AColor: TColor;
      ASizeX, ASizeY: Integer); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property Color: TColor  read FColor write SetColor;
    property SizeX: Integer read FSizeX write SetSizeX default 1;
    property SizeY: Integer read FSizeY write SetSizeY default 1;
  end;

//==============================================================================
// TabfRunTimeDesc
//==============================================================================
// A prototype of all 4 run-time states property descriptors.

  TabfRunTimeDesc = class(TabfPropertyDesc)
  private
    FRunTimeState: TabfRunTimeState;
    FUseOnlyNormal: Boolean;
  protected
    procedure SetRunTimeState(A: TabfRunTimeState); virtual;
    procedure SetUseOnlyNormal(A: Boolean); virtual;
  // Properties
    property UseOnlyNormal: Boolean read FUseOnlyNormal write SetUseOnlyNormal
      default False;
  public
    procedure Assign(Source: TPersistent); override;
  // Properties
    property RunTimeState: TabfRunTimeState read FRunTimeState
      write SetRunTimeState;
  end;

//==============================================================================
// TabfRunTimeBooleanDesc
//==============================================================================
// Boolean 4 run-time states property descriptor.

  TabfRunTimeBooleanDesc = class(TabfRunTimeDesc)
  private
    FNormal  : Boolean;
    FSelected: Boolean;
    FActive  : Boolean;
    FDisabled: Boolean;
    function  GetCurrent: Boolean;
    procedure SetNormal  (A: Boolean);
    procedure SetSelected(A: Boolean);
    procedure SetActive  (A: Boolean);
    procedure SetDisabled(A: Boolean);
  public
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: Boolean); virtual;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: Boolean read GetCurrent;
  published
    property Normal  : Boolean read FNormal   write SetNormal;
    property Selected: Boolean read FSelected write SetSelected;
    property Active  : Boolean read FActive   write SetActive;
    property Disabled: Boolean read FDisabled write SetDisabled;
  end;

//==============================================================================
// TabfRunTimeColorDesc
//==============================================================================
// TColor 4 run-time states property descriptor.

  TabfRunTimeColorDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TColor;
    FSelected: TColor;
    FActive  : TColor;
    FDisabled: TColor;
    function  GetCurrent: TColor;
    procedure SetNormal  (A: TColor);
    procedure SetSelected(A: TColor);
    procedure SetActive  (A: TColor);
    procedure SetDisabled(A: TColor);
  public
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: TColor); virtual;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TColor read GetCurrent;
  published
    property Normal  : TColor read FNormal   write SetNormal;
    property Selected: TColor read FSelected write SetSelected;
    property Active  : TColor read FActive   write SetActive;
    property Disabled: TColor read FDisabled write SetDisabled;
  end;

//==============================================================================
// TabfRunTimeCursorDesc
//==============================================================================
// TCursor 4 run-time states property descriptor.

  TabfRunTimeCursorDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TCursor;
    FSelected: TCursor;
    FActive  : TCursor;
    FDisabled: TCursor;
    function  GetCurrent: TCursor;
    procedure SetNormal  (A: TCursor);
    procedure SetSelected(A: TCursor);
    procedure SetActive  (A: TCursor);
    procedure SetDisabled(A: TCursor);
  public
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: TCursor); virtual;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TCursor read GetCurrent;
  published
    property Normal  : TCursor read FNormal   write SetNormal;
    property Selected: TCursor read FSelected write SetSelected;
    property Active  : TCursor read FActive   write SetActive;
    property Disabled: TCursor read FDisabled write SetDisabled;
  end;

//==============================================================================
// TabfRunTimeBitmapDesc
//==============================================================================
// TBitmap 4 run-time states property descriptor.

  TabfRunTimeBitmapDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TBitmap;
    FSelected: TBitmap;
    FActive  : TBitmap;
    FDisabled: TBitmap;
    function  GetCurrent: TBitmap;
    procedure SetNormal  (A: TBitmap);
    procedure SetSelected(A: TBitmap);
    procedure SetActive  (A: TBitmap);
    procedure SetDisabled(A: TBitmap);
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: TBitmap); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TBitmap read GetCurrent;
  published
    property Normal  : TBitmap read FNormal   write SetNormal;
    property Selected: TBitmap read FSelected write SetSelected;
    property Active  : TBitmap read FActive   write SetActive;
    property Disabled: TBitmap read FDisabled write SetDisabled;
  end;

//==============================================================================
// TabfRunTimePictureDesc
//==============================================================================
// TPicture 4 run-time states property descriptor.

  TabfRunTimePictureDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TPicture;
    FSelected: TPicture;
    FActive  : TPicture;
    FDisabled: TPicture;
    function  GetCurrent: TPicture;
    procedure SetNormal  (A: TPicture);
    procedure SetSelected(A: TPicture);
    procedure SetActive  (A: TPicture);
    procedure SetDisabled(A: TPicture);
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: TPicture); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TPicture read GetCurrent;
  published
    property Normal  : TPicture read FNormal   write SetNormal;
    property Selected: TPicture read FSelected write SetSelected;
    property Active  : TPicture read FActive   write SetActive;
    property Disabled: TPicture read FDisabled write SetDisabled;
  end;

//==============================================================================
// TabfRunTimeFontDesc
//==============================================================================
// TFont 4 run-time states property descriptor.

  TabfRunTimeFontDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TFont;
    FSelected: TFont;
    FActive  : TFont;
    FDisabled: TFont;
    function  GetCurrent: TFont;
    procedure SetNormal  (A: TFont);
    procedure SetSelected(A: TFont);
    procedure SetActive  (A: TFont);
    procedure SetDisabled(A: TFont);
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: TFont); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TFont read GetCurrent;
  published
    property Normal  : TFont read FNormal   write SetNormal;
    property Selected: TFont read FSelected write SetSelected;
    property Active  : TFont read FActive   write SetActive;
    property Disabled: TFont read FDisabled write SetDisabled;
  end;

//==============================================================================
// TabfRunTimeTextDecorationDesc
//==============================================================================
// Text decoration 4 run-time states property descriptor.

  TabfRunTimeTextDecorationDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TabfTextDecorationDesc;
    FSelected: TabfTextDecorationDesc;
    FActive  : TabfTextDecorationDesc;
    FDisabled: TabfTextDecorationDesc;
    function  GetCurrent: TabfTextDecorationDesc;
    procedure SetNormal  (A: TabfTextDecorationDesc);
    procedure SetSelected(A: TabfTextDecorationDesc);
    procedure SetActive  (A: TabfTextDecorationDesc);
    procedure SetDisabled(A: TabfTextDecorationDesc);
  protected
    procedure SetOnChange(A: TNotifyEvent); override;
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: TabfTextDecorationDesc); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TabfTextDecorationDesc read GetCurrent;
  published
    property Normal  : TabfTextDecorationDesc read FNormal   write SetNormal;
    property Selected: TabfTextDecorationDesc read FSelected write SetSelected;
    property Active  : TabfTextDecorationDesc read FActive   write SetActive;
    property Disabled: TabfTextDecorationDesc read FDisabled write SetDisabled;
  end;

//==============================================================================
// TabfRunTimeColorDecorationDesc
//==============================================================================
// Color decoration 4 run-time states property descriptor.

  TabfRunTimeColorDecorationDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TabfColorDecorationDesc;
    FSelected: TabfColorDecorationDesc;
    FActive  : TabfColorDecorationDesc;
    FDisabled: TabfColorDecorationDesc;
    FSizeY: Integer;
    FSizeX: Integer;
    function  GetCurrent: TabfColorDecorationDesc;
    procedure SetNormal  (A: TabfColorDecorationDesc);
    procedure SetSelected(A: TabfColorDecorationDesc);
    procedure SetActive  (A: TabfColorDecorationDesc);
    procedure SetDisabled(A: TabfColorDecorationDesc);
    procedure SetSizeX(A: Integer);
    procedure SetSizeY(A: Integer);
  protected
    procedure SetOnChange(A: TNotifyEvent); override;
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack;
      ANormal, ASelected, AActive, ADisabled: TabfColorDecorationDesc); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TabfColorDecorationDesc read GetCurrent;
  published
    property Normal  : TabfColorDecorationDesc read FNormal   write SetNormal;
    property Selected: TabfColorDecorationDesc read FSelected write SetSelected;
    property Active  : TabfColorDecorationDesc read FActive   write SetActive;
    property Disabled: TabfColorDecorationDesc read FDisabled write SetDisabled;
    property SizeX: Integer read FSizeX write SetSizeX default 1;
    property SizeY: Integer read FSizeY write SetSizeY default 1;
  end;

//==============================================================================
// TabfRunTimeBorderDecorationDesc
//==============================================================================
// Border decoration 4 run-time states property descriptor.

  TabfRunTimeBorderDecorationDesc = class(TabfRunTimeDesc)
  private
    FNormal  : TabfBorderDecorationDesc;
    FSelected: TabfBorderDecorationDesc;
    FActive  : TabfBorderDecorationDesc;
    FDisabled: TabfBorderDecorationDesc;
    function  GetCurrent: TabfBorderDecorationDesc;
    procedure SetNormal  (A: TabfBorderDecorationDesc);
    procedure SetSelected(A: TabfBorderDecorationDesc);
    procedure SetActive  (A: TabfBorderDecorationDesc);
    procedure SetDisabled(A: TabfBorderDecorationDesc);
  protected
    procedure SetOnChange(A: TNotifyEvent); override;
  public
    constructor Create(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack); override;
    constructor CreateWithData(AOwner: TPersistent;
      ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
      ADisabled: TabfBorderDecorationDesc); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  // Properties
    property Current: TabfBorderDecorationDesc read GetCurrent;
  published
    property Normal  : TabfBorderDecorationDesc read FNormal   write SetNormal;
    property Selected: TabfBorderDecorationDesc read FSelected write SetSelected;
    property Active  : TabfBorderDecorationDesc read FActive   write SetActive;
    property Disabled: TabfBorderDecorationDesc read FDisabled write SetDisabled;
  end;

{******************************************************************************}
implementation
{******************************************************************************}

//==============================================================================
// TabfPropertyDesc
//==============================================================================
// TabfPropertyDesc is a prototype of all property descriptors.
// Date: 04/18/2001
{ TabfPropertyDesc }

constructor TabfPropertyDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  FOwner    := AOwner;
  FCallBack := ACallBack;
  inherited Create;
end;

//------------------------------------------------------------------------------

procedure TabfPropertyDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfPropertyDesc) then
    with (Source as TabfPropertyDesc) do
    begin
      Self.FOwner    := Owner;
      Self.FOnChange := OnChange;
      Self.FCallBack := CallBack;
      Invalidate;
      Exit;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfPropertyDesc.Invalidate;
begin
  if Assigned(CallBack) then CallBack;
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TabfPropertyDesc.SetOnChange(A: TNotifyEvent);
begin
  FOnChange := A;
end;


//==============================================================================
// TabfMarginsDesc
//==============================================================================
// Descriptor for margins properties
{ TabfMarginsDesc }

constructor TabfMarginsDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  inherited Create(AOwner, ACallBack);
  FLeft   := 0.0;
  FTop    := 0.0;
  FRight  := 0.0;
  FBottom := 0.0;
  FUnits  := muPixels;
end;

//------------------------------------------------------------------------------

constructor TabfMarginsDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ALeft, ATop, ARight, ABottom: Double;
  AUnits: TabfMarginsUnits);
begin
  Create(AOwner, ACallBack);
  FLeft   := Left;
  FTop    := Top;
  FRight  := Right;
  FBottom := Bottom;
  FUnits  := Units;
end;

//------------------------------------------------------------------------------

procedure TabfMarginsDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfMarginsDesc) then
    with TabfMarginsDesc(Source) do
    begin
      Self.FLeft   := Left;
      Self.FTop    := Top;
      Self.FRight  := Right;
      Self.FBottom := Bottom;
      Self.FUnits  := Units;
    end;
  inherited Assign(Source);
end;

//==============================================================================
// Properties Get/Set

procedure TabfMarginsDesc.SetLeft(Value: Double);
begin
  if Left = Value then Exit;
  FLeft := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfMarginsDesc.SetTop(Value: Double);
begin
  if Top = Value then Exit;
  FTop := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfMarginsDesc.SetRight(Value: Double);
begin
  if Right = Value then Exit;
  FRight := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfMarginsDesc.SetBottom(Value: Double);
begin
  if Bottom = Value then Exit;
  FBottom := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfMarginsDesc.SetUnits(Value: TabfMarginsUnits);
begin
  if Units = Value then Exit;
  FUnits := Value;
  Invalidate;
end;


//==============================================================================
// TabfTextDecorationDesc
//==============================================================================
// Descriptor for text decorations.
// Date: 04/18/2001
{ TabfTextDecorationDesc }

constructor TabfTextDecorationDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  inherited Create(AOwner, ACallBack);
  FColor := clBlack;
  FStyle := [];
end;

//------------------------------------------------------------------------------

constructor TabfTextDecorationDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; AColor: TColor; AStyle: TFontStyles);
begin
  Create(AOwner, ACallBack);
  FColor := AColor;
  FStyle := AStyle;
end;

//------------------------------------------------------------------------------

procedure TabfTextDecorationDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfTextDecorationDesc) then
    with (Source as TabfTextDecorationDesc) do
    begin
      Self.FColor := Color;
      Self.FStyle := Style;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfTextDecorationDesc.SetColor(A: TColor);
begin
  if FColor = A then Exit;
  FColor := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfTextDecorationDesc.SetStyle(A: TFontStyles);
begin
  if FStyle = A then Exit;
  FStyle := A;
  Invalidate;
end;


//==============================================================================
// TabfColorDecorationDesc
//==============================================================================
// Descriptor for color decorations.
// Date: 04/18/2001
{ TabfColorDecorationDesc }

constructor TabfColorDecorationDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  inherited Create(AOwner, ACallBack);
  FColor   := clWhite;
  FVisible := True;
end;

//------------------------------------------------------------------------------

constructor TabfColorDecorationDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; AColor: TColor; AVisible: Boolean);
begin
  Create(AOwner, ACallBack);
  FColor := AColor;
  FVisible := AVisible;
end;

//------------------------------------------------------------------------------

procedure TabfColorDecorationDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfColorDecorationDesc) then
    with (Source as TabfColorDecorationDesc) do
    begin
      Self.FColor   := Color;
      Self.FVisible := Visible;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfColorDecorationDesc.SetColor(A: TColor);
begin
  if FColor = A then Exit;
  FColor := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfColorDecorationDesc.SetVisible(A: Boolean);
begin
  if FVisible = A then Exit;
  FVisible := A;
  Invalidate;
end;


//==============================================================================
// TabfBorderDecorationDesc
//==============================================================================
// Descriptor for Border, Shadow or other decorations.
// Date: 04/18/2001
{ TabfBorderDecorationDesc }

constructor TabfBorderDecorationDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  FColor := clBtnShadow;
  FVisible := True;
  FSizeX := 1;
  FSizeY := 1;
  inherited Create(AOwner, ACallBack);
end;

//------------------------------------------------------------------------------

constructor TabfBorderDecorationDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; AVisible: Boolean; AColor: TColor; ASizeX,
  ASizeY: Integer);
begin
  Create(AOwner, ACallBack);
  FVisible := AVisible;
  FColor := AColor;
  FSizeX := ASizeX;
  FSizeY := ASizeY;
end;

//------------------------------------------------------------------------------

procedure TabfBorderDecorationDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfBorderDecorationDesc) then
    with (Source as TabfBorderDecorationDesc) do
    begin
      Self.FVisible := Visible;
      Self.FColor := Color;
      Self.FSizeX := SizeX;
      Self.FSizeY := SizeY;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfBorderDecorationDesc.SetColor(A: TColor);
begin
  if FColor = A then Exit;
  FColor := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfBorderDecorationDesc.SetVisible(A: Boolean);
begin
  if FVisible = A then Exit;
  FVisible := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfBorderDecorationDesc.SetSizeX(A: Integer);
begin
  if FSizeX = A then Exit;
  FSizeX := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfBorderDecorationDesc.SetSizeY(A: Integer);
begin
  if FSizeY = A then Exit;
  FSizeY := A;
  Invalidate;
end;


//==============================================================================
// TabfRunTimeDesc
//==============================================================================
// A prototype of all 4 run-time states property descriptors.
// Date: 04/18/2001
{ TabfRunTimeDesc }

procedure TabfRunTimeDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeDesc) then
    with (Source as TabfRunTimeDesc) do
    begin
      Self.FRunTimeState := RunTimeState;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeDesc.SetRunTimeState(A: TabfRunTimeState);
begin
  if FRunTimeState = A then Exit;
  FRunTimeState := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeDesc.SetUseOnlyNormal(A: Boolean);
begin
  if FUseOnlyNormal = A then Exit;
  FUseOnlyNormal := A;
  Invalidate;
end;


//==============================================================================
// TabfRunTimeBooleanDesc
//==============================================================================
// Boolean 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeBooleanDesc }

constructor TabfRunTimeBooleanDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: Boolean);
begin
  Create(AOwner, ACallBack);
  FNormal   := ANormal;
  FSelected := ASelected;
  FActive   := AActive;
  FDisabled := ADisabled;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBooleanDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeBooleanDesc) then
    with (Source as TabfRunTimeBooleanDesc) do
    begin
      Self.FNormal   := Normal;
      Self.FSelected := Selected;
      Self.FActive   := Active;
      Self.FDisabled := Disabled;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TabfRunTimeBooleanDesc.GetCurrent: Boolean;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBooleanDesc.SetNormal(A: Boolean);
begin
  if FNormal = A then Exit;
  FNormal := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBooleanDesc.SetSelected(A: Boolean);
begin
  if FSelected = A then Exit;
  FSelected := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBooleanDesc.SetActive(A: Boolean);
begin
  if FActive = A then Exit;
  FActive := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBooleanDesc.SetDisabled(A: Boolean);
begin
  if FDisabled = A then Exit;
  FDisabled := A;
  Invalidate;
end;


//==============================================================================
// TabfRunTimeColorDesc
//==============================================================================
// TColor 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeColorDesc }

constructor TabfRunTimeColorDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TColor);
begin
  Create(AOwner, ACallBack);
  FNormal   := ANormal;
  FSelected := ASelected;
  FActive   := AActive;
  FDisabled := ADisabled;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeColorDesc) then
    with (Source as TabfRunTimeColorDesc) do
    begin
      Self.FNormal   := Normal;
      Self.FSelected := Selected;
      Self.FActive   := Active;
      Self.FDisabled := Disabled;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TabfRunTimeColorDesc.GetCurrent: TColor;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDesc.SetNormal(A: TColor);
begin
  if FNormal = A then Exit;
  FNormal := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDesc.SetSelected(A: TColor);
begin
  if FSelected = A then Exit;
  FSelected := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDesc.SetActive(A: TColor);
begin
  if FActive = A then Exit;
  FActive := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDesc.SetDisabled(A: TColor);
begin
  if FDisabled = A then Exit;
  FDisabled := A;
  Invalidate;
end;


//==============================================================================
// TabfRunTimeCursorDesc
//==============================================================================
// TCursor 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeCursorDesc }

constructor TabfRunTimeCursorDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TCursor);
begin
  Create(AOwner, ACallBack);
  FNormal   := ANormal;
  FSelected := ASelected;
  FActive   := AActive;
  FDisabled := ADisabled;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeCursorDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeCursorDesc) then
    with (Source as TabfRunTimeCursorDesc) do
    begin
      Self.FNormal   := Normal;
      Self.FSelected := Selected;
      Self.FActive   := Active;
      Self.FDisabled := Disabled;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TabfRunTimeCursorDesc.GetCurrent: TCursor;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeCursorDesc.SetNormal(A: TCursor);
begin
  if FNormal = A then Exit;
  FNormal := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeCursorDesc.SetSelected(A: TCursor);
begin
  if FSelected = A then Exit;
  FSelected := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeCursorDesc.SetActive(A: TCursor);
begin
  if FActive = A then Exit;
  FActive := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeCursorDesc.SetDisabled(A: TCursor);
begin
  if FDisabled = A then Exit;
  FDisabled := A;
  Invalidate;
end;


//==============================================================================
// TabfRunTimeBitmapDesc
//==============================================================================
// TBitmap 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeBitmapDesc }

constructor TabfRunTimeBitmapDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  FNormal   := TBitmap.Create;
  FSelected := TBitmap.Create;
  FActive   := TBitmap.Create;
  FDisabled := TBitmap.Create;
  inherited Create(AOwner, ACallBack);
end;

//------------------------------------------------------------------------------

constructor TabfRunTimeBitmapDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TBitmap);
begin
  Create(AOwner, ACallBack);
  FNormal  .Assign(ANormal);
  FSelected.Assign(ASelected);
  FActive  .Assign(AActive);
  FDisabled.Assign(ADisabled);
end;

//------------------------------------------------------------------------------

destructor TabfRunTimeBitmapDesc.Destroy;
begin
  inherited Destroy;
  FNormal  .Free;
  FSelected.Free;
  FActive  .Free;
  FDisabled.Free;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBitmapDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeBitmapDesc) then
    with (Source as TabfRunTimeBitmapDesc) do
    begin
      Self.FNormal  .Assign(Normal);
      Self.FSelected.Assign(Selected);
      Self.FActive  .Assign(Active);
      Self.FDisabled.Assign(Disabled);
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TabfRunTimeBitmapDesc.GetCurrent: TBitmap;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBitmapDesc.SetNormal(A: TBitmap);
begin
  if FNormal = A then Exit;
  FNormal.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBitmapDesc.SetSelected(A: TBitmap);
begin
  if FSelected = A then Exit;
  FSelected.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBitmapDesc.SetActive(A: TBitmap);
begin
  if FActive = A then Exit;
  FActive.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBitmapDesc.SetDisabled(A: TBitmap);
begin
  if FDisabled = A then Exit;
  FDisabled.Assign(A);
  Invalidate;
end;


//==============================================================================
// TabfRunTimePictureDesc
//==============================================================================
// TPicture 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimePictureDesc }

constructor TabfRunTimePictureDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  FNormal   := TPicture.Create;
  FSelected := TPicture.Create;
  FActive   := TPicture.Create;
  FDisabled := TPicture.Create;
  inherited Create(AOwner, ACallBack);
end;

//------------------------------------------------------------------------------

constructor TabfRunTimePictureDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TPicture);
begin
  Create(AOwner, ACallBack);
  FNormal  .Assign(ANormal);
  FSelected.Assign(ASelected);
  FActive  .Assign(AActive);
  FDisabled.Assign(ADisabled);
end;

//------------------------------------------------------------------------------

destructor TabfRunTimePictureDesc.Destroy;
begin
  inherited Destroy;
  FNormal  .Free;
  FSelected.Free;
  FActive  .Free;
  FDisabled.Free;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimePictureDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimePictureDesc) then
    with (Source as TabfRunTimePictureDesc) do
    begin
      Self.FNormal  .Assign(Normal);
      Self.FSelected.Assign(Selected);
      Self.FActive  .Assign(Active);
      Self.FDisabled.Assign(Disabled);
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TabfRunTimePictureDesc.GetCurrent: TPicture;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimePictureDesc.SetNormal(A: TPicture);
begin
  if FNormal = A then Exit;
  FNormal.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimePictureDesc.SetSelected(A: TPicture);
begin
  if FSelected = A then Exit;
  FSelected.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimePictureDesc.SetActive(A: TPicture);
begin
  if FActive = A then Exit;
  FActive.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimePictureDesc.SetDisabled(A: TPicture);
begin
  if FDisabled = A then Exit;
  FDisabled.Assign(A);
  Invalidate;
end;


//==============================================================================
// TabfRunTimeFontDesc
//==============================================================================
// TFont 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeFontDesc }

constructor TabfRunTimeFontDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  FNormal   := TFont.Create;
  FSelected := TFont.Create;
  FActive   := TFont.Create;
  FDisabled := TFont.Create;
  inherited Create(AOwner, ACallBack);
end;

//------------------------------------------------------------------------------

constructor TabfRunTimeFontDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TFont);
begin
  Create(AOwner, ACallBack);
  FNormal  .Assign(ANormal);
  FSelected.Assign(ASelected);
  FActive  .Assign(AActive);
  FDisabled.Assign(ADisabled);
end;

//------------------------------------------------------------------------------

destructor TabfRunTimeFontDesc.Destroy;
begin
  inherited Destroy;
  FNormal  .Free;
  FSelected.Free;
  FActive  .Free;
  FDisabled.Free;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeFontDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeFontDesc) then
    with (Source as TabfRunTimeFontDesc) do
    begin
      Self.FNormal  .Assign(Normal);
      Self.FSelected.Assign(Selected);
      Self.FActive  .Assign(Active);
      Self.FDisabled.Assign(Disabled);
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TabfRunTimeFontDesc.GetCurrent: TFont;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeFontDesc.SetNormal(A: TFont);
begin
  if FNormal = A then Exit;
  FNormal.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeFontDesc.SetSelected(A: TFont);
begin
  if FSelected = A then Exit;
  FSelected.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeFontDesc.SetActive(A: TFont);
begin
  if FActive = A then Exit;
  FActive.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeFontDesc.SetDisabled(A: TFont);
begin
  if FDisabled = A then Exit;
  FDisabled.Assign(A);
  Invalidate;
end;


//==============================================================================
// TabfRunTimeTextDecorationDesc
//==============================================================================
// Text decoration 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeTextDecorationDesc }

constructor TabfRunTimeTextDecorationDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  inherited Create(AOwner, ACallBack);
  FNormal   := TabfTextDecorationDesc.Create(Self, ACallBack);
  FSelected := TabfTextDecorationDesc.Create(Self, ACallBack);
  FActive   := TabfTextDecorationDesc.Create(Self, ACallBack);
  FDisabled := TabfTextDecorationDesc.Create(Self, ACallBack);
end;

//------------------------------------------------------------------------------

constructor TabfRunTimeTextDecorationDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TabfTextDecorationDesc);
begin
  Create(AOwner, ACallBack);
  FNormal  .Assign(ANormal);
  FSelected.Assign(ASelected);
  FActive  .Assign(AActive);
  FDisabled.Assign(ADisabled);
end;

//------------------------------------------------------------------------------

destructor TabfRunTimeTextDecorationDesc.Destroy;
begin
  FNormal  .Free;
  FSelected.Free;
  FActive  .Free;
  FDisabled.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeTextDecorationDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeTextDecorationDesc) then
    with (Source as TabfRunTimeTextDecorationDesc) do
    begin
      Self.FNormal  .Assign(Normal);
      Self.FSelected.Assign(Selected);
      Self.FActive  .Assign(Active);
      Self.FDisabled.Assign(Disabled);
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeTextDecorationDesc.SetOnChange(A: TNotifyEvent);
begin
  inherited SetOnChange(A);
  FNormal  .OnChange := A;
  FSelected.OnChange := A;
  FActive  .OnChange := A;
  FDisabled.OnChange := A;
end;

//------------------------------------------------------------------------------

function TabfRunTimeTextDecorationDesc.GetCurrent: TabfTextDecorationDesc;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeTextDecorationDesc.SetNormal(A: TabfTextDecorationDesc);
begin
  if FNormal = A then Exit;
  FNormal.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeTextDecorationDesc.SetSelected(A: TabfTextDecorationDesc);
begin
  if FSelected = A then Exit;
  FSelected.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeTextDecorationDesc.SetActive(A: TabfTextDecorationDesc);
begin
  if FActive = A then Exit;
  FActive.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeTextDecorationDesc.SetDisabled(A: TabfTextDecorationDesc);
begin
  if FDisabled = A then Exit;
  FDisabled.Assign(A);
  Invalidate;
end;


//==============================================================================
// TabfRunTimeColorDecorationDesc
//==============================================================================
// Color decoration 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeColorDecorationDesc }

constructor TabfRunTimeColorDecorationDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  inherited Create(AOwner, ACallBack);
  FNormal   := TabfColorDecorationDesc.Create(Self, ACallBack);
  FSelected := TabfColorDecorationDesc.Create(Self, ACallBack);
  FActive   := TabfColorDecorationDesc.Create(Self, ACallBack);
  FDisabled := TabfColorDecorationDesc.Create(Self, ACallBack);
  FSizeX := 1;
  FSizeY := 1;
end;

//------------------------------------------------------------------------------

constructor TabfRunTimeColorDecorationDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TabfColorDecorationDesc);
begin
  Create(AOwner, ACallBack);
  FNormal  .Assign(ANormal);
  FSelected.Assign(ASelected);
  FActive  .Assign(AActive);
  FDisabled.Assign(ADisabled);
end;

//------------------------------------------------------------------------------

destructor TabfRunTimeColorDecorationDesc.Destroy;
begin
  FNormal  .Free;
  FSelected.Free;
  FActive  .Free;
  FDisabled.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeColorDecorationDesc) then
    with (Source as TabfRunTimeColorDecorationDesc) do
    begin
      Self.FNormal  .Assign(Normal);
      Self.FSelected.Assign(Selected);
      Self.FActive  .Assign(Active);
      Self.FDisabled.Assign(Disabled);
      Self.FSizeX := SizeX;
      Self.FSizeY := SizeY;
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.SetOnChange(A: TNotifyEvent);
begin
  inherited SetOnChange(A);
  FNormal  .OnChange := A;
  FSelected.OnChange := A;
  FActive  .OnChange := A;
  FDisabled.OnChange := A;
end;

//------------------------------------------------------------------------------

function TabfRunTimeColorDecorationDesc.GetCurrent: TabfColorDecorationDesc;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.SetNormal(A: TabfColorDecorationDesc);
begin
  if FNormal = A then Exit;
  FNormal.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.SetSelected(A: TabfColorDecorationDesc);
begin
  if FSelected = A then Exit;
  FSelected.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.SetActive(A: TabfColorDecorationDesc);
begin
  if FActive = A then Exit;
  FActive.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.SetDisabled(A: TabfColorDecorationDesc);
begin
  if FDisabled = A then Exit;
  FDisabled.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.SetSizeX(A: Integer);
begin
  if FSizeX = A then Exit;
  FSizeX := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeColorDecorationDesc.SetSizeY(A: Integer);
begin
  if FSizeY = A then Exit;
  FSizeY := A;
  Invalidate;
end;


//==============================================================================
// TabfRunTimeBorderDecorationDesc
//==============================================================================
// Border decoration 4 run-time states property descriptor.
// Date: 04/18/2001
{ TabfRunTimeBorderDecorationDesc }

constructor TabfRunTimeBorderDecorationDesc.Create(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack);
begin
  inherited Create(AOwner, ACallBack);
  FNormal   := TabfBorderDecorationDesc.Create(Self, ACallBack);
  FSelected := TabfBorderDecorationDesc.Create(Self, ACallBack);
  FActive   := TabfBorderDecorationDesc.Create(Self, ACallBack);
  FDisabled := TabfBorderDecorationDesc.Create(Self, ACallBack);
end;

//------------------------------------------------------------------------------

constructor TabfRunTimeBorderDecorationDesc.CreateWithData(AOwner: TPersistent;
  ACallBack: TabfObjectCallBack; ANormal, ASelected, AActive,
  ADisabled: TabfBorderDecorationDesc);
begin
  Create(AOwner, ACallBack);
  FNormal  .Assign(ANormal);
  FSelected.Assign(ASelected);
  FActive  .Assign(AActive);
  FDisabled.Assign(ADisabled);
end;

//------------------------------------------------------------------------------

destructor TabfRunTimeBorderDecorationDesc.Destroy;
begin
  FNormal  .Free;
  FSelected.Free;
  FActive  .Free;
  FDisabled.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBorderDecorationDesc.Assign(Source: TPersistent);
begin
  if Source = Self then Exit;
  if (Source is TabfRunTimeBorderDecorationDesc) then
    with (Source as TabfRunTimeBorderDecorationDesc) do
    begin
      Self.FNormal  .Assign(Normal);
      Self.FSelected.Assign(Selected);
      Self.FActive  .Assign(Active);
      Self.FDisabled.Assign(Disabled);
    end;
  inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBorderDecorationDesc.SetOnChange(A: TNotifyEvent);
begin
  inherited SetOnChange(A);
  FNormal  .OnChange := A;
  FSelected.OnChange := A;
  FActive  .OnChange := A;
  FDisabled.OnChange := A;
end;

//------------------------------------------------------------------------------

function TabfRunTimeBorderDecorationDesc.GetCurrent: TabfBorderDecorationDesc;
begin
  if UseOnlyNormal then
  begin
    Result := FNormal;
    Exit;
  end;
  case RunTimeState of
    rtsNormal  : Result := FNormal;
    rtsSelected: Result := FSelected;
    rtsActive  : Result := FActive;
    else         Result := FDisabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBorderDecorationDesc.SetNormal(A: TabfBorderDecorationDesc);
begin
  if FNormal = A then Exit;
  FNormal.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBorderDecorationDesc.SetSelected(A: TabfBorderDecorationDesc);
begin
  if FSelected = A then Exit;
  FSelected.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBorderDecorationDesc.SetActive(A: TabfBorderDecorationDesc);
begin
  if FActive = A then Exit;
  FActive.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfRunTimeBorderDecorationDesc.SetDisabled(A: TabfBorderDecorationDesc);
begin
  if FDisabled = A then Exit;
  FDisabled.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

end{unit abfPropertyDesc}.
