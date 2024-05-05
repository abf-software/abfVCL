{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfLabels;

{$I abf.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, SysUtils, StdCtrls, Forms,
  abfClasses, abfPropertyDesc;

{$IfNDef D3}
const
  crHandPoint = crUpArrow;
{$EndIf D3}

type

//==============================================================================
// TabfCustomLabel
//==============================================================================
// A prototype of all abfXXX labels. Provides a mouse enter/leave support. Can
// draw text on the angle.

  TabfCustomLabel = class(TCustomLabel)
  private
    FAbout: string;
    FAngle: Integer;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
  // Messages routine
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    FMouseInControl: Boolean;
    FAngleFont: HFONT;
    FTextPosition: TPoint;
    procedure Loaded; override;
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure InvalidateWithAdjusting; virtual;
    procedure ReCreateFont; virtual;
    procedure CalcRect(var ARect: TRect); virtual;
    procedure CalcRectByText(var ARect: TRect); virtual;
    procedure DoDrawText(var ARect: TRect;
      AFlags: Integer);{$IfDef D4}override;{$Else}virtual;{$EndIf}
  {$IfNDef D4}
    procedure AdjustBounds; virtual;
    procedure Paint; override;
  {$EndIf D4}
  // Properties Get/Set
    procedure SetAngle(Value: Integer); virtual;
  // Properties
    property About: string read FAbout write FAbout stored False;
    property ParentFont default False;
    property TextPosition: TPoint read FTextPosition;
  // Events
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property Angle: Integer read FAngle write SetAngle default 0;
    property MouseInControl: Boolean read FMouseInControl;
  end;{TabfCustomLabel = class(TCustomLabel)}


//==============================================================================
// TabfLabel
//==============================================================================
// Enhanced label. Provides a mouse enter/leave support. Can draw text on the
// angle.

  TabfLabel = class(TabfCustomLabel)
  public
    property TextPosition;
  published
  // Properties
    property About;
    property Align;
    property Alignment;
    property Angle;
    property AutoSize;
    property Caption;
    property Color;
    property Cursor;
    property DragCursor;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
{$IfDef D3}
    property Layout;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property DragMode;
    property ParentBiDiMode;
{$EndIf D4}
  // Events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseUp;
    property OnStartDrag;
{$IfDef D4}
    property OnStartDock;
    property OnEndDock;
{$EndIf D4}
  end;{TabfLabel = class(TabfCustomLabel)}


//==============================================================================
// TabfCustomActiveLabel
//==============================================================================
// A prototype of the labels that can be active at the run-time.

  TabfActiveLabelColors  = class(TabfRunTimeColorDesc);
  TabfActiveLabelCursors = class(TabfRunTimeCursorDesc);
  TabfActiveLabelColorDecorations = class(TabfRunTimeColorDecorationDesc);
  TabfActiveLabelTextDecorations  = class(TabfRunTimeTextDecorationDesc);

  TabfCustomActiveLabel = class(TabfCustomLabel)
  private
    FRunTimeState: TabfRunTimeState;
    FPressed: Boolean;
    FActive: Boolean;
    FAutoSize: Boolean;
    FEnabled: Boolean;
    FBorders: TabfActiveLabelColorDecorations;
    FShadows: TabfActiveLabelColorDecorations;
    FCursors: TabfActiveLabelCursors;
    FTextDecorations: TabfActiveLabelTextDecorations;
    FColors: TabfActiveLabelColors;
  // Misc
    procedure PropertyDescInit;
    procedure PropertyDescDone;
  // Messages routine
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
    procedure WMMouseMove  (var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp  (var Message: TMessage); message WM_LBUTTONUP;
  protected
    FBitmapBuffer: TBitmap;
    procedure Loaded; override;
  // Drawing
    procedure Paint; override;
    procedure DrawToBuffer; virtual;
  // Updating
    procedure UpdateRunTimeState; virtual;
    procedure UpdateStateProperies; virtual;
  // TextOut routines
    procedure AdjustBounds; override;
    procedure CalcRectByText(var ARect: TRect); override;
    procedure DoDrawText(var ARect: TRect; AFlags: Integer); override;
    procedure InternalDrawText(const ACanvas: TCanvas; const AText: string;
      var ARect: TRect; AFlags: Integer); virtual;
  // Event handlers
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  // Properties Get/Set
    procedure SetRuntimeState(A: TabfRuntimeState); virtual;
    procedure SetActive(A: Boolean); virtual;
    procedure SetAutoSize(A: Boolean); override;
    function  GetEnabled: Boolean;{$IfDef D4}override;{$Else}virtual;{$EndIf}
    procedure SetEnabled(A: Boolean);{$IfDef D4}override;{$Else}virtual;{$EndIf}
    procedure SetBorders(const A: TabfActiveLabelColorDecorations); virtual;
    procedure SetColors(const A: TabfActiveLabelColors); virtual;
    procedure SetCursors(const A: TabfActiveLabelCursors); virtual;
    procedure SetTextDecorations(const A: TabfActiveLabelTextDecorations); virtual;
    procedure SetShadows(const A: TabfActiveLabelColorDecorations); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
  // Properties
    property Active: Boolean read FActive write SetActive default True;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Borders: TabfActiveLabelColorDecorations read FBorders
      write SetBorders;
    property Colors: TabfActiveLabelColors read FColors write SetColors;
    property Cursors: TabfActiveLabelCursors read FCursors write SetCursors;
    property RuntimeState: TabfRuntimeState read FRuntimeState
      write SetRuntimeState default rtsNormal;
    property Pressed: Boolean read FPressed;
    property Shadows: TabfActiveLabelColorDecorations read FShadows
      write SetShadows;
    property TextDecorations: TabfActiveLabelTextDecorations
      read FTextDecorations write SetTextDecorations;
  end;{TabfCustomActiveLabel = Class(TCustomLabel)}


//==============================================================================
// TabfActiveLabel
//==============================================================================
// TabfActiveLabel is a label that can be active at run-time, provides a mouse
// enter/leave support and contains some visual extensions.

  TabfActiveLabel = class(TabfCustomActiveLabel)
  published
  // Properties
    property About;
    property Active;
    property Align;
    property Alignment;
{ TODO -oKARPOLAN : Add angle support... }
//    property Angle;
    property AutoSize;
    property Borders;
    property Caption;
    property Colors;
    property Cursors;
    property DragCursor;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RuntimeState;
    property ShowAccelChar;
    property ShowHint;
    property Shadows;
    property TextDecorations;
    property Transparent;
    property Visible;
    property WordWrap;
{$IfDef D3}
    property Layout;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property DragMode;
    property ParentBiDiMode;
{$EndIf D4}
  // Events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseUp;
    property OnStartDrag;
{$IfDef D4}
    property OnStartDock;
    property OnEndDock;
{$EndIf D4}
  end;{TabfActiveLabel = class(TabfCustomActiveLabel)}

{******************************************************************************}
implementation
{******************************************************************************}
uses
  Math,
  abfSysUtils, abfGraphics;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);

{$I abf_init.inc}

//==============================================================================
// TabfCustomLabel
//==============================================================================
// A prototype of all abfXXX labels. Provides a mouse enter/leave support.
{ TabfCustomLabel }

constructor TabfCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FAngle := 0;
  FTextPosition := Point(0, 0);
  Font.Name := 'Arial';
  Font.Size := 9;
  ParentFont := False;
end;

//------------------------------------------------------------------------------

destructor TabfCustomLabel.Destroy;
begin
  if FAngleFont <> 0 then DeleteObject(FAngleFont);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.Loaded;
begin
  inherited Loaded;
{$IfNDef D4}
  AdjustBounds;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
{$IfNDef D4}
  if Angle <> 0 then AdjustBounds; // Fix for default size applying
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
{$IfNDef D4}
  if Angle <> 0 then AdjustBounds; // Fix for default size applying
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.InvalidateWithAdjusting;
begin
  AdjustBounds;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.ReCreateFont;
var
  LogFont: TLOGFONT;
begin
  GetObject(Font.Handle, SizeOf(LogFont), @LogFont);
  LogFont.lfEscapement := Angle * 10;
  if FAngleFont <> 0 then DeleteObject(FAngleFont);
  FAngleFont := CreateFontIndirect(LogFont);
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.CalcRect(var ARect: TRect);
var
  AWidth, AHeight: Integer;
  ASin, ACos: Extended;
  NRect: TRect;
begin
  CalcRectByText(ARect);
  NRect := ARect;
// Calc rectangle size depending on angle
  ASin := Sin(DegToRad(Angle));
  ACos := Cos(DegToRad(Angle));
  with NRect do
  begin
    AWidth  := Round(Abs((Right - Left) * ACos) + Abs((Bottom - Top) * ASin));
    AHeight := Round(Abs((Bottom - Top) * ACos) + Abs((Right - Left) * ASin));
    ARect := Rect(Left, Top, Left + AWidth, Top + AHeight);
  end;
// Select position of text depending on angle
  with NRect do
    case Angle of
      0..89   : begin
        FTextPosition.X := ARect.Left;
        FTextPosition.Y := ARect.Bottom - Round(Top + Abs((Bottom - Top) * ACos));
      end;
      90..179 : begin
        FTextPosition.X := ARect.Left + Round(Top + Abs((Right - Left) * ACos));
        FTextPosition.Y := ARect.Bottom;
      end;
      180..269: begin
        FTextPosition.X := ARect.Right;
        FTextPosition.Y := ARect.Top + Round(Top + Abs((Bottom - Top) * ACos));
      end;
      270..360: begin
        FTextPosition.X := ARect.Right - Round(Top + Abs((Right - Left) * ACos));;
        FTextPosition.Y := ARect.Top;
      end;
    end;
end;{procedure TabfCustomLabel.CalcRect}

//------------------------------------------------------------------------------

procedure TabfCustomLabel.CalcRectByText(var ARect: TRect);
var
  Text: string;
  AFlags: Word;
begin
  Text := Caption;
  if ((Text = '') or ShowAccelChar and (Text[1] = '&')
    and (Text[2] = #0)) then Text := Text + ' ';
  AFlags := DT_CALCRECT or DT_EXPANDTABS or WordWraps[WordWrap]
    or Alignments[Alignment];
  if not ShowAccelChar then AFlags := AFlags or DT_NOPREFIX;
  Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, AFlags);
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.DoDrawText(var ARect: TRect; AFlags: Integer);
var
  Text: string;
  TextMetric: TTextMetric;
  SavedFont: HFONT;
begin
  if (AFlags and DT_CALCRECT <> 0) then
  begin
    CalcRect(ARect);
    Exit;
  end;
  Text := Caption;
  Canvas.Font := Font;
  
// Check for TrueType font
  GetTextMetrics(Canvas.Handle, TextMetric);
  if (TextMetric.tmPitchAndFamily and TMPF_TRUETYPE) <> 0 then
  begin
    ReCreateFont;
    SavedFont := SelectObject(Canvas.Handle, FAngleFont);
    TextOut(Canvas.Handle, TextPosition.X, TextPosition.Y,
      PChar(Text), Length(Text));
    SelectObject(Canvas.Handle, SavedFont);
  end else
  begin
    TextOut(Canvas.Handle, TextPosition.X, TextPosition.Y,
      PChar(Text), Length(Text));
    Angle := 0;
  end;
end;

//------------------------------------------------------------------------------
{$IfNDef D4}

procedure TabfCustomLabel.AdjustBounds;
var
  DC: HDC;
  X: Integer;
  R: TRect;
  AAlignment: TAlignment;
begin
  if (csReading in ComponentState) or (not AutoSize) then Exit;
  R := ClientRect;
  DC := GetDC(0);
  Canvas.Handle := DC;
  DoDrawText(R, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap]);
  Canvas.Handle := 0;
  ReleaseDC(0, DC);
  X := Left;
  AAlignment := Alignment;
{$IfDef D4}
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
{$EndIf D4}
  if AAlignment = taRightJustify then Inc(X, Width - R.Right);
  SetBounds(X, Top, R.Right, R.Bottom);
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.Paint;
var
  R{$IfDef D3}, R2{$EndIf}: TRect;
  DrawStyle: Longint;
begin
  with Canvas do
  begin
    R := ClientRect;
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(R);
    end;
    Brush.Style := bsClear;
  // DoDrawText takes care of BiDi alignments
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment];
{$IfDef D3}
  // Calculate vertical layout 
    if Layout <> tlTop then
    begin
      R2 := R;
      DoDrawText(R2, DrawStyle or DT_CALCRECT);
      if Layout = tlBottom then OffsetRect(R, 0, Height - R2.Bottom)
      else OffsetRect(R, 0, (Height - R2.Bottom) div 2);
    end;
{$EndIf D3}
    DoDrawText(R, DrawStyle);
  end;
end;

{$EndIf D4}

//==============================================================================
// Properties Get/Set

procedure TabfCustomLabel.SetAngle(Value: Integer);
begin
  Value := Value mod 360;
  if Value < 0 then Value := 360 + Value;
  if FAngle = Value then Exit;
  FAngle := Value;
  InvalidateWithAdjusting;
  if (Angle > 45) and not (csDesigning in ComponentState) then
    if not _ASK then _TADA;
end;


//==============================================================================
// Messages routine

procedure TabfCustomLabel.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := True;
  DoMouseEnter;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TabfCustomLabel.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := False;
  DoMouseLeave;
  inherited;
end;


//==============================================================================
// TabfCustomActiveLabel
//==============================================================================
// A prototype of the labels that can be active at the run-time.
// Date: 04/18/2001
{ TabfCustomActiveLabel }

constructor TabfCustomActiveLabel.Create(AOwner: TComponent);
begin
  FBitmapBuffer := TBitmap.Create;
  inherited Create(AOwner);
  PropertyDescInit;
  FRunTimeState := rtsNormal;
  FActive := True;
  FAutoSize:= True;
  FEnabled := True;
  Font.Name := 'Arial';
  Font.Size := 16;
end;

//------------------------------------------------------------------------------

destructor TabfCustomActiveLabel.Destroy;
begin
  PropertyDescDone;
  inherited Destroy;
  FreeAndNil(FBitmapBuffer);
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.Update;
begin
  inherited Update;
  DrawToBuffer;
  UpdateRunTimeState;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.Loaded;
begin
{$IfDef D4}
  inherited Loaded;
{$EndIf D4}
  InvalidateWithAdjusting;
// All properties are read, so render BitmapBuffer
  DrawToBuffer;
end;

//==============================================================================
// Drawing

procedure TabfCustomActiveLabel.Paint;
var
  R: TRect;
begin
// Check is the BitmapBuffer should be updated
  if (csDesigning in ComponentState)
    or (FBitmapBuffer.Width <> Width) or (FBitmapBuffer.Height <> Height) then
    DrawToBuffer;
// Copy BitmapBuffer to Canvas depending on Trasparent property
  R := ClientRect;
  if Transparent then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.BrushCopy(R, FBitmapBuffer, R, Color);
  end else
    Canvas.CopyRect(R, FBitmapBuffer.Canvas, R);
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.DrawToBuffer;
var
  R{$IfDef D3}, R2{$EndIf}: TRect;
  DrawStyle: Integer;
begin
  UpdateStateProperies;
  with FBitmapBuffer, FBitmapBuffer.Canvas do
  begin
    Width  := Self.Width;
    Height := Self.Height;
  // Fill client area
    R := ClientRect;
    Brush.Color := Self.Color;
    FillRect(R);
  // Otput text
    Brush.Style := bsClear;
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment];
{$IfDef D3}
  // Calculate vertical layout
    if Layout <> tlTop then
    begin
      R2 := R;
      DoDrawText(R2, DrawStyle or DT_CALCRECT);
      if Layout = tlBottom then
        OffsetRect(R, 0, (Height - R2.Bottom))
      else
        OffsetRect(R, 0, (Height - R2.Bottom) div 2);
    end;
{$EndIf D3}
    DoDrawText(R, DrawStyle);
  end;{with ...}
end;{procedure TabfCustomActiveLabel.DrawToBuffer}


//==============================================================================
// Updating

procedure TabfCustomActiveLabel.UpdateRunTimeState;
begin
  if not Enabled then RunTimeState := rtsDisabled
  else
  if (not Visible) or (not Active) then RunTimeState := rtsNormal
  else
  if Pressed then
  begin
    if MouseInControl then RunTimeState := rtsActive
    else RunTimeState := rtsSelected;
    if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  end else
  if MouseInControl then RunTimeState := rtsSelected
  else RunTimeState := rtsNormal;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.UpdateStateProperies;
begin
  Font.Color := TextDecorations.Current.Color;
  Font.Style := TextDecorations.Current.Style;
  Color := Colors.Current;
end;

//==============================================================================
// TextOut Routines

procedure TabfCustomActiveLabel.AdjustBounds;
var
  X: Integer;
  R: TRect;
  AAlignment: TAlignment;
begin
  if (csReading in ComponentState) or (not AutoSize) then Exit;
  R := ClientRect;
  DoDrawText(R, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap]);
  InflateRect(R, Borders.SizeX, Borders.SizeY);
  X := Left;
  AAlignment := Alignment;
{$IfDef D4}
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
{$EndIf D4}
  if AAlignment = taRightJustify then
  begin
    OffsetRect(R, -R.Left, 0);
    Inc(X, Width - R.Right);
  end;
  SetBounds(X, Top, R.Right, R.Bottom);
end;

//------------------------------------------------------------------------------
// Actually is not used right now. Implemented for future support of Angle.

procedure TabfCustomActiveLabel.CalcRectByText(var ARect: TRect);
var
  Text: string;
  AFlags: Word;
begin
  Text := Caption;
  if ((Text = '') or ShowAccelChar and (Text[1] = '&')
    and (Text[2] = #0)) then Text := Text + ' ';
  AFlags := DT_CALCRECT or DT_EXPANDTABS or WordWraps[WordWrap]
    or Alignments[Alignment];
  if not ShowAccelChar then AFlags := AFlags or DT_NOPREFIX;
  InternalDrawText(FBitmapBuffer.Canvas, Text, ARect, AFlags);
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.DoDrawText(var ARect: TRect; AFlags: Integer);
var
  Text: string;
//  SavedFont: HFONT;
begin
  Text := Caption;
  if (AFlags and DT_CALCRECT <> 0) and ((Text = '') or
    (ShowAccelChar and (Text[1] = '&') and (Text[2] = #0))) then
    Text := Text + ' ';

  if not ShowAccelChar then AFlags := (AFlags or DT_NOPREFIX);
  InternalDrawText(FBitmapBuffer.Canvas, Text, ARect, AFlags);

//  InflateRect(ARect, Borders.SizeX, -Borders.SizeY);

{ Next lines for angle support. Don't remove }{
  ReCreateFont;
  SavedFont := SelectObject(FBitmapBuffer.Canvas.Handle, FAngleFont);
  InternalDrawText(FBitmapBuffer.Canvas, Text, ARect, AFlags);
  SelectObject(FBitmapBuffer.Canvas.Handle, SavedFont);
{}
end;

//------------------------------------------------------------------------------
// Draws text and decoration on the ACanvas. Used for drawing in DoDrawText and
// for calculating size in CalcRectByText.

procedure TabfCustomActiveLabel.InternalDrawText(const ACanvas: TCanvas;
  const AText: string; var ARect: TRect; AFlags: Integer);
var
  R: TRect;
begin
  InflateRect(ARect, -Borders.SizeX, -Borders.SizeY);
  R := Rect(ARect.Left, ARect.Top, ARect.Left, ARect.Top);
  UpdateStateProperies;
  ACanvas.Font := Font;

// Draw Shadows if it is needed
  with Shadows, Shadows.Current do
    if Visible then
    begin
      if Alignment = taRightJustify then OffsetRect(ARect, -SizeX, 0);
      R := ARect;
      abfDrawTextShadow(ACanvas, AText, R, Font.Color, Color,
        SizeX, SizeY, AFlags)
    end;

// Draw Borders if it is needed
  with Borders, Borders.Current do
    if Visible then
    begin
      abfDrawTextBorder(ACanvas, AText, ARect, Font.Color, Color,
        SizeX, SizeY, AFlags);
    end;
  abfDrawText(ACanvas, AText, ARect, Font.Color, True{Enabled}, AFlags);
  
// Prepare ARect for output
  UnionRect(ARect, ARect, R);
end;

//==============================================================================
// Event handlers

procedure TabfCustomActiveLabel.DoMouseEnter;
begin
  if (csDesigning in ComponentState) then Exit;

  if Assigned(OnMouseEnter) then OnMouseEnter(Self);
  UpdateRunTimeState;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.DoMouseLeave;
begin
  if (csDesigning in ComponentState) then Exit;

  if Assigned(OnMouseLeave) then OnMouseLeave(Self);
  UpdateRunTimeState;
end;


//==============================================================================
// Properties Get/Set

procedure TabfCustomActiveLabel.SetRuntimeState(A: TabfRuntimeState);
begin
  if FRuntimeState = A then Exit;
  FRuntimeState := A;
// Apply RunTimeState to all property Descriptors
  Borders.RunTimeState := FRuntimeState;
  Colors .RunTimeState := FRuntimeState;
  Cursors.RunTimeState := FRuntimeState;
  Shadows.RunTimeState := FRuntimeState;
  TextDecorations.RunTimeState := FRuntimeState;
// Update other properties
  Cursor := Cursors.Current;
  SetCursor(Screen.Cursors[Cursor]);
// Draw to offscreen buffer 
  DrawToBuffer;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetActive(A: Boolean);
begin
  if FActive = A then Exit;
  FActive := A;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetAutoSize(A: Boolean);
begin
  if FAutoSize = A then Exit;
  FAutoSize := A;
  InvalidateWithAdjusting;
end;

//------------------------------------------------------------------------------

function TabfCustomActiveLabel.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetEnabled(A: Boolean);
begin
  if FEnabled = A then Exit;
{$IfNDef D4}
  inherited Enabled := A;
{$Else D4}
  inherited SetEnabled(A);
{$EndIf D4}
  FEnabled := A;
  UpdateRunTimeState;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetBorders(const A: TabfActiveLabelColorDecorations);
begin
  FBorders.Assign(A);
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetColors(const A: TabfActiveLabelColors);
begin
  FColors.Assign(A);
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetCursors(const A: TabfActiveLabelCursors);
begin
  FCursors.Assign(A);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetTextDecorations(const A: TabfActiveLabelTextDecorations);
begin
  FTextDecorations.Assign(A);
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.SetShadows(const A: TabfActiveLabelColorDecorations);
begin
  FShadows.Assign(A);
  Repaint;
end;


//==============================================================================
// Misc

procedure TabfCustomActiveLabel.PropertyDescInit;
var
  T1, T2, T3, T4: TabfTextDecorationDesc;
  E1, E2, E3, E4: TabfColorDecorationDesc;

  //-------------------------------------

  procedure _InitBorders;
  begin
    E1 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clWhite       , True);
    E2 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clYellow      , True);
    E3 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clLime        , True);
    E4 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBtnHighLight, True);
    try
      FBorders := TabfActiveLabelColorDecorations.CreateWithData(Self,
        InvalidateWithAdjusting, E1, E2, E3, E4);
    finally
      E1.Free; E2.Free; E3.Free; E4.Free;
    end;
  end;{Internal procedure _InitBorders}

  //-------------------------------------

  procedure _InitShadows;
  const
    cVisible: Boolean = False;
  begin
    E1 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBtnShadow   , cVisible);
    E2 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBtnShadow   , cVisible);
    E3 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBtnShadow   , cVisible);
    E4 := TabfColorDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBtnHighLight, cVisible);
    try
      FShadows := TabfActiveLabelColorDecorations.CreateWithData(Self,
        InvalidateWithAdjusting, E1, E2, E3, E4);
    finally
      E1.Free; E2.Free; E3.Free; E4.Free;
    end;
  end;{Internal procedure _InitShadows}

  //-------------------------------------

  procedure _InitTexts;
  begin
    T1 := TabfTextDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBlack, []);
    T2 := TabfTextDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBlue, [fsUnderline]);
    T3 := TabfTextDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clRed, [fsUnderline]);
    T4 := TabfTextDecorationDesc.CreateWithData(Self, InvalidateWithAdjusting, clBtnShadow, []);
    try
      FTextDecorations := TabfActiveLabelTextDecorations.CreateWithData(Self,
        InvalidateWithAdjusting, T1, T2, T3, T4);
    finally
      T1.Free; T2.Free; T3.Free; T4.Free;
    end;
  end;{Internal procedure _InitTextss}

  //-------------------------------------

begin
  _InitBorders;
  FColors := TabfActiveLabelColors.CreateWithData(Self, Invalidate {Repaint},
    clBtnFace, clBtnFace, clBtnFace, clBtnFace);
  FCursors := TabfActiveLabelCursors.CreateWithData(Self, nil,
    crDefault, crHandPoint, crHandPoint, crDefault);
  _InitShadows;
  _InitTexts;
end;{procedure TabfCustomActiveLabel.PropertyDescInit}

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.PropertyDescDone;
begin
  FColors.Free;
  FBorders.Free;
  FCursors.Free;
  FShadows.Free;
  FTextDecorations.Free;
end;


//==============================================================================
// Messages routine

procedure TabfCustomActiveLabel.CMFontChanged(var Message: TMessage);
begin
  InvalidateWithAdjusting;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  DrawToBuffer;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.CMTextChanged(var Message: TMessage);
begin
  DrawToBuffer;
  InvalidateWithAdjusting;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.CMWinIniChange(var Message: TWMWinIniChange);
begin
  inherited;
  DrawToBuffer;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.WMMouseMove(var Message: TWMMouseMove);
var
  OldValue: Boolean;
begin
  OldValue := FMouseInControl;
  with Message do
    FMouseInControl := PtInRect(ClientRect, Point(XPos, YPos));
  if FMouseInControl <> OldValue then UpdateRunTimeState;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.WMLButtonDown(var Message: TMessage);
begin
  FPressed := True;
  UpdateRunTimeState;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TabfCustomActiveLabel.WMLButtonUp(var Message: TMessage);
begin
  FPressed := False;
  UpdateRunTimeState;
  inherited;
end;


//==============================================================================
// Routines for initialization/finalization.
//==============================================================================

procedure _DoInitialization;
begin
  if Assigned(Screen) then
  begin
    if OSVersion.Check(5) then
      Screen.Cursors[crHandPoint] := LoadCursor(0, IDC_HAND);
  end;
  
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfCustomLabel, TabfCustomActiveLabel, TabfLabel,
    TabfActiveLabel]);
end;

//------------------------------------------------------------------------------

procedure _DoFinalization;
begin
end;

{******************************************************************************}
initialization
{******************************************************************************}

  _DoInitialization;

{******************************************************************************}
finalization
{******************************************************************************}

  _DoFinalization;

end{unit abfLabels}.
