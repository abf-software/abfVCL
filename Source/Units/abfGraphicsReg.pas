{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfGraphicsReg;

{$I abf.inc}
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
{$IfDef D6}
  DesignIntf, VclEditors,
{$Else D6}
  DsgnIntf,
{$EndIf D6}
  Windows, Classes, SysUtils, Graphics;

type

//==============================================================================
// TabfColorProperty
//==============================================================================
// Color property for all components that enables extended colors set

  TabfColorProperty = class(TColorProperty{$IfDef D6}, ICustomPropertyListDrawing{$EndIf})
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
{$IfDef D6}
  // ICustomPropertyListDrawing
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
{$Else D6}
  {$IfDef D5}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
  {$EndIf D5}
{$EndIf D6}
  end;

//==============================================================================
// Registration
//==============================================================================

procedure Register;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfGraphics;

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
{$IfDef abfColors}
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TabfColorProperty);
{$EndIf abfColors}
end;


//==============================================================================
// TabfColorProperty
//==============================================================================
// Color property for all components that enables extended colors set
// Date: 03/07/2000

function TabfColorProperty.GetValue: string;
begin
  Result := abfColorToString(TColor(GetOrdValue));
end;

//------------------------------------------------------------------------------{--------------------------------------}

procedure TabfColorProperty.GetValues(Proc: TGetStrProc);
begin
  abfGetColorValues(Proc);
end;

//------------------------------------------------------------------------------{--------------------------------------}

procedure TabfColorProperty.SetValue(const Value: string);
var
  NewValue: LongInt;
begin
//  SetOrdValue(abfStringToColor(Value));
  if abfIdentToColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;


//==============================================================================
// ICustomPropertyListDrawing

//------------------------------------------------------------------------------{--------------------------------------}
{$IfDef D5}

procedure TabfColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);

  //-------------------------------------

  function _ColorToBorderColor(AColor: TColor): TColor;
  begin
    if (TabfColorQuad(AColor).Red > 192) or (TabfColorQuad(AColor).Green > 192) or
       (TabfColorQuad(AColor).Blue > 192) then Result := clBlack
    else
    if ASelected then Result := clWhite
    else Result := AColor;
  end;{Internal function _ColorToBorderColor}

  //-------------------------------------

var
  TextRight: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  TextRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  OldPenColor := ACanvas.Pen.Color;
  OldBrushColor := ACanvas.Brush.Color;
  with ACanvas do
  try
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, TextRight, ARect.Bottom);
    Brush.Color := abfStringToColor(Value);
    Pen.Color := _ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, TextRight - 1, ARect.Bottom - 1);
  finally
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    ACanvas.TextRect(Rect(TextRight, ARect.Top, ARect.Right, ARect.Bottom),
      TextRight + 1, ARect.Top + 1, Value);
  end;
end;
{$EndIf D5}

//------------------------------------------------------------------------------{--------------------------------------}


end{unit abfGraphicsReg}.
