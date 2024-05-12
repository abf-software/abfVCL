{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComboBoxes;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, Classes, Controls, Graphics, StdCtrls, Forms,
  abfControls;

type

//==============================================================================
// TabfDrawComboBox
//==============================================================================
// A prototype of custom drawing comboboxes.

  TabfDrawComboBoxStyle = csDropDown..csDropDownList;

  TabfDrawComboBox = class(TabfCustomComboBox)
  private
    FBorderStyle: TBorderStyle;
    FStyle: TabfDrawComboBoxStyle;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  // Properties Get/Set
    procedure SetStyle(A: TabfDrawComboBoxStyle);{$IfDef D4}reintroduce;{$Else}virtual;{$EndIf}
    procedure SetBorderStyle(A: TBorderStyle); virtual;
  // Properties
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
  public
    constructor Create(AOwner: TComponent); override;
  // Properties
    property Style: TabfDrawComboBoxStyle read FStyle write SetStyle
      default csDropDownList;
  end;{TabfDrawComboBox = class(TCustomComboBox)}


//==============================================================================
// TabfColorComboBox
//==============================================================================
// A color picker combobox.

  TabfColorItemStyle = (cisColorAndText, cisColor, cisText);
  TabfColorNameStyle = (cnsStandard, cnsFromList, cnsHex6, cnsHex8,
    cnsRGBPercents, cnsRGBDecimal, cnsRGBHex, cnsHexTriple);
  TabfColorsSet = (cls16, cls8, cls2, clsText, clsBackground, clsSystem,
    clsAll);

  TabfColorComboBox = class(TabfDrawComboBox)
  private
    FListSelected: Boolean;
    FColorItemStyle: TabfColorItemStyle;
    FColorNameStyle: TabfColorNameStyle;
    FColorNames: TStrings;
    FColorSample: TBitmap;
    FColorSampleWidth: Integer;
    FColorsSet: TabfColorsSet;
    FSelectedColor: TColor;
    FCustomColor: TColor;
    FCustomColorAutoSelect: Boolean;
    FCustomColorCaption: string;
    FUseColorCustom: Boolean;
    FUseColorNone: Boolean;
    FUseColorDefault: Boolean;
    FUpdate: Boolean;
    FOnChange: TNotifyEvent;
    procedure ColorNamesChange(Sender: TObject);
    procedure ColorSampleChange(Sender: TObject);
  // Properties Get/Set
    procedure SetColorItemStyle(A: TabfColorItemStyle);
    procedure SetColorNameStyle(A: TabfColorNameStyle);
    procedure SetColorNames(const A: TStrings);
    procedure SetColorSample(const A: TBitmap);
    procedure SetColorSampleWidth(A: Integer);
    function  GetColorCount: Integer;
    function  GetColors(Index: Integer): TColor;
    procedure SetColorsSet(A: TabfColorsSet);
    function  GetSelectedColor: TColor;
    procedure SetSelectedColor(A: TColor);
    procedure SetCustomColor(A: TColor);
    procedure SetCustomColorCaption(const A: string);
    procedure SetUseColorDefault(A: Boolean);
    procedure SetUseColorNone(A: Boolean);
    procedure SetUseColorCustom(A: Boolean);
  protected
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); override;
    procedure Loaded; override;
    procedure InitList; virtual;
  // Event handlers
    procedure Change; override;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoChange; dynamic;
    procedure DropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColorName(AColor: TColor): string; virtual;
    function SelectCustomColor: Boolean; virtual;
  // Properties
    property Text;
    property Colors[Index: Integer]: TColor read GetColors;
    property ColorCount: Integer read GetColorCount;
  published
  // Properties
    property About;
    property AutoComplete;
    property AutoDropDown;
    property Color;
    property ColorNames: TStrings read FColorNames write SetColorNames; // Must be before ColorNameStyle
    property ColorItemStyle: TabfColorItemStyle read FColorItemStyle
      write SetColorItemStyle default cisColorAndText;
    property ColorNameStyle: TabfColorNameStyle read FColorNameStyle
      write SetColorNameStyle default cnsStandard;
    property ColorSample: TBitmap read FColorSample write SetColorSample;
    property ColorSampleWidth: Integer read FColorSampleWidth
      write SetColorSampleWidth default 0;
    property ColorsSet: TabfColorsSet read FColorsSet write SetColorsSet
      default cls16;
    property Ctl3D;
    property CustomColor: TColor read FCustomColor write SetCustomColor
      stored FUseColorCustom default clDefault;
    property CustomColorAutoSelect: Boolean read FCustomColorAutoSelect
      write FCustomColorAutoSelect stored FUseColorCustom default True;
    property CustomColorCaption: string read FCustomColorCaption
      write SetCustomColorCaption stored FUseColorCustom;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Etched;
    property Flat;
    property FocusBorder;
    property Font;
    property ListWidth;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor
      default clBlack;
    property Style; // Must be streamed before Items
    property TabOrder;
    property TabStop;
    property UseColorCustom: Boolean read FUseColorCustom
      write SetUseColorCustom default False;
    property UseColorDefault: Boolean read FUseColorDefault
      write SetUseColorDefault default False;
    property UseColorNone: Boolean read FUseColorNone
      write SetUseColorNone default False;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
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
  end;{TabfColorComboBox = class(TabfDrawComboBox)}


//==============================================================================
// TabfFontNameComboBox
//==============================================================================
// A FontName picker combobox.

  TabfFontDevice = (fdScreen, fdPrinter, fdBoth);
  TabfFontItemStyle = (fisFont, fisNameOnly, fisSample);
  TabfFontListOption = (foAnsiOnly, foTrueTypeOnly, foFixedPitchOnly,
    foNoOEMFonts, foOEMFontsOnly, foScalableOnly);
  TabfFontListOptions = set of TabfFontListOption;

  TabfFontNameComboBox = class(TabfDrawComboBox)
  private
    FFontDevice: TabfFontDevice;
    FFontItemSample: string;
    FFontItemStyle: TabfFontItemStyle;
    FFontOptions: TabfFontListOptions;
    FUpdate: Boolean;
    FOnChange: TNotifyEvent;
    FUseSameFont: Boolean;
    procedure Reset;
  // Properties Get/Set
    procedure SetFontDevice(A: TabfFontDevice);
    procedure SetFontItemSample(const A: string);
    procedure SetFontItemStyle(A: TabfFontItemStyle);
    function  GetFontName: TFontName;
    procedure SetFontName(const A: TFontName);
    function  GetFontNameCount: Integer;
    function  GetFontNames(Index: Integer): TFontName;
    procedure SetFontOptions(A: TabfFontListOptions);
    procedure SetUseSameFont(A: Boolean);
  // Messages
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
  protected
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; ARect: TRect;
      State: TOwnerDrawState); override;
    function  GetMinItemHeight: Integer; override;
    procedure Loaded; override;
    procedure InitList; virtual;
  // Event handlers
    procedure Change; override;
    procedure Click; override;
    procedure DoChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // Properties
    property Text;
    property FontNames[Index: Integer]: TFontName read GetFontNames;
    property FontNameCount: Integer read GetFontNameCount;
  published
  // Properties
    property About;
    property AutoComplete;
    property AutoDropDown;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Etched;
    property Flat;
    property FocusBorder;
    property Font;
    property FontDevice: TabfFontDevice read FFontDevice write SetFontDevice
      default fdScreen;
    property FontItemSample: string read FFontItemSample
      write SetFontItemSample;
    property FontItemStyle: TabfFontItemStyle read FFontItemStyle
      write SetFontItemStyle default fisFont;
    property FontName: TFontName read GetFontName write SetFontName;
    property FontOptions: TabfFontListOptions read FFontOptions
      write SetFontOptions default [];
    property ListWidth;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style; // Must be streamed before Items
    property TabOrder;
    property TabStop;
    property Visible;
    property UseSameFont: Boolean read FUseSameFont write SetUseSameFont
      default False;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
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
  end;{TabfFontNameComboBox = class(TabfDrawComboBox)}


//==============================================================================
// TabfFontSizeComboBox
//==============================================================================
// A FontSize picker combobox.

  TabfFontSizeComboBox = class(TabfCustomComboBox)
  private
    FUpdate: Boolean;
    FOnChange: TNotifyEvent;
    FFontName: TFontName;
    procedure SetFontName(A: TFontName);
    function  GetFontSize: Integer;
    procedure SetFontSize(A: Integer);
    function  GetFontSizeCount: Integer;
    function  GetFontSizes(Index: Integer): Integer;
  protected
    procedure CreateWnd; override;
    procedure InitList; virtual;
    procedure Click; override;
    procedure DoChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
  // Properties
    property Text;
    property FontSizes[Index: Integer]: Integer read GetFontSizes;
    property FontSizeCount: Integer read GetFontSizeCount;
  published
  // Properties
    property About;
    property AutoComplete;
    property AutoDropDown;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Etched;
    property Flat;
    property FocusBorder;
    property Font;
    property FontName: TFontName read FFontName write SetFontName;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property ImageList;
    property ItemHeight;
    property ListWidth;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style default csDropDown;
    property TabOrder;
    property TabStop;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
  end;{TabfFontSizeComboBox = class(TabfDrawComboBox)}


{******************************************************************************}
implementation
{******************************************************************************}
{$R *.res}
uses
  SysUtils, Printers, Dialogs, abfSysUtils,
  abfGraphics;

{$I abf_init.inc}

{$IfNDef D4}
var
  {$IfDef Builder}
  HexDisplayPrefix: string = '$';
//  HexDisplayPrefix: string = '0x';
  {$Else Builder}
  HexDisplayPrefix: string = '$';
  {$EndIf Builder}
{$EndIf D4}

//==============================================================================
// TabfDrawComboBox
//==============================================================================
// A prototype of custom drawing comboboxes.
// Date: 03/05/2001
{ TabfDrawComboBox }

constructor TabfDrawComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  inherited Style := csDropDownList;
  FStyle := csDropDownList;
  FBorderStyle := bsSingle;
{$IfDef D3}
  FDoubleBuffered := True;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

procedure TabfDrawComboBox.CreateParams(var Params: TCreateParams);
const
  ComboBoxStyles: array[TabfDrawComboBoxStyle] of DWORD = (CBS_DROPDOWN,
    CBS_SIMPLE, CBS_DROPDOWNLIST);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and (not CBS_DROPDOWNLIST) and (not WS_BORDER) or
      CBS_OWNERDRAWFIXED or ComboBoxStyles[FStyle] or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
   end;
end;

//------------------------------------------------------------------------------

procedure TabfDrawComboBox.CreateWnd;
begin
  inherited CreateWnd;
  UpdateItemHeight;
end;

//------------------------------------------------------------------------------

procedure TabfDrawComboBox.SetStyle(A: TabfDrawComboBoxStyle);
begin
  if FStyle = A then Exit;
  FStyle := A;
  inherited Style := A;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfDrawComboBox.SetBorderStyle(A: TBorderStyle);
begin
  if FBorderStyle = A then Exit;
  FBorderStyle := A;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TabfDrawComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;


//==============================================================================
// TabfColorComboBox
//==============================================================================
// A color picker combobox.
// Date: 10/18/2001
{ TabfColorComboBox }

var
  _ColorDialog: TColorDialog;

//------------------------------------------------------------------------------

constructor TabfColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorNames := TStringList.Create;
  TStringList(FColorNames).OnChange := ColorNamesChange;
  FColorItemStyle := cisColorAndText;
  FColorNameStyle := cnsStandard;
  FColorSample := TBitmap.Create;
  FColorSample.OnChange := ColorSampleChange;
  FColorSampleWidth := 0;
  FColorsSet := cls16;
  FCustomColor := clDefault;
  FCustomColorAutoSelect := True;
  FCustomColorCaption := 'Custom...';
  FUseColorCustom := False;
  FUseColorNone := False;
  FUseColorDefault := False;
end;

//------------------------------------------------------------------------------

destructor TabfColorComboBox.Destroy;
begin
  TStringList(FColorNames).OnChange := nil;
  FColorSample.OnChange := nil;
  FreeAndNil(FColorSample);
  FreeAndNil(FColorNames);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfColorComboBox.GetColorName(AColor: TColor): string;

  //-------------------------------------

 procedure _GetNameFromList;
 var
   i: Integer;
 begin
   for i := 0 to Min(ColorNames.Count, Items.Count) - 1 do
     if AColor = Colors[i] then
     begin
       Result := ColorNames[i];
       Exit;
     end;
   Result := abfColorToString(AColor);
 end;{Internal procedure _GetNameFromList}

  //-------------------------------------

var
  C: TabfColorQuad;
begin
  C := TabfColorQuad(ColorToRGB(AColor));
  case ColorNameStyle of
    cnsStandard   : Result := abfColorToString(AColor);
    cnsFromList   : _GetNameFromList;
    cnsHex6       : Result := IntToHex(Integer(c), 6);
    cnsHex8       : Result := IntToHex(Integer(c), 8);
    cnsRGBPercents: Result := Format('RGB{%.0f%%,%.0f%%,%.0f%%}',
      [C.Red/2.56, C.Green/2.56, C.Blue/2.56]);
    cnsRGBDecimal : Result := Format('RGB{%d,%d,%d}'       , [C.Red, C.Green, C.Blue]);
    cnsRGBHex     : Result := Format('RGB{%.2x,%.2x,%.2x}' , [C.Red, C.Green, C.Blue]);
    cnsHexTriple  : Result := Format('Hex={%.2x,%.2x,%.2x}', [C.Red, C.Green, C.Blue]);
  end;
  if Pos('cl', Result) = 1 then Delete(Result, 1, 2) else
  if Pos(HexDisplayPrefix, Result) = 1 then Delete(Result, 1, 1)
end;

//------------------------------------------------------------------------------
// Brings up a "shared" color dialog.

function TabfColorComboBox.SelectCustomColor: Boolean;
const
  SDefColor = 'FFFFFFFF';

  //-------------------------------------

  procedure _FixCustomColors;
  var
    i, k: Integer;
    S: string;
  begin
  // Remove duplicates. Default color will be 'FFFFFFFF'
    with _ColorDialog, _ColorDialog.CustomColors do
      for i := 0 to Count - 1 do
      begin
        S := Values[Names[i]];
        for k := i + 1 to Count - 1 do
          if Values[Names[k]] = S then Values[Names[k]] := SDefColor;
      end;
  end;{Internal procedure _FixCustomColors}

  //-------------------------------------

  procedure _FillCustomColors;
  var
    i: Integer;
    SColor, SItem: string;
  begin
    with _ColorDialog, _ColorDialog.CustomColors do
    begin
    // Add current color if it is not present. Use first empty slot, or the
    // last slot if all slots are busy
      SColor := IntToHex(ColorToRGB(Color), 6);
      for i := 0 to Count - 1 do
      begin
        SItem := Values[Names[i]];
        if (SItem = SColor) then Break;
        if (SItem = SDefColor) or (i = Count - 1) then
        begin
          Values[Names[i]] := SColor;
          Break;
        end
      end;{for...}
    end;
  end;{Internal procedure _FillCustomColors}

  //-------------------------------------

begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;

// Create dialog on first usage
  if not Assigned(_ColorDialog) then _ColorDialog := TColorDialog.Create(nil);

// Fill dialogs properties
  _ColorDialog.Color := TColor(Items.Objects[0]);
  _FixCustomColors;
  _FillCustomColors; // Add current color to the CustomColors

// Show dialog
  Result := _ColorDialog.Execute;
  if Result then
  begin
    _FillCustomColors; // Add new color to the CustomColors
    CustomColor := _ColorDialog.Color;
    SelectedColor := CustomColor;
    Invalidate;
  end;
end;{function TabfColorComboBox.SelectCustomColor}

//------------------------------------------------------------------------------

procedure TabfColorComboBox.CreateWnd;
var
  OldColor: TColor;
begin
  OldColor := SelectedColor;
  inherited CreateWnd;
  InitList;
  SetSelectedColor(OldColor);
  if SelectedColor <> OldColor then DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);

  //-----------------------------------

  procedure _DrawColorRect(R: TRect);
  var
    AColor: TabfColorQuad;
  begin
    if ColorSampleWidth <> 0 then
      R.Right := Min(R.Left + ColorSampleWidth, R.Right);
    with Canvas do
    begin
    { Draw border arround color square }
      InflateRect(R, -1 , -1);
      AColor := TabfColorQuad(ColorToRGB(Colors[Index]));
      if (odSelected in State) then Pen.Color := clWhite
      else
      if (AColor.Red > 128) or (AColor.Green > 128) or (AColor.Blue > 128) or
        (Brush.Color = Colors[Index]) then Pen.Color := clBlack
      else Pen.Color := Colors[Index];
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    { Don't draw clNone color (Draw X mark) }
      if Colors[Index] = clNone then
      begin
        PenPos := Point(R.Left, R.Top);
        LineTo(R.Right - 1, R.Bottom - 1);
        PenPos := Point(R.Left, R.Bottom - 1);
        LineTo(R.Right - 1, R.Top);
        Exit;
      end;
    { Draw color square }
      InflateRect(R, -1 , -1);
      Brush.Color := Colors[Index];
      FillRect(R);
    end;
  end;{Internal procedure _DrawColorRect}

  //-----------------------------------

  procedure _DrawText(R: TRect; C: TColor; const Text: string);
  begin
    Inc(R.Left, 2);
    with Canvas do
    begin
      Font := Self.Font;
      if (odSelected in State) and (ColorItemStyle <> cisText) then
        Font.Color := clHighlightText
      else
        Font.Color := C;
      Brush.Color := Self.Color;
      Brush.Style := bsClear;
      DrawText(Canvas.Handle, PChar(Text), Length(Text), R,
  {$IfDef D4}
        DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
  {$Else  D4}
        DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  {$EndIf D4}
    end;
  end;{Internal procedure _DrawText}

  //-----------------------------------

  procedure _DrawColor(R: TRect);
  var
    ABitmap: TBitmap;
    ACopyMode, EW: Integer;
  begin
    if ColorSample.Empty then
    begin
      if (Index = 0) and UseColorCustom and (ColorItemStyle = cisColor) then
      begin
     { Draw a smaller rect for Custom Color }
        if ColorSampleWidth <> 0 then R.Right := R.Left + ColorSampleWidth;
        EW := Canvas.TextWidth('...') + 4;
        R.Right := Max(R.Right - EW, R.Left);
        _DrawText(Rect(R.Right, R.Top, R.Right + EW, R.Bottom), Font.Color,
          '...');
      end;
      _DrawColorRect(R);
      Exit;
    end;
  { Don't draw clNone color }
    if Colors[Index] = clNone then Exit;
  { Draw a sample bitmap }
    InflateRect(R, -1 , -1);
    ABitmap := TBitmap.Create;
    with ABitmap, ABitmap.Canvas do
    try
      Height := R.Bottom - R.Top;
      if ColorSampleWidth = 0 then Width := Height
      else Width := ColorSampleWidth;
      Brush.Color := Colors[Index];
      FillRect(Rect(0, 0, Width, Height));
      if Brush.Color <> clWhite then ACopyMode := SRCPAINT
      else ACopyMode := NOTSRCCOPY ;
      StretchBlt(Handle, 0, 0, Width, Height,
        FColorSample.Canvas.Handle, 0, 0, FColorSample.Width, FColorSample.Height,
        ACopyMode);
{$IfNDef D3}
      with R do
        Canvas.BrushCopy(Bounds(Left, (Top + Bottom - Height) div 2, Width,
          Height), ABitmap, Bounds(0, 0, Width, Height), TransparentColor);
{$Else D3}
      Transparent := True;
      Self.Canvas.Draw(R.Left, R.Top, ABitmap);
{$EndIf D3}
    finally
      ABitmap.Free;
    end;
  end;{Internal procedure _DrawColor}

  //-----------------------------------

  procedure _DrawColorAndText;
  var
    R: TRect;
  begin
  { Draw Color rect }
    R := ARect;
    if ColorSampleWidth = 0 then R.Right := R.Left + (R.Bottom - R.Top)
    else R.Right := R.Left + ColorSampleWidth;
    _DrawColor(R);
  { Draw Text }
    R.Left := R.Right;
    R.Right := ARect.Right;
    _DrawText(R, Font.Color, Items[Index]);
  end;{Internal procedure _DrawColorAndText}

  //-----------------------------------

var
  OldColor: TColor;
begin
  OldColor := Canvas.Brush.Color;
  try
    Canvas.FillRect(ARect);
    case ColorItemStyle of
      cisColorAndText: _DrawColorAndText;
      cisColor       : _DrawColor(ARect);
      cisText        : _DrawText(ARect, Colors[Index], Items[Index]);
    end;
  finally
    Canvas.Brush.Color := OldColor;
  end
end;{procedure TabfColorComboBox.DrawItem}

//------------------------------------------------------------------------------

procedure TabfColorComboBox.Loaded;
begin
  inherited Loaded;
  InitList;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.InitList;

  //-----------------------------------

  procedure _AddColor(AColor: TColor);
  var
    AddPos: Integer;
  begin
  // We need add an item and then apply the color name. cnsFromList workaraund.
    AddPos := Items.AddObject('dummy', TObject(AColor));
    Items[AddPos] := GetColorName(AColor);
  end;{Internal procedure _AddColor}

  //-----------------------------------

  procedure _ColorsBase16;
  var i: Integer;
  begin
    for i := Low(CabfColors16) to High(CabfColors16) do _AddColor(CabfColors16[i]);
  end;{Internal procedure _ColorsBase16}

  //-----------------------------------

  procedure _ColorsBase8;
  var i: Integer;
  begin
    for i := Low(CabfColors8) to High(CabfColors8) do _AddColor(CabfColors8[i]);
  end;{Internal procedure _ColorsBase8}

  //-----------------------------------

  procedure _ColorsBase2;
  var i: Integer;
  begin
    for i := Low(CabfColors2) to High(CabfColors2) do _AddColor(CabfColors2[i]);
  end;{Internal procedure _ColorsBase2}

  //-----------------------------------

  procedure _ColorsText;
  var i: Integer;
  begin
    for i := Low(CabfTextColors) to High(CabfTextColors) do _AddColor(CabfTextColors[i]);
  end;{Internal procedure _ColorsText}

  //-----------------------------------

  procedure _ColorsBackground;
  var i: Integer;
  begin
    for i := Low(CabfBackgroundColors) to High(CabfBackgroundColors) do _AddColor(CabfBackgroundColors[i]);
  end;{Internal procedure _ColorsBackground}

  //-----------------------------------

  procedure _ColorsSystem;
  var
    i: Integer;
  begin
    for i := 0 to COLOR_INFOBK do
      _AddColor(TColor(DWORD(i) or clSystemColor));
      
    for i := COLOR_HOTLIGHT to COLOR_ENDCOLORS do
      _AddColor(TColor(DWORD(i) or clSystemColor));
  end;{Internal procedure _ColorsSystem}

  //-----------------------------------

begin
  if ComponentState * [csLoading, csReading, csDestroying] <> [] then Exit;

  Items.BeginUpdate;
  try
    Clear;
    if UseColorCustom then
      Items.AddObject(CustomColorCaption, TObject(CustomColor));

    if UseColorDefault then _AddColor(clDefault);

    case ColorsSet of
      cls16        : _ColorsBase16;
      cls8         : _ColorsBase8;
      cls2         : _ColorsBase2;
      clsText      : _ColorsText;
      clsBackground: _ColorsBackground;
      clsSystem    : _ColorsSystem;
      clsAll       : begin
        _ColorsBase16;
        _ColorsSystem;
      end;
    end;{case ColorsSet of}

    if UseColorNone then _AddColor(clNone);

  finally
    Items.EndUpdate;
  end;

  SetSelectedColor(FSelectedColor);
end;{procedure TabfColorComboBox.InitList}


//==============================================================================
// Event handlers

procedure TabfColorComboBox.Change;
var
  i: Integer;
begin
  inherited Change;
  if Style = csDropDownList then Exit;
  i := Items.IndexOf(inherited Text);
  if (i >= 0) and (i <> ItemIndex) then
  begin
    ItemIndex := i;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.Click;
begin
  if (ItemIndex = 0) and UseColorCustom and FListSelected
    and CustomColorAutoSelect then
    SelectCustomColor
  else
  if ItemIndex >= 0 then SelectedColor := Colors[ItemIndex];
  inherited Click;
  FListSelected := False; // Reset flag
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FListSelected := False; // Reset flag
  inherited KeyDown(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = #13) and (ItemIndex = 0) and UseColorCustom then
  begin
    if SelectCustomColor then Click; // Apply custom color change
    Key := #0;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.DoChange;
begin
  if (csReading in ComponentState) then Exit;
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.DropDown;
begin
  inherited DropDown;
  FListSelected := True; // Set flag to determine selection from the list
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.ColorNamesChange(Sender: TObject);
begin
  if ColorNameStyle <> cnsFromList then Exit;
  InitList;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.ColorSampleChange(Sender: TObject);
begin
  if FUpdate then Exit;
  FUpdate := True;
  try
{$IfNDef D3}
    FColorSample.Monochrome := True;
{$Else D3}
    FColorSample.Mask(FColorSample.TransparentColor);
    FColorSample.PixelFormat := pf1bit;
{$EndIf D3}
    Repaint;
  finally
    FUpdate := False;
  end
end;


//==============================================================================
// Properties Get/Set

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetColorItemStyle(A: TabfColorItemStyle);
begin
  if FColorItemStyle = A then Exit;
  FColorItemStyle := A;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetColorNameStyle(A: TabfColorNameStyle);
begin
  if FColorNameStyle = A then Exit;
  FColorNameStyle := A;
  InitList;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetColorNames(const A: TStrings);
begin
  FColorNames.Assign(A);
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetColorSample(const A: TBitmap);
begin
  FColorSample.Assign(A);
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetColorSampleWidth(A: Integer);
begin
  if (ColorSampleWidth = A) or (A < 0) or (A > 2048) then Exit;
  FColorSampleWidth := A;
  Repaint;
end;

//------------------------------------------------------------------------------

function TabfColorComboBox.GetColorCount: Integer;
begin
  Result := Items.Count;
end;

//------------------------------------------------------------------------------

function TabfColorComboBox.GetColors(Index: Integer): TColor;
begin
  Result := TColor(Items.Objects[Index]);
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetColorsSet(A: TabfColorsSet);
begin
  if FColorsSet = A then Exit;
  FColorsSet := A;
  InitList;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TabfColorComboBox.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetSelectedColor(A: TColor);
var
  i: Integer;
begin
  if (FSelectedColor = A) and (ItemIndex >= 0) then Exit;
  FSelectedColor := A;
{ Search for item with same color }
  for i := 0 to Items.Count - 1 do
    if FSelectedColor = Colors[i] then
    begin
      if ItemIndex <> i then ItemIndex := i;
      DoChange;
      Exit;
    end;
{ No such item, set only text or default selection }
  if Style <> csDropDownList then inherited Text := GetColorName(FSelectedColor)
  else
{  if UseColorCustom then
  begin
    if
    CustomColor := FSelectedColor;
    ItemIndex := 0;
  end else}
    ItemIndex := -1;
  DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetCustomColor(A: TColor);
begin
  if FCustomColor = A then Exit;
  FCustomColor := A;
  if UseColorCustom  and (Items.Count > 0) then
    Items.Objects[0] := TObject(CustomColor);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetCustomColorCaption(const A: string);
begin
  if FCustomColorCaption = A then Exit;
  FCustomColorCaption := A;
  InitList;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetUseColorCustom(A: Boolean);
begin
  if FUseColorCustom = A then Exit;
  FUseColorCustom := A;
  InitList;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetUseColorDefault(A: Boolean);
begin
  if FUseColorDefault = A then Exit;
  FUseColorDefault := A;
  InitList;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TabfColorComboBox.SetUseColorNone(A: Boolean);
begin
  if FUseColorNone = A then Exit;
  FUseColorNone := A;
  InitList;
  Invalidate;
end;


//==============================================================================
// TabfFontNameComboBox
//==============================================================================
// A FontName picker combobox.
// Date: 04/01/2000
{ TabfFontNameComboBox }

var
  _BmpTTF, _BmpPrn: TBitmap;

//------------------------------------------------------------------------------

function _CreateFontBitmap(const ResName: string): TBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromResourceName(HInstance, ResName);
{$IfDef D3}
  Result.Transparent := True;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

function _IsValidFont(Box: TabfFontNameComboBox; LogFont: TLogFont;
  FontType: Integer): Boolean;
begin
  Result := True;
  if foAnsiOnly in Box.FontOptions then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if foTrueTypeOnly in Box.FontOptions then
    Result := Result and (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE);
  if foFixedPitchOnly in Box.FontOptions then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH);
  if foOEMFontsOnly in Box.FontOptions then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if foNoOEMFonts in Box.FontOptions then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if foScalableOnly in Box.FontOptions then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

//------------------------------------------------------------------------------

{$IFDEF WIN32}
function _EnumFontsProc(var EnumLogFont: TEnumLogFont;
  var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
{$ELSE}
function _EnumFontsProc(var EnumLogFont: TEnumLogFont;
  var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): NativeInt;
  export; stdcall;
{$ENDIF}
var
  FaceName: string;
begin
  FaceName := StrPas(EnumLogFont.elfLogFont.lfFaceName);
  with TabfFontNameComboBox(Data) do
    if (Items.IndexOf(FaceName) < 0) and
      _IsValidFont(TabfFontNameComboBox(Data), EnumLogFont.elfLogFont, FontType) then
      Items.AddObject(FaceName, TObject(FontType));
  Result := 1;
end;

{------------------------------------------------}

constructor TabfFontNameComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFontDevice  := fdScreen;
  FFontItemSample := 'AaBb����';
  FFontItemStyle := fisFont;
  FFontOptions := [];
  FUseSameFont := False;
  Sorted := True;
  inherited Text := Font.Name;
  inherited ItemHeight := GetMinItemHeight;
end;

//------------------------------------------------------------------------------

destructor TabfFontNameComboBox.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.CreateWnd;
var
  OldFont: TFontName;
begin
  OldFont := FontName;
  inherited CreateWnd;
  FUpdate := True;
  try
    InitList;
    inherited Text := '';
    SetFontName(OldFont);
  finally
    FUpdate := False;
  end;
  if AnsiCompareText(FontName, OldFont) <> 0 then DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);

  //-----------------------------------

  procedure _DrawSymbol;
  var
    FontType: Integer;
    Bitmap: TBitmap;
  begin
    FontType := Integer(Items.Objects[Index]);
  { Select symbol bitmap }
    if (FontType and TRUETYPE_FONTTYPE) <> 0 then Bitmap := _BmpTTF
    else
    if (FontType and DEVICE_FONTTYPE) <> 0 then Bitmap := _BmpPrn
    else Bitmap := nil;
  { Draw symbol bitmap if needed }
    if not Assigned(Bitmap) then Exit;
{$IfNDef D3}
    with ARect do
      Canvas.BrushCopy(Bounds(Left + 2, (Top + Bottom - Bitmap.Height) div 2,
        Bitmap.Width, Bitmap.Height), Bitmap, Bounds(0, 0, Bitmap.Width,
        Bitmap.Height), Bitmap.TransparentColor);
{$Else D3}
    with ARect do
      Canvas.Draw(Left + 2, (Top + Bottom - Bitmap.Height) div 2, Bitmap);
{$EndIf D3}
  end;{Internal procedure _DrawSymbol}

  //-----------------------------------

  procedure _DrawText(const Text: string; IsSymbolDrawn: Boolean);
  begin
    if not UseSameFont then Canvas.Font.Name := Items[Index];
    if not IsSymbolDrawn then ARect.Left := ARect.Left + 2
    else ARect.Left := ARect.Left + _BmpTTF.Width + 4;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect,
{$IfDef D4}
      DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
{$Else}
      DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
{$EndIf D4}
  end;{Internal procedure _DrawText}

begin
  Canvas.FillRect(ARect);
  case FontItemStyle of
    fisFont: begin
      _DrawSymbol;
      _DrawText(Items[Index], True);
    end;
    fisNameOnly: begin
      _DrawText(Items[Index], False);
    end;
    fisSample: begin
      _DrawSymbol;
      _DrawText(FontItemSample, True);
    end;
  end;
end;{procedure TabfFontNameComboBox.DrawItem}

//------------------------------------------------------------------------------

function TabfFontNameComboBox.GetMinItemHeight: Integer;
begin
  Result := Max(inherited GetMinItemHeight, _BmpTTF.Height - 1);
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.Loaded;
begin
  inherited Loaded;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.InitList;
var
  DC: HDC;

  procedure _InitScreen;
  begin
    {$IFDEF WIN32}
    EnumFontFamilies(DC, nil, @_EnumFontsProc, Longint(Self));
    {$ELSE}
    EnumFontFamilies(DC, nil, @_EnumFontsProc, NativeInt(Self));
    {$ENDIF}
  end;{Internal procedure _InitScreen}

  //-----------------------------------

  procedure _InitPrinter;
  begin
    try
    {$IFDEF WIN32}
      EnumFontFamilies(Printer.Handle, nil, @_EnumFontsProc, Longint(Self));
    {$ELSE}
      EnumFontFamilies(Printer.Handle, nil, @_EnumFontsProc, NativeInt(Self));
    {$ENDIF}
    except
    end;
  end;{Internal procedure _InitPrinter}

begin
  if not HandleAllocated then Exit;
  Items.BeginUpdate;
  try
    Clear;
    DC := GetDC(0);
    try
      case FontDevice of
        fdScreen : _InitScreen;
        fdPrinter: _InitPrinter;
        fdBoth   : begin
          _InitScreen;
          _InitPrinter;
        end;
      end;{case FontDevice of}
    finally
      ReleaseDC(0, DC);
    end;
  finally
    Items.EndUpdate;
  end;
  SetFontName(FontName);
end;{procedure TabfFontNameComboBox.InitList}


//==============================================================================
// Event handlers

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.Change;
var
  i: Integer;
begin
  inherited Change;
  if Style = csDropDownList then Exit;
  i := Items.IndexOf(FontName);
  if (i >= 0) and (i <> ItemIndex) then
  begin
    ItemIndex := i;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.Click;
begin
  inherited Click;
  DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.DoChange;
begin
  if not (csReading in ComponentState) then
    if not FUpdate and Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.Reset;
var
  SaveName: TFontName;
begin
  if not HandleAllocated then Exit;
  FUpdate := True;
  try
    SaveName := FontName;
    InitList;
    FontName := SaveName;
  finally
    FUpdate := False;
    if FontName <> SaveName then DoChange;
  end;
end;


//==============================================================================
// Properties Get/Set

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.SetFontDevice(A: TabfFontDevice);
begin
  if FFontDevice = A then Exit;
  FFontDevice := A;
  Reset;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.SetFontItemSample(const A: string);
begin
  if FFontItemSample = A then Exit;
  FFontItemSample := A;
  Repaint;
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.SetFontItemStyle(A: TabfFontItemStyle);
begin
  if FFontItemStyle = A then Exit;
  FFontItemStyle := A;
  Repaint;
end;

//------------------------------------------------------------------------------

function TabfFontNameComboBox.GetFontName: TFontName;
begin
  Result := inherited Text;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.SetFontName(const A: TFontName);
var
  i: Integer;
begin
  if (FontName = A) and (ItemIndex >= 0) then Exit;
{ Search for item contains same size }
  for i := 0 to Items.Count - 1 do
    if AnsiCompareText(Items[i], A) = 0 then
    begin
      ItemIndex := i;
      DoChange;
      Exit;
    end;
{ No such item, set size directly or use default selection }
  if Style <> csDropDownList then inherited Text := A
  else ItemIndex := -1;
  DoChange;
end;{procedure TabfFontNameComboBox.SetFontName}

//------------------------------------------------------------------------------

function TabfFontNameComboBox.GetFontNameCount: Integer;
begin
  Result := Items.Count;
end;

//------------------------------------------------------------------------------

function TabfFontNameComboBox.GetFontNames(Index: Integer): TFontName;
begin
  Result := Items[Index];
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.SetFontOptions(A: TabfFontListOptions);
begin
  if FFontOptions = A then Exit;
  FFontOptions := A;
  Reset;
end;

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.SetUseSameFont(A: Boolean);
begin
  if FUseSameFont = A then Exit;
  FUseSameFont := A;
  Repaint;
end;


//==============================================================================
// Messages

//------------------------------------------------------------------------------

procedure TabfFontNameComboBox.WMFontChange(var Message: TMessage);
begin
  inherited;
  Reset;
end;


//==============================================================================
// TabfFontSizeComboBox
//==============================================================================
// A FontSize picker combobox.
// Date: 04/01/2000
{ TabfFontSizeComboBox }

const
  cFontSizes: array[0..15] of Integer = (
    8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48, 72);
var
  _CurrentFontTTF: Boolean;

//------------------------------------------------------------------------------

function _SetCurrentFontTTF(var LogFont: TEnumLogFont; var ptm: TNewTextMetric;
      FontType: integer; Data: Pointer): Integer; stdcall;
begin
  _CurrentFontTTF := (ptm.tmPitchAndFamily and TMPF_TRUETYPE) = TMPF_TRUETYPE;
  Result := 0;
end;

//------------------------------------------------------------------------------

function _IsFontTrueType(const FontName : string): Boolean;
var
  DC: HDC;
begin
  DC := GetDC(0);
  EnumFontFamilies(DC, PChar(FontName), @_SetCurrentFontTTF, 0);
  Result := _CurrentFontTTF;
  ReleaseDC(0, DC);
end;

//------------------------------------------------------------------------------

{$IFDEF WIN32}
function _EnumFontsBySize(var LogFont: TEnumLogFont; var ptm: TNewTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
{$ENDIF}
{$IFDEF WIN64}
function _EnumFontsBySize(var LogFont: TEnumLogFont; var ptm: TNewTextMetric;
  FontType: Integer; Data: Pointer): NativeInt; stdcall;
{$ENDIF}
var
 Str: string;
 i, Height: Integer;
begin
  Result := 1;
  Height := MulDiv(LogFont.elfLogFont.lfHeight, 72, Screen.PixelsPerInch);
  Str := IntToStr(Height);
  with TStrings(Data) do
  begin
    if IndexOf(Str) >= 0 then Exit;
    for i := 0 to Count - 1 do
    begin
      if StrToIntDef(Strings[i], 0) < Height then Continue;
      Insert(i, Str);
      Exit;
    end;
    Add(Str);
  end;
end;{function _EnumFontsBySize}

//------------------------------------------------------------------------------

constructor TabfFontSizeComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdate := True;
  FFontName := Font.Name;
  inherited Text := IntToStr(Font.Size);
  Style := csDropDown;
  FUpdate := False;
end;

//------------------------------------------------------------------------------

procedure TabfFontSizeComboBox.CreateWnd;
var
  OldFontSize: Integer;
begin
  OldFontSize := FontSize;
  inherited CreateWnd;
  FUpdate := True;
  try
    InitList;
    SetFontSize(OldFontSize);
  finally
    FUpdate := False;
  end;
  if FontSize <> OldFontSize then DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfFontSizeComboBox.InitList;
var
  OldFontSize: Integer;

  //-----------------------------------

  procedure _InitTTF;
  var
    i: Integer;
  begin
    for i := Low(cFontSizes) to High(cFontSizes) do
      Items.Add(IntToStr(cFontSizes[i]));
  end;{Internal procedure _InitTTF}

  //-----------------------------------

  procedure _InitDevice;
  var
    DC: HDC;
    Str: string;
  begin
    DC := GetDC(0);
    try
      Str := FontName;
      {$IFDEF WIN32}
      EnumFontFamilies(DC, PChar(Str), @_EnumFontsBySize, LongInt(Items));
      {$ELSE}
      EnumFontFamilies(DC, PChar(Str), @_EnumFontsBySize, NativeInt(Items));
      {$ENDIF}
    finally
      ReleaseDC(0, DC);
    end;
  end;{Internal procedure _InitDevice}

begin
  OldFontSize := FontSize;
  Items.BeginUpdate;
  try
    Clear;
    if _IsFontTrueType(FontName) then _InitTTF
    else _InitDevice;
  finally
    Items.EndUpdate;
  end;
  SetFontSize(OldFontSize);
end;{procedure TabfFontSizeComboBox.InitList}

//------------------------------------------------------------------------------

procedure TabfFontSizeComboBox.Click;
begin
  inherited Click;
  DoChange;
end;

//------------------------------------------------------------------------------

procedure TabfFontSizeComboBox.DoChange;
begin
  if not (csReading in ComponentState) then
    if not FUpdate and Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TabfFontSizeComboBox.SetFontName(A: TFontName);
begin
  if AnsiCompareText(FFontName, A) = 0 then Exit;
  FFontName := A;
  InitList;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TabfFontSizeComboBox.GetFontSize: Integer;
begin
  Result := StrToIntDef(Text, 0);
end;

//------------------------------------------------------------------------------

procedure TabfFontSizeComboBox.SetFontSize(A: Integer);
var
  i: Integer;
begin
  if (FontSize = A) and (ItemIndex >= 0) then Exit;
{ Search for item contains same size }
  for i := 0 to Items.Count - 1 do
    if StrToIntDef(Items[i], -1) = A then
    begin
      if ItemIndex <> i then ItemIndex := i;
      DoChange;
      Exit;
    end;
{ No such item, set size directly or use default selection }
  if Style <> csDropDownList then Text := IntToStr(A)
  else ItemIndex := -1;
  DoChange;
end;{procedure TabfFontSizeComboBox.SetFontSize}

//------------------------------------------------------------------------------

function TabfFontSizeComboBox.GetFontSizeCount: Integer;
begin
  Result := Items.Count;
end;

//------------------------------------------------------------------------------

function TabfFontSizeComboBox.GetFontSizes(Index: Integer): Integer;
begin
  Result := StrToIntDef(Items[Index], 0);
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

  _BmpTTF := _CreateFontBitmap('FONT_TTF');
  _BmpPrn := _CreateFontBitmap('FONT_PRN');

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfCustomComboBox, TabfComboBox, TabfColorComboBox,
    TabfFontNameComboBox, TabfFontSizeComboBox]);{}

{******************************************************************************}
finalization
{******************************************************************************}
  if Assigned(_ColorDialog) then FreeAndNil(_ColorDialog);

  FreeAndNil(_BmpTTF);
  FreeAndNil(_BmpPrn);

end{unit abfComboBoxes}.

