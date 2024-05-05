{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfGraphics;

{$I abf.inc}

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics;

const

//==============================================================================
// Colors
//==============================================================================

{$IfNDef D4}
  COLOR_HOTLIGHT = 26;
  COLOR_GRADIENTACTIVECAPTION = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
//  COLOR_ENDCOLORS = 28;
{$EndIf D4}

{$IfNDef D6}
  COLOR_MENUHILIGHT = 29;
  COLOR_MENUBAR = 30;
  COLOR_ENDCOLORS = COLOR_MENUBAR;
{$EndIf D6}

{$IfNDef D7}
  clSystemColor = $80000000;
{$EndIf D7}

// New System colors
{$IfNDef D6}
  clHotLight = TColor(clSystemColor or COLOR_HOTLIGHT);
  clGradientActiveCaption = TColor(clSystemColor or COLOR_GRADIENTACTIVECAPTION);
  clGradientInactiveCaption = TColor(clSystemColor or COLOR_GRADIENTINACTIVECAPTION);
  clMenuHighlight = TColor(clSystemColor or COLOR_MENUHILIGHT);
  clMenuBar = TColor(clSystemColor or COLOR_MENUBAR);
{$EndIf D6}

// New custom colors
  clBrown = TColor($003090);
  clOliveGreen = TColor($003030);
  clDarkGreen = TColor($003000);
  clDarkTeal = TColor($603000);
  clIndigo = TColor($903030);
  clNight = TColor($303030);
  clOrange = TColor($0068FF);
  clBlueGray = TColor($906060);
  clLightOrange = TColor($0098FF);
  clSeaGreen = TColor($609830);
  clLightBlue = TColor($FF6830);
  clMetal = TColor($909090);
  clGold = TColor($00C8FF);
  clSkyBlue = {TColor($FFFBF0);} TColor($FFC800);
  clPlum = TColor($603090);
  clRose = TColor($D098FF);
  clTan = TColor($A0C8FF);
  clLightYellow = TColor($90FFFF);
  clLightGreen = TColor($D0FFD0);
  clLightTurquoise = TColor($FFFFC0);
  clPaleBlue = TColor($FFC890);
  clLavender = TColor($FF98C0);
  clCream = TColor($A6CAF0);
  clMoneyGreen = TColor($C0DCC0);

// Standard 16 colors set
  CabfColors16: array [0..15] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

// Standard 8 colors set
  CabfColors8: array [0..7] of TColor = (
    clBlack, clRed, clGreen, clYellow, clBlue, clPurple, clTeal, clWhite);

// Standard 2 colors set
  CabfColors2: array [0..1] of TColor = (clBlack, clWhite);

// FrontPage 2000 Text colors set
  CabfTextColors: array[0..15] of TColor = (
    clBlack, clWhite, clGreen, clMaroon, clOlive, clNavy, clPurple, clDkGray,
    clYellow, clLime, clAqua, clFuchsia, clLtGray, clRed, clBlue, clTeal);

// Word 2000 Background colors set
  CabfBackgroundColors: array[0..14] of TColor = (
    clYellow, clLime, clAqua, clFuchsia, clBlue,
    clRed, clNavy, clGreen, clTeal, clPurple,
    clMaroon, clOlive, clDkGray, clLtGray, clBlack);


//==============================================================================
// Types
//==============================================================================

type
  TabfColorQuad = packed record
    Red, Green, Blue, Alpha: Byte;
  end;

  TabfARGB = packed record
    B, G, R, A: Byte;
  end;

  TabfRGBA = packed record
    A, B, G, R: Byte;
  end;

  TabfABGR = packed record
    R, G, B, A: Byte;
  end;

  TabfBGRA = packed record
    A, R, G, B: Byte;
  end;


//==============================================================================
// Color translation
//==============================================================================

function abfColorToString(Color: TColor): string;
function abfStringToColor(const S: string): TColor;

function abfColorToARGB(Color: TColor): LongInt;
function abfColorToRGBA(Color: TColor): LongInt;
function abfColorToABGR(Color: TColor): LongInt;
function abfColorToBGRA(Color: TColor): LongInt;


//==============================================================================
// Color enumerates
//==============================================================================

procedure abfGetColorValues(Proc: TGetStrProc);
function abfColorToIdent(Color: LongInt; var Ident: string): Boolean;
function abfIdentToColor(const Ident: string; var Color: LongInt): Boolean;


//==============================================================================
// Determination routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns bit per pixel value for current video mode.
function abfGetCurrentBPP: Integer;

function abfGetColorUnderCursor: TColor;

//------------------------------------------------------------------------------
// Returns average character size for given canvas
function abfGetAverageCharSize(Canvas: TCanvas): TPoint;


//==============================================================================
// Icons related routines
//==============================================================================
// Returns handle of icon from module instance that most closely matches (<=)
// the requested color depth (or the current screen depth)
// and the requested icon size.

function abfGetIconFromInstanceA(AInstance: HINST; AResName: PAnsiChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
function abfGetIconFromInstance(AInstance: HINST; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
function abfGetIconFromInstanceW(AInstance: HINST; AResName: PWideChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;

// Returns handle of icon from library that most closely matches (<=)
// the requested color depth (or the current screen depth)
// and the requested icon size.

function abfGetIconFromLibraryA(const ALibFileName: AnsiString;
  AResName: PAnsiChar; ARequestedIconSize: TPoint;
  ARequestedIconBPP: Integer): HICON;
function abfGetIconFromLibrary(const ALibFileName: string; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
function abfGetIconFromLibraryW(const ALibFileName: WideString;
  AResName: PWideChar; ARequestedIconSize: TPoint;
  ARequestedIconBPP: Integer): HICON;

//------------------------------------------------------------------------------
// Loads icon from module instance that most closely matches (<=)
// the requested color depth (or the current screen depth)
// and the requested icon size.

function abfLoadIconFromInstanceA(AInstance: HINST; AResName: PAnsiChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
function abfLoadIconFromInstance(AInstance: HINST; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
function abfLoadIconFromInstanceW(AInstance: HINST; AResName: PWideChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;

//------------------------------------------------------------------------------
// Loads icon from library that most closely matches (<=)
// the requested color depth (or the current screen depth)
// and the requested icon size.

function abfLoadIconFromLibraryA(const ALibFileName: AnsiString;
  AResName: PAnsiChar; ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
function abfLoadIconFromLibrary(const ALibFileName: string; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
function abfLoadIconFromLibraryW(const ALibFileName: WideString;
  AResName: PWideChar; ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;


//==============================================================================
// Dialog functions
//==============================================================================

function abfDialogUnitsToPixelsX(DialogUnits: Word): Word;
function abfDialogUnitsToPixelsY(DialogUnits: Word): Word;
function abfPixelsToDialogUnitsX(PixelUnits: Word): Word;
function abfPixelsToDialogUnitsY(PixelUnits: Word): Word;


//==============================================================================
// Simple graphic routines
//==============================================================================

//------------------------------------------------------------------------------
// Fills rect on DC with specified color
procedure abfFillRect(ADC: HDC; const ARect: TRect; AColor: TColor);

//------------------------------------------------------------------------------
// Draws a frame rect on DC with specified color
procedure abfFrameRect(ADC: HDC; const ARect: TRect; AColor: TColor);

//==============================================================================
// VCL graphic routines
//==============================================================================

//procedure abfCopyParentImage(AControl: TControl; ACanvas: TCanvas);


//------------------------------------------------------------------------------
// Draws border on the ACanvas inside the ARect area with AColor and of
// ASize wide.
procedure abfDrawWideBorder(const Canvas: TCanvas; const ARect: TRect;
  AColor: TColor; ASize: Integer);

//------------------------------------------------------------------------------
// Draws a simple gradient in the ARect area of the Canvas. If Horizontal is
// True draws horizontal gradien otherwise draws vertical one.
procedure abfDrawGradient(const Canvas: TCanvas; const ARect: TRect;
  ColorBegin, ColorEnd: TColor; Horizontal: Boolean);


//==============================================================================
// Draw text routines
//==============================================================================

procedure abfDrawText(const Canvas: TCanvas; const Text: string;
  var R: TRect; TextColor: TColor; Enabled: Boolean; DrawTextFlags: Integer);
procedure abfDrawTextShadow(const Canvas: TCanvas; const Text: string;
  var R: TRect; TextColor, ShadowColor: TColor; SizeX, SizeY: Integer;
  DrawTextFlags: Integer);
procedure abfDrawTextBorder(const Canvas: TCanvas; const Text: string;
  const R: TRect; TextColor, BorderColor: TColor; SizeX, SizeY: Integer;
  DrawTextFlags: Integer);


//==============================================================================
// Additional standard procedures
//==============================================================================

//------------------------------------------------------------------------------
// Layered window support

function abfSetLayeredWindowAttributes(Handle: THandle; Key: COLORREF;
  Alpha: Byte; Flags: DWORD): Boolean;


{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfClasses, abfSysUtils;

const

//==============================================================================
// Colors
//==============================================================================

{$IfDef D6}
  abfExColorsCount = 20;
{$Else D6}
  abfExColorsCount = 29;
{$EndIf D6}

  abfExColors: array[0..abfExColorsCount] of TIdentMapEntry = (
  // Additional system colors
{$IfNDef D6}
    (Value: clHotLight               ; Name: 'clHotLight'                     ),
    (Value: clGradientActiveCaption  ; Name: 'clGradientActiveCaption'        ),
    (Value: clGradientInactiveCaption; Name: 'clGradientInactiveCaption'      ),
    (Value: clMenuHighlight          ; Name: 'clMenuHighlight'                ),
    (Value: clMenuBar                ; Name: 'clMenuBar'                      ),

    (Value: clDefault                ; Name: 'clDefault'                      ),
{$EndIf D6}
  // Additional colors
    (Value: clBrown                  ; Name: 'clBrown'                        ),
    (Value: clOliveGreen             ; Name: 'clOliveGreen'                   ),
    (Value: clDarkGreen              ; Name: 'clDarkGreen'                    ),
    (Value: clDarkTeal               ; Name: 'clDarkTeal'                     ),
    (Value: clIndigo                 ; Name: 'clIndigo'                       ),
    (Value: clNight                  ; Name: 'clNight'                        ),
    (Value: clOrange                 ; Name: 'clOrange'                       ),
    (Value: clBlueGray               ; Name: 'clBlueGray'                     ),
    (Value: clLightOrange            ; Name: 'clLightOrange'                  ),
    (Value: clSeaGreen               ; Name: 'clSeaGreen'                     ),
    (Value: clLightBlue              ; Name: 'clLightBlue'                    ),
    (Value: clMetal                  ; Name: 'clMetal'                        ),
    (Value: clGold                   ; Name: 'clGold'                         ),
    (Value: clPlum                   ; Name: 'clPlum'                         ),
    (Value: clRose                   ; Name: 'clRose'                         ),
    (Value: clTan                    ; Name: 'clTan'                          ),
    (Value: clLightYellow            ; Name: 'clLightYellow'                  ),
    (Value: clLightGreen             ; Name: 'clLightGreen'                   ),
    (Value: clLightTurquoise         ; Name: 'clLightTurquoise'               ),
    (Value: clPaleBlue               ; Name: 'clPaleBlue'                     ),
    (Value: clLavender               ; Name: 'clLavender'                     )
{$IfNDef D6}
    ,
    (Value: clMoneyGreen             ; Name: 'clMoneyGreen'                   ),
    (Value: clSkyBlue                ; Name: 'clSkyBlue'                      ),
    (Value: clCream                  ; Name: 'clCream'                        )
{$EndIf D6}
  // Add new colors before
  );{abfExColors}


//==============================================================================
// Color translation
//==============================================================================

function abfColorToString(Color: TColor): string;
begin
  if not abfColorToIdent(Color, Result) then
    Result := ColorToString(Color);
end;

//------------------------------------------------------------------------------

function abfStringToColor(const S: string): TColor;
begin
  if not abfIdentToColor(S, LongInt(Result)) then
    Result := StringToColor(S);
end;

//------------------------------------------------------------------------------

function abfColorToRGBA(Color: TColor): LongInt;
var
  ABGR: TabfABGR; // Default color value is AABBGGRR
begin
  ABGR := TabfABGR(abfColorToABGR(Color));
  with TabfRGBA(Result) do
  begin
    R := ABGR.R;
    G := ABGR.G;
    B := ABGR.B;
    A := ABGR.A;
  end;
end;

//------------------------------------------------------------------------------

function abfColorToARGB(Color: TColor): LongInt;
var
  ABGR: TabfABGR; // Default color value is AABBGGRR
begin
  ABGR := TabfABGR(abfColorToABGR(Color));
  with TabfARGB(Result) do
  begin
    R := ABGR.R;
    G := ABGR.G;
    B := ABGR.B;
    A := ABGR.A;
  end;
end;

//------------------------------------------------------------------------------

function abfColorToBGRA(Color: TColor): LongInt;
var
  ABGR: TabfABGR; // Default color value is AABBGGRR
begin
  ABGR := TabfABGR(abfColorToABGR(Color));
  with TabfBGRA(Result) do
  begin
    R := ABGR.R;
    G := ABGR.G;
    B := ABGR.B;
    A := ABGR.A;
  end;
end;

//------------------------------------------------------------------------------

function abfColorToABGR(Color: TColor): LongInt;
begin
  Result := ColorToRGB(Color);
end;


//==============================================================================
// Color enumerates
//==============================================================================

//------------------------------------------------------------------------------
// Enumerates color values

procedure abfGetColorValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  GetColorValues(Proc);
  for i := Low(abfExColors) to High(abfExColors) do
    Proc(abfExColors[i].Name);
end;

//------------------------------------------------------------------------------

function abfColorToIdent(Color: LongInt; var Ident: string): Boolean;
begin
  Result := IntToIdent(Color, Ident, abfExColors);
  if not Result then Result := ColorToIdent(Color, Ident);
end;

//------------------------------------------------------------------------------

function abfIdentToColor(const Ident: string; var Color: LongInt): Boolean;
begin
  Result := IdentToInt(Ident, Color, abfExColors);
  if not Result then Result := IdentToColor(Ident, Color);
end;


//==============================================================================
// Determination routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns bit per pixel value for current video mode.

function abfGetCurrentBPP: Integer;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    Result := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
  finally
    ReleaseDC(0, DC);
  end;
end;

//------------------------------------------------------------------------------

function abfGetColorUnderCursor: TColor;
var
  Pnt: TPoint;
  Handle: THandle;
  DC: HDC;
begin
  GetCursorPos(Pnt);
  Handle := GetDesktopWindow;
  DC := GetWindowDC(Handle);
  try
    Result := GetPixel(DC, Pnt.X, Pnt.Y);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

//------------------------------------------------------------------------------
// Returns average character size for given canvas

function abfGetAverageCharSize(Canvas: TCanvas): TPoint;
var
  i: Integer;
  Buffer: array[0..51] of Char;
begin
  for i := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for i := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;


//==============================================================================
// Icons related routines
//==============================================================================
// Returns handle of icon from module instance that most closely matches (<=)
// the requested color depth (or the current screen depth)
// and the requested icon size.

type
  PGRPICONDIRENTRY = ^GRPICONDIRENTRY;
  GRPICONDIRENTRY = packed record
   bWidth: BYTE;               // Width, in pixels, of the image
   bHeight: BYTE;              // Height, in pixels, of the image
   bColorCount: BYTE;          // Number of colors in image (0 if >=8bpp)
   bReserved: BYTE;            // Reserved
   wPlanes: WORD;              // Color Planes
   wBitCount: WORD;            // Bits per pixel
   dwBytesInRes: DWORD;        // how many bytes in this resource?
   nID: WORD;                  // the ID
  end;
  LPGRPICONDIRENTRY = PGRPICONDIRENTRY;

  PGRPICONDIR = ^GRPICONDIR;
  GRPICONDIR = packed record
   idReserved: WORD;                          // Reserved (must be 0)
   idType: WORD;                              // Resource type (1 for icons)
   idCount: WORD;                             // How many images?
   idEntries: array[0..0] of GRPICONDIRENTRY; // The entries for each image
  end;
  LPGRPICONDIR = PGRPICONDIR;


//------------------------------------------------------------------------------

function _GetIconFromInstance(AInstance: HINST; ARsrc: HRSRC;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
var
  TempRsrc: HRSRC;
  TempGlobal: HGLOBAL;
  TempIconDir: PGRPICONDIR;
  TempBIH: PBitmapInfoHeader;
  SystemBPP: Integer;
  BestIconBPP: Integer;
  BestIconSize: TPoint;
  BestIconIndex: Integer;
  TempIconBPP: Integer;
  TempIconSize: TPoint;
  i: Integer;

  //-------------------------------------

  function BetterSize(const Old, New: TPoint): Boolean;
  var
    NewX, NewY, OldX, OldY: Integer;
  begin
    NewX := New.X - ARequestedIconSize.X;
    NewY := New.Y - ARequestedIconSize.Y;
    OldX := Old.X - ARequestedIconSize.X;
    OldY := Old.Y - ARequestedIconSize.Y;
    Result := (Abs(NewX) <= Abs(OldX)) and ((NewX <= 0) or (NewX <= OldX)) and
       (Abs(NewY) <= Abs(OldY)) and ((NewY <= 0) or (NewY <= OldY));
  end;

  //-------------------------------------

begin
  Result := 0;
  if ARsrc = 0 then Exit;

  if (ARequestedIconSize.X or ARequestedIconSize.Y) = 0 then
  begin
    ARequestedIconSize.X := GetSystemMetrics(SM_CXICON);
    ARequestedIconSize.Y := GetSystemMetrics(SM_CYICON);
  end;

  SystemBPP := abfGetCurrentBPP;

  if (ARequestedIconBPP < 1) or (ARequestedIconBPP > SystemBPP) then
    ARequestedIconBPP := SystemBPP;

  TempGlobal := LoadResource(AInstance, ARsrc);
  if TempGlobal = 0 then Exit;

  TempIconDir := LockResource(TempGlobal);
  if not Assigned(TempIconDir) then Exit;

// Find the image that most closely matches (<=) the requested color depth
// (or the system screen depth) and the requested image size.

  BestIconBPP := 0;
  BestIconSize := Point(0, 0);
  BestIconIndex := -1;

  for i := 0 to PGRPICONDIR(TempIconDir)^.idCount - 1 do
  begin
    TempRsrc := FindResource(AInstance,
      MAKEINTRESOURCE(PGRPICONDIR(TempIconDir)^.idEntries[i].nID), RT_ICON);
    if TempRsrc = 0 then Continue;

    TempGlobal := LoadResource(AInstance, TempRsrc);
    if TempGlobal = 0 then Continue;

    TempBIH := LockResource(TempGlobal);
    if not Assigned(TempBIH) then Continue;

    TempIconBPP := TempBIH^.biBitCount;
    TempIconSize := Point(TempBIH^.biWidth, TempBIH^.biHeight div 2);

    if (TempIconBPP <= ARequestedIconBPP) and (TempIconBPP >= BestIconBPP) and
      BetterSize(BestIconSize, TempIconSize) then
    begin
      BestIconBPP := TempIconBPP;
      BestIconSize := TempIconSize;
      BestIconIndex := i;
    end;
  end;

  if BestIconIndex < 0 then Exit;

  TempRsrc := FindResource(AInstance,
    MAKEINTRESOURCE(PGRPICONDIR(TempIconDir)^.idEntries[BestIconIndex].nID),
    RT_ICON);
  if TempRsrc = 0 then Exit;

  TempGlobal := LoadResource(AInstance, TempRsrc);
  if TempGlobal = 0 then Exit;

  TempBIH := LockResource(TempGlobal);
  if not Assigned(TempBIH) then Exit;

// Let the OS make us an icon
  Result := CreateIconFromResourceEx(Pointer(TempBIH),
    SizeofResource(AInstance, TempRsrc), TRUE, $00030000,
    BestIconSize.X, BestIconSize.Y, 0);
end;

//------------------------------------------------------------------------------

function abfGetIconFromInstanceA(AInstance: HINST; AResName: PAnsiChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
begin
  Result := _GetIconFromInstance(AInstance,
    FindResource(AInstance, AResName, RT_GROUP_ICON), ARequestedIconSize,
    ARequestedIconBPP);
end;

//------------------------------------------------------------------------------

function abfGetIconFromInstance(AInstance: HINST; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
begin
  Result := abfGetIconFromInstanceA(AInstance, AResName, ARequestedIconSize,
    ARequestedIconBPP);
end;

//------------------------------------------------------------------------------

function abfGetIconFromInstanceW(AInstance: HINST; AResName: PWideChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
begin
  if not IsWinNT then
  begin
    if (DWORD(AResName) shr 16) = 0 then
      Result := abfGetIconFromInstanceA(AInstance, PAnsiChar(AResName),
        ARequestedIconSize, ARequestedIconBPP)
    else
      Result := abfGetIconFromInstanceA(AInstance,
        PAnsiChar(string(WideString(AResName))), ARequestedIconSize,
        ARequestedIconBPP);

    Exit;
  end;

  Result := _GetIconFromInstance(AInstance,
    FindResourceW(AInstance, AResName, PWideChar(RT_GROUP_ICON)), ARequestedIconSize,
    ARequestedIconBPP);
end;


//------------------------------------------------------------------------------
// Returns handle of icon from library that most closely matches (<=)
// the requested color depth (or the current screen depth)
// and the requested icon size.

function abfGetIconFromLibraryA(const ALibFileName: AnsiString;
  AResName: PAnsiChar; ARequestedIconSize: TPoint;
  ARequestedIconBPP: Integer): HICON;
var
  TempInstance: HINST;
begin
  Result := 0;

  TempInstance := LoadLibraryExA(PAnsiChar(ALibFileName), 0,
    LOAD_LIBRARY_AS_DATAFILE);
  if TempInstance = 0 then Exit;

  try
    Result := abfGetIconFromInstanceA(TempInstance, AResName,
      ARequestedIconSize, ARequestedIconBPP);
  finally
    FreeLibrary(TempInstance);
  end;
end;

//------------------------------------------------------------------------------

function abfGetIconFromLibrary(const ALibFileName: string; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer): HICON;
begin
  Result := abfGetIconFromLibraryA(ALibFileName, AResName,
    ARequestedIconSize, ARequestedIconBPP);
end;

//------------------------------------------------------------------------------

function abfGetIconFromLibraryW(const ALibFileName: WideString;
  AResName: PWideChar; ARequestedIconSize: TPoint;
  ARequestedIconBPP: Integer): HICON;
var
  TempInstance: HINST;
begin
  Result := 0;

  if not IsWinNT then
    TempInstance := LoadLibraryExA(PAnsiChar(AnsiString(ALibFileName)), 0,
      LOAD_LIBRARY_AS_DATAFILE)
  else
    TempInstance := LoadLibraryExW(PWideChar(ALibFileName), 0,
      LOAD_LIBRARY_AS_DATAFILE);

  if TempInstance = 0 then Exit;

  try
    Result := abfGetIconFromInstanceW(TempInstance, AResName,
      ARequestedIconSize, ARequestedIconBPP);
  finally
    FreeLibrary(TempInstance);
  end;
end;


//------------------------------------------------------------------------------
// Loads icon that most closely matches (<=) the requested color
// depth (or the current screen depth) and the requested icon size.

function abfLoadIconFromInstanceA(AInstance: HINST; AResName: PAnsiChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
var
  TempIconHandle: HICON;
begin
  Result := False;
  if not Assigned(AOutputIcon) then Exit;

  TempIconHandle := abfGetIconFromInstanceA(AInstance, AResName,
    ARequestedIconSize, ARequestedIconBPP);

  if TempIconHandle > 0 then
  begin
    AOutputIcon.Handle := TempIconHandle;
    Result := not AOutputIcon.Empty;
  end;
end;

//------------------------------------------------------------------------------

function abfLoadIconFromInstance(AInstance: HINST; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
begin
  Result := abfLoadIconFromInstanceA(AInstance, AResName, ARequestedIconSize,
    ARequestedIconBPP, AOutputIcon);
end;

//------------------------------------------------------------------------------

function abfLoadIconFromInstanceW(AInstance: HINST; AResName: PWideChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
var
  TempIconHandle: HICON;
begin
  Result := False;
  if not Assigned(AOutputIcon) then Exit;

  TempIconHandle := abfGetIconFromInstanceW(AInstance, AResName,
    ARequestedIconSize, ARequestedIconBPP);

  if TempIconHandle > 0 then
  begin
    AOutputIcon.Handle := TempIconHandle;
    Result := not AOutputIcon.Empty;
  end;
end;

//------------------------------------------------------------------------------
// Loads icon from library that most closely matches (<=)
// the requested color depth (or the current screen depth)
// and the requested icon size.

function abfLoadIconFromLibraryA(const ALibFileName: AnsiString;
  AResName: PAnsiChar; ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
var
  TempInstance: HINST;
begin
  Result := False;

  TempInstance := LoadLibraryExA(PAnsiChar(ALibFileName), 0,
    LOAD_LIBRARY_AS_DATAFILE);
  if TempInstance = 0 then Exit;

  try
    Result := abfLoadIconFromInstanceA(TempInstance, AResName,
      ARequestedIconSize, ARequestedIconBPP, AOutputIcon);
  finally
    FreeLibrary(TempInstance);
  end;
end;

//------------------------------------------------------------------------------

function abfLoadIconFromLibrary(const ALibFileName: string; AResName: PChar;
  ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
begin
  Result := abfLoadIconFromLibraryA(ALibFileName, AResName,
    ARequestedIconSize, ARequestedIconBPP, AOutputIcon);
end;

//------------------------------------------------------------------------------

function abfLoadIconFromLibraryW(const ALibFileName: WideString;
  AResName: PWideChar; ARequestedIconSize: TPoint; ARequestedIconBPP: Integer;
  AOutputIcon: TIcon): Boolean;
var
  TempInstance: HINST;
begin
  Result := False;

  if not IsWinNT then
    TempInstance := LoadLibraryExA(PAnsiChar(AnsiString(ALibFileName)), 0,
      LOAD_LIBRARY_AS_DATAFILE)
  else
    TempInstance := LoadLibraryExW(PWideChar(ALibFileName), 0,
      LOAD_LIBRARY_AS_DATAFILE);

  if TempInstance = 0 then Exit;

  try
    Result := abfLoadIconFromInstanceW(TempInstance, AResName,
      ARequestedIconSize, ARequestedIconBPP, AOutputIcon);
  finally
    FreeLibrary(TempInstance);
  end;
end;


//==============================================================================
// Dialog functions
//==============================================================================

function abfDialogUnitsToPixelsX(DialogUnits: Word): Word;
begin
  Result := (DialogUnits * LoWord(GetDialogBaseUnits)) div 4;
end;

//------------------------------------------------------------------------------

function abfDialogUnitsToPixelsY(DialogUnits: Word): Word;
begin
  Result := (DialogUnits * HiWord(GetDialogBaseUnits)) div 8;
end;

//------------------------------------------------------------------------------

function abfPixelsToDialogUnitsX(PixelUnits: Word): Word;
begin
  Result := PixelUnits * 4 div LoWord(GetDialogBaseUnits);
end;

//------------------------------------------------------------------------------

function abfPixelsToDialogUnitsY(PixelUnits: Word): Word;
begin
  Result := PixelUnits * 8 div HiWord(GetDialogBaseUnits);
end;


//==============================================================================
// Simple graphic routines
//==============================================================================

//------------------------------------------------------------------------------
// Fills rect on DC with specified color

procedure abfFillRect(ADC: HDC; const ARect: TRect; AColor: TColor);
var
  Brush: HBrush;
begin
  Brush := CreateSolidBrush(ColorToRGB(AColor));
  try
    FillRect(ADC, ARect, Brush);
  finally
    DeleteObject(Brush);
  end;
end;

//------------------------------------------------------------------------------
// Draws a frame rect on DC with specified color

procedure abfFrameRect(ADC: HDC; const ARect: TRect; AColor: TColor);
var
  Brush: HBrush;
begin
  Brush := CreateSolidBrush(ColorToRGB(AColor));
  try
    FrameRect(ADC, ARect, Brush);
  finally
    DeleteObject(Brush);
  end;
end;


//==============================================================================
// VCL graphic routines
//==============================================================================

(*
type
  TParentControl = class(TWinControl);

//------------------------------------------------------------------------------

procedure abfCopyParentImage(AControl: TControl; ACanvas: TCanvas);
var
  i, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
  CurrentControl: TControl;
begin
  if (AControl = nil) or (AControl.Parent = nil) then Exit;
  Count := AControl.Parent.ControlCount;
  DC := ACanvas.Handle;
  with AControl.Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    with AControl do
    begin
      SelfR := Bounds(Left, Top, Width, Height);
      X := -Left;
      Y := -Top;
    end;
  // Copy parent control image
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, AControl.Parent.ClientWidth,
        AControl.Parent.ClientHeight);
      with TParentControl(AControl.Parent) do
      begin
        Perform(WM_ERASEBKGND, DC, 0);
        PaintWindow(DC);
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;
  // Copy images of graphic controls
    for i := 0 to Count - 1 do
    begin
      CurrentControl := AControl.Parent.Controls[i];
      if CurrentControl = AControl then Break else
      if (CurrentControl <> nil) and (CurrentControl is TGraphicControl) then
        with TGraphicControl(CurrentControl) do
        begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SaveIndex := SaveDC(DC);
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
              IntersectClipRect(DC, 0, 0, Width, Height);
              Perform(WM_PAINT, DC, 0);
            finally
              RestoreDC(DC, SaveIndex);
              ControlState := ControlState - [csPaintCopy];
            end;
          end;{if Bool(...}
        end;{with TGraphicControl(CurrentControl) do}
    end;{for i := 0 to Count - 1 do}
  finally
    with AControl.Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;{procedure abfCopyParentImage}
(**)

//------------------------------------------------------------------------------
// Draws a border on the ACanvas inside the ARect area with AColor and of
// ASize wide.

procedure abfDrawWideBorder(const Canvas: TCanvas; const ARect: TRect;
  AColor: TColor; ASize: Integer);
var
  Width, Height: Integer;
begin
  with Canvas, ARect do
  begin
    Brush.Color := AColor;
    Width  := Right - Left;
    Height := Bottom - Top;
  // Top line
    FillRect(Rect(Left, Top, Width, Top + ASize));
  // Bottom line
    FillRect(Rect(Left, Height - ASize, Width, Height));
  // Left line
    FillRect(Rect(Left, Top, Left + ASize, Height));
  // Right line
    FillRect(Rect(Width - ASize, Top, Width, Height));
  end;
end;

//------------------------------------------------------------------------------
// Draws a simple gradient in an ARect area of the Canvas. If Horizontal is
// True draws horizontal gradien otherwise draws vertical one.
// Date: 03/19/2001

procedure abfDrawGradient(const Canvas: TCanvas; const ARect: TRect;
  ColorBegin, ColorEnd: TColor; Horizontal: Boolean);
var
  i, Width, Height, MaxLen, Step, Divisor: Integer;
  RGB1, RGB2, R1, G1, B1, R2, G2, B2, StepR, StepG, StepB: Integer;
  R: TRect;
Begin
  Width  := ARect.Right - ARect.Left;
  Height := ARect.Bottom - ARect.Top;
// Is area empty?
  if (Width <= 0) or (Height <= 0) then Exit;

  RGB1 := ColorToRGB(ColorBegin);
  RGB2 := ColorToRGB(ColorEnd);
  R1 := GetRValue(RGB1);
  G1 := GetGValue(RGB1);
  B1 := GetBValue(RGB1);
  R2 := GetRValue(RGB2) - R1;
  G2 := GetGValue(RGB2) - G1;
  B2 := GetBValue(RGB2) - B1;

  MaxLen := Max(Max(Abs(R2), Abs(G2)), Abs(B2)) + 1;

  if Horizontal then
  begin
    Divisor := Width;
    Step := Width div MaxLen + 1;
    R := Rect(ARect.Left, ARect.Top, Step, Height);
  end else
  begin
    Divisor := Height;
    Step := Height div MaxLen + 1;
    R := Rect(ARect.Left, ARect.Top, Width, Step)
  end;

  StepR := Step * R2;
  StepG := Step * G2;
  StepB := Step * B2;

  Canvas.Brush.Style := bsSolid;
  if Horizontal then
    for i := 0 to MaxLen - 1 do
    begin
      Canvas.Brush.Color := RGB(R1  + (i * StepR) div Divisor,
        G1  + (i * StepG) div Divisor,
        B1  + (i * StepB) div Divisor);
      Canvas.FillRect(R);
      Inc(R.Left, Step);
      if ARect.Left > Width then Break;
      Inc(R.Right, Step);
    end{for i := 0 to MaxLen - 1 do}
  else
    for i := 0 to MaxLen - 1 do
    begin
      Canvas.Brush.Color := RGB(R1  + (i * StepR) div Divisor,
        G1  + (i * StepG) div Divisor,
        B1  + (i * StepB) div Divisor);
      Canvas.FillRect(R);
      Inc(R.Top, Step);
      if ARect.Top > Height then Break;
      Inc(R.Bottom, Step);
    end;{for i := 0 to MaxLen - 1 do}
end;{procedure abfDrawGradient}


//==============================================================================
// Draw text routines
//==============================================================================

procedure abfDrawText(const Canvas: TCanvas; const Text: string;
  var R: TRect; TextColor: TColor; Enabled: Boolean;
  DrawTextFlags: Integer);
var
  RText: TRect;
  TextLen: Integer;
begin
  RText := R;
  TextLen := Length(Text);
  with Canvas do
  begin
    if not Enabled then
    begin
    // Draw default shadow
      OffsetRect(RText, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Text), TextLen, RText, DrawTextFlags);
      OffsetRect(RText, -1, -1);
      TextColor := clBtnShadow;
    end;
  // Draw text
    Font.Color := TextColor;
    DrawText(Handle, PChar(Text), TextLen, RText, DrawTextFlags);
  end;{with Canvas do}
  R := RText;
end;{procedure abfDrawText}

//------------------------------------------------------------------------------

procedure abfDrawTextShadow(const Canvas: TCanvas; const Text: string;
  var R: TRect; TextColor, ShadowColor: TColor; SizeX, SizeY: Integer;
  DrawTextFlags: Integer);
var
  TextLen: Integer;
begin
  TextLen := Length(Text);
  with Canvas do
  begin
  // Draw shadow
    if (SizeX <> 0) or (SizeY <> 0) then
    begin
      OffsetRect(R, SizeX, SizeY);
      Font.Color := ShadowColor;
      DrawText(Handle, PChar(Text), TextLen, R, DrawTextFlags);
    end;
  end;
end;{procedure abfDrawTextWithShadow}

//------------------------------------------------------------------------------

procedure abfDrawTextBorder(const Canvas: TCanvas; const Text: string;
  const R: TRect; TextColor, BorderColor: TColor; SizeX, SizeY: Integer;
  DrawTextFlags: Integer);
var
  RText: TRect;
  i, TextLen, Delta: Integer;
  DC: HDC;
begin
  RText := R;
  TextLen := Length(Text);
  with Canvas do
  begin
  // Draw border
    Font.Color := BorderColor;
    DC := Handle;
  // X part
    if SizeX <> 0 then
      for i := -SizeX to SizeX do
      begin
        if i = 0 then Continue;
      // Draw top side of border
        RText := R;
        OffsetRect(RText, i, -SizeY);
        DrawText(DC, PChar(Text), TextLen, RText, DrawTextFlags);
      // Draw bottom side of border
        OffsetRect(RText, 0, 2 * SizeY);
        DrawText(DC, PChar(Text), TextLen, RText, DrawTextFlags);
      end;
  // Y part
    if SizeX = 0 then Delta := 0 else Delta := 1;
    if SizeY <> 0 then
      for i := (-SizeY + Delta) to (SizeY - Delta) do
      begin
        if i = 0 then Continue;
      // Draw left side of border
        RText := R;
        OffsetRect(RText, -SizeX, i);
        DrawText(DC, PChar(Text), TextLen, RText, DrawTextFlags);
      // Draw right side of border
        OffsetRect(RText, 2 * SizeX, 0);
        DrawText(DC, PChar(Text), TextLen, RText, DrawTextFlags);
      end;
  end;{with Canvas do}
end;{procedure abfDrawTextWithBorder}


//==============================================================================
// Additional standard procedures
//==============================================================================


//------------------------------------------------------------------------------
// Layered window support

type
  TSetLayeredWindowAttributes = function(Handle: THandle; Key: COLORREF;
    Alpha: Byte; Flags: DWORD): Boolean; stdcall;

var
  _SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;

function abfSetLayeredWindowAttributes(Handle: THandle; Key: COLORREF;
  Alpha: Byte; Flags: DWORD): Boolean;
begin
  Result := False;
  if not Assigned(_SetLayeredWindowAttributes) then Exit;
  Result := _SetLayeredWindowAttributes(Handle, Key, Alpha, Flags);
end;


//------------------------------------------------------------------------------
// Initializes undocumented or missed procedures

procedure _InitAdditionalProcs;
const
  sUser32 = 'user32.dll';
var
  ModH: HMODULE;
begin
  ModH := GetModuleHandle(sUser32);
  if ModH <> 0 then
     @_SetLayeredWindowAttributes := GetProcAddress(ModH,
       'SetLayeredWindowAttributes');
end;


{******************************************************************************}
initialization
{******************************************************************************}

  _InitAdditionalProcs;

{$IfDef abfColors}
  RegisterIntegerConsts(TypeInfo(TColor), abfIdentToColor, abfColorToIdent);
{$EndIf abfColors}


{******************************************************************************}
finalization
{******************************************************************************}

{$IfDef abfColors}
  {$IfDef D6}
  UnRegisterIntegerConsts(TypeInfo(TColor), abfIdentToColor, abfColorToIdent);
  {$EndIf D6}
{$EndIf abfColors}


end{unit abfGraphics}.
