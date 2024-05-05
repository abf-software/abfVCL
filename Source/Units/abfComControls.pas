{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComControls;

{$I abf.inc}

interface

uses
{$IfDef D7}
  Themes,
{$EndIf D7}
  Windows, Messages, Classes, Controls,  SysUtils, Graphics, Forms, StdCtrls,
  ExtCtrls, ComCtrls, CommCtrl,
// ABF VCL  
  abfPropertyDesc;

type

//==============================================================================
// TabfCustomRichEdit
//==============================================================================
// Prototype of enhanced RichEdit

  TabfCustomRichEdit = class(TCustomRichEdit)
  private
    FAbout: string;
    FMargins: TabfMarginsDesc;
  // Properties Get/Set
    procedure SetMargins(const Value: TabfMarginsDesc);
  protected
    property About: string read FAbout write FAbout stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print(const Caption: string);{$IfDef D4}override;{$EndIf D4}
    procedure ReplaceSelectedText(const NewText: string); virtual;
    function ReplaceText(const OldText, NewText : string;
      StartPos, SearchLength: Integer; Options: TSearchTypes;
      AllOccured: Boolean): Integer; virtual;
    procedure LoadFromFile(const FileName: string;
      APlainText: Boolean); virtual;
    procedure SaveToFile(const FileName: string; APlainText: Boolean); virtual;
  // Properties
    property Margins: TabfMarginsDesc read FMargins write SetMargins;
  end;


//==============================================================================
// TabfRichEdit
//==============================================================================
// Enhanced RichEdit

  TabfRichEdit = class(TabfCustomRichEdit)
  published
    property About;
    property Align;
    property Alignment;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property Lines;
    property MaxLength;
    property Margins;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
{$IfDef D3}
    property ImeMode;
    property ImeName;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property DragKind;
    property Constraints;
    property ParentBiDiMode;
{$EndIf D4}
    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDrag;
{$IfDef D4}
    property OnEndDock;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnContextPopup;
{$EndIf D5}
  end;


//==============================================================================
// TabfCustomListView
//==============================================================================
// Prototype of listview with sort feature and triangle sort mark.

  TabfListViewSortDirection = (sdAscending, sdDescending);
  TabfListViewCompareItems = function(Str1, Str2: string): Integer;

  TabfCustomListView = class(TCustomListView)
  private
    FAbout: string;
    FSortColumn: Word;
    FSortDirection: TabfListViewSortDirection;
    FHeadLBDown, FHeadOnDiv: Boolean;
    FHeadLBCol: Integer;
    FShowSortMark: Boolean;
  // Properties Get/Set
    procedure SetSortColumn(Value: Word);
    procedure SetSortDirection(Value: TabfListViewSortDirection);
    procedure SetShowSortMark(Value: Boolean);
    function  GetSortType: TSortType;
    procedure SetSortType(Value: TSortType);
  // Messages
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
  protected
    FHeaderHandle: THandle;
    FNewWndProc, FPrevWndProc, FNewHeaderProc, FPrevHeaderProc: Pointer;
    procedure CreateWnd; override;
    procedure ColClick(Column: TListColumn); override;
    procedure DrawSortMark; virtual;
  // Hook routines
    procedure HookControl; virtual;
    procedure UnhookControl; virtual;
    procedure HookWndProc(var AMsg: TMessage); virtual;
    procedure HookHeader(Wnd: THandle); virtual;
    procedure UnhookHeader; virtual;
    procedure HeaderWndProc(var AMsg: TMessage); virtual;
  // Properties
    property About: string read FAbout write FAbout stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CustomSort(SortProc: TLVCompare; Data: LongInt): Boolean; virtual;
    procedure RepaintHeader; virtual;
    procedure DoAutoSizeEx(AVertScroll: Boolean); virtual;
    procedure DoAutoSize; virtual;
  // Properties
    property ViewStyle default vsReport;
    property SortColumn: Word read FSortColumn write SetSortColumn;
    property SortDirection: TabfListViewSortDirection read FSortDirection
      write SetSortDirection default sdAscending;
    property ShowSortMark: Boolean read FShowSortMark
      write SetShowSortMark default True;
    property SortType: TSortType read GetSortType write SetSortType;
  end;{TabfCustomListView = class(TCustomListView)}


//==============================================================================
// TabfListView
//==============================================================================
// TabfListView is an enhanced ListView with sorting feature and triangle sort
// mark (a-la Outlook) in the header for report style. A ComCtrl color bug
// (when the color scheme of the system is changed) is fixed. Custom sorting is
// supported.

  TabfListView = class(TabfCustomListView)
  published
  // Properties
  // From TCustomListView
    property Align;
    property AllocBy;
    property BorderStyle;
    property Color;
    property Columns;
    property ColumnClick;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property Font;
    property HideSelection;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property ReadOnly default False;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowHint;
    property SmallImages;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
  // From TabfCustomListView
    property About;
    property SortDirection;
    property ShowSortMark;
    property SortType;
{$IfDef D3}
    property Checkboxes;
    property DragMode;
    property GridLines;
    property HotTrack;
    property RowSelect;
{$EndIf D3}
{$IfDef D4}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property DragKind;
    property FlatScrollBars;
    property FullDrag;
    property HotTrackStyles;
    property OwnerData;
    property OwnerDraw;
    property ParentBiDiMode;
{$EndIf D4}
{$IfDef D5}
    property HoverTime;
    property ShowWorkAreas;
{$EndIf D5}
    property SortColumn; // Should be stored last
  // Events
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$IfDef D4}
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDrawItem;
    property OnEndDock;
    property OnGetImageIndex;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
{$EndIf D4}
{$IfDef D5}
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnContextPopup;
    property OnGetSubItemImage;
    property OnInfoTip;
{$EndIf D5}
  end;{TabfListView = class(TCustomListView)}


//==============================================================================
// TabfProgressBar
//==============================================================================
// TabfProgressBar is an enhanced progress bar. Allows specify background and
// bar colors. Can be horizontal or vertical.

{$IfNDef D4}
  TProgressBarOrientation = (pbHorizontal, pbVertical);
{$EndIf D4}

  TabfProgressBar = class(TProgressBar)
  private
    FAbout: string;
    FColor: TColor;
    FBarColor: TColor;
    FPosition: Integer;
{$IfNDef D4}
    FOrientation: TProgressBarOrientation;
    FSmooth: Boolean;
{$EndIf D4}
  // Properties Get/Set
    procedure SetBarColor(Value: TColor);
    procedure SetColor(Value: TColor);
    function  GetOrientation: TProgressBarOrientation;
    procedure SetOrientation(Value: TProgressBarOrientation);
    function  GetPosition: Integer;
    procedure SetPosition(Value: Integer);
{$IfNDef D4}
    procedure SetSmooth(Value: Boolean);
{$EndIf D4}
  // Messages routines
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
{$IfNDef D4}
    procedure CreateParams(var Params: TCreateParams); override;
{$EndIf D4}
  public
    constructor Create(AOwner: TComponent); override;
  published
  // Properties
    property About: string read FAbout write FAbout stored False;
    property BarColor: TColor read FBarColor write SetBarColor
      default clHighlight;
    property Color: TColor read FColor  write SetColor default clBtnFace;
    property Orientation: TProgressBarOrientation read GetOrientation
      write SetOrientation default pbHorizontal;
    property Position: Integer read GetPosition write SetPosition default 0;
{$IfNDef D4}
    property Smooth: Boolean read FSmooth write SetSmooth default False;
{$EndIf D4}
  // Events
    property OnClick;
    property OnDblClick;
  end;{TabfProgressBar = class(TProgressBar)}


//==============================================================================
// TabfTrackBar
//==============================================================================
// Enhanced track bar. Allows hide a SelRange

  TabfTrackBar = class(TTrackBar)
  private
    FAbout: string;
    FSelRange: Boolean;
{$IfNDef D4}
    FSliderVisible: Boolean;
    procedure SetSliderVisible(Value: Boolean);
{$EndIf D4}
    procedure SetSelRange(Value: Boolean);
  // Messages routines
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
  // Properties
    property About: string read FAbout write FAbout stored False;
    property SelRange: Boolean read FSelRange
      write SetSelRange default False;
{$IfNDef D4}
    property SliderVisible: Boolean read FSliderVisible write
      SetSliderVisible default True;
{$EndIf D4}
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
  end;


//==============================================================================
// Global function pointers
//==============================================================================
// Use this variables to assign own procedures for sorting and comparing.

var
  abfListViewSortProc: TLVCompare;
  abfListViewCompareItemsProc: TabfListViewCompareItems;


//==============================================================================
// For old versions of Delphi and C++ Builder
//==============================================================================

{$IfNDef D3}
const
  PBM_SETRANGE            = WM_USER + 1;
  PBM_SETPOS              = WM_USER + 2;
  PBM_DELTAPOS            = WM_USER + 3;
  PBM_SETSTEP             = WM_USER + 4;
  PBM_STEPIT              = WM_USER + 5;
  PBM_SETRANGE32          = WM_USER + 6;  // lParam = high, wParam = low
  PBM_GETRANGE            = WM_USER + 7;  // lParam = PPBRange or Nil
                                				  // wParam = False: Result = high
                              					  // wParam = True: Result = low
  PBM_GETPOS              = WM_USER + 8;

type
  PPBRange = ^TPBRange;
  TPBRange = record
    iLow:  integer;
    iHigh: integer;
  end;

{$EndIf D3}
{$IfNDef C3}

const
  HDM_GETITEMRECT         = HDM_FIRST + 7;
  HDM_GETORDERARRAY       = HDM_FIRST + 17;
  HDM_SETORDERARRAY       = HDM_FIRST + 18;

  CCM_FIRST               = $2000;      { Common control shared messages }
  CCM_SETBKCOLOR          = CCM_FIRST + 1; // lParam is bkColor

  PBS_SMOOTH              = $01;
  PBS_VERTICAL            = $04;

  PBM_SETBARCOLOR         = WM_USER + 9;     // lParam = bar color
  PBM_SETBKCOLOR          = CCM_SETBKCOLOR;  // lParam = bkColor
  LVM_GETHEADER           = LVM_FIRST + 31;


function Header_GetItemRect(hwnd: HWND; iItem: Integer; lprc: PRect): Integer;
function Header_GetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;
function Header_SetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;
function ListView_GetHeader(hwnd: HWND): HWND;

{$EndIf C3}

{******************************************************************************}
implementation
{******************************************************************************}

uses
  Printers, {$IfDef D6} Types, {$EndIf D6}
  abfGraphics, abfSysUtils;

{$I abf_init.inc}

//==============================================================================
// For old versions of Delphi and C++ Builder
//==============================================================================
{$IfNDef C3}

//------------------------------------------------------------------------------

function Header_GetItemRect(hwnd: HWND; iItem: Integer; lprc: PRect): Integer;
begin
  Result := SendMessage(hwnd, HDM_GETITEMRECT, iItem, LPARAM(lprc));
end;

//------------------------------------------------------------------------------

function Header_GetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;
begin
  Result := SendMessage(hwnd, HDM_GETORDERARRAY, iCount, LPARAM(lpi));
end;

//------------------------------------------------------------------------------

function Header_SetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;
begin
  Result := SendMessage(hwnd, HDM_SETORDERARRAY, iCount, LPARAM(lpi));
end;

//------------------------------------------------------------------------------

function ListView_GetHeader(hwnd: HWND): HWND;
begin
  Result := SendMessage(hwnd, LVM_GETHEADER, 0, 0);
end;

{$EndIf C3}

//==============================================================================
// TabfCustomRichEdit
//==============================================================================
// Prototype of enhanced RichEdit
{ TabfCustomRichEdit }

constructor TabfCustomRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FMargins := TabfMarginsDesc.Create(Self, nil);
end;

//------------------------------------------------------------------------------

destructor TabfCustomRichEdit.Destroy;
begin
  FMargins.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomRichEdit.Print(const Caption: string);
var
  dPageWidth, dPageHeight, dpiX, dpiY: Integer;
  dLeftMargin, dTopMargin, dRightMargin, dBottomMargin: Double; // Device values
  cLeftMargin, cTopMargin, cRightMargin, cBottomMargin: Double;
begin
  with Printer, Margins do
  begin
  // Get system/device values
    dPageWidth  := GetDeviceCaps(Handle, PHYSICALWIDTH);
    dPageHeight := GetDeviceCaps(Handle, PHYSICALHEIGHT);
    dLeftMargin := GetDeviceCaps(Handle, PHYSICALOFFSETX);
    dTopMargin  := GetDeviceCaps(Handle, PHYSICALOFFSETY);
    dRightMargin  := dPageWidth  - PageWidth  - dLeftMargin;
    dBottomMargin := dPageHeight - PageHeight - dTopMargin;
    dpiX := GetDeviceCaps(Handle, LOGPIXELSX);
    dpiY := GetDeviceCaps(Handle, LOGPIXELSY);

  // Calc margins depending on units type
    case Units of
      muInches:
        begin
          cLeftMargin   := Left   * dpiX;
          cTopMargin    := Top    * dpiY;
          cRightMargin  := Right  * dpiX;
          cBottomMargin := Bottom * dpiY;
        end;
      muMillimeters:
        begin
          cLeftMargin   := Left   * dpiX / 25.4;
          cTopMargin    := Top    * dpiY / 25.4;
          cRightMargin  := Right  * dpiX / 25.4;
          cBottomMargin := Bottom * dpiY / 25.4;
        end;
      muCentimeters:
        begin
          cLeftMargin   := Left   * dpiX / 2.54;
          cTopMargin    := Top    * dpiY / 2.54;
          cRightMargin  := Right  * dpiX / 2.54;
          cBottomMargin := Bottom * dpiY / 2.54;
        end;
      else
        begin
          cLeftMargin   := Left;
          cTopMargin    := Top;
          cRightMargin  := Right;
          cBottomMargin := Bottom;
        end;
    end;

  // Apply device margins
    if cLeftMargin < dLeftMargin then
      cLeftMargin := 0
    else
      cLeftMargin := cLeftMargin - dLeftMargin;
    if cTopMargin < dTopMargin then
      cTopMargin := 0
    else
      cTopMargin := cTopMargin - dTopMargin;
    if cRightMargin < dRightMargin then
      cRightMargin := 0
    else
      cRightMargin := cRightMargin - dRightMargin;
    if cBottomMargin < dBottomMargin then
      cBottomMargin := 0
    else
      cBottomMargin := cBottomMargin - dBottomMargin;

  // Set new page rectangle
    PageRect := Bounds(Round(cLeftMargin), Round(cTopMargin),
      Round(PageWidth - cLeftMargin - cRightMargin),
      Round(PageHeight - cTopMargin - cBottomMargin));
  end;
  inherited Print(Caption);
end;

//------------------------------------------------------------------------------

procedure TabfCustomRichEdit.ReplaceSelectedText(const NewText: string);
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  SendMessage(Handle, EM_REPLACESEL, Integer(True), Integer(PChar(NewText)));
end;

//------------------------------------------------------------------------------

function TabfCustomRichEdit.ReplaceText(const OldText, NewText: string;
  StartPos, SearchLength: Integer; Options: TSearchTypes;
  AllOccured: Boolean): Integer;
var
  i, OldLen, NewLen: Integer;
begin
  Result := 0;
  OldLen := Length(OldText);
  NewLen := Length(NewText);
  repeat
    i := FindText(OldText, StartPos, SearchLength, Options);
    if i < 0 then Exit;
    SelStart  := i;
    SelLength := OldLen;
    ReplaceSelectedText(NewText);
  // Fix SearchLength
    SearchLength := SearchLength - OldLen + NewLen;
    Inc(Result);
  until (not AllOccured);
end;

//------------------------------------------------------------------------------

procedure TabfCustomRichEdit.LoadFromFile(const FileName: string;
  APlainText: Boolean);
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  PlainText := APlainText;
  Lines.LoadFromFile(FileName);
end;

//------------------------------------------------------------------------------

procedure TabfCustomRichEdit.SaveToFile(const FileName: string;
  APlainText: Boolean);
begin
  PlainText := APlainText;
  Lines.SaveToFile(FileName);
end;

//==============================================================================
// Properties Get/Set

procedure TabfCustomRichEdit.SetMargins(const Value: TabfMarginsDesc);
begin
  FMargins.Assign(Value);
end;


//==============================================================================
// TabfCustomListView
//==============================================================================
// Prototype of listview with sort feature and triangle sort marks
{ TabfCustomListView }

function _DefaultListViewCompareItems(Str1, Str2: string): Integer;

  //-------------------------------------

  function _IsValidNumber(S: string; var V: Extended): Boolean;
  var
    ErrCode: Integer;
  begin
// TODO -oKARPOLAN: Make fast separator removement
//    abfRemoveCharSet([ThousandSeparator, ' '], S);
    Val(S, V, ErrCode);
    Result := (ErrCode = 0);
  end;

  //-------------------------------------

  function _IsValidDate(const S: string; var DT: TDateTime): Boolean;
  begin
    Result := False;
    Exit;

// TODO -oKARPOLAN: Make fast validation without exceptions !!
    try
      DT := StrToDate(S);
      Result := True;
    except
    end;
  end;

  //-------------------------------------

var
  Val1, Val2: Extended;
  DT1, DT2: TDateTime;
begin
  if (Str1 <> '') and (Str2 = '') then Result := -1 else
  if (Str2 <> '') and (Str1 = '') then Result := 1 else
  if _IsValidNumber(Str1, Val1) and _IsValidNumber(Str2, Val2) then
    if Val1 < Val2 then Result := -1 else
    if Val1 > Val2 then Result := 1 else Result := 0
  else
  if _IsValidDate(Str1, DT1) and _IsValidDate(Str2, DT2) then
    if DT1 < DT2 then Result := -1 else
    if DT1 > DT2 then Result := 1 else Result := 0
  else
    Result := AnsiCompareStr(Str1, Str2);
end;{function _DefaultListViewCompareItems}

//------------------------------------------------------------------------------
// Default listview sort function

function _DefaultListViewSort(Item1, Item2: TListItem;
  lParam: Integer): Integer; stdcall;
var
  Str1, Str2: string;
  Column: Integer;
begin
  with Item1 do
    if Assigned(TabfCustomListView(ListView).OnCompare) then
      TabfCustomListView(ListView).OnCompare(ListView, Item1, Item2,
        lParam, Result)
    else
    begin
      Column := LoWord(lParam);
      if Column = 0 then
      begin
        Str1 := Item1.Caption;
        Str2 := Item2.Caption;
      end else
      begin
        if Item1.SubItems.Count > Column - 1 then
          Str1 := Item1.SubItems[Column - 1]
        else
          Str1 := '';
        if Item2.SubItems.Count > Column - 1 then
          Str2 := Item2.SubItems[Column - 1]
        else
          Str2 := '';
      end;
      Result := abfListViewCompareItemsProc(Str1,Str2) * ShortInt(HiWord(lParam));
    end;
end;

//------------------------------------------------------------------------------

constructor TabfCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FNewWndProc     := MakeObjectInstance(HookWndProc);
  FNewHeaderProc  := MakeObjectInstance(HeaderWndProc);
  FPrevWndProc    := nil;
  FPrevHeaderProc := nil;
  FSortColumn     := 0;
  FSortDirection  := sdAscending;
  FHeaderHandle   := 0;
  FHeadLBDown     := False;
  FShowSortMark   := True;
  ViewStyle := vsReport;
end;

//------------------------------------------------------------------------------

destructor TabfCustomListView.Destroy;
begin
  UnhookHeader;
  UnhookControl;

  inherited Destroy;

// Free after inherited due to the exception when delete a component
  FreeObjectInstance(FNewWndProc);
  FreeObjectInstance(FNewHeaderProc);
end;

//------------------------------------------------------------------------------
// Method for sorting by custom sort function

function TabfCustomListView.CustomSort(SortProc: TLVCompare;
  Data: LongInt): Boolean;
begin
  Result := False;
  if HandleAllocated then
  begin
    if Assigned(SortProc) then UnhookControl;
    Result := ListView_SortItems(Handle, SortProc, Data);
    if Assigned(SortProc) then HookControl;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.RepaintHeader;
begin
  if IsWindowVisible(FHeaderHandle) then
    InvalidateRect(FHeaderHandle, nil, True);
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.CreateWnd;
begin
  inherited CreateWnd;
  HookControl;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.ColClick(Column: TListColumn);
begin
  if SortColumn <> Column.Index then
    SortColumn := Column.Index
  else
  if FSortDirection = sdAscending then
    SortDirection := sdDescending
  else
    SortDirection := sdAscending;
  inherited ColClick(Column);
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.DrawSortMark;

  //------------------------------------------------------------------------------

  function _GetOrderIndex(Index: Integer): Integer;
  var
    i: Integer;
{$IfNDef D4}
    SectionOrder: array[0..1024] of Integer;
{$Else D4}
    SectionOrder: array of Integer;
{$EndIf D4}
  begin
    Result := 0;
{$IfNDef D4}
    Header_GetOrderArray(FHeaderHandle, Columns.Count, PInteger(@SectionOrder));
{$Else D4}
    SetLength(SectionOrder, Columns.Count);
    Header_GetOrderArray(FHeaderHandle, Columns.Count, PInteger(SectionOrder));
{$EndIf D4}
    for i := 0 to Columns.Count - 1 do
      if SectionOrder[i] = Index then
      begin
        Result := i;
        Exit;
      end;
  end;

  //------------------------------------------------------------------------------

const
  cMarkWidth  = 7;
  cMarkHeight = 7;
var
  Cnv: TCanvas;
  X, Y, MarkMargin: Integer;
  R: TRect;
begin
  if (ViewStyle <> vsReport) or (Columns.Count = 0) or
    (SortType = stNone) or (not ShowSortMark) then Exit;

  MarkMargin := 10;
{$IfDef D7}
// Enlarge margin if theme is enabled for Alignment=taLeftJustify
   if ThemeServices.ThemesEnabled then MarkMargin := 14;
{$EndIf D7}
  
  Cnv := TCanvas.Create;
  Cnv.Handle := GetDC(FHeaderHandle);
  try
    Cnv.Font := Font;
    Header_GetItemRect(FHeaderHandle, SortColumn, @R);

    with Columns[_GetOrderIndex(SortColumn)] do
      case Alignment of
        taLeftJustify:
          X := R.Left + Cnv.TextWidth(Caption) + MarkMargin;
        taRightJustify:
          X := R.Right - Cnv.TextWidth(Caption) - cMarkWidth - MarkMargin;
      else
        if Caption <> '' then Exit else
          X := R.Right - ((R.Right - R.Left) div 2) - (cMarkWidth div 2);
      end;

  // Check limits
    if (X < R.Left + 3) or (X + cMarkWidth > R.Right - 3) then Exit;

    Y := ((Cnv.TextHeight('Wj') - cMarkHeight) div 2) + 1;

    if FHeadLBDown and not(FHeadOnDiv) then
    begin
      if FHeadLBCol <> SortColumn then Exit;
      Inc(X);
      Inc(Y);
    end;

    with Cnv do
      if FSortDirection = sdAscending then
      begin
      // Highlight
        Pen.Color := clBtnHighlight;
        MoveTo(X, Y + cMarkHeight);
        LineTo(X + cMarkWidth, Y + cMarkHeight);
        LineTo(X + (cMarkWidth div 2) + 1, Y);
      // Shadow
        Pen.Color := clBtnShadow;
        MoveTo(X + (cMarkWidth div 2), Y + 1);
        LineTo(X, Y + cMarkHeight);
      end else
      begin
        Inc(Y);
      // Shadow
        Pen.Color := clBtnShadow;
        MoveTo(X + cMarkWidth, Y);
        LineTo(X, Y);
        LineTo(X + (cMarkWidth div 2), Y + cMarkHeight);
      // Highlight
        Pen.Color := clBtnHighlight;
        MoveTo(X + cMarkWidth, Y);
        LineTo(X + (cMarkWidth div 2) + 1, Y + cMarkHeight);
      end;
  finally
    ReleaseDC(FHeaderHandle, Cnv.Handle);
    Cnv.Free;
  end;
end;{procedure TabfCustomListView.DrawSortMark}

//------------------------------------------------------------------------------
// Resizes columns to fit all client area. Reserves area for vertical scrollbar
// if AVertScroll is True.

procedure TabfCustomListView.DoAutoSizeEx(AVertScroll: Boolean);
var
  I, Count, WorkWidth, TmpWidth, Remain: Integer;
  List: TList;
  Column: TListColumn;
begin
// Try to fit all sections within client width
  List := TList.Create;
  try
    WorkWidth := ClientWidth;

    if AVertScroll then
      if (WorkWidth > Width - 5) and (WorkWidth < Width) then
        WorkWidth := WorkWidth - GetSystemMetrics(SM_CXVSCROLL);

    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns[I];
{$IfNDef D4}
      if Column.Width = 0 then
{$Else D4}
      if Column.AutoSize then
{$EndIf D4}
        List.Add(Column)
      else
        Dec(WorkWidth, Column.Width);
    end;
    if List.Count > 0 then
    begin
      Columns.BeginUpdate;
      try
        repeat
          Count := List.Count;
          Remain := WorkWidth mod Count;
        // Try to redistribute sizes to those sections which can take it
          TmpWidth := WorkWidth div Count;
          for I := Count - 1 downto 0 do
          begin
            Column := TListColumn(List[I]);
            if I = 0 then
              Inc(TmpWidth, Remain);
            Column.Width := TmpWidth;
          end;

        // Verify new sizes don't conflict with min/max section widths and
        // adjust if necessary. 
          TmpWidth := WorkWidth div Count;
          for I := Count - 1 downto 0 do
          begin
            Column := TListColumn(List[I]);
            if I = 0 then
              Inc(TmpWidth, Remain);
            if Column.Width <> TmpWidth then
            begin
              List.Delete(I);
              Dec(WorkWidth, Column.Width);
            end;
          end;
        until (List.Count = 0) or (List.Count = Count);
      finally
        Columns.EndUpdate;
      end;
    end;
  finally
    List.Free;
  end;
end;

//------------------------------------------------------------------------------
// Resizes columns to fit all client area.

procedure TabfCustomListView.DoAutoSize;
begin
  DoAutoSizeEx(False);
end;


//==============================================================================
// Hook routines

procedure TabfCustomListView.HookControl;
var
  P: Pointer;
begin
  if (csDestroying in ComponentState) then Exit;

  HandleNeeded;
  P := Pointer(GetWindowLong(Handle, GWL_WNDPROC));
  if (P <> FNewWndProc) then
  begin
    FPrevWndProc := P;
    SetWindowLong(Handle, GWL_WNDPROC, LongInt(FNewWndProc));
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.UnhookControl;
begin
  if (csDestroying in ComponentState) then Exit;

  if Assigned(FPrevWndProc) and HandleAllocated and
    (Pointer(GetWindowLong(Handle, GWL_WNDPROC)) = FNewWndProc) then
    SetWindowLong(Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
  FPrevWndProc := nil;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.HookWndProc(var AMsg: TMessage);
begin
  with AMsg do
  begin
    if (Msg = LVM_SORTITEMS) and Assigned(abfListViewSortProc) then
    begin
      if ViewStyle = vsReport then WParamLo := SortColumn
      else WParamLo := 0;
      if FSortDirection = sdAscending then WParamHi := 1
      else WParamHi := Word(-1);
      LParam := Integer(@abfListViewSortProc);
    end;
    Result := CallWindowProc(FPrevWndProc, Handle, Msg, WParam, LParam);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.HookHeader(Wnd: THandle);
begin
  if (csDestroying in ComponentState) then Exit;

  FHeaderHandle := Wnd;
  FPrevHeaderProc := Pointer(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
  SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FNewHeaderProc));
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.UnhookHeader;
begin
  if (csDestroying in ComponentState) then Exit;

  if Assigned(FPrevHeaderProc) and (Pointer(GetWindowLong(FHeaderHandle,
    GWL_WNDPROC)) = FNewHeaderProc) then
  begin
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FPrevHeaderProc));
    FPrevHeaderProc := nil;
    FHeaderHandle := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.HeaderWndProc(var AMsg: TMessage);
var
  Info: THDHitTestInfo;
begin
  with AMsg do
  begin
    Result := CallWindowProc(FPrevHeaderProc, FHeaderHandle, Msg, WParam, LParam);
    case Msg of
      WM_PAINT:
        DrawSortMark;
      WM_LBUTTONDOWN: begin
      if not (csDesigning in ComponentState) then if not _ASK then _TADA;
        FHeadLBDown := True;
        Info.Point.X := TWMLButtonDown(AMsg).Pos.X;
        Info.Point.Y := TWMLButtonDown(AMsg).Pos.Y;
        FHeadLBCol := SendMessage(FHeaderHandle, HDM_HITTEST, 0, Integer(@Info));
        if (Info.Flags and HHT_ONDIVIDER) = 0 then
          FHeadOnDiv := False
        else
          FHeadOnDiv := True;
      end;
      WM_LBUTTONUP: begin
        FHeadLBDown := False;
        FHeadOnDiv := False;
      end;
      WM_MOUSEMOVE:
        if FHeadOnDiv then DrawSortMark;
    end;
  end;{with AMsg do}
end;{procedure TabfCustomListView.HeadWndProc}


//==============================================================================
// Properties Get/Set

procedure TabfCustomListView.SetSortColumn(Value: Word);
begin
// Constrain value
  if Columns.Count = 0 then Value := 0 else
  if Value >= Columns.Count then Value := Columns.Count - 1;
// Assign property 
  if FSortColumn = Value then Exit;
  FSortColumn := Value;
  RepaintHeader;
  if SortType <> stNone then AlphaSort;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.SetSortDirection(Value: TabfListViewSortDirection);
begin
  if FSortDirection = Value then Exit;
  FSortDirection := Value;
  RepaintHeader;
  if SortType <> stNone then AlphaSort;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.SetShowSortMark(Value: Boolean);
begin
  if FShowSortMark = Value then Exit;
  FShowSortMark := Value;
  RepaintHeader;
end;

//------------------------------------------------------------------------------

function TabfCustomListView.GetSortType: TSortType;
begin
  Result := inherited SortType;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.SetSortType(Value: TSortType);
begin
  if SortType = Value then Exit;
  inherited SortType := Value;
  RepaintHeader;
end;


//==============================================================================
// Messages

procedure TabfCustomListView.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    ListView_SetTextBkColor(Handle, ColorToRGB(Color));
    ListView_SetBkColor(Handle, ColorToRGB(Color));
  end
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
//  if not (csReading in ComponentState) and
//     (Message.WindowPos^.flags and SWP_NOSIZE = 0) and HandleAllocated then
//    DoAutoSize;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TabfCustomListView.WMParentNotify(var Message: TWMParentNotify);
begin
  inherited;
  with Message do
    if (Event = WM_CREATE) and
      ((FHeaderHandle = 0) or (FHeaderHandle <> ListView_GetHeader(Handle))) then
    begin
      if IsWindow(FHeaderHandle) then UnhookHeader;
      HookHeader(ChildWnd);
    end;
end;


//==============================================================================
// TabfProgressBar
//==============================================================================
// TabfProgressBar is an enhanced progress bar. Allows specify background and
// bar colors. Can be horizontal or vertical.
{ TabfProgressBar }

constructor TabfProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FBarColor := clHighlight;
  FColor := clBtnFace;
{$IfNDef D4}
  FOrientation := pbHorizontal;
  FSmooth := False;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TabfProgressBar.CreateWnd;
begin
  inherited CreateWnd;
  Position := FPosition;
// Set colors 
  SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(BarColor));
  SendMessage(Handle, PBM_SETBKCOLOR , 0, ColorToRGB(Color));
end;

//------------------------------------------------------------------------------

procedure TabfProgressBar.DestroyWnd;
begin
{ Save the position }
  FPosition := Position;
  inherited DestroyWnd;
end;

//------------------------------------------------------------------------------

procedure TabfProgressBar.Loaded;
var
  Temp: Integer;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) and (Orientation = pbVertical) then
  begin
    Temp   := Width;
    Width  := Height;
    Height := Temp;
  end;
end;

//------------------------------------------------------------------------------
{$IfNDef D4}

procedure TabfProgressBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if Orientation = pbVertical then Style := Style or PBS_VERTICAL;
    if Smooth then Style := Style or PBS_SMOOTH;
  end;
end;

{$EndIf D4}

//==============================================================================
{ Properties Get/Set }

procedure TabfProgressBar.SetBarColor(Value: TColor);
begin
  if BarColor = Value then Exit;
  FBarColor := Value;
  if HandleAllocated then
    SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(BarColor));
end;

//------------------------------------------------------------------------------

procedure TabfProgressBar.SetColor(Value: TColor);
begin
// Don't check previous color! Needed for system color changing
  FColor := Value;
// Background color is ignored, bug in Windows?
  if HandleAllocated then
    SendMessage(Handle, PBM_SETBKCOLOR, 0, ColorToRGB(Color));
  Invalidate;
end;

//------------------------------------------------------------------------------

function TabfProgressBar.GetOrientation: TProgressBarOrientation;
begin
{$IfNDef D4}
  Result := FOrientation;
{$Else D4}
  Result := inherited Orientation;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TabfProgressBar.SetOrientation(Value: TProgressBarOrientation);
begin
  if Orientation = Value then Exit;
// Swap height and width in design-time
  if (csDesigning in ComponentState) then SetBounds(Left, Top, Height, Width);
{$IfNDef D4}
  FOrientation := Value;
  RecreateWnd;
{$Else D4}
  inherited Orientation := Value;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

function TabfProgressBar.GetPosition: Integer;
begin
  Result := inherited Position;
end;

//------------------------------------------------------------------------------

procedure TabfProgressBar.SetPosition(Value: Integer);
begin
  inherited Position := Value;
  FPosition := inherited Position;
end;

//------------------------------------------------------------------------------
{$IfNDef D4}

procedure TabfProgressBar.SetSmooth(Value: Boolean);
begin
  if FSmooth = Value then Exit;
  FSmooth := Value;
  RecreateWnd;
end;

{$EndIf D4}

//==============================================================================
// Messages routines

procedure TabfProgressBar.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
// Fix for background color
  Message.Result := 1;
  abfFillRect(Message.DC, ClientRect, ColorToRGB(Color))
end;

//------------------------------------------------------------------------------

procedure TabfProgressBar.CMSysColorChange(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  inherited;
// Update background color
  Color := Color;
end;


//==============================================================================
// TabfTrackBar
//==============================================================================
// Enhanced track bar.
{ TabfTrackBar }

constructor TabfTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FSelRange := False;
{$IfNDef D4}
  FSliderVisible := True;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TabfTrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FSelRange then
    Params.Style := Params.Style or TBS_ENABLESELRANGE
  else
    Params.Style := Params.Style and (not TBS_ENABLESELRANGE);
  if not SliderVisible then
    Params.Style := Params.Style or TBS_NOTHUMB;
end;

//------------------------------------------------------------------------------

procedure TabfTrackBar.SetSelRange(Value: Boolean);
begin
  if FSelRange = Value then Exit;
  FSelRange := Value;
  RecreateWnd;
end;

//------------------------------------------------------------------------------
{$IfNDef D4}

procedure TabfTrackBar.SetSliderVisible(Value: Boolean);
begin
  if FSliderVisible = Value then Exit;
  FSliderVisible := Value;
  RecreateWnd;
end;

{$EndIf D4}
//------------------------------------------------------------------------------

procedure TabfTrackBar.CMSysColorChange(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  inherited;
// Update background and thumb colors
  RecreateWnd;
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

  abfListViewSortProc         := @_DefaultListViewSort;
  abfListViewCompareItemsProc := @_DefaultListViewCompareItems;

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfCustomListView, TabfCustomRichEdit, TabfListView,
    TabfProgressBar, TabfTrackBar, TabfRichEdit]);{}

end{unit abfComCtrls}.
