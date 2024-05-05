{*******************************************************************************

  abfComControls Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComControlsDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, Buttons, Menus, Dialogs, ComCtrls, abfComControls;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmMain
//==============================================================================

  TfrmComControlsDemoMain = class(TForm)
    grpLV: TGroupBox;
      imgLV: TImage;
      lbLVInfo: TLabel;
      abfListView: TabfListView;
      lbLVSortType: TLabel;
        cmbLVSortType: TComboBox;
      lbLVSortColumn: TLabel;
        edLVSortColumn: TEdit;
        udLVSortColumn: TUpDown;
    grpP: TGroupBox;
      lbPInfo: TLabel;
      imgP: TImage;
      abfProgressBar: TabfProgressBar;
      chbPVertical: TCheckBox;
      chbPSmooth: TCheckBox;
      lbPPosition: TLabel;
        edPPosition: TEdit;
        udPPosition: TUpDown;
      btnPBarColor: TBitBtn;
      btnPColor: TBitBtn;
    grpT: TGroupBox;
      lbTInfo: TLabel;
      imgT: TImage;
      abfTrackBar: TabfTrackBar;
      chbTVertical: TCheckBox;
      chbTSelRange: TCheckBox;
      chbTSlider: TCheckBox;
  { Non-visual }
    imglstListView: TImageList;
    dlgColor: TColorDialog;
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
      mi1: TMenuItem;
      miExit: TMenuItem;
    pmListView: TPopupMenu;
      miResizeColumns: TMenuItem;
  { Events }
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure WebClick(Sender: TObject);
    procedure EditWithUpDownChange(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
  { TabfListView }
    procedure cmbLVSortTypeChange(Sender: TObject);
    procedure ResizeColumnsClick(Sender: TObject);
  { TabfProgressBar }
    procedure chbPVerticalClick(Sender: TObject);
    procedure btnPColorClick(Sender: TObject);
  { TabfTrackBar }
    procedure chbTVerticalClick(Sender: TObject);
  private
    procedure ChangeButtonGlyph(Button: TBitBtn; AColor: TColor);
  end;

var
  frmComControlsDemoMain: TfrmComControlsDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  TypInfo, abfTypInfo, abfConsts, abfVclUtils, abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmComControlsDemoMain) then
    Application.CreateForm(TfrmComControlsDemoMain, frmComControlsDemoMain);
  frmComControlsDemoMain.Show;
  frmComControlsDemoMain.BringToFront;
end;

//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

procedure TfrmComControlsDemoMain.ChangeButtonGlyph(Button: TBitBtn; AColor: TColor);
var
  Glyph: TBitmap;
  R: TRect;
begin
  Glyph := TBitmap.Create;
  with Glyph, Glyph.Canvas do
  try
    Canvas.Font := Button.Font;
    Width  := Button.Width - TextWidth(Button.Caption) - 2 * Button.Spacing - 4;
//    Width  := Button.Width  - 8;
    Height := Button.Height - 10;
    R := Rect(0, 0, Width, Height);
    Brush.Style := bsSolid;
  { Create transparent part becouse LeftBotom point specifies a transparent
    color of the glyph. Select color for it other then Application.HintColor }
    Brush.Color := clBtnFace;
    FrameRect(R);
  { Draw complex border }
    InflateRect(R, -1, -1);
    Brush.Color := clWhite;
    FrameRect(R);
    InflateRect(R, -1, -1);
    Brush.Color := clBtnShadow;
    FrameRect(R);
  { Draw color rect }
    InflateRect(R, -1, -1);
    Brush.Color := AColor;
    FillRect(R);
  { Assign new glyph to button }
    Button.Glyph := Glyph;
    Button.NumGlyphs := 1;
  finally
    Glyph.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.FormCreate(Sender: TObject);
begin
  abfGetEnumTypeValueNames(
    abfGetTypeInfo(
      GetPropInfo(abfListView.ClassInfo, 'SortType')),
    cmbLVSortType.Items);
  cmbLVSortType.ItemIndex := Integer(abfListView.SortType);
  ChangeButtonGlyph(btnPBarColor, abfProgressBar.BarColor);
  ChangeButtonGlyph(btnPColor, abfProgressBar.Color);
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  abfListView.FullDrag := True;
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfListView, '', '', True);
    2: abfComponentAboutEx(abfProgressBar, '', '', True);
    3: abfComponentAboutEx(abfTrackBar, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.WebClick(Sender: TObject);
begin
  abfGotoUrl(SabfWeb);
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.EditWithUpDownChange(Sender: TObject);
var
  UD: TUpDown;
  Value: Integer;
begin
  if (csLoading in ComponentState) then Exit;
  with (Sender as TEdit) do
  begin
    UD := TUpDown(Self.FindComponent('ud' + Copy(Name, 3, MaxInt)));
    Value := StrToIntDef(Text, UD.Position);
    Text := IntToStr(Value);
    case Tag of
      1: abfListView.SortColumn := Value;
      2: abfProgressBar.Position := Value;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.CheckBoxClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    case Tag of
      1: abfProgressBar.Smooth := Checked;
      2: abfTrackBar.SelRange := Checked;
      3: abfTrackBar.SliderVisible := Checked;
    end;
end;

//==============================================================================
// TabfListView

procedure TfrmComControlsDemoMain.cmbLVSortTypeChange(Sender: TObject);
begin
  abfListView.SortType := TSortType(TComboBox(Sender).ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.ResizeColumnsClick(Sender: TObject);
var
  i: Integer;
begin
  with abfListView do
  begin
    Columns.BeginUpdate;
    try
      for i := 0 to Columns.Count - 1 do
{$IfDef D4}
        Columns[i].AutoSize := True;
      abfListView.DoAutoSize;
{$Else D4}
        Columns[i].Width := 85;
{$EndIf D4}
    finally
      Columns.EndUpdate;
    end;
  end;
end;


//==============================================================================
// TabfProgressBar

procedure TfrmComControlsDemoMain.chbPVerticalClick(Sender: TObject);
begin
  with abfProgressBar do
    if TCheckBox(Sender).Checked then
    begin
      Width  := 20;
      Height := 103;
      Orientation := pbVertical;
    end else
    begin
      Width  := 280;
      Height := 20;
      Orientation := pbHorizontal;
    end;
end;

//------------------------------------------------------------------------------

procedure TfrmComControlsDemoMain.btnPColorClick(Sender: TObject);
begin
  if TBitBtn(Sender).Tag = 1 then
  begin
    dlgColor.Color := abfProgressBar.BarColor;
    if not dlgColor.Execute then Exit;
    abfProgressBar.BarColor := dlgColor.Color;
  end else
  begin
    dlgColor.Color := abfProgressBar.Color;
    if not dlgColor.Execute then Exit;
    abfProgressBar.Color := dlgColor.Color;
  end;
  ChangeButtonGlyph(TBitBtn(Sender), dlgColor.Color);
end;


//==============================================================================
// TabfTrackBar

procedure TfrmComControlsDemoMain.chbTVerticalClick(Sender: TObject);
begin
  with abfTrackBar do
    if TCheckBox(Sender).Checked then
    begin
      Orientation := trVertical;
      Width  := 45;
      Height := 85;
    end else
    begin
      Orientation := trHorizontal;
      Width  := 280;
      Height := 45;
    end;
end;

//------------------------------------------------------------------------------

end{unit abfComControlsDemoMain}.
