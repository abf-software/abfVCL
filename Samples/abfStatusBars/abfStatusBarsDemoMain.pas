{*******************************************************************************

  abfStatusBars Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfStatusBarsDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, Buttons, Menus, ComCtrls, abfComponents, abfStatusBars;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmMain
//==============================================================================

  TfrmStatusBarsDemoMain = class(TForm)
  { TabfStatusBar }
    abfStatusBar: TabfStatusBar;
    grpSB: TGroupBox;
      imgStatusBar: TImage;
      lbSBInfo: TLabel;
      chbSBShowGlyph: TCheckBox;
      chbSBGlyphRight: TCheckBox;
      chbSBShowProgressCaption: TCheckBox;
      chbSBProgressInPercents: TCheckBox;
      grpSBScroll: TRadioGroup;
      grpSBBevel: TRadioGroup;
      chbSBSolidBrush: TCheckBox;
      chbSBNumLock: TCheckBox;
      chbSBCapsLock: TCheckBox;
      chbSBScrollLock: TCheckBox;
  { TabfStatusBarInserter }
    abfStatusBarInserter1: TabfStatusBarInserter;
    abfStatusBarInserter2: TabfStatusBarInserter;
    grpSBI: TGroupBox;
      imfSBI: TImage;
      lbSBIInfo: TLabel;
      grpSBIControls: TGroupBox;
        Edit: TEdit;
        ComboBox: TComboBox;
        Image: TImage;
        CheckBox: TCheckBox;
        Button: TButton;
        ProgressBar: TProgressBar;
        TrackBar: TTrackBar;
        Panel: TPanel;
          btnSBIPanel1: TSpeedButton;
          btnSBIPanel2: TSpeedButton;
          pnSBIPanelShape: TShape;
      lbSBIControl1: TLabel;
        cmbSBIControl1: TComboBox;
      lbSBIControl2: TLabel;
        cmbSBIControl2: TComboBox;
  { Non-visual }
    tmrSB: TTimer;
    pmSB: TPopupMenu;
      miItem1: TMenuItem;
      miItem2: TMenuItem;
      miItem3: TMenuItem;
        mi1: TMenuItem;
      miAbout: TMenuItem;
    imgsSB: TImageList;
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
        mi0: TMenuItem;
      miExit: TMenuItem;
  { Events }
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  { abfStatusBar }
    procedure tmrSBTimer(Sender: TObject);
    procedure abfStatusBarPanelDblClick(Panel: TabfStatusPanel;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chbSBShowGlyphClick(Sender: TObject);
    procedure chbSBGlyphRightClick(Sender: TObject);
    procedure chbSBShowProgressCaptionClick(Sender: TObject);
    procedure chbSBProgressInPercentsClick(Sender: TObject);
    procedure grpSBScrollClick(Sender: TObject);
    procedure grpSBBevelClick(Sender: TObject);
    procedure chbSBSolidBrushClick(Sender: TObject);
    procedure chbSBLockClick(Sender: TObject);
  { abfStatusBarInserter }
    procedure cmbSBIControlClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure btnSBIPanel2Click(Sender: TObject);
  private
    ProgressPos: Integer;
    procedure UpdateScrollTextHint;
    procedure UpdateControlComboBoxes;
  end;

var
  frmStatusBarsDemoMain: TfrmStatusBarsDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmStatusBarsDemoMain) then
    Application.CreateForm(TfrmStatusBarsDemoMain, frmStatusBarsDemoMain);
  frmStatusBarsDemoMain.Show;
  frmStatusBarsDemoMain.BringToFront;
end;


//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

procedure TfrmStatusBarsDemoMain.UpdateScrollTextHint;
const
  SScrollText = 'This panel has a scrolling text, ScrollEffect = ';
  SNames: array [TabfScrollEffect] of string = ('seNone', 'seFromLeftToRight',
    'seFromRightToLeft', 'seChangeableDirection');
begin
  abfStatusBar.Panels[2].Hint := SScrollText +
    SNames[abfStatusBar.Panels[2].TextStyle.ScrollEffect];
  grpSBScroll.ItemIndex := Integer(abfStatusBar.Panels[2].TextStyle.ScrollEffect);
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.UpdateControlComboBoxes;
begin
  if not Assigned(abfStatusBarInserter1.Control) then
    cmbSBIControl1.ItemIndex := 0;
  if not Assigned(abfStatusBarInserter2.Control) then
    cmbSBIControl2.ItemIndex := 0;
end;

//==============================================================================
// Events

procedure TfrmStatusBarsDemoMain.FormCreate(Sender: TObject);

  //-------------------------------------

  procedure _EnumControlsToInsert;
  var
    i: Integer;
  begin
    cmbSBIControl1.Items.Clear;
    cmbSBIControl1.Items.Add('(None)');
    with grpSBIControls do
      for i := 0 to ControlCount - 1 do
        cmbSBIControl1.Items.Add(Controls[i].Name);
    cmbSBIControl2.Items := cmbSBIControl1.Items;
  end;{Internal procedure _EnumControlsToInsert}

  //-------------------------------------

begin
  ProgressPos := abfStatusBar.Panels[1].ProgressBarStyle.Progress;
{$IfDef D4}
  TrackBar.ThumbLength := 10;
{$EndIf D4}
  _EnumControlsToInsert;
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.FormShow(Sender: TObject);
begin
  abfStatusBar.Panels[2].TextStyle.ScrollEffect := seChangeableDirection;
  UpdateScrollTextHint;
  tmrSB.Enabled := True;
  UpdateControlComboBoxes;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfStatusBar, '', '', True);
    2: abfComponentAboutEx(abfStatusBarInserter1, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;


//==============================================================================
// abfStatusBar

procedure TfrmStatusBarsDemoMain.tmrSBTimer(Sender: TObject);
begin
  Inc(ProgressPos);
  abfStatusBar.Panels[1].ProgressBarStyle.Progress := ProgressPos;
  if ProgressPos >= 100 then ProgressPos := 0;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.abfStatusBarPanelDblClick(Panel: TabfStatusPanel;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (Panel.Name = 'ScrollText') and (Button = mbLeft) then Exit;
  with Panel.TextStyle do
    if ScrollEffect >= High(ScrollEffect) then ScrollEffect := Low(ScrollEffect)
    else ScrollEffect := Succ(ScrollEffect);
  UpdateScrollTextHint;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.chbSBShowGlyphClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then abfStatusBar.Panels[0].TextStyle.GlyphIndex := 0
  else abfStatusBar.Panels[0].TextStyle.GlyphIndex := -1;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.chbSBGlyphRightClick(Sender: TObject);
begin
  abfStatusBar.Panels[0].TextStyle.Layout :=
    TabfTextLayout(TCheckBox(Sender).Checked);
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.chbSBShowProgressCaptionClick(Sender: TObject);
begin
  abfStatusBar.Panels[1].ProgressBarStyle.ShowCaption := TCheckBox(Sender).Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.chbSBProgressInPercentsClick(Sender: TObject);
begin
  abfStatusBar.Panels[1].ProgressBarStyle.ProgressType :=
    TabfProgressType(not TCheckBox(Sender).Checked);
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.grpSBScrollClick(Sender: TObject);
begin
  abfStatusBar.Panels[2].TextStyle.ScrollEffect :=
    TabfScrollEffect(TRadioGroup(Sender).ItemIndex);
  UpdateScrollTextHint;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.grpSBBevelClick(Sender: TObject);
begin
  abfStatusBar.Panels[2].Bevel :=
    TabfStatusPanelBevel(TRadioGroup(Sender).ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.chbSBSolidBrushClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then abfStatusBar.Panels[2].BrushStyle := bsSolid
  else abfStatusBar.Panels[2].BrushStyle := bsBDiagonal;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.chbSBLockClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    abfStatusBar.Panels[Tag + 2].Visible := Checked;
end;


//==============================================================================
// abfStatusBarInserter

procedure TfrmStatusBarsDemoMain.cmbSBIControlClick(Sender: TObject);
var
  Inserter, OtherInserter: TabfStatusBarInserter;
  ControlToInsert: TControl;
begin
  with TComboBox(Sender) do
  begin
    if Tag = 1 then
    begin
      Inserter := abfStatusBarInserter1;
      OtherInserter := abfStatusBarInserter2;
    end else
    begin
      Inserter := abfStatusBarInserter2;
      OtherInserter := abfStatusBarInserter1;
    end;
  { Check for (None) item }
    if ItemIndex = 0 then
    begin
      Inserter.Control := nil;
      Exit;
    end;
  { Get the Control and insert it }
    ControlToInsert := TControl(Self.FindComponent(Items[ItemIndex]));
    if not Assigned(ControlToInsert)
      or (Inserter.Control = ControlToInsert) then Exit;
  { Check is control already inserted to statusbar by other inserter }
    if (OtherInserter.Control = ControlToInsert) then
      OtherInserter.Control := nil;
  { Insert control to the statusbar }
    Inserter.Control := ControlToInsert;
  end;

  UpdateControlComboBoxes;
end;{procedure TfrmMain.cmbSBIControlClick}

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.TrackBarChange(Sender: TObject);
const
  SHint = 'Position = ';
begin
  TrackBar.Hint := SHint + IntToStr(TrackBar.Position);
  ProgressBar.Position := TrackBar.Position;
  ProgressBar.Hint := TrackBar.Hint;
end;

//------------------------------------------------------------------------------

procedure TfrmStatusBarsDemoMain.btnSBIPanel2Click(Sender: TObject);
begin
  Beep;
end;

//------------------------------------------------------------------------------

end{unit abfStatusBarsDemoMain}.
