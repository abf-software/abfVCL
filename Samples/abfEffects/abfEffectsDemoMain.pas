{*******************************************************************************

  abfEffects Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEffectsDemoMain;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, Buttons, Menus, ComCtrls,
  abfComponents, abfEffects;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmMain
//==============================================================================
// Main form of the application

  TfrmEffectsDemoMain = class(TForm)
    abfFormEffect: TabfFormEffect;
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
        mi1: TMenuItem;
      miExit: TMenuItem;
    grpFE: TGroupBox;
      imgFE: TImage;
      lbFEInfo: TLabel;
      grpFERolling: TGroupBox;
        chbFERollingAnimate: TCheckBox;
        chbFERollingOnMinMax: TCheckBox;
        lbFERollingDelay: TLabel;
          edFERollingDelay: TEdit;
          udFERollingDelay: TUpDown;
        lbFERollingHeight: TLabel;
          edFERollingHeight: TEdit;
          udFERollingHeight: TUpDown;
      grpFEShaking: TGroupBox;
        lbFEShakingAmplitude: TLabel;
          edFEShakingAmplitude: TEdit;
          udFEShakingAmplitude: TUpDown;
        lbFEShakingInterval: TLabel;
          edFEShakingInterval: TEdit;
          udFEShakingInterval: TUpDown;
        lbFEShakingTime: TLabel;
          edFEShakingTime: TEdit;
          udFEShakingTime: TUpDown;
      btnFERolling: TButton;
      btnFEShaking: TButton;
    grpBG: TGroupBox;
      lbBGInfo: TLabel;
      imgBG: TImage;
      lbBGFillType: TLabel;
        cmbBGFillType: TComboBox;
      pnBGSample: TPanel;
        abfBackGround: TabfBackGround;
        lbBGSample: TLabel;
    grpC: TGroupBox;
      lbCInfo: TLabel;
      imgC: TImage;
      pnCSample: TPanel;
        abfCredits: TabfCredits;
    abfMovablePanel: TabfMovablePanel;
      imgMP: TImage;
      lbMPCaption: TLabel;
      lbMPInfo: TLabel;
      lbMPMark: TLabel;
      chbMPSound: TCheckBox;
    wavMPDo: TabfWav;
    wavMPUndo: TabfWav;
  // Events
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure btnFERollingClick(Sender: TObject);
    procedure btnFEShakingClick(Sender: TObject);
    procedure chbClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure cmbBGFillTypeClick(Sender: TObject);
    procedure BGSampleDblClick(Sender: TObject);
    procedure abfCreditsDblClick(Sender: TObject);
    procedure MovablePanelDblClick(Sender: TObject);
    procedure abfMovablePanelDoMove(Sender: TObject);
    procedure abfMovablePanelUndoMove(Sender: TObject);
    procedure abfMovablePanelMoveEnd(Sender: TObject);
  private
    procedure UpdateFormEffect;
  end;{TfrmMain = class(TForm)}

var
  frmEffectsDemoMain: TfrmEffectsDemoMain;

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
  if not Assigned(frmEffectsDemoMain) then
    Application.CreateForm(TfrmEffectsDemoMain, frmEffectsDemoMain);
  frmEffectsDemoMain.Show;
  frmEffectsDemoMain.BringToFront;
end;


//==============================================================================
// TfrmMain
//==============================================================================
// Main form of the application
{ TfrmMain }

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.UpdateFormEffect;
begin
  chbFERollingAnimate .Checked := abfFormEffect.Rolling.Animate;
  chbFERollingOnMinMax.Checked := abfFormEffect.Rolling.OnMinMax;
  udFERollingDelay    .Position := Integer(abfFormEffect.Rolling.Delay);
  udFERollingHeight   .Position := abfFormEffect.Rolling.Height;
  udFEShakingamplitude.Position := abfFormEffect.Shaking.Amplitude;
  udFEShakingInterval .Position := abfFormEffect.Shaking.Interval;
  udFEShakingTime     .Position := abfFormEffect.Shaking.Time;
end;

//==============================================================================
// Events

procedure TfrmEffectsDemoMain.FormCreate(Sender: TObject);

  //-------------------------------------

  procedure _InitBGFillType;
  var
    i: Integer;
  begin
    cmbBGFillType.Items.Assign(abfGraphicFillList);
    for i := cmbBGFillType.Items.Count - 1 downto 0 do
      if (Pos('(', cmbBGFillType.Items[i]) > 0) then
        cmbBGFillType.Items.Delete(i);
    cmbBGFillType.ItemIndex := 0;
  end;{Internal _InitBGFillType}

  //-------------------------------------

begin
// Init TabfFormEffect routines
  udFERollingDelay.Min := Low(abfFormEffect.Rolling.Delay);
  udFERollingDelay.Max := High(abfFormEffect.Rolling.Delay);
  UpdateFormEffect;
// Init TabfBackGround routines
  _InitBGFillType;
// Fix form width and height
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
//  if Width  < cWidth  then Width  := cWidth;
//  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
//  if NewWidth  < cWidth  then NewWidth  := cWidth;
//  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfFormEffect, '', '', True);
    2: abfComponentAboutEx(abfBackGround, '', '', True);
    3: abfComponentAboutEx(abfCredits, '', '', True);
    4: abfComponentAboutEx(abfMovablePanel, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.btnFERollingClick(Sender: TObject);
begin
  abfFormEffect.Roll(not abfFormEffect.Rolled);
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.btnFEShakingClick(Sender: TObject);
begin
  abfFormEffect.Shake;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.chbClick(Sender: TObject);
begin
  with (Sender as TCheckBox) do
  begin
    case Tag of
      1: abfFormEffect.Rolling.Animate := Checked;
      2: abfFormEffect.Rolling.OnMinMax := Checked;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.EditChange(Sender: TObject);
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
      1: abfFormEffect.Rolling.Delay := TabfFormEffectRollingDelay(Value);
      2: abfFormEffect.Rolling.Height := Value;
      3: abfFormEffect.Shaking.Amplitude := Value;
      4: abfFormEffect.Shaking.Interval := Value;
      5: abfFormEffect.Shaking.Time := Value;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.cmbBGFillTypeClick(Sender: TObject);
begin
  abfBackGround.FillType := cmbBGFillType.Text;
end;

//------------------------------------------------------------------------------
// Select next FillType on each DoubleClick

procedure TfrmEffectsDemoMain.BGSampleDblClick(Sender: TObject);
begin
  if cmbBGFillType.ItemIndex >= cmbBGFillType.Items.Count - 1 then
    cmbBGFillType.ItemIndex := 0
  else
    cmbBGFillType.ItemIndex := cmbBGFillType.ItemIndex + 1;
  cmbBGFillTypeClick(nil);
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.abfCreditsDblClick(Sender: TObject);
begin
  abfCredits.Backward := not abfCredits.Backward;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.MovablePanelDblClick(Sender: TObject);
begin
  if abfMovablePanel.Moved then
    abfMovablePanel.UndoMove
  else
    abfMovablePanel.DoMove;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.abfMovablePanelDoMove(Sender: TObject);
begin
  if chbMPSound.Checked then wavMPDo.Play;
end;

//------------------------------------------------------------------------------

procedure TfrmEffectsDemoMain.abfMovablePanelUndoMove(Sender: TObject);
begin
  if chbMPSound.Checked then wavMPUndo.Play;
end;

//------------------------------------------------------------------------------
// Changes caption of the Mark label

procedure TfrmEffectsDemoMain.abfMovablePanelMoveEnd(Sender: TObject);
begin
  if abfMovablePanel.Moved then
   lbMPMark.Caption := '>>'
  else
   lbMPMark.Caption := '<<';
end;

//------------------------------------------------------------------------------

end{unit abfEffectsDemoMain}.
