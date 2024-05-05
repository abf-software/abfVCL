{*******************************************************************************

  abfLabels Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfLabelsDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, Buttons, Menus, Dialogs, ComCtrls, abfLabels;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmMain
//==============================================================================

  TfrmLabelsDemoMain = class(TForm)
    grpL: TGroupBox;
      imgL: TImage;
      lbLInfo: TLabel;
      bvL: TBevel;
        abfLabel: TabfLabel;
      lbLAngle: TLabel;
        edLAngle: TEdit;
        udLAngle: TUpDown;
      chbLHotTrack: TCheckBox;
      chbLTimer: TCheckBox;
    grpAL: TGroupBox;
      imgAL: TImage;
      lbALInfo: TLabel;
    abfActiveLabel: TabfActiveLabel;
    abfActiveLabel1: TabfActiveLabel;
    abfActiveLabel2: TabfActiveLabel;
    abfActiveLabel3: TabfActiveLabel;
    abfActiveLabel4: TabfActiveLabel;
  // Non-visual
    tmrL: TTimer;
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
      mi1: TMenuItem;
      miExit: TMenuItem;
  // Events
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
  // TabfLabel
    procedure tmrLTimer(Sender: TObject);
    procedure abfLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure abfLabelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure abfLabelMouseEnter(Sender: TObject);
    procedure abfLabelMouseLeave(Sender: TObject);
  private
    LPressed: Boolean;
    procedure UpdateAngle;
  end;

var
  frmLabelsDemoMain: TfrmLabelsDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfConsts, abfVclUtils, abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmLabelsDemoMain) then
    Application.CreateForm(TfrmLabelsDemoMain, frmLabelsDemoMain);
  frmLabelsDemoMain.Show;
  frmLabelsDemoMain.BringToFront;
end;


//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

procedure TfrmLabelsDemoMain.UpdateAngle;
begin
  udLAngle.Position := abfLabel.Angle;
end;


//==============================================================================
// Events

procedure TfrmLabelsDemoMain.FormCreate(Sender: TObject);
begin
{ Create a TabfActiveLabel with default cursors settings, and apply these settings }
  with TabfActiveLabel.Create(nil) do
  try
    abfActiveLabel .Cursors := Cursors;
    abfActiveLabel1.Cursors := Cursors;
    abfActiveLabel2.Cursors := Cursors;
    abfActiveLabel3.Cursors := Cursors;
    abfActiveLabel4.Cursors := Cursors;
  finally
    Free;
  end;
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfLabel, '', '', True);
    2: abfComponentAboutEx(abfActiveLabel, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.WebClick(Sender: TObject);
begin
  abfGotoUrl(SabfWeb);
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.EditWithUpDownChange(Sender: TObject);
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
      1: begin
        abfLabel.Angle := Value;
        UpdateAngle;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.CheckBoxClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    case Tag of
      1: tmrL.Enabled := Checked;
    end;
end;

//==============================================================================
// TabfLabel

procedure TfrmLabelsDemoMain.tmrLTimer(Sender: TObject);
begin
  abfLabel.Angle := abfLabel.Angle + 5;
  UpdateAngle;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.abfLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  LPressed := True;
  TabfLabel(Sender).Font.Color := clRed;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.abfLabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  LPressed := False;
  TabfLabel(Sender).Font.Color := clNavy;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.abfLabelMouseEnter(Sender: TObject);
begin
  if not chbLHotTrack.Checked then Exit;
  if LPressed then
    TabfLabel(Sender).Font.Color := clRed
  else
    TabfLabel(Sender).Font.Color := clBlue;
end;

//------------------------------------------------------------------------------

procedure TfrmLabelsDemoMain.abfLabelMouseLeave(Sender: TObject);
begin
  if not chbLHotTrack.Checked then Exit;
  if LPressed then
    TabfLabel(Sender).Font.Color := clBlue
  else
    TabfLabel(Sender).Font.Color := clNavy;
end;

//------------------------------------------------------------------------------

end{unit abfLabelsDemoMain}.
