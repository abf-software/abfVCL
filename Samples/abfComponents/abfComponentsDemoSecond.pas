{*******************************************************************************

  abfComponents Demo. Second form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComponentsDemoSecond;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, ComCtrls, abfComponents;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmComponentsDemoSecond
//==============================================================================
// Second form of application.

  TfrmComponentsDemoSecond = class(TForm)
  // TabfShutdown
    abfShutdown: TabfShutdown;
    grpShutdown: TGroupBox;
      imgShutdown: TImage;
      lbShutdownInfo: TLabel;
      lbShutdown: TLabel;
        cmbShutdown: TComboBox;
      btnShutdown: TBitBtn;
  // TabfFileStorage
    abfFileStorage: TabfFileStorage;
    dlgFSSaveName: TSaveDialog;
    grpFS: TGroupBox;
      imgFS: TImage;
      lbFSInfo: TLabel;
      lbFSSaveName: TLabel;
        edFSSaveName: TEdit;
        btnFSSaveName: TSpeedButton;
       btnFS: TBitBtn;
  // TabfThreadTimer
    abfThreadTimer: TabfThreadTimer;
    grpTT: TGroupBox;
      imgTT: TImage;
      lbTTInfo: TLabel;
      lbTTPriority: TLabel;
      cmbTTPriority: TComboBox;
      lbInterval: TLabel;
        edTTInterval: TEdit;
        udTTInterval: TUpDown;
      chbTTEnabled: TCheckBox;
      pnTTIndicator: TPanel;
  // TabfColorPicker
    abfColorPicker: TabfColorPicker;
    grpCP: TGroupBox;
      imgCP: TImage;
      lbCPInfo: TLabel;
      lbCPInterval: TLabel;
        edCPInterval: TEdit;
        udCPInterval: TUpDown;
      chbCPEnabled: TCheckBox;
      pnCPColor: TPanel;
  // Non-visual
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
        mi1: TMenuItem;
      miExit: TMenuItem;
  // Visual
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure NumericEditChange(Sender: TObject);
    procedure CheckboxEnabledClick(Sender: TObject);
  // abfShutdown
    procedure btnShutdownClick(Sender: TObject);
  // abfFileStorage
    procedure btnFSSaveNameClick(Sender: TObject);
    procedure btnFSClick(Sender: TObject);
  // abfThreadTimer
    procedure abfThreadTimerTimer(Sender: TObject);
    procedure cmbTTPriorityChange(Sender: TObject);
  // abfColorPicker
    procedure abfColorPickerTimer(Sender: TObject);
  end;

var
  frmComponentsDemoSecond: TfrmComponentsDemoSecond;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  TypInfo, abfTypInfo, abfGraphics, abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmComponentsDemoSecond) then
    Application.CreateForm(TfrmComponentsDemoSecond, frmComponentsDemoSecond);
  frmComponentsDemoSecond.Show;
  frmComponentsDemoSecond.BringToFront;
end;


//==============================================================================
// TfrmComponentsDemoSecond
//==============================================================================
// Second form of application.
{ TfrmComponentsDemoSecond }

procedure TfrmComponentsDemoSecond.FormCreate(Sender: TObject);
begin
// abfShutdown
  abfGetTypeInfoValueNames(
    abfGetTypeInfo(
      abfGetPropInfo(abfShutdown.ClassInfo, 'ActionType')),
    cmbShutdown.Items);
  cmbShutdown.ItemIndex := Integer(abfShutdown.ActionType);
// abfThreadTimer
  abfGetTypeInfoValueNames(
    abfGetTypeInfo(
      abfGetPropInfo(abfThreadTimer.ClassInfo, 'Priority')),
    cmbTTPriority.Items);
  cmbTTPriority.ItemIndex := Integer(abfThreadTimer.Priority);
// Fix form width and height 
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfShutdown, '', '', True);
    2: abfComponentAboutEx(abfFileStorage, '', '', True);
    3: abfComponentAboutEx(abfThreadTimer, '', '', True);
    4: abfComponentAboutEx(abfColorPicker, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.NumericEditChange(Sender: TObject);
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
      1: abfThreadTimer.Interval := Value;
      2: abfColorPicker.Interval := Value;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.CheckboxEnabledClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    case Tag of
      1: abfThreadTimer.Enabled := Checked;
      2: abfColorPicker.Enabled := Checked;
    end;
end;

//==============================================================================
// abfShutdown

procedure TfrmComponentsDemoSecond.btnShutdownClick(Sender: TObject);
begin
  abfShutdown.ActionType := TabfShutdownActionType(cmbShutdown.ItemIndex);
  abfShutdown.Execute;
end;

//==============================================================================
// abfFileStorage

procedure TfrmComponentsDemoSecond.btnFSSaveNameClick(Sender: TObject);
begin
  dlgFSSaveName.FileName := edFSSaveName.Text;
  if not dlgFSSaveName.Execute then Exit;
  edFSSaveName.Text := dlgFSSaveName.FileName;
  btnFSClick(btnFS);
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.btnFSClick(Sender: TObject);
begin
  abfFileStorage.SaveName := edFSSaveName.Text;
  abfFileStorage.Save;
end;

//==============================================================================
// abfThreadTimer

var
  _TTCount: Cardinal = 0;

procedure TfrmComponentsDemoSecond.abfThreadTimerTimer(Sender: TObject);
begin
  Inc(_TTCount);
  pnTTIndicator.Caption := IntToStr(_TTCount);
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoSecond.cmbTTPriorityChange(Sender: TObject);
begin
  abfThreadTimer.Priority := TThreadPriority(TComboBox(Sender).ItemIndex);
end;

//==============================================================================
// abfColorPicker

procedure TfrmComponentsDemoSecond.abfColorPickerTimer(Sender: TObject);
var
  AColor: TColor;
begin
  AColor := abfColorPicker.Color;
  pnCPColor.Color := AColor;
  pnCPColor.Caption := ColorToString(AColor);
  with TabfColorQuad(AColor) do
    if (Red > 192) or (Green > 192) or (Blue > 192) then
      AColor := clBlack
    else
      AColor := clWhite;
  pnCPColor.Font.Color := AColor
end;

//------------------------------------------------------------------------------

end{unit abfComponentsDemoSecond}.
