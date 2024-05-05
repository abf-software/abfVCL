{*******************************************************************************

  abfEdits Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEditsDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D3}
  ExtDlgs,
{$EndIf D3}
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, Buttons, ComCtrls,
// ABF VCL
  abfControls, abfEdits;

const
  cFlatButtons = False; // Are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmEditsDemoMain
//==============================================================================
// Main form of application.
{ TfrmEditsDemoMain }

  TfrmEditsDemoMain = class(TForm)
    grpIE: TGroupBox;
      imgIE: TImage;
      lbIEInfo: TLabel;
      abfIE: TabfIntegerEdit;
      lbIENumberFormat: TLabel;
        cmbIENumberFormat: TComboBox;
      chbIEEnabled: TCheckBox;
      chbIEFlat: TCheckBox;
      chbIEFocusBorder: TCheckBox;

    grpAE: TGroupBox;
      imgAE: TImage;
      lbAEInfo: TLabel;
      abfAE: TabfAdvancedEdit;
      lbAEButtonControl: TLabel;
        cmbAEButtonControl: TComboBox;
      chbAEEnabled: TCheckBox;
      chbAEFlat: TCheckBox;
      chbAEFocusBorder: TCheckBox;
      chbAEButtonInside: TCheckBox;
      chbAEButtonVisible: TCheckBox;
      chbAEButtonAligment: TCheckBox;
      SpeedButton: TSpeedButton;
      UpDown: TUpDown;
      Image: TImage;
      Shape: TShape;

    grpBE: TGroupBox;
      imgBE: TImage;
      lbBEInfo: TLabel;
      abfBE: TabfButtonEdit;
      chbBEFlat: TCheckBox;
      chbBEEnabled: TCheckBox;
      chbBEButtonInside: TCheckBox;
      chbBEButtonVisible: TCheckBox;
      chbBEButtonAlignment: TCheckBox;

    grpUDE: TGroupBox;
      imgUDE: TImage;
      lbUDEInfo: TLabel;
      abfUDE: TabfUpDownEdit;
      chbUDEFlat: TCheckBox;
      chbUDEEnabled: TCheckBox;
      chbUDEButtonInside: TCheckBox;
      chbUDEButtonVisible: TCheckBox;
      chbUDEButtonAligment: TCheckBox;

    grpFE: TGroupBox;
      imgFE: TImage;
      lbFEInfo: TLabel;
      abfFE: TabfFileNameEdit;
      lbFEDialogKind: TLabel;
        cmbFEDialogKind: TComboBox;
    grpDE: TGroupBox;
      imgDE: TImage;
      lbDEInfo: TLabel;
      abfDE: TabfDirectoryNameEdit;
      chbDEButtonaligment: TCheckBox;

  // Non-visual
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
        mi1: TMenuItem;
      miExit: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure EnabledClick(Sender: TObject);
    procedure FlatClick(Sender: TObject);
    procedure FocusBorderClick(Sender: TObject);
    procedure AlignmentChange(Sender: TObject);
    procedure cmbIENumberFormatChange(Sender: TObject);
    procedure ButtonInsideClick(Sender: TObject);
    procedure ButtonVisibleClick(Sender: TObject);
    procedure ButtonAlignmentClick(Sender: TObject);
    procedure cmbAEButtonControlChange(Sender: TObject);
    procedure abfBEButtonClick(Sender: TObject);
    procedure cmbFEDialogKindChange(Sender: TObject);
  end;

var
  frmEditsDemoMain: TfrmEditsDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfAboutComponent, abfAboutApplication;
  
const
  cButtonAligments: array[Boolean] of TabfEditorButtonAlignment = (
    baRight, baLeft);

{$IfNDef D3}
type
  TOpenPictureDialog = TOpenDialog;
{$EndIf D3}

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmEditsDemoMain) then
    Application.CreateForm(TfrmEditsDemoMain, frmEditsDemoMain);
  frmEditsDemoMain.Show;
  frmEditsDemoMain.BringToFront;
end;


//==============================================================================
// TfrmEditsDemoMain
//==============================================================================
// Main form of application.
{ TfrmEditsDemoMain }

procedure TfrmEditsDemoMain.FormCreate(Sender: TObject);
begin
  cmbIENumberFormat.ItemIndex := Integer(abfIE.NumberFormat);
  cmbAEButtonControl.ItemIndex := 0;
  cmbFEDialogKind.ItemIndex := Integer(abfFE.DialogKind);
// Fix form width and height
  Width  := cWidth;
  Height := cHeight;
{$IfDef D3}
//  btnImageLoad .Flat := cFlatButtons;
//  btnIconDelete.Flat := cFlatButtons;
{$EndIf D3}
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfIE, '', '', True);
    2: abfComponentAboutEx(abfAE, '', '', True);
    3: abfComponentAboutEx(abfBE, '', '', True);
    4: abfComponentAboutEx(abfUDE, '', '', True);
    5: abfComponentAboutEx(abfFE, '', '', True);
    6: abfComponentAboutEx(abfDE, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.EnabledClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    1: abfIE.Enabled := Checked;
    2: abfAE.Enabled := Checked;
    3: abfBE.Enabled := Checked;
    4: abfUDE.Enabled := Checked;
    5: abfFE.Enabled := Checked;
    6: abfDE.Enabled := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.FlatClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    1: abfIE.Flat := Checked;
    2: abfAE.Flat := Checked;
    3: abfBE.Flat := Checked;
    4: abfUDE.Flat := Checked;
    5: abfFE.Flat := Checked;
    6: abfDE.Flat := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.FocusBorderClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    1: abfIE.FocusBorder := Checked;
    2: abfAE.FocusBorder := Checked;
    3: abfBE.FocusBorder := Checked;
    4: abfUDE.FocusBorder := Checked;
    5: abfFE.FocusBorder := Checked;
    6: abfDE.FocusBorder := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.AlignmentChange(Sender: TObject);
begin
  with TComboBox(Sender) do
  case Tag of
    1: abfIE.Alignment := TAlignment(ItemIndex);
    2: abfAE.Alignment := TAlignment(ItemIndex);
    3: abfBE.Alignment := TAlignment(ItemIndex);
    4: abfUDE.Alignment := TAlignment(ItemIndex);
    5: abfFE.Alignment := TAlignment(ItemIndex);
    6: abfDE.Alignment := TAlignment(ItemIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.ButtonInsideClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    2: abfAE.ButtonInside := Checked;
    3: abfBE.ButtonInside := Checked;
    4: abfUDE.ButtonInside := Checked;
    5: abfFE.ButtonInside := Checked;
    6: abfDE.ButtonInside := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.ButtonVisibleClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    2: abfAE.ButtonVisible := Checked;
    3: abfBE.ButtonVisible := Checked;
    4: abfUDE.ButtonVisible := Checked;
    5: abfFE.ButtonVisible := Checked;
    6: abfDE.ButtonVisible := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.ButtonAlignmentClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    2: abfAE.ButtonAlignment := cButtonAligments[Checked];
    3: abfBE.ButtonAlignment := cButtonAligments[Checked];
    4: abfUDE.ButtonAlignment := cButtonAligments[Checked];
    5: abfFE.ButtonAlignment := cButtonAligments[Checked];
    6: abfDE.ButtonAlignment := cButtonAligments[Checked];
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.cmbAEButtonControlChange(Sender: TObject);
begin
  if Assigned(abfAE.ButtonControl) then abfAE.ButtonControl.Visible := False;

  case cmbAEButtonControl.ItemIndex of
    0: abfAE.ButtonControl := nil;
    1: abfAE.ButtonControl := SpeedButton;
    2: abfAE.ButtonControl := UpDown;
    3: abfAE.ButtonControl := Image;
    4: abfAE.ButtonControl := Shape;
  end;

  if Assigned(abfAE.ButtonControl) then abfAE.ButtonControl.Visible := True;
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.cmbIENumberFormatChange(Sender: TObject);
begin
  abfIE.NumberFormat := TabfNumberFormat(cmbIENumberFormat.ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.abfBEButtonClick(Sender: TObject);
begin
  ShowMessage('Editor''s button clicked');
end;

//------------------------------------------------------------------------------

procedure TfrmEditsDemoMain.cmbFEDialogKindChange(Sender: TObject);
begin
  abfFE.DialogKind := TabfFileDialogKind(cmbFEDialogKind.ItemIndex);
end;

//------------------------------------------------------------------------------

end{unit abfEditsDemoMain}.
