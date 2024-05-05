{*******************************************************************************

  abfControls Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfControlsDemoMain;

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
  Menus, StdCtrls, ExtCtrls, Buttons,
// ABF VCL
  abfControls;

const
  cFlatButtons = False; // Are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmControlsDemoMain
//==============================================================================
// Main form of application.
{ TfrmControlsDemoMain }

  TfrmControlsDemoMain = class(TForm)
  // abfEdit
    grpEdit: TGroupBox;
      imgEdit: TImage;
      lbEditInfo: TLabel;
      abfEdit: TabfEdit;
      chbEditEnabled: TCheckBox;
      chbEditFlat: TCheckBox;
      chbEditFocusBorder: TCheckBox;
      lbEditAlignment: TLabel;
        cmbEditAlignment: TComboBox;
  // abfComboBox
    grpCombo: TGroupBox;
      imgCombo: TImage;
      lbComboInfo: TLabel;
      abfComboBox: TabfComboBox;
      chbComboEnabled: TCheckBox;
      chbComboFlat: TCheckBox;
      chbComboFocusBorder: TCheckBox;
      chbComboEtched: TCheckBox;
  // abfScrollBar
    grpScroll: TGroupBox;
      imgScroll: TImage;
      lbScrollInfo: TLabel;
      pnScroll: TPanel;
        abfScrollBar: TabfScrollBar;
        lbScrollMessage: TLabel;
      chbScrollHideButtons: TCheckBox;
      chbScrollTransparent: TCheckBox;
      lbScrollPosition: TLabel;
  // abfImage
    grpImage: TGroupBox;
      imgImage: TImage;
      lbImageInfo: TLabel;
      pnImage: TPanel;
        abfImage: TabfImage;
      chbImageCenter: TCheckBox;
      chbImageProportional: TCheckBox;
      chbImageStretch: TCheckBox;
      chbImageCaptionVisible: TCheckBox;
      chbImageCaptionWhenEmpty: TCheckBox;
      btnImageLoad: TSpeedButton;
      btnIconDelete: TSpeedButton;
  // abfGroupBox
    grpGroup: TGroupBox;
      imgGroup: TImage;
      lbGroupInfo: TLabel;
      abfGroupBox: TabfGroupBox;
        chbGroupSample: TCheckBox;
        edGroupSample: TEdit;
        cmbGroupSample: TComboBox;
  // abfGroupBox
    grpDate: TGroupBox;
      imgDate: TImage;
      lbDateInfo: TLabel;
      abfDatePanel: TabfDatePanel;
      chbDateEnabled: TCheckBox;
      chbDateFlat: TCheckBox;
      chbDateFocusBorder: TCheckBox;
      chbDateEtched: TCheckBox;
  // Non-visual
    ImageList: TImageList;
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
  // TabfEdit/TabfComboBox
    procedure EnabledClick(Sender: TObject);
    procedure FlatClick(Sender: TObject);
    procedure FocusBorderClick(Sender: TObject);
    procedure AlignmentChange(Sender: TObject);
    procedure EtchedClick(Sender: TObject);
  // TabfScrollBar
    procedure abfScrollBarChange(Sender: TObject);
    procedure chbScrollHideButtonsClick(Sender: TObject);
    procedure chbScrollTransparentClick(Sender: TObject);
  // TabfImage
    procedure chbImageClick(Sender: TObject);
    procedure btnImageLoadClick(Sender: TObject);
    procedure btnIconDeleteClick(Sender: TObject);
  end;

var
  frmControlsDemoMain: TfrmControlsDemoMain;

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

{$IfNDef D3}
type
  TOpenPictureDialog = TOpenDialog;
{$EndIf D3}

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmControlsDemoMain) then
    Application.CreateForm(TfrmControlsDemoMain, frmControlsDemoMain);
  frmControlsDemoMain.Show;
  frmControlsDemoMain.BringToFront;
end;


//==============================================================================
// TfrmControlsDemoMain
//==============================================================================
// Main form of application.
{ TfrmControlsDemoMain }

procedure TfrmControlsDemoMain.FormCreate(Sender: TObject);
begin
  cmbEditAlignment.ItemIndex := Integer(abfEdit.Alignment);
  abfScrollBarChange(abfScrollBar);
// Fix form width and height
  Width  := cWidth;
  Height := cHeight;
{$IfDef D3}
  btnImageLoad .Flat := cFlatButtons;
  btnIconDelete.Flat := cFlatButtons;
{$EndIf D3}
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfEdit, '', '', True);
    2: abfComponentAboutEx(abfComboBox, '', '', True);
    3: abfComponentAboutEx(abfScrollBar, '', '', True);
    4: abfComponentAboutEx(abfImage, '', '', True);
    5: abfComponentAboutEx(abfGroupBox, '', '', True);
    6: abfComponentAboutEx(abfDatePanel, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//==============================================================================
// TabfEdit/TabfComboBox

procedure TfrmControlsDemoMain.EnabledClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    1: abfEdit     .Enabled := Checked;
    2: abfComboBox .Enabled := Checked;
    6: abfDatePanel.Enabled := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.FlatClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    1: abfEdit     .Flat := Checked;
    2: abfComboBox .Flat := Checked;
    6: abfDatePanel.Flat := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.FocusBorderClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    1: abfEdit     .FocusBorder := Checked;
    2: abfCombobox .FocusBorder := Checked;
    6: abfDatePanel.FocusBorder := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.EtchedClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Tag of
    2: abfComboBox .Etched := Checked;
    6: abfDatePanel.Etched := Checked;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.AlignmentChange(Sender: TObject);
begin
  with TComboBox(Sender) do
  case Tag of
    1: abfEdit.Alignment := TAlignment(ItemIndex);
  end;
end;


//==============================================================================
// TabfScrollBar

procedure TfrmControlsDemoMain.abfScrollBarChange(Sender: TObject);
begin
  lbScrollPosition.Caption := 'Position: ' + IntToStr(abfScrollBar.Position);
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.chbScrollHideButtonsClick(Sender: TObject);
begin
  abfScrollBar.HideButtons := TCheckBox(Sender).Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.chbScrollTransparentClick(Sender: TObject);
begin
  abfScrollBar.Transparent := TCheckBox(Sender).Checked;
  lbScrollMessage.Visible := abfScrollBar.Transparent;
end;


//==============================================================================
// TabfImage

procedure TfrmControlsDemoMain.chbImageClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    case Tag of
      1: abfImage.Center           := Checked;
      2: abfImage.Proportional     := Checked;
      3: abfImage.Stretch          := Checked;
      4: abfImage.CaptionVisible   := Checked;
      5: abfImage.CaptionWhenEmpty := Checked;
    end;

// Update image on some video cards when switch Center property
  abfImage.Hide;
  pnImage.Repaint;
  abfImage.Show;
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.btnImageLoadClick(Sender: TObject);
var
  Dialog: TOpenPictureDialog;
begin
  Dialog := TOpenPictureDialog.Create(nil);
  try
    Dialog.Filter := GraphicFilter(TBitmap) + '|' + GraphicFilter(TIcon);
    if Dialog.Execute then
      abfImage.Picture.LoadFromFile(Dialog.FileName);
  finally
    Dialog.Free;
  end
end;

//------------------------------------------------------------------------------

procedure TfrmControlsDemoMain.btnIconDeleteClick(Sender: TObject);
begin
  abfImage.Picture := nil;
end;

//------------------------------------------------------------------------------

end{unit abfControlsDemoMain}.
