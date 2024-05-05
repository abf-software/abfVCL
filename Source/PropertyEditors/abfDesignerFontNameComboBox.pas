{*******************************************************************************

  ABF Visual Components Library, OneTouch(tm) Designer

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDesignerFontNameComboBox;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
  abfComboBoxes, abfDesignerComboBox, abfControls;

type

//==============================================================================
// TfrmColorComboBoxDesigner
//==============================================================================
// OneTouch Designer� form for the TabfFontNameComboBox component.

  TfrmDesignerFontNameComboBox = class(TForm)
    grpItemStyle: TGroupBox;
      btnItemStyle0: TSpeedButton;
      btnItemStyle1: TSpeedButton;
      btnItemStyle2: TSpeedButton;
        pnItemStyleSample: TPanel;
      lbItemSample: TLabel;
        edItemSample: TEdit;
      chbUseSameFont: TCheckBox;
    grpSample: TGroupBox;
      Sample: TabfFontNameComboBox;
    grpFontOptions: TGroupBox;
      chbFontOptions0: TCheckBox;
      chbFontOptions1: TCheckBox;
      chbFontOptions2: TCheckBox;
      chbFontOptions3: TCheckBox;
      chbFontOptions4: TCheckBox;
      chbFontOptions5: TCheckBox;
    grpStyle: TGroupBox;
      btnStyle0: TSpeedButton;
      btnStyle1: TSpeedButton;
      btnStyle2: TSpeedButton;
    grpProperties: TGroupBox;
      dlgFont: TFontDialog;
      lbColor: TLabel;
        cmbColor: TabfColorComboBox;
      lbDropDownCount: TLabel;
        edDropDownCount: TEdit;
        udDropDownCount: TUpDown;
      btnFont: TSpeedButton;
      chbEnabled: TCheckBox;
      chbEtched: TCheckBox;
      chbFlat: TCheckBox;
      chbFocusBorder: TCheckBox;
    grpDevice: TGroupBox;
      btnDevice0: TSpeedButton;
      btnDevice1: TSpeedButton;
      btnDevice2: TSpeedButton;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
  { Event handlers }
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  { Item style routines }
    procedure ItemStyleClick(Sender: TObject);
    procedure edItemSampleChange(Sender: TObject);
    procedure chbUseSameFontClick(Sender: TObject);
  { Combobox style routines }
    procedure StyleClick(Sender: TObject);
  { Font device routines }
    procedure DeviceClick(Sender: TObject);
  { Font options routines }
    procedure chbFontOptionsClick(Sender: TObject);
  { Other properties routines  }
    procedure cmbColorClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure edDropDownCountChange(Sender: TObject);
    procedure PropertiesCheckClick(Sender: TObject);
  private
    pnItemStyleSampleLeft,
    pnItemStyleSampleTop: Integer;
    procedure UpdateCharset;
  public
    procedure Run(ASample: TabfFontNameComboBox);
    procedure UpdateProperties;
  end;{TfrmDesignerFontNameComboBox = class(TForm)}


//==============================================================================
// Designing
//==============================================================================

procedure abfFontNameComboBoxDesign(const AComponent: TComponent);

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}

uses
  ShellApi, abfConsts, abfSysUtils, abfVclUtils;

//==============================================================================
// Designing
//==============================================================================

var
  frmDesignerFontNameComboBox: TfrmDesignerFontNameComboBox;

procedure abfFontNameComboBoxDesign(const AComponent: TComponent);
begin
  if not (Assigned(AComponent) and (AComponent is TabfFontNameComboBox)) then Exit;
  if not Assigned(frmDesignerFontNameComboBox) then
    frmDesignerFontNameComboBox := TfrmDesignerFontNameComboBox.Create(nil);
  frmDesignerFontNameComboBox.Run(TabfFontNameComboBox(AComponent));
end;


//==============================================================================
// TfrmColorComboBoxDesigner
//==============================================================================
// OneTouch Designer� form for the TabfFontNameComboBox component.
// Author: KARPOLAN
// Date: 04/01/2000

procedure TfrmDesignerFontNameComboBox.Run(ASample: TabfFontNameComboBox);
const
  SCaptionMask = 'Designing: %s: %s';

  procedure _AssignSample(Src, Dst: TabfFontNameComboBox);
  begin
    with Dst do
    begin
      DropDownCount  := Src.DropDownCount;
      Enabled        := Src.Enabled;
      Etched         := Src.Etched;
      Flat           := Src.Flat;
      FocusBorder    := Src.FocusBorder;
      Font           := Src.Font;
      FontDevice     := Src.FontDevice;
      FontItemSample := Src.FontItemSample;
      FontItemStyle  := Src.FontItemStyle;
      FontName       := Src.FontName;
      FontOptions    := Src.FontOptions;
      Hint           := Src.Hint;
      ShowHint       := Src.ShowHint;
      Style          := Src.Style;
      UseSameFont    := Src.UseSameFont;
      Color          := Src.Color;
    end;
  end;{Internal procedure _AssignSample}

begin
  if not Assigned(ASample) then Exit;
  _AssignSample(ASample, Sample);
  Caption := Format(SCaptionMask, [ASample.Name, ASample.ClassName]);
  UpdateProperties;
  abfFormFitToScreen(Self);
  if ShowModal <> mrOk then Exit;
  _AssignSample(Sample, ASample);
end;{procedure TfrmColorComboBoxDesigner.Run}

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.UpdateProperties;

   procedure _UpdateByTag(ATag: Integer; const ANamePart: string);
   var
     i: Integer;
   begin
     for i := 0 to ComponentCount - 1 do
       if (Components[i].Tag = ATag) and (Pos(ANamePart, Components[i].Name) > 0) then
       begin
         TSpeedButton(Components[i]).Down := True;;
         Exit;
       end;
   end;{Internal function _ComponentByTag}

begin
  with Sample do
  begin
    cmbColor.SelectedColor := Color;
    _UpdateByTag(Integer(FontDevice), 'btnDevice');
    _UpdateByTag(Integer(FontItemStyle), 'btnItemStyle');
    _UpdateByTag(Integer(Style), 'btnStyle');
    edItemSample.Text := FontItemSample;
    chbUseSameFont.Checked := UseSameFont;
    chbFontOptions0.Checked := foAnsiOnly       in FontOptions;
    chbFontOptions1.Checked := foTrueTypeOnly   in FontOptions;
    chbFontOptions2.Checked := foFixedPitchOnly in FontOptions;
    chbFontOptions3.Checked := foNoOEMFonts     in FontOptions;
    chbFontOptions4.Checked := foOEMFontsOnly   in FontOptions;
    chbFontOptions5.Checked := foScalableOnly   in FontOptions;
    chbEnabled.Checked := Enabled;
    chbEtched.Checked := Etched;
    chbFlat.Checked := Flat;
    chbFocusBorder.Checked := FocusBorder;
    udDropDownCount.Position := DropDownCount;
    udDropDownCount.Update;
  end;
  UpdateCharset;
end;{procedure TfrmDesignerFontNameComboBox.UpdateProperties}

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.UpdateCharset;
begin
{$IfDef D3}
  edItemSample.Font.Charset := Sample.Font.Charset;
  pnItemStyleSample.Font.Charset := Sample.Font.Charset;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.FormCreate(Sender: TObject);
begin
  pnItemStyleSampleLeft := pnItemStyleSample.Left;
  pnItemStyleSampleTop := pnItemStyleSample.Top;
{$IfDef D3}
  btnItemStyle0.Flat := cDesignerFlat;
  btnItemStyle1.Flat := cDesignerFlat;
  btnItemStyle2.Flat := cDesignerFlat;
  btnStyle0.Flat := cDesignerFlat;
  btnStyle1.Flat := cDesignerFlat;
  btnStyle2.Flat := cDesignerFlat;
  btnDevice0.Flat := cDesignerFlat;
  btnDevice1.Flat := cDesignerFlat;
  btnDevice2.Flat := cDesignerFlat;
//  cmbColor.Flat := cDesignerFlat;
//  cmbColor.Flat := cDesignerFlat;
//  cmbFont.Flat := cDesignerFlat;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.FormShow(Sender: TObject);
begin
  UpdateProperties;
  if Sample.CanFocus then ActiveControl := Sample;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.ItemStyleClick(Sender: TObject);
var
  ATag: Integer;
begin
  with pnItemStyleSample do
    SetBounds(pnItemStyleSampleLeft, pnItemStyleSampleTop, Width, Height);
  ATag := TControl(Sender).Tag;
  if ATag = 2 then
    with pnItemStyleSample do
      SetBounds(pnItemStyleSampleLeft + 1, pnItemStyleSampleTop + 1, Width, Height);
  Sample.FontItemStyle := TabfFontItemStyle(ATag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.edItemSampleChange(Sender: TObject);
begin
  Sample.FontItemSample := TEdit(Sender).Text;
  pnItemStyleSample.Caption := TEdit(Sender).Text;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.chbUseSameFontClick(Sender: TObject);
begin
  Sample.UseSameFont := TCheckBox(Sender).Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.StyleClick(Sender: TObject);
begin
  Sample.Style := TabfDrawComboBoxStyle(TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.DeviceClick(Sender: TObject);
begin
  Sample.FontDevice := TabfFontDevice(TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.chbFontOptionsClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    if Checked then
      case Tag of
        0: Sample.FontOptions := Sample.FontOptions + [foAnsiOnly];
        1: Sample.FontOptions := Sample.FontOptions + [foTrueTypeOnly];
        2: Sample.FontOptions := Sample.FontOptions + [foFixedPitchOnly];
        3: Sample.FontOptions := Sample.FontOptions + [foNoOEMFonts];
        4: Sample.FontOptions := Sample.FontOptions + [foOEMFontsOnly];
        5: Sample.FontOptions := Sample.FontOptions + [foScalableOnly];
      end
    else
      case Tag of
        0: Sample.FontOptions := Sample.FontOptions - [foAnsiOnly];
        1: Sample.FontOptions := Sample.FontOptions - [foTrueTypeOnly];
        2: Sample.FontOptions := Sample.FontOptions - [foFixedPitchOnly];
        3: Sample.FontOptions := Sample.FontOptions - [foNoOEMFonts];
        4: Sample.FontOptions := Sample.FontOptions - [foOEMFontsOnly];
        5: Sample.FontOptions := Sample.FontOptions - [foScalableOnly];
      end;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.cmbColorClick(Sender: TObject);
begin
  Sample.Color := cmbColor.SelectedColor;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.btnFontClick(Sender: TObject);
begin
  dlgFont.Font := Sample.Font;
  if not dlgFont.Execute then Exit;
  Sample.Font := dlgFont.Font;
  UpdateCharset;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.edDropDownCountChange(
  Sender: TObject);
begin
  Sample.DropDownCount := StrToIntDef(edDropDownCount.Text, udDropDownCount.Position);
  edDropDownCount.Text := IntToStr(Sample.DropDownCount);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerFontNameComboBox.PropertiesCheckClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    case Tag of
      1: Sample.Enabled := Checked;
      2: Sample.Etched := Checked;
      3: Sample.Flat := Checked;
      4: Sample.FocusBorder := Checked;
    end;
end;


{******************************************************************************}
initialization
{******************************************************************************}

{******************************************************************************}
finalization
{******************************************************************************}
  if Assigned(frmDesignerFontNameComboBox) then
    frmDesignerFontNameComboBox.Free;
  frmDesignerFontNameComboBox := nil;

end{unit abfDesignerFontNameComboBox}.
