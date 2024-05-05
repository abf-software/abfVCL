{*******************************************************************************

  ABF Visual Components Library, OneTouch(tm) Designer

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020-2024 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDesignerColorComboBox;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
{$IfDef D3}
  ExtDlgs,
{$EndIf D3}
  abfComboBoxes, abfDesignerComboBox, abfControls;

type

//==============================================================================
// TfrmColorComboBoxDesigner
//==============================================================================
// OneTouch Designer� form for the TabfColorComboBox component.

  TfrmDesignerColorComboBox = class(TForm)
    grpNameStyle: TGroupBox;
      btnNameStyle0: TSpeedButton;
      btnNameStyle1: TSpeedButton;
      btnNameStyle2: TSpeedButton;
      btnNameStyle3: TSpeedButton;
      btnNameStyle4: TSpeedButton;
      btnNameStyle5: TSpeedButton;
      btnNameStyle6: TSpeedButton;
      btnNameStyle7: TSpeedButton;
    grpColorSample: TGroupBox;
      imgSampleWidthDemo: TImage;
      lbSampleWidth: TLabel;
        edSampleWidth: TEdit;
        udSampleWidth: TUpDown;
      lbSampleGlyphNone: TLabel;
        imgSampleGlyph: TImage;
      btnLoadSampleGlyph: TSpeedButton;
      btnClearSampleGlyph: TSpeedButton;
    grpColorsSet: TGroupBox;
      rbtColorsSet0: TRadioButton;
      rbtColorsSet1: TRadioButton;
      rbtColorsSet2: TRadioButton;
      rbtColorsSet3: TRadioButton;
      rbtColorsSet4: TRadioButton;
      rbtColorsSet5: TRadioButton;
      rbtColorsSet6: TRadioButton;
        bvColorsSet: TBevel;
      chbColorsSetNone: TCheckBox;
      chbColorsSetDefault: TCheckBox;
    grpSample: TGroupBox;
    grpItemStyle: TGroupBox;
      btnItemStyle0: TSpeedButton;
      btnItemStyle1: TSpeedButton;
      btnItemStyle2: TSpeedButton;
    grpStyle: TGroupBox;
      btnStyle0: TSpeedButton;
      btnStyle1: TSpeedButton;
      btnStyle2: TSpeedButton;
    grpProperties: TGroupBox;
      dlgFont: TFontDialog;
      lbDropDownCount: TLabel;
        edDropDownCount: TEdit;
        udDropDownCount: TUpDown;
      btnFont: TSpeedButton;
      chbEnabled: TCheckBox;
      chbFlat: TCheckBox;
      chbFocusBorder: TCheckBox;
      chbEtched: TCheckBox;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    chbColorsSetCustom: TCheckBox;
    Sample: TabfColorComboBox;
    cmbColor: TabfColorComboBox;
  { Event handlers }
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  { Item style routines }
    procedure ItemStyleClick(Sender: TObject);
  { Sample glyph routines }
    procedure NameStyleClick(Sender: TObject);
  { Color Set routines }
    procedure rbtColorSetClick(Sender: TObject);
    procedure chbColorsSetNoneClick(Sender: TObject);
    procedure chbColorsSetDefaultClick(Sender: TObject);
    procedure chbColorsSetCustomClick(Sender: TObject);
  { Color sample routines }
    procedure grpColorSampleClick(Sender: TObject);
    procedure edSampleWidthChange(Sender: TObject);
    procedure btnLoadSampleGlyphClick(Sender: TObject);
    procedure btnClearSampleGlyphClick(Sender: TObject);
  { Combobox style routines }
    procedure StyleClick(Sender: TObject);
  { Other properties routines  }
    procedure cmbColorClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure edDropDownCountChange(Sender: TObject);
    procedure PropertiesCheckClick(Sender: TObject);
  protected
{$IfDef D3}
    dlgLoadSampleGlyph: TOpenPictureDialog;
{$Else D3}
    dlgLoadSampleGlyph: TOpenDialog;
{$EndIf D3}
  public
    procedure Run(ASample: TabfColorComboBox);
    procedure UpdateProperties;
  end;{TfrmColorComboBoxDesigner = class(TForm)}


//==============================================================================
// Designing
//==============================================================================

procedure abfColorComboBoxDesign(const AComponent: TComponent);

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
  frmDesignerColorComboBox: TfrmDesignerColorComboBox;

procedure abfColorComboBoxDesign(const AComponent: TComponent);
begin
  if not (Assigned(AComponent) and (AComponent is TabfColorComboBox)) then Exit;
  if not Assigned(frmDesignerColorComboBox) then
    frmDesignerColorComboBox := TfrmDesignerColorComboBox.Create(nil);
  frmDesignerColorComboBox.Run(TabfColorComboBox(AComponent));
end;


//==============================================================================
// TfrmColorComboBoxDesigner
//==============================================================================
// OneTouch Designer� form for the TabfColorComboBox component.
// Author: KARPOLAN
// Date: 04/01/2000

procedure TfrmDesignerColorComboBox.Run(ASample: TabfColorComboBox);
const
  SCaptionMask = 'Designing: %s: %s';

  procedure _AssignSample(Src, Dst: TabfColorComboBox);
  begin
    with Dst do
    begin
      Color            := Src.Color;
      ColorItemStyle   := Src.ColorItemStyle;
      ColorNames       := Src.ColorNames;
      ColorNameStyle   := Src.ColorNameStyle;
      ColorSample      := Src.ColorSample;
      ColorSampleWidth := Src.ColorSampleWidth;
      ColorsSet        := Src.ColorsSet;
      DropDownCount    := Src.DropDownCount;
      Enabled          := Src.Enabled;
      Etched           := Src.Etched;
      Flat             := Src.Flat;
      FocusBorder      := Src.FocusBorder;
      Font             := Src.Font;
      Hint             := Src.Hint;
      SelectedColor    := Src.SelectedColor;
      ShowHint         := Src.ShowHint;
      Style            := Src.Style;
      UseColorCustom   := Src.UseColorCustom;
      UseColorDefault  := Src.UseColorDefault;
      UseColorNone     := Src.UseColorNone;
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

procedure TfrmDesignerColorComboBox.UpdateProperties;

   procedure _UpdateByTag(ATag: Integer; const ANamePart: string);
   var
     i: Integer;
   begin
     for i := 0 to ComponentCount - 1 do
       if (Components[i].Tag = ATag) and (Pos(ANamePart, Components[i].Name) > 0) then
       begin
         if (Components[i] is TRadioButton) then
           TRadioButton(Components[i]).Checked := True
         else
         if (Components[i] is TSpeedButton) then
           TSpeedButton(Components[i]).Down := True;
         Exit;
       end;
   end;{Internal function _ComponentByTag}

begin
  with Sample do
  begin
    cmbColor.SelectedColor := Color;
    _UpdateByTag(Integer(ColorItemStyle), 'btnItemStyle');
    _UpdateByTag(Integer(ColorNameStyle), 'btnNameStyle');
    _UpdateByTag(Integer(ColorsSet), 'rbtColorsSet');
    _UpdateByTag(Integer(Style), 'btnStyle');
    imgSampleGlyph.Picture.Assign(ColorSample);
      lbSampleGlyphNone.Visible := imgSampleGlyph.Picture.Graphic.Empty;
    chbColorsSetCustom .Checked := UseColorCustom;
    chbColorsSetDefault.Checked := UseColorDefault;
    chbColorsSetNone   .Checked := UseColorNone;
    chbEnabled.Checked := Enabled;
    chbEtched.Checked := Etched;
    chbFlat.Checked := Flat;
    chbFocusBorder.Checked := FocusBorder;
    udDropDownCount.Position := DropDownCount;
    udDropDownCount.Update;
  end;
end;{procedure TfrmDesignerColorComboBox.UpdateProperties}

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.FormCreate(Sender: TObject);
begin
{$IfNDef D3}
  dlgLoadSampleGlyph := TOpenDialog.Create(Self);
{$Else D3}
  dlgLoadSampleGlyph := TOpenPictureDialog.Create(Self);
  btnStyle0.Flat := cDesignerFlat;
  btnStyle1.Flat := cDesignerFlat;
  btnStyle2.Flat := cDesignerFlat;
  btnItemStyle0.Flat := cDesignerFlat;
  btnItemStyle1.Flat := cDesignerFlat;
  btnItemStyle2.Flat := cDesignerFlat;
  btnNameStyle0.Flat := cDesignerFlat;
  btnNameStyle1.Flat := cDesignerFlat;
  btnNameStyle2.Flat := cDesignerFlat;
  btnNameStyle3.Flat := cDesignerFlat;
  btnNameStyle4.Flat := cDesignerFlat;
  btnNameStyle5.Flat := cDesignerFlat;
  btnNameStyle6.Flat := cDesignerFlat;
  btnNameStyle7.Flat := cDesignerFlat;
//  btnLoadSampleGlyph.Flat := True;
//  btnClearSampleGlyph.Flat := True;
//  cmbColor.Flat := cDesignerFlat;
//  cmbColor.Flat := cDesignerFlat;
//  cmbFont.Flat := cDesignerFlat;
  imgSampleWidthDemo.Transparent := True;
{$EndIf D3}
  with dlgLoadSampleGlyph do
  begin
    Title := 'Load a sample glyph';
    Filter := 'Bitmaps (*.bmp)|*.bmp';
    DefaultExt := 'bmp';
  end;
  UpdateProperties;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.FormShow(Sender: TObject);
begin
  UpdateProperties;
  if Sample.CanFocus then ActiveControl := Sample;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.ItemStyleClick(Sender: TObject);
begin
  Sample.ColorItemStyle := TabfColorItemStyle(TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.NameStyleClick(Sender: TObject);
begin
  Sample.ColorNameStyle := TabfColorNameStyle(TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.rbtColorSetClick(Sender: TObject);
begin
  Sample.ColorsSet := TabfColorsSet(TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.chbColorsSetNoneClick(Sender: TObject);
begin
  Sample.UseColorNone := chbColorsSetNone.Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.chbColorsSetDefaultClick(Sender: TObject);
begin
  Sample.UseColorDefault := chbColorsSetDefault.Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.chbColorsSetCustomClick(Sender: TObject);
begin
  Sample.UseColorCustom := chbColorsSetCustom.Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.grpColorSampleClick(Sender: TObject);
begin
  ActiveControl := edSampleWidth;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.edSampleWidthChange(Sender: TObject);
begin
  Sample.ColorSampleWidth := StrToIntDef(edSampleWidth.Text, udSampleWidth.Position);
  edSampleWidth.Text := IntToStr(Sample.ColorSampleWidth);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.btnLoadSampleGlyphClick(Sender: TObject);
begin
  if not dlgLoadSampleGlyph.Execute then Exit;
  Sample.ColorSample := nil;
  try
    Sample.ColorSample.LoadFromFile(dlgLoadSampleGlyph.FileName);
  except
  end;
  imgSampleGlyph.Picture.Assign(Sample.ColorSample);
  imgSampleGlyph.Repaint;
  lbSampleGlyphNone.Visible := imgSampleGlyph.Picture.Graphic.Empty;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.btnClearSampleGlyphClick(Sender: TObject);
begin
  Sample.ColorSample := nil;
  imgSampleGlyph.Picture.Assign(Sample.ColorSample);
  imgSampleGlyph.Repaint;
  lbSampleGlyphNone.Visible := True;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.StyleClick(Sender: TObject);
begin
  Sample.Style := TabfDrawComboBoxStyle(TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.cmbColorClick(Sender: TObject);
begin
  Sample.Color := cmbColor.SelectedColor;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.btnFontClick(Sender: TObject);
begin
  dlgFont.Font := Sample.Font;
  if not dlgFont.Execute then Exit;
  Sample.Font := dlgFont.Font;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.edDropDownCountChange(Sender: TObject);
begin
  Sample.DropDownCount := StrToIntDef(edDropDownCount.Text, udDropDownCount.Position);
  edDropDownCount.Text := IntToStr(Sample.DropDownCount);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerColorComboBox.PropertiesCheckClick(Sender: TObject);
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
  if Assigned(frmDesignerColorComboBox) then frmDesignerColorComboBox.Free;
  frmDesignerColorComboBox := nil;

end{unit abfDesignerColorComboBox}.
