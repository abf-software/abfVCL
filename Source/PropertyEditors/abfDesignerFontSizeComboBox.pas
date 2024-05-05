{*******************************************************************************

  ABF Visual Components Library, OneTouch(tm) Designer

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDesignerFontSizeComboBox;

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
// OneTouch Designer� form for the TabfFontSizeComboBox component.

  TfrmDesignerFontSizeComboBox = class(TForm)
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
      lbFontName: TLabel;
       cmbFontName: TabfFontNameComboBox;
    grpSample: TGroupBox;
      Sample: TabfFontSizeComboBox;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
  { Event handlers }
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  { Combobox style routines }
    procedure StyleClick(Sender: TObject);
  { Other properties routines  }
    procedure cmbColorClick(Sender: TObject);
    procedure edDropDownCountChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure PropertiesCheckClick(Sender: TObject);
    procedure cmbFontNameChange(Sender: TObject);
  public
    procedure Run(ASample: TabfFontSizeComboBox);
    procedure UpdateProperties;
  end;{TfrmDesignerFontSizeComboBox = class(TForm)}


//==============================================================================
// Designing
//==============================================================================

procedure abfFontSizeComboBoxDesign(const AComponent: TComponent);

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}

uses
  abfConsts, abfSysUtils, abfVclUtils;

//==============================================================================
// Designing
//==============================================================================

var
  frmDesignerFontSizeComboBox: TfrmDesignerFontSizeComboBox;

procedure abfFontSizeComboBoxDesign(const AComponent: TComponent);
begin
  if not (Assigned(AComponent) and (AComponent is TabfFontSizeComboBox)) then Exit;
  if not Assigned(frmDesignerFontSizeComboBox) then
    frmDesignerFontSizeComboBox := TfrmDesignerFontSizeComboBox.Create(nil);
  frmDesignerFontSizeComboBox.Run(TabfFontSizeComboBox(AComponent));
end;


//==============================================================================
// TfrmColorComboBoxDesigner
//==============================================================================
// OneTouch Designer� form for the TabfFontSizeComboBox component.
// Author: KARPOLAN
// Date: 04/01/2000

procedure TfrmDesignerFontSizeComboBox.Run(ASample: TabfFontSizeComboBox);
const
  SCaptionMask = 'Designing: %s: %s';

  procedure _AssignSample(Src, Dst: TabfFontSizeComboBox);
  begin
    with Dst do
    begin
      DropDownCount := Src.DropDownCount;
      Enabled       := Src.Enabled;
      Etched        := Src.Etched;
      Flat          := Src.Flat;
      FocusBorder   := Src.FocusBorder;
      Font          := Src.Font;
      FontName      := Src.FontName;
      Hint          := Src.Hint;
      ShowHint      := Src.ShowHint;
      Style         := Src.Style;
      Color         := Src.Color;
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

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.UpdateProperties;

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
    cmbFontName.FontName := FontName;
    _UpdateByTag(Integer(Style), 'btnStyle');
    chbEnabled.Checked := Enabled;
    chbEtched.Checked := Etched;
    chbFlat.Checked := Flat;
    chbFocusBorder.Checked := FocusBorder;
    udDropDownCount.Position := DropDownCount;
    udDropDownCount.Update;
  end;
end;{procedure TfrmDesignerFontSizeComboBox.UpdateProperties}

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.FormCreate(Sender: TObject);
begin
{$IfDef D3}
  btnStyle0.Flat := cDesignerFlat;
  btnStyle1.Flat := cDesignerFlat;
  btnStyle2.Flat := cDesignerFlat;
//  cmbColor.Flat := cDesignerFlat;
//  cmbColor.Flat := cDesignerFlat;
//  cmbFont.Flat := cDesignerFlat;
//  cmbFontName.Flat := cDesignerFlat;
{$EndIf D3}
end;

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.FormShow(Sender: TObject);
begin
  UpdateProperties;
  if Sample.CanFocus then ActiveControl := Sample;
end;

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.StyleClick(Sender: TObject);
begin
  Sample.Style := TComboBoxStyle(TControl(Sender).Tag);
end;

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.cmbFontNameChange(Sender: TObject);
begin
  Sample.FontName := cmbFontName.FontName;
end;

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.cmbColorClick(Sender: TObject);
begin
  Sample.Color := cmbColor.SelectedColor;
end;


{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.edDropDownCountChange(Sender: TObject);
begin
  Sample.DropDownCount := StrToIntDef(edDropDownCount.Text, udDropDownCount.Position);
  edDropDownCount.Text := IntToStr(Sample.DropDownCount);
end;

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.btnFontClick(Sender: TObject);
begin
  dlgFont.Font := Sample.Font;
  if not dlgFont.Execute then Exit;
  Sample.Font := dlgFont.Font;
end;

{--------------------------------------}

procedure TfrmDesignerFontSizeComboBox.PropertiesCheckClick(Sender: TObject);
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
  if Assigned(frmDesignerFontSizeComboBox) then frmDesignerFontSizeComboBox.Free;
  frmDesignerFontSizeComboBox := nil;

end{unit abfDesignerFontSizeComboBox}.
