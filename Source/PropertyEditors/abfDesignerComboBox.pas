{*******************************************************************************

  ABF Visual Components Library, OneTouch(tm) Designer

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDesignerComboBox;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
  abfControls, abfComboBoxes;

const
  cDesignerFlat = True;

type

//==============================================================================
// TfrmColorComboBoxDesigner
//==============================================================================
// Default OneTouch Designer� form for all descendants of the
// TabfCustomComboBox control.

  TfrmDesignerComboBox = class(TForm)
    grpStyle: TGroupBox;
      btnStyle0: TSpeedButton;
      btnStyle1: TSpeedButton;
      btnStyle2: TSpeedButton;
      btnStyle3: TSpeedButton;
      btnStyle4: TSpeedButton;
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
    grpSample: TGroupBox;
      Sample: TabfComboBox;
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
  public
    procedure Run(ASample: TabfCustomComboBox);
    procedure UpdateProperties;
  end;{TfrmDesignerComboBox = class(TForm)}


//==============================================================================
// Designing
//==============================================================================

procedure abfComboBoxDesign(const AComponent: TComponent);

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
  frmDesignerComboBox: TfrmDesignerComboBox;

procedure abfComboBoxDesign(const AComponent: TComponent);
begin
  if not (Assigned(AComponent) and (AComponent is TabfCustomComboBox)) then Exit;
  if not Assigned(frmDesignerComboBox) then
    frmDesignerComboBox := TfrmDesignerComboBox.Create(nil);
  frmDesignerComboBox.Run(TabfCustomComboBox(AComponent));
end;


//==============================================================================
// TfrmColorComboBoxDesigner
//==============================================================================
// Default OneTouch Designer� form for all descendants of the
// TabfCustomComboBox control.
// Author: KARPOLAN
// Date: 09/01/2000

type
  TabfHackComboBox = class(TabfCustomComboBox);

procedure TfrmDesignerComboBox.Run(ASample: TabfCustomComboBox);
const
  SCaptionMask = 'Designing: %s: %s';

  procedure _AssignSample(Src, Dst: TabfHackComboBox);
  begin
    with Dst do
    begin
      DropDownCount := Src.DropDownCount;
      Enabled       := Src.Enabled;
      Etched        := Src.Etched;
      Flat          := Src.Flat;
      FocusBorder   := Src.FocusBorder;
      Font          := Src.Font;
      Hint          := Src.Hint;
      ShowHint      := Src.ShowHint;
      Style         := Src.Style;
      Color         := Src.Color;
    end;
  end;{Internal procedure _AssignSample}

begin
  if not Assigned(ASample) then Exit;
  _AssignSample(TabfHackComboBox(ASample), TabfHackComboBox(Sample));
  Caption := Format(SCaptionMask, [ASample.Name, ASample.ClassName]);
  UpdateProperties;
  abfFormFitToScreen(Self);
  if ShowModal <> mrOk then Exit;
  _AssignSample(TabfHackComboBox(Sample), TabfHackComboBox(ASample));
end;{procedure TfrmColorComboBoxDesigner.Run}

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.UpdateProperties;

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
    _UpdateByTag(Integer(Style), 'btnStyle');
    chbEnabled.Checked := Enabled;
    chbEtched.Checked := Etched;
    chbFlat.Checked := Flat;
    chbFocusBorder.Checked := FocusBorder;
    udDropDownCount.Position := DropDownCount;
    udDropDownCount.Update;
  end;
end;{procedure TfrmDesignerComboBox.UpdateProperties}

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.FormCreate(Sender: TObject);
begin
{$IfDef D3}
  btnStyle0.Flat := cDesignerFlat;
  btnStyle1.Flat := cDesignerFlat;
  btnStyle2.Flat := cDesignerFlat;
  btnStyle3.Flat := cDesignerFlat;
  btnStyle4.Flat := cDesignerFlat;
//  cmbColor.Flat := cDesignerFlat;
{$EndIf D3}
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.FormShow(Sender: TObject);
begin
  UpdateProperties;
  if Sample.CanFocus then ActiveControl := Sample;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.StyleClick(Sender: TObject);
begin
  Sample.Style := TComboBoxStyle(TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.cmbColorClick(Sender: TObject);
begin
  Sample.Color := cmbColor.SelectedColor;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.edDropDownCountChange(Sender: TObject);
begin
  Sample.DropDownCount := StrToIntDef(edDropDownCount.Text, udDropDownCount.Position);
  edDropDownCount.Text := IntToStr(Sample.DropDownCount);
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.btnFontClick(Sender: TObject);
begin
  dlgFont.Font := Sample.Font;
  if not dlgFont.Execute then Exit;
  Sample.Font := dlgFont.Font;
end;

//------------------------------------------------------------------------------

procedure TfrmDesignerComboBox.PropertiesCheckClick(Sender: TObject);
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
  if Assigned(frmDesignerComboBox) then frmDesignerComboBox.Free;
  frmDesignerComboBox := nil;

end{unit abfDesignerComboBox}.
