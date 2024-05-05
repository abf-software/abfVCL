{*******************************************************************************

  abfColorPicker component DEMO. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL
  
*******************************************************************************}
unit abfColorPickerDemoMain;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, abfComponents;

type

{******************************************************************************}
{ TfrmMain }

  TfrmMain = class(TForm)
    abfColorPicker: TabfColorPicker;
    pnColor: TPanel;
    lb1: TLabel;
    btnAbout: TBitBtn;
    procedure ColorPickerIdle(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfAboutComponent;

{******************************************************************************}
{ TfrmMain }

type
  TColorQuad = record
    Red, Green, Blue, Alpha: Byte;
  end;

procedure TfrmMain.ColorPickerIdle(Sender: TObject);
var
  AColor: TColor;
begin
  AColor := abfColorPicker.Color;
  pnColor.Color := AColor;
  pnColor.Caption := ColorToString(AColor);
  with TColorQuad(AColor) do
    if (Red > 192) or (Green > 192) or (Blue > 192) then AColor := clBlack
    else AColor := clWhite;
  pnColor.Font.Color := AColor
end;

{--------------------------------------}

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  abfComponentAbout(abfColorPicker);
end;

{--------------------------------------}

end{unit abfColorPickerDemoMain}.
