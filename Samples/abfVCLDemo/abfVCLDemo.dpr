{*******************************************************************************

  ABF VCL Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfVCLdemo;

{$I abf.inc}

uses
  Forms,
  abfComponentsDemoMain in '..\abfComponents\abfComponentsDemoMain.pas',
  abfComponentsDemoSecond in '..\abfComponents\abfComponentsDemoSecond.pas',
  abfControlsDemoMain in '..\abfControls\abfControlsDemoMain.pas',
  abfComboBoxesDemoMain in '..\abfComboBoxes\abfComboBoxesDemoMain.pas',
  abfComControlsDemoMain in '..\abfComControls\abfComControlsDemoMain.pas',
  abfDialogsDemoMain in '..\abfDialogs\abfDialogsDemoMain.pas',
  abfDialogsDemoSecond in '..\abfDialogs\abfDialogsDemoSecond.pas',
  abfDialogsDemoThird in '..\abfDialogs\abfDialogsDemoThird.pas',
  abfEditsDemoMain in '..\abfEdits\abfEditsDemoMain.pas',
  abfMenusDemoMain in '..\abfMenus\abfMenusDemoMain.pas',
  abfLabelsDemoMain in '..\abfLabels\abfLabelsDemoMain.pas',
  abfStatusBarsDemoMain in '..\abfStatusBars\abfStatusBarsDemoMain.pas',
{$IfDef D3}
  abfAPMDemoMain in '..\abfAPM\abfAPMDemoMain.pas',
  abfWabDemoMain in '..\abfWab\abfWabDemoMain.pas',
  abfEffectsDemoMain in '..\abfEffects\abfEffectsDemoMain.pas',
{$EndIf D3}  
  abfVCLdemoMain in 'abfVCLdemoMain.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ABF VCL Demo';
  Application.CreateForm(TfrmABFVCLdemoMain, frmABFVCLdemoMain);
  Application.Run;
end.
