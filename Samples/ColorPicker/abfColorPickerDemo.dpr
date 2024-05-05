{*******************************************************************************

  abfColorPicker component DEMO.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfColorPickerDemo;

uses
  Forms,
  abfColorPickerDemoMain in 'abfColorPickerDemoMain.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfColorPicker component DEMO';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
