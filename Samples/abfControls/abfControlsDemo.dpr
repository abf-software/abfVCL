{*******************************************************************************

  abfControls Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfControlsDemo;

uses
  Forms,
  abfControlsDemoMain in 'abfControlsDemoMain.pas' {frmControlsDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfControls Demo';
  Application.CreateForm(TfrmControlsDemoMain, frmControlsDemoMain);
  Application.Run;
end.
