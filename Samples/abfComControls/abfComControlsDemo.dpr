{*******************************************************************************

  abfComControls Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfComControlsDemo;

uses
  Forms,
  abfComControlsDemoMain in 'abfComControlsDemoMain.pas' {frmComControlsDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfComControls Demo';
  Application.CreateForm(TfrmComControlsDemoMain, frmComControlsDemoMain);
  Application.Run;
end.
