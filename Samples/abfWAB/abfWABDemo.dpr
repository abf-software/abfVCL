{*******************************************************************************

  abfWAB Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfWABDemo;

uses
  Forms,
  abfWABDemoMain in 'abfWABDemoMain.pas' {frmWabDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfWAB Demo';
  Application.CreateForm(TfrmWabDemoMain, frmWabDemoMain);
  Application.Run;
end.
