{*******************************************************************************

  abfStatusBars DEMO.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfStatusBarsDemo;

uses
  Forms,
  abfStatusBarsDemoMain in 'abfStatusBarsDemoMain.pas' {frmStatusBarsDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfStatusBars Demo';
  Application.CreateForm(TfrmStatusBarsDemoMain, frmStatusBarsDemoMain);
  Application.Run;
end.
