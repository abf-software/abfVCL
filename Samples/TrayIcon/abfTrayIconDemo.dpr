{*******************************************************************************

  abfTrayIcon component DEMO.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfTrayIconDemo;

uses
  Forms,
  abfTrayIconDemoMain in 'abfTrayIconDemoMain.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfTrayIcon component DEMO';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
