{*******************************************************************************

  abfAPM Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL
  
*******************************************************************************}
program abfAPMDemo;

uses
  Forms,
  abfAPMDemoMain in 'abfAPMDemoMain.pas' {frmAPMDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfAPM Demo';
  Application.CreateForm(TfrmAPMDemoMain, frmAPMDemoMain);
  Application.Run;
end.

