{*******************************************************************************

  abfEdits Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfEditsDemo;

uses
  Forms,
  abfEditsDemoMain in 'abfEditsDemoMain.pas' {frmEditsDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfEdits Demo';
  Application.CreateForm(TfrmEditsDemoMain, frmEditsDemoMain);
  Application.Run;
end.
