{*******************************************************************************

  abfMenus Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfMenusDemo;

uses
  Forms,
  abfMenusDemoMain in 'abfMenusDemoMain.pas' {frmMenusDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfMenus Demo';
  Application.CreateForm(TfrmMenusDemoMain, frmMenusDemoMain);
  Application.Run;
end.
