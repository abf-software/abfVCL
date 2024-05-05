{*******************************************************************************

  abfLabels Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfLabelsDemo;

uses
  Forms,
  abfLabelsDemoMain in 'abfLabelsDemoMain.pas' {frmLabelsDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfLabels Demo';
  Application.CreateForm(TfrmLabelsDemoMain, frmLabelsDemoMain);
  Application.Run;
end.
