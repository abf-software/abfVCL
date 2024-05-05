{*******************************************************************************

  abfComboBoxes Demo.
  
  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfComboBoxesDemo;

uses
  Forms,
  abfComboBoxesDemoMain in 'abfComboBoxesDemoMain.pas' {frmComboBoxesDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfComboBoxes Demo';
  Application.CreateForm(TfrmComboBoxesDemoMain, frmComboBoxesDemoMain);
  Application.Run;
end.
