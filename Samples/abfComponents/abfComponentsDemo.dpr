{*******************************************************************************

  abfComponents Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfComponentsDemo;

uses
  Forms,
  abfComponentsDemoMain in 'abfComponentsDemoMain.pas' {frmComponentsDemoMain},
  abfComponentsDemoSecond in 'abfComponentsDemoSecond.pas' {frmComponentsDemoSecond};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfComponents Demo';
  Application.CreateForm(TfrmComponentsDemoMain, frmComponentsDemoMain);
  Application.CreateForm(TfrmComponentsDemoSecond, frmComponentsDemoSecond);
  with frmComponentsDemoMain do
  begin
    BringToFront;
    SetBounds(Left + 50, Top + 50, Width, Height);
  end;
  Application.Run;
end.


