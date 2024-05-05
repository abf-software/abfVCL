{*******************************************************************************

  abfDialogs Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfDialogsDemo;

uses
  Forms,
  abfDialogsDemoSecond in 'abfDialogsDemoSecond.pas' {frmDialogsDemoSecond},
  abfDialogsDemoMain in 'abfDialogsDemoMain.pas' {frmDialogsDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfDialogs Demo';
  Application.CreateForm(TfrmDialogsDemoMain, frmDialogsDemoMain);
  Application.CreateForm(TfrmDialogsDemoSecond, frmDialogsDemoSecond);
  with frmDialogsDemoMain do
  begin
    BringToFront;
    SetBounds(Left + 50, Top + 50, Width, Height);
  end;
  Application.Run;
end.
