{*******************************************************************************

  abfEffects Demo.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
program abfEffectsDemo;

uses
  Forms,
  abfEffectsDemoMain in 'abfEffectsDemoMain.pas' {frmEffectsDemoMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'abfEffects Demo';
  Application.CreateForm(TfrmEffectsDemoMain, frmEffectsDemoMain);
  Application.Run;
end.
