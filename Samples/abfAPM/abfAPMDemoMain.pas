{*******************************************************************************

  abfAPM Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfAPMDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, SysUtils, Classes, Forms, StdCtrls, Menus, Graphics, Controls,
  ExtCtrls, abfComponents, abfAPM;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmMain
//==============================================================================

  TfrmAPMDemoMain = class(TForm)
    grpManager: TGroupBox;
      imgManager: TImage;
      lbManagerInfo: TLabel;
      chbManager: TCheckBox;
      pnManager: TPanel;
        lbACLineStatus: TLabel;
          lbACLineStatusResult: TLabel;
        lbBatteryFlag: TLabel;
          lbBatteryFlagResult: TLabel;
        lbBatteryLifePercent: TLabel;
          lbBatteryLifePercentResult: TLabel;
        lbBatteryFullLifeTime: TLabel;
          lbBatteryFullLifeTimeResult: TLabel;
        lbBatteryLifeTime: TLabel;
          lbBatteryLifeTimeResult: TLabel;
        grpManagerBlockingModes: TGroupBox;
          chbSystemActivity: TCheckBox;
          chbDisplayActivity: TCheckBox;
          chbUserPresent: TCheckBox;
    grpScheduler: TGroupBox;
      lbSchedulerInfo: TLabel;
      imgScheduler: TImage;
      chbScheduler: TCheckBox;
      pnScheduler: TPanel;
        chbSuspendEnabled: TCheckBox;
        grpSuspend: TGroupBox;
          lbSuspendDate: TLabel;
            edSuspendDate: TEdit;
          lbSuspendTime: TLabel;
            edSuspendTime: TEdit;
          cmbSuspendMode: TComboBox;
          chbForced: TCheckBox;
        chbWakeUpEnabled: TCheckBox;
        grpWakeUp: TGroupBox;
          lbWakeUpDate: TLabel;
            edWakeUpDate: TEdit;
          lbWakeUpTime: TLabel;
            edWakeUpTime: TEdit;
          cmbWakeUpMode: TComboBox;
  { Non-visual }
    abfAPMManager: TabfAPMManager;
    abfAPMScheduler: TabfAPMScheduler;
    TrayIcon: TabfTrayIcon;
    ilstMain: TImageList;
    pmMain: TPopupMenu;
      miRestore: TMenuItem;
        mi1: TMenuItem;
      miAbout: TMenuItem;
        mi2: TMenuItem;
      miEnabledManager: TMenuItem;
      miEnabledScheduler: TMenuItem;
        mi3: TMenuItem;
      miClose: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  { TabfAPMManager }
    procedure EnabledManagerClick(Sender: TObject);
    procedure chbBlockingModeClick(Sender: TObject);
    procedure abfAPMManagerPowerStatusChange(Sender: TObject);
  { TabfAPMScheduler }
    procedure EnabledSchedulerClick(Sender: TObject);
    procedure chbGroupEnabledClick(Sender: TObject);
    procedure cmbModeChange(Sender: TObject);
    procedure edDateTimeChange(Sender: TObject);
    procedure pmMainPopup(Sender: TObject);
    procedure miRestoreClick(Sender: TObject);
  private
    procedure UpdatePowerStatus;
    procedure RefreshManagerStatus;
    procedure RefreshSchedulerStatus;
  end;

var
  frmAPMDemoMain: TfrmAPMDemoMain;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmAPMDemoMain) then
    Application.CreateForm(TfrmAPMDemoMain, frmAPMDemoMain);
  frmAPMDemoMain.Show;
  frmAPMDemoMain.BringToFront;
end;


//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

procedure TfrmAPMDemoMain.UpdatePowerStatus;
var
  Hour, Min, Sec: Integer;
begin
  case abfAPMManager.ACLineStatus of
  lsOffline:
    begin
      lbACLineStatusResult.Caption := 'Offline';
      lbACLineStatusResult.Font.Color := clRed;
    end;
  lsOnline:
    begin
      lbACLineStatusResult.Caption := 'Online';
      lbACLineStatusResult.Font.Color := clGreen;
    end;
  lsBackupPower:
    begin
      lbACLineStatusResult.Caption := 'Backup power';
      lbACLineStatusResult.Font.Color := clBlue;
    end
  else
    begin
      lbACLineStatusResult.Caption := 'Unknown';
      lbACLineStatusResult.Font.Color := clBlack;
    end;
  end;
  case abfAPMManager.BatteryFlag of
  bfHigh:
    begin
      lbBatteryFlagResult.Caption := 'High';
      lbBatteryFlagResult.Font.Color := clGreen;
    end;
  bfLow:
    begin
      lbBatteryFlagResult.Caption := 'Low';
      lbBatteryFlagResult.Font.Color := clYellow;
    end;
  bfCritical:
    begin
      lbBatteryFlagResult.Caption := 'Critical';
      lbBatteryFlagResult.Font.Color := clRed;
    end;
  bfCharging:
    begin
      lbBatteryFlagResult.Caption := 'Charging';
      lbBatteryFlagResult.Font.Color := clBlue;
    end;
  bfNoBattery:
    begin
      lbBatteryFlagResult.Caption := 'No battery';
      lbBatteryFlagResult.Font.Color := clBlack;
    end
  else
    begin
      lbBatteryFlagResult.Caption := 'Unknown';
      lbACLineStatusResult.Font.Color := clBlack;
    end;
  end;
  if abfAPMManager.BatteryFullLifeTime > -1 then
  begin
    Hour := abfAPMManager.BatteryFullLifeTime div 3600;
    Min := (abfAPMManager.BatteryFullLifeTime - Hour * 3600) div 60;
    Sec := abfAPMManager.BatteryFullLifeTime - Hour * 3600 - Min * 60;
    lbBatteryFullLifeTimeResult.Caption := Format('%2.2dh:%2.2dm:%2.2ds',
                                                  [Hour, Min, Sec]);
    lbBatteryFullLifeTimeResult.Font.Color := clGreen;
  end else
  begin
    lbBatteryFullLifeTimeResult.Caption := 'Unknown';
    lbBatteryFullLifeTimeResult.Font.Color := clBlack;
  end;
  if abfAPMManager.BatteryLifePercent in [0..100] then
  begin
    lbBatteryLifePercentResult.Caption := Format('%d%', [abfAPMManager.BatteryLifePercent]);
    lbBatteryLifePercentResult.Font.Color := lbBatteryFlagResult.Font.Color;
  end else
  begin
    lbBatteryLifePercentResult.Caption := 'Unknown';
    lbBatteryLifePercentResult.Font.Color := clBlack;
  end;
  if abfAPMManager.BatteryLifeTime > -1 then
  begin
    Hour := abfAPMManager.BatteryLifeTime div 3600;
    Min := (abfAPMManager.BatteryLifeTime - Hour * 3600) div 60;
    Sec := abfAPMManager.BatteryLifeTime - Hour * 3600 - Min * 60;
    lbBatteryLifeTimeResult.Caption := Format('%2.2dh:%2.2dm:%2.2ds',
                                                  [Hour, Min, Sec]);
    lbBatteryLifeTimeResult.Font.Color := lbBatteryFlagResult.Font.Color;
  end else
  begin
    lbBatteryLifeTimeResult.Caption := 'Unknown';
    lbBatteryLifeTimeResult.Font.Color := clBlack;
  end;
end{procedure TfrmAPMDemoMain.UpdatePowerStatus};

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.RefreshManagerStatus;
var
  i: Integer;
begin
  chbManager.OnClick := nil;
  try
    chbManager.Checked := abfAPMManager.Enabled;
  finally
    chbManager.OnClick := EnabledManagerClick;
  end;
  for i:= 0 to pnManager.ControlCount - 1 do
    pnManager.Controls[i].Enabled := chbManager.Checked;

  UpdatePowerStatus;

  for i:= 0 to grpManagerBlockingModes.ControlCount - 1 do
    grpManagerBlockingModes.Controls[i].Enabled := chbManager.Checked;

  chbSystemActivity.OnClick := nil;
  try
    chbSystemActivity .Checked := amSystemActivity  in abfAPMManager.BlockingModes;
  finally
    chbSystemActivity.OnClick := chbBlockingModeClick;
  end;

  chbDisplayActivity.OnClick := nil;
  try
    chbDisplayActivity.Checked := amDisplayActivity in abfAPMManager.BlockingModes;
  finally
    chbDisplayActivity.OnClick := chbBlockingModeClick;
  end;

  chbUserPresent.OnClick := nil;
  try
    chbUserPresent.Checked := amUserPresent in abfAPMManager.BlockingModes;
  finally
    chbUserPresent.OnClick := chbBlockingModeClick;
  end;

end;{procedure TfrmMain.RefreshabfAPMManagerStatus}

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.RefreshSchedulerStatus;
var
  i: Integer;
begin
  chbScheduler.OnClick := nil;
  try
    chbScheduler.Checked := abfAPMScheduler.Enabled;
  finally
    chbScheduler.OnClick := EnabledSchedulerClick;
  end;

  chbSuspendEnabled.OnClick := nil;
  try
    chbSuspendEnabled.Checked := abfAPMScheduler.SuspendOptions.Enabled;
  finally
    chbSuspendEnabled.OnClick := chbGroupEnabledClick;
  end;

  chbWakeUpEnabled.OnClick := nil;
  try
    chbWakeUpEnabled.Checked := abfAPMScheduler.WakeUpOptions.Enabled;
  finally
    chbWakeUpEnabled.OnClick := chbGroupEnabledClick;
  end;

  for i:= 0 to pnScheduler.ControlCount - 1 do
    pnScheduler.Controls[i].Enabled := chbScheduler.Checked;
  if abfAPMScheduler.SuspendOptions.Enabled then
    for i:= 0 to grpSuspend.ControlCount - 1 do
      grpSuspend.Controls[i].Enabled := chbScheduler.Checked;

  edSuspendDate.OnChange := nil;
  try
    edSuspendDate.Text := DateToStr(abfAPMScheduler.SuspendOptions.Date);
  finally
    edSuspendDate.OnChange := edDateTimeChange;
  end;

  edSuspendTime.OnChange := nil;
  try
    edSuspendTime.Text := TimeToStr(abfAPMScheduler.SuspendOptions.Time);
  finally
    edSuspendTime.OnChange := edDateTimeChange;
  end;

  cmbSuspendMode.OnChange := nil;
  try
    cmbSuspendMode.ItemIndex := Integer(abfAPMScheduler.SuspendOptions.Mode);
  finally
    cmbSuspendMode.OnChange := cmbModeChange;
  end;

  chbForced.Checked := abfAPMScheduler.SuspendOptions.Forced;

  if abfAPMScheduler.WakeUpOptions.Enabled then
    for i:= 0 to grpWakeUp.ControlCount - 1 do
      grpWakeUp.Controls[i].Enabled := chbScheduler.Checked;

  edWakeUpDate.OnChange := nil;
  try
    edWakeUpDate.Text := DateToStr(abfAPMScheduler.WakeUpOptions.Date);
  finally
    edWakeUpDate.OnChange := edDateTimeChange;
  end;

  edWakeUpTime.OnChange := nil;
  try
    edWakeUpTime.Text := TimeToStr(abfAPMScheduler.WakeUpOptions.Time);
  finally
    edWakeUpTime.OnChange := edDateTimeChange;
  end;

  cmbWakeUpMode.OnChange := nil;
  try
    cmbWakeUpMode.ItemIndex := Integer(abfAPMScheduler.SuspendOptions.Mode);
  finally
    cmbWakeUpMode.OnChange := cmbModeChange;
  end;
end;{procedure TfrmMain.RefreshAPMSchedulerStatus}

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.FormCreate(Sender: TObject);
begin
  abfAPMScheduler.SuspendOptions.Date := Date;
  abfAPMScheduler.WakeUpOptions.Date := Date;
  abfAPMScheduler.SuspendOptions.Time := Time;
  abfAPMScheduler.WakeUpOptions.Time := Time;
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.FormShow(Sender: TObject);
begin
  TrayIcon.Visible := False;
  RefreshManagerStatus;
  RefreshSchedulerStatus;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfAPMManager, '', '', True);
    2: abfComponentAboutEx(abfAPMScheduler, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;


//==============================================================================
// TabfAPMManager

procedure TfrmAPMDemoMain.EnabledManagerClick(Sender: TObject);
begin
  abfAPMManager.Enabled := not abfAPMManager.Enabled;
  RefreshManagerStatus;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.chbBlockingModeClick(Sender: TObject);
var
  Temp: TabfAPMManagerBlockingModes;
begin
  TCheckBox(Sender).OnClick := nil;
  try
    Temp := [];
    if chbSystemActivity .Checked then Include(Temp, amSystemActivity);
    if chbDisplayActivity.Checked then Include(Temp, amDisplayActivity);
    if chbUserPresent    .Checked then Include(Temp, amUserPresent);

    abfAPMManager.BlockingModes := Temp;

    chbSystemActivity .Checked := amSystemActivity in abfAPMManager.BlockingModes;
    chbDisplayActivity.Checked := amDisplayActivity in abfAPMManager.BlockingModes;
    chbUserPresent    .Checked := amUserPresent in abfAPMManager.BlockingModes;
  finally
    TCheckBox(Sender).OnClick := chbBlockingModeClick;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.abfAPMManagerPowerStatusChange(Sender: TObject);
begin
  UpdatePowerStatus;
end;

//==============================================================================
// TabfAPMScheduler

procedure TfrmAPMDemoMain.EnabledSchedulerClick(Sender: TObject);
begin
  abfAPMScheduler.Enabled := not abfAPMScheduler.Enabled;
  RefreshSchedulerStatus;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.chbGroupEnabledClick(Sender: TObject);
var
  i: Integer;
begin
  with TCheckBox(Sender) do
    if Tag = 1 then
    begin
      abfAPMScheduler.SuspendOptions.Enabled := Checked;
      for i:= 0 to grpSuspend.ControlCount - 1 do
        grpSuspend.Controls[i].Enabled := Checked;
    end else
    begin
      abfAPMScheduler.WakeUpOptions.Enabled := Checked;
      for i:= 0 to grpWakeUp.ControlCount - 1 do
        grpWakeUp.Controls[i].Enabled := Checked;
    end;
end;
//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.cmbModeChange(Sender: TObject);
begin
  with TComboBox(Sender) do
    if Tag = 1 then
      abfAPMScheduler.SuspendOptions.Mode := TabfAPMTimerMode(ItemIndex)
    else
      abfAPMScheduler.WakeUpOptions.Mode := TabfAPMTimerMode(ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.edDateTimeChange(Sender: TObject);
begin
  try
    with TEdit(Sender) do
      case Tag of
        1: abfAPMScheduler.SuspendOptions.Date := StrToDate(Text);
        2: abfAPMScheduler.SuspendOptions.Time := StrToTime(Text);
        3: abfAPMScheduler.WakeUpOptions .Date := StrToDate(Text);
        4: abfAPMScheduler.WakeUpOptions .Time := StrToTime(Text);
      end;
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.pmMainPopup(Sender: TObject);
begin
  miEnabledManager.Checked := abfAPMManager.Enabled;
  miEnabledScheduler.Checked := abfAPMScheduler.Enabled;
end;

//------------------------------------------------------------------------------

procedure TfrmAPMDemoMain.miRestoreClick(Sender: TObject);
begin
  TrayIcon.Visible := False;
end;

//------------------------------------------------------------------------------

end{unit abfAPMDemoMain}.

