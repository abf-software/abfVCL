{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfAPM;

{$I abf.inc}

interface

uses
  Windows, Consts, Classes, Controls, Messages,
  abfClasses, abfComponents,
  abfAPMUtils;

type

//==============================================================================
// TabfAPMManager
//==============================================================================
// TabfAPMManager is a non-visual component that allows control the system
// power behaviours. Contains information about power supply, battery lifetime
// and other APM statistics and information. Allows suspending and resuming of
// the system or blocking of these features. Provides easy access to the APM
// functionality.

  TabfPowerCapabilities = class(TPersistent)
  private
    FPC: TSystemPowerCapabilities;
    function GetBooleanValue(Index: Integer): Boolean;
    function GetByteValue(Index: Integer): Byte;
    function GetSystemPowerStateValue(Index: Integer): TSystemPowerState;
    procedure SetBooleanValue(Index: Integer; Value: Boolean);
    procedure SetByteValue(Index: Integer; Value: Byte);
    procedure SetSystemPowerStateValue(Index: Integer; Value: TSystemPowerState);
  public
    constructor Create;
    procedure Refresh; virtual;
  published
    property PowerButtonPresent: Boolean index 0 read GetBooleanValue
      write SetBooleanValue stored False;
    property SleepButtonPresent: Boolean index 1 read GetBooleanValue
      write SetBooleanValue stored False;
    property LidPresent: Boolean index 2 read GetBooleanValue
      write SetBooleanValue stored False;
    property SystemS1: Boolean index 3 read GetBooleanValue
      write SetBooleanValue stored False;
    property SystemS2: Boolean index 4 read GetBooleanValue
      write SetBooleanValue stored False;
    property SystemS3: Boolean index 5 read GetBooleanValue
      write SetBooleanValue stored False;
    property SystemS4: Boolean index 6 read GetBooleanValue
      write SetBooleanValue stored False;
    property SystemS5: Boolean index 7 read GetBooleanValue
      write SetBooleanValue stored False;
    property HiberFilePresent: Boolean index 8 read GetBooleanValue
      write SetBooleanValue stored False;
    property FullWake: Boolean index 9 read GetBooleanValue
      write SetBooleanValue stored False;
    property VideoDimPresent: Boolean index 10 read GetBooleanValue
      write SetBooleanValue stored False;
    property ApmPresent: Boolean index 11 read GetBooleanValue
      write SetBooleanValue stored False;
    property UpsPresent: Boolean index 12 read GetBooleanValue
      write SetBooleanValue stored False;
    property ThermalControl: Boolean index 13 read GetBooleanValue
      write SetBooleanValue stored False;
    property ProcessorThrottle: Boolean index 14 read GetBooleanValue
      write SetBooleanValue stored False;
    property ProcessorMinThrottle: Byte index 0 read GetByteValue
      write SetByteValue stored False;
    property ProcessorMaxThrottle: Byte index 1 read GetByteValue
      write SetByteValue stored False;
    property DiskSpinDown: Boolean index 15 read GetBooleanValue
      write SetBooleanValue stored False;
    property SystemBatteriesPresent: Boolean index 16 read GetBooleanValue
      write SetBooleanValue stored False;
    property BatteriesAreShortTerm: Boolean index 17 read GetBooleanValue
      write SetBooleanValue stored False;
    property AcOnLineWake: TSystemPowerState index 0 read GetSystemPowerStateValue
      write SetSystemPowerStateValue stored False;
    property SoftLidWake: TSystemPowerState index 1 read GetSystemPowerStateValue
      write SetSystemPowerStateValue stored False;
    property RtcWake: TSystemPowerState index 2 read GetSystemPowerStateValue
      write SetSystemPowerStateValue stored False;
  end;

  TabfSystemBatteryState = class(TPersistent)
  private
    FSBS: TSystemBatteryState;
    function GetBooleanValue(Index: Integer): Boolean;
    function GetCardinalValue(Index: Integer): Cardinal;
    procedure SetBooleanValue(Index: Integer; Value: Boolean);
    procedure SetCardinalValue(Index: Integer; Value: Cardinal);
  public
    constructor Create;
    procedure Refresh; virtual;
  published
    property AcOnLine: Boolean index 0 read GetBooleanValue
      write SetBooleanValue stored False;
    property BatteryPresent: Boolean index 1 read GetBooleanValue
      write SetBooleanValue stored False;
    property Charging: Boolean index 2 read GetBooleanValue
      write SetBooleanValue stored False;
    property Discharging: Boolean index 3 read GetBooleanValue
      write SetBooleanValue stored False;
    property MaxCapacity: Cardinal index 0 read GetCardinalValue
      write SetCardinalValue stored False;
    property RemainingCapacity: Cardinal index 1 read GetCardinalValue
      write SetCardinalValue stored False;
    property Rate: Cardinal index 2 read GetCardinalValue
      write SetCardinalValue stored False;
    property EstimatedTime: Cardinal index 3 read GetCardinalValue
      write SetCardinalValue stored False;
  end;

  TabfACLineStatus = (lsOffline, lsOnline, lsBackupPower, lsUnknown);

  TabfBatteryFlag = (bfHigh, bfLow, bfCritical, bfCharging, bfNoBattery,
    bfUnknown);

  TabfAPMManagerBlockingMode = (amSystemActivity, amDisplayActivity,
    amUserPresent);
  TabfAPMManagerBlockingModes = set of TabfAPMManagerBlockingMode;

  TabfQuerySuspendEvent = procedure(Sender: TObject;
    var CanSuspend : Boolean) of object;

  TabfAPMManager = class(TabfComponent)
  private
    FWndProcHook: TabfCustomWndProcHook;
    FBlockingModes: TabfAPMManagerBlockingModes;
    FEnabled: Boolean;
    FPowerCapabilities: TabfPowerCapabilities;
    FOnBatteryLow: TNotifyEvent;
    FOnPowerStatusChange: TNotifyEvent;
    FOnQuerySuspend: TabfQuerySuspendEvent;
    FOnQuerySuspendFailed: TNotifyEvent;
    FOnResume: TNotifyEvent;
    FOnSuspend: TNotifyEvent;
    FSystemBatteryState: TabfSystemBatteryState;
    FSystemPowerStatus: TSystemPowerStatus;
    function GetACLineStatus: TabfACLineStatus;
    function GetBatteryFlag: TabfBatteryFlag;
    function GetBatteryFullLifeTime: Integer;
    function GetBatteryLifePercent: ShortInt;
    function GetBatteryLifeTime: Integer;
    function GetBlockingModes: TabfAPMManagerBlockingModes;
    procedure OnMessageBefore(Sender: TObject; var Msg: TMessage;
      var Handled: Boolean);
    procedure SetACLineStatus(Value: TabfACLineStatus);
    procedure SetBatteryFlag(Value: TabfBatteryFlag);
    procedure SetBatteryFullLifeTime(Value: Integer);
    procedure SetBatteryLifePercent(Value: ShortInt);
    procedure SetBatteryLifeTime(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetBlockingModes(Value: TabfAPMManagerBlockingModes);
    procedure SetPowerCapabilities(Value: TabfPowerCapabilities);
    procedure SetSystemBatteryState(Value: TabfSystemBatteryState);
  protected
    procedure UpdateBlockingModes; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HibernateSystem(Force: Boolean): Boolean;
    function SuspendSystem(Force: Boolean): Boolean;
    procedure RefreshSystemPowerStatus; virtual;
  published
    property About;
    property ACLineStatus: TabfACLineStatus read GetACLineStatus
      write SetACLineStatus stored False;
    property BatteryFlag: TabfBatteryFlag read GetBatteryFlag
      write SetBatteryFlag stored False;
    property BatteryFullLifeTime: Integer read GetBatteryFullLifeTime
      write SetBatteryFullLifeTime stored False;
    property BatteryLifePercent: ShortInt read GetBatteryLifePercent
      write SetBatteryLifePercent stored False;
    property BatteryLifeTime: Integer read GetBatteryLifeTime
      write SetBatteryLifeTime stored False;
    property BlockingModes: TabfAPMManagerBlockingModes read GetBlockingModes
      write SetBlockingModes;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property PowerCapabilities: TabfPowerCapabilities read FPowerCapabilities
      write SetPowerCapabilities;
    property SystemBatteryState: TabfSystemBatteryState read FSystemBatteryState
      write SetSystemBatteryState;
    property OnBatteryLow: TNotifyEvent read FOnBatteryLow write FOnBatteryLow;
    property OnPowerStatusChange: TNotifyEvent read FOnPowerStatusChange
      write FOnPowerStatusChange;
    property OnQuerySuspend: TabfQuerySuspendEvent read FOnQuerySuspend
      write FOnQuerySuspend;
    property OnQuerySuspendFailed: TNotifyEvent read FOnQuerySuspendFailed
      write FOnQuerySuspendFailed;
    property OnResume: TNotifyEvent read FOnResume write FOnResume;
    property OnSuspend: TNotifyEvent read FOnSuspend write FOnSuspend;
  end;{TabfAPMManager = class(TabfComponent)}


//==============================================================================
// Forward declaration

  TabfAPMScheduler = class;


//==============================================================================
// TabfAPMSchedulerOptions
//==============================================================================
// Prototype class for XXXOptions properties of the TabfAPMScheduler component.

  TabfAPMTimerMode = (tmOneTime, tmEveryDay, tmEveryWeek);

  TabfAPMSchedulerOptions = class(TPersistent)
  private
    FDateTime: TDateTime;
    FEnabled: Boolean;
    FMode: TabfAPMTimerMode;
    FOwner: TabfAPMScheduler;
    function GetDate: TDate;
    function GetTime: TTime;
    procedure SetDate(Value: TDate);
    procedure SetEnabled(Value: Boolean);
    procedure SetMode(Value: TabfAPMTimerMode);
    procedure SetTime(Value: TTime);
  protected
    constructor Create(AOwner: TabfAPMScheduler);
  public
    property Date: TDate read GetDate write SetDate;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Mode: TabfAPMTimerMode read FMode write SetMode default tmOneTime;
    property Time: TTime read GetTime write SetTime;
  end;


//==============================================================================
// TabfAPMSuspendOptions
//==============================================================================
// Class for the SuspendOptions property of the TabfAPMScheduler component.

  TabfAPMSuspendOptions = class(TabfAPMSchedulerOptions)
  private
    FForced: Boolean;
  protected
    constructor Create(AOwner: TabfAPMScheduler);
  published
    property Date;
    property Enabled;
    property Forced: Boolean read FForced write FForced default True;
    property Mode;
    property Time;
  end;


//==============================================================================
// TabfAPMWakeUpOptions
//==============================================================================
// Class for the WakeUpOptions property of the TabfAPMScheduler component.

  TabfAPMWakeUpOptions = class(TabfAPMSchedulerOptions)
  published
    property Date;
    property Enabled;
    property Mode;
    property Time;
  end;

//==============================================================================
// TabfAPMScheduler
//==============================================================================
// TabfAPMScheduler is a non-visual component that allows easily create APM
// oriented timers and schedulers. Has two independent timers to control a
// suspending and resuming of the system. Also you can specify the APM events
// that will occur every day, week, month etc. Very useful in diagnostic, auto
// sleep/wake-up, and "long time working" programs.

  TabfAPMScheduler = class(TabfComponent)
  private
    FEnabled: Boolean;
    FOnSuspend: TNotifyEvent;
    FOnWakeUp: TNotifyEvent;
    FSuspendOptions: TabfAPMSuspendOptions;
    FTimerThread: TThread;
    FWakeUpOptions: TabfAPMWakeUpOptions;
    function GetSuspendDateTime: TDateTime;
    function GetWakeUpDateTime: TDateTime;
    procedure SetEnabled(Value: Boolean);
    procedure SetSuspendDateTime(Value: TDateTime);
    procedure SetSuspendOptions(Value: TabfAPMSuspendOptions);
    procedure SetWakeUpDateTime(Value: TDateTime);
    procedure SetWakeUpOptions(Value: TabfAPMWakeUpOptions);
  protected
    procedure Loaded; override;
    procedure DoSuspend; virtual;
    procedure DoWakeUp; virtual;
    procedure UpdateTimerThread; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property About;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property SuspendDateTime: TDateTime read GetSuspendDateTime
      write SetSuspendDateTime;
    property SuspendOptions: TabfAPMSuspendOptions read FSuspendOptions
      write SetSuspendOptions;
    property WakeUpDateTime: TDateTime read GetWakeUpDateTime
      write SetWakeUpDateTime;
    property WakeUpOptions: TabfAPMWakeUpOptions read FWakeUpOptions
      write SetWakeUpOptions;
    property OnSuspend: TNotifyEvent read FOnSuspend write FOnSuspend;
    property OnWakeUp: TNotifyEvent read FOnWakeUp write FOnWakeUp;
  end;

//==============================================================================
// Interface to WIN32 API functions. Special for D2/C1
//==============================================================================

{$IFNDEF D3}
function CreateWaitableTimer(lpTimerAttributes: PSecurityAttributes;
  bManualReset: BOOL; lpTimerName: PChar): THandle; stdcall;
function SetWaitableTimer(hTimer: THandle; var lpDueTime: TLargeInteger;
  lPeriod: Longint; pfnCompletionRoutine: TFarProc;
  lpArgToCompletionRoutine: Pointer; fResume: BOOL): BOOL; stdcall;
function CancelWaitableTimer(hTimer: THandle): BOOL; stdcall;
{$ENDIF D3}

{******************************************************************************}
implementation
{******************************************************************************}

uses
   Forms, SysUtils,
   abfSysUtils;

{$I abf_init.inc}


//==============================================================================
// TabfAPMManager
//==============================================================================
// TabfAPMManager is a non-visual component that allows control the system
// power behaviours. Contains information about power supply, battery lifetime
// and other APM statistics and information. Allows suspending and resuming of
// the system or blocking of these features. Provides easy access to the APM
// functionality.
// Date: 07/01/2000

constructor TabfPowerCapabilities.Create;
begin
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TabfPowerCapabilities.Refresh;
begin
  GetPwrCapabilities(FPC);
end;

//------------------------------------------------------------------------------

function TabfPowerCapabilities.GetBooleanValue(Index: Integer): Boolean;
begin
  case Index of
    0: Result := FPC.PowerButtonPresent;
    1: Result := FPC.SleepButtonPresent;
    2: Result := FPC.LidPresent;
    3: Result := FPC.SystemS1;
    4: Result := FPC.SystemS2;
    5: Result := FPC.SystemS3;
    6: Result := FPC.SystemS4;
    7: Result := FPC.SystemS5;
    8: Result := FPC.HiberFilePresent;
    9: Result := FPC.FullWake;
    10: Result := FPC.VideoDimPresent;
    11: Result := FPC.ApmPresent;
    12: Result := FPC.UpsPresent;
    13: Result := FPC.ThermalControl;
    14: Result := FPC.ProcessorThrottle;
    15: Result := FPC.DiskSpinDown;
    16: Result := FPC.SystemBatteriesPresent;
    17: Result := FPC.BatteriesAreShortTerm;
  else
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

function TabfPowerCapabilities.GetByteValue(Index: Integer): Byte;
begin
  case Index of
    0: Result := FPC.ProcessorMinThrottle;
    1: Result := FPC.ProcessorMaxThrottle;
  else
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

function TabfPowerCapabilities.GetSystemPowerStateValue(
  Index: Integer): TSystemPowerState;
begin
  case Index of
    0: Result := FPC.AcOnLineWake;
    1: Result := FPC.SoftLidWake;
    2: Result := FPC.RtcWake;
  else
    Result := spsPowerSystemUnspecified;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfPowerCapabilities.SetBooleanValue(Index: Integer; Value: Boolean);
begin
//fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfPowerCapabilities.SetByteValue(Index: Integer; Value: Byte);
begin
//fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfPowerCapabilities.SetSystemPowerStateValue(Index: Integer;
  Value: TSystemPowerState);
begin
//fake procedure
end;

//------------------------------------------------------------------------------

constructor TabfSystemBatteryState.Create;
begin
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TabfSystemBatteryState.Refresh;
begin
  FillChar(FSBS, SizeOf(FSBS), 0);
  CallNtPowerInformation(pilSystemBatteryState, nil, 0, @FSBS,
    SizeOf(FSBS));
end;

//------------------------------------------------------------------------------

function TabfSystemBatteryState.GetBooleanValue(Index: Integer): Boolean;
begin
  case Index of
    0: Result := FSBS.AcOnLine;
    1: Result := FSBS.BatteryPresent;
    2: Result := FSBS.Charging;
    3: Result := FSBS.Discharging;
  else
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

function TabfSystemBatteryState.GetCardinalValue(Index: Integer): Cardinal;
begin
  case Index of
    0: Result := FSBS.MaxCapacity;
    1: Result := FSBS.RemainingCapacity;
    2: Result := FSBS.Rate;
    3: Result := FSBS.EstimatedTime;    
  else
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfSystemBatteryState.SetBooleanValue(Index: Integer; Value: Boolean);
begin
//fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfSystemBatteryState.SetCardinalValue(Index: Integer; Value: Cardinal);
begin
//fake procedure
end;

//------------------------------------------------------------------------------

constructor TabfAPMManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FPowerCapabilities := TabfPowerCapabilities.Create;
  FSystemBatteryState := TabfSystemBatteryState.Create;
  GetBlockingModes;
  FWndProcHook := TabfCustomWndProcHook.Create(nil);
  FWndProcHook.OnMessageBefore := OnMessageBefore;
  FWndProcHook.Handle := Application.Handle;
  Enabled := True;
end;

//------------------------------------------------------------------------------

destructor TabfAPMManager.Destroy;
begin
  FEnabled := False;
  if Assigned(FPowerCapabilities) then
    FreeAndNil(FPowerCapabilities);
  if Assigned(FSystemBatteryState) then
    FreeAndNil(FSystemBatteryState);
  UpdateBlockingModes;
  FWndProcHook.OnMessageBefore := nil;
  FWndProcHook.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfAPMManager.GetACLineStatus: TabfACLineStatus;
begin
  case FSystemPowerStatus.ACLineStatus of
    0: Result := lsOffline;
    1: Result := lsOnline;
    2: Result := lsBackupPower;
  else
    Result := lsUnknown;
  end;
end;

//------------------------------------------------------------------------------

function TabfAPMManager.GetBatteryFlag: TabfBatteryFlag;
begin
  Result := bfUnknown;
  if Boolean(FSystemPowerStatus.BatteryFlag and $80) then
    Result := bfNoBattery
  else if Boolean(FSystemPowerStatus.BatteryFlag and 8) then
    Result := bfCharging
  else if Boolean(FSystemPowerStatus.BatteryFlag and 1) then
    Result := bfHigh
  else if Boolean(FSystemPowerStatus.BatteryFlag and 0) then
    Result := bfHigh
  else if Boolean(FSystemPowerStatus.BatteryFlag and 2) then
    Result := bfLow
  else if Boolean(FSystemPowerStatus.BatteryFlag and 4) then
    Result := bfCritical;
end;

//------------------------------------------------------------------------------

function TabfAPMManager.GetBatteryFullLifeTime: Integer;
begin
  Result := Integer(FSystemPowerStatus.BatteryFullLifeTime);
end;

//------------------------------------------------------------------------------

function TabfAPMManager.GetBatteryLifePercent: ShortInt;
begin
  Result := ShortInt(FSystemPowerStatus.BatteryLifePercent);
end;

//------------------------------------------------------------------------------

function TabfAPMManager.GetBatteryLifeTime: Integer;
begin
  Result := Integer(FSystemPowerStatus.BatteryLifeTime);
end;

//------------------------------------------------------------------------------

function TabfAPMManager.GetBlockingModes: TabfAPMManagerBlockingModes;
var
  Flags: DWORD;
begin
  FBlockingModes := [];
  Flags := SetThreadExecutionState_Safe(ES_CONTINUOUS);
  SetThreadExecutionState_Safe(Flags);

  if Bool(Flags and ES_SYSTEM_REQUIRED) then
    Include(FBlockingModes, amSystemActivity);
  if Bool(Flags and ES_DISPLAY_REQUIRED) then
    Include(FBlockingModes, amDisplayActivity);
  if Bool(Flags and ES_USER_PRESENT) then
    Include(FBlockingModes, amUserPresent);

  Result := FBlockingModes;
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.OnMessageBefore(Sender: TObject; var Msg: TMessage;
  var Handled: Boolean);
var
  CanSuspend: Boolean;
begin
  with Msg do
    if Enabled and (not (csReading in ComponentState)) and
      (Msg = WM_POWERBROADCAST) then
    begin
      case WParam of
        PBT_APMBATTERYLOW:
          begin
            if not (csDesigning in ComponentState) then
              if not _ASK then _TADA;
            RefreshSystemPowerStatus;
            if Assigned(FOnBatteryLow) then FOnBatteryLow(Self);
          end;
        PBT_APMQUERYSUSPEND:
          begin
            if LParamLo = 1 then
            begin
              CanSuspend := True;
              if Assigned(FOnQuerySuspend) then
                FOnQuerySuspend(Self, CanSuspend);
              if CanSuspend then Result := 0
              else Result := BROADCAST_QUERY_DENY;
            end else
            if Assigned(FOnSuspend) then FOnSuspend(Self);
          end;
        PBT_APMQUERYSUSPENDFAILED:
          if Assigned(FOnSuspend) then FOnSuspend(Self);
        PBT_APMSUSPEND:
          if Assigned(FOnQuerySuspendFailed) then FOnQuerySuspendFailed(Self);
        PBT_APMRESUMECRITICAL,
        PBT_APMRESUMESUSPEND,
        PBT_APMRESUMESTANDBY,
        PBT_APMRESUMEAUTOMATIC:
          if Assigned(FOnResume) then FOnResume(Self);
        PBT_APMOEMEVENT: ;
        PBT_APMPOWERSTATUSCHANGE:
          begin
            RefreshSystemPowerStatus;
            if Assigned(FOnPowerStatusChange) then FOnPowerStatusChange(Self);
          end;
      end;
    end else

end;{procedure TabfAPMManager.OnMessageBefore}

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetACLineStatus(Value: TabfACLineStatus);
begin
  //fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetBatteryFlag(Value: TabfBatteryFlag);
begin
  //fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetBatteryFullLifeTime(Value: Integer);
begin
  //fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetBatteryLifePercent(Value: ShortInt);
begin
  //fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetBatteryLifeTime(Value: Integer);
begin
  //fake procedure
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    FWndProcHook.Active := FEnabled;
    if not (csReading in ComponentState) then
    begin
      if FEnabled then
        RefreshSystemPowerStatus;
      UpdateBlockingModes;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetBlockingModes(Value: TabfAPMManagerBlockingModes);
begin
  if FBlockingModes <> Value then
  begin
    FBlockingModes := Value;
    if not (csReading in ComponentState) then UpdateBlockingModes;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetPowerCapabilities(Value: TabfPowerCapabilities);
begin
  if FPowerCapabilities <> Value then
    FPowerCapabilities.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.SetSystemBatteryState(Value: TabfSystemBatteryState);
begin
  if FSystemBatteryState <> Value then
    FSystemBatteryState.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.Loaded;
begin
  inherited Loaded;
  UpdateBlockingModes;
  RefreshSystemPowerStatus;
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.UpdateBlockingModes;
var
  Flags: DWORD;
begin
  Flags := SetThreadExecutionState_Safe(ES_CONTINUOUS);
  if FEnabled then
  begin
    if amSystemActivity in FBlockingModes then
      Flags := Flags or ES_SYSTEM_REQUIRED
    else
      Flags := Flags and (not ES_SYSTEM_REQUIRED);

    if amDisplayActivity in FBlockingModes then
      Flags := Flags or ES_DISPLAY_REQUIRED
    else
      Flags := Flags and (not ES_DISPLAY_REQUIRED);

    if amUserPresent in FBlockingModes then
      Flags := Flags or ES_USER_PRESENT
    else
      Flags := Flags and (not ES_USER_PRESENT);
    SetThreadExecutionState_Safe(Flags);
  end;
end;

//------------------------------------------------------------------------------

function TabfAPMManager.HibernateSystem(Force: Boolean): Boolean;
begin
  Result := SetSystemPowerState_Safe(False, Force);
end;

//------------------------------------------------------------------------------

function TabfAPMManager.SuspendSystem(Force: Boolean): Boolean;
begin
  Result := SetSystemPowerState_Safe(True, Force);
end;

//------------------------------------------------------------------------------

procedure TabfAPMManager.RefreshSystemPowerStatus;
var
  Hour, Min, Sec: Cardinal;
begin
  FPowerCapabilities.Refresh;
  FSystemBatteryState.Refresh;
  GetSystemPowerStatus(FSystemPowerStatus);

  Hour := FSystemBatteryState.EstimatedTime div 3600;
  Min := (FSystemBatteryState.EstimatedTime - Hour * 3600) div 60;
  Sec := FSystemBatteryState.EstimatedTime - Hour * 3600 - Min * 60;

  abfTrace(Format('AcOnLine = %0:d, BatteryPresent = %1:d, Charging = %2:d, Discharging = %3:d',
    [Integer(FSystemBatteryState.AcOnLine), Integer(FSystemBatteryState.BatteryPresent),
      Integer(FSystemBatteryState.Charging), Integer(FSystemBatteryState.Discharging)]));
  abfTrace(Format('EstimatedTime: %2.2dh:%2.2dm:%2.2ds', [Hour, Min, Sec]));
  abfTrace(Format('MaxCapacity: %d mWh', [FSystemBatteryState.MaxCapacity]));
  abfTrace(Format('RemainingCapacity: %d mWh', [FSystemBatteryState.RemainingCapacity]));
  abfTrace(Format('Rate: %d mWh', [FSystemBatteryState.Rate]));{}
end;


//==============================================================================
// TWaitTimerThread
//==============================================================================
// Used internally.
// Date: 04/01/2000

type
  TWaitTimerThread = class(TThread)
  private
    FOwner: TabfAPMScheduler;
    FSuspendTimerCallback: TabfCallbackThunk;
    FWakeUpTimerCallback: TabfCallbackThunk;
    FSuspendTimerFired: Boolean;
    FWakeUpTimerFired: Boolean;
    FSuspendTimerHandle: THandle;
    FTerminateHandle: THandle;
    FUpdatingTimers: Boolean;
    FWakeUpTimerHandle: THandle;
    FException: Exception;
    procedure HandleException;
    procedure SuspendTimerProc(lpArgs: Pointer; dwTimerLow: DWORD;
      dwTimerHigh: DWORD); stdcall;
    procedure WakeUpTimerProc(lpArgs: Pointer; dwTimerLow: DWORD;
      dwTimerHigh: DWORD); stdcall;
  protected
    procedure Execute; override;
    procedure UpdateTimers; virtual;
  public
    constructor Create(AOwner: TabfAPMScheduler);
    destructor Destroy; override;
    property TerminateHandle: THandle read FTerminateHandle;
  end;

//------------------------------------------------------------------------------

constructor TWaitTimerThread.Create(AOwner: TabfAPMScheduler);
begin
  inherited Create(True);
  FOwner := AOwner;
  FSuspendTimerFired := False;
  FUpdatingTimers := True;
  FWakeUpTimerFired := False;
  FreeOnTerminate := True;
  Priority := tpNormal;
  FTerminateHandle := CreateEvent(nil, True, False, '');
  if FOwner.FSuspendOptions.Enabled and (FOwner.SuspendDateTime <> 0) then
    FSuspendTimerHandle := THandle(CreateWaitableTimer(nil, False, nil))
  else
    FSuspendTimerHandle := 0;
  if FOwner.FWakeUpOptions.Enabled and (FOwner.WakeUpDateTime <> 0) then
    FWakeUpTimerHandle := THandle(CreateWaitableTimer(nil, False, nil))
  else
    FWakeUpTimerHandle := 0;
  FSuspendTimerCallback := CabfCallbackThunk;
  FSuspendTimerCallback.SelfPtr := Self;
  FSuspendTimerCallback.JmpOffset := Integer(@TWaitTimerThread.SuspendTimerProc) -
    Integer(@FSuspendTimerCallback.JMP) - 5;
  FWakeUpTimerCallback := CabfCallbackThunk;
  FWakeUpTimerCallback.SelfPtr := Self;
  FWakeUpTimerCallback.JmpOffset := Integer(@TWaitTimerThread.WakeUpTimerProc) -
    Integer(@FWakeUpTimerCallback.JMP) - 5;
  Resume;
end;{constructor TWaitTimerThread.Create}

//------------------------------------------------------------------------------

destructor TWaitTimerThread.Destroy;
begin
  FOwner := nil;

  if FSuspendTimerHandle <> 0 then
  begin
    CloseHandle(FSuspendTimerHandle);
    FSuspendTimerHandle := 0;
  end;

  if FWakeUpTimerHandle <> 0 then
  begin
    CloseHandle(FWakeUpTimerHandle);
  end;

  if FTerminateHandle <> 0 then
  begin
    SetEvent(FTerminateHandle);
    CloseHandle(FTerminateHandle);
    FTerminateHandle := 0;
  end;

  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TWaitTimerThread.HandleException;
begin
  if not (FException is EAbort) then
  begin
    if Assigned(Application.OnException) then
      Application.OnException(Self, FException)
    else
      Application.ShowException(FException);
  end;
end;

//------------------------------------------------------------------------------

procedure TWaitTimerThread.SuspendTimerProc(lpArgs: Pointer; dwTimerLow: DWORD;
  dwTimerHigh: DWORD); stdcall;
begin
  if not FUpdatingTimers then FSuspendTimerFired := True;
end;

//------------------------------------------------------------------------------

procedure TWaitTimerThread.WakeUpTimerProc(lpArgs: Pointer; dwTimerLow: DWORD;
  dwTimerHigh: DWORD); stdcall;
begin
  if not FUpdatingTimers then FWakeUpTimerFired := True;
end;

//------------------------------------------------------------------------------

procedure TWaitTimerThread.Execute;

  function _ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwner = nil);
  end;

begin
  UpdateTimers;
  repeat
    if not _ThreadClosed then
      WaitForSingleObjectEx(FTerminateHandle, INFINITE, True);
    if FSuspendTimerFired then
    begin
      FSuspendTimerFired := False;
      try
        if not FUpdatingTimers and Assigned(FOwner) then
        begin
          Synchronize(FOwner.DoSuspend);
          SetSystemPowerState_Safe(True, FOwner.SuspendOptions.Forced);
        end;  
      except
        on E: Exception do
        begin
          FException := E;
          HandleException;
        end;
      end;
    end else
    if FWakeUpTimerFired then
    begin
      FWakeUpTimerFired := False;
      try
        if not FUpdatingTimers and Assigned(FOwner) then Synchronize(FOwner.DoWakeUp);
      except
        on E: Exception do begin
          FException := E;
          HandleException;
        end;
      end;
    end else Terminate;
  until Terminated;
end;{procedure TWaitTimerThread.Execute}

//------------------------------------------------------------------------------

procedure TWaitTimerThread.UpdateTimers;

   function _GetPeriod(Value: TabfAPMTimerMode): Cardinal;
   begin
     case Value of
       tmOneTime: Result := 0;
       tmEveryDay: Result := 86400000;
       tmEveryWeek: Result := 604800000;
     else Result := 0;
     end;
   end;

var
  TimeValue: TLargeInteger;
  FileTime: TFileTime;
  Period: Cardinal;
begin
  if not Assigned(FOwner) or (FOwner.ComponentState *
    [csDesigning, csReading, csDestroying] <> []) then Exit;
  FUpdatingTimers := True;
  try
    if Assigned(FOwner) and (FSuspendTimerHandle > 0) and
      (FOwner.SuspendDateTime <> 0) then
    begin
      Period := _GetPeriod(FOwner.SuspendOptions.Mode);
      FileTime := abfDateTimeToFileTime(FOwner.SuspendDateTime);
      CopyMemory(@TimeValue, @FileTime, SizeOf(TimeValue));
      SetWaitableTimer(FSuspendTimerHandle, TimeValue, Period,
        @FSuspendTimerCallback, nil, False);
      repeat
      until WaitForSingleObjectEx(FSuspendTimerHandle, 10, True) <> WAIT_OBJECT_0;
    end;
    if Assigned(FOwner) and (FWakeUpTimerHandle > 0) and
      (FOwner.WakeUpDateTime <>0) then
    begin
      Period := _GetPeriod(FOwner.WakeUpOptions.Mode);
      FileTime := abfDateTimeToFileTime(FOwner.WakeUpDateTime);
      CopyMemory(@TimeValue, @FileTime, SizeOf(TimeValue));
      SetWaitableTimer(FWakeUpTimerHandle, TimeValue, Period,
        @FWakeUpTimerCallback, nil, True);
      repeat
      until WaitForSingleObjectEx(FWakeUpTimerHandle, 10, True) <> WAIT_OBJECT_0;
    end;
  finally
    FUpdatingTimers := False;
  end;
end;{procedure TWaitTimerThread.UpdateTimers}


//==============================================================================
// TabfAPMSchedulerOptions
//==============================================================================
// Prototype class for XXXOptions properties of the TabfAPMScheduler component.
// Date: 07/01/2000

constructor TabfAPMSchedulerOptions.Create(AOwner: TabfAPMScheduler);
begin
  FOwner := AOwner;
  FDateTime := 0;
  FEnabled := True;
  FMode := tmOneTime;
end;

//------------------------------------------------------------------------------

function TabfAPMSchedulerOptions.GetDate: TDate;
begin
  Result := Int(FDateTime);
end;

//------------------------------------------------------------------------------

function TabfAPMSchedulerOptions.GetTime: TTime;
begin
  Result := Frac(FDateTime);
end;

//------------------------------------------------------------------------------

procedure TabfAPMSchedulerOptions.SetDate(Value: TDate);
begin
  if Int(FDateTime) <> Int(Value) then
  begin
    FDateTime := Int(Value) + GetTime;
    FOwner.UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMSchedulerOptions.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FOwner) then FOwner.UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMSchedulerOptions.SetMode(Value: TabfAPMTimerMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    if Assigned(FOwner) then FOwner.UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMSchedulerOptions.SetTime(Value: TTime);
begin
  if Frac(FDateTime) <> Frac(Value) then
  begin
    FDateTime := GetDate + Frac(Value);
    if Assigned(FOwner) then FOwner.UpdateTimerThread;
  end;
end;


//==============================================================================
// TabfAPMSuspendOptions
//==============================================================================
// Class for the SuspendOptions property of the TabfAPMScheduler component.
// Date: 07/01/2000

constructor TabfAPMSuspendOptions.Create(AOwner: TabfAPMScheduler);
begin
  inherited Create(AOwner);
  FForced := True;
end;


//==============================================================================
// TabfAPMScheduler
//==============================================================================
// TabfAPMScheduler is a non-visual component that allows easily create APM
// oriented timers and schedulers. Has two independent timers to control a
// suspending and resuming of the system. Also you can specify the APM events
// that will occur every day, week, month etc. Very useful in diagnostic, auto
// sleep/wake-up, and "long time working" programs.
// Date: 07/01/2000

constructor TabfAPMScheduler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FSuspendOptions := TabfAPMSuspendOptions.Create(Self);
  FWakeUpOptions := TabfAPMWakeUpOptions.Create(Self);
  FEnabled := True;
end;

//------------------------------------------------------------------------------

destructor TabfAPMScheduler.Destroy;
begin
  Destroying;
  UpdateTimerThread;
  FSuspendOptions.Free;
  FWakeUpOptions.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.Loaded;
begin
  inherited Loaded;
  UpdateTimerThread;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.DoSuspend;
begin
  if (not Enabled) or (csDestroying in ComponentState) then Exit;
  if Assigned(FOnSuspend) then FOnSuspend(Self);
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.DoWakeUp;
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  if (not Enabled) or (csDestroying in ComponentState) then Exit;
  if Assigned(FOnWakeUp) then FOnWakeUp(Self);
end;

//------------------------------------------------------------------------------

function TabfAPMScheduler.GetSuspendDateTime: TDateTime;
begin
  Result := FSuspendOptions.FDateTime;
end;

//------------------------------------------------------------------------------

function TabfAPMScheduler.GetWakeUpDateTime: TDateTime;
begin
  Result := FWakeUpOptions.FDateTime;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.SetSuspendDateTime(Value: TDateTime);
begin
  if FSuspendOptions.FDateTime <> Value then
  begin
    FSuspendOptions.FDateTime := Value;
    UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.SetSuspendOptions(Value: TabfAPMSuspendOptions);
begin
  if FSuspendOptions <> Value then
  begin
    FSuspendOptions.Assign(Value);
    UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.SetWakeUpDateTime(Value: TDateTime);
begin
  if FWakeUpOptions.FDateTime <> Value then
  begin
    FWakeUpOptions.FDateTime := Value;
    UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.SetWakeUpOptions(Value: TabfAPMWakeUpOptions);
begin
  if FWakeUpOptions <> Value then
  begin
    FWakeUpOptions.Assign(Value);
    UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    UpdateTimerThread;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAPMScheduler.UpdateTimerThread;
begin
  if ComponentState * [csDesigning, csReading] <> [] then Exit;
  if Assigned(FTimerThread) then
  begin
    while FTimerThread.Suspended do FTimerThread.Resume;
    SetEvent(TWaitTimerThread(FTimerThread).TerminateHandle);
    FTimerThread.Terminate;
  end;
  FTimerThread := nil;
  if Enabled and
    ((FSuspendOptions.Enabled and (FSuspendOptions.FDateTime <> 0)) or
    (FWakeUpOptions.Enabled and (FWakeUpOptions.FDateTime <> 0))) and
    (not (csDestroying in ComponentState)) then
    FTimerThread := TWaitTimerThread.Create(Self);
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfAPMManager, TabfAPMScheduler]);


{******************************************************************************}
finalization
{******************************************************************************}

end{unit abfAPM}.

