{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfAPMUtils;

{$I abf.inc}
{$ALIGN ON}
{$MINENUMSIZE 4}
{$WARNINGS OFF}
{$HINTS OFF}
{$STACKFRAMES ON}

interface

uses
  Windows;

//==============================================================================
// Power management consts
//==============================================================================
const
// Request for permission to suspend.
  PBT_APMQUERYSUSPEND       = $0000;
  {$IfDef C3}{$NODEFINE PBT_APMQUERYSUSPEND}{$EndIf C3}
// Suspension request denied.
  PBT_APMQUERYSUSPENDFAILED = $0002;
  {$IfDef C3}{$NODEFINE PBT_APMQUERYSUSPENDFAILED}{$EndIf C3}
// System is suspending operation.
  PBT_APMSUSPEND            = $0004;
  {$IfDef C3}{$NODEFINE PBT_APMSUSPEND}{$EndIf C3}
// Operation resuming after critical suspension.
  PBT_APMRESUMECRITICAL     = $0006;
  {$IfDef C3}{$NODEFINE PBT_APMRESUMECRITICAL}{$EndIf C3}
// Operation resuming after suspension.
  PBT_APMRESUMESUSPEND      = $0007;
  {$IfDef C3}{$NODEFINE PBT_APMRESUMESUSPEND}{$EndIf C3}
  PBT_APMRESUMESTANDBY      = $0008;
  {$IfDef C3}{$NODEFINE PBT_APMRESUMESTANDBY}{$EndIf C3}
// Battery power is low.
  PBT_APMBATTERYLOW         = $0009;
  {$IfDef C3}{$NODEFINE PBT_APMBATTERYLOW}{$EndIf C3}
// Power status has changed.
  PBT_APMPOWERSTATUSCHANGE  = $000A;
  {$IfDef C3}{$NODEFINE PBT_APMPOWERSTATUSCHANGE}{$EndIf C3}
// OEM-defined event occurred.
  PBT_APMOEMEVENT           = $000B;
  {$IfDef C3}{$NODEFINE PBT_APMOEMEVENT}{$EndIf C3}
// Operation resuming automatically after event.
  PBT_APMRESUMEAUTOMATIC    = $0012;
  {$IfDef C3}{$NODEFINE PBT_APMRESUMEAUTOMATIC}{$EndIf C3}

  ES_SYSTEM_REQUIRED        = DWORD($00000001);
  {$IfDef C3}{$NODEFINE ES_SYSTEM_REQUIRED}{$EndIf C3}
  ES_DISPLAY_REQUIRED       = DWORD($00000002);
  {$IfDef C3}{$NODEFINE ES_DISPLAY_REQUIRED}{$EndIf C3}
  ES_USER_PRESENT           = DWORD($00000004);
  {$IfDef C3}{$NODEFINE ES_USER_PRESENT}{$EndIf C3}
  ES_CONTINUOUS             = DWORD($80000000);
  {$IfDef C3}{$NODEFINE ES_CONTINUOUS}{$EndIf C3}

// Discharge policy constants
  NUM_DISCHARGE_POLICIES    = 4;
  {$IfDef C3}{$NODEFINE NUM_DISCHARGE_POLICIES}{$EndIf C3}
  DISCHARGE_POLICY_CRITICAL = 0;
  {$IfDef C3}{$NODEFINE DISCHARGE_POLICY_CRITICAL}{$EndIf C3}
  DISCHARGE_POLICY_LOW      = 1;
  {$IfDef C3}{$NODEFINE DISCHARGE_POLICY_LOW}{$EndIf C3}

// The global flags constants are used to enable or disable user
// power policy options (TGlobalUserPowerPolicy.GlobalFlags).

// Enables or disables the battery meter icon in the system tray.
// When this flag is cleared, the battery meter icon is not displayed.
  EnableSysTrayBatteryMeter = $01;
  {$IfDef C3}{$NODEFINE EnableSysTrayBatteryMeter}{$EndIf C3}

// Enables or disables multiple battery display in the system Power Meter.
  EnableMultiBatteryDisplay = $02;
  {$IfDef C3}{$NODEFINE EnableMultiBatteryDisplay}{$EndIf C3}

// Enables or disables requiring password logon when the system resumes
// from standby or hibernate.
  EnablePasswordLogon       = $04;
  {$IfDef C3}{$NODEFINE EnablePasswordLogon}{$EndIf C3}

// Enables or disables wake on ring support.
  EnableWakeOnRing          = $08;
  {$IfDef C3}{$NODEFINE EnableWakeOnRing}{$EndIf C3}

// Enables or disables support for dimming the video display
// when the system changes from running on AC power to running on battery power.
  EnableVideoDimDisplay     = $10;
  {$IfDef C3}{$NODEFINE EnableVideoDimDisplay}{$EndIf C3}

// This constant is passed as a uiID to WritePwrScheme.
  NEWSCHEME                 = -1;
  {$IfDef C3}{$NODEFINE NEWSCHEME}{$EndIf C3}

// Error codes
  STATUS_SUCCESS            = DWORD(0);
  {$IfDef C3}{$NODEFINE STATUS_SUCCESS}{$EndIf C3}
// An invalid parameter was passed to a function.
  STATUS_INVALID_PARAMETER  = DWORD($C000000D);
  {$IfDef C3}{$NODEFINE STATUS_INVALID_PARAMETER}{$EndIf C3}
//  The caller had insufficient access rights to perform the requested action.
  STATUS_ACCESS_DENIED      = DWORD($C0000022);
  {$IfDef C3}{$NODEFINE STATUS_ACCESS_DENIED}{$EndIf C3}
// The output buffer is of insufficient size to contain the data to be returned.
  STATUS_BUFFER_TOO_SMALL   = DWORD($C0000023);
  {$IfDef C3}{$NODEFINE STATUS_BUFFER_TOO_SMALL}{$EndIf C3}

//==============================================================================
// Power management types
//==============================================================================
type
  {$IfDef C3}{$NODEFINE LPTSTR}{$EndIf C3}
  LPTSTR = PAnsiChar; { should be PWideChar if UNICODE }


// The TPowerInformationLevel enumeration type defines values that are used
// to specify the power information to be set or retrieved.
  TPowerInformationLevel = (
    pilSystemPowerPolicyAc,
    pilSystemPowerPolicyDc,
    pilVerifySystemPolicyAc,
    pilVerifySystemPolicyDc,
    pilSystemPowerCapabilities,
    pilSystemBatteryState,
    pilSystemPowerStateHandler,
    pilProcessorStateHandler,
    pilSystemPowerPolicyCurrent,
    pilAdministratorPowerPolicy,
    pilSystemReserveHiberFile,
    pilProcessorInformation,
    pilSystemPowerInformation,
    pilProcessorStateHandler2,
    pilLastWakeTime,           // Compare with KeQueryInterruptTime()
    pilLastSleepTime,          // Compare with KeQueryInterruptTime()
    pilSystemExecutionState,
    pilSystemPowerStateNotifyHandler,
    pilProcessorPowerPolicyAc,
    pilProcessorPowerPolicyDc,
    pilVerifyProcessorPowerPolicyAc,
    pilVerifyProcessorPowerPolicyDc,
    pilProcessorPowerPolicyCurrent
  );

// The TSystemPowerState enumeration type defines values that are used
// to specify system power states.
  PSystemPowerState = ^TSystemPowerState;
  TSystemPowerState = (
    spsPowerSystemUnspecified,
    spsPowerSystemWorking,
    spsPowerSystemSleeping1,
    spsPowerSystemSleeping2,
    spsPowerSystemSleeping3,
    spsPowerSystemHibernate,
    spsPowerSystemShutdown,
    spsPowerSystemMaximum
  );


// The TPowerAction enumeration type defines values that are used
// to specify system power action types.
  PPowerAction = ^TPowerAction;
  TPowerAction = (
    paPowerActionNone,
    paPowerActionReserved,
    paPowerActionSleep,
    paPowerActionHibernate,
    paPowerActionShutdown,
    paPowerActionShutdownReset,
    paPowerActionShutdownOff,
    paPowerActionWarmEject
  );


// The TSystemBatteryState structure contains information about
// the current state of the system battery.
  PSystemBatteryState = ^TSystemBatteryState;
  TSystemBatteryState = packed record
    AcOnLine         : Boolean;
    BatteryPresent   : Boolean;
    Charging         : Boolean;
    Discharging      : Boolean;
    Spare1           : packed array[0..3] of Boolean;
    MaxCapacity      : DWORD;
    RemainingCapacity: DWORD;
    Rate             : DWORD;
    EstimatedTime    : DWORD;
    DefaultAlert1    : DWORD;
    DefaultAlert2    : DWORD;
  end;


// The TPowerActionPolicy structure contains information used to set
// the system power state.
  PPowerActionPolicy = ^TPowerActionPolicy;
  TPowerActionPolicy = packed record
    Action: TPowerAction;
    Flags: ULONG;
    EventCode: ULONG;
  end;


// The TSystemPowerLevel structure contains information about system battery
// drain policy settings.
  PSystemPowerLevel = ^TSystemPowerLevel;
  TSystemPowerLevel = packed record
    Enable: Boolean;
    Spare: packed array[0..2] of UCHAR;
    BatteryLevel: ULONG;
    PowerPolicy: TPowerActionPolicy;
    MinSystemState: TSystemPowerState;
  end;


// The TGlobalUserPowerPolicy structure contains global user power policy
// settings that apply to all power schemes for a user.
  PGlobalUserPowerPolicy = ^TGlobalUserPowerPolicy;
  TGlobalUserPowerPolicy = packed record
    Revision: ULONG;
    PowerButtonAc: TPowerActionPolicy;
    PowerButtonDc: TPowerActionPolicy;
    SleepButtonAc: TPowerActionPolicy;
    SleepButtonDc: TPowerActionPolicy;
    LidCloseAc: TPowerActionPolicy;
    LidCloseDc: TPowerActionPolicy;
    DischargePolicy: array[0..NUM_DISCHARGE_POLICIES] of TSystemPowerLevel;
    GlobalFlags: ULONG;
  end;


// The TGlobalMachinePowerPolicy structure contains global computer
// power policy settings that apply to all power schemes for all users.
  PGlobalMachinePowerPolicy = ^TGlobalMachinePowerPolicy;
  TGlobalMachinePowerPolicy = packed record
    Revision: ULONG;
    LidOpenWakeAc: TSystemPowerState;
    LidOpenWakeDc: TSystemPowerState;
    BroadcastCapacityResolution: ULONG;
  end;


// The TGlobalPowerPolicy structure contains global power policy settings
// that apply to all power schemes.
  PGlobalPowerPolicy = ^TGlobalPowerPolicy;
  TGlobalPowerPolicy = packed record
    user: TGlobalUserPowerPolicy;
    mach: TGlobalMachinePowerPolicy;
  end;


// The TUserPowerPolicy structure contains power policy settings
// that are unique to each power scheme for a user.
  PUserPowerPolicy = ^TUserPowerPolicy;
  TUserPowerPolicy = packed record
    Revision: ULONG;
    IdleAc: TPowerActionPolicy;
    IdleDc: TPowerActionPolicy;
    IdleTimeoutAc: ULONG;
    IdleTimeoutDc: ULONG;
    IdleSensitivityAc: UCHAR;
    IdleSensitivityDc: UCHAR;
    ThrottlePolicyAc: UCHAR;
    ThrottlePolicyDc: UCHAR;
    MaxSleepAc: TSystemPowerState;
    MaxSleepDc: TSystemPowerState;
    Reserved: array[0..1] of ULONG;
    VideoTimeoutAc: ULONG;
    VideoTimeoutDc: ULONG;
    SpindownTimeoutAc: ULONG;
    SpindownTimeoutDc: ULONG;
    OptimizeForPowerAc: Boolean;
    OptimizeForPowerDc: Boolean;
    FanThrottleToleranceAc: UCHAR;
    FanThrottleToleranceDc: UCHAR;
    ForcedThrottleAc: UCHAR;
    ForcedThrottleDc: UCHAR;
  end;


// The TMachinePowerPolicy structure contains computer power policy settings
// that are unique to each power scheme on the computer.
  PMachinePowerPolicy = ^TMachinePowerPolicy;
  TMachinePowerPolicy = packed record
    Revision: ULONG;
    MinSleepAc: TSystemPowerState;
    MinSleepDc: TSystemPowerState;
    ReducedLatencySleepAc: TSystemPowerState;
    ReducedLatencySleepDc: TSystemPowerState;
    DozeTimeoutAc: ULONG;
    DozeTimeoutDc: ULONG;
    DozeS4TimeoutAc: ULONG;
    DozeS4TimeoutDc: ULONG;
    MinThrottleAc: UCHAR;
    MinThrottleDc: UCHAR;
    pad1: packed array[0..1] of UCHAR;
    OverThrottledAc: TPowerActionPolicy;
    OverThrottledDc: TPowerActionPolicy;
  end;


// The TPowerPolicy structure contains power policy settings that are unique
// to each power scheme.
  PPowerPolicy = ^TPowerPolicy;
  TPowerPolicy = packed record
    user: TUserPowerPolicy;
    mach: TMachinePowerPolicy;
  end;


// The TBatteryReportingScale structure reports the remaining capacity
// and the capacity-reporting granularity that the hardware supports.
  PBatteryReportingScale = ^TBatteryReportingScale;
  TBatteryReportingScale = packed record
    Granularity: ULONG;
    Capacity: ULONG;
  end;


// The TSystemPowerInformation structure contains information about
// the idleness of the system.
  PSystemPowerInformation = ^TSystemPowerInformation;
  TSystemPowerInformation = packed record
    MaxIdlenessAllowed: ULONG;
    Idleness: ULONG;
    TimeRemaining: ULONG;
    CoolingMode: UCHAR;
  end;


// The TSystemPowerCapabilities structure contains information
// about the power capabilities of the system.
  PSystemPowerCapabilities = ^TSystemPowerCapabilities;
  TSystemPowerCapabilities = packed record
    PowerButtonPresent: Boolean;
    SleepButtonPresent: Boolean;
    LidPresent: Boolean;
    SystemS1: Boolean;
    SystemS2: Boolean;
    SystemS3: Boolean;
    SystemS4: Boolean;
    SystemS5: Boolean;
    HiberFilePresent: Boolean;
    FullWake: Boolean;
    VideoDimPresent: Boolean;
    ApmPresent: Boolean;
    UpsPresent: Boolean;
    ThermalControl: Boolean;
    ProcessorThrottle: Boolean;
    ProcessorMinThrottle: UCHAR;
    ProcessorMaxThrottle: UCHAR;
    spare2: packed array[0..3] of UCHAR;
    DiskSpinDown: Boolean;
    spare3: packed array[0..7] of UCHAR;
    SystemBatteriesPresent: Boolean;
    BatteriesAreShortTerm: Boolean;
    BatteryScale: array[0..2] of TBatteryReportingScale;
    AcOnLineWake: TSystemPowerState;
    SoftLidWake: TSystemPowerState;
    RtcWake: TSystemPowerState;
    MinDeviceWakeState: TSystemPowerState;
    DefaultLowLatencyWake: TSystemPowerState;
  end;


// The callback function prototype for the EnumPwrSchemes function
  PPwrSchemesEnumProc = ^TPwrSchemesEnumProc;
  TPwrSchemesEnumProc = function(
    uiIndex: UINT;      // power scheme index
    dwName: DWORD;      // size of the sName string, in bytes
    sName: LPTSTR;      // name of the power scheme
    dwDesc: DWORD;      // size of the sDesc string, in bytes
    sDesc: LPTSTR;      // description string
    pp: PPowerPolicy;  // receives the power policy
    lParam: LPARAM      // user-defined value
  ): Boolean;



// The TProcessorPowerInformation structure contains
// information about a processor.
  PProcessorPowerInformation = ^TProcessorPowerInformation;
  TProcessorPowerInformation = packed record
    Number: ULONG;
    MaxMhz: ULONG;
    CurrentMhz: ULONG;
    MhzLimit: ULONG;
    MaxIdleState: ULONG;
    CurrentIdleState: ULONG;
  end;


//==============================================================================
//  Implementation of subroutines
//==============================================================================

// The CallNtPowerInformation function sets or retrieves power information.
function CallNtPowerInformation(InformationLevel: TPowerInformationLevel;
  lpInputBuffer: Pointer; nInputBufferSize: ULONG;
  lpOutputBuffer: Pointer; OutputBufferSize: ULONG): Integer; stdcall;

// The DeletePwrScheme function deletes the specified power scheme.
function DeletePwrScheme(uiIndex: UINT): Bool; stdcall;

// The EnumPwrSchemes function enumerates all power schemes.
// For each power scheme enumerated, the function calls a callback function
// with information about the power scheme.
function EnumPwrSchemes(lpfnPwrSchemesEnumProc: PPwrSchemesEnumProc;
  lParam: LPARAM): Bool; stdcall;

// The GetActivePwrScheme function retrieves the index of the active
// power scheme.
function GetActivePwrScheme(var puiID: UINT): Bool; stdcall;

// The GetCurrentPowerPolicies function retrieves the current system
// power policy settings.
function GetCurrentPowerPolicies(var pGlobalPowerPolicy: TGlobalPowerPolicy;
  var pPowerPolicy: TPowerPolicy): Bool; stdcall;

// The GetPwrCapabilities function retrieves information about the system
// power capabilities.
function GetPwrCapabilities(
  var lpSystemPowerCapabilities: TSystemPowerCapabilities): Bool; stdcall;

// The GetPwrDiskSpindownRange function retrieves the disk spindown range.
function GetPwrDiskSpindownRange(var RangeMax: UINT;
  var RangeMin: UINT): Bool; stdcall;

// The IsPwrHibernateAllowed function determines whether
// the computer supports hibernation.
function IsPwrHibernateAllowed: Bool; stdcall;

// The IsPwrShutdownAllowed function determines whether
// the computer supports the soft off power state.
function IsPwrShutdownAllowed: Bool; stdcall;

// The IsPwrSuspendAllowed function determines whether
// the computer supports the sleep states.
function IsPwrSuspendAllowed: Bool; stdcall;

// The ReadGlobalPwrPolicy function retrieves the current global
// power policy settings.
function ReadGlobalPwrPolicy(
  var pGlobalPowerPolicy: TGlobalPowerPolicy): Bool; stdcall;

// The ReadPwrScheme function retrieves the power policy settings
// that are unique to the specified power scheme.
function ReadPwrScheme(uiID: UINT; var pPowerPolicy: TPowerPolicy): Bool; stdcall;

// The SetActivePwrScheme function sets the active power scheme.
function SetActivePwrScheme(uiID: UINT; lpGlobalPowerPolicy: TGlobalPowerPolicy;
  lpPowerPolicy: TPowerPolicy): Bool; stdcall;

// The SetSuspendState function suspends the system by shutting power down.
// Depending on the Hibernate parameter, the system either
// enters a suspend (sleep) state or hibernation (S4).
// If the ForceFlag parameter is TRUE, the system suspends operation
// immediately; if it is FALSE, the system requests permission from all
// applications and device drivers before doing so.
// If the DisableWakeEvent parameter is TRUE, the system disables all
// wake events. If FALSE, any system wake events remain enabled.
function SetSuspendState(Hibernate: Bool; ForceCritical: Bool;
  DisableWakeEvent: Bool): Bool; stdcall;

// The WriteGlobalPwrPolicy function writes global power policy settings.
function WriteGlobalPwrPolicy(pGlobalPowerPolicy: TGlobalPowerPolicy): Bool; stdcall;

// The WritePwrScheme function writes policy settings that are unique
// to the specified power scheme.
function WritePwrScheme(var puiID: UINT; lpszName: LPTSTR;
  lpszDescription: LPTSTR; pPowerPolicy: TPowerPolicy): Bool; stdcall;

// The SetThreadExecutionState function enables applications to inform
// the system that it is in use, thereby preventing the system
// from entering the sleeping power state while the application is running.
function SetThreadExecutionState(esFlags: DWORD): DWORD; stdcall;

// Safe functions
function SetThreadExecutionState_Safe(Flags: DWORD): DWORD;
function SetSystemPowerState_Safe(fSuspend: Bool; fForce: Bool): Bool;

{******************************************************************************}
implementation
{******************************************************************************}

uses
   SysUtils, abfSysUtils;

const
  powrprof = 'powrprof.dll';


procedure _SetProcAddr(const LibName: string; const ProcName: string;
  var ProcVar: Pointer);
var
  ModuleHandle: THandle;
begin
  if not Assigned(ProcVar) then
  begin
    ModuleHandle := GetModuleHandle(PChar(LibName));
    if ModuleHandle = 0 then
    begin
      ModuleHandle := LoadLibrary(PChar(LibName));
      if ModuleHandle = 0 then
        Abort;
    end;
    ProcVar := GetProcAddress(ModuleHandle, PChar(ProcName));
    if not Assigned(ProcVar) then
      Abort;
  end;
end;

//------------------------------------------------------------------------------

var
  _CallNtPowerInformation: Pointer;

function CallNtPowerInformation;
begin
  _SetProcAddr(powrprof, 'CallNtPowerInformation', _CallNtPowerInformation);
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallNtPowerInformation]
  end;
end;

//------------------------------------------------------------------------------

var
  _DeletePwrScheme: Pointer;

function DeletePwrScheme;
begin
  _SetProcAddr(powrprof, 'DeletePwrScheme', _DeletePwrScheme);
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeletePwrScheme]
  end;
end;

//------------------------------------------------------------------------------

var
  _EnumPwrSchemes: Pointer;

function EnumPwrSchemes;
begin
  _SetProcAddr(powrprof, 'EnumPwrSchemes', _EnumPwrSchemes);
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumPwrSchemes]
  end;
end;

//------------------------------------------------------------------------------

var
  _GetActivePwrScheme: Pointer;

function GetActivePwrScheme;
begin
  _SetProcAddr(powrprof, 'GetActivePwrScheme', _GetActivePwrScheme);
  if not Assigned(_GetActivePwrScheme) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetActivePwrScheme]
  end;
end;

//------------------------------------------------------------------------------

var
  _GetCurrentPowerPolicies: Pointer;

function GetCurrentPowerPolicies;
begin
  _SetProcAddr(powrprof, 'GetCurrentPowerPolicies', _GetCurrentPowerPolicies);
  if not Assigned(_GetCurrentPowerPolicies) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentPowerPolicies]
  end;
end;

//------------------------------------------------------------------------------

var
  _GetPwrCapabilities: Pointer;

function GetPwrCapabilities;
begin
  _SetProcAddr(powrprof, 'GetPwrCapabilities', _GetPwrCapabilities);
  if not Assigned(_GetPwrCapabilities) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPwrCapabilities]
  end;
end;

//------------------------------------------------------------------------------

var
  _GetPwrDiskSpindownRange: Pointer;

function GetPwrDiskSpindownRange;
begin
  _SetProcAddr(powrprof, 'GetPwrDiskSpindownRange', _GetPwrDiskSpindownRange);
  if not Assigned(_GetPwrDiskSpindownRange) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPwrDiskSpindownRange]
  end;
end;

//------------------------------------------------------------------------------

var
  _IsPwrHibernateAllowed: Pointer;

function IsPwrHibernateAllowed;
begin
  _SetProcAddr(powrprof, 'IsPwrHibernateAllowed', _IsPwrHibernateAllowed);
  if not Assigned(_IsPwrHibernateAllowed) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsPwrHibernateAllowed]
  end;
end;

//------------------------------------------------------------------------------

var
  _IsPwrShutdownAllowed: Pointer;

function IsPwrShutdownAllowed;
begin
  _SetProcAddr(powrprof, 'IsPwrShutdownAllowed', _IsPwrShutdownAllowed);
  if not Assigned(_IsPwrShutdownAllowed) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsPwrShutdownAllowed]
  end;
end;

//------------------------------------------------------------------------------

var
  _IsPwrSuspendAllowed: Pointer;

function IsPwrSuspendAllowed;
begin
  _SetProcAddr(powrprof, 'IsPwrSuspendAllowed', _IsPwrSuspendAllowed);
  if not Assigned(_IsPwrSuspendAllowed) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsPwrSuspendAllowed]
  end;
end;

//------------------------------------------------------------------------------

var
  _ReadGlobalPwrPolicy: Pointer;

function ReadGlobalPwrPolicy;
begin
  _SetProcAddr(powrprof, 'ReadGlobalPwrPolicy', _ReadGlobalPwrPolicy);
  if not Assigned(_ReadGlobalPwrPolicy) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadGlobalPwrPolicy]
  end;
end;

//------------------------------------------------------------------------------

var
  _ReadPwrScheme: Pointer;

function ReadPwrScheme;
begin
  _SetProcAddr(powrprof, 'ReadPwrScheme', _ReadPwrScheme);
  if not Assigned(_ReadPwrScheme) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadPwrScheme]
  end;
end;

//------------------------------------------------------------------------------

var
  _SetActivePwrScheme: Pointer;

function SetActivePwrScheme;
begin
  _SetProcAddr(powrprof, 'SetActivePwrScheme', _SetActivePwrScheme);
  if not Assigned(_SetActivePwrScheme) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetActivePwrScheme]
  end;
end;

//------------------------------------------------------------------------------

var
  _SetSuspendState: Pointer;

function SetSuspendState;
begin
  _SetProcAddr(powrprof, 'SetSuspendState', _SetSuspendState);
  if not Assigned(_SetSuspendState) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSuspendState]
  end;
end;

//------------------------------------------------------------------------------

var
  _WriteGlobalPwrPolicy: Pointer;

function WriteGlobalPwrPolicy;
begin
  _SetProcAddr(powrprof, 'WriteGlobalPwrPolicy', _WriteGlobalPwrPolicy);
  if not Assigned(_WriteGlobalPwrPolicy) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteGlobalPwrPolicy]
  end;
end;

//------------------------------------------------------------------------------

var
  _WritePwrScheme: Pointer;

function WritePwrScheme;
begin
  _SetProcAddr(powrprof, 'WritePwrScheme', _WritePwrScheme);
  if not Assigned(_WritePwrScheme) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePwrScheme]
  end;
end;

//------------------------------------------------------------------------------

var
  _SetThreadExecutionState: Pointer;

function SetThreadExecutionState;
begin
  _SetProcAddr(kernel32, 'SetThreadExecutionState', _SetThreadExecutionState);
  if not Assigned(_SetThreadExecutionState) then Exit;
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadExecutionState]
  end;
end;

//------------------------------------------------------------------------------

function SetThreadExecutionState_Safe(Flags: DWORD): DWORD;
begin
  Result := 0;
  if IsWin98orHigher or OSVersion.Check(5) then
    Result := SetThreadExecutionState(Flags);
end;

//------------------------------------------------------------------------------

function SetSystemPowerState_Safe(fSuspend: Bool; fForce: Bool): Bool;
var
  hToken, hProcess: THandle;
  tp, prev_tp: TTokenPrivileges;
  Len: DWORD;
begin
  Result := False;
  if IsWinNT then
  begin
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, True, GetCurrentProcessID);
    try
      if not OpenProcessToken(hProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
        hToken) then Exit;
    finally
      CloseHandle(hProcess);
    end;
    try
      if not LookupPrivilegeValue('', 'SeShutdownPrivilege',
        tp.Privileges[0].Luid) then Exit;
      tp.PrivilegeCount := 1;
      tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      if not AdjustTokenPrivileges(hToken, False, tp, Sizeof(prev_tp),
        prev_tp, Len) then Exit;
    finally
      CloseHandle(hToken);
    end;
  end;
  Result := SetSystemPowerState(fSuspend, fForce);
end;

//------------------------------------------------------------------------------

end{unit abfAPMUtils}.
