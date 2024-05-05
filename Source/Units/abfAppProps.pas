{*******************************************************************************

  ABF Visual Components Library. TabfApplicationProperties component

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfAppProps;

{$I abf.inc}

interface

uses
{$IfDef D5}
  AppEvnts,
{$EndIf D5}
{$IfDef D4}
  ActnList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Menus, Forms,
  abfClasses, abfSysUtils, abfVclUtils;

const
  cDefHintColor = clInfoBk;
  cDefHintPause = 500;
  cDefHintShortPause = cDefHintPause div 10;
  cDefHintHidePause = cDefHintPause * 5;
  cDefMDThreshold = 5;

type

//==============================================================================
// TabfApplicationProperties
//==============================================================================
// Use TabfApplicationProperties to intercept the events or assign properties of
// the global Application object. When you add a TabfApplicationProperties
// object to a form, the Application object forwards all events to the
// TabfApplicationProperties object.

  TabfApplicationProperties = class({$IfDef D5}TCustomApplicationEvents{$Else}TComponent{$EndIf})
  private
    FAbout: string;
    FCanvas: TCanvas;
    FChained: Boolean;
    FHelpFile: string;
    FHidden: Boolean;
    FHintColor: TColor;
    FHintHidePause: Integer;
    FHintPause: Integer;
    FHintShortPause: Integer;
    FTitle: string;
    FShowHint: Boolean;
    FShowMainForm: Boolean;
    FUpdateFormatSettings: Boolean;
{$IfDef D3}
    FUpdateMetricSettings: Boolean;
{$EndIf D3}
{$IfDef D4}
    FBiDiMode: TBiDiMode;
    FHintShortCuts: Boolean;
    FMouseDragImmediate: Boolean;
    FMouseDragThreshold: Integer;
{$EndIf D4}
{$IfDef D5}
    FBiDiKeyboard: string;
    FNonBiDiKeyboard: string;
{$EndIf D5}
    FOnPaintIcon: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnMessage: TMessageEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShowHint: TShowHintEvent;
    FOnSettingsChanged: TNotifyEvent;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
{$IfDef D4}
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnShortCut: TShortCutEvent;
{$EndIf D4}
  // Properties Get/Set
    function  GetCanvas: TCanvas;
    function  GetHelpFile: TFileName;
    procedure SetHelpFile(const A: TFileName);
    procedure SetHidden(A: Boolean);
    function  GetHintColor: TColor;
    procedure SetHintColor(A: TColor);
    function  GetHintHidePause: Integer;
    procedure SetHintHidePause(A: Integer);
    function  GetHintPause: Integer;
    procedure SetHintPause(A: Integer);
    function  GetHintShortPause: Integer;
    procedure SetHintShortPause(A: Integer);
    function  GetIcon: TIcon;
    procedure SetIcon(const A: TIcon);
    function  GetShowHint: Boolean;
    procedure SetShowHint(A: Boolean);
    function  GetShowMainForm: Boolean;
    procedure SetShowMainForm(A: Boolean);
    function  GetTitle: string;
    procedure SetTitle(const A: string);
    function  GetUpdateFormatSettings: Boolean;
    procedure SetUpdateFormatSettings(A: Boolean);
{$IfDef D3}
    function  GetIconFont: TFont;
    procedure SetIconFont(const A: TFont);
    function  GetUpdateMetricSettings: Boolean;
    procedure SetUpdateMetricSettings(A: Boolean);
{$EndIf D3}
{$IfDef D4}
    function  GetBiDiMode: TBiDiMode;
    procedure SetBiDiMode(A: TBiDiMode);
    function  GetHintShortCuts: Boolean;
    procedure SetHintShortCuts(A: Boolean);
    function  GetMouseDragImmediate: Boolean;
    procedure SetMouseDragImmediate(A: Boolean);
    function  GetMouseDragThreshold: Integer;
    procedure SetMouseDragThreshold(A: Integer);
{$EndIf D4}
{$IfDef D5}
    function  GetBiDiKeyboard: string;
    procedure SetBiDiKeyboard(const A: string);
    function  GetHintFont: TFont;
    procedure SetHintFont(const A: TFont);
    function  GetNonBiDiKeyboard: string;
    procedure SetNonBiDiKeyboard(const A: string);
{$EndIf D5}
  protected
    FIcon: TIcon;  // The application icon design-time object.
    procedure Loaded; override;
    procedure UpdateProperties; virtual;
    procedure UpdateHidden; virtual;
{$IfDef D3}
    procedure UpdateIconFont; virtual;
{$EndIf D3}
{$IfDef D5}
    procedure UpdateHintFont; virtual;
{$EndIf D5}
    procedure PaintIcon; virtual;
    procedure SettingsChanged; dynamic;
    function  MessageHook(var Msg: TMessage): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
{$IfDef D3}
    property IconFont: TFont read GetIconFont write SetIconFont;
{$EndIf D3}
{$IfDef D5}
    property HintFont: TFont read GetHintFont write SetHintFont;
{$EndIf D5}
  published
  // Properties
    property About: string read FAbout write FAbout stored False;
    property Chained: Boolean read FChained write FChained default True;
    property HelpFile: TFileName read GetHelpFile write SetHelpFile;
    property Hidden: Boolean read FHidden write SetHidden default False;
    property HintColor: TColor read GetHintColor write SetHintColor
      default cDefHintColor;
    property HintHidePause: Integer read GetHintHidePause
      write SetHintHidePause default cDefHintHidePause;
    property HintPause: Integer read GetHintPause write SetHintPause
      default cDefHintPause;
    property HintShortPause: Integer read GetHintShortPause
      write SetHintShortPause default cDefHintShortPause;
    property Icon: TIcon read GetIcon write SetIcon;
    property Title: string read GetTitle write SetTitle;
    property ShowHint: Boolean read GetShowHint write SetShowHint default True;
    property UpdateFormatSettings: Boolean read GetUpdateFormatSettings
      write SetUpdateFormatSettings default True;
    property ShowMainForm: Boolean read GetShowMainForm write SetShowMainForm
      default True;
{$IfDef D3}
    property UpdateMetricSettings: Boolean read GetUpdateMetricSettings
      write SetUpdateMetricSettings default True;
{$EndIf D3}
{$IfDef D4}
    property BiDiMode: TBiDiMode read GetBiDiMode write SetBiDiMode
      default bdLeftToRight;
    property HintShortCuts: Boolean read GetHintShortCuts write SetHintShortCuts
      default True;
    property MouseDragImmediate: Boolean read GetMouseDragImmediate
      write SetMouseDragImmediate default True;
    property MouseDragThreshold: Integer read GetMouseDragThreshold
      write SetMouseDragThreshold default cDefMDThreshold;
{$EndIf D4}
{$IfDef D5}
    property BiDiKeyboard: string read GetBiDiKeyboard write SetBiDiKeyboard;
    property NonBiDiKeyboard: string read GetNonBiDiKeyboard
      write SetNonBiDiKeyboard;
{$EndIf D5}
  // Events
    property OnPaintIcon: TNotifyEvent read FOnPaintIcon write FOnPaintIcon;
    property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged
      write FOnSettingsChanged;
    property OnActiveControlChange: TNotifyEvent read FOnActiveControlChange
      write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent read FOnActiveFormChange
      write FOnActiveFormChange;
{$IfDef D5}
    property OnActionExecute;
    property OnActionUpdate;
    property OnActivate;
    property OnDeactivate;
    property OnException;
    property OnIdle;
    property OnHelp;
    property OnHint;
    property OnMessage;
    property OnMinimize;
    property OnRestore;
    property OnShowHint;
    property OnShortCut;
{$Else D5}
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
  {$IfDef D4}
    property OnActionExecute: TActionEvent read FOnActionExecute
      write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate
      write FOnActionUpdate;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
  {$EndIf D4}
{$EndIf D5}
  end;{TabfApplicationProperties = ...}

{******************************************************************************}
implementation
{******************************************************************************}

uses
  Consts, abfConsts;

//==============================================================================
// TabfAppPropList
//==============================================================================
// List of TabfApplicationProperties and it descendants, used internally.
// Date: 07/15/2000

type
  TabfAppPropList = class(TObject)
  private
    FAppProps: TList;
    FHooked: Boolean;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnMessage: TMessageEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShowHint: TShowHintEvent;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    FOnIconFontChange: TNotifyEvent;
{$IfDef D4}
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnShortCut: TShortCutEvent;
{$EndIf D4}
{$IfDef D5}
    FOnHintFontChange: TNotifyEvent;
{$EndIf D5}
    function GetCount: Integer;
  protected
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    function DoHelp(Command: Word; Data: Longint;
      var CallHelp: Boolean): Boolean;
    procedure DoHint(Sender: TObject);
    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure DoActiveControlChange(Sender: TObject);
    procedure DoActiveFormChange(Sender: TObject);
{$IfDef D3}
    procedure DoIconFontChange(Sender: TObject);
{$EndIf D3}
{$IfDef D4}
    procedure DoActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DoShortCut(var Msg: TWMKey; var Handled: Boolean);
{$EndIf D4}
{$IfDef D5}
    procedure DoHintFontChange(Sender: TObject);
{$EndIf D5}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(App: TabfApplicationProperties);
    procedure Remove(App: TabfApplicationProperties);
    procedure ClearEvents;
  { Properties }
    property Count: Integer read GetCount;
  end;{TabfAppPropList = class(TObject)}

var
  _AppPropList: TabfAppPropList = nil;

//==============================================================================
// TabfAppPropList
//==============================================================================
// Internal list
{ TabfAppPropList }

constructor TabfAppPropList.Create;
begin
  FAppProps := TList.Create;
  inherited Create;
end;

//------------------------------------------------------------------------------

destructor TabfAppPropList.Destroy;
begin
  ClearEvents;
  inherited Destroy;
  FreeAndNil(FAppProps);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.Add(App: TabfApplicationProperties);
begin
  if not Assigned(App) or (FAppProps.IndexOf(App) >= 0) then Exit;
  FAppProps.Add(App);
  if (csDesigning in App.ComponentState) or (FAppProps.Count <> 1) then Exit;
  FOnActivate := Application.OnActivate;
  FOnDeactivate := Application.OnDeactivate;
  FOnException := Application.OnException;
  FOnIdle := Application.OnIdle;
  FOnHelp := Application.OnHelp;
  FOnHint := Application.OnHint;
  FOnMessage := Application.OnMessage;
  FOnMinimize := Application.OnMinimize;
  FOnRestore := Application.OnRestore;
  FOnShowHint := Application.OnShowHint;
{$IfDef D4}
  FOnActionExecute := Application.OnActionExecute;
  FOnActionUpdate := Application.OnActionUpdate;
  FOnShortCut := Application.OnShortCut;
  Application.OnActionExecute := DoActionExecute;
  Application.OnActionUpdate := DoActionUpdate;
  Application.OnShortCut := DoShortCut;
{$EndIf D4}
  Application.OnActivate := DoActivate;
  Application.OnDeactivate := DoDeactivate;
  Application.OnException := DoException;
  Application.OnIdle := DoIdle;
  Application.OnHelp := DoHelp;
  Application.OnHint := DoHint;
  Application.OnMessage := DoMessage;
  Application.OnMinimize := DoMinimize;
  Application.OnRestore := DoRestore;
  Application.OnShowHint := DoShowHint;
  if Assigned(Screen) then
  begin
    FOnActiveControlChange := Screen.OnActiveControlChange;
    FOnActiveFormChange := Screen.OnActiveFormChange;
    Screen.OnActiveControlChange := DoActiveControlChange;
    Screen.OnActiveFormChange := DoActiveFormChange;
{$IfDef D3}
    FOnIconFontChange := Screen.IconFont.OnChange;
    Screen.IconFont.OnChange := DoIconFontChange;
{$EndIf D3}
{$IfDef D5}
    FOnHintFontChange := Screen.HintFont.OnChange;
    Screen.HintFont.OnChange := DoHintFontChange;
{$EndIf D5}
  end;
  FHooked := True;
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.Remove(App: TabfApplicationProperties);
begin
  if FAppProps.IndexOf(App) >= 0 then FAppProps.Remove(App);
  if not (csDesigning in App.ComponentState) and (FAppProps.Count = 0) then
    ClearEvents;
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.ClearEvents;
begin
  if not FHooked then Exit;
  Application.OnActivate := FOnActivate;
  Application.OnDeactivate := FOnDeactivate;
  Application.OnException := FOnException;
  Application.OnIdle := FOnIdle;
  Application.OnHelp := FOnHelp;
  Application.OnHint := FOnHint;
  Application.OnMessage := FOnMessage;
  Application.OnMinimize := FOnMinimize;
  Application.OnRestore := FOnRestore;
  Application.OnShowHint := FOnShowHint;
{$IfDef D4}
  Application.OnActionExecute := FOnActionExecute;
  Application.OnActionUpdate := FOnActionUpdate;
  Application.OnShortCut := FOnShortCut;
{$EndIf D4}
  if Assigned(Screen) then
  begin
    Screen.OnActiveControlChange := FOnActiveControlChange;
    Screen.OnActiveFormChange := FOnActiveFormChange;
{$IfDef D3}
    Screen.IconFont.OnChange := FOnIconFontChange;
{$EndIf D3}
{$IfDef D5}
    Screen.HintFont.OnChange := FOnHintFontChange;
{$EndIf D5}
  end;
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoActivate(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
    // Update if Application is hidden
//      UpdateHidden;

    // Process event
      if Assigned(FOnActivate) then FOnActivate(Sender);
      if not Chained then Exit;
    end;
  if Assigned(FOnActivate) then FOnActivate(Sender);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoDeactivate(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnDeactivate) then FOnDeactivate(Sender);
      if not Chained then Exit;
    end;
  if Assigned(FOnDeactivate) then FOnDeactivate(Sender);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoException(Sender: TObject; E: Exception);
var
  i: Integer;
  Handled: Boolean;
begin
  Handled := False;
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnException) then
      begin
        FOnException(Sender, E);
        Handled := True;
      end;
      if not Chained then
      begin
        if not Handled then Application.ShowException(E);
        Exit;
      end;
    end;{with TabfApplicationProperties(FAppProps[i]) do}

  if Assigned(FOnException) then
  begin
    FOnException(Sender, E);
    Handled := True;
  end;
  if not Handled then Application.ShowException(E);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoIdle(Sender: TObject; var Done: Boolean);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnIdle) then FOnIdle(Sender, Done);
      if not Chained then Exit;
    end;
  if Assigned(FOnIdle) then FOnIdle(Sender, Done);
end;

//------------------------------------------------------------------------------

function TabfAppPropList.DoHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnHelp) then FOnHelp(Command, Data, CallHelp);
      if not Chained then Exit;
    end;
  if Assigned(FOnHelp) then Result := FOnHelp(Command, Data, CallHelp);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoHint(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnHint) then FOnHint(Sender);
      if not Chained then Exit;
    end;
  if Assigned(FOnHint) then FOnHint(Sender);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoMessage(var Msg: TMsg; var Handled: Boolean);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
      if not Chained then Exit;
    end;
  if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoMinimize(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnMinimize) then FOnMinimize(Sender);
      if not Chained then Exit;
    end;
  if Assigned(FOnMinimize) then FOnMinimize(Sender);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoRestore(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnRestore) then FOnRestore(Sender);
      if not Chained then Exit;
    end;
  if Assigned(FOnRestore) then FOnRestore(Sender);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
      if not Chained then Exit;
    end;
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoActiveControlChange(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnActiveControlChange) then FOnActiveControlChange(Sender);
      if not Chained then Exit;
    end;
  if Assigned(FOnActiveControlChange) then FOnActiveControlChange(Sender);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoActiveFormChange(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnActiveFormChange) then FOnActiveFormChange(Sender);
      if not Chained then Exit;
    end;
  if Assigned(FOnActiveFormChange) then FOnActiveFormChange(Sender);
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfAppPropList.DoHintFontChange(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      UpdateHintFont;
      if not Chained then Exit;
    end;
  if Assigned(FOnHintFontChange) then FOnHintFontChange(Sender);
end;

{$EndIf D5}
//------------------------------------------------------------------------------
{$IfDef D3}

procedure TabfAppPropList.DoIconFontChange(Sender: TObject);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      UpdateIconFont;
      if not Chained then Exit;
    end;
  if Assigned(FOnIconFontChange) then FOnIconFontChange(Sender);
end;

{$EndIf D5}
//------------------------------------------------------------------------------
{$IfDef D4}

procedure TabfAppPropList.DoActionExecute(Action: TBasicAction;
  var Handled: Boolean);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnActionExecute) then FOnActionExecute(Action, Handled);
      if not Chained then Exit;
    end;
  if Assigned(FOnActionExecute) then FOnActionExecute(Action, Handled);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoActionUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnActionUpdate) then FOnActionUpdate(Action, Handled);
      if not Chained then Exit;
    end;
  if Assigned(FOnActionUpdate) then FOnActionUpdate(Action, Handled);
end;

//------------------------------------------------------------------------------

procedure TabfAppPropList.DoShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  i: Integer;
begin
  for i := FAppProps.Count - 1 downto 0 do
    with TabfApplicationProperties(FAppProps[i]) do
    begin
      if Assigned(FOnShortCut) then FOnShortCut(Msg, Handled);
      if not Chained then Exit;
    end;
  if Assigned(FOnShortCut) then FOnShortCut(Msg, Handled);
end;

{$EndIf D4}
//------------------------------------------------------------------------------

function TabfAppPropList.GetCount: Integer;
begin
  Result := FAppProps.Count;
end;


//==============================================================================
// TabfApplicationProperties
//==============================================================================
// Use TabfApplicationProperties to intercept the events or assign properties of
// the global Application object. When you add a TabfApplicationProperties
// object to a form, the Application object forwards all events to the
// TabfApplicationProperties object.
// Date: 07/15/2000
{ TabfApplicationProperties }

constructor TabfApplicationProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
// Create the list if it was not used before
  if not Assigned(_AppPropList) then _AppPropList := TabfAppPropList.Create;
  FChained := True;
  FHintColor := cDefHintColor;
  FHintHidePause := cDefHintHidePause;
  FHintPause := cDefHintPause;
  FHintShortPause := cDefHintShortPause;
  FShowHint := True;
  FShowMainForm := True;
{$IfDef D3}
  FUpdateMetricSettings := True;
{$EndIf}
{$IfDef D4}
  FHintShortCuts := True;
  FBiDiMode := bdLeftToRight;
  FMouseDragImmediate := True;
  FMouseDragThreshold := cDefMDThreshold;
{$EndIf}
  FUpdateFormatSettings := True;
  if (csDesigning in ComponentState) then
  begin
    FIcon := TIcon.Create;
  end else
    Application.HookMainWindow(MessageHook);
  _AppPropList.Add(Self);
end;

//------------------------------------------------------------------------------

destructor TabfApplicationProperties.Destroy;
begin
  if (csDesigning in ComponentState) then
  begin
    FreeAndNil(FIcon);
  end else
    Application.UnhookMainWindow(MessageHook);
  if Self <> nil then _AppPropList.Remove(Self);
  if Assigned(FCanvas) then FreeAndNil(FCanvas);
  inherited Destroy;

// Free the list if it is empty
  if _AppPropList.Count < 1 then FreeAndNil(_AppPropList);
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.Loaded;
begin
  inherited Loaded;
  UpdateProperties;
  if Hidden then UpdateHidden;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.UpdateProperties;
begin
  if (csDesigning in ComponentState) then Exit;
  with Application do
  begin
    HintColor := FHintColor;
    HintHidePause := FHintHidePause;
    HintPause := FHintPause;
    HintShortPause := FHintShortPause;
    ShowMainForm := FShowMainForm;
    ShowHint := FShowHint;
    UpdateFormatSettings := FUpdateFormatSettings;
{$IfDef D3}
    UpdateMetricSettings := FUpdateMetricSettings;
{$EndIf}
{$IfDef D4}
    HintShortCuts := FHintShortCuts;
    BiDiMode := FBiDiMode;
    with Mouse do
    begin
      DragImmediate := FMouseDragImmediate;
      DragThreshold := FMouseDragThreshold;
    end;
{$EndIf}
{$IfDef D5}
    BiDiKeyboard := FBiDiKeyboard;
    NonBiDiKeyboard := FNonBiDiKeyboard;
{$EndIf}
  end;
end;{procedure TabfApplicationProperties.UpdateProperties}

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.UpdateHidden;
begin
  if (csDesigning in ComponentState) then Exit;
  with Application do
    if IsWindow(Handle) then
      if Hidden then
      begin
      // Hide TaskBar button
        abfApplicationTaskBarButtonHide;
      // Remove WS_EX_APPWINDOW
        SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE)
          and (not WS_EX_APPWINDOW) or WS_EX_TOOLWINDOW);
      end else
      begin
      // Restore WS_EX_APPWINDOW
        SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE)
          and (not WS_EX_TOOLWINDOW) or WS_EX_APPWINDOW);
      // Show TaskBar button 
        abfApplicationTaskBarButtonShow;
      end;
end;{procedure TabfApplicationProperties.UpdateHidden}

//------------------------------------------------------------------------------
{$IfDef D3}

procedure TabfApplicationProperties.UpdateIconFont;
begin
  // Do nothing.
end;

{$EndIf D3}
//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfApplicationProperties.UpdateHintFont;
var
  i: Integer;
begin
  for i := 0 to Application.ComponentCount - 1 do
    if Application.Components[i] is THintWindow then
    begin
      THintWindow(Application.Components[i]).Canvas.Font := HintFont;
//      Exit;
    end;
end;

{$EndIf D5}
//------------------------------------------------------------------------------

procedure TabfApplicationProperties.PaintIcon;
var
  PS: TPaintStruct;
begin
  BeginPaint(Application.Handle, PS);
  try
    if FCanvas <> nil then FreeAndNil(FCanvas);
    FCanvas := TCanvas.Create;
    try
      Canvas.Handle := PS.hDC;
      Canvas.Brush.Color := clBackground;
      if PS.fErase then Canvas.FillRect(PS.rcPaint);
      if Assigned(FOnPaintIcon) then FOnPaintIcon(Self);
    finally
      FreeAndNil(FCanvas);
    end;
  finally
    EndPaint(Application.Handle, PS);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SettingsChanged;
begin
  if Assigned(FOnSettingsChanged) then FOnSettingsChanged(Self);
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.MessageHook(var Msg: TMessage): Boolean;
begin
  Result := False;
  case Msg.Msg of
    WM_WININICHANGE: begin
{$IfNDef D3}
      if Application.ShowHint then
      begin
        Application.ShowHint := False;
        Application.ShowHint := True;
      end;
{$EndIf D3}
      try
        SettingsChanged;
      except
        Application.HandleException(Self);
      end;
    end;{WM_WININICHANGE}
    WM_PAINT: begin
      if Assigned(FOnPaintIcon) and IsIconic(Application.Handle) then
      begin
        PaintIcon;
        Result := True;
      end;
    end;{WM_PAINT}
  end;
end;{function TabfApplicationProperties.MessageHook}


//==============================================================================
// Properties Get/Set

function TabfApplicationProperties.GetCanvas: TCanvas;
begin
  if FCanvas = nil then FCanvas := TCanvas.Create;
  Result := FCanvas;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetHelpFile: TFileName;
begin
  if (csDesigning in ComponentState) then Result := FHelpFile
  else Result := Application.HelpFile;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHelpFile(const A: TFileName);
begin
  if HelpFile = A then Exit;
  if (csDesigning in ComponentState) then FHelpFile := A
  else Application.HelpFile := A;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHidden(A: Boolean);
begin
  FHidden := A;
  UpdateHidden;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetHintColor: TColor;
begin
  if (csDesigning in ComponentState) then Result := FHintColor
  else Result := Application.HintColor;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHintColor(A: TColor);
begin
  FHintColor := A;
  if not (csDesigning in ComponentState) then Application.HintColor := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetHintHidePause: Integer;
begin
  if (csDesigning in ComponentState) then Result := FHintHidePause
  else Result := Application.HintHidePause;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHintHidePause(A: Integer);
begin
  FHintHidePause := A;
  if not (csDesigning in ComponentState) then Application.HintHidePause := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetHintPause: Integer;
begin
  if (csDesigning in ComponentState) then Result := FHintPause
  else Result := Application.HintPause;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHintPause(A: Integer);
begin
  FHintPause := A;
  if not (csDesigning in ComponentState) then Application.HintPause := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetHintShortPause: Integer;
begin
  if (csDesigning in ComponentState) then Result := FHintShortPause
  else Result := Application.HintShortPause;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHintShortPause(A: Integer);
begin
  FHintShortPause := A;
  if not (csDesigning in ComponentState) then Application.HintShortPause := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetIcon: TIcon;
begin
  if (csDesigning in ComponentState) then Result := FIcon
  else Result := Application.Icon;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetIcon(const A: TIcon);
begin
  if A = Icon then Exit;
  if (csDesigning in ComponentState) then FIcon.Assign(A)
  else Application.Icon := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetShowHint: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FShowHint
  else Result := Application.ShowHint;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetShowHint(A: Boolean);
begin
  FShowHint := A;
  if not (csDesigning in ComponentState) then Application.ShowHint := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetShowMainForm: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FShowMainForm
  else Result := Application.ShowMainForm;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetShowMainForm(A: Boolean);
begin
  FShowMainForm := A;
  if not (csDesigning in ComponentState) then Application.ShowMainForm := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetTitle: string;
begin
  if (csDesigning in ComponentState) then Result := FTitle
  else Result := Application.Title;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetTitle(const A: string);
begin
  if A = Title then Exit;
  if (csDesigning in ComponentState) then FTitle := A
  else Application.Title := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetUpdateFormatSettings: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FUpdateFormatSettings
  else Result := Application.UpdateFormatSettings;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetUpdateFormatSettings(A: Boolean);
begin
  FUpdateFormatSettings := A;
  if not (csDesigning in ComponentState) then
    Application.UpdateFormatSettings := A;
end;

//------------------------------------------------------------------------------
{$IfDef D3}

function TabfApplicationProperties.GetIconFont: TFont;
begin
  Result := Screen.IconFont;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetIconFont(const A: TFont);
begin
  if A = IconFont then Exit;
  Screen.IconFont := A;
  UpdateIconFont;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetUpdateMetricSettings: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FUpdateMetricSettings
  else Result := Application.UpdateMetricSettings;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetUpdateMetricSettings(A: Boolean);
begin
  FUpdateMetricSettings := A;
  if not (csDesigning in ComponentState) then
    Application.UpdateMetricSettings := A;
end;

{$EndIf D3}
//------------------------------------------------------------------------------
{$IfDef D4}

function TabfApplicationProperties.GetBiDiMode: TBiDiMode;
begin
  if (csDesigning in ComponentState) then Result := FBiDiMode
  else Result := Application.BiDiMode;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetBiDiMode(A: TBiDiMode);
begin
  FBiDiMode := A;
  if not (csDesigning in ComponentState) then Application.BiDiMode := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetHintShortCuts: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FHintShortCuts
  else Result := Application.HintShortCuts;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHintShortCuts(A: Boolean);
begin
  FHintShortCuts := A;
  if not (csDesigning in ComponentState) then Application.HintShortCuts := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetMouseDragImmediate: Boolean;
begin
  if (csDesigning in ComponentState) or (Mouse = nil) then
    Result := FMouseDragImmediate
  else Result := Mouse.DragImmediate;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetMouseDragImmediate(A: Boolean);
begin
  FMouseDragImmediate := A;
  if not (csDesigning in ComponentState) and (Mouse <> nil) then
    Mouse.DragImmediate := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetMouseDragThreshold: Integer;
begin
  if (csDesigning in ComponentState) or (Mouse = nil) then
    Result := FMouseDragThreshold
  else Result := Mouse.DragThreshold;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetMouseDragThreshold(A: Integer);
begin
  FMouseDragThreshold := A;
  if not (csDesigning in ComponentState) and (Mouse <> nil) then
    Mouse.DragThreshold := A;
end;

{$EndIf D4}
//------------------------------------------------------------------------------
{$IfDef D5}

function TabfApplicationProperties.GetBiDiKeyboard: string;
begin
  if (csDesigning in ComponentState) then Result := FBiDiKeyboard
  else Result := Application.BiDiKeyboard;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetBiDiKeyboard(const A: string);
begin
  FBiDiKeyboard := A;
  if not (csDesigning in ComponentState) then Application.BiDiKeyboard := A;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetHintFont: TFont;
begin
  Result := Screen.HintFont;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetHintFont(const A: TFont);
begin
  if A = HintFont then Exit;
  Screen.HintFont := A;
  UpdateHintFont;
end;

//------------------------------------------------------------------------------

function TabfApplicationProperties.GetNonBiDiKeyboard: string;
begin
  if (csDesigning in ComponentState) then Result := FNonBiDiKeyboard
  else Result := Application.NonBiDiKeyboard;
end;

//------------------------------------------------------------------------------

procedure TabfApplicationProperties.SetNonBiDiKeyboard(const A: string);
begin
  FNonBiDiKeyboard := A;
  if not (csDesigning in ComponentState) then Application.NonBiDiKeyboard := A;
end;

{$EndIf D5}
//------------------------------------------------------------------------------


{******************************************************************************}
initialization
{******************************************************************************}

{*******************************************************************************
 The trick for the Borland bug in the AppEvnts.pas unit (Delphi 5). They create
a TMultiCaster internal class instance on initialization of unit (not on the
first usage). TMultiCaster properly work only when at least one instance of
TCustomApplicationEvents or it descendant is existed. So create one of it to
make all happy.

Note: Don't add AppEvnts unit to the uses if you will not create at least one
instance of TCustomApplicationEvents or it descendant! Actions and application
events will be lost, AutoHint of TStausBar will not working.
*******************************************************************************}
{$IfDef D5}
  TApplicationEvents.Create(Application);
{$EndIf D5}

//------------------------------------------------------------------------------
// Commented due to dll problems.
  abfRegisterClasses([TabfApplicationProperties]);

{******************************************************************************}
finalization
{******************************************************************************}
  if Assigned(_AppPropList) then FreeAndNil(_AppPropList);

end{unit abfAppProps}.
