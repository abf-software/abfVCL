{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfIdentityManager;

{$I abf.inc}

interface

uses
  Windows, Forms, Classes, Controls, {$IfDef D5}Contnrs, {$EndIf D5}SysUtils, 
  abfClasses, abfComponents,
  msident;


type
//==============================================================================
//  Forward declaration
//==============================================================================
  TabfIdentityManager = class;

//==============================================================================
//  TabfUserIdentity
//==============================================================================
  TabfUserIdentity = class(TPersistent)
  private
    FID: TGUID;
    FIntf: IUnknown;
    FUser: IUserIdentity;
    FUser2: IUserIdentity2;
  protected
    function CheckUser: Boolean; virtual;
    function CheckUser2: Boolean; virtual;
    function GetName: WideString; virtual;
    function GetFolder: WideString; virtual;
    function GetID: TGUID; virtual;
    function GetOrdinal: Cardinal; virtual;
    procedure SetName(const AValue: WideString); virtual;
  public
    constructor Create(AUnkPtr: Pointer); virtual;
    destructor Destroy; override;
    function ChangePassword(const AOldPassword,
      ANewPassword: WideString): Boolean; virtual;
    property Folder: WideString read GetFolder;
    property ID: TGUID read GetID;
    property Name: WideString read GetName write SetName;
    property Ordinal: Cardinal read GetOrdinal;    
  end;

//==============================================================================
//  TabfIdentityChangeNotify
//==============================================================================
  TabfIdentityChangeNotify = class(TInterfacedObject, IIdentityChangeNotify)
  private
    FOwner: TabfIdentityManager;
  protected
	  function QuerySwitchIdentities: HRESULT; stdcall;
    function SwitchIdentities: HRESULT; stdcall;
    function IdentityInformationChanged(dwType: DWORD): HRESULT; stdcall;
  public
    constructor Create(AOwner: TabfIdentityManager);
  end;

//==============================================================================
//  TabfIdentityManager
//==============================================================================

  TabfQuerySwitchIdentitiesEvent = procedure(Sender: TObject;
    AOutgoingIdentity: TabfUserIdentity; AIncomingIdentity: TabfUserIdentity;
    var AllowChange: Boolean) of object;

  TabfIdentityManager = class(TabfComponent)
  private
    FActive: Boolean;
    FCallbackIntf: IIdentityChangeNotify;
    FCallbackObject: TabfIdentityChangeNotify;
    FHookID: Integer;
    FIdentities: TObjectList;
    FLoading: Boolean;
    FManager: IUserIdentityManager;
    FOnChangeIdentities: TNotifyEvent;
    FOnCurrentIdentityChanged: TNotifyEvent;
    FOnDeleteIdentity: TNotifyEvent;
    FOnIdentityChanged: TNotifyEvent;
	  FOnQuerySwitchIdentities: TabfQuerySwitchIdentitiesEvent;
    FOnNewIdentity: TNotifyEvent;
    FOnSwitchIdentities: TNotifyEvent;
    FPrivManager: IPrivateIdentityManager;
    FPrivManager2: IPrivateIdentityManager2;
    FStreamedActive: Boolean;
    procedure SetActive(AValue: Boolean);
  protected
    function CheckPrivManager: Boolean; virtual;
    function CheckPrivManager2: Boolean; virtual;
    function GetCurrentIdentity: TabfUserIdentity; virtual;
    function GetDefaultIdentity: TabfUserIdentity; virtual;
    function GetIdentity(AIndex: Integer): TabfUserIdentity; virtual;
    function GetIdentityCount: Integer; virtual;
    function GetIdentityBySpecialID(AID: TGUID): TabfUserIdentity; virtual;
    procedure DoChangeIdentities; virtual;
	  procedure DoQuerySwitchIdentities(var AllowChange: Boolean); virtual;
    procedure DoIdentityInformationChanged(dwType: DWORD); virtual;
    procedure DoSwitchIdentities; virtual;
    procedure Loaded; override;
    procedure LoadIdentities; virtual;
    procedure LoadManager; virtual;
    procedure UnloadManager; virtual;
    procedure HookManager; virtual;
    procedure UnhookManager; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateIdentity(const AName: WideString;
      const APassword: WideString): TabfUserIdentity; virtual;
    function DestroyIdentity(AID: TGUID): Boolean; virtual;
    function GetIdentityByID(AID: TGUID): TabfUserIdentity; virtual;
    function Logoff: Boolean; virtual;
    function Logon(const AName: WideString;
      const APassword: WideString): TabfUserIdentity; virtual;
    function ManageIdentities(ACreateNew: Boolean): Boolean; virtual;
    function SetDefaultIdentity(AID: TGUID): TabfUserIdentity; virtual;
    procedure Refresh; virtual;
    property CurrentIdentity: TabfUserIdentity read GetCurrentIdentity;
    property DefaultIdentity: TabfUserIdentity read GetDefaultIdentity;
    property IdentityCount: Integer read GetIdentityCount;
    property Identities[AIndex: Integer]: TabfUserIdentity read GetIdentity;
  published
    property About;
    property Active: Boolean read FActive write SetActive default False;
    property OnChangeIdentities: TNotifyEvent read FOnChangeIdentities
      write FOnChangeIdentities;
    property OnCurrentIdentityChanged: TNotifyEvent read FOnCurrentIdentityChanged
      write FOnCurrentIdentityChanged;
    property OnDeleteIdentity: TNotifyEvent read FOnDeleteIdentity
      write FOnDeleteIdentity;
    property OnIdentityChanged: TNotifyEvent read FOnIdentityChanged
      write FOnIdentityChanged;
	  property OnQuerySwitchIdentities: TabfQuerySwitchIdentitiesEvent
      read FOnQuerySwitchIdentities write FOnQuerySwitchIdentities;
    property OnNewIdentity: TNotifyEvent read FOnNewIdentity
      write FOnNewIdentity;
    property OnSwitchIdentities: TNotifyEvent read FOnSwitchIdentities
      write FOnSwitchIdentities;
  end;

//==============================================================================
// EabfWABException
//==============================================================================

  EabfIdentityManagerException = class(EabfException);

{******************************************************************************}
implementation
{******************************************************************************}

uses
  ComObj, ActiveX,
  abfSysUtils,
  abfIdentityManagerConsts;

{$I abf_init.inc}


//==============================================================================
//  TabfUserIdentity
//==============================================================================

constructor TabfUserIdentity.Create(AUnkPtr: Pointer);
begin
  FillChar(FID, SizeOf(FID), 0);
  FIntf := IUnknown(AUnkPtr);
end;

//------------------------------------------------------------------------------

destructor TabfUserIdentity.Destroy;
begin
  FUser2 := nil;
  FUser := nil;
  FIntf := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfUserIdentity.CheckUser: Boolean;
begin
  Result := Assigned(FUser);
  if Result then Exit;

  try
    if not Assigned(FIntf) then Exit;

    FIntf.QueryInterface(IID_IUserIdentity, FUser);
    Result := Assigned(FUser);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfUserIdentity.CheckUser2: Boolean;
begin
  Result := Assigned(FUser2);
  if Result then Exit;

  try
    if not Assigned(FIntf) then Exit;

    FIntf.QueryInterface(IID_IUserIdentity2, FUser2);
    Result := Assigned(FUser2);
  except
  end;
end;

//------------------------------------------------------------------------------

function TabfUserIdentity.GetName: WideString;
var
  Buf: array[0..CCH_IDENTITY_NAME_MAX_LENGTH] of WideChar;
begin
  Result := '';
  FillChar(Buf, SizeOf(Buf), 0);

  if not CheckUser then Exit;

  FUser.GetName(@Buf, SizeOf(Buf));
  Result := PWideChar(@Buf);
end;

//------------------------------------------------------------------------------

function TabfUserIdentity.GetFolder: WideString;
var
  Buf: array[0..MAX_PATH] of WideChar;
begin
  Result := '';
  FillChar(Buf, SizeOf(Buf), 0);

  if not CheckUser then Exit;

  FUser.GetIdentityFolder(GIF_NON_ROAMING_FOLDER, @Buf, SizeOf(Buf));
  Result := PWideChar(@Buf);
end;

//------------------------------------------------------------------------------

function TabfUserIdentity.GetID: TGUID;
begin
  Result := FID;
  if not IsEqualGUID(Result, GUID_NULL) then Exit;

  if not CheckUser then Exit;

  FUser.GetCookie(FID);
  Result := FID;  
end;

//------------------------------------------------------------------------------

function TabfUserIdentity.GetOrdinal: Cardinal;
begin
  Result := 0;
  if not CheckUser2 then Exit;

  FUser2.GetOrdinal(Result);
end;

//------------------------------------------------------------------------------

procedure TabfUserIdentity.SetName(const AValue: WideString);
begin
  if not CheckUser2 then Exit;

  FUser2.SetName(PWideChar(AValue));
end;

//------------------------------------------------------------------------------

function TabfUserIdentity.ChangePassword(const AOldPassword,
  ANewPassword: WideString): Boolean;
begin
  Result := False;
  if not CheckUser2 then Exit;

  Result := FUser2.ChangePassword(PWideChar(AOldPassword),
    PWideChar(ANewPassword)) = S_OK;
end;

//==============================================================================
//  TabfIdentityChangeNotify
//==============================================================================

constructor TabfIdentityChangeNotify.Create(AOwner: TabfIdentityManager);
begin
  inherited Create;
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TabfIdentityChangeNotify.QuerySwitchIdentities: HRESULT;
var
  AllowChangeFlag: Boolean;
begin
  Result := S_OK;
  if not Assigned(FOwner) then Exit;

  AllowChangeFlag := True;

  FOwner.DoQuerySwitchIdentities(AllowChangeFlag);

  if not AllowChangeFlag then
    Result := HRESULT(E_PROCESS_CANCELLED_SWITCH);
end;

//------------------------------------------------------------------------------

function TabfIdentityChangeNotify.SwitchIdentities: HRESULT;
begin
  Result := S_OK;
  if not Assigned(FOwner) then Exit;

  FOwner.DoSwitchIdentities;
end;

//------------------------------------------------------------------------------

function TabfIdentityChangeNotify.IdentityInformationChanged(dwType: DWORD): HRESULT;
begin
  Result := S_OK;
  if not Assigned(FOwner) then Exit;

  FOwner.DoIdentityInformationChanged(dwType);
end;

//==============================================================================
//  TabfIdentityManager
//==============================================================================

constructor TabfIdentityManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FIdentities := TObjectList.Create;
  FActive := False;
  FHookID := 0;
  FLoading := False;
  FStreamedActive := False;
end;

//------------------------------------------------------------------------------

destructor TabfIdentityManager.Destroy;
begin
  UnloadManager;

  FreeAndNil(FIdentities);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.Loaded;
begin
  inherited Loaded;
  if FStreamedActive then Active := True;
  if not (csDesigning in ComponentState) then
    if not _ASK then _TADA;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.SetActive(AValue: Boolean);
begin
  if (csReading in ComponentState) then
  begin
    FStreamedActive := AValue;
  end else
  if (FActive <> AValue) then
  begin
    if AValue then LoadManager else UnloadManager;
  end;
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.CheckPrivManager: Boolean;
begin
  Result := Assigned(FPrivManager);
  if Result then Exit;

  if not Assigned(FManager) then Exit;

  FManager.QueryInterface(IID_IPrivateIdentityManager, FPrivManager);

  Result := Assigned(FPrivManager);
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.CheckPrivManager2: Boolean;
begin
  Result := Assigned(FPrivManager2);
  if Result then Exit;

  if not Assigned(FManager) then Exit;

  FManager.QueryInterface(IID_IPrivateIdentityManager2, FPrivManager2);

  Result := Assigned(FPrivManager2);
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.GetCurrentIdentity: TabfUserIdentity;
begin
  Result := GetIdentityBySpecialID(UID_GIBC_CURRENT_USER);
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.GetDefaultIdentity: TabfUserIdentity;
begin
  Result := GetIdentityBySpecialID(UID_GIBC_DEFAULT_USER);
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.GetIdentity(AIndex: Integer): TabfUserIdentity;
begin
  Result := nil;
  if not Assigned(FIdentities) then Exit;
  if (AIndex < 0) or (AIndex >= GetIdentityCount) then Exit;

  Result := TabfUserIdentity(FIdentities[AIndex]);
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.GetIdentityCount: Integer;
begin
  Result := 0;
  if not Assigned(FIdentities) then Exit;

  Result := FIdentities.Count;
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.GetIdentityBySpecialID(AID: TGUID): TabfUserIdentity;
var
  User: IUserIdentity;
  UserID: TGUID;
begin
  Result := nil;

  if not Assigned(FManager) then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  UserID := AID;
  FManager.GetIdentityByCookie(UserID, User);
  if not Assigned(User) then Exit;
  try
    FillChar(UserID, SizeOf(UserID), 0);
    User.GetCookie(UserID);
  finally
    User := nil;
  end;

  while FLoading do Application.ProcessMessages;

  Result := GetIdentityByID(UserID);
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.DoChangeIdentities;
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  try
    if Assigned(FOnChangeIdentities) then
      FOnChangeIdentities(Self);
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.DoQuerySwitchIdentities(var AllowChange: Boolean);
var
  OutgoingIdentity, IncomingIdentity: TabfUserIdentity;
begin
  try
    if Assigned(FOnQuerySwitchIdentities) then
    begin
      OutgoingIdentity := GetIdentityBySpecialID(UID_GIBC_OUTGOING_USER);
      IncomingIdentity := GetIdentityBySpecialID(UID_GIBC_INCOMING_USER);

      FOnQuerySwitchIdentities(Self, OutgoingIdentity, IncomingIdentity,
        AllowChange);
    end;
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.DoIdentityInformationChanged(dwType: DWORD);
begin
  LoadIdentities;
  try
    case dwType of
    IIC_CURRENT_IDENTITY_CHANGED:
      if Assigned(FOnCurrentIdentityChanged) then
        FOnCurrentIdentityChanged(Self);
    IIC_IDENTITY_CHANGED:
      if Assigned(FOnIdentityChanged) then
        FOnIdentityChanged(Self);
    IIC_IDENTITY_DELETED:
      if Assigned(FOnDeleteIdentity) then
        FOnDeleteIdentity(Self);
    IIC_IDENTITY_ADDED:
      if Assigned(FOnNewIdentity) then
        FOnNewIdentity(Self);
    end;
  except
  end;
  DoChangeIdentities;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.DoSwitchIdentities;
begin
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
  try
    if Assigned(FOnSwitchIdentities) then
      FOnSwitchIdentities(Self);
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.LoadIdentities;
var
  EUI: IEnumUserIdentity;
  i: Integer;
  Count: Integer;
  Arr: array[0..0] of Pointer;
begin
  if not Assigned(FIdentities) then Exit;
  FIdentities.Clear;

  if not Assigned(FManager) then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  FManager.EnumIdentities(EUI);
  if not Assigned(EUI) then Exit;
  
  FLoading := True;
  try
    Count := 0;
    EUI.GetCount(Cardinal(Count));

    for i := 0 to Count - 1 do
    begin
      EUI.Next(1, Arr, nil);

      if Assigned(Arr[0]) then
      try
        FIdentities.Add(TabfUserIdentity.Create(Arr[0]));
      except
      end;
    end;
  finally
    FLoading := False;
    EUI := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.LoadManager;
var
  COMIntf: IUnknown;
begin
  UnloadManager;

  COMIntf := CreateCOMObject(CLSID_UserIdentityManager);
  if not Assigned(COMIntf) then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_ErrorLoadingAPI, [Name]));
  try
    COMIntf.QueryInterface(IID_IUserIdentityManager, FManager);
    if not Assigned(FManager) then Exit;

    LoadIdentities;
    HookManager;

    FActive := True;

    DoChangeIdentities;    
  finally
    COMIntf := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.UnloadManager;
begin
  UnhookManager;

  if Assigned(FIdentities) then FIdentities.Clear;

  FPrivManager2 := nil;
  FPrivManager := nil;
  FManager := nil;
  FActive := False;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.HookManager;
var
  CP: IConnectionPoint;
begin
  UnhookManager;

  FCallbackObject := TabfIdentityChangeNotify.Create(Self);
  FCallbackIntf := FCallbackObject;

  if not Assigned(FManager) then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  FManager.QueryInterface(IConnectionPoint, CP);
  if not Assigned(CP) then Exit;

  CP.Advise(FCallbackIntf, FHookID);
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.UnhookManager;
var
  CP: IConnectionPoint;
begin
  if FHookID > 0 then
  try
    if not Assigned(FManager) then Exit;

    FManager.QueryInterface(IConnectionPoint, CP);
    if not Assigned(CP) then Exit;

    CP.Unadvise(FHookID);
  finally
    FHookID := 0;
    FCallbackIntf := nil;
  end else
  if Assigned(FCallbackObject) then FCallbackIntf := nil;
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.CreateIdentity(const AName: WideString;
  const APassword: WideString): TabfUserIdentity;
var
  User: IUserIdentity;
  UserID: TGUID;
begin
  Result := nil;

  if not CheckPrivManager2 then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  FPrivManager2.CreateIdentity2(PWideChar(AName), PWideChar(APassword), User);

  if not Assigned(User) then Exit;
  try
    FillChar(UserID, SizeOf(UserID), 0);
    User.GetCookie(UserID);
  finally
    User := nil;
  end;

  Result := GetIdentityByID(UserID);
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.DestroyIdentity(AID: TGUID): Boolean;
begin
  if not CheckPrivManager2 then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  Result := FPrivManager2.DestroyIdentity(AID) = S_OK;

  while FLoading do Application.ProcessMessages;
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.GetIdentityByID(AID: TGUID): TabfUserIdentity;
var
  i: Integer;
begin
  Result := nil;

  while FLoading do Application.ProcessMessages;

  for i := 0 to IdentityCount - 1 do
    if IsEqualGUID(Identities[i].ID, AID) then
    begin
      Result := Identities[i];
      Exit;
    end;
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.Logoff: Boolean;
begin
  if not Assigned(FManager) then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  Result := FManager.Logoff(ParentHandle) = S_OK;
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.Logon(const AName: WideString;
  const APassword: WideString): TabfUserIdentity;
var
  User: IUserIdentity;
  UserID: TGUID;
begin
  Result := nil;

  if Trim(AName) = '' then
  begin
    if not Assigned(FManager) then
      raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

    FManager.Logon(ParentHandle, UIL_FORCE_UI, User);
  end else
  begin
    if not CheckPrivManager2 then
      raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

    FPrivManager2.LogonAs(PWideChar(AName), PWideChar(APassword), User);
  end;

  if not Assigned(User) then Exit;
  try
    FillChar(UserID, SizeOf(UserID), 0);
    User.GetCookie(UserID);
  finally
    User := nil;
  end;

  Result := GetIdentityByID(UserID);
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.ManageIdentities(ACreateNew: Boolean): Boolean;
var
  Flag: Cardinal;
begin
  if not Assigned(FManager) then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  Flag := 0;
  if ACreateNew then
    Flag := UIMI_CREATE_NEW_IDENTITY;

  Result := FManager.ManageIdentities(ParentHandle, Flag) = S_OK;
end;

//------------------------------------------------------------------------------

function TabfIdentityManager.SetDefaultIdentity(AID: TGUID): TabfUserIdentity;
begin
  if not CheckPrivManager2 then
    raise EabfIdentityManagerException.Create(Format(SabfIdMan_NotOpenedError, [Name]));

  FPrivManager2.SetDefaultIdentity(AID);

  Result := DefaultIdentity;
end;

//------------------------------------------------------------------------------

procedure TabfIdentityManager.Refresh;
begin
  LoadIdentities;
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfIdentityManager]);


end.
