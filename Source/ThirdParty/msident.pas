//------------------------------------------------------------------------------
//
//  Microsoft Windows
// Copyright (c) Microsoft Corporation. All rights reserved.
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//------------------------------------------------------------------------------
unit msident;

interface

uses
  Windows;

//------------------------------------------------------------------------------
// Lightweight User Profile Interfaces.

// -----------------------------------------------------------------------------
// GUIDS
// -----------------------------------------------------------------------------
const
  CLSID_UserIdentityManager   : TGUID = '{A9AE6C91-1D1B-11D2-B21A-00C04FA357FA}';
  IID_IUserIdentity           : TGUID = '{A9AE6C8E-1D1B-11D2-B21A-00C04FA357FA}';
  IID_IEnumUserIdentity       : TGUID = '{A9AE6C8F-1D1B-11D2-B21A-00C04FA357FA}';
  IID_IUserIdentityManager    : TGUID = '{A9AE6C90-1D1B-11D2-B21A-00C04FA357FA}';
  IID_IIdentityChangeNotify   : TGUID = '{A9AE6C92-1D1B-11D2-B21A-00C04FA357FA}';
  IID_IPrivateIdentityManager : TGUID = '{A9AE6C93-1D1B-11D2-B21A-00C04FA357FA}';
  IID_IUserIdentity2          : TGUID = '{A9AE6C94-1D1B-11D2-B21A-00C04FA357FA}';
  IID_IPrivateIdentityManager2: TGUID = '{47172E6C-EA67-4ccd-B5CE-2EABBE051404}';

  UID_GIBC_DEFAULT_USER : TGUID = '{C28E26E6-219D-11d2-B200-0000F8085266}';
  UID_GIBC_CURRENT_USER : TGUID = '{C28E26E7-219D-11d2-B200-0000F8085266}';
  UID_GIBC_OUTGOING_USER: TGUID = '{C28E26E8-219D-11d2-B200-0000F8085266}';
  UID_GIBC_INCOMING_USER: TGUID = '{C28E26E9-219D-11d2-B200-0000F8085266}';

// -----------------------------------------------------------------------------
// ERROR CODES
// -----------------------------------------------------------------------------
  E_IDENTITIES_DISABLED        = DWORD($80007110);
  S_IDENTITIES_DISABLED        = DWORD($00007110);
  E_NO_CURRENT_IDENTITY        = DWORD($80007111);
  E_USER_CANCELLED             = DWORD($80007112);
  E_PROCESS_CANCELLED_SWITCH   = DWORD($80007113);
  E_IDENTITY_NOT_FOUND         = DWORD($80007114);
  E_IDENTITY_EXISTS            = DWORD($80007115);
  E_IDENTITY_CHANGING          = DWORD($80007116);

  CCH_IDENTITY_NAME_MAX_LENGTH = 63;

  {IUserIdentity.GetIdentityFolder}
  GIF_ROAMING_FOLDER     = DWORD($00000001);
  GIF_NON_ROAMING_FOLDER = DWORD($00000002);

type
  IUserIdentity = interface
  ['{A9AE6C8E-1D1B-11D2-B21A-00C04FA357FA}']
    // Get cookie for this user
    function GetCookie(out puidCookie: TGUID): HRESULT; stdcall;
    // Get user name
    function GetName(pszName: PWideChar; ulBuffSize: ULONG): HRESULT; stdcall;
    // Get private registry location
    function OpenIdentityRegKey(dwDesiredAccess: DWORD;
      out phKey: HKEY): HRESULT; stdcall;
    // Get a per-user folder
    function GetIdentityFolder(dwFlags: DWORD; pszPath: PWideCHAR;
      ulBuffSize: ULONG): HRESULT; stdcall;
  end;


  IUserIdentity2 = interface(IUserIdentity)
  ['{A9AE6C94-1D1B-11D2-B21A-00C04FA357FA}']
    // Get ordinal for this user
    function GetOrdinal(out dwOrdinal: DWORD): HRESULT; stdcall;
    function SetName(pszName: PWideChar): HRESULT; stdcall;
    function ChangePassword(szOldPass: PWideChar;
      szNewPass: PWideChar): HRESULT; stdcall;
  end;


  IEnumUserIdentity = interface
  ['{A9AE6C8F-1D1B-11D2-B21A-00C04FA357FA}']
    function Next(celt: ULONG; out rgelt;
      pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumUserIdentity): HRESULT; stdcall;
    function GetCount(out pnCount: ULONG): HRESULT; stdcall;
  end;

const
  {IUserIdentityManager.ManageIdentities}
  UIMI_CREATE_NEW_IDENTITY = DWORD($00000001);
  {IUserIdentityManager.Logon}
  UIL_FORCE_UI             = DWORD($80000001);

type
  IUserIdentityManager = interface
  ['{A9AE6C90-1D1B-11D2-B21A-00C04FA357FA}']
    // Get a list of all users
    function EnumIdentities(out ppEnumUser: IEnumUserIdentity): HRESULT; stdcall;
    // Show UI to manage users
    function ManageIdentities(hwndParent: HWND; dwFlags: DWORD): HRESULT; stdcall;
    // Show UI to logon a user
    function Logon(hwndParent: HWND; dwFlags: DWORD;
      out ppIdentity: IUserIdentity): HRESULT; stdcall;
    // Log the current user off
    function Logoff(hwndParent: HWND): HRESULT; stdcall;
    // Get an arbitrary user by cookie
    function GetIdentityByCookie(var uidCookie: TGUID;
      out ppIdentity: IUserIdentity): HRESULT; stdcall;
  end;

const
  {IIdentityChangeNotify.IdentityInformationChanged}
  IIC_CURRENT_IDENTITY_CHANGED = DWORD($00000001);
  IIC_IDENTITY_CHANGED         = DWORD($00000002);
  IIC_IDENTITY_DELETED         = DWORD($00000004);
  IIC_IDENTITY_ADDED           = DWORD($00000008);

type
  IIdentityChangeNotify = interface
  ['{A9AE6C92-1D1B-11D2-B21A-00C04FA357FA}']
    // The user has requested a switch.  Return E_PROCESS_CANCELLED_SWITCH
    // if the switch should not take place.
    function QuerySwitchIdentities: HRESULT; stdcall;
    // A switch has occurred.  Reload any settings necessary from the current 
    // identity.
    function SwitchIdentities: HRESULT; stdcall;
    // Some property (name, etc) of an identity has been updated or an identity
    // has been added/deleted.
    function IdentityInformationChanged(dwType: DWORD): HRESULT; stdcall;
  end;

  IPrivateIdentityManager = interface
  ['{A9AE6C93-1D1B-11D2-B21A-00C04FA357FA}']
    // Create a new identity with a given name.
    function CreateIdentity(pszName: PWideChar;
      out ppIdentity: IUserIdentity): HRESULT; stdcall;
    // Verify a password for the indicated identity
    function ConfirmPassword(uidCookie: TGUID;
      pszPassword: PWideChar): HRESULT; stdcall;
  end;

  IPrivateIdentityManager2 = interface
  ['{15E84C92-2E4D-11d3-9C92-00104B35E7F9}']
    // Create a new identity with a given name.
    function CreateIdentity2(pszName: PWideChar; pszPassword: PWideChar;
      out ppIdentity: IUserIdentity): HRESULT; stdcall;
    // Removes an identity
    function DestroyIdentity(const uidCookie: TGUID): HRESULT; stdcall;
    // Logon as named user with password
    function LogonAs(pszName: PWideChar; pszPassword: PWideChar;
      out ppIdentity: IUserIdentity): HRESULT; stdcall;
    function SetDefaultIdentity(const puidCookie: TGUID): HRESULT; stdcall;
    function GetDefaultIdentity(out puidCookie: TGUID): HRESULT; stdcall;
  end;

implementation

end.
