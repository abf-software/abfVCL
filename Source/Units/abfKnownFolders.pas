{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfKnownFolders;

{$I abf.inc}

interface

uses
  Windows, abfShlObj,
  KnownFolders;

const
  CLSID_KnownFolderManager: TGUID = '{4DF0C730-DF9D-4AE3-9153-AA6B82E9795A}';

type
  PFOLDERTYPEID = ^FOLDERTYPEID;
  FOLDERTYPEID = TGUID;

  tagKF_CATEGORY = DWORD;
  PKF_CATEGORY = ^KF_CATEGORY;
  KF_CATEGORY = tagKF_CATEGORY;


const
  KF_CATEGORY_VIRTUAL: tagKF_CATEGORY = $00000001; // virtual folders, have not file system path
  KF_CATEGORY_FIXED  : tagKF_CATEGORY = $00000002; // fixed, predefined OS folders, might be different for different users
  KF_CATEGORY_COMMON : tagKF_CATEGORY = $00000003; // Public folders shared by all users, can be redirected
  KF_CATEGORY_PERUSER: tagKF_CATEGORY = $00000004; // User folders, can be redirected


type
  tagKF_DEFINITION_FLAGS = DWORD;
  KF_DEFINITION_FLAGS = tagKF_DEFINITION_FLAGS;


const
  KFDF_LOCAL_REDIRECT_ONLY: tagKF_DEFINITION_FLAGS = $00000002; // Can only be redirected to a local disk
  KFDF_ROAMABLE           : tagKF_DEFINITION_FLAGS = $00000004; // Can be roamed via PC to PC sync
  KFDF_PRECREATE          : tagKF_DEFINITION_FLAGS = $00000008; // Pre create folder
  KFDF_STREAM             : tagKF_DEFINITION_FLAGS = $00000010; // folder is actually a file


type
  tagKF_REDIRECT_FLAGS = DWORD;
  KF_REDIRECT_FLAGS = tagKF_REDIRECT_FLAGS;


const
  KF_REDIRECT_USER_EXCLUSIVE     : tagKF_REDIRECT_FLAGS = $00000001; // Give user exclusive permission
  KF_REDIRECT_COPY_SOURCE_DACL   : tagKF_REDIRECT_FLAGS = $00000002; // Copy the DACL of the source directory to target

// Ownership checks for the target folder if the folder exists, by default, the API does not do any ownership checks
  KF_REDIRECT_OWNER_USER         : tagKF_REDIRECT_FLAGS = $00000004; // The owner of the folder must be the user itself
  KF_REDIRECT_SET_OWNER_EXPLICIT : tagKF_REDIRECT_FLAGS = $00000008; // The owner of any newly created folder will be set explicitly
                                                                     // by default if user belongs to Administrators group Administrators will be the owner

// Check if the redirection is already done, S_OK if yes, S_FALSE if some actions need to be done.
  KF_REDIRECT_CHECK_ONLY         : tagKF_REDIRECT_FLAGS = $00000010;

// Enable user interaction when redirecting
  KF_REDIRECT_WITH_UI            : tagKF_REDIRECT_FLAGS = $00000020;

// CSC related settings
  KF_REDIRECT_UNPIN              : tagKF_REDIRECT_FLAGS = $00000040; // Unpin the source folder
  KF_REDIRECT_PIN                : tagKF_REDIRECT_FLAGS = $00000080; // Pin the target folder

  KF_REDIRECT_COPY_CONTENTS      : tagKF_REDIRECT_FLAGS = $00000200; // Copy the contents (both files and subfolders) under the known folder
  KF_REDIRECT_DEL_SOURCE_CONTENTS: tagKF_REDIRECT_FLAGS = $00000400; // Delete source, valid only if KF_REDIRECT_COPY_CONTENTS set

  KF_REDIRECT_EXCLUDE_ALL_KNOWN_SUBFOLDERS
                                 : tagKF_REDIRECT_FLAGS = $00000800; // Exclude all known subfolders from redirection

type
  tagKF_REDIRECTION_CAPABILITIES = DWORD;
  PKF_REDIRECTION_CAPABILITIES = ^KF_REDIRECTION_CAPABILITIES;
  KF_REDIRECTION_CAPABILITIES = DWORD;


const
  KF_REDIRECTION_CAPABILITIES_ALLOW_ALL
                                   : tagKF_REDIRECTION_CAPABILITIES = $000000FF; // if any of these flags set this means that
                                                                                 // the folder can potentially be redirected unless
                                                                                 // any deny flag is set
  KF_REDIRECTION_CAPABILITIES_REDIRECTABLE
                                   : tagKF_REDIRECTION_CAPABILITIES = $00000001; // the folder can potentially be redirected
                                                                                 // currently we only allow redirection for
                                                                                 // common and user's folders.
                                                                                 // Fixed and virtual can not be redirected

  KF_REDIRECTION_CAPABILITIES_DENY_ALL
                                   : tagKF_REDIRECTION_CAPABILITIES = $000FFF00; // If any of there flags set then redirection is blocked
  KF_REDIRECTION_CAPABILITIES_DENY_POLICY_REDIRECTED
                                   : tagKF_REDIRECTION_CAPABILITIES = $00000100; // Folder can not be redirected because it is redirected by group policy
  KF_REDIRECTION_CAPABILITIES_DENY_POLICY
                                   : tagKF_REDIRECTION_CAPABILITIES = $00000200; // Folder can not be redirected because the policy prohibits redirecting this folder
  KF_REDIRECTION_CAPABILITIES_DENY_PERMISSIONS
                                   : tagKF_REDIRECTION_CAPABILITIES = $00000400; // Folder can not be redirected because the caller does not have sufficient permissions


type
  tagKNOWNFOLDER_DEFINITION = record
    category: KF_CATEGORY;
    pszName: LPWSTR;
    pszDescription: LPWSTR;
    fidParent: KNOWNFOLDERID;
    pszRelativePath: LPWSTR;
    pszParsingName: LPWSTR;
    pszTooltip: LPWSTR;
    pszLocalizedName: LPWSTR;
    pszIcon: LPWSTR;
    pszSecurity: LPWSTR;
    dwAttributes: DWORD;
    kfdFlags: KF_DEFINITION_FLAGS;
    ftidType: FOLDERTYPEID;
  end;
  PKNOWNFOLDER_DEFINITION = ^KNOWNFOLDER_DEFINITION;
  KNOWNFOLDER_DEFINITION = tagKNOWNFOLDER_DEFINITION;

  PIDLIST_ABSOLUTE  = PItemIDList;
  PCIDLIST_ABSOLUTE = PItemIDList;

  KNOWN_FOLDER_FLAG = DWORD;

const
// flags for Known Folder APIs

  KF_FLAG_DEFAULT            : KNOWN_FOLDER_FLAG = $00000000;

  // Make sure that the folder already exists or create it and apply security specified in folder definition
  // If folder can not be created then function will return failure and no folder path (IDList) will be returned
  // If folder is located on the network the function may take long time to execute
  KF_FLAG_CREATE             : KNOWN_FOLDER_FLAG = $00008000;

  // If this flag is specified then the folder path is returned and no verification is performed
  // Use this flag is you want to get folder's path (IDList) and do not need to verify folder's existence
  //
  // If this flag is NOT specified then Known Folder API will try to verify that the folder exists
  //     If folder does not exist or can not be accessed then function will return failure and no folder path (IDList) will be returned
  //     If folder is located on the network the function may take long time to execute
  KF_FLAG_DONT_VERIFY        : KNOWN_FOLDER_FLAG = $00004000;

  // Set folder path as is and do not try to substitute parts of the path with environments variables.
  // If flag is not specified then Known Folder will try to replace parts of the path with some
  // known environment variables (%USERPROFILE%, %APPDATA% etc.)
  KF_FLAG_DONT_UNEXPAND      : KNOWN_FOLDER_FLAG = $00002000;

  // Get file system based IDList if available. If the flag is not specified the Known Folder API
  // will try to return aliased IDList by default. Example for FOLDERID_Documents -
  // Aliased - [desktop]\[user]\[Documents] - exact location is determined by shell namespace layout and might change
  // Non aliased - [desktop]\[computer]\[disk_c]\[users]\[user]\[Documents] - location is determined by folder location in the file system
  KF_FLAG_NO_ALIAS           : KNOWN_FOLDER_FLAG = $00001000;

  // Initialize the folder with desktop.ini settings
  // If folder can not be initialized then function will return failure and no folder path will be returned
  // If folder is located on the network the function may take long time to execute
  KF_FLAG_INIT               : KNOWN_FOLDER_FLAG = $00000800;

  // Get the default path, will also verify folder existence unless KF_FLAG_DONT_VERIFY is also specified
  KF_FLAG_DEFAULT_PATH       : KNOWN_FOLDER_FLAG = $00000400;

  // Get the not-parent-relative default path. Only valid with KF_FLAG_DEFAULT_PATH
  KF_FLAG_NOT_PARENT_RELATIVE: KNOWN_FOLDER_FLAG = $00000200;

  // Build simple IDList
  KF_FLAG_SIMPLE_IDLIST      : KNOWN_FOLDER_FLAG = $00000100;

  // only return the aliased IDLists, don't fallback to file system path
  KF_FLAG_ALIAS_ONLY         : KNOWN_FOLDER_FLAG = $80000000;


//==============================================================================
// Known Folder Interface
//==============================================================================

type
  IKnownFolder = interface(IUnknown)
    ['{3AA7AF7E-9B36-420c-A8E3-F77D4674A488}']
    function GetId(out pkfid: KNOWNFOLDERID): HRESULT; stdcall;
    function GetCategory(out pCategory: PKF_CATEGORY): HRESULT; stdcall;
    // get the ShellItem (IShellItem or derived interface) for this known folder
    function GetShellItem(
      dwFlags: KNOWN_FOLDER_FLAG;
      riid: TGUID;
      out ppv: IUnknown): HRESULT; stdcall;
    function GetPath(
      dwFlags: KNOWN_FOLDER_FLAG;
      out ppszPath: LPWSTR): HRESULT; stdcall; // A call of CoTaskMemFree are needed to free ppszPath
    function SetPath(
      dwFlags: KNOWN_FOLDER_FLAG;
      pszPath: LPCWSTR): HRESULT; stdcall;
    function GetIDList(
      dwFlags: KNOWN_FOLDER_FLAG;
      out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall; // A call of ILFree (or CoTaskMemFree) are needed to free ppidl
    function GetFolderType(out pftid: PFOLDERTYPEID): HRESULT; stdcall;
    function GetRedirectionCapabilities(out pCapabilities: PKF_REDIRECTION_CAPABILITIES): HRESULT; stdcall;
    function GetFolderDefinition(out pKFD: KNOWNFOLDER_DEFINITION): HRESULT; stdcall; // A call of FreeKnownFolderDefinitionFields are needed to free resources of pKFD
  end;


//==============================================================================
// Known Folder Manager
//==============================================================================

  tagFFFP_MODE = (
    FFFP_EXACTMATCH,
    FFFP_NEARESTPARENTMATCH
  );
  FFFP_MODE = tagFFFP_MODE;


  IKnownFolderManager = interface(IUnknown)
    ['{8BE2D872-86AA-4d47-B776-32CCA40C7018}']
    function FolderIdFromCsidl(
      nCsidl: Integer;
      out pfid: KNOWNFOLDERID): HRESULT; stdcall;
    function FolderIdToCsidl(
      rfid: REFKNOWNFOLDERID;
      out pnCsidl: DWORD): HRESULT; stdcall;
    function GetFolderIds(
      out ppKFId: PKNOWNFOLDERID;
      var pCount: UINT): HRESULT; stdcall;
    function GetFolder(
      rfid: REFKNOWNFOLDERID;
      out ppkf: IKnownFolder): HRESULT; stdcall;
    function GetFolderByName(
      pszCanonicalName: LPCWSTR;
      out ppkf: IKnownFolder): HRESULT; stdcall;
    function RegisterFolder(
      rfid: REFKNOWNFOLDERID;
      pKFD: KNOWNFOLDER_DEFINITION): HRESULT; stdcall;
    function UnregisterFolder(rfid: REFKNOWNFOLDERID): HRESULT; stdcall;
    function FindFolderFromPath(
      pszPath: LPCWSTR;
      mode: FFFP_MODE;
      out ppkf: IKnownFolder): HRESULT; stdcall;
    function FindFolderFromIDList(
      pidl: PCIDLIST_ABSOLUTE;
      out ppkf: IKnownFolder): HRESULT; stdcall;
    function Redirect(
      rfid: REFKNOWNFOLDERID;
      hwnd: HWND;
      flags: KF_REDIRECT_FLAGS;
      pszTargetPath: LPCWSTR;
      cFolders: UINT;
      pExclusion: PGUID;
      out ppszError: PLPWSTR): HRESULT; stdcall;
  end;

//==============================================================================
// Misc functions
//==============================================================================

// Frees the allocated fields in the result from IKnownFolderManager.GetFolderDefinition
procedure FreeKnownFolderDefinitionFields(pKFD: KNOWNFOLDER_DEFINITION);

// Retrieves the full path of a known folder identified by the folder's KNOWNFOLDERID.
// A call of CoTaskMemFree are needed to free ppszPath
function SHGetKnownFolderPath(rfid: REFKNOWNFOLDERID; dwFlags: KNOWN_FOLDER_FLAG;
  hToken: THandle; out ppszPath: LPWSTR): HRESULT; stdcall;

// Retrieves the path of a known folder as an ITEMIDLIST structure.
// A call of ILFree (or CoTaskMemFree) are needed to free ppidl
function SHGetKnownFolderIDList(rfid: REFKNOWNFOLDERID; dwFlags: KNOWN_FOLDER_FLAG;
  hToken: THandle; out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;

// Redirects a known folder to a new location.
function SHSetKnownFolderPath(rfid: REFKNOWNFOLDERID; dwFlags: KNOWN_FOLDER_FLAG;
  hToken: THandle; pszPath: LPCWSTR): HRESULT; stdcall;


{******************************************************************************}
implementation
{******************************************************************************}

uses
  SysUtils, ActiveX;

//==============================================================================
// Private types
//==============================================================================

type
  TSHGetKnownFolderPath = function (rfid: REFKNOWNFOLDERID; dwFlags: DWORD;
    hToken: THandle; out ppszPath: LPWSTR): HRESULT; stdcall;
  TSHGetKnownFolderIDList = function (rfid: REFKNOWNFOLDERID; dwFlags: DWORD;
    hToken: THandle; out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;
  TSHSetKnownFolderPath = function (rfid: REFKNOWNFOLDERID; dwFlags: DWORD;
    hToken: THandle; pszPath: LPCWSTR): HRESULT; stdcall;

//==============================================================================
// Private variables
//==============================================================================

var
  _LibHandle: THandle = 0;
  _SHGetKnownFolderPath: TSHGetKnownFolderPath = nil;
  _SHGetKnownFolderIDList: TSHGetKnownFolderIDList = nil;
  _SHSetKnownFolderPath: TSHSetKnownFolderPath = nil;


//==============================================================================
// Routines for initialization/finalization
//==============================================================================

procedure _DoInitialization;
begin
  if _LibHandle <= 0 then
    _LibHandle := SafeLoadLibrary('shell32.dll');
  if _LibHandle <= 0 then Exit;

  if not Assigned(@_SHGetKnownFolderPath) then
    @_SHGetKnownFolderPath := GetProcAddress(_LibHandle,
      PAnsiChar('SHGetKnownFolderPath'));

  if not Assigned(@_SHGetKnownFolderIDList) then
    @_SHGetKnownFolderIDList := GetProcAddress(_LibHandle,
      PAnsiChar('SHGetKnownFolderIDList'));

  if not Assigned(@_SHSetKnownFolderPath) then
    @_SHSetKnownFolderPath := GetProcAddress(_LibHandle,
      PAnsiChar('SHSetKnownFolderPath'));
end;

//------------------------------------------------------------------------------

procedure _DoFinalization;
begin
  @_SHGetKnownFolderPath := nil;
  @_SHGetKnownFolderIDList := nil;
  @_SHSetKnownFolderPath := nil;

  if _LibHandle <= 0 then Exit;

  FreeLibrary(_LibHandle);
  _LibHandle := 0;
end;


//==============================================================================
// Misc functions
//==============================================================================

procedure FreeKnownFolderDefinitionFields(pKFD: KNOWNFOLDER_DEFINITION);
begin
  CoTaskMemFree(pKFD.pszName);
  CoTaskMemFree(pKFD.pszDescription);
  CoTaskMemFree(pKFD.pszRelativePath);
  CoTaskMemFree(pKFD.pszParsingName);
  CoTaskMemFree(pKFD.pszTooltip);
  CoTaskMemFree(pKFD.pszLocalizedName);
  CoTaskMemFree(pKFD.pszIcon);
  CoTaskMemFree(pKFD.pszSecurity);
end;

//------------------------------------------------------------------------------

function SHGetKnownFolderPath(rfid: REFKNOWNFOLDERID; dwFlags: DWORD;
  hToken: THandle; out ppszPath: LPWSTR): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
  _DoInitialization;
  if not Assigned(@_SHGetKnownFolderPath) then Exit;

  Result := _SHGetKnownFolderPath(rfid, dwFlags, hToken, ppszPath);
end;
//------------------------------------------------------------------------------

function SHGetKnownFolderIDList(rfid: REFKNOWNFOLDERID; dwFlags: DWORD;
  hToken: THandle; out ppidl: PIDLIST_ABSOLUTE): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
  _DoInitialization;
  if not Assigned(@_SHGetKnownFolderIDList) then Exit;

  Result := _SHGetKnownFolderIDList(rfid, dwFlags, hToken, ppidl);
end;

//------------------------------------------------------------------------------

function SHSetKnownFolderPath(rfid: REFKNOWNFOLDERID; dwFlags: DWORD;
  hToken: THandle; pszPath: LPCWSTR): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
  _DoInitialization;
  if not Assigned(@_SHSetKnownFolderPath) then Exit;

  Result := _SHSetKnownFolderPath(rfid, dwFlags, hToken, pszPath);
end;


{******************************************************************************}
initialization
{******************************************************************************}
//  _DoInitialization;

{******************************************************************************}
finalization
{******************************************************************************}
  _DoFinalization;

end.
