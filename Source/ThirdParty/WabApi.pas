{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ Windows Address Book (WAB) functions interface unit              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) 1995-2000 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: wabapi.h, released 31 Jan 2000.            }
{ The original Pascal code is: WabApi.pas, released 15 Mar 2000.   }
{ The initial developer of the Pascal code is Petr Vones           }
{ (petr.v@mujmail.cz).                                             }
{                                                                  }
{ Portions created by Petr Vones are                               }
{ Copyright (c) 2000 Petr Vones                                    }
{                                                                  }
{ Obtained through:                                                }
{                                                                  }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

{*******************************************************************************

  Added some $IfDef compiler derectives to support Borland Delphi 3

  Portions created by ABF software, Inc.
  Copyright (c) 2000 ABF software, Inc.

*******************************************************************************}
unit WabApi;

interface

uses
  Windows, ActiveX, WabDefs, WabIab;

{$I abf.inc} // Delphi or C++Builder determination there...
{$I wab.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}

{$IfDef C3}
(*$HPPEMIT '#include <wabapi.h>'*)
{$EndIf C3}

type
  IWabObject = interface(IUnknown)
    function GetLastError(hResult: HResult; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function AllocateBuffer(cbSize: ULONG; out lppBuffer: Pointer): HResult;
      stdcall;
    function AllocateMore(cbSize: ULONG; lpObject: Pointer;
      out lppBuffer: Pointer): HResult; stdcall;
    function FreeBuffer(lpBuffer: Pointer): HResult; stdcall;
    function Backup(lpFileName: LPSTR): HResult; stdcall;
    function Import(lpImportParam: LPSTR): HResult; stdcall;
    function Find(lpIAB: IAddrBook; hWnd: HWND): HResult; stdcall;
    function VCardDisplay(lpIAB: IAddrBook; hWnd: HWND;
      lpszFileName: LPSTR): HResult; stdcall;
    function LDAPUrl(lpIAB: IAddrBook; hWnd: HWND; Flags: ULONG; lpszURL: LPSTR;
      lppMailUser: IMailUser): HResult; stdcall;
    function VCardCreate(lpIAB: IAddrBook; ulFlags: ULONG; lpszVCard: LPSTR;
      lppMailUser: IMailUser): HResult; stdcall;
    function VCardRetrieve(lpIAB: IAddrBook; ulFlags: ULONG; lpszVCard: LPSTR;
      out lppMailUser: IMailUser): HResult; stdcall;
    function GetMe(lpIAB: IAddrBook; ulFlags: ULONG; lpdwAction: PDWORD;
      lpsbEID: PSBinary; ulParam: ULONG): HResult; stdcall;
    function SetMe(lpIAB: IAddrBook; ulFlags: ULONG; sbEID: SBinary;
      ulParam: ULONG): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IWabObject}{$EndIf C3}

const

{ WABObject_LDAPUrl flags }

// If this flag is specified and the LDAPUrl returns a single
// query result, instructs the WAB to return the result in the
// form of a MailUser object instead of displaying Details on it
// If there are multiple results to the query, fail ..
  WABOBJECT_LDAPURL_RETURN_MAILUSER   = $00000001;
  {$IfDef C3}{$EXTERNALSYM WABOBJECT_LDAPURL_RETURN_MAILUSER}{$EndIf C3}

// WAB 5.0x and higher:
// If your application supports Unicode and wants to pass ina Unicode
//  URL to the WAB, you can cast the Unicode URL to an LPSTR and pass it
// to the LDAPUrl API, *also* setting ulFlags to MAPI_UNICODE to mark the URL
// as such. Casting is prefered to converting the string because
// (a) Converting the string may result in loss of data (b) since this is an
// already published interface we can't modify the interface.
  MAPI_UNICODE                        = $80000000;
  {$IfDef C3}{$EXTERNALSYM MAPI_UNICODE}{$EndIf C3}


{ WABObject_GetMe returned parameter }

// If the GetMe call results in the creation of a new 'Me' contact,
// the lpdwAction returned parameter will contain this value
// indicating to the caller that the object is freshly created and
// does not have any properties in it
  WABOBJECT_ME_NEW                    = $00000001;
  {$IfDef C3}{$EXTERNALSYM WABOBJECT_ME_NEW}{$EndIf C3}


{ WABObject_GetMe flags }

// WABObject_GetMe will create a new ME object by default, if
// none already exists. To force the call to not-create an object, if
// one doesn't already exist, specify the WABOBJECT_ME_NOCREATE flag.
// If no me is found, the call fails with MAPI_E_NOT_FOUND.
// Other flag for WABObject_GetMe is AB_NO_DIALOG defined in wabdefs.h
  WABOBJECT_ME_NOCREATE               = $00000002;
  {$IfDef C3}{$EXTERNALSYM WABOBJECT_ME_NOCREATE}{$EndIf C3}


{ IWABObject_VCard Create/Retrieve }

// Flags the WAB whether the lpszVCard parameter is a filename or if
// it is a NULL terminated string containing the compelte VCard contents
  WAB_VCARD_FILE                      = $00000000;
  {$IfDef C3}{$EXTERNALSYM WAB_VCARD_FILE}{$EndIf C3}
  WAB_VCARD_STREAM                    = $00000001;
  {$IfDef C3}{$EXTERNALSYM WAB_VCARD_STREAM}{$EndIf C3}

type
  PWabParam = ^TWabParam;
  _tagWAB_PARAM = record
    cbSize: ULONG;            // sizeof(WAB_PARAM).
    hwnd: HWND;               // hWnd of calling client Application. Can be NULL
    szFileName: LPSTR;        // WAB File name to open. if NULL, opens default.
    ulFlags: ULONG;           // See below
    guidPSExt: TGUID;         // A GUID that identifies the calling application's Property Sheet extensions
                              // The GUID can be used to determine whether the extension prop sheets are displayed or not.
  end;
  {$IfDef C3}{$EXTERNALSYM _tagWAB_PARAM}{$EndIf C3}
  TWabParam = _tagWAB_PARAM;
  WAB_PARAM = _tagWAB_PARAM;
  {$IfDef C3}{$EXTERNALSYM WAB_PARAM}{$EndIf C3}

const

{ flags for WAB_PARAM }

  WAB_USE_OE_SENDMAIL     = $00000001;  // Tells WAB to use Outlook Express for e-mail before checking for a
                                        // default Simple MAPI client. Default behaviour is to check for the
                                        // Simple MAPI client first
  {$IfDef C3}{$EXTERNALSYM WAB_USE_OE_SENDMAIL}{$EndIf C3}

  WAB_ENABLE_PROFILES     = $00400000;  // Invokes WAB in a Identity-aware session using Identity-Manager
                                        // based profiles
  {$IfDef C3}{$EXTERNALSYM WAB_ENABLE_PROFILES}{$EndIf C3}


type
  TWABOpen = function (out lppAdrBook: IAddrBook; out lppWABObject: IWabObject;
    lpWP: PWabParam; Reserved2: DWORD): HResult; stdcall;
  {$IfDef C3}{$EXTERNALSYM TWABOpen}{$EndIf C3}
  
  TWABOpenEx = function (out lppAdrBook: IAddrBook; out lppWABObject: IWabObject;
    lpWP: PWabParam; Reserved: DWORD; fnAllocateBuffer: TAllocateBuffer;
    fnAllocateMore: TAllocateMore; fnFreeBuffer: TFreeBuffer): HResult; stdcall;
  {$IfDef C3}{$EXTERNALSYM TWABOpenEx}{$EndIf C3}

  PWabImportParam = ^TWabImportParam;
  _WABIMPORTPARAM = record
    cbSize: ULONG;        // sizeof(WABIMPORTPARAM)
    lpAdrBook: IAddrBook; // ptr to the IAdrBook object (required)
    hWnd: HWND;           // Parent HWND for any dialogs
    ulFlags: ULONG;       // 0 or MAPI_DIALOG to show progress dialog and messages
    lpszFileName: LPSTR;  // FileName to import or NULL .. if NULL will show FileOpen dialog
  end;
  {$IfDef C3}{$EXTERNALSYM _WABIMPORTPARAM}{$EndIf C3}
  TWabImportParam = _WABIMPORTPARAM;
  WABIMPORTPARAM = _WABIMPORTPARAM;
  {$IfDef C3}{$EXTERNALSYM WABIMPORTPARAM}{$EndIf C3}

const

// ---- WABEXTDISPLAY -----------------
// WABEXTDISPLAY Structure used in extending the WAB Details Property Dialogs
// and for doing WAB Context Menu verb extensions.
// The structure is passed into the IWABExtInit::Initialize method
// of the implementor

  WAB_DISPLAY_LDAPURL = $00000001;  // The object being displayed is an LDAP URL
                                    // The URL can be found in the lpsz struct member
  {$IfDef C3}{$EXTERNALSYM WAB_DISPLAY_LDAPURL}{$EndIf C3}

  WAB_CONTEXT_ADRLIST = $00000002;  // THe lpv parameter contains a pointer to an
                                    // AdrList structure corresponding to selected items
                                    // on which to display a context menu
  {$IfDef C3}{$EXTERNALSYM WAB_CONTEXT_ADRLIST}{$EndIf C3}

  WAB_DISPLAY_ISNTDS  = $00000004;  // Identifies that the entry being displayed originated
                                    // on the NT Directory Service, for clients that use ADSI and
                                    // retrieve additional information from the service.
  {$IfDef C3}{$EXTERNALSYM WAB_DISPLAY_ISNTDS}{$EndIf C3}

//      MAPI_UNICODE        0x80000000  // Indicates that the WED.lpsz string is actually a UNICODE
                                        //  string and should be cast to a (LPWSTR) before using it
                                        // If this flag is not present then the WED.lpsz is a DBCS string
                                        //  and should be cast to an LPSTR before using.

type
  PWabExtDisplay = ^TWabExtDisplay;
  _WABEXTDISPLAY = record
    cbSize: ULONG;
    lpWABObject: IWabObject;    // pointer to IWABObject
    lpAdrBook: IAddrBook;       // pointer to IAdrBook object
    lpPropObj: IMapiProp;       // Object being displayed
    fReadOnly: BOOL;            // Indicates if this is a ReadOnly mode
    fDataChanged: BOOL;         // Set by extension sheet to signal data change
    ulFlags: ULONG;             // See above
    lpv: Pointer;               // Used for passing in specific data
    lpsz: LPTSTR;               // Used for passing in specific data
  end;
  {$IfDef C3}{$EXTERNALSYM _WABEXTDISPLAY}{$EndIf C3}
  TWabExtDisplay = _WABEXTDISPLAY;
  WABEXTDISPLAY = _WABEXTDISPLAY;
  {$IfDef C3}{$EXTERNALSYM WABEXTDISPLAY}{$EndIf C3}

type
  IWABExtInit = interface(IUnknown)
    ['{EA22EBF0-87A4-11D1-9ACF-00A0C91F9C8B}']
    function Initialize(lpWABExtDisplay: PWabExtDisplay): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IWABExtInit}{$EndIf C3}

const
  IID_IWABExtInit = IWABExtInit;
  {$IfDef C3}{$EXTERNALSYM IID_IWABExtInit}{$EndIf C3}

  WAB_DLL_NAME = 'WAB32.DLL';
  {$IfDef C3}{$EXTERNALSYM WAB_DLL_NAME}{$EndIf C3}
  WAB_DLL_PATH_KEY = 'Software\Microsoft\WAB\DLLPath';
  {$IfDef C3}{$EXTERNALSYM WAB_DLL_PATH_KEY}{$EndIf C3}

function GetWabDllPath(var Path: string): Boolean;

function WabApiLoaded: Boolean;
{$IFDEF WAB_DYNAMIC_LINK_EXPLICIT}
function LoadWabApi: Boolean;
function UnloadWabApi: Boolean;
{$ENDIF}

var
  WABOpen: TWABOpen = nil;
  {$IfDef C3}{$EXTERNALSYM WABOpen}{$EndIf C3}
  WABOpenEx: TWABOpenEx = nil;
  {$IfDef C3}{$EXTERNALSYM WABOpenEx}{$EndIf C3}

{******************************************************************************}
implementation
{******************************************************************************}

function GetWabDllPath(var Path: string): Boolean;
var
  Key: HKEY;
  BufSize: DWORD;
  Buffer: array[0..MAX_PATH] of Char;
begin
  Path := '';
  Result := False;
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, WAB_DLL_PATH_KEY, 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    BufSize := Sizeof(Buffer);
    RegQueryValueEx(Key, '', nil, nil, @Buffer, @BufSize);
    RegCloseKey(Key);
    if BufSize > 0 then
    begin
      SetString(Path, Buffer, BufSize - 1);
      Result := True;
    end;
  end;
end;

{--------------------------------------}

var
  LibHandle: THandle = 0;

function WabApiLoaded: Boolean;
begin
  Result := LibHandle <> 0;
end;

{--------------------------------------}

function UnloadWabApi: Boolean;
begin
  if WabApiLoaded then
  begin
    Result := FreeLibrary(LibHandle);
    LibHandle := 0;
    @WabOpen := nil;
    @WabOpenEx := nil;
  end else Result := True;
end;

{--------------------------------------}

function LoadWabApi: Boolean;
var
  WabDllPath: string;
begin
  Result := WabApiLoaded;
  if (not Result) and GetWabDllPath(WabDllPath) then
  begin
    LibHandle := LoadLibrary(PChar(WabDllPath));
    if LibHandle = 0 then
      LibHandle := LoadLibrary(PChar(WAB_DLL_NAME));
    if WabApiLoaded then
    begin
      @WABOpen := GetProcAddress(LibHandle, 'WABOpen');
      @WABOpenEx := GetProcAddress(LibHandle, 'WABOpenEx');
      Result := Assigned(WABOpen) and Assigned(WABOpenEx);
      if not Result then UnloadWabApi;
    end;
  end;
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IFNDEF WAB_DYNAMIC_LINK_EXPLICIT}
  LoadWabApi;
{$ENDIF }

{******************************************************************************}
finalization
{******************************************************************************}
  UnloadWabApi;

end.
