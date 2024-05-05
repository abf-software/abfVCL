{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ Windows Address Book (WAB) functions interface unit              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) 1995-2000 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: wabcode.h, released 31 Jan 2000.           }
{ The original Pascal code is: WabCode.pas, released 15 Mar 2000.  }
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
unit WabCode;

interface

uses
  Windows, ActiveX;

{$I abf.inc} // Delphi or C++Builder determination there...
{$I wab.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

{$IfDef C3}
(*$HPPEMIT '#include <wabcode.h>'*)
(*$HPPEMIT '#include <objerror.h>'*)
{$EndIf C3}

{*
 *  WAB Status codes follow the style of OLE 2.0 sCodes as defined in the
 *  OLE 2.0 Programmer's Reference and header file scode.h (Windows 3.x)
 *  or objerror.h (Windows NT 3.5 and Windows 95).
 *
 */

/*  On Windows 3.x, status codes have 32-bit values as follows:
 *
 *   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
 *   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *  +-+---------------------+-------+-------------------------------+
 *  |S|       Context       | Facil |               Code            |
 *  +-+---------------------+-------+-------------------------------+
 *
 *  where
 *
 *      S - is the severity code
 *
 *          0 - SEVERITY_SUCCESS
 *          1 - SEVERITY_ERROR
 *
 *      Context - context info
 *
 *      Facility - is the facility code
 *
 *          0x0 - FACILITY_NULL     generally useful errors ([SE]_*)
 *          0x1 - FACILITY_RPC      remote procedure call errors (RPC_E_*)
 *          0x2 - FACILITY_DISPATCH late binding dispatch errors
 *          0x3 - FACILITY_STORAGE  storage errors (STG_E_*)
 *          0x4 - FACILITY_ITF      interface-specific errors
 *
 *      Code - is the facility's status code
 *
 *
 *}

{*
 *  On Windows NT 3.5 and Windows 95, scodes are 32-bit values
 *  laid out as follows:
 *
 *    3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
 *    1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 *   +-+-+-+-+-+---------------------+-------------------------------+
 *   |S|R|C|N|r|    Facility         |               Code            |
 *   +-+-+-+-+-+---------------------+-------------------------------+
 *
 *   where
 *
 *      S - Severity - indicates success/fail
 *
 *          0 - Success
 *          1 - Fail (COERROR)
 *
 *      R - reserved portion of the facility code, corresponds to NT's
 *          second severity bit.
 *
 *      C - reserved portion of the facility code, corresponds to NT's
 *          C field.
 *
 *      N - reserved portion of the facility code. Used to indicate a
 *          mapped NT status value.
 *
 *      r - reserved portion of the facility code. Reserved for internal
 *          use. Used to indicate HRESULT values that are not status
 *          values, but are instead message ids for display strings.
 *
 *      Facility - is the facility code
 *          FACILITY_NULL                    0x0
 *          FACILITY_RPC                     0x1
 *          FACILITY_DISPATCH                0x2
 *          FACILITY_STORAGE                 0x3
 *          FACILITY_ITF                     0x4
 *          FACILITY_WIN32                   0x7
 *          FACILITY_WINDOWS                 0x8
 *
 *      Code - is the facility's status code
 *
 *}


{ *  We can't use OLE 2.0 macros to build sCodes because the definition has
  *  changed and we wish to conform to the new definition.
}

function MAKE_MAPI_SCODE(sev,fac,code: DWORD): SCODE;
{$IfDef C3}{$EXTERNALSYM MAKE_MAPI_SCODE}{$EndIf C3}

{ The following two macros are used to build OLE 2.0 style sCodes }

function MAKE_MAPI_E(err: DWORD): SCODE;
{$IfDef C3}{$EXTERNALSYM MAKE_MAPI_E}{$EndIf C3}
function MAKE_MAPI_S(warn: DWORD): SCODE;
{$IfDef C3}{$EXTERNALSYM MAKE_MAPI_S}{$EndIf C3}

const
  SUCCESS_SUCCESS     = 0;
  {$IfDef C3}{$EXTERNALSYM SUCCESS_SUCCESS}{$EndIf C3}

{ General errors (used by more than one WAB object) }

  MAPI_E_CALL_FAILED               = E_FAIL;
  {$IfDef C3}{$EXTERNALSYM MAPI_E_CALL_FAILED}{$EndIf C3}
  MAPI_E_NOT_ENOUGH_MEMORY         = E_OUTOFMEMORY;
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NOT_ENOUGH_MEMORY}{$EndIf C3}
  MAPI_E_INVALID_PARAMETER         = E_INVALIDARG;
  {$IfDef C3}{$EXTERNALSYM MAPI_E_INVALID_PARAMETER}{$EndIf C3}
  MAPI_E_INTERFACE_NOT_SUPPORTED   = E_NOINTERFACE;
  {$IfDef C3}{$EXTERNALSYM MAPI_E_INTERFACE_NOT_SUPPORTED}{$EndIf C3}
  MAPI_E_NO_ACCESS                 = E_ACCESSDENIED;
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NO_ACCESS}{$EndIf C3}

  MAPI_E_NO_SUPPORT                = (1 shl 31 or FACILITY_ITF shl 16 or $102);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NO_SUPPORT}{$EndIf C3}
  MAPI_E_BAD_CHARWIDTH             = (1 shl 31 or FACILITY_ITF shl 16 or $103);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_BAD_CHARWIDTH}{$EndIf C3}
  MAPI_E_STRING_TOO_LONG           = (1 shl 31 or FACILITY_ITF shl 16 or $105);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_STRING_TOO_LONG}{$EndIf C3}
  MAPI_E_UNKNOWN_FLAGS             = (1 shl 31 or FACILITY_ITF shl 16 or $106);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_UNKNOWN_FLAGS}{$EndIf C3}
  MAPI_E_INVALID_ENTRYID           = (1 shl 31 or FACILITY_ITF shl 16 or $107);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_INVALID_ENTRYID}{$EndIf C3}
  MAPI_E_INVALID_OBJECT            = (1 shl 31 or FACILITY_ITF shl 16 or $108);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_INVALID_OBJECT}{$EndIf C3}
  MAPI_E_OBJECT_CHANGED            = (1 shl 31 or FACILITY_ITF shl 16 or $109);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_OBJECT_CHANGED}{$EndIf C3}
  MAPI_E_OBJECT_DELETED            = (1 shl 31 or FACILITY_ITF shl 16 or $10A);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_OBJECT_DELETED}{$EndIf C3}
  MAPI_E_BUSY                      = (1 shl 31 or FACILITY_ITF shl 16 or $10B);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_BUSY}{$EndIf C3}
  MAPI_E_NOT_ENOUGH_DISK           = (1 shl 31 or FACILITY_ITF shl 16 or $10D);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NOT_ENOUGH_DISK}{$EndIf C3}
  MAPI_E_NOT_ENOUGH_RESOURCES      = (1 shl 31 or FACILITY_ITF shl 16 or $10E);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NOT_ENOUGH_RESOURCES}{$EndIf C3}
  MAPI_E_NOT_FOUND                 = (1 shl 31 or FACILITY_ITF shl 16 or $10F);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NOT_FOUND}{$EndIf C3}
  MAPI_E_VERSION                   = (1 shl 31 or FACILITY_ITF shl 16 or $110);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_VERSION}{$EndIf C3}
  MAPI_E_LOGON_FAILED              = (1 shl 31 or FACILITY_ITF shl 16 or $111);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_LOGON_FAILED}{$EndIf C3}
  MAPI_E_SESSION_LIMIT             = (1 shl 31 or FACILITY_ITF shl 16 or $112);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_SESSION_LIMIT}{$EndIf C3}
  MAPI_E_USER_CANCEL               = (1 shl 31 or FACILITY_ITF shl 16 or $113);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_USER_CANCEL}{$EndIf C3}
  MAPI_E_UNABLE_TO_ABORT           = (1 shl 31 or FACILITY_ITF shl 16 or $114);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_UNABLE_TO_ABORT}{$EndIf C3}
  MAPI_E_NETWORK_ERROR             = (1 shl 31 or FACILITY_ITF shl 16 or $115);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NETWORK_ERROR}{$EndIf C3}
  MAPI_E_DISK_ERROR                = (1 shl 31 or FACILITY_ITF shl 16 or $116);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_DISK_ERROR}{$EndIf C3}
  MAPI_E_TOO_COMPLEX               = (1 shl 31 or FACILITY_ITF shl 16 or $117);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_TOO_COMPLEX}{$EndIf C3}
  MAPI_E_BAD_COLUMN                = (1 shl 31 or FACILITY_ITF shl 16 or $118);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_BAD_COLUMN}{$EndIf C3}
  MAPI_E_EXTENDED_ERROR            = (1 shl 31 or FACILITY_ITF shl 16 or $119);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_EXTENDED_ERROR}{$EndIf C3}
  MAPI_E_COMPUTED                  = (1 shl 31 or FACILITY_ITF shl 16 or $11A);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_COMPUTED}{$EndIf C3}
  MAPI_E_CORRUPT_DATA              = (1 shl 31 or FACILITY_ITF shl 16 or $11B);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_CORRUPT_DATA}{$EndIf C3}
  MAPI_E_UNCONFIGURED              = (1 shl 31 or FACILITY_ITF shl 16 or $11C);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_UNCONFIGURED}{$EndIf C3}
  MAPI_E_FAILONEPROVIDER           = (1 shl 31 or FACILITY_ITF shl 16 or $11D);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_FAILONEPROVIDER}{$EndIf C3}

{ WAB base function and status object specific errors and warnings }

  MAPI_E_END_OF_SESSION            = (1 shl 31 or FACILITY_ITF shl 16 or $200);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_END_OF_SESSION}{$EndIf C3}
  MAPI_E_UNKNOWN_ENTRYID           = (1 shl 31 or FACILITY_ITF shl 16 or $201);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_UNKNOWN_ENTRYID}{$EndIf C3}
  MAPI_E_MISSING_REQUIRED_COLUMN   = (1 shl 31 or FACILITY_ITF shl 16 or $202);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_MISSING_REQUIRED_COLUMN}{$EndIf C3}
  MAPI_W_NO_SERVICE                = (FACILITY_ITF shl 16 or $203);
  {$IfDef C3}{$EXTERNALSYM MAPI_W_NO_SERVICE}{$EndIf C3}

{ Property specific errors and warnings }

  MAPI_E_BAD_VALUE                 = (1 shl 31 or FACILITY_ITF shl 16 or $301);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_BAD_VALUE}{$EndIf C3}
  MAPI_E_INVALID_TYPE              = (1 shl 31 or FACILITY_ITF shl 16 or $302);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_INVALID_TYPE}{$EndIf C3}
  MAPI_E_TYPE_NO_SUPPORT           = (1 shl 31 or FACILITY_ITF shl 16 or $303);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_TYPE_NO_SUPPORT}{$EndIf C3}
  MAPI_E_UNEXPECTED_TYPE           = (1 shl 31 or FACILITY_ITF shl 16 or $304);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_UNEXPECTED_TYPE}{$EndIf C3}
  MAPI_E_TOO_BIG                   = (1 shl 31 or FACILITY_ITF shl 16 or $305);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_TOO_BIG}{$EndIf C3}
  MAPI_E_DECLINE_COPY              = (1 shl 31 or FACILITY_ITF shl 16 or $306);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_DECLINE_COPY}{$EndIf C3}
  MAPI_E_UNEXPECTED_ID             = (1 shl 31 or FACILITY_ITF shl 16 or $307);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_UNEXPECTED_ID}{$EndIf C3}

  MAPI_W_ERRORS_RETURNED           = (FACILITY_ITF shl 16 or $380);
  {$IfDef C3}{$EXTERNALSYM MAPI_W_ERRORS_RETURNED}{$EndIf C3}

{ Table specific errors and warnings }

  MAPI_E_UNABLE_TO_COMPLETE        = (1 shl 31 or FACILITY_ITF shl 16 or $400);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_UNABLE_TO_COMPLETE}{$EndIf C3}
  MAPI_E_TIMEOUT                   = (1 shl 31 or FACILITY_ITF shl 16 or $401);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_TIMEOUT}{$EndIf C3}
  MAPI_E_TABLE_EMPTY               = (1 shl 31 or FACILITY_ITF shl 16 or $402);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_TABLE_EMPTY}{$EndIf C3}
  MAPI_E_TABLE_TOO_BIG             = (1 shl 31 or FACILITY_ITF shl 16 or $403);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_TABLE_TOO_BIG}{$EndIf C3}

  MAPI_E_INVALID_BOOKMARK          = (1 shl 31 or FACILITY_ITF shl 16 or $405);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_INVALID_BOOKMARK}{$EndIf C3}

  MAPI_W_POSITION_CHANGED          = (FACILITY_ITF shl 16 or $481);
  {$IfDef C3}{$EXTERNALSYM MAPI_W_POSITION_CHANGED}{$EndIf C3}
  MAPI_W_APPROX_COUNT              = (FACILITY_ITF shl 16 or $482);
  {$IfDef C3}{$EXTERNALSYM MAPI_W_APPROX_COUNT}{$EndIf C3}

  MAPI_W_PARTIAL_COMPLETION        = (FACILITY_ITF shl 16 or $680);
  {$IfDef C3}{$EXTERNALSYM MAPI_W_PARTIAL_COMPLETION}{$EndIf C3}

{ Address Book specific errors and warnings }

  MAPI_E_AMBIGUOUS_RECIP           = (1 shl 31 or FACILITY_ITF shl 16 or $700);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_AMBIGUOUS_RECIP}{$EndIf C3}

{ Miscellaneous errors }

  MAPI_E_COLLISION                 = (1 shl 31 or FACILITY_ITF shl 16 or $604);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_COLLISION}{$EndIf C3}
  MAPI_E_NOT_INITIALIZED           = (1 shl 31 or FACILITY_ITF shl 16 or $605);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_NOT_INITIALIZED}{$EndIf C3}
  MAPI_E_FOLDER_CYCLE              = (1 shl 31 or FACILITY_ITF shl 16 or $60B);
  {$IfDef C3}{$EXTERNALSYM MAPI_E_FOLDER_CYCLE}{$EndIf C3}

{ The range 0x0800 to 0x08FF is reserved }

{* We expect these to eventually be defined by OLE, but for now,
 * here they are.  When OLE defines them they can be much more
 * efficient than these, but these are "proper" and don't make
 * use of any hidden tricks.
}

function HR_SUCCEEDED(_hr: HRESULT): Boolean;
{$IfDef C3}{$EXTERNALSYM HR_SUCCEEDED}{$EndIf C3}
function HR_FAILED(_hr: HRESULT): Boolean;
{$IfDef C3}{$EXTERNALSYM HR_FAILED}{$EndIf C3}

{******************************************************************************}
implementation
{******************************************************************************}

function MAKE_MAPI_SCODE(sev,fac,code: DWORD): SCODE;
begin
  Result := sev shl 31 or fac shl 16 or code;
end;

{--------------------------------------}

function MAKE_MAPI_E(err: DWORD): SCODE;
begin
  Result := MAKE_MAPI_SCODE(1, FACILITY_ITF, err);
end;

{--------------------------------------}

function MAKE_MAPI_S(warn: DWORD): SCODE;
begin
  Result := MAKE_MAPI_SCODE(0, FACILITY_ITF, warn);
end;

{--------------------------------------}

function HR_SUCCEEDED(_hr: HRESULT): Boolean;
begin
  Result := Succeeded(_hr);
end;

{--------------------------------------}

function HR_FAILED(_hr: HRESULT): Boolean;
begin
  Result := Failed(_hr);
end;

{--------------------------------------}

end.
