{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ Windows Address Book (WAB) functions interface unit              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) 1995-2000 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: wabdefs.h, released 31 Jan 2000.           }
{ The original Pascal code is: WabDefs.pas, released 15 Mar 2000.  }
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
unit WabDefs;

interface

uses
  Windows, ActiveX;

{$I abf.inc} // Delphi or C++Builder determination there...
{$I wab.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

{$IfDef C3}
(*$HPPEMIT '#include <wabdefs.h>'*)
(*$HPPEMIT '#include <windows.h>'*)
(*$HPPEMIT '#include <objerror.h>'*)
(*$HPPEMIT '#include <objbase.h>'*)
(*$HPPEMIT '#include <stddef.h>'*)
{$EndIf C3}

{ Array dimension for structures with variable-sized arrays at the end. }

const
  MAPI_DIM    = 1;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIM}{$EndIf C3}

{*  This flag is used in many different MAPI calls to signify that
 *  the object opened by the call should be modifiable (MAPI_MODIFY).
 *  If the flag MAPI_MAX_ACCESS is set, the object returned should be
 *  returned at the maximum access level allowed.  An additional
 *  property available on the object (PR_ACCESS_LEVEL) uses the same
 *  MAPI_MODIFY flag to say just what this new access level is.
}

  MAPI_MODIFY               = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_MODIFY}{$EndIf C3}

{*  The following flags are used to indicate to the client what access
 *  level is permissible in the object. They appear in PR_ACCESS in
 *  message and folder objects as well as in contents and associated
 *  contents tables
}

  MAPI_ACCESS_MODIFY                    = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_ACCESS_MODIFY}{$EndIf C3}
  MAPI_ACCESS_READ                      = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MAPI_ACCESS_READ}{$EndIf C3}
  MAPI_ACCESS_DELETE                    = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM MAPI_ACCESS_DELETE}{$EndIf C3}
  MAPI_ACCESS_CREATE_HIERARCHY          = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM MAPI_ACCESS_CREATE_HIERARCHY}{$EndIf C3}
  MAPI_ACCESS_CREATE_CONTENTS           = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM MAPI_ACCESS_CREATE_CONTENTS}{$EndIf C3}
  MAPI_ACCESS_CREATE_ASSOCIATED         = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM MAPI_ACCESS_CREATE_ASSOCIATED}{$EndIf C3}

{*  The MAPI_UNICODE flag is used in many different MAPI calls to signify
 *  that strings passed through the interface are in Unicode (a 16-bit
 *  character set). The default is an 8-bit character set.
 *
 *  The value fMapiUnicode can be used as the 'normal' value for
 *  that bit, given the application's default character set.
}

  MAPI_UNICODE = ULONG($80000000);
  {$IfDef C3}{$EXTERNALSYM MAPI_UNICODE}{$EndIf C3}

  hrSuccess    = 0;
  {$IfDef C3}{$EXTERNALSYM hrSuccess}{$EndIf C3}

{ Recipient types }
  MAPI_ORIG   = 0;             // Recipient is message originator
  {$IfDef C3}{$EXTERNALSYM MAPI_ORIG}{$EndIf C3}
  MAPI_TO     = 1;             // Recipient is a primary recipient
  {$IfDef C3}{$EXTERNALSYM MAPI_TO}{$EndIf C3}
  MAPI_CC     = 2;             // Recipient is a copy recipient
  {$IfDef C3}{$EXTERNALSYM MAPI_CC}{$EndIf C3}
  MAPI_BCC    = 3;             // Recipient is blind copy recipient
  {$IfDef C3}{$EXTERNALSYM MAPI_BCC}{$EndIf C3}
  MAPI_P1        = $10000000;  // Recipient is a P1 resend recipient
  {$IfDef C3}{$EXTERNALSYM MAPI_P1}{$EndIf C3}
  MAPI_SUBMITTED = $80000000;  // Recipient is already processed
  {$IfDef C3}{$EXTERNALSYM MAPI_SUBMITTED}{$EndIf C3}
  MAPI_AUTHORIZE = 4;          // Recipient is a CMC authorizing user
  {$IfDef C3}{$EXTERNALSYM MAPI_AUTHORIZE}{$EndIf C3}
  MAPI_DISCRETE  = $10000000;  // Recipient is a P1 resend recipient
  {$IfDef C3}{$EXTERNALSYM MAPI_DISCRETE}{$EndIf C3}

{ Bit definitions for abFlags[0] of ENTRYID }
  MAPI_SHORTTERM          = $80;
  {$IfDef C3}{$EXTERNALSYM MAPI_SHORTTERM}{$EndIf C3}
  MAPI_NOTRECIP           = $40;
  {$IfDef C3}{$EXTERNALSYM MAPI_NOTRECIP}{$EndIf C3}
  MAPI_THISSESSION        = $20;
  {$IfDef C3}{$EXTERNALSYM MAPI_THISSESSION}{$EndIf C3}
  MAPI_NOW                = $10;
  {$IfDef C3}{$EXTERNALSYM MAPI_NOW}{$EndIf C3}
  MAPI_NOTRESERVED        = $08;
  {$IfDef C3}{$EXTERNALSYM MAPI_NOTRESERVED}{$EndIf C3}

{ Bit definitions for abFlags[1] of ENTRYID }
  MAPI_COMPOUND           = $80;
  {$IfDef C3}{$EXTERNALSYM MAPI_COMPOUND}{$EndIf C3}

type

{--------------------------------------}
{ Delphi 3 routines }

{$IfNDef C3}
  {$IfDef UNICODE}
  LPTSTR = PWideChar;
  {$Else UNICODE}
  LPTSTR = PAnsiChar;
  {$EndIf UNICODE}
{$EndIf C3}


  PEntryID = ^TEntryID;
  _ENTRYID = record
    abFlags: array[0..3] of Byte;
    ab: array[0..MAPI_DIM-1] of Byte;
  end;
  //{$IfDef C3}{$EXTERNALSYM _ENTRYID}{$EndIf C3}
  TEntryID = _ENTRYID;
  ENTRYID = _ENTRYID;
  {$IfDef C3}{$EXTERNALSYM ENTRYID}{$EndIf C3}

(*!!!
#define CbNewENTRYID(_cb)       (offsetof(ENTRYID,ab) + (_cb))
#define CbENTRYID(_cb)          (offsetof(ENTRYID,ab) + (_cb))
#define SizedENTRYID(_cb, _name) \
    struct _ENTRYID_ ## _name \
{ \
    BYTE    abFlags[4]; \
    BYTE    ab[_cb]; \
} _name
*)

{ Byte-order-independent version of GUID (world-unique identifier) }

  PMapiUID = ^TMapiUID;
  _MAPIUID = record
    ab: array[0..15] of Byte;
  end;
  {$IfDef C3}{$EXTERNALSYM _MAPIUID}{$EndIf C3}
  TMapiUID = _MAPIUID;
  MAPIUID = _MAPIUID;
  {$IfDef C3}{$EXTERNALSYM MAPIUID}{$EndIf C3}

function IsEqualMAPIUID(lpuid1, lpuid2: TMapiUID): Boolean;
{$IfDef C3}{$EXTERNALSYM IsEqualMAPIUID}{$EndIf C3}

const
  MAPI_STORE      = ULONG($00000001);    // Message Store
  {$IfDef C3}{$EXTERNALSYM MAPI_STORE}{$EndIf C3}
  MAPI_ADDRBOOK   = ULONG($00000002);    // Address Book
  {$IfDef C3}{$EXTERNALSYM MAPI_ADDRBOOK}{$EndIf C3}
  MAPI_FOLDER     = ULONG($00000003);    // Folder
  {$IfDef C3}{$EXTERNALSYM MAPI_FOLDER}{$EndIf C3}
  MAPI_ABCONT     = ULONG($00000004);    // Address Book Container
  {$IfDef C3}{$EXTERNALSYM MAPI_ABCONT}{$EndIf C3}
  MAPI_MESSAGE    = ULONG($00000005);    // Message
  {$IfDef C3}{$EXTERNALSYM MAPI_MESSAGE}{$EndIf C3}
  MAPI_MAILUSER   = ULONG($00000006);    // Individual Recipient
  {$IfDef C3}{$EXTERNALSYM MAPI_MAILUSER}{$EndIf C3}
  MAPI_ATTACH     = ULONG($00000007);    // Attachment
  {$IfDef C3}{$EXTERNALSYM MAPI_ATTACH}{$EndIf C3}
  MAPI_DISTLIST   = ULONG($00000008);    // Distribution List Recipient
  {$IfDef C3}{$EXTERNALSYM MAPI_DISTLIST}{$EndIf C3}
  MAPI_PROFSECT   = ULONG($00000009);    // Profile Section
  {$IfDef C3}{$EXTERNALSYM MAPI_PROFSECT}{$EndIf C3}
  MAPI_STATUS     = ULONG($0000000A);    // Status Object
  {$IfDef C3}{$EXTERNALSYM MAPI_STATUS}{$EndIf C3}
  MAPI_SESSION    = ULONG($0000000B);    // Session
  {$IfDef C3}{$EXTERNALSYM MAPI_SESSION}{$EndIf C3}
  MAPI_FORMINFO   = ULONG($0000000C);    // Form Information
  {$IfDef C3}{$EXTERNALSYM MAPI_FORMINFO}{$EndIf C3}

{ Maximum length of profile names and passwords, not including the null
  termination character. }

  cchProfileNameMax   = 64;
  {$IfDef C3}{$EXTERNALSYM cchProfileNameMax}{$EndIf C3}
  cchProfilePassMax   = 64;
  {$IfDef C3}{$EXTERNALSYM cchProfilePassMax}{$EndIf C3}

{ Property Types }

  MV_FLAG         = $1000;        // Multi-value flag
  {$IfDef C3}{$EXTERNALSYM MV_FLAG}{$EndIf C3}

  PT_UNSPECIFIED  = ULONG(0);     // (Reserved for interface use) type doesn't matter to caller
  {$IfDef C3}{$EXTERNALSYM PT_UNSPECIFIED}{$EndIf C3}
  PT_NULL         = ULONG(1);     // NULL property value
  {$IfDef C3}{$EXTERNALSYM PT_NULL}{$EndIf C3}
  PT_I2           = ULONG(2);     // Signed 16-bit value
  {$IfDef C3}{$EXTERNALSYM PT_I2}{$EndIf C3}
  PT_LONG         = ULONG(3);     // Signed 32-bit value
  {$IfDef C3}{$EXTERNALSYM PT_LONG}{$EndIf C3}
  PT_R4           = ULONG(4);     // 4-byte floating point
  {$IfDef C3}{$EXTERNALSYM PT_R4}{$EndIf C3}
  PT_DOUBLE       = ULONG(5);     // Floating point double
  {$IfDef C3}{$EXTERNALSYM PT_DOUBLE}{$EndIf C3}
  PT_CURRENCY     = ULONG(6);     // Signed 64-bit int (decimal w/    4 digits right of decimal pt)
  {$IfDef C3}{$EXTERNALSYM PT_CURRENCY}{$EndIf C3}
  PT_APPTIME      = ULONG(7);     // Application time
  {$IfDef C3}{$EXTERNALSYM PT_APPTIME}{$EndIf C3}
  PT_ERROR        = ULONG(10);    // 32-bit error value
  {$IfDef C3}{$EXTERNALSYM PT_ERROR}{$EndIf C3}
  PT_BOOLEAN      = ULONG(11);    // 16-bit boolean (non-zero true)
  {$IfDef C3}{$EXTERNALSYM PT_BOOLEAN}{$EndIf C3}
  PT_OBJECT       = ULONG(13);    // Embedded object in a property
  {$IfDef C3}{$EXTERNALSYM PT_OBJECT}{$EndIf C3}
  PT_I8           = ULONG(20);    // 8-byte signed integer
  {$IfDef C3}{$EXTERNALSYM PT_I8}{$EndIf C3}
  PT_STRING8      = ULONG(30);    // Null terminated 8-bit character string
  {$IfDef C3}{$EXTERNALSYM PT_STRING8}{$EndIf C3}
  PT_UNICODE      = ULONG(31);    // Null terminated Unicode string
  {$IfDef C3}{$EXTERNALSYM PT_UNICODE}{$EndIf C3}
  PT_SYSTIME      = ULONG(64);    // FILETIME 64-bit int w/ number of 100ns periods since Jan 1,1601
  {$IfDef C3}{$EXTERNALSYM PT_SYSTIME}{$EndIf C3}
  PT_CLSID        = ULONG(72);    // OLE GUID
  {$IfDef C3}{$EXTERNALSYM PT_CLSID}{$EndIf C3}
  PT_BINARY       = ULONG(258);   // Uninterpreted (counted byte array)
  {$IfDef C3}{$EXTERNALSYM PT_BINARY}{$EndIf C3}

{ Alternate property type names for ease of use }
  PT_SHORT    = PT_I2;
  {$IfDef C3}{$EXTERNALSYM PT_SHORT}{$EndIf C3}
  PT_I4       = PT_LONG;
  {$IfDef C3}{$EXTERNALSYM PT_I4}{$EndIf C3}
  PT_FLOAT    = PT_R4;
  {$IfDef C3}{$EXTERNALSYM PT_FLOAT}{$EndIf C3}
  PT_R8       = PT_DOUBLE;
  {$IfDef C3}{$EXTERNALSYM PT_R8}{$EndIf C3}
  PT_LONGLONG = PT_I8;
  {$IfDef C3}{$EXTERNALSYM PT_LONGLONG}{$EndIf C3}

{*  The type of a MAPI-defined string property is indirected, so
 *  that it defaults to Unicode string on a Unicode platform and to
 *  String8 on an ANSI or DBCS platform.
 *
 *  Macros are defined here both for the property type, and for the
 *  field of the property value structure which should be
 *  dereferenced to obtain the string pointer.
}

{$IFDEF UNICODE}
  PT_TSTRING          = PT_UNICODE;
  {$IfDef C3}{$EXTERNALSYM PT_TSTRING}{$EndIf C3}
  PT_MV_TSTRING       = MV_FLAG or PT_UNICODE;
  {$IfDef C3}{$EXTERNALSYM PT_MV_TSTRING}{$EndIf C3}
{$ELSE}
  PT_TSTRING          = PT_STRING8;
  {$IfDef C3}{$EXTERNALSYM PT_TSTRING}{$EndIf C3}
  PT_MV_TSTRING       = MV_FLAG or PT_STRING8;
  {$IfDef C3}{$EXTERNALSYM PT_MV_TSTRING}{$EndIf C3}
{$ENDIF}

{* Property Tags
 *
 * By convention, MAPI never uses 0 or FFFF as a property ID.
 * Use as null values, initializers, sentinels, or what have you.
}

  PROP_TYPE_MASK          = ULONG($0000FFFF); // Mask for Property type
  {$IfDef C3}{$EXTERNALSYM PROP_TYPE_MASK}{$EndIf C3}
  PROP_ID_NULL            = 0;
  {$IfDef C3}{$EXTERNALSYM PROP_ID_NULL}{$EndIf C3}
  PROP_ID_INVALID         = $FFFF;
  {$IfDef C3}{$EXTERNALSYM PROP_ID_INVALID}{$EndIf C3}
  PR_NULL                 = PT_NULL shl 16 or PROP_ID_NULL;
  {$IfDef C3}{$EXTERNALSYM PR_NULL}{$EndIf C3}

function PROP_TYPE(ulPropTag: ULONG): ULONG;
{$IfDef C3}{$EXTERNALSYM PROP_TYPE}{$EndIf C3}
function PROP_ID(ulPropTag: ULONG): ULONG;
{$IfDef C3}{$EXTERNALSYM PROP_ID}{$EndIf C3}
function PROP_TAG(ulPropType, ulPropID: ULONG): ULONG;
{$IfDef C3}{$EXTERNALSYM PROP_TAG}{$EndIf C3}
function CHANGE_PROP_TYPE(ulPropTag, ulPropType: ULONG): ULONG;
{$IfDef C3}{$EXTERNALSYM CHANGE_PROP_TYPE}{$EndIf C3}

const
{ Multi-valued Property Types }
  PT_MV_I2        = (MV_FLAG or PT_I2);
  {$IfDef C3}{$EXTERNALSYM PT_MV_I2}{$EndIf C3}
  PT_MV_LONG      = (MV_FLAG or PT_LONG);
  {$IfDef C3}{$EXTERNALSYM PT_MV_LONG}{$EndIf C3}
  PT_MV_R4        = (MV_FLAG or PT_R4);
  {$IfDef C3}{$EXTERNALSYM PT_MV_R4}{$EndIf C3}
  PT_MV_DOUBLE    = (MV_FLAG or PT_DOUBLE);
  {$IfDef C3}{$EXTERNALSYM PT_MV_DOUBLE}{$EndIf C3}
  PT_MV_CURRENCY  = (MV_FLAG or PT_CURRENCY);
  {$IfDef C3}{$EXTERNALSYM PT_MV_CURRENCY}{$EndIf C3}
  PT_MV_APPTIME   = (MV_FLAG or PT_APPTIME);
  {$IfDef C3}{$EXTERNALSYM PT_MV_APPTIME}{$EndIf C3}
  PT_MV_SYSTIME   = (MV_FLAG or PT_SYSTIME);
  {$IfDef C3}{$EXTERNALSYM PT_MV_SYSTIME}{$EndIf C3}
  PT_MV_STRING8   = (MV_FLAG or PT_STRING8);
  {$IfDef C3}{$EXTERNALSYM PT_MV_STRING8}{$EndIf C3}
  PT_MV_BINARY    = (MV_FLAG or PT_BINARY);
  {$IfDef C3}{$EXTERNALSYM PT_MV_BINARY}{$EndIf C3}
  PT_MV_UNICODE   = (MV_FLAG or PT_UNICODE);
  {$IfDef C3}{$EXTERNALSYM PT_MV_UNICODE}{$EndIf C3}
  PT_MV_CLSID     = (MV_FLAG or PT_CLSID);
  {$IfDef C3}{$EXTERNALSYM PT_MV_CLSID}{$EndIf C3}
  PT_MV_I8        = (MV_FLAG or PT_I8);
  {$IfDef C3}{$EXTERNALSYM PT_MV_I8}{$EndIf C3}

{ Alternate property type names for ease of use }
  PT_MV_SHORT     = PT_MV_I2;
  {$IfDef C3}{$EXTERNALSYM PT_MV_SHORT}{$EndIf C3}
  PT_MV_I4        = PT_MV_LONG;
  {$IfDef C3}{$EXTERNALSYM PT_MV_I4}{$EndIf C3}
  PT_MV_FLOAT     = PT_MV_R4;
  {$IfDef C3}{$EXTERNALSYM PT_MV_FLOAT}{$EndIf C3}
  PT_MV_R8        = PT_MV_DOUBLE;
  {$IfDef C3}{$EXTERNALSYM PT_MV_R8}{$EndIf C3}
  PT_MV_LONGLONG  = PT_MV_I8;
  {$IfDef C3}{$EXTERNALSYM PT_MV_LONGLONG}{$EndIf C3}

{*  Property type reserved bits
 *
 *  MV_INSTANCE is used as a flag in table operations to request
 *  that a multi-valued property be presented as a single-valued
 *  property appearing in multiple rows.
}

  MV_INSTANCE     = $2000;
  {$IfDef C3}{$EXTERNALSYM MV_INSTANCE}{$EndIf C3}
  MVI_FLAG        = (MV_FLAG or MV_INSTANCE);
  {$IfDef C3}{$EXTERNALSYM MVI_FLAG}{$EndIf C3}

function MVI_PROP(tag: ULONG): ULONG;
{$IfDef C3}{$EXTERNALSYM MVI_PROP}{$EndIf C3}

{ Data Structures }

{ Property Tag Array }

type
  PSPropTagArray = ^TSPropTagArray;
  _SPropTagArray = record
    cValues: ULONG;
    aulPropTag: array[0..MAPI_DIM-1] of ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _SPropTagArray}{$EndIf C3}
  TSPropTagArray = _SPropTagArray;
  SPropTagArray = _SPropTagArray;
  {$IfDef C3}{$EXTERNALSYM SPropTagArray}{$EndIf C3}

(*!!!
#define CbNewSPropTagArray(_ctag) \
    (offsetof(SPropTagArray,aulPropTag) + (_ctag)*sizeof(ULONG))
#define CbSPropTagArray(_lparray) \
    (offsetof(SPropTagArray,aulPropTag) + \
    (UINT)((_lparray)->cValues)*sizeof(ULONG))
/*  SPropTagArray
#define SizedSPropTagArray(_ctag, _name) \
struct _SPropTagArray_ ## _name \
{ \
    ULONG   cValues; \
    ULONG   aulPropTag[_ctag]; \
} _name
*)

{ 32-bit CURRENCY definition stolen from oaidl.h }

{ real definition that makes the C++ compiler happy }

type
  PCY = ^TCY;
  tagCY = record
    case Integer of
      1: (
           {$IFDEF _MAC}
           Hi : Longint;
           Lo : Longint;
           {$ELSE}
           Lo : Longint;
           Hi : Longint;
           {$ENDIF}
         );
      2: (
         int64: LONGLONG;
         );
  end;
  {$IfDef C3}{$EXTERNALSYM tagCY}{$EndIf C3}
  TCY = tagCY;
  CY = tagCY;
  {$IfDef C3}{$EXTERNALSYM CY}{$EndIf C3}

  TCURRENCY = TCY;

  PSBinary = ^TSBinary;
  _SBinary = record
    cb: ULONG;
    lpb: Pointer;
  end;
  {$IfDef C3}{$EXTERNALSYM _SBinary}{$EndIf C3}
  TSBinary = _SBinary;
  SBinary = _SBinary;
  {$IfDef C3}{$EXTERNALSYM SBinary}{$EndIf C3}

  PSShortArray = ^TSShortArray;
  _SShortArray = record
    cValues: ULONG;
    lpi: ^SmallInt;
  end;
  {$IfDef C3}{$EXTERNALSYM _SShortArray}{$EndIf C3}
  TSShortArray = _SShortArray;
  SShortArray = _SShortArray;
  {$IfDef C3}{$EXTERNALSYM SShortArray}{$EndIf C3}

  PSGuidArray = ^TSGuidArray;
  _SGuidArray = record
    cValues: ULONG;
    lpguid: ^TGUID;
  end;
  {$IfDef C3}{$EXTERNALSYM _SGuidArray}{$EndIf C3}
  TSGuidArray = _SGuidArray;
  SGuidArray = _SGuidArray;
  {$IfDef C3}{$EXTERNALSYM SGuidArray}{$EndIf C3}

  PSRealArray = ^TSRealArray;
  _SRealArray = record
    cValues: ULONG;
    lpflt: ^Single;
  end;
  {$IfDef C3}{$EXTERNALSYM _SRealArray}{$EndIf C3}
  TSRealArray = _SRealArray;
  SRealArray = _SRealArray;
  {$IfDef C3}{$EXTERNALSYM SRealArray}{$EndIf C3}

  PSLongArray = ^TSLongArray;
  _SLongArray = record
    cValues: ULONG;
    lpl: ^LongInt;
  end;
  {$IfDef C3}{$EXTERNALSYM _SLongArray}{$EndIf C3}
  TSLongArray = _SLongArray;
  SLongArray = _SLongArray;
  {$IfDef C3}{$EXTERNALSYM SLongArray}{$EndIf C3}

  PSLargeIntegerArray = ^TSLargeIntegerArray;
  _SLargeIntegerArray = record
    cValues: ULONG;
    lpli: ^TLargeInteger;
  end;
  {$IfDef C3}{$EXTERNALSYM _SLargeIntegerArray}{$EndIf C3}
  TSLargeIntegerArray = _SLargeIntegerArray;
  SLargeIntegerArray = _SLargeIntegerArray;
  {$IfDef C3}{$EXTERNALSYM SLargeIntegerArray}{$EndIf C3}

  PSDateTimeArray = ^TSDateTimeArray;
  _SDateTimeArray = record
    cValues: ULONG;
    lpft: ^TFileTime;
  end;
  {$IfDef C3}{$EXTERNALSYM _SDateTimeArray}{$EndIf C3}
  TSDateTimeArray = _SDateTimeArray;
  SDateTimeArray = _SDateTimeArray;
  {$IfDef C3}{$EXTERNALSYM SDateTimeArray}{$EndIf C3}

  PSAppTimeArray = ^TSAppTimeArray;
  _SAppTimeArray = record
    cValues: ULONG;
    lpat: ^Double;
  end;
  {$IfDef C3}{$EXTERNALSYM _SAppTimeArray}{$EndIf C3}
  TSAppTimeArray = _SAppTimeArray;
  SAppTimeArray = _SAppTimeArray;
  {$IfDef C3}{$EXTERNALSYM SAppTimeArray}{$EndIf C3}

  PSCurrencyArray = ^TSCurrencyArray;
  _SCurrencyArray = record
    cValues: ULONG;
    lpcur: ^Currency;
  end;
  {$IfDef C3}{$EXTERNALSYM _SCurrencyArray}{$EndIf C3}
  TSCurrencyArray = _SCurrencyArray;
  SCurrencyArray = _SCurrencyArray;
  {$IfDef C3}{$EXTERNALSYM SCurrencyArray}{$EndIf C3}

  PSBinaryArray = ^TSBinaryArray;
  _SBinaryArray = record
    cValues: ULONG;
    lpbin: PSBinary;
  end;
  {$IfDef C3}{$EXTERNALSYM _SBinaryArray}{$EndIf C3}
  TSBinaryArray = _SBinaryArray;
  SBinaryArray = _SBinaryArray;
  {$IfDef C3}{$EXTERNALSYM SBinaryArray}{$EndIf C3}

  PSDoubleArray = ^TSDoubleArray;
  _SDoubleArray = record
    cValues: ULONG;
    lpdbl: ^Double;
  end;
  {$IfDef C3}{$EXTERNALSYM _SDoubleArray}{$EndIf C3}
  TSDoubleArray = _SDoubleArray;
  SDoubleArray = _SDoubleArray;
  {$IfDef C3}{$EXTERNALSYM SDoubleArray}{$EndIf C3}

  PSWStringArray = ^TSWStringArray;
  _SWStringArray = record
    cValues: ULONG;
    lppszW: ^LPWSTR;
  end;
  {$IfDef C3}{$EXTERNALSYM _SWStringArray}{$EndIf C3}
  TSWStringArray = _SWStringArray;
  SWStringArray = _SWStringArray;
  {$IfDef C3}{$EXTERNALSYM SWStringArray}{$EndIf C3}

  PSLPSTRArray = ^TSLPSTRArray;
  _SLPSTRArray = record
    cValues: ULONG;
    lppszA: ^LPSTR;
  end;
  {$IfDef C3}{$EXTERNALSYM _SLPSTRArray}{$EndIf C3}
  TSLPSTRArray = _SLPSTRArray;
  SLPSTRArray = _SLPSTRArray;
  {$IfDef C3}{$EXTERNALSYM SLPSTRArray}{$EndIf C3}

  __UPV = record
    case Integer of
      0: (i: SmallInt;);             // case PT_I2
      1: (l: LongInt;);              // case PT_LONG
      2: (ul: ULONG;);               // alias for PT_LONG
      3: (flt: Single;);             // case PT_R4
      4: (dbl: Double;);             // case PT_DOUBLE
      5: (b: Word;);                 // case PT_BOOLEAN
      6: (cur: Currency;);           // case PT_CURRENCY
      7: (at: Double;);              // case PT_APPTIME
      8: (ft: TFileTime;);           // case PT_SYSTIME
      9: (lpszA: LPSTR;);            // case PT_STRING8
     10: (bin: TSBinary;);           // case PT_BINARY
     11: (lpszW: LPWSTR;);           // case PT_UNICODE
     12: (lpguid: PGUID;);           // case PT_CLSID
     13: (li: TLargeInteger;);       // case PT_I8
     14: (MVi: TSShortArray;);       // case PT_MV_I2
     15: (MVl: TSLongArray;);        // case PT_MV_LONG
     16: (MVflt: TSRealArray;);      // case PT_MV_R4
     17: (MVdbl: TSDoubleArray;);    // case PT_MV_DOUBLE
     18: (MVcur: TSCurrencyArray;);  // case PT_MV_CURRENCY
     19: (MVat: TSAppTimeArray;);    // case PT_MV_APPTIME
     20: (MVft: TSDateTimeArray;);   // case PT_MV_SYSTIME
     21: (MVbin: TSBinaryArray;);    // case PT_MV_BINARY
     22: (MVszA: TSLPSTRArray;);     // case PT_MV_STRING8
     23: (MVszW: TSWStringArray;);   // case PT_MV_UNICODE
     24: (MVguid: TSGuidArray;);     // case PT_MV_CLSID
     25: (MVli: TSLargeIntegerArray;);  // case PT_MV_I8
     26: (err: SCODE;);              // case PT_ERROR
     27: (x: LongInt;);              // case PT_NULL, PT_OBJECT (no usable value) *
  end;
  {$IfDef C3}{$EXTERNALSYM __UPV}{$EndIf C3}
  _PV = __UPV;
  {$IfDef C3}{$EXTERNALSYM _PV}{$EndIf C3}

  PSPropValue = ^TSPropValue;
  _SPropValue = record
    ulPropTag: ULONG;
    dwAlignPad: ULONG;
    Value: _PV;
  end;
  {$IfDef C3}{$EXTERNALSYM _SPropValue}{$EndIf C3}
  TSPropValue = _SPropValue;
  SPropValue = _SPropValue;
  {$IfDef C3}{$EXTERNALSYM SPropValue}{$EndIf C3}

{ Property Problem and Property Problem Arrays }

  PSPropProblem = ^TSPropProblem;
  _SPropProblem = record
    ulIndex: ULONG;
    ulPropTag: ULONG;
    _scode: SCODE;
  end;
  {$IfDef C3}{$EXTERNALSYM _SPropProblem}{$EndIf C3}
  TSPropProblem = _SPropProblem;
  SPropProblem = _SPropProblem;
  {$IfDef C3}{$EXTERNALSYM SPropProblem}{$EndIf C3}

  PSPropProblemArray = ^TSPropProblemArray;
  _SPropProblemArray = record
    cProblem: ULONG;
    aProblem: array[0..MAPI_DIM-1] of TSPropProblem;
  end;
  {$IfDef C3}{$EXTERNALSYM _SPropProblemArray}{$EndIf C3}
  TSPropProblemArray = _SPropProblemArray;
  SPropProblemArray = _SPropProblemArray;
  {$IfDef C3}{$EXTERNALSYM SPropProblemArray}{$EndIf C3}

(*!!!
#define CbNewSPropProblemArray(_cprob) \
    (offsetof(SPropProblemArray,aProblem) + (_cprob)*sizeof(SPropProblem))
#define CbSPropProblemArray(_lparray) \
    (offsetof(SPropProblemArray,aProblem) + \
    (UINT) ((_lparray)->cProblem*sizeof(SPropProblem)))
#define SizedSPropProblemArray(_cprob, _name) \
struct _SPropProblemArray_ ## _name \
{ \
    ULONG           cProblem; \
    SPropProblem    aProblem[_cprob]; \
} _name
*)

{ ENTRYLIST }

  PEntryList = ^TEntryList;
  ENTRYLIST = TSBinaryArray;
  {$IfDef C3}{$EXTERNALSYM ENTRYLIST}{$EndIf C3}
  TEntryList = ENTRYLIST;

{ FLATENTRYLIST }
{ MTSID }
{ FLATMTSIDLIST }

  PFlatEntry = ^TFlatEntry;
  FLATENTRY = record
    cb: ULONG;
    abEntry: array[0..MAPI_DIM-1] of Byte;
  end;
  {$IfDef C3}{$EXTERNALSYM FLATENTRY}{$EndIf C3}
  TFlatEntry = FLATENTRY;

  PFlatEntryList = ^TFlatEntryList;
  FLATENTRYLIST = record
    cEntries: ULONG;
    cbEntries: ULONG;
    abEntries: array[0..MAPI_DIM-1] of Byte;
  end;
  {$IfDef C3}{$EXTERNALSYM FLATENTRYLIST}{$EndIf C3}
  TFlatEntryList = FLATENTRYLIST;

  PMTSID = ^TMTSID;
  MTSID = record
    cb: ULONG;
    ab: array[0..MAPI_DIM-1] of Byte;
  end;
  {$IfDef C3}{$EXTERNALSYM TMTSID}{$EndIf C3}
  TMTSID = MTSID;

  PFlatMtsIdList = ^TFlatMtsIdList;
  FLATMTSIDLIST = record
    cMTSIDs: ULONG;
    cbMTSIDs: ULONG;
    abMTSIDs: array[0..MAPI_DIM-1] of Byte;
  end;
  {$IfDef C3}{$EXTERNALSYM FLATMTSIDLIST}{$EndIf C3}
  TFlatMtsIdList = FLATMTSIDLIST;

(*!!!
#define CbNewFLATENTRY(_cb)     (offsetof(FLATENTRY,abEntry) + (_cb))
#define CbFLATENTRY(_lpentry)   (offsetof(FLATENTRY,abEntry) + (_lpentry)->cb)
#define CbNewFLATENTRYLIST(_cb) (offsetof(FLATENTRYLIST,abEntries) + (_cb))
#define CbFLATENTRYLIST(_lplist) (offsetof(FLATENTRYLIST,abEntries) + (_lplist)->cbEntries)
#define CbNewMTSID(_cb)         (offsetof(MTSID,ab) + (_cb))
#define CbMTSID(_lpentry)       (offsetof(MTSID,ab) + (_lpentry)->cb)
#define CbNewFLATMTSIDLIST(_cb) (offsetof(FLATMTSIDLIST,abMTSIDs) + (_cb))
#define CbFLATMTSIDLIST(_lplist) (offsetof(FLATMTSIDLIST,abMTSIDs) + (_lplist)->cbMTSIDs)
/* No SizedXXX macros for these types.
*)

  PAdrEntry = ^TAdrEntry;
  _ADRENTRY = record
    ulReserved1: ULONG;    // Never used
    cValues: ULONG;
    rgPropVals: PSPropValue;
  end;
  {$IfDef C3}{$EXTERNALSYM _ADRENTRY}{$EndIf C3}
  TAdrEntry = _ADRENTRY;
  ADRENTRY = _ADRENTRY;
  {$IfDef C3}{$EXTERNALSYM ADRENTRY}{$EndIf C3}

  PAdrList = ^TAdrList;
  _ADRLIST = record
    cEntries: ULONG;
    aEntries: array[0..MAPI_DIM-1] of TAdrEntry;
  end;
  {$IfDef C3}{$EXTERNALSYM _ADRLIST}{$EndIf C3}
  TAdrList = _ADRLIST;
  ADRLIST = _ADRLIST;
  {$IfDef C3}{$EXTERNALSYM ADRLIST}{$EndIf C3}

(*!!!
#define CbNewADRLIST(_centries) \
    (offsetof(ADRLIST,aEntries) + (_centries)*sizeof(ADRENTRY))
#define CbADRLIST(_lpadrlist) \
    (offsetof(ADRLIST,aEntries) + (UINT)(_lpadrlist)->cEntries*sizeof(ADRENTRY))
#define SizedADRLIST(_centries, _name) \
struct _ADRLIST_ ## _name \
{ \
    ULONG           cEntries; \
    ADRENTRY        aEntries[_centries]; \
} _name
*)

  PSPropsArray = ^TSPropsArray;
  TSPropsArray = packed array[Byte] of TSPropValue;
//  {$IfDef C3}{$NODEFINE TSPropsArray}{$EndIf C3}

  PSRow = ^TSRow;
  _SRow = record
    ulAdrEntryPad: ULONG;  // Pad so SRow's can map to ADRENTRY's
    cValues: ULONG;        // Count of property values
    lpProps: PSPropsArray ; // Property value array
  end;
  {$IfDef C3}{$EXTERNALSYM _SRow}{$EndIf C3}
  TSRow = _SRow;
  SRow = _SRow;
  {$IfDef C3}{$EXTERNALSYM SRow}{$EndIf C3}

  PSRowSet = ^TSRowSet;
  _SRowSet = record
    cRows: ULONG;          // Count of rows
    aRow: array[0..MAPI_DIM-1] of TSRow; // Array of rows
  end;
  {$IfDef C3}{$EXTERNALSYM _SRowSet}{$EndIf C3}
  TSRowSet = _SRowSet;
  SRowSet = _SRowSet;
  {$IfDef C3}{$EXTERNALSYM SRowSet}{$EndIf C3}

(*!!!
#define CbNewSRowSet(_crow)     (offsetof(SRowSet,aRow) + (_crow)*sizeof(SRow))
#define CbSRowSet(_lprowset)    (offsetof(SRowSet,aRow) + \
                                    (UINT)((_lprowset)->cRows*sizeof(SRow)))
#define SizedSRowSet(_crow, _name) \
struct _SRowSet_ ## _name \
{ \
    ULONG           cRows; \
    SRow            aRow[_crow]; \
} _name
*)

{ MAPI Allocation Routines ------------------------------------------------- }

type
  PAllocateBuffer = ^TAllocateBuffer;
  ALLOCATEBUFFER = function (cbSize: ULONG; var lppBuffer: Pointer): SCODE; stdcall;
  {$IfDef C3}{$EXTERNALSYM ALLOCATEBUFFER}{$EndIf C3}
  TAllocateBuffer = ALLOCATEBUFFER;

  PAllocateMore = ^TAllocateMore;
  ALLOCATEMORE = function (cbSize: ULONG; lpObject: Pointer; var lppBuffer: Pointer): SCODE; stdcall;
  {$IfDef C3}{$EXTERNALSYM ALLOCATEMORE}{$EndIf C3}
  TAllocateMore = ALLOCATEMORE;

  PFreeBuffer = ^TFreeBuffer;
  FREEBUFFER = function (lpBuffer: Pointer): ULONG; stdcall;
  {$IfDef C3}{$EXTERNALSYM FREEBUFFER}{$EndIf C3}
  TFreeBuffer = FREEBUFFER;

{ Pointers to MAPI Interfaces ---------------------------------------------- }

  PCIID = ^TIID;

{ Extended MAPI Error Information ------------------------------------------ }

  PMapiError = ^TMapiError;
  _MAPIERROR = record
    ulVersion: ULONG;
    lpszError: LPTSTR;
    lpszComponent: LPTSTR;
    ulLowLevelError: ULONG;
    ulContext: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _MAPIERROR}{$EndIf C3}
  TMapiError = _MAPIERROR;
  MAPIERROR = _MAPIERROR;
  {$IfDef C3}{$EXTERNALSYM MAPIERROR}{$EndIf C3}

{ IMAPIAdviseSink Interface ------------------------------------------------ }

{*
 *  Notification event types. The event types can be combined in a bitmask
 *  for filtering. Each one has a parameter structure associated with it:
 *
 *      fnevCriticalError       ERROR_NOTIFICATION
 *      fnevNewMail             NEWMAIL_NOTIFICATION
 *      fnevObjectCreated       OBJECT_NOTIFICATION
 *      fnevObjectDeleted       OBJECT_NOTIFICATION
 *      fnevObjectModified      OBJECT_NOTIFICATION
 *      fnevObjectCopied        OBJECT_NOTIFICATION
 *      fnevSearchComplete      OBJECT_NOTIFICATION
 *      fnevTableModified       TABLE_NOTIFICATION
 *      fnevStatusObjectModified OBJECT_NOTIFICATION
 *
 *      fnevExtended            EXTENDED_NOTIFICATION
}

const
  fnevCriticalError           = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM fnevCriticalError}{$EndIf C3}
  fnevNewMail                 = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM fnevNewMail}{$EndIf C3}
  fnevObjectCreated           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM fnevObjectCreated}{$EndIf C3}
  fnevObjectDeleted           = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM fnevObjectDeleted}{$EndIf C3}
  fnevObjectModified          = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM fnevObjectModified}{$EndIf C3}
  fnevObjectMoved             = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM fnevObjectMoved}{$EndIf C3}
  fnevObjectCopied            = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM fnevObjectCopied}{$EndIf C3}
  fnevSearchComplete          = ULONG($00000080);
  {$IfDef C3}{$EXTERNALSYM fnevSearchComplete}{$EndIf C3}
  fnevTableModified           = ULONG($00000100);
  {$IfDef C3}{$EXTERNALSYM fnevTableModified}{$EndIf C3}
  fnevStatusObjectModified    = ULONG($00000200);
  {$IfDef C3}{$EXTERNALSYM fnevStatusObjectModified}{$EndIf C3}
  fnevReservedForMapi         = ULONG($40000000);
  {$IfDef C3}{$EXTERNALSYM fnevReservedForMapi}{$EndIf C3}
  fnevExtended                = ULONG($80000000);
  {$IfDef C3}{$EXTERNALSYM fnevExtended}{$EndIf C3}

{ TABLE_NOTIFICATION event types passed in ulTableEvent }

  TABLE_CHANGED       = 1;
  {$IfDef C3}{$EXTERNALSYM TABLE_CHANGED}{$EndIf C3}
  TABLE_ERROR         = 2;
  {$IfDef C3}{$EXTERNALSYM TABLE_ERROR}{$EndIf C3}
  TABLE_ROW_ADDED     = 3;
  {$IfDef C3}{$EXTERNALSYM TABLE_ROW_ADDED}{$EndIf C3}
  TABLE_ROW_DELETED   = 4;
  {$IfDef C3}{$EXTERNALSYM TABLE_ROW_DELETED}{$EndIf C3}
  TABLE_ROW_MODIFIED  = 5;
  {$IfDef C3}{$EXTERNALSYM TABLE_ROW_MODIFIED}{$EndIf C3}
  TABLE_SORT_DONE     = 6;
  {$IfDef C3}{$EXTERNALSYM TABLE_SORT_DONE}{$EndIf C3}
  TABLE_RESTRICT_DONE = 7;
  {$IfDef C3}{$EXTERNALSYM TABLE_RESTRICT_DONE}{$EndIf C3}
  TABLE_SETCOL_DONE   = 8;
  {$IfDef C3}{$EXTERNALSYM TABLE_SETCOL_DONE}{$EndIf C3}
  TABLE_RELOAD        = 9;
  {$IfDef C3}{$EXTERNALSYM TABLE_RELOAD}{$EndIf C3}

{ Event Structures }

type
  PErrorNotification = ^TErrorNotification;
  _ERROR_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;
    scode: SCODE;
    ulFlags: ULONG;                   // 0 or MAPI_UNICODE
    lpMAPIError: PMapiError;          // Detailed error information
  end;
  {$IfDef C3}{$EXTERNALSYM _ERROR_NOTIFICATION}{$EndIf C3}
  TErrorNotification = _ERROR_NOTIFICATION;
  ERROR_NOTIFICATION = _ERROR_NOTIFICATION;
  {$IfDef C3}{$EXTERNALSYM ERROR_NOTIFICATION}{$EndIf C3}

  PNewMailNotification = ^TNewMailNotification;
  _NEWMAIL_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;             // identifies the new message
    cbParentID: ULONG;
    lpParentID: PEntryID;            // identifies the folder it lives in
    ulFlags: ULONG;                  // 0 or MAPI_UNICODE
    lpszMessageClass: LPTSTR;        // message class (UNICODE or string8)
    ulMessageFlags: ULONG;           // copy of PR_MESSAGE_FLAGS
  end;
  {$IfDef C3}{$EXTERNALSYM _NEWMAIL_NOTIFICATION}{$EndIf C3}
  TNewMailNotification = _NEWMAIL_NOTIFICATION;
  NEWMAIL_NOTIFICATION = _NEWMAIL_NOTIFICATION;
  {$IfDef C3}{$EXTERNALSYM NEWMAIL_NOTIFICATION}{$EndIf C3}

  PObjectNotification = ^TObjectNotification;
  _OBJECT_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;                 // EntryID of object
    ulObjType: ULONG;                    // Type of object
    cbParentID: ULONG;
    lpParentID: PEntryID;                // EntryID of parent object
    cbOldID: ULONG;
    lpOldID: PEntryID;                   // EntryID of old object
    cbOldParentID: ULONG;
    lpOldParentID: PEntryID;             // EntryID of old parent
    lpPropTagArray: PSPropTagArray;
  end;
  {$IfDef C3}{$EXTERNALSYM _OBJECT_NOTIFICATION}{$EndIf C3}
  TObjectNotification = _OBJECT_NOTIFICATION;
  OBJECT_NOTIFICATION = _OBJECT_NOTIFICATION;
  {$IfDef C3}{$EXTERNALSYM OBJECT_NOTIFICATION}{$EndIf C3}

  PTableNotification = ^TTableNotification;
  _TABLE_NOTIFICATION = record
    ulTableEvent: ULONG;                 // Identifies WHICH table event
    hResult: HRESULT;                    // Value for TABLE_ERROR
    propIndex: TSPropValue;              // This row's "index property"
    propPrior: TSPropValue;              // Preceding row's "index property"
    row: TSRow;                          // New data of added/modified row
    ulPad: ULONG;                        // Force to 8-byte boundary
  end;
  {$IfDef C3}{$EXTERNALSYM _TABLE_NOTIFICATION}{$EndIf C3}
  TTableNotification = _TABLE_NOTIFICATION;
  TABLE_NOTIFICATION = _TABLE_NOTIFICATION;
  {$IfDef C3}{$EXTERNALSYM TABLE_NOTIFICATION}{$EndIf C3}

  PExtendedNotification = ^TExtendedNotification;
  _EXTENDED_NOTIFICATION = record
    ulEvent: ULONG;                       // extended event code
    cb: ULONG;                            // size of event parameters
    pbEventParameters: Pointer;           // event parameters
  end;
  {$IfDef C3}{$EXTERNALSYM _EXTENDED_NOTIFICATION}{$EndIf C3}
  TExtendedNotification = _EXTENDED_NOTIFICATION;
  EXTENDED_NOTIFICATION = _EXTENDED_NOTIFICATION;
  {$IfDef C3}{$EXTERNALSYM EXTENDED_NOTIFICATION}{$EndIf C3}

  PStatusObjectNotification = ^TStatusObjectNotification;
  STATUS_OBJECT_NOTIFICATION = record
    cbEntryID: ULONG;
    lpEntryID: PEntryID;
    cValues: ULONG;
    lpPropVals: PSPropValue;
  end;
  TStatusObjectNotification = STATUS_OBJECT_NOTIFICATION;
  {$IfDef C3}{$EXTERNALSYM STATUS_OBJECT_NOTIFICATION}{$EndIf C3}

  PNotification = ^TNotification;
  _NOTIFICATION = record
    ulEventType: ULONG;           // notification type, i.e. fnevSomething
    ulAlignPad: ULONG;            // Force to 8-byte boundary
    case Integer of
      1: (err: TErrorNotification;);
      2: (newmail: TNewMailNotification;);
      3: (obj: TObjectNotification;);
      4: (tab: TTableNotification;);
      5: (ext: TExtendedNotification;);
      6: (statobj: TStatusObjectNotification;);
  end;
  {$IfDef C3}{$EXTERNALSYM _NOTIFICATION}{$EndIf C3}
  TNotification = _NOTIFICATION;
  NOTIFICATION = _NOTIFICATION;
  {$IfDef C3}{$EXTERNALSYM NOTIFICATION}{$EndIf C3}

{ Interface used for registering and issuing notification callbacks. }

  IMAPIAdviseSink = interface(IUnknown)
    function OnNotify(cNotif: ULONG; lpNotifications: PNotification): ULONG; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMAPIAdviseSink}{$EndIf C3}

{ Callback function type for MAPIAllocAdviseSink }

  PNotifyCallback = ^TNotifyCallback;
  NOTIFCALLBACK = function (lpvContext: Pointer; cNotification: ULONG;
    lpNotifications: PNotification): Integer; stdcall;
  {$IfDef C3}{$EXTERNALSYM NOTIFCALLBACK}{$EndIf C3}
  TNotifyCallback = NOTIFCALLBACK;

{ IMAPIProgress Interface -------------------------------------------------- }

{ Flag values for the progress indicator }

const
  MAPI_TOP_LEVEL      = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_TOP_LEVEL}{$EndIf C3}

type
  IMAPIProgress = interface(IUnknown)
    function Progress(ulValue, ulCount, ulTotal: ULONG): HResult; stdcall;
    function GetFlags(var lpulFlags: ULONG): HResult; stdcall;
    function GetMax(var lpulMax: ULONG): HResult; stdcall;
    function GetMin(var lpulMin: ULONG): HResult; stdcall;
    function SetLimits(lpulMin, lpulMax, lpulFlags: PULONG): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMAPIProgress}{$EndIf C3}

{ IMAPIProp Interface ------------------------------------------------------ }

const
  MAPI_ERROR_VERSION      = $00000000;
  {$IfDef C3}{$EXTERNALSYM MAPI_ERROR_VERSION}{$EndIf C3}

{ SaveChanges }

  KEEP_OPEN_READONLY      = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM KEEP_OPEN_READONLY}{$EndIf C3}
  KEEP_OPEN_READWRITE     = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM KEEP_OPEN_READWRITE}{$EndIf C3}
  FORCE_SAVE              = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM FORCE_SAVE}{$EndIf C3}
// define MAPI_DEFERRED_ERRORS  ((ULONG) 0x00000008) below

{ OpenProperty  - ulFlags }
//***** MAPI_MODIFY             ((ULONG) 0x00000001) above
  MAPI_CREATE             = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MAPI_CREATE}{$EndIf C3}
  STREAM_APPEND           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM STREAM_APPEND}{$EndIf C3}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below

{ OpenProperty  - ulInterfaceOptions, IID_IMAPITable }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ CopyTo, CopyProps }

  MAPI_MOVE               = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_MOVE}{$EndIf C3}
  MAPI_NOREPLACE          = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MAPI_NOREPLACE}{$EndIf C3}
  MAPI_DECLINE_OK         = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM MAPI_DECLINE_OK}{$EndIf C3}

  MAPI_DIALOG             = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM MAPI_DIALOG}{$EndIf C3}

  MAPI_USE_DEFAULT        = $00000040;  // Use default profile in logon
  {$IfDef C3}{$EXTERNALSYM MAPI_USE_DEFAULT}{$EndIf C3}

{ Flags used in GetIDsFromNames }
//***** MAPI_CREATE             ((ULONG) 0x00000002) above

{ Flags used in GetNamesFromIDs  (bit fields) }
  MAPI_NO_STRINGS         = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_NO_STRINGS}{$EndIf C3}
  MAPI_NO_IDS             = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MAPI_NO_IDS}{$EndIf C3}

{ Union discriminator }
  MNID_ID                 = 0;
  {$IfDef C3}{$EXTERNALSYM MNID_ID}{$EndIf C3}
  MNID_STRING             = 1;
  {$IfDef C3}{$EXTERNALSYM MNID_STRING}{$EndIf C3}

type
  PMapiNameID = ^TMapiNameID;
  _MAPINAMEID = record
    lpguid: PGUID;
    ulKind: ULONG;
    case Integer of
      MNID_ID: (lID: LongInt;);
      MNID_STRING: (lpwstrName: LPWSTR;);
  end;
  {$IfDef C3}{$EXTERNALSYM _MAPINAMEID}{$EndIf C3}
  TMapiNameID = _MAPINAMEID;
  MAPINAMEID = _MAPINAMEID;
  {$IfDef C3}{$EXTERNALSYM MAPINAMEID}{$EndIf C3}

  IMAPIProp = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function SaveChanges(ulFlags: ULONG): HResult; stdcall;
    function GetProps(lpPropTagArray: PSPropTagArray; ulFlags: ULONG;
      lpcValues: PULONG; var lppPropArray: PSPropsArray): HResult; stdcall;
    function GetPropList(ulFlags: ULONG; var lppPropTagArray: PSPropTagArray): HResult; stdcall;
    function OpenProperty(ulPropTag: ULONG; const lpiid: TIID;
      ulInterfaceOptions, ulFlags: ULONG; out lppUnk: IUnknown): HResult; stdcall;
    function SetProps(cValues: ULONG; lpPropArray: PSPropsArray;
      lppProblems: PSPropProblemArray): HResult; stdcall;
    function DeleteProps(lpPropTagArray: PSPropTagArray;
      lppProblems: PSPropProblemArray): HResult; stdcall;
    function CopyTo(ciidExclude: ULONG; rgiidExclude: PCIID;
      lpExcludeProps: PSPropTagArray; ulUIParam: ULONG; lpProgress: IMAPIProgress;
      lpInterface: PIID; lpDestObj: Pointer; ulFlags: ULONG;
      lppProblems: PSPropProblemArray): HResult; stdcall;
    function CopyProps(lpIncludeProps: PSPropTagArray; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; lpInterface: PIID; lpDestObj: Pointer;
      ulFlags: ULONG; lppProblems: PSPropProblemArray): HResult; stdcall;
    function GetNamesFromIDs(lppPropTags: PSPropTagArray; lpPropSetGuid: PGUID;
      ulFlags: ULONG; var lpcPropNames: ULONG; var lpppPropNames: TMapiNameID): HResult; stdcall;
    function GetIDsFromNames(cPropNames: ULONG; lppPropNames: PMapiNameID;
      ulFlags: ULONG; var lppPropTags: PSPropTagArray): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMAPIProp}{$EndIf C3}

{ IMAPITable Interface ----------------------------------------------------- }

{ Table status }

const
  TBLSTAT_COMPLETE            = ULONG(0);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_COMPLETE}{$EndIf C3}
  TBLSTAT_QCHANGED            = ULONG(7);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_QCHANGED}{$EndIf C3}
  TBLSTAT_SORTING             = ULONG(9);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_SORTING}{$EndIf C3}
  TBLSTAT_SORT_ERROR          = ULONG(10);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_SORT_ERROR}{$EndIf C3}
  TBLSTAT_SETTING_COLS        = ULONG(11);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_SETTING_COLS}{$EndIf C3}
  TBLSTAT_SETCOL_ERROR        = ULONG(13);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_SETCOL_ERROR}{$EndIf C3}
  TBLSTAT_RESTRICTING         = ULONG(14);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_RESTRICTING}{$EndIf C3}
  TBLSTAT_RESTRICT_ERROR      = ULONG(15);
  {$IfDef C3}{$EXTERNALSYM TBLSTAT_RESTRICT_ERROR}{$EndIf C3}

{ Table Type }

  TBLTYPE_SNAPSHOT            = ULONG(0);
  {$IfDef C3}{$EXTERNALSYM TBLTYPE_SNAPSHOT}{$EndIf C3}
  TBLTYPE_KEYSET              = ULONG(1);
  {$IfDef C3}{$EXTERNALSYM TBLTYPE_KEYSET}{$EndIf C3}
  TBLTYPE_DYNAMIC             = ULONG(2);
  {$IfDef C3}{$EXTERNALSYM TBLTYPE_DYNAMIC}{$EndIf C3}

{ Sort order }

{ bit 0: set if descending, clear if ascending }

  TABLE_SORT_ASCEND       = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM TABLE_SORT_ASCEND}{$EndIf C3}
  TABLE_SORT_DESCEND      = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM TABLE_SORT_DESCEND}{$EndIf C3}
  TABLE_SORT_COMBINE      = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM TABLE_SORT_COMBINE}{$EndIf C3}

{ Data structures }

type
  PSSortOrder = ^TSSortOrder;
  _SSortOrder = record
    ulPropTag: ULONG;             // Column to sort on
    ulOrder: ULONG;               // Ascending, descending, combine to left
  end;
  {$IfDef C3}{$EXTERNALSYM _SSortOrder}{$EndIf C3}
  TSSortOrder = _SSortOrder;
  SSortOrder = _SSortOrder;
  {$IfDef C3}{$EXTERNALSYM SSortOrder}{$EndIf C3}

  PSSortOrderSet = ^TSSortOrderSet;
  _SSortOrderSet = record
    cSorts: ULONG;                // Number of sort columns in aSort below
    cCategories: ULONG;           // 0 for non-categorized, up to cSorts
    cExpanded: ULONG;             // 0 if no categories start expanded,
                                  //      up to cExpanded
    aSort: array[0..MAPI_DIM-1] of TSSortOrder;  // The sort orders
  end;
  {$IfDef C3}{$EXTERNALSYM _SSortOrderSet}{$EndIf C3}
  TSSortOrderSet = _SSortOrderSet;
  SSortOrderSet = _SSortOrderSet;
  {$IfDef C3}{$EXTERNALSYM SSortOrderSet}{$EndIf C3}

(*!!!
#define CbNewSSortOrderSet(_csort) \
    (offsetof(SSortOrderSet,aSort) + (_csort)*sizeof(SSortOrder))
#define CbSSortOrderSet(_lpset) \
    (offsetof(SSortOrderSet,aSort) + \
    (UINT)((_lpset)->cSorts*sizeof(SSortOrder)))
#define SizedSSortOrderSet(_csort, _name) \
struct _SSortOrderSet_ ## _name \
{ \
    ULONG           cSorts;         \
    ULONG           cCategories;    \
    ULONG           cExpanded;      \
    SSortOrder      aSort[_csort];  \
} _name
*)

  PBookMark = ^TBookMark;
  BOOKMARK = ULONG;
  {$IfDef C3}{$EXTERNALSYM BOOKMARK}{$EndIf C3}
  TBookMark = BOOKMARK;

const
  BOOKMARK_BEGINNING  = TBookMark(0);      // Before first row
  {$IfDef C3}{$EXTERNALSYM BOOKMARK_BEGINNING}{$EndIf C3}
  BOOKMARK_CURRENT    = TBookMark(1);      // Before current row
  {$IfDef C3}{$EXTERNALSYM BOOKMARK_CURRENT}{$EndIf C3}
  BOOKMARK_END        = TBookMark(2);      // After last row
  {$IfDef C3}{$EXTERNALSYM BOOKMARK_END}{$EndIf C3}

{ Fuzzy Level }

  FL_FULLSTRING        = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM FL_FULLSTRING}{$EndIf C3}
  FL_SUBSTRING         = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM FL_SUBSTRING}{$EndIf C3}
  FL_PREFIX            = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM FL_PREFIX}{$EndIf C3}

  FL_IGNORECASE        = ULONG($00010000);
  {$IfDef C3}{$EXTERNALSYM FL_IGNORECASE}{$EndIf C3}
  FL_IGNORENONSPACE    = ULONG($00020000);
  {$IfDef C3}{$EXTERNALSYM FL_IGNORENONSPACE}{$EndIf C3}
  FL_LOOSE             = ULONG($00040000);
  {$IfDef C3}{$EXTERNALSYM FL_LOOSE}{$EndIf C3}

{ Restrictions }

{ Restriction types }

const
  RES_AND              = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM RES_AND}{$EndIf C3}
  RES_OR               = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM RES_OR}{$EndIf C3}
  RES_NOT              = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM RES_NOT}{$EndIf C3}
  RES_CONTENT          = ULONG($00000003);
  {$IfDef C3}{$EXTERNALSYM RES_CONTENT}{$EndIf C3}
  RES_PROPERTY         = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM RES_PROPERTY}{$EndIf C3}
  RES_COMPAREPROPS     = ULONG($00000005);
  {$IfDef C3}{$EXTERNALSYM RES_COMPAREPROPS}{$EndIf C3}
  RES_BITMASK          = ULONG($00000006);
  {$IfDef C3}{$EXTERNALSYM RES_BITMASK}{$EndIf C3}
  RES_SIZE             = ULONG($00000007);
  {$IfDef C3}{$EXTERNALSYM RES_SIZE}{$EndIf C3}
  RES_EXIST            = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM RES_EXIST}{$EndIf C3}
  RES_SUBRESTRICTION   = ULONG($00000009);
  {$IfDef C3}{$EXTERNALSYM RES_SUBRESTRICTION}{$EndIf C3}
  RES_COMMENT          = ULONG($0000000A);
  {$IfDef C3}{$EXTERNALSYM RES_COMMENT}{$EndIf C3}

{ Relational operators. These apply to all property comparison restrictions. }

  RELOP_LT         = ULONG(0);     // <
  {$IfDef C3}{$EXTERNALSYM RELOP_LT}{$EndIf C3}
  RELOP_LE         = ULONG(1);     // <=
  {$IfDef C3}{$EXTERNALSYM RELOP_LE}{$EndIf C3}
  RELOP_GT         = ULONG(2);     // >
  {$IfDef C3}{$EXTERNALSYM RELOP_GT}{$EndIf C3}
  RELOP_GE         = ULONG(3);     // >=
  {$IfDef C3}{$EXTERNALSYM RELOP_GE}{$EndIf C3}
  RELOP_EQ         = ULONG(4);     // ==
  {$IfDef C3}{$EXTERNALSYM RELOP_EQ}{$EndIf C3}
  RELOP_NE         = ULONG(5);     // !=
  {$IfDef C3}{$EXTERNALSYM RELOP_NE}{$EndIf C3}
  RELOP_RE         = ULONG(6);     // LIKE (Regular expression)
  {$IfDef C3}{$EXTERNALSYM RELOP_RE}{$EndIf C3}

{ Bitmask operators, for RES_BITMASK only. }

  BMR_EQZ          = ULONG(0);     // ==0
  {$IfDef C3}{$EXTERNALSYM BMR_EQZ}{$EndIf C3}
  BMR_NEZ          = ULONG(1);     // !=0
  {$IfDef C3}{$EXTERNALSYM BMR_NEZ}{$EndIf C3}

{ Subobject identifiers for RES_SUBRESTRICTION only. See MAPITAGS.H. }

// #define PR_MESSAGE_RECIPIENTS  PROP_TAG(PT_OBJECT,0x0E12)
// #define PR_MESSAGE_ATTACHMENTS PROP_TAG(PT_OBJECT,0x0E13)

type

  PSRestriction = ^TSRestriction;

  PSAndRestriction = ^TSAndRestriction;
  _SAndRestriction = record
    cRes: ULONG;
    lpRes: PSRestriction;
  end;
  {$IfDef C3}{$EXTERNALSYM _SAndRestriction}{$EndIf C3}
  TSAndRestriction = _SAndRestriction;
  SAndRestriction = _SAndRestriction;
  {$IfDef C3}{$EXTERNALSYM SAndRestriction}{$EndIf C3}

  PSOrRestriction = ^TSOrRestriction;
  _SOrRestriction = record
    cRes: ULONG;
    lpRes: PSRestriction;
  end;
  {$IfDef C3}{$EXTERNALSYM _SOrRestriction}{$EndIf C3}
  TSOrRestriction = _SOrRestriction;
  SOrRestriction = _SOrRestriction;
  {$IfDef C3}{$EXTERNALSYM SOrRestriction}{$EndIf C3}

  PSNotRestriction = ^TSNotRestriction;
  _SNotRestriction = record
    ulReserved: ULONG;
    lpRes: PSRestriction;
  end;
  {$IfDef C3}{$EXTERNALSYM _SNotRestriction}{$EndIf C3}
  TSNotRestriction = _SNotRestriction;
  SNotRestriction = _SNotRestriction;
  {$IfDef C3}{$EXTERNALSYM SNotRestriction}{$EndIf C3}

  PSContentRestriction = ^TSContentRestriction;
  _SContentRestriction = record
    ulFuzzyLevel: ULONG;
    ulPropTag: ULONG;
    lpProp: PSPropValue;
  end;
  {$IfDef C3}{$EXTERNALSYM _SContentRestriction}{$EndIf C3}
  TSContentRestriction = _SContentRestriction;
  SContentRestriction = _SContentRestriction;
  {$IfDef C3}{$EXTERNALSYM SContentRestriction}{$EndIf C3}

  PSBitMaskRestriction = ^TSBitMaskRestriction;
  _SBitMaskRestriction = record
    relBMR: ULONG;
    ulPropTag: ULONG;
    ulMask: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _SBitMaskRestriction}{$EndIf C3}
  TSBitMaskRestriction = _SBitMaskRestriction;
  SBitMaskRestriction = _SBitMaskRestriction;
  {$IfDef C3}{$EXTERNALSYM SBitMaskRestriction}{$EndIf C3}

  PSPropertyRestriction = ^TSPropertyRestriction;
  _SPropertyRestriction = record
    relop: ULONG;
    ulPropTag: ULONG;
    lpProp: PSPropValue;
  end;
  {$IfDef C3}{$EXTERNALSYM _SPropertyRestriction}{$EndIf C3}
  TSPropertyRestriction = _SPropertyRestriction;
  SPropertyRestriction = _SPropertyRestriction;
  {$IfDef C3}{$EXTERNALSYM SPropertyRestriction}{$EndIf C3}

  PSComparePropsRestriction = ^TSComparePropsRestriction;
  _SComparePropsRestriction = record
    relop: ULONG;
    ulPropTag1: ULONG;
    ulPropTag2: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _SComparePropsRestriction}{$EndIf C3}
  TSComparePropsRestriction = _SComparePropsRestriction;
  SComparePropsRestriction = _SComparePropsRestriction;
  {$IfDef C3}{$EXTERNALSYM SComparePropsRestriction}{$EndIf C3}

  PSSizeRestriction = ^TSSizeRestriction;
  _SSizeRestriction = record
    relop: ULONG;
    ulPropTag: ULONG;
    cb: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _SSizeRestriction}{$EndIf C3}
  TSSizeRestriction = _SSizeRestriction;
  SSizeRestriction = _SSizeRestriction;
  {$IfDef C3}{$EXTERNALSYM SSizeRestriction}{$EndIf C3}

  PSExistRestriction = ^TSExistRestriction;
  _SExistRestriction = record
    ulReserved1: ULONG;
    ulPropTag: ULONG;
    ulReserved2: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _SExistRestriction}{$EndIf C3}
  TSExistRestriction = _SExistRestriction;
  SExistRestriction = _SExistRestriction;
  {$IfDef C3}{$EXTERNALSYM SExistRestriction}{$EndIf C3}

  PSSubRestriction = ^TSSubRestriction;
  _SSubRestriction = record
    ulSubObject: ULONG;
    lpRes: PSRestriction;
  end;
  {$IfDef C3}{$EXTERNALSYM _SSubRestriction}{$EndIf C3}
  TSSubRestriction = _SSubRestriction;
  SSubRestriction = _SSubRestriction;
  {$IfDef C3}{$EXTERNALSYM SSubRestriction}{$EndIf C3}

  PSCommentRestriction = ^TSCommentRestriction;
  _SCommentRestriction = record
    cValues: ULONG;     // # of properties in lpProp *:
    lpRes: PSRestriction;
    lpProp: PSPropValue;
  end;
  {$IfDef C3}{$EXTERNALSYM _SCommentRestriction}{$EndIf C3}
  TSCommentRestriction = _SCommentRestriction;
  SCommentRestriction = _SCommentRestriction;
  {$IfDef C3}{$EXTERNALSYM SCommentRestriction}{$EndIf C3}

  _SRestriction = record
    rt: ULONG;         // Restriction type
    case Integer of
      1: (resCompareProps: SComparePropsRestriction;);    // first
      2: (resAnd: TSAndRestriction;);
      3: (resOr: TSOrRestriction;);
      4: (resNot: TSNotRestriction;);
      5: (resContent: TSContentRestriction;);
      6: (resProperty: TSPropertyRestriction;);
      7: (resBitMask: TSBitMaskRestriction;);
      8: (resSize: TSSizeRestriction;);
      9: (resExist: TSExistRestriction;);
     10: (resSub: TSSubRestriction;);
     11: (resComment: TSCommentRestriction;);
  end;
  {$IfDef C3}{$EXTERNALSYM _SRestriction}{$EndIf C3}
  TSRestriction = _SRestriction;

{ SComparePropsRestriction is first in the union so that static initializations
  of 3-value restriction work }

{ Flags of the methods of IMAPITable }

{ QueryColumn }

const
  TBL_ALL_COLUMNS     = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM TBL_ALL_COLUMNS}{$EndIf C3}

{ QueryRows }
{ Possible values for PR_ROW_TYPE (for categorization) }

  TBL_LEAF_ROW            = ULONG(1);
  {$IfDef C3}{$EXTERNALSYM TBL_LEAF_ROW}{$EndIf C3}
  TBL_EMPTY_CATEGORY      = ULONG(2);
  {$IfDef C3}{$EXTERNALSYM TBL_EMPTY_CATEGORY}{$EndIf C3}
  TBL_EXPANDED_CATEGORY   = ULONG(3);
  {$IfDef C3}{$EXTERNALSYM TBL_EXPANDED_CATEGORY}{$EndIf C3}
  TBL_COLLAPSED_CATEGORY  = ULONG(4);
  {$IfDef C3}{$EXTERNALSYM TBL_COLLAPSED_CATEGORY}{$EndIf C3}

{ Table wait flag }

  TBL_NOWAIT              = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM TBL_NOWAIT}{$EndIf C3}
{ alternative name for TBL_NOWAIT }
  TBL_ASYNC               = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM TBL_ASYNC}{$EndIf C3}
  TBL_BATCH               = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM TBL_BATCH}{$EndIf C3}

{ FindRow }

  DIR_BACKWARD            = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM DIR_BACKWARD}{$EndIf C3}

{ Table cursor states }

  TBL_NOADVANCE           = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM TBL_NOADVANCE}{$EndIf C3}

type
  IMAPITable = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function Advise(ulEventMask: ULONG; lpAdviseSink: IMAPIAdviseSink;
      var lpulConnection: ULONG): HResult; stdcall;
    function Unadvise(ulConnection: ULONG): HResult; stdcall;
    function GetStatus(var lpulTableStatus, lpulTableType: ULONG): HResult; stdcall;
    function SetColumns(lpPropTagArray: PSPropTagArray; ulFlags: ULONG): HResult; stdcall;
    function QueryColumns(ulFlags: ULONG; var lpPropTagArray: PSPropTagArray): HResult; stdcall;
    function GetRowCount(ulFlags: ULONG; var lpulCount: ULONG): HResult; stdcall;
    function SeekRow(bkOrigin: TBookMark; lRowCount: LongInt; lplRowsSought: PLongInt): HResult; stdcall;
    function SeekRowApprox(ulNumerator, ulDenominator: ULONG): HResult; stdcall;
    function QueryPosition(var lpulRow, lpulNumerator, lpulDenominator: ULONG): HResult; stdcall;
    function FindRow(lpRestriction: PSRestriction; bkOrigin: TBookMark;
      ulFlags: ULONG): HResult; stdcall;
    function Restrict(lpRestriction: PSRestriction; ulFlags: ULONG): HResult; stdcall;
    function CreateBookmark(var lpbkPosition: TBookMark): HResult; stdcall;
    function FreeBookmark(bkPosition: TBookMark): HResult; stdcall;
    function SortTable(lpSortCriteria: PSSortOrderSet; ulFlags: ULONG): HResult; stdcall;
    function QuerySortOrder(var lppSortCriteria: PSSortOrderSet): HResult; stdcall;
    function QueryRows(lRowCount: LongInt; ulFlags: ULONG; var lppRows: PSRowSet): HResult; stdcall;
    function Abort: HResult; stdcall;
    function ExpandRow(cbInstanceKey: ULONG; pbInstanceKey: Pointer;
      ulRowCount, ulFlags: ULONG; lppRows: PSRowSet; var lpulMoreRows: ULONG): HResult; stdcall;
    function CollapseRow(cbInstanceKey: ULONG; pbInstanceKey: Pointer;
      ulFlags: ULONG; var lpulRowCount: ULONG): HResult; stdcall;
    function WaitForCompletion(ulFlags, ulTimeout: ULONG; lpulTableStatus: PULONG): HResult; stdcall;
    function GetCollapseState(ulFlags, cbInstanceKey: ULONG; lpbInstanceKey: Pointer;
      var lpcbCollapseState: ULONG; var lppbCollapseState: Pointer): HResult; stdcall;
    function SetCollapseState(ulFlags, cbCollapseState: ULONG; pbCollapseState: Pointer;
      var lpbkLocation: TBookMark): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMAPITable}{$EndIf C3}

{ IProfSect Interface ------------------------------------------------------ }

{ Standard section for public profile properties }

const
  PS_PROFILE_PROPERTIES_INIT: array[0..15] of Byte =
   ($98, $15, $AC, $08, $AA, $B0, $10, $1A,
    $8C, $93, $08, $00, $2B, $2A, $56, $C2);
  {$IfDef C3}{$EXTERNALSYM PS_PROFILE_PROPERTIES_INIT}{$EndIf C3}

type
  IProfSect = interface(IMAPIProp)
  end;
  {$IfDef C3}{$EXTERNALSYM IProfSect}{$EndIf C3}

{ IMAPIStatus Interface ---------------------------------------------------- }

{ Values for PR_RESOURCE_TYPE, _METHODS, _FLAGS }

const
  MAPI_STORE_PROVIDER     = ULONG(33);    // Message Store
  {$IfDef C3}{$EXTERNALSYM MAPI_STORE_PROVIDER}{$EndIf C3}
  MAPI_AB                 = ULONG(34);    // Address Book
  {$IfDef C3}{$EXTERNALSYM MAPI_AB}{$EndIf C3}
  MAPI_AB_PROVIDER        = ULONG(35);    // Address Book Provider
  {$IfDef C3}{$EXTERNALSYM MAPI_AB_PROVIDER}{$EndIf C3}
  MAPI_TRANSPORT_PROVIDER = ULONG(36);    // Transport Provider
  {$IfDef C3}{$EXTERNALSYM MAPI_TRANSPORT_PROVIDER}{$EndIf C3}
  MAPI_SPOOLER            = ULONG(37);    // Message Spooler
  {$IfDef C3}{$EXTERNALSYM MAPI_SPOOLER}{$EndIf C3}
  MAPI_PROFILE_PROVIDER   = ULONG(38);    // Profile Provider
  {$IfDef C3}{$EXTERNALSYM MAPI_PROFILE_PROVIDER}{$EndIf C3}
  MAPI_SUBSYSTEM          = ULONG(39);    // Overall Subsystem Status
  {$IfDef C3}{$EXTERNALSYM MAPI_SUBSYSTEM}{$EndIf C3}
  MAPI_HOOK_PROVIDER      = ULONG(40);    // Spooler Hook
  {$IfDef C3}{$EXTERNALSYM MAPI_HOOK_PROVIDER}{$EndIf C3}

  STATUS_VALIDATE_STATE   = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM STATUS_VALIDATE_STATE}{$EndIf C3}
  STATUS_SETTINGS_DIALOG  = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM STATUS_SETTINGS_DIALOG}{$EndIf C3}
  STATUS_CHANGE_PASSWORD  = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM STATUS_CHANGE_PASSWORD}{$EndIf C3}
  STATUS_FLUSH_QUEUES     = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM STATUS_FLUSH_QUEUES}{$EndIf C3}

  STATUS_DEFAULT_OUTBOUND = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM STATUS_DEFAULT_OUTBOUND}{$EndIf C3}
  STATUS_DEFAULT_STORE    = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM STATUS_DEFAULT_STORE}{$EndIf C3}
  STATUS_PRIMARY_IDENTITY = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM STATUS_PRIMARY_IDENTITY}{$EndIf C3}
  STATUS_SIMPLE_STORE     = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM STATUS_SIMPLE_STORE}{$EndIf C3}
  STATUS_XP_PREFER_LAST   = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM STATUS_XP_PREFER_LAST}{$EndIf C3}
  STATUS_NO_PRIMARY_IDENTITY = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM STATUS_NO_PRIMARY_IDENTITY}{$EndIf C3}
  STATUS_NO_DEFAULT_STORE = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM STATUS_NO_DEFAULT_STORE}{$EndIf C3}
  STATUS_TEMP_SECTION     = ULONG($00000080);
  {$IfDef C3}{$EXTERNALSYM STATUS_TEMP_SECTION}{$EndIf C3}
  STATUS_OWN_STORE        = ULONG($00000100);
  {$IfDef C3}{$EXTERNALSYM STATUS_OWN_STORE}{$EndIf C3}
//***** HOOK_INBOUND            ((ULONG) 0x00000200) Defined in MAPIHOOK.H
//***** HOOK_OUTBOUND           ((ULONG) 0x00000400) Defined in MAPIHOOK.H
  STATUS_NEED_IPM_TREE    = ULONG($00000800);
  {$IfDef C3}{$EXTERNALSYM STATUS_NEED_IPM_TREE}{$EndIf C3}
  STATUS_PRIMARY_STORE    = ULONG($00001000);
  {$IfDef C3}{$EXTERNALSYM STATUS_PRIMARY_STORE}{$EndIf C3}
  STATUS_SECONDARY_STORE  = ULONG($00002000);
  {$IfDef C3}{$EXTERNALSYM STATUS_SECONDARY_STORE}{$EndIf C3}


{* PR_STATUS_CODE bit. Low 16 bits for common values; High 16 bits
 * for provider type-specific values. (DCR 304)
}

  STATUS_AVAILABLE        = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM STATUS_AVAILABLE}{$EndIf C3}
  STATUS_OFFLINE          = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM STATUS_OFFLINE}{$EndIf C3}
  STATUS_FAILURE          = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM STATUS_FAILURE}{$EndIf C3}

{ Transport values of PR_STATUS_CODE }

  STATUS_INBOUND_ENABLED  = ULONG($00010000);
  {$IfDef C3}{$EXTERNALSYM STATUS_INBOUND_ENABLED}{$EndIf C3}
  STATUS_INBOUND_ACTIVE   = ULONG($00020000);
  {$IfDef C3}{$EXTERNALSYM STATUS_INBOUND_ACTIVE}{$EndIf C3}
  STATUS_INBOUND_FLUSH    = ULONG($00040000);
  {$IfDef C3}{$EXTERNALSYM STATUS_INBOUND_FLUSH}{$EndIf C3}
  STATUS_OUTBOUND_ENABLED = ULONG($00100000);
  {$IfDef C3}{$EXTERNALSYM STATUS_OUTBOUND_ENABLED}{$EndIf C3}
  STATUS_OUTBOUND_ACTIVE  = ULONG($00200000);
  {$IfDef C3}{$EXTERNALSYM STATUS_OUTBOUND_ACTIVE}{$EndIf C3}
  STATUS_OUTBOUND_FLUSH   = ULONG($00400000);
  {$IfDef C3}{$EXTERNALSYM STATUS_OUTBOUND_FLUSH}{$EndIf C3}
  STATUS_REMOTE_ACCESS    = ULONG($00800000);
  {$IfDef C3}{$EXTERNALSYM STATUS_REMOTE_ACCESS}{$EndIf C3}

{ ValidateState flags }

  SUPPRESS_UI                 = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM SUPPRESS_UI}{$EndIf C3}
  REFRESH_XP_HEADER_CACHE     = ULONG($00010000);
  {$IfDef C3}{$EXTERNALSYM REFRESH_XP_HEADER_CACHE}{$EndIf C3}
  PROCESS_XP_HEADER_CACHE     = ULONG($00020000);
  {$IfDef C3}{$EXTERNALSYM PROCESS_XP_HEADER_CACHE}{$EndIf C3}
  FORCE_XP_CONNECT            = ULONG($00040000);
  {$IfDef C3}{$EXTERNALSYM FORCE_XP_CONNECT}{$EndIf C3}
  FORCE_XP_DISCONNECT         = ULONG($00080000);
  {$IfDef C3}{$EXTERNALSYM FORCE_XP_DISCONNECT}{$EndIf C3}
  CONFIG_CHANGED              = ULONG($00100000);
  {$IfDef C3}{$EXTERNALSYM CONFIG_CHANGED}{$EndIf C3}
  ABORT_XP_HEADER_OPERATION   = ULONG($00200000);
  {$IfDef C3}{$EXTERNALSYM ABORT_XP_HEADER_OPERATION}{$EndIf C3}
  SHOW_XP_SESSION_UI          = ULONG($00400000);
  {$IfDef C3}{$EXTERNALSYM SHOW_XP_SESSION_UI}{$EndIf C3}

{ SettingsDialog flags }

  UI_READONLY     = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM UI_READONLY}{$EndIf C3}

{ FlushQueues flags }

  FLUSH_UPLOAD        = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM FLUSH_UPLOAD}{$EndIf C3}
  FLUSH_DOWNLOAD      = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM FLUSH_DOWNLOAD}{$EndIf C3}
  FLUSH_FORCE         = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM FLUSH_FORCE}{$EndIf C3}
  FLUSH_NO_UI         = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM FLUSH_NO_UI}{$EndIf C3}
  FLUSH_ASYNC_OK      = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM FLUSH_ASYNC_OK}{$EndIf C3}

type
  IMAPIStatus = interface(IMAPIProp)
    function ValidateState(ulUIParam, ulFlags: ULONG): HResult; stdcall;
    function SettingsDialog(ulUIParam, ulFlags: ULONG): HResult; stdcall;
    function ChangePassword(lpOldPass, lpNewPass: LPTSTR; ulFlags: ULONG): HResult; stdcall;
    function FlushQueues(ulUIParam, cbTargetTransport: ULONG;
     lpTargetTransport: PEntryID; ulFlags: ULONG): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMAPIStatus}{$EndIf C3}

{ IMAPIContainer Interface ------------------------------------------------ }

{ Flags for OpenEntry() }

const
//***** MAPI_MODIFY             ((ULONG) 0x00000001) above
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
  MAPI_BEST_ACCESS        = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM MAPI_BEST_ACCESS}{$EndIf C3}

{ GetContentsTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_ASSOCIATED         ((ULONG) 0x00000040) below
  WAB_LOCAL_CONTAINERS    = $00100000;
  {$IfDef C3}{$EXTERNALSYM WAB_LOCAL_CONTAINERS}{$EndIf C3}
  WAB_PROFILE_CONTENTS    = $00200000;
  {$IfDef C3}{$EXTERNALSYM WAB_PROFILE_CONTENTS}{$EndIf C3}

{ GetHierarchyTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
  CONVENIENT_DEPTH        = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM CONVENIENT_DEPTH}{$EndIf C3}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below

{ GetSearchCriteria }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
  SEARCH_RUNNING          = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM SEARCH_RUNNING}{$EndIf C3}
  SEARCH_REBUILD          = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM SEARCH_REBUILD}{$EndIf C3}
  SEARCH_RECURSIVE        = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM SEARCH_RECURSIVE}{$EndIf C3}
  SEARCH_FOREGROUND       = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM SEARCH_FOREGROUND}{$EndIf C3}

{ SetSearchCriteria }
  STOP_SEARCH             = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM STOP_SEARCH}{$EndIf C3}
  RESTART_SEARCH          = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM RESTART_SEARCH}{$EndIf C3}
  RECURSIVE_SEARCH        = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM RECURSIVE_SEARCH}{$EndIf C3}
  SHALLOW_SEARCH          = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM SHALLOW_SEARCH}{$EndIf C3}
  FOREGROUND_SEARCH       = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM FOREGROUND_SEARCH}{$EndIf C3}
  BACKGROUND_SEARCH       = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM BACKGROUND_SEARCH}{$EndIf C3}

type
  IMAPIContainer = interface(IMAPIProp)
    function GetContentsTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function GetHierarchyTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function OpenEntry(cbEntryID: ULONG; lpEntryID: PEntryID; lpInterface: PIID;
      ulFlags: ULONG; var lpulObjType: ULONG; out lppUnk: IUnknown): HResult; stdcall;
    function SetSearchCriteria(lpRestriction: PSRestriction;
      lpContainerList: PEntryList; ulSearchFlags: ULONG): HResult; stdcall;
    function GetSearchCriteria(ulFlags: ULONG; var lppRestriction: PSRestriction;
      var lppContainerList: PEntryList; var lpulSearchState: ULONG): HResult; stdcall;
   end;
  {$IfDef C3}{$EXTERNALSYM IMAPIContainer}{$EndIf C3}

{ IABContainer Interface --------------------------------------------------- }

{*  IABContainer PR_CONTAINER_FLAGS values
 *  If AB_UNMODIFIABLE and AB_MODIFIABLE are both set, it means the container
 *  doesn't know if it's modifiable or not, and the client should
 *  try to modify the contents but we won't expect it to work.
 *  If the AB_RECIPIENTS flag is set and neither AB_MODIFIABLE or AB_UNMODIFIABLE
 *  bits are set, it is an error.
}

  PFlagList = ^TFlagList;
  _flaglist = record
    cFlags: ULONG;
    ulFlag: array[0..MAPI_DIM-1] of ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _flaglist}{$EndIf C3}
  TFlagList = _flaglist;

{ Container flags }

const
  AB_RECIPIENTS           = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM AB_RECIPIENTS}{$EndIf C3}
  AB_SUBCONTAINERS        = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM AB_SUBCONTAINERS}{$EndIf C3}
  AB_MODIFIABLE           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM AB_MODIFIABLE}{$EndIf C3}
  AB_UNMODIFIABLE         = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM AB_UNMODIFIABLE}{$EndIf C3}
  AB_FIND_ON_OPEN         = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM AB_FIND_ON_OPEN}{$EndIf C3}
  AB_NOT_DEFAULT          = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM AB_NOT_DEFAULT}{$EndIf C3}

{ CreateEntry() }

  CREATE_CHECK_DUP_STRICT = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM CREATE_CHECK_DUP_STRICT}{$EndIf C3}
  CREATE_CHECK_DUP_LOOSE  = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM CREATE_CHECK_DUP_LOOSE}{$EndIf C3}
  CREATE_REPLACE          = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM CREATE_REPLACE}{$EndIf C3}
  CREATE_MERGE            = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM CREATE_MERGE}{$EndIf C3}

{ ResolveNames() - ulFlags }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above
  WAB_IGNORE_PROFILES     = $00800000;
  {$IfDef C3}{$EXTERNALSYM WAB_IGNORE_PROFILES}{$EndIf C3}

{ ResolveNames() - rgulFlags }
  MAPI_UNRESOLVED         = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM MAPI_UNRESOLVED}{$EndIf C3}
  MAPI_AMBIGUOUS          = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_AMBIGUOUS}{$EndIf C3}
  MAPI_RESOLVED           = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MAPI_RESOLVED}{$EndIf C3}

type
  IABContainer = interface(IMAPIContainer)
    function CreateEntry(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulCreateFlags: ULONG; out lppMAPIPropEntry: IMAPIProp): HResult; stdcall;
    function CopyEntries(lpEntries: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function DeleteEntries(lpEntries: PEntryList; ulFlags: ULONG): HResult; stdcall;
    function ResolveNames(lpPropTagArray: PSPropTagArray; ulFlags: ULONG;
      lpAdrList: PAdrList; lpFlagList: PFlagList): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IABContainer}{$EndIf C3}

{ IMailUser Interface ------------------------------------------------------ }

{*  Any call which can create a one-off entryID (i.e. MAPISupport::CreateOneOff
    or IAddrBook::CreateOneOff) can encode the value for PR_SEND_RICH_INFO by
    passing in the following flag in the ulFlags parameter.  Setting this flag
    indicates that PR_SEND_RICH_INFO will be FALSE.
}

const
  MAPI_SEND_NO_RICH_INFO      = ULONG($00010000);
  {$IfDef C3}{$EXTERNALSYM MAPI_SEND_NO_RICH_INFO}{$EndIf C3}

{ Values of PR_NDR_DIAG_CODE }

  MAPI_DIAG_NO_DIAGNOSTIC                     = -1;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_NO_DIAGNOSTIC}{$EndIf C3}
  MAPI_DIAG_OR_NAME_UNRECOGNIZED              = 0;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_OR_NAME_UNRECOGNIZED}{$EndIf C3}
  MAPI_DIAG_OR_NAME_AMBIGUOUS                 = 1;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_OR_NAME_AMBIGUOUS}{$EndIf C3}
  MAPI_DIAG_MTS_CONGESTED                     = 2;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MTS_CONGESTED}{$EndIf C3}
  MAPI_DIAG_LOOP_DETECTED                     = 3;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_LOOP_DETECTED}{$EndIf C3}
  MAPI_DIAG_RECIPIENT_UNAVAILABLE             = 4;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_RECIPIENT_UNAVAILABLE}{$EndIf C3}
  MAPI_DIAG_MAXIMUM_TIME_EXPIRED              = 5;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAXIMUM_TIME_EXPIRED}{$EndIf C3}
  MAPI_DIAG_EITS_UNSUPPORTED                  = 6;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_EITS_UNSUPPORTED}{$EndIf C3}
  MAPI_DIAG_CONTENT_TOO_LONG                  = 7;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_CONTENT_TOO_LONG}{$EndIf C3}
  MAPI_DIAG_IMPRACTICAL_TO_CONVERT            = 8;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_IMPRACTICAL_TO_CONVERT}{$EndIf C3}
  MAPI_DIAG_PROHIBITED_TO_CONVERT             = 9;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_PROHIBITED_TO_CONVERT}{$EndIf C3}
  MAPI_DIAG_CONVERSION_UNSUBSCRIBED           = 10;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_CONVERSION_UNSUBSCRIBED}{$EndIf C3}
  MAPI_DIAG_PARAMETERS_INVALID                = 11;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_PARAMETERS_INVALID}{$EndIf C3}
  MAPI_DIAG_CONTENT_SYNTAX_IN_ERROR           = 12;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_CONTENT_SYNTAX_IN_ERROR}{$EndIf C3}
  MAPI_DIAG_LENGTH_CONSTRAINT_VIOLATD         = 13;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_LENGTH_CONSTRAINT_VIOLATD}{$EndIf C3}
  MAPI_DIAG_NUMBER_CONSTRAINT_VIOLATD         = 14;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_NUMBER_CONSTRAINT_VIOLATD}{$EndIf C3}
  MAPI_DIAG_CONTENT_TYPE_UNSUPPORTED          = 15;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_CONTENT_TYPE_UNSUPPORTED}{$EndIf C3}
  MAPI_DIAG_TOO_MANY_RECIPIENTS               = 16;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_TOO_MANY_RECIPIENTS}{$EndIf C3}
  MAPI_DIAG_NO_BILATERAL_AGREEMENT            = 17;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_NO_BILATERAL_AGREEMENT}{$EndIf C3}
  MAPI_DIAG_CRITICAL_FUNC_UNSUPPORTED         = 18;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_CRITICAL_FUNC_UNSUPPORTED}{$EndIf C3}
  MAPI_DIAG_CONVERSION_LOSS_PROHIB            = 19;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_CONVERSION_LOSS_PROHIB}{$EndIf C3}
  MAPI_DIAG_LINE_TOO_LONG                     = 20;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_LINE_TOO_LONG}{$EndIf C3}
  MAPI_DIAG_PAGE_TOO_LONG                     = 21;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_PAGE_TOO_LONG}{$EndIf C3}
  MAPI_DIAG_PICTORIAL_SYMBOL_LOST             = 22;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_PICTORIAL_SYMBOL_LOST}{$EndIf C3}
  MAPI_DIAG_PUNCTUATION_SYMBOL_LOST           = 23;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_PUNCTUATION_SYMBOL_LOST}{$EndIf C3}
  MAPI_DIAG_ALPHABETIC_CHARACTER_LOST         = 24;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_ALPHABETIC_CHARACTER_LOST}{$EndIf C3}
  MAPI_DIAG_MULTIPLE_INFO_LOSSES              = 25;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MULTIPLE_INFO_LOSSES}{$EndIf C3}
  MAPI_DIAG_REASSIGNMENT_PROHIBITED           = 26;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_REASSIGNMENT_PROHIBITED}{$EndIf C3}
  MAPI_DIAG_REDIRECTION_LOOP_DETECTED         = 27;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_REDIRECTION_LOOP_DETECTED}{$EndIf C3}
  MAPI_DIAG_EXPANSION_PROHIBITED              = 28;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_EXPANSION_PROHIBITED}{$EndIf C3}
  MAPI_DIAG_SUBMISSION_PROHIBITED             = 29;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_SUBMISSION_PROHIBITED}{$EndIf C3}
  MAPI_DIAG_EXPANSION_FAILED                  = 30;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_EXPANSION_FAILED}{$EndIf C3}
  MAPI_DIAG_RENDITION_UNSUPPORTED             = 31;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_RENDITION_UNSUPPORTED}{$EndIf C3}
  MAPI_DIAG_MAIL_ADDRESS_INCORRECT            = 32;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_ADDRESS_INCORRECT}{$EndIf C3}
  MAPI_DIAG_MAIL_OFFICE_INCOR_OR_INVD         = 33;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_OFFICE_INCOR_OR_INVD}{$EndIf C3}
  MAPI_DIAG_MAIL_ADDRESS_INCOMPLETE           = 34;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_ADDRESS_INCOMPLETE}{$EndIf C3}
  MAPI_DIAG_MAIL_RECIPIENT_UNKNOWN            = 35;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_UNKNOWN}{$EndIf C3}
  MAPI_DIAG_MAIL_RECIPIENT_DECEASED           = 36;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_DECEASED}{$EndIf C3}
  MAPI_DIAG_MAIL_ORGANIZATION_EXPIRED         = 37;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_ORGANIZATION_EXPIRED}{$EndIf C3}
  MAPI_DIAG_MAIL_REFUSED                      = 38;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_REFUSED}{$EndIf C3}
  MAPI_DIAG_MAIL_UNCLAIMED                    = 39;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_UNCLAIMED}{$EndIf C3}
  MAPI_DIAG_MAIL_RECIPIENT_MOVED              = 40;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_MOVED}{$EndIf C3}
  MAPI_DIAG_MAIL_RECIPIENT_TRAVELLING         = 41;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_TRAVELLING}{$EndIf C3}
  MAPI_DIAG_MAIL_RECIPIENT_DEPARTED           = 42;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_RECIPIENT_DEPARTED}{$EndIf C3}
  MAPI_DIAG_MAIL_NEW_ADDRESS_UNKNOWN          = 43;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_NEW_ADDRESS_UNKNOWN}{$EndIf C3}
  MAPI_DIAG_MAIL_FORWARDING_UNWANTED          = 44;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_FORWARDING_UNWANTED}{$EndIf C3}
  MAPI_DIAG_MAIL_FORWARDING_PROHIB            = 45;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_MAIL_FORWARDING_PROHIB}{$EndIf C3}
  MAPI_DIAG_SECURE_MESSAGING_ERROR            = 46;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_SECURE_MESSAGING_ERROR}{$EndIf C3}
  MAPI_DIAG_DOWNGRADING_IMPOSSIBLE            = 47;
  {$IfDef C3}{$EXTERNALSYM MAPI_DIAG_DOWNGRADING_IMPOSSIBLE}{$EndIf C3}

type
  IMailUser = interface(IMAPIProp)
  end;
  {$IfDef C3}{$EXTERNALSYM IMailUser}{$EndIf C3}

{ IDistList Interface ------------------------------------------------------ }

  IDistList = interface(IMAPIContainer)
    function CreateEntry(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulCreateFlags: ULONG; out lppMAPIPropEntry: IMAPIProp): HResult; stdcall;
    function CopyEntries(lpEntries: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function DeleteEntries(lpEntries: PEntryList; ulFlags: ULONG): HResult; stdcall;
    function ResolveNames(lpPropTagArray: PSPropTagArray; ulFlags: ULONG;
      lpAdrList: PAdrList; lpFlagList: PFlagList): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IDistList}{$EndIf C3}

{ IMAPIFolder Interface ---------------------------------------------------- }

{ IMAPIFolder folder type (enum) }

const
  FOLDER_ROOT               = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM FOLDER_ROOT}{$EndIf C3}
  FOLDER_GENERIC            = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM FOLDER_GENERIC}{$EndIf C3}
  FOLDER_SEARCH             = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM FOLDER_SEARCH}{$EndIf C3}

{ CreateMessage }
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_ASSOCIATED         ((ULONG) 0x00000040) below

{ CopyMessages }

  MESSAGE_MOVE              = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MESSAGE_MOVE}{$EndIf C3}
  MESSAGE_DIALOG            = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MESSAGE_DIALOG}{$EndIf C3}
//***** MAPI_DECLINE_OK         ((ULONG) 0x00000004) above

{ CreateFolder }

  OPEN_IF_EXISTS            = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM OPEN_IF_EXISTS}{$EndIf C3}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ DeleteFolder }

  DEL_MESSAGES              = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM DEL_MESSAGES}{$EndIf C3}
  FOLDER_DIALOG             = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM FOLDER_DIALOG}{$EndIf C3}
  DEL_FOLDERS               = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM DEL_FOLDERS}{$EndIf C3}

{ EmptyFolder }

  DEL_ASSOCIATED            = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM DEL_ASSOCIATED}{$EndIf C3}

{ CopyFolder }

  FOLDER_MOVE               = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM FOLDER_MOVE}{$EndIf C3}
//***** FOLDER_DIALOG           ((ULONG) 0x00000002) above
//***** MAPI_DECLINE_OK         ((ULONG) 0x00000004) above
  COPY_SUBFOLDERS           = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM COPY_SUBFOLDERS}{$EndIf C3}
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above


{ SetReadFlags }

//***** SUPPRESS_RECEIPT        ((ULONG) 0x00000001) below
//***** MESSAGE_DIALOG          ((ULONG) 0x00000002) above
//***** CLEAR_READ_FLAG         ((ULONG) 0x00000004) below
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
  GENERATE_RECEIPT_ONLY     = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM GENERATE_RECEIPT_ONLY}{$EndIf C3}


{ GetMessageStatus }

  MSGSTATUS_HIGHLIGHTED     = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MSGSTATUS_HIGHLIGHTED}{$EndIf C3}
  MSGSTATUS_TAGGED          = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MSGSTATUS_TAGGED}{$EndIf C3}
  MSGSTATUS_HIDDEN          = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM MSGSTATUS_HIDDEN}{$EndIf C3}
  MSGSTATUS_DELMARKED       = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM MSGSTATUS_DELMARKED}{$EndIf C3}

{ Bits for remote message status }

  MSGSTATUS_REMOTE_DOWNLOAD     = ULONG($00001000);
  {$IfDef C3}{$EXTERNALSYM MSGSTATUS_REMOTE_DOWNLOAD}{$EndIf C3}
  MSGSTATUS_REMOTE_DELETE       = ULONG($00002000);
  {$IfDef C3}{$EXTERNALSYM MSGSTATUS_REMOTE_DELETE}{$EndIf C3}

{ SaveContentsSort }

  RECURSIVE_SORT            = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM RECURSIVE_SORT}{$EndIf C3}

{ PR_STATUS property }

  FLDSTATUS_HIGHLIGHTED     = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM FLDSTATUS_HIGHLIGHTED}{$EndIf C3}
  FLDSTATUS_TAGGED          = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM FLDSTATUS_TAGGED}{$EndIf C3}
  FLDSTATUS_HIDDEN          = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM FLDSTATUS_HIDDEN}{$EndIf C3}
  FLDSTATUS_DELMARKED       = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM FLDSTATUS_DELMARKED}{$EndIf C3}

{ IMsgStore Interface ------------------------------------------------------ }

{ PR_STORE_SUPPORT_MASK bits }
const
  STORE_ENTRYID_UNIQUE      = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM STORE_ENTRYID_UNIQUE}{$EndIf C3}
  STORE_READONLY            = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM STORE_READONLY}{$EndIf C3}
  STORE_SEARCH_OK           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM STORE_SEARCH_OK}{$EndIf C3}
  STORE_MODIFY_OK           = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM STORE_MODIFY_OK}{$EndIf C3}
  STORE_CREATE_OK           = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM STORE_CREATE_OK}{$EndIf C3}
  STORE_ATTACH_OK           = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM STORE_ATTACH_OK}{$EndIf C3}
  STORE_OLE_OK              = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM STORE_OLE_OK}{$EndIf C3}
  STORE_SUBMIT_OK           = ULONG($00000080);
  {$IfDef C3}{$EXTERNALSYM STORE_SUBMIT_OK}{$EndIf C3}
  STORE_NOTIFY_OK           = ULONG($00000100);
  {$IfDef C3}{$EXTERNALSYM STORE_NOTIFY_OK}{$EndIf C3}
  STORE_MV_PROPS_OK         = ULONG($00000200);
  {$IfDef C3}{$EXTERNALSYM STORE_MV_PROPS_OK}{$EndIf C3}
  STORE_CATEGORIZE_OK       = ULONG($00000400);
  {$IfDef C3}{$EXTERNALSYM STORE_CATEGORIZE_OK}{$EndIf C3}
  STORE_RTF_OK              = ULONG($00000800);
  {$IfDef C3}{$EXTERNALSYM STORE_RTF_OK}{$EndIf C3}
  STORE_RESTRICTION_OK      = ULONG($00001000);
  {$IfDef C3}{$EXTERNALSYM STORE_RESTRICTION_OK}{$EndIf C3}
  STORE_SORT_OK             = ULONG($00002000);
  {$IfDef C3}{$EXTERNALSYM STORE_SORT_OK}{$EndIf C3}

{ PR_STORE_STATE bits, try not to collide with PR_STORE_SUPPORT_MASK }

  STORE_HAS_SEARCHES        = ULONG($01000000);
  {$IfDef C3}{$EXTERNALSYM STORE_HAS_SEARCHES}{$EndIf C3}

{ OpenEntry() }

//***** MAPI_MODIFY             ((ULONG) 0x00000001) above
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** MAPI_BEST_ACCESS        ((ULONG) 0x00000010) above

{ SetReceiveFolder() }

//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ GetReceiveFolder() }

//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ GetReceiveFolderTable() }

//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below

{ StoreLogoff() }

  LOGOFF_NO_WAIT            = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_NO_WAIT}{$EndIf C3}
  LOGOFF_ORDERLY            = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_ORDERLY}{$EndIf C3}
  LOGOFF_PURGE              = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_PURGE}{$EndIf C3}
  LOGOFF_ABORT              = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_ABORT}{$EndIf C3}
  LOGOFF_QUIET              = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_QUIET}{$EndIf C3}

  LOGOFF_COMPLETE           = ULONG($00010000);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_COMPLETE}{$EndIf C3}
  LOGOFF_INBOUND            = ULONG($00020000);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_INBOUND}{$EndIf C3}
  LOGOFF_OUTBOUND           = ULONG($00040000);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_OUTBOUND}{$EndIf C3}
  LOGOFF_OUTBOUND_QUEUE     = ULONG($00080000);
  {$IfDef C3}{$EXTERNALSYM LOGOFF_OUTBOUND_QUEUE}{$EndIf C3}

{ SetLockState() }

  MSG_LOCKED                = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MSG_LOCKED}{$EndIf C3}
  MSG_UNLOCKED              = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM MSG_UNLOCKED}{$EndIf C3}

{ Flag bits for PR_VALID_FOLDER_MASK }

  FOLDER_IPM_SUBTREE_VALID          = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM FOLDER_IPM_SUBTREE_VALID}{$EndIf C3}
  FOLDER_IPM_INBOX_VALID            = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM FOLDER_IPM_INBOX_VALID}{$EndIf C3}
  FOLDER_IPM_OUTBOX_VALID           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM FOLDER_IPM_OUTBOX_VALID}{$EndIf C3}
  FOLDER_IPM_WASTEBASKET_VALID      = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM FOLDER_IPM_WASTEBASKET_VALID}{$EndIf C3}
  FOLDER_IPM_SENTMAIL_VALID         = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM FOLDER_IPM_SENTMAIL_VALID}{$EndIf C3}
  FOLDER_VIEWS_VALID                = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM FOLDER_VIEWS_VALID}{$EndIf C3}
  FOLDER_COMMON_VIEWS_VALID         = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM FOLDER_COMMON_VIEWS_VALID}{$EndIf C3}
  FOLDER_FINDER_VALID               = ULONG($00000080);
  {$IfDef C3}{$EXTERNALSYM FOLDER_FINDER_VALID}{$EndIf C3}

{ IMessage Interface ------------------------------------------------------- }

{ SubmitMessage }

const
  FORCE_SUBMIT                  = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM FORCE_SUBMIT}{$EndIf C3}

{ Flags defined in PR_MESSAGE_FLAGS }

  MSGFLAG_READ              = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_READ}{$EndIf C3}
  MSGFLAG_UNMODIFIED        = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_UNMODIFIED}{$EndIf C3}
  MSGFLAG_SUBMIT            = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_SUBMIT}{$EndIf C3}
  MSGFLAG_UNSENT            = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_UNSENT}{$EndIf C3}
  MSGFLAG_HASATTACH         = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_HASATTACH}{$EndIf C3}
  MSGFLAG_FROMME            = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_FROMME}{$EndIf C3}
  MSGFLAG_ASSOCIATED        = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_ASSOCIATED}{$EndIf C3}
  MSGFLAG_RESEND            = ULONG($00000080);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_RESEND}{$EndIf C3}
  MSGFLAG_RN_PENDING        = ULONG($00000100);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_RN_PENDING}{$EndIf C3}
  MSGFLAG_NRN_PENDING       = ULONG($00000200);
  {$IfDef C3}{$EXTERNALSYM MSGFLAG_NRN_PENDING}{$EndIf C3}

{ Flags defined in PR_SUBMIT_FLAGS }

  SUBMITFLAG_LOCKED         = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM SUBMITFLAG_LOCKED}{$EndIf C3}
  SUBMITFLAG_PREPROCESS     = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM SUBMITFLAG_PREPROCESS}{$EndIf C3}

{ GetAttachmentTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ GetRecipientTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ ModifyRecipients }

{ ((ULONG) 0x00000001 is not a valid flag on ModifyRecipients. }
  MODRECIP_ADD              = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MODRECIP_ADD}{$EndIf C3}
  MODRECIP_MODIFY           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM MODRECIP_MODIFY}{$EndIf C3}
  MODRECIP_REMOVE           = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM MODRECIP_REMOVE}{$EndIf C3}

{ SetReadFlag }

  SUPPRESS_RECEIPT          = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM SUPPRESS_RECEIPT}{$EndIf C3}
  CLEAR_READ_FLAG           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM CLEAR_READ_FLAG}{$EndIf C3}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) below
//***** GENERATE_RECEIPT_ONLY   ((ULONG) 0x00000010) above
  CLEAR_RN_PENDING          = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM CLEAR_RN_PENDING}{$EndIf C3}
  CLEAR_NRN_PENDING         = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM CLEAR_NRN_PENDING}{$EndIf C3}

{ DeleteAttach }

  ATTACH_DIALOG             = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM ATTACH_DIALOG}{$EndIf C3}

{ PR_SECURITY values }
  SECURITY_SIGNED           = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM SECURITY_SIGNED}{$EndIf C3}
  SECURITY_ENCRYPTED        = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM SECURITY_ENCRYPTED}{$EndIf C3}

{ PR_PRIORITY values }
  PRIO_URGENT               = LongInt(1);
  {$IfDef C3}{$EXTERNALSYM PRIO_URGENT}{$EndIf C3}
  PRIO_NORMAL               = LongInt(0);
  {$IfDef C3}{$EXTERNALSYM PRIO_NORMAL}{$EndIf C3}
  PRIO_NONURGENT            = LongInt(-1);
  {$IfDef C3}{$EXTERNALSYM PRIO_NONURGENT}{$EndIf C3}

{ PR_SENSITIVITY values }
  SENSITIVITY_NONE                      = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM SENSITIVITY_NONE}{$EndIf C3}
  SENSITIVITY_PERSONAL                  = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM SENSITIVITY_PERSONAL}{$EndIf C3}
  SENSITIVITY_PRIVATE                   = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM SENSITIVITY_PRIVATE}{$EndIf C3}
  SENSITIVITY_COMPANY_CONFIDENTIAL      = ULONG($00000003);
  {$IfDef C3}{$EXTERNALSYM SENSITIVITY_COMPANY_CONFIDENTIAL}{$EndIf C3}

{ PR_IMPORTANCE values }
  IMPORTANCE_LOW            = LongInt(0);
  {$IfDef C3}{$EXTERNALSYM IMPORTANCE_LOW}{$EndIf C3}
  IMPORTANCE_NORMAL         = LongInt(1);
  {$IfDef C3}{$EXTERNALSYM IMPORTANCE_NORMAL}{$EndIf C3}
  IMPORTANCE_HIGH           = LongInt(2);
  {$IfDef C3}{$EXTERNALSYM IMPORTANCE_HIGH}{$EndIf C3}

{ IAttach Interface -------------------------------------------------------- }

{ IAttach attachment methods: PR_ATTACH_METHOD values }

const
  NO_ATTACHMENT             = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM NO_ATTACHMENT}{$EndIf C3}
  ATTACH_BY_VALUE           = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM ATTACH_BY_VALUE}{$EndIf C3}
  ATTACH_BY_REFERENCE       = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM ATTACH_BY_REFERENCE}{$EndIf C3}
  ATTACH_BY_REF_RESOLVE     = ULONG($00000003);
  {$IfDef C3}{$EXTERNALSYM ATTACH_BY_REF_RESOLVE}{$EndIf C3}
  ATTACH_BY_REF_ONLY        = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM ATTACH_BY_REF_ONLY}{$EndIf C3}
  ATTACH_EMBEDDED_MSG       = ULONG($00000005);
  {$IfDef C3}{$EXTERNALSYM ATTACH_EMBEDDED_MSG}{$EndIf C3}
  ATTACH_OLE                = ULONG($00000006);
  {$IfDef C3}{$EXTERNALSYM ATTACH_OLE}{$EndIf C3}

{ Address Book interface definition }

{ ADRPARM ulFlags - top 4 bits used for versioning }

function GET_ADRPARM_VERSION(ulFlags: ULONG): ULONG;
{$IfDef C3}{$EXTERNALSYM GET_ADRPARM_VERSION}{$EndIf C3}
function SET_ADRPARM_VERSION(ulFlags, ulVersion: ULONG): ULONG;
{$IfDef C3}{$EXTERNALSYM SET_ADRPARM_VERSION}{$EndIf C3}

{ Current versions of ADRPARM }
const
  ADRPARM_HELP_CTX          = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM ADRPARM_HELP_CTX}{$EndIf C3}

{ ulFlags   - bit fields }
  DIALOG_MODAL              = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM DIALOG_MODAL}{$EndIf C3}
  DIALOG_SDI                = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM DIALOG_SDI}{$EndIf C3}
  DIALOG_OPTIONS            = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM DIALOG_OPTIONS}{$EndIf C3}
  ADDRESS_ONE               = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM ADDRESS_ONE}{$EndIf C3}
  AB_SELECTONLY             = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM AB_SELECTONLY}{$EndIf C3}
  AB_RESOLVE                = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM AB_RESOLVE}{$EndIf C3}

// ---------------------------------
//  PR_DISPLAY_TYPEs
//
// *  These standard display types are
// *  by default handled by MAPI.
// *  They have default icons associated
// *  with them.

{ For address book contents tables }
  DT_MAILUSER           = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM DT_MAILUSER}{$EndIf C3}
  DT_DISTLIST           = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM DT_DISTLIST}{$EndIf C3}
  DT_FORUM              = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM DT_FORUM}{$EndIf C3}
  DT_AGENT              = ULONG($00000003);
  {$IfDef C3}{$EXTERNALSYM DT_AGENT}{$EndIf C3}
  DT_ORGANIZATION       = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM DT_ORGANIZATION}{$EndIf C3}
  DT_PRIVATE_DISTLIST   = ULONG($00000005);
  {$IfDef C3}{$EXTERNALSYM DT_PRIVATE_DISTLIST}{$EndIf C3}
  DT_REMOTE_MAILUSER    = ULONG($00000006);
  {$IfDef C3}{$EXTERNALSYM DT_REMOTE_MAILUSER}{$EndIf C3}

{ For address book hierarchy tables }
  DT_MODIFIABLE         = ULONG($00010000);
  {$IfDef C3}{$EXTERNALSYM DT_MODIFIABLE}{$EndIf C3}
  DT_GLOBAL             = ULONG($00020000);
  {$IfDef C3}{$EXTERNALSYM DT_GLOBAL}{$EndIf C3}
  DT_LOCAL              = ULONG($00030000);
  {$IfDef C3}{$EXTERNALSYM DT_LOCAL}{$EndIf C3}
  DT_WAN                = ULONG($00040000);
  {$IfDef C3}{$EXTERNALSYM DT_WAN}{$EndIf C3}
  DT_NOT_SPECIFIC       = ULONG($00050000);
  {$IfDef C3}{$EXTERNALSYM DT_NOT_SPECIFIC}{$EndIf C3}

{ For folder hierarchy tables }
  DT_FOLDER             = ULONG($01000000);
  {$IfDef C3}{$EXTERNALSYM DT_FOLDER}{$EndIf C3}
  DT_FOLDER_LINK        = ULONG($02000000);
  {$IfDef C3}{$EXTERNALSYM DT_FOLDER_LINK}{$EndIf C3}

{ Accelerator callback for DIALOG_SDI form of AB UI }
type
  PFnABSDI = ^TAccelerateABSDI;
  ACCELERATEABSDI = function (ulUIParam: ULONG; lpvmsg: Pointer): BOOL; stdcall;
  {$IfDef C3}{$EXTERNALSYM ACCELERATEABSDI}{$EndIf C3}
  TAccelerateABSDI = ACCELERATEABSDI;

{ Callback to application telling it that the DIALOG_SDI form of the
  AB UI has been dismissed.  This is so that the above LPFNABSDI
  function doesn't keep being called. }

  PFnDismiss = ^TDismissModeless;
  DISMISSMODELESS = function (ulUIParam: ULONG; lpvContext: Pointer): Pointer; stdcall;
  {$IfDef C3}{$EXTERNALSYM DISMISSMODELESS}{$EndIf C3}
  TDismissModeless = DISMISSMODELESS;

{ Prototype for the client function hooked to an optional button on
  the address book dialog }

  PFnButton = ^TFnButton;
  FNBUTTON = function (ulUIParam: ULONG; lpvContext: Pointer; cbEntryID: ULONG;
    lpSelection: PEntryID; ulFlags: ULONG): SCODE; stdcall;
  //{$IfDef C3}{$EXTERNALSYM FNBUTTON}{$EndIf C3}
  TFnButton = FNBUTTON;

{ Parameters for the address book dialog }

  PAdrParam = ^TAdrParam;
  _ADRPARM = record
    cbABContEntryID: ULONG;
    lpABContEntryID: PEntryID;
    ulFlags: ULONG;
    lpReserved: Pointer;
    ulHelpContext: ULONG;
    lpszHelpFileName: LPTSTR;
    lpfnABSDI: PFnABSDI;
    lpfnDismiss: PFnDismiss;
    lpvDismissContext: Pointer;
    lpszCaption: LPTSTR;
    lpszNewEntryTitle: LPTSTR;
    lpszDestWellsTitle: LPTSTR;
    cDestFields: ULONG;
    nDestFieldFocus: ULONG;
    lppszDestTitles: ^LPTSTR;
    lpulDestComps: ^ULONG;
    lpContRestriction: PSRestriction;
    lpHierRestriction: PSRestriction;
  end;
  {$IfDef C3}{$EXTERNALSYM _ADRPARM}{$EndIf C3}
  TAdrParam = _ADRPARM;
  ADRPARM = _ADRPARM;
  {$IfDef C3}{$EXTERNALSYM ADRPARM}{$EndIf C3}

{ Random flags }
const
{ Flag set in MAPI one off entryids }
  MAPI_ONE_OFF_NO_RICH_INFO = $0001;
  {$IfDef C3}{$EXTERNALSYM MAPI_ONE_OFF_NO_RICH_INFO}{$EndIf C3}

{ Flag for deferred error }
  MAPI_DEFERRED_ERRORS      = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM MAPI_DEFERRED_ERRORS}{$EndIf C3}

{ Flag for creating and using Folder Associated Information Messages }
  MAPI_ASSOCIATED           = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM MAPI_ASSOCIATED}{$EndIf C3}

{ Flags for OpenMessageStore() }

  MDB_NO_DIALOG             = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MDB_NO_DIALOG}{$EndIf C3}
  MDB_WRITE                 = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM MDB_WRITE}{$EndIf C3}
//***** MAPI_DEFERRED_ERRORS    ((ULONG) 0x00000008) above
//***** MAPI_BEST_ACCESS        ((ULONG) 0x00000010) above
  MDB_TEMPORARY             = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM MDB_TEMPORARY}{$EndIf C3}
  MDB_NO_MAIL               = ULONG($00000080);
  {$IfDef C3}{$EXTERNALSYM MDB_NO_MAIL}{$EndIf C3}

{ Flags for OpenAddressBook }

  AB_NO_DIALOG              = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM AB_NO_DIALOG}{$EndIf C3}

{ IMAPIControl Interface --------------------------------------------------- }

{ Interface used in controls (particularly the button) defined by Display Tables }

{ Flags for GetState }

  MAPI_ENABLED         = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM MAPI_ENABLED}{$EndIf C3}
  MAPI_DISABLED        = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_DISABLED}{$EndIf C3}

type
  IMAPIControl = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function Activate(ulFlags, ulUIParam: ULONG): HResult; stdcall;
    function GetState(ulFlags: ULONG; var lpulState: ULONG): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMAPIControl}{$EndIf C3}

{ Display Tables ----------------------------------------------------------- }

{ Flags used in display tables - that is, PR_CONTROL_FLAGS }

const
  DT_MULTILINE          = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM DT_MULTILINE}{$EndIf C3}
  DT_EDITABLE           = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM DT_EDITABLE}{$EndIf C3}
  DT_REQUIRED           = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM DT_REQUIRED}{$EndIf C3}
  DT_SET_IMMEDIATE      = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM DT_SET_IMMEDIATE}{$EndIf C3}
  DT_PASSWORD_EDIT      = ULONG($00000010);
  {$IfDef C3}{$EXTERNALSYM DT_PASSWORD_EDIT}{$EndIf C3}
  DT_ACCEPT_DBCS        = ULONG($00000020);
  {$IfDef C3}{$EXTERNALSYM DT_ACCEPT_DBCS}{$EndIf C3}
  DT_SET_SELECTION      = ULONG($00000040);
  {$IfDef C3}{$EXTERNALSYM DT_SET_SELECTION}{$EndIf C3}

{ Display Table structures }

  DTCT_LABEL            = ULONG($00000000);
  {$IfDef C3}{$EXTERNALSYM DTCT_LABEL}{$EndIf C3}
  DTCT_EDIT             = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM DTCT_EDIT}{$EndIf C3}
  DTCT_LBX              = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM DTCT_LBX}{$EndIf C3}
  DTCT_COMBOBOX         = ULONG($00000003);
  {$IfDef C3}{$EXTERNALSYM DTCT_COMBOBOX}{$EndIf C3}
  DTCT_DDLBX            = ULONG($00000004);
  {$IfDef C3}{$EXTERNALSYM DTCT_DDLBX}{$EndIf C3}
  DTCT_CHECKBOX         = ULONG($00000005);
  {$IfDef C3}{$EXTERNALSYM DTCT_CHECKBOX}{$EndIf C3}
  DTCT_GROUPBOX         = ULONG($00000006);
  {$IfDef C3}{$EXTERNALSYM DTCT_GROUPBOX}{$EndIf C3}
  DTCT_BUTTON           = ULONG($00000007);
  {$IfDef C3}{$EXTERNALSYM DTCT_BUTTON}{$EndIf C3}
  DTCT_PAGE             = ULONG($00000008);
  {$IfDef C3}{$EXTERNALSYM DTCT_PAGE}{$EndIf C3}
  DTCT_RADIOBUTTON      = ULONG($00000009);
  {$IfDef C3}{$EXTERNALSYM DTCT_RADIOBUTTON}{$EndIf C3}
  DTCT_MVLISTBOX        = ULONG($0000000B);
  {$IfDef C3}{$EXTERNALSYM DTCT_MVLISTBOX}{$EndIf C3}
  DTCT_MVDDLBX          = ULONG($0000000C);
  {$IfDef C3}{$EXTERNALSYM DTCT_MVDDLBX}{$EndIf C3}

type
{ Labels }
  PDTblLabel = ^TDTblLabel;
  _DTBLLABEL = record
    ulbLpszLabelName: ULONG;
    ulFlags: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLLABEL}{$EndIf C3}
  TDTblLabel = _DTBLLABEL;
  DTBLLABEL = _DTBLLABEL;
  {$IfDef C3}{$EXTERNALSYM DTBLLABEL}{$EndIf C3}

(*!!!
#define SizedDtblLabel(n,u) \
struct _DTBLLABEL_ ## u \
{ \
    DTBLLABEL   dtbllabel; \
    TCHAR       lpszLabelName[n]; \
} u
*)

{ Simple Text Edits }
  PDTblEdit = ^TDTblEdit;
  _DTBLEDIT = record
    ulbLpszCharsAllowed: ULONG;
    ulFlags: ULONG;
    ulNumCharsAllowed: ULONG;
    ulPropTag: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLEDIT}{$EndIf C3}
  TDTblEdit = _DTBLEDIT;
  DTBLEDIT = _DTBLEDIT;
  {$IfDef C3}{$EXTERNALSYM DTBLEDIT}{$EndIf C3}

(*!!!
#define SizedDtblEdit(n,u) \
struct _DTBLEDIT_ ## u \
{ \
    DTBLEDIT    dtbledit; \
    TCHAR       lpszCharsAllowed[n]; \
} u
*)

{ List Box }
const
  MAPI_NO_HBAR          = ULONG($00000001);
  {$IfDef C3}{$EXTERNALSYM MAPI_NO_HBAR}{$EndIf C3}
  MAPI_NO_VBAR          = ULONG($00000002);
  {$IfDef C3}{$EXTERNALSYM MAPI_NO_VBAR}{$EndIf C3}

type
  PDTblLbx = ^TDTblLbx;
  _DTBLLBX = record
    ulFlags: ULONG;
    ulPRSetProperty: ULONG;
    ulPRTableName: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLLBX}{$EndIf C3}
  TDTblLbx = _DTBLLBX;
  DTBLLBX = _DTBLLBX;
  {$IfDef C3}{$EXTERNALSYM DTBLLBX}{$EndIf C3}

{ Combo Box }
  PDTblComboBox = ^TDTblComboBox;
  _DTBLCOMBOBOX = record
    ulbLpszCharsAllowed: ULONG;
    ulFlags: ULONG;
    ulNumCharsAllowed: ULONG;
    ulPRPropertyName: ULONG;
    ulPRTableName: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLCOMBOBOX}{$EndIf C3}
  TDTblComboBox = _DTBLCOMBOBOX;
  DTBLCOMBOBOX = _DTBLCOMBOBOX;
  {$IfDef C3}{$EXTERNALSYM DTBLCOMBOBOX}{$EndIf C3}

(*!!!
#define SizedDtblComboBox(n,u) \
struct _DTBLCOMBOBOX_ ## u \
{ \
    DTBLCOMBOBOX    dtblcombobox; \
    TCHAR           lpszCharsAllowed[n]; \
} u
*)

{ Drop Down }
  PDTblDDLbx = ^TDTblDDLbx;
  _DTBLDDLBX = record
    ulFlags: ULONG;
    ulPRDisplayProperty: ULONG;
    ulPRSetProperty: ULONG;
    ulPRTableName: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLDDLBX}{$EndIf C3}
  TDTblDDLbx = _DTBLDDLBX;
  DTBLDDLBX = _DTBLDDLBX;
  {$IfDef C3}{$EXTERNALSYM DTBLDDLBX}{$EndIf C3}

{ Check Box }
  PDTblCheckBox = ^TDTblCheckBox;
  _DTBLCHECKBOX = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulPRPropertyName: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLCHECKBOX}{$EndIf C3}
  TDTblCheckBox = _DTBLCHECKBOX;
  DTBLCHECKBOX = _DTBLCHECKBOX;
  {$IfDef C3}{$EXTERNALSYM DTBLCHECKBOX}{$EndIf C3}

(*!!!
#define SizedDtblCheckBox(n,u) \
struct _DTBLCHECKBOX_ ## u \
{ \
    DTBLCHECKBOX    dtblcheckbox; \
    TCHAR       lpszLabel[n]; \
} u
*)

{ Group Box }
  PDTblGroupBox = ^TDTblGroupBox;
  _DTBLGROUPBOX = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLGROUPBOX}{$EndIf C3}
  TDTblGroupBox = _DTBLGROUPBOX;
  DTBLGROUPBOX = _DTBLGROUPBOX;
  {$IfDef C3}{$EXTERNALSYM DTBLGROUPBOX}{$EndIf C3}

(*!!!
#define SizedDtblGroupBox(n,u) \
struct _DTBLGROUPBOX_ ## u \
{ \
    DTBLGROUPBOX    dtblgroupbox; \
    TCHAR           lpszLabel[n]; \
} u
*)

{ Button control }
  PDTblButton = ^TDTblButton;
  _DTBLBUTTON = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulPRControl: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLBUTTON}{$EndIf C3}
  TDTblButton = _DTBLBUTTON;
  DTBLBUTTON = _DTBLBUTTON;
  {$IfDef C3}{$EXTERNALSYM DTBLBUTTON}{$EndIf C3}

(*!!!
#define SizedDtblButton(n,u) \
struct _DTBLBUTTON_ ## u \
{ \
    DTBLBUTTON  dtblbutton; \
    TCHAR       lpszLabel[n]; \
} u
*)

{ Pages }
  PDTblPage = ^TDTblPage;
  _DTBLPAGE = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulbLpszComponent: ULONG;
    ulContext: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLPAGE}{$EndIf C3}
  TDTblPage = _DTBLPAGE;
  DTBLPAGE = _DTBLPAGE;
  {$IfDef C3}{$EXTERNALSYM DTBLPAGE}{$EndIf C3}

(*!!!
#define SizedDtblPage(n,n1,u) \
struct _DTBLPAGE_ ## u \
{ \
    DTBLPAGE    dtblpage; \
    TCHAR       lpszLabel[n]; \
    TCHAR       lpszComponent[n1]; \
} u
*)

{ Radio button }
  PDTblRadioButton = ^TDTblRadioButton;
  _DTBLRADIOBUTTON = record
    ulbLpszLabel: ULONG;
    ulFlags: ULONG;
    ulcButtons: ULONG;
    ulPropTag: ULONG;
    lReturnValue: LongInt;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLRADIOBUTTON}{$EndIf C3}
  TDTblRadioButton = _DTBLRADIOBUTTON;
  DTBLRADIOBUTTON = _DTBLRADIOBUTTON;
  {$IfDef C3}{$EXTERNALSYM DTBLRADIOBUTTON}{$EndIf C3}

(*!!!
#define SizedDtblRadioButton(n,u) \
struct _DTBLRADIOBUTTON_ ## u \
{ \
    DTBLRADIOBUTTON dtblradiobutton; \
    TCHAR           lpszLabel[n]; \
} u
*)

{ MultiValued listbox }
  PDTblMvListBox = ^TDTblMvListBox;
  _DTBLMVLISTBOX = record
    ulFlags: ULONG;
    ulMVPropTag: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLMVLISTBOX}{$EndIf C3}
  TDTblMvListBox = _DTBLMVLISTBOX;
  DTBLMVLISTBOX = _DTBLMVLISTBOX;
  {$IfDef C3}{$EXTERNALSYM DTBLMVLISTBOX}{$EndIf C3}

{ MultiValued dropdown }
  PDTblMvDDLbx = ^TDTblMvDDLbx;
  _DTBLMVDDLBX = record
    ulFlags: ULONG;
    ulMVPropTag: ULONG;
  end;
  {$IfDef C3}{$EXTERNALSYM _DTBLMVDDLBX}{$EndIf C3}
  TDTblMvDDLbx = _DTBLMVDDLBX;
  DTBLMVDDLBX = _DTBLMVDDLBX;
  {$IfDef C3}{$EXTERNALSYM DTBLMVDDLBX}{$EndIf C3}

{ IProviderAdmin Interface ------------------------------------------------- }

{ Flags for ConfigureMsgService }
const
  UI_SERVICE                  = $00000002;
  {$IfDef C3}{$EXTERNALSYM UI_SERVICE}{$EndIf C3}
  SERVICE_UI_ALWAYS           = $00000002; // Duplicate UI_SERVICE for consistency and compatibility
  {$IfDef C3}{$EXTERNALSYM SERVICE_UI_ALWAYS}{$EndIf C3}
  SERVICE_UI_ALLOWED          = $00000010;
  {$IfDef C3}{$EXTERNALSYM SERVICE_UI_ALLOWED}{$EndIf C3}
  UI_CURRENT_PROVIDER_FIRST   = $00000004;
  {$IfDef C3}{$EXTERNALSYM UI_CURRENT_PROVIDER_FIRST}{$EndIf C3}
// MSG_SERVICE_UI_READ_ONLY         0x00000008 - in MAPISPI.H

{ GetProviderTable() }
//***** MAPI_UNICODE            ((ULONG) 0x80000000) above

{ Values for PR_RESOURCE_FLAGS in message service table }

type
  IMessage = interface;

  IMAPIFolder = interface(IMAPIContainer)
    function CreateMessage(lpInterface: PIID; ulFlags: ULONG;
      out lppMessage: IMessage): HResult; stdcall;
    function CopyMessages(lpMsgList: PEntryList; lpInterface: PIID;
      lpDestFolder: Pointer; ulUIParam: ULONG; lpProgress: IMAPIProgress;
      ulFlags: ULONG): HResult; stdcall;
    function DeleteMessages(lpMsgList: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function CreateFolder(ulFolderType: ULONG; lpszFolderName,
      lpszFolderComment: LPTSTR; lpInterface: PIID; ulFlags: ULONG;
      out lppFolder: IMAPIFolder): HResult; stdcall;
    function CopyFolder(cbEntryID: ULONG; lpEntryID: PEntryID;
      lpInterface: PIID; lpDestFolder: Pointer; lpszNewFolderName: LPTSTR;
      ulUIParam: ULONG; lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function DeleteFolder(cbEntryID: ULONG; lpEntryID: PEntryID; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function SetReadFlags(lpMsgList: PEntryList; ulUIParam: ULONG;
      lpProgress: IMAPIProgress; ulFlags: ULONG): HResult; stdcall;
    function GetMessageStatus(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulFlags: ULONG; var lpulMessageStatus: ULONG): HResult; stdcall;
    function SetMessageStatus(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulNewStatus, ulNewStatusMask: ULONG; var lpulOldStatus: ULONG): HResult; stdcall;
    function SaveContentsSort(lpSortCriteria: PSSortOrderSet;
      ulFlags: ULONG): HResult; stdcall;
    function EmptyFolder(ulUIParam: ULONG; lpProgress: IMAPIProgress;
      ulFlags: ULONG): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMAPIFolder}{$EndIf C3}

  IMsgStore = interface(IMAPIProp)
    function Advise(cbEntryID: ULONG; lpEntryID: PEntryID; ulEventMask: ULONG;
      lpAdviseSink: IMAPIAdviseSink; var lpulConnection: ULONG): HResult; stdcall;
    function Unadvise(ulConnection: ULONG): HResult; stdcall;
    function CompareEntryIDs(cbEntryID1: ULONG; lpEntryID1: PEntryID;
      cbEntryID2: ULONG; lpEntryID2: PEntryID; ulFlags: ULONG;
      var lpulResult: ULONG): HResult; stdcall;
    function OpenEntry(cbEntryID: ULONG; lpEntryID: PEntryID;
      lpInterface: PIID; ulFlags: ULONG; var lpulObjType: ULONG;
      out lppUnk: IUnknown): HResult; stdcall;
    function SetReceiveFolder(lpszMessageClass: LPTSTR; ulFlags, cbEntryID: ULONG;
      lpEntryID: PEntryID): HResult; stdcall;
    function GetReceiveFolder(lpszMessageClass: LPTSTR; ulFlags: ULONG;
      var lpcbEntryID: ULONG; lppEntryID: PEntryID;
      lppszExplicitClass: LPTSTR): HResult; stdcall;
    function GetReceiveFolderTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function StoreLogoff(var lpulFlags: ULONG): HResult; stdcall;
    function AbortSubmit(cbEntryID: ULONG; lpEntryID: PEntryID;
      ulFlags: ULONG): HResult; stdcall;
    function GetOutgoingQueue(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function SetLockState(lpMessage: IMessage; ulLockState: ULONG): HResult; stdcall;
    function FinishedMsg(ulFlags, cbEntryID: ULONG; lpEntryID: PEntryID): HResult; stdcall;
    function NotifyNewMail(lpNotification: PNotification): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMsgStore}{$EndIf C3}

  IAttach = interface(IMAPIProp)
  end;
  {$IfDef C3}{$EXTERNALSYM IAttach}{$EndIf C3}

  IMessage = interface(IMAPIProp)
    function GetAttachmentTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function OpenAttach(ulAttachmentNum: ULONG; lpInterface: PIID;
      ulFlags: ULONG; out lppAttach: IAttach): HResult; stdcall;
    function CreateAttach(lpInterface: PIID; ulFlags: ULONG;
      var lpulAttachmentNum: ULONG; out lppAttach: IAttach): HResult; stdcall;
    function DeleteAttach(ulAttachmentNum, ulUIParam: ULONG; lpProgress: IMAPIProgress;
      ulFlags: ULONG): HResult; stdcall;
    function GetRecipientTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function ModifyRecipients(ulFlags: ULONG; lpMods: PAdrList): HResult; stdcall;
    function SubmitMessage(ulFlags: ULONG): HResult; stdcall;
    function SetReadFlag(ulFlags: ULONG): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IMessage}{$EndIf C3}

  IProviderAdmin = interface(IUnknown)
    function GetLastError(hResult: HRESULT; ulFlags: ULONG;
      var lppMAPIError: TMapiError): HResult; stdcall;
    function GetProviderTable(ulFlags: ULONG; out lppTable: IMAPITable): HResult; stdcall;
    function CreateProvider(lpszProvider: LPTSTR; cValues: ULONG;
      lpProps: PSPropValue; ulUIParam, ulFlags: ULONG; lpUID: TMapiUID): HResult; stdcall;
    function DeleteProvider(lpUID: PMapiUID): HResult; stdcall;
    function OpenProfileSection(lpUID: PMapiUID; lpInterface: PIID;
      ulFlags: ULONG; out lppProfSect: IProfSect): HResult; stdcall;
  end;
  {$IfDef C3}{$EXTERNALSYM IProviderAdmin}{$EndIf C3}


{******************************************************************************}
implementation
{******************************************************************************}
uses
  SysUtils;

{--------------------------------------}

function IsEqualMAPIUID(lpuid1, lpuid2: TMapiUID): Boolean;
begin
  Result := CompareMem(@lpuid1, @lpuid2, Sizeof(TMapiUID));
end;

{--------------------------------------}

function PROP_TYPE(ulPropTag: ULONG): ULONG;
begin
  Result := ulPropTag and PROP_TYPE_MASK;
end;

{--------------------------------------}

function PROP_ID(ulPropTag: ULONG): ULONG;
begin
  Result := ulPropTag shr 16;
end;

{--------------------------------------}

function PROP_TAG(ulPropType, ulPropID: ULONG): ULONG;
begin
  Result := (ulPropID shl 16) or ulPropType;
end;

{--------------------------------------}

function CHANGE_PROP_TYPE(ulPropTag, ulPropType: ULONG): ULONG;
begin
  Result := (ULONG($FFFF0000) and ulPropTag) or ulPropType;
end;

{--------------------------------------}

function MVI_PROP(tag: ULONG): ULONG;
begin
  Result := tag or MVI_FLAG;
end;

{--------------------------------------}

function GET_ADRPARM_VERSION(ulFlags: ULONG): ULONG;
begin
  Result := ulFlags and $F0000000;
end;

{--------------------------------------}

function SET_ADRPARM_VERSION(ulFlags, ulVersion: ULONG): ULONG;
begin
  Result := ulVersion or (ulFlags and $0FFFFFFF);
end;

{--------------------------------------}

end.
