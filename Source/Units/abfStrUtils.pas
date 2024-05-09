{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfStrUtils;

{$I abf.inc}

interface

uses
  Windows, Classes{$IFDEF D9}, WideStrings{$ENDIF},
  abfClasses;

//==============================================================================
// Types
//==============================================================================

type
  TabfAnsiCharSet = set of AnsiChar;
  TabfCharSet = set of Char;
  TabfWideCharSet = WideString;

{$IfNDef D6}
  UTF8String = type string;
  PUTF8String = ^UTF8String;
  {$NODEFINE UTF8String}
  {$NODEFINE PUTF8String}
{$EndIf D6}

{$IfNDef D3}
//------------------------------------------------------------------------------
// LastDelimiter returns the byte index in S of the rightmost whole
// character that matches any character in Delimiters (except null (#0)).
// S may contain multibyte characters; Delimiters must contain only single
// byte non-null characters.
// Example: LastDelimiter('\.:', 'c:\filename.ext') returns 12.
function LastDelimiter(const Delimiters, S: AnsiString): Integer;
{$EndIf D3}

{$IfNDef D6}
//------------------------------------------------------------------------------
// WideUpperCase returns S converted to upper case.
// The conversion uses the current locale.
function WideUpperCase(const S: WideString): WideString;

//------------------------------------------------------------------------------
// WideLowerCase returns S converted to lower case.
// The conversion uses the current locale.
function WideLowerCase(const S: WideString): WideString;

//------------------------------------------------------------------------------
{ PChar/PWideChar Unicode <-> UTF8 conversion }

// UnicodeToUTF8(3):
// UTF8ToUnicode(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.

function UnicodeToUtf8(Dest: PChar; Source: PWideChar;
  MaxBytes: Integer): Integer; overload;
function Utf8ToUnicode(Dest: PWideChar; Source: PChar;
  MaxChars: Integer): Integer; overload;

// UnicodeToUtf8(4):
// UTF8ToUnicode(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar;
  SourceChars: Cardinal): Cardinal; overload;
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar;
  SourceBytes: Cardinal): Cardinal; overload;

//------------------------------------------------------------------------------
{ WideString <-> UTF8 conversion }

function UTF8Encode(const WS: WideString): UTF8String;
function UTF8Decode(const S: UTF8String): WideString;

//------------------------------------------------------------------------------
{ Ansi <-> UTF8 conversion }

function AnsiToUtf8(const S: string): UTF8String;
function Utf8ToAnsi(const S: UTF8String): string;
{$EndIf D6}

//------------------------------------------------------------------------------
// Searches a position of the SubStr sub-string in the S string from the From
// position. Returns an integer value that is the index of the first character
// of SubStr within S. Case-sensitive. When SubStr is not found, function
// returns zero.
function abfPosExA(const SubStr, S: AnsiString; From: Integer = 1): Integer;
function abfPosEx(const SubStr, S: string; From: Integer = 1): Integer;
function abfPosExW(const SubStr, S: WideString; From: Integer = 1): Integer;

//------------------------------------------------------------------------------
// Searches a position of the SubStr sub-string in the S string backward from
// the From position. Returns an integer value that is the index of the first
// character of SubStr within S. Case-sensitive. When SubStr is not found,
// function returns zero.
function abfBackPosExA(const SubStr, S: AnsiString; From: Integer): Integer;
function abfBackPosEx(const SubStr, S: string; From: Integer): Integer;
function abfBackPosExW(const SubStr, S: WideString; From: Integer): Integer;

//------------------------------------------------------------------------------
// Searches for SubStr within S string and returns an integer value that is the
// index of the first character of Substr within S. Case-sensitive. When SubStr
// is not found, function returns zero.
function abfBackPosA(const SubStr, S: AnsiString): Integer;
function abfBackPos(const SubStr, S: string): Integer;
function abfBackPosW(const SubStr, S: WideString): Integer;

//------------------------------------------------------------------------------
// Returns the index value of the first character in a specified substring that
// occurs in the S string after the FromPos position. Ignores string cases.
function abfPosIgnoreCaseExA(const SubStr, S: AnsiString; FromPos: Integer): Integer;
function abfPosIgnoreCaseEx(const SubStr, S: string; FromPos: Integer): Integer;
function abfPosIgnoreCaseExW(const SubStr, S: WideString;
  FromPos: Integer): Integer;

//------------------------------------------------------------------------------
// Returns the index value of the first character in a specified substring that
// occurs in the S string. Ignores string cases.
function abfPosIgnoreCaseA(const SubStr, S: AnsiString): Integer;
function abfPosIgnoreCase(const SubStr, S: string): Integer;
function abfPosIgnoreCaseW(const SubStr, S: WideString): Integer;

//------------------------------------------------------------------------------
// Replaces the OldStr sub-string in the S string from the Index position to
// the NewStr sub-string.
procedure abfReplaceA(var S: AnsiString; Index: Integer;
  const OldStr, NewStr: AnsiString);
procedure abfReplace(var S: string; Index: Integer; const OldStr, NewStr: string);
procedure abfReplaceW(var S: WideString; Index: Integer;
  const OldStr, NewStr: WideString);

//------------------------------------------------------------------------------
// Replaces all occured OldStr sub-strings with NewStr sub-strings in the given
// S string. Set IgnoreCase True to ignore string cases.
procedure abfReplaceSubStringsExA(var S: AnsiString;
  const OldStr, NewStr: AnsiString; IgnoreCase: Boolean);
procedure abfReplaceSubStringsEx(var S: string; const OldStr, NewStr: string;
  IgnoreCase: Boolean);
procedure abfReplaceSubStringsExW(var S: WideString;
  const OldStr, NewStr: WideString; IgnoreCase: Boolean);

//------------------------------------------------------------------------------
// Replaces all occured OldStr sub-strings with NewStr sub-strings in the given
// S string.
procedure abfReplaceSubStringsA(var S: AnsiString;
  const OldStr, NewStr: AnsiString);
procedure abfReplaceSubStrings(var S: string; const OldStr, NewStr: string);
procedure abfReplaceSubStringsW(var S: WideString;
  const OldStr, NewStr: Widestring);

//------------------------------------------------------------------------------
// Replaces all occured OldChar with NewChar in the given S string
procedure abfReplaceCharsA(var S: AnsiString; OldChar, NewChar: AnsiChar);
procedure abfReplaceChars(var S: string; OldChar, NewChar: Char);
procedure abfReplaceCharsW(var S: WideString; OldChar, NewChar: WideChar);

//------------------------------------------------------------------------------
// Removes all chars contained in CharSet from the S string
function abfRemoveCharSetA(const CharSet: TabfAnsiCharSet;
  const S: AnsiString): AnsiString;
function abfRemoveCharSet(const CharSet: TabfCharSet; const S: string): string;
function abfRemoveCharSetW(const CharSet: TabfWideCharSet;
  const S: WideString): WideString;

//------------------------------------------------------------------------------
// Leaves in the S string only chars contained in CharSet
function abfLeaveCharSetA(const CharSet: TabfAnsiCharSet;
  const S: AnsiString): AnsiString;
function abfLeaveCharSet(const CharSet: TabfCharSet; const S: string): string;
function abfLeaveCharSetW(const CharSet: TabfWideCharSet;
  const S: WideString): WideString;

//------------------------------------------------------------------------------
// Adds the char to the end of the S string if it it ends with other char.
function abfAddCharA(const S: AnsiString; C: AnsiChar): AnsiString;
function abfAddChar(const S: string; C: Char): string;
function abfAddCharW(const S: WideString; C: WideChar): WideString;

//------------------------------------------------------------------------------
// Adds the C char at the beginning of the S string if it begins with other
// char.
function abfAddCharBeforeA(const S: AnsiString; C: AnsiChar): AnsiString;
function abfAddCharBefore(const S: string; C: Char): string;
function abfAddCharBeforeW(const S: WideString; C: WideChar): WideString;

//------------------------------------------------------------------------------
// Deletes a part of the S string after the C character occurred after the
// FromPos position.
procedure abfDeleteAfterCharExA(var S: AnsiString; C: AnsiChar; FromPos: Integer);
//procedure abfDeleteAfterCharEx(var S: string; C: Char; FromPos: Integer);
procedure abfDeleteAfterCharExW(var S: WideString; C: WideChar; FromPos: Integer);

//------------------------------------------------------------------------------
// Deletes a part of the S string before the C character occurred after the
// FromPos position.
procedure abfDeleteBeforeCharExA(var S: AnsiString; C: AnsiChar; FromPos: Integer);
//procedure abfDeleteBeforeCharEx(var S: string; C: Char; FromPos: Integer);
procedure abfDeleteBeforeCharExW(var S: WideString; C: WideChar; FromPos: Integer);

//------------------------------------------------------------------------------
// Deletes a part of the S string after the C char.
procedure abfDeleteAfterCharA(var S: AnsiString; C: AnsiChar);
//procedure abfDeleteAfterChar(var S: string; C: Char);
procedure abfDeleteAfterCharW(var S: WideString; C: WideChar);

//------------------------------------------------------------------------------
// Deletes a part of the S string before the C char.
procedure abfDeleteBeforeCharA(var S: AnsiString; C: AnsiChar);
procedure abfDeleteBeforeChar(var S: string; C: Char);
procedure abfDeleteBeforeCharW(var S: WideString; C: WideChar);

//------------------------------------------------------------------------------
// Generates a list of the strings from the S string and append it to the given
// List. Parts of string should be delimited with Separator char. The List
// should not be nil.
// Note: the List will not be cleared before appending!
procedure abfParseStringA(S: AnsiString; const Separator: AnsiString;
  List: TabfAnsiStrings);
procedure abfParseString(S: string; const Separator: string; List: TStrings);
{$IFDEF D9}
procedure abfParseStringW(S: WideString; const Separator: WideString;
  List: TWideStrings);
{$ENDIF}

//------------------------------------------------------------------------------
// Creates a string separated with Separator symbols from items of the given
// List. Inverse of the abfParseString function.
function abfUnParseStringA(List: TabfAnsiStrings;
  const Separator: AnsiString): AnsiString;
function abfUnParseString(List: TStrings; const Separator: string): string;
{$IFDEF D9}
function abfUnParseStringW(List: TWideStrings;
  const Separator: WideString): WideString;
{$ENDIF}

//------------------------------------------------------------------------------
// Generates a list of the strings from a given string. Parts of string should
// be delimited with Separator string. The result list is created in any way.
function abfStrToListA(const Str: AnsiString;
  const Separator: AnsiString): TabfAnsiStrings;
function abfStrToList(const Str: string; const Separator: string): TStrings;
{$IFDEF D9}
function abfStrToListW(const Str: WideString;
  const Separator: WideString): TWideStrings;
{$ENDIF}

//------------------------------------------------------------------------------
// Opposite to abfStrToList function. Generates a string from the given List.
function abfListToStrA(const List: TabfAnsiStrings;
  const Separator: AnsiString): AnsiString;
function abfListToStr(const List: TStrings; const Separator: string): string;
{$IFDEF D9}
function abfListToStrW(const List: TWideStrings;
  const Separator: WideString): WideString;
{$ENDIF}

//------------------------------------------------------------------------------
// Removes all strings equal to the S string from the string List. Non-case
// sensitive.
procedure abfRemoveStringFromListA(const S: AnsiString; List: TabfAnsiStrings);
procedure abfRemoveStringFromList(const S: string; List: TStrings);
{$IFDEF D9}
procedure abfRemoveStringFromListW(const S: WideString; List: TWideStrings);
{$ENDIF}

//------------------------------------------------------------------------------
// Peplaces all parameters in the S string by values taken from the Params list.
// Parameters in S string should be enclosed with Separator symbol
// (%AppPath% for example). Format of Partams list items is
// 'ParamName=ParamValue'.
function abfChangeParamsA(const S: AnsiString; Params: TabfAnsiStrings;
  Separator: AnsiChar): AnsiString;
function abfChangeParams(const S: string; Params: TStrings;
  Separator: Char): string;
{$IFDEF D9}
function abfChangeParamsW(const S: WideString; Params: TWideStrings;
  Separator: WideChar): WideString;
{$ENDIF}

//------------------------------------------------------------------------------
// Encloses the S string with C chars. If AllowDoubleEnclose is True already
// enclosed string will be enclosed again.
function abfEncloseStringExA(const S: AnsiString; C: AnsiChar;
  AllowDoubleEnclose: Boolean): AnsiString;
function abfEncloseStringEx(const S: string; C: Char;
  AllowDoubleEnclose: Boolean): string;
function abfEncloseStringExW(const S: WideString; C: WideChar;
  AllowDoubleEnclose: Boolean): WideString;

//------------------------------------------------------------------------------
// Encloses the S string with C chars. Already enclosed string will not be
// changed
function abfEncloseStringA(const S: AnsiString; C: AnsiChar): AnsiString;
function abfEncloseString(const S: string; C: Char): string;
function abfEncloseStringW(const S: WideString; C: WideChar): WideString;

//------------------------------------------------------------------------------
// Trims leading characters are present in CharSet from the S string.
function abfTrimLeftA(const S: AnsiString;
  const CharSet: TabfAnsiCharSet): AnsiString;
function abfTrimLeft(const S: string; const CharSet: TabfCharSet): string;
function abfTrimLeftW(const S: WideString;
  const CharSet: TabfWideCharSet): WideString;

//------------------------------------------------------------------------------
// Trims trailing characters are present in CharSet from the S string.
function abfTrimRightA(const S: AnsiString;
  const CharSet: TabfAnsiCharSet): AnsiString;
function abfTrimRight(const S: string; const CharSet: TabfCharSet): string;
function abfTrimRightW(const S: WideString;
  const CharSet: TabfWideCharSet): WideString;

//------------------------------------------------------------------------------
// Trims leadind and trailing characters are present in CharSet from the string.
function abfTrimA(const S: AnsiString;
  const CharSet: TabfAnsiCharSet): AnsiString;
function abfTrim(const S: string; const CharSet: TabfCharSet): string;
function abfTrimW(const S: WideString;
  const CharSet: TabfWideCharSet): WideString;


//==============================================================================
// Internet relative
//==============================================================================

//------------------------------------------------------------------------------
// Encodes given URL to escape sequences
function abfEscapeURL(const AURL: AnsiString): AnsiString;

//------------------------------------------------------------------------------
// Decodes given URL from escape sequences
function abfUnescapeURL(const AURL: AnsiString): UTF8String;


{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfConsts, abfSysUtils, SysUtils;

type
  TabfStrRec = record
    AllocSize: LongInt;
    RefConunt: LongInt;
    Length   : LongInt;
  end;

{$IfNDef D3}
//------------------------------------------------------------------------------
// LastDelimiter returns the byte index in S of the rightmost whole
// character that matches any character in Delimiters (except null (#0)).
// S may contain multibyte characters; Delimiters must contain only single
// byte non-null characters.
// Example: LastDelimiter('\.:', 'c:\filename.ext') returns 12.

function LastDelimiter(const Delimiters, S: AnsiString): Integer;
var
  P: PAnsiChar;
begin
  Result := Length(S);
  P := PAnsiChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then Exit;
    Dec(Result);
  end;
end;

{$EndIf D3}

{$IfNDef D6}
//------------------------------------------------------------------------------
// WideUpperCase returns S converted to upper case.
// The conversion uses the current locale.
function WideUpperCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  if IsWinNT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiUpperCase(S);
end;

//------------------------------------------------------------------------------
// WideLowerCase returns S converted to lower case.
// The conversion uses the current locale.
function WideLowerCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  if IsWinNT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharLowerBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiLowerCase(S);
end;

//------------------------------------------------------------------------------
// UnicodeToUTF8(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.

function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: Integer): Integer;
var
  len: Cardinal;
begin
  len := 0;
  if Source <> nil then
    while Source[len] <> #0 do
      Inc(len);
  Result := UnicodeToUtf8(Dest, MaxBytes, Source, len);
end;

//------------------------------------------------------------------------------
// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count+1] := Char($80 or ((c shr 6) and $3F));
        Dest[count+2] := Char($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count+1] := Char($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

//------------------------------------------------------------------------------

function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: Integer): Integer;
var
  len: Cardinal;
begin
  len := 0;
  if Source <> nil then
    while Source[len] <> #0 do
      Inc(len);
  Result := Utf8ToUnicode(Dest, MaxChars, Source, len);
end;

//------------------------------------------------------------------------------

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

//------------------------------------------------------------------------------

function Utf8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

//------------------------------------------------------------------------------

function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

//------------------------------------------------------------------------------

function AnsiToUtf8(const S: string): UTF8String;
begin
  Result := Utf8Encode(S);
end;

//------------------------------------------------------------------------------

function Utf8ToAnsi(const S: UTF8String): string;
begin
  Result := Utf8Decode(S);
end;

{$EndIf D6}

//------------------------------------------------------------------------------
// Searches a position of the SubStr sub-string in the S string from the From
// position. Returns an integer value that is the index of the first character
// of SubStr within S. Case-sensitive. When SubStr is not found, function
// returns zero.
// Date: 05/30/2007


(* ***** BEGIN LICENSE BLOCK *****
 * The Original Code is Fastcode
 *
 * The Initial Developer of the Original Code is Fastcode
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 * Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)

//Slightly modified copy of Fastcode function PosEx_JOH_IA32_8
function abfPosExA(const SubStr, S: AnsiString; From: Integer = 1): Integer;
(*
asm {299 Bytes}
  sub     esp, 20
  mov     [esp], ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  sub     ecx, 1           {From - 1}
  or      ebx, ecx         {Negative if S = '' or SubStr = '' or From < 1}
  jl      @@InvalidInput
  mov     [esp+4], edi
  mov     [esp+8], esi
  mov     [esp+12], ebp
  mov     [esp+16], edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  add     ecx, edi
  cmp     ecx, esi
  jg      @@NotFound       {From to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  lea     ebp, [eax+edi]   {Last Character Position in SubStr + 1}
  add     esi, edx         {Last Character Position in S}
  movzx   eax, byte ptr [ebp-1]     {Last Character of SubStr}
  add     edx, ecx         {Search Start Position in S for Last Character}
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-3] {Address of First Match on Last Character + 1}
@@Compare:
  cmp     edi, -4
  jle     @@Large          {Lenght(SubStr) >= 4}
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  mov     ax, [ebp+edi]    {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edx+edi]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  mov     ebx, [esp]
  mov     edi, [esp+4]
  mov     esi, [esp+8]
  mov     ebp, [esp+12]
  sub     eax, [esp+16]
  add     esp, 20
  ret
@@NotFound:
  mov     edi, [esp+4]
  mov     esi, [esp+8]
  mov     ebp, [esp+12]
@@InvalidInput:
  mov     ebx, [esp]
  add     esp, 20
  xor     eax, eax         {Return 0}
  ret
@@Remainder:               {Check Last 1 to 4 Characters}
  mov     eax, [esi-3]     {Last 4 Characters of S - May include Length Bytes}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@NotFound       {No Match Possible}
  lea     eax, [edx-4]     {Check Valid Match Positions}
  cmp     cl, [eax]
  lea     edx, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+2]
  cmp     cl, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+3]
  cmp     cl, [eax+2]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+4]
  jmp     @@Compare
@@Large:
  mov     eax, [ebp-4]     {Compare Last 4 Characters of S and SubStr}
  cmp     eax, [edx-4]
  jne     @@MainLoop       {No Match on Last 4 Characters}
  mov     ebx, edi
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [ebp+ebx-4]
  cmp     eax, [edx+ebx-4]
  je      @@CompareLoop    {Match on Next 4 Characters}
  jmp     @@MainLoop       {No Match}
*)
begin
  Result := pos(SubStr, S, From);
end;

//------------------------------------------------------------------------------

function abfPosEx(const SubStr, S: string; From: Integer = 1): Integer;
begin
  Result := abfPosExA(SubStr, S, From);
end;

//------------------------------------------------------------------------------

//Modified copy of Fastcode function PosEx_JOH_IA32_8
function abfPosExW(const SubStr, S: WideString; From: Integer = 1): Integer;
begin
  Result := Pos(SubStr, S, From);
end;

//------------------------------------------------------------------------------
// Searches a position of the SubStr sub-string in the S string backward from
// the From position. Returns an integer value that is the index of the first
// character of SubStr within S. Case-sensitive. When SubStr is not found,
// function returns zero.
// Date: 09/22/2000
// Unicode version added 05/30/2007

function abfBackPosExA(const SubStr, S: AnsiString; From: Integer): Integer;
var
  i, Len, SubLen: Integer;
begin
  Result := 0;
// Check parameters
  if S = '' then Exit;
  Len := Length(S);
  SubLen := Length(SubStr);
  if Len < SubLen then Exit;
// Start searching
  for i := (Min(From, Len) - SubLen + 1) downto 1 do
    if S[i] = SubStr[1] then
      if Copy(S, i, SubLen) = SubStr then
      begin
        Result := i;
        Exit;
      end;
end;

//------------------------------------------------------------------------------

function abfBackPosEx(const SubStr, S: string; From: Integer): Integer;
begin
  Result := abfBackPosExA(SubStr, S, From);
end;

//------------------------------------------------------------------------------

function abfBackPosExW(const SubStr, S: WideString; From: Integer): Integer;
var
  i, Len, SubLen: Integer;
begin
  Result := 0;
// Check parameters
  if S = '' then Exit;
  Len := Length(S);
  SubLen := Length(SubStr);
  if Len < SubLen then Exit;
// Start searching
  for i := (Min(From, Len) - SubLen + 1) downto 1 do
    if S[i] = SubStr[1] then
      if Copy(S, i, SubLen) = SubStr then
      begin
        Result := i;
        Exit;
      end;
end;

//------------------------------------------------------------------------------
// Searches for SubStr within S string and returns an integer value that is the
// index of the first character of Substr within S string backward.
// Case-sensitive. When SubStr is not found, function returns zero.
// Date: 09/22/2000
// Unicode version added 05/30/2007

function abfBackPosA(const SubStr, S: AnsiString): Integer;
begin
  Result := abfBackPosExA(SubStr, S, Length(S));
end;

//------------------------------------------------------------------------------

function abfBackPos(const SubStr, S: string): Integer;
begin
  Result := abfBackPosA(SubStr, S);
end;

//------------------------------------------------------------------------------

function abfBackPosW(const SubStr, S: WideString): Integer;
begin
  Result := abfBackPosExW(SubStr, S, Length(S));
end;

//------------------------------------------------------------------------------
// Returns the index value of the first character in a specified substring that
// occurs in the S string after the FromPos position. Ignores string cases.
// Date: 02/23/2000
// Unicode version added 05/30/2007

function abfPosIgnoreCaseExA(const SubStr, S: AnsiString; FromPos: Integer): Integer;
begin
  Result := abfPosExA(AnsiUpperCase(SubStr), AnsiUpperCase(S), FromPos);
end;

//------------------------------------------------------------------------------

function abfPosIgnoreCaseEx(const SubStr, S: string; FromPos: Integer): Integer;
begin
  Result := abfPosIgnoreCaseExA(SubStr, S, FromPos);
end;

//------------------------------------------------------------------------------

function abfPosIgnoreCaseExW(const SubStr, S: WideString;
  FromPos: Integer): Integer;
begin
  Result := abfPosExW(WideUpperCase(SubStr), WideUpperCase(S), FromPos);
end;

//------------------------------------------------------------------------------
// Returns the index value of the first character in a specified substring that
// occurs in the S string. Ignores string cases.
// Date: 02/23/2000
// Unicode version added 05/30/2007

function abfPosIgnoreCaseA(const SubStr, S: AnsiString): Integer;
begin
  Result := Pos(AnsiUpperCase(SubStr), AnsiUpperCase(S));
end;

//------------------------------------------------------------------------------

function abfPosIgnoreCase(const SubStr, S: string): Integer;
begin
  Result := abfPosIgnoreCaseA(SubStr,S);
end;

//------------------------------------------------------------------------------

function abfPosIgnoreCaseW(const SubStr, S: WideString): Integer;
begin
  Result := Pos(WideUpperCase(SubStr), WideUpperCase(S));
end;

//------------------------------------------------------------------------------
// Replaces the OldStr sub-string in the S string from the Index position to
// the NewStr sub-string.
// Date: 02/23/2000
// Unicode version added 05/30/2007

procedure abfReplaceA(var S: AnsiString; Index: Integer;
  const OldStr, NewStr: AnsiString);
begin
  Delete(S, Index, Length(OldStr));
  Insert(NewStr, S, Index);
end;

//------------------------------------------------------------------------------

procedure abfReplace(var S: string; Index: Integer; const OldStr, NewStr: string);
begin
  Delete(S, Index, Length(OldStr));
  Insert(NewStr, S, Index);

  //abfReplaceA(S, Index, OldStr, NewStr);
end;

//------------------------------------------------------------------------------

procedure abfReplaceW(var S: WideString; Index: Integer; const OldStr, NewStr: WideString);
begin
  Delete(S, Index, Length(OldStr));
  Insert(NewStr, S, Index);
end;

//------------------------------------------------------------------------------
// Replaces all occured OldStr sub-strings with NewStr sub-strings in the given
// S string. Set IgnoreCase True to ignore string cases.
// Date: 03/05/2001
// Unicode version added 05/30/2007

procedure abfReplaceSubStringsExA(var S: AnsiString;
  const OldStr, NewStr: AnsiString; IgnoreCase: Boolean);
var
  PosChar, Len: Integer;
  SUp, OldStrUp: AnsiString;
begin
  if not IgnoreCase then
  begin
    abfReplaceSubStringsA(S, OldStr, NewStr);
    Exit;
  end;

  SUp := AnsiUpperCase(S);
  OldStrUp := AnsiUpperCase(OldStr);
  PosChar := 1;
  Len := Length(NewStr);
  repeat
    PosChar := abfPosExA(OldStrUp, SUp, PosChar);
    if PosChar = 0 then Break;
    abfReplaceA(S  , PosChar, OldStr, NewStr);
    abfReplaceA(SUp, PosChar, OldStr, NewStr);
    Inc(PosChar, Len);
  until False;
end;

//------------------------------------------------------------------------------

procedure abfReplaceSubStringsEx(var S: string; const OldStr, NewStr: string;
  IgnoreCase: Boolean);
var
  PosChar, Len: Integer;
  SUp, OldStrUp: String;
begin
  if not IgnoreCase then
  begin
    abfReplaceSubStrings(S, OldStr, NewStr);
    Exit;
  end;

  SUp := AnsiUpperCase(S);
  OldStrUp := AnsiUpperCase(OldStr);
  PosChar := 1;
  Len := Length(NewStr);
  repeat
    PosChar := abfPosExA(OldStrUp, SUp, PosChar);
    if PosChar = 0 then Break;
    abfReplace(S  , PosChar, OldStr, NewStr);
    abfReplace(SUp, PosChar, OldStr, NewStr);
    Inc(PosChar, Len);
  until False;
end;

//------------------------------------------------------------------------------

procedure abfReplaceSubStringsExW(var S: WideString;
  const OldStr, NewStr: WideString; IgnoreCase: Boolean);
var
  PosChar, Len: Integer;
  SUp, OldStrUp: WideString;
begin
  if not IgnoreCase then
  begin
    abfReplaceSubStringsW(S, OldStr, NewStr);
    Exit;
  end;

  SUp := WideUpperCase(S);
  OldStrUp := WideUpperCase(OldStr);
  PosChar := 1;
  Len := Length(NewStr);
  repeat
    PosChar := abfPosExW(OldStrUp, SUp, PosChar);
    if PosChar = 0 then Break;
    abfReplaceW(S  , PosChar, OldStr, NewStr);
    abfReplaceW(SUp, PosChar, OldStr, NewStr);
    Inc(PosChar, Len);
  until False;
end;

//------------------------------------------------------------------------------
// Replaces all occured OldStr sub-strings with NewStr sub-strings in the given
// S string.
// Date: 02/23/2000
// Unicode version added 05/30/2007

procedure abfReplaceSubStringsA(var S: AnsiString;
  const OldStr, NewStr: AnsiString);
var
  PosChar, Len: Integer;
begin
  PosChar := 1;
  Len := Length(NewStr);
  repeat
    PosChar := abfPosExA(OldStr, S, PosChar);
    if PosChar = 0 then Break;
    abfReplaceA(S, PosChar, OldStr, NewStr);
    Inc(PosChar, Len);
  until False;
end;

//------------------------------------------------------------------------------

procedure abfReplaceSubStrings(var S: string; const OldStr, NewStr: string);
var
  PosChar, Len: Integer;
begin
  PosChar := 1;
  Len := Length(NewStr);
  repeat
    PosChar := abfPosExA(OldStr, S, PosChar);
    if PosChar = 0 then Break;
    abfReplace(S, PosChar, OldStr, NewStr);
    Inc(PosChar, Len);
  until False;
end;

//------------------------------------------------------------------------------

procedure abfReplaceSubStringsW(var S: WideString;
  const OldStr, NewStr: WideString);
var
  PosChar, Len: Integer;
begin
  PosChar := 1;
  Len := Length(NewStr);
  repeat
    PosChar := abfPosExW(OldStr, S, PosChar);
    if PosChar = 0 then Break;
    abfReplaceW(S, PosChar, OldStr, NewStr);
    Inc(PosChar, Len);
  until False;
end;

//------------------------------------------------------------------------------
// Replaces all occured OldChar with NewChar in the given S string
// Date: 02/23/2000
// Unicode version added 05/30/2007

procedure abfReplaceCharsA(var S: AnsiString; OldChar, NewChar: AnsiChar);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = OldChar then S[i] := NewChar;
end;

//------------------------------------------------------------------------------

procedure abfReplaceChars(var S: string; OldChar, NewChar: Char);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = OldChar then S[i] := NewChar;
end;

//------------------------------------------------------------------------------

procedure abfReplaceCharsW(var S: WideString; OldChar, NewChar: WideChar);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = OldChar then S[i] := NewChar;
end;


//------------------------------------------------------------------------------
// Removes all chars contained in CharSet from the S string
// Date: 02/23/2000
// Unicode version added 05/30/2007

function abfRemoveCharSetA(const CharSet: TabfAnsiCharSet;
  const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 1 do
    if Result[i] in CharSet then Delete(Result, i, 1);
end;

//------------------------------------------------------------------------------

function abfRemoveCharSet(const CharSet: TabfCharSet; const S: string): string;
begin
  Result := abfRemoveCharSetA(CharSet, S);
end;

//------------------------------------------------------------------------------

function abfRemoveCharSetW(const CharSet: TabfWideCharSet;
  const S: WideString): WideString;
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 1 do
    if (Pos(Result[i], CharSet) > 0) then Delete(Result, i, 1);
end;

//------------------------------------------------------------------------------
// Leaves in the S string only chars contained in CharSet
// Date: 02/23/2000
// Unicode version added 05/30/2007

function abfLeaveCharSetA(const CharSet: TabfAnsiCharSet;
  const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 1 do
    if not (Result[i] in CharSet) then Delete(Result, i, 1);
end;

//------------------------------------------------------------------------------

function abfLeaveCharSet(const CharSet: TabfCharSet; const S: string): string;
begin
  Result := abfLeaveCharSetA(CharSet, S);
end;

//------------------------------------------------------------------------------

function abfLeaveCharSetW(const CharSet: TabfWideCharSet;
  const S: WideString): WideString;
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 1 do
    if (Pos(Result[i], CharSet) = 0) then Delete(Result, i, 1);
end;

//------------------------------------------------------------------------------
// Adds the C char to the end of the S string if it it ends with other char.
// Date: 02/23/2000
// Unicode version added 05/30/2007

function abfAddCharA(const S: AnsiString; C: AnsiChar): AnsiString;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[Length(Result)] <> C) then
    Result := Result + C;
end;

//------------------------------------------------------------------------------

function abfAddChar(const S: string; C: Char): string;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[Length(Result)] <> C) then
    Result := Result + C;
end;

//------------------------------------------------------------------------------

function abfAddCharW(const S: WideString; C: WideChar): WideString;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[Length(Result)] <> C) then
    Result := Result + C;
end;

//------------------------------------------------------------------------------
// Adds the C char at the beginning of the S string if it begins with other
// char.
// Date: 02/23/2000
// Unicode version added 05/30/2007

function abfAddCharBeforeA(const S: AnsiString; C: AnsiChar): AnsiString;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[1] <> C) then Result := C + Result;
end;

//------------------------------------------------------------------------------

function abfAddCharBefore(const S: string; C: Char): string;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[1] <> C) then Result := C + Result;
end;

//------------------------------------------------------------------------------

function abfAddCharBeforeW(const S: WideString; C: WideChar): WideString;
begin
  Result := S;
  if (Length(Result) > 0) and (Result[1] <> C) then Result := C + Result;
end;

//------------------------------------------------------------------------------
// Deletes a part of the S string after the C character occurred after the
// FromPos position.
// Date: 02/23/2000
// Unicode version added 05/30/2007

procedure abfDeleteAfterCharExA(var S: AnsiString; C: AnsiChar; FromPos: Integer);
var
  APos: Integer;
begin
  APos := abfPosExA(C, S, FromPos);
  if APos > 0 then Delete(S, APos, MaxInt);
end;

//------------------------------------------------------------------------------

procedure abfDeleteAfterCharEx(var S: string; C: Char; FromPos: Integer);
var
  APos: Integer;
begin
  APos := abfPosExA(C, S, FromPos);
  if APos > 0 then Delete(S, APos, MaxInt);
end;

//------------------------------------------------------------------------------

procedure abfDeleteAfterCharExW(var S: WideString; C: WideChar; FromPos: Integer);
var
  APos: Integer;
begin
  APos := abfPosExW(C, S, FromPos);
  if APos > 0 then Delete(S, APos, MaxInt);
end;

//------------------------------------------------------------------------------
// Deletes a part of the S string before the C character occurred after the
// FromPos position.
// Date: 02/23/2000
// Unicode version added 05/30/2007

procedure abfDeleteBeforeCharExA(var S: AnsiString; C: AnsiChar; FromPos: Integer);
var
  APos: Integer;
begin
  APos := abfPosExA(C, S, FromPos);
  if APos > 0 then Delete(S, 1, APos);
end;

//------------------------------------------------------------------------------

procedure abfDeleteBeforeCharEx(var S: string; C: Char; FromPos: Integer);
var
  APos: Integer;
begin
  APos := abfPosEx(C, S, FromPos);
  if APos > 0 then Delete(S, 1, APos);
end;

//------------------------------------------------------------------------------

procedure abfDeleteBeforeCharExW(var S: WideString; C: WideChar; FromPos: Integer);
var
  APos: Integer;
begin
  APos := abfPosExW(C, S, FromPos);
  if APos > 0 then Delete(S, 1, APos);
end;

//------------------------------------------------------------------------------
// Deletes a part of the S string after the C char.
// Date: 02/23/2000
// Unicode version added 05/30/2007

procedure abfDeleteAfterCharA(var S: AnsiString; C: AnsiChar);
begin
  abfDeleteAfterCharExA(S, C, 1);
end;

//------------------------------------------------------------------------------

{procedure abfDeleteAfterChar(var S: string; C: Char);
begin
  abfDeleteAfterCharW(S, C);
end;}

//------------------------------------------------------------------------------

procedure abfDeleteAfterCharW(var S: WideString; C: WideChar);
begin
  abfDeleteAfterCharExW(S, C, 1);
end;

//------------------------------------------------------------------------------
// Deletes a part of the S string before the C char.
// Date: 02/23/2000
// Unicode version added 05/30/2007

procedure abfDeleteBeforeCharA(var S: AnsiString; C: AnsiChar);
begin
  abfDeleteBeforeCharExA(S, C, 1);
end;

//------------------------------------------------------------------------------

procedure abfDeleteBeforeChar(var S: string; C: Char);
begin
  abfDeleteBeforeChar(S, C);
end;

//------------------------------------------------------------------------------

procedure abfDeleteBeforeCharW(var S: WideString; C: WideChar);
begin
  abfDeleteBeforeCharExW(S, C, 1);
end;

//------------------------------------------------------------------------------
// Generates a list of the strings from the S string and append it to the given
// List. Parts of string should be delimited with Separator string. The List
// should not be nil.
// Note: the List will not be cleared before appending!
// Date: 12/11/2001
// Unicode version added 06/02/2007

procedure abfParseStringA(S: AnsiString; const Separator: AnsiString;
  List: TabfAnsiStrings);
var
  Lst: TabfAnsiStrings;
begin
  Lst := TabfAnsiStringList.Create;
  with Lst do
  try
    if Length(Separator) = 1 then
      abfReplaceCharsA(S, Separator[1], #10)
    else
      abfReplaceSubStringsA(S, Separator, #10);
    Text := S;
    List.AddStrings(Lst);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfParseString(S: string; const Separator: string; List: TStrings);
begin
  abfParseStringA(S, Separator, List);
end;

//------------------------------------------------------------------------------

{$IFDEF D9}
procedure abfParseStringW(S: WideString; const Separator: WideString;
  List: TWideStrings);
var
  Lst: TWideStrings;
begin
  Lst := TWideStringList.Create;
  with Lst do
  try
    if Length(Separator) = 1 then
      abfReplaceCharsW(S, Separator[1], WideChar(#10))
    else
      abfReplaceSubStringsW(S, Separator, WideChar(#10));
    Text := S;
    List.AddStrings(Lst);
  finally
    Free;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Creates a string separated with Separator symbols from items of the given
// List. Opposite to the abfParseString function.
// Date: 12/11/2001
// Unicode version added 06/02/2007

function abfUnParseStringA(List: TabfAnsiStrings;
  const Separator: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(List) or (List.Count < 1) then Exit;
  Result := List[List.Count - 1];
  for i := List.Count - 2 downto 0 do
    Result := List[i] + Separator + Result;
end;

//------------------------------------------------------------------------------

function abfUnParseString(List: TStrings; const Separator: string): string;
begin
  Result := abfUnParseStringA(List, Separator);
end;

//------------------------------------------------------------------------------

{$IFDEF D9}
function abfUnParseStringW(List: TWideStrings;
  const Separator: WideString): Widestring;
var
  i: Integer;
begin         
  Result := '';
  if not Assigned(List) or (List.Count < 1) then Exit;
  Result := List[List.Count - 1];
  for i := List.Count - 2 downto 0 do
    Result := List[i] + Separator + Result;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Generates a list of the strings from a given string. Parts of string should
// be delimited with Separator string. The result list is created in any way.
// Date: 01/14/2005
// Unicode version added 06/02/2007

function abfStrToListA(const Str: AnsiString;
  const Separator: AnsiString): TabfAnsiStrings;
var
  S: AnsiString;
begin
  Result := TabfAnsiStringList.Create;
  S := Str;

  if Length(Separator) = 1 then
  begin
    if Separator[1] <> CR then
      abfReplaceCharsA(S, Separator[1], CR);
  end else
    abfReplaceSubStringsA(S, Separator, CR);

  Result.Text := S;
end;

//------------------------------------------------------------------------------

function abfStrToList(const Str: string; const Separator: string): TStrings;
begin
  Result := abfStrToListA(Str, Separator);
end;

//------------------------------------------------------------------------------

{$IFDEF D9}
function abfStrToListW(const Str: WideString;
  const Separator: WideString): TWideStrings;
var
  S: WideString;
begin
  Result := TWideStringList.Create;
  S := Str;

  if Length(Separator) = 1 then
  begin
    if Separator[1] <> CR then
      abfReplaceCharsW(S, Separator[1], WideChar(CR));
  end else
    abfReplaceSubStringsW(S, Separator, WideChar(CR));

  Result.Text := S;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Opposite to abfStrToList function. Generates a string from the given List.
// Date: 01/14/2005
// Unicode version added 06/02/2007

function abfListToStrA(const List: TabfAnsiStrings;
  const Separator: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(List) or (List.Count < 1) then Exit;

  Result := List[List.Count - 1];
  for i := List.Count - 2 downto 0 do
    Result := List[i] + Separator + Result;
end;

//------------------------------------------------------------------------------

function abfListToStr(const List: TStrings; const Separator: string): string;
begin
  Result := abfListToStrA(List, Separator);
end;

//------------------------------------------------------------------------------

{$IFDEF D9}
function abfListToStrW(const List: TWideStrings;
  const Separator: WideString): WideString;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(List) or (List.Count < 1) then Exit;

  Result := List[List.Count - 1];
  for i := List.Count - 2 downto 0 do
    Result := List[i] + Separator + Result;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Removes all strings equal to the S string from the string List. Non-case
// sensitive.
// Unicode version added 06/02/2007

procedure abfRemoveStringFromListA(const S: AnsiString; List: TabfAnsiStrings);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;
  for i := List.Count - 1 downto 0 do
    if AnsiCompareText(List[i], S) = 0 then List.Delete(i);
end;

//------------------------------------------------------------------------------

procedure abfRemoveStringFromList(const S: string; List: TStrings);
begin
  abfRemoveStringFromListA(S, List);
end;

//------------------------------------------------------------------------------

{$IFDEF D9}
procedure abfRemoveStringFromListW(const S: WideString; List: TWideStrings);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;
  for i := List.Count - 1 downto 0 do
    if WideCompareText(List[i], S) = 0 then List.Delete(i);
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Peplaces all parameters in the S string by values taken from the Params list.
// Parameters in S string should be enclosed with Separator symbol
// (%AppPath% for example). Format of Partams list items is
// 'ParamName=ParamValue'.
// Date: 02/23/2000
// Unicode version added 06/02/2007

function abfChangeParamsA(const S: AnsiString; Params: TabfAnsiStrings;
  Separator: AnsiChar): AnsiString;
var
  i: Integer;
  OldStr, NewStr: AnsiString;
begin
  Result := S;
  if S = '' then Exit;
  with Params do
    for i := 0 to Count-1 do
    begin
      OldStr := Separator + Trim(Names[i]) + Separator;
      NewStr := Trim(Values[Names[i]]);
      abfReplaceSubStringsExA(Result, OldStr, NewStr, True);
    end;
end;

//------------------------------------------------------------------------------

function abfChangeParams(const S: string; Params: TStrings;
  Separator: Char): string;
begin
  Result := abfChangeParams(S, Params, Separator);
end;

//------------------------------------------------------------------------------

{$IFDEF D9}
function abfChangeParamsW(const S: WideString; Params: TWideStrings;
  Separator: WideChar): WideString;
var
  i: Integer;
  OldStr, NewStr: WideString;
begin
  Result := S;
  if S = '' then Exit;
  with Params do
    for i := 0 to Count - 1 do
    begin
      OldStr := Separator + Trim(Names[i]) + Separator;
      NewStr := Trim(Values[Names[i]]);
      abfReplaceSubStringsExW(Result, OldStr, NewStr, True);
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Encloses the S string with C chars. If AllowDoubleEnclose is True already
// enclosed string will be enclosed again.
// Date: 07/13/2001
// Unicode version added 05/30/2007

function abfEncloseStringExA(const S: AnsiString; C: AnsiChar;
  AllowDoubleEnclose: Boolean): AnsiString;
var
  Len: Integer;
begin
  Result := S;

  Len := Length(Result);
  if Len < 1 then
  begin
    Result := AnsiString(C) + AnsiString(C);
    Exit;
  end;

  if (not AllowDoubleEnclose) and (Len > 1) and
    (Result[1] = C) and (Result[Len] = C) then Exit;
// Begin of string
  if Result[1] <> C then
    Result := C + Result;
// End of string
  Len := Length(Result);
  if Result[Len] <> C then
    Result := Result + C;
end;

//------------------------------------------------------------------------------

function abfEncloseStringEx(const S: string; C: Char;
  AllowDoubleEnclose: Boolean): string;
begin
  Result := abfEncloseStringEx(S, C, AllowDoubleEnclose);
end;

//------------------------------------------------------------------------------

function abfEncloseStringExW(const S: WideString; C: WideChar;
  AllowDoubleEnclose: Boolean): WideString;
var
  Len: Integer;
begin
  Result := S;

  Len := Length(Result);
  if Len < 1 then
  begin
    Result := WideString(C) + WideString(C);
    Exit;
  end;

  if (not AllowDoubleEnclose) and (Len > 1) and
    (Result[1] = C) and (Result[Len] = C) then Exit;
// Begin of string
  if Result[1] <> C then
    Result := C + Result;
// End of string
  Len := Length(Result);
  if Result[Len] <> C then
    Result := Result + C;
end;

//------------------------------------------------------------------------------
// Encloses the S string with C chars. Already enclosed string will not be
// changed
// Date: 03/14/2001
// Unicode version added 05/30/2007

function abfEncloseStringA(const S: AnsiString; C: AnsiChar): AnsiString;
begin
  Result := abfEncloseStringExA(S, C, False);
end;

//------------------------------------------------------------------------------

function abfEncloseString(const S: string; C: Char): string;
begin
  Result := abfEncloseString(S, C);
end;

//------------------------------------------------------------------------------

function abfEncloseStringW(const S: WideString; C: WideChar): WideString;
begin
  Result := abfEncloseStringExW(S, C, False);
end;

//------------------------------------------------------------------------------
// Trims leading characters are present in CharSet from the S string.
// Date: 04/23/2001
// Unicode version added 05/30/2007

function abfTrimLeftA(const S: AnsiString;
  const CharSet: TabfAnsiCharSet): AnsiString;
var
  i, Len: Integer;
begin
  Len := Length(S);
  i := 1;
  while (i <= Len) and (S[i] in CharSet) do
    Inc(i);
  Result := Copy(S, i, MaxInt);
end;

//------------------------------------------------------------------------------

function abfTrimLeft(const S: string; const CharSet: TabfCharSet): string;
begin
  Result := abfTrimLeftA(S, CharSet);
end;

//------------------------------------------------------------------------------

function abfTrimLeftW(const S: WideString; const CharSet: TabfWideCharSet): WideString;
var
  i, Len: Integer;
begin
  Len := Length(S);
  i := 1;
  while (i <= Len) and (Pos(S[i], CharSet) > 0) do
    Inc(i);

  Result := Copy(S, i, MaxInt);
end;

//------------------------------------------------------------------------------
// Trims trailing characters are present in CharSet from the S string.
// Date: 04/23/2001
// Unicode version added 05/30/2007

function abfTrimRightA(const S: AnsiString;
  const CharSet: TabfAnsiCharSet): AnsiString;
var
  i: Integer;
begin
  i := Length(S);
  while (i > 0) and (S[i] in CharSet) do
    Dec(i);
  Result := Copy(S, 1, i);
end;

//------------------------------------------------------------------------------

function abfTrimRight(const S: string; const CharSet: TabfCharSet): string;
begin
  Result := abfTrimRightA(S, CharSet);
end;

//------------------------------------------------------------------------------

function abfTrimRightW(const S: WideString;
  const CharSet: TabfWideCharSet): Widestring;
var
  i: Integer;
begin
  i := Length(S);
  while (i > 0) and (Pos(S[i], CharSet) > 0) do
    Dec(i);
  Result := Copy(S, 1, i);
end;

//------------------------------------------------------------------------------
// Trims leadind and trailing characters are present in CharSet from the string.
// Date: 04/23/2001
// Unicode version added 05/30/2007

function abfTrimA(const S: AnsiString;
  const CharSet: TabfAnsiCharSet): AnsiString;
begin
  Result := abfTrimRightA(abfTrimLeftA(S, CharSet), CharSet);
end;

//------------------------------------------------------------------------------

function abfTrim(const S: string; const CharSet: TabfCharSet): string;
begin
  Result := abfTrimA(S, CharSet);
end;

//------------------------------------------------------------------------------

function abfTrimW(const S: WideString;
  const CharSet: TabfWideCharSet): WideString;
begin
  Result := abfTrimRightW(abfTrimLeftW(S, CharSet), CharSet);
end;


//==============================================================================
// Internet relative
//==============================================================================

//------------------------------------------------------------------------------
// Encodes given URL to escape sequences

function abfEscapeURL(const AURL: AnsiString): AnsiString;
// The NoConversion set contains characters as specificed in RFC 1738 and
// should not be modified unless the standard changes.
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PAnsiChar;
begin
  SetLength(Result, Length(AURL) * 3);
  Sp := PAnsiChar(AURL);
  Rp := PAnsiChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      if Sp^ = ' ' then
        Rp^ := '+'
      else
      begin
        FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PAnsiChar(Result));
end;

//------------------------------------------------------------------------------
// Decodes given URL from escape sequences

function abfUnescapeURL(const AURL: AnsiString): UTF8String;
var
  Sp: PAnsiChar;
  S: AnsiString;
  i: Integer;
  TempWS: WideString;
  IsMSUnicode: Boolean;
begin
  Result := '';
  IsMSUnicode := False;
  Sp := PAnsiChar(AURL);
  try
    TempWS := '';
    while Sp^ <> #0 do
    begin
      case Sp^ of
        '+': TempWS := TempWS + ' ';
        '%': begin
               // Look for an escaped % (%%) or %<hex> encoded character
               // Unicode characters are stored using the %uXXXX format.
               Inc(Sp);
               if Sp^ = '%' then
                 TempWS := TempWS + '%'
               else
               if LowerCase(Sp^) = 'u' then
               begin
                 IsMSUnicode := True;
                 Inc(Sp);

                 S := '';
                 for i := 0 to 3 do
                 begin
                   if Sp^ = #0 then Exit;
                   S := S + Sp^;
                   if i < 3 then Inc(Sp);
                 end;
                 TempWS := TempWS + WideChar(StrToInt('$' + S));
               end else
               begin
                 S := '';
                 for i := 0 to 1 do
                 begin
                   if Sp^ = #0 then Exit;
                   S := S + Sp^;
                   if i < 1 then Inc(Sp);
                 end;
                 TempWS := TempWS + AnsiChar(StrToInt('$' + S));
               end;
             end;
      else
        TempWS := TempWS + Sp^;
      end;
      Inc(Sp);
    end;

    if IsMSUnicode then
      Result := UTF8Encode(TempWS)
    else
      Result := TempWS;
  except
  end;
end;

//------------------------------------------------------------------------------

end.
