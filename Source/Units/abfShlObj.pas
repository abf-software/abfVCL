{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfShlObj;

{$I abf.inc}

interface

uses
  Windows;

//==============================================================================
// Misc types
//==============================================================================

type
{ TSHItemID -- Item ID }
  PSHItemID = ^TSHItemID;
  {$EXTERNALSYM _SHITEMID}
  _SHITEMID = record
    cb: Word;                         { Size of the ID (including cb itself) }
    abID: array[0..0] of Byte;        { The item ID (variable length) }
  end;
  TSHItemID = _SHITEMID;
  {$EXTERNALSYM SHITEMID}
  SHITEMID = _SHITEMID;


{ TItemIDList -- List if item IDs (combined with 0-terminator) }
  PItemIDList = ^TItemIDList;
  {$EXTERNALSYM _ITEMIDLIST}
  _ITEMIDLIST = record
     mkid: TSHItemID;
   end;
  TItemIDList = _ITEMIDLIST;
  {$EXTERNALSYM ITEMIDLIST}
  ITEMIDLIST = _ITEMIDLIST;


//==============================================================================
// Misc functions
//==============================================================================

function SHGetPathFromIDListA(pidl: PItemIDList;
  pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM SHGetPathFromIDListA}
function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM SHGetPathFromIDList}
function SHGetPathFromIDListW(pidl: PItemIDList;
  pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM SHGetPathFromIDListW}

{$EXTERNALSYM SHGetSpecialFolderLocation}
function SHGetSpecialFolderLocation(hwndOwner: HWND; nFolder: Integer;
  var ppidl: PItemIDList): HResult; stdcall;

function SHGetSpecialFolderPathA(hwndOwner: HWND; lpszPath: PAnsiChar;
  nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHGetSpecialFolderPathA}
function SHGetSpecialFolderPath(hwndOwner: HWND; lpszPath: PChar;
  nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHGetSpecialFolderPath}
function SHGetSpecialFolderPathW(hwndOwner: HWND; lpszPath: PWideChar;
  nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHGetSpecialFolderPathW}


{******************************************************************************}
implementation
{******************************************************************************}

const
  shell32 = 'shell32.dll';


//==============================================================================
// Private variables
//==============================================================================

var
  Shell32Lib: HModule = 0;

var
  _SHGetPathFromIDListA: function (pidl: PItemIDList;
    pszPath: PAnsiChar): BOOL; stdcall;
  _SHGetPathFromIDListW: function (pidl: PItemIDList;
    pszPath: PWideChar): BOOL; stdcall;
  _SHGetSpecialFolderLocation: function (hwndOwner: HWND; nFolder: Integer;
    var ppidl: PItemIDList): HResult; stdcall;
  _SHGetSpecialFolderPathA: function (hwndOwner: HWND; lpszPath: PAnsiChar;
    nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
  _SHGetSpecialFolderPathW: function (hwndOwner: HWND; lpszPath: PWideChar;
    nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
    

//==============================================================================
// Routines for initialization/finalization
//==============================================================================

procedure InitShlObj; inline;
begin
  Shell32Lib := GetModuleHandle(shell32);
end;


//==============================================================================
// Misc functions
//==============================================================================

function SHGetPathFromIDListA(pidl: PItemIDList; pszPath: PAnsiChar): BOOL;
begin
  if Assigned(_SHGetPathFromIDListA) then
    Result := _SHGetPathFromIDListA(pidl, pszPath)
  else
  begin
    InitShlObj;

    Result := False;
    if Shell32Lib > 0 then
    begin
      _SHGetPathFromIDListA :=
        GetProcAddress(Shell32Lib, 'SHGetPathFromIDListA'); // Do not localize

      if Assigned(_SHGetPathFromIDListA) then
        Result := _SHGetPathFromIDListA(pidl, pszPath);
    end;
  end;
end;

//------------------------------------------------------------------------------

function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PChar): BOOL;
begin
  Result := SHGetPathFromIDListA(pidl, pszPath);
end;

//------------------------------------------------------------------------------

function SHGetPathFromIDListW(pidl: PItemIDList;
  pszPath: PWideChar): BOOL;
begin
  if Assigned(_SHGetPathFromIDListW) then
    Result := _SHGetPathFromIDListW(pidl, pszPath)
  else
  begin
    InitShlObj;

    Result := False;
    if Shell32Lib > 0 then
    begin
      _SHGetPathFromIDListW :=
        GetProcAddress(Shell32Lib, 'SHGetPathFromIDListW'); // Do not localize

      if Assigned(_SHGetPathFromIDListW) then
        Result := _SHGetPathFromIDListW(pidl, pszPath);
    end;
  end;
end;

//------------------------------------------------------------------------------

function SHGetSpecialFolderLocation(hwndOwner: HWND; nFolder: Integer;
  var ppidl: PItemIDList): HResult;
begin
  if Assigned(_SHGetSpecialFolderLocation) then
    Result := _SHGetSpecialFolderLocation(hwndOwner, nFolder, ppidl)
  else
  begin
    InitShlObj;

    Result := E_NOTIMPL;
    if Shell32Lib > 0 then
    begin
      _SHGetSpecialFolderLocation :=
        GetProcAddress(Shell32Lib, 'SHGetSpecialFolderLocation'); // Do not localize

      if Assigned(_SHGetSpecialFolderLocation) then
        Result := _SHGetSpecialFolderLocation(hwndOwner, nFolder, ppidl);
    end;
  end;
end;

//------------------------------------------------------------------------------

function SHGetSpecialFolderPathA(hwndOwner: HWND; lpszPath: PAnsiChar;
  nFolder: Integer; fCreate: BOOL): BOOL;
begin
  if Assigned(_SHGetSpecialFolderPathA) then
    Result := _SHGetSpecialFolderPathA(hwndOwner, lpszPath, nFolder, fCreate)
  else
  begin
    InitShlObj;

    Result := False;
    if Shell32Lib > 0 then
    begin
      _SHGetSpecialFolderPathA :=
        GetProcAddress(Shell32Lib, 'SHGetSpecialFolderPathA'); // Do not localize

      if Assigned(_SHGetSpecialFolderPathA) then
        Result := _SHGetSpecialFolderPathA(hwndOwner, lpszPath, nFolder,
          fCreate);
    end;
  end;
end;

//------------------------------------------------------------------------------

function SHGetSpecialFolderPath(hwndOwner: HWND; lpszPath: PChar;
  nFolder: Integer; fCreate: BOOL): BOOL;
begin
  Result := SHGetSpecialFolderPathA(hwndOwner, lpszPath, nFolder, fCreate);
end;

//------------------------------------------------------------------------------

function SHGetSpecialFolderPathW(hwndOwner: HWND; lpszPath: PWideChar;
  nFolder: Integer; fCreate: BOOL): BOOL;
begin
  if Assigned(_SHGetSpecialFolderPathW) then
    Result := _SHGetSpecialFolderPathW(hwndOwner, lpszPath, nFolder, fCreate)
  else
  begin
    InitShlObj;

    Result := False;
    if Shell32Lib > 0 then
    begin
      _SHGetSpecialFolderPathW :=
        GetProcAddress(Shell32Lib, 'SHGetSpecialFolderPathW'); // Do not localize

      if Assigned(_SHGetSpecialFolderPathW) then
        Result := _SHGetSpecialFolderPathW(hwndOwner, lpszPath, nFolder,
          fCreate);
    end;
  end;
end;

//------------------------------------------------------------------------------

end.
