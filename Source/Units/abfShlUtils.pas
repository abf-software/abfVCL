{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfShlUtils;

{$I abf.inc}

interface

uses
  Windows,
// ABF VCL
  KnownFolders, abfShlObj, abfKnownFolders;

const
  CSIDL_PERSONAL                = $0005;  // My Documents
  {$IfDef C3}{$EXTERNALSYM CSIDL_PERSONAL}{$EndIf C3}
  CSIDL_FAVORITES               = $0006;  // <user name>\Favorites
  {$IfDef C3}{$EXTERNALSYM CSIDL_FAVORITES}{$EndIf C3}
  CSIDL_SENDTO                  = $0009;  // <user name>\SendTo
  {$IfDef C3}{$EXTERNALSYM CSIDL_SENDTO}{$EndIf C3}
  CSIDL_APPDATA                 = $001a;  // <user name>\Application Data
  {$IfDef C3}{$EXTERNALSYM CSIDL_APPDATA}{$EndIf C3}
  CSIDL_LOCAL_APPDATA           = $001c;  // <user name>\Local Settings\Applicaiton Data (non roaming)
  {$IfDef C3}{$EXTERNALSYM CSIDL_LOCAL_APPDATA}{$EndIf C3}
  CSIDL_MYMUSIC                 = $000d;  // "My Music" folder
  {$IfDef C3}{$EXTERNALSYM CSIDL_MYMUSIC}{$EndIf C3}
  CSIDL_MYVIDEO                 = $000e;  // "My Videos" folder
  {$IfDef C3}{$EXTERNALSYM CSIDL_MYVIDEO}{$EndIf C3}
  CSIDL_PROGRAM_FILES           = $0026;  // C:\Program Files
  {$IfDef C3}{$EXTERNALSYM CSIDL_PROGRAM_FILES}{$EndIf C3}
  CSIDL_MYPICTURES              = $0027;  // C:\Program Files\My Pictures
  {$IfDef C3}{$EXTERNALSYM CSIDL_MYPICTURES}{$EndIf C3}
  CSIDL_PROFILE                 = $0028;  // USERPROFILE
  {$IfDef C3}{$EXTERNALSYM CSIDL_PROFILE}{$EndIf C3}
  CSIDL_SYSTEMX86               = $0029; // x86 system directory on RISC
  {$IfDef C3}{$EXTERNALSYM CSIDL_SYSTEMX86}{$EndIf C3}
  CSIDL_PROGRAM_FILESX86        = $002a; // x86 C:\Program Files on RISC
  {$IfDef C3}{$EXTERNALSYM CSIDL_PROGRAM_FILESX86}{$EndIf C3}
  CSIDL_PROGRAM_FILES_COMMON    = $002b; // C:\Program Files\Common
  {$IfDef C3}{$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}{$EndIf C3}
  CSIDL_PROGRAM_FILES_COMMONX86 = $002c; // x86 C:\Program Files\Common on RISC
  {$IfDef C3}{$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMONX86}{$EndIf C3}
  CSIDL_COMMON_DOCUMENTS        = $002e;  // All Users\Documents
  {$IfDef C3}{$EXTERNALSYM CSIDL_COMMON_DOCUMENTS}{$EndIf C3}


(*
#define CSIDL_DESKTOP                   0x0000        // <desktop>
#define CSIDL_INTERNET                  0x0001        // Internet Explorer (icon on desktop)
#define CSIDL_PROGRAMS                  0x0002        // Start Menu\Programs
#define CSIDL_CONTROLS                  0x0003        // My Computer\Control Panel
#define CSIDL_PRINTERS                  0x0004        // My Computer\Printers
#define CSIDL_PERSONAL                  0x0005        // My Documents
#define CSIDL_FAVORITES                 0x0006        // <user name>\Favorites
#define CSIDL_STARTUP                   0x0007        // Start Menu\Programs\Startup
#define CSIDL_RECENT                    0x0008        // <user name>\Recent
#define CSIDL_SENDTO                    0x0009        // <user name>\SendTo
#define CSIDL_BITBUCKET                 0x000a        // <desktop>\Recycle Bin
#define CSIDL_STARTMENU                 0x000b        // <user name>\Start Menu
#define CSIDL_MYDOCUMENTS               0x000c        // logical "My Documents" desktop icon
#define CSIDL_MYMUSIC                   0x000d        // "My Music" folder
#define CSIDL_MYVIDEO                   0x000e        // "My Videos" folder
#define CSIDL_DESKTOPDIRECTORY          0x0010        // <user name>\Desktop
#define CSIDL_DRIVES                    0x0011        // My Computer
#define CSIDL_NETWORK                   0x0012        // Network Neighborhood (My Network Places)
#define CSIDL_NETHOOD                   0x0013        // <user name>\nethood
#define CSIDL_FONTS                     0x0014        // windows\fonts
#define CSIDL_TEMPLATES                 0x0015
#define CSIDL_COMMON_STARTMENU          0x0016        // All Users\Start Menu
#define CSIDL_COMMON_PROGRAMS           0X0017        // All Users\Start Menu\Programs
#define CSIDL_COMMON_STARTUP            0x0018        // All Users\Startup
#define CSIDL_COMMON_DESKTOPDIRECTORY   0x0019        // All Users\Desktop
#define CSIDL_APPDATA                   0x001a        // <user name>\Application Data
#define CSIDL_PRINTHOOD                 0x001b        // <user name>\PrintHood

#ifndef CSIDL_LOCAL_APPDATA
#define CSIDL_LOCAL_APPDATA             0x001c        // <user name>\Local Settings\Applicaiton Data (non roaming)
#endif // CSIDL_LOCAL_APPDATA

#define CSIDL_ALTSTARTUP                0x001d        // non localized startup
#define CSIDL_COMMON_ALTSTARTUP         0x001e        // non localized common startup
#define CSIDL_COMMON_FAVORITES          0x001f

#ifndef _SHFOLDER_H_
#define CSIDL_INTERNET_CACHE            0x0020
#define CSIDL_COOKIES                   0x0021
#define CSIDL_HISTORY                   0x0022
#define CSIDL_COMMON_APPDATA            0x0023        // All Users\Application Data
#define CSIDL_WINDOWS                   0x0024        // GetWindowsDirectory()
#define CSIDL_SYSTEM                    0x0025        // GetSystemDirectory()
#define CSIDL_PROGRAM_FILES             0x0026        // C:\Program Files
#define CSIDL_MYPICTURES                0x0027        // C:\Program Files\My Pictures
#endif // _SHFOLDER_H_

#define CSIDL_PROFILE                   0x0028        // USERPROFILE
#define CSIDL_SYSTEMX86                 0x0029        // x86 system directory on RISC
#define CSIDL_PROGRAM_FILESX86          0x002a        // x86 C:\Program Files on RISC

#ifndef _SHFOLDER_H_
#define CSIDL_PROGRAM_FILES_COMMON      0x002b        // C:\Program Files\Common
#endif // _SHFOLDER_H_

#define CSIDL_PROGRAM_FILES_COMMONX86   0x002c        // x86 Program Files\Common on RISC
#define CSIDL_COMMON_TEMPLATES          0x002d        // All Users\Templates

#ifndef _SHFOLDER_H_
#define CSIDL_COMMON_DOCUMENTS          0x002e        // All Users\Documents
#define CSIDL_COMMON_ADMINTOOLS         0x002f        // All Users\Start Menu\Programs\Administrative Tools
#define CSIDL_ADMINTOOLS                0x0030        // <user name>\Start Menu\Programs\Administrative Tools
#endif // _SHFOLDER_H_

#define CSIDL_CONNECTIONS               0x0031        // Network and Dial-up Connections
#define CSIDL_COMMON_MUSIC              0x0035        // All Users\My Music
#define CSIDL_COMMON_PICTURES           0x0036        // All Users\My Pictures
#define CSIDL_COMMON_VIDEO              0x0037        // All Users\My Video
#define CSIDL_RESOURCES                 0x0038        // Resource Direcotry

#ifndef _SHFOLDER_H_
#define CSIDL_RESOURCES_LOCALIZED       0x0039        // Localized Resource Direcotry
#endif // _SHFOLDER_H_

#define CSIDL_COMMON_OEM_LINKS          0x003a        // Links to All Users OEM specific apps
#define CSIDL_CDBURN_AREA               0x003b        // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
// unused                               0x003c
#define CSIDL_COMPUTERSNEARME           0x003d        // Computers Near Me (computered from Workgroup membership)

#ifndef _SHFOLDER_H_
#define CSIDL_FLAG_CREATE               0x8000        // combine with CSIDL_ value to force folder creation in SHGetFolderPath()
#endif // _SHFOLDER_H_

#define CSIDL_FLAG_DONT_VERIFY          0x4000        // combine with CSIDL_ value to return an unverified folder path
#define CSIDL_FLAG_NO_ALIAS             0x1000        // combine with CSIDL_ value to insure non-alias versions of the pidl
#define CSIDL_FLAG_PER_USER_INIT        0x0800        // combine with CSIDL_ value to indicate per-user init (eg. upgrade)
#define CSIDL_FLAG_MASK                 0xFF00        // mask for all possible flag values
*)


//==============================================================================
// Files/folders utilities
//==============================================================================


//------------------------------------------------------------------------------
// Retrieves the location of a special folder.
function abfGetSpecialFolderLocationA(ACSIDL: Integer): AnsiString;
function abfGetSpecialFolderLocation(ACSIDL: Integer): string;
function abfGetSpecialFolderLocationW(ACSIDL: Integer): WideString;

//------------------------------------------------------------------------------
// Retrieves the location of a special folder. ACreateDir indicates
// if the folder should be created if it does not already exist.
function abfGetSpecialFolderPathA(ACSIDL: Integer;
  ACreateDir: Boolean): AnsiString;
function abfGetSpecialFolderPath(ACSIDL: Integer; ACreateDir: Boolean): string;
function abfGetSpecialFolderPathW(ACSIDL: Integer;
  ACreateDir: Boolean): WideString;

//------------------------------------------------------------------------------
// Retrieves the full path of a known folder identified by
// the folder's KNOWNFOLDERID.
function abfGetKnownFolderPathA(AKFID: KNOWNFOLDERID;
  ACreateDir: Boolean): AnsiString;
function abfGetKnownFolderPath(AKFID: KNOWNFOLDERID; ACreateDir: Boolean): string;
function abfGetKnownFolderPathW(AKFID: KNOWNFOLDERID;
  ACreateDir: Boolean): WideString;

//------------------------------------------------------------------------------
// Retrieves the path to the directory that contains documents that are common
// to all users.
function abfGetCommonDocumentsDirA: AnsiString;
function abfGetCommonDocumentsDir: string;
function abfGetCommonDocumentsDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the "%ProgramFiles%\Common Files" directory.
function abfGetCommonFilesDirA: AnsiString;
function abfGetCommonFilesDir: string;
function abfGetCommonFilesDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of a data repository for local (nonroaming) applications.
function abfGetLocalAppDataDirA: AnsiString;
function abfGetLocalAppDataDir: string;
function abfGetLocalAppDataDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of a common repository for application-specific data.
function abfGetRoamingAppDataDirA: AnsiString;
function abfGetRoamingAppDataDir: string;
function abfGetRoamingAppDataDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the Favorites folder.
function abfGetFavoritesDirA: AnsiString;
function abfGetFavoritesDir: string;
function abfGetFavoritesDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the personal folder.
function abfGetPersonalDirA: AnsiString;
function abfGetPersonalDir: string;
function abfGetPersonalDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the "Program Files" directory.
function abfGetProgramFilesDirA: AnsiString;
function abfGetProgramFilesDir: string;
function abfGetProgramFilesDirW: WideString;

//------------------------------------------------------------------------------
// Retrieves the path of the SendTo folder.
function abfGetSendToDirA: AnsiString;
function abfGetSendToDir: string;
function abfGetSendToDirW: WideString;


{******************************************************************************}
implementation
{******************************************************************************}

uses
  ActiveX,
// ABF VCL
  abfSysUtils, abfRegistry;


//==============================================================================
// Files/folders utilities
//==============================================================================

//------------------------------------------------------------------------------
// Retrieves the location of a special folder.
// Date: 07/06/2001
// Unicode version added 06/27/2007

function abfGetSpecialFolderLocationA(ACSIDL: Integer): AnsiString;
var
  PIDL: PItemIDList;
  TempPath: PAnsiChar;
begin
  Result := '';
  PIDL := nil;
  if not Succeeded(abfShlObj.SHGetSpecialFolderLocation(0, ACSIDL,
    PIDL)) then Exit;
  try
    GetMem(TempPath, SizeOf(TempPath^) * (MAX_PATH + 1));
    try
      if abfShlObj.SHGetPathFromIDListA(PIDL, TempPath) then
        Result := TempPath;
    finally
      FreeMem(TempPath)
    end;
  finally
    if Assigned(PIDL) then CoTaskMemFree(PIDL);
  end;
end;

//------------------------------------------------------------------------------

function abfGetSpecialFolderLocation(ACSIDL: Integer): string;
begin
  Result := abfGetSpecialFolderLocationA(ACSIDL);
end;

//------------------------------------------------------------------------------

function abfGetSpecialFolderLocationW(ACSIDL: Integer): WideString;
var
  PIDL: PItemIDList;
  TempPath: PWideChar;
begin
  if not IsWinNT then
  begin
    Result := abfGetSpecialFolderLocationA(ACSIDL);
    Exit;
  end;

  Result := '';
  PIDL := nil;
  if not Succeeded(abfShlObj.SHGetSpecialFolderLocation(0, ACSIDL,
    PIDL)) then Exit;
  try
    GetMem(TempPath, SizeOf(TempPath^) * (MAX_PATH + 1));
    try
      if abfShlObj.SHGetPathFromIDListW(PIDL, TempPath) then
        Result := TempPath;
    finally
      FreeMem(TempPath)
    end;
  finally
    if Assigned(PIDL) then CoTaskMemFree(PIDL);
  end;
end;

//------------------------------------------------------------------------------
// Retrieves the location of a special folder. ACreateDir indicates
// if the folder should be created if it does not already exist.
// Date: 07/10/2007

function abfGetSpecialFolderPathA(ACSIDL: Integer;
  ACreateDir: Boolean): AnsiString;
var
  Buf: PAnsiChar;
begin
  Result := '';
  GetMem(Buf, SizeOf(Buf^) * (MAX_PATH + 2048));
  try
    if not abfShlObj.SHGetSpecialFolderPathA(0, Buf, ACSIDL,
      ACreateDir) then Exit;

    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

//------------------------------------------------------------------------------

function abfGetSpecialFolderPath(ACSIDL: Integer; ACreateDir: Boolean): string;
begin
  Result := abfGetSpecialFolderPathA(ACSIDL, ACreateDir);
end;

//------------------------------------------------------------------------------

function abfGetSpecialFolderPathW(ACSIDL: Integer;
  ACreateDir: Boolean): WideString;
var
  Buf: PWideChar;
begin
  Result := '';

  if not IsWinNT then
  begin
    Result := abfGetSpecialFolderPathA(ACSIDL, ACreateDir);
    Exit;
  end;

  GetMem(Buf, SizeOf(Buf^) * (MAX_PATH + 2048));
  try
    if not abfShlObj.SHGetSpecialFolderPathW(0, Buf, ACSIDL,
      ACreateDir) then Exit;

    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;


//------------------------------------------------------------------------------
// Retrieves the full path of a known folder identified by
// the folder's KNOWNFOLDERID.
// Date: 11/01/2009

function abfGetKnownFolderPathA(AKFID: KNOWNFOLDERID;
  ACreateDir: Boolean): AnsiString;
begin
  Result := AnsiString(abfGetKnownFolderPathW(AKFID, ACreateDir));
end;

//------------------------------------------------------------------------------

function abfGetKnownFolderPath(AKFID: KNOWNFOLDERID; ACreateDir: Boolean): string;
begin
  Result := abfGetKnownFolderPathA(AKFID, ACreateDir);
end;

//------------------------------------------------------------------------------

function abfGetKnownFolderPathW(AKFID: KNOWNFOLDERID;
  ACreateDir: Boolean): WideString;
var
  TempFlag: DWORD;
  TempPath: LPWSTR;
begin
  Result := '';
  if not OSVersion.Check(6) then Exit;

  TempFlag := 0;
  if ACreateDir then
    TempFlag := TempFlag or KF_FLAG_CREATE;

  TempPath := nil;
  if not Succeeded(SHGetKnownFolderPath(@AKFID, TempFlag, 0, TempPath)) then Exit;
  try
    Result := WideString(TempPath);
  finally
    if Assigned(TempPath) then CoTaskMemFree(TempPath);
  end;
end;


//------------------------------------------------------------------------------
// Retrieves the path to the directory that contains documents that are common
// to all users.
// Date: 06/27/2007

function abfGetCommonDocumentsDirA: AnsiString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathA(FOLDERID_PublicDocuments, False)
  else
    Result := abfGetSpecialFolderLocationA(CSIDL_COMMON_DOCUMENTS);
end;

//------------------------------------------------------------------------------

function abfGetCommonDocumentsDir: string;
begin
  Result := abfGetCommonDocumentsDirA;
end;

//------------------------------------------------------------------------------

function abfGetCommonDocumentsDirW: WideString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathW(FOLDERID_PublicDocuments, False)
  else
    Result := abfGetSpecialFolderLocationW(CSIDL_COMMON_DOCUMENTS);
end;

//------------------------------------------------------------------------------
// Retrieves the path of the "%ProgramFiles%\Common Files" directory.
// Date: 10/24/2015

function abfGetCommonFilesDirA: AnsiString;
var
  S: AnsiString;
begin
  if not IsWinNT then
  begin
    S := abfAddSlashA(abfExtractFileDriveA(abfGetWindowsDirA)) +
      'Program Files\Common Files';
    try
      Result := abfRegReadStringDef(HKEY_LOCAL_MACHINE,
        'SOFTWARE\Microsoft\Windows\CurrentVersion', 'CommonFilesDir', S);
    except
      Result := S;
    end;
  end else
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathA(FOLDERID_ProgramFilesCommon, False);
end;

//------------------------------------------------------------------------------

function abfGetCommonFilesDir: string;
begin
  Result := abfGetCommonFilesDirA;
end;

//------------------------------------------------------------------------------

function abfGetCommonFilesDirW: WideString;
begin
  if not IsWinNT then
  begin
    Result := abfGetCommonFilesDirA;
    Exit;
  end;

  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathW(FOLDERID_ProgramFilesCommon, False)
  else
    try
      Result := abfGetSpecialFolderLocationW(CSIDL_PROGRAM_FILES_COMMON);
    except
      Result := abfGetCommonFilesDirA;
    end;
end;

//------------------------------------------------------------------------------
// Retrieves the path of a data repository for local (nonroaming) applications.
// Date: 11/01/2009

function abfGetLocalAppDataDirA: AnsiString;
begin
  if OSVersion.Check(5) then
  begin
    if OSVersion.Check(6) then
      Result := abfGetKnownFolderPathA(FOLDERID_LocalAppData, False)
    else
      Result := abfGetSpecialFolderLocationA(CSIDL_LOCAL_APPDATA);
  end else
  begin
    if IsWinNT then
      Result := abfAddSlashA(abfGetEnvVarA('USERPROFILE')) +
        'Local Settings\Application Data'
    else
      Result := abfAddSlashA(abfGetWindowsDirA) +
        'Local Settings\Application Data';
  end;
end;

//------------------------------------------------------------------------------

function abfGetLocalAppDataDir: string;
begin
  Result := abfGetLocalAppDataDirA;
end;

//------------------------------------------------------------------------------

function abfGetLocalAppDataDirW: WideString;
begin
  if OSVersion.Check(5) then
  begin
    if OSVersion.Check(6) then
      Result := abfGetKnownFolderPathW(FOLDERID_LocalAppData, False)
    else
      Result := abfGetSpecialFolderLocationW(CSIDL_LOCAL_APPDATA);
  end else
  begin
    if IsWinNT then
      Result := abfAddSlashW(abfGetEnvVarW('USERPROFILE')) +
        'Local Settings\Application Data'
    else
      Result := abfAddSlashW(abfGetWindowsDirW) +
        'Local Settings\Application Data';
  end;
end;


//------------------------------------------------------------------------------
// Retrieves the path of a common repository for application-specific data.
// Date: 03/19/2015

function abfGetRoamingAppDataDirA: AnsiString;
begin
  if OSVersion.Check(5) then
  begin
    if OSVersion.Check(6) then
      Result := abfGetKnownFolderPathA(FOLDERID_RoamingAppData, False)
    else
      Result := abfGetSpecialFolderLocationA(CSIDL_APPDATA);
  end else
  begin
    if IsWinNT then
      Result := abfAddSlashA(abfGetEnvVarA('USERPROFILE')) + 'Application Data'
    else
      Result := abfAddSlashA(abfGetWindowsDirA) + 'Application Data';
  end;
end;

//------------------------------------------------------------------------------

function abfGetRoamingAppDataDir: string;
begin
  Result := abfGetRoamingAppDataDirA;
end;

//------------------------------------------------------------------------------

function abfGetRoamingAppDataDirW: WideString;
begin
  if OSVersion.Check(5) then
  begin
    if OSVersion.Check(6) then
      Result := abfGetKnownFolderPathW(FOLDERID_RoamingAppData, False)
    else
      Result := abfGetSpecialFolderLocationW(CSIDL_APPDATA);
  end else
  begin
    if IsWinNT then
      Result := abfAddSlashW(abfGetEnvVarW('USERPROFILE')) + 'Application Data'
    else
      Result := abfAddSlashW(abfGetWindowsDirW) + 'Application Data';
  end;
end;


//------------------------------------------------------------------------------
// Retrieves the path of the Favorites folder.
// Date: 07/06/2001
// Unicode version added 06/27/2007

function abfGetFavoritesDirA: AnsiString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathA(FOLDERID_Favorites, False)
  else
    Result := abfGetSpecialFolderLocationA(CSIDL_FAVORITES);
end;

//------------------------------------------------------------------------------

function abfGetFavoritesDir: string;
begin
  Result := abfGetFavoritesDirA;
end;

//------------------------------------------------------------------------------

function abfGetFavoritesDirW: WideString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathW(FOLDERID_Favorites, False)
  else
    Result := abfGetSpecialFolderLocationW(CSIDL_FAVORITES);
end;

//------------------------------------------------------------------------------
// Retrieves the path of the personal folder.
// Date: 06/27/2007

function abfGetPersonalDirA: AnsiString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathA(FOLDERID_Documents, False)
  else
    Result := abfGetSpecialFolderLocationA(CSIDL_PERSONAL);
end;

//------------------------------------------------------------------------------

function abfGetPersonalDir: string;
begin
  Result := abfGetPersonalDirA;
end;

//------------------------------------------------------------------------------

function abfGetPersonalDirW: WideString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathW(FOLDERID_Documents, False)
  else
    Result := abfGetSpecialFolderLocationW(CSIDL_PERSONAL);
end;

//------------------------------------------------------------------------------
// Retrieves the path of the "Program Files" directory.
// Date: 03/11/2001
// Unicode version added 07/02/2007

function abfGetProgramFilesDirA: AnsiString;
var
  S: AnsiString;
begin
  if not IsWinNT then
  begin
    S := abfAddSlashA(abfExtractFileDriveA(abfGetWindowsDirA)) +
      'Program Files';
    try
      Result := abfRegReadStringDef(HKEY_LOCAL_MACHINE,
        'SOFTWARE\Microsoft\Windows\CurrentVersion', 'ProgramFilesDir', S);
    except
      Result := S;
    end;
  end else
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathA(FOLDERID_ProgramFiles, False);
end;

//------------------------------------------------------------------------------

function abfGetProgramFilesDir: string;
begin
  Result := abfGetProgramFilesDirA;
end;

//------------------------------------------------------------------------------

function abfGetProgramFilesDirW: WideString;
begin
  if not IsWinNT then
  begin
    Result := abfGetProgramFilesDirA;
    Exit;
  end;

  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathW(FOLDERID_ProgramFiles, False)
  else
    try
      Result := abfGetSpecialFolderLocationW(CSIDL_PROGRAM_FILES);
    except
      Result := abfGetProgramFilesDirA;
    end;
end;

//------------------------------------------------------------------------------
// Retrieves the path of the SendTo folder.
// Date: 07/06/2001
// Unicode version added 06/27/2007

function abfGetSendToDirA: AnsiString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathA(FOLDERID_SendTo, False)
  else
    Result := abfGetSpecialFolderLocationA(CSIDL_SENDTO);
end;

//------------------------------------------------------------------------------

function abfGetSendToDir: string;
begin
  Result := abfGetSendToDirA;
end;

//------------------------------------------------------------------------------

function abfGetSendToDirW: WideString;
begin
  if OSVersion.Check(6) then
    Result := abfGetKnownFolderPathW(FOLDERID_SendTo, False)
  else
    Result := abfGetSpecialFolderLocationW(CSIDL_SENDTO);
end;


end.
