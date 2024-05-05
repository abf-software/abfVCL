{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfVclConsts; { English version }

{$I abf.inc}

interface

uses
  abfConsts;

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}

//==============================================================================
// Messages, captions, errors, etc.
//==============================================================================

  SabfAbout       = 'About';
  SabfDesigner    = 'Designer';
  SabfWarning     = 'Warning';
  SabfError       = 'Error';
  SabfInformation = 'Information';
  SabfImportant   = 'Important';
  SabfBuy         = 'Buy';

  SabfAboutApplicationCaption    = 'About application';
  SabfAboutApplicationBy         = 'created by';
  SabfAboutApplicationVersionFmt = 'Version: %s (build %s)';

  SabfAboutComponentCaption      = 'About component';
  SabfAboutComponentBy           = 'created by';
  SabfAboutComponentVersionFmt   = 'Version: %s';

{$IfNDef D3}
  SNoTimers = 'Not enough timers available';
  SWin32Error = 'Win32 Error.  Code: %d.'#10'%s';
  SUnkWin32Error = 'A Win32 API function failed';
  SDefaultFilter = 'All files (*.*)|*.*';
{$EndIf D3}

{$IfNDef D4}
  SCannotOpenClipboard = 'Cannot open clipboard';
{$EndIf D4}

  SabfIncompatibleShell = '%s is not compatible with the current version of shell.';
  SabfNoFile = 'File "%s" does not exist';

  SabfProperty_ReadOnlyMode = 'Cannot change the property in Read-Only mode.';
  SabfRegistry_UnableToOpenKey = 'Unable to open the %s key';
  SabfRegistry_CannotWrite = 'Cannot write to registry.';
  SabfChannel_CreationFailed = 'Channel creation is failed';
  SabfChannel_MappingFailed = 'Channel mapping is failed';
  SabfFileVersionInfo_NoInfo = 'File contains no version information';
  SabfFileVersionInfo_WrongLngIndex = 'Illegal language index';
  SabfFCreateErrorEx = 'Cannot create file "%s". %s';
  SabfFOpenErrorEx = 'Cannot open file "%s". %s';
  SabfTreadComponent_Exception = 'Thread %s raised exception class %s with message "%s".';
  SabfFileOperation_DesignerMenuItem = 'Edit file list...';
  SabfFileAssociation_DesignerMenuItem = 'Load file association';
  SabfFileAssociation_DesignerQuestion = 'Are you sure you want to load a file association?';
  SabfOneInstance_AppAlreadyRun = 'Application "%s" is already running.';
  SabfTrayIcon_ErrInsertTo   = '%s.InsertToTray failed';
  SabfTrayIcon_ErrDeleteFrom = '%s.DeleteFromTray failed';
  SabfTrayIcon_DesignerMenuItem1 = 'Show balloon info';
  SabfFileStorage_AutoSaveWhenNoSaveName = 'AutoSave can not be set because the SaveName property contains a wrong file name';
  SabfFileStorage_CannotLoad = 'Can not load data from the "%s" file';
  SabfFileStorage_CannotSave = 'Can not save data to the "%s" file';
  SabfFileStorage_DesignerMenuItem1 = 'Load file into DFM...';
  SabfFileStorage_DesignerMenuItem2 = 'Remove file from DFM';
  SabfWav_DesignerMenuItem1 = 'Play sound';
  SabfWav_DesignerMenuItem2 = 'Stop playing';
  SabfWave_PlayInDesignerWarning = 'Playing is canceled. Synchronous playing of the looped sound can freeze the IDE';


{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfVclConsts}.

