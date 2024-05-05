{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDialogsConsts; { English version }

{$I abf.inc}

interface

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}
  SMsgDlgWarning     = 'Warning';
  SMsgDlgError       = 'Error';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm     = 'Confirm';
  SMsgDlgYes         = '&Yes';
  SMsgDlgNo          = '&No';
  SMsgDlgOK          = 'OK';
  SMsgDlgCancel      = 'Cancel';
  SMsgDlgHelp        = '&Help';
  SMsgDlgHelpNone    = 'No help available';
  SMsgDlgHelpHelp    = 'Help';
  SMsgDlgAbort       = '&Abort';
  SMsgDlgRetry       = '&Retry';
  SMsgDlgIgnore      = '&Ignore';
  SMsgDlgAll         = '&All';
  SMsgDlgNoToAll     = 'N&o to All';
  SMsgDlgYesToAll    = 'Yes to &All';

  SabfBrowseFolderDlg_CreateButtonCaption = 'Create...';
  SabfBrowseFolderDlg_DlgFolderNameCaption = 'Create directory';
  SabfBrowseFolderDlg_DlgFolderNamePrompt  = 'Name';

  SabfDialogs_DirNamePropertyText = 'Select the folder to be a property value';
  SabfDialogs_DesignerMenuItem    = 'Test dialog...';

{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfDialogsConsts}.
