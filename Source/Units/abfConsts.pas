{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfConsts;

{$I abf.inc}

interface

uses
  Consts, Graphics;

const

//==============================================================================
// VCL stuff
//==============================================================================

  abfVCLVersionMajor   = 5;
  abfVCLVersionMinor   = 0;
  abfVCLVersionRelease = 0;
  abfVCLVersionBuild   = 5;
  abfVCLVersion = (abfVCLVersionMajor shl 24) or (abfVCLVersionMinor shl 16) or
    (abfVCLVersionRelease shl 8) or abfVCLVersionBuild;

  SabfVCLVersion: string = '5.0.0.5';

  SabfComponentPalette = 'ABF software';
  SabfComponentPaletteNonVisual = 'ABF non-visual';
  SabfComponentPaletteVisual    = 'ABF visual';
  SabfComponentPaletteEdits     = 'ABF edits';
  SabfComponentPaletteDialogs   = 'ABF dialogs';
  SabfAboutProperty = 'About';

//==============================================================================
// Misc
//==============================================================================

  Kb = 1024;
  Mb = 1024 * Kb;
  Gb = 1024 * Mb;
  CR = #13'';
  LF = #10'';
  CRLF = CR + LF;
  SabfTrue  = 'True';
  SabfFalse = 'False';
  CabfBooleanValues: array[Boolean] of string = (SabfFalse, SabfTrue);
{$IfNDef D4}
  SPI_GETSCREENSAVERRUNNING = 114;
{$EndIf D4}
{$IfNDef D6}
  {$IfDef C3}{$EXTERNALSYM WS_EX_LAYERED}{$EndIf C3}
  WS_EX_LAYERED = $00080000;
  {$IfDef C3}{$EXTERNALSYM LWA_COLORKEY}{$EndIf C3}
  LWA_COLORKEY = $00000001;
  {$IfDef C3}{$EXTERNALSYM LWA_ALPHA}{$EndIf C3}
  LWA_ALPHA = $00000002;
{$EndIf D6}


//==============================================================================
// Names, TMs, Copyrights, integration. Company data and style.
//==============================================================================

  SabfCompanyName      = 'ABF software, Inc.';
  SabfCompanyShortName = 'ABF software';
  SabfCopyright    = 'Copyright � 2018 '   + SabfCompanyName;
  SabfCopyrightTxt = 'Copyright (c) 2018 ' + SabfCompanyName;
  SabfAllRightsReserved = 'All rights reserved';
  SabfOneTouch     = 'OneTouch�';
  SabfOneTouchTxt  = 'OneTouch(tm)';
  SabfOneTouchDesigner    = SabfOneTouch    + ' designer';
  SabfOneTouchDesignerTxt = SabfOneTouchTxt + ' designer';

  SabfBaseRegKey = '\Software\' + SabfCompanyShortName + '\';

  clABFLink: TColor = $00CAFDFC; // Color of links

//==============================================================================
// http:// consts
//==============================================================================

  SabfWebURL          = 'http://www.abf-dev.com';
  SabfWeb_AboutURL    = SabfWebURL + '/about.shtml';
  SabfWeb_DownloadURL = SabfWebURL + '/download.shtml';
  SabfWeb_BuyURL      = SabfWebURL + '/buy.shtml';

//==============================================================================
// mailto: consts
//==============================================================================

  SabfEmail      = '@abf-dev.com';
  SabfSupportURL = 'support'  + SabfEmail;
  SabfInfoURL    = 'info'     + SabfEmail;
  SabfSalesURL   = 'sales'    + SabfEmail;

//==============================================================================
// Borland, Delphi/C++Builder consts
//==============================================================================

  SabfDelphi  = 'Delphi';
  SabfBuilder = 'C++Builder';
  SabfBDS     = 'BDS';
  SabfRegSoftwareBorland = '\Software\Borland\';
  SabfBorland = 'Borland';
  SabfBorlandWeb = 'www.borland.com';

//==============================================================================
// File extensions
//==============================================================================

  SabfExt_Pas = '.pas';
  SabfExt_Dfm = '.dfm';
  SabfExt_Dcu = '.dcu';
  SabfExt_Dpr = '.dpr';

  SabfExt_Hpp = '.hpp';
  SabfExt_Cpp = '.cpp';
  SabfExt_Obj = '.obj';
  SabfExt_Bpr = '.bpr';

  SabfExt_Dpk = '.dpk';
  SabfExt_Dpl = '.dpl';
  SabfExt_Dcp = '.dcp';
  SabfExt_Lsp = '.lsp';

  SabfExt_Bpk = '.bpk';
  SabfExt_Bmk = '.bmk';
  SabfExt_Mak = '.mak';
  SabfExt_Bpl = '.bpl';
  SabfExt_Tds = '.tds';
  SabfExt_Bpi = '.bpi';
  SabfExt_Lib = '.lib';

  SabfExt_Int = '.int';
  SabfExt_Inc = '.inc';
  SabfExt_Dcr = '.dcr';
  SabfExt_Res = '.res';

  SabfExt_Bat = '.bat';
  SabfExt_Exe = '.exe';
  SabfExt_Dll = '.dll';
  SabfExt_Log = '.log';
  SabfExt_Ini = '.ini';
  SabfExt_Lnk = '.lnk';

  SabfExt_Cnt = '.cnt';
  SabfExt_Hlp = '.hlp';
  SabfExt_CHM = '.chm';

  SabfExt_Wav = '.wav';

  SabfExt_Txt = '.txt';
  SabfExt_Rtf = '.rtf';
  SabfExt_Doc = '.doc';

//==============================================================================
// Messages, captions, errors, etc.
//==============================================================================

//------------------------------------------------------------------------------
// Not resourced

{$IfDef abfVCLTrial}
  SabfMsgTrial =
    'This application uses an unregistered' + CRLF +
    'version of ABF VCL by ABF software, Inc.' + CRLF +
    'Do you want to register ABF VCL now?';

  SabfMsgHacked =
    'This application uses a HACKED version of ABF VCL by ABF software, Inc.' + CRLF +
    'STOP using it! It is ILLEGAL! Do you want to register ABF VCL now?';
{$EndIf abfVCLTrial}

{******************************************************************************}
implementation
{******************************************************************************}
{$IfDef abfVCLTrial}
var
  S: string;
{$EndIf abfVCLTrial}

{******************************************************************************}
initialization
{******************************************************************************}

{$IfDef abfVCLTrial}
  SabfVCLVersion := SabfVCLVersion + ' <Trial>';
  S := SabfMsgHacked + ' '; // Prevent a removing of constant by optimization.
{$EndIf abfVCLTrial}

end{unit abfConsts}.
