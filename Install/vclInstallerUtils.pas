{*******************************************************************************

  VCL Installer. Functions and procedures.

  Author: LinX and KARPOLAN
  E-Mail: info@abf-dev.com
  WEB:    http://www.abf-dev.com
  Copyright (c) 2000-2008 ABF software, Inc.
  All rights reserved.

*******************************************************************************}
unit vclInstallerUtils;

{$I abf.inc}

interface

//==============================================================================


//------------------------------------------------------------------------------
// Main work routine
procedure Execute;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  Windows, SysUtils, Classes, Registry, IniFiles, StrUtils,
  {$IFDEF D9}WideStrings, {$ENDIF}
  abfConsts, abfClasses, abfSysUtils, abfStrUtils, abfVclUtils, abfIDEUtils;

const
//==============================================================================
// Messages and string consts
//==============================================================================

  SVersion = '2.0';
  SMsgAppInfo = CRLF +
    'VCL Installer v' + SVersion + ' - Delphi/C++Builder package and help manager.' + CRLF +
    SabfCopyrightTxt + ' ' + SabfAllRightsReserved + '.' + CRLF +
    '  Web:    ' + SabfWebURL + CRLF +
    '  E-mail: ' + SabfInfoURL + CRLF;

  SMsgUsage1 =
    'Installs/removes packages and help files into/from Delphi/C++Builder IDE' + CRLF +
    'Usage:   vclInstaller.exe config [/u] [/b]' + CRLF +
    'Example: vclInstaller.exe packages.cfg /b' + CRLF;
  SMsgUsage2 =
    '  config - name of the configuration file. See ReadMe.txt for details' + CRLF +
    '  /u     - uninstall switch' + CRLF +
    '  /b     - "build only" switch' + CRLF;

  SMsgDone = CRLF + 'Done.' + CRLF;

  cCommonSection = 'COMMON';
  cSearchPath    = 'SEARCH_PATH';
  cBrowsingPath  = 'BROWSING_PATH';
  cDefineSymbols = 'DEFINE_SYMBOLS';
  cBPLOutputDir  = 'BPL_OUTPUT_DIR';
  cDCPOutputDir  = 'DCP_OUTPUT_DIR';
  cHPPOutputDir  = 'HPP_OUTPUT_DIR';
  cOBJOutputDir  = 'OBJ_OUTPUT_DIR';
  cDCUOutputDir  = 'DCU_OUTPUT_DIR';

  cSections: array[TabfCompilerVersion] of string = (
    'D2', 'C1', 'D3', 'C3', 'D4', 'C4', 'D5', 'C5', 'D6', 'C6', 'D7',
    'D8', 'D9', 'D10', 'C10', 'D11', 'C11');
  cHelp = 'Help';


//==============================================================================
// Variables
//==============================================================================

var
  SMsgUsage, SMsgError: string;
  CMDParams: TabfWideStringList;
  ConfigFile: TIniFile;
  ConfigFileName: WideString  = '';
  UninstallMode: Boolean = False;
  BuildOnlyMode: Boolean = False;

//==============================================================================
// Routines
//==============================================================================

//------------------------------------------------------------------------------
// Output routines

procedure WriteToRightJustifiedOutput(const AText: string);
var
  TempCSBI: TConsoleScreenBufferInfo;
  TempOffset: Integer;
  TempS: string;
begin
  FillChar(TempCSBI, SizeOf(TempCSBI), 0);
  if not GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), TempCSBI) then
  begin
    Write(' ', AText);
    Exit;
  end;

  TempOffset := (TempCSBI.srWindow.Right - TempCSBI.dwCursorPosition.X) -
    Length(AText) + 1;

  if TempOffset < 1 then
  begin
    WriteLn;
    Write(AText);
    Exit;
  end else
  begin
    TempS := DupeString('.', TempOffset);
    Write(TempS, AText);
  end;
end;

//------------------------------------------------------------------------------
// Installs package to the Delphi/C++Builder IDE

function InstallPackage(AProjectConfig: TabfProjectConfig;
  const APackName: string): Boolean;
var
  TempFileName, TempFileExt: WideString;
begin
  Result := False;
  if not Assigned(AProjectConfig) then Exit;

  TempFileName := AProjectConfig.FileName;

  if not abfFileExistsW(TempFileName) then
  begin
    WriteLn(Format('Error: "%s" file is not found!', [TempFileName]));
    Exit;
  end;

// Compile the file if it is needed
  TempFileExt := abfExtractFileExtW(TempFileName);
  if not ((TempFileExt = SabfExt_Bpl) or (TempFileExt = SabfExt_Dpl)) then
  begin
    try
      WriteLn;
      Write(Format('Compiling package "%s"',
        [abfExtractFileNameW(TempFileName)]));
      TempFileName := abfCompileFile(AProjectConfig, True);

      if not abfFileExistsW(TempFileName) then
        Abort;

      WriteToRightJustifiedOutput('OK');
    except
      WriteToRightJustifiedOutput('ERROR');
      Exit;
    end;
  end else
    WriteLn;

  if BuildOnlyMode then Exit;

  Write(Format('Installing package "%s"', [abfExtractFileNameW(TempFileName)]));

// Install package
  abfInstallPackage(AProjectConfig.Version, TempFileName, APackName);
// Add search path
  abfAddSearchPathToIDE(AProjectConfig.CompilerConfig, abfExtractFilePathW(TempFileName));

  WriteToRightJustifiedOutput('OK');

  Result := True;
end;

//------------------------------------------------------------------------------
// Uninstalls package from the Delphi/C++Builder IDE

procedure UninstallPackage(AProjectConfig: TabfProjectConfig;
  const APackName: string);
var
  TempFileName: WideString;
begin
  if BuildOnlyMode then Exit;
  if not Assigned(AProjectConfig) then Exit;
  
  TempFileName := AProjectConfig.OutputFileName;
  if TempFileName = '' then Exit;

  WriteLn;
  Write(Format('Uninstalling package "%s"', [abfExtractFileNameW(TempFileName)]));

// Remove package
  abfUninstallPackage(AProjectConfig.Version, TempFileName);

// Remove search path
  abfRemoveSearchPathFromIDE(AProjectConfig.CompilerConfig,
    abfExtractFilePathW(TempFileName));

  WriteToRightJustifiedOutput('OK');
end;

//------------------------------------------------------------------------------

procedure InstallHelp(AVersion: TabfCompilerVersion; const FileName, HelpName: WideString);
{var
  DelphiDir : string;
  F : TStrings;
  i, j : integer;
  bWasIndex : boolean;
  bIndexRow : boolean;
  S : string;
  Options : string;
  SourceFile : string;
  DestFile : string;
  IncludeStr, LinkStr, IndexStr : string;
  IncludeExt, LinkExt, IndexExt : string;
  HelpCaption : string;
  InstallFile, NeedCopyFile : boolean;
  DelphiNumStr : string;{}
begin
{ TODO -oKARPOLAN : Make help installer }
(*
  DelphiDir := '';
  DelphiNumStr := IntToStr(DelphiNum);

  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.OpenKeyReadOnly(RegKey + DelphiNumStr + '.0');
  try
    DelphiDir := Reg.ReadString('RootDir');
  except
  end;
  Reg.CloseKey;
  Reg.RootKey := HKEY_CURRENT_USER;

  if DelphiDir = '' then exit;
  if DelphiDir[Length(DelphiDir)] <> '\' then DelphiDir := DelphiDir + '\';

  if DelphiNum >= 5 then
  begin
    IncludeExt := '.ohc';
    LinkExt := '.ohl';
    IndexExt := '.ohi';
  end
  else begin
    IncludeExt := '.cnt';
    LinkExt := '.cfg';
    IndexExt := '.cnt';
  end;

  F := TStringList.Create;
  try
    for I := 0 to Files.Count - 1  do
    begin
      HelpCaption := Files.Names[I];
      S := Files.Values[Files.Names[I]];
      Options := GetAfterSep(';', S);
{      InstallFile := CXPos('install', Options, 1, 1) > 0;
      NeedCopyFile := CXPos('copy', Options, 1, 1) > 0;{}
      SourceFile := ConcPath(RunDir, GetBeforeSep(';', S));
      FileName := ChangeFileExt(ExtractFileName(SourceFile), '');
      if NeedCopyFile then
      begin
        DestFile := ConcPath(DelphiDir, 'help\' + ExtractFileName(SourceFile));
        CopyFile(PChar(SourceFile), PChar(DestFile), false);
      end;

      SourceFile := ChangeFileExt(SourceFile, '.cnt');

      if (DelphiNum >= 5) and InstallFile then
      begin
        if NeedCopyFile then
          DestFile := ChangeFileExt(DestFile, '.toc')
        else
          DestFile := ChangeFileExt(SourceFile, '.toc');
        CopyFile(PChar(SourceFile), PChar(DestFile), false);
      end
      else if NeedCopyFile then
      begin
        DestFile := ChangeFileExt(DestFile, '.cnt');
        CopyFile(PChar(SourceFile), PChar(DestFile), false);
      end;

      if InstallFile then
      begin
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey('Software\Microsoft\Windows\Help', true) then
        begin
          Reg.WriteString(FileName + '.hlp', ExtractFileDir(SourceFile));
          Reg.CloseKey;
        end;
        Reg.RootKey := HKEY_CURRENT_USER;

        LinkStr := ':Link ' + FileName + '.hlp';
        if DelphiNum >= 5 then
          IncludeStr := ':Include ' + FileName + '.toc'
        else
          IncludeStr := ':Include ' + FileName + '.cnt';
        IndexStr := ':Index ' + HelpCaption + ' =' + FileName + '.hlp';

        try
          F.LoadFromFile(DelphiDir + 'Help\delphi' + DelphiNumStr +  LinkExt);
          if F.IndexOf(LinkStr) = -1 then
          begin
            F.Add(LinkStr);
            F.SaveToFile(DelphiDir + 'Help\delphi' + DelphiNumStr + LinkExt);
          end;

          F.Clear;
          F.LoadFromFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IncludeExt);
          if F.IndexOf(IncludeStr) = -1 then
          begin
            F.Add(IncludeStr);
            F.SaveToFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IncludeExt);
          end;

          F.Clear;
          F.LoadFromFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IndexExt);
          if F.IndexOf(IndexStr) = -1 then
          begin
            //find end of index section
            for j := 0 to F.Count - 1 do
            begin
              bIndexRow := CompareText(Copy(F[j], 1, Length(':Index')), ':index') = 0;
              if not bWasIndex and bIndexRow  then bWasIndex := true;
              if bWasIndex and not bIndexRow then break;
            end;
            F.Insert(j, IndexStr);
            F.SaveToFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IndexExt);
          end;

        except
           //ignore error if one of files not founded
        end;
      end;
    end;
    DestFile := DelphiDir + 'help\delphi' + DelphiNumStr + '.gid';
    DeleteFile(DestFile);
  finally
    F.Free;
  end;
*)
end;

//------------------------------------------------------------------------------

procedure UninstallHelp(AVersion: TabfCompilerVersion; const FileName, HelpName: string);
{var
  DelphiDir : string;
  F : TStrings;
  i, j, idx : integer;
  bWasIndex : boolean;
  bIndexRow : boolean;
  S : string;
  Options : string;
  SourceFile : string;
  DestFile : string;
  IncludeStr, LinkStr, IndexStr : string;
  IncludeExt, LinkExt, IndexExt : string;
  HelpCaption : string;
  InstallFile, NeedCopyFile : boolean;
  DelphiNumStr : string;{}
begin
{ TODO -oKARPOLAN : Make help uninstaller }

(*
  DelphiDir := '';
  DelphiNumStr := IntToStr(DelphiNum);

  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.OpenKeyReadOnly(RegKey + DelphiNumStr + '.0');
  try
    DelphiDir := Reg.ReadString('RootDir');
  except
  end;

  Reg.CloseKey;
  Reg.RootKey := HKEY_CURRENT_USER;

  if DelphiDir = '' then exit;
  if DelphiDir[Length(DelphiDir)] <> '\' then DelphiDir := DelphiDir + '\';

  if DelphiNum >= 5 then
  begin
    IncludeExt := '.ohc';
    LinkExt := '.ohl';
    IndexExt := '.ohi';
  end
  else begin
    IncludeExt := '.cnt';
    LinkExt := '.cfg';
    IndexExt := '.cnt';
  end;

  F := TStringList.Create;
  try
    for I := 0 to Files.Count - 1  do
    begin
      HelpCaption := Files.Names[I];
      S := Files.Values[Files.Names[I]];

      Options := GetAfterSep(';', S);
{      InstallFile := CXPos('install', Options, 1, 1) > 0;
      NeedCopyFile := CXPos('copy', Options, 1, 1) > 0;{}
      SourceFile := ConcPath(RunDir, GetBeforeSep(';', S));
      FileName := ChangeFileExt(ExtractFileName(SourceFile), '');
      if NeedCopyFile then
      begin
        DestFile := ConcPath(DelphiDir, 'help\' + ExtractFileName(SourceFile));
        DeleteFile(DestFile);
      end;

      SourceFile := ChangeFileExt(SourceFile, '.cnt');
      if (DelphiNum >= 5) and InstallFile then
      begin
        if NeedCopyFile then
          DestFile := ChangeFileExt(DestFile, '.toc')
        else
          DestFile := ChangeFileExt(SourceFile, '.toc');
        DeleteFile(DestFile);
      end
      else if NeedCopyFile then
      begin
        DestFile := ChangeFileExt(DestFile, '.cnt');
        DeleteFile(DestFile);
      end;

      if InstallFile then
      begin
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey('Software\Microsoft\Windows\Help', true) then
        begin
          Reg.DeleteValue(FileName + '.hlp');
          Reg.CloseKey;
        end;
        Reg.RootKey := HKEY_CURRENT_USER;

        LinkStr := ':Link ' + FileName + '.hlp';
        if DelphiNum >= 5 then
          IncludeStr := ':Include ' + FileName + '.toc'
        else
          IncludeStr := ':Include ' + FileName + '.cnt';
        IndexStr := ':Index ' + HelpCaption + ' =' + FileName + '.hlp';

        try
          F.LoadFromFile(DelphiDir + 'Help\delphi' + DelphiNumStr +  LinkExt);
          idx := F.IndexOf(LinkStr);
          if idx >= 0 then
          begin
            F.Delete(idx);
            F.SaveToFile(DelphiDir + 'Help\delphi' + DelphiNumStr + LinkExt);
          end;

          F.Clear;
          F.LoadFromFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IncludeExt);
          idx := F.IndexOf(IncludeStr);
          if idx >= 0 then
          begin
            F.Delete(idx);
            F.SaveToFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IncludeExt);
          end;

          F.Clear;
          F.LoadFromFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IndexExt);
          idx := F.IndexOf(IndexStr);
          if idx >= 0 then
          begin
            F.Delete(idx);
            F.SaveToFile(DelphiDir + 'Help\delphi' + DelphiNumStr + IndexExt);
          end;

        except
           //ignore error if one of files not founded
        end;
      end;
    end;
    DestFile := DelphiDir + 'help\delphi' + DelphiNumStr + '.gid';
    DeleteFile(DestFile);
  finally
    F.Free;
  end;
*)
end;

//------------------------------------------------------------------------------
// Loads config file. Returns False if file not found.

function LoadConfigFile: Boolean;
begin
  Result := False;
  if not abfFileExistsW(ConfigFileName) then Exit;
  ConfigFile := TIniFile.Create(ConfigFileName);
  Result := True;
end;

//------------------------------------------------------------------------------
// Performs installation or uninstallation operations depends on AInstall flag

procedure DoWork(AInstall: Boolean);
var
  TempVersion: TabfCompilerVersion;
  TempProjectConfig: TabfProjectConfig;
  TempSearchPath: WideString;
  TempBrowsingPath: WideString;
  TempDefineSymbols: WideString;
  TempBPLOutputDir: WideString;
  TempDCPOutputDir: WideString;
  TempHPPOutputDir: WideString;
  TempOBJOutputDir: WideString;
  TempDCUOutputDir: WideString;
  TempSPath, TempBPath, TempDSymbols: WideString;
  SectionList, List: TStringList;
  S: WideString;
  i: Integer;

  //------------------------------------

  function _ReadString(const AName: WideString): WideString;
  var
    TempIndex: Integer;
  begin
    Result := '';
    TempIndex := List.IndexOfName(AName);
    if TempIndex < 0 then Exit;

    Result := abfTrimW(List.Values[AName], ' "');
    List.Delete(TempIndex);
  end;

  //------------------------------------

  function _ConcatStrings(const AStr1, AStr2: WideString): WideString;
  begin
    Result := AStr1;

    if Result = '' then
      Result := AStr2
    else if AStr2 <> '' then
      Result := abfAddCharW(Result, WideChar(';')) + AStr2;
  end;

  //------------------------------------

begin
  SectionList := TStringList.Create;
  try
    TempSearchPath := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cSearchPath, ''), ' "');
    TempBrowsingPath := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cBrowsingPath, ''), ' "');
    TempDefineSymbols := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cDefineSymbols, ''), ' "');
    TempBPLOutputDir := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cBPLOutputDir, ''), ' "');
    TempDCPOutputDir := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cDCPOutputDir, ''), ' "');
    TempHPPOutputDir := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cHPPOutputDir, ''), ' "');
    TempOBJOutputDir := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cOBJOutputDir, ''), ' "');
    TempDCUOutputDir := abfTrimW(ConfigFile.ReadString(cCommonSection,
      cDCUOutputDir, ''), ' "');

    ConfigFile.ReadSections(SectionList);

    SectionList.Delete(SectionList.IndexOf(cCommonSection));
    if SectionList.Count < 1 then Exit;

    List := TStringList.Create;
    with List do
    try
      for TempVersion := Low(TempVersion) to High(TempVersion) do
      if SectionList.IndexOf(cSections[TempVersion]) > -1 then
      begin
      // Packages or pas files
        ConfigFile.ReadSectionValues(cSections[TempVersion], List);

        TempSPath := _ConcatStrings(TempSearchPath, _ReadString(cSearchPath));
        TempBPath := _ConcatStrings(TempBrowsingPath,
          _ReadString(cBrowsingPath));
        TempDSymbols := _ConcatStrings(TempDefineSymbols,
          _ReadString(cDefineSymbols));

        S := _ReadString(cBPLOutputDir);
        if Trim(S) <> '' then
          TempBPLOutputDir := S;

        S := _ReadString(cDCPOutputDir);
        if Trim(S) <> '' then
          TempDCPOutputDir := S;

        S := _ReadString(cHPPOutputDir);
        if Trim(S) <> '' then
          TempHPPOutputDir := S;

        S := _ReadString(cOBJOutputDir);
        if Trim(S) <> '' then
          TempOBJOutputDir := S;

        S := _ReadString(cDCUOutputDir);
        if Trim(S) <> '' then
          TempDCUOutputDir := S;

        for i := 0 to Count - 1 do
        begin
          TempProjectConfig := TabfProjectConfig.Create(TempVersion);
          with TempProjectConfig do
          try
            FileName := List.Names[i];
            DefineSymbols := TempDSymbols;
            SearchPath := TempSPath;
            BPLOutputDir:= TempBPLOutputDir;
            DCPOutputDir := TempDCPOutputDir;
            HPPOutputDir := TempHPPOutputDir;
            OBJOutputDir := TempOBJOutputDir;
            DCUOutputDir := TempDCUOutputDir;

            if AInstall then
            begin
              if not BuildOnlyMode then
              begin
                abfAddSearchPathToIDE(CompilerConfig, TempSPath);
                abfAddSearchPathToIDE(CompilerConfig, DCUOutputDir);
                abfAddIncludePathToIDE(CompilerConfig, HPPOutputDir);
                abfAddBrowsingPathToIDE(CompilerConfig, TempBPath);
              end;

              InstallPackage(TempProjectConfig,
                ConfigFile.ReadString(cSections[TempVersion], List[i], ''))
            end else
            begin
              if not BuildOnlyMode then
              begin
                abfRemoveSearchPathFromIDE(CompilerConfig, TempSPath);
                abfRemoveSearchPathFromIDE(CompilerConfig, DCUOutputDir);
                abfRemoveIncludePathFromIDE(CompilerConfig, HPPOutputDir);
                abfRemoveBrowsingPathFromIDE(CompilerConfig, TempBPath);
              end;

              UninstallPackage(TempProjectConfig,
                ConfigFile.ReadString(cSections[TempVersion], List[i], ''));
            end;
          finally
            Free;
          end;
        end;

        if BuildOnlyMode then Continue;

      // Help files by versions
        ConfigFile.ReadSection(cSections[TempVersion] + cHelp, List);
        for i := 0 to Count - 1 do
          if AInstall then
            InstallHelp(TempVersion, abfSmartExpandRelativeFileName(List[i]),
              ConfigFile.ReadString(cSections[TempVersion], List[i], ''))
          else
            UninstallHelp(TempVersion, abfSmartExpandRelativeFileName(List[i]),
              ConfigFile.ReadString(cSections[TempVersion], List[i], ''));

      // Help files for all versions
        ConfigFile.ReadSection(cHelp, List);
        for i := 0 to Count - 1 do
          if AInstall then        
            InstallHelp(TempVersion, abfSmartExpandRelativeFileName(List[i]),
              ConfigFile.ReadString(cSections[TempVersion], List[i], ''))
          else
            UninstallHelp(TempVersion, abfSmartExpandRelativeFileName(List[i]),
              ConfigFile.ReadString(cSections[TempVersion], List[i], ''));
      end;
    finally
      List.Free;
    end;
  finally
    SectionList.Free;
  end;
end;

//------------------------------------------------------------------------------
// Exits program with option of waiting

procedure ExitProgram(AMsg: string; Wait: Boolean);
begin
  WriteLn(AMsg);
  if Wait then
  begin
    WriteLn;
    WriteLn('Press "Enter" to continue...');
    ReadLn;
  end;
  Halt(0);
end;

//------------------------------------------------------------------------------
// Loads command line parameters

procedure LoadCMDParams;
var
  WS: WideString;
  i: Integer;
begin
  if not Assigned(CMDParams) then Exit;
  CMDParams.Clear;

  for i := 0 to ParamCount do
  begin
    WS := abfTrimW(abfParamStrW(i), ' "');

    if WS <> '' then
      CMDParams.Add(WS);
  end;

  // Check parameters count
  if CMDParams.Count < 2 then
    ExitProgram(SMsgError + 'No enough parameters', False);

  for i := 1 to CMDParams.Count - 1 do
  begin
    if (Length(CMDParams[i]) > 1)
      and (CMDParams[i][1] in [WideChar('/'), WideChar('-')]) then
    begin
      case WideUpperCase(CMDParams[i])[2] of
        '?': ExitProgram(SMsgUsage, True);
        'U': UninstallMode := True;
        'B': BuildOnlyMode := True;
      end;
      Continue;
    end;
    
  // Read config file parameter
    if ConfigFileName = '' then
      ConfigFileName := abfSmartExpandRelativeFileNameW(CMDParams[i]);
  end;
end;

//------------------------------------------------------------------------------
// Main work routine

procedure Execute;
begin
  try
    WriteLn(SMsgAppInfo);
    LoadCMDParams;
    if not LoadConfigFile then
      ExitProgram(SMsgError + 'Config file not found', False);

    DoWork(not UninstallMode);

    ExitProgram(SMsgDone, False);
  except
    ExitProgram(CRLF + 'Fatal error: Maybe some files are locked by system.', False);
  end;
end;


{******************************************************************************}
initialization
{******************************************************************************}

  CMDParams := TabfWideStringList.Create;
  SAppPath := abfExtractFilePathW(abfParamStrW(0));
  SMsgUsage := SMsgUsage1 + SMsgUsage2;
  SMsgError := SMsgUsage + CRLF + 'Error: ';

{******************************************************************************}
finalization
{******************************************************************************}
  FreeAndNil(CMDParams);
  FreeAndNil(ConfigFile);

end.
