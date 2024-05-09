{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfIDEUtils;

{$I abf.inc}

interface

uses
  Windows, Classes,{$IFDEF D9} WideStrings,{$ENDIF}
// ABF VCL
  abfConsts, abfClasses;

//==============================================================================
// IDE related types
//==============================================================================

type
  TabfCompilerVersion = (cv_D2, cv_C1, cv_D3, cv_C3, cv_D4, cv_C4, cv_D5, cv_C5,
    cv_D6, cv_C6, cv_D7, cv_D8, cv_D9, cv_D10, cv_C10, cv_D11, cv_C11);
  TabfCompilerVersions = set of TabfCompilerVersion;

  TabfCompilerPersonality = (persDelphi, persDelphiNet, persBCB, persManagedCpp,
    persCSharp, persVisualBasic);
  TabfCompilerPersonalities = set of TabfCompilerPersonality;

//==============================================================================
// IDE related constants
//==============================================================================

const
  CabfIDERegPaths: array[TabfCompilerVersion] of string = (
    SabfRegSoftwareBorland + SabfDelphi  + '\2.0',
    SabfRegSoftwareBorland + SabfBuilder + '\1.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\3.0',
    SabfRegSoftwareBorland + SabfBuilder + '\3.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\4.0',
    SabfRegSoftwareBorland + SabfBuilder + '\4.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\5.0',
    SabfRegSoftwareBorland + SabfBuilder + '\5.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\6.0',
    SabfRegSoftwareBorland + SabfBuilder + '\6.0',
    SabfRegSoftwareBorland + SabfDelphi  + '\7.0',
    SabfRegSoftwareBorland + SabfBDS     + '\1.0',
    SabfRegSoftwareBorland + SabfBDS     + '\3.0',
    SabfRegSoftwareBorland + SabfBDS     + '\4.0',
    SabfRegSoftwareBorland + SabfBDS     + '\4.0',
    SabfRegSoftwareBorland + SabfBDS     + '\5.0',
    SabfRegSoftwareBorland + SabfBDS     + '\5.0'
  );

  CabfIDENames: array[TabfCompilerVersion] of string = (
    SabfDelphi,
    SabfBuilder,
    SabfDelphi,
    SabfBuilder,
    SabfDelphi,
    SabfBuilder,
    SabfDelphi,
    SabfBuilder,
    SabfDelphi,
    SabfBuilder,
    SabfDelphi,
    SabfDelphi,
    SabfDelphi,
    SabfDelphi,
    SabfBuilder,
    SabfDelphi,
    SabfBuilder
  );

  CabfPersNames: array[TabfCompilerPersonality] of string = (
    'Delphi.Win32', 'Delphi.NET', 'BCB', 'BCB.NET', 'C#Builder', 'VB.NET'
  );

//==============================================================================
// IDE related types
//==============================================================================

type
//------------------------------------------------------------------------------
//  TabfCompilerConfig
//------------------------------------------------------------------------------

  TabfCompilerConfig = class(TPersistent)
  private
    FEnvVars: TabfWideStringList;
    FVersion: TabfCompilerVersion;
    FIDEPath: WideString;
    FPersonalities: TabfCompilerPersonalities;
  protected
    function GetCompilerFileName(AIndex: Integer): WideString; virtual;
    function GetDefaultBPLOutputDir: WideString; virtual;
    function GetDefaultDCPOutputDir: WideString; virtual;
    function GetIDEVersion: Integer; virtual;
    procedure LoadEnvParams; virtual;
    procedure LoadPersonalities; virtual;
  public
    constructor Create(AVersion: TabfCompilerVersion); virtual;
    destructor Destroy; override;
  // Decodes paths with IDE variables or expands the relative paths
    function DecodePath(const APath: WideString): WideString; virtual;
  // Encodes paths to paths with IDE variables
    function EncodePath(const APath: WideString): WideString; virtual;
  // Returns True if it is Developer Studio
    function IsBDS: Boolean; virtual;
  // Tests if the compiler config supports specified personalities
    function SupportsPersonalities(APersonalities: TabfCompilerPersonalities;
      AExact: Boolean = False): Boolean; virtual;
  // Compiler's paths
    property DCC32: WideString index 0 read GetCompilerFileName;
    property Make: WideString index 1 read GetCompilerFileName;
    property Bpr2mak: WideString index 2 read GetCompilerFileName;
  // Default outpur directories
    property DefaultBPLOutputDir: WideString read GetDefaultBPLOutputDir;
    property DefaultDCPOutputDir: WideString read GetDefaultDCPOutputDir;
  // Environment variables for macros replacement
    property EnvVars: TabfWideStringList read FEnvVars;
  // Complier version and a root directory
    property Version: TabfCompilerVersion read FVersion;
    property IDEPath: WideString read FIDEPath;
    property IDEVersion: Integer read GetIDEVersion;    
  // Supported personalities. For non-BDS version returns persDelphi or persBCB
    property SupportedPersonalities: TabfCompilerPersonalities
      read FPersonalities default [];
  end;


//------------------------------------------------------------------------------
//  TabfProjectConfig
//------------------------------------------------------------------------------

  TabfProjectConfig = class(TPersistent)
  private
    FFileName: WideString;
    FCompilerConfig: TabfCompilerConfig;
    FDefineSymbols: WideString;
    FVersion: TabfCompilerVersion;
    FSearchPath: WideString;
    FBPLOutputDir: WideString;
    FDCPOutputDir: WideString;
    FDCUOutputDir: WideString;
    FOBJOutputDir: WideString;
    FHPPOutputDir: WideString;
  protected
    function GetFileName(AIndex: Integer): WideString; virtual;
    procedure SetFileName(AIndex: Integer; const AValue: WideString); virtual;
    function GetOutputDir(AIndex: Integer): WideString; virtual;
    function GetSearchPath: WideString; virtual;
  public
    constructor Create(AVersion: TabfCompilerVersion); virtual;
    destructor Destroy; override;
    property CompilerConfig: TabfCompilerConfig read FCompilerConfig;
    property DefineSymbols: WideString read FDefineSymbols write FDefineSymbols;
    property Version: TabfCompilerVersion read FVersion;
    property FileName: WideString index 0 read GetFileName
      write SetFileName;
    property OutputFileName: WideString index 1 read GetFileName;
    property SearchPath: WideString read GetSearchPath
      write FSearchPath;
    property BPLOutputDir: WideString index 0 read GetOutputDir
      write FBPLOutputDir;
    property DCPOutputDir: WideString index 1 read GetOutputDir
      write FDCPOutputDir;
    property DCUOutputDir: WideString index 2 read GetOutputDir
      write FDCUOutputDir;
    property OBJOutputDir: WideString index 3 read GetOutputDir
      write FOBJOutputDir;
    property HPPOutputDir: WideString index 4 read GetOutputDir
      write FHPPOutputDir;
  end;


//==============================================================================
// IDE related routines
//==============================================================================

//------------------------------------------------------------------------------
// Encodes paths that starts with any IDE variables.
function abfEncodeIDEPath(AVersion: TabfCompilerVersion;
  const APath: WideString): WideString;

//------------------------------------------------------------------------------
// Decodes paths with any IDE variables.
function abfDecodeIDEPath(AVersion: TabfCompilerVersion;
  const APath: WideString): WideString;

//------------------------------------------------------------------------------
// Returns the version number for specified Delphi/C++Builder IDE.
function abfGetIDEVersion(AVersion: TabfCompilerVersion): Integer;

//------------------------------------------------------------------------------
// Returns True if IDE is Developer Studio
function abfIsBDS(AVersion: TabfCompilerVersion): Boolean;

//------------------------------------------------------------------------------
// Returns True if IDE is Delphi.
function abfIsDelphi(AVersion: TabfCompilerVersion): Boolean;

//------------------------------------------------------------------------------
// Returns True if IDE is C++Builder.
function abfIsBCB(AVersion: TabfCompilerVersion): Boolean;

//------------------------------------------------------------------------------
// Returns the root dir for specified Delphi/C++Builder version.
function abfGetIDEDir(AVersion: TabfCompilerVersion): WideString;

//------------------------------------------------------------------------------
// Returns the help dir for specified Delphi/C++Builder version.
function abfGetIDEHelpDir(AVersion: TabfCompilerVersion): WideString;

//------------------------------------------------------------------------------
// Returns the BPL output dir for specified Delphi/C++Builder version.
function abfGetBPLOutputDir(AVersion: TabfCompilerVersion): WideString;

//------------------------------------------------------------------------------
// Returns the DCP output dir for specified Delphi/C++Builder version.
function abfGetDCPOutputDir(AVersion: TabfCompilerVersion): WideString;

//------------------------------------------------------------------------------
// Creates *.int files for SrcFile and place it at DstPath, if the
// "implementation" string were not found *.int file will be same to SrcFile
function abfCreateIntFile(const SrcFileName, DstPath: WideString): Boolean;

//------------------------------------------------------------------------------
// Compiles input file using specified version of Delphi/C++Builder. Supports
// PAS, DPK, BPK files. Rebuilds all used units if ARebuildAll is True.
function abfCompileFile(AProjectConfig: TabfProjectConfig;
  ARebuildAll: Boolean): WideString;

//------------------------------------------------------------------------------
// Returns flags of Borland package.
function abfGetPackageFlagsA(const AFileName: AnsiString): Cardinal;
function abfGetPackageFlags(const AFileName: string): Cardinal;
function abfGetPackageFlagsW(const AFileName: WideString): Cardinal;

//------------------------------------------------------------------------------
// Returns a description of Borland package.
function abfGetPackageDescriptionA(const AFileName: AnsiString): AnsiString;
function abfGetPackageDescription(const AFileName: string): string;
function abfGetPackageDescriptionW(const AFileName: WideString): WideString;

//------------------------------------------------------------------------------
// Uninstalls existed package in the Delphi/C++Builder IDE. If PackageName is
// empty search by file name will be used.
procedure abfUninstallPackage(AVersion: TabfCompilerVersion; const AFileName: WideString);

//------------------------------------------------------------------------------
// Installs existed package in the Delphi/C++Builder IDE.
procedure abfInstallPackage(AVersion: TabfCompilerVersion; const AFileName: WideString;
  const APackageName: WideString);

//------------------------------------------------------------------------------
// Removes specified Path from the Delphi/C++Builder IDE browsing paths.
procedure abfRemoveBrowsingPathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Removes specified Path from the C++Builder IDE search paths.
procedure abfRemoveIncludePathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Removes specified Path from the C++Builder IDE library paths.
procedure abfRemoveLibraryPathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Removes specified Path from the Delphi/C++Builder IDE search paths.
procedure abfRemoveSearchPathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Adds specified Path to the Delphi/C++Builder IDE browsing paths.
procedure abfAddBrowsingPathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Adds specified Path to the C++Builder IDE include paths.
procedure abfAddIncludePathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Adds specified Path to the C++Builder IDE library paths.
procedure abfAddLibraryPathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Adds specified Path to the Delphi/C++Builder IDE search paths.
procedure abfAddSearchPathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);

//------------------------------------------------------------------------------
// Removes a record that containes info about specified Help file from the
// given Cnt file.
procedure abfRemoveHelpFromCntFile(const AHelpFile, ACntFile: string);

//------------------------------------------------------------------------------
// Adds a record that containes info about specified Help file into the given
// Cnt file.
procedure abfAddHelpToCntFile(const AHelpFile, AHelpName, ACntFile: string);

{******************************************************************************}
implementation
{******************************************************************************}

uses
  Registry, SysUtils,
  abfShlUtils, abfStrUtils, abfSysUtils, abfVclUtils;

//------------------------------------------------------------------------------
// Private consts
//------------------------------------------------------------------------------

const
  SIndex   = ':Index ';
  SInclude = ':Include';
  SabfIDERegPath_Library               = '\Library\';
  SabfIDERegPath_Globals               = '\Globals\';
  SabfIDERegPath_Known_Packages        = '\Known Packages\';
  SabfIDERegPath_Disabled_Packages     = '\Disabled Packages\';
  SabfIDERegPath_Package_Cache         = '\Package Cache\';
  SabfIDERegPath_CppPaths              = '\CppPaths\';
  SabfIDERegPath_CPlusPlus_Paths       = '\C++\Paths\';
  SabfIDERegPath_Environment_Variables = '\Environment Variables\';
  SabfIDERegPath_Personalities         = '\Personalities\';

  SabfIDERegKey_Browsing_Path         = 'Browsing Path';
  SabfIDERegKey_BrowsingPath          = 'BrowsingPath';
  SabfIDERegKey_Search_Path           = 'Search Path';
  SabfIDERegKey_SearchPath            = 'SearchPath';
  SabfIDERegKey_IncludePath           = 'IncludePath';
  SabfIDERegKey_LibraryPath           = 'LibraryPath';

  SabfIDERegKey_Package_BPL_Output    = 'Package DPL Output';
  SabfIDERegKey_Package_DCP_Output    = 'Package DCP Output';
  SabfIDERegKey_BPIOutput             = 'BPIOutput';
  SabfIDERegKey_BPLOutput             = 'BPLOutput';
  SabfIDERegKey_ForceEnvOptionsUpdate = 'ForceEnvOptionsUpdate';

//------------------------------------------------------------------------------
// Private routines
//------------------------------------------------------------------------------

function _ExpandRelativePath(AVersion: TabfCompilerVersion;
  const APath: WideString): WideString;
var
  TempPathList: TabfWideStringList;
  TempPath: TabfWideString;
  i: Integer;
begin
  Result := '';

  TempPathList := TabfWideStringList.Create;
  with TempPathList do
  try
  {$IFDEF D9}
    abfParseStringW(abfTrimW(APath, ' "'), ';', TempPathList);

    for i := 0 to Count - 1 do
    begin
      TempPath := abfDecodeIDEPath(AVersion, TempPathList[i]);

      if TempPath = TempPathList[i] then
        TempPath := abfSmartExpandRelativePathW(TempPath);

      TempPathList[i] := abfRemoveSlashW(TempPath);
    end;

    Result := abfUnParseStringW(TempPathList, ';');
  {$ELSE}
    abfParseString(abfTrim(APath, [' ', '"']), ';', TempPathList);

    for i := 0 to Count - 1 do
    begin
      TempPath := abfDecodeIDEPath(AVersion, TempPathList[i]);

      if TempPath = TempPathList[i] then
        TempPath := abfSmartExpandRelativePath(TempPath);
        
      TempPathList[i] := abfRemoveSlash(TempPath);
    end;

    Result := abfUnParseString(TempPathList, ';');
  {$ENDIF}
  finally
    TempPathList.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure _RemovePath(AVersion: TabfCompilerVersion; const APath: WideString;
  var ASourcePath: WideString);
var
  TempSourcePathList, TempPathList: TabfWideStringList;
  TempSourcePath, TempPath: TabfWideString;
  i, a: Integer;
begin
  TempSourcePathList := TabfWideStringList.Create;
  try
    TempPathList := TabfWideStringList.Create;
    try
    {$IFDEF D9}
      abfParseStringW(abfTrimW(ASourcePath, ' "'), ';', TempSourcePathList);
      abfParseStringW(abfTrimW(APath, ' "'), ';', TempPathList);

      for i := 0 to TempPathList.Count - 1 do
      begin
        TempPath := abfDecodeIDEPath(AVersion, TempPathList[i]);

        if TempPath = TempPathList[i] then
          TempPath := abfSmartExpandRelativePathW(TempPath);

        TempPath := abfRemoveSlashW(TempPath);

        for a := TempSourcePathList.Count - 1 downto 0 do
        begin
          TempSourcePath := abfDecodeIDEPath(AVersion, TempSourcePathList[a]);

          if TempSourcePath = TempSourcePathList[a] then
            TempSourcePath := abfSmartExpandRelativePathW(TempSourcePath);

          TempSourcePath := abfRemoveSlashW(TempSourcePath);

          if WideCompareText(TempSourcePath, TempPath) = 0 then
            TempSourcePathList.Delete(a);
        end;
      end;

      ASourcePath := abfUnParseStringW(TempSourcePathList, ';');
    {$ELSE}
      abfParseString(abfTrim(ASourcePath, [' ', '"']), ';', TempSourcePathList);
      abfParseString(abfTrim(APath, [' ', '"']), ';', TempPathList);

      for i := 0 to TempPathList.Count - 1 do
      begin
        TempPath := abfDecodeIDEPath(AVersion, TempPathList[i]);

        if TempPath = TempPathList[i] then
          TempPath := abfSmartExpandRelativePath(TempPath);

        TempPath := abfRemoveSlash(TempPath);

        for a := TempSourcePathList.Count - 1 downto 0 do
        begin
          TempSourcePath := abfDecodeIDEPath(AVersion, TempSourcePathList[a]);

          if TempSourcePath = TempSourcePathList[a] then
            TempSourcePath := abfSmartExpandRelativePath(TempSourcePath);

          TempSourcePath := abfRemoveSlash(TempSourcePath);

          if AnsiCompareText(TempSourcePath, TempPath) = 0 then
            TempSourcePathList.Delete(a);
        end;
      end;

      ASourcePath := abfUnParseString(TempSourcePathList, ';');
    {$ENDIF}
    finally
      TempPathList.Free;
    end;
  finally
    TempSourcePathList.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure _AddPath(AVersion: TabfCompilerVersion; const APath: WideString;
  var ASourcePath: WideString);
var
  TempSourcePathList, TempPathList: TabfWideStringList;
  TempSourcePath, TempPath, ResultPath: TabfWideString;
  Found: Boolean;
  i, a: Integer;
begin
  ResultPath := '';
  TempSourcePathList := TabfWideStringList.Create;
  try
    TempPathList := TabfWideStringList.Create;
    try
    {$IFDEF D9}
      abfParseStringW(abfTrimW(ASourcePath, ' "'), ';', TempSourcePathList);
      abfParseStringW(abfTrimW(APath, ' "'), ';', TempPathList);

      for i := TempPathList.Count - 1 downto 0 do
      begin
        TempPath := abfDecodeIDEPath(AVersion, TempPathList[i]);

        if TempPath = TempPathList[i] then
           TempPath := abfSmartExpandRelativePathW(TempPath);

        TempPath := abfRemoveSlashW(TempPath);

        Found := False;

        for a := 0 to TempSourcePathList.Count - 1 do
        begin
          TempSourcePath := abfDecodeIDEPath(AVersion, TempSourcePathList[a]);

          if TempSourcePath = TempSourcePathList[a] then
            TempSourcePath := abfSmartExpandRelativePathW(TempSourcePath);

          TempSourcePath := abfRemoveSlashW(TempSourcePath);

          if WideCompareText(TempSourcePath, TempPath) = 0 then
          begin
            Found := True;
            Break;
          end;
        end;

        if not Found then
          TempSourcePathList.Insert(0, abfEncodeIDEPath(AVersion, TempPath));
      end;

      ASourcePath := abfUnParseStringW(TempSourcePathList, ';');
    {$ELSE}
      abfParseString(abfTrim(ASourcePath, [' ', '"']), ';', TempSourcePathList);
      abfParseString(abfTrim(APath, [' ', '"']), ';', TempPathList);

      for i := TempPathList.Count - 1 downto 0 do
      begin
        TempPath := abfDecodeIDEPath(AVersion, TempPathList[i]);

        if TempPath = TempPathList[i] then
           TempPath := abfSmartExpandRelativePath(TempPath);

        TempPath := abfRemoveSlash(TempPath);

        Found := False;

        for a := 0 to TempSourcePathList.Count - 1 do
        begin
          TempSourcePath := abfDecodeIDEPath(AVersion, TempSourcePathList[a]);

          if TempSourcePath = TempSourcePathList[a] then
            TempSourcePath := abfSmartExpandRelativePath(TempSourcePath);

          TempSourcePath := abfRemoveSlash(TempSourcePath);

          if AnsiCompareText(TempSourcePath, TempPath) = 0 then
          begin
            Found := True;
            Break;
          end;
        end;

        if not Found then
          TempSourcePathList.Insert(0, abfEncodeIDEPath(AVersion, TempPath));
      end;

      ASourcePath := abfUnParseString(TempSourcePathList, ';');
    {$ENDIF}
    finally
      TempPathList.Free;
    end;
  finally
    TempSourcePathList.Free;
  end;
end;

//------------------------------------------------------------------------------
//  TabfCompilerConfig
//------------------------------------------------------------------------------

constructor TabfCompilerConfig.Create(AVersion: TabfCompilerVersion);
begin
  inherited Create;

  FVersion := AVersion;
  FEnvVars := TabfWideStringList.Create;
  FIDEPath := abfGetIDEDir(FVersion);
  FPersonalities := [];

  LoadPersonalities; // LoadPersonalities must be before LoadEnvParams!!!
  LoadEnvParams;
end;

//------------------------------------------------------------------------------

destructor TabfCompilerConfig.Destroy;
begin
  FreeAndNil(FEnvVars);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.GetCompilerFileName(AIndex: Integer): WideString;
begin
  case AIndex of
    0: Result := abfAddSlashW(FIDEPath) + 'Bin\dcc32.exe';
    1: Result := abfAddSlashW(FIDEPath) + 'Bin\make.exe';
    2: Result := abfAddSlashW(FIDEPath) + 'Bin\bpr2mak.exe';
  else
    Result := '';
  end;

  if not abfFileExistsW(Result) then
    Result := ''
  else
    Result := abfEncloseStringW(Result, '"');
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.GetDefaultBPLOutputDir: WideString;
var
  TempKey, TempName: string;
begin
  Result := '';

  TempKey := CabfIDERegPaths[FVersion] + SabfIDERegPath_Library;
  TempName := SabfIDERegKey_Package_BPL_Output;

  if IsBDS and SupportsPersonalities([persBCB]) then
  begin
    if IDEVersion >= 5 then
      TempKey := CabfIDERegPaths[FVersion] + SabfIDERegPath_CPlusPlus_Paths else
    if IDEVersion = 4 then
      TempKey := CabfIDERegPaths[FVersion] + SabfIDERegPath_CppPaths;

    TempName := SabfIDERegKey_BPLOutput;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(TempKey, False) then
    begin
      Result := DecodePath(ReadString(TempName));
    end;
  finally
    CloseKey;
    Free;
  end;
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.GetDefaultDCPOutputDir: WideString;
var
  TempKey, TempName: string;
begin
  Result := '';

  TempKey := CabfIDERegPaths[FVersion] + SabfIDERegPath_Library;
  TempName := SabfIDERegKey_Package_DCP_Output;

  if IsBDS and SupportsPersonalities([persBCB]) then
  begin
    if IDEVersion >= 5 then
      TempKey := CabfIDERegPaths[FVersion] + SabfIDERegPath_CPlusPlus_Paths else
    if IDEVersion = 4 then
      TempKey := CabfIDERegPaths[FVersion] + SabfIDERegPath_CppPaths;

    TempName := SabfIDERegKey_BPIOutput;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(TempKey, False) then
    begin
      Result := DecodePath(ReadString(TempName));
    end;
  finally
    CloseKey;
    Free;
  end;
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.GetIDEVersion: Integer;
var
  TempS: AnsiString;
  i: Integer;
begin
  TempS := CabfIDERegPaths[FVersion];

  Delete(TempS, 1, abfBackPosA('\', TempS));

  for i := 1 to Length(TempS) do
    if not (TempS[i] in ['0'..'9']) then
    begin
      SetLength(TempS, i - 1);
      Break;
    end;

  Result := StrToIntDef(TempS, 0);
end;

//------------------------------------------------------------------------------

procedure TabfCompilerConfig.LoadEnvParams;
var
  TempProjectsDir, TempCommonProjectsDir: WideString;
  TempWS: string;
  TempReg: TRegistry;
  TempList: TStringList;
  i: Integer;
begin
  if not Assigned(FEnvVars) then Exit;

  if not IsBDS then
  begin
    if abfIsDelphi(FVersion) then
      FEnvVars.Values['DELPHI'] := abfRemoveSlashW(FIDEPath) else
    if abfIsBCB(FVersion) then
      FEnvVars.Values['BCB'] := abfRemoveSlashW(FIDEPath);

    Exit;
  end;

  FEnvVars.Values['BDS'] := abfRemoveSlashW(FIDEPath);

  if IDEVersion >= 3 then
  begin
    if IDEVersion >= 5 then
      TempWS := 'RAD Studio\Projects'
    else
      TempWS := 'Borland Studio Projects';

    TempProjectsDir := abfAddSlashW(abfGetPersonalDirW) + TempWS;
  end else
    TempProjectsDir := abfAddSlashW(FIDEPath) + 'Projects';

  TempWS := 'RAD Studio';

  if IDEVersion >= 5 then
    TempCommonProjectsDir := abfAddSlashW(abfGetCommonDocumentsDirW) +
      abfAddSlashW(TempWS) + Format('%d.0', [abfGetIDEVersion(FVersion)])
  else
    TempCommonProjectsDir := TempProjectsDir;

  FEnvVars.Values['BDSPROJECTSDIR'] := abfRemoveSlashW(TempProjectsDir);
  FEnvVars.Values['BDSCOMMONDIR'] := abfRemoveSlashW(TempCommonProjectsDir);

  if IDEVersion >= 3 then
  begin
    TempReg := TRegistry.Create;
    try
      TempReg.RootKey := HKEY_CURRENT_USER;
      if TempReg.OpenKeyReadOnly(CabfIDERegPaths[FVersion] +
        SabfIDERegPath_Environment_Variables) then
      begin
        TempList := TStringList.Create;
        try
          TempReg.GetValueNames(TempList);
          for i := 0 to TempList.Count - 1 do
            FEnvVars.Values[UpperCase(TempList[i])] :=
              abfRemoveSlashW(TempReg.ReadString(TempList[i]));
        finally
          TempList.Free;
        end;
        TempReg.CloseKey;
      end;
    finally
      TempReg.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCompilerConfig.LoadPersonalities;
var
  TempReg: TRegistry;
  TempList: TStringList;
  TempPers: TabfCompilerPersonality;
begin
  FPersonalities := [];

  if not IsBDS then
  begin
    if abfIsDelphi(FVersion) then
      FPersonalities := [persDelphi]
    else
    if abfIsBCB((FVersion)) then
      FPersonalities := [persBCB];

    Exit;
  end;

  TempReg := TRegistry.Create;
  try
    TempReg.RootKey := HKEY_CURRENT_USER;

    if TempReg.OpenKeyReadOnly(CabfIDERegPaths[FVersion] +
      SabfIDERegPath_Personalities) then
    begin
      TempList := TStringList.Create;
      try
        TempReg.GetValueNames(TempList);

        for TempPers := Low(TempPers) to
          High(TempPers) do
          if TempList.IndexOf(CabfPersNames[TempPers]) >= 0 then
            Include(FPersonalities, TempPers);
      finally
        TempList.Free;
      end;
      TempReg.CloseKey;
    end;
  finally
    TempReg.Free;
  end;
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.DecodePath(const APath: WideString): WideString;
var
  TempPathList: TabfWideStringList;
  TempPath: TabfWideString;
  TempWS, TempNewWS: WideString;
  i, TempPos: Integer;
begin
  Result := APath;
  if not Assigned(FEnvVars) then Exit;

  i := 1;
  while i < Length(Result) do
  begin
    if (Result[i] = '$') and (Result[i + 1] = '(') then
    begin
      TempPos := i + 2;

      while (TempPos <= Length(Result)) and (Result[TempPos] <> ')') do
        Inc(TempPos);

      TempWS := WideUpperCase(Copy(Result, i + 2, TempPos - i - 2));
      if TempWS = '' then Continue;

      TempNewWS := TempWS;

      if FEnvVars.IndexOfName(TempWS) >= 0 then
        TempNewWS := FEnvVars.Values[TempWS]
      else
        TempNewWS := abfGetEnvVarW(TempWS);

      if TempNewWS <> TempWS then
      begin
        Delete(Result, i, TempPos - i + 1);
        Insert(TempNewWS, Result, i);
        Inc(i, Length(TempNewWS) - 1);
        TempNewWS := '';
      end;
    end;
    Inc(i);
  end;

  TempPathList := TabfWideStringList.Create;
  try
  {$IFDEF D9}
    abfParseStringW(abfTrimW(Result, ' "'), ';', TempPathList);

    for i := TempPathList.Count - 1 downto 0 do
    begin
      TempPath := abfTrimW(TempPathList[i], ' "');
      if TempPath = '' then
      begin
        TempPathList.Delete(i);
        Continue;
      end;

      TempPathList[i] := abfRemoveSlashW(abfSmartExpandRelativePathW(TempPath));
    end;

    Result := abfUnParseStringW(TempPathList, ';');
  {$ELSE}
    abfParseString(abfTrim(Result, [' ', '"']), ';', TempPathList);

    for i := TempPathList.Count - 1 downto 0 do
    begin
      TempPath := abfTrim(TempPathList[i],  [' ', '"']);
      if TempPath = '' then
      begin
        TempPathList.Delete(i);
        Continue;
      end;

      TempPath := abfRemoveSlash(abfSmartExpandRelativePath(TempPath));
    end;

    Result := abfUnParseString(TempPathList, ';');
  {$ENDIF}
  finally
    TempPathList.Free;
  end;
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.EncodePath(const APath: WideString): WideString;
var
  TempPathList: TabfWideStringList;
  TempPath, TempValue: TabfWideString;
  i, a: Integer;
begin
  Result := APath;
  if not Assigned(FEnvVars) then Exit;

  TempPathList := TabfWideStringList.Create;
  try
  {$IFDEF D9}
    abfParseStringW(abfTrimW(APath, ' "'), ';', TempPathList);

    for i := TempPathList.Count - 1 downto 0 do
    begin
      TempPath := abfTrimW(TempPathList[i], ' "');
      if TempPath = '' then
      begin
        TempPathList.Delete(i);
        Continue;
      end;

      TempPath := abfRemoveSlashW(abfSmartExpandRelativePathW(TempPath));

      for a := 0 to FEnvVars.Count - 1 do
      begin
        TempValue := FEnvVars.ValueFromIndex[a];
        if abfPosIgnoreCaseW(TempValue, TempPath) = 1 then
        begin
          Delete(TempPath, 1, Length(TempValue));
          TempPathList[i] := '$(' + FEnvVars.Names[a] + ')' + TempPath;
        end;
      end;  
    end;

    Result := abfUnParseStringW(TempPathList, ';');
  {$ELSE}
    abfParseString(abfTrim(APath, [' ', '"']), ';', TempPathList);

    for i := TempPathList.Count - 1 downto 0 do
    begin
      TempPath := abfTrim(TempPathList[i],  [' ', '"']);
      if TempPath = '' then
      begin
        TempPathList.Delete(i);
        Continue;
      end;

      TempPath := abfRemoveSlash(abfSmartExpandRelativePath(TempPath));

      for a := 0 to FEnvVars.Count - 1 do
      begin
        TempValue := FEnvVars.Values[FEnvVars.Names[a]];
        if abfPosIgnoreCase(TempValue, TempPath) = 1 then
        begin
          Delete(TempPath, 1, Length(TempValue));
          TempPathList[i] := '$(' + FEnvVars.Names[a] + ')' + TempPath;
        end;
      end;
    end;

    Result := abfUnParseString(TempPathList, ';');
  {$ENDIF}
  finally
    TempPathList.Free;
  end;
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.IsBDS: Boolean;
begin
  Result := FVersion > cv_D8;
end;

//------------------------------------------------------------------------------

function TabfCompilerConfig.SupportsPersonalities(
  APersonalities: TabfCompilerPersonalities; AExact: Boolean = False): Boolean;
begin
  if AExact then
    Result := SupportedPersonalities = APersonalities
  else
    Result := SupportedPersonalities * APersonalities = APersonalities;
end;

//------------------------------------------------------------------------------
//  TabfProjectConfig
//------------------------------------------------------------------------------

constructor TabfProjectConfig.Create(AVersion: TabfCompilerVersion);
begin
  inherited Create;

  FVersion := AVersion;
  FCompilerConfig := TabfCompilerConfig.Create(FVersion);
end;

//------------------------------------------------------------------------------

destructor TabfProjectConfig.Destroy;
begin
  FreeAndNil(FCompilerConfig);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfProjectConfig.GetFileName(AIndex: Integer): WideString;
var
  TempFilePath, TempFileName, TempFileExt: WideString;
begin
  Result := '';
  if not Assigned(FCompilerConfig) then Exit;

  case AIndex of
    0: Result := FFileName;
    1: begin
         TempFilePath := abfExtractFilePathW(FileName);
         TempFileName := abfExtractFileNameW(FileName);
         TempFileExt := WideLowerCase(abfExtractFileExtW(FileName));

         if TempFileExt = SabfExt_Pas then
           TempFileExt := SabfExt_Dcu else
         if TempFileExt = SabfExt_Dpr then
           TempFileExt := SabfExt_Exe else
         if Version = cv_D3 then
           TempFileExt := SabfExt_Dpl
         else
           TempFileExt := SabfExt_Bpl;

         TempFileName := abfChangeFileExtW(TempFileName, TempFileExt);

         Result := abfAddSlashW(BPLOutputDir) + TempFileName;
         if abfFileExistsW(Result) then Exit;

         Result := abfAddSlashW(FCompilerConfig.DefaultBPLOutputDir) +
           TempFileName;
         if abfFileExistsW(Result) then Exit;

         Result := abfAddSlashW(TempFilePath) + TempFileName;
         if abfFileExistsW(Result) then Exit;

         Result := ''
       end;
  else
    Result := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TabfProjectConfig.SetFileName(AIndex: Integer; const AValue: WideString);
begin
  case AIndex of
    0: FFileName := abfExpandRelativeFileNameW(abfTrimW(AValue, ' "'));
  end;
end;

//------------------------------------------------------------------------------

function TabfProjectConfig.GetOutputDir(AIndex: Integer): WideString;
begin
  Result := '';
  if not Assigned(FCompilerConfig) then Exit;

  case AIndex of
    0: begin
         Result := FBPLOutputDir;

         if Result = '' then
           Result := FCompilerConfig.DefaultBPLOutputDir;

         Result := FCompilerConfig.DecodePath(Result);
       end;
    1: begin
         Result := FDCPOutputDir;

         if Result = '' then
           Result := FCompilerConfig.DefaultDCPOutputDir;

         Result := FCompilerConfig.DecodePath(Result);
       end;
    2: Result := FCompilerConfig.DecodePath(FDCUOutputDir);
    3: Result := FCompilerConfig.DecodePath(FOBJOutputDir);
    4: Result := FCompilerConfig.DecodePath(FHPPOutputDir);
  else
    Result := '';
  end;
end;

//------------------------------------------------------------------------------

function TabfProjectConfig.GetSearchPath: WideString;
begin
  Result := '';
  if not Assigned(FCompilerConfig) then Exit;

  Result := FCompilerConfig.DecodePath(FSearchPath);
end;

//------------------------------------------------------------------------------
// Encodes paths that starts with any IDE variables.
// Date: 05/29/2007

function abfEncodeIDEPath(AVersion: TabfCompilerVersion;
  const APath: WideString): WideString;
var
  TempCompilerConfig: TabfCompilerConfig;
begin
  Result := APath;

  TempCompilerConfig := TabfCompilerConfig.Create(AVersion);
  try
    Result := TempCompilerConfig.EncodePath(Result);
  finally
    TempCompilerConfig.Free;
  end;
end;

//------------------------------------------------------------------------------
// Decodes paths with any IDE variables.
// Date: 05/29/2007

function abfDecodeIDEPath(AVersion: TabfCompilerVersion;
  const APath: WideString): WideString;
var
  TempCompilerConfig: TabfCompilerConfig;
begin
  Result := APath;

  TempCompilerConfig := TabfCompilerConfig.Create(AVersion);
  try
    Result := TempCompilerConfig.DecodePath(Result);
  finally
    TempCompilerConfig.Free;
  end;
end;

//------------------------------------------------------------------------------
// Returns the version number for specified Delphi/C++Builder IDE.
// Date: 06/28/2007

function abfGetIDEVersion(AVersion: TabfCompilerVersion): Integer;
var
  TempS: AnsiString;
  i: Integer;
begin
  TempS := CabfIDERegPaths[AVersion];

  Delete(TempS, 1, abfBackPosA('\', TempS));

  for i := 1 to Length(TempS) do
    if not (TempS[i] in ['0'..'9']) then
    begin
      SetLength(TempS, i - 1);
      Break;
    end;

  Result := StrToIntDef(TempS, 0);
end;

//------------------------------------------------------------------------------
// Returns True if the IDE is BDS.

function abfIsBDS(AVersion: TabfCompilerVersion): Boolean;
begin
  Result := AVersion > cv_D8;
end;

//------------------------------------------------------------------------------
// Returns True if the IDE is Delphi.

function abfIsDelphi(AVersion: TabfCompilerVersion): Boolean;
begin
  Result := (CabfIDENames[AVersion] = SabfDelphi);
end;

//------------------------------------------------------------------------------
// Returns True if the IDE is C++Builder.

function abfIsBCB(AVersion: TabfCompilerVersion): Boolean;
begin
  Result := (CabfIDENames[AVersion] = SabfBuilder);
end;

//------------------------------------------------------------------------------
// Returns the root dir for specified Delphi/C++Builder version.
// Date: 02/29/2007
// Converted to Unicode version: 06/13/2007

function abfGetIDEDir(AVersion: TabfCompilerVersion): WideString;
var
  R: TRegistry;
  RegKey: string;
begin
  RegKey := CabfIDERegPaths[AVersion];
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.Openkey(RegKey, False) then
      Result := abfRemoveSlashW(R.ReadString('RootDir'))
    else
      Result := '';
  finally
    R.Free;
  end;
end;

//------------------------------------------------------------------------------
// Returns the help dir for specified Delphi/C++Builder version.
// Date: 02/23/2000

function abfGetIDEHelpDir(AVersion: TabfCompilerVersion): WideString;
begin
  Result := abfAddSlashW(abfGetIDEDir(AVersion)) + 'HELP\';
end;

//------------------------------------------------------------------------------
// Returns the BPL output dir for specified Delphi/C++Builder version.
// Date: 06/26/2007

function abfGetBPLOutputDir(AVersion: TabfCompilerVersion): WideString;
var
  TempCompilerConfig: TabfCompilerConfig;
begin
  TempCompilerConfig := TabfCompilerConfig.Create(AVersion);
  try
    Result := TempCompilerConfig.DefaultBPLOutputDir;
  finally
    TempCompilerConfig.Free;
  end;
end;

//------------------------------------------------------------------------------
// Returns the DCP output dir for specified Delphi/C++Builder version.
// Date: 06/26/2007

function abfGetDCPOutputDir(AVersion: TabfCompilerVersion): WideString;
var
  TempCompilerConfig: TabfCompilerConfig;
begin
  TempCompilerConfig := TabfCompilerConfig.Create(AVersion);
  try
    Result := TempCompilerConfig.DefaultDCPOutputDir;
  finally
    TempCompilerConfig.Free;
  end;
end;

//------------------------------------------------------------------------------
// Creates *.int files for SrcFile and place it at DstPath, if the
// "implementation" string were not found *.int file will be same to SrcFile
// Date: 07/23/2008

function abfCreateIntFile(const SrcFileName, DstPath: WideString): Boolean;
const
  SImplementation = 'IMPLEMENTATION';
var
  FileContent: TabfAnsiStringList;
  i, LineNumber, Position: Integer;
begin
  Result := abfFileExistsW(SrcFileName) and abfDirectoryExistsW(DstPath);
  if not Result then Exit;

  FileContent := TabfAnsiStringList.Create;
  try
    FileContent.LoadFromFile(SrcFileName);
    Position := 0;
    LineNumber := -1;
    
    for i := 0 to FileContent.Count - 1 do
    begin
      Position := Pos(SImplementation, AnsiUpperCase(FileContent[i]));
      if Position > 0 then
      begin
        LineNumber := i;
        Break;
      end;
    end;

    if Position > 0 then
    begin
      for i := FileContent.Count - 1 downto LineNumber + 1 do
        FileContent.Delete(i);

      FileContent[LineNumber] := Copy(FileContent[LineNumber], 1,
        Position + Length(SImplementation));
    end;

    FileContent.SaveToFile(abfAddSlashW(DstPath) +
      abfChangeFileExtW(abfExtractFileNameW(SrcFileName), SabfExt_Int));

    Result := True;
  finally
    FileContent.Free;
  end;
end;

//------------------------------------------------------------------------------
// Compiles input file using specified version of Delphi/C++Builder. Supports
// PAS, DPK, BPK files. Rebuilds all used units if ARebuildAll is True.
// Date: 05/29/2007
// Converted to Unicode version: 06/13/2007

function abfCompileFile(AProjectConfig: TabfProjectConfig;
  ARebuildAll: Boolean): WideString;
var
  TempNewDir, TempOldDir, TempDir, SaveTempDir: WideString;
  TempBPLOutputDir, TempDCPOutputDir: WideString;
  TempFilePath, TempFileName, TempFileExt: WideString;
  TempMakFile, TempBmkFile: WideString;
  TempDeleteFilesList: TabfWideStringList;
  TempList: TabfWideStringList;
  Compiler, Params, S: WideString;
  i: Integer;

  //------------------------------------

  procedure _AddSearchPath;
  var
    TempS: WideString;
  begin
    TempS := AProjectConfig.SearchPath;
    if TempS <> '' then
      Params := Params + ' -I' + abfEncloseStringW(TempS, '"');
  end;

  //------------------------------------

  function _UpdateUserDefines(AMakFileName: WideString;
    const AUserDefines: WideString): Boolean;
  var
    TempStream: TabfFileStream;
    TempList: TStringList;
    TempS, TempCurrS, TempPrevS: string;
    TempUpdated: Boolean;
    i: Integer;
  begin
    Result := False;
    AMakFileName := abfTrimW(AMakFileName, ' "');

    if not abfFileExistsW(AMakFileName) then Exit;

    TempList := TStringList.Create;
    try
      TempStream := TabfFileStream.Create(AMakFileName,
        fmOpenRead or fmShareDenyWrite);
      try
        TempList.LoadFromStream(TempStream);
      finally
        TempStream.Free;
      end;

      TempUpdated := False;
      TempCurrS := '';
      i := 0;
      while i < TempList.Count do
      begin
        TempPrevS := TempCurrS;
        TempCurrS := Trim(TempList[i]);

        if TempUpdated then
        begin
          if Copy(TempCurrS, Length(TempCurrS) - 2, 2) = ' \' then
          begin
            TempList.Delete(i);
            Continue;
          end else
            Break;
        end;

        if (Copy(TempPrevS, Length(TempPrevS) - 2, 2) <> ' \') and
          (Pos('USERDEFINES', TempCurrS) = 1) then
        begin
          TempS := Trim(Copy(TempCurrS, Length('USERDEFINES') + 1, MaxInt));

          if (TempS <> '') and (TempS[1] = '=') then
          begin
            TempS := TempList[i];

            TempList[i] := Copy(TempS, 1, Pos('USERDEFINES', TempS) - 1) +
              'USERDEFINES = ' + AUserDefines;
            TempUpdated := True;
          end;
        end;

        Inc(i);
      end;

      TempStream := TabfFileStream.Create(AMakFileName, fmCreate);
      try
        TempList.SaveToStream(TempStream);
      finally
        TempStream.Free;
      end;
    finally
      TempList.Free;
    end;
  end;

  //------------------------------------
  
begin
  Result := '';

  if not (Assigned(AProjectConfig)
    and Assigned(AProjectConfig.CompilerConfig)) then Exit;

  if not abfFileExistsW(AProjectConfig.FileName) then Exit;

  TempFileExt := WideLowerCase(abfExtractFileExtW(AProjectConfig.FileName));
  if (TempFileExt = SabfExt_Dpl) or (TempFileExt = SabfExt_Bpl) or
    (TempFileExt = SabfExt_Dcu) then Exit;

  abfDeleteFileW(AProjectConfig.OutputFileName);

  TempFilePath := TempBPLOutputDir;
  TempFileName := AProjectConfig.FileName;

  TempBPLOutputDir := AProjectConfig.BPLOutputDir;
  TempDCPOutputDir := AProjectConfig.DCPOutputDir;

  Compiler := AProjectConfig.CompilerConfig.DCC32;
  Params := '';

  TempDir := abfGetUniqueTempDirW('abf');

  SaveTempDir := TempDir;

// Set current directory to compiling file directory
  TempNewDir := abfExtractFilePathW(AProjectConfig.FileName);
  TempOldDir := abfGetCurrentDirectoryW;
  abfSetCurrentDirectoryW(TempNewDir);
  try
    _AddSearchPath;

    if ARebuildAll then
      Params := Params + ' -B '
    else
      Params := Params + ' -M ';

    TempDeleteFilesList := TabfWideStringList.Create;
    try
      case AProjectConfig.Version of
        cv_C1: Params := Params + ' -JPHN ' + abfEncloseStringW(TempFileName, '"');
        cv_C3, cv_C4: begin
          Compiler := AProjectConfig.CompilerConfig.Make;
          Params := Params + ' -f' + abfEncloseStringW(TempFileName, '"');
        end;
        cv_C5, cv_C6: begin
          if (TempFileExt = SabfExt_Bpr) or (TempFileExt = SabfExt_Bpk) then
          begin
            if AProjectConfig.CompilerConfig.Bpr2Mak = '' then Exit;

            TempDir := TempNewDir;
            Compiler := AProjectConfig.CompilerConfig.Make;

            TempBmkFile := abfEncloseStringW(abfChangeFileExtW(AProjectConfig.FileName,
              SabfExt_Bmk), '"');

            TempMakFile := abfEncloseStringW(abfChangeFileExtW(AProjectConfig.FileName,
              SabfExt_Mak), '"');

            S := '';

            // use a template file if it exists
            if abfFileExistsW(TempBmkFile) then
              S := S + ' -t' + TempBmkFile;

            S := S + ' -o' + TempMakFile;

            // Convert to MAK file
            if abfRunExW(AProjectConfig.CompilerConfig.Bpr2Mak + S + ' ' +
              abfEncloseStringW(TempFileName, '"'), True, False) <> 0 then
              Exit;

            _UpdateUserDefines(TempMakFile, AProjectConfig.DefineSymbols);

            TempDeleteFilesList.Add(TempMakFile);

            Params := ' -l+ ' + Params + ' -f' + TempMakFile;
          end else
          begin
            TempDeleteFilesList.Add(abfChangeFileExtW(AProjectConfig.FileName,
              SabfExt_Lsp));

            S := abfEncloseStringW(TempDir, '"');

            Params := Params + ' -LE' + S + ' -LN' + S +
              ' -N0' + S + ' -NH' + S + ' -NO' + S + ' -NB' + S +
              ' -U' + abfEncloseStringW(TempDCPOutputDir, '"') +
              ' ' + abfEncloseStringW(TempFileName, '"');

            if AProjectConfig.DefineSymbols <> '' then
              Params := Params +  ' -D' +
              abfEncloseStringW(AProjectConfig.DefineSymbols, '"');

            if abfRunExW(Compiler + ' -JPHNE --BCB ' + ' ' + Params,
              True, False) <> 0 then Exit;

            Params := ' -JL ' + Params;
          end;
        end;
      else
        begin
          TempDeleteFilesList.Add(abfChangeFileExtW(AProjectConfig.FileName,
            SabfExt_Lsp));

          S := abfEncloseStringW(TempDir, '"');

          Params := Params + ' -LE' + S + ' -LN' + S +
            ' -N0' + S + ' -NH' + S + ' -NO' + S + ' -NB' + S +
            ' -U' + abfEncloseStringW(TempDCPOutputDir, '"') +
            ' ' + abfEncloseStringW(TempFileName, '"');

          if AProjectConfig.DefineSymbols <> '' then
            Params := Params +  ' -D' +
            abfEncloseStringW(AProjectConfig.DefineSymbols, '"');

          if AProjectConfig.CompilerConfig.SupportsPersonalities([persBCB]) then
          begin
            if abfRunExW(Compiler + ' -JPHNE --BCB ' + ' ' + Params,
              True, False) <> 0 then Exit;

            Params := ' -JL ' + Params;
          end;
        end;
      end;{case AProjectConfig.IDEVer of}

      S := Compiler + ' ' + Params;

      abfTrace('OldDir:' + TempOldDir);
      abfTrace('NewDir:' + TempNewDir);

      abfTrace('Run Compiler:' + #13#10 + S);
      abfTrace('HPPOutputDir:' + AProjectConfig.HPPOutputDir);
      abfTrace('OBJOutputDir:' + AProjectConfig.OBJOutputDir);
      abfTrace('DCUOutputDir:' + AProjectConfig.DCUOutputDir);
      abfTrace('DCPOutputDir:' + TempDCPOutputDir);
      abfTrace('BPLOutputDir:' + TempBPLOutputDir);

    // Run Compiler
      if abfRunExW(S, True, False) = 0 then
      begin
        TempList := TabfWideStringList.Create;
        try
          TempList.Clear;
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Hpp);
          abfMoveFilesToDir(TempList, abfAddSlashW(AProjectConfig.HPPOutputDir));

          TempList.Clear;
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Obj);
          abfMoveFilesToDir(TempList, abfAddSlashW(AProjectConfig.OBJOutputDir));

          TempList.Clear;
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Dcu);
          abfMoveFilesToDir(TempList, abfAddSlashW(AProjectConfig.DCUOutputDir));

          TempList.Clear;
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Lib);
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Bpi);
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Dcp);
          abfMoveFilesToDir(TempList, abfAddSlashW(TempDCPOutputDir));


          TempList.Clear;
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Bpl);
          TempList.Add(abfAddSlashW(TempDir) + '*' + SabfExt_Tds);
          abfMoveFilesToDir(TempList, abfAddSlashW(TempBPLOutputDir));
        finally
          TempList.Free;
        end;

        Result := AProjectConfig.OutputFileName;
      end;
    finally
      abfRemoveDir(SaveTempDir);

      for i := 0 to TempDeleteFilesList.Count - 1 do
        abfDeleteFileW(TempDeleteFilesList[i]);

      TempDeleteFilesList.Free;
    end;
  finally
    abfSetCurrentDirectoryW(TempOldDir);
  end;
end;{function abfCompileFile}

//------------------------------------------------------------------------------
// Returns flags of Borland package
// Date: 06/25/2007

{ Package flags:
  bit     meaning
  -----------------------------------------------------------------------------------------
  0     | 1: never-build                  0: always build
  1     | 1: design-time only             0: not design-time only      on => bit 2 = off
  2     | 1: run-time only                0: not run-time only         on => bit 1 = off
  3     | 1: do not check for dup units   0: perform normal dup unit check
  4..25 | reserved
  26..27| (producer) 0: pre-V4, 1: undefined, 2: c++, 3: Pascal
  28..29| reserved
  30..31| 0: EXE, 1: Package DLL, 2: Library DLL, 3: undefined
}

function abfGetPackageFlagsA(const AFileName: AnsiString): Cardinal;
var
  ResModule: HMODULE;
  ResInfo: HRSRC;
  ResData: HGLOBAL;
  NeedFreeLibrary: Boolean;
  TempInfo: ^Cardinal;
begin
  Result := 0;
  NeedFreeLibrary := True;

  ResModule := GetModuleHandleA(PAnsiChar(AFileName));
  if ResModule = 0 then
    ResModule := LoadLibraryExA(PAnsiChar(AFileName), 0, LOAD_LIBRARY_AS_DATAFILE)
  else
    NeedFreeLibrary := False;

  if ResModule = 0 then Exit;

  try
    ResInfo := FindResource(ResModule, 'PACKAGEINFO', RT_RCDATA);
    if ResInfo <> 0 then
    begin
      ResData := LoadResource(ResModule, ResInfo);
      if ResData <> 0 then
      try
        TempInfo := LockResource(ResData);
        UnlockResource(ResData);

        Result := TempInfo^;
      finally
        FreeResource(ResData);
      end;
    end;
  finally
    if NeedFreeLibrary then
      FreeLibrary(ResModule);
  end;
end;

//------------------------------------------------------------------------------

function abfGetPackageFlags(const AFileName: string): Cardinal;
begin
  Result := abfGetPackageFlagsA(AFileName);
end;

//------------------------------------------------------------------------------

function abfGetPackageFlagsW(const AFileName: WideString): Cardinal;
var
  ResModule: HMODULE;
  ResInfo: HRSRC;
  ResData: HGLOBAL;
  NeedFreeLibrary: Boolean;
  TempInfo: ^Cardinal;
begin
  if not IsWinNT then
  begin
    Result := abfGetPackageFlagsA(AFileName);
    Exit;
  end;

  Result := 0;
  NeedFreeLibrary := True;

  ResModule := GetModuleHandleW(PWideChar(AFileName));
  if ResModule = 0 then
    ResModule := LoadLibraryExW(PWideChar(AFileName), 0, LOAD_LIBRARY_AS_DATAFILE)
  else
    NeedFreeLibrary := False;

  if ResModule = 0 then Exit;

  try
    ResInfo := FindResource(ResModule, 'PACKAGEINFO', RT_RCDATA);
    if ResInfo <> 0 then
    begin
      ResData := LoadResource(ResModule, ResInfo);
      if ResData <> 0 then
      try
        TempInfo := LockResource(ResData);
        UnlockResource(ResData);

        Result := TempInfo^;
      finally
        FreeResource(ResData);
      end;
    end;
  finally
    if NeedFreeLibrary then
      FreeLibrary(ResModule);
  end;
end;{function abfGetPackageFlagsW}

//------------------------------------------------------------------------------
// Returns a description of Borland package
// Date: 06/25/2007

function abfGetPackageDescriptionA(const AFileName: AnsiString): AnsiString;
var
  ResModule: HMODULE;
  ResInfo: HRSRC;
  ResData: HGLOBAL;
  NeedFreeLibrary: Boolean;
begin
  Result := '';
  NeedFreeLibrary := True;

  ResModule := GetModuleHandleA(PAnsiChar(AFileName));
  if ResModule = 0 then
    ResModule := LoadLibraryExA(PAnsiChar(AFileName), 0, LOAD_LIBRARY_AS_DATAFILE)
  else
    NeedFreeLibrary := False;

  if ResModule = 0 then Exit;

  try
    ResInfo := FindResourceA(ResModule, 'DESCRIPTION', PAnsiChar(RT_RCDATA));
    if ResInfo <> 0 then
    begin
      ResData := LoadResource(ResModule, ResInfo);
      if ResData <> 0 then
      try
        Result := PWideChar(LockResource(ResData));
        UnlockResource(ResData);
      finally
        FreeResource(ResData);
      end;
    end;
  finally
    if NeedFreeLibrary then
      FreeLibrary(ResModule);
  end;
end;{function abfGetPackageDescriptionA}

//------------------------------------------------------------------------------

function abfGetPackageDescription(const AFileName: string): string;
begin
  Result := abfGetPackageDescriptionA(AFileName);
end;{function abfGetPackageDescription}
//------------------------------------------------------------------------------

function abfGetPackageDescriptionW(const AFileName: WideString): WideString;
var
  ResModule: HMODULE;
  ResInfo: HRSRC;
  ResData: HGLOBAL;
  NeedFreeLibrary: Boolean;
begin
  if not IsWinNT then
  begin
    Result := abfGetPackageDescriptionA(AFileName);
    Exit;
  end;

  Result := '';
  NeedFreeLibrary := True;

  ResModule := GetModuleHandleW(PWideChar(AFileName));
  if ResModule = 0 then
    ResModule := LoadLibraryExW(PWideChar(AFileName), 0, LOAD_LIBRARY_AS_DATAFILE)
  else
    NeedFreeLibrary := False;

  if ResModule = 0 then Exit;

  try
    ResInfo := FindResourceA(ResModule, 'DESCRIPTION', PAnsiChar(RT_RCDATA));
    if ResInfo <> 0 then
    begin
      ResData := LoadResource(ResModule, ResInfo);
      if ResData <> 0 then
      try
        Result := PWideChar(LockResource(ResData));
        UnlockResource(ResData);
      finally
        FreeResource(ResData);
      end;
    end;
  finally
    if NeedFreeLibrary then
      FreeLibrary(ResModule);
  end;
end;{function abfGetPackageDescriptionW}

//------------------------------------------------------------------------------
// Uninstalls existed package in the Delphi/C++Builder IDE.
// Date: 05/29/2007
// Converted to Unicode version: 06/13/2007

procedure abfUninstallPackage(AVersion: TabfCompilerVersion; const AFileName: WideString);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(CabfIDERegPaths[AVersion] + SabfIDERegPath_Known_Packages,
      False) and ValueExists(AFileName) then
    try
      DeleteValue(AFileName);
    finally
      CloseKey;
    end;

    RootKey := HKEY_CURRENT_USER;
    if OpenKey(CabfIDERegPaths[AVersion] + SabfIDERegPath_Disabled_Packages,
      False) and ValueExists(AFileName) then
    try
      DeleteValue(AFileName);
    finally
      CloseKey;
    end;

    RootKey := HKEY_CURRENT_USER;
    if OpenKey(CabfIDERegPaths[AVersion] + SabfIDERegPath_Package_Cache,
      False) then
    try
      DeleteKey(abfExtractFileNameW(AFileName));
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;{procedure abfUninstallPackage}

//------------------------------------------------------------------------------
// Installs existed package in the Delphi/C++Builder IDE.
// Date: 05/29/2007
// Converted to Unicode version: 06/13/2007

procedure abfInstallPackage(AVersion: TabfCompilerVersion; const AFileName: WideString;
  const APackageName: WideString);
var
  TempKeyName: string;
  TempPkgName: WideString;
  TempFlags: Cardinal;
begin
// Remove previous version
  abfUninstallPackage(AVersion, AFileName);

  if not abfFileExistsW(AFileName) then Exit;

  TempFlags := abfGetPackageFlagsW(AFileName);
  if (TempFlags and pfPackageModule) = 0 then Exit;
  if (TempFlags and pfDesignOnly) = 0 then Exit;

  TempPkgName := Trim(APackageName);
  if TempPkgName = '' then
    TempPkgName := Trim(abfGetPackageDescriptionW(AFileName));
  if TempPkgName = '' then
    TempPkgName := Trim(abfGetFileDescriptionW(AFileName));
  if TempPkgName = '' then
    TempPkgName := Trim(abfExtractFileNameOnlyW(AFileName));

// Add package
  TempKeyName := CabfIDERegPaths[AVersion] + SabfIDERegPath_Known_Packages;
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(TempKeyName, True) then
    try
      WriteString(AFileName, string(TempPkgName));
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;{procedure abfInstallPackage}

//------------------------------------------------------------------------------
// Removes specified Path from the Delphi/C++Builder IDE browsing path.
// Date: 07/03/2007

procedure abfRemoveBrowsingPathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
      SabfIDERegPath_Library, False) then
    try
      TempValue := ReadString(SabfIDERegKey_Browsing_Path);
      _RemovePath(ACompilerConfig.Version, APath, TempValue);
      WriteString(SabfIDERegKey_Browsing_Path, TempValue);
    finally
      CloseKey;
    end;

    if ACompilerConfig.IsBDS
      and ACompilerConfig.SupportsPersonalities([persBCB]) then
    begin
      if ACompilerConfig.IDEVersion >= 5 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CPlusPlus_Paths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_BrowsingPath);
          _RemovePath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_BrowsingPath, TempValue);
        finally
          CloseKey;
        end;
      end else
      if ACompilerConfig.IDEVersion = 4 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CppPaths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_BrowsingPath);
          _RemovePath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_BrowsingPath, TempValue);
        finally
          CloseKey;
        end;
      end;
    end;

    if ACompilerConfig.IsBDS and (ACompilerConfig.IDEVersion >= 5) then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfRemoveBrowsingPathFromIDE}

//------------------------------------------------------------------------------
// Removes specified Path from the C++Builder IDE search paths.
// Date: 08/13/2007

procedure abfRemoveIncludePathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  if not (ACompilerConfig.IsBDS and
    ACompilerConfig.SupportsPersonalities([persBCB])) then Exit;

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CPlusPlus_Paths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_IncludePath);
        _RemovePath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_IncludePath, TempValue);
      finally
        CloseKey;
      end;
    end else
    if ACompilerConfig.IDEVersion = 4 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CppPaths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_IncludePath);
        _RemovePath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_IncludePath, TempValue);
      finally
        CloseKey;
      end;
    end;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfRemoveIncludePathFromIDE}

//------------------------------------------------------------------------------
// Removes specified Path from the C++Builder IDE library paths.
// Date: 08/13/2007

procedure abfRemoveLibraryPathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  if not (ACompilerConfig.IsBDS and
    ACompilerConfig.SupportsPersonalities([persBCB])) then Exit;

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CPlusPlus_Paths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_LibraryPath);
        _RemovePath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_LibraryPath, TempValue);
      finally
        CloseKey;
      end;
    end else
    if ACompilerConfig.IDEVersion = 4 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CppPaths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_LibraryPath);
        _RemovePath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_LibraryPath, TempValue);
      finally
        CloseKey;
      end;
    end;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfRemoveLibraryPathFromIDE}

//------------------------------------------------------------------------------
// Removes specified Path from the Delphi/C++Builder IDE search path.
// Date: 07/03/2007

procedure abfRemoveSearchPathFromIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempName, TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if ACompilerConfig.Version <= cv_D3 then
      TempName := SabfIDERegKey_SearchPath
    else
      TempName := SabfIDERegKey_Search_Path;

    if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
      SabfIDERegPath_Library, False) then
    try
      TempValue := ReadString(TempName);
      _RemovePath(ACompilerConfig.Version, APath, TempValue);
      WriteString(TempName, TempValue);
    finally
      CloseKey;
    end;

    if ACompilerConfig.IsBDS
      and ACompilerConfig.SupportsPersonalities([persBCB]) then
    begin
      if ACompilerConfig.IDEVersion >= 5 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CPlusPlus_Paths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_SearchPath);
          _RemovePath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_SearchPath, TempValue);
        finally
          CloseKey;
        end;
      end else
      if ACompilerConfig.IDEVersion = 4 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CppPaths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_SearchPath);
          _RemovePath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_SearchPath, TempValue);
        finally
          CloseKey;
        end;
      end;
    end;

    if ACompilerConfig.IsBDS and (ACompilerConfig.IDEVersion >= 5) then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfRemoveSearchPathFromIDE}

//------------------------------------------------------------------------------
// Adds specified Path to the Delphi/C++Builder IDE browsing path.
// Date: 07/03/2007

procedure abfAddBrowsingPathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  abfRemoveBrowsingPathFromIDE(ACompilerConfig, APath);

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
      SabfIDERegPath_Library, False) then
    try
      TempValue := ReadString(SabfIDERegKey_Browsing_Path);
      _AddPath(ACompilerConfig.Version, APath, TempValue);
      WriteString(SabfIDERegKey_Browsing_Path, TempValue);
    finally
      CloseKey;
    end;

    if ACompilerConfig.IsBDS
      and ACompilerConfig.SupportsPersonalities([persBCB]) then
    begin
      if ACompilerConfig.IDEVersion >= 5 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CPlusPlus_Paths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_BrowsingPath);
          _AddPath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_BrowsingPath, TempValue);
        finally
          CloseKey;
        end;
      end else
      if ACompilerConfig.IDEVersion = 4 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CppPaths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_BrowsingPath);
          _AddPath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_BrowsingPath, TempValue);
        finally
          CloseKey;
        end;
      end;
    end;

    if ACompilerConfig.IsBDS and (ACompilerConfig.IDEVersion >= 5) then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfAddBrowsingPathToIDE}

//------------------------------------------------------------------------------
// Adds specified Path to the C++Builder IDE include paths.
// Date: 08/13/2007

procedure abfAddIncludePathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  if not (ACompilerConfig.IsBDS
    and ACompilerConfig.SupportsPersonalities([persBCB])) then Exit;

  abfRemoveBrowsingPathFromIDE(ACompilerConfig, APath);

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CPlusPlus_Paths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_IncludePath);
        _AddPath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_IncludePath, TempValue);
      finally
        CloseKey;
      end;
    end else
    if ACompilerConfig.IDEVersion = 4 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CppPaths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_IncludePath);
        _AddPath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_IncludePath, TempValue);
      finally
        CloseKey;
      end;
    end;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfAddIncludePathToIDE}

//------------------------------------------------------------------------------
// Adds specified Path to the C++Builder IDE library paths.
// Date: 08/13/2007

procedure abfAddLibraryPathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  if not (ACompilerConfig.IsBDS
    and ACompilerConfig.SupportsPersonalities([persBCB])) then Exit;

  abfRemoveBrowsingPathFromIDE(ACompilerConfig, APath);

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CPlusPlus_Paths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_LibraryPath);
        _AddPath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_LibraryPath, TempValue);
      finally
        CloseKey;
      end;
    end else
    if ACompilerConfig.IDEVersion = 4 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_CppPaths, False) then
      try
        TempValue := ReadString(SabfIDERegKey_LibraryPath);
        _AddPath(ACompilerConfig.Version, APath, TempValue);
        WriteString(SabfIDERegKey_LibraryPath, TempValue);
      finally
        CloseKey;
      end;
    end;

    if ACompilerConfig.IDEVersion >= 5 then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfAddLibraryPathToIDE}

//------------------------------------------------------------------------------
// Adds specified Path to the Delphi/C++Builder IDE search path.
// Date: 07/03/2007
// Converted to Unicode version: 06/13/2007

procedure abfAddSearchPathToIDE(ACompilerConfig: TabfCompilerConfig;
  APath: WideString);
var
  TempName, TempValue: WideString;
begin
  if not Assigned(ACompilerConfig) then Exit;

  abfRemoveSearchPathFromIDE(ACompilerConfig, APath);

  APath := _ExpandRelativePath(ACompilerConfig.Version, APath);

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;

    if ACompilerConfig.Version <= cv_D3 then
      TempName := SabfIDERegKey_SearchPath
    else
      TempName := SabfIDERegKey_Search_Path;

    if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
      SabfIDERegPath_Library, False) then
    try
      TempValue := ReadString(TempName);
      _AddPath(ACompilerConfig.Version, APath, TempValue);
      WriteString(TempName, TempValue);
    finally
      CloseKey;
    end;

    if ACompilerConfig.IsBDS
      and ACompilerConfig.SupportsPersonalities([persBCB]) then
    begin
      if ACompilerConfig.IDEVersion >= 5 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CPlusPlus_Paths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_SearchPath);
          _AddPath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_SearchPath, TempValue);
        finally
          CloseKey;
        end;
      end else
      if ACompilerConfig.IDEVersion = 4 then
      begin
        if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
          SabfIDERegPath_CppPaths, False) then
        try
          TempValue := ReadString(SabfIDERegKey_SearchPath);
          _AddPath(ACompilerConfig.Version, APath, TempValue);
          WriteString(SabfIDERegKey_SearchPath, TempValue);
        finally
          CloseKey;
        end;
      end;
    end;

    if ACompilerConfig.IsBDS and (ACompilerConfig.IDEVersion >= 5) then
    begin
      if OpenKey(CabfIDERegPaths[ACompilerConfig.Version] +
        SabfIDERegPath_Globals, False) then
      try
        WriteString(SabfIDERegKey_ForceEnvOptionsUpdate, '1');
      finally
        CloseKey;
      end;
    end;
  finally
    Free;
  end;
end;{procedure abfAddSearchPathToIDE}

//------------------------------------------------------------------------------
// Removes a record that containes info about specified Help file from the
// given Cnt file.
// Date: 05/29/2007

procedure abfRemoveHelpFromCntFile(const AHelpFile, ACntFile: string);
var
  i: Integer;
  CntStr: string;
begin
  CntStr := ChangeFileExt(AHelpFile, SabfExt_Cnt);
  with TStringList.Create do
  try
    LoadFromFile(ACntFile);
    for i := Count - 1 downto 0 do
      if (abfPosIgnoreCase(AHelpFile, Strings[i]) <> 0) or
        (abfPosIgnoreCase(CntStr, Strings[i]) <> 0) then
        Delete(i);
    SaveToFile(ACntFile);
  finally
    Free;
  end;
end;{procedure abfRemoveHelpFromCntFile}

//------------------------------------------------------------------------------
// Adds a record that containes info about specified Help file into the given
// Cnt file.
// Date: 05/29/2007

procedure abfAddHelpToCntFile(const AHelpFile, AHelpName, ACntFile: string);
begin
  abfRemoveHelpFromCntFile(AHelpFile, ACntFile); // Remove previous version
  with TStringList.Create do
  try
    LoadFromFile(ACntFile);
    Add(SInclude + ' ' + ChangeFileExt(AHelpFile, SabfExt_Cnt));
    Add(SIndex + AHelpName + ' =' + AHelpFile);
    SaveToFile(ACntFile);
  finally
    Free;
  end;
end;{procedure abfAddHelpToCntFile}


end.
