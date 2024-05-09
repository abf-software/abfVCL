{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfIniFiles;

{$I abf.inc}

interface

uses
  Classes, IniFiles,
  abfClasses;

type

//==============================================================================
// TabfIniFile
//==============================================================================
// Extended TabfIniFile class

  TabfIniFile = class(TCustomIniFile)
  protected
    FFileName: WideString;
    FSections: TStringList;
    function AddSection(const Section: string): TStrings; virtual;
    procedure LoadValues; virtual;
  public
    constructor Create(const AFileName: WideString);
    destructor Destroy; override;
    procedure Clear;
    procedure Rename(const AFileName: string; Reload: Boolean);
    procedure UpdateFile; override;
    procedure GetStrings(List: TStrings); virtual;
    procedure SetStrings(List: TStrings); virtual;
    procedure DeleteKey(const Section,
      Ident: string); override;
    procedure EraseSection(const Section: string); override;
    procedure ReadSectionText(const Section: string; Text: TStrings); virtual;
    procedure WriteSectionText(const Section: string; Text: TStrings); virtual;
    procedure ReadSection(const Section: string;
      Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string;
      Strings: TStrings); override;
    function ReadString(const Section, Ident,
      Default: string): string; override;
    procedure WriteString(const Section, Ident,
      Value: string); override;
  // Properties
    property FileName: WideString read FFileName;
  end;


//==============================================================================
// Ini-files routines
//==============================================================================

function abfIniReadBool(const AFileName: WideString;
  const ASection, ALine: AnsiString): Boolean;
function abfIniReadInteger(const AFileName: WideString;
  const ASection, ALine: AnsiString): Integer;
function abfIniReadString(const AFileName: WideString;
  const ASection, ALine: AnsiString): AnsiString;
procedure abfIniWriteBool(const AFileName: WideString;
  const ASection, ALine: AnsiString; Value: Boolean);
procedure abfIniWriteInteger(const AFileName: WideString;
  const ASection, ALine: AnsiString; Value: Integer);
procedure abfIniWriteString(const AFileName: WideString;
  const ASection, ALine, Value: AnsiString);


{******************************************************************************}
implementation
{******************************************************************************}

uses
  SysUtils, abfSysUtils, abfStrUtils;

//==============================================================================
// TabfIniFile
//==============================================================================
// Extended TabfIniFile class
// Date: 09/28/2000
{ TabfIniFile }

//------------------------------------------------------------------------------

constructor TabfIniFile.Create(const AFileName: WideString);
begin
  FFileName := AFileName;
  inherited Create(FFileName);
  FSections := TStringList.Create;
  LoadValues;
end;

//------------------------------------------------------------------------------

destructor TabfIniFile.Destroy;
begin
  if Assigned(FSections) then
    Clear;

  FreeAndNil(FSections);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TabfIniFile.AddSection(const Section: string): TStrings;
begin
  Result := TStringList.Create;
  try
    FSections.AddObject(Section, Result);
  except
    Result.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.LoadValues;
var
  List: TabfAnsiStringList;
begin
  if (FileName <> '') and abfFileExistsW(FileName) then
  begin
    List := TabfAnsiStringList.Create;
    try
      List.LoadFromFile(FileName);
      SetStrings(List);
    finally
      List.Free;
    end;
  end else
    Clear;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.Clear;
var
  i: Integer;
begin
  for i := 0 to FSections.Count - 1 do
    TStrings(FSections.Objects[i]).Free;
  FSections.Clear;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.Rename(const AFileName: string; Reload: Boolean);
begin
  FFileName := AFileName;
  if Reload then LoadValues;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.UpdateFile;
var
  List: TabfAnsiStringList;
begin
  try
    List := TabfAnsiStringList.Create;
    try
      GetStrings(List);
      List.SaveToFile(FileName);
    finally
      List.Free;
    end;
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.GetStrings(List: TStrings);
var
  i, j: Integer;
  Strings: TStrings;
begin
  List.BeginUpdate;
  try
    for i := 0 to FSections.Count - 1 do
    begin
      List.Add('[' + FSections[i] + ']');
      Strings := TStrings(FSections.Objects[i]);
      for j := 0 to Strings.Count - 1 do List.Add(Strings[j]);
      List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.SetStrings(List: TStrings);

  //-------------------------------------

  function _TrimAroundEqual(const S: string): widestring;
  var
    P: Integer;
    Name: widestring;
  begin
    Result := {Trim}(S);
    P := AnsiPos('=', Result);
    if P <= 0 then Exit;
    Name := Result;
{$IFDEF D9}
    abfDeleteAfterCharW(Name, '=');
    abfDeleteBeforeCharW(Result, '=');
{$ELSE}
    abfDeleteAfterCharA(Name, '=');
    abfDeleteBeforeCharA(Result, '=');
{$ENDIF}
    Result := TrimRight(Name) + '=' + TrimLeft(Result);
  end;{Internal _TrimAroundEqual}

  //-------------------------------------

var
  i: Integer;
  S: string;
  Strings: TStrings;
begin
  Clear;
  Strings := nil;
  for i := 0 to List.Count - 1 do
  begin
    S := Trim(List[i]);
    if (S <> '') and (S[1] <> ';') then
      if (S[1] = '[') and (S[Length(S)] = ']') then
        Strings := AddSection(Copy(S, 2, Length(S) - 2))
      else
        if Strings <> nil then Strings.Add(_TrimAroundEqual(S));
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.DeleteKey(const Section, Ident: string);
var
  i, j: Integer;
  Strings: TStrings;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then
  begin
    Strings := TStrings(FSections.Objects[i]);
    j := Strings.IndexOfName(Ident);
    if j >= 0 then Strings.Delete(j);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.EraseSection(const Section: string);
var
  i: Integer;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then
  begin
    TStrings(FSections.Objects[i]).Free;
    FSections.Delete(I);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.ReadSectionText(const Section: string;
  Text: TStrings);
var
  i: Integer;
begin
  if not Assigned(Text) then Exit;
  Text.BeginUpdate;
  try
    Text.Clear;
    i := FSections.IndexOf(Section);
    if i >= 0 then Text.AddStrings(TStrings(FSections.Objects[i]));
  finally
    Text.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.WriteSectionText(const Section: string; Text: TStrings);
var
  ASection: TStrings;
  i: Integer;
begin
  if not Assigned(Text) then Exit;
  i := FSections.IndexOf(Section);
  if i >= 0 then
    ASection := TStrings(FSections.Objects[i])
  else
    ASection := AddSection(Section);
  ASection.Clear;
  ASection.AddStrings(Text);
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.ReadSection(const Section: string;
  Strings: TStrings);
var
  i, j: Integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    i := FSections.IndexOf(Section);
    if i >= 0 then
    begin
      SectionStrings := TStrings(FSections.Objects[i]);
      for j := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[j]);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.ReadSections(Strings: TStrings);
begin
  Strings.Assign(FSections);
end;

//------------------------------------------------------------------------------

procedure TabfIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  i: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    i := FSections.IndexOf(Section);
    if i >= 0 then Strings.Assign(TStrings(FSections.Objects[i]));
  finally
    Strings.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

function TabfIniFile.ReadString(const Section, Ident,
  Default: string): string;
var
  i: Integer;
  Strings: TStrings;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then
  begin
    Strings := TStrings(FSections.Objects[i]);
    i := Strings.IndexOfName(Ident);
    if i >= 0 then
    begin
      Result := Copy(Strings[i], Length(Ident) + 2, Maxint);
      Exit;
    end;
  end;
  Result := Default;
end;


//------------------------------------------------------------------------------

procedure TabfIniFile.WriteString(const Section, Ident, Value: string);
var
  i: Integer;
  S: string;
  Strings: TStrings;
begin
  i := FSections.IndexOf(Section);
  if i >= 0 then
    Strings := TStrings(FSections.Objects[i]) else
    Strings := AddSection(Section);
  S := Ident + '=' + Value;
  i := Strings.IndexOfName(Ident);
  if i >= 0 then Strings[i] := S else Strings.Add(S);
end;

//==============================================================================
// Ini-files routines
//==============================================================================

function abfIniReadBool(const AFileName: WideString;
  const ASection, ALine: AnsiString): Boolean;
var
  Ini: TabfIniFile;
begin
  Ini := TabfIniFile.Create(AFileName);
  try
    Result := Ini.ReadBool(ASection, ALine, False);
  finally
    Ini.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfIniReadInteger(const AFileName: WideString;
  const ASection, ALine: AnsiString): Integer;
var
  Ini: TabfIniFile;
begin
  Ini := TabfIniFile.Create(AFileName);
  try
    Result := Ini.ReadInteger(ASection, ALine, 0);
  finally
    Ini.Free;
  end;
end;

//------------------------------------------------------------------------------

function abfIniReadString(const AFileName: WideString;
  const ASection, ALine: AnsiString): AnsiString;
var
  Ini: TabfIniFile;
begin
  Ini := TabfIniFile.Create(AFileName);
  try
    Result := Ini.ReadString(ASection, ALine, '');
  finally
    Ini.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfIniWriteBool(const AFileName: WideString;
  const ASection, ALine: AnsiString; Value: Boolean);
var
  Ini: TabfIniFile;
begin
  Ini := TabfIniFile.Create(AFileName);
  try
    Ini.WriteBool(ASection, ALine, Value);
  finally
    Ini.Free;
  end;
end;

//------------------------------------------------------------------------------


procedure abfIniWriteInteger(const AFileName: WideString;
  const ASection, ALine: AnsiString; Value: Integer);
var
  Ini: TabfIniFile;
begin
  Ini := TabfIniFile.Create(AFileName);
  try
    Ini.WriteInteger(ASection, ALine, Value);
  finally
    Ini.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure abfIniWriteString(const AFileName: WideString;
  const ASection, ALine, Value: AnsiString);
var
  Ini: TabfIniFile;
begin
  Ini := TabfIniFile.Create(AFileName);
  try
    Ini.WriteString(ASection, ALine, Value);
  finally
    Ini.Free;
  end;
end;

//------------------------------------------------------------------------------

end{unit abfIniFiles}.
