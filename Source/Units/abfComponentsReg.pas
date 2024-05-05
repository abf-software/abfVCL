{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComponentsReg;

{$I abf.inc}
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
{$IfDef D6}
  DesignIntf, DesignEditors, DesignMenus,
{$Else D6}
  DsgnIntf,
{$EndIf D6}
{$IfDef D5}
  Menus, ImgList,
{$EndIf D5}
  Windows, Classes, SysUtils;

const
  cSystemEvents: array[0..18] of string = (
    '.Default', 'AppGPFault', 'Close', 'MailBeep', 'Maximize', 'MenuCommand',
    'MenuPopup', 'Minimize', 'Open', 'RestoreDown', 'RestoreUp', 'RingIn',
    'RingOut', 'SystemAsterisk', 'SystemExclamation', 'SystemExit',
    'SystemHand', 'SystemQuestion', 'SystemStart'); // Don't localize

type

//==============================================================================
// Alliases for diferent versions of ToolsAPI
//==============================================================================

{$IfDef D5}
  {$IfNDef D6}
  IMenuItem = TMenuItem;
  IProperty = TPropertyEditor;
  {$EndIf D6}
{$EndIf D5}

//==============================================================================
// TabfAboutProperty
//==============================================================================
// About property editor for all TabfXXX components, implements About dialog.

  TabfAboutProperty = class(TStringProperty)
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

//==============================================================================
// TabfFileNameProperty
//==============================================================================
// File name property editor for all FileName' properties, implements Open File
// dialog.

  TabfFileNameProperty = class(TStringProperty)
  protected
    function GetFilter: string; virtual;
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

//==============================================================================
// TabfShortFileNameProperty
//==============================================================================
// Additional file name property editor for short (without path) file name
// selection, implements Open File dialog.

  TabfShortFileNameProperty = class(TabfFileNameProperty)
  public
    procedure Edit; override;
  end;

//==============================================================================
// TabfEventAliasProperty
//==============================================================================
// EventAlias property editor for TabfWav component.

  TabfEventAliasProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

//==============================================================================
// TabfComponentEditor
//==============================================================================
// Default component editor for TabfXXX components. Implements "About..." item
// in the popup menu.

  TabfComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
{$IfDef D5}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$EndIf D5}
  end;

//==============================================================================
// TabfTrayIconEditor
//==============================================================================
// Component editor for all TabfTrayIcon component, inherits all features of
// TabfComponentEditor and implements "Show balloon info" menu item.

  TabfTrayIconEditor = class(TabfComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

//==============================================================================
// TabfFileStorageEditor
//==============================================================================
// Component editor for all TabfFileStorage components, implements About dialog,
// "Load file to DFM" and "Remove file from DFM" features.

  TabfFileStorageEditor = class(TabfComponentEditor)
  protected
    function GetFilter: string; virtual;
    function LoadFile: Boolean; virtual;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
{$IfDef D5}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$EndIf D5}
  end;

//==============================================================================
// TabfWavEditor
//==============================================================================
// Component editor for all TabfWav components, inherits all features of
// TabfFileStorageEditor and implements Play and Stop sound routines.

  TabfWavEditor = class(TabfFileStorageEditor)
  protected
    function GetFilter: string; override;
    function LoadFile: Boolean; override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
{$IfDef D5}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$EndIf D5}
  end;


//==============================================================================
// TabfFileOperationEditor
//==============================================================================

  TabfFileOperationEditor = class(TDefaultEditor)
  protected
{$IfDef D6}
    procedure EditProperty(const Prop: IProperty;
      var Continue: Boolean); override;
{$Else D6}
    procedure EditProperty(Prop: TPropertyEditor; var Continue,
      FreeEditor: Boolean); override;
{$EndIf D6}
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
{$IfDef D5}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$EndIf D5}
  end;


//==============================================================================
// TabfFileAssociationEditor
//==============================================================================

  TabfFileAssociationEditor = class(TabfComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
{$IfDef D5}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$EndIf D5}
  end;


{*******************************************************************************
  Date and Time property for D2/C1 as it is in higher versions.
*******************************************************************************}
{$IfNDef D3}

//==============================================================================
// TabfDateProperty
//==============================================================================

  TabfDateProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

//==============================================================================
// TabfTimeProperty
//==============================================================================

  TabfTimeProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

//==============================================================================
// TabfDateTimeProperty
//==============================================================================

  TabfDateTimeProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{$EndIf D3}

//==============================================================================
// Registration
//==============================================================================

procedure Register;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.res}

uses
  Consts, Controls, Dialogs, Forms,
  abfGraphicsReg,      // don't remove this line !!!
  abfAboutComponent,   // don't remove this line !!!
  abfAboutApplication, // don't remove this line !!!
  abfClasses,          // don't remove this line !!!
  abfTypInfo,          // don't remove this line !!!
  abfPropertyDesc,     // don't remove this line !!!
  abfConsts, abfVclConsts, abfComponents, abfAppProps;

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
{$IfNDef D3}
  abfGraphicsReg.Register;
  RegisterPropertyEditor(TypeInfo(TDate), nil, '', TabfDateProperty);
  RegisterPropertyEditor(TypeInfo(TTime), nil, '', TabfTimeProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TabfDateTimeProperty);
{$EndIf D3}

  RegisterComponents(SabfComponentPaletteNonVisual,
    [TabfApplicationProperties, TabfAutoRun, TabfOneInstance, TabfShutdown,
    TabfTrayIcon, TabfWndProcHook, TabfFileStorage, TabfWav,
    TabfThreadComponent, TabfThreadTimer, TabfFileOperation,
    TabfFileAssociation, TabfFolderMonitor, TabfRegistryMonitor,
    TabfStartButtonProperties, TabfColorPicker]);

// Assign component editors
  RegisterComponentEditor(TabfComponent, TabfComponentEditor);
  RegisterComponentEditor(TabfApplicationProperties, TabfComponentEditor);
  RegisterComponentEditor(TabfTrayIcon, TabfTrayIconEditor);
  RegisterComponentEditor(TabfWav, TabfWavEditor);
  RegisterComponentEditor(TabfCustomFileStorage, TabfFileStorageEditor);
  RegisterComponentEditor(TabfFileOperation, TabfFileOperationEditor);
  RegisterComponentEditor(TabfFileAssociation, TabfFileAssociationEditor);

// Assign property editors
  RegisterPropertyEditor(TypeInfo(string), TabfComponent, SabfAboutProperty,
    TabfAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TabfApplicationProperties,
    SabfAboutProperty, TabfAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TabfWav, 'EventAlias',
    TabfEventAliasProperty);
  RegisterPropertyEditor(TypeInfo(string), TPersistent, 'FileName',
    TabfFileNameProperty);
{$IfDef D4}
  RegisterPropertyEditor(TypeInfo(TFileName), TPersistent, '',
    TabfFileNameProperty);
{$Else D4}
  RegisterPropertyEditor(TypeInfo(TFileName), TabfApplicationProperties,
    'HelpFile', TabfFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TabfAutoRun, 'FileName',
    TabfFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TabfCustomFileStorage, 'FileName',
    TabfFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TabfCustomFileStorage, 'SaveName',
    TabfFileNameProperty);
{$EndIf D4}
end;{procedure Register}

//==============================================================================
// TabfAboutProperty
//==============================================================================
// About property editor for all TabfXXX components, implements About dialog.
// Date: 03/07/2001
{ TabfAboutProperty }

procedure TabfAboutProperty.Edit;
begin
  abfComponentAbout(TComponent(GetComponent(0)));
end;

//------------------------------------------------------------------------------

function TabfAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paReadOnly, paDialog];
end;

//------------------------------------------------------------------------------

function TabfAboutProperty.GetValue: string;
begin
  Result := 'Ver ' + SabfVCLVersion;
end;


//==============================================================================
// TabfFileNameProperty
//==============================================================================
// File name property editor for all FileName' properties, implements Open File
// dialog.
// Date: 03/07/2001
{ TabfFileNameProperty }

procedure TabfFileNameProperty.Edit;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Application);
  with Dialog do
  try
    FileName   := GetValue;
    InitialDir := ExtractFilePath(FileName);
    FileName   := ExtractFileName(FileName);
    Filter     := GetFilter;
    Options    := Options + [ofHideReadOnly];
    if Execute then SetValue(FileName);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------

function TabfFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

//------------------------------------------------------------------------------

function TabfFileNameProperty.GetFilter: string;
begin
  Result := SDefaultFilter;
end;


//==============================================================================
// TabfShortFileNameProperty
//==============================================================================
// Additional file name property editor for short (without path) file name
// selection, implements Open File dialog.
// Date: 03/07/2001
{ TabfShortFileNameProperty }

procedure TabfShortFileNameProperty.Edit;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Application);
  with Dialog do
  try
    FileName   := GetValue;
    InitialDir := ExtractFilePath(FileName);
    FileName   := ExtractFileName(FileName);
    Filter     := GetFilter;
    Options    := Options + [ofHideReadOnly];
    if Execute then SetValue(ExtractFileName(FileName));
  finally
    Free;
  end;
end;


//==============================================================================
// TabfEventAliasProperty
//==============================================================================
// EventAlias property editor for TabfWav component.
// Date: 03/11/2001
{ TabfEventAliasProperty }

function TabfEventAliasProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

//------------------------------------------------------------------------------

procedure TabfEventAliasProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(cSystemEvents) to High(cSystemEvents) do
    Proc(cSystemEvents[i]);
end;


//==============================================================================
// TabfComponentEditor
//==============================================================================
// Default component editor for TabfXXX components. Implements "About..." item
// in the popup menu.
// Date: 03/07/2001
{ TabfComponentEditor }

procedure TabfComponentEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 1) then abfComponentAbout(Component)
  else inherited ExecuteVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfComponentEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 1) then Result := SabfAbout + '...'
  else Result := inherited GetVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfComponentEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
begin
  {$IfNDef D6}
  if (Index = GetVerbCount - 1) then
    AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_ABOUT')
  else
  {$EndIf D6}
    inherited PrepareItem(Index, AItem)
end;

{$EndIf D5}


//==============================================================================
// TabfTrayIconEditor
//==============================================================================
// Component editor for all TabfTrayIcon component, inherits all features of
// TabfFileStorageEditor and implements Play and Stop sound routines.
{ TabfTrayIconEditor }

procedure TabfTrayIconEditor.ExecuteVerb(Index: Integer);

  procedure _ShowInfo;
  begin
    with TabfTrayIcon(Component) do
    begin
      VisibleOnDesigning := True;
      ShowInfo;
    end;
  end;{Internal procedure _ShowInfo}

begin
  case Index of
    0: inherited ExecuteVerb(1);
    1: _ShowInfo
  else
    inherited GetVerb(Index);
  end;
end;

//------------------------------------------------------------------------------

function TabfTrayIconEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := inherited GetVerb(1);
    1: Result := SabfTrayIcon_DesignerMenuItem1;
  else
    Result := inherited GetVerb(Index);
  end;
end;

//------------------------------------------------------------------------------

function TabfTrayIconEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;


//==============================================================================
// TabfFileStorageEditor
//==============================================================================
// Component editor for all TabfFileStorage components, implements About dialog,
// "Load file to DFM" and "Remove file from DFM" features.
// Date: 11/11/2000
{ TabfFileStorageEditor }

//------------------------------------------------------------------------------

procedure TabfFileStorageEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 3) then LoadFile else
  if (Index = GetVerbCount - 2) then
  begin
    TabfCustomFileStorage(Component).Clear;
    Designer.Modified;
  end else
    inherited ExecuteVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfFileStorageEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 3) then Result := SabfFileStorage_DesignerMenuItem1
  else
  if (Index = GetVerbCount - 2) then Result := SabfFileStorage_DesignerMenuItem2
  else Result := inherited GetVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfFileStorageEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfFileStorageEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
begin
  {$IfNDef D6}
  if (Index = GetVerbCount - 3) then
    AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_ADDFILE')
  else
  if (Index = GetVerbCount - 2) then
    AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_REMOVEFILE')
  else
  {$EndIf D6}
    inherited PrepareItem(Index, AItem)
end;

{$EndIf D5}
//------------------------------------------------------------------------------

function TabfFileStorageEditor.GetFilter: string;
begin
  Result := SDefaultFilter;
end;

//------------------------------------------------------------------------------
// Loads file into DFM. Shows open dilog. Return True if file was selected.

function TabfFileStorageEditor.LoadFile: Boolean;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Application);
  with Dialog do
  try
    FileName   := TabfCustomFileStorage(Component).FileName;
    Title      := SabfFileStorage_DesignerMenuItem1;
    InitialDir := ExtractFilePath(FileName);
    FileName   := ExtractFileName(FileName);
    Filter     := GetFilter;
    Options    := Options + [ofHideReadOnly];
    Result := Execute;
    if Result then
    begin
      TabfCustomFileStorage(Component).FileName := FileName;
      Designer.Modified;
    end;
  finally
    Dialog.Free;
  end;
end;


//==============================================================================
// TabfWavEditor
//==============================================================================
// Component editor for all TabfWav components, inherits all features of
// TabfFileStorageEditor and implements Play and Stop sound routines.
// Date: 03/11/2001
{ TabfWavEditor }

//------------------------------------------------------------------------------

procedure TabfWavEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 6) then
    with TabfWav(Component) do
    begin
      if Looped and (not AsyncPlay) then
      begin
        Application.MessageBox(PChar(SabfWave_PlayInDesignerWarning),
          PChar(SabfWarning), MB_ICONWARNING or MB_OK);
        Exit;
      end;
      Play;
    end
  else
  if (Index = GetVerbCount - 5) then TabfWav(Component).Stop
  else inherited ExecuteVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfWavEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 6) then Result := SabfWav_DesignerMenuItem1
  else
  if (Index = GetVerbCount - 5) then Result := SabfWav_DesignerMenuItem2
  else
  if (Index = GetVerbCount - 4) then Result := '-'
  else Result := inherited GetVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfWavEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 3;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfWavEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
begin
  {$IfNDef D6}
  if (Index = GetVerbCount - 6) then
    AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_YES')
  else
  if (Index = GetVerbCount - 5) then
    AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_NO')
  else
  {$EndIf D6}
    inherited PrepareItem(Index, AItem)
end;

{$EndIf D5}
//------------------------------------------------------------------------------
// Sets filter to *.wav only files.

function TabfWavEditor.GetFilter: string;
begin
  Result := 'Wave sound (*.wav)|*' + SabfExt_Wav  + '|'+ inherited GetFilter;
end;

//------------------------------------------------------------------------------
// Loads file into DFM. If file was loaded, changes PlayFrom property.

function TabfWavEditor.LoadFile: Boolean;
begin
  Result := inherited LoadFile;
  if not Result then Exit;
  TabfWav(Component).PlayFrom := pfDFM;
  Designer.Modified;
end;


//==============================================================================
// TabfFileOperationEditor
//==============================================================================
// Component editor for all TabfFileOperation components, implements About
// dialog and sets Default editor.
// Date: 03/07/2001
{ TabfFileOperationEditor }

//------------------------------------------------------------------------------

{$IfDef D6}
procedure TabfFileOperationEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
{$Else D6}
procedure TabfFileOperationEditor.EditProperty(Prop: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$EndIf D6}
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if CompareText(PropName, 'FileList') = 0 then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfFileOperationEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1: abfComponentAbout(Component)
  else
    inherited ExecuteVerb(Index);
  end;
end;

//------------------------------------------------------------------------------

function TabfFileOperationEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SabfFileOperation_DesignerMenuItem;
    1: Result := SabfAbout + '...';
  else
    inherited ExecuteVerb(Index);
  end;
end;

//------------------------------------------------------------------------------

function TabfFileOperationEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfFileOperationEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
begin
  {$IfNDef D6}
  if (Index = GetVerbCount - 1) then
    AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_ABOUT')
  else
  {$EndIf D6}
    inherited PrepareItem(Index, AItem)
end;
{$EndIf D5}


//==============================================================================
// TabfFileAssociationEditor
//==============================================================================
// Date: 09/24/2008

procedure TabfFileAssociationEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 2) then
  begin
    if MessageDlg(SabfFileAssociation_DesignerQuestion,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      TabfFileAssociation(Component).ReadAssociation;
      Designer.Modified;
    end;
  end else
    inherited ExecuteVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfFileAssociationEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 2) then Result := SabfFileAssociation_DesignerMenuItem
  else Result := inherited GetVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfFileAssociationEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfFileAssociationEditor.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
begin
{$IfDef D5_ONLY}
  if (Index = GetVerbCount - 2) then
    AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_PREVIEW')
  else
{$EndIf D5_ONLY}
    inherited PrepareItem(Index, AItem)
end;

{$EndIf D5}


{*******************************************************************************
  Date and Time property for D2/C1 as it is in higher versions.
*******************************************************************************}
{$IfNDef D3}

//==============================================================================
// TabfDateProperty
//==============================================================================
// Date: 08/01/2000
{ TabfDateProperty }

function TabfDateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

//------------------------------------------------------------------------------

function TabfDateProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := DateToStr(DT);
end;

//------------------------------------------------------------------------------

procedure TabfDateProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToDate(Value);
  SetFloatValue(DT);
end;

//==============================================================================
// TabfTimeProperty
//==============================================================================
// Date: 08/01/2000
{ TabfTimeProperty }

function TabfTimeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

//------------------------------------------------------------------------------

function TabfTimeProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := TimeToStr(DT);
end;

//------------------------------------------------------------------------------

procedure TabfTimeProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToTime(Value);
  SetFloatValue(DT);
end;


//==============================================================================
// TabfDateTimeProperty
//==============================================================================
// Date: 08/01/2000
{ TabfDateTimeProperty }

function TabfDateTimeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

//------------------------------------------------------------------------------

function TabfDateTimeProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else Result := DateTimeToStr(DT);
end;

//------------------------------------------------------------------------------

procedure TabfDateTimeProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0 else DT := StrToDateTime(Value);
  SetFloatValue(DT);
end;

{$EndIf D3}
//------------------------------------------------------------------------------

end{unit abfComponentsReg}.
