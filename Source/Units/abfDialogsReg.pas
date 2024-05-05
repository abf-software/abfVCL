{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDialogsReg;

{$I abf.inc}
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
{$IfDef D5}
  Menus,
{$EndIf D5}
{$IfDef D6}
  DesignIntf, DesignEditors, DesignMenus,
{$Else D6}
  DsgnIntf,
{$EndIf D6}
  abfComponentsReg, // don't remove this line !!!
  Classes, SysUtils;

type

//==============================================================================
// TabfDirNameProperty
//==============================================================================
// Directory name property editor for all 'Directory' properties, implements
// a select dir dialog.

  TabfDirNameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

//==============================================================================
// TabfDialogEditor
//==============================================================================
// Default component editor for all abfXXX dialog components, implements a
// design-time dialog execution and About dialog.

  TabfDialogEditor = class(TabfComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
{$IfDef D5}
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
{$EndIf D5}
  end;

//==============================================================================
// Registration
//==============================================================================

procedure Register;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.res}

uses
  abfControlsReg, // don't remove this line !!!
{$IfNDef D3}
  FileCtrl,
{$EndIf D3}
  Forms,
  abfConsts, abfVclConsts, abfDialogsConsts, abfComponents, abfAboutComponent,
  abfDialogs;

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
  abfControlsReg.Register;

  RegisterComponents(SabfComponentPaletteDialogs,
    [TabfWinAboutDlg, TabfSelectDirDlg, TabfBrowseFolderDlg, TabfCplDlg,
    TabfRunDlg, TabfOpenWithDlg, TabfObjectPropertiesDlg, TabfPickIconDlg,
    TabfFindDlg, TabfCreateShortcutDlg, TabfAddToFavoritesDlg,
    TabfOrganizeFavoritesDlg]);

// Assign component editors
  RegisterComponentEditor(TabfCustomDlg, TabfDialogEditor);

// Assign property editors
  RegisterPropertyEditor(TypeInfo(string), TabfCustomDlg, 'Directory',
    TabfDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TabfCustomDlg, 'Folder',
    TabfDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TabfBrowseFolderDlg, 'Item',
    TabfDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TabfFileOperation, 'DestFolder',
    TabfDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TabfCustomFolderMonitor, 'Folder',
    TabfDirNameProperty);

// Property editors for common property names
  RegisterPropertyEditor(TypeInfo(string), TPersistent, 'Directory',
    TabfDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TPersistent, 'Folder',
    TabfDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TPersistent, 'InitialDir',
    TabfDirNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TPersistent, 'DialogInitialDir',
    TabfDirNameProperty);

end;


//==============================================================================
// TabfDirNameProperty
//==============================================================================
// Directory name property editor for all 'Directory' properties, implements
// a select dir dialog.
// Date: 07/01/2000

procedure TabfDirNameProperty.Edit;
var
  Dialog: TabfBrowseFolderDlg;
begin
  Dialog := TabfBrowseFolderDlg.Create(Application);
  with Dialog do
  try
    Dialog.Options := Dialog.Options - [sdoNewDialogStyle];
    Text := SabfDialogs_DirNamePropertyText;
    Item := GetValue;
    if Execute then SetValue(Item);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------

function TabfDirNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;


//==============================================================================
// TabfDialogEditor
//==============================================================================
// Default component editor for all abfXXX dialog components, implements a
// design-time dialog execution and About dialog.
// Date: 11/10/2000
{ TabfDialogEditor }

procedure TabfDialogEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 2) then
  begin
    TabfCustomDlg(Component).Execute;
    Designer.Modified;
  end else
    inherited ExecuteVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfDialogEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 2) then Result := SabfDialogs_DesignerMenuItem
  else Result := inherited GetVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfDialogEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfDialogEditor.PrepareItem(Index: Integer;
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
//------------------------------------------------------------------------------

end{unit abfComponentsReg}.

