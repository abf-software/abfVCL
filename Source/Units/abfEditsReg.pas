{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEditsReg;

{$I abf.inc}
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  abfComponentsReg, abfControlsReg, // don't remove this line !!!
{$IfDef D6}
  DesignIntf, DesignEditors, DesignMenus,
{$Else D6}
  DsgnIntf, 
{$EndIf D6}
  Classes, SysUtils, FiltEdit;

type

//==============================================================================
// TabfButtonEditEditor
//==============================================================================
// Component editor for all TabfCustomButtonEdit component, inherits all
// features of TabfControlEditor and implements "Click on button" menu item.

  TabfButtonEditEditor = class(TabfControlEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
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
  Forms,
  abfConsts, abfVclConsts, abfAboutComponent, abfEdits, abfEditsConsts;

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
  abfControlsReg.Register;

  RegisterComponents(SabfComponentPaletteEdits,
    [TabfIntegerEdit, TabfAdvancedEdit, TabfButtonEdit, TabfUpDownEdit,
    TabfFileNameEdit, TabfDirectoryNameEdit]);

// Assign component editors
  RegisterComponentEditor(TabfCustomButtonEdit, TabfButtonEditEditor);

// Assign property editors
  RegisterPropertyEditor(TypeInfo(string), TabfCustomButtonEdit,
    'DialogFilter', TFilterProperty);

// Load default images for editor buttons    
  ABF_GLYPH_FILE.LoadFromResourceName(HInstance, 'ABF_GLYPH_FILE');
  ABF_GLYPH_FOLDER.LoadFromResourceName(HInstance, 'ABF_GLYPH_FOLDER');
end;

//==============================================================================
// TabfButtonEditEditor
//==============================================================================
// Component editor for all TabfButtonEdit component, inherits all features of
// TabfFileStorageEditor and implements Play and Stop sound routines.
{ TabfButtonEditEditor }

procedure TabfButtonEditEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: inherited ExecuteVerb(1);
    1: TabfCustomButtonEdit(Component).Button.Click;
  else
    inherited GetVerb(Index);
  end;
end;

//------------------------------------------------------------------------------

function TabfButtonEditEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := inherited GetVerb(1);
    1: Result := SabfButtonEdit_DesignerMenuItem1;
  else
    Result := inherited GetVerb(Index);
  end;
end;

//------------------------------------------------------------------------------

function TabfButtonEditEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

//------------------------------------------------------------------------------
{$IfDef D5}

procedure TabfButtonEditEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  {$IfNDef D6}
  case Index of
    0: inherited PrepareItem(1, AItem);
    1: AItem.Bitmap.LoadFromResourceName(hInstance, 'GLYPH_PREVIEW')
  else
    inherited GetVerb(Index);
  end;
  {$Else D6}
  inherited PrepareItem(Index, AItem)
  {$EndIf D6}
end;

{$EndIf D5}
//------------------------------------------------------------------------------


end{unit abfEditsReg}.

