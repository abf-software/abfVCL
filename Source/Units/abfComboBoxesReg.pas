{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComboBoxesReg;

{$I abf.inc}
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
{$IfDef D6}
  DesignIntf,
{$Else D6}
  DsgnIntf,
{$EndIf D6}
  abfComponentsReg, // don't remove this line !!!
  Classes;

type

//==============================================================================
// TabfComboBoxEditor
//==============================================================================
// Component editor for TabfXXXC ComboBoxes, implements OneTouch� Designer and
// About dialog.

  TabfComboBoxEditor = class(TabfComponentEditor)
  protected
    procedure DesignComponent; virtual;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
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
  abfConsts, abfVclConsts,
  abfAboutComponent, abfControls, abfComboBoxes, abfDesignerComboBox,
  abfDesignerColorComboBox, abfDesignerFontNameComboBox,
  abfDesignerFontSizeComboBox;

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
  abfControlsReg.Register;

  RegisterComponents(SabfComponentPaletteVisual,
    [TabfColorComboBox, TabfFontNameComboBox,
    TabfFontSizeComboBox]);

// Assign component editors
  RegisterComponentEditor(TabfCustomComboBox, TabfComboBoxEditor);
  RegisterComponentEditor(TabfDrawComboBox, TabfComboBoxEditor);
  
// Assign property editors
  RegisterPropertyEditor(TypeInfo(string), TabfCustomComboBox,
    SabfAboutProperty, TabfAboutProperty);
end;


//==============================================================================
// TabfComboBoxEditor
//==============================================================================
// Component editor for TabfXXXC ComboBoxes, implements OneTouch� Designer and
// About dialog.
// Date: 04/01/2000
{ TabfComboBoxEditor }

procedure TabfComboBoxEditor.DesignComponent;
begin
  if Component is TabfColorComboBox then abfColorComboBoxDesign(Component)
  else
  if Component is TabfFontNameComboBox then abfFontNameComboBoxDesign(Component)
  else
  if Component is TabfFontsizeComboBox then abfFontsizeComboBoxDesign(Component)
  else abfComboBoxDesign(Component);
  Designer.Modified;
end;

//------------------------------------------------------------------------------

procedure TabfComboBoxEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 2) then DesignComponent
  else inherited ExecuteVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfComboBoxEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 2) then Result := SabfOneTouchDesigner + '...'
  else Result := inherited GetVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfComboBoxEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

//------------------------------------------------------------------------------

end{unit abfComboBoxesReg}.
