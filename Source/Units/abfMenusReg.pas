{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfMenusReg;

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
  Classes, SysUtils, Forms;

//==============================================================================
// Registration
//==============================================================================

procedure Register;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.res}

uses
  abfComponentsReg, abfControlsReg, // don't remove this line !!!
  abfConsts, abfComponents, abfAboutComponent, abfMenus;

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
  abfControlsReg.Register;

  RegisterComponents(SabfComponentPaletteNonVisual,
    [TabfSystemMenuItem, TabfSystemMenuInserter, TabfPopupMenu]);

// Assign component editors
//  RegisterComponentEditor(TabfPopupMenu, TabfComponentEditor);

// Assign property editors
  RegisterPropertyEditor(TypeInfo(string), TabfPopupMenu, SabfAboutProperty,
    TabfAboutProperty);
end;

//------------------------------------------------------------------------------

end{unit abfMenusReg}.

