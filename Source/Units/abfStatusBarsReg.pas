{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfStatusBarsReg;

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
// TabfStatusBarEditor
//==============================================================================
// Component editor for TabfStatusBar

  TabfStatusBarEditor = class(TabfComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
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
  abfConsts, abfVclConsts, abfStatusBarsConsts,
  abfAboutComponent, abfStatusBars, abfDesignerStatusBar;

  
//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
  abfControlsReg.Register;

  RegisterComponents(SabfComponentPaletteVisual,
    [TabfStatusBar]);
  RegisterComponents(SabfComponentPaletteNonVisual,
    [TabfStatusBarInserter]);

// Assign component editors
  RegisterComponentEditor(TabfStatusBar, TabfStatusBarEditor);

// Assign property editors
  RegisterPropertyEditor(TypeInfo(string), TabfStatusBar, SabfAboutProperty,
    TabfAboutProperty);

{$IfNDef D3}
  RegisterPropertyEditor(TypeInfo(TabfStatusPanels), TabfStatusBar,
    'Panels', TabfStatusPanelsPropertyEditor);
{$EndIf D3}
end;


//==============================================================================
// TabfStatusBarEditor
//==============================================================================
// Component editor for TabfStatusBar
// Date: 09/01/2000
{ TabfStatusBarEditor }

procedure TabfStatusBarEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 2) then
    ShowAbfStatusPanelsEditor(Designer, Component as TabfStatusBar)
  else
    inherited ExecuteVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfStatusBarEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 2) then Result := SabfStatusBars_DesignerMenuItem
  else Result := inherited GetVerb(Index);
end;

//------------------------------------------------------------------------------

function TabfStatusBarEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

//------------------------------------------------------------------------------

end{unit abfStatusBarsReg}.
