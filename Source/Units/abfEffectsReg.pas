{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEffectsReg;

{$I abf.inc}
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
{$IfDef D6}
  DesignIntf, DesignEditors,
{$Else D6}
  DsgnIntf,
{$EndIf D6}
  abfComponentsReg, // don't remove this line !!!
  Classes, SysUtils;

type

//==============================================================================
// TabfFillTypeEditor
//==============================================================================
// FillType property editor for TabfBackGround control.

  TabfFillTypeProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
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
  abfConsts, abfEffects;

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
  abfControlsReg.Register;

  RegisterComponents(SabfComponentPaletteNonVisual,
    [TabfFormEffect]);
  RegisterComponents(SabfComponentPaletteVisual,
    [TabfBackGround, TabfCredits, TabfMovablePanel, TabfMagnifier]);

// Assign component editors
  RegisterComponentEditor(TabfCustomBackGround, TabfControlEditor);
  RegisterComponentEditor(TabfMovablePanel, TabfControlEditor);

// Assign property editors
  RegisterPropertyEditor(TypeInfo(string), TabfCustomBackGround,
    SabfAboutProperty, TabfAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TabfMovablePanel,
    SabfAboutProperty, TabfAboutProperty);
  RegisterPropertyEditor(TypeInfo(TabfBackGroundFillType), TabfCustomBackGround,
    'FillType', TabfFillTypeProperty);
end;


//==============================================================================
// TabfFillTypeProperty
//==============================================================================
// FillType property editor for TabfBackGround control.
{ TabfFillTypeProperty } 

function TabfFillTypeProperty.GetAttributes:TPropertyAttributes;
begin
  Result := [paValueList];
end;

//------------------------------------------------------------------------------

procedure TabfFillTypeProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to abfGraphicFillList.Count - 1 do
    Proc(abfGraphicFillList[i]);
end;

//------------------------------------------------------------------------------

end{unit abfEffectsReg}.
