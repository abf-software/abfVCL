{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfReg;

{$I abf.inc}

(*******************************************************************************
Use abf_vcl.inc file to define which part of ABF VCL should be installed or not.
For example: if you don't want to use abfComboBoxes, simple remove (or comment)
{$Define abfComboBoxes} directive in the abf_vcl.inc file.
*******************************************************************************)
{$I abf_vcl.inc}

interface

uses
{$IfDef D6}
  DesignIntf,
{$Else D6}
  DsgnIntf,
{$EndIf D6}
  Forms;

//==============================================================================
// Registration
//==============================================================================

procedure Register;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfControls, abfControlsReg
{$IfDef abfComboBoxes} , abfComboBoxesReg  {$EndIf abfComboBoxes}
{$IfDef abfComControls}, abfComControlsReg {$EndIf abfComControls}
{$IfDef abfDialogs}    , abfDialogsReg     {$EndIf abfDialogs}
{$IfDef abfEdits}      , abfEditsReg       {$EndIf abfEdits}
{$IfDef abfLabels}     , abfLabelsReg      {$EndIf abfLabels}
{$IfDef abfMenus}      , abfMenusReg       {$EndIf abfMenus}
{$IfDef abfStatusBars} , abfStatusBarsReg  {$EndIf abfStatusBars}
{$IfDef D3}
  {$IfDef abfAPM}      , abfAPMReg         {$EndIf abfAPM}
  {$IfDef abfEffects}  , abfEffectsReg     {$EndIf abfEffects}
  {$IfDef abfWAB}      , abfWABReg         {$EndIf abfWAB}
{$EndIf D3}
  ;{uses}

//==============================================================================
// Registration
//==============================================================================

procedure Register;
begin
  abfControlsReg.Register;
{$IfDef abfComboBoxes}  abfComboBoxesReg .Register; {$EndIf abfComboBoxes}
{$IfDef abfComControls} abfComControlsReg.Register; {$EndIf abfComControls}
{$IfDef abfDialogs}     abfDialogsReg    .Register; {$EndIf abfDialogs}
{$IfDef abfDialogs}     abfEditsReg      .Register; {$EndIf abfDialogs}
{$IfDef abfLabels}      abfLabelsReg     .Register; {$EndIf abfLabels}
{$IfDef abfMenus}       abfMenusReg      .Register; {$EndIf abfMenus}
{$IfDef abfStatusBars}  abfStatusBarsReg .Register; {$EndIf abfStatusBars}
{$IfDef D3}
  {$IfDef abfAPM}       abfAPMReg        .Register; {$EndIf abfAPM}
  {$IfDef abfEffects}   abfEffectsReg    .Register; {$EndIf abfEffects}
  {$IfDef abfWAB}       abfWABReg        .Register; {$EndIf abfWAB}
{$EndIf D3}


{$IfDef abfComboBoxes}
  RegisterComponentEditor(TabfCustomComboBox, TabfComboBoxEditor);
{$EndIf abfComboBoxes}
end;

//------------------------------------------------------------------------------

end{unit abfReg}.
