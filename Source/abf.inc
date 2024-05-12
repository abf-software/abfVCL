{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}

//==============================================================================
// Default compiler options.
//==============================================================================
{$B-} // Complete Boolean Evaluation
{$R-} // Range-Checking
{$T-} // Typed @ operator
{$X+} // Extended syntax
{$P+} // Open string params
{$J+} // Writeable structured consts
{$H+} // Use long strings by default
{$Q-} // Overflow checking


//==============================================================================
// Determination of the Delphi/C++Builder version.
//==============================================================================

{$Define Unknown_Compiler}

//------------------------------------------------------------------------------
// Delphi 1

{$IfDef VER80}
  {$Define D1}
  {$Define D1_ONLY}
  {$Define Delphi}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 2

{$IfDef VER90}
  {$Define D2}
  {$Define D2_ONLY}
  {$Define Delphi}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// C++Builder 1

{$IfDef VER93}
  {$Define C1}
  {$Define C1_ONLY}
  {$Define Builder}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 3

{$IfDef VER100}
  {$Define D3}
  {$Define D3_ONLY}
  {$Define Delphi}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// C++Builder 3

{$IfDef VER110}
  {$Define C3}
  {$Define C3_ONLY}
  {$Define Builder}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 4

{$IfDef VER120}
  {$Define D4}
  {$Define D4_ONLY}
  {$Define Delphi}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// C++Builder 4

{$IfDef VER125}
  {$Define C4}
  {$Define C4_ONLY}
  {$Define Builder}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi / C++Builder 5

{$IfDef VER130}
  {$IfDef BCB}
    {$Define C5}
    {$Define C5_ONLY}
    {$Define Builder}
  {$Else BCB}
    {$Define D5}
    {$Define D5_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 6 / C++Builder 6

{$IfDef VER140}
  {$IfDef BCB}
    {$Define C6}
    {$Define C6_ONLY}
    {$Define Builder}
  {$Else BCB}
    {$Define D6}
    {$Define D6_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 7

{$IfDef VER150}
  {$Define D7}
  {$Define D7_ONLY}
  {$Define Delphi}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 8

{$IfDef VER160}
  {$Define D8}
  {$Define D8_ONLY}
  {$Define Delphi}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 9

{$IfDef VER170}
  {$Define D9}
  {$Define D9_ONLY}
  {$Define Delphi}
  {$UnDef Unknown_Compiler}
{$EndIf}

//------------------------------------------------------------------------------
// Delphi 10-11 / C++Builder 10-11

{$IfDef VER180}
  {$IfDef VER185} // Delphi 11 / C++Builder 11
    {$IfDef BCB}
      {$Define D9}
      {$Define C11}
      {$Define C11_ONLY}
      {$Define Builder}
    {$Else BCB}
      {$Define D11}
      {$Define D11_ONLY}
      {$Define Delphi}
    {$EndIf BCB}
  {$Else VER185} // Delphi 10 / C++Builder 10
    {$IfDef BCB}
      {$Define D9}
      {$Define C10}
      {$Define C10_ONLY}
      {$Define Builder}
      {$ObjExportAll On}
    {$Else BCB}
      {$Define D10}
      {$Define D10_ONLY}
      {$Define Delphi}
    {$EndIf BCB}
  {$EndIf}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER190}
  {$IfDef BCB}
    {$Define C9}
    {$Define C11}
    {$Define C11_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D11}
    {$Define D11_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER200}
  {$IfDef BCB}
    {$Define D9}
    {$Define C12}
    {$Define C12_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D12}
    {$Define D12_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER210}
  {$IfDef BCB}
    {$Define C13}
    {$Define C13_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D12}
    {$Define D12_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER220}
  {$IfDef BCB}
    {$Define C13}
    {$Define C13_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D13}
    {$Define D13_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER230}
  {$IfDef BCB}
    {$Define C14}
    {$Define C14_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D14}
    {$Define D14_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER230}
  {$IfDef BCB}
    {$Define C14}
    {$Define C14_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D14}
    {$Define D14_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER240}
  {$IfDef BCB}
    {$Define C14}
    {$Define C14_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D14}
    {$Define D14_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER250}
  {$IfDef BCB}
    {$Define C15}
    {$Define C15_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D15}
    {$Define D15_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER260}
  {$IfDef BCB}
    {$Define C17}
    {$Define C17_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D17}
    {$Define D17_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER270}
  {$IfDef BCB}
    {$Define C18}
    {$Define C18_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D19}
    {$Define D19_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER280}
  {$IfDef BCB}
    {$Define C20}
    {$Define C20_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D20}
    {$Define D20_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER290}
  {$IfDef BCB}
    {$Define C9}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D9}
    {$Define D21}
    {$Define D21_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER300}
  {$IfDef BCB}
    {$Define C22}
    {$Define C22_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D22}
    {$Define D22_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER310}
  {$IfDef BCB}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D23}
    {$Define D23_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}
{$IfDef VER320}
  {$IfDef BCB}
    {$Define C23}
    {$Define C23_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
  {$DEFINE DFMT}
    {$Define D9}
    {$Define D23}
    {$Define D23_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}
{$IfDef VER330}
  {$IfDef BCB}
  {$DEFINE DFMT}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
  {$DEFINE DFMT}
  {$DEFINE DFMT}
    {$Define D9}
    {$Define D24}
    {$Define D24_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}
{$IfDef VER340}
  {$IfDef BCB}
  {$DEFINE DFMT}
    {$Define C25}
    {$Define C25_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
  {$DEFINE DFMT}
    {$Define D9}
    {$Define D25}
    {$Define D25_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER350} // Delphi 11.3
  {$DEFINE DFMT}
  {$IfDef BCB}
    {$Define C26}
    {$Define C26_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D9}
    {$Define D26}
    {$Define D26_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER360}
  {$DEFINE DFMT}
  {$IfDef BCB}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D9}
    {$Define D27}
    {$Define D27_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER370}
  {$DEFINE DFMT}
  {$IfDef BCB}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D28}
    {$Define D28_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER380}
  {$DEFINE DFMT}
  {$IfDef BCB}
    {$Define C9}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D9}
    {$Define D29}
    {$Define D29_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}
// Forward looking
{$IfDef VER390}
  {$IfDef BCB}
    {$Define C9}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
    1/
  {$Else BCB}
    {$Define D9}
    {$Define D21}
    {$Define D21_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}

{$IfDef VER400}
  {$IfDef BCB}
    {$Define C9}
    {$Define C21}
    {$Define C21_ONLY}
    {$Define Builder}
    {$ObjExportAll On}
  {$Else BCB}
    {$Define D9}
    {$Define D21}
    {$Define D21_ONLY}
    {$Define Delphi}
  {$EndIf BCB}
  {$UnDef Unknown_Compiler}
{$EndIf}


//------------------------------------------------------------------------------
// Definitions for an unknown compiler

{$IfDef Unknown_Compiler}
  {$Define D11}
  {$Define D11_ONLY}
  {$Define Delphi}
{$EndIf}

//------------------------------------------------------------------------------
// Definition mappings

{$IfDef D11}
  {$Define D10}
  {$Define C10}
{$EndIf}

{$IfDef C11}
  {$Define D11}
  {$Define D10}
  {$Define C10}
{$EndIf}

{$IfDef D10}
  {$Define D9}
{$EndIf}

{$IfDef C10}
  {$Define D10}
  {$Define D9}
{$EndIf}

{$IfDef D9}
  {$Define D8}
{$EndIf}

{$IfDef D8}
  {$Define D7}
{$EndIf}

{$IfDef D7}
  {$Define D6}
  {$Define C6}
{$EndIf}

{$IfDef D6}
  {$Define D5}
  {$Define C5}
{$EndIf}

{$IfDef C6}
  {$Define D6}
  {$Define D5}
  {$Define C5}
{$EndIf}

{$IfDef D5}
  {$Define D4}
  {$Define C4}
{$EndIf}

{$IfDef C5}
  {$Define D5}
  {$Define D4}
  {$Define C4}
{$EndIf}

{$IfDef D4}
  {$Define D3}
  {$Define C3}
{$EndIf}

{$IfDef C4}
  {$Define D4}
  {$Define D3}
  {$Define C3}
{$EndIf}

{$IfDef D3}
  {$Define D2}
  {$Define C1}
{$EndIf}

{$IfDef C3}
  {$Define D3}
  {$Define D2}
  {$Define C1}
{$EndIf}

{$IfDef D2}
  {$Define D1}
{$EndIf}

{$IfDef C1}
  {$Define D2}
  {$Define D1}
{$EndIf}

//------------------------------------------------------------------------------
// Switch off some warning messages

{$IfDef D7}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$EndIf}

{$IfDef D6}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_LIBRARY OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN UNIT_LIBRARY OFF}
  {$WARN UNIT_PLATFORM OFF}
{$EndIf}

{$IfDef D9}
  {$WARN SYMBOL_EXPERIMENTAL OFF}
  {$WARN UNIT_EXPERIMENTAL OFF}
{$EndIf}

//==============================================================================
// Directive abfVCLDebug turn on/off some debug messages and logs.
//==============================================================================
{$IfDef DEBUG}
//{$Define abfVCLDebug}
{$EndIf}

//==============================================================================
// Directive abfVCLTrial turn on/off the trial message.
//==============================================================================
//{$Define abfVCLTrial}


//==============================================================================
// Enables exteneded color set (abfGraphics.pas)
//==============================================================================
{$Define abfColors}

