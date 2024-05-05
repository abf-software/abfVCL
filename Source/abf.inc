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

