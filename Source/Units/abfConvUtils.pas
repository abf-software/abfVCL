{*******************************************************************************

  ABF Visual Components Library. Conversion utilities.

  Based on Borland and Project JEDI routines
  http://www.borland.com
  http://www.delphi-jedi.org

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfConvUtils;

{$I abf.inc}

interface

uses
  Windows, SysUtils, Math,
  abfClasses;

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}
  SConvIncompatibleTypes2 = 'Incompatible conversion types [%s, %s]';
  SConvIncompatibleTypes3 = 'Incompatible conversion types [%s, %s, %s]';
  SConvIncompatibleTypes4 = 'Incompatible conversion types [%s - %s, %s - %s]';
  SConvUnknownType = 'Unknown conversion type %s';
  SConvDuplicateType = 'Conversion type (%s) already registered';
  SConvUnknownFamily = 'Unknown conversion family %s';
  SConvDuplicateFamily = 'Conversion family (%s) already registered';
  SConvUnknownDescription = '[%.8x]';
  SConvIllegalType = 'Illegal type';
  SConvIllegalFamily = 'Illegal family';
  SConvFactorZero = '%s has a factor of zero';

type
  TabfConvFamily = type Word;
  TabfConvType = type Word;
  TabfConversionProc = function(const AValue: Double): Double;
  TabfConvTypeArray = array of TabfConvType;
  TabfConvFamilyArray = array of TabfConvFamily;
  TValueRelationship = -1..1;


type
  EabfConversionError = class(EabfException);

//==============================================================================
// TabfConvFamilyInfo
//==============================================================================

  TabfConvFamilyInfo = class(TObject)
  private
    FDescription: string;
    FConvFamily: TabfConvFamily;
  public
    constructor Create(const AConvFamily: TabfConvFamily;
      const ADescription: string);
    property ConvFamily: TabfConvFamily read FConvFamily;
    property Description: string read FDescription;
  end;

  TabfConvFamilyList = array of TabfConvFamilyInfo;


//==============================================================================
// TabfConvTypeInfo
//==============================================================================

  TabfConvTypeInfo = class(TObject)
  private
    FDescription: string;
    FConvFamily: TabfConvFamily;
    FConvType: TabfConvType;
    FAbbreviation: string;
  public
    constructor Create(const AConvFamily: TabfConvFamily;
      const ADescription, AAbbreviation: string);
    function ToCommon(const AValue: Double): Double; virtual; abstract;
    function FromCommon(const AValue: Double): Double; virtual; abstract;
    property ConvFamily: TabfConvFamily read FConvFamily;
    property ConvType: TabfConvType read FConvType;
    property Description: string read FDescription;
    property Abbreviation: string read FAbbreviation;
  end;

  TabfConvTypeList = array of TabfConvTypeInfo;


//==============================================================================
// TabfConvTypeFactor
//==============================================================================

  TabfConvTypeFactor = class(TabfConvTypeInfo)
  private
    FFactor: Double;
  public
    constructor Create(const AConvFamily: TabfConvFamily;
      const ADescription, AAbbreviation: string; const AFactor: Double);
    function ToCommon(const AValue: Double): Double; override;
    function FromCommon(const AValue: Double): Double; override;
  end;


//==============================================================================
// TabfConvTypeProcs
//==============================================================================

  TabfConvTypeProcs = class(TabfConvTypeInfo)
  private
    FToCommonProc: TabfConversionProc;
    FFromCommonProc: TabfConversionProc;
  public
    constructor Create(const AConvFamily: TabfConvFamily;
      const ADescription, AAbbreviation: string; const AToCommonProc,
      AFromCommonProc: TabfConversionProc);
    function ToCommon(const AValue: Double): Double; override;
    function FromCommon(const AValue: Double): Double; override;
  end;


//==============================================================================
// Routines

//------------------------------------------------------------------------------
// Simple conversion between two measurement types
function Convert(const AValue: Double; const AFrom,
  ATo: TabfConvType): Double; overload;

//------------------------------------------------------------------------------
// Complex conversion between two double measurement types.  An example of
//  this would be converting miles per hour to meters per minute (speed) or
//  gallons per minute to liters per hour (flow).  There are lots of
//  combinations but not all of them make much sense.
function Convert(const AValue: Double; const AFrom1, AFrom2, ATo1,
  ATo2: TabfConvType): Double; overload;

//------------------------------------------------------------------------------
// Convert from and to the base unit type for a particular conversion family
function ConvertFrom(const AFrom: TabfConvType; const AValue: Double): Double;
function ConvertTo(const AValue: Double; const ATo: TabfConvType): Double;

//------------------------------------------------------------------------------
// Add two values together and return the result in a specified type
function ConvUnitAdd(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2, AResultType: TabfConvType): Double;

//------------------------------------------------------------------------------
// Subtract one values from other and return the result in a specified type
function ConvUnitDiff(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2, AResultType: TabfConvType): Double;

//------------------------------------------------------------------------------
// Increment a value by a value of a specified type
function ConvUnitInc(const AValue: Double;
  const AType, AAmountType: TabfConvType): Double; overload;
function ConvUnitInc(const AValue: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Double; overload;

//------------------------------------------------------------------------------
// Decrement a value by a value of a specified type
function ConvUnitDec(const AValue: Double;
  const AType, AAmountType: TabfConvType): Double; overload;
function ConvUnitDec(const AValue: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Double; overload;

//------------------------------------------------------------------------------
// Test to see if a given value is within the previous given units of a certian
// type
function ConvUnitWithinPrevious(const AValue, ATest: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Boolean;

//------------------------------------------------------------------------------
// Test to see if a given value is within the next given units of a certian type
function ConvUnitWithinNext(const AValue, ATest: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Boolean;

//------------------------------------------------------------------------------
// Comparison and Equality testing
function ConvUnitCompareValue(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2: TabfConvType): TValueRelationship;
function ConvUnitSameValue(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2: TabfConvType): Boolean;

//------------------------------------------------------------------------------
// Allows check if two conversion types are compatible.
function CompatibleConversionTypes(const AFrom, ATo: TabfConvType): Boolean;

//------------------------------------------------------------------------------
// Test to see if the type is part of particular family
function CompatibleConversionType(const AType: TabfConvType;
  const AFamily: TabfConvFamily): Boolean;

//------------------------------------------------------------------------------
// Registation/UnRegistation of conversion types.  The only one that has to
// truly unregister are the ones with callbacks (ie. not the simple factor ones)
function RegisterConversionType(const AFamily: TabfConvFamily;
  const ADescription, AAbbreviation: string;
  const AFactor: Double): TabfConvType; overload;
function RegisterConversionType(const AFamily: TabfConvFamily;
  const ADescription, AAbbreviation: string; const AToCommonProc,
  AFromCommonProc: TabfConversionProc): TabfConvType; overload;
procedure UnregisterConversionType(const AType: TabfConvType);

//------------------------------------------------------------------------------
// Registation/UnRegistation of conversion families.  You really don't need to
// unregister the conversion families, they will be cleaned up when your app
// shuts down.
function RegisterConversionFamily(const ADescription: string): TabfConvFamily;
procedure UnregisterConversionFamily(const AFamily: TabfConvFamily);

//------------------------------------------------------------------------------
// Conversion support
function StrToConvUnit(AText: string; var AValue: Double; var AType: TabfConvType): Boolean;
function ConvUnitToStr(const AValue: Double; const AType: TabfConvType): string;
function ConvUnitToShortStr(const AValue: Double;
  const AType: TabfConvType): string;

//------------------------------------------------------------------------------
// Discovery support functions
procedure GetConvTypes(const AFamily: TabfConvFamily; out ATypes: TabfConvTypeArray);
procedure GetConvFamilies(out AFamilies: TabfConvFamilyArray);

function ConvTypeToDescription(const AType: TabfConvType): string;
function ConvTypeToAbbreviation(const AType: TabfConvType): string;
function ConvFamilyToDescription(const AFamily: TabfConvFamily): string;
function DescriptionToConvType(const ADescription: string; var AType: TabfConvType): Boolean; overload;
function DescriptionToConvType(const AFamily: TabfConvFamily; const ADescription: string): TabfConvType; overload;
function DescriptionToConvFamily(const ADescription: string): TabfConvFamily;

function ConvTypeToFamily(const AType: TabfConvType): TabfConvFamily; overload;
function ConvTypeToFamily(const AFrom, ATo: TabfConvType): TabfConvFamily; overload;
function ConvTypeToFamily(const AFrom, ATo: TabfConvType;
  var AFamily: TabfConvFamily): Boolean; overload;

procedure RaiseConversionError(const AText: string); overload;
procedure RaiseConversionError(const AText: string; const AArgs: array of const); overload;
procedure RaiseConversionRegError(AFamily: TabfConvFamily;
  const ADescription: string);

  
//==============================================================================
// Globals
//==============================================================================

const
  CListGrowthDelta = 16;
  CIllegalConvFamily: TabfConvFamily = 0;
  CIllegalConvType: TabfConvType = 0;

var
  GConvFamilyList: TabfConvFamilyList;
  GConvTypeList: TabfConvTypeList;
  GLasTabfConvFamily: TabfConvFamily;
  GLasTabfConvType: TabfConvType;
  GConvFamilySync: TMultiReadExclusiveWriteSynchronizer;
  GConvTypeSync: TMultiReadExclusiveWriteSynchronizer;
  GConvUnitToStrFmt: string = '%f %s';


{******************************************************************************}
implementation
{******************************************************************************}

uses
  SysConst;

const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);

  FuzzFactor = 1000;
//  ExtendedResolution = 1E-19 * FuzzFactor;
  DoubleResolution   = 1E-15 * FuzzFactor;
//  SingleResolution   = 1E-7 * FuzzFactor;

//------------------------------------------------------------------------------
{
function SameValue(const A, B: Extended; Epsilon: Extended): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := Min(Abs(A), Abs(B)) * ExtendedResolution;
  Result := Abs(A - B) < Epsilon;
end;{}

//------------------------------------------------------------------------------

function SameValue(const A, B: Double; Epsilon: Double): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := Min(Abs(A), Abs(B)) * DoubleResolution;
  Result := Abs(A - B) < Epsilon;
end;

//------------------------------------------------------------------------------
{
function CompareValue(const A, B: Extended; Epsilon: Extended): TValueRelationship;
begin
  if SameValue(A, B, Epsilon) then
    Result := EqualsValue
  else if A < B then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;{}

//------------------------------------------------------------------------------

function CompareValue(const A, B: Double; Epsilon: Double): TValueRelationship;
begin
  if SameValue(A, B, Epsilon) then
    Result := EqualsValue
  else if A < B then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;


//==============================================================================
// Local Funtions and Procedures
//==============================================================================

function GetConvFamilyInfo(const AFamily: TabfConvFamily;
  var AConvFamilyInfo: TabfConvFamilyInfo): Boolean;
begin
  GConvFamilySync.BeginRead;
  try
    Result := AFamily < Length(GConvFamilyList);
    if Result then
    begin
      AConvFamilyInfo := GConvFamilyList[AFamily];
      Result := Assigned(AConvFamilyInfo);
    end;
  finally
    GConvFamilySync.EndRead;
  end;
end;

//------------------------------------------------------------------------------

function GetConvInfo(const AType: TabfConvType;
  var AConvTypeInfo: TabfConvTypeInfo): Boolean; overload;
begin
  GConvTypeSync.BeginRead;
  try
    Result := AType < Length(GConvTypeList);
    if Result then
    begin
      AConvTypeInfo := GConvTypeList[AType];
      Result := Assigned(AConvTypeInfo);
    end;
  finally
    GConvTypeSync.EndRead;
  end;
end;

//------------------------------------------------------------------------------

function GetConvInfo(const AType: TabfConvType; var AConvTypeInfo: TabfConvTypeInfo;
  var AConvFamilyInfo: TabfConvFamilyInfo): Boolean; overload;
begin
  Result := GetConvInfo(AType, AConvTypeInfo) and
    GetConvFamilyInfo(AConvTypeInfo.ConvFamily, AConvFamilyInfo);
end;

//------------------------------------------------------------------------------

function GetConvInfo(const AType: TabfConvType;
  var AConvFamilyInfo: TabfConvFamilyInfo): Boolean; overload;
var
  LConvTypeInfo: TabfConvTypeInfo;
begin
  Result := GetConvInfo(AType, LConvTypeInfo) and
    GetConvFamilyInfo(LConvTypeInfo.ConvFamily, AConvFamilyInfo);
end;

//------------------------------------------------------------------------------

function GetConvInfo(const AFrom, ATo: TabfConvType; var AFromTypeInfo,
  AToTypeInfo: TabfConvTypeInfo;
  var AConvFamilyInfo: TabfConvFamilyInfo): Boolean; overload;
var
  LFromFamilyInfo: TabfConvFamilyInfo;
begin
  Result := GetConvInfo(AFrom, AFromTypeInfo, LFromFamilyInfo) and
    GetConvInfo(ATo, AToTypeInfo, AConvFamilyInfo) and
    (AConvFamilyInfo = LFromFamilyInfo);
end;

//------------------------------------------------------------------------------

function GetConvInfo(const AFrom, ATo: TabfConvType; var AFromTypeInfo,
  AToTypeInfo: TabfConvTypeInfo): Boolean; overload;
begin
  Result := GetConvInfo(AFrom, AFromTypeInfo) and
    GetConvInfo(ATo, AToTypeInfo) and
    (AFromTypeInfo.ConvFamily = AToTypeInfo.ConvFamily);
end;

//------------------------------------------------------------------------------

function GetConvInfo(const AFrom, ATo, AResult: TabfConvType; var AFromTypeInfo,
  AToTypeInfo, AResultTypeInfo: TabfConvTypeInfo): Boolean; overload;
begin
  Result := GetConvInfo(AFrom, AFromTypeInfo) and
    GetConvInfo(ATo, AToTypeInfo) and
    GetConvInfo(AResult, AResultTypeInfo) and
    (AFromTypeInfo.ConvFamily = AToTypeInfo.ConvFamily) and
    (AToTypeInfo.ConvFamily = AResultTypeInfo.ConvFamily);
end;

//------------------------------------------------------------------------------

function GetConvInfo(const AFrom, ATo: TabfConvType;
  var AConvFamilyInfo: TabfConvFamilyInfo): Boolean; overload;
var
  LFromFamilyInfo: TabfConvFamilyInfo;
begin
  Result := GetConvInfo(AFrom, LFromFamilyInfo) and
    GetConvInfo(ATo, AConvFamilyInfo) and
    (AConvFamilyInfo = LFromFamilyInfo);
end;

//------------------------------------------------------------------------------

procedure FreeConversionType(const AType: TabfConvType);
var
  LConvTypeInfo: TabfConvTypeInfo;
begin
  if GetConvInfo(AType, LConvTypeInfo) then
  begin
    GConvTypeList[AType] := nil;
    LConvTypeInfo.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure CleanUpLists;
var
  I: Integer;
  LConvFamilyInfo: TabfConvFamilyInfo;
  LConvTypeInfo: TabfConvTypeInfo;
begin
  GConvTypeSync.BeginWrite;
  try
    for I := 0 to Length(GConvTypeList) - 1 do
    begin
      LConvTypeInfo := GConvTypeList[I];
      if Assigned(LConvTypeInfo) then
      begin
        GConvTypeList[I] := nil;
        LConvTypeInfo.Free;
      end;
    end;
    SetLength(GConvTypeList, 0);
  finally
    GConvTypeSync.EndWrite;
  end;

  GConvFamilySync.BeginWrite;
  try
    for I := 0 to Length(GConvFamilyList) - 1 do
    begin
      LConvFamilyInfo := GConvFamilyList[I];
      if Assigned(LConvFamilyInfo) then
      begin
        GConvFamilyList[I] := nil;
        LConvFamilyInfo.Free;
      end;
    end;
    SetLength(GConvFamilyList, 0);
  finally
    GConvFamilySync.EndWrite;
  end;
end;


//==============================================================================
// Published Funtions and Procedures
//==============================================================================

//------------------------------------------------------------------------------
// Simple conversion between two measurement types

function Convert(const AValue: Double; const AFrom, ATo: TabfConvType): Double;
var
  LFromTypeInfo, LToTypeInfo: TabfConvTypeInfo;
begin
  if not GetConvInfo(AFrom, ATo, LFromTypeInfo, LToTypeInfo) then
    RaiseConversionError(SConvIncompatibleTypes2,
      [ConvTypeToDescription(AFrom), ConvTypeToDescription(ATo)]);
  Result := LToTypeInfo.FromCommon(LFromTypeInfo.ToCommon(AValue));
end;

//------------------------------------------------------------------------------
// Complex conversion between two double measurement types.  An example of
//  this would be converting miles per hour to meters per minute (speed) or
//  gallons per minute to liters per hour (flow).  There are lots of
//  combinations but not all of them make much sense.

function Convert(const AValue: Double; const AFrom1, AFrom2, ATo1, ATo2: TabfConvType): Double;
begin
  Result := Convert(Convert(AValue, AFrom1, ATo1), ATo2, AFrom2);
end;

//------------------------------------------------------------------------------

function ConvertFrom(const AFrom: TabfConvType; const AValue: Double): Double;
var
  LFromTypeInfo: TabfConvTypeInfo;
begin
  if not GetConvInfo(AFrom, LFromTypeInfo) then
    RaiseConversionError(SConvUnknownType,
      [Format(SConvUnknownDescription, [AFrom])]);
  Result := LFromTypeInfo.ToCommon(AValue);
end;

//------------------------------------------------------------------------------

function ConvertTo(const AValue: Double; const ATo: TabfConvType): Double;
var
  LToTypeInfo: TabfConvTypeInfo;
begin
  if not GetConvInfo(ATo, LToTypeInfo) then
    RaiseConversionError(SConvUnknownType,
      [Format(SConvUnknownDescription, [ATo])]);
  Result := LToTypeInfo.FromCommon(AValue);
end;

//------------------------------------------------------------------------------
// Add two values together and return the result in a specified type

function ConvUnitAdd(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2, AResultType: TabfConvType): Double;
var
  LType1Info, LType2Info, LResultTypeInfo: TabfConvTypeInfo;
begin
  if not GetConvInfo(AType1, AType2, AResultType, LType1Info, LType2Info,
    LResultTypeInfo) then
    RaiseConversionError(SConvIncompatibleTypes3,
      [ConvTypeToDescription(AType1),
       ConvTypeToDescription(AType2),
       ConvTypeToDescription(AResultType)]);
  Result := LResultTypeInfo.FromCommon(LType1Info.ToCommon(AValue1) +
    LType2Info.ToCommon(AValue2));
end;

//------------------------------------------------------------------------------
// Subtract one values from other and return the result in a specified type

function ConvUnitDiff(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2, AResultType: TabfConvType): Double;
begin
  Result := ConvUnitAdd(AValue1, AType1, -AValue2, AType2, AResultType);
end;

//------------------------------------------------------------------------------
// Increment a value by a value of a specified type

function ConvUnitInc(const AValue: Double; const AType,
  AAmountType: TabfConvType): Double;
begin
  Result := ConvUnitInc(AValue, AType, 1, AAmountType);
end;

//------------------------------------------------------------------------------

function ConvUnitInc(const AValue: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Double;
var
  LTypeInfo, LAmountTypeInfo: TabfConvTypeInfo;
begin
  if not GetConvInfo(AType, AAmountType, LTypeInfo, LAmountTypeInfo) then
    RaiseConversionError(SConvIncompatibleTypes2,
      [ConvTypeToDescription(AType),
       ConvTypeToDescription(AAmountType)]);
  Result := AValue + LTypeInfo.FromCommon(LAmountTypeInfo.ToCommon(AAmount));
end;

//------------------------------------------------------------------------------
// Decrement a value by a value of a specified type

function ConvUnitDec(const AValue: Double;
  const AType, AAmountType: TabfConvType): Double;
begin
  Result := ConvUnitInc(AValue, AType, -1, AAmountType);
end;

//------------------------------------------------------------------------------

function ConvUnitDec(const AValue: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Double;
begin
  Result := ConvUnitInc(AValue, AType, -AAmount, AAmountType);
end;

//------------------------------------------------------------------------------
// Test to see if a given value is within the previous given units of a certian
// type

function ConvUnitWithinPrevious(const AValue, ATest: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Boolean;
begin
  Result := (ATest <= AValue) and
    (ATest >= ConvUnitDec(AValue, AType, AAmount, AAmountType));
end;

//------------------------------------------------------------------------------
// Test to see if a given value is within the next given units of a certian type

function ConvUnitWithinNext(const AValue, ATest: Double; const AType: TabfConvType;
  const AAmount: Double; const AAmountType: TabfConvType): Boolean;
begin
  Result := (ATest >= AValue) and
    (ATest <= ConvUnitInc(AValue, AType, AAmount, AAmountType));
end;

//------------------------------------------------------------------------------
// Comparison and Equality testing

function ConvUnitCompareValue(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2: TabfConvType): TValueRelationship;
var
  LType1Info, LType2Info: TabfConvTypeInfo;
begin
  if not GetConvInfo(AType1, AType2, LType1Info, LType2Info) then
    RaiseConversionError(SConvIncompatibleTypes2,
      [ConvTypeToDescription(AType1), ConvTypeToDescription(AType2)]);
  Result := CompareValue(LType1Info.ToCommon(AValue1),
    LType2Info.ToCommon(AValue2), 0.0);
end;

//------------------------------------------------------------------------------

function ConvUnitSameValue(const AValue1: Double; const AType1: TabfConvType;
  const AValue2: Double; const AType2: TabfConvType): Boolean;
var
  LType1Info, LType2Info: TabfConvTypeInfo;
begin
  if not GetConvInfo(AType1, AType2, LType1Info, LType2Info) then
    RaiseConversionError(SConvIncompatibleTypes2,
      [ConvTypeToDescription(AType1), ConvTypeToDescription(AType2)]);
  Result := SameValue(LType1Info.ToCommon(AValue1),
    LType2Info.ToCommon(AValue2), 0.0);
end;

//------------------------------------------------------------------------------

function CompatibleConversionTypes(const AFrom, ATo: TabfConvType): Boolean;
begin
  Result := ConvTypeToFamily(AFrom, ATo) <> CIllegalConvFamily;
end;

//------------------------------------------------------------------------------
// Test to see if the type is part of particular family

function CompatibleConversionType(const AType: TabfConvType; const AFamily: TabfConvFamily): Boolean;
var
  LTypeInfo: TabfConvTypeInfo;
begin
  if not GetConvInfo(AType, LTypeInfo) then
    RaiseConversionError(SConvUnknownType,
      [Format(SConvUnknownDescription, [AType])]);
  Result := (LTypeInfo.ConvFamily = AFamily);
end;

//------------------------------------------------------------------------------
// Registation/UnRegistation of conversion types.  The only one that has to
// truly unregister are the ones with callbacks (ie. not the simple factor ones)

function InternalRegisterConversionType(const AConvType: TabfConvTypeInfo): TabfConvType;
begin
  Inc(GLasTabfConvType);
  if GLasTabfConvType > Length(GConvTypeList) - 1 then
    SetLength(GConvTypeList, GLasTabfConvType + CListGrowthDelta);
  Result := GLasTabfConvType;
  AConvType.FConvType := Result;
  GConvTypeList[Result] := AConvType;
end;

//------------------------------------------------------------------------------

function RegisterConversionType(const AFamily: TabfConvFamily;
  const ADescription, AAbbreviation: string; const AFactor: Double): TabfConvType;
begin
  GConvTypeSync.BeginWrite;
  try
    if DescriptionToConvType(AFamily, ADescription) <> CIllegalConvType then
      RaiseConversionError(SConvDuplicateType, [ADescription]);
    Result := InternalRegisterConversionType(TabfConvTypeFactor.Create(AFamily,
      ADescription, AAbbreviation, AFactor));
  finally
    GConvTypeSync.EndWrite;
  end;
end;

//------------------------------------------------------------------------------

function RegisterConversionType(const AFamily: TabfConvFamily;
  const ADescription, AAbbreviation: string; const AToCommonProc,
   AFromCommonProc: TabfConversionProc): TabfConvType;
begin
  GConvTypeSync.BeginWrite;
  try
    if DescriptionToConvType(AFamily, ADescription) <> CIllegalConvType then
      RaiseConversionError(SConvDuplicateType, [ADescription]);
    Result := InternalRegisterConversionType(TabfConvTypeProcs.Create(AFamily,
      ADescription, AAbbreviation, AToCommonProc, AFromCommonProc));
  finally
    GConvTypeSync.EndWrite;
  end;
end;

//------------------------------------------------------------------------------

procedure UnregisterConversionType(const AType: TabfConvType);
begin
  GConvTypeSync.BeginWrite;
  try
    FreeConversionType(AType);
  finally
    GConvTypeSync.EndWrite;
  end;
end;

//------------------------------------------------------------------------------
// Registation/UnRegistation of conversion families.  You really don't need to
// unregister the conversion families, they will be cleaned up when your app
// shuts down.

function RegisterConversionFamily(const ADescription: string): TabfConvFamily;
begin
  GConvFamilySync.BeginWrite;
  try
    if DescriptionToConvFamily(ADescription) <> CIllegalConvFamily then
      RaiseConversionError(SConvDuplicateFamily, [ADescription]);
    Inc(GLasTabfConvFamily);
    if GLasTabfConvFamily > Length(GConvFamilyList) - 1 then
      SetLength(GConvFamilyList, GLasTabfConvFamily + CListGrowthDelta);
    Result := GLasTabfConvFamily;
    GConvFamilyList[Result] := TabfConvFamilyInfo.Create(Result, ADescription);
  finally
    GConvFamilySync.EndWrite;
  end;
end;

//------------------------------------------------------------------------------

procedure UnregisterConversionFamily(const AFamily: TabfConvFamily);
var
  I: Integer;
  LConvFamilyInfo: TabfConvFamilyInfo;
begin
  GConvFamilySync.BeginWrite;
  try
    if GetConvFamilyInfo(AFamily, LConvFamilyInfo) then
    begin
      GConvTypeSync.BeginWrite;
      try
        for I := 0 to Length(GConvTypeList) - 1 do
          if Assigned(GConvTypeList[I]) and
             (GConvTypeList[I].FConvFamily = AFamily) then
            FreeConversionType(I);
      finally
        GConvTypeSync.EndWrite;
      end;
      GConvFamilyList[AFamily] := nil;
      LConvFamilyInfo.Free;
    end;
  finally
    GConvFamilySync.EndWrite;
  end;
end;

//------------------------------------------------------------------------------
// Conversion support

function StrToConvUnit(AText: string; var AValue: Double; var AType: TabfConvType): Boolean;
var
  LSpaceAt: Integer;
  LType: string;
begin
  AText := TrimLeft(AText);
  LSpaceAt := Pos(' ', AText);
  if LSpaceAt > 0 then
  begin
    AValue := StrToFloat(Copy(AText, 1, LSpaceAt - 1));
    LType := Trim(Copy(AText, LSpaceAt + 1, MaxInt));
    Result := (LType <> '') and DescriptionToConvType(LType, AType);
  end
  else
  begin
    AValue := StrToFloat(AText);
    AType := CIllegalConvType;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

function ConvUnitToStr(const AValue: Double; const AType: TabfConvType): string;
begin
  Result := Format(GConvUnitToStrFmt, [AValue, ConvTypeToDescription(AType)]);
end;

//------------------------------------------------------------------------------

function ConvUnitToShortStr(const AValue: Double; const AType: TabfConvType): string;
begin
  Result := Format(GConvUnitToStrFmt, [AValue, ConvTypeToAbbreviation(AType)]);
end;

//------------------------------------------------------------------------------
// Discovery support functions

procedure GetConvTypes(const AFamily: TabfConvFamily; out ATypes: TabfConvTypeArray);
var
  I, LCount: Integer;
begin
  GConvTypeSync.BeginRead;
  try
    LCount := 0;
    for I := 0 to Length(GConvTypeList) - 1 do
      if Assigned(GConvTypeList[I]) and
        (GConvTypeList[I].ConvFamily = AFamily) then Inc(LCount);
    SetLength(ATypes, LCount);
    LCount := 0;
    for I := 0 to Length(GConvTypeList) - 1 do
      if Assigned(GConvTypeList[I]) and
        (GConvTypeList[I].ConvFamily = AFamily) then
      begin
        ATypes[LCount] := GConvTypeList[I].ConvType;
        Inc(LCount);
      end;
  finally
    GConvTypeSync.EndRead;
  end;
end;

//------------------------------------------------------------------------------

procedure GetConvFamilies(out AFamilies: TabfConvFamilyArray);
var
  I, LCount: Integer;
begin
  GConvFamilySync.BeginRead;
  try
    LCount := 0;
    for I := 0 to Length(GConvFamilyList) - 1 do
      if Assigned(GConvFamilyList[I]) then
        Inc(LCount);
    SetLength(AFamilies, LCount);
    LCount := 0;
    for I := 0 to Length(GConvFamilyList) - 1 do
      if Assigned(GConvFamilyList[I]) then
      begin
        AFamilies[LCount] := GConvFamilyList[I].ConvFamily;
        Inc(LCount);
      end;
  finally
    GConvFamilySync.EndRead;
  end;
end;

//------------------------------------------------------------------------------

function ConvTypeToDescription(const AType: TabfConvType): string;
var
  LConvTypeInfo: TabfConvTypeInfo;
begin
  if AType = CIllegalConvType then
    Result := SConvIllegalType
  else
  if GetConvInfo(AType, LConvTypeInfo) then
    Result := LConvTypeInfo.Description
  else
    Result := Format(SConvUnknownDescription, [AType]);
end;

//------------------------------------------------------------------------------

function ConvTypeToAbbreviation(const AType: TabfConvType): string;
var
  LConvTypeInfo: TabfConvTypeInfo;
begin
  if AType = CIllegalConvType then
    Result := SConvIllegalType
  else
  if GetConvInfo(AType, LConvTypeInfo) then
    Result := LConvTypeInfo.Abbreviation
  else
    Result := Format(SConvUnknownDescription, [AType]);
end;

//------------------------------------------------------------------------------

function ConvFamilyToDescription(const AFamily: TabfConvFamily): string;
var
  LConvFamilyInfo: TabfConvFamilyInfo;
begin
  if AFamily = CIllegalConvFamily then
    Result := SConvIllegalFamily
  else
  if GetConvFamilyInfo(AFamily, LConvFamilyInfo) then
    Result := LConvFamilyInfo.Description
  else
    Result := Format(SConvUnknownDescription, [AFamily]);
end;

//------------------------------------------------------------------------------

function DescriptionToConvType(const ADescription: string; var AType: TabfConvType): Boolean;
var
  I: Integer;
begin
  GConvTypeSync.BeginRead;
  try
    AType := CIllegalConvType;
    for I := 0 to Length(GConvTypeList) - 1 do
      if Assigned(GConvTypeList[I]) and
         AnsiSameText(ADescription, GConvTypeList[I].Description) then
      begin
        if AType <> CIllegalConvType then
        begin
          Result := False;
          Exit;
        end;
        AType := I;
      end;
  finally
    GConvTypeSync.EndRead;
  end;
  Result := AType <> CIllegalConvType;
end;

//------------------------------------------------------------------------------

function DescriptionToConvType(const AFamily: TabfConvFamily; const ADescription: string): TabfConvType;
var
  I: Integer;
begin
  GConvTypeSync.BeginRead;
  try
    Result := CIllegalConvType;
    for I := 0 to Length(GConvTypeList) - 1 do
      if Assigned(GConvTypeList[I]) and
         (GConvTypeList[I].ConvFamily = AFamily) and
         AnsiSameText(ADescription, GConvTypeList[I].Description) then
      begin
        Result := I;
        Break;
      end;
  finally
    GConvTypeSync.EndRead;
  end;
end;

//------------------------------------------------------------------------------

function DescriptionToConvFamily(const ADescription: string): TabfConvFamily;
var
  I: Integer;
begin
  GConvFamilySync.BeginRead;
  try
    Result := CIllegalConvFamily;
    for I := 0 to Length(GConvFamilyList) - 1 do
      if Assigned(GConvFamilyList[I]) and
         AnsiSameText(ADescription, GConvFamilyList[I].Description) then
      begin
        Result := I;
        Break;
      end;
  finally
    GConvFamilySync.EndRead;
  end;
end;

//------------------------------------------------------------------------------

function ConvTypeToFamily(const AFrom, ATo: TabfConvType; var AFamily: TabfConvFamily): Boolean;
begin
  AFamily := ConvTypeToFamily(AFrom, ATo);
  Result := AFamily <> CIllegalConvFamily;
end;

//------------------------------------------------------------------------------

function ConvTypeToFamily(const AType: TabfConvType): TabfConvFamily;
var
  LTypeInfo: TabfConvTypeInfo;
begin
  if GetConvInfo(AType, LTypeInfo) then
    Result := LTypeInfo.ConvFamily
  else
    Result := CIllegalConvFamily;
end;

//------------------------------------------------------------------------------

function ConvTypeToFamily(const AFrom, ATo: TabfConvType): TabfConvFamily;
var
  LFromTypeInfo, LToTypeInfo: TabfConvTypeInfo;
begin
  if GetConvInfo(AFrom, LFromTypeInfo) and GetConvInfo(ATo, LToTypeInfo) and
     (LFromTypeInfo.ConvFamily = LToTypeInfo.ConvFamily) then
    Result := LFromTypeInfo.ConvFamily
  else
    Result := CIllegalConvFamily;
end;

//------------------------------------------------------------------------------

procedure RaiseConversionError(const AText: string);
begin
  raise EabfConversionError.Create(AText);
end;

//------------------------------------------------------------------------------

procedure RaiseConversionError(const AText: string;
  const AArgs: array of const);
begin
  raise EabfConversionError.CreateFmt(AText, AArgs);
end;

//------------------------------------------------------------------------------

procedure RaiseConversionRegError(AFamily: TabfConvFamily;
  const ADescription: string);
begin
  RaiseConversionError(SConvDuplicateType,
    [ADescription, ConvFamilyToDescription(AFamily)]);
end;


//==============================================================================
// TabfConvFamilyInfo
//==============================================================================

constructor TabfConvFamilyInfo.Create(const AConvFamily: TabfConvFamily; const ADescription: string);
begin
  inherited Create;
  FConvFamily := AConvFamily;
  FDescription := ADescription;
end;

//==============================================================================
// TabfConvTypeInfo
//==============================================================================

constructor TabfConvTypeInfo.Create(const AConvFamily: TabfConvFamily;
  const ADescription, AAbbreviation: string);
var
  LConvFamilyInfo: TabfConvFamilyInfo;
begin
  inherited Create;
  if not GetConvFamilyInfo(AConvFamily, LConvFamilyInfo) then
    RaiseConversionError(SConvUnknownFamily,
      [Format(SConvUnknownDescription, [AConvFamily])]);
  FConvFamily := AConvFamily;
  FDescription := ADescription;
  FAbbreviation := AAbbreviation;
end;


//==============================================================================
// TabfConvTypeFactor
//==============================================================================

constructor TabfConvTypeFactor.Create(const AConvFamily: TabfConvFamily;
  const ADescription, AAbbreviation: string; const AFactor: Double);
begin
  inherited Create(AConvFamily, ADescription, AAbbreviation);
  if AFactor = 0 then
    raise EZeroDivide.CreateFmt(SConvFactorZero, [ADescription]);
  FFactor := AFactor;
end;

//------------------------------------------------------------------------------

function TabfConvTypeFactor.FromCommon(const AValue: Double): Double;
begin
  Result := AValue / FFactor;
end;

//------------------------------------------------------------------------------

function TabfConvTypeFactor.ToCommon(const AValue: Double): Double;
begin
  Result := AValue * FFactor;
end;


//==============================================================================
// TabfConvTypeProcs
//==============================================================================

constructor TabfConvTypeProcs.Create(const AConvFamily: TabfConvFamily;
  const ADescription, AAbbreviation: string; const AToCommonProc,
  AFromCommonProc: TabfConversionProc);
begin
  inherited Create(AConvFamily, ADescription, AAbbreviation);
  FToCommonProc := AToCommonProc;
  FFromCommonProc := AFromCommonProc;
end;

//------------------------------------------------------------------------------

function TabfConvTypeProcs.FromCommon(const AValue: Double): Double;
begin
  Result := FFromCommonProc(AValue);
end;

//------------------------------------------------------------------------------

function TabfConvTypeProcs.ToCommon(const AValue: Double): Double;
begin
  Result := FToCommonProc(AValue);
end;


{******************************************************************************}
initialization
{******************************************************************************}

  GConvFamilySync := TMultiReadExclusiveWriteSynchronizer.Create;
  GConvTypeSync := TMultiReadExclusiveWriteSynchronizer.Create;

{******************************************************************************}
finalization
{******************************************************************************}

  CleanUpLists;
  FreeAndNil(GConvFamilySync);
  FreeAndNil(GConvTypeSync);

end{unit abfConvUtils}.
