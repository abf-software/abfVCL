{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfTypInfo;

{$I abf.inc}

interface

uses
{$IfDef D6}
  Variants, RTLConsts,
{$EndIf D6}  
  TypInfo, Classes, SysUtils;

{$IfNDef D5}
  {$IfNDef D3}
const
  {$Else D3}
resourcestring
  {$EndIf D3}
  SInvalidPropertyValue = 'Invalid property value';
  SInvalidPropertyPath = 'Invalid property path';
  SInvalidPropertyType = 'Invalid property type: %s';
  SInvalidPropertyElement = 'Invalid property element: %s';

type
  EPropertyConvertError = class(Exception);
{$EndIf D5}


//==============================================================================
// TypeInfo routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns the property information record. Supports complex property names.
// Example:
//   PropName = 'Font'
//   PropName = 'Font.Style'
//   PropName = 'Font.Style.fsUnderline'
function abfGetPropInfo(TypeInfo: PTypeInfo; const PropName: string): PPropInfo;

//------------------------------------------------------------------------------
// Returns a list of value names for a given type.
procedure abfGetTypeInfoValueNames(TypeInfo: PTypeInfo; ValueNames: TStrings);

//------------------------------------------------------------------------------
// Returns a list of value names for a given enumerate type.
procedure abfGetEnumTypeValueNames(TypeInfo: PTypeInfo; ValueNames: TStrings);

//------------------------------------------------------------------------------
// Returns a list of value names for a given set type.
procedure abfGetSetTypeValueNames(TypeInfo: PTypeInfo; ValueNames: TStrings);


//==============================================================================
// PropInfo routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns a TypeInfo for the specified PropInfo.
function abfGetTypeInfo(PropInfo: PPropInfo): PTypeInfo;

//------------------------------------------------------------------------------
// Returns a TypeData for the specified PropInfo.
function abfGetTypeData(PropInfo: PPropInfo): PTypeData;

//------------------------------------------------------------------------------
// Calculates the size of the given PropInfo property of the Instance object.
function abfGetPropInfoValueSize(Instance: TObject;
  PropInfo: PPropInfo): Integer;

//------------------------------------------------------------------------------
// Returns the value of the PropInfo property of the Instance object.
function abfGetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant;

//------------------------------------------------------------------------------
// Sets the value of the PropInfo property of the Instance object.
procedure abfSetPropValue(Instance: TObject; PropInfo: PPropInfo;
  const Value: Variant);

//------------------------------------------------------------------------------
// Returns a value of the enumerated PropInfo property of the Instance object.
function GetEnumProp(Instance: TObject; PropInfo: PPropInfo): string;

//------------------------------------------------------------------------------
// Sets a value of the enumerated PropInfo property of the Instance object.
procedure abfSetEnumProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);

//==============================================================================
// Properties of set type routines.

//------------------------------------------------------------------------------
// Sets a value of the set type PropInfo property of the Instance object.
procedure abfSetSetProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);

//------------------------------------------------------------------------------
// Returns lists of included and excluded values of the set type PropInfo
// property of the Instance object. Results are stored in Included and Excluded
// lists. You can specify Included or Excluded parameters as nil if you don't
// need an information about one of them.
// Example:
//   SomeObject.SomeProperty: (smFirst, smThird, smFourth, smFifth, smSixth);
//
//   SomeObject.SomeProperty = [smFirst, smThird, smSixth];
//
//   Included[0] = 'smFirst'
//   Included[1] = 'smThird'
//   Included[2] = 'smLast'
//
//   Excluded[0] = 'smFourth'
//   Excluded[1] = 'smFifth'
procedure abfGetSetPropValueLists(Instance: TObject; PropInfo: PPropInfo;
  const Included, Excluded: TStrings);

//------------------------------------------------------------------------------
// Sets value of the set type PropInfo property of the Instance object.
// All elements are present in the Values list will be included,
// not present - excluded.
// Example:
//   Values[0] = 'smFirst'
//   Values[1] = 'smThird'
//   Values[2] = 'smSixth'
//   SomeObject.SomeProperty = [smFirst, smThird, smSixth];
procedure abfSetSetPropValues(Instance: TObject; PropInfo: PPropInfo;
  const Values: TStrings);

//------------------------------------------------------------------------------
// Returns a list of included elements of the set type PropInfo property of the
// Instance object.
// Example:
//   SomeObject.SomeProperty = [smFirst, smThird, smSixth];
//   AValues[0] = 'smFirst'
//   AValues[1] = 'smThird'
//   AValues[2] = 'smSixth'
procedure abfGetSetPropValues(Instance: TObject; PropInfo: PPropInfo;
  Values: TStrings);

//------------------------------------------------------------------------------
// Returms a state of the ElementName element in the PropInfo property of
// the Instance object. Returns True if an element is included in the set,
// otherwise returns False.
function abfGetSetPropElement(Instance: TObject; PropInfo: PPropInfo;
  const ElementName: string): Boolean;

//------------------------------------------------------------------------------
// Include or excludes the ElementName element in the PropInfo property of the
// Instance object. To include an element set Value True, to exclude - set
// Value False.
procedure abfSetSetPropElement(Instance: TObject; PropInfo: PPropInfo;
  const ElementName: string; Value: Boolean);


//==============================================================================
// Copy properties routines
//==============================================================================

//------------------------------------------------------------------------------
// Copies the value from one property to another. Src and SrcInfo specify the
// source property. Dst and DstInfo specify the destination property.
procedure abfCopyProp(Src, Dst: TObject; SrcInfo, DstInfo: PPropInfo);

//------------------------------------------------------------------------------
// Copies published PropName property from the Src object to the Dst object.
// Note: use this function carefully, it works only with published properties.
procedure abfCopyProperty(Src, Dst: TObject; const PropName: string);

//------------------------------------------------------------------------------
// Copies specified published properties from the Src object to the Dst object.
// Note: use this function carefully, it works only with published properties.
procedure abfCopyProperties(Src, Dst: TObject; PropNames: TStrings);
  {$IfDef D4}overload;{$EndIf D4}

{$IfDef D4}
//------------------------------------------------------------------------------
// Copies specified published properties from the Src object to the Dst object.
// Note: use this function carefully, it works only with published properties.
procedure abfCopyProperties(Src, Dst: TObject;
  const PropNames: array of string); overload;
{$EndIf D4}

//------------------------------------------------------------------------------
// Copies all published properties from the Src object to the Dst object.
// ExcludeList determines list of property names that should not be copied.
// ExcludeList can be nil.
// Note: use this function carefully, it copies only published properties.
procedure abfCopyAllPropertiesEx(Src, Dst: TObject; ExcludeList: TStrings);

//------------------------------------------------------------------------------
// Copies all published properties from the Src object to the Dst object.
// Note: use this function carefully, it copies only published properties.
procedure abfCopyAllProperties(Src, Dst: TObject);

//------------------------------------------------------------------------------
// Assigns properties of the Instance object using the string list data.
// The format of string list items should be: "PropertyName=PropertyValue".
// Property is skipped if the name or value is undefined or empty.
procedure abfAssignPropertiesFromStrings(Instance: TObject; S: TStrings);


//==============================================================================
// Misc
//==============================================================================

//------------------------------------------------------------------------------
// Returns a list of property names are contained in the Instance object.
// Relurned names are stored in the Properties list. The PropInfo inforamation
// of each property is stored in corresponding Properties.Objects[x] (Use
// PPropInfo(Properties.Objects[x]) to access it). You can select what kind of
// properties should be returned using the Kinds parameter. Set Kinds
// empty [] to receive property names of any kind. Use IncludeStored and
// IncludeNotStored parameters to apply additional restrictions.
procedure abfGetPropertyListEx(Instance: TObject; Properties: TStrings;
  Kinds: TTypeKinds; IncludeStored, IncludeNotStored: Boolean);

//------------------------------------------------------------------------------
// Returns a list of property names are contained in the Instance object.
// Relurned names are stored in the Properties list. The PropInfo inforamation
// of each property is stored in corresponding Properties.Objects[x] (Use
// PPropInfo(Properties.Objects[x]) to access it).
procedure abfGetPropertyList(Instance: TObject; Properties: TStrings);

//------------------------------------------------------------------------------
// Converts value of the set type to the string.
function abfSetToString(PropInfo: PPropInfo; Value: Integer;
  Brackets: Boolean): string;

//------------------------------------------------------------------------------
// Converts string value to the set type.
function abfStringToSet(PropInfo: PPropInfo; const Value: string): Integer;

//------------------------------------------------------------------------------
// Returns the size (in bytes) of the given OrdType.
function abfGetOrdSize(OrdType: TOrdType): Integer;

//------------------------------------------------------------------------------
// Returns the size (in bytes) of the given FloatType.
function abfGetFloatSize(FloatType: TFloatType): Integer;

//------------------------------------------------------------------------------
// Fixes complex PropName by removing spaces; leading, trailing and doubled
// dots.
function abfFixComplexPropertyName(const PropName: string): string;

//------------------------------------------------------------------------------
// Parses complex PropName.
// Example:
//   PropName='Form.Edit1.Font'
//   CurrPropName='Form'
//   NextPropName='Edit1.Font'
procedure abfParseComplexPropertyName(const PropName: string; var CurrPropName,
  NextPropName: string);

//------------------------------------------------------------------------------
// Retutns a list of all possible values for Boolean property.
procedure abfGetBooleanPropValueNames(ValueNames: TStrings);

//------------------------------------------------------------------------------
// Determines does the PropName property exist in the Instance object.
function abfPropertyExists(Instance: TObject; const PropName: string): Boolean;

//------------------------------------------------------------------------------
// Determines is the PropName property of the Instance object ReadOnly.
function abfIsPropertyReadOnly(Instance: TObject;
  const PropName: string): Boolean;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  Consts, abfClasses, abfSysUtils, abfStrUtils;

//==============================================================================
// TypeInfo routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns the property information record. Supports complex property names.
// Example:
//   PropName = 'Font'
//   PropName = 'Font.Style'
//   PropName = 'Font.Style.fsUnderline'

function abfGetPropInfo(TypeInfo: PTypeInfo; const PropName: string): PPropInfo;
var
  CurrentPropName, NextPropName: string;
begin
  abfParseComplexPropertyName(PropName, CurrentPropName, NextPropName);
  Result := GetPropInfo(TypeInfo, CurrentPropName);
  if Result = nil then Exit;
{ Process part of property name after '.' for set properties }
  if (NextPropName <> '') and (abfGetTypeInfo(Result)^.Kind <> tkSet) then
    Result := abfGetPropInfo(abfGetTypeInfo(Result), NextPropName);
end;

//------------------------------------------------------------------------------
// Returns a list of value names for a given type.

procedure abfGetTypeInfoValueNames(TypeInfo: PTypeInfo; ValueNames: TStrings);
begin
  if (TypeInfo <> nil) then
    case TypeInfo.Kind of
      tkEnumeration:
        abfGetEnumTypeValueNames(TypeInfo, ValueNames);
      tkSet:
        abfGetSetTypeValueNames(TypeInfo, ValueNames);
    end;
end;

//------------------------------------------------------------------------------
// Returns a list of value names for a given enumerate type.

procedure abfGetEnumTypeValueNames(TypeInfo: PTypeInfo; ValueNames: TStrings);
var
  i: Integer;
begin
  if (TypeInfo = nil) or (TypeInfo^.Kind <> tkEnumeration) then Exit;
{ Enumerate all posible values }
  ValueNames.Clear;
  for i := GetTypeData(TypeInfo)^.MinValue to GetTypeData(TypeInfo)^.MaxValue do
    ValueNames.Add(GetEnumName(TypeInfo, i));
end;

//------------------------------------------------------------------------------
// Returns a list of value names for a given set type.

procedure abfGetSetTypeValueNames(TypeInfo: PTypeInfo; ValueNames: TStrings);
begin
  if (TypeInfo = nil) or (TypeInfo.Kind <> tkSet) then Exit;
  abfGetEnumTypeValueNames({$IfNDef D3}@{$EndIf}GetTypeData(TypeInfo)^.CompType^,
    ValueNames);
end;


//==============================================================================
// PropInfo routines
//==============================================================================

//------------------------------------------------------------------------------
// Returns a TypeInfo for the specified PropInfo.

function abfGetTypeInfo(PropInfo: PPropInfo): PTypeInfo;
begin
  Result := nil;
  if PropInfo = nil then Exit;
{$IfDef D3}
    Result := PropInfo^.PropType^;
{$Else D3}
    Result := PropInfo^.PropType;
{$EndIf D3}
end;

//------------------------------------------------------------------------------
// Returns a TypeData for the specified PropInfo.

function abfGetTypeData(PropInfo: PPropInfo): PTypeData;
begin
  Result := GetTypeData(abfGetTypeInfo(PropInfo));
end;

//------------------------------------------------------------------------------
// Returns a size (in bytes) of the given PropInfo property of the Instance
// object.

function abfGetPropInfoValueSize(Instance: TObject;
  PropInfo: PPropInfo): Integer;
var
  TypeInfo : PTypeInfo;
begin
  Result := 0;
  if PropInfo = nil then Exit;

  TypeInfo := abfGetTypeInfo(PropInfo);
  with TypeInfo^, GetTypeData(TypeInfo)^ do
    case kind of
      tkInteger, tkChar, tkSet, tkClass, tkEnumeration:
        Result := abfGetOrdSize(OrdType);
      tkFloat:
        Result := abfGetFloatSize(FloatType);
      tkString, tkWChar, tkLString {$IFDEF D3} ,tkWString {$ENDIF}:
        Result := Length(GetStrProp(Instance, PropInfo));
      tkVariant:
        Result := SizeOf(Variant);
    end;
end;

//------------------------------------------------------------------------------
// Returns the value of the PropInfo property of the Instance object.

function abfGetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant;
begin
  Result := Null {UnAssigned};
  if PropInfo = nil then Exit;

  case abfGetTypeInfo(PropInfo)^.Kind of
    tkInteger, tkChar, tkSet, tkClass, tkWChar:
      Result := GetOrdProp(Instance, PropInfo);
    tkEnumeration:
      if {$IfNDef D3}@{$EndIf}abfGetTypeData(PropInfo)^.BaseType^ = TypeInfo(Boolean) then
        Result := Boolean(GetOrdProp(Instance, PropInfo))
      else
        Result := GetOrdProp(Instance, PropInfo);
    tkFloat:
      Result := GetFloatProp(Instance, PropInfo);
    tkString, tkLString{$IfNDef D6}{$IfDef D3}, tkWString{$EndIf}{$EndIf}:
      Result := GetStrProp(Instance, PropInfo);
    tkMethod:
       Result := PropInfo^.PropType^.Name;
{$IfDef D6}
    tkWString:
      Result := GetWideStrProp(Instance, PropInfo);
{$EndIf D6}
    tkVariant:
      Result := GetVariantProp(Instance, PropInfo);
{$IfDef D4}
    tkInt64:
      Result := GetInt64Prop(Instance, PropInfo) + 0.0;
{$EndIf D4}
  else
    raise EPropertyConvertError.CreateFmt(SInvalidPropertyType,
      [PropInfo^.PropType^{$IfDef D3}^{$EndIf}.Name]);
  end;
end;{function abfGetPropValueByPropInfo}

//------------------------------------------------------------------------------
// Sets the value of the PropInfo property of the Instance object.

procedure abfSetPropValue(Instance: TObject; PropInfo: PPropInfo;
  const Value: Variant);

  //-------------------------------------
{$IfDef D4}
  function _RangedValue(const AMin, AMax: Int64): Int64;
{$Else D4}
  function _RangedValue(const AMin, AMax: Integer): Integer;
{$EndIf D4}
  begin
    Result := Trunc(Value);
    if Result < AMin then Result := AMin;
    if Result > AMax then Result := AMax;
  end;

  //-------------------------------------

var
  TypeData: PTypeData;
begin
  if PropInfo = nil then Exit;

  TypeData := abfGetTypeData(PropInfo);
  case abfGetTypeInfo(PropInfo)^.Kind of
    tkInteger, tkChar, tkWChar:
      SetOrdProp(Instance, PropInfo, _RangedValue(TypeData^.MinValue,
        TypeData^.MaxValue));
    tkEnumeration:
      if VarType(Value) = varString then
        abfSetEnumProp(Instance, PropInfo, VarToStr(Value))
      else
        SetOrdProp(Instance, PropInfo, _RangedValue(TypeData^.MinValue,
          TypeData^.MaxValue));
    tkSet:
      if VarType(Value) = varInteger then
        SetOrdProp(Instance, PropInfo, Value)
      else
        abfSetSetProp(Instance, PropInfo, VarToStr(Value));
    tkFloat:
      SetFloatProp(Instance, PropInfo, Value);
    tkString, tkLString{$IfNDef D6}{$IfDef D3}, tkWString{$EndIf}{$EndIf}:
      SetStrProp(Instance, PropInfo, VarToStr(Value));
    tkMethod:
      PropInfo^.PropType^.Name := VarToStr(Value);
{$IfDef D6}
    tkWString:
      SetWideStrProp(Instance, PropInfo, VarToWideStr(Value));
{$EndIf D6}
    tkVariant:
      SetVariantProp(Instance, PropInfo, Value);
{$IfDef D4}
    tkInt64:
      SetInt64Prop(Instance, PropInfo, _RangedValue(TypeData^.MinInt64Value,
        TypeData^.MaxInt64Value));
{$EndIf D4}
  else
    raise EPropertyConvertError.CreateFmt(SInvalidPropertyType,
      [PropInfo^.PropType^{$IfDef D3}^{$EndIf}.Name]);
  end;
end;

//------------------------------------------------------------------------------
// Returns a value of the enumerated PropInfo property of the Instance object.

function GetEnumProp(Instance: TObject; PropInfo: PPropInfo): string;
begin
{$IfDef D5}
  Result := GetEnumProp(Instance, PropInfo);
{$Else D5}
  Result := GetEnumName({$IfNDef D3}@{$EndIf}PropInfo^.PropType^, GetOrdProp(Instance, PropInfo));
{$EndIf D5}
end;

//------------------------------------------------------------------------------
// Sets a value of the enumerated PropInfo property of the Instance object.

procedure abfSetEnumProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
{$IfNDef D5}
var
  Data: Longint;
{$EndIf D5}
begin
{$IfDef D5}
  SetEnumProp(Instance, PropInfo, Value);
{$Else D5}
  Data := GetEnumValue({$IfNDef D3}@{$EndIf}PropInfo^.PropType^, Value);
  if Data < 0 then
    raise EPropertyConvertError.CreateFmt(SInvalidPropertyElement, [Value]);
  SetOrdProp(Instance, PropInfo, Data);
{$EndIf D5}
end;


//==============================================================================
// Properties of set type routines.

//------------------------------------------------------------------------------
// Sets a value of the set type PropInfo property of the Instance object.

procedure abfSetSetProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
begin
  SetOrdProp(Instance, PropInfo, abfStringToSet(PropInfo, Value));
end;


//------------------------------------------------------------------------------
// Returns lists of included and excluded values of the set type PropInfo
// property of the Instance object. Results are stored in Included and Excluded
// lists. You can specify Included or Excluded parameters as nil if you don't
// need an information about one of them.
// Example:
//   SomeObject.SomeProperty: (smFirst, smThird, smFourth, smFifth, smSixth);
//
//   SomeObject.SomeProperty = [smFirst, smThird, smSixth];
//
//   Included[0] = 'smFirst'
//   Included[1] = 'smThird'
//   Included[2] = 'smLast'
//
//   Excluded[0] = 'smFourth'
//   Excluded[1] = 'smFifth'

procedure abfGetSetPropValueLists(Instance: TObject; PropInfo: PPropInfo;
  const Included, Excluded: TStrings);
var
  i: Integer;
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
begin
  if not (Assigned(Included) or Assigned(Excluded)) or
    (abfGetTypeInfo(PropInfo)^.Kind <> tkSet) then Exit;

  Integer(S) := GetOrdProp(Instance, PropInfo);
  TypeInfo := {$IfNDef D3}@{$EndIf}abfGetTypeData(PropInfo)^.CompType^;
{ Enumerate all posible values }
  if Assigned(Included) then Included.Clear;
  if Assigned(Excluded) then Excluded.Clear;
  for i := GetTypeData(TypeInfo)^.MinValue to GetTypeData(TypeInfo)^.MaxValue do
   if (i in S) then
   begin
     if Assigned(Included) then Included.Add(GetEnumName(TypeInfo, i));
   end else
   begin
     if Assigned(Excluded) then Excluded.Add(GetEnumName(TypeInfo, i));
   end;
end;

//------------------------------------------------------------------------------
// Sets value of the set type PropInfo property of the Instance object.
// All elements are present in the Values list will be included,
// not present - excluded.
// Example:
//   Values[0] = 'smFirst'
//   Values[1] = 'smThird'
//   Values[2] = 'smSixth'
//   SomeObject.SomeProperty = [smFirst, smThird, smSixth];

procedure abfSetSetPropValues(Instance: TObject; PropInfo: PPropInfo;
  const Values: TStrings);
var
  i: Integer;
  TypeInfo: PTypeInfo;
  Data, EnumValue: LongInt;
begin
  if (not Assigned(Values)) or (abfGetTypeInfo(PropInfo)^.Kind <> tkSet) then
    Exit;
  TypeInfo := {$IfNDef D3}@{$EndIf}abfGetTypeData(PropInfo)^.CompType^;
  Data := 0;
{ Include all values from list }
  for i := 0 to Values.Count - 1 do
  begin
    EnumValue := GetEnumValue(TypeInfo, Values[i]);
    if EnumValue >= 0 then Include(TIntegerSet(Data), EnumValue);
  end;
  SetOrdProp(Instance, PropInfo, Data);
end;

//------------------------------------------------------------------------------
// Returns a list of included elements of the set type PropInfo property of the
// Instance object.
// Example:
//   SomeObject.SomeProperty = [smFirst, smThird, smSixth];
//   AValues[0] = 'smFirst'
//   AValues[1] = 'smThird'
//   AValues[2] = 'smSixth'

procedure abfGetSetPropValues(Instance: TObject; PropInfo: PPropInfo;
  Values: TStrings);
begin
  abfGetSetPropValueLists(Instance, PropInfo, Values, nil);
end;

//------------------------------------------------------------------------------
// Returms a state of the ElementName element in the PropInfo property of
// the Instance object. Returns True if an element is included in the set,
// otherwise returns False.

function abfGetSetPropElement(Instance: TObject; PropInfo: PPropInfo;
  const ElementName: string): Boolean;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    abfGetSetPropValueLists(Instance, PropInfo, Values, nil);
    Result := (Values.IndexOf(ElementName) >= 0);
  finally
    Values.Free;
  end;
end;

//------------------------------------------------------------------------------
// Include or excludes the ElementName element in the PropInfo property of the
// Instance object. To include an element set Value True, to exclude - set
// Value False.

procedure abfSetSetPropElement(Instance: TObject; PropInfo: PPropInfo;
  const ElementName: string; Value: Boolean);
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    abfGetSetPropValueLists(Instance, PropInfo, Values, nil);
    if Value then
      Values.Add(ElementName)
    else
      abfRemoveStringFromList(ElementName, Values);
    abfSetSetPropValues(Instance, PropInfo, Values);
  finally
    Values.Free;
  end;
end;


//==============================================================================
// Copy properties routines
//==============================================================================

//------------------------------------------------------------------------------
// Copies the value from one property to another. Src and SrcInfo specify the
// source property. Dst and DstInfo specify the destination property.

procedure abfCopyProp(Src, Dst: TObject; SrcInfo, DstInfo: PPropInfo);
begin
  if (Src = Dst) or (Src = nil) or (Dst = nil) then Exit;

  case abfGetTypeInfo(SrcInfo)^.Kind of
    tkInteger, tkChar, tkSet, tkClass, tkEnumeration, tkWChar:
      SetOrdProp(Dst, DstInfo, GetOrdProp(Src, SrcInfo));
    tkFloat:
      SetFloatProp(Dst, DstInfo, GetFloatProp(Src, SrcInfo));
    tkString, tkLString{$IfNDef D6}{$IfDef D3}, tkWString{$EndIf}{$EndIf}:
      SetStrProp(Dst, DstInfo, GetStrProp(Src, SrcInfo));
    tkMethod:
      DstInfo^.PropType^.Name := SrcInfo^.PropType^.Name;
{$IfDef D6}
    tkWString:
      SetWideStrProp(Dst, DstInfo, GetWideStrProp(Src, SrcInfo));
{$EndIf D6}
    tkVariant:
      SetVariantProp(Dst, DstInfo, GetVariantProp(Src, SrcInfo));
{$IfDef D4}
    tkInt64:
      SetInt64Prop(Dst, DstInfo, GetInt64Prop(Src, SrcInfo));
{$EndIf D4}
  else
    raise EPropertyConvertError.CreateFmt(SInvalidPropertyType,
      [DstInfo.PropType^{$IfDef D3}^{$EndIf}.Name]);
  end;
end;

//------------------------------------------------------------------------------
// Copies published PropName property from the Src object to the Dst object.
// Note: use this function carefully, it works only with published properties.

procedure abfCopyProperty(Src, Dst: TObject; const PropName: string);
begin
  if (Src = Dst) or (Src = nil) or (Dst = nil) then Exit;
  abfCopyProp(Src, Dst, abfGetPropInfo(Src.ClassInfo, PropName),
    abfGetPropInfo(Dst.ClassInfo, PropName));
end;

//------------------------------------------------------------------------------
// Copies specified published properties from the Src object to the Dst object.
// Note: use this function carefully, it works only with published properties.

procedure abfCopyProperties(Src, Dst: TObject; PropNames: TStrings);
  {$IfDef D4}overload;{$EndIf D4}
var
  i: Integer;
begin
  if (Src = Dst) or (Src = nil) or (Dst = nil) then Exit;
  for i := 0 to PropNames.Count - 1 do
    abfCopyProperty(Src, Dst, PropNames[i]);
end;

{$IfDef D4}
//------------------------------------------------------------------------------
// Copies specified published properties from the Src object to the Dst object.
// Note: use this function carefully, it works only with published properties.

procedure abfCopyProperties(Src, Dst: TObject;
  const PropNames: array of string); overload;
var
  i: Integer;
begin
  if (Src = Dst) or (Src = nil) or (Dst = nil) then Exit;
  for i := Low(PropNames) to High(PropNames) do
    abfCopyProperty(Src, Dst, PropNames[i]);
end;

{$EndIf D4}

//------------------------------------------------------------------------------
// Copies all published properties from the Src object to the Dst object.
// ExcludeList determines list of property names that should not be copied.
// ExcludeList can be nil.
// Note: use this function carefully, it copies only published properties.

procedure abfCopyAllPropertiesEx(Src, Dst: TObject; ExcludeList: TStrings);
var
  i, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
begin
  if (Src = Dst) or (Src = nil) or (Dst = nil) then Exit;

  Count := GetTypeData(Src.ClassInfo)^.PropCount;
  if Count < 0 then Exit;

  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropInfos(Src.ClassInfo, PropList);
    for i := 0 to Count - 1 do
    begin
      PropInfo := PropList^[i];
      if Assigned(ExcludeList)
        and (ExcludeList.IndexOf(PropInfo^.Name) >= 0) then Continue;
      abfCopyProperty(Src, Dst, PropInfo^.Name);
    end;
  finally
    FreeMem(PropList, Count * SizeOf(Pointer));
  end;
end;

//------------------------------------------------------------------------------
// Copies all published properties from the Src object to the Dst object.
// Note: use this function carefully, it copies only published properties.

procedure abfCopyAllProperties(Src, Dst: TObject);
begin
  abfCopyAllPropertiesEx(Src, Dst, nil);
end;

//------------------------------------------------------------------------------
// Assigns properties of the Instance object using the string list data.
// The format of string list items should be: "PropertyName=PropertyValue".
// Property is skipped if the name or value is undefined or empty.

procedure abfAssignPropertiesFromStrings(Instance: TObject; S: TStrings);
var
  PropNames: TStringList;
  i: Integer;
  V: string;
begin
  PropNames := TStringList.Create;
  try
    abfGetPropertyList(Instance, PropNames);
    with PropNames do
      for i := 0 to Count - 1 do
      begin
        V := S.Values[Trim(Strings[i])];
        if V <> '' then
          abfSetPropValue(Instance, GetPropInfo(Instance.ClassInfo,
            Strings[i]), V);
      end;
  finally
    PropNames.Free;
  end;
end;


//==============================================================================
// Misc
//==============================================================================

//------------------------------------------------------------------------------
// Returns a list of property names are contained in the Instance object.
// Relurned names are stored in the Properties list. The PropInfo inforamation
// of each property is stored in corresponding Properties.Objects[x] (Use
// PPropInfo(Properties.Objects[x]) to access it). You can select what kind of
// properties should be returned using the Kinds parameter. Set Kinds
// empty [] to receive property names of any kind. Use IncludeStored and
// IncludeNotStored parameters to apply additional restrictions.

procedure abfGetPropertyListEx(Instance: TObject; Properties: TStrings;
  Kinds: TTypeKinds; IncludeStored, IncludeNotStored: Boolean);
var
  i, Count: Integer;
  PropList: PPropList;
  Stored: Boolean;
begin
  if not Assigned(Properties) then Exit;
  Properties.Clear;
  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  if Count <= 0 then Exit;
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    for i := 0 to Count - 1 do
      if (Kinds = []) or (PropList^[I].PropType^{$IfDef D3}^{$EndIf}.Kind in Kinds) then
      begin
        Stored := IsStoredProp(Instance, PropList^[i]);
        if (Stored and IncludeStored) or (not Stored and IncludeNotStored) then
          Properties.AddObject(PropList^[i].Name, TObject(PropList^[i]));
      end
  finally
    FreeMem(PropList, Count * SizeOf(Pointer));
  end;
end;

//------------------------------------------------------------------------------
// Returns a list of property names are contained in the Instance object.
// Relurned names are stored in the Properties list. The PropInfo inforamation
// of each property is stored in corresponding Properties.Objects[x] (Use
// PPropInfo(Properties.Objects[x]) to access it).

procedure abfGetPropertyList(Instance: TObject; Properties: TStrings);
begin
  abfGetPropertyListEx(Instance, Properties, [], True, True);
end;


//------------------------------------------------------------------------------
// Converts value of the set type to the string.

function abfSetToString(PropInfo: PPropInfo; Value: Integer;
  Brackets: Boolean): string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  i: Integer;
begin
  Result := '';
  Integer(S) := Value;
  TypeInfo := {$IfNDef D3}@{$EndIf}abfGetTypeData(PropInfo)^.CompType^;
  for i := 0 to SizeOf(Integer) * 8 - 1 do
    if i in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, i);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

//------------------------------------------------------------------------------
// Converts string value to the set type.

function abfStringToSet(PropInfo: PPropInfo; const Value: string): Integer;
var
  S: string;
  TypeInfo: PTypeInfo;
  ElementName: string;
  EnumValue: LongInt;

  //-------------------------------------

  function _NextWord: string;
  var
    i: Integer;
  begin
    Result := '';
    if Length(S) < 1 then Exit;
    i := 1;
  { Skip wrong chars }
    while S[i] in [',', ' ', '[', ']'] do Inc(i);
    Delete(S, 1, i - 1);
  { Scan til wrong chars }
    if Length(S) < 1 then Exit;
    i := 1;
    while not (S[i] in [',', ' ', #0, ']']) do Inc(i);
    Result := Copy(S, 1, i - 1);
    Delete(S, 1, i - 1);
  end;

  //-------------------------------------

begin
  Result := 0;
  if Value = '' then Exit;
  S := Value;

  TypeInfo := {$IfNDef D3}@{$EndIf}abfGetTypeData(PropInfo)^.CompType^;
  ElementName := _NextWord;
  while ElementName <> '' do
  begin
    EnumValue := GetEnumValue(TypeInfo, ElementName);
    if EnumValue < 0 then
      raise EPropertyConvertError.CreateFmt(SInvalidPropertyElement,
        [ElementName]);
    Include(TIntegerSet(Result), EnumValue);
    ElementName := _NextWord;
  end;
end;

//------------------------------------------------------------------------------
// Returns the size (in bytes) of the given OrdType.

function abfGetOrdSize(OrdType: TOrdType): Integer;
begin
  case OrdType of
    otSByte: Result := SizeOf(Byte);
    otUByte: Result := SizeOf(Byte);
    otSWord: Result := SizeOf(Word);
    otUWord: Result := SizeOf(Word);
  else
    Result := SizeOf(Integer);
  end;
end;

//------------------------------------------------------------------------------
// Returns the size (in bytes) of the given FloatType.

function abfGetFloatSize(FloatType: TFloatType): Integer;
begin
  case FloatType of
    ftSingle  : Result := SizeOf(Single);
    ftDouble  : Result := SizeOf(Double);
    ftExtended: Result := SizeOf(Extended);
    ftComp    : Result := SizeOf(Comp);
    ftCurr    : Result := SizeOf(Currency);
  else
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------
// Fixes complex PropName by removing spaces; leading, trailing and doubled
// dots.

function abfFixComplexPropertyName(const PropName: string): string;
begin
  Result := abfRemoveCharSet([' '], abfTrim(PropName, ['.', ' ']));
  abfReplaceSubStrings(Result, '..', '.');
end;

//------------------------------------------------------------------------------
// Parses complex PropName.
// Example:
//   PropName='Form.Edit1.Font'
//   CurrPropName='Form'
//   NextPropName='Edit1.Font'

procedure abfParseComplexPropertyName(const PropName: string; var CurrPropName,
  NextPropName: string);
{$IFDEF D9}
var
  a: widestring;
{$ELSE}
  a: string;
{$ENDIF}
begin
  CurrPropName := abfFixComplexPropertyName(PropName);
  NextPropName := CurrPropName;
{$IFDEF D9}
  a := CurrPropName;
  abfDeleteAfterCharW(a, '.');
  CurrPropName := a;
{$ELSE}
  abfDeleteAfterCharA(CurrPropName, '.');
{$ENDIF}
  Delete(NextPropName, 1, Length(CurrPropName));
  NextPropName := abfFixComplexPropertyName(NextPropName);
end;

//------------------------------------------------------------------------------
// Retutns a list of all possible values for Boolean property.

procedure abfGetBooleanPropValueNames(ValueNames: TStrings);
begin
  if not Assigned(ValueNames) then Exit;
  ValueNames.Clear;
  ValueNames.Add(GetEnumName(System.TypeInfo(Boolean), Integer(False)));
  ValueNames.Add(GetEnumName(System.TypeInfo(Boolean), Integer(True )));
end;

//------------------------------------------------------------------------------
// Determines is the PropName property exists in the Instance object.

function abfPropertyExists(Instance: TObject; const PropName: string): Boolean;
begin
  Result := Assigned(GetPropInfo(Instance.ClassInfo, PropName));
end;

//------------------------------------------------------------------------------
// Determines is the PropName property of the Instance object ReadOnly.

function abfIsPropertyReadOnly(Instance: TObject;
  const PropName: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  Result := False;
  PropInfo := abfGetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo = nil) or (PropInfo^.SetProc = nil) then
    Result := True;
end;

//------------------------------------------------------------------------------

end{unit abfTypInfo}.
