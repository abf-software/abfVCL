{*******************************************************************************

  ABF Visual Components Library. Standard conversion unit.

  Based on Borland and Project JEDI routines
  http://www.borland.com
  http://www.delphi-jedi.org

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

********************************************************************************

  The metric units and prefixes in this unit follow the various
  SI/NIST standards (http://physics.nist.gov/cuu/Units/index.html)

  References:
  [1] NIST: Mendenhall Order of 1893
  [2] ds.dial.pipex.com/nib/metric.htm
  [3] www.omnis.demon.co.uk/conversn/oldenguk.htm
  [4] NIST (physics.nist.gov/cuu/Units/outside.html)
  [5] NIST (physics.nist.gov/cuu/Units/meter.html)
  [6] Accepted best guess, but nobody really knows
  [7] www.ex.ac.uk/cimt/dictunit/dictunit.htm   !! GREAT SITE !!

  Other great sites
  www.unc.edu/~rowlett/units/index.html !! GREAT SITE !!
  www.omnis.demon.co.uk/indexfrm.htm !! GREAT SITE !!
  www.sciencemadesimple.com/conversions.html
  www.numberexchange.net/Convert/Weight.html
  students.washington.edu/kyle/temp.html
  www.convertit.com

*******************************************************************************}
unit abfStdConvs;

{$I abf.inc}

interface

uses
  SysUtils, abfConvUtils, abfStdConvsConsts;

//==============================================================================
// Constants (and their derivatives) used in this unit
//==============================================================================

const
  MetersPerInch = 0.0254; // [1]
  MetersPerMil = MetersPerInch / 1000;
  MetersPerLine = MetersPerInch / 12;
  MetersPerSpan = MetersPerInch * 9;
  MetersPerFoot = MetersPerInch * 12;
  MetersPerYard = MetersPerFoot * 3;
  MetersPerMile = MetersPerFoot * 5280;
  MetersPerNauticalMiles = 1852;
  MetersPerAstronomicalUnit = 1.49598E11; // [4]
  MetersPerLightSecond = 2.99792458E8; // [5]
  MetersPerLightYear = MetersPerLightSecond * 31556925.9747; // [7]
  MetersPerParsec = MetersPerAstronomicalUnit * 206264.806247096; // 60 * 60 * (180 / Pi)
  MetersPerCubit = 0.4572; // [6][7]
  MetersPerFathom = MetersPerFoot * 6;
  MetersPerFurlong = MetersPerYard * 220;
  MetersPerHand = MetersPerInch * 4;
  MetersPerPace = MetersPerInch * 30;
  MetersPerRod = MetersPerFoot * 16.5;
  MetersPerChain = MetersPerRod * 4;
  MetersPerLink = MetersPerChain / 100;
  MetersPerPoint = MetersPerInch * 0.01384;
  MetersPerPica = MetersPerPoint * 12;

  KilometersPerMile = (MetersPerMile / 1000); // 1.609344
  MilesPerKilometer = 1 / KilometersPerMile;  // 0.62137119223733396961743418436332

  SquareMetersPerSquareInch = MetersPerInch * MetersPerInch;
  SquareMetersPerSquareFoot = MetersPerFoot * MetersPerFoot;
  SquareMetersPerSquareYard = MetersPerYard * MetersPerYard;
  SquareMetersPerSquareMile = MetersPerMile * MetersPerMile;
  SquareMetersPerAcre = SquareMetersPerSquareYard * 4840;
  SquareMetersPerSquareRod = MetersPerRod * MetersPerRod;

  CubicMetersPerCubicInch = MetersPerInch * MetersPerInch * MetersPerInch;
  CubicMetersPerCubicFoot = MetersPerFoot * MetersPerFoot * MetersPerFoot;
  CubicMetersPerCubicYard = MetersPerYard * MetersPerYard * MetersPerYard;
  CubicMetersPerCubicMile = MetersPerMile * MetersPerMile * MetersPerMile;
  CubicMetersPerAcreFoot = SquareMetersPerAcre * MetersPerFoot;
  CubicMetersPerAcreInch = SquareMetersPerAcre * MetersPerInch;
  CubicMetersPerCord = CubicMetersPerCubicFoot * 128;
  CubicMetersPerCordFoot = CubicMetersPerCubicFoot * 16;

  CubicMetersPerUSGallon = CubicMetersPerCubicInch * 231; // [2][3][7]
  CubicMetersPerUSQuart = CubicMetersPerUSGallon / 4;
  CubicMetersPerUSPint = CubicMetersPerUSQuart / 2;
  CubicMetersPerUSCup = CubicMetersPerUSPint / 2;
  CubicMetersPerUSGill = CubicMetersPerUSCup / 2;
  CubicMetersPerUSOunce = CubicMetersPerUSCup / 8;
  CubicMetersPerUSTablespoon = CubicMetersPerUSOunce / 2;
  CubicMetersPerUSTeaspoon = CubicMetersPerUSOunce / 6;

  CubicMetersPerUSDryGallon = CubicMetersPerCubicInch * 268.8025; // [7]
  CubicMetersPerUSDryQuart = CubicMetersPerUSDryGallon / 4;
  CubicMetersPerUSDryPint = CubicMetersPerUSDryQuart / 2;
  CubicMetersPerUSPeck = CubicMetersPerUSDryGallon * 2;
  CubicMetersPerUSBucket = CubicMetersPerUSPeck * 2;
  CubicMetersPerUSBushel = CubicMetersPerUSBucket * 2;

  CubicMetersPerUKGallon = 0.00454609; // [2][7]
  CubicMetersPerUKPottle = CubicMetersPerUKGallon / 2;
  CubicMetersPerUKQuart = CubicMetersPerUKPottle / 2;
  CubicMetersPerUKPint = CubicMetersPerUKQuart / 2;
  CubicMetersPerUKGill = CubicMetersPerUKPint / 4;
  CubicMetersPerUKOunce = CubicMetersPerUKPint / 20;
  CubicMetersPerUKPeck = CubicMetersPerUKGallon * 2;
  CubicMetersPerUKBucket = CubicMetersPerUKPeck * 2;
  CubicMetersPerUKBushel = CubicMetersPerUKBucket * 2;

  LitersPerCubicMeter = 1000;
  LitersPerUSGallon  = CubicMetersPerUSGallon * LitersPerCubicMeter;
  USGallonsPerLiter = 1 / LitersPerUSGallon;
  LitersPerUKGallon  = CubicMetersPerUKGallon * LitersPerCubicMeter;
  UKGallonsPerLiter = 1 / LitersPerUKGallon;

  GramsPerPound = 453.59237; // [1][7]
  GramsPerDrams = GramsPerPound / 256;
  GramsPerGrains = GramsPerPound / 7000;
  GramsPerTons = GramsPerPound * 2000;
  GramsPerLongTons = GramsPerPound * 2240;
  GramsPerOunces = GramsPerPound / 16;

{ TODO -oKARPOLAN : Check and specify }
  GramsPerTroyPound = 373.236;
  GramsPerTroyOunces = GramsPerTroyPound / 12;
  GramsPerTroyDrams = GramsPerTroyOunces / 8;
  GramsPerPennyweights = 24 * GramsPerGrains {1.555};

  BitsPerByte = 8;
  BitsPerWord = BitsPerByte * 2;
var

//==============================================================================
// Area conversion units
//==============================================================================
// Basic unit of measurement is square meters

  cbArea: TabfConvFamily;

  auSquareMillimeters: TabfConvType;
  auSquareCentimeters: TabfConvType;
  auSquareDecimeters: TabfConvType;
  auSquareMeters: TabfConvType;
  auSquareDekameters: TabfConvType;
  auSquareHectometers: TabfConvType;
  auSquareKilometers: TabfConvType;
  auSquareInches: TabfConvType;
  auSquareFeet: TabfConvType;
  auSquareYards: TabfConvType;
  auSquareMiles: TabfConvType;
  auAcres: TabfConvType;
  auCentares: TabfConvType;
  auAres: TabfConvType;
  auHectares: TabfConvType;
  auBarns: TabfConvType;
  auSquareRods: TabfConvType;


//==============================================================================
// Distance conversion units
//==============================================================================
// Basic unit of measurement is meters

  cbDistance: TabfConvFamily;

  duMicromicrons: TabfConvType;
  duAngstroms: TabfConvType;
  duMillimicrons: TabfConvType;
  duMicrons: TabfConvType;
  duMillimeters: TabfConvType;
  duCentimeters: TabfConvType;
  duDecimeters: TabfConvType;
  duMeters: TabfConvType;
  duDekameters: TabfConvType;
  duHectometers: TabfConvType;
  duKilometers: TabfConvType;
  duMegameters: TabfConvType;
  duGigameters: TabfConvType;
  duMils: TabfConvType;
  duLines: TabfConvType;
  duInches: TabfConvType;
  duSpans: TabfConvType;
  duFeet: TabfConvType;
  duYards: TabfConvType;
  duFathoms: TabfConvType;
  duFurlongs: TabfConvType;
  duChains: TabfConvType;
  duRods: TabfConvType;
  duMiles: TabfConvType;
  duLeagues: TabfConvType;
  duCables: TabfConvType;
  duUSCables: TabfConvType;
  duNauticalMiles: TabfConvType;
  duNauticalLeagues: TabfConvType;
  duAstronomicalUnits: TabfConvType;
  duLightYears: TabfConvType;
  duLightMinutes: TabfConvType;
  duLightSeconds: TabfConvType;
  duParsecs: TabfConvType;
  duCubits: TabfConvType;
  duHands: TabfConvType;
  duPaces: TabfConvType;
  duLinks: TabfConvType;
  duPoints: TabfConvType;
  duPicas: TabfConvType;
  duXUnits: TabfConvType;


//==============================================================================
// Fuel Consumption conversion units
//==============================================================================
// Basic unit of measurement is "litres per 100 km"

  cbFuelConsumption: TabfConvFamily;

  fuLitersPer100KiloMeters: TabfConvType;
  fuLitersPer100Miles: TabfConvType;
  fuUSGallonsPer100KiloMeters: TabfConvType;
  fuUSGallonsPer100Miles: TabfConvType;
  fuUKGallonsPer100KiloMeters: TabfConvType;
  fuUKGallonsPer100Miles: TabfConvType;

  fuKiloMetersPerLiter: TabfConvType;
  fuMilesPerLiter: TabfConvType;
  fuKiloMetersPerUSGallon: TabfConvType;
  fuMilesPerUSGallon: TabfConvType;
  fuKiloMetersPerUKGallon: TabfConvType;
  fuMilesPerUKGallon: TabfConvType;


//==============================================================================
// Mass conversion units
//==============================================================================
// Basic unit of measurement is grams

  cbMass: TabfConvFamily;

  muNanograms: TabfConvType;
  muMicrograms: TabfConvType;
  muMilligrams: TabfConvType;
  muCentigrams: TabfConvType;
  muDecigrams: TabfConvType;
  muGrams: TabfConvType;
  muDekagrams: TabfConvType;
  muHectograms: TabfConvType;
  muKilograms: TabfConvType;
  muMetricTons: TabfConvType;
  muMetricCentners: TabfConvType;
  muMetricCarats: TabfConvType;
  muLongTons: TabfConvType;
  muTons: TabfConvType;
  muGrains: TabfConvType;
  muPounds: TabfConvType;
  muOunces: TabfConvType;
  muStones: TabfConvType;

  muUSCentners: TabfConvType;
  muUKCentners: TabfConvType;

  muTroyOunces: TabfConvType; // Troy units
  muTroyPounds: TabfConvType;
  muPennyweights: TabfConvType;
  muTroyDrams: TabfConvType;
  muScrupules: TabfConvType;

  muDrams: TabfConvType; // Avoirdupois Units

//==============================================================================
// Power conversion consts
//==============================================================================
// Basic unit of measurement is Watts

  cbPower: TabfConvFamily;

  puWatts: TabfConvType;
  puMiliWatts: TabfConvType;
  puKiloWatts: TabfConvType;
  puMegaWatts: TabfConvType;
  puGigaWatts: TabfConvType;
  puTeraWatts: TabfConvType;
  puJoulesPerSec: TabfConvType;
  puKiloJoulesPerMin: TabfConvType;
  puMegaJoulesPerHour: TabfConvType;
  puKilogramForceMetersPerSec: TabfConvType;
  puFootPoundForcePerSec: TabfConvType;
  puCaloriesPerSec: TabfConvType;
  puCaloriesPerMin: TabfConvType;
  puKiloCaloriesPerSec: TabfConvType;
  puKiloCaloriesPerMin: TabfConvType;
  puKiloCaloriesPerHour: TabfConvType;
  puHorsePowerElectric: TabfConvType;
  puHorsePowerMetric: TabfConvType;
  puBTUPerSec: TabfConvType;
  puBTUPerMin: TabfConvType;
  puBTUPerHour: TabfConvType;
  puThermsPerHour: TabfConvType;


//==============================================================================
// Prefixes conversion consts
//==============================================================================
// Conversions for 10^x

  cbPrefixes: TabfConvFamily;

  puYotta: TabfConvType;
  puZetta: TabfConvType;
  puExa  : TabfConvType;
  puPeta : TabfConvType;
  puTera : TabfConvType;
  puGiga : TabfConvType;
  puMega : TabfConvType;
  puKilo : TabfConvType;
  puHecto: TabfConvType;
  puDeka : TabfConvType;

  puNone : TabfConvType;

  puDeci : TabfConvType;
  puCenti: TabfConvType;
  puMilli: TabfConvType;
  puMicro: TabfConvType;
  puNano : TabfConvType;
  puPico : TabfConvType;
  puFemto: TabfConvType;
  puAtto : TabfConvType;
  puZepto: TabfConvType;
  puYocto: TabfConvType;

//==============================================================================
// Pressure conversion units
//==============================================================================
// Basic unit of measurement is Pascal

  cbPressure: TabfConvFamily;

  puPascals                      : TabfConvType;
  puKiloPascals                  : TabfConvType;
  puMegaPascals                  : TabfConvType;
  puAtmospheres                  : TabfConvType;
  puBars                         : TabfConvType;
  puMiliBars                     : TabfConvType;
  puNewtonsPerSquareMeter        : TabfConvType;
  puNewtonsPerSquareCentiMeter   : TabfConvType;
  puKiloNewtonsPerSquareMeter    : TabfConvType;
  puKilogramsPerSquareMeter      : TabfConvType;
  puKilogramsPerSquareCentiMeter : TabfConvType;
  puMetricTonsPerSquareMeter     : TabfConvType;
  puMetersOfWater                : TabfConvType;
  puMiliMetersOfWater            : TabfConvType;
  puMiliMetersOfMercury          : TabfConvType;
  puTorrs                        : TabfConvType;
  puInchesOfWater                : TabfConvType;
  puFeetOfWater                  : TabfConvType;
  puInchesOfMercury              : TabfConvType;
  puPoundsPerSquareInch          : TabfConvType;
  puPoundsPerSquareFoot          : TabfConvType;
  puUSTonsPerSquareFoot          : TabfConvType;
  puUKTonsPerSquareFoot          : TabfConvType;

//==============================================================================
// Speed conversion units
//==============================================================================
// Basic unit of measurement is Meter/Second

  cbSpeed: TabfConvFamily;

  suMetersPerSec: TabfConvType;
  suMetersPerMin: TabfConvType;
  suMetersPerHour: TabfConvType;
  suMetersPerDay: TabfConvType;
  suKilometersPerSec: TabfConvType;
  suKilometersPerMin: TabfConvType;
  suKilometersPerHour: TabfConvType;
  suKilometersPerDay: TabfConvType;
  suMilimetersPerSec: TabfConvType;
  suCentimetersPerSec: TabfConvType;
  suInchesPerSec: TabfConvType;
  suInchesPerMin: TabfConvType;
  suInchesPerHour: TabfConvType;
  suInchesPerDay: TabfConvType;
  suFeetPerSec: TabfConvType;
  suFeetPerMin: TabfConvType;
  suFeetPerHour: TabfConvType;
  suFeetPerDay: TabfConvType;
  suFurlongsPerSec: TabfConvType;
  suFurlongsPerMin: TabfConvType;
  suFurlongsPerHour: TabfConvType;
  suFurlongsPerDay: TabfConvType;
  suMilesPerSec: TabfConvType;
  suMilesPerMin: TabfConvType;
  suMilesPerHour: TabfConvType;
  suMilesPerDay: TabfConvType;
  suLeaguesPerSec: TabfConvType;
  suLeaguesPerMin: TabfConvType;
  suLeaguesPerHour: TabfConvType;
  suLeaguesPerDay: TabfConvType;
  suKnots: TabfConvType;
  suMach: TabfConvType;

//==============================================================================
// Temperature conversion units
//==============================================================================
// Basic unit of measurement is celsius

  cbTemperature: TabfConvFamily;

  tuCelsius: TabfConvType;
  tuKelvin: TabfConvType;
  tuFahrenheit: TabfConvType;
  tuRankine: TabfConvType;
  tuReaumur: TabfConvType;

//==============================================================================
// Time conversion units
//==============================================================================
// Basic unit of measurement is days (which is also the same as TDateTime)

  cbTime: TabfConvFamily;

  tuMilliSeconds: TabfConvType;
  tuSeconds: TabfConvType;
  tuMinutes: TabfConvType;
  tuHours: TabfConvType;
  tuDays: TabfConvType;
  tuWeeks: TabfConvType;
  tuFortnights: TabfConvType;
  tuMonths: TabfConvType;
  tuYears: TabfConvType;
  tuDecades: TabfConvType;
  tuCenturies: TabfConvType;
  tuMillennia: TabfConvType;
  tuDateTime: TabfConvType;
  tuJulianDate: TabfConvType;
  tuModifiedJulianDate: TabfConvType;

//==============================================================================
// Volume conversion units
//==============================================================================
// Basic unit of measurement is cubic meters

  cbVolume: TabfConvFamily;

  vuCubicMillimeters: TabfConvType;
  vuCubicCentimeters: TabfConvType;
  vuCubicDecimeters: TabfConvType;
  vuCubicMeters: TabfConvType;
  vuCubicDekameters: TabfConvType;
  vuCubicHectometers: TabfConvType;
  vuCubicKilometers: TabfConvType;
  vuCubicInches: TabfConvType;
  vuCubicFeet: TabfConvType;
  vuCubicYards: TabfConvType;
  vuCubicMiles: TabfConvType;
  vuMilliLiters: TabfConvType;
  vuCentiLiters: TabfConvType;
  vuDeciLiters: TabfConvType;
  vuLiters: TabfConvType;
  vuDekaLiters: TabfConvType;
  vuHectoLiters: TabfConvType;
  vuKiloLiters: TabfConvType;
  vuAcreFeet: TabfConvType;
  vuAcreInches: TabfConvType;
  vuBarrels: TabfConvType;
  vuCordFeet: TabfConvType;
  vuCords: TabfConvType;
  vuDecisteres: TabfConvType;
  vuSteres: TabfConvType;
  vuDekasteres: TabfConvType;

// American Fluid Units
  vuUSGallons: TabfConvType;
  vuUSQuarts: TabfConvType;
  vuUSPints: TabfConvType;
  vuUSCups: TabfConvType;
  vuUSGills: TabfConvType;
  vuUSOunces: TabfConvType;
  vuUSTablespoons: TabfConvType;
  vuUSTeaspoons: TabfConvType;

// American Dry Units
  vuUSDryGallons: TabfConvType;
  vuUSDryQuarts: TabfConvType;
  vuUSDryPints: TabfConvType;
  vuUSPecks: TabfConvType;
  vuUSBuckets: TabfConvType;
  vuUSBushels: TabfConvType;

// English Imperial Fluid/Dry Units
  vuUKGallons: TabfConvType;
  vuUKPottles: TabfConvType;
  vuUKQuarts: TabfConvType;
  vuUKPints: TabfConvType;
  vuUKGills: TabfConvType;
  vuUKOunces: TabfConvType;
  vuUKPecks: TabfConvType;
  vuUKBuckets: TabfConvType;
  vuUKBushels: TabfConvType;

//==============================================================================
// Information conversion unit
//==============================================================================
// Basic unit of measurement is gigabit

  cbInformation: TabfConvFamily;

  cuBit: TabfConvType;
  cuKilobit: TabfConvType;
  cuMegabit: TabfConvType;
  cuGigabit: TabfConvType;
  cuTerabit: TabfConvType;
  cuByte: TabfConvType;
  cuKilobyte: TabfConvType;
  cuMegabyte: TabfConvType;
  cuGigabyte: TabfConvType;
  cuTerabyte: TabfConvType;
  cuWord: TabfConvType;
  cuDWord: TabfConvType;
  cuQWord: TabfConvType;

//==============================================================================
// Date conversion
//==============================================================================

function ConvDateTimeToJulianDate(const AValue: Double): Double;
function ConvJulianDateToDateTime(const AValue: Double): Double;
function ConvDateTimeToModifiedJulianDate(const AValue: Double): Double;
function ConvModifiedJulianDateToDateTime(const AValue: Double): Double;


//==============================================================================
// Temperature conversion
//==============================================================================

function FahrenheitToCelsius(const AValue: Double): Double;
function CelsiusToFahrenheit(const AValue: Double): Double;
function KelvinToCelsius(const AValue: Double): Double;
function CelsiusToKelvin(const AValue: Double): Double;
function RankineToCelsius(const AValue: Double): Double;
function CelsiusToRankine(const AValue: Double): Double;
function ReaumurToCelsius(const AValue: Double): Double;
function CelsiusToReaumur(const AValue: Double): Double;


{******************************************************************************}
implementation
{******************************************************************************}

const
  HoursPerDay   = 24;
  MinsPerDay    = HoursPerDay * 60;
  SecsPerDay    = MinsPerDay * 60;
  MSecsPerDay   = SecsPerDay * 1000;

  DaysPerWeek = 7;
  WeeksPerFortnight = 2;
  MonthsPerYear = 12;
  YearsPerDecade = 10;
  YearsPerCentury = 100;
  YearsPerMillennium = 1000;

  DayMonday = 1;
  DayTuesday = 2;
  DayWednesday = 3;
  DayThursday = 4;
  DayFriday = 5;
  DaySaturday = 6;
  DaySunday = 7;

  OneHour = 1 / HoursPerDay;
  OneMinute = 1 / MinsPerDay;
  OneSecond = 1 / SecsPerDay;
  OneMillisecond = 1 / MSecsPerDay;

// This is actual days per year but you need to know if it's a leap year
  DaysPerYear: array [Boolean] of Word = (365, 366);

var
// average over a 4 year span
  ApproxDaysPerMonth: Double = 30.4375;
  ApproxDaysPerYear: Double  = 365.25;

// The above are the average days per month/year over a normal 4 year period.
// We use these approximations because they are more accurate for the next
// century or so.  After that you may want to switch over to these 400 year
// approximations...
//   ApproxDaysPerMonth = 30.436875 
//   ApproxDaysPerYear  = 365.2425

//------------------------------------------------------------------------------

function DateTimeToJulianDate(const AValue: TDateTime): Double;
var
  LYear, LMonth, LDay: Word;
begin
  DecodeDate(AValue, LYear, LMonth, LDay);
  Result := (1461 * (LYear + 4800 + (LMonth - 14) div 12)) div 4 +
            (367 * (LMonth - 2 - 12 * ((LMonth - 14) div 12))) div 12 -
            (3 * ((LYear + 4900 + (LMonth - 14) div 12) div 100)) div 4 +
            LDay - 32075.5 + Frac(AValue);
end;

//------------------------------------------------------------------------------

function JulianDateToDateTime(const AValue: Double): TDateTime;
var
  L, N, LYear, LMonth, LDay: Integer;
begin
  L := Trunc(AValue) + 68570;
  N := 4 * L div 146097;
  L := L - (146097 * N + 3) div 4;
  LYear := 4000 * (L + 1) div 1461001;
  L := L - 1461 * LYear div 4 + 31;
  LMonth := 80 * L div 2447;
  LDay := L - 2447 * LMonth div 80;
  L := LMonth div 11;
  LMonth := LMonth + 2 - 12 * L;
  LYear := 100 * (N - 49) + LYear + L;
  Result := EncodeDate(LYear, LMonth, LDay) + Frac(AValue) - 0.5;
end;

//------------------------------------------------------------------------------

function DateTimeToModifiedJulianDate(const AValue: TDateTime): Double;
begin
  Result := DateTimeToJulianDate(AValue) - 2400000.5;
end;

//------------------------------------------------------------------------------

function ModifiedJulianDateToDateTime(const AValue: Double): TDateTime;
begin
  Result := JulianDateToDateTime(AValue + 2400000.5);
end;


//==============================================================================
// Date conversion
//==============================================================================

function ConvDateTimeToJulianDate(const AValue: Double): Double;
begin
  Result := DateTimeToJulianDate(AValue);
end;

//------------------------------------------------------------------------------

function ConvJulianDateToDateTime(const AValue: Double): Double;
begin
  Result := JulianDateToDateTime(AValue);
end;

//------------------------------------------------------------------------------

function ConvDateTimeToModifiedJulianDate(const AValue: Double): Double;
begin
  Result := DateTimeToModifiedJulianDate(AValue);
end;

//------------------------------------------------------------------------------

function ConvModifiedJulianDateToDateTime(const AValue: Double): Double;
begin
  Result := ModifiedJulianDateToDateTime(AValue);
end;


//==============================================================================
// Fuel Consumption conversion
//==============================================================================

//------------------------------------------------------------------------------

function _KilometersPerLiter(const AValue: Double): Double;
begin
  if AValue = 0.0 then Result := 0.0 else Result := 100 / AValue;
end;

//------------------------------------------------------------------------------

function _MilesPerLiter(const AValue: Double): Double;
begin
  if AValue = 0.0 then Result := 0.0 else
    Result := 100 / AValue * MilesPerKilometer;
end;

//------------------------------------------------------------------------------

function _KilometersPerUSGallon(const AValue: Double): Double;
begin
  if AValue = 0.0 then Result := 0.0 else
    Result := 100 / AValue * LitersPerUSGallon;
end;

//------------------------------------------------------------------------------

function _MilesPerUSGallon(const AValue: Double): Double;
begin
  if AValue = 0.0 then Result := 0.0 else
    Result := 100 / AValue * LitersPerUSGallon * MilesPerKilometer;
end;

//------------------------------------------------------------------------------

function _KilometersPerUKGallon(const AValue: Double): Double;
begin
  if AValue = 0.0 then Result := 0.0 else
    Result := 100 / AValue * LitersPerUKGallon;
end;

//------------------------------------------------------------------------------

function _MilesPerUKGallon(const AValue: Double): Double;
begin
  if AValue = 0.0 then Result := 0.0 else
    Result := 100 / AValue * LitersPerUKGallon * MilesPerKilometer;
end;


//==============================================================================
// Temperature conversion
//==============================================================================

//------------------------------------------------------------------------------

function FahrenheitToCelsius(const AValue: Double): Double;
begin
  Result := ((AValue - 32) * 5) / 9;
end;

//------------------------------------------------------------------------------

function CelsiusToFahrenheit(const AValue: Double): Double;
begin
  Result := ((AValue * 9) / 5) + 32;
end;

//------------------------------------------------------------------------------


function KelvinToCelsius(const AValue: Double): Double;
begin
  Result := AValue - 273.15;
end;

//------------------------------------------------------------------------------

function CelsiusToKelvin(const AValue: Double): Double;
begin
  Result := AValue + 273.15;
end;

//------------------------------------------------------------------------------

function RankineToCelsius(const AValue: Double): Double;
begin
  Result := FahrenheitToCelsius(AValue - 459.67);
end;

//------------------------------------------------------------------------------

function CelsiusToRankine(const AValue: Double): Double;
begin
  Result := CelsiusToFahrenheit(AValue) + 459.67;
end;

//------------------------------------------------------------------------------

function ReaumurToCelsius(const AValue: Double): Double;
begin
//  Result := ((CelsiusToFahrenheit(AValue) - 32) * 4) / 9;
  Result := AValue * 1.25;
end;

//------------------------------------------------------------------------------

function CelsiusToReaumur(const AValue: Double): Double;
begin
//  Result := FahrenheitToCelsius(((AValue * 9) / 4) + 32);
  Result := AValue * 0.8;
end;

//------------------------------------------------------------------------------


{******************************************************************************}
initialization
{******************************************************************************}

//==============================================================================
// Areas conversion units
//==============================================================================
// Basic unit of measurement is square meters

  cbArea := RegisterConversionFamily(ScbArea);

  auSquareMillimeters := RegisterConversionType(cbArea, SauSquareMillimeters, SauaSquareMillimeters, 1E-6);
  auSquareCentimeters := RegisterConversionType(cbArea, SauSquareCentimeters, SauaSquareCentimeters, 0.0001);
  auSquareDecimeters  := RegisterConversionType(cbArea, SauSquareDecimeters , SauaSquareDecimeters , 0.01);
  auSquareMeters      := RegisterConversionType(cbArea, SauSquareMeters     , SauaSquareMeters     , 1);
  auSquareDekameters  := RegisterConversionType(cbArea, SauSquareDekameters , SauaSquareDekameters , 100);
  auSquareHectometers := RegisterConversionType(cbArea, SauSquareHectometers, SauaSquareHectometers, 10000);
  auSquareKilometers  := RegisterConversionType(cbArea, SauSquareKilometers , SauaSquareKilometers , 1E+6);
  auSquareInches      := RegisterConversionType(cbArea, SauSquareInches     , SauaSquareInches     , SquareMetersPerSquareInch);
  auSquareFeet        := RegisterConversionType(cbArea, SauSquareFeet       , SauaSquareFeet       , SquareMetersPerSquareFoot);
  auSquareYards       := RegisterConversionType(cbArea, SauSquareYards      , SauaSquareYards      , SquareMetersPerSquareYard);
  auSquareMiles       := RegisterConversionType(cbArea, SauSquareMiles      , SauaSquareMiles      , SquareMetersPerSquareMile);
  auAcres             := RegisterConversionType(cbArea, SauAcres            , SauaAcres            , SquareMetersPerAcre);
  auCentares          := RegisterConversionType(cbArea, SauCentares         , SauaCentares         , 1);
  auAres              := RegisterConversionType(cbArea, SauAres             , SauaAres             , 100);
  auHectares          := RegisterConversionType(cbArea, SauHectares         , SauaHectares         , 10000);
  auBarns             := RegisterConversionType(cbArea, SauBarns            , SauaBarns            , 1E-28);
  auSquareRods        := RegisterConversionType(cbArea, SauSquareRods       , SauaSquareRods       , SquareMetersPerSquareRod);


//==============================================================================
// Distance conversion units
//==============================================================================
// Basic unit of measurement is meters

  cbDistance := RegisterConversionFamily(ScbDistance);

  duMicromicrons      := RegisterConversionType(cbDistance, SduMicromicrons     , SduaMicromicrons     , 1E-12);
  duAngstroms         := RegisterConversionType(cbDistance, SduAngstroms        , SduaAngstroms        , 1E-10);
  duMillimicrons      := RegisterConversionType(cbDistance, SduMillimicrons     , SduaMillimicrons     , 1E-9);
  duMicrons           := RegisterConversionType(cbDistance, SduMicrons          , SduaMicrons          , 1E-6);
  duMillimeters       := RegisterConversionType(cbDistance, SduMillimeters      , SduaMillimeters      , 0.001);
  duCentimeters       := RegisterConversionType(cbDistance, SduCentimeters      , SduaCentimeters      , 0.01);
  duDecimeters        := RegisterConversionType(cbDistance, SduDecimeters       , SduaDecimeters       , 0.1);
  duMeters            := RegisterConversionType(cbDistance, SduMeters           , SduaMeters           , 1);
  duDekameters        := RegisterConversionType(cbDistance, SduDekameters       , SduaDekameters       , 10);
  duHectometers       := RegisterConversionType(cbDistance, SduHectometers      , SduaHectometers      , 100);
  duKilometers        := RegisterConversionType(cbDistance, SduKilometers       , SduaKilometers       , 1000);
  duMegameters        := RegisterConversionType(cbDistance, SduMegameters       , SduaMegameters       , 1E+6);
  duGigameters        := RegisterConversionType(cbDistance, SduGigameters       , SduaGigameters       , 1E+9);

  duMils              := RegisterConversionType(cbDistance, SduMils             , SduaMils             , MetersPerMil);
  duLines             := RegisterConversionType(cbDistance, SduLines            , SduaLines            , MetersPerLine);
  duInches            := RegisterConversionType(cbDistance, SduInches           , SduaInches           , MetersPerInch);
  duSpans             := RegisterConversionType(cbDistance, SduSpans            , SduaSpans            , MetersPerSpan);
  duFeet              := RegisterConversionType(cbDistance, SduFeet             , SduaFeet             , MetersPerFoot);
  duYards             := RegisterConversionType(cbDistance, SduYards            , SduaYards            , MetersPerYard);
  duFathoms           := RegisterConversionType(cbDistance, SduFathoms          , SduaFathoms          , MetersPerFathom);
  duRods              := RegisterConversionType(cbDistance, SduRods             , SduaRods             , MetersPerRod);
  duChains            := RegisterConversionType(cbDistance, SduChains           , SduaChains           , MetersPerChain);
  duFurlongs          := RegisterConversionType(cbDistance, SduFurlongs         , SduaFurlongs         , MetersPerFurlong);
  duMiles             := RegisterConversionType(cbDistance, SduMiles            , SduaMiles            , MetersPerMile);
  duLeagues           := RegisterConversionType(cbDistance, SduLeagues          , SduaLeagues          , 3 * MetersPerMile);

  duCables            := RegisterConversionType(cbDistance, SduCables           , SduaCables           , MetersPerNauticalMiles / 10);
  duUSCables          := RegisterConversionType(cbDistance, SduUSCables         , SduaUSCables         , 219.456);
  duNauticalMiles     := RegisterConversionType(cbDistance, SduNauticalMiles    , SduaNauticalMiles    , MetersPerNauticalMiles);
  duNauticalLeagues   := RegisterConversionType(cbDistance, SduNauticalLeagues  , SduaNauticalLeagues  , 3 * MetersPerNauticalMiles);

  duLightSeconds      := RegisterConversionType(cbDistance, SduLightSeconds     , SduaLightSeconds     , MetersPerLightSecond);
  duLightMinutes      := RegisterConversionType(cbDistance, SduLightMinutes     , SduaLightMinutes     , MetersPerLightSecond * 60);
  duLightYears        := RegisterConversionType(cbDistance, SduLightYears       , SduaLightYears       , MetersPerLightYear);
  duParsecs           := RegisterConversionType(cbDistance, SduParsecs          , SduaParsecs          , MetersPerParsec);
  duAstronomicalUnits := RegisterConversionType(cbDistance, SduAstronomicalUnits, SduaAstronomicalUnits, MetersPerAstronomicalUnit);

  duHands             := RegisterConversionType(cbDistance, SduHands            , SduaHands            , MetersPerHand);
  duLinks             := RegisterConversionType(cbDistance, SduLinks            , SduaLinks            , MetersPerLink);
  duCubits            := RegisterConversionType(cbDistance, SduCubits           , SduaCubits           , MetersPerCubit);
  duPaces             := RegisterConversionType(cbDistance, SduPaces            , SduaPaces            , MetersPerPace);

  duPicas             := RegisterConversionType(cbDistance, SduPicas            , SduaPicas            , MetersPerPica);

  duXUnits            := RegisterConversionType(cbDistance, SduXUnits           , SduaXUnits           , 1.00206E-13);


//==============================================================================
// Fuel Consumption conversion units
//==============================================================================
// Basic unit of measurement is "litres per 100 km"

  cbFuelConsumption := RegisterConversionFamily(ScbFuelConsumption);

  fuLitersPer100KiloMeters    := RegisterConversionType(cbFuelConsumption, SfuLitersPer100KiloMeters   , SfuaLitersPer100KiloMeters   , 1);
  fuLitersPer100Miles         := RegisterConversionType(cbFuelConsumption, SfuLitersPer100Miles        , SfuaLitersPer100Miles        , 1 / KilometersPerMile);
  fuUSGallonsPer100KiloMeters := RegisterConversionType(cbFuelConsumption, SfuUSGallonsPer100KiloMeters, SfuaUSGallonsPer100KiloMeters, LitersPerUSGallon);
  fuUSGallonsPer100Miles      := RegisterConversionType(cbFuelConsumption, SfuUSGallonsPer100Miles     , SfuaUSGallonsPer100Miles     , LitersPerUSGallon * (1 / KilometersPerMile));
  fuUKGallonsPer100KiloMeters := RegisterConversionType(cbFuelConsumption, SfuUKGallonsPer100KiloMeters, SfuaUKGallonsPer100KiloMeters, LitersPerUKGallon);
  fuUKGallonsPer100Miles      := RegisterConversionType(cbFuelConsumption, SfuUKGallonsPer100Miles     , SfuaUKGallonsPer100Miles     , LitersPerUKGallon * (1 / KilometersPerMile));

  fuKiloMetersPerLiter        := RegisterConversionType(cbFuelConsumption, SfuKiloMetersPerLiter       , SfuaKiloMetersPerLiter       , _KilometersPerLiter   , _KilometersPerLiter);
  fuMilesPerLiter             := RegisterConversionType(cbFuelConsumption, SfuMilesPerLiter            , SfuaMilesPerLiter            , _MilesPerLiter        , _MilesPerLiter);
  fuKiloMetersPerUSGallon     := RegisterConversionType(cbFuelConsumption, SfuKiloMetersPerUSGallon    , SfuaKiloMetersPerUSGallon    , _KilometersPerUSGallon, _KilometersPerUSGallon);
  fuMilesPerUSGallon          := RegisterConversionType(cbFuelConsumption, SfuMilesPerUSGallon         , SfuaMilesPerUSGallon         , _MilesPerUSGallon     , _MilesPerUSGallon);
  fuKiloMetersPerUKGallon     := RegisterConversionType(cbFuelConsumption, SfuKiloMetersPerUKGallon    , SfuaKiloMetersPerUKGallon    , _KilometersPerUKGallon, _KilometersPerUKGallon);
  fuMilesPerUKGallon          := RegisterConversionType(cbFuelConsumption, SfuMilesPerUKGallon         , SfuaMilesPerUKGallon         , _MilesPerUKGallon     , _MilesPerUKGallon);


//==============================================================================
// Mass conversion units
//==============================================================================
// Basic unit of measurement is grams

  cbMass := RegisterConversionFamily(ScbMass);

  muNanograms      := RegisterConversionType(cbMass, SmuNanograms     , SmuaNanograms     , 1E-9);
  muMicrograms     := RegisterConversionType(cbMass, SmuMicrograms    , SmuaMicrograms    , 1E-6);
  muMilligrams     := RegisterConversionType(cbMass, SmuMilligrams    , SmuaMilligrams    , 0.001);
  muCentigrams     := RegisterConversionType(cbMass, SmuCentigrams    , SmuaCentigrams    , 0.01);
  muDecigrams      := RegisterConversionType(cbMass, SmuDecigrams     , SmuaDecigrams     , 0.1);
  muGrams          := RegisterConversionType(cbMass, SmuGrams         , SmuaGrams         , 1);
  muDekagrams      := RegisterConversionType(cbMass, SmuDekagrams     , SmuaDekagrams     , 10);
  muHectograms     := RegisterConversionType(cbMass, SmuHectograms    , SmuaHectograms    , 100);
  muKilograms      := RegisterConversionType(cbMass, SmuKilograms     , SmuaKilograms     , 1000);
  muMetricCentners := RegisterConversionType(cbMass, SmuMetricCentners, SmuaMetricCentners, 1E+5);
  muMetricTons     := RegisterConversionType(cbMass, SmuMetricTons    , SmuaMetricTons    , 1E+6);
  muMetricCarats   := RegisterConversionType(cbMass, SmuMetricCarats  , SmuaMetricCarats  , 0.2);

  muGrains         := RegisterConversionType(cbMass, SmuGrains        , SmuaGrains        , GramsPerGrains);
  muOunces         := RegisterConversionType(cbMass, SmuOunces        , SmuaOunces        , GramsPerOunces);
  muStones         := RegisterConversionType(cbMass, SmuStones        , SmuaStones        , GramsPerPound * 14);
  muPounds         := RegisterConversionType(cbMass, SmuPounds        , SmuaPounds        , GramsPerPound);
  muTons           := RegisterConversionType(cbMass, SmuTons          , SmuaTons          , GramsPerTons);
  muLongTons       := RegisterConversionType(cbMass, SmuLongTons      , SmuaLongTons      , GramsPerLongTons);

  muUSCentners     := RegisterConversionType(cbMass, SmuUSCentners    , SmuaUSCentners    , GramsPerPound * 100);
  muUKCentners     := RegisterConversionType(cbMass, SmuUKCentners    , SmuaUKCentners    , GramsPerPound * 112);

  muPennyweights   := RegisterConversionType(cbMass, SmuPennyweights  , SmuaPennyweights  , GramsPerPennyweights);
  muScrupules      := RegisterConversionType(cbMass, SmuScrupules     , SmuaScrupules     , GramsPerTroyDrams / 3);
  muTroyDrams      := RegisterConversionType(cbMass, SmuTroyDrams     , SmuaTroyDrams     , GramsPerTroyDrams);
  muTroyOunces     := RegisterConversionType(cbMass, SmuTroyOunces    , SmuaTroyOunces    , GramsPerTroyOunces);
  muTroyPounds     := RegisterConversionType(cbMass, SmuTroyPounds    , SmuaTroyPounds    , GramsPerTroyPound);

  muDrams          := RegisterConversionType(cbMass, SmuDrams         , SmuaDrams         , GramsPerDrams);


//==============================================================================
// Power conversion consts
//==============================================================================
// Basic unit of measurement is Watts

  cbPower := RegisterConversionFamily(ScbPower);

  puWatts                     := RegisterConversionType(cbPower, SpuWatts                    , SpuaWatts                    , 1);
  puMiliWatts                 := RegisterConversionType(cbPower, SpuMiliWatts                , SpuaMiliWatts                , 1E-3);
  puKiloWatts                 := RegisterConversionType(cbPower, SpuKiloWatts                , SpuaKiloWatts                , 1E+3);
  puMegaWatts                 := RegisterConversionType(cbPower, SpuMegaWatts                , SpuaMegaWatts                , 1E+6);
  puGigaWatts                 := RegisterConversionType(cbPower, SpuGigaWatts                , SpuaGigaWatts                , 1E+9);
  puTeraWatts                 := RegisterConversionType(cbPower, SpuTeraWatts                , SpuaTeraWatts                , 1E+12);
  puJoulesPerSec              := RegisterConversionType(cbPower, SpuJoulesPerSec             , SpuaJoulesPerSec             , 1);
  puKiloJoulesPerMin          := RegisterConversionType(cbPower, SpuKiloJoulesPerMin         , SpuaKiloJoulesPerMin         , 16.666666666666666666666666666667);
  puMegaJoulesPerHour         := RegisterConversionType(cbPower, SpuMegaJoulesPerHour        , SpuaMegaJoulesPerHour        , 277.77777777777777777777777777778);
  puKilogramForceMetersPerSec := RegisterConversionType(cbPower, SpuKilogramForceMetersPerSec, SpuaKilogramForceMetersPerSec, 9.80665);
  puFootPoundForcePerSec      := RegisterConversionType(cbPower, SpuFootPoundForcePerSec     , SpuaFootPoundForcePerSec     , 1.35581795);
  puCaloriesPerSec            := RegisterConversionType(cbPower, SpuCaloriesPerSec           , SpuaCaloriesPerSec           , 4.1868);
  puCaloriesPerMin            := RegisterConversionType(cbPower, SpuCaloriesPerMin           , SpuaCaloriesPerMin           , 0.06978);
  puKiloCaloriesPerSec        := RegisterConversionType(cbPower, SpuKiloCaloriesPerSec       , SpuaKiloCaloriesPerSec       , 4186.8);
  puKiloCaloriesPerMin        := RegisterConversionType(cbPower, SpuKiloCaloriesPerMin       , SpuaKiloCaloriesPerMin       , 69.78);
  puKiloCaloriesPerHour       := RegisterConversionType(cbPower, SpuKiloCaloriesPerHour      , SpuaKiloCaloriesPerHour      , 1.163);
  puHorsePowerElectric        := RegisterConversionType(cbPower, SpuHorsePowerElectric       , SpuaHorsePowerElectric       , 746);
  puHorsePowerMetric          := RegisterConversionType(cbPower, SpuHorsePowerMetric         , SpuaHorsePowerMetric         , 735.49875);
  puBTUPerSec                 := RegisterConversionType(cbPower, SpuBTUPerSec                , SpuaBTUPerSec                , 1055.05583);
  puBTUPerMin                 := RegisterConversionType(cbPower, SpuBTUPerMin                , SpuaBTUPerMin                , 17.5842638);
  puBTUPerHour                := RegisterConversionType(cbPower, SpuBTUPerHour               , SpuaBTUPerHour               , 0.293071063);
  puThermsPerHour             := RegisterConversionType(cbPower, SpuThermsPerHour            , SpuaThermsPerHour            , 29307.1063);


//==============================================================================
// Prefixes conversion consts
//==============================================================================
// Conversions for 10^x

  cbPrefixes := RegisterConversionFamily(ScbPrefixes);

  puYotta := RegisterConversionType(cbPrefixes, SpuYotta, SpuaYotta, 1E+24);
  puZetta := RegisterConversionType(cbPrefixes, SpuZetta, SpuaZetta, 1E+21);
  puExa   := RegisterConversionType(cbPrefixes, SpuExa  , SpuaExa  , 1E+18);
  puPeta  := RegisterConversionType(cbPrefixes, SpuPeta , SpuaPeta , 1E+15);
  puTera  := RegisterConversionType(cbPrefixes, SpuTera , SpuaTera , 1E+12);
  puGiga  := RegisterConversionType(cbPrefixes, SpuGiga , SpuaGiga , 1E+9);
  puMega  := RegisterConversionType(cbPrefixes, SpuMega , SpuaMega , 1E+6);
  puKilo  := RegisterConversionType(cbPrefixes, SpuKilo , SpuaKilo , 1E+3);
  puHecto := RegisterConversionType(cbPrefixes, SpuHecto, SpuaHecto, 1E+2);
  puDeka  := RegisterConversionType(cbPrefixes, SpuDeka , SpuaDeka , 1E+1);

  puNone  := RegisterConversionType(cbPrefixes, SpuNone , SpuaNone , 1E+0);

  puDeci  := RegisterConversionType(cbPrefixes, SpuDeci , SpuaDeci , 1E-1);
  puCenti := RegisterConversionType(cbPrefixes, SpuCenti, SpuaCenti, 1E-2);
  puMilli := RegisterConversionType(cbPrefixes, SpuMilli, SpuaMilli, 1E-3);
  puMicro := RegisterConversionType(cbPrefixes, SpuMicro, SpuaMicro, 1E-6);
  puNano  := RegisterConversionType(cbPrefixes, SpuNano , SpuaNano , 1E-9);
  puPico  := RegisterConversionType(cbPrefixes, SpuPico , SpuaPico , 1E-12);
  puFemto := RegisterConversionType(cbPrefixes, SpuFemto, SpuaFemto, 1E-15);
  puAtto  := RegisterConversionType(cbPrefixes, SpuAtto , SpuaAtto , 1E-18);
  puZepto := RegisterConversionType(cbPrefixes, SpuZepto, SpuaZepto, 1E-21);
  puYocto := RegisterConversionType(cbPrefixes, SpuYocto, SpuaYocto, 1E-24);


//==============================================================================
// Pressure conversion units
//==============================================================================
// Basic unit of measurement is Pascal

  cbPressure := RegisterConversionFamily(ScbPressure);

  puPascals                      := RegisterConversionType(cbPressure, SpuPascals                     , SpuaPascals                     , 1);
  puKiloPascals                  := RegisterConversionType(cbPressure, SpuKiloPascals                 , SpuaKiloPascals                 , 1E+3);
  puMegaPascals                  := RegisterConversionType(cbPressure, SpuMegaPascals                 , SpuaMegaPascals                 , 1E+6);
  puNewtonsPerSquareMeter        := RegisterConversionType(cbPressure, SpuNewtonsPerSquareMeter       , SpuaNewtonsPerSquareMeter       , 1);
  puNewtonsPerSquareCentiMeter   := RegisterConversionType(cbPressure, SpuNewtonsPerSquareCentiMeter  , SpuaNewtonsPerSquareCentiMeter  , 10000);
  puKiloNewtonsPerSquareMeter    := RegisterConversionType(cbPressure, SpuKiloNewtonsPerSquareMeter   , SpuaKiloNewtonsPerSquareMeter   , 1000);
  puKilogramsPerSquareMeter      := RegisterConversionType(cbPressure, SpuKilogramsPerSquareMeter     , SpuaKilogramsPerSquareMeter     , 9.80665);
  puKilogramsPerSquareCentiMeter := RegisterConversionType(cbPressure, SpuKilogramsPerSquareCentiMeter, SpuaKilogramsPerSquareCentiMeter, 98066.5);
  puMetricTonsPerSquareMeter     := RegisterConversionType(cbPressure, SpuMetricTonsPerSquareMeter    , SpuaMetricTonsPerSquareMeter    , 9806.65);
  puPoundsPerSquareInch          := RegisterConversionType(cbPressure, SpuPoundsPerSquareInch         , SpuaPoundsPerSquareInch         , 6894.75729);
  puPoundsPerSquareFoot          := RegisterConversionType(cbPressure, SpuPoundsPerSquareFoot         , SpuaPoundsPerSquareFoot         , 47.880259);
  puUSTonsPerSquareFoot          := RegisterConversionType(cbPressure, SpuUSTonsPerSquareFoot         , SpuaUSTonsPerSquareFoot         , 95760.518);
  puUKTonsPerSquareFoot          := RegisterConversionType(cbPressure, SpuUKTonsPerSquareFoot         , SpuaUKTonsPerSquareFoot         , 107251.78);
  puAtmospheres                  := RegisterConversionType(cbPressure, SpuAtmospheres                 , SpuaAtmospheres                 , 101325);
  puBars                         := RegisterConversionType(cbPressure, SpuBars                        , SpuaBars                        , 100000);
  puMiliBars                     := RegisterConversionType(cbPressure, SpuMiliBars                    , SpuaMiliBars                    , 100);
  puTorrs                        := RegisterConversionType(cbPressure, SpuTorrs                       , SpuaTorrs                       , 133.322368);
  puMiliMetersOfMercury          := RegisterConversionType(cbPressure, SpuMiliMetersOfMercury         , SpuaMiliMetersOfMercury         , 133.322368);
  puInchesOfMercury              := RegisterConversionType(cbPressure, SpuInchesOfMercury             , SpuaInchesOfMercury             , 3386.38864);
  puMiliMetersOfWater            := RegisterConversionType(cbPressure, SpuMiliMetersOfWater           , SpuaMiliMetersOfWater           , 9.80665);
  puMetersOfWater                := RegisterConversionType(cbPressure, SpuMetersOfWater               , SpuaMetersOfWater               , 9806.65);
  puInchesOfWater                := RegisterConversionType(cbPressure, SpuInchesOfWater               , SpuaInchesOfWater               , 249.08891);
  puFeetOfWater                  := RegisterConversionType(cbPressure, SpuFeetOfWater                 , SpuaFeetOfWater                 , 2989.06692);


//==============================================================================
// Speed conversion units
//==============================================================================
// Basic unit of measurement is Meter/Second

  cbSpeed := RegisterConversionFamily(ScbSpeed);

  suMetersPerSec      := RegisterConversionType(cbSpeed, SsuMetersPerSec     , SsuaMetersPerSec     , 1);
  suMetersPerMin      := RegisterConversionType(cbSpeed, SsuMetersPerMin     , SsuaMetersPerMin     , 1 / 60);
  suMetersPerHour     := RegisterConversionType(cbSpeed, SsuMetersPerHour    , SsuaMetersPerHour    , 1 / 3600);
  suMetersPerDay      := RegisterConversionType(cbSpeed, SsuMetersPerDay     , SsuaMetersPerDay     , 1 / 86400);
  suKilometersPerSec  := RegisterConversionType(cbSpeed, SsuKilometersPerSec , SsuaKilometersPerSec , 1 / 0.001);
  suKilometersPerMin  := RegisterConversionType(cbSpeed, SsuKilometersPerMin , SsuaKilometersPerMin , 1 / 0.06);
  suKilometersPerHour := RegisterConversionType(cbSpeed, SsuKilometersPerHour, SsuaKilometersPerHour, 1 / 3.6);
  suKilometersPerDay  := RegisterConversionType(cbSpeed, SsuKilometersPerDay , SsuaKilometersPerDay , 1 / 86.4);
  suMilimetersPerSec  := RegisterConversionType(cbSpeed, SsuMilimetersPerSec , SsuaMilimetersPerSec , 1 / 1000);
  suCentimetersPerSec := RegisterConversionType(cbSpeed, SsuCentimetersPerSec, SsuaCentimetersPerSec, 1 / 100);

  suInchesPerSec      := RegisterConversionType(cbSpeed, SsuInchesPerSec     , SsuaInchesPerSec     , MetersPerInch);
  suInchesPerMin      := RegisterConversionType(cbSpeed, SsuInchesPerMin     , SsuaInchesPerMin     , MetersPerInch / 60);
  suInchesPerHour     := RegisterConversionType(cbSpeed, SsuInchesPerHour    , SsuaInchesPerHour    , MetersPerInch / 3600);
  suInchesPerDay      := RegisterConversionType(cbSpeed, SsuInchesPerDay     , SsuaInchesPerDay     , MetersPerInch / 86400);
  suFeetPerSec        := RegisterConversionType(cbSpeed, SsuFeetPerSec       , SsuaFeetPerSec       , MetersPerFoot);
  suFeetPerMin        := RegisterConversionType(cbSpeed, SsuFeetPerMin       , SsuaFeetPerMin       , MetersPerFoot / 60);
  suFeetPerHour       := RegisterConversionType(cbSpeed, SsuFeetPerHour      , SsuaFeetPerHour      , MetersPerFoot / 3600);
  suFeetPerDay        := RegisterConversionType(cbSpeed, SsuFeetPerDay       , SsuaFeetPerDay       , MetersPerFoot / 86400);
  suFurlongsPerSec    := RegisterConversionType(cbSpeed, SsuFurlongsPerSec   , SsuaFurlongsPerSec   , MetersPerFurlong);
  suFurlongsPerMin    := RegisterConversionType(cbSpeed, SsuFurlongsPerMin   , SsuaFurlongsPerMin   , MetersPerFurlong / 60);
  suFurlongsPerHour   := RegisterConversionType(cbSpeed, SsuFurlongsPerHour  , SsuaFurlongsPerHour  , MetersPerFurlong / 3600);
  suFurlongsPerDay    := RegisterConversionType(cbSpeed, SsuFurlongsPerDay   , SsuaFurlongsPerDay   , MetersPerFurlong / 86400);
  suMilesPerSec       := RegisterConversionType(cbSpeed, SsuMilesPerSec      , SsuaMilesPerSec      , MetersPerMile);
  suMilesPerMin       := RegisterConversionType(cbSpeed, SsuMilesPerMin      , SsuaMilesPerMin      , MetersPerMile / 60);
  suMilesPerHour      := RegisterConversionType(cbSpeed, SsuMilesPerHour     , SsuaMilesPerHour     , MetersPerMile / 3600);
  suMilesPerDay       := RegisterConversionType(cbSpeed, SsuMilesPerDay      , SsuaMilesPerDay      , MetersPerMile / 86400);
  suLeaguesPerSec     := RegisterConversionType(cbSpeed, SsuLeaguesPerSec    , SsuaLeaguesPerSec    , (3 * MetersPerMile));
  suLeaguesPerMin     := RegisterConversionType(cbSpeed, SsuLeaguesPerMin    , SsuaLeaguesPerMin    , (3 * MetersPerMile) / 60);
  suLeaguesPerHour    := RegisterConversionType(cbSpeed, SsuLeaguesPerHour   , SsuaLeaguesPerHour   , (3 * MetersPerMile) / 3600);
  suLeaguesPerDay     := RegisterConversionType(cbSpeed, SsuLeaguesPerDay    , SsuaLeaguesPerDay    , (3 * MetersPerMile) / 86400);

  suKnots             := RegisterConversionType(cbSpeed, SsuKnots            , SsuaKnots            , 0.514444444);
  suMach              := RegisterConversionType(cbSpeed, SsuMach             , SsuaMach             , 331.622);


//==============================================================================
// Temperature conversion units
//==============================================================================
// Basic unit of measurement is celsius

  cbTemperature := RegisterConversionFamily(ScbTemperature);

  tuCelsius    := RegisterConversionType(cbTemperature, StuCelsius   , StuaCelsius   , 1);
  tuFahrenheit := RegisterConversionType(cbTemperature, StuFahrenheit, StuaFahrenheit, FahrenheitToCelsius, CelsiusToFahrenheit);
  tuKelvin     := RegisterConversionType(cbTemperature, StuKelvin    , StuaKelvin    , KelvinToCelsius, CelsiusToKelvin);
  tuRankine    := RegisterConversionType(cbTemperature, StuRankine   , StuaRankine   , RankineToCelsius, CelsiusToRankine);
  tuReaumur    := RegisterConversionType(cbTemperature, StuReaumur   , StuaReaumur   , ReaumurToCelsius, CelsiusToReaumur);


//==============================================================================
// Time conversion units
//==============================================================================
// Basic unit of measurement is days (which is also the same as TDateTime)

  cbTime := RegisterConversionFamily(ScbTime);

  tuMilliSeconds       := RegisterConversionType(cbTime, StuMilliSeconds      , StuaMilliSeconds      , 1 / MSecsPerDay);
  tuSeconds            := RegisterConversionType(cbTime, StuSeconds           , StuaSeconds           , 1 / SecsPerDay);
  tuMinutes            := RegisterConversionType(cbTime, StuMinutes           , StuaMinutes           , 1 / MinsPerDay);
  tuHours              := RegisterConversionType(cbTime, StuHours             , StuaHours             , 1 / HoursPerDay);
  tuDays               := RegisterConversionType(cbTime, StuDays              , StuaDays              , 1);
  tuWeeks              := RegisterConversionType(cbTime, StuWeeks             , StuaWeeks             , DaysPerWeek);
  tuFortnights         := RegisterConversionType(cbTime, StuFortnights        , StuaFortnights        , WeeksPerFortnight * DaysPerWeek);
  tuMonths             := RegisterConversionType(cbTime, StuMonths            , StuaMonths            , ApproxDaysPerMonth);
  tuYears              := RegisterConversionType(cbTime, StuYears             , StuaYears             , ApproxDaysPerYear);
  tuDecades            := RegisterConversionType(cbTime, StuDecades           , StuaDecades           , ApproxDaysPerYear * YearsPerDecade);
  tuCenturies          := RegisterConversionType(cbTime, StuCenturies         , StuaCenturies         , ApproxDaysPerYear * YearsPerCentury);
  tuMillennia          := RegisterConversionType(cbTime, StuMillennia         , StuaMillennia         , ApproxDaysPerYear * YearsPerMillennium);
  tuDateTime           := RegisterConversionType(cbTime, StuDateTime          , StuaDateTime          , 1);
  tuJulianDate         := RegisterConversionType(cbTime, StuJulianDate        , StuaJulianDate        , ConvJulianDateToDateTime, ConvDateTimeToJulianDate);
  tuModifiedJulianDate := RegisterConversionType(cbTime, StuModifiedJulianDate, StuaModifiedJulianDate, ConvModifiedJulianDateToDateTime, ConvDateTimeToModifiedJulianDate);


//==============================================================================
// Volume conversion units
//==============================================================================
// Basic unit of measurement is cubic meters

  cbVolume := RegisterConversionFamily(ScbVolume);

  vuCubicMillimeters := RegisterConversionType(cbVolume, SvuCubicMillimeters, SvuaCubicMillimeters, 1E-9);
  vuCubicCentimeters := RegisterConversionType(cbVolume, SvuCubicCentimeters, SvuaCubicCentimeters, 1E-6);
  vuCubicDecimeters  := RegisterConversionType(cbVolume, SvuCubicDecimeters , SvuaCubicDecimeters , 0.001);
  vuCubicMeters      := RegisterConversionType(cbVolume, SvuCubicMeters     , SvuaCubicMeters     , 1);
  vuCubicDekameters  := RegisterConversionType(cbVolume, SvuCubicDekameters , SvuaCubicDekameters , 1000);
  vuCubicHectometers := RegisterConversionType(cbVolume, SvuCubicHectometers, SvuaCubicHectometers, 1E+6);
  vuCubicKilometers  := RegisterConversionType(cbVolume, SvuCubicKilometers , SvuaCubicKilometers , 1E+9);

  vuCubicInches      := RegisterConversionType(cbVolume, SvuCubicInches     , SvuaCubicInches     , CubicMetersPerCubicInch);
  vuCubicFeet        := RegisterConversionType(cbVolume, SvuCubicFeet       , SvuaCubicFeet       , CubicMetersPerCubicFoot);
  vuCubicYards       := RegisterConversionType(cbVolume, SvuCubicYards      , SvuaCubicYards      , CubicMetersPerCubicYard);
  vuCubicMiles       := RegisterConversionType(cbVolume, SvuCubicMiles      , SvuaCubicMiles      , CubicMetersPerCubicMile);

  vuMilliLiters      := RegisterConversionType(cbVolume, SvuMilliLiters     , SvuaMilliLiters     , 1E-6);
  vuCentiLiters      := RegisterConversionType(cbVolume, SvuCentiLiters     , SvuaCentiLiters     , 1E-5);
  vuDeciLiters       := RegisterConversionType(cbVolume, SvuDeciLiters      , SvuaDeciLiters      , 1E-4);
  vuLiters           := RegisterConversionType(cbVolume, SvuLiters          , SvuaLiters          , 0.001);
  vuDekaLiters       := RegisterConversionType(cbVolume, SvuDekaLiters      , SvuaDekaLiters      , 0.01);
  vuHectoLiters      := RegisterConversionType(cbVolume, SvuHectoLiters     , SvuaHectoLiters     , 0.1);
  vuKiloLiters       := RegisterConversionType(cbVolume, SvuKiloLiters      , SvuaKiloLiters      , 1);

  vuAcreInches       := RegisterConversionType(cbVolume, SvuAcreInches      , SvuaAcreInches      , CubicMetersPerAcreInch);
  vuAcreFeet         := RegisterConversionType(cbVolume, SvuAcreFeet        , SvuaAcreFeet        , CubicMetersPerAcreFoot);
  vuBarrels          := RegisterConversionType(cbVolume, SvuBarrels         , SvuaBarrels         , 0.1192405);
  vuCordFeet         := RegisterConversionType(cbVolume, SvuCordFeet        , SvuaCordFeet        , CubicMetersPerCordFoot);
  vuCords            := RegisterConversionType(cbVolume, SvuCords           , SvuaCords           , CubicMetersPerCord);

  vuDecisteres       := RegisterConversionType(cbVolume, SvuDecisteres      , SvuaDecisteres      , 0.1);
  vuSteres           := RegisterConversionType(cbVolume, SvuSteres          , SvuaSteres          , 1);
  vuDekasteres       := RegisterConversionType(cbVolume, SvuDekasteres      , SvuaDekasteres      , 10);

  // American Fluid Units
  vuUSOunces         := RegisterConversionType(cbVolume, SvuUSOunces        , SvuaUSOunces        , CubicMetersPerUSOunce);
  vuUSGills          := RegisterConversionType(cbVolume, SvuUSGills         , SvuaUSGills         , CubicMetersPerUSGill);
  vuUSPints          := RegisterConversionType(cbVolume, SvuUSPints         , SvuaUSPints         , CubicMetersPerUSPint);
  vuUSQuarts         := RegisterConversionType(cbVolume, SvuUSQuarts        , SvuaUSQuarts        , CubicMetersPerUSQuart);
  vuUSGallons        := RegisterConversionType(cbVolume, SvuUSGallons       , SvuaUSGallons       , CubicMetersPerUSGallon);

  vuUSTeaspoons      := RegisterConversionType(cbVolume, SvuUSTeaspoons     , SvuaUSTeaspoons     , CubicMetersPerUSTeaspoon);
  vuUSTablespoons    := RegisterConversionType(cbVolume, SvuUSTablespoons   , SvuaUSTablespoons   , CubicMetersPerUSTablespoon);
  vuUSCups           := RegisterConversionType(cbVolume, SvuUSCups          , SvuaUSCups          , CubicMetersPerUSCup);

  // American Dry Units
  vuUSDryPints       := RegisterConversionType(cbVolume, SvuUSDryPints      , SvuaUSDryPints      , CubicMetersPerUSDryPint);
  vuUSDryQuarts      := RegisterConversionType(cbVolume, SvuUSDryQuarts     , SvuaUSDryQuarts     , CubicMetersPerUSDryQuart);
  vuUSDryGallons     := RegisterConversionType(cbVolume, SvuUSDryGallons    , SvuaUSDryGallons    , CubicMetersPerUSDryGallon);
  vuUSPecks          := RegisterConversionType(cbVolume, SvuUSPecks         , SvuaUSPecks         , CubicMetersPerUSPeck);
  vuUSBuckets        := RegisterConversionType(cbVolume, SvuUSBuckets       , SvuaUSBuckets       , CubicMetersPerUSBucket);
  vuUSBushels        := RegisterConversionType(cbVolume, SvuUSBushels       , SvuaUSBushels       , CubicMetersPerUSBushel);

  // English Imperial Fluid/Dry Units
  vuUKOunces         := RegisterConversionType(cbVolume, SvuUKOunces        , SvuaUKOunces        , CubicMetersPerUKOunce);
  vuUKGills          := RegisterConversionType(cbVolume, SvuUKGills         , SvuaUKGills         , CubicMetersPerUKGill);
  vuUKPints          := RegisterConversionType(cbVolume, SvuUKPints         , SvuaUKPints         , CubicMetersPerUKPint);
  vuUKQuarts         := RegisterConversionType(cbVolume, SvuUKQuarts        , SvuaUKQuarts        , CubicMetersPerUKQuart);
  vuUKPottles        := RegisterConversionType(cbVolume, SvuUKPottles       , SvuaUKPottles       , CubicMetersPerUKPottle);
  vuUKGallons        := RegisterConversionType(cbVolume, SvuUKGallons       , SvuaUKGallons       , CubicMetersPerUKGallon);
  vuUKPecks          := RegisterConversionType(cbVolume, SvuUKPecks         , SvuaUKPecks         , CubicMetersPerUKPeck);
  vuUKBuckets        := RegisterConversionType(cbVolume, SvuUKBuckets       , SvuaUKBuckets       , CubicMetersPerUKBucket);
  vuUKBushels        := RegisterConversionType(cbVolume, SvuUKBushels       , SvuaUKBushels       , CubicMetersPerUKBushel);

//==============================================================================
// Computer memory conversion unit
//==============================================================================
// Basic unit of measurement is gigabit

  cbInformation := RegisterConversionFamily(ScbInformation);

  cuBit       := RegisterConversionType(cbInformation, ScuBit,       ScuaBit,      1 / (1024 * 1024 * 1024));
  cuByte      := RegisterConversionType(cbInformation, ScuByte,      ScuaByte,     BitsPerByte / (1024 * 1024 * 1024));
  cuWord      := RegisterConversionType(cbInformation, ScuWord,      ScuaWord,     BitsPerWord / (1024 * 1024 * 1024));
  cuDWord     := RegisterConversionType(cbInformation, ScuDWord,     ScuaDWord,    BitsPerWord * 2 / (1024 * 1024 * 1024));
  cuQWord     := RegisterConversionType(cbInformation, ScuQWord,     ScuaQWord,    BitsPerWord * 4 / (1024 * 1024 * 1024));

  cuKilobit   := RegisterConversionType(cbInformation, ScuKilobit,   ScuaKilobit,  1 / (1024 * 1024));
  cuMegabit   := RegisterConversionType(cbInformation, ScuMegabit,   ScuaMegabit,  1 / 1024);
  cuGigabit   := RegisterConversionType(cbInformation, ScuGigabit,   ScuaGigabit,  1);
  cuTerabit   := RegisterConversionType(cbInformation, ScuTerabit,   ScuaKilobit,  1024);

  cuKilobyte  := RegisterConversionType(cbInformation, ScuKilobyte,  ScuaKilobyte, BitsPerByte / (1024 * 1024));
  cuMegabyte  := RegisterConversionType(cbInformation, ScuMegabyte,  ScuaMegabyte, BitsPerByte / 1024);
  cuGigabyte  := RegisterConversionType(cbInformation, ScuGigabyte,  ScuaGigabyte, BitsPerByte );
  cuTerabyte  := RegisterConversionType(cbInformation, ScuTerabyte,  ScuaTerabyte, BitsPerByte * 1024);


{******************************************************************************}
finalization
{******************************************************************************}

// Unregister all the conversion types we are responsible for
  UnregisterConversionFamily(cbArea);
  UnregisterConversionFamily(cbDistance);
  UnregisterConversionFamily(cbFuelConsumption);
  UnregisterConversionFamily(cbMass);
  UnregisterConversionFamily(cbPower);
  UnregisterConversionFamily(cbPrefixes);
  UnregisterConversionFamily(cbPressure);
  UnregisterConversionFamily(cbSpeed);
  UnregisterConversionFamily(cbTemperature);
  UnregisterConversionFamily(cbTime);
  UnregisterConversionFamily(cbVolume);

end{unit abfStdConvs}.
