{*******************************************************************************

  ABF Visual Components Library. Standard conversion localization consts.

  Based on Borland and Project JEDI routines
  http://www.borland.com
  http://www.delphi-jedi.org

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfStdConvsConsts; { English }

{$I abf.inc}

interface

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}

//==============================================================================
// Area conversion consts
//==============================================================================

  ScbArea = 'Area';

  SauSquareMillimeters = 'Square Millimeters'; SauaSquareMillimeters = 'mm'#0178;
  SauSquareCentimeters = 'Square Centimeters'; SauaSquareCentimeters = 'cm'#0178;
  SauSquareDecimeters  = 'Square Decimeters';  SauaSquareDecimeters  = 'dm'#0178;
  SauSquareMeters      = 'Square Meters';      SauaSquareMeters      = 'm'#0178;
  SauSquareDekameters  = 'Square Dekameters';  SauaSquareDekameters  = 'dam'#0178;
  SauSquareHectometers = 'Square Hectometers'; SauaSquareHectometers = 'hm'#0178;
  SauSquareKilometers  = 'Square Kilometers';  SauaSquareKilometers  = 'km'#0178;
  SauSquareInches      = 'Square Inches';      SauaSquareInches      = 'In'#0178;
  SauSquareFeet        = 'Square Feet';        SauaSquareFeet        = 'Ft'#0178;
  SauSquareYards       = 'Square Yards';       SauaSquareYards       = 'Yd'#0178;
  SauSquareMiles       = 'Square Miles';       SauaSquareMiles       = 'Mi'#0178;
  SauAcres             = 'Acres';              SauaAcres             = '';
  SauCentares          = 'Centares';           SauaCentares          = 'ca';
  SauAres              = 'Ares';               SauaAres              = 'a';
  SauHectares          = 'Hectares';           SauaHectares          = 'ha';
  SauBarns             = 'Barns';              SauaBarns             = 'b';
  SauSquareRods        = 'Square Rods';        SauaSquareRods        = '';


//==============================================================================
// Distance conversion consts
//==============================================================================

  ScbDistance = 'Distance';

  SduMicromicrons      = 'Micromicrons';       SduaMicromicrons      = #0181#0181;
  SduAngstroms         = 'Angstroms';          SduaAngstroms         = #0197;
  SduMillimicrons      = 'Millimicrons';       SduaMillimicrons      = 'm'#0181;
  SduMicrons           = 'Microns';            SduaMicrons           = #0181;
  SduMillimeters       = 'Millimeters';        SduaMillimeters       = 'mm';
  SduCentimeters       = 'Centimeters';        SduaCentimeters       = 'cm';
  SduDecimeters        = 'Decimeters';         SduaDecimeters        = 'dm';
  SduMeters            = 'Meters';             SduaMeters            = 'm';
  SduDekameters        = 'Dekameters';         SduaDekameters        = 'dam';
  SduHectometers       = 'Hectometers';        SduaHectometers       = 'hm';
  SduKilometers        = 'Kilometers';         SduaKilometers        = 'km';
  SduMegameters        = 'Megameters';         SduaMegameters        = 'Mm';
  SduGigameters        = 'Gigameters';         SduaGigameters        = 'Gm';

  SduMils              = 'Mils';               SduaMils              = '';
  SduLines             = 'Lines';              SduaLines              = '';
  SduInches            = 'Inches';             SduaInches            = 'In';
  SduSpans             = 'Spans';              SduaSpans             = '';
  SduFeet              = 'Feet';               SduaFeet              = 'Ft';
  SduYards             = 'Yards';              SduaYards             = 'Yd';
  SduFathoms           = 'Fathoms';            SduaFathoms           = '';
  SduFurlongs          = 'Furlongs';           SduaFurlongs          = 'Fur';
  SduRods              = 'Rods';               SduaRods              = '';
  SduChains            = 'Chains';             SduaChains            = '';
  SduMiles             = 'Miles';              SduaMiles             = 'Mi';
  SduLeagues           = 'Leagues';            SduaLeagues           = 'Lg';

  SduCables            = 'Cables';             SduaCables            = '';
  SduUSCables          = 'US Cables';          SduaUSCables          = '';
  SduNauticalMiles     = 'Nautical Miles';     SduaNauticalMiles     = '';
  SduNauticalLeagues   = 'Nautical Leagues';   SduaNauticalLeagues   = '';

  SduAstronomicalUnits = 'Astronomical Units'; SduaAstronomicalUnits = 'AU';
  SduLightYears        = 'Light Years';        SduaLightYears        = '';
  SduLightMinutes      = 'Light Minutes';      SduaLightMinutes      = '';
  SduLightSeconds      = 'Light Seconds';      SduaLightSeconds      = '';
  SduParsecs           = 'Parsecs';            SduaParsecs           = '';

  SduCubits            = 'Cubits';             SduaCubits            = '';
  SduHands             = 'Hands';              SduaHands             = '';
  SduPaces             = 'Paces';              SduaPaces             = '';
  SduLinks             = 'Links';              SduaLinks             = '';

  SduPicas             = 'Picas';              SduaPicas             = '';
  SduPoints            = 'Points';             SduaPoints            = '';

  SduXUnits            = 'X-Units';            SduaXUnits            = 'X';


//==============================================================================
// Fuel Consumption conversion units
//==============================================================================
// Basic unit of measurement is "litres per 100 km"

  ScbFuelConsumption = 'Fuel Consumption';

  SfuLitersPer100KiloMeters    = 'Liters per 100 Kilometers';     SfuaLitersPer100KiloMeters    = 'L/100 km';
  SfuLitersPer100Miles         = 'Liters per 100 Miles';          SfuaLitersPer100Miles         = 'L/100 M';
  SfuUSGallonsPer100KiloMeters = 'US Gallons per 100 Kilometers'; SfuaUSGallonsPer100KiloMeters = '';
  SfuUSGallonsPer100Miles      = 'US Gallons per 100 Miles';      SfuaUSGallonsPer100Miles      = '';
  SfuUKGallonsPer100KiloMeters = 'UK Gallons per 100 Kilometers'; SfuaUKGallonsPer100KiloMeters = '';
  SfuUKGallonsPer100Miles      = 'UK Gallons per 100 Miles';      SfuaUKGallonsPer100Miles      = '';

  SfuKiloMetersPerLiter        = 'Kilometers per Liter';          SfuaKiloMetersPerLiter        = 'km/L';
  SfuMilesPerLiter             = 'Miles per Liter';               SfuaMilesPerLiter             = 'Mi/L';
  SfuKiloMetersPerUSGallon     = 'Kilometers per US Gallon';      SfuaKiloMetersPerUSGallon     = '';
  SfuMilesPerUSGallon          = 'Miles per US Gallon';           SfuaMilesPerUSGallon          = '';
  SfuKiloMetersPerUKGallon     = 'Kilometers per UK Gallon';      SfuaKiloMetersPerUKGallon     = '';
  SfuMilesPerUKGallon          = 'Miles per UK Gallon';           SfuaMilesPerUKGallon          = '';


//==============================================================================
// Mass conversion consts
//==============================================================================

  ScbMass = 'Mass';

  SmuNanograms      = 'Nanograms';       SmuaNanograms      = 'ngr';
  SmuMicrograms     = 'Micrograms';      SmuaMicrograms     = #0181'g';
  SmuMilligrams     = 'Milligrams';      SmuaMilligrams     = 'mg';
  SmuCentigrams     = 'Centigrams';      SmuaCentigrams     = 'cg';
  SmuDecigrams      = 'Decigrams';       SmuaDecigrams      = 'dg';
  SmuGrams          = 'Grams';           SmuaGrams          = 'g';
  SmuDekagrams      = 'Dekagrams';       SmuaDekagrams      = 'dag';
  SmuHectograms     = 'Hectograms';      SmuaHectograms     = 'hg';
  SmuKilograms      = 'Kilograms';       SmuaKilograms      = 'kg';
  SmuMetricCentners = 'Metric Centners'; SmuaMetricCentners = 'q';
  SmuMetricTons     = 'Metric Tons';     SmuaMetricTons     = 't';
  SmuMetricCarats   = 'Metric Carats';   SmuaMetricCarats   = '';

  SmuGrains         = 'Grains';          SmuaGrains         = '';
  SmuDrams          = 'Drams';           SmuaDrams          = '';
  SmuOunces         = 'Ounces';          SmuaOunces         = 'oz';
  SmuPounds         = 'Pounds';          SmuaPounds         = 'lb';
  SmuStones         = 'Stones';          SmuaStones         = '';
  SmuTons           = 'Tons';            SmuaTons           = '';
  SmuLongTons       = 'Long Tons';       SmuaLongTons       = '';

  SmuUSCentners     = 'US Centners';     SmuaUSCentners     = 'US cwt';
  SmuUKCentners     = 'UK Centners';     SmuaUKCentners     = 'UK cwt';

  SmuPennyweights   = 'Pennyweights';    SmuaPennyweights   = 'dwt';
  SmuScrupules      = 'Scrupules';       SmuaScrupules      = '';
  SmuTroyDrams      = 'Troy Drams';      SmuaTroyDrams      = '';
  SmuTroyOunces     = 'Troy Ounces';     SmuaTroyOunces     = '';
  SmuTroyPounds     = 'Troy Pounds';     SmuaTroyPounds     = '';


//==============================================================================
// Power conversion consts
//==============================================================================
// Basic unit of measurement is Watts

  ScbPower = 'Power';

  SpuWatts                     = 'Watts';                         SpuaWatts                     = 'W';
  SpuMiliWatts                 = 'Milliwatts';                    SpuaMiliWatts                 = 'mW';
  SpuKiloWatts                 = 'Kilowatts';                     SpuaKiloWatts                 = 'kW';
  SpuMegaWatts                 = 'Megawatts';                     SpuaMegaWatts                 = 'MW';
  SpuGigaWatts                 = 'Gigawatts';                     SpuaGigaWatts                 = 'GW';
  SpuTeraWatts                 = 'Terawatts';                     SpuaTeraWatts                 = 'TW';
  SpuJoulesPerSec              = 'Joules per Sec';                SpuaJoulesPerSec              = 'J/s';
  SpuKiloJoulesPerMin          = 'Kilojoules per Min';            SpuaKiloJoulesPerMin          = 'kJ/min';
  SpuMegaJoulesPerHour         = 'Megajoules per Hour';           SpuaMegaJoulesPerHour         = 'MJ/hour';
  SpuKilogramForceMetersPerSec = 'Kilogram-force meters per Sec'; SpuaKilogramForceMetersPerSec = 'kgf'#0183'm/s';
  SpuFootPoundForcePerSec      = 'Foot pound-force per Sec';      SpuaFootPoundForcePerSec      = 'foot'#0183'lbf/s ';
  SpuCaloriesPerSec            = 'Calories per Sec';              SpuaCaloriesPerSec            = 'cal/s';
  SpuCaloriesPerMin            = 'Calories per Min';              SpuaCaloriesPerMin            = 'cal/min';
  SpuKiloCaloriesPerSec        = 'Kilocalories per Sec';          SpuaKiloCaloriesPerSec        = 'kcal/s';
  SpuKiloCaloriesPerMin        = 'Kilocalories per Min';          SpuaKiloCaloriesPerMin        = 'kcal/min';
  SpuKiloCaloriesPerHour       = 'Kilocalories per Hour';         SpuaKiloCaloriesPerHour       = 'kcal/hour';
  SpuHorsePowerElectric        = 'Horsepower (Electric)';         SpuaHorsePowerElectric        = '';
  SpuHorsePowerMetric          = 'Horsepower (Metric)';           SpuaHorsePowerMetric          = '';
  SpuBTUPerSec                 = 'British thermal unit per Sec';  SpuaBTUPerSec                 = 'Btu/s';
  SpuBTUPerMin                 = 'British thermal unit per Min';  SpuaBTUPerMin                 = 'Btu/min';
  SpuBTUPerHour                = 'British thermal unit per Hour'; SpuaBTUPerHour                = 'Btu/hour';
  SpuThermsPerHour             = 'Therms per Hour';               SpuaThermsPerHour             = '';


//==============================================================================
// Prefixes conversion consts
//==============================================================================

  ScbPrefixes = 'Prefixes';

  SpuYotta = 'Yotta'; SpuaYotta = 'Y';
  SpuZetta = 'Zetta'; SpuaZetta = 'Z';
  SpuExa   = 'Exa';   SpuaExa   = 'E';
  SpuPeta  = 'Peta';  SpuaPeta  = 'P';
  SpuTera  = 'Tera';  SpuaTera  = 'T';
  SpuGiga  = 'Giga';  SpuaGiga  = 'G';
  SpuMega  = 'Mega';  SpuaMega  = 'M';
  SpuKilo  = 'Kilo';  SpuaKilo  = 'k';
  SpuHecto = 'Hecto'; SpuaHecto = 'h';
  SpuDeka  = 'Deka';  SpuaDeka  = 'da';

  SpuNone  = '(none)';      SpuaNone  = '';

  SpuDeci  = 'Deci';  SpuaDeci  = 'd';
  SpuCenti = 'Centi'; SpuaCenti = 'c';
  SpuMilli = 'Milli'; SpuaMilli = 'm';
  SpuMicro = 'Micro'; SpuaMicro = #0181;
  SpuNano  = 'Nano';  SpuaNano  = 'n';
  SpuPico  = 'Pico';  SpuaPico  = 'p';
  SpuFemto = 'Femto'; SpuaFemto = 'f';
  SpuAtto  = 'Atto';  SpuaAtto  = 'a';
  SpuZepto = 'Zepto'; SpuaZepto = 'z';
  SpuYocto = 'Yocto'; SpuaYocto = 'y';


//==============================================================================
// Pressure conversion units
//==============================================================================
// Basic unit of measurement is Pascal

  ScbPressure = 'Pressure';

  SpuPascals                      = 'Pascals';                         SpuaPascals                      = 'Pa';
  SpuKiloPascals                  = 'Kilopascals';                     SpuaKiloPascals                  = 'kPa';
  SpuMegaPascals                  = 'Megapascals';                     SpuaMegaPascals                  = 'MPa';
  SpuNewtonsPerSquareMeter        = 'Newtons per Square Meter';        SpuaNewtonsPerSquareMeter        = 'N/m'#0178;
  SpuNewtonsPerSquareCentiMeter   = 'Newtons per Square Centimeter';   SpuaNewtonsPerSquareCentiMeter   = 'N/cm'#0178;
  SpuKiloNewtonsPerSquareMeter    = 'Kilonewtons per Square Meter';    SpuaKiloNewtonsPerSquareMeter    = 'kN/m'#0178;
  SpuKilogramsPerSquareMeter      = 'Kilograms per Square Meter';      SpuaKilogramsPerSquareMeter      = 'kg/m'#0178;
  SpuKilogramsPerSquareCentiMeter = 'Kilograms per Square Centimeter'; SpuaKilogramsPerSquareCentiMeter = 'kg/cm'#0178;
  SpuMetricTonsPerSquareMeter     = 'MetricTons per Square Meter';     SpuaMetricTonsPerSquareMeter     = 't/m'#0178;
  SpuPoundsPerSquareInch          = 'Pounds per Square Inch';          SpuaPoundsPerSquareInch          = 'lb/In'#0178;
  SpuPoundsPerSquareFoot          = 'Pounds per Square Foot';          SpuaPoundsPerSquareFoot          = 'lb/Ft'#0178;
  SpuUSTonsPerSquareFoot          = 'US Tons per Square Foot';         SpuaUSTonsPerSquareFoot          = 't(US)/Ft'#0178;
  SpuUKTonsPerSquareFoot          = 'UK Tons per Square Foot';         SpuaUKTonsPerSquareFoot          = 't(UK)/Ft'#0178;
  SpuAtmospheres                  = 'Atmospheres';                     SpuaAtmospheres                  = 'At';
  SpuBars                         = 'Bars';                            SpuaBars                         = 'Bar';
  SpuMiliBars                     = 'Milibars';                        SpuaMiliBars                     = 'mBar';
  SpuTorrs                        = 'Torrs';                           SpuaTorrs                        = 'Torr';
  SpuMetersOfWater                = 'Meters of Water';                 SpuaMetersOfWater                = 'm H2O';
  SpuMiliMetersOfWater            = 'Milimeters of Water';             SpuaMiliMetersOfWater            = 'mm H2O';
  SpuMiliMetersOfMercury          = 'Milimeters of Mercury';           SpuaMiliMetersOfMercury          = 'mm Hg';
  SpuInchesOfWater                = 'Inches of Water';                 SpuaInchesOfWater                = 'In H2O';
  SpuFeetOfWater                  = 'Feet of Water';                   SpuaFeetOfWater                  = 'Ft H2O';
  SpuInchesOfMercury              = 'Inches of Mercury';               SpuaInchesOfMercury              = 'In Hg';


//==============================================================================
// Speed conversion units
//==============================================================================
// Basic unit of measurement is Meter/Second

  ScbSpeed = 'Speed';

  SsuMetersPerSec      = 'Meters per Second';      SsuaMetersPerSec      = 'm/s';
  SsuMetersPerMin      = 'Meters per Minute';      SsuaMetersPerMin      = 'm/min';
  SsuMetersPerHour     = 'Meters per Hour';        SsuaMetersPerHour     = 'm/h';
  SsuMetersPerDay      = 'Meters per Day';         SsuaMetersPerDay      = 'm/d';
  SsuKilometersPerSec  = 'Kilometers per Second';  SsuaKilometersPerSec  = 'km/s';
  SsuKilometersPerMin  = 'Kilometers per Minute';  SsuaKilometersPerMin  = 'km/min';
  SsuKilometersPerHour = 'Kilometers per Hour';    SsuaKilometersPerHour = 'km/h';
  SsuKilometersPerDay  = 'Kilometers per Day';     SsuaKilometersPerDay  = 'km/d';
  SsuMilimetersPerSec  = 'Milimeters per Second';  SsuaMilimetersPerSec  = 'mm/s';
  SsuCentimetersPerSec = 'Centimeters per Second'; SsuaCentimetersPerSec = 'cm/s';
  SsuInchesPerSec      = 'Inches per Second';      SsuaInchesPerSec      = 'In/s';
  SsuInchesPerMin      = 'Inches per Minute';      SsuaInchesPerMin      = 'In/min';
  SsuInchesPerHour     = 'Inches per Hour';        SsuaInchesPerHour     = 'In/h';
  SsuInchesPerDay      = 'Inches per Day';         SsuaInchesPerDay      = 'In/d';
  SsuFeetPerSec        = 'Feet per Second';        SsuaFeetPerSec        = 'Ft/s';
  SsuFeetPerMin        = 'Feet per Minute';        SsuaFeetPerMin        = 'Ft/min';
  SsuFeetPerHour       = 'Feet per Hour';          SsuaFeetPerHour       = 'Ft/h';
  SsuFeetPerDay        = 'Feet per Day';           SsuaFeetPerDay        = 'Ft/d';
  SsuFurlongsPerSec    = 'Furlongs per Second';    SsuaFurlongsPerSec    = 'Fur/s';
  SsuFurlongsPerMin    = 'Furlongs per Minute';    SsuaFurlongsPerMin    = 'Fur/min';
  SsuFurlongsPerHour   = 'Furlongs per Hour';      SsuaFurlongsPerHour   = 'Fur/h';
  SsuFurlongsPerDay    = 'Furlongs per Day';       SsuaFurlongsPerDay    = 'Fur/d';
  SsuMilesPerSec       = 'Miles per Second';       SsuaMilesPerSec       = 'Mi/s';
  SsuMilesPerMin       = 'Miles per Minute';       SsuaMilesPerMin       = 'Mi/min';
  SsuMilesPerHour      = 'Miles per Hour';         SsuaMilesPerHour      = 'Mph';
  SsuMilesPerDay       = 'Miles per Day';          SsuaMilesPerDay       = 'Mi/d';
  SsuLeaguesPerSec     = 'Ligues per Second';      SsuaLeaguesPerSec     = 'Lg/s';
  SsuLeaguesPerMin     = 'Ligues per Minute';      SsuaLeaguesPerMin     = 'Lg/min';
  SsuLeaguesPerHour    = 'Ligues per Hour';        SsuaLeaguesPerHour    = 'Lg/h';
  SsuLeaguesPerDay     = 'Ligues per Day';         SsuaLeaguesPerDay     = 'Lg/d';

  SsuKnots             = 'Knots';                  SsuaKnots             = '';
  SsuMach              = 'Mach (dry air, 273 Kelvin)'; SsuaMach          = '';


//==============================================================================
// Temperature conversion consts
//==============================================================================

  ScbTemperature = 'Temperature';

  StuCelsius    = 'Celsius';    StuaCelsius    = #0176'C';
  StuFahrenheit = 'Fahrenheit'; StuaFahrenheit = #0176'F';
  StuKelvin     = 'Kelvin';     StuaKelvin     = 'K';
  StuRankine    = 'Rankine';    StuaRankine    = '';
  StuReaumur    = 'Reaumur';    StuaReaumur    = '';


//==============================================================================
// Time conversion units
//==============================================================================

  ScbTime = 'Time';

  StuMilliSeconds       = 'Milliseconds';         StuaMilliSeconds       = 'ms';
  StuSeconds            = 'Seconds';              StuaSeconds            = 's';
  StuMinutes            = 'Minutes';              StuaMinutes            = 'min';
  StuHours              = 'Hours';                StuaHours              = 'h';
  StuDays               = 'Days';                 StuaDays               = 'd';
  StuWeeks              = 'Weeks';                StuaWeeks              = 'w';
  StuFortnights         = 'Fortnights';           StuaFortnights         = '';
  StuMonths             = 'Months';               StuaMonths             = 'm';
  StuYears              = 'Years';                StuaYears              = 'y';
  StuDecades            = 'Decades';              StuaDecades            = '';
  StuCenturies          = 'Centuries';            StuaCenturies          = '';
  StuMillennia          = 'Millennia';            StuaMillennia          = '';
  StuDateTime           = 'DateTime';             StuaDateTime           = '';
  StuJulianDate         = 'Julian Date';          StuaJulianDate         = '';
  StuModifiedJulianDate = 'Modified Julian Date'; StuaModifiedJulianDate = '';


//==============================================================================
// Volume conversion consts
//==============================================================================

  ScbVolume = 'Volume';

  SvuCubicMillimeters = 'Cubic Millimeters'; SvuaCubicMillimeters = 'mm'#0179;
  SvuCubicCentimeters = 'Cubic Centimeters'; SvuaCubicCentimeters = 'cm'#0179;
  SvuCubicDecimeters  = 'Cubic Decimeters';  SvuaCubicDecimeters  = 'dm'#0179;
  SvuCubicMeters      = 'Cubic Meters';      SvuaCubicMeters      = 'm'#0179;
  SvuCubicDekameters  = 'Cubic Dekameters';  SvuaCubicDekameters  = 'dam'#0179;
  SvuCubicHectometers = 'Cubic Hectometers'; SvuaCubicHectometers = 'hm'#0179;
  SvuCubicKilometers  = 'Cubic Kilometers';  SvuaCubicKilometers  = 'km'#0179;

  SvuCubicInches      = 'Cubic Inches';      SvuaCubicInches      = 'In'#0179;
  SvuCubicFeet        = 'Cubic Feet';        SvuaCubicFeet        = 'Ft'#0179;
  SvuCubicYards       = 'Cubic Yards';       SvuaCubicYards       = 'Yd'#0179;
  SvuCubicMiles       = 'Cubic Miles';       SvuaCubicMiles       = 'Mi'#0179;

  SvuMilliLiters      = 'Milliliters';       SvuaMilliLiters      = 'mL';
  SvuCentiLiters      = 'Centiliters';       SvuaCentiLiters      = 'cL';
  SvuDeciLiters       = 'Deciliters';        SvuaDeciLiters       = 'dL';
  SvuLiters           = 'Liters';            SvuaLiters           = 'L';
  SvuDekaLiters       = 'Dekaliters';        SvuaDekaLiters       = 'daL';
  SvuHectoLiters      = 'Hectoliters';       SvuaHectoLiters      = 'hL';
  SvuKiloLiters       = 'Kiloliters';        SvuaKiloLiters       = 'kL';

  SvuAcreInches       = 'Acre Inches';       SvuaAcreInches       = '';
  SvuAcreFeet         = 'Acre Feet';         SvuaAcreFeet         = '';
  SvuBarrels          = 'Barrels';           SvuaBarrels            = '';
  SvuCordFeet         = 'Cord Feet';         SvuaCordFeet         = '';
  SvuCords            = 'Cords';             SvuaCords            = '';

  SvuDecisteres       = 'Decisteres';        SvuaDecisteres       = '';
  SvuSteres           = 'Steres';            SvuaSteres           = '';
  SvuDekasteres       = 'Dekasteres';        SvuaDekasteres       = '';

  // American Fluid Units
  SvuUSOunces         = 'US Ounces';        SvuaUSOunces         = 'oz';
  SvuUSGills          = 'US Gills';         SvuaUSGills          = 'gi';
  SvuUSPints          = 'US Pints';         SvuaUSPints          = 'pt';
  SvuUSQuarts         = 'US Quarts';        SvuaUSQuarts         = 'qt';
  SvuUSGallons        = 'US Gallons';       SvuaUSGallons        = 'gal';

  SvuUSTeaspoons      = 'US Teaspoons';     SvuaUSTeaspoons      = '';
  SvuUSTablespoons    = 'US Tablespoons';   SvuaUSTablespoons    = '';
  SvuUSCups           = 'US Cups';          SvuaUSCups           = '';

  // American Dry Units
  SvuUSDryPints       = 'US Dry Pints';     SvuaUSDryPints       = 'pt';
  SvuUSDryQuarts      = 'US Dry Quarts';    SvuaUSDryQuarts      = 'qt';
  SvuUSDryGallons     = 'US Dry Gallons';   SvuaUSDryGallons     = 'gal';
  SvuUSPecks          = 'US Pecks';         SvuaUSPecks          = 'pk';
  SvuUSBuckets        = 'US Buckets';       SvuaUSBuckets        = 'bt';
  SvuUSBushels        = 'US Bushels';       SvuaUSBushels        = 'bu';

  // English Imperial Fluid/Dry Units
  SvuUKOunces         = 'UK Ounces';        SvuaUKOunces         = '';
  SvuUKGills          = 'UK Gill';          SvuaUKGills          = '';
  SvuUKPints          = 'UK Pints';         SvuaUKPints          = '';
  SvuUKQuarts         = 'UK Quarts';        SvuaUKQuarts         = '';
  SvuUKPottles        = 'UK Pottle';        SvuaUKPottles        = '';
  SvuUKGallons        = 'UK Gallons';       SvuaUKGallons        = '';
  SvuUKPecks          = 'UK Pecks';         SvuaUKPecks          = '';
  SvuUKBuckets        = 'UK Buckets';       SvuaUKBuckets        = '';
  SvuUKBushels        = 'UK Bushels';       SvuaUKBushels        = '';

//==============================================================================
// Information conversion units
//==============================================================================

  ScbInformation = 'Information';

  ScuBit       = 'Bits';       ScuaBit      = 'b';
  ScuKilobit   = 'Kilobits';   ScuaKilobit  = 'Kb';
  ScuMegabit   = 'Megabits';   ScuaMegabit  = 'Mb';
  ScuGigabit   = 'Gigabits';   ScuaGigabit  = 'Gb';
  ScuTerabit   = 'Terabits';   ScuaTerabit  = 'Tb';

  Scubyte      = 'Bytes';      ScuaByte     = 'B';
  ScuKilobyte  = 'Kilobytes';  ScuaKilobyte = 'KB';
  ScuMegabyte  = 'Megabytes';  ScuaMegabyte = 'MB';
  ScuGigabyte  = 'Gigabytes';  ScuaGigabyte = 'GB';
  ScuTerabyte  = 'Terabytes';  ScuaTerabyte = 'TB';

  ScuWord      = 'Words';      ScuaWord     = 'W';
  ScuDWord     = 'Dwords';     ScuaDWord    = 'DW';
  ScuQWord     = 'Qwords';     ScuaQWord    = 'QW';

{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfStdConvsConsts}.
