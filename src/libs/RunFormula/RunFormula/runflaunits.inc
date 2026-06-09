//*****************************************************
//  RunFormula Expression Scripting Engine
//  Dimension units include file
//  Rev. 20.05.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: runflaunits.inc

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Phy -cRev.2026.05.00:

const
  UMPF = $100;      // multiple prefix forbidden
  USPF = $200;      // submultiple prefix forbidden

  DimUnits : array of TUnitDesc = (

// Electromagnetics
    (Sig: $0000000000FD0102; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'W'),     // Power (Watt)
    (Sig: $0000000001010000; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'C'),     // Charge (Coulomb)
    (Sig: $000000000204FFFE; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'F'),     // Capacitance (Farad)
    (Sig: $00000000FEFE0102; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'H'),     // Inductance (Henry)
    (Sig: $00000000FEFD0102; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'Ohm'),   // Resistance (Ohm)
    (Sig: $000000000203FFFE; Scale: 1; Offset: 0; Flag: UOP;  Abbr: 'S'),     // Conductance (Siemens)
    (Sig: $00000000FFFE0102; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'Wb'),    // Magnetic Flux (Weber)
    (Sig: $00000000FFFE0100; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'T'),     // Magnetic Induction (Tesla)
    (Sig: $00000000FFFD0102; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'V'),     // Voltage (Volt)
    (Sig: $0000000000FF0000; Scale: 1; Offset: 0; Flag: USG;  Abbr: 'Hz'),    // Frequency (Hertz)
    (Sig: $0000000000FE0102; Scale: 3600; Offset: 0; Flag: UOP; Abbr: 'Wh'),  // Watt-hour
    (Sig: $0000000001010000; Scale: 3600; Offset: 0; Flag: UOP; Abbr: 'Ah'),  // Ampere-hour

// Mass volume area
    (Sig: $0000000000000100; Scale: 1E-3; Offset: 0; Flag: UOP; Abbr: 'g'),           // Metric Gram
    (Sig: $0000000000000100; Scale: 1E+3; Offset: 0; Flag: USPF or UOP; Abbr: 't'),   // Metric Ton
    (Sig: $0000000000000003; Scale: 1E-3; Offset: 0; Flag: UMPF or UOP; Abbr: 'l'),   // Liter
    (Sig: $0000000000000002; Scale: 1E+4; Offset: 0; Flag: UPF or UOP;  Abbr: 'ha'),  // Hectare

// Mechanics
    (Sig: $0000000000FE0101; Scale: 1; Offset: 0; Flag: 0; Abbr: 'N'),                // Force (Newton)
    (Sig: $0000000000FE0102; Scale: 1; Offset: 0; Flag: 0; Abbr: 'J'),                // Energy (Joule)
    (Sig: $0000000000FE01FF; Scale: 1; Offset: 0; Flag: 0; Abbr: 'Pa'),               // Pressure (Pascal)
    (Sig: $0000000000FE01FF; Scale: 101325;    Offset: 0; Flag: UPF or UOP; Abbr: 'atm'),  // Pressure (Standard Atmosphere)
    (Sig: $0000000000FE01FF; Scale: 1E+5;      Offset: 0; Flag: UOP; Abbr: 'bar'),    // Pressure (bar)
    (Sig: $0000000000FE01FF; Scale: 133.322387415; Offset: 0; Flag: UPF or UOP; Abbr: 'mmHg'),  // Pressure (mmHg)
    (Sig: $0000000000FE01FF; Scale: 9.80665;   Offset: 0; Flag: UPF or UOP; Abbr: 'mmH2O'),   // Pressure (mmH2O)
    (Sig: $0000000000FE0101; Scale: 9.80665;   Offset: 0; Flag: UPF or UOP; Abbr: 'kgf'),     // Kilogram-force
    (Sig: $0000000000FD0102; Scale: 745.699871582; Offset: 0; Flag: UPF or UOP; Abbr: 'hp'),  // Horsepower
    (Sig: $0000000000FF0000; Scale: 1/60;      Offset: 0; Flag: UPF or UOP; Abbr: 'rpm'),     // Revolutions per minute

// Optics
    (Sig: $0001000000000000; Scale: 1; Offset: 0; Flag: UOP; Abbr: 'lm'),     // Luminous Flux (Lumen)
    (Sig: $00010000000000FE; Scale: 1; Offset: 0; Flag: 0;   Abbr: 'lx'),     // Illuminance (Lux)

// Thermo
    (Sig: $0000000100000000; Scale: 1; Offset: 273.15; Flag: UOP;  Abbr: 'degC'),    // Temperature (Celsius)
    (Sig: $0000000000FE0102; Scale: 4.184; Offset: 0; Flag:  UOP;  Abbr: 'cal'),     // Calorie
    (Sig: $0000000000FE0102; Scale: 1055.05585262; Offset: 0; Flag: UPF or UOP;  Abbr: 'BTU'),  // British Thermal Unit
    (Sig: $0000000100000000; Scale: 5/9; Offset: 273.15-(32*5/9); Flag: UOP; Abbr: 'degF'),     // Temperature (Fahrenheit)

// Nuclear
    (Sig: $0000000000FE0102; Scale: 1.602176634E-19;   Offset: 0; Flag: UOP; Abbr: 'eV'),  // Electronvolt (eV)
    (Sig: $000000000101FF00; Scale: 2.58E-4;           Offset: 0; Flag: UOP; Abbr: 'R'),   // Roentgen
    (Sig: $0000000000FE0002; Scale: 1;                 Offset: 0; Flag: UOP; Abbr: 'Sv'),  // Equivalent Dose (Sievert)
    (Sig: $0000000000FF0000; Scale: 3.7E+10;           Offset: 0; Flag: UOP; Abbr: 'Ci'),  // Curie
    (Sig: $0000000000FF0000; Scale: 1;                 Offset: 0; Flag: UOP; Abbr: 'Bq'),  // Activity (Becquerel)
    (Sig: $0000000000FE0002; Scale: 1;                 Offset: 0; Flag: UOP; Abbr: 'Gy'),  // Absorbed Dose (Gray)
    (Sig: $0000000000000002; Scale: 1E-28;             Offset: 0; Flag: UOP; Abbr: 'b'),   // Cross-section (Barn)
    (Sig: $0000000000000100; Scale: 1.66053906892E-27; Offset: 0; Flag: UPF or UOP;  Abbr: 'u'),   // Unified atomic mass unit

// Speed
    (Sig: $0000000000FF0001; Scale: 1/3.6;     Offset: 0; Flag: UPF or UOP; Abbr: 'km/h'),  // Speed (km/h)
    (Sig: $0000000000FF0001; Scale: 0.44704;   Offset: 0; Flag: UPF or UOP; Abbr: 'mph'),   // Miles per hour
    (Sig: $0000000000FF0001; Scale: 1852/3600; Offset: 0; Flag: UPF or UOP; Abbr: 'kn'),    // Knots

// Time
    (Sig: $0000000000010000; Scale: 60;        Offset: 0; Flag: UPF or UOP; Abbr: 'min'),   // Minute
    (Sig: $0000000000010000; Scale: 3600;      Offset: 0; Flag: UPF or UOP; Abbr: 'h'),     // Hour
    (Sig: $0000000000010000; Scale: 86400;     Offset: 0; Flag: UPF or UOP; Abbr: 'd'),     // Day
    (Sig: $0000000000010000; Scale: 604800;    Offset: 0; Flag: UPF or UOP; Abbr: 'wk'),    // Week
    (Sig: $0000000000010000; Scale: 2629800;   Offset: 0; Flag: UPF or UOP; Abbr: 'mo'),    // Month
    (Sig: $0000000000010000; Scale: 31557600;  Offset: 0; Flag: UPF or UOP; Abbr: 'yr'),    // Year

// US units
    (Sig: $0000000000000001; Scale: 0.0254;     Offset: 0; Flag: UPF or UOP; Abbr: 'in'),   // Inch
    (Sig: $0000000000000001; Scale: 0.3048;     Offset: 0; Flag: UPF or UOP; Abbr: 'ft'),   // Foot
    (Sig: $0000000000000001; Scale: 0.9144;     Offset: 0; Flag: UPF or UOP; Abbr: 'yd'),   // Yard
    (Sig: $0000000000000001; Scale: 1609.344;   Offset: 0; Flag: UPF or UOP; Abbr: 'mi'),   // Mile
    (Sig: $0000000000000001; Scale: 1852;       Offset: 0; Flag: UPF or UOP; Abbr: 'nmi'),  // Nautical Mile
    (Sig: $0000000000000002; Scale: 4046.8564224; Offset: 0; Flag: UPF or UOP; Abbr: 'ac'),  // Acre
    (Sig: $0000000000000100; Scale: 0.45359237;   Offset: 0; Flag: UPF or UOP; Abbr: 'lb'),  // Pound
    (Sig: $0000000000000100; Scale: 0.45359237/16; Offset: 0; Flag: UPF or UOP; Abbr: 'oz'),   // Ounce
    (Sig: $0000000000000100; Scale: 907.18474;     Offset: 0; Flag: UPF or UOP; Abbr: 'ton'),  // Short Ton
    (Sig: $0000000000000100; Scale: 6.35029318;    Offset: 0; Flag: UPF or UOP; Abbr: 'st'),   // Stone
    (Sig: $0000000000000003; Scale: 0.003785411784;   Offset: 0; Flag: UPF or UOP; Abbr: 'gal'),  // Gallon
    (Sig: $0000000000000003; Scale: 0.003785411784/4; Offset: 0; Flag: UPF or UOP; Abbr: 'qt'),   // Quart
    (Sig: $0000000000000003; Scale: 0.003785411784/8; Offset: 0; Flag: UPF or UOP; Abbr: 'pt'),   // Pint
    (Sig: $0000000000000003; Scale: 0.003785411784/128;  Offset: 0; Flag: UPF or UOP; Abbr: 'fl oz'),  // Fluid Ounce

// SI units: J N Th I T M L
    (Sig: $0000000001000000; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'A'),      // Current (Ampere)
    (Sig: $0000000100000000; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'K'),      // Temperature (Kelvin)
    (Sig: $0000000000000100; Scale: 1; Offset: 0; Flag: UPF;  Abbr: 'kg'),     // Mass (Kilogram)
    (Sig: $0000000000000001; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'm'),      // Length (Meter)
    (Sig: $0000000000010000; Scale: 1; Offset: 0; Flag: UMPF; Abbr: 's'),      // Time (Second)
    (Sig: $0000010000000000; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'mol'),    // Amount of Substance (Mole)
    (Sig: $0001000000000000; Scale: 1; Offset: 0; Flag: 0;    Abbr: 'cd')      // Luminous Intensity (Candela)
  );

  UnitPrefixes : array of TPrefAbbr = (
    (Scale: 1E+24; Flag: UMPF; Abbr: 'Y'),    // yotta
    (Scale: 1E+21; Flag: UMPF; Abbr: 'Z'),    // zetta
    (Scale: 1E+18; Flag: UMPF; Abbr: 'E'),    // exa
    (Scale: 1E+15; Flag: UMPF; Abbr: 'P'),    // peta
    (Scale: 1E+12; Flag: UMPF; Abbr: 'T'),    // tera
    (Scale: 1E+9;  Flag: UMPF; Abbr: 'G'),    // giga
    (Scale: 1E+6;  Flag: UMPF; Abbr: 'M'),    // mega
    (Scale: 1E+3;  Flag: UMPF; Abbr: 'k'),    // kilo
    (Scale: 1E+2;  Flag: UOP;  Abbr: 'h'),    // hecto
    (Scale: 1E+1;  Flag: UOP;  Abbr: 'da'),   // deca
    (Scale: 1E-1;  Flag: UOP;  Abbr: 'd'),    // deci
    (Scale: 1E-2;  Flag: UOP;  Abbr: 'c'),    // centi
    (Scale: 1E-3;  Flag: USPF; Abbr: 'm'),    // milli
    (Scale: 1E-6;  Flag: USPF; Abbr: 'u'),    // micro
    (Scale: 1E-9;  Flag: USPF; Abbr: 'n'),    // nano
    (Scale: 1E-12; Flag: USPF; Abbr: 'p'),    // pico
    (Scale: 1E-15; Flag: USPF; Abbr: 'f'),    // femto
    (Scale: 1E-18; Flag: USPF; Abbr: 'a'),    // atto
    (Scale: 1E-21; Flag: USPF; Abbr: 'z'),    // zepto
    (Scale: 1E-24; Flag: USPF; Abbr: 'y')     // yocto
  );

