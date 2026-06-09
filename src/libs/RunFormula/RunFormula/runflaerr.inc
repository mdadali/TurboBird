//*****************************************************
//  RunFormula Expression Scripting Engine
//  Error codes include file
//  Rev. 15.05.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: runflaerr.inc

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Err -cRev.2026.04.15:

type
  TRunFlaErrCode = (
    OK = 0,
    Unknown,
    Malloc,
    StackOver,
    Overflow,
    FuncExists,
    InvalidChar,
    InvalidFormat,
    UnexpectedEnd,
    IncompType_FI,
    IncompType_CI,
    IncompType_CF,
    IncompType_GI,
    IncompType_CG,
    IncompType_NI,
    IncompType_NF,
    IncompType_NC,
    IncompType_NG,
    InvalidStrVal,
    InvalidArrVal,
    DivZero,
    MissingBracket,
    IllegalBracket,
    UnknownFunc,
    VarExpected,
    VariantError,
    UnknownVar,
    ParamNumber,
    DefExists,
    MaxDepth,
    Unsupported,
    VarExists,
    OpExpected,
    NotOpExpected,
    AssignExpected,
    InvalidValue,
    InvalidRange,
    IllegalBreak,
    DimMismatch,
    IllegalDim,
    DimOver,
    DimExists,
    DimlessValue,
    OffsetUnit,
    UnknownUnit,
    UnDecodeDim
  );

  TRunFlaError = packed record
    Code : TRunFlaErrCode;
    Position : integer;
    Value : string;
  end;

