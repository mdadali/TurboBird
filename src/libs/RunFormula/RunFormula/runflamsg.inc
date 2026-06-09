//*****************************************************
//  RunFormula Expression Scripting Engine
//  Error messages include file
//  Rev. 15.05.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: runflamsg.inc

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Msg -cRev.2026.04.15:

const
  RunFlaErrorMsg : array[TRunFlaErrCode] of record
      ErrCode : TRunFlaErrCode;
      ErrMsg  : string;
    end = (
      (ErrCode : OK; ErrMsg : 'OK'),
      (ErrCode : Unknown; ErrMsg : 'Unknown Error'),
      (ErrCode : Malloc; ErrMsg : 'Memory Allocation Error'),
      (ErrCode : StackOver; ErrMsg : 'Stack overflow'),
      (ErrCode : Overflow; ErrMsg : 'Float Overflow Error'),
      (ErrCode : FuncExists; ErrMsg : 'Function already exists'),
      (ErrCode : InvalidChar; ErrMsg : 'Invalid character'),
      (ErrCode : InvalidFormat; ErrMsg : 'Invalid format'),
      (ErrCode : UnexpectedEnd; ErrMsg : 'Unexpected End'),
      (ErrCode : IncompType_FI; ErrMsg : 'Incompatible operand type: got Real but expected Integer'),
      (ErrCode : IncompType_CI; ErrMsg : 'Incompatible operand type: got Complex but expected Integer'),
      (ErrCode : IncompType_CF; ErrMsg : 'Incompatible operand type: got Complex but expected Real'),
      (ErrCode : IncompType_GI; ErrMsg : 'Incompatible operand type: got Interval but expected Integer'),
      (ErrCode : IncompType_CG; ErrMsg : 'Incompatible operand type: got Complex but expected Interval'),
      (ErrCode : IncompType_NI; ErrMsg : 'Incompatible operand type: got None but expected Integer'),
      (ErrCode : IncompType_NF; ErrMsg : 'Incompatible operand type: got None but expected Real'),
      (ErrCode : IncompType_NC; ErrMsg : 'Incompatible operand type: got None but expected Complex'),
      (ErrCode : IncompType_NG; ErrMsg : 'Incompatible operand type: got None but expected Interval'),
      (ErrCode : InvalidStrVal; ErrMsg : 'Invalid string operand'),
      (ErrCode : InvalidArrVal; ErrMsg : 'Invalid array operand'),
      (ErrCode : DivZero; ErrMsg : 'Division by Zero'),
      (ErrCode : MissingBracket; ErrMsg : 'One or more closing brackets or block terminators are missing'),
      (ErrCode : IllegalBracket; ErrMsg : 'Mismatched bracket or block terminator'),
      (ErrCode : UnknownFunc; ErrMsg : 'Function is not found'),
      (ErrCode : VarExpected; ErrMsg : 'Variable expected'),
      (ErrCode : VariantError; ErrMsg : 'Variant Conversion Error'),
      (ErrCode : UnknownVar; ErrMsg : 'Variable is not found'),
      (ErrCode : ParamNumber; ErrMsg : 'Incorrect number of parameters'),
      (ErrCode : DefExists; ErrMsg : 'Define already exists'),
      (ErrCode : MaxDepth; ErrMsg : 'Maximum depth has been exceeded'),
      (ErrCode : Unsupported; ErrMsg : 'Unsupported Feature'),
      (ErrCode : VarExists; ErrMsg : 'Variable already exists'),
      (ErrCode : OpExpected; ErrMsg : 'Operator, comma or close bracket expected'),
      (ErrCode : NotOpExpected; ErrMsg : 'Value, variable, function or open bracket expected'),
      (ErrCode : AssignExpected; ErrMsg : 'Assignment expected'),
      (ErrCode : InvalidValue; ErrMsg : 'Invalid operand value'),
      (ErrCode : InvalidRange; ErrMsg : 'Incorrect borders of range'),
      (ErrCode : IllegalBreak; ErrMsg : 'Illegal Break or Continue operation'),
      (ErrCode : DimMismatch; ErrMsg : 'Dimensioned quantities mismatch'),
      (ErrCode : IllegalDim; ErrMsg : 'Operation not allowed on dimensioned quantity'),
      (ErrCode : DimOver; ErrMsg : 'Dimensional exponent overflow'),
      (ErrCode : DimExists; ErrMsg : 'Dimensioned quantities already assigned'),
      (ErrCode : DimlessValue; ErrMsg : 'Cannot assign units to None, String or Array value'),
      (ErrCode : OffsetUnit; ErrMsg : 'Unit with offset cannot be specified'),
      (ErrCode : UnknownUnit; ErrMsg : 'Unit is not found'),
      (ErrCode : UnDecodeDim; ErrMsg : 'Can not decode dimension, list of units is incomplete')
    );

