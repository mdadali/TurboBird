//*****************************************************
//  RunFormula Expression Scripting Engine
//  Definitions include file
//  Rev. 15.05.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: runfladef.inc

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Def -cRev.2026.04.16:

type
  TTag = (
    TagValue,
    TagVar,
    TagArray,
    TagBracket,
    TagCode,
    TagFunc,              // call build-in function
    TagCall,              // call function in Fla
    TagText,
    TagLocal,
    TagSubr,
    TagNone,
    TagDo,                // pseudo

    TagExpr,              //      <--  anchor,
    TagAssign,            // =    all Operators Tokens should be greater

    TagOR,                // or
    TagXOR,               // xor
    TagAND,               // and
    TagEqual,             // ==
    TagNotEqual,          // <>
    TagGreater,           // >
    TagLess,              // <
    TagNotLess,           // >=
    TagNotGreater,        // <=
    TagConcat,            // &
    TagPlus,              // +
    TagMinus,             // -
    TagMult,              // *
    TagSlash,             // /
    TagSHL,               // shl <<
    TagSHR,               // shr >>
    TagDIV,               // div
    TagMOD,               // mod
    TagNegative,          // - unary
    TagNOT,               // not unary
    TagPower,             // **
    TagPhy);              // Phy

  TAnsiRec = packed record                         // from astrings.inc
    CodePage : TSystemCodePage;                    // but packed just in case
    ElementSize : word;
{$ifdef CPU64}
    { align fields  }
    Dummy : DWord;
{$endif CPU64}
    Ref : SizeInt;
    Len : SizeInt;
  end;

  PAnsiRec = ^TAnsiRec;

  TMemList = record
    List : PPointer;          // list of pointers, must be first !!!
    Tail : PPointer;          // tail of last block
    Head : PByte;             // point to first block
    Legs : PByte;             // point to last taken item in chain
    Count : SizeInt;          // in items
    ListLng : SizeInt;        // in items
    Size : SizeInt;           // size of item in bytes
    Grow : SizeInt;           // in items
  end;

  PMemList = ^TMemList;

const
  SI = SizeOf(SizeInt);
  LI = Low(SizeInt);
  Sint = SizeOf(integer);
  SPtr = SizeOf(pointer);
  SRFloat = SizeOf(TRFloat);

type
  TVType = (VNone = 0, VInt, VFloat, VRsvd, VCplex, VChar, VStr, VGap, VIGap, VArr);

  TValRec = record
    VType : TVType;
    VAlloc : (BC, VP, LV);
    VPhy : SizeInt;
    case byte of
      0 : (Int, IGap : SizeInt);
      1 : (Flo, Img : TRFloat);
      2 : (Chr : byte);
      3 : (Str : PByte);
      4 : (Ansi : PSizeInt);
      5 : (Arr : PMemList);
  end;

  PValRec = ^TValRec;
  PPValRec = ^PValRec;

  TVariable = record
    VarID : SizeInt;
    Value : TValRec;
  end;

  PVariable = ^TVariable;

const
  SAnsiRec = SizeOf(TAnsiRec);
  SValRec = SizeOf(TValRec);

  AbbrMaxSize = 15;       // maximum size of unit abbreviation in bytes

  BufSize = $8000;        // bytecode buffer increment
  DefDepth = 64;          // maximum define nesting level
  VarDepth = 4096;        // maximum number of variable lists

  ClsGrow = 32;           // nesting depth step * 2
  SubrGrow = 16;          // subroutine count increment
  DefGrow = 16;           // define count increment
  VarIDGrow = 64;         // unique variable names increment
  FuncGrow = 64;          // function count increment
  VarPoolGrow = 16;       // variable list pool size increment
  VarGrow = 16;           // variables per list increment
  LVStackGrow = 64;       // stack depth increment
  ParamGrow = 32;         // parameter buffer increment (total for nested calls)
  UnitDescGrow = 32;
  PrefAbbrGrow = 32;

type
  TToken = packed record
    Tag : TTag;
{$if (SizeOf(TTag) and (Sint-1)) > 0}
    Dummy : array[0..(SizeOf(TTag) or (Sint-1))] of byte;
{$endif}
    Source : integer;                   //aligning to Size of Integer
    Size : SizeInt;
    case byte of
      0 : (Index : SizeInt);
      1 : (ValRec : TValRec);
      2 : (AnsiRec : TAnsiRec;
           Text : char);
  end;

  PToken = ^TToken;

  TFlow = (NML = 0, RSL, EXT, BRK, CON);

  TContext = record
    LVStack    : TMemList;           // list of TValRec
    VarPool    : TMemList;           // list of TMemList
    FuncArg    : TMemList;           // list of PByte
    RunFlaVar  : TRunFlaVar;
    TermResult : PValRec;
    VarTable   : PByte;              // point to offset table in bytecode
    ProcToken  : PByte;
    ByteCode   : PByte;
    PoolIndex  : SizeInt;            // variable list index in VarPool
    Flow       : TFlow;
  end;

  TFlaFunc = function(ParamCount:SizeInt; var Context:TContext):PValRec;

  TFlaRec = record
  ID : PByte;
  Lng : SizeInt;
  IsUser : boolean;
    case byte of
      0 : (Func : TFlaFunc);
      1 : (UserFunc : TRunFlaFunc);
      2 : (Index : SizeInt);
      3 : (PText : PByte);
      4 : (PChr : PChar);
  end;

  PFlaRec = ^TFlaRec;

  TDPhy = packed record
    case byte of
      0 : (DI : SizeInt);
      1 : (DB : array[0..SI-1] of ShortInt);
  end;

  EError = class(Exception)
    FCode : TRunFlaErrCode;
    FPos : pointer;
    FToken : PByte;
    constructor Create(Code:TRunFlaErrCode);
    constructor Create(Code:TRunFlaErrCode; CharPos:PChar);
    constructor Create(Code:TRunFlaErrCode; Token:PByte);
  end;

  EResult = class(Exception);

  TCharSet = set of char;

  TUnitDesc = record
    Sig : SizeInt;
    Scale : TRFloat;
    Offset : TRFloat;             // K = (C * 1.0) + 273.15
    Flag : integer;
    Abbr : string[AbbrMaxSize];
  end;

  PUnitDesc = ^TUnitDesc;

  TPrefAbbr = record
    Scale : TRFloat;
    Flag : integer;
    Abbr : string[AbbrMaxSize];
  end;

  PPrefAbbr = ^TPrefAbbr;

const
  CVNone : TValRec = (VType : VNone; VAlloc : BC; VPhy : 0);
  TextChars : TCharSet = ['.', '0'..'9', 'A'..'Z', '_', 'a'..'z'];
  FirstChars : TCharSet = ['A'..'Z', '_', 'a'..'z'];
  UnitChars : TCharSet = ['A'..'Z', 'a'..'z', #$80..#$FF];
  Cpi = 3.14159265358979323846;
  CElr = 2.71828182845904523536;

  OpTokenSize = PtrUInt(@TToken(nil^).Size);
  ExprTokenSize = PtrUInt(@TToken(nil^).Index);
  IndexTokenSize = ExprTokenSize+SI;

  SFlaRec = SizeOf(TFlaRec);

  // Units control flags
  UOP  = 1;                // output prohibited
  USG  = 2;                // single unit, compounding prohibited
  UPF  = 4;                // prefix forbidden
  UCPP = 8;                // compounding prefix prohibited

  OpTokenLevel : array[TTag] of integer = (
    0,       // TagValue
    0,       // TagVar
    0,       // TagArray
    0,       // TagBracket
    0,       // TagCode
    0,       // TagFunc
    0,       // TagCall
    0,       // TagText
    0,       // TagLocal
    0,       // TagSubr
    0,       // TagNone
    0,       // TagDo

    0,       // TagExpr
    101,     // TagAssign     =    right-associative, enable A = B = 10

    200,     // TagOR         or
    200,     // TagXOR        xor
    300,     // TagAND        and
    400,     // TagEqual      ==
    400,     // TagNotEqual   <>
    400,     // TagGreater    >
    400,     // TagLess       <
    400,     // TagNotLess    >=
    400,     // TagNotGreater <=
    500,     // TagConcat     &
    600,     // TagPlus       +
    600,     // TagMinus      -         10-5-2 = 3, not 7
    700,     // TagMult       *
    700,     // TagSlash      /
    700,     // TagSHL        shl <<
    700,     // TagSHR        shr >>
    700,     // TagDIV        div
    700,     // TagMOD        mod
    800,     // TagNegative   - unary
    800,     // TagNOT        not
    901,     // TagPower      **   right-associative, 4**3**2 = 4**9
  10000);    // TagPhy must have highest priority

  CaseType : array[TVType, TVType] of TVType = (
// ret. VInt VFloat VCplex VStr VGap VIGap VArr
//           VNone   VInt    VFloat  VRsvd   VCplex  VChar   VStr    VGap    VIGap   VArr
{ VNone  } ( VStr,   VInt,   VFloat, VRsvd,  VCplex, VStr,   VStr,   VGap,   VGap,   VArr  ),
{ VInt   } ( VInt,   VInt,   VFloat, VRsvd,  VCplex, VInt,   VInt,   VGap,   VIGap,  VArr  ),
{ VFloat } ( VFloat, VFloat, VFloat, VRsvd,  VCplex, VFloat, VFloat, VGap,   VGap,   VArr  ),
{ VRsvd  } ( VRsvd,  VRsvd,  VRsvd,  VRsvd,  VRsvd,  VRsvd,  VRsvd,  VRsvd,  VRsvd,  VArr  ),
{ VCplex } ( VCplex, VCplex, VCplex, VRsvd,  VCplex, VCplex, VCplex, VCplex, VCplex, VArr  ),
{ VChar  } ( VStr,   VInt,   VFloat, VRsvd,  VCplex, VInt,   VStr,   VGap,   VIGap,  VArr  ),
{ VStr   } ( VStr,   VInt,   VFloat, VRsvd,  VCplex, VStr,   VStr,   VGap,   VGap,   VArr  ),
{ VGap   } ( VGap,   VGap,   VGap,   VRsvd,  VCplex, VGap,   VGap,   VGap,   VGap,   VArr  ),
{ VIGap  } ( VGap,   VIGap,  VGap,   VRsvd,  VCplex, VIGap,  VGap,   VGap,   VIGap,  VArr  ),
{ VArr   } ( VArr,   VArr,   VArr,   VArr,   VArr,   VArr,   VArr,   VArr,   VArr,   VArr  ));

var
  FuncList : TMemList;      // list of TFlaRec
  UnitList : TMemList;
  PrefList : TMemList;

