//*****************************************************
//  RunFormula Unit-aware Expression Scripting Engine
//  Version 0.1.b
//  Released at 1.06.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: runformula.pas
//  Source Code: Object Pascal / FreePascal
//  Compatible: Lazarus 4.2 x64 win10

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Main -cRev.2026.04.14:

unit RunFormula;

{$mode ObjFPC}{$H+}

interface

type
  TRFloat = Double;             // or Extended... or Single...

  TRunFlaVar = function(constref Name:string; out Dim:SizeInt; out Save:boolean):Variant;
  TRunFlaFunc = function(const ParamCount:SizeInt; out Dim:SizeInt; Context:pointer):Variant;

{$include runflaerr.inc}
{$include runflamsg.inc}

function RunFlaParse(constref Fla:string; var Error:TRunFlaError):string;
function RunFlaParse(constref Fla:string):string;
function RunFlaExecStr(constref Fla:string; var Error:TRunFlaError; FlaVar:TRunFlaVar=nil):string;
function RunFlaExecStr(constref Fla:string; FlaVar:TRunFlaVar=nil):string;
function RunFlaExecVrt(constref Fla:string; var Error:TRunFlaError; FlaVar:TRunFlaVar=nil):Variant;
function RunFlaExecVrt(constref Fla:string; FlaVar:TRunFlaVar=nil):Variant;
function RunFlaParam(Offset:SizeInt; out Dim:SizeInt; Context:pointer):Variant;
procedure RunFlaRaise(ErrCode:TRunFlaErrCode);
function RunFlaFuncReg(constref Name:string; Func:TRunFlaFunc):TRunFlaErrCode;

var RunFlaErrCode : TRunFlaErrCode = OK;

implementation

{$B-}                           // do not complete boolean evaluation
{$POINTERMATH ON}               // allow use of pointer math
{$R-}                           // switch off range checking
{$Q-}                           // switch off overflow checking
{$T-}                           // untyped address operator
{$inline on}

{$define runfla_optuses}

uses SysUtils

{$include runflaopt.inc}

{$undef runfla_optuses} ;

{$include runfladef.inc}
{$include runflalib.inc}

function Term(Pnt:PByte; var Context:TContext):PValRec;         //DONE -oRFla.Main -cRev.2026.04.28: Func Term
var lst : PMemList;
    pv : PValRec;
    p, fin, fn : PByte;
    loc : SizeInt = 0;
    idx, i : SizeInt;
    flg : boolean;

{$include runflaterm.inc}

begin
  with Context, PToken(Pnt)^ do begin
    ProcToken:=Pnt;
    case Tag of
      TagValue : Result:=@ValRec;
      TagVar   : begin
                   i:=VarPool.Count-1;
                   repeat
                     lst:=VarPool.List[i];
                     idx:=VarListFind(lst, Index);
                     if idx>=0 then break;
                     if loc=0 then loc:=idx;
                     dec(i);
                   until i<0;
                   if idx<0 then begin
                     if PToken(Pnt+IndexTokenSize)^.Tag<>TagAssign then begin
                       if RunFlaVar=nil then raise EError.Create(UnknownVar);
                       Result:=DoRunFlaVar(Context, Index);
                       if flg then ValCopy(Result, NewVar(Pnt, idx, false));
                     end else Result:=NewVar(Pnt, loc);
                   end else begin
                     Result:=@PVariable(lst^.List[idx])^.Value;
                     PoolIndex:=i;
                   end;
                 end;
      TagArray : raise EError.Create(Unsupported);
      TagBracket, TagCode : begin
                              fin:=Pnt+Size;
                              p:=Pnt+ExprTokenSize;
                              Result:=@CVNone;
                              while (p<fin) and (Flow=NML) do begin
                                FreeTerm(Result);
                                fn:=p+PToken(p)^.Size;
                                inc(p, ExprTokenSize);
                                Result:=Expr(p, fn, 0);
                              end;
                            end;
      TagFunc  : begin
                   idx:=FuncArg.Count;
                   fin:=Pnt+Size;
                   p:=Pnt+IndexTokenSize;
                   while p<fin do begin
                     PPByte(MemListAdd(FuncArg))^:=p;
                     inc(p, PToken(p)^.Size);
                   end;
                   with PFlaRec(FuncList.List[Index])^ do if IsUser
                     then Result:=DoUserFunc(UserFunc, FuncArg.Count-idx, @Context)
                     else Result:=Func(FuncArg.Count-idx, Context);
                   FuncArg.Count:=idx;
                 end;
      TagCall  : begin
                   idx:=FuncArg.Count;
                   fin:=Pnt+Size;
                   p:=Pnt+IndexTokenSize;
                   while p<fin do begin
                     PPValRec(MemListAdd(FuncArg))^:=Term(p, Context);
                     inc(p, PToken(p)^.Size);
                   end;
                   p:=Pnt-Index;
                   fin:=p+PToken(p)^.Size;
                   inc(p, ExprTokenSize*2);
                   fn:=p+PToken(p)^.Size;
                   inc(p, ExprTokenSize);
                   loc:=VarPool.Count;           // index of last varlist
                   CreateVarList(Context);
                   i:=idx;
                   while p<fn do begin
                     pv:=nil;
                     if i<FuncArg.Count then pv:=PPValRec(FuncArg.List[i])^;
                     InitVar(p, pv, true);
                     inc(p, PToken(p)^.Size);
                     inc(i);
                   end;
                   if i<FuncArg.Count then raise EError.Create(ParamNumber, Pnt);
                   for i:=FuncArg.Count-1 downto idx do FreeTerm(PPValRec(FuncArg.List[i])^);
                   FuncArg.Count:=idx;
                   Result:=@CVNone;
                   while (p<fin) and (Flow=NML) do begin
                     FreeTerm(Result);
                     fn:=p+PToken(p)^.Size;
                     inc(p, ExprTokenSize);
                     Result:=Expr(p, fn, 0);
                   end;
                   if Flow=EXT then Flow:=NML;
                   if (Result^.VAlloc=VP) and (PoolIndex=loc) then
                     Result:=ValCopy(Result, NewLV(Context));
                   with PMemList(VarPool.List[loc])^ do begin
                     for i:=Count-1 downto 0 do FreeValue(@PVariable(List[i])^.Value);
                     MemListClear(PMemList(@List)^);
                   end;
                   dec(VarPool.Count);
                 end;
      TagText  : begin
                   Result:=NewLV(Context);
                   with Result^ do begin
                     VType:=VStr;
                     Str:=@Text;
                   end;
                 end;
      TagLocal : begin
                   fin:=Pnt+Size;
                   p:=Pnt+ExprTokenSize;
                   while p<fin do begin
                     InitVar(p, nil, false);
                     inc(p, PToken(p)^.Size);
                   end;
                   Result:=@CVNone;
                 end;
      TagSubr, TagNone : Result:=@CVNone;
      TagExpr  : begin
                   p:=Pnt+ExprTokenSize;
                   Result:=Expr(p, Pnt+Size, 0);
                 end;
    end;
    TermResult:=Result;
    ProcToken:=Pnt;
  end;
end;

function Param(Offset:SizeInt; var Context:TContext):PValRec;   //DONE -oRFla.Main -cRev.2026.04.21: Func Param
begin
  with Context.FuncArg do Result:=Term(PPByte(List[Count+Offset])^, Context);
end;

function RunFlaParam(Offset:SizeInt; out Dim:SizeInt; Context:pointer):Variant;
type PContext = ^TContext;                    //DONE -oRFla.Main -cRev.2026.04.21: Func RunFlaParam
begin
  with PContext(Context)^.FuncArg do begin
    if (Offset>=0) or (Offset<(-Count)) then raise EError.Create(ParamNumber);
    Result:=AsVrt(Term(PPByte(List[Count+Offset])^, PContext(Context)^));
    Dim:=PContext(Context)^.TermResult^.VPhy;
  end;
  PostParam(PContext(Context)^);
end;

function Exec(constref Fla:string; var Error:TRunFlaError; FlaVar:TRunFlaVar; Buf:PValRec):PValRec;
const InitProc : TToken = (Tag : TagCode; Source : 0);     //DONE -oRFla.Main -cRev.2026.05.06: Func Exec
var Context : TContext;
    p : PByte;
    i : SizeInt;
    j : SizeInt = -1;

  procedure FillError(Err:TRunFlaErrCode; P:PByte=nil);    //DONE -oRFla.Main -cRev.2026.05.06: Proc FillError
  begin
    if Err<>OK then begin
      if RunFlaErrCode=OK then RunFlaErrCode:=Err;
      if P=nil then P:=Context.ProcToken;
      with Error do if Code=OK then begin
        Code:=Err;
        Position:=PToken(P)^.Source;
        try
          Value:=Str2Str(AsStr(Context.TermResult));
        except
        end;
      end;
    end;
  end;

begin
  Result:=@CVNone;
  p:=PByte(Fla);
  with Context do begin
    MemListInit(LVStack, SValRec, LVStackGrow);
    MemListInit(VarPool, SizeOf(TMemList), VarPoolGrow);
    MemListInit(FuncArg, SPtr, ParamGrow);
    RunFlaVar:=FlaVar;
    TermResult:=@CVNone;
    ProcToken:=@InitProc;
    ByteCode:=p;
    Flow:=NML;
  end;
  try
    if p=nil then raise EError.Create(OK);
    Context.VarTable:=p+PToken(p)^.Size+OpTokenSize;
    CreateVarList(Context);
    Result:=Term(p, Context);
    if Context.Flow in [BRK, CON] then raise EError.Create(IllegalBreak);
    FillError(OK);
  except
    on E:EError do FillError(E.FCode, E.FToken);
    on E:EResult do begin
      Result:=Context.TermResult;
      FillError(OK);
    end;
    on E:EOverflow do FillError(Overflow);
    on E:EDivByZero do FillError(DivZero);
    on E:EZeroDivide do FillError(DivZero);
    on E:EInvalidOp do FillError(InvalidValue);
    on E:EStackOverflow do FillError(StackOver);
    on E:EHeapException do FillError(Malloc);
    else FillError(Unknown);
  end;
  if Result^.VAlloc<>BC then Result:=ValCopy(Result, Buf);
  with Context do begin
    if VarPool.ListLng>0 then repeat
      inc(j);
      with PMemList(VarPool.List[j])^ do begin
        if j<VarPool.Count then
          for i:=Count-1 downto 0 do FreeValue(@PVariable(PMemList(@List)^.List[i])^.Value);
        MemListFree(PMemList(@List)^);
      end;
    until MemListIsLegs(VarPool, j);
    for i:=LVStack.Count-1 downto 0 do FreeValue(LVStack.List[i]);
    MemListFree(VarPool);
    MemListFree(LVStack);
    MemListFree(FuncArg);
  end;
end;

function RunFlaExecStr(constref Fla:string; var Error:TRunFlaError; FlaVar:TRunFlaVar=nil):string;
var R : TValRec;                                          //DONE -oRFla.Main -cRev.2026.05.03: Func RunFlaExecStr
begin
  Result:=Str2Str(AsStr(Exec(Fla, Error, FlaVar, @R)));
  FreeValue(@R);
end;

function RunFlaExecStr(constref Fla:string; FlaVar:TRunFlaVar=nil):string;
var Error : TRunFlaError = (Code : Unknown);              //DONE -oRFla.Main -cRev.2026.05.03: Func RunFlaExecStr
    R : TValRec;             // prevent filling
begin
  Result:=Str2Str(AsStr(Exec(Fla, Error, FlaVar, @R)));
  FreeValue(@R);
end;

function RunFlaExecVrt(constref Fla:string; var Error:TRunFlaError; FlaVar:TRunFlaVar=nil):Variant;
var R : TValRec;                                          //DONE -oRFla.Main -cRev.2026.05.03: Func RunFlaExecVrt
begin
  Result:=AsVrt(Exec(Fla, Error, FlaVar, @R));
  FreeValue(@R);
end;

function RunFlaExecVrt(constref Fla:string; FlaVar:TRunFlaVar=nil):Variant;
var Error : TRunFlaError = (Code : Unknown);              //DONE -oRFla.Main -cRev.2026.05.03: Func RunFlaExecVrt
    R : TValRec;             // prevent filling
begin
  Result:=AsVrt(Exec(Fla, Error, FlaVar, @R));
  FreeValue(@R);
end;

procedure RunFlaRaise(ErrCode:TRunFlaErrCode);            //DONE -oRFla.Main -cRev.2026.05.03: Proc RunFlaRaise
begin
  raise EError.Create(ErrCode);
end;

function RunFlaParse(constref Fla:string; var Error:TRunFlaError):string;
const TagOp = TagExpr;                     //DONE -oRFla.Main -cRev.2026.05.03: Func RunFlaParse
type TCls = record
       ClsTag : TTag;
       SizeP : PSizeInt;                      // to Token.Size
       Offs : SizeInt;
     end;
     PCls = ^TCls;
var Buf : array of string;
    SBuf : PByte = nil;
    BufP : PByte = PByte(BufSize);
    BufSum : SizeInt = 0;                          // sum of closed Bufs
    ClsList, SubrList, DefList, VarList : TMemList;
    PreTag : TTag = TagLess;
    TextCodePage : TSystemCodePage;
    TokenAddr : PByte;                       // --> ValueToken
    InDef : integer = 0;
    Comment : char = #0;
    FlaPos : integer = 0;                    // position in Fla
    Pnt : PChar;
    ofs, i : SizeInt;

{$include runflaparse.inc}

begin
  try
    PByte(Result):=nil;
    MemListInit(ClsList, SizeOf(TCls), ClsGrow);
    MemListInit(SubrList, SFlaRec, SubrGrow);
    MemListInit(DefList, SFlaRec, DefGrow);
    MemListInit(VarList, SFlaRec, VarIDGrow);
    Pnt:=pointer(Fla);
    if Pnt=nil then raise EError.Create(OK);
    TextCodePage:=PAnsiRec(Pnt-SAnsiRec)^.CodePage;
    ExprToken(TagCode, TagCode);
    ExprToken(TagExpr, TagExpr);
    DoParse(Pnt);
    if ClsList.Count>2 then raise EError.Create(MissingBracket);
    CloseExpr;
    CloseToken(TagCode);
    OpToken(TagGreater);                           // insert stub
    RequestBuf(VarList.Count*SI);                  // writing Var Name Table
    ofs:=VarList.Count*SI+SAnsiRec;
    for i:=0 to VarList.Count-1 do begin
      PSizeInt(BufP)^:=ofs;
      inc(BufP, SI);
      inc(ofs, AlignString(PFlaRec(VarList.List[i])^.Lng));
    end;
    for i:=0 to VarList.Count-1 do with PFlaRec(VarList.List[i])^ do
      WriteString(ID, Lng, AlignString(Lng));
    i:=length(Buf)-1;
    SetLength(Buf[i], BufP-SBuf);
    if i>0 then begin
      inc(BufSum, BufP-SBuf);
      SetLength(Result, BufSum);
      BufP:=PByte(Result);
      for i:=0 to i do begin
        ofs:=length(Buf[i]);
        DataCopy(PByte(Buf[i]), BufP, ofs);
        inc(BufP, ofs);
      end;
    end else Result:=Buf[0];
    FillError(OK);
  except
    on E:EError do FillError(E.FCode, E.FPos);
    on E:EOverflow do FillError(Overflow);
    on E:EHeapException do FillError(Malloc);
    else FillError(Unknown);
  end;
  for i:=0 to DefList.Count-1 do DecStrRef(PFlaRec(DefList.List[i])^.PText);
  MemListFree(VarList);
  MemListFree(DefList);
  MemListFree(SubrList);
  MemListFree(ClsList);
  for i:=0 to length(Buf)-1 do SetLength(Buf[i], 0);
  SetLength(Buf, 0);
end;

function RunFlaParse(constref Fla:string):string;             //DONE -oRFla.Main -cRev.2026.05.03: Func RunFlaParse
var Error : TRunFlaError;
begin
  Result:=RunFlaParse(Fla, Error);
end;

function FuncReg(constref Name:string; FlaFunc:TFlaFunc; User:boolean=false):TRunFlaErrCode;
var fr : PFlaRec;                                         //DONE -oRFla.Main -cRev.2026.05.03: Func FuncReg
    p : PByte;
    L, i : SizeInt;
begin
  try
    p:=Str2Ptr(Name);
    L:=PSizeInt(p)[-1];
    i:=MemListFind(FuncList, p, p+L);
    if i<0 then begin
      fr:=MemListInsert(FuncList, not i);
      Result:=OK;
    end else begin
      fr:=FuncList.List[i];
      DecStrRef(fr^.ID);
      Result:=FuncExists;
    end;
    with fr^ do begin
      ID:=p;
      Lng:=L;
      IsUser:=User;
      Func:=FlaFunc;
    end;
  except
    on E:EHeapException do Result:=Malloc;
    else Result:=Unknown;
  end;
  if (Result<>OK) and (RunFlaErrCode=OK) then RunFlaErrCode:=Result;
end;

function RunFlaFuncReg(constref Name:string; Func:TRunFlaFunc):TRunFlaErrCode;
begin                                                     //DONE -oRFla.Main -cRev.2026.05.03: Func RunFlaFuncReg
  Result:=FuncReg(Name, TFlaFunc(Func), true);
end;

{$include runflafunc.inc}
{$include runflaunits.inc}

procedure Init;
var UnitDesc : TUnitDesc;
    PrefAbbr : TPrefAbbr;
begin
  MemListInit(FuncList, SFlaRec, FuncGrow);
  MemListInit(UnitList, SizeOf(TUnitDesc), UnitDescGrow);
  for UnitDesc in DimUnits do PUnitDesc(MemListAdd(UnitList))^:=UnitDesc;
  MemListInit(PrefList, SizeOf(TPrefAbbr), PrefAbbrGrow);
  for PrefAbbr in UnitPrefixes do PPrefAbbr(MemListAdd(PrefList))^:=PrefAbbr;
end;

initialization

  Init;
  FuncRegister;

finalization

  while FuncList.Count>0 do begin
    DecStrRef(PFlaRec(MemListGetLast(FuncList))^.ID);
    dec(FuncList.Count);
  end;
  MemListFree(FuncList);
  MemListFree(UnitList);
  MemListFree(PrefList);

end.

