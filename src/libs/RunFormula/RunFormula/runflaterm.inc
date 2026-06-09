//*****************************************************
//  RunFormula Expression Scripting Engine
//  Evaluator include file
//  Rev. 27.04.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: runflaterm.inc

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Term -cRev.2026.04.27:

function Vrt2Val(constref V:Variant):PValRec;                  //DONE -oRFla.Term -cRev.2026.03.30: Func Vrt2Val
var S : string;
begin
  Result:=NewLV(Context);
  try
    with Result^ do case PVarData(@V)^.vtype of
      varEmpty, varNull     : begin VType:=VNone{; Str:=nil; }end;
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord,
        varInt64, varQWord  : begin VType:=VInt; Int:=V; end;
      varSingle, varDouble  : begin VType:=VFloat; Flo:=V; end;
      varString, varUString : begin
                                S:=V;
                                if length(S)<>1 then begin
                                  VType:=VStr;
                                  Str:=Str2Ptr(S);
                                  if Str=nil then VType:=VNone;
                                end else begin
                                  VType:=VChar;
                                  Chr:=PByte(S)^;
                                end;
                              end;
      varBoolean            : begin VType:=VInt; if V then Int:=-1 else Int:=0; end;
      else raise EError.Create(VariantError);
    end;
  except
    on E:EConvertError do raise EError.Create(VariantError);
  end;
end;

procedure FreeTerm(PV:PValRec); inline;                        //DONE -oRFla.Term -cRev.2026.04.18: Inline FreeTerm
begin                              // like PostParam
  if PV^.VAlloc=LV then begin
    FreeValue(PV);
    dec(Context.LVStack.Count);
  end;
end;

function Expr(var P:PByte; Fin:PByte; Level:integer):PValRec;  //DONE -oRFla.Term -cRev.2026.05.18: Func Expr
var L : PValRec = @CVNone;        // init VMode
    R : PValRec;
    tag : TTag;
    tkn : PByte;
    lvl : integer;
    f, fa, fb, fc, fd, t : TRFloat;
    phy, I, J : SizeInt;

  procedure FreeLR; inline;                                         //DONE -oRFla.Term -cRev.2026.05.18: Inline FreeLR
  begin                                            // LVal should be called singly
    FreeTerm(R);
    if L^.VAlloc<>LV then begin
      L:=MemListAdd(Context.LVStack);             // like NewLV
      L^.VAlloc:=LV;
    end else FreeValue(L);
  end;

  procedure LVal(N:SizeInt);                                 //DONE -oRFla.Term -cRev.2026.04.22: Proc LVal Int
  begin
    FreeLR;
    with L^ do begin
      VType:=VInt;
      Int:=N;
    end;
  end;

  procedure LVal(const F:TRFloat);                           //DONE -oRFla.Term -cRev.2026.04.22: Proc LVal Float
  begin
    FreeLR;
    with L^ do begin
      VType:=VFloat;
      Flo:=F;
    end;
  end;

  procedure LVal(const Re, Im:TRFloat);                      //DONE -oRFla.Term -cRev.2026.04.22: Proc LVal Cplex
  begin
    FreeLR;
    with L^ do begin
      VType:=VCplex;
      Flo:=Re;
      Img:=Im;
    end;
  end;

  procedure LVal(B:boolean);                                 //DONE -oRFla.Term -cRev.2026.04.22: Proc LVal Bool
  begin
    FreeLR;
    with L^ do begin
      VType:=VInt;
      if B then Int:=-1 else Int:=0;
    end;
  end;

{  procedure LVal(constref S:string);                         //DONE -oRFla.Term -cRev.2026.04.22: Proc LVal Str
  begin
    FreeLR;
    with L^ do begin
      VType:=VStr;
      Str:=Str2Ptr(S);
      if Str=nil then VType:=VNone;
    end;
  end;          }

  procedure LGap(const GL, GR:TRFloat);                      //DONE -oRFla.Term -cRev.2026.04.22: Proc LGap Float
  begin
    FreeLR;
    with L^ do begin
      VType:=VGap;
      Flo:=GL;
      Img:=GR;
    end;
  end;

  procedure LGap(GL, GR:SizeInt);                            //DONE -oRFla.Term -cRev.2026.04.25: Proc LGap Int
  begin
    FreeLR;
    with L^ do begin
      VType:=VIGap;
      Int:=GL;
      IGap:=GR;
    end;
  end;

  function OverPower(Base:SizeInt; out RI:SizeInt):boolean;  //DONE -oRFla.Term -cRev.2026.04.25: Func OverPower
  var res : SizeInt = 1;
      scl : SizeInt;
  begin
    Result:=true;
    scl:=AsInt(R);
    if scl<0 then exit;
    repeat
      if ((scl and 1)<>0) and OverMult(res, Base, res) then exit;
      scl:=scl shr 1;
      if scl=0 then break;
      if OverMult(Base, Base, Base) then exit;
    until false;
    RI:=res;
    Result:=false;
  end;

  procedure PowerComplex(BRE, BIM:TRFloat);                  //DONE -oRFla.Term -cRev.2026.04.23: Proc PowerComplex
  var re : TRFloat = 1;
      im : TRFloat = 0;
      t : TRFloat;
      scl, i : SizeInt;
  begin
    scl:=AsInt(R);
    i:=abs(scl);
    repeat
      if (i and 1)<>0 then begin
        t:=re*BRE-im*BIM;
        im:=re*BIM+im*BRE;
        re:=t;
      end;
      i:=i shr 1;
      if i=0 then break;
      t:=sqr(BRE)-sqr(BIM);
      BIM:=BRE*BIM*2;
      BRE:=t;
    until false;
    if scl<0 then begin
      t:=sqr(re)+sqr(im);
      re:=re/t;
      im:=-im/t;
    end;
    FreeLR;
    with L^ do begin
      VType:=VCplex;
      Flo:=re;
      Img:=im;
    end
  end;

  procedure Concat;                                          //DONE -oRFla.Term -cRev.2026.04.26: Proc Concat
  var p : PByte;                             // not inline
  begin
    p:=Str2Ptr(AsStr(L)+AsStr(R));
    FreeLR;
    with L^ do begin
      VType:=VStr;
      Str:=p;
      if Str=nil then VType:=VNone;
    end;
  end;

begin
  while (P<Fin) and (Context.Flow=NML) do begin
    tag:=PToken(P)^.Tag;
    if tag>=TagAssign then begin
      lvl:=OpTokenLevel[tag];
      if (lvl<Level) or ((lvl=Level) and ((lvl and 1)=0)) then break;
      tkn:=P;
      inc(P, OpTokenSize);
      R:=Expr(P, Fin, lvl);                      // R should be free on exit
      Context.ProcToken:=tkn;
      phy:=0;
      if (L^.VPhy or R^.VPhy)<>0 then case tag of
        TagOR, TagXOR, TagAND, TagSHL,
        TagSHR, TagNOT    : raise EError.Create(IllegalDim);
        TagPlus, TagMinus : begin
                              phy:=L^.VPhy;
                              if (phy<>R^.VPhy) then raise EError.Create(DimMismatch);
                            end;
        TagNegative       : phy:=R^.VPhy;
        TagMult           : phy:=PhyPlus(L^.VPhy, R^.VPhy);
        TagSlash, TagDIV  : phy:=PhyMinus(L^.VPhy, R^.VPhy);
        TagMOD            : begin
                              phy:=R^.VPhy;
                              if (phy<>0) and (phy<>L^.VPhy) then raise EError.Create(DimMismatch);
                              phy:=L^.VPhy;
                            end;
        TagPower          : phy:=PhyMult(L^.VPhy, AsInt(R));
      end;
      case tag of
        TagAssign   : begin                                                  // =
                        if L^.VAlloc<>VP then raise EError.Create(VarExpected);
                        FreeValue(L);
                        ValCopy(R, L);
                        FreeTerm(R);
                        continue;
                      end;
        TagOR       : LVal(AsInt(L) or AsInt(R));                            // or
        TagXOR      : LVal(AsInt(L) xor AsInt(R));                           // xor
        TagAND      : LVal(AsInt(L) and AsInt(R));                           // and
        TagEqual    : LVal(Compare(L, R)=0);                                 // ==
        TagNotEqual : LVal(Compare(L, R)<>0);                                // <>
        TagGreater  : LVal(Compare(L, R)>0);                                 // >
        TagLess     : LVal(Compare(L, R)<0);                                 // <
        TagNotLess  : LVal(Compare(L, R)>=0);                                // >=
        TagNotGreater : LVal(Compare(L, R)<=0);                              // <=
        TagConcat   : Concat;                                                // &
        TagPlus     : case CaseType[L^.VType, R^.VType] of                   // +
                        VInt   : if OverPlus(AsInt(L), AsInt(R), I)
                                   then LVal(AsFloat(L)+AsFloat(R)) else LVal(I);
                        VFloat : LVal(AsFloat(L)+AsFloat(R));
                        VCplex : LVal(AsRe(L)+AsRe(R), AsIm(L)+AsIm(R));
                        VStr   : raise EError.Create(InvalidStrVal);
                        VGap   : LGap(AsLGap(L)+AsLGap(R), AsRGap(L)+AsRGap(R));
                        VIGap  : if OverPlus(AsIGapL(L), AsIGapL(R), I) or
                                   OverPlus(AsIGapR(L), AsIGapR(R), J)
                                   then LGap(AsLGap(L)+AsLGap(R), AsRGap(L)+AsRGap(R)) else LGap(I, J);
                        VArr   : raise EError.Create(InvalidArrVal);
                      end;
        TagMinus    : case CaseType[L^.VType, R^.VType] of                   // -
                        VInt   : if OverPlus(AsInt(L), -AsInt(R), I)
                                   then LVal(AsFloat(L)-AsFloat(R)) else LVal(I);
                        VFloat : LVal(AsFloat(L)-AsFloat(R));
                        VCplex : LVal(AsRe(L)-AsRe(R), AsIm(L)-AsIm(R));
                        VStr   : raise EError.Create(InvalidStrVal);
                        VGap   : LGap(AsLGap(L)-AsRGap(R), AsRGap(L)-AsLGap(R));
                        VIGap  : if OverPlus(AsIGapL(L), -AsIGapR(R), I) or
                                   OverPlus(AsIGapR(L), -AsIGapL(R), J)
                                   then LGap(AsLGap(L)-AsRGap(R), AsRGap(L)-AsLGap(R)) else LGap(I, J);
                        VArr   : raise EError.Create(InvalidArrVal);
                      end;
        TagMult     : case CaseType[L^.VType, R^.VType] of                   // *
                        VInt   : if OverMult(AsInt(L), AsInt(R), I)
                                   then LVal(AsFloat(L)*AsFloat(R)) else LVal(I);
                        VFloat : LVal(AsFloat(L)*AsFloat(R));
                        VCplex : begin
                                   fa:=AsRe(L); fb:=AsIm(L); fc:=AsRe(R); fd:=AsIm(R);
                                   LVal(fa*fc-fb*fd, fa*fd+fb*fc);
                                 end;
                        VStr   : raise EError.Create(InvalidStrVal);
                        VGap, VIGap : begin
                                        fa:=AsLGap(L); fb:=AsRGap(L); fc:=AsLGap(R); fd:=AsRGap(R);
                                        LGap(Min4(fa*fc, fa*fd, fb*fc, fb*fd), Max4(fa*fc, fa*fd, fb*fc, fb*fd));
                                      end;
                        VArr   : raise EError.Create(InvalidArrVal);
                      end;
        TagSlash    : case CaseType[L^.VType, R^.VType] of                   // /
                        VInt, VFloat : LVal(AsFloat(L)/AsFloat(R));
                        VCplex : begin
                                   fa:=AsRe(L); fb:=AsIm(L); fc:=AsRe(R); fd:=AsIm(R);
                                   if abs(fc)>=abs(fd) then begin
                                     t:=fd/fc;
                                     f:=fc+fd*t;
                                     LVal((fa+fb*t)/f, (fb-fa*t)/f);
                                   end else begin
                                     t:=fc/fd;
                                     f:=fd+fc*t;
                                     LVal((fa*t+fb)/f, (fb*t-fa)/f);
                                   end;
                                 end;
                        VStr   : raise EError.Create(InvalidStrVal);
                        VGap, VIGap : begin
                                        fa:=AsLGap(L); fb:=AsRGap(L); fc:=AsLGap(R); fd:=AsRGap(R);
                                        if (fc<=0) and (fd>=0) then raise EError.Create(DivZero);
                                        LGap(Min4(fa/fc, fa/fd, fb/fc, fb/fd), Max4(fa/fc, fa/fd, fb/fc, fb/fd));
                                      end;
                        VArr   : raise EError.Create(InvalidArrVal);
                      end;
        TagSHL      : LVal(AsInt(L) shl AsInt(R));                           // shl <<
        TagSHR      : LVal(AsInt(L) shr AsInt(R));                           // shr >>
        TagDIV      : LVal(AsInt(L) div AsInt(R));                           // div
        TagMOD      : LVal(AsInt(L) mod AsInt(R));                           // mod
        TagNegative : with R^ do case VType of                               // - unary
                        VNone  : AsFloat(R);
                        VInt   : if Int<>LI then LVal(-Int) else LVal(-TRFloat(Int));
                        VFloat : LVal(-Flo);
                        VCplex : LVal(-Flo, -Img);
                        VChar  : LVal(-SizeInt(Chr));
                        VStr   : raise EError.Create(InvalidStrVal);
                        VGap   : LGap(-Img, -Flo);
                        VIGap  : if (Int<>LI) and (IGap<>LI) then LGap(-IGap, -Int)
                                   else LGap(-TRFloat(IGap), -TRFloat(Int));
                        VArr   : raise EError.Create(InvalidArrVal);
                      end;
        TagNOT      : LVal(not AsInt(R));                                    // not unary
        TagPower    : with L^ do case VType of                               // **
                        VNone  : AsFloat(L);
                        VInt   : if OverPower(Int, I) then LVal(Power(Int, AsInt(R)))
                                   else LVal(I);
                        VFloat : LVal(Power(Flo, AsInt(R)));
                        VCplex : PowerComplex(Flo, Img);
                        VChar  : if OverPower(Chr, I) then LVal(Power(Chr, AsInt(R)))
                                   else LVal(I);
                        VStr   : raise EError.Create(InvalidStrVal);
                        VGap, VIGap : begin
                                        fa:=AsLGap(L); fb:=AsRGap(L); I:=AsInt(R);
                                        fc:=Power(fa, I); fd:=Power(fb, I);
                                        if (fa<=0) and (fb>=0) then begin
                                          if I<0 then raise EError.Create(DivZero);
                                          if (I and 1)=0 then LGap(0, Max4(fc, fd, 0, 0))
                                            else LGap(fc, fd);
                                          end else LGap(Min4(fc, fc, fd, fd), Max4(fc, fc, fd, fd));
                                        end;
                        VArr : raise EError.Create(InvalidArrVal);
                      end;
        TagPhy      : begin                   // R always must be in BC so FreeTerm(R) is not needed
                        if L^.VAlloc<>LV then L:=ValCopy(L, NewLV(Context));
                        with R^ do ApplyPhyToPV(L, VPhy, Flo, Img);
                        continue;
                      end;
      end;
      L^.VPhy:=phy;
    end else begin
      L:=Term(P, Context);
      inc(P, PToken(P)^.Size);
    end;
  end;
  Context.TermResult:=L;
  Result:=L;
end;

function VarListFind(Lst:PMemList; ID:SizeInt):SizeInt;     //DONE -oRFla.Term -cRev.2026.04.19: Func VarListFind
var L : SizeInt = 0;
    R, diff : SizeInt;
    vl : PPointer;
begin
  vl:=Lst^.List;
  R:=Lst^.Count-1;
  while L<=R do begin
    Result:=L+((R-L) shr 1);
    diff:=ID-PVariable(vl[Result])^.VarID;
    if diff=0 then exit;
    if diff>0 then L:=Result+1 else R:=Result-1;
  end;
  Result:=not L;
end;

function NewVar(P:PByte; Vidx:SizeInt=0; InLocal:boolean=true):PValRec;
var lst : PMemList;               //DONE -oRFla.Term -cRev.2026.04.19: Func NewVar
    i : SizeInt = 0;      // P --> VarToken
begin
  with Context do begin
    if InLocal then i:=VarPool.Count-1;
    PoolIndex:=i;
    lst:=VarPool.List[i];
  end;
  i:=PToken(P)^.Index;
  if Vidx=0 then Vidx:=VarListFind(lst, i);
  if Vidx>=0 then raise EError.Create(VarExists, P);
  with PVariable(MemListInsert(lst^, not Vidx))^ do begin
    VarID:=i;
    Value.VType:=VNone;
    Value.VAlloc:=VP;
    Value.VPhy:=0;
   // Value.Str:=nil;
    Result:=@Value;
  end;
end;

procedure InitVar(Exp:PByte; Pini:PValRec; Eflag:boolean);  //DONE -oRFla.Term -cRev.2026.04.20: Proc InitVar
var p, fin : PByte;                // Exp --> ExprToken
    pv, t : PValRec;
begin
  fin:=Exp+PToken(Exp)^.Size;
  p:=Exp+ExprTokenSize;
  if PToken(p)^.Tag<>TagVar then raise EError.Create(VarExpected, p);
  pv:=NewVar(p);
  inc(p, IndexTokenSize);
  if Pini=nil then begin
    if p<fin then begin
      if PToken(p)^.Tag<>TagAssign then raise EError.Create(AssignExpected, p);
      inc(p, OpTokenSize);
      t:=Expr(p, fin, 0);
      ValCopy(t, pv);
      FreeTerm(t);
    end else if Eflag then raise EError.Create(ParamNumber, Pnt);
  end else ValCopy(Pini, pv);
end;

function DoRunFlaVar(var Context:TContext; I:SizeInt):PValRec;   //DONE -oRFla.Term -cRev.2026.04.25: Func DoRunFlaVar
var phy : SizeInt;
begin                                                   // not inline
  with Context do Result:=Vrt2Val(RunFlaVar(string(VarTable+PSizeInt(VarTable)[I]), phy, flg));
  Result^.VPhy:=phy;
end;

function DoUserFunc(UFunc:TRunFlaFunc; I:SizeInt; P:pointer):PValRec;     // not inline
var phy : SizeInt;                                               //DONE -oRFla.Term -cRev.2026.04.25: Func DoUserFunc
begin
  Result:=Vrt2Val(UFunc(I, phy, P));
  Result^.VPhy:=phy;
end;

