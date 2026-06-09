//*****************************************************
//  RunFormula Expression Scripting Engine
//  Parser include file
//  Rev. 3.05.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: runflaparse.inc

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Parse -cRev.2026.05.03:move Buf to MemList

procedure FillError(Err:TRunFlaErrCode; P:pointer=nil);   //DONE -oRFla.Parse -cRev.2026.05.01: Proc FillError
begin
  if (Err<>OK) and (RunFlaErrCode=OK) then RunFlaErrCode:=Err;
  with Error do begin
    PByte(Value):=nil;
    Position:=0;
    Code:=Err;
    if Code=OK then exit;
    Position:=FlaPos;
    if (P=nil) or (InDef>0) then exit;
    Position:=P-pointer(Fla);
    if PByte(P)^=0 then dec(Position);
  end;
end;

procedure Sequence(T:TTag; Unary:boolean=false);      //DONE -oRFla.Parse -cRev.2026.05.01: Proc Sequence
begin
  if (T<TagOp) xor (PreTag>=TagOp) xor Unary then begin
    if T<TagOp then raise EError.Create(OpExpected);
    raise EError.Create(NotOpExpected);
  end;
  PreTag:=T;
end;

procedure RequestBuf(Size:SizeInt);                   //DONE -oRFla.Parse -cRev.2026.05.01: Proc RequestBuf
var lng, i : SizeInt;
begin
  lng:=BufP-SBuf;
  if (lng+Size)<=BufSize then exit;
  i:=length(Buf);                          // new index
  if i>0 then begin
    SetLength(Buf[i-1], lng);
    inc(BufSum, lng);
  end;
  SetLength(Buf, i+1);
  SetLength(Buf[i], BufSize);
  SBuf:=PByte(Buf[i]);
  BufP:=SBuf;
end;

procedure NoneToken;                                  //DONE -oRFla.Parse -cRev.2026.05.01: Proc NoneToken
begin
  Sequence(TagNone);
  RequestBuf(ExprTokenSize);
  with PToken(BufP)^ do begin
    Tag:=TagNone;
    Source:=FlaPos;
    Size:=ExprTokenSize;
  end;
  inc(BufP, ExprTokenSize);
end;

function AlignString(Lng:SizeInt):SizeInt; inline;        //DONE -oRFla.Parse -cRev.2026.05.01: Inline AlignString
begin
  Result:=Lng+(SAnsiRec+1);                                      // with trailing null
  if (Result and (SI-1))>0 then Result:=(Result or (SI-1))+1;    // aligning to SizeInt
end;

procedure WriteString(P:PByte; Lng, Size:SizeInt; Mask:integer=$20202020);
var fin, dst : PByte;                                 //DONE -oRFla.Parse -cRev.2026.05.01: Proc WriteString
begin
  RequestBuf(Size);
  with PAnsiRec(BufP)^ do begin
    CodePage:=TextCodePage;
    ElementSize:=1;
    Ref:=-1;                             // const string
    Len:=Lng;
  end;
  dst:=BufP+SAnsiRec;
  fin:=P+Lng-Sint;
  while P<=fin do begin
    PInteger(dst)^:=PInteger(P)^ or Mask;
    inc(dst, Sint);
    inc(P, Sint);
  end;
  inc(fin, Sint);
  while P<fin do begin
    dst^:=P^ or byte(Mask);
    inc(dst);
    inc(P);
  end;
  dst^:=0;
  inc(BufP, Size);
end;

procedure OpToken(T:TTag; Unary:boolean=false);       //DONE -oRFla.Parse -cRev.2026.04.30: Proc OpToken
begin
  Sequence(T, Unary);
  RequestBuf(OpTokenSize);
  with PToken(BufP)^ do begin
    Tag:=T;
    Source:=FlaPos;
  end;
  inc(BufP, OpTokenSize);
end;

procedure ExprToken(Expr, Cls:TTag; Idx:SizeInt=-1);  //DONE -oRFla.Parse -cRev.2026.04.30: Proc ExprToken
var L : SizeInt = ExprTokenSize;
    ps : PSizeInt;
begin
  Sequence(Expr);
  if Idx>=0 then L:=IndexTokenSize;
  RequestBuf(L);
  with PToken(BufP)^ do begin
    Tag:=Expr;
    Source:=FlaPos;
    ps:=@Size;
    if Idx>=0 then Index:=Idx;
  end;
  with PCls(MemListAdd(ClsList))^ do begin
    ClsTag:=Cls;
    SizeP:=ps;
    Offs:=BufP-SBuf+BufSum;
  end;
  inc(BufP, L);
end;

procedure CloseToken(T:TTag);                         //DONE -oRFla.Parse -cRev.2026.04.30: Proc CloseToken
begin
  Sequence(T);
  with PCls(MemListGetLast(ClsList))^ do begin
    if ClsTag<>T then raise EError.Create(IllegalBracket);
    SizeP^:=BufP-SBuf+BufSum-Offs;
  end;
  dec(ClsList.Count);
end;

procedure CloseExpr; inline;                          //DONE -oRFla.Parse -cRev.2026.04.30: Inline CloseExpr
begin
  if PreTag=TagExpr then begin
    dec(BufP, ExprTokenSize);       // undo ExprToken, allow ()
    dec(ClsList.Count);
  end else CloseToken(TagExpr);
end;

procedure DoParse(P:PChar);                           //DONE -oRFla.Parse -cRev.2026.05.03: Proc DoParse
var vr : TValRec;
    tf : PChar;                     // textfin
    S : string;
    I : SizeInt;
    scl, ofs : TRFloat;
    c : char;
    flg : boolean;

  procedure TextToken(P:PChar; Lng:SizeInt);          //DONE -oRFla.Parse -cRev.2026.05.01: Proc TextToken
  var sz : SizeInt;
  begin
    Sequence(TagText);
    RequestBuf(ExprTokenSize);
    sz:=AlignString(Lng);
    with PToken(BufP)^ do begin
      Tag:=TagText;
      Source:=FlaPos;
      Size:=sz+ExprTokenSize;
    end;
    inc(BufP, ExprTokenSize);
    WriteString(PByte(P), Lng, sz, 0);
  end;

  procedure ValueToken(constref V:TValRec; Phy:SizeInt=0);   //DONE -oRFla.Parse -cRev.2026.05.10: Proc ValueToken
  var L : SizeInt;
      pv : PValRec;
  begin
    case V.VType of
      VNone  : begin NoneToken; exit; end;
      VInt   : L:=PtrUint(@TToken(nil^).ValRec.Int)+SI;
      VFloat : L:=PtrUint(@TToken(nil^).ValRec.Flo)+SRFloat;
      VCplex, VGap : L:=PtrUint(@TToken(nil^).ValRec.Img)+SRFloat;
      VChar  : L:=PtrUint(@TToken(nil^).ValRec.Chr)+SizeOf(TValRec.Chr);
      VStr   : begin TextToken(PChar(V.Str), V.Ansi[-1]); exit; end;
      VIGap  : L:=PtrUint(@TToken(nil^).ValRec.IGap)+SI;
    end;
    if (L and (SI-1))>0 then L:=(L or (SI-1))+1;         // aligning to SizeInt
    Sequence(TagValue);
    RequestBuf(L);
    with PToken(BufP)^ do begin
      Tag:=TagValue;
      Source:=FlaPos;
      Size:=L;
      pv:=@ValRec;
      with pv^ do begin
        VType:=V.VType;
        VAlloc:=BC;
        VPhy:=Phy;
        case V.VType of
          VInt   : Int:=V.Int;
          VFloat : Flo:=V.Flo;
          VCplex, VGap : begin Flo:=V.Flo; Img:=V.Img; end;
          VChar  : Chr:=V.Chr;
          VIGap  : begin Int:=V.Int; IGap:=V.IGap; end;
        end;
      end;
    end;
    TokenAddr:=BufP;
    inc(BufP, L);
  end;

  procedure IntToken(I:SizeInt); inline;              //DONE -oRFla.Parse -cRev.2026.04.30: Inline IntToken
  begin
    with vr do begin
      VType:=VInt;
      Int:=I;
    end;
    ValueToken(vr);
  end;

  procedure FloatToken(const F:TRFloat); inline;      //DONE -oRFla.Parse -cRev.2026.04.30: Inline FloatToken
  begin
    with vr do begin
      VType:=VFloat;
      Flo:=F;
    end;
    ValueToken(vr);
  end;

  procedure CplexToken(const Re, Im:TRFloat); inline; //DONE -oRFla.Parse -cRev.2026.04.30: Inline CplexToken
  begin
    with vr do begin
      VType:=VCplex;
      Flo:=Re;
      Img:=Im;
    end;
    ValueToken(vr);
  end;

  procedure CharToken(Ch:byte); inline;               //DONE -oRFla.Parse -cRev.2026.04.30: Inline CharToken
  begin
    with vr do begin
      VType:=VChar;
      Chr:=Ch;
    end;
    ValueToken(vr);
  end;
{
  procedure GapToken(const L, R:TRFloat); inline;     //DONE -oRFla.Parse -cRev.2026.04.30: Inline GapToken
  begin
    with vr do begin
      VType:=VGap;
      Flo:=L;
      Img:=R;
    end;
    ValueToken(vr);
  end;

  procedure IGapToken(L, R:SizeInt);                  //DONE -oRFla.Parse -cRev.2026.04.30: Inline IGapToken
  begin
    with vr do begin
      VType:=VIGap;
      Int:=L;
      IGap:=R;
    end;
    ValueToken(vr);
  end;
}
  procedure VarToken;                                 //DONE -oRFla.Parse -cRev.2026.05.01: Proc VarToken
  var i : SizeInt;
  begin
    Sequence(TagVar);
    RequestBuf(IndexTokenSize);
    i:=MemListFind(VarList, P, tf);
    if i<0 then begin
      i:=not i;
      with PFlaRec(MemListInsert(VarList, i))^ do begin
        ID:=PByte(P);
        Lng:=tf-P;
        Index:=VarList.Count-1;
      end;
    end;
    with PToken(BufP)^ do begin
      Tag:=TagVar;
      Source:=FlaPos;
      Size:=IndexTokenSize;
      Index:=PFlaRec(VarList.List[i])^.Index;
    end;
    inc(BufP, IndexTokenSize);
    P:=tf;
  end;

  function IsKey(constref Key:ShortString):boolean;   //DONE -oRFla.Parse -cRev.2026.05.01: Func IsKey
  var PKey, KeyFin : PByte;
      pf : PChar;
  begin
    Result:=false;
    PKey:=PByte(@Key);                       // PKey point to ShortString[0]
    if PKey^<>(tf-P) then exit;
    pf:=P;
    KeyFin:=PKey+PKey^;                      // point to last char
    inc(PKey);
    while PKey<KeyFin do begin
      if (PWord(pf)^ or $2020)<>PWord(PKey)^ then exit;
      inc(PKey, 2);
      inc(pf, 2);
    end;
    if (pf<tf) and ((PByte(pf)^ or $20)<>PKey^) then exit;
    P:=tf;
    Result:=true;
  end;

begin
  repeat
    c:=P^;
    if c=#0 then exit;
    if Comment<>#0 then begin
      if c=#$D then c:=#$A;
      if c=Comment then Comment:=#0;
      inc(P);
      continue;
    end;
    if InDef=0 then FlaPos:=P-pointer(Fla);
    flg:=true;
    case PWord(P)^ of
  {$ifdef ENDIAN_LITTLE}
      $2A2A : OpToken(TagPower);                // **
      $2F2F : Comment:=#$A;                     // // comment
      $3C3C : OpToken(TagSHL);                  // <<
      $3D3C : OpToken(TagNotGreater);           // <=
      $3D3D : OpToken(TagEqual);                // ==
      $3D3E : OpToken(TagNotLess);              // >=
      $3E3C : OpToken(TagNotEqual);             // <>
      $3E3E : OpToken(TagSHR);                  // >>
  {$else}
      $2A2A : OpToken(TagPower);                // **
      $2F2F : Comment:=#$A;                     // // comment
      $3C3C : OpToken(TagSHL);                  // <<
      $3C3D : OpToken(TagNotGreater);           // <=
      $3C3E : OpToken(TagNotEqual);             // <>
      $3D3D : OpToken(TagEqual);                // ==
      $3E3D : OpToken(TagNotLess);              // >=
      $3E3E : OpToken(TagSHR);                  // >>
  {$endif}
      else flg:=false;
    end;
    if flg then begin
      inc(P, 2);
      continue;
    end;
    case c of
      #0..#$20 : ;
      '"', #$27 : begin
                    inc(P);
                    tf:=FindTextFin(P, c);
                    case tf-P of
                      0 : NoneToken;
                      1 : CharToken(PByte(P)^);
                      2 : if P^='^' then CharToken(PByte(P+1)^-$40) else TextToken(P, tf-P);
                      else TextToken(P, tf-P);
                    end;
                    P:=tf;
                  end;
      '%' : OpToken(TagMOD);
      '&' : OpToken(TagConcat);
      '(' : begin
              ExprToken(TagBracket, TagBracket);
              ExprToken(TagExpr, TagExpr);
            end;
      ')' : begin
              CloseExpr;
              CloseToken(TagBracket);
            end;
      '*' : OpToken(TagMult);
      '+' : OpToken(TagPlus);
      ',',';' : begin
                  CloseToken(TagExpr);
                  PreTag:=TagNone;
                  ExprToken(TagExpr, TagExpr);
                end;
      '-' : if PreTag>=TagOp then OpToken(TagNegative, true) else OpToken(TagMinus);
      '/' : OpToken(TagSlash);
      '0'..'9' : begin
                   tf:=FindTextFin(P);
                   I:=IntCapture(P, tf, flg);
                   if flg then begin
                     IntToken(I);
                     P:=tf;
                   end else FloatToken(FloatCapture(P));
                   continue;
                 end;
      '<' : OpToken(TagLess);
      '=' : begin
              if not (PreTag in [TagVar, TagArray]) then raise EError.Create(VarExpected);
              OpToken(TagAssign);
            end;
      '>' : OpToken(TagGreater);
      '[' : begin
              inc(P);
              tf:=FindTextFin(P, ']');
              ParseValue(P, tf, @vr);
              ValueToken(vr, vr.VPhy);
              if vr.VType=VStr then DecStrRef(vr.Str);
              P:=tf;
            end;
      ']' : begin
              CloseToken(TagExpr);
              CloseToken(TagArray);
            end;
      '`' : case PreTag of
              TagValue : begin
                           tf:=FindTextFin(P+1, '`');
                           I:=PhyCapture(P, tf, scl, ofs);
                           if (scl<>1) or (ofs<>0) then begin
                             ApplyPhyToPV(ValCopy(@PToken(TokenAddr)^.ValRec, @vr), I, scl, ofs);
                             BufP:=TokenAddr;
                             PreTag:=TagOp;
                             ValueToken(vr, vr.VPhy);
                           end else with PToken(TokenAddr)^.ValRec do begin
                             if VPhy<>0 then raise EError.Create(DimExists);
                             VPhy:=I;
                           end;
                           P:=tf;
                         end;
              TagVar, TagArray, TagBracket, TagDo : begin
                                                      tf:=FindTextFin(P+1, '`');
                                                      OpToken(TagPhy);
                                                      with vr do begin
                                                        VType:=VCplex;        // safe due to constref
                                                        ValueToken(vr, PhyCapture(P, tf, Flo, Img));
                                                      end;
                                                      P:=tf;
                                                    end;
              TagText, TagLocal, TagSubr, TagNone : raise EError.Create(DimlessValue);
              else raise EError.Create(NotOpExpected);
            end;
      '{' : Comment:='}';
      else
        if c in FirstChars then begin
          tf:=FindTextFin(P);
          if IsKey('and') then begin OpToken(TagAND); continue; end;
          if IsKey('or') then begin OpToken(TagOR); continue; end;
          if IsKey('not') then begin OpToken(TagNOT, true); continue; end;
          if IsKey('func') then begin
            FirstChar(tf, FirstChars);                // check first char
            ExprToken(TagSubr, TagSubr);
            continue;
          end;
          if IsKey('endfunc') then begin
            CloseExpr;
            CloseToken(TagSubr);
            PreTag:=TagNone;           // prevent possible <endfunc X()>
            continue;
          end;
          if IsKey('local') then begin
            if P^<>'(' then raise EError.Create(InvalidChar, P);
            ExprToken(TagLocal, TagBracket);
            ExprToken(TagExpr, TagExpr);
            inc(P);
            continue;
          end;
          if IsKey('do') then begin
            ExprToken(TagBracket, TagDo);
            ExprToken(TagExpr, TagExpr);
            continue;
          end;
          if IsKey('end') then begin
            CloseExpr;
            CloseToken(TagDo);
            continue;
          end;
          if IsKey('define') then begin
            P:=FirstChar(tf, FirstChars);
            tf:=FindTextFin(P);
            I:=MemListFind(DefList, P, tf);
            if I>=0 then raise EError.Create(DefExists, P);
            with PFlaRec(MemListInsert(DefList, not I))^ do begin
              ID:=PByte(P);
              Lng:=tf-P;
              P:=FirstChar(tf, [#$22, #$27])+1;
              tf:=FindTextFin(P, (P-1)^);
              I:=tf-P;
              if I=0 then begin                         // avoid ""
                dec(P);
                I:=2;
              end;
              SetLength(S, I);
              DataCopy(P, PByte(S), I);
              PText:=Str2Ptr(S);
            end;
            SetLength(S, 0);
            P:=tf+1;
            continue;
          end;
          if IsKey('true') then begin IntToken(-1); continue; end;
          if IsKey('false') then begin IntToken(0); continue; end;
          if IsKey('shl') then begin OpToken(TagSHL); continue; end;
          if IsKey('shr') then begin OpToken(TagSHR); continue; end;
          if IsKey('div') then begin OpToken(TagDIV); continue; end;
          if IsKey('mod') then begin OpToken(TagMOD); continue; end;
          if IsKey('xor') then begin OpToken(TagXOR); continue; end;
          if IsKey('pi') then begin FloatToken(Cpi); continue; end;
          if IsKey('e') then begin FloatToken(CElr); continue; end;
          if IsKey('j') then begin CplexToken(0, 1); continue; end;
          if IsKey('none') then begin NoneToken; continue; end;
          if IsKey('error') then raise EError.Create(Unsupported);
          if IsKey('const') then raise EError.Create(Unsupported);
          if IsKey('global') then raise EError.Create(Unsupported);
          if IsKey('array') then raise EError.Create(Unsupported);
          if IsKey('include') then raise EError.Create(Unsupported);
          if IsKey('inline') then raise EError.Create(Unsupported);
          if IsKey('branch') then raise EError.Create(Unsupported);
          if IsKey('debug') then raise EError.Create(Unsupported);
          if IsKey('is') then raise EError.Create(Unsupported);
          if IsKey('ref') then raise EError.Create(Unsupported);
          case tf^ of
            '(' : begin
                    I:=MemListFind(SubrList, P, tf);
                    if PreTag=TagSubr then begin
                      if I>=0 then raise EError.Create(FuncExists);
                      with PFlaRec(MemListInsert(SubrList, not I))^ do begin
                        ID:=PByte(P);
                        Lng:=tf-P;
                        Index:=BufP-SBuf+BufSum-ExprTokenSize;
                      end;
                      ExprToken(TagExpr, TagExpr);
                      P:=tf;                    // next TagBracket
                      continue;
                    end;
                    if I<0 then begin
                      I:=MemListFind(FuncList, P, tf);
                      if I<0 then raise EError.Create(UnknownFunc);
                      ExprToken(TagFunc, TagBracket, I);
                    end else ExprToken(TagCall, TagBracket,
                      BufP-SBuf+BufSum-PFlaRec(SubrList.List[I])^.Index);
                    ExprToken(TagExpr, TagExpr);
                    P:=tf;
                  end;
            '[' : begin
                    ExprToken(TagArray, TagArray);
                    PreTag:=TagEqual;
                    VarToken;
                    ExprToken(TagExpr, TagExpr);
                  end;
            else
              I:=MemListFind(DefList, P, tf);
              if I>=0 then begin
                if InDef>=DefDepth then raise EError.Create(MaxDepth);
                inc(InDef);
                DoParse(PFlaRec(DefList.List[I])^.PChr);
                dec(InDef);
                P:=tf;
                continue;
              end;
              VarToken;
              continue;
          end;
        end else raise EError.Create(InvalidChar, P);
    end;
    inc(P);
  until false;
end;

