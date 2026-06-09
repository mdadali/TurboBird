//*****************************************************
//  RunFormula Expression Scripting Engine
//  RunFormula Demo and Test
//  Rev. 14.04.2026

//  Author: Alexander Torubarov
//  Contact: runfla@yandex.com

//  Filename: RFTestUnit.pas

//  Copyright (C) 2026 Alexander Torubarov
//  Licensed under the MIT License.
//  See the LICENSE file in the project root
//  or a copy available at https://opensource.org
//  for full license information.
//*****************************************************

// TODO -oRFla.Demo -cRev.2026.04.14:

unit RFDemoUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit;

type

  { TDemoForm }

  TDemoForm = class(TForm)
    LoopButton: TButton;
    ByteCodeCheckBox: TCheckBox;
    SaveCheckBox: TCheckBox;
    VrtCheckBox: TCheckBox;
    TestsButton: TButton;
    CloseButton: TButton;
    ExecButton: TButton;
    Label2: TLabel;
    ResultMemo: TMemo;
    ParseButton: TButton;
    Label1: TLabel;
    FlaSynEdit: TSynEdit;
    procedure LoopButtonClick(Sender: TObject);
    procedure ParseButtonClick(Sender: TObject);
    procedure ExecButtonClick(Sender: TObject);
    procedure TestsButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  DemoForm: TDemoForm;

implementation

uses
  RunFormula, Variants;

{$R *.lfm}

{$include ../RunFormula/runflamsg.inc}
{$include ../RunFormula/runfladef.inc}
{$include ../RunFormula/runflalib.inc}
{$include ../RunFormula/runflaunits.inc}

{ TDemoForm }

const TagName : array[TTag] of string = (
  'Value',
  'Var',
  'Array',
  'Bracket',
  'Code',
  'Func',
  'Call',
  'Text',
  'Local',
  'Subr',
  'None',
  'Do',
  'Expr',
  'Assign',
  'OR',
  'XOR',
  'AND',
  'Equal',
  'NotEqual',
  'Greater',
  'Less',
  'NotLess',
  'NotGreater',
  'Concat',
  'Plus',
  'Minus',
  'Mult',
  'Slash',
  'SHL',
  'SHR',
  'DIV',
  'MOD',
  'Negative',
  'NOT',
  'Power',
  'Phy');

function MyRunFlaVar(constref Name:string; out Phy:SizeInt; out Save:boolean):Variant;
begin
  Result:=StrToFloat(InputBox('', 'Get value for variable <'+Name+'>', ''));
  Phy:=0;
  Save:=false;
end;

function AppendResult(const ParamCount:SizeInt; out Phy:SizeInt; Context:pointer):Variant;
begin
  DemoForm.ResultMemo.Text:=DemoForm.ResultMemo.Text+RunFlaParam(0-ParamCount, Phy, Context);
end;

procedure TDemoForm.ParseButtonClick(Sender: TObject);
const Digits: array[0..$F] of char = '0123456789ABCDEF';
var Error : TRunFlaError;
    cod, info, S : string;
    sz, L : SizeInt;
    p, bp, fin : PByte;
    sp : PChar;
begin
  ResultMemo.Clear;
  RunFlaFuncReg('appendresult', @AppendResult);
  cod:=RunFlaParse(FlaSynEdit.Text, Error);
  with Error do if Code<>OK then begin
    ResultMemo.Text:='ERROR at Position '+IntToStr(Position)+': '+RunFlaErrorMsg[Code].ErrMsg;
    FlaSynEdit.SelStart:=Position+1;
    FlaSynEdit.SelEnd:=Position+2;
    exit;
  end;
  p:=PByte(cod);
  fin:=p+PToken(p)^.Size;
  while p<fin do with PToken(p)^ do begin
    case Tag of
      TagValue, TagText : sz:=Size;
      TagVar, TagFunc, TagCall : sz:=IndexTokenSize;
      TagArray, TagBracket, TagCode, TagLocal, TagSubr, TagNone, TagExpr : sz:=ExprTokenSize;
      else sz:=OpTokenSize
    end;
    SetLength(S, sz*3);
    bp:=p;
    sp:=pointer(S);
    for L:=1 to sz do begin
      sp^:=Digits[bp^ shr 4];
      inc(sp);
      sp^:=Digits[bp^ and $F];
      inc(sp);
      sp^:=#$20;
      inc(sp);
      inc(bp);
    end;
    ResultMemo.Append(S);
    info:=IntToStr(p-PByte(cod))+': Tag '+TagName[Tag]+
      ', Position '+IntToStr(Source)+', Token size '+IntToStr(sz);
    if Tag<TagAssign then info:=info+', Next '+IntToStr(p-PByte(cod)+Size);
    case Tag of
      TagValue : with ValRec do case VType of
                   VNone  : S:=': None';
                   VInt   : S:=': Int = '+IntToStr(Int)+' [0x'+IntToHex(Int)+']';
                   VFloat : S:=': Float = '+FloatToStr(Flo);
                   VCplex : S:=': Complex = ('+FloatToStr(Flo)+','+FloatToStr(Img)+')';
                   VChar  : begin
                              S:=': Char = ';
                              if (Chr>$20) and (Chr<$7F) then S:=S+'"'+char(Chr)+'" ';
                              S:=S+'[0x'+IntToHex(Chr)+']';
                            end;
                   VGap   : S:=': Range = ['+FloatToStr(Flo)+':'+FloatToStr(Img)+']';
                   VIGap  : S:=': Integer Range = ['+IntToStr(Int)+':'+IntToStr(IGap)+']';
                 end;
      TagVar : S:=': ID '+IntToStr(Index)+' <'+
                 string(fin+PSizeInt(fin+Index*SI+OpTokenSize)^+OpTokenSize)+'>';
      TagFunc : S:=': Index '+IntToStr(Index);
      TagCall : S:=': Address '+IntToStr(p-PByte(cod)-Index);
      TagText : S:=': Text "'+string(@Text)+'" (length '+IntToStr(length(string(@Text)))+')';
      else SetLength(S, 0);
    end;
    ResultMemo.Append(info+S);
    ResultMemo.Append('');
    inc(p, sz);
  end;
end;

procedure TDemoForm.LoopButtonClick(Sender: TObject);
var Error : TRunFlaError;
    S, src, cod : string;
    i : SizeInt;
    bc : boolean;
begin
  RunFlaFuncReg('appendresult', @AppendResult);
  src:=FlaSynEdit.Text;
  S:=RunFlaExecStr(RunFlaParse(src, Error), Error);
  with Error do if Code<>OK then begin
    ResultMemo.Text:='ERROR at Position '+IntToStr(Position)+': '+RunFlaErrorMsg[Code].ErrMsg;
    FlaSynEdit.SelStart:=Position+1;
    FlaSynEdit.SelEnd:=Position+2;
    exit;
  end;
  try
    i:=StrToInt(InputBox('', 'Loop count:', ''));
  except
    exit;
  end;
  cod:=RunFlaParse(src);
  bc:=ByteCodeCheckBox.Checked;
  for i:=0 to i do if bc then begin
    S:=RunFlaExecStr(cod);
    SetLength(S, 0);
  end else begin
    S:=RunFlaExecStr(RunFlaParse(src));
    SetLength(S, 0);
  end;
  ShowMessage('Done.');
  ResultMemo.Clear;
  ResultMemo.Append(S);
end;

procedure TDemoForm.ExecButtonClick(Sender: TObject);
var FlaError : TRunFlaError;
    Exec : string;
begin
  ResultMemo.Clear;
  RunFlaFuncReg('appendresult', @AppendResult);
  if VrtCheckBox.Checked
  then Exec:=RunFlaExecVrt(RunFlaParse(FlaSynEdit.Text, FlaError), FlaError, @MyRunFlaVar)
  else Exec:=RunFlaExecStr(RunFlaParse(FlaSynEdit.Text, FlaError), FlaError, @MyRunFlaVar);
  with FlaError do if Code<>OK then begin
    ResultMemo.Text:='ERROR at Position '+IntToStr(Position)+': '+RunFlaErrorMsg[Code].ErrMsg+
    ' "'+Value+'"';
    FlaSynEdit.SelStart:=Position+1;
    FlaSynEdit.SelEnd:=Position+2;
    exit;
  end;
  ResultMemo.Append(Exec);
end;

procedure TDemoForm.TestsButtonClick(Sender: TObject);
var T : TTag;
    Token : TToken;
    Info, s : string;
    flg : boolean;
    i, j : SizeInt;
    k : integer;
    p : pointer;
    VT : TVType;
    MemList : TMemList;
    Lst : TStringList;
    UnitDesc : TUnitDesc;
    PrefAbbr : TPrefAbbr;

  function YN(b:boolean):string;
  begin
    if b then Result:=': yes' else Result:=': NO';
  end;

begin
  ResultMemo.Clear;
  s:='0123456789';
  p:=@s[10];
  ResultMemo.Append('Is PSizeInt[-1] = (PSizeInt-1)^'+YN(PSizeInt(p)[-1]=(PSizeInt(p)-1)^));
  ResultMemo.Append('Is PSizeInt[-1] = (pointer-SI)^'+YN(PSizeInt(p)[-1]=PSizeInt(PByte(p)-SI)^));
  ResultMemo.Append('Is PByte(string) = @string[1]'+YN(PByte(s)=@s[1]));
  with MemList do p:=@List;
  ResultMemo.Append('Is PMemList = @TMemList.List'+YN(p=@MemList));
  flg:=true;
  SetLength(s, 12);
  with PAnsiRec(PByte(s)-SAnsiRec)^ do begin
    flg:=flg and (Len=12);
    flg:=flg and (Ref=1);
    flg:=flg and (ElementSize=1);
  end;
  ResultMemo.Append('Is TAnsiRec valid'+YN(flg));
  s:='0123456789';
  ResultMemo.Append('Is PSizeInt(string)[-1] = length(stirng)'+YN(PSizeInt(s)[-1]=10));
  ResultMemo.Append('Is PSizeInt(string)[-2] = stirng ref'+YN(PSizeInt(s)[-2]=-1));
  ResultMemo.Append('Is SAnsiRec = SizeInt * 3'+YN(SAnsiRec=SI*3));
  SetLength(s, 0);
  ResultMemo.Append('Is SetLength(S, 0) = nil'+YN(PByte(s)=nil));
  SetLength(info, 0);
  s:=s+info;
  ResultMemo.Append('Is nil & nil = nil'+YN(PByte(s)=nil));
  flg:=true;
  Lst:=TStringList.Create;
  Lst.Sorted:=true;
  Lst.CaseSensitive:=true;
  for UnitDesc in DimUnits do begin
    Info:=UnitDesc.Abbr;
    if Lst.Find(Info, k) then begin
      ResultMemo.Append('Unit duplicated: '+Info);
      flg:=false;
    end else begin
      Lst.Append(Info);
      if (UnitDesc.Flag and UPF)=0 then for PrefAbbr in UnitPrefixes do begin
        s:=PrefAbbr.Abbr+Info;
        if Lst.Find(s, k) then begin
          ResultMemo.Append('Unit duplicated: '+s);
          flg:=false;
        end else Lst.Append(s);
      end;
    end;
  end;
  Lst.Destroy;
  ResultMemo.Append('Is Unit List valid'+YN(flg));
  ResultMemo.Append('Size of SizeInt = '+IntToStr(SI));
  ResultMemo.Append('Size of integer = '+IntToStr(Sint));
  ResultMemo.Append('Size of pointer = '+IntToStr(SPtr));
  ResultMemo.Append('Size of TRFloat = '+IntToStr(SRFloat));
  ResultMemo.Append('Size of TTag = '+IntToStr(SizeOf(TTag)));
  ResultMemo.Append('Size of TAnsiRec = '+IntToStr(SAnsiRec));
  ResultMemo.Append('Size of TValRec = '+IntToStr(SValRec));
  ResultMemo.Append('Size of TUnitDesc = '+IntToStr(SizeOf(TUnitDesc)));
  ResultMemo.Append('Size of TPrefAbbr = '+IntToStr(SizeOf(TPrefAbbr)));
  ResultMemo.Append('Size of OpToken = '+IntToStr(OpTokenSize));
  ResultMemo.Append('Size of ExprToken = '+IntToStr(ExprTokenSize));
  ResultMemo.Append('Size of IndexToken = '+IntToStr(IndexTokenSize));

  ResultMemo.Append('Done.');

  exit;





  Info:='Tag Name Check: ';
  flg:=true;
  for T in TTag do begin
    str(T, S);
    if 'Tag'+TagName[T]<>S then begin
      ResultMemo.Append(Info+'ERROR (Tag'+TagName[T]+' <=> '+S+')');
      flg:=false;
    end;
  end;
  if flg then ResultMemo.Append(Info+'OK');


                      // !!! TEST Str2Ptr, DecStrRef



                      {
                      with PVarData(@Result)^ do begin
              vtype:=varString;
              vstring:=nil;
            end;  }


end;

procedure TDemoForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

