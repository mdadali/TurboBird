(*
Variant List (C) Sergey Bodrov 2020
MIT License

Usage:

var MyList: Variant;
MyList := VarListCreate;
MyList.AddValue(1);
MyList.AddValue('test');
WriteLn(MyList.GetValue(0), MyList.GetValue(1));

Methods:
  SetCapacity(MaxCount) - allocate memory for list values
  AddValue(Value) - add value to end of list. Capacity increased if needed
  GetCount() - get stored items count
  GetValue(Index) - get item value for given index (0..GetCount()-1)
  SetValue(Index, Value) - set item value for given index (0..GetCount()-1)

Note: '+' operator can't be used because it require conversion of right-side operand to same type

*)
unit VarLists;

interface

uses Variants, VarUtils;

type

  { TVarListType }

  TVarListType = class(TInvokeableVariantType)
  private
    procedure VarDerefAndCopy(V: PVarData);
  protected
    function LeftPromotion(const V: TVarData; const Operation: TVarOp;
      out RequiredVarType: TVarType): Boolean; override;
  public
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp); override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
  end;

  function VarListCreate(): Variant;
  function VarTypeList(): TVarType;
  function VarIsList(const V: Variant): Boolean;
  function VarListAsStr(const V: Variant): string;

implementation

uses SysUtils;

type
  TVariantListItemArray = array of TVarData;

  TVariantList = class
  public
    Items: TVariantListItemArray;
    ItemsCount: Integer;
    RefCount: Integer;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure SetCapacity(AValue: Integer);
    function GetValue(AIndex: Integer; var AValue: TVarData): Boolean;
    procedure SetValue(AIndex: Integer; const AValue: TVarData);
    procedure AddValue(const AValue: TVarData);
    procedure Assign(ASource: TVariantList);

    function AsString(): string;
  end;

  TVarListData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VList: TVariantList;
    Reserved4: {$ifdef FPC}PtrInt{$else}LongInt{$endif};
  end;

var
  VarListType: TVarListType = nil;

function VarTypeList(): TVarType;
begin
  Result := VarListType.VarType;
end;

function VarListCreate(): Variant;
begin
  VarClear(Result);
  TVarListData(Result).VType := VarTypeList;
  TVarListData(Result).VList := TVariantList.Create();
end;

function VarIsList(const V: Variant): Boolean;
begin
  Result := (not VarIsNull(V)) and (TVarListData(V).VType = VarTypeList);
end;

function VarListAsStr(const V: Variant): string;
begin
  if TVarListData(V).VType = VarTypeList then
    Result := TVarListData(V).VList.AsString
  else
    Result := VarToStrDef(V, '');
end;

{ TVarListType }

procedure TVarListType.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  LTemp: TVarData;
begin
  if Source.VType = VarType then
  begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TVarListData(Source).VList.AsString);
      varString:
        VarDataFromStr(Dest, TVarListData(Source).VList.AsString);
    else
      VarDataInit(LTemp);
      try
        LTemp.VType := varInteger;
        LTemp.VInteger := TVarListData(Source).VList.ItemsCount;
        VarDataCastTo(Dest, LTemp, AVarType);
      finally
        VarDataClear(LTemp);
      end;
    end;
  end
  else
    RaiseCastError;
end;

procedure TVarListType.BinaryOp(var Left: TVarData; const Right: TVarData;
  const Operation: TVarOp);
begin
  case Operation of
    opAdd:
    begin
      if (Left.VType = VarTypeList) and (Right.VType = VarTypeList) then
      begin
        TVarListData(Left).VList.Assign(TVarListData(Right).VList);
      end
      else
      if VarIsStr(Variant(Left)) and (Right.VType = VarTypeList) then
      begin
        Variant(Left) := Variant(Left) + TVarListData(Right).VList.AsString();
      end
      else
        inherited BinaryOp(Left, Right, Operation);
    end;
  else
    inherited BinaryOp(Left, Right, Operation);
  end;
end;

procedure TVarListType.Clear(var V: TVarData);
var
  i: Integer;
begin
  V.VType := varEmpty;
  if TVarListData(V).VList <> nil then
  begin
    if TVarListData(V).VList.RefCount > 0 then
      Dec(TVarListData(V).VList.RefCount)
    else
    begin
      for i := TVarListData(V).VList.ItemsCount-1 downto 0 do
        VarDataClear(TVarListData(V).VList.Items[i]);

      TVarListData(V).VList.Free();
      TVarListData(V).VList := nil;
    end;
  end;
end;

procedure TVarListType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    TVarListData(Dest).VType := VarType;
    // reference to source
    TVarListData(Dest).VList := TVarListData(Source).VList;
    Inc(TVarListData(Source).VList.RefCount);
    {Log('List.Copy() - create VList and clone data to dest');
    TVarListData(Dest).VType := VarType;
    TVarListData(Dest).VList := TVariantList.Create();
    TVarListData(Dest).VList.Assign(TVarListData(Source).VList); }
  end;
end;

function TVarListType.DoFunction(var Dest: TVarData;
  const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
var
  sName: string;
begin
  Result := False;
 
  sName := UpperCase(Name);
  if (sName = 'GETCOUNT') then
  begin
    Variant(Dest) := TVarListData(V).VList.ItemsCount;
    Result := True;
  end
  else
  if (sName = 'GETVALUE') and (Length(Arguments) = 1) then
  begin
    // GetValue(Index)
    TVarListData(V).VList.GetValue(Variant(Arguments[0]), Dest);
    Result := True;
  end;

end;

function TVarListType.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
var
  sName: string;
  i: Integer;
begin
  sName := UpperCase(Name);
  if (sName = 'SETVALUE') and (Length(Arguments) = 2) then
  begin
    i := Variant(Arguments[0]);
    // SetValue(Index, Value)
    VarDerefAndCopy(@V);
    TVarListData(V).VList.SetValue(i, Arguments[1]);
    Result := True;
  end
  else
  if (sName = 'ADDVALUE') and (Length(Arguments) = 1) then
  begin
    // AddValue(Value)
    VarDerefAndCopy(@V);
    TVarListData(V).VList.AddValue(Arguments[0]);
    Result := True;
  end
  else
  if (sName = 'SETCAPACITY') and (Length(Arguments) = 1) then
  begin
    // SetCapacity(MaxCount)
    VarDerefAndCopy(@V);
    TVarListData(V).VList.SetCapacity(Variant(Arguments[0]));
    Result := True;
  end
  else
    Result := False;
end;

function TVarListType.LeftPromotion(const V: TVarData; const Operation: TVarOp;
  out RequiredVarType: TVarType): Boolean;
begin
  // not need to cast left argument of BinaryOp to this type
  Result := False;
end;

procedure TVarListType.VarDerefAndCopy(V: PVarData);
var
  PrevVList: TVariantList;
begin
  PrevVList := TVarListData(V^).VList;
  if PrevVList.RefCount > 0 then
  begin
    TVarListData(V^).VList := TVariantList.Create();
    TVarListData(V^).VList.Assign(PrevVList);
    Dec(PrevVList.RefCount);
  end;
end;

{ TVariantList }

procedure TVariantList.AddValue(const AValue: TVarData);
begin
  Inc(ItemsCount);
  if Length(Items) < ItemsCount then
    SetCapacity(ItemsCount + 1);
  //Variant(FItems[FItemsCount-1]) := Variant(AValue);
  SetValue(ItemsCount-1, AValue);
end;

procedure TVariantList.AfterConstruction;
begin
  inherited;
  SetCapacity(0);
end;

function TVariantList.AsString: string;
var
  i: Integer;
begin
  Result := '[';
  for i := 0 to ItemsCount-1 do
  begin
    if i > 0 then
      Result := Result + ',';
    Result := Result + VarToStr(Variant(Items[i]));
  end;
  Result := Result + ']';
end;

procedure TVariantList.BeforeDestruction;
begin
  inherited;

end;

procedure TVariantList.Assign(ASource: TVariantList);
var
  i: Integer;
begin
  SetCapacity(ASource.ItemsCount);
  ItemsCount := ASource.ItemsCount;
  for i := 0 to ItemsCount-1 do
  begin
    SetValue(i, ASource.Items[i]);
  end;
end;

function TVariantList.GetValue(AIndex: Integer; var AValue: TVarData): Boolean;
begin
  if AIndex < ItemsCount then
  begin
    //Variant(AValue) := Variant(FItems[AIndex]);
    //{$ifdef FPC}
    //VarDataCopy(AValue, Items[AIndex]);
    //{$else}
    //VarCopyNoInd(Variant(AValue), Variant(Items[AIndex]));
    VarListType.VarDataCopyNoInd(AValue, Items[AIndex]);
    //{$endif}
    Result := True;
  end
  else
    Result := False;
end;

procedure TVariantList.SetCapacity(AValue: Integer);
begin
  SetLength(Items, AValue);
  if ItemsCount > AValue then
    ItemsCount := AValue;
end;

procedure TVariantList.SetValue(AIndex: Integer; const AValue: TVarData);
var
  pValue: PVarData;
begin
  // dereference VarData
  pValue := @AValue;
  while pValue^.vType = (varVariant or varByRef) do
    pValue := PVarData(pValue^.vPointer);

  if (AIndex >= 0) and (AIndex < ItemsCount) then
  begin
    //Variant(FItems[AIndex]) := Variant(AValue);
    //{$ifdef FPC}
    //VarDataCopy(FItems[AIndex], AValue);
    //{$else}
    VarListType.VarDataCopyNoInd(Items[AIndex], pValue^);
    //VarCopyNoInd(Variant(Items[AIndex]), Variant(AValue));
    //{$endif}
  end;
end;

initialization

VarListType := TVarListType.Create();

finalization

VarListType.Free();

end.
