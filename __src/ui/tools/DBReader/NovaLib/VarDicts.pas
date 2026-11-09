(*
Variant Dictionary (C) Sergey Bodrov 2020
Auto-sorted by names, binary search used to get value by name

MIT License

Usage:

var MyDict: Variant;
MyDict := VarDictCreate();
MyDict.Field1 := 1;
MyDict.Field2 := 'test';

Methods:
  GetCount() - get stored items count
  GetValue(Index) - get item value for given Index (0..GetCount()-1)
  GetName(Index) - get item name for given Index (0..GetCount()-1)
  GetNameIndex(Name) - get index of item Name, or <0 if Name not found
  SetValue(Name, Value) - set value for Name

*)
unit VarDicts;

{$IFDEF fpc}
{$mode objfpc}
{$ENDIF}
{$h+}

interface

uses Variants, VarUtils{?};

type

  { TVarDictType }

  TVarDictType = class(TInvokeableVariantType)
  private
    procedure VarDerefAndCopy(V: PVarData);
  protected
    function LeftPromotion(const V: TVarData; const Operation: TVarOp;
      out RequiredVarType: TVarType): Boolean; override;
    function SetPropertyInternal(const V: TVarData; const Name: string; const Value: TVarData): Boolean;
  public
    function IsClear(const V: TVarData): Boolean; override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp); override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty({$ifdef FPC}var{$else}const{$endif} V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
  end;

  function VarDictCreate(): Variant;
  function VarTypeDict(): TVarType;
  function VarDictAsStr(const V: Variant): string;
  function VarIsDict(const V: Variant): Boolean;

var
  VarDictCaseSensitive: Boolean;

implementation

uses SysUtils;

type
  TVariantDictionaryItem = record
    Name: string;
    Value: TVarData;
  end;
  TVariantDictionaryItemArray = array of TVariantDictionaryItem;

  TVariantDictionary = class
  public
    RefCount: Integer; // 0 - only one owner
    Items: TVariantDictionaryItemArray;
    ItemsCount: Integer;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    { if >= 0 then index of found name
      if < 0 then 1-based index for name (can be > Count)
      IsAddItem - add named item }
    function GetNameIndex(const AName: string; IsAddItem: Boolean = False): Integer;

    function GetCount(): Integer;
    function AsString(): string;
    procedure Assign(ASource: TVariantDictionary);
  end;

  TVarDictData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VDict: TVariantDictionary;
    Reserved4: {$ifdef FPC}PtrInt{$else}LongInt{$endif};
  end;
  PVarDictData = ^TVarDictData;

var
  VarDictType: TVarDictType = nil;

function VarTypeDict(): TVarType;
begin
  Result := VarDictType.VarType;
end;

function VarDictCreate(): Variant;
begin
  VarClear(Result);
  TVarDictData(Result).VType := VarTypeDict;
  TVarDictData(Result).VDict := TVariantDictionary.Create();
end;

function VarDictAsStr(const V: Variant): string;
begin
  if TVarDictData(V).VType = VarTypeDict then
    Result := TVarDictData(V).VDict.AsString
  else
    Result := VarToStrDef(V, '');
end;

function VarIsDict(const V: Variant): Boolean;
begin
  Result := (not VarIsNull(V)) and (TVarDictData(V).VType = VarTypeDict);
end;

{ TVariantDictionaryType }

procedure TVarDictType.Clear(var V: TVarData);
var
  i: Integer;
  pVarDict: PVarDictData;
begin
  V.VType := varEmpty;
  pVarDict := Addr(V);
  if pVarDict^.VDict <> nil then
  begin
    if pVarDict^.VDict.RefCount > 0 then
      Dec(pVarDict^.VDict.RefCount)
    else
    begin
      for i := pVarDict^.VDict.ItemsCount-1 downto 0 do
        VarDataClear(pVarDict^.VDict.Items[i].Value);

      pVarDict^.VDict.Free();
      pVarDict^.VDict := nil;
    end;
  end;
end;

function TVarDictType.IsClear(const V: TVarData): Boolean;
begin
  Result := (TVarDictData(V).VDict = nil) or (TVarDictData(V).VDict.GetCount() = 0);
end;

function TVarDictType.LeftPromotion(const V: TVarData; const Operation: TVarOp;
  out RequiredVarType: TVarType): Boolean;
begin
  // not need to cast left argument of BinaryOp to this type
  Result := False;
end;

procedure TVarDictType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
var
  i: Integer;
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    {$ifdef FPC}
    VarDataCopyNoInd(Dest, Source);
    {$else}
    VariantCopy(Dest, Source);
    {$endif}
  end
  else
  begin
    TVarDictData(Dest).VType := VarType;
    // reference to source
    TVarDictData(Dest).VDict := TVarDictData(Source).VDict;
    Inc(TVarDictData(Source).VDict.RefCount);
  end;
end;

function TVarDictType.DoFunction(var Dest: TVarData;
  const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
var
  i: Integer;
  sName: string;
begin
  Result := False;

  sName := UpperCase(Name);
  if (sName = 'GETCOUNT') then
  begin
    Variant(Dest) := TVarDictData(V).VDict.GetCount();
    Result := True;
  end
  else
  if (sName = 'GETVALUE') and (Length(Arguments) = 1) then
  begin
    if VarDataIsOrdinal(Arguments[0]) then
      i := Variant(Arguments[0])
    else
      i := TVarDictData(V).VDict.GetNameIndex(Variant(Arguments[0]));
    if (i >= 0) and (i < TVarDictData(V).VDict.ItemsCount) then
    begin
      {$ifdef FPC}
      VarDataCopy(Dest, TVarDictData(V).VDict.Items[i].Value);
      {$else}
      VariantCopy(Dest, TVarDictData(V).VDict.Items[i].Value);
      {$endif}
    end
    else
      Variant(Dest) := Null;
    Result := True;
  end
  else
  if (sName = 'GETNAME') and (Length(Arguments) = 1) then
  begin
    i := Variant(Arguments[0]);
    if (i >= 0) and (i < TVarDictData(V).VDict.ItemsCount) then
    begin
      VarDataFromStr(Dest, TVarDictData(V).VDict.Items[i].Name);
      Result := True;
    end
    else
      VarRangeCheckError(VarType);
  end
  else
  if (sName = 'GETNAMEINDEX') and (Length(Arguments) = 1) then
  begin
    Variant(Dest) := TVarDictData(V).VDict.GetNameIndex(Variant(Arguments[0]));
    Result := True;
  end;
end;

function TVarDictType.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  if (UpperCase(Name) = 'SETVALUE') and (Length(Arguments) = 2) then
  begin
    Result := SetPropertyInternal(V, Variant(Arguments[0]), Arguments[1]);
  end
  else
    Result := False;
end;

function TVarDictType.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  i: Integer;
begin
  i := TVarDictData(V).VDict.GetNameIndex(Name);
  if i >= 0 then
  begin
    Result := True;
    {$ifdef FPC}
    VarDataCopy(Dest, TVarDictData(V).VDict.Items[i].Value);
    {$else}
    VariantCopy(Dest, TVarDictData(V).VDict.Items[i].Value);
    {$endif}
  end
  else
    Result := False;
end;

function TVarDictType.SetProperty({$ifdef FPC}var{$else}const{$endif} V: TVarData;
  const Name: string; const Value: TVarData): Boolean;
begin
  Result := SetPropertyInternal(V, Name, Value);
end;

function TVarDictType.SetPropertyInternal(const V: TVarData;
  const Name: string; const Value: TVarData): Boolean;
var
  i: Integer;
  pValue: PVarData;
begin
  Result := True;
  VarDerefAndCopy(@V);

  i := TVarDictData(V).VDict.GetNameIndex(Name, True);
  // deref and clone Value, invoke Value.Copy()
  // dereference VarData
  pValue := @Value;
  while pValue^.vType = (varVariant or varByRef) do
    pValue := PVarData(pValue^.vPointer);
  {$ifdef FPC}
  VarDataCopyNoInd(TVarDictData(V).VDict.Items[i].Value, pValue^);
  {$else}
  VarDataCopyNoInd(TVarDictData(V).VDict.Items[i].Value, Value);
  {$endif}
  //VarCopyNoInd(Variant(TVarDictData(V).VDict.Items[i].Value), Variant(Value));
end;

procedure TVarDictType.Cast(var Dest: TVarData; const Source: TVarData);
begin
  //inherited;
  VariantInit(Dest);
  TVarDictData(Dest).VType := VarType;
  TVarDictData(Dest).VDict := TVariantDictionary.Create();
end;

procedure TVarDictType.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  LTemp: TVarData;
begin
  if Source.VType = VarType then
  begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TVarDictData(Source).VDict.AsString);
      varString:
        VarDataFromStr(Dest, TVarDictData(Source).VDict.AsString);
    else
      VarDataInit(LTemp);
      try
        LTemp.VType := varInteger;
        LTemp.VInteger := TVarDictData(Source).VDict.ItemsCount;
        VarDataCastTo(Dest, LTemp, AVarType);
      finally
        VarDataClear(LTemp);
      end;
    end;
  end
  else
    RaiseCastError;
end;

procedure TVarDictType.BinaryOp(var Left: TVarData; const Right: TVarData;
  const Operation: TVarOp);
var
  i: Integer;
begin
  case Operation of
    opAdd:
    begin
      if (Left.VType = VarTypeDict) and (Right.VType = VarTypeDict) then
      begin
        for i := 0 to TVarDictData(Right).VDict.ItemsCount - 1 do
        begin
          SetProperty(Left, TVarDictData(Right).VDict.Items[i].Name, TVarDictData(Right).VDict.Items[i].Value);
        end;
      end
      else
      if VarIsStr(Variant(Left)) and (Right.VType = VarTypeDict) then
      begin
        Variant(Left) := Variant(Left) + TVarDictData(Right).VDict.AsString();
      end
      else
        inherited BinaryOp(Left, Right, Operation);
    end;
  else
    inherited BinaryOp(Left, Right, Operation);
  end;
end;

procedure TVarDictType.VarDerefAndCopy(V: PVarData);
var
  PrevVDict: TVariantDictionary;
begin
  PrevVDict := TVarDictData(V^).VDict;
  if PrevVDict.RefCount > 0 then
  begin
    TVarDictData(V^).VDict := TVariantDictionary.Create();
    TVarDictData(V^).VDict.Assign(PrevVDict);
    Dec(PrevVDict.RefCount);
  end;
end;

{ TVariantDictionary }

procedure TVariantDictionary.AfterConstruction;
begin
  inherited;
  ItemsCount := 0;
  SetLength(Items, ItemsCount);
end;

procedure TVariantDictionary.BeforeDestruction;
begin
  inherited;

end;

function TVariantDictionary.GetCount: Integer;
begin
  Result := ItemsCount;
end;

function TVariantDictionary.GetNameIndex(const AName: string; IsAddItem: Boolean): Integer;
var
  i, iCompResult, iL, iR, iM: Integer;
  sName: string;
begin
  if not VarDictCaseSensitive then
    sName := LowerCase(AName)
  else
    sName := AName;
  // binary search
  Result := -1;
  iL := 0;
  iR := ItemsCount-1;
  while iL <= iR do
  begin
    iM := (iL + iR) div 2;
    iCompResult := CompareText(sName, Items[iM].Name);
    if iCompResult > 0 then
    begin
      iL := iM + 1;
      Result := -(iM + 2);
    end
    else
    if iCompResult < 0 then
    begin
      iR := iM - 1;
      Result := -(iM + 1);
    end
    else
    begin
      Result := iM;
      Break;
    end;
  end;

  // insert name if needed
  if (Result < 0) and IsAddItem then
  begin
    Result := -Result - 1;
    Inc(ItemsCount);
    SetLength(Items, ItemsCount);
    // shift elements
    for i := ItemsCount-1 downto Result + 1 do
      Items[i] := Items[i-1];

    Items[Result].Name := sName;
    VariantInit(Items[Result].Value);
  end;
end;

procedure TVariantDictionary.Assign(ASource: TVariantDictionary);
var
  i: Integer;
  pValue: PVarData;
begin
  ItemsCount := ASource.ItemsCount;
  SetLength(Items, ItemsCount);
  // copy values
  for i := 0 to ItemsCount - 1 do
  begin
    Items[i].Name := ASource.Items[i].Name;
    Items[i].Value.VType := varEmpty;
    // dereference source value
    pValue := Addr(ASource.Items[i].Value);
    while pValue^.vType = (varVariant or varByRef) do
      pValue := PVarData(pValue^.vPointer);
    //SetProperty(Items[i].Value, ASource.Items[i].Name, ASource.Items[i].Value);
    //{$ifdef FPC}
    //VarDataCopy(Items[i].Value, ASource.Items[i].Value);
    //{$else}
    //VarCopyNoInd(Variant(Items[i].Value), Variant(ASource.Items[i].Value)); // deref and clone Value, invoke Value.Copy()
    VarDictType.VarDataCopyNoInd(Items[i].Value, pValue^);
    //{$endif}
  end;
end;

function TVariantDictionary.AsString: string;
var
  i: Integer;
begin
  Result := '{';
  for i := 0 to ItemsCount-1 do
  begin
    if i > 0 then
      Result := Result + ',';
    Result := Result + Items[i].Name + ':';
    try
      Result := Result + VarToStr(Variant(Items[i].Value));
    except
      // not converted to string
      Result := Result + VarTypeAsText(VarType(Variant(Items[i].Value))); // !!!
    end;
  end;
  Result := Result + '}';
end;

initialization

VarDictType := TVarDictType.Create();
VarDictCaseSensitive := False;

finalization

VarDictType.Free();

end.
