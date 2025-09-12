unit IBArrayHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  IBDatabase, IBCustomDataSet, IBQuery, IB, IBTable, IBSQL;


function IB_Array_AsText(AIBArrayField: TIBArrayField): string;

function IB_1D_Array_AsString(AIBArrayField: TIBArrayField): string;
function IB_2D_Array_AsStringTable(AIBArrayField: TIBArrayField): string;
function IB_ND_Array_AsText(AIBArrayField: TIBArrayField): string;


  { TIBArrayHelper }

implementation

function IB_Array_AsText(AIBArrayField: TIBArrayField): string;
var
  DimCount: Integer;
begin
  Result := '';
  if not Assigned(AIBArrayField) then Exit;

  DimCount := Length(AIBArrayField.ArrayBounds);

  case DimCount of
    1: Result := IB_1D_Array_AsString(AIBArrayField);           // 1D Array
    2: Result := IB_2D_Array_AsStringTable(AIBArrayField);      // 2D Array
  else
    Result := IB_ND_Array_AsText(AIBArrayField) // n-D > 2
  end;
end;

function IB_1D_Array_AsString(AIBArrayField: TIBArrayField): string;
var
  Bounds: TArrayBounds;
  L, U, i: Integer;
  v: string;
begin
  Result := '';
  if not Assigned(AIBArrayField) then Exit;

  try
    Bounds :=  AIBArrayField.ArrayBounds;
    if Length(Bounds) = 0 then
    begin
      Result := 'Array';
      Exit;
    end;

    L := Bounds[0].LowerBound;
    U := Bounds[0].UpperBound;

    for i := L to U do
    begin
      v := AIBArrayField.GetEltAsString(i);

      // Leere Elemente überspringen oder markieren
      if v = '' then
        v := ''; // Optional: 'Array' oder leer lassen

      if Result <> '' then
        Result := Result + ', ';
      Result := Result + v;
    end;

    if Result = '' then
      Result := 'Array'; // fallback wenn alle Elemente leer
  except
    Result := 'Array';
  end;
end;

function IB_2D_Array_AsStringTable(AIBArrayField: TIBArrayField): string;
var
  Bounds: TArrayBounds;
  L1, U1, L2, U2, i, j: Integer;
  v: string;
begin
  Result := '';
  if not Assigned(AIBArrayField) then Exit;

  Bounds := AIBArrayField.ArrayBounds;
  if Length(Bounds) < 2 then
  begin
    Result := 'Array'; // fallback, kein 2D-Array
    Exit;
  end;

  L1 := Bounds[0].LowerBound; U1 := Bounds[0].UpperBound;
  L2 := Bounds[1].LowerBound; U2 := Bounds[1].UpperBound;

  for i := L1 to U1 do
  begin
    for j := L2 to U2 do
    begin
      v := AIBArrayField.GetEltAsString([i, j]);
      if v = '' then v := ''; // optional: 'Array'
      if j > L2 then
        Result := Result + ', ';
      Result := Result + v;
    end;
    Result := Result + sLineBreak; // neue Zeile für nächste äußere Dimension
  end;
end;

function IB_ND_Array_AsText(AIBArrayField: TIBArrayField): string;

  // Rekursive Hilfsfunktion für nD-Arrays
  function ArrayToTextRecursive(const Indices: array of Integer; Dim: Integer): string;
  var
    Bounds: TArrayBounds;
    L, U, i: Integer;
    SubIndices: array of Integer;
    v: string;
  begin
    Bounds := AIBArrayField.ArrayBounds;
    Result := '';

    if Dim >= Length(Bounds) then Exit;

    L := Bounds[Dim].LowerBound;
    U := Bounds[Dim].UpperBound;

    for i := L to U do
    begin
      SetLength(SubIndices, Length(Indices)+1);
      if Length(Indices) > 0 then
        Move(Indices[0], SubIndices[0], Length(Indices)*SizeOf(Integer));
      SubIndices[High(SubIndices)] := i;

      if Dim = High(Bounds) then
      begin
        // letzte Dimension → Element auslesen
        v := AIBArrayField.GetEltAsString(SubIndices);
        if v = '' then v := ''; // optional: 'Array'
        if Result <> '' then Result := Result + ', ';
        Result := Result + v;
      end
      else
      begin
        // Rekursion in nächste Dimension
        if Result <> '' then Result := Result + sLineBreak;
        Result := Result + '[' + ArrayToTextRecursive(SubIndices, Dim+1) + ']';
      end;
    end;
  end;

var
  DimCount: Integer;
begin
  Result := '';
  if not Assigned(AIBArrayField) then Exit;

  DimCount := Length(AIBArrayField.ArrayBounds);
  if DimCount = 0 then
  begin
    Result := 'Array';
    Exit;
  end;

  Result := ArrayToTextRecursive([], 0);
end;

end.

