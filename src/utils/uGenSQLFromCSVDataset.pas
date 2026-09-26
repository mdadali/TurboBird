unit uGenSQLFromCSVDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, csvdataset;

type
  TFBFieldInfo = record
    FieldName: string;
    FieldType: string;
    Detected: Boolean;
    MaxLength: Integer;
  end;

  TFBFieldInfoArray = array of TFBFieldInfo;

  TGenSQLFromCSVDataset = class
  private
    const
      ANALYZE_ROWS = 100;   // Erste N Zeilen analysieren (schnell)
  private
    FDataSet: TDataSet;
    FTableName: string;
    FDefaultFieldLength: Integer;
    FSQL: string;
    FFields: TFBFieldInfoArray;

    procedure AnalyzeFields;
    procedure GenerateSQL;
    procedure DetectFieldTypeAndLength(const FieldName: string;
      out FieldType: TFieldType; out MaxLen: Integer);
    function FieldTypeToFirebird(AType: TFieldType; MaxLength: Integer): string;
    function RoundUpLength(Len: Integer): Integer;
  public
    constructor Create(ADataSet: TDataSet; const ATableName: string;
      ADefaultFieldLength: Integer);
    property SQL: string read FSQL;
    property Fields: TFBFieldInfoArray read FFields;
  end;

implementation

uses
  StrUtils, turbocommon;

constructor TGenSQLFromCSVDataset.Create(
  ADataSet: TDataSet;
  const ATableName: string;
  ADefaultFieldLength: Integer);
begin
  inherited Create;
  FDataSet := ADataSet;
  FTableName := ATableName;
  FDefaultFieldLength := ADefaultFieldLength;
  GenerateSQL;
end;

// ---------------------------------------------------------------
//  Rundet eine Länge auf die nächste sinnvolle VARCHAR-Größe
// ---------------------------------------------------------------
function TGenSQLFromCSVDataset.RoundUpLength(Len: Integer): Integer;
begin
  if Len <= 0 then Result := 10
  else if Len <= 10 then Result := 10
  else if Len <= 25 then Result := 25
  else if Len <= 50 then Result := 50
  else if Len <= 100 then Result := 100
  else if Len <= 255 then Result := 255
  else if Len <= 500 then Result := 500
  else if Len <= 1000 then Result := 1000
  else if Len <= 2000 then Result := 2000
  else if Len <= 4000 then Result := 4000
  else if Len <= 8000 then Result := 8000
  else Result := 32765;
end;

// ---------------------------------------------------------------
//  Analysiert MEHRERE Zeilen für bessere Typ-/Längenerkennung
//  WICHTIG: Keine Bereichsprüfung für SMALLINT/INTEGER/BIGINT.
//           Alle Ganzzahlen → INTEGER
// ---------------------------------------------------------------
procedure TGenSQLFromCSVDataset.DetectFieldTypeAndLength(
  const FieldName: string;
  out FieldType: TFieldType;
  out MaxLen: Integer);
var
  RowIdx: Integer;
  Value: string;
  CandidateBool, CandidateInt, CandidateFloat: Boolean;
  CandidateDate, CandidateDateTime, CandidateGuid: Boolean;
  IntVal64: Int64;
  FloatVal: Double;
  DateVal: TDateTime;
  AllEmpty: Boolean;
  FS: TFormatSettings;         // ← NEU: für Locale-unabhängige Float-Erkennung
begin
  FieldType := ftString;
  MaxLen := 0;

  CandidateBool      := True;
  CandidateInt       := True;
  CandidateFloat     := True;
  CandidateDate      := True;
  CandidateDateTime  := True;
  CandidateGuid      := True;
  AllEmpty           := True;

  // Locale-unabhängig: Punkt als Dezimaltrenner
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := #0;

  FDataSet.DisableControls;
  try
    FDataSet.First;
    RowIdx := 0;
    while (not FDataSet.EOF) and (RowIdx < ANALYZE_ROWS) do
    begin
      Value := Trim(FDataSet.FieldByName(FieldName).AsString);

      if Value <> '' then
      begin
        AllEmpty := False;

        if Length(Value) > MaxLen then
          MaxLen := Length(Value);

        // --- Bool ---
        if CandidateBool then
          if not (SameText(Value, 'true') or SameText(Value, 'false')
               or SameText(Value, 'yes') or SameText(Value, 'no')
               or (Value = '0') or (Value = '1')) then
            CandidateBool := False;

        // --- Int ---
        if CandidateInt then
          if not TryStrToInt64(Value, IntVal64) then
            CandidateInt := False;

        // --- Float (LOCALE-UNABHÄNGIG) ---
        if CandidateFloat and (not CandidateInt) then
        begin
          // Versuch 1: Mit Punkt als Dezimaltrenner
          if not TryStrToFloat(Value, FloatVal, FS) then
          begin
            // Versuch 2: Mit System-Locale (falls Komma)
            if not TryStrToFloat(Value, FloatVal) then
              CandidateFloat := False;
          end;
        end;

        // --- Date (kein ':', kein Space) ---
        if CandidateDate then
        begin
          if (Pos(':', Value) > 0) or (Pos(' ', Value) > 0) then
            CandidateDate := False
          else if not TryStrToDate(Value, DateVal) then
            CandidateDate := False;
        end;

        // --- DateTime (mit ':') ---
        if CandidateDateTime then
        begin
          if Pos(':', Value) = 0 then
            CandidateDateTime := False
          else if not TryStrToDateTime(Value, DateVal) then
            CandidateDateTime := False;
        end;

        // --- GUID ---
        if CandidateGuid then
          if not ((Length(Value) = 36) and (Pos('-', Value) > 0)) then
            CandidateGuid := False;
      end;

      FDataSet.Next;
      Inc(RowIdx);
    end;
  finally
    FDataSet.First;
    FDataSet.EnableControls;
  end;

  if AllEmpty then
  begin
    FieldType := ftString;
    MaxLen := FDefaultFieldLength;
    Exit;
  end;

  // Priorität
  if CandidateBool then
    FieldType := ftBoolean
  else if CandidateInt then
    FieldType := ftInteger
  else if CandidateFloat then
    FieldType := ftFloat
  else if CandidateDateTime then
    FieldType := ftDateTime
  else if CandidateDate then
    FieldType := ftDate
  else if CandidateGuid then
    FieldType := ftGuid
  else
    FieldType := ftString;

  if FieldType in [ftString, ftFixedChar, ftWideString] then
    MaxLen := RoundUpLength(MaxLen);
end;

// ---------------------------------------------------------------
//  Firebird-Typ aus TFieldType
// ---------------------------------------------------------------
function TGenSQLFromCSVDataset.FieldTypeToFirebird(
  AType: TFieldType;
  MaxLength: Integer): string;
begin
  case AType of
    ftSmallint:  Result := 'INTEGER';       // ← vereinfacht
    ftInteger:   Result := 'INTEGER';
    ftLargeint:  Result := 'BIGINT';
    ftFloat:     Result := 'DOUBLE PRECISION';
    ftCurrency:  Result := 'DECIMAL(18,2)';
    ftBoolean:   Result := 'SMALLINT';
    ftDate:      Result := 'DATE';
    ftDateTime, ftTimeStamp: Result := 'TIMESTAMP';
    ftGuid:      Result := 'CHAR(36)';
  else
    begin
      if MaxLength <= 0 then
        MaxLength := FDefaultFieldLength;
      Result := 'VARCHAR(' + IntToStr(MaxLength) + ')';
    end;
  end;
end;

// ---------------------------------------------------------------
//  Felder analysieren und FFields befüllen
// ---------------------------------------------------------------
procedure TGenSQLFromCSVDataset.AnalyzeFields;
var
  i: Integer;
  Field: TField;
  fName: string;
  DetectedType: TFieldType;
  DetectedLen: Integer;
  UseHeader: Boolean;
begin
  SetLength(FFields, 0);
  if not FDataSet.Active then Exit;

  UseHeader := (FDataSet is TCSVDataset) and
               TCSVDataset(FDataSet).CSVOptions.FirstLineAsFieldNames;

  for i := 0 to FDataSet.FieldCount - 1 do
  begin
    Field := FDataSet.Fields[i];

    if UseHeader then
      fName := Field.FieldName
    else if FDataSet is TCSVDataset then
      fName := 'Column' + IntToStr(i + 1)
    else
      fName := Field.FieldName;

    DetectFieldTypeAndLength(fName, DetectedType, DetectedLen);

    SetLength(FFields, i + 1);
    FFields[i].FieldName := fName;
    FFields[i].FieldType := FieldTypeToFirebird(DetectedType, DetectedLen);
    FFields[i].Detected  := DetectedType <> ftString;
    FFields[i].MaxLength := DetectedLen;
  end;
end;

// ---------------------------------------------------------------
//  SQL generieren
// ---------------------------------------------------------------
procedure TGenSQLFromCSVDataset.GenerateSQL;
var
  i: Integer;
  SL: TStringList;
begin
  AnalyzeFields;

  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + FTableName + ' (');
    for i := 0 to High(FFields) do
    begin
      SL.Add('  ' + FFields[i].FieldName + ' ' + FFields[i].FieldType +
             IfThen(i < High(FFields), ',', ''));
    end;
    SL.Add(');');
    FSQL := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
