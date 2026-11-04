unit uGenSQLFromCSVDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, csvdataset, fbcommon;

// ============================================================
//  Robuste DateTime-Erkennung (öffentlich, unit-weit nutzbar)
//
//  Wird von TGenSQLFromCSVDataset für die Typ-Erkennung UND
//  von TfrmCreateTableFromDataSet beim INSERT genutzt.
// ============================================================
function TryParseDateTimeEx(const S: string; out DT: TDateTime): Boolean;

type
  { Feld-Info für CSV-Dataset-Felder (NICHT Firebird-Metadaten!).
    Enthält den fertigen Firebird-Typ-String für die SQL-Generierung. }
  TCSVDataSetField = record
    FieldName: string;       // Feldname (aus CSV-Header oder ColumnN)
    FieldType: string;       // Firebird-Typ als String, z.B. 'VARCHAR(50)'
    Detected: Boolean;       // True = Typ automatisch erkannt, False = Fallback
    MaxLength: Integer;      // Erkannte max. Länge (für String-Typen)
  end;

  TCSVDataSetFieldArray = array of TCSVDataSetField;

TGenSQLFromCSVDataset = class
  private
    const
      ANALYZE_ROWS = 100;   // Erste N Zeilen analysieren (schnell)
  private
    FDataSet: TDataSet;
    FTableName: string;
    FDefaultFieldLength: Integer;
    FSQL: string;
    FFields: TCSVDataSetFieldArray;

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
    property Fields: TCSVDataSetFieldArray read FFields;
  end;

implementation

uses
  StrUtils, turbocommon;

// ================================================================
//  TryParseDateTimeEx
//
//  Robust gegen:
//    - User-Locale (deutsch: "15.01.2024 10:30:00")
//    - ISO-Format   ("2024-01-15 10:30:00")
//    - ISO mit ms   ("2024-01-15 10:30:00.000")
//    - ISO mit 4+ Stellen ms ("2024-01-15 10:30:00.0000")
//    - ISO mit 1,2 Stellen ms  (wird auf 3 Stellen gepadded)
//
//  Reihenfolge:
//    1) User-Locale versuchen
//    2) ISO-Format (Punkt als Dezimaltrenner) versuchen
//    3) Wenn Bruchteil ≠ 3 Stellen hat: Bruchteil manuell abschneiden,
//       ohne Bruchteil parsen, ms addieren
// ================================================================
function TryParseDateTimeEx(const S: string; out DT: TDateTime): Boolean;
var
  FS: TFormatSettings;
  TestStr, MsStr: string;
  DotPos, ColonPos, MsLen, MsVal: Integer;
begin
  Result := False;
  DT := 0;
  if Trim(S) = '' then Exit;

  // ---- 1) Standard (User-Locale) ----
  if TryStrToDateTime(S, DT) then
    Exit(True);

  // ---- 2) ISO-Format mit Punkt als ms-Trenner ----
  FS := DefaultFormatSettings;
  FS.DateSeparator      := '-';
  FS.ShortDateFormat    := 'yyyy-mm-dd';
  FS.LongDateFormat     := 'yyyy-mm-dd';
  FS.TimeSeparator      := ':';
  FS.LongTimeFormat     := 'hh:nn:ss';
  FS.DecimalSeparator   := '.';
  FS.ThousandSeparator  := #0;

  if TryStrToDateTime(S, DT, FS) then
    Exit(True);

  // ---- 3) Bruchteil mit ≠ 3 Stellen (z.B. '.0000') ----
  //      Nur wenn der Punkt NACH dem ersten Doppelpunkt steht
  //      (= ms-Trenner, kein Datumstrenner).
  ColonPos := Pos(':', S);
  DotPos   := Pos('.', S);

  if (DotPos > 0) and (ColonPos > 0) and (DotPos > ColonPos) then
  begin
    TestStr := Copy(S, 1, DotPos - 1);          // ohne Bruchteil
    if TryStrToDateTime(TestStr, DT, FS) then
    begin
      MsStr := Copy(S, DotPos + 1, MaxInt);     // Bruchteil
      MsLen := Length(MsStr);

      if MsLen > 0 then
      begin
        // Auf 3 Stellen normalisieren (pad/truncate)
        if MsLen >= 3 then
          MsStr := Copy(MsStr, 1, 3)
        else
          MsStr := MsStr + StringOfChar('0', 3 - MsLen);

        if TryStrToInt(MsStr, MsVal) then
          DT := DT + MsVal / MSecsPerDay;       // ms addieren
      end;

      Exit(True);
    end;
  end;
end;

// ================================================================
//  TGenSQLFromCSVDataset
// ================================================================

constructor TGenSQLFromCSVDataset.Create(
  ADataSet: TDataSet;
  const ATableName: string;
  ADefaultFieldLength: Integer);
begin
  inherited Create;
  FDataSet            := ADataSet;
  FTableName          := ATableName;
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
//
//  WICHTIG: Keine Bereichsprüfung für SMALLINT/INTEGER/BIGINT.
//           Alle Ganzzahlen → INTEGER
// ---------------------------------------------------------------
procedure TGenSQLFromCSVDataset.DetectFieldTypeAndLength(
  const FieldName: string;
  out FieldType: TFieldType;
  out MaxLen: Integer);
const
  FB_INT_MIN = -2147483648;
  FB_INT_MAX =  2147483647;
  FB_BIG_MIN = Int64(-9223372036854775807) - 1;
  FB_BIG_MAX = Int64( 9223372036854775807);
var
  RowIdx: Integer;
  Value: string;
  CandidateBool, CandidateInt: Boolean;
  CandidateIntFits32: Boolean;         // NEU: passt es in Firebird INTEGER?
  CandidateIntFits64: Boolean;         // NEU: passt es in Firebird BIGINT?
  CandidateFloat: Boolean;
  CandidateDate, CandidateDateTime, CandidateGuid: Boolean;
  IntVal64: Int64;
  FloatVal: Double;
  DateVal: TDateTime;
  AllEmpty: Boolean;
  FSFloat, FSDate: TFormatSettings;
begin
  FieldType := ftString;
  MaxLen    := 0;

  CandidateBool       := True;
  CandidateInt        := True;
  CandidateIntFits32  := True;
  CandidateIntFits64  := True;
  CandidateFloat      := True;
  CandidateDate       := True;
  CandidateDateTime   := True;
  CandidateGuid       := True;
  AllEmpty            := True;

  // FormatSettings wie gehabt
  FSFloat := DefaultFormatSettings;
  FSFloat.DecimalSeparator  := '.';
  FSFloat.ThousandSeparator := #0;

  FSDate := DefaultFormatSettings;
  FSDate.DateSeparator     := '-';
  FSDate.ShortDateFormat   := 'yyyy-mm-dd';
  FSDate.LongDateFormat    := 'yyyy-mm-dd';
  FSDate.TimeSeparator     := ':';
  FSDate.LongTimeFormat    := 'hh:nn:ss';
  FSDate.DecimalSeparator  := '.';
  FSDate.ThousandSeparator := #0;

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
               or SameText(Value, 'yes')  or SameText(Value, 'no')
               or (Value = '0') or (Value = '1')) then
            CandidateBool := False;

        // --- Int ---
        if CandidateInt then
        begin
          if TryStrToInt64(Value, IntVal64) then
          begin
            // NEU: Bereichsprüfung nach Firebird-Zieltyp
            if (IntVal64 < FB_INT_MIN) or (IntVal64 > FB_INT_MAX) then
              CandidateIntFits32 := False;
            if (IntVal64 < FB_BIG_MIN) or (IntVal64 > FB_BIG_MAX) then
              CandidateIntFits64 := False;
          end
          else
            CandidateInt := False;
        end;

        // --- Float (Locale-unabhängig) ---
        if CandidateFloat and (not CandidateInt) then
        begin
          if not TryStrToFloat(Value, FloatVal, FSFloat) then
            if not TryStrToFloat(Value, FloatVal) then
              CandidateFloat := False;
        end;

        // --- Date (kein ':', kein Space) ---
        if CandidateDate then
        begin
          if (Pos(':', Value) > 0) or (Pos(' ', Value) > 0) then
            CandidateDate := False
          else if not (TryStrToDate(Value, DateVal, FSDate)
                    or TryStrToDate(Value, DateVal)) then
            CandidateDate := False;
        end;

        // --- DateTime (mit ':') ---
        if CandidateDateTime then
        begin
          if Pos(':', Value) = 0 then
            CandidateDateTime := False
          else if not TryParseDateTimeEx(Value, DateVal) then
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
    MaxLen    := FDefaultFieldLength;
    Exit;
  end;

  // ============================================================
  // Priorität — mit korrekter Integer-Stufung
  //
  //   passt in INTEGER (32-bit)  → ftInteger  → 'INTEGER'
  //   passt nur in BIGINT (64-bit) → ftLargeint → 'BIGINT'
  //   passt in keins von beiden  → ftFloat (oder ftString)
  // ============================================================
  if CandidateBool then
    FieldType := ftBoolean
  else if CandidateInt and CandidateIntFits32 then
    FieldType := ftInteger
  else if CandidateInt and CandidateIntFits64 then
    FieldType := ftLargeint
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
    ftSmallint:  Result := 'INTEGER';       // vereinfacht
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
