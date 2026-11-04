unit uGenSQLFromCSVDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, csvdataset, fbcommon;

const
  { Wie viele Zeilen maximal analysiert werden.
    Bei mehr Zeilen gilt der Scan als "Sampling" → Margin aktiv. }
  CSV_ANALYZE_ROWS = 100;

  { Max. VARCHAR-Länge in Firebird }
  CSV_MAX_VARCHAR = 32765;

type
  TCSVDataSetField = record
    FieldName:     string;
    FieldType:     string;
    Detected:      Boolean;
    MaxLength:     Integer;
    MarginApplied: Boolean;   // Sicherheits-Margin wurde angewandt
  end;

  TCSVDataSetFieldArray = array of TCSVDataSetField;

TGenSQLFromCSVDataset = class
  private
    FDataSet: TDataSet;
    FTableName: string;
    FDefaultFieldLength: Integer;
    FSQL: string;
    FFields: TCSVDataSetFieldArray;

    FSamplingPartial:   Boolean;   // nur erste N Zeilen analysiert
    FAnyMarginApplied:  Boolean;   // mind. 1 Feld wurde vergrößert
    FTotalRows:         Integer;   // RecordCount des Datasets
    FTotalRowsAnalyzed: Integer;   // tatsächlich gelesene Zeilen

    procedure AnalyzeFields;
    procedure GenerateSQL;
    procedure DetectFieldTypeAndLength(const FieldName: string;
      out FieldType: TFieldType; out MaxLen: Integer);
    function  FieldTypeToFirebird(AType: TFieldType; MaxLength: Integer): string;
    function  RoundUpLength(Len: Integer): Integer;
  public
    constructor Create(ADataSet: TDataSet; const ATableName: string;
      ADefaultFieldLength: Integer);
    property SQL: string read FSQL;
    property Fields: TCSVDataSetFieldArray read FFields;

    // NEU: Info über Sampling-Zustand
    property SamplingPartial:   Boolean read FSamplingPartial;
    property AnyMarginApplied:  Boolean read FAnyMarginApplied;
    property TotalRows:         Integer read FTotalRows;
    property TotalRowsAnalyzed: Integer read FTotalRowsAnalyzed;
  end;

function TryParseDateTimeEx(const S: string; out DT: TDateTime): Boolean;

implementation

uses
  StrUtils, Math, turbocommon;

// ================================================================
//  TryParseDateTimeEx
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

  if TryStrToDateTime(S, DT) then Exit(True);

  FS := DefaultFormatSettings;
  FS.DateSeparator      := '-';
  FS.ShortDateFormat    := 'yyyy-mm-dd';
  FS.LongDateFormat     := 'yyyy-mm-dd';
  FS.TimeSeparator      := ':';
  FS.LongTimeFormat     := 'hh:nn:ss';
  FS.DecimalSeparator   := '.';
  FS.ThousandSeparator  := #0;

  if TryStrToDateTime(S, DT, FS) then Exit(True);

  ColonPos := Pos(':', S);
  DotPos   := Pos('.', S);
  if (DotPos > 0) and (ColonPos > 0) and (DotPos > ColonPos) then
  begin
    TestStr := Copy(S, 1, DotPos - 1);
    if TryStrToDateTime(TestStr, DT, FS) then
    begin
      MsStr := Copy(S, DotPos + 1, MaxInt);
      MsLen := Length(MsStr);
      if MsLen > 0 then
      begin
        if MsLen >= 3 then
          MsStr := Copy(MsStr, 1, 3)
        else
          MsStr := MsStr + StringOfChar('0', 3 - MsLen);
        if TryStrToInt(MsStr, MsVal) then
          DT := DT + MsVal / MSecsPerDay;
      end;
      Exit(True);
    end;
  end;
end;

// ================================================================
//  Helper
// ================================================================
function QuickIsAllDigits(const S: string): Boolean;
var
  I, Start: Integer;
begin
  if S = '' then Exit(False);
  Start := 1;
  if (S[1] = '+') or (S[1] = '-') then Start := 2;
  if Start > Length(S) then Exit(False);
  for I := Start to Length(S) do
    if not (S[I] in ['0'..'9']) then Exit(False);
  Result := True;
end;

function TryStrToFloatSafe(const S: string; out Value: Double): Boolean;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator  := '.';
  FS.ThousandSeparator := #0;
  if TryStrToFloat(S, Value, FS) then Exit(True);
  Result := TryStrToFloat(S, Value);
end;

function TryStrToDateSafe(const S: string; out Value: TDateTime): Boolean;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DateSeparator    := '-';
  FS.ShortDateFormat  := 'yyyy-mm-dd';
  FS.LongDateFormat   := 'yyyy-mm-dd';
  if TryStrToDate(S, Value, FS) then Exit(True);
  Result := TryStrToDate(S, Value);
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
  FSamplingPartial    := False;
  FAnyMarginApplied   := False;
  FTotalRows          := 0;
  FTotalRowsAnalyzed  := 0;
  GenerateSQL;
end;

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
  else Result := CSV_MAX_VARCHAR;
end;

procedure TGenSQLFromCSVDataset.DetectFieldTypeAndLength(
  const FieldName: string;
  out FieldType: TFieldType;
  out MaxLen: Integer);
const
  FB_INT_MIN = -2147483648;
  FB_INT_MAX =  2147483647;
var
  RowIdx: Integer;
  Value: string;
  CandidateBool, CandidateInt: Boolean;
  CandidateIntFits32, CandidateIntFits64, CandidateTooBig: Boolean;
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

  CandidateBool      := True;
  CandidateInt       := True;
  CandidateIntFits32 := True;
  CandidateIntFits64 := True;
  CandidateTooBig    := False;
  CandidateFloat     := True;
  CandidateDate      := True;
  CandidateDateTime  := True;
  CandidateGuid      := True;
  AllEmpty           := True;

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

  RowIdx := 0;
  FDataSet.DisableControls;
  try
    FDataSet.First;
    while (not FDataSet.EOF) and (RowIdx < CSV_ANALYZE_ROWS) do
    begin
      Value := Trim(FDataSet.FieldByName(FieldName).AsString);

      if Value <> '' then
      begin
        AllEmpty := False;
        if Length(Value) > MaxLen then
          MaxLen := Length(Value);

        if CandidateBool then
          if not (SameText(Value, 'true') or SameText(Value, 'false')
               or SameText(Value, 'yes')  or SameText(Value, 'no')
               or (Value = '0') or (Value = '1')) then
            CandidateBool := False;

        if CandidateInt then
        begin
          if TryStrToInt64(Value, IntVal64) then
          begin
            if (IntVal64 < FB_INT_MIN) or (IntVal64 > FB_INT_MAX) then
              CandidateIntFits32 := False;
          end
          else if QuickIsAllDigits(Value) then
          begin
            CandidateTooBig := True;
            CandidateIntFits64 := False;
          end
          else
            CandidateInt := False;
        end;

        if CandidateFloat and (not CandidateInt) then
          if not TryStrToFloatSafe(Value, FloatVal) then
            CandidateFloat := False;

        if CandidateDate then
        begin
          if (Pos(':', Value) > 0) or (Pos(' ', Value) > 0) then
            CandidateDate := False
          else if not TryStrToDateSafe(Value, DateVal) then
            CandidateDate := False;
        end;

        if CandidateDateTime then
        begin
          if Pos(':', Value) = 0 then
            CandidateDateTime := False
          else if not TryParseDateTimeEx(Value, DateVal) then
            CandidateDateTime := False;
        end;

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

  FTotalRowsAnalyzed := RowIdx;

  if AllEmpty then
  begin
    FieldType := ftString;
    MaxLen    := FDefaultFieldLength;
    Exit;
  end;

  if CandidateBool then
    FieldType := ftBoolean
  else if CandidateInt and CandidateIntFits32 then
    FieldType := ftInteger
  else if CandidateInt and CandidateIntFits64 then
    FieldType := ftLargeint
  else if CandidateTooBig then
    FieldType := ftFMTBcd
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

function TGenSQLFromCSVDataset.FieldTypeToFirebird(
  AType: TFieldType; MaxLength: Integer): string;
begin
  case AType of
    ftSmallint:  Result := 'INTEGER';
    ftInteger:   Result := 'INTEGER';
    ftLargeint:  Result := 'BIGINT';
    ftFloat:     Result := 'DOUBLE PRECISION';
    ftCurrency:  Result := 'DECIMAL(18,2)';
    ftFMTBcd:    Result := 'DECIMAL(38,0)';
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
  FSamplingPartial   := False;
  FAnyMarginApplied  := False;
  FTotalRows         := 0;
  FTotalRowsAnalyzed := 0;

  if not FDataSet.Active then Exit;

  // ============================================================
  // Sampling-Zustand ermitteln
  // ============================================================
  FTotalRows := FDataSet.RecordCount;
  if FTotalRows < 0 then FTotalRows := 0;

  // 0 (unbekannt) oder > 100 → Sampling, Sicherheits-Margin aktiv
  FSamplingPartial := (FTotalRows = 0) or (FTotalRows > CSV_ANALYZE_ROWS);

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
    FFields[i].FieldName     := fName;
    FFields[i].MarginApplied := False;

    // ============================================================
    // Sicherheits-Margin — nur bei Sampling
    //
    //  INTEGER (32-bit)  →  BIGINT (64-bit)
    //  VARCHAR(n)        →  VARCHAR(2n), cap 32765
    //
    //  Andere Typen (DATE, TIME, TIMESTAMP, FLOAT, BOOLEAN,
    //  DECIMAL) sind bereits maximal — kein Margin möglich.
    // ============================================================
    if FSamplingPartial then
    begin
      if DetectedType = ftInteger then
      begin
        DetectedType := ftLargeint;
        FFields[i].MarginApplied := True;
        FAnyMarginApplied := True;
      end;

      if DetectedType in [ftString, ftFixedChar, ftWideString] then
      begin
        DetectedLen := DetectedLen * 2;
        if DetectedLen > CSV_MAX_VARCHAR then
          DetectedLen := CSV_MAX_VARCHAR;
        FFields[i].MarginApplied := True;
        FAnyMarginApplied := True;
      end;
    end;

    FFields[i].FieldType := FieldTypeToFirebird(DetectedType, DetectedLen);
    FFields[i].Detected  := DetectedType <> ftString;
    FFields[i].MaxLength := DetectedLen;
  end;
end;

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
