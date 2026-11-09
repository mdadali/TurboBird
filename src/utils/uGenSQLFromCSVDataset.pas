unit uGenSQLFromCSVDataset;

{$mode objfpc}{$H+}

{ ==========================================================================
  TGenSQLFromCSVDataset

  Generiert ein CREATE TABLE-Skript aus den Daten eines TDataSet.

  Firebird-Version wird über AFBVersionMajor gesteuert:
    1  = FB 1.0    (kein BIGINT)
    15 = FB 1.5    (+ BIGINT)
    2  = FB 2.0
    25 = FB 2.5
    3  = FB 3.0    (+ BOOLEAN)
    4  = FB 4.0    (+ INT128, DECFLOAT, TIME/TIMESTAMP WITH TIME ZONE, DECIMAL(38))
    5  = FB 5.0    (wie 4.0)
    6  = FB 6.0    (wie 4.0)

  Der Parameter AFBVersionMajor MUSS > 0 sein, sonst Exception.
  Wird ServerVersionMajor (= 6) direkt übergeben, funktioniert es.
  Wird Major*10+Minor (= 60) übergeben, funktioniert es auch (wird
  intern auf Major normalisiert).

  Sampling: Es werden max. CSV_ANALYZE_ROWS Zeilen analysiert.
  Bei mehr Zeilen wird eine Sicherheits-Margin angewandt
  (INTEGER→BIGINT, VARCHAR(n)→VARCHAR(2n)).
  ========================================================================== }

interface

uses
  Classes, SysUtils, DB, csvdataset, fbcommon;

const
  CSV_ANALYZE_ROWS = 100;
  CSV_MAX_VARCHAR  = 32765;

type
  TCSVDataSetField = record
    FieldName:     string;
    FieldType:     string;
    Detected:      Boolean;
    MaxLength:     Integer;
    MarginApplied: Boolean;
  end;

  TCSVDataSetFieldArray = array of TCSVDataSetField;

  TDetectedFBType = (
    dftUnknown,
    dftBoolean,
    dftInteger,
    dftBigInt,
    dftInt128,
    dftDecimal,
    dftFloat,
    dftDate,
    dftTime,
    dftTimeTZ,
    dftTimestamp,
    dftTimestampTZ,
    dftGuid,
    dftString
  );

  TGenSQLFromCSVDataset = class
  private
    FDataSet: TDataSet;
    FTableName: string;
    FDefaultFieldLength: Integer;
    FSQL: string;
    FFields: TCSVDataSetFieldArray;
    FFBVersionMajor: Integer;

    FSamplingPartial:   Boolean;
    FAnyMarginApplied:  Boolean;
    FTotalRows:         Integer;
    FTotalRowsAnalyzed: Integer;

    procedure AnalyzeFields;
    procedure GenerateSQL;
    procedure DetectFieldTypeAndLength(const FieldName: string;
      out DetectedType: TDetectedFBType;
      out MaxLen, IntDigits, DecDigits: Integer);
    function  DetectedTypeToFirebird(AType: TDetectedFBType;
      MaxLength, IntDigits, DecDigits: Integer): string;
    function  RoundUpLength(Len: Integer): Integer;
  public
    constructor Create(ADataSet: TDataSet; const ATableName: string;
      ADefaultFieldLength: Integer; AFBVersionMajor: Integer);
    property SQL: string read FSQL;
    property Fields: TCSVDataSetFieldArray read FFields;
    property FBVersionMajor: Integer read FFBVersionMajor;

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
//  Versions-Normalisierung
// ================================================================

{ Normalisiert Version auf Major-only.
  Unterstützt: 1, 15, 2, 25, 3, 30, 4, 40, 5, 50, 6, 60, 600
  Ausgabe:     1,  1, 2,  2, 3,  3, 4,  4, 5,  5, 6,  6,   6 }
function NormalizeToMajor(AVersion: Integer): Integer;
begin
  if AVersion <= 0 then
    Exit(0);

  if AVersion >= 100 then
    Result := AVersion div 100      // 600 → 6
  else if AVersion >= 10 then
    Result := AVersion div 10       // 60 → 6, 25 → 2
  else
    Result := AVersion;             // 6 → 6, 2 → 2

  if Result < 1 then
    Result := 1;
  if Result > 6 then
    Result := 6;
end;

function IsFBVersionAtLeast(AVersion, AMinimum: Integer): Boolean;
var
  NormVersion, NormMinimum: Integer;
begin
  NormVersion := NormalizeToMajor(AVersion);
  NormMinimum := NormalizeToMajor(AMinimum);
  Result := NormVersion >= NormMinimum;
end;

function FBVersionHasBoolean(AVersion: Integer): Boolean;
begin
  Result := IsFBVersionAtLeast(AVersion, 3);
end;

function FBVersionHasBigInt(AVersion: Integer): Boolean;
begin
  Result := IsFBVersionAtLeast(AVersion, 15);
end;

function FBVersionHasInt128(AVersion: Integer): Boolean;
begin
  Result := IsFBVersionAtLeast(AVersion, 4);
end;

function FBVersionHasTimeTZ(AVersion: Integer): Boolean;
begin
  Result := IsFBVersionAtLeast(AVersion, 4);
end;

function FBVersionHasTimestampTZ(AVersion: Integer): Boolean;
begin
  Result := IsFBVersionAtLeast(AVersion, 4);
end;

function FBVersionHasDecimal38(AVersion: Integer): Boolean;
begin
  Result := IsFBVersionAtLeast(AVersion, 4);
end;

// ================================================================
//  Helper: FormatSettings
// ================================================================
function MakeFS(const ADateSep: Char; const AShortDate, ALongTime: string): TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DateSeparator     := ADateSep;
  Result.ShortDateFormat   := AShortDate;
  Result.LongDateFormat    := AShortDate;
  Result.TimeSeparator     := ':';
  Result.LongTimeFormat    := ALongTime;
  Result.ShortTimeFormat   := 'hh:nn';
  Result.DecimalSeparator  := '.';
  Result.ThousandSeparator := #0;
end;

// ================================================================
//  LooksLikeDatePattern
// ================================================================
function LooksLikeDatePattern(const S: string): Boolean;
var
  i, Digits, Seps: Integer;
  HasColon, HasT: Boolean;
  C: Char;
begin
  Result := False;
  if (S = '') or (Length(S) < 4) or (Length(S) > 60) then Exit;

  Digits := 0;
  Seps := 0;
  HasColon := False;
  HasT := False;

  for i := 1 to Length(S) do
  begin
    C := S[i];
    if C in ['0'..'9'] then
      Inc(Digits)
    else if C in ['-', '.', '/'] then
      Inc(Seps)
    else if C = ':' then
      HasColon := True
    else if (C = 'T') or (C = 't') then
      HasT := True
    else if C in [' ', 'Z', 'z', '+'] then
      { ok }
    else if C in ['A'..'Z', 'a'..'z', '_', '/', ','] then
      { Zeitzonen-Name }
    else
      Exit;
  end;

  if Digits < 4 then Exit;
  if (Seps >= 1) and (Seps <= 2) then
    Result := True
  else if HasT then
    Result := True
  else if HasColon and (Digits >= 4) then
    Result := True;
end;

// ================================================================
//  Zeitzonen-Suffix
// ================================================================
function StripTimeZoneSuffix(const S: string; out HasTZ: Boolean): string;
var
  i, Len: Integer;
  T: string;
begin
  HasTZ := False;
  T := Trim(S);
  Len := Length(T);
  if Len = 0 then
  begin
    Result := T;
    Exit;
  end;

  if (T[Len] = 'Z') or (T[Len] = 'z') then
  begin
    HasTZ := True;
    Result := Trim(Copy(T, 1, Len - 1));
    Exit;
  end;

  for i := Len downto 2 do
  begin
    if (T[i] = '+') or (T[i] = '-') then
    begin
      if (i > 1) and ((T[i - 1] = ' ') or (i >= 10)) then
      begin
        if Pos(':', Copy(T, i, MaxInt)) > 0 then
        begin
          HasTZ := True;
          Result := Trim(Copy(T, 1, i - 1));
          Exit;
        end;
        if (Len - i >= 4) and
           (T[i + 1] in ['0'..'9']) and (T[i + 2] in ['0'..'9']) and
           (T[i + 3] in ['0'..'9']) and (T[i + 4] in ['0'..'9']) then
        begin
          HasTZ := True;
          Result := Trim(Copy(T, 1, i - 1));
          Exit;
        end;
      end;
    end;
  end;

  for i := Len downto 2 do
  begin
    if T[i] = ' ' then
    begin
      if (i < Len) and (T[i + 1] in ['A'..'Z', 'a'..'z']) then
      begin
        HasTZ := True;
        Result := Trim(Copy(T, 1, i - 1));
        Exit;
      end;
      Break;
    end;
  end;

  Result := T;
end;

// ================================================================
//  TryParseTimeOnly
// ================================================================
function TryParseTimeOnly(const S: string; out DT: TDateTime): Boolean;
var
  FS: TFormatSettings;
  Clean: string;
  HasTZ: Boolean;
  ColonCount, i: Integer;
begin
  Result := False;
  DT := 0;
  Clean := StripTimeZoneSuffix(Trim(S), HasTZ);
  if Clean = '' then Exit;

  ColonCount := 0;
  for i := 1 to Length(Clean) do
    if Clean[i] = ':' then Inc(ColonCount);
  if ColonCount < 1 then Exit;

  if (Pos('-', Clean) > 0) or (Pos('/', Clean) > 0) then Exit;

  FS := DefaultFormatSettings;
  FS.TimeSeparator    := ':';
  FS.LongTimeFormat   := 'hh:nn:ss';
  FS.ShortTimeFormat  := 'hh:nn';
  FS.DecimalSeparator := '.';

  if TryStrToTime(Clean, DT, FS) then Exit(True);
  if TryStrToTime(Clean, DT) then Exit(True);

  i := Pos('.', Clean);
  if (i > 0) and (ColonCount >= 2) then
  begin
    if TryStrToTime(Copy(Clean, 1, i - 1), DT, FS) then
      Exit(True);
  end;
end;

// ================================================================
//  TryParseDateOnly
//  Verlangt Datumstrenner (schützt vor "1" → 01.01.aktuelles Jahr)
// ================================================================
function TryParseDateOnly(const S: string; out DT: TDateTime): Boolean;
var
  FS: TFormatSettings;
  Clean: string;
  HasTZ, HasSep: Boolean;
  i: Integer;
begin
  Result := False;
  DT := 0;
  Clean := StripTimeZoneSuffix(Trim(S), HasTZ);
  if Clean = '' then Exit;

  if Pos(':', Clean) > 0 then Exit;
  if Length(Clean) < 6 then Exit;

  HasSep := False;
  for i := 1 to Length(Clean) do
    if Clean[i] in ['-', '.', '/'] then
    begin
      HasSep := True;
      Break;
    end;
  if not HasSep then Exit;

  if TryStrToDate(Clean, DT) then Exit(True);

  FS := MakeFS('-', 'yyyy-mm-dd', 'hh:nn:ss');
  if TryStrToDate(Clean, DT, FS) then Exit(True);

  FS := MakeFS('.', 'dd.mm.yyyy', 'hh:nn:ss');
  if TryStrToDate(Clean, DT, FS) then Exit(True);

  FS := MakeFS('/', 'dd/mm/yyyy', 'hh:nn:ss');
  if TryStrToDate(Clean, DT, FS) then Exit(True);

  FS := MakeFS('/', 'mm/dd/yyyy', 'hh:nn:ss');
  if TryStrToDate(Clean, DT, FS) then Exit(True);

  FS := MakeFS('/', 'yyyy/mm/dd', 'hh:nn:ss');
  if TryStrToDate(Clean, DT, FS) then Exit(True);

  FS := MakeFS('-', 'dd-mm-yyyy', 'hh:nn:ss');
  if TryStrToDate(Clean, DT, FS) then Exit(True);

  FS := MakeFS('.', 'yyyy.mm.dd', 'hh:nn:ss');
  if TryStrToDate(Clean, DT, FS) then Exit(True);
end;

// ================================================================
//  TryParseDateTimeEx
// ================================================================
function TryParseDateTimeEx(const S: string; out DT: TDateTime): Boolean;
var
  FS: TFormatSettings;
  Clean, TestStr, MsStr: string;
  DotPos, TPos, MsLen, MsVal, i, LastColon: Integer;
  HasTZ: Boolean;

  function TryWithFS(const AStr: string; const AFS: TFormatSettings): Boolean;
  begin
    Result := TryStrToDateTime(AStr, DT, AFS);
  end;

  function TryMsSuffix(const Base: string; const AFS: TFormatSettings): Boolean;
  var i: Integer;
  begin
    Result := False;
    DotPos := 0;
    LastColon := 0;
    for i := Length(Base) downto 1 do
      if Base[i] = ':' then
      begin
        LastColon := i;
        Break;
      end;
    if LastColon = 0 then Exit;
    for i := Length(Base) downto LastColon + 1 do
      if Base[i] = '.' then
      begin
        DotPos := i;
        Break;
      end;
    if DotPos = 0 then Exit;

    TestStr := Copy(Base, 1, DotPos - 1);
    if not TryStrToDateTime(TestStr, DT, AFS) then Exit;

    MsStr := Copy(Base, DotPos + 1, MaxInt);
    for i := 1 to Length(MsStr) do
      if not (MsStr[i] in ['0'..'9']) then
      begin
        MsStr := Copy(MsStr, 1, i - 1);
        Break;
      end;
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
    Result := True;
  end;

begin
  Result := False;
  DT := 0;
  Clean := Trim(S);
  if Clean = '' then Exit;

  TPos := Pos('T', Clean);
  if TPos = 0 then
    TPos := Pos('t', Clean);
  if TPos > 0 then
    Clean := Copy(Clean, 1, TPos - 1) + ' ' + Copy(Clean, TPos + 1, MaxInt);

  Clean := StripTimeZoneSuffix(Clean, HasTZ);
  if Clean = '' then Exit;

  if TryStrToDateTime(Clean, DT) then Exit(True);

  FS := MakeFS('-', 'yyyy-mm-dd', 'hh:nn:ss');
  if TryWithFS(Clean, FS) then Exit(True);
  if TryMsSuffix(Clean, FS) then Exit(True);

  FS := MakeFS('.', 'dd.mm.yyyy', 'hh:nn:ss');
  if TryWithFS(Clean, FS) then Exit(True);
  if TryMsSuffix(Clean, FS) then Exit(True);

  FS := MakeFS('/', 'dd/mm/yyyy', 'hh:nn:ss');
  if TryWithFS(Clean, FS) then Exit(True);
  if TryMsSuffix(Clean, FS) then Exit(True);

  FS := MakeFS('/', 'mm/dd/yyyy', 'hh:nn:ss');
  if TryWithFS(Clean, FS) then Exit(True);
  if TryMsSuffix(Clean, FS) then Exit(True);

  FS := MakeFS('/', 'yyyy/mm/dd', 'hh:nn:ss');
  if TryWithFS(Clean, FS) then Exit(True);
  if TryMsSuffix(Clean, FS) then Exit(True);

  FS := MakeFS('-', 'dd-mm-yyyy', 'hh:nn:ss');
  if TryWithFS(Clean, FS) then Exit(True);
  if TryMsSuffix(Clean, FS) then Exit(True);

  FS := MakeFS('.', 'yyyy.mm.dd', 'hh:nn:ss');
  if TryWithFS(Clean, FS) then Exit(True);
  if TryMsSuffix(Clean, FS) then Exit(True);
end;

// ================================================================
//  ParseDecimalParts
// ================================================================
procedure ParseDecimalParts(const S: string;
  out IntDigits, DecDigits: Integer);
var
  P, DotPos, I: Integer;
begin
  IntDigits := 0;
  DecDigits := 0;
  if S = '' then Exit;

  P := 1;
  if S[1] in ['+', '-'] then
    P := 2;
  if P > Length(S) then Exit;

  DotPos := 0;
  for I := P to Length(S) do
    if (S[I] = '.') or (S[I] = ',') then
    begin
      DotPos := I;
      Break;
    end;

  if DotPos = 0 then
  begin
    IntDigits := Length(S) - P + 1;
    while (IntDigits > 0) and (S[P] = '0') do
    begin
      Inc(P);
      Dec(IntDigits);
    end;
  end
  else
  begin
    IntDigits := DotPos - P;
    while (IntDigits > 0) and (S[P] = '0') do
    begin
      Inc(P);
      Dec(IntDigits);
    end;
    DecDigits := Length(S) - DotPos;
  end;
end;

// ================================================================
//  CountSignificantDigits
// ================================================================
function CountSignificantDigits(const S: string): Integer;
var
  I, Count: Integer;
  SeenNonZero: Boolean;
  C: Char;
begin
  SeenNonZero := False;
  Count := 0;
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if C in ['0'..'9'] then
    begin
      if (C <> '0') or SeenNonZero then
      begin
        SeenNonZero := True;
        Inc(Count);
      end;
    end
    else if (C = '.') or (C = ',') then
      Continue
    else if (C = 'e') or (C = 'E') then
      Break;
  end;
  Result := Count;
end;

// ================================================================
//  Klassifizierung
// ================================================================
type
  TValueKind = (
    vkEmpty,
    vkBool,
    vkInt,          // passt in INTEGER (32-bit)
    vkIntBig,       // passt in BIGINT, aber nicht INTEGER
    vkInt128,       // passt in INT128 (bis 39 Stellen), aber nicht BIGINT
    vkIntHuge,      // > 39 Stellen → String
    vkFloat,        // bis 15 signifikante Stellen → DOUBLE
    vkDecimal,      // > 15 sig. Stellen, aber ≤ 38 → DECIMAL(I,D)
    vkDate,
    vkTime,
    vkTimeTZ,
    vkTimestamp,
    vkTimestampTZ,
    vkGuid,
    vkString
  );

function ClassifyValue(const Value: string): TValueKind;
const
  FB_INT_MIN = -2147483648;
  FB_INT_MAX =  2147483647;
var
  S: string;
  IntVal64: Int64;
  FloatVal: Double;
  DT: TDateTime;
  HasTZ: Boolean;
  Stripped: string;
  ColonCount, i, SigDigits, IntD, DecD: Integer;
  FS: TFormatSettings;
  AllDigits: Boolean;
begin
  S := Trim(Value);
  if S = '' then
    Exit(vkEmpty);

  // ---- Boolean ----
  if SameText(S, 'true') or SameText(S, 'false')
     or SameText(S, 'yes') or SameText(S, 'no') then
    Exit(vkBool);

  // ---- GUID ----
  if (Length(S) = 36) and (Pos('-', S) > 0) then
  begin
    if (S[9] = '-') and (S[14] = '-') and (S[19] = '-') and (S[24] = '-') then
      Exit(vkGuid);
  end;

  ColonCount := 0;
  for i := 1 to Length(S) do
    if S[i] = ':' then Inc(ColonCount);

  Stripped := StripTimeZoneSuffix(S, HasTZ);

  // ---- Zeit / Timestamp ----
  if ColonCount >= 1 then
  begin
    if TryParseTimeOnly(Stripped, DT) then
    begin
      if HasTZ then Exit(vkTimeTZ) else Exit(vkTime);
    end;

    if TryParseDateTimeEx(S, DT) then
    begin
      if HasTZ then Exit(vkTimestampTZ) else Exit(vkTimestamp);
    end;
  end;

  // ---- Integer (VOR Date!) ----
  if TryStrToInt64(S, IntVal64) then
  begin
    if (IntVal64 >= FB_INT_MIN) and (IntVal64 <= FB_INT_MAX) then
      Exit(vkInt)
    else
      Exit(vkIntBig);
  end;

  // ---- Nur Ziffern, aber zu groß für Int64 → INT128 oder Huge ----
  if (S[1] in ['0'..'9', '+', '-']) then
  begin
    AllDigits := True;
    i := 1;
    if S[1] in ['+', '-'] then i := 2;
    while (i <= Length(S)) do
    begin
      if not (S[i] in ['0'..'9']) then
      begin
        AllDigits := False;
        Break;
      end;
      Inc(i);
    end;
    if AllDigits then
    begin
      if Length(S) <= 39 then
        Exit(vkInt128)
      else
        Exit(vkIntHuge);
    end;
  end;

  // ---- Reines Datum ----
  if (ColonCount = 0) and TryParseDateOnly(S, DT) then
    Exit(vkDate);

  // ---- Float oder Decimal ----
  if not LooksLikeDatePattern(S) then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator  := '.';
    FS.ThousandSeparator := #0;
    if TryStrToFloat(S, FloatVal, FS) or TryStrToFloat(S, FloatVal) then
    begin
      SigDigits := CountSignificantDigits(S);
      if SigDigits > 15 then
      begin
        ParseDecimalParts(S, IntD, DecD);
        if (IntD + DecD <= 38) and (DecD <= 18) then
          Exit(vkDecimal)
        else
          Exit(vkString);
      end
      else
        Exit(vkFloat);
    end;
  end;

  Result := vkString;
end;

// ================================================================
//  Quick helpers
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

// ================================================================
//  TGenSQLFromCSVDataset
// ================================================================

constructor TGenSQLFromCSVDataset.Create(
  ADataSet: TDataSet;
  const ATableName: string;
  ADefaultFieldLength: Integer;
  AFBVersionMajor: Integer);
begin
  inherited Create;

  if AFBVersionMajor <= 0 then
    raise Exception.Create(
      'TGenSQLFromCSVDataset: Firebird version is unknown (0). ' +
      'Please connect to the target database first, or pass a valid ' +
      'FB version (e.g. 6 for Firebird 6.0).');

  FDataSet            := ADataSet;
  FTableName          := ATableName;
  FDefaultFieldLength := ADefaultFieldLength;
  FFBVersionMajor     := AFBVersionMajor;
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
  out DetectedType: TDetectedFBType;
  out MaxLen, IntDigits, DecDigits: Integer);
var
  RowIdx: Integer;
  Value: string;
  Kind: TValueKind;
  AllEmpty: Boolean;

  CanBool, CanInt, CanBigInt, CanInt128, CanDecimal: Boolean;
  CanFloat: Boolean;
  CanDate, CanTime, CanTimeTZ: Boolean;
  CanTimestamp, CanTimestampTZ: Boolean;
  CanGuid: Boolean;

  SeenNonEmpty: Boolean;
  AnyTimeTZ, AnyTimestampTZ: Boolean;
  AnyPureTime, AnyTimestamp: Boolean;
  AnyDate, AnyInt128, AnyDecimal: Boolean;

  TmpIntD, TmpDecD: Integer;
begin
  DetectedType := dftString;
  MaxLen       := 0;
  IntDigits    := 0;
  DecDigits    := 0;

  CanBool        := True;
  CanInt         := True;
  CanBigInt      := True;
  CanInt128      := True;
  CanDecimal     := True;
  CanFloat       := True;
  CanDate        := True;
  CanTime        := True;
  CanTimeTZ      := True;
  CanTimestamp   := True;
  CanTimestampTZ := True;
  CanGuid        := True;

  AllEmpty         := True;
  SeenNonEmpty     := False;
  AnyTimeTZ        := False;
  AnyTimestampTZ   := False;
  AnyPureTime      := False;
  AnyTimestamp     := False;
  AnyDate          := False;
  AnyInt128        := False;
  AnyDecimal       := False;

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
        SeenNonEmpty := True;
        if Length(Value) > MaxLen then
          MaxLen := Length(Value);

        Kind := ClassifyValue(Value);

        case Kind of
          vkBool:
            begin
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkInt:
            begin
              CanBool := CanBool and ((Value = '0') or (Value = '1'));
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkIntBig:
            begin
              CanBool := False;
              CanInt := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkInt128:
            begin
              AnyInt128 := True;
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkIntHuge:
            begin
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkDecimal:
            begin
              AnyDecimal := True;
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
              ParseDecimalParts(Value, TmpIntD, TmpDecD);
              if TmpIntD > IntDigits then IntDigits := TmpIntD;
              if TmpDecD > DecDigits then DecDigits := TmpDecD;
            end;
          vkFloat:
            begin
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkDate:
            begin
              AnyDate := True;
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanTime := False;
              CanTimeTZ := False;
              CanGuid := False;
            end;
          vkTime:
            begin
              AnyPureTime := True;
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanDate := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkTimeTZ:
            begin
              AnyTimeTZ := True;
              AnyPureTime := True;
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanDate := False;
              CanTime := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
          vkTimestamp:
            begin
              AnyTimestamp := True;
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanGuid := False;
            end;
          vkTimestampTZ:
            begin
              AnyTimestampTZ := True;
              AnyTimestamp := True;
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanGuid := False;
            end;
          vkGuid:
            begin
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
            end;
          vkString:
            begin
              CanBool := False;
              CanInt := False;
              CanBigInt := False;
              CanInt128 := False;
              CanDecimal := False;
              CanFloat := False;
              CanDate := False;
              CanTime := False;
              CanTimeTZ := False;
              CanTimestamp := False;
              CanTimestampTZ := False;
              CanGuid := False;
            end;
        end;
      end;

      FDataSet.Next;
      Inc(RowIdx);
    end;
  finally
    FDataSet.First;
    FDataSet.EnableControls;
  end;

  FTotalRowsAnalyzed := RowIdx;

  if AllEmpty or (not SeenNonEmpty) then
  begin
    DetectedType := dftString;
    MaxLen := FDefaultFieldLength;
    Exit;
  end;

  if CanBool and not (CanInt or CanBigInt) then
    DetectedType := dftBoolean
  else if CanBool and CanInt then
    DetectedType := dftBoolean
  else if CanInt then
    DetectedType := dftInteger
  else if CanBigInt then
    DetectedType := dftBigInt
  else if CanInt128 or AnyInt128 then
    DetectedType := dftInt128
  else if CanDecimal and AnyDecimal then
    DetectedType := dftDecimal
  else if AnyTimestampTZ and CanTimestampTZ then
    DetectedType := dftTimestampTZ
  else if AnyTimestamp and CanTimestamp then
    DetectedType := dftTimestamp
  else if AnyTimeTZ and CanTimeTZ then
    DetectedType := dftTimeTZ
  else if AnyPureTime and CanTime then
    DetectedType := dftTime
  else if AnyDate and CanDate then
    DetectedType := dftDate
  else if CanFloat then
    DetectedType := dftFloat
  else if CanGuid then
    DetectedType := dftGuid
  else
    DetectedType := dftString;

  if DetectedType = dftString then
    MaxLen := RoundUpLength(MaxLen);
end;

function TGenSQLFromCSVDataset.DetectedTypeToFirebird(
  AType: TDetectedFBType;
  MaxLength, IntDigits, DecDigits: Integer): string;
begin
  case AType of
    dftBoolean:
      begin
        if FBVersionHasBoolean(FFBVersionMajor) then
          Result := 'BOOLEAN'
        else
          Result := 'SMALLINT';
      end;

    dftInteger:
      Result := 'INTEGER';

    dftBigInt:
      begin
        if FBVersionHasBigInt(FFBVersionMajor) then
          Result := 'BIGINT'
        else
          // FB 1.0 kennt kein BIGINT → INTEGER als Fallback
          Result := 'INTEGER';
      end;

    dftInt128:
      begin
        if FBVersionHasInt128(FFBVersionMajor) then
          Result := 'INT128'
        else if FBVersionHasDecimal38(FFBVersionMajor) then
        begin
          // Kein INT128, aber DECIMAL(38) verfügbar
          if IntDigits <= 38 then
            Result := 'DECIMAL(38,0)'
          else
            Result := 'VARCHAR(40)';
        end
        else
          Result := 'VARCHAR(40)';   // ältere FB: String
      end;

    dftDecimal:
      begin
        if FBVersionHasDecimal38(FFBVersionMajor) then
        begin
          if (IntDigits + DecDigits <= 38) and (DecDigits <= 18) then
            Result := Format('DECIMAL(%d,%d)', [IntDigits + DecDigits, DecDigits])
          else
            Result := 'VARCHAR(50)';
        end
        else if IsFBVersionAtLeast(FFBVersionMajor, 2) then
        begin
          // FB 2.x/3.x: DECIMAL(18,x) max
          if (IntDigits + DecDigits <= 18) and (DecDigits <= 18) then
            Result := Format('DECIMAL(%d,%d)', [IntDigits + DecDigits, DecDigits])
          else
            Result := 'VARCHAR(50)';
        end
        else
          Result := 'VARCHAR(50)';   // FB 1.x
      end;

    dftFloat:
      Result := 'DOUBLE PRECISION';

    dftDate:
      Result := 'DATE';

    dftTime:
      Result := 'TIME';

    dftTimeTZ:
      begin
        if FBVersionHasTimeTZ(FFBVersionMajor) then
          Result := 'TIME WITH TIME ZONE'
        else
          Result := 'VARCHAR(50)';
      end;

    dftTimestamp:
      Result := 'TIMESTAMP';

    dftTimestampTZ:
      begin
        if FBVersionHasTimestampTZ(FFBVersionMajor) then
          Result := 'TIMESTAMP WITH TIME ZONE'
        else
          Result := 'VARCHAR(60)';
      end;

    dftGuid:
      Result := 'CHAR(36)';

    dftString:
      begin
        if MaxLength <= 0 then
          MaxLength := FDefaultFieldLength;
        Result := 'VARCHAR(' + IntToStr(MaxLength) + ')';
      end;
  else
    Result := 'VARCHAR(' + IntToStr(MaxLength) + ')';
  end;
end;

procedure TGenSQLFromCSVDataset.AnalyzeFields;
var
  i: Integer;
  Field: TField;
  fName: string;
  DetectedType: TDetectedFBType;
  DetectedLen, IntD, DecD: Integer;
  UseHeader: Boolean;
begin
  SetLength(FFields, 0);
  FSamplingPartial   := False;
  FAnyMarginApplied  := False;
  FTotalRows         := 0;
  FTotalRowsAnalyzed := 0;

  if not FDataSet.Active then Exit;

  FTotalRows := FDataSet.RecordCount;
  if FTotalRows < 0 then FTotalRows := 0;

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

    DetectFieldTypeAndLength(fName, DetectedType, DetectedLen, IntD, DecD);

    SetLength(FFields, i + 1);
    FFields[i].FieldName     := fName;
    FFields[i].MarginApplied := False;

    if FSamplingPartial then
    begin
      if DetectedType = dftInteger then
      begin
        if FBVersionHasBigInt(FFBVersionMajor) then
        begin
          DetectedType := dftBigInt;
          FFields[i].MarginApplied := True;
          FAnyMarginApplied := True;
        end;
      end;

      if DetectedType = dftString then
      begin
        DetectedLen := DetectedLen * 2;
        if DetectedLen > CSV_MAX_VARCHAR then
          DetectedLen := CSV_MAX_VARCHAR;
        FFields[i].MarginApplied := True;
        FAnyMarginApplied := True;
      end;
    end;

    FFields[i].FieldType := DetectedTypeToFirebird(DetectedType,
      DetectedLen, IntD, DecD);
    FFields[i].Detected  := DetectedType <> dftString;
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
