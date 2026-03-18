unit uGenSQLFromCSVDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DB, csvdataset;

type
  TGenSQLFromCSVDataset = class
  private
    FDataSet: TDataSet;
    FTableName: string;
    FDefaultFieldLength: Integer;
    FSQL: string;

    function DetectFieldType(const FieldName: string): TFieldType;
    function GetFieldTypeByString(const AStr: string): TFieldType;
    function FieldTypeToFirebird(AType: TFieldType; MaxLength: Integer): string;

    procedure GenerateSQL;
  public
    constructor Create(ADataSet: TDataSet; const ATableName: string; ADefaultFieldLength: Integer);
    property SQL: string read FSQL;
  end;

implementation

uses
  StrUtils, TypInfo;

{ Constructor }

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

{ ============================= }
{ Type Detection From String }
{ ============================= }

function TGenSQLFromCSVDataset.GetFieldTypeByString(const AStr: string): TFieldType;
var
  i: Integer;
  d: Double;
  dt: TDateTime;
begin
  if AStr = '' then
    Exit(ftString);

  if SameText(AStr, 'true') or SameText(AStr, 'false') then
    Exit(ftBoolean);

  if TryStrToInt(AStr, i) then
  begin
    if (i >= Low(SmallInt)) and (i <= High(SmallInt)) then
      Exit(ftSmallint)
    else
      Exit(ftInteger);
  end;

  if TryStrToFloat(AStr, d) then
    Exit(ftFloat);

  if TryStrToDateTime(AStr, dt) then
    Exit(ftDateTime);

  if TryStrToDate(AStr, dt) then
    Exit(ftDate);

  if (Length(AStr) = 36) and (Pos('-', AStr) > 0) then
    Exit(ftGuid);

  Result := ftString;
end;

{ ============================= }
{ Analyze Entire Dataset Column }
{ ============================= }

function TGenSQLFromCSVDataset.DetectFieldType(const FieldName: string): TFieldType;
var
  SavedPos: TBookmark;
  CurrentType: TFieldType;
  ValueType: TFieldType;
  MaxLength: Integer;
begin
  Result := ftString;
  MaxLength := 0;

  if not FDataSet.Active then Exit;

  SavedPos := FDataSet.GetBookmark;
  try
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      CurrentType := GetFieldTypeByString(FDataSet.FieldByName(FieldName).AsString);

      if CurrentType > Result then
        Result := CurrentType;

      MaxLength := Max(MaxLength,
        Length(FDataSet.FieldByName(FieldName).AsString));

      FDataSet.Next;
    end;
  finally
    if FDataSet.BookmarkValid(SavedPos) then
      FDataSet.GotoBookmark(SavedPos);
    FDataSet.FreeBookmark(SavedPos);
  end;
end;

{ ============================= }
{ Firebird Mapping }
{ ============================= }

function TGenSQLFromCSVDataset.FieldTypeToFirebird(
  AType: TFieldType;
  MaxLength: Integer): string;
begin
  case AType of
    ftSmallint: Result := 'SMALLINT';
    ftInteger: Result := 'INTEGER';
    ftLargeint: Result := 'BIGINT';
    ftFloat: Result := 'DOUBLE PRECISION';
    ftCurrency: Result := 'DECIMAL(18,2)';
    ftBoolean: Result := 'SMALLINT';
    ftDate: Result := 'DATE';
    ftDateTime, ftTimeStamp: Result := 'TIMESTAMP';
    ftGuid: Result := 'CHAR(36)';
  else
    begin
      // 👉 Hier DefaultFieldLength nutzen
      if MaxLength <= 0 then
        MaxLength := FDefaultFieldLength;

      Result := 'VARCHAR(' + IntToStr(MaxLength) + ')';
    end;
  end;
end;

{ ============================= }
{ SQL Generator }
{ ============================= }
procedure TGenSQLFromCSVDataset.GenerateSQL;
var
  i: Integer;
  Field: TField;
  SL: TStringList;
  FieldName: string;
  UseHeader: Boolean;
  DetectedType: TFieldType;
  MaxLen: Integer;
begin
  if not FDataSet.Active then Exit;

  UseHeader := False;

  if FDataSet is TCSVDataset then
    UseHeader := TCSVDataset(FDataSet).CSVOptions.FirstLineAsFieldNames;

  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + FTableName + ' (');

    for i := 0 to FDataSet.FieldCount - 1 do
    begin
      Field := FDataSet.Fields[i];

      // Feldname bestimmen
      if UseHeader then
        FieldName := Field.FieldName
      else
        FieldName := 'Column' + IntToStr(i + 1);

      // Typ erkennen
      DetectedType := DetectFieldType(Field.FieldName);

      // Länge bestimmen (Fallback später)
      MaxLen := Field.Size;

      // JETZT richtiges Mapping benutzen
      SL.Add('  ' + FieldName + ' ' +
        FieldTypeToFirebird(DetectedType, MaxLen) +
        IfThen(i < FDataSet.FieldCount - 1, ',', ''));
    end;

    SL.Add(');');

    FSQL := SL.Text;

  finally
    SL.Free;
  end;
end;

{procedure TGenSQLFromCSVDataset.GenerateSQL;
var
  i: Integer;
  Field: TField;
  SL: TStringList;
  FieldName: string;
  UseHeader: Boolean;
begin
  if not FDataSet.Active then Exit;

  UseHeader := False;

  if FDataSet is TCSVDataset then
    UseHeader := TCSVDataset(FDataSet).CSVOptions.FirstLineAsFieldNames;

  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + FTableName + ' (');

    for i := 0 to FDataSet.FieldCount - 1 do
    begin
      Field := FDataSet.Fields[i];

      if UseHeader then
        FieldName := Field.FieldName
      else
        FieldName := 'Column' + IntToStr(i + 1);

      SL.Add('  ' + FieldName + ' VARCHAR(255)' +
        IfThen(i < FDataSet.FieldCount - 1, ',', ''));
    end;

    SL.Add(');');

    FSQL := SL.Text;

  finally
    SL.Free;
  end;
end;}

end.
