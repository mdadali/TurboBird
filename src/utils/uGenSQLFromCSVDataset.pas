unit uGenSQLFromCSVDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, csvdataset;

type
  TFBFieldInfo = record
    FieldName: string;
    FieldType: string;
  end;

  TFBFieldInfoArray = array of TFBFieldInfo;

  TGenSQLFromCSVDataset = class
  private
    FDataSet: TDataSet;
    FTableName: string;
    FDefaultFieldLength: Integer;
    FSQL: string;
    FFields: TFBFieldInfoArray;

    function GetFieldTypeByString(const AStr: string): TFieldType;
    function DetectFieldType(const FieldName: string): TFieldType;  // nur noch eine Zeile
    function FieldTypeToFirebird(AType: TFieldType; MaxLength: Integer): string;
    procedure AnalyzeFields;
    procedure GenerateSQL;
  public
    constructor Create(ADataSet: TDataSet; const ATableName: string; ADefaultFieldLength: Integer);
    property SQL: string read FSQL;
    property Fields: TFBFieldInfoArray read FFields;
  end;

implementation

uses
  StrUtils, TypInfo, turbocommon;   // CSVFirstLineAsFieldNames / CSVDefaultFieldLength

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

// ---------------------------------------------------------------
//  NEU: Nur eine einzige Zeile analysieren!
// ---------------------------------------------------------------
function TGenSQLFromCSVDataset.DetectFieldType(const FieldName: string): TFieldType;
var
  SavedPos: TBookmark;
  Value: string;
begin
  Result := ftString;   // Fallback
  if not FDataSet.Active then Exit;

  SavedPos := FDataSet.GetBookmark;
  try
    // Auf die erste Datenzeile springen
    FDataSet.First;
    if (FDataSet is TCSVDataset) and
       TCSVDataset(FDataSet).CSVOptions.FirstLineAsFieldNames then
    begin
      // Erste Zeile enthält Feldnamen → zweite Zeile ist die erste Datenzeile
      if not FDataSet.EOF then
        FDataSet.Next;
    end;

    if not FDataSet.EOF then
    begin
      Value := FDataSet.FieldByName(FieldName).AsString;
      Result := GetFieldTypeByString(Value);
    end;
  finally
    if FDataSet.BookmarkValid(SavedPos) then
      FDataSet.GotoBookmark(SavedPos);
    FDataSet.FreeBookmark(SavedPos);
  end;
end;

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
    else
      fName := 'Column' + IntToStr(i + 1);

    // Nur EINE Zeile ansehen
    DetectedType := DetectFieldType(fName);

    SetLength(FFields, i + 1);
    FFields[i].FieldName := fName;

    // VARCHAR-Länge direkt aus der globalen Einstellung
    if DetectedType in [ftString, ftGuid, ftMemo, ftFmtMemo, ftWideString] then
      FFields[i].FieldType := FieldTypeToFirebird(DetectedType, CSVDefaultFieldLength)
    else
      FFields[i].FieldType := FieldTypeToFirebird(DetectedType, 0);
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
