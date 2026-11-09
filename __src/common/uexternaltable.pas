unit uExternalTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  turbocommon;

type
  TExternalFieldInfo = record
    FieldName: string;
    FieldType: string;      // Firebird SQL Typ
    CharLength: Integer;    // für CHAR/VARCHAR
    FieldSize: Integer;
    IsProblemField: boolean;
    ProblemReason: string;  // z.B. 'BLOB not supported in external tables'
  end;

  TExternalFieldArray = array of TExternalFieldInfo;

  TExternalTableGenerator = class
  private
    FTableName: string;
    FExternalFileName: string;
    FFields: TExternalFieldArray;
    FWarnings: TStringList;
    function GetMaxRowLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Aus CSV-Dataset Felder analysieren (nutzt DEINE bestehende Logik!)
    procedure AnalyzeFromCSVDataset(ADataSet: TDataSet;
      ADefaultFieldLength: Integer);

    // Aus bestehender Firebird-Tabelle Felder holen (für Clone)
    procedure AnalyzeFromFBTable(ATableFields: TDataSet;
      const ATableName: string);

    // SQL generieren
    function GenerateCreateSQL: string;
    function GenerateInsertFromSourceSQL(const SourceTable: string): string;

    property Fields: TExternalFieldArray read FFields;
    property Warnings: TStringList read FWarnings;
    property TableName: string read FTableName write FTableName;
    property ExternalFileName: string read FExternalFileName write FExternalFileName;
  end;

implementation

uses
  StrUtils, Math;

constructor TExternalTableGenerator.Create;
begin
  inherited Create;
  FWarnings := TStringList.Create;
end;

destructor TExternalTableGenerator.Destroy;
begin
  FWarnings.Free;
  inherited;
end;

procedure TExternalTableGenerator.AnalyzeFromCSVDataset(
  ADataSet: TDataSet; ADefaultFieldLength: Integer);
var
  i, MaxLen: Integer;
  Field: TField;
  SavedPos: TBookmark;
  SampleValue: string;
begin
  SetLength(FFields, 0);
  FWarnings.Clear;

  if not ADataSet.Active then Exit;

  for i := 0 to ADataSet.FieldCount - 1 do
  begin
    Field := ADataSet.Fields[i];
    SetLength(FFields, Length(FFields) + 1);
    FFields[i].FieldName := Field.FieldName;

    // Max. Länge aus Daten ermitteln
    MaxLen := 0;
    SavedPos := ADataSet.GetBookmark;
    try
      ADataSet.First;
      while not ADataSet.EOF do
      begin
        SampleValue := Field.AsString;
        MaxLen := Max(MaxLen, Length(SampleValue));
        ADataSet.Next;
      end;
    finally
      if ADataSet.BookmarkValid(SavedPos) then
        ADataSet.GotoBookmark(SavedPos);
      ADataSet.FreeBookmark(SavedPos);
    end;

    // Sicherheitspuffer
    if MaxLen = 0 then
      MaxLen := ADefaultFieldLength
    else
      MaxLen := Min(MaxLen + 20, 32765);  // Firebird Limit

    // CHAR vs VARCHAR: CHAR für kurze Felder (< 100), sonst VARCHAR
    if MaxLen <= 100 then
      FFields[i].FieldType := 'CHAR(' + IntToStr(MaxLen) + ')'
    else
      FFields[i].FieldType := 'VARCHAR(' + IntToStr(MaxLen) + ')';

    FFields[i].CharLength := MaxLen;
    FFields[i].IsProblemField := false;
  end;
end;

procedure TExternalTableGenerator.AnalyzeFromFBTable(
  ATableFields: TDataSet; const ATableName: string);
var
  FieldName, FieldType: string;
  FieldLen, CharLen, FieldPrecision, FieldScale, FieldSubType, FieldTypeInt: Integer;
  i: Integer;
begin
  SetLength(FFields, 0);
  FWarnings.Clear;

  ATableFields.First;
  while not ATableFields.EOF do
  begin
    FieldName := Trim(ATableFields.FieldByName('FIELD_NAME').AsString);

    FieldTypeInt := ATableFields.FieldByName('FIELD_TYPE').AsInteger;
    FieldLen := ATableFields.FieldByName('FIELD_LENGTH').AsInteger;
    CharLen := ATableFields.FieldByName('CHAR_LEN').AsInteger;
    FieldPrecision := ATableFields.FieldByName('FIELD_PRECISION').AsInteger;
    FieldScale := ATableFields.FieldByName('FIELD_SCALE').AsInteger;
    FieldSubType := ATableFields.FieldByName('FIELD_SUB_TYPE').AsInteger;

    i := Length(FFields);
    SetLength(FFields, i + 1);
    FFields[i].FieldName := FieldName;
    FFields[i].FieldSize := FieldLen;
    FFields[i].CharLength := CharLen;
    FFields[i].IsProblemField := false;

    // Typ mit GetFBTypeName ermitteln
    FieldType := UpperCase(GetFBTypeName(
      FieldTypeInt,
      FieldSubType,
      FieldLen,
      FieldPrecision,
      FieldScale,
      '',
      CharLen
    ));

    // External-Table-spezifische Behandlung
    if Pos('BLOB', FieldType) > 0 then
    begin
      FFields[i].FieldType := 'CHAR(32765)';
      FFields[i].CharLength := 32765;
      FFields[i].IsProblemField := true;
      FFields[i].ProblemReason := 'BLOB → CHAR(32765)';
      FWarnings.Add('Column "' + FieldName + '" is BLOB type - converted to CHAR(32765).');
    end
    else if Pos('ARRAY', FieldType) > 0 then
    begin
      FFields[i].IsProblemField := true;
      FFields[i].ProblemReason := 'ARRAY type not supported in external tables';
      FWarnings.Add('Column "' + FieldName + '" is ARRAY - skipped.');
      SetLength(FFields, i);
      ATableFields.Next;
      Continue;
    end
    else if (Pos('INT128', FieldType) > 0) or (Pos('DECFLOAT', FieldType) > 0) then
    begin
      FFields[i].IsProblemField := true;
      FFields[i].ProblemReason := '128-bit types not supported in external tables';
      FWarnings.Add('Column "' + FieldName + '" is 128-bit type - skipped.');
      SetLength(FFields, i);
      ATableFields.Next;
      Continue;
    end
    else
    begin
      FFields[i].FieldType := FieldType;
      FFields[i].CharLength := CharLen;
      if CharLen = 0 then
        FFields[i].CharLength := FieldLen;
    end;

    ATableFields.Next;
  end;
end;

function TExternalTableGenerator.GetMaxRowLength: Integer;
var
  i, TotalLen: Integer;
begin
  TotalLen := 0;
  for i := 0 to High(FFields) do
  begin
    if not FFields[i].IsProblemField then
    begin
      if FFields[i].CharLength > 0 then
        Inc(TotalLen, FFields[i].CharLength)
      else
        Inc(TotalLen, FFields[i].FieldSize);
    end;
  end;
  Result := TotalLen;
end;

function TExternalTableGenerator.GenerateCreateSQL: string;
var
  SL: TStringList;
  i: Integer;
  MaxLen: Integer;
  HasFields: Boolean;
begin
  SL := TStringList.Create;
  try
    // Warnungen als Kommentar
    if FWarnings.Count > 0 then
    begin
      SL.Add('/* WARNINGS:');
      for i := 0 to FWarnings.Count - 1 do
        SL.Add('  ' + FWarnings[i]);
      SL.Add('*/');
      SL.Add('');
    end;

    SL.Add('CREATE TABLE ' + FTableName + ' EXTERNAL FILE ''' +
           FExternalFileName + ''' (');

    HasFields := False;
    for i := 0 to High(FFields) do
    begin
      if FFields[i].IsProblemField then
        Continue;  // Problemfelder überspringen (ARRAY, INT128 etc.)

      HasFields := True;

      if i < High(FFields) then
        SL.Add('  ' + FFields[i].FieldName + ' ' + FFields[i].FieldType + ',')
      else
        SL.Add('  ' + FFields[i].FieldName + ' ' + FFields[i].FieldType);
    end;

    // Prüfen ob letzte Zeile ein Komma hat (falls letztes Feld Problemfeld war)
    if HasFields then
    begin
      i := SL.Count - 1;
      if (i >= 0) and (Length(SL[i]) > 0) and (SL[i][Length(SL[i])] = ',') then
        SL[i] := Copy(SL[i], 1, Length(SL[i]) - 1);  // Komma entfernen
    end;

    SL.Add(');');

    // Zeilenlänge prüfen
    MaxLen := GetMaxRowLength;
    if MaxLen > 32767 then
    begin
      SL.Insert(0, '/* WARNING: Row length ' + IntToStr(MaxLen) +
                      ' exceeds 32767 bytes limit! */');
      SL.Insert(1, '');
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TExternalTableGenerator.GenerateInsertFromSourceSQL(
  const SourceTable: string): string;
var
  SL: TStringList;
  i: Integer;
  FieldList: string;
begin
  SL := TStringList.Create;
  try
    FieldList := '';
    for i := 0 to High(FFields) do
    begin
      if not FFields[i].IsProblemField then
      begin
        if FieldList <> '' then
          FieldList := FieldList + ', ';
        FieldList := FieldList + FFields[i].FieldName;
      end;
    end;

    SL.Add('INSERT INTO ' + FTableName + ' (' + FieldList + ')');
    SL.Add('SELECT ' + FieldList);
    SL.Add('FROM ' + SourceTable + ';');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
