unit CheckDBIntegrity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, IBDatabase, DB, IBQuery, DateUtils,
  turbocommon;

type
  // Erweiterte Result-Struktur mit besseren Metadaten
  TDBCheckResult = record
    Title: string;
    DBName: string;
    DateTimeChecked: TDateTime;
    DBServerVersion: string;
    TotalTables: Integer;
    TotalFields: Integer;
    CharsetIssues: TStringList;
    LengthIssues: TStringList;
    NotNullIssues: TStringList;
    DataTypeIssues: TStringList;
    PKIssues: TStringList;
    FKIssues: TStringList;
    ViewsIssues: TStringList;
    IndexUniqueIssues: TStringList;
    TriggerDefaultIssues: TStringList;
    // Neue: Zusammenfassung
    Summary: TStringList;
  end;

  // Interface für flexible Check-Erweiterung
  IDBCheck = interface
    procedure Execute(DB: TIBDatabase; var Result: TDBCheckResult);
    function GetName: string;
    function GetDescription: string;
  end;

  // Basis-Klasse für Checks
  TDBCheckBase = class(TInterfacedObject, IDBCheck)
  protected
    FName: string;
    FDescription: string;
  public
    constructor Create(const AName, ADescription: string);
    procedure Execute(DB: TIBDatabase; var Result: TDBCheckResult); virtual; abstract;
    function GetName: string;
    function GetDescription: string;
  end;

// Hauptfunktionen
procedure InitCheckResult(var Res: TDBCheckResult; const Title, DBName: string);
procedure FreeCheckResult(var Res: TDBCheckResult);
procedure CheckFieldsCharset(DB: TIBDatabase; CharsetIssues: TStringList);
procedure CheckFieldsLength(DB: TIBDatabase; LengthIssues: TStringList; MaxLength: Integer = 32765);
procedure CheckFieldsNotNull(DB: TIBDatabase; NotNullIssues: TStringList);
procedure CheckFieldsDataType(DB: TIBDatabase; DataTypeIssues: TStringList);
procedure CheckPrimaryKeys(DB: TIBDatabase; PKIssues: TStringList);
procedure CheckForeignKeys(DB: TIBDatabase; FKIssues: TStringList);
procedure CheckViews(DB: TIBDatabase; ViewsIssues: TStringList);
procedure CheckIndicesUnique(DB: TIBDatabase; IndexUniqueIssues: TStringList);
procedure CheckTriggerDefaults(DB: TIBDatabase; TriggerDefaultIssues: TStringList);

// Neue Hilfsfunktionen
function GetTableCount(DB: TIBDatabase): Integer;
function GetFieldCount(DB: TIBDatabase): Integer;
function GetDBServerVersion(DB: TIBDatabase): string;

implementation

uses
  StrUtils, Math;

{ TDBCheckBase }

constructor TDBCheckBase.Create(const AName, ADescription: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
end;

function TDBCheckBase.GetName: string;
begin
  Result := FName;
end;

function TDBCheckBase.GetDescription: string;
begin
  Result := FDescription;
end;

{ Hilfsfunktionen }

function GetTableCount(DB: TIBDatabase): Integer;
var
  Q: TIBQuery;
begin
  Result := 0;
  if not Assigned(DB) or not DB.Connected then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.SQL.Text := 'SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$SYSTEM_FLAG = 0 AND RDB$VIEW_BLR IS NULL';
    Q.Open;
    if not Q.EOF then
      Result := Q.Fields[0].AsInteger;
    Q.Close;
  finally
    Q.Free;
  end;
end;

function GetFieldCount(DB: TIBDatabase): Integer;
var
  Q: TIBQuery;
begin
  Result := 0;
  if not Assigned(DB) or not DB.Connected then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.SQL.Text := 'SELECT COUNT(*) FROM RDB$RELATION_FIELDS rf WHERE rf.RDB$SYSTEM_FLAG = 0';
    Q.Open;
    if not Q.EOF then
      Result := Q.Fields[0].AsInteger;
    Q.Close;
  finally
    Q.Free;
  end;
end;

function GetDBServerVersion(DB: TIBDatabase): string;
var
  Q: TIBQuery;
begin
  Result := 'Unknown';
  if not Assigned(DB) or not DB.Connected then Exit;

  Q := TIBQuery.Create(nil);
  try
    try
      Q.Database := DB;
      Q.SQL.Text := 'SELECT RDB$GET_CONTEXT(''SYSTEM'', ''ENGINE_VERSION'') FROM RDB$DATABASE';
      Q.Open;
      if not Q.EOF then
        Result := Trim(Q.Fields[0].AsString);
      Q.Close;
    except
      Result := 'Unknown';
    end;
  finally
    Q.Free;
  end;
end;

procedure InitCheckResult(var Res: TDBCheckResult; const Title, DBName: string);
begin
  Res.Title := Title;
  Res.DBName := DBName;
  Res.DateTimeChecked := Now;
  Res.DBServerVersion := '';
  Res.TotalTables := 0;
  Res.TotalFields := 0;
  Res.CharsetIssues := TStringList.Create;
  Res.LengthIssues := TStringList.Create;
  Res.NotNullIssues := TStringList.Create;
  Res.DataTypeIssues := TStringList.Create;
  Res.PKIssues := TStringList.Create;
  Res.FKIssues := TStringList.Create;
  Res.ViewsIssues := TStringList.Create;
  Res.IndexUniqueIssues := TStringList.Create;
  Res.TriggerDefaultIssues := TStringList.Create;
  Res.Summary := TStringList.Create;
end;

procedure FreeCheckResult(var Res: TDBCheckResult);
begin
  FreeAndNil(Res.CharsetIssues);
  FreeAndNil(Res.LengthIssues);
  FreeAndNil(Res.NotNullIssues);
  FreeAndNil(Res.DataTypeIssues);
  FreeAndNil(Res.PKIssues);
  FreeAndNil(Res.FKIssues);
  FreeAndNil(Res.ViewsIssues);
  FreeAndNil(Res.IndexUniqueIssues);
  FreeAndNil(Res.TriggerDefaultIssues);
  FreeAndNil(Res.Summary);
end;

{ Optimierte Check-Funktionen }

procedure CheckFieldsCharset(DB: TIBDatabase; CharsetIssues: TStringList);
var
  Q: TIBQuery;
  ExpectedCharset, ActualCharset, TableName, FieldName: string;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(CharsetIssues) then Exit;

  ExpectedCharset := UpperCase(DB.Params.Values['lc_ctype']);
  if ExpectedCharset = '' then
    ExpectedCharset := 'NONE';

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    // Optimiert: ALLE Felder in EINER Query
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(rf.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  TRIM(rf.RDB$FIELD_NAME) AS FIELD_NAME, ' +
      '  TRIM(cs.RDB$CHARACTER_SET_NAME) AS CHARSET_NAME ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON f.RDB$FIELD_NAME = rf.RDB$FIELD_SOURCE ' +
      'LEFT JOIN RDB$CHARACTER_SETS cs ON cs.RDB$CHARACTER_SET_ID = f.RDB$CHARACTER_SET_ID ' +
      'WHERE rf.RDB$SYSTEM_FLAG = 0 ' +
      '  AND f.RDB$FIELD_TYPE IN (14, 37, 40, 41, 42, 43) ' + // CHAR, VARCHAR, BLOB etc.
      'ORDER BY rf.RDB$RELATION_NAME, rf.RDB$FIELD_POSITION';

    Q.Open;
    while not Q.EOF do
    begin
      TableName := Trim(Q.Fields[0].AsString);
      FieldName := Trim(Q.Fields[1].AsString);
      ActualCharset := UpperCase(Trim(Q.Fields[2].AsString));

      if ActualCharset = '' then
        ActualCharset := 'NONE';

      if ActualCharset <> ExpectedCharset then
        CharsetIssues.Add(Format('Table "%s" Field "%s": Charset "%s" should be "%s"',
          [TableName, FieldName, ActualCharset, ExpectedCharset]));

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckFieldsLength(DB: TIBDatabase; LengthIssues: TStringList; MaxLength: Integer = 32765);
var
  Q: TIBQuery;
  TableName, FieldName: string;
  CharLen, FieldType: Integer;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(LengthIssues) then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    // Optimiert: ALLE Felder in EINER Query
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(rf.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  TRIM(rf.RDB$FIELD_NAME) AS FIELD_NAME, ' +
      '  f.RDB$FIELD_TYPE, ' +
      '  f.RDB$CHARACTER_LENGTH ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON f.RDB$FIELD_NAME = rf.RDB$FIELD_SOURCE ' +
      'WHERE rf.RDB$SYSTEM_FLAG = 0 ' +
      '  AND f.RDB$FIELD_TYPE IN (14, 37) ' + // nur CHAR und VARCHAR
      'ORDER BY rf.RDB$RELATION_NAME, rf.RDB$FIELD_POSITION';

    Q.Open;
    while not Q.EOF do
    begin
      TableName := Trim(Q.Fields[0].AsString);
      FieldName := Trim(Q.Fields[1].AsString);
      FieldType := Q.Fields[2].AsInteger;
      CharLen := Q.Fields[3].AsInteger;

      if (CharLen <= 0) or (CharLen > MaxLength) then
        LengthIssues.Add(Format('Table "%s" Field "%s": Invalid length %d (max: %d)',
          [TableName, FieldName, CharLen, MaxLength]))
      else if CharLen < 10 then
        LengthIssues.Add(Format('Table "%s" Field "%s": Very short length %d (consider increasing)',
          [TableName, FieldName, CharLen]));

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckFieldsNotNull(DB: TIBDatabase; NotNullIssues: TStringList);
var
  Q: TIBQuery;
  TableName, FieldName: string;
  NullFlag: Integer;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(NotNullIssues) then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    // Optimiert: ALLE Felder in EINER Query
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(rf.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  TRIM(rf.RDB$FIELD_NAME) AS FIELD_NAME, ' +
      '  rf.RDB$NULL_FLAG ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'WHERE rf.RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY rf.RDB$RELATION_NAME, rf.RDB$FIELD_POSITION';

    Q.Open;
    while not Q.EOF do
    begin
      TableName := Trim(Q.Fields[0].AsString);
      FieldName := Trim(Q.Fields[1].AsString);
      if Q.Fields[2].IsNull then
        NullFlag := 0
      else
        NullFlag := Q.Fields[2].AsInteger;

      // 0 = nullable, 1 = not null
      if NullFlag = 0 then
        NotNullIssues.Add(Format('Table "%s" Field "%s": Nullable (consider NOT NULL)',
          [TableName, FieldName]));

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckFieldsDataType(DB: TIBDatabase; DataTypeIssues: TStringList);
var
  Q: TIBQuery;
  TableName, FieldName: string;
  FieldType, FieldSubType: Integer;
  FieldLength: Integer;
  TypeName: string;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(DataTypeIssues) then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(rf.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  TRIM(rf.RDB$FIELD_NAME) AS FIELD_NAME, ' +
      '  f.RDB$FIELD_TYPE, ' +
      '  f.RDB$FIELD_SUB_TYPE, ' +
      '  f.RDB$CHARACTER_LENGTH ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON f.RDB$FIELD_NAME = rf.RDB$FIELD_SOURCE ' +
      'WHERE rf.RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY rf.RDB$RELATION_NAME, rf.RDB$FIELD_POSITION';

    Q.Open;
    while not Q.EOF do
    begin
      TableName := Trim(Q.Fields[0].AsString);
      FieldName := Trim(Q.Fields[1].AsString);
      FieldType := Q.Fields[2].AsInteger;
      FieldSubType := Q.Fields[3].AsInteger;
      FieldLength := Q.Fields[4].AsInteger;

      case FieldType of
        7: TypeName := 'SMALLINT';
        8: TypeName := 'INTEGER';
        10: TypeName := 'FLOAT';
        12: TypeName := 'DATE';
        13: TypeName := 'TIME';
        14: TypeName := 'CHAR';
        16: TypeName := 'BIGINT';
        27: TypeName := 'DOUBLE PRECISION';
        35: TypeName := 'TIMESTAMP';
        37: TypeName := 'VARCHAR';
        40: TypeName := 'CSTRING';
        45: TypeName := 'BLOB_ID';
        261: TypeName := 'BLOB';
        else TypeName := Format('Unknown(%d)', [FieldType]);
      end;

      // Prüfe auf problematische Datentypen
      if FieldType = 261 then // BLOB
      begin
        if FieldSubType = 1 then
          DataTypeIssues.Add(Format('Table "%s" Field "%s": BLOB SUB_TYPE TEXT (consider VARCHAR)',
            [TableName, FieldName]))
        else if FieldSubType = 0 then
          DataTypeIssues.Add(Format('Table "%s" Field "%s": BLOB SUB_TYPE BINARY (consider VARBINARY)',
            [TableName, FieldName]));
      end
      else if FieldType = 14 then // CHAR mit fester Länge
      begin
        if FieldLength > 255 then
          DataTypeIssues.Add(Format('Table "%s" Field "%s": CHAR length %d (consider VARCHAR)',
            [TableName, FieldName, FieldLength]));
      end
      else if FieldType in [7, 8, 10, 16, 27] then // Numerische Typen
      begin
        // Prüfe ob ein kleinerer Typ ausreicht
        // (hier könnte man noch Range-Checks einbauen)
      end;

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckPrimaryKeys(DB: TIBDatabase; PKIssues: TStringList);
var
  Q: TIBQuery;
  TableName, PKName: string;
  PKCount: Integer;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(PKIssues) then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(rc.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  TRIM(rc.RDB$CONSTRAINT_NAME) AS PK_NAME, ' +
      '  COUNT(sg.RDB$FIELD_NAME) AS FIELD_COUNT ' +
      'FROM RDB$RELATION_CONSTRAINTS rc ' +
      'LEFT JOIN RDB$INDEX_SEGMENTS sg ON sg.RDB$INDEX_NAME = rc.RDB$INDEX_NAME ' +
      'WHERE rc.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
      '  AND rc.RDB$SYSTEM_FLAG = 0 ' +
      'GROUP BY rc.RDB$RELATION_NAME, rc.RDB$CONSTRAINT_NAME';

    Q.Open;
    while not Q.EOF do
    begin
      TableName := Trim(Q.Fields[0].AsString);
      PKName := Trim(Q.Fields[1].AsString);
      PKCount := Q.Fields[2].AsInteger;

      if PKCount > 3 then
        PKIssues.Add(Format('Table "%s" PK "%s" has %d fields (consider composite key optimization)',
          [TableName, PKName, PKCount]));

      Q.Next;
    end;
    Q.Close;

    // Prüfe auf Tabellen ohne PK
    Q.SQL.Text :=
      'SELECT TRIM(RDB$RELATION_NAME) AS TABLE_NAME ' +
      'FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      '  AND RDB$VIEW_BLR IS NULL ' +
      '  AND RDB$RELATION_NAME NOT IN (' +
      '    SELECT RDB$RELATION_NAME FROM RDB$RELATION_CONSTRAINTS ' +
      '    WHERE RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
      '      AND RDB$SYSTEM_FLAG = 0' +
      ')';

    Q.Open;
    while not Q.EOF do
    begin
      TableName := Trim(Q.Fields[0].AsString);
      PKIssues.Add(Format('Table "%s" has NO Primary Key!', [TableName]));
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckForeignKeys(DB: TIBDatabase; FKIssues: TStringList);
var
  Q: TIBQuery;
  FKName, TableName, RefTable: string;
  UpdateRule, DeleteRule: Integer;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(FKIssues) then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(rc.RDB$CONSTRAINT_NAME) AS FK_NAME, ' +
      '  TRIM(rc.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  TRIM(idx.RDB$RELATION_NAME) AS REF_TABLE, ' +
      '  rc.RDB$UPDATE_RULE, ' +
      '  rc.RDB$DELETE_RULE ' +
      'FROM RDB$RELATION_CONSTRAINTS rc ' +
      'LEFT JOIN RDB$INDEX_SEGMENTS sg ON sg.RDB$INDEX_NAME = rc.RDB$INDEX_NAME ' +
      'LEFT JOIN RDB$INDICES idx ON idx.RDB$INDEX_NAME = rc.RDB$INDEX_NAME ' +
      'WHERE rc.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' ' +
      '  AND rc.RDB$SYSTEM_FLAG = 0';

    Q.Open;
    while not Q.EOF do
    begin
      FKName := Trim(Q.Fields[0].AsString);
      TableName := Trim(Q.Fields[1].AsString);
      RefTable := Trim(Q.Fields[2].AsString);
      UpdateRule := Q.Fields[3].AsInteger;
      DeleteRule := Q.Fields[4].AsInteger;

      // Prüfe auf fehlende Referenzen
      if RefTable = '' then
        FKIssues.Add(Format('FK "%s" on Table "%s": Missing reference table!', [FKName, TableName]));

      // Prüfe auf ON DELETE/UPDATE Regeln
      if DeleteRule = 0 then
        FKIssues.Add(Format('FK "%s" on Table "%s": No ON DELETE rule (RESTRICT recommended)',
          [FKName, TableName]));

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckViews(DB: TIBDatabase; ViewsIssues: TStringList);
var
  Q: TIBQuery;
  ViewName, ViewSource: string;
  InvalidViews: TStringList;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(ViewsIssues) then Exit;

  Q := TIBQuery.Create(nil);
  InvalidViews := TStringList.Create;
  try
    Q.Database := DB;
    // Prüfe auf Views mit SELECT *
    Q.SQL.Text :=
      'SELECT TRIM(RDB$RELATION_NAME) AS VIEW_NAME, RDB$VIEW_SOURCE ' +
      'FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      '  AND RDB$VIEW_BLR IS NOT NULL';

    Q.Open;
    while not Q.EOF do
    begin
      ViewName := Trim(Q.Fields[0].AsString);
      ViewSource := Trim(Q.Fields[1].AsString);

      // Prüfe auf SELECT *
      if Pos('SELECT *', UpperCase(ViewSource)) > 0 then
        ViewsIssues.Add(Format('View "%s": Uses SELECT * (consider explicit column list)', [ViewName]));

      // Prüfe auf JOIN ohne WHERE
      if (Pos('JOIN', UpperCase(ViewSource)) > 0) and (Pos('WHERE', UpperCase(ViewSource)) = 0) then
        ViewsIssues.Add(Format('View "%s": JOIN without WHERE (potential cartesian product)', [ViewName]));

      Q.Next;
    end;
    Q.Close;

    // Prüfe auf fehlerhafte Views (via RDB$VIEW_SOURCE ist NULL oder leer)
    Q.SQL.Text :=
      'SELECT TRIM(RDB$RELATION_NAME) ' +
      'FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      '  AND RDB$VIEW_BLR IS NOT NULL ' +
      '  AND (RDB$VIEW_SOURCE IS NULL OR RDB$VIEW_SOURCE = '''')';

    Q.Open;
    while not Q.EOF do
    begin
      ViewName := Trim(Q.Fields[0].AsString);
      ViewsIssues.Add(Format('View "%s": Has no source definition (corrupted?)', [ViewName]));
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
    InvalidViews.Free;
  end;
end;

procedure CheckIndicesUnique(DB: TIBDatabase; IndexUniqueIssues: TStringList);
var
  Q: TIBQuery;
  IndexName, TableName: string;
  IsUnique, IsDesc, IsActive: Integer;
  FieldCount: Integer;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(IndexUniqueIssues) then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(idx.RDB$INDEX_NAME) AS INDEX_NAME, ' +
      '  TRIM(idx.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  idx.RDB$UNIQUE_FLAG, ' +
      '  idx.RDB$DESC_FLAG, ' +
      '  idx.RDB$INDEX_INACTIVE, ' +
      '  COUNT(sg.RDB$FIELD_NAME) AS FIELD_COUNT ' +
      'FROM RDB$INDICES idx ' +
      'LEFT JOIN RDB$INDEX_SEGMENTS sg ON sg.RDB$INDEX_NAME = idx.RDB$INDEX_NAME ' +
      'WHERE idx.RDB$SYSTEM_FLAG = 0 ' +
      'GROUP BY idx.RDB$INDEX_NAME, idx.RDB$RELATION_NAME, idx.RDB$UNIQUE_FLAG, ' +
      '         idx.RDB$DESC_FLAG, idx.RDB$INDEX_INACTIVE';

    Q.Open;
    while not Q.EOF do
    begin
      IndexName := Trim(Q.Fields[0].AsString);
      TableName := Trim(Q.Fields[1].AsString);
      IsUnique := Q.Fields[2].AsInteger;
      IsDesc := Q.Fields[3].AsInteger;
      IsActive := Q.Fields[4].AsInteger;
      FieldCount := Q.Fields[5].AsInteger;

      // Prüfe auf inaktive Indizes
      if IsActive = 1 then
        IndexUniqueIssues.Add(Format('Index "%s" on Table "%s": INACTIVE (rebuild needed)',
          [IndexName, TableName]));

      // Prüfe auf Unique-Indizes mit vielen Feldern
      if (IsUnique = 1) and (FieldCount > 3) then
        IndexUniqueIssues.Add(Format('Unique Index "%s" on Table "%s": %d fields (consider simplifying)',
          [IndexName, TableName, FieldCount]));

      // Prüfe auf Indizes ohne Felder (seltsam)
      if FieldCount = 0 then
        IndexUniqueIssues.Add(Format('Index "%s" on Table "%s": Has no fields!', [IndexName, TableName]));

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckTriggerDefaults(DB: TIBDatabase; TriggerDefaultIssues: TStringList);
var
  Q: TIBQuery;
  TriggerName, TableName, TriggerSource: string;
  TriggerType: Integer;
begin
  if not Assigned(DB) or not DB.Connected or not Assigned(TriggerDefaultIssues) then Exit;

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(t.RDB$TRIGGER_NAME) AS TRIGGER_NAME, ' +
      '  TRIM(t.RDB$RELATION_NAME) AS TABLE_NAME, ' +
      '  t.RDB$TRIGGER_TYPE, ' +
      '  t.RDB$TRIGGER_SOURCE ' +
      'FROM RDB$TRIGGERS t ' +
      'WHERE t.RDB$SYSTEM_FLAG = 0';

    Q.Open;
    while not Q.EOF do
    begin
      TriggerName := Trim(Q.Fields[0].AsString);
      TableName := Trim(Q.Fields[1].AsString);
      TriggerType := Q.Fields[2].AsInteger;
      TriggerSource := Trim(Q.Fields[3].AsString);

      // Prüfe auf BEFOR-Trigger (empfohlen für Validierung)
      if (TriggerType >= 1) and (TriggerType <= 3) then // BEFORE Insert/Update/Delete
      begin
        // Alles gut
      end
      else if (TriggerType >= 4) and (TriggerType <= 6) then // AFTER
      begin
        // AFTER-Trigger sind OK, aber prüfen wir auf Logik
      end;

      // Prüfe auf leere Trigger
      if (TriggerSource = '') or (Length(TriggerSource) < 10) then
        TriggerDefaultIssues.Add(Format('Trigger "%s" on Table "%s": Empty or too short!',
          [TriggerName, TableName]));

      // Prüfe auf fehlende Tabellen
      if TableName = '' then
        TriggerDefaultIssues.Add(Format('Trigger "%s": No table associated!', [TriggerName]));

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

end.
