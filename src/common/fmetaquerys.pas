unit fmetaquerys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBDatabase, IBQuery, turbocommon, fbcommon;

type
  TIsolatedQuery = class
  private
    FDatabase: TIBDatabase;
    FTransaction: TIBTransaction;
    FQuery: TIBQuery;
  public
    constructor Create(const SourceDB: TIBDatabase; const SQLText: string; FieldsList: TStringList = nil);
    destructor Destroy; override;
    property Query: TIBQuery read FQuery;
  end;

{------------------------------------------------------------------------------
  Funktionen, die Meta-Informationen aus Systemtabellen holen
------------------------------------------------------------------------------}

function GetFieldsIsolated(const SourceDB: TIBDatabase; const ATableName: string; AStrList: TStringList = nil): TIsolatedQuery;
function GetIndicesIsolated(const SourceDB: TIBDatabase; const ATableName: string): TIsolatedQuery;
function GetTableConstraintsIsolated(const SourceDB: TIBDatabase;
  const ATableName: string; AStrList: TStringList = nil): TIsolatedQuery;
function GetPrimaryKeyIndexNameIsolated(const SourceDB: TIBDatabase;
  const ATableName: string; out ConstraintName: string): string;
function GetIndexFieldsIsolated(const SourceDB: TIBDatabase; const ATableName, AIndexName: string;
  AStrList: TStringList = nil): TIsolatedQuery;
function GetConstraintForeignKeyFieldsIsolated(const SourceDB: TIBDatabase;
  const AIndexName: string): string;

function GetConstraintFieldsIsolated(const SourceDB: TIBDatabase;
  const ATableName, AIndexName: string; var List: TStringList): TIsolatedQuery;


implementation

uses
  Dialogs; // nur falls ShowMessage gebraucht wird

function GetFieldsIsolated(const SourceDB: TIBDatabase; const ATableName: string; AStrList: TStringList = nil): TIsolatedQuery;
var
  SQL: string;
begin
  // QueryTemplate aus deinem bestehenden Code, nur parametrisiert
  SQL :=
    'SELECT r.RDB$FIELD_NAME AS field_name, ' +
    ' r.RDB$DESCRIPTION AS field_description, ' +
    ' r.RDB$DEFAULT_SOURCE AS field_default_source, ' +
    ' r.RDB$NULL_FLAG AS field_not_null_constraint, ' +
    ' f.RDB$FIELD_LENGTH AS field_length, ' +
    ' f.RDB$CHARACTER_LENGTH AS characterlength, ' +
    ' f.RDB$FIELD_PRECISION AS field_precision, ' +
    ' f.RDB$FIELD_SCALE AS field_scale, ' +
    ' f.RDB$FIELD_TYPE as field_type_int, ' +
    ' f.RDB$FIELD_SUB_TYPE AS field_sub_type, ' +
    ' coll.RDB$COLLATION_NAME AS field_collation, ' +
    ' cset.RDB$CHARACTER_SET_NAME AS field_charset, ' +
    ' f.RDB$computed_source AS computed_source, ' +
    ' dim.RDB$UPPER_BOUND AS array_upper_bound, ' +
    ' r.RDB$FIELD_SOURCE AS field_source ' +
    ' FROM RDB$RELATION_FIELDS r ' +
    ' LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
    ' LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID and f.rdb$character_set_id=coll.rdb$character_set_id ' +
    ' LEFT JOIN RDB$CHARACTER_SETS cset ON f.RDB$CHARACTER_SET_ID = cset.RDB$CHARACTER_SET_ID ' +
    ' LEFT JOIN RDB$FIELD_DIMENSIONS dim ON f.RDB$FIELD_NAME = dim.RDB$FIELD_NAME ' +
    ' WHERE r.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(ATableName)) +
    ' ORDER BY r.RDB$FIELD_POSITION';

  Result := TIsolatedQuery.Create(SourceDB, SQL, AStrList);
end;

function GetIndicesIsolated(const SourceDB: TIBDatabase; const ATableName: string): TIsolatedQuery;
var
  SQL: string;
  IndexName: string;
begin
  SQL := 'SELECT * FROM RDB$INDICES WHERE RDB$RELATION_NAME=''' + UpperCase(ATableName) +
    ''' AND RDB$FOREIGN_KEY IS NULL';

  Result := TIsolatedQuery.Create(SourceDB, SQL, nil);
end;

{function GetTableConstraintsIsolated(const SourceDB: TIBDatabase;
  const ATableName: string; AStrList: TStringList = nil): TIsolatedQuery;
const
  QueryTemplate =
    'select '+
    'trim(rc.rdb$constraint_name) as ConstName, '+
    'trim(rfc.rdb$const_name_uq) as KeyName, '+
    'trim(rc2.rdb$relation_name) as OtherTableName, '+
    'trim(flds_pk.rdb$field_name) as OtherFieldName, '+
    'trim(rc.rdb$relation_name) as CurrentTableName, '+
    'trim(flds_fk.rdb$field_name) as CurrentFieldName, '+
    'trim(rfc.rdb$update_rule) as UpdateRule, '+
    'trim(rfc.rdb$delete_rule) as DeleteRule '+
    'from rdb$relation_constraints AS rc '+
    'inner join rdb$ref_constraints as rfc on (rc.rdb$constraint_name = rfc.rdb$constraint_name) '+
    'inner join rdb$index_segments as flds_fk on (flds_fk.rdb$index_name = rc.rdb$index_name) ' +
    'inner join rdb$relation_constraints as rc2 on (rc2.rdb$constraint_name = rfc.rdb$const_name_uq) ' +
    'inner join rdb$index_segments as flds_pk on ' +
    '((flds_pk.rdb$index_name = rc2.rdb$index_name) and (flds_fk.rdb$field_position = flds_pk.rdb$field_position)) ' +
    'where rc.rdb$constraint_type = ''FOREIGN KEY'' '+
    'and rc.rdb$relation_name = ''%s'' '+
    'order by rc.rdb$constraint_name, flds_fk.rdb$field_position';
var
  SQL: string;
begin
  SQL := Format(QueryTemplate, [UpperCase(ATableName)]);
  Result := TIsolatedQuery.Create(SourceDB, SQL);

  if Assigned(AStrList) then
  begin
    AStrList.Clear;
    while not Result.Query.EOF do
    begin
      AStrList.Add(Trim(Result.Query.FieldByName('ConstName').AsString));
      Result.Query.Next;
    end;
    Result.Query.First;
  end;
end;}

function GetTableConstraintsIsolated(const SourceDB: TIBDatabase;
  const ATableName: string; AStrList: TStringList = nil): TIsolatedQuery;
const
  QueryTemplate =
    'select ' +
    'trim(rc.rdb$constraint_name) as ConstName, ' +
    'trim(rfc.rdb$const_name_uq) as KeyName, ' +
    'trim(rc2.rdb$relation_name) as OtherTableName, ' +
    'trim(flds_pk.rdb$field_name) as OtherFieldName, ' +
    'trim(rc.rdb$relation_name) as CurrentTableName, ' +
    'trim(flds_fk.rdb$field_name) as CurrentFieldName, ' +
    'trim(rfc.rdb$update_rule) as UpdateRule, ' +
    'trim(rfc.rdb$delete_rule) as DeleteRule, ' +
    'trim(flds_fk.rdb$field_name) as ForeignKeyField ' + // ✨ NEU ✨
    'from rdb$relation_constraints rc ' +
    'inner join rdb$ref_constraints rfc on (rc.rdb$constraint_name = rfc.rdb$constraint_name) ' +
    'inner join rdb$index_segments flds_fk on (flds_fk.rdb$index_name = rc.rdb$index_name) ' +
    'inner join rdb$relation_constraints rc2 on (rc2.rdb$constraint_name = rfc.rdb$const_name_uq) ' +
    'inner join rdb$index_segments flds_pk on ' +
    '((flds_pk.rdb$index_name = rc2.rdb$index_name) and (flds_fk.rdb$field_position = flds_pk.rdb$field_position)) ' +
    'where rc.rdb$constraint_type = ''FOREIGN KEY'' ' +
    'and rc.rdb$relation_name = ''%s'' ' +
    'order by rc.rdb$constraint_name, flds_fk.rdb$field_position ';
var
  SQL: string;
begin
  SQL := Format(QueryTemplate, [UpperCase(ATableName)]);
  Result := TIsolatedQuery.Create(SourceDB, SQL, AStrList);
end;


function GetPrimaryKeyIndexNameIsolated(const SourceDB: TIBDatabase;
  const ATableName: string; out ConstraintName: string): string;
var
  Iso: TIsolatedQuery;
begin
  Result := '';
  ConstraintName := '';

  Iso := TIsolatedQuery.Create(SourceDB,
    'select RDB$Index_name, RDB$Constraint_Name ' +
    'from RDB$RELATION_CONSTRAINTS ' +
    'where RDB$Relation_Name = ' + QuotedStr(UpperCase(ATableName)) +
    ' and RDB$Constraint_Type = ''PRIMARY KEY'' ');
  try
    if not Iso.Query.EOF then
    begin
      Result := Trim(Iso.Query.FieldByName('RDB$Index_name').AsString);
      ConstraintName := Trim(Iso.Query.FieldByName('RDB$Constraint_Name').AsString);
    end;
  finally
    Iso.Free;
  end;
end;

function GetIndexFieldsIsolated(const SourceDB: TIBDatabase; const ATableName, AIndexName: string;
  AStrList: TStringList = nil): TIsolatedQuery;
var
  SQL: string;
begin
  SQL :=
    'SELECT RDB$INDEX_SEGMENTS.RDB$FIELD_NAME AS field_name, ' + LineEnding +
    'RDB$INDICES.RDB$DESCRIPTION AS description, ' + LineEnding +
    '(RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION + 1) AS field_position ' + LineEnding +
    'FROM RDB$INDEX_SEGMENTS ' + LineEnding +
    'LEFT JOIN RDB$INDICES ON RDB$INDICES.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' + LineEnding +
    'LEFT JOIN RDB$RELATION_CONSTRAINTS ON RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' + LineEnding +
    ' WHERE UPPER(RDB$INDICES.RDB$RELATION_NAME)=''' + UpperCase(ATableName) + '''         -- table name ' + LineEnding +
    '  AND UPPER(RDB$INDICES.RDB$INDEX_NAME)=''' + UpperCase(AIndexName) + ''' -- index name ' + LineEnding +
    '--  AND RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE IS NULL ' + LineEnding +
    'ORDER BY RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION;';

  Result := TIsolatedQuery.Create(SourceDB, SQL, AStrList);
end;

function GetConstraintForeignKeyFieldsIsolated(const SourceDB: TIBDatabase;
  const AIndexName: string): string;
var
  IsoQuery: TIsolatedQuery;
begin
  Result := '';
  IsoQuery := TIsolatedQuery.Create(
    SourceDB,
    'select RDB$Field_name as FieldName from RDB$INDEX_SEGMENTS ' +
    'where RDB$Index_name = ' + QuotedStr(UpperCase(Trim(AIndexName))))
  ;
  try
    while not IsoQuery.Query.EOF do
    begin
      if Result <> '' then
        Result := Result + ','; // Felder trennen mit Komma
      Result := Result + Trim(IsoQuery.Query.FieldByName('FieldName').AsString);
      IsoQuery.Query.Next;
    end;
  finally
    IsoQuery.Free;
  end;
end;

function GetConstraintFieldsIsolated(const SourceDB: TIBDatabase;
  const ATableName, AIndexName: string; var List: TStringList): TIsolatedQuery;
var SQL: string;
begin
  SQL := 'SELECT s.RDB$FIELD_NAME AS field_name ' +
     'FROM RDB$INDEX_SEGMENTS s ' +
     'LEFT JOIN RDB$INDICES i ON i.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS rc ON rc.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$REF_CONSTRAINTS refc ON rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME ' +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS rc2 ON rc2.RDB$CONSTRAINT_NAME = refc.RDB$CONST_NAME_UQ ' +
     'LEFT JOIN RDB$INDICES i2 ON i2.RDB$INDEX_NAME = rc2.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$INDEX_SEGMENTS s2 ON i2.RDB$INDEX_NAME = s2.RDB$INDEX_NAME ' +
     '   WHERE i.RDB$RELATION_NAME=''' + UpperCase(ATableName) + '''  ' +
      'AND rc.RDB$INDEX_NAME=''' + UpperCase(AIndexName) + ''' ' +
      'AND rc.RDB$CONSTRAINT_TYPE IS NOT NULL ' +
      'ORDER BY s.RDB$FIELD_POSITION';

  Result := TIsolatedQuery.Create(SourceDB, SQL, List);
end;

{ TIsolatedQuery }
constructor TIsolatedQuery.Create(const SourceDB: TIBDatabase; const SQLText: string; FieldsList: TStringList = nil);
Var FieldName: string;
begin
  inherited Create;

  // lokale Kopie der DB anlegen
  FDatabase := TIBDatabase.Create(nil);
  AssignIBDatabase(SourceDB, FDatabase);

  FDatabase.Connected := true;

  FTransaction := TIBTransaction.Create(nil);
  FTransaction.DefaultDatabase := FDatabase;
  FTransaction.StartTransaction;

  FQuery := TIBQuery.Create(nil);
  FQuery.Database := FDatabase;
  FQuery.Transaction := FTransaction;

  FQuery.SQL.Text := SQLText;
  FQuery.Open; // direkt öffnen

  // optional FieldsList befüllen
  if Assigned(FieldsList) then
  begin
    FieldsList.Clear;
    while not FQuery.EOF do
    begin
      FieldName := Trim(FQuery.FieldByName('field_name').AsString);
      if FieldsList.IndexOf(FieldName) = -1 then
        FieldsList.Add(FieldName);
      FQuery.Next;
    end;
    FQuery.First; // Cursor wieder auf erste Zeile
  end;

end;

{constructor TIsolatedQuery.Create(const SourceDB: TIBDatabase; const SQLText: string; FieldsList: TStringList = nil);
Var FieldName: string;
begin
  inherited Create;

  // lokale Kopie der DB anlegen
  FDatabase := TIBDatabase.Create(nil);
  AssignIBDatabase(SourceDB, FDatabase);

  FDatabase.Connected := true;

  FTransaction := TIBTransaction.Create(nil);
  FTransaction.DefaultDatabase := FDatabase;
  FTransaction.StartTransaction;

  FQuery := TIBQuery.Create(nil);
  FQuery.Database := FDatabase;
  FQuery.Transaction := FTransaction;

  FQuery.SQL.Text := SQLText;
  FQuery.Open; // direkt öffnen

  // optional FieldsList befüllen
  if Assigned(FieldsList) then
  begin
    FieldsList.Clear;
    while not FQuery.EOF do
    begin
      FieldName := Trim(FQuery.FieldByName('field_name').AsString);
      if FieldsList.IndexOf(FieldName) = -1 then
        FieldsList.Add(FieldName);
      FQuery.Next;
    end;
    FQuery.First; // Cursor wieder auf erste Zeile
  end;
end;}

destructor TIsolatedQuery.Destroy;
begin
  if Assigned(FQuery) then
  begin
    if FQuery.Active then
      FQuery.Close;
    FreeAndNil(FQuery);
  end;
  if Assigned(FTransaction) then
  begin
    if FTransaction.InTransaction then
      FTransaction.Rollback;
    FreeAndNil(FTransaction);
  end;

  if Assigned(FDatabase) then
  begin
    if FDatabase.Connected then
      FDatabase.Connected := false;
    FreeAndNil(FDatabase);
  end;

  inherited Destroy;
end;


end.

