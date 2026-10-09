unit fsimpleobjextractor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  Dialogs, ComCtrls, RegExpr,
  IB, IBDatabase, IBSQL, IBExtract,

  fbcommon,

  udb_udr_func_fetcher;

type

  TSimpleObjExtractor = class
    FInitialized: boolean;
    FDBIndex: integer;
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBSQL: TIBSQL;
    FIBExtract: TIBExtract;
    procedure FixDomainQuoting(AItems: TStrings);
    procedure FixArraySyntax(AItems: TStrings);
    function TBTypeToIBXType(AObjectType: TObjectType): TExtractObjectTypes;
    procedure GetUDRFunction(Conn: TIBDatabase; AName: string; AItems: TStrings);
  public
    constructor Create(DBIndex: integer);
    procedure   ResetExtract;
    destructor  Destroy; override;

    function GetTableFieldsRaw(const ATableName: string): TFBFieldRawArray;

    function GetPrimaryKeyIndexName(const ATableName: string;
      out AConstraintName: string): string;

    function GetConstraintFields(const AIndexName: string;
      var AFields: TStringList): Boolean;

    function GetConstraintFieldsByName(
      const ATableName, AConstraintName: string;
      var AFields: TStringList): Boolean;

    function GetCheckConstraintSource(const ATableName, AConstraintName: string): string;
    function GetTableReferences(const ATableName: string): TForeignKeyInfoArray;
    function GetTableForeignKeys(const ATableName: string): TFBForeignKeyDefArray;

    function GetTableTriggersRaw(const ATableName: string): TFBTriggerRawArray;
    function GetTriggerInfo(const ATriggerName: string): TFBTriggerInfo;
    procedure DecodeTriggerType(ATriggerType: Int64;
      out AfterBefore, Event: string;
      out IsDDLTrigger, IsDBTrigger: Boolean);
    procedure GetTriggerScript(const ATriggerName: string; AItems: TStrings);

    procedure GetAllFieldTypes(AItems: TStrings);
    procedure GetBasicFieldTypes(AItems: TStrings);
    procedure GetExtendedFieldTypes(AItems: TStrings);
    function  GetFieldTypeSize(const ATypeName: string): Integer;
    function  GetDomainFieldSize(const ADomainName: string): Integer;


    function GetObjectPermissions(const AObjectName: string): TFBPermissionArray;
    function GetUserObjectGrants(const AUserName: string;
      AObjectType: Integer): TFBUserGrantArray;

    procedure Extract(ObjectType: TObjectType; ObjectName : String;
                       ExtractTypes: TExtractTypes; Quoted: boolean; var AItems: TStrings);

    procedure   ExtractObjectNames(dbIndex: integer; ObjectType: TObjectType; SystemFlag: boolean;  var AItems: TStrings; OwnerObjName: string='');

    procedure   ExtractTableNames(AItems: TStrings; Quoted: boolean; SystemFlag: boolean);
    procedure   ExtractTableNamesToTreeNode(Quoted: boolean; Node: TTreeNode; SystemFlag: boolean);

    procedure   EndTransaction;

    procedure   ExtractToTreeNode(ObjectType: TObjectType; ObjectName : String; ExtractTypes: TExtractTypes; Quoted: boolean; var Node: TTreeNode; AImageIndex: integer);
    procedure   ExtractTableFields(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char; RemoveLastComma: boolean);
    procedure   ExtractTableFieldsWithComma(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
    procedure   ExtractTableFieldsForExternalTable(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
    procedure   ExtractCleanTableFields(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
    procedure   ExtractTableFieldsToTreeNode(ATableName: string; var Node: TTreeNode; Quoted: boolean; Delimiter: char; ImageIndex: integer; SysFlag: boolean);

    property   Initialized: boolean read FInitialized;
  end;

  { ============================================================
    FBIdentifierCast — liefert einen SQL-Ausdruck, der auf FB 1.5
    den CHAR(31)-Abschneide-Bug (ab 10 Zeichen) umgeht.

    FB 1.5 : CAST(<expr> AS VARCHAR(255))
    FB 2.0+: <expr> unverändert

    Wird auf Identifier-Spalten im SELECT-Output angewendet.
    WHERE- und JOIN-Vergleiche bleiben unverändert — der Server
    vergleicht intern korrekt, nur der Client-Output wird gestutzt.
    ============================================================ }
  function FBIdentifierCast(const AExpr: string; AServerVersionMajor: Word): string;

implementation

uses  SysTables, turbocommon;

procedure TSimpleObjExtractor.EndTransaction;
begin
  if Assigned(FIBTransaction) and FIBTransaction.InTransaction then
    FIBTransaction.Rollback;
end;

{ ============================================================
  FBIdentifierCast — liefert einen SQL-Ausdruck, der auf FB 1.5
  den CHAR(31)-Abschneide-Bug (ab 10 Zeichen) umgeht.

  FB 1.5 : CAST(<expr> AS VARCHAR(255))
  FB 2.0+: <expr> unverändert

  Wird auf Identifier-Spalten im SELECT-Output angewendet.
  WHERE- und JOIN-Vergleiche bleiben unverändert — der Server
  vergleicht intern korrekt, nur der Client-Output wird gestutzt.
  ============================================================ }
  function FBIdentifierCast(const AExpr: string; AServerVersionMajor: Word): string;
  begin
    if AServerVersionMajor < 2 then
      Result := 'CAST(' + AExpr + ' AS VARCHAR(255))'
    else
      Result := AExpr;
  end;

procedure TSimpleObjExtractor.ExtractTableNames(AItems: TStrings; Quoted: boolean; SystemFlag: boolean);
var
  i: Integer;
  S: string;
  tmpObjType: TObjectType;
begin
  if SystemFlag then
    tmpObjType := otSystemTables
  else
    tmpObjType := otTables;

  AItems.Clear;

  ExtractObjectNames(FDBIndex, tmpObjType, SystemFlag, AItems, '');


  if Quoted then
  begin
    for i := 0 to AItems.Count - 1 do
    begin
      S := AItems[i];
      S :=  '"' + S + '"';
      AItems[i] := S;
    end;
  end;
end;

procedure TSimpleObjExtractor.ExtractTableNamesToTreeNode(
  Quoted: boolean;
  Node: TTreeNode;
  SystemFlag: boolean);

var tmpNode, dummyNode: TTreeNode;
  Items: TStringList;
  i: Integer;
begin
  if Node = nil then Exit;

  Node.DeleteChildren;

  Items := TStringList.Create;

  try
    ExtractTableNames(Items, Quoted, SystemFlag);

    for i := 0 to Items.Count - 1 do
    begin
      if Trim(Items[i]) = '' then Continue;
      tmpNode := Node.TreeView.Items.AddChild(Node, Items[i]);

      TPNodeInfos(tmpNode.Data)^.dbIndex := FDBIndex;

      if SystemFlag then
        TPNodeInfos(tmpNode.Data)^.ObjectType := tvotSystemTable
      else
        TPNodeInfos(tmpNode.Data)^.ObjectType := tvotTable;

      tmpNode.ImageIndex := 4;
      dummyNode := Node.TreeView.Items.AddChild(tmpNode, 'Loading...');
    end;

  finally
    Items.Free;
  end;
end;

constructor TSimpleObjExtractor.Create(DBIndex: integer);
var
  OrigRec: TRegisteredDatabase;
  CachedPwd: string;
begin
  inherited Create;
  FInitialized := False;
  FDBIndex := DBIndex;

  FIBDatabase := nil;
  FIBTransaction := nil;
  FIBExtract := nil;

  try
    // Datenbankobjekt erstellen
    FIBDatabase := TIBDatabase.Create(nil);
    AssignIBDatabase(RegisteredDatabases[FDBIndex].IBDatabase, FIBDatabase);

    // Transaction erstellen
    FIBTransaction := TIBTransaction.Create(FIBDatabase);
    FIBDatabase.DefaultTransaction := FIBTransaction;
    FIBTransaction.DefaultDatabase := FIBDatabase;

    // OnLogin-Handler setzen
    FIBDatabase.OnLogin := @dmSysTables.OnDatabaseLogin;
    FIBDatabase.LoginPrompt := True;

    // WICHTIG: DB-Params mit Passwort aus Registrierung ODER Session-Cache befüllen
    OrigRec := RegisteredDatabases[FDBIndex].RegRec;

    FIBDatabase.Params.Values['user_name'] := OrigRec.UserName;

    // Passwort: Erst aus Registrierung, dann aus Session-Cache
    if OrigRec.SavePassword and (OrigRec.Password <> '') then
      FIBDatabase.Params.Values['password'] := OrigRec.Password
    else
    begin
      CachedPwd := GetDBSessionPassword(OrigRec.ServerName, OrigRec.DatabaseName);
      if CachedPwd <> '' then
        FIBDatabase.Params.Values['password'] := CachedPwd
      else
        FIBDatabase.Params.Values['password'] := OrigRec.Password; // leer → OnLogin feuert
    end;

    if OrigRec.Role <> '' then
      FIBDatabase.Params.Values['sql_role_name'] := OrigRec.Role;

    if OrigRec.Charset <> '' then
      FIBDatabase.Params.Values['lc_ctype'] := OrigRec.Charset;

    // Verbindung prüfen
    if not FIBDatabase.Connected then
      FIBDatabase.Connected := True;

    // Extract-Objekt erstellen
    FIBExtract := TIBExtract.Create(FIBDatabase);
    FIBExtract.Database := FIBDatabase;
    FIBExtract.Transaction := FIBTransaction;

    FIBExtract.AlwaysQuoteIdentifiers := AlwaysQuoteIdentifiers;
    FIBExtract.CaseSensitiveObjectNames := CaseSensitiveObjectNames;
    FIBExtract.ShowSystem := ShowSystem;

    // Sicherstellen, dass DB verbunden bleibt
    FIBDatabase.Connected := True;

    FInitialized := True;

  except
    on E: Exception do
    begin
      // Ressourcen sauber freigeben
      FreeAndNil(FIBExtract);
      FreeAndNil(FIBTransaction);
      FreeAndNil(FIBDatabase);

      FInitialized := False;

      raise Exception.Create('Error creating TSimpleObjExtractor: ' + E.Message);
    end;
  end;
end;

procedure TSimpleObjExtractor.ResetExtract;
begin
  FIBExtract.Items.Clear;
end;

destructor TSimpleObjExtractor.destroy;
begin
  if Assigned(FIBExtract) then
  begin
    FIBExtract.Database := nil;
    FIBExtract.Free;
  end;

  if Assigned(FIBDatabase) and Assigned(FIBDatabase.DefaultTransaction) then
  begin
    if FIBDatabase.DefaultTransaction.InTransaction then
      FIBDatabase.DefaultTransaction.Rollback;
  end;

  if Assigned(FIBTransaction) then
    FIBTransaction.Free;

  if Assigned(FIBDatabase) then
  begin
    if FIBDatabase.Connected then
      FIBDatabase.Connected := false;
    FIBDatabase.Free;
  end;

  inherited destroy;
end;

// ============================================================
// Felder einer Tabelle als strukturierte Rohdaten holen
// FB 1.5 + FB 2.0 bis 6 mit Versions-Weiche
//
// Array-Erkennung: erste Dimension via LEFT JOIN (RDB$DIMENSION = 0).
// Alle Dimensionen werden in einem zweiten Durchlauf aus
// RDB$FIELD_DIMENSIONS nachgelesen (inkl. Lower/UpperBound).
// ============================================================
function TSimpleObjExtractor.GetTableFieldsRaw(const ATableName: string): TFBFieldRawArray;
var
  Qry: TIBSQL;
  QDims: TIBSQL;
  i: Integer;
  ServerVersionMajor: Word;
  SQL: string;
  FieldSrcName: string;
begin
  SetLength(Result, 0);

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;

    if ServerVersionMajor < 2 then
      // FB 1.5 — keine RDB$CHARACTER_LENGTH, RDB$FIELD_PRECISION,
      // RDB$DEFAULT_SOURCE, RDB$COLLATION_ID, RDB$COLLATION_NAME
      // Achtung: Alias CHAR_LEN statt CHARACTER_LENGTH — letzteres ist
      // ab FB 2.0 ein reserviertes Wort und als Alias verboten.
      // FB 1.5: Identifier-Spalten via FBIdentifierCast → VARCHAR(255),
      // sonst werden CHAR(31)-Namen ab 10 Zeichen abgeschnitten.
      SQL :=
        'SELECT ' +
        '  ' + FBIdentifierCast('rf.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
        '  f.RDB$FIELD_TYPE AS FIELD_TYPE, ' +
        '  f.RDB$FIELD_SUB_TYPE AS FIELD_SUB_TYPE, ' +
        '  f.RDB$FIELD_LENGTH AS FIELD_LENGTH, ' +
        '  CAST(NULL AS INTEGER) AS CHAR_LEN, ' +
        '  CAST(NULL AS INTEGER) AS FIELD_PRECISION, ' +
        '  f.RDB$FIELD_SCALE AS FIELD_SCALE, ' +
        '  rf.RDB$NULL_FLAG AS NULL_FLAG, ' +
        '  CAST(NULL AS VARCHAR(255)) AS DEFAULT_SOURCE, ' +
        '  rf.RDB$DESCRIPTION AS DESCRIPTION, ' +
        '  f.RDB$COMPUTED_SOURCE AS COMPUTED_SOURCE, ' +
        '  f.RDB$CHARACTER_SET_ID AS CHARACTER_SET_ID, ' +
        '  CAST(NULL AS INTEGER) AS COLLATION_ID, ' +
        '  rf.RDB$FIELD_POSITION AS FIELD_POSITION, ' +
        '  ' + FBIdentifierCast('rf.RDB$FIELD_SOURCE', ServerVersionMajor) + ' AS FIELD_SOURCE, ' +
        '  ' + FBIdentifierCast('cs.RDB$CHARACTER_SET_NAME', ServerVersionMajor) + ' AS CHARACTER_SET_NAME, ' +
        '  CAST(NULL AS VARCHAR(255)) AS COLLATION_NAME, ' +
        '  dim.RDB$UPPER_BOUND AS ARRAY_UPPER_BOUND ' +
        'FROM RDB$RELATION_FIELDS rf ' +
        'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
        'LEFT JOIN RDB$CHARACTER_SETS cs ON f.RDB$CHARACTER_SET_ID = cs.RDB$CHARACTER_SET_ID ' +
        'LEFT JOIN RDB$FIELD_DIMENSIONS dim ON f.RDB$FIELD_NAME = dim.RDB$FIELD_NAME ' +
        '                                   AND dim.RDB$DIMENSION = 0 ' +
        'WHERE rf.RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ' ' +
        'ORDER BY rf.RDB$FIELD_POSITION'
    else
      // FB 2.0+ — voller Satz
      SQL :=
        'SELECT ' +
        '  ' + FBIdentifierCast('rf.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
        '  f.RDB$FIELD_TYPE AS FIELD_TYPE, ' +
        '  f.RDB$FIELD_SUB_TYPE AS FIELD_SUB_TYPE, ' +
        '  f.RDB$FIELD_LENGTH AS FIELD_LENGTH, ' +
        '  f.RDB$CHARACTER_LENGTH AS CHAR_LEN, ' +
        '  f.RDB$FIELD_PRECISION AS FIELD_PRECISION, ' +
        '  f.RDB$FIELD_SCALE AS FIELD_SCALE, ' +
        '  rf.RDB$NULL_FLAG AS NULL_FLAG, ' +
        '  rf.RDB$DEFAULT_SOURCE AS DEFAULT_SOURCE, ' +
        '  rf.RDB$DESCRIPTION AS DESCRIPTION, ' +
        '  f.RDB$COMPUTED_SOURCE AS COMPUTED_SOURCE, ' +
        '  f.RDB$CHARACTER_SET_ID AS CHARACTER_SET_ID, ' +
        '  f.RDB$COLLATION_ID AS COLLATION_ID, ' +
        '  rf.RDB$FIELD_POSITION AS FIELD_POSITION, ' +
        '  ' + FBIdentifierCast('rf.RDB$FIELD_SOURCE', ServerVersionMajor) + ' AS FIELD_SOURCE, ' +
        '  ' + FBIdentifierCast('cs.RDB$CHARACTER_SET_NAME', ServerVersionMajor) + ' AS CHARACTER_SET_NAME, ' +
        '  ' + FBIdentifierCast('coll.RDB$COLLATION_NAME', ServerVersionMajor) + ' AS COLLATION_NAME, ' +
        '  dim.RDB$UPPER_BOUND AS ARRAY_UPPER_BOUND ' +
        'FROM RDB$RELATION_FIELDS rf ' +
        'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
        'LEFT JOIN RDB$CHARACTER_SETS cs ON f.RDB$CHARACTER_SET_ID = cs.RDB$CHARACTER_SET_ID ' +
        'LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID ' +
        '                            AND f.RDB$CHARACTER_SET_ID = coll.RDB$CHARACTER_SET_ID ' +
        'LEFT JOIN RDB$FIELD_DIMENSIONS dim ON f.RDB$FIELD_NAME = dim.RDB$FIELD_NAME ' +
        '                                   AND dim.RDB$DIMENSION = 0 ' +
        'WHERE rf.RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ' ' +
        'ORDER BY rf.RDB$FIELD_POSITION';

    Qry.SQL.Text := SQL;
    Qry.ExecQuery;

    while not Qry.EOF do
    begin
      i := Length(Result);
      SetLength(Result, i + 1);

      with Result[i] do
      begin
        FieldName := Trim(Qry.FieldByName('FIELD_NAME').AsString);

        if Qry.FieldByName('FIELD_TYPE').IsNull then
          FieldType := 0
        else
          FieldType := Qry.FieldByName('FIELD_TYPE').AsInteger;

        if Qry.FieldByName('FIELD_SUB_TYPE').IsNull then
          FieldSubType := -1
        else
          FieldSubType := Qry.FieldByName('FIELD_SUB_TYPE').AsInteger;

        if Qry.FieldByName('FIELD_LENGTH').IsNull then
          FieldLength := -1
        else
          FieldLength := Qry.FieldByName('FIELD_LENGTH').AsInteger;

        if Qry.FieldByName('CHAR_LEN').IsNull then
          CharacterLength := -1
        else
          CharacterLength := Qry.FieldByName('CHAR_LEN').AsInteger;

        if Qry.FieldByName('FIELD_PRECISION').IsNull then
          FieldPrecision := -1
        else
          FieldPrecision := Qry.FieldByName('FIELD_PRECISION').AsInteger;

        if Qry.FieldByName('FIELD_SCALE').IsNull then
          FieldScale := -1
        else
          FieldScale := Qry.FieldByName('FIELD_SCALE').AsInteger;

        NotNull := (not Qry.FieldByName('NULL_FLAG').IsNull)
                   and (Qry.FieldByName('NULL_FLAG').AsInteger = 1);

        if Qry.FieldByName('DEFAULT_SOURCE').IsNull then
          DefaultSource := ''
        else
          DefaultSource := Trim(Qry.FieldByName('DEFAULT_SOURCE').AsString);

        if Qry.FieldByName('DESCRIPTION').IsNull then
          Description := ''
        else
          Description := Trim(Qry.FieldByName('DESCRIPTION').AsString);

        if Qry.FieldByName('COMPUTED_SOURCE').IsNull then
          ComputedSource := ''
        else
          ComputedSource := Trim(Qry.FieldByName('COMPUTED_SOURCE').AsString);

        if Qry.FieldByName('CHARACTER_SET_ID').IsNull then
          CharacterSetID := -1
        else
          CharacterSetID := Qry.FieldByName('CHARACTER_SET_ID').AsInteger;

        if Qry.FieldByName('COLLATION_ID').IsNull then
          CollationID := -1
        else
          CollationID := Qry.FieldByName('COLLATION_ID').AsInteger;

        if Qry.FieldByName('CHARACTER_SET_NAME').IsNull then
          CharacterSetName := ''
        else
          CharacterSetName := Trim(Qry.FieldByName('CHARACTER_SET_NAME').AsString);

        if Qry.FieldByName('COLLATION_NAME').IsNull then
          CollationName := ''
        else
          CollationName := Trim(Qry.FieldByName('COLLATION_NAME').AsString);

        if Qry.FieldByName('FIELD_SOURCE').IsNull then
          FieldSource := ''
        else
          FieldSource := Trim(Qry.FieldByName('FIELD_SOURCE').AsString);

        if Qry.FieldByName('FIELD_POSITION').IsNull then
          FieldPosition := -1
        else
          FieldPosition := Qry.FieldByName('FIELD_POSITION').AsInteger;

        // Erste Array-Dimension (nur oberer Wert)
        if Qry.FieldByName('ARRAY_UPPER_BOUND').IsNull then
          ArrayUpperBound := 0
        else
          ArrayUpperBound := Qry.FieldByName('ARRAY_UPPER_BOUND').AsInteger;

        // ArrayDims wird im Nachlauf befüllt
        SetLength(ArrayDims, 0);
      end;

      Qry.Next;
    end;

    Qry.Close;

    // ============================================================
    // Nachlauf: alle Dimensionen für Array-Felder lesen
    // RDB$FIELD_DIMENSIONS.RDB$FIELD_NAME verweist auf
    // RDB$FIELDS.RDB$FIELD_NAME (also FieldSource, nicht FieldName!)
    // ============================================================
    QDims := TIBSQL.Create(FIBDatabase);
    try
      QDims.Transaction := FIBTransaction;
      QDims.SQL.Text :=
        'SELECT RDB$DIMENSION, RDB$LOWER_BOUND, RDB$UPPER_BOUND ' +
        'FROM RDB$FIELD_DIMENSIONS ' +
        'WHERE RDB$FIELD_NAME = :FN ' +
        'ORDER BY RDB$DIMENSION';
      QDims.Prepare;

      for i := 0 to High(Result) do
      begin
        if Result[i].ArrayUpperBound = 0 then
          Continue;

        FieldSrcName := Result[i].FieldSource;
        if FieldSrcName = '' then
          Continue;

        QDims.Close;
        QDims.Params.ByName('FN').AsString := FieldSrcName;
        QDims.ExecQuery;

        SetLength(Result[i].ArrayDims, 0);
        while not QDims.EOF do
        begin
          SetLength(Result[i].ArrayDims,
                   Length(Result[i].ArrayDims) + 1);

          with Result[i].ArrayDims[High(Result[i].ArrayDims)] do
          begin
            LowerBound := QDims.FieldByName('RDB$LOWER_BOUND').AsInteger;
            UpperBound := QDims.FieldByName('RDB$UPPER_BOUND').AsInteger;
          end;

          QDims.Next;
        end;
      end;
    finally
      QDims.Free;
    end;

  finally
    Qry.Free;
    EndTransaction;
  end;
end;

function TSimpleObjExtractor.GetPrimaryKeyIndexName(const ATableName: string;
  out AConstraintName: string): string;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
begin
  Result := '';
  AConstraintName := '';

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;

    // FB 1.5: CAST auf VARCHAR(255), damit der CHAR(31)-Abschneide-Bug nicht zuschlägt
    Qry.SQL.Text :=
      'SELECT ' +
      FBIdentifierCast('RDB$INDEX_NAME', ServerVersionMajor) + ' AS INDEX_NAME, ' +
      FBIdentifierCast('RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS CONSTRAINT_NAME ' +
      'FROM RDB$RELATION_CONSTRAINTS ' +
      'WHERE RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ' ' +
      '  AND RDB$CONSTRAINT_TYPE = ' + QuotedStr('PRIMARY KEY');

    Qry.ExecQuery;

    if not Qry.EOF then
    begin
      Result := Trim(Qry.FieldByName('INDEX_NAME').AsString);
      AConstraintName := Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString);
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Liefert die Feldnamen eines Index / Constraints in Positions-Reihenfolge.
// FB 1.5: CAST auf VARCHAR(255) umgeht den CHAR(31)-Abschneide-Bug.
// ============================================================
function TSimpleObjExtractor.GetConstraintFields(const AIndexName: string;
  var AFields: TStringList): Boolean;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
begin
  Result := False;
  AFields.Clear;

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT ' + FBIdentifierCast('RDB$FIELD_NAME', ServerVersionMajor) + ' ' +
      'FROM RDB$INDEX_SEGMENTS ' +
      'WHERE RDB$INDEX_NAME = ' + QuotedStr(AIndexName) + ' ' +
      'ORDER BY RDB$FIELD_POSITION';
    Qry.ExecQuery;

    while not Qry.EOF do
    begin
      AFields.Add(Trim(Qry.Fields[0].AsString));
      Qry.Next;
    end;

    Result := AFields.Count > 0;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

function TSimpleObjExtractor.GetConstraintFieldsByName(
  const ATableName, AConstraintName: string;
  var AFields: TStringList): Boolean;
var
  Qry: TIBSQL;
  IndexName: string;
  ServerVersionMajor: Word;
begin
  Result := False;
  AFields.Clear;

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT ' +
      FBIdentifierCast('RDB$INDEX_NAME', ServerVersionMajor) + ' AS INDEX_NAME ' +
      'FROM RDB$RELATION_CONSTRAINTS ' +
      'WHERE RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ' ' +
      '  AND RDB$CONSTRAINT_NAME = ' + QuotedStr(AConstraintName);

    Qry.ExecQuery;
    if Qry.EOF then
      Exit;

    IndexName := Trim(Qry.FieldByName('INDEX_NAME').AsString);
  finally
    Qry.Free;
  end;

  if IndexName = '' then
    Exit;

  Result := GetConstraintFields(IndexName, AFields);
end;

// ============================================================
// Liefert den vollständigen CHECK-Ausdruck eines benannten
// CHECK-Constraints. Mehrere Trigger pro Constraint werden
// mit " AND " verbunden.
// FB 1.5: kein TRIM im SQL — Trimming passiert in Pascal.
// ============================================================
function TSimpleObjExtractor.GetCheckConstraintSource(
  const ATableName, AConstraintName: string): string;
var
  Qry: TIBSQL;
  RawSource: string;
begin
  Result := '';

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT trg.RDB$TRIGGER_SOURCE AS CHECK_SOURCE ' +
      'FROM RDB$RELATION_CONSTRAINTS rc ' +
      'JOIN RDB$CHECK_CONSTRAINTS cc ON rc.RDB$CONSTRAINT_NAME = cc.RDB$CONSTRAINT_NAME ' +
      'JOIN RDB$TRIGGERS trg ON cc.RDB$TRIGGER_NAME = trg.RDB$TRIGGER_NAME ' +
      'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ' ' +
      '  AND rc.RDB$CONSTRAINT_NAME = ' + QuotedStr(AConstraintName) + ' ' +
      '  AND rc.RDB$CONSTRAINT_TYPE = ' + QuotedStr('CHECK') + ' ' +
      'ORDER BY trg.RDB$TRIGGER_NAME';

    Qry.ExecQuery;

    while not Qry.EOF do
    begin
      if not Qry.FieldByName('CHECK_SOURCE').IsNull then
      begin
        RawSource := Trim(Qry.FieldByName('CHECK_SOURCE').AsString);
        if RawSource <> '' then
        begin
          if Result <> '' then
            Result := Result + ' AND ';
          Result := Result + RawSource;
        end;
      end;
      Qry.Next;
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Liefert eingehende Foreign Keys für eine Tabelle.
// "Eingehend" = andere Tabellen, die auf DIESE Tabelle zeigen.
// Composite FKs werden per Pascal-Aggregation zu einem Eintrag
// zusammengefasst (Felder mit ';' getrennt).
// FB 1.5: FBIdentifierCast für alle Identifier-Spalten.
// ============================================================
function TSimpleObjExtractor.GetTableReferences(
  const ATableName: string): TForeignKeyInfoArray;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
  i: Integer;
  CurrFK, CurrRefTable, CurrMasterTable: string;
  CurrForeignFields, CurrMasterFields: string;
begin
  SetLength(Result, 0);

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT ' +
      '  ' + FBIdentifierCast('rc.RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS FK_NAME, ' +
      '  ' + FBIdentifierCast('rc.RDB$RELATION_NAME', ServerVersionMajor) + ' AS REF_TABLE, ' +
      '  ' + FBIdentifierCast('flds_fk.RDB$FIELD_NAME', ServerVersionMajor) + ' AS REF_FIELD, ' +
      '  ' + FBIdentifierCast('rc2.RDB$RELATION_NAME', ServerVersionMajor) + ' AS MASTER_TABLE, ' +
      '  ' + FBIdentifierCast('flds_pk.RDB$FIELD_NAME', ServerVersionMajor) + ' AS MASTER_FIELD, ' +
      '  flds_fk.RDB$FIELD_POSITION ' +
      'FROM RDB$RELATION_CONSTRAINTS rc ' +
      'JOIN RDB$REF_CONSTRAINTS rfc ON rc.RDB$CONSTRAINT_NAME = rfc.RDB$CONSTRAINT_NAME ' +
      'JOIN RDB$INDEX_SEGMENTS flds_fk ON flds_fk.RDB$INDEX_NAME = rc.RDB$INDEX_NAME ' +
      'JOIN RDB$RELATION_CONSTRAINTS rc2 ON rc2.RDB$CONSTRAINT_NAME = rfc.RDB$CONST_NAME_UQ ' +
      'JOIN RDB$INDEX_SEGMENTS flds_pk ON flds_pk.RDB$INDEX_NAME = rc2.RDB$INDEX_NAME ' +
      '                            AND flds_fk.RDB$FIELD_POSITION = flds_pk.RDB$FIELD_POSITION ' +
      'WHERE rc.RDB$CONSTRAINT_TYPE = ' + QuotedStr('FOREIGN KEY') + ' ' +
      '  AND rc2.RDB$RELATION_NAME = ' + QuotedStr(ATableName) + ' ' +
      'ORDER BY rc.RDB$CONSTRAINT_NAME, flds_fk.RDB$FIELD_POSITION';

    Qry.ExecQuery;

    CurrFK := '';
    CurrRefTable := '';
    CurrMasterTable := '';
    CurrForeignFields := '';
    CurrMasterFields := '';

    while not Qry.EOF do
    begin
      // Neuer FK → vorherigen Eintrag abschließen
      if Trim(Qry.FieldByName('FK_NAME').AsString) <> CurrFK then
      begin
        if CurrFK <> '' then
        begin
          i := Length(Result);
          SetLength(Result, i + 1);
          Result[i].ConstraintName := CurrFK;
          Result[i].ForeignTable   := CurrRefTable;
          Result[i].ForeignFields  := CurrForeignFields;
          Result[i].MasterTable    := CurrMasterTable;
          Result[i].MasterFields   := CurrMasterFields;
        end;

        CurrFK           := Trim(Qry.FieldByName('FK_NAME').AsString);
        CurrRefTable     := Trim(Qry.FieldByName('REF_TABLE').AsString);
        CurrMasterTable  := Trim(Qry.FieldByName('MASTER_TABLE').AsString);
        CurrForeignFields := '';
        CurrMasterFields  := '';
      end;

      // Feld-Paare sammeln
      if CurrForeignFields <> '' then
        CurrForeignFields := CurrForeignFields + ';';
      if CurrMasterFields <> '' then
        CurrMasterFields := CurrMasterFields + ';';

      CurrForeignFields := CurrForeignFields + Trim(Qry.FieldByName('REF_FIELD').AsString);
      CurrMasterFields  := CurrMasterFields  + Trim(Qry.FieldByName('MASTER_FIELD').AsString);

      Qry.Next;
    end;

    // Letzten FK abschließen
    if CurrFK <> '' then
    begin
      i := Length(Result);
      SetLength(Result, i + 1);
      Result[i].ConstraintName := CurrFK;
      Result[i].ForeignTable   := CurrRefTable;
      Result[i].ForeignFields  := CurrForeignFields;
      Result[i].MasterTable    := CurrMasterTable;
      Result[i].MasterFields   := CurrMasterFields;
    end;

  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Liefert alle FOREIGN KEYs einer Tabelle, mit Feld-Paarung.
// Composite-FKs werden per Pascal-Aggregation zu einem Eintrag
// zusammengefasst (Felder mit ';' getrennt).
// FB 1.5: FBIdentifierCast auf alle Identifier-Spalten.
// Case: StripQuotes für RDB$-Lookup.
// ============================================================
function TSimpleObjExtractor.GetTableForeignKeys(
  const ATableName: string): TFBForeignKeyDefArray;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
  i, CurIdx: Integer;
  CurrFK, LookupName: string;
begin
  SetLength(Result, 0);

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;
  LookupName := StripQuotes(ATableName);

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT ' +
      '  ' + FBIdentifierCast('rc.RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS CONSTRAINT_NAME, ' +
      '  ' + FBIdentifierCast('refc.RDB$CONST_NAME_UQ', ServerVersionMajor) + ' AS KEY_NAME, ' +
      '  ' + FBIdentifierCast('isg.RDB$FIELD_NAME', ServerVersionMajor) + ' AS ON_FIELD, ' +
      '  ' + FBIdentifierCast('rc2.RDB$RELATION_NAME', ServerVersionMajor) + ' AS FOREIGN_TABLE, ' +
      '  ' + FBIdentifierCast('isg2.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FOREIGN_FIELD, ' +
      '  refc.RDB$UPDATE_RULE AS UPDATE_RULE, ' +
      '  refc.RDB$DELETE_RULE AS DELETE_RULE, ' +
      '  isg.RDB$FIELD_POSITION AS FIELD_POS ' +
      'FROM RDB$RELATION_CONSTRAINTS rc ' +
      'JOIN RDB$REF_CONSTRAINTS refc ON rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME ' +
      'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
      'JOIN RDB$RELATION_CONSTRAINTS rc2 ON rc2.RDB$CONSTRAINT_NAME = refc.RDB$CONST_NAME_UQ ' +
      'JOIN RDB$INDEX_SEGMENTS isg2 ON rc2.RDB$INDEX_NAME = isg2.RDB$INDEX_NAME ' +
      '                              AND isg2.RDB$FIELD_POSITION = isg.RDB$FIELD_POSITION ' +
      'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(LookupName) + ' ' +
      '  AND rc.RDB$CONSTRAINT_TYPE = ' + QuotedStr('FOREIGN KEY') + ' ' +
      'ORDER BY rc.RDB$CONSTRAINT_NAME, isg.RDB$FIELD_POSITION';

    Qry.ExecQuery;

    CurrFK := '';
    CurIdx := -1;

    while not Qry.EOF do
    begin
      if Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString) <> CurrFK then
      begin
        CurrFK := Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString);

        i := Length(Result);
        SetLength(Result, i + 1);
        CurIdx := i;

        Result[i].ConstraintName := CurrFK;
        Result[i].KeyName        := Trim(Qry.FieldByName('KEY_NAME').AsString);
        Result[i].RefTable       := Trim(Qry.FieldByName('FOREIGN_TABLE').AsString);
        Result[i].OnFields       := '';
        Result[i].RefFields      := '';
        Result[i].UpdateRule     := Trim(Qry.FieldByName('UPDATE_RULE').AsString);
        Result[i].DeleteRule     := Trim(Qry.FieldByName('DELETE_RULE').AsString);
      end;

      if Result[CurIdx].OnFields <> '' then
        Result[CurIdx].OnFields := Result[CurIdx].OnFields + ';';
      if Result[CurIdx].RefFields <> '' then
        Result[CurIdx].RefFields := Result[CurIdx].RefFields + ';';

      Result[CurIdx].OnFields := Result[CurIdx].OnFields +
        Trim(Qry.FieldByName('ON_FIELD').AsString);
      Result[CurIdx].RefFields := Result[CurIdx].RefFields +
        Trim(Qry.FieldByName('FOREIGN_FIELD').AsString);

      Qry.Next;
    end;

  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Liefert alle Table-Trigger einer Tabelle.
// CHECK-Constraint-Trigger werden ausgeschlossen — sie sind keine
// eigenständigen Trigger und gehören in den Check-Constraints-Tab.
// FB 1.5: FBIdentifierCast für Trigger-Namen.
// FB 3+ : UDR-Trigger ausgeschlossen.
// Case: RDB$ speichert Namen ohne Quotes — StripQuotes für den Filter.
// ============================================================
function TSimpleObjExtractor.GetTableTriggersRaw(
  const ATableName: string): TFBTriggerRawArray;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
  i: Integer;
  Filter: string;
begin
  SetLength(Result, 0);

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  if ServerVersionMajor < 3 then
    Filter := ''
  else
    Filter := '  AND (t.rdb$engine_name IS NULL OR TRIM(t.rdb$engine_name) = ' +
              QuotedStr('') + ') ';

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT ' +
      '  ' + FBIdentifierCast('t.rdb$trigger_name', ServerVersionMajor) + ' AS TRIGGER_NAME, ' +
      '  t.rdb$trigger_inactive AS TRIGGER_STATE ' +
      'FROM rdb$triggers t ' +
      'WHERE (t.rdb$system_flag IS NULL OR t.rdb$system_flag = 0) ' +
      '  AND NOT EXISTS ( ' +
      '    SELECT 1 FROM rdb$check_constraints cc ' +
      '    WHERE cc.rdb$trigger_name = t.rdb$trigger_name ' +
      '  ) ' +
      '  AND t.rdb$relation_name = ' + QuotedStr(StripQuotes(ATableName)) + ' ' +
      Filter +
      'ORDER BY t.rdb$trigger_name';

    Qry.ExecQuery;

    while not Qry.EOF do
    begin
      i := Length(Result);
      SetLength(Result, i + 1);

      Result[i].TriggerName := Trim(Qry.FieldByName('TRIGGER_NAME').AsString);

      if Qry.FieldByName('TRIGGER_STATE').IsNull then
        Result[i].IsActive := True
      else
        Result[i].IsActive := Qry.FieldByName('TRIGGER_STATE').AsInteger = 0;

      Qry.Next;
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Liest die Rohdaten eines Triggers.
// FB 1.5/2.x : ohne RDB$ENGINE_NAME und RDB$ENTRYPOINT
// FB 3.0+    : mit UDR-Feldern
// Case: Lookup über StripQuotes (RDB$ speichert ohne Quotes),
//       DDL-Namen werden später über MakeCaseSensitiveAuto erzeugt.
// ============================================================
function TSimpleObjExtractor.GetTriggerInfo(const ATriggerName: string): TFBTriggerInfo;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
  LookupName: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.IsActive := True;

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;
  LookupName := StripQuotes(ATriggerName);

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;

    if ServerVersionMajor < 3 then
      Qry.SQL.Text :=
        'SELECT ' +
        '  ' + FBIdentifierCast('RDB$TRIGGER_NAME', ServerVersionMajor) + ' AS TRIGGER_NAME, ' +
        '  ' + FBIdentifierCast('RDB$RELATION_NAME', ServerVersionMajor) + ' AS RELATION_NAME, ' +
        '  RDB$TRIGGER_SOURCE AS TRIGGER_SOURCE, ' +
        '  RDB$TRIGGER_TYPE AS TRIGGER_TYPE, ' +
        '  RDB$TRIGGER_SEQUENCE AS TRIGGER_SEQUENCE, ' +
        '  RDB$TRIGGER_INACTIVE AS TRIGGER_STATE ' +
        'FROM RDB$TRIGGERS ' +
        'WHERE RDB$TRIGGER_NAME = ' + QuotedStr(LookupName)
    else
      Qry.SQL.Text :=
        'SELECT ' +
        '  RDB$TRIGGER_NAME AS TRIGGER_NAME, ' +
        '  RDB$RELATION_NAME AS RELATION_NAME, ' +
        '  RDB$TRIGGER_SOURCE AS TRIGGER_SOURCE, ' +
        '  RDB$TRIGGER_TYPE AS TRIGGER_TYPE, ' +
        '  RDB$TRIGGER_SEQUENCE AS TRIGGER_SEQUENCE, ' +
        '  RDB$TRIGGER_INACTIVE AS TRIGGER_STATE, ' +
        '  RDB$ENGINE_NAME AS ENGINE_NAME, ' +
        '  RDB$ENTRYPOINT AS ENTRYPOINT ' +
        'FROM RDB$TRIGGERS ' +
        'WHERE RDB$TRIGGER_NAME = ' + QuotedStr(LookupName);

    Qry.ExecQuery;
    if Qry.EOF then
      Exit;

    Result.TriggerName := Trim(Qry.FieldByName('TRIGGER_NAME').AsString);

    if not Qry.FieldByName('RELATION_NAME').IsNull then
      Result.RelationName := Trim(Qry.FieldByName('RELATION_NAME').AsString);

    if not Qry.FieldByName('TRIGGER_SOURCE').IsNull then
      Result.TriggerSource := Trim(Qry.FieldByName('TRIGGER_SOURCE').AsString);

    if not Qry.FieldByName('TRIGGER_TYPE').IsNull then
      Result.TriggerType := Qry.FieldByName('TRIGGER_TYPE').AsInt64;

    if not Qry.FieldByName('TRIGGER_SEQUENCE').IsNull then
      Result.TriggerSequence := Qry.FieldByName('TRIGGER_SEQUENCE').AsInteger;

    if Qry.FieldByName('TRIGGER_STATE').IsNull then
      Result.IsActive := True
    else
      Result.IsActive := Qry.FieldByName('TRIGGER_STATE').AsInteger = 0;

    if ServerVersionMajor >= 3 then
    begin
      if not Qry.FieldByName('ENGINE_NAME').IsNull then
        Result.EngineName := Trim(Qry.FieldByName('ENGINE_NAME').AsString);
      if not Qry.FieldByName('ENTRYPOINT').IsNull then
        Result.EntryPoint := Trim(Qry.FieldByName('ENTRYPOINT').AsString);
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Dekodiert RDB$TRIGGERS.RDB$TRIGGER_TYPE in lesbare Teile.
// Wird von GetTriggerScript und der UI (View/Edit) gemeinsam genutzt.
// ============================================================
procedure TSimpleObjExtractor.DecodeTriggerType(ATriggerType: Int64;
  out AfterBefore, Event: string;
  out IsDDLTrigger, IsDBTrigger: Boolean);
var
  Code: Int64;
begin
  AfterBefore := '';
  Event := '';
  IsDDLTrigger := False;
  IsDBTrigger := False;

  Code := ATriggerType;

  // FB 3+ DDL-Trigger: Wert = 2^63 - event_code (riesige Zahl)
  // Ältere FB-3-Builds: negativer Wert
  // Erkennung: Wert < 0 oder Wert > 2^62
  if (Code < 0) or (Code > Int64($4000000000000000)) then
  begin
    IsDDLTrigger := True;
    if Code < 0 then
      Code := -Code
    else
      Code := Int64(QWord($8000000000000000) - QWord(Code));
  end
  else if (Code >= 8192) and (Code <= 8196) then
  begin
    IsDBTrigger := True;
  end;

  // Jetzt dekodieren
  if IsDDLTrigger then
  begin
    case Code of
      8194: Event := 'BEFORE ANY DDL STATEMENT';
      8195: Event := 'AFTER ANY DDL STATEMENT';
      8196: Event := 'BEFORE CREATE TABLE';
      8197: Event := 'AFTER CREATE TABLE';
      8198: Event := 'BEFORE ALTER TABLE';
      8199: Event := 'AFTER ALTER TABLE';
      8200: Event := 'BEFORE DROP TABLE';
      8201: Event := 'AFTER DROP TABLE';
      8202: Event := 'BEFORE CREATE PROCEDURE';
      8203: Event := 'AFTER CREATE PROCEDURE';
      8204: Event := 'BEFORE ALTER PROCEDURE';
      8205: Event := 'AFTER ALTER PROCEDURE';
      8206: Event := 'BEFORE DROP PROCEDURE';
      8207: Event := 'AFTER DROP PROCEDURE';
      8208: Event := 'BEFORE CREATE FUNCTION';
      8209: Event := 'AFTER CREATE FUNCTION';
      8210: Event := 'BEFORE ALTER FUNCTION';
      8211: Event := 'AFTER ALTER FUNCTION';
      8212: Event := 'BEFORE DROP FUNCTION';
      8213: Event := 'AFTER DROP FUNCTION';
    else
      Event := 'UNKNOWN DDL EVENT (' + IntToStr(Code) + ')';
    end;
    Exit;
  end;

  if IsDBTrigger then
  begin
    case Code of
      8192: Event := 'ON CONNECT';
      8193: Event := 'ON DISCONNECT';
      8194: Event := 'ON TRANSACTION START';
      8195: Event := 'ON TRANSACTION COMMIT';
      8196: Event := 'ON TRANSACTION ROLLBACK';
    else
      Event := 'UNKNOWN DB EVENT (' + IntToStr(Code) + ')';
    end;
    Exit;
  end;

  // Table-Trigger (Code 1..114)
  case Integer(Code) of
    1:   begin AfterBefore := 'BEFORE'; Event := 'INSERT'; end;
    2:   begin AfterBefore := 'AFTER';  Event := 'INSERT'; end;
    3:   begin AfterBefore := 'BEFORE'; Event := 'UPDATE'; end;
    4:   begin AfterBefore := 'AFTER';  Event := 'UPDATE'; end;
    5:   begin AfterBefore := 'BEFORE'; Event := 'DELETE'; end;
    6:   begin AfterBefore := 'AFTER';  Event := 'DELETE'; end;
    17:  begin AfterBefore := 'BEFORE'; Event := 'INSERT OR UPDATE'; end;
    18:  begin AfterBefore := 'AFTER';  Event := 'INSERT OR UPDATE'; end;
    25:  begin AfterBefore := 'BEFORE'; Event := 'INSERT OR DELETE'; end;
    26:  begin AfterBefore := 'AFTER';  Event := 'INSERT OR DELETE'; end;
    27:  begin AfterBefore := 'BEFORE'; Event := 'UPDATE OR DELETE'; end;
    28:  begin AfterBefore := 'AFTER';  Event := 'UPDATE OR DELETE'; end;
    113: begin AfterBefore := 'BEFORE'; Event := 'INSERT OR UPDATE OR DELETE'; end;
    114: begin AfterBefore := 'AFTER';  Event := 'INSERT OR UPDATE OR DELETE'; end;
  else
    AfterBefore := 'UNKNOWN';
    Event := 'UNKNOWN (' + IntToStr(Code) + ')';
  end;
end;

// ============================================================
// Baut das vollständige Trigger-Script.
// Case: DDL-Namen via MakeCaseSensitiveAuto, RDB$-Lookup via StripQuotes.
//
// Versions-Weiche:
//   FB 1.5     : ALTER TRIGGER name ...         (kein FOR/ON table)
//   FB 2.0/2.5 : RECREATE TRIGGER name ... ON table
//   FB 3.0+    : CREATE OR ALTER TRIGGER name ... ON table
// ============================================================
procedure TSimpleObjExtractor.GetTriggerScript(const ATriggerName: string; AItems: TStrings);
var
  Info: TFBTriggerInfo;
  DDLTriggerName, DDLTableName: string;
  AfterBefore, Event: string;
  IsDDLTrigger, IsDBTrigger, IsUDRTrigger: Boolean;
  SL: TStringList;
  i, LastIdx: Integer;
  ServerVersionMajor: Word;
  CreateKeyword: string;
  Body: string;
begin
  AItems.Clear;

  Info := GetTriggerInfo(ATriggerName);
  if Info.TriggerName = '' then
    Exit;

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  // Case: DDL-Namen mit Auto-Quoting (RDB$ liefert unquoted)
  DDLTriggerName := MakeCaseSensitiveAuto(Info.TriggerName);
  DDLTableName   := MakeCaseSensitiveAuto(Info.RelationName);

  IsUDRTrigger := Info.EngineName <> '';

  // Trigger-Typ dekodieren (gemeinsamer Helper mit lmViewTriggerClick)
  DecodeTriggerType(Info.TriggerType, AfterBefore, Event,
    IsDDLTrigger, IsDBTrigger);

  // ============================================================
  // Body vorbereiten — "AS"-Präfix entfernen, falls vorhanden.
  // RDB$TRIGGER_SOURCE enthält auf FB 1.5 den Body inkl. "AS".
  // ============================================================
  Body := Trim(Info.TriggerSource);
  if (Length(Body) >= 2) and (UpperCase(Copy(Body, 1, 2)) = 'AS') then
    Body := Trim(Copy(Body, 3, MaxInt));

  // ============================================================
  // Header aufbauen
  // ============================================================
  AItems.Add('SET TERM ^;');
  AItems.Add('');

  if ServerVersionMajor < 2 then
  begin
    // =========================================================
    // FB 1.5 — ALTER TRIGGER (kein FOR/ON table, Tabelle ist fix)
    // =========================================================
    AItems.Add('ALTER TRIGGER ' + DDLTriggerName);

    if Info.IsActive then
      AItems.Add('ACTIVE')
    else
      AItems.Add('INACTIVE');

    AItems.Add(AfterBefore + ' ' + Event);

    if Info.TriggerSequence > 0 then
      AItems.Add('POSITION ' + IntToStr(Info.TriggerSequence));
  end
  else
  begin
    // =========================================================
    // FB 2.0+ — RECREATE (2.x) / CREATE OR ALTER (3+)
    // =========================================================
    if ServerVersionMajor >= 3 then
      CreateKeyword := 'CREATE OR ALTER TRIGGER '
    else
      CreateKeyword := 'RECREATE TRIGGER ';

    AItems.Add(CreateKeyword + DDLTriggerName);

    if Info.IsActive then
      AItems.Add('ACTIVE')
    else
      AItems.Add('INACTIVE');

    if IsDDLTrigger or IsDBTrigger then
      AItems.Add(Event)
    else
    begin
      AItems.Add(AfterBefore + ' ' + Event);
      if DDLTableName <> '' then
        AItems.Add('ON ' + DDLTableName);
    end;
  end;

  // ============================================================
  // UDR-Trigger — externer Aufruf statt PSQL-Body
  // ============================================================
  if IsUDRTrigger then
  begin
    if Info.EntryPoint <> '' then
      AItems.Add('EXTERNAL NAME ' + QuotedStr(Info.EntryPoint));
    if Info.EngineName <> '' then
      AItems.Add('ENGINE ' + Info.EngineName);
    if Body <> '' then
      AItems.Add('AS ' + QuotedStr(Body));
    AItems.Add('^');
    AItems.Add('');
    AItems.Add('SET TERM ;^');
    Exit;
  end;

  // ============================================================
  // PSQL-Body
  // ============================================================
  AItems.Add('AS');

  if Body <> '' then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Body;
      for i := 0 to SL.Count - 1 do
        AItems.Add(SL[i]);
    finally
      SL.Free;
    end;
  end
  else
  begin
    AItems.Add('BEGIN');
    AItems.Add('  EXIT;');
    AItems.Add('END');
  end;

  // Terminator an letzter nicht-leerer Zeile
  LastIdx := AItems.Count - 1;
  while (LastIdx >= 0) and (Trim(AItems[LastIdx]) = '') do
    Dec(LastIdx);

  if LastIdx >= 0 then
    AItems[LastIdx] := TrimRight(AItems[LastIdx]) + ' ^'
  else
    AItems.Add('^');

  AItems.Add('');
  AItems.Add('SET TERM ;^');
end;

// ============================================================
// Basis-Typen aus RDB$TYPES
// FB 1.5: FBIdentifierCast für TYPE_NAME (CHAR(31) → VARCHAR)
// ============================================================
procedure TSimpleObjExtractor.GetBasicFieldTypes(AItems: TStrings);
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
begin
  AItems.Clear;

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT ' + FBIdentifierCast('RDB$TYPE_NAME', ServerVersionMajor) + ' ' +
      'FROM RDB$TYPES ' +
      'WHERE RDB$FIELD_NAME = ' + QuotedStr('RDB$FIELD_TYPE') + ' ' +
      'ORDER BY RDB$TYPE_NAME';

    Qry.ExecQuery;
    while not Qry.EOF do
    begin
      AItems.Add(Trim(Qry.Fields[0].AsString));
      Qry.Next;
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Erweiterte Typen (DECIMAL, NUMERIC, CHAR, UUID) — nur wenn
// in der aktuellen Datenbank tatsächlich vorhanden.
// FB 1.5: RDB$FIELD_PRECISION fehlt → Workaround über FIELD_SCALE
// FB 1.5: ROWS 1 fehlt → FIRST 1
// ============================================================
procedure TSimpleObjExtractor.GetExtendedFieldTypes(AItems: TStrings);
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
  HasDecimal, HasNumeric, HasChar, HasUUID: Boolean;
begin
  AItems.Clear;

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;

    // DECIMAL / NUMERIC
    if ServerVersionMajor < 2 then
      // FB 1.5 hat keine RDB$FIELD_PRECISION → über Scale erkennen
      Qry.SQL.Text :=
        'SELECT FIRST 1 1 FROM RDB$FIELDS ' +
        'WHERE RDB$FIELD_TYPE IN (7, 8, 16) AND RDB$FIELD_SCALE <> 0'
    else
      Qry.SQL.Text :=
        'SELECT FIRST 1 1 FROM RDB$FIELDS ' +
        'WHERE RDB$FIELD_PRECISION IS NOT NULL AND RDB$FIELD_PRECISION > 0';

    Qry.ExecQuery;
    HasDecimal := not Qry.EOF;
    HasNumeric := HasDecimal;
    Qry.Close;

    // CHAR
    Qry.SQL.Text := 'SELECT FIRST 1 1 FROM RDB$FIELDS WHERE RDB$FIELD_TYPE = 14';
    Qry.ExecQuery;
    HasChar := not Qry.EOF;
    Qry.Close;

    // UUID (CHAR(16) OCTETS — charset id 1)
    Qry.SQL.Text :=
      'SELECT FIRST 1 1 FROM RDB$FIELDS ' +
      'WHERE RDB$FIELD_TYPE = 14 ' +
      '  AND RDB$FIELD_LENGTH = 16 ' +
      '  AND RDB$CHARACTER_SET_ID = 1';
    Qry.ExecQuery;
    HasUUID := not Qry.EOF;
    Qry.Close;

    if HasDecimal then AItems.Add('DECIMAL');
    if HasNumeric then AItems.Add('NUMERIC');
    if HasChar then AItems.Add('CHAR');
    if HasUUID then AItems.Add('UUID');
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Typ-Liste aufräumen — Legacy-Typen entfernen, Aliase normalisieren
// Reine String-Manipulation, keine DB.
// ============================================================
procedure CleanFirebirdTypeListEx(AList: TStrings);
var
  i: Integer;
  TypeName: string;
begin
  for i := AList.Count - 1 downto 0 do
  begin
    TypeName := Trim(UpperCase(AList[i]));

    if TypeName = 'VARYING' then
      AList[i] := 'VARCHAR'

    else if TypeName = 'TEXT' then
      AList[i] := 'BLOB SUB_TYPE TEXT'

    else if TypeName = 'DOUBLE' then
      AList[i] := 'DOUBLE PRECISION'

    else if (TypeName = 'BLOB') or
            (TypeName = 'BOOLEAN') or
            (TypeName = 'DATE') or
            (TypeName = 'DECFLOAT(16)') or
            (TypeName = 'DECFLOAT(34)') or
            (TypeName = 'DOUBLE PRECISION') or
            (TypeName = 'FLOAT') or
            (TypeName = 'INT128') or
            (TypeName = 'TIME') or
            (TypeName = 'TIME WITH TIME ZONE') or
            (TypeName = 'TIMESTAMP') or
            (TypeName = 'TIMESTAMP WITH TIME ZONE') or
            (TypeName = 'DECIMAL') or
            (TypeName = 'NUMERIC') or
            (TypeName = 'CHAR') or
            (TypeName = 'UUID') or
            (TypeName = 'VARCHAR') then
       // gültiger Typ — nichts tun

    else if (TypeName = 'CSTRING') or
            (TypeName = 'BLOB_ID') or
            (TypeName = 'QUAD') then
      AList.Delete(i)

    else if TypeName = 'LONG' then
      AList[i] := 'INTEGER'
    else if TypeName = 'SHORT' then
      AList[i] := 'SMALLINT'
    else if TypeName = 'INT64' then
      AList[i] := 'BIGINT'

    else if (TypeName = '') or
            (Pos('MON$', TypeName) = 1) or
            (Pos('SEC$', TypeName) = 1) then
      // ignorieren
    else
      // unbekannt — belassen
  end;
end;

// ============================================================
// Kombination Basic + Extended, deduplicated, dann cleanup
// ============================================================
procedure TSimpleObjExtractor.GetAllFieldTypes(AItems: TStrings);
var
  BasicList, ExtendedList: TStringList;
  i: Integer;
begin
  AItems.Clear;

  BasicList := TStringList.Create;
  ExtendedList := TStringList.Create;
  try
    GetBasicFieldTypes(BasicList);
    GetExtendedFieldTypes(ExtendedList);

    for i := 0 to BasicList.Count - 1 do
      if AItems.IndexOf(BasicList[i]) = -1 then
        AItems.Add(BasicList[i]);

    for i := 0 to ExtendedList.Count - 1 do
      if AItems.IndexOf(ExtendedList[i]) = -1 then
        AItems.Add(ExtendedList[i]);
  finally
    BasicList.Free;
    ExtendedList.Free;
  end;

  CleanFirebirdTypeListEx(AItems);
end;

// ============================================================
// Liefert die Feld-Länge eines Domain-Namens aus RDB$FIELDS.
// Wird von GetFieldTypeSize als Fallback genutzt.
// ============================================================
function TSimpleObjExtractor.GetDomainFieldSize(const ADomainName: string): Integer;
var
  Qry: TIBSQL;
begin
  Result := 0;

  if Trim(ADomainName) = '' then
    Exit;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT RDB$FIELD_LENGTH ' +
      'FROM RDB$FIELDS ' +
      'WHERE RDB$FIELD_NAME = ' + QuotedStr(UpperCase(ADomainName));

    Qry.ExecQuery;
    if not Qry.EOF then
    begin
      if not Qry.FieldByName('RDB$FIELD_LENGTH').IsNull then
        Result := Qry.FieldByName('RDB$FIELD_LENGTH').AsInteger;
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Default-Größe für einen Feldtyp oder Domain-Namen.
// Bekannte Basis-Typen sind hard-coded; alles andere geht an
// RDB$FIELDS (Domain-Auflösung).
// ============================================================
function TSimpleObjExtractor.GetFieldTypeSize(const ATypeName: string): Integer;
var
  TypeName: string;
begin
  TypeName := LowerCase(Trim(ATypeName));

  if TypeName = 'varchar' then Exit(50);
  if TypeName = 'char' then Exit(20);
  if TypeName = 'smallint' then Exit(2);
  if TypeName = 'integer' then Exit(4);
  if TypeName = 'bigint' then Exit(8);
  if TypeName = 'int128' then Exit(16);
  if TypeName = 'float' then Exit(4);
  if TypeName = 'timestamp' then Exit(8);
  if TypeName = 'timestamp with time zone' then Exit(10);
  if TypeName = 'date' then Exit(4);
  if TypeName = 'time' then Exit(4);
  if TypeName = 'time with time zone' then Exit(6);
  if TypeName = 'double precision' then Exit(8);

  // Fallback: Domain? — in RDB$FIELDS nachschauen
  Result := GetDomainFieldSize(ATypeName);
end;


// ============================================================
// Liefert alle User/Rollen mit Berechtigungen auf ein Objekt.
// EINE Query — Aggregation in Pascal (User → Rechte-String).
// FB 1.5: FBIdentifierCast für RDB$USER (CHAR(31)-Bug).
// Case: StripQuotes für RDB$-Lookup.
// ============================================================
function TSimpleObjExtractor.GetObjectPermissions(
  const AObjectName: string): TFBPermissionArray;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
  Idx, CurrentIdx: Integer;
  UserName: string;
  IsRole: Boolean;
  Priv, Item: string;
  LookupName: string;
begin
  SetLength(Result, 0);

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;
  LookupName := StripQuotes(AObjectName);

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT ' +
      '  ' + FBIdentifierCast('RDB$USER', ServerVersionMajor) + ' AS USER_NAME, ' +
      '  RDB$USER_TYPE AS USER_TYPE, ' +
      '  RDB$PRIVILEGE AS PRIV, ' +
      '  RDB$GRANT_OPTION AS GRANT_OPT ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$RELATION_NAME = ' + QuotedStr(LookupName) + ' ' +
      'ORDER BY RDB$USER_TYPE, RDB$USER, RDB$PRIVILEGE';

    Qry.ExecQuery;

    // Aggregation: da nach USER sortiert, kommen alle Privilegien
    // eines Users am Stück. Wir bauen Einträge User für User auf.
    CurrentIdx := -1;
    UserName := '';

    while not Qry.EOF do
    begin
      // User-Wechsel?
      if Trim(Qry.FieldByName('USER_NAME').AsString) <> UserName then
      begin
        UserName := Trim(Qry.FieldByName('USER_NAME').AsString);
        IsRole := Qry.FieldByName('USER_TYPE').AsInteger = 13;

        Idx := Length(Result);
        SetLength(Result, Idx + 1);
        Result[Idx].UserName := UserName;
        Result[Idx].IsRole := IsRole;
        Result[Idx].Privileges := '';
        CurrentIdx := Idx;
      end;

      // Privileg anhängen
      Priv := Trim(Qry.FieldByName('PRIV').AsString);
      if Qry.FieldByName('GRANT_OPT').AsInteger <> 0 then
        Item := Priv + 'G'
      else
        Item := Priv;

      if Result[CurrentIdx].Privileges <> '' then
        Result[CurrentIdx].Privileges := Result[CurrentIdx].Privileges + ',';
      Result[CurrentIdx].Privileges := Result[CurrentIdx].Privileges + Item;

      Qry.Next;
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

// ============================================================
// Liefert alle Objekte eines Typs, auf die ein User Rechte hat.
// AObjectType: 0 = Table/View, 5 = Procedure, 13 = Role
// Format je Eintrag: ObjectName + WithGrant-Flag.
// FB 1.5: FBIdentifierCast für RDB$RELATION_NAME (CHAR(31)-Bug).
// Case: StripQuotes für RDB$-Lookup.
// ============================================================
function TSimpleObjExtractor.GetUserObjectGrants(
  const AUserName: string;
  AObjectType: Integer): TFBUserGrantArray;
var
  Qry: TIBSQL;
  ServerVersionMajor: Word;
  i: Integer;
  LookupUser: string;
begin
  SetLength(Result, 0);

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;
  LookupUser := StripQuotes(AUserName);

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := True;
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  Qry := TIBSQL.Create(FIBDatabase);
  try
    Qry.Transaction := FIBTransaction;
    Qry.SQL.Text :=
      'SELECT DISTINCT ' +
      '  ' + FBIdentifierCast('RDB$RELATION_NAME', ServerVersionMajor) + ' AS OBJ_NAME, ' +
      '  RDB$GRANT_OPTION AS GRANT_OPT ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$USER = ' + QuotedStr(LookupUser) + ' ' +
      '  AND RDB$OBJECT_TYPE = ' + IntToStr(AObjectType) + ' ' +
      'ORDER BY RDB$RELATION_NAME';

    Qry.ExecQuery;

    while not Qry.EOF do
    begin
      i := Length(Result);
      SetLength(Result, i + 1);

      Result[i].ObjectName := Trim(Qry.FieldByName('OBJ_NAME').AsString);
      Result[i].WithGrant :=
        (not Qry.FieldByName('GRANT_OPT').IsNull) and
        (Qry.FieldByName('GRANT_OPT').AsInteger <> 0);

      Qry.Next;
    end;
  finally
    Qry.Free;
    EndTransaction;
  end;
end;

procedure TSimpleObjExtractor.Extract(
  ObjectType: TObjectType;
  ObjectName : String;
  ExtractTypes: TExtractTypes;
  Quoted: boolean;
  var AItems: TStrings);
var
  tmpQuoted: boolean;
  ExtractObjectType: TExtractObjectTypes;
  SQL: string;
  Qry: TIBSQL;
  LastConstraint: string;
  CurrConstraint: string;
  FieldsList: string;
  RefConstraint: string;
  IsUnique: boolean;
  RefTable: string;
  ServerVersionMajor: Word;
begin
  tmpQuoted := FIBExtract.AlwaysQuoteIdentifiers;
  FIBExtract.AlwaysQuoteIdentifiers := Quoted;

  ResetExtract;

  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  try
    if not FIBTransaction.InTransaction then
      FIBTransaction.StartTransaction;

    case ObjectType of
      otUDRFunctions:
        GetUDRFunction(FIBDatabase, ObjectName, AItems);

      otUDRProcedures:
        begin
          // TODO
        end;

      // ============================================================
      // Primary Keys
      // ============================================================
      otPrimaryKeys:
        begin
          SQL :=
            'SELECT ' +
            FBIdentifierCast('rc.RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS CONSTRAINT_NAME, ' +
            FBIdentifierCast('isg.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
            'isg.RDB$FIELD_POSITION ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rc.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
            'ORDER BY rc.RDB$CONSTRAINT_NAME, isg.RDB$FIELD_POSITION';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;

            LastConstraint := '';
            FieldsList := '';

            while not Qry.EOF do
            begin
              CurrConstraint := Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString);

              if CurrConstraint <> LastConstraint then
              begin
                if LastConstraint <> '' then
                  AItems.Add('PRIMARY KEY (' + FieldsList + ')');
                LastConstraint := CurrConstraint;
                FieldsList := '';
              end;

              if FieldsList <> '' then
                FieldsList := FieldsList + ', ';
              FieldsList := FieldsList + Trim(Qry.FieldByName('FIELD_NAME').AsString);

              Qry.Next;
            end;

            if LastConstraint <> '' then
              AItems.Add('PRIMARY KEY (' + FieldsList + ')');

          finally
            Qry.Free;
          end;
        end;

      // ============================================================
      // Foreign Keys
      // ============================================================
      otForeignKeys:
        begin
          // Format: FK_NAME: FIELD1, FIELD2 -> REFERENCED_TABLE
          // Zusätzlicher JOIN auf RDB$RELATION_CONSTRAINTS (rc2) holt den
          // Tabellennamen der referenzierten Tabelle.
          SQL :=
            'SELECT ' +
            FBIdentifierCast('rc.RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS CONSTRAINT_NAME, ' +
            FBIdentifierCast('isg.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
            'isg.RDB$FIELD_POSITION, ' +
            FBIdentifierCast('rc2.RDB$RELATION_NAME', ServerVersionMajor) + ' AS REF_TABLE ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'JOIN RDB$REF_CONSTRAINTS refc ON rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'JOIN RDB$RELATION_CONSTRAINTS rc2 ON rc2.RDB$CONSTRAINT_NAME = refc.RDB$CONST_NAME_UQ ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rc.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' ' +
            'ORDER BY rc.RDB$CONSTRAINT_NAME, isg.RDB$FIELD_POSITION';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;

            LastConstraint := '';
            FieldsList := '';
            RefTable := '';

            while not Qry.EOF do
            begin
              CurrConstraint := Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString);

              if CurrConstraint <> LastConstraint then
              begin
                if LastConstraint <> '' then
                  AItems.Add(LastConstraint + ': ' + FieldsList + ' -> ' + RefTable);

                LastConstraint := CurrConstraint;
                FieldsList := '';
                RefTable := Trim(Qry.FieldByName('REF_TABLE').AsString);
              end;

              if FieldsList <> '' then
                FieldsList := FieldsList + ', ';
              FieldsList := FieldsList + Trim(Qry.FieldByName('FIELD_NAME').AsString);

              Qry.Next;
            end;

            if LastConstraint <> '' then
              AItems.Add(LastConstraint + ': ' + FieldsList + ' -> ' + RefTable);

          finally
            Qry.Free;
          end;
        end;


      // ============================================================
      // Unique Constraints
      // ============================================================
      otUniqueConstraints:
        begin
          SQL :=
            'SELECT ' +
            FBIdentifierCast('rc.RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS CONSTRAINT_NAME, ' +
            FBIdentifierCast('isg.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
            'isg.RDB$FIELD_POSITION ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rc.RDB$CONSTRAINT_TYPE = ''UNIQUE'' ' +
            'ORDER BY rc.RDB$CONSTRAINT_NAME, isg.RDB$FIELD_POSITION';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;

            LastConstraint := '';
            FieldsList := '';

            while not Qry.EOF do
            begin
              CurrConstraint := Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString);

              if CurrConstraint <> LastConstraint then
              begin
                if LastConstraint <> '' then
                  AItems.Add(LastConstraint + ': UNIQUE (' + FieldsList + ')');

                LastConstraint := CurrConstraint;
                FieldsList := '';
              end;

              if FieldsList <> '' then
                FieldsList := FieldsList + ', ';
              FieldsList := FieldsList + Trim(Qry.FieldByName('FIELD_NAME').AsString);

              Qry.Next;
            end;

            if LastConstraint <> '' then
              AItems.Add(LastConstraint + ': UNIQUE (' + FieldsList + ')');

          finally
            Qry.Free;
          end;
        end;

      // ============================================================
      // Check Constraints
      // ============================================================
      // ============================================================
      // Check Constraints — nur Namen für den Tree
      // Die vollständige CHECK-Expression liefert GetCheckConstraintSource.
      // ============================================================
      otCheckConstraints:
        begin
          SQL :=
            'SELECT ' +
            FBIdentifierCast('rc.RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS CONSTRAINT_NAME ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            '  AND rc.RDB$CONSTRAINT_TYPE = ' + QuotedStr('CHECK') + ' ' +
            'ORDER BY rc.RDB$CONSTRAINT_NAME';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;

            while not Qry.EOF do
            begin
              AItems.Add(Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString) + ': CHECK');
              Qry.Next;
            end;
          finally
            Qry.Free;
          end;
        end;

      // ============================================================
      // Not Null Constraints
      // ============================================================
      otNotNullConstraints:
        begin
          SQL :=
            'SELECT ' +
            FBIdentifierCast('rf.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
            'rf.RDB$FIELD_POSITION ' +
            'FROM RDB$RELATION_FIELDS rf ' +
            'WHERE rf.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rf.RDB$NULL_FLAG = 1 ' +
            'ORDER BY rf.RDB$FIELD_POSITION';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;

            while not Qry.EOF do
            begin
              AItems.Add(Trim(Qry.FieldByName('FIELD_NAME').AsString) + ': NOT NULL');
              Qry.Next;
            end;

          finally
            Qry.Free;
          end;
        end;

      // ============================================================
      // Indices (ohne Constraints)
      // ============================================================
      otIndexes:
        begin
          SQL :=
            'SELECT ' +
            FBIdentifierCast('i.RDB$INDEX_NAME', ServerVersionMajor) + ' AS INDEX_NAME, ' +
            'i.RDB$UNIQUE_FLAG, ' +
            FBIdentifierCast('isg.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
            'isg.RDB$FIELD_POSITION ' +
            'FROM RDB$INDICES i ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON i.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'WHERE i.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND NOT EXISTS (' +
            '  SELECT 1 FROM RDB$RELATION_CONSTRAINTS rc ' +
            '  WHERE rc.RDB$INDEX_NAME = i.RDB$INDEX_NAME ' +
            '  AND rc.RDB$CONSTRAINT_TYPE IS NOT NULL' +
            ') ' +
            'ORDER BY i.RDB$INDEX_NAME, isg.RDB$FIELD_POSITION';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;

            LastConstraint := '';
            FieldsList := '';
            IsUnique := False;

            while not Qry.EOF do
            begin
              CurrConstraint := Trim(Qry.FieldByName('INDEX_NAME').AsString);

              if CurrConstraint <> LastConstraint then
              begin
                if LastConstraint <> '' then
                begin
                  if IsUnique then
                    AItems.Add(LastConstraint + ': UNIQUE ON (' + FieldsList + ')')
                  else
                    AItems.Add(LastConstraint + ': ON (' + FieldsList + ')');
                end;

                LastConstraint := CurrConstraint;
                FieldsList := '';
                IsUnique := (Qry.FieldByName('RDB$UNIQUE_FLAG').AsInteger = 1);
              end;

              if FieldsList <> '' then
                FieldsList := FieldsList + ', ';
              FieldsList := FieldsList + Trim(Qry.FieldByName('FIELD_NAME').AsString);

              Qry.Next;
            end;

            if LastConstraint <> '' then
            begin
              if IsUnique then
                AItems.Add(LastConstraint + ': UNIQUE ON (' + FieldsList + ')')
              else
                AItems.Add(LastConstraint + ': ON (' + FieldsList + ')');
            end;

          finally
            Qry.Free;
          end;
        end;

      // ============================================================
      // Table References (eingehende FKs)
      // ============================================================
      otTableReferences:
        begin
          SQL :=
            'SELECT ' +
            FBIdentifierCast('rc.RDB$CONSTRAINT_NAME', ServerVersionMajor) + ' AS CONSTRAINT_NAME, ' +
            FBIdentifierCast('flds_fk.RDB$FIELD_NAME', ServerVersionMajor) + ' AS FIELD_NAME, ' +
            'flds_fk.RDB$FIELD_POSITION, ' +
            FBIdentifierCast('rc.RDB$RELATION_NAME', ServerVersionMajor) + ' AS RELATION_NAME ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'JOIN RDB$REF_CONSTRAINTS rfc ON rc.RDB$CONSTRAINT_NAME = rfc.RDB$CONSTRAINT_NAME ' +
            'JOIN RDB$INDEX_SEGMENTS flds_fk ON rc.RDB$INDEX_NAME = flds_fk.RDB$INDEX_NAME ' +
            'JOIN RDB$RELATION_CONSTRAINTS rc2 ON rc2.RDB$CONSTRAINT_NAME = rfc.RDB$CONST_NAME_UQ ' +
            'WHERE rc.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' ' +
            '  AND rc2.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'ORDER BY rc.RDB$CONSTRAINT_NAME, flds_fk.RDB$FIELD_POSITION';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;

            LastConstraint := '';
            FieldsList := '';
            RefTable := '';

            while not Qry.EOF do
            begin
              CurrConstraint := Trim(Qry.FieldByName('CONSTRAINT_NAME').AsString);

              if CurrConstraint <> LastConstraint then
              begin
                if LastConstraint <> '' then
                  AItems.Add(LastConstraint + ': ' + FieldsList + ' -> ' + RefTable);

                LastConstraint := CurrConstraint;
                FieldsList := '';
                RefTable := Trim(Qry.FieldByName('RELATION_NAME').AsString);
              end;

              if FieldsList <> '' then
                FieldsList := FieldsList + ', ';
              FieldsList := FieldsList + Trim(Qry.FieldByName('FIELD_NAME').AsString);

              Qry.Next;
            end;

            if LastConstraint <> '' then
              AItems.Add(LastConstraint + ': ' + FieldsList + ' -> ' + RefTable);

          finally
            Qry.Free;
            EndTransaction;
          end;
        end;

      else
        // Standard IBExtract für alle anderen Typen
        ExtractObjectType := TBTypeToIBXType(ObjectType);
        FIBExtract.ExtractObject(ExtractObjectType, ObjectName, ExtractTypes);
        if FIBExtract.Items.Count > 0 then
        begin
          FixArraySyntax(FIBExtract.Items);
          AItems.Assign(FIBExtract.Items);
        end;
    end;

    if Assigned(FIBDatabase) and Assigned(FIBDatabase.DefaultTransaction) then
    begin
      if FIBDatabase.DefaultTransaction.InTransaction then
        FIBDatabase.DefaultTransaction.Rollback;
    end;

  finally
    FIBExtract.AlwaysQuoteIdentifiers := tmpQuoted;
  end;
end;

procedure TSimpleObjExtractor.ExtractToTreeNode(
  ObjectType: TObjectType;
  ObjectName : String;
  ExtractTypes: TExtractTypes;
  Quoted: boolean;
  var Node: TTreeNode; AImageIndex: integer);

var
  Items: TStringList;
  i: Integer;
  Line: string;
  TmpNode: TTreeNode;
begin
  if Node = nil then Exit;

  // Alte Children entfernen
  Node.DeleteChildren;

  Items := TStringList.Create;
  try
    Extract(ObjectType, ObjectName, ExtractTypes, Quoted, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      Line := Trim(Items[i]);

      if Line = '' then Continue;
      if Pos('/*', Line) = 1 then Continue;
      if Pos('--', Line) = 1 then Continue;

      TmpNode := Node.TreeView.Items.AddChild(Node, Line);
      TmpNode.ImageIndex := AImageIndex;
      TPNodeInfos(TmpNode.Data)^.dbIndex := FDBIndex;
      TPNodeInfos(TmpNode.Data)^.ObjectType := FBTypeToTreeViewType(ObjectType);
    end;

  finally
    Items.Free;
  end;
end;

{procedure TSimpleObjExtractor.ExtractTableFields(
  ATableName: string;
  var AItems: TStringList;
  Quoted: boolean; Delimiter: char; RemoveLastComma: boolean);
var
  RawFields: TFBFieldRawArray;
  i: Integer;
  FieldName, FieldType, BaseType, Line, DefSrc: string;
  IsDomain: Boolean;
begin
  AItems.Clear;

  RawFields := GetTableFieldsRaw(ATableName);

  for i := 0 to High(RawFields) do
  begin
    FieldName := RawFields[i].FieldName;

    // Anführungszeichen nur wenn gewünscht UND Name case-sensitive
    if Quoted and IsObjectNameCaseSensitive(FieldName) then
      FieldName := '"' + FieldName + '"';

    // Computed Field — nur COMPUTED BY zeigen, ohne Typ
    if Trim(RawFields[i].ComputedSource) <> '' then
    begin
      Line := FieldName + Delimiter + 'COMPUTED BY (' +
              Trim(RawFields[i].ComputedSource) + ')';
      AItems.Add(Line);
      Continue;
    end;

    // Base-Type immer ermitteln (auch wenn Domain)
    BaseType := GetFBTypeName(
      RawFields[i].FieldType,
      RawFields[i].FieldSubType,
      RawFields[i].FieldLength,
      RawFields[i].FieldPrecision,
      RawFields[i].FieldScale,
      RawFields[i].CharacterSetName,
      RawFields[i].CharacterLength
    );

    // Domain-basiert? — RDB$... = intern generiert, alles andere = User-Domain
    IsDomain := (RawFields[i].FieldSource <> '') and
                (not IsFieldDomainSystemGenerated(RawFields[i].FieldSource));

    if IsDomain then
      // Domain mit aufgelöstem Typ: JOBCODE (VARCHAR(5))
      FieldType := RawFields[i].FieldSource + ' (' + BaseType + ')'
    else
      // Nur Base-Type: INTEGER, VARCHAR(100), TIMESTAMP, ...
      FieldType := BaseType;

    Line := FieldName + Delimiter + FieldType;

    // NOT NULL
    if RawFields[i].NotNull then
      Line := Line + ' NOT NULL';

    // DEFAULT
    DefSrc := Trim(RawFields[i].DefaultSource);
    if DefSrc <> '' then
    begin
      // Firebird liefert bei manchen Versionen das Wort "DEFAULT" bereits mit
      if UpperCase(Copy(DefSrc, 1, 7)) = 'DEFAULT' then
        Line := Line + ' ' + DefSrc
      else
        Line := Line + ' DEFAULT ' + DefSrc;
    end;

    AItems.Add(Line);
  end;
end;}

procedure TSimpleObjExtractor.ExtractTableFields(
  ATableName: string;
  var AItems: TStringList;
  Quoted: boolean; Delimiter: char; RemoveLastComma: boolean);
var
  RawFields: TFBFieldRawArray;
  i: Integer;
  FieldName, FieldType, BaseType, Line, DefSrc: string;
  IsDomain: Boolean;
  ArraySuffix: string;
begin
  AItems.Clear;

  RawFields := GetTableFieldsRaw(ATableName);

  for i := 0 to High(RawFields) do
  begin
    FieldName := RawFields[i].FieldName;

    if Quoted and IsObjectNameCaseSensitive(FieldName) then
      FieldName := '"' + FieldName + '"';

    // Computed Field — unverändert
    if Trim(RawFields[i].ComputedSource) <> '' then
    begin
      Line := FieldName + Delimiter + 'COMPUTED BY (' +
              Trim(RawFields[i].ComputedSource) + ')';
      AItems.Add(Line);
      Continue;
    end;

    // Basis-Typ ermitteln
    BaseType := GetFBTypeName(
      RawFields[i].FieldType,
      RawFields[i].FieldSubType,
      RawFields[i].FieldLength,
      RawFields[i].FieldPrecision,
      RawFields[i].FieldScale,
      RawFields[i].CharacterSetName,
      RawFields[i].CharacterLength
    );

    // Array-Suffix VOR dem Domain-Wrapping bauen,
    // damit auch Domain-basierte Arrays korrekt aussehen.
    ArraySuffix := ArrayDimsToSuffix(RawFields[i].ArrayDims);

    IsDomain := (RawFields[i].FieldSource <> '') and
                (not IsFieldDomainSystemGenerated(RawFields[i].FieldSource));

    if IsDomain then
      FieldType := RawFields[i].FieldSource + ' (' + BaseType + ')'
    else
      FieldType := BaseType;

    // Array-Suffix anhängen (funktioniert für Domain- und Base-Typ)
    if ArraySuffix <> '' then
      FieldType := FieldType + ArraySuffix;

    Line := FieldName + Delimiter + FieldType;

    if RawFields[i].NotNull then
      Line := Line + ' NOT NULL';

    DefSrc := Trim(RawFields[i].DefaultSource);
    if DefSrc <> '' then
    begin
      if UpperCase(Copy(DefSrc, 1, 7)) = 'DEFAULT' then
        Line := Line + ' ' + DefSrc
      else
        Line := Line + ' DEFAULT ' + DefSrc;
    end;

    AItems.Add(Line);
  end;
end;


procedure TSimpleObjExtractor.ExtractTableFieldsWithComma(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
var
  i: Integer;
  Line: string;
begin
  // Bestehende Methode nutzen (mit RemoveLastComma=True)
  ExtractTableFields(ATableName, AItems, Quoted, Delimiter, True);

  // Fehlende Kommas am Ende jeder Zeile hinzufügen
  for i := 0 to AItems.Count - 1 do
  begin
    Line := AItems[i];
    if (Length(Line) > 0) and (Line[Length(Line)] <> ',') then
      AItems[i] := Line + ',';
  end;

  // Letztes Komma wieder entfernen (letzte Zeile)
  if AItems.Count > 0 then
  begin
    i := AItems.Count - 1;
    Line := AItems[i];
    if (Length(Line) > 0) and (Line[Length(Line)] = ',') then
      AItems[i] := Copy(Line, 1, Length(Line) - 1);
  end;
end;

procedure TSimpleObjExtractor.ExtractTableFieldsForExternalTable(
  ATableName: string;
  var AItems: TStringList;
  Quoted: boolean;
  Delimiter: char);
var
  TempItems: TStringList;
  i: Integer;
  Line, FieldName, FieldType, DomainName, BaseType: string;
  SpacePos: Integer;
  RestAfterDomain: string;
begin
  // 1. Felder mit Komma extrahieren
  TempItems := TStringList.Create;
  try
    ExtractTableFieldsWithComma(ATableName, TempItems, false, Delimiter);

    AItems.Clear;

    // 2. Domänen auflösen
    for i := 0 to TempItems.Count - 1 do
    begin
      Line := TempItems[i];

      // Feldname und Typ trennen
      SpacePos := Pos(Delimiter, Line);
      if SpacePos > 0 then
      begin
        FieldName := Copy(Line, 1, SpacePos - 1);
        FieldType := Trim(Copy(Line, SpacePos + 1, MaxInt));
      end
      else
      begin
        FieldName := Line;
        FieldType := '';
      end;

      // Letztes Komma entfernen
      if (Length(FieldType) > 0) and (FieldType[Length(FieldType)] = ',') then
        FieldType := Copy(FieldType, 1, Length(FieldType) - 1);

      // Domänen-Namen extrahieren (erster Teil)
      DomainName := FieldType;
      if Pos(' ', DomainName) > 0 then
        DomainName := Copy(DomainName, 1, Pos(' ', DomainName) - 1);
      if Pos('(', DomainName) > 0 then
        DomainName := Copy(DomainName, 1, Pos('(', DomainName) - 1);

      // Rest nach Domänen-Namen (NOT NULL, DEFAULT etc.)
      RestAfterDomain := '';
      if Pos(' ', FieldType) > 0 then
        RestAfterDomain := Copy(FieldType, Pos(' ', FieldType) + 1, MaxInt);
      if Pos('(', FieldType) > 0 then
        RestAfterDomain := Copy(FieldType, Pos('(', FieldType), MaxInt);

      // Domäne auflösen mit der neuen Funktion!
      BaseType := DomainToDataType(DomainName, FIBDatabase, FIBTransaction);

      if BaseType <> '' then
      begin
        // Domäne gefunden → Typ ersetzen, Rest behalten
        FieldType := BaseType;

        // NOT NULL, DEFAULT etc. wieder anhängen
        if RestAfterDomain <> '' then
          FieldType := FieldType + ' ' + RestAfterDomain;
      end;

      // Komma wieder anhängen (außer letzte Zeile)
      if i < TempItems.Count - 1 then
        FieldType := FieldType + ',';

      AItems.Add(FieldName + Delimiter + FieldType);
    end;

  finally
    TempItems.Free;
  end;
end;

procedure TSimpleObjExtractor.ExtractCleanTableFields(
  ATableName: string;
  var AItems: TStringList;
  Quoted: boolean;
  Delimiter: char
);
var
  i, P: Integer;
  S: string;
begin
  ExtractTableFields(ATableName, AItems, Quoted, Delimiter, True);

  for i := 0 to AItems.Count - 1 do
  begin
    S := AItems[i];
    P := Pos(Delimiter, S);
    if P > 0 then
      S := Copy(S, 1, P - 1);  // alles ab Delimiter entfernen
    AItems[i] := Trim(S);
  end;
end;

procedure TSimpleObjExtractor.ExtractTableFieldsToTreeNode(
  ATableName: string;
  var Node: TTreeNode;
  Quoted: boolean;
  Delimiter: char; ImageIndex: integer; SysFlag: boolean);
var
  Items: TStringList;
  i: Integer;
  Line: string;
  TmpNode: TTreeNode;
begin
  if Node = nil then Exit;

  Node.DeleteChildren;

  Items := TStringList.Create;
  try
    ExtractTableFields(ATableName, Items, Quoted, Delimiter, false);

    for i := 0 to Items.Count - 1 do
    begin
      Line := Trim(Items[i]);
      if Line = '' then Continue;
      TmpNode := Node.TreeView.Items.AddChild(Node, Line);
      TPNodeInfos(TmpNode.Data)^.dbIndex := FDBIndex;

      if SysFlag then
        TPNodeInfos(TmpNode.Data)^.ObjectType := tvotSystemTableField
      else
        TPNodeInfos(TmpNode.Data)^.ObjectType := tvotTableField;

      TmpNode.ImageIndex := ImageIndex;
    end;

  finally
    Items.Free;
  end;
end;

procedure TSimpleObjExtractor.GetUDRFunction(Conn: TIBDatabase; AName: string; AItems: TStrings);
var
  str: string;
begin
  if not Assigned(AItems) then Exit;
  str := GetUDRFunctionDeclaration(Conn, AName, '');
  AItems.DelimitedText := str;
end;

procedure TSimpleObjExtractor.FixArraySyntax(AItems: TStrings);
var
  i: Integer;
  Line: string;
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'CHARACTER SET \w+\[(\d+):(\d+)\]';

    for i := 0 to AItems.Count - 1 do
    begin
      Line := AItems[i];
      if Regex.Exec(Line) then
      begin
        // Ersetze CHARACTER SET ...[m:n] durch [n] direkt nach Typ
        Line := Regex.Replace(Line, '[' + Regex.Match[2] + ']');
        AItems[i] := Line;
      end;
    end;
  finally
    Regex.Free;
  end;
end;

procedure TSimpleObjExtractor.FixDomainQuoting(AItems: TStrings);
var
  i: Integer;
  S, NewLine: string;
  R: TRegExpr;
begin
  R := TRegExpr.Create;
  try
    {
      Erklärungen zum Regex:

        ^(\s*"[^"]+"\s+)  = erstes quoted Feld ("FIELDNAME")
        "([^"]+)"        = der DOMAIN-Name, den wir ausquoten wollen
        (.*)$            = Rest der Zeile (NOT NULL, Komma, etc.)
    }
    R.Expression := '^(\s*"[^"]+"\s+)"([^"]+)"(.*)$';

    for i := 0 to AItems.Count - 1 do
    begin
      S := AItems[i];

      if R.Exec(S) then
      begin
        // Match[1] = linker Teil mit Fieldname
        // Match[2] = Domain (ohne Quotes)
        // Match[3] = Rest
        NewLine :=
          R.Match[1] +     // "FIELD"
          R.Match[2] +     // DOMAIN
          R.Match[3];      // Rest (NOT NULL, , usw.)

        AItems[i] := NewLine;
      end;
    end;

  finally
    R.Free;
  end;
end;



function TSimpleObjExtractor.TBTypeToIBXType(AObjectType: TObjectType): TExtractObjectTypes;
begin
  case AObjectType of
    // --- Tabellen / Views ---
    otTables:                Result := eoTable;
    otTableFields:           Result := eoTable;
    otViews:                 Result := eoView;

    // --- Triggers ---
    otTriggers,
    otTableTriggers,
    otDBTriggers,
    otDDLTriggers,
    otUDRTriggers:           Result := eoTrigger;

    // --- Procedures / Functions ---
    otProcedures:            Result := eoProcedure;
    otUDRProcedures:         Result := eoProcedure;
    otFunctions:             Result := eoFunction;
    otUDRFunctions:          Result := eoFunction;
    otUDF:                   Result := eoFunction;

    // --- Package-Objekte ---
    otPackages,
    otPackageFunctions,
    otPackageProcedures,
    otPackageUDFFunctions,
    otPackageUDRFunctions,
    otPackageUDRProcedures,
    otPackageUDRTriggers:    Result := eoPackage;

    // --- Generators / Sequences ---
    otGenerators,
    otSequences:             Result := eoGenerator;

    // --- Domains / Roles / Exceptions ---
    otDomains,
    otSystemDomains:         Result := eoDomain;
    otRoles,
    otSystemRoles:           Result := eoRole;
    otExceptions,
    otSystemExceptions:      Result := eoException;

    // --- Indexes / Constraints ---
    otIndexes:               Result := eoIndexes;
    otSystemIndexes:         Result := eoIndexes;
    otForeignKeys:           Result := eoForeign;
    otCheckConstraints:      Result := eoChecks;

    // Diese Typen werden MANUELL per SQL behandelt (kein IBX-Support)
    // Dummy-Wert, wird nie für FIBExtract.ExtractObject verwendet
    otPrimaryKeys:           Result := eoTable;       // Dummy
    otUniqueConstraints:     Result := eoTable;       // Dummy
    otNotNullConstraints:    Result := eoTable;       // Dummy
    otConstraints:           Result := eoTable;       // Dummy

    // --- System Tables ---
    otSystemTables:          Result := eoTable;

    // --- Data / BLOBs / Comments ---
    otData:                  Result := eoData;
    otBLOBFilters:           Result := eoBLOBFilter;
    otComments:              Result := eoComments;

    // --- Datenbank selbst ---
    otDatabase:              Result := eoDatabase;

  else
    raise Exception.Create(
      'Unknown ObjectType in function TBTypeToIBXType'
    );
  end;
end;

procedure TSimpleObjExtractor.ExtractObjectNames(dbIndex: integer; ObjectType: TObjectType; SystemFlag: boolean; var AItems: TStrings; OwnerObjName: string='');
var
  ServerVersionMajor: word;
  ItemStr: string;
  SQL: string;
begin
  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  SQL := '';

  case ObjectType of

    // ============================================================
    // TABLES / VIEWS
    // ============================================================
    otTables:
      SQL :=
        'SELECT ' + FBIdentifierCast('rdb$relation_name', ServerVersionMajor) + ' ' +
        'FROM rdb$relations ' +
        'WHERE rdb$view_blr IS NULL ' +
        '  AND (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
        'ORDER BY rdb$relation_name';

    otSystemTables:
      SQL :=
        'SELECT ' + FBIdentifierCast('rdb$relation_name', ServerVersionMajor) + ' ' +
        'FROM rdb$relations ' +
        'WHERE rdb$view_blr IS NULL ' +
        '  AND rdb$system_flag = 1 ' +
        'ORDER BY rdb$relation_name';

    otViews:
      SQL :=
        'SELECT DISTINCT ' + FBIdentifierCast('rdb$view_name', ServerVersionMajor) + ' ' +
        'FROM rdb$view_relations ' +
        'ORDER BY rdb$view_name';

    // ============================================================
    // GENERATORS / SEQUENCES
    // ============================================================
    otGenerators:
      SQL :=
        'SELECT ' + FBIdentifierCast('rdb$generator_name', ServerVersionMajor) + ' ' +
        'FROM rdb$generators ' +
        'WHERE rdb$system_flag = 0 ' +
        'ORDER BY rdb$generator_name';

    // ============================================================
    // PROCEDURES / FUNCTIONS / UDFs
    // ============================================================
    otProcedures:
      begin
        if ServerVersionMajor < 3 then
          SQL :=
            'SELECT ' + FBIdentifierCast('rdb$procedure_name', ServerVersionMajor) + ' ' +
            'FROM rdb$procedures ' +
            'ORDER BY rdb$procedure_name'
        else
          SQL :=
            'SELECT rdb$procedure_name ' +
            'FROM rdb$procedures ' +
            'WHERE rdb$package_name IS NULL ' +
            '  AND rdb$engine_name IS NULL ' +
            'ORDER BY rdb$procedure_name';
      end;

    otUDF:
      begin
        if ServerVersionMajor < 3 then
          SQL :=
            'SELECT ' + FBIdentifierCast('rdb$function_name', ServerVersionMajor) + ' ' +
            'FROM rdb$functions ' +
            'WHERE rdb$system_flag = 0 ' +
            'ORDER BY rdb$function_name'
        else
          SQL :=
            'SELECT rdb$function_name ' +
            'FROM rdb$functions ' +
            'WHERE rdb$system_flag = 0 ' +
            '  AND rdb$module_name IS NOT NULL ' +
            'ORDER BY rdb$function_name';
      end;

    otFunctions:
      SQL :=
        'SELECT rdb$function_name ' +
        'FROM rdb$functions ' +
        'WHERE rdb$module_name IS NULL ' +
        '  AND rdb$engine_name IS NULL ' +
        '  AND rdb$package_name IS NULL ' +
        'ORDER BY rdb$function_name';

    otUDRFunctions:
      SQL :=
        'SELECT rdb$function_name ' +
        'FROM rdb$functions ' +
        'WHERE rdb$engine_name IS NOT NULL ' +
        '  AND rdb$package_name IS NULL ' +
        'ORDER BY rdb$function_name';

    otUDRProcedures:
      SQL :=
        'SELECT rdb$procedure_name ' +
        'FROM rdb$procedures ' +
        'WHERE rdb$engine_name IS NOT NULL ' +
        '  AND rdb$package_name IS NULL ' +
        'ORDER BY rdb$procedure_name';

    // ============================================================
    // PACKAGES (FB 3.0+)
    // ============================================================
    otPackages:
      SQL :=
        'SELECT rdb$package_name ' +
        'FROM rdb$packages ' +
        'WHERE rdb$system_flag = 0 ' +
        'ORDER BY rdb$package_name';

    otPackageFunctions:
      if OwnerObjName = '' then
        SQL :=
          'SELECT rdb$function_name ' +
          'FROM rdb$functions ' +
          'WHERE rdb$module_name IS NULL ' +
          '  AND rdb$engine_name IS NULL ' +
          '  AND rdb$package_name IS NOT NULL ' +
          'ORDER BY rdb$function_name'
      else
        SQL :=
          'SELECT rdb$function_name ' +
          'FROM rdb$functions ' +
          'WHERE rdb$module_name IS NULL ' +
          '  AND rdb$engine_name IS NULL ' +
          '  AND rdb$package_name = ' + QuotedStr(OwnerObjName) + ' ' +
          'ORDER BY rdb$function_name';

    otPackageProcedures:
      if OwnerObjName = '' then
        SQL :=
          'SELECT rdb$procedure_name ' +
          'FROM rdb$procedures ' +
          'WHERE rdb$engine_name IS NULL ' +
          '  AND rdb$package_name IS NOT NULL ' +
          'ORDER BY rdb$procedure_name'
      else
        SQL :=
          'SELECT rdb$procedure_name ' +
          'FROM rdb$procedures ' +
          'WHERE rdb$engine_name IS NULL ' +
          '  AND rdb$package_name = ' + QuotedStr(OwnerObjName) + ' ' +
          'ORDER BY rdb$procedure_name';

    otPackageUDFFunctions:
      if OwnerObjName = '' then
        SQL :=
          'SELECT rdb$function_name ' +
          'FROM rdb$functions ' +
          'WHERE rdb$module_name IS NULL ' +
          '  AND rdb$engine_name IS NULL ' +
          '  AND rdb$package_name IS NOT NULL ' +
          'ORDER BY rdb$function_name'
      else
        SQL :=
          'SELECT rdb$function_name ' +
          'FROM rdb$functions ' +
          'WHERE rdb$module_name IS NULL ' +
          '  AND rdb$engine_name IS NULL ' +
          '  AND rdb$package_name = ' + QuotedStr(OwnerObjName) + ' ' +
          'ORDER BY rdb$function_name';

    otPackageUDRFunctions:
      if OwnerObjName = '' then
        SQL :=
          'SELECT rdb$function_name ' +
          'FROM rdb$functions ' +
          'WHERE rdb$module_name IS NULL ' +
          '  AND rdb$engine_name IS NOT NULL ' +
          '  AND rdb$package_name IS NOT NULL ' +
          'ORDER BY rdb$function_name'
      else
        SQL :=
          'SELECT rdb$function_name ' +
          'FROM rdb$functions ' +
          'WHERE rdb$module_name IS NULL ' +
          '  AND rdb$engine_name IS NOT NULL ' +
          '  AND rdb$package_name = ' + QuotedStr(OwnerObjName) + ' ' +
          'ORDER BY rdb$function_name';

    otPackageUDRProcedures:
      if OwnerObjName = '' then
        SQL :=
          'SELECT rdb$procedure_name ' +
          'FROM rdb$procedures ' +
          'WHERE rdb$engine_name IS NOT NULL ' +
          '  AND rdb$package_name IS NOT NULL ' +
          'ORDER BY rdb$procedure_name'
      else
        SQL :=
          'SELECT rdb$procedure_name ' +
          'FROM rdb$procedures ' +
          'WHERE rdb$engine_name IS NOT NULL ' +
          '  AND rdb$package_name = ' + QuotedStr(OwnerObjName) + ' ' +
          'ORDER BY rdb$procedure_name';

    // ============================================================
    // DOMAINS / EXCEPTIONS / ROLES / USERS
    // ============================================================
    otDomains:
      SQL :=
        'SELECT ' + FBIdentifierCast('rdb$field_name', ServerVersionMajor) + ' ' +
        'FROM rdb$fields ' +
        'WHERE (rdb$system_flag = 0 OR rdb$system_flag IS NULL) ' +
        '  AND rdb$field_name NOT LIKE ' + QuotedStr('RDB$%') + ' ' +
        'ORDER BY rdb$field_name';

    otExceptions:
      SQL :=
        'SELECT ' + FBIdentifierCast('rdb$exception_name', ServerVersionMajor) + ' ' +
        'FROM rdb$exceptions ' +
        'ORDER BY rdb$exception_name';

    otRoles:
      SQL :=
        'SELECT ' + FBIdentifierCast('rdb$role_name', ServerVersionMajor) + ' ' +
        'FROM rdb$roles ' +
        'WHERE rdb$role_name <> ' + QuotedStr('DUMMYROLE') + ' ' +
        'ORDER BY rdb$role_name';

    otUsers:
      begin
        if ServerVersionMajor < 3 then
          SQL :=
            'SELECT DISTINCT ' + FBIdentifierCast('rdb$user', ServerVersionMajor) + ' ' +
            'FROM rdb$user_privileges ' +
            'WHERE rdb$user_type = 8 ' +
            'ORDER BY rdb$user'
        else
          SQL :=
            'SELECT sec$user_name ' +
            'FROM sec$users ' +
            'ORDER BY sec$user_name';
      end;

    // ============================================================
    // CONSTRAINTS / INDEXES (OwnerObjName erforderlich)
    // ============================================================
    otPrimaryKeys:
      SQL :=
        'SELECT ' + FBIdentifierCast('rc.rdb$constraint_name', ServerVersionMajor) + ' ' +
        'FROM rdb$relation_constraints rc ' +
        'WHERE rc.rdb$relation_name = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        '  AND rc.rdb$constraint_type = ' + QuotedStr('PRIMARY KEY') + ' ' +
        'ORDER BY rc.rdb$constraint_name';

    otForeignKeys:
      SQL :=
        'SELECT ' + FBIdentifierCast('rc.rdb$constraint_name', ServerVersionMajor) + ' ' +
        'FROM rdb$relation_constraints rc ' +
        'WHERE rc.rdb$relation_name = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        '  AND rc.rdb$constraint_type = ' + QuotedStr('FOREIGN KEY') + ' ' +
        'ORDER BY rc.rdb$constraint_name';

    otUniqueConstraints:
      SQL :=
        'SELECT ' + FBIdentifierCast('rc.rdb$constraint_name', ServerVersionMajor) + ' ' +
        'FROM rdb$relation_constraints rc ' +
        'WHERE rc.rdb$relation_name = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        '  AND rc.rdb$constraint_type = ' + QuotedStr('UNIQUE') + ' ' +
        'ORDER BY rc.rdb$constraint_name';

    otCheckConstraints:
      SQL :=
        'SELECT ' + FBIdentifierCast('rc.rdb$constraint_name', ServerVersionMajor) + ' ' +
        'FROM rdb$relation_constraints rc ' +
        'WHERE rc.rdb$relation_name = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        '  AND rc.rdb$constraint_type = ' + QuotedStr('CHECK') + ' ' +
        'ORDER BY rc.rdb$constraint_name';

    otNotNullConstraints:
      // NOT NULL ist erst ab FB 3.0 als benannter Constraint in
      // RDB$RELATION_CONSTRAINTS sichtbar. Auf FB 1.5–2.5 liegt die Info
      // ausschließlich in RDB$RELATION_FIELDS.RDB$NULL_FLAG.
      // Deshalb direkt über die Felder gehen — läuft auf allen Versionen.
      SQL :=
        'SELECT ' + FBIdentifierCast('rf.RDB$FIELD_NAME', ServerVersionMajor) + ' ' +
        'FROM rdb$relation_fields rf ' +
        'WHERE rf.rdb$relation_name = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        '  AND rf.rdb$null_flag = 1 ' +
        'ORDER BY rf.rdb$field_position';

    otIndexes:
      SQL :=
        'SELECT ' + FBIdentifierCast('i.rdb$index_name', ServerVersionMajor) + ' ' +
        'FROM rdb$indices i ' +
        'LEFT JOIN rdb$relation_constraints rc ON i.rdb$index_name = rc.rdb$index_name ' +
        'WHERE i.rdb$relation_name = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        '  AND rc.rdb$index_name IS NULL ' +
        'ORDER BY i.rdb$index_name';

    // ============================================================
    // TRIGGERS — alle Varianten
    //
    // Versions-Matrix:
    //   FB 1.5 : nur Table-Triggers
    //   FB 2.0 : nur Table-Triggers
    //   FB 2.1+: Table + DB-Triggers
    //   FB 3.0+: Table + DB + DDL + UDR
    //
    // FB 1.5: FBIdentifierCast → VARCHAR(255) umgeht CHAR(31)-Bug
    // FB 3+ : rdb$engine_name existiert (UDR-Trigger)
    // Case: OwnerObjName via StripQuotes für RDB$-Lookup
    // ============================================================

    otTriggers:
      SQL :=
        'SELECT ' + FBIdentifierCast('rdb$trigger_name', ServerVersionMajor) + ' ' +
        'FROM rdb$triggers ' +
        'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
        'ORDER BY rdb$trigger_name';

    otTableTriggers:
      begin
        // CHECK-Constraint-Trigger immer ausschließen.
        // Leerer OwnerObjName = globale Liste (Format TABLE.TRIGGER)
        if Trim(OwnerObjName) = '' then
        begin
          // GLOBAL — alle Table-Trigger mit Tabellen-Präfix
          if ServerVersionMajor < 3 then
            SQL :=
              'SELECT ' +
              FBIdentifierCast('t.rdb$relation_name', ServerVersionMajor) + ', ' +
              FBIdentifierCast('t.rdb$trigger_name', ServerVersionMajor) + ' ' +
              'FROM rdb$triggers t ' +
              'WHERE (t.rdb$system_flag IS NULL OR t.rdb$system_flag = 0) ' +
              '  AND NOT EXISTS ( ' +
              '    SELECT 1 FROM rdb$check_constraints cc ' +
              '    WHERE cc.rdb$trigger_name = t.rdb$trigger_name ' +
              '  ) ' +
              '  AND t.rdb$relation_name IS NOT NULL ' +
              '  AND t.rdb$trigger_type < 8192 ' +
              'ORDER BY t.rdb$relation_name, t.rdb$trigger_name'
          else
            SQL :=
              'SELECT t.rdb$relation_name, t.rdb$trigger_name ' +
              'FROM rdb$triggers t ' +
              'WHERE (t.rdb$system_flag IS NULL OR t.rdb$system_flag = 0) ' +
              '  AND NOT EXISTS ( ' +
              '    SELECT 1 FROM rdb$check_constraints cc ' +
              '    WHERE cc.rdb$trigger_name = t.rdb$trigger_name ' +
              '  ) ' +
              '  AND t.rdb$relation_name IS NOT NULL ' +
              '  AND t.rdb$trigger_type < 8192 ' +
              '  AND (t.rdb$engine_name IS NULL OR TRIM(t.rdb$engine_name) = ' + QuotedStr('') + ') ' +
              'ORDER BY t.rdb$relation_name, t.rdb$trigger_name';
        end
        else
        begin
          // PRO TABELLE — bisheriges Verhalten
          if ServerVersionMajor < 3 then
            SQL :=
              'SELECT ' + FBIdentifierCast('t.rdb$trigger_name', ServerVersionMajor) + ' ' +
              'FROM rdb$triggers t ' +
              'WHERE (t.rdb$system_flag IS NULL OR t.rdb$system_flag = 0) ' +
              '  AND NOT EXISTS ( ' +
              '    SELECT 1 FROM rdb$check_constraints cc ' +
              '    WHERE cc.rdb$trigger_name = t.rdb$trigger_name ' +
              '  ) ' +
              '  AND t.rdb$relation_name IS NOT NULL ' +
              '  AND t.rdb$trigger_type < 8192 ' +
              '  AND t.rdb$relation_name = ' + QuotedStr(Trim(OwnerObjName)) + ' ' +
              'ORDER BY t.rdb$trigger_name'
          else
            SQL :=
              'SELECT t.rdb$trigger_name ' +
              'FROM rdb$triggers t ' +
              'WHERE (t.rdb$system_flag IS NULL OR t.rdb$system_flag = 0) ' +
              '  AND NOT EXISTS ( ' +
              '    SELECT 1 FROM rdb$check_constraints cc ' +
              '    WHERE cc.rdb$trigger_name = t.rdb$trigger_name ' +
              '  ) ' +
              '  AND t.rdb$relation_name IS NOT NULL ' +
              '  AND t.rdb$trigger_type < 8192 ' +
              '  AND t.rdb$relation_name = ' + QuotedStr(Trim(OwnerObjName)) + ' ' +
              '  AND (t.rdb$engine_name IS NULL OR TRIM(t.rdb$engine_name) = ' + QuotedStr('') + ') ' +
              'ORDER BY t.rdb$trigger_name';
        end;
      end;

    otDBTriggers:
      begin
        if ServerVersionMajor < 3 then
          SQL :=
            'SELECT ' + FBIdentifierCast('rdb$trigger_name', ServerVersionMajor) + ' ' +
            'FROM rdb$triggers ' +
            'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
            '  AND rdb$relation_name IS NULL ' +
            '  AND rdb$trigger_type BETWEEN 8192 AND 8196 ' +
            'ORDER BY rdb$trigger_name'
        else
          SQL :=
            'SELECT rdb$trigger_name ' +
            'FROM rdb$triggers ' +
            'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
            '  AND rdb$relation_name IS NULL ' +
            '  AND rdb$trigger_type BETWEEN 8192 AND 8196 ' +
            '  AND (rdb$engine_name IS NULL OR TRIM(rdb$engine_name) = ' + QuotedStr('') + ') ' +
            'ORDER BY rdb$trigger_name';
      end;

    otDDLTriggers:
      begin
        // FB 3+ kodiert DDL-Trigger als 2^63 - event_code (riesige Zahl),
        // daher die Bedingung "< 0 OR >= 16384".
        if ServerVersionMajor < 3 then
          SQL :=
            'SELECT ' + FBIdentifierCast('rdb$trigger_name', ServerVersionMajor) + ' ' +
            'FROM rdb$triggers ' +
            'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
            '  AND (rdb$trigger_type < 0 OR rdb$trigger_type >= 16384) ' +
            'ORDER BY rdb$trigger_name'
        else
          SQL :=
            'SELECT rdb$trigger_name ' +
            'FROM rdb$triggers ' +
            'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
            '  AND (rdb$trigger_type < 0 OR rdb$trigger_type >= 16384) ' +
            '  AND (rdb$engine_name IS NULL OR TRIM(rdb$engine_name) = ' + QuotedStr('') + ') ' +
            'ORDER BY rdb$trigger_name';
      end;

    otUDRTriggers:
      SQL :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
        '  AND rdb$engine_name = ' + QuotedStr('UDR') + ' ' +
        'ORDER BY rdb$trigger_name';

    otUDRTableTriggers:
      begin
        if Trim(OwnerObjName) = '' then
          // GLOBAL — alle UDR-Table-Trigger mit Tabellen-Präfix
          SQL :=
            'SELECT t.rdb$relation_name, t.rdb$trigger_name ' +
            'FROM rdb$triggers t ' +
            'WHERE (t.rdb$system_flag IS NULL OR t.rdb$system_flag = 0) ' +
            '  AND t.rdb$engine_name = ' + QuotedStr('UDR') + ' ' +
            '  AND t.rdb$relation_name IS NOT NULL ' +
            '  AND t.rdb$trigger_type < 8192 ' +
            'ORDER BY t.rdb$relation_name, t.rdb$trigger_name'
        else
          // PRO TABELLE
          SQL :=
            'SELECT t.rdb$trigger_name ' +
            'FROM rdb$triggers t ' +
            'WHERE (t.rdb$system_flag IS NULL OR t.rdb$system_flag = 0) ' +
            '  AND t.rdb$engine_name = ' + QuotedStr('UDR') + ' ' +
            '  AND t.rdb$relation_name IS NOT NULL ' +
            '  AND t.rdb$trigger_type < 8192 ' +
            '  AND t.rdb$relation_name = ' + QuotedStr(Trim(OwnerObjName)) + ' ' +
            'ORDER BY t.rdb$trigger_name';
      end;

    otUDRDBTriggers:
      SQL :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
        '  AND rdb$engine_name = ' + QuotedStr('UDR') + ' ' +
        '  AND rdb$relation_name IS NULL ' +
        '  AND rdb$trigger_type BETWEEN 8192 AND 8196 ' +
        'ORDER BY rdb$trigger_name';

    otUDRDDLTriggers:
      SQL :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE (rdb$system_flag IS NULL OR rdb$system_flag = 0) ' +
        '  AND rdb$engine_name = ' + QuotedStr('UDR') + ' ' +
        '  AND (rdb$trigger_type < 0 OR rdb$trigger_type >= 16384) ' +
        'ORDER BY rdb$trigger_name';

  else
    // Unbekannter/nichtt unterstützter Objekttyp → leere Liste
    Exit;
  end;

  if SQL = '' then
    Exit;

  // ============================================================
  // Query ausführen
  // ============================================================
  if FIBTransaction.InTransaction then
    FIBTransaction.Rollback;

  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  FIBSQL := TIBSQL.Create(FIBDatabase);
  try
    FIBSQL.Transaction := FIBTransaction;
    FIBSQL.SQL.Text := SQL;
    FIBSQL.ExecQuery;

    while not FIBSQL.EOF do
    begin
      // Zwei-Spalten-Modus: TABLE.TRIGGER (globale Trigger-Liste)
      // Ein-Spalten-Modus: nur Name
      if FIBSQL.FieldCount >= 2 then
        ItemStr := Trim(FIBSQL.Fields[0].AsString) + '.' +
                   Trim(FIBSQL.Fields[1].AsString)
      else
        ItemStr := Trim(FIBSQL.Fields[0].AsString);

      AItems.Add(ItemStr);
      FIBSQL.Next;
    end;

    if FIBTransaction.InTransaction then
      FIBTransaction.Rollback;
  finally
    if Assigned(FIBSQL) then
    begin
      FIBSQL.Close;
      FreeAndNil(FIBSQL);
    end;
  end;
end;

end.




