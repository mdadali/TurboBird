unit fsimpleobjextractor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  Dialogs, ComCtrls, RegExpr,
  IB, IBDatabase, IBSQL, IBExtract,

  fbcommon,

  udb_udr_func_fetcher;

{$I turbocommon.inc}


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

    procedure Extract(ObjectType: TObjectType; ObjectName : String;
                       ExtractTypes: TExtractTypes; Quoted: boolean; var AItems: TStrings);

    procedure   ExtractObjectNames(dbIndex: integer; ObjectType: TObjectType; SystemFlag: boolean;  var AItems: TStrings; OwnerObjName: string='');

    procedure   ExtractTableNames(AItems: TStrings; Quoted: boolean; SystemFlag: boolean);
    procedure   ExtractTableNamesToTreeNode(Quoted: boolean; Node: TTreeNode; SystemFlag: boolean);

    procedure   ExtractToTreeNode(ObjectType: TObjectType; ObjectName : String; ExtractTypes: TExtractTypes; Quoted: boolean; var Node: TTreeNode; AImageIndex: integer);
    procedure   ExtractTableFields(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char; RemoveLastComma: boolean);
    procedure   ExtractTableFieldsWithComma(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
    procedure   ExtractTableFieldsForExternalTable(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
    procedure   ExtractCleanTableFields(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
    procedure   ExtractTableFieldsToTreeNode(ATableName: string; var Node: TTreeNode; Quoted: boolean; Delimiter: char; ImageIndex: integer; SysFlag: boolean);

    property   Initialized: boolean read FInitialized;
  end;


implementation

uses  SysTables, turbocommon;

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

{constructor TSimpleObjExtractor.Create(DBIndex: integer);
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

    FIBDatabase.OnLogin := RegisteredDatabases[FDBIndex].IBDatabase.OnLogin;
    FIBDatabase.LoginPrompt := RegisteredDatabases[FDBIndex].IBDatabase.LoginPrompt;

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
end;}

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
begin
  tmpQuoted := FIBExtract.AlwaysQuoteIdentifiers;
  FIBExtract.AlwaysQuoteIdentifiers := Quoted;

  ResetExtract;

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

      // Primary Keys - gruppiert
      // Primary Keys - gruppiert
      otPrimaryKeys:
        begin
          SQL :=
            'SELECT ' +
            '  ''PRIMARY KEY ('' || LIST(TRIM(isg.RDB$FIELD_NAME), '', '') || '')'' AS CONSTRAINT_DDL ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rc.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
            'GROUP BY rc.RDB$CONSTRAINT_NAME';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;
            while not Qry.EOF do
            begin
              AItems.Add(Trim(Qry.Fields[0].AsString));
              Qry.Next;
            end;
          finally
            Qry.Free;
          end;
        end;

      // Foreign Keys - gruppiert
      otForeignKeys:
        begin
          SQL :=
            'SELECT ' +
            '  rc.RDB$CONSTRAINT_NAME || '': '' || ' +
            '  LIST(TRIM(isg.RDB$FIELD_NAME), '', '') || '' → '' || ' +
            '  TRIM(refc.RDB$CONST_NAME_UQ) AS FK_DDL ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'JOIN RDB$REF_CONSTRAINTS refc ON rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rc.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' ' +
            'GROUP BY rc.RDB$CONSTRAINT_NAME, refc.RDB$CONST_NAME_UQ';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;
            while not Qry.EOF do
            begin
              AItems.Add(Trim(Qry.Fields[0].AsString));
              Qry.Next;
            end;
          finally
            Qry.Free;
          end;
        end;

      // Unique Constraints - gruppiert
      otUniqueConstraints:
        begin
          SQL :=
            'SELECT ' +
            '  rc.RDB$CONSTRAINT_NAME || '': UNIQUE ('' || ' +
            '  LIST(TRIM(isg.RDB$FIELD_NAME), '', '') || '')'' AS UNIQUE_DDL ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rc.RDB$CONSTRAINT_TYPE = ''UNIQUE'' ' +
            'GROUP BY rc.RDB$CONSTRAINT_NAME';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;
            while not Qry.EOF do
            begin
              AItems.Add(Trim(Qry.Fields[0].AsString));
              Qry.Next;
            end;
          finally
            Qry.Free;
          end;
        end;

      // Check Constraints - einfach
      otCheckConstraints:
        begin
          SQL :=
            'SELECT ' +
            '  rc.RDB$CONSTRAINT_NAME || '': CHECK'' AS CHECK_DDL ' +
            'FROM RDB$RELATION_CONSTRAINTS rc ' +
            'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND rc.RDB$CONSTRAINT_TYPE = ''CHECK'' ' +
            'ORDER BY rc.RDB$CONSTRAINT_NAME';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;
            while not Qry.EOF do
            begin
              AItems.Add(Trim(Qry.Fields[0].AsString));
              Qry.Next;
            end;
          finally
            Qry.Free;
          end;
        end;

      // Not Null Constraints - einfach
      otNotNullConstraints:
        begin
          SQL :=
            'SELECT ' +
            '  TRIM(rf.RDB$FIELD_NAME) || '': NOT NULL'' AS NOTNULL_DDL ' +
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
              AItems.Add(Trim(Qry.Fields[0].AsString));
              Qry.Next;
            end;
          finally
            Qry.Free;
          end;
        end;

      // Indices - gruppiert
      otIndexes:
        begin
          SQL :=
            'SELECT ' +
            '  i.RDB$INDEX_NAME || '': '' || ' +
            '  CASE i.RDB$UNIQUE_FLAG WHEN 1 THEN ''UNIQUE '' ELSE '''' END || ' +
            '  ''ON ('' || LIST(TRIM(isg.RDB$FIELD_NAME), '', '') || '')'' AS INDEX_DDL ' +
            'FROM RDB$INDICES i ' +
            'JOIN RDB$INDEX_SEGMENTS isg ON i.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
            'WHERE i.RDB$RELATION_NAME = ' + QuotedStr(ObjectName) + ' ' +
            'AND NOT EXISTS (' +
            '  SELECT 1 FROM RDB$RELATION_CONSTRAINTS rc ' +
            '  WHERE rc.RDB$INDEX_NAME = i.RDB$INDEX_NAME ' +
            '  AND rc.RDB$CONSTRAINT_TYPE IS NOT NULL' +
            ') ' +
            'GROUP BY i.RDB$INDEX_NAME, i.RDB$UNIQUE_FLAG';

          Qry := TIBSQL.Create(FIBDatabase);
          try
            Qry.Transaction := FIBTransaction;
            Qry.SQL.Text := SQL;
            Qry.ExecQuery;
            while not Qry.EOF do
            begin
              AItems.Add(Trim(Qry.Fields[0].AsString));
              Qry.Next;
            end;
          finally
            Qry.Free;
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

procedure TSimpleObjExtractor.ExtractTableFields(
  ATableName: string;
  var AItems: TStringList;
   Quoted: boolean; Delimiter: char; RemoveLastComma: boolean);
var
  i, StartIdx, EndIdx: Integer;
  Line, FieldName, FieldType: string;
  InTable: Boolean;
  re: TRegExpr;
  tmpQuoted: boolean;
  S: string;
begin
  AItems.Clear;
  tmpQuoted := FIBExtract.AlwaysQuoteIdentifiers;
  FIBExtract.AlwaysQuoteIdentifiers := Quoted;

  ResetExtract;
  FIBExtract.ExtractObject(eoTable, ATableName, []);
  if FIBExtract.Items.Count = 0
    then Exit;
  FixArraySyntax(FIBExtract.Items);

  InTable := False;
  StartIdx := -1;
  EndIdx := -1;

  // Bereich der Spaltendefinition finden
  for i := 0 to FIBExtract.Items.Count - 1 do
  begin
    Line := Trim(FIBExtract.Items[i]);
    if not InTable then
    begin
      if Pos('CREATE TABLE', UpperCase(Line)) > 0 then
        InTable := True;
    end
    else
    begin
      if Line = '(' then
        StartIdx := i + 1
      else
      if Line = ');' then
      begin
        EndIdx := i - 1;
        Break;
      end;
    end;
  end;

  if (StartIdx < 0) or (EndIdx < 0) then Exit;

  re := TRegExpr.Create;
  try

    if Quoted then
      re.Expression := '"([^"]+)"\s+(.+)'   // quoted
    else
      re.Expression := '(\S+)\s+(.+)';       // unquoted

    for i := StartIdx to EndIdx do
    begin
      Line := Trim(FIBExtract.Items[i]);
      if Line = '' then Continue;

      // Constraints / computed ignorieren
      if (Pos('PRIMARY KEY', UpperCase(Line)) > 0) or
         (Pos('FOREIGN KEY', UpperCase(Line)) > 0) or
         (Pos('CHECK', UpperCase(Line)) > 0) or
         (Pos('COMPUTED BY', UpperCase(Line)) > 0) then
        Continue;

      if re.Exec(Line) then
      begin
        FieldName := re.Match[1];
        FieldType := re.Match[2];

        if Quoted then
        begin
          if turbocommon.IsObjectNameCaseSensitive(FieldName) then
            FieldName := '"' + FieldName + '"'
        end else
        begin
          FieldName := StringReplace(FieldName, '"', '', [rfReplaceAll]);
          FieldType := StringReplace(FieldType, '"', '', [rfReplaceAll]);
        end;

        //if RemoveLastComma then
          //FieldType := StringReplace(FieldType, ',', '', [rfReplaceAll]);

        if RemoveLastComma then
        begin
          // Nur das letzte Komma entfernen (falls vorhanden)
          if (Length(FieldType) > 0) and (FieldType[Length(FieldType)] = ',') then
            FieldType := Copy(FieldType, 1, Length(FieldType) - 1);
        end;

        AItems.Add(FieldName + Delimiter + FieldType);
      end;

    end;

  finally
    re.Free;
    FIBExtract.AlwaysQuoteIdentifiers := tmpQuoted;
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
    ExtractTableFields(ATableName, Items, Quoted, Delimiter, true);

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

procedure  TSimpleObjExtractor.ExtractObjectNames(dbIndex: integer; ObjectType: TObjectType; SystemFlag: boolean;  var AItems: TStrings; OwnerObjName: string='');
var ServerVersionMajor: word;
    isObjNameCaseSensitive: boolean;
    i, RecCount: integer;
    SQL: string;
    ItemStr: string;
begin
  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

try

  case ObjectType  of

    otTables:
      SQL := 'select rdb$relation_name from rdb$relations where rdb$view_blr is null ' +
      ' and (rdb$system_flag is null or rdb$system_flag = 0) order by rdb$relation_name';

    // Constraints - Primärschlüssel
    otPrimaryKeys:
      SQL :=
        'SELECT ' +
        '  rc.RDB$CONSTRAINT_NAME ' +
        'FROM RDB$RELATION_CONSTRAINTS rc ' +
        'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        'AND rc.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
        'ORDER BY rc.RDB$CONSTRAINT_NAME';

    // Constraints - Fremdschlüssel
    otForeignKeys:
      SQL :=
        'SELECT ' +
        '  rc.RDB$CONSTRAINT_NAME ' +
        'FROM RDB$RELATION_CONSTRAINTS rc ' +
        'JOIN RDB$REF_CONSTRAINTS ref ON rc.RDB$CONSTRAINT_NAME = ref.RDB$CONSTRAINT_NAME ' +
        'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        'AND rc.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' ' +
        'ORDER BY rc.RDB$CONSTRAINT_NAME';

    // Constraints - Unique
    otUniqueConstraints:
      SQL :=
        'SELECT ' +
        '  rc.RDB$CONSTRAINT_NAME ' +
        'FROM RDB$RELATION_CONSTRAINTS rc ' +
        'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        'AND rc.RDB$CONSTRAINT_TYPE = ''UNIQUE'' ' +
        'ORDER BY rc.RDB$CONSTRAINT_NAME';

    // Constraints - Check
    otCheckConstraints:
      SQL :=
        'SELECT ' +
        '  rc.RDB$CONSTRAINT_NAME ' +
        'FROM RDB$RELATION_CONSTRAINTS rc ' +
        'JOIN RDB$CHECK_CONSTRAINTS cc ON rc.RDB$CONSTRAINT_NAME = cc.RDB$CONSTRAINT_NAME ' +
        'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        'AND rc.RDB$CONSTRAINT_TYPE = ''CHECK'' ' +
        'ORDER BY rc.RDB$CONSTRAINT_NAME';

    // Constraints - Not Null
    otNotNullConstraints:
      SQL :=
        'SELECT ' +
        '  rc.RDB$CONSTRAINT_NAME ' +
        'FROM RDB$RELATION_CONSTRAINTS rc ' +
        'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        'AND rc.RDB$CONSTRAINT_TYPE = ''NOT NULL'' ' +
        'ORDER BY rc.RDB$CONSTRAINT_NAME';

    // Indices (ohne Constraints)
    otIndexes:
      SQL :=
        'SELECT ' +
        '  i.RDB$INDEX_NAME ' +
        'FROM RDB$INDICES i ' +
        'LEFT JOIN RDB$RELATION_CONSTRAINTS rc ON i.RDB$INDEX_NAME = rc.RDB$INDEX_NAME ' +
        'WHERE i.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(OwnerObjName)) + ' ' +
        'AND rc.RDB$INDEX_NAME IS NULL ' +  // kein Constraint-Index
        'ORDER BY i.RDB$INDEX_NAME';

    otSystemTables:
      SQL :=
        'Select Rdb$Relation_Name ' +
        'From Rdb$Relations ' +
        'Where Rdb$View_Blr Is Null ' +
        'And Rdb$System_Flag = 1 ' +
        'Order By Rdb$Relation_Name';

    otGenerators:
      SQL := 'select RDB$GENERATOR_Name from RDB$GENERATORS where RDB$SYSTEM_FLAG = 0 order by rdb$generator_Name';

    otTriggers:
      SQL:= 'SELECT rdb$Trigger_Name FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 order by rdb$Trigger_Name';


    otTableTriggers:
       SQL :=
      'SELECT rdb$trigger_name ' +
      'FROM rdb$triggers ' +
      'WHERE rdb$system_flag = 0 ' +
      'AND rdb$relation_name IS NOT NULL ' +
      'ORDER BY rdb$trigger_name';

    otDBTriggers:
      SQL := 'SELECT rdb$trigger_name FROM rdb$triggers WHERE rdb$system_flag = 0 AND rdb$relation_name IS NULL ' +
                       ' AND rdb$trigger_type < 16384 ORDER BY rdb$trigger_name';

    otDDLTriggers:
      SQL :=
        'SELECT rdb$trigger_name FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        'AND rdb$trigger_type >= 16384 ' +
        'ORDER BY rdb$trigger_name';

    otUDRTriggers:
      SQL :=
        'SELECT rdb$trigger_name ' +
         'FROM rdb$triggers ' +
         'WHERE rdb$system_flag = 0 ' +
         'AND rdb$engine_name = ''UDR'' ' +
         'ORDER BY rdb$trigger_name';

    otUDRTableTriggers:
      SQL :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        'AND rdb$engine_name = ''UDR'' ' +
        'AND rdb$relation_name IS NOT NULL ' +
        'ORDER BY rdb$trigger_name';

    otUDRDBTriggers:
      SQL :=
            'SELECT rdb$trigger_name ' +
            'FROM rdb$triggers ' +
            'WHERE rdb$system_flag = 0 ' +
            'AND rdb$engine_name = ''UDR'' ' +
            'AND rdb$relation_name IS NULL ' +
            'AND rdb$trigger_type < 16384 ' +
            'ORDER BY rdb$trigger_name';

    otUDRDDLTriggers:
      SQL :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        'AND rdb$engine_name = ''UDR'' ' +
        'AND rdb$trigger_type >= 16384 ' +
        'ORDER BY rdb$trigger_name';

    otViews:
      SQL := 'SELECT DISTINCT RDB$VIEW_NAME FROM RDB$VIEW_RELATIONS order by rdb$View_Name';

    otProcedures:
      if ServerVersionMajor < 3 then
        SQL := 'SELECT RDB$Procedure_Name FROM RDB$PROCEDURES order by rdb$Procedure_Name'
      else
        SQL :=
        'SELECT RDB$PROCEDURE_NAME ' +
        'FROM RDB$PROCEDURES ' +
        'WHERE ' +
        '  RDB$PACKAGE_NAME IS NULL ' +
        '  AND   RDB$ENGINE_NAME IS NULL ' +
        '  AND RDB$SYSTEM_FLAG IN (0, 2) ' +
        'ORDER BY RDB$PROCEDURE_NAME';

    otUDF: begin
      SQL :=
        'Select Rdb$Function_Name ' +
        'From Rdb$Functions ' +
        'Where Rdb$System_Flag = 0';

      if ServerVersionMajor >= 3 then
        SQL := FIBSQL.SQL.Text +
          ' And Rdb$Module_Name Is Not Null';
    end;

    otFunctions: // FB-Functions
      SQL :=
        'SELECT ' +
        '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
        '  RDB$DESCRIPTION, ' +
        '  RDB$SYSTEM_FLAG, ' +
        '  RDB$FUNCTION_SOURCE ' +
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$MODULE_NAME IS NULL ' +
        '  AND RDB$ENGINE_NAME IS  NULL ' +
        '  AND RDB$PACKAGE_NAME IS NULL ' +   // Nur freie Funktionen, keine Package-Funktionen
        '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$FUNCTION_NAME;';

    {otProcedures: // FB-Procedures
      FIBSQL.SQL.Text :=
      'SELECT ' +
      '  RDB$PROCEDURE_NAME AS PROCEDURE_NAME, ' +
      '  RDB$DESCRIPTION, ' +
      '  RDB$SYSTEM_FLAG, ' +
      '  RDB$PROCEDURE_SOURCE ' +
      'FROM RDB$PROCEDURES ' +
      'WHERE RDB$ENGINE_NAME IS NULL ' +
      '  AND RDB$PACKAGE_NAME IS  NULL ' +
      '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
      '  AND (RDB$PROCEDURE_TYPE IS NULL OR RDB$PROCEDURE_TYPE = 0) ' +
      'ORDER BY RDB$PROCEDURE_NAME;'; }

    otUDRFunctions: //External Engine  Global-Funcs
      SQL :=
        'SELECT ' +
        '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
        '  RDB$DESCRIPTION, ' +
        '  RDB$SYSTEM_FLAG, ' +
        '  RDB$FUNCTION_SOURCE, ' +
        '  RDB$ENGINE_NAME ' +
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$ENGINE_NAME IS NOT NULL ' +       // externe Engine (Python, Java etc)
        'AND RDB$PACKAGE_NAME  IS NULL '      +
        '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$FUNCTION_NAME;';

    otUDRProcedures: //External Engine  Global-Procs
      SQL :=
        'SELECT ' +
        '  RDB$PROCEDURE_NAME AS PROCEDURE_NAME, ' +
        '  RDB$DESCRIPTION, ' +
        '  RDB$SYSTEM_FLAG, ' +
        '  RDB$PROCEDURE_SOURCE, ' +
        '  RDB$ENGINE_NAME ' +
        'FROM RDB$PROCEDURES ' +
        'WHERE RDB$ENGINE_NAME IS NOT NULL ' + // externe Procs
        'AND RDB$PACKAGE_NAME  IS NULL ' +     // no packages-Proocs
        '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$PROCEDURE_NAME;';

    otDomains:
      //FIBSQL.SQL.Text:= 'select RDB$FIELD_NAME from RDB$FIELDS where RDB$Field_Name not like ''RDB$%''  order by rdb$Field_Name'
      SQL :=   //newlib
        'SELECT RDB$FIELD_NAME  FROM RDB$FIELDS ' +
        'WHERE (RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL) ' +
        'AND RDB$FIELD_NAME NOT LIKE ' + QuotedStr('RDB$%') + ' ' +
        'ORDER BY RDB$FIELD_NAME';

    otExceptions:
      SQL:= 'select RDB$EXCEPTION_NAME from RDB$EXCEPTIONS order by rdb$Exception_Name';

    otRoles:
    //FIBSQL.SQL.Text:= 'select RDB$ROLE_NAME from RDB$ROLES order by rdb$Role_Name'
      SQL :=
       'SELECT RDB$ROLE_NAME ' +
       'FROM RDB$ROLES ' +
       'WHERE RDB$ROLE_NAME <> ''DUMMYROLE'' ' +  // only for FireBird Version < 3.
       'ORDER BY RDB$ROLE_NAME';

    otUsers:
      // Benutzerliste je nach Firebird-Version
      if ServerVersionMajor < 3 then
        // Firebird 2.5: RDB$USER_PRIVILEGES
         SQL :=
          'SELECT DISTINCT RDB$USER ' +
          'FROM RDB$USER_PRIVILEGES ' +
          'WHERE RDB$USER_TYPE = 8 ' +
          'AND UPPER(RDB$USER) <> ' + QuotedStr(UpperCase(InitialServiceUser)) + ' ' +
          'ORDER BY RDB$USER'
      else
        // Firebird 3+: SEC$USERS
         SQL :=
          'SELECT SEC$USER_NAME AS RDB$USER ' +
           'FROM SEC$USERS ' +
           'WHERE UPPER(SEC$USER_NAME) <> ' + QuotedStr(UpperCase(InitialServiceUser)) + ' ' +
           'ORDER BY SEC$USER_NAME';

    otPackages:
       SQL := 'SELECT RDB$PACKAGE_NAME, RDB$OWNER_NAME, RDB$DESCRIPTION, RDB$SYSTEM_FLAG ' +
        'FROM RDB$PACKAGES WHERE RDB$SYSTEM_FLAG = 0 ' +
        'ORDER BY RDB$PACKAGE_NAME;';

    otPackageFunctions:
      if OwnerObjName = '' then
         SQL :=
          'SELECT ' +
          '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$FUNCTION_SOURCE ' +
          'FROM RDB$FUNCTIONS ' +
          'WHERE RDB$MODULE_NAME IS NULL ' +
          '  AND RDB$ENGINE_NAME IS  NULL ' +
          '  AND RDB$PACKAGE_NAME IS NOT NULL ' +
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$FUNCTION_NAME;'
        else
          SQL :=
            'SELECT ' +
            '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
            '  RDB$DESCRIPTION, ' +
            '  RDB$SYSTEM_FLAG, ' +
            '  RDB$FUNCTION_SOURCE ' +
            'FROM RDB$FUNCTIONS ' +
            'WHERE RDB$MODULE_NAME IS NULL ' +
            '  AND RDB$ENGINE_NAME IS  NULL ' +
            '  AND RDB$PACKAGE_NAME = ' + QuotedStr(OwnerObjName) +
            '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
            'ORDER BY RDB$FUNCTION_NAME;';

    otPackageProcedures:
      if OwnerObjName = '' then
         SQL :=
          'SELECT ' +
          '  RDB$PROCEDURE_NAME AS PROCEDURE_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$PROCEDURE_SOURCE ' +
          'FROM RDB$PROCEDURES ' +
          'WHERE RDB$ENGINE_NAME IS NULL ' +     // kein UDR
          '  AND RDB$PACKAGE_NAME IS NOT NULL ' +// gehört zu einem Package
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$PROCEDURE_NAME;'
      else
         SQL :=
          'SELECT ' +
          '  RDB$PROCEDURE_NAME AS PROCEDURE_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$PROCEDURE_SOURCE ' +
          'FROM RDB$PROCEDURES ' +
          'WHERE RDB$ENGINE_NAME IS NULL ' +     // kein UDR
          '  AND RDB$PACKAGE_NAME = ' + QuotedStr(OwnerObjName) +
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$PROCEDURE_NAME;';

    otPackageUDFFunctions:
      if OwnerObjName = '' then
         SQL :=
          'SELECT ' +
          '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$FUNCTION_SOURCE ' +
          'FROM RDB$FUNCTIONS ' +
          'WHERE RDB$MODULE_NAME IS NULL ' +
          '  AND RDB$ENGINE_NAME IS  NULL ' +
          '  AND RDB$PACKAGE_NAME IS NOT NULL ' +   // Nur freie Funktionen, keine Package-Funktionen
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$FUNCTION_NAME;'
        else
          if OwnerObjName = '' then
            SQL :=
            'SELECT ' +
            '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
            '  RDB$DESCRIPTION, ' +
            '  RDB$SYSTEM_FLAG, ' +
            '  RDB$FUNCTION_SOURCE ' +
            'FROM RDB$FUNCTIONS ' +
            'WHERE RDB$MODULE_NAME IS NULL ' +
            '  AND RDB$ENGINE_NAME IS  NULL ' +
            '  AND RDB$PACKAGE_NAME = ' + QuotedStr(OwnerObjName) +
            '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
            'ORDER BY RDB$FUNCTION_NAME;';

    otPackageUDRFunctions:
      if OwnerObjName = '' then
         SQL :=
          'SELECT ' +
          '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$FUNCTION_SOURCE ' +
          'FROM RDB$FUNCTIONS ' +
          'WHERE RDB$MODULE_NAME IS NULL ' +
          '  AND RDB$ENGINE_NAME IS NOT NULL ' +  // UDR-Funktionen
          '  AND RDB$PACKAGE_NAME IS NOT NULL ' + // Funktionen innerhalb eines Packages
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$FUNCTION_NAME;'
      else
         SQL :=
          'SELECT ' +
          '  RDB$FUNCTION_NAME AS FUNCTION_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$FUNCTION_SOURCE ' +
          'FROM RDB$FUNCTIONS ' +
          'WHERE RDB$MODULE_NAME IS NULL ' +
          '  AND RDB$ENGINE_NAME IS NOT NULL ' +  // UDR-Funktionen
          '  AND RDB$PACKAGE_NAME = ' + QuotedStr(OwnerObjName) +
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$FUNCTION_NAME;';

    otPackageUDRProcedures:
      if OwnerObjName = '' then
         SQL :=
          'SELECT ' +
          '  RDB$PROCEDURE_NAME AS PROCEDURE_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$PROCEDURE_SOURCE ' +
          'FROM RDB$PROCEDURES ' +
          'WHERE RDB$ENGINE_NAME IS NOT NULL ' +   // UDR-Prozeduren
          '  AND RDB$PACKAGE_NAME IS NOT NULL ' +  // innerhalb eines Packages
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$PROCEDURE_NAME;'
      else
         SQL :=
          'SELECT ' +
          '  RDB$PROCEDURE_NAME AS PROCEDURE_NAME, ' +
          '  RDB$DESCRIPTION, ' +
          '  RDB$SYSTEM_FLAG, ' +
          '  RDB$PROCEDURE_SOURCE ' +
          'FROM RDB$PROCEDURES ' +
          'WHERE RDB$ENGINE_NAME IS NOT NULL ' +   // UDR-Prozeduren
          '  AND RDB$PACKAGE_NAME = ' + QuotedStr(OwnerObjName) +
          '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$PROCEDURE_NAME;';
  end;

  try
    if FIBTransaction.InTransaction then
      FIBTransaction.Rollback;

    if not FIBTransaction.InTransaction then
      FIBTransaction.StartTransaction;

    FIBSQL := TIBSQL.Create(FIBDatabase);
    FIBSQL.Transaction := FIBTransaction;

    FIBSQL.SQL.Text := SQL;
    FIBSQL.ExecQuery;

    while not FIBSQL.EOF do
    begin
      ItemStr := Trim(FIBSQL.Fields[0].AsString);
      //isObjNameCaseSensitive := (ItemStr <> AnsiUpperCase(ItemStr));
      //if  isObjNameCaseSensitive then
        //ItemStr := MakeObjectNameQuoted(ItemStr);
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

finally

end;


end;

end.




