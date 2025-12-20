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

    procedure   ExtractObjectNames(dbIndex: integer; ObjectType: TObjectType; SystemFlag: boolean;  var AItems: TStrings; OwnerObjName: string='');

    procedure   ExtractTableNames(AItems: TStrings; Quoted: boolean; SystemFlag: boolean);
    procedure   ExtractTableNamesToTreeNode(Quoted: boolean; Node: TTreeNode; SystemFlag: boolean);

    procedure   Extract(ObjectType: TObjectType; ObjectName : String; ExtractTypes: TExtractTypes; Quoted: boolean; var AItems: TStrings);
    procedure   ExtractToTreeNode(ObjectType: TObjectType; ObjectName : String; ExtractTypes: TExtractTypes; Quoted: boolean; var Node: TTreeNode; AImageIndex: integer);
    procedure   ExtractTableFields(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char; RemoveLastComma: boolean);
    procedure   ExtractCleanTableFields(ATableName: string; var AItems: TStringList; Quoted: boolean; Delimiter: char);
    procedure   ExtractTableFieldsToTreeNode(ATableName: string; var Node: TTreeNode; Quoted: boolean; Delimiter: char; ImageIndex: integer);
  end;


implementation

uses turbocommon;

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

constructor TSimpleObjExtractor.create(DBIndex: integer);
var
  DBRec: TDatabaseRec;
begin
  FDBIndex := DBIndex;

  FIBDatabase := TIBDatabase.Create(nil);
  AssignIBDatabase(RegisteredDatabases[FDBIndex].IBDatabase, FIBDatabase);
  FIBDatabase.LoginPrompt := false;

  FIBTransaction := TIBTransaction.Create(FIBDatabase); // besser Ownership setzen
  FIBDatabase.DefaultTransaction := FIBTransaction;
  FIBTransaction.DefaultDatabase := FIBDatabase;

  if not FIBDatabase.Connected then
    FIBDatabase.Connected := true;

  FIBExtract := TIBExtract.Create(FIBDatabase);
  FIBExtract.Database := FIBDatabase;
  FIBExtract.Transaction := FIBTransaction; // unbedingt zuweisen

  FIBExtract.AlwaysQuoteIdentifiers := AlwaysQuoteIdentifiers;
  FIBExtract.CaseSensitiveObjectNames := CaseSensitiveObjectNames;
  FIBExtract.ShowSystem := ShowSystem;
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

var tmpQuoted: boolean;
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
          // TODO: gleiche Methode wie GetUDRFunction
        end;

    else  //case
      FIBExtract.ExtractObject(TBTypeToIBXType(ObjectType), ObjectName, ExtractTypes);

      if FIBExtract.Items.Count > 0 then
      begin
        FixArraySyntax(FIBExtract.Items);
        AItems.Assign(FIBExtract.Items);
      end;

    end; //case

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
          FieldName := '"' + FieldName + '"'
        else begin
          FieldName := StringReplace(FieldName, '"', '', [rfReplaceAll]);
          FieldType := StringReplace(FieldType, '"', '', [rfReplaceAll]);
        end;

        if RemoveLastComma then
          FieldType := StringReplace(FieldType, ',', '', [rfReplaceAll]);

        AItems.Add(FieldName + Delimiter + FieldType);
      end;

    end;

  finally
    re.Free;
    FIBExtract.AlwaysQuoteIdentifiers := tmpQuoted;
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
  Delimiter: char; ImageIndex: integer);
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
    otTableFields:           Result := eoTable;  // Felder sind Teil der Table in IBX
    otViews:                 Result := eoView;

    // --- Triggers ---
    otTriggers,
    otTableTriggers,
    otDBTriggers,
    otDDLTriggers,
    otUDRTriggers:           Result := eoTrigger;

    // --- Procedures / Functions ---
    otProcedures:            Result := eoProcedure;   // PSQL procedures
    otUDRProcedures:         Result := eoProcedure;   // External / UDR procedures
    otFunctions:             Result := eoFunction;    // Firebird interne Functions
    otUDRFunctions:          Result := eoFunction;    // UDRs über normale Function-DDL
    otUDF:                   Result := eoFunction;    // Firebird 3+: UDFs sind Functions

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
    otConstraints,
    otPrimaryKeys,
    //otUniqueConstraints,
    //otNotNullConstraints,
    //otSystemConstraints:     Result := eoForeign + eoChecks;  // IBX unterscheidet nicht alle Constraint-Arten

    // --- System Tables / Users ---
    otSystemTables:          Result := eoTable;
    otUsers,
    //otSystemUsers:           Result := []; // IBX unterstützt USERS nicht

    // --- Data / BLOBs / Comments ---
    otData:                  Result := eoData;
    otBLOBFilters:           Result := eoBLOBFilter;
    otComments:              Result := eoComments;

    // --- Datenbank selbst ---
    otDatabase:              Result := eoDatabase;

    else
      raise Exception.Create(
        'Unknown ObjectType in function TBTypeToIBXType (Unit TSimpleObjExtractor)'
      );
  end;
end;

procedure  TSimpleObjExtractor.ExtractObjectNames(dbIndex: integer; ObjectType: TObjectType; SystemFlag: boolean;  var AItems: TStrings; OwnerObjName: string='');
var ServerVersionMajor: word;
    isObjNameCaseSensitive: boolean;
    i, RecCount: integer;
begin
  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

try

  FIBSQL := TIBSQL.Create(FIBDatabase);
  FIBSQL.Transaction := FIBTransaction;

  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  case ObjectType  of

    otTables:
      FIBSQL.SQL.Text:= 'select rdb$relation_name from rdb$relations where rdb$view_blr is null ' +
      ' and (rdb$system_flag is null or rdb$system_flag = 0) order by rdb$relation_name';

    otSystemTables:
      FIBSQL.SQL.Text :=
        'Select Rdb$Relation_Name ' +
        'From Rdb$Relations ' +
        'Where Rdb$View_Blr Is Null ' +
        'And Rdb$System_Flag = 1 ' +
        'Order By Rdb$Relation_Name';

    otGenerators:
      FIBSQL.SQL.Text:= 'select RDB$GENERATOR_Name from RDB$GENERATORS where RDB$SYSTEM_FLAG = 0 order by rdb$generator_Name';

    otTriggers:
      FIBSQL.SQL.Text:= 'SELECT rdb$Trigger_Name FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 order by rdb$Trigger_Name';


    otTableTriggers:
      FIBSQL.SQL.Text :=
      'SELECT rdb$trigger_name ' +
      'FROM rdb$triggers ' +
      'WHERE rdb$system_flag = 0 ' +
      'AND rdb$relation_name IS NOT NULL ' +
      'ORDER BY rdb$trigger_name';

    otDBTriggers:
    FIBSQL.SQL.Text:= 'SELECT rdb$trigger_name FROM rdb$triggers WHERE rdb$system_flag = 0 AND rdb$relation_name IS NULL ' +
                       ' AND rdb$trigger_type < 16384 ORDER BY rdb$trigger_name';

    otDDLTriggers:
      FIBSQL.SQL.Text :=
        'SELECT rdb$trigger_name FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        'AND rdb$trigger_type >= 16384 ' +
        'ORDER BY rdb$trigger_name';

    otUDRTriggers:
      FIBSQL.SQL.Text :=
        'SELECT rdb$trigger_name ' +
         'FROM rdb$triggers ' +
         'WHERE rdb$system_flag = 0 ' +
         'AND rdb$engine_name = ''UDR'' ' +
         'ORDER BY rdb$trigger_name';

    otUDRTableTriggers:
      FIBSQL.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        'AND rdb$engine_name = ''UDR'' ' +
        'AND rdb$relation_name IS NOT NULL ' +
        'ORDER BY rdb$trigger_name';

    otUDRDBTriggers:
          FIBSQL.SQL.Text :=
            'SELECT rdb$trigger_name ' +
            'FROM rdb$triggers ' +
            'WHERE rdb$system_flag = 0 ' +
            'AND rdb$engine_name = ''UDR'' ' +
            'AND rdb$relation_name IS NULL ' +
            'AND rdb$trigger_type < 16384 ' +
            'ORDER BY rdb$trigger_name';

    otUDRDDLTriggers:
      FIBSQL.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        'AND rdb$engine_name = ''UDR'' ' +
        'AND rdb$trigger_type >= 16384 ' +
        'ORDER BY rdb$trigger_name';

    otViews:
      FIBSQL.SQL.Text:= 'SELECT DISTINCT RDB$VIEW_NAME FROM RDB$VIEW_RELATIONS order by rdb$View_Name';

    otProcedures:
      if ServerVersionMajor < 3 then
        FIBSQL.SQL.Text:= 'SELECT RDB$Procedure_Name FROM RDB$PROCEDURES order by rdb$Procedure_Name'
      else
        FIBSQL.SQL.Text :=
        'SELECT RDB$PROCEDURE_NAME ' +
        'FROM RDB$PROCEDURES ' +
        'WHERE ' +
        '  RDB$PACKAGE_NAME IS NULL ' +
        '  AND   RDB$ENGINE_NAME IS NULL ' +
        '  AND RDB$SYSTEM_FLAG IN (0, 2) ' +
        'ORDER BY RDB$PROCEDURE_NAME';

    otUDF: begin
      FIBSQL.SQL.Text :=
        'Select Rdb$Function_Name ' +
        'From Rdb$Functions ' +
        'Where Rdb$System_Flag = 0';

      if ServerVersionMajor >= 3 then
        FIBSQL.SQL.Text := FIBSQL.SQL.Text +
          ' And Rdb$Module_Name Is Not Null';
    end;

    otFunctions: // FB-Functions
    FIBSQL.SQL.Text :=
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
      FIBSQL.SQL.Text :=
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
      FIBSQL.SQL.Text :=
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
      FIBSQL.SQL.Text :=   //newlib
        'SELECT RDB$FIELD_NAME  FROM RDB$FIELDS ' +
        'WHERE (RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL) ' +
        'AND RDB$FIELD_NAME NOT LIKE ' + QuotedStr('RDB$%') + ' ' +
        'ORDER BY RDB$FIELD_NAME';

    otExceptions:
      FIBSQL.SQL.Text:= 'select RDB$EXCEPTION_NAME from RDB$EXCEPTIONS order by rdb$Exception_Name';

    otRoles:
    //FIBSQL.SQL.Text:= 'select RDB$ROLE_NAME from RDB$ROLES order by rdb$Role_Name'
      FIBSQL.SQL.Text :=
       'SELECT RDB$ROLE_NAME ' +
       'FROM RDB$ROLES ' +
       'WHERE RDB$ROLE_NAME <> ''DUMMYROLE'' ' +  // only for FireBird Version < 3.
       'ORDER BY RDB$ROLE_NAME';

    otUsers:
      // Benutzerliste je nach Firebird-Version
      if ServerVersionMajor < 3 then
        // Firebird 2.5: RDB$USER_PRIVILEGES
        FIBSQL.SQL.Text :=
          'SELECT DISTINCT RDB$USER ' +
          'FROM RDB$USER_PRIVILEGES ' +
          'WHERE RDB$USER_TYPE = 8 ' +
          'AND UPPER(RDB$USER) <> ' + QuotedStr(UpperCase(InitialServiceUser)) + ' ' +
          'ORDER BY RDB$USER'
      else
        // Firebird 3+: SEC$USERS
        FIBSQL.SQL.Text :=
          'SELECT SEC$USER_NAME AS RDB$USER ' +
           'FROM SEC$USERS ' +
           'WHERE UPPER(SEC$USER_NAME) <> ' + QuotedStr(UpperCase(InitialServiceUser)) + ' ' +
           'ORDER BY SEC$USER_NAME';

    otPackages:
      FIBSQL.SQL.Text:= 'SELECT RDB$PACKAGE_NAME, RDB$OWNER_NAME, RDB$DESCRIPTION, RDB$SYSTEM_FLAG ' +
        'FROM RDB$PACKAGES WHERE RDB$SYSTEM_FLAG = 0 ' +
        'ORDER BY RDB$PACKAGE_NAME;';

    otPackageFunctions:
      if OwnerObjName = '' then
        FIBSQL.SQL.Text :=
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
          FIBSQL.SQL.Text :=
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
        FIBSQL.SQL.Text :=
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
        FIBSQL.SQL.Text :=
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
        FIBSQL.SQL.Text :=
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
            FIBSQL.SQL.Text :=
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
        FIBSQL.SQL.Text :=
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
        FIBSQL.SQL.Text :=
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
        FIBSQL.SQL.Text :=
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
        FIBSQL.SQL.Text :=
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

  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  FIBSQL.ExecQuery;

  while not FIBSQL.EOF do
  begin

    AItems.Add(Trim(FIBSQL.Fields[0].AsString));
    isObjNameCaseSensitive := (AItems[i] <> UpperCase(AItems[i]));

    if  isObjNameCaseSensitive then
      AItems[i] := MakeObjectNameQuoted(AItems[i]);

    FIBSQL.Next;
  end;
  FIBSQL.Close;

finally
  //FIBSQL.Free;
  //if FIBTransaction.InTransaction then
    //FIBTransaction.Rollback;
end;

end;


end.

