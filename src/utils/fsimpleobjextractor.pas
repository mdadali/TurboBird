unit fsimpleobjextractor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  Dialogs, ComCtrls, RegExpr,
  IB, IBDatabase, IBQuery, IBExtract,

  fbcommon,

  udb_udr_func_fetcher;

{$I turbocommon.inc}


type
  TSimpleObjExtractor = class
    FDBIndex: integer;
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBQuery: TIBQuery;
    FIBExtract: TIBExtract;
    procedure FixDomainQuoting(AItems: TStrings);
    procedure FixArraySyntax(AItems: TStrings);
    function TBTypeToIBXType(AObjectType: TObjectType): TExtractObjectTypes;
    procedure GetUDRFunction(Conn: TIBDatabase; AName: string; AItems: TStrings);
  public
    constructor Create(DBIndex: integer);
    procedure   ResetExtract;
    destructor  Destroy; override;
    procedure   ExtractTableNames(AItems: TStrings; Quoted: boolean);
    procedure   ExtractTableNamesToTreeNode(Quoted: boolean; Node: TTreeNode);

    procedure   Extract(ObjectType: TObjectType; ObjectName : String; ExtractTypes: TExtractTypes; Quoted: boolean; var AItems: TStrings);
    procedure   ExtractToTreeNode(ObjectType: TObjectType; ObjectName : String; ExtractTypes: TExtractTypes; Quoted: boolean; var Node: TTreeNode);
    procedure   ExtractTableFields(ATableName: string; var AItems: TStrings; Quoted: boolean; Delimiter: char);
    procedure   ExtractTableFieldsToTreeNode(ATableName: string; var Node: TTreeNode; Quoted: boolean; Delimiter: char);
  end;

implementation

uses turbocommon;

procedure TSimpleObjExtractor.ExtractTableNames(AItems: TStrings; Quoted: boolean);
var
  i: Integer;
  S: string;
begin
  AItems.Clear;

  FIBDatabase.GetTableNames(AItems);

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
  Node: TTreeNode);

var tmpNode, dummyNode: TTreeNode;
  Items: TStringList;
  i: Integer;
begin
  if Node = nil then Exit;

  Node.DeleteChildren;

  Items := TStringList.Create;

  try
    ExtractTableNames(Items, Quoted);

    for i := 0 to Items.Count - 1 do
    begin
      if Trim(Items[i]) = '' then Continue;
      tmpNode := Node.TreeView.Items.AddChild(Node, Items[i]);

      TPNodeInfos(tmpNode.Data)^.dbIndex := FDBIndex;
      TPNodeInfos(tmpNode.Data)^.ObjectType := tvotTable;
      tmpNode.ImageIndex := 4;

      dummyNode := Node.TreeView.Items.AddChild(TmpNode, 'Loading...');
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
  var Node: TTreeNode);
var
  Items: TStringList;
  i: Integer;
  Line: string;
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

      Node.TreeView.Items.AddChild(Node, Line);
    end;

  finally
    Items.Free;
  end;
end;

procedure TSimpleObjExtractor.ExtractTableFields(
  ATableName: string;
  var AItems: TStrings;
   Quoted: boolean; Delimiter: char);
var
  i, StartIdx, EndIdx: Integer;
  Line, FieldName, FieldType: string;
  InTable: Boolean;
  re: TRegExpr;
  tmpQuoted: boolean;
begin
  AItems.Clear;
  tmpQuoted := FIBExtract.AlwaysQuoteIdentifiers;
  FIBExtract.AlwaysQuoteIdentifiers := Quoted;

  ResetExtract;
  FIBExtract.ExtractObject(eoTable, ATableName, []);
  if FIBExtract.Items.Count = 0 then Exit;
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
        AItems.Add(FieldName + Delimiter + FieldType);
      end;

    end;
  finally
    re.Free;
    FIBExtract.AlwaysQuoteIdentifiers := tmpQuoted;
  end;
end;

procedure TSimpleObjExtractor.ExtractTableFieldsToTreeNode(
  ATableName: string;
  var Node: TTreeNode;
  Quoted: boolean;
  Delimiter: char);
var
  Items: TStringList;
  i: Integer;
  Line: string;
begin
  if Node = nil then Exit;

  // Alte Children entfernen
  Node.DeleteChildren;

  Items := TStringList.Create;
  try
    ExtractTableFields(ATableName, TStrings(Items), Quoted, Delimiter);

    for i := 0 to Items.Count - 1 do
    begin
      Line := Trim(Items[i]);
      if Line = '' then Continue;

      Node.TreeView.Items.AddChild(Node, Line);
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

    otTables:
      Result := eoTable;

    otViews:
      Result := eoView;

    otTriggers:
      Result := eoTrigger;

    otStoredProcedures:
      Result := eoProcedure;

    otFBProcedures:     // Firebird PSQL procedures
      Result := eoProcedure;

    otUDRProcedures:    // External / UDR procedures
      Result := eoProcedure;  // IBX macht hier keinen Unterschied

    otFBFunctions:
      Result := eoFunction;

    otUDRFunctions:
      Result := eoFunction;   // UDR ebenfalls über normale Function-DDL

    otPackageFunctions,
    otPackageProcedures,
    otPackageUDFFunctions,
    otPackageUDRFunctions,
    otPackageUDRProcedures,
    otPackageUDRTriggers:
      Result := eoPackage;     // IBX extrahiert PACKAGES komplett, nicht einzeln

    otPackages:
      Result := eoPackage;

    otGenerators:
      Result := eoGenerator;

    otDomains:
      Result := eoDomain;

    otRoles:
      Result := eoRole;

    otExceptions:
      Result := eoException;

    otIndexes:
      Result := eoIndexes;

    {otConstraints:
      Result := [eoForeign, eoChecks]; }


    otChecks:
      Result := eoChecks;

    otForeign:
      Result := eoForeign;

    otSystemTables:
      // Nur über ShowSystem = True extrahierbar, aber Objekt = TABLE
      Result := eoTable;

    otUsers: ;
      // IBX unterstützt USERS nicht → kein Mapping möglich
      //Result := [];

    otUDF:
      // Firebird 3+: UDFs sind eigentlich Functions
      Result := eoFunction;

    otUDRTriggers:
      // Externe Trigger werden wie normale Trigger extrahiert
      Result := eoTrigger;

    otData:
      Result := eoData;

    //else
      //Result := [];
  end;
end;

end.

