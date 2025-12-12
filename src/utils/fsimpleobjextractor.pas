unit fsimpleobjextractor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  Dialogs,
  IB, IBDatabase, IBQuery, IBExtract,
  turbocommon,
  fbcommon,

  udb_udr_func_fetcher;

type
  TSimpleObjExtractor = class
    FDBIndex: integer;
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBQuery: TIBQuery;
    FIBExtract: TIBExtract;
    function TBTypeToIBXType(AObjectType: TObjectType): TExtractObjectTypes;
    procedure GetUDRFunction(Conn: TIBDatabase; AName: string; AItems: TStrings);
  public
    constructor create(DBIndex: integer);
    destructor destroy; override;
    procedure Extract(ObjectType: TObjectType; ObjectName : String; ExtractTypes: TExtractTypes; var AItems: TStrings);
  end;

implementation

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

  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;

  FIBExtract := TIBExtract.Create(nil);
  FIBExtract.Database := FIBDatabase;
  FIBExtract.Transaction := FIBTransaction; // unbedingt zuweisen
  FIBExtract.AlwaysQuoteIdentifiers := true;
  FIBExtract.CaseSensitiveObjectNames := true;
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

procedure TSimpleObjExtractor.Extract(ObjectType: TObjectType; ObjectName: string; ExtractTypes: TExtractTypes; var AItems: TStrings);
begin
  if not Assigned(AItems) then Exit;
  AItems.Clear;

  case ObjectType of
    otUDRFunctions: begin
      GetUDRFunction(FIBDatabase, ObjectName, AItems);
    end;

    otUDRProcedures: begin
      // Platzhalter, evtl. später GetUDRProcedure implementieren
      AItems.Clear;
    end

    else begin
      FIBExtract.ExtractObject(TBTypeToIBXType(ObjectType), ObjectName, ExtractTypes);
      AItems.Assign(FIBExtract.Items);
    end;
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

    //else
      //Result := [];
  end;
end;

end.

