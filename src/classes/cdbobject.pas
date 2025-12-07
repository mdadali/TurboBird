unit cDBObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

type
  TDBObjectType = (
    otNone,
    otTables,
    otGenerators,
    otTriggers,
    otViews,
    otStoredProcedures,
    otUDF,
    otSystemTables,
    otDomains,
    otRoles,
    otExceptions,
    otUsers,
    otIndexes,
    otConstraints,
    otFBFunctions,
    otFBProcedures,
    otUDRFunctions,
    otUDRProcedures,
    otPackages,
    otPackageFunctions,
    otPackageProcedures,
    otPackageUDFFunctions,
    otPackageUDRFunctions,
    otPackageUDRProcedures
  );


  TDBObject = class
  protected
    FConnection: TSQLConnection;
    FName: string;
    FDBObjectType: TDBObjectType;
    FCurrentSQL: string;  // Original SQL aus DB
    FEditedSQL: string;   // Geänderter SQL-Text
    FIsModified: Boolean;

    function GetSQLString: string; virtual;
    procedure SetConnection(AConn: TSQLConnection); virtual;
  public
    constructor Create(AConn: TSQLConnection; const AName: string; ADBObjectType: TDBObjectType); virtual;

    function LoadFromDatabase: Boolean; virtual; abstract;
    function SaveToDatabase: Boolean; virtual; abstract;
    function DropFromDatabase: Boolean; virtual; abstract;

    procedure Edit(const AEditedSQL: string);
    procedure CommitChanges; virtual;
    procedure RollbackChanges; virtual;

    property Name: string read FName;
    property DBObjectType: TDBObjectType read FDBObjectType;
    property SQLStr: string read GetSQLString;
    property IsModified: Boolean read FIsModified;
    property Connection: TSQLConnection read FConnection write SetConnection;
  end;

implementation

{ TDBObject }
constructor TDBObject.Create(AConn: TSQLConnection; const AName: string; ADBObjectType: TDBObjectType);
begin
  inherited Create;
  FConnection := AConn;
  FName := AName;
  FDBObjectType := ADBObjectType;
  FCurrentSQL := '';
  FEditedSQL := '';
  FIsModified := False;
end;

function TDBObject.GetSQLString: string;
begin
  if FIsModified then
    Result := FEditedSQL
  else
    Result := FCurrentSQL;
end;

procedure TDBObject.SetConnection(AConn: TSQLConnection);
begin
  FConnection := AConn;
end;

procedure TDBObject.Edit(const AEditedSQL: string);
begin
  FEditedSQL := AEditedSQL;
  FIsModified := True;
end;

procedure TDBObject.CommitChanges;
begin
  if FIsModified then
  begin
    if SaveToDatabase then
    begin
      FCurrentSQL := FEditedSQL;
      FIsModified := False;
    end;
  end;
end;

procedure TDBObject.RollbackChanges;
begin
  FEditedSQL := FCurrentSQL;
  FIsModified := False;
end;

end.

