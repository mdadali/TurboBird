unit uAIContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, IBDatabase, IBQuery, IB,
  turbocommon;

type
  { Interface für TurboBird-spezifischen Kontext, den der AI-Agent nutzen kann }
  IAIContextProvider = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    // ===== AKTUELLE VERBINDUNG / DATENBANK =====
    function GetCurrentDatabaseIndex: Integer;
    function GetCurrentDatabaseName: string;
    function GetCurrentServerName: string;
    function GetCurrentServerAlias: string;
    function GetIBDatabase: TIBDatabase;
    function GetIBTransaction: TIBTransaction;
    function IsConnected: Boolean;

    // Selektierte Tabelle im TreeView
    function GetSelectedTableName: string;
    function HasTableSelected: Boolean;

    // Aktuelles Query Window
    function GetActiveQueryWindow: TComponent;
    function GetCurrentQueryText: string;
    function GetActiveQueryCaption: string;

    // Schema-Informationen (aktuell verbundene DB)
    function GetTableSchema(ATableName: string; AWithIndexes: Boolean = True): string;
    function GetTableDDL(ATableName: string): string;
    function GetVisibleTableNames: TStringList;
    function GetLastError: string;

    // ===== TURBOBIRD SYSTEM: REGISTRIERTE SERVER =====
    function GetRegisteredServerCount: Integer;
    function GetServerInfo(AIndex: Integer): string; // JSON: ServerName, Alias, Version, Protocol, Port, etc.
    function GetServerInfoAsJSON(AIndex: Integer): string;
    function FindServerIndexByName(AName: string): Integer;
    function FindServerIndexByAlias(AAlias: string): Integer;

    // ===== TURBOBIRD SYSTEM: REGISTRIERTE DATENBANKEN =====
    function GetRegisteredDatabaseCount: Integer;
    function GetDatabaseInfo(AIndex: Integer): string; // JSON: Title, ServerName, DBPath, User, etc.
    function GetDatabaseInfoAsJSON(AIndex: Integer): string;
    function GetDatabasesForServer(AServerAlias: string): string; // JSON array
    function FindDatabaseIndexByTitle(ATitle: string): Integer;

    // ===== VERBINDUNGS-MANAGEMENT =====
    function ConnectToDatabase(ADBIndex: Integer): Boolean;
    function DisconnectDatabase(ADBIndex: Integer): Boolean;
    function IsDatabaseConnected(ADBIndex: Integer): Boolean;

    // ===== METADATEN (für beliebige registrierte DB, nicht nur aktuelle) =====
    function GetDatabaseMetadata(ADBIndex: Integer; AObjectType: string = 'tables'): string; // tables, views, procedures, functions, triggers, generators, domains, roles, users
    function GetDatabaseTableList(ADBIndex: Integer): string; // JSON array of table names
    function GetDatabaseViewList(ADBIndex: Integer): string;
    function GetDatabaseProcedureList(ADBIndex: Integer): string;
    function GetDatabaseFunctionList(ADBIndex: Integer): string;
    function GetDatabaseTriggerList(ADBIndex: Integer): string;
    function GetDatabaseGeneratorList(ADBIndex: Integer): string;
    function GetDatabaseDomainList(ADBIndex: Integer): string;
    function GetDatabaseRoleList(ADBIndex: Integer): string;
    function GetDatabaseUserList(ADBIndex: Integer): string;

    // ===== SERVER-STATUS & ADMIN =====
    function TestServerConnection(AServerIndex: Integer): string; // JSON: success, error, version
    function GetServerInventoryAsText: string; // menschlich lesbar
    function GetServerInventoryAsJSON: string; // strukturiert

    // High-Level Context String für Prompt-Injection
    function BuildContextString: string;
    function BuildFullSystemContextString: string; // mit allen Servern & DBs
  end;

  { Globale Zugriffsfunktion - wird von main.pas implementiert }
function GetAIContextProvider: IAIContextProvider;
procedure SetAIContextProvider(AProvider: IAIContextProvider);

implementation

var
  GAIContextProvider: IAIContextProvider = nil;

function GetAIContextProvider: IAIContextProvider;
begin
  Result := GAIContextProvider;
end;

procedure SetAIContextProvider(AProvider: IAIContextProvider);
begin
  GAIContextProvider := AProvider;
end;

end.