unit SysTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,  FileUtil,
  LResources, Forms, Controls, DB, BufDataset, fpstdexports, Dialogs, dbugintf,

  IB,
  IBDatabase,
  IBQuery,
  //IBDatabaseInfo, IBXServices,

  fbcommon,
  fsimpleobjextractor,
  EnterPass
  ;

type

  // e.g. used for composite foreign key constraints
  TConstraintCount = record
    Name: string; // name of constraint
    Count: integer; // count of occurrences
  end;
  TConstraintCounts = array of TConstraintCount;

  { TdmSysTables }

  TdmSysTables = class(TDataModule)
    BufDsServersAlias: TStringField;
    BufDsServersID: TAutoIncField;
    ibcDatabase: TIBDatabase;
    stTrans: TIBTransaction;
    sqQuery: TIBQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    // Fills array with composite (multikey) foreign key constraint names
    // and field count for specified table
    // Then use GetCompositeFKConstraint to check this array for a specified
    // constraint name
    procedure FillCompositeFKConstraints(const TableName: string;
      var ConstraintsArray: TConstraintCounts);
    // Initialize unit to work with specified database
    procedure Init(dbIndex: Integer);
    // Gets list of object names that have type specified by TVIndex
    // Returns count of objects in Count
    function GetDBObjectNames(DatabaseIndex: integer; ObjectType: TObjectType; var Count: Integer; OwnerObjName: string=''): string;

    // Recalculates index statistics for all fields in database
    // Useful when a large amount of inserts/deletes may have changed the statistics
    // and indexes are not as efficient as they could be.
    function RecalculateIndexStatistics(dbIndex: integer): boolean;

    // Returns object list (list of object names, i.e. tables, views) sorted by dependency
    // Limits sorting within one category (e.g. views)
    procedure SortDependencies(var ObjectList: TStringList);

    // Gets information on specified trigger
    function GetTriggerInfo(DatabaseIndex: Integer; ATriggername: string;
      var AfterBefore, OnTable, Event, Body: string; var TriggerEnabled: Boolean;
      var TriggerPosition: Integer; var IsDatabaseTrigger: Boolean;
      var IsDDLTrigger: Boolean;
      var IsUDRTrigger: Boolean;
      var ExternalName: string;
      var EngineName: string;
      var UDRParams: string): Boolean;
    // Scripts all check constraints for a database's tables as alter table
    // statement, adding the SQL to List
    function ScriptCheckConstraints(dbIndex: Integer; List: TStrings): boolean;
    // Script trigger creation for specified trigger
    function ScriptTrigger(dbIndex: Integer; ATriggerName: string; List: TStrings;
      AsCreate: Boolean = False): Boolean;
    // Used e.g. in scripting foreign keys
    function GetTableConstraints(ATableName: string; var SqlQuery: TIBQuery;
      ConstraintsList: TStringList = nil): Boolean;

    function GetAllConstraints(dbIndex: Integer; ConstraintsList, TablesList: TStringList): Boolean;

    // Returns non-0 if foreign key constraint has a composite index (multiple fields)
    // Returns 0 otherwise
    // Expects array that has been filled by FillCompositeFKConstraints
    function GetCompositeFKConstraint(const ConstraintName: string; ConstraintsArray: TConstraintCounts): integer;

    function GetConstraintInfo(dbIndex: integer; ATableName, ConstraintName: string; var KeyName,
        CurrentTableName, CurrentFieldName, OtherTableName, OtherFieldName, UpdateRule, DeleteRule: string): Boolean;

    // Gets information about exception.
    // Returns CREATE EXCEPTION statement in SQLQuery, or
    // CREATE OR ALTER EXCEPTION if CreateOrAlter is true
    function GetExceptionInfo(dbIndex: integer; ExceptionName: string; var Msg, Description, SqlQuery: string; CreateOrAlter: boolean): Boolean;
    // Gets information about domain
    procedure GetDomainInfo(dbIndex: integer; DomainName: string; var DomainType: string;
      var DomainSize: Integer; var DefaultValue: string; var CheckConstraint: string; var CharacterSet: string; var Collation: string);
    function GetConstraintForeignKeyFields(AIndexName: string; SqlQuery: TIBQuery): string;

    function GetDBUsers(dbIndex: Integer; ObjectName: string = ''): string;
    function GetDBObjectsForPermissions(dbIndex: Integer; AObjectType: Integer = -1): string;
    function GetObjectUsers(dbIndex: Integer; ObjectName: string): string;
    function GetUserObjects(dbIndex: Integer; UserName: string; AObjectType: Integer = -1): string;
    // Get permissions that specified user has for indicated object
    function GetObjectUserPermission(dbIndex: Integer; ObjectName, UserName: string; var ObjType: Integer): string;
    function GetAllUserPermissions(dbIndex: Integer; const UserName: string): TDataSet;

    // Add field types into List
    procedure GetBasicTypes(List: TStrings; dbIndex: integer);
    procedure GetExtendedTypes(List: TStrings; dbIndex: integer);
    procedure GetAllTypes(List: TStrings; dbIndex: integer);

    // Gets domain types; used in addition to basic types for GUI selections
    procedure GetDomainTypes(dbIndex: Integer; List: TStrings);
    function GetDefaultTypeSize(dbIndex: Integer; TypeName: string): Integer;
    function GetDomainTypeSize(dbIndex: Integer; DomainTypeName: string): Integer;

    // Gets details of field for given database/table/field
    function GetFieldInfo(dbIndex: Integer; TableName, FieldName: string;
      var FieldType: string;
      var FieldSize: integer; var FieldPrecision: integer; var FieldScale: integer;
      var NotNull: Boolean;
      var DefaultValue, CharacterSet, Collation, Description : string): Boolean;

    function GetDatabaseInfo(dbIndex: Integer; var ADatabaseName, CharSet, CreationDate, ServerTime: string;
      var ODSVerMajor, ODSVerMinor, Pages, PageSize: Integer;
      var ProcessList: TStringList; var ErrorMsg: string): Boolean;

    // Gets index info for a certain database+table
    function GetIndices(dbIndex: Integer; ATableName: string; PrimaryIndexName: string;
      var List: TStringList): Boolean;

    // Gets all index info for a certain database
    function GetAllIndices(dbIndex: Integer; List, TablesList: TStringList): Boolean;

    // Gets index names associated with a primary key
    function GetPrimaryKeyIndexName(dbIndex: Integer; ATableName: string; var ConstraintName: string): string;

    function GetIndexInfo(dbIndex: Integer; ATableName, AIndexName: string;
      var FieldsList: TStringList; var ConstraintName: string; var Unique, Ascending, IsPrimary: Boolean): Boolean;

    // Gets field names for table
    procedure GetTableFields(dbIndex: Integer; ATableName: string; FieldsList: TStringList);

    function EnsureDummyRole: Boolean;
    function DummyRoleExists: Boolean;

    procedure OnDatabaseLogin(Database: TIBDatabase; LoginParams: TStrings);
  end; 

var
  dmSysTables: TdmSysTables;

implementation

uses Main, turbocommon, topologicalsort;

function DecToBin(Dec, Len: Byte): string;
var
  Temp: string;
  i: byte;
begin
  Temp:= '';
  for i:= 1 to Len do
  begin
    Temp:= Char((Dec mod 2) + 48) + Temp;
    Dec:= Dec shr 1;
  end;
  Result:= Temp;
end;

{ TdmSysTables }

procedure TdmSysTables.Init(dbIndex: Integer);
var Conn: TIBDatabase;
    IBDb: TIBDatabase;
    FSQLTransaction: TIBTransaction;
begin
  if sqQuery.Active then
    sqQuery.Close;
  Conn := RegisteredDatabases[dbIndex].IBDatabase;
  IBDb := fmMain.CurrentIBConnection;

  // alte Verbindung lösen
  IBDb := Conn;
  FSQLTransaction := RegisteredDatabases[dbIndex].IBTransaction;

  if IBDb.DefaultTransaction.InTransaction then
    IBDb.DefaultTransaction.Rollback;

  if FSQLTransaction.InTransaction then
    FSQLTransaction.Rollback;

  IBDb.DefaultTransaction := FSQLTransaction;
  sqQuery.DataBase := IBDb;
  sqQuery.Transaction := FSQLTransaction;

  ibcDatabase:= IBDb;
  stTrans:= FSQLTransaction;

  // WICHTIG: OnLogin-Handler setzen bevor Connect!
  RegisteredDatabases[dbIndex].IBDatabase.OnLogin := @dmSysTables.OnDatabaseLogin;
  RegisteredDatabases[dbIndex].IBDatabase.LoginPrompt := True;

  if not RegisteredDatabases[dbIndex].IBDatabase.Connected then
    RegisteredDatabases[dbIndex].IBDatabase.Connected := true;

  if not RegisteredDatabases[dbIndex].IBTransaction.InTransaction then
    RegisteredDatabases[dbIndex].IBTransaction.StartTransaction;

  // Prüfen, ob DB überhaupt verbunden ist
  if not Conn.Connected then
  begin
    Conn.Params.Clear;
    Conn.DatabaseName := RegisteredDatabases[dbIndex].RegRec.DatabaseName;
    Conn.Params.Add('user_name=' + RegisteredDatabases[dbIndex].RegRec.UserName);
    Conn.Params.Add('password=' + RegisteredDatabases[dbIndex].RegRec.Password);
    Conn.Params.Add('sql_role_name=' + RegisteredDatabases[dbIndex].RegRec.Role);
    Conn.Params.Add('lc_ctype=' + RegisteredDatabases[dbIndex].RegRec.Charset);

    Conn.FirebirdLibraryPathName := RegisteredDatabases[dbIndex].RegRec.FireBirdClientLibPath;

    Conn.OnLogin := @dmSysTables.OnDatabaseLogin;
    Conn.LoginPrompt := True;

    Conn.Open;
  end;
end;

procedure TdmSysTables.OnDatabaseLogin(Database: TIBDatabase; LoginParams: TStrings);
var
  UserName, Password: string;
  DBIndex: Integer;
  Rec: TRegisteredDatabase;
  // Count: Integer;  // nicht mehr nötig
begin
  DBIndex := GetDBIndexByDatabase(Database);
  if DBIndex < 0 then
    Exit;

  Rec := RegisteredDatabases[DBIndex].RegRec;

  // 1. Wenn Passwort gespeichert → direkt verwenden
  if Rec.SavePassword and (Rec.Password <> '') then
  begin
    LoginParams.Values['user_name'] := Rec.UserName;
    LoginParams.Values['password'] := Rec.Password;
    Exit;
  end;

  // 2. Session-Cache prüfen
  Password := GetDBSessionPassword(Rec.ServerName, Rec.DatabaseName);
  if Password <> '' then
  begin
    LoginParams.Values['user_name'] := Rec.UserName;
    LoginParams.Values['password'] := Password;
    RegisteredDatabases[DBIndex].RegRec.Password := Password;
    Exit;
  end;

  // 3. Login-Dialog anzeigen (OHNE Rollen zu laden – DB ist ja noch nicht verbunden!)
  fmEnterPass.laDatabase.Caption := Rec.Title;
  fmEnterPass.edUser.Text := Rec.UserName;
  fmEnterPass.edPassword.Clear;
  fmEnterPass.edtRole.Text := '';
  fmEnterPass.cxSavePassword.Checked := False;

  if fmEnterPass.ShowModal = mrOK then
  begin
    UserName := fmEnterPass.edUser.Text;
    Password := fmEnterPass.edPassword.Text;

    LoginParams.Values['user_name'] := UserName;
    LoginParams.Values['password'] := Password;

    RegisteredDatabases[DBIndex].RegRec.UserName := UserName;
    RegisteredDatabases[DBIndex].RegRec.Password := Password;
    RegisteredDatabases[DBIndex].RegRec.Role := fmEnterPass.edtRole.Text;

    if fmEnterPass.cxSavePassword.Checked then
    begin
      RegisteredDatabases[DBIndex].RegRec.SavePassword := True;
      EditRegisteredDB(RegisteredDatabases[DBIndex].RegRec);
    end
    else
    begin
      RegisteredDatabases[DBIndex].RegRec.SavePassword := False;
      SetDBSessionPassword(Rec.ServerName, Rec.DatabaseName, Password);
    end;
  end
  else
    Abort;
end;

(*****  GetDBObjectNames, like Table names, Triggers, Generators, etc according to TVIndex  ****)
function TdmSysTables.GetDBObjectNames(DatabaseIndex: integer; ObjectType: TObjectType; var Count: Integer; OwnerObjName: string=''): string;
var ServerVersionMajor: word;
    isObjNameCaseSensitive: boolean;
begin
  Init(DatabaseIndex);

  //ServerVersionMajor := RegisteredDatabases[DatabaseIndex].RegRec.ServerVersionMajor;
  ServerVersionMajor := GetServerMajorVersionFromIBDB(RegisteredDatabases[DatabaseIndex].IBDatabase);

  sqQuery.Close;

  case ObjectType  of
    otTables:
      sqQuery.SQL.Text:= 'select rdb$relation_name from rdb$relations where rdb$view_blr is null ' +
      ' and (rdb$system_flag is null or rdb$system_flag = 0) order by rdb$relation_name';

    otGenerators:
      sqQuery.SQL.Text:= 'select RDB$GENERATOR_Name from RDB$GENERATORS where RDB$SYSTEM_FLAG = 0 order by rdb$generator_Name';

    {otTriggers:
      sqQuery.SQL.Text:= 'SELECT rdb$Trigger_Name FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 order by rdb$Trigger_Name';
    //überflüssig
    }

    otTableTriggers: begin
      if OwnerObjName = '' then
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        '  AND rdb$relation_name IS NOT NULL ' +
        '  AND COALESCE(rdb$engine_name, '''') = '''' ' +
        '  AND rdb$trigger_type < 8192 ' +
        'ORDER BY rdb$trigger_name'
      else
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        '  AND rdb$relation_name = ' + QuotedStr(OwnerObjName) +
        '  AND COALESCE(rdb$engine_name, '''') = '''' ' +
        '  AND rdb$trigger_type < 8192 ' +
        'ORDER BY rdb$trigger_name'
    end;

    otDBTriggers:
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        '  AND rdb$relation_name IS NULL ' +
        '  AND COALESCE(rdb$engine_name, '''') = '''' ' +
        '  AND rdb$trigger_type BETWEEN 8192 AND 8196 ' +
        'ORDER BY rdb$trigger_name';

    otDDLTriggers:
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        '  AND rdb$trigger_type >= 16384 ' +
        '  AND COALESCE(rdb$engine_name, '''') = '''' ' +
        'ORDER BY rdb$trigger_name';

    otUDRTriggers:
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
         'FROM rdb$triggers ' +
         'WHERE rdb$system_flag = 0 ' +
         'AND rdb$engine_name = ''UDR'' ' +
         'ORDER BY rdb$trigger_name';

    otUDRTableTriggers:
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        '  AND rdb$relation_name IS NOT NULL ' +
        '  AND TRIM(UPPER(rdb$engine_name)) LIKE ''%UDR%'' ' +
        'ORDER BY rdb$trigger_name';

    otUDRDBTriggers:
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        '  AND rdb$relation_name IS NULL ' +
        '  AND TRIM(UPPER(rdb$engine_name)) LIKE ''%UDR%'' ' +
        'ORDER BY rdb$trigger_name';

    otUDRDDLTriggers:
      sqQuery.SQL.Text :=
        'SELECT rdb$trigger_name ' +
        'FROM rdb$triggers ' +
        'WHERE rdb$system_flag = 0 ' +
        '  AND TRIM(UPPER(rdb$engine_name)) LIKE ''%UDR%'' ' +
        '  AND rdb$trigger_type >= 16384 ' +
        'ORDER BY rdb$trigger_name';

    otViews:
      sqQuery.SQL.Text:= 'SELECT DISTINCT RDB$VIEW_NAME FROM RDB$VIEW_RELATIONS order by rdb$View_Name';

    otProcedures:
      if ServerVersionMajor < 3 then
        sqQuery.SQL.Text:= 'SELECT RDB$Procedure_Name FROM RDB$PROCEDURES order by rdb$Procedure_Name'
      else
        sqQuery.SQL.Text :=
        'SELECT RDB$PROCEDURE_NAME ' +
        'FROM RDB$PROCEDURES ' +
        'WHERE ' +
        '  RDB$PACKAGE_NAME IS NULL ' +
        '  AND   RDB$ENGINE_NAME IS NULL ' +
        '  AND RDB$SYSTEM_FLAG IN (0, 2) ' +
        'ORDER BY RDB$PROCEDURE_NAME';

    otUDF: begin
      sqQuery.SQL.Text :=
        'Select Rdb$Function_Name ' +
        'From Rdb$Functions ' +
        'Where Rdb$System_Flag = 0';

      if ServerVersionMajor >= 3 then
        sqQuery.SQL.Text := sqQuery.SQL.Text +
          ' And Rdb$Module_Name Is Not Null';
    end;

    otFunctions: // FB-Functions
    sqQuery.SQL.Text :=
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
      sqQuery.SQL.Text :=
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
      sqQuery.SQL.Text :=
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
      sqQuery.SQL.Text :=
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

    otSystemTables:
      sqQuery.SQL.Text:=
        'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS where RDB$SYSTEM_FLAG=1 ' +
        'order by RDB$RELATION_NAME';

    otDomains:
      //sqQuery.SQL.Text:= 'select RDB$FIELD_NAME from RDB$FIELDS where RDB$Field_Name not like ''RDB$%''  order by rdb$Field_Name'
      sqQuery.SQL.Text :=   //newlib
        'SELECT RDB$FIELD_NAME  FROM RDB$FIELDS ' +
        'WHERE (RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL) ' +
        'AND RDB$FIELD_NAME NOT LIKE ' + QuotedStr('RDB$%') + ' ' +
        'ORDER BY RDB$FIELD_NAME';

    otExceptions:
      sqQuery.SQL.Text:= 'select RDB$EXCEPTION_NAME from RDB$EXCEPTIONS order by rdb$Exception_Name';

    otRoles:
    //sqQuery.SQL.Text:= 'select RDB$ROLE_NAME from RDB$ROLES order by rdb$Role_Name'
      sqQuery.SQL.Text :=
       'SELECT RDB$ROLE_NAME ' +
       'FROM RDB$ROLES ' +
       'WHERE RDB$ROLE_NAME <> ''DUMMYROLE'' ' +  // only for FireBird Version < 3.
       'ORDER BY RDB$ROLE_NAME';

    otUsers:
      // Benutzerliste je nach Firebird-Version
      if ServerVersionMajor < 3 then
        // Firebird 2.5: RDB$USER_PRIVILEGES
        sqQuery.SQL.Text :=
          'SELECT DISTINCT RDB$USER ' +
          'FROM RDB$USER_PRIVILEGES ' +
          'WHERE RDB$USER_TYPE = 8 ' +
          'ORDER BY RDB$USER'
      else
        // Firebird 3+: SEC$USERS
        sqQuery.SQL.Text :=
          'SELECT SEC$USER_NAME AS RDB$USER ' +
           'FROM SEC$USERS ' +
           'ORDER BY SEC$USER_NAME';

    otPackages:
      sqQuery.SQL.Text:= 'SELECT RDB$PACKAGE_NAME, RDB$OWNER_NAME, RDB$DESCRIPTION, RDB$SYSTEM_FLAG ' +
        'FROM RDB$PACKAGES WHERE RDB$SYSTEM_FLAG = 0 ' +
        'ORDER BY RDB$PACKAGE_NAME;';

    otPackageFunctions:
      if OwnerObjName = '' then
        sqQuery.SQL.Text :=
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
          sqQuery.SQL.Text :=
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
        sqQuery.SQL.Text :=
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
        sqQuery.SQL.Text :=
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
        sqQuery.SQL.Text :=
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
            sqQuery.SQL.Text :=
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
        sqQuery.SQL.Text :=
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
        sqQuery.SQL.Text :=
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
        sqQuery.SQL.Text :=
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
        sqQuery.SQL.Text :=
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

  Result := '';
  Count := 0;

  if not Assigned(sqQuery.Database) then
    sqQuery.Database := RegisteredDatabases[DatabaseIndex].IBDatabase;

  if sqQuery.Database.Connected then
    sqQuery.Database.Connected := false;

   sqQuery.Database.OnLogin := @dmSysTables.OnDatabaseLogin;
   sqQuery.Database.LoginPrompt := True;

  if not sqQuery.Database.Connected then
    sqQuery.Database.Connected := true;

  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;

  sqQuery.Open;
  while not sqQuery.EOF do
  begin
    inc(count);
    isObjNameCaseSensitive := (Trim(sqQuery.Fields[0].AsString) <> UpperCase(Trim(sqQuery.Fields[0].AsString)));
    //if isObjNameCaseSensitive then
      //Result := Result + '"""' + Trim(sqQuery.Fields[0].AsString) + '"""'
    //else
      Result:= Result + Trim(sqQuery.Fields[0].AsString);

    sqQuery.Next;
    if not sqQuery.EOF then
      Result:= Result + ',';
  end;
  //Count:= sqQuery.RecordCount;
  sqQuery.Close;
end;

function TdmSysTables.EnsureDummyRole: Boolean;
begin
  Result := True; // Standard: alles ok

  // 1. Prüfen, ob Dummy-Rolle existiert
  sqQuery.Close;
  sqQuery.SQL.Text := 'SELECT RDB$ROLE_NAME FROM RDB$ROLES WHERE RDB$ROLE_NAME = ''DUMMYROLE''';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;

  if sqQuery.EOF then
  begin
    // 2. Dummy-Rolle existiert nicht → erstellen
    sqQuery.Close;
    sqQuery.SQL.Text := 'CREATE ROLE DUMMYROLE';
    try
      if not sqQuery.Transaction.InTransaction then
        sqQuery.Transaction.StartTransaction;
      sqQuery.ExecSQL;
    except
      on E: Exception do
      begin
        Result := False;
        ShowMessage('Error creating the dummy role: ' + E.Message);
        Exit;
      end;
    end;
  end;
  sqQuery.Close;
end;

function TdmSysTables.DummyRoleExists: Boolean;
begin
  sqQuery.Close;
  sqQuery.SQL.Text := 'SELECT RDB$ROLE_NAME FROM RDB$ROLES WHERE RDB$ROLE_NAME = ''DUMMYROLE''';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  Result := not sqQuery.EOF;
  sqQuery.Close;
end;

{function TdmSysTables.RecalculateIndexStatistics(dbIndex: integer): boolean;
var
  i: integer;
  Indices, Tables: TStringList;
  TransActive: boolean;
begin
  result:= false;
  Init(dbIndex);
  sqQuery.Close;
  Indices:= TStringList.Create;
  Tables:= TStringList.Create;
  try
    if not(GetAllIndices(dbIndex, Indices, Tables)) then
    begin
      {$IFDEF DEBUG}
      SendDebug('RecalculateIndexStatistics: GetAllIndices call failed.');
      {$ENDIF}
      exit(false);
    end;

    // Loop through all indices and reset statistics
    TransActive:= stTrans.Active;
    if (TransActive) then
      stTrans.Commit;
    for i:= 0 to Indices.Count-1 do
    begin
      sqQuery.SQL.Text:= format('SET statistics INDEX %s',[Indices[i]]);
      if not stTrans.InTransaction then
        stTrans.StartTransaction;
      sqQuery.ExecSQL;
      { Commit after each index; no need to batch it all up in one
      big atomic transaction...}
      stTrans.Commit;
    end;
    if TransActive then
      if not stTrans.InTransaction then
        stTrans.StartTransaction; //leave transaction the way we found it
  finally
    Indices.Free;
    Tables.Free;
  end;
  result:= true;
end;}

function TdmSysTables.RecalculateIndexStatistics(dbIndex: integer): boolean;
var
  i: integer;
  Indices, Tables: TStringList;
  WasActive: boolean;
begin
  Result := False;

  Init(dbIndex);

  Indices := TStringList.Create;
  Tables := TStringList.Create;

  try
    if not GetAllIndices(dbIndex, Indices, Tables) then
      Exit(False);

    WasActive := stTrans.InTransaction;

    if WasActive then
      stTrans.Commit;

    for i := 0 to Indices.Count - 1 do
    begin
      try
        sqQuery.Close;

        sqQuery.SQL.Text :=
          'SET STATISTICS INDEX "' + Indices[i] + '"';

        stTrans.StartTransaction;

        sqQuery.ExecSQL;

        stTrans.Commit;

      except
        on E: Exception do
        begin
          if stTrans.InTransaction then
            stTrans.Rollback;

          {$IFDEF DEBUG}
          SendDebug('SET STATISTICS failed for index ' +
                    Indices[i] + ': ' + E.Message);
          {$ENDIF}
        end;
      end;
    end;

    if WasActive then
      stTrans.StartTransaction;

    Result := True;

  finally
    Indices.Free;
    Tables.Free;
  end;
end;

procedure TdmSysTables.SortDependencies(var ObjectList: TStringList);
const
  QueryTemplate=
    'select rdb$dependent_name, ' +
    ' rdb$depended_on_name, ' +
    ' rdb$dependent_type, '+
    ' rdb$depended_on_type '+
    ' from rdb$dependencies ';
var
  i: integer;
  TopSort: TTopologicalSort;
begin
  TopSort:= TTopologicalSort.Create;
  try
    sqQuery.SQL.Text:= QueryTemplate;
    if not sqQuery.Transaction.InTransaction then
      sqQuery.Transaction.StartTransaction;
    sqQuery.Open;
    // First add all objects that should end up as results without any depdencies
    for i:=0 to ObjectList.Count-1 do
    begin
      TopSort.AddNode(ObjectList[i]);
    end;
    // Now add dependencies:
    while not sqQuery.EOF do
    begin
      for i:=0 to ObjectList.Count-1 do
      begin
        if trim(sqQuery.FieldByName('rdb$dependent_name').AsString)=uppercase(ObjectList[i]) then
          begin
            // Get kind of object using dependency type in query;
            // limit to same category (i.e. all views or all stored procedures)
            if sqQuery. FieldByName('rdb$dependent_type').AsInteger=
              sqQuery. FieldByName('rdb$depended_on_type').AsInteger then
              TopSort.AddDependency(
                trim(sqQuery.FieldByName('rdb$dependent_name').AsString),
                trim(sqQuery.FieldByName('rdb$depended_on_name').AsString));
          end;
      end;
      sqQuery.Next;
    end;
    sqQuery.Close;
    // Resulting sorted list should end up in ObjectList:
    ObjectList.Clear;
    TopSort.Sort(ObjectList);
  finally
    TopSort.Free;
  end;
end;

procedure AddBodyToList(const Body: string; List: TStrings);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Body;
    List.AddStrings(SL);
  finally
    SL.Free;
  end;
end;

function TdmSysTables.GetTriggerInfo(
  DatabaseIndex: Integer;
  ATriggername: string;
  var AfterBefore, OnTable, Event, Body: string;
  var TriggerEnabled: Boolean;
  var TriggerPosition: Integer;
  var IsDatabaseTrigger: Boolean;
  var IsDDLTrigger: Boolean;
  var IsUDRTrigger: Boolean;
  var ExternalName: string;
  var EngineName: string;
  var UDRParams: string): Boolean;
var
  TrigType: Integer;
  AbsTrigType: Integer;
  FullEvent: string;
begin
  Result := False;

  try
    Init(DatabaseIndex);

    sqQuery.Close;
    sqQuery.SQL.Text :=
      'SELECT ' +
      '  RDB$TRIGGER_NAME, ' +
      '  RDB$RELATION_NAME, ' +
      '  RDB$TRIGGER_SOURCE, ' +
      '  RDB$TRIGGER_TYPE, ' +
      '  RDB$TRIGGER_SEQUENCE, ' +
      '  RDB$TRIGGER_INACTIVE, ' +
      '  RDB$ENGINE_NAME, ' +
      '  RDB$ENTRYPOINT ' +
      'FROM RDB$TRIGGERS ' +
      'WHERE UPPER(RDB$TRIGGER_NAME) = :TRIGGERNAME';

    sqQuery.ParamByName('TRIGGERNAME').AsString := UpperCase(ATriggerName);

    if not sqQuery.Transaction.InTransaction then
      sqQuery.Transaction.StartTransaction;

    sqQuery.Open;

    if sqQuery.IsEmpty then
      Exit;

    Body := Trim(sqQuery.FieldByName('RDB$TRIGGER_SOURCE').AsString);
    OnTable := Trim(sqQuery.FieldByName('RDB$RELATION_NAME').AsString);
    TriggerEnabled := sqQuery.FieldByName('RDB$TRIGGER_INACTIVE').AsInteger = 0;
    TriggerPosition := sqQuery.FieldByName('RDB$TRIGGER_SEQUENCE').AsInteger;
    TrigType := sqQuery.FieldByName('RDB$TRIGGER_TYPE').AsInteger;
    ExternalName := Trim(sqQuery.FieldByName('RDB$ENTRYPOINT').AsString);
    EngineName := Trim(sqQuery.FieldByName('RDB$ENGINE_NAME').AsString);
    UDRParams := Body;

    IsDatabaseTrigger := False;
    IsDDLTrigger := False;
    IsUDRTrigger := EngineName <> '';

    AbsTrigType := Abs(TrigType);
    AfterBefore := '';
    Event := '';

    // =========================================================
    // DDL TRIGGERS - haben NEGATIVE Typ-Werte!
    // =========================================================
    if TrigType < 0 then
    begin
      IsDDLTrigger := True;

      case AbsTrigType of
        8194: FullEvent := 'BEFORE ANY DDL STATEMENT';
        8195: FullEvent := 'AFTER ANY DDL STATEMENT';

        8196: FullEvent := 'BEFORE CREATE TABLE';
        8197: FullEvent := 'AFTER CREATE TABLE';

        8198: FullEvent := 'BEFORE ALTER TABLE';
        8199: FullEvent := 'AFTER ALTER TABLE';

        8200: FullEvent := 'BEFORE DROP TABLE';
        8201: FullEvent := 'AFTER DROP TABLE';

        8202: FullEvent := 'BEFORE CREATE PROCEDURE';
        8203: FullEvent := 'AFTER CREATE PROCEDURE';

        8204: FullEvent := 'BEFORE ALTER PROCEDURE';
        8205: FullEvent := 'AFTER ALTER PROCEDURE';

        8206: FullEvent := 'BEFORE DROP PROCEDURE';
        8207: FullEvent := 'AFTER DROP PROCEDURE';

        8208: FullEvent := 'BEFORE CREATE FUNCTION';
        8209: FullEvent := 'AFTER CREATE FUNCTION';

        8210: FullEvent := 'BEFORE ALTER FUNCTION';
        8211: FullEvent := 'AFTER ALTER FUNCTION';

        8212: FullEvent := 'BEFORE DROP FUNCTION';
        8213: FullEvent := 'AFTER DROP FUNCTION';

        // ... weitere bei Bedarf ergänzen ...
      else
        FullEvent := 'UNKNOWN DDL EVENT (' + IntToStr(AbsTrigType) + ')';
      end;

      Event := FullEvent;
    end

    // =========================================================
    // DATABASE TRIGGERS (8192-8196 positiv)
    // =========================================================
    else if (TrigType >= 8192) and (TrigType <= 8196) then
    begin
      IsDatabaseTrigger := True;

      case TrigType of
        8192: Event := 'ON CONNECT';
        8193: Event := 'ON DISCONNECT';
        8194: Event := 'ON TRANSACTION START';
        8195: Event := 'ON TRANSACTION COMMIT';
        8196: Event := 'ON TRANSACTION ROLLBACK';
      else
        Event := 'UNKNOWN DB EVENT (' + IntToStr(TrigType) + ')';
      end;
    end

    // =========================================================
    // TABLE TRIGGERS
    // =========================================================
    else
    begin
      case TrigType of
        1:  begin AfterBefore := 'BEFORE'; Event := 'INSERT'; end;
        2:  begin AfterBefore := 'AFTER';  Event := 'INSERT'; end;
        3:  begin AfterBefore := 'BEFORE'; Event := 'UPDATE'; end;
        4:  begin AfterBefore := 'AFTER';  Event := 'UPDATE'; end;
        5:  begin AfterBefore := 'BEFORE'; Event := 'DELETE'; end;
        6:  begin AfterBefore := 'AFTER';  Event := 'DELETE'; end;
        17: begin AfterBefore := 'BEFORE'; Event := 'INSERT OR UPDATE'; end;
        18: begin AfterBefore := 'AFTER';  Event := 'INSERT OR UPDATE'; end;
        25: begin AfterBefore := 'BEFORE'; Event := 'INSERT OR DELETE'; end;
        26: begin AfterBefore := 'AFTER';  Event := 'INSERT OR DELETE'; end;
        27: begin AfterBefore := 'BEFORE'; Event := 'UPDATE OR DELETE'; end;
        28: begin AfterBefore := 'AFTER';  Event := 'UPDATE OR DELETE'; end;
        113: begin AfterBefore := 'BEFORE'; Event := 'INSERT OR UPDATE OR DELETE'; end;
        114: begin AfterBefore := 'AFTER';  Event := 'INSERT OR UPDATE OR DELETE'; end;
      else
        begin
          AfterBefore := 'UNKNOWN';
          Event := 'UNKNOWN (' + IntToStr(TrigType) + ')';
        end;
      end;
    end;

    sqQuery.Close;
    Result := True;

  except
    on E: Exception do
    begin
      MessageDlg('Error while opening Trigger ' + ATriggerName + sLineBreak +
        sLineBreak + E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

function TdmSysTables.ScriptTrigger(
  dbIndex: Integer;
  ATriggerName: string;
  List: TStrings;
  AsCreate: Boolean): Boolean;
var
  Body: string;
  AfterBefore: string;
  Event: string;
  OnTable: string;
  TriggerEnabled: Boolean;
  TriggerPosition: Integer;
  IsDatabaseTrigger: Boolean;
  IsDDLTrigger: Boolean;
  IsUDRTrigger: Boolean;
  ExternalName: string;
  EngineName: string;
  UDRParams: string;
  i: Integer;
begin
  Result := GetTriggerInfo(
    dbIndex, ATriggerName,
    AfterBefore, OnTable, Event, Body,
    TriggerEnabled, TriggerPosition,
    IsDatabaseTrigger, IsDDLTrigger, IsUDRTrigger,
    ExternalName, EngineName, UDRParams);

  if not Result then
    Exit;

  List.Clear;
  List.Add('SET TERM ^;');
  List.Add('');

  // =========================================================
  // CREATE HEADER
  // =========================================================
  List.Add('CREATE OR ALTER TRIGGER ' + ATriggerName);

  // =========================================================
  // ACTIVE / INACTIVE
  // =========================================================
  if TriggerEnabled then
    List.Add('ACTIVE')
  else
    List.Add('INACTIVE');

  // =========================================================
  // EVENT
  // =========================================================
  if IsDDLTrigger then
    List.Add(Event)
  else if IsDatabaseTrigger then
    List.Add(Event)
  else
  begin
    List.Add(AfterBefore + ' ' + Event);
    if OnTable <> '' then
      List.Add('ON ' + OnTable);
  end;

  // =========================================================
  // POSITION - weglassen wegen Firebird 3.0 Bug
  // =========================================================
  // Nicht ausgeben - Firebird 3.0 akzeptiert POSITION nicht
  // bei CREATE OR ALTER TRIGGER

  // =========================================================
  // UDR TRIGGER
  // =========================================================
  if IsUDRTrigger then
  begin
    if ExternalName <> '' then
      List.Add('EXTERNAL NAME ''' + ExternalName + '''');
    if EngineName <> '' then
      List.Add('ENGINE ' + EngineName);
    if Trim(UDRParams) <> '' then
      List.Add('AS ' + QuotedStr(Trim(UDRParams)));
    List.Add('^');
    List.Add('');
    List.Add('SET TERM ;^');
    Exit(True);
  end;

  // =========================================================
  // NORMAL PSQL BODY
  // =========================================================
  if Body <> '' then
  begin
    List.Text := List.Text + Body;
  end
  else
  begin
    List.Add('AS');
    List.Add('BEGIN');
    List.Add('  EXIT;');
    List.Add('END');
  end;

  // Sicherstellen, dass END mit ^ abgeschlossen wird
  if List.Count > 0 then
  begin
    i := List.Count - 1;
    if Trim(List[i]) <> '' then
      List[i] := TrimRight(List[i]) + ' ^'
    else
      List.Add('^');
  end;

  List.Add('');
  List.Add('SET TERM ;^');
end;

function TdmSysTables.ScriptCheckConstraints(dbIndex: Integer; List: TStrings
  ): boolean;
const
  QueryTemplate='select '+
    'rc.rdb$relation_name, t.rdb$trigger_source '+
    'from rdb$check_constraints cc '+
    'inner join rdb$relation_constraints rc '+
    'on cc.rdb$constraint_name=rc.rdb$constraint_name '+
    'inner join rdb$triggers t '+
    'on cc.rdb$trigger_name=t.rdb$trigger_name '+
    'where rc.rdb$constraint_type=''CHECK'' and '+
    't.rdb$trigger_inactive<>1 and '+
    't.rdb$trigger_type=1' {type=1:before insert probably};
begin
  Result:= false;
  List.Clear;
  try
    Init(dbIndex);
    sqQuery.Close;
    sqQuery.SQL.Text:= QueryTemplate;
    if not sqQuery.Transaction.InTransaction then
      sqQuery.Transaction.StartTransaction;
    sqQuery.Open;
    while not sqQuery.EOF do
    begin
      List.Add(Format('ALTER TABLE %s ADD ',
        [trim(sqQuery.FieldByName('rdb$relation_name').AsString)]));
      // Field starts with CHECK
      List.Add(trim(sqQuery.FieldByName('rdb$trigger_source').AsString)+';');
      sqQuery.Next;
    end;
    sqQuery.Close;
    Result:= True;
  except
    on E: Exception do
    begin
      MessageDlg('Error: ' + e.Message, mtError, [mbOk], 0);
    end;
  end;
end;

(**********  Get Table Constraints Info  ********************)

function TdmSysTables.GetTableConstraints(ATableName: string; var SqlQuery: TIBQuery;
   ConstraintsList: TStringList = nil): Boolean;
const
  // Note that this query differs from the way constraints are
  // presented in GetConstraintsOfTable.
  // to do: find out what the differences are and indicate better in code/comments
  QueryTemplate='select '+
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
    'order by rc.rdb$constraint_name, flds_fk.rdb$field_position ';
begin
  SqlQuery.Close;
  SQLQuery.SQL.Text:= format(QueryTemplate, [UpperCase(ATableName)]);

  if SQLQuery.Transaction.InTransaction then
    SQLQuery.Transaction.Rollback;

  if SqlQuery.Database.Connected then
    SqlQuery.Database.Connected := false;

  SqlQuery.Database.OnLogin := @dmSysTables.OnDatabaseLogin;
  SqlQuery.Database.LoginPrompt := True;

  if not SqlQuery.Database.Connected then
    SqlQuery.Database.Connected := true;

  if not SQLQuery.Transaction.InTransaction then
    SQLQuery.Transaction.StartTransaction;

  SQLQuery.Open;

  Result:= SqlQuery.RecordCount > 0;

  with SqlQuery do
  if Result and Assigned(ConstraintsList) then
  begin
    ConstraintsList.Clear;
    while not Eof do
    begin
      ConstraintsList.Add(FieldByName('ConstName').AsString);
      Next;
    end;
    First;
  end;
end;

function TdmSysTables.GetAllConstraints(dbIndex: Integer; ConstraintsList, TablesList: TStringList): Boolean;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select Trim(Refc.RDB$Constraint_Name) as ConstName, ' +
    'Trim(Refc.RDB$CONST_NAME_UQ) as KeyName, ' +
    'Trim(Ind.RDB$Relation_Name) as CurrentTableName, ' +
    'Trim(Seg.RDB$Field_name) as CurrentFieldName, ' +
    'Trim(Con.RDB$Relation_Name) as OtherTableName, ' +
    'Trim(Ind.RDB$Foreign_key) as OtherFieldName, ' +
    'RDB$Update_Rule as UpdateRule, RDB$Delete_Rule as DeleteRule ' +
    'from RDB$RELATION_CONSTRAINTS Con, rdb$REF_Constraints Refc, RDB$INDEX_SEGMENTS Seg, ' +
    'RDB$INDICES Ind ' +
    'where Con.RDB$COnstraint_Name = Refc.RDB$Const_Name_UQ ' +
    '  and Refc.RDB$COnstraint_Name = Ind.RDB$Index_Name' +
    '  and Refc.RDB$COnstraint_Name = Seg.RDB$Index_Name';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  //Result:= sqQuery.RecordCount > 0;
  Result:= not sqQuery.DataSource.DataSet.IsEmpty;
  with sqQuery do
  if Result then
  begin
    ConstraintsList.Clear;
    if Assigned(TablesList) then
      TablesList.Clear;
    while not Eof do
    begin
      ConstraintsList.Add(FieldByName('ConstName').AsString);
      if Assigned(TablesList) then
        TablesList.Add(FieldByName('CurrentTableName').AsString);
      Next;
    end;
  end;
  sqQuery.Close;
end;

procedure TdmSysTables.DataModuleCreate(Sender: TObject);
begin

end;

procedure TdmSysTables.FillCompositeFKConstraints(const TableName: string;
  var ConstraintsArray: TConstraintCounts);
const
  // For specified table, returns foreign key constraints and count
  // if it has a composite key (i.e. multiple foreign key fields).
  // Based on query in GetTableConstraints
  CompositeCountSQL =
   'select trim(rc.rdb$constraint_name) as ConstName, '+
   'count(rfc.rdb$const_name_uq) as KeyCount '+
   'from rdb$relation_constraints AS rc '+
   'inner join rdb$ref_constraints as rfc on (rc.rdb$constraint_name = rfc.rdb$constraint_name) '+
   'inner join rdb$index_segments as flds_fk on (flds_fk.rdb$index_name = rc.rdb$index_name) '+
   'inner join rdb$relation_constraints as rc2 on (rc2.rdb$constraint_name = rfc.rdb$const_name_uq) '+
   'inner join rdb$index_segments as flds_pk on '+
   '((flds_pk.rdb$index_name = rc2.rdb$index_name) and (flds_fk.rdb$field_position = flds_pk.rdb$field_position)) '+
   'where rc.rdb$constraint_type = ''FOREIGN KEY'' and '+
   'upper(rc.rdb$relation_name) = ''%s'' '+
   'group by rc.rdb$constraint_name '+
   'having count(rfc.rdb$const_name_uq)>1 '+
   'order by rc.rdb$constraint_name ';
var
  CompositeQuery: TIBQuery;
  i:integer;
begin
  CompositeQuery:= TIBQuery.Create(nil);
  try
    CompositeQuery.DataBase:= ibcDatabase;
    CompositeQuery.Transaction:= stTrans;
    CompositeQuery.SQL.Text:= Format(CompositeCountSQL,[TableName]);
    if not CompositeQuery.Transaction.InTransaction then
      CompositeQuery.Transaction.StartTransaction;
    CompositeQuery.Open;
    CompositeQuery.Last; //needed for accurate recordcount
    //if CompositeQuery.RecordCount=0 then
    if CompositeQuery.IsEmpty then
    begin
      SetLength(ConstraintsArray,0);
    end
    else
    begin
      SetLength(ConstraintsArray,CompositeQuery.RecordCount);
      i:= 0;
      CompositeQuery.First;
      while not(CompositeQuery.EOF) do
      begin
        ConstraintsArray[i].Name:= CompositeQuery.FieldByName('ConstName').AsString;
        ConstraintsArray[i].Count:= CompositeQuery.FieldByName('KeyCount').AsInteger;
        CompositeQuery.Next;
        inc(i);
      end;
    end;
    CompositeQuery.Close;
  finally
    CompositeQuery.Free;
  end;
end;

function TdmSysTables.GetCompositeFKConstraint(const ConstraintName: string; ConstraintsArray: TConstraintCounts): integer;
var
  i: integer;
begin
  result:= 0;
  for i:= low(ConstraintsArray) to high(ConstraintsArray) do
  begin
    if ConstraintsArray[i].Name=ConstraintName then
    begin
      result:= ConstraintsArray[i].Count;
      break;
    end;
  end;
end;

(**********  Get Constraint Info  ********************)

function TdmSysTables.GetConstraintInfo(dbIndex: Integer; ATableName, ConstraintName: string; var KeyName,
    CurrentTableName, CurrentFieldName, OtherTableName, OtherFieldName, UpdateRule, DeleteRule: string): Boolean;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select Trim(Refc.RDB$Constraint_Name) as ConstName, Trim(Refc.RDB$CONST_NAME_UQ) as KeyName, ' +
    'Trim(Ind.RDB$Relation_Name) as CurrentTableName, ' +
    'Trim(Seg.RDB$Field_name) as CurrentFieldName, ' +
    'Trim(Con.RDB$Relation_Name) as OtherTableName, ' +
    'Trim(Ind.RDB$Foreign_key) as OtherFieldName, ' +
    'RDB$Update_Rule as UpdateRule, RDB$Delete_Rule as DeleteRule ' +
    'from RDB$RELATION_CONSTRAINTS Con, rdb$REF_Constraints Refc, RDB$INDEX_SEGMENTS Seg, ' +
    'RDB$INDICES Ind ' +
    'where Con.RDB$COnstraint_Name = Refc.RDB$Const_Name_UQ ' +
    '  and Refc.RDB$COnstraint_Name = Ind.RDB$Index_Name' +
    '  and Refc.RDB$COnstraint_Name = Seg.RDB$Index_Name' +
    '  and Ind.RDB$Relation_Name = ' + QuotedStr(UpperCase(ATableName)) + ' ' +
    '  and Refc.RDB$Constraint_Name = ' + QuotedStr(ConstraintName);
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  //Result:= sqQuery.RecordCount > 0;
  Result:= not sqQuery.IsEmpty;
  with sqQuery do
  if Result then
  begin
    KeyName:= FieldByName('KeyName').AsString;
    CurrentTableName:= FieldByName('CurrentTableName').AsString;
    CurrentFieldName:= FieldByName('CurrentFieldName').AsString;
    OtherTableName:= FieldByName('OtherTableName').AsString;
    OtherFieldName:= FieldByName('OtherFieldName').AsString;
    UpdateRule:= FieldByName('UpdateRule').AsString;
    DeleteRule:= FieldByName('DeleteRule').AsString;
  end;
  sqQuery.Close;
end;

(*********  Get Exception Info ***************)

function TdmSysTables.GetExceptionInfo(dbIndex: integer; ExceptionName: string; var Msg, Description,
  SqlQuery: string; CreateOrAlter: boolean): Boolean;
var
  CreatePart: string;
begin
  sqQuery.Close;
  init(dbIndex);
  sqQuery.SQL.Text:= 'select * from RDB$EXCEPTIONS ' +
   'where RDB$EXCEPTION_NAME = ' + QuotedStr(ExceptionName);
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  Result:= sqQuery.RecordCount > 0;
  if Result then
  begin
    if CreateOrAlter then
      CreatePart:= 'CREATE OR ALTER EXCEPTION ' {Since Firebird 2.0; create or replace existing}
    else
      CreatePart:= 'CREATE EXCEPTION ';
    Msg:= sqQuery.FieldByName('RDB$MESSAGE').AsString;
    Description:= sqQuery.FieldByName('RDB$DESCRIPTION').AsString;
    SqlQuery:= CreatePart + ExceptionName + LineEnding +
      QuotedStr(Msg) + ';';
    if Description<>'' then
      SQLQuery:= SQLQuery + LineEnding +
        'UPDATE RDB$EXCEPTIONS set ' + LineEnding +
        'RDB$DESCRIPTION = ''' + Description + ''' ' + LineEnding +
        'where RDB$EXCEPTION_NAME = ''' + ExceptionName + ''';';
  end;
  sqQuery.Close;
end;


(************  View Domain info  ***************)
procedure TdmSysTables.GetDomainInfo(dbIndex: Integer; DomainName: string; var DomainType: string;
  var DomainSize: Integer; var DefaultValue: string; var CheckConstraint: string; var CharacterSet: string; var Collation: string);
const
  // Select domain and associated collation (if text type domain)
  // note weird double join fields required...
  //
  QueryTemplate= 'select f.*, '+
    'coll.rdb$collation_name, '+
    'cs.rdb$character_set_name '+
    'from rdb$fields as f '+
    'left join '+
    { the entire inner join in the next part is treated as one entity that
    is left outer joined. Result: you get collation info with associated
    character set or just NULL }
    '( '+
    'rdb$collations as coll '+
    'inner join rdb$character_sets as cs on '+
    'coll.rdb$character_set_id=cs.rdb$character_set_id '+
    ') '+
    'on f.rdb$collation_id=coll.rdb$collation_id and '+
    'f.rdb$character_set_id=coll.rdb$character_set_id '+
    'where f.rdb$field_name=''%s'' ';
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= format(QueryTemplate, [UpperCase(DomainName)]);
  {$IFDEF NEVER}
  // Left for debugging
  SendDebug(sqQuery.SQL.Text);
  {$ENDIF}
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;

  if sqQuery.RecordCount > 0 then
  begin
    DomainType:= GetFBTypeName(sqQuery.FieldByName('RDB$FIELD_TYPE').AsInteger,
      sqQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
      sqQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger,
      sqQuery.FieldByName('RDB$FIELD_PRECISION').AsInteger,
      sqQuery.FieldByName('RDB$FIELD_SCALE').AsInteger);
    DomainSize:= sqQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger;
    DefaultValue:= trim(sqQuery.FieldByName('RDB$DEFAULT_SOURCE').AsString);
    CheckConstraint:= trim(sqQuery.FieldByName('RDB$VALIDATION_SOURCE').AsString); //e.g. CHECK (VALUE > 10000 AND VALUE <= 2000000)
    CharacterSet:= trim(sqQuery.FieldByName('rdb$character_set_name').AsString);
    Collation:= trim(sqQuery.FieldByName('rdb$collation_name').AsString);
  end
  else
    DomainSize:= 0;
  sqQuery.Close;
end;

(*************  Get constraint foreign key fields  *************)
function TdmSysTables.GetConstraintForeignKeyFields(AIndexName: string; SqlQuery: TIBQuery): string;
begin
  SqlQuery.Close;
  SqlQuery.SQL.Text:= 'select RDB$Index_Name as IndexName, RDB$Field_name as FieldName from RDB$INDEX_SEGMENTS ' +
    'where RDB$Index_name = ' + QuotedStr(UpperCase(Trim(AIndexName)));
  if not SqlQuery.Transaction.InTransaction then
    SqlQuery.Transaction.StartTransaction;
  SqlQuery.Open;
  while not SQLQuery.EOF do
  begin
    Result:= Result + Trim(SQLQuery.FieldByName('FieldName').AsString);
    SQLQuery.Next;
    if not SQLQuery.EOF then
      Result:= Result + ',';
  end;
  SQLQuery.Close;
end;


(************  Get Database Users  ************)

function TdmSysTables.GetDBUsers(dbIndex: Integer; ObjectName: string = ''): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$User, RDB$User_Type from RDB$USER_PRIVILEGES ';
  if ObjectName <> '' then // Specify specific Object
    sqQuery.SQL.Add('where RDB$Relation_Name = ''' + UpperCase(ObjectName) + ''' ');
  sqQuery.SQL.Add('order by RDB$User_Type');

  if sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.Rollback;

  if sqQuery.Database.Connected then
    sqQuery.Database.Connected := false;

  sqQuery.Database.OnLogin := @dmSysTables.OnDatabaseLogin;
  sqQuery.Database.LoginPrompt := True;

  if not sqQuery.Database.Connected then
    sqQuery.Database.Connected := true;

  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;

  sqQuery.Open;
  while not sqQuery.EOF do
  begin
    if sqQuery.Fields[1].AsInteger = 13 then // Role
      Result:= Result + '<R>';
    Result:= Result + Trim(sqQuery.Fields[0].Text);
    sqQuery.Next;
    if not sqQuery.EOF then
      Result:= Result + ',';
  end;
  sqQuery.Close;
end;


(************  Get Database Objects for permissions ************)

function TdmSysTables.GetDBObjectsForPermissions(dbIndex: Integer; AObjectType: Integer = -1): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$Relation_Name, RDB$Object_Type from RDB$USER_PRIVILEGES ';
  if AObjectType <> -1 then
    sqQuery.SQL.Add('where RDB$Object_Type = ' + IntToStr(AObjectType));
  sqQuery.SQL.Add(' order by RDB$Object_Type');
  //sqQuery.Database.DefaultTransaction.StartTransaction;
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  while not sqQuery.EOF do
  begin
    if Pos('$', sqQuery.Fields[0].AsString) = 0 then
    begin
      if AObjectType = -1 then
      case sqQuery.Fields[1].AsInteger of
        0: Result:= Result + '<T>'; // Table/View
        5: Result:= Result + '<P>'; // Procedure
        13: Result:= Result + '<R>'; // Role
      end;
      Result:= Result + Trim(sqQuery.Fields[0].Text);
      sqQuery.Next;
      if not sqQuery.EOF then
        Result:= Result + ',';

    end
    else
      sqQuery.Next;
  end;
  sqQuery.Close;
end;

(************  Get Object Users ************)

function TdmSysTables.GetObjectUsers(dbIndex: Integer; ObjectName: string): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$User, RDB$User_Type from RDB$USER_PRIVILEGES  ' +
    'where RDB$Relation_Name = ' + QuotedStr(ObjectName);
  //sqQuery.Database.DefaultTransaction.StartTransaction;
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  while not sqQuery.EOF do
  begin
      if sqQuery.Fields[1].AsInteger = 13 then // Role
        Result:= Result + '<R>';
      Result:= Result + Trim(sqQuery.Fields[0].Text);

      sqQuery.Next;
      if not sqQuery.EOF then
        Result:= Result + ',';
  end;
  sqQuery.Close;
end;

(************  Get Users Objects ************)

function TdmSysTables.GetUserObjects(dbIndex: Integer; UserName: string; AObjectType: Integer = -1): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select distinct RDB$Relation_Name, RDB$Grant_Option from RDB$USER_PRIVILEGES  ' +
    'where RDB$User = ''' + UserName + ''' ';
  if AObjectType <> -1 then
    sqQuery.SQL.Add(' and RDB$Object_Type = ' + IntToStr(AObjectType));
  sqQuery.SQL.Add(' order by RDB$Object_Type');
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  Result:= '';
  while not sqQuery.EOF do
  begin
    if sqQuery.FieldByName('RDB$Grant_Option').AsInteger <> 0 then
      Result:= Result + '<G>';
    Result:= Result + Trim(sqQuery.Fields[0].Text);

    sqQuery.Next;
    if not sqQuery.EOF then
      Result:= Result + ',';
  end;
  sqQuery.Close;
end;

(************  Get Object User permission ************)

function TdmSysTables.GetObjectUserPermission(dbIndex: Integer; ObjectName, UserName: string; var ObjType: Integer): string;
var  ServerVersionMajor: word;
begin
  Init(dbIndex);

  ServerVersionMajor := RegisteredDatabases[dbIndex].RegRec.ServerVersionMajor;

  // Firebird-Version bereits erkannt → direkt loslegen
  sqQuery.Close;

  if ServerVersionMajor < 3 then
  begin
    // Firebird 2.5 oder älter
    sqQuery.SQL.Text :=
      'SELECT RDB$OBJECT_TYPE, RDB$PRIVILEGE, RDB$GRANT_OPTION ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$RELATION_NAME = ' +  QuotedStr(ObjectName)  +  ' AND  RDB$USER = ' + QuotedStr(UserName);
  end else
  begin
    // Firebird 3.0 oder neuer – moderne Rechte mit USER_TYPE-Filter
    sqQuery.SQL.Text :=
      'SELECT RDB$OBJECT_TYPE, RDB$PRIVILEGE, RDB$GRANT_OPTION ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$RELATION_NAME = ' +  QuotedStr(ObjectName)  +  ' AND  RDB$USER = ' + QuotedStr(UserName) +
      '  AND RDB$USER_TYPE IN (8, 13)';  // 8 = USER, 13 = ROLE
  end;

  //sqQuery.Params.ParamByName('ObjectName').AsString := ObjectName;
  //sqQuery.Params.ParamByName('UserName').AsString := UserName;
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;

  Result := '';
  if sqQuery.RecordCount > 0 then
  begin
    ObjType := sqQuery.FieldByName('RDB$OBJECT_TYPE').AsInteger;
    while not sqQuery.EOF do
    begin
      Result := Result + Trim(sqQuery.FieldByName('RDB$PRIVILEGE').AsString);
      if sqQuery.FieldByName('RDB$GRANT_OPTION').AsInteger <> 0 then
        Result := Result + 'G';
      sqQuery.Next;
      if not sqQuery.EOF then
        Result := Result + ',';
    end;
  end
  else
    ObjType := -1;

  sqQuery.Close;
end;

{function TdmSysTables.GetAllUserPermissions(dbIndex: Integer; const UserName: string): TDataSet;
begin
  Init(dbIndex);

  if FBVersionMajor < 3 then
  begin
    {sqQuery.SQL.Text :=
      'SELECT RDB$RELATION_NAME, RDB$OBJECT_TYPE, RDB$PRIVILEGE, RDB$GRANT_OPTION ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$USER = :UserName';}
    sqQuery.SQL.Text :=
          'SELECT RDB$RELATION_NAME, RDB$OBJECT_TYPE, RDB$PRIVILEGE, RDB$GRANT_OPTION ' +
          'FROM RDB$USER_PRIVILEGES ' +
          'WHERE RDB$RELATION_NAME <> ' + QuotedStr('DUMMYROLE') + ' AND RDB$USER = ' + QuotedStr(UserName);
  end else
  begin
    sqQuery.SQL.Text :=
      'SELECT RDB$RELATION_NAME, RDB$OBJECT_TYPE, RDB$PRIVILEGE, RDB$GRANT_OPTION ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$USER = ' + QuotedStr(UserName) + ' AND RDB$USER_TYPE IN (8, 13)';
  end;

  //sqQuery.Params[0].AsString := UserName;
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  Result := sqQuery;
end;}

function TdmSysTables.GetAllUserPermissions(dbIndex: Integer; const UserName: string): TDataSet;
var  ServerVersionMajor: word;
begin
  Init(dbIndex);

  ServerVersionMajor := RegisteredDatabases[dbIndex].RegRec.ServerVersionMajor;

  if ServerVersionMajor < 3 then
  begin
    sqQuery.SQL.Text :=
      'SELECT RDB$RELATION_NAME, RDB$OBJECT_TYPE, RDB$PRIVILEGE, RDB$GRANT_OPTION ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$USER = ' + QuotedStr(UserName) + ' ' +
      'AND RDB$RELATION_NAME <> ' + QuotedStr('DUMMYROLE');
  end
  else
  begin
    sqQuery.SQL.Text :=
      'SELECT RDB$RELATION_NAME, RDB$OBJECT_TYPE, RDB$PRIVILEGE, RDB$GRANT_OPTION ' +
      'FROM RDB$USER_PRIVILEGES ' +
      'WHERE RDB$USER = ' + QuotedStr(UserName) + ' ' +
      'AND RDB$USER_TYPE IN (8, 13)';
  end;

  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;

  sqQuery.Open;
  Result := sqQuery;
end;

//newlib
{procedure TdmSysTables.GetBasicTypes(List: TStrings);
begin
  List.CommaText:= List.CommaText +
    'SMALLINT,INTEGER,BIGINT,VARCHAR,'+
    'FLOAT,"DOUBLE PRECISION",DECIMAL,NUMERIC,'+
    'CHAR,DATE,TIME,' +
    'TIMESTAMP,CSTRING,D_FLOAT,QUAD,BLOB';
end;
}
procedure CleanFirebirdTypeList(AList: TStrings);
var
  i: Integer;
  TypeName: string;
begin
  //AList.SaveToFile(ExtractFilePath(Application.ExeName) + 'FBDataTypes_Before');
  for i := AList.Count - 1 downto  0 do
  begin
    TypeName := Trim(UpperCase(AList[i]));

    // === Gültige Typen – ggf. normalisieren ===
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
       // gültiger Typ – keine Änderung

    // === Interne/veraltete Typen – behalten, aber ggf. markieren oder ignorieren ===
    else if (TypeName = 'CSTRING') or
            (TypeName = 'BLOB_ID') or
            (TypeName = 'QUAD') then
    begin
      // Belassen – könnten später als Legacy erkannt werden
      // Optional: AList[i] := TypeName + ' {legacy}';
      AList.Delete(i);
    end

    else if (TypeName = 'LONG') then
      AList[i] := 'INTEGER'
    else if (TypeName = 'SHORT') then
      AList[i] := 'SMALLINT'
   else if (TypeName = 'INT64') then
     AList[i] := 'BIGINT'

    else if (TypeName = '') or
            (TypeName.StartsWith('MON$')) or
            (TypeName.StartsWith('SEC$')) then
    begin
      // Leer oder Systemobjekte – typischerweise ignorieren
      // Optional: AList[i] := '<IGNORED>';
    end

    else
    begin
      // Unbekannt – ggf. loggen oder markieren
      // Optional: AList[i] := TypeName + ' {unknown}';
    end;
  end;
  //Types_After');
end;

procedure TdmSysTables.GetBasicTypes(List: TStrings; dbIndex: integer);
begin
  init(dbIndex);
  List.Clear;
  try
    if sqQuery.Active then
      sqQuery.Close;

    if sqQuery.Transaction.InTransaction then
      sqQuery.Transaction.Rollback;

    if sqQuery.Database.Connected then
      sqQuery.Database.Connected := false;

    sqQuery.Database.OnLogin := @dmSysTables.OnDatabaseLogin;
    sqQuery.Database.LoginPrompt := True;

    if not sqQuery.Database.Connected then
      sqQuery.Database.Connected := true;

    //sqQuery.SQL.Text := 'SELECT RDB$TYPE_NAME FROM RDB$TYPES WHERE RDB$FIELD_NAME = ' + quotedstr('RDB$FIELD_TYPE');
    sqQuery.SQL.Text :=
      'SELECT RDB$TYPE_NAME ' +
      'FROM RDB$TYPES ' +
      'WHERE RDB$FIELD_NAME = ' + QuotedStr('RDB$FIELD_TYPE') + ' ' +
      'AND RDB$TYPE_NAME NOT STARTING WITH ' + QuotedStr('MON$') + ' ' +
      'AND RDB$TYPE_NAME NOT STARTING WITH ' + QuotedStr('SEC$') + ' ' +
      'ORDER BY RDB$TYPE_NAME';

    sqQuery.Open;
    while not sqQuery.Eof do
    begin
      List.Add(Trim(sqQuery.FieldByName('RDB$TYPE_NAME').AsString));
      sqQuery.Next;
    end;
  finally
    //ibcDatabase.Connected := false;
    //List.SaveToFile('types.txt');
    //CleanFirebirdTypeList(List);
  end;
end;

procedure TdmSysTables.GetExtendedTypes(List: TStrings; dbIndex: integer);
var
  HasDecimal, HasNumeric, HasChar, HasUUID: Boolean;
begin
  init(dbIndex);

  List.Clear;
  try
    if sqQuery.Active then
      sqQuery.Close;
    if stTrans.InTransaction then
      stTrans.Rollback;

    // DECIMAL / NUMERIC
    sqQuery.SQL.Text :=
      'SELECT DISTINCT RDB$FIELD_PRECISION ' +
      'FROM RDB$FIELDS ' +
      'WHERE RDB$FIELD_PRECISION IS NOT NULL AND RDB$FIELD_PRECISION > 0';
    if not sqQuery.Transaction.InTransaction then
      sqQuery.Transaction.StartTransaction;
    sqQuery.Open;
    HasDecimal := (sqQuery.RecordCount > 0);
    HasNumeric := HasDecimal;
    sqQuery.Close;

    // CHAR
    sqQuery.SQL.Text :=
      'SELECT 1 FROM RDB$FIELDS WHERE RDB$FIELD_TYPE = 14 ROWS 1';
    if not sqQuery.Transaction.InTransaction then
      sqQuery.Transaction.StartTransaction;
    sqQuery.Open;
    HasChar := (sqQuery.RecordCount > 0);
    sqQuery.Close;

    // UUID
    sqQuery.SQL.Text :=
      'SELECT 1 FROM RDB$FIELDS ' +
      'WHERE RDB$FIELD_TYPE = 14 ' + // CHAR
      'AND RDB$FIELD_LENGTH = 63 ' +
      'AND RDB$CHARACTER_SET_ID = 1 ' + // 1 = OCTETS
      'ROWS 1';
    if not sqQuery.Transaction.InTransaction then
      sqQuery.Transaction.StartTransaction;
    sqQuery.Open;
    HasUUID := (sqQuery.RecordCount > 0);
    sqQuery.Close;

    if HasDecimal then List.Add('DECIMAL');
    if HasNumeric then List.Add('NUMERIC');
    if HasChar then List.Add('CHAR');
    if HasUUID then List.Add('UUID');
  finally
  end;
end;

procedure TdmSysTables.GetAllTypes(List: TStrings; dbIndex: integer);
var
  BasicList, ExtendedList: TStringList;
  i: Integer;
  ServerVersionMajor: word;
begin
  List.Clear;

  BasicList := TStringList.Create;
  ExtendedList := TStringList.Create;
  try
    GetBasicTypes(BasicList, dbIndex);
    GetExtendedTypes(ExtendedList, dbIndex);

    // Beide Listen zusammenführen
    for i := 0 to BasicList.Count - 1 do
      if List.IndexOf(BasicList[i]) = -1 then
        List.Add(BasicList[i]);

    for i := 0 to ExtendedList.Count - 1 do
      if List.IndexOf(ExtendedList[i]) = -1 then
        List.Add(ExtendedList[i]);

  finally

    BasicList.Free;
    ExtendedList.Free;
    CleanFirebirdTypeList(List);

  end;
end;

procedure TdmSysTables.GetDomainTypes(dbIndex: Integer; List: TStrings);
var
  Count: Integer;
begin
  List.CommaText:= List.CommaText + ',' + GetDBObjectNames(dbIndex, otDomains, Count);
end;

function TdmSysTables.GetDefaultTypeSize(dbIndex: Integer; TypeName: string): Integer;
begin
  TypeName:= LowerCase(TypeName);
  if TypeName = 'varchar' then
    Result:= 50
  else
  if TypeName = 'char' then
    Result:= 20
  else
  if TypeName = 'smallint' then
    Result:= 2
  else
  if TypeName = 'integer' then
    Result:= 4
  else
  if TypeName = 'bigint' then
    Result:= 8
  else
  if TypeName = 'int128' then //newlib
    Result:= 16
  else
  if TypeName = 'float' then
    Result:= 4
  else
  if TypeName = 'timestamp' then
    Result:= 8
  else
  if TypeName = 'timestamp with time zone' then //newlib
    Result:= 10
  else
  if TypeName = 'date' then
    Result:= 4
  else
  if TypeName = 'time' then
    Result:= 4
  else
  if TypeName = 'time with time zone' then  //newlib
    Result:= 6
  else
  if TypeName = 'double precision' then
    Result:= 8
  else
    Result:= GetDomainTypeSize(dbIndex, TypeName);
end;

function TdmSysTables.GetDomainTypeSize(dbIndex: Integer; DomainTypeName: string): Integer;
var
  DomainType, DefaultValue, CheckConstraint, CharacterSet, Collation: string;
begin
  GetDomainInfo(dbIndex, DomainTypeName, DomainType, Result, DefaultValue, CheckConstraint, CharacterSet, Collation);
end;


function TdmSysTables.GetFieldInfo(dbIndex: Integer; TableName, FieldName: string;
  var FieldType: string;
  var FieldSize: integer;  var FieldPrecision: integer; var FieldScale: integer;
  var NotNull: Boolean;
  var DefaultValue, CharacterSet, Collation, Description : string): Boolean;
begin
  Init(dbIndex);
  sqQuery.SQL.Text:= 'SELECT r.RDB$FIELD_NAME AS field_name, ' +
    ' r.RDB$DESCRIPTION AS field_description, ' +
    ' r.RDB$DEFAULT_SOURCE AS field_default_source, ' {SQL text for default value}+
    ' r.RDB$NULL_FLAG AS field_not_null_constraint, ' +
    ' f.RDB$FIELD_LENGTH AS field_length, ' +
    ' f.RDB$Character_LENGTH AS characterlength, ' + {character_length seems a reserved word }
    ' f.RDB$FIELD_PRECISION AS field_precision, ' +
    ' f.RDB$FIELD_SCALE AS field_scale, ' +
    ' f.RDB$FIELD_TYPE as field_type_int, ' +
    ' f.RDB$FIELD_SUB_TYPE AS field_sub_type, ' +
    ' coll.RDB$COLLATION_NAME AS field_collation, ' +
    ' cset.RDB$CHARACTER_SET_NAME AS field_charset, ' +
    ' f.RDB$computed_source AS computed_source, ' +
    ' dim.RDB$UPPER_BOUND AS array_upper_bound, ' +
    ' r.RDB$FIELD_SOURCE AS field_source ' {domain if field based on domain} +
    ' FROM RDB$RELATION_FIELDS r ' +
    ' LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
    ' LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID and f.rdb$character_set_id=coll.rdb$character_set_id ' +
    ' LEFT JOIN RDB$CHARACTER_SETS cset ON f.RDB$CHARACTER_SET_ID = cset.RDB$CHARACTER_SET_ID ' +
    ' LEFT JOIN RDB$FIELD_DIMENSIONS dim on f.RDB$FIELD_NAME = dim.RDB$FIELD_NAME '+
    ' WHERE r.RDB$RELATION_NAME = ' + QuotedStr(TableName) +  ' and Trim(r.RDB$FIELD_NAME) =  ' +  QuotedStr(FieldName)  +
    '  ORDER BY r.RDB$FIELD_POSITION ';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  //Result:= sqQuery.RecordCount > 0;
  Result:= not sqQuery.IsEmpty;
  if Result then
  begin
    with sqQuery do
    begin
      //todo: harmonize with implementation in turbocommon
      if (FieldByName('field_source').IsNull) or
        (trim(FieldByName('field_source').AsString)='') or
        (IsFieldDomainSystemGenerated(trim(FieldByname('field_source').AsString))) then
      begin
        // Field type is not based on a domain but a standard SQL type
        FieldType:= GetFBTypeName(FieldByName('field_type_int').AsInteger,  // in turbocommon
          FieldByName('field_sub_type').AsInteger,
          FieldByName('field_length').AsInteger,
          FieldByName('field_precision').AsInteger,
          FieldByName('field_scale').AsInteger,
          FieldByName('field_charset').AsString,
          FieldByName('characterlength').AsInteger
          );
        // Array should really be [lowerbound:upperbound] (if dimension is 0)
        // but for now don't bother as arrays are not supported anyway
        // Assume 0 dimension, 1 lower bound; just fill in upper bound
        if not(FieldByName('array_upper_bound').IsNull) then
          FieldType := FieldType +
            ' [' +
            FieldByName('array_upper_bound').AsString +
            ']';
        if FieldByName('field_type_int').AsInteger = VarCharType then
          FieldSize:= FieldByName('characterlength').AsInteger
        else
          FieldSize:= FieldByName('field_length').AsInteger;
      end
      else
      begin
        // Field is based on a domain
        FieldType:= trim(FieldByName('field_source').AsString);
        // Reset other value to avoid strange values
        FieldSize:= 0;
      end;
      FieldPrecision:= FieldByName('field_precision').AsInteger;
      FieldScale:= FieldByName('field_scale').AsInteger;
      NotNull:= (FieldByName('field_not_null_constraint').AsString = '1');
      Collation:= trim(FieldByName('field_collation').AsString);
      CharacterSet:= trim(FieldByName('field_charset').AsString);
      // Note: no trim here - defaultvalue could be an empty string
      //DefaultValue:= FieldByName('field_default_source').AsString;
      DefaultValue := Trim(FieldByName('field_default_source').AsString);
      if AnsiStartsText('DEFAULT ', DefaultValue) then
        system.Delete(DefaultValue, 1, Length('DEFAULT '));
      Description:= trim(FieldByName('field_description').AsString);
    end;
  end;
  sqQuery.Close;
end;

function TdmSysTables.GetDatabaseInfo(
  dbIndex: Integer; var ADatabaseName, CharSet, CreationDate, ServerTime: string;
  var ODSVerMajor, ODSVerMinor, Pages, PageSize: Integer;
  var ProcessList: TStringList; var ErrorMsg: string
): Boolean;
var
  DB: TIBDatabase;
  Trans: TIBTransaction;
  SQL: TIBQuery;
begin
  Result := False;
  ErrorMsg := '';

  DB := TIBDatabase.Create(nil);
  try
    Trans := TIBTransaction.Create(nil);
    try
      SQL := TIBQuery.Create(nil);
      try
        // --- Datenbank vorbereiten ---
        DB.Params.Clear;
        DB.DatabaseName := Trim(RegisteredDatabases[dbIndex].RegRec.DatabaseName);
        DB.Params.Add('user_name=' + RegisteredDatabases[dbIndex].RegRec.UserName);
        DB.Params.Add('password=' + RegisteredDatabases[dbIndex].RegRec.Password);
        DB.Params.Add('sql_role_name=' + RegisteredDatabases[dbIndex].RegRec.Role);
        DB.Params.Add('lc_ctype=' + RegisteredDatabases[dbIndex].RegRec.Charset);

        DB.OnLogin := @dmSysTables.OnDatabaseLogin;
        DB.LoginPrompt := True;

        DB.FirebirdLibraryPathName := RegisteredDatabases[dbIndex].RegRec.FireBirdClientLibPath;

        Trans.DefaultDatabase := DB;
        //DB.DefaultTransaction := Trans;

        SQL.Database := DB;
        SQL.Transaction := Trans;

        // --- Verbinden ---
        DB.Connected := True;

        // --- Charset ---
        Trans.StartTransaction;
        try
          SQL.SQL.Text := 'select * from RDB$DATABASE';
          SQL.Open;
          CharSet := Trim(SQL.FieldByName('RDB$CHARACTER_SET_NAME').AsString);
        finally
          SQL.Close;
          Trans.Rollback;
        end;

        // --- DB-Infos ---
        Trans.StartTransaction;
        try
          SQL.SQL.Text := 'select * from MON$DATABASE';
          SQL.Open;
          ADatabaseName := Trim(SQL.FieldByName('MON$DATABASE_NAME').AsString);
          PageSize := SQL.FieldByName('MON$PAGE_SIZE').AsInteger;
          ODSVerMajor := SQL.FieldByName('MON$ODS_MAJOR').AsInteger;
          ODSVerMinor := SQL.FieldByName('MON$ODS_MINOR').AsInteger;
          CreationDate := Trim(SQL.FieldByName('MON$CREATION_DATE').AsString);
          Pages := SQL.FieldByName('MON$PAGES').AsInteger;
        finally
          SQL.Close;
          Trans.Rollback;
        end;

        // --- Clients ---
        Trans.StartTransaction;
        try
          SQL.SQL.Text := 'select * from MON$ATTACHMENTS';
          SQL.Open;
          while not SQL.Eof do
          begin
            ProcessList.Add(
              'Host: ' + Trim(SQL.FieldByName('MON$REMOTE_ADDRESS').AsString) +
              '   User: ' + Trim(SQL.FieldByName('MON$USER').AsString) +
              '   Process: ' + Trim(SQL.FieldByName('MON$REMOTE_PROCESS').AsString)
            );
            SQL.Next;
          end;
        finally
          SQL.Close;
          Trans.Rollback;
        end;

        // --- Serverzeit ---
        Trans.StartTransaction;
        try
          SQL.SQL.Text := 'select current_timestamp from RDB$DATABASE';
          SQL.Open;
          ServerTime := SQL.Fields[0].AsString;
        finally
          SQL.Close;
          Trans.Rollback;
        end;

        Result := True;

      except
        on E: Exception do
        begin
          ErrorMsg := E.Message;
          Result := False;
        end;
      end;
      SQL.Transaction := nil;
      SQL.Database := nil;
      FreeAndNil(SQL);

    finally
      if Trans.InTransaction then
        Trans.Rollback;
      Trans.DefaultDatabase := nil;
      FreeAndNil(Trans);
    end;

    if DB.Connected then
    begin
      try
        DB.Connected := False;
      except
        //
      end;
    end;

  finally
    FreeAndNil(DB);
  end;
end;


function TdmSysTables.GetIndices(dbIndex: Integer; ATableName: string; PrimaryIndexName: string;
  var List: TStringList): Boolean;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'SELECT * FROM RDB$INDICES WHERE RDB$RELATION_NAME=''' + UpperCase(ATableName) +
    ''' AND RDB$FOREIGN_KEY IS NULL';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  //Result:= sqQuery.RecordCount > 0;
  Result:= not sqQuery.IsEmpty;
  with sqQuery do
  if Result then
  begin
    while not Eof do
    begin
      if UpperCase(Trim(PrimaryIndexName)) <> Trim(Fields[0].AsString) then
        List.Add(Trim(Fields[0].AsString));
      Next;
    end;
  end;
  sqQuery.Close
end;

{function TdmSysTables.GetAllIndices(dbIndex: Integer; List, TablesList: TStringList): Boolean;
const
  SQL = 'SELECT * FROM RDB$INDICES ' +
    'WHERE RDB$FOREIGN_KEY IS NULL ' +
    'and RDB$system_flag = 0';
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= SQL;
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  //Result:= sqQuery.RecordCount > 0;
  Result:= not sqQuery.IsEmpty;
  List.Clear;
  if TablesList <> nil then
    TablesList.Clear;
  with sqQuery do
  if Result then
  begin
    while not Eof do
    begin
      List.Add(Trim(Fields[0].AsString));
      if TablesList <> nil then
        TablesList.Add(Trim(FieldByName('RDB$Relation_Name').AsString));
      Next;
    end;
  end;
  sqQuery.Close
end;}

function TdmSysTables.GetAllIndices(
  dbIndex: Integer;
  List, TablesList: TStringList
): Boolean;
const
  SQL =
    'SELECT '+
    ' RDB$INDEX_NAME, '+
    ' RDB$RELATION_NAME '+
    'FROM RDB$INDICES '+
    'WHERE RDB$SYSTEM_FLAG = 0 '+
    ' AND RDB$FOREIGN_KEY IS NULL '+
    'ORDER BY RDB$RELATION_NAME, RDB$INDEX_NAME';
begin
  Result := False;

  Init(dbIndex);

  List.Clear;

  if Assigned(TablesList) then
    TablesList.Clear;

  sqQuery.Close;
  sqQuery.SQL.Text := SQL;

  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;

  try
    sqQuery.Open;

    Result := not sqQuery.IsEmpty;

    while not sqQuery.EOF do
    begin
      List.Add(
        Trim(sqQuery.FieldByName('RDB$INDEX_NAME').AsString)
      );

      if Assigned(TablesList) then
        TablesList.Add(
          Trim(sqQuery.FieldByName('RDB$RELATION_NAME').AsString)
        );

      sqQuery.Next;
    end;

    sqQuery.Close;

    sqQuery.Transaction.Commit;

  except
    sqQuery.Transaction.Rollback;
    raise;
  end;
end;

function TdmSysTables.GetPrimaryKeyIndexName(dbIndex: Integer; ATableName: string; var ConstraintName: string): string;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'select RDB$Index_name, RDB$Constraint_Name from RDB$RELATION_CONSTRAINTS ' +
    'where RDB$Relation_Name = ''' + UpperCase(ATableName) + ''' and RDB$Constraint_Type = ''PRIMARY KEY'' ';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  if sqQuery.RecordCount > 0 then
  begin
    Result:= Trim(sqQuery.Fields[0].AsString);
    ConstraintName:= Trim(sqQuery.Fields[1].AsString);
  end
  else
    Result:= '';
  sqQuery.Close;
end;

function TdmSysTables.GetIndexInfo(dbIndex: Integer; ATableName, AIndexName: string;
  var FieldsList: TStringList; var ConstraintName: string; var Unique, Ascending, IsPrimary: Boolean): Boolean;
begin
  Init(dbIndex);
  sqQuery.Close;
  sqQuery.SQL.Text:= 'SELECT RDB$Indices.*, RDB$INDEX_SEGMENTS.RDB$FIELD_NAME AS field_name, ' + LineEnding +
     'RDB$INDICES.RDB$DESCRIPTION AS description, ' + LineEnding +
     '(RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION + 1) AS field_position, ' + LineEnding +
     'RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE as IndexType, ' + LineEnding +
     'RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_Name as ConstraintName' + LineEnding +
     'FROM RDB$INDEX_SEGMENTS ' + LineEnding +
     'LEFT JOIN RDB$INDICES ON RDB$INDICES.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' + LineEnding +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS ON RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME '
     + LineEnding +
     ' WHERE UPPER(RDB$INDICES.RDB$RELATION_NAME)=''' + UpperCase(ATablename) + '''         -- table name ' + LineEnding +
     '  AND UPPER(RDB$INDICES.RDB$INDEX_NAME)=''' + UpperCase(AIndexName) + ''' ' + LineEnding +
     'ORDER BY RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION;';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  Result:= sqQuery.FieldCount > 0;
  if Result then
  begin
    Unique:= sqQuery.FieldByName('RDB$Unique_Flag').AsString = '1';
    Ascending:= sqQuery.FieldByName('RDB$Index_Type').AsString <> '1';
    IsPrimary:= Trim(sqQuery.FieldByName('IndexType').AsString) = 'PRIMARY KEY';
    ConstraintName:= Trim(sqQuery.FieldByName('ConstraintName').AsString);
  end;
  FieldsList.Clear;
  if Result then
  while not sqQuery.EOF do
  begin
    FieldsList.Add(Trim(sqQuery.FieldByName('field_name').AsString));
    sqQuery.Next;
  end;
  sqQuery.Close;
end;

procedure TdmSysTables.GetTableFields(dbIndex: Integer; ATableName: string; FieldsList: TStringList);
var
  FieldName: string;
begin
  Init(dbIndex);
  sqQuery.SQL.Text:= 'SELECT r.RDB$FIELD_NAME AS field_name, ' +
      ' r.RDB$DESCRIPTION AS field_description, ' +
      ' r.RDB$DEFAULT_SOURCE AS field_default_source, ' {SQL source for default value}+
      ' r.RDB$NULL_FLAG AS field_not_null_constraint, ' +
      ' f.RDB$FIELD_LENGTH AS field_length, ' +
      ' f.RDB$FIELD_PRECISION AS field_precision, ' +
      ' f.RDB$FIELD_SCALE AS field_scale, ' +
      ' f.RDB$FIELD_TYPE as field_type_int, ' +
      ' f.RDB$FIELD_SUB_TYPE AS field_sub_type, ' +
      ' coll.RDB$COLLATION_NAME AS field_collation, ' +
      ' cset.RDB$CHARACTER_SET_NAME AS field_charset, ' +
      ' f.RDB$computed_source AS computed_source, ' +
      ' dim.RDB$UPPER_BOUND AS array_upper_bound, ' +
      ' r.RDB$FIELD_SOURCE AS field_source ' {domain if field based on domain} +
      ' FROM RDB$RELATION_FIELDS r ' +
      ' LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      ' LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID ' +
      ' LEFT JOIN RDB$CHARACTER_SETS cset ON f.RDB$CHARACTER_SET_ID = cset.RDB$CHARACTER_SET_ID ' +
      ' LEFT JOIN RDB$FIELD_DIMENSIONS dim on f.RDB$FIELD_NAME = dim.RDB$FIELD_NAME '+
      ' WHERE r.RDB$RELATION_NAME=''' + ATableName + '''  ' +
      ' ORDER BY r.RDB$FIELD_POSITION;';
  if not sqQuery.Transaction.InTransaction then
    sqQuery.Transaction.StartTransaction;
  sqQuery.Open;
  FieldsList.Clear;
  while not sqQuery.EOF do
  begin
    FieldName:= Trim(sqQuery.FieldByName('field_name').AsString);
    if FieldsList.IndexOf(FieldName) = -1 then
      FieldsList.Add(FieldName);
    sqQuery.Next;
  end;
  sqQuery.Close;
end;

initialization
  {$I systables.lrs}

end.

