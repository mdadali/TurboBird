unit turbocommon;

{ Non-GUI common code for TurboBird that do not depend on a database connection.
SysTables covers functionality for which a db connection is required. }
{$mode objfpc}{$H+}

interface

uses

  Forms,Graphics,  Types, Classes, StdCtrls, ComCtrls, SysUtils, StrUtils, DateUtils, IniFiles, Dialogs, {$IFDEF WINDOWS} Windows, {$ENDIF}
  Controls,
  AbUnzper,   AbZBrows, AbArcTyp, AbZipTyp,
  LCLType, LCLVersion, versiontypes, versionresource,
  interfaces, LCLPlatformDef,
  DB,  RegExpr,

  fpstdexports,
  fpDataExporter,

  IB,
  IBDatabase,
  IBDatabaseInfo,
  IBQuery,

  Variants,

  fbcommon,
  fSetFBClient,
  fServerSession,

  SysTables,
  EnterPass;


{$I version.inc}

const

  CompileDate = {$I %DATE%};  // z. B. '2025/07/25'
  CompileTime = {$I %TIME%};  // z. B. '17:42:33'

  // Some field types used in e.g. RDB$FIELDS
  // todo: (low priority) perhaps move to enumeration with fixed constant values
  BlobType = 261;
  CharType = 14;
  CStringType = 40; // probably null-terminated string used for UDFs
  VarCharType = 37;

  // Available character set encodings for Firebird.
  // Update this whenever Firebird supports new character sets
  DefaultFBCharacterSet=42; //Used for GUI controls etc. UTF8 in CharacterSets below.
  // Available character sets as per Firebird 2.5
  FBCharacterSets: array[0..51] of string =
    ('NONE',
    'ASCII',
    'BIG_5',
    'CP943C',
    'CYRL',
    'DOS437',
    'DOS737',
    'DOS775',
    'DOS850',
    'DOS852',
    'DOS857',
    'DOS858',
    'DOS860',
    'DOS861',
    'DOS862',
    'DOS863',
    'DOS864',
    'DOS865',
    'DOS866',
    'DOS869',
    'EUCJ_0208',
    'GB18030',
    'GBK',
    'GB_2312',
    'ISO8859_1',
    'ISO8859_13',
    'ISO8859_2',
    'ISO8859_3',
    'ISO8859_4',
    'ISO8859_5',
    'ISO8859_6',
    'ISO8859_7',
    'ISO8859_8',
    'ISO8859_9',
    'KOI8R',
    'KOI8U',
    'KSC_5601',
    'NEXT',
    'OCTETS',
    'SJIS_0208',
    'TIS620',
    'UNICODE_FSS', //obsolete
    'UTF8', //good default
    'WIN1250',
    'WIN1251',
    'WIN1252',
    'WIN1253',
    'WIN1254',
    'WIN1255',
    'WIN1256',
    'WIN1257',
    'WIN1258');
  // Available collations as per Firebird 2.5
  // Pairs of collation names and the character set name
  // that must be used to support this collation
  FBCollations: array[0..148-1,0..1] of string =
    (
    ('ASCII','ASCII'),
    ('BIG_5','BIG_5'),
    ('BS_BA','WIN1250'),
    ('CP943C','CP943C'),
    ('CP943C_UNICODE','CP943C'),
    ('CS_CZ','ISO8859_2'),
    ('CYRL','CYRL'),
    ('DA_DA','ISO8859_1'),
    ('DB_CSY','DOS852'),
    ('DB_DAN865','DOS865'),
    ('DB_DEU437','DOS437'),
    ('DB_DEU850','DOS850'),
    ('DB_ESP437','DOS437'),
    ('DB_ESP850','DOS850'),
    ('DB_FIN437','DOS437'),
    ('DB_FRA437','DOS437'),
    ('DB_FRA850','DOS850'),
    ('DB_FRC850','DOS850'),
    ('DB_FRC863','DOS863'),
    ('DB_ITA437','DOS437'),
    ('DB_ITA850','DOS850'),
    ('DB_NLD437','DOS437'),
    ('DB_NLD850','DOS850'),
    ('DB_NOR865','DOS865'),
    ('DB_PLK','DOS852'),
    ('DB_PTB850','DOS850'),
    ('DB_PTG860','DOS860'),
    ('DB_RUS','CYRL'),
    ('DB_SLO','DOS852'),
    ('DB_SVE437','DOS437'),
    ('DB_SVE850','DOS850'),
    ('DB_TRK','DOS857'),
    ('DB_UK437','DOS437'),
    ('DB_UK850','DOS850'),
    ('DB_US437','DOS437'),
    ('DB_US850','DOS850'),
    ('DE_DE','ISO8859_1'),
    ('DOS437','DOS437'),
    ('DOS737','DOS737'),
    ('DOS775','DOS775'),
    ('DOS850','DOS850'),
    ('DOS852','DOS852'),
    ('DOS857','DOS857'),
    ('DOS858','DOS858'),
    ('DOS860','DOS860'),
    ('DOS861','DOS861'),
    ('DOS862','DOS862'),
    ('DOS863','DOS863'),
    ('DOS864','DOS864'),
    ('DOS865','DOS865'),
    ('DOS866','DOS866'),
    ('DOS869','DOS869'),
    ('DU_NL','ISO8859_1'),
    ('EN_UK','ISO8859_1'),
    ('EN_US','ISO8859_1'),
    ('ES_ES','ISO8859_1'),
    ('ES_ES_CI_AI','ISO8859_1'),
    ('EUCJ_0208','EUCJ_0208'),
    ('FI_FI','ISO8859_1'),
    ('FR_CA','ISO8859_1'),
    ('FR_FR','ISO8859_1'),
    ('FR_FR_CI_AI','ISO8859_1'),
    ('GB18030','GB18030'),
    ('GB18030_UNICODE','GB18030'),
    ('GBK','GBK'),
    ('GBK_UNICODE','GBK'),
    ('GB_2312','GB_2312'),
    ('ISO8859_1','ISO8859_1'),
    ('ISO8859_13','ISO8859_13'),
    ('ISO8859_2','ISO8859_2'),
    ('ISO8859_3','ISO8859_3'),
    ('ISO8859_4','ISO8859_4'),
    ('ISO8859_5','ISO8859_5'),
    ('ISO8859_6','ISO8859_6'),
    ('ISO8859_7','ISO8859_7'),
    ('ISO8859_8','ISO8859_8'),
    ('ISO8859_9','ISO8859_9'),
    ('ISO_HUN','ISO8859_2'),
    ('ISO_PLK','ISO8859_2'),
    ('IS_IS','ISO8859_1'),
    ('IT_IT','ISO8859_1'),
    ('KOI8R','KOI8R'),
    ('KOI8R_RU','KOI8R'),
    ('KOI8U','KOI8U'),
    ('KOI8U_UA','KOI8U'),
    ('KSC_5601','KSC_5601'),
    ('KSC_DICTIONARY','KSC_5601'),
    ('LT_LT','ISO8859_13'),
    ('NEXT','NEXT'),
    ('NONE','NONE'),
    ('NO_NO','ISO8859_1'),
    ('NXT_DEU','NEXT'),
    ('NXT_ESP','NEXT'),
    ('NXT_FRA','NEXT'),
    ('NXT_ITA','NEXT'),
    ('NXT_US','NEXT'),
    ('OCTETS','OCTETS'),
    ('PDOX_ASCII','DOS437'),
    ('PDOX_CSY','DOS852'),
    ('PDOX_CYRL','CYRL'),
    ('PDOX_HUN','DOS852'),
    ('PDOX_INTL','DOS437'),
    ('PDOX_ISL','DOS861'),
    ('PDOX_NORDAN4','DOS865'),
    ('PDOX_PLK','DOS852'),
    ('PDOX_SLO','DOS852'),
    ('PDOX_SWEDFIN','DOS437'),
    ('PT_BR','ISO8859_1'),
    ('PT_PT','ISO8859_1'),
    ('PXW_CSY','WIN1250'),
    ('PXW_CYRL','WIN1251'),
    ('PXW_GREEK','WIN1253'),
    ('PXW_HUN','WIN1250'),
    ('PXW_HUNDC','WIN1250'),
    ('PXW_INTL','WIN1252'),
    ('PXW_INTL850','WIN1252'),
    ('PXW_NORDAN4','WIN1252'),
    ('PXW_PLK','WIN1250'),
    ('PXW_SLOV','WIN1250'),
    ('PXW_SPAN','WIN1252'),
    ('PXW_SWEDFIN','WIN1252'),
    ('PXW_TURK','WIN1254'),
    ('SJIS_0208','SJIS_0208'),
    ('SV_SV','ISO8859_1'),
    ('TIS620','TIS620'),
    ('TIS620_UNICODE','TIS620'),
    ('UCS_BASIC','UTF8'),
    ('UNICODE','UTF8'),
    ('UNICODE_CI','UTF8'),
    ('UNICODE_CI_AI','UTF8'),
    ('UNICODE_FSS','UNICODE_FSS'),
    ('UTF8','UTF8'),
    ('WIN1250','WIN1250'),
    ('WIN1251','WIN1251'),
    ('WIN1251_UA','WIN1251'),
    ('WIN1252','WIN1252'),
    ('WIN1253','WIN1253'),
    ('WIN1254','WIN1254'),
    ('WIN1255','WIN1255'),
    ('WIN1256','WIN1256'),
    ('WIN1257','WIN1257'),
    ('WIN1257_EE','WIN1257'),
    ('WIN1257_LT','WIN1257'),
    ('WIN1257_LV','WIN1257'),
    ('WIN1258','WIN1258'),
    ('WIN_CZ','WIN1250'),
    ('WIN_CZ_CI_AI','WIN1250'),
    ('WIN_PTBR','WIN1252')
    );

type

  TCommitKind = (tctCommit, tctCommitRetaining);

  // Types of objects in database
  // Note: the order and count must match the array below
  // Also, do not assign values to the individual enums; code depends
  // on them starting with 0 and being contiguous

  TTreeViewObjectType = (
  tvotNone,
  tvotServerAlias,
  tvotServerInfo,
  tvotServerDatabases,
  tvotServer,
  tvotEmbeddedServer,
  tvotDB,
  tvotQueryWindow,
  tvotTableRoot,
  tvotTable,
  tvotTableField,
  tvotGeneratorRoot,
  tvotGenerator,
  tvotTriggerRoot,
  tvotTrigger,
  tvotViewRoot,
  tvotView,
  tvotSystemTableRoot,
  tvotSystemTable,
  tvotDomainRoot, {excludes system domains}
  tvotDomain,
  tvotRoleRoot,
  tvotRole,
  tvotExceptionRoot,
  tvotException,
  tvotUserRoot,
  tvotUser,
  tvotIndexRoot,
  tvotIndex,
  tvotConstraintRoot,
  tvotConstraint,
  tvotStoredProcedureRoot,
  tvotStoredProcedure,
  tvotFunctionRoot,
  tvotFunction,
  tvotUDRoot,
  tvotUDFRoot,
  tvotUDFFunction,
  tvotUDRRoot,
  tvotUDRFunctionRoot,
  tvotUDRFunction,
  tvotUDRProcedureRoot,
  tvotUDRProcedure,
  tvotUDRTriggerRoot,
  tvotUDRTrigger,
  tvotPackageRoot,
  tvotPackage,
  tvotPackageFunctionRoot,
  tvotPackageFunction,
  tvotPackageProcedureRoot,
  tvotPackageProcedure,
  tvotPackageUDRFunctionRoot,
  tvotPackageUDRFunction,
  tvotPackageUDRProcedureRoot,
  tvotPackageUDRProcedure,
  tvotPackageUDRTriggerRoot,
  tvotPackageUDRTrigger
  );

type
  TServerRecord = packed record
    ServerName: string[127];
    ServerAlias: string[127];

    VersionMajor: word;
    VersionString: string[50];
    VersionMinor: word;

    UserName: string[63];
    Password: string[63];
    Role: string[63];
    Protocol: TProtocol;
    Port: string[10];
    Charset: string[31];
    RootPath: string[255];
    ClientLibraryPath: string[255];
    ConfigFilePath: string[255];
    LoadRegisteredClientLib: boolean;
    IsEmbedded: Boolean;

    // Neue Felder:
    ConnectTimeoutMS: LongInt;   // Timeout beim Verbindungsaufbau (Millisekunden)
    RetryCount: SmallInt;        // Anzahl Wiederholversuche
    QueryTimeoutMS: LongInt;     // Timeout für lange Operationen (optional)

    Reserved: array[0..30] of Byte; // (etwas verkleinert)
  end;


  TTransactionConfigRecord = packed record
    TxIsolation: Byte;           // 0=ReadCommitted,1=ReadCommittedRecVer,2=RepeatableRead,3=Serializable
    TxAccessMode: Boolean;       // True=ReadWrite, False=ReadOnly
    TxWaitMode: Boolean;         // True=Wait, False=NoWait
    TxAutoCommit: Boolean;
    TxTimeout: LongInt;          // ms
    StatementTimeout: LongInt;   // ms
    LockTimeout: LongInt;        // ms
    ReadConsistency: Boolean;
    CommitRetaining: Boolean;
    ForceAutoCommitOnDDL: Boolean;
    TxName: string[50];          // optional Label / Alias
  end;

  TRegisteredDatabase = packed record
    Title: string[30];
    DatabaseName: string[200];
    UserName: string[100];
    Password: string[100];
    Role: string[100];
    Charset: string[40];
    SQLDialect: string[1];

    {
    ODSMinor: word;
    ODSMajor: word;
    ConnectString: string[255];
    DBOwner: string[100];
    DateDBCreated: DateTime;
    PageSize: integer;
    //PageAllocated,
    //PageUsed,
    //PageAvailable,
    NumberOfBuffers: bigInt;
    //CurrentMemory: Bigint;
    MaxMemory: BigInt;
    LingerDelay: bigint; //seconds
    SweepIntervall: bigint; //transactions
    ReadOnly: boolean;
    Online: boolean;
    SynchronousDiskWrites: boolean;
    ShadowDatabase: boolean;
    SpaceReservedForBackupRecords: boolean;}

    Deleted: Boolean;
    SavePassword: Boolean;
    LastOpened: TDateTime;
    ServerName: string[255];
    FireBirdClientLibPath: string[255];
    OverwriteLoadedClientLib: boolean;
    Port: string[10];
    ConnectOnApplicationStart: boolean;

    ServerVersionMajor: word;
    ServerVersionString: string[50];  //LI-V6.3.3.1683 Firebird 5.0
    ServerVersionMinor: word;

    Reserved: array [0 .. 40] of Byte;
  end;

  TDatabaseRec = record
    Index: Integer;    // for sort
    RegRec: TRegisteredDatabase;
    OrigRegRec: TRegisteredDatabase;
    IBDatabase: TIBDatabase;
    IBTransaction: TIBTransaction;
    IBQuery: TIBQuery;
    IBDatabaseInfo: TIBDatabaseInfo;
    TxConfig: TTransactionConfigRecord; // NEU: alle Transaction-Parameter
  end;

  TPNodeInfos = ^TNodeInfos;
  TNodeInfos = record
    dbIndex: integer;
    ObjectType: TTreeViewObjectType;
    PopupMenuTag: integer;
    ImageIndex: Integer;
    ViewForm,
    EditorForm,
    NewForm,
    ExecuteForm: TForm;

    ServerSession: TServerSession;
  end;

  //search
  TDBField = (
   dbfTitle,
   dbfDatabaseName,
   dbfUserName,
   dbfPassword,
   dbfCharset,
   dbfRole,
   dbfLastOpened,
   dbfDeleted,
   dbfSavePassword
  );

  TDBSearchResult = record
     Index: Integer;
     Rec: TRegisteredDatabase;
   end;

  TDBCheckResult = record
    Title: string;
    DBName: string;
    DateTimeChecked: TDateTime;
    CharsetIssues: TStringList;
    LengthIssues: TStringList;
    NotNullIssues: TStringList;
    DataTypeIssues: TStringList;
    PKIssues: TStringList;
    FKIssues: TStringList;
    ViewsIssues: TStringList;
    IndexUniqueIssues: TStringList;
    TriggerDefaultIssues: TStringList;
  end;


const
  InitialServiceUser = 'service_user';
  InitialServiceUserPwd = 'service_pwd';

  DatabasesRegFile = 'databases.reg';
  ServersRegFile   = 'servers.reg';
  ThemesIniFile    = 'theme.ini';
  TmpDir = 'temp';

var
    MainTreeView: TTreeView;

    fLanguage: string;
    fIniFileName: string;
    fIniFile: TInifile;

    RegisteredServers: array of TServerRecord;
    RegisteredDatabases: array of TDatabaseRec;

    MultiVersionConnection: string;

    InitialFBClientLibPath: string;
    InitialFBClient: IFirebirdLibrary;

    FB25, FB30, FB40, FB50, FB60: IFirebirdLibrary;

    //Backup options
    CloseDBBeforeBackup: boolean;

    //Restore options
    DefaultPageSize,
    DefaultNumBuffers: integer;
    RegisterDBAfterRestore: boolean;

    //Scriper
    EchoInput: boolean;
    StopOnFirstError: boolean;
    AutoDDL: boolean;
    IgnoreCreateDatabase: boolean;
    IgnoreGrants: boolean;
    ShowAffectedRows: boolean;
    ShowPerformanceStats: boolean;

    //Export to Clipboard
    MaxExportRows: integer;


function MakeConnectionString(AServerName, APort, ADBFileName: string): string;

function IsServerReachable(AserverName: string; out ErrorStr: string): boolean;

function GetDBFileNameFromConnectionString(AConnStr: string): string;

function GetSeverImplemetationVersionFromIBDB(ADB: TIBDatabase): string;
function GetSeverImplemetationVersionFromDBIndex(dbIndex: word): string;
function GetServerMajorVersionFromDBIndex(dbIndex: Word): Word;
function GetServerMinorVersionFromDBIndex(dbIndex: Word): Word;
function GetServerMajorVersionFromIBDB(ADB: TIBDatabase): Word;
function GetServerMinorVersionFromIBDB(ADB: TIBDatabase): Word;

function ConnectToDBAs(dbIndex: Integer; ForceConnectDialog: boolean=false): Boolean;

function LoadClientLibIBX(ALib: string): boolean;
function SetInitialClientLib: boolean;

function TestEmbeddedConnection(AServerRec: TServerRecord; out ODSMajor: Integer; out ODSMinor: integer; out ServerVersion: string): boolean;

function  CloseDB(dbIndex: integer): boolean;

function DeleteDBRegistrationFromFile(const ATitle: string): Boolean;
//Remove from RegisteredDatabases array!
procedure UnregisterDatabaseByTitle(const ATitle: string);


procedure MarkAllServerDatabasesDeleted(const ServerName: string);
procedure MarkAllDatabasesDeleted;
procedure RemoveDeletedDBRegistrationsFromFile;

function IsValidPortNr(AStr: string): boolean;
function ServerNameContainPortNr(AServerName: string): boolean;
function GetPortNrFromServerName(AServerName: string): string;
function GetHostFromServerName(AServerName: string): string;

procedure AssignIBDatabase(const Source, Target: TIBDatabase);
function AreSameDB(const DB1, DB2: TIBDatabase): Boolean;

function GetServerName(DBName: string): string;

function GetServerRecordFromFileByName(AServerName: string): TServerRecord;
function GetFirstServerRecordFromFile: TServerRecord;
function GetServerRecordFromFileByIndex(AIdx: Integer): TServerRecord;

procedure SaveServerDataToFile(const Rec: TServerRecord);
procedure ApplyServerRecordToSession(const Rec: TServerRecord; Session: TServerSession);
function  BuildServerRecordFromSession(const Session: TServerSession; SavePwd: Boolean): TServerRecord;

function GetServerNodeByServerName(const AServerName: string): TTreeNode;
function GetServerListFromTreeView: TStringList;
function GetServerAndPortListFromTreeView: TStringList;


function GetAncestorAtLevel(ANode: TTreeNode; ALevel: Integer): TTreeNode;
function GetAncestorNodeText(ANode: TTreeNode; ALevel: Integer): string;

function GetServerNameFromConnString(ADBIndex: word): string;
function GetDBNameFromConnString(ADBIndex: word): string;

function GetConfigurationDirectory: string;
procedure ReadIniFile;
procedure WriteIniFile;
function FirstRun: boolean;
procedure ExtractResources;
function ExtractVersionFromName(const Name: string): string;
function GetProgramVersion: string;
function GetLazarusVersion: string;
function GetFPCVersion: string;
function ReadOSReleaseFile: string;
function ReadFileToString(const AFileName: string): string;
function GetOSInfo: string;
function GetLCLWidgetSet: string;
function GetProgramBuildDate: string;
function GetProgramBuildTime: string;

function StripQuotes(const S: string): string;
function ExtractDefaultValue(const DefaultSource: string): string;
function IsValidUUIDHex(const S: string): Boolean;
function CreateUUIDHexLiteral: string;
function IsSizedTypeName(const ATypeName: string): Boolean;
function GetNameFromSizedTypeName(const ATypeName: string): string;
function GetSizeFromSizedTypeName(const ATypeName: string): string;
procedure GetPrecisionAndScaleFromSizedTypeName(const ATypeName: string; out Precision, Scale: string);
function ExtractObjectName(const Input: string): string; // Tables(11) -> Tables
function FormatNodeCaptionnWithCount(const Text: string; Count: Integer): string;
function GetClearNodeText(const ANodeText: string): string;
function RoutineTypeToTreeViewObjectType(RT: TRoutineType): TTreeViewObjectType;
function GetRootObjectTypeFor(AObjectType: TTreeViewObjectType): TTreeViewObjectType;

function FBObjectToStr(AType: TObjectType): string;
function TreeViewObjectToStr(AType: TTreeViewObjectType): string;



// Retrieve available collations for specified Characterset into Collations
function GetCollations(const Characterset: string; var Collations: TStringList): boolean;

// Given field retrieval query in FieldQuery, return field type and size.
// Includes support for field types that are domains and arrays
procedure GetFieldType(FieldQuery: TIBQuery; var FieldType: string; var FieldSize: integer);

// Returns field type DDL given a RDB$FIELD_TYPE value as well
// as subtype/length/scale (use -1 for empty/unknown values)
function GetFBTypeName(Index: Integer;
  SubType: integer = -1; FieldLength: integer = -1;
  Precision: integer = -1; Scale: integer = -1; CharacterSet: string = ''; CharacterLengt: integer = -1
): string;
// Tries to guess if an RDB$RELATION_FIELDS.RDB$FIELD_SOURCE domain name for a column is system-generated.
function IsFieldDomainSystemGenerated(FieldSource: string): boolean;

// Tries to guess if an index name is a system generated primary key index
function IsPrimaryIndexSystemGenerated(IndexName: string): boolean;

// Given TIBConnection parameters, sets transaction isolation level
procedure SetTransactionIsolation(Params: TStrings);


function AdjustColorByBrightness(AColor: TColor): TColor;    function ColorRValue(AColor: TColor): Byte;
function ColorGValue(AColor: TColor): Byte;
function ColorBValue(AColor: TColor): Byte;

implementation

uses Reg;

function MakeConnectionString(AServerName, APort, ADBFileName: string): string;
begin
  // Falls kein Port angegeben ist, nimm Standardport 3050
  if Trim(APort) = '' then
    APort := '3050';

  // Wenn Servername leer → lokale Verbindung
  if Trim(AServerName) = '' then
    Result := ADBFileName
  else
    Result := AServerName + '/' + APort + ':' + ADBFileName;
end;

function IsServerReachable(AServerName: string; out ErrorStr: string): Boolean;
var
  ServerRecord: TServerRecord;
  ServerSession: TServerSession;
begin
  Result := False;
  ErrorStr := '';

  try
    ServerRecord := GetServerRecordFromFileByName(AServerName);
    ServerSession := TServerSession.Create(
      AServerName, '', '', '', '', TCP, '', '', '', '', '', 0, 0, False, False, 0, 0, 0
    );

    ApplyServerRecordToSession(ServerRecord, ServerSession);

    if not ServerSession.Connected then
      if not ServerSession.IBXConnect then
      begin
        ErrorStr := ServerSession.ErrorStr;
        Exit;
      end;

    Result := True;

  finally
    ServerSession.Disconnect;
    ServerSession.Free;
  end;
end;

function GetDBFileNameFromConnectionString(AConnStr: string): string;
var
  p: Integer;
  s: string;
begin
  s := Trim(AConnStr);

  // Windows-Pfad hat "C:\", das darf NICHT getrennt werden.
  // Also: Wenn an Position 2 ein ':' steht, ist es ein lokaler Windows-Pfad.
  if (Length(s) >= 2) and (s[2] = ':') and
     ((s[1] in ['A'..'Z']) or (s[1] in ['a'..'z'])) then
  begin
    Exit(s);  // reiner Windows-Pfad
  end;

  // Embedded / absolute Linux path
  // z.B. "/opt/firebird/data/test.fdb"
  if (Length(s) > 0) and (s[1] = '/') then
  begin
    Exit(s);
  end;

  // Ansonsten: Firebird-typische ConnStrings
  // host[:port]:<filename>
  // Wir suchen die LETZTE ':' – danach kommt der Pfad
  p := LastDelimiter(':', s);

  if p > 0 then
    Result := Copy(s, p + 1, Length(s))
  else
    Result := s; // Falls kein ':' vorhanden ist (selten)
end;


function GetSeverImplemetationVersionFromIBDB(ADB: TIBDatabase): string;
begin
  result := ADB.FirebirdAPI.GetImplementationVersion;
end;

function GetSeverImplemetationVersionFromDBIndex(dbIndex: word): string;
begin
  result := GetSeverImplemetationVersionFromIBDB (RegisteredDatabases[dbIndex].IBDatabase);
end;

function GetServerMajorVersionFromDBIndex(dbIndex: Word): Word;
var
  VerStr: string;
  P: Integer;
begin
  VerStr := GetSeverImplemetationVersionFromDBIndex(dbIndex);
  P := Pos('.', VerStr);

  if P = 0 then
    Result := 0
  else
    Result := StrToIntDef(Copy(VerStr, 1, P - 1), 0);
end;

function GetServerMinorVersionFromDBIndex(dbIndex: Word): Word;
var
  VerStr: string;
  P: Integer;
begin
  VerStr := GetSeverImplemetationVersionFromDBIndex(dbIndex);
  P := Pos('.', VerStr);

  if P = 0 then
    Result := 0
  else
    Result := StrToIntDef(Copy(VerStr, P + 1, MaxInt), 0);
end;

function GetServerMajorVersionFromIBDB(ADB: TIBDatabase): Word;
var
  VerStr: string;
  P: Integer;
begin
  VerStr := GetSeverImplemetationVersionFromIBDB(ADB);
  P := Pos('.', VerStr);

  if P = 0 then
    Result := 0
  else
    Result := StrToIntDef(Copy(VerStr, 1, P - 1), 0);
end;

function GetServerMinorVersionFromIBDB(ADB: TIBDatabase): Word;
var
  VerStr: string;
  P: Integer;
begin
  VerStr := GetSeverImplemetationVersionFromIBDB(ADB);
  P := Pos('.', VerStr);

  if P = 0 then
    Result := 0
  else
    Result := StrToIntDef(Copy(VerStr, P + 1, MaxInt), 0);
end;

function TestEmbeddedConnection(AServerRec: TServerRecord; out ODSMajor: Integer; out ODSMinor: integer; out ServerVersion: string): boolean;
var tmpDB: TIBDatabase;
    tmpDbInfo: TIBDatabaseInfo;
    dbFile: string;
begin
  result := false;
  dbFile := ExtractFilePath(Application.ExeName) + TmpDir + '/embedded_test.fdb';

  tmpDB := TIBDatabase.Create(nil);
  try
    tmpDB.FirebirdLibraryPathName := AServerRec.ClientLibraryPath;
    tmpDB.Params.Values['User_Name'] := AServerRec.UserName;
    tmpDB.Params.Values['Password'] :=  AServerRec.Password;
    tmpDB.LoginPrompt := false;
    try
      if tmpDB.Connected then
        tmpDB.Connected := false;

      if FileExists(dbFile) then

        DeleteFile(PChar(dbFile));

      //tmpDB.CreateIfNotExists := true;
      tmpDB.DatabaseName :=  dbFile;
      tmpDB.CreateDatabase;
      tmpDB.Open;

      tmpDbInfo := TIBDatabaseInfo.Create(nil);
      tmpDbInfo.Database := tmpDB;

      ServerVersion := tmpDbInfo.FirebirdVersion;
      ODSMajor := tmpDbInfo.ODSMajorVersion;
      ODSMinor := tmpDbInfo.ODSMinorVersion;

      tmpDB.Close;
      //ReadODSFromFile(tmpDB.DatabaseName, ODSMajor, ODSMinor);
      result := true;
    except
      on E: Exception do
      begin
        ODSMajor := 0;
        ODSMinor := 0;
        ShowMessage('Embedded Connection failed:' + sLineBreak + E.Message);
      end;
    end;

  finally
    tmpDbInfo.Free;
    tmpDB.Free;
  end;
end;

function CloseDB(dbIndex: Integer): Boolean;
begin
  Result := False;

  // Sicherheits-Checks
  if (dbIndex < Low(RegisteredDatabases)) or (dbIndex > High(RegisteredDatabases)) then
    Exit;

  with RegisteredDatabases[dbIndex] do
  begin
    try
      // Query schließen
      if Assigned(IBQuery) and IBQuery.Active then
      begin
        try
          IBQuery.Close;
        except
          // silent
        end;
      end;

      // Transaktion: Commit → Rollback bei Fehler
      if Assigned(IBTransaction) and IBTransaction.InTransaction then
      begin
        try
          IBTransaction.Commit;
        except
          try
            IBTransaction.Rollback;
          except
            // silent – wenn Rollback auch fehlschlägt
          end;
        end;
      end;

      // Datenbankverbindung schließen
      if Assigned(IBDatabase) and IBDatabase.Connected then
      begin
        try
          IBDatabase.Connected := False;
        except
          // silent
        end;
      end;

      Result := True;
    except
      // silent: nichts anzeigen, kein raise
      Result := False;
    end;
  end;
end;

function DeleteDBRegistrationFromFile(const ATitle: string): Boolean;
var
  F: file of TRegisteredDatabase;
  Rec: TRegisteredDatabase;
  FileName: string;
  Found: Boolean;
  i: Integer;
begin
  Result := False;
  FileName := GetConfigurationDirectory + DatabasesRegFile;

  // Datei prüfen
  if not FileExists(FileName) then
    Exit;

  try
    AssignFile(F, FileName);
    FileMode := 2; // read/write
    Reset(F);

    Found := False;
    i := 0;

    // Durchsuchen aller Datensätze
    while not Eof(F) do
    begin
      Seek(F, i);
      Read(F, Rec);

      // Vergleich: gleiche Bezeichnung und nicht bereits gelöscht
      if (not Rec.Deleted) and SameText(Trim(Rec.Title), Trim(ATitle)) then
      begin
        Rec.Deleted := True;
        Seek(F, i);
        Write(F, Rec);
        Found := True;
        Break;
      end;

      Inc(i);
    end;

    Result := Found;
  finally
    try
      CloseFile(F);
    except
      // Fehler beim Schließen ignorieren oder loggen
    end;
  end;
end;

procedure UnregisterDatabaseByTitle(const ATitle: string);
var
  i, foundIndex: Integer;
begin
  foundIndex := -1;

  // 1. Array nach Title durchsuchen
  for i := 0 to High(RegisteredDatabases) do
  begin
    if SameText(RegisteredDatabases[i].RegRec.Title, ATitle) then
    begin
      foundIndex := i;
      Break;
    end;
  end;

  if foundIndex = -1 then Exit; // nicht gefunden

  // 2. Aus Datei löschen
  DeleteDBRegistrationFromFile(ATitle);

  // 3. Aus Array entfernen
  for i := foundIndex to High(RegisteredDatabases) - 1 do
    RegisteredDatabases[i] := RegisteredDatabases[i + 1];
  SetLength(RegisteredDatabases, Length(RegisteredDatabases) - 1);
end;


procedure MarkAllServerDatabasesDeleted(const ServerName: string);
var
  F: file of TRegisteredDatabase;
  Rec: TRegisteredDatabase;
  FileName: string;
  i: Integer;
begin
  FileName := GetConfigurationDirectory + DatabasesRegFile;

  // Datei vorhanden?
  if not FileExists(FileName) then
    Exit;

  try
    AssignFile(F, FileName);
    FileMode := 2; // read/write
    Reset(F);

    try
      for i := 0 to FileSize(F) - 1 do
      begin
        try
          Seek(F, i);
          Read(F, Rec);

          if (not Rec.Deleted) and SameText(Trim(Rec.ServerName), Trim(ServerName)) then
          begin
            Rec.Deleted := True;
            Seek(F, i);
            Write(F, Rec);
          end;
        except
          // silent – ignoriert fehlerhafte Datensätze
        end;
      end;
    finally
      try
        CloseFile(F);
      except
        // silent
      end;
    end;
  except
    // silent – kein raise, kein Dialog
  end;
end;

procedure MarkAllDatabasesDeleted;
var
  F: file of TRegisteredDatabase;
  Rec: TRegisteredDatabase;
  FileName: string;
  i: Integer;
begin
  FileName := GetConfigurationDirectory + DatabasesRegFile;

  if not FileExists(FileName) then
    Exit;

  try
    AssignFile(F, FileName);
    FileMode := 2; // read/write
    Reset(F);

    try
      for i := 0 to FileSize(F) - 1 do
      begin
        try
          Seek(F, i);
          Read(F, Rec);

          if not Rec.Deleted then
          begin
            Rec.Deleted := True;
            Seek(F, i);
            Write(F, Rec);
          end;
        except
          // silent – fehlerhafte Records überspringen
        end;
      end;
    finally
      try
        CloseFile(F);
      except
        // silent
      end;
    end;
  except
    // silent – keine Meldungen, keine Exceptions
  end;
end;

procedure RemoveDeletedDBRegistrationsFromFile;
var
  F, TempF: file of TRegisteredDatabase;
  Rec: TRegisteredDatabase;
  FileName, TempName: string;
begin
  FileName := GetConfigurationDirectory + DatabasesRegFile;
  TempName := GetConfigurationDirectory + 'turbobird.tmp';

  // Falls Datei nicht existiert → nichts tun
  if not FileExists(FileName) then
    Exit;

  try
    AssignFile(F, FileName);
    FileMode := 0; // read-only
    Reset(F);

    AssignFile(TempF, TempName);
    Rewrite(TempF);

    try
      while not Eof(F) do
      begin
        try
          Read(F, Rec);
          if not Rec.Deleted then
            Write(TempF, Rec);  // Nur gültige Datensätze behalten
        except
          // silent – überspringt fehlerhafte Einträge
        end;
      end;
    finally
      // Dateien schließen, egal was passiert
      try
        CloseFile(F);
      except
        // silent
      end;
      try
        CloseFile(TempF);
      except
        // silent
      end;
    end;

    // Alte Datei ersetzen
    try
      DeleteFile(PChar(FileName));
      RenameFile(TempName, PChar(FileName));
    except
      // silent – z. B. falls Datei gesperrt oder Berechtigung fehlt
    end;
  except
    // silent – keine Exceptions oder Dialoge
  end;
end;

procedure SetupTransaction(DBRec: TDatabaseRec; TRRec: TTransactionConfigRecord);
begin
  with DBRec do
  begin
{    // Transaktion erstellen, falls noch nil
    if SQLTrans = nil then
      SQLTrans := TIBTransaction.Create(nil);

    // Verbindung koppeln
    if IBConnection <> nil then
      IBConnection.DefaultTransaction := SQLTrans;

    // Isolation-Level
    case TRRec.TxIsolation of
      0: SQLTrans.Isolation := xiReadCommitted;
      1: SQLTrans.Isolation := xiReadCommittedRecVersion;
      2: SQLTrans.Isolation := xiRepeatableRead;
      3: SQLTrans.Isolation := xiSerializable;
    end;

    SQLTrans.ReadOnly := not TRRec.TxAccessMode;

    if TRRec.TxWaitMode then
      SQLTrans.Options := SQLTrans.Options + [toWait]
    else
      SQLTrans.Options := SQLTrans.Options - [toWait];
    }
  end;
end;


function IsValidPortNr(AStr: string): boolean;
var
  PortNum: Integer;
begin
  Result := False;
  if TryStrToInt(Trim(AStr), PortNum) then
    Result := (PortNum >= 1) and (PortNum <= 65535);
end;

function ServerNameContainPortNr(AServerName: string): boolean;
var
  SlashPos: Integer;
  PortStr: string;
begin
  Result := False;
  SlashPos := Pos('/', AServerName);
  if SlashPos > 0 then
  begin
    PortStr := Copy(AServerName, SlashPos+1, MaxInt);
    Result := IsValidPortNr(PortStr);
  end;
end;

function GetPortNrFromServerName(AServerName: string): string;
var
  SlashPos: Integer;
  PortStr: string;
begin
  Result := '';
  SlashPos := Pos('/', AServerName);
  if SlashPos > 0 then
  begin
    PortStr := Copy(AServerName, SlashPos+1, MaxInt);
    if IsValidPortNr(PortStr) then
      Result := Trim(PortStr);
  end;
end;

function GetHostFromServerName(AServerName: string): string;
var
  SlashPos: Integer;
begin
  SlashPos := Pos('/', AServerName);
  if SlashPos > 0 then
    Result := Trim(Copy(AServerName, 1, SlashPos-1))
  else
    Result := Trim(AServerName);
end;


(*  Get server name from database string  *)
Function GetServerName(DBName: string): string;
begin
  if Pos(':', DBName) > 2 then
    Result:= Copy(DBName, 1, Pos(':', DBName) - 1)
  else
    Result:= 'localhost';
end;

procedure AssignIBDatabase(const Source, Target: TIBDatabase);
begin
  if not Assigned(Source) or not Assigned(Target) then
    Exit;

  // Wichtig: erst schließen
  if Target.Connected then
    Target.Close;

  // Basis-Einstellungen
  Target.DatabaseName   := Source.DatabaseName;
  Target.FirebirdLibraryPathName := Source.FirebirdLibraryPathName;
  Target.Params.Assign(Source.Params);
  Target.LoginPrompt    := Source.LoginPrompt;
  Target.SQLDialect     := Source.SQLDialect;
  //Target.DefaultTransaction := Source.DefaultTransaction; // Achtung: nur Referenz, keine Kopie!

  // Provider-Flags und Misc
  Target.TraceFlags     := Source.TraceFlags;
  //Target.SQLRoleName    := Source.SQLRoleName;

  // In IBX gibt es "IdleTimer", "AllowStreamedConnected" usw. -> falls vorhanden:
  Target.IdleTimer      := Source.IdleTimer;
  Target.AllowStreamedConnected := Source.AllowStreamedConnected;

  // Events nicht übernehmen (da meist fenster-/threadabhängig)
  {Target.BeforeConnect  := Source.BeforeConnect;
  Target.AfterConnect   := Source.AfterConnect;
  Target.BeforeDisconnect := Source.BeforeDisconnect;
  Target.AfterDisconnect  := Source.AfterDisconnect;
  Target.OnLogin        := Source.OnLogin;}
end;

function AreSameDB(const DB1, DB2: TIBDatabase): Boolean;
begin
  Result := False;

  if (not Assigned(DB1)) or (not Assigned(DB2)) then
    Exit;

  // Direkt identisch? -> sofort true
  if DB1 = DB2 then
  begin
    Result := True;
    Exit;
  end;

  // Vergleich wichtiger Properties
  Result :=
    (DB1.DatabaseName      = DB2.DatabaseName) and
    (DB1.LoginPrompt       = DB2.LoginPrompt) and
    (DB1.SQLDialect        = DB2.SQLDialect) and
    (DB1.Params.Text       = DB2.Params.Text) and
    (DB1.FirebirdLibraryPathName = DB2.FirebirdLibraryPathName);

  // Optional: DefaultTransaction prüfen
  {if Result then
  begin
    if Assigned(DB1.DefaultTransaction) and Assigned(DB2.DefaultTransaction) then
      Result := DB1.DefaultTransaction.Params.Text = DB2.DefaultTransaction.Params.Text
    else if Assigned(DB1.DefaultTransaction) xor Assigned(DB2.DefaultTransaction) then
      Result := False;
  end;}
end;

function GetServerRecordFromFileByName(AServerName: string): TServerRecord;
var
  fs: TFileStream;
  rec: TServerRecord;
  found: Boolean;
  fileName: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  found := False;
  fileName := getConfigurationDirectory + ServersRegFile;

  if not FileExists(fileName) then
    Exit; // nichts vorhanden → leer zurück

  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    while fs.Position < fs.Size do
    begin
      fs.ReadBuffer(rec, SizeOf(TServerRecord));
      if SameText(Trim(rec.ServerName), Trim(AServerName)) then
      begin
        Result := rec;
        found := True;
        Break;
      end;
    end;
  finally
    fs.Free;
  end;

  if not found then
    FillChar(Result, SizeOf(Result), 0); // leer zurück wenn nicht gefunden
end;

function GetFirstServerRecordFromFile: TServerRecord;
var
  fs: TFileStream;
  rec: TServerRecord;
  fileName: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  fileName := getConfigurationDirectory + ServersRegFile;

  if not FileExists(fileName) then
    Exit; // keine Datei vorhanden → leer zurück

  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    if fs.Size >= SizeOf(TServerRecord) then
    begin
      fs.ReadBuffer(rec, SizeOf(TServerRecord));
      Result := rec;
    end
    else
      FillChar(Result, SizeOf(Result), 0); // Datei leer oder defekt
  finally
    fs.Free;
  end;
end;

function GetServerRecordFromFileByIndex(AIdx: Integer): TServerRecord;
var
  fs: TFileStream;
  rec: TServerRecord;
  fileName: string;
  offset: Int64;
begin
  FillChar(Result, SizeOf(Result), 0);
  fileName := getConfigurationDirectory + ServersRegFile;

  if (AIdx < 0) or (not FileExists(fileName)) then
    Exit; // ungültiger Index oder Datei nicht vorhanden

  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    // Prüfen, ob der Index in der Datei liegt
    if fs.Size < (AIdx + 1) * SizeOf(TServerRecord) then
      Exit; // Index außerhalb des gültigen Bereichs

    // Direkt an die Position springen
    offset := AIdx * SizeOf(TServerRecord);
    fs.Position := offset;

    // Record lesen
    fs.ReadBuffer(rec, SizeOf(TServerRecord));
    Result := rec;
  finally
    fs.Free;
  end;
end;

procedure SaveServerDataToFile(const Rec: TServerRecord);
var
  tmpList: TMemoryStream;
  oldRec: TServerRecord;
  found: Boolean;
  regFile: string;
begin
  regFile := GetConfigurationDirectory + ServersRegFile;

  tmpList := TMemoryStream.Create;
  try
    if FileExists(regFile) then
      tmpList.LoadFromFile(regFile);

    tmpList.Position := 0;
    found := False;

    // prüfen ob schon ein Datensatz für diesen Server existiert
    while tmpList.Position < tmpList.Size do
    begin
      tmpList.ReadBuffer(oldRec, SizeOf(oldRec));
      if Trim(oldRec.ServerName) = Trim(Rec.ServerName) then
      begin
        // überschreiben
        tmpList.Position := tmpList.Position - SizeOf(oldRec);
        tmpList.WriteBuffer(Rec, SizeOf(Rec));
        found := True;
        Break;
      end;
    end;

    // falls nicht gefunden → neuen Datensatz anhängen
    if not found then
    begin
      tmpList.Position := tmpList.Size;
      tmpList.WriteBuffer(Rec, SizeOf(Rec));
    end;

    tmpList.SaveToFile(regFile);
  finally
    tmpList.Free;
  end;
end;

procedure ApplyServerRecordToSession(const Rec: TServerRecord; Session: TServerSession);
begin
  // nur wenn Record gültig (ServerName nicht leer)
  if Trim(Rec.ServerName) = '' then
    Exit;

  //Session.ServerName          := Rec.ServerName;
  Session.ServerAlias         := Rec.ServerAlias;

  {Session.FBVersionString     := Rec.VersionString;
  Session.FBVersionMajor      := Rec.VersionMajor;
  Session.FBVersionMinor      := Rec.VersionMinor;}

  Session.UserName            := Rec.UserName;
  Session.Password            := Rec.Password;
  Session.Role                := Rec.Role;
  Session.Protocol            := Rec.Protocol;
  Session.Charset             := Rec.Charset;
  Session.Port                := Rec.Port;
  Session.RootPath            := Rec.RootPath;
  Session.ClientLibraryPath   := Rec.ClientLibraryPath;
  Session.ConfigFilePath      := Rec.ConfigFilePath;
  Session.LoadRegisteredClientLib := Rec.LoadRegisteredClientLib;
  Session.IsEmbedded              := Rec.IsEmbedded;

  Session.ConnectTimeout    := Rec.ConnectTimeoutMS;
  Session.RetryConnectCount := Rec.RetryCount;
  Session.QueryTimeout      := Rec.QueryTimeoutMS;
end;

function BuildServerRecordFromSession(const Session: TServerSession; SavePwd: Boolean): TServerRecord;
var Rec: TServerRecord;
begin
  FillChar(Rec, SizeOf(Rec), 0);

  Rec.ServerName   := Session.ServerName;
  Rec.ServerAlias  := Session.ServerAlias;

  Rec.VersionString := Session.FBVersionString;
  Rec.VersionMajor  := Session.FBVersionMajor;
  Rec.VersionMinor  := Session.FBVersionMinor;

  Rec.UserName     := Session.UserName;
  if SavePwd then
    Rec.Password := Session.Password
  else
   Rec.Password := '';
  Rec.Role         := Session.Role;
  Rec.Protocol     := Session.Protocol;
  Rec.Charset := Session.Charset;
  Rec.Port :=     Session.Port;
  Rec.IsEmbedded := (Rec.Protocol = local);

  Rec.RootPath := Session.RootPath;
  Rec.ClientLibraryPath   := Session.ClientLibraryPath;
  Rec.ConfigFilePath      := Session.ConfigFilePath;
  Rec.LoadRegisteredClientLib := Session.LoadRegisteredClientLib;
  Rec.IsEmbedded              := Session.IsEmbedded;

  Rec.ConnectTimeoutMS := Session.ConnectTimeout;
  Rec.RetryCount       := Session.RetryConnectCount;
  Rec.QueryTimeoutMS   := Session.QueryTimeout;

  Result := Rec;
end;

function GetServerNodeByServerName(const AServerName: string): TTreeNode;
var
  Node: TTreeNode;
begin
  Result := nil;

  Node := MainTreeView.Items.GetFirstNode;

  while Node <> nil do
  begin
    if SameText(Node.Text, AServerName) then
    begin
      Result := Node;
      Exit;
    end;

    Node := Node.GetNextSibling;  // nur Level 0 durchsuchen
  end;
end;

function GetServerListFromTreeView: TStringList;
var
  i: Integer;
  Node: TTreeNode;
begin
  result := nil;
  if MainTreeView.Items.Count = 0 then
    exit;
  Result := TStringList.Create;
  for i := 0 to MainTreeView.Items.Count - 1 do
  begin
    Node := MainTreeView.Items[i];
    if (Node.Level = 0) and (Trim(Node.Text) <> '') then
      Result.Add(Node.Text);
  end;
end;

function GetServerAndPortListFromTreeView: TStringList;
var
  i: Integer;
  Node: TTreeNode;
  Port: string;
  hstr: string;
begin
  result := nil;
  if MainTreeView.Items.Count = 0 then
    exit;
  Result := TStringList.Create;
  for i := 0 to MainTreeView.Items.Count - 1 do
  begin
    Node := MainTreeView.Items[i];
    if (Node.Level = 0) and (Trim(Node.Text) <> '') then
    begin
      hStr := Node.Text;
      if not TPNodeInfos(Node.Data)^.ServerSession.IsEmbedded   then
        hStr := hStr + '/' + TPNodeInfos(Node.Data)^.ServerSession.Port;
      Result.Add(hStr);
    end;
  end;
end;

function GetAncestorAtLevel(ANode: TTreeNode; ALevel: Integer): TTreeNode;
begin
  Result := nil;

  // Kein Knoten übergeben -> raus
  if ANode = nil then
    Exit;

  // Ungültiges Level -> raus
  if ALevel < 0 then
    Exit;

  // Wenn das gewünschte Level höher ist als der aktuelle Node.Level
  // -> kann niemals erreicht werden
  if ALevel > ANode.Level then
    Exit;

  // Jetzt hochlaufen, bis Level erreicht oder kein Parent mehr
  while (ANode.Level > ALevel) and (ANode.Parent <> nil) do
    ANode := ANode.Parent;

  // Nur zurückgeben, wenn wirklich erreicht
  if ANode.Level = ALevel then
    Result := ANode;
end;

function GetAncestorNodeText(ANode: TTreeNode; ALevel: Integer): string;
var
  Ancestor: TTreeNode;
begin
  Ancestor := GetAncestorAtLevel(ANode, ALevel);
  if Assigned(Ancestor) then
    Result := Trim(Ancestor.Text)
  else
    Result := '';
end;

function GetServerNameFromConnString(ADBIndex: word): string;
var DBName: string;
begin
  DBName := RegisteredDatabases[ADBIndex].RegRec.DatabaseName;
  if Pos(':', DBName) > 2 then
    Result:= Copy(DBName, 1, Pos(':', DBName) - 1)
  else
    Result:= 'localhost';
end;

function GetDBNameFromConnString(ADBIndex: Word): string;
var
  DBName: string;
  P: Integer;
begin
  DBName := RegisteredDatabases[ADBIndex].RegRec.DatabaseName;

  P := Pos(':', DBName);
  if P > 2 then
    // alles nach dem Doppelpunkt
    Result := Copy(DBName, P + 1, Length(DBName) - P)
  else
    // kein Server angegeben → kompletter String ist DB-Pfad
    Result := DBName;
end;

function GetConfigurationDirectory: string;
var ConfigDir: string;
begin
  ConfigDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName) + 'data' + PathDelim + 'config');
  if not DirectoryExists(ConfigDir) then
      CreateDir(ConfigDir);
  Result:= ConfigDir;
end;


procedure ReadIniFile;
begin
  fLanguage  := fIniFile.ReadString('UserInterface',  'Language', 'en');
  InitialFBClientLibPath := fIniFile.ReadString('FireBird',  'InitialFBClientLib', '');

  //Backup
  CloseDBBeforeBackup :=  fIniFile.ReadBool('Backup',  'CloseDBBeforeBackup', true);

  //Restore
  DefaultPageSize        := fIniFile.ReadInteger('Restore', 'DefaultPageSize', 8192);
  DefaultNumBuffers      := fIniFile.ReadInteger('Restore', 'DefaultNumBuffers', 2048);
  RegisterDBAfterRestore := fIniFile.ReadBool('Restore',  'RegisterDBAfterRestore', true);

  //Scripter
  EchoInput := fIniFile.ReadBool('Scripter',  'EchoInput', true);
  StopOnFirstError := fIniFile.ReadBool('Scripter',  'StopOnFirstError', false);
  AutoDDL := fIniFile.ReadBool('Scripter',  'AutoDDL', true);
  IgnoreCreateDatabase := fIniFile.ReadBool('Scripter',  'IgnoreCreateDatabase', false);
  IgnoreGrants := fIniFile.ReadBool('Scripter',  'IgnoreGrants', false);
  ShowAffectedRows := fIniFile.ReadBool('Scripter',  'ShowAffectedRows', true);
  ShowPerformanceStats := fIniFile.ReadBool('Scripter',  'ShowPerformanceStats', true);

  //ClipboardExport
  MaxExportRows := fIniFile.ReadInteger('ClipboardExport',  'MaxExportRows', 200);

end;

procedure WriteIniFile;
begin
  fIniFile.WriteString('UserInterface', 'Language', fLanguage);

  //Backup
  fIniFile.WriteBool('Backup',  'CloseDBBeforeBackup', CloseDBBeforeBackup);

  //Restore
  fIniFile.WriteInteger('Restore', 'DefaultPageSize', DefaultPageSize);
  fIniFile.WriteInteger('Restore', 'DefaultNumBuffers', DefaultNumBuffers);
  fIniFile.WriteBool('Restore',  'RegisterDBAfterRestore', RegisterDBAfterRestore);

  //Scripter
  fIniFile.WriteBool('Scripter',  'EchoInput', EchoInput);
  fIniFile.WriteBool('Scripter',  'StopOnFirstError', StopOnFirstError);
  fIniFile.WriteBool('Scripter',  'AutoDDL', AutoDDL);
  fIniFile.WriteBool('Scripter',  'IgnoreCreateDatabase', IgnoreCreateDatabase);
  fIniFile.WriteBool('Scripter',  'IgnoreGrants', IgnoreGrants);
  fIniFile.WriteBool('Scripter',  'ShowAffectedRows', ShowAffectedRows);
  fIniFile.WriteBool('Scripter',  'ShowPerformanceStats', ShowPerformanceStats);

  //ClipboardExport
  fIniFile.WriteInteger('ClipboardExport',  'MaxExportRows', MaxExportRows);
end;


function FirstRun: boolean;
var DataDirectory: string;
begin
  {$IFDEF WINDOWS}
  DataDirectory := ExtractFilePath(Application.ExeName) + '\data';
  {$ELSE}
  DataDirectory := ExtractFilePath(Application.ExeName) + '/data';
 {$ENDIF}
  result := not DirectoryExists(DataDirectory);
end;

procedure ExtractResources;
var
  DataStream: TResourceStream;
  MemoryStream: TMemoryStream;
  AbUnZipper: TAbUnZipper;
begin
  AbUnZipper := TAbUnZipper.Create(nil);
  if FirstRun then
  begin
    DataStream := TResourceStream.Create(HInstance, 'DATA', RT_RCDATA);
    MemoryStream := TMemoryStream.Create;
    try
      // Load the ZIP file from the resource into memory
      MemoryStream.LoadFromStream(DataStream);
      MemoryStream.Position := 0;

      // Extract the contents of the ZIP file to the target directory.
      AbUnZipper.Stream := MemoryStream;
      AbUnZipper.ExtractOptions := [eoCreateDirs, eoRestorePath]; // Create directories
      AbUnZipper.BaseDirectory := ExtractFilePath(Application.ExeName);
      AbUnZipper.ExtractFiles('*.*');
    finally
      MemoryStream.Free;
      DataStream.Free;
      AbUnZipper.Free;
    end;
  end;
end;


function ExtractVersionFromName(const Name: string): string;
var
  Parts: TStringArray;
  CleanName: string;
begin
  Result := '';
  CleanName := Name;

  // Entferne .zip oder .exe.zip oder .exe
  if EndsText('.exe.zip', CleanName) then
    Delete(CleanName, Length(CleanName) - 7 + 1, 8)
  else if EndsText('.zip', CleanName) then
    Delete(CleanName, Length(CleanName) - 3 + 1, 4)
  else if EndsText('.exe', CleanName) then
    Delete(CleanName, Length(CleanName) - 3 + 1, 4);

  Parts := SplitString(CleanName, '-v');
  if Length(Parts) <> 2 then Exit;

  Result := Parts[1];

  // Entferne ggf. einen Punkt am Ende
  if EndsText('.', Result) then
    Delete(Result, Length(Result), 1);

  // Entferne ggf. noch .exe, falls es doch drin blieb
  Result := StringReplace(Result, '.exe', '', [rfIgnoreCase]);
end;

function GetProgramVersion: string;
var
  vr: TVersionResource;
  rs: TResourceStream;
begin
  Result := 'unknow';
  vr := TVersionResource.Create;
  try
    rs := TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      vr.SetCustomRawDataStream(rs);
      Result := Format('%d.%d.%d.%d', [vr.FixedInfo.FileVersion[0],
                                       vr.FixedInfo.FileVersion[1],
                                       vr.FixedInfo.FileVersion[2],
                                       vr.FixedInfo.FileVersion[3]]);
    finally
      rs.Free;
    end;
  finally
    vr.Free;
  end;
end;

function GetLazarusVersion: string;
begin
  result := lcl_version;
end;

function GetFPCVersion: string;
begin
  Result := {$I %FPCVERSION%};
end;

function ReadFileToString(const AFileName: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File "%s" not found!', [AFileName]);

  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(FileStream, FileStream.Size);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function ReadOSReleaseFile: string;
var
  OSReleaseFile: TextFile;
  Line, OSVersion: string;
begin
  OSVersion := 'Unknown Linux Version';
  if FileExists('/etc/os-release') then
  begin
    AssignFile(OSReleaseFile, '/etc/os-release');
    try
      Reset(OSReleaseFile);
      while not EOF(OSReleaseFile) do
      begin
        ReadLn(OSReleaseFile, Line);
        if Pos('PRETTY_NAME=', Line) = 1 then
        begin
          OSVersion := Copy(Line, Pos('=', Line) + 1, Length(Line));
          OSVersion := StringReplace(OSVersion, '"', '', [rfReplaceAll]);
          Break;
        end;
      end;
    finally
      CloseFile(OSReleaseFile);
    end;
  end
  else if FileExists('/proc/version') then
  begin
    // Fallback: read from /proc/version, if /etc/os-release don't exists
    OSVersion := Trim(StringReplace(ReadFileToString('/proc/version'), 'Linux version ', '', []));
  end;
  Result := OSVersion;
end;

function GetOSInfo: string;
var OSName, OSType, OSVersion: string; Buffer: array[0..255] of Char;
begin
  // Betriebssystem bestimmen
  {$IFDEF WINDOWS}
  OSName := 'Windows';
  if GetEnvironmentVariable('OS', Buffer, SizeOf(Buffer)) > 0 then
    OSVersion := Buffer
  else
    OSVersion := 'unknow';
  {$ENDIF}

  {$IFDEF LINUX}
  OSName := 'Linux';
  OSVersion := ReadOSReleaseFile;
  {$ENDIF}

  {$IFDEF DARWIN}
  OSName := 'Mac OS';
  OSVersion := GetEnvironmentVariable('OSTYPE'); // Alternativ mit `uname -r` die Mac-Version auslesen
  {$ENDIF}

  // 32-Bit or 64-Bit
  {$IFDEF CPU32}
  OSType := '32-bit';
  {$ENDIF}
  {$IFDEF CPU64}
  OSType := '64-bit';
  {$ENDIF}

  //Result := Format('OS: %s' + sLineBreak +
  //                 '%s' + sLineBreak +
  //                 'Architecture: %s', [OSName, OSVersion, OSType]);

  Result := Format('%s' + sLineBreak +
                   '%s' + sLineBreak +
                   'Architecture: %s', [OSName, OSVersion, OSType]);
end;

function GetLCLWidgetSet: string;
begin
  {$IFDEF LCLQT}
  Result := 'Qt4';
  {$ENDIF}

  {$IFDEF LCLQT5}
  Result := 'Qt5';
  {$ENDIF}

  {$IFDEF LCLQT6}
  Result := 'Qt6';
  {$ENDIF}

  {$IFDEF LCLGTK2}
  Result := 'GTK2';
  {$ENDIF}

  {$IFDEF LCLGTK3}
  Result := 'GTK3';
  {$ENDIF}

  {$IFDEF LCLCOCOA}
  Result := 'Cocoa (Mac OS)';
  {$ENDIF}

  {$IFDEF LCLCARBON}
  Result := 'Carbon (Mac OS)';
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Result := 'Windows';
  {$ENDIF}

  if Result = '' then
    Result := 'unknow Widgetset';
end;

function GetProgramBuildDate: string;
begin
  result := CompileDate;
end;

function GetProgramBuildTime: string;
begin
  result := CompileTime;
end;

function StripQuotes(const S: string): string;
begin
  if (Length(S) >= 2) and (S[1] = '''') and (S[Length(S)] = '''') then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

function ExtractDefaultValue(const DefaultSource: string): string;
var
  Src: string;
begin
  Src := Trim(DefaultSource);
  if AnsiStartsText('DEFAULT', Src) then
  begin
    // Entferne das 'DEFAULT'-Keyword (case-insensitive)
    Result := Trim(Copy(Src, 8, Length(Src)));
  end
  else
    Result := Src;
end;

function IsValidUUIDHex(const S: string): Boolean;
var
  i: Integer;
begin
  Result := (Length(S) = 35) and
            (Copy(S, 1, 2) = 'x''') and
            (Copy(S, 35, 1) = '''');
  if Result then
    for i := 3 to 34 do
      if not (UpCase(S[i]) in ['0'..'9', 'A'..'F']) then
        Exit(False);
end;

function CreateUUIDHexLiteral: string;
var
  GUID: TGUID;
  UUIDBytes: array[0..15] of Byte;
  i: Integer;
begin
  CreateGUID(GUID);
  Move(GUID.D1, UUIDBytes[0], 16);  // GUID in Byte-Array kopieren

  Result := 'x''';
  for i := 0 to 15 do
    Result := Result + IntToHex(UUIDBytes[i], 2);
  Result := Result + '''';
end;

function IsSizedTypeName(const ATypeName: string): Boolean;
var
  S: string;
  LParen, RParen, I, CommaPos: Integer;
  Params, CleanParams, P, Sc: string;
  DummyInt1, DummyInt2: Integer;
begin
  S := Trim(ATypeName);
  LParen := Pos('(', S);
  RParen := Pos(')', S);
  Result := False;

  if (LParen > 0) and (RParen > LParen) then
  begin
    Params := Copy(S, LParen + 1, RParen - LParen - 1);

    // Alle Leerzeichen entfernen
    CleanParams := '';
    for I := 1 to Length(Params) do
      if not (Params[I] in [' ', #9, #13, #10]) then
        CleanParams := CleanParams + Params[I];

    CommaPos := Pos(',', CleanParams);

    if CommaPos = 0 then
    begin
      Result := TryStrToInt(CleanParams, DummyInt1);
    end
    else if CommaPos > 1 then
    begin
      P := Copy(CleanParams, 1, CommaPos - 1);
      Sc := Copy(CleanParams, CommaPos + 1, Length(CleanParams) - CommaPos);
      Result := TryStrToInt(P, DummyInt1) and TryStrToInt(Sc, DummyInt2);
    end;
  end;
end;

function GetNameFromSizedTypeName(const ATypeName: string): string;
var
  LParen: Integer;
begin
  Result := Trim(ATypeName);
  LParen := Pos('(', Result);
  if LParen > 0 then
    Result := Trim(Copy(Result, 1, LParen - 1));
end;

function GetSizeFromSizedTypeName(const ATypeName: string): string;
var
  Precision, Scale: string;
begin
  GetPrecisionAndScaleFromSizedTypeName(ATypeName, Precision, Scale);
  Result := Precision;
end;

procedure GetPrecisionAndScaleFromSizedTypeName(const ATypeName: string;
  out Precision, Scale: string);
var
  S: string;
  LParen, RParen, CommaPos: Integer;
  Params: string;
begin
  Precision := '';
  Scale := '';
  S := Trim(ATypeName);
  LParen := Pos('(', S);
  RParen := Pos(')', S);
  if (LParen > 0) and (RParen > LParen) then
  begin
    Params := Trim(Copy(S, LParen + 1, RParen - LParen - 1));
    CommaPos := Pos(',', Params);
    if CommaPos = 0 then
    begin
      Precision := Trim(Params);
      Scale := '0';
    end
    else
    begin
      Precision := Trim(Copy(Params, 1, CommaPos - 1));
      Scale := Trim(Copy(Params, CommaPos + 1, MaxInt));
    end;
  end;
end;


function GetRootObjectTypeFor(AObjectType: TTreeViewObjectType): TTreeViewObjectType;
begin
  case AObjectType of
    tvotFunction:            Result := tvotFunctionRoot;
    tvotStoredProcedure:     Result := tvotStoredProcedureRoot;
    tvotUDFFunction:         Result := tvotUDFRoot;
    tvotUDRFunction:         Result := tvotUDRFunctionRoot;
    tvotUDRProcedure:        Result := tvotUDRProcedureRoot;
    tvotPackageFunction:     Result := tvotPackageFunctionRoot;
    tvotPackageProcedure:    Result := tvotPackageProcedureRoot;
    tvotPackageUDRFunction:  Result := tvotPackageUDRFunctionRoot;
    tvotPackageUDRProcedure: Result := tvotPackageUDRProcedureRoot;
    tvotTable:               Result := tvotTableRoot;
    tvotView:                Result := tvotViewRoot;
    tvotSystemTable:         Result := tvotSystemTableRoot;
  else
    Result := tvotNone;
  end;
end;

function FBObjectToStr(AType: TObjectType): string;
begin
  case AType of
    otNone:                  Result := 'None';
    otTables:                Result := 'Table';
    otGenerators:            Result := 'Generator';
    otTriggers:              Result := 'Trigger';
    otViews:                 Result := 'View';
    otStoredProcedures:      Result := 'Procedure';
    otUDF:                   Result := 'User-Defined Function';
    otSystemTables:          Result := 'System Table';
    otDomains:               Result := 'Domain';
    otRoles:                 Result := 'Role';
    otExceptions:            Result := 'Exception';
    otUsers:                 Result := 'User';
    otIndexes:               Result := 'Indexe';
    otConstraints:           Result := 'Constraint';
    otFBFunctions:           Result := 'Function';
    otFBProcedures:          Result := 'Procedure';
    otUDRFunctions:          Result := 'UDR Function';
    otUDRProcedures:         Result := 'UDR Procedure';
    otPackages:              Result := 'Package';
    otPackageFunctions:      Result := 'Package Function';
    otPackageProcedures:     Result := 'Package Procedure';
    otPackageUDFFunctions:   Result := 'Package UDF Function';
    otPackageUDRFunctions:   Result := 'Package UDR Function';
    otPackageUDRProcedures:  Result := 'Package UDR Procedure';
  else
    Result := 'Unknown';
  end;
end;

function TreeViewObjectToStr(AType: TTreeViewObjectType): string;
begin
  case AType of
    tvotNone:                  Result := 'None';
    tvotServer:                Result := 'Server';
    tvotDB:                    Result := 'Database';
    tvotQueryWindow:           Result := 'Query Window';

    tvotTableRoot:             Result := 'Tables';
    tvotTable:                 Result := 'Table';
    tvotTableField:            Result := 'Table Field';

    tvotGeneratorRoot:         Result := 'Generators';
    tvotGenerator:             Result := 'Generator';

    tvotTriggerRoot:           Result := 'Triggers';
    tvotTrigger:               Result := 'Trigger';

    tvotViewRoot:              Result := 'Views';
    tvotView:                  Result := 'View';

    tvotSystemTableRoot:       Result := 'System Tables';
    tvotSystemTable:           Result := 'System Table';

    tvotDomainRoot:            Result := 'Domains';
    tvotDomain:                Result := 'Domain';

    tvotRoleRoot:              Result := 'Roles';
    tvotRole:                  Result := 'Role';

    tvotExceptionRoot:         Result := 'Exceptions';
    tvotException:             Result := 'Exception';

    tvotUserRoot:              Result := 'Users';
    tvotUser:                  Result := 'User';

    tvotIndexRoot:             Result := 'Indexes';
    tvotIndex:                 Result := 'Index';

    tvotConstraintRoot:        Result := 'Constraints';
    tvotConstraint:            Result := 'Constraint';

    tvotStoredProcedureRoot:   Result := 'Stored Procedures';
    tvotStoredProcedure:       Result := 'Stored Procedure';

    tvotFunctionRoot:          Result := 'Functions';
    tvotFunction:              Result := 'Function';

    tvotUDRoot:                Result := 'UD Objects';
    tvotUDFRoot:               Result := 'UDFs';
    tvotUDFFunction:           Result := 'UDF Function';

    tvotUDRRoot:               Result := 'UDRs';
    tvotUDRFunctionRoot:       Result := 'UDR Functions';
    tvotUDRFunction:           Result := 'UDR Function';
    tvotUDRProcedureRoot:      Result := 'UDR Procedures';
    tvotUDRProcedure:          Result := 'UDR Procedure';

    tvotPackageRoot:           Result := 'Packages';
    tvotPackage:               Result := 'Package';
    tvotPackageFunctionRoot:   Result := 'Package Functions';
    tvotPackageFunction:       Result := 'Package Function';
    tvotPackageProcedureRoot:  Result := 'Package Procedures';
    tvotPackageProcedure:      Result := 'Package Procedure';
    tvotPackageUDRFunctionRoot:Result := 'Package UDR Functions';
    tvotPackageUDRFunction:    Result := 'Package UDR Function';
    tvotPackageUDRProcedureRoot:Result := 'Package UDR Procedures';
    tvotPackageUDRProcedure:   Result := 'Package UDR Procedure';

  else
    Result := 'Unknown';
  end;
end;


function RoutineTypeToTreeViewObjectType(RT: TRoutineType): TTreeViewObjectType;
begin
  case RT of
    rtUDF:              Result := tvotUDFFunction;
    rtFBFunc:           Result := tvotFunction;
    rtFBProc:           Result := tvotStoredProcedure;
    rtUDRFunc:          Result := tvotUDRFunction;
    rtUDRProc:          Result := tvotUDRProcedure;
    rtPackageFBFunc:    Result := tvotPackageFunction;
    rtPackageFBProc:    Result := tvotPackageProcedure;
    rtPackageUDRFunc:   Result := tvotPackageUDRFunction;
    rtPackageUDRProc:   Result := tvotPackageUDRProcedure;
    else                Result := tvotNone; // fallback
  end;
end;

function ExtractObjectName(const Input: string): string;
var
  PosOpenParen: Integer;
begin
  PosOpenParen := Pos('(', Input);
  if PosOpenParen > 1 then
    Result := Copy(Input, 1, PosOpenParen - 1)
  else
    Result := Input;
end;

function GetClearNodeText(const ANodeText: string): string;
var
  s: string;
  pParen, pBracket, pSpace, pMin: Integer;
begin
  s := Trim(ANodeText);

  // Erste Position von ( oder [ oder Leerzeichen suchen
  pParen  := Pos('(', s);
  pBracket:= Pos('[', s);
  pSpace  := Pos(' ', s);

  // kleinstes >0 nehmen
  pMin := 0;
  if (pParen > 0) and ((pMin = 0) or (pParen < pMin)) then pMin := pParen;
  if (pBracket > 0) and ((pMin = 0) or (pBracket < pMin)) then pMin := pBracket;
  if (pSpace > 0) and ((pMin = 0) or (pSpace < pMin)) then pMin := pSpace;

  if pMin > 0 then
    Result := Trim(Copy(s, 1, pMin - 1))
  else
    Result := s;
end;

function FormatNodeCaptionnWithCount(const Text: string; Count: Integer): string;
var
  BaseText: string;
  StartPos, EndPos, ExistingCount, Total: Integer;
begin
  if Count < 0 then
  begin
    ShowMessage('Warning: Negative number ignored: ' + IntToStr(Count));
    Result := Text;
    Exit;
  end;

  StartPos := Pos('(', Text);
  EndPos := Pos(')', Text);

  if (StartPos > 0) and (EndPos > StartPos) then
  begin
    BaseText := Copy(Text, 1, StartPos - 1);
    ExistingCount := StrToIntDef(Copy(Text, StartPos + 1, EndPos - StartPos - 1), 0);
    Total := ExistingCount + Count;

    if Total > 0 then
      Result := BaseText + '(' + IntToStr(Total) + ')'
    else
      Result := BaseText;
  end
  else
  begin
    if Count > 0 then
      Result := Text + '(' + IntToStr(Count) + ')'
    else
      Result := Text;
  end;
end;

function GetCollations(const Characterset: string; var Collations: TStringList): boolean;
var
  i: integer;
begin
  result:= false;
  Collations.Clear;
  Collations.BeginUpdate;
  for i:= low(FBCollations) to high(FBCollations) do
  begin
    if FBCollations[i,1]=Characterset then
    begin
      Collations.Add(FBCollations[i,0]);
    end;
  end;
  Collations.EndUpdate;
  result:= true;
end;

procedure SetTransactionIsolation(Params: TStrings);
begin
  Params.Clear;
  // typische Firebird-Transaktionseinstellungen:
  Params.Add('read_committed');
  Params.Add('rec_version');
  Params.Add('nowait');
end;

procedure GetFieldType(FieldQuery: TIBQuery; var FieldType: string; var FieldSize: integer);
// Requires FieldQuery to be the correct field retrieval query.
// todo: migrate field retrieval query to systables if not already done
begin
  FieldType:= '';
  FieldSize:= 0;

  if (FieldQuery.FieldByName('field_source').IsNull) or
    (trim(FieldQuery.FieldByName('field_source').AsString)='') or
    (IsFieldDomainSystemGenerated(trim(FieldQuery.FieldByname('field_source').AsString))) then
  begin
    // Field type is not based on a domain but a standard SQL type
    FieldType:= GetFBTypeName(FieldQuery.FieldByName('field_type_int').AsInteger,
      FieldQuery.FieldByName('field_sub_type').AsInteger,
      FieldQuery.FieldByName('field_length').AsInteger,
      FieldQuery.FieldByName('field_precision').AsInteger,
      FieldQuery.FieldByName('field_scale').AsInteger,
      FieldQuery.FieldByName('field_charset').Asstring,
      FieldQuery.FieldByName('characterlength').AsInteger);
    // Array should really be [lowerbound:upperbound] (if dimension is 0)
    // but for now don't bother as arrays are not supported anyway
    // Assume 0 dimension, 1 lower bound; just fill in upper bound
    if not(FieldQuery.FieldByName('array_upper_bound').IsNull) then
      FieldType := FieldType +
        ' [' +
        FieldQuery.FieldByName('array_upper_bound').AsString +
        ']';
    if FieldQuery.FieldByName('field_type_int').AsInteger = VarCharType then
      FieldSize:= FieldQuery.FieldByName('characterlength').AsInteger
    else
      FieldSize:= FieldQuery.FieldByName('field_length').AsInteger;
  end
  else
  begin
    // Field is based on a domain
    FieldType:= trim(FieldQuery.FieldByName('field_source').AsString);
  end;
end;

(**************  Get Firebird Type name  *****************)
function GetFBTypeName(Index: Integer;
  SubType: integer = -1; FieldLength: integer = -1;
  Precision: integer = -1; Scale: integer = -1;
  CharacterSet: string = ''; CharacterLengt: integer = -1
): string;
begin
  case Index of
    7  : Result := 'SMALLINT';   // SHORT
    8  : Result := 'INTEGER';    // LONG
    9  : Result := 'QUAD';       // alt, historisch
    10 : Result := 'FLOAT';
    11 : Result := 'D_FLOAT';    // deprecated
    12 : Result := 'DATE';
    13 : Result := 'TIME';
    14 : Result := 'CHAR';       // Hier Länge in Klammern anhängen
    16 : Result := 'BIGINT';     // INT64 oder DECIMAL/NUMERIC abhängig von SubType
    23 : Result := 'BOOLEAN';    // Firebird 3+
    27 : Result := 'DOUBLE PRECISION';
    35 : Result := 'TIMESTAMP';
    37 : Result := 'VARCHAR';    // VarCharType
    40 : Result := 'CSTRING';    // nur für UDFs
    45 : Result := 'BLOB_ID';    // interner Blob-Identifier
    261: Result := 'BLOB';       // BlobType

    // Firebird 4.0 / 5.0 neue Typen
    24: Result := 'DECFLOAT(16)';
    25: Result := 'DECFLOAT(34)';
    26: Result := 'INT128';
    28: Result := 'TIME WITH TIME ZONE';
    29: Result := 'TIMESTAMP WITH TIME ZONE';

  else
    Result := 'UNKNOWN_TYPE. CODE = ' + IntToStr(Index);
  end;

  // Längenangabe bei CHAR und VARCHAR
  if Index in [14, 37] then
  begin
    if FieldLength > 0 then
      //Result := Result + '(' + IntToStr(FieldLength) + ')';
      Result := Result + '(' + IntToStr(CharacterLengt) + ')';
  end;

  // Numerische Typen mit SubType (NUMERIC/DECIMAL)
  if Index in [7, 8, 16] then
  begin
    if SubType = 0 then
    begin
      // Normale Integer-Typen
      case Index of
        7: Result := 'SMALLINT';
        8: Result := 'INTEGER';
        16: Result := 'BIGINT';
      end;
    end
    else
    begin
      // NUMERIC oder DECIMAL mit Präzision und Scale
      if SubType = 1 then
        Result := 'NUMERIC('
      else if SubType = 2 then
        Result := 'DECIMAL('
      else
        Result := 'UNKNOWN_NUMERIC(';

      if Precision < 0 then
        Precision := 2; // Default-Wert

      Result := Result + IntToStr(Precision) + ',' + IntToStr(Abs(Scale)) + ')';
    end;
  end;

  if (Index = 14) and (CharacterLengt = 63) and (UpperCase(Trim(CharacterSet)) = 'OCTETS') then
      Result := 'UUID';
end;
//end-newlib

function IsFieldDomainSystemGenerated(FieldSource: string): boolean;
begin
  // Unfortunately there does not seem to be a way to search the system tables to find out
  // if the constraint name is system-generated
  result:= (pos('RDB$',uppercase(Trim(FieldSource)))=1);
end;

function IsPrimaryIndexSystemGenerated(IndexName: string): boolean;
begin
  result:= (pos('RDB$PRIMARY',uppercase(Trim(IndexName)))=1);
end;

procedure CheckInitialIniFile;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF WIN64}
      fIniFile.WriteString('FireBird', 'ClientLib', 'C:\Program Files\Firebird\fbclient.dl');
      fIniFile.WriteString('FireBird', 'ConfPath',  'C:\Program Files\Firebird\firebird.conf');
    {$ELSE} // 32-Bit
      fIniFile.WriteString('FireBird', 'ClientLib', 'C:\Program Files (x86)\Firebird\fbclient.dl');
      fIniFile.WriteString('FireBird', 'ConfPath',  'C:\Program Files (x86)\Firebird\firebird.conf');
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
  fIniFile.WriteString('FireBird', 'ClientLib', '/opt/firebird/lib/libfbclient.s');
  fIniFile.WriteString('FireBird', 'ConfPath',  '/opt/firebird/firebird.conf');
  {$ENDIF}
end;


function ColorRValue(AColor: TColor): Byte;
begin
  Result := Red(ColorToRGB(AColor));
end;

function ColorGValue(AColor: TColor): Byte;
begin
  Result := Green(ColorToRGB(AColor));
end;

function ColorBValue(AColor: TColor): Byte;
begin
  Result := Blue(ColorToRGB(AColor));
end;

function AdjustColorByBrightness(AColor: TColor): TColor;
var
  R, G, B: Byte;
  Brightness: Double;
  RGBColor: TColor;
begin
  // Stelle sicher, dass Systemfarben in echte RGB-Werte umgewandelt werden
  RGBColor := ColorToRGB(AColor);

  // RGB-Komponenten extrahieren
  R := ColorRValue(RGBColor);
  G := ColorGValue(RGBColor);
  B := ColorBValue(RGBColor);

  // Helligkeit berechnen (mit Gewichtung für menschliches Auge)
  Brightness := 0.299 * R + 0.587 * G + 0.114 * B;

  // Rückgabe: bei heller Farbe → Schwarz, sonst → Weiß
  if Brightness > 180 then
    Result := clBlack
  else
    Result := clWhite;
end;

function SetInitialClientLib: boolean;
var  frmSetFBClient: TfrmSetFBClient;
begin
  frmSetFBClient := TfrmSetFBClient.Create(nil);
    try
      if frmSetFBClient.ShowModal = mrOK then
      begin
        // Benutzer hat neuen Pfad ausgewählt → erneut versuchen
        if FileExists(InitialFBClientLibPath) then
        begin
          InitialFBClient := LoadFBLibrary(InitialFBClientLibPath);

          try
            InitialFBClient.GetFirebirdAPI;     // ← Testen
            fIniFile.WriteString('FireBird', 'InitialFBClientLib', InitialFBClientLibPath);
            Result := True;
          except
            on E: Exception do
            begin
              MessageDlg('Fehler beim Laden der ausgewählten Client Library:' + LineEnding +
                         InitialFBClientLibPath + LineEnding + LineEnding +
                         E.Message, mtError, [mbOK], 0);
            end;
          end;

        end else
        begin
          MessageDlg('Die ausgewählte Datei existiert nicht:' + LineEnding +
                      InitialFBClientLibPath, mtError, [mbOK], 0);
        end;
      end;
    finally
      frmSetFBClient.Free;
    end;
end;

function LoadClientLibIBX(ALib: string): boolean;
var frmSetFBClient: TfrmSetFBClient;
begin
  Result := False;
  if FileExists(ALib) then
  begin
    InitialFBClient := LoadFBLibrary(ALib);

    try
      InitialFBClient.GetFirebirdAPI;  // ← HIER Fehler falls Library nicht geladen werden konnte
      Result := True;           // Erfolg!
      Exit;
    except
      on E: Exception do
      begin
        MessageDlg('Error loading the Firebird client library:' + LineEnding +
                   ALib + LineEnding + LineEnding +
                   E.Message, mtError, [mbOK], 0);
      end;
    end;
  end;
end;

Function ConnectToDBAs(dbIndex: Integer; ForceConnectDialog: boolean=false): Boolean;
var
  Rec: TRegisteredDatabase;
  Count: Integer;
begin
  Result:= False;
  Rec:= RegisteredDatabases[dbIndex].RegRec;
  fmEnterPass.laDatabase.Caption:= Rec.Title;
  fmEnterPass.edUser.Text:= Rec.UserName;
  fmEnterPass.edPassword.Clear;
  fmEnterPass.cbRole.Clear;
  // Use may have saved an empty password, which is valid for embedded dbs
  // So check SavePassword instead of Password itself.
  if (ForceConnectDialog=false) and Rec.SavePassword then
  try
    fmEnterPass.cbRole.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, otRoles, Count);
    fmEnterPass.cbRole.ItemIndex:= -1;
    fmEnterPass.cbRole.Text:= '';
    Result:= True; //this works, no need to go through a retry attempt below
  except
    // We don't particularly care which error occurred; we're trying again below.
    Result:= False;
  end;
  // Only show form if connection failed before
  if (ForceConnectDialog or (Result=false)) and
    (fmEnterPass.ShowModal = mrOk) then
  begin
    if fmReg.TestConnection(Rec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
      Rec.Charset, Rec.FireBirdClientLibPath, Rec.SQLDialect, Rec.Port, Rec.ServerName, Rec.OverwriteLoadedClientLib) then
    begin
      RegisteredDatabases[dbIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
      RegisteredDatabases[dbIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
      RegisteredDatabases[dbIndex].RegRec.Role:= fmEnterPass.cbRole.Text;

      RegisteredDatabases[dbIndex].IBDatabase.Params.Values['user_name'] := fmEnterPass.edUser.Text;
      RegisteredDatabases[dbIndex].IBDatabase.Params.Values['password']  := fmEnterPass.edPassword.Text;
      RegisteredDatabases[dbIndex].IBDatabase.LoginPrompt := false;
      Result:= True;
    end;
  end;
end;

initialization
  fIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  fIniFile     := TIniFile.Create(fIniFileName);

  if FirstRun then
    ExtractResources;

  ReadIniFile;


  //if not LoadClientLibIBX(InitialFBClientLibPath) then
    //SetInitialClientLib;

finalization
  WriteIniFile;
  fIniFile.Free;
  fIniFile := nil;
end.
