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
  DB,  RegExpr,  FileUtil,

  fbcommon,

  floginservicemanager,

  fpstdexports,
  fpDataExporter,

  IBInternals,
  IB,
  IBXServices,
  IBDatabase,
  IBDatabaseInfo,
  IBLocalDBSupport,
  IBQuery,
  IBCustomDataSet,
  IBTable,
  IBXScript,
  IBExtract,

  Variants,

  fSetFBClient,
  fServerSession,

  SysTables,
  EnterPass,
  fsimpleobjextractor,
  cUnIntelliSenseCache,
  uSystemInfo;

{$I version.inc}
{$I turbocommon.inc}

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
  tvotServer,
  tvotEmbeddedServer,

  tvotDatabase,

  tvotSearchFiter,

  tvotQueryWindow,

  tvotTableRoot,
  tvotTable,
  tvotTableFieldRoot,
  tvotTableField,

  // Constraints (NEU)
  tvotConstraintRoot,
  tvotConstraint,
  tvotPrimaryKeyRoot,
  tvotPrimaryKey,
  tvotForeignKeyRoot,
  tvotForeignKey,
  tvotUniqueConstraintRoot,
  tvotUniqueConstraint,
  tvotCheckConstraintRoot,
  tvotCheckConstraint,
  tvotNotNullConstraintRoot,
  tvotNotNullConstraint,
  tvotIndexRoot,
  tvotIndex,

  tvotTableReferencesRoot,
  tvotTableReference,

  tvotTablePermissionsRoot,

  tvotGeneratorRoot,
  tvotGenerator,

  //triggers

  tvotTriggerRoot,
  tvotTrigger,

  tvotTableTriggerRoot,
  tvotTableTrigger,


  tvotDBTriggerRoot,
  tvotDBTrigger,

  tvotDDLTriggerRoot,
  tvotDDLTrigger,

  tvotUDRTriggerRoot,
  tvotUDRTrigger,

  tvotUDRTableTriggerRoot,
  tvotUDRTableTrigger,

  tvotUDRDBTriggerRoot,
  tvotUDRDBTrigger,

  tvotUDRDDLTriggerRoot,
  tvotUDRDDLTrigger,

  tvotViewRoot,
  tvotView,

  tvotDomainRoot, {excludes system domains}
  tvotDomain,
  tvotRoleRoot,
  tvotRole,
  tvotExceptionRoot,
  tvotException,
  tvotUserRoot,
  tvotUser,

  tvotProcedureRoot,
  tvotProcedure,
  tvotFunctionRoot,
  tvotFunction,
  tvotUDRoot,
  tvotUDFRoot,
  tvotUDF,
  tvotUDRRoot,
  tvotUDRFunctionRoot,
  tvotUDRFunction,
  tvotUDRProcedureRoot,
  tvotUDRProcedure,

  tvotPackageRoot,
  tvotPackage,
  tvotPackageUDFFunctionRoot,
  tvotPackageUDFFunction,
  tvotPackageFunctionRoot,
  tvotPackageFunction,
  tvotPackageProcedureRoot,
  tvotPackageProcedure,
  tvotPackageUDRFunctionRoot,
  tvotPackageUDRFunction,
  tvotPackageUDRProcedureRoot,
  tvotPackageUDRProcedure,
  tvotPackageTriggerRoot,
  tvotPackageTrigger,
  tvotPackageUDRTriggerRoot,
  tvotPackageUDRTrigger,

  tvotDatabaseRoot,
  tvotBLOBFilterRoot,
  tvotBLOBFilter,
  tvotCommentRoot,
  tvotComment,
  tvotDataRoot,
  tvotData,

  tvotSystemObjectRoot,
  tvotSystemTableRoot,
  tvotSystemTable,
  tvotSystemTableFieldRoot,
  tvotSystemTableField,

  tvotSystemDomainRoot,
  tvotSystemDomain,
  tvotSystemGeneratorRoot,
  tvotSystemGenerator,
  tvotSystemTriggerRoot,
  tvotSystemTrigger,
  tvotSystemConstraintRoot,
  tvotSystemConstraint,
  tvotSystemIndexRoot,
  tvotSystemIndex,
  tvotSystemRoleRoot,
  tvotSystemRole,
  tvotSystemUserRoot,
  tvotSystemUser,
  tvotSystemExceptionRoot,
  tvotSystemException,

  tvotFormRoot,
  tvotForm
  );

  TFieldTransform = record
    SourceField   : string;      // Quell-Spaltenname
    DestField     : string;      // Ziel-Spaltenname
    Formula       : string;      // SQL-Ausdruck mit $1 als Platzhalter für SourceField
    CopyField     : Boolean;     // True = kopieren/transformieren, False = überspringen
    DestFieldType : string;      // Firebird-Datentyp für DECLARE (z.B. 'INTEGER', 'VARCHAR(100)')
  end;
  TFieldTransformArray = array of TFieldTransform;

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



  TTxFlags = set of (
    txReadOnly,
    txWait,
    txRecVersion,
    txAutoCommit,
    txReadConsistency
  );

  TTransactionConfigRecord = packed record
    Isolation: Byte;
    Flags: TTxFlags;
    LockTimeout: string[6];
    TxName: string[50];
  end;

  TRegisteredDatabase = packed record
    Title: string[30];
    DatabaseName: string[200];
    UserName: string[100];
    Password: string[100];
    Role: string[100];
    Charset: string[40];
    SQLDialect: string[1];
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

    IsEmbedded: boolean;

    TxConfig: string[255];

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
    IBLocalDBSupport: TIBLocalDBSupport;
  end;

  TPNodeInfos = ^TNodeInfos;
  TNodeInfos = record
      dbIndex: integer;
      ObjectType: TTreeViewObjectType;
      Refreshable: boolean;
      PopupMenuTag: integer;
      ImageIndex: Integer;
      ViewForm,
      EditorForm,
      NewForm,
      ExecuteForm: TForm;
      SimpleObjExtractor: TSimpleObjExtractor;
      UnIntelliSenseCache: TUnIntelliSenseCache;
      ServerSession: TServerSession;
      ServerVersionMajor: word;
      ServerVersionMinor: word;
      ServerVersionString: string;
      Visible: boolean;
      OwnerNode: TTreeNode;
    end;


  // ==========================================================================
  // Transfer-Statistik (für CloneTable, CSVExport, DataEditor)
  // ==========================================================================

  TCopyMethod = (cmLocal, cmLocalRowByRow, cmCrossExecuteBlock, cmCrossRowByRow, cmFBIntf);

  TTransferKind = (
    tkCopy,           // CloneTable: Tabelle → Tabelle
    tkExport,         // CSVExport: Tabelle → Datei
    tkLoadFile,       // DataEditor: Datei → Dataset
    tkLoadTable,      // DataEditor: Tabelle → Dataset
    tkCreateTable     // DataEditor: Dataset → Tabelle
  );

type
  TTransferStatistic = record
    Kind: TTransferKind;

    // === SOURCE ===
    SourceKind: string;
    SourceServer: string;
    SourceDatabase: string;
    SourceTable: string;
    SourceServerVersion: string;
    SourceFileFormat: string;
    SourceFileName: string;
    SourceFileSize: Int64;

    // === DESTINATION ===
    DestKind: string;
    DestServer: string;
    DestDatabase: string;
    DestTable: string;
    DestServerVersion: string;
    DestFileName: string;
    DestFileSize: Int64;

    // === OPTIONS ===
    Method: TCopyMethod;
    BatchSize: Integer;
    FromRow: Integer;
    ToRow: Integer;
    UseRowRange: Boolean;
    FormulaUsed: Boolean;
    FormulasApplied: string;
    CreateTableSQL: string;

    RowsPerSecond: Double;         // Load only, Interval-Mean
    BytesPerSecond: Double;        // Load only, Interval-Mean
    LoadElapsedSeconds: Double;    // Load only (ohne Scan)

    CSVOptions: string;

    // === CSV BUFFER PERFORMANCE ===
    CSVBlockSizeBytes: Integer;
    CSVBufferSizeBytes: Integer;
    CSVInitialCellCapacity: Integer;
    CSVRowWidthScanMode: string;
    CSVRowWidthScanRows: Int64;
    CSVRowWidthScanIsCustom: Boolean;
    CSVRowWidthScanTime: Double;
    CSVMaxRowWidth: Int64;
    CSVBufferAutoAdjusted: Boolean;
    CSVBufferOriginalBytes: Int64;
    CSVBlockAutoAdjusted: Boolean;
    CSVBlockOriginalBytes: Int64;

    OptionsExtra: string;

    PresetName: string;
    PresetFileName: string;
    PresetFileContent: string;

    // === RESULT ===
    RowsProcessed: Int64;
    FieldsCount: Integer;
    ElapsedSeconds: Double;

    // === MEMORY ===
    PeakRAMUsageMB:    Int64;       // höchster gemessener Systemverbrauch
    PeakEstimatedMB:   Int64;       // höchste Dataset-Schätzung
    PeakSwapUsedMB:    Int64;       // höchster gemessener Swap-Verbrauch
    SwapTotalMB:       Int64;       // Swap-Gesamt beim Start
    SwapStartUsedMB:   Int64;       // Swap-Verbrauch beim Load-Start
    ThrashingDetected: Boolean;     // Swap-Wachstum > 500 MB

    // === ENVIRONMENT ===
    ClientLibVersion: string;
    SystemInfo: TSystemInfo;
  end;


type
  TCSVScanProgressEvent = procedure(ABytesRead, ATotalBytes: Int64) of object;


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
  InitialServiceUser = 'service';
  InitialServiceUserPwd = 'service_pwd';

  DatabasesRegFile = 'databases.reg';
  ServersRegFile   = 'servers.reg';
  ThemesIniFile    = 'theme.ini';
  TmpDir = 'temp';

var

   AppName: string;
   WinCloseCanceled: Boolean = False;

    MainTreeView: TTreeView;

    fIniFileName: string;
    fIniFile: TInifile;

    RegisteredServers: array of TServerRecord;
    RegisteredDatabases: array of TDatabaseRec;

    MultiVersionConnection: string;

    InitialFBClientLibPath: string;
    InitialFBClient: IFirebirdLibrary;

    FB25, FB30, FB40, FB50, FB60: IFirebirdLibrary;

    //for Lazy-load & IntelliSense
    MetaDataChanged: boolean;  //Tables and Fields

    //Ini.File////////////////////////////////////////////////////////////////
    //UserInterface
    Language: string;

    //Backup options
    CloseDBBeforeBackup: boolean;

    //Restore options
    DefaultPageSize,
    DefaultNumBuffers: integer;
    RegisterDBAfterRestore: boolean;
    DefaultSegmentSize: string;
    DefaultSegmentUnit: string;


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

    //MetaData
    AlwaysQuoteIdentifiers: boolean;
    CaseSensitiveObjectNames: boolean;
    ShowSystem: boolean;

    //MetaDataTableExtract
    ExtractDomains,
    ExtractIndex,
    ExtractForeign,
    ExtractCheck,
    ExtractTrigger,
    ExtractGrant,
    ExtractData: boolean;

    //IntelliSense
    ReloadCacheOnNodeExpand: boolean;


    //MainTreeView
    MainTreeViewAlwaysRefresh: boolean;
    MainTreeViewColor: TColor;
    MainTreeViewFontName: string;
    MainTreeViewFontStyle: TFontStyles;
    MainTreeViewFontSize: word;
    MainTreeViewFontColor: TColor;
    AutoFilter: boolean;

    //QueryWindow
    QWAutoCommit: boolean;
    QWExecMultiStatementsAsScript: boolean;
    QWShowNavigator: boolean;
    QWEditorBackgroundColor: TColor;
    QWEditorFontName: string;
    QWEditorFontSize: word;
    QWEditorFontColor: TColor;
    QWEditorFontStyle: TFontStyles;

    //Theme
    AllowIniOverrides: boolean;


    //Transaction Defaults (txt file) ====
    DefTxFileName: string[255];          // optionaler Label / Alias


    //CSVExport
    DefaultBatchSize: integer;
    CSVExportDefaultPreset: string;

    //CSV Editor
    CSVDefaultFieldLength: integer;
    CSVDelimiter: char;
    CSVLineEnding: string;
    CSVQuoteChar: char;
    CSVFirstLineAsFieldNames,
    CSVIgnoreOuterWhitespace,
    CSVQuoteOuterWhitespace: boolean;

    //CSV FileRead Options
    CSVBlockSizeBytes: Integer = 1048576;      // 1 MB
    CSVBufferSizeBytes: Int64 = 2097152;       // 2 MB
    CSVInitialCellCapacity: Int64 = 64;
    CSVRowWidthScanMB: Integer = 10;
    CSVMinRowsForRowWidthScan: Integer = 1000;
    CSVProgressUpdatePercent: Integer = 1;
    CSVAutoTuneCSVBuffer: boolean;

    // Auto-Detect-Schalter (pro Feld einzeln)
    AutoDetectLineEnding: Boolean;
    AutoDetectDelimiter:  Boolean;
    AutoDetectQuoteChar:  Boolean;

    //UpdateChecker
    AutoSearchOnProgramStart: boolean;
    UnzipAfterDownload: boolean;
    RunNewVersionAfterUnzip: boolean;
    DeleteOldVersions: boolean;

    //Database
    NumBuffers: integer;


    //SQLMonitor
    TraceEnabled: boolean;
    TraceFlags: string;
    SlowQueryThreshold: Integer;  // in Millisekunden

//end-Ini.File////////////////////////////////////////////////////////////////

//Format
function FormatBytesSmart(ABytes: Int64): string;


//Files
function CountTextFileLines(const AFileName: string; ABufferSize: Int64 = 0; ALineEnding: Char = #0): Int64;

//CSV
function CSVLineEndingNameToStr(const AName: string): string;
function CSVLineEndingStrToName(const AValue: string): string;
function DetectLineEnding(const AFileName: string): string;
function DetectDelimiter(const AFileName: string): Char;
function DetectQuoteChar(const AFileName: string; ADelimiter: Char): Char;
function EstimateCSVMaxLineWidth(
  const AFileName: string;
  AFromRow: Int64;
  AToRow: Int64;
  AScanRows: Int64;
  AQuoteChar: Char;
  out ATotalRowsInRange: Int64;
  AOnProgress: TCSVScanProgressEvent = nil;
  ACancelFlag: PBoolean = nil): Int64;

procedure SetDBInstanceIndex(AInstance: TIBDatabase; ADBIndex: Integer);
function  GetDBInstanceIndex(AInstance: TIBDatabase): Integer;
function FindDBIndexByDatabaseInstance(ADatabase: TIBDatabase): Integer;

function RunStartupCheck: string;

// Transaktions-Sicherheit
procedure EnsureTransactionParams(ATrans: TIBTransaction;
                                  const ADefaultFile: string = '');
function  IsTransactionParamsValid(ATrans: TIBTransaction;
                                   out Reason: string): Boolean;
function  IsTransactionParamsValid(ATrans: TIBTransaction): Boolean; overload;

// ============================================================
// Prüft, ob ein Privileg-Code in einem Privilegien-String enthalten ist.
// APrivileges Format: 'S,I,U,SG' (Komma-getrennt).
// APriv: 'S', 'I', 'U', 'D', 'R', 'SG', 'IG', 'UG', 'DG', 'RG'.
//
// Semantik: Pos(APriv, APrivileges) > 0 — d.h. 'S' matcht auch 'SG',
// weil WITH GRANT OPTION die Basis-Berechtigung implizit enthält.
// Das entspricht der bisherigen Logik in TableManage.FillPermissions.
// ============================================================
function PermissionHas(const APrivileges, APriv: string): Boolean;


// ============================================================
// Entfernt einen optionalen TABLE.-Präfix aus einem Node-Text.
// Wird von globalen Trigger-Listen verwendet (Format: TABLE.TRIGGER),
// damit der Edit/View-Code nur den reinen Trigger-Namen weiterreicht.
// Trigger-Namen sind DB-weit eindeutig — deshalb reicht der Suffix.
// ============================================================
function StripTablePrefix(const AName: string): string;

// ==========================================================================
// Transfer-Statistik (für CloneTable, CSVExport, DataEditor)
// ==========================================================================
function CopyMethodToStr(M: TCopyMethod): string;
function CopyStatsRowsPerSec(const Stats: TTransferStatistic): Double;
function FormatNumberEN(const AValue: Int64): string;
function FormatTransferReport(const Stats: TTransferStatistic): string;


//transactions
function GetTransactionParamsAsString(ATrans: TIBTransaction): string;

//Dialogs
function ShowInfoDialog(const aMsg: string): TModalResult;
function ShowInfoDialog(const aCaption, aMsg: string): TModalResult;
function ShowWarningDialog(const aMsg: string): TModalResult;
function ShowWarningDialog(const aCaption, aMsg: string): TModalResult;
function ShowErrorDialog(const aMsg: string): TModalResult;
function ShowErrorDialog(const aCaption, aMsg: string): TModalResult;
function ShowConfirmDialog(const aMsg: string): TModalResult;
function ShowConfirmDialog(const aCaption, aMsg: string): TModalResult;
function ShowMessageDialog(const aMsg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons): TModalResult; overload;
function ShowMessageDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons): TModalResult; overload;


function StringToTraceFlags(const S: string): TTraceFlags;
function TraceFlagsToString(Flags: TTraceFlags): string;

function ShowServerSelectDialog(
      const ATitle: string;
      const ASuggestedDBTitle: string;
      out ASelectedServer: string;
      out ADBTitle: string
    ): Boolean;

function CloneServerRegistry(
  const ASourceAlias: string;
  const ADestServerName: string;
  const ADestServerAlias: string;
  const ADestPort: string;
  const ADestProtocol: TProtocol
): Boolean;

function ShowDBCloneDialog(
  const ATitle: string;
  const ASuggestedDBTitle: string;
  out ASelectedServer: string;
  out ADBTitle: string
): Boolean;

function CloneServerRegistryInteractive: Boolean;
function CloneAllDatabasesForServer(const ASourceServer, ADestServer: string): integer;


//CopyDBRegistry Server->Server
// gibt neuen DBIndex zurück
function CopyDBRegistry(ASourceDBIndex  : Integer; const ADestServerName : string;
                        const ADestDBTitle : string = ''; ATemporary: Boolean = False): Integer;
function CreateEmptyDatabase(const AServerName, ADBTitle: string): Boolean;
function CreateDatabaseWithMetadata(ASourceDBIndex, ADestDBIndex: Integer): Boolean;
function CloneDBRegistryInteractive(ANodeInfos: TPNodeInfos): Boolean;


// Session-Cache für Passwörter
var    SessionPasswordCache: TStringList;
  function  GetServerSessionPassword(const AServerName: string): string;
  procedure SetServerSessionPassword(const AServerName, APassword: string);
  procedure ClearServerSessionPassword(const AServerName: string);
  function  GetDBSessionPassword(const AServerName, ADatabaseName: string): string;
  function  GetDBSessionPasswordByIndex(ADBIndex: Integer): string;
  procedure SetDBSessionPassword(const AServerName, ADatabaseName, APassword: string);
  procedure ClearDBSessionPassword(const AServerName, ADatabaseName: string);
  procedure ClearAllSessionPasswords;
  procedure CachePasswordAfterConnect(ADBIndex: Integer; ADatabase: TIBDatabase);

//Forms
function GetFormsBasePath: string;
function GetDBFormsPath(const AServerName, ADBAlias: string): string;
function SanitizeFileName(const AName: string): string;
//

function FormatMultilineSQLText(const AText: string): string;

procedure SaveTxConfigToFile(const FileName: string; Params: TStrings);
procedure LoadTxConfigFromFile(const FileName: string; Params: TStrings);

function IBXProtocolToString(AProtocol: TProtocol): string;
function StringToIBXProtocol(const AValue: string): TProtocol;

function IsFilterScopeNode(ANode: TTreeNode): Boolean;
function MatchesFilter(const AText, AFilter: string): Boolean;

function  IsObjectNameCaseSensitive(AObjectName: string): boolean;
function  IsObjectNameQuoted(const AStr: string): Boolean;
function  MakeObjectNameQuoted(AObjectName: string): string;
function  MakeCaseSensitiveAuto(const AObjectName: string): string;

procedure MakeObjectNameListQuoted(var AObjectList: TStringList);
function  StripQuotes(const S: string): string;
function StripIdentifierQuotes(const AName: string): string;

function NeedsCommit(Q: TIBQuery): Boolean;

function MakeConnectionString(AServerName, APort, ADBFileName: string): string;

function IsServerReachable(AserverName: string; out ErrorStr: string): boolean;
function CreateServerSessionFromRegistry(const AServerName: string): TServerSession;

procedure ParseDBConnectionString(const AConnStr: string;
  out AHost, APort, APath: string);
function ValidateDBConnectionString(const AConnStr, AExpectedServerAlias: string;
  out AWarningMsg: string): Boolean;


function GetServerInventoryAsText: string;
function GetDBFileNameFromConnectionString(AConnStr: string): string;

function GetSeverImplemetationVersionFromIBDB(ADB: TIBDatabase): string;
function GetServerMajorVersionFromIBDB(ADB: TIBDatabase): Word;
function GetServerMinorVersionFromIBDB(ADB: TIBDatabase): Word;

function GetDBIndexByDatabase(Database: TIBDatabase): Integer;
function GetDBTitlesForServer(const ADisplayAlias: string; AList: TStrings): Boolean;
function FindDBIndexByTitleAndServer(const ATitle, AServerName: string): Integer;


function ConnectToDBAs(dbIndex: Integer; ForceConnectDialog: boolean=false): Boolean;

function LoadClientLibIBX(ALib: string): boolean;
function SetInitialClientLib: boolean;

function TestEmbeddedConnection(AServerRec: TServerRecord; out AFBVersionMajor: word;
                                                           out AFBVersionMinor: word;
                                                           out AFBServerVersion: string): boolean;

function  CloseDB(dbIndex: integer): boolean;

function DeleteDBRegistrationFromFile(const ATitle: string; const AServerName: string): Boolean;
function EditRegisteredDB(var ARec: TRegisteredDatabase): Boolean;

procedure MarkAllServerDatabasesDeleted(const ServerName: string);
procedure MarkAllDatabasesDeleted;
procedure RemoveDeletedDBRegistrationsFromFile;

function GetDBRecByServerAndDBName(const AServerName, ADBName: string): TDatabaseRec;

function IsValidPortNr(AStr: string): boolean;
function ServerNameContainPortNr(AServerName: string): boolean;
function GetPortNrFromServerName(AServerName: string): string;
function GetHostFromServerName(AServerName: string): string;

procedure AssignIBDatabase(const Source, Target: TIBDatabase);
function AreSameDB(const DB1, DB2: TIBDatabase): Boolean;

function GetServerName(DBName: string): string;

function IsServerAliasUnique(const AAlias, AExcludeAlias, AExcludeServerName: string): Boolean;
function SaveDBRegistrationsToFile: Boolean;
function RenameDBRegistrationsServerAlias(
  const AOldName, ANewAlias, ARealServerName: string): Integer;
function GetServerRecordFromFileByAlias(const AAlias: string): TServerRecord;
function GetServerRecordFromFileByName(AServerName: string): TServerRecord;
function GetFirstServerRecordFromFile: TServerRecord;
function GetServerRecordFromFileByIndex(AIdx: Integer): TServerRecord;

procedure SaveServerDataToFile(const Rec: TServerRecord; const AOldAlias: string = '');
procedure ApplyServerRecordToSession(const Rec: TServerRecord; Session: TServerSession);
function  BuildServerRecordFromSession(const Session: TServerSession; SavePwd: Boolean): TServerRecord;

function GetServerAliasFromNode(ANode: TTreeNode): string;
function GetServerNodeByAlias(const AAlias: string): TTreeNode;
function GetServerNameFromNode(ANode: TTreeNode): string;
function GetServerNodeByServerName(const AServerName: string): TTreeNode;

function GetServerAliasListFromTreeView: TStringList;
function GetServerListFromTreeView: TStringList;
function GetServerAndPortListFromTreeView: TStringList;

function FindNodeByDBIndex(ADBIndex: Word): TTreeNode;
function GetAncestorAtLevel(ANode: TTreeNode; ALevel: Integer): TTreeNode;
function GetAncestorNodeText(ANode: TTreeNode; ALevel: Integer): string;

function GetServerNameFromConnString(ADBIndex: word): string;
function GetDBNameFromConnString(ADBIndex: word): string;

function GetConfigurationDirectory: string;
procedure ReadIniFile;
procedure WriteIniFile;
function FirstRun: boolean;
procedure ExtractResourcesIfNeeded;
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
function FBTypeToTreeViewType(AType: TObjectType): TTreeViewObjectType;
function TreeViewTypeToFBType(AType: TTreeViewObjectType): TObjectType;


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

function StrToFontStyles(const S: string): TFontStyles;
function FontStylesToStr(AStyles: TFontStyles): string;

//Arrays
function GetArrayFieldInfo(DB: TIBDatabase; Field: TIBArrayField): string;
function ArrayDimsToSuffix(const ADims: TArrayDims): string;


//ExternalTable
function GetTableFieldsForExternalTable(ADBIndex: Integer;
  const ATableName: string): TDataSet;

function CountChar(const S: string; const C: Char): Integer;
procedure ConnectDBPrepared(ADatabase: TIBDatabase; ATransaction: TIBTransaction; ADBIndex: Integer;
                            ATxParams: TStrings);

function IsDomain(const ATypeName: string): Boolean;
function DomainToDataType(const ADomainName: string; ADatabase: TIBDatabase; ATransaction: TIBTransaction): string;



implementation

uses  main, Reg;

//Format
function FormatBytesSmart(ABytes: Int64): string;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator  := '.';
  FS.ThousandSeparator := ',';

  if ABytes >= (Int64(1024) * 1024 * 1024) then
    Result := FormatFloat('#,##0.00', ABytes / (1024.0 * 1024 * 1024), FS) + ' GB'
  else if ABytes >= (Int64(1024) * 1024) then
    Result := FormatFloat('#,##0.00', ABytes / (1024.0 * 1024), FS) + ' MB'
  else if ABytes >= 1024 then
    Result := FormatFloat('#,##0.00', ABytes / 1024.0, FS) + ' KB'
  else
    Result := IntToStr(ABytes) + ' Bytes';
end;

//Files
function CountTextFileLines(const AFileName: string; ABufferSize: Int64; ALineEnding: Char): Int64;
const
  DEFAULT_BUFFER_SIZE = 16 * 1024* 1024;   // 16 KB
var
  FS: TFileStream;
  Buf: array of Byte;
  BytesRead: Integer;
  I: Integer;
  TargetByte: Byte;
  LastByte: Byte;
  CRCount, LFCount: Int64;
  DetectedLineEnding: Char;
  BufSize: Integer;
begin
  Result := -1;
  if not FileExists(AFileName) then Exit;

  // Puffergröße: 0 → Standard, sonst Minimum 4 KB, Maximum 16 MB
  BufSize := ABufferSize;
  if BufSize <= 0 then
    BufSize := DEFAULT_BUFFER_SIZE
  else
  begin
    if BufSize < 4096 then BufSize := 4096;
    if BufSize > 16 * 1024 * 1024 then BufSize := 16 * 1024 * 1024;
  end;

  SetLength(Buf, BufSize);
  Result := 0;
  LastByte := 0;
  DetectedLineEnding := ALineEnding;

  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    // ============================================================
    // Phase 1: LineEnding automatisch erkennen (wenn nicht vorgegeben)
    // ============================================================
    if DetectedLineEnding = #0 then
    begin
      BytesRead := FS.Read(Buf[0], BufSize);
      if BytesRead <= 0 then
      begin
        // Leere Datei
        Result := 0;
        Exit;
      end;

      CRCount := 0;
      LFCount := 0;
      for I := 0 to BytesRead - 1 do
      begin
        if Buf[I] = 13 then Inc(CRCount)
        else if Buf[I] = 10 then Inc(LFCount);
      end;

      // Entscheidung:
      // LF vorhanden → LF (deckt LF und CRLF ab)
      // Nur CR (kein LF) → CR (alte Mac-Format)
      // Weder noch → Datei hat nur eine Zeile
      if LFCount > 0 then
        DetectedLineEnding := #10
      else if CRCount > 0 then
        DetectedLineEnding := #13
      else
      begin
        // Keine Zeilenumbrüche → eine Zeile
        Result := 1;
        Exit;
      end;

      // Position zurück auf Anfang
      FS.Position := 0;
    end;

    // ============================================================
    // Phase 2: Zählen mit erkanntem/vorgegebenem LineEnding
    // ============================================================
    TargetByte := Byte(DetectedLineEnding);

    repeat
      BytesRead := FS.Read(Buf[0], BufSize);
      if BytesRead > 0 then
      begin
        I := 0;
        while I < BytesRead do
        begin
          if Buf[I] = TargetByte then
            Inc(Result);
          Inc(I);
        end;
        LastByte := Buf[BytesRead - 1];
      end;
    until BytesRead < BufSize;

    // Letzte Zeile ohne abschließenden Zeilenumbruch mitzählen
    if (FS.Size > 0) and (LastByte <> TargetByte) then
      Inc(Result);
  finally
    FS.Free;
    SetLength(Buf, 0);
  end;
end;

//CSV
// ============================================================
// CSV Auto-Detection — LineEnding, Delimiter & QuoteChar
//
// DetectLineEnding:
//   Liest die ersten 8 KB und ermittelt das vorherrschende
//   Zeilenende (CRLF, LF oder CR) per Mehrheitsprinzip.
//   Fallback: LF
//
// DetectDelimiter:
//   Liest die ersten 8 KB und findet das häufigste Trennzeichen
//   außerhalb von Quotes in der ersten Zeile.
//   Kandidaten: ','  ';'  #9 (Tab)  '|'
//   Fallback: ','
//
// DetectQuoteChar:
//   Sucht nach einem Quote-Zeichen das an einer Feldgrenze
//   (nach Delimiter, nach Zeilenumbruch oder am Dateianfang)
//   auftritt. Damit werden Apostrophe im Text (it's) nicht mit
//   Single-Quote-Feldern verwechselt.
//   Fallback: '"'
function CSVLineEndingNameToStr(const AName: string): string;
begin
  if SameText(Trim(AName), 'CRLF') then Result := #13#10
  else if SameText(Trim(AName), 'LF') then Result := #10
  else if SameText(Trim(AName), 'CR') then Result := #13
  else if SameText(Trim(AName), 'LINEENDING') then Result := LineEnding
  else if SameText(Trim(AName), 'SYSTEM') then Result := LineEnding
  else Result := #10;  // portabler Default
end;

function CSVLineEndingStrToName(const AValue: string): string;
begin
  if AValue = #13#10 then Result := 'CRLF'
  else if AValue = #10 then Result := 'LF'
  else if AValue = #13 then Result := 'CR'
  else Result := 'LF';
end;

function DetectLineEnding(const AFileName: string): string;
var
  FS: TFileStream;
  Buf: array[0..8191] of Byte;
  BytesRead, i: Integer;
  CRCount, LFCount, CRLFCount: Integer;
begin
  Result := #10;   // Default LF — portabel

  if not FileExists(AFileName) then
    Exit;

  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    BytesRead := FS.Read(Buf, SizeOf(Buf));
  finally
    FS.Free;
  end;

  if BytesRead < 2 then
    Exit;

  CRCount   := 0;
  LFCount   := 0;
  CRLFCount := 0;
  i := 0;

  while i < BytesRead do
  begin
    if Buf[i] = 13 then
    begin
      if (i + 1 < BytesRead) and (Buf[i + 1] = 10) then
      begin
        Inc(CRLFCount);
        Inc(i, 2);
        Continue;
      end
      else
        Inc(CRCount);
    end
    else if Buf[i] = 10 then
      Inc(LFCount);
    Inc(i);
  end;

  // Mehrheit entscheidet
  if (CRLFCount >= CRCount) and (CRLFCount >= LFCount) and (CRLFCount > 0) then
    Result := #13#10
  else if (LFCount >= CRCount) and (LFCount > 0) then
    Result := #10
  else if CRCount > 0 then
    Result := #13;
end;

function DetectDelimiter(const AFileName: string): Char;
var
  FS: TFileStream;
  Buf: array[0..8191] of Byte;
  BytesRead, i: Integer;
  S: string;
  CommaCount, SemicolonCount, TabCount, PipeCount: Integer;
  InDoubleQuote, InSingleQuote: Boolean;
begin
  Result := ',';

  if not FileExists(AFileName) then
    Exit;

  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    BytesRead := FS.Read(Buf, SizeOf(Buf));
  finally
    FS.Free;
  end;

  if BytesRead <= 0 then
    Exit;

  SetLength(S, BytesRead);
  Move(Buf[0], S[1], BytesRead);

  CommaCount     := 0;
  SemicolonCount := 0;
  TabCount       := 0;
  PipeCount      := 0;
  InDoubleQuote  := False;
  InSingleQuote  := False;

  for i := 1 to Length(S) do
  begin
    // Nur erste Zeile analysieren
    if S[i] = #10 then Break;
    if S[i] = #13 then Continue;

    // Quote-Zustand tracken
    if S[i] = '"' then
    begin
      InDoubleQuote := not InDoubleQuote;
      Continue;
    end;

    if (S[i] = '''') and (not InDoubleQuote) then
    begin
      InSingleQuote := not InSingleQuote;
      Continue;
    end;

    if InDoubleQuote or InSingleQuote then
      Continue;

    case S[i] of
      ',': Inc(CommaCount);
      ';': Inc(SemicolonCount);
      #9:  Inc(TabCount);
      '|': Inc(PipeCount);
    end;
  end;

  // Häufigster gewinnt — mindestens 1 Vorkommen
  if (SemicolonCount >= 1) and
     (SemicolonCount > CommaCount) and
     (SemicolonCount > TabCount) and
     (SemicolonCount > PipeCount) then
    Result := ';'

  else if (TabCount >= 1) and
          (TabCount > CommaCount) and
          (TabCount > SemicolonCount) and
          (TabCount > PipeCount) then
    Result := #9

  else if (PipeCount >= 1) and
          (PipeCount > CommaCount) and
          (PipeCount > SemicolonCount) and
          (PipeCount > TabCount) then
    Result := '|'

  else
    Result := ',';
end;

function DetectQuoteChar(const AFileName: string; ADelimiter: Char): Char;
var
  FS: TFileStream;
  Buf: array[0..8191] of Byte;
  BytesRead, i: Integer;
  S: string;
  C, Prev: Char;
  DoubleAtBoundary, SingleAtBoundary: Integer;
begin
  Result := '"';

  if not FileExists(AFileName) then
    Exit;

  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    BytesRead := FS.Read(Buf, SizeOf(Buf));
  finally
    FS.Free;
  end;

  if BytesRead <= 0 then
    Exit;

  SetLength(S, BytesRead);
  Move(Buf[0], S[1], BytesRead);

  DoubleAtBoundary := 0;
  SingleAtBoundary := 0;
  Prev := #0;

  for i := 1 to Length(S) do
  begin
    C := S[i];

    // Quote an einer Feldgrenze?
    // (nach Delimiter, nach Zeilenumbruch oder am Dateianfang)
    if (Prev = ADelimiter) or (Prev = #10) or (Prev = #13) or (i = 1) then
    begin
      if C = '"' then
        Inc(DoubleAtBoundary)
      else if C = '''' then
        Inc(SingleAtBoundary);
    end;

    Prev := C;

    // Nur erste Zeile reicht für die Erkennung
    if (i > 1) and (C = #10) then
      Break;
  end;

  if (DoubleAtBoundary = 0) and (SingleAtBoundary = 0) then
    Result := #0                     // keine Quotes erkannt
  else if SingleAtBoundary > DoubleAtBoundary then
    Result := ''''
  else
    Result := '"';
end;

{ ------------------------------------------------------------------
  EstimateCSVMaxLineWidth
  Scannt eine CSV-Datei und liefert die maximale Zeilenbreite.

  Parameter:
    AFileName    : Datei
    AFromRow     : Erste Datenzeile im Range (1-basiert, ohne Header)
    AToRow       : Letzte Datenzeile im Range (0 = bis Datei-Ende)
    AScanRows    : 0 = FullScan (kompletter Range)
                   >0 = CustomScan (nur die ersten N Zeilen ab AFromRow)
    AQuoteChar   : Quote-Zeichen (z.B. ")

  Out:
    ATotalRowsInRange : Anzahl gescannter Datenzeilen
                        - bei FullScan: exakt
                        - bei CustomScan: = AScanRows

  Return:
    Maximale Zeilenbreite in Bytes, oder -1 bei Fehler.

  Hinweis:
    Bei CustomScan wird der Rest des Range NICHT geprüft.
    Das ist gewollt: User akzeptiert bewusst das Risiko,
    dass später eine größere Zeile auftaucht.
  ------------------------------------------------------------------ }
  function EstimateCSVMaxLineWidth(
    const AFileName: string;
    AFromRow: Int64;
    AToRow: Int64;          // 0 = no upper limit
    AScanRows: Int64;       // 0 = full scan of range
    AQuoteChar: Char;
    out ATotalRowsInRange: Int64;
    AOnProgress: TCSVScanProgressEvent = nil;
    ACancelFlag: PBoolean = nil): Int64;
  const
    BUF_SIZE = 4 * 1024 * 1024;
  var
    FS: TFileStream;
    Buf: TBytes;
    P, BufEnd: PChar;
    BytesRead: Integer;
    MaxWidth, CurrentWidth: Int64;
    InQuote: Boolean;
    PendingLF: Boolean;
    DataRowInFile, DataRowIdx: Int64;
    RowsScannedInRange: Int64;
    StopScan: Boolean;
    TotalBytesRead: Int64;
  begin
    Result := -1;
    ATotalRowsInRange := 0;
    if not FileExists(AFileName) then Exit;

    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Buf, BUF_SIZE);
      MaxWidth           := 0;
      CurrentWidth       := 0;
      InQuote            := False;
      PendingLF          := False;
      DataRowInFile      := 0;
      RowsScannedInRange := 0;
      StopScan           := False;
      TotalBytesRead     := 0;

      while not StopScan do
      begin
        BytesRead := FS.Read(Buf[0], BUF_SIZE);
        if BytesRead <= 0 then Break;

        Inc(TotalBytesRead, BytesRead);

        if Assigned(AOnProgress) then
          AOnProgress(TotalBytesRead, FS.Size);

        if Assigned(ACancelFlag) and ACancelFlag^ then
          raise EAbort.Create('Cancelled by user');

        P := Pointer(Buf);
        BufEnd := P + BytesRead;

        // Consume LF that belonged to a CR at the previous buffer end.
        if PendingLF then
        begin
          PendingLF := False;
          if (P < BufEnd) and (P^ = #10) then
            Inc(P);
        end;

        while P < BufEnd do
        begin
          if P^ = AQuoteChar then
          begin
            InQuote := not InQuote;
            Inc(CurrentWidth);   // quote bytes count towards row width
          end
          else if (not InQuote) and ((P^ = #10) or (P^ = #13)) then
          begin
            Inc(DataRowInFile);
            DataRowIdx := DataRowInFile - 1;

            if DataRowIdx >= AFromRow then
            begin
              if (AToRow > 0) and (DataRowIdx > AToRow) then
              begin
                StopScan := True;
                Break;
              end;

              if CurrentWidth > MaxWidth then MaxWidth := CurrentWidth;
              Inc(RowsScannedInRange);
              Inc(ATotalRowsInRange);

              if (AScanRows > 0) and (RowsScannedInRange >= AScanRows) then
              begin
                StopScan := True;
                Break;
              end;
            end;

            CurrentWidth := 0;

            if P^ = #13 then
            begin
              if (P + 1 < BufEnd) and ((P + 1)^ = #10) then
                Inc(P)               // CRLF inside buffer
              else if P + 1 >= BufEnd then
                PendingLF := True;   // CR at boundary; LF may start next buffer
              // else: bare CR
            end;
          end
          else
            Inc(CurrentWidth);

          Inc(P);
        end;
      end;

      // Last row without trailing newline.
      if (not StopScan) and (not PendingLF) and (CurrentWidth > 0) then
      begin
        Inc(DataRowInFile);
        DataRowIdx := DataRowInFile - 1;
        if DataRowIdx >= AFromRow then
        begin
          if (AToRow = 0) or (DataRowIdx <= AToRow) then
          begin
            if CurrentWidth > MaxWidth then MaxWidth := CurrentWidth;
            Inc(ATotalRowsInRange);
          end;
        end;
      end;

      Result := MaxWidth;
    finally
      FS.Free;
    end;
  end;

// ============================================================================
// Markiert eine TIBDatabase-Instanz mit ihrem RegisteredDatabases-Index.
//
// Wird vom Tag-Feld der Komponente gespeichert (Offset +1, damit 0 = "unset"
// bleibt — das ist der TComponent-Default).
//
// Zweck: Form-lokale TIBDatabase-Instanzen (FIBConnection in QueryWindow,
// IBDBSource/IBDBDest in CloneTable, etc.) eindeutig ihrem registrierten
// Eintrag in RegisteredDatabases[] zuordnen — auch wenn dieselbe DB unter
// mehreren Titeln registriert ist.
// ============================================================================
procedure SetDBInstanceIndex(AInstance: TIBDatabase; ADBIndex: Integer);
begin
  if Assigned(AInstance) then
    AInstance.Tag := ADBIndex + 1;
end;

function GetDBInstanceIndex(AInstance: TIBDatabase): Integer;
begin
  Result := -1;
  if Assigned(AInstance) and (AInstance.Tag > 0) then
    Result := AInstance.Tag - 1;
end;

// ============================================================================
// Findet den DB-Index einer TIBDatabase-Instanz — 100% eindeutig.
//
// Reihenfolge:
//   1. Tag-Feld der Instanz (form-lokale Verbindungen)
//   2. Pointer-Match gegen RegisteredDatabases[] (geteilte Instanz)
//
// Kein String-Fallback mehr nötig — beide Pfade sind eindeutig.
// ============================================================================
function FindDBIndexByDatabaseInstance(ADatabase: TIBDatabase): Integer;
var
  i: Integer;
begin
  Result := -1;
  if not Assigned(ADatabase) then
    Exit;

  // 1. Tag (form-lokal)
  Result := GetDBInstanceIndex(ADatabase);
  if Result >= 0 then
    Exit;

  // 2. Pointer (geteilte Instanz)
  for i := 0 to High(RegisteredDatabases) do
    if RegisteredDatabases[i].IBDatabase = ADatabase then
      Exit(i);
end;

// ============================================================================
// Ensures that a transaction has non-empty Params.
//
// If ADefaultFile is provided and exists → loads params from there.
// Otherwise → falls back to a hardcoded default (read_committed etc.).
//
// If Params already has content, this function does nothing.
// ============================================================================
procedure EnsureTransactionParams(ATrans: TIBTransaction;
                                  const ADefaultFile: string);
var
  UseFile: string;
begin
  if not Assigned(ATrans) then
    Exit;

  // Nur eingreifen wenn Params leer sind
  if ATrans.Params.Count > 0 then
    Exit;

  UseFile := ADefaultFile;

  if FileExists(UseFile) then
    ATrans.Params.LoadFromFile(UseFile)
  else
  begin
    // Harter Fallback — funktioniert immer
    ATrans.Params.Clear;
    ATrans.Params.Add('read_committed');
    ATrans.Params.Add('rec_version');
    ATrans.Params.Add('nowait');
  end;
end;

// ============================================================================
// Validates a transaction's Params.
//
// Konservativ: erkennt offensichtlich kaputte Inhalte (leer, unbekannte
// Parameter, Steuerzeichen) — nicht exotische aber gültige IBX-Flags.
//
// Bekannte gültige Keys (Firebird TPB):
//   read_committed, read_consistency, rec_version, no_rec_version,
//   nowait, wait, read_only, write
// ============================================================================
function IsTransactionParamsValid(ATrans: TIBTransaction;
                                  out Reason: string): Boolean;
const
  ValidKeys: array[0..8] of string = (
    'read_committed',
    'read_consistency',
    'rec_version',
    'no_rec_version',
    'nowait',
    'wait',
    'read_only',
    'write',
    'autocommit'
  );
var
  i, j: Integer;
  Line, LineLC: string;
  IsValid: Boolean;
  HasValidKey: Boolean;
begin
  Result := True;
  Reason := '';

  if not Assigned(ATrans) then
  begin
    Reason := 'Transaction is nil';
    Exit(False);
  end;

  if ATrans.Params.Count = 0 then
  begin
    Reason := 'Params are empty';
    Exit(False);
  end;

  HasValidKey := False;

  for i := 0 to ATrans.Params.Count - 1 do
  begin
    Line := Trim(ATrans.Params[i]);

    // Leere Zeilen ignorieren (können durch Text-Assign entstehen)
    if Line = '' then
      Continue;

    LineLC := LowerCase(Line);

    // Steuerzeichen-Check (Müll aus Speicher-Drift)
    for j := 1 to Length(Line) do
      if Ord(Line[j]) < 32 then
      begin
        Reason := 'Invalid control character in param: "' + Line + '"';
        Exit(False);
      end;

    // Bekannter Key?
    IsValid := False;
    for j := Low(ValidKeys) to High(ValidKeys) do
      if Pos(ValidKeys[j], LineLC) = 1 then
      begin
        IsValid := True;
        HasValidKey := True;
        Break;
      end;

    // Unbekannter Parameter → nicht unbedingt Fehler,
    // aber wenn NICHTS bekannt ist, ist es wahrscheinlich Müll.
    // Wir zählen gültige Keys und entscheiden am Ende.
  end;

  if not HasValidKey then
  begin
    Reason := 'No recognized transaction parameter found (all params unknown)';
    Exit(False);
  end;
end;

function IsTransactionParamsValid(ATrans: TIBTransaction): Boolean;
var
  Dummy: string;
begin
  Result := IsTransactionParamsValid(ATrans, Dummy);
end;

function RunStartupCheck: string;
var
  Msg: TStringList;
  RegFile: string;
  FileLen: Int64;
  ConfigDir: string;
  i: Integer;
  Bad: Integer;
  HasLayoutDrift: Boolean;
  LayoutDriftReport: string;
  DbsRegExists: Boolean;
  SrvRegExists: Boolean;
  TxFile: string;

  procedure Add(const S: string);
  begin
    Msg.Add('• ' + S);
  end;

  function GetFileSizeBytes(const AFileName: string): Int64;
  var
    FS: TFileStream;
  begin
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      Result := FS.Size;
    finally
      FS.Free;
    end;
  end;

begin
  Msg := TStringList.Create;
  HasLayoutDrift := False;
  LayoutDriftReport := '';
  try
    ConfigDir := GetConfigurationDirectory;

    // 1) databases.reg — check record layout
    RegFile := ConfigDir + DatabasesRegFile;
    DbsRegExists := FileExists(RegFile);
    if DbsRegExists then
    begin
      FileLen := GetFileSizeBytes(RegFile);

      if (FileLen mod SizeOf(TRegisteredDatabase)) <> 0 then
      begin
        HasLayoutDrift := True;
        LayoutDriftReport := LayoutDriftReport +
          '  - databases.reg: ' +
          IntToStr(FileLen mod SizeOf(TRegisteredDatabase)) +
          ' bytes leftover' + sLineBreak;
      end;
    end;
    // Missing databases.reg alone is NOT an error
    // (fresh install / user hasn't registered any DB yet)

    // 2) servers.reg — check record layout
    RegFile := ConfigDir + ServersRegFile;
    SrvRegExists := FileExists(RegFile);
    if SrvRegExists then
    begin
      FileLen := GetFileSizeBytes(RegFile);

      if (FileLen mod SizeOf(TServerRecord)) <> 0 then
      begin
        HasLayoutDrift := True;
        LayoutDriftReport := LayoutDriftReport +
          '  - servers.reg: ' +
          IntToStr(FileLen mod SizeOf(TServerRecord)) +
          ' bytes leftover' + sLineBreak;
      end;
    end;

    // 2b) Inconsistency: servers.reg missing but databases.reg exists
    if (not SrvRegExists) and DbsRegExists then
      Add('servers.reg is missing while databases.reg exists — ' +
          'registry files are inconsistent. Please re-register your servers.');

    // 3) Report layout drift with recovery instructions
    if HasLayoutDrift then
    begin
      Add('Registry file layout mismatch detected:');
      Msg.Add(LayoutDriftReport);
      Add('This usually happens after a TurboBird upgrade that changed');
      Add('the internal record structure of the registry files.');
      Add('');
      Add('To fix this, close TurboBird and delete the following files:');
      Add('    ' + ConfigDir + DatabasesRegFile);
      Add('    ' + ConfigDir + ServersRegFile);
      Add('');
      Add('Then restart TurboBird and register your servers and databases again.');
      Add('(Your actual Firebird database files are NOT affected.)');
    end;

    // 4) Default transaction file present?
    TxFile := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
              + 'data' + PathDelim
              + 'transaction_presets' + PathDelim
              + DefTxFileName;

    if (DefTxFileName = '') or not FileExists(TxFile) then
      Add('Default transaction file is missing: ' + TxFile);

    // 5) Per registered DB: TxConfig + Params
    Bad := 0;
    for i := 0 to High(RegisteredDatabases) do
    begin
      if Trim(RegisteredDatabases[i].RegRec.TxConfig) = '' then
        Inc(Bad);

      if Assigned(RegisteredDatabases[i].IBTransaction) and
         (RegisteredDatabases[i].IBTransaction.Params.Count = 0) then
        Inc(Bad);
    end;

    if Bad > 0 then
      Add(IntToStr(Bad) + ' database(s) with empty transaction configuration');

    // Result
    if Msg.Count = 0 then
      Result := ''
    else
      Result := 'Startup check found problems:' + sLineBreak +
                sLineBreak + Msg.Text;
  finally
    Msg.Free;
  end;
end;

// ============================================================
// Prüft, ob ein Privileg-Code in einem Privilegien-String enthalten ist.
// APrivileges Format: 'S,I,U,SG' (Komma-getrennt).
// APriv: 'S', 'I', 'U', 'D', 'R', 'SG', 'IG', 'UG', 'DG', 'RG'.
//
// Semantik: Pos(APriv, APrivileges) > 0 — d.h. 'S' matcht auch 'SG',
// weil WITH GRANT OPTION die Basis-Berechtigung implizit enthält.
// Das entspricht der bisherigen Logik in TableManage.FillPermissions.
// ============================================================
function PermissionHas(const APrivileges, APriv: string): Boolean;
begin
  if APriv = '' then
    Exit(False);
  Result := Pos(APriv, APrivileges) > 0;
end;

// ============================================================
// Entfernt einen optionalen TABLE.-Präfix aus einem Node-Text.
// Wird von globalen Trigger-Listen verwendet (Format: TABLE.TRIGGER),
// damit der Edit/View-Code nur den reinen Trigger-Namen weiterreicht.
// Trigger-Namen sind DB-weit eindeutig — deshalb reicht der Suffix.
// ============================================================
function StripTablePrefix(const AName: string): string;
var
  P: Integer;
  S: string;
begin
  S := Trim(AName);
  P := Pos('.', S);
  if P > 0 then
    Result := Trim(Copy(S, P + 1, MaxInt))
  else
    Result := S;
end;

{ ==========================================================================
  Transfer-Statistik: Hilfsfunktionen und Formatierung
  ========================================================================== }
function CopyMethodToStr(M: TCopyMethod): string;
begin
  case M of
    cmLocal:              Result := 'INSERT...SELECT (Same Database only)';
    cmCrossExecuteBlock:  Result := 'EXECUTE BLOCK';
    cmLocalRowByRow:      Result := 'Row-by-Row (IBX)';
    cmCrossRowByRow:      Result := 'Row-by-Row (IBX)';
    cmFBIntf:             Result := 'Firebird API (FBIntf)';
    else
      Result := 'Unknown';
  end;
end;

function CopyStatsRowsPerSec(const Stats: TTransferStatistic): Double;
begin
  if Stats.ElapsedSeconds > 0 then
    Result := Stats.RowsProcessed / Stats.ElapsedSeconds
  else
    Result := 0;
end;

function FormatNumberEN(const AValue: Int64): string;
var
  S: string;
  i, Len: Integer;
begin
  S := IntToStr(AValue);
  Len := Length(S);
  Result := '';

  for i := 1 to Len do
  begin
    Result := Result + S[i];
    if ((Len - i) mod 3 = 0) and (i < Len) then
      Result := Result + ',';
  end;
end;

// ---------------------------------------------------------------------------
// Report-Titel je nach Art
// ---------------------------------------------------------------------------
function TransferKindToTitle(K: TTransferKind): string;
begin
  case K of
    tkCopy:        Result := 'COPY REPORT';
    tkExport:      Result := 'EXPORT REPORT';
    tkLoadFile:    Result := 'LOAD REPORT';
    tkLoadTable:   Result := 'LOAD REPORT';
    tkCreateTable: Result := 'CREATE TABLE REPORT';
  else
    Result := 'REPORT';
  end;
end;

// ---------------------------------------------------------------------------
// Environment-Sektion (gemeinsam für alle Report-Typen)
// ---------------------------------------------------------------------------
procedure AddEnvironmentSection(SL: TStringList; const Stats: TTransferStatistic);
var
  FS: TFormatSettings;
  SwapUsedMB: Int64;
begin
  // Englischer Format-Stil — unabhängig vom OS-Locale
  FS := DefaultFormatSettings;
  FS.DecimalSeparator  := '.';
  FS.ThousandSeparator := ',';

  SL.Add('Environment:');
  if Stats.ClientLibVersion <> '' then
    SL.Add('  Client Lib: ' + Stats.ClientLibVersion);
  SL.Add('  OS:        ' + Stats.SystemInfo.OSName);
  SL.Add('  CPU:       ' + Stats.SystemInfo.CPUModel);
  SL.Add('  Cores:     ' + IntToStr(Stats.SystemInfo.CPUCores));
  SL.Add('  RAM:       ' + FormatFloat('#,##0', Stats.SystemInfo.RAMTotalMB, FS) + ' MB');

  // NEU: Swap-Zeile — nur wenn das System überhaupt Swap hat
  if Stats.SystemInfo.SwapTotalMB > 0 then
  begin
    SwapUsedMB := Stats.SystemInfo.SwapTotalMB - Stats.SystemInfo.SwapFreeMB;
    SL.Add('  Swap:      ' +
      FormatFloat('#,##0', Stats.SystemInfo.SwapTotalMB, FS) + ' MB total / ' +
      FormatFloat('#,##0', SwapUsedMB, FS) + ' MB used');
  end;

  if Stats.SystemInfo.DiskPath <> '' then
  begin
    SL.Add('  Disk (' + ExtractFileName(Stats.SystemInfo.DiskPath) + '):');
    SL.Add('    Free:    ' + FormatFloat('#,##0', Stats.SystemInfo.DiskFreeMB, FS) + ' MB');
    SL.Add('    Total:   ' + FormatFloat('#,##0', Stats.SystemInfo.DiskTotalMB, FS) + ' MB');
    if Stats.SystemInfo.DiskType <> '' then
      SL.Add('    Type:    ' + Stats.SystemInfo.DiskType);
  end;
end;

// ---------------------------------------------------------------------------
// Hauptfunktion: Transfer-Report formatieren
// ---------------------------------------------------------------------------
function FormatTransferReport(const Stats: TTransferStatistic): string;
var
  SL: TStringList;
  RowsPerSec: Double;
  SwapDeltaMB: Int64;
  FS: TFormatSettings;

  function ComputeRowsPerSec: Double;
  begin
    if Stats.RowsPerSecond > 0 then
      Result := Stats.RowsPerSecond
    else if (Stats.LoadElapsedSeconds > 0) and (Stats.RowsProcessed > 0) then
      Result := Stats.RowsProcessed / Stats.LoadElapsedSeconds
    else if (Stats.ElapsedSeconds > 0) and (Stats.RowsProcessed > 0) then
      Result := Stats.RowsProcessed / Stats.ElapsedSeconds
    else
      Result := 0;
  end;

begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator  := '.';
  FS.ThousandSeparator := ',';

  SL := TStringList.Create;
  try
    SL.Add('═══════════════════════════════════════════════════');
    SL.Add('              ' + TransferKindToTitle(Stats.Kind));
    SL.Add('═══════════════════════════════════════════════════');
    SL.Add('');

    if Stats.Kind = tkCopy then
    begin
      SL.Add('Copy Method:');
      SL.Add('  ' + CopyMethodToStr(Stats.Method));
      SL.Add('');
    end;

    if (Stats.SourceFileName <> '') or
       (Stats.SourceServer <> '') or
       (Stats.SourceDatabase <> '') or
       (Stats.SourceTable <> '') then
    begin
      SL.Add('Source:');
      if Stats.SourceFileName <> '' then
      begin
        SL.Add('  File:      ' + ExtractFileName(Stats.SourceFileName));
        SL.Add('  Path:      ' + ExtractFilePath(Stats.SourceFileName));
      end;
      if Stats.SourceServer <> '' then
        SL.Add('  Server:    ' + Stats.SourceServer);
      if Stats.SourceDatabase <> '' then
        SL.Add('  Database:  ' + Stats.SourceDatabase);
      if Stats.SourceTable <> '' then
        SL.Add('  Table:     ' + Stats.SourceTable);
      if Stats.SourceKind <> '' then
        SL.Add('  Type:      ' + Stats.SourceKind);
      if Stats.SourceServerVersion <> '' then
        SL.Add('  Version:   ' + Stats.SourceServerVersion);
      if Stats.SourceFileFormat <> '' then
        SL.Add('  Format:    ' + Stats.SourceFileFormat);
      if Stats.SourceFileSize > 0 then
        SL.Add('  Size:      ' + FormatBytesSmart(Stats.SourceFileSize));
      SL.Add('');
    end;

    if (Stats.DestServer <> '') or
       (Stats.DestDatabase <> '') or
       (Stats.DestTable <> '') or
       (Stats.DestFileName <> '') then
    begin
      SL.Add('Destination:');
      if Stats.DestServer <> '' then
        SL.Add('  Server:    ' + Stats.DestServer);
      if Stats.DestDatabase <> '' then
        SL.Add('  Database:  ' + Stats.DestDatabase);
      if Stats.DestTable <> '' then
        SL.Add('  Table:     ' + Stats.DestTable);
      if Stats.DestKind <> '' then
        SL.Add('  Type:      ' + Stats.DestKind);
      if Stats.DestServerVersion <> '' then
        SL.Add('  Version:   ' + Stats.DestServerVersion);
      if Stats.DestFileName <> '' then
      begin
        SL.Add('  File:      ' + ExtractFileName(Stats.DestFileName));
        SL.Add('  Path:      ' + ExtractFilePath(Stats.DestFileName));
      end;
      if Stats.DestFileSize > 0 then
        SL.Add('  Size:      ' + FormatBytesSmart(Stats.DestFileSize));
      SL.Add('');
    end;

    if (Stats.BatchSize > 0) or
       Stats.UseRowRange or
       (Stats.OptionsExtra <> '') or
       (Stats.CSVOptions <> '') or
       (Stats.Kind in [tkCopy, tkExport, tkCreateTable]) then
    begin
      SL.Add('Options:');

      if Stats.OptionsExtra <> '' then
        SL.Add('  ' + Stats.OptionsExtra);

      if Stats.BatchSize > 0 then
      begin
        if Stats.Method = cmFBIntf then
          SL.Add('  Batch Memory:  ' + FormatNumberEN(Stats.BatchSize) + ' MB')
        else
          SL.Add('  Batch Size:    ' + FormatNumberEN(Stats.BatchSize));
      end;

      if Stats.UseRowRange then
        SL.Add(Format('  Row Range:     %s .. %s',
          [FormatNumberEN(Stats.FromRow), FormatNumberEN(Stats.ToRow)]));

      if Stats.Kind in [tkCopy, tkExport, tkCreateTable] then
      begin
        if Stats.FormulaUsed then
          SL.Add('  Formula Used:  Yes')
        else
          SL.Add('  Formula Used:  No');
      end;

      if Stats.CSVOptions <> '' then
      begin
        SL.Add('');
        SL.Add('  CSV Options:');
        SL.Add(Stats.CSVOptions);
      end;

      if (Stats.CSVBlockSizeBytes > 0) or
         (Stats.CSVBufferSizeBytes > 0) or
         (Stats.CSVRowWidthScanMode <> '') then
      begin
        SL.Add('');
        SL.Add('  Buffer Performance:');

        if Stats.CSVBlockSizeBytes > 0 then
        begin
          if Stats.CSVBlockAutoAdjusted then
            SL.Add(Format('    Block Size:            %s (%s Bytes) — auto-adjusted from %s',
              [FormatBytesSmart(Stats.CSVBlockSizeBytes),
               FormatNumberEN(Stats.CSVBlockSizeBytes),
               FormatBytesSmart(Stats.CSVBlockOriginalBytes)]))
          else
            SL.Add(Format('    Block Size:            %s (%s Bytes)',
              [FormatBytesSmart(Stats.CSVBlockSizeBytes),
               FormatNumberEN(Stats.CSVBlockSizeBytes)]));
        end;

        if Stats.CSVBufferSizeBytes > 0 then
        begin
          if Stats.CSVBufferAutoAdjusted then
            SL.Add(Format('    Buffer Size:           %s (%s Bytes) — auto-adjusted from %s',
              [FormatBytesSmart(Stats.CSVBufferSizeBytes),
               FormatNumberEN(Stats.CSVBufferSizeBytes),
               FormatBytesSmart(Stats.CSVBufferOriginalBytes)]))
          else
            SL.Add(Format('    Buffer Size:           %s (%s Bytes)',
              [FormatBytesSmart(Stats.CSVBufferSizeBytes),
               FormatNumberEN(Stats.CSVBufferSizeBytes)]));
        end;

        if Stats.CSVInitialCellCapacity > 0 then
          SL.Add('    Initial Cells:         ' + IntToStr(Stats.CSVInitialCellCapacity));

        if Stats.CSVRowWidthScanMode <> '' then
          SL.Add('    Row Width Scan Mode:   ' + Stats.CSVRowWidthScanMode);

        if Stats.CSVRowWidthScanRows > 0 then
        begin
          if Stats.CSVRowWidthScanIsCustom then
            SL.Add(Format('    Row Width Scan Rows:   %s (CustomScan)',
              [FormatNumberEN(Stats.CSVRowWidthScanRows)]))
          else
            SL.Add(Format('    Row Width Scan Rows:   %s (FullScan)',
              [FormatNumberEN(Stats.CSVRowWidthScanRows)]));
        end;

        if Stats.CSVRowWidthScanTime > 0 then
          SL.Add(Format('    Row Width Scan Time:   %.3f s', [Stats.CSVRowWidthScanTime]));

        if Stats.CSVMaxRowWidth > 0 then
          SL.Add(Format('    Max Row Width:         %s Bytes (%s)',
            [FormatNumberEN(Stats.CSVMaxRowWidth),
             FormatBytesSmart(Stats.CSVMaxRowWidth)]));

        if Stats.CSVRowWidthScanIsCustom then
          SL.Add('    Note:                  CustomScan only - later rows may be wider');
      end;

      SL.Add('');
    end;

    if Stats.PresetFileContent <> '' then
    begin
      SL.Add('Preset Definition:');
      if Stats.PresetName <> '' then
        SL.Add('  Name:  ' + Stats.PresetName);
      if Stats.PresetFileName <> '' then
        SL.Add('  File:  ' + Stats.PresetFileName);
      SL.Add('');
      SL.Add('----- BEGIN PRESET FILE ---------------------------------');
      SL.Add(Stats.PresetFileContent);
      SL.Add('----- END PRESET FILE -----------------------------------');
      SL.Add('');
    end;

    if Stats.FormulasApplied <> '' then
    begin
      SL.Add('Formulas Applied:');
      SL.Add(Stats.FormulasApplied);
      SL.Add('');
    end;

    // ======================================================================
    // RESULT — Scan / Load / Total getrennt, ehrliche Speed
    // ======================================================================
    SL.Add('Result:');
    SL.Add('  Rows:          ' + FormatNumberEN(Stats.RowsProcessed));
    if Stats.FieldsCount > 0 then
      SL.Add('  Fields:        ' + IntToStr(Stats.FieldsCount));

    if Stats.CSVRowWidthScanTime > 0 then
      SL.Add('  Scan Time:     ' + FormatDateTime('hh:nn:ss.zzz',
        Stats.CSVRowWidthScanTime / SecsPerDay));

    if Stats.LoadElapsedSeconds > 0 then
      SL.Add('  Load Time:     ' + FormatDateTime('hh:nn:ss.zzz',
        Stats.LoadElapsedSeconds / SecsPerDay));

    SL.Add('  Total Time:    ' + FormatDateTime('hh:nn:ss.zzz',
      Stats.ElapsedSeconds / SecsPerDay));

    RowsPerSec := ComputeRowsPerSec;
    if RowsPerSec > 0 then
      SL.Add('  Speed:         ' + FormatFloat('#,##0', RowsPerSec, FS) + ' rows/sec');
    if Stats.BytesPerSecond > 0 then
      SL.Add('  Data Rate:     ' + FormatBytesSmart(Round(Stats.BytesPerSecond)) + '/sec');

    // === MEMORY ===
    if Stats.PeakRAMUsageMB > 0 then
      SL.Add('  Peak RAM:      ' + FormatBytesSmart(Stats.PeakRAMUsageMB * 1024 * 1024));
    if Stats.PeakEstimatedMB > 0 then
      SL.Add('  Dataset Peak:  ' + FormatBytesSmart(Stats.PeakEstimatedMB * 1024 * 1024) +
             ' (estimated)');

    // === SWAP ===
    if (Stats.SwapTotalMB > 0) and (Stats.PeakSwapUsedMB > 0) then
    begin
      SwapDeltaMB := Stats.PeakSwapUsedMB - Stats.SwapStartUsedMB;
      if SwapDeltaMB > 100 then
        SL.Add('  Peak Swap:     ' + FormatBytesSmart(Stats.PeakSwapUsedMB * 1024 * 1024) +
               ' / ' + FormatBytesSmart(Stats.SwapTotalMB * 1024 * 1024) +
               ' (+' + FormatBytesSmart(SwapDeltaMB * 1024 * 1024) + ' during load)')
      else
        SL.Add('  Peak Swap:     ' + FormatBytesSmart(Stats.PeakSwapUsedMB * 1024 * 1024) +
               ' / ' + FormatBytesSmart(Stats.SwapTotalMB * 1024 * 1024));
    end;


    if Stats.ThrashingDetected then
      SL.Add('  Note:          Swapping detected — load was slower than RAM-only');

    SL.Add('');

    if Stats.CreateTableSQL <> '' then
    begin
      SL.Add('Table Structure:');
      SL.Add(Stats.CreateTableSQL);
      SL.Add('');
    end;

    AddEnvironmentSection(SL, Stats);

    SL.Add('');
    SL.Add('═══════════════════════════════════════════════════');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

//Transactions
function GetTransactionParamsAsString(ATrans: TIBTransaction): string;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(ATrans) then
  begin
    Result := 'Transaction is NIL';
    Exit;
  end;

  Result := 'Transaction: ' + ATrans.TransactionName + sLineBreak;
  Result := Result + 'Active: ' + BoolToStr(ATrans.InTransaction, True) + sLineBreak;
  Result := Result + 'Params Count: ' + IntToStr(ATrans.Params.Count) + sLineBreak;
  Result := Result + 'Params:' + sLineBreak;

  if ATrans.Params.Count = 0 then
    Result := Result + '  (empty - using server defaults)' + sLineBreak
  else
    for i := 0 to ATrans.Params.Count - 1 do
      Result := Result + '  ' + ATrans.Params[i] + sLineBreak;

  // Zusätzlich die Connection-Parameter (für den Kontext)
  if Assigned(ATrans.DefaultDatabase) then
  begin
    Result := Result + sLineBreak + 'Database Params:' + sLineBreak;
    for i := 0 to ATrans.DefaultDatabase.Params.Count - 1 do
      Result := Result + '  ' + ATrans.DefaultDatabase.Params[i] + sLineBreak;
  end;
end;


// ========================================================================
// Zentrale MessageBox-Funktionen für TurboBird
// (immer im Vordergrund, einheitliches Verhalten)
// ========================================================================

function ShowInfoDialog(const aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aMsg, mtInformation, [mbOK]);
end;

function ShowInfoDialog(const aCaption, aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aCaption, aMsg, mtInformation, [mbOK]);
end;

function ShowWarningDialog(const aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aMsg, mtWarning, [mbOK]);
end;

function ShowWarningDialog(const aCaption, aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aCaption, aMsg, mtWarning, [mbOK]);
end;

function ShowErrorDialog(const aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aMsg, mtError, [mbOK]);
end;

function ShowErrorDialog(const aCaption, aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aCaption, aMsg, mtError, [mbOK]);
end;

function ShowConfirmDialog(const aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aMsg, mtConfirmation, [mbYes, mbNo]);
end;

function ShowConfirmDialog(const aCaption, aMsg: string): TModalResult;
begin
  Result := ShowMessageDialog(aCaption, aMsg, mtConfirmation, [mbYes, mbNo]);
end;

function ShowMessageDialog(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TModalResult; overload;
var
  Dlg: TForm;
begin
  Dlg := CreateMessageDialog(aMsg, DlgType, Buttons);
  try
    Dlg.Position := poScreenCenter;
    Dlg.FormStyle := fsSystemStayOnTop;
    Dlg.PopupMode := pmExplicit;
    Dlg.PopupParent := Application.MainForm;
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function ShowMessageDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TModalResult; overload;
var
  Dlg: TForm;
begin
  Dlg := CreateMessageDialog(aCaption, aMsg, DlgType, Buttons);
  try
    Dlg.Position := poScreenCenter;
    Dlg.FormStyle := fsSystemStayOnTop;
    Dlg.PopupMode := pmExplicit;
    Dlg.PopupParent := Application.MainForm;
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function StringToTraceFlags(const S: string): TTraceFlags;
var
  SL: TStringList;
  i: Integer;
  FlagName: string;
begin
  Result := [];
  SL := TStringList.Create;
  try
    SL.CommaText := S;
    for i := 0 to SL.Count - 1 do
    begin
      FlagName := Trim(SL[i]);
      if SameText(FlagName, 'tfConnect')      then Include(Result, tfConnect)
      else if SameText(FlagName, 'tfTransact') then Include(Result, tfTransact)
      else if SameText(FlagName, 'tfQPrepare') then Include(Result, tfQPrepare)
      else if SameText(FlagName, 'tfQExecute') then Include(Result, tfQExecute)
      else if SameText(FlagName, 'tfQFetch')   then Include(Result, tfQFetch)
      else if SameText(FlagName, 'tfBlob')     then Include(Result, tfBlob)
      else if SameText(FlagName, 'tfStmt')     then Include(Result, tfStmt)
      else if SameText(FlagName, 'tfService')  then Include(Result, tfService)
      else if SameText(FlagName, 'tfError')    then Include(Result, tfError)
      else if SameText(FlagName, 'tfMisc')     then Include(Result, tfMisc)
      else if SameText(FlagName, 'tfAll')      then
        Result := [tfConnect,tfTransact,tfQPrepare,tfQExecute,tfQFetch,tfStmt,tfService,tfBlob,tfMisc,tfError];
    end;
  finally
    SL.Free;
  end;
end;

function TraceFlagsToString(Flags: TTraceFlags): string;
begin
  Result := '';
  if tfConnect  in Flags then Result := Result + 'tfConnect,';
  if tfTransact in Flags then Result := Result + 'tfTransact,';
  if tfQPrepare in Flags then Result := Result + 'tfQPrepare,';
  if tfQExecute in Flags then Result := Result + 'tfQExecute,';
  if tfQFetch   in Flags then Result := Result + 'tfQFetch,';
  if tfService  in Flags then Result := Result + 'tfService,';
  if tfBlob     in Flags then Result := Result + 'tfBlob,';
  if tfStmt     in Flags then Result := Result + 'tfStmt,';
  if tfError    in Flags then Result := Result + 'tfError,';
  if tfMisc     in Flags then Result := Result + 'tfMisc,';
  // Letztes Komma entfernen
  if Result <> '' then
    Delete(Result, Length(Result), 1);
end;

{ ============================================================
  CloneServerRegistry: Kopiert einen Server-Record.

  NEU (Alias-Unterstützung):
    - Quell-Server wird über ALIAS gesucht (eindeutig).
    - Ziel: nur ALIAS-Eindeutigkeit prüfen (ServerName darf
      mehrfach vorkommen — z.B. 7x 'localhost' mit
      unterschiedlichen Ports).
    - Protocol wird als Parameter übergeben, damit man von
      TCP nach Local (und umgekehrt) clonen kann.
    - Version wird zurückgesetzt (beim ersten Connect neu
      ermittelt).
  ============================================================ }
function CloneServerRegistry(
  const ASourceAlias: string;
  const ADestServerName: string;
  const ADestServerAlias: string;
  const ADestPort: string;
  const ADestProtocol: TProtocol
): Boolean;
var
  SourceRec: TServerRecord;
  DestRec: TServerRecord;
begin
  Result := False;

  // Quell-Server über Alias suchen
  SourceRec := GetServerRecordFromFileByAlias(ASourceAlias);
  if SourceRec.ServerName = '' then
  begin
    MessageDlg('Source server not found: ' + ASourceAlias, mtError, [mbOK], 0);
    Exit;
  end;

  // Nur ALIAS-Eindeutigkeit prüfen
  if not IsServerAliasUnique(ADestServerAlias, '', ADestServerName) then
  begin
    MessageDlg('Server alias "' + ADestServerAlias + '" already exists!' +
               sLineBreak + sLineBreak +
               'Server aliases must be system-wide unique. ' +
               'Server names (like localhost) may be reused.',
               mtError, [mbOK], 0);
    Exit;
  end;

  DestRec := SourceRec;
  DestRec.ServerName  := ADestServerName;
  DestRec.ServerAlias := ADestServerAlias;
  DestRec.Protocol    := ADestProtocol;

  // Version zurücksetzen – wird beim ersten Connect neu ermittelt
  DestRec.VersionString := '';
  DestRec.VersionMajor  := 0;
  DestRec.VersionMinor  := 0;

  if Trim(ADestPort) <> '' then
    DestRec.Port := ADestPort
  else
    DestRec.Port := '';

  SaveServerDataToFile(DestRec, '');
  Result := True;
end;

{ ============================================================
  CloneServerRegistryInteractive: Dialog für Server-Clone.

  NEU:
    - Protocol-ComboBox (TCP / Local / SPX / …)
    - Port-Feld wird bei Local deaktiviert
    - Alias-Eindeutigkeit prüfen (nicht ServerName)
    - Nach dem Clone: LoadRegisteredServers aufrufen, damit
      der neue Knoten mit korrektem ServerSession angelegt wird
    - Knoten-Text = Alias (konsistent zu LoadRegisteredServers)
  ============================================================ }
function CloneServerRegistryInteractive: Boolean;
var
  SourceAlias: string;
  DestServer, DestAlias, DestPort: string;
  DestProtocol: TProtocol;
  ServerList, AliasList: TStringList;
  i: Integer;
  Form: TForm;
  lblSource, lblDest, lblAlias, lblPort, lblProtocol: TLabel;
  cbSource, cbProtocol: TComboBox;
  edtDest, edtAlias, edtPort: TEdit;
  chkCloneDBs: TCheckBox;
  BtnOK, BtnCancel: TButton;
  SourceRec: TServerRecord;
  SelectedIdx: Integer;
  DBCount: integer;
  SelectedNode: TTreeNode;
begin
  Result := False;
  AliasList := nil;

  ServerList := GetServerListFromTreeView;
  if (ServerList = nil) or (ServerList.Count = 0) then
  begin
    MessageDlg('No servers registered.', mtWarning, [mbOK], 0);
    if Assigned(ServerList) then
      ServerList.Free;
    Exit;
  end;

  AliasList := GetServerAliasListFromTreeView;

  try
    SelectedIdx := 0;
    if Assigned(MainTreeView) and Assigned(MainTreeView.Selected) then
    begin
      SelectedNode := GetAncestorAtLevel(MainTreeView.Selected, 0);
      SourceAlias := GetServerAliasFromNode(SelectedNode);
      if SourceAlias <> '' then
        for i := 0 to AliasList.Count - 1 do
          if SameText(AliasList[i], SourceAlias) then
          begin
            SelectedIdx := i;
            Break;
          end;
    end;

    Form := TForm.Create(nil);
    try
      Form.Caption := 'Clone Server';
      Form.Width := 480;
      Form.Height := 400;
      Form.Position := poScreenCenter;
      Form.BorderStyle := bsDialog;
      Form.FormStyle := fsStayOnTop;

      // Source Server
      lblSource := TLabel.Create(Form);
      lblSource.Parent := Form;
      lblSource.Left := 20;
      lblSource.Top := 15;
      lblSource.Caption := 'Source Server:';

      cbSource := TComboBox.Create(Form);
      cbSource.Parent := Form;
      cbSource.Left := 20;
      cbSource.Top := 38;
      cbSource.Width := 420;
      cbSource.Style := csDropDownList;
      for i := 0 to AliasList.Count - 1 do
        cbSource.Items.Add(AliasList[i]);
      cbSource.ItemIndex := SelectedIdx;

      // New Server Name (ServerName)
      lblDest := TLabel.Create(Form);
      lblDest.Parent := Form;
      lblDest.Left := 20;
      lblDest.Top := 70;
      lblDest.Caption := 'New Server Name (e.g. localhost):';

      edtDest := TEdit.Create(Form);
      edtDest.Parent := Form;
      edtDest.Left := 20;
      edtDest.Top := 93;
      edtDest.Width := 420;
      edtDest.Text := 'localhost';

      // New Server Alias
      lblAlias := TLabel.Create(Form);
      lblAlias.Parent := Form;
      lblAlias.Left := 20;
      lblAlias.Top := 125;
      lblAlias.Caption := 'New Server Alias:';

      edtAlias := TEdit.Create(Form);
      edtAlias.Parent := Form;
      edtAlias.Left := 20;
      edtAlias.Top := 148;
      edtAlias.Width := 420;
      edtAlias.Text := Trim(cbSource.Text) + '_Clone';

      // Protocol
      lblProtocol := TLabel.Create(Form);
      lblProtocol.Parent := Form;
      lblProtocol.Left := 20;
      lblProtocol.Top := 180;
      lblProtocol.Caption := 'Protocol:';

      cbProtocol := TComboBox.Create(Form);
      cbProtocol.Parent := Form;
      cbProtocol.Left := 20;
      cbProtocol.Top := 203;
      cbProtocol.Width := 150;
      cbProtocol.Style := csDropDownList;
      cbProtocol.Items.Add('TCP');
      cbProtocol.Items.Add('Local');
      cbProtocol.Items.Add('SPX');
      cbProtocol.Items.Add('NamedPipe');
      cbProtocol.Items.Add('inet');
      cbProtocol.Items.Add('inet4');
      cbProtocol.Items.Add('inet6');
      cbProtocol.Items.Add('wnet');
      cbProtocol.Items.Add('xnet');
      cbProtocol.ItemIndex := 0;   // TCP default

      // Port
      lblPort := TLabel.Create(Form);
      lblPort.Parent := Form;
      lblPort.Left := 190;
      lblPort.Top := 180;
      lblPort.Caption := 'Port:';

      edtPort := TEdit.Create(Form);
      edtPort.Parent := Form;
      edtPort.Left := 190;
      edtPort.Top := 203;
      edtPort.Width := 100;

      // Port vom Quell-Server laden
      SourceRec := GetServerRecordFromFileByAlias(cbSource.Text);
      edtPort.Text := SourceRec.Port;

      // Initial: bei Local → Port leer
      if SameText(cbProtocol.Text, 'Local') then
      begin
        edtPort.Text := '';
        edtPort.Enabled := False;
      end;

      // Clone Databases Checkbox
      chkCloneDBs := TCheckBox.Create(Form);
      chkCloneDBs.Parent := Form;
      chkCloneDBs.Left := 20;
      chkCloneDBs.Top := 240;
      chkCloneDBs.Width := 380;
      chkCloneDBs.Caption := 'Clone all databases of this server';
      chkCloneDBs.Checked := False;

      // Buttons
      BtnOK := TButton.Create(Form);
      BtnOK.Parent := Form;
      BtnOK.Caption := 'OK';
      BtnOK.Left := 180;
      BtnOK.Top := 300;
      BtnOK.Width := 100;
      BtnOK.Default := True;
      BtnOK.ModalResult := mrOK;

      BtnCancel := TButton.Create(Form);
      BtnCancel.Parent := Form;
      BtnCancel.Caption := 'Cancel';
      BtnCancel.Left := 300;
      BtnCancel.Top := 300;
      BtnCancel.Width := 100;
      BtnCancel.ModalResult := mrCancel;

      if Form.ShowModal = mrOK then
      begin
        if (cbSource.ItemIndex >= 0) and (cbSource.ItemIndex < ServerList.Count) then
          SourceAlias := AliasList[cbSource.ItemIndex]
        else
          SourceAlias := AliasList[SelectedIdx];

        DestServer := Trim(edtDest.Text);
        DestAlias  := Trim(edtAlias.Text);
        DestPort   := Trim(edtPort.Text);

        // Protocol-Index → TProtocol
        case cbProtocol.ItemIndex of
          0: DestProtocol := TCP;
          1: DestProtocol := Local;
          2: DestProtocol := SPX;
          3: DestProtocol := NamedPipe;
          4: DestProtocol := inet;
          5: DestProtocol := inet4;
          6: DestProtocol := inet6;
          7: DestProtocol := wnet;
          8: DestProtocol := xnet;
        else
          DestProtocol := TCP;
        end;

        if DestServer = '' then
        begin
          MessageDlg('Server name cannot be empty.', mtWarning, [mbOK], 0);
          Exit;
        end;
        if DestAlias = '' then
        begin
          MessageDlg('Server alias cannot be empty.', mtWarning, [mbOK], 0);
          Exit;
        end;

        if CloneServerRegistry(SourceAlias, DestServer, DestAlias, DestPort, DestProtocol) then
        begin
          if chkCloneDBs.Checked then
            DBCount := CloneAllDatabasesForServer(SourceAlias, DestAlias)
          else
            DBCount := 0;

          // TreeView neu aufbauen – dann hat der Knoten korrekten ServerSession
          //fmMain.LoadRegisteredServers;
          fmMain.LoadRegisteredDatabases;

          MessageDlg(
            'Clone completed!' + sLineBreak +
            sLineBreak +
            'Server:   ' + DestAlias + sLineBreak +
            'Name:     ' + DestServer + sLineBreak +
            'Protocol: ' + cbProtocol.Text + sLineBreak +
            'Databases: ' + IntToStr(DBCount),
            mtInformation, [mbOK], 0);

          Result := True;
        end;
      end;

    finally
      Form.Free;
    end;

  finally
    ServerList.Free;
    if Assigned(AliasList) then
      AliasList.Free;
  end;
end;

function CloneAllDatabasesForServer(const ASourceServer, ADestServer: string): integer;
var
  i: Integer;
  SourceTitle: string;
  NewIndex: Integer;
  ServerNode, NewDBNode, DummyNode: TTreeNode;
  Count: Integer;
begin
  Count := 0;

  for i := 0 to High(RegisteredDatabases) do
  begin
    if SameText(RegisteredDatabases[i].RegRec.ServerName, ASourceServer) then
    begin
      SourceTitle := RegisteredDatabases[i].RegRec.Title;

      // DB-Registrierung kopieren (ohne Dialog, mit Default-Titel)
      NewIndex := CopyDBRegistry(i, ADestServer, '', False);

      if NewIndex >= 0 then
      begin
        Inc(Count);

        // TreeView aktualisieren
        ServerNode := GetServerNodeByServerName(ADestServer);
        if Assigned(ServerNode) then
        begin
          NewDBNode := MainTreeView.Items.AddChild(ServerNode, RegisteredDatabases[NewIndex].RegRec.Title);
          NewDBNode.ImageIndex := 3;
          NewDBNode.SelectedIndex := 3;

          if not Assigned(NewDBNode.Data) then
          begin
            NewDBNode.Data := AllocMem(SizeOf(TNodeInfos));
          end;

          TPNodeInfos(NewDBNode.Data)^.dbIndex := NewIndex;
          TPNodeInfos(NewDBNode.Data)^.ObjectType := tvotDatabase;
          TPNodeInfos(NewDBNode.Data)^.ServerSession := nil;
          TPNodeInfos(NewDBNode.Data)^.SimpleObjExtractor := nil;
          TPNodeInfos(NewDBNode.Data)^.UnIntelliSenseCache := nil;

          DummyNode := MainTreeView.Items.AddChild(NewDBNode, 'Loading...');
        end;
      end;
    end;
  end;

  Result := Count;
end;

function ShowServerSelectDialog(
  const ATitle: string;
  const ASuggestedDBTitle: string;
  out ASelectedServer: string;
  out ADBTitle: string
): Boolean;
var
  Form: TForm;
  ComboBox: TComboBox;
  edtDBTitle: TEdit;
  lblServer, lblTitle: TLabel;
  BtnOK, BtnCancel: TButton;
  ServerList: TStringList;
  i: Integer;
begin
  Result := False;
  ASelectedServer := '';
  ADBTitle := '';

  ServerList := GetServerListFromTreeView;
  if ServerList.Count = 0 then
  begin
    MessageDlg('No servers registered.', mtWarning, [mbOK], 0);
    ServerList.Free;
    Exit;
  end;

  Form := TForm.Create(nil);
  try
    Form.Caption := ATitle;
    Form.Width := 440;
    Form.Height := 250;
    Form.Position := poScreenCenter;
    Form.BorderStyle := bsDialog;
    Form.FormStyle := fsStayOnTop;

    // Server-Label
    lblServer := TLabel.Create(Form);
    lblServer.Parent := Form;
    lblServer.Left := 20;
    lblServer.Top := 15;
    lblServer.Caption := 'Destination Server:';

    // Server-ComboBox
    ComboBox := TComboBox.Create(Form);
    ComboBox.Parent := Form;
    ComboBox.Left := 20;
    ComboBox.Top := 40;
    ComboBox.Width := 390;
    ComboBox.Style := csDropDownList;
    for i := 0 to ServerList.Count - 1 do
      ComboBox.Items.Add(ServerList[i]);
    ComboBox.ItemIndex := 0;

    // Titel-Label
    lblTitle := TLabel.Create(Form);
    lblTitle.Parent := Form;
    lblTitle.Left := 20;
    lblTitle.Top := 80;
    lblTitle.Caption := 'Database Title:';

    // Titel-Edit
    edtDBTitle := TEdit.Create(Form);
    edtDBTitle.Parent := Form;
    edtDBTitle.Left := 20;
    edtDBTitle.Top := 105;
    edtDBTitle.Width := 390;
    edtDBTitle.Text := ASuggestedDBTitle;

    // Buttons
    BtnOK := TButton.Create(Form);
    BtnOK.Parent := Form;
    BtnOK.Caption := 'OK';
    BtnOK.Left := 140;
    BtnOK.Top := 155;
    BtnOK.Width := 100;
    BtnOK.Default := True;
    BtnOK.ModalResult := mrOK;

    BtnCancel := TButton.Create(Form);
    BtnCancel.Parent := Form;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 260;
    BtnCancel.Top := 155;
    BtnCancel.Width := 100;
    BtnCancel.ModalResult := mrCancel;

    if Form.ShowModal = mrOK then
    begin
      ASelectedServer := ComboBox.Text;
      ADBTitle := Trim(edtDBTitle.Text);
      if ADBTitle = '' then
        ADBTitle := ASuggestedDBTitle;
      Result := True;
    end;

  finally
    Form.Free;
    ServerList.Free;
  end;
end;

{ ============================================================
  ShowDBCloneDialog: Interaktiver Dialog für DB-Clone.

  VEREINFACHT:
    - Beide Checkboxen entfernt:
        "Create database on destination (empty)" — zu riskant
          (Ziel-DB muss exakt zur OPC passen, Path-Mapping,
           ODS-Version etc.) und in der Praxis selten genutzt.
        "Include metadata" — war ohnehin Visible := False.

    - Neue Signatur: nur noch ASelectedServer und ADBTitle als
      out-Parameter. Die Aufrufer müssen entsprechend angepasst
      werden (siehe CloneDBRegistryInteractive).

    - ComboBox zeigt weiterhin ALIASE
      (GetServerAliasListFromTreeView).

    - Vorauswahl: Server, unter dem die markierte DB hängt,
      wird vorausgewählt (über GetServerAliasFromNode).
  ============================================================ }
function ShowDBCloneDialog(
  const ATitle: string;
  const ASuggestedDBTitle: string;
  out ASelectedServer: string;
  out ADBTitle: string
): Boolean;
var
  Form: TForm;
  ComboBox: TComboBox;
  edtDBTitle: TEdit;
  lblServer, lblTitle: TLabel;
  BtnOK, BtnCancel: TButton;
  AliasList: TStringList;
  i, PreIdx: Integer;
  SelectedNode, ServerNode: TTreeNode;
  PreAlias: string;
begin
  Result := False;
  ASelectedServer := '';
  ADBTitle := '';

  AliasList := GetServerAliasListFromTreeView;
  if (AliasList = nil) or (AliasList.Count = 0) then
  begin
    if Assigned(AliasList) then
      AliasList.Free;
    MessageDlg('No servers registered.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // Vorauswahl ermitteln
  PreIdx := 0;
  PreAlias := '';
  if Assigned(MainTreeView) and Assigned(MainTreeView.Selected) then
  begin
    SelectedNode := MainTreeView.Selected;
    ServerNode := GetAncestorAtLevel(SelectedNode, 0);
    if Assigned(ServerNode) then
      PreAlias := GetServerAliasFromNode(ServerNode);
    if PreAlias <> '' then
    begin
      for i := 0 to AliasList.Count - 1 do
        if SameText(AliasList[i], PreAlias) then
        begin
          PreIdx := i;
          Break;
        end;
    end;
  end;

  Form := TForm.Create(nil);
  try
    Form.Caption := ATitle;
    Form.Width := 440;
    Form.Height := 230;
    Form.Position := poScreenCenter;
    Form.BorderStyle := bsDialog;
    Form.FormStyle := fsStayOnTop;

    // Server-Label
    lblServer := TLabel.Create(Form);
    lblServer.Parent := Form;
    lblServer.Left := 20;
    lblServer.Top := 15;
    lblServer.Caption := 'Destination Server:';

    // Server-ComboBox (zeigt Aliase)
    ComboBox := TComboBox.Create(Form);
    ComboBox.Parent := Form;
    ComboBox.Left := 20;
    ComboBox.Top := 40;
    ComboBox.Width := 390;
    ComboBox.Style := csDropDownList;
    for i := 0 to AliasList.Count - 1 do
      ComboBox.Items.Add(AliasList[i]);
    ComboBox.ItemIndex := PreIdx;

    // Titel-Label
    lblTitle := TLabel.Create(Form);
    lblTitle.Parent := Form;
    lblTitle.Left := 20;
    lblTitle.Top := 80;
    lblTitle.Caption := 'Database Title:';

    // Titel-Edit
    edtDBTitle := TEdit.Create(Form);
    edtDBTitle.Parent := Form;
    edtDBTitle.Left := 20;
    edtDBTitle.Top := 105;
    edtDBTitle.Width := 390;
    edtDBTitle.Text := ASuggestedDBTitle;

    // Buttons
    BtnOK := TButton.Create(Form);
    BtnOK.Parent := Form;
    BtnOK.Caption := 'OK';
    BtnOK.Left := 140;
    BtnOK.Top := 155;
    BtnOK.Width := 100;
    BtnOK.Default := True;
    BtnOK.ModalResult := mrOK;

    BtnCancel := TButton.Create(Form);
    BtnCancel.Parent := Form;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 260;
    BtnCancel.Top := 155;
    BtnCancel.Width := 100;
    BtnCancel.ModalResult := mrCancel;

    if Form.ShowModal = mrOK then
    begin
      ASelectedServer := ComboBox.Text;
      ADBTitle := Trim(edtDBTitle.Text);
      if ADBTitle = '' then
        ADBTitle := ASuggestedDBTitle;
      Result := True;
    end;

  finally
    Form.Free;
    AliasList.Free;
  end;
end;

// --------------------------------------------------------------------------
// Datenbank‑Registrierung kopieren (evtl. temporär)
// --------------------------------------------------------------------------
{ ============================================================
  Kopiert eine DB-Registrierung auf einen anderen Server.

  ADestServerName kann ALIAS oder ECHTER Servername sein.
  In DestRec.ServerName wird IMMER der ALIAS gespeichert
  (Fallback: echter Name, falls kein Alias vorhanden) —
  das ist der eindeutige Identifikator für die DB-Zuordnung.
  ============================================================ }
function CopyDBRegistry(
  ASourceDBIndex  : Integer;
  const ADestServerName : string;
  const ADestDBTitle : string = '';
  ATemporary      : Boolean = False
): Integer;
var
  SourceRec  : TRegisteredDatabase;
  DestRec    : TRegisteredDatabase;
  ServerRec  : TServerRecord;
  FileName   : string;
  F          : file of TRegisteredDatabase;
  NewTitle   : string;
  Counter    : Integer;
  DBFilePath : string;
  DestServerIdent : string;
begin
  Result := -1;

  if (ASourceDBIndex < 0) or (ASourceDBIndex >= Length(RegisteredDatabases)) then
    raise Exception.Create('CopyDBRegistry: Invalid source DBIndex');

  SourceRec := RegisteredDatabases[ASourceDBIndex].RegRec;

  // Alias-Lookup mit Fallback
  ServerRec := GetServerRecordFromFileByAlias(ADestServerName);
  if ServerRec.ServerName = '' then
    raise Exception.Create('CopyDBRegistry: Destination server not found: ' + ADestServerName);

  // Eindeutiger DB-Identifikator = Alias (Fallback: echter Name)
  if Trim(ServerRec.ServerAlias) <> '' then
    DestServerIdent := Trim(ServerRec.ServerAlias)
  else
    DestServerIdent := Trim(ServerRec.ServerName);

  DestRec := SourceRec;
  DestRec.ServerName := DestServerIdent;   // ← IMMER Alias
  DestRec.Deleted    := False;
  DestRec.LastOpened := Now;
  DestRec.ConnectOnApplicationStart := False;

  // Sicherheit: Passwort NICHT übernehmen
  DestRec.Password     := '';
  DestRec.SavePassword := False;
  DestRec.UserName     := SourceRec.UserName;
  DestRec.Role         := SourceRec.Role;

  // Server-Einstellungen übernehmen
  DestRec.IsEmbedded            := ServerRec.IsEmbedded;
  DestRec.FireBirdClientLibPath := ServerRec.ClientLibraryPath;
  DestRec.OverwriteLoadedClientLib := False;
  DestRec.Port                  := ServerRec.Port;
  DestRec.ServerVersionMajor    := ServerRec.VersionMajor;
  DestRec.ServerVersionMinor    := ServerRec.VersionMinor;
  DestRec.ServerVersionString   := ServerRec.VersionString;

  // Connection-String je nach Ziel-Server-Typ anpassen
  DBFilePath := GetDBFileNameFromConnectionString(SourceRec.DatabaseName);

  if ServerRec.IsEmbedded then
  begin
    DestRec.DatabaseName := DBFilePath;

    if not FileExists(DBFilePath) then
    begin
      if MessageDlg(
           'The destination server is a Local/Embedded server.' + sLineBreak +
           sLineBreak +
           'The database file does NOT exist at:' + sLineBreak +
           DBFilePath + sLineBreak +
           sLineBreak +
           'TurboBird will NOT copy the file automatically.' + sLineBreak +
           'Please ensure the file exists before connecting.' + sLineBreak +
           sLineBreak +
           'Continue with registration anyway?',
           mtWarning, [mbYes, mbNo], 0) <> mrYes then
        Exit;
    end;
  end
  else if (ServerRec.Port <> '') then
    DestRec.DatabaseName := ServerRec.ServerName + '/' + ServerRec.Port + ':' + DBFilePath
  else
    DestRec.DatabaseName := ServerRec.ServerName + ':' + DBFilePath;

  // Titel eindeutig machen
  if ADestDBTitle <> '' then
    NewTitle := ADestDBTitle
  else
    NewTitle := SourceRec.Title;

  Counter := 1;
  while FindDBIndexByTitleAndServer(NewTitle, DestServerIdent) >= 0 do
  begin
    Inc(Counter);
    if ADestDBTitle <> '' then
      NewTitle := ADestDBTitle + '_' + IntToStr(Counter)
    else
      NewTitle := SourceRec.Title + '_' + IntToStr(Counter);
  end;
  DestRec.Title := NewTitle;

  // In Datei speichern
  FileName := GetConfigurationDirectory + DatabasesRegFile;
  AssignFile(F, FileName);
  try
    if FileExists(FileName) then
    begin
      FileMode := 2;
      Reset(F);
      Seek(F, System.FileSize(F));
    end
    else
      Rewrite(F);

    Write(F, DestRec);
    CloseFile(F);
  except
    try CloseFile(F); except end;
    raise;
  end;

  // Ins RegisteredDatabases-Array aufnehmen
  SetLength(RegisteredDatabases, Length(RegisteredDatabases) + 1);
  with RegisteredDatabases[High(RegisteredDatabases)] do
  begin
    RegRec     := DestRec;
    OrigRegRec := DestRec;
    Index      := High(RegisteredDatabases);

    IBDatabase := TIBDatabase.Create(nil);
    IBDatabase.DatabaseName := DestRec.DatabaseName;
    IBDatabase.FirebirdLibraryPathName := DestRec.FireBirdClientLibPath;
    IBDatabase.LoginPrompt := True;
    IBDatabase.OnLogin := @dmSysTables.OnDatabaseLogin;

    IBTransaction := TIBTransaction.Create(nil);
    IBTransaction.DefaultDatabase := IBDatabase;
    IBDatabase.DefaultTransaction := IBTransaction;

    IBQuery := TIBQuery.Create(nil);
    IBQuery.Database := IBDatabase;
    IBQuery.Transaction := IBTransaction;

    IBDatabaseInfo := TIBDatabaseInfo.Create(nil);
    IBDatabaseInfo.Database := IBDatabase;
  end;

  Result := High(RegisteredDatabases);
end;

{ ============================================================
  CreateEmptyDatabase: Erstellt eine leere DB auf dem Ziel-Server.

  BUGFIX:
    - ServiceConn.FirebirdLibraryPathName wird jetzt explizit
      auf die Client-Library des Ziel-Servers gesetzt. Vorher
      blieb sie leer → globale FB-6.0-Lib wurde verwendet →
      CreateDatabase mit lokalem Pfad öffnete die DB als
      EMBEDDED im TB-Prozess → DB blieb offen → jeder spätere
      TCP-Zugriff scheiterte mit "already opened with engine
      instance".

    - Nach CreateDatabase wird die ServiceConn explizit
      getrennt, damit keine Verbindung offen bleibt.

    - Der Embedded-Pfad (ServerRecord.IsEmbedded = True)
      bleibt unverändert – dort ist Embedded-Öffnung gewollt,
      und die DB wird nach dem Create explizit geschlossen.

  HINWEIS:
    Wenn der Ziel-Server eine andere Firebird-Version hat als
    die Quell-DB, schlägt der spätere Zugriff mit
    "wrong ODS version" fehl. Das ist ein Test-Setup-Problem,
    kein Bug in dieser Funktion.
  ============================================================ }
function CreateEmptyDatabase(const AServerName, ADBTitle: string): Boolean;
var
  DBIndex: Integer;
  {$ifdef windows}
  DBFilePath: PChar;
  {$else}
  DBFilePath: string;
  {$endif}
  ServerRecord: TServerRecord;
  ServiceConn: TIBXServicesConnection;
  DPB: IDPB;
  DBName: string;
  ResolvedServerName: string;
begin
  Result := False;

  ServerRecord := GetServerRecordFromFileByAlias(AServerName);
  if ServerRecord.ServerName = '' then
    Exit;

  if Trim(ServerRecord.ServerAlias) <> '' then
    ResolvedServerName := Trim(ServerRecord.ServerAlias)
  else
    ResolvedServerName := Trim(ServerRecord.ServerName);

  DBIndex := FindDBIndexByTitleAndServer(ADBTitle, ResolvedServerName);
  if DBIndex < 0 then Exit;

  {$ifdef windows}
  DBFilePath := PChar(GetDBFileNameFromConnectionString(
                  RegisteredDatabases[DBIndex].IBDatabase.DatabaseName));
  {$else}
  DBFilePath := GetDBFileNameFromConnectionString(
                  RegisteredDatabases[DBIndex].IBDatabase.DatabaseName);
  {$endif}

  if ServerRecord.IsEmbedded then
  begin
    with RegisteredDatabases[DBIndex] do
    begin
      ForceDirectories(ExtractFilePath(DBFilePath));
      if FileExists(DBFilePath) then
        DeleteFile(DBFilePath);

      IBDatabase.CreateIfNotExists := True;
      try
        IBDatabase.Connected := True;
        IBDatabase.Connected := False;
        Result := FileExists(DBFilePath);
      finally
        IBDatabase.CreateIfNotExists := False;
      end;
    end;
  end
  else
  begin
    ServiceConn := TIBXServicesConnection.Create(nil);
    try
      ServiceConn.ServerName := ServerRecord.ServerName;
      ServiceConn.PortNo := ServerRecord.Port;
      ServiceConn.Protocol := ServerRecord.Protocol;

      // WICHTIG: Client-Library des Ziel-Servers setzen, sonst
      // nutzt die API die globale (falsche) Lib und öffnet die
      // DB als Embedded.
      ServiceConn.FirebirdLibraryPathName := ServerRecord.ClientLibraryPath;

      ServiceConn.Params.Add('user_name=' + ServerRecord.UserName);
      ServiceConn.Params.Add('password=' + ServerRecord.Password);
      ServiceConn.LoginPrompt := False;
      ServiceConn.Connected := True;

      DPB := ServiceConn.FirebirdAPI.AllocateDPB;
      DPB.Add(isc_dpb_user_name).AsString := RegisteredDatabases[DBIndex].RegRec.UserName;
      DPB.Add(isc_dpb_password).AsString := RegisteredDatabases[DBIndex].RegRec.Password;
      DPB.Add(isc_dpb_page_size).AsInteger := 8192;
      DPB.Add(isc_dpb_set_db_charset).AsString := 'UTF8';

      DBName := RegisteredDatabases[DBIndex].RegRec.DatabaseName;

      ServiceConn.FirebirdAPI.CreateDatabase(DBName, DPB);
      Result := True;
    finally
      // Explizit trennen, damit keine Verbindung offen bleibt
      try
        if Assigned(ServiceConn) then
          if ServiceConn.Connected then
            ServiceConn.Connected := False;
      except
      end;
      ServiceConn.Free;
    end;
  end;
end;

{ ============================================================
  CreateDatabaseWithMetadata: Erstellt Ziel-DB inkl. Metadaten.

  BUGFIX:
    - Nach der Metadaten-Kopie wird die Verbindung zur
      Ziel-DB explizit geschlossen (IBDatabase.Connected := False).
      Vorher blieb sie offen, wenn sie vorher schon verbunden
      war – dann blockierte sie spätere Zugriffe.
    - Defensive Prüfung: DBWasConnected wird korrekt
      zurückgesetzt, damit der Aufrufer den ursprünglichen
      Zustand wiederherstellen kann.
  ============================================================ }
function CreateDatabaseWithMetadata(ASourceDBIndex, ADestDBIndex: Integer): Boolean;
var
  Extractor: TSimpleObjExtractor;
  Metadata: TStringList;
  Script: TIBXScript;
  DBWasConnected: boolean;
  ServerRecord: TServerRecord;
  DBFilePath: string;
  DBExistedBefore: Boolean;
begin
  Result := False;
  DBExistedBefore := False;

  ServerRecord := GetServerRecordFromFileByAlias(
    RegisteredDatabases[ADestDBIndex].RegRec.ServerName);

  if ServerRecord.IsEmbedded then
  begin
    DBFilePath := GetDBFileNameFromConnectionString(
      RegisteredDatabases[ADestDBIndex].IBDatabase.DatabaseName);

    if FileExists(DBFilePath) then
      DBExistedBefore := True;
  end;

  if not CreateEmptyDatabase(
       RegisteredDatabases[ADestDBIndex].RegRec.ServerName,
       RegisteredDatabases[ADestDBIndex].RegRec.Title) then
    Exit;

  if not ConnectToDBAs(ADestDBIndex, True) then
  begin
    MessageDlg(
      'Could not connect to the destination database.' + sLineBreak +
      sLineBreak +
      'The database exists, but metadata could not be copied.' + sLineBreak +
      'You can connect to it manually later.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  if DBExistedBefore then
  begin
    Result := True;
    Exit;
  end;

  Metadata := TStringList.Create;
  Extractor := TSimpleObjExtractor.Create(ASourceDBIndex);
  try
    Extractor.Extract(otDatabase, '', [], AlwaysQuoteIdentifiers, TStrings(Metadata));

    Script := TIBXScript.Create(nil);
    try
      Script.Database := RegisteredDatabases[ADestDBIndex].IBDatabase;

      DBWasConnected := RegisteredDatabases[ADestDBIndex].IBDatabase.Connected;
      if not DBWasConnected then
        RegisteredDatabases[ADestDBIndex].IBDatabase.Connected := true;

      Script.Transaction := RegisteredDatabases[ADestDBIndex].IBTransaction;

      if not Script.Transaction.InTransaction then
        Script.Transaction.StartTransaction;

      Script.ExecSQLScript(Metadata.Text);
      Script.Transaction.Commit;

      Result := True;
    finally
      Script.Free;
    end;
  finally
    // WICHTIG: Nach der Kopie die Ziel-DB immer schließen,
    // damit sie nicht als Embedded-Verbindung offen bleibt
    // und spätere TCP-Zugriffe blockiert.
    if Assigned(RegisteredDatabases[ADestDBIndex].IBTransaction) then
      if RegisteredDatabases[ADestDBIndex].IBTransaction.InTransaction then
        try RegisteredDatabases[ADestDBIndex].IBTransaction.Rollback; except end;

    if Assigned(RegisteredDatabases[ADestDBIndex].IBDatabase) then
      if RegisteredDatabases[ADestDBIndex].IBDatabase.Connected then
        try RegisteredDatabases[ADestDBIndex].IBDatabase.Connected := False; except end;

    Metadata.Free;
    Extractor.Free;
  end;
end;

{ ============================================================
  CloneDBRegistryInteractive: Interaktiver DB-Clone-Aufruf.

  VEREINFACHT:
    - Keine CreateDB- oder WithMetadata-Logik mehr.
      CreateEmptyDatabase und CreateDatabaseWithMetadata
      bleiben in der Unit erhalten (könnten anderswo genutzt
      werden), werden aber hier nicht mehr aufgerufen.

    - Nach dem Klonen erscheint eine INFO-MELDUNG, die den
      User ausdrücklich darauf hinweist, die geklonte
      Registrierung am Ziel anzupassen. Grund:
        - Der DB-Pfad im geklonten Record zeigt noch auf
          die Original-Datei. Wenn die Ziel-DB woanders
          liegt, muss der Pfad angepasst werden.
        - Wenn Quell- und Ziel-Server unterschiedliche
          Firebird-Versionen haben, kann die DB nicht
          geöffnet werden (ODS-Version). Der User wird
          darauf hingewiesen, den Clone zu prüfen.
        - Connection-Charset kann vom Original abweichen.

    - TreeView-Update über GetServerNodeByAlias
      (funktioniert auch bei Alias-Anzeige).
  ============================================================ }
function CloneDBRegistryInteractive(ANodeInfos: TPNodeInfos): Boolean;
var
  DestServerName, DestDBTitle: string;
  NewIndex: Integer;
  SourceDBName: string;
  ServerNode, NewDBNode, DummyNode: TTreeNode;
begin
  Result := False;
  if not Assigned(ANodeInfos) then Exit;

  SourceDBName := RegisteredDatabases[ANodeInfos^.dbIndex].RegRec.Title;

  // Vereinfachter Clone-Dialog (nur Ziel-Server + Titel)
  if not ShowDBCloneDialog(
       'Clone Database Registration',
       SourceDBName + '_Clone',
       DestServerName,
       DestDBTitle
     ) then Exit;

  try
    NewIndex := CopyDBRegistry(ANodeInfos^.dbIndex, DestServerName, DestDBTitle, False);

    if NewIndex >= 0 then
    begin
      // TreeView aktualisieren — Knoten über Alias suchen
      ServerNode := GetServerNodeByAlias(DestServerName);
      if Assigned(ServerNode) then
      begin
        if (ServerNode.Count > 0) and (ServerNode.Items[0].Text = 'Loading...') then
          ServerNode.Items[0].Delete;

        NewDBNode := MainTreeView.Items.AddChild(ServerNode, RegisteredDatabases[NewIndex].RegRec.Title);
        NewDBNode.ImageIndex := 3;
        NewDBNode.SelectedIndex := 3;

        if not Assigned(NewDBNode.Data) then
          NewDBNode.Data := AllocMem(SizeOf(TNodeInfos));

        TPNodeInfos(NewDBNode.Data)^.dbIndex := NewIndex;
        TPNodeInfos(NewDBNode.Data)^.ObjectType := tvotDatabase;
        TPNodeInfos(NewDBNode.Data)^.ServerSession := nil;
        TPNodeInfos(NewDBNode.Data)^.SimpleObjExtractor := nil;
        TPNodeInfos(NewDBNode.Data)^.UnIntelliSenseCache := nil;

        DummyNode := MainTreeView.Items.AddChild(NewDBNode, 'Loading...');
        ServerNode.Expand(False);
      end
      else
      begin
        MessageDlg(
          'Clone successful, but the server node "' + DestServerName + '" ' +
          'could not be found in the TreeView.' + sLineBreak +
          'Please reload the database list (Ctrl+R or restart).',
          mtWarning, [mbOK], 0);
      end;

      // Erweiterte Erfolgsmeldung mit Hinweis auf Anpassung
      MessageDlg(
        'Database registration cloned successfully!' + sLineBreak +
        sLineBreak +
        'Source:      ' + SourceDBName + sLineBreak +
        'Destination: ' + DestDBTitle + sLineBreak +
        'Server:      ' + DestServerName + sLineBreak +
        sLineBreak +
        'IMPORTANT — please review the cloned registration:' + sLineBreak +
        sLineBreak +
        '  • Database path still points to the original file.' + sLineBreak +
        '    Edit the registration if the target DB lives elsewhere.' + sLineBreak +
        sLineBreak +
        '  • If source and target servers have different' + sLineBreak +
        '    Firebird versions, the database may not open' + sLineBreak +
        '    (ODS version incompatibility).' + sLineBreak +
        sLineBreak +
        '  • Connection charset may differ from the original.' + sLineBreak +
        sLineBreak +
        'Right-click the cloned entry and choose' + sLineBreak +
        '"Edit Database Registration" to verify settings.',
        mtInformation, [mbOK], 0);

      Result := True;
    end;

  except
    on E: Exception do
    begin
      MessageDlg('Clone failed:' + sLineBreak + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

{ ============================================================
  Baut eine DB-Verbindung auf, inkl. Credentials, Session-Cache
  und Transaktionsstart.

  NEU: Server-Lookup für IsLocal über GetServerRecordFromFileByAlias
  statt GetServerRecordFromFileByName. Rec.ServerName kann seit
  der Alias-Umstellung ein Alias sein — GetServerRecordFromFileByName
  würde dann nichts finden und IsLocal bliebe fälschlich False.
  Folge: Embedded-Server würden ein Passwort verlangen, obwohl
  sie keins brauchen.
  ============================================================ }
procedure ConnectDBPrepared(ADatabase: TIBDatabase; ATransaction: TIBTransaction; ADBIndex: Integer;
                            ATxParams: TStrings);
var
  Rec: TRegisteredDatabase;
  ServerRecord: TServerRecord;
  CachedPwd: string;
  ErrMsg: string;
  IsLocal: Boolean;
begin
  ErrMsg := '';

  try
    // 1. Validierung
    if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then
      raise Exception.Create('Invalid DBIndex: ' + IntToStr(ADBIndex));

    Rec := RegisteredDatabases[ADBIndex].RegRec;

    // 2. Disconnect (Fehler ignorieren)
    if Assigned(ATransaction) and ATransaction.InTransaction then
      try ATransaction.Rollback; except end;

    if Assigned(ADatabase) and ADatabase.Connected then
      try ADatabase.Connected := false; except end;

    // 3. Prüfen ob der SERVER Local/Embedded ist.
    //    Alias-Lookup mit Fallback auf echten Servernamen.
    ServerRecord := GetServerRecordFromFileByAlias(Rec.ServerName);
    IsLocal := ServerRecord.IsEmbedded;

    // 4. Credentials setzen
    ADatabase.Params.Values['user_name'] := Rec.UserName;

    if Rec.Password <> '' then
      ADatabase.Params.Values['password'] := Rec.Password
    else
    begin
      CachedPwd := GetDBSessionPassword(Rec.ServerName, Rec.DatabaseName);
      if CachedPwd <> '' then
        ADatabase.Params.Values['password'] := CachedPwd
      else
      begin
        // Bei Local/Embedded: Dummy-Passwort für den Cache
        if IsLocal then
        begin
          ADatabase.Params.Values['password'] := 'embedded_local';
          SetDBSessionPassword(Rec.ServerName, Rec.DatabaseName, 'embedded_local');
        end
        else
          ADatabase.Params.Values['password'] := '';
      end;
    end;

    if Rec.Role <> '' then
      ADatabase.Params.Values['sql_role_name'] := Rec.Role;
    if Rec.Charset <> '' then
      ADatabase.Params.Values['lc_ctype'] := Rec.Charset;

    // 5. OnLogin-Handler + LoginPrompt
    ADatabase.OnLogin := @dmSysTables.OnDatabaseLogin;
    ADatabase.LoginPrompt := True;

    // 6. Connect
    if not ADatabase.Connected then
      ADatabase.Connected := true;

    // 7. Nach erfolgreicher Verbindung Session-Cache füllen.
    //    Nur wenn das Passwort NICHT im RegRec gespeichert ist.
    if (not Rec.SavePassword) and (ADatabase.Params.Values['password'] <> '') then
    begin
      SetDBSessionPassword(Rec.ServerName, Rec.DatabaseName,
                           ADatabase.Params.Values['password']);
      SetServerSessionPassword(Rec.ServerName,
                               ADatabase.Params.Values['password']);
    end;

    // 8. Transaktionsparameter zuweisen (falls übergeben), dann starten
    if Assigned(ATransaction) then
    begin
      if Assigned(ATxParams) and (ATxParams.Count > 0) then
        ATransaction.Params.Assign(ATxParams)
      else if ATransaction.Params.Count = 0 then
        ATransaction.Params.LoadFromFile(
          IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
          + 'data' + PathDelim
          + 'transaction_presets' + PathDelim
          + DefTxFileName
        );

      if not ATransaction.InTransaction then
        ATransaction.StartTransaction;
    end;

  except
    on E: Exception do
    begin
      if (Rec.Password = '') and (ADBIndex >= 0) then
        ClearDBSessionPassword(Rec.ServerName, Rec.DatabaseName);

      ErrMsg := 'ConnectDBPrepared failed:' + sLineBreak +
                '  Server: ' + Rec.ServerName + sLineBreak +
                '  Database: ' + Rec.DatabaseName + sLineBreak +
                '  Error: ' + E.Message;
      raise Exception.Create(ErrMsg);
    end;
  end;
end;

// ============================================================================
// Session-Cache für temporäre Passwörter (nicht in Datei gespeichert)
// ============================================================================
function GetServerSessionPassword(const AServerName: string): string;
var
  idx: Integer;
begin
  Result := '';
  if not Assigned(SessionPasswordCache) then
    Exit;

  idx := SessionPasswordCache.IndexOfName('SRV_' + UpperCase(AServerName));
  if idx >= 0 then
    Result := SessionPasswordCache.ValueFromIndex[idx];
end;

procedure SetServerSessionPassword(const AServerName, APassword: string);
var
  idx: Integer;
  Key: string;
begin
  if not Assigned(SessionPasswordCache) then
    SessionPasswordCache := TStringList.Create;

  Key := 'SRV_' + UpperCase(AServerName);
  idx := SessionPasswordCache.IndexOfName(Key);
  if idx >= 0 then
    SessionPasswordCache[idx] := Key + '=' + APassword
  else
    SessionPasswordCache.Add(Key + '=' + APassword);
end;

procedure ClearServerSessionPassword(const AServerName: string);
var
  idx: Integer;
begin
  if not Assigned(SessionPasswordCache) then
    Exit;

  idx := SessionPasswordCache.IndexOfName('SRV_' + UpperCase(AServerName));
  if idx >= 0 then
    SessionPasswordCache.Delete(idx);
end;

{ ============================================================
  Liefert das Session-Passwort für eine DB.

  Sucht zuerst im Cache (Key: DB_<Servername>_<DatabaseName>).
  Wenn nichts im Cache: prüft, ob der SERVER Embedded ist —
  dann wird 'embedded_local' als Dummy zurückgegeben und
  gecacht, weil Embedded-Verbindungen kein Passwort brauchen.

  NEU: Server-Lookup über GetServerRecordFromFileByAlias statt
  GetServerRecordFromFileByName. Rec.ServerName kann seit der
  Alias-Umstellung ein Alias sein — GetServerRecordFromFileByName
  würde dann nichts finden und IsEmbedded bliebe fälschlich
  False. GetServerRecordFromFileByAlias findet beides (Alias
  in Pass 1, echter Servername in Pass 2).
  ============================================================ }
function GetDBSessionPassword(const AServerName, ADatabaseName: string): string;
var
  idx: Integer;
  ServerRecord: TServerRecord;
begin
  Result := '';

  // 1. Im Session-Cache nachsehen
  if Assigned(SessionPasswordCache) then
  begin
    idx := SessionPasswordCache.IndexOfName('DB_' + UpperCase(AServerName) + '_' + UpperCase(ADatabaseName));
    if idx >= 0 then
    begin
      Result := SessionPasswordCache.ValueFromIndex[idx];
      Exit;
    end;
  end;

  // 2. Wenn nichts im Cache → prüfen ob der SERVER Local/Embedded ist.
  //    Alias-Lookup mit Fallback auf echten Servernamen.
  ServerRecord := GetServerRecordFromFileByAlias(AServerName);
  if ServerRecord.IsEmbedded then
  begin
    Result := 'embedded_local';
    SetDBSessionPassword(AServerName, ADatabaseName, 'embedded_local');
  end;
end;

function GetDBSessionPasswordByIndex(ADBIndex: Integer): string;
begin
  Result := '';
  if (ADBIndex < 0) or (ADBIndex > High(RegisteredDatabases)) then
    Exit;

  Result := GetDBSessionPassword(
    RegisteredDatabases[ADBIndex].RegRec.ServerName,
    RegisteredDatabases[ADBIndex].RegRec.DatabaseName);
end;

procedure SetDBSessionPassword(const AServerName, ADatabaseName, APassword: string);
var
  idx: Integer;
  Key: string;
begin
  if not Assigned(SessionPasswordCache) then
    SessionPasswordCache := TStringList.Create;

  Key := 'DB_' + UpperCase(AServerName) + '_' + UpperCase(ADatabaseName);
  idx := SessionPasswordCache.IndexOfName(Key);
  if idx >= 0 then
    SessionPasswordCache[idx] := Key + '=' + APassword
  else
    SessionPasswordCache.Add(Key + '=' + APassword);
end;

procedure ClearDBSessionPassword(const AServerName, ADatabaseName: string);
var
  idx: Integer;
begin
  if not Assigned(SessionPasswordCache) then
    Exit;

  idx := SessionPasswordCache.IndexOfName('DB_' + UpperCase(AServerName) + '_' + UpperCase(ADatabaseName));
  if idx >= 0 then
    SessionPasswordCache.Delete(idx);
end;

procedure ClearAllSessionPasswords;
begin
  if Assigned(SessionPasswordCache) then
    SessionPasswordCache.Clear;
end;

// ============================================================================
// Nach erfolgreichem Connect: Passwort in den Session-Cache,
// wenn der User es nicht dauerhaft speichern wollte.
//
// Analog zu tvMainExpanding in main.pas — die zentrale Stelle, damit alle
// Formen (CloneTable, CSVExport, QueryWindow, ...) dasselbe Verhalten haben:
// Passwort einmal eingeben, danach für die ganze Session im Cache.
// ============================================================================
procedure CachePasswordAfterConnect(ADBIndex: Integer; ADatabase: TIBDatabase);
var
  Pwd: string;
begin
  if (ADBIndex < 0) or (ADBIndex > High(RegisteredDatabases)) then Exit;
  if not Assigned(ADatabase) then Exit;
  if not ADatabase.Connected then Exit;

  Pwd := ADatabase.Params.Values['password'];
  if Pwd = '' then Exit;

  // Wenn User "Save password" wollte → das ist schon persistent.
  // Nur cachen, was NICHT auf Platte geschrieben wurde.
  if RegisteredDatabases[ADBIndex].RegRec.SavePassword then Exit;

  // In den Session-Cache
  SetDBSessionPassword(
    RegisteredDatabases[ADBIndex].RegRec.ServerName,
    RegisteredDatabases[ADBIndex].RegRec.DatabaseName,
    Pwd);

  // RegRec in-memory auch aktualisieren, damit OnDatabaseLogin
  // beim nächsten Aufruf schon beim zweiten Check (DB-Cache) trifft.
  RegisteredDatabases[ADBIndex].RegRec.Password := Pwd;
end;
//////////////////////////////////////////////////

function GetFormsBasePath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
            + 'data' + PathDelim + 'PSStudio' + PathDelim + 'Forms' + PathDelim;
end;

function GetDBFormsPath(const AServerName, ADBAlias: string): string;
begin
  Result := GetFormsBasePath
            + SanitizeFileName(AServerName) + PathDelim
            + SanitizeFileName(ADBAlias) + PathDelim;
end;

function SanitizeFileName(const AName: string): string;
var
  i: Integer;
  S: string;
begin
  S := Trim(AName);
  if S = '' then
    Result := '_'
  else
  begin
    for i := 1 to Length(S) do
      if not (S[i] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', ' ']) then
        S[i] := '_';

    // Entferne Punkte am Anfang/Ende (verursachen Probleme im Dateisystem)
    while (S <> '') and (S[1] = '.') do
      Delete(S, 1, 1);
    while (S <> '') and (S[Length(S)] = '.') do
      Delete(S, Length(S), 1);

    if S = '' then
      S := '_';
    Result := S;
  end;
end;

function FormatMultilineSQLText(const AText: string): string;
var
  S: string;
begin
  // Zeilenenden normalisieren (optional, aber sauber)
  S := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  S := StringReplace(S, #13, #10, [rfReplaceAll]);

  // Firebird akzeptiert direkte Zeilenumbrüche im String
  Result := QuotedStr(S);
end;

procedure SaveTxConfigToFile(const FileName: string; Params: TStrings);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Assign(Params);
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

procedure LoadTxConfigFromFile(const FileName: string; Params: TStrings);
var
  SL: TStringList;
begin
  if not FileExists(FileName) then
    Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Params.Assign(SL);
  finally
    SL.Free;
  end;
end;

function FindNodeByDBIndex(ADBIndex: Word): TTreeNode;
var
  i, j: Integer;
  ServerNode, ChildNode: TTreeNode;
  NodeInfo: TPNodeInfos;
begin
  Result := nil;

  // Level 0 Nodes (Server) durchsuchen
  for i := 0 to MainTreeView.Items.Count - 1 do
  begin
    ServerNode := MainTreeView.Items[i];
    if ServerNode.Level <> 0 then
      Continue; // nur Server Nodes

    // Level 1 Nodes (DB) prüfen
    ChildNode := ServerNode.getFirstChild;
    while ChildNode <> nil do
    begin
      if Assigned(ChildNode.Data) then
      begin
        NodeInfo := TPNodeInfos(ChildNode.Data);
        if NodeInfo^.dbIndex = ADBIndex then
        begin
          Result := ChildNode;
          Exit;
        end;
      end;
      ChildNode := ChildNode.getNextSibling;
    end;
  end;
end;

function IBXProtocolToString(AProtocol: TProtocol): string;
begin
  case AProtocol of
    TCP:       Result := 'TCP';
    SPX:       Result := 'SPX';
    NamedPipe: Result := 'NamedPipe';
    Local:     Result := 'Local';
    inet:      Result := 'inet';
    inet4:     Result := 'inet4';
    inet6:     Result := 'inet6';
    wnet:      Result := 'wnet';
    xnet:      Result := 'xnet';
  end;
end;

function StringToIBXProtocol(const AValue: string): TProtocol;
var
  P: TProtocolAll;
begin
  if SameText(AValue,'TCP') then P := TCP
  else if SameText(AValue,'SPX') then P := SPX
  else if SameText(AValue,'NamedPipe') then P := NamedPipe
  else if SameText(AValue,'Local') then P := Local
  else if SameText(AValue,'inet') then P := inet
  else if SameText(AValue,'inet4') then P := inet4
  else if SameText(AValue,'inet6') then P := inet6
  else if SameText(AValue,'wnet') then P := wnet
  else if SameText(AValue,'xnet') then P := xnet
  else
    P := unknownProtocol;

  if P = unknownProtocol then
    raise Exception.Create('Unknown IBX protocol: ' + AValue);

  Result := TProtocol(P);

end;
//TransactionConfig
function HasParam(P: TStrings; const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to P.Count - 1 do
    if SameText(Trim(P[i]), S) then
      Exit(True);
end;

//TreeView Filter
function IsFilterScopeNode(ANode: TTreeNode): Boolean;
var
  NI: TPNodeInfos;
begin
  Result := False;
  if (ANode = nil) or (ANode.Data = nil) then Exit;

  NI := TPNodeInfos(ANode.Data);

  case NI^.ObjectType of
    tvotServer,
    tvotDatabase,
    tvotQueryWindow,

    tvotTableRoot,
    tvotTableFieldRoot,
    tvotSystemTableFieldRoot,

    tvotGeneratorRoot,
    tvotViewRoot,
    tvotUDFRoot,
    tvotProcedureRoot,
    tvotFunctionRoot,
    tvotDomainRoot,
    tvotUserRoot,
    tvotRoleRoot,
    tvotExceptionRoot,

    tvotTriggerRoot,
    tvotTableTriggerRoot,
    tvotDBTriggerRoot,
    tvotDDLTriggerRoot,

    tvotUDRRoot,
    tvotUDRTriggerRoot,
    tvotUDRTableTriggerRoot,
    tvotUDRDBTriggerRoot,
    tvotUDRDDLTriggerRoot,

    tvotPackage,
    tvotPackageRoot,
    tvotPackageFunctionRoot,
    tvotPackageProcedureRoot,
    tvotPackageUDRFunctionRoot,
    tvotPackageUDRProcedureRoot,

    // Constraints (NEU)
    tvotConstraintRoot,
    tvotPrimaryKeyRoot,
    tvotForeignKeyRoot,
    tvotUniqueConstraintRoot,
    tvotCheckConstraintRoot,
    tvotNotNullConstraintRoot,

    tvotSystemObjectRoot,
    tvotSystemTableRoot:
      Result := true;
  end;
end;

function IsObjectNameCaseSensitive(AObjectName: string): boolean;
var
  S: string;
begin
  S := Trim(AObjectName);
  Result := (S <> AnsiUpperCase(S));
end;

function IsObjectNameQuoted(const AStr: string): Boolean;
var
  S: string;
begin
  S := Trim(AStr);

  Result :=
    (Length(S) >= 2) and
    (S[1] = '"') and
    (S[Length(S)] = '"');
end;

function MakeObjectNameQuoted(AObjectName: string): string;
begin
  result := '"' + AObjectName + '"';
end;

function MakeCaseSensitiveAuto(const AObjectName: string): string;
var
  S: string;
begin
  S := Trim(AObjectName);

  if S = '' then
    Exit(S);

  // Bereits quoted → nichts ändern
  if IsObjectNameQuoted(S) then
    Exit(S);

  // Nur wenn NICHT komplett uppercase → quoten
  if IsObjectNameCaseSensitive(S) then
    Result := '"' + S + '"'
  else
    Result := S;  // Nur Uppercase → kein Quote nötig
end;

procedure MakeObjectNameListQuoted(var AObjectList: TStringList);
var i: integer;
begin
  for i := 0 to AObjectList.Count - 1 do
    if IsObjectNameCaseSensitive(AObjectList[i]) then
      AObjectList[i] := MakeObjectNameQuoted(AObjectList[i]);
end;

{  TIBSQLStatementTypes =
                 (SQLUnknown, SQLSelect, SQLInsert,
                  SQLUpdate, SQLDelete, SQLDDL,
                  SQLGetSegment, SQLPutSegment,
                  SQLExecProcedure, SQLStartTransaction,
                  SQLCommit, SQLRollback,
                  SQLSelectForUpdate, SQLSetGenerator,
                  SQLSavePoint);
}
function NeedsCommit(Q: TIBQuery): Boolean;
begin
  Result := False;

  if not Assigned(Q) then
    Exit;

  case Q.StatementType of
    SQLInsert,
    SQLUpdate,
    SQLDelete,
    SQLDDL,
    SQLSetGenerator,
    SQLSavePoint:
      Result := True;
  else
    Result := False;
  end;
end;

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
  LoginForm: TfrmLoginServiceManager;
  UserName, Password: string;
begin
  Result := False;
  ErrorStr := '';
  ServerSession := nil;

  try
    // Alias-Lookup mit Fallback auf echten Servernamen — dadurch
    // funktioniert IsServerReachable sowohl mit Alias (Reg.pas)
    // als auch mit echtem Servernamen (main.pas).
    ServerRecord := GetServerRecordFromFileByAlias(AServerName);

    UserName := ServerRecord.UserName;
    Password := ServerRecord.Password;

    // 1. Wenn kein Passwort gespeichert → Session-Cache prüfen
    if (Password = '') and (UserName <> '') then
      Password := GetServerSessionPassword(AServerName);

    // 2. Wenn immer noch kein Passwort
    if (Password = '') and (UserName <> '') then
    begin
      // Bei Local/Embedded: Dummy-Passwort für den Cache
      if ServerRecord.IsEmbedded then
      begin
        Password := 'embedded_local';
        SetServerSessionPassword(AServerName, Password);
      end
      else
      begin
        // TCP: Server-Login-Dialog
        LoginForm := TfrmLoginServiceManager.Create(nil);
        try
          LoginForm.lbSever.Caption := AServerName;
          LoginForm.edtUserName.Text := UserName;
          LoginForm.edtPassword.Text := '';
          LoginForm.chkBoxSavePwd.Checked := False;

          if LoginForm.ShowModal <> mrOK then
          begin
            ErrorStr := 'Login cancelled by user.';
            Exit;
          end;

          UserName := LoginForm.edtUserName.Text;
          Password := LoginForm.edtPassword.Text;

          if LoginForm.chkBoxSavePwd.Checked then
          begin
            // Dauerhaft in Datei speichern
            ServerRecord.UserName := UserName;
            ServerRecord.Password := Password;
            SaveServerDataToFile(ServerRecord);
          end
          else
          begin
            // NUR in Session-Cache (temporär)
            SetServerSessionPassword(AServerName, Password);
          end;
        finally
          LoginForm.Free;
        end;
      end;
    end;

    // 3. ServerSession erstellen und verbinden
    ServerSession := TServerSession.Create(
      AServerName, '', '', '', '', TCP, '', '', '', '', '', 0, 0, False, False, 0, 0, 0
    );

    ApplyServerRecordToSession(ServerRecord, ServerSession);
    ServerSession.UserName := UserName;
    ServerSession.Password := Password;

    if not ServerSession.Connected then
      if not ServerSession.IBXConnect then
      begin
        ErrorStr := ServerSession.ErrorStr;
        // Bei Fehler: Session-Cache löschen
        ClearServerSessionPassword(AServerName);
        Exit;
      end;

    Result := True;

  finally
    if Assigned(ServerSession) then
    begin
      ServerSession.Disconnect;
      ServerSession.Free;
    end;
  end;
end;

{ ============================================================
  CreateServerSessionFromRegistry: Erzeugt eine TServerSession
  aus dem Registry-Record.

  NEU:
    - Lookup über GetServerRecordFromFileByAlias statt ByName.
      AServerName ist seit der Alias-Umstellung der ANZEIGE-Name
      (Alias oder Fallback echter Name). ByAlias findet den
      richtigen Record auch dann, wenn mehrere Aliase auf
      demselben echten Server sitzen (Prod/Test → beide
      FB60Local) — ByName würde immer den ersten liefern und
      damit Port/User/Password des falschen Servers verwenden.

    - Defensive Prüfung: Wenn der Record nicht gefunden wird,
      liefert die Funktion nil. Der Aufrufer muss den
      Rückgabewert prüfen.

  Die Session selbst bekommt sowohl ServerName (echter Name,
  für die Verbindung) als auch ServerAlias (für die Anzeige).
  ============================================================ }
function CreateServerSessionFromRegistry(const AServerName: string): TServerSession;
var
  ServerRecord: TServerRecord;
begin
  ServerRecord := GetServerRecordFromFileByAlias(AServerName);

  if Trim(ServerRecord.ServerName) = '' then
    Exit(nil);

  Result := TServerSession.Create(
    ServerRecord.ServerName,
    ServerRecord.ServerAlias,
    ServerRecord.UserName,
    ServerRecord.Password,
    ServerRecord.Role,
    ServerRecord.Protocol,
    ServerRecord.Port,
    ServerRecord.Charset,
    ServerRecord.RootPath,
    ServerRecord.ClientLibraryPath,
    ServerRecord.ConfigFilePath,
    ServerRecord.VersionMinor,
    ServerRecord.VersionMajor,
    ServerRecord.LoadRegisteredClientLib,
    ServerRecord.IsEmbedded,
    ServerRecord.ConnectTimeoutMS,
    ServerRecord.RetryCount,
    ServerRecord.QueryTimeoutMS
  );
end;

{ ============================================================
  ParseDBConnectionString: Zerlegt einen Firebird-ConnStr in
  Hostname, Port und Dateipfad.

  Format: [host[/port]:]path
  Beispiele:
    'FB60Local/3060:/opt/firebird6/data/test.fdb'
      → Host='FB60Local', Port='3060', Path='/opt/firebird6/...'
    'FB60Local:/opt/firebird6/data/test.fdb'
      → Host='FB60Local', Port='', Path='/opt/firebird6/...'
    '127.0.0.1:employee.fdb'
      → Host='127.0.0.1', Port='', Path='employee.fdb'
    '/opt/firebird/data/test.fdb'   (Embedded, unix)
      → Host='', Port='', Path='/opt/...'
    'C:\fb\data\test.fdb'           (Embedded, windows)
      → Host='', Port='', Path='C:\fb\data\test.fdb'

  Wichtig: Windows-Laufwerksbuchstabe (C:) wird NICHT als
  Trennzeichen interpretiert.
  ============================================================ }
procedure ParseDBConnectionString(const AConnStr: string;
  out AHost, APort, APath: string);
var
  S, HostPart: string;
  P: Integer;
begin
  AHost := ''; APort := ''; APath := '';
  S := Trim(AConnStr);
  if S = '' then Exit;

  // Windows-Pfad: 'C:\...' oder 'C:/...' → nur Pfad
  if (Length(S) >= 2) and (S[2] = ':') and
     ((S[1] in ['A'..'Z']) or (S[1] in ['a'..'z'])) then
  begin
    APath := S;
    Exit;
  end;

  // Unix-Pfad: '/opt/...' → nur Pfad
  if S[1] = '/' then
  begin
    APath := S;
    Exit;
  end;

  // Firebird-ConnStr: host[/port]:path
  P := LastDelimiter(':', S);
  if P = 0 then
  begin
    // Kein ':' vorhanden → als reinen Pfad behandeln
    APath := S;
    Exit;
  end;

  APath := Copy(S, P + 1, MaxInt);
  HostPart := Copy(S, 1, P - 1);

  // Port vom Host trennen (host/port)
  P := LastDelimiter('/', HostPart);
  if P > 0 then
  begin
    APort := Copy(HostPart, P + 1, MaxInt);
    AHost := Copy(HostPart, 1, P - 1);
  end
  else
    AHost := HostPart;
end;

{ ============================================================
  ValidateDBConnectionString: Prüft, ob der eingegebene
  Connection-String zum AUSGEWÄHLTEN Server passt.

  NEU (Bug-Fix bei mehreren Servern mit demselben ServerName):
    Die Prüfung geht jetzt vom AExpectedServerAlias (dem in der
    ComboBox ausgewählten Server) aus, NICHT vom Host im ConnStr.

    Grund: Wenn mehrere Server denselben ServerName haben
    (z.B. 7x 'localhost' mit unterschiedlichen Ports), kann
    GetServerRecordFromFileByAlias(Host) den Host nicht
    eindeutig zuordnen — es findet immer den ersten Treffer.

    Alte Logik:
      1. ServerRec := ByAlias(Host)  ← falsch bei mehreren
                                        Servern mit gleichem Namen
      2. Port vergleichen

    Neue Logik:
      1. ExpectedRec := ByAlias(AExpectedServerAlias)
         (eindeutig, weil Alias systemweit eindeutig ist)
      2. Host im ConnStr muss zu ExpectedRec.ServerName oder
         ExpectedRec.ServerAlias passen
      3. Port im ConnStr muss zu ExpectedRec.Port passen

  Liefert:
    Result = True  → alles OK
    Result = False → AWarningMsg enthält eine verständliche
                     Fehlerbeschreibung.

  Embedded-Verbindungen (Host = '') werden immer akzeptiert.
  ============================================================ }
function ValidateDBConnectionString(const AConnStr, AExpectedServerAlias: string;
  out AWarningMsg: string): Boolean;
var
  Host, Port, Path: string;
  ExpectedRec: TServerRecord;
begin
  Result := True;
  AWarningMsg := '';

  ParseDBConnectionString(AConnStr, Host, Port, Path);

  // Embedded: kein Host → immer OK
  if Host = '' then
    Exit;

  // Erwarteten Server (aus ComboBox-Alias) holen — der Alias ist
  // systemweit eindeutig, daher zuverlässig.
  ExpectedRec := GetServerRecordFromFileByAlias(AExpectedServerAlias);

  if Trim(ExpectedRec.ServerName) = '' then
  begin
    AWarningMsg := 'Selected server "' + AExpectedServerAlias +
                   '" is not a registered server.';
    Result := False;
    Exit;
  end;

  // ============================================================
  // 1) Host im ConnStr muss zum erwarteten Server passen
  //    (entweder ServerName oder Alias des erwarteten Servers)
  // ============================================================
  if not (SameText(Host, Trim(ExpectedRec.ServerName)) or
          SameText(Host, Trim(ExpectedRec.ServerAlias)) or
          SameText(Host, AExpectedServerAlias)) then
  begin
    AWarningMsg := 'Host "' + Host + '" does not match the selected ' +
                   'server "' + AExpectedServerAlias + '" (registered ' +
                   'as "' + Trim(ExpectedRec.ServerName) + '").';
    Result := False;
    Exit;
  end;

  // ============================================================
  // 2) Port im ConnStr muss zum erwarteten Server passen
  // ============================================================
  if (Port <> '') and (Trim(ExpectedRec.Port) <> '') and
     (Port <> Trim(ExpectedRec.Port)) then
  begin
    AWarningMsg := 'Port ' + Port + ' does not match the port ' +
                   Trim(ExpectedRec.Port) + ' configured for server "' +
                   AExpectedServerAlias + '".';
    Result := False;
    Exit;
  end;
end;

{ ============================================================
  GetServerInventoryAsText: Erstellt einen formatierten Text-
  Bericht aller registrierten Server und ihrer Datenbanken.

  Format:

    Firebird 6.0 on 127.0.0.1 (TCP Connection): Port: 3060  [Alias: FB60Local]
      employee: FB60Local, 127.0.0.1/3060:/opt/firebird6/.../employee.fdb
      test_db: FB60Local, 127.0.0.1/3060:/opt/firebird6/.../test_db.fdb

    Firebird 3.0 on 127.0.0.1 (TCP Connection): Port: 3030  [Alias: FB30Local]
      (no databases registered)

    Total: 7 server(s), 12 database(s)

  Verwendung:
    - ShowMessage(GetServerInventoryAsText);
    - oder in eine Datei schreiben
    - oder in ein Memo / Report

  Die Funktion nutzt die globalen Arrays RegisteredServers und
  RegisteredDatabases. Sie ist unabhängig von der TreeView.

  DB-Zuordnung:
    - Primär: Rec.ServerName = ServerRec.ServerAlias
      (neue Registrierungen speichern den Alias)
    - Fallback: Rec.ServerName = ServerRec.ServerName
      (nur wenn ServerAlias leer ist — Legacy-Server ohne Alias)
  ============================================================ }
function GetServerInventoryAsText: string;
var
  i, j: Integer;
  ServerRec: TServerRecord;
  DBRec: TRegisteredDatabase;
  VersionStr, ProtocolStr, HeaderLine: string;
  HasDBs: Boolean;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    for i := 0 to High(RegisteredServers) do
    begin
      ServerRec := RegisteredServers[i];

      // ---- Version ----
      if (ServerRec.VersionMajor > 0) or (ServerRec.VersionMinor > 0) then
        VersionStr := 'Firebird ' + IntToStr(ServerRec.VersionMajor) + '.' +
                      IntToStr(ServerRec.VersionMinor)
      else
        VersionStr := 'Firebird (unknown version)';

      // ---- Protocol ----
      ProtocolStr := IBXProtocolToString(ServerRec.Protocol);

      // ---- Header-Zeile ----
      HeaderLine := VersionStr + ' on ' + ServerRec.ServerName +
                    ' (' + ProtocolStr + ' Connection)';
      if Trim(ServerRec.Port) <> '' then
        HeaderLine := HeaderLine + ': Port: ' + Trim(ServerRec.Port);
      if Trim(ServerRec.ServerAlias) <> '' then
        HeaderLine := HeaderLine + '  [Alias: ' + Trim(ServerRec.ServerAlias) + ']';

      SL.Add(HeaderLine);

      // ---- DBs unter diesem Server ----
      HasDBs := False;
      for j := 0 to High(RegisteredDatabases) do
      begin
        DBRec := RegisteredDatabases[j].RegRec;
        if DBRec.Deleted then
          Continue;

        // Zuordnung prüfen
        if SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerAlias)) or
           ((Trim(ServerRec.ServerAlias) = '') and
            SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerName))) then
        begin
          SL.Add('  ' + DBRec.Title + ': ' + Trim(DBRec.ServerName) + ', ' +
                 DBRec.DatabaseName);
          HasDBs := True;
        end;
      end;

      if not HasDBs then
        SL.Add('  (no databases registered)');

      SL.Add('');
    end;

    if Length(RegisteredServers) = 0 then
      SL.Add('No servers registered.')
    else
      SL.Add('Total: ' + IntToStr(Length(RegisteredServers)) + ' server(s), ' +
             IntToStr(Length(RegisteredDatabases)) + ' database(s)');

    Result := SL.Text;
  finally
    SL.Free;
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

procedure ParseFBVersion(AFBVersionString: string; out AFBVersionMajor: word; out AFBVersionMinor: word);
begin
  ParseFBVersionString(AFBVersionString, AFBVersionMajor, AFBVersionMinor);
end;

function TestEmbeddedConnection(AServerRec: TServerRecord; out AFBVersionMajor: word;
                                                           out AFBVersionMinor: word;
                                                           out AFBServerVersion: string): boolean;
var tmpDB: TIBDatabase;
    tmpDbInfo: TIBDatabaseInfo;
    dbFile: string;
begin
  result := false;

  dbFile := ExpandFileName(ExtractFilePath(Application.ExeName) +
            TmpDir + DirectorySeparator + 'embedded_test.fdb');

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

      AFBServerVersion := tmpDbInfo.FirebirdVersion;
      ParseFBVersion(AFBServerVersion, AFBVersionMajor, AFBVersionMinor);

      tmpDB.Close;
      result := true;
    except
      on E: Exception do
      begin
        AFBVersionMajor := 0;
        AFBVersionMinor := 0;
        AFBServerVersion := 'Unknown';
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

function DeleteDBRegistrationFromFile(const ATitle: string; const AServerName: string): Boolean;
var
  F: file of TRegisteredDatabase;
  Rec: TRegisteredDatabase;
  FileName: string;
  CmpTitle: string;
  CmpServer: string;
begin
  Result := False;
  FileName := GetConfigurationDirectory + DatabasesRegFile;

  if not FileExists(FileName) then
    Exit;

  // Vergleichswerte vorbereiten (nur einmal!)
  CmpTitle := Trim(ATitle);
  CmpServer := Trim(AServerName);

  AssignFile(F, FileName);
  FileMode := 2; // read/write

  try
    Reset(F);

    while not Eof(F) do
    begin
      Read(F, Rec);

      if (not Rec.Deleted) and
         SameText(Rec.Title, CmpTitle) and
         SameText(Rec.ServerName, CmpServer) then
      begin
        Rec.Deleted := True;

        Seek(F, FilePos(F) - 1);
        Write(F, Rec);

        Result := True;
        Exit;
      end;
    end;

  finally
    CloseFile(F);
  end;
end;

function EditRegisteredDB(var ARec: TRegisteredDatabase): Boolean;
var
  F: file of TRegisteredDatabase;
  FileName: string;
  Rec: TRegisteredDatabase;
  Found: Boolean;
  PosIndex: Integer;
begin
  Result := False;
  Found := False;
  PosIndex := -1;

  FileName := GetConfigurationDirectory + DatabasesRegFile;

  if not FileExists(FileName) then
    Exit;

  AssignFile(F, FileName);
  try
    FileMode := 2;
    Reset(F);

    // Nach dem passenden Record suchen (ServerName + DatabaseName)
    while not Eof(F) do
    begin
      Read(F, Rec);
      if SameText(Trim(Rec.ServerName), Trim(ARec.ServerName)) and
         SameText(Trim(Rec.DatabaseName), Trim(ARec.DatabaseName)) then
      begin
        PosIndex := FilePos(F) - 1;
        Found := True;
        Break;
      end;
    end;

    if Found then
    begin
      // Record an der gefundenen Position überschreiben
      Seek(F, PosIndex);
      Write(F, ARec);
      Result := True;
    end;

  finally
    CloseFile(F);
  end;
end;

{ ============================================================
  Schreibt die komplette RegisteredDatabases-Liste neu in
  databases.reg.

  Wird für die Alias-Migration benötigt, weil EditRegisteredDB
  den Record anhand ServerName + DatabaseName identifiziert —
  nach einer Alias-Änderung würde das nicht mehr matchen.
  ============================================================ }
function SaveDBRegistrationsToFile: Boolean;
var
  F: file of TRegisteredDatabase;
  FileName: string;
  i: Integer;
begin
  Result := False;
  try
    FileName := GetConfigurationDirectory + DatabasesRegFile;

    AssignFile(F, FileName);
    FileMode := 2;
    Rewrite(F);

    for i := 0 to High(RegisteredDatabases) do
    begin
      RegisteredDatabases[i].OrigRegRec := RegisteredDatabases[i].RegRec;
      Write(F, RegisteredDatabases[i].OrigRegRec);
    end;

    CloseFile(F);
    Result := True;
  except
    try
      CloseFile(F);
    except
    end;
    Result := False;
  end;
end;

{ ============================================================
  Schreibt DB-Registrierungen um, wenn sich der Server-Alias
  ändert.

  Zwei Fälle:
    1) AOldName wird direkt gespeichert (neue Registrierung)
       → auf ANewAlias umschreiben
    2) Der ECHTE Servername wird gespeichert (Legacy),
       und der Server bekommt jetzt einen Alias
       → auf ANewAlias umschreiben

  ARealServerName = der echte Servername des Servers, dessen
  Alias sich ändert. Damit Fall 2 erkannt werden kann.

  Liefert die Anzahl geänderter Records.
  ============================================================ }
function RenameDBRegistrationsServerAlias(
  const AOldName, ANewAlias, ARealServerName: string): Integer;
var
  i: Integer;
  Count: Integer;
  OldName, NewAliasT, RealName, RecName: string;
  Changed: Boolean;
begin
  Result := 0;

  OldName  := Trim(AOldName);
  NewAliasT := Trim(ANewAlias);
  RealName  := Trim(ARealServerName);

  if NewAliasT = '' then
    Exit;

  Count := 0;

  for i := 0 to High(RegisteredDatabases) do
  begin
    Changed := False;
    RecName := Trim(RegisteredDatabases[i].RegRec.ServerName);

    // Fall 1: Record speichert exakt den alten Alias
    if (OldName <> '') and SameText(RecName, OldName) then
    begin
      RegisteredDatabases[i].RegRec.ServerName := NewAliasT;
      Changed := True;
    end
    // Fall 2: Record speichert den echten Servernamen (Legacy)
    else if (RealName <> '') and SameText(RecName, RealName) and
            not SameText(NewAliasT, RealName) then
    begin
      RegisteredDatabases[i].RegRec.ServerName := NewAliasT;
      Changed := True;
    end;

    if Changed then
      Inc(Count);
  end;

  if Count > 0 then
    SaveDBRegistrationsToFile;

  Result := Count;
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

function GetDBRecByServerAndDBName(const AServerName, ADBName: string): TDatabaseRec;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);

  for i := 0 to High(RegisteredDatabases) do
  begin
    if (RegisteredDatabases[i].RegRec.ServerName = AServerName) and
       (RegisteredDatabases[i].RegRec.Title = ADBName) then
    begin
      Result := RegisteredDatabases[i];
      Exit;
    end;
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

  if Target.Connected then
    Target.Close;

  // Basis-Einstellungen
  Target.DatabaseName   := Source.DatabaseName;
  Target.FirebirdLibraryPathName := Source.FirebirdLibraryPathName;
  Target.Params.Assign(Source.Params);

  Target.LoginPrompt    := True;
  Target.OnLogin        := @dmSysTables.OnDatabaseLogin;
  Target.SQLDialect     := Source.SQLDialect;

  // Provider-Flags und Misc
  Target.TraceFlags     := Source.TraceFlags;
  Target.IdleTimer      := Source.IdleTimer;
  Target.AllowStreamedConnected := Source.AllowStreamedConnected;
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

{ ============================================================
  Prüft, ob ein Alias systemweit eindeutig ist.

  ZWEI Ausschluss-Parameter:

    AExcludeAlias      — der aktuelle Alias des zu bearbeitenden
                         Servers (primärer Ausschluss, weil Alias
                         der eindeutige Identifikator ist).

    AExcludeServerName — der echte Servername des zu bearbeitenden
                         Servers. Wird benötigt, um zwischen
                         "echter Kollision mit anderem Server" und
                         "derselbe physische Server" unterscheiden
                         zu können.

  Kollisionen:

    1) Alias = Alias eines anderen Servers
       → immer Kollision.

    2) Alias = echter Servername eines anderen Servers
       → Kollision, ABER NICHT wenn der andere Server
         denselben echten Servernamen hat wie der editierte
         Server (dann ist es derselbe physische Server, und
         der Alias = eigene ServerName ist zulässig).

  Warum Punkt 2 die Ausnahme braucht:
    Wenn zwei Server auf demselben echten Server laufen
    (Prod/Test → beide FB60Local) und einer davon den
    Alias "FB60Local" trägt, würde die naive Prüfung
    "Alias = echter Name eines anderen Servers" beim
    Editieren des Prod-Servers fälschlich Kollision
    melden — weil der Test-Server auch "FB60Local"
    als echten Namen hat.
  ============================================================ }
function IsServerAliasUnique(const AAlias, AExcludeAlias, AExcludeServerName: string): Boolean;
var
  fs: TFileStream;
  rec: TServerRecord;
  fileName: string;
  compareAlias, excludeAlias, excludeName, recAlias, recName: string;
  IsSelfRecord: Boolean;
begin
  Result := True;

  compareAlias := Trim(AAlias);
  if compareAlias = '' then
    Exit;

  excludeAlias := Trim(AExcludeAlias);
  excludeName  := Trim(AExcludeServerName);

  fileName := GetConfigurationDirectory + ServersRegFile;
  if not FileExists(fileName) then
    Exit;

  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    while fs.Position < fs.Size do
    begin
      fs.ReadBuffer(rec, SizeOf(TServerRecord));

      recAlias := Trim(rec.ServerAlias);
      recName  := Trim(rec.ServerName);

      // ------------------------------------------------------
      // Ausschluss: aktuell bearbeiteter Record
      // ------------------------------------------------------
      IsSelfRecord := False;

      // Fall A: Record hat einen Alias, der dem alten Alias
      // des bearbeiteten Servers entspricht
      if (excludeAlias <> '') and (recAlias <> '') and
         SameText(recAlias, excludeAlias) then
        IsSelfRecord := True;

      // Fall B: Legacy-Record ohne Alias, dessen echter Name
      // dem des bearbeiteten Servers entspricht
      if (excludeAlias = '') and (excludeName <> '') and
         (recAlias = '') and SameText(recName, excludeName) then
        IsSelfRecord := True;

      if IsSelfRecord then
        Continue;

      // ------------------------------------------------------
      // Kollision 1: Alias = Alias eines anderen Servers
      // ------------------------------------------------------
      if (recAlias <> '') and SameText(recAlias, compareAlias) then
      begin
        Result := False;
        Exit;
      end;

      // ------------------------------------------------------
      // Kollision 2: Alias = echter Servername eines anderen
      // Servers — ABER nur, wenn der andere Server einen
      // ANDEREN echten Servernamen hat als der editierte.
      // Wenn der echte Name derselbe ist, ist es derselbe
      // physische Server → keine Kollision.
      // ------------------------------------------------------
      if SameText(recName, compareAlias) and
         not SameText(recName, excludeName) then
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    fs.Free;
  end;
end;

{ ============================================================
  Sucht einen Server-Record anhand des Alias.

  ZWEI DURCHGÄNGE (beide Pflicht!):

    Pass 1: Alias-Vergleich
      → Für neue DB-Registrierungen (Alias gespeichert).

    Pass 2: Echter Servername (Fallback)
      → Für Legacy-DB-Registrierungen (echter Name gespeichert)
        UND für Aufrufer wie IsServerReachable, die den
        echten Servernamen übergeben.

  WICHTIG: Der Fallback auf Pass 2 greift auch dann, wenn
  ein Record einen Alias hat — sonst würden Aufrufer mit
  echtem Servernamen (z.B. IsServerReachable in main.pas)
  keinen Record mehr finden, sobald ein Alias gesetzt ist.

  Liefert einen leeren Record, wenn nichts gefunden wurde.
  ============================================================ }
function GetServerRecordFromFileByAlias(const AAlias: string): TServerRecord;
var
  fs: TFileStream;
  rec: TServerRecord;
  fileName: string;
  compareAlias: string;
  FoundByAlias: Boolean;
begin
  FillChar(Result, SizeOf(Result), 0);

  compareAlias := Trim(AAlias);
  if compareAlias = '' then
    Exit;

  fileName := getConfigurationDirectory + ServersRegFile;
  if not FileExists(fileName) then
    Exit;

  FoundByAlias := False;
  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    // ----------------------------------------------------------
    // Pass 1: ALIAS-Vergleich (bevorzugt)
    // ----------------------------------------------------------
    while fs.Position < fs.Size do
    begin
      fs.ReadBuffer(rec, SizeOf(TServerRecord));
      if (Trim(rec.ServerAlias) <> '') and SameText(Trim(rec.ServerAlias), compareAlias) then
      begin
        Result := rec;
        FoundByAlias := True;
        Break;
      end;
    end;

    // ----------------------------------------------------------
    // Pass 2: ECHTER SERVERNAME (Fallback)
    // Greift auch dann, wenn der Record einen Alias hat!
    // ----------------------------------------------------------
    if not FoundByAlias then
    begin
      fs.Position := 0;
      while fs.Position < fs.Size do
      begin
        fs.ReadBuffer(rec, SizeOf(TServerRecord));
        if SameText(Trim(rec.ServerName), compareAlias) then
        begin
          Result := rec;
          Break;
        end;
      end;
    end;
  finally
    fs.Free;
  end;
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

{ ============================================================
  Speichert einen Server-Record in servers.reg.

  Identifikation des zu überschreibenden Records:

    Pass 1 — AOldAlias (explizit übergeben)
      → Der Anwender hat den Alias geändert. Wir suchen nach
        dem ALTEN Alias und überschreiben den Record.
      → Verhindert das Anlegen eines Duplikats.

    Pass 2 — Rec.ServerAlias (normaler Update)
      → Kein Alias-Wechsel. Wir suchen nach dem aktuellen
        Alias und überschreiben den Record.
      → Greift nur, wenn AOldAlias leer übergeben wurde
        (sonst würde ein fremder Record gefunden).

    Pass 3 — Legacy-Match (ServerName)
      → Record hat noch keinen Alias, soll aber einen bekommen.
      → Suche über ServerName mit LEEREM Alias im alten Record.
      → Verhindert, dass ein Record mit bereits gesetztem Alias
        (der zufällig denselben ServerName hat) überschrieben wird.

  Wenn nichts gefunden: Record wird ANGEHÄNGT (neuer Server).

  WICHTIG: Die alte Version identifizierte Records nur über
  ServerName. Bei zwei Einträgen mit demselben echten Servernamen
  (z.B. FB60Local → zwei Aliase) überschrieb sie den ersten
  Eintrag — genau der Bug, der zum Verschwinden des Servers
  mit seiner DB geführt hat.
  ============================================================ }
procedure SaveServerDataToFile(const Rec: TServerRecord; const AOldAlias: string = '');
var
  tmpList: TMemoryStream;
  oldRec: TServerRecord;
  found: Boolean;
  regFile: string;
  NewAlias, OldAlias, RecAlias, RecServerName: string;
begin
  regFile := GetConfigurationDirectory + ServersRegFile;

  NewAlias  := Trim(Rec.ServerAlias);
  OldAlias  := Trim(AOldAlias);

  tmpList := TMemoryStream.Create;
  try
    if FileExists(regFile) then
      tmpList.LoadFromFile(regFile);

    tmpList.Position := 0;
    found := False;

    while tmpList.Position < tmpList.Size do
    begin
      tmpList.ReadBuffer(oldRec, SizeOf(oldRec));
      RecAlias      := Trim(oldRec.ServerAlias);
      RecServerName := Trim(oldRec.ServerName);

      // ----------------------------------------------------
      // Pass 1: Expliziter Alias-Wechsel
      // ----------------------------------------------------
      if (OldAlias <> '') and (RecAlias <> '') and SameText(RecAlias, OldAlias) then
      begin
        tmpList.Position := tmpList.Position - SizeOf(oldRec);
        tmpList.WriteBuffer(Rec, SizeOf(Rec));
        found := True;
        Break;
      end;

      // ----------------------------------------------------
      // Pass 2: Normaler Update (kein Alias-Wechsel)
      // ----------------------------------------------------
      if (OldAlias = '') and (NewAlias <> '') and (RecAlias <> '') and SameText(RecAlias, NewAlias) then
      begin
        tmpList.Position := tmpList.Position - SizeOf(oldRec);
        tmpList.WriteBuffer(Rec, SizeOf(Rec));
        found := True;
        Break;
      end;

      // ----------------------------------------------------
      // Pass 3: Legacy-Record bekommt erstmals einen Alias
      // ----------------------------------------------------
      if (OldAlias = '') and (NewAlias <> '') and (RecAlias = '')
         and SameText(RecServerName, Trim(Rec.ServerName)) then
      begin
        tmpList.Position := tmpList.Position - SizeOf(oldRec);
        tmpList.WriteBuffer(Rec, SizeOf(Rec));
        found := True;
        Break;
      end;

      // ----------------------------------------------------
      // Pass 4: Legacy-Update ohne Alias (beide leer)
      // ----------------------------------------------------
      if (OldAlias = '') and (NewAlias = '') and (RecAlias = '')
         and SameText(RecServerName, Trim(Rec.ServerName)) then
      begin
        tmpList.Position := tmpList.Position - SizeOf(oldRec);
        tmpList.WriteBuffer(Rec, SizeOf(Rec));
        found := True;
        Break;
      end;
    end;

    // ----------------------------------------------------
    // Neuer Record → anhängen
    // ----------------------------------------------------
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
  Session.ServerName          := Rec.ServerName;
  Session.FBVersionString     := Rec.VersionString;
  Session.FBVersionMajor      := Rec.VersionMajor;
  Session.FBVersionMinor      := Rec.VersionMinor;

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

{ ============================================================
  Liefert den ALIAS eines Server-Knotens.

  Bevorzugt ServerSession.ServerAlias (der eindeutige,
  systemweit vergebene Alias).
  Fallback auf Node.Text — das ist der Anzeige-Name, der bei
  fehlendem Alias automatisch der echte Servername ist.

  WICHTIG: Diese Funktion liefert den Anzeige-Namen.
  Für den ECHTEN Servernamen (Lookup in servers.reg)
  stattdessen GetServerNameFromNode() verwenden.

  Verwendung:
    - DB-Registrierung: Alias als Server-Identifikator speichern
    - Alle Stellen, die den Anwender-seitigen Namen brauchen
  ============================================================ }
function GetServerAliasFromNode(ANode: TTreeNode): string;
var
  Info: TPNodeInfos;
begin
  Result := '';

  if (ANode = nil) or (not Assigned(ANode.Data)) then
    Exit;

  Info := TPNodeInfos(ANode.Data);

  if Assigned(Info^.ServerSession) and (Trim(Info^.ServerSession.ServerAlias) <> '') then
    Result := Trim(Info^.ServerSession.ServerAlias)
  else
    Result := Trim(ANode.Text);
end;

{ ============================================================
  GetServerNodeByAlias: Findet den Server-Knoten anhand des
  Alias (Anzeige-Name).

  DESIGN-REGELN:
    - Jeder Server hat einen Alias (beim Anlegen via InputBox
      erzwungen).
    - Jede DB hängt unter einem registrierten Server
      (Registrierung nur per Rechtsklick auf Server-Node).
    - Ein leerer ServerAlias ist nur bei kaputter .reg-Datei
      möglich → dann lieber nil zurückgeben als raten.

  Liefert nil, wenn kein Knoten mit diesem Alias existiert.
  ============================================================ }
function GetServerNodeByAlias(const AAlias: string): TTreeNode;
var
  Node: TTreeNode;
  Info: TPNodeInfos;
  CompareName, NodeAlias: string;
begin
  Result := nil;

  if MainTreeView = nil then
    Exit;

  CompareName := Trim(AAlias);
  if CompareName = '' then
    Exit;

  Node := MainTreeView.Items.GetFirstNode;
  while Node <> nil do
  begin
    if (Node.Level = 0) and Assigned(Node.Data) then
    begin
      Info := TPNodeInfos(Node.Data);
      if Assigned(Info^.ServerSession) then
      begin
        NodeAlias := Trim(Info^.ServerSession.ServerAlias);
        if (NodeAlias <> '') and SameText(NodeAlias, CompareName) then
        begin
          Result := Node;
          Exit;
        end;
      end;
    end;
    Node := Node.GetNextSibling;
  end;
end;

{ ============================================================
  Liefert den echten Servernamen eines Server-Knotens.

  Bevorzugt ServerSession.ServerName (zuverlässig, auch wenn
  Node.Text künftig einen Alias anzeigt).
  Fallback auf Node.Text, falls keine ServerSession vorhanden
  ist (z. B. bei alten Knoten ohne Session).
  ============================================================ }
function GetServerNameFromNode(ANode: TTreeNode): string;
var
  Info: TPNodeInfos;
begin
  Result := '';

  if (ANode = nil) or (not Assigned(ANode.Data)) then
    Exit;

  Info := TPNodeInfos(ANode.Data);

  if Assigned(Info^.ServerSession) and (Trim(Info^.ServerSession.ServerName) <> '') then
    Result := Trim(Info^.ServerSession.ServerName)
  else
    Result := Trim(ANode.Text);
end;

{ ============================================================
  Findet den Server-Knoten (Level 0) anhand des ECHTEN
  Servernamens.

  Verwendet GetServerNameFromNode(), damit auch dann der
  korrekte Knoten gefunden wird, wenn die TreeView künftig
  den Alias anzeigt.

  Nur Level-0-Knoten werden durchsucht (GetNextSibling-Lauf
  über die Wurzel-Ebene).

  Defensive Prüfungen:
    - MainTreeView = nil
    - Node.Data = nil
    - leere AServerName

  Liefert nil, wenn kein passender Knoten gefunden wurde.
  ============================================================ }
function GetServerNodeByServerName(const AServerName: string): TTreeNode;
var
  Node: TTreeNode;
  CompareName, NodeName: string;
begin
  Result := nil;

  if MainTreeView = nil then
    Exit;

  CompareName := Trim(AServerName);
  if CompareName = '' then
    Exit;

  Node := MainTreeView.Items.GetFirstNode;

  while Node <> nil do
  begin
    // Nur Level 0 (Server-Ebene) durchsuchen
    if Node.Level = 0 then
    begin
      NodeName := GetServerNameFromNode(Node);

      if (NodeName <> '') and SameText(NodeName, CompareName) then
      begin
        Result := Node;
        Exit;
      end;
    end;

    Node := Node.GetNextSibling;
  end;
end;

{ ============================================================
  Liefert die ANZEIGE-Namen aller Server-Knoten (Level 0).

  Bevorzugt ServerAlias aus der ServerSession; fällt auf den
  echten Servernamen zurück, wenn kein Alias gesetzt ist.

  Gedacht für ComboBoxen und andere UI-Elemente, die dem
  Anwender den Alias zeigen sollen.

  Für interne Lookups immer GetServerListFromTreeView
  (echte Servernamen) verwenden!
  ============================================================ }
function GetServerAliasListFromTreeView: TStringList;
var
  i: Integer;
  Node: TTreeNode;
  Info: TPNodeInfos;
  DisplayName: string;
begin
  Result := nil;
  if MainTreeView = nil then
    Exit;
  if MainTreeView.Items.Count = 0 then
    Exit;

  Result := TStringList.Create;

  for i := 0 to MainTreeView.Items.Count - 1 do
  begin
    Node := MainTreeView.Items[i];

    if (Node.Level = 0) and (Trim(Node.Text) <> '') then
    begin
      DisplayName := '';

      if Assigned(Node.Data) then
      begin
        Info := TPNodeInfos(Node.Data);

        if Assigned(Info^.ServerSession)
           and (Trim(Info^.ServerSession.ServerAlias) <> '') then
          DisplayName := Trim(Info^.ServerSession.ServerAlias)
        else
          DisplayName := Trim(Node.Text);
      end
      else
        DisplayName := Trim(Node.Text);

      if DisplayName <> '' then
        Result.Add(DisplayName);
    end;
  end;
end;

{ ============================================================
  Liefert die ECHTEN Servernamen aller Server-Knoten (Level 0).

  Verwendet GetServerNameFromNode(), damit auch dann der
  korrekte Servername zurückkommt, wenn die TreeView künftig
  den Alias anzeigt (ServerSession.ServerName statt Node.Text).

  Diese Funktion ist der zentrale Lookup für interne Zwecke:
    - Server-Auswahl in DB-Registrierungs-Dialogen
    - Clone-Dialoge (Quell-/Ziel-Server)
    - Jede Stelle, die den Server-Record nachschlagen muss

  Für die Anzeige in UI-Elementen (ComboBoxen etc.)
  stattdessen GetServerAliasListFromTreeView() verwenden.
  ============================================================ }
function GetServerListFromTreeView: TStringList;
var
  i: Integer;
  Node: TTreeNode;
  ServerName: string;
begin
  Result := nil;

  if MainTreeView = nil then
    Exit;
  if MainTreeView.Items.Count = 0 then
    Exit;

  Result := TStringList.Create;

  for i := 0 to MainTreeView.Items.Count - 1 do
  begin
    Node := MainTreeView.Items[i];

    if (Node.Level = 0) and (Trim(Node.Text) <> '') then
    begin
      ServerName := GetServerNameFromNode(Node);

      if ServerName <> '' then
        Result.Add(ServerName);
    end;
  end;
end;

{ ============================================================
  Liefert Verbindungsstrings der Form "Servername/Port" für
  alle Server-Knoten (Level 0).

  Verwendet GetServerNameFromNode(), damit auch dann der
  korrekte Servername zurückkommt, wenn die TreeView künftig
  den Alias anzeigt.

  Bei Embedded-Verbindungen (lokal) wird der Port weggelassen,
  weil er dort keine Rolle spielt.

  Defensive Prüfungen:
    - MainTreeView = nil
    - Node.Data = nil
    - ServerSession = nil

  Verhalten bei fehlender Session: Fallback auf Node.Text
  ohne Port (konservativ, damit kein Crash entsteht).
  ============================================================ }
function GetServerAndPortListFromTreeView: TStringList;
var
  i: Integer;
  Node: TTreeNode;
  Info: TPNodeInfos;
  ServerName: string;
  hStr: string;
begin
  Result := nil;

  if MainTreeView = nil then
    Exit;
  if MainTreeView.Items.Count = 0 then
    Exit;

  Result := TStringList.Create;

  for i := 0 to MainTreeView.Items.Count - 1 do
  begin
    Node := MainTreeView.Items[i];

    if (Node.Level = 0) and (Trim(Node.Text) <> '') then
    begin
      ServerName := GetServerNameFromNode(Node);
      if ServerName = '' then
        Continue;

      hStr := ServerName;

      // Port nur anhängen, wenn eine Session existiert und
      // es sich NICHT um eine Embedded-Verbindung handelt.
      if Assigned(Node.Data) then
      begin
        Info := TPNodeInfos(Node.Data);

        if Assigned(Info^.ServerSession)
           and (not Info^.ServerSession.IsEmbedded)
           and (Trim(Info^.ServerSession.Port) <> '') then
          hStr := hStr + '/' + Trim(Info^.ServerSession.Port);
      end;

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
var TempStr: string;
begin
  //FireBird
  //InitialFBClientLibPath := fIniFile.ReadString('FireBird',  'InitialFBClientLib', '/opt/firebird/lib/libfbclient.so');
  InitialFBClientLibPath := fIniFile.ReadString('FireBird',  'InitialFBClientLib', '');
  //UI
  Language  := fIniFile.ReadString('UI',  'Language', 'eng');

  //Backup
  CloseDBBeforeBackup :=  fIniFile.ReadBool('Backup',  'CloseDBBeforeBackup', true);

  //Restore
  DefaultPageSize        := fIniFile.ReadInteger('Restore', 'DefaultPageSize', 8192);
  DefaultNumBuffers      := fIniFile.ReadInteger('Restore', 'DefaultNumBuffers', 2048);
  RegisterDBAfterRestore := fIniFile.ReadBool('Restore',  'RegisterDBAfterRestore', true);
  DefaultSegmentSize     := fIniFile.ReadString('Restore',  'DefaultSegmentSize', '64');
  DefaultSegmentUnit     := fIniFile.ReadString('Restore',  'DefaultSegmentUnit', 'MB');

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

  //MetaData
  AlwaysQuoteIdentifiers   := fIniFile.ReadBool('MetaData',  'AlwaysQuoteIdentifiers', true);
  CaseSensitiveObjectNames := fIniFile.ReadBool('MetaData',  'CaseSensitiveObjectNames', true);
  ShowSystem               := fIniFile.ReadBool('MetaData',  'ShowSystem', false);

  //IntelliSense
  ReloadCacheOnNodeExpand := fIniFile.ReadBool('IntelliSense',  'ReloadCacheOnNodeExpand', false);

  //MetaDataTableExtract
  ExtractDomains := fIniFile.ReadBool('MetaDataTableExtract',  'ExtractDomains', true);
  ExtractIndex   := fIniFile.ReadBool('MetaDataTableExtract',  'ExtractIndex', true);
  ExtractForeign := fIniFile.ReadBool('MetaDataTableExtract',  'ExtractForeign', true);
  ExtractCheck   := fIniFile.ReadBool('MetaDataTableExtract',  'ExtractCheck', true);
  ExtractTrigger := fIniFile.ReadBool('MetaDataTableExtract',  'ExtractTrigger', true);
  ExtractGrant   := fIniFile.ReadBool('MetaDataTableExtract',  'ExtractGrant', true);
  ExtractData    := fIniFile.ReadBool('MetaDataTableExtract',  'ExtractData', false);


  //Theme
  AllowIniOverrides           :=  fIniFile.ReadBool('UI',  'AllowIniOverrides', true);

  //MainTreeView
  MainTreeViewAlwaysRefresh   :=  fIniFile.ReadBool('MainTreeView',  'AlwaysRefresh', false);
  MainTreeViewColor           :=  StringToColor(fIniFile.Readstring('MainTreeView',  'FontColor', 'clRed'));
  MainTreeViewFontName        :=  fIniFile.Readstring('MainTreeView',  'FontName', 'Arial');
  MainTreeViewFontStyle       :=  StrToFontStyles(fIniFile.Readstring('MainTreeView',  'FontStyle', 'fsNormal'));
  MainTreeViewFontSize        :=  fIniFile.ReadInteger('MainTreeView',  'FontSize', 14);
  MainTreeViewFontColor       :=  StringToColor(fIniFile.Readstring('MainTreeView',  'FontColor', 'clRed'));
  AutoFilter                  :=  fIniFile.ReadBool('MainTreeView',  'AutoFilter', false);

  //QueryWindow
  QWAutoCommit             := fIniFile.ReadBool('QueryWindow',  'AutoCommit', true);
  QWExecMultiStatementsAsScript := fIniFile.ReadBool('QueryWindow',  'ExecMultiStatementsAsScript', true);
  QWShowNavigator          :=  fIniFile.ReadBool('QueryWindow',  'ShowNavigator', true);
  QWEditorBackgroundColor  :=  StringToColor(fIniFile.ReadString('QueryWindow',  'BackgroundColor', 'clSilver'));
  QWEditorFontName         :=  fIniFile.ReadString('QueryWindow',  'FontName', 'Courier New');
  QWEditorFontSize         :=  fIniFile.ReadInteger('QueryWindow',  'FontSize', 10);
  QWEditorFontColor        :=  StringToColor(fIniFile.ReadString('QueryWindow',  'FontColor', 'clBlack'));
  QWEditorFontStyle        :=  StrToFontStyles(fIniFile.ReadString('QueryWindow',  'FontStyle', 'Arial'));

  //TransactionConfig-Defaults
  DefTxFileName := fIniFile.ReadString('TransactionDefaults','TxFileName','read_committed.txt');


  //CSVExport
  DefaultBatchSize := fIniFile.ReadInteger('CSVExport','DefaultBatchSize', 500000);
  CSVExportDefaultPreset    := fIniFile.ReadString('CSVExport','DefaultPreset', 'csv_quoted');

  //CSV Editor
  CSVDefaultFieldLength := fIniFile.ReadInteger('CSVEditor','CSVDefaultFieldLength',50);
  TempStr := fIniFile.ReadString('CSVEditor','CSVDelimiter',',');
  if TempStr <> '' then
    CSVDelimiter := TempStr[1]
  else
    CSVDelimiter := ',';

  TempStr := fIniFile.ReadString('CSVEditor','CSVQuoteChar','"');
  if TempStr <> '' then
    CSVQuoteChar := TempStr[1]
  else
    CSVQuoteChar := '"';

  TempStr := fIniFile.ReadString('CSVEditor','CSVLineEnding','LF');
  CSVLineEnding := CSVLineEndingNameToStr(TempStr);

  CSVFirstLineAsFieldNames := fIniFile.ReadBool('CSVEditor','CSVFirstLineAsFieldNames',True);
  CSVIgnoreOuterWhitespace := fIniFile.ReadBool('CSVEditor','CSVIgnoreOuterWhitespace',True);
  CSVQuoteOuterWhitespace  := fIniFile.ReadBool('CSVEditor','CSVQuoteOuterWhitespace',False);

  // Auto-Detect-Schalter — Default: alle an
  AutoDetectLineEnding := fIniFile.ReadBool('CSVEditor','AutoDetectLineEnding', True);
  AutoDetectDelimiter  := fIniFile.ReadBool('CSVEditor','AutoDetectDelimiter',  True);
  AutoDetectQuoteChar  := fIniFile.ReadBool('CSVEditor','AutoDetectQuoteChar',  True);

  // Performance-Parameter (TSVFastDataset) — in KB bzw. Anzahl
  CSVBlockSizeBytes           := fIniFile.ReadInteger('CSVEditor','CSVBlockSize', 1024);
  CSVBufferSizeBytes          := fIniFile.ReadInteger('CSVEditor','CSVBufferSize', 2048);
  CSVInitialCellCapacity   := fIniFile.ReadInteger('CSVEditor','CSVInitialCellCapacity', 64);
  CSVRowWidthScanMB         := fIniFile.ReadInteger('CSVEditor','CSVRowWidthScanMB', 10);
  CSVMinRowsForRowWidthScan := fIniFile.ReadInteger('CSVEditor','CSVMinRowsForRowWidthScan', 1000);

  // Sicherheitskorrektur: BufferSize muss >= BlockSize * 2 sein
  if CSVBufferSizeBytes < CSVBlockSizeBytes * 2 then
    CSVBufferSizeBytes := CSVBlockSizeBytes * 2;

  CSVAutoTuneCSVBuffer := fIniFile.ReadBool('CSVEditor', 'CSVAutoTuneCSVBuffer', true);

  CSVProgressUpdatePercent := fIniFile.ReadInteger('CSVEditor','CSVProgressUpdatePercent', 1);


  //UpdateChecker
  AutoSearchOnProgramStart := fIniFile.ReadBool('UpdateChecker','AutoSearchOnProgramStart', True);
  UnzipAfterDownload := fIniFile.ReadBool('UpdateChecker','UnzipAfterDownload', True);
  RunNewVersionAfterUnzip:= fIniFile.ReadBool('UpdateChecker','RunNewVersionAfterUnzip', false);
  DeleteOldVersions := fIniFile.ReadBool('UpdateChecker','DeleteOldVersions', false);


  //Database
  NumBuffers := fIniFile.ReadInteger('Database','NumBuffers', 1024);


  //SQLMonitor
  TraceEnabled := fIniFile.ReadBool('SQLMonitor','TraceEnabled', false);
  TraceFlags   := fIniFile.ReadString('SQLMonitor','TraceFlags', '');
  SlowQueryThreshold := fIniFile.ReadInteger('SQLMonitor', 'SlowQueryThreshold', 5000);
end;

procedure WriteIniFile;
var TempStr : string;
begin
  fIniFile.WriteString('UI', 'Language', Language);

  //Backup
  fIniFile.WriteBool('Backup',  'CloseDBBeforeBackup', CloseDBBeforeBackup);

  //Restore
  fIniFile.WriteInteger('Restore', 'DefaultPageSize', DefaultPageSize);
  fIniFile.WriteInteger('Restore', 'DefaultNumBuffers', DefaultNumBuffers);
  fIniFile.WriteBool('Restore',  'RegisterDBAfterRestore', RegisterDBAfterRestore);
  fIniFile.WriteString('Restore',  'DefaultSegmentSize', DefaultSegmentSize);
  fIniFile.WriteString('Restore',  'DefaultSegmentUnit', DefaultSegmentUnit);

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

  //MetaData
  fIniFile.WriteBool('MetaData',  'AlwaysQuoteIdentifiers', AlwaysQuoteIdentifiers);
  fIniFile.WriteBool('MetaData',  'CaseSensitiveObjectNames', CaseSensitiveObjectNames);
  fIniFile.WriteBool('MetaData',  'ShowSystem', ShowSystem);

  //IntelliSense
  fIniFile.WriteBool('IntelliSense',  'ReloadCacheOnNodeExpand', ReloadCacheOnNodeExpand);


  //MetaDataTableExtract
  fIniFile.WriteBool('MetaDataTableExtract',  'ExtractDomains', ExtractDomains);
  fIniFile.WriteBool('MetaDataTableExtract',  'ExtractIndex', ExtractIndex);
  fIniFile.WriteBool('MetaDataTableExtract',  'ExtractForeign', ExtractForeign);
  fIniFile.WriteBool('MetaDataTableExtract',  'ExtractCheck', ExtractCheck);
  fIniFile.WriteBool('MetaDataTableExtract',  'ExtractTrigger', ExtractTrigger);
  fIniFile.WriteBool('MetaDataTableExtract',  'ExtractGrant', ExtractGrant);
  fIniFile.WriteBool('MetaDataTableExtract',  'ExtractData', ExtractData);

  //Theme
  fIniFile.WriteBool('UI',  'AllowIniOverrides', AllowIniOverrides);

  //MainTreeView
  fIniFile.WriteBool('MainTreeView',  'AlwaysRefresh', MainTreeViewAlwaysRefresh);
  fIniFile.WriteString('MainTreeView',  'FontName', MainTreeViewFontName);
  fIniFile.WriteString('MainTreeView',  'FontStyle', FontStylesToStr(MainTreeViewFontStyle));
  fIniFile.WriteInteger('MainTreeView',  'FontSize', MainTreeViewFontSize);
  fIniFile.WriteString('MainTreeView',  'FontColor', ColorToString(MainTreeViewFontColor));
  fIniFile.WriteBool('MainTreeView',  'AutoFilter', AutoFilter);

  //QueryWindow
  fIniFile.WriteBool('QueryWindow',  'AutoCommit', QWAutoCommit);
  fIniFile.WriteBool('QueryWindow',  'ExecMultiStatementsAsScript', QWExecMultiStatementsAsScript);
  fIniFile.WriteBool('QueryWindow',  'ShowNavigator', QWShowNavigator);
  fIniFile.WriteString('QueryWindow',  'BackgroundColor', ColorToString(QWEditorBackgroundColor));
  fIniFile.WriteString('QueryWindow',  'FontName', QWEditorFontName);
  fIniFile.WriteInteger('QueryWindow',  'FontSize', QWEditorFontSize);
  fIniFile.WriteString('QueryWindow',  'FontColor', ColorToString(QWEditorFontColor));
  fIniFile.WriteString('QueryWindow',  'FontStyle', FontStylesToStr(QWEditorFontStyle));

  //Transaction-Default config
  fIniFile.WriteString('TransactionDefaults', 'TxFileName', DefTxFileName);

  //CSVExport
  fIniFile.WriteInteger('CSVExport', 'DefaultBatchSize', DefaultBatchSize);
  fIniFile.ReadString('CSVkExport','DefaultPreset', CSVExportDefaultPreset);

  // CSV Editor
  if CSVLineEnding = #13#10 then
    TempStr := 'CRLF'
  else if CSVLineEnding = #10 then
    TempStr := 'LF'
  else if CSVLineEnding = #13 then
    TempStr := 'CR'
  else if CSVLineEnding = LineEnding then
    TempStr := 'SYSTEM'
  else
    TempStr := 'LF';
  fIniFile.WriteString('CSVEditor','CSVLineEnding', TempStr);
  fIniFile.WriteInteger('CSVEditor','CSVDefaultFieldLength',CSVDefaultFieldLength);
  fIniFile.WriteString('CSVEditor','CSVDelimiter',CSVDelimiter);
  fIniFile.WriteString('CSVEditor','CSVQuoteChar',CSVQuoteChar);
  fIniFile.WriteBool('CSVEditor','CSVFirstLineAsFieldNames',CSVFirstLineAsFieldNames);
  fIniFile.WriteBool('CSVEditor','CSVIgnoreOuterWhitespace',CSVIgnoreOuterWhitespace);
  fIniFile.WriteBool('CSVEditor','CSVQuoteOuterWhitespace',CSVQuoteOuterWhitespace);
  // Auto-Detect-Schalter
  fIniFile.WriteBool('CSVEditor','AutoDetectLineEnding', AutoDetectLineEnding);
  fIniFile.WriteBool('CSVEditor','AutoDetectDelimiter',  AutoDetectDelimiter);
  fIniFile.WriteBool('CSVEditor','AutoDetectQuoteChar',  AutoDetectQuoteChar);
  // Performance-Parameter
  fIniFile.WriteInteger('CSVEditor','CSVBlockSize',          CSVBlockSizeBytes);
  fIniFile.WriteInteger('CSVEditor','CSVBufferSize',         CSVBufferSizeBytes);
  fIniFile.WriteInteger('CSVEditor','CSVInitialCellCapacity', CSVInitialCellCapacity);
  fIniFile.WriteInteger('CSVEditor','CSVRowWidthScanMB', CSVRowWidthScanMB);
  fIniFile.WriteInteger('CSVEditor','CSVMinRowsForRowWidthScan', CSVMinRowsForRowWidthScan);
  fIniFile.WriteInteger('CSVEditor','CSVProgressUpdatePercent', CSVProgressUpdatePercent);
  fIniFile.WriteBool('CSVEditor','CSVAutoTuneCSVBuffer', CSVAutoTuneCSVBuffer);

  //UpdateChecker
  fIniFile.WriteBool('UpdateChecker','AutoSearchOnProgramStart', AutoSearchOnProgramStart);
  fIniFile.WriteBool('UpdateChecker','UnzipAfterDownload', UnzipAfterDownload);
  fIniFile.WriteBool('UpdateChecker','RunNewVersionAfterUnzip', RunNewVersionAfterUnzip);
  fIniFile.WriteBool('UpdateChecker','DeleteOldVersions', DeleteOldVersions);

  //Database
  fIniFile.WriteInteger('Database','NumBuffers', NumBuffers);

  //SQLMonitor
  fIniFile.WriteBool('SQLMonitor','TraceEnabled', TraceEnabled);
  fIniFile.WriteString('SQLMonitor','TraceFlags', TraceFlags);
  fIniFile.WriteInteger('SQLMonitor', 'SlowQueryThreshold', SlowQueryThreshold);
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

procedure UpdateFilesFromList(const DataDir, TempDataDir: string);
var
  UpdateList: TStringList;
  i: Integer;
  SourceFile, TargetFile: string;
  UpdateFilePath: string;
begin
  UpdateFilePath := TempDataDir + PathDelim + 'FilesToUpdate.txt';

  if not FileExists(UpdateFilePath) then
    Exit;

  UpdateList := TStringList.Create;
  try
    UpdateList.LoadFromFile(UpdateFilePath);

    for i := 0 to UpdateList.Count - 1 do
    begin
      // Kommentare und Leerzeilen überspringen
      if (Trim(UpdateList[i]) = '') or (UpdateList[i][1] = '#') then
        Continue;

      SourceFile := IncludeTrailingPathDelimiter(TempDataDir) + UpdateList[i];
      TargetFile := IncludeTrailingPathDelimiter(DataDir) + UpdateList[i];

      if FileExists(SourceFile) then
      begin
        // Zielverzeichnis sicherstellen
        ForceDirectories(ExtractFilePath(TargetFile));
        // Datei überschreiben
        CopyFile(SourceFile, TargetFile);
      end;
    end;
  finally
    UpdateList.Free;
  end;
end;

procedure CopyMissingFiles(const SourceDir, TargetDir: string);
var
  SearchRec: TSearchRec;
  SourceFile, TargetFile: string;
begin
  if not DirectoryExists(SourceDir) then
    Exit;

  ForceDirectories(TargetDir);

  if FindFirst(SourceDir + PathDelim + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;

      SourceFile := SourceDir + PathDelim + SearchRec.Name;
      TargetFile := TargetDir + PathDelim + SearchRec.Name;

      if (SearchRec.Attr and faDirectory <> 0) then
      begin
        // Unterverzeichnis rekursiv kopieren
        CopyMissingFiles(SourceFile, TargetFile);
      end
      else
      begin
        // Datei nur kopieren wenn sie noch nicht existiert
        if not FileExists(TargetFile) then
          CopyFile(SourceFile, TargetFile);
      end;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure ExtractResourcesIfNeeded;
var
  DataStream: TResourceStream;
  MemoryStream: TMemoryStream;
  AbUnZipper: TAbUnZipper;
  DataDir, TempDir: string;
  IsFirstRun: Boolean;
begin
  {$IFDEF WINDOWS}
  DataDir := ExtractFilePath(Application.ExeName) + '\data';
  {$ELSE}
  DataDir := ExtractFilePath(Application.ExeName) + '/data';
  {$ENDIF}

  IsFirstRun := FirstRun;

  if IsFirstRun then
  begin
    // Komplett extrahieren wie bisher
    DataStream := TResourceStream.Create(HInstance, 'DATA', RT_RCDATA);
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.LoadFromStream(DataStream);
      MemoryStream.Position := 0;
      AbUnZipper := TAbUnZipper.Create(nil);
      try
        AbUnZipper.Stream := MemoryStream;
        AbUnZipper.ExtractOptions := [eoCreateDirs, eoRestorePath];
        AbUnZipper.BaseDirectory := ExtractFilePath(Application.ExeName);
        AbUnZipper.ExtractFiles('*.*');
      finally
        AbUnZipper.Free;
      end;
    finally
      MemoryStream.Free;
      DataStream.Free;
    end;
  end
  else
  begin
    // DataDir existiert bereits
    // 1. Ins Temp-Verzeichnis entpacken
    TempDir := GetTempDir + 'turbobird_update' + PathDelim;
    ForceDirectories(TempDir);

    try
      DataStream := TResourceStream.Create(HInstance, 'DATA', RT_RCDATA);
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.LoadFromStream(DataStream);
        MemoryStream.Position := 0;
        AbUnZipper := TAbUnZipper.Create(nil);
        try
          AbUnZipper.Stream := MemoryStream;
          AbUnZipper.ExtractOptions := [eoCreateDirs, eoRestorePath];
          AbUnZipper.BaseDirectory := TempDir;
          AbUnZipper.ExtractFiles('*.*');
        finally
          AbUnZipper.Free;
        end;
      finally
        MemoryStream.Free;
        DataStream.Free;
      end;

      // 2. Fehlende Dateien aus TempDir nach DataDir kopieren
      CopyMissingFiles(IncludeTrailingPathDelimiter(TempDir) + 'data', DataDir);

      // 3. FilesToUpdate.txt einlesen und gelistete Dateien überschreiben
      UpdateFilesFromList(DataDir, IncludeTrailingPathDelimiter(TempDir) + 'data');

    finally
      // TempDir aufräumen
      DeleteDirectory(TempDir, True);
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

// ============================================================
// Entfernt doppelte Anführungszeichen (Identifier-Quotes).
// Wird für RDB$-Lookups verwendet — RDB$ speichert Identifier
// ohne Quotes.
//
// WICHTIG: Nicht zu verwechseln mit StripQuotes — das entfernt
// einfache Anführungszeichen (SQL-String-Literale).
// ============================================================
function StripIdentifierQuotes(const AName: string): string;
var
  S: string;
begin
  S := Trim(AName);
  if (Length(S) >= 2) and (S[1] = '"') and (S[Length(S)] = '"') then
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

    // ============================================================
    // Root-Typen auf sich selbst (Idempotenz)
    // ============================================================
    tvotServer:                     Result := tvotServer;
    tvotEmbeddedServer:             Result := tvotEmbeddedServer;
    tvotDatabase:                   Result := tvotDatabase;
    tvotQueryWindow:                Result := tvotQueryWindow;

    tvotTableRoot:                  Result := tvotTableRoot;
    tvotTableFieldRoot:             Result := tvotTableFieldRoot;
    tvotConstraintRoot:             Result := tvotConstraintRoot;
    tvotPrimaryKeyRoot:             Result := tvotPrimaryKeyRoot;
    tvotForeignKeyRoot:             Result := tvotForeignKeyRoot;
    tvotUniqueConstraintRoot:       Result := tvotUniqueConstraintRoot;
    tvotCheckConstraintRoot:        Result := tvotCheckConstraintRoot;
    tvotNotNullConstraintRoot:      Result := tvotNotNullConstraintRoot;
    tvotIndexRoot:                  Result := tvotIndexRoot;
    tvotTableReferencesRoot:        Result := tvotTableReferencesRoot;
    tvotTablePermissionsRoot:       Result := tvotTablePermissionsRoot;

    tvotGeneratorRoot:              Result := tvotGeneratorRoot;
    tvotTriggerRoot:                Result := tvotTriggerRoot;
    tvotTableTriggerRoot:           Result := tvotTableTriggerRoot;
    tvotDBTriggerRoot:              Result := tvotDBTriggerRoot;
    tvotDDLTriggerRoot:             Result := tvotDDLTriggerRoot;
    tvotUDRTriggerRoot:             Result := tvotUDRTriggerRoot;
    tvotUDRTableTriggerRoot:        Result := tvotUDRTableTriggerRoot;
    tvotUDRDBTriggerRoot:           Result := tvotUDRDBTriggerRoot;
    tvotUDRDDLTriggerRoot:          Result := tvotUDRDDLTriggerRoot;

    tvotViewRoot:                   Result := tvotViewRoot;
    tvotDomainRoot:                 Result := tvotDomainRoot;
    tvotRoleRoot:                   Result := tvotRoleRoot;
    tvotExceptionRoot:              Result := tvotExceptionRoot;
    tvotUserRoot:                   Result := tvotUserRoot;

    tvotProcedureRoot:              Result := tvotProcedureRoot;
    tvotFunctionRoot:               Result := tvotFunctionRoot;
    tvotUDRoot:                     Result := tvotUDRoot;
    tvotUDFRoot:                    Result := tvotUDFRoot;
    tvotUDRRoot:                    Result := tvotUDRRoot;
    tvotUDRFunctionRoot:            Result := tvotUDRFunctionRoot;
    tvotUDRProcedureRoot:           Result := tvotUDRProcedureRoot;

    tvotPackageRoot:                Result := tvotPackageRoot;
    tvotPackageUDFFunctionRoot:     Result := tvotPackageUDFFunctionRoot;
    tvotPackageFunctionRoot:        Result := tvotPackageFunctionRoot;
    tvotPackageProcedureRoot:       Result := tvotPackageProcedureRoot;
    tvotPackageUDRFunctionRoot:     Result := tvotPackageUDRFunctionRoot;
    tvotPackageUDRProcedureRoot:    Result := tvotPackageUDRProcedureRoot;
    tvotPackageTriggerRoot:         Result := tvotPackageTriggerRoot;
    tvotPackageUDRTriggerRoot:      Result := tvotPackageUDRTriggerRoot;

    tvotDatabaseRoot:               Result := tvotDatabaseRoot;
    tvotBLOBFilterRoot:             Result := tvotBLOBFilterRoot;
    tvotCommentRoot:                Result := tvotCommentRoot;
    tvotDataRoot:                   Result := tvotDataRoot;

    tvotSystemObjectRoot:           Result := tvotSystemObjectRoot;
    tvotSystemTableRoot:            Result := tvotSystemTableRoot;
    tvotSystemTableFieldRoot:       Result := tvotSystemTableFieldRoot;
    tvotSystemDomainRoot:           Result := tvotSystemDomainRoot;
    tvotSystemGeneratorRoot:        Result := tvotSystemGeneratorRoot;
    tvotSystemTriggerRoot:          Result := tvotSystemTriggerRoot;
    tvotSystemConstraintRoot:       Result := tvotSystemConstraintRoot;
    tvotSystemIndexRoot:            Result := tvotSystemIndexRoot;
    tvotSystemRoleRoot:             Result := tvotSystemRoleRoot;
    tvotSystemUserRoot:             Result := tvotSystemUserRoot;
    tvotSystemExceptionRoot:        Result := tvotSystemExceptionRoot;

    tvotFormRoot:                   Result := tvotFormRoot;

    // ============================================================
    // Einzel-Objekte → ihr Root
    // ============================================================
    tvotTable:                      Result := tvotTableRoot;
    tvotTableField:                 Result := tvotTableFieldRoot;
    tvotConstraint:                 Result := tvotConstraintRoot;
    tvotPrimaryKey:                 Result := tvotPrimaryKeyRoot;
    tvotForeignKey:                 Result := tvotForeignKeyRoot;
    tvotUniqueConstraint:           Result := tvotUniqueConstraintRoot;
    tvotCheckConstraint:            Result := tvotCheckConstraintRoot;
    tvotNotNullConstraint:          Result := tvotNotNullConstraintRoot;
    tvotIndex:                      Result := tvotIndexRoot;
    tvotTableReference:             Result := tvotTableReferencesRoot;   // ← NEU

    tvotGenerator:                  Result := tvotGeneratorRoot;
    tvotTrigger:                    Result := tvotTriggerRoot;
    tvotTableTrigger:               Result := tvotTableTriggerRoot;
    tvotDBTrigger:                  Result := tvotDBTriggerRoot;
    tvotDDLTrigger:                 Result := tvotDDLTriggerRoot;
    tvotUDRTrigger:                 Result := tvotUDRTriggerRoot;
    tvotUDRTableTrigger:            Result := tvotUDRTableTriggerRoot;
    tvotUDRDBTrigger:               Result := tvotUDRDBTriggerRoot;
    tvotUDRDDLTrigger:              Result := tvotUDRDDLTriggerRoot;

    tvotView:                       Result := tvotViewRoot;
    tvotDomain:                     Result := tvotDomainRoot;
    tvotRole:                       Result := tvotRoleRoot;
    tvotException:                  Result := tvotExceptionRoot;
    tvotUser:                       Result := tvotUserRoot;

    tvotProcedure:                  Result := tvotProcedureRoot;
    tvotFunction:                   Result := tvotFunctionRoot;
    tvotUDF:                        Result := tvotUDFRoot;
    tvotUDRFunction:                Result := tvotUDRFunctionRoot;
    tvotUDRProcedure:               Result := tvotUDRProcedureRoot;

    tvotPackage:                    Result := tvotPackageRoot;
    tvotPackageUDFFunction:         Result := tvotPackageUDFFunctionRoot;
    tvotPackageFunction:            Result := tvotPackageFunctionRoot;
    tvotPackageProcedure:           Result := tvotPackageProcedureRoot;
    tvotPackageUDRFunction:         Result := tvotPackageUDRFunctionRoot;
    tvotPackageUDRProcedure:        Result := tvotPackageUDRProcedureRoot;
    tvotPackageTrigger:             Result := tvotPackageTriggerRoot;
    tvotPackageUDRTrigger:          Result := tvotPackageUDRTriggerRoot;

    tvotBLOBFilter:                 Result := tvotBLOBFilterRoot;
    tvotComment:                    Result := tvotCommentRoot;
    tvotData:                       Result := tvotDataRoot;

    tvotSystemTable:                Result := tvotSystemTableRoot;
    tvotSystemTableField:           Result := tvotSystemTableFieldRoot;
    tvotSystemDomain:               Result := tvotSystemDomainRoot;
    tvotSystemGenerator:            Result := tvotSystemGeneratorRoot;
    tvotSystemTrigger:              Result := tvotSystemTriggerRoot;
    tvotSystemConstraint:           Result := tvotSystemConstraintRoot;
    tvotSystemIndex:                Result := tvotSystemIndexRoot;
    tvotSystemRole:                 Result := tvotSystemRoleRoot;
    tvotSystemUser:                 Result := tvotSystemUserRoot;
    tvotSystemException:            Result := tvotSystemExceptionRoot;

    tvotForm:                       Result := tvotFormRoot;

  else
    Result := tvotNone;
  end;
end;

function FBObjectToStr(AType: TObjectType): string;
begin
  case AType of
    // --- Allgemein ---
    otNone:                  Result := 'None';

    // --- User-Objekte ---
    otTables:                Result := 'Table';
    otTableFields:           Result := 'Table Field';
    otViews:                 Result := 'View';
    otGenerators:            Result := 'Generator';
    otSequences:             Result := 'Sequence';

    otTriggers:              Result := 'Trigger';
    otTableTriggers:         Result := 'Table Trigger';
    otDBTriggers:            Result := 'Database Trigger';
    otDDLTriggers:           Result := 'DDL Trigger';

    otUDF:                   Result := 'User-Defined Function';
    otRoles:                 Result := 'Role';
    otUsers:                 Result := 'User';
    otDomains:               Result := 'Domain';
    otIndexes:               Result := 'Index';
    otTableReferences:       Result := 'Reference';
    otExceptions:            Result := 'Exception';

    otConstraints:           Result := 'Constraint';
    otPrimaryKeys:           Result := 'Primary Key';
    otForeignKeys:           Result := 'Foreign Key';
    otUniqueConstraints:     Result := 'Unique Constraint';
    otCheckConstraints:      Result := 'Check Constraint';
    otNotNullConstraints:    Result := 'Not Null Constraint';

    otFunctions:             Result := 'Function';
    otProcedures:            Result := 'Procedure';

    // --- UDR / Package ---
    otUDRFunctions:          Result := 'UDR Function';
    otUDRProcedures:         Result := 'UDR Procedure';
    otUDRTriggers:           Result := 'UDR Trigger';
    otPackages:              Result := 'Package';
    otPackageFunctions:      Result := 'Package Function';
    otPackageProcedures:     Result := 'Package Procedure';
    otPackageUDFFunctions:   Result := 'Package UDF Function';
    otPackageUDRFunctions:   Result := 'Package UDR Function';
    otPackageUDRProcedures:  Result := 'Package UDR Procedure';
    otPackageUDRTriggers:    Result := 'Package UDR Trigger';

    // --- Datenbank / Metadaten ---
    otDatabase:              Result := 'Database';
    otBLOBFilters:           Result := 'BLOB Filter';
    otComments:              Result := 'Comment';
    otData:                  Result := 'Data';

    // --- System-Objekte ---
    otSystemTables:          Result := 'System Table';
    otSystemDomains:         Result := 'System Domain';
    otSystemGenerators:      Result := 'System Generator';
    otSystemTriggers:        Result := 'System Trigger';
    otSystemConstraints:     Result := 'System Constraint';
    otSystemIndexes:         Result := 'System Index';
    otSystemRoles:           Result := 'System Role';
    otSystemUsers:           Result := 'System User';
    otSystemExceptions:      Result := 'System Exception';
  else
    Result := 'Unknown';
  end;
end;

function FBTypeToTreeViewType(AType: TObjectType): TTreeViewObjectType;
begin
  case AType of
    otNone:                  Result := tvotNone;

    // --- Tables / Views ---
    otTables:                Result := tvotTable;
    otTableFields:           Result := tvotTableField;
    otViews:                 Result := tvotView;

    otSystemTableFields:     Result := tvotSystemTableField;

    // --- Generators / Sequences ---
    otGenerators,
    otSequences:             Result := tvotGenerator;

    // --- Triggers ---
    otTriggers:              Result := tvotTrigger;
    otTableTriggers:         Result := tvotTableTrigger;
    otDBTriggers:            Result := tvotDBTrigger;
    otDDLTriggers:           Result := tvotDDLTrigger;

    // --- Security / Types ---
    otDomains:               Result := tvotDomain;
    otRoles:                 Result := tvotRole;
    otUsers:                 Result := tvotUser;
    otExceptions:            Result := tvotException;

    // --- Indexes / Constraints ---
    otIndexes:               Result := tvotIndex;
    otTableReferences:       Result := tvotTableReference;
    otConstraints:           Result := tvotConstraint;
    otPrimaryKeys:           Result := tvotPrimaryKey;
    otForeignKeys:           Result := tvotForeignKey;
    otUniqueConstraints:     Result := tvotUniqueConstraint;
    otCheckConstraints:      Result := tvotCheckConstraint;
    otNotNullConstraints:    Result := tvotNotNullConstraint;

    // --- Procedures / Functions ---
    otProcedures:            Result := tvotProcedure;
    otFunctions:             Result := tvotFunction;
    otUDF:                   Result := tvotUDF;

    // --- UDR ---
    otUDRFunctions:          Result := tvotUDRFunction;
    otUDRProcedures:         Result := tvotUDRProcedure;
    otUDRTriggers:           Result := tvotUDRTrigger;

    // --- Packages ---
    otPackages:              Result := tvotPackage;
    otPackageFunctions:      Result := tvotPackageFunction;
    otPackageProcedures:     Result := tvotPackageProcedure;
    otPackageUDFFunctions:   Result := tvotPackageUDFFunction;
    otPackageUDRFunctions:   Result := tvotPackageUDRFunction;
    otPackageUDRProcedures:  Result := tvotPackageUDRProcedure;
    otPackageUDRTriggers:    Result := tvotPackageUDRTrigger;

    // --- Database / Metadata ---
    otDatabase:              Result := tvotDatabase;
    otBLOBFilters:           Result := tvotBLOBFilter;
    otComments:              Result := tvotComment;
    otData:                  Result := tvotData;

    // --- System Objects ---
    otSystemTables:          Result := tvotSystemTable;
    otSystemDomains:         Result := tvotSystemDomain;
    otSystemGenerators:      Result := tvotSystemGenerator;
    otSystemTriggers:        Result := tvotSystemTrigger;
    otSystemConstraints:     Result := tvotSystemConstraint;
    otSystemIndexes:         Result := tvotSystemIndex;
    otSystemRoles:           Result := tvotSystemRole;
    otSystemUsers:           Result := tvotSystemUser;
    otSystemExceptions:      Result := tvotSystemException;
  else
    Result := tvotNone;
  end;
end;

function TreeViewTypeToFBType(AType: TTreeViewObjectType): TObjectType;
begin
  case AType of
    tvotNone:                    Result := otNone;

    // --- Tables / Views ---
    tvotTable:                   Result := otTables;
    tvotTableField:              Result := otTableFields;
    tvotView:                    Result := otViews;

    // --- Generators ---
    tvotGenerator:               Result := otGenerators;

    // --- Triggers ---
    tvotTrigger:                 Result := otTriggers;
    tvotTableTrigger:            Result := otTableTriggers;
    tvotDBTrigger:               Result := otDBTriggers;
    tvotDDLTrigger:              Result := otDDLTriggers;

    // --- Security / Types ---
    tvotDomain:                  Result := otDomains;
    tvotRole:                    Result := otRoles;
    tvotUser:                    Result := otUsers;
    tvotException:               Result := otExceptions;

    // --- Indexes / Constraints ---
    tvotIndex:                   Result := otIndexes;
    tvotTableReference:          Result := otTableReferences;
    tvotConstraint:              Result := otConstraints;
    tvotPrimaryKey:              Result := otPrimaryKeys;
    tvotForeignKey:              Result := otForeignKeys;
    tvotUniqueConstraint:        Result := otUniqueConstraints;
    tvotCheckConstraint:         Result := otCheckConstraints;
    tvotNotNullConstraint:       Result := otNotNullConstraints;

    // --- Procedures / Functions ---
    tvotProcedure:               Result := otProcedures;
    tvotFunction:                Result := otFunctions;
    tvotUDF:                     Result := otUDF;

    // --- UDR ---
    tvotUDRFunction:             Result := otUDRFunctions;
    tvotUDRProcedure:            Result := otUDRProcedures;
    tvotUDRTrigger:              Result := otUDRTriggers;

    // --- Packages ---
    tvotPackage:                 Result := otPackages;
    tvotPackageFunction:         Result := otPackageFunctions;
    tvotPackageProcedure:        Result := otPackageProcedures;
    tvotPackageUDFFunction:      Result := otPackageUDFFunctions;
    tvotPackageUDRFunction:      Result := otPackageUDRFunctions;
    tvotPackageUDRProcedure:     Result := otPackageUDRProcedures;
    tvotPackageUDRTrigger:       Result := otPackageUDRTriggers;

    // --- Database / Metadata ---
    tvotDatabase:                Result := otDatabase;
    tvotBLOBFilter:              Result := otBLOBFilters;
    tvotComment:                 Result := otComments;
    tvotData:                    Result := otData;

    // --- System Objects ---
    tvotSystemTable:             Result := otSystemTables;
    tvotSystemDomain:            Result := otSystemDomains;
    tvotSystemGenerator:         Result := otSystemGenerators;
    tvotSystemTrigger:           Result := otSystemTriggers;
    tvotSystemConstraint:        Result := otSystemConstraints;
    tvotSystemIndex:             Result := otSystemIndexes;
    tvotSystemRole:              Result := otSystemRoles;
    tvotSystemUser:              Result := otSystemUsers;
    tvotSystemException:         Result := otSystemExceptions;
  else
    Result := otNone;
  end;
end;

function TreeViewObjectToStr(AType: TTreeViewObjectType): string;
begin
  case AType of
    // --- Allgemein ---
    tvotNone:                  Result := 'None';
    tvotServer:                Result := 'Server';
    tvotDatabase:              Result := 'Database';
    tvotQueryWindow:           Result := 'Query Window';

    // --- Tables ---
    tvotTableRoot:             Result := 'Tables';
    tvotTable:                 Result := 'Table';
    tvotTableFieldRoot:        Result := 'Fields';
    tvotTableField:            Result := 'Table Field';

    // --- Generators / Sequences ---
    tvotGeneratorRoot:         Result := 'Generators';
    tvotGenerator:             Result := 'Generator';

    // --- Triggers ---
    tvotTriggerRoot:           Result := 'Triggers';
    tvotTrigger:               Result := 'Trigger';
    tvotTableTriggerRoot:      Result := 'Table Triggers';
    tvotTableTrigger:          Result := 'Table Trigger';
    tvotDBTriggerRoot:         Result := 'Database Triggers';
    tvotDBTrigger:             Result := 'Database Trigger';
    tvotDDLTriggerRoot:        Result := 'DDL Triggers';
    tvotDDLTrigger:            Result := 'DDL Trigger';

    // --- Views ---
    tvotViewRoot:              Result := 'Views';
    tvotView:                  Result := 'View';

    // --- Domains ---
    tvotDomainRoot:            Result := 'Domains';
    tvotDomain:                Result := 'Domain';

    // --- Roles / Users ---
    tvotRoleRoot:              Result := 'Roles';
    tvotRole:                  Result := 'Role';
    tvotUserRoot:              Result := 'Users';
    tvotUser:                  Result := 'User';

    // --- Exceptions ---
    tvotExceptionRoot:         Result := 'Exceptions';
    tvotException:             Result := 'Exception';

    // --- Indexes / Constraints ---
    tvotIndexRoot:             Result := 'Indexes';
    tvotIndex:                 Result := 'Index';
    tvotTableReferencesRoot:   Result := 'Table References';
    tvotTableReference:        Result := 'Table Reference';

    tvotConstraintRoot:        Result := 'Constraints';
    tvotConstraint:            Result := 'Constraint';
    tvotPrimaryKeyRoot:        Result := 'Primary Keys';
    tvotPrimaryKey:            Result := 'Primary Key';
    tvotForeignKeyRoot:        Result := 'Foreign Keys';
    tvotForeignKey:            Result := 'Foreign Key';
    tvotUniqueConstraintRoot:  Result := 'Unique Constraints';
    tvotUniqueConstraint:      Result := 'Unique Constraint';
    tvotCheckConstraintRoot:   Result := 'Check Constraints';
    tvotCheckConstraint:       Result := 'Check Constraint';
    tvotNotNullConstraintRoot: Result := 'Not Null Constraints';
    tvotNotNullConstraint:     Result := 'Not Null Constraint';

    // --- Stored Procedures / Functions ---
    tvotProcedureRoot:         Result := 'Stored Procedures';
    tvotProcedure:             Result := 'Stored Procedure';
    tvotFunctionRoot:          Result := 'Functions';
    tvotFunction:              Result := 'Function';

    // --- UDR / UDF ---
    tvotUDRoot:                Result := 'UD Objects';
    tvotUDFRoot:               Result := 'UDFs';
    tvotUDF:                   Result := 'UDF Function';

    tvotUDRRoot:               Result := 'UDRs';
    tvotUDRFunctionRoot:       Result := 'UDR Functions';
    tvotUDRFunction:           Result := 'UDR Function';
    tvotUDRProcedureRoot:      Result := 'UDR Procedures';
    tvotUDRProcedure:          Result := 'UDR Procedure';
    tvotUDRTriggerRoot:        Result := 'UDR Triggers';
    tvotUDRTrigger:            Result := 'UDR Trigger';


    // UDR Trigger Untertypen (NEU - falls noch nicht vorhanden)
    tvotUDRTableTriggerRoot:   Result := 'UDR Table Triggers';
    tvotUDRTableTrigger:       Result := 'UDR Table Trigger';
    tvotUDRDBTriggerRoot:      Result := 'UDR DB Triggers';
    tvotUDRDBTrigger:          Result := 'UDR DB Trigger';
    tvotUDRDDLTriggerRoot:     Result := 'UDR DDL Triggers';
    tvotUDRDDLTrigger:         Result := 'UDR DDL Trigger';

    // --- Packages ---
    tvotPackageRoot:           Result := 'Packages';
    tvotPackage:               Result := 'Package';
    tvotPackageFunctionRoot:   Result := 'Package Functions';
    tvotPackageFunction:       Result := 'Package Function';
    tvotPackageProcedureRoot:  Result := 'Package Procedures';
    tvotPackageProcedure:      Result := 'Package Procedure';
    tvotPackageUDFFunctionRoot:Result := 'Package UDF Functions';
    tvotPackageUDFFunction:    Result := 'Package UDF Function';
    tvotPackageUDRFunctionRoot:Result := 'Package UDR Functions';
    tvotPackageUDRFunction:    Result := 'Package UDR Function';
    tvotPackageUDRProcedureRoot:Result := 'Package UDR Procedures';
    tvotPackageUDRProcedure:   Result := 'Package UDR Procedure';
    tvotPackageUDRTriggerRoot: Result := 'Package UDR Triggers';
    tvotPackageUDRTrigger:     Result := 'Package UDR Trigger';

    // --- Database / Metadata ---
    tvotDatabaseRoot:          Result := 'Database';
    tvotBLOBFilterRoot:        Result := 'BLOB Filters';
    tvotBLOBFilter:            Result := 'BLOB Filter';
    tvotCommentRoot:           Result := 'Comments';
    tvotComment:               Result := 'Comment';
    tvotDataRoot:              Result := 'Data';
    tvotData:                  Result := 'Data';

    // --- System Objects ---
    tvotSystemTableRoot:       Result := 'System Tables';
    tvotSystemTable:           Result := 'System Table';
    tvotSystemTableFieldRoot:  Result := 'Fields';
    tvotSystemTableField:      Result := 'System Table Field';

    tvotSystemDomainRoot:      Result := 'System Domains';
    tvotSystemDomain:          Result := 'System Domain';
    tvotSystemGeneratorRoot:   Result := 'System Generators';
    tvotSystemGenerator:       Result := 'System Generator';
    tvotSystemTriggerRoot:     Result := 'System Triggers';
    tvotSystemTrigger:         Result := 'System Trigger';
    tvotSystemConstraintRoot:  Result := 'System Constraints';
    tvotSystemConstraint:      Result := 'System Constraint';
    tvotSystemIndexRoot:       Result := 'System Indexes';
    tvotSystemIndex:           Result := 'System Index';
    tvotSystemRoleRoot:        Result := 'System Roles';
    tvotSystemRole:            Result := 'System Role';
    tvotSystemUserRoot:        Result := 'System Users';
    tvotSystemUser:            Result := 'System User';
    tvotSystemExceptionRoot:   Result := 'System Exceptions';
    tvotSystemException:       Result := 'System Exception';
  else
    Result := 'Unknown';
  end;
end;

function TBTypeToIBXType(AObjectType: TObjectType): TExtractObjectTypes;
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


function RoutineTypeToTreeViewObjectType(RT: TRoutineType): TTreeViewObjectType;
begin
  case RT of
    rtUDF:              Result := tvotUDF;
    rtFBFunc:           Result := tvotFunction;
    rtFBProc:           Result := tvotProcedure;
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
    Result := Trim(Copy(Input, 1, PosOpenParen - 1))
  else
    Result := Trim(Input);
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
    7  : Result := 'SMALLINT';
    8  : Result := 'INTEGER';
    9  : Result := 'QUAD';
    10 : Result := 'FLOAT';
    11 : Result := 'D_FLOAT';
    12 : Result := 'DATE';
    13 : Result := 'TIME';
    14 : Result := 'CHAR';
    16 : Result := 'BIGINT';
    23 : Result := 'BOOLEAN';
    24 : Result := 'DECFLOAT(16)';
    25 : Result := 'DECFLOAT(34)';
    26 : Result := 'INT128';                    // Fallback, wird unten korrigiert
    27 : Result := 'DOUBLE PRECISION';
    28 : Result := 'TIME WITH TIME ZONE';
    29 : Result := 'TIMESTAMP WITH TIME ZONE';
    35 : Result := 'TIMESTAMP';
    37 : Result := 'VARCHAR';
    40 : Result := 'CSTRING';
    45 : Result := 'BLOB_ID';
    261:
      begin
        if SubType = 1 then
          Result := 'BLOB SUB_TYPE TEXT'
        else
          Result := 'BLOB SUB_TYPE BINARY';
      end;

  else
    Result := 'UNKNOWN_TYPE. CODE = ' + IntToStr(Index);
  end;

  // Längenangabe bei CHAR und VARCHAR
  if Index in [14, 37] then  // 14=CHAR, 37=VARCHAR
  begin
    if CharacterLengt > 0 then
      Result := Result + '(' + IntToStr(CharacterLengt) + ')'
    else if FieldLength > 0 then
      Result := Result + '(' + IntToStr(FieldLength) + ')';
  end;

  // Numerische Typen mit SubType (NUMERIC/DECIMAL) — inkl. INT128 (26)
  if Index in [7, 8, 16, 26] then
  begin
    // Sonderfall INT128 mit Scale ≠ 0 → NUMERIC(38,x)
    if Index = 26 then
    begin
      if (Scale <> 0) and (Scale <> -1) then
      begin
        if Precision <= 0 then
          Precision := 38;
        Result := 'NUMERIC(' + IntToStr(Precision) + ',' + IntToStr(Abs(Scale)) + ')';
      end;
      // sonst: bleibt 'INT128' (Fallback vom case)
    end
    else
    begin
      if SubType <= 0 then
      begin
        case Index of
          7: Result := 'SMALLINT';
          8: Result := 'INTEGER';
          16: Result := 'BIGINT';
        end;
      end
      else
      begin
        if SubType = 1 then
          Result := 'NUMERIC('
        else if SubType = 2 then
          Result := 'DECIMAL('
        else
          Result := 'UNKNOWN_NUMERIC(';

        if Precision < 0 then
          Precision := 2;

        Result := Result + IntToStr(Precision) + ',' + IntToStr(Abs(Scale)) + ')';
      end;
    end;
  end;

  if (Index = 14) and (CharacterLengt = 63) and (UpperCase(Trim(CharacterSet)) = 'OCTETS') then
      Result := 'UUID';
end;

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

function GetDBIndexByDatabase(Database: TIBDatabase): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(RegisteredDatabases) do
    if RegisteredDatabases[i].IBDatabase = Database then
      Exit(i);
end;

{ ============================================================
  GetDBTitlesForServer: Liefert alle DB-Titel, die zu einem
  Server (Anzeige-Name = Alias oder echter Name) gehören.

  Signatur verwendet TStrings statt TStringList, damit auch
  TComboBox.Items direkt übergeben werden kann (TComboBox.Items
  ist vom Typ TStrings; TStringList ist davon abgeleitet).

  Logik:
    - Für jede DB-Registrierung wird der zugehörige Server-
      Record über GetServerRecordFromFileByAlias aufgelöst.
      Das nutzt Pass 1 (Alias) + Pass 2 (echter Name) und
      wendet damit dieselbe "Legacy gehört dem ersten Server
      mit diesem echten Namen"-Regel an wie GetServerNodeByAlias.
    - Verglichen werden beide Felder (ServerAlias + ServerName).
      Dadurch gehören DBs unter 'Prod' nicht zu 'Test', auch
      wenn beide auf denselben echten Server zeigen.

  AList muss vom Aufrufer erstellt und freigegeben werden.
  Die Liste wird VORHER geleert.

  Result = False nur bei nil-Liste oder leerem ADisplayAlias.
  ============================================================ }
function GetDBTitlesForServer(const ADisplayAlias: string; AList: TStrings): Boolean;
var
  i: Integer;
  TargetRec, RecServer: TServerRecord;
begin
  Result := False;
  if AList = nil then Exit;

  AList.Clear;

  if Trim(ADisplayAlias) = '' then Exit;

  TargetRec := GetServerRecordFromFileByAlias(ADisplayAlias);
  if Trim(TargetRec.ServerName) = '' then Exit;

  for i := 0 to High(RegisteredDatabases) do
  begin
    if Trim(RegisteredDatabases[i].RegRec.ServerName) = '' then Continue;

    RecServer := GetServerRecordFromFileByAlias(
                   RegisteredDatabases[i].RegRec.ServerName);
    if Trim(RecServer.ServerName) = '' then Continue;

    // Beide Felder vergleichen (ServerAlias + ServerName) —
    // bei zwei Aliasen auf demselben echten Server
    // unterscheiden sie sich im Alias.
    if SameText(Trim(TargetRec.ServerAlias), Trim(RecServer.ServerAlias))
       and SameText(Trim(TargetRec.ServerName), Trim(RecServer.ServerName)) then
      AList.Add(RegisteredDatabases[i].RegRec.Title);
  end;

  Result := True;
end;

// --------------------------------------------------------------------------
// Hilfsfunktion: DB‑Index anhand Title + Servername finden
// --------------------------------------------------------------------------
{ ============================================================
  Findet den DB-Index anhand Title + Servername.

  Seit der Alias-Umstellung kann RegRec.ServerName entweder
  der ALIAS (neue Registrierung) oder der ECHTE Servername
  (Legacy) sein. AServerName kann ebenfalls beides sein.

  Daher ZWEI Vergleiche pro Record:
    1) Direkter Vergleich (Alias↔Alias oder Legacy↔Legacy)
    2) Alias↔echter Name (über Server-Record-Auflösung)

  Liefert -1, wenn nichts gefunden wurde.
  ============================================================ }
function FindDBIndexByTitleAndServer(const ATitle, AServerName: string): Integer;
var
  i: Integer;
  SearchName, RecName: string;
  ServerRec: TServerRecord;
begin
  Result := -1;

  SearchName := Trim(AServerName);
  if SearchName = '' then
    Exit;

  for i := 0 to High(RegisteredDatabases) do
  begin
    if not SameText(RegisteredDatabases[i].RegRec.Title, ATitle) then
      Continue;

    RecName := Trim(RegisteredDatabases[i].RegRec.ServerName);

    // 1) Direkter Vergleich
    if SameText(RecName, SearchName) then
    begin
      Result := i;
      Exit;
    end;

    // 2) Alias↔echter Name: Server-Record auflösen
    if RecName <> '' then
    begin
      ServerRec := GetServerRecordFromFileByAlias(RecName);
      if Trim(ServerRec.ServerAlias) <> '' then
      begin
        if SameText(Trim(ServerRec.ServerAlias), SearchName) or
           SameText(Trim(ServerRec.ServerName), SearchName) then
        begin
          Result := i;
          Exit;
        end;
      end;
    end;
  end;
end;

function ConnectToDBAs(dbIndex: Integer; ForceConnectDialog: boolean = false): Boolean;
var
  Rec: TRegisteredDatabase;
  CachedPwd: string;
begin
  Result := False;
  Rec := RegisteredDatabases[dbIndex].RegRec;

  // ============================================================
  // 1. Session-Cache prüfen (nur wenn Passwort NICHT gespeichert)
  // ============================================================
  if (not Rec.SavePassword) and (Rec.Password = '') then
  begin
    CachedPwd := GetDBSessionPassword(Rec.ServerName, Rec.DatabaseName);
    if CachedPwd <> '' then
    begin
      Rec.Password := CachedPwd;
      RegisteredDatabases[dbIndex].RegRec.Password := CachedPwd;
    end;
  end;

  // ============================================================
  // 2. Wenn Passwort vorhanden (gespeichert ODER aus Cache)
  //    UND kein erzwungener Dialog → direkt verbinden
  // ============================================================
  if (not ForceConnectDialog) and (Rec.Password <> '') then
  begin
    try
      if fmReg.TestDBConnection(Rec.DatabaseName, Rec.UserName, Rec.Password,
        Rec.Charset, Rec.FireBirdClientLibPath, Rec.SQLDialect, Rec.Port,
        Rec.ServerName, Rec.OverwriteLoadedClientLib) then
      begin
        // Erfolg: DB-Params setzen
        RegisteredDatabases[dbIndex].IBDatabase.Params.Values['user_name'] := Rec.UserName;
        RegisteredDatabases[dbIndex].IBDatabase.Params.Values['password'] := Rec.Password;
        RegisteredDatabases[dbIndex].IBDatabase.LoginPrompt := False;
        Result := True;
        Exit;
      end
      else
      begin
        // Fehlgeschlagen: Cache löschen wenn daraus gelesen
        if (not Rec.SavePassword) then
        begin
          ClearDBSessionPassword(Rec.ServerName, Rec.DatabaseName);
          Rec.Password := '';
          RegisteredDatabases[dbIndex].RegRec.Password := '';
        end;
      end;
    except
      // Bei Exception ebenfalls Cache bereinigen
      if (not Rec.SavePassword) then
      begin
        ClearDBSessionPassword(Rec.ServerName, Rec.DatabaseName);
        Rec.Password := '';
        RegisteredDatabases[dbIndex].RegRec.Password := '';
      end;
    end;
  end;

  // ============================================================
  // 3. DB-Login-Dialog anzeigen (OHNE Rollen zu laden!)
  // ============================================================
  fmEnterPass.laDatabase.Caption := Rec.Title;
  fmEnterPass.edUser.Text := Rec.UserName;
  fmEnterPass.edPassword.Clear;
  fmEnterPass.edtRole.Text := '';
  fmEnterPass.cxSavePassword.Checked := False;

  if fmEnterPass.ShowModal = mrOK then
  begin
    // Testen ob die eingegebenen Credentials funktionieren
    if fmReg.TestDBConnection(Rec.DatabaseName, fmEnterPass.edUser.Text,
      fmEnterPass.edPassword.Text, Rec.Charset, Rec.FireBirdClientLibPath,
      Rec.SQLDialect, Rec.Port, Rec.ServerName, Rec.OverwriteLoadedClientLib) then
    begin
      // User + Passwort + Rolle in Registrierung speichern
      RegisteredDatabases[dbIndex].RegRec.UserName := fmEnterPass.edUser.Text;
      RegisteredDatabases[dbIndex].RegRec.Password := fmEnterPass.edPassword.Text;
      RegisteredDatabases[dbIndex].RegRec.Role := fmEnterPass.edtRole.Text;

      // Prüfen ob Passwort dauerhaft gespeichert werden soll
      if fmEnterPass.cxSavePassword.Checked then
      begin
        // Dauerhaft in Datei speichern
        RegisteredDatabases[dbIndex].RegRec.SavePassword := True;
        EditRegisteredDB(RegisteredDatabases[dbIndex].RegRec);
      end
      else
      begin
        // Nur temporär in Session-Cache
        RegisteredDatabases[dbIndex].RegRec.SavePassword := False;
        SetDBSessionPassword(Rec.ServerName, Rec.DatabaseName, fmEnterPass.edPassword.Text);
      end;

      // DB-Params setzen
      RegisteredDatabases[dbIndex].IBDatabase.Params.Values['user_name'] := fmEnterPass.edUser.Text;
      RegisteredDatabases[dbIndex].IBDatabase.Params.Values['password'] := fmEnterPass.edPassword.Text;
      RegisteredDatabases[dbIndex].IBDatabase.LoginPrompt := False;
      Result := True;
    end;
  end;
end;

function StrToFontStyles(const S: string): TFontStyles;var
  Parts: TStringList;
  I: Integer;
begin
  Result := [];
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := S;

    for I := 0 to Parts.Count - 1 do
      case LowerCase(Trim(Parts[I])) of
        'bold':       Include(Result, fsBold);
        'italic':     Include(Result, fsItalic);
        'underline':  Include(Result, fsUnderline);
        'strikeout':  Include(Result, fsStrikeOut);
      end;
  finally
    Parts.Free;
  end;
end;

function FontStylesToStr(AStyles: TFontStyles): string;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    if fsBold in AStyles then
      L.Add('Bold');
    if fsItalic in AStyles then
      L.Add('Italic');
    if fsUnderline in AStyles then
      L.Add('Underline');
    if fsStrikeOut in AStyles then
      L.Add('StrikeOut');

    Result := L.CommaText;
  finally
    L.Free;
  end;
end;

function MatchesFilter(const AText, AFilter: string): Boolean;
var
  Text, Filter: string;
begin
  if AFilter = '' then
    Exit(True);

  Text   := AnsiLowerCase(AText);
  Filter := AnsiLowerCase(AFilter);

  // *abc*
  if (Filter[1] = '*') and (Filter[Length(Filter)] = '*') then
    Exit(Pos(Copy(Filter, 2, Length(Filter)-2), Text) > 0);

  // *abc
  if Filter[1] = '*' then
    Exit(EndsText(Copy(Filter, 2, MaxInt), Text));

  // abc*
  if Filter[Length(Filter)] = '*' then
    Exit(StartsText(Copy(Filter, 1, Length(Filter)-1), Text));

  // exakt
  Result := Text = Filter;
end;

//Arrays
function GetArrayFieldInfo(DB: TIBDatabase; Field: TIBArrayField): string;
var
  MetaQuery, DimQuery: TIBQuery;
  TableName, FieldName, TypeName: string;
  FieldType, FieldSubType, Dimensions, FieldLength: Integer;
  DimStr: string;
begin
  Result := '';
  FieldName := Field.FieldName;

  // Wir brauchen den Tabellen-Namen
  if Field.DataSet is TIBTable then
    TableName := TIBTable(Field.DataSet).TableName
  else
    Exit;

  MetaQuery := TIBQuery.Create(nil);
  DimQuery := TIBQuery.Create(nil);
  try
    MetaQuery.Database := DB;
    DimQuery.Database := DB;

    // 1. Feldmetadaten holen
    MetaQuery.SQL.Text :=
      'SELECT f.RDB$FIELD_NAME AS TYPENAME, f.RDB$FIELD_TYPE, f.RDB$FIELD_SUB_TYPE, ' +
      'f.RDB$DIMENSIONS, f.RDB$FIELD_LENGTH ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'WHERE rf.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(TableName)) +
      ' AND rf.RDB$FIELD_NAME = ' + QuotedStr(UpperCase(FieldName));
    MetaQuery.Open;
    if MetaQuery.EOF then Exit;

    TypeName := Trim(MetaQuery.FieldByName('TYPENAME').AsString);
    FieldType := MetaQuery.FieldByName('RDB$FIELD_TYPE').AsInteger;
    FieldSubType := MetaQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
    Dimensions := MetaQuery.FieldByName('RDB$DIMENSIONS').AsInteger;
    FieldLength := MetaQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger;

    // 2. Dimensionen abfragen
    DimQuery.SQL.Text :=
      'SELECT RDB$LOWER_BOUND, RDB$UPPER_BOUND ' +
      'FROM RDB$FIELD_DIMENSIONS ' +
      'WHERE RDB$FIELD_NAME = ' + QuotedStr(TypeName) +
      ' ORDER BY RDB$DIMENSION';
    DimQuery.Open;

    DimStr := '';
    while not DimQuery.EOF do
    begin
      if DimStr <> '' then DimStr := DimStr + ' x ';
      DimStr := DimStr + Format('%d..%d', [
        DimQuery.FieldByName('RDB$LOWER_BOUND').AsInteger,
        DimQuery.FieldByName('RDB$UPPER_BOUND').AsInteger
      ]);
      DimQuery.Next;
    end;

    // 3. Datentyp bestimmen
    case FieldType of
      7: Result := Format('Array[%s] of SMALLINT', [DimStr]);       // SMALLINT
      8: Result := Format('Array[%s] of INTEGER', [DimStr]);        // INTEGER
      10: Result := Format('Array[%s] of FLOAT', [DimStr]);         // FLOAT
      12: Result := Format('Array[%s] of DATE', [DimStr]);          // DATE/TIME
      14: Result := Format('Array[%s] of CHAR', [DimStr]);          // CHAR
      37: // VARCHAR
        begin
          // Länge ermitteln
          Result := Format('Array[%s] of VARCHAR(%d)', [DimStr, MetaQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger]);
        end;
      261: Result := Format('Array[%s] of BLOB', [DimStr]);         // BLOB
      else Result := Format('Array[%s] of UNKNOWN', [DimStr]);
    end;

  finally
    MetaQuery.Free;
    DimQuery.Free;
  end;
end;

function ArrayDimsToSuffix(const ADims: TArrayDims): string;
var
  j: Integer;
  S: string;
begin
  S := '';
  for j := 0 to High(ADims) do
  begin
    if S <> '' then
      S := S + ', ';
    S := S + IntToStr(ADims[j].LowerBound) + ':' + IntToStr(ADims[j].UpperBound);
  end;
  if S <> '' then
    Result := ' [' + S + ']'
  else
    Result := '';
end;


//ExternalTable
function GetTableFieldsForExternalTable(ADBIndex: Integer;
  const ATableName: string): TDataSet;
var
  Query: TIBQuery;
begin
  Query := TIBQuery.Create(nil);
  try
    Query.Database := RegisteredDatabases[ADBIndex].IBDatabase;
    Query.Transaction := RegisteredDatabases[ADBIndex].IBTransaction;

    Query.SQL.Text :=
      'SELECT ' +
      '  RF.RDB$FIELD_NAME AS FIELD_NAME, ' +
      '  F.RDB$FIELD_TYPE AS FIELD_TYPE, ' +
      '  F.RDB$FIELD_LENGTH AS FIELD_LENGTH, ' +
      '  F.RDB$FIELD_PRECISION AS FIELD_PRECISION, ' +
      '  F.RDB$FIELD_SCALE AS FIELD_SCALE, ' +
      '  F.RDB$FIELD_SUB_TYPE AS FIELD_SUB_TYPE, ' +
      '  COALESCE(F.RDB$CHARACTER_LENGTH, F.RDB$FIELD_LENGTH) AS CHAR_LEN ' +
      'FROM RDB$RELATION_FIELDS RF ' +
      'JOIN RDB$FIELDS F ON RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ' +
      'WHERE RF.RDB$RELATION_NAME = :TableName ' +
      'ORDER BY RF.RDB$FIELD_POSITION';

    Query.ParamByName('TableName').AsString := ATableName;

    if not Query.Database.Connected then
      Query.Database.Connected := true;
    if not Query.Transaction.InTransaction then
      Query.Transaction.StartTransaction;

    Query.Open;
    Result := Query;
  except
    Query.Free;
    raise;
  end;
end;

function CountChar(const S: string; const C: Char): Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    if S[i] = C then
      Inc(Result);
end;

{ ==========================================================================
  IsDomain

  Prüft, ob ein Feldtyp-String ein Domain-Name ist (True) oder ein
  bekannter Firebird-Basistyp (False).

  Standard-Typen (auch alte InterBase-Typen) werden erkannt.
  Alles andere wird als Domain-Name interpretiert.

  Behandelt werden auch:
    - BLOB-Varianten (SUB_TYPE TEXT, SUB_TYPE BINARY, SUB_TYPE 0, SUB_TYPE 1)
    - Suffixe wie CHARACTER SET, SEGMENT SIZE
    - Klammer-Parameter (z.B. VARCHAR(50))

  Beispiele:
    'INTEGER'                                          → False (Basistyp)
    'VARCHAR'                                          → False (Basistyp)
    'VARCHAR(50)'                                      → False (Basistyp)
    'CHAR(10) CHARACTER SET NONE'                      → False (Basistyp)
    'BLOB'                                             → False (Basistyp)
    'BLOB SUB_TYPE TEXT'                               → False (Basistyp)
    'BLOB SUB_TYPE BINARY'                             → False (Basistyp)
    'BLOB SUB_TYPE 0'                                  → False (Basistyp)
    'BLOB SUB_TYPE TEXT SEGMENT SIZE 80'               → False (Basistyp)
    'BLOB SUB_TYPE TEXT SEGMENT SIZE 80 CHARACTER SET NONE' → False (Basistyp)
    'BLOB_ID'                                          → False (Spezial-Typ)
    'JOBCODE'                                          → True  (Domain)
    'SALARY'                                           → True  (Domain)
  ========================================================================== }
  function IsDomain(const ATypeName: string): Boolean;
  const
    StandardTypes: array[0..23] of string = (
      'SMALLINT', 'INTEGER', 'BIGINT', 'INT128',
      'FLOAT', 'DOUBLE', 'DOUBLE PRECISION', 'DECFLOAT',
      'DECIMAL', 'NUMERIC',
      'CHAR', 'VARCHAR', 'CSTRING',
      'DATE', 'TIME', 'TIMESTAMP',
      'BLOB', 'BLOB_ID',
      'BOOLEAN', 'UUID', 'QUAD', 'D_FLOAT',
      'TIME WITH TIME ZONE', 'TIMESTAMP WITH TIME ZONE'
    );
  var
    CleanType: string;
    i, P: Integer;
  begin
    CleanType := UpperCase(Trim(ATypeName));

    // 1. CHARACTER SET Suffix abstreifen
    P := Pos(' CHARACTER SET ', CleanType);
    if P > 0 then
      CleanType := Trim(Copy(CleanType, 1, P - 1));

    // 2. SEGMENT SIZE Suffix abstreifen (BLOB)
    P := Pos(' SEGMENT SIZE ', CleanType);
    if P > 0 then
      CleanType := Trim(Copy(CleanType, 1, P - 1));

    // 3. Array-Klammern abstreifen: INTEGER [10] → INTEGER
    P := Pos('[', CleanType);
    if P > 0 then
      CleanType := Trim(Copy(CleanType, 1, P - 1));

    // 4. Klammer-Parameter abstreifen: VARCHAR(50) → VARCHAR
    P := Pos('(', CleanType);
    if P > 0 then
      CleanType := Trim(Copy(CleanType, 1, P - 1));

    // 5. BLOB-Varianten alle als Standardtyp
    if (CleanType = 'BLOB SUB_TYPE TEXT') or
       (CleanType = 'BLOB SUB_TYPE BINARY') or
       (CleanType = 'BLOB SUB_TYPE 0') or
       (CleanType = 'BLOB SUB_TYPE 1') then
      Exit(False);

    // 6. Standard-Typen prüfen
    for i := Low(StandardTypes) to High(StandardTypes) do
      if CleanType = StandardTypes[i] then
        Exit(False);

    Result := True;
  end;

function DomainToDataType(const ADomainName: string; ADatabase: TIBDatabase; ATransaction: TIBTransaction): string;
var
  Qry: TIBQuery;
  FieldType, FieldSubType, FieldLen, FieldPrecision, FieldScale, CharLen: Integer;
  Charset: string;
begin
  Result := '';

  if (Trim(ADomainName) = '') or (not Assigned(ADatabase)) then
    Exit;

  // Keine System-Domänen (RDB$...)
  if IsFieldDomainSystemGenerated(ADomainName) then
    Exit;

  // Keine Standard-Typen
  if not IsDomain(ADomainName) then
    Exit;

  if not Assigned(ATransaction) then
    Exit;

  if not ATransaction.InTransaction then
    ATransaction.StartTransaction;

  Qry := TIBQuery.Create(nil);
  try
    Qry.Database := ADatabase;
    Qry.Transaction := ATransaction;

    // Gleiche Query wie GetDomainInfo – bewährt und funktionierend
    Qry.SQL.Text :=
      'SELECT f.*, ' +
      '  coll.RDB$COLLATION_NAME, ' +
      '  cs.RDB$CHARACTER_SET_NAME ' +
      'FROM RDB$FIELDS f ' +
      'LEFT JOIN ( ' +
      '  RDB$COLLATIONS coll ' +
      '  INNER JOIN RDB$CHARACTER_SETS cs ON ' +
      '    coll.RDB$CHARACTER_SET_ID = cs.RDB$CHARACTER_SET_ID ' +
      ') ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID AND ' +
      '     f.RDB$CHARACTER_SET_ID = coll.RDB$CHARACTER_SET_ID ' +
      'WHERE f.RDB$FIELD_NAME = :DomainName';

    Qry.ParamByName('DomainName').AsString := UpperCase(ADomainName);

    Qry.Open;

    if Qry.RecordCount > 0 then
    begin
      Qry.First;
      FieldType := Qry.FieldByName('RDB$FIELD_TYPE').AsInteger;
      FieldSubType := Qry.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      FieldLen := Qry.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      FieldPrecision := Qry.FieldByName('RDB$FIELD_PRECISION').AsInteger;
      FieldScale := Qry.FieldByName('RDB$FIELD_SCALE').AsInteger;
      // RDB$CHARACTER_LENGTH könnte NULL sein → FieldLen als Fallback
      if Qry.FieldByName('RDB$CHARACTER_LENGTH').IsNull then
        CharLen := FieldLen
      else
        CharLen := Qry.FieldByName('RDB$CHARACTER_LENGTH').AsInteger;
      Charset := Trim(Qry.FieldByName('RDB$CHARACTER_SET_NAME').AsString);

      Result := GetFBTypeName(FieldType, FieldSubType, FieldLen, FieldPrecision, FieldScale, Charset, CharLen);
    end;

    Qry.Close;

  finally
    Qry.Free;
  end;
end;

initialization
  SessionPasswordCache := nil;
  MetaDataChanged := false;

  //fIniFileName := ChangeFileExt(Application.ExeName, '.ini');

  {$IFDEF LINUX}
    fIniFileName := Application.ExeName + '.ini';
  {$ENDIF}

  {$IFDEF WINDOWS}
    // .exe entfernen, .ini anhängen
    fIniFileName := ChangeFileExt(Application.ExeName, '.ini');
  {$ENDIF}

  {$IFDEF DARWIN}
    // Auf macOS liegt Application.ExeName im .app-Bundle:
    //   .../TurboBird.app/Contents/MacOS/TurboBird
    // INI soll neben dem Bundle liegen, also drei Ebenen hoch:
    //   .../TurboBird.ini
    fIniFileName := ExtractFilePath(Application.ExeName);           // .../Contents/MacOS/
    fIniFileName := ExcludeTrailingPathDelimiter(fIniFileName);      // .../Contents/MacOS
    fIniFileName := ExtractFileDir(fIniFileName);                    // .../Contents
    fIniFileName := ExtractFileDir(fIniFileName);                    // .../TurboBird.app
    fIniFileName := ExtractFileDir(fIniFileName);                    // .../ (Ordner mit .app)
    fIniFileName := IncludeTrailingPathDelimiter(fIniFileName)
                  + ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
    // Ergebnis: .../TurboBird.ini
  {$ENDIF}

  fIniFile := TIniFile.Create(fIniFileName);


  ExtractResourcesIfNeeded;

  // temp-Verzeichnis für embedded connections sicherstellen
  ForceDirectories(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + TmpDir);

  ReadIniFile;
  //WriteIniFile;

  //if not LoadClientLibIBX(InitialFBClientLibPath) then
    //SetInitialClientLib;

finalization
  if Assigned(SessionPasswordCache) then
    SessionPasswordCache.Free;

  WriteIniFile;
  fIniFile.Free;
  fIniFile := nil;
end.
