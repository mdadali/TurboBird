unit main;

{ Main TurboBird form
If you want to add popup menus for tables, views etc, please set the Tag
property of the popup menu to the right value.
}

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, IBConnection, sqldb, sqldblib, memds, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, Reg, QueryWindow, Grids,
  ExtCtrls, Buttons, StdCtrls, TableManage, dbugintf, turbocommon, importtable,
  DB, IniFiles, Types,
  fSetFBClient,
  fTestFunction,
  fCheckDBIntegrity,
  fFirebirdConfig,
  fsqlmonitor,
  ibase60dyn,

  usqlqueryext,
  udb_udf_fetcher,

  udb_udr_func_fetcher,
  udb_udr_proc_fetcher,

  udb_firebird_func_fetcher,
  udb_firebird_proc_fetcher,

  udb_package_firebird_func_fetcher,
  udb_package_firebird_proc_fetcher,

  udb_package_udr_func_fetcher,
  udb_package_udr_proc_fetcher,

  fetch_package,

  package_deps,
  generator_deps,
  trigger_deps,
  view_deps,
  fb_proc_deps,
  fb_func_deps,
  udf_deps,
  udr_func_deps,
  udr_proc_deps,
  domain_deps,
  role_deps,
  exception_deps,
  user_deps,
  fbcommon,

  fServerSession;


{$i turbocommon.inc}

type

  { TfmMain }

  TfmMain = class(TForm)
      editorFontDialog: TFontDialog;
      Image1: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImNewFBFunction: TMenuItem;
    ImCreateNewPackage: TMenuItem;
    lmEditPackage: TMenuItem;
    lmDropPackage: TMenuItem;
    lmPackageEditor: TMenuItem;
    lmNewUDRFunction: TMenuItem;
    lmNewUDRProcedure: TMenuItem;
    lmDropStoredProcedure: TMenuItem;
    lmDropGenerator: TMenuItem;
    lmDropView: TMenuItem;
    lmDropTrigger: TMenuItem;
    lmPackageFunction: TMenuItem;
    lmGetPackageFunction: TMenuItem;
    lmDropPackagesFunction: TMenuItem;
    lmPackageProcedure: TMenuItem;
    lmGetPackageProcedure: TMenuItem;
    lmDropPackageProcedure: TMenuItem;
    lmPackageUDRFunction: TMenuItem;
    lmGetPackageUDRFunction: TMenuItem;
    lmDropPackageUDRFunction: TMenuItem;
    lmPackageUDRProcedure: TMenuItem;
    lmGetPackageUDRProcedure: TMenuItem;
    lmDropPackageUDRProcedure: TMenuItem;
    lmCopyStoredProcedureAS: TMenuItem;
    lmTestPackageUDRFunction: TMenuItem;
    lmTestPackageFunction: TMenuItem;
    lmTestPackageProcedure: TMenuItem;
    lmTestPackageUDRProcedure: TMenuItem;
    lmFireBirdFunction: TMenuItem;
    lmEditFireBirdFunction: TMenuItem;
    lmDropFireBirdFunction: TMenuItem;
    lmTestFireBirdFunction: TMenuItem;
    lmUDFFunction: TMenuItem;
    lmEditUDFFunction: TMenuItem;
    lmViewUDFFunction: TMenuItem;
    lmDropUDFFunction: TMenuItem;
    lmTestUDFFunction: TMenuItem;
    lmUDRFunction: TMenuItem;
    lmEditUDRFunction: TMenuItem;
    lmDropUDRFunction: TMenuItem;
    lmTestUDRFunction: TMenuItem;
    lmUDRProcedure: TMenuItem;
    lmEditUDRProcedure: TMenuItem;
    lmDropUDRProcedure: TMenuItem;
    lmTestUDRProcedure: TMenuItem;
    lmDropDomain: TMenuItem;
    lmDropUser: TMenuItem;
    lmSetFBClient: TMenuItem;
    lmDisconnectAll: TMenuItem;
    lmFirebirdConfig: TMenuItem;
    mnOptions: TMenuItem;
    mnEditorFont: TMenuItem;
    SQLQuery1: TSQLQueryExt;
    toolbarImages: TImageList;
    MainMenu1: TMainMenu;
    mdsHistory: TMemDataset;
    lmImportTable: TMenuItem;
    mnFile: TMenuItem;
    lmDisplayView: TMenuItem;
    lmViewTrigger: TMenuItem;
    lmCreateDB: TMenuItem;
    lmRegdb: TMenuItem;
    lmRestore: TMenuItem;
    lmSweep: TMenuItem;
    lmAddUser: TMenuItem;
    lmChangePassword: TMenuItem;
    lmUserPermManagement: TMenuItem;
    lmRolePerManagement: TMenuItem;
    lmSetGen: TMenuItem;
    lmDisconnect: TMenuItem;
    lmCopyTable: TMenuItem;
    lmCopyUserPermission: TMenuItem;
    lmViewFields: TMenuItem;
    lmEditField: TMenuItem;
    lmDBInfo: TMenuItem;
    lmCopyRolePermission: TMenuItem;
    lmCompare: TMenuItem;
    lmGetIncrementGen: TMenuItem;
    lmDropTable: TMenuItem;
    lmRecalculateStatistics: TMenuItem;
    mnuImport: TMenuItem;
    mnExit: TMenuItem;
    mnCreateDB: TMenuItem;
    mnRegDB: TMenuItem;
    mnHelp: TMenuItem;
    mnAbout: TMenuItem;
    lmEditReg: TMenuItem;
    lmUnregisterDatabase: TMenuItem;
    lmViewFirst1000: TMenuItem;
    lmViewStoredProcedure: TMenuItem;
    lmViewGen: TMenuItem;
    lmNewTable: TMenuItem;
    lmNewGen: TMenuItem;
    lmCreateAutoInc: TMenuItem;
    lmCreateStoredProc: TMenuItem;
    lmEditProc: TMenuItem;
    lmCreateView: TMenuItem;
    lmDisplay1000V: TMenuItem;
    lmEditView: TMenuItem;
    lmCreateTrigger: TMenuItem;
    lmEditTrigger: TMenuItem;
    lmActivateTrig: TMenuItem;
    lmDeactiveTrig: TMenuItem;
    lmScriptTable: TMenuItem;
    lmScriptTableCreate: TMenuItem;
    lmScriptInsert: TMenuItem;
    lmScriptUpdate: TMenuItem;
    lmEditTableData: TMenuItem;
    lmCallStoreProc: TMenuItem;
    lmNewUDF: TMenuItem;
    lmOpenSystemTable: TMenuItem;
    lmViewDomain: TMenuItem;
    lmNewDomain: TMenuItem;
    lmNewRole: TMenuItem;
    lmSeparator: TMenuItem;
    lmOpenQuery: TMenuItem;
    lmNewException: TMenuItem;
    lmRefresh: TMenuItem;
    lmDropException: TMenuItem;
    lmScriptException: TMenuItem;
    lmScriptDatabase: TMenuItem;
    lmConnectAs: TMenuItem;
    lmPermissions: TMenuItem;
    lmRolePermissions: TMenuItem;
    lmTableManage: TMenuItem;
    lmSeparator2: TMenuItem;
    lmBackup: TMenuItem;
    mnRestore: TMenuItem;
    PageControl1: TPageControl;
    pmDatabase: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    ToolBar1: TToolBar;
    tbtnCreateNewDB: TToolButton;
    tbtnRegDatabase: TToolButton;
    tbtnRestoreDatabase: TToolButton;
    tbtnAbout: TToolButton;
    tbCheckDBIntegrity: TToolButton;
    tbSQLMonitor: TToolButton;
    ToolButton3: TToolButton;
    tbtnEditorFont: TToolButton;
    tvMain: TTreeView;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ImNewFBFunctionClick(Sender: TObject);
    procedure ImCreateNewPackageClick(Sender: TObject);
    procedure ImEditFBFunctionClick(Sender: TObject);
    procedure lmAddUserClick(Sender: TObject);
    procedure lmBackupClick(Sender: TObject);
    procedure lmChangePasswordClick(Sender: TObject);
    procedure lmCompareClick(Sender: TObject);
    procedure lmCopyRolePermissionClick(Sender: TObject);
    procedure lmCopyUserPermissionClick(Sender: TObject);
    procedure lmCopyTableClick(Sender: TObject);
    procedure lmCreateDBClick(Sender: TObject);
    procedure lmDBInfoClick(Sender: TObject);
    procedure lmDisconnectAllClick(Sender: TObject);
    procedure lmDisconnectClick(Sender: TObject);
    procedure lmDropDomainClick(Sender: TObject);
    procedure lmDropFireBirdFunctionClick(Sender: TObject);
    procedure lmDropGeneratorClick(Sender: TObject);
    procedure lmDropPackageClick(Sender: TObject);
    procedure lmDropStoredProcedureClick(Sender: TObject);
    procedure lmDropTriggerClick(Sender: TObject);
    procedure lmDropUDFFunctionClick(Sender: TObject);
    procedure lmDropUDRFunctionClick(Sender: TObject);
    procedure lmDropUDRProcedureClick(Sender: TObject);
    procedure lmDropUserClick(Sender: TObject);
    procedure lmDropViewClick(Sender: TObject);
    procedure lmEditFieldClick(Sender: TObject);
    procedure lmEditPackageClick(Sender: TObject);
    procedure lmFirebirdConfigClick(Sender: TObject);
    procedure lmGetPackageFunctionClick(Sender: TObject);
    procedure lmGetPackageProcedureClick(Sender: TObject);
    procedure lmGetPackageUDRFunctionClick(Sender: TObject);
    procedure lmGetPackageUDRProcedureClick(Sender: TObject);
    procedure lmEditUDFFuctionClick(Sender: TObject);
    procedure lmEditUDRFunctionClick(Sender: TObject);
    procedure lmEditUDRProcedureClick(Sender: TObject);
    procedure lmGetIncrementGenClick(Sender: TObject);
    procedure lmImportTableClick(Sender: TObject);
    procedure lmNewUDRFunctionClick(Sender: TObject);
    procedure lmNewUDRProcedureClick(Sender: TObject);
    // Show all records in table
    procedure lmOpenSystemTableClick(Sender: TObject);
    procedure lmActivateTrigClick(Sender: TObject);
    procedure lmCallStoreProcClick(Sender: TObject);
    procedure lmConnectAsClick(Sender: TObject);
    procedure lmCreateAutoIncClick(Sender: TObject);
    procedure lmCreateStoredProcClick(Sender: TObject);
    procedure lmCreateTriggerClick(Sender: TObject);
    procedure lmCreateViewClick(Sender: TObject);
    procedure lmDeactiveTrigClick(Sender: TObject);
    procedure lmDisplay1000VClick(Sender: TObject);
    procedure lmDropExceptionClick(Sender: TObject);
    procedure lmEditProcClick(Sender: TObject);
    procedure lmEditTableDataClick(Sender: TObject);
    procedure lmEditTriggerClick(Sender: TObject);
    procedure lmEditViewClick(Sender: TObject);
    procedure lmNewDomainClick(Sender: TObject);
    procedure lmNewExceptionClick(Sender: TObject);
    procedure lmNewGenClick(Sender: TObject);
    procedure lmNewTableClick(Sender: TObject);
    procedure lmNewUDFClick(Sender: TObject);
    procedure lmOpenQueryClick(Sender: TObject);
    procedure lmPermissionsClick(Sender: TObject);
    procedure lmRefreshClick(Sender: TObject);
    procedure lmRegdbClick(Sender: TObject);
    procedure lmRestoreClick(Sender: TObject);
    procedure lmRolePerManagementClick(Sender: TObject);
    procedure lmRolePermissionsClick(Sender: TObject);
    procedure lmScriptDatabaseClick(Sender: TObject);
    procedure lmScriptExceptionClick(Sender: TObject);
    procedure lmScriptInsertClick(Sender: TObject);
    procedure lmScriptTableCreateClick(Sender: TObject);
    procedure lmScriptUpdateClick(Sender: TObject);
    procedure lmSetFBClientClick(Sender: TObject);
    procedure lmSetGenClick(Sender: TObject);
    procedure lmSweepClick(Sender: TObject);
    procedure lmTableManageClick(Sender: TObject);
    procedure lmTestFireBirdFunctionClick(Sender: TObject);
    procedure lmTestPackageFunctionClick(Sender: TObject);
    procedure lmTestPackageProcedureClick(Sender: TObject);
    procedure lmTestPackageUDRProcedureClick(Sender: TObject);
    procedure lmTestUDFFunctionClick(Sender: TObject);
    procedure lmTestUDRFunctionClick(Sender: TObject);
    procedure lmTestPackageUDRFunctionClick(Sender: TObject);
    procedure lmTestUDRProcedureClick(Sender: TObject);
    procedure lmUserPermManagementClick(Sender: TObject);
    procedure lmViewDomainClick(Sender: TObject);
    procedure lmDisplayViewClick(Sender: TObject);
    // Expand table field nodes
    procedure lmViewFieldsClick(Sender: TObject);
    procedure lmViewGenClick(Sender: TObject);
    procedure lmViewStoredProcedureClick(Sender: TObject);
    procedure lmViewTriggerClick(Sender: TObject);
    procedure lmViewUDFClick(Sender: TObject);
    procedure lmDropTableClick(Sender: TObject);
    procedure lmRecalculateStatisticsClick(Sender: TObject);
    procedure mnEditorFontClick(Sender: TObject);
    procedure mnExitClick(Sender: TObject);
    procedure mnCreateDBClick(Sender: TObject);
    procedure mnRegDBClick(Sender: TObject);
    procedure mnAboutClick(Sender: TObject);
    procedure lmEditRegClick(Sender: TObject);
    procedure lmUnregisterDatabaseClick(Sender: TObject);
    procedure lmViewFirst1000Click(Sender: TObject);
    procedure lmNewRoleClick(Sender: TObject);
    procedure mnRestoreClick(Sender: TObject);
    procedure PageControl1CloseTabClicked(Sender: TObject);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pmDatabasePopup(Sender: TObject);
    procedure tbCheckDBIntegrityClick(Sender: TObject);
    procedure tbSQLMonitorClick(Sender: TObject);
    procedure tvMainAddition(Sender: TObject; Node: TTreeNode);
    procedure tvMainClick(Sender: TObject);
    procedure tvMainDblClick(Sender: TObject);
    procedure tvMainDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvMainExpanded(Sender: TObject; Node: TTreeNode);
    procedure GlobalException(Sender: TObject; E : Exception);
    procedure tvMainExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FIBConnection: TIBConnection;
    FSQLTransaction: TSQLTransaction;
    FCurrentHistoryFile: string;
    FActivated: Boolean;
    Function FindCustomForm(ATitle: string; AClass: TClass): TComponent;
    // Show new generator form
    procedure InitNewGen(DatabaseIndex: Integer);
    Function GetServerNameNode(ServerName: string): TTreeNode;
    // Remove RegisteredDatabases and clean up memory held by its objects
    procedure ReleaseRegisteredDatabases;
    // Set connection for SQLQuery1 to selected registered database
    procedure SetConnection(Index: Integer);
    procedure SetFocus; override; // solve a bug in Lazarus
    procedure AppShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  protected
    // This procedure will receive the events that are logged by the connection:
    procedure GetLogEvent(Sender: TSQLConnection; EventType: TDBEventType; Const Msg : String);
  public
    // Array of database connection details as stored in turbobird.reg file
    Version: string;
    VersionDate: string;
    Major, Minor, ReleaseVersion: word;
    Function GetServerName(DBName: string): string;
    Function RetrieveInputParamFromSP(Body: string): string;
    // Load registered databases from file and show them in treeview
    Function LoadRegisteredDatabases: Boolean;
    Function FindQueryWindow(ATitle: string): TComponent;
    Function DeleteRegistration(Index: Integer): Boolean;
    // Returns BLOB subtype clause depending on subtype
    Function GetBlobSubTypeName(SubType: integer): string;
    // Get name of index used for primary key
    // Also returns name of constraint used
    Function GetPrimaryKeyIndexName(DatabaseIndex: Integer; ATableName: string; var ConstraintName: string): string;
    // Get primary key field(s) names into KeyFields
    Function GetPrimaryKeyFields(DatabaseIndex: Integer; ATableName: string; var KeyFields: TStringList): boolean;
    Function GetConstraintFields(ATableName, AIndexName: string; var List: TStringList): Boolean;
    // Get fields information for specified table
    // Fills SQLQuery1 with details
    procedure GetFields(DatabaseIndex: Integer; ATableName: string; FieldsList: TStringList);

    procedure GetPackageFunctions(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);
    procedure GetPackageProcedures(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);
    procedure GetPackageUDRFunctions(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);
    procedure GetPackageUDRProcedures(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);

    // Get body of a stored procedure (without SET TERM... clauses)
    // Fills SQLQuery1 with details
    Function GetStoredProcBody(DatabaseIndex: Integer; AProcName: string; var SPOwner: string): string;
    // Get body and output parameters of a view
    // Does *not* fill SQLQuery1 with details
    Function GetViewInfo(DatabaseIndex: Integer; AViewName: string; var Columns, Body: string): Boolean;
    Function ChangeTriggerActivity(DatabaseIndex: Integer; ATriggerName: string; ActiveState: Boolean): Boolean;
    Function GetIndices(ATableName: string; AQuery: TSQLQuery): Boolean;
    Function GetIndexFields(ATableName, AIndexName: string; AQuery: TSQLQuery; var FieldsList: TStringList): Boolean;
    Function GetUDFInfo(DatabaseIndex: Integer; UDFName: string; var ModuleName, EntryPoint, Params: string): Boolean;
    Function ShowQueryWindow(DatabaseIndex: Integer; ATitle: string; ANodeInfos: TPNodeInfos=nil): TfmQueryWindow;
    procedure FillObjectRoot(Node: TTreeNode);
    procedure FillAndShowConstraintsForm(Form: TfmTableManage; ATableName: string; dbIndex: Integer);
    procedure ShowCompleteQueryWindow(DatabaseIndex: Integer; ATitle,
      AQueryText: string; OnCommitProcedure: TNotifyEvent = nil; ANodeInfos: TPNodeInfos=nil);
    // Gets fields info and fills TableManage form(!) grids with info
    procedure ViewTableFields(ATableName: string; dbIndex: Integer; AStringGrid: TStringGrid);
    procedure ShowIndicesManagement(AForm: TForm; DatabaseIndex: Integer; ATableName: string);
    Function GetTableNames(dbIndex: Integer): string;
    Function CreateNewTrigger(dbIndex: Integer; ATableName: string; OnCommitProcedure: TNotifyEvent = nil): Boolean;
    Function AddToSQLHistory(DatabaseTitle: string; SQLType, SQLStatement: string): Boolean;
    Function SaveAndCloseSQLHistory: Boolean;
    Function OpenSQLHistory(DatabaseTitle: string): Boolean;
    // Connects to database.
    // If not succesful (or if ForceConnectDialog is true), ask user for credentials and try again
    Function ConnectToDBAs(dbIndex: Integer; ForceConnectDialog: boolean=false): Boolean;
    Function IsLinux: Boolean;
    Function IsWindows: Boolean;
    Function IsUnix: Boolean;
    Function Is64bit: Boolean;
    Function Is32bit: Boolean;
    procedure CallRoutine(ARoutineType: TRoutineType);
    procedure SelectTreeViewNode(ARoutineInfo: TRoutineInfo);
    procedure AddRootObjects(DBNode: TTreeNode; DBIndex: Integer);
  end;


var
  fmMain: TfmMain;

  FClickedTabIndex: Integer;
  NoDragTab: Integer;  // -1 = no exclusion, otherwise excluded tab index
  FExcludeTabs: array of Integer; // list of excluded tab indexes


implementation

{ TfmMain }

uses CreateDb, ViewView, ViewTrigger, ViewSProc, ViewGen, NewTable, NewGen,
     EnterPass, CreateTrigger, fedittabledata, CallProc,  UDFInfo, ViewDomain,
     NewDomain, SysTables, Scriptdb, UserPermissions, BackupRestore, UnitFirebirdServices, CreateUser, ChangePass,
     PermissionManage, CopyTable, About, NewEditField, dbInfo, Comparison;


procedure TfmMain.mnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  {$IFNDEF DEBUG}
  // Do not log to debug server if built as release instead of debug
  SetDebuggingEnabled(false);
  {$ENDIF}
  Application.OnException:= @GlobalException;
  FActivated:= False;
  LoadRegisteredDatabases;
  StatusBar1.Panels[0].Text:= 'TurboBird for ' + Target + '-' + Arch;
  Application.OnShowHint := @AppShowHint;
  PageControl1.ShowHint := True;
  Application.ShowHint := True;
  NoDragTab := 0;
  SetLength(FExcludeTabs, 1);
  FExcludeTabs[0] := 0;
end;

procedure TfmMain.AppShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  i: Integer;
  r: TRect;
  p: TPoint;
  ts: TTabSheet;
begin
  // Mausposition ins PageControl-Koordinatensystem
  p := PageControl1.ScreenToClient(Mouse.CursorPos);

  for i := 0 to PageControl1.PageCount - 1 do
  begin
    r := PageControl1.TabRect(i);
    if PtInRect(r, p) then
    begin
      ts := PageControl1.Pages[i];
      HintStr := ts.Hint;
      CanShow := True;
      Exit;
    end;
  end;

  HintStr := '';
  CanShow := False;
end;


procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    if Length(RegisteredDatabases) > 0 then
      fmReg.SaveRegistrations;
    SaveAndCloseSQLHistory;
    lmDisconnectAllClick(nil);
    ReleaseRegisteredDatabases;
    CloseAction := caFree; // Alles OK → freigeben
  except
    on E: Exception do
    begin
      ShowMessage('Error while closing the databases: ' + E.Message);
      CloseAction := caNone; // NICHT schließen
    end;
  end;
end;

procedure TfmMain.ReleaseRegisteredDatabases;
var
  i: Integer;
  dbName: String;
begin
  for i := 0 to High(RegisteredDatabases) do
  begin
    dbName := RegisteredDatabases[i].IBConnection.DatabaseName;
    if dbName = '' then
      dbName := '(unknow)';

    if RegisteredDatabases[i].SQLTrans.Active then
      if  MessageDlg( Format('There is an open transaction in the database "%s". Do you want to commit it?',
         [dbName]), mtConfirmation, [mbYes, mbNo], 0 ) = mrYes
      then  RegisteredDatabases[i].SQLTrans.Commit
      else  RegisteredDatabases[i].SQLTrans.Rollback;

    try
      if RegisteredDatabases[i].IBConnection.Connected then
        RegisteredDatabases[i].IBConnection.Close;
    except
      on E: Exception do
        raise Exception.CreateFmt('Failed to close database connection "%s": %s',
          [dbName, E.Message]);
    end;
    FreeAndNil(RegisteredDatabases[i].SQLTrans);
    FreeAndNil(RegisteredDatabases[i].IBConnection);
  end;
  RegisteredDatabases := nil;
end;

procedure TfmMain.ImNewFBFunctionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new FireBird Functions');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('/*');
    QWindow.meQuery.Lines.Add('  Example Firebird SQL Functions');
    QWindow.meQuery.Lines.Add('  These functions demonstrate basic usage of native Firebird SQL');
    QWindow.meQuery.Lines.Add('  capabilities including numeric operations, string manipulation, ');
    QWindow.meQuery.Lines.Add('  date/time handling, and null checking.');
    QWindow.meQuery.Lines.Add('*/');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('SET TERM ^ ;');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION double_int(val INTEGER)');
    QWindow.meQuery.Lines.Add('RETURNS INTEGER');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  RETURN val * 2;');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION halve_float(val FLOAT)');
    QWindow.meQuery.Lines.Add('RETURNS FLOAT');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  RETURN val / 2;');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION percent_of_decimal(val DECIMAL(10,2), percent DECIMAL(5,2))');
    QWindow.meQuery.Lines.Add('RETURNS DECIMAL(10,2)');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  RETURN (val * percent) / 100;');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION reverse_string(txt VARCHAR(100))');
    QWindow.meQuery.Lines.Add('RETURNS VARCHAR(100)');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('DECLARE VARIABLE i INT;');
    QWindow.meQuery.Lines.Add('DECLARE VARIABLE res VARCHAR(100);');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  res = '''';');
    QWindow.meQuery.Lines.Add('  i = CHAR_LENGTH(txt);');
    QWindow.meQuery.Lines.Add('  WHILE (i > 0) DO');
    QWindow.meQuery.Lines.Add('  BEGIN');
    QWindow.meQuery.Lines.Add('    res = res || SUBSTRING(txt FROM i FOR 1);');
    QWindow.meQuery.Lines.Add('    i = i - 1;');
    QWindow.meQuery.Lines.Add('  END');
    QWindow.meQuery.Lines.Add('  RETURN res;');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION is_null(val INTEGER)');
    QWindow.meQuery.Lines.Add('RETURNS BOOLEAN');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  RETURN val IS NULL;');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION add_days(date_val DATE, days INTEGER)');
    QWindow.meQuery.Lines.Add('RETURNS DATE');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  RETURN date_val + days;');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION time_from_timestamp(ts TIMESTAMP)');
    QWindow.meQuery.Lines.Add('RETURNS TIME');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  RETURN CAST(ts AS TIME);');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION to_uppercase(val CHAR(10))');
    QWindow.meQuery.Lines.Add('RETURNS CHAR(10)');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  RETURN UPPER(val);');
    QWindow.meQuery.Lines.Add('END^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('SET TERM ; ^');
    QWindow.Show;
  end;
end;

procedure TfmMain.ImCreateNewPackageClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new package');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('/*');
    QWindow.meQuery.Lines.Add('    Package DEMO_PACKAGE Header');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    This package includes:');
    QWindow.meQuery.Lines.Add('    - Commented-out regex-based UDR routines (based on the REGEX package),');
    QWindow.meQuery.Lines.Add('      since REGEX is not installed by default in Firebird.');
    QWindow.meQuery.Lines.Add('      However, the package can be downloaded and installed from:');
    QWindow.meQuery.Lines.Add('      https://github.com/shalamyansky/fb_regex');
    QWindow.meQuery.Lines.Add('    - Regex-equivalent native (PSQL) functions and procedures in Firebird,');
    QWindow.meQuery.Lines.Add('      as an alternative to using external UDRs');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    Regex UDRs are prefixed with regex_ and');
    QWindow.meQuery.Lines.Add('    native routines are prefixed with native_');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    The purpose is to demonstrate the structure of Firebird packages.');
    QWindow.meQuery.Lines.Add('*/');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('SET TERM ^;');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER PACKAGE DEMO_PACKAGE');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');

    QWindow.meQuery.Lines.Add('procedure nativ_matches(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number  integer,');
    QWindow.meQuery.Lines.Add('    groups  varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('procedure nativ_groups(');
    QWindow.meQuery.Lines.Add('    groups varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    origin integer,');
    QWindow.meQuery.Lines.Add('    finish integer');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('procedure nativ_find(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    amount  integer,');
    QWindow.meQuery.Lines.Add('    pass    integer');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number  integer,');
    QWindow.meQuery.Lines.Add('    match   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('function nativ_find_first(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pass    integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set UTF8;');

    QWindow.meQuery.Lines.Add('function nativ_replace(');
    QWindow.meQuery.Lines.Add('    text        varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern     varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    replacement varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    amount      integer,');
    QWindow.meQuery.Lines.Add('    pass        integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set UTF8;');

    QWindow.meQuery.Lines.Add('procedure nativ_split_words(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    word   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('procedure nativ_split(');
    QWindow.meQuery.Lines.Add('    text      varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    separator varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    part   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('procedure regex_matches(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number  integer,');
    QWindow.meQuery.Lines.Add('    groups  varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('procedure regex_groups(');
    QWindow.meQuery.Lines.Add('    groups varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    origin integer,');
    QWindow.meQuery.Lines.Add('    finish integer');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('procedure regex_find(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    amount  integer,');
    QWindow.meQuery.Lines.Add('    pass    integer');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number  integer,');
    QWindow.meQuery.Lines.Add('    match   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('function regex_find_first(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pass    integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set UTF8;');

    QWindow.meQuery.Lines.Add('function regex_replace(');
    QWindow.meQuery.Lines.Add('    text        varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern     varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    replacement varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    amount      integer,');
    QWindow.meQuery.Lines.Add('    pass        integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set UTF8;');

    QWindow.meQuery.Lines.Add('procedure regex_split_words(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    word   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('procedure regex_split(');
    QWindow.meQuery.Lines.Add('    text      varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    separator varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    part   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(');');

    QWindow.meQuery.Lines.Add('end^');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('recreate package body demo_package');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('begin');

    QWindow.meQuery.Lines.Add('procedure nativ_matches(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    groups varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('declare variable pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable idx integer;');
    QWindow.meQuery.Lines.Add('declare variable found varchar(8191);');
    QWindow.meQuery.Lines.Add('declare variable res varchar(8191) = '''';');
    QWindow.meQuery.Lines.Add('declare variable count_matches integer = 0;');
    QWindow.meQuery.Lines.Add('begin');
    QWindow.meQuery.Lines.Add('  if (pattern is null or pattern = '''') then');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    number = 0;');
    QWindow.meQuery.Lines.Add('    groups = '''';');
    QWindow.meQuery.Lines.Add('    suspend;');
    QWindow.meQuery.Lines.Add('    exit;');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('  while (idx > 0) do');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    count_matches = count_matches + 1;');
    QWindow.meQuery.Lines.Add('    found = substring(text from idx for char_length(pattern));');
    QWindow.meQuery.Lines.Add('    if (res = '''') then');
    QWindow.meQuery.Lines.Add('      res = found;');
    QWindow.meQuery.Lines.Add('    else');
    QWindow.meQuery.Lines.Add('      res = res || '','' || found;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    pos = idx + char_length(pattern);');
    QWindow.meQuery.Lines.Add('    idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  number = count_matches;');
    QWindow.meQuery.Lines.Add('  groups = res;');
    QWindow.meQuery.Lines.Add('  suspend;');
    QWindow.meQuery.Lines.Add('end');

    QWindow.meQuery.Lines.Add('procedure nativ_groups(');
    QWindow.meQuery.Lines.Add('    groups varchar(8191) character set utf8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    origin integer,');
    QWindow.meQuery.Lines.Add('    finish integer');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('declare variable pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable idx integer;');
    QWindow.meQuery.Lines.Add('declare variable part varchar(8191);');
    QWindow.meQuery.Lines.Add('declare variable len_part integer;');
    QWindow.meQuery.Lines.Add('begin');
    QWindow.meQuery.Lines.Add('  number = 0;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  if (groups is null or groups = '''') then');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    number = 0;');
    QWindow.meQuery.Lines.Add('    origin = 0;');
    QWindow.meQuery.Lines.Add('    finish = 0;');
    QWindow.meQuery.Lines.Add('    suspend;');
    QWindow.meQuery.Lines.Add('    exit;');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  while (pos <= char_length(groups)) do');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    idx = position('','' , groups, pos);');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    if (idx > 0) then');
    QWindow.meQuery.Lines.Add('    begin');
    QWindow.meQuery.Lines.Add('      part = substring(groups from pos for idx - pos);');
    QWindow.meQuery.Lines.Add('      pos = idx + 1;');
    QWindow.meQuery.Lines.Add('    end');
    QWindow.meQuery.Lines.Add('    else');
    QWindow.meQuery.Lines.Add('    begin');
    QWindow.meQuery.Lines.Add('      part = substring(groups from pos);');
    QWindow.meQuery.Lines.Add('      pos = char_length(groups) + 1;');
    QWindow.meQuery.Lines.Add('    end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    len_part = char_length(part);');
    QWindow.meQuery.Lines.Add('    number = number + 1;');
    QWindow.meQuery.Lines.Add('    origin = 0;');
    QWindow.meQuery.Lines.Add('    finish = len_part - 1;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    suspend;');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('end');

    QWindow.meQuery.Lines.Add('procedure nativ_find(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    amount integer,');
    QWindow.meQuery.Lines.Add('    pass integer');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    match varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('declare variable pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable idx integer;');
    QWindow.meQuery.Lines.Add('declare variable count_matches integer = 0;');
    QWindow.meQuery.Lines.Add('declare variable output_count integer = 0;');
    QWindow.meQuery.Lines.Add('begin');
    QWindow.meQuery.Lines.Add('  if (pattern is null or pattern = '''' or amount <= 0) then');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    number = 0;');
    QWindow.meQuery.Lines.Add('    match = '''';');
    QWindow.meQuery.Lines.Add('    suspend;');
    QWindow.meQuery.Lines.Add('    exit;');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('  while (idx > 0) do');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    count_matches = count_matches + 1;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    if (count_matches >= pass and output_count < amount) then');
    QWindow.meQuery.Lines.Add('    begin');
    QWindow.meQuery.Lines.Add('      number = count_matches;');
    QWindow.meQuery.Lines.Add('      match = substring(text from idx for char_length(pattern));');
    QWindow.meQuery.Lines.Add('      output_count = output_count + 1;');
    QWindow.meQuery.Lines.Add('      suspend;');
    QWindow.meQuery.Lines.Add('    end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    pos = idx + char_length(pattern);');
    QWindow.meQuery.Lines.Add('    idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    if (output_count >= amount) then');
    QWindow.meQuery.Lines.Add('      leave;');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('end');

    QWindow.meQuery.Lines.Add('function nativ_find_first(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pass integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('declare variable pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable idx integer;');
    QWindow.meQuery.Lines.Add('declare variable count_matches integer = 0;');
    QWindow.meQuery.Lines.Add('declare variable result varchar(8191);');
    QWindow.meQuery.Lines.Add('begin');
    QWindow.meQuery.Lines.Add('  if (pattern is null or pattern = '''' or pass < 1) then');
    QWindow.meQuery.Lines.Add('    return null;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('  while (idx > 0) do');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    count_matches = count_matches + 1;');
    QWindow.meQuery.Lines.Add('    if (count_matches = pass) then');
    QWindow.meQuery.Lines.Add('    begin');
    QWindow.meQuery.Lines.Add('      result = substring(text from idx for char_length(pattern));');
    QWindow.meQuery.Lines.Add('      return result;');
    QWindow.meQuery.Lines.Add('    end');
    QWindow.meQuery.Lines.Add('    pos = idx + char_length(pattern);');
    QWindow.meQuery.Lines.Add('    idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  return null;');
    QWindow.meQuery.Lines.Add('end');

    QWindow.meQuery.Lines.Add('function nativ_replace(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set utf8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set utf8,');
    QWindow.meQuery.Lines.Add('    replacement varchar(8191) character set utf8,');
    QWindow.meQuery.Lines.Add('    amount integer,');
    QWindow.meQuery.Lines.Add('    pass integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set utf8');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('declare variable pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable idx integer;');
    QWindow.meQuery.Lines.Add('declare variable count_matches integer = 0;');
    QWindow.meQuery.Lines.Add('declare variable result varchar(8191) = '''';');
    QWindow.meQuery.Lines.Add('declare variable prev_pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable replaced integer = 0;');
    QWindow.meQuery.Lines.Add('declare variable text_len integer;');
    QWindow.meQuery.Lines.Add('declare variable pat_len integer;');
    QWindow.meQuery.Lines.Add('begin');
    QWindow.meQuery.Lines.Add('  if (pattern is null or pattern = '''' or amount = 0) then');
    QWindow.meQuery.Lines.Add('    return text;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  text_len = character_length(text);');
    QWindow.meQuery.Lines.Add('  pat_len = character_length(pattern);');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  while (idx > 0 and replaced < amount) do');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    count_matches = count_matches + 1;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    if (count_matches >= pass) then');
    QWindow.meQuery.Lines.Add('    begin');
    QWindow.meQuery.Lines.Add('      result = result || substring(text from prev_pos for idx - prev_pos);');
    QWindow.meQuery.Lines.Add('      result = result || replacement;');
    QWindow.meQuery.Lines.Add('      replaced = replaced + 1;');
    QWindow.meQuery.Lines.Add('      prev_pos = idx + pat_len;');
    QWindow.meQuery.Lines.Add('    end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    pos = idx + pat_len;');
    QWindow.meQuery.Lines.Add('    idx = position(pattern, text, pos);');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  if (prev_pos <= text_len) then');
    QWindow.meQuery.Lines.Add('    result = result || substring(text from prev_pos);');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  return result;');
    QWindow.meQuery.Lines.Add('end');

    QWindow.meQuery.Lines.Add('procedure nativ_split_words(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set utf8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    word varchar(8191) character set utf8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('declare variable pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable start_pos integer;');
    QWindow.meQuery.Lines.Add('declare variable end_pos integer;');
    QWindow.meQuery.Lines.Add('declare variable len integer;');
    QWindow.meQuery.Lines.Add('declare variable w varchar(8191);');
    QWindow.meQuery.Lines.Add('begin');
    QWindow.meQuery.Lines.Add('  number = 0;');
    QWindow.meQuery.Lines.Add('  len = character_length(text);');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  while (pos <= len) do');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    while (pos <= len and substring(text from pos for 1) = '' '') do');
    QWindow.meQuery.Lines.Add('      pos = pos + 1;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    if (pos > len) then');
    QWindow.meQuery.Lines.Add('      exit;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    start_pos = pos;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    while (pos <= len and substring(text from pos for 1) <> '' '') do');
    QWindow.meQuery.Lines.Add('      pos = pos + 1;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    end_pos = pos - 1;');
    QWindow.meQuery.Lines.Add('    number = number + 1;');
    QWindow.meQuery.Lines.Add('    word = substring(text from start_pos for end_pos - start_pos + 1);');
    QWindow.meQuery.Lines.Add('    suspend;');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('end');

    QWindow.meQuery.Lines.Add('procedure nativ_split(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set utf8,');
    QWindow.meQuery.Lines.Add('    separator varchar(8191) character set utf8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    part varchar(8191) character set utf8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('as');
    QWindow.meQuery.Lines.Add('declare variable pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable start_pos integer = 1;');
    QWindow.meQuery.Lines.Add('declare variable idx integer;');
    QWindow.meQuery.Lines.Add('declare variable count_parts integer = 0;');
    QWindow.meQuery.Lines.Add('declare variable sep_char char(1);');
    QWindow.meQuery.Lines.Add('begin');
    QWindow.meQuery.Lines.Add('  if (separator is null or character_length(separator) = 0) then');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    number = 1;');
    QWindow.meQuery.Lines.Add('    part = text;');
    QWindow.meQuery.Lines.Add('    suspend;');
    QWindow.meQuery.Lines.Add('    exit;');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  sep_char = substring(separator from 1 for 1);');
    QWindow.meQuery.Lines.Add('  idx = position(sep_char, text, start_pos);');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('  while (true) do');
    QWindow.meQuery.Lines.Add('  begin');
    QWindow.meQuery.Lines.Add('    if (idx = 0) then');
    QWindow.meQuery.Lines.Add('    begin');
    QWindow.meQuery.Lines.Add('      count_parts = count_parts + 1;');
    QWindow.meQuery.Lines.Add('      number = count_parts;');
    QWindow.meQuery.Lines.Add('      part = substring(text from start_pos);');
    QWindow.meQuery.Lines.Add('      suspend;');
    QWindow.meQuery.Lines.Add('      exit;');
    QWindow.meQuery.Lines.Add('    end');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    count_parts = count_parts + 1;');
    QWindow.meQuery.Lines.Add('    number = count_parts;');
    QWindow.meQuery.Lines.Add('    part = substring(text from start_pos for idx - start_pos);');
    QWindow.meQuery.Lines.Add('    suspend;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('    start_pos = idx + 1;');
    QWindow.meQuery.Lines.Add('    idx = position(sep_char, text, start_pos);');
    QWindow.meQuery.Lines.Add('  end');
    QWindow.meQuery.Lines.Add('end');

    QWindow.meQuery.Lines.Add('procedure regex_matches(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number  integer,');
    QWindow.meQuery.Lines.Add('    groups  varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') external name');
    QWindow.meQuery.Lines.Add('    ''fb_regex!matches''');
    QWindow.meQuery.Lines.Add('engine');
    QWindow.meQuery.Lines.Add('    udr;');

    QWindow.meQuery.Lines.Add('procedure regex_groups(');
    QWindow.meQuery.Lines.Add('    groups varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    origin integer,');
    QWindow.meQuery.Lines.Add('    finish integer');
    QWindow.meQuery.Lines.Add(') external name');
    QWindow.meQuery.Lines.Add('    ''fb_regex!groups''');
    QWindow.meQuery.Lines.Add('engine');
    QWindow.meQuery.Lines.Add('    udr;');

    QWindow.meQuery.Lines.Add('procedure regex_find(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    amount  integer,');
    QWindow.meQuery.Lines.Add('    pass    integer');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    match  varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') external name');
    QWindow.meQuery.Lines.Add('    ''fb_regex!find''');
    QWindow.meQuery.Lines.Add('engine');
    QWindow.meQuery.Lines.Add('    udr;');

    QWindow.meQuery.Lines.Add('function regex_find_first(');
    QWindow.meQuery.Lines.Add('    text    varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pass    integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add('external name');
    QWindow.meQuery.Lines.Add('    ''fb_regex!find_first''');
    QWindow.meQuery.Lines.Add('engine');
    QWindow.meQuery.Lines.Add('    udr;');

    QWindow.meQuery.Lines.Add('function regex_replace(');
    QWindow.meQuery.Lines.Add('    text        varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    pattern     varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    replacement varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    amount      integer,');
    QWindow.meQuery.Lines.Add('    pass        integer');
    QWindow.meQuery.Lines.Add(') returns varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add('external name');
    QWindow.meQuery.Lines.Add('    ''fb_regex!replace''');
    QWindow.meQuery.Lines.Add('engine');
    QWindow.meQuery.Lines.Add('    udr;');

    QWindow.meQuery.Lines.Add('procedure regex_split_words(');
    QWindow.meQuery.Lines.Add('    text varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    word   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') external name');
    QWindow.meQuery.Lines.Add('    ''fb_regex!split_words''');
    QWindow.meQuery.Lines.Add('engine');
    QWindow.meQuery.Lines.Add('    udr;');

    QWindow.meQuery.Lines.Add('procedure regex_split(');
    QWindow.meQuery.Lines.Add('    text      varchar(8191) character set UTF8,');
    QWindow.meQuery.Lines.Add('    separator varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') returns (');
    QWindow.meQuery.Lines.Add('    number integer,');
    QWindow.meQuery.Lines.Add('    part   varchar(8191) character set UTF8');
    QWindow.meQuery.Lines.Add(') external name');
    QWindow.meQuery.Lines.Add('    ''fb_regex!split''');
    QWindow.meQuery.Lines.Add('engine');
    QWindow.meQuery.Lines.Add('    udr;');

    QWindow.meQuery.Lines.Add('end^');

    QWindow.meQuery.Lines.Add('SET TERM ;^');
  end;
end;

procedure TfmMain.ImEditFBFunctionClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TSQLQuery;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetFirebirdFunctionDeclaration(Rec.IBConnection, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit FireBird Function: ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

(*****************  Add New user  ***********************)
procedure TfmMain.lmAddUserClick(Sender: TObject);
var
  SelNode: TTreeNode;
  dbIndex: Integer;
begin
  with fmCreateUser do
  try
    SelNode:= tvMain.Selected;
    dbIndex:= TPNodeInfos(SelNode.Parent.Data)^.dbIndex;
    Init(dbIndex);
    edUserName.Clear;
    edPassword.Clear;
    if ShowModal = mrOK then
    begin
      // Create user
      dmSysTables.Init(dbIndex);
      dmSysTables.sqQuery.Close;
      dmSysTables.sqQuery.SQL.Text:= 'create user ' + edUserName.Text + ' password ' + QuotedStr(edPassword.Text);
      dmSysTables.sqQuery.ExecSQL;

      // Grant rule
      if cxGrantRole.Checked then
      begin
        dmSysTables.sqQuery.SQL.Text:= 'grant ' + cbRoles.Text + ' to ' + edUserName.Text;
        dmSysTables.sqQuery.ExecSQL;
      end;
      dmSysTables.stTrans.Commit;
      MessageDlg('New user (' + edUserName.Text + ') has been created successfully', mtInformation, [mbOk], 0);
      if not cxGrantRole.Checked then
        ShowMessage('User (' + edUserName.Text + ') will not appear in users list unless you grant it a permission');
      lmRefresh.Click;
    end;
  except
    on E: Exception do
    begin
      MessageDlg('Error while creating new user: ' + e.Message, mtError, [mbOk], 0);
    end;
  end;
end;

(***********  Backup / Restore database ************)
procedure TfmMain.lmBackupClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  with RegisteredDatabases[TPNodeInfos(tvMain.Selected.Data)^.dbIndex ].RegRec do
    fmBackupRestore.Init(SelNode.Text, DatabaseName, UserName, Password);
  fmBackupRestore.cbOperation.Enabled:= True;
  fmBackupRestore.Show;
end;

(**********  change user password  **********)
procedure TfmMain.lmChangePasswordClick(Sender: TObject);
begin
  fmChangePass.Caption:= 'Change password for user: ' + tvMain.Selected.Text;
  fmChangePass.edPassword.Clear;
  fmChangePass.edConfirm.Clear;
  if fmChangePass.ShowModal = mrOK then
  begin
    try
       dmSysTables.Init(TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex );
       dmSysTables.sqQuery.Close;
       dmSysTables.sqQuery.SQL.Text:= 'alter user ' + tvMain.Selected.Text +
         ' password ' + QuotedStr(fmChangePass.edPassword.Text);
       dmSysTables.sqQuery.ExecSQL;
       dmSysTables.stTrans.Commit;
       MessageDlg('Password has been changed', mtInformation, [mbOk], 0);
    except
      on E: Exception do
        ShowMessage('Error while changing password: ' + e.Message);
    end;
  end;
end;

procedure TfmMain.lmCompareClick(Sender: TObject);
var
  dbIndex: Integer;
  Title: string;
  ATab: TTabSheet;
begin
  dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  // Check if password is saved - it may be empty, which can be valid for
  // e.g. embedded databases
  if (RegisteredDatabases[dbIndex].RegRec.SavePassword) or
    ConnectToDBAs(dbIndex) then
  begin
    Title:= RegisteredDatabases[dbIndex].RegRec.Title + ': Database Comparison';
    fmComparison:= FindCustomForm(Title, TfmComparison) as TfmComparison;
    if fmComparison = nil then
    begin
      fmComparison:= TfmComparison.Create(Application);
      ATab:= TTabSheet.Create(self);
      ATab.Parent:= PageControl1;
      fmComparison.Parent:= ATab;
      fmComparison.Left:= 0;
      fmComparison.Top:= 0;
      fmComparison.BorderStyle:= bsNone;
      fmComparison.Align:= alClient;
      fmComparison.Caption:= Title;
    end
    else
      ATab:= fmComparison.Parent as TTabSheet;

    PageControl1.ActivePage:= ATab;
    ATab.Tag:= dbIndex;
    ATab.Caption:= Title;
    fmComparison.Init(dbIndex);
    fmComparison.Show;
  end;
end;

procedure TfmMain.lmCopyRolePermissionClick(Sender: TObject);
begin
  lmCopyUserPermissionClick(nil);
end;

procedure TfmMain.lmCopyUserPermissionClick(Sender: TObject);
var
  List: TStringList;
  dbIndex: Integer;
  UserName: string;
  NewUser: string;
begin
  if InputQuery('Permission', 'Please type a User/Role name to copy perissions to', NewUser) then
  begin
    UserName:= tvMain.Selected.Text;
    dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
    List:= TStringList.Create;
    try
      Scriptdb.ScriptUserAllPermissions(dbIndex, UserName, List, NewUser);
      ShowCompleteQueryWindow(dbIndex, 'Script permissions for : ' + UserName, List.Text);
    finally
      List.Free;
    end;
  end;
end;

procedure TfmMain.lmCopyTableClick(Sender: TObject);
begin
  fmCopyTable.Init(TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex, tvMain.Selected.Text);
  fmCopyTable.Show;
end;

procedure TfmMain.lmCreateDBClick(Sender: TObject);
begin
  fmCreateDB.edNewDatabase.Text:= tvMain.Selected.Text + ':';
  mnCreateDBClick(nil);
end;

procedure TfmMain.lmDBInfoClick(Sender: TObject);
var
  ATab: TTabSheet;
  Title: string;
  dbIndex: Integer;
  fmDBInfo: TfmDBInfo;
begin
  dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  Title:= 'DB-infos for: ' + tvMain.Selected.Text;
  fmDBInfo:= FindCustomForm(Title, TfmDBInfo) as TfmDBInfo;

  if fmDBInfo = nil then
  begin
    fmDBInfo:= TfmDBInfo.Create(Application);
    ATab:= TTabSheet.Create(self);
    ATab.Parent:= PageControl1;
    fmDBInfo.Parent:= ATab;
    fmDBInfo.Left:= 0;
    fmDBInfo.Top:= 0;
    fmDBInfo.BorderStyle:= bsNone;
    fmDBInfo.Align:= alClient;
    Caption:= Title;
  end
  else
    ATab:= fmDBInfo.Parent as TTabSheet;

  PageControl1.ActivePage:= ATab;
  ATab.Tag:= dbIndex;
  ATab.Caption:= Title;

  fmDBInfo.Init(dbIndex);
end;

procedure TfmMain.lmDisconnectAllClick(Sender: TObject);
var
  dbIndex, i, j, k: Integer;
  TabSheet: TTabSheet;
begin
  // Alle Verbindungen durchgehen
  for dbIndex := 0 to Length(RegisteredDatabases) - 1 do
  begin
    if RegisteredDatabases[dbIndex].IBConnection.Connected then
    begin
      // Verbindung trennen
      RegisteredDatabases[dbIndex].IBConnection.Close;

      // Tabs schließen, die zu dieser Verbindung gehören
      for i := PageControl1.PageCount - 1 downto 0 do
      begin
        if (PageControl1.Pages[i] as TComponent).Tag = dbIndex then
        begin
          TabSheet := PageControl1.Pages[i] as TTabSheet;

          // Unterformulare schließen
          for j := 0 to TabSheet.ControlCount - 1 do
          begin
            if TabSheet.Controls[j] is TForm then
            begin
              (TabSheet.Controls[j] as TForm).Close;
              Break;
            end;
          end;

          // TabSheet freigeben
          TabSheet.Free;
        end;
      end;

      // Knoten im TreeView einklappen, falls vorhanden
      for k := 0 to tvMain.Items.Count - 1 do
      begin
        if Assigned(tvMain.Items[k].Data) and
           (TPNodeInfos(tvMain.Items[k].Data)^.dbIndex = dbIndex) then
        begin
          tvMain.Items[k].Collapse(True);
        end;
      end;
    end;
  end;
end;
procedure TfmMain.lmDisconnectClick(Sender: TObject);
var
  dbIndex: Integer;
  i: Integer;
  j: Integer;
  TabSheet: TTabSheet;
begin
  //dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  if tvMain.Selected = nil then exit;
  dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  RegisteredDatabases[dbIndex].IBConnection.Close;
  for i:= PageControl1.PageCount - 1 downto 0 do
    if (PageControl1.Pages[i] as TComponent).Tag = dbIndex then
    begin
      TabSheet:= PageControl1.Page[i] as TTabSheet;
      for j:= 0 to TabSheet.ControlCount - 1 do
      if TabSheet.Controls[j] is TForm then
      begin
        (TabSheet.Controls[j] as TForm).Close;
        TabSheet.Free;
        Break;
      end;
    end;
  tvMain.Selected.Collapse(True);
end;

procedure TfmMain.lmDropDomainClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetDomainDeps(Rec.IBConnection, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP Domain ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Domain ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The Domain "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the Domain anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP Domain ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Domain ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropFireBirdFunctionClick(Sender: TObject);
var
  DepStr, TmpQueryStr: string;
  dbIndex: integer;
  Rec: TDatabaseRec;
  Q: TSQLQuery;
  IsNative: Boolean;
  FuncName: string;
begin
  // Get DB index from node hierarchy
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  FuncName := UpperCase(Trim(tvMain.Selected.Text));
  IsNative := False;

  // Check if the function is really a native PSQL function (not UDR)
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Rec.IBConnection;
    Q.SQL.Text :=
      'SELECT RDB$ENGINE_NAME FROM RDB$FUNCTIONS WHERE RDB$FUNCTION_NAME = :FN';
    Q.ParamByName('FN').AsString := FuncName;
    Q.Open;
    if not Q.EOF then
      IsNative := Q.FieldByName('RDB$ENGINE_NAME').IsNull;
  finally
    Q.Free;
  end;

  if not IsNative then
  begin
    MessageDlg(
      Format('Function "%s" is not a native Firebird PSQL function and cannot be dropped using this operation.', [FuncName]),
      mtWarning, [mbOK], 0);
    Exit;
  end;

  // Check dependencies
  DepStr := GetFBFunctionDeps(Rec.IBConnection, FuncName, '');
  if DepStr = '' then
  begin
    TmpQueryStr := 'DROP FUNCTION ' + FuncName;
    ShowCompleteQueryWindow(dbIndex, 'Drop Function ' + FuncName, TmpQueryStr, nil);
  end
  else
  begin
    if MessageDlg(
      Format('The Function "%s" has dependencies!' + sLineBreak + sLineBreak +
             '%s' + sLineBreak +
             'Do you want to delete the Function anyway?', [FuncName, DepStr]),
      mtWarning, [mbYes, mbCancel], 0) = mrYes then
    begin
      TmpQueryStr := 'DROP FUNCTION ' + FuncName;
      ShowCompleteQueryWindow(dbIndex, 'Drop Function ' + FuncName, TmpQueryStr, nil);
    end;
  end;
end;

procedure TfmMain.lmDropGeneratorClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetGeneratorDeps(Rec.IBConnection, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP Generator ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Generator ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The Generator "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the Generator anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP Generator ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Generator ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropPackageClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetPackageDependencies(Rec.IBConnection, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP PACKAGE ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Package ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The package "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the package anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP PACKAGE ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Package ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropStoredProcedureClick(Sender: TObject);
var
  DepStr, TmpQueryStr: string;
  dbIndex: integer;
  Rec: TDatabaseRec;
  Q: TSQLQuery;
  IsNative: Boolean;
  ProcName: string;
begin
  // Get DB index from node hierarchy
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  ProcName := UpperCase(Trim(tvMain.Selected.Text));
  IsNative := False;

  // Check if the procedure is a native PSQL procedure
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Rec.IBConnection;
    Q.SQL.Text :=
      'SELECT RDB$ENGINE_NAME FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = :PN';
    Q.ParamByName('PN').AsString := ProcName;
    Q.Open;
    if not Q.EOF then
      IsNative := Q.FieldByName('RDB$ENGINE_NAME').IsNull;
  finally
    Q.Free;
  end;

  if not IsNative then
  begin
    MessageDlg(
      Format('Procedure "%s" is not a native Firebird PSQL procedure and cannot be dropped using this operation.', [ProcName]),
      mtWarning, [mbOK], 0);
    Exit;
  end;

  // Check dependencies
  DepStr := GetFBProcedureDeps(Rec.IBConnection, ProcName, '');
  if DepStr = '' then
  begin
    TmpQueryStr := 'DROP PROCEDURE ' + ProcName;
    ShowCompleteQueryWindow(dbIndex, 'Drop Procedure ' + ProcName, TmpQueryStr, nil);
  end
  else
  begin
    if MessageDlg(
      Format('The Procedure "%s" has dependencies!' + sLineBreak + sLineBreak +
             '%s' + sLineBreak +
             'Do you want to delete the Procedure anyway?', [ProcName, DepStr]),
      mtWarning, [mbYes, mbCancel], 0) = mrYes then
    begin
      TmpQueryStr := 'DROP PROCEDURE ' + ProcName;
      ShowCompleteQueryWindow(dbIndex, 'Drop Procedure ' + ProcName, TmpQueryStr, nil);
    end;
  end;
end;

procedure TfmMain.lmDropTriggerClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetTriggerDeps(Rec.IBConnection, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP Trigger ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Trigger ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The Trigger "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the Trigger anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP Trigger ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Trigger ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropUDFFunctionClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetUDFDeps(Rec.IBConnection, tvMain.Selected.Text, '');
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP Function ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Function ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The Function "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the Function anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP EXTERNAL Function ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Function ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropUDRFunctionClick(Sender: TObject);
var
  DepStr, TmpQueryStr: string;
  dbIndex: integer;
  Rec: TDatabaseRec;
  Q: TSQLQuery;
  IsUDR: Boolean;
  FunctionName: string;
begin
  // Get database index (parent node)
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  FunctionName := UpperCase(Trim(tvMain.Selected.Text));
  IsUDR := False;

  // Check if the function is really a UDR function by inspecting RDB$ENGINE_NAME
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Rec.IBConnection;
    Q.SQL.Text :=
      'SELECT RDB$ENGINE_NAME FROM RDB$FUNCTIONS ' +
      'WHERE RDB$FUNCTION_NAME = :FN';
    Q.ParamByName('FN').AsString := FunctionName;
    Q.Open;
    if not Q.EOF then
      IsUDR := not Q.FieldByName('RDB$ENGINE_NAME').IsNull;
  finally
    Q.Free;
  end;

  if not IsUDR then
  begin
    MessageDlg(
      Format('Function "%s" is not a UDR function and cannot be dropped using this operation.', [FunctionName]),
      mtWarning, [mbOK], 0);
    Exit;
  end;

  // Check for dependencies before dropping
  DepStr := GetUDRFunctionDeps(Rec.IBConnection, FunctionName, '');
  if DepStr = '' then
  begin
    TmpQueryStr := 'DROP FUNCTION ' + FunctionName;
    ShowCompleteQueryWindow(dbIndex, 'Drop Function ' + FunctionName, TmpQueryStr, nil);
  end
  else
  begin
    if MessageDlg(
      Format('The UDR function "%s" has dependencies!' + sLineBreak + sLineBreak +
             '%s' + sLineBreak +
             'Do you want to delete the function anyway?', [FunctionName, DepStr]),
      mtWarning, [mbYes, mbCancel], 0) = mrYes then
    begin
      TmpQueryStr := 'DROP FUNCTION ' + FunctionName;
      ShowCompleteQueryWindow(dbIndex, 'Drop Function ' + FunctionName, TmpQueryStr, nil);
    end;
  end;
end;

procedure TfmMain.lmDropUDRProcedureClick(Sender: TObject);
var
  DepStr, TmpQueryStr: string;
  dbIndex: integer;
  Rec: TDatabaseRec;
  Q: TSQLQuery;
  IsUDR: Boolean;
  ProcName: string;
begin
  // Get database index (parent of parent node)
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  ProcName := UpperCase(Trim(tvMain.Selected.Text));
  IsUDR := False;

  // Check if the procedure is really a UDR procedure by inspecting RDB$ENGINE_NAME
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Rec.IBConnection;
    Q.SQL.Text :=
      'SELECT RDB$ENGINE_NAME FROM RDB$PROCEDURES ' +
      'WHERE RDB$PROCEDURE_NAME = :PN';
    Q.ParamByName('PN').AsString := ProcName;
    Q.Open;
    if not Q.EOF then
      IsUDR := not Q.FieldByName('RDB$ENGINE_NAME').IsNull;
  finally
    Q.Free;
  end;

  if not IsUDR then
  begin
    MessageDlg(
      Format('Procedure "%s" is not a UDR procedure and cannot be dropped using this operation.', [ProcName]),
      mtWarning, [mbOK], 0);
    Exit;
  end;

  // Check for dependencies before dropping
  DepStr := GetUDRProcedureDeps(Rec.IBConnection, ProcName, '');
  if DepStr = '' then
  begin
    TmpQueryStr := 'DROP PROCEDURE ' + ProcName;
    ShowCompleteQueryWindow(dbIndex, 'Drop Procedure ' + ProcName, TmpQueryStr, nil);
  end
  else
  begin
    if MessageDlg(
      Format('The UDR procedure "%s" has dependencies!' + sLineBreak + sLineBreak +
             '%s' + sLineBreak +
             'Do you want to delete the procedure anyway?', [ProcName, DepStr]),
      mtWarning, [mbYes, mbCancel], 0) = mrYes then
    begin
      TmpQueryStr := 'DROP PROCEDURE ' + ProcName;
      ShowCompleteQueryWindow(dbIndex, 'Drop Procedure ' + ProcName, TmpQueryStr, nil);
    end;
  end;
end;

procedure TfmMain.lmDropUserClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetUserDeps(Rec.IBConnection, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP User ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop User ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The User "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the User anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP User ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop User ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropViewClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetViewDeps(Rec.IBConnection, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP View ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop View ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The View "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the View anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP View ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop View ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmEditFieldClick(Sender: TObject);
var
  SelNode: TTreeNode;
  dbIndex: Integer;
  FieldName: string;
  FieldType, DefaultValue: string;
  Size, Scale, Precision: Integer;
  Description, Characterset, Collation: string;
  NotNull: Boolean;
begin
  SelNode:= tvMain.Selected;
  dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Parent.Data)^.dbIndex;
  FieldName:= Copy(SelNode.Text, 1, Pos(' ', SelNode.Text) - 1);
  if dmSysTables.GetFieldInfo(dbIndex, SelNode.Parent.Text, FieldName,
    FieldType, Size, Precision, Scale, NotNull,
    DefaultValue, Characterset, Collation, Description) then
  begin
    fmNewEditField:= TfmNewEditField.Create(nil);
    fmNewEditField.Init(dbIndex, SelNode.Parent.Text, foEdit,
      FieldName, FieldType,
      CharacterSet, Collation,
      DefaultValue, Description,
      Size, Precision, Scale,
      //PtrInt(SelNode.Data),
      TPNodeInfos(SelNode.Data)^.dbIndex,
      not NotNull, nil);

    fmNewEditField.ShowModal;
  end
  else
    ShowMessage('Unable to locate the field: ' + SelNode.Text);
end;

procedure TfmMain.lmEditPackageClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TSQLQuery;
    TmpQueryList: TStringList;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryList := fetch_package.GetPackageDeclaration(Rec.IBConnection, tvMain.Selected.Text);
  TmpQueryStr  := TmpQueryList.Text;
  TmpQueryList.Free;
  ShowCompleteQueryWindow(dbIndex, 'Edit Package ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmFirebirdConfigClick(Sender: TObject);
var Rec: TDatabaseRec;
    dbIndex: Integer; Title: string; ATab: TTabSheet;
    fmFirebirdConfig: TfmFirebirdConfig;
begin
    dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
    Rec := RegisteredDatabases[dbIndex];

    Title := 'Configure FireBird Server';
    fmFirebirdConfig:= FindCustomForm(Title, TfmFirebirdConfig) as TfmFirebirdConfig;

    if fmFirebirdConfig = nil then
    begin
      fmFirebirdConfig := TfmFirebirdConfig.Create(Application);
      ATab:= TTabSheet.Create(self);
      ATab.Parent:= PageControl1;
      fmFirebirdConfig.Parent:= ATab;
      fmFirebirdConfig.Left:= 0;
      fmFirebirdConfig.Top:= 0;
      fmFirebirdConfig.BorderStyle:= bsNone;
      fmFirebirdConfig.Align:= alClient;
      fmFirebirdConfig.Caption:= Title;
  end else;
    ATab:= fmFirebirdConfig.Parent as TTabSheet;

  PageControl1.ActivePage:= ATab;
  ATab.Tag:= dbIndex;
  ATab.Caption:= Title;
  fmFirebirdConfig.Init(dbIndex);
  fmFirebirdConfig.Show;
end;

procedure TfmMain.lmGetPackageFunctionClick(Sender: TObject);
var   TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
      Node: TTreeNode;
begin
  Node := tvMain.Selected;
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := udb_package_firebird_func_fetcher.GetPackageFirebirdFunctionDeclaration(Rec.IBConnection, Node.Text, Node.Parent.Parent.Text);
  ShowCompleteQueryWindow(dbIndex, 'Edit Package-FireBird Function: ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmGetPackageProcedureClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
        Rec: TDatabaseRec;
        TmpQuery: TSQLQuery;
        Node: TTreeNode;
begin
    Node := tvMain.Selected;
    //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
    dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
    Rec := RegisteredDatabases[dbIndex];
    TmpQueryStr := udb_package_firebird_proc_fetcher.GetPackageFirebirdProcedureDeclaration(Rec.IBConnection, Node.Text, Node.Parent.Parent.Text);
    ShowCompleteQueryWindow(dbIndex, 'Edit Package-FireBird Procedure: ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmEditUDFFuctionClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TSQLQuery;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetUDFFunctionDeclaration(Rec.IBConnection, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit UDF ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmEditUDRFunctionClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TSQLQuery;
begin
  dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  //dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetUDRFunctionDeclaration(Rec.IBConnection, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit UDR Function: ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmEditUDRProcedureClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TSQLQuery;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := udb_udr_proc_fetcher.GetUDRProcedureDeclaration(Rec.IBConnection, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit UDR Procedure: ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmGetIncrementGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AGenName: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;

    AGenName:= SelNode.Text;

    ShowCompleteQueryWindow(dbIndex, 'get increment generator SQL for:' + AGenName,
      'select GEN_ID(' + AGenName + ', 1) from RDB$Database;');
  end;
end;

procedure TfmMain.lmImportTableClick(Sender: TObject);
var
  MyImportTable: TfmImportTable;
begin
  MyImportTable:=TfmImportTable.Create(nil);
  try
	  // Pass db index and table name
	  MyImportTable.Init(TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex,
		  tvMain.Selected.Text);
    MyImportTable.ShowModal;
  finally
    MyImportTable.Free;
  end;
end;

procedure TfmMain.lmNewUDRFunctionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create UDR-Function');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('/*');
    QWindow.meQuery.Lines.Add('  Firebird REGEX Package Functions');
    QWindow.meQuery.Lines.Add('  These are UDR-based functions from fb_regex.dll');
    QWindow.meQuery.Lines.Add('  for performing advanced regular expression operations.');
    QWindow.meQuery.Lines.Add('*/');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('SET TERM ^ ;');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION find_first(');
    QWindow.meQuery.Lines.Add('    text    VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    pattern VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    pass    INTEGER');
    QWindow.meQuery.Lines.Add(') RETURNS VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add('EXTERNAL NAME ''fb_regex!find_first''');
    QWindow.meQuery.Lines.Add('ENGINE UDR^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER FUNCTION replace(');
    QWindow.meQuery.Lines.Add('    text        VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    pattern     VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    replacement VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    amount      INTEGER,');
    QWindow.meQuery.Lines.Add('    pass        INTEGER');
    QWindow.meQuery.Lines.Add(') RETURNS VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add('EXTERNAL NAME ''fb_regex!replace''');
    QWindow.meQuery.Lines.Add('ENGINE UDR^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('SET TERM ; ^');
    QWindow.Show;
  end;
end;

procedure TfmMain.lmNewUDRProcedureClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create UDR-Function');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('/*');
    QWindow.meQuery.Lines.Add('  Firebird REGEX Package Procedures');
    QWindow.meQuery.Lines.Add('  These are UDR-based procedures from fb_regex.dll');
    QWindow.meQuery.Lines.Add('  for regex matching, finding, grouping, and splitting.');
    QWindow.meQuery.Lines.Add('*/');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('SET TERM ^ ;');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER PROCEDURE matches(');
    QWindow.meQuery.Lines.Add('    text    VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    pattern VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(') RETURNS (');
    QWindow.meQuery.Lines.Add('    number  INTEGER,');
    QWindow.meQuery.Lines.Add('    groups  VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('EXTERNAL NAME ''fb_regex!matches''');
    QWindow.meQuery.Lines.Add('ENGINE UDR^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER PROCEDURE groups(');
    QWindow.meQuery.Lines.Add('    groups VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(') RETURNS (');
    QWindow.meQuery.Lines.Add('    number INTEGER,');
    QWindow.meQuery.Lines.Add('    origin INTEGER,');
    QWindow.meQuery.Lines.Add('    finish INTEGER');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('EXTERNAL NAME ''fb_regex!groups''');
    QWindow.meQuery.Lines.Add('ENGINE UDR^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER PROCEDURE find(');
    QWindow.meQuery.Lines.Add('    text    VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    pattern VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    amount  INTEGER,');
    QWindow.meQuery.Lines.Add('    pass    INTEGER');
    QWindow.meQuery.Lines.Add(') RETURNS (');
    QWindow.meQuery.Lines.Add('    number  INTEGER,');
    QWindow.meQuery.Lines.Add('    match   VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('EXTERNAL NAME ''fb_regex!find''');
    QWindow.meQuery.Lines.Add('ENGINE UDR^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER PROCEDURE split_words(');
    QWindow.meQuery.Lines.Add('    text VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(') RETURNS (');
    QWindow.meQuery.Lines.Add('    number INTEGER,');
    QWindow.meQuery.Lines.Add('    word   VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('EXTERNAL NAME ''fb_regex!split_words''');
    QWindow.meQuery.Lines.Add('ENGINE UDR^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('CREATE OR ALTER PROCEDURE split(');
    QWindow.meQuery.Lines.Add('    text      VARCHAR(8191) CHARACTER SET UTF8,');
    QWindow.meQuery.Lines.Add('    separator VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(') RETURNS (');
    QWindow.meQuery.Lines.Add('    number INTEGER,');
    QWindow.meQuery.Lines.Add('    part   VARCHAR(8191) CHARACTER SET UTF8');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('EXTERNAL NAME ''fb_regex!split''');
    QWindow.meQuery.Lines.Add('ENGINE UDR^');
    QWindow.meQuery.Lines.Add('');

    QWindow.meQuery.Lines.Add('SET TERM ; ^');

    QWindow.Show;
  end;
end;

procedure TfmMain.FormActivate(Sender: TObject);
begin
  FActivated:= True;
end;

(***************  Open System table  **************)
procedure TfmMain.lmOpenSystemTableClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, SelNode.Text, SelNode.Data);
    QWindow.meQuery.Lines.Text:= 'select * from ' + SelNode.Text;
    QWindow.bbRunClick(nil);
    QWindow.Show;
  end;
end;

procedure TfmMain.lmActivateTrigClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if ChangeTriggerActivity(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, SelNode.Text, True) then
    MessageDlg('Trigger has been activated', mtInformation, [mbOk], 0);
end;

procedure TfmMain.CallRoutine(ARoutineType: TRoutineType);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  FRoutineInfo: TRoutineInfo;
  Rec: TDatabaseRec;
  dbIndex: Integer;
  ATab: TTabSheet;
  frmTestFunction: TfrmTestFunction;
  DBAlias: string;
  ShortTitle, FullHint: string;
begin
  SelNode := tvMain.Selected;
  if SelNode = nil then Exit;

  NodeInfos := SelNode.Data;
  dbIndex := NodeInfos^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  // Routine-Info vorbereiten
  FRoutineInfo.RoutineType := ARoutineType;
  FRoutineInfo.RoutineName := GetClearNodeText(SelNode.Text);
  FRoutineInfo.Connection  := Rec.IBConnection;
  FRoutineInfo.dbIndex     := dbIndex;

  if ARoutineType in [rtPackageFBFunc, rtPackageFBProc, rtPackageUDRFunc, rtPackageUDRProc, rtPackageUDF] then
  begin
    FRoutineInfo.PackageName := SelNode.Parent.Parent.Text;
  end
  else
  begin
    FRoutineInfo.PackageName := '';
  end;

  DBAlias := GetAncestorNodeText(SelNode, 1);
  // Prüfen, ob Editor schon existiert
  if Assigned(NodeInfos^.ExecuteForm) then
  begin
    frmTestFunction := TfrmTestFunction(NodeInfos^.ExecuteForm);
    ATab:= frmTestFunction.Parent as TTabSheet;
    //ATab := TTabSheet(frmTestFunction.Parent);
  end
  else
  begin
    frmTestFunction := TfrmTestFunction.CreateForRoutine(Application, FRoutineInfo);

    ATab := TTabSheet.Create(Self);
    ATab.PageControl := PageControl1;
    ATab.ControlStyle := ATab.ControlStyle + [csAcceptsControls];

    ATab.Tag := dbIndex;

    frmTestFunction.Parent := ATab;
    frmTestFunction.Align := alClient;
    frmTestFunction.BorderStyle := bsNone;
    //frmTestFunction.Show;

    NodeInfos^.ExecuteForm := frmTestFunction;
  end;

  // Kurzer Tab-Titel = nur RoutineName
  ShortTitle := FRoutineInfo.RoutineName;
  ATab.Caption := ShortTitle;
  ATab.ImageIndex := SelNode.ImageIndex;

  // Mehrzeiliger Tooltip
    if FRoutineInfo.PackageName <> '' then
    begin
      FullHint :=
        'Server:   ' + GetAncestorNodeText(SelNode, 0) +   sLineBreak +
        //'Server:   ' + GetServerName(Rec.IBConnection.DatabaseName) +   sLineBreak +
        'DBAlias:   ' + DBAlias + sLineBreak +
        //'DBAlias:  ' + Rec.AliasName  + sLineBreak +
        'DBPath:   ' + Rec.IBConnection.DatabaseName + sLineBreak +
        'Package:  ' + FRoutineInfo.PackageName + sLineBreak +
        'Object type:   ' + RoutineTypeToStr(ARoutineType) + sLineBreak +
        'Object name:     ' + FRoutineInfo.RoutineName;
    end
    else
    begin
      FullHint :=
        'Server:   ' + GetAncestorNodeText(SelNode, 0) +   sLineBreak +
        'DBAlias:  ' + DBAlias + sLineBreak +
        'DBPath:   ' + Rec.IBConnection.DatabaseName + sLineBreak +
        'Object type:   ' + RoutineTypeToStr(ARoutineType) + sLineBreak +
        'Object name:     ' + FRoutineInfo.RoutineName;
    end;

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Tab aktivieren + Routine initialisieren
  PageControl1.ActivePage := ATab;
  frmTestFunction.Init(FRoutineInfo, NodeInfos);
  frmTestFunction.Show;
end;

procedure TfmMain.lmCallStoreProcClick(Sender: TObject);
begin
  CallRoutine(rtFBProc);
end;

(****************  Connect As  *****************)
procedure TfmMain.lmConnectAsClick(Sender: TObject);
begin
  if ConnectToDBAs(TPNodeInfos(tvMain.Selected.Data)^.dbIndex, True) then
    tvMain.Selected.Expand(False)
  else
    tvMain.Selected.Collapse(False);
end;

Function TfmMain.ConnectToDBAs(dbIndex: Integer; ForceConnectDialog: boolean=false): Boolean;
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
      Rec.Charset) then
    begin
      RegisteredDatabases[dbIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
      RegisteredDatabases[dbIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
      RegisteredDatabases[dbIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
      Result:= True;
    end;
  end;
end;

Function TfmMain.IsLinux: Boolean;
begin
  Result := Target = 'Linux';
end;

Function TfmMain.IsWindows: Boolean;
begin
  Result := Target = 'Win';
end;

Function TfmMain.IsUnix: Boolean;
begin
end;

Function TfmMain.Is64bit: Boolean;
begin
  result := Arch = '64';
end;

Function TfmMain.Is32bit: Boolean;
begin
  result := Arch = '32';
end;

(****************  Fill and show constraints form ************************)
procedure TfmMain.FillAndShowConstraintsForm(Form: TfmTableManage; ATableName: string; dbIndex: Integer);
begin
  dmSysTables.Init(dbIndex);
  dmSysTables.GetTableConstraints(ATableName, Form.SQLQuery1);
  Form.FillConstraints(dbIndex);
end;


(***********************  Constraint Management  ********************)

(**********  Create Auto Increment Trigger from current generator  **********)
procedure TfmMain.lmCreateAutoIncClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    InitNewGen(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex);
    fmNewGen.edGenName.Text:= SelNode.Text;
    fmNewGen.edGenName.Enabled:= False;
    fmNewGen.cxTrigger.Checked:= True;
    fmNewGen.ShowModal;
  end;
end;

(****************  Create new stored proc  *******************)
procedure TfmMain.lmCreateStoredProcClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AProcName: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new stored procedure', 'Please enter new procedure name', AProcName) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new stored procedure');
    QWindow.meQuery.Lines.Clear;
 //   QWindow.meQuery.Lines.Add('SET TERM ^;');
    QWindow.meQuery.Lines.Add('CREATE PROCEDURE ' + AProcName);
    QWindow.meQuery.Lines.Add('-- Input parameters, you can modify,remove them');

    QWindow.meQuery.Lines.Add('( Input1 int, -- You can replace it by your first parameter');
    QWindow.meQuery.Lines.Add(' Input2 varchar(20) -- you can replace it by your second parameter');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('RETURNS');
    QWindow.meQuery.Lines.Add('( Out1 int -- You can replace it by your first parameter');
    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('  -- Write your procedure code here');
    QWindow.meQuery.Lines.Add('END;');
   // QWindow.meQuery.Lines.Add('SET TERM ;^');
    QWindow.Show;
  end;
end;

(***************  Create new Trigger  ****************)
Function TfmMain.CreateNewTrigger(dbIndex: Integer; ATableName: string; OnCommitProcedure: TNotifyEvent = nil): Boolean;
var
  QWindow: TfmQueryWindow;
  TrigType: string;
begin
  Result:= False;
  if ATableName <> '' then
  begin
    fmCreateTrigger.cbTables.Clear;
    fmCreateTrigger.cbTables.Items.Add(ATableName);
    fmCreateTrigger.cbTables.ItemIndex:= 0;
  end;
  fmCreateTrigger.edTriggerName.Clear;
  fmCreateTrigger.cxUpdate.Checked:= False;
  fmCreateTrigger.cxInsert.Checked:= False;
  fmCreateTrigger.cxDelete.Checked:= False;

  if fmCreateTrigger.ShowModal = mrOK then
  begin
    Result:= True;
    QWindow:= ShowQueryWindow(dbIndex, 'Create new Trigger');
    if fmCreateTrigger.rbAfter.Checked then
      TrigType:= 'After'
    else
      TrigType:= 'Before';
    if fmCreateTrigger.cxInsert.Checked then
      TrigType:= TrigType + ' insert or';
    if fmCreateTrigger.cxUpdate.Checked then
      TrigType:= TrigType + ' update or';
    if fmCreateTrigger.cxDelete.Checked then
      TrigType:= TrigType + ' delete or';
    Delete(TrigType, Length(TrigType) - 2, 3);

    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE TRIGGER ' + fmCreateTrigger.edTriggerName.Text + ' for ' +
      fmCreateTrigger.cbTables.Text);
    QWindow.meQuery.Lines.Add('Active');
    QWindow.meQuery.Lines.Add(TrigType);
    QWindow.meQuery.Lines.Add('Position 0');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add(' -- Your code here');
    QWindow.meQuery.Lines.Add(' -- New.FieldName = YourData;');
    QWindow.meQuery.Lines.Add('END;');
    fmMain.Show;

    if OnCommitProcedure <> nil then
      QWindow.OnCommit:= OnCommitProcedure;
  end;
end;

(*******  Create Trigger click  ********)
procedure TfmMain.lmCreateTriggerClick(Sender: TObject);
var
  SelNode: TTreeNode;
  DBIndex: Integer;
  TableNames: string;
  Count: Integer;
begin
  SelNode:= tvMain.Selected;
  DBIndex:= TPNodeInfos(SelNode.Parent.Data)^.dbIndex;

  TableNames:= dmSysTables.GetDBObjectNames(DBIndex, otTables, Count);
  fmCreateTrigger.cbTables.Items.CommaText:= TableNames;
  CreateNewTrigger(DBIndex, '');
end;

(******************  Create New View   ***************)
procedure TfmMain.lmCreateViewClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AViewName: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new view', 'Please enter new view name', AViewName) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new view');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE VIEW "' + AViewName + '" (');
    QWindow.meQuery.Lines.Add('Field1Name, Field2Name) ');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('SELECT Field1, Field2 FROM ATableName');
    QWindow.meQuery.Lines.Add('-- WHERE condition');
    QWindow.Show;
  end;
end;

procedure TfmMain.lmDeactiveTrigClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if ChangeTriggerActivity(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, SelNode.Text, False) then
    MessageDlg('Trigger has been DeActivated', mtInformation, [mbOk], 0);
end;

(***************  Display view top 1000 records  ************)
procedure TfmMain.lmDisplay1000VClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, SelNode.Text, SelNode.Data);
    QWindow.meQuery.Lines.Text:= 'select * from ' + SelNode.Text;
    QWindow.bbRunClick(nil);
    QWindow.Show;
  end;
end;

(**********  Drop Exception ********)
procedure TfmMain.lmDropExceptionClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetExceptionDeps(Rec.IBConnection, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP Exception ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Exception ' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The Exception "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the Exception anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP Exception ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Exception ' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

(***************  Edit stored procedure  *****************)
procedure TfmMain.lmEditProcClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TSQLQuery;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetFirebirdProcedureDeclaration(Rec.IBConnection, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit Stored Procedure ' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

{procedure TfmMain.lmEditProcClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AProcName: string;
  SPOwner: string;
  spBody: string;
  QWindow: TfmQueryWindow;
  DBIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AProcName:= SelNode.Text;
    DBIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    SPBody:= GetStoredProcBody(DBIndex, AProcName, SPOwner);

    // Procedure body
    QWindow:= ShowQueryWindow(DBIndex, 'Edit stored procedure ' + AProcName);
    QWindow.meQuery.Lines.Clear;
  //  QWindow.meQuery.Lines.Add('SET TERM ^ ;');
    QWindow.meQuery.Lines.Add('ALTER PROCEDURE ' + AProcName);
    QWindow.meQuery.Text:= QWindow.meQuery.Text + Trim(spBody) + ';';
   // QWindow.meQuery.Lines.Add('SET TERM ; ^');

    QWindow.Show;
  end;
end;
}

(**********************  Edit Table data  ***************************)
procedure TfmMain.lmEditTableDataClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  EditWindow: TfmEditTable;
  ATableName: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    Rec:= RegisteredDatabases[dbIndex];
    EditWindow:= TfmEditTable(FindCustomForm(Rec.RegRec.Title + ': Edit Data for Table : ' + ATableName, TfmEditTable));
    if EditWindow = nil then
    begin
      EditWindow:= TfmEditTable.Create(Application);
      EditWindow.Rec:= Rec;
      EditWindow.Caption:= EditWindow.Rec.RegRec.Title + ': Edit Data for Table : ' + ATableName;
      EditWindow.Init(dbIndex, ATableName);
    end;
    EditWindow.ShowModal;
  end;
end;

(****************  Edit Trigger  ******************)
procedure TfmMain.lmEditTriggerClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATriggerName: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATriggerName:= SelNode.Text;
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, 'Edit Trigger ' + ATriggerName);

    QWindow.meQuery.Lines.Clear;
    dmSysTables.ScriptTrigger(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, ATriggerName, QWindow.meQuery.Lines);
    QWindow.Show;
  end;
end;

(********************  Edit View  ********************)
procedure TfmMain.lmEditViewClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AViewName: string;
  ViewBody, Columns: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AViewName:= SelNode.Text;
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, 'Edit view ' + AViewName);

    GetViewInfo(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, AViewName, Columns, ViewBody);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('DROP VIEW "' + AViewName + '";');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('CREATE VIEW "' + AViewName + '" (' + Columns + ')');
    QWindow.meQuery.Lines.Add('AS');

    QWindow.meQuery.Text:= QWindow.meQuery.Text + ViewBody;
    QWindow.Show;
  end;
end;

(***********  Show Indices Management  **************)
procedure TfmMain.ShowIndicesManagement(AForm: TForm; DatabaseIndex: Integer; ATableName: string);
var
  AQuery: TSQLQuery;
  i: Integer;
  IndexFields: string;
  Rec: TDatabaseRec;
  CurrentRow: Integer;
  FieldsList: TStringList;
  ConstraintName: string;
  Form: TfmTableManage;
begin
  Form:= AForm as TfmTableManage;
  Rec:= RegisteredDatabases[DatabaseIndex];
  AQuery:= TSQLQuery.Create(nil);
  try
    AQuery.Close;

    if FIBConnection <> RegisteredDatabases[DatabaseIndex].IBConnection then
    begin
      FIBConnection:= RegisteredDatabases[DatabaseIndex].IBConnection;
      FSQLTransaction:= RegisteredDatabases[DatabaseIndex].SQLTrans;
    end;
    AQuery.DataBase:= FIBConnection;
    FSQLTransaction.Commit;

    Form.sgIndices.RowCount:= 1;

    // Get primary key index name
    Form.PKeyName:= GetPrimaryKeyIndexName(DatabaseIndex, ATableName, ConstraintName);
    Form.ConstraintName:= ConstraintName;

    // Index names
    if GetIndices(ATableName, AQuery) then
    with Form do
    while not AQuery.EOF do
    begin
      if Trim(AQuery.FieldByName('RDB$Index_name').AsString) = PKeyName then
      begin
        sgIndices.InsertColRow(False, 1);
        CurrentRow:= 1;
      end
      else
      begin
        sgIndices.RowCount:= sgIndices.RowCount + 1;
        CurrentRow:= sgIndices.RowCount - 1;
      end;
      sgIndices.Cells[0, CurrentRow]:= Trim(AQuery.FieldByName('RDB$Index_Name').AsString);
      if AQuery.FieldByName('RDB$Unique_Flag').AsString = '1' then
        sgIndices.Cells[1, CurrentRow]:= '1'
      else
        sgIndices.Cells[1, CurrentRow]:= '0';

      if AQuery.FieldByName('RDB$Index_Type').AsString = '1' then
        sgIndices.Cells[2, CurrentRow]:= 'Desc'
      else
        sgIndices.Cells[2, CurrentRow]:= 'Asc';

      if Trim(AQuery.FieldByName('RDB$Index_Name').AsString) = Form.PKeyName then
        sgIndices.Cells[4, CurrentRow]:= '1'
      else
        sgIndices.Cells[4, CurrentRow]:= '0';
      AQuery.Next;
    end;

    FieldsList:= TStringList.Create;
    try
      // Index fields
      with Form do
      for i:= 1 to sgIndices.RowCount - 1 do
      begin
        IndexFields:= '';
        if GetIndexFields(ATableName, sgIndices.Cells[0, i], AQuery, FieldsList) then
        begin
          IndexFields:= FieldsList.CommaText;
          sgIndices.Cells[3, i]:= IndexFields;
        end;
      end;
    finally
      FieldsList.Free;
    end;

    Form.edIndexName.Text:= 'IX_' + ATableName + '_' + IntToStr(Form.sgIndices.RowCount);

    // Field names
    GetFields(DatabaseIndex, ATableName, nil);
    with Form, Self.SQLQuery1 do
    begin
      clbFields.Clear;
      while not EOF do
      begin
        // Allow creating indexes on any field except blobs
        if (FieldByName('field_type_int').AsInteger <> BlobType) then
          clbFields.Items.Add(FieldByName('Field_Name').AsString);
        Next;
      end;
      Self.SQLQuery1.Close;
    end;
    AQuery.Close;
  finally
    AQuery.Free;
  end;

  if Form.sgIndices.RowCount > 1 then
    Form.sgIndices.Row:= 1;
end;

(**************  New Domain  *************)
procedure TfmMain.lmNewDomainClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  Line: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if fmNewDomain.ShowModal = mrOk then
  with QWindow do
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new domain');
    meQuery.Lines.Clear;
    Line:= 'CREATE DOMAIN ' + fmNewDomain.edName.Text + ' AS ' + fmNewDomain.cbType.Text;
    if Pos('char', LowerCase(fmNewDomain.cbType.Text)) > 0 then
      Line:= Line + '(' + IntToStr(fmNewDomain.seSize.Value) + ')';
    meQuery.Lines.Add(Line);

    if Trim(fmNewDomain.edDefault.Text) <> '' then
    begin
      if (Pos('char', LowerCase(fmNewDomain.cbType.Text)) > 0) or
        (LowerCase(fmNewDomain.cbType.Text)='cstring') then
        meQuery.Lines.Add('DEFAULT ' + QuotedStr(fmNewDomain.edDefault.Text))
      else
        meQuery.Lines.Add('DEFAULT ' + fmNewDomain.edDefault.Text);
    end;
    Show;
  end;
end;

(***********  Add New exception  ****************)
procedure TfmMain.lmNewExceptionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new Exception');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE EXCEPTION Exception_name_1 ''exception message'';');
    QWindow.Show;
  end;
end;

(**************  Initialize New Generator form  *************)
procedure TfmMain.InitNewGen(DatabaseIndex: Integer);
var
  Rec: TDatabaseRec;
begin
  Rec:= RegisteredDatabases[DatabaseIndex];
  fmNewGen.Init(DatabaseIndex);
end;

(*  Get server name from database string  *)
Function TfmMain.GetServerName(DBName: string): string;
begin
  if Pos(':', DBName) > 2 then
    Result:= Copy(DBName, 1, Pos(':', DBName) - 1)
  else
    Result:= 'localhost';
end;

(* Search and get server node in tree view *)
Function TfmMain.GetServerNameNode(ServerName: string): TTreeNode;
var
  Node: TTreeNode;
begin
  Node:= nil;
  ServerName:= LowerCase(ServerName);
  if tvMain.Items.Count > 0 then
    Node:= tvMain.Items[0];
  Result:= nil;
  while Node <> nil do
  begin
    if (Node.Text <> '') and (LowerCase(Node.Text) = ServerName) then
    begin
      Result:= Node;
      Break;
    end;
    Node:= Node.GetNextSibling;
  end;
end;

procedure TfmMain.SetConnection(Index: Integer);
begin
  if FIBConnection <> RegisteredDatabases[Index].IBConnection then
  begin
    FIBConnection:= RegisteredDatabases[Index].IBConnection;
    // This used to say FIBConnection.Close which will simply also close all open
    // queries - not a good idea
    //FIBConnection.Close;
    FSQLTransaction:= RegisteredDatabases[Index].SQLTrans;
    FIBConnection.Transaction:= FSQLTransaction;
    SQLQuery1.DataBase:= FIBConnection;
    SQLQuery1.Transaction:= FSQLTransaction;
  end;
end;

procedure TfmMain.SetFocus;
begin
  if not FActivated then
    inherited SetFocus;
end;

procedure TfmMain.GetLogEvent(Sender: TSQLConnection; EventType: TDBEventType;
  const Msg: String);
// Used to log everything sent through the connection
var
  Source: string;
begin
  case EventType of
    detCustom:   Source:='Custom:   ';
    detPrepare:  Source:='Prepare:  ';
    detExecute:  Source:='Execute:  ';
    detFetch:    Source:='Fetch:    ';
    detCommit:   Source:='Commit:   ';
    detRollBack: Source:='Rollback: ';
    else Source:='Unknown event. Please fix program code.';
  end;
  try
    SendDebug(Source + Msg);
  except
    // Ignore errors (e.g. debug server not active)
  end;
end;

(* Insert SQL query into database history file *)
Function TfmMain.AddToSQLHistory(DatabaseTitle: string; SQLType, SQLStatement: string): Boolean;
begin
  try
    Result:= OpenSQLHistory(DatabaseTitle);
    if Result then
    begin
      mdsHistory.Last;
      //if (SQLType <> 'SELECT') or (mdsHistory.FieldByName('SQLStatement').AsString <> SQLStatement) then
      if not mdsHistory.Locate('SQLStatement', SQLStatement, []) then
      begin
        mdsHistory.AppendRecord([Now, SQLType, SQLStatement, 0]);
        if SQLType = 'DDL' then
          mdsHistory.SaveToFile(FCurrentHistoryFile);
      end;
    end;
  except
    on E: Exception do
    begin
      Result:= False;
    end;
  end;
end;

Function TfmMain.SaveAndCloseSQLHistory: Boolean;
begin
  try
    if mdsHistory.Active then
      mdsHistory.SaveToFile(FCurrentHistoryFile);
    mdsHistory.Close;
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
      ShowMessage(e.Message)
    end;
  end;
end;

(* Open SQL history file for current database *)

Function TfmMain.OpenSQLHistory(DatabaseTitle: string): Boolean;
var
  AFileName: string;
  i: Integer;

  // Removes spaces, braces, brackets etc
  Function RemoveSpecialChars(AText: string): string;
  var
    i: Integer;
  begin
    for i:= Length(AText) to 1 do
      if Pos(AText[i], ' !@#$%^&*()[]{}/?<>:;"|\,.~`''') > 0 then
        System.Delete(AText, i, 1);
    Result:= AText;
  end;

begin
  try
    AFileName:= getConfigurationDirectory + LowerCase(RemoveSpecialChars(DatabaseTitle)) + '.history';

    // Different opened history file
    if mdsHistory.Active and (AFileName <> FCurrentHistoryFile) then
    begin
      if FCurrentHistoryFile <> '' then
        mdsHistory.SaveToFile(FCurrentHistoryFile);
       mdsHistory.Close;
    end;

    if not mdsHistory.Active then
    if FileExists(AFileName) then
    begin
      try
        mdsHistory.LoadFromFile(AFileName);
      except
        on E: Exception do
          mdsHistory.SaveToFile(AFileName);
      end;
    end
    else
      mdsHistory.CreateTable;

    if not mdsHistory.Active then
      mdsHistory.Open;

    if mdsHistory.RecNo > 10000 then
    begin
      mdsHistory.First;
      for i:= 1 to 2 do
        mdsHistory.Delete;
    end;
    FCurrentHistoryFile:= AFileName;
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
      ShowMessage(e.Message);
    end;
  end;
end;

(* Get input parameters from stored procedure body *)
Function TfmMain.RetrieveInputParamFromSP(Body: string): string;
var
  i: Integer;
  SizeStarted: Boolean;
begin
  SizeStarted:= False;
  if (Pos('(', Body) > 0) and (Pos('(', Body) < Pos(')', Body)) then
  for i:= 1 to Length(Body) do
  begin
    if (Body[i] = ')') and (not SizeStarted) then
    begin
      Result:= Trim(Copy(Body, 1, i - 1));
      Break;
    end;

    if (Body[i] = ')') and (SizeStarted) then
      SizeStarted:= False;

    if Body[i] = '(' then
      SizeStarted:= True;

  end
  else
    Result:= Trim(Copy(Body, 1, Pos(')', Body) - 1));
end;

(**************  New Generator  *******************)
procedure TfmMain.lmNewGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    InitNewGen(TPNodeInfos(SelNode.Parent.Data)^.dbIndex);
    fmNewGen.edGenName.Clear;
    fmNewGen.edGenName.Enabled:= True;
    fmNewGen.cxTrigger.Checked:= False;
    fmNewGen.ShowModal;
  end;
end;

(************  Add New Table   ******************)
{procedure TfmMain.lmNewTableClick(Sender: TObject);
var
  Rec: TDatabaseRec;
  SelNode: TTreeNode;
  dbIndex: Integer;
  Form: TfmNewTable;
  Title: string;
  ATab: TTabSheet;
begin
  SelNode:= tvMain.Selected;
  dbIndex:= TPNodeInfos(SelNode.Parent.Data)^.dbIndex;
  Rec:= RegisteredDatabases[dbIndex];

  Title:= SelNode.Parent.Text + ': New Table';

  Form:= FindCustomForm(Title, TfmNewTable) as TfmNewTable;
  if Form = nil then
  begin
    Form:= TfmNewTable.Create(Application);
    ATab:= TTabSheet.Create(self);
    ATab.Parent:= PageControl1;
    Form.Parent:= ATab;
    Form.Caption:= Title;
    ATab.Caption:= Form.Caption;
    Form.Left:= 0;
    Form.Top:= 0;
    Form.BorderStyle:= bsNone;
    Form.Align:= alClient;
    Form.Init(dbIndex);
  end
  else
    ATab:= Form.Parent as TTabSheet;
  PageControl1.ActivePage:= ATab;
  Form.Show;
  Form.edNewTable.SetFocus;

  ATab.Tag:= dbIndex;
  PageControl1.ActivePage:= ATab;
end;}

procedure TfmMain.lmNewTableClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  Form: TfmNewTable;
  ATab: TTabSheet;
  ShortTitle, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if SelNode = nil then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  DBAlias := GetAncestorNodeText(SelNode, 1);

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.NewForm) and (NodeInfos^.NewForm is TfmNewTable) then
    Form := TfmNewTable(NodeInfos^.NewForm)
  else
  begin
    Form := TfmNewTable.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;

    Form.Parent := ATab;
    Form.Align := alClient;
    Form.BorderStyle := bsNone;

    NodeInfos^.NewForm := Form;
    Form.Init(dbIndex, SelNode.Data);
  end;

  // Tab vorbereiten
  ATab := Form.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Titel
  ShortTitle := 'New Table';
  ATab.Caption := ShortTitle;
  Form.Caption := ShortTitle;

  // Tooltip
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBConnection.DatabaseName + sLineBreak +
    'Action:   Create new table';
  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Anzeigen und Fokus setzen
  Form.Show;
  Form.edNewTable.SetFocus;
end;

(*************  Create new UDF  ******************)
procedure TfmMain.lmNewUDFClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  AFuncName: string;
  ModuleName, EntryPoint: string;
begin
  SelNode:= tvMain.Selected;
  ModuleName:= '<modulename>';
  EntryPoint:= '<entryname>';
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new UDF-Function', 'Please enter new Function name', AFuncName) then
  if InputQuery('Create new UDF-Function', 'Please enter module name (Library)', ModuleName) then
  if InputQuery('Create new UDF-Function', 'Please enter entry point (External Function name)', EntryPoint) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new UDF');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('DECLARE EXTERNAL Function "' + AFuncName + '"');
    QWindow.meQuery.Lines.Add('-- (int, varchar(100))');
    QWindow.meQuery.Lines.Add('RETURNS (int)');
    QWindow.meQuery.Lines.Add('ENTRY_POINT ' + QuotedStr(entryPoint));
    QWindow.meQuery.Lines.Add('MODULE_NAME ' + QuotedStr(modulename) + ';');
    QWindow.Show;
  end;
end;

(**********  Open Query 2 Click ************)


(**********  Open Query Window from Database  *************)
procedure TfmMain.lmOpenQueryClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  Rec: TRegisteredDatabase;
  Count: Integer;
  dbIndex: Integer;
begin
  dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  Rec:= RegisteredDatabases[dbIndex].RegRec;
  // Password form
  if (Rec.Password = '') and (not tvMain.Selected.Expanded) then
  begin
    fmEnterPass.edPassword.Clear;
    try
      fmEnterPass.cbRole.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, otRoles, Count);
    except
    end;
    if fmEnterPass.ShowModal = mrOk then
    begin
      if fmReg.TestConnection(Rec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
        Rec.Charset) then
          RegisteredDatabases[dbIndex].RegRec.Password:= fmEnterPass.edPassword.Text
        else
          Exit;
    end;
  end;
  QWindow:= ShowQueryWindow(dbIndex, 'Query Window');
  QWindow.Show;
end;

{procedure TfmMain.lmPermissionsClick(Sender: TObject);
var
  dbIndex: Integer;
  Form: TfmUserPermissions;
  UserName: string;
  ATab: TTabSheet;
  Title: string;
  ds: TDataSet;
  ObjName, ObjTypeName, PrivStr, Priv: string;
  CurrentObj: string;
  ObjType: Integer;
  RowIndex: Integer;
begin
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  UserName := tvMain.Selected.Text;
  Title := 'Permissions for: ' + UserName;

  Form := FindCustomForm(Title, TfmUserPermissions) as TfmUserPermissions;
  if Form = nil then
  begin
    Form := TfmUserPermissions.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    Form.Parent := ATab;
    Form.Caption := Title;
    ATab.Caption := Form.Caption;
    Form.Left := 0;
    Form.Top := 0;
    Form.BorderStyle := bsNone;
    Form.Align := alClient;
  end
  else
    ATab := Form.Parent as TTabSheet;

  ATab.Tag := dbIndex;
  PageControl1.ActivePage := ATab;

  Form.laObject.Caption := UserName;
  Form.StringGrid1.RowCount := 1;
  RowIndex := 1;
  CurrentObj := '';
  PrivStr := '';

  ds := dmSysTables.GetAllUserPermissions(dbIndex, UserName);
  try
    while not ds.EOF do
    begin
      ObjName := Trim(ds.FieldByName('RDB$RELATION_NAME').AsString);
      Priv := Trim(ds.FieldByName('RDB$PRIVILEGE').AsString);
      if ds.FieldByName('RDB$GRANT_OPTION').AsInteger <> 0 then
        Priv := Priv + 'G';

      if (ObjName <> CurrentObj) and (CurrentObj <> '') then
      begin
        // Neue Zeile ins Grid
        Form.StringGrid1.RowCount := RowIndex + 1;
        Form.StringGrid1.Cells[0, RowIndex] := ObjTypeName;
        Form.StringGrid1.Cells[1, RowIndex] := CurrentObj;
        Form.StringGrid1.Cells[2, RowIndex] := PrivStr;
        Inc(RowIndex);
        PrivStr := '';
      end;

      CurrentObj := ObjName;
      ObjType := ds.FieldByName('RDB$OBJECT_TYPE').AsInteger;

      case ObjType of
        0: ObjTypeName := 'Table/View';
        5: ObjTypeName := 'Procedure';
        13: ObjTypeName := 'Role';
      else
        ObjTypeName := IntToStr(ObjType);
      end;

      if PrivStr <> '' then
        PrivStr := PrivStr + ',';
      PrivStr := PrivStr + Priv;
      ds.Next;
    end;

    // Letzte Zeile hinzufügen
    if CurrentObj <> '' then
    begin
      Form.StringGrid1.RowCount := RowIndex + 1;
      Form.StringGrid1.Cells[0, RowIndex] := ObjTypeName;
      Form.StringGrid1.Cells[1, RowIndex] := CurrentObj;
      Form.StringGrid1.Cells[2, RowIndex] := PrivStr;
    end;
  finally
    // Dataset wird vom Modul verwaltet, kein Free nötig
  end;
  Form.Show;
end;}

procedure TfmMain.lmPermissionsClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  UserName: string;
  frmUserPermissions: TfmUserPermissions;
  ATab: TTabSheet;
  ShortTitle, FullHint, DBAlias: string;
  ds: TDataSet;
  ObjName, ObjTypeName, PrivStr, Priv, CurrentObj: string;
  ObjType: Integer;
  RowIndex: Integer;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) or (SelNode.Parent.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  UserName := SelNode.Text;

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmUserPermissions) then
    frmUserPermissions := TfmUserPermissions(NodeInfos^.ViewForm)
  else
  begin
    frmUserPermissions := TfmUserPermissions.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmUserPermissions.Parent := ATab;
    frmUserPermissions.Align := alClient;
    frmUserPermissions.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmUserPermissions;
  end;

  // Tab vorbereiten
  ATab := frmUserPermissions.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Titel
  ShortTitle := UserName;
  ATab.Caption := ShortTitle;
  frmUserPermissions.Caption := ShortTitle;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBConnection.DatabaseName + sLineBreak +
    //'Object type: Permissions' + sLineBreak +
    'Object type: User/Role' + sLineBreak +
    'User/Role: ' + UserName;
  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Grid zurücksetzen
  frmUserPermissions.laObject.Caption := UserName;
  frmUserPermissions.StringGrid1.RowCount := 1;
  RowIndex := 1;
  CurrentObj := '';
  PrivStr := '';

  // Daten auslesen
  ds := dmSysTables.GetAllUserPermissions(dbIndex, UserName);
  try
    while not ds.EOF do
    begin
      ObjName := Trim(ds.FieldByName('RDB$RELATION_NAME').AsString);
      Priv := Trim(ds.FieldByName('RDB$PRIVILEGE').AsString);
      if ds.FieldByName('RDB$GRANT_OPTION').AsInteger <> 0 then
        Priv := Priv + 'G';

      if (ObjName <> CurrentObj) and (CurrentObj <> '') then
      begin
        frmUserPermissions.StringGrid1.RowCount := RowIndex + 1;
        frmUserPermissions.StringGrid1.Cells[0, RowIndex] := ObjTypeName;
        frmUserPermissions.StringGrid1.Cells[1, RowIndex] := CurrentObj;
        frmUserPermissions.StringGrid1.Cells[2, RowIndex] := PrivStr;
        Inc(RowIndex);
        PrivStr := '';
      end;

      CurrentObj := ObjName;
      ObjType := ds.FieldByName('RDB$OBJECT_TYPE').AsInteger;

      case ObjType of
        0: ObjTypeName := 'Table/View';
        5: ObjTypeName := 'Procedure';
        13: ObjTypeName := 'Role';
      else
        ObjTypeName := IntToStr(ObjType);
      end;

      if PrivStr <> '' then
        PrivStr := PrivStr + ',';
      PrivStr := PrivStr + Priv;

      ds.Next;
    end;

    // letzte Zeile
    if CurrentObj <> '' then
    begin
      frmUserPermissions.StringGrid1.RowCount := RowIndex + 1;
      frmUserPermissions.StringGrid1.Cells[0, RowIndex] := ObjTypeName;
      frmUserPermissions.StringGrid1.Cells[1, RowIndex] := CurrentObj;
      frmUserPermissions.StringGrid1.Cells[2, RowIndex] := PrivStr;
    end;
  finally
    // Dataset wird vom Modul verwaltet, kein Free nötig
  end;

  frmUserPermissions.Init(SelNode.Data);
  frmUserPermissions.Show;
end;

(***********  Refresh Click  *************)
procedure TfmMain.lmRefreshClick(Sender: TObject);
begin
  if tvMain.Selected.Expanded then
    tvMain.Selected.Collapse(False);
  tvMainExpanded(nil, tvMain.Selected)
end;

procedure TfmMain.lmRegdbClick(Sender: TObject);
begin
  fmReg.edDatabaseName.Text:= tvMain.Selected.Text + ':';
  mnRegDBClick(nil);
end;

procedure TfmMain.lmRestoreClick(Sender: TObject);
begin
  fmBackupRestore.Init('', tvMain.Selected.Text +  ':', '', '');
  fmBackupRestore.cbOperation.ItemIndex:= 1;
  fmBackupRestore.cbOperation.Enabled:= False;
  fmBackupRestore.meLog.Clear;
  fmBackupRestore.Show;
end;

procedure TfmMain.lmRolePerManagementClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  ATab: TTabSheet;
  Title: string;
  dbIndex: Integer;
begin
  Title:= 'Permission management for: ' + tvMain.Selected.Text;
  fmPermissions:= FindCustomForm(Title, TfmPermissionManage) as TfmPermissionManage;
  if fmPermissions = nil then
  begin
    fmPermissions:= TfmPermissionManage.Create(nil);
    ATab:= TTabSheet.Create(self);
    ATab.Parent:= PageControl1;
    fmPermissions.Parent:= ATab;
    fmPermissions.Left:= 0;
    fmPermissions.Top:= 0;
    fmPermissions.BorderStyle:= bsNone;
    fmPermissions.Align:= alClient;
    ATab.Caption:= Title;
  end
  else
    ATab:= fmViewGen.Parent as TTabSheet;
  PageControl1.ActivePage:= ATab;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  ATab.Tag:= dbIndex;
  fmPermissions.Init(dbIndex, '', tvMain.Selected.Text, 2);
  fmPermissions.Show;
end;

procedure TfmMain.lmRolePermissionsClick(Sender: TObject);
begin
  lmPermissionsClick(Sender);
end;

(***********  Script Database  ************)
procedure TfmMain.lmScriptDatabaseClick(Sender: TObject);
var
  QueryWindow: TfmQueryWindow;
  List: TStringList;
  dbIndex: Integer;
begin
  dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  QueryWindow:= ShowQueryWindow(dbIndex, 'scriptDatabase#:' + IntToStr(dbIndex) + tvMain.Selected.Text);

  Screen.Cursor:= crSQLWait;
  List:= TStringList.Create;
  try //...finally for resource release
    try //...except for error reporting
      Application.ProcessMessages;
      with QueryWindow.meQuery do
      begin
        ClearAll;
        Lines.Add('-- ' + tvMain.Selected.Text + ' database script. Generated on: ' + DateTimeToStr(Now) );

        Lines.Add('');
        Lines.Add('--     Roles');
        Lines.Add('');
        ScriptAllRoles(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--     Exceptions');
        Lines.Add('');
        ScriptAllExceptions(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--     Functions (UDF)');
        Lines.Add('');
        ScriptAllFunctions(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--     Domains');
        Lines.Add('');
        ScriptAllDomains(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Generators/Sequences');
        Lines.Add('');
        ScriptAllGenerators(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Tables');
        ScriptAllTables(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Stored Procedures');
        Lines.Add('');
        ScriptAllProcedureTemplates(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('/*      Views  */');
        Lines.Add('');
        ScriptAllViews(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Triggers');
        Lines.Add('');
        ScriptAllTriggers(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Secondary Indices');
        Lines.Add('');
        ScriptAllSecIndices(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Constraints');
        Lines.Add('');
        ScriptAllConstraints(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Check constraints');
        Lines.Add('');
        ScriptAllCheckConstraints(dbIndex, List);
        Lines.AddStrings(List);

        Lines.Add('');
        Lines.Add('--      Permissions');
        Lines.Add('');
        ScriptAllPermissions(dbIndex, List);
        Lines.AddStrings(List);
        Lines.Add('');
      end;
      QueryWindow.Show;
    except
      on E: Exception do
      begin
        Screen.Cursor:= crDefault;
        ShowMessage(E.Message);
      end;
    end;
  finally
    Screen.Cursor:= crDefault;
    List.Free;
  end;
end;

(**************  Script Exception  ****************)
procedure TfmMain.lmScriptExceptionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Script, Msg, Desc: string;
begin
  SelNode:= tvMain.Selected;
  if dmSysTables.GetExceptionInfo(TPNodeInfos(tvMain.Selected.Data)^.dbIndex, SelNode.Text,
    Msg, Desc, Script, false) then
    ShowCompleteQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, 'Script Exception ' + SelNode.Text, Script, nil, SelNode.Data);
end;

(**************  Script table as Insert stored procedure ************)
procedure TfmMain.lmScriptInsertClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATableName: string;
  FieldLine: string;
  FieldNames: string;
  ParamNames: string;
  Skipped: Boolean;
  dbIndex: Integer;
  LastParam: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'asInsert#' + IntToStr(dbIndex) + ':' + ATableName);
    GetFields(dbIndex, ATableName, nil);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('create procedure InsertTo' + ATableName + ' (');

    // Params
    FieldNames:= '';
    ParamNames:= '';
    with SQLQuery1 do
    while not EOF do
    begin
      Skipped:= False;
      if (FieldByName('computed_source').AsString = '') then
      begin
        FieldNames:= FieldNames + Trim(FieldByName('Field_Name').AsString);
        ParamNames:= ParamNames + ':' + Trim(FieldByName('Field_Name').AsString);
        FieldLine:= Trim(FieldByName('Field_Name').AsString) + ' ';
        FieldLine:= FieldLine +
          GetFBTypeName(SQLQuery1.FieldByName('field_type_int').AsInteger,
            SQLQuery1.FieldByName('field_sub_type').AsInteger,
            SQLQuery1.FieldByName('field_length').AsInteger,
            SQLQuery1.FieldByName('field_precision').AsInteger,
            SQLQuery1.FieldByName('field_scale').AsInteger);
        if FieldByName('field_type_int').AsInteger in [CStringType,CharType,VarCharType] then
          FieldLine:= FieldLine + '(' + FieldByName('CharacterLength').AsString + ') ';
      end
      else
        Skipped:= True;

      Next;

      if not Skipped then
      begin
        if not EOF then
        begin
          FieldLine:= FieldLine + ',';
          FieldNames:= FieldNames + ', ';
          ParamNames:= ParamNames + ', ';
        end;
        QWindow.meQuery.Lines.Add(FieldLine);
      end;
    end;
    SQLQuery1.Close;

    // Remote last , if any
    if RightStr(FieldNames, 2) = ', ' then
    begin
      System.Delete(FieldNames, Length(FieldNames) - 1, 2);
      System.Delete(ParamNames, Length(ParamNames) - 1, 2);
    end;

    // Remove last , if any
    LastParam:= QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1];
    if Pos(',', LastParam) > 0 then
    begin
      LastParam:= StringReplace(LastParam, ',', '', []);
      QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1]:= LastParam;
    end;

    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('insert into ' + ATableName + ' (' + FieldNames + ')');
    QWindow.meQuery.Lines.Add('values (' + ParamNames + ');');
    QWindow.meQuery.Lines.Add('end;');

    QWindow.Show;
  end;
end;

(********  Script table as Create  ***********)
procedure TfmMain.lmScriptTableCreateClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATableName: string;
  dbIndex: Integer;
  ScriptList: TStringList;
  Line: string;
  PKIndexName: string;
  ConstraintName: string;
  List: TStringList;
  i: Integer;
  UserName: string;
  ObjType: Integer;
  Triggers: TStringList;
  j: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    ScriptList:= TStringList.Create;
    try
      ScriptTableAsCreate(dbIndex, ATableName, ScriptList);
      QWindow:= ShowQueryWindow(dbIndex, 'asCreate#' + IntToStr(dbIndex) + ':' + ATableName);
      QWindow.meQuery.Lines.Clear;
      QWindow.meQuery.Lines.AddStrings(ScriptList);

      // Script table constraints
      dmSysTables.sqQuery.Close;
      SQLQuery1.Close;
      dmSysTables.GetTableConstraints(ATableName, dmSysTables.sqQuery);
      with dmSysTables do
      while not sqQuery.EOF do
      begin
         Line:= 'alter table ' + ATableName + ' add constraint ' + sqQuery.Fields[0].AsString +
           ' foreign key (' + sqQuery.Fields[3].AsString + ') references ' +  sqQuery.Fields[4].AsString  +
           ' (' + dmSysTables.GetConstraintForeignKeyFields(sqQuery.Fields[5].AsString, fmMain.SQLQuery1) + ') ';
         if Trim(sqQuery.Fields[6].AsString) <> 'RESTRICT' then
           Line:= Line + ' on update ' + Trim(sqQuery.Fields[6].AsString);
         if Trim(sqQuery.Fields[7].AsString) <> 'RESTRICT' then
           Line:= Line + ' on delete ' + Trim(sqQuery.Fields[7].AsString);
         QWindow.meQuery.Lines.Add(Line + ';');
         sqQuery.Next;
      end;
      dmSysTables.sqQuery.Close;
      SQLQuery1.Close;
      QWindow.meQuery.Lines.Add('');

      // Script indices
      PKIndexName:= GetPrimaryKeyIndexName(dbIndex, ATableName, ConstraintName);
      List:= TStringList.Create;
      try
        with dmSysTables do
        if fmMain.GetIndices(ATableName, sqQuery) then
        with sqQuery do
        while not EOF do
        begin
          if PKIndexName <> Trim(FieldByName('RDB$Index_name').AsString) then
          begin
            Line:= 'create ';
            if FieldByName('RDB$Unique_Flag').AsString = '1' then
              Line:= Line + 'Unique ';
            if FieldByName('RDB$Index_Type').AsString = '1' then
              Line:= Line + 'Descending ';

            Line:= Line + 'index ' + Trim(FieldByName('RDB$Index_name').AsString) + ' on ' + ATableName;

            GetIndexFields(ATableName, Trim(FieldByName('RDB$Index_Name').AsString), fmMain.SQLQuery1, List);

            Line:= Line + ' (' + List.CommaText + ') ;';
            QWindow.meQuery.Lines.Add(Line);
          end;
          Next;
        end;

        QWindow.meQuery.Lines.Add('');
        SQLQuery1.Close;
        dmSysTables.sqQuery.Close;

        // Script triggers
        SQLQuery1.Close;
        SQLQuery1.SQL.Text:= 'SELECT RDB$Trigger_Name, RDB$Trigger_Inactive FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 ' +
          'and RDB$Relation_Name = ' + QuotedStr(aTableName);
        SQLQuery1.Open;
        Triggers:= TStringList.Create;
        try
          with SQLQuery1 do
          while not EOF do
          begin
            Triggers.Add(Trim(Fields[0].AsString));
            Next;
          end;
          SQLQuery1.Close;
          for j:= 0 to Triggers.Count - 1 do
          begin
            List.Clear;
            dmSysTables.ScriptTrigger(dbIndex, Triggers[j], List, True);

            // Search for generators
            Line:= '';
            for i:= 0 to List.Count - 1 do
              if Pos('gen_id', LowerCase(List[i])) > 0 then
              begin
                Line:= Copy(List[i], Pos('gen_id', LowerCase(List[i])), Length(List[i]));
                System.Delete(Line, 1, Pos('(', Line));
                Line:= Trim(Copy(Line, 1, Pos(', ', Line) - 1));
              end;

             // Script Generator
             if Trim(Line) <> '' then
             begin
               QWindow.meQuery.Lines.Add('Create Generator ' + Line + ';');
               QWindow.meQuery.Lines.Add('');
             end;

            QWindow.meQuery.Lines.AddStrings(List);
          end;
        finally
          Triggers.Free;
        end;

        QWindow.meQuery.Lines.Add('');

        // Script permissions
        List.CommaText:= dmSysTables.GetDBUsers(dbIndex);

        for i:= 0 to List.Count - 1 do
        begin
          if Pos('<R>', List[i]) = 1 then
            UserName:= Copy(List[i], 4, Length(List[i]) - 3)
          else
            UserName:= List[i];

          ScriptObjectPermission(dbIndex, '<T>' + ATableName, UserName, ObjType, QWindow.meQuery.Lines);
        end;
      finally
        List.Free;
      end;
    finally
      ScriptList.Free;
    end;
    QWindow.Show;
  end;
end;

(*****************  Script as Update table stored proc  ****************)
procedure TfmMain.lmScriptUpdateClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATableName: string;
  i: Integer;
  PKFieldsList: TStringList;
  FieldLine: string;
  ParamAndValue: string;
  AFieldName: string;
  WhereClause: string;
  Skipped: Boolean;
  PKIndexName: string;
  dbIndex: Integer;
  ConstraintName: string;
  LastParam: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'asUpdate#' + IntToStr(dbIndex) + ':' + ATableName);
    GetFields(dbIndex, ATableName, nil);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('create procedure Update' + ATableName + ' (');

    // Params
    ParamAndValue:= '';
    with SQLQuery1 do
    while not EOF do
    begin
      Skipped:= False;
      if (FieldByName('computed_source').AsString = '') then
      begin
        AFieldName:= Trim(SQLQuery1.FieldByName('Field_Name').AsString);
        ParamAndValue:= ParamAndValue + AFieldName + ' = :' + AFieldName;
        FieldLine:= AFieldName + ' ';
        FieldLine:= FieldLine + GetFBTypeName(SQLQuery1.FieldByName('field_type_int').AsInteger,
          SQLQuery1.FieldByName('field_sub_type').AsInteger,
          SQLQuery1.FieldByName('field_length').AsInteger,
          SQLQuery1.FieldByName('field_precision').AsInteger,
          SQLQuery1.FieldByName('field_scale').AsInteger);
        if FieldByName('field_type_int').AsInteger in [CStringType,CharType,VarCharType] then
          FieldLine:= FieldLine + '(' + FieldByName('CharacterLength').AsString + ') ';
      end
      else
        Skipped:= True;
      Next;

      if not Skipped then
      begin
        if not EOF then
        begin
          FieldLine:= FieldLine + ',';
          ParamAndValue:= ParamAndValue + ', ';
        end;
        QWindow.meQuery.Lines.Add(FieldLine);

      end;
    end;

    // Remote last , if any
    if RightStr(ParamAndValue, 2) = ', ' then
      Delete(ParamAndValue, Length(ParamAndValue) - 1, 2);
    SQLQuery1.Close;

    // Primary Keys
    WhereClause:= '';
    PKFieldsList:= TStringList.Create;
    PKIndexName:= GetPrimaryKeyIndexName(dbIndex, ATableName, ConstraintName);
    if PKIndexName <> '' then
    begin
      GetConstraintFields(ATableName, PKIndexName, PKFieldsList);
      for i:= 0 to PKFieldsList.Count - 1 do
      begin
        WhereClause:= WhereClause + PKFieldsList[i] + ' = :' + PKFieldsList[i];
        if i < PKFieldsList.Count - 1 then
          WhereClause:= WhereClause + ' and ';
      end;
    end;

    // Remove last , if any
    LastParam:= QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1];
    if Pos(',', LastParam) > 0 then
    begin
      LastParam:= StringReplace(LastParam, ',', '', []);
      QWindow.meQuery.Lines[QWindow.meQuery.Lines.Count - 1]:= LastParam;
    end;

    QWindow.meQuery.Lines.Add(')');
    QWindow.meQuery.Lines.Add('AS');
    QWindow.meQuery.Lines.Add('BEGIN');
    QWindow.meQuery.Lines.Add('update ' + ATableName);
    QWindow.meQuery.Lines.Add('set ' + ParamAndValue);
    QWindow.meQuery.Lines.Add('where ' + WhereClause + ';');
    QWindow.meQuery.Lines.Add('END;');
    QWindow.Show;
  end;
end;

procedure TfmMain.lmSetFBClientClick(Sender: TObject);
begin
   lmDisconnectAllClick(nil);
   SetFBClient(1);

   //if not SetFBClient(1) then
     //Application.Terminate;
end;

(******************  Set generator value  *********************)
procedure TfmMain.lmSetGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  AGenName: string;
  OrigValue: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    Rec:= RegisteredDatabases[dbIndex];
    SetConnection(dbIndex);

    AGenName:= SelNode.Text;
    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'select GEN_ID(' + AGenName + ', 0) from RDB$Database;';

    SQLQuery1.Open;
    OrigValue:= SQLQuery1.Fields[0].AsString;
    SQLQuery1.Close;

    ShowCompleteQueryWindow(dbIndex, 'set generator value', 'set generator ' + AGenName + ' to ' + OrigValue);
  end;
end;

(*************   Sweep Database   ***********)
procedure TfmMain.lmSweepClick(Sender: TObject);
var
  FireBirdServices: TFirebirdServices;
  dbIndex: Integer;
  AdbName: string;
  Lines: string;
  s: string;
begin
  dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  FireBirdServices:= TFirebirdServices.Create;
  Screen.Cursor:= crSQLWait;
  try
    FireBirdServices.VerboseOutput:= True;
    with FireBirdServices, RegisteredDatabases[dbIndex] do
    begin
      HostName:= GetServerName(RegRec.DatabaseName);
      AdbName:= RegRec.DatabaseName;
      if Pos(':', AdbName) > 2 then
        Delete(AdbName, 1, Pos(':', AdbName));
      DBName:= AdbName;
      UserName := RegRec.UserName;
      Password := RegRec.Password;

      try
        AttachService;
        StartSweep;
        while ServiceQuery(S) do
          Lines:= Lines + S;
        Screen.Cursor:= crDefault;
        ShowMessage('Sweep database: ' + AdbName + ' completed');
      except
        on E: Exception do
        begin
          MessageDlg('Error: ' + E.Message, mtError, [mbOK], 0);
        end;
      end;
      DetachService;
    end;
  finally
    Screen.Cursor:= crDefault;
    FireBirdServices.Free;
  end;
end;

{(*************  Table management  ****************)
procedure TfmMain.lmTableManageClick(Sender: TObject);
var
  SelNode: TTreeNode;
  dbIndex: Integer;
  fmTableManage: TfmTableManage;
  ATab: TTabSheet;
  Title: string;
begin
  try
    SelNode:= tvMain.Selected;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;

    Title:= RegisteredDatabases[dbIndex].RegRec.Title +  ': Management of : ' + SelNode.Text;
    // Fields
    fmTableManage:= FindCustomForm(Title, TfmTableManage) as TfmTableManage;
    if fmTableManage = nil then
    begin
      fmTableManage:= TfmTableManage.Create(Application);
      ATab:= TTabSheet.Create(self);
      ATab.Parent:= PageControl1;
      fmTableManage.Parent:= ATab;
      fmTableManage.Left:= 0;
      fmTableManage.Top:= 0;
      fmTableManage.Align:= alClient;
      fmTableManage.BorderStyle:= bsNone;
    end
    else
      ATab:= fmTableManage.Parent as TTabSheet;

    PageControl1.ActivePage:= ATab;
    fmTableManage.Caption:= Title;
    ATab.Caption:= Title;
    ATab.Tag:= dbIndex;
    fmTableManage.Init(dbIndex, SelNode.Text);
    fmTableManage.PageControl1.TabIndex:= 0;

    // Fields
    ViewTableFields(SelNode.Text, dbIndex, fmTableManage.sgFields);

    // Indices
    ShowIndicesManagement(fmTableManage, dbIndex, SelNode.Text);

    // Constraints
    FillAndShowConstraintsForm(fmTableManage, SelNode.Text, dbIndex);

    // Triggers
    fmTableManage.FillTriggers;

    // Permissions
    fmTableManage.FillPermissions;

    fmTableManage.bbRefreshReferencesClick(nil);
    fmTableManage.Show;
  except
    on E: Exception do
      MessageDlg('Error while opening Table Management: ' + e.Message, mtError, [mbOk], 0);
  end;
end;}

procedure TfmMain.lmTableManageClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  fmTableManage: TfmTableManage;
  ATab: TTabSheet;
  ShortTitle, FullHint, DBAlias, TableName: string;
begin
  SelNode := tvMain.Selected;
  if SelNode = nil then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  TableName := SelNode.Text;
  DBAlias := GetAncestorNodeText(SelNode, 1);

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.EditorForm) and (NodeInfos^.EditorForm is TfmTableManage) then
    fmTableManage := TfmTableManage(NodeInfos^.EditorForm)
  else
  begin
    fmTableManage := TfmTableManage.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;

    fmTableManage.Parent := ATab;
    fmTableManage.Align := alClient;
    fmTableManage.BorderStyle := bsNone;

    NodeInfos^.EditorForm := fmTableManage;
  end;

  // Tab vorbereiten
  ATab := fmTableManage.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Titel = nur Tabellenname
  ShortTitle := 'EDIT:' + TableName;
  ATab.Caption := ShortTitle;
  fmTableManage.Caption := ShortTitle;

  // Mehrzeiliger Tooltip
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBConnection.DatabaseName + sLineBreak +
    'Object type: Table' + sLineBreak +
    'Table name: ' + TableName;

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Initialisierung
  fmTableManage.Init(dbIndex, TableName, SelNode.Data);
  fmTableManage.PageControl1.TabIndex := 0;

  // Teilbereiche laden
  ViewTableFields(TableName, dbIndex, fmTableManage.sgFields);
  ShowIndicesManagement(fmTableManage, dbIndex, TableName);
  FillAndShowConstraintsForm(fmTableManage, TableName, dbIndex);
  fmTableManage.FillTriggers;
  fmTableManage.FillPermissions;
  fmTableManage.bbRefreshReferencesClick(nil);

  fmTableManage.Show;
end;


procedure TfmMain.lmTestFireBirdFunctionClick(Sender: TObject);
begin
  CallRoutine(rtFBFunc);
end;

procedure TfmMain.lmTestPackageFunctionClick(Sender: TObject);
begin
  CallRoutine(rtPackageFBFunc);
end;

procedure TfmMain.lmTestPackageProcedureClick(Sender: TObject);
begin
  CallRoutine(rtPackageFBProc);
end;

procedure TfmMain.lmTestPackageUDRProcedureClick(Sender: TObject);
begin
  CallRoutine(rtPackageUDRProc);
end;

procedure TfmMain.lmTestUDFFunctionClick(Sender: TObject);
begin
  CallRoutine(rtUDF);
end;

procedure TfmMain.lmTestUDRFunctionClick(Sender: TObject);
begin
  CallRoutine(rtUDRFunc);
end;

procedure TfmMain.lmTestPackageUDRFunctionClick(Sender: TObject);
begin
  CallRoutine(rtPackageUDRFunc);
end;

procedure TfmMain.lmTestUDRProcedureClick(Sender: TObject);
begin
  CallRoutine(rtUDRProc);
end;

procedure TfmMain.lmUserPermManagementClick(Sender: TObject);
begin
  lmRolePerManagementClick(nil);
end;

(**********  View Domain info ************)
{procedure TfmMain.lmViewDomainClick(Sender: TObject);
var
  SelNode: TTreeNode;
  ADomainName: string;
  CheckConstraint: string;
  CharacterSet: string;
  Collation: string;
  DomainType: string;
  DomainSize: Integer;
  ADomainForm: TFmViewDomain;
  DefaultValue: string;
  ATab: TTabSheet;
  dbIndex: Integer;
  Title: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ADomainName:= SelNode.Text;
    Title:= SelNode.Parent.Parent.Text + ': Domain: ' + ADomainName;
    ADomainForm:= TfmViewDomain(FindCustomForm(Title, TfmViewDomain));
    if ADomainForm  = nil then
    begin
      ADomainForm:= TfmViewDomain.Create(Application);
      ATab:= TTabSheet.Create(self);
      ATab.Parent:= PageControl1;
      ADomainForm.Parent:= ATab;
      ADomainForm.Left:= 0;
      ADomainForm.Top:= 0;
      ADomainForm.BorderStyle:= bsNone;
      ADomainForm.Align:= alClient;
      PageControl1.ActivePage:= ATab;
    end
    else
      ATab:= ADomainForm.Parent as TTabSheet;
    PageControl1.ActivePage:= ATab;

    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    dmSysTables.GetDomainInfo(dbIndex, ADomainName, DomainType, DomainSize, DefaultValue, CheckConstraint, CharacterSet, Collation);
    ATab.Tag:= dbIndex;
    if Pos('default', LowerCase(DefaultValue)) = 1 then
      DefaultValue:= Trim(Copy(DefaultValue, 8, Length(DefaultValue)));
    if (Pos('CHAR', DomainType) > 0) or
      (Pos('CSTRING', DomainType) >0) then
      DomainType:= DomainType + '(' + IntToStr(DomainSize) + ')';

    // Fill ViewDomain form
    with ADomainForm do
    begin
      Caption:= Title;
      ATab.Caption:= Caption;
      edName.Caption:= ADomainName;
      laType.Caption:= DomainType;
      laSize.Caption:= IntToStr(DomainSize);
      laDefault.Caption:= DefaultValue;
      laCheckConstraint.Caption:= CheckConstraint;
      laCharacterSet.Caption:= CharacterSet;
      laCollation.Caption:= Collation;
    end;
    ADomainForm.Show;
  end;
end; }

procedure TfmMain.lmViewDomainClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  ADomainName: string;
  CheckConstraint, CharacterSet, Collation, DomainType, DefaultValue: string;
  DomainSize, dbIndex: Integer;
  ATab: TTabSheet;
  frmViewDomain: TfmViewDomain;
  ShortTitle, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  ADomainName := SelNode.Text;

  // Domain-Info aus DB holen
  dmSysTables.GetDomainInfo(dbIndex, ADomainName, DomainType, DomainSize, DefaultValue,
    CheckConstraint, CharacterSet, Collation);

  // DefaultValue bereinigen
  if Pos('default', LowerCase(DefaultValue)) = 1 then
    DefaultValue := Trim(Copy(DefaultValue, 8, Length(DefaultValue)));

  if (Pos('CHAR', DomainType) > 0) or (Pos('CSTRING', DomainType) > 0) then
    DomainType := DomainType + '(' + IntToStr(DomainSize) + ')';

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmViewDomain) then
    frmViewDomain := TfmViewDomain(NodeInfos^.ViewForm)
  else
  begin
    frmViewDomain := TfmViewDomain.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmViewDomain.Parent := ATab;
    frmViewDomain.Align := alClient;
    frmViewDomain.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmViewDomain;
  end;

  // Tab vorbereiten
  ATab := frmViewDomain.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Tab-Titel
  ShortTitle := ADomainName;
  ATab.Caption := ShortTitle;
  frmViewDomain.Caption := ShortTitle;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBConnection.DatabaseName + sLineBreak +
    'Object type: Domain' + sLineBreak +
    'Domain name: ' + ADomainName + sLineBreak +
    'Type: ' + DomainType + sLineBreak +
    'Size: ' + IntToStr(DomainSize) + sLineBreak +
    'Default: ' + DefaultValue + sLineBreak +
    'Check: ' + CheckConstraint + sLineBreak +
    'Charset: ' + CharacterSet + sLineBreak +
    'Collation: ' + Collation;

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form mit Domaindaten füllen
  with frmViewDomain do
  begin
    edName.Caption := ADomainName;
    laType.Caption := DomainType;
    laSize.Caption := IntToStr(DomainSize);
    laDefault.Caption := DefaultValue;
    laCheckConstraint.Caption := CheckConstraint;
    laCharacterSet.Caption := CharacterSet;
    laCollation.Caption := Collation;
  end;

  frmViewDomain.Init(SelNode.Data);
  frmViewDomain.Show;
end;


procedure TfmMain.GetPackageFunctions(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);
var
  QueryTemplate: string;
  Rec: TDatabaseRec;
  FBFunctionName: string;
  TmpNode: TTreeNode;
begin
  QueryTemplate :=
    'SELECT RDB$Function_NAME AS NAME ' +
    'FROM RDB$Functions ' +
    'WHERE RDB$PACKAGE_NAME = ''%s'' ' +        // Muss Teil eines Packages sein
    'AND RDB$SYSTEM_FLAG = 0 ' +                 // Keine Systemfunktionen
    'AND RDB$MODULE_NAME IS NULL ' +             // Modulname ≠ extern → keine UDF
    'AND RDB$ENGINE_NAME IS NULL ' +
    'ORDER BY NAME';

  //SQLQuery1.SQLConnection.Params.Add('sql_dialect=3');
  SQLQuery1.Close;

  if Assigned(FSQLTransaction) then
    FSQLTransaction.Commit;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);
  SQLQuery1.Open;

  //if SQLQuery1.RecordCount  > 0 then
  if not SQLQuery1.IsEmpty then
  begin
    AStrList.Clear;
    SQLQuery1.First;
    while not SQLQuery1.EOF do
    begin
      FBFunctionName := Trim(SQLQuery1.FieldByName('NAME').AsString);
      if AStrList.IndexOf(FBFunctionName) = -1 then
        AStrList.Add(FBFunctionName);
      SQLQuery1.Next;
    end;
  end;
  SQLQuery1.Close;
  //SQLQuery1.First;
end;

procedure TfmMain.GetPackageProcedures(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);
var
  QueryTemplate: string;
  Rec: TDatabaseRec;
  ProcName: string;
begin
  QueryTemplate :=
    'SELECT RDB$Procedure_NAME AS NAME ' +
    'FROM RDB$Procedures ' +
    'WHERE RDB$PACKAGE_NAME = ''%s'' ' +        // Muss Teil eines Packages sein
    'AND RDB$SYSTEM_FLAG = 0 ' +                 // Keine Systemfunktionen
    //'AND RDB$MODULE_NAME IS NULL ' +             // Modulname ≠ extern → keine UDF
    'AND RDB$ENGINE_NAME IS NULL ' +
    'ORDER BY NAME';

  SQLQuery1.SQLConnection.Params.Add('sql_dialect=3');
  SQLQuery1.Close;

  if Assigned(FSQLTransaction) then
    FSQLTransaction.Commit;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);
  SQLQuery1.Open;

  if AStrList <> nil then
  begin
    AStrList.Clear;
    while not SQLQuery1.EOF do
    begin
      ProcName := Trim(SQLQuery1.FieldByName('NAME').AsString);
      if AStrList.IndexOf(ProcName) = -1 then
        AStrList.Add(ProcName);
      SQLQuery1.Next;
    end;
  end;
  SQLQuery1.Close;
  //SQLQuery1.First;
end;

procedure TfmMain.GetPackageUDRFunctions(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);
var
  QueryTemplate: string;
  Rec: TDatabaseRec;
  FBFunctionName: string;
  TmpNode: TTreeNode;
begin
  QueryTemplate :=
    'SELECT RDB$Function_NAME AS NAME ' +
    'FROM RDB$Functions ' +
    'WHERE RDB$PACKAGE_NAME = ''%s'' ' +        // Muss Teil eines Packages sein
    'AND RDB$SYSTEM_FLAG = 0 ' +                 // Keine Systemfunktionen
    'AND RDB$MODULE_NAME IS NULL ' +             // Modulname ≠ extern → keine UDF
    'AND RDB$ENGINE_NAME IS NOT NULL ' +
    'ORDER BY NAME';

  //SQLQuery1.SQLConnection.Params.Add('sql_dialect=3');
  SQLQuery1.Close;

  if Assigned(FSQLTransaction) then
    FSQLTransaction.Commit;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);
  SQLQuery1.Open;

  //if SQLQuery1.RecordCount  > 0 then
  if not SQLQuery1.IsEmpty then
  begin
    AStrList.Clear;
    SQLQuery1.First;
    while not SQLQuery1.EOF do
    begin
      FBFunctionName := Trim(SQLQuery1.FieldByName('NAME').AsString);
      if AStrList.IndexOf(FBFunctionName) = -1 then
        AStrList.Add(FBFunctionName);
      SQLQuery1.Next;
    end;
  end;
  SQLQuery1.Close;
  //SQLQuery1.First;
end;

procedure TfmMain.GetPackageUDRProcedures(DatabaseIndex: Integer; APackageName: string; AStrList: TStringList);
var
  QueryTemplate: string;
  Rec: TDatabaseRec;
  ProcName: string;
begin
  QueryTemplate :=
    'SELECT RDB$Procedure_NAME AS NAME ' +
    'FROM RDB$Procedures ' +
    'WHERE RDB$PACKAGE_NAME = ''%s'' ' +        // Muss Teil eines Packages sein
    'AND RDB$SYSTEM_FLAG = 0 ' +                 // Keine Systemfunktionen
    //'AND RDB$MODULE_NAME IS NOT NULL ' +             // Modulname ≠ extern → keine UDF
    'AND RDB$ENGINE_NAME IS NOT NULL ' +
    'ORDER BY NAME';

  SQLQuery1.SQLConnection.Params.Add('sql_dialect=3');
  SQLQuery1.Close;

  if Assigned(FSQLTransaction) then
    FSQLTransaction.Commit;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);
  SQLQuery1.Open;

  if AStrList <> nil then
  begin
    AStrList.Clear;
    while not SQLQuery1.EOF do
    begin
      ProcName := Trim(SQLQuery1.FieldByName('NAME').AsString);
      if AStrList.IndexOf(ProcName) = -1 then
        AStrList.Add(ProcName);
      SQLQuery1.Next;
    end;
  end;
  SQLQuery1.Close;
  //SQLQuery1.First;
end;


(********************  Get Fields  **************************)
procedure TfmMain.GetFields(DatabaseIndex: Integer; ATableName: string; FieldsList: TStringList);
var QueryTemplate: string;
  Rec: TDatabaseRec;
  FieldName: string;
begin
  QueryTemplate := 'SELECT r.RDB$FIELD_NAME AS field_name, ' +
    ' r.RDB$DESCRIPTION AS field_description, ' +
    ' r.RDB$DEFAULT_SOURCE AS field_default_source, ' {SQL source for default value }+
    ' r.RDB$NULL_FLAG AS field_not_null_constraint, ' +
    ' f.RDB$FIELD_LENGTH AS field_length, ' +
    ' f.RDB$CHARACTER_LENGTH AS characterlength, ' + {character_length seems a reserved word}
    ' f.RDB$FIELD_PRECISION AS field_precision, ' +
    ' f.RDB$FIELD_SCALE AS field_scale, ' +
    ' f.RDB$FIELD_TYPE as field_type_int, ' +
    ' f.RDB$FIELD_SUB_TYPE AS field_sub_type, ' +
    ' coll.RDB$COLLATION_NAME AS field_collation, ' +
    ' cset.RDB$CHARACTER_SET_NAME AS field_charset, ' +
    ' f.RDB$computed_source AS computed_source, ' +
    ' dim.RDB$UPPER_BOUND AS array_upper_bound, ' +
    ' r.RDB$FIELD_SOURCE AS field_source ' {domain if field based on domain}+
    ' FROM RDB$RELATION_FIELDS r ' +
    ' LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
    ' LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID and f.rdb$character_set_id=coll.rdb$character_set_id ' +
    ' LEFT JOIN RDB$CHARACTER_SETS cset ON f.RDB$CHARACTER_SET_ID = cset.RDB$CHARACTER_SET_ID ' +
    ' LEFT JOIN RDB$FIELD_DIMENSIONS dim ON f.RDB$FIELD_NAME = dim.RDB$FIELD_NAME ' +
    ' WHERE r.RDB$RELATION_NAME=''%s'' ' +
    ' ORDER BY r.RDB$FIELD_POSITION;';


{  QueryTemplate :=
    'SELECT r.RDB$FIELD_NAME AS field_name, ' +
    ' CAST(r.RDB$DESCRIPTION AS VARCHAR(255)) AS field_description, ' +
    ' CAST(r.RDB$DEFAULT_SOURCE AS VARCHAR(255)) AS field_default_source, ' +
    ' r.RDB$NULL_FLAG AS field_not_null_constraint, ' +
    ' f.RDB$FIELD_LENGTH AS field_length, ' +
    ' f.RDB$CHARACTER_LENGTH AS characterlength, ' +
    ' f.RDB$FIELD_PRECISION AS field_precision, ' +
    ' f.RDB$FIELD_SCALE AS field_scale, ' +
    ' f.RDB$FIELD_TYPE AS field_type_int, ' +
    ' f.RDB$FIELD_SUB_TYPE AS field_sub_type, ' +
    ' coll.RDB$COLLATION_NAME AS field_collation, ' +
    ' cset.RDB$CHARACTER_SET_NAME AS field_charset, ' +
    ' CAST(f.RDB$COMPUTED_SOURCE AS VARCHAR(255)) AS computed_source, ' +
    ' dim.RDB$UPPER_BOUND AS array_upper_bound, ' +
    ' r.RDB$FIELD_SOURCE AS field_source ' +
    'FROM RDB$RELATION_FIELDS r ' +
    'LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
    'LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID ' +
    '  AND f.RDB$CHARACTER_SET_ID = coll.RDB$CHARACTER_SET_ID ' +
    'LEFT JOIN RDB$CHARACTER_SETS cset ON f.RDB$CHARACTER_SET_ID = cset.RDB$CHARACTER_SET_ID ' +
    'LEFT JOIN RDB$FIELD_DIMENSIONS dim ON f.RDB$FIELD_NAME = dim.RDB$FIELD_NAME ' +
    'WHERE r.RDB$RELATION_NAME = ''%s'' ' +
    'ORDER BY r.RDB$FIELD_POSITION;';
 }

  SQLQuery1.SQLConnection := RegisteredDatabases[DatabaseIndex].IBConnection;
  SQLQuery1.SQLConnection.Params.Add('sql-dialect=3');
  SQLQuery1.Close;
  {A bit unclear why the transaction needs to be committed but at least do it
  before changing the query's transaction}
  if (Assigned(FSQLTransaction)) then
    FSQLTransaction.Commit;
  Rec:= RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);
  SQLQuery1.SQL.Text:= format(QueryTemplate,[ATableName]);
  {$IFDEF NEVER}
  // Left for debugging
  SendDebug('GetFields: '+SQLQuery1.SQL.Text);
  {$ENDIF}
  SQLQuery1.Open;
  // If FieldsList is nil, don't try to fill results. Calling code probably
  // just wants the query. Let's hope so.
  if FieldsList <> nil then
  begin
    FieldsList.Clear;
    while not SQLQuery1.EOF do
    begin
      FieldName:= Trim(SQLQuery1.FieldByName('field_name').AsString);
      // Avoid duplicate field names
      if FieldsList.IndexOf(FieldName) = -1 then
        FieldsList.Add(FieldName);
      SQLQuery1.Next;
    end;
  end;
  SQLQuery1.First;
end;

(**********  Get Stored Proc body  ****************)
Function TfmMain.GetStoredProcBody(DatabaseIndex: Integer; AProcName: string; var SPOwner: string): string;
const
  BodyTemplate=
    'SELECT * FROM rdb$procedures where rdb$Procedure_name =  ''%s'' ';
  ParamTemplate=
   'SELECT rdb$parameter_name, rdb$field_type, rdb$field_sub_type, '+
   'rdb$field_length, rdb$field_scale, rdb$field_precision, '+
   'rdb$character_length, rdb$parameter_type '+
   'FROM rdb$procedure_parameters sp_param '+
   'JOIN rdb$fields fld '+
   'ON sp_param.rdb$field_source = fld.rdb$field_name '+
   'WHERE '+
   'sp_param.rdb$procedure_name =''%s'' ' +
   'order by rdb$parameter_type, rdb$parameter_number';
var
  Rec: TDatabaseRec;
  i: Integer;
  InputParams: integer; //count of input parameters
  Line: string;
  ParamName: string;
  OutputParams: integer; //count of output params
  BodyList: TStringList; // procedure body
begin
  try
    AProcName:= UpperCase(AProcName);
    BodyList:= TStringList.Create;
    try
      Rec:= RegisteredDatabases[DatabaseIndex];
      SetConnection(DatabaseIndex);

      // Get number of input and output parameters
      SQLQuery1.Close;
      SQLQuery1.SQL.Text:= format(BodyTemplate,[AProcName]);
      SQLQuery1.Open;
      // Null will result in 0 which is fine here
      InputParams:= SQLQuery1.FieldByName('rdb$procedure_inputs').AsInteger;
      OutputParams:= SQLQuery1.FieldByName('rdb$procedure_outputs').AsInteger;

      SQLQuery1.Close;
      SQLQuery1.SQL.Text:= format(ParamTemplate,[AProcName]);
      SQLQuery1.Open;

      // Get input parameters
      if InputParams>0 then
      begin
        BodyList.Add('(');
        i:= 1;
        while (not SQLQuery1.EOF) and (i<=InputParams) do
        begin
          // Check for input parameter type:
          if (SQLQuery1.FieldByName('rdb$parameter_type').AsInteger=0) then
          begin
            ParamName:= Trim(SQLQuery1.FieldByName('rdb$parameter_name').AsString);
            Line:= '  ' + ParamName + '    ' +
              GetFBTypeName(SQLQuery1.FieldByName('RDB$Field_Type').AsInteger,
              SQLQuery1.FieldByName('rdb$field_sub_type').AsInteger,
              SQLQuery1.FieldByName('rdb$field_length').AsInteger,
              SQLQuery1.FieldByName('rdb$field_precision').AsInteger,
              SQLQuery1.FieldByName('rdb$field_scale').AsInteger);
            if SQLQuery1.FieldByName('RDB$Field_Type').AsInteger in [CharType,CStringType,VarCharType] then
              Line:= Line + '(' + SQLQuery1.FieldByName('RDB$Character_Length').AsString + ')';
            if (InputParams>1) and (i<InputParams) then
              Line:= Line + ',';
            BodyList.Add(Line);
            inc(i);
          end;
          SQLQuery1.Next;
        end;
        BodyList.Add(')' + LineEnding);
      end;

      // Get output parameters
      if OutputParams>0 then
      begin
        BodyList.Add('RETURNS (');
        i:= 1;
        while (not SQLQuery1.EOF) and (i<=OutputParams) do
        begin
          // Check for input parameter type:
          if (SQLQuery1.FieldByName('rdb$parameter_type').AsInteger=1) then
          begin
            ParamName:= Trim(SQLQuery1.FieldByName('rdb$parameter_name').AsString);
            Line:= '  ' + ParamName + '    ' +
              GetFBTypeName(SQLQuery1.FieldByName('RDB$Field_Type').AsInteger,
              SQLQuery1.FieldByName('rdb$field_sub_type').AsInteger,
              SQLQuery1.FieldByName('rdb$field_length').AsInteger,
              SQLQuery1.FieldByName('rdb$field_precision').AsInteger,
              SQLQuery1.FieldByName('rdb$field_scale').AsInteger);
            if SQLQuery1.FieldByName('RDB$Field_Type').AsInteger in [CharType,CStringType,VarCharType] then
              Line:= Line + '(' + SQLQuery1.FieldByName('RDB$Character_Length').AsString + ')';
            if (OutputParams>1) and (i<OutputParams) then
              Line:= Line + ',';
            BodyList.Add(Line);
            inc(i);
          end;
          SQLQuery1.Next;
        end;
        BodyList.Add(')' + LineEnding);
      end;
      SQLQuery1.Close;

      BodyList.Add('AS');

      // Get Procedure body (using the same query as before)
      SQLQuery1.SQL.Text:= format(BodyTemplate,[AProcName]);
      SQLQuery1.Open;
      SPOwner:= Trim(SQLQuery1.FieldByName('rdb$Owner_Name').AsString);
      // Actual body text:
      BodyList.Add(SQLQuery1.FieldByName('rdb$Procedure_Source').AsString);
      SQLQuery1.Close;
      Result:= BodyList.Text;
    finally
      BodyList.Free;
    end;
  except
    on E: Exception do
      MessageDlg('Error while getting stored procedure information: ' + e.Message, mtError, [mbOk], 0);
  end;
end;

(******************  Get View Info (SQL Source) ***************)
Function TfmMain.GetViewInfo(DatabaseIndex: Integer; AViewName: string; var Columns, Body: string): Boolean;
const
  BodyTemplate= 'SELECT RDB$VIEW_SOURCE ' +
    ' FROM RDB$RELATIONS ' +
    ' WHERE RDB$VIEW_SOURCE IS NOT NULL ' +
    ' AND UPPER(RDB$RELATION_NAME) = ''%s'';';
  ColumnsTemplate= 'select r.rdb$field_name '+
    ' from rdb$relation_fields r ' +
    ' inner join rdb$fields f on ' +
    ' r.rdb$field_source=f.rdb$field_name ' +
    ' inner join rdb$types t on ' +
    ' f.rdb$field_type=t.rdb$type ' +
    ' where upper(r.rdb$relation_name)=''%s'' and ' +
    ' t.rdb$field_name=''RDB$FIELD_TYPE'' ' +
    ' order by r.RDB$FIELD_POSITION ';
var
  Rec: TDatabaseRec;
begin
  Rec:= RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  // View Body
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= format(BodyTemplate, [UpperCase(AViewName)]);

  SQLQuery1.Open;
  Body:= SQLQuery1.Fields[0].AsString;

  // View Columns
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= format(ColumnsTemplate, [UpperCase(AViewName)]);
  Columns:= '';
  SQLQuery1.Open;
  while not SQLQuery1.EOF do
  begin
    Columns:= Columns + Trim(SQLQuery1.FieldByName('rdb$field_name').AsString);
    SQLQuery1.Next;
    if not SQLQuery1.EOF then
      Columns:= Columns + ', ';
  end;
  SQLQuery1.Close;
  Result:= True;
end;

(************  Change Trigger activity  *************)
Function TfmMain.ChangeTriggerActivity(DatabaseIndex: Integer;
  ATriggerName: string; ActiveState: Boolean): Boolean;
var
  Rec: TDatabaseRec;
  ActiveStr: string;
begin
  try
    Rec:= RegisteredDatabases[DatabaseIndex];
    SetConnection(DatabaseIndex);

    SQLQuery1.Close;
    if ActiveState then
      ActiveStr:= 'Active'
    else
      ActiveStr:= 'InActive';
    SQLQuery1.SQL.Text:= 'alter trigger '+ ATriggerName + ' ' + ActiveStr;

    SQLQuery1.ExecSQL;
    Result:= True;
    FSQLTransaction.Commit;
    AddToSQLHistory(Rec.RegRec.Title, 'DDL', SQLQuery1.SQL.Text);
  except
    on E: Exception do
    begin
      ShowMessage('Error: ' + e.Message);
      Result:= False;
    end;
  end;
end;

(***************  Get Index fields  *******************)
Function TfmMain.GetIndexFields(ATableName, AIndexName: string;
  AQuery: TSQLQuery; var FieldsList: TStringList): Boolean;
begin
  AQuery.Close;
  AQuery.SQL.Text:= 'SELECT RDB$INDEX_SEGMENTS.RDB$FIELD_NAME AS field_name, ' + LineEnding +
     'RDB$INDICES.RDB$DESCRIPTION AS description, ' +LineEnding +
     '(RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION + 1) AS field_position ' +LineEnding +
     'FROM RDB$INDEX_SEGMENTS ' +LineEnding +
     'LEFT JOIN RDB$INDICES ON RDB$INDICES.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' +LineEnding +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS ON RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME ' +LineEnding +
     ' WHERE UPPER(RDB$INDICES.RDB$RELATION_NAME)=''' + UpperCase(ATablename) + '''         -- table name ' +LineEnding +
     '  AND UPPER(RDB$INDICES.RDB$INDEX_NAME)=''' + UpperCase(AIndexName) + ''' -- index name ' +LineEnding +
     '--  AND RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE IS NULL ' +LineEnding +
     'ORDER BY RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION;';
  AQuery.Open;
  Result:= AQuery.FieldCount > 0;
  FieldsList.Clear;

  // Get index field names
  if Result then
  while not AQuery.EOF do
  begin
    FieldsList.Add(Trim(AQuery.FieldByName('field_name').AsString));
    AQuery.Next;
  end;
  if not Result then
    AQuery.Close;
end;

(***********  Get UDF Info  ***************)
function TfmMain.GetUDFInfo(DatabaseIndex: Integer; UDFName: string;
  var ModuleName, EntryPoint, Params: string): Boolean;
var
  Rec: TDatabaseRec;
  FieldType, SubType, FieldLength, Precision, Scale: Integer;
  CharLen: string;
begin
  try
    Rec := RegisteredDatabases[DatabaseIndex];
    SetConnection(DatabaseIndex);

    // 1) Funktionskopf (welches Modul, welcher EntryPoint)
    SQLQuery1.Close;
    SQLQuery1.SQL.Text :=
      'SELECT RDB$MODULE_NAME, RDB$ENTRYPOINT ' +
      'FROM RDB$FUNCTIONS ' +
      'WHERE RDB$FUNCTION_NAME = :FNAME';
    SQLQuery1.ParamByName('FNAME').AsString := UpperCase(UDFName);
    SQLQuery1.Open;

    if SQLQuery1.EOF then
    begin
      Result := False;
      Exit;
    end;

    ModuleName := Trim(SQLQuery1.FieldByName('RDB$MODULE_NAME').AsString);
    EntryPoint := Trim(SQLQuery1.FieldByName('RDB$ENTRYPOINT').AsString);

    SQLQuery1.Close;

    // 2) Eingabeparameter
    SQLQuery1.SQL.Text :=
      'SELECT RDB$FIELD_TYPE, RDB$FIELD_SUB_TYPE, RDB$FIELD_LENGTH, ' +
      '       RDB$FIELD_PRECISION, RDB$FIELD_SCALE, RDB$CHARACTER_LENGTH ' +
      'FROM RDB$FUNCTION_ARGUMENTS ' +
      'WHERE RDB$FUNCTION_NAME = :FNAME ' +
      '  AND RDB$MECHANISM = 1 ' +
      'ORDER BY RDB$ARGUMENT_POSITION';
    SQLQuery1.ParamByName('FNAME').AsString := UpperCase(UDFName);
    SQLQuery1.Open;

    Params := '(';
    while not SQLQuery1.EOF do
    begin
      FieldType   := SQLQuery1.FieldByName('RDB$FIELD_TYPE').AsInteger;
      SubType     := SQLQuery1.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      FieldLength := SQLQuery1.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      Precision   := SQLQuery1.FieldByName('RDB$FIELD_PRECISION').AsInteger;
      Scale       := SQLQuery1.FieldByName('RDB$FIELD_SCALE').AsInteger;
      CharLen     := SQLQuery1.FieldByName('RDB$CHARACTER_LENGTH').AsString;

      Params := Params + GetFBTypeName(FieldType, SubType, FieldLength, Precision, Scale);

      if FieldType in [CharType, CStringType, VarCharType] then
        Params := Params + '(' + CharLen + ')';

      SQLQuery1.Next;
      if not SQLQuery1.EOF then
        Params := Params + ', ';
    end;
    SQLQuery1.Close;

    Params := Params + ')' + LineEnding + 'RETURNS ';

    // 3) Rückgabewerte
    SQLQuery1.SQL.Text :=
      'SELECT RDB$FIELD_TYPE, RDB$FIELD_SUB_TYPE, RDB$FIELD_LENGTH, ' +
      '       RDB$FIELD_PRECISION, RDB$FIELD_SCALE, RDB$CHARACTER_LENGTH ' +
      'FROM RDB$FUNCTION_ARGUMENTS ' +
      'WHERE RDB$FUNCTION_NAME = :FNAME ' +
      '  AND RDB$MECHANISM = 0 ' +
      'ORDER BY RDB$ARGUMENT_POSITION';
    SQLQuery1.ParamByName('FNAME').AsString := UpperCase(UDFName);
    SQLQuery1.Open;

    while not SQLQuery1.EOF do
    begin
      FieldType   := SQLQuery1.FieldByName('RDB$FIELD_TYPE').AsInteger;
      SubType     := SQLQuery1.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      FieldLength := SQLQuery1.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      Precision   := SQLQuery1.FieldByName('RDB$FIELD_PRECISION').AsInteger;
      Scale       := SQLQuery1.FieldByName('RDB$FIELD_SCALE').AsInteger;
      CharLen     := SQLQuery1.FieldByName('RDB$CHARACTER_LENGTH').AsString;

      Params := Params + GetFBTypeName(FieldType, SubType, FieldLength, Precision, Scale);

      if FieldType in [CharType, CStringType, VarCharType] then
        Params := Params + '(' + CharLen + ')';

      SQLQuery1.Next;
      if not SQLQuery1.EOF then
        Params := Params + ', ';
    end;
    SQLQuery1.Close;

    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      FIBConnection.Close;
      Result := False;
    end;
  end;
end;

(***********  Show Query window  ************)
function TfmMain.ShowQueryWindow(DatabaseIndex: Integer; ATitle: string; ANodeInfos: TPNodeInfos=nil): TfmQueryWindow;
var
  Rec: TDatabaseRec;
  ATab: TTabSheet;
  AServer,
  ADBAlias,
  ADBPath,
  AObjectType,
  AName,
  ACaption,
  FullHint: string;
  NodeQueryWindow: boolean;
begin
  Result := nil;
  NodeQueryWindow := false;

  Rec:= RegisteredDatabases[DatabaseIndex];

  AServer   := 'Server:   ' +  GetAncestorNodeText(tvMain.Selected, 0);
  ADBAlias  := 'DBAlias:  ' +  Rec.RegRec.Title;
  ADBPath   := 'DBPath:   ' +  Rec.RegRec.DatabaseName;
  FullHint  :=  AServer + sLineBreak + ADBAlias + sLineBreak + ADBPath + sLineBreak;

  if ANodeInfos = nil then
  begin
    //ACaption  := Rec.RegRec.Title + ': ' + ATitle;
    ACaption  := ATitle;
    Result:= TfmQueryWindow(FindQueryWindow(ACaption))
  end else
  begin
    NodeQueryWindow := true;
    if Assigned(ANodeInfos^.ViewForm) and (ANodeInfos^.ViewForm is TfmQueryWindow) then
      Result := TfmQueryWindow(ANodeInfos^.ViewForm);
  end;

  if Result = nil then
  begin
    // No opened query window
    Result:= TfmQueryWindow.Create(Application);
    ATab:= TTabSheet.Create(self);
    ATab.Parent:= PageControl1;
    Result.Parent:= ATab;
    Result.Left:= 0;
    Result.Top:= 0;
    Result.Align:= alClient;

    ATab.Tag:= DatabaseIndex;
    ATab.ShowHint := True;
    Result.BorderStyle:= bsNone;
    Result.Caption:= ACaption;

    if NodeQueryWindow then
    begin
      ANodeInfos^.ViewForm := Result;
      ATab.ImageIndex := tvMain.Selected.ImageIndex;
      if tvMain.Selected.Text <> 'Query Window' then //Main-QueryWindow
      begin
        ACaption :=  tvMain.Selected.Text;
        FullHint := FullHint + 'Object type: '  + TreeViewObjectToStr(ANodeInfos^.ObjectType) + sLineBreak +
                    'Object Name:  ' + ACaption;
        //ACaption := 'SELECT:' + ACaption;
      end else
      begin
        ACaption := 'SQL';
        FullHint := FullHint + 'Object type: '  + TreeViewObjectToStr(ANodeInfos^.ObjectType);
      end;
    end else
    begin
      ATab.ImageIndex := 1;
      FullHint := FullHint + 'Object type: Query Window';
    end;

    ATab.Caption:= ACaption;
    ATab.Hint := FullHint;
  end

  else // Already opened query window found
    ATab:= Result.Parent as TTabSheet;

  Result.Init(DatabaseIndex, ANodeInfos);
  OpenSQLHistory(Rec.RegRec.Title);
  Result.Parent.Show;
  Result.Show;
  fmMain.Show;
end;

(***********  Show and Fill Query Window *****************)
procedure TfmMain.ShowCompleteQueryWindow(DatabaseIndex: Integer; ATitle,
  AQueryText: string; OnCommitProcedure: TNotifyEvent = nil; ANodeInfos: TPNodeInfos=nil);
var
  QWindow: TfmQueryWindow;
  Part: string;
begin
  QWindow:= ShowQueryWindow(DatabaseIndex, ATitle, ANodeInfos);
  QWindow.meQuery.ClearAll;
  QWindow.OnCommit:= OnCommitProcedure;
  repeat
    if Pos(LineEnding, AQueryText) > 0 then
      Part:= Copy(AQueryText, 1, Pos(LineEnding, AQueryText))
    else
      Part:= AQueryText;
    Delete(AQueryText, 1, Length(Part));
    Part:= StringReplace(Part, LineEnding, ' ', [rfReplaceAll]);

    QWindow.meQuery.Lines.Add(Part);
  until AQueryText = '';
end;

(******* Fill Object Root, like (Tables, Views, etc)  ******)
procedure TfmMain.FillObjectRoot(Node: TTreeNode);
var
  DBIndex: Integer;
  Objects: TStringList;
  i, x, Count: Integer;
  Item, SubNode: TTreeNode;
  ANodeText: string;
begin
  DBIndex := TPNodeInfos(Node.Parent.Data)^.dbIndex;
  Objects := TStringList.Create;
  try
    ANodeText := Node.Text;
    if Pos('(', ANodeText) > 0 then
      ANodeText := Trim(Copy(ANodeText, 1, Pos('(', ANodeText)-1));

    Node.DeleteChildren;

    case TPNodeInfos(Node.Data)^.ObjectType of
      tvotPackageRoot:
        begin
          Objects.CommaText := dmSysTables.GetDBObjectNames(DBIndex, otPackages, Count);
          Node.Text := ANodeText + ' (' + IntToStr(Objects.Count) + ')';

          for i := 0 to Objects.Count-1 do
          begin
            Item := tvMain.Items.AddChild(Node, Objects[i]);
            TPNodeInfos(Item.Data)^.ObjectType := tvotPackage;
            TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
            Item.ImageIndex := 60;
            Item.SelectedIndex := 60;

            // Funktionen
            SubNode := tvMain.Items.AddChild(Item, 'Functions');
            TPNodeInfos(SubNode.Data)^.ObjectType := tvotPackageFunctionRoot;
            SubNode.ImageIndex := 61;
            Objects.Clear;
            GetPackageFunctions(DBIndex, Item.Text, Objects);
            SubNode.Text := 'Functions (' + IntToStr(Objects.Count) + ')';
            for x := 0 to Objects.Count-1 do
            begin
              Item := tvMain.Items.AddChild(SubNode, Objects[x]);
              TPNodeInfos(Item.Data)^.ObjectType := tvotPackageFunction;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item.ImageIndex := 61;
            end;

            // Prozeduren
            SubNode := tvMain.Items.AddChild(Item, 'Procedures');
            TPNodeInfos(SubNode.Data)^.ObjectType := tvotPackageProcedureRoot;
            SubNode.ImageIndex := 62;
            Objects.Clear;
            GetPackageProcedures(DBIndex, Item.Text, Objects);
            SubNode.Text := 'Procedures (' + IntToStr(Objects.Count) + ')';
            for x := 0 to Objects.Count-1 do
            begin
              Item := tvMain.Items.AddChild(SubNode, Objects[x]);
              TPNodeInfos(Item.Data)^.ObjectType := tvotPackageProcedure;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item.ImageIndex := 62;
            end;

            // UDR-Funktionen
            SubNode := tvMain.Items.AddChild(Item, 'UDR-Functions');
            TPNodeInfos(SubNode.Data)^.ObjectType := tvotPackageUDRFunctionRoot;
            SubNode.ImageIndex := 64;
            Objects.Clear;
            GetPackageUDRFunctions(DBIndex, Item.Text, Objects);
            SubNode.Text := 'UDR-Functions (' + IntToStr(Objects.Count) + ')';
            for x := 0 to Objects.Count-1 do
            begin
              Item := tvMain.Items.AddChild(SubNode, Objects[x]);
              TPNodeInfos(Item.Data)^.ObjectType := tvotPackageUDRFunction;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item.ImageIndex := 64;
            end;

            // UDR-Prozeduren
            SubNode := tvMain.Items.AddChild(Item, 'UDR-Procedures');
            TPNodeInfos(SubNode.Data)^.ObjectType := tvotPackageUDRProcedureRoot;
            SubNode.ImageIndex := 65;
            Objects.Clear;
            GetPackageUDRProcedures(DBIndex, Item.Text, Objects);
            SubNode.Text := 'UDR-Procedures (' + IntToStr(Objects.Count) + ')';
            for x := 0 to Objects.Count-1 do
            begin
              Item := tvMain.Items.AddChild(SubNode, Objects[x]);
              TPNodeInfos(Item.Data)^.ObjectType := tvotPackageUDRProcedure;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item.ImageIndex := 65;
            end;
          end;
        end;

      tvotUDRRoot:
        begin
          // UDR-Functions
          Objects.CommaText := dmSysTables.GetDBObjectNames(DBIndex, otUDRFunctions, Count);
          if Count > 0 then
          begin
            SubNode := tvMain.Items.AddChild(Node, '');
            TPNodeInfos(SubNode.Data)^.ObjectType := tvotUDRFunctionRoot;
            SubNode.ImageIndex := 67;
            for i := 0 to Objects.Count-1 do
            begin
              Item := tvMain.Items.AddChild(SubNode, Objects[i]);
              TPNodeInfos(Item.Data)^.ObjectType := tvotUDRFunction;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item.ImageIndex := 67;
            end;
            SubNode.Text := 'Functions (' + IntToStr(SubNode.Count) + ')';
          end;

          // UDR-Procedures
          Objects.CommaText := dmSysTables.GetDBObjectNames(DBIndex, otUDRProcedures, Count);
          if Count > 0 then
          begin
            SubNode := tvMain.Items.AddChild(Node, '');
            TPNodeInfos(SubNode.Data)^.ObjectType := tvotUDRProcedureRoot;
            SubNode.ImageIndex := 68;
            for i := 0 to Objects.Count-1 do
            begin
              Item := tvMain.Items.AddChild(SubNode, Objects[i]);
              TPNodeInfos(Item.Data)^.ObjectType := tvotUDRProcedure;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item.ImageIndex := 68;
            end;
            SubNode.Text := 'Procedures (' + IntToStr(SubNode.Count) + ')';
          end;
        end;
    end;

  finally
    Objects.Free;
    Node.Expand(False);
  end;
end;

(*************  Get main indices information  ******************)
Function TfmMain.GetIndices(ATableName: string; AQuery: TSQLQuery): Boolean;
begin
  AQuery.Close;
  AQuery.SQL.Text:= 'SELECT * FROM RDB$INDICES WHERE RDB$RELATION_NAME=''' + UpperCase(ATableName) +
    ''' AND RDB$FOREIGN_KEY IS NULL';
  AQuery.Open;
  //Result:= AQuery.RecordCount > 0;
  Result := not SQLQuery1.IsEmpty;
  if not Result then
    AQuery.Close;
end;

(***************  View Table Fields/ Fields Management  ***************)
{ todo: getting table field data should be moved to tablemanage.pas if possible; even better split out
between non-GUI query part and GUI updater part}
procedure TfmMain.ViewTableFields(ATableName: string; dbIndex: Integer;
  AStringGrid: TStringGrid);
var
  FieldSize: integer;
  FieldType: string;
  CleanTypeName: string;
  i: Integer;
  PKFieldsList: TStringList;
  DefaultValue: string;
  PKIndexName: string;
  ConstraintName: string;
  TmpInt: integer;
  IsUUID: boolean;
begin
  try
    GetFields(dbIndex, ATableName, nil);

    // Fill TableInfo grid
    AStringGrid.RowCount:= 1;
    with AStringGrid, SQLQuery1 do
    while not EOF do
    begin
      RowCount:= RowCount + 1;

      // Field Name
      Cells[1, RowCount - 1]:= Trim(FieldByName('Field_Name').AsString);

      // Field Type
      GetFieldType(SQLQuery1,FieldType,FieldSize);
      Cells[2, RowCount - 1]:= FieldType;

      CleanTypeName := GetNameFromSizedTypeName(FieldType);

      IsUUID := (CleanTypeName = 'CHAR') and  (FieldByName('Field_Length').AsInteger = 16)
        and (Trim(UpperCase(FieldByName('Field_Charset').AsString)) = 'OCTETS');

      If isUUID then
      begin
        FieldType := 'UUID';
        Cells[7, RowCount - 1] := '';  //collation   ignore...
      end;

        // Computed fields (Calculated)
      if FieldByName('computed_source').AsString <> '' then
        Cells[2, RowCount - 1]:= FieldByName('computed_source').AsString;

      // Field Size
      if FieldByName('field_type_int').AsInteger in [CharType,CStringType,VarCharType] then
        Cells[3, RowCount - 1]:= FieldByName('CharacterLength').AsString
      else // why show byte size for numerical fields like integer fields?
        Cells[3, RowCount - 1]:= FieldByName('Field_Length').AsString;

      if (CleanTypeName = 'DECIMAL') or (CleanTypeName = 'NUMERIC') then
      begin
        Cells[4, RowCount - 1]:= FieldByName('field_precision').AsString;
        TmpInt := Abs(FieldByName('field_scale').AsInteger);
        Cells[5, RowCount - 1]:= IntToStr(TmpInt);
      end;

      if ((CleanTypeName = 'CHAR') or (CleanTypeName = 'VARCHAR') or (CleanTypeName = 'UUID'))  then
      begin
        Cells[6, RowCount - 1]:= FieldByName('field_charset').AsString;
      end;

      if ((CleanTypeName = 'CHAR') or (CleanTypeName = 'VARCHAR')) and (not IsUUID) then
        Cells[7, RowCount - 1]:= FieldByName('field_collation').AsString;

      // Null/Not null
      if FieldByName('field_not_null_constraint').AsString = '1' then
        Cells[8, RowCount - 1]:= '0'
      else
        Cells[8, RowCount - 1]:= '1';

      // Default Value
      DefaultValue := FieldByName('Field_Default_Source').AsString;
      Cells[9, RowCount - 1] := ExtractDefaultValue(DefaultValue);

      Cells[10, RowCount - 1]:= FieldByName('Field_Description').AsString;
      Next;
    end;
    SQLQuery1.Close;

    // Primary Keys
    PKFieldsList:= TStringList.Create;
    try
      PKIndexName:= GetPrimaryKeyIndexName(dbIndex, ATableName, ConstraintName);
      if PKIndexName <> '' then
        GetConstraintFields(ATableName, PKIndexName, PKFieldsList);

      with AStringGrid do
      for i:= 1 to RowCount - 1 do
        if PKFieldsList.IndexOf(Cells[1, i]) <> -1 then
          Cells[0, i]:= '1'
        else
          Cells[0, i]:= '0';
    finally
      PKFieldsList.Free;
    end;
  except
    on E: Exception do
      MessageDlg('Error while reading table fields: ' + e.Message, mtError, [mbOk], 0);
  end;
end;

(*************   Display View DDL *******************)
procedure TfmMain.lmDisplayViewClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Rec: TDatabaseRec;
  AViewName: string;
  ViewBody, Columns: string;
  dbIndex: Integer;
  ATab: TTabSheet;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin

    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    Rec:= RegisteredDatabases[dbIndex];
    AViewName:= SelNode.Text;

    // Fill ViewView grid
    ATab:= TTabSheet.Create(self);
    ATab.Parent:= PageControl1;
    fmViewView:= TfmViewView.Create(nil);
    fmViewView.Parent:= ATab;
    fmViewView.Left:= 0;
    fmViewView.Top:= 0;
    fmViewView.BorderStyle:= bsNone;
    fmViewView.Align:= alClient;
    fmViewView.SynSQLSyn1.TableNames.CommaText:= GetTableNames(dbIndex);
    fmViewView.Caption:= 'View DDL: ' + AViewName;
    ATab.Caption:= fmViewView.Caption;
    fmViewView.edName.Caption:= AViewName;
    ATab.Tag:= dbIndex;

    GetViewInfo(dbIndex, AViewName, Columns, ViewBody);
    fmViewView.seScript.Lines.Clear;
    fmViewView.seScript.Lines.Text:= 'create view "' + AviewName + '" (' + Columns + ')' + LineEnding + ViewBody;
    PageControl1.ActivePage:= ATab;
    fmViewView.Show;
  end;
end;

(***************  ExpandFields: Expand table fields  ************)
procedure TfmMain.lmViewFieldsClick(Sender: TObject);
var
  Node: TTreeNode;
  dbIndex: Integer;
  FieldTitle: string;
  FieldNode: TTreeNode;
  PKFieldsList: TStringList;
  PKIndexName: string;
  ConstraintName: string;
  AFieldName: string;
  i: Integer;
  ArraySuffix: string;
begin
  try
    Node := tvMain.Selected;
    dbIndex := TPNodeInfos(Node.Data)^.dbIndex;
    Node.DeleteChildren;

    // Primary Keys
    PKFieldsList := TStringList.Create;
    try
      PKIndexName := GetPrimaryKeyIndexName(dbIndex, Node.Text, ConstraintName);
      if PKIndexName <> '' then
        GetConstraintFields(Node.Text, PKIndexName, PKFieldsList);

      // Fields
      GetFields(dbIndex, Node.Text, nil);
      i := 1;
      with SQLQuery1 do
        while not EOF do
        begin
          AFieldName := Trim(FieldByName('Field_Name').AsString);

          // Array-Suffix zusammenbauen, falls vorhanden
          if not(FieldByName('array_upper_bound').IsNull) then
            ArraySuffix := ' [' + FieldByName('array_upper_bound').AsString + ']'
          else
            ArraySuffix := '';

          // Typname mit Länge, Präzision usw. holen
          FieldTitle := AFieldName + '   ' +
            GetFBTypeName(
              FieldByName('field_type_int').AsInteger,
              FieldByName('field_sub_type').AsInteger,
              FieldByName('field_length').AsInteger,
              FieldByName('field_precision').AsInteger,
              FieldByName('field_scale').AsInteger,
              FieldByName('field_charset').AsString,
              FieldByName('characterlength').AsInteger
            ) + ArraySuffix;

          FieldNode := tvMain.Items.AddChild(Node, FieldTitle);
          TPNodeInfos(FieldNode.Data)^.dbIndex := dbIndex;
          TPNodeInfos(FieldNode.Data)^.ObjectType := tvotTableField;

          // Primärschlüssel hervorheben
          if PKFieldsList.IndexOf(AFieldName) <> -1 then
          begin
            FieldNode.ImageIndex := 28;
            FieldNode.SelectedIndex := 28;
          end
          else
          begin
            FieldNode.ImageIndex := 27;
            FieldNode.SelectedIndex := 27;
          end;

          Inc(i);
          Next;
        end;
      SQLQuery1.Close;
      Node.Expand(False);
    finally
      PKFieldsList.Free;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfmMain.lmViewGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  Rec: TDatabaseRec;
  AGenName: string;
  dbIndex: Integer;
  ATab: TTabSheet;
  frmViewGen: TfmViewGen;
  ShortTitle, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  // Generator-Wert aus DB holen
  SetConnection(dbIndex);
  AGenName := SelNode.Text;
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := 'select GEN_ID(' + AGenName + ', 0) from RDB$Database;';
  SQLQuery1.Open;

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmViewGen) then
    frmViewGen := TfmViewGen(NodeInfos^.ViewForm)
  else
  begin
    frmViewGen := TfmViewGen.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmViewGen.Parent := ATab;
    frmViewGen.Align := alClient;
    frmViewGen.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmViewGen;
  end;

  // Tab vorbereiten
  ATab := frmViewGen.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Tab-Titel
  ShortTitle := AGenName;
  ATab.Caption := ShortTitle;
  frmViewGen.Caption := ShortTitle;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + Rec.IBConnection.DatabaseName + sLineBreak +
    'Object type: Generator' + sLineBreak +
    'Generator name: ' + AGenName + sLineBreak +
    'Current value: ' + SQLQuery1.Fields[0].AsString;

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form mit Generatordaten füllen
  with frmViewGen do
  begin
    edGenName.Caption := AGenName;
    edValue.Caption := SQLQuery1.Fields[0].AsString;
  end;

  frmViewGen.Init(SelNode.Data);
  frmViewGen.Show;
end;


procedure TfmMain.lmGetPackageUDRFunctionClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TSQLQuery;
    Node: TTreeNode;
begin
  Node := tvMain.Selected;
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := udb_package_udr_func_fetcher.GetPackageUDRFunctionDeclaration(Rec.IBConnection, Node.Text, Node.Parent.Parent.Text);
  ShowCompleteQueryWindow(dbIndex, 'Edit Package-UDR Function: ' + Node.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmGetPackageUDRProcedureClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
      TmpQuery: TSQLQuery;
      Node: TTreeNode;
begin
    Node := tvMain.Selected;
    //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
    dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
    Rec := RegisteredDatabases[dbIndex];
    TmpQueryStr := udb_package_udr_proc_fetcher.GetPackageUDRProcedureDeclaration(Rec.IBConnection, Node.Text, Node.Parent.Parent.Text);
    ShowCompleteQueryWindow(dbIndex, 'Edit Package UDR-Procedure: ' + Node.Text, TmpQueryStr, nil);
end;

(*******************  view Stored Procedure  ****************************)
procedure TfmMain.lmViewStoredProcedureClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AProcName: string;
  SPOwner: string;
  spBody: string;
  dbIndex: Integer;
  ATab: TTabSheet;
  Title: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AProcName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    SPBody:= GetStoredProcBody(dbIndex, AProcName, SPOwner);
    Title:= SelNode.Parent.Parent.Text +  ': StoredProcedure : ' + AProcName;
    // Fill SProc Parameters
    fmViewSProc:= FindCustomForm(Title, TfmViewSProc) as TfmViewSProc;
    if fmViewSProc = nil then
    begin
      fmViewSProc:= TfmViewSProc.Create(Application);
      ATab:= TTabSheet.Create(self);
      ATab.Parent:= PageControl1;
      fmViewSProc.Parent:= ATab;
      fmViewSProc.Left:= 0;
      fmViewSProc.Top:= 0;
      fmViewSProc.BorderStyle:= bsNone;
      fmViewSProc.Align:= alClient;
    end
    else
      ATab:= fmViewSProc.Parent as TTabSheet;
    PageControl1.ActivePage:= ATab;
    with fmViewSProc do
    begin
      SynSQLSyn1.TableNames.CommaText:= GetTableNames(dbIndex);
      Caption:= Title;
      ATab.Caption:= Caption;
      ATab.Tag:= dbIndex;
      edName.Caption:= AProcName;
      seScript.Lines.Clear;
      seScript.Lines.Add('create procedure ' + AProcName + '(');
      edOwner.Caption:= SPOwner;

      // Procedure body
      seScript.Lines.Text:= seScript.Lines.Text + spBody;

      fmViewSProc.Show;
    end; // with fmViewSProc
  end;
end;

(*******************  View Trigger   **********************)
procedure TfmMain.lmViewTriggerClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  ATriggerName, Event, BeforeAfter, OnTable, Body: string;
  TriggerEnabled: Boolean;
  TriggerPosition, dbIndex: Integer;
  ATab: TTabSheet;
  frmViewTrigger: TfmViewTrigger;
  ShortTitle, FullHint, DBAlias, EnabledText: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  // Trigger-Info aus DB holen
  ATriggerName := SelNode.Text;
  dmSysTables.GetTriggerInfo(dbIndex, ATriggerName, BeforeAfter, OnTable,
    Event, Body, TriggerEnabled, TriggerPosition);

  DBAlias := GetAncestorNodeText(SelNode, 1);

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmViewTrigger) then
    frmViewTrigger := TfmViewTrigger(NodeInfos^.ViewForm)
  else
  begin
    frmViewTrigger := TfmViewTrigger.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmViewTrigger.Parent := ATab;
    frmViewTrigger.Align := alClient;
    frmViewTrigger.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmViewTrigger;
  end;

  // Tab vorbereiten
  ATab := frmViewTrigger.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Tab-Titel
  ShortTitle := ATriggerName;
  ATab.Caption := ShortTitle;
  frmViewTrigger.Caption := ShortTitle;

  // Detaillierte Infos als Hint
  if TriggerEnabled then
    EnabledText := 'Yes'
  else
    EnabledText := 'No';

  FullHint :=
      'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
      'DBAlias:  ' + DBAlias + sLineBreak +
      'DBPath:   ' + RegisteredDatabases[dbIndex].IBConnection.DatabaseName + sLineBreak +
      'Object type: Trigger' + sLineBreak +
      'Trigger name: ' + ATriggerName + sLineBreak +
      'On Table: ' + OnTable + sLineBreak +
      'Event: ' + Event + sLineBreak +
      'Type: ' + BeforeAfter + sLineBreak +
      'Position: ' + IntToStr(TriggerPosition) + sLineBreak +
      'Enabled: ' + EnabledText;


  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form mit Triggerdaten füllen
  with frmViewTrigger do
  begin
    edName.Caption := ATriggerName;
    edOnTable.Caption := OnTable;
    laEvent.Caption := Event;
    laType.Caption := BeforeAfter;
    laPos.Caption := IntToStr(TriggerPosition);
    seScript.Lines.Text := Body;

    if TriggerEnabled then
      laEnabled.Font.Color := clGreen
    else
      laEnabled.Font.Color := clRed;

    if TriggerEnabled then
      laEnabled.Caption := 'Yes'
    else
      laEnabled.Caption := 'No';
  end;

  frmViewTrigger.Init(SelNode.Data);
  frmViewTrigger.Show;
end;


(********  View UDF  **********)
procedure TfmMain.lmViewUDFClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AFuncName: string;
  ModuleName, EntryPoint: string;
  Params: string;
  ATab: TTabSheet;
  dbIndex: Integer;
  Title: string;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    AFuncName:= SelNode.Text;
    Title:= SelNode.Parent.Parent.Text + ': UDF: ' + AFuncName;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;

    if GetUDFInfo(dbIndex, AFuncName, ModuleName, EntryPoint, Params) then
    with fmUDFINfo do
    begin
      fmUDFInfo:= FindCustomForm(Title, TfmUDFInfo) as TfmUDFInfo;
      if fmUDFInfo = nil then
      begin
        fmUDFInfo:= TfmUDFInfo.Create(Application);
        fmUDFInfo.Caption:= Title;
        ATab:= TTabSheet.Create(self);
        ATab.Parent:= PageControl1;
        fmUDFInfo.Parent:= ATab;
        ATab.Tag:= dbIndex;
        fmUDFInfo.Left:= 0;
        fmUDFInfo.Top:= 0;
        fmUDFInfo.BorderStyle:= bsNone;
        fmUDFInfo.Align:= alClient;
      end
      else
        ATab:= fmUDFInfo.Parent as TTabSheet;

      PageControl1.ActivePage:= ATab;
      ATab.Caption:= Title;
      edName.Caption:= AFuncName;
      edModule.Caption:= ModuleName;
      edEntry.Caption:= EntryPoint;
      meBody.Clear;
      meBody.Lines.Add('Function ' + AFuncName + '(');
      meBody.Lines.Add(Params);
      fmUDFInfo.Show;
    end; // with fmUDFInfo
  end;
end;

procedure TfmMain.lmDropTableClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
  if MessageDlg('Are you sure you want to delete ' + SelNode.Text + ' permanently', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    // Move selection to tables above so object is not in use when deleting it
    SelNode.Collapse(true);
    SelNode.Parent.Selected:=true;
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, 'dropTable#:' + IntToStr(dbIndex) + SelNode.Text);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('DROP TABLE ' + SelNode.Text + ';');
    QWindow.Show;
  end;
end;

procedure TfmMain.lmRecalculateStatisticsClick(Sender: TObject);
var
  Message: string;
begin
  //Recalculate index statistics. May take a while for big dbs.
  Message:= '';
  Screen.Cursor:= crSQLWait;
  try
    try
      dmSysTables.RecalculateIndexStatistics(TPNodeInfos(tvMain.Selected.Data)^.dbIndex);
    except
      on E: Exception do
      begin
        Message:= E.Message
      end;
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
  if Message='' then
    ShowMessage('Recalculation of index statistics complete.')
  else
    ShowMessage('Error recalculating index statistics: '+Message);
end;

procedure TfmMain.mnEditorFontClick(Sender: TObject);
var
  configFile: TIniFile;
  configFilePath: String;

begin
     configFilePath:= ConcatPaths([ExtractFilePath(Application.ExeName), 'config.ini']);
     configFile:= TIniFile.Create(configFilePath);

     if editorFontDialog.Execute then
     begin
        configFile.WriteString('Editor Font', 'font_name', editorFontDialog.Font.Name);
        configFile.WriteInteger('Editor Font', 'font_size', editorFontDialog.Font.Size);

     end;
     configFile.Free;
end;

(********  Create new database  ********)
procedure TfmMain.mnCreateDBClick(Sender: TObject);
begin
  if fmCreateDB.ShowModal = mrOk then
    LoadRegisteredDatabases;
end;

(**********  Register New database  ***********)
procedure TfmMain.mnRegDBClick(Sender: TObject);
begin
  fmReg.NewReg:= True;
  fmReg.bbReg.Caption:= 'Register';
  if fmReg.ShowModal = mrOK then
  begin
    LoadRegisteredDatabases;
    fmReg.SaveRegistrations;
    LoadRegisteredDatabases;
  end;
end;

(**********  About  ****************)
procedure TfmMain.mnAboutClick(Sender: TObject);
begin
  fmAbout:= TfmAbout.Create(nil);
  fmAbout.Init;
  fmAbout.ShowModal;
  fmAbout.Free;
end;

(************* Edit Registration  *************)
procedure TfmMain.lmEditRegClick(Sender: TObject);
var
  Rec: TRegisteredDatabase;
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if SelNode <> nil then
  begin
    fmReg.NewReg:= False;
    fmReg.bbReg.Caption:= 'Save';
    //fmreg.RecPos:= RegisteredDatabases[PtrInt(SelNode.Data)].Index;
    fmreg.RecPos:= RegisteredDatabases[TPNodeInfos(SelNode.Data)^.dbIndex].Index;
    //Rec:= RegisteredDatabases[PtrInt(SelNode.Data)].OrigRegRec;
    Rec:= RegisteredDatabases[TPNodeInfos(SelNode.Data)^.dbIndex].OrigRegRec;
    fmReg.edDatabaseName.Text:= Rec.DatabaseName;
    fmReg.edTitle.Text:= Rec.Title;
    fmReg.edUserName.Text:= Rec.UserName;
    fmReg.edPassword.Text:= Rec.Password;
    fmReg.cbCharset.Text:= Rec.Charset;
    fmReg.edRole.Text:= Rec.Role;
    fmReg.cxSavePassword.Checked:= Rec.SavePassword;

    if fmReg.ShowModal = mrOK then
    begin
      LoadRegisteredDatabases;
      fmReg.SaveRegistrations;
      LoadRegisteredDatabases;
    end;
  end;
end;


(****************  Unregister database *************)
procedure TfmMain.lmUnregisterDatabaseClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) and (SelNode.Parent.Parent = nil) then
  if MessageDlg('Are you sure you want to Unregister this database', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    DeleteRegistration(RegisteredDatabases[TPNodeInfos(SelNode.Data)^.dbIndex].Index);
    LoadRegisteredDatabases;
  end;
  SelNode:= nil;
end;

(**********  View 1000 records  **************)
procedure TfmMain.lmViewFirst1000Click(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex,  SelNode.Text, SelNode.Data);
    QWindow.meQuery.Lines.Text:= 'select * from ' + SelNode.Text;
    QWindow.bbRunClick(nil);
    QWindow.Show;
  end;
end;

(***********  Create New Role   ************)

procedure TfmMain.lmNewRoleClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'Create new Role');
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE ROLE role_name;');
    QWindow.Show;
  end;
end;

procedure TfmMain.mnRestoreClick(Sender: TObject);
begin
  fmBackupRestore.Init('', '', '', '');
  fmBackupRestore.cbOperation.ItemIndex:= 1;
  fmBackupRestore.cbOperation.Enabled:= False;
  fmBackupRestore.meLog.Clear;
  fmBackupRestore.Show;
end;

procedure TfmMain.PageControl1CloseTabClicked(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to Application.ComponentCount - 1 do
    if Application.Components[i] is TfmQueryWindow then
    begin
      (Application.Components[i] as TfmQueryWindow).lmCloseTabClick(nil);
      Break;
    end;
end;

function IsExcluded(Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(FExcludeTabs) to High(FExcludeTabs) do
    if FExcludeTabs[i] = Index then
      Exit(True);
end;

procedure TfmMain.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ClickedIndex: Integer;
begin
  // Check on which tab the mouse was pressed
  ClickedIndex := PageControl1.IndexOfTabAt(X, Y);

  // If click is outside any tab or on excluded tab, block drag
  if (ClickedIndex = -1) or IsExcluded(ClickedIndex) then
  begin
    FClickedTabIndex := -1;
    PageControl1.Cursor := crNo;
    Exit;
  end;

  // Remember clicked tab
  FClickedTabIndex := ClickedIndex;

  // Change cursor to drag
  if Button = mbLeft then
    PageControl1.Cursor := crDrag;
end;

procedure TfmMain.PageControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  HoverTabIndex: Integer;
begin
  if ssLeft in Shift then
  begin
    // If no draggable tab is active, always forbidden
    if FClickedTabIndex = -1 then
    begin
      PageControl1.Cursor := crNo;
      Exit;
    end;

    // Check if the mouse is over a tab
    HoverTabIndex := PageControl1.IndexOfTabAt(X, Y);

    if (HoverTabIndex > -1) and not IsExcluded(HoverTabIndex) then
      PageControl1.Cursor := crDrag   // Allowed only on valid tabs
    else
      PageControl1.Cursor := crNo;    // Empty area or excluded tab → forbidden
  end
  else
    PageControl1.Cursor := crDefault;
end;

procedure TfmMain.PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
  ActiveTab: TTabSheet;
begin
  // Reset cursor
  PageControl1.Cursor := crDefault;

  // If no draggable tab is active, do nothing
  if FClickedTabIndex = -1 then Exit;

  // Check on which tab the mouse was released
  TabIndex := PageControl1.IndexOfTabAt(X, Y);
  if (TabIndex = -1) or (TabIndex = FClickedTabIndex) or IsExcluded(TabIndex) then
  begin
    FClickedTabIndex := -1;
    Exit;
  end;

  ActiveTab := PageControl1.Pages[FClickedTabIndex];

  // Move tab
  if Assigned(ActiveTab) then
    ActiveTab.PageIndex := TabIndex;

  FClickedTabIndex := -1;
end;

(*****************   Database Popup menu   ********************)
procedure TfmMain.pmDatabasePopup(Sender: TObject);
var
  SelNode: TTreeNode;
  Filter: Integer;
  i: Integer;
  ParentNodeText: string;
  NodeText: string;

begin
  SelNode:= tvMain.Selected;

  if SelNode <> nil then
  begin
    //ShowMessage(IntToStr(Ord(TPNodeInfos(SelNode.Data)^.ObjectType)));
    NodeText:= SelNode.Text;
    if Pos('(', NodeText) > 0 then
      NodeText:= Trim(Copy(NodeText, 1, Pos('(', NodeText) - 1));

    ParentNodeText:= '';
    if SelNode.Parent <> nil then
      ParentNodeText:= SelNode.Parent.Text;
    if Pos('(', ParentNodeText) > 0 then
      ParentNodeText:= Trim(Copy(ParentNodeText, 1, Pos('(', ParentNodeText) - 1));

    if (SelNode <> nil) then
    if (SelNode.Parent = nil) then // Servers
      Filter:= -2
    else
    if (SelNode.Parent.Parent = nil) then // Database
      Filter:= 0
    else
    if ParentNodeText = 'Tables' then // Tables
      Filter:= 1
    else
    if ParentNodeText = 'Generators' then // Generators
      Filter:= 2
    else
    if ParentNodeText = 'Triggers' then // Triggers
      Filter:= 3
    else
    if ParentNodeText = 'Views' then // View
      Filter:= 4
    else
    if ParentNodeText = 'Procedures' then // Stored Proc
      Filter:= 5
    else
    if ParentNodeText = 'UDFs' then // UDF
      Filter:= 6
    else
    if ParentNodeText = 'System Tables' then // System Tables
      Filter:= 7
    else
    if ParentNodeText = 'Domains' then // Domains
      Filter:= 8
    else
    if ParentNodeText = 'Roles' then // Roles
      Filter:= 9
    else
    if ParentNodeText = 'Exceptions' then // Roles
      Filter:= 10
    else
    if ParentNodeText = 'Users' then // Users
      Filter:= 111
    else
    if NodeText = 'Tables' then // Tables root              //  Higher level (Roots)
      Filter:= 11
    else
    if NodeText = 'Generators' then // Generators root
      Filter:= 12
    else
    if (SelNode.Level = 2) and (NodeText = 'Procedures') then // Stored Proc root
      Filter:= 15
    else
    if NodeText = 'UDFs' then // UDF root
      Filter:= 16
    else
    if NodeText = 'Views' then // Views root
      Filter:= 14
    else
    if NodeText = 'Triggers' then // Triggers root
      Filter:= 13
    else
    if NodeText = 'Domains' then // Domains root
      Filter:= 18
    else
    if NodeText = 'Roles' then // Roles root
      Filter:= 19
    else
    if NodeText = 'Exceptions' then // Exceptions
      Filter:= 20
    else
    if NodeText = 'Users' then // Users
      Filter:= 21
    else
    if NodeText = 'Query Window' then // Query Window
      Filter:= 30
    else
    if (SelNode.Level = 2) and (NodeText = 'Functions') then //
      Filter:= 31
    else
    if (SelNode.Level = 3) and (ParentNodeText = 'Functions') then //
      Filter:= 311
    else
    ////////////////////////////////////////////////////////////
    //Packages
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageRoot then
      Filter:= 32
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackage then
      Filter:= 33
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageFunctionRoot then
      Filter:= 331
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageFunction then
      Filter:= 332
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageProcedureRoot then
      Filter:= 333
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageProcedure then
      Filter:= 334
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageUDRFunctionRoot then
      Filter:= 335
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageUDRFunction then
      Filter:= 336
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageUDRProcedureRoot then
      Filter:= 337
    else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotPackageUDRProcedure then
      Filter:= 338
    else

    /////////////////////////////////////////////////////////////
    //UDR's
    if NodeText = 'UDRs' then //
      Filter:= 42
   else
    if (TPNodeInfos(SelNode.Data)^.ObjectType = tvotUDRFunctionRoot)then
      filter := 43
    else
     if TPNodeInfos(SelNode.Data)^.ObjectType = tvotUDRFunction then
       filter := 44
  else
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotUDRProcedureRoot then
      filter := 45
   else
     if TPNodeInfos(SelNode.Data)^.ObjectType = tvotUDRProcedure then
       filter := 46

   else
      Filter:= -1;

    // Table Fields
    if (SelNode.Level = 4) then
    begin
      ParentNodeText:= SelNode.Parent.Parent.Text;
      if Pos('(', ParentNodeText) > 0 then
        ParentNodeText:= Trim(Copy(ParentNodeText, 1, Pos('(', ParentNodeText) - 1));
      if (ParentNodeText = 'Tables') then
        Filter:= 120
    end;

  end
  else
    Filter:= -1;

  // Show menu for specific filter
  for i:= 0 to pmDatabase.Items.Count - 1 do
    pmDatabase.Items[i].Visible:= (pmDatabase.Items[i].Tag = Filter) or
      ((pmDatabase.Items[i].Tag = 100) and (SelNode <> nil) and (SelNode.Parent <> nil) and
      (SelNode.Parent.Parent <> nil) and (SelNode.Parent.Parent.Parent = nil));

  SelNode:= nil;
end;

procedure TfmMain.tbCheckDBIntegrityClick(Sender: TObject);
var dbIndex: integer;
    Node: TTreeNode;
    fmCheckDBIntegrity: TfmCheckDBIntegrity;
begin
  if Length(RegisteredDatabases) > 0 then
  begin
    try
      Node := tvMain.Selected;
      if  Node <> nil then
      begin
        if (Node.Level = 0) then
        begin
          Node := Node.GetFirstChild;
        end;
        dbIndex := TPNodeInfos(Node.Data)^.dbIndex;
      end;

      fmCheckDBIntegrity := TfmCheckDBIntegrity.Create(self);
      fmCheckDBIntegrity.Init(RegisteredDatabases[dbIndex].RegRec.Title);
      fmCheckDBIntegrity.ShowModal;

    finally
      fmCheckDBIntegrity.Free;
    end;
  end;
end;

procedure TfmMain.tbSQLMonitorClick(Sender: TObject);
//var fmSQLMonitor: TfmSQLMonitor;
begin
  //fmSQLMonitor := TfmSQLMonitor.Create(self);
  fmSQLMonitor.Show;
end;

procedure TfmMain.tvMainAddition(Sender: TObject; Node: TTreeNode);
var PNodeInfos: TPNodeInfos; TmpNode: TTreeNode; TestStr: string;
begin
  if Node <> nil then
  begin
    new(PNodeInfos);
    PNodeInfos^.dbIndex := -1;
    PNodeInfos^.ObjectType := tvotNone;
    PNodeInfos^.PopupMenuTag := -1;
    PNodeInfos^.ImageIndex := -1;
    PNodeInfos^.ViewForm := nil;
    PNodeInfos^.EditorForm := nil;
    PNodeInfos^.NewForm := nil;
    PNodeInfos^.ExecuteForm := nil;
    PNodeInfos^.ServerSession := nil;

    if Node.Level = 0 then
      PNodeInfos^.ServerSession := TServerSession.Create(Node, '127.0.0.1');
  end;
  Node.Data := PNodeInfos;
end;

procedure TfmMain.tvMainClick(Sender: TObject);
var dbIndex: integer;
begin
  {if tvMain.Selected <> nil then
    if tvMain.Selected.Level > 0 then
    begin
      dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
      SetConnection(dbIndex);
    end;}
end;

procedure TfmMain.tvMainDeletion(Sender: TObject; Node: TTreeNode);
var
  Infos: TPNodeInfos;
  ServerSession: TServerSession;
begin
  if Node = nil then Exit;

  Infos := TPNodeInfos(Node.Data);
  if Assigned(Infos) then
  begin
    // Alle Form-Pointer auf nil setzen
    Infos^.ViewForm := nil;
    Infos^.EditorForm := nil;
    Infos^.NewForm := nil;
    Infos^.ExecuteForm := nil;

    // ServerSession nur freigeben, wenn sie existiert
    ServerSession := TServerSession(Infos^.ServerSession);
    if Assigned(ServerSession) then
    begin
      ServerSession.Free;
      Infos^.ServerSession := nil; // Dangling-Pointer vermeiden
    end;

    // TPNodeInfos freigeben
    Dispose(Infos);
  end;

  Node.Data := nil;
end;

(**************    Expanded     *****************)
{procedure TfmMain.tvMainExpanded(Sender: TObject; Node: TTreeNode);
var
  Rec: TRegisteredDatabase;
begin
  if (Node <> nil) then
  if (Node.Parent <> nil) and (Node.Parent.Parent = nil) then   // Expand database
  begin
    Rec:= RegisteredDatabases[TPNodeInfos(Node.Data)^.dbIndex].RegRec;
    RegisteredDatabases[TPNodeInfos(Node.Data)^.dbIndex].RegRec.LastOpened:= Now;
    RegisteredDatabases[TPNodeInfos(Node.Data)^.dbIndex].OrigRegRec.LastOpened:= Now;
    // Password form
    if Rec.Password = '' then
    if ConnectToDBAs(TPNodeInfos(Node.Data)^.dbIndex) then
      Node.Expand(False)
    else
      Node.Collapse(False);
  end
  else  // Expand objects root (Tables, Procedures, etc)
  if (Node.Parent <> nil) and (Node.Parent.Parent <> nil) and
     (Node.Parent.Parent.Parent = nil) and (not Node.Expanded) then
  begin
    if Node.HasChildren then
    begin
      Node.DeleteChildren;
      Node.Text:= Trim(Copy(Node.Text, 1, Pos('(', Node.Text) - 1));
    end;
    FillObjectRoot(Node);
  end;
end;}

procedure TfmMain.AddRootObjects(DBNode: TTreeNode; DBIndex: Integer);
var
  CNode: TTreeNode;
begin
  // Query Window
  CNode := tvMain.Items.AddChild(DBNode, 'Query Window');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotQueryWindow;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 1;
  CNode.SelectedIndex := 1;

  // Tables
  CNode := tvMain.Items.AddChild(DBNode, 'Tables');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotTableRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 2;
  CNode.SelectedIndex := 2;

  // Generators
  CNode := tvMain.Items.AddChild(DBNode, 'Generators');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotGeneratorRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 5;
  CNode.SelectedIndex := 5;

  // Triggers
  CNode := tvMain.Items.AddChild(DBNode, 'Triggers');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotTriggerRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 7;
  CNode.SelectedIndex := 7;

  // Views
  CNode := tvMain.Items.AddChild(DBNode, 'Views');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotViewRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 9;
  CNode.SelectedIndex := 9;

  // UDFs
  CNode := tvMain.Items.AddChild(DBNode, 'UDFs');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotUDFRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 13;
  CNode.SelectedIndex := 13;

  // Functions (FB3+)
  if FBVersionMajor >= 3 then
  begin
    CNode := tvMain.Items.AddChild(DBNode, 'Functions');
    TPNodeInfos(CNode.Data)^.ObjectType := tvotFunctionRoot;
    TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
    CNode.ImageIndex := 52;
    CNode.SelectedIndex := 52;
  end;

  // Procedures
  CNode := tvMain.Items.AddChild(DBNode, 'Procedures');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotStoredProcedureRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 11;
  CNode.SelectedIndex := 11;

  // Packages (FB3+)
  if FBVersionMajor >= 3 then
  begin
    CNode := tvMain.Items.AddChild(DBNode, 'Packages');
    TPNodeInfos(CNode.Data)^.ObjectType := tvotPackageRoot;
    TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
    CNode.ImageIndex := 60;
    CNode.SelectedIndex := 60;

    // Dummy-Knoten für Lazy Load der Sub-Objekte
    //tvMain.Items.AddChild(CNode, 'Loading...');
  end;

  // UDRs (FB3+)
  if FBVersionMajor >= 3 then
  begin
    CNode := tvMain.Items.AddChild(DBNode, 'UDRs');
    TPNodeInfos(CNode.Data)^.ObjectType := tvotUDRRoot;
    TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
    CNode.ImageIndex := 66;
    CNode.SelectedIndex := 66;

    //tvMain.Items.AddChild(CNode, 'Loading...');
  end;

  // System Tables
  CNode := tvMain.Items.AddChild(DBNode, 'System Tables');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotSystemTableRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 15;
  CNode.SelectedIndex := 15;

  // Domains
  CNode := tvMain.Items.AddChild(DBNode, 'Domains');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotDomainRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 17;
  CNode.SelectedIndex := 17;

  // Roles
  CNode := tvMain.Items.AddChild(DBNode, 'Roles');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotRoleRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 19;
  CNode.SelectedIndex := 19;

  // Exceptions
  CNode := tvMain.Items.AddChild(DBNode, 'Exceptions');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotExceptionRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 21;
  CNode.SelectedIndex := 21;

  // Users
  CNode := tvMain.Items.AddChild(DBNode, 'Users');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotUserRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := DBIndex;
  CNode.ImageIndex := 23;
  CNode.SelectedIndex := 23;
end;

// ============================================================================
// Called when the user tries to expand a database node (Level 1).
// Here we check the connection and optionally prevent expansion.
// ============================================================================
procedure TfmMain.tvMainExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  Rec: TRegisteredDatabase;
  NodeInfo: TPNodeInfos;
  ServerSession: TServerSession;
begin
  AllowExpansion := True;
  if (Node = nil) or (Node.Data = nil) then Exit;

  NodeInfo := Node.Data;

  case Node.Level of
    0: // Server
      begin
        ServerSession := TServerSession(NodeInfo^.ServerSession);
        if not ServerSession.Connected then
          if ServerSession.Login then
          begin
            ServerSession.FetchVersion;
            fbcommon.FBVersionMajor  := ServerSession.FBVersionMajor;
            fbcommon.FBVersionMinor  := ServerSession.FBVersionMinor;
            fbcommon.FBVersionString := ServerSession.FBVersionString;
          end;
      end;

    1: // DB-Knoten
      begin
        Rec := RegisteredDatabases[NodeInfo^.dbIndex].RegRec;
        EnsureCorrectClientLib(Rec.DatabaseName);

        if Rec.Password = '' then
          if not ConnectToDBAs(NodeInfo^.dbIndex) then
          begin
            AllowExpansion := False;
            Exit;
          end;

        RegisteredDatabases[NodeInfo^.dbIndex].RegRec.LastOpened := Now;
        RegisteredDatabases[NodeInfo^.dbIndex].OrigRegRec.LastOpened := Now;

        // Wenn noch Dummy-Knoten vorhanden → Root-Ordner erzeugen
        if (Node.Count = 0) then
        begin
          AddRootObjects(Node, NodeInfo^.dbIndex);
        end;
      end;

    else // Root-Ordner (Level > 1)
    begin
      if (Node.Count = 0) then
      begin
        FillObjectRoot(Node);
      end;
    end;
  end;
end;

// ============================================================================
// Called when a child node (e.g. Tables, Procedures) is expanded.
// Here we populate the database objects.
// ============================================================================
procedure TfmMain.tvMainExpanded(Sender: TObject; Node: TTreeNode);
begin
  if (Node = nil) or (Node.Level <= 1) then Exit;

  try
    if Node.Count = 0 then
      FillObjectRoot(Node);
  except
    on E: Exception do
      ShowMessage('Error while loading objects: ' + E.Message);
  end;
end;

(**********************            Double click        *********************************)
procedure TfmMain.tvMainDblClick(Sender: TObject);
var
  Node: TTreeNode;
  Info: TPNodeInfos;
  QWindow: TfmQueryWindow;
  DBIndex: Integer;
begin
  Node := tvMain.Selected;
  if (Node = nil) or (Node.Data = nil) then Exit;

  Info := TPNodeInfos(Node.Data);
  DBIndex := Info^.dbIndex;
  if (DBIndex < 0) or (DBIndex >= Length(RegisteredDatabases)) then Exit;

  // DB-Knoten doppelklick → nix tun (Expandieren über [+] oder Auto-Lazy-Load)
  if Info^.ObjectType = tvotDB then Exit;

  if Node.Level = 1 then
  begin
    //SetConnection(DBIndex);
  end;

  if Node.Level = 2 then
  begin
    try
      if tvMain.Selected.Text = 'Query Window' then
      begin
        QWindow:= ShowQueryWindow(TPNodeInfos(tvMain.Selected.Parent.Data)^.dbIndex, 'QW', tvMain.Selected.Data);
        QWindow.Show;
      end else // Expand object
      begin
        tvMainExpanded(nil, Node);
      end;
      Exit;
    except
      on E: Exception do
      ShowMessage(E.Message);
    end;
  end;


  try
    case Info^.ObjectType of

      // ----------------------
      // Database node
      // ----------------------
      tvotDB:
        begin
          // Do nothing on dblclick
        end;


      // ----------------------
      // Table / View / Procedure / Generator / Trigger / UDF / Function / System Table / Domain / Role / Exception / User
      // ----------------------
      tvotTable:
        begin
          lmViewFieldsClick(nil);
          lmViewFirst1000Click(nil);
        end;

      tvotTableField:
        begin
          lmEditFieldClick(nil);
        end;

      tvotGenerator:
        begin
          lmViewGenClick(nil);
        end;

      tvotTrigger:
        begin
          lmViewTriggerClick(nil);
        end;

      tvotView:
        begin
          lmDisplay1000VClick(nil);
        end;

      tvotStoredProcedure:
        begin
          lmCallStoreProcClick(nil);
        end;

       tvotUDFFunction:
       begin
          lmTestUDFFunctionClick(nil);
        end;

      tvotFunction:
        begin
          lmTestFireBirdFunctionClick(nil);
        end;

      tvotSystemTable:
        begin
          lmViewFieldsClick(nil);
          lmOpenSystemTableClick(nil);
        end;

      tvotDomain:
        begin
          lmViewDomainClick(nil);
        end;

      tvotRole:
        begin
          lmPermissionsClick(nil);
        end;

      tvotException:
        begin
          lmScriptExceptionClick(nil);
        end;

      tvotUser:
        begin
          lmPermissionsClick(nil);
        end;

      // ----------------------
      // UDRs
      // ----------------------
      tvotUDRFunction:
        begin
          lmTestUDRFunctionClick(nil);
        end;

      tvotUDRProcedure:
        begin
          lmTestUDRProcedureClick(nil);
        end;

      // ----------------------
      // Packages
      // ----------------------
      tvotPackageFunction:
        begin
          lmTestPackageFunctionClick(nil);
        end;

      tvotPackageProcedure:
        begin
          lmTestPackageProcedureClick(nil);
        end;

      tvotPackageUDRFunction:
        begin
          lmTestPackageUDRFunctionClick(nil);
        end;

      tvotPackageUDRProcedure:
        begin
          lmTestPackageUDRProcedureClick(nil);
        end;

      else
        begin
          // Unknown / do nothing
        end;

    end;
  except
    on E: Exception do
      ShowMessage('Error while handling node action: ' + E.Message);
  end;
end;

procedure TfmMain.GlobalException(Sender: TObject; E : Exception);
begin
  MessageDlg('Exception', e.Message, mtError, [mbOk], 0);
end;

procedure TfmMain.tvMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  P: TPoint;
begin
  // Kontextmenü mit Shift + F10 oder Menü-Taste (VK_APPS = $5D)
  if ((Key = VK_F10) and (ssShift in Shift)) or (Key = VK_APPS) then
  //if (Key = VK_F10)  and (ssShift in Shift) then
  begin
    // aktuelle Position des selektierten Knotens holen
    if Assigned(tvMain.Selected) then
    begin
      P := tvMain.Selected.DisplayRect(True).TopLeft;
      P := tvMain.ClientToScreen(P);
      // Kontextmenü anzeigen
      tvMain.PopupMenu.PopUp(P.X, P.Y);
      Key := 0; // Taste als verarbeitet markieren
    end;
  end;
end;

(**********************             Load databases            *********************************)
function TfmMain.LoadRegisteredDatabases: Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;
  ServerNode, MainNode: TTreeNode;
  i: Integer;
  AServerName: string;
begin
  try
    tvMain.Items.Clear;
    ReleaseRegisteredDatabases;

    FileName := getConfigurationDirectory + 'turbobird.reg';

    // Alte Konfigurationsdatei kopieren
    if not FileExists(FileName) and (FileExists(ChangeFileExt(ParamStr(0), '.reg'))) then
      CopyFile(ChangeFileExt(ParamStr(0), '.reg'), FileName);

    if FileExists(FileName) then
    begin
      AssignFile(F, FileName);
      Reset(F);
      i := 0;
      while not Eof(F) do
      begin
        Read(F, Rec);
        if not Rec.Deleted then
        begin
          SetLength(RegisteredDatabases, Length(RegisteredDatabases) + 1);
          with RegisteredDatabases[High(RegisteredDatabases)] do
          begin
            RegRec := Rec;
            OrigRegRec := Rec;
            Index := FilePos(F) - 1;
            IBConnection := TIBConnection.Create(nil);
            SQLTrans := TSQLTransaction.Create(nil);
            SetTransactionIsolation(SQLTrans.Params);
            IBConnection.Transaction := SQLTrans;
            SQLTrans.DataBase := IBConnection;
            IBConnection.DatabaseName := Rec.DatabaseName;
            IBConnection.UserName := Rec.UserName;
            IBConnection.Password := Rec.Password;
            IBConnection.Role := Rec.Role;
            IBConnection.CharSet := Rec.Charset;
          end;

          // Serverknoten erzeugen
          AServerName := GetServerName(Rec.DatabaseName);
          ServerNode := GetServerNameNode(AServerName);
          if ServerNode = nil then
          begin
            ServerNode := tvMain.Items.Add(nil, AServerName);
            ServerNode.ImageIndex := 25;
            ServerNode.SelectedIndex := 26;
            TPNodeInfos(ServerNode.Data)^.ObjectType := tvotServer;
          end;

          // DB-Knoten erzeugen (nur DB, keine Root-Ordner)
          MainNode := tvMain.Items.AddChild(ServerNode, Rec.Title);
          TPNodeInfos(MainNode.Data)^.ObjectType := tvotDB;
          TPNodeInfos(MainNode.Data)^.dbIndex := i;
          MainNode.ImageIndex := 0;
          MainNode.SelectedIndex := 3;

          tvMain.PopupMenu := pmDatabase;
          tbCheckDBIntegrity.Enabled := true;

          Inc(i);
        end;
      end;
      CloseFile(F);
    end;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

(**********************           Find QueryWindow                *********************************)

Function TfmMain.FindQueryWindow(ATitle: string): TComponent;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Application.ComponentCount- 1 do
    if Application.Components[i] is TfmQueryWindow then
      if (Application.Components[i] as TfmQueryWindow).Caption = ATitle then
        begin
          Result:= Application.Components[i];
          Break;
        end;
end;

(**********************   Find CustomForm   *********************************)

Function TfmMain.FindCustomForm(ATitle: string; AClass: TClass): TComponent;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Application.ComponentCount- 1 do
    if Application.Components[i] is AClass then
      if (Application.Components[i] as TForm).Caption = ATitle then
        begin
          Result:= Application.Components[i];
          Break;
        end;
end;

(****************  Delete Registration   *************************)

Function TfmMain.DeleteRegistration(Index: Integer): Boolean;
var
  F: file of TRegisteredDatabase;
  Rec: TRegisteredDatabase;
  FileName: string;
begin
  Result:= False;
  FileName:= getConfigurationDirectory + 'turbobird.reg';

  AssignFile(F, FileName);
  if FileExists(FileName) then
  begin
    FileMode:= 2;
    Reset(F);
    Seek(F, Index);
    Read(F, Rec);
    Rec.Deleted:= True;
    Seek(F, Index);
    Write(F, Rec);
    CloseFile(F);
    Result:= True;
  end;
end;

Function TfmMain.GetBlobSubTypeName(SubType: integer): string;
begin
  case SubType of
    //<0: user-defined
    0: Result:= 'SUB_TYPE BINARY';
    1: Result:= 'SUB_TYPE TEXT';
    2: Result:= 'SUB_TYPE BLR'; //(used for definitions of Firebird procedures, triggers, etc.
    //>2: reserved by Firebird
    else Result:= ''; //unknown
  end;
end;


(*******************  Get Primary Key fields  ************************)

Function TfmMain.GetPrimaryKeyIndexName(DatabaseIndex: Integer; ATableName: string; var ConstraintName: string): string;
begin
  SQLQuery1.Close;
  SetConnection(DatabaseIndex);
  SQLQuery1.SQL.Text:= 'select RDB$Index_name, RDB$Constraint_Name from RDB$RELATION_CONSTRAINTS ' +
    'where RDB$Relation_Name = ''' + UpperCase(ATableName) + ''' and RDB$Constraint_Type = ''PRIMARY KEY'' ';
  SQLQuery1.Open;
  //if SQLQuery1.RecordCount > 0 then
  if not SQLQuery1.IsEmpty then
  begin
    Result:= Trim(SQLQuery1.FieldByName('RDB$Index_name').AsString);
    ConstraintName:= Trim(SQLQuery1.FieldByName('RDB$Constraint_Name').AsString);
  end
  else
    Result:= '';
  SQLQuery1.Close;
end;

Function TfmMain.GetPrimaryKeyFields(DatabaseIndex: Integer;
  ATableName: string; var KeyFields: TStringList): boolean;
const
  // Select field(s) that make up primary key
  Template=' SELECT r.rdb$field_name ' +
    ' FROM RDB$RELATION_FIELDS r ' +
    ' LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
    ' LEFT JOIN RDB$INDEX_SEGMENTS s ON s.RDB$FIELD_NAME=r.RDB$FIELD_NAME ' +
    ' LEFT JOIN RDB$INDICES i ON i.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
    ' AND i.RDB$RELATION_NAME=r.RDB$RELATION_NAME ' +
    ' LEFT JOIN RDB$RELATION_CONSTRAINTS rc ON rc.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
    ' AND rc.RDB$INDEX_NAME = i.RDB$INDEX_NAME ' +
    ' AND rc.RDB$RELATION_NAME = i.RDB$RELATION_NAME ' +
    ' WHERE r.RDB$RELATION_NAME=''%s'' AND ' +
    ' rc.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ';
begin
  result:= false;
  KeyFields.Clear;
  SQLQuery1.Close;
  SetConnection(DatabaseIndex);
  SQLQuery1.SQL.Text:=format(Template,[UpperCase(ATableName)]);
  SQLQuery1.Open;
  while not(SQLQuery1.EOF) do
  begin
    KeyFields.Add(Trim(SQLQuery1.FieldByName('rdb$field_name').AsString));
    SQLQuery1.Next;
  end;
  SQLQuery1.Close;
  result:= true;
end;

(*********  Get constrain fields  *********)

Function TfmMain.GetConstraintFields(ATableName, AIndexName: string; var List: TStringList): Boolean;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= 'SELECT s.RDB$FIELD_NAME AS field_name ' +
     'FROM RDB$INDEX_SEGMENTS s ' +
     'LEFT JOIN RDB$INDICES i ON i.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS rc ON rc.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$REF_CONSTRAINTS refc ON rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME ' +
     'LEFT JOIN RDB$RELATION_CONSTRAINTS rc2 ON rc2.RDB$CONSTRAINT_NAME = refc.RDB$CONST_NAME_UQ ' +
     'LEFT JOIN RDB$INDICES i2 ON i2.RDB$INDEX_NAME = rc2.RDB$INDEX_NAME ' +
     'LEFT JOIN RDB$INDEX_SEGMENTS s2 ON i2.RDB$INDEX_NAME = s2.RDB$INDEX_NAME ' +
     '   WHERE i.RDB$RELATION_NAME=''' + UpperCase(ATableName) + '''  ' +
      'AND rc.RDB$INDEX_NAME=''' + UpperCase(AIndexName) + ''' ' +
      'AND rc.RDB$CONSTRAINT_TYPE IS NOT NULL ' +
      'ORDER BY s.RDB$FIELD_POSITION';
  List.Clear;
  SQLQuery1.Open;
  while not SQLQuery1.EOF do
  begin
    List.Add(Trim(SQLQuery1.Fields[0].AsString));
    SQLQuery1.Next;
  end;
  SQLQuery1.Close;
  Result:= List.Count > 0;
end;

(********  Get table names   ********)

Function TfmMain.GetTableNames(dbIndex: Integer): string;
var
  Count: Integer;
begin
  Result:= dmSysTables.GetDBObjectNames(dbIndex, otTables, Count);
end;

procedure TfmMain.SelectTreeViewNode(ARoutineInfo: TRoutineInfo);

  function MatchNode(Node: TTreeNode): Boolean;
  var
    Info: TPNodeInfos;
    NodeRoutineType: TRoutineType;
    ParentNode: TTreeNode;
  begin
    Result := False;

    if Node = nil then Exit;
    if Node.Data = nil then Exit;

    Info := TPNodeInfos(Node.Data);

    // ObjektTyp aus RoutineType
    NodeRoutineType := ARoutineInfo.RoutineType;
    Result := (Info^.dbIndex = ARoutineInfo.dbIndex) and
              (Info^.ObjectType = RoutineTypeToTreeViewObjectType(NodeRoutineType)) and
              SameText(Node.Text, ARoutineInfo.RoutineName);

    // Wenn Package verwendet wird, prüfen ob es auch in der Parent-Kette liegt
    if Result and (ARoutineInfo.PackageName <> '') then
    begin
      ParentNode := Node.Parent;
      Result := False;
      while ParentNode <> nil do
      begin
        if SameText(ParentNode.Text, ARoutineInfo.PackageName) then
        begin
          Result := True;
          Break;
        end;
        ParentNode := ParentNode.Parent;
      end;
    end;
  end;

  function FindAndSelect(Node: TTreeNode): Boolean;
  begin
    Result := False;
    while Node <> nil do
    begin
      if MatchNode(Node) then
      begin
        tvMain.Selected := Node;
        tvMain.SetFocus;
        Result := True;
        Exit;
      end;

      if Node.HasChildren then
      begin
        if FindAndSelect(Node.GetFirstChild) then
        begin
          Result := True;
          Exit;
        end;
      end;

      Node := Node.GetNextSibling;
    end;
  end;

begin
  if tvMain.Items.Count = 0 then Exit;
  FindAndSelect(tvMain.Items[0]); // Start vom ersten Node
end;


initialization
  {$I main.lrs}

end.

