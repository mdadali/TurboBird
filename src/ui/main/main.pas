unit main;

{ Main TurboBird form
If you want to add popup menus for tables, views etc, please set the Tag
property of the popup menu to the right value.
}

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, Classes, SysUtils, IBConnection, sqldb, sqldblib, odbcconn,
  memds, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Reg, QueryWindow, Grids, ExtCtrls, Buttons, StdCtrls, TableManage,
  dbugintf, turbocommon, importtable, DB,  HtmlView, FramView, FramBrwz,
  IniFiles, Types, fSetFBClient, fTestFunction, fCheckDBIntegrity,
  fFirebirdConfig, fsqlmonitor,

  ibase60dyn,
  IB,
  IBDatabase,
  IBQuery,
  IBDatabaseInfo,

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

  fServerSession,
  floginservicemanager,
  fserverregistry,

  fblobedit, HTMLUn2, HtmlGlobals,

  fmetaquerys,
  uthemeselector,

  //MWA Tools
  fScriptEngine,
  DataModule,
  MainFormUnit, //DBAdmin
  BackupDlgUnit,
  RestoreDlgUnit
  ;

{$i turbocommon.inc}

type

  { TfmMain }

  TfmMain = class(TForm)
    Button1: TButton;
    editorFontDialog: TFontDialog;
    CurrentIBConnection: TIBDatabase;
    CurrentIBTransaction: TIBTransaction;
    HtmlViewer1: THtmlViewer;
    Image1: TImage;
    Memo1: TMemo;
    lmMaintenance: TMenuItem;
    lmBackupNew: TMenuItem;
    lmRestoreNew: TMenuItem;
    lmScriptDB: TMenuItem;
    lmRecalculateStatistics: TMenuItem;
    lmDBAdmin: TMenuItem;
    lmDBInfo: TMenuItem;
    lmBackup: TMenuItem;
    lmSweep: TMenuItem;
    lmCompare: TMenuItem;
    lmScriptEngine: TMenuItem;
    mnTheme: TMenuItem;
    mnServerRegistry: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    SQLQuery1: TIBQuery;
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
    lmEditException: TMenuItem;
    lmServerRegistry: TMenuItem;
    mnOptions: TMenuItem;
    mnEditorFont: TMenuItem;
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
    lmCopyRolePermission: TMenuItem;
    lmGetIncrementGen: TMenuItem;
    lmDropTable: TMenuItem;
    mnuImport: TMenuItem;
    mnExit: TMenuItem;
    mnCreateDB: TMenuItem;
    mnRegDB: TMenuItem;
    mnHelp: TMenuItem;
    mnAbout: TMenuItem;
    lmEditDBReg: TMenuItem;
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
    lmConnectAs: TMenuItem;
    lmPermissions: TMenuItem;
    lmRolePermissions: TMenuItem;
    lmTableManage: TMenuItem;
    lmSeparator2: TMenuItem;
    mnRestore: TMenuItem;
    pmDatabase: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    tbtnCreateNewDB: TToolButton;
    tbtnRegDatabase: TToolButton;
    tbtnRestoreDatabase: TToolButton;
    tbtnAbout: TToolButton;
    tbCheckDBIntegrity: TToolButton;
    tbSQLMonitor: TToolButton;
    ToolButton3: TToolButton;
    tbtnEditorFont: TToolButton;
    tsMain: TTabSheet;
    tvMain: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HtmlViewer1Link(Sender: TObject; const Rel, Rev, Href: ThtString);
    procedure ImNewFBFunctionClick(Sender: TObject);
    procedure ImCreateNewPackageClick(Sender: TObject);
    procedure ImEditFBFunctionClick(Sender: TObject);
    procedure lmAddUserClick(Sender: TObject);
    procedure lmBackupClick(Sender: TObject);
    procedure lmBackupNewClick(Sender: TObject);
    procedure lmBlobEditorClick(Sender: TObject);
    procedure lmChangePasswordClick(Sender: TObject);
    procedure lmCompareClick(Sender: TObject);
    procedure lmCopyRolePermissionClick(Sender: TObject);
    procedure lmCopyUserPermissionClick(Sender: TObject);
    procedure lmCopyTableClick(Sender: TObject);
    procedure lmCreateDBClick(Sender: TObject);
    procedure lmDBAdminClick(Sender: TObject);
    procedure lmDBInfoClick(Sender: TObject);
    procedure lmDisconnectAllClick(Sender: TObject);
    procedure lmDisconnectClick(Sender: TObject);
    procedure lmDropDomainClick(Sender: TObject);
    procedure lmDropFireBirdFunctionClick(Sender: TObject);
    procedure lmDropGeneratorClick(Sender: TObject);
    procedure lmDropPackageClick(Sender: TObject);
    procedure lmDropPackageProcedureClick(Sender: TObject);
    procedure lmDropStoredProcedureClick(Sender: TObject);
    procedure lmDropTriggerClick(Sender: TObject);
    procedure lmDropUDFFunctionClick(Sender: TObject);
    procedure lmDropUDRFunctionClick(Sender: TObject);
    procedure lmDropUDRProcedureClick(Sender: TObject);
    procedure lmDropUserClick(Sender: TObject);
    procedure lmDropViewClick(Sender: TObject);
    procedure lmEditExceptionClick(Sender: TObject);
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
    procedure lmRestoreNewClick(Sender: TObject);
    procedure lmRolePerManagementClick(Sender: TObject);
    procedure lmRolePermissionsClick(Sender: TObject);
    procedure lmScriptDatabaseClick(Sender: TObject);
    procedure lmScriptEngineClick(Sender: TObject);
    procedure lmScriptExceptionClick(Sender: TObject);
    procedure lmScriptInsertClick(Sender: TObject);
    procedure lmScriptTableCreateClick(Sender: TObject);
    procedure lmScriptUpdateClick(Sender: TObject);
    procedure lmServerRegistryClick(Sender: TObject);
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
    procedure lmEditDBRegClick(Sender: TObject);
    procedure lmUnregisterDatabaseClick(Sender: TObject);
    procedure lmViewFirst1000Click(Sender: TObject);
    procedure lmNewRoleClick(Sender: TObject);
    procedure mnRestoreClick(Sender: TObject);
    procedure mnServerRegistryClick(Sender: TObject);
    procedure mnThemeClick(Sender: TObject);
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
    procedure tvMainChange(Sender: TObject; Node: TTreeNode);
    procedure tvMainClick(Sender: TObject);
    procedure tvMainDblClick(Sender: TObject);
    procedure tvMainDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvMainExpanded(Sender: TObject; Node: TTreeNode);
    procedure GlobalException(Sender: TObject; E : Exception);
    procedure tvMainExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FCurrentHistoryFile: string;
    FActivated: Boolean;
    Function FindCustomForm(ATitle: string; AClass: TClass): TComponent;
    // Show new generator form
    procedure InitNewGen(DatabaseIndex: Integer);

    procedure ReleaseRegisteredDatabase(dbIndex: Integer);
    procedure ReleaseRegisteredDatabases;

    // Set connection for SQLQuery1 to selected registered database
    procedure SetConnection(Index: Integer);
    procedure SetFocus; override; // solve a bug in Lazarus
    procedure AddRootObjects(ANode: TTreeNode; AServerVersion: word);
  protected
    // This procedure will receive the events that are logged by the connection:
    procedure GetLogEvent(Sender: TSQLConnection; EventType: TDBEventType; Const Msg : String);
  public
    Version: string;
    VersionDate: string;
    Major, Minor, ReleaseVersion: word;
    procedure AppShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    Function RetrieveInputParamFromSP(Body: string): string;
    function LoadRegisteredServers: Boolean;
    // Load registered databases from file and show them in treeview
    Function LoadRegisteredDatabases: Boolean;
    Function FindQueryWindow(ATitle: string): TComponent;
    // Returns BLOB subtype clause depending on subtype
    Function GetBlobSubTypeName(SubType: integer): string;
    // Get name of index used for primary key
    // Also returns name of constraint used
    // Get primary key field(s) names into KeyFields
    Function GetPrimaryKeyFields(DatabaseIndex: Integer; ATableName: string; var KeyFields: TStringList): boolean;
    Function GetConstraintFields(ATableName, AIndexName: string; var List: TStringList): Boolean;
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
   // Function GetIndices(ATableName: string; AQuery: TIBQuery): Boolean;
    Function GetUDFInfo(DatabaseIndex: Integer; UDFName: string; var ModuleName, EntryPoint, Params: string): Boolean;
    Function ShowQueryWindow(DatabaseIndex: Integer; ATitle: string; ANodeInfos: TPNodeInfos=nil): TfmQueryWindow;
    procedure FillObjectRoot(Node: TTreeNode);
    procedure ShowCompleteQueryWindow(DatabaseIndex: Integer; ATitle,
      AQueryText: string; OnCommitProcedure: TNotifyEvent = nil; ANodeInfos: TPNodeInfos=nil);
    Function GetTableNames(dbIndex: Integer): string;
    Function CreateNewTrigger(dbIndex: Integer; ATableName: string; OnCommitProcedure: TNotifyEvent = nil): Boolean;
    Function AddToSQLHistory(DatabaseTitle: string; SQLType, SQLStatement: string): Boolean;
    Function SaveAndCloseSQLHistory: Boolean;
    Function OpenSQLHistory(DatabaseTitle: string): Boolean;
    // Connects to database.
    // If not succesful (or if ForceConnectDialog is true), ask user for credentials and try again
    Function IsLinux: Boolean;
    Function IsWindows: Boolean;
    Function IsUnix: Boolean;
    Function Is64bit: Boolean;
    Function Is32bit: Boolean;
    procedure CallRoutine(ARoutineType: TRoutineType);
    procedure SelectTreeViewNode(ARoutineInfo: TRoutineInfo);
    function  CheckActiveTransaction(AIBTransaction: TIBTransaction; ADefaultCommitKind: TCommitKind; ASilent: Boolean): Boolean;

    function CheckActiveTransActions(dbIndex: Integer;ADefaultCommitKind: TCommitKind; ASilent: Boolean): Boolean;
    procedure CloseDB(dbIndex: Integer; Silent: Boolean = True);
    procedure CheckOpenDBsBeforeClientSwap(TargetDB: string);
    function  ReadODSViaSQL(dbIndex: Integer; out ODSMajor, ODSMinor: Integer): Boolean;
    function  TryReadODS(dbIndex: Integer; out ODSMajor, ODSMinor: Integer): Boolean;
    function  GetODSVersion(dbIndex: Integer; out ODSMajor, ODSMinor: Integer): Boolean;
    function  GetServerLoginDlg(AserverName: string): TfrmLoginServiceManager;
    function  ConnectToServiceManager(ServerSession: TServerSession): boolean;
    function  ConnectEmbedded(ServerSession: TServerSession): Boolean;

    procedure OnIBConnectionLogin(Database: TIBDatabase; LoginParams: TStrings);
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
var htmlPath: string;
begin
  turbocommon.MainTreeView := tvMain;

  {$IFNDEF DEBUG}
  // Do not log to debug server if built as release instead of debug
  SetDebuggingEnabled(false);
  {$ENDIF}
  Application.OnException:= @GlobalException;
  FActivated:= False;
  //LoadRegisteredServers;
  LoadRegisteredDatabases;
  StatusBar1.Panels[0].Text:= 'TurboBird for ' + Target + '-' + Arch;
  Application.OnShowHint := @AppShowHint;
  PageControl1.ShowHint := True;
  Application.ShowHint := True;
  NoDragTab := 0;
  SetLength(FExcludeTabs, 1);
  FExcludeTabs[0] := 0;

  htmlPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '/help/index.html';

  if FileExists(htmlPath) then
    HtmlViewer1.LoadFromFile(htmlPath)
  else
    HtmlViewer1.LoadFromString('<html><body><h1>index.html not found</h1></body></html>');
end;

procedure TfmMain.FormShow(Sender: TObject);
var frmThemeSelectorLocal: TfrmThemeSelector;
begin
  frmThemeSelectorLocal := TfrmThemeSelector.Create(self);

  frmThemeSelectorLocal.btnApplyClick(fmMain);
  frmThemeSelectorLocal.Free;
  //Repaint;
end;

procedure TfmMain.HtmlViewer1Link(Sender: TObject; const Rel, Rev, Href: ThtString);
var htmlPath: string;
begin
  // Lokale Datei: ohne http/https → load in THtmlViewer
  htmlPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'index.html';
  if (Pos('http://', Href) = 0) and (Pos('https://', Href) = 0) then
  begin
    if FileExists(htmlPath + Href) then
      HtmlViewer1.LoadFromFile(htmlPath + Href)
    else
      HtmlViewer1.LoadFromString('<html><body><h1>File not found: ' + Href + '</h1></body></html>');
  end
  else
  begin
    // Externe Links öffnen im Standardbrowser
    OpenURL(Href);
  end;
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

  // Prüfen ob Maus auf einem Tab-Reiter ist
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    r := PageControl1.TabRect(i);
    if PtInRect(r, p) then
    begin
      ts := PageControl1.Pages[i];
      HintStr := ts.Hint;
      CanShow := (HintStr <> '');
      Exit;
    end;
  end;

  // Prüfen ob Maus im PageControl-Bereich liegt (aber nicht auf einem Tab)
  if PtInRect(PageControl1.ClientRect, p) then
  begin
    // => keine Hints auf der Tab-Fläche
    HintStr := '';
    CanShow := False;
    Exit;
  end;

  // Sonst Standard-Hint anzeigen (z. B. für Toolbars, TreeView etc.)
  HintStr := HintInfo.HintStr;
  CanShow := (HintStr <> '');
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    //if Length(RegisteredDatabases) > 0 then
      //fmReg.SaveRegistrations;
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

procedure TfmMain.ImNewFBFunctionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'New Function#' + IntToStr(dbIndex));

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
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  dbIndex := TPNodeInfos(SelNode.Parent.Data)^.dbIndex;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(dbIndex, 'New Package#' + IntToStr(dbIndex));
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
    TmpQuery: TIBQuery;
begin
  dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetFirebirdFunctionDeclaration(Rec.IBDatabase, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit Function#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

{
  Why do we create a dummy role?

  In Firebird versions prior to 3 there is no unified view of all users inside
  a regular database. User information is stored in the separate security
  database, which would require an extra connection. Unlike Firebird 3+,
  there is no direct query like "RDB$USERS" available.

  This causes a problem:
  - In Firebird < 3, users are only visible in the system tables if they
    already have at least one privilege or role assigned.
  - A newly created user without any rights would therefore NOT appear in
    the user list of our tool.

  To work around this limitation, the tool automatically creates a special
  "dummy role" (if it does not already exist) and assigns it to each newly
  created user. This ensures that:
    1. All users become visible immediately, even if they do not yet
       have any real privileges.
    2. We do not need to establish a separate connection to the security
       database.
    3. The solution stays fully compatible with Firebird < 3.

  Starting with Firebird 3 this workaround is no longer required, since
  all users can be queried directly via RDB$USERS.
}
(*****************  Add New user  ***********************)
procedure TfmMain.lmAddUserClick(Sender: TObject);
var
  SelNode: TTreeNode;
  dbIndex: Integer;
  DummyCreated, DummyJustCreated: Boolean;
  ServerVersionMajor: word;
begin
  with fmCreateUser do
  try
    SelNode := tvMain.Selected;
    dbIndex := TPNodeInfos(SelNode.Parent.Data)^.dbIndex;
    Init(dbIndex);


    edUserName.Clear;
    edPassword.Clear;

    if ShowModal = mrOK then
    begin
      // Create user
      dmSysTables.Init(dbIndex);
      dmSysTables.sqQuery.Close;
      dmSysTables.sqQuery.SQL.Text := 'CREATE USER "' + edUserName.Text + '" PASSWORD ' + QuotedStr(edPassword.Text);
      if not dmSysTables.sqQuery.Transaction.InTransaction then
        dmSysTables.sqQuery.Transaction.StartTransaction;
      dmSysTables.sqQuery.ExecSQL;

      DummyCreated := False;
      DummyJustCreated := False;

      // FB 2.5: Dummy-Rolle prüfen / anlegen und ggf. zuweisen
      ServerVersionMajor := GetServerMajorVersionFromDBIndex(dbIndex);
      if ServerVersionMajor < 3 then
      begin
        if not dmSysTables.DummyRoleExists then
        begin
          // Dummy-Rolle existiert nicht → erstellen
          if dmSysTables.EnsureDummyRole then
          begin
            DummyJustCreated := True;
            DummyCreated := True;
          end
          else
          begin
            ShowMessage('Error creating the Dummy role. The new user will still be created, but may not appear in the user list.');
          end;
        end
        else
          DummyCreated := True;

        // Dummy-Rolle zuweisen
        if DummyCreated then
        begin
          dmSysTables.sqQuery.Close;
          dmSysTables.sqQuery.SQL.Text := 'GRANT DUMMYROLE TO "' + edUserName.Text + '"';
          dmSysTables.sqQuery.ExecSQL;

          if DummyJustCreated then
            ShowMessage('A Dummy role has been created and automatically assigned to the new user because you are working with Firebird version < 3. ' +
                        'Without this Dummy role, a user without any other rights would not appear in the user list.')
          else
            ShowMessage('The Dummy role already exists and has been automatically assigned to the new user. ' +
                        'This ensures that the user is visible in the user list while working with Firebird version < 3.');
        end;
      end;

      // Grant role, falls ausgewählt
      if cxGrantRole.Checked and (Trim(cbRoles.Text) <> '') then
      begin
        dmSysTables.sqQuery.Close;
        dmSysTables.sqQuery.SQL.Text := 'GRANT "' + cbRoles.Text + '" TO "' + edUserName.Text + '"';
        dmSysTables.sqQuery.ExecSQL;
      end;

      dmSysTables.stTrans.CommitRetaining;

      MessageDlg('New user (' + edUserName.Text + ') has been created successfully', mtInformation, [mbOk], 0);

      lmRefresh.Click;
    end;
  except
    on E: Exception do
      MessageDlg('Error while creating new user: ' + E.Message, mtError, [mbOk], 0);
  end;
end;

{procedure TfmMain.mnRestoreClick(Sender: TObject);
begin
  fmBackupRestore.Init('', '', '', '');
  fmBackupRestore.cbOperation.ItemIndex:= 1;
  fmBackupRestore.cbOperation.Enabled:= False;
  fmBackupRestore.meLog.Clear;
  fmBackupRestore.Show;
end;}


procedure TfmMain.mnServerRegistryClick(Sender: TObject);
begin
  lmServerRegistryClick(nil);
end;

procedure TfmMain.mnThemeClick(Sender: TObject);
begin
  frmThemeSelector.ShowModal;
end;

procedure TfmMain.lmRestoreClick(Sender: TObject);
begin
  mnRestoreClick(nil);
end;

procedure TfmMain.lmBackupClick(Sender: TObject);
var
  fmBackupRestore: TfmBackupRestore;
  SelNode: TTreeNode;
  ServerNode: TTreeNode;
  hStr: string;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  Title, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Data = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  Title := 'Backup/Restore: ' + SelNode.Text;

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmBackupRestore) then
    fmBackupRestore := TfmBackupRestore(NodeInfos^.ViewForm)
  else
  begin
    fmBackupRestore := TfmBackupRestore.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    fmBackupRestore.Parent := ATab;
    fmBackupRestore.Align := alClient;
    fmBackupRestore.BorderStyle := bsNone;

    NodeInfos^.ViewForm := fmBackupRestore;
  end;

  // Tab vorbereiten
  ATab := fmBackupRestore.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Tab-Titel
  ATab.Caption := Title;
  fmBackupRestore.Caption := Title;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Database' + sLineBreak +
    'Action: Backup/Restore';
  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form initialisieren
  if Sender = lmRestore then
  begin
    fmBackupRestore.Init('', tvMain.Selected.Text +  ':', '', '', NodeInfos);
    fmBackupRestore.cbOperation.ItemIndex:= 1;
    fmBackupRestore.cbOperation.Enabled:= False;
  end else
  begin
    with RegisteredDatabases[dbIndex].RegRec do
      fmBackupRestore.Init(SelNode.Text, DatabaseName, UserName, Password, NodeInfos);
    fmBackupRestore.cbOperation.Enabled := True;
  end;

  //fmBackupRestore.cmbBoxHostList.Items := GetServerListFromTreeView;
  fmBackupRestore.cmbBoxHostList.Items := GetServerAndPortListFromTreeView;

  //if fmBackupRestore.cmbBoxHostList.Items.Count = 0 then
    //fmBackupRestore.cmbBoxHostList.Items.Add('localhost');

  ServerNode := SelNode.Parent;
  hStr := Trim(ServerNode.Text);

  if not TPNodeInfos(ServerNode.Data)^.ServerSession.IsEmbedded then
    hStr := hStr + '/' + TPNodeInfos(ServerNode.Data)^.ServerSession.Port;

  fmBackupRestore.cmbBoxHostList.ItemIndex := fmBackupRestore.cmbBoxHostList.Items.IndexOf(hStr);

  if not TPNodeInfos(ServerNode.Data)^.ServerSession.Connected then
  begin
    TPNodeInfos(ServerNode.Data)^.ServerSession.LoadRegisteredClientLib := true;
    TPNodeInfos(ServerNode.Data)^.ServerSession.IBXConnect;
  end;
  fmBackupRestore.Show;
end;

procedure TfmMain.lmBackupNewClick(Sender: TObject);
var
  TmpBackupDlg: TBackupDlg;
  ServerRec: TServerRecord;
  DBRec: TDatabaseRec;
  dbIndex: Word;
  isDbConnected: Boolean;
  TmpIniFile: TIniFile;
  ServerErrStr: string;
  BackupFileName: string;
  TS: string;
begin
  if tvMain.Items.Count = 0 then exit;
  if tvMain.Selected = nil then exit;
  if tvMain.Selected.Level <> 1 then exit;

  if not IsServerReachable(tvMain.Selected.Parent.Text, ServerErrStr) then
  begin
    MessageDlg(ServerErrStr, mtError, [mbOK], 0);
    Exit;
  end;

  dbIndex := TPNodeInfos(tvMain.Selected.Data)^.dbIndex;

  isDbConnected := RegisteredDatabases[dbIndex].IBDatabase.Connected;

  DBRec := RegisteredDatabases[dbIndex];
  ServerRec := GetServerRecordFromFileByName(DBRec.RegRec.ServerName);

  try
    TmpIniFile :=  TIniFile.Create(fIniFileName, []);
    CloseDBBeforeBackup :=  TmpIniFile.ReadBool('Backup',  'CloseDBBeforeBackup', true);

    if  CloseDBBeforeBackup then
      if isDbConnected then
        CloseDB(dbIndex);

    TmpBackupDlg := TBackupDlg.Create(Self);

    try
      if TmpBackupDlg.IBXServicesConnection1.Connected then
        TmpBackupDlg.IBXServicesConnection1.Connected := False;

      TmpBackupDlg.IBXServicesConnection1.ServerName := ServerRec.ServerName;
      TmpBackupDlg.IBXServicesConnection1.PortNo := ServerRec.Port;
      TmpBackupDlg.IBXServicesConnection1.Protocol := ServerRec.Protocol;

      TmpBackupDlg.IBXServicesConnection1.Params.Clear;
      TmpBackupDlg.IBXServicesConnection1.Params.Add('user_name=' + ServerRec.UserName);
      TmpBackupDlg.IBXServicesConnection1.Params.Add('password=' + ServerRec.Password);
      TmpBackupDlg.IBXServicesConnection1.LoginPrompt := False;

      TmpBackupDlg.IBXServicesConnection1.Connected := True;
      TmpBackupDlg.IBXClientSideBackupService1.DatabaseName := DBRec.RegRec.DatabaseName;
      TmpBackupDlg.IBXServerSideBackupService1.DatabaseName := DBRec.RegRec.DatabaseName;

      BackupFileName := GetDBFileNameFromConnectionString(DBRec.RegRec.DatabaseName);
      BackupFileName := ChangeFileExt(BackupFileName, '');
      TS := FormatDateTime('yyyy_mm_dd_hh_nn_ss', Now);
      BackupFileName := BackupFileName + '_' + TS;

      BackupFileName := ChangeFileExt(BackupFileName, '.gbk');
      TmpBackupDlg.BackupFileName.Text := BackupFileName;

      TmpBackupDlg.ShowModal;
    except
      on E: Exception do
        ShowMessage('Backup-Error: ' + E.Message);
    end;

  finally
    TmpIniFile.Free;
    TmpBackupDlg.Free;

    if isDbConnected and (not RegisteredDatabases[dbIndex].IBDatabase.Connected) then
      RegisteredDatabases[dbIndex].IBDatabase.Connected := True;
  end;
end;

procedure TfmMain.mnRestoreClick(Sender: TObject);
var TmpRestoreDlg: TRestoreDlg;
    ServerName, DatabaseName: string;
    SelNode, ServerNode, DBNode: TTreeNode;
    dbIndex: integer;
    ServerErrStr: string;

    ServerRec: TServerRecord;
    TmpModalResult: TModalResult;
    cboxItems: TStringList;
    isDbConnected: boolean;
begin
  if tvMain.Items.Count = 0 then
    exit;

  DBNode := nil;
  isDbConnected := false;

  SelNode := tvMain.Selected;

  if SelNode = nil then
  begin
    ShowMessage('Please select a server first before starting the restore operation');
    exit;
  end;

  ServerNode := turbocommon.GetAncestorAtLevel(SelNode, 0);

  ServerName := ServerNode.Text;

  if not IsServerReachable(ServerName, ServerErrStr) then
  begin
    MessageDlg(ServerErrStr, mtError, [mbOK], 0);
    Exit;
  end;

  if SelNode.Level > 0 then
    DBNode := turbocommon.GetAncestorAtLevel(SelNode, 1);

  if DBNode <> nil then
  begin
    dbIndex := TPNodeInfos(DBNode.Data)^.dbIndex;
    DatabaseName := GetDBFileNameFromConnectionString(RegisteredDatabases[dbIndex].IBDatabase.DatabaseName);
    isDbConnected := RegisteredDatabases[dbIndex].IBDatabase.Connected;
    if isDbConnected then
      CloseDB(dbIndex);
  end else
    DatabaseName := 'RestoredDB_at_' + FormatDateTime('yyyy_mm_dd_hh_nn_ss', Now) + '.fdb';

  TmpRestoreDlg := TRestoreDlg.Create(self);
  TmpRestoreDlg.Init(ServerName, DatabaseName, DefaultPageSize, DefaultNumBuffers);

  TmpModalResult := TmpRestoreDlg.ShowModal(DefaultPageSize, DefaultNumBuffers);

  if not RegisterDBAfterRestore then exit;

  if  TmpModalResult = mrOK then
  begin
    ServerRec := GetServerRecordFromFileByName(ServerName);
    if DBNode = nil then
    begin
      try
        cboxItems := GetServerListFromTreeView;
        fmReg.cboxServers.Items.Assign(cboxItems);

        fmReg.cboxServers.ItemIndex := fmReg.cboxServers.Items.IndexOf(ServerName);

        if ServerRec.IsEmbedded then
          fmReg.edDatabaseName.Text := TmpRestoreDlg.DBName.Text
        else
          fmReg.edDatabaseName.Text := MakeConnectionString(ServerRec.ServerName, ServerRec.Port, TmpRestoreDlg.DBName.Text);

        fmReg.edTitle.Text :=  ChangeFileExt(ExtractFileName(TmpRestoreDlg.DBName.Text), '');

        fmReg.edtPort.Text := ServerRec.Port;
        fmReg.cboxSQLDialect.ItemIndex := 2;
        fmReg.cbCharset.ItemIndex := fmReg.cbCharset.Items.IndexOf(ServerRec.Charset);
        fmReg.edUserName.Text := ServerRec.UserName;
        fmReg.edPassword.Text := ServerRec.Password;
        fmReg.edRole.Text := ServerRec.Role;

        fmReg.edtFBClient.Text := ServerRec.ClientLibraryPath;
        fmReg.edtPort.Text := ServerRec.Port;

        fmReg.NewReg:= True;
        ModalResult := fmReg.ShowModal;
      finally
        cboxItems.Free;
        LoadRegisteredDatabases;
        ServerNode := GetServerNodeByServerName(ServerName);
        if ServerNode <> nil then
          ServerNode.Expand(false);
      end;
    end else
    begin
      DBNode := turbocommon.GetAncestorAtLevel(tvMain.Selected, 1);
      DBNode.DeleteChildren;
      AddRootObjects(DBNode, ServerRec.VersionMajor);
      //if isDbConnected then
        //RegisteredDatabases[dbIndex].IBDatabase.Connected := True;
    end;
  end;
  TmpRestoreDlg.Free;
end;

procedure TfmMain.lmRestoreNewClick(Sender: TObject);
begin
  mnRestoreClick(nil);
end;

procedure TfmMain.lmBlobEditorClick(Sender: TObject);
var
  ServerNode, DBNode, SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  Rec: TDatabaseRec;
  dbIndex: Integer;
  ATab: TTabSheet;
  frmBlobEdit: TfrmBlobEdit;
  Server, DBAlias, ShortTitle, FullHint: string;
begin
  SelNode := tvMain.Selected;
  if SelNode = nil then Exit;

  ServerNode := TTreeNode(GetAncestorAtLevel(SelNode, 0));
  DBNode     := TTreeNode(GetAncestorAtLevel(SelNode, 1));

  NodeInfos := TPNodeInfos(DBNode.Data);
  dbIndex   := NodeInfos^.dbIndex;
  Rec       := RegisteredDatabases[dbIndex];

  Server    := ServerNode.Text; // Servername (Level 0)
  DBAlias   := DBNode.Text;

  // Prüfen, ob Editor schon existiert
  if Assigned(NodeInfos^.ViewForm) then
  begin
    frmBlobEdit := TfrmBlobEdit(NodeInfos^.ViewForm);
    ATab := frmBlobEdit.Parent as TTabSheet;
  end
  else
  begin
    frmBlobEdit := TfrmBlobEdit.Create(Application);

    ATab := TTabSheet.Create(Self);
    ATab.PageControl := PageControl1;
    ATab.Tag := dbIndex;

    frmBlobEdit.Parent := ATab;
    frmBlobEdit.Align := alClient;
    frmBlobEdit.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmBlobEdit;
  end;

  // Tab-Titel kurz halten
  ShortTitle := 'BlobEditor';
  ATab.Caption := ShortTitle;

  // Vollständiger Kontext im Tooltip
  FullHint :=
    'Server: '  + Server + sLineBreak +
    'DBAlias: ' + DBAlias + sLineBreak +
    'DBPath: '  + Rec.RegRec.DatabaseName + sLineBreak +
    'Object: BLOB-Editor';

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Tab aktivieren + Initialisierung
  PageControl1.ActivePage := ATab;
  frmBlobEdit.Init(nil, NodeInfos);
  frmBlobEdit.Show;
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
       dmSysTables.stTrans.CommitRetaining;
       MessageDlg('Password has been changed', mtInformation, [mbOk], 0);
    except
      on E: Exception do
        ShowMessage('Error while changing password: ' + e.Message);
    end;
  end;
end;

procedure TfmMain.lmCompareClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  frmComparison: TfmComparison;
  ShortTitle, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  // Verbindung prüfen (PW evtl. leer → kann trotzdem gültig sein)
  if not (RegisteredDatabases[dbIndex].RegRec.SavePassword or ConnectToDBAs(dbIndex)) then
    Exit;

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmComparison) then
    frmComparison := TfmComparison(NodeInfos^.ViewForm)
  else
  begin
    frmComparison := TfmComparison.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmComparison.Parent := ATab;
    frmComparison.Align := alClient;
    frmComparison.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmComparison;
  end;

  // Tab vorbereiten
  ATab := frmComparison.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Titel für Tab & Form
  ShortTitle := RegisteredDatabases[dbIndex].RegRec.Title + ': Database Comparison';
  ATab.Caption := ShortTitle;
  frmComparison.Caption := ShortTitle;

  // Hint mit Details
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Database Comparison';

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Formular initialisieren
  frmComparison.Init(dbIndex, NodeInfos);
  frmComparison.Show;
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
      ShowCompleteQueryWindow(dbIndex, 'Script permissions#' + IntToStr(dbIndex) + ':' + UserName, List.Text);
    finally
      List.Free;
    end;
  end;
end;

procedure TfmMain.lmCopyTableClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  frmCopyTable: TfmCopyTable;
  TableName, ShortTitle, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) or (SelNode.Parent.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Parent.Parent.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  TableName := SelNode.Text;

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmCopyTable) then
    frmCopyTable := TfmCopyTable(NodeInfos^.ViewForm)
  else
  begin
    frmCopyTable := TfmCopyTable.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmCopyTable.Parent := ATab;
    frmCopyTable.Align := alClient;
    frmCopyTable.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmCopyTable;
  end;

  // Tab vorbereiten
  ATab := frmCopyTable.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Titel
  ShortTitle := 'Copy Table: ' + TableName;
  ATab.Caption := ShortTitle;
  frmCopyTable.Caption := ShortTitle;

  // Hint mit Details
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Table Copy' + sLineBreak +
    'Table: ' + TableName;

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Formular initialisieren
  frmCopyTable.Init(dbIndex, TableName, NodeInfos);
  frmCopyTable.Show;
end;


procedure TfmMain.lmCreateDBClick(Sender: TObject);
var cboxItems: TStringList;
    SelectedServerNode: TTreeNode;
    ServerName: string;
    ServerErrStr: string;
begin
  if tvMain.Items.Count = 0 then
    exit;

  try
    cboxItems := GetServerListFromTreeView;

    if tvMain.Selected = nil then
      tvMain.Selected := tvMain.Items[0];

    SelectedServerNode := turbocommon.GetAncestorAtLevel(tvMain.Selected, 0);

    if not IsServerReachable(SelectedServerNode.Text, ServerErrStr) then
    begin
      MessageDlg(ServerErrStr, mtError, [mbOK], 0);
      Exit;
    end;

    ServerName := SelectedServerNode.Text;

    fmCreateDB.cmbBoxServers.Items.Assign(cboxItems);

    fmCreateDB.cmbBoxServers.ItemIndex := fmCreateDB.cmbBoxServers.Items.IndexOf(SelectedServerNode.Text);

    fmCreateDB.edNewDatabase.Text:= '';
    fmCreateDB.Init;
    if fmCreateDB.ShowModal = mrOK then
    begin
      LoadRegisteredDatabases;
      SelectedServerNode := GetServerNodeByServerName(ServerName);
      if SelectedServerNode <> nil then
        SelectedServerNode.Expand(false);
    end;

  finally
    cboxItems.Free;
  end;
end;

procedure TfmMain.lmDBAdminClick(Sender: TObject);
var
  SelNode, ServerNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  //MainForm: TMainForm;  //DBAdmin
  ShortTitle, FullHint, DBAlias: string;
  ServerErrStr: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  ServerNode := turbocommon.GetAncestorAtLevel(SelNode, 0);

  if not IsServerReachable(ServerNode.Text, ServerErrStr) then
  begin
    MessageDlg(ServerErrStr, mtError, [mbOK], 0);
    Exit;
  end;

  //CloseDB(dbIndex);
  MainForm.Init(dbIndex, NodeInfos);
  MainForm.ShowModal;
end;

procedure TfmMain.lmDBInfoClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  frmDBInfo: TfmDBInfo;
  ShortTitle, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmDBInfo) then
    frmDBInfo := TfmDBInfo(NodeInfos^.ViewForm)
  else
  begin
    frmDBInfo := TfmDBInfo.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmDBInfo.Parent := ATab;
    frmDBInfo.Align := alClient;
    frmDBInfo.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmDBInfo;
  end;

  // Tab vorbereiten
  ATab := frmDBInfo.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Titel
  ShortTitle := 'DB Info: ' + RegisteredDatabases[dbIndex].RegRec.Title;
  ATab.Caption := ShortTitle;
  frmDBInfo.Caption := ShortTitle;

  // Hint mit Details
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Database Information';

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Formular initialisieren

  //frmDBInfo.edServerString.Text := TPNodeInfos(SelNode.Parent.Data)^ .ServerSession.FBVersionString;
  frmDBInfo.Init(dbIndex, NodeInfos);
  frmDBInfo.Show;
end;

procedure TfmMain.ReleaseRegisteredDatabase(dbIndex: Integer);
var
  i: Integer;
begin
  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then Exit;

  with RegisteredDatabases[dbIndex] do
  begin
    CloseDB(dbIndex, True);
    FreeAndNil(IBDatabaseInfo);
    FreeAndNil(IBQuery);
    FreeAndNil(IBTransaction);
    FreeAndNil(IBDatabase);
  end;
end;

procedure TfmMain.ReleaseRegisteredDatabases;
var
  i: Integer;
begin
  for i := High(RegisteredDatabases) downto 0 do
    ReleaseRegisteredDatabase(i);

  RegisteredDatabases := nil;
end;

{procedure TfmMain.ReleaseRegisteredDatabases;
var
  i: Integer;
  dbName: String;
begin
  for i := 0 to High(RegisteredDatabases) do
  begin
    dbName := RegisteredDatabases[i].IBDatabase.DatabaseName;
    if dbName = '' then
      dbName := '(unknow)';

    if RegisteredDatabases[i].IBTransaction.Active then
      if  MessageDlg( Format('There is an open transaction in the database "%s". Do you want to commit it?',
         [dbName]), mtConfirmation, [mbYes, mbNo], 0 ) = mrYes
      then  RegisteredDatabases[i].IBTransaction.Commit
      else  RegisteredDatabases[i].IBTransaction.Rollback;

    try
      if RegisteredDatabases[i].IBDatabase.Connected then
        RegisteredDatabases[i].IBDatabase.Close;
    except
      on E: Exception do
        raise Exception.CreateFmt('Failed to close database connection "%s": %s',
          [dbName, E.Message]);
    end;
    FreeAndNil(RegisteredDatabases[i].IBTransaction);
    FreeAndNil(RegisteredDatabases[i].IBDatabase);
  end;
  RegisteredDatabases := nil;
end;}


function TfmMain.CheckActiveTransactions(
  dbIndex: Integer;
  ADefaultCommitKind: TCommitKind;
  ASilent: Boolean
): Boolean;
var
  Trans: TIBTransaction;
begin
  Result := True;
  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then Exit;

  Trans := RegisteredDatabases[dbIndex].IBTransaction;
  Result := CheckActiveTransaction(Trans, ADefaultCommitKind, ASilent);
end;

function TfmMain.CheckActiveTransaction(
  AIBTransaction: TIBTransaction;
  ADefaultCommitKind: TCommitKind;
  ASilent: Boolean
): Boolean;
var
  dlgResult: Integer;
  CommitOK: Boolean;
  dbName: string;
begin
  Result := True;
  if (AIBTransaction = nil) or (not AIBTransaction.Active) then
    Exit;

  if (AIBTransaction.DefaultDatabase <> nil) then
    dbName := AIBTransaction.DefaultDatabase.DatabaseName
  else
    dbName := '(unknown database)';

  CommitOK := False;

  if not ASilent then
  begin
    dlgResult := MessageDlg(
      'The database "' + dbName + '" still has an active transaction.' + LineEnding +
      'Do you want to save the changes (Commit) or discard them (Rollback)?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0
    );

    case dlgResult of
      mrYes:
        begin
          try
            // zuerst versuchen mit gewünschtem Commit-Typ
            case ADefaultCommitKind of
              tctCommit: AIBTransaction.Commit;
              tctCommitRetaining: AIBTransaction.CommitRetaining;
            end;
            CommitOK := True;
          except
            on E: Exception do
            begin
              // zweiter Versuch mit alternativer Commit-Art
              try
                if ADefaultCommitKind = tctCommit then
                  AIBTransaction.CommitRetaining
                else
                  AIBTransaction.Commit;
                CommitOK := True;
              except
                on E2: Exception do
                begin
                  MessageDlg('Commit error for "' + dbName + '": ' + E2.Message, mtError, [mbOK], 0);
                  CommitOK := False;
                end;
              end;
            end;
          end;

          // falls kein Commit erfolgreich → Rollback
          if not CommitOK then
          begin
            try
              AIBTransaction.Rollback;
            except
              on E3: Exception do
                MessageDlg('Rollback error for "' + dbName + '": ' + E3.Message, mtError, [mbOK], 0);
            end;
            Result := False;
          end;
        end;

      mrNo:
        try
          AIBTransaction.Rollback;
        except
          on E: Exception do
          begin
            MessageDlg('Rollback error for "' + dbName + '": ' + E.Message, mtError, [mbOK], 0);
            Result := False;
          end;
        end;

      mrCancel:
        Result := False;
    end;
  end
  else
  begin
    // Silent-Modus → kein Dialog, nur automatischer Commit-Versuch
    try
      case ADefaultCommitKind of
        tctCommit: AIBTransaction.Commit;
        tctCommitRetaining: AIBTransaction.CommitRetaining;
      end;
    except
      on E: Exception do
      begin
        try
          if ADefaultCommitKind = tctCommit then
            AIBTransaction.CommitRetaining
          else
            AIBTransaction.Commit;
        except
          on E2: Exception do
          begin
            try
              AIBTransaction.Rollback;
            except end;
            Result := False;
          end;
        end;
      end;
    end;
  end;
end;

// ============================================================================
// Schließt eine einzelne DB (inkl. Tabs, TreeView-Knoten, ggf. Session)
// Silent = True → keine Dialoge, Rollback automatisch
// ============================================================================
procedure TfmMain.CloseDB(dbIndex: Integer; Silent: Boolean = True);
var
  i, j, k: Integer;
  TabSheet: TTabSheet;
  Trans: TIBTransaction;
  NodeInfo: TPNodeInfos;
begin
  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then Exit;

  Trans := RegisteredDatabases[dbIndex].IBTransaction;

  // Offene Transaktion behandeln
  if (Trans <> nil) and Trans.InTransaction then
  begin
    if Silent then
    begin
      try
        Trans.Commit;
      except
        on E: Exception do
          ShowMessage('Commit-Error (Silent): ' + E.Message);
      end;
    end
    else
    begin
      // GUI-Aufruf → Dialog
      if not CheckActiveTransActions(dbIndex, tctCommit, true) then
        Exit; // Benutzer hat abgebrochen
    end;
  end;

  // DB-Verbindung schließen
  try
    if RegisteredDatabases[dbIndex].IBDatabase.Connected then
      RegisteredDatabases[dbIndex].IBDatabase.Close;
  except
    on E: Exception do
      ShowMessage('Error-CloseDB: ' + E.Message);
  end;

  // Tabs schließen
  for i := PageControl1.PageCount - 1 downto 1 do
  begin
    if (PageControl1.Pages[i] as TComponent).Tag = dbIndex then
    begin
      TabSheet := PageControl1.Pages[i] as TTabSheet;

      // Unterformulare schließen
      for j := 0 to TabSheet.ControlCount - 1 do
        if TabSheet.Controls[j] is TForm then
          (TabSheet.Controls[j] as TForm).Close;

      TabSheet.Free;
    end;
  end;

  // TreeView-Knoten einklappen
  for k := 0 to tvMain.Items.Count - 1 do
  begin
    if Assigned(tvMain.Items[k].Data) and
       (TPNodeInfos(tvMain.Items[k].Data)^.dbIndex = dbIndex) then
      tvMain.Items[k].Collapse(True);
  end;

  // Session trennen, falls vorhanden
  for k := 0 to tvMain.Items.Count - 1 do
  begin
    if (tvMain.Items[k].Level = 0) and Assigned(tvMain.Items[k].Data) then
    begin
      NodeInfo := TPNodeInfos(tvMain.Items[k].Data);
      if Assigned(NodeInfo^.ServerSession) then
      begin
        if TServerSession(NodeInfo^.ServerSession).Connected then
          TServerSession(NodeInfo^.ServerSession).Disconnect;
      end;

    end;
  end;

end;

// ============================================================================
// Markierte DB schließen (inkl. offene Transaktionen, Tabs, TreeView)
// ============================================================================
procedure TfmMain.lmDisconnectClick(Sender: TObject);
var
  dbIndex: Integer;
begin
  if tvMain.Selected = nil then Exit;

  dbIndex := TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  CloseDB(dbIndex, true); // Silent = False → Dialoge behandelt
end;

// ============================================================================
// Alle DBs schließen (Silent, keine Dialoge)
// ============================================================================
procedure TfmMain.lmDisconnectAllClick(Sender: TObject);
var i: Integer;
begin
  if tvMain.Items.Count = 0 then
    exit;

  for i := 0 to Length(RegisteredDatabases) - 1 do
    CloseDB(i, True);
end;

procedure TfmMain.lmDropDomainClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetDomainDeps(Rec.IBDatabase, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP Domain ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Domain#' + IntToStr(dbIndex) + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The Domain "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the Domain anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP Domain ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Domain#' + IntToStr(dbIndex) + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropFireBirdFunctionClick(Sender: TObject);
var
  DepStr, TmpQueryStr: string;
  dbIndex: integer;
  Rec: TDatabaseRec;
  Q: TIBQuery;
  IsNative: Boolean;
  FuncName: string;
begin
  // Get DB index from node hierarchy
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  FuncName := UpperCase(Trim(tvMain.Selected.Text));
  IsNative := False;

  // Check if the function is really a native PSQL function (not UDR)
  Q := TIBQuery.Create(nil);
  try
    Q.DataBase := Rec.IBDatabase;
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
  DepStr := GetFBFunctionDeps(Rec.IBDatabase, FuncName, '');
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
      ShowCompleteQueryWindow(dbIndex, 'Drop Function#' + IntToStr(dbIndex) + ':' + FuncName, TmpQueryStr, nil);
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
  DepStr := GetGeneratorDeps(Rec.IBDatabase, tvMain.Selected.Text);
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
  DepStr := '';
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  //DepStr := GetPackageDependencies(Rec.IBDatabase, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP PACKAGE ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Package#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The package "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the package anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP PACKAGE ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop Package#' + IntToStr(dbIndex) + ':' +  tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmDropPackageProcedureClick(Sender: TObject);
begin

end;

procedure TfmMain.lmDropStoredProcedureClick(Sender: TObject);
var
  DepStr, TmpQueryStr: string;
  dbIndex: integer;
  Rec: TDatabaseRec;
  Q: TIBQuery;
  IsNative: Boolean;
  ProcName: string;
begin
  // Get DB index from node hierarchy
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  ProcName := UpperCase(Trim(tvMain.Selected.Text));
  IsNative := False;

  // Check if the procedure is a native PSQL procedure
  Q := TIBQuery.Create(nil);
  try
    Q.DataBase := Rec.IBDatabase;
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
  DepStr := GetFBProcedureDeps(Rec.IBDatabase, ProcName, '');
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
  DepStr := GetTriggerDeps(Rec.IBDatabase, tvMain.Selected.Text);
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
  DepStr := GetUDFDeps(Rec.IBDatabase, tvMain.Selected.Text, '');
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
  Q: TIBQuery;
  IsUDR: Boolean;
  FunctionName: string;
begin
  // Get database index (parent node)
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  FunctionName := UpperCase(Trim(tvMain.Selected.Text));
  IsUDR := False;

  // Check if the function is really a UDR function by inspecting RDB$ENGINE_NAME
  Q := TIBQuery.Create(nil);
  try
    Q.DataBase := Rec.IBDatabase;
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
  DepStr := GetUDRFunctionDeps(Rec.IBDatabase, FunctionName, '');
  if DepStr = '' then
  begin
    TmpQueryStr := 'DROP FUNCTION ' + FunctionName;
    ShowCompleteQueryWindow(dbIndex, 'Drop Function#' + IntToStr(dbIndex) + ':' + FunctionName, TmpQueryStr, nil);
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
      ShowCompleteQueryWindow(dbIndex, 'Drop Function#' + IntToStr(dbIndex) + ':' + FunctionName, TmpQueryStr, nil);
    end;
  end;
end;

procedure TfmMain.lmDropUDRProcedureClick(Sender: TObject);
var
  DepStr, TmpQueryStr: string;
  dbIndex: integer;
  Rec: TDatabaseRec;
  Q: TIBQuery;
  IsUDR: Boolean;
  ProcName: string;
begin
  // Get database index (parent of parent node)
  dbIndex := TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  ProcName := UpperCase(Trim(tvMain.Selected.Text));
  IsUDR := False;

  // Check if the procedure is really a UDR procedure by inspecting RDB$ENGINE_NAME
  Q := TIBQuery.Create(nil);
  try
    Q.DataBase := Rec.IBDatabase;
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
  DepStr := GetUDRProcedureDeps(Rec.IBDatabase, ProcName, '');
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
      ShowCompleteQueryWindow(dbIndex, 'Drop UDR-Procedure#' + IntToStr(dbIndex) + ':' + ProcName, TmpQueryStr, nil);
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
  DepStr := GetUserDeps(Rec.IBDatabase, tvMain.Selected.Text);
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
       ShowCompleteQueryWindow(dbIndex, 'Drop User#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;


procedure TfmMain.lmDropViewClick(Sender: TObject);
var   DepStr, TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
begin
  dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DepStr := GetViewDeps(Rec.IBDatabase, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP View ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop View#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
  end else
  begin
    if MessageDlg(
     Format('The View "%s" has dependencies!' + sLineBreak + sLineBreak +
            '%s' + sLineBreak +
            'Do you want to delete the View anyway?', [tvMain.Selected.Text, DepStr]),
     mtWarning, [mbYes, mbCancel], 0) = mrYes then
     begin
       TmpQueryStr := 'DROP View ' + tvMain.Selected.Text;
       ShowCompleteQueryWindow(dbIndex, 'Drop View#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
     end;
  end;
end;

procedure TfmMain.lmEditExceptionClick(Sender: TObject);
begin
  lmScriptExceptionClick(nil);
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
    TmpQuery: TIBQuery;
    TmpQueryList: TStringList;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryList := fetch_package.GetPackageDeclaration(Rec.IBDatabase, tvMain.Selected.Text);
  TmpQueryStr  := TmpQueryList.Text;
  TmpQueryList.Free;
  ShowCompleteQueryWindow(dbIndex, 'Edit Package#' + IntToStr(dbIndex) + ':' +  tvMain.Selected.Text, TmpQueryStr, nil);
end;

{procedure TfmMain.lmFirebirdConfigClick(Sender: TObject);
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
end;}

procedure TfmMain.lmFirebirdConfigClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  Rec: TDatabaseRec;
  ATab: TTabSheet;
  frmFirebirdConfig: TfmFirebirdConfig;
  ShortTitle, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmFirebirdConfig) then
    frmFirebirdConfig := TfmFirebirdConfig(NodeInfos^.ViewForm)
  else
  begin
    frmFirebirdConfig := TfmFirebirdConfig.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := 25;
    frmFirebirdConfig.Parent := ATab;
    frmFirebirdConfig.Align := alClient;
    frmFirebirdConfig.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmFirebirdConfig;
  end;

  // Tab vorbereiten
  ATab := frmFirebirdConfig.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Titel
  ShortTitle := 'Configure Firebird Server';
  ATab.Caption := ShortTitle;
  frmFirebirdConfig.Caption := ShortTitle;

  // Hint mit Details
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + Rec.IBDatabase.DatabaseName + sLineBreak +
    'Object type: Firebird Configuration';

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Formular initialisieren
  frmFirebirdConfig.Init(dbIndex, NodeInfos);
  frmFirebirdConfig.Show;
end;

procedure TfmMain.lmGetPackageFunctionClick(Sender: TObject);
var   TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
      Node: TTreeNode;
      PackageName: string;
begin
  Node := tvMain.Selected;
  PackageName := Node.Parent.Parent.Text;
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := udb_package_firebird_func_fetcher.GetPackageFirebirdFunctionDeclaration(Rec.IBDatabase, Node.Text, PackageName);
  ShowCompleteQueryWindow(dbIndex, 'Package-Function:' + PackageName + '#' + IntToStr(dbIndex) + ':' +  tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmGetPackageProcedureClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TIBQuery;
    Node: TTreeNode;
    PackageName: string;
begin
    Node := tvMain.Selected;
    PackageName := Node.Parent.Parent.Text;
    //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
    dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
    Rec := RegisteredDatabases[dbIndex];
    TmpQueryStr := udb_package_firebird_proc_fetcher.GetPackageFirebirdProcedureDeclaration(Rec.IBDatabase, Node.Text, PackageName);
    ShowCompleteQueryWindow(dbIndex, 'Package-Procedure:' + PackageName + '#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmGetPackageUDRFunctionClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TIBQuery;
    Node: TTreeNode;
    PackageName: string;
begin
  Node := tvMain.Selected;
  PackageName := Node.Parent.Parent.Text;
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := udb_package_udr_func_fetcher.GetPackageUDRFunctionDeclaration(Rec.IBDatabase, Node.Text, PackageName);
  ShowCompleteQueryWindow(dbIndex, 'Package-UDRFunction:' + PackageName + '#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmGetPackageUDRProcedureClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
      Rec: TDatabaseRec;
      TmpQuery: TIBQuery;
      Node: TTreeNode;
      PackageName: string;
begin
    Node := tvMain.Selected;
    PackageName := Node.Parent.Parent.Text;
    //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
    dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
    Rec := RegisteredDatabases[dbIndex];
    TmpQueryStr := udb_package_udr_proc_fetcher.GetPackageUDRProcedureDeclaration(Rec.IBDatabase, Node.Text, PackageName);
    ShowCompleteQueryWindow(dbIndex, 'Package-UDRProcedure:' + PackageName + '#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmEditUDFFuctionClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TIBQuery;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetUDFFunctionDeclaration(Rec.IBDatabase, tvMain.Selected.Text);
  ShowCompleteQueryWindow(dbIndex, 'Edit UDF#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmEditUDRFunctionClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TIBQuery;
begin
  dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  //dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetUDRFunctionDeclaration(Rec.IBDatabase, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit UDRFunction#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
end;

procedure TfmMain.lmEditUDRProcedureClick(Sender: TObject);
var TmpQueryStr: string; dbIndex: integer;
    Rec: TDatabaseRec;
    TmpQuery: TIBQuery;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := udb_udr_proc_fetcher.GetUDRProcedureDeclaration(Rec.IBDatabase, tvMain.Selected.Text, '');
  ShowCompleteQueryWindow(dbIndex, 'Edit UDR-Procedure#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
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

    ShowCompleteQueryWindow(dbIndex, 'getIncSQL#'  + IntToStr(dbIndex) + ':'  + AGenName,
      'SELECT GEN_ID(' + AGenName + ', 1) FROM RDB$DATABASE;');
  end;
end;

{procedure TfmMain.lmImportTableClick(Sender: TObject);
var fmImportTable: TfmImportTable;
    SelNode: TTreeNode;
    dbIndex: integer;
begin
  SelNode := tvMain.Selected;
  dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
  fmImportTable := TfmImportTable.Create(nil);
  fmImportTable.Init(dbIndex, SelNode.Text, TPNodeInfos(SelNode.Data));
  fmImportTable.Show;
end;}

procedure TfmMain.lmImportTableClick(Sender: TObject);
var
  fmImportTable: TfmImportTable;
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  Title, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Data = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  Title := 'Import Table: ' + SelNode.Text;

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmImportTable) then
    fmImportTable := TfmImportTable(NodeInfos^.ViewForm)
  else
  begin
    fmImportTable := TfmImportTable.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    fmImportTable.Parent := ATab;
    fmImportTable.Align := alClient;
    fmImportTable.BorderStyle := bsNone;

    NodeInfos^.ViewForm := fmImportTable;
  end;

  // Tab vorbereiten
  ATab := fmImportTable.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Tab-Titel
  ATab.Caption := Title;
  fmImportTable.Caption := Title;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Table' + sLineBreak +
    'Table name: ' + SelNode.Text + sLineBreak +
    'Action: Import';
  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form initialisieren
  fmImportTable.Init(dbIndex, SelNode.Text, NodeInfos);
  fmImportTable.Show;
end;

procedure TfmMain.lmNewUDRFunctionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex := TPNodeInfos(SelNode.Parent.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'New UDR Function#' + IntToStr(dbIndex));

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
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'New UDR-Procedure' + IntToStr(dbIndex));

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

procedure TfmMain.Button1Click(Sender: TObject);
begin
  tvMain.Visible := not tvMain.Visible;
end;

procedure TfmMain.OnIBConnectionLogin(Database: TIBDatabase; LoginParams: TStrings);
var mr: TModalResult;
begin
  fmEnterPass.edUser.Text      := Database.Params.Values['user_name'];
  fmEnterPass.edPassword.Text  := Database.Params.Values['password'];
  fmEnterPass.laDatabase.Caption := Database.DatabaseName;
  repeat
    mr := fmEnterPass.ShowModal;
    if mr = mrCancel then
    begin
      Database.Connected := false;
      Exit;
    end;

    Database.Params.Values['user_name'] := fmEnterPass.edUser.Text;
    Database.Params.Values['password']  := fmEnterPass.edPassword.Text;
    Database.Open;
    if not Database.Connected then
      ShowMessage('Connection failed. Please try again.');

  until Database.Connected;
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
  FRoutineInfo.Connection  := Rec.IBDatabase;
  FRoutineInfo.dbIndex     := dbIndex;

  if ARoutineType in [rtPackageFBFunc, rtPackageFBProc, rtPackageUDRFunc, rtPackageUDRProc] then
  begin
    FRoutineInfo.PackageName := SelNode.Parent.Parent.Text;
  end else
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
  end else
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
        //'Server:   ' + GetServerName(Rec.IBDatabase.DatabaseName) +   sLineBreak +
        'DBAlias:   ' + DBAlias + sLineBreak +
        //'DBAlias:  ' + Rec.AliasName  + sLineBreak +
        'DBPath:   ' + Rec.IBDatabase.DatabaseName + sLineBreak +
        'Package:  ' + FRoutineInfo.PackageName + sLineBreak +
        'Object type:   ' + RoutineTypeToStr(ARoutineType) + sLineBreak +
        'Object name:     ' + FRoutineInfo.RoutineName;
    end else
    begin
      FullHint :=
        'Server:   ' + GetAncestorNodeText(SelNode, 0) +   sLineBreak +
        'DBAlias:  ' + DBAlias + sLineBreak +
        'DBPath:   ' + Rec.IBDatabase.DatabaseName + sLineBreak +
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

(****************  Connect As  *****************)
procedure TfmMain.lmConnectAsClick(Sender: TObject);
begin
  if ConnectToDBAs(TPNodeInfos(tvMain.Selected.Data)^.dbIndex, True) then
    tvMain.Selected.Expand(False)
  else
    tvMain.Selected.Collapse(False);
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
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'New Procedure#' +
                    IntToStr(TPNodeInfos(SelNode.Data)^.dbIndex) + ':' + AProcName);

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

    QWindow:= ShowQueryWindow(dbIndex, 'New Trigger#' + IntToStr(dbIndex) + ':'
                 + TrigType + ':' + fmCreateTrigger.edTriggerName.Text);

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
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new view', 'Please enter new view name', AViewName) then
  begin
    dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Data)^.dbIndex, 'New View#' + IntToStr(dbIndex) + ':' +  AViewName);
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
  DepStr := GetExceptionDeps(Rec.IBDatabase, tvMain.Selected.Text);
  if  DepStr = '' then
  begin
    TmpQueryStr := 'DROP Exception ' + tvMain.Selected.Text;
    ShowCompleteQueryWindow(dbIndex, 'Drop Exception#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
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
    TmpQuery: TIBQuery;
begin
  //dbIndex :=  TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
  dbIndex:= TPNodeInfos(tvMain.Selected.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  TmpQueryStr := GetFirebirdProcedureDeclaration(Rec.IBDatabase, tvMain.Selected.Text, '', true);
  ShowCompleteQueryWindow(dbIndex, 'Edit Procedure#' + IntToStr(dbIndex) + ':' + tvMain.Selected.Text, TmpQueryStr, nil);
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

procedure TfmMain.lmEditTableDataClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  Rec: TDatabaseRec;
  EditWindow: TfmEditTable;
  ATableName, DBAlias, ShortTitle, FullHint: string;
  dbIndex: Integer;
  ATab: TTabSheet;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;

  // Tabellenname und DB-Index ermitteln
  ATableName := SelNode.Text;
  dbIndex := TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];
  DBAlias := GetAncestorNodeText(SelNode, 1);

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.EditorForm) and (NodeInfos^.EditorForm is TfmEditTable) then
    EditWindow := TfmEditTable(NodeInfos^.EditorForm)
  else
  begin
    EditWindow := TfmEditTable.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    EditWindow.Parent := ATab;
    EditWindow.Align := alClient;
    EditWindow.BorderStyle := bsNone;

    NodeInfos^.EditorForm := EditWindow;
  end;

  // Tab vorbereiten
  ATab := EditWindow.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Tab-Titel
  ShortTitle := ATableName;
  ATab.Caption := ShortTitle;
  EditWindow.Caption := ShortTitle;

  // Detaillierte Infos als Hint
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + Rec.IBDatabase.DatabaseName + sLineBreak +
    'Object type: Table' + sLineBreak +
    'Table name: ' + ATableName  + sLineBreak +
    'Modus: Edit Tabledata';

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Formular initialisieren
  EditWindow.Rec := Rec;
  EditWindow.Init(dbIndex, ATableName, NodeInfos);
  EditWindow.Show;
end;


(****************  Edit Trigger  ******************)
procedure TfmMain.lmEditTriggerClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  ATriggerName: string;
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
    ATriggerName:= SelNode.Text;
    QWindow:= ShowQueryWindow(dbIndex, 'Edit Trigger#' + IntToStr(dbIndex) + ':' + ATriggerName);

    QWindow.meQuery.Lines.Clear;
    dmSysTables.ScriptTrigger(dbIndex, ATriggerName, QWindow.meQuery.Lines);
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
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
    AViewName:= SelNode.Text;
    QWindow:= ShowQueryWindow(dbIndex, 'Edit View#' + IntToStr(dbIndex) + ':' + AViewName);

    GetViewInfo(dbIndex, AViewName, Columns, ViewBody);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('DROP VIEW "' + AViewName + '";');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('CREATE VIEW "' + AViewName + '" (' + Columns + ')');
    QWindow.meQuery.Lines.Add('AS');

    QWindow.meQuery.Text:= QWindow.meQuery.Text + ViewBody;
    QWindow.Show;
  end;
end;

(**************  New Domain  *************)
procedure TfmMain.lmNewDomainClick(Sender: TObject);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
  Line: string;
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
   if (SelNode = nil) or  (SelNode.Parent = nil) then exit;

  dbIndex := TPNodeInfos(SelNode.Parent.Data)^.dbIndex;

  fmNewDomain.Init(dbIndex);

  if fmNewDomain.ShowModal = mrOk then
  with QWindow do
  begin
    QWindow:= ShowQueryWindow(dbIndex, 'New Domain#' + IntToStr(dbIndex));
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
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'New Exception#' + IntToStr(dbIndex));
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE EXCEPTION Exception_name_1 ''exception message'';');
    QWindow.Show;
  end;
end;

procedure TfmMain.SetConnection(Index: Integer);
var ServerNode: TTreeNode;
    Rec: TRegisteredDatabase;
begin
  if  CurrentIBConnection = RegisteredDatabases[Index].IBDatabase then
    exit;

  CurrentIBConnection  := RegisteredDatabases[Index].IBDatabase;
  CurrentIBTransaction := RegisteredDatabases[Index].IBTransaction;
  SQLQuery1 := RegisteredDatabases[Index].IBQuery;


  {if not CurrentIBConnection.Connected then
  begin
    Rec := RegisteredDatabases[Index].RegRec;
    if Rec.Password = '' then
    begin
      if not ConnectToDBAs(Index) then
        Exit;
    end;

    CurrentIBConnection.Params.Values['user_name'] := Rec.UserName;
    CurrentIBConnection.Params.Values['password']  := Rec.Password;
    CurrentIBConnection.LoginPrompt := false;
  end;
  }
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
var description: string;
begin
  try
    Result:= OpenSQLHistory(DatabaseTitle);
    if Result then
    begin
      mdsHistory.Last;
      //if (SQLType <> 'SELECT') or (mdsHistory.FieldByName('SQLStatement').AsString <> SQLStatement) then
      if not mdsHistory.Locate('SQLStatement', SQLStatement, []) then
      begin
        description := 'Description';
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
{procedure TfmMain.lmNewGenClick(Sender: TObject);
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
end;}

(**************  Initialize New Generator form  *************)
procedure TfmMain.InitNewGen(DatabaseIndex: Integer);
var
  Rec: TDatabaseRec;
begin
  Rec:= RegisteredDatabases[DatabaseIndex];
  fmNewGen.Init(DatabaseIndex);
end;

procedure TfmMain.lmNewGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    fmNewGen.Init(TPNodeInfos(SelNode.Parent.Data)^.dbIndex);
    fmNewGen.edGenName.Clear;
    fmNewGen.edGenName.Enabled:= True;
    fmNewGen.cxTrigger.Checked:= False;
    fmNewGen.ShowModal;
  end;
end;

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

  NodeInfos := TPNodeInfos(SelNode.Parent.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  DBAlias := GetAncestorNodeText(SelNode, 1);

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.NewForm) and (NodeInfos^.NewForm is TfmNewTable) then
    Form := TfmNewTable(NodeInfos^.NewForm)
  else begin
    Form := TfmNewTable.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;

    Form.Parent := ATab;
    Form.Align := alClient;
    Form.BorderStyle := bsNone;

    NodeInfos^.NewForm := Form;
  end;

  Form.Init(dbIndex, SelNode.Data);

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
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
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
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  ModuleName:= '<modulename>';
  EntryPoint:= '<entryname>';
  dbIndex := TPNodeInfos(SelNode.Parent.Data)^.dbIndex;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  if InputQuery('Create new UDF-Function', 'Please enter new Function name', AFuncName) then
  if InputQuery('Create new UDF-Function', 'Please enter module name (Library)', ModuleName) then
  if InputQuery('Create new UDF-Function', 'Please enter entry point (External Function name)', EntryPoint) then
  begin
    QWindow:= ShowQueryWindow(dbIndex, 'New UDF#' + IntToStr(dbIndex) + ':' + AFuncName);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('DECLARE EXTERNAL Function "' + AFuncName + '"');
    QWindow.meQuery.Lines.Add('-- (int, varchar(100))');
    QWindow.meQuery.Lines.Add('RETURNS (int)');
    QWindow.meQuery.Lines.Add('ENTRY_POINT ' + QuotedStr(entryPoint));
    QWindow.meQuery.Lines.Add('MODULE_NAME ' + QuotedStr(modulename) + ';');
    QWindow.Show;
  end;
end;

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
        Rec.Charset, Rec.FireBirdClientLibPath, Rec.SQLDialect, Rec.Port, Rec.ServerName, Rec.OverwriteLoadedClientLib) then
          RegisteredDatabases[dbIndex].RegRec.Password:= fmEnterPass.edPassword.Text
        else
          Exit;
    end;
  end;
  QWindow:= ShowQueryWindow(dbIndex, Rec.Title + ':' + IntToStr(dbIndex) + ':Query Window');
  QWindow.Show;
end;

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
  else begin
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
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
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

procedure TfmMain.lmRolePermissionsClick(Sender: TObject);
begin
  lmPermissionsClick(Sender);
end;

procedure TfmMain.lmRolePerManagementClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  fmPermissions: TfmPermissionManage;
  Title, FullHint, DBAlias: string;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  Title := 'Permissions:' + SelNode.Text;

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.EditorForm) and (NodeInfos^.EditorForm is TfmPermissionManage) then
    fmPermissions := TfmPermissionManage(NodeInfos^.EditorForm)
  else
  begin
    fmPermissions := TfmPermissionManage.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    fmPermissions.Parent := ATab;
    fmPermissions.Align := alClient;
    fmPermissions.BorderStyle := bsNone;

    NodeInfos^.EditorForm := fmPermissions;
  end;

  // Tab vorbereiten
  ATab := fmPermissions.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Tab-Titel
  ATab.Caption := Title;
  fmPermissions.Caption := Title;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Role' + sLineBreak +
    'User/Role name: ' + SelNode.Text;
  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form initialisieren
  fmPermissions.Init(NodeInfos, dbIndex, '', SelNode.Text, 2);
  fmPermissions.Show;
end;


(***********  Refresh Click  *************)
procedure TfmMain.lmRefreshClick(Sender: TObject);
begin
  if tvMain.Selected.Expanded then
    tvMain.Selected.Collapse(False);
  tvMainExpanded(nil, tvMain.Selected)
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

procedure TfmMain.lmScriptEngineClick(Sender: TObject);
var
  frmScriptEngine: TfrmScriptEngine;
  dbIndex: integer;
  DBNode: TTreeNode;
begin
  DBNode := tvMain.Selected;
  if DBNode = nil then exit;

  DBNode := turbocommon.GetAncestorAtLevel(DBNode, 1);
  if DBNode = nil then exit;

  dbIndex := TPNodeInfos(DBNode.Data)^.dbIndex;

  frmScriptEngine := TfrmScriptEngine.Create(self);
  frmScriptEngine.Init(dbIndex);
  frmScriptEngine.ShowModal;
end;

(**************  Script Exception  ****************)
procedure TfmMain.lmScriptExceptionClick(Sender: TObject);
var
  SelNode: TTreeNode;
  Script, Msg, Desc: string;
  dbIndex: integer;
begin
  SelNode := tvMain.Selected;
  dbIndex := TPNodeInfos(SelNode.Data)^.dbIndex;
  if dmSysTables.GetExceptionInfo(dbIndex, SelNode.Text, Msg, Desc, Script, false) then
    ShowCompleteQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, 'Script Exception#' + IntToStr(dbIndex) + ':' + SelNode.Text, Script, nil, SelNode.Data);
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
  Iso: TIsolatedQuery;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'Script as insert#' + IntToStr(dbIndex) + ':' + ATableName);


    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('create procedure InsertTo' + ATableName + ' (');

    // Params
    FieldNames:= '';
    ParamNames:= '';

    //GetFields(dbIndex, ATableName, nil);
    Iso := GetFieldsIsolated(RegisteredDatabases[dbIndex].IBDatabase, ATableName);
    //with SQLQuery1 do
    with Iso.Query do
    while not EOF do
    begin
      Skipped:= False;
      if (FieldByName('computed_source').AsString = '') then
      begin
        FieldNames:= FieldNames + Trim(FieldByName('Field_Name').AsString);
        ParamNames:= ParamNames + ':' + Trim(FieldByName('Field_Name').AsString);
        FieldLine:= Trim(FieldByName('Field_Name').AsString) + ' ';
        FieldLine:= FieldLine +
          GetFBTypeName(Iso.Query.FieldByName('field_type_int').AsInteger,
            Iso.Query.FieldByName('field_sub_type').AsInteger,
            Iso.Query.FieldByName('field_length').AsInteger,
            Iso.Query.FieldByName('field_precision').AsInteger,
            Iso.Query.FieldByName('field_scale').AsInteger);
        if Iso.Query.FieldByName('field_type_int').AsInteger in [CStringType,CharType,VarCharType] then
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
    Iso.Free;
    //SQLQuery1.Close;

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
  IsoIndices, IsoIndexFields: TIsolatedQuery;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    ScriptList:= TStringList.Create;
    try
      ScriptTableAsCreate(dbIndex, ATableName, ScriptList);
      QWindow:= ShowQueryWindow(dbIndex, 'Script as create#' + IntToStr(dbIndex) + ':' + ATableName);
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
      PKIndexName := GetPrimaryKeyIndexNameIsolated(RegisteredDatabases[dbIndex].IBDatabase, ATableName, ConstraintName);

      List:= TStringList.Create;
      try
        with dmSysTables do

        IsoIndices := GetIndicesIsolated(RegisteredDatabases[dbIndex].IBDatabase, ATableName);
        if IsoIndices.Query.RecordCount > 0 then
        with IsoIndices.Query do
        while not EOF do
        begin
          if PKIndexName <> Trim(IsoIndices.Query.FieldByName('RDB$Index_name').AsString) then
          begin
            Line:= 'create ';
            if IsoIndices.Query.FieldByName('RDB$Unique_Flag').AsString = '1' then
              Line:= Line + 'Unique ';
            if IsoIndices.Query.FieldByName('RDB$Index_Type').AsString = '1' then
              Line:= Line + 'Descending ';
            Line:= Line + 'index ' + Trim(IsoIndices.Query.FieldByName('RDB$Index_name').AsString) + ' on ' + ATableName;
            IsoIndexFields := GetIndexFieldsIsolated(RegisteredDatabases[dbIndex].IBDatabase, Trim(IsoIndices.Query.FieldByName('RDB$Index_Name').AsString), ATableName, List);
            Line:= Line + ' (' + List.CommaText + ') ;';
            QWindow.meQuery.Lines.Add(Line);
            IsoIndexFields.Free;
          end;
          Next;
        end;
        IsoIndices.Free;

        QWindow.meQuery.Lines.Add('');
        //SQLQuery1.Close;
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
  Iso: TIsolatedQuery;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    ATableName:= SelNode.Text;
    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    QWindow:= ShowQueryWindow(dbIndex, 'Script as update#' + IntToStr(dbIndex) + ':' + ATableName);

    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('create procedure Update' + ATableName + ' (');

    // Params
    ParamAndValue:= '';
    Iso := GetFieldsIsolated(RegisteredDatabases[dbIndex].IBDatabase, ATableName);
    //with SQLQuery1 do
    with Iso.Query do
    while not EOF do
    begin
      Skipped:= False;
      if (Iso.Query.FieldByName('computed_source').AsString = '') then
      begin
        AFieldName:= Trim(Iso.Query.FieldByName('Field_Name').AsString);
        ParamAndValue:= ParamAndValue + AFieldName + ' = :' + AFieldName;
        FieldLine:= AFieldName + ' ';
        FieldLine:= FieldLine + GetFBTypeName(Iso.Query.FieldByName('field_type_int').AsInteger,
          Iso.Query.FieldByName('field_sub_type').AsInteger,
          Iso.Query.FieldByName('field_length').AsInteger,
          Iso.Query.FieldByName('field_precision').AsInteger,
          Iso.Query.FieldByName('field_scale').AsInteger);
        if Iso.Query.FieldByName('field_type_int').AsInteger in [CStringType,CharType,VarCharType] then
          FieldLine:= FieldLine + '(' + Iso.Query.FieldByName('CharacterLength').AsString + ') ';
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
    Iso.Free;
    // Primary Keys
    WhereClause:= '';
    PKFieldsList:= TStringList.Create;
    PKIndexName := GetPrimaryKeyIndexNameIsolated(RegisteredDatabases[dbIndex].IBDatabase, ATableName, ConstraintName);

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

{procedure TfmMain.lmServerRegistryClick(Sender: TObject);
var fmServerRegistry: TfmServerRegistry;
    Node: TTreeNode;
begin
  fmServerRegistry := TfmServerRegistry.Create(nil);

  Node := GetAncestorAtLevel(tvMain.Selected, 0);

  if Node = nil then
    if tvMain.Items.Count > 0 then
    begin
      tvMain.Selected := tvMain.Items[0];
      Node := tvMain.Items[0];
    end;

  if  Node <> nil then
  begin
    //dbs schlissen!
    fmServerRegistry.init(TPNodeInfos(Node.Data)^.ServerSession);
  end
  else
    fmServerRegistry.init(nil);

  fmServerRegistry.ShowModal;
  fmServerRegistry.Free;
end;}

procedure TfmMain.lmServerRegistryClick(Sender: TObject);
var
  fmServerRegistry: TfmServerRegistry;
  Node: TTreeNode;
begin
  fmServerRegistry := TfmServerRegistry.Create(nil);
  try
    Node := GetAncestorAtLevel(tvMain.Selected, 0);

    if Node <> nil then
    begin
      // Falls Node gültig ist
      fmServerRegistry.Init(TPNodeInfos(Node.Data)^.ServerSession);
    end else
    begin
      if tvMain.Items.Count > 0 then
      begin
        tvMain.Selected := tvMain.Items[0];
        Node := tvMain.Items[0];
        fmServerRegistry.Init(TPNodeInfos(Node.Data)^.ServerSession);
      end else
      begin
        fmServerRegistry.Init(nil);
      end;
    end;

    fmServerRegistry.ShowModal;
  finally
    fmServerRegistry.Free;
  end;
end;


procedure TfmMain.lmSetFBClientClick(Sender: TObject);
begin
   //lmDisconnectAllClick(nil);
  turbocommon.SetInitialClientLib;

   //if not SetFBClient(1) then
     //Application.Terminate;
end;

(******************  Set generator value  *********************)
procedure TfmMain.lmSetGenClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AGenName: string;
  OrigValue: string;
  dbIndex: Integer;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    dbIndex:= TPNodeInfos(SelNode.Data)^.dbIndex;

    AGenName:= SelNode.Text;
    SQLQuery1.Close;
    SQLQuery1.SQL.Text:= 'select GEN_ID(' + AGenName + ', 0) from RDB$Database;';

    SQLQuery1.Open;
    OrigValue:= SQLQuery1.Fields[0].AsString;
    SQLQuery1.Close;

    ShowCompleteQueryWindow(dbIndex, 'setGenVal#' +  IntToStr(dbIndex) + ':' + AGenName, 'SET GENERATOR ' + AGenName + ' TO ' + OrigValue);
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
  ShortTitle := TableName;
  ATab.Caption := ShortTitle;
  fmTableManage.Caption := ShortTitle;

  // Mehrzeiliger Tooltip
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Table' + sLineBreak +
    'Table name: ' + TableName + sLineBreak +
    'Modus: Edit Table';

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Initialisierung
  fmTableManage.Init(dbIndex, TableName, SelNode.Data);
  fmTableManage.PageControl1.TabIndex := 0;

  // Teilbereiche laden
  //ViewTableFields(TableName, dbIndex, fmTableManage.sgFields);
  //fmTableManage.FillTriggers;
  //fmTableManage.FillPermissions;
  //fmTableManage.bbRefreshReferencesClick(nil);

  fmTableManage.Show;
end;


procedure TfmMain.lmTestFireBirdFunctionClick(Sender: TObject);
begin
  CallRoutine(rtFBFunc);
end;

procedure TfmMain.lmCallStoreProcClick(Sender: TObject);
begin
  CallRoutine(rtFBProc);
end;

procedure TfmMain.lmTestPackageFunctionClick(Sender: TObject);
begin
  CallRoutine(rtPackageFBFunc);
end;

procedure TfmMain.lmTestPackageProcedureClick(Sender: TObject);
begin
  CallRoutine(rtPackageFBProc);
end;

procedure TfmMain.lmTestUDFFunctionClick(Sender: TObject);
begin
  CallRoutine(rtUDF);
end;

procedure TfmMain.lmTestUDRFunctionClick(Sender: TObject);
begin
  CallRoutine(rtUDRFunc);
end;

procedure TfmMain.lmTestUDRProcedureClick(Sender: TObject);
begin
  CallRoutine(rtUDRProc);
end;

procedure TfmMain.lmTestPackageUDRFunctionClick(Sender: TObject);
begin
  CallRoutine(rtPackageUDRFunc);
end;

procedure TfmMain.lmTestPackageUDRProcedureClick(Sender: TObject);
begin
  CallRoutine(rtPackageUDRProc);
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
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
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

  if Assigned(CurrentIBTransaction) then
    CurrentIBTransaction.CommitRetaining;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);
  if not CurrentIBTransaction.InTransaction then
    CurrentIBTransaction.StartTransaction;
  SQLQuery1.Open;

  if SQLQuery1.RecordCount  > 0 then
  //if not SQLQuery1.IsEmpty then
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

  SQLQuery1.Close;

  if Assigned(CurrentIBTransaction) then
    CurrentIBTransaction.CommitRetaining;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);
  if not CurrentIBTransaction.InTransaction then
    CurrentIBTransaction.StartTransaction;
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

  if Assigned(CurrentIBTransaction) then
    CurrentIBTransaction.CommitRetaining;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);

  if not CurrentIBTransaction.InTransaction then
    CurrentIBTransaction.StartTransaction;
  SQLQuery1.Open;

  if SQLQuery1.RecordCount  > 0 then
  //if not SQLQuery1.IsEmpty then
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

  if SQLQuery1.Active then
    SQLQuery1.Close;

  //if Assigned(CurrentIBTransaction) then
    //CurrentIBTransaction.CommitRetaining;

  Rec := RegisteredDatabases[DatabaseIndex];
  SetConnection(DatabaseIndex);

  SQLQuery1.SQL.Text := Format(QueryTemplate, [APackageName]);
  if  not CurrentIBTransaction.InTransaction then
    CurrentIBTransaction.StartTransaction;
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

Function TfmMain.GetStoredProcBody(DatabaseIndex: Integer; AProcName: string; var SPOwner: string): string;
const
  BodyTemplate =
    'SELECT rdb$procedure_source, rdb$owner_name, rdb$procedure_inputs, rdb$procedure_outputs ' +
    'FROM rdb$procedures WHERE rdb$procedure_name = ''%s'' ';
  ParamTemplate =
    'SELECT sp_param.rdb$parameter_name, sp_param.rdb$field_source, ' +
    'fld.rdb$field_type, fld.rdb$field_sub_type, fld.rdb$field_length, ' +
    'fld.rdb$field_scale, fld.rdb$field_precision, fld.rdb$character_length, ' +
    'sp_param.rdb$parameter_type, sp_param.rdb$parameter_number ' +
    'FROM rdb$procedure_parameters sp_param ' +
    'JOIN rdb$fields fld ON sp_param.rdb$field_source = fld.rdb$field_name ' +
    'WHERE sp_param.rdb$procedure_name = ''%s'' ' +
    'ORDER BY sp_param.rdb$parameter_type, sp_param.rdb$parameter_number';
var
  Rec: TDatabaseRec;
  i: Integer;
  InputParams, OutputParams: Integer;
  Line, ParamName: string;
  BodyList: TStringList;
  ProcSource: string;
begin
  try
    AProcName := UpperCase(AProcName);
    BodyList := TStringList.Create;
    try
      Rec := RegisteredDatabases[DatabaseIndex];
      SetConnection(DatabaseIndex);

      // --- Get inputs / outputs counts ---
      SQLQuery1.Close;
      SQLQuery1.SQL.Text := Format(BodyTemplate, [AProcName]);
      SQLQuery1.Open;
      InputParams := SQLQuery1.FieldByName('rdb$procedure_inputs').AsInteger;
      OutputParams := SQLQuery1.FieldByName('rdb$procedure_outputs').AsInteger;
      ProcSource := Trim(SQLQuery1.FieldByName('rdb$procedure_source').AsString);
      SPOwner := Trim(SQLQuery1.FieldByName('rdb$owner_name').AsString);
      SQLQuery1.Close;

      // --- Build CREATE PROCEDURE header ---
      BodyList.Add('CREATE PROCEDURE ' + AProcName);

      // --- Parameters ---
      SQLQuery1.SQL.Text := Format(ParamTemplate, [AProcName]);
      SQLQuery1.Open;

      // Input parameters
      if InputParams > 0 then
      begin
        BodyList.Add('(');
        i := 1;
        while (not SQLQuery1.EOF) and (i <= InputParams) do
        begin
          if SQLQuery1.FieldByName('rdb$parameter_type').AsInteger = 0 then
          begin
            ParamName := Trim(SQLQuery1.FieldByName('rdb$parameter_name').AsString);
            Line := '  ' + ParamName + ' ' +
              GetFBTypeName(
                SQLQuery1.FieldByName('rdb$field_type').AsInteger,
                SQLQuery1.FieldByName('rdb$field_sub_type').AsInteger,
                SQLQuery1.FieldByName('rdb$field_length').AsInteger,
                SQLQuery1.FieldByName('rdb$field_precision').AsInteger,
                SQLQuery1.FieldByName('rdb$field_scale').AsInteger
              );

            // String length fix
            if SQLQuery1.FieldByName('rdb$field_type').AsInteger in [CharType, CStringType, VarCharType] then
              Line := Line + '(' + SQLQuery1.FieldByName('rdb$character_length').AsString + ')';

            if (InputParams > 1) and (i < InputParams) then
              Line := Line + ',';

            BodyList.Add(Line);
            Inc(i);
          end;
          SQLQuery1.Next;
        end;
        BodyList.Add(')');
      end;

      // Output parameters
      if OutputParams > 0 then
      begin
        BodyList.Add('RETURNS (');
        i := 1;
        SQLQuery1.First; // rewind
        while (not SQLQuery1.EOF) and (i <= OutputParams) do
        begin
          if SQLQuery1.FieldByName('rdb$parameter_type').AsInteger = 1 then
          begin
            ParamName := Trim(SQLQuery1.FieldByName('rdb$parameter_name').AsString);
            Line := '  ' + ParamName + ' ' +
              GetFBTypeName(
                SQLQuery1.FieldByName('rdb$field_type').AsInteger,
                SQLQuery1.FieldByName('rdb$field_sub_type').AsInteger,
                SQLQuery1.FieldByName('rdb$field_length').AsInteger,
                SQLQuery1.FieldByName('rdb$field_precision').AsInteger,
                SQLQuery1.FieldByName('rdb$field_scale').AsInteger
              );

            // String length fix
            if SQLQuery1.FieldByName('rdb$field_type').AsInteger in [CharType, CStringType, VarCharType] then
              Line := Line + '(' + SQLQuery1.FieldByName('rdb$character_length').AsString + ')';

            if (OutputParams > 1) and (i < OutputParams) then
              Line := Line + ',';

            BodyList.Add(Line);
            Inc(i);
          end;
          SQLQuery1.Next;
        end;
        BodyList.Add(')');
      end;

      SQLQuery1.Close;

      // --- Add AS + body ---
      BodyList.Add('AS');
      if ProcSource <> '' then
        BodyList.Add(ProcSource)
      else
      begin
        //BodyList.Add('BEGIN');
        BodyList.Add('  -- procedure body');
        //BodyList.Add('END');
      end;

      Result := BodyList.Text;
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

  if not SQLQuery1.Transaction.InTransaction then
    SQLQuery1.Transaction.StartTransaction;
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
    CurrentIBTransaction.CommitRetaining;
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
{Function TfmMain.GetIndexFields(ATableName, AIndexName: string;
  AQuery: TIBQuery; var FieldsList: TStringList): Boolean;
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
end;}

function TfmMain.GetUDFInfo(DatabaseIndex: Integer; UDFName: string;
  var ModuleName, EntryPoint, Params: string): Boolean;
var
  Rec: TDatabaseRec;
  ModAndEntry: string;
  SepPos: Integer;
begin
  Result := False;
  try
    Rec := RegisteredDatabases[DatabaseIndex];
    SetConnection(DatabaseIndex);

    // Modul und EntryPoint holen
    ModAndEntry := GetUDFModuleNameAndEntryPoint(CurrentIBConnection, UDFName);
    if ModAndEntry = '' then Exit;

    SepPos := Pos(',', ModAndEntry);
    if SepPos > 0 then
    begin
      ModuleName := Copy(ModAndEntry, 1, SepPos - 1);
      EntryPoint := Copy(ModAndEntry, SepPos + 1, MaxInt);
    end
    else
    begin
      ModuleName := ModAndEntry;
      EntryPoint := '';
    end;

    // komplette Parametertyp-Liste holen (inkl. Rückgabeparam)
    Params := GetUDFParams(CurrentIBConnection, UDFName);

    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      CurrentIBConnection.Close;
      Result := False;
    end;
  end;
end;

(***********  Show Query window  ************)
{function TfmMain.ShowQueryWindow(DatabaseIndex: Integer; ATitle: string; ANodeInfos: TPNodeInfos=nil): TfmQueryWindow;
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
  ACaption := ATitle;
  Result := nil;
  NodeQueryWindow := false;

  Rec:= RegisteredDatabases[DatabaseIndex];

  AServer   := 'Server:   ' +  GetAncestorNodeText(tvMain.Selected, 0);
  ADBAlias  := 'DBAlias:  ' +  Rec.RegRec.Title;
  ADBPath   := 'DBPath:   ' +  Rec.RegRec.DatabaseName;
  FullHint  :=  AServer + sLineBreak + ADBAlias + sLineBreak + ADBPath + sLineBreak;

  if ANodeInfos = nil then
  begin
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
      end else
        FullHint := FullHint + 'Object type: '  + TreeViewObjectToStr(ANodeInfos^.ObjectType)
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
end;  }

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

  frmThemeSelector.btnApplyClick(QWindow);

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
  Rec: TRegisteredDatabase;
  Objects: TStringList;
  TableNode, Item, GenNode, TrigNode, ViewsNode :TTreeNode;
  StoredProcNode, UDFNode, FBFunctionNode,
  PackageNode, PackageFuncsNone, PackageProcsNode, PackagesUDFsNode,
  PackagesUDRsNode, PackagesUDRFuncsNode, PackagesUDRProcsNode,
  UDRsNode, UDRsFuncNode, UDRsProcNode, SysTableNode,
  DomainsNode, ExceptionNode: TTreeNode;
  RoleNode, UserNode: TTreeNode;
  i, x: Integer;
  DBIndex: Integer;
  Count, TmpCount: Integer;
  ANodeText: string;
begin
  //DBIndex:= TPNodeInfos(Node.Parent.Data)^.dbIndex;
  DBIndex:= TPNodeInfos(Node.Parent.Data)^.dbIndex;
  Rec:= RegisteredDatabases[DBIndex].RegRec;
  Screen.Cursor:= crSQLWait;
  Objects:= TStringList.Create;
  try //try..finally for making sure Objects is released
    try //try..except for error reporting
      ANodeText:= Node.Text;
      if Pos('(', ANodeText) > 0 then
        ANodeText:= Trim(Copy(ANodeText, 1, Pos('(', ANodeText) - 1));

      // Tables

      if ANodeText = 'Tables' then
      begin
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otTables, Count);
        TableNode:= Node;
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';

        TableNode.DeleteChildren;

        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(TableNode, Objects[i]);
          Item.ImageIndex:= 4;
          Item.SelectedIndex:= 4;
          TPNodeInfos(Item.Data)^.ObjectType := tvotTable;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;

      end
      else
        // Generators
      if ANodeText = 'Generators' then
      begin
        GenNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otGenerators, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        GenNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(GenNode, Objects[i]);
          Item.ImageIndex:= 6;
          Item.SelectedIndex:= 6;
          TPNodeInfos(Item.Data)^.ObjectType := tvotGenerator;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;

      end
      else
        // Triggers
      if Node.Text = 'Triggers' then
      begin
        TrigNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otTriggers, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        TrigNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(TrigNode, Objects[i]);
          Item.ImageIndex:= 8;
          Item.SelectedIndex:= 8;
          TPNodeInfos(Item.Data)^.ObjectType := tvotTrigger;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end
      else
        // Views
      if Node.Text = 'Views' then
      begin
        ViewsNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otViews, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        ViewsNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(ViewsNode, Objects[i]);
          Item.ImageIndex:= 10;
          Item.SelectedIndex:= 10;
          TPNodeInfos(Item.Data)^.ObjectType := tvotView;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end
      else
        // Stored Procedures
      if (Node.Level = 2) and (Node.Text = 'Procedures') then
      begin
        StoredProcNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otStoredProcedures, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        StoredProcNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(StoredProcNode, Objects[i]);
          Item.ImageIndex:= 12;
          Item.SelectedIndex:= 12;
          TPNodeInfos(Item.Data)^.ObjectType := tvotStoredProcedure;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end
      else
        // UDF (UDF-Functions)
      if Node.Text = 'UDFs' then
      begin
        UDFNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otUDF, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        UDFNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(UDFNode, Objects[i]);
          Item.ImageIndex:= 13;
          Item.SelectedIndex:= 13;
          TPNodeInfos(Item.Data)^.ObjectType := tvotUDFFunction;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end
      else
        // System Tables
      if Node.Text = 'System Tables' then
      begin
        SysTableNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otSystemTables, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        SysTableNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(SysTableNode, Objects[i]);
          Item.ImageIndex:= 16;
          Item.SelectedIndex:= 16;
          TPNodeInfos(Item.Data)^.ObjectType := tvotSystemTable;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end

      else
        // Domains
      if Node.Text = 'Domains' then
      begin
        DomainsNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otDomains, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        DomainsNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(DomainsNode, Objects[i]);
          Item.ImageIndex:= 17;
          Item.SelectedIndex:= 17;
          TPNodeInfos(Item.Data)^.ObjectType := tvotDomain;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end

      else
        // Roles
      if Node.Text = 'Roles' then
      begin
        RoleNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otRoles, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        RoleNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(RoleNode, Objects[i]);
          Item.ImageIndex:= 20;
          Item.SelectedIndex:= 20;
          TPNodeInfos(Item.Data)^.ObjectType := tvotRole;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end

      else
        // Exceptions
      if Node.Text = 'Exceptions' then
      begin
        ExceptionNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otExceptions, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
        ExceptionNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(ExceptionNode, Objects[i]);
          Item.ImageIndex:= 22;
          Item.SelectedIndex:= 22;
          TPNodeInfos(Item.Data)^.ObjectType := tvotException;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end
      else

        // Users
      if Node.Text = 'Users' then
      begin
        UserNode:= Node;
        Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otUsers, Count);
        Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')'; // - Public User
        UserNode.DeleteChildren;
        for i:= 0 to Objects.Count - 1 do
        begin
          if Trim(LowerCase(Objects[i])) <> 'public' then   //Hide Public User.
          begin
            Item:= tvMain.Items.AddChild(UserNode, Objects[i]);
            Item.ImageIndex:= 23;
            Item.SelectedIndex:= 23;
            TPNodeInfos(Item.Data)^.ObjectType := tvotUser;
            TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
          end;
        end;
      end

      else
      // FB(FBFunctions)
    if (Node.Level = 2) and (Node.Text = 'Functions') then
    begin
      FBFunctionNode:= Node;
      //Get FBFunctions
      Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otFBFunctions, Count);
      Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
      FBFunctionNode.DeleteChildren;
      if Count > 0 then
      for i:= 0 to Objects.Count - 1 do
      begin
        Item:= tvMain.Items.AddChild(FBFunctionNode, Objects[i]);
        Item.ImageIndex:= 52;
        Item.SelectedIndex:= 52;
        TPNodeInfos(Item.Data)^.ObjectType := tvotFunction;
        TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
      end;
    end

    else
    // Packages
    if Node.Text = 'Packages' then
    begin
      PackageNode:= Node;
      Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otPackages, Count);
      if Objects.Count > 0  then
      begin
      PackageNode.Text := PackageNode.Text + ' (' + IntToStr(Objects.Count) + ')';
      Node.Text:= ANodeText + ' (' + IntToStr(Count) + ')';
      PackageNode.DeleteChildren;
      if Count > 0 then
      begin
        for i:= 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(PackageNode, Objects[i]);
          Item.Text := Objects[i];
          Item.ImageIndex:= 60;
          Item.SelectedIndex:= 60;
          TPNodeInfos(Item.Data)^.ObjectType := tvotPackage;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        end;
      end;

      Item  := Item.Parent.Parent;
      for i := 0 to PackageNode.Count - 1 do
      begin
        Item:= tvMain.Items.AddChild(PackageNode.Items[i], 'Functions');
        TPNodeInfos(Item.Data)^.ObjectType := tvotPackageFunctionRoot;
        TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        Item.ImageIndex := 61;
        Objects.Clear;
        GetPackageFunctions(dbIndex, Item.Parent.Text, Objects);
        Item.Text := Item.Text + ' (' + IntToStr(Objects.Count) + ')';
        for x := 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(Item, Objects[x]);
          Item.ImageIndex := 61;
          TPNodeInfos(Item.Data)^.ObjectType := tvotPackageFunction;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
          Item := Item.Parent;
        end;
      end;

      Item  := Item.Parent.Parent;
      for i := 0 to PackageNode.Count - 1 do
      begin
        Item:= tvMain.Items.AddChild(PackageNode.Items[i], 'Procedures');
        TPNodeInfos(Item.Data)^.ObjectType := tvotPackageProcedureRoot;
        TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        Item.ImageIndex := 62;
        Objects.Clear;
        GetPackageProcedures(dbIndex, Item.Parent.Text, Objects);
        Item.Text := Item.Text + ' (' + IntToStr(Objects.Count) + ')';
        for x := 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(Item, Objects[x]);
          Item.ImageIndex := 62;
          TPNodeInfos(Item.Data)^.ObjectType := tvotPackageProcedure;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
          Item := Item.Parent;
        end;
      end;

      Item  := Item.Parent.Parent;
      for i := 0 to PackageNode.Count - 1 do
      begin
        Item:= tvMain.Items.AddChild(PackageNode.Items[i], 'UDR-Functions');
        TPNodeInfos(Item.Data)^.ObjectType := tvotPackageUDRFunctionRoot;
        TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        Item.ImageIndex := 64;
        Objects.Clear;
        GetPackageUDRFunctions(dbIndex, Item.Parent.Text, Objects);
        Item.Text := Item.Text + ' (' + IntToStr(Objects.Count) + ')';
        for x := 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(Item, Objects[x]);
          Item.ImageIndex := 64;
          TPNodeInfos(Item.Data)^.ObjectType := tvotPackageUDRFunction;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
          Item := Item.Parent;
        end;
      end;

      Item  := Item.Parent.Parent;
      for i := 0 to PackageNode.Count - 1 do
      begin
        Item:= tvMain.Items.AddChild(PackageNode.Items[i], 'UDR-Procedures');
        TPNodeInfos(Item.Data)^.ObjectType := tvotPackageUDRProcedureRoot;
        TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
        Item.ImageIndex := 65;
        Objects.Clear;
        GetPackageUDRProcedures(dbIndex, Item.Parent.Text, Objects);
        Item.Text := Item.Text + ' (' + IntToStr(Objects.Count) + ')';
        for x := 0 to Objects.Count - 1 do
        begin
          Item:= tvMain.Items.AddChild(Item, Objects[x]);
          Item.ImageIndex := 65;
          TPNodeInfos(Item.Data)^.ObjectType := tvotPackageUDRProcedure;
          TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
          Item := Item.Parent;
        end;
      end;

      end else
        PackageNode.Text := PackageNode.Text + ' (0)';
    end

    else
    // UDRs
      if Node.Text = 'UDRs' then
        begin
          UDRsNode:= Node;
          UDRsNode.DeleteChildren;

          //UDRsFuncNode :=  Node.Items[0];
          //UDRsFuncNode.DeleteChildren;

          //Get UDR-Functions
          Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otUDRFunctions, Count);
          if Count > 0 then
          begin
            Item := tvMain.Items.AddChild(UDRsNode, '');
            Item.ImageIndex:= 67;
            TPNodeInfos(Item.Data)^.ObjectType := tvotUDRFunctionRoot;
            TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
            for i:= 0 to Objects.Count - 1 do
            begin
              Item:= tvMain.Items.AddChild(Item, Objects[i]);
              Item.ImageIndex:= 67;
              Item.SelectedIndex:= 67;
              TPNodeInfos(Item.Data)^.ObjectType := tvotUDRFunction;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item := Item.Parent;
            end;
            UDRsNode.Items[0].Text := 'Functions (' + IntToStr(UDRsNode.Items[0].Count) + ')';
          end;

          //Get UDR-Procedures
          //UDRsProcNode := Node.Items[1];
          //UDRsProcNode.DeleteChildren;

          Objects.CommaText:= dmSysTables.GetDBObjectNames(DBIndex, otUDRProcedures, TmpCount);
          if TmpCount > 0 then
          begin
            Item := tvMain.Items.AddChild(UDRsNode, '');
            Item.ImageIndex:= 68;
            TPNodeInfos(Item.Data)^.ObjectType := tvotUDRProcedureRoot;
            TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
            for i:= 0 to Objects.Count - 1 do
            begin
              Item:= tvMain.Items.AddChild(Item, Objects[i]);
              Item.ImageIndex:= 68;
              Item.SelectedIndex:= 68;
              TPNodeInfos(Item.Data)^.ObjectType := tvotUDRProcedure;
              TPNodeInfos(Item.Data)^.dbIndex := DBIndex;
              Item := Item.Parent;
            end;
            UDRsNode.Items[1].Text := 'Procedures (' + IntToStr(UDRsNode.Items[1].Count) + ')';
          end;
          UDRsNode.Text:= ANodeText + ' (' + IntToStr(Count + TmpCount) + ')';
        end;

      if not Node.Expanded then
        Node.Expand(False);
    except
      on E: Exception do
      begin
        Screen.Cursor:= crDefault;
        ShowMessage(e.Message);
      end;
    end;
  finally
    Objects.Free;
    Screen.Cursor:= crDefault;
  end;
end;

(*************  Get main indices information  ******************)
{Function TfmMain.GetIndices(ATableName: string; AQuery: TIBQuery): Boolean;
begin
  AQuery.Close;
  AQuery.SQL.Text:= 'SELECT * FROM RDB$INDICES WHERE RDB$RELATION_NAME=''' + UpperCase(ATableName) +
    ''' AND RDB$FOREIGN_KEY IS NULL';
  AQuery.Open;
  Result:= AQuery.RecordCount > 0;
  //Result := not SQLQuery1.IsEmpty;
  if not Result then
    AQuery.Close;
end;}

(*************   Display View DDL *******************)
procedure TfmMain.lmDisplayViewClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  Rec: TDatabaseRec;
  AViewName: string;
  ViewBody, Columns: string;
  dbIndex: Integer;
  ATab: TTabSheet;
  fmViewView: TfmViewView;
begin
  SelNode:= tvMain.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin

    dbIndex:= TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex;
    Rec:= RegisteredDatabases[dbIndex];
    AViewName:= SelNode.Text;


    // Prüfen ob ViewForm schon existiert
    NodeInfos := TPNodeInfos(SelNode.Data);
    if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmViewView) then
      fmViewView := TfmViewView(NodeInfos^.ViewForm)
    else
    begin
      fmViewView := TfmViewView.Create(Application);
      ATab := TTabSheet.Create(Self);
      ATab.Parent := PageControl1;
      ATab.ImageIndex := SelNode.ImageIndex;
      ATab.Tag:= dbIndex;
      fmViewView.Parent := ATab;
      fmViewView.Align := alClient;
      fmViewView.BorderStyle := bsNone;

      NodeInfos^.ViewForm := fmViewView;
    end;

    // Tab vorbereiten
    ATab := fmViewView.Parent as TTabSheet;

    fmViewView.Align:= alClient;
    fmViewView.SynSQLSyn1.TableNames.CommaText:= GetTableNames(dbIndex);
    fmViewView.Caption:= 'View DDL: ' + AViewName;
    ATab.Caption:= fmViewView.Caption;
    fmViewView.edName.Caption:= AViewName;

    GetViewInfo(dbIndex, AViewName, Columns, ViewBody);
    fmViewView.seScript.Lines.Clear;
    fmViewView.seScript.Lines.Text:= 'create view "' + AviewName + '" (' + Columns + ')' + LineEnding + ViewBody;
    PageControl1.ActivePage:= ATab;

    fmViewView.Init(NodeInfos);
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
  IsoFields, IsoConstraintFields: TIsolatedQuery;
begin
  IsoFields := nil;
  IsoConstraintFields := nil;
  try
    Node := tvMain.Selected;
    dbIndex := TPNodeInfos(Node.Data)^.dbIndex;
    Node.DeleteChildren;

    // Primary Keys
    PKFieldsList := TStringList.Create;
    try
      PKIndexName := GetPrimaryKeyIndexNameIsolated(RegisteredDatabases[dbIndex].IBDatabase, Node.Text, ConstraintName);

      if PKIndexName <> '' then
        //GetConstraintFields(Node.Text, PKIndexName, PKFieldsList);
        IsoConstraintFields := GetConstraintFieldsIsolated(RegisteredDatabases[dbIndex].IBDatabase, Node.Text, PKIndexName, PKFieldsList);

      IsoFields := GetFieldsIsolated(RegisteredDatabases[dbIndex].IBDatabase, Node.Text);
      //GetFields(dbIndex, Node.Text, nil);
      i := 1;
      //with SQLQuery1 do
      with IsoFields.Query do
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
      //SQLQuery1.Close;
      if Assigned(IsoFields) then
        IsoFields.Free;
      Node.Expand(False);
    finally
      PKFieldsList.Free;
      if Assigned(IsoConstraintFields) then
        IsoConstraintFields.Free;
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
    'DBPath:   ' + Rec.IBDatabase.DatabaseName + sLineBreak +
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

procedure TfmMain.lmViewStoredProcedureClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  AProcName, SPOwner, spBody: string;
  dbIndex: Integer;
  ATab: TTabSheet;
  frmViewSProc: TfmViewSProc;
  ShortTitle, FullHint, DBAlias: string;
  Rec: TDatabaseRec;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;

  dbIndex := NodeInfos^.dbIndex;
  Rec := RegisteredDatabases[dbIndex];

  AProcName := SelNode.Text;
  SPOwner := GetObjectOwner(Rec.IBDatabase, AProcName, otStoredProcedures);
  spBody := GetFirebirdProcedureHeader(RegisteredDatabases[dbIndex].IBDatabase, AProcName, '', false) + sLineBreak +
            GetFirebirdProcedureBody(RegisteredDatabases[dbIndex].IBDatabase, AProcName, '');

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmViewSProc) then
    frmViewSProc := TfmViewSProc(NodeInfos^.ViewForm)
  else
  begin
    frmViewSProc := TfmViewSProc.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmViewSProc.Parent := ATab;
    frmViewSProc.Align := alClient;
    frmViewSProc.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmViewSProc;
  end;

  // Tab vorbereiten
  ATab := frmViewSProc.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Tab-Titel
  ShortTitle := AProcName;
  ATab.Caption := ShortTitle;
  frmViewSProc.Caption := ShortTitle;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Stored Procedure' + sLineBreak +
    'Procedure name: ' + AProcName + sLineBreak +
    'Owner: ' + SPOwner;

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form mit Prozedurdaten füllen
  with frmViewSProc do
  begin
    SynSQLSyn1.TableNames.CommaText := GetTableNames(dbIndex);
    edName.Caption := AProcName;
    edOwner.Caption := SPOwner;

    seScript.Lines.Clear;
    seScript.Text := spBody;
  end;

  frmViewSProc.Init(SelNode.Data);
  frmViewSProc.Show;
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
      'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
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

procedure TfmMain.lmViewUDFClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  AFuncName, ModuleName, EntryPoint, Params: string;
  ParamList, ReturnType: string;
  ATab: TTabSheet;
  dbIndex: Integer;
  frmUDFInfo: TfmUDFInfo;
  ShortTitle, FullHint, DBAlias: string;
  CommaPos: Integer;
  Lines: TStringList;
  fmUDFInfo: TfmUDFInfo;
begin
  SelNode := tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) or (SelNode.Parent.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Parent.Parent.Data);
  if NodeInfos = nil then Exit;
  dbIndex := NodeInfos^.dbIndex;

  AFuncName := SelNode.Text;

  // UDF-Infos aus DB holen
  if not GetUDFInfo(dbIndex, AFuncName, ModuleName, EntryPoint, Params) then Exit;

  // Prüfen ob ViewForm schon existiert
  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmUDFInfo) then
    frmUDFInfo := TfmUDFInfo(NodeInfos^.ViewForm)
  else
  begin
    frmUDFInfo := TfmUDFInfo.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    frmUDFInfo.Parent := ATab;
    frmUDFInfo.Align := alClient;
    frmUDFInfo.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmUDFInfo;
  end;

  // Tab vorbereiten
  ATab := frmUDFInfo.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Kurzer Tab-Titel
  ShortTitle := 'UDF: ' + AFuncName;
  ATab.Caption := ShortTitle;
  frmUDFInfo.Caption := ShortTitle;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: UDF' + sLineBreak +
    'Function name: ' + AFuncName + sLineBreak +
    'Module: ' + ModuleName + sLineBreak +
    'Entry: ' + EntryPoint;
  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Form mit Daten füllen
  with frmUDFInfo do
  begin
    edName.Caption := AFuncName;
    edModule.Caption := ModuleName;
    edEntry.Caption := EntryPoint;

    // Rückgabe & Parameterliste trennen
    CommaPos := Pos(',', Params);
    if CommaPos > 0 then
    begin
      ReturnType := Trim(Copy(Params, 1, CommaPos - 1));
      ParamList := Trim(Copy(Params, CommaPos + 1, MaxInt));
    end
    else
    begin
      ReturnType := Trim(Params);
      ParamList := '';
    end;

    meBody.Clear;
    Lines := TStringList.Create;
    try
      Lines.Add('DECLARE EXTERNAL FUNCTION ' + AFuncName + '(');
      if ParamList <> '' then
        Lines.Add('  ' + ParamList);
      Lines.Add(')');
      Lines.Add('RETURNS ' + ReturnType);
      Lines.Add('ENTRY_POINT ''' + EntryPoint + '''');
      Lines.Add('MODULE_NAME ''' + ModuleName + ''';');

      meBody.Lines.Assign(Lines);
    finally
      Lines.Free;
    end;
  end;

  frmUDFInfo.Init(SelNode.Data);
  frmUDFInfo.Show;
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
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Parent.Parent.Data)^.dbIndex, 'Drop Table#:' + IntToStr(dbIndex) + SelNode.Text);
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
  lmCreateDBClick(nil);
end;

procedure TfmMain.lmRegdbClick(Sender: TObject);
begin
  //fmReg.edDatabaseName.Text:= tvMain.Selected.Text + ':';
  mnRegDBClick(nil);
end;

(**********  Register New database  ***********)
procedure TfmMain.mnRegDBClick(Sender: TObject);
var ServerComboBoxIdx: integer;
    ServerRec: TserverRecord;
    ServerNode: TTreeNode;
begin
  if tvMain.Items.Count = 0 then
    exit;

  fmReg.NewReg:= True;
  fmReg.bbReg.Caption:= 'Register Database';

  fmReg.RefreshServerCombobox;

  if tvMain.Selected = nil then
    ServerRec := GetServerRecordFromFileByIndex(0)
  else
    ServerRec := GetServerRecordFromFileByName(tvMain.Selected.Text);

  fmReg.cboxServers.ItemIndex := fmReg.cboxServers.Items.IndexOf(Trim(ServerRec.ServerName));
  fmReg.edTitle.Text := '';
  fmReg.edRole.Text := ServerRec.Role;
  fmReg.cboxSQLDialect.ItemIndex := 2;  //SQLDialect = 3
  fmReg.cbCharset.ItemIndex :=  fmReg.cbCharset.Items.IndexOf(ServerRec.Charset);
  fmReg.edUserName.Text := ServerRec.UserName;
  fmReg.edPassword.Text := ServerRec.Password;
  fmReg.cxSavePassword.Checked := (ServerRec.Password <> '');
  fmReg.chkboxOverwriteServerClientLib.Checked := false;
  fmReg.edtFBClient.Text := ServerRec.ClientLibraryPath;
  fmReg.edtPort.Text := ServerRec.Port;

  fmReg.chkBoxConnectDBOnStart.Checked := false;

  if fmReg.ShowModal = mrOK then
  begin
    LoadRegisteredDatabases;
    fmReg.SaveRegistrations;
    LoadRegisteredDatabases;
    ServerNode := GetServerNodeByServerName(ServerRec.ServerName);
    if ServerNode <> nil then
      ServerNode.Expand(false);
  end;
end;

(************* Edit Registration  *************)
procedure TfmMain.lmEditDBRegClick(Sender: TObject);
var
  Rec: TRegisteredDatabase;
  SelNode: TTreeNode;
  ServerName: string;
begin
  if tvMain.Items.Count = 0 then exit;
  if tvMain.Selected = nil then exit;
  if tvMain.Selected.Level <> 1 then exit;

  fmReg.RefreshServerCombobox;

  SelNode := tvMain.Selected;
  ServerName := SelNode.Parent.Text;

  if SelNode <> nil then
  begin
    fmReg.NewReg:= False;
    fmReg.bbReg.Caption:= 'Save';
    fmreg.RecPos:= RegisteredDatabases[TPNodeInfos(SelNode.Data)^.dbIndex].Index;

    Rec:= RegisteredDatabases[TPNodeInfos(SelNode.Data)^.dbIndex].OrigRegRec;

    fmReg.edDatabaseName.Text:= Rec.DatabaseName;
    fmReg.edTitle.Text:= Rec.Title;
    fmReg.edUserName.Text:= Rec.UserName;
    fmReg.edPassword.Text:= Rec.Password;
    fmReg.cbCharset.Text:= Rec.Charset;
    fmReg.edRole.Text:= Rec.Role;
    fmReg.cxSavePassword.Checked:= Rec.SavePassword;
    fmReg.edtFBClient.Text := Rec.FireBirdClientLibPath;
    fmReg.edtPort.Text := Rec.Port;

    fmReg.chkboxOverwriteServerClientLib.Checked := Rec.OverwriteLoadedClientLib;
    fmReg.chkBoxConnectDBOnStart.Checked := Rec.ConnectOnApplicationStart;

    fmReg.cboxServers.ItemIndex := fmReg.cboxServers.Items.IndexOf(Rec.ServerName);

    if fmReg.ShowModal = mrOK then
    begin
      LoadRegisteredDatabases;
      fmReg.SaveRegistrations;
      LoadRegisteredDatabases;
      SelNode := GetServerNodeByServerName(ServerName);
      if SelNode <> nil then
        SelNode.Expand(false);
    end;
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

(****************  Unregister database *************)
procedure TfmMain.lmUnregisterDatabaseClick(Sender: TObject);
var DBNode, SerVerNode: TTreeNode;
    Rec: TDatabaseRec;
    Title: string;
    ServerName: string;
begin
  if tvMain.Items.Count = 0 then exit;
  if tvMain.Selected = nil then exit;
  if tvMain.Selected.Level <> 1 then exit;

  DBNode := GetAncestorAtLevel(tvMain.Selected, 1);
  if (DBNode <> nil) then
  begin
    if MessageDlg('Are you sure you want to Unregister this database ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ServerNode := DBNode.Parent;
      ServerName := ServerNode.Text;
      If RegisteredDatabases[TPNodeInfos(DBNode.Data)^.dbIndex].IBDatabase.Connected then
        CloseDB(TPNodeInfos(DBNode.Data)^.dbIndex);
      Title := RegisteredDatabases[TPNodeInfos(DBNode.Data)^.dbIndex].RegRec.Title;
      DeleteDBRegistrationFromFile(Title);
      LoadRegisteredDatabases;
      ServerNode := GetServerNodeByServerName(ServerName);
      if ServerNode <> nil then
        ServerNode.Expand(false);
    end;
  end;
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
    QWindow:= ShowQueryWindow(TPNodeInfos(SelNode.Data)^.dbIndex,  SelNode.Text,  SelNode.Data);
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
  dbIndex: integer;
begin
  SelNode:= tvMain.Selected;
  dbIndex := TPNodeInfos(SelNode.Parent.Data)^.dbIndex;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow:= ShowQueryWindow(dbIndex, 'New Role#' + IntToStr(dbIndex));
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('CREATE ROLE role_name;');
    QWindow.Show;
  end;
end;

{procedure TfmMain.PageControl1CloseTabClicked(Sender: TObject);
var
  i: Integer;
  tmpCloseAction: TCloseAction;
begin
  for i:= 0 to Application.ComponentCount - 1 do
    if Application.Components[i] is TfmQueryWindow then
    begin
      (Application.Components[i] as TfmQueryWindow).lmCloseTabClick(nil);
      Break;
    end;
end;}

procedure TfmMain.PageControl1CloseTabClicked(Sender: TObject);
var
  i: Integer;
  ts: TTabSheet;
  ctrl: TControl;
begin
  // Ermittle, welches Tab geschlossen werden soll
  ts := PageControl1.ActivePage;
  if ts = nil then Exit;

  if ts.TabIndex = 0 then
    exit;

  // Falls im Tab ein Formular eingebettet ist, schließe es korrekt
  for i := 0 to ts.ControlCount - 1 do
  begin
    ctrl := ts.Controls[i];
    if ctrl is TForm then
    begin
      // Formular-spezifische Logik aufrufen
      if ctrl is TfmQueryWindow then
        (ctrl as TfmQueryWindow).lmCloseTabClick(nil)
      //else if ctrl is TfmDBInfo then
        //(ctrl as TfmDBInfo).Close // Beispiel
      else
        (ctrl as TForm).Close; // Standard
    end
    else
      ctrl.Free; // Sonstige Controls (Frames, Panels etc.)
  end;

  // Danach das Tab selbst freigeben
  if not (ctrl is TfmQueryWindow) then
    ts.Free;
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
    //ShowMessage(TreeViewObjectToStr(TPNodeInfos(SelNode.Data)^.ObjectType));

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
    if TPNodeInfos(SelNode.Data)^.ObjectType = tvotStoredProcedure then
    //if ParentNodeText = 'Procedures' then // Stored Proc
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

  //ShowMessage(TreeViewObjectToStr(TPNodeInfos(SelNode.Data)^.ObjectType));

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
    tmpProtocol: TProtocol;
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
    begin

      {if Node.Text = 'localhost' then
        tmpProtocol := ptEmbedded
      else
        tmpProtocol := ptTCPIP; }

      tmpProtocol := TCP;

      PNodeInfos^.ServerSession := TServerSession.Create(Node.Text,
                                                         '',  //           Rec.ServerAlias,
                                                         '',  //           Rec.UserName,
                                                         '',  //           Rec.Password,
                                                         '',  //           Rec.Role,
                                                         tmpProtocol,  //  Rec.Protocol,
                                                         '0',            //Port
                                                         'UTF8',       //CharSet
                                                         '',  //           Rec.ClientLibraryPath,
                                                         '',  //           Rec.ConfigFilePath
                                                         0,   //Minor
                                                         0,    //Major
                                                          false, //        Rec.LoadRegisteredClient
                                                          false, //       Rec.IsEmbedded
                                                          2000,  //       Rec.CoonctionTimeout
                                                             3,  //       Rec.RetryCout
                                                             0   //       Rec.QueryTimeOut.

                                                             );
    end;

  end;
  Node.Data := PNodeInfos;
end;

procedure TfmMain.tvMainChange(Sender: TObject; Node: TTreeNode);
var dbIndex: integer;
begin
  if tvMain.Selected = nil then
    exit;

  if tvMain.Selected.Level > 0 then
  begin
    //dbIndex:= TPNodeInfos(tvMain.Selected.Data)^.dbIndex;
    dbIndex:= TPNodeInfos(Node.Data)^.dbIndex;
    SetConnection(dbIndex);
  end;
end;

procedure TfmMain.tvMainClick(Sender: TObject);
var dbIndex: integer;
begin
  {if tvMain.Selected = nil then
    exit;

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
      if ServerSession.Connected then
        ServerSession.Disconnect;
      ServerSession.Free;
      ServerSession := nil; // Dangling-Pointer vermeiden
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

// ============================================================================
// Liest ODS-Version ausschließlich per SQL aus einer offenen Datenbank
// ============================================================================
function TfmMain.ReadODSViaSQL(dbIndex: Integer; out ODSMajor, ODSMinor: Integer): Boolean;
var
  qry: TIBQuery;
begin
  Result := False;
  ODSMajor := 0;
  ODSMinor := 0;

  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then
    Exit;

  if not RegisteredDatabases[dbIndex].IBDatabase.Connected then
    Exit; // DB muss offen sein

  qry := TIBQuery.Create(nil);
  try
    qry.DataBase := RegisteredDatabases[dbIndex].IBDatabase;
    qry.Transaction := RegisteredDatabases[dbIndex].IBTransaction;
    qry.SQL.Text := 'SELECT MON$ODS_MAJOR, MON$ODS_MINOR FROM MON$DATABASE';
    qry.Open;
    if not qry.EOF then
    begin
      ODSMajor := qry.Fields[0].AsInteger;
      ODSMinor := qry.Fields[1].AsInteger;
      Result := True;
    end;
    qry.Close;
  finally
    qry.Free;
  end;
end;

// ============================================================================
// Versucht die ODS-Version zu lesen:
// - wenn DB offen → ReadODSViaSQL
// - wenn DB geschlossen → ReadODSFromFile
// ============================================================================
function TfmMain.TryReadODS(dbIndex: Integer; out ODSMajor, ODSMinor: Integer): Boolean;
var
  Rec: TRegisteredDatabase;
begin
  Result := False;
  ODSMajor := 0;
  ODSMinor := 0;

  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then
    Exit;

  Rec := RegisteredDatabases[dbIndex].RegRec;

  // zuerst über SQL versuchen
  if ReadODSViaSQL(dbIndex, ODSMajor, ODSMinor) then
  begin
    Result := True;
    Exit;
  end;

  // sonst aus Datei lesen
  Result := ReadODSFromFile(Rec.DatabaseName, ODSMajor, ODSMinor);
end;

/// ============================================================================
// Liest ODS-Version aus einer offenen Datenbank (SQL) oder aus Datei, falls geschlossen
// ============================================================================
function TfmMain.GetODSVersion(dbIndex: Integer; out ODSMajor, ODSMinor: Integer): Boolean;
var
  qry: TIBQuery;
  Rec: TRegisteredDatabase;
begin
  Result := False;
  ODSMajor := 0;
  ODSMinor := 0;

  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then Exit;
  Rec := RegisteredDatabases[dbIndex].RegRec;

  // 1) Offene DB → ODS per SQL holen
  if RegisteredDatabases[dbIndex].IBDatabase.Connected then
  begin
    qry := TIBQuery.Create(nil);
    try
      qry.DataBase := RegisteredDatabases[dbIndex].IBDatabase;
      qry.Transaction := RegisteredDatabases[dbIndex].IBTransaction;
      qry.SQL.Text := 'SELECT MON$ODS_MAJOR, MON$ODS_MINOR FROM MON$DATABASE';
      qry.Open;
      if not qry.EOF then
      begin
        ODSMajor := qry.Fields[0].AsInteger;
        ODSMinor := qry.Fields[1].AsInteger;
        Result := True;
      end;
      qry.Close;
    finally
      qry.Free;
    end;
  end
  else
    // 2) Geschlossene DB → ODS aus Datei lesen
    Result := ReadODSFromFile(Rec.DatabaseName, ODSMajor, ODSMinor);
end;

// ============================================================================
// Prüft offene DBs vor einem Client-Swap und schließt inkompatible DBs
// ============================================================================
procedure TfmMain.CheckOpenDBsBeforeClientSwap(TargetDB: string);
var
  dbIndex: Integer;
  TargetODSMajor, TargetODSMinor: Integer;
  OpenODSMajor, OpenODSMinor: Integer;
  DBPath: string;
begin
  // Ziel-DB → ODS aus Datei
  if not ReadODSFromFile(TargetDB, TargetODSMajor, TargetODSMinor) then Exit;

  for dbIndex := 0 to Length(RegisteredDatabases) - 1 do
  begin
    if RegisteredDatabases[dbIndex].IBDatabase.Connected then
    begin
      DBPath := RegisteredDatabases[dbIndex].RegRec.DatabaseName;

      if GetODSVersion(dbIndex, OpenODSMajor, OpenODSMinor) then
      begin
        // Nur prüfen, wenn es nicht dieselbe DB ist
        if not SameFileName(DBPath, TargetDB) then
        begin
          if (OpenODSMajor <> TargetODSMajor) or (OpenODSMinor <> TargetODSMinor) then
          begin
            // Nicht kompatibel → still schließen
            CloseDB(dbIndex, True); // Silent = True
          end;
        end;
      end;
    end;
  end;
end;

function  TfmMain.GetServerLoginDlg(AServerName: string): TfrmLoginServiceManager;
var LoginForm: TfrmLoginServiceManager;
begin
  LoginForm := TfrmLoginServiceManager.Create(nil);
  LoginForm.lbSever.Caption := AServerName;
  LoginForm.edtUserName.Text := 'SYSDBA';
  LoginForm.edtPassword.Text := '';
  Result := LoginForm;
end;

function TfmMain.ConnectToServiceManager(ServerSession: TServerSession): Boolean;
var
  fmServerRegistry: TfmServerRegistry;
  ServerRecord: TServerRecord;
  mr: TModalResult;
  SavePwd: Boolean;
begin
  Result := False;

    if not ServerSession.Connected then
    begin
      ServerRecord := GetServerRecordFromFileByName(ServerSession.ServerName);
      ApplyServerRecordToSession(ServerRecord,  ServerSession);

      // zuerst versuchen mit gespeicherten Credentials
      if (not ServerSession.IBXConnect) then
      begin
      // falls fehlgeschlagen -> interaktives Login
        fmServerRegistry := TfmServerRegistry.Create(self);
        fmServerRegistry.init(ServerSession);
        try
          repeat
            mr := fmServerRegistry.ShowModal;

            if mr = mrCancel then
              Exit(False);

            SavePwd := fmServerRegistry.chkSavePassword.Checked;
            ServerSession.UserName := fmServerRegistry.edtUserName.Text;
            ServerSession.Password := fmServerRegistry.edtPassword.Text;

            if not ServerSession.IBXConnect then
              ShowMessage('Connection failed. Please try again.');

          until ServerSession.Connected;

        finally
          fmServerRegistry.Free;
        end;
      end;
      // Versionsinfo übernehmen
    end;
  Result := ServerSession.Connected;
end;

function TfmMain.ConnectEmbedded(ServerSession: TServerSession): Boolean;
var
  fmServerRegistry: TfmServerRegistry;
  ServerRecord: TServerRecord;
  mr: TModalResult;
  SavePwd: Boolean;
  ODSMajor,
  ODSMinor: integer;
  ODSMajorMinorStr: string;
  ServerVersion: string;
begin
  Result := False;

  //ServerSession.Connected := false;

  //ServerRecord := GetServerRecordFromFileByName(ServerSession.ServerName);
  //ApplyServerRecordToSession(ServerRecord,  ServerSession);



    if not ServerSession.Connected then
    begin
      ServerRecord := GetServerRecordFromFileByName(ServerSession.ServerName);
      ApplyServerRecordToSession(ServerRecord,  ServerSession);

      ServerSession.Connected := TestEmbeddedConnection(ServerRecord, ODSMajor, ODSMinor, ServerVersion);
      if (not ServerSession.Connected) then
      begin
      // falls fehlgeschlagen -> interaktives Login
        fmServerRegistry := TfmServerRegistry.Create(self);
        fmServerRegistry.init(ServerSession);
        try
          repeat
            mr := fmServerRegistry.ShowModal;

            if mr = mrCancel then
              Exit(False);

            SavePwd := fmServerRegistry.chkSavePassword.Checked;
            ServerSession.UserName := fmServerRegistry.edtUserName.Text;
            ServerSession.Password := fmServerRegistry.edtPassword.Text;

            ServerSession.Connected := TestEmbeddedConnection(ServerRecord, ODSMajor, ODSMinor, ServerVersion);

            if not ServerSession.Connected then
              ShowMessage('Embedded Connection failed. Please try again.');

          until ServerSession.Connected;

        finally
          fmServerRegistry.Free;
        end;
      end;
      // Versionsinfo übernehmen
      //ODSMajorMinorStr := ODSVersionToFBVersion(ODSMajor, ODSMinor);
      //ServerSession.FBVersionMajor := StrToInt(ODSMajorMinorStr[1]);
      //ServerSession.FBVersionMinor := StrToInt(ODSMajorMinorStr[3]);
      //fbcommon.FBVersionMajor  := StrToInt(ODSMajorMinorStr[1]);
      //fbcommon.FBVersionMinor  := StrToInt(ODSMajorMinorStr[3]);
      //fbcommon.FBVersionString := ''; //ServerSession.FBVersionString;
    end;

  Result := ServerSession.Connected;
end;

// ============================================================================
// Called when the user tries to expand a database node (Level 1).
// Here we check the connection and optionally prevent expansion.
// ============================================================================
procedure TfmMain.tvMainExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  Rec: TRegisteredDatabase;
  NodeInfo: TPNodeInfos;
  ServerNode: TTreeNode;
  ServerSession: TServerSession;
  ServerRec: TServerRecord;
  SavePwd: boolean;
  ServerErrStr: string;
begin

  AllowExpansion := True;

  if (Node = nil) or (Node.Data = nil) then
    Exit;

  NodeInfo := Node.Data;

  // Database nodes are always Level 1
  if Node.Level = 1 then
  begin

    if not IsServerReachable(Node.Parent.Text, ServerErrStr) then
    begin
      AllowExpansion := false;
      MessageDlg(ServerErrStr, mtError, [mbOK], 0);
      Exit;
    end;

    Rec := RegisteredDatabases[NodeInfo^.dbIndex].RegRec;

    // Only if no password is set → ask for connection
    if Rec.Password = '' then
    begin
      SavePwd := false;
      if not ConnectToDBAs(NodeInfo^.dbIndex) then
      begin
        //Connection failed → do not expand
        AllowExpansion := False;
        Exit;
      end;
    end else
      SavePwd := true;

    Rec := RegisteredDatabases[NodeInfo^.dbIndex].RegRec;  //neu lesen, in ConnectToDBAs werden User und pwd gespeichert

    if not RegisteredDatabases[NodeInfo^.dbIndex].IBDatabase.Connected then
    begin
      RegisteredDatabases[NodeInfo^.dbIndex].IBDatabase.Params.Values['user_name'] := Rec.UserName;
      RegisteredDatabases[NodeInfo^.dbIndex].IBDatabase.Params.Values['password'] := Rec.Password;
      RegisteredDatabases[NodeInfo^.dbIndex].IBDatabase.LoginPrompt := false;
      RegisteredDatabases[NodeInfo^.dbIndex].IBDatabase.Connected := true;
    end;

    //wenn connect erfolg den server infos holen und in serverfile speichern
    ServerNode := GetAncestorAtLevel(Node, 0);
    ServerSession := TServerSession(TPNodeInfos(ServerNode.Data)^.ServerSession);

    if ServerSession.FBVersionMajor = 0 then
    begin
      ServerSession.UserName := Rec.UserName;
      ServerSession.Password := Rec.Password;
      if ServerSession.IBXConnect then
      begin
        Rec.ServerVersionMajor := ServerSession.FBVersionMajor;
        Rec.ServerVersionMinor := ServerSession.FBVersionMinor;
        Rec.ServerVersionString := ServerSession.FBVersionString;
        ServerRec := BuildServerRecordFromSession(ServerSession, SavePwd);
        SaveServerDataToFile(ServerRec);
      end;
    end;

    if (Node.Items[0].Text = 'Loading...') then
    begin
      Node.Items[0].Delete;
      AddRootObjects(Node, ServerSession.FBVersionMajor);
    end;

    SetConnection(NodeInfo^.dbIndex);

    // Connection successful → update timestamp
    RegisteredDatabases[NodeInfo^.dbIndex].RegRec.LastOpened := Now;
    RegisteredDatabases[NodeInfo^.dbIndex].OrigRegRec.LastOpened := Now;
  end;
end;

// ============================================================================
// Called when a child node (e.g. Tables, Procedures) is expanded.
// Here we populate the database objects.
// ============================================================================
procedure TfmMain.tvMainExpanded(Sender: TObject; Node: TTreeNode);
var
  BracketPos: Integer;
  NodeInfo: TPNodeInfos;
begin
  if Node = nil then
    Exit;

  NodeInfo := TPNodeInfos(Node.Data);

  {if not RegisteredDatabases[NodeInfo^.dbIndex].IBConnection.Connected then
    RegisteredDatabases[NodeInfo^.dbIndex].IBConnection.Connected := true;

  if not RegisteredDatabases[NodeInfo^.dbIndex].SQLTrans.Active then
    RegisteredDatabases[NodeInfo^.dbIndex].SQLTrans.StartTransaction;
 }

  // Only child nodes (below the database node)
  if (Node.Level > 1) and (not Node.Expanded) then
  begin
    if Node.HasChildren then
    begin
      Node.DeleteChildren;
      BracketPos := Pos('(', Node.Text);
      if BracketPos > 0 then
        Node.Text := Trim(Copy(Node.Text, 1, BracketPos - 1));
    end;

    try
      FillObjectRoot(Node);
    except
      on E: Exception do
        ShowMessage('Error while loading objects: ' + E.Message);
    end;
  end;
end;

(**********************            Double click        *********************************)
procedure TfmMain.tvMainDblClick(Sender: TObject);
var
  Node: TTreeNode;
  Info: TPNodeInfos;
  QWindow: TfmQueryWindow;
  DBIndex: Integer;
  DBPath: string;
  AllowExpanding: boolean;
begin
  Node := tvMain.Selected;

  if Node = nil then Exit;

  Info := TPNodeInfos(Node.Data);
  if Info = nil then Exit;

  DBIndex := Info^.dbIndex;

  if Node.Level = 2 then
  begin
    try
      if tvMain.Selected.Text = 'Query Window' then
      begin
        QWindow:= ShowQueryWindow(TPNodeInfos(tvMain.Selected.Data)^.dbIndex, 'SQLdb#:' + IntToStr(DBIndex), tvMain.Selected.Data);
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
      // Table
      // ----------------------
      tvotTable:
        begin
          try
            lmViewFieldsClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening table fields: ' + E.Message);
          end;

          try
            lmViewFirst1000Click(nil);
          except
            on E: Exception do
              ShowMessage('Error while loading first 1000 rows: ' + E.Message);
          end;
        end;

      // ----------------------
      // Table Field
      // ----------------------
      tvotTableField:
        begin
          try
            lmEditFieldClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while editing field: ' + E.Message);
          end;
        end;

      // ----------------------
      // Generator
      // ----------------------
      tvotGenerator:
        begin
          try
            lmViewGenClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening generator: ' + E.Message);
          end;
        end;

      // ----------------------
      // Trigger
      // ----------------------
      tvotTrigger:
        begin
          try
            //lmViewTriggerClick(nil);
            lmEditTriggerClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening trigger: ' + E.Message);
          end;
        end;

      // ----------------------
      // View
      // ----------------------
      tvotView:
        begin
          try
            lmDisplay1000VClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening view: ' + E.Message);
          end;
        end;

      // ----------------------
      // Stored Procedure
      // ----------------------
      tvotStoredProcedure:
        begin
          try
            //lmCallStoreProcClick(nil);
            lmEditProcClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening stored procedure: ' + E.Message);
          end;
        end;

      // ----------------------
      // UDF Function
      // ----------------------
      tvotUDFFunction:
        begin
          try
            //lmTestUDFFunctionClick(nil);
            lmEditUDFFuctionClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening UDF function: ' + E.Message);
          end;
        end;

      // ----------------------
      // Firebird Function
      // ----------------------
      tvotFunction:
        begin
          try
            //lmTestFireBirdFunctionClick(nil);
            ImEditFBFunctionClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening Firebird function: ' + E.Message);
          end;
        end;

      // ----------------------
      // System Table
      // ----------------------
      tvotSystemTable:
        begin
          try
            lmViewFieldsClick(nil);
            lmOpenSystemTableClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening system table: ' + E.Message);
          end;
        end;

      // ----------------------
      // Domain
      // ----------------------
      tvotDomain:
        begin
          try
            lmViewDomainClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening domain: ' + E.Message);
          end;
        end;

      // ----------------------
      // Role
      // ----------------------
      tvotRole:
        begin
          try
            lmPermissionsClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening role permissions: ' + E.Message);
          end;
        end;

      // ----------------------
      // Exception
      // ----------------------
      tvotException:
        begin
          try
            lmScriptExceptionClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening exception: ' + E.Message);
          end;
        end;

      // ----------------------
      // User
      // ----------------------
      tvotUser:
        begin
          try
            lmPermissionsClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening user permissions: ' + E.Message);
          end;
        end;

      // ----------------------
      // UDR Function
      // ----------------------
      tvotUDRFunction:
        begin
          try
            //lmTestUDRFunctionClick(nil);
            lmEditUDRFunctionClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening UDR function: ' + E.Message);
          end;
        end;

      // ----------------------
      // UDR Procedure
      // ----------------------
      tvotUDRProcedure:
        begin
          try
            //lmTestUDRProcedureClick(nil);
            lmEditUDRProcedureClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening UDR procedure: ' + E.Message);
          end;
        end;

      // ----------------------
      // Package Function
      // ----------------------
      tvotPackageFunction:
        begin
          try
            lmTestPackageFunctionClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening package function: ' + E.Message);
          end;
        end;

      // ----------------------
      // Package Procedure
      // ----------------------
      tvotPackageProcedure:
        begin
          try
            lmTestPackageProcedureClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening package procedure: ' + E.Message);
          end;
        end;

      // ----------------------
      // Package UDR Function
      // ----------------------
      tvotPackageUDRFunction:
        begin
          try
            lmTestPackageUDRFunctionClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening package UDR function: ' + E.Message);
          end;
        end;

      // ----------------------
      // Package UDR Procedure
      // ----------------------
      tvotPackageUDRProcedure:
        begin
          try
            lmTestPackageUDRProcedureClick(nil);
          except
            on E: Exception do
              ShowMessage('Error while opening package UDR procedure: ' + E.Message);
          end;
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

procedure TfmMain.AddRootObjects(ANode: TTreeNode; AServerVersion: word);
var CNode: TTreeNode;
    ServerNode: TTreeNode;
    ServerSession: TServerSession;
begin
  CNode:= tvMain.Items.AddChild(ANode, 'Query Window');
  CNode.ImageIndex:= 1;
  CNode.SelectedIndex:= 1;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotQueryWindow;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'Tables');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotTableRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;
  CNode.ImageIndex:= 2;
  CNode.SelectedIndex:= 2;

  CNode:= tvMain.Items.AddChild(ANode, 'Generators');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotGeneratorRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;
  CNode.ImageIndex:= 6;
  CNode.SelectedIndex:= 6;

  CNode:= tvMain.Items.AddChild(ANode, 'Triggers');
  TPNodeInfos(CNode.Data)^.ObjectType := tvotTriggerRoot;
  CNode.ImageIndex:= 8;
  CNode.SelectedIndex:= 8;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotTriggerRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'Views');
  CNode.ImageIndex:= 10;
  CNode.SelectedIndex:= 10;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotViewRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'UDFs');
  CNode.ImageIndex:= 14;
  CNode.SelectedIndex:= 14;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotUDFRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'Procedures');
  CNode.ImageIndex:= 12;
  CNode.SelectedIndex:= 12;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotStoredProcedureRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;


  //if RegisteredDatabases[TPNodeInfos(ANode.Data)^.dbIndex].RegRec.ServerVersionMajor >= 3 then
  //if ServerSession.FBVersionMajor >= 3 then
  if AServerVersion >= 3 then
  begin
    CNode:= tvMain.Items.AddChild(ANode, 'Functions');
    CNode.ImageIndex:= 52;
    CNode.SelectedIndex:= 52;
    TPNodeInfos(CNode.Data)^.ObjectType := tvotFunctionRoot;
    TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

    CNode:= tvMain.Items.AddChild(ANode, 'UDRs');
    CNode.ImageIndex:= 66;
    CNode.SelectedIndex:= 66;
    TPNodeInfos(CNode.Data)^.ObjectType := tvotUDRRoot;
    TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

    CNode:= tvMain.Items.AddChild(ANode, 'Packages');
    CNode.ImageIndex:= 60;
    CNode.SelectedIndex:= 60;
    TPNodeInfos(CNode.Data)^.ObjectType := tvotPackageRoot;
    TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

      {CNode:= tvMain.Items.AddChild(CNode, 'Functions');
      CNode.ImageIndex:= 52;
      CNode.SelectedIndex:= 52;
      TPNodeInfos(CNode.Data)^.ObjectType := tvotUDRFunctionRoot;
      TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;
      CNode := CNode.Parent;

      CNode:= tvMain.Items.AddChild(CNode, 'Procedures');
      CNode.ImageIndex:= 53;
      CNode.SelectedIndex:= 53;
      TPNodeInfos(CNode.Data)^.ObjectType := tvotUDRProcedureRoot;
      TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;
      CNode := CNode.Parent;}
  end;

  CNode:= tvMain.Items.AddChild(ANode, 'System Tables');
  CNode.ImageIndex:= 15;
  CNode.SelectedIndex:= 15;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotSystemTableRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'Domains');
  CNode.ImageIndex:= 17;
  CNode.SelectedIndex:= 17;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotDomainRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'Exceptions');
  CNode.ImageIndex:= 22;
  CNode.SelectedIndex:= 22;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotExceptionRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'Roles');
  CNode.ImageIndex:= 19;
  CNode.SelectedIndex:= 19;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotRoleRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;

  CNode:= tvMain.Items.AddChild(ANode, 'Users');
  CNode.ImageIndex:= 30;
  CNode.SelectedIndex:= 30;
  TPNodeInfos(CNode.Data)^.ObjectType := tvotUserRoot;
  TPNodeInfos(CNode.Data)^.dbIndex := TPNodeInfos(ANode.Data)^.dbIndex;
end;

(**********************             Load databases            *********************************)
Function TfmMain.LoadRegisteredDatabases: Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;
  MainNode, CNode: TTreeNode;
  DummyNode: TTreeNode;
  i: Integer;
  AServerName: string;
  ServerNode: TTreeNode;
  ServerRecord: TServerRecord;
  ClientLibPath: string;
begin
  try
    tvMain.Items.Clear;

    LoadRegisteredServers;
    if Length(RegisteredServers) = 0 then
      exit(false);
    if Length(RegisteredDatabases) > 0 then
      ReleaseRegisteredDatabases;

    FileName:= getConfigurationDirectory + DatabasesRegFile;

    // Copy old configuration file
    if not FileExists(FileName) and (FileExists(ChangeFileExt(ParamStr(0), '.reg'))) then
    begin
      CopyFile(ChangeFileExt(ParamStr(0), '.reg'), FileName);
    end;

    if FileExists(FileName) then
    begin
      AssignFile(F, FileName);
      Reset(F);
      i:= 0;
      while not Eof(F) do
      begin
        Read(F, Rec);
        if not Rec.Deleted then
        begin
          SetLength(RegisteredDatabases, Length(RegisteredDatabases) + 1);
          with RegisteredDatabases[high(RegisteredDatabases)] do
          begin
            RegRec:= Rec;
            OrigRegRec:= Rec;
            Index:= FilePos(F) - 1;

            if Rec.OverwriteLoadedClientLib then
              ClientLibPath := RegRec.FireBirdClientLibPath
            else begin
              AServerName := Rec.ServerName;
              ServerRecord := GetServerRecordFromFileByName(AServerName);
              ClientLibPath := ServerRecord.ClientLibraryPath;
            end;
            IBDatabase := TIBDatabase.Create(nil);
            IBDatabase.FirebirdLibraryPathName := ClientLibPath;

            IBTransaction:= TIBTransaction.Create(nil);
            IBTransaction.DefaultDatabase := IBDatabase;
            IBDatabase.DefaultTransaction := IBTransaction;

            IBQuery := TIBQuery.Create(nil);
            IBQuery.Database := IBDatabase;
            IBQuery.Transaction := IBTransaction;
            IBQuery.AllowAutoActivateTransaction := true;

            IBDatabase.DatabaseName:= Rec.DatabaseName;
            IBDatabase.Connected := False;
            IBDatabase.DatabaseName := Rec.DatabaseName;

            IBDatabase.Params.Clear;

            IBDatabase.Params.Add('user_name=' + Rec.UserName);
            IBDatabase.Params.Add('password=' + Rec.Password);

            if Rec.Role <> '' then
              IBDatabase.Params.Add('sql_role_name=' + Rec.Role);

            if Rec.Charset <> '' then
              IBDatabase.Params.Add('lc_ctype=' + Rec.Charset);

            IBDatabase.Params.Add('sql_dialect=' + Rec.SQLDialect);
            IBDatabase.Params.Add('isc_dpb_num_buffers=1024');
            IBDatabase.Params.Add('isc_dpb_force_write=1');

            IBDatabase.DefaultTransaction := IBTransaction;
            IBDatabase.LoginPrompt := (Rec.Password = '');

            IBDatabaseInfo := TIBDatabaseInfo.Create(nil);
            IBDatabaseInfo.Database := IBDatabase;

            //IBDatabase.Connected := True;
            //IBTransaction.StartTransaction;
          end;

          // Server node
          //AServerName:= GetServerName(Rec.DatabaseName);
          AServerName := Rec.ServerName;
          ServerNode:= GetServerNodeByServerName(AServerName);

          {if ServerNode = nil then // Add new Server node
          begin
            tvMain.Items.Add(nil, '');
            ServerNode:= tvMain.Items.Add(nil, AServerName);
            ServerNode.ImageIndex:= 25;
            ServerNode.SelectedIndex:= 26;
            TPNodeInfos(ServerNode.Data)^.ObjectType := tvotServer;
          end;}

          // Display databases
          MainNode:= tvMain.Items.AddChild(ServerNode, Rec.Title);
          MainNode.ImageIndex:= 3;
          MainNode.SelectedIndex:= 3;
          TPNodeInfos(MainNode.Data)^.dbIndex := i;
          TPNodeInfos(MainNode.Data)^.ObjectType := tvotDB;
          DummyNode := tvMain.Items.AddChild(MainNode, 'Loading...');

          tvMain.PopupMenu:= pmDatabase;
          tbCheckDBIntegrity.Enabled := true;

           Inc(i);
        end;

      end;
      CloseFile(F);

      //if Length(RegisteredDatabases) > 0 then
        //SetConnection(0);

      // Add spaces at end of tree
      //tvMain.Items.Add(nil, '');
      //tvMain.Items.Add(nil, '');
      //tvMain.Items.Add(nil, '');
    end;
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
  //self.Resize;
  //self.Repaint;
  //tvMain.Repaint;
end;

function TfmMain.LoadRegisteredServers: Boolean;
var
  F: file of TServerRecord;
  Rec: TServerRecord;
  ServerNode: TTreeNode;
  NodeInfo: TPNodeInfos;
  i: Integer;
  FileName: string;
  ServerSession: TServerSession;
begin
  Result := False;
  tvMain.Items.Clear;
  SetLength(RegisteredServers, 0);
  try
    FileName := getConfigurationDirectory + ServersRegFile;
    if not FileExists(FileName) then
      Exit;

    AssignFile(F, FileName);
    Reset(F);
    i := 0;
    while not Eof(F) do
    begin
      Read(F, Rec);

      // In Array aufnehmen
      SetLength(RegisteredServers, Length(RegisteredServers)+1);
      RegisteredServers[High(RegisteredServers)] := Rec;

      // TreeView-Knoten erstellen
      ServerNode := tvMain.Items.Add(nil, Rec.ServerName);
      //ServerNode := tvMain.Items.Add(nil, Rec.ServerAlias);
      tvMain.Items.Add(nil, '');
      ServerNode.ImageIndex := 25;
      ServerNode.SelectedIndex := 25;
      NodeInfo := TPNodeInfos(ServerNode.Data);

      NodeInfo^.ObjectType := tvotServer;

      ServerSession := TServerSession(NodeInfo^.ServerSession).Create(           Rec.ServerName,
                                                                                 Rec.ServerAlias,
                                                                                 Rec.UserName,
                                                                                 Rec.Password,
                                                                                 Rec.Role,
                                                                                 Rec.Protocol,
                                                                                 Rec.Port,
                                                                                 Rec.Charset,
                                                                                 Rec.ClientLibraryPath,
                                                                                 Rec.ConfigFilePath,
                                                                                 Rec.VersionMinor,
                                                                                 Rec.VersionMajor,
                                                                                 Rec.LoadRegisteredClientLib,
                                                                                 Rec.IsEmbedded,
                                                                                 Rec.ConnectTimeoutMS,
                                                                                 Rec.RetryCount,
                                                                                 Rec.QueryTimeoutMS
                                                                           );

      Inc(i);
    end;
    CloseFile(F);
    Result := True;
  except
    on E: Exception do
      ShowMessage('LoadRegisteredServers error: ' + E.Message);
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
  if not SQLQuery1.Transaction.InTransaction then
    SQLQuery1.Transaction.StartTransaction;
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
  if not SQLQuery1.Database.Connected then
    SQLQuery1.Database.Connected := true;
  if not SQLQuery1.Transaction.InTransaction then
    SQLQuery1.Transaction.StartTransaction;
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


procedure AdjustTreeViewSignColor(ATreeView: TTreeView);
var
  R, G, B: Byte;
  Brightness: Double;
  BgColor: TColor;
begin
  if ATreeView = nil then Exit;

  // Hintergrundfarbe holen
  BgColor := ColorToRGB(ATreeView.Color);

  // RGB-Komponenten extrahieren
  R := GetRValue(BgColor);
  G := GetGValue(BgColor);
  B := GetBValue(BgColor);

  // Helligkeit berechnen (Wahrnehmungskorrektur)
  Brightness := (0.299 * R + 0.587 * G + 0.114 * B);

  // Wenn hell → schwarze Zeichen, sonst weiß
  if Brightness > 180 then
    ATreeView.ExpandSignColor := clBlack
  else
    ATreeView.ExpandSignColor := clWhite;
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

