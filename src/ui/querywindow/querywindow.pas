unit QueryWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpstdexports, FileUtil,   LMessages,
  LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, PairSplitter,
  StdCtrls, Buttons, DBGrids, Menus, ComCtrls, SynEdit, SynHighlighterSQL, Reg,
  SynEditTypes, SynCompletion, Clipbrd, grids, DbCtrls, types, LCLType,
  dbugintf, turbocommon, variants, strutils, IniFiles, fpdataexporter, LR_Class,
  QBuilder, QBEIBX,  {, QBESqlDb, QBEZEOS, ZConnection, ZCompatibility, ZDatasetUtils; }
  LR_DSet,

  typinfo,

  IBDynamicGrid, IBQuery, IBDatabase, IBTable, IB, RxDBGrid, RxDBGridExportPdf,
  RxDBGridPrintGrid, RxDBGridExportSpreadSheet, ibxscript,

  fdataexportersintrf,
  //fblobedit,
  uthemeselector,
  fsimpleobjextractor,
  cUnIntelliSenseCache,
  cSelectSQLParserExt;

type

  TQueryTypes = (
    qtUnknown=0,
    qtSelectable=1,
    qtExecute=2,
    qtScript=3);

  TQueryActions = (
    qaCommit,
    qaCommitRet,
    qaRollBack,
    qaRollbackRet,
    qaOpen,
    qaDDL,
    qaExec );


  { TQueryThread }

  TQueryThread = class(TThread)
    private
      FSQLQuery: TIBQuery;
      FTrans: TIBTransaction;
      FConnection: TIBDatabase;

    public
      Error: Boolean;
      ErrorMsg: string;
      fTerminated: Boolean;
      fType: TQueryActions;
      fStatement: string;
      property Query: TIBQuery read FSQLQuery write FSQLQuery;
      property Trans: TIBTransaction read FTrans write FTrans;
      property Connection: TIBDatabase read FConnection write FConnection;
      property Statement: String read fStatement write fStatement;

      procedure DoJob;
      procedure Execute; override;
      constructor Create(aType: TQueryActions);

      function CanCommit: Boolean;
  end;


  { TfmQueryWindow }

  TfmQueryWindow = class(TForm)
    cxAutoCommit: TCheckBox;
    FindDialog1: TFindDialog;
    FontDialog1: TFontDialog;
    FIBConnection: TIBDatabase;
    FontDialogEditor: TFontDialog;
    FSQLTrans: TIBTransaction;
    lmExportDataSet: TMenuItem;
    lmExportDataAsMarkDownTable: TMenuItem;
    lmStdExportFormats: TMenuItem;
    lmExportDataAsHtml: TMenuItem;
    lmExportToClipboard: TMenuItem;
    lmExportDataAsPDF: TMenuItem;
    lmPrintData: TMenuItem;
    lmExportDataAsSpreadSheet: TMenuItem;
    pmUnIntelliSense: TPopupMenu;
    rgScreenModes: TRadioGroup;
    RxDBGridExportPDF1: TRxDBGridExportPDF;
    RxDBGridExportSpreadSheet1: TRxDBGridExportSpreadSheet;
    RxDBGridPrint1: TRxDBGridPrint;
    SaveDialogPDF: TSaveDialog;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    SynAutoComplete1: TSynAutoComplete;
    tbCommit: TToolButton;
    tbCommitRetaining: TToolButton;
    tbHistory: TToolButton;
    tbMenu: TToolButton;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbQueryDesign: TToolButton;
    tbRollback: TToolButton;
    tbRollbackRetaining: TToolButton;
    tbRun: TToolButton;
    tbSave: TToolButton;
    ToolBar1: TToolBar;
    toolbarImages: TImageList;
    imTools: TImageList;
    imTabs: TImageList;
    lmCloseTab: TMenuItem;
    lmCopy: TMenuItem;
    lmPaste: TMenuItem;
    lmSelectAll: TMenuItem;
    lmUndo: TMenuItem;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    lmCut: TMenuItem;
    lmRedo: TMenuItem;
    MenuItem2: TMenuItem;
    lmFind: TMenuItem;
    lmFindAgain: TMenuItem;
    MenuItem3: TMenuItem;
    lmCopyCell: TMenuItem;
    MenuItem5: TMenuItem;
    lmRun: TMenuItem;
    lmRunSelect: TMenuItem;
    lmRunExec: TMenuItem;
    lmRunScript: TMenuItem;
    OpenDialog1: TOpenDialog;
    pgOutputPageCtl: TPageControl;
    pnlOutputPanel: TPanel;
    pmTab: TPopupMenu;
    pmMemo: TPopupMenu;
    pmGrid: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    meQuery: TSynEdit;
    SynCompletion1: TSynCompletion;
    SynSQLSyn1: TSynSQLSyn;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure bbRunClick(Sender: TObject);
    procedure cxAutoCommitMouseEnter(Sender: TObject);
    procedure cxAutoCommitMouseLeave(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lmCloseTabClick(Sender: TObject);
    procedure lmCopyCellClick(Sender: TObject);
    procedure lmCopyClick(Sender: TObject);
    procedure lmCutClick(Sender: TObject);
    procedure lmExportDataAsHtmlClick(Sender: TObject);
    procedure lmExportDataAsMarkDownTableClick(Sender: TObject);
    procedure lmExportDataAsPDFClick(Sender: TObject);
    procedure lmExportDataAsSpreadSheetClick(Sender: TObject);
    procedure lmExportToClipboardClick(Sender: TObject);
    procedure lmPasteClick(Sender: TObject);
    procedure lmPrintDataClick(Sender: TObject);
    procedure lmRedoClick(Sender: TObject);
    procedure lmRunClick(Sender: TObject);
    procedure lmRunExecClick(Sender: TObject);
    procedure lmRunScriptClick(Sender: TObject);
    procedure lmRunSelectClick(Sender: TObject);
    procedure lmSelectAllClick(Sender: TObject);
    procedure lmStdExportFormatsClick(Sender: TObject);
    procedure lmUndoClick(Sender: TObject);
    procedure lmFindClick(Sender: TObject);
    procedure lmFindAgainClick(Sender: TObject);
    procedure meQueryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure meQueryMouseEnter(Sender: TObject);
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure pgOutputPageCtlChange(Sender: TObject);
    procedure pmGridPopup(Sender: TObject);
    procedure pmUnIntelliSensePopup(Sender: TObject);
    procedure rgScreenModesClick(Sender: TObject);
    procedure rgScreenModesMouseEnter(Sender: TObject);

    procedure SQLScript1Exception(Sender: TObject; Statement: TStrings;
      TheException: Exception; var Continue: boolean);
    procedure SynCompletion1CodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    procedure tbCloseClick(Sender: TObject);
    procedure tbCommitClick(Sender: TObject);
    procedure tbCommitMouseEnter(Sender: TObject);
    procedure tbCommitRetainingClick(Sender: TObject);
    procedure tbCommitRetainingMouseEnter(Sender: TObject);
    procedure tbHistoryClick(Sender: TObject);
    procedure tbHistoryMouseEnter(Sender: TObject);
    procedure tbMenuClick(Sender: TObject);
    procedure tbMenuMouseEnter(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbNewMouseEnter(Sender: TObject);
    procedure tbNewMouseLeave(Sender: TObject);
    procedure tbOpenClick(Sender: TObject);
    procedure tbOpenMouseEnter(Sender: TObject);
    procedure tbQueryDesignMouseEnter(Sender: TObject);
    procedure tbRollbackClick(Sender: TObject);
    procedure tbRollbackMouseEnter(Sender: TObject);
    procedure tbRollbackRetainingClick(Sender: TObject);
    procedure tbRollbackRetainingMouseEnter(Sender: TObject);
    procedure tbRunClick(Sender: TObject);
    procedure tbRunMouseEnter(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure tbQueryDesignClick(Sender: TObject);
    procedure tbSaveMouseEnter(Sender: TObject);
    procedure ToolBar1MouseEnter(Sender: TObject);
    procedure ToolBar1MouseLeave(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
  private
    { private declarations }
    FDBIndex: Integer; // Index of selected registered database
    FRegRec: TRegisteredDatabase;
    FOptions: set of TSynSearchOption;
    FCanceled: Boolean;
    FStartLine: Integer;
    FQuery: TStringList; //query text
    FOrigQueryType: TQueryTypes;
    FFinished: Boolean;
    FQT: TQueryThread;
    FQueryPart: string;
    FTab: TTabSheet;
    FResultMemo: TMemo;
    FSQLScript: TIBXScript;
    // Text for caption
    FAText: string;
    FModifyCount: Integer;
    FCounter: Integer;
    OutputTabsList: TStrings;

    FNodeInfos: TPNodeInfos;
    // Makes commit button in current tabsheet visible
    procedure EnableCommitButton;
    procedure ExecuteQuery;
    function GetNewTabNum: string;
    // Gets TIBQuery of current result tabsheet - only if it is a select query
    function GetCurrentSelectQuery: TIBQuery;
    // Gets both querytype and whether SQL is DML or DDL
    // Investigates QueryList[LookAtIndex] to find out
    function GetQuerySQLType(QueryList: TStringList; var LookAtIndex: Integer;
      var IsDDL: Boolean): TQueryTypes;
    procedure NewCommitButton(const Pan: TPanel; var ATab: TTabSheet);
    procedure RemoveComments(QueryList: TStringList; StartLine: Integer;
      var RealStartLine: Integer);
    procedure RemoveAllSingleLineComments(QueryList: TStringList);
    procedure RemoveEmptyLines(QueryList: TStringList;
      var SecondRealStart: Integer; const RealStartLine: Integer);
    procedure ApplyClick(Sender: TObject);
    procedure EnableApplyButton;
    function GetTableName(SQLText: string): string;
    procedure CommitResultClick(Sender: TObject);
    procedure RemovePreviousResultTabs;

    procedure LoadpmUnIntelliSense;
    procedure pmUnIntelliSenseMaintemClick(Sender: TObject);
    procedure pmUnIntelliSenseSubItemClick(Sender: TObject);
    function  GetFieldsForAlias(const AliasName: string;
      Cache: TUnIntelliSenseCache; Parser: TSelectSQLParserExt): TStringList;

    //RxDBGrid////////////////////////////
    function  BuildOrderBy(Grid: TRxDBGrid; QuoteNames: Boolean = True): string;
    procedure ApplyGridSort(Grid: TRxDBGrid);
    procedure SaveGridSort(Grid: TRxDBGrid);
    procedure RxDBGridSortControllerTitleClick(Column: TColumn);
    /////////////////////////////////////////

  protected
    // This procedure will receive the events that are logged by the connection:
  public
    OnCommit: TNotifyEvent;
    procedure Init(dbIndex: Integer; ANodeInfos: TPNodeInfos=nil);
    function GetQueryType(AQuery: string): TQueryTypes;
    // Get query text from GUI/memo into
    // QueryContents
    function GetQuery(QueryContents: tstrings): boolean;
    function CreateResultTab(QueryType: TQueryTypes; var aSqlQuery: TIBQuery; var aSQLScript: TIBXScript;
      var meResult: TMemo; AdditionalTitle: string = ''): TTabSheet;

    procedure OnFIBXScriptSelectSQL(Sender: TObject; SQLText: string);
    function ExecuteScript(Script: string): Boolean;

    // Create a new Apply button in the specified panel
    procedure NewApplyButton(var Pan: TPanel; var ATab: TTabSheet);
    // Returns whether query is DDL or DML
    function GetSQLType(Query: string; var Command: string): string;
    // Tries to split up text into separate queries
    function GetSQLSegment(QueryList: TStringList; StartLine: Integer;
      var QueryType: TQueryTypes; var EndLine: Integer;
      var SQLSegment: string; var IsDDL: Boolean): Boolean;
    procedure QueryAfterPost(DataSet: TDataSet);
    procedure QueryAfterScroll(DataSet: TDataSet);
    // Run query; use aQueryType to force running as e.g. script or open query
    procedure CallExecuteQuery(aQueryType: TQueryTypes);
    procedure SortSynCompletion;
    procedure ThreadTerminated(Sender: TObject);
    procedure EnableButtons;

    { public declarations }
  end; 


type
  TSortInfo = record
    FieldName: string;
    SortOrder: TSortMarker;  // ← korrekt
  end;

var
  SavedSort: array of TSortInfo;

implementation

uses main, SQLHistory;

{ TfmQueryWindow }
{ NewCommitButton: Create commit button for editable query result }


procedure TfmQueryWindow.NewCommitButton(const Pan: TPanel; var ATab: TTabSheet);
var
  Commit: TBitBtn;
begin
  Commit:= TBitBtn.Create(self);
  Commit.Parent:= Pan;
  Commit.Caption:= 'Commit'; //don't change this; code looks for this exact caption
  Commit.Left:= 400;
  Commit.Visible:= False;
  Commit.OnClick:= @CommitResultClick;
  Commit.Tag:= ATab.TabIndex;
end;


{ RemoveComments: Remove comments from Query window }

procedure TfmQueryWindow.RemoveComments(QueryList: TStringList; StartLine: Integer; var RealStartLine: Integer);
var
  Comment: Boolean;
  i: Integer;
  MultiComment: Boolean;
begin
  MultiComment:= False;
  for i:= StartLine to QueryList.Count - 1 do
  begin
    if Pos('/*', Trim(QueryList[i])) = 1 then
    begin
      MultiComment:= True;
      Comment:= False;
    end;

    // Avoid checking for comments if there's any chance they're within
    // a string literal e.g. select 'this is -- no -- comment' from rdb$database
    if (not MultiComment) and (pos('''',QueryList[i])=0) then
      Comment:= Pos('--', Trim(QueryList[i])) = 1;

    if (Trim(QueryList[i]) <> '') and (not Comment) and (not MultiComment) then
    begin
      RealStartLine:= i;
      Break;
    end;

    if MultiComment and (Pos('*/', QueryList[i]) > 0) then // End of multi-line comment
    begin
      QueryList[i]:= Trim(Copy(QueryList[i], Pos('*/', QueryList[i]) + 2, Length
        (QueryList[i])));
      RealStartLine:= i;
      MultiComment:= False;
      Comment:= False;
      if (i = QueryList.Count - 1) or
         ((Trim(QueryList[i + 1]) <> '') and  (Pos('/*', Trim(QueryList[i + 1])
           ) <> 1) and
         (Pos('--', Trim(QueryList[i + 1])) <> 1)) then
          Break;
    end;
  end;
end;


{ RemoveAllSingleLineComments: remove single line comments from query }

procedure TfmQueryWindow.RemoveAllSingleLineComments(QueryList: TStringList);
var
  i: Integer;
begin
  for i:= QueryList.Count - 1 downto 0 do
  begin
    if Pos('--', QueryList[i]) > 0 then
    begin
      if Pos('--', Trim(QueryList[i])) = 1 then
        QueryList.Delete(i);
      {
      else
        // this will also pick up -- within string literals which is wrong
        QueryList[i]:= Copy(QueryList[i], 1, Pos('--', QueryList[i]) - 1);
      }
    end;
  end;
end;


{ RemoveEmptyLines: remove empty lines in query }

procedure TfmQueryWindow.RemoveEmptyLines(QueryList: TStringList; var SecondRealStart: Integer;
  const RealStartLine: Integer);
var
  i: integer;
begin
  for i:= RealStartLine to QueryList.Count - 1 do
  begin
    if Trim(QueryList[i]) <> '' then
    begin
      SecondRealStart:= i;
      Break;
    end;
  end;
end;


{ ApplyClick: Save Updates for the query }

procedure TfmQueryWindow.ApplyClick(Sender: TObject);
var
  i, x: Integer;
  TableName: string;
  UpdateQuery: TIBQuery;
  PKIndexName: string;
  ConstraintName: string;
  KeyList, FieldsList: TStringList;
  WhereClause: string;
  UserData: TIBQuery;
  TabIndex: Integer;
  FieldsSQL: string;
begin
  try
    TabIndex:= pgOutputPageCtl.TabIndex;
    UserData:= nil;
    UserData:= GetCurrentSelectQuery;
    // Better safe than sorry
    if not(Assigned(UserData)) then
    begin
      ShowMessage('Error getting query from tabsheet.');
      {$IFDEF DEBUG}
      SendDebug('ApplyClick: GetRecordSet call returned nil recordset');
      {$ENDIF}
      exit;
    end;
    UserData.ApplyUpdates; // lets query run InsertSQL, UpdateSQL, DeleteSQL

    (Sender as TBitBtn).Visible:= False;

    // Auto commit
    if cxAutoCommit.Checked then
      FSQLTrans.Commit
    else
      EnableCommitButton;

    UserData.EnableControls;
  except
    on E: Exception do
    begin
      ShowMessage('Error trying to save data: ' + e.Message);
    end;
  end;
end;

{ EnableApplyButton: enable save updates button on current tab when records have been modified }

procedure TfmQueryWindow.EnableApplyButton;
var
  i: Integer;
  Ctl: TControl;
  ParentPanel: TPanel;
begin
  // The page has a panel that contains the button
  ParentPanel:=nil;
  for i:= 0 to pgOutputPageCtl.ActivePage.ControlCount-1 do
  begin
    Ctl:=pgOutputPageCtl.ActivePage.Controls[i];
    if Ctl is TPanel then
    begin
      ParentPanel:= TPanel(Ctl); //found
      break;
    end;
  end;
  // Found the hosting panel; this should have the Apply button
  // as well as the commit button and the tdbnavigator
  if assigned(ParentPanel) then
  begin
    for i:= 0 to ParentPanel.ControlCount-1 do
    begin
      Ctl:=ParentPanel.Controls[i];
      if (Ctl is TBitBtn) and
        ((Ctl as TBitBtn).Caption = 'Apply') then
      begin
        (Ctl as TBitBtn).Visible:= true;
        Break;
      end;
    end;
  end;
end;


{ EnableCommitButton: enable commit button after applying updates }

procedure TfmQueryWindow.EnableCommitButton;
var
  i: Integer;
  Ctl: TControl;
  ParentPanel: TPanel;
begin
  // The page has a panel that contains the button
  ParentPanel:=nil;
  for i:= 0 to pgOutputPageCtl.ActivePage.ControlCount-1 do
  begin
    Ctl:=pgOutputPageCtl.ActivePage.Controls[i];
    if Ctl is TPanel then
    begin
      ParentPanel:= TPanel(Ctl); //found
      break;
    end;
  end;
  // Found the hosting panel; this should have the Apply, Commit button
  // as well as the navigator
  if assigned(ParentPanel) then
  begin
    for i:= 0 to ParentPanel.ControlCount-1 do
    begin
      Ctl:=ParentPanel.Controls[i];
      if (Ctl is TBitBtn) and
        ((Ctl as TBitBtn).Caption = 'Commit') then
      begin
        (Ctl as TBitBtn).Visible:= true;
        Break;
      end;
    end;
  end;
end;


{ GetTableName: get table name from query text }

function TfmQueryWindow.GetTableName(SQLText: string): string;
begin
  SQLText:= Trim(Copy(SQLText, Pos('from', LowerCase(SQLText)) + 4, Length(SQLText)));
  if Pos('"', SQLText) = 1 then
  begin
    Delete(SQLText, 1, 1);
    Result:= Copy(SQLText, 1, Pos('"', SQLText) - 1);
  end
  else
  begin
    if Pos(' ', SQLText) > 0 then
      Result:= Copy(SQLText, 1, Pos(' ', SQLText) - 1)
    else
      Result:= SQLText;
  end;
  if Pos(';', Result) > 0 then
    Delete(Result, Pos(';', Result), 1);
end;


{ CommitResultClick: commit current transaction }

procedure TfmQueryWindow.CommitResultClick(Sender: TObject);
begin
  FSQLTrans.CommitRetaining;
  (Sender as TBitBtn).Visible:= False;
end;

{ GetCurrentSelectQuery: return result recordset of a page tab }

function TfmQueryWindow.GetCurrentSelectQuery: TIBQuery;
var
  i: Integer;
  Ctl: TControl;
begin
  // Tabsheet's tag property should point to any select query
  Result:= nil;
  if (pgOutputPageCtl.PageCount > 0) then
  begin
    if (pgOutputPageCtl.ActivePage.Tag<>0) then
    begin
      Result:= TIBQuery(pgOutputPageCtl.ActivePage.Tag);
    end;
  end;
end;


{ GetQuerySQLType: get query type: select, script, execute from current string list }

function TfmQueryWindow.GetQuerySQLType(QueryList: TStringList; var LookAtIndex: Integer; var IsDDL: Boolean): TQueryTypes;
var
  MassagedSQL: string;
begin
  Result:= qtUnknown;
  IsDDL:= False; //default
  if LookAtIndex < QueryList.Count then
  begin
    MassagedSQL:= LowerCase(Trim(QueryList[LookAtIndex]));

    // Script overrides rest
    if Pos('set term', MassagedSQL) = 1 then
    begin
      // Using set term does not mean the SQL you're running has to be
      // DDL (could be an execute block or something) but it most probably is
      IsDDL:= true;
      exit(qtScript);
    end;

    if (Pos('select', MassagedSQL) = 1) then
      { todo: (low priority) misses insert...returning,
       update...returning, merge.. returning...}
      Result:= qtSelectable
    else
    begin
      Result:= qtExecute;
      IsDDL:= (Pos('alter', MassagedSQL) = 1) or
        (Pos('create', MassagedSQL) = 1) or
        (Pos('drop', MassagedSQL) = 1) or
        (Pos('grant', MassagedSQL) = 1) {actually DCL} or
        (Pos('revoke', MassagedSQL) = 1) {actually DCL};
    end;
  end;
end;

{ TQueryThread }

function TQueryThread.CanCommit: Boolean;
begin
  if not Assigned(FSQLQuery) then
    Exit(False);

  case FSQLQuery.StatementType of
    SQLSelect,
    SQLSelectForUpdate,
    SQLExecProcedure:  // stored procedure → lieber nicht automatisch committen!
      Result := False;

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

{ DoJob: Execute thread job: open query, execute, commit, rollback, etc }

procedure TQueryThread.DoJob;
begin
  try
    if fType = qaOpen then
      FSQLQuery.Open
    else
    if fType = qaExec then
      FSQLQuery.ExecSQL
    else
    if fType = qaDDL then
    begin
      //MetaDataChanged := true;
      FSQLQuery.SQL.Text := Trim(AnsiString(fStatement));
      FSQLQuery.ExecSQL;
    end
    else
    if fType = qaCommit then
      FTrans.Commit
    else
    if fType = qaCommitRet then
      FTrans.CommitRetaining
    else
    if fType = qaRollBack then
      FTrans.Rollback
    else
    if fType = qaRollbackRet then
      FTrans.RollbackRetaining;

    Error:= False;
    fTerminated:= True;
  except
    on E: Exception do
    begin
      Error:= True;
      ErrorMsg:= e.Message;
      fTerminated:= True;
    end;
  end;
end;

{procedure TQueryThread.DoJob;
begin
  try
    if fType = qaOpen then
      FSQLQuery.Open

    else if fType = qaExec then
      FSQLQuery.ExecSQL

    else if fType = qaDDL then
    begin
      FSQLQuery.SQL.Text := Trim(AnsiString(fStatement));
      FSQLQuery.ExecSQL;
    end

    else if fType = qaCommit then
    begin
      if CanCommit then
        FTrans.Commit
      else
        raise Exception.Create('Cannot commit: current statement is not committable.');
    end

    else if fType = qaCommitRet then
    begin
      if CanCommit then
        FTrans.CommitRetaining
      else
        raise Exception.Create('Cannot commit: current statement is not committable.');
    end

    else if fType = qaRollBack then
      FTrans.Rollback

    else if fType = qaRollbackRet then
      FTrans.RollbackRetaining;

    Error := False;
    fTerminated := True;
  except
    on E: Exception do
    begin
      Error := True;
      ErrorMsg := E.Message;
      fTerminated := True;
    end;
  end;
end;}

{ Execute: Query thread main loop }

procedure TQueryThread.Execute;
begin
  try
    fTerminated:= False;
    Error:= False;
    DoJob;
    fTerminated:= True;
  except
    on E: Exception do
    begin
      Error:= True;
      ErrorMsg:= e.Message;
      fTerminated:= True;
    end;
  end;
end;


{ Create query thread }

constructor TQueryThread.Create(aType: TQueryActions);
begin
  inherited Create(True);
  fType:= aType;
  FreeOnTerminate:= False;
end;


{ Display SQL script exception message }

procedure TfmQueryWindow.SQLScript1Exception(Sender: TObject;
  Statement: TStrings; TheException: Exception; var Continue: boolean);
begin
  ShowMessage('Error running script: '+TheException.Message);
end;


procedure TfmQueryWindow.SynCompletion1CodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  SynCompletion1.Deactivate;
end;

{ Close button pressed: close current Query window and free parent page tab }

procedure TfmQueryWindow.tbCloseClick(Sender: TObject);
begin
end;

{ Commit current transaction }

procedure TfmQueryWindow.tbCommitClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TIBQuery;
  SqlScript: TIBXScript;
  ATab: TTabSheet;
  QT: TQueryThread;
begin
  ATab:= CreateResultTab(qtExecute, SqlQuery, SqlScript, meResult);
  QT:= TQueryThread.Create(qaCommit);
  try
    QT.Trans:= FSQLTrans;
    if not FSQLTrans.InTransaction then
      //FSQLTrans.StartTransaction;
      exit;


    ATab.ImageIndex:= 6;

    // Run thread
    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated;

    if QT.Error then
    begin
      ATab.ImageIndex:= 3;
      meResult.Lines.Text:= QT.ErrorMsg;
      meResult.Font.Color:= clRed;
    end
    else
    begin
      ATab.ImageIndex:= 4;
      meResult.Lines.Add('Commited');
      meResult.Font.Color:= clGreen;


      {if not NeedsCommit(QT.FSQLQuery) then
        exit;
      if not NeedsCommit(SqlQuery) then
        exit;}

      // Call OnCommit procedure if assigned, it is used to refresh table management view
      if OnCommit <> nil then
        OnCommit(self);
      OnCommit:= nil;
    end;

  finally
    QT.Free;
  end;

end;

{procedure TfmQueryWindow.tbCommitClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TIBQuery;
  SqlScript: TIBXScript;
  ATab: TTabSheet;
  QT: TQueryThread;
begin
  ATab := CreateResultTab(qtExecute, SqlQuery, SqlScript, meResult);

  // ⛔ Kein Commit, wenn nie etwas ausgeführt wurde
  if not Assigned(SqlQuery) then
  begin
    meResult.Lines.Add('Commit skipped: no active statement.');
    meResult.Font.Color := clGray;
    Exit;
  end;

  // ⛔ Kein Commit, wenn letztes Statement kein DML/DDL war

  if not NeedsCommit(SqlQuery) then
  begin
    meResult.Lines.Add(
      'Commit skipped: last statement type = ' +
      GetEnumName(TypeInfo(TIBSQLStatementTypes), Ord(SqlQuery.StatementType))
    );
    meResult.Font.Color := clGray;
    Exit;
  end;

  // 🔥 Commit durchführen
  QT := TQueryThread.Create(qaCommit);
  try
    QT.Trans := FSQLTrans;
    ATab.ImageIndex := 6;

    QT.Resume;

    repeat
      Application.ProcessMessages;
    until QT.fTerminated;

    if QT.Error then
    begin
      ATab.ImageIndex := 3;
      meResult.Lines.Text := QT.ErrorMsg;
      meResult.Font.Color := clRed;
    end
    else
    begin
      ATab.ImageIndex := 4;
      meResult.Lines.Add('Committed.');
      meResult.Font.Color := clGreen;

      if Assigned(OnCommit) then
        OnCommit(Self);
      OnCommit := nil;
    end;

  finally
    QT.Free;
  end;
end;}

procedure TfmQueryWindow.tbCommitMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;


{ Commit retaining for current transaction }

procedure TfmQueryWindow.tbCommitRetainingClick(Sender: TObject);
var
  QT: TQueryThread;
begin
  QT:= TQueryThread.Create(qaCommitRet);
  try
    QT.Trans:= FSQLTrans;

    // Run thread
    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated;

    if QT.Error then
      ShowMessage('Error trying commit retaining: '+QT.ErrorMsg)
    else
    begin
      // Call OnCommit procedure if assigned, it is used to refresh table management view
      if OnCommit <> nil then
        OnCommit(self);
      OnCommit:= nil;
    end;

  finally
    QT.Free;
  end;
end;

procedure TfmQueryWindow.tbCommitRetainingMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

{HistoryClick: show SQL history form }
procedure TfmQueryWindow.tbHistoryClick(Sender: TObject);
var Node: TTreeNode;
begin
  fmSQLHistory.Init(FRegRec.Title, Self, nil);
  fmSQLHistory.ShowModal;
end;

procedure TfmQueryWindow.tbHistoryMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

{procedure TfmQueryWindow.tbHistoryClick(Sender: TObject);
var
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  ATab: TTabSheet;
  frmSQLHistory: TfmSQLHistory;
  ShortTitle, FullHint: string;
begin
  SelNode := fmMain.tvMain.Selected;
  if (SelNode = nil) or (SelNode.Parent = nil) or (SelNode.Parent.Parent = nil) then Exit;

  NodeInfos := TPNodeInfos(SelNode.Data);
  if NodeInfos = nil then Exit;

  // Prüfen ob das Fenster schon existiert

  if Assigned(NodeInfos^.ViewForm) and (NodeInfos^.ViewForm is TfmSQLHistory) then
    frmSQLHistory := TfmSQLHistory(NodeInfos^.ViewForm)
  else
  begin
    frmSQLHistory := TfmSQLHistory.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := fmMain.PageControl1;
    ATab.ImageIndex := -1; // optional eigenes Icon setzen
    frmSQLHistory.Parent := ATab;
    frmSQLHistory.Align := alClient;
    frmSQLHistory.BorderStyle := bsNone;

    // merken, damit wiederverwendet wird
    NodeInfos^.ViewForm := frmSQLHistory;
  end;

  // Tab vorbereiten
  ATab := frmSQLHistory.Parent as TTabSheet;
  fmMain.PageControl1.ActivePage := ATab;

  // Titel
  ShortTitle := 'SQL History:' + TTreeNode(GetAncestorAtLevel(SelNode, 1)).Text;
  ATab.Caption := ShortTitle;
  frmSQLHistory.Caption := ShortTitle;

  // Hint
  FullHint :=
    'SQL History' + sLineBreak +
    'Database: ' + FRegRec.Title;
  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Initialisieren
  frmSQLHistory.Init(FRegRec.Title, Self, NodeInfos);
  frmSQLHistory.Show;
end;}


{ Display popup menu }
procedure TfmQueryWindow.tbMenuClick(Sender: TObject);
begin
  pmTab.PopUp;
end;

procedure TfmQueryWindow.tbMenuMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;


{ display New SQL Window tab }

procedure TfmQueryWindow.tbNewClick(Sender: TObject);
var
  i: Integer;
begin
  // Get a free number to be assigned to the new Query window
  for i:= 1 to 1000 do
  begin
    if fmMain.FindQueryWindow('SQL # ' + IntToStr(i)) = nil then
    begin
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'SQL # ' + IntToStr(i), '', nil);
      Break;
    end;
  end;
end;

procedure TfmQueryWindow.tbNewMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.tbNewMouseLeave(Sender: TObject);
begin

end;

{ Read SQL query from text file }

procedure TfmQueryWindow.tbOpenClick(Sender: TObject);
var BasePath, SqlPath: string;
begin
  BasePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  SqlPath  := BasePath + 'data' + PathDelim + 'sql_scripts' + PathDelim;
  OpenDialog1.InitialDir := SqlPath;

  OpenDialog1.DefaultExt:= '.sql';
  if OpenDialog1.Execute then
    meQuery.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfmQueryWindow.tbOpenMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.tbQueryDesignMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;


{ RollBack current transaction }

procedure TfmQueryWindow.tbRollbackClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TIBQuery;
  SqlScript: TIBXScript;
  ATab: TTabSheet;
  QT: TQueryThread;
begin
  ATab:= CreateResultTab(qtExecute, SqlQuery, SqlScript, meResult);
  QT:= TQueryThread.Create(qaRollBack);
  try
    QT.Trans:= FSQLTrans;
    ATab.ImageIndex:= 6;
    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated;

    if QT.Error then
    begin
      ATab.ImageIndex:= 3;
      meResult.Lines.Text:= QT.ErrorMsg;
      meResult.Font.Color:= clRed;
    end
    else
    begin
      ATab.ImageIndex:= 4;
      meResult.Lines.Add('Rollback');
      meResult.Font.Color:= clGreen;
      if OnCommit <> nil then
        OnCommit(self);
      OnCommit:= nil;
      meResult.Font.Color:= $AA6666;
    end;

  finally
    QT.Free;
  end;
end;

procedure TfmQueryWindow.tbRollbackMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;


{ Rollback retaning for current transaction }

procedure TfmQueryWindow.tbRollbackRetainingClick(Sender: TObject);
var
  QT: TQueryThread;
begin
  QT:= TQueryThread.Create(qaRollbackRet);
  try
    QT.Trans:= FSQLTrans;

    QT.Resume;
    repeat
      application.ProcessMessages;
    until QT.fTerminated or (FCanceled);
    if QT.Error then
      ShowMessage('Error trying rollback retaining: '+QT.ErrorMsg);
  finally
    QT.Free;
  end;
end;

procedure TfmQueryWindow.tbRollbackRetainingMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;


{ Run current SQL, auto-detect type }
procedure TfmQueryWindow.tbRunClick(Sender: TObject);
begin
  CallExecuteQuery(qtUnknown);
end;

procedure TfmQueryWindow.tbRunMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;


{ Save current SQL in a text file }

procedure TfmQueryWindow.tbSaveClick(Sender: TObject);
var BasePath, SqlPath: string;
begin
  BasePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  SqlPath  := BasePath + 'data' + PathDelim + 'sql_scripts' + PathDelim;
  SaveDialog1.InitialDir := SqlPath;

  SaveDialog1.DefaultExt:= '.sql';
  if SaveDialog1.Execute then
    meQuery.Lines.SaveToFile(SaveDialog1.FileName);
end;

//ZEOS Query Designer
{procedure TfmQueryWindow.tbQueryDesignClick(Sender: TObject);
var
  meuqb: TOQBuilderDialog;
  VisualQueryEngine: TOQBEngineZEOS;
  FDbConnection: TZConnection;
  dbIndex: integer;
begin
  dbIndex := TPNodeInfos(fmMain.tvMain.Selected.Data)^.dbIndex;

  FDbConnection := TZConnection.Create(nil);
  FDbConnection.Protocol := 'firebird';
  //FDbConnection.ControlsCodePage := cCP_UTF8;
  FDbConnection.Database := RegisteredDatabases[dbIndex].RegRec.DatabaseName;
  FDbConnection.User := RegisteredDatabases[dbIndex].RegRec.UserName;
  FDbConnection.Password := RegisteredDatabases[dbIndex].RegRec.Password;

  meuqb := TOQBuilderDialog.Create(nil);
  VisualQueryEngine := TOQBEngineZEOS.Create(nil);
  VisualQueryEngine.Connection := FDbConnection;
  meuqb.OQBEngine := VisualQueryEngine;
  VisualQueryEngine.ShowSystemTables := False;
  meuqb.OQBEngine.DatabaseName := FDbConnection.Database;
  FDbConnection.Connect;

  //{$R-}
  if meuqb.Execute  then
  begin
    self.meQuery.Lines.Text := meuqb.SQL.Text;
  end;
  //{$R+}

  meuqb.Free;
  VisualQueryEngine.Free;
  FDbConnection.Free;
end;
}
//IBX Query Designer problematic
procedure TfmQueryWindow.tbQueryDesignClick(Sender: TObject);
var
  meuqb: TOQBuilderDialog;
  VisualQueryEngine: TOQBEngineIBX;
  FDbConnection: TIBDatabase;
  FTransaction: TIBTransaction;
  dbIndex: integer;
begin
  dbIndex := TPNodeInfos(fmMain.tvMain.Selected.Data)^.dbIndex;

  FDbConnection := TIBDatabase.Create(nil);
  FTransaction  := TIBTransaction.Create(nil);
  FDbConnection.DefaultTransaction := FTransaction;


  FDbConnection.Params.clear;
  FDbConnection.DatabaseName := RegisteredDatabases[dbIndex].RegRec.DatabaseName;
  FDbConnection.Params.Add('user_name=' + RegisteredDatabases[dbIndex].RegRec.UserName);
  FDbConnection.Params.Add('password=' + RegisteredDatabases[dbIndex].RegRec.Password);
  FDbConnection.LoginPrompt := false;

  meuqb := TOQBuilderDialog.Create(nil);
  VisualQueryEngine := TOQBEngineIBX.Create(nil);
  VisualQueryEngine.Connection := FDbConnection;
  meuqb.OQBEngine := VisualQueryEngine;
  VisualQueryEngine.ShowSystemTables := False;
  meuqb.OQBEngine.DatabaseName := FDbConnection.DatabaseName;

  try
    if not FDbConnection.Connected then
      FDbConnection.Connected := true;
  except
    raise;
  end;
  if not FTransaction.InTransaction then
    FTransaction.StartTransaction;

  if meuqb.Execute  then
  begin
    self.meQuery.Lines.Text := meuqb.SQL.Text;
  end;

  meuqb.Free;
  VisualQueryEngine.Free;
  if FTransaction.InTransaction then
    FTransaction.Rollback;
  FTransaction.Free;
  if FDbConnection.Connected then
    FDbConnection.Connected := false;
  FDbConnection.Free;
end;

procedure TfmQueryWindow.tbSaveMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.ToolBar1MouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.ToolBar1MouseLeave(Sender: TObject);
begin
  Application.OnShowHint := @fmMain.AppShowHint;
end;

procedure TfmQueryWindow.ToolButton4Click(Sender: TObject);
begin
  fmMain.lmScriptEngineClick(self);
end;

//SQLDb Query Designer
{procedure TfmQueryWindow.tbQueryDesignClick(Sender: TObject);
var
  meuqb: TOQBuilderDialog;
  VisualQueryEngine: TOQBEngineSQLDB;
  FDbConnection: TSQLConnection;
  FDBTrans: TSQLTransaction;
  dbIndex: integer;
begin
  dbIndex := TPNodeInfos(fmMain.tvMain.Selected.Data)^.dbIndex;

  FDbConnection := TSQLConnection.Create(nil);
  FDbConnection.DatabaseName := RegisteredDatabases[dbIndex].RegRec.DatabaseName;
  FDbConnection.Params.Clear;
  FDbConnection.Params.Add('user_name=' + RegisteredDatabases[dbIndex].RegRec.UserName);
  FDbConnection.Params.Add('password=' + RegisteredDatabases[dbIndex].RegRec.Password);
  FDbConnection.LoginPrompt := false;
  FDbConnection.HostName := RegisteredDatabases[dbIndex].IBConnection.HostName;

  FDBTrans := TSQLTransaction.Create(nil);
  FDbConnection.Transaction := FDBTrans;

  VisualQueryEngine := TOQBEngineSQLDB.Create(nil);
  VisualQueryEngine.Connection := FDbConnection;
  meuqb := TOQBuilderDialog.Create(nil);
  meuqb.OQBEngine := VisualQueryEngine;
  meuqb.OQBEngine.DatabaseName := FDbConnection.DatabaseName;
  VisualQueryEngine.ShowSystemTables := False;

  FDbConnection.Open;

  //{$R-}
  if meuqb.Execute  then
  begin
    self.meQuery.Lines.Text := meuqb.SQL.Text;
  end;
  //{$R+}

  meuqb.Free;
  VisualQueryEngine.Free;
  FDbConnection.Free;
end;
}

{GetNewTabNum: get last tab number and increase result by one }

function TfmQueryWindow.GetNewTabNum: string;
var
  i: Integer;
  Cnt: Integer;
begin
  Cnt:= 0;
  for i:= 0 to pgOutputPageCtl.ControlCount - 1 do
  if pgOutputPageCtl.Pages[i].TabVisible then
   Inc(Cnt);
  Result:= IntToStr(Cnt);
end;


{ Initialize query window: fill connection parameters from selected registered database }

procedure TfmQueryWindow.Init(dbIndex: Integer; ANodeInfos: TPNodeInfos=nil);
var i: integer;
begin
  FNodeInfos := ANodeInfos;
  FDBIndex:= dbIndex;
  FRegRec:= RegisteredDatabases[dbIndex].RegRec;

  // Remove old tabs in case of opening the same QueryWindow
  if Assigned(OutputTabsList) then
    RemovePreviousResultTabs
  else
    OutputTabsList:= TStringList.Create;

  // Set instances of FIBConnection and SQLTransaction for the current Query Window

  if (FIBConnection <> RegisteredDatabases[dbIndex].IBDatabase) then
  begin
  FIBConnection := RegisteredDatabases[dbIndex].IBDatabase;
  FSQLTrans.DefaultDatabase := FIBConnection;
  FIBConnection.DefaultTransaction := FSQLTrans;
  //SetTransactionIsolation(FSQLTrans.Params);

  // Set connection parameters to FIBConnection
  with RegisteredDatabases[dbIndex] do
  begin
    if not FIBConnection.Connected then
    begin
      Self.FIBConnection.DatabaseName:= FRegRec.DatabaseName;
      with Self.FIBConnection.Params do
      begin
        Clear;
        Add('user_name='     + RegRec.UserName);
        Add('password='      + RegRec.Password);
        Add('lc_ctype='      + RegRec.Charset);
        Add('sql_role_name=' + RegRec.Role);

      end;
      Self.FIBConnection.LoginPrompt := (FRegRec.Password = '');
      FIBConnection.Connected := true;
    end;
  end;

  end;
  //if not FSQLTrans.DefaultDatabase.Connected then
    //FSQLTrans.DefaultDatabase.Connected := true;
  if not FIBConnection.Connected then
    FIBConnection.Connected := true;

  //if not FSQLTrans.InTransaction then
    //FSQLTrans.StartTransaction;
  // Get current database tables to be highlighted in SQL query editor
  SynSQLSyn1.TableNames.CommaText:= fmMain.GetTableNames(dbIndex);
  for i := 0 to SynSQLSyn1.TableNames.Count - 1 do
    if IsObjectNameCaseSensitive(SynSQLSyn1.TableNames[i]) then
      SynSQLSyn1.TableNames[i] := MakeObjectNameQuoted(SynSQLSyn1.TableNames[i]);
  SynCompletion1.ItemList.AddStrings(SynSQLSyn1.TableNames);
  //SortSynCompletion;
end;

(************* Is Selectable (Check statement type Select, Update, Alter, etc) *******************)
function TfmQueryWindow.GetQueryType(AQuery: string): TQueryTypes;
var
  Clean: string;
  token: string;
  lower: string;

  function StripCommentsAndNormalize(const ASQL: string): string;
  var
    i, L: Integer;
    InSingleQuote, InDoubleQuote, InBlockComment: Boolean;
    B: TStringBuilder;
    ch, nextch: Char;
  begin
    B := TStringBuilder.Create;
    try
      InSingleQuote := False;
      InDoubleQuote := False;
      InBlockComment := False;
      L := Length(ASQL);
      i := 1;
      while i <= L do
      begin
        ch := ASQL[i];
        nextch := #0;
        if i < L then
          nextch := ASQL[i+1];

        // handle block comments /* ... */
        if not InSingleQuote and not InDoubleQuote then
        begin
          if InBlockComment then
          begin
            if (ch = '*') and (nextch = '/') then
            begin
              InBlockComment := False;
              Inc(i, 2);
              Continue;
            end
            else
            begin
              Inc(i);
              Continue;
            end;
          end
          else if (ch = '/') and (nextch = '*') then
          begin
            InBlockComment := True;
            Inc(i, 2);
            Continue;
          end;
        end;

        // handle single-line comment -- to end of line (only outside quotes and block comments)
        if not InSingleQuote and not InDoubleQuote and not InBlockComment then
        begin
          if (ch = '-') and (nextch = '-') then
          begin
            // skip until newline or end
            Inc(i, 2);
            while (i <= L) and not (ASQL[i] in [#10, #13]) do
              Inc(i);
            Continue;
          end;
        end;

        // handle quotes (strings) - keep them as-is
        if not InBlockComment then
        begin
          if (ch = '''') and (not InDoubleQuote) then
          begin
            // toggle single quote, handle doubled quote as escape
            if InSingleQuote and (i < L) and (ASQL[i+1] = '''') then
            begin
              // doubled quote inside single-quoted string -> copy both and advance
              B.Append(ch);
              B.Append(ASQL[i+1]);
              Inc(i, 2);
              Continue;
            end
            else
            begin
              InSingleQuote := not InSingleQuote;
              B.Append(ch);
              Inc(i);
              Continue;
            end;
          end
          else if (ch = '"') and (not InSingleQuote) then
          begin
            // double-quoted identifier or string - toggle
            if InDoubleQuote and (i < L) and (ASQL[i+1] = '"') then
            begin
              // doubled double-quote inside double-quote -> escape
              B.Append(ch);
              B.Append(ASQL[i+1]);
              Inc(i, 2);
              Continue;
            end
            else
            begin
              InDoubleQuote := not InDoubleQuote;
              B.Append(ch);
              Inc(i);
              Continue;
            end;
          end;
        end;

        // normal character (unless we are inside block comment)
        if not InBlockComment then
        begin
          // normalize whitespace: convert control chars to single space
          if (ch = #9) or (ch = #10) or (ch = #13) or (ch = ' ') then
          begin
            // only append a single space if last char is not space
            if (B.Length = 0) or (B.Chars[B.Length - 1] <> ' ') then
              B.Append(' ');
          end
          else
            B.Append(ch);
        end;

        Inc(i);
      end;

      // final trim and lowercase for simpler checks (we keep original case for potential further use)
      Result := Trim(B.ToString);
    finally
      B.Free;
    end;
  end;

  function FirstToken(const S: string): string;
  var
    i: Integer;
  begin
    Result := '';
    i := 1;
    while (i <= Length(S)) and (CharInSet(S[i], [#9, #10, #13, ' ' , '(', ')'])) do
      Inc(i);
    while (i <= Length(S)) and not CharInSet(S[i], [#9, #10, #13, ' ', '(', ')']) do
    begin
      Inc(i);
    end;
    begin
      Result := Result + S[i];
      Inc(i);
    end;
    Result := LowerCase(Result);
  end;

  // Count semicolons at top level (not inside single quotes, not inside parentheses)
  function HasMultipleTopLevelStatements(const S: string): Boolean;
  var
    i, L, parenDepth: Integer;
    ch: Char;
    InSingleQuote, InDoubleQuote: Boolean;
    countSemi: Integer;
  begin
    InSingleQuote := False;
    InDoubleQuote := False;
    parenDepth := 0;
    countSemi := 0;
    L := Length(S);
    i := 1;
    while i <= L do
    begin
      ch := S[i];
      // handle quotes ('' and "")
      if (ch = '''') and not InDoubleQuote then
      begin
        // if doubled quote inside single, skip escaped
        if InSingleQuote and (i < L) and (S[i+1] = '''') then
        begin
          Inc(i, 2);
          Continue;
        end;
        InSingleQuote := not InSingleQuote;
        Inc(i);
        Continue;
      end;
      if (ch = '"') and not InSingleQuote then
      begin
        if InDoubleQuote and (i < L) and (S[i+1] = '"') then
        begin
          Inc(i, 2);
          Continue;
        end;
        InDoubleQuote := not InDoubleQuote;
        Inc(i);
        Continue;
      end;

      if InSingleQuote or InDoubleQuote then
      begin
        Inc(i);
        Continue;
      end;

      // adjust paren depth
      if ch = '(' then
        Inc(parenDepth)
      else if ch = ')' then
        Dec(parenDepth);

      // semicolon at top level (parenDepth = 0)
      if (ch = ';') and (parenDepth = 0) then
        Inc(countSemi);

      Inc(i);
    end;

    // If more than 1 semicolon or exactly 1 semicolon but extra content after it -> multiple statements
    Result := (countSemi > 1) or (countSemi = 1) and (Pos(';', S) < Length(S));
  end;


begin
  // Step 1: strip comments and normalize whitespace
  Clean := StripCommentsAndNormalize(AQuery);
  Clean := Trim(Clean);

  if Clean = '' then
  begin
    Result := qtExecute; // empty -> nothing to run
    Exit;
  end;

  // Step 2: Detect script-like statements (SET, EXECUTE BLOCK, DDL, transaction control)
  token := FirstToken(Clean);
  lower := LowerCase(Trim(Clean));

  // IBXScript and Firebird ISQL script commands
  if
      // SET commands (all)
      (lower.StartsWith('set ')) or

      // Execute Block
      (lower.StartsWith('execute block')) or

      // Statement type tokens matching DDL and script control
      (token = 'create') or
      (token = 'alter')  or
      (token = 'drop')   or
      (token = 'grant')  or
      (token = 'revoke') or

      // Transaction control commands
      (token = 'commit') or
      (token = 'rollback') or

      // Connection control commands
      (token = 'connect') or
      (token = 'disconnect') or
      (token = 'create') and (Pos('database', lower) > 0) or
      (token = 'drop')   and (Pos('database', lower) > 0)
  then
  begin
    Result := qtScript;
    Exit;
  end;

  // If multiple top-level statements -> treat as script
  if HasMultipleTopLevelStatements(Clean) then
  begin
    Result := qtScript;
    Exit;
  end;

  // Accept SELECT or WITH (CTE) as selectable
  token := FirstToken(Clean);
  if (token = 'select') or (token = 'with') then
  begin
    Result := qtSelectable;
    Exit;
  end;

  // Also accept "(" followed by select - first token function already skips leading '('
  // Default: executable (INSERT, UPDATE, DELETE, DDL, etc.)
  Result := qtExecute;
end;



{ GetQuery: get query text from editor }

function TfmQueryWindow.GetQuery(QueryContents: tstrings): boolean;
var
  Seltext: string;
begin
  Result:= false;
  if assigned(QueryContents) then
  begin
    SelText:= trim(meQuery.SelText);
    if SelTExt<>'' then
      QueryContents.Text:= SelText
    else
      QueryContents.Text:= trim(meQuery.Lines.Text);
    Result:= true;
  end;
end;


procedure TfmQueryWindow.SaveGridSort(Grid: TRxDBGrid);
var
  i, j: Integer;
  Cols: array of TRxColumn;
  Tmp: TRxColumn;
begin
  SetLength(SavedSort, 0);

  // S️ortierte Spalten sammeln
  SetLength(Cols, 0);
  for i := 0 to Grid.Columns.Count - 1 do
    if Grid.Columns[i].SortOrder <> smNone then
    begin
      SetLength(Cols, Length(Cols) + 1);
      Cols[High(Cols)] := Grid.Columns[i];
    end;

  // Nach SortPosition sortieren
  for i := 0 to High(Cols) - 1 do
    for j := i + 1 to High(Cols) do
      if Cols[i].SortPosition > Cols[j].SortPosition then
      begin
        Tmp := Cols[i];
        Cols[i] := Cols[j];
        Cols[j] := Tmp;
      end;

  // i️n dieser Reihenfolge speichern
  SetLength(SavedSort, Length(Cols));
  for i := 0 to High(Cols) do
  begin
    SavedSort[i].FieldName := Cols[i].FieldName;
    SavedSort[i].SortOrder := Cols[i].SortOrder;
  end;
end;

function TfmQueryWindow.BuildOrderBy(Grid: TRxDBGrid; QuoteNames: Boolean = True): string;
var
  SortCols: TStringList;
  i, j: Integer;
  Col, Tmp: TRxColumn;
  FieldName: string;
  ResultStr: string;
begin
  SortCols := TStringList.Create;
  try
    // ️sortierte Spalten sammeln
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      Col := TRxColumn(Grid.Columns[i]);
      if Col.SortOrder <> smNone then
        SortCols.AddObject('', Col);
    end;

    // nach SortPosition sortieren (wie RxDBGrid intern)
    for i := 0 to SortCols.Count - 2 do
      for j := i + 1 to SortCols.Count - 1 do
        if TRxColumn(SortCols.Objects[i]).SortPosition >
           TRxColumn(SortCols.Objects[j]).SortPosition then
        begin
          Tmp := TRxColumn(SortCols.Objects[i]);
          SortCols.Objects[i] := SortCols.Objects[j];
          SortCols.Objects[j] := Tmp;
        end;

    // ORDER BY aufbauen
    ResultStr := '';
    for i := 0 to SortCols.Count - 1 do
    begin
      Col := TRxColumn(SortCols.Objects[i]);

      FieldName := Col.FieldName;
      if QuoteNames then
        FieldName := '"' + FieldName + '"';

      if ResultStr <> '' then
        ResultStr := ResultStr + ', ';

      if Col.SortOrder = smUp then
        ResultStr := ResultStr + FieldName + ' ASC'
      else
        ResultStr := ResultStr + FieldName + ' DESC';
    end;

    Result := ResultStr;
  finally
    SortCols.Free;
    Grid.Repaint;
  end;
end;

procedure TfmQueryWindow.ApplyGridSort(Grid: TRxDBGrid);
var
  Fields: array of string;
  Markers: array of TSortMarker;
  i: Integer;
  IBQuery: TIBQuery;
  OrderBy: string;
begin
  // ORDER BY bauen
  OrderBy := BuildOrderBy(Grid, False);
  SaveGridSort(Grid);

  IBQuery := GetCurrentSelectQuery;

  IBQuery.Close;
  IBQuery.Parser.OrderByClause := OrderBy;
  IBQuery.SQL.Text := IBQuery.Parser.SQLText;
  IBQuery.Open;

  // Arrays vorbereiten
  SetLength(Fields, Length(SavedSort));
  SetLength(Markers, Length(SavedSort));

  for i := 0 to High(SavedSort) do
  begin
    Fields[i] := SavedSort[i].FieldName;
    Markers[i] := SavedSort[i].SortOrder;
  end;

  Grid.SetSort(Fields, Markers, False);
end;


procedure TfmQueryWindow.RxDBGridSortControllerTitleClick(Column: TColumn);
begin
  ApplyGridSort(TRxDBGrid(pmGrid.PopupComponent));
end;

{ Create new result tab depending on query type }

function TfmQueryWindow.CreateResultTab(QueryType: TQueryTypes;
  var aSqlQuery: TIBQuery; var aSQLScript: TIBXScript; var meResult: TMemo;
  AdditionalTitle: string): TTabSheet;
var
  ATab: TTabSheet;
  DBGrid: TRxDBGrid;
  DataSource: TDataSource;
  StatusBar: TStatusBar;
  Nav: TDBNavigator;
  Pan: TPanel;
begin
  ATab:= TTabSheet.Create(nil);
  OutputTabsList.AddObject('', ATab);
  BeginUpdateBounds;
  Result:= ATab;
  ATab.Parent:= pgOutputPageCtl;
  pgOutputPageCtl.ActivePage:= ATab; //set focus to new tab
  ATab.Caption:= 'Result # ' + GetNewTabNum + ' ' + AdditionalTitle;
  if QueryType = qtSelectable then // Select, need record set result
  begin
    // Query
    // Clean up any existing object to avoid memory leak
    //if assigned(aSQLQuery) then
      //aSQLQuery.Free;
    //aSqlQuery:= TIBQuery.Create(self);
    //aSqlQuery.DataBase:= FIBConnection;
    //aSqlQuery.Transaction:= FSQLTrans;
    aSqlQuery.AfterPost:= @QueryAfterPost; //detect user-edited grid
    aSqlQuery.AfterScroll:= @QueryAfterScroll;
    aSqlQuery.Tag:= ATab.TabIndex; //Query points to tabsheet number
    {Tab points to query object so we can look it up more easily via the
    tab sheet if we need to enable Apply/Commit buttons etc}
    ATab.Tag:= PtrInt(aSQLQuery);

    // Status Bar
    StatusBar:= TStatusBar.Create(ATab);
    StatusBar.Parent:= ATab;
    StatusBar.Tag:= aSqlQuery.Tag;

    // Datasource
    DataSource:= TDataSource.Create(self);
    DataSource.DataSet:= aSqlQuery;

    // Panel
    pan:= TPanel.Create(self);
    pan.Parent:= ATab;
    Pan.Height:= 30;
    Pan.Align:= alTop;

    // Query result Grid
    //DBGrid:= TIBDynamicGrid.Create(self);
    DBGrid:= TRxDBGrid.Create(self);

    //TmpDBGrid.Visible := false;
    DBGrid.Parent:= ATab;
    DBGrid.DataSource:= DataSource;
    DBGrid.Align:= alClient;
    DBGrid.OnDblClick:= @DBGrid1DblClick;
    DBGrid.OptionsRx := [rdgAllowColumnsForm,rdgAllowDialogFind,rdgHighlightFocusCol,rdgHighlightFocusRow,rdgFooterRows,rdgAllowQuickFilter,rdgAllowFilterForm,rdgAllowSortForm,rdgAllowToolMenu,rdgCaseInsensitiveSort,rdgDisableWordWrapTitles,rdgColSpanning];

    DBGrid.TitleButtons := true;
    DBGrid.AutoSort := true;
    DBGrid.DoubleBuffered := true;
    DBGrid.OnTitleClick := @RxDBGridSortControllerTitleClick;
    pmGrid.PopupComponent := DBGrid;

    DBGrid.Tag:= ATab.TabIndex;
    DBGrid.ReadOnly:= False;
    DBGrid.AutoEdit:= false;


    DBGrid.PopupMenu:= pmGrid;
    DBGrid.TitleStyle:= tsNative;

    // Navigator
    Nav:= TDBNavigator.Create(self);
    Nav.Parent:= Pan;
    Nav.VisibleButtons:= [nbFirst, nbNext, nbPrior, nbLast];
    Nav.DataSource:= DataSource;

    Nav.Visible := QWShowNavigator;

    // Apply button
    NewApplyButton(Pan, ATab);

    // Commit button
    NewCommitButton(Pan, ATab);

    RxDBGridExportPDF1.RxDBGrid := DBGrid;
    RxDBGridPrint1.RxDBGrid := DBGrid;
    RxDBGridExportSpreadSheet1.RxDBGrid := DBGrid;
  end
  else
  if QueryType in [qtExecute, qtScript] then
  begin
    meResult:= TMemo.Create(self);
    meResult.Parent:= ATab;
    meResult.ReadOnly:= True;
    meResult.Align:= alClient;
    case QueryType of
      qtExecute:
      begin
        aSqlQuery:= TIBQuery.Create(self);
        aSqlQuery.DataBase:= FIBConnection;
        aSqlQuery.Transaction:= FSQLTrans;
        aSqlQuery.AllowAutoActivateTransaction := true;
      end;
      qtScript: // Script
      begin
        // Clean up to avoid memory leak
        if assigned(aSQLScript) then
          aSQLScript.Free;
        aSQLScript:= TIBXScript.Create(self);
        aSQLScript.DataBase:= FIBConnection;
        aSQLScript.Transaction:= FSQLTrans;
        //aSQLScript.CommentsInSQL:= true;
        //aSQLScript.UseSetTerm:= true; //needed if set term is used, e.g. for stored procedures
      end;
    end;
  end;
  if Assigned(ATab) then
    frmThemeSelector.btnApplyClick(ATab);
end;


(***************  Execute Query   ******************)
//{$IFDEF ___DEBUG}
procedure TfmQueryWindow.ExecuteQuery;  //without TQueryThread
var
  StartTime: TDateTime;
  SqlType: string;
  EndLine: Integer;
  Command: string;
  IsDDL: Boolean;
  Affected: Integer;
  fQueryType: TQueryTypes;
  FSQLQuery: TIBQuery;
  FSQLTrans_Local: TIBTransaction;
  dbIndex: word;
begin
  if not FIBConnection.Connected then
    FIBConnection.Connected := true;

  //if not  FSQLTrans.InTransaction then
    //FSQLTrans.StartTransaction;

  try
    if (FOrigQueryType = qtScript) then
    begin
      ExecuteScript(FQuery.Text);
      Inc(FModifyCount);
      SqlType := GetSQLType(FQuery.Text, Command);
      //fmMain.AddToSQLHistory(FRegRec.Title, SqlType, FQuery.Text);
      fmMain.AddToSQLHistory(FRegRec.Title, SqlType, meQuery.Text);
      FFinished := True;
      FQuery.Clear;
    end
    else
    begin
      Inc(FCounter);
      if not GetSQLSegment(FQuery, FStartLine, fQueryType, EndLine, FQueryPart, IsDDL) then
      begin
        FFinished := True;
        Exit;
      end;

      FStartLine := EndLine + 1;

      if Trim(FQueryPart) <> '' then
      begin
        if fQueryType = qtSelectable then
        begin
          FTab := nil;
          try
            FSQLQuery := TIBQuery.Create(Self);
            FSQLTrans_Local := TIBTransaction.Create(nil);
            FSQLTrans_Local.DefaultDatabase := FIBConnection;
            FSQLTrans_Local.StartTransaction;

            FSQLQuery.DataBase := FIBConnection;
            FSQLQuery.Transaction := FSQLTrans_Local;


            if cxAutoCommit.Checked then
              if FSQLTrans.InTransaction then
                FSQLTrans.CommitRetaining;
            FTab := CreateResultTab(qtSelectable, FSQLQuery, FSQLScript, FResultMemo);
            FTab.ImageIndex := 6;

            FTab.Hint := FQueryPart;
            FTab.ShowHint := True;
            FSQLQuery.SQL.Text := FQueryPart;

            // Open dataset synchronously
            if not FSQLTrans_Local.InTransaction then
              FSQLTrans_Local.StartTransaction;
            FSQLQuery.Transaction := FSQLTrans_Local;
            FTab.Caption := 'Running...';
            FSQLQuery.Open;
            FTab.Caption := 'Query Result';
            FTab.ImageIndex := 0;
            //fmMain.AddToSQLHistory(FRegRec.Title, 'SELECT', FQueryPart);
            fmMain.AddToSQLHistory(FRegRec.Title, 'SELECT', meQuery.Text);
          except
            on e: Exception do
            begin
              if Assigned(FTab) then
                FTab.TabVisible := False;
              FTab := CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
              pgOutputPageCtl.ActivePage := FTab;
              FResultMemo.Text := e.Message;
              FResultMemo.Lines.Add(FQueryPart);
              FResultMemo.Font.Color := clRed;
              FTab.Font.Color := clRed;
              FTab.ImageIndex := 3;
            end;
          end;
        end
        else if fQueryType = qtExecute then
        begin
          FTab := nil;
          FTab := CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
          FTab.ImageIndex := 1;
          SqlType := GetSQLType(FQueryPart, Command);
          StartTime := Now;
          Affected := 0;
          try
            if IsDDL then
            begin
              // Execute DDL synchronously
              FSQLQuery.SQL.Text := FQueryPart;
              FSQLQuery.ExecSQL;
              if cxAutoCommit.Checked then
                FSQLTrans.CommitRetaining;
              FTab.Caption := 'DDL Executed';
            end
            else
            begin
              // Execute DML synchronously
              FSQLQuery.SQL.Text := FQueryPart;
              FTab.Caption := 'Running...';
              FSQLQuery.ExecSQL;
              if cxAutoCommit.Checked then
                FSQLTrans.CommitRetaining;
              Affected := FSQLQuery.RowsAffected;
              FTab.Caption := 'DML Executed';
            end;
            Inc(FModifyCount);
            fmMain.AddToSQLHistory(FRegRec.Title, SqlType, meQuery.Text);
            FResultMemo.Visible := True;
            FResultMemo.Clear;
            FResultMemo.Lines.Add('Statement #' + IntToStr(FCounter));
            if IsDDL then
              FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) + ' - DDL Executed. Duration: ' +
                FormatDateTime('HH:nn:ss.z', Now - StartTime))
            else
            begin
              FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) + ' - DML Executed. Duration: ' +
                FormatDateTime('HH:nn:ss.z', Now - StartTime));
              FResultMemo.Lines.Add('Rows affected: ' + IntToStr(Affected));
            end;
            FResultMemo.Lines.Add('----');
            FResultMemo.Lines.Add(FQueryPart);
          except
            on E: Exception do
            begin
              if Assigned(FTab) then
                FTab.TabVisible := False;
              FTab := CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
              pgOutputPageCtl.ActivePage := FTab;
              FResultMemo.Text := e.Message;
              FResultMemo.Lines.Add(FQueryPart);
              FResultMemo.Font.Color := clRed;
              FTab.Font.Color := clRed;
              FTab.ImageIndex := 3;
            end;
          end;
        end
        else
        begin
          try
            if ExecuteScript(FQueryPart) then
            begin
              Inc(FModifyCount);
              SqlType := GetSQLType(FQueryPart, Command);
              fmMain.AddToSQLHistory(FRegRec.Title, SqlType, meQuery.Text);
            end;
          except
            on E: Exception do
            begin
              if Assigned(FTab) then
                FTab.TabVisible := False;
              FTab := CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
              pgOutputPageCtl.ActivePage := FTab;
              FResultMemo.Text := e.Message;
              FResultMemo.Lines.Add(FQueryPart);
              FResultMemo.Lines.Add('--------');
              FResultMemo.Font.Color := clRed;
              FTab.Font.Color := clRed;
              FTab.ImageIndex := 3;
            end;
          end;
        end;

        if (FModifyCount > 50) then
        begin
          if (MessageDlg('Commit', 'There are too many transactions, do you want to commit?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            FSQLTrans.CommitRetaining;
            FModifyCount := 0;
          end
          else
            FModifyCount := 0;
        end;
      end;

      if FStartLine >= FQuery.Count then
        FFinished := True;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FTab) then
        FTab.TabVisible := False;
      FTab := CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
      FTab.ImageIndex := 2;
      pgOutputPageCtl.ActivePage := FTab;

      FResultMemo.Text := E.Message;
      FResultMemo.Lines.Add('--------');
      FResultMemo.Lines.Add(FQueryPart);
      FResultMemo.Font.Color := clRed;
      FFinished := True;
    end;
  end;
end;

//{$ELSE}

{procedure TfmQueryWindow.ExecuteQuery;  //with TQueryThread
var
  StartTime: TDateTime;
  SqlType: string;
  EndLine: Integer;
  Command: string;
  IsDDL: Boolean;
  Affected: Integer;
  fQueryType: TQueryTypes;
  TempQuery: TIBQuery;
  SanitizedSQL: string;
  i: integer;
  FSQLQuery: TIBQuery;
begin
  if not FIBConnection.Connected then
    FIBConnection.Connected := true;

  if not  FSQLTrans.InTransaction then
    FSQLTrans.StartTransaction;

  FSQLQuery := TIBQuery.Create(self);
  FSQLQuery.DataBase:= FIBConnection;
  FSQLQuery.Transaction:= FSQLTrans;
  FSQLQuery.AllowAutoActivateTransaction := true;

  try
    // Script
    if (FOrigQueryType = qtScript) then
    begin // script
      ExecuteScript(FQuery.Text);
      Inc(FModifyCount);
      SqlType:= GetSQLType(FQuery.Text, Command);
      fmMain.AddToSQLHistory(FRegRec.Title, SqlType, FQuery.Text);
      FFinished:= True;
      FQuery.Clear;
    end
    else  // normal statement / Multi statements
    begin
      Inc(FCounter);
      if not GetSQLSegment(FQuery, FStartLine, fQueryType, EndLine, FQueryPart, IsDDL) then
      begin
        FFinished:= True;
        Exit;
      end;

      {if EndLine < FStartLine then
        FStartLine:= FStartLine + 1
      else}
        FStartLine:= EndLine + 1;

      if Trim(FQueryPart) <> '' then   // Select
      if fQueryType = qtSelectable then
      begin
        FTab:= nil;
        try
          if cxAutoCommit.Checked then
          begin
            if FSQLTrans.InTransaction then
              FSQLTrans.CommitRetaining;
            if not FSQLTrans.InTransaction then
              FSQLTrans.StartTransaction;
          end;

          FTab:= CreateResultTab(qtSelectable, FSQLQuery, FSQLScript, FResultMemo);
          FTab.ImageIndex:= 6;
          FTab.Hint:= FQueryPart;
          FTab.ShowHint:= True;
          FSQLQuery.SQL.Text:= Trim(FQueryPart);

          if (pos('select first ',lowercase(FQueryPart))=1) then
          begin
            // Get rid of the select first x part by copying everything after
            // the third word
            SanitizedSQL:= ExtractWordPos(3, FQueryPart, StdWordDelims, i);
            if i > 0 then
              SanitizedSQL:= 'select ' + trim(copy(FQueryPart, i+length(SanitizedSQL), maxint));
          end;

          // Create thread to open dataset
          FQT:= TQueryThread.Create(qaOpen);
          FQT.Query:= FSQLQuery;
          FQT.Trans:= FSQLTrans;
          FQT.OnTerminate:= @ThreadTerminated;
          FAText:= FTab.Caption;
          FTab.Caption:= 'Running..';
          FQT.Resume;

          // Wait for the thread to complete
          repeat
            Sleep(100);
            application.ProcessMessages; // This prevents display freeze
          until FQT.fTerminated;

          // Raise exception if an error occured during thread execution (Open)
          if FQT.Error then
            raise Exception.Create(FQT.ErrorMsg);

          //additional code: refresh
          TIBQuery(FTab.Tag).DisableControls;
          FQT.FSQLQuery.Last;
          FQT.FSQLQuery.First;
          TIBQuery(FTab.Tag).EnableControls;

          FQT.Free;
          FTab.Caption:= FAText;
          FTab.ImageIndex:= 0;
          fmMain.AddToSQLHistory(FRegRec.Title, 'SELECT', FQueryPart);
        except
          on e: Exception do
          begin
            if Assigned(FTab) then
              FTab.TabVisible:= False;
            FTab:= CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
            pgOutputPageCtl.ActivePage:= FTab;

            FResultMemo.Text:= e.message;
            FResultMemo.Lines.Add(FQueryPart);
            FResultMemo.Font.Color:= clRed;
            FTab.Font.Color:= clRed;
            FTab.ImageIndex:= 3;
          end;
        end;
      end
      else  // Execute
        if fQueryType = qtExecute then
        begin
          FTab:= nil;
          FTab:= CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);

          FTab.ImageIndex:= 1;
          SqlType:= GetSQLType(FQueryPart, Command);
          StartTime:= Now;
          Affected:= 0;
          try
            if IsDDL then
            begin
              // Execute the statement in thread
              FQT:= TQueryThread.Create(qaDDL);
              FQT.Query:= FSQLQuery;
              FQT.Connection:= FIBConnection;
              FQT.Trans:= FSQLTrans;
              FQT.Statement:= FQueryPart;
              FQT.Resume;
              FAText:= FTab.Caption;
              FTab.Caption:= 'Running..';

              // Wait for thread completion
              repeat
                //application.ProcessMessages;
              until (FQT.fTerminated) or (FCanceled);

              // Raise exception if an error occured during thread execution (ExecProc)
              if FQT.Error then
                raise Exception.Create(FQT.ErrorMsg);

              FTab.Caption:= FAText;

              // Auto commit
              if cxAutoCommit.Checked then
                FSQLTrans.CommitRetaining;
              FQT.Free;
            end
            else
            begin // DML
              FSQLQuery.Close;
              FSQLQuery.SQL.Text:= FQueryPart;
              FTab.ImageIndex:= 6;
              FTab.Hint:= FQueryPart;
              FTab.ShowHint:= True;
              FSQLQuery.SQL.Text:= FQueryPart;

              // Execute the statement in thread
              FQT:= TQueryThread.Create(qaExec);
              try
                FQT.Query:= FSQLQuery;
                FQT.Trans:= FSQLTrans;
                FQT.Resume;
                FAText:= FTab.Caption;
                FTab.Caption:= 'Running..';

                // Wait for thread completion
                repeat
                  application.ProcessMessages;
                until (FQT.fTerminated) or (FCanceled);

                // Raise exception if an error occured during thread execution (ExecProc)
                if FQT.Error then
                  raise Exception.Create(FQT.ErrorMsg);

                // Auto commit
                if cxAutoCommit.Checked then
                  FSQLTrans.CommitRetaining;
              finally
                FQT.Free;
              end;
              FTab.Caption:= FAText;
              FTab.ImageIndex:= 1;
              Affected:= FSQLQuery.RowsAffected;
            end;
            Inc(FModifyCount);

            fmMain.AddToSQLHistory(FRegRec.Title, SQLType, FQueryPart);
            FResultMemo.Visible:= True;
            FResultMemo.Clear;
            FResultMemo.Lines.Add('statement #' + IntToStr(FCounter));
            if IsDDL then
              FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) + ' - DDL Executed. Takes (H:M:S.MS) ' +
                FormatDateTime('HH:nn:ss.z', Now - StartTime))
            else // DML
            begin
              FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) + ' - DML Executed. Takes (H:M:S.MS) ' +
                FormatDateTime('HH:nn:ss.z', Now - StartTime));
              FResultMemo.Lines.Add('Rows affected: ' + Format('%3.0n', [Affected / 1]));
            end;
            FResultMemo.Lines.Add('----');
            FResultMemo.Lines.Add(FQueryPart);
          except
            on E: Exception do
            begin
              if Assigned(FTab) then
                FTab.TabVisible:= False;
              FTab:= CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
              pgOutputPageCtl.ActivePage:= FTab;
              FResultMemo.Text:= e.message;
              FResultMemo.Lines.Add(FQueryPart);
              FResultMemo.Font.Color:= clRed;
              FTab.Font.Color:= clRed;
              FTab.ImageIndex:= 3;
            end;
          end;
        end
        else  // Script
        begin
          try
            if ExecuteScript(FQueryPart) then
            begin
              Inc(FModifyCount);
              SqlType:= GetSQLType(FQueryPart, Command);
              fmMain.AddToSQLHistory(FRegRec.Title, SqlType, FQueryPart);
            end;
          except
            on E: Exception do
            begin
              if Assigned(FTab) then
                FTab.TabVisible:= False;
              FTab:= CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
              pgOutputPageCtl.ActivePage:= FTab;
              FResultMemo.Text:= e.message;
              FResultMemo.Lines.Add(FQueryPart);
              FResultMemo.Lines.Add('--------');
              FResultMemo.Font.Color:= clRed;
              FTab.Font.Color:= clRed;
              FTab.ImageIndex:= 3;
            end;
          end;
        end;
        if (FModifyCount > 50) then
        begin
          if (MessageDlg('Commit', 'There are too many transactions, do you want to commit',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            //FSQLTrans.CommitRetaining;
            FSQLTrans.Commit;
            FModifyCount:= 0;
          end
          else
          begin
            FModifyCount:= 0;
          end;
        end;
      if FStartLine >= FQuery.Count then
        FFinished:= True;
    end;

  except
    on E: Exception do
    begin
      if Assigned(FTab) then
        FTab.TabVisible:= False;
      FTab:= CreateResultTab(qtExecute, FSQLQuery, FSQLScript, FResultMemo);
      FTab.ImageIndex:= 2;
      pgOutputPageCtl.ActivePage:= FTab;

      FResultMemo.Text:= e.message;
      FResultMemo.Lines.Add('--------');
      FResultMemo.Lines.Add(FQueryPart);
      FResultMemo.Font.Color:= clRed;
      FFinished:= True;
      FSQLTrans.Rollback;
    end;
  end;
end;}
//{$ENDIF}

procedure TfmQueryWindow.OnFIBXScriptSelectSQL(Sender: TObject; SQLText: string);
var
  SelNode: TTreeNode;
  QWindow: TfmQueryWindow;
begin
  SelNode := turbocommon.MainTreeView.Selected;
  if (SelNode <> nil) and (SelNode.Parent <> nil) then
  begin
    QWindow := fmMain.ShowCompleteQueryWindow(TPNodeInfos(SelNode.Data)^.dbIndex, SQLText, SQLText, nil);
    QWindow.meQuery.Lines.Text:= SQLText;
    QWindow.bbRunClick(nil);
    QWindow.Show;
  end;
end;

{ Execute script }
function TfmQueryWindow.ExecuteScript(Script: string): Boolean;
var
  StartTime: TDateTime;
  SqlQuery: TIBQuery;
  FIBXScript: TIBXScript;
  meResult: TMemo;
  ATab: TTabSheet;
begin
  StartTime:= Now;
  ATab:= nil;
  //SQLScript:= nil;

  try
    // CreateResultTab creates the SQLScript object for us.
    ATab:= CreateResultTab(qtScript, SqlQuery, FIBXScript, meResult);
    try
      ATab.ImageIndex:= 2;
      //SQLScript.Script.Text:= Script;
      {$IFDEF DEBUG}
      SendDebug('going to run script: ' + Script);
      {$Endif}
      Script := Trim(Script);
      FIBXScript.OnSelectSQL := @OnFIBXScriptSelectSQL;
      FIBXScript.ExecSQLScript(Script);
      // Auto commit
      if cxAutoCommit.Checked then
        FSQLTrans.CommitRetaining;

      Result:= True;
      meResult.Lines.Text:= FormatDateTime('hh:nn:ss.z', Now) + ' - Script Executed. It took (H:M:S.MS) ' +
        FormatDateTime('HH:nn:ss.z', Now - StartTime);
      meResult.Lines.Add('--------');
      meResult.Lines.Add(Script);
    finally
      FIBXScript.Free;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF DEBUG}
      SendDebug('ExecuteScript failed; error '+E.Message);
      {$Endif}
      Result:= False;
      if Assigned(ATab) then
        ATab.TabVisible:= False;
      ATab:= CreateResultTab(qtExecute, SqlQuery, FIBXScript, meResult);
      pgOutputPageCtl.ActivePage:= ATab;
      meResult.Text:= e.Message;
      meResult.Lines.Add('--------');
      meResult.Lines.Add(Script);
      meResult.Font.Color:= clRed;
      ATab.Font.Color:= clRed;
      ATab.ImageIndex:= 3;
    end;
  end;
end;

{ Display new Save/Apply button for current query result been edited }

procedure TfmQueryWindow.NewApplyButton(var Pan: TPanel; var ATab: TTabSheet);
var
  Apply: TBitBtn;
begin
  Apply:= TBitBtn.Create(self);
  Apply.Parent:= Pan;
  Apply.Caption:= 'Apply'; //don't change this; code looks for this exact caption
  Apply.Left:= 300;
  Apply.Visible:= False;
  Apply.OnClick:= @ApplyClick;
  Apply.Tag:= ATab.TabIndex;
end;


{ GetSQLType: get SQL type of current SQL text }

function TfmQueryWindow.GetSQLType(Query: string; var Command: string): string;
begin
  Result:= 'DML'; //default
  Query:= Trim(Query);
  if (Query <> '') and (Pos(' ', Query) > 0) then
  begin
    // to do: this does not take comments into account...
    Command:= Copy(Query, 1, Pos(' ', Query) - 1);
    Command:= LowerCase(Command);
    if (Command = 'alter') or
       (Command = 'create') or
       (Command = 'drop') or
       (Command = 'grant') {actually DCL} or
       (Command = 'revoke') {actually DCL} then
      Result:= 'DDL';
  end;
end;


{ GetSQLSeqment: read part of SQL end by ; }

function TfmQueryWindow.GetSQLSegment(QueryList: TStringList; StartLine: Integer;
  var QueryType: TQueryTypes; var EndLine: Integer;
  var SQLSegment: string; var IsDDL: Boolean): Boolean;
var
  i: Integer;
  RealStartLine: Integer;
  SecondRealStart: Integer;
  BeginExists: Boolean;
begin
  // Get start
  SQLSegment:= '';
  RealStartLine:= StartLine;
  SecondRealStart:= RealStartLine;
  Result:= False;

  // Remove comments
  RemoveAllSingleLineComments(QueryList);
  RemoveComments(QueryList, StartLine, RealStartLine);

  SecondRealStart:= RealStartLine;

  // remove empty lines
  RemoveEmptyLines(QueryList, SecondRealStart, RealStartLine);

  // Get SQL type
  QueryType:= GetQuerySQLType(QueryList, SecondRealStart, IsDDL);

  // Concatenate
  SQLSegment:= '';
  BeginExists:= False;
  for i:= SecondRealStart to QueryList.Count - 1 do
  begin
    if Pos('begin', Trim(LowerCase(QueryList[i]))) > 0 then
      BeginExists:= True;

    SQLSegment:= SQLSegment + QueryList[i] + LineEnding;

    if (QueryType in [qtSelectable, qtExecute]) and
      (((Pos(';', QueryList[i]) > 0) and (Not BeginExists)) or
      ((Pos('end', LowerCase(Trim(QueryList[i]))) = 1) and BeginExists)
      or (i = QueryList.Count - 1)) then
    begin
      Result:= True;
      if (not BeginExists) and (Pos(';', QueryList[i]) > 0) then
      begin
        QueryList[i]:= Trim(Copy(QueryList[i],  Pos(';', QueryList[i]) + 1, Length(QueryList[i])));
        if QueryList[i] = '' then
        EndLine:= i
        else
        begin
          EndLine:= i - 1;
          SQLSegment:= Trim(Copy(SQLSegment, 1, Pos(';',  SQLSegment)));
        end;
      end
      else
        EndLine:= i;
      Break;
    end
    else
    if (QueryType = qtScript) and
      ((i > SecondRealStart) and (Pos('set term', LowerCase(Trim(QueryList[i]))) = 1)) or
      (i = QueryList.Count - 1) then
    begin
      Result:= True;
      EndLine:= i;
      Break;
    end;
  end;
end;

procedure TfmQueryWindow.QueryAfterPost(DataSet: TDataSet);
begin
  // User has edited cells, so let him save
  EnableApplyButton;
end;


{ Run query, 0 for auto-detect query type }

procedure TfmQueryWindow.bbRunClick(Sender: TObject);
begin
  CallExecuteQuery(qtUnknown);
end;

procedure TfmQueryWindow.cxAutoCommitMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.cxAutoCommitMouseLeave(Sender: TObject);
begin
  Application.OnShowHint := @fmMain.AppShowHint;
end;


{ Display Blob contents in a message box }

procedure TfmQueryWindow.DBGrid1DblClick(Sender: TObject);
begin
  {ShowMessage('Field contents: ' + LineEnding +
    (Sender as TDBGrid).SelectedField.AsString)}
end;


{ Find text }

procedure TfmQueryWindow.FindDialog1Find(Sender: TObject);
begin
  FOptions:= [];

  if frMatchCase in FindDialog1.Options then
    FOptions:= FOptions + [ssoMatchCase];

  if frWholeWord in FindDialog1.Options then
    FOptions:= FOptions + [ssoWholeWord];

  if not (frDown in FindDialog1.Options) then
    FOptions:= FOptions + [ssoBackwards];

   if frEntireScope in FindDialog1.Options then
     FOptions:= FOptions + [ssoEntireScope];

  meQuery.SearchReplace(FindDialog1.FindText, '', FOptions);
end;


{ QueryWindow onClose event, commit active transaction, remove controls }

{procedure TfmQueryWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    if Assigned(FNodeInfos^.ViewForm) then
      FNodeInfos^.ViewForm := nil;

  // Check if the transaction is active; then commit it
  if FSQLTrans.Active then
  begin
    FSQLTrans.CommitRetaining;
    if OnCommit <> nil then
      OnCommit(self);
    OnCommit:= nil;
  end;
  //FIBConnection.Close;   //IBX!!!!
  //   OutputTabsList.Free;  causes exception, still used in RemovePreviousResultTabs
  RemovePreviousResultTabs;
  OutputTabsList.Free;
  CloseAction:= caFree;
end;}

procedure TfmQueryWindow.RemovePreviousResultTabs;
var
  i: Integer;
begin
  for i:= OutputTabsList.Count - 1 downto 0 do
  begin
    OutputTabsList.Objects[i].Free;
    OutputTabsList.Delete(i);
  end;
end;

procedure TfmQueryWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  RxDBGridExportPDF1.RxDBGrid := nil;
  RxDBGridPrint1.RxDBGrid := nil;
  RxDBGridExportSpreadSheet1.RxDBGrid := nil;

  if Assigned(FNodeInfos) then
    if Assigned(FNodeInfos^.ViewForm) then
      FNodeInfos^.ViewForm := nil;

  // Alle TIBQuery-Komponenten freigeben
  for i := ComponentCount - 1 downto 0 do
  begin
    if Components[i] is TIBQuery then
    begin
      if TIBQuery(Components[i]).Active then
        TIBQuery(Components[i]).Close;
      Components[i].Free;
    end;
  end;

  // Aktive globale Transaktion sauber beenden
  {if FSQLTrans.InTransaction then
  begin
    FSQLTrans.Commit;
    if OnCommit <> nil then
      OnCommit(Self);
    OnCommit := nil;
  end;}

  //locale transactionen freigeben
  for i := ComponentCount - 1 downto 0 do
  begin
    if Components[i] is TIBTransaction then
    begin
      if TIBTransaction(Components[i]).InTransaction then
        TIBTransaction(Components[i]).Commit;
      Components[i].Free;
    end;
  end;

  // Tabs schließen
  RemovePreviousResultTabs;
  OutputTabsList.Free;

  CloseAction := caFree;
end;

{ Initialize auto-completion text in QueryWindow OnCreate event }

procedure TfmQueryWindow.FormCreate(Sender: TObject);
var
   configFile: TIniFile;
   configFilePath: String;
begin
  rgScreenModes.Hint :=
    'Screen Modes' + LineEnding +
    'NFS = Normal Screen' + LineEnding +
    'CFS = Code Window FullScreen' + LineEnding +
    'RFS = Result Window FullScreen';

  OutputTabsList:= nil;
  {$IFNDEF DEBUG}
  // Do not log to debug server if built as release instead of debug
  SetDebuggingEnabled(false);
  {$ENDIF}
  FQuery:= TStringList.Create;
  // Initialize new instance of FIBConnection and SQLTransaction
  //FIBConnection:= TIBDatabase.Create(nil);
  //FSQLTrans:= TIBTransaction.Create(nil);
  //FIBConnection.DefaultTransaction := FSQLTrans;
  SynCompletion1.ItemList.Add('select * from ');
  //SynCompletion1.ItemList.CommaText:= QuotedStr('select * from ') + 'Select, *,  From, create, table';
  SortSynCompletion;

  LoadpmUnIntelliSense;

  meQuery.Font.Name  := turbocommon.QWEditorFontName;
  meQuery.Font.Size  := turbocommon.QWEditorFontSize;
  meQuery.Font.Style := turbocommon.QWEditorFontStyle;

  meQuery.Color := turbocommon.QWEditorBackgroundColor;
end;

procedure TfmQueryWindow.FormDestroy(Sender: TObject);
begin
  // Clean up resources to avoid memory leaks
  FQuery.Free;
end;

procedure TfmQueryWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if ((Trim(meQuery.Lines.Text) = '') or
      (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes))
      then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;


{ focus on Query SQL window editor on form show }

procedure TfmQueryWindow.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
  meQuery.SetFocus;

  {if AllowIniOverrides then
  begin
    meQuery.Color      :=  QWEditorBackgroundColor;
    meQuery.Font.Name  :=  QWEditorFontName;
    meQuery.Font.Size  :=  QWEditorFontSize;
    meQuery.Font.Color :=  QWEditorFontColor;
    meQuery.Font.Style :=  QWEditorFontStyle;
  end;}
end;

{ Close current Query window }

procedure TfmQueryWindow.lmCloseTabClick(Sender: TObject);
begin
  if (Trim(meQuery.Lines.Text) = '') or
    (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
  begin
    Close;
    Parent.Free;
  end;
end;

{ Copy cell in clipboard }

{procedure TfmQueryWindow.lmCopyCellClick(Sender: TObject);
var Field: TField; Grid: TDBGrid;
begin
  Grid := TdbGrid(pmGrid.PopupComponent);
  Field := Grid.SelectedField;
   if (Field is TMemoField) or (Field is TBlobField) then
   begin
     Clipboard.AsText := Field.DataSet.FieldByName(Field.FieldName).AsString;
   end
   else
   begin
     // Normales Feld
     Clipboard.AsText := Field.AsString;
   end;

  //Clipboard.AsText := TdbGrid(pmGrid.PopupComponent).SelectedField.AsString;
end;}

procedure TfmQueryWindow.lmCopyCellClick(Sender: TObject);
var
  Field: TField; Grid: TDBGrid;
  ServerNode, DBNode, SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  Rec: TDatabaseRec;
  dbIndex: Integer;
  ATab: TTabSheet;
  //frmBlobEdit: TfrmBlobEdit;
  Server, DBAlias, ShortTitle, FullHint: string;
begin
  {Grid := TdbGrid(pmGrid.PopupComponent);
  Field := Grid.SelectedField;

  if not ((Field is TMemoField) or (Field is TBlobField)) then exit;
  SelNode := fmMain.tvMain.Selected;
  if SelNode = nil then Exit;

  ServerNode := TTreeNode(GetAncestorAtLevel(SelNode, 0));
  DBNode     := TTreeNode(GetAncestorAtLevel(SelNode, 1));

  if (ServerNode = SelNode) or (ServerNode = nil) or (DBNode = nil) or (ServerNode = DBNode) then
    exit;

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
    ATab.PageControl := fmMain.PageControl1;
    ATab.Tag := dbIndex;

    frmBlobEdit.Parent := ATab;
    frmBlobEdit.Align := alClient;
    frmBlobEdit.BorderStyle := bsNone;

    NodeInfos^.ViewForm := frmBlobEdit;
  end;

  // Tab-Titel kurz halten
  ShortTitle := 'Blob Viewer';
  ATab.Caption := ShortTitle;

  // Vollständiger Kontext im Tooltip
  FullHint :=
    'Server: '  + Server + sLineBreak +
    'DBAlias: ' + DBAlias + sLineBreak +
    'DBPath: '  + Rec.RegRec.DatabaseName + sLineBreak +
    'Object: BLOB-Viewer';

  ATab.Hint := FullHint;
  ATab.ShowHint := True;

  // Tab aktivieren + Initialisierung
  fmMain.PageControl1.ActivePage := ATab;
  frmBlobEdit.Init(Field, NodeInfos);
  frmBlobEdit.Show; }
end;

procedure TfmQueryWindow.pmGridPopup(Sender: TObject);
var Field: TField; Grid: TDBGrid;
begin
  Grid := TdbGrid(pmGrid.PopupComponent);
  Field := Grid.SelectedField;
end;

procedure TfmQueryWindow.lmStdExportFormatsClick(Sender: TObject);
var SqlQuery: TIBQuery;
begin
  SqlQuery:= GetCurrentSelectQuery;

  if not Assigned(SqlQuery) then
  begin
    ShowMessage('Query not assigned!');
    exit;
  end;

  if SqlQuery.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;

  ExportStdFormat(SqlQuery);
end;

{ Copy query text into clipboard }

procedure TfmQueryWindow.lmCopyClick(Sender: TObject);
begin
  meQuery.CopyToClipboard;
end;


{ Cut query text into clipboard}

procedure TfmQueryWindow.lmCutClick(Sender: TObject);
begin
  meQuery.CutToClipboard;
end;

procedure TfmQueryWindow.lmExportDataAsHtmlClick(Sender: TObject);
var SqlQuery: TIBQuery;
begin
  SqlQuery:= GetCurrentSelectQuery;

  if not Assigned(SqlQuery) then
  begin
    ShowMessage('Query not assigned!');
    exit;
  end;

  if SqlQuery.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;
  ExportDataHtml(SqlQuery);
end;

procedure TfmQueryWindow.lmExportDataAsMarkDownTableClick(Sender: TObject);
var SqlQuery: TIBQuery;
begin
  SqlQuery:= GetCurrentSelectQuery;

  if not Assigned(SqlQuery) then
  begin
    ShowMessage('Query not assigned!');
    exit;
  end;

  if SqlQuery.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;
  ExportDataMarkDownTable(SqlQuery);
end;

procedure TfmQueryWindow.lmExportDataAsPDFClick(Sender: TObject);
begin
  try
    RxDBGridExportPDF1.Execute;

  except
    raise;
  end;
  {if SaveDialogPDF.Execute then
  begin
    RxDBGridExportPDF1.FileName := SaveDialogPDF.FileName;

  end;}
end;

procedure TfmQueryWindow.lmExportDataAsSpreadSheetClick(Sender: TObject);
begin
  RxDBGridExportSpreadSheet1.Execute;
end;

procedure TfmQueryWindow.lmPrintDataClick(Sender: TObject);
begin
   RxDBGridPrint1.Execute;
end;


{ Copy query result to Clipboard }

procedure TfmQueryWindow.lmExportToClipboardClick(Sender: TObject);
var
  SqlQuery: TIBQuery;
  Grid: TIBDynamicGrid;
  MaxExportRows, RowCount, CopiedRows: Integer;
  MsgText: string;
begin
  Grid := TIBDynamicGrid(pmGrid.PopupComponent);
  Grid.DataSource.DataSet.DisableControls;

  SqlQuery := GetCurrentSelectQuery;
  //SqlQuery.Last;

  if not Assigned(SqlQuery) then
  begin
    MessageDlg('Query not assigned!', mtError, [mbOK], 0);
    Grid.DataSource.DataSet.EnableControls;
    Exit;
  end;

  if SqlQuery.IsEmpty then
  begin
    MessageDlg('DataSet has no records!', mtError, [mbOK], 0);
    Grid.DataSource.DataSet.EnableControls;
    Exit;
  end;

  // --- Max rows load from INI---
  MaxExportRows := fIniFile.ReadInteger('ClipboardExport', 'MaxExportRows', -1);
  if MaxExportRows <= 0 then
  begin
    if MaxExportRows = -1 then
    begin
      MaxExportRows := 200;
      fIniFile.WriteInteger('ClipboardExport', 'MaxExportRows', MaxExportRows);
      MessageDlg('No valid MaxExportRows entry found in turbobird.ini. Default 200 has been set.', mtWarning, [mbOK], 0);
    end
    else
    begin
      MessageDlg('MaxExportRows is set to 0 in turbobird.ini. ' +
                 'Exporting very large tables may cause program or system crash!', mtWarning, [mbOK], 0);
    end;
  end;

  RowCount := SqlQuery.RecordCount;

  if (MaxExportRows > 0) and (RowCount > MaxExportRows) then
  begin
    MessageDlg(
      'The result contains ' + IntToStr(RowCount) + ' rows, which exceeds the export limit of ' +
      IntToStr(MaxExportRows) + ' rows.'#13#10 +
      'Only the first ' + IntToStr(MaxExportRows) + ' rows will be copied to the clipboard.'#13#10#13#10 +
      'You can change this limit in turbobird.ini under [ClipboardExport].',
      mtWarning, [mbOK], 0
    );
  end;

  if MaxExportRows = 0 then
    CopiedRows := RowCount
  else if RowCount > MaxExportRows then
    CopiedRows := MaxExportRows
  else
    CopiedRows := RowCount;

  // --- Export ---
  try
    ExportDataToClipboard(SqlQuery, MaxExportRows);

    MsgText := 'Successfully copied ' + IntToStr(CopiedRows) + ' records to the clipboard.';
    if MaxExportRows = 0 then
      MsgText := MsgText + ' (Warning: no export limit set in turbobird.ini! Large tables may cause program/system crash.)';

    MessageDlg(MsgText, mtInformation, [mbOK], 0);
  finally
    Grid.DataSource.DataSet.First;
    Grid.DataSource.DataSet.EnableControls;
  end;
end;


{ Paste from clipboard into SQL editor }
procedure TfmQueryWindow.lmPasteClick(Sender: TObject);
begin
  meQuery.PasteFromClipboard;
end;

{ SQL Editor Redo }

procedure TfmQueryWindow.lmRedoClick(Sender: TObject);
begin
  meQuery.Redo;
end;


{ Run Query, auto type detection }

procedure TfmQueryWindow.lmRunClick(Sender: TObject);
begin
  CallExecuteQuery(qtUnknown);
end;


{ Run query and force its type as executable statement}

procedure TfmQueryWindow.lmRunExecClick(Sender: TObject);
begin
  CallExecuteQuery(qtExecute);
end;


{ Run query, and force its type as script }

procedure TfmQueryWindow.lmRunScriptClick(Sender: TObject);
begin
  CallExecuteQuery(qtScript);
end;


{ Run query, force its type as select statement }

procedure TfmQueryWindow.lmRunSelectClick(Sender: TObject);
begin
  CallExecuteQuery(qtSelectable);
end;


{ select all in SQL Editor }

procedure TfmQueryWindow.lmSelectAllClick(Sender: TObject);
begin
  meQuery.SelectAll;
end;

{ SQL Editor undo }

procedure TfmQueryWindow.lmUndoClick(Sender: TObject);
begin
  meQuery.Undo;
end;


{ Search in SQL Editor }

procedure TfmQueryWindow.lmFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;


{ Find again }

procedure TfmQueryWindow.lmFindAgainClick(Sender: TObject);
begin
  meQuery.SearchReplace(FindDialog1.FindText, '', FOptions);
end;

procedure TfmQueryWindow.meQueryMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := @fmMain.AppShowHint;
end;

procedure TfmQueryWindow.Panel1MouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.Panel1MouseLeave(Sender: TObject);
begin
  Application.OnShowHint := @fmMain.AppShowHint;
end;

procedure TfmQueryWindow.pgOutputPageCtlChange(Sender: TObject);
begin

end;

//mit parser
{procedure TfmQueryWindow.meQueryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  P: TPoint;
  CursorPos: Integer;
  TextBeforeCursor: string;
  SQLParser: TSelectSQLParserExt;
begin
  if (ssCtrl in Shift) and (Key = VK_RETURN) then
  begin
    CallExecuteQuery(qtUnknown);
    Key := 0;
    Exit;
  end;

  if (ssCtrl in Shift) and (Key = VK_SPACE) then
  begin
    P := GetSynEditCaretScreenPos(meQuery);
    CursorPos := CaretPosToOffset(meQuery);
    TextBeforeCursor := Copy(meQuery.Text, 1, CursorPos);

    //SQLParser := TSelectSQLParserExt.Create(TextBeforeCursor);
    SQLParser := TSelectSQLParserExt.CreateFromString(TextBeforeCursor);

    try
      SQLParser.BuildAliasMap;
      ShowIntelliSensePopup(SQLParser); // Jetzt passend
      pmUnIntelliSense.Popup(P.X, P.Y);
    finally
      SQLParser.Free;
    end;

    Key := 0;
  end;
end; }

function CaretPosToOffset(ASynEdit: TSynEdit): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ASynEdit.CaretY - 2 do
    Inc(Result, Length(ASynEdit.Lines[i]) + 1); // +1 für Zeilenumbruch
  Inc(Result, ASynEdit.CaretX - 1);             // Spalte addieren
end;

function GetSynEditCaretScreenPos(ASynEdit: TSynEdit): TPoint;
var
  P: TPoint;
begin
  // Caret → Client-Pixel
  P := ASynEdit.RowColumnToPixels(ASynEdit.CaretXY);

  // etwas unterhalb der Zeile
  Inc(P.Y, ASynEdit.LineHeight);

  // Client → Screen
  Result := ASynEdit.ClientToScreen(P);
end;

{ Run query by pressing Ctrl + Enter }

procedure TfmQueryWindow.meQueryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var P: TPoint;
    Node: TTreeNode;
    NodeInfo: TPNodeInfos;
begin
  // Execute query by pressing Ctrl + Enter
  if (ssCtrl in shift) and (key = VK_RETURN) then
  begin
    CallExecuteQuery(qtUnknown);
    key:= 0;
  end;

  if (Key = VK_SPACE) and (ssCtrl in Shift) then
  begin
    P := GetSynEditCaretScreenPos(meQuery);
    pmUnIntelliSense.Popup(P.X, P.Y);
    Key := 0; // Event verbrauchen
  end;
end;

procedure TfmQueryWindow.pmUnIntelliSenseSubItemClick(Sender: TObject);
begin
  if (TMenuItem(Sender).Caption = UpperCase(TMenuItem(Sender).Caption)) then
    meQuery.SelText := TMenuItem(Sender).Caption
  else
    meQuery.SelText := MakeObjectNameQuoted(TMenuItem(Sender).Caption);
end;

function TfmQueryWindow.GetFieldsForAlias(const AliasName: string;
  Cache: TUnIntelliSenseCache; Parser: TSelectSQLParserExt): TStringList;
var
  TableName: string;
  i: Integer;
begin
  Result := TStringList.Create;

  // Alias auflösen (wenn vorhanden)
  TableName := Parser.ResolveAlias(AliasName);

  // Tabelle im Cache suchen
  for i := 0 to Cache.TableCache.Count - 1 do
  begin
    if Cache.TableCache[i] = TableName then
    begin
      if Assigned(Cache.FieldCache[i]) then
      begin
        Result.AddStrings(Cache.FieldCache[i]);
        Result.Delete(0); // Entferne den ersten Eintrag = Tabellenname
      end;
      Exit;
    end;
  end;
end;

procedure TfmQueryWindow.pmUnIntelliSenseMaintemClick(Sender: TObject);
var
  DBNode: TTreeNode;
  IntelliCache: TUnIntelliSenseCache;
  TableItem, FieldItem: TMenuItem;
  Fields: TStringList;
  i: Integer;
begin
  TableItem := TMenuItem(Sender);

  // Nur einmal laden (Tabellenname + Separator)
  if TableItem.Count <> 2 then Exit;

  DBNode := turbocommon.GetAncestorAtLevel(fmMain.tvMain.Selected, 1);
  IntelliCache := TPNodeInfos(DBNode.Data)^.UnIntelliSenseCache;

  if (IntelliCache = nil) or not IntelliCache.Initialized then
  begin
    MessageDlg(
      'IntelliSense',
      'The IntelliSense cache is not initialized yet.',
      mtInformation,
      [mbOK],
      0
    );
    Exit;
  end;

  //LAZY LOAD aus Cache
  Fields := IntelliCache.FieldsForTable(TableItem.Caption);
  if Fields = nil then Exit;

  // Felder unterhalb des Separators einfügen
  for i := 1 to Fields.Count - 1 do   // Index 0 = Tabellenname
  begin
    FieldItem := TMenuItem.Create(TableItem);
    FieldItem.Caption := Fields[i];
    FieldItem.OnClick := @pmUnIntelliSenseSubItemClick;
    TableItem.Add(FieldItem);
  end;
end;

procedure TfmQueryWindow.LoadpmUnIntelliSense;
var
  TableItem, FieldItem, DummyItem: TMenuItem;
  TableName, FieldName: string;
  i, j: Integer;
  DBNode: TTreeNode;
  IntelliCache: TUnIntelliSenseCache;
begin
  pmUnIntelliSense.Items.Clear;

  // Ancestor & Cache holen
  DBNode := turbocommon.GetAncestorAtLevel(fmMain.tvMain.Selected, 1);
  IntelliCache := TPNodeInfos(DBNode.Data)^.UnIntelliSenseCache;

  if (IntelliCache = nil) or not IntelliCache.Initialized then
  begin
    MessageDlg(
      'IntelliSense',
      'The IntelliSense cache is not initialized yet.',
      mtInformation,
      [mbOK],
      0
    );
    Exit;
  end;

  // Über Tabellen iterieren
  for i := 0 to IntelliCache.TableCache.Count - 1 do
  begin
    TableName := IntelliCache.TableCache[i];

    // Hauptmenü = Tabelle
    TableItem := TMenuItem.Create(pmUnIntelliSense);
    TableItem.Caption := TableName;
    TableItem.OnClick := @pmUnIntelliSenseMaintemClick;


    // DummyItem mit Tabellenname
    DummyItem := TMenuItem.Create(TableItem);
    DummyItem.Caption := TableName;
    DummyItem.OnClick := @pmUnIntelliSenseSubItemClick;
    TableItem.Add(DummyItem);

    // Trennstrich
    DummyItem := TMenuItem.Create(TableItem);
    DummyItem.Caption := '-';
    TableItem.Add(DummyItem);

    // Felder aus Cache holen
    {if (i <= High(IntelliCache.FieldCache)) and Assigned(IntelliCache.FieldCache[i]) then
    begin
      for j := 0 to IntelliCache.FieldCache[i].Count - 1 do
      begin
        FieldName := IntelliCache.FieldCache[i][j];

        // Skip first entry? (falls erste Position TableName ist)
        if j = 0 then Continue;

        FieldItem := TMenuItem.Create(TableItem);
        FieldItem.Caption := FieldName;
        FieldItem.OnClick := @pmUnIntelliSenseSubItemClick;

        TableItem.Add(FieldItem);
      end;
    end;
    }
    pmUnIntelliSense.Items.Add(TableItem);
  end;
end;

procedure TfmQueryWindow.pmUnIntelliSensePopup(Sender: TObject);
var DBNode: TTreeNode;
    IntelliCache: TUnIntelliSenseCache;
begin
  if MetaDataChanged then
  begin
    DBNode := turbocommon.GetAncestorAtLevel(fmMain.tvMain.Selected, 1);
    if DBNode = nil  then exit;
    if DBNode.Data = nil then exit;

    IntelliCache := TPNodeInfos(DBNode.Data)^.UnIntelliSenseCache;

    if (IntelliCache = nil) or not IntelliCache.Initialized then
    begin
      MessageDlg(
        'IntelliSense',
        'The IntelliSense cache is not initialized yet.',
        mtInformation,
        [mbOK],
        0
      );
      Exit;
    end;

    IntelliCache.RefreshCache;
    MetaDataChanged := false;
    LoadpmUnIntelliSense;

  end;
end;

procedure TfmQueryWindow.rgScreenModesClick(Sender: TObject);
begin
  case rgScreenModes.ItemIndex of
  0:begin
      //fmMain.tvMain.Visible  := true;
      //fmMain.ToolBar1.Visible := true;
      meQuery.Align := alTop;
      meQuery.Visible := true;
      pnlOutputPanel.Visible := true;
    end;
  1:begin
      //fmMain.tvMain.Visible  := true;
      //fmMain.ToolBar1.Visible := false;
      pnlOutputPanel.Visible := false;
      meQuery.Align := alClient;
      meQuery.Visible := true;
    end;
  2:begin
      //fmMain.tvMain.Visible  := true;
      //fmMain.ToolBar1.Visible := false;
      meQuery.Visible := false;
      meQuery.Align := alTop;
      pnlOutputPanel.Visible := true;
    end;
  end;
end;

procedure TfmQueryWindow.rgScreenModesMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;


{ Scrolling in query result recordset }

procedure TfmQueryWindow.QueryAfterScroll(DataSet: TDataSet);
var
  Ctl: TControl;
  TabSheet: TTabSheet;
  i: Integer;
begin
  TabSheet:= nil;

  // Get DataSet's TTabsheet
  // The query object's tag should be the tab index number
  if (Dataset is TIBQuery) then
    TabSheet:= pgOutputPageCtl.Pages[TIBQuery(DataSet).Tag];

  if assigned(TabSheet) then
  begin
    for i:= 0 to TabSheet.ControlCount-1 do
    begin
      Ctl:= TabSheet.Controls[i];
      if (Ctl is TStatusBar) then
      begin
        // Display current record and number of total records in status bar
        TStatusBar(Ctl).SimpleText:= IntToStr(DataSet.RecordCount) +
          ' records fetched. At record # ' + IntToStr(DataSet.RecNo);
        break;

      end;
    end;
  end;
end;


{ Execute query according to passed query type }

procedure TfmQueryWindow.CallExecuteQuery(aQueryType: TQueryTypes);
begin
  // Get query text from memo
  if not(GetQuery(FQuery)) then
  begin
    ShowMessage('Could not get valid query');
    exit;
  end;

  FStartLine:= 0;
  RemovePreviousResultTabs;

  // Disable buttons to prevent query interrupt
  tbRun.Enabled:= False;
  tbCommit.Enabled:= False;
  tbCommitRetaining.Enabled:= False;
  tbRollback.Enabled:= False;
  tbRollbackRetaining.Enabled:= False;

  Application.ProcessMessages;
  FModifyCount:= 0;

  // Get initial query type; this can be changed later in the next parts
  if aQueryType = qtUnknown then // Auto
    FOrigQueryType:= GetQueryType(FQuery.Text)
  else
    FOrigQueryType:= aQueryType;

  // Call execute query for each part until finished
  FCounter:= 0;
  FFinished:= False;

  repeat
    ExecuteQuery;
  until FFinished;
  EnableButtons;
  Application.ProcessMessages;
end;


{ sort auto completion options }

procedure TfmQueryWindow.SortSynCompletion;
var
  SortingList: TStringList;
  i: Integer;
begin
  SortingList:=TStringList.Create;
  try
    for i:=0 to SynCompletion1.ItemList.Count-1 do
       SortingList.Add(SynCompletion1.ItemList.Strings[i]);
    SortingList.Sort;
    SynCompletion1.ItemList.Clear;
    for i:=0 to SortingList.Count-1 do
      SynCompletion1.ItemList.Add(SortingList.Strings[i]);
  finally
    SortingList.Free;
  end;
end;


{ SQL thread termination }

procedure TfmQueryWindow.ThreadTerminated(Sender: TObject);
var
  aSQLQuery: TIBQuery;
begin
  // Raise exception if an error occured during thread execution (Open)
  if FQT.Error then
  begin
    if Assigned(FTab) then
      FTab.TabVisible:= False;
    aSQLQuery:= (Sender as TQueryThread).Query;
    FTab:= CreateResultTab(qtExecute, aSQLQuery, FSQLScript, FResultMemo);
    pgOutputPageCtl.ActivePage:= FTab;

    FResultMemo.Text:= FQT.ErrorMsg;
    FResultMemo.Lines.Add(FQueryPart);
    FResultMemo.Font.Color:= clRed;
    FTab.Font.Color:= clRed;
    FTab.ImageIndex:= 3;
  end else
  begin
    FTab.Caption:= FAText;
    FTab.ImageIndex:= 0;
    fmMain.AddToSQLHistory(FRegRec.Title, 'SELECT', FQueryPart);
  end;

  FQT.Free;
  if FFinished then
    EnableButtons;

  if not FFinished then
    ExecuteQuery;

end;

{ Enable SQL buttons: Run, Commit, Rollbak after thread termination }

procedure TfmQueryWindow.EnableButtons;
begin
  tbRun.Enabled:= True;
  tbCommit.Enabled:= True;
  tbCommitRetaining.Enabled:= True;
  tbRollback.Enabled:= True;
  tbRollbackRetaining.Enabled:= True;
end;

initialization
  {$I querywindow.lrs}

end.

