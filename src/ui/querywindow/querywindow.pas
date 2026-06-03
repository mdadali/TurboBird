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

  IBDynamicGrid, IBQuery, IBSQL, IBDatabase, IBTable, IB, RxDBGrid, RxDBGridExportPdf,
  RxDBGridPrintGrid, RxDBGridExportSpreadSheet, ibxscript,

  SysTables,
  fdataexportersintrf,
  //fblobedit,
  uthemeselector,
  fsimpleobjextractor,
  cUnIntelliSenseCache,
  cSelectSQLParserExt,

  ibsqleditor,
  fSQLParser;

type

  { TfmQueryWindow }

  TfmQueryWindow = class(TForm)
    cxAutoCommit: TCheckBox;
    FindDialog1: TFindDialog;
    FontDialog1: TFontDialog;
    FIBConnection: TIBDatabase;
    FontDialogEditor: TFontDialog;
    FQueryTrans: TIBTransaction;
    FScriptTrans: TIBTransaction;
    FIBXScript: TIBXScript;
    lmExportDataSet: TMenuItem;
    lmExportDataAsMarkDownTable: TMenuItem;
    lmStdExportFormats: TMenuItem;
    lmExportDataAsHtml: TMenuItem;
    lmExportToClipboard: TMenuItem;
    lmExportDataAsPDF: TMenuItem;
    lmPrintData: TMenuItem;
    lmExportDataAsSpreadSheet: TMenuItem;
    lmSQLParser: TMenuItem;
    Separator3: TMenuItem;
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
    tbIBSQL: TToolButton;
    ToolButton6: TToolButton;
    procedure bbRunClick(Sender: TObject);
    procedure cxAutoCommitChange(Sender: TObject);
    procedure cxAutoCommitMouseEnter(Sender: TObject);
    procedure cxAutoCommitMouseLeave(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure FIBXScriptSelectSQL(Sender: TObject; SQLText: string);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FQueryTransAfterTransactionEnd(Sender: TObject);
    procedure FQueryTransStartTransaction(Sender: TObject);
    procedure FScriptTransAfterTransactionEnd(Sender: TObject);
    procedure FScriptTransStartTransaction(Sender: TObject);
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
    procedure lmSQLParserClick(Sender: TObject);
    procedure lmStdExportFormatsClick(Sender: TObject);
    procedure lmUndoClick(Sender: TObject);
    procedure lmFindClick(Sender: TObject);
    procedure lmFindAgainClick(Sender: TObject);
    procedure meQueryChangeUpdating(ASender: TObject; AnUpdating: Boolean);
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
    procedure tbIBSQLClick(Sender: TObject);
  private
    { private declarations }
    FLastStatementWasSelect: Boolean;

    FDBIndex: Integer; // Index of selected registered database
    FRegRec: TRegisteredDatabase;
    FOptions: set of TSynSearchOption;
    FCanceled: Boolean;
    FStartLine: Integer;
    FQuery: TStringList; //query text
    FFinished: Boolean;
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

    function SplitSQLStatements(const SQL: string): TStringList;
    function IsOnlyComments(const SQL: string): Boolean;
    function IsTransactionControl(const SQL: string): Boolean;
    procedure ExecuteQuery;
    function GetNewTabNum: string;
    // Gets TIBQuery of current result tabsheet - only if it is a select query
    function GetCurrentSelectQuery: TIBQuery;
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
    FCloseCancelled: Boolean;
    OnCommit: TNotifyEvent;
    procedure Init(dbIndex: Integer; ANodeInfos: TPNodeInfos=nil);

    function GetStatementType(const SQL: string): TIBSQLStatementTypes;

    // Get query text from GUI/memo into
    // QueryContents
    function GetQuery(QueryContents: tstrings): boolean;
    function CreateResultTab(IsSelect: Boolean;
      var aSqlQuery: TIBQuery; var meResult: TMemo;
      AdditionalTitle: string = ''): TTabSheet;

    procedure OnFIBXScriptSelectSQL(Sender: TObject; SQLText: string);
    function ExecuteScript(Script: string): Boolean;

    // Create a new Apply button in the specified panel
    procedure NewApplyButton(var Pan: TPanel; var ATab: TTabSheet);
    // Returns whether query is DDL or DML
    procedure QueryAfterPost(DataSet: TDataSet);
    procedure QueryAfterScroll(DataSet: TDataSet);

    function  IsSQLScript(const SQL: string): Boolean;
    // Run query; use aQueryType to force running as e.g. script or open query
    procedure CallExecuteQuery;

    procedure SortSynCompletion;

    { public declarations }
    procedure SetTransactionButtonsState(AEnabled: Boolean);
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
      FQueryTrans.Commit
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
  FQueryTrans.CommitRetaining;
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

procedure TfmQueryWindow.tbCommitClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TIBQuery;
  ATab: TTabSheet;
begin
  if not FQueryTrans.InTransaction and not FScriptTrans.InTransaction then
  begin
    ShowMessage('No active transaction to commit.');
    Exit;
  end;

  ATab := CreateResultTab(False, SqlQuery, meResult);
  try
    if FQueryTrans.InTransaction then
      FQueryTrans.Commit;
    if FScriptTrans.InTransaction then
      FScriptTrans.Commit;

    ATab.ImageIndex := 4;
    meResult.Lines.Add('Committed');
    meResult.Font.Color := clGreen;
    if OnCommit <> nil then OnCommit(Self);
    OnCommit := nil;

    // AfterTransactionEnd deaktiviert die Buttons automatisch.
  except
    on E: Exception do
    begin
      ATab.ImageIndex := 3;
      meResult.Lines.Text := E.Message;
      meResult.Font.Color := clRed;
    end;
  end;
end;

procedure TfmQueryWindow.tbRollbackClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TIBQuery;
  ATab: TTabSheet;
begin
  if not FQueryTrans.InTransaction and not FScriptTrans.InTransaction then
  begin
    ShowMessage('No active transaction to rollback.');
    Exit;
  end;

  ATab := CreateResultTab(False, SqlQuery, meResult);
  try
    if FQueryTrans.InTransaction then
      FQueryTrans.Rollback;
    if FScriptTrans.InTransaction then
      FScriptTrans.Rollback;

    ATab.ImageIndex := 4;
    meResult.Lines.Add('Rollback performed');
    meResult.Font.Color := clGreen;
    if OnCommit <> nil then OnCommit(Self);
    OnCommit := nil;

    // AfterTransactionEnd deaktiviert die Buttons automatisch.
  except
    on E: Exception do
    begin
      ATab.ImageIndex := 3;
      meResult.Lines.Text := E.Message;
      meResult.Font.Color := clRed;
    end;
  end;
end;

procedure TfmQueryWindow.tbCommitRetainingClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TIBQuery;
  ATab: TTabSheet;
begin
  if not FQueryTrans.InTransaction and not FScriptTrans.InTransaction then
  begin
    ShowMessage('No active transaction to commit (retaining).');
    Exit;
  end;

  ATab := CreateResultTab(False, SqlQuery, meResult);
  try
    if FQueryTrans.InTransaction then
      FQueryTrans.CommitRetaining;
    if FScriptTrans.InTransaction then
      FScriptTrans.CommitRetaining;

    ATab.ImageIndex := 4;
    meResult.Lines.Add('Commit Retaining performed.');
    meResult.Font.Color := clGreen;

    if OnCommit <> nil then OnCommit(Self);
    OnCommit := nil;

    // Alles gesichert, aber Transaktion läuft noch
    SetTransactionButtonsState(False);
    tbCommit.Enabled := True;
  except
    on E: Exception do
    begin
      ATab.ImageIndex := 3;
      meResult.Lines.Text := E.Message;
      meResult.Font.Color := clRed;
    end;
  end;
end;

procedure TfmQueryWindow.tbRollbackRetainingClick(Sender: TObject);
var
  meResult: TMemo;
  SqlQuery: TIBQuery;
  ATab: TTabSheet;
begin
  if not FQueryTrans.InTransaction and not FScriptTrans.InTransaction then
  begin
    ShowMessage('No active transaction to rollback (retaining).');
    Exit;
  end;

  ATab := CreateResultTab(False, SqlQuery, meResult);
  try
    if FQueryTrans.InTransaction then
      FQueryTrans.RollbackRetaining;
    if FScriptTrans.InTransaction then
      FScriptTrans.RollbackRetaining;

    ATab.ImageIndex := 4;
    meResult.Lines.Add('Rollback Retaining performed.');
    meResult.Font.Color := clGreen;

    if OnCommit <> nil then OnCommit(Self);
    OnCommit := nil;

    // Alles verworfen, aber Transaktion läuft noch
    SetTransactionButtonsState(False);
    tbCommit.Enabled := True;
  except
    on E: Exception do
    begin
      ATab.ImageIndex := 3;
      meResult.Lines.Text := E.Message;
      meResult.Font.Color := clRed;
    end;
  end;
end;

procedure TfmQueryWindow.tbCommitMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
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
    QW: TfmQueryWindow;
begin
  // Get a free number to be assigned to the new Query window
  for i:= 1 to 1000 do
  begin
    if fmMain.FindQueryWindow('SQL # ' + IntToStr(i)) = nil then
    begin
      QW := fmMain.ShowCompleteQueryWindow(FDBIndex, 'SQL # ' + IntToStr(i), '', nil);
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
  begin
    meQuery.Lines.LoadFromFile(OpenDialog1.FileName);
    if not tbRun.Enabled then
      tbRun.Enabled := true;
  end;
end;

procedure TfmQueryWindow.tbOpenMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.tbQueryDesignMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.tbRollbackMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

procedure TfmQueryWindow.tbRollbackRetainingMouseEnter(Sender: TObject);
begin
  Application.OnShowHint := nil;
end;

{ Run current SQL, auto-detect type }
procedure TfmQueryWindow.tbRunClick(Sender: TObject);
begin
  tbRun.Enabled := False;
  Application.ProcessMessages;

  try
    CallExecuteQuery;
  finally
    tbRun.Enabled := true;
    Application.ProcessMessages;
  end;

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

  FDbConnection := RegisteredDatabases[dbIndex].IBDatabase;
  FTransaction  := RegisteredDatabases[dbIndex].IBTransaction;

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

procedure TfmQueryWindow.tbIBSQLClick(Sender: TObject);
var
  SelNode: TTreeNode;
  AGenName: string;
  dbIndex: Integer;
  Rec: TDatabaseRec;
  IBSQL: TIBSQL;
begin
  try
    SelNode:= fmMain.tvMain.Selected;

    if (SelNode = nil) or (SelNode.Parent = nil) then
      exit;

    dbIndex:= TPNodeInfos(SelNode.Data)^.dbIndex;
    Rec := RegisteredDatabases[dbIndex];

    IBSQL := TIBSQL.Create(self);
    IBSQL.Database := Rec.IBDatabase;
    IBSQL.Transaction := Rec.IBDatabase.DefaultTransaction;

    if EditSQL(IBSQL) then
      meQuery.Text := IBSQL.SQL.Text + ';';
  finally
    IBSQL.Free;
  end;
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

{procedure TfmQueryWindow.Init(dbIndex: Integer; ANodeInfos: TPNodeInfos = nil);
var
  i: Integer;
  TxParams: TStrings;
begin
  FNodeInfos := ANodeInfos;
  FDBIndex   := dbIndex;
  FRegRec    := RegisteredDatabases[dbIndex].RegRec;

  // Alte Tabs nur innerhalb DIESES Fensters entfernen
  if Assigned(OutputTabsList) then
    RemovePreviousResultTabs
  else
    OutputTabsList := TStringList.Create;

  // Eigene Datenbank‑Kopie für dieses Fenster
  AssignIBDatabase(RegisteredDatabases[dbIndex].IBDatabase, FIBConnection);
  FQueryTrans.DefaultDatabase := FIBConnection;
  FIBConnection.DefaultTransaction := FQueryTrans;

  // Skript‑Komponenten an die lokale DB binden
  FScriptTrans.DefaultDatabase := FIBConnection;
  FIBXScript.Database := FIBConnection;
  FIBXScript.Transaction := FScriptTrans;

  // Transaktionsparameter aus registrierter DB holen
  TxParams := nil;
  if Assigned(RegisteredDatabases[dbIndex].IBTransaction) then
    TxParams := RegisteredDatabases[dbIndex].IBTransaction.Params;

  // Verbinden + Transaktion mit Parametern starten
  ConnectDBPrepared(FIBConnection, FQueryTrans, FDBIndex, TxParams);

  // Tabellennamen für Syntax‑Highlighting laden
  SynSQLSyn1.TableNames.CommaText := fmMain.GetTableNames(dbIndex);
  for i := 0 to SynSQLSyn1.TableNames.Count - 1 do
    if IsObjectNameCaseSensitive(SynSQLSyn1.TableNames[i]) then
      SynSQLSyn1.TableNames[i] := MakeObjectNameQuoted(SynSQLSyn1.TableNames[i]);
  SynCompletion1.ItemList.AddStrings(SynSQLSyn1.TableNames);
end;}

procedure TfmQueryWindow.Init(dbIndex: Integer; ANodeInfos: TPNodeInfos = nil);
var
  i: Integer;
begin
  FNodeInfos := ANodeInfos;
  FDBIndex   := dbIndex;
  FRegRec    := RegisteredDatabases[dbIndex].RegRec;

  // Alte Tabs nur innerhalb DIESES Fensters entfernen
  if Assigned(OutputTabsList) then
    RemovePreviousResultTabs
  else
    OutputTabsList := TStringList.Create;

  // Eigene Datenbank‑Kopie für dieses Fenster
  AssignIBDatabase(RegisteredDatabases[dbIndex].IBDatabase, FIBConnection);
  FQueryTrans.DefaultDatabase := FIBConnection;
  FIBConnection.DefaultTransaction := FQueryTrans;

  // Skript‑Komponenten an die lokale DB binden
  FScriptTrans.DefaultDatabase := FIBConnection;
  FIBXScript.Database := FIBConnection;
  FIBXScript.Transaction := FScriptTrans;

  // Verbindung herstellen – mit OnLogin-Schutz, aber ohne automatische Transaction
  if not FIBConnection.Connected then
  begin
    FIBConnection.Params.Values['user_name'] := FRegRec.UserName;
    FIBConnection.Params.Values['password']  := FRegRec.Password;
    FIBConnection.OnLogin := @dmSysTables.OnDatabaseLogin;
    FIBConnection.LoginPrompt := True;
    FIBConnection.Connected := True;
  end;

  // Transaktionsparameter übernehmen (Transaction wird NICHT automatisch gestartet)
  if Assigned(RegisteredDatabases[dbIndex].IBTransaction) then
    FQueryTrans.Params.Assign(RegisteredDatabases[dbIndex].IBTransaction.Params);

  // Tabellennamen für Syntax‑Highlighting laden
  SynSQLSyn1.TableNames.CommaText := fmMain.GetTableNames(dbIndex);
  for i := 0 to SynSQLSyn1.TableNames.Count - 1 do
    if IsObjectNameCaseSensitive(SynSQLSyn1.TableNames[i]) then
      SynSQLSyn1.TableNames[i] := MakeObjectNameQuoted(SynSQLSyn1.TableNames[i]);
  SynCompletion1.ItemList.AddStrings(SynSQLSyn1.TableNames);
end;

function TfmQueryWindow.GetStatementType(const SQL: string): TIBSQLStatementTypes;
var
  Statements: TStringList;
  TempQuery: TIBQuery;
  TempTrans: TIBTransaction;
  SingleSQL: string;
begin
  Result := SQLUnknown;
  if Trim(SQL) = '' then Exit;

  Statements := SplitSQLStatements(SQL);
  try
    // Mehrere Statements -> als Script behandeln (später über TIBXScript)
    if Statements.Count > 1 then
    begin
      Result := SQLUnknown;   // signalisiert: mehrere Statements
      Exit;
    end;

    SingleSQL := Statements[0];
    if IsOnlyComments(SingleSQL) then
    begin
      Result := SQLUnknown;   // nichts zu tun
      Exit;
    end;

    TempQuery := TIBQuery.Create(nil);
    TempTrans := TIBTransaction.Create(nil);
    try
      TempTrans.DefaultDatabase := FIBConnection;
      TempTrans.StartTransaction;
      TempQuery.Database := FIBConnection;
      TempQuery.Transaction := TempTrans;
      TempQuery.SQL.Text := SingleSQL;
      try
        TempQuery.Prepare;
        Result := TempQuery.StatementType;
        TempQuery.Unprepare;
      except
        // Prepare gescheitert -> einfache SELECT-Erkennung, sonst unbekannt
        if Pos('SELECT', UpperCase(SingleSQL)) = 1 then
          Result := SQLSelect
        else
          Result := SQLUnknown;
      end;
      TempTrans.Commit;
    finally
      TempQuery.Free;
      TempTrans.Free;
    end;
  finally
    Statements.Free;
  end;
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
function TfmQueryWindow.CreateResultTab(IsSelect: Boolean;
  var aSqlQuery: TIBQuery; var meResult: TMemo;
  AdditionalTitle: string = ''): TTabSheet;
var
  ATab: TTabSheet;
  DBGrid: TRxDBGrid;
  DataSource: TDataSource;
  StatusBar: TStatusBar;
  Nav: TDBNavigator;
  Pan: TPanel;
begin
  ATab := TTabSheet.Create(nil);
  OutputTabsList.AddObject('', ATab);
  Result := ATab;
  ATab.Parent := pgOutputPageCtl;
  pgOutputPageCtl.ActivePage := ATab;
  ATab.Caption := 'Result # ' + GetNewTabNum + ' ' + AdditionalTitle;

  if IsSelect then
  begin
    // SELECT: Grid-Ergebnis mit eigener Transaktion
    aSqlQuery.AfterPost := @QueryAfterPost;
    aSqlQuery.AfterScroll := @QueryAfterScroll;
    aSqlQuery.Tag := ATab.TabIndex;
    ATab.Tag := PtrInt(aSQLQuery);

    StatusBar := TStatusBar.Create(ATab);
    StatusBar.Parent := ATab;
    StatusBar.Tag := aSqlQuery.Tag;

    DataSource := TDataSource.Create(self);
    DataSource.DataSet := aSqlQuery;

    Pan := TPanel.Create(self);
    Pan.Parent := ATab;
    Pan.Height := 30;
    Pan.Align := alTop;

    DBGrid := TRxDBGrid.Create(self);
    DBGrid.Parent := ATab;
    DBGrid.DataSource := DataSource;
    DBGrid.Align := alClient;
    DBGrid.OnDblClick := @DBGrid1DblClick;
    DBGrid.OptionsRx := [rdgAllowColumnsForm, rdgAllowDialogFind, rdgHighlightFocusCol,
                         rdgHighlightFocusRow, rdgFooterRows, rdgAllowQuickFilter,
                         rdgAllowFilterForm, rdgAllowSortForm, rdgAllowToolMenu,
                         rdgCaseInsensitiveSort, rdgDisableWordWrapTitles, rdgColSpanning];

    DBGrid.TitleButtons := True;
    DBGrid.AutoSort := True;
    DBGrid.DoubleBuffered := True;
    DBGrid.OnTitleClick := @RxDBGridSortControllerTitleClick;

    pmGrid.PopupComponent := DBGrid;
    DBGrid.Tag := ATab.TabIndex;
    DBGrid.ReadOnly := False;
    DBGrid.AutoEdit := False;
    DBGrid.PopupMenu := pmGrid;
    DBGrid.TitleStyle := tsNative;

    Nav := TDBNavigator.Create(self);
    Nav.Parent := Pan;
    Nav.VisibleButtons := [nbFirst, nbNext, nbPrior, nbLast];
    Nav.DataSource := DataSource;
    Nav.Visible := QWShowNavigator;

    NewApplyButton(Pan, ATab);
    NewCommitButton(Pan, ATab);

    RxDBGridExportPDF1.RxDBGrid := DBGrid;
    RxDBGridPrint1.RxDBGrid := DBGrid;
    RxDBGridExportSpreadSheet1.RxDBGrid := DBGrid;
  end
  else
  begin
    // Execute / Script: Nur Memo
    meResult := TMemo.Create(self);
    meResult.Parent := ATab;
    meResult.ReadOnly := True;
    meResult.Align := alClient;
  end;

  if Assigned(ATab) then
    frmThemeSelector.btnApplyClick(ATab);
end;

procedure TfmQueryWindow.SetTransactionButtonsState(AEnabled: Boolean);
begin
  tbCommit.Enabled          := AEnabled;
  tbCommitRetaining.Enabled := AEnabled;
  tbRollback.Enabled        := AEnabled;
  tbRollbackRetaining.Enabled := AEnabled;
end;

function TfmQueryWindow.IsOnlyComments(const SQL: string): Boolean;
var
  CleanSQL: string;
  i: Integer;
  InString: Boolean;
  InSingleLineComment: Boolean;
  InMultiLineComment: Boolean;
begin
  CleanSQL := '';
  InString := False;
  InSingleLineComment := False;
  InMultiLineComment := False;
  i := 1;

  while i <= Length(SQL) do
  begin
    // String-Literale erkennen
    if (SQL[i] = '''') and not InSingleLineComment and not InMultiLineComment then
      InString := not InString;

    if not InString then
    begin
      // Mehrzeilige Kommentare Anfang
      if (not InSingleLineComment) and (i < Length(SQL)) and (SQL[i] = '/') and (SQL[i+1] = '*') then
      begin
        InMultiLineComment := True;
        Inc(i, 2);
        Continue;
      end;

      // Mehrzeilige Kommentare Ende
      if InMultiLineComment and (i < Length(SQL)) and (SQL[i] = '*') and (SQL[i+1] = '/') then
      begin
        InMultiLineComment := False;
        Inc(i, 2);
        Continue;
      end;

      // Einzeilige Kommentare
      if (not InMultiLineComment) and (i < Length(SQL)) and (SQL[i] = '-') and (SQL[i+1] = '-') then
      begin
        InSingleLineComment := True;
        Inc(i, 2);
        Continue;
      end;

      // Zeilenende für einzeilige Kommentare
      if InSingleLineComment and (SQL[i] in [#10, #13]) then
        InSingleLineComment := False;
    end;

    // Nur Zeichen sammeln die NICHT in Kommentaren sind
    if not InSingleLineComment and not InMultiLineComment then
      CleanSQL := CleanSQL + SQL[i];

    Inc(i);
  end;

  // Prüfen ob nach Entfernen der Kommentare noch etwas übrig ist
  Result := Trim(CleanSQL) = '';
end;

function TfmQueryWindow.SplitSQLStatements(const SQL: string): TStringList;
var
  i: Integer;
  InString: Boolean;
  InSingleLineComment: Boolean;
  InMultiLineComment: Boolean;
  CurrentStatement: string;
begin
  Result := TStringList.Create;
  CurrentStatement := '';
  InString := False;
  InSingleLineComment := False;
  InMultiLineComment := False;
  i := 1;

  while i <= Length(SQL) do
  begin
    // String-Literale
    if (SQL[i] = '''') and not InSingleLineComment and not InMultiLineComment then
    begin
      InString := not InString;
      CurrentStatement := CurrentStatement + SQL[i];
      Inc(i);
      Continue;
    end;

    if not InString then
    begin
      // Mehrzeilige Kommentare Anfang
      if (not InSingleLineComment) and (i < Length(SQL)) and (SQL[i] = '/') and (SQL[i+1] = '*') then
      begin
        InMultiLineComment := True;
        CurrentStatement := CurrentStatement + '/*';
        Inc(i, 2);
        Continue;
      end;

      // Mehrzeilige Kommentare Ende
      if InMultiLineComment and (i < Length(SQL)) and (SQL[i] = '*') and (SQL[i+1] = '/') then
      begin
        InMultiLineComment := False;
        CurrentStatement := CurrentStatement + '*/';
        Inc(i, 2);
        Continue;
      end;

      // Einzeilige Kommentare
      if (not InMultiLineComment) and (i < Length(SQL)) and (SQL[i] = '-') and (SQL[i+1] = '-') then
      begin
        InSingleLineComment := True;
        CurrentStatement := CurrentStatement + '--';
        Inc(i, 2);
        Continue;
      end;

      // Zeilenende für einzeilige Kommentare
      if InSingleLineComment and (SQL[i] in [#10, #13]) then
      begin
        InSingleLineComment := False;
        CurrentStatement := CurrentStatement + SQL[i];
        Inc(i);
        Continue;
      end;
    end;

    // Semikolon als Statement-Trenner (nur wenn nicht in String/Kommentar)
    if (SQL[i] = ';') and not InString and not InSingleLineComment and not InMultiLineComment then
    begin
      // Statement abschließen (ohne das Semikolon)
      if Trim(CurrentStatement) <> '' then
      begin
        Result.Add(Trim(CurrentStatement));
        CurrentStatement := '';
      end;
      Inc(i);
      Continue;
    end;

    // Normales Zeichen
    CurrentStatement := CurrentStatement + SQL[i];
    Inc(i);
  end;

  // Rest (auch wenn kein Semikolon am Ende)
  if Trim(CurrentStatement) <> '' then
    Result.Add(Trim(CurrentStatement));
end;

// Prüft, ob das SQL ein Transaktionskontroll-Statement ist (COMMIT/ROLLBACK)
function TfmQueryWindow.IsTransactionControl(const SQL: string): Boolean;
var
  UpperSQL: string;
begin
  UpperSQL := UpperCase(Trim(SQL));
  Result := (Pos('COMMIT', UpperSQL) = 1) or (Pos('ROLLBACK', UpperSQL) = 1);
end;

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

procedure TfmQueryWindow.QueryAfterPost(DataSet: TDataSet);
begin
  // User has edited cells, so let him save
  EnableApplyButton;
end;


{ Run query, 0 for auto-detect query type }

procedure TfmQueryWindow.bbRunClick(Sender: TObject);
begin
  CallExecuteQuery;
end;

procedure TfmQueryWindow.cxAutoCommitChange(Sender: TObject);
begin
  FIBXScript.AutoDDL := cxAutoCommit.Checked;
  QWAutoCommit       := cxAutoCommit.Checked;
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

procedure TfmQueryWindow.FIBXScriptSelectSQL(Sender: TObject; SQLText: string);
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

{procedure TfmQueryWindow.RemovePreviousResultTabs;
var
  i: Integer;
begin
  for i:= OutputTabsList.Count - 1 downto 0 do
  begin
    OutputTabsList.Objects[i].Free;
    OutputTabsList.Delete(i);
  end;
end;}

{procedure TfmQueryWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  {if FQueryTrans.InTransaction then
  begin
    FQueryTrans.Commit;
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
end;}

procedure TfmQueryWindow.RemovePreviousResultTabs;
var
  i, j: Integer;
  ATab: TTabSheet;
  AQuery: TIBQuery;
  ATrans: TIBTransaction;
  Grid: TRxDBGrid;
begin
  // 1. Alle aktiven Grids von Abhängigkeiten lösen und Queries schließen
  for i := 0 to pgOutputPageCtl.PageCount - 1 do
  begin
    ATab := pgOutputPageCtl.Pages[i];

    // Grid im Tab finden (es gibt nur eins pro Tab)
    Grid := nil;
    for j := 0 to ATab.ControlCount - 1 do
      if ATab.Controls[j] is TRxDBGrid then
      begin
        Grid := TRxDBGrid(ATab.Controls[j]);
        Break;
      end;

    if Assigned(Grid) then
    begin
      // Popup-Menü lösen, damit RemoveTools nicht ins Leere greift
      Grid.PopupMenu := nil;
      // Export-Komponenten lösen
      if RxDBGridExportPDF1.RxDBGrid = Grid then
        RxDBGridExportPDF1.RxDBGrid := nil;
      if RxDBGridPrint1.RxDBGrid = Grid then
        RxDBGridPrint1.RxDBGrid := nil;
      if RxDBGridExportSpreadSheet1.RxDBGrid = Grid then
        RxDBGridExportSpreadSheet1.RxDBGrid := nil;
    end;

    // SELECT-Query sauber schließen, falls vorhanden
    if ATab.Tag <> 0 then
    begin
      AQuery := TIBQuery(ATab.Tag);
      if Assigned(AQuery) then
      begin
        ATrans := AQuery.Transaction;
        if AQuery.Active then
          AQuery.Close;
        if Assigned(ATrans) and ATrans.InTransaction then
          ATrans.Rollback;
      end;
    end;

  end;

  // 2. Tabs selbst freigeben
  for i := OutputTabsList.Count - 1 downto 0 do
  begin
    OutputTabsList.Objects[i].Free;
    OutputTabsList.Delete(i);
  end;
end;

{procedure TfmQueryWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  DlgResult: Integer;
  DBName: string;
begin
  // Datenbankname aus der registrierten Datenbank holen
  DBName := RegisteredDatabases[FDBIndex].IBDatabase.DatabaseName;

  // Nur nachfragen, wenn wirklich eine Transaktion aktiv ist
  if FQueryTrans.InTransaction or FScriptTrans.InTransaction then
  begin
    DlgResult := MessageDlg(
      'There is still an active transaction with uncommitted work' + sLineBreak +
      'in database ''' + DBName + '''.' + sLineBreak + sLineBreak +
      'Do you want to commit the changes before closing?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0
    );

    case DlgResult of
      mrYes:
        begin
          try
            if FQueryTrans.InTransaction then
              FQueryTrans.Commit;
            if FScriptTrans.InTransaction then
              FScriptTrans.Commit;
          except
            on E: Exception do
            begin
              MessageDlg('Commit failed: ' + E.Message, mtError, [mbOK], 0);
              CloseAction := caNone;
              Exit;
            end;
          end;
        end;
      mrNo:
        begin
          try
            if FQueryTrans.InTransaction then
              FQueryTrans.Rollback;
            if FScriptTrans.InTransaction then
              FScriptTrans.Rollback;
          except
            on E: Exception do
            begin
              MessageDlg('Rollback failed: ' + E.Message, mtError, [mbOK], 0);
              CloseAction := caNone;
              Exit;
            end;
          end;
        end;
      mrCancel:
        begin
          CloseAction := caNone;
          Exit;
        end;
    end;
  end;

  // Ergebnis‑Tabs aufräumen
  RemovePreviousResultTabs;
  OutputTabsList.Free;

  if Assigned(FNodeInfos) and Assigned(FNodeInfos^.ViewForm) then
    FNodeInfos^.ViewForm := nil;

  CloseAction := caFree;
end;}

procedure TfmQueryWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  DlgResult: Integer;
  DBName: string;
begin
  // Datenbankname aus der registrierten Datenbank holen
  DBName := RegisteredDatabases[FDBIndex].IBDatabase.DatabaseName;

  if FQueryTrans.InTransaction or FScriptTrans.InTransaction then
  begin
    DlgResult := MessageDlg(
      'There is still an active transaction with uncommitted work' + sLineBreak +
      'in database ''' + DBName + '''.' + sLineBreak + sLineBreak +
      'Do you want to commit the changes before closing?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0
    );

    case DlgResult of
      mrYes:
        begin
          try
            if FQueryTrans.InTransaction then FQueryTrans.Commit;
            if FScriptTrans.InTransaction then FScriptTrans.Commit;
          except
            on E: Exception do
            begin
              MessageDlg('Commit failed: ' + E.Message, mtError, [mbOK], 0);
              CloseAction := caNone;
              Exit;
            end;
          end;
        end;
      mrNo:
        begin
          try
            if FQueryTrans.InTransaction then FQueryTrans.Rollback;
            if FScriptTrans.InTransaction then FScriptTrans.Rollback;
          except
            on E: Exception do
            begin
              MessageDlg('Rollback failed: ' + E.Message, mtError, [mbOK], 0);
              CloseAction := caNone;
              Exit;
            end;
          end;
        end;
      mrCancel:
        begin
          WinCloseCanceled := True;      // globales Flag setzen
          CloseAction := caNone;
          Exit;
        end;
    end;
  end;

  RemovePreviousResultTabs;
  OutputTabsList.Free;

  if Assigned(FNodeInfos) and Assigned(FNodeInfos^.ViewForm) then
    FNodeInfos^.ViewForm := nil;

  if Parent is TTabSheet then
    Parent.Free;

  CloseAction := caFree;
end;

{ Initialize auto-completion text in QueryWindow OnCreate event }

procedure TfmQueryWindow.FormCreate(Sender: TObject);
var
   configFile: TIniFile;
   configFilePath: String;
begin
  FLastStatementWasSelect := false;

  cxAutoCommit.Checked := QWAutoCommit;
  FIBXScript.AutoDDL := cxAutoCommit.Checked;

  SetTransactionButtonsState(false);

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
  SetTransactionButtonsState(False);

  {if AllowIniOverrides then
  begin
    meQuery.Color      :=  QWEditorBackgroundColor;
    meQuery.Font.Name  :=  QWEditorFontName;
    meQuery.Font.Size  :=  QWEditorFontSize;
    meQuery.Font.Color :=  QWEditorFontColor;
    meQuery.Font.Style :=  QWEditorFontStyle;
  end;}
end;

procedure TfmQueryWindow.FQueryTransAfterTransactionEnd(Sender: TObject);
begin
  SetTransactionButtonsState(False);
end;

procedure TfmQueryWindow.FQueryTransStartTransaction(Sender: TObject);
begin
  SetTransactionButtonsState(True);
end;

procedure TfmQueryWindow.FScriptTransAfterTransactionEnd(Sender: TObject);
begin
  SetTransactionButtonsState(False);
end;

procedure TfmQueryWindow.FScriptTransStartTransaction(Sender: TObject);
begin
  SetTransactionButtonsState(True);
end;



{ Close current Query window }
{procedure TfmQueryWindow.lmCloseTabClick(Sender: TObject);
begin
  if (Trim(meQuery.Lines.Text) = '') or
    (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
  begin
    Close;
    Parent.Free;
  end;
end;}

procedure TfmQueryWindow.lmCloseTabClick(Sender: TObject);
begin
    Close;
end;

{ Copy cell in clipboard }
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
  CallExecuteQuery;
end;


{ Run query and force its type as executable statement}

procedure TfmQueryWindow.lmRunExecClick(Sender: TObject);
begin
  CallExecuteQuery;
end;


{ Run query, and force its type as script }

procedure TfmQueryWindow.lmRunScriptClick(Sender: TObject);
begin
  CallExecuteQuery;
end;


{ Run query, force its type as select statement }

procedure TfmQueryWindow.lmRunSelectClick(Sender: TObject);
begin
  CallExecuteQuery;
end;


{ select all in SQL Editor }

procedure TfmQueryWindow.lmSelectAllClick(Sender: TObject);
begin
  meQuery.SelectAll;
end;

procedure TfmQueryWindow.lmSQLParserClick(Sender: TObject);
var fmSQLParser: TfmSQLParser;
begin
  fmSQLParser := TfmSQLParser.create(Self);
  fmSQLParser.FStrings := meQuery.Lines;
  fmSQLParser.OriginalSQL.Lines.Assign(meQuery.Lines);
  fmSQLParser.btnApply.Visible := true;
  fmSQLParser.ShowModal;
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

procedure TfmQueryWindow.meQueryChangeUpdating(ASender: TObject;
  AnUpdating: Boolean);
begin
  if Trim(meQuery.Text) <> '' then
    tbRun.Enabled := True
  else
    tbRun.Enabled := False;
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
    CallExecuteQuery;
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

// Erkennt, ob der SQL‑Text als Skript (mit TIBXScript) ausgeführt werden muss,
// weil er COMMIT, ROLLBACK, SET TERM oder EXECUTE BLOCK enthält.
// Kommentare und Zeichenketten werden dabei ignoriert, um Fehlerkennungen zu vermeiden.
function TfmQueryWindow.IsSQLScript(const SQL: string): Boolean;
var
  CleanSQL: string;
  i: Integer;
  InString, InSingleLine, InMultiLine: Boolean;
  TopLevelSemicolons: Integer;
begin
  CleanSQL := '';
  InString := False;
  InSingleLine := False;
  InMultiLine := False;
  i := 1;

  while i <= Length(SQL) do
  begin
    // Zeichenketten
    if (SQL[i] = '''') and not InSingleLine and not InMultiLine then
    begin
      InString := not InString;
      CleanSQL := CleanSQL + SQL[i];
      Inc(i);
      Continue;
    end;

    if not InString then
    begin
      // Blockkommentar Anfang /*
      if (not InSingleLine) and (i < Length(SQL)) and (SQL[i] = '/') and (SQL[i+1] = '*') then
      begin
        InMultiLine := True;
        Inc(i, 2);
        Continue;
      end;
      // Blockkommentar Ende */
      if InMultiLine and (i < Length(SQL)) and (SQL[i] = '*') and (SQL[i+1] = '/') then
      begin
        InMultiLine := False;
        Inc(i, 2);
        Continue;
      end;
      // Zeilenkommentar --
      if (not InMultiLine) and (i < Length(SQL)) and (SQL[i] = '-') and (SQL[i+1] = '-') then
      begin
        InSingleLine := True;
        Inc(i, 2);
        Continue;
      end;
      // Zeilenende beendet Zeilenkommentar
      if InSingleLine and (SQL[i] in [#10, #13]) then
      begin
        InSingleLine := False;
        CleanSQL := CleanSQL + SQL[i];
        Inc(i);
        Continue;
      end;
    end;

    if not InSingleLine and not InMultiLine then
      CleanSQL := CleanSQL + SQL[i];

    Inc(i);
  end;

  CleanSQL := UpperCase(CleanSQL);

  // Prüfung auf Script-Schlüsselwörter
  Result := (Pos('COMMIT', CleanSQL) > 0) or
            (Pos('ROLLBACK', CleanSQL) > 0) or
            (Pos('SET TERM', CleanSQL) > 0) or
            (Pos('SET AUTODDL', CleanSQL) > 0) or
            (Pos('SET BAIL', CleanSQL) > 0) or
            (Pos('SET ECHO', CleanSQL) > 0) or
            (Pos('SET COUNT', CleanSQL) > 0) or
            (Pos('SET STATS', CleanSQL) > 0) or
            (Pos('SET NAMES', CleanSQL) > 0) or
            (Pos('SET GENERATOR', CleanSQL) > 0) or
            (Pos('SET SQL DIALECT', CleanSQL) > 0) or
            (Pos('EXECUTE BLOCK', CleanSQL) > 0);

  // Wenn bereits ein Schlüsselwort gefunden → sofort True
  if Result then
    Exit;

  // Wenn Schalter aktiv: Prüfung auf mehrere Top-Level-Semicolons
  if QWExecMultiStatementsAsScript then
  begin
    TopLevelSemicolons := 0;
    for i := 1 to Length(CleanSQL) do
      if CleanSQL[i] = ';' then
      begin
        Inc(TopLevelSemicolons);
        if TopLevelSemicolons > 1 then
          Exit(True);
      end;
  end;
end;

{procedure TfmQueryWindow.ExecuteQuery;
var
  StartTime: TDateTime;
  SqlType: string;
  Affected: Integer;
  FSQLQuery: TIBQuery;
  FSQLTrans_Local: TIBTransaction;
  StatementType: TIBSQLStatementTypes;
  Statements: TStringList;
  i: Integer;
begin
  if not FIBConnection.Connected then
    FIBConnection.Connected := true;

  Statements := SplitSQLStatements(FQuery.Text);
  try
    for i := 0 to Statements.Count - 1 do
    begin
      FQueryPart := Statements[i];
      if (FQueryPart = '') or IsOnlyComments(FQueryPart) then
        Continue;

      Inc(FCounter);

      FSQLQuery := TIBQuery.Create(nil);
      FSQLTrans_Local := TIBTransaction.Create(nil);
      try
        FSQLTrans_Local.DefaultDatabase := FIBConnection;
        FSQLTrans_Local.StartTransaction;
        FSQLQuery.Database := FIBConnection;
        FSQLQuery.Transaction := FSQLTrans_Local;
        FSQLQuery.SQL.Text := FQueryPart;
        try
          FSQLQuery.Prepare;
          StatementType := FSQLQuery.StatementType;
          FSQLQuery.Unprepare;
        except
          StatementType := SQLUnknown;
        end;
        FSQLTrans_Local.Commit;
      finally
        FSQLQuery.Free;
        FSQLTrans_Local.Free;
      end;

      if StatementType in [SQLSelect, SQLSelectForUpdate] then
      begin
        FTab := nil;
        try
          FSQLQuery := TIBQuery.Create(Self);
          FSQLTrans_Local := TIBTransaction.Create(Self);
          FSQLTrans_Local.DefaultDatabase := FIBConnection;
          FSQLTrans_Local.Params.Assign(FIBConnection.DefaultTransaction.Params);
          FSQLTrans_Local.StartTransaction;
          FSQLQuery.Database := FIBConnection;
          FSQLQuery.Transaction := FSQLTrans_Local;
          FSQLQuery.AllowAutoActivateTransaction := False;

          if cxAutoCommit.Checked then
            if FQueryTrans.InTransaction then
              FQueryTrans.CommitRetaining;

          FTab := CreateResultTab(True, FSQLQuery, FResultMemo);
          FTab.ImageIndex := 6;
          FTab.Hint := FQueryPart;
          FTab.ShowHint := True;
          FSQLQuery.SQL.Text := FQueryPart;
          FTab.Caption := 'Running...';
          FSQLQuery.Open;
          FTab.Caption := 'Query Result';
          FTab.ImageIndex := 0;
          fmMain.AddToSQLHistory(FRegRec.Title, 'SELECT', meQuery.Text);
        except
          on e: Exception do
          begin
            if Assigned(FSQLTrans_Local) then
            begin
              if FSQLTrans_Local.InTransaction then
                FSQLTrans_Local.Rollback;
              FSQLTrans_Local.Free;
            end;
            if Assigned(FTab) then FTab.TabVisible := False;
            FTab := CreateResultTab(False, FSQLQuery, FResultMemo);
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
        //DML/DDL
        if not IsTransactionControl(FQueryPart) and not FQueryTrans.InTransaction then
          FQueryTrans.StartTransaction;   // <-- nur hier starten, wenn DML/DDL

        FTab := nil;
        FSQLQuery := TIBQuery.Create(Self);
        FSQLQuery.Database := FIBConnection;
        FSQLQuery.Transaction := FQueryTrans;
        FSQLQuery.AllowAutoActivateTransaction := True;
        FTab := CreateResultTab(False, FSQLQuery, FResultMemo);
        FTab.ImageIndex := 1;

        if IsTransactionControl(FQueryPart) and not FQueryTrans.InTransaction then
        begin
          FResultMemo.Visible := True;
          FResultMemo.Clear;
          FResultMemo.Lines.Add('Statement #' + IntToStr(FCounter));
          FResultMemo.Lines.Add('No active transaction to ' +
            UpperCase(Trim(FQueryPart)) + '.');
          FResultMemo.Font.Color := clRed;
          FTab.Font.Color := clRed;
          FTab.ImageIndex := 3;
          Continue;
        end;

        if StatementType = SQLDDL then
          SqlType := 'DDL'
        else
          SqlType := 'DML';

        StartTime := Now;
        Affected := 0;
        try
          FSQLQuery.SQL.Text := FQueryPart;

          if StatementType = SQLDDL then
          begin
            FSQLQuery.ExecSQL;
            if cxAutoCommit.Checked then
              if FQueryTrans.InTransaction then
                FQueryTrans.CommitRetaining;
            FTab.Caption := 'DDL Executed';
            FResultMemo.Visible := True;
            FResultMemo.Clear;
            FResultMemo.Lines.Add('Statement #' + IntToStr(FCounter));
            FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) +
              ' - DDL Executed. Duration: ' +
              FormatDateTime('HH:nn:ss.z', Now - StartTime));
          end
          else
          begin
            FTab.Caption := 'Running...';
            FSQLQuery.ExecSQL;
            if cxAutoCommit.Checked then
              if FQueryTrans.InTransaction then
                FQueryTrans.CommitRetaining;
            Affected := FSQLQuery.RowsAffected;
            FTab.Caption := 'DML Executed';
            FResultMemo.Visible := True;
            FResultMemo.Clear;
            FResultMemo.Lines.Add('Statement #' + IntToStr(FCounter));
            FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) +
              ' - DML Executed. Duration: ' +
              FormatDateTime('HH:nn:ss.z', Now - StartTime));
            FResultMemo.Lines.Add('Rows affected: ' + IntToStr(Affected));
          end;

          Inc(FModifyCount);
          fmMain.AddToSQLHistory(FRegRec.Title, SqlType, meQuery.Text);
          FResultMemo.Lines.Add('----');
          FResultMemo.Lines.Add(FQueryPart);
        except
          on E: Exception do
          begin
            if Assigned(FTab) then FTab.TabVisible := False;
            FTab := CreateResultTab(False, FSQLQuery, FResultMemo);
            pgOutputPageCtl.ActivePage := FTab;
            FResultMemo.Text := E.Message;
            FResultMemo.Lines.Add(FQueryPart);
            FResultMemo.Font.Color := clRed;
            FTab.Font.Color := clRed;
            FTab.ImageIndex := 3;
          end;
        end;
      end;

      if FModifyCount > 50 then
        if MessageDlg('Commit',
            'Too many modifications. Do you want to commit?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          if FQueryTrans.InTransaction then
            FQueryTrans.CommitRetaining;
          FModifyCount := 0;
        end
        else
          FModifyCount := 0;
    end;

  finally
    Statements.Free;
  end;

  FFinished := True;
  FQuery.Clear;
end;}

procedure TfmQueryWindow.ExecuteQuery;
var
  StartTime: TDateTime;
  SqlType: string;
  Affected: Integer;
  FSQLQuery: TIBQuery;
  FSQLTrans_Local: TIBTransaction;
  StatementType: TIBSQLStatementTypes;
  Statements: TStringList;
  i: Integer;
begin
  if not FIBConnection.Connected then
    FIBConnection.Connected := true;

  Statements := SplitSQLStatements(FQuery.Text);
  try
    for i := 0 to Statements.Count - 1 do
    begin
      FQueryPart := Statements[i];
      if (FQueryPart = '') or IsOnlyComments(FQueryPart) then
        Continue;

      Inc(FCounter);

      FSQLQuery := TIBQuery.Create(nil);
      FSQLTrans_Local := TIBTransaction.Create(nil);
      try
        FSQLTrans_Local.DefaultDatabase := FIBConnection;
        FSQLTrans_Local.StartTransaction;
        FSQLQuery.Database := FIBConnection;
        FSQLQuery.Transaction := FSQLTrans_Local;
        FSQLQuery.SQL.Text := FQueryPart;
        try
          FSQLQuery.Prepare;
          StatementType := FSQLQuery.StatementType;
          FSQLQuery.Unprepare;
        except
          StatementType := SQLUnknown;
        end;
        FSQLTrans_Local.Commit;
      finally
        FSQLQuery.Free;
        FSQLTrans_Local.Free;
      end;

      if StatementType in [SQLSelect, SQLSelectForUpdate] then
      begin
        // SELECT – eigene Transaktion, Haupttransaktion nicht berühren
        FTab := nil;
        try
          FSQLQuery := TIBQuery.Create(Self);
          FSQLTrans_Local := TIBTransaction.Create(Self);
          FSQLTrans_Local.DefaultDatabase := FIBConnection;
          FSQLTrans_Local.Params.Assign(FIBConnection.DefaultTransaction.Params);
          FSQLTrans_Local.StartTransaction;
          FSQLQuery.Database := FIBConnection;
          FSQLQuery.Transaction := FSQLTrans_Local;
          FSQLQuery.AllowAutoActivateTransaction := False;

          if cxAutoCommit.Checked then
            if FQueryTrans.InTransaction then
              FQueryTrans.CommitRetaining;

          FTab := CreateResultTab(True, FSQLQuery, FResultMemo);
          FTab.ImageIndex := 6;
          FTab.Hint := FQueryPart;
          FTab.ShowHint := True;
          FSQLQuery.SQL.Text := FQueryPart;
          FTab.Caption := 'Running...';
          FSQLQuery.Open;
          FTab.Caption := 'Query Result';
          FTab.ImageIndex := 0;

          // Nach einem SELECT keine Transaktionsbuttons aktivieren
          SetTransactionButtonsState(False);
          FLastStatementWasSelect := True;

          fmMain.AddToSQLHistory(FRegRec.Title, 'SELECT', meQuery.Text);
        except
          on e: Exception do
          begin
            if Assigned(FSQLTrans_Local) then
            begin
              if FSQLTrans_Local.InTransaction then
                FSQLTrans_Local.Rollback;
              FSQLTrans_Local.Free;
            end;
            if Assigned(FTab) then FTab.TabVisible := False;
            FTab := CreateResultTab(False, FSQLQuery, FResultMemo);
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
        // DML / DDL – Haupttransaktion verwenden
        if not IsTransactionControl(FQueryPart) and not FQueryTrans.InTransaction then
          FQueryTrans.StartTransaction;   // löst FQueryTransStartTransaction → Buttons aktiv

        FTab := nil;
        FSQLQuery := TIBQuery.Create(Self);
        FSQLQuery.Database := FIBConnection;
        FSQLQuery.Transaction := FQueryTrans;
        FSQLQuery.AllowAutoActivateTransaction := True;
        FTab := CreateResultTab(False, FSQLQuery, FResultMemo);
        FTab.ImageIndex := 1;

        if IsTransactionControl(FQueryPart) and not FQueryTrans.InTransaction then
        begin
          FResultMemo.Visible := True;
          FResultMemo.Clear;
          FResultMemo.Lines.Add('Statement #' + IntToStr(FCounter));
          FResultMemo.Lines.Add('No active transaction to ' +
            UpperCase(Trim(FQueryPart)) + '.');
          FResultMemo.Font.Color := clRed;
          FTab.Font.Color := clRed;
          FTab.ImageIndex := 3;
          Continue;
        end;

        if StatementType = SQLDDL then
          SqlType := 'DDL'
        else
          SqlType := 'DML';

        StartTime := Now;
        Affected := 0;
        try
          FSQLQuery.SQL.Text := FQueryPart;

          if StatementType = SQLDDL then
          begin
            FSQLQuery.ExecSQL;
            if cxAutoCommit.Checked then
              if FQueryTrans.InTransaction then
                FQueryTrans.CommitRetaining;
            FTab.Caption := 'DDL Executed';
            FResultMemo.Visible := True;
            FResultMemo.Clear;
            FResultMemo.Lines.Add('Statement #' + IntToStr(FCounter));
            FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) +
              ' - DDL Executed. Duration: ' +
              FormatDateTime('HH:nn:ss.z', Now - StartTime));
          end
          else
          begin
            FTab.Caption := 'Running...';
            FSQLQuery.ExecSQL;
            if cxAutoCommit.Checked then
              if FQueryTrans.InTransaction then
                FQueryTrans.CommitRetaining;
            Affected := FSQLQuery.RowsAffected;
            FTab.Caption := 'DML Executed';
            FResultMemo.Visible := True;
            FResultMemo.Clear;
            FResultMemo.Lines.Add('Statement #' + IntToStr(FCounter));
            FResultMemo.Lines.Add(FormatDateTime('hh:nn:ss.z', Now) +
              ' - DML Executed. Duration: ' +
              FormatDateTime('HH:nn:ss.z', Now - StartTime));
            FResultMemo.Lines.Add('Rows affected: ' + IntToStr(Affected));
          end;

          FLastStatementWasSelect := False;

          Inc(FModifyCount);
          fmMain.AddToSQLHistory(FRegRec.Title, SqlType, meQuery.Text);
          FResultMemo.Lines.Add('----');
          FResultMemo.Lines.Add(FQueryPart);
        except
          on E: Exception do
          begin
            if Assigned(FTab) then FTab.TabVisible := False;
            FTab := CreateResultTab(False, FSQLQuery, FResultMemo);
            pgOutputPageCtl.ActivePage := FTab;
            FResultMemo.Text := E.Message;
            FResultMemo.Lines.Add(FQueryPart);
            FResultMemo.Font.Color := clRed;
            FTab.Font.Color := clRed;
            FTab.ImageIndex := 3;
          end;
        end;
      end;

      if FModifyCount > 50 then
        if MessageDlg('Commit',
            'Too many modifications. Do you want to commit?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          if FQueryTrans.InTransaction then
            FQueryTrans.CommitRetaining;
          FModifyCount := 0;
        end
        else
          FModifyCount := 0;
    end;

  finally
    Statements.Free;
  end;

  FFinished := True;
  FQuery.Clear;
end;

function TfmQueryWindow.ExecuteScript(Script: string): Boolean;
var
  StartTime: TDateTime;
  TempQuery: TIBQuery;
  meResult: TMemo;
  ATab: TTabSheet;
begin
  StartTime := Now;
  ATab := nil;
  Result := False;

  try
    ATab := CreateResultTab(False, TempQuery, meResult);
    try
      ATab.ImageIndex := 2;
      Script := Trim(Script);

      if not FIBConnection.Connected then
        FIBConnection.Connected := true;

      FScriptTrans.DefaultDatabase := FIBConnection;

      if not FScriptTrans.InTransaction then
        FScriptTrans.StartTransaction;

      FIBXScript.Database := FIBConnection;
      FIBXScript.Transaction := FScriptTrans;
      FIBXScript.ExecSQLScript(Script);

      if FScriptTrans.InTransaction then
        if cxAutoCommit.Checked then
          FScriptTrans.CommitRetaining;

      Result := True;
      meResult.Lines.Text := FormatDateTime('hh:nn:ss.z', Now) +
        ' - Script Executed. Duration: ' +
        FormatDateTime('HH:nn:ss.z', Now - StartTime);
      meResult.Lines.Add('--------');
      meResult.Lines.Add(Script);
      meResult.Font.Color := clGreen;
    except
      on E: Exception do
      begin
        FScriptTrans.Rollback;
        if Assigned(ATab) then ATab.TabVisible := False;
        ATab := CreateResultTab(False, TempQuery, meResult);
        pgOutputPageCtl.ActivePage := ATab;
        meResult.Text := E.Message;
        meResult.Lines.Add('--------');
        meResult.Lines.Add(Script);
        meResult.Font.Color := clRed;
        ATab.Font.Color := clRed;
        ATab.ImageIndex := 3;
        Result := False;
      end;
    end;
  finally
    //
  end;
end;

{procedure TfmQueryWindow.CallExecuteQuery;
var EnableTransButtons: boolean;
begin
  if not GetQuery(FQuery) then
  begin
    ShowMessage('Could not get valid query');
    Exit;
  end;

  RemovePreviousResultTabs;

  tbRun.Enabled := False;
  Application.ProcessMessages;

  FModifyCount := 0;
  FCounter := 0;

  if IsSQLScript(FQuery.Text) then
  begin
    if not FScriptTrans.InTransaction then
      FScriptTrans.Params.Assign(FIBConnection.DefaultTransaction.Params);
    ExecuteScript(FQuery.Text);
  end
  else begin
    if not FQueryTrans.InTransaction then
      FQueryTrans.Params.Assign(FIBConnection.DefaultTransaction.Params);
    ExecuteQuery;
  end;

  EnableTransButtons := (not FLastStatementWasSelect) and
                        (not cxAutoCommit.Checked) and
                        (FScriptTrans.Active or FQueryTrans.Active);
  SetTransactionButtonsState(EnableTransButtons);

  tbRun.Enabled := True;
  Application.ProcessMessages;
end;}

procedure TfmQueryWindow.CallExecuteQuery;
var EnableTransButtons: Boolean;
begin
  if not GetQuery(FQuery) then
  begin
    ShowMessage('Could not get valid query');
    Exit;
  end;

  RemovePreviousResultTabs;

  FModifyCount := 0;
  FCounter := 0;
  FLastStatementWasSelect := False;  // <-- Reset, damit kein alter Wert stört

  // ── Skript ──────────────────────────────────────────────────
  if IsSQLScript(FQuery.Text) then
  begin
    if not FScriptTrans.InTransaction then
      FScriptTrans.Params.Assign(FIBConnection.DefaultTransaction.Params);
    ExecuteScript(FQuery.Text);

    // Buttons aktiv, wenn IRGENDEINE Transaktion offen ist und kein AutoCommit
    EnableTransButtons := (FScriptTrans.InTransaction or FQueryTrans.Active) and
                          (not cxAutoCommit.Checked);

    if cxAutoCommit.Checked then
      FScriptTrans.Commit;
  end
  // ── Einzel‑Statement ────────────────────────────────────────
  else
  begin
    if not FQueryTrans.InTransaction then
      FQueryTrans.Params.Assign(FIBConnection.DefaultTransaction.Params);
    ExecuteQuery;

    EnableTransButtons := FQueryTrans.Active and
                          (not cxAutoCommit.Checked) and
                          (not FLastStatementWasSelect);

    if cxAutoCommit.Checked and (not FLastStatementWasSelect) then
      FQueryTrans.Commit;
  end;

  SetTransactionButtonsState(EnableTransButtons);
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


initialization
  {$I querywindow.lrs}

end.

