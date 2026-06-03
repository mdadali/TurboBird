//Version: 31Jan2005

unit u_consoleide;

{$MODE Delphi}
//{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF Windows} Windows, {$ENDIF} LCLType, LCLIntf, LMessages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls, StdCtrls,
  ComCtrls, ActnList, ValEdit, SynEdit, SynEditTypes, SynHighlighterPas,
  SynEditSearch, SynEditMiscClasses, SynEditHighlighter, SynGutterBase,
  SynGutterMarks, SynGutterLineNumber, SynGutterChanges, SynGutter,
  SynGutterCodeFolding, SynEditMarkupSpecialLine, SynEditRegexSearch,
  SynEditMarks, PrintersDlgs,

  uPSComponent_COM,
  uPSComponent_StdCtrls,
  uPSComponent_Forms,
  uPSComponent_Default,
  uPSComponent_Controls,
  uPSRuntime,
  uPSDisassembly,
  uPSUtils,
  uPSComponent,
  uPSDebugger,
  uPSComponent_DB,
  uPSCompiler,
  uPSR_std,
  uPSC_std,
  uPSR_classes,
  uPSC_classes,
  uPSC_controls,
  uPSR_controls,
  uPSC_forms,
  uPSR_forms,
  uPSR_dateutils,
  uPSC_dateutils,
  uPSR_dll,
  uPSC_dll,

  MainScriptinterface;

type

  { TfrmConsoleIDE }

  TfrmConsoleIDE = class(TForm)
    acFileNew: TAction;
    acFileOpen: TAction;
    acFileSave: TAction;
    acFileSaveAs: TAction;
    acFileExit: TAction;
    acFilePrint: TAction;
    acFileRecent: TAction;
    acEditUndo: TAction;
    acEditRedo: TAction;
    acEditCopy: TAction;
    acEditCut: TAction;
    acEditPaste: TAction;
    acDebugSyntaxCheck: TAction;
    acDebugDecompile: TAction;
    acDebugStepOver: TAction;
    acDebugStepInto: TAction;
    acDebugPause: TAction;
    acDebugReset: TAction;
    acDebugRun: TAction;
    acDebugBreakPoint: TAction;
    ActionList1: TActionList;
    btnAddWatch: TButton;
    btnDeleteWatch: TButton;
    ce: TPSScriptDebugger;
    edtFormName: TEdit;
    IFPS3DllPlugin1: TPSDllPlugin;
    ImageListClassic: TImageList;
    messages: TListBox;
    pnlAddDeleteWatch: TPanel;
    pgcDebug: TPageControl;
    pnlDebug: TPanel;
    pnlEdit: TPanel;
    pnlFile: TPanel;
    pnlTools: TPanel;
    pashighlighter: TSynPasSyn;
    PopupMenu1: TPopupMenu;
    BreakPointMenu: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    PrintDialog1: TPrintDialog;
    PSCustomPlugin1: TPSCustomPlugin;
    PSImport_Controls1: TPSImport_Controls;
    PSImport_DB1: TPSImport_DB;
    Run1: TMenuItem;
    Splitter2: TSplitter;
    StepOver1: TMenuItem;
    StepInto1: TMenuItem;
    N1: TMenuItem;
    Reset1: TMenuItem;
    N2: TMenuItem;
    Run2: TMenuItem;
    Exit1: TMenuItem;
    Splitter1: TSplitter;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    N3: TMenuItem;
    N4: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    StatusBar: TStatusBar;
    Decompile1: TMenuItem;
    N5: TMenuItem;
    IFPS3CE_Controls1: TPSImport_Controls;
    IFPS3CE_DateUtils1: TPSImport_DateUtils;
    IFPS3CE_Std1: TPSImport_Classes;
    IFPS3CE_Forms1: TPSImport_Forms;
    IFPS3CE_StdCtrls1: TPSImport_StdCtrls;
    IFPS3CE_ComObj1: TPSImport_ComObj;
    Pause1: TMenuItem;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    Searchagain1: TMenuItem;
    N6: TMenuItem;
    Gotolinenumber1: TMenuItem;
    ed: TSynEdit;
    SynGutterChanges1: TSynGutterChanges;
    SynGutterCodeFolding1: TSynGutterCodeFolding;
    SynGutterLineNumber1: TSynGutterLineNumber;
    SynGutterMarks1: TSynGutterMarks;
    SynGutterSeparator1: TSynGutterSeparator;
    SynLeftGutterPartList1: TSynGutterPartList;
    Syntaxcheck1: TMenuItem;
    tsMessages: TTabSheet;
    tsWatches: TTabSheet;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    vleWatches: TValueListEditor;

    procedure acDebugBreakPointExecute(Sender: TObject);
    procedure acDebugDecompileExecute(Sender: TObject);
    procedure acDebugPauseExecute(Sender: TObject);
    procedure acDebugResetExecute(Sender: TObject);
    procedure acDebugRunExecute(Sender: TObject);
    procedure acDebugStepIntoExecute(Sender: TObject);
    procedure acDebugStepOverExecute(Sender: TObject);
    procedure acDebugSyntaxCheckExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditRedoExecute(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFilePrintExecute(Sender: TObject);
    procedure acFileRecentExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure btnAddWatchClick(Sender: TObject);
    procedure btnDeleteWatchClick(Sender: TObject);
    procedure ceCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure ceExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    function ceFindUnknownFile(Sender: TObject; const OrginFileName: tbtstring;
      var FileName, Output: tbtstring): Boolean;
    procedure ceLine(Sender: TObject);
    procedure edChange(Sender: TObject);
    procedure edGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure edSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure BreakPointMenuClick(Sender: TObject);
    procedure ceLineInfo(Sender: TObject; const FileName: String; APosition, Row, Col: Cardinal);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ceIdle(Sender: TObject);
    procedure ceExecute(Sender: TPSScript);
    procedure ceAfterExecute(Sender: TPSScript);
    procedure ceCompile(Sender: TPSScript);
    procedure edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure Decompile1Click(Sender: TObject);
    function  ceNeedFile(Sender: TObject; const OrginFileName: String; var FileName, Output: String): Boolean;
    procedure ceBreakpoint(Sender: TObject; const FileName: String; APosition, Row, Col: Cardinal);
    procedure FormDestroy(Sender: TObject);
    procedure messagesDblClick(Sender: TObject);
    procedure Gotolinenumber1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Searchagain1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure StepInto1Click(Sender: TObject);
    procedure StepOver1Click(Sender: TObject);
    procedure Syntaxcheck1Click(Sender: TObject);
    procedure edDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
  private
    FWatchList: TStringList;

    FSearchFromCaret: boolean;
    FActiveLine: Longint;
    FResume: Boolean;
    FActiveFile: string;

    procedure ReplaceCommentsKeepLayout(const SourceLines: TStrings; DestLines: TStringList);

    procedure JumpToError(ALine, ACol: Integer);
    function Compile: Boolean;
    function Execute: Boolean;

    procedure Writeln(const s: string);
    procedure Readln(var s: string);
    procedure SetActiveFile(const Value: string);

    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);

    property aFile: string read FActiveFile write SetActiveFile;
  public
    FErrorLine: Integer;
    procedure edMyPaint(Sender: TObject; ACanvas: TCanvas);
    function SaveCheck: Boolean;

    procedure AssignOnClick(Sender: TButton; const ScriptProcName: string);

    procedure AddWatch(const Expr: string);
    procedure UpdateWatches;
    function  HasExecuted: Boolean;
  end;


//var
  //ConsoleIDE: TfrmConsoleIDE;

implementation

uses
  //fMain,
  ide_debugoutput,
  uFrmGotoLine;
  //dlgSearchText,
  //dlgReplaceText, dlgConfirmReplace;

{$R *.lfm}

const
  isRunningOrPaused = [isRunning, isPaused];

  //added
  debugger_no_source_line = 24;
  debugger_source_line = 25;
  debugger_current_line = 26;
  debugger_current_line_breakpoint = 27;
  debugger_current_line_disabled_breakpoint = 28;
  debugger_InactiveBreakPoint = 29;
  debugger_InvalidBreakPoint = 30;
  debugger_InvalidDisabledBreakPoint = 31;
  degugger_UnknowBreakpoint = 32;


// options - to be saved to the registry
var
  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;
  gbSearchRegex: boolean;
  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

resourcestring
  STR_TEXT_NOTFOUND = 'Text not found';
  STR_UNNAMED = 'Unnamed';
  STR_SUCCESSFULLY_COMPILED = 'Successfully compiled';
  STR_SUCCESSFULLY_EXECUTED = 'Successfully executed';
  STR_RUNTIME_ERROR='[Runtime error] %s(%d:%d), bytecode(%d:%d): %s'; //Birb
  STR_FORM_TITLE = 'LazCAD IDE ';
  STR_FORM_TITLE_RUNNING = 'LazCAD IDE  - Running';
  STR_INPUTBOX_TITLE = 'Script';
  STR_DEFAULT_PROGRAM = 'Program test;'#13#10'begin'#13#10'end.';
  STR_NOTSAVED = 'File has not been saved, save now?';


function TfrmConsoleIDE.HasExecuted: Boolean;
begin
  Result :=
      (ce <> nil) and
      (ce.Exec <> nil) and
      (ce.Exec.Status in isRunningOrPaused);
end;

procedure TfrmConsoleIDE.AddWatch(const Expr: string);
begin
  if Trim(Expr) = '' then Exit;

  if FWatchList.IndexOf(Expr) = -1 then
    FWatchList.Add(Expr);

  UpdateWatches;
end;

procedure TfrmConsoleIDE.UpdateWatches;
var
  i: Integer;
  Expr, Value: string;
begin
  vleWatches.Strings.BeginUpdate;
  try
    vleWatches.Strings.Clear;

    if ce.Exec = nil then Exit;

    for i := 0 to FWatchList.Count - 1 do
    begin
      Expr := FWatchList[i];

      try
        if not HasExecuted then
        begin
          vleWatches.Values[Expr] := '<not available>';
          Continue;
        end;

        if ce.Exec.CurrentProcVars = nil then
          Continue;

        if ce.Exec.CurrentProcParams = nil then
          Continue;

        if ce.Exec.GlobalVarNames = nil then
          Continue;

        Value := ce.GetVarContents(Expr);
      except
        Value := '<error>';
      end;

      vleWatches.Values[Expr] := Value;
    end;

  finally
    vleWatches.Strings.EndUpdate;
  end;
end;

procedure TfrmConsoleIDE.btnAddWatchClick(Sender: TObject);
var
  s: string;
begin
  s := InputBox('Add Watch', 'Expression (e.g. i or Form1.Caption):', '');

  s := Trim(s);
  if s = '' then Exit;

  AddWatch(s);
end;

procedure TfrmConsoleIDE.btnDeleteWatchClick(Sender: TObject);
var
  Key: string;
  idx: Integer;
begin
  if vleWatches.Row < 1 then Exit; // 0 = Header

  Key := vleWatches.Keys[vleWatches.Row];

  idx := FWatchList.IndexOf(Key);
  if idx <> -1 then
    FWatchList.Delete(idx);

  UpdateWatches;
end;

procedure TfrmConsoleIDE.AssignOnClick(Sender: TButton; const ScriptProcName: string);
var
  Met: TMethod;
begin
  Met := ce.GetProcMethod(ScriptProcName);
  TButton(Sender).OnClick := TNotifyEvent(Met);
end;

procedure TfrmConsoleIDE.edMyPaint(Sender: TObject; ACanvas: TCanvas);
var
  Line, ImgIndex, Y: Integer;
  HasCode, HasBreakpoint, BreakpointUnknown: Boolean;
begin
  // Verwende ACanvas anstelle von ed.Canvas
  for Line := 1 to ed.Lines.Count  do
  begin
    // Prüfen, ob die Zeile Code enthält oder einen Breakpoint hat
    HasCode := ce.Exec.HasCode(ce.MainFileName, Line);
    //{$IFDEF Linux} HasCode := ce.Exec.HasCode(ce.MainFileName, Line); {$ENDIF}

    HasBreakpoint := ce.HasBreakPoint(ce.MainFileName, Line);

    // Überprüfen, ob der Breakpoint vor dem Start der Anwendung gesetzt wurde und noch keine Debug-Infos existieren
    BreakpointUnknown := (HasBreakpoint and not (ce.Exec.Status = isRunning));  // Wenn Breakpoint gesetzt, aber das Skript noch nicht läuft

    // Bestimme den Bildindex basierend auf dem Zustand der Zeile
    if Line = FActiveLine then
    begin
      if HasBreakpoint then
        ImgIndex := debugger_current_line_breakpoint   // Breakpoint und aktuelle Zeile
      else
        ImgIndex := debugger_current_line;  // Nur aktuelle Zeile
    end
    else if HasCode then
    begin
      if HasBreakpoint then
        ImgIndex := debugger_source_line  // Gültiger Breakpoint
      else
        ImgIndex := debugger_no_source_line;  // Zeile enthält Code
    end
    else
    begin
      if BreakpointUnknown then
        ImgIndex := degugger_UnknowBreakpoint  // Breakpoint ohne Debug-Infos (vor Start der Anwendung)
      else if HasBreakpoint then
        ImgIndex := debugger_InvalidBreakPoint  // Ungültiger Breakpoint
      else
        ImgIndex := -1; // Keine Markierung
    end;

    // Wenn ein Bild zu zeichnen ist, berechne Y-Position und zeichne es im Gutter
    if (ImgIndex >= 0) and Assigned(ImageListClassic) then
    begin
      Y := (Line - ed.TopLine) * ed.LineHeight; // Y-Position berechnen
      ImageListClassic.Draw(ACanvas, 4, Y, ImgIndex); // Zeichne Bild in Gutter-Bereich
    end;
  end;
end;

procedure TfrmConsoleIDE.FormCreate(Sender: TObject);
begin
  FWatchList := TStringList.Create;
  FErrorLine := -1;
  ed.OnPaint := edMyPaint;
end;

procedure TfrmConsoleIDE.DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
var Options: TSynSearchOptions;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  {if gbSearchRegex then
    ed.SearchEngine := SynEditRegexSearch
  else
    ed.SearchEngine := SynEditSearch;
    }
  if ed.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    //MessageBeep(MB_ICONASTERISK);
    Statusbar.SimpleText := STR_TEXT_NOTFOUND;
    if ssoBackwards in Options then
      ed.BlockEnd := ed.BlockBegin
    else
      ed.BlockBegin := ed.BlockEnd;
    ed.CaretXY := ed.BlockBegin;
  end;

  //if ConfirmReplaceDialog <> nil then
    //ConfirmReplaceDialog.Free;
end;

procedure TfrmConsoleIDE.ShowSearchReplaceDialog(AReplace: boolean);
//var
  //dlg: TTextSearchDialog;
begin
  {Statusbar.SimpleText := '';
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then begin
      // if something is selected search for that text
      if ed.SelAvail and (ed.BlockBegin.Line = ed.BlockEnd.Line) //Birb (fix at SynEdit's SearchReplaceDemo)
      then
        SearchText := ed.SelText
      else
        SearchText := ed.GetWordAtRowCol(ed.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then begin
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gbSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do begin
        gsReplaceText := ReplaceText;
        gsReplaceTextHistory := ReplaceTextHistory;
      end;
      fSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        fSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;}
end;

procedure TfrmConsoleIDE.edSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  //1. ERROR LINE (höchste Priorität)
  if Line = FErrorLine then
  begin
    Special := True;
    BG := $00C0C0FF; // etwas dunkleres Rot
    FG := clBlack;
    Exit;
  end;

  //2. BREAKPOINTS
  if ce.HasBreakPoint(ce.MainFileName, Line) then
  begin
    Special := True;

    if Line = FActiveLine then
    begin
      BG := clSilver;
      FG := clRed;
    end
    else
    begin
      FG := clWhite;
      BG := clRed;
    end;

    Exit;
  end;

  //3. ACTIVE LINE
  if Line = FActiveLine then
  begin
    Special := True;
    FG := clWhite;
    BG := clBlue;
    Exit;
  end;

  // DEFAULT
  Special := False;
end;

procedure TfrmConsoleIDE.ceLineInfo(Sender: TObject; const FileName: String; APosition, Row,
  Col: Cardinal);
begin
  if ce.Exec.DebugMode <> dmRun then
  begin
    FActiveLine := Row;  //orig
    if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
    begin
      Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
    end;
    ed.CaretY := FActiveLine;
    ed.CaretX := 1;

    ed.Refresh;
  end
  else
    Application.ProcessMessages;
end;

procedure TfrmConsoleIDE.BreakPointMenuClick(Sender: TObject);
begin

end;

procedure TfrmConsoleIDE.acFileNewExecute(Sender: TObject);
begin
  if SaveCheck then //check if script changed and not yet saved
  begin
    ed.ClearAll;
    ed.Lines.Text := STR_DEFAULT_PROGRAM;
    ed.Modified := False;
    aFile := '';
  end;
end;

procedure TfrmConsoleIDE.acFileExitExecute(Sender: TObject);
begin
  close;
end;

procedure TfrmConsoleIDE.acEditUndoExecute(Sender: TObject);
begin
  ed.Undo;
end;

procedure TfrmConsoleIDE.acEditRedoExecute(Sender: TObject);
begin
  ed.Redo;
end;

procedure TfrmConsoleIDE.acEditCopyExecute(Sender: TObject);
begin
  ed.CopyToClipboard;
end;

procedure TfrmConsoleIDE.acDebugSyntaxCheckExecute(Sender: TObject);
begin
  if Compile then
    UpdateWatches;

  acDebugSyntaxCheck.Checked := false;
end;

procedure TfrmConsoleIDE.acDebugDecompileExecute(Sender: TObject);
var s: string;
    List: TStringList;
begin
  if Compile then
  begin
    ce.GetCompiled(s);
    IFPS3DataToText(s, s);
    //debugoutput.output.Lines.Text := s;
    //debugoutput.visible := true;
    Messages.Items.Add(S);

    List := TStringList.Create;
    List.Text := S;
    //List.SaveToFile('/home/maurog/decompiled.txt');
    List.Free;
  end;
  acDebugDecompile.Checked := false;
end;

procedure TfrmConsoleIDE.acDebugBreakPointExecute(Sender: TObject);
var Line: Longint;
begin
  Line := Ed.CaretY;
  if ce.HasBreakPoint(ce.MainFileName, Line) then
    ce.ClearBreakPoint(ce.MainFileName, Line)
  else
    ce.SetBreakPoint(ce.MainFileName, Line);
  ed.Refresh;
end;

procedure TfrmConsoleIDE.acDebugPauseExecute(Sender: TObject);
begin
  if not acDebugPause.Checked then
  begin
    acDebugRun.Execute;
    acDebugRun.Checked := true;
  end;
  if ce.Exec.Status = isRunning then
  begin
    ce.Pause;
    //if acDebugRun.Checked then
      //acDebugRun.Checked := false;
    //ce.StepInto;
    exit;
  end;

  if ce.Exec.Status = isPaused then
  begin
    ce.Resume;
    exit;
  end;
end;

procedure TfrmConsoleIDE.acDebugResetExecute(Sender: TObject);
begin
  if ce.Exec.Status in isRunningOrPaused then
    ce.Stop;
  if acDebugPause.Checked then
    acDebugPause.Checked := false;
end;

{procedure TfrmConsoleIDE.acDebugRunExecute(Sender: TObject);
begin
  if acDebugPause.Checked then
    acDebugPause.Checked := false;

  if CE.Running then
  begin
    FResume := True
  end else
  begin
    if Compile then
      Execute;
  end;
  acDebugRun.Checked := false;
end;}

procedure TfrmConsoleIDE.acDebugRunExecute(Sender: TObject);
begin
  if acDebugPause.Checked then
    acDebugPause.Checked := false;

  if CE.Running then
  begin
    FResume := True;
    //UpdateWatches;
  end else
  begin
    if Compile then
    begin
      Execute;
      //UpdateWatches;
    end;
  end;
  acDebugRun.Checked := false;
end;

procedure TfrmConsoleIDE.acDebugStepIntoExecute(Sender: TObject);
begin
  if ce.Exec.Status in isRunningOrPaused then
  begin
    ce.StepInto;
    //UpdateWatches;
  end
  else
  begin
    if Compile then
    begin
      ce.StepInto;
      Execute;
      //UpdateWatches;
    end;
  end;
end;

procedure TfrmConsoleIDE.acDebugStepOverExecute(Sender: TObject);
begin
  if ce.Exec.Status in isRunningOrPaused then
  begin
    ce.StepOver;
  end
  else
  begin
    if Compile then
    begin
      ce.StepInto;
      Execute;
    end;
  end;
end;


procedure TfrmConsoleIDE.acEditCutExecute(Sender: TObject);
begin
  ed.CutToClipboard;
end;

procedure TfrmConsoleIDE.acEditPasteExecute(Sender: TObject);
begin
  ed.PasteFromClipboard(false);
end;

procedure TfrmConsoleIDE.acFileOpenExecute(Sender: TObject);
begin
  if SaveCheck then //check if script changed and not yet saved
  begin
    if OpenDialog1.Execute then
    begin
      FErrorLine := -1;
      ed.ClearAll;
      ed.Lines.LoadFromFile(OpenDialog1.FileName);
      ed.Modified := False;
      aFile := OpenDialog1.FileName;
    end;
  end;
  acFileOpen.Checked := false;
end;

procedure TfrmConsoleIDE.acFilePrintExecute(Sender: TObject);
begin
  //
  acFilePrint.Checked := false;
end;

procedure TfrmConsoleIDE.acFileRecentExecute(Sender: TObject);
begin
  //
  acFileRecent.Checked := false;
end;

procedure TfrmConsoleIDE.acFileSaveExecute(Sender: TObject);
begin
  if aFile <> '' then
  begin
    ed.Lines.SaveToFile(aFile);
    ed.Modified := False;
  end else
    acFileSaveAsExecute(nil);
  acFileSave.Checked := false;
end;

procedure TfrmConsoleIDE.acFileSaveAsExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    aFile := SaveDialog1.FileName;
    ed.Lines.SaveToFile(aFile);
    ed.Modified := False;
  end;
  acFileSaveAs.Checked := false;
end;

procedure TfrmConsoleIDE.ceCompile(Sender: TPSScript);
begin
  Sender.AddMethod(Self, @TfrmConsoleIDE.Writeln, 'procedure writeln(s: string)');
  Sender.AddMethod(Self, @TfrmConsoleIDE.Readln, 'procedure readln(var s: string)');
  Sender.AddRegisteredVariable('Self', 'TForm');
  Sender.AddRegisteredVariable('Application', 'TApplication');
end;

procedure TfrmConsoleIDE.ceCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_MainScriptInterface(x);
end;

procedure TfrmConsoleIDE.ceExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_MainScriptInterface_Routines(se, x);
end;

procedure TfrmConsoleIDE.ceLine(Sender: TObject);
begin
  UpdateWatches;
end;

procedure TfrmConsoleIDE.edChange(Sender: TObject);
begin
  FErrorLine := -1;
end;

procedure TfrmConsoleIDE.edGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
begin
  ed.CaretXY := Point(1, Line);
  acDebugBreakPointExecute(nil);
end;

procedure TfrmConsoleIDE.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 acDebugResetExecute(nil); //terminate any running script
 if SaveCheck then //check if script changed and not yet saved
 begin
   CloseAction := caHide;
   //frmMain.acToolsScripter.Checked := false;
 end
 else
   CloseAction := caNone;
end;

procedure TfrmConsoleIDE.ReplaceCommentsKeepLayout(const SourceLines: TStrings; DestLines: TStringList);
var
  i, j: Integer;
  line, tempLine: string;
  inBlockComment: Boolean;
  blockType: (btNone, btBrace, btParen);
begin
  inBlockComment := False;
  blockType := btNone;

  for i := 0 to SourceLines.Count - 1 do
  begin
    line := SourceLines[i];
    tempLine := '';
    j := 1;

    //DIRECTIVE SAFETY CHECK (WICHTIG!)
    if (Length(line) > 0) and (line[1] = '{') and (Length(line) > 1) and (line[2] = '$') then
    begin
      // Compiler-Direktive 1:1 durchlassen
      DestLines.Add(line);
      Continue;
    end;

    while j <= Length(line) do
    begin

      // =========================
      // STRING HANDLING
      // =========================
      if not inBlockComment and (line[j] = '''') then
      begin
        tempLine := tempLine + line[j];
        Inc(j);

        while j <= Length(line) do
        begin
          tempLine := tempLine + line[j];

          if (line[j] = '''') then
          begin
            if (j < Length(line)) and (line[j + 1] = '''') then
            begin
              Inc(j);
              tempLine := tempLine + line[j];
            end
            else
              Break;
          end;

          Inc(j);
        end;

        Inc(j);
        Continue;
      end;

      // =========================
      // BLOCK COMMENT ACTIVE
      // =========================
      if inBlockComment then
      begin
        if (blockType = btBrace) and (line[j] = '}') then
        begin
          inBlockComment := False;
          blockType := btNone;
          tempLine := tempLine + ' ';
          Inc(j);
          Continue;
        end;

        if (blockType = btParen) and
           (j < Length(line)) and (line[j] = '*') and (line[j + 1] = ')') then
        begin
          inBlockComment := False;
          blockType := btNone;
          tempLine := tempLine + '  ';
          Inc(j, 2);
          Continue;
        end;

        tempLine := tempLine + ' ';
        Inc(j);
        Continue;
      end;

      // =========================
      // LINE COMMENT //
      // =========================
      if (line[j] = '/') and (j < Length(line)) and (line[j + 1] = '/') then
      begin
        while j <= Length(line) do
        begin
          tempLine := tempLine + ' ';
          Inc(j);
        end;
        Break;
      end;

      // =========================
      // BLOCK COMMENT START
      // =========================
      if line[j] = '{' then
      begin
        inBlockComment := True;
        blockType := btBrace;
        tempLine := tempLine + ' ';
        Inc(j);
        Continue;
      end;

      if (line[j] = '(') and (j < Length(line)) and (line[j + 1] = '*') then
      begin
        inBlockComment := True;
        blockType := btParen;
        tempLine := tempLine + '  ';
        Inc(j, 2);
        Continue;
      end;

      // =========================
      // NORMAL CHAR
      // =========================
      tempLine := tempLine + line[j];
      Inc(j);
    end;

    // Layout stabil halten
    while Length(tempLine) < Length(line) do
      tempLine := tempLine + ' ';

    DestLines.Add(tempLine);
  end;
end;

{function TfrmConsoleIDE.Compile: Boolean;
var
  tempLines: TStringList;
  i: Integer;
  TmpString: string;
begin
  tempLines := TStringList.Create;
  try
    ReplaceCommentsKeepLayout(ed.Lines, tempLines);
    ce.Script.Assign(tempLines);

    Result := ce.Compile;
    messages.Clear;
    for i := 0 to ce.CompilerMessageCount - 1 do
    begin
      Messages.Items.Add(ce.CompilerMessages[i].MessageToString);
    end;
    if Result then
      Messages.Items.Add(STR_SUCCESSFULLY_COMPILED);
  finally
    tempLines.Free;
  end;
end;}

function TfrmConsoleIDE.Compile: Boolean;
var
  tempLines: TStringList;
  i: Integer;
  Msg: TPSPascalCompilerMessage;
begin
  tempLines := TStringList.Create;
  try
    ReplaceCommentsKeepLayout(ed.Lines, tempLines);
    ce.Script.Assign(tempLines);

    Result := ce.Compile;
    Messages.Clear;

    for i := 0 to ce.CompilerMessageCount - 1 do
    begin
      Msg := ce.CompilerMessages[i];

      Messages.Items.Add(Msg.MessageToString);

      // 👉 ERSTER FEHLER → direkt hinspringen
      if (not Result) and (i = 0) then
      begin
        JumpToError(Msg.Row, Msg.Col);
      end;
    end;

    if Result then
      Messages.Items.Add(STR_SUCCESSFULLY_COMPILED);

  finally
    tempLines.Free;
  end;
end;

procedure TfrmConsoleIDE.JumpToError(ALine, ACol: Integer);
begin
  if ALine < 1 then ALine := 1;
  if ACol < 1 then ACol := 1;

  ed.CaretY := ALine;
  ed.CaretX := ACol;

  // 👉 Fehlerzeile setzen
  FErrorLine := ALine;

  ed.Invalidate; // neu zeichnen
  ed.SetFocus;
end;

procedure TfrmConsoleIDE.ceIdle(Sender: TObject);
begin
  Application.ProcessMessages; //Birb: don't use Application.HandleMessage here, else GUI will be unrensponsive if you have a tight loop and won't be able to use Run/Reset menu action
  if FResume then
  begin
    FResume := False;
    ce.Resume;
    FActiveLine := 0;
    ed.Refresh;
  end;
end;

procedure TfrmConsoleIDE.ceExecute(Sender: TPSScript);
begin
  if (Self.Parent is TTabSheet) then
    ce.SetVarToInstance('SELF', TForm(Self.Parent.Parent.Parent.Parent))
  else
    ce.SetVarToInstance('SELF', Self);

  ce.SetVarToInstance('APPLICATION', Application);
  Caption := STR_FORM_TITLE_RUNNING;
end;

procedure TfrmConsoleIDE.ceAfterExecute(Sender: TPSScript);
begin
  Caption := STR_FORM_TITLE;
  FActiveLine := 0;
  ed.Refresh;
end;

function TfrmConsoleIDE.Execute: Boolean;
begin
  //debugoutput.Output.Clear;
  if CE.Execute then
  begin
    Messages.Items.Add(STR_SUCCESSFULLY_EXECUTED);
    Result := True; 
  end else
  begin
    messages.Items.Add(Format(STR_RUNTIME_ERROR, [extractFileName(aFile), ce.ExecErrorRow,ce.ExecErrorCol,ce.ExecErrorProcNo,ce.ExecErrorByteCodePosition,ce.ExecErrorToString])); //Birb
    Result := False;
  end;
end;

procedure TfrmConsoleIDE.Writeln(const s: string);
begin
  //debugoutput.output.Lines.Add(S);
  //debugoutput.Visible := True;
  Messages.Items.Add(S);
end;

procedure TfrmConsoleIDE.Readln(var s: string);
begin
  s := InputBox(STR_INPUTBOX_TITLE, '', '');
end;

//check if script changed and not yet saved//
function TfrmConsoleIDE.SaveCheck: Boolean;
begin
  if ed.Modified then
  begin
    case MessageDlg(STR_NOTSAVED, mtConfirmation, mbYesNoCancel, 0) of
      idYes:
        begin
          acFileSaveAsExecute(nil);
          Result := aFile <> '';
        end;
      IDNO: Result := True;
      else
        Result := False;
    end;
  end else Result := True;
end;

procedure TfrmConsoleIDE.edStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  StatusBar.Panels[0].Text := IntToStr(ed.CaretY)+':'+IntToStr(ed.CaretX)
end;

procedure TfrmConsoleIDE.Decompile1Click(Sender: TObject);
begin

end;


function TfrmConsoleIDE.ceNeedFile(Sender: TObject; const OrginFileName: String;
  var FileName, Output: String): Boolean;
var
  BasePath, LibPath: string;
  f: TFileStream;
begin
  if aFile <> '' then
    BasePath := ExtractFilePath(aFile)
  else
    BasePath := ExtractFilePath(ParamStr(0));

  LibPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
             'data' + PathDelim + 'PSStudio' + PathDelim + 'lib' + PathDelim;

  FileName := LibPath + FileName;

  f := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Output, f.Size);
    if f.Size > 0 then
      f.ReadBuffer(Output[1], f.Size);
  finally
    f.Free;
  end;

  Result := True;
end;

function TfrmConsoleIDE.ceFindUnknownFile(Sender: TObject;
  const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
var
  BasePath, LibPath: string;
  f: TFileStream;
begin
  if aFile <> '' then
    BasePath := ExtractFilePath(aFile)
  else
    BasePath := ExtractFilePath(ParamStr(0));

  LibPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
             'data' + PathDelim + 'PSStudio' + PathDelim + 'lib' + PathDelim;

  FileName := LibPath + FileName;

  f := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Output, f.Size);
    if f.Size > 0 then
      f.ReadBuffer(Output[1], f.Size);
  finally
    f.Free;
  end;

  Result := True;
end;

procedure TfrmConsoleIDE.ceBreakpoint(Sender: TObject; const FileName: String; APosition, Row,
  Col: Cardinal);
begin
  pgcDebug.ActivePage := tsWatches;

  FActiveLine := Row;
  if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
  begin
    Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
  end;
  ed.CaretY := FActiveLine;
  ed.CaretX := 1;

  ed.Refresh;
end;

procedure TfrmConsoleIDE.FormDestroy(Sender: TObject);
begin
  FWatchList.Free;
end;

procedure TfrmConsoleIDE.SetActiveFile(const Value: string);
begin
  FActiveFile := Value;
  ce.MainFileName := ExtractFileName(FActiveFile);
  if Ce.MainFileName = '' then
    Ce.MainFileName := STR_UNNAMED;
end;

function GetErrorRowCol(const inStr: string): TPoint;
var
  Row:string;
  Col:string;
  p1,p2,p3:integer;
begin
  p1:=Pos('(',inStr);
  p2:=Pos(':',inStr);
  p3:=Pos(')',inStr);
  if (p1>0) and (p2>p1) and (p3>p2) then
  begin
    Row := Copy(inStr, p1+1,p2-p1-1);
    Col := Copy(inStr, p2+1,p3-p2-1);
    Result.X := StrToInt(Trim(Col));
    Result.Y := StrToInt(Trim(Row));
  end
  else
  begin
    Result.X := 1;
    Result.Y:= 1;
  end
end;

procedure TfrmConsoleIDE.messagesDblClick(Sender: TObject);
begin
  //if Copy(messages.Items[messages.ItemIndex],1,7)= '[Error]' then
  //begin
    ed.CaretXY := GetErrorRowCol(messages.Items[messages.ItemIndex]);
    ed.SetFocus;
  //end;
end;

procedure TfrmConsoleIDE.Gotolinenumber1Click(Sender: TObject);
begin
  with TfrmGotoLine.Create(self) do
  try
    Char := ed.CaretX;
    Line := ed.CaretY;
    ShowModal;
    if ModalResult = mrOK then
      ed.CaretXY := CaretXY;
  finally
    Free;
    ed.SetFocus;
  end;
end;

procedure TfrmConsoleIDE.Find1Click(Sender: TObject);
begin
  ShowSearchReplaceDialog(FALSE);
end;

procedure TfrmConsoleIDE.Searchagain1Click(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, FALSE);
end;

procedure TfrmConsoleIDE.Replace1Click(Sender: TObject);
begin
  ShowSearchReplaceDialog(TRUE);
end;

procedure TfrmConsoleIDE.StepInto1Click(Sender: TObject);
begin
  acDebugStepIntoExecute(nil);
end;

procedure TfrmConsoleIDE.StepOver1Click(Sender: TObject);
begin

end;

procedure TfrmConsoleIDE.Syntaxcheck1Click(Sender: TObject);
begin

end;

procedure TfrmConsoleIDE.edDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
 if AFiles.Count>=1 then
  if SaveCheck then //check if script changed and not yet saved
  begin
    ed.ClearAll;
    ed.Lines.LoadFromFile(AFiles[0]);
    ed.Modified := False;
    aFile := AFiles[0];
  end;
end;

end.

