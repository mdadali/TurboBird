//Version: 31Jan2005

unit ide_editor;

{$MODE Delphi}
//{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF Windows} Windows, {$ENDIF} LCLType, LCLIntf, LMessages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls, StdCtrls,
  ComCtrls, ActnList, SynEdit, SynEditTypes, SynHighlighterPas, SynEditSearch,
  SynEditMiscClasses, SynEditHighlighter, SynGutterBase, SynGutterMarks,
  SynGutterLineNumber, SynGutterChanges, SynGutter, SynGutterCodeFolding,
  SynEditMarkupSpecialLine, SynEditRegexSearch, SynEditMarks, PrintersDlgs, BCButtonFocus,

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

  { TIDE }

  TIDE = class(TForm)
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
    ce: TPSScriptDebugger;
    edtFormName: TEdit;
    IFPS3DllPlugin1: TPSDllPlugin;
    ImageListClassic: TImageList;
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
    messages: TListBox;
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
    procedure ceCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure ceExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
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
    FSearchFromCaret: boolean;
    FActiveLine: Longint;
    FResume: Boolean;
    FActiveFile: string;
    procedure RemoveComments(const SourceLines: TStrings; DestLines: TStringList );
    function Compile: Boolean;
    function Execute: Boolean;

    procedure Writeln(const s: string);
    procedure Readln(var s: string);
    procedure SetActiveFile(const Value: string);

    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);

    property aFile: string read FActiveFile write SetActiveFile;
  public
    procedure edMyPaint(Sender: TObject; ACanvas: TCanvas);
    function SaveCheck: Boolean;

    procedure AssignOnClick(Sender: TButton; const ScriptProcName: string);
  end;

var
  IDE: TIDE;

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


procedure TIDE.AssignOnClick(Sender: TButton; const ScriptProcName: string);
var
  Met: TMethod;
begin
  Met := ce.GetProcMethod(ScriptProcName);
  TButton(Sender).OnClick := TNotifyEvent(Met);
end;

procedure TIDE.edMyPaint(Sender: TObject; ACanvas: TCanvas);
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

procedure TIDE.FormCreate(Sender: TObject);
begin
  ed.OnPaint := edMyPaint;
end;

procedure TIDE.DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
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

procedure TIDE.ShowSearchReplaceDialog(AReplace: boolean);
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

//added
function IsCodeLine(LineText: String): Boolean;
begin
  // Entferne führende und nachfolgende Leerzeichen
  LineText := Trim(LineText);
  // Prüfe, ob die Zeile leer ist oder mit einem Kommentar beginnt (Pascal-Kommentarsyntax)
  Result := (LineText <> '') and (not LineText.StartsWith('//')) and
            (not LineText.StartsWith('{')) and (not LineText.EndsWith('}'));
end;

function IsCommentLine(LineText: String): Boolean;
begin
  // Entferne führende und nachfolgende Leerzeichen
  LineText := Trim(LineText);
  // Prüfe, ob die Zeile mit '//' oder '{' beginnt und mit '}' endet
  Result := LineText.StartsWith('//') or
            (LineText.StartsWith('{') and LineText.EndsWith('}'));
end;


procedure TIDE.edSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  if ce.HasBreakPoint(ce.MainFileName, Line) then
  begin
    Special := True;
    if Line = FActiveLine then
    begin
      BG := clSilver;
      FG := clRed;
    end else
    begin
      FG := clWhite;
      BG := clRed;
    end;
  end else
  if Line = FActiveLine then
  begin
    Special := True;
    FG := clWhite;
    bg := clBlue;
  end else Special := False;
end;

procedure TIDE.ceLineInfo(Sender: TObject; const FileName: String; APosition, Row,
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

procedure TIDE.BreakPointMenuClick(Sender: TObject);
begin

end;

procedure TIDE.acFileNewExecute(Sender: TObject);
begin
  if SaveCheck then //check if script changed and not yet saved
  begin
    ed.ClearAll;
    ed.Lines.Text := STR_DEFAULT_PROGRAM;
    ed.Modified := False;
    aFile := '';
  end;
end;

procedure TIDE.acFileExitExecute(Sender: TObject);
begin
  close;
end;

procedure TIDE.acEditUndoExecute(Sender: TObject);
begin
  ed.Undo;
end;

procedure TIDE.acEditRedoExecute(Sender: TObject);
begin
  ed.Redo;
end;

procedure TIDE.acEditCopyExecute(Sender: TObject);
begin
  ed.CopyToClipboard;
end;

procedure TIDE.acDebugSyntaxCheckExecute(Sender: TObject);
begin
  Compile;
  acDebugSyntaxCheck.Checked := false;
end;

procedure TIDE.acDebugDecompileExecute(Sender: TObject);
var s: string;
begin
  if Compile then
  begin
    ce.GetCompiled(s);
    IFPS3DataToText(s, s);
    //debugoutput.output.Lines.Text := s;
    //debugoutput.visible := true;
    Messages.Items.Add(S);
  end;
  acDebugDecompile.Checked := false;
end;

procedure TIDE.acDebugBreakPointExecute(Sender: TObject);
var Line: Longint;
begin
  Line := Ed.CaretY;
  if ce.HasBreakPoint(ce.MainFileName, Line) then
    ce.ClearBreakPoint(ce.MainFileName, Line)
  else
    ce.SetBreakPoint(ce.MainFileName, Line);
  ed.Refresh;
end;

procedure TIDE.acDebugPauseExecute(Sender: TObject);
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

procedure TIDE.acDebugResetExecute(Sender: TObject);
begin
  if ce.Exec.Status in isRunningOrPaused then
    ce.Stop;
  if acDebugPause.Checked then
    acDebugPause.Checked := false;
end;

procedure TIDE.acDebugRunExecute(Sender: TObject);
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
end;

procedure TIDE.acDebugStepIntoExecute(Sender: TObject);
begin
  if ce.Exec.Status in isRunningOrPaused then
    ce.StepInto
  else
  begin
    if Compile then
    begin
      ce.StepInto;
      Execute;
    end;
  end;
end;

procedure TIDE.acDebugStepOverExecute(Sender: TObject);
begin
  if ce.Exec.Status in isRunningOrPaused then
    ce.StepOver
  else
  begin
    if Compile then
    begin
      ce.StepInto;
      Execute;
    end;
  end;
end;

procedure TIDE.acEditCutExecute(Sender: TObject);
begin
  ed.CutToClipboard;
end;

procedure TIDE.acEditPasteExecute(Sender: TObject);
begin
  ed.PasteFromClipboard(false);
end;

procedure TIDE.acFileOpenExecute(Sender: TObject);
begin
  if SaveCheck then //check if script changed and not yet saved
  begin
    if OpenDialog1.Execute then
    begin
      ed.ClearAll;
      ed.Lines.LoadFromFile(OpenDialog1.FileName);
      ed.Modified := False;
      aFile := OpenDialog1.FileName;
    end;
  end;
  acFileOpen.Checked := false;
end;

procedure TIDE.acFilePrintExecute(Sender: TObject);
begin
  //
  acFilePrint.Checked := false;
end;

procedure TIDE.acFileRecentExecute(Sender: TObject);
begin
  //
  acFileRecent.Checked := false;
end;

procedure TIDE.acFileSaveExecute(Sender: TObject);
begin
  if aFile <> '' then
  begin
    ed.Lines.SaveToFile(aFile);
    ed.Modified := False;
  end else
    acFileSaveAsExecute(nil);
  acFileSave.Checked := false;
end;

procedure TIDE.acFileSaveAsExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    aFile := SaveDialog1.FileName;
    ed.Lines.SaveToFile(aFile);
    ed.Modified := False;
  end;
  acFileSaveAs.Checked := false;
end;

procedure TIDE.ceCompile(Sender: TPSScript);
begin
  Sender.AddMethod(Self, @TIDE.Writeln, 'procedure writeln(s: string)');
  Sender.AddMethod(Self, @TIDE.Readln, 'procedure readln(var s: string)');
  Sender.AddRegisteredVariable('Self', 'TForm');
  Sender.AddRegisteredVariable('Application', 'TApplication');
end;

procedure TIDE.ceCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_MainScriptInterface(x);
end;

procedure TIDE.ceExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_MainScriptInterface_Routines(se);
end;

procedure TIDE.edGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
begin
  ed.CaretXY := Point(1, Line);
  acDebugBreakPointExecute(nil);
end;

procedure TIDE.FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

procedure TIDE.RemoveComments(const SourceLines: TStrings; DestLines: TStringList );
var
  i, j: Integer;
  line, tempLine: string;
  inBlockComment, inString: Boolean;
begin
  inBlockComment := False;
  for i := 0 to SourceLines.Count - 1 do
  begin
    line := SourceLines[i];
    tempLine := '';
    inString := False;

    j := 1;
    while j <= Length(line) do
    begin
      // Zeichenfolge innerhalb von Anführungszeichen erkennen
      if line[j] = '''' then
      begin
        tempLine := tempLine + line[j];
        Inc(j);
        while (j <= Length(line)) and (line[j] <> '''') do
        begin
          tempLine := tempLine + line[j];
          Inc(j);
        end;
        if j <= Length(line) then
          tempLine := tempLine + line[j];
        Inc(j);
        Continue;
      end;

      // Blockkommentar beenden
      if inBlockComment then
      begin
        if (j < Length(line)) and (line[j] = '}') then
        begin
          inBlockComment := False;
          Inc(j);
        end
        else if (j < Length(line) - 1) and (line[j] = '*') and (line[j + 1] = ')') then
        begin
          inBlockComment := False;
          Inc(j, 2);
        end
        else
          Inc(j);
        Continue;
      end;

      // Einzeiliger Kommentar "//"
      if (line[j] = '/') and (j < Length(line)) and (line[j + 1] = '/') then
        Break // Beende die Zeilenverarbeitung für einzeilige Kommentare

      // Blockkommentar starten
      else if (line[j] = '{') then
      begin
        inBlockComment := True;
        Inc(j);
      end
      else if (j < Length(line) - 1) and (line[j] = '(') and (line[j + 1] = '*') then
      begin
        inBlockComment := True;
        Inc(j, 2);
      end
      else
      begin
        tempLine := tempLine + line[j];
        Inc(j);
      end;
    end;

    DestLines.Add(TrimRight(tempLine)); // Leere Zeilen werden beibehalten
  end;
end;

function TIDE.Compile: Boolean;
var
  tempLines: TStringList;
  i: Integer;
  TmpString: string;
begin
  tempLines := TStringList.Create;
  try
    RemoveComments(ed.Lines, tempLines);

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
end;

{function TIDE.Compile: Boolean;
var
  i: Longint;
begin
  ce.Script.Assign(ed.Lines);
  Result := ce.Compile;
  messages.Clear;
  for i := 0 to ce.CompilerMessageCount -1 do
  begin
    Messages.Items.Add(ce.CompilerMessages[i].MessageToString);
  end;
  if Result then
    Messages.Items.Add(STR_SUCCESSFULLY_COMPILED);
end;
}

procedure TIDE.ceIdle(Sender: TObject);
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

procedure TIDE.ceExecute(Sender: TPSScript);
begin
  ce.SetVarToInstance('SELF', Self);
  ce.SetVarToInstance('APPLICATION', Application);
  Caption := STR_FORM_TITLE_RUNNING;
end;

procedure TIDE.ceAfterExecute(Sender: TPSScript);
begin
  Caption := STR_FORM_TITLE;
  FActiveLine := 0;
  ed.Refresh;
end;

function TIDE.Execute: Boolean;
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

procedure TIDE.Writeln(const s: string);
begin
  //debugoutput.output.Lines.Add(S);
  //debugoutput.Visible := True;
  Messages.Items.Add(S);
end;

procedure TIDE.Readln(var s: string);
begin
  s := InputBox(STR_INPUTBOX_TITLE, '', '');
end;

//check if script changed and not yet saved//
function TIDE.SaveCheck: Boolean;
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

procedure TIDE.edStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  StatusBar.Panels[0].Text := IntToStr(ed.CaretY)+':'+IntToStr(ed.CaretX)
end;

procedure TIDE.Decompile1Click(Sender: TObject);
begin

end;


function TIDE.ceNeedFile(Sender: TObject; const OrginFileName: String;
  var FileName, Output: String): Boolean;
var
  path: string;
  f: TFileStream;
begin
  if aFile <> '' then
    Path := ExtractFilePath(aFile)
  else
    Path := ExtractFilePath(ParamStr(0));
  Path := Path + FileName;
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  except
    Result := false;
    exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
end;

procedure TIDE.ceBreakpoint(Sender: TObject; const FileName: String; APosition, Row,
  Col: Cardinal);
begin
  FActiveLine := Row;
  if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
  begin
    Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
  end;
  ed.CaretY := FActiveLine;
  ed.CaretX := 1;

  ed.Refresh;
end;

procedure TIDE.SetActiveFile(const Value: string);
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

procedure TIDE.messagesDblClick(Sender: TObject);
begin
  //if Copy(messages.Items[messages.ItemIndex],1,7)= '[Error]' then
  //begin
    ed.CaretXY := GetErrorRowCol(messages.Items[messages.ItemIndex]);
    ed.SetFocus;
  //end;
end;

procedure TIDE.Gotolinenumber1Click(Sender: TObject);
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

procedure TIDE.Find1Click(Sender: TObject);
begin
  ShowSearchReplaceDialog(FALSE);
end;

procedure TIDE.Searchagain1Click(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, FALSE);
end;

procedure TIDE.Replace1Click(Sender: TObject);
begin
  ShowSearchReplaceDialog(TRUE);
end;

procedure TIDE.StepInto1Click(Sender: TObject);
begin
  acDebugStepIntoExecute(nil);
end;

procedure TIDE.StepOver1Click(Sender: TObject);
begin

end;

procedure TIDE.Syntaxcheck1Click(Sender: TObject);
begin

end;

procedure TIDE.edDropFiles(Sender: TObject; X, Y: Integer;
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

