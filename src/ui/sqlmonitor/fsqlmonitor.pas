unit fsqlmonitor;

{$mode ObjFPC}{$H+}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit, SynHighlighterSQL,

  DateUtils,

  IBSQLMonitor,
  IBInternals,

  turbocommon,
  uthemeselector, SynEditMarkupSpecialLine;

type

  { TfmSQLMonitor }

  TfmSQLMonitor = class(TForm)
    btnClearLog: TButton;
    btnSaveLog: TButton;
    btnStartStopMonitor: TButton;
    btnCopyLog: TButton;
    btnClose: TButton;
    btnSearch: TButton;
    btnSearchNext: TButton;
    chkgrboxTraceFlags: TCheckGroup;
    IBSQLMonitor1: TIBSQLMonitor;
    pnlButtons: TPanel;
    pnlTop: TPanel;
    SaveDialog: TSaveDialog;
    SynEditMonitor: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure btnClearLogClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyLogClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSearchNextClick(Sender: TObject);
    procedure btnStartStopMonitorClick(Sender: TObject);
    procedure chkgrboxTraceFlagsItemClick(Sender: TObject; Index: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
    procedure SynEditMonitorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditMonitorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
  private
    SlowLines: TList;        // Integer-Liste: Zeilennummern der SLOW-Queries
    SearchHitLine: Integer;  // Aktueller Suchtreffer (-1 = keiner
    SearchText: string;
  public

  end;

var
  fmSQLMonitor: TfmSQLMonitor;
  ExecuteStartTime: TDateTime;
  ExecuteStatement: string;
  IsExecuting: Boolean;


implementation

{$R *.lfm}

{ TfmSQLMonitor }

procedure TfmSQLMonitor.IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
var
  Line: string;
  DurationMs: Integer;
  SlowMsg: string;
  CurrentLine: Integer;
begin
  Line := FormatDateTime('dd/mm/yyyy hh:nn:ss.zzzz', EventTime) + ': ' + EventText;
  SlowMsg := '';

  // --- Slow Query Detection (BLEIBT GLEICH WIE VORHER) ---
  if Pos('[Execute]', EventText) > 0 then
  begin
    ExecuteStartTime := EventTime;
    IsExecuting := True;
  end
  else if IsExecuting and (Pos('[Fetch]', EventText) > 0) and (Pos('SEOFReached', EventText) = 0) then
  begin
    DurationMs := MilliSecondsBetween(EventTime, ExecuteStartTime);
    if DurationMs > turbocommon.SlowQueryThreshold then
      SlowMsg := Format(' [SLOW: %d ms]', [DurationMs]);
    IsExecuting := False;
  end
  else if IsExecuting and (
    (Pos('[Commit', EventText) > 0) or
    (Pos('[Rollback', EventText) > 0) or
    (Pos('[Prepare]', EventText) > 0)
  ) then
  begin
    DurationMs := MilliSecondsBetween(EventTime, ExecuteStartTime);
    if DurationMs > turbocommon.SlowQueryThreshold then
      SlowMsg := Format(' [SLOW: %d ms]', [DurationMs]);
    IsExecuting := False;
  end;

  // --- In SynEdit einfügen ---
  if SlowMsg <> '' then
    Line := Line + SlowMsg;

  SynEditMonitor.Lines.Add(Line);
  CurrentLine := SynEditMonitor.Lines.Count - 1;

  // NEU: Wenn SLOW, Zeilennummer merken
  if SlowMsg <> '' then
    SlowLines.Add(Pointer(CurrentLine));

  // Auto-Scroll
  SynEditMonitor.CaretY := SynEditMonitor.Lines.Count;
  SynEditMonitor.CaretX := 1;
  if SynEditMonitor.Lines.Count > SynEditMonitor.LinesInWindow then
    SynEditMonitor.TopLine := SynEditMonitor.Lines.Count - SynEditMonitor.LinesInWindow + 1;

  Application.ProcessMessages;
end;

procedure TfmSQLMonitor.SynEditMonitorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F3 then
  begin
    if Shift = [ssShift] then
    begin
      // Shift+F3 = rückwärts suchen (optional, lassen wir erstmal weg)
    end
    else
      btnSearchNextClick(nil);
    Key := 0;
  end;
end;

procedure TfmSQLMonitor.SynEditMonitorSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
var
  i: Integer;
begin
  // 1. Suchtreffer (aktueller) gelb markieren
  if (SearchHitLine >= 0) and (Line - 1 = SearchHitLine) then
  begin
    Special := True;
    FG := clBlack;
    BG := clYellow;
    Exit;
  end;

  // 2. SLOW-Queries rot markieren
  for i := 0 to SlowLines.Count - 1 do
  begin
    if Integer(SlowLines[i]) = Line - 1 then
    begin
      Special := True;
      FG := clWhite;
      BG := clRed;
      Exit;
    end;
  end;

  // 3. NEU: Alle Zeilen mit Suchtext blau markieren (außer dem aktuellen Treffer)
  if (SearchText <> '') and (Pos(SearchText, SynEditMonitor.Lines[Line - 1]) > 0) then
  begin
    Special := True;
    FG := clWhite;
    BG := clBlue;
    Exit;
  end;
end;

procedure TfmSQLMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  {if IBSQLMonitor1.Enabled then
    IBSQLMonitor1.Enabled := false;
  DisableMonitoring;}

  //Application.ProcessMessages;
  CloseAction := caHide;
end;

procedure TfmSQLMonitor.btnStartStopMonitorClick(Sender: TObject);
begin
  if  btnStartStopMonitor.Caption = 'Stop' then
  begin
    IBSQLMonitor1.Enabled := false;
    DisableMonitoring;
    btnStartStopMonitor.Caption := 'Start';
    TraceEnabled := false;
  end else
  begin
    IBSQLMonitor1.Enabled := true;
    EnableMonitoring;
    btnStartStopMonitor.Caption := 'Stop';
    TraceEnabled := true;
  end;
  turbocommon.WriteIniFile;
  Application.ProcessMessages;
end;

procedure TfmSQLMonitor.btnSaveLogClick(Sender: TObject);
begin
  SaveDialog.FileName := 'SQLMonitor_' + FormatDateTime('yyyy-mm-dd_hhnnss', Now) + '.log';

  if SaveDialog.Execute then
    SynEditMonitor.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TfmSQLMonitor.btnSearchClick(Sender: TObject);
var
  i: Integer;
begin
  SearchText := InputBox('Search in SQL Monitor', 'Text:', 'SLOW');
  if SearchText = '' then Exit;

  for i := SearchHitLine + 1 to SynEditMonitor.Lines.Count - 1 do
  begin
    if Pos(SearchText, SynEditMonitor.Lines[i]) > 0 then
    begin
      SearchHitLine := i;
      SynEditMonitor.CaretY := i + 1;
      SynEditMonitor.CaretX := 1;
      if i > SynEditMonitor.LinesInWindow then
        SynEditMonitor.TopLine := i - (SynEditMonitor.LinesInWindow div 2) + 1;
      SynEditMonitor.Invalidate;
      Exit;
    end;
  end;

  SearchHitLine := -1;
  SearchText := '';                  // NEU: zurücksetzen wenn nichts gefunden
  SynEditMonitor.Invalidate;
  ShowMessage('"' + SearchText + '" nicht gefunden.');
end;

procedure TfmSQLMonitor.btnSearchNextClick(Sender: TObject);
var
  i: Integer;
begin
  if SearchText = '' then
  begin
    btnSearchClick(nil);
    Exit;
  end;

  for i := SearchHitLine + 1 to SynEditMonitor.Lines.Count - 1 do
  begin
    if Pos(SearchText, SynEditMonitor.Lines[i]) > 0 then
    begin
      SearchHitLine := i;
      SynEditMonitor.CaretY := i + 1;
      SynEditMonitor.CaretX := 1;
      if i > SynEditMonitor.LinesInWindow then
        SynEditMonitor.TopLine := i - (SynEditMonitor.LinesInWindow div 2) + 1;
      SynEditMonitor.Invalidate;
      Exit;
    end;
  end;

  // Am Ende angelangt – von vorne beginnen?
  SearchHitLine := -1;
  SynEditMonitor.Invalidate;
  ShowMessage('No more matches found for "' + SearchText + '".');
end;

procedure TfmSQLMonitor.btnClearLogClick(Sender: TObject);
begin
  SynEditMonitor.Lines.Clear;
end;

procedure TfmSQLMonitor.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmSQLMonitor.btnCopyLogClick(Sender: TObject);
begin
  SynEditMonitor.CopyToClipboard;
end;

procedure TfmSQLMonitor.chkgrboxTraceFlagsItemClick(Sender: TObject; Index: integer);
var
  WasRunning: Boolean;
  NewFlags: TTraceFlags;
  DBRec: TDatabaseRec;
  i: integer;
begin
  // Merken, ob Monitor lief
  WasRunning := IBSQLMonitor1.Enabled;

  // Stoppen, falls aktiv
  if WasRunning then
  begin
    IBSQLMonitor1.Enabled := False;
    DisableMonitoring;
  end;

  // Neue Flags aus CheckGroup zusammenbauen
  NewFlags := [];
  if chkgrboxTraceFlags.Checked[0] then Include(NewFlags, tfConnect);
  if chkgrboxTraceFlags.Checked[1] then Include(NewFlags, tfTransact);
  if chkgrboxTraceFlags.Checked[2] then Include(NewFlags, tfQPrepare);
  if chkgrboxTraceFlags.Checked[3] then Include(NewFlags, tfQExecute);
  if chkgrboxTraceFlags.Checked[4] then Include(NewFlags, tfQFetch);
  if chkgrboxTraceFlags.Checked[5] then Include(NewFlags, tfStmt);
  if chkgrboxTraceFlags.Checked[6] then Include(NewFlags, tfService);
  if chkgrboxTraceFlags.Checked[7] then Include(NewFlags, tfBlob);
  if chkgrboxTraceFlags.Checked[8] then Include(NewFlags, tfMisc);
  if chkgrboxTraceFlags.Checked[9] then Include(NewFlags, tfError);

  // IBSQLMonitor aktualisieren
  IBSQLMonitor1.TraceFlags := NewFlags;

  // Globale Variablen aktualisieren
  turbocommon.TraceFlags := TraceFlagsToString(NewFlags);
  turbocommon.TraceEnabled := WasRunning; // bleibt so wie vorher

  // INI sofort schreiben
  turbocommon.WriteIniFile;

  for i := 0 to Length(RegisteredDatabases) - 1 do
    RegisteredDatabases[i].IBDatabase.TraceFlags := StringToTraceFlags(turbocommon.TraceFlags);

  // Wieder starten, wenn vorher aktiv
  if WasRunning then
  begin
    IBSQLMonitor1.Enabled := True;
    EnableMonitoring;
  end;

  Application.ProcessMessages;
end;

procedure TfmSQLMonitor.FormCreate(Sender: TObject);
var
  i: Integer;
  Flags: TTraceFlags;
begin
  SaveDialog.Filter := 'SQL Log Files (*.log)|*.log|Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
  SaveDialog.DefaultExt := 'log';

  turbocommon.ReadIniFile;

  SlowLines := TList.Create;
  SearchHitLine := -1;

  Flags := StringToTraceFlags(turbocommon.TraceFlags);

  // CheckGroup belegen
  chkgrboxTraceFlags.Checked[0] := tfConnect  in Flags;
  chkgrboxTraceFlags.Checked[1] := tfTransact in Flags;
  chkgrboxTraceFlags.Checked[2] := tfQPrepare in Flags;
  chkgrboxTraceFlags.Checked[3] := tfQExecute in Flags;
  chkgrboxTraceFlags.Checked[4] := tfQFetch   in Flags;
  chkgrboxTraceFlags.Checked[5] := tfStmt     in Flags;
  chkgrboxTraceFlags.Checked[6] := tfService  in Flags;
  chkgrboxTraceFlags.Checked[7] := tfBlob     in Flags;
  chkgrboxTraceFlags.Checked[8] := tfMisc     in Flags;
  chkgrboxTraceFlags.Checked[9] := tfError    in Flags;

  IBSQLMonitor1.TraceFlags := Flags;
  IBSQLMonitor1.Enabled := turbocommon.TraceEnabled;

  if IBSQLMonitor1.Enabled then
  begin
    EnableMonitoring;
    btnStartStopMonitor.Caption := 'Stop';
  end else
  begin
    DisableMonitoring;
    btnStartStopMonitor.Caption := 'Start';
  end;

  Application.ProcessMessages;
end;

procedure TfmSQLMonitor.FormDestroy(Sender: TObject);
begin
  SlowLines.Free;
end;

procedure TfmSQLMonitor.FormShow(Sender: TObject);
begin
  // frmThemeSelector.btnApplyClick(self);

  Application.ProcessMessages;
end;

end.

