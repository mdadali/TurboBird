unit fsqlmonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit, SynHighlighterSQL,

  IBSQLMonitor,
  IBInternals,

  turbocommon,
  uthemeselector;

type

  { TfmSQLMonitor }

  TfmSQLMonitor = class(TForm)
    btnStartStopMonitor: TButton;
    btnSaveLog: TButton;
    btnClearLog: TButton;
    chkgrboxTraceFlags: TCheckGroup;
    IBSQLMonitor1: TIBSQLMonitor;
    pnlButtons: TPanel;
    pnlTop: TPanel;
    SaveDialog: TSaveDialog;
    SynEditMonitor: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure btnClearLogClick(Sender: TObject);
    procedure btnSaveLogClick(Sender: TObject);
    procedure btnStartStopMonitorClick(Sender: TObject);
    procedure chkgrboxTraceFlagsItemClick(Sender: TObject; Index: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
  private

  public

  end;

var
  fmSQLMonitor: TfmSQLMonitor;

implementation

{$R *.lfm}

{ TfmSQLMonitor }

procedure TfmSQLMonitor.IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
begin
  SynEditMonitor.Lines.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss.zzzz',EventTime) + ': ' + EventText);

  SynEditMonitor.CaretY := SynEditMonitor.Lines.Count;
  SynEditMonitor.CaretX := 1;

  Application.ProcessMessages;
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

procedure TfmSQLMonitor.btnClearLogClick(Sender: TObject);
begin
  SynEditMonitor.Lines.Clear;
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

procedure TfmSQLMonitor.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);

  Application.ProcessMessages;
end;

end.

