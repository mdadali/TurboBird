unit u_bulk_export;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Math, DateUtils, Dialogs,
  Graphics, StdCtrls, ExtCtrls,
  SynEdit, Grids, CheckLst, ComCtrls, DB, BufStream,
  IBDatabase, IBQuery, IBSQL, IBXScript,

  turbocommon,
  fbcommon,
  uthemeselector,
  uFormulaPresets,
  fmetaquerys,

  uSystemInfo,
  uReport;

type
  { TfrmBulkExport }

  TfrmBulkExport = class(TForm)
    btnAddToQueue: TButton;
    btnClose: TButton;
    btnDeselectAll: TButton;
    btnExecute: TButton;
    btnOpenExternalFile: TButton;
    btnPreviewSQL: TButton;
    btnRefreshPresets: TButton;
    btnSelectAll: TButton;
    btnExportFileName: TButton;
    cbFormulaPreset: TComboBox;
    chkLstFields: TCheckListBox;
    comboxSourceDB: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    edtExportFileName: TEdit;
    edtBatchSize: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    edtLineBuffer: TEdit;
    grboxExportOptions: TGroupBox;
    grBoxFields: TGroupBox;
    grBoxFormulaPresets: TGroupBox;
    grBoxSource: TGroupBox;
    grBoxGeneratedQuery: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbSourceTable: TLabel;
    Panel1: TPanel;
    pnlFields: TPanel;
    pnlTop: TPanel;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
    sgFields: TStringGrid;
    syneditGenerateQuery: TSynEdit;

    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExportFileNameClick(Sender: TObject);
    procedure btnPreviewSQLClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnRefreshPresetsClick(Sender: TObject);
    procedure cbFormulaPresetChange(Sender: TObject);
    procedure comboxSourceServerChange(Sender: TObject);
    procedure comboxSourceDBChange(Sender: TObject);
    procedure comboxSourceTablesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;

    FFields: array of record
      FieldName: string;
      FieldType: string;
      Checked: Boolean;
      Formula: string;
    end;
    FSourceDBIndex: Integer;
    FDB: TIBDatabase;
    FTrans: TIBTransaction;
    FCancelled: Boolean;

    FInitialTableName: string;
    FInitialDBIndex: Integer;
    FUpdatingCombos: Boolean;
    FLastPresetIndex: Integer;

    procedure LoadServerList;
    procedure LoadDBList;
    procedure LoadTableList;
    procedure LoadFields;
    procedure LoadServerListFillOnly;
    procedure LoadFormulaPresets;
    procedure ApplyInitialSelection;
    procedure ApplyPresetToGrid;
    function  CountManualFormulas: Integer;
    function  GetBatchSize: Integer;
    function  GetFromRow: Integer;
    function  GetToRow: Integer;
    function  GetActivePreset: TFormulaPreset;
    procedure CancelClick(Sender: TObject);
    function  BuildExportSQL: string;
    function  BuildHeaderLine: string;
    function  NeedsQuoting(const AFieldType: string): Boolean;
    function  WrapWithQuotes(const AFieldExpr, AQuoteChar: string): string;
    procedure DoBulkExport(const ASQL: string);
  public
    procedure Init(ANodeInfos: TPNodeInfos; const ATableName: string);
  end;

implementation

{$R *.lfm}

// ============================================================================
// HILFSFUNKTIONEN
// ============================================================================

function ExpandSep(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '\t', #9,  [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
end;

// ============================================================================
// INITIALISIERUNG
// ============================================================================

procedure TfrmBulkExport.Init(ANodeInfos: TPNodeInfos; const ATableName: string);
begin
  FInitialTableName := Trim(ATableName);
  FInitialDBIndex := -1;
  if Assigned(ANodeInfos) then
    FInitialDBIndex := ANodeInfos^.dbIndex;
end;

procedure TfrmBulkExport.LoadServerListFillOnly;
var
  List: TStringList;
begin
  List := GetServerListFromTreeView;
  try
    comboxSourceServer.Items.Assign(List);
  finally
    List.Free;
  end;
end;

procedure TfrmBulkExport.LoadServerList;
begin
  ApplyInitialSelection;
end;

procedure TfrmBulkExport.ApplyInitialSelection;
var
  ServerName, DBTitle: string;
  Idx: Integer;
begin
  FUpdatingCombos := True;
  try
    LoadServerListFillOnly;

    if (FInitialDBIndex >= 0) and (FInitialDBIndex < Length(RegisteredDatabases)) then
    begin
      ServerName := RegisteredDatabases[FInitialDBIndex].RegRec.ServerName;
      Idx := comboxSourceServer.Items.IndexOf(ServerName);
      if Idx >= 0 then
        comboxSourceServer.ItemIndex := Idx;
    end
    else if comboxSourceServer.Items.Count > 0 then
      comboxSourceServer.ItemIndex := 0;
  finally
    FUpdatingCombos := False;
  end;

  if comboxSourceServer.ItemIndex >= 0 then
    comboxSourceServerChange(nil);

  if (FInitialDBIndex >= 0) and (FInitialDBIndex < Length(RegisteredDatabases)) then
  begin
    DBTitle := RegisteredDatabases[FInitialDBIndex].RegRec.Title;
    Idx := comboxSourceDB.Items.IndexOf(DBTitle);
    if Idx >= 0 then
    begin
      comboxSourceDB.ItemIndex := Idx;
      comboxSourceDBChange(nil);
    end;
  end;

  if (FInitialTableName <> '') and (comboxSourceTables.Items.Count > 0) then
  begin
    Idx := comboxSourceTables.Items.IndexOf(FInitialTableName);
    if Idx >= 0 then
    begin
      comboxSourceTables.ItemIndex := Idx;
      comboxSourceTablesChange(nil);
    end;
  end;
end;

procedure TfrmBulkExport.FormCreate(Sender: TObject);
begin
  sgFields.ColCount := 3;
  sgFields.Cells[0, 0] := 'Source Field';
  sgFields.Cells[1, 0] := 'Field Type';
  sgFields.Cells[2, 0] := 'Formula ($1 = value)';
  sgFields.ColWidths[0] := 150;
  sgFields.ColWidths[1] := 120;
  sgFields.ColWidths[2] := 250;

  btnExecute.Enabled := False;
  btnPreviewSQL.Enabled := False;

  FSourceDBIndex := -1;
  FDB := nil;
  FTrans := nil;

  FInitialTableName := '';
  FInitialDBIndex := -1;
  FUpdatingCombos := False;
  FLastPresetIndex := -1;

  edtBatchSize.Text := IntToStr(DefaultBatchSize);
  edtLineBuffer.Text := '70000000';   // 70 MB Default (≈ 1 Mio Zeilen)

  LoadFormulaPresets;
end;

procedure TfrmBulkExport.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
  ApplyInitialSelection;
end;

// ============================================================================
// PRESET-MANAGEMENT
// ============================================================================

procedure TfrmBulkExport.LoadFormulaPresets;
var
  i, DefIdx: Integer;
begin
  cbFormulaPreset.Items.Clear;

  for i := 0 to FormulaPresetManager.PresetCount - 1 do
    cbFormulaPreset.Items.Add(FormulaPresetManager.PresetName(i));

  if cbFormulaPreset.Items.Count = 0 then
  begin
    cbFormulaPreset.ItemIndex := -1;
    FLastPresetIndex := -1;
    Exit;
  end;

  DefIdx := FormulaPresetManager.GetDefaultPresetIndex;
  if DefIdx < 0 then DefIdx := 0;

  cbFormulaPreset.ItemIndex := DefIdx;
  FLastPresetIndex := DefIdx;
end;

function TfrmBulkExport.GetActivePreset: TFormulaPreset;
begin
  if cbFormulaPreset.ItemIndex < 0 then
    Exit(nil);
  Result := FormulaPresetManager.GetPresetByIndex(cbFormulaPreset.ItemIndex);
end;

procedure TfrmBulkExport.ApplyPresetToGrid;
var
  Preset: TFormulaPreset;
  i: Integer;
  Formula: string;
begin
  Preset := GetActivePreset;
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;
    Formula := Preset.GetFormulaForFieldType(FFields[i].FieldType);
    FFields[i].Formula := Formula;
    sgFields.Cells[2, i + 1] := Formula;
  end;
end;

function TfrmBulkExport.CountManualFormulas: Integer;
var
  i: Integer;
  Preset: TFormulaPreset;
  Expected, Actual: string;
begin
  Result := 0;
  if (FLastPresetIndex < 0) or (FLastPresetIndex >= FormulaPresetManager.PresetCount) then
    Exit;
  Preset := FormulaPresetManager.GetPresetByIndex(FLastPresetIndex);
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;
    Expected := Trim(Preset.GetFormulaForFieldType(FFields[i].FieldType));
    Actual := Trim(sgFields.Cells[2, i + 1]);
    if (Actual <> '') and (Actual <> Expected) then
      Inc(Result);
  end;
end;

procedure TfrmBulkExport.cbFormulaPresetChange(Sender: TObject);
var
  NewIdx, ManualCount: Integer;
begin
  if FUpdatingCombos then Exit;

  NewIdx := cbFormulaPreset.ItemIndex;
  if NewIdx = FLastPresetIndex then Exit;

  ManualCount := CountManualFormulas;
  if (ManualCount > 0) and (NewIdx >= 0) then
  begin
    if MessageDlg(
         Format('%d field(s) have manual formulas.' + sLineBreak +
                'Changing the preset will overwrite them.' + sLineBreak + sLineBreak +
                'Continue?', [ManualCount]),
         mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      FUpdatingCombos := True;
      try
        cbFormulaPreset.ItemIndex := FLastPresetIndex;
      finally
        FUpdatingCombos := False;
      end;
      Exit;
    end;
  end;

  FLastPresetIndex := NewIdx;
  ApplyPresetToGrid;
end;

procedure TfrmBulkExport.btnRefreshPresetsClick(Sender: TObject);
begin
  FormulaPresetManager.Reload;
  LoadFormulaPresets;
  if cbFormulaPreset.ItemIndex >= 0 then
  begin
    ApplyPresetToGrid;
    FLastPresetIndex := cbFormulaPreset.ItemIndex;
  end;
end;

procedure TfrmBulkExport.sgFieldsDblClick(Sender: TObject);
var
  NewFormula: string;
  Row: Integer;
begin
  Row := sgFields.Row;
  if (Row < 1) or (Row >= sgFields.RowCount) then Exit;

  NewFormula := sgFields.Cells[2, Row];
  if InputQuery('Formula for ' + sgFields.Cells[0, Row],
                'Enter SQL expression ($1 = field value):', NewFormula) then
  begin
    sgFields.Cells[2, Row] := NewFormula;
    if Row - 1 <= High(FFields) then
      FFields[Row - 1].Formula := NewFormula;
  end;
end;

// ============================================================================
// SOURCE-KASKADE
// ============================================================================

procedure TfrmBulkExport.comboxSourceServerChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  LoadDBList;
end;

procedure TfrmBulkExport.LoadDBList;
var
  i: Integer;
begin
  comboxSourceDB.Items.Clear;
  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxSourceServer.Text) then
      comboxSourceDB.Items.Add(RegisteredDatabases[i].RegRec.Title);
  if comboxSourceDB.Items.Count > 0 then
  begin
    comboxSourceDB.ItemIndex := 0;
    comboxSourceDBChange(nil);
  end;
end;

procedure TfrmBulkExport.comboxSourceDBChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  LoadTableList;
end;

procedure TfrmBulkExport.LoadTableList;
var
  i: Integer;
begin
  FSourceDBIndex := -1;
  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxSourceServer.Text) and
       SameText(RegisteredDatabases[i].RegRec.Title, comboxSourceDB.Text) then
    begin
      FSourceDBIndex := i;
      Break;
    end;

  if FSourceDBIndex < 0 then Exit;

  if Assigned(FDB) then
  begin
    if FDB.Connected then FDB.Connected := False;
    FreeAndNil(FDB);
  end;
  if Assigned(FTrans) then FreeAndNil(FTrans);

  FDB := TIBDatabase.Create(nil);
  FTrans := TIBTransaction.Create(nil);
  FDB.DefaultTransaction := FTrans;
  AssignIBDatabase(RegisteredDatabases[FSourceDBIndex].IBDatabase, FDB);
  with RegisteredDatabases[FSourceDBIndex] do
  begin
    FDB.Params.Values['user_name'] := RegRec.UserName;
    if RegRec.Password <> '' then
      FDB.Params.Values['password'] := RegRec.Password
    else
      FDB.Params.Values['password'] := GetDBSessionPassword(RegRec.ServerName, RegRec.DatabaseName);
  end;
  FDB.LoginPrompt := False;
  FDB.Connected := True;
  FTrans.StartTransaction;

  comboxSourceTables.Items.Clear;
  FDB.GetTableNames(comboxSourceTables.Items);

  if comboxSourceTables.Items.Count > 0 then
  begin
    comboxSourceTables.ItemIndex := 0;
    LoadFields;
  end;

  btnPreviewSQL.Enabled := True;
end;

procedure TfrmBulkExport.comboxSourceTablesChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  LoadFields;
  syneditGenerateQuery.Clear;
  btnExecute.Enabled := False;
end;

procedure TfrmBulkExport.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FDB) then
  begin
    if FDB.Connected then FDB.Connected := False;
    FreeAndNil(FDB);
  end;
  if Assigned(FTrans) then FreeAndNil(FTrans);
end;

// ============================================================================
// FELDER LADEN
// ============================================================================

procedure TfrmBulkExport.LoadFields;
var
  Iso: TIsolatedQuery;
  i: Integer;
  FSize: Integer;
begin
  if FSourceDBIndex < 0 then Exit;

  Iso := GetFieldsIsolated(RegisteredDatabases[FSourceDBIndex].IBDatabase,
                           comboxSourceTables.Text);
  try
    SetLength(FFields, 0);
    chkLstFields.Clear;

    sgFields.RowCount := 1;
    sgFields.ColCount := 3;
    sgFields.Cells[0, 0] := 'Source Field';
    sgFields.Cells[1, 0] := 'Field Type';
    sgFields.Cells[2, 0] := 'Formula ($1 = value)';
    sgFields.ColWidths[0] := 150;
    sgFields.ColWidths[1] := 120;
    sgFields.ColWidths[2] := 250;

    i := 0;
    while not Iso.Query.EOF do
    begin
      SetLength(FFields, i + 1);
      FFields[i].FieldName := Trim(Iso.Query.FieldByName('field_name').AsString);
      GetFieldType(Iso.Query, FFields[i].FieldType, FSize);
      FFields[i].Checked := True;
      FFields[i].Formula := '';

      chkLstFields.Items.Add(FFields[i].FieldName);
      chkLstFields.Checked[i] := True;

      sgFields.RowCount := i + 2;
      sgFields.Cells[0, i + 1] := FFields[i].FieldName;
      sgFields.Cells[1, i + 1] := FFields[i].FieldType;
      sgFields.Cells[2, i + 1] := '';

      Inc(i);
      Iso.Query.Next;
    end;
  finally
    Iso.Free;
  end;

  if cbFormulaPreset.ItemIndex >= 0 then
  begin
    ApplyPresetToGrid;
    FLastPresetIndex := cbFormulaPreset.ItemIndex;
  end;
end;

procedure TfrmBulkExport.btnSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := True;
end;

procedure TfrmBulkExport.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := False;
end;

// ============================================================================
// BATCH / RANGE
// ============================================================================

procedure TfrmBulkExport.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

function TfrmBulkExport.GetBatchSize: Integer;
begin
  Result := StrToIntDef(edtBatchSize.Text, 1000000);
  if Result < 1 then Result := 1000000;
end;

function TfrmBulkExport.GetFromRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtFrom.Text, 1)
  else
    Result := 1;
end;

function TfrmBulkExport.GetToRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtTo.Text, MaxInt)
  else
    Result := MaxInt;
end;

// ============================================================================
// QUOTE-WRAP
// ============================================================================

function TfrmBulkExport.NeedsQuoting(const AFieldType: string): Boolean;
var
  CT: string;
begin
  CT := UpperCase(AFieldType);

  // Zahlen und Bool werden NICHT gequotet
  if Pos('SMALLINT', CT) > 0 then Exit(False);
  if Pos('BIGINT',   CT) > 0 then Exit(False);
  if Pos('INTEGER',  CT) > 0 then Exit(False);
  if Pos('FLOAT',    CT) > 0 then Exit(False);
  if Pos('DOUBLE',   CT) > 0 then Exit(False);
  if Pos('NUMERIC',  CT) > 0 then Exit(False);
  if Pos('DECIMAL',  CT) > 0 then Exit(False);
  if Pos('BOOLEAN',  CT) > 0 then Exit(False);

  // Alles andere wird gequotet
  Result := True;
end;

function TfrmBulkExport.WrapWithQuotes(
  const AFieldExpr, AQuoteChar: string): string;
var
  QC, QC2: string;
begin
  if AQuoteChar = '' then
    Exit(AFieldExpr);

  QC  := QuotedStr(AQuoteChar);              // '"'
  QC2 := QuotedStr(AQuoteChar + AQuoteChar); // '""'

  Result := QC + ' || REPLACE(' + AFieldExpr + ', ' + QC + ', ' + QC2 + ') || ' + QC;
end;

// ============================================================================
// SQL-BAU
// ============================================================================

function TfrmBulkExport.BuildExportSQL: string;
var
  i: Integer;
  Preset: TFormulaPreset;
  FieldExpr, Formula, Separator, LineExpr: string;
begin
  Result := '';

  Preset := GetActivePreset;
  if Preset = nil then
  begin
    ShowMessage('Please select a formula preset.');
    Exit;
  end;

  Separator := ExpandSep(Preset.Separator);

  LineExpr := '';
  for i := 0 to High(FFields) do
  begin
    if not chkLstFields.Checked[i] then Continue;
    if i + 1 >= sgFields.RowCount then Break;

    // Formel aus Grid (oder $1)
    Formula := Trim(sgFields.Cells[2, i + 1]);
    if Formula = '' then
      Formula := '$1';

    // $1 durch Feldname ersetzen
    Formula := StringReplace(Formula, '$1', FFields[i].FieldName, [rfReplaceAll]);
    FieldExpr := '(' + Formula + ')';

    // Quote-Wrap wenn Preset.DataQuoted + Feldtyp braucht es
    if Preset.DataQuoted and (Preset.QuoteChar <> '') and
       NeedsQuoting(FFields[i].FieldType) then
      FieldExpr := WrapWithQuotes(FieldExpr, Preset.QuoteChar);

    // Separator davor (außer beim ersten)
    if LineExpr <> '' then
    begin
      if Separator <> '' then
        LineExpr := LineExpr + ' || ' + QuotedStr(Separator) + ' || '
      else
        LineExpr := LineExpr + ' || ';
    end;
    LineExpr := LineExpr + FieldExpr;
  end;

  if LineExpr = '' then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  Result := 'SELECT ' + LineExpr + ' AS csv_line FROM ' +
            MakeObjectNameQuoted(comboxSourceTables.Text);
end;

function TfrmBulkExport.BuildHeaderLine: string;
var
  i: Integer;
  Preset: TFormulaPreset;
  Sep, QC, Line: string;
begin
  Result := '';
  Preset := GetActivePreset;
  if Preset = nil then Exit;
  if not Preset.IncludeHeader then Exit;

  Sep := ExpandSep(Preset.Separator);
  QC := Preset.QuoteChar;

  Line := '';
  for i := 0 to High(FFields) do
  begin
    if not chkLstFields.Checked[i] then Continue;
    if Line <> '' then Line := Line + Sep;
    if Preset.HeaderQuoted and (QC <> '') then
      Line := Line + QC + FFields[i].FieldName + QC
    else
      Line := Line + FFields[i].FieldName;
  end;

  if Line <> '' then
    Result := Line + sLineBreak;
end;

// ============================================================================
// PREVIEW SQL
// ============================================================================

procedure TfrmBulkExport.btnPreviewSQLClick(Sender: TObject);
var
  SQL: string;
begin
  SQL := BuildExportSQL;
  if SQL = '' then Exit;

  syneditGenerateQuery.Text := SQL;
  btnExecute.Enabled := True;
end;

// ============================================================================
// EXPORT AUSFÜHREN
// ============================================================================

procedure TfrmBulkExport.btnExecuteClick(Sender: TObject);
var
  ExportSQL: string;
begin
  ExportSQL := Trim(syneditGenerateQuery.Text);
  if ExportSQL = '' then
  begin
    ShowMessage('No SQL to execute. Please click "Preview SQL" first.');
    Exit;
  end;
  if Trim(edtExportFileName.Text) = '' then
  begin
    ShowMessage('Please select an export file.');
    Exit;
  end;

  DoBulkExport(ExportSQL);
end;

procedure TfrmBulkExport.btnExportFileNameClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    Filter := 'CSV files (*.csv)|*.csv|Text files (*.txt)|*.txt|All files (*.*)|*.*';
    if Execute then
      edtExportFileName.Text := FileName;
  finally
    Free;
  end;
end;

procedure TfrmBulkExport.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmBulkExport.btnAddToQueueClick(Sender: TObject);
begin
  MessageDlg('Queue feature coming soon!', mtInformation, [mbOK], 0);
end;

procedure TfrmBulkExport.CancelClick(Sender: TObject);
begin
  FCancelled := True;
  if Sender is TButton then
  begin
    TButton(Sender).Enabled := False;
    TButton(Sender).Caption := 'Cancelling...';
  end;
end;

// ============================================================================
// BULK EXPORT ENGINE
// ============================================================================

procedure TfrmBulkExport.DoBulkExport(const ASQL: string);
var
  TotalRows, ExpectedRows: Int64;
  Exported: Int64;
  FromRow, ToRow: Integer;
  UseRange: Boolean;
  ProgressForm: TForm;
  ProgressLabel, LblElapsed, LblPhase: TLabel;
  ProgressBar: TProgressBar;
  BtnCancel: TButton;
  StartTime, EndTime: TDateTime;
  DB: TIBDatabase;
  Trans: TIBTransaction;
  Q: TIBSQL;
  Line: RawByteString;
  LineEnd: RawByteString;
  LenLine, LenEnd: Integer;
  SQL, BaseSQL, CountSQL: string;
  FileStream: TBufferedFileStream;
  BufferBytes: Int64;
  BytesSinceGui: Int64;
  FileSize: Int64;
  Stats: TTransferStatistic;
  ReportForm: TfrmReport;
  i, CheckedCount: Integer;
  HeaderLine: string;

  procedure UpdateProgress;
  var
    ElapsedSec: Double;
    RowsPerSec: Double;
  begin
    if ExpectedRows > 0 then
      ProgressBar.Position := Exported;

    ElapsedSec := (Now - StartTime) * 86400;
    if ElapsedSec > 0 then
      RowsPerSec := Exported / ElapsedSec
    else
      RowsPerSec := 0;

    if ExpectedRows > 0 then
      ProgressLabel.Caption := Format('Exported %s of %s rows',
        [FormatFloat('#,##0', Exported), FormatFloat('#,##0', ExpectedRows)])
    else
      ProgressLabel.Caption := Format('Exported %s rows',
        [FormatFloat('#,##0', Exported)]);

    LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
      '   |   ' + FormatFloat('#,##0', Round(RowsPerSec)) + ' rows/sec';

    Application.ProcessMessages;
  end;

begin
  UseRange := rbRange.Checked;
  FromRow := GetFromRow;
  ToRow := GetToRow;

  // --- Buffer-Größe in Bytes (aus edtLineBuffer) ---
  BufferBytes := StrToIntDef(edtLineBuffer.Text, 70000000);
  if BufferBytes < 65536 then
    BufferBytes := 65536;
  if BufferBytes > 268435456 then
    BufferBytes := 268435456;

  // ------------------------------------------------------------------
  // Statistik vorbereiten
  // ------------------------------------------------------------------
  FillChar(Stats, SizeOf(Stats), 0);
  Stats.Kind             := tkExport;
  Stats.SourceKind       := 'Firebird Table';
  Stats.SourceServer     := comboxSourceServer.Text;
  Stats.SourceDatabase   := comboxSourceDB.Text;
  Stats.SourceTable      := comboxSourceTables.Text;
  Stats.DestKind         := 'File';
  Stats.DestFileName     := edtExportFileName.Text;
  Stats.BatchSize        := GetBatchSize;
  Stats.FormulaUsed      := True;    // immer, per Modell A
  Stats.FromRow          := FromRow;
  Stats.ToRow            := ToRow;
  Stats.UseRowRange      := UseRange;

  if (FSourceDBIndex >= 0) and (FSourceDBIndex < Length(RegisteredDatabases)) then
  begin
    Stats.SourceServerVersion := RegisteredDatabases[FSourceDBIndex].RegRec.ServerVersionString;
    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;
  end;

  Stats.SystemInfo := GetSystemInfo(edtExportFileName.Text);

  CheckedCount := 0;
  for i := 0 to High(FFields) do
    if chkLstFields.Checked[i] then Inc(CheckedCount);
  Stats.FieldsCount := CheckedCount;

  Stats.FormulasApplied := '';
  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;
    if not chkLstFields.Checked[i] then Continue;
    if Trim(sgFields.Cells[2, i + 1]) <> '' then
      Stats.FormulasApplied := Stats.FormulasApplied +
        '  • ' + FFields[i].FieldName + ' = ' + sgFields.Cells[2, i + 1] + sLineBreak;
  end;

  // ------------------------------------------------------------------
  // Eigene DB-Verbindung
  // ------------------------------------------------------------------
  DB := TIBDatabase.Create(nil);
  Trans := TIBTransaction.Create(nil);
  DB.DefaultTransaction := Trans;
  Trans.DefaultDatabase := DB;
  AssignIBDatabase(RegisteredDatabases[FSourceDBIndex].IBDatabase, DB);
  with RegisteredDatabases[FSourceDBIndex] do
  begin
    DB.Params.Values['user_name'] := RegRec.UserName;
    if RegRec.Password <> '' then
      DB.Params.Values['password'] := RegRec.Password
    else
      DB.Params.Values['password'] := GetDBSessionPassword(RegRec.ServerName, RegRec.DatabaseName);
  end;
  DB.LoginPrompt := False;
  DB.Connected := True;
  Trans.StartTransaction;

  Q := TIBSQL.Create(nil);
  Q.Database := DB;
  Q.Transaction := Trans;

  // ------------------------------------------------------------------
  // BaseSQL: "SELECT " am Anfang entfernen
  // ------------------------------------------------------------------
  BaseSQL := Trim(ASQL);
  if UpperCase(Copy(BaseSQL, 1, 7)) = 'SELECT ' then
    BaseSQL := Trim(Copy(BaseSQL, 8, MaxInt));

  LineEnd := sLineBreak;
  HeaderLine := BuildHeaderLine;

  // ------------------------------------------------------------------
  // Progress-Formular
  // ------------------------------------------------------------------
  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.Width := 520;
    ProgressForm.Height := 220;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;
    ProgressForm.Caption := 'Bulk Export';

    LblPhase := TLabel.Create(ProgressForm);
    LblPhase.Parent := ProgressForm;
    LblPhase.Left := 16;
    LblPhase.Top := 16;
    LblPhase.Caption := 'Counting rows...';
    LblPhase.Font.Style := [fsBold];
    LblPhase.Width := 480;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 42;
    ProgressLabel.Caption := 'Please wait...';
    ProgressLabel.Width := 480;

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 70;
    ProgressBar.Width := 480;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;
    ProgressBar.Style := pbstMarquee;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 100;
    LblElapsed.Caption := 'Elapsed: 00:00:00';
    LblElapsed.Width := 480;

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 200;
    BtnCancel.Top := 140;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelClick;

    ProgressForm.Show;
    ProgressForm.BringToFront;
    Application.ProcessMessages;
    Sleep(50);
    Application.ProcessMessages;

    FCancelled := False;
    Exported := 0;
    BytesSinceGui := 0;

    // ============================================================
    // Phase 1: Zeilen zählen
    // ============================================================
    LblPhase.Caption := 'Counting rows...';
    Application.ProcessMessages;

    TotalRows := 0;
    try
      CountSQL := 'SELECT COUNT(*) FROM (SELECT ' + BaseSQL + ') AS cnt_qry';
      Q.SQL.Text := CountSQL;
      Q.ExecQuery;
      if not Q.EOF then
        TotalRows := Q.Fields[0].AsInt64;
      Q.Close;
    except
      TotalRows := 0;
    end;

    if UseRange then
    begin
      if ToRow > TotalRows then ToRow := TotalRows;
      if FromRow > TotalRows then FromRow := TotalRows;
      ExpectedRows := ToRow - FromRow + 1;
      if ExpectedRows < 0 then ExpectedRows := 0;
    end
    else
    begin
      ToRow := TotalRows;
      ExpectedRows := TotalRows;
    end;

    if ExpectedRows > 0 then
    begin
      ProgressBar.Style := pbstNormal;
      ProgressBar.Max := ExpectedRows;
      ProgressBar.Position := 0;
    end;

    // ============================================================
    // Phase 2: Datei öffnen + Header
    // ============================================================
    LblPhase.Caption := 'Opening output file...';
    Application.ProcessMessages;

    FileStream := TBufferedFileStream.Create(edtExportFileName.Text, fmCreate, BufferBytes);

    if HeaderLine <> '' then
      FileStream.Write(HeaderLine[1], Length(HeaderLine));

    // ============================================================
    // Phase 3: Export
    // ============================================================
    StartTime := Now;
    LblPhase.Caption := 'Exporting data...';
    Application.ProcessMessages;

    if UseRange and (FromRow > 1) then
      SQL := 'SELECT SKIP ' + IntToStr(FromRow - 1) +
             ' FIRST ' + IntToStr(ExpectedRows) + ' ' + BaseSQL
    else if UseRange then
      SQL := 'SELECT FIRST ' + IntToStr(ExpectedRows) + ' ' + BaseSQL
    else
      SQL := 'SELECT ' + BaseSQL;

    try
      Q.Close;
      Q.SQL.Text := SQL;
      Q.ExecQuery;

      while not Q.EOF do
      begin
        if FCancelled then Break;

        Line := Q.Fields[0].AsString;
        LenLine := Length(Line);
        LenEnd := Length(LineEnd);

        if LenLine > 0 then
          FileStream.Write(Line[1], LenLine);
        if LenEnd > 0 then
          FileStream.Write(LineEnd[1], LenEnd);

        Inc(Exported);
        Inc(BytesSinceGui, LenLine + LenEnd);

        if BytesSinceGui >= BufferBytes then
        begin
          BytesSinceGui := 0;
          UpdateProgress;
        end;

        Q.Next;
      end;

    finally
      FileStream.Flush;
      FileStream.Free;
    end;

    // ============================================================
    // Phase 4: Finalisieren
    // ============================================================
    LblPhase.Caption := 'Finalizing...';
    UpdateProgress;
    EndTime := Now;

    Stats.RowsProcessed  := Exported;
    Stats.ElapsedSeconds := (EndTime - StartTime) * SecsPerDay;

    FileSize := 0;
    if FileExists(edtExportFileName.Text) then
    begin
      try
        with TFileStream.Create(edtExportFileName.Text, fmOpenRead or fmShareDenyNone) do
        try
          FileSize := Size;
        finally
          Free;
        end;
      except
        FileSize := 0;
      end;
    end;
    Stats.DestFileSize := FileSize;

    if FCancelled then
      Stats.OptionsExtra := Format('CANCELLED after %s rows',
        [FormatFloat('#,##0', Exported)]);

    // ============================================================
    // Report anzeigen
    // ============================================================
    ReportForm := TfrmReport.Create(nil);
    try
      ReportForm.SetReportText(FormatTransferReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;

  finally
    Q.Free;
    if Trans.InTransaction then Trans.Rollback;
    DB.Connected := False;
    DB.Free;
    Trans.Free;
    ProgressForm.Free;
  end;
end;

end.
