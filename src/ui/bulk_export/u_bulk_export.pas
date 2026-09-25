unit u_bulk_export;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Math, DateUtils, Dialogs,
  Graphics, StdCtrls, ExtCtrls,
  //StreamIO,
  //Iostream,
  SynEdit, Grids, CheckLst, ComCtrls, DB,  BufStream,
  IBDatabase, IBQuery, IBSQL, IBXScript,

  turbocommon,
  fbcommon,
  uthemeselector,
  uFormulaPresets,
  fmetaquerys;

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
    chkUseFormula: TCheckBox;
    comboxSourceDB: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    edtExportFileName: TEdit;
    edtBatchSize: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
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
    procedure chkUseFormulaChange(Sender: TObject);
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
    FDB: TIBDatabase;           // eigene Verbindung für Quell-Metadaten
    FTrans: TIBTransaction;
    FCancelled: Boolean;

    FInitialTableName: string;
    FInitialDBIndex: Integer;
    FUpdatingCombos: Boolean;


    procedure LoadServerList;
    procedure LoadDBList;
    procedure LoadTableList;
    procedure LoadFields;
    procedure LoadServerListFillOnly;
    procedure ApplyInitialSelection;
    function  GetBatchSize: Integer;
    function  GetFromRow: Integer;
    function  GetToRow: Integer;
    procedure CancelClick(Sender: TObject);
    procedure DoBulkExport(const ASQL: string);
  public
    procedure Init(ANodeInfos: TPNodeInfos; const ATableName: string);
  end;

//var
  //frmBulkExport: TfrmBulkExport;

implementation

{$R *.lfm}

{ TfrmBulkExport }

// ------------------------------------------------------------------
// Source-Auswahl – nur FÜLLEN, keine Kaskade
// ------------------------------------------------------------------
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

// ------------------------------------------------------------------
// Vorbelegung anwenden
// ------------------------------------------------------------------
procedure TfrmBulkExport.Init(ANodeInfos: TPNodeInfos; const ATableName: string);
begin
  FInitialTableName := Trim(ATableName);
  FInitialDBIndex := -1;
  if Assigned(ANodeInfos) then
    FInitialDBIndex := ANodeInfos^.dbIndex;
end;

procedure TfrmBulkExport.ApplyInitialSelection;
var
  ServerName, DBTitle: string;
  Idx: Integer;
begin
  // Guard AN – Events werden blockiert
  FUpdatingCombos := True;
  try
    // 1. Server-Liste füllen
    LoadServerListFillOnly;

    // 2. Initial-Server oder Default = 0
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

  // 3. Kaskade explizit auslösen (füllt die DB-Liste)
  if comboxSourceServer.ItemIndex >= 0 then
    comboxSourceServerChange(nil);

  // 4. Initial-DB auswählen
  if (FInitialDBIndex >= 0) and (FInitialDBIndex < Length(RegisteredDatabases)) then
  begin
    DBTitle := RegisteredDatabases[FInitialDBIndex].RegRec.Title;

    Idx := comboxSourceDB.Items.IndexOf(DBTitle);
    if Idx >= 0 then
    begin
      comboxSourceDB.ItemIndex := Idx;
      comboxSourceDBChange(nil);   // lädt Tabellen
    end;
  end;

  // 5. Initial-Tabelle auswählen
  if (FInitialTableName <> '') and (comboxSourceTables.Items.Count > 0) then
  begin
    Idx := comboxSourceTables.Items.IndexOf(FInitialTableName);
    if Idx >= 0 then
    begin
      comboxSourceTables.ItemIndex := Idx;
      comboxSourceTablesChange(nil);   // lädt Felder
    end;
  end;
end;

// ------------------------------------------------------------------
// Neue Methode: nur Server-Liste füllen, kein Cascade
// ------------------------------------------------------------------
procedure TfrmBulkExport.LoadServerList;
begin
  ApplyInitialSelection;
end;


procedure TfrmBulkExport.FormCreate(Sender: TObject);
begin
  // Grid initialisieren: 3 Spalten, KEINE "Copy"-Spalte
  sgFields.ColCount := 3;
  sgFields.Cells[0, 0] := 'Field Name';
  sgFields.Cells[1, 0] := 'Field Type';
  sgFields.Cells[2, 0] := 'Formula ($1 = value)';
  sgFields.ColWidths[0] := 150;
  sgFields.ColWidths[1] := 120;
  sgFields.ColWidths[2] := 250;

  // Buttons initial
  btnExecute.Enabled := False;
  btnPreviewSQL.Enabled := False;

  FSourceDBIndex := -1;
  FDB := nil;
  FTrans := nil;

  FInitialTableName := '';
  FInitialDBIndex := -1;
  FUpdatingCombos := False;

  edtBatchSize.Text := IntToStr(DefaultBatchSize);
end;

procedure TfrmBulkExport.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
  ApplyInitialSelection;
end;


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

  // Eigene Datenbankverbindung für Metadaten (kurzlebig)
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
  // Credentials
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

procedure TfrmBulkExport.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  //if Assigned(FNodeInfos) then

end;

// ------------------------------------------------------------------
// Felder laden
// ------------------------------------------------------------------
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

  // Formel-Presets laden
  cbFormulaPreset.Items.Clear;
  cbFormulaPreset.Items.Add('None');
  for i := 0 to FormulaPresetManager.PresetCount - 1 do
    cbFormulaPreset.Items.Add(FormulaPresetManager.PresetName(i));
  cbFormulaPreset.ItemIndex := 0;
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

// ------------------------------------------------------------------
// Formeln (Presets / Use Checkbox / DblClick)
// ------------------------------------------------------------------
procedure TfrmBulkExport.cbFormulaPresetChange(Sender: TObject);
var
  Preset: TFormulaPreset;
  i: Integer;
  Formula: string;
begin
  if cbFormulaPreset.ItemIndex <= 0 then
  begin
    for i := 0 to High(FFields) do
    begin
      FFields[i].Formula := '';
      sgFields.Cells[2, i + 1] := '';
    end;
    Exit;
  end;

  Preset := FormulaPresetManager.GetPreset(cbFormulaPreset.Text);
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    Formula := Preset.GetFormulaForFieldType(FFields[i].FieldType);
    FFields[i].Formula := Formula;
    sgFields.Cells[2, i + 1] := Formula;
  end;
end;

procedure TfrmBulkExport.btnRefreshPresetsClick(Sender: TObject);
var i: integer;
begin
  FormulaPresetManager.Reload;
  cbFormulaPreset.Items.Clear;
  cbFormulaPreset.Items.Add('None');
  for i := 0 to FormulaPresetManager.PresetCount - 1 do
    cbFormulaPreset.Items.Add(FormulaPresetManager.PresetName(i));
  cbFormulaPreset.ItemIndex := 0;
end;

procedure TfrmBulkExport.chkUseFormulaChange(Sender: TObject);
begin
  // Nichts Besonderes, wird bei SQL-Generierung berücksichtigt
end;

procedure TfrmBulkExport.sgFieldsDblClick(Sender: TObject);
var
  NewFormula: string;
  Row: Integer;
begin
  if not chkUseFormula.Checked then
  begin
    ShowMessage('Enable "Use Formula" to enter formulas.');
    Exit;
  end;

  Row := sgFields.Row;
  if (Row < 1) or (Row >= sgFields.RowCount) then Exit;

  NewFormula := sgFields.Cells[2, Row];
  if InputQuery('Formula for ' + sgFields.Cells[0, Row],
                'Enter SQL expression ($1 = field value):', NewFormula) then
  begin
    sgFields.Cells[2, Row] := NewFormula;
  end;
end;

// ------------------------------------------------------------------
// Batch / Range
// ------------------------------------------------------------------
procedure TfrmBulkExport.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

function TfrmBulkExport.GetBatchSize: Integer;
begin
  Result := StrToIntDef(edtBatchSize.Text, 1000000);
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
    Result := MaxInt;   // Alle Zeilen
end;

// ------------------------------------------------------------------
// Preview SQL
// ------------------------------------------------------------------
{procedure TfrmBulkExport.btnPreviewSQLClick(Sender: TObject);
var
  i: Integer;
  SelectFields, TableName, SQL: string;
  Formula: string;
begin
  // Checkbox‑Status und Formeln aus dem Grid übernehmen
  for i := 0 to High(FFields) do
  begin
    FFields[i].Checked := chkLstFields.Checked[i];
    if chkUseFormula.Checked then
      FFields[i].Formula := sgFields.Cells[2, i + 1]
    else
      FFields[i].Formula := '';
  end;

  SelectFields := '';
  for i := 0 to High(FFields) do
  begin
    if not FFields[i].Checked then Continue;
    if SelectFields <> '' then SelectFields := SelectFields + ', ';

    if (FFields[i].Formula <> '') and chkUseFormula.Checked then
    begin
      Formula := StringReplace(FFields[i].Formula, '$1',
                               FFields[i].FieldName, [rfReplaceAll]);
      SelectFields := SelectFields + '(' + Formula + ') AS "' + FFields[i].FieldName + '"';
    end
    else
      SelectFields := SelectFields + FFields[i].FieldName;
  end;

  if SelectFields = '' then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  TableName := MakeObjectNameQuoted(comboxSourceTables.Text);
  SQL := 'SELECT ' + SelectFields + ' FROM ' + TableName;

  syneditGenerateQuery.Text := SQL;
  btnExecute.Enabled := True;
end;}

procedure TfrmBulkExport.btnPreviewSQLClick(Sender: TObject);
var
  i: Integer;
  FieldExpr, Formula, TableName, SQL: string;
  ConcatStr: string;
begin
  // Checkbox-Status und Formeln aus dem Grid übernehmen
  for i := 0 to High(FFields) do
  begin
    FFields[i].Checked := chkLstFields.Checked[i];
    if chkUseFormula.Checked then
      FFields[i].Formula := sgFields.Cells[2, i + 1]
    else
      FFields[i].Formula := '';
  end;

  ConcatStr := '';
  for i := 0 to High(FFields) do
  begin
    if not FFields[i].Checked then Continue;

    // Basis-Ausdruck für die Spalte (mit oder ohne Formel)
    if (FFields[i].Formula <> '') and chkUseFormula.Checked then
    begin
      Formula := StringReplace(FFields[i].Formula, '$1',
                               FFields[i].FieldName, [rfReplaceAll]);
      FieldExpr := '(' + Formula + ')';
    end
    else
      FieldExpr := FFields[i].FieldName;

    // CAST zu VARCHAR, damit die Konkatenation sicher klappt
    // BLOB-Felder: erst SUBSTRING, dann CAST
    if Pos('BLOB', UpperCase(FFields[i].FieldType)) > 0 then
      FieldExpr := 'CAST(SUBSTRING(' + FieldExpr + ' FROM 1 FOR 8191) AS VARCHAR(8191))'
    else
      // Alle anderen: großzügige Länge, damit nichts abgeschnitten wird
      FieldExpr := 'CAST(' + FieldExpr + ' AS VARCHAR(8191))';

    // Spaltenwert in Hochkommas einschließen und Komma anhängen
    if ConcatStr <> '' then
      ConcatStr := ConcatStr + ' || '','' || ';
    ConcatStr := ConcatStr + '''"'' || REPLACE(' + FieldExpr + ', ''"'', ''""'') || ''"''';
  end;

  if ConcatStr = '' then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  TableName := MakeObjectNameQuoted(comboxSourceTables.Text);
  SQL := 'SELECT ' + ConcatStr + ' AS result_row FROM ' + TableName;

  syneditGenerateQuery.Text := SQL;
  btnExecute.Enabled := True;
end;


// ------------------------------------------------------------------
// Export-Engine (basiert auf der SQL aus dem SynEdit)
// ------------------------------------------------------------------
procedure TfrmBulkExport.btnExecuteClick(Sender: TObject);
var
  ExportSQL: string;
begin
  ExportSQL := Trim(syneditGenerateQuery.Text);
  if ExportSQL = '' then
  begin
    ShowMessage('No SQL to execute.');
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
    Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    if Execute then
      edtExportFileName.Text := FileName;
  finally
    Free;
  end;
end;

procedure TfrmBulkExport.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmBulkExport.btnAddToQueueClick(Sender: TObject);
begin
  MessageDlg('Queue feature coming soon!', mtInformation, [mbOK], 0);
end;

procedure TfrmBulkExport.CancelClick(Sender: TObject);
begin
  FCancelled := True;
end;

procedure TfrmBulkExport.DoBulkExport(const ASQL: string);
const
  LINE_BUFFER_SIZE = 10000;
var
  TotalRows, BatchSize, Exported, StartRow: Integer;
  FromRow, ToRow: Integer;
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
  SQL: string;
  BaseSQL: string;
  FileStream: TBufferedFileStream;
  LineBuffer: TStringList;
  BufferCount: Integer;

  procedure FlushBuffer;
  var
    j: Integer;
    BufLine: RawByteString;
  begin
    for j := 0 to LineBuffer.Count - 1 do
    begin
      BufLine := LineBuffer[j];
      FileStream.Write(BufLine[1], Length(BufLine));
      FileStream.Write(LineEnd[1], Length(LineEnd));
    end;
    LineBuffer.Clear;
    BufferCount := 0;
  end;

  procedure UpdateProgress;
  begin
    ProgressBar.Position := Exported;
    ProgressLabel.Caption := Format('Exported %s of %s rows',
      [FormatFloat('#,##0', Exported), FormatFloat('#,##0', TotalRows)]);
    LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
      '   |   ' + FormatFloat('#,##0',
        Round(Exported / Max(0.001, (Now - StartTime) * 86400))) + ' rows/sec';
    Application.ProcessMessages;
  end;

begin
  BatchSize := GetBatchSize;
  FromRow := GetFromRow;
  ToRow := GetToRow;

  // Eigene DB-Verbindung
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

  BaseSQL := Copy(ASQL, Pos('SELECT ', UpperCase(ASQL)) + 7, MaxInt);
  LineEnd := sLineBreak;

  LineBuffer := TStringList.Create;
  LineBuffer.Capacity := LINE_BUFFER_SIZE;
  BufferCount := 0;

  ProgressForm := TForm.Create(nil);
  try
    // === Progress-Formular aufbauen ===
    ProgressForm.Width := 520;
    ProgressForm.Height := 220;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;
    ProgressForm.Caption := 'Bulk Export';

    // Phase-Label (Counting / Preparing / Exporting / Finalizing)
    LblPhase := TLabel.Create(ProgressForm);
    LblPhase.Parent := ProgressForm;
    LblPhase.Left := 16;
    LblPhase.Top := 16;
    LblPhase.Caption := 'Counting rows...';
    LblPhase.Font.Style := [fsBold];
    LblPhase.Width := 480;

    // Zeilen-Label
    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 42;
    ProgressLabel.Caption := 'Please wait...';
    ProgressLabel.Width := 480;

    // Progressbar
    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 70;
    ProgressBar.Width := 480;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;
    ProgressBar.Style := pbstMarquee;   // ← animiert, solange TotalRows unbekannt

    // Elapsed
    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 100;
    LblElapsed.Caption := 'Elapsed: 00:00:00';
    LblElapsed.Width := 480;

    // Cancel
    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 200;
    BtnCancel.Top := 140;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelClick;

    // === SOFORT SICHTBAR ===
    ProgressForm.Show;
    ProgressForm.BringToFront;
    Application.ProcessMessages;
    Sleep(50);
    Application.ProcessMessages;

    FCancelled := False;
    StartTime := Now;
    Exported := 0;
    StartRow := FromRow;

    // ============================================================
    // Phase 1: Zeilen zählen
    // ============================================================
    LblPhase.Caption := 'Counting rows...';
    Application.ProcessMessages;

    TotalRows := 0;
    try
      Q.SQL.Text := 'SELECT COUNT(*) FROM ' + Trim(comboxSourceTables.Text);
      Q.ExecQuery;
      if not Q.EOF then
        TotalRows := Q.Fields[0].AsInteger;
      Q.Close;
    except
      TotalRows := 0;
    end;

    if (FromRow > 1) or (ToRow < TotalRows) then
    begin
      if ToRow > TotalRows then ToRow := TotalRows;
      TotalRows := ToRow - FromRow + 1;
    end;

    // Jetzt kennen wir die Zeilenzahl → Progressbar umstellen
    ProgressBar.Style := pbstNormal;
    ProgressBar.Max := TotalRows;
    ProgressBar.Position := 0;

    // ============================================================
    // Phase 2: Datei öffnen
    // ============================================================
    LblPhase.Caption := 'Opening output file...';
    Application.ProcessMessages;

    FileStream := TBufferedFileStream.Create(edtExportFileName.Text, fmCreate, 1048576);

    // ============================================================
    // Phase 3: Export
    // ============================================================
    LblPhase.Caption := 'Exporting data...';
    LblElapsed.Caption := 'Elapsed: 00:00:00';
    Application.ProcessMessages;

    try
      repeat
        SQL := 'SELECT FIRST ' + IntToStr(BatchSize) +
               ' SKIP ' + IntToStr(StartRow - 1) + ' ' + BaseSQL;
        Q.Close;
        Q.SQL.Text := SQL;
        Q.ExecQuery;

        if Q.EOF then Break;

        while not Q.EOF do
        begin
          if FCancelled then Break;

          Line := Q.Fields[0].AsString;
          LineBuffer.Add(Line);
          Inc(BufferCount);
          Inc(Exported);

          if BufferCount >= LINE_BUFFER_SIZE then
          begin
            FlushBuffer;
            UpdateProgress;
          end;

          Q.Next;
        end;

        StartRow := StartRow + BatchSize;

      until (Exported >= TotalRows) or FCancelled;

      // Rest schreiben
      if BufferCount > 0 then
        FlushBuffer;

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
  finally
    LineBuffer.Free;
    Q.Free;
    Trans.Rollback;
    DB.Connected := False;
    DB.Free;
    Trans.Free;
    ProgressForm.Free;
  end;

  // Statistik
  if FCancelled then
    ShowMessage(Format('Export cancelled!' + sLineBreak +
                       'Rows: %s' + sLineBreak +
                       'Time: %s' + sLineBreak +
                       'Speed: %s rows/sec',
                       [FormatFloat('#,##0', Exported),
                        FormatDateTime('hh:nn:ss', EndTime - StartTime),
                        FormatFloat('#,##0', Round(Exported / Max(0.001, (EndTime - StartTime) * 86400)))]))
  else
    ShowMessage(Format('Export completed!' + sLineBreak +
                       'Rows: %s' + sLineBreak +
                       'Time: %s' + sLineBreak +
                       'Speed: %s rows/sec' + sLineBreak +
                       'Batch size: %s' + sLineBreak +
                       'Formula used: %s',
                       [FormatFloat('#,##0', Exported),
                        FormatDateTime('hh:nn:ss', EndTime - StartTime),
                        FormatFloat('#,##0', Round(Exported / Max(0.001, (EndTime - StartTime) * 86400))),
                        FormatFloat('#,##0', BatchSize),
                        BoolToStr(chkUseFormula.Checked, 'Yes', 'No')]));
end;

end.
