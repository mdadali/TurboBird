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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);
  private
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

    procedure LoadServerList;
    procedure LoadDBList;
    procedure LoadTableList;
    procedure LoadFields;
    function  GetBatchSize: Integer;
    function  GetFromRow: Integer;
    function  GetToRow: Integer;
    procedure CancelClick(Sender: TObject);
    procedure DoBulkExport(const ASQL: string);
  public

  end;

//var
  //frmBulkExport: TfrmBulkExport;

implementation

{$R *.lfm}

{ TfrmBulkExport }

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

  edtBatchSize.Text := IntToStr(DefaultBatchSize);
end;

procedure TfrmBulkExport.FormShow(Sender: TObject);
begin
 // frmThemeSelector.btnApplyClick(self);
  LoadServerList;
end;

// ------------------------------------------------------------------
// Source-Auswahl
// ------------------------------------------------------------------
procedure TfrmBulkExport.LoadServerList;
var
  List: TStringList;
begin
  List := GetServerListFromTreeView;
  try
    comboxSourceServer.Items.Assign(List);
    if comboxSourceServer.Items.Count > 0 then
    begin
      comboxSourceServer.ItemIndex := 0;
      comboxSourceServerChange(nil);
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmBulkExport.comboxSourceServerChange(Sender: TObject);
begin
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
  LoadFields;
  syneditGenerateQuery.Clear;      // Alte Abfrage löschen
  btnExecute.Enabled := False;     // Execute erst wieder nach Preview möglich
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
    FieldExpr := 'CAST(' + FieldExpr + ' AS VARCHAR(' + IntToStr(CSVDefaultFieldLength) + '))';

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

{procedure TfrmBulkExport.DoBulkExport(const ASQL: string);
var
  TotalRows, BatchSize, Exported, StartRow, i: Integer;
  FromRow, ToRow: Integer;
  ProgressForm: TForm;
  ProgressLabel, LblElapsed: TLabel;
  ProgressBar: TProgressBar;
  btnCancel: TButton;
  StartTime, EndTime: TDateTime;
  Q: TIBQuery;
  DB: TIBDatabase;
  Trans: TIBTransaction;
  Line, Value: string;
  SQL: string;
  FileStream: TFileStream;
  Buffer: TStringList;
  HeaderWritten: Boolean;
  Cancel: Boolean;
begin
  BatchSize := GetBatchSize;
  FromRow := GetFromRow;
  ToRow := GetToRow;

  // Eigene Datenbankverbindung aufbauen (mit Passwort-Cache)
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

  Q := TIBQuery.Create(nil);
  Q.Database := DB;
  Q.Transaction := Trans;

  // Gesamtzahl ermitteln (für Fortschritt)
  TotalRows := 0;
  try
    Q.SQL.Text := 'SELECT COUNT(*) FROM ' + Trim(comboxSourceTables.Text);
    Q.Open;
    TotalRows := Q.Fields[0].AsInteger;
    Q.Close;
  except
    TotalRows := 0;
  end;

  // Range anwenden
  if (FromRow > 1) or (ToRow < TotalRows) then
  begin
    if ToRow > TotalRows then ToRow := TotalRows;
    TotalRows := ToRow - FromRow + 1;
  end;

  // Fortschrittsdialog
  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.Width := 500;
    ProgressForm.Height := 200;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;
    ProgressForm.Caption := 'Bulk Export';

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := 'Preparing...';

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 45;
    ProgressBar.Width := 460;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := TotalRows;
    ProgressBar.Style := pbstNormal;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 80;

    btnCancel := TButton.Create(ProgressForm);
    btnCancel.Parent := ProgressForm;
    btnCancel.Caption := 'Cancel';
    btnCancel.Left := 190;
    btnCancel.Top := 120;
    btnCancel.Width := 100;
    btnCancel.OnClick := @CancelClick;

    ProgressForm.Show;
    Application.ProcessMessages;

    FCancelled := False;
    StartTime := Now;
    Exported := 0;
    StartRow := FromRow;
    HeaderWritten := False;

    FileStream := TFileStream.Create(edtExportFileName.Text, fmCreate);
    try
      // Batchweise exportieren
      repeat
        SQL := 'SELECT FIRST ' + IntToStr(BatchSize) +
               ' SKIP ' + IntToStr(StartRow - 1) + ' ' +
               Copy(ASQL, Pos('SELECT ', UpperCase(ASQL)) + 7, MaxInt);
        Q.Close;
        Q.SQL.Text := SQL;
        Q.Open;

        if Q.RecordCount <= 0 then Break;

        Buffer := TStringList.Create;
        try
          // Header schreiben, falls erste Batch
          if not HeaderWritten then
          begin
            Line := '';
            for i := 0 to Q.FieldCount - 1 do
            begin
              if Line <> '' then Line := Line + ',';
              Line := Line + '"' + Q.Fields[i].FieldName + '"';
            end;
            Buffer.Add(Line);
            HeaderWritten := True;
          end;

          // Datenzeilen
          while not Q.EOF do
          begin
            if FCancelled then Break;

            Line := '';
            for i := 0 to Q.FieldCount - 1 do
            begin
              if Line <> '' then Line := Line + ',';
              Value := Q.Fields[i].AsString;
              Line := Line + '"' + StringReplace(Value, '"', '""', [rfReplaceAll]) + '"';
            end;
            Buffer.Add(Line);
            Inc(Exported);
            Q.Next;
          end;

          // Batch in Datei schreiben
          Buffer.SaveToStream(FileStream);
          Application.ProcessMessages;
        finally
          Buffer.Free;
        end;

        StartRow := StartRow + Q.RecordCount;
        Q.Close;

        ProgressBar.Position := Exported;
        ProgressLabel.Caption := Format('Exported %d of %d rows...', [Exported, TotalRows]);
        LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime);
        Application.ProcessMessages;

      until (Exported >= TotalRows) or FCancelled;

    finally
      FileStream.Free;
    end;

    EndTime := Now;
  finally
    Q.Free;
    Trans.Rollback;
    DB.Connected := False;
    DB.Free;
    Trans.Free;
    ProgressForm.Free;
  end;

  if FCancelled then
  ShowMessage(Format('Export canceled!' + sLineBreak +
                     'Rows: %d' + sLineBreak +
                     'Time: %s' + sLineBreak +
                     'Speed: %.0f rows/sec',
                     [Exported, FormatDateTime('hh:nn:ss', EndTime - StartTime),
                      Exported / Max(1, (EndTime - StartTime) * 86400)]))
  else
    ShowMessage(Format('Export completed!' + sLineBreak +
                       'Rows: %d' + sLineBreak +
                       'Time: %s' + sLineBreak +
                       'Speed: %.0f rows/sec',
                       [Exported, FormatDateTime('hh:nn:ss', EndTime - StartTime),
                        Exported / Max(1, (EndTime - StartTime) * 86400)]));
end; }

procedure TfrmBulkExport.DoBulkExport(const ASQL: string);
var
  TotalRows, BatchSize, Exported, StartRow, i: Integer;
  FromRow, ToRow: Integer;
  ProgressForm: TForm;
  ProgressLabel, LblElapsed: TLabel;
  ProgressBar: TProgressBar;
  BtnCancel: TButton;
  StartTime, EndTime: TDateTime;
  DB: TIBDatabase;
  Trans: TIBTransaction;
  Q: TIBSQL;                     // ← TIBSQL statt TIBQuery
  Line: string;
  SQL: string;
  FileStream: TBufferedFileStream;
  LineBytes: RawByteString;
begin
  BatchSize := GetBatchSize;
  FromRow := GetFromRow;
  ToRow := GetToRow;

  // Eigene Datenbankverbindung aufbauen (wie gehabt)
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

  // Gesamtzahl ermitteln (für Fortschritt)
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

  // Fortschrittsdialog (wie gehabt)
  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.Width := 500;
    ProgressForm.Height := 200;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;
    ProgressForm.Caption := 'Bulk Export';

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := 'Preparing...';

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 45;
    ProgressBar.Width := 460;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := TotalRows;
    ProgressBar.Style := pbstNormal;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 80;

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 190;
    BtnCancel.Top := 120;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelClick;

    ProgressForm.Show;
    Application.ProcessMessages;

    FCancelled := False;
    StartTime := Now;
    Exported := 0;
    StartRow := FromRow;

    //FileStream := TBufferedFileStream.Create(edtExportFileName.Text, fmCreate);
    FileStream := TBufferedFileStream.Create(edtExportFileName.Text, fmCreate, 1048576);
    try
      repeat
        // Batch-SQL: FIRST … SKIP … + die bereits fertige Projektion
        SQL := 'SELECT FIRST ' + IntToStr(BatchSize) +
               ' SKIP ' + IntToStr(StartRow - 1) + ' ' +
               Copy(ASQL, Pos('SELECT ', UpperCase(ASQL)) + 7, MaxInt);
        Q.Close;
        Q.SQL.Text := SQL;
        Q.ExecQuery;

        if Q.EOF then Break;

        while not Q.EOF do
        begin
          if FCancelled then Break;

          Line := Q.Fields[0].AsString;
          // Jede Zeile direkt in den Stream schreiben
          LineBytes := Line + sLineBreak;
          FileStream.Write(LineBytes[1], Length(LineBytes));
          Inc(Exported);
          Q.Next;
        end;

        StartRow := StartRow + BatchSize;
        // Falls die letzte Batch weniger Zeilen hatte, ist EOF bereits erreicht
        // und der nächste Durchlauf wird mit EOF sofort beendet.

        ProgressBar.Position := Exported;
        ProgressLabel.Caption := Format('Exported %d of %d rows...', [Exported, TotalRows]);
        LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime);
        Application.ProcessMessages;

      until (Exported >= TotalRows) or FCancelled;

    finally
      FileStream.Flush;   // Restliche Bytes aus dem Puffer schreiben
      FileStream.Free;
    end;

    EndTime := Now;
  finally
    Q.Free;
    Trans.Rollback;
    DB.Connected := False;
    DB.Free;
    Trans.Free;
    ProgressForm.Free;
  end;

  // Statistik immer anzeigen (auch bei Abbruch)
  if FCancelled then
    ShowMessage(Format('Export cancelled!' + sLineBreak +
                       'Rows: %d' + sLineBreak +
                       'Time: %s' + sLineBreak +
                       'Speed: %.0f rows/sec' + sLineBreak +
                       'Batch size: %d' + sLineBreak +
                       'Formula used: %s',
                       [Exported, FormatDateTime('hh:nn:ss', EndTime - StartTime),
                        Exported / Max(1, (EndTime - StartTime) * 86400),
                        BatchSize,
                        BoolToStr(chkUseFormula.Checked, 'Yes', 'No')]))
  else
    ShowMessage(Format('Export completed!' + sLineBreak +
                       'Rows: %d' + sLineBreak +
                       'Time: %s' + sLineBreak +
                       'Speed: %.0f rows/sec' + sLineBreak +
                       'Batch size: %d' + sLineBreak +
                       'Formula used: %s',
                       [Exported, FormatDateTime('hh:nn:ss', EndTime - StartTime),
                        Exported / Max(1, (EndTime - StartTime) * 86400),
                        BatchSize,
                        BoolToStr(chkUseFormula.Checked, 'Yes', 'No')]));

end;

end.
