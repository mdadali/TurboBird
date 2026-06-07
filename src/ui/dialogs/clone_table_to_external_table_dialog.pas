unit clone_table_to_external_table_dialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IBDatabase,
  IBQuery, SynEdit, SynHighlighterSQL, ComCtrls, ExtCtrls,

  turbocommon,
  fsimpleobjextractor;

type

  { TfmCloneToExternalTable }

  TfmCloneToExternalTable = class(TForm)
    btnBrowseFile: TButton;
    btnOK: TButton;
    btnCreateQuery: TButton;
    btnCopy: TButton;
    btnCreateExtTable: TButton;
    btnReset: TButton;
    edtDefaultBatchSize: TEdit;
    edtExternalFileName: TEdit;
    edtExtTableName: TEdit;
    grBoxTables: TGroupBox;
    grBoxQuery: TGroupBox;
    grBoxCopy: TGroupBox;
    IBDatabase1: TIBDatabase;
    IBQuery1: TIBQuery;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SaveDialog1: TSaveDialog;
    SynEditExtTableQuery: TSynEdit;
    SynEditInsertQuery: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure btnBrowseFileClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnCreateExtTableClick(Sender: TObject);
    procedure btnCreateQueryClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FDBIndex: integer;
    FTableName: string;
    FUsedFields: TStringList;       // Feldname=TYPE,...
    FUsedFieldNames: TStringList;   // Nur Feldnamen
    procedure CloneTableAsExternal;
    procedure GenerateInsertQuery;
    procedure CopyDataWithProgress;
  public
    procedure Init(const ATableName: string; ADBIndex: Integer);
  end;

var
  fmCloneToExternalTable: TfmCloneToExternalTable;

implementation

{$R *.lfm}

{ TfmCloneToExternalTable }

procedure TfmCloneToExternalTable.FormCreate(Sender: TObject);
begin
  FUsedFields := TStringList.Create;
  FUsedFieldNames := TStringList.Create;
  SaveDialog1.Filter := 'External Table Files|*.dat;*.txt;*.csv|All Files|*.*';
  SaveDialog1.DefaultExt := 'dat';
end;

procedure TfmCloneToExternalTable.Init(const ATableName: string; ADBIndex: Integer);
begin
  FDBIndex   := ADBIndex;
  FTableName := ATableName;
  edtExtTableName.Text := 'EXT_' + FTableName;
  FUsedFields.Clear;
  FUsedFieldNames.Clear;

  SynEditExtTableQuery.Lines.Clear;
  SynEditInsertQuery.Lines.Clear;

  btnCreateExtTable.Enabled := False;
  btnCopy.Enabled := False;

  if IBQuery1.Active then
    IBQuery1.Close;
  if IBTransaction1.InTransaction then
    IBTransaction1.Commit;
  if IBDatabase1.Connected then
    IBDatabase1.Connected := False;

  AssignIBDatabase(RegisteredDatabases[FDBIndex].IBDatabase, IBDatabase1);
  IBTransaction1.Params.Assign(RegisteredDatabases[FDBIndex].IBTransaction.Params);

  try
    ConnectDBPrepared(IBDatabase1, IBTransaction1, FDBIndex, IBTransaction1.Params);
  except
    on E: Exception do
    begin
      MessageDlg('Could not connect to database:' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
      Exit;
    end;
  end;
end;

procedure TfmCloneToExternalTable.btnBrowseFileClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    edtExternalFileName.Text := SaveDialog1.FileName;
end;

// ============================================================================
// CREATE TABLE SQL generieren
// ============================================================================
procedure TfmCloneToExternalTable.btnCreateQueryClick(Sender: TObject);
begin
  try
    CloneTableAsExternal;
    btnCreateExtTable.Enabled := True;
    btnCopy.Enabled := False;
    SynEditInsertQuery.Lines.Clear;
  except
    on E: Exception do
    begin
      MessageDlg('Error generating query:' + sLineBreak + E.Message, mtError, [mbOK], 0);
      btnCreateExtTable.Enabled := False;
    end;
  end;
end;

// ============================================================================
// CREATE TABLE ausführen
// ============================================================================
procedure TfmCloneToExternalTable.btnCreateExtTableClick(Sender: TObject);
begin
  try
    if SynEditExtTableQuery.Lines.Count = 0 then
    begin
      MessageDlg('No CREATE TABLE SQL to execute.', mtWarning, [mbOK], 0);
      Exit;
    end;

    IBQuery1.Close;
    IBQuery1.SQL.Assign(SynEditExtTableQuery.Lines);

    if not IBTransaction1.InTransaction then
      IBTransaction1.StartTransaction;

    IBQuery1.ExecSQL;
    IBTransaction1.Commit;

    // INSERT generieren
    GenerateInsertQuery;
    btnCopy.Enabled := True;

    MessageDlg(
      'External Table created successfully!' + sLineBreak +
      sLineBreak +
      'The INSERT query has been generated.' + sLineBreak +
      sLineBreak +
      'You can now copy the data by clicking' + sLineBreak +
      'the [Copy] button.',
      mtInformation, [mbOK], 0
    );

  except
    on E: Exception do
    begin
      if IBTransaction1.InTransaction then
        IBTransaction1.Rollback;
      MessageDlg('Create External Table failed:' + sLineBreak + E.Message, mtError, [mbOK], 0);
      btnCopy.Enabled := False;
    end;
  end;
end;

// ============================================================================
// INSERT INTO ... SELECT generieren
// ============================================================================
procedure TfmCloneToExternalTable.GenerateInsertQuery;
var
  FieldList: string;
  i, SpacePos: Integer;
  FieldName: string;
begin
  // Nur Feldnamen aus FUsedFields extrahieren
  FUsedFieldNames.Clear;

  for i := 0 to FUsedFields.Count - 1 do
  begin
    SpacePos := Pos(' ', FUsedFields[i]);
    if SpacePos > 0 then
      FieldName := Copy(FUsedFields[i], 1, SpacePos - 1)
    else
      FieldName := FUsedFields[i];
    FUsedFieldNames.Add(FieldName);
  end;

  // Kommagetrennte Feldliste
  FieldList := '';
  for i := 0 to FUsedFieldNames.Count - 1 do
  begin
    if FieldList <> '' then
      FieldList := FieldList + ', ';
    FieldList := FieldList + FUsedFieldNames[i];
  end;

  SynEditInsertQuery.Lines.Clear;
  SynEditInsertQuery.Lines.Add('INSERT INTO ' + edtExtTableName.Text + ' (' + FieldList + ')');
  SynEditInsertQuery.Lines.Add('SELECT ' + FieldList);
  SynEditInsertQuery.Lines.Add('FROM ' + FTableName + ';');
end;

// ============================================================================
// COPY DATA mit Progress-Fenster
// ============================================================================
procedure TfmCloneToExternalTable.btnCopyClick(Sender: TObject);
begin
  if SynEditInsertQuery.Lines.Count = 0 then
  begin
    MessageDlg('No INSERT SQL to execute.', mtWarning, [mbOK], 0);
    Exit;
  end;

  CopyDataWithProgress;
end;

procedure TfmCloneToExternalTable.CopyDataWithProgress;
var
  ProgressForm: TForm;
  ProgressLabel, lblElapsed: TLabel;
  ProgressBar: TProgressBar;
  StartTime, EndTime: TDateTime;
  RecCount, BatchSize, BatchCount, BatchIndex, FromRow, ToRow: Integer;
  RowsPerSec: Double;
  FieldList: string;
  CountQuery: TIBQuery;
begin
  BatchSize := StrToIntDef(edtDefaultBatchSize.Text, 500000);

  // ========================================================================
  // Record Count ermitteln
  // ========================================================================
  CountQuery := TIBQuery.Create(nil);
  try
    CountQuery.Database := IBDatabase1;
    CountQuery.Transaction := IBTransaction1;
    CountQuery.AllowAutoActivateTransaction := true;
    CountQuery.SQL.Text := 'SELECT COUNT(*) FROM ' + FTableName;
    CountQuery.Open;
    RecCount := CountQuery.Fields[0].AsInteger;
    CountQuery.Close;
  finally
    CountQuery.Free;
  end;

  if RecCount = 0 then
  begin
    MessageDlg('Source table is empty. Nothing to copy.', mtInformation, [mbOK], 0);
    Exit;
  end;

  BatchCount := (RecCount + BatchSize - 1) div BatchSize;

  // ========================================================================
  // Feldliste aus FUsedFieldNames
  // ========================================================================
  FieldList := '';
  for FromRow := 0 to FUsedFieldNames.Count - 1 do
  begin
    if FieldList <> '' then
      FieldList := FieldList + ', ';
    FieldList := FieldList + FUsedFieldNames[FromRow];
  end;

  // ========================================================================
  // Progress Window
  // ========================================================================
  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.FormStyle := fsStayOnTop;
    ProgressForm.Caption := 'Copying data...';
    ProgressForm.Width := 500;
    ProgressForm.Height := 180;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := Format('Total Records: %d', [RecCount]);

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 45;
    ProgressBar.Width := 460;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := RecCount;

    lblElapsed := TLabel.Create(ProgressForm);
    lblElapsed.Parent := ProgressForm;
    lblElapsed.Left := 16;
    lblElapsed.Top := 80;

    ProgressForm.Show;
    Application.ProcessMessages;

    // ========================================================================
    // Batch-Verarbeitung
    // ========================================================================
    StartTime := Now;

    for BatchIndex := 0 to BatchCount - 1 do
    begin
      FromRow := (BatchIndex * BatchSize) + 1;
      ToRow := (BatchIndex + 1) * BatchSize;
      if ToRow > RecCount then
        ToRow := RecCount;

      IBQuery1.Close;
      IBQuery1.SQL.Text :=
        'INSERT INTO ' + edtExtTableName.Text + ' (' + FieldList + ')' + sLineBreak +
        'SELECT FIRST ' + IntToStr(ToRow - FromRow + 1) + ' SKIP ' + IntToStr(FromRow - 1) + ' ' +
        FieldList + sLineBreak +
        'FROM ' + FTableName;

      if not IBTransaction1.InTransaction then
        IBTransaction1.StartTransaction;

      IBQuery1.ExecSQL;
      IBTransaction1.CommitRetaining;
      Application.ProcessMessages;

      ProgressBar.Position := ToRow;
      ProgressLabel.Caption := Format('Copied %d of %d rows...', [ToRow, RecCount]);
      lblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime);
      Application.ProcessMessages;
    end;

    IBTransaction1.Commit;
    Application.ProcessMessages;

    // ========================================================================
    // Finish
    // ========================================================================
    EndTime := Now;
    RowsPerSec := RecCount / ((EndTime - StartTime) * 24 * 60 * 60);

    ProgressBar.Position := ProgressBar.Max;
    ProgressLabel.Caption := Format('Copy complete. %d rows (%.0f rows/sec).', [RecCount, RowsPerSec]);

    MessageDlg(
      Format('Data copy completed!' + sLineBreak + sLineBreak +
             'Rows copied: %d' + sLineBreak +
             'Time: %s' + sLineBreak +
             'Speed: %.0f rows/sec',
             [RecCount, FormatDateTime('hh:nn:ss', EndTime - StartTime), RowsPerSec]),
      mtInformation, [mbOK], 0);

  finally
    ProgressForm.Free;
  end;
end;

// ============================================================================
// Reset
// ============================================================================
procedure TfmCloneToExternalTable.btnResetClick(Sender: TObject);
begin
  Init(FTableName, FDBIndex);
end;

// ============================================================================
// Form schließen
// ============================================================================
procedure TfmCloneToExternalTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FUsedFields.Free;
  FUsedFieldNames.Free;

  if IBQuery1.Active then
    IBQuery1.Close;
  if IBTransaction1.InTransaction then
    IBTransaction1.Commit;
  if IBDatabase1.Connected then
    IBDatabase1.Connected := False;

  CloseAction := caFree;
end;

// ============================================================================
// CloneTableAsExternal (unverändert)
// ============================================================================
procedure TfmCloneToExternalTable.CloneTableAsExternal;
var
  Items, Warnings: TStringList;
  SQL: string;
  i: Integer;
  Extractor: TSimpleObjExtractor;
  Line, FieldType, CleanType: string;
  SpacePos: Integer;
  FieldName: string;
begin
  if not IBDatabase1.Connected then
    Exit;
  if not IBTransaction1.InTransaction then
    IBTransaction1.StartTransaction;

  try
    Screen.Cursor := crSQLWait;
    Application.ProcessMessages;

    Items    := TStringList.Create;
    Warnings := TStringList.Create;
    Extractor := TSimpleObjExtractor.Create(FDBIndex);
    try
      Extractor.ExtractTableFieldsForExternalTable(FTableName, Items, AlwaysQuoteIdentifiers, ' ');

      if Items.Count = 0 then
      begin
        MessageDlg('Could not retrieve table fields.', mtError, [mbOK], 0);
        Exit;
      end;

      FUsedFields.Clear;

      // Problemfelder aus Items entfernen
      // Problemfelder aus Items entfernen (BLOB, ARRAY, COMPUTED BY, INT128, DECFLOAT)
      for i := Items.Count - 1 downto 0 do
      begin
        Line := Items[i];

        // Typ extrahieren (alles nach erstem Space)
        SpacePos := Pos(' ', Line);
        if SpacePos > 0 then
          FieldType := Trim(Copy(Line, SpacePos + 1, MaxInt))
        else
          FieldType := '';

        // Letztes Komma entfernen
        if (Length(FieldType) > 0) and (FieldType[Length(FieldType)] = ',') then
          FieldType := Copy(FieldType, 1, Length(FieldType) - 1);

        CleanType := UpperCase(Trim(FieldType));
        if Pos(' ', CleanType) > 0 then
          CleanType := Copy(CleanType, 1, Pos(' ', CleanType) - 1);
        if Pos('(', CleanType) > 0 then
          CleanType := Copy(CleanType, 1, Pos('(', CleanType) - 1);

        // BOOLEAN → SMALLINT konvertieren (nicht löschen!)
        if CleanType = 'BOOLEAN' then
        begin
          Items[i] := StringReplace(Items[i], 'BOOLEAN', 'SMALLINT', [rfIgnoreCase]);
          // Feldname aus Items[i] extrahieren für Warnung
          SpacePos := Pos(' ', Items[i]);
          if SpacePos > 0 then
            FieldName := Copy(Items[i], 1, SpacePos - 1)
          else
            FieldName := Items[i];
          Warnings.Add('Column "' + FieldName + '" is BOOLEAN - converted to SMALLINT (0=false, 1=true).');
        end

        // BLOB → ignorieren
        else if CleanType = 'BLOB' then
        begin
          Warnings.Add('-- ' + Items[i]);
          Items.Delete(i);
        end

        // ARRAY → ignorieren
        else if Pos('[', FieldType) > 0 then
        begin
          Warnings.Add('-- ' + Items[i]);
          Items.Delete(i);
        end

        // COMPUTED BY → ignorieren
        else if Pos('COMPUTED', UpperCase(FieldType)) > 0 then
        begin
          Warnings.Add('-- ' + Items[i]);
          Items.Delete(i);
        end

        // INT128 / DECFLOAT → ignorieren
        else if (CleanType = 'INT128') or (CleanType = 'DECFLOAT(16)') or (CleanType = 'DECFLOAT(34)') then
        begin
          Warnings.Add('-- ' + Items[i]);
          Items.Delete(i);
        end

        // Gültiges Feld → in FUsedFields speichern
        else
          FUsedFields.Add(Items[i]);
      end;

      if Items.Count = 0 then
      begin
        MessageDlg('No valid fields for external table! All fields are BLOB, ARRAY or unsupported types.',
                   mtError, [mbOK], 0);
        Exit;
      end;

      // SQL generieren
      SQL := 'CREATE TABLE ' + edtExtTableName.Text +
             ' EXTERNAL FILE ''' + edtExternalFileName.Text + ''' (' + sLineBreak;

      for i := 0 to Items.Count - 1 do
        SQL := SQL + '  ' + Items[i] + sLineBreak;

      SQL := SQL + ');';

      if Warnings.Count > 0 then
      begin
        Warnings.Insert(0, '-- Unsupported Fields:');
        Warnings.Add(sLineBreak);
      end;

      SQL := Warnings.Text + SQL;

      if Trim(SQL) <> '' then
        SynEditExtTableQuery.Lines.Text := Trim(SQL);

    finally
      Extractor.Free;
      Items.Free;
      Warnings.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
