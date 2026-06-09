unit clone_table_to_external_table_dialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IBDatabase,
  IBQuery, SynEdit, SynHighlighterSQL, ComCtrls, ExtCtrls,

  turbocommon,
  fsimpleobjextractor,
  uCopyTable;


type
  { TfmCloneToExternalTable }

  TfmCloneToExternalTable = class(TForm)
    btnBrowseFile: TButton;
    btnCreateQuery: TButton;
    btnCopy: TButton;
    btnCreateExtTable: TButton;
    btnOK: TButton;
    btnReset: TButton;
    edtTo: TEdit;
    edtFrom: TEdit;
    edtBatchSize: TEdit;
    edtExternalFileName: TEdit;
    edtExtTableName: TEdit;
    grBoxTables: TGroupBox;
    grBoxQuery: TGroupBox;
    grBoxCopy: TGroupBox;
    grboxCopyOptions: TGroupBox;
    IBDatabase1: TIBDatabase;
    IBQuery1: TIBQuery;
    IBTransaction1: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pnlBottom: TPanel;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
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
    procedure rbAllRowsChange(Sender: TObject);
  private
    FDBIndex: integer;
    FTableName: string;
    FUsedFields: TStringList;
    FUsedFieldNames: TStringList;

    procedure CloneTableAsExternal;
    procedure GenerateInsertQuery;
    procedure CopyDataWithProgress;
    function ExternalTableExists: Boolean;
    function ExternalFileExists: Boolean;
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

procedure TfmCloneToExternalTable.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
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
// Prüft ob die External Table bereits existiert
// ============================================================================
function TfmCloneToExternalTable.ExternalTableExists: Boolean;
var
  Q: TIBQuery;
begin
  Result := False;
  Q := TIBQuery.Create(nil);
  try
    Q.Database := IBDatabase1;
    Q.Transaction := IBTransaction1;
    Q.AllowAutoActivateTransaction := True;
    Q.SQL.Text :=
      'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
      'WHERE UPPER(RDB$RELATION_NAME) = :T AND RDB$EXTERNAL_FILE IS NOT NULL';
    Q.ParamByName('T').AsString := UpperCase(edtExtTableName.Text);
    Q.Open;
    Result := not Q.EOF;
    Q.Close;
  finally
    Q.Free;
  end;
end;

// ============================================================================
// Prüft ob die External-Datei bereits existiert
// ============================================================================
function TfmCloneToExternalTable.ExternalFileExists: Boolean;
begin
  Result := FileExists(edtExternalFileName.Text);
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
var
  LocalDoCreate: Boolean;
begin
  try
    if SynEditExtTableQuery.Lines.Count = 0 then
    begin
      MessageDlg('No CREATE TABLE SQL to execute.', mtWarning, [mbOK], 0);
      Exit;
    end;

    LocalDoCreate := True;  // Default: CREATE ausführen

    // Prüfen ob External Table bereits existiert
    if ExternalTableExists then
    begin
      if MessageDlg(
           'External Table "' + edtExtTableName.Text + '" already exists!' + sLineBreak +
           sLineBreak +
           'Do you want to drop and recreate it?' + sLineBreak +
           sLineBreak +
           'Yes = Drop and recreate' + sLineBreak +
           'No  = Use existing table',
           mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        IBQuery1.Close;
        IBQuery1.SQL.Text := 'DROP TABLE ' + edtExtTableName.Text;
        IBQuery1.ExecSQL;
        IBTransaction1.Commit;
        // DoCreate bleibt True
      end
      else
        LocalDoCreate := False;  // Bestehende Tabelle nutzen
    end;

    // Prüfen ob External-Datei bereits existiert (nur wenn CREATE ausgeführt wird)
    if LocalDoCreate and ExternalFileExists then
    begin
      if MessageDlg(
           'External File "' + edtExternalFileName.Text + '" already exists!' + sLineBreak +
           sLineBreak +
           'Do you want to recreate it?' + sLineBreak +
           sLineBreak +
           'Yes = Recreate file (existing data will be lost)' + sLineBreak +
           'No  = Use existing file',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      begin
        // User will bestehende Datei nutzen → CREATE trotzdem ausführen
      end;
    end;

    // CREATE TABLE ausführen
    if LocalDoCreate then
    begin
      IBQuery1.Close;
      IBQuery1.SQL.Assign(SynEditExtTableQuery.Lines);

      if not IBTransaction1.InTransaction then
        IBTransaction1.StartTransaction;

      IBQuery1.ExecSQL;
      IBTransaction1.Commit;
    end;

    // INSERT generieren (immer, egal ob CREATE oder nicht)
    GenerateInsertQuery;
    btnCopy.Enabled := True;

    if LocalDoCreate then
      MessageDlg(
        'External Table created successfully!' + sLineBreak +
        sLineBreak +
        'The INSERT query has been generated.' + sLineBreak +
        sLineBreak +
        'You can now copy the data by clicking' + sLineBreak +
        'the [Copy] button.',
        mtInformation, [mbOK], 0)
    else
      MessageDlg(
        'Using existing External Table.' + sLineBreak +
        sLineBreak +
        'The INSERT query has been generated.' + sLineBreak +
        sLineBreak +
        'You can now copy the data by clicking' + sLineBreak +
        'the [Copy] button.',
        mtInformation, [mbOK], 0);

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
// COPY DATA mit Progress-Fenster + Cancel-Button
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
  Fields: array of TFieldTransform;
  CopyEngine: TCopyTable;
  i: Integer;
  FromRow: Integer;
  ToRow: Integer;
begin
  SetLength(Fields, FUsedFieldNames.Count);
  for i := 0 to FUsedFieldNames.Count - 1 do
  begin
    Fields[i].SourceField := FUsedFieldNames[i];
    Fields[i].DestField := FUsedFieldNames[i];
    Fields[i].Formula := '';
    Fields[i].CopyField := True;
  end;

  // TEST: Formeln mit %s-Platzhalter (werden in TransformRow automatisch typgerecht ersetzt)
  {for i := 0 to High(Fields) do
  begin
      if SameText(Fields[i].SourceField, 'NAME') then
        Fields[i].Formula := '$1 || ''_fromFormula'''   // String anhängen
      else if SameText(Fields[i].SourceField, 'QUANTITY') then
        Fields[i].Formula := '$1 * 2'                   // Multiplikation
      else if SameText(Fields[i].SourceField, 'PRICE') then
        Fields[i].Formula := '$1 * 1.5'                 // 50% Aufschlag
      else if SameText(Fields[i].SourceField, 'SALARY') then
        Fields[i].Formula := '$1 + 1000'                // Addition
      else if SameText(Fields[i].SourceField, 'CODE') then
        Fields[i].Formula := 'UPPER($1)';               // Großbuchstaben
  end;}

  if rbRange.Checked then
  begin
    FromRow := StrToIntDef(edtFrom.Text, 1);
    ToRow := StrToIntDef(edtTo.Text, 0);
  end
  else
  begin
    FromRow := 1;
    ToRow := 0;
  end;

  CopyEngine := TCopyTable.Create(
    FDBIndex, FDBIndex,
    FTableName, edtExtTableName.Text,
    Fields,
    StrToIntDef(edtBatchSize.Text, 500000),
    FromRow, ToRow
  );
  try
    CopyEngine.Execute;
  finally
    CopyEngine.Free;
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
// CloneTableAsExternal
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
  LastItemIndex: Integer;
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
      for i := Items.Count - 1 downto 0 do
      begin
        Line := Items[i];

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

        // BOOLEAN → SMALLINT konvertieren
        if CleanType = 'BOOLEAN' then
        begin
          Items[i] := StringReplace(Items[i], 'BOOLEAN', 'SMALLINT', [rfIgnoreCase]);
          SpacePos := Pos(' ', Items[i]);
          if SpacePos > 0 then
            FieldName := Copy(Items[i], 1, SpacePos - 1)
          else
            FieldName := Items[i];
          Warnings.Add('-- Column "' + FieldName + '" is BOOLEAN - converted to SMALLINT (0=false, 1=true).');
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

      // ============================================================
      // LETZTES KOMMA ENTFERNEN
      // ============================================================
      if Items.Count > 0 then
      begin
        LastItemIndex := Items.Count - 1;
        Line := Items[LastItemIndex];
        // Letztes Komma entfernen (falls vorhanden)
        if (Length(Line) > 0) and (Line[Length(Line)] = ',') then
          Items[LastItemIndex] := Copy(Line, 1, Length(Line) - 1);
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

