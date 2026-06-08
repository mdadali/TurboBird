unit clone_table_to_external_table_dialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IBDatabase,
  IBQuery, SynEdit, SynHighlighterSQL, ComCtrls, ExtCtrls,

  turbocommon,
  fsimpleobjextractor;


type
  TCopyDataThread = class(TThread)
  private
    FDatabase: TIBDatabase;
    FTransaction: TIBTransaction;
    FTableName: string;
    FExtTableName: string;
    FFieldList: string;
    FBatchSize: Integer;
    FRecCount: Integer;
    FFromRow: Integer;
    FToRow: Integer;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FCopiedRows: Integer;
    FRowsPerSec: Double;
    FErrorMessage: string;

    // Für Synchronisation mit GUI
    FOnProgress: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure DoProgress;
    procedure DoComplete;
    procedure DoError;
  protected
    procedure Execute; override;
  public
    Cancelled: Boolean;

    constructor Create(ADatabase: TIBDatabase; ATransaction: TIBTransaction;
      const ATableName, AExtTableName, AFieldList: string;
      ABatchSize, ARecCount, AFromRow: Integer);

    property CopiedRows: Integer read FCopiedRows;
    property RowsPerSec: Double read FRowsPerSec;
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
    property ErrorMessage: string read FErrorMessage;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

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
    procedure Label4Click(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
  private
    FDBIndex: integer;
    FTableName: string;
    FUsedFields: TStringList;
    FUsedFieldNames: TStringList;

    FThread: TCopyDataThread;
    FProgressForm: TForm;
    FProgressLabel, FlblElapsed: TLabel;
    FProgressBar: TProgressBar;
    FBtnCancel: TButton;

    procedure ThreadProgress(Sender: TObject);
    procedure ThreadComplete(Sender: TObject);
    procedure ThreadError(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);

    procedure CloneTableAsExternal;
    procedure GenerateInsertQuery;
    procedure CopyDataWithProgress;
    function ExternalTableExists: Boolean;
    function ExternalFileExists: Boolean;
    procedure ProgressFormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    procedure Init(const ATableName: string; ADBIndex: Integer);
  end;

var
  fmCloneToExternalTable: TfmCloneToExternalTable;

implementation

{$R *.lfm}

{ TCopyDataThread }

constructor TCopyDataThread.Create(ADatabase: TIBDatabase; ATransaction: TIBTransaction;
  const ATableName, AExtTableName, AFieldList: string;
  ABatchSize, ARecCount, AFromRow: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;

  FDatabase := ADatabase;
  FTransaction := ATransaction;
  FTableName := ATableName;
  FExtTableName := AExtTableName;
  FFieldList := AFieldList;
  FBatchSize := ABatchSize;
  FRecCount := ARecCount;
  FFromRow := AFromRow;

  FCopiedRows := 0;
  Cancelled := False;
end;

procedure TCopyDataThread.Execute;
var
  Query: TIBQuery;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow: Integer;
  BatchRows: Integer;
begin
  try
    Query := TIBQuery.Create(nil);
    try
      Query.Database := FDatabase;
      Query.Transaction := FTransaction;

      BatchCount := (FRecCount + FBatchSize - 1) div FBatchSize;

      FStartTime := Now;

      for BatchIndex := 0 to BatchCount - 1 do
      begin
        if Cancelled then
          Break;

        FromRow := FFromRow + (BatchIndex * FBatchSize);
        ToRow := FromRow + FBatchSize - 1;
        if ToRow > (FFromRow + FRecCount - 1) then
          ToRow := FFromRow + FRecCount - 1;

        BatchRows := ToRow - FromRow + 1;

        Query.Close;
        Query.SQL.Text :=
          'INSERT INTO ' + FExtTableName + ' (' + FFieldList + ')' + sLineBreak +
          'SELECT FIRST ' + IntToStr(BatchRows) +
          ' SKIP ' + IntToStr(FromRow - 1) + ' ' +
          FFieldList + sLineBreak +
          'FROM ' + FTableName;

        if not FTransaction.InTransaction then
          FTransaction.StartTransaction;

        Query.ExecSQL;
        FTransaction.CommitRetaining;

        FCopiedRows := FCopiedRows + BatchRows;  // Aufaddieren!

        Synchronize(@DoProgress);
      end;

      if not Cancelled then
        FTransaction.Commit;

      FEndTime := Now;
      if FEndTime > FStartTime then
        FRowsPerSec := FCopiedRows / ((FEndTime - FStartTime) * 24 * 60 * 60)
      else
        FRowsPerSec := 0;

    finally
      Query.Free;
    end;

    Synchronize(@DoComplete);

  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Synchronize(@DoError);
    end;
  end;
end;

procedure TCopyDataThread.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TCopyDataThread.DoComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

procedure TCopyDataThread.DoError;
begin
  if Assigned(FOnError) then
    FOnError(Self);
end;

{ TfmCloneToExternalTable }

procedure TfmCloneToExternalTable.ProgressFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FThread) and (not FThread.Finished) then
  begin
    FThread.Cancelled := True;
    CloseAction := caNone;  // Fenster noch nicht schließen – Thread läuft noch
  end
  else
    CloseAction := caFree;
end;

procedure TfmCloneToExternalTable.ThreadProgress(Sender: TObject);
begin
  if Assigned(FProgressBar) and Assigned(FThread) then
  begin
    FProgressBar.Position := FThread.CopiedRows;
    FProgressLabel.Caption := Format('Copied %d of %d rows...',
      [FThread.CopiedRows, FProgressBar.Max]);
    FlblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss',
      Now - FThread.StartTime);
    Application.ProcessMessages;
  end;
end;

procedure TfmCloneToExternalTable.ThreadComplete(Sender: TObject);
var
  StatusStr: string;
  Msg: string;
begin
  // Progress-Fenster schließen
  FProgressForm.ModalResult := mrOK;
  Application.ProcessMessages;
  Sleep(100);

  if FThread.Cancelled then
    StatusStr := 'cancelled'
  else
    StatusStr := 'completed';

  Msg := Format('Data copy %s!' + sLineBreak + sLineBreak +
                'Rows copied: %d' + sLineBreak +
                'Time: %s' + sLineBreak +
                'Speed: %.0f rows/sec',
                [StatusStr, FThread.CopiedRows,
                 FormatDateTime('hh:nn:ss', FThread.EndTime - FThread.StartTime),
                 FThread.RowsPerSec]);

  ShowMessage(Msg);
end;

procedure TfmCloneToExternalTable.ThreadError(Sender: TObject);
begin
  FProgressForm.ModalResult := mrOK;
  Application.ProcessMessages;
  Sleep(100);

  ShowMessage('Data copy failed:' + sLineBreak + FThread.ErrorMessage);
end;

procedure TfmCloneToExternalTable.CancelButtonClick(Sender: TObject);
begin
  if Assigned(FThread) then
  begin
    FThread.Cancelled := True;
    FBtnCancel.Enabled := False;
    FBtnCancel.Caption := 'Cancelling...';
  end;
end;

procedure TfmCloneToExternalTable.FormCreate(Sender: TObject);
begin
  FUsedFields := TStringList.Create;
  FUsedFieldNames := TStringList.Create;
  SaveDialog1.Filter := 'External Table Files|*.dat;*.txt;*.csv|All Files|*.*';
  SaveDialog1.DefaultExt := 'dat';
end;

procedure TfmCloneToExternalTable.Label4Click(Sender: TObject);
begin

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
  BatchSize: Integer;
  RecCount: Integer;
  FromRow: Integer;
  ToRow: Integer;
  FieldList: string;
  CountQuery: TIBQuery;
  i: Integer;
begin
  BatchSize := StrToIntDef(edtBatchSize.Text, 500000);

  // Feldliste
  FieldList := '';
  for i := 0 to FUsedFieldNames.Count - 1 do
  begin
    if FieldList <> '' then
      FieldList := FieldList + ', ';
    FieldList := FieldList + FUsedFieldNames[i];
  end;

  // ========================================================================
  // Progress-Fenster EINMAL anzeigen
  // ========================================================================
  FProgressForm := TForm.Create(nil);
  try
    FProgressForm.FormStyle := fsNormal;
    FProgressForm.Caption := 'Copying data...';
    FProgressForm.Width := 520;
    FProgressForm.Height := 230;
    FProgressForm.Position := poScreenCenter;
    FProgressForm.BorderStyle := bsDialog;
    FProgressForm.OnClose := @ProgressFormClose;

    FProgressLabel := TLabel.Create(FProgressForm);
    FProgressLabel.Parent := FProgressForm;
    FProgressLabel.Left := 16;
    FProgressLabel.Top := 16;
    FProgressLabel.Caption := 'Please wait, counting records...';
    FProgressLabel.Width := 460;

    FProgressBar := TProgressBar.Create(FProgressForm);
    FProgressBar.Parent := FProgressForm;
    FProgressBar.Left := 16;
    FProgressBar.Top := 45;
    FProgressBar.Width := 470;
    FProgressBar.Height := 20;
    FProgressBar.Min := 0;
    FProgressBar.Max := 100;
    FProgressBar.Style := pbstMarquee;

    FlblElapsed := TLabel.Create(FProgressForm);
    FlblElapsed.Parent := FProgressForm;
    FlblElapsed.Left := 16;
    FlblElapsed.Top := 80;

    FBtnCancel := TButton.Create(FProgressForm);
    FBtnCancel.Parent := FProgressForm;
    FBtnCancel.Caption := 'Cancel';
    FBtnCancel.Left := 200;
    FBtnCancel.Top := 120;
    FBtnCancel.Width := 100;
    FBtnCancel.Enabled := False;
    FBtnCancel.OnClick := @CancelButtonClick;

    FProgressForm.Show;
    Application.ProcessMessages;

    // ========================================================================
    // Record Count ermitteln
    // ========================================================================
    CountQuery := TIBQuery.Create(nil);
    try
      CountQuery.Database := IBDatabase1;
      CountQuery.Transaction := IBTransaction1;
      CountQuery.AllowAutoActivateTransaction := True;
      CountQuery.SQL.Text := 'SELECT COUNT(*) FROM ' + FTableName;
      CountQuery.Open;
      RecCount := CountQuery.Fields[0].AsInteger;
      CountQuery.Close;
    finally
      CountQuery.Free;
    end;

    if RecCount = 0 then
    begin
      FProgressForm.Close;
      ShowMessage('Source table is empty. Nothing to copy.');
      Exit;
    end;

    // ========================================================================
    // From/To je nach Auswahl
    // ========================================================================
    if rbRange.Checked then
    begin
      FromRow := StrToIntDef(edtFrom.Text, 1);
      ToRow := StrToIntDef(edtTo.Text, RecCount);
      if FromRow < 1 then
        FromRow := 1;
      if ToRow > RecCount then
        ToRow := RecCount;
      if FromRow > ToRow then
      begin
        FProgressForm.Close;
        ShowMessage('"From" must be less than or equal to "To".');
        Exit;
      end;
      RecCount := ToRow - FromRow + 1;
    end
    else
    begin
      FromRow := 1;
    end;

    // ========================================================================
    // ProgressBar konfigurieren
    // ========================================================================
    FProgressBar.Style := pbstNormal;
    FProgressBar.Max := RecCount;
    FProgressBar.Position := 0;
    FProgressLabel.Caption := Format('Total Records: %d', [RecCount]);
    FBtnCancel.Enabled := True;
    Application.ProcessMessages;

    // ========================================================================
    // Thread starten
    // ========================================================================
    FThread := TCopyDataThread.Create(
      IBDatabase1, IBTransaction1,
      FTableName, edtExtTableName.Text, FieldList,
      BatchSize, RecCount, FromRow
    );
    try
      FThread.OnProgress := @ThreadProgress;
      FThread.OnComplete := @ThreadComplete;
      FThread.OnError := @ThreadError;
      FThread.Start;

      while (not FThread.Finished) and (FProgressForm.Visible) do
      begin
        Application.ProcessMessages;
        Sleep(50);
      end;

    finally
      if Assigned(FThread) then
      begin
        if not FThread.Finished then
        begin
          FThread.Cancelled := True;
          FThread.WaitFor;
        end;
        FThread.Free;
        FThread := nil;
      end;
    end;
  finally
    FProgressForm.Free;
    FProgressForm := nil;
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

