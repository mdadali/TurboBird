unit uCreateTableFromDataSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls,
  CheckLst, Grids, DB, Math, DateUtils,
  IBDatabase, IBQuery, IBXScript,
  turbocommon, fbcommon,
  uGenSQLFromCSVDataset,
  uthemeselector;

type
  { TfrmCreateTableFromDataSet }

  TfrmCreateTableFromDataSet = class(TForm)
    btnMainCancel: TButton;
    btnDeselectAll: TButton;
    btnOK: TButton;
    btnSelectAll: TButton;
    btnRun: TButton;
    chkboxCopyData: TCheckBox;
    chkLstFields: TCheckListBox;
    cmbBoxServers: TComboBox;
    cmbBoxDBs: TComboBox;
    edtDestTableName: TEdit;
    edtCommitInterval: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    grBoxCopyOptions1: TGroupBox;
    grBoxFields: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblHint: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    pnlBottom: TPanel;
    pnlFields: TPanel;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
    sgFields: TStringGrid;
    StatusBar1: TStatusBar;

    procedure btnMainCancelClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure cmbBoxServersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
  private
    FDataSet: TDataSet;
    FFileName: string;
    FFields: array of record
      FieldName: string;
      FieldType: string;
      CharLength: Integer;
      Checked: Boolean;
    end;
    FCancelled: Boolean;

    procedure LoadFieldList;
    procedure FillServerCombo;
    procedure FillDBCombo;
    function  GetTargetDBIndex: Integer;
    function  GetTargetTable: string;
    function  GetCommitInterval: Integer;
    function  GetFromRow: integer;
    function  GetToRow: Integer;
    procedure RunFBInsertBatched(ADBIndex: Integer; const ATableName: string);
    procedure AssignParamFromField(Param: TParam; SourceField: TField);
    procedure CancelButtonClick(Sender: TObject);
    function  TableExists(ADB: TIBDatabase; const ATableName: string): Boolean;
  public
    procedure Init(ADataSet: TDataSet; const AFileName: string);
  end;

implementation

{$R *.lfm}

procedure TfrmCreateTableFromDataSet.AssignParamFromField(Param: TParam; SourceField: TField);
begin
  if SourceField.IsNull then
  begin
    Param.Clear;
    Exit;
  end;

  case SourceField.DataType of
    ftSmallint, ftWord:
      Param.AsSmallInt := SourceField.AsInteger;

    ftInteger, ftLargeint, ftAutoInc:
      Param.AsInteger := SourceField.AsInteger;

    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      Param.AsFloat := SourceField.AsFloat;

    ftDateTime, ftTimeStamp, ftDate, ftTime:
      Param.AsDateTime := SourceField.AsDateTime;

    ftBoolean:
      Param.AsBoolean := SourceField.AsBoolean;

  else
    Param.AsString := SourceField.AsString;
  end;
end;

procedure TfrmCreateTableFromDataSet.FormCreate(Sender: TObject);
begin
  sgFields.ColCount := 2;
  sgFields.Cells[0, 0] := 'Source Field';
  sgFields.Cells[1, 0] := 'Field Type';
  sgFields.ColWidths[0] := 250;
  sgFields.ColWidths[1] := 150;

  lblHint.Alignment := taCenter;
  lblHint.Layout := tlCenter;
  lblHint.WordWrap := True;
  lblHint.Caption := 'If you need to transform data with formulas,' + sLineBreak +
                     'please open the created table with Clone Table.';
end;

procedure TfrmCreateTableFromDataSet.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfrmCreateTableFromDataSet.Init(ADataSet: TDataSet; const AFileName: string);
begin
  FDataSet := ADataSet;
  FFileName := AFileName;

  edtDestTableName.Text := UpperCase(ChangeFileExt(ExtractFileName(AFileName), ''));

  FillServerCombo;
  if cmbBoxServers.Items.Count > 0 then
  begin
    cmbBoxServers.ItemIndex := 0;
    FillDBCombo;
  end;

  LoadFieldList;
end;

procedure TfrmCreateTableFromDataSet.FillServerCombo;
var
  List: TStringList;
begin
  List := GetServerListFromTreeView;
  try
    cmbBoxServers.Items.Assign(List);
  finally
    List.Free;
  end;
end;

procedure TfrmCreateTableFromDataSet.FillDBCombo;
var
  i: Integer;
begin
  cmbBoxDBs.Items.Clear;
  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, cmbBoxServers.Text) then
      cmbBoxDBs.Items.Add(RegisteredDatabases[i].RegRec.Title);
  if cmbBoxDBs.Items.Count > 0 then
    cmbBoxDBs.ItemIndex := 0;
end;

procedure TfrmCreateTableFromDataSet.LoadFieldList;
var
  Gen: TGenSQLFromCSVDataset;
  i: Integer;
begin
  Gen := TGenSQLFromCSVDataset.Create(FDataSet,
           UpperCase(ChangeFileExt(ExtractFileName(FFileName), '')),
           50);
  try
    SetLength(FFields, Length(Gen.Fields));
    for i := 0 to High(Gen.Fields) do
    begin
      FFields[i].FieldName := Gen.Fields[i].FieldName;
      FFields[i].FieldType := Gen.Fields[i].FieldType;
      FFields[i].Checked    := True;
      FFields[i].CharLength := 0;
    end;

    chkLstFields.Clear;
    sgFields.RowCount := 1;
    for i := 0 to High(FFields) do
    begin
      chkLstFields.Items.Add(FFields[i].FieldName);
      chkLstFields.Checked[i] := True;
      sgFields.RowCount := i + 2;
      sgFields.Cells[0, i + 1] := FFields[i].FieldName;
      sgFields.Cells[1, i + 1] := FFields[i].FieldType;
    end;
  finally
    Gen.Free;
  end;
end;

procedure TfrmCreateTableFromDataSet.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

procedure TfrmCreateTableFromDataSet.btnMainCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmCreateTableFromDataSet.btnRunClick(Sender: TObject);
var
  DBIndex, i: Integer;
  TableName, SQL, FieldList: string;
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  Script: TIBXScript;
begin
  DBIndex := GetTargetDBIndex;
  if DBIndex < 0 then
  begin
    ShowMessage('Please select a valid destination database.');
    Exit;
  end;

  TableName := GetTargetTable;
  if TableName = '' then
  begin
    ShowMessage('Please enter a table name.');
    Exit;
  end;

  for i := 0 to High(FFields) do
    FFields[i].Checked := chkLstFields.Checked[i];

  DestDB := RegisteredDatabases[DBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[DBIndex].IBTransaction;
  if not DestDB.Connected then DestDB.Connected := True;
  if not DestTrans.InTransaction then DestTrans.StartTransaction;

  Script := TIBXScript.Create(nil);
  try
    Script.Database := DestDB;
    Script.Transaction := DestTrans;

    // Firebird-Tabelle erstellen
    if TableExists(DestDB, TableName) then
    begin
      if MessageDlg('Table "' + TableName + '" already exists. Drop and recreate?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        SQL := 'DROP TABLE ' + TableName;
        Script.ExecSQLScript(SQL);
        DestTrans.CommitRetaining;
      end
      else
      begin
        if chkboxCopyData.Checked then
          //RunFBInsert(DBIndex, TableName);
          RunFBInsertBatched(DBIndex, TableName);
        Exit;
      end;
    end;

    FieldList := '';
    for i := 0 to High(FFields) do
    begin
      if not FFields[i].Checked then Continue;
      if FieldList <> '' then FieldList := FieldList + ', ';
      FieldList := FieldList + FFields[i].FieldName + ' ' + FFields[i].FieldType;
    end;
    SQL := 'CREATE TABLE ' + TableName + ' (' + FieldList + ')';
    Script.ExecSQLScript(SQL);
    DestTrans.CommitRetaining;

    // Daten kopieren, falls Checkbox aktiv
    if chkboxCopyData.Checked then
      //RunFBInsert(DBIndex, TableName);
      RunFBInsertBatched(DBIndex, TableName);

  finally
    Script.Free;
  end;
end;

procedure TfrmCreateTableFromDataSet.btnSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := True;
end;

procedure TfrmCreateTableFromDataSet.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := False;
end;

procedure TfrmCreateTableFromDataSet.cmbBoxServersChange(Sender: TObject);
begin
  if cmbBoxServers.ItemIndex >= 0 then
    FillDBCombo;
end;

function TfrmCreateTableFromDataSet.GetTargetDBIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, cmbBoxServers.Text) and
       SameText(RegisteredDatabases[i].RegRec.Title, cmbBoxDBs.Text) then
      Exit(i);
end;

function TfrmCreateTableFromDataSet.GetTargetTable: string;
begin
  Result := Trim(edtDestTableName.Text);
end;

function TfrmCreateTableFromDataSet.GetCommitInterval: Integer;
begin
  Result := StrToIntDef(edtCommitInterval.Text, 1000000);
end;

function TfrmCreateTableFromDataSet.GetFromRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtFrom.Text, 1)
  else
    Result := 1;
end;

function TfrmCreateTableFromDataSet.GetToRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtTo.Text, FDataSet.RecordCount)
  else
    Result := FDataSet.RecordCount;
end;

procedure TfrmCreateTableFromDataSet.CancelButtonClick(Sender: TObject);
begin
  FCancelled := True;
  if Sender is TButton then
  begin
    TButton(Sender).Enabled := False;
    TButton(Sender).Caption := 'Cancelling...';
  end;
end;

// ---------------------------------------------------------------
//  BATCHED INSERT via EXECUTE BLOCK mit Literal-Werten
//
//  - 256 Zeilen pro EXECUTE BLOCK (Firebird 256-Kontext-Limit)
//  - Commit-Intervall kommt aus edtCommitInterval (User-Eingabe)
//  - Progress zeigt Zeilen UND Commits
// ---------------------------------------------------------------
procedure TfrmCreateTableFromDataSet.RunFBInsertBatched(
  ADBIndex: Integer; const ATableName: string);
const
  ROWS_PER_BATCH = 256;
var
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  Query: TIBQuery;
  StartTime, EndTime: TDateTime;
  ProgressForm: TForm;
  ProgressLabel, LblElapsed, LblCommit: TLabel;
  ProgressBar, CommitBar: TProgressBar;
  BtnCancel: TButton;
  FieldNames: string;
  CheckedFields: array of Integer;
  FieldsPerRow: Integer;
  FromRow, ToRow, TotalRows: Integer;
  TotalBatches, TotalCommits: Integer;
  CommitsDone: Integer;
  RowsThisBatch, RowsInThisBatch: Integer;
  BatchIdx, i, f: Integer;
  CurrentRow: Integer;
  CommitInterval: Integer;
  RowsSinceLastCommit: Integer;
  SourceField: TField;
  SQLBody: TStringList;
  SQL, OneInsert: string;
  OldDecimalSep: Char;
begin
  DestDB := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;
  if not DestDB.Connected then DestDB.Connected := True;
  if not DestTrans.InTransaction then DestTrans.StartTransaction;

  // Commit-Intervall aus dem Formular
  CommitInterval := GetCommitInterval;
  if CommitInterval < 1 then
    CommitInterval := 100000;

  // 1. Felder sammeln
  SetLength(CheckedFields, 0);
  FieldNames := '';
  for i := 0 to High(FFields) do
    if FFields[i].Checked then
    begin
      SetLength(CheckedFields, Length(CheckedFields) + 1);
      CheckedFields[High(CheckedFields)] := i;
      if FieldNames <> '' then FieldNames := FieldNames + ', ';
      FieldNames := FieldNames + FFields[i].FieldName;
    end;

  FieldsPerRow := Length(CheckedFields);
  if FieldsPerRow = 0 then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  // 2. Range
  FromRow := GetFromRow;
  ToRow := GetToRow;
  if ToRow > FDataSet.RecordCount then ToRow := FDataSet.RecordCount;
  TotalRows := ToRow - FromRow + 1;
  if TotalRows <= 0 then
  begin
    ShowMessage('No rows to copy.');
    Exit;
  end;

  TotalBatches := (TotalRows + ROWS_PER_BATCH - 1) div ROWS_PER_BATCH;
  TotalCommits := (TotalRows + CommitInterval - 1) div CommitInterval;
  CommitsDone := 0;

  // 3. Progress + Query
  Query := TIBQuery.Create(nil);
  ProgressForm := TForm.Create(nil);
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  try
    DefaultFormatSettings.DecimalSeparator := '.';

    Query.Database := DestDB;
    Query.Transaction := DestTrans;
    Query.AllowAutoActivateTransaction := True;

    ProgressForm.FormStyle := fsNormal;
    ProgressForm.Caption := 'Copying data to ' + ATableName;
    ProgressForm.Width := 540;
    ProgressForm.Height := 300;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    // === Zeilen-Label ===
    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := Format('Total: %d rows   |   Batch: %d rows',
      [TotalRows, ROWS_PER_BATCH]);
    ProgressLabel.Width := 500;

    // === Zeilen-ProgressBar ===
    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 40;
    ProgressBar.Width := 500;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := TotalRows;
    ProgressBar.Position := 0;

    // === Commit-Label ===
    LblCommit := TLabel.Create(ProgressForm);
    LblCommit.Parent := ProgressForm;
    LblCommit.Left := 16;
    LblCommit.Top := 75;
    LblCommit.Caption := Format('Commits: 0 of %d   (every %d rows)',
      [TotalCommits, CommitInterval]);
    LblCommit.Width := 500;

    // === Commit-ProgressBar ===
    CommitBar := TProgressBar.Create(ProgressForm);
    CommitBar.Parent := ProgressForm;
    CommitBar.Left := 16;
    CommitBar.Top := 100;
    CommitBar.Width := 500;
    CommitBar.Height := 20;
    CommitBar.Min := 0;
    CommitBar.Max := TotalCommits;
    CommitBar.Position := 0;

    // === Elapsed-Label ===
    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 135;
    LblElapsed.Caption := 'Elapsed: 00:00:00   |   0 rows/sec';
    LblElapsed.Width := 500;

    // === Cancel-Button ===
    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 210;
    BtnCancel.Top := 180;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelButtonClick;

    ProgressForm.Show;
    Application.ProcessMessages;

    FCancelled := False;
    RowsSinceLastCommit := 0;

    FDataSet.DisableControls;
    try
      FDataSet.First;
      for i := 1 to FromRow - 1 do
        FDataSet.Next;

      StartTime := Now;
      CurrentRow := 0;

      // ============================================================
      // Batch-Schleife
      // ============================================================
      for BatchIdx := 0 to TotalBatches - 1 do
      begin
        if FCancelled then Break;

        RowsThisBatch := ROWS_PER_BATCH;
        if (BatchIdx + 1) * ROWS_PER_BATCH > TotalRows then
          RowsThisBatch := TotalRows - (BatchIdx * ROWS_PER_BATCH);

        // EXECUTE BLOCK aufbauen
        SQLBody := TStringList.Create;
        try
          SQLBody.Add('EXECUTE BLOCK AS');
          SQLBody.Add('BEGIN');

          RowsInThisBatch := 0;
          for i := 0 to RowsThisBatch - 1 do
          begin
            if FDataSet.EOF then Break;

            OneInsert := '  INSERT INTO ' + ATableName + ' (' + FieldNames + ') VALUES (';

            for f := 0 to FieldsPerRow - 1 do
            begin
              if f > 0 then OneInsert := OneInsert + ', ';

              SourceField := FDataSet.FieldByName(FFields[CheckedFields[f]].FieldName);
              if SourceField.IsNull then
                OneInsert := OneInsert + 'NULL'
              else
                case SourceField.DataType of
                  ftSmallint, ftInteger, ftLargeint, ftAutoInc, ftWord:
                    OneInsert := OneInsert + SourceField.AsString;

                  ftFloat, ftCurrency, ftBCD, ftFMTBcd:
                    OneInsert := OneInsert + FloatToStr(SourceField.AsFloat);

                  ftDateTime, ftTimeStamp:
                    OneInsert := OneInsert + QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', SourceField.AsDateTime));

                  ftDate:
                    OneInsert := OneInsert + QuotedStr(FormatDateTime('yyyy-mm-dd', SourceField.AsDateTime));

                  ftTime:
                    OneInsert := OneInsert + QuotedStr(FormatDateTime('hh:nn:ss.zzz', SourceField.AsDateTime));

                  ftBoolean:
                    if SourceField.AsBoolean then
                      OneInsert := OneInsert + 'TRUE'
                    else
                      OneInsert := OneInsert + 'FALSE';
                else
                  OneInsert := OneInsert + QuotedStr(SourceField.AsString);
                end;
            end;

            OneInsert := OneInsert + ');';
            SQLBody.Add(OneInsert);

            FDataSet.Next;
            Inc(CurrentRow);
            Inc(RowsInThisBatch);
          end;

          SQLBody.Add('END');
          SQL := SQLBody.Text;
        finally
          SQLBody.Free;
        end;

        // Batch ausführen
        Query.Close;
        Query.SQL.Text := SQL;
        Query.ExecSQL;

        // ============================================================
        // Commit nur alle CommitInterval Zeilen
        // ============================================================
        Inc(RowsSinceLastCommit, RowsInThisBatch);

        if RowsSinceLastCommit >= CommitInterval then
        begin
          DestTrans.CommitRetaining;
          RowsSinceLastCommit := 0;
          Inc(CommitsDone);

          // Commit-ProgressBar aktualisieren
          CommitBar.Position := CommitsDone;
          LblCommit.Caption := Format('Commits: %d of %d   (every %d rows)',
            [CommitsDone, TotalCommits, CommitInterval]);
        end;

        // Zeilen-Progress
        ProgressBar.Position := CurrentRow;
        ProgressLabel.Caption := Format('Total: %d rows   |   Batch: %d rows   |   Current: %d',
          [TotalRows, ROWS_PER_BATCH, CurrentRow]);
        LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
          '   |   ' + Format('%.0f rows/sec',
            [CurrentRow / Max(1, (Now - StartTime) * 86400)]);
        Application.ProcessMessages;
      end;

      // ============================================================
      // Finaler Commit für den Rest
      // ============================================================
      if (not FCancelled) and DestTrans.InTransaction then
      begin
        DestTrans.Commit;
        Inc(CommitsDone);
        CommitBar.Position := CommitsDone;
        LblCommit.Caption := Format('Commits: %d of %d   (every %d rows)',
          [CommitsDone, TotalCommits, CommitInterval]);
      end;

      EndTime := Now;

    finally
      FDataSet.EnableControls;
    end;

    if not FCancelled then
    begin
      ShowMessage(Format('Data copy completed!' + sLineBreak +
                         'Rows: %d' + sLineBreak +
                         'Time: %s' + sLineBreak +
                         'Speed: %.0f rows/sec' + sLineBreak +
                         'Batch: %d rows' + sLineBreak +
                         'Commits: %d (every %d rows)',
                         [CurrentRow,
                          FormatDateTime('hh:nn:ss', EndTime - StartTime),
                          CurrentRow / Max(1, (EndTime - StartTime) * 86400),
                          ROWS_PER_BATCH,
                          CommitsDone,
                          CommitInterval]));
    end
    else
    begin
      if DestTrans.InTransaction then
        DestTrans.Rollback;
      ShowMessage('Copy cancelled by user.');
    end;

  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
    Query.Free;
    ProgressForm.Free;
  end;
end;

function TfrmCreateTableFromDataSet.TableExists(ADB: TIBDatabase; const ATableName: string): Boolean;
var
  qry: TIBQuery;
begin
  Result := False;
  qry := TIBQuery.Create(nil);
  try
    qry.Database := ADB;
    qry.Transaction := ADB.DefaultTransaction;
    qry.AllowAutoActivateTransaction := True;
    qry.SQL.Text :=
      'SELECT 1 FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ' +
      QuotedStr(UpperCase(ATableName)) + ' AND RDB$VIEW_BLR IS NULL';
    qry.Open;
    Result := not qry.EOF;
    qry.Close;
  finally
    qry.Free;
  end;
end;

end.
