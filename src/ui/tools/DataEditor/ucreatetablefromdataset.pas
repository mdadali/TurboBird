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
    lblStatus: TLabel;
    Panel1: TPanel;
    pnlBottom: TPanel;
    pnlFields: TPanel;
    ProgressBar1: TProgressBar;
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
    procedure ParseFieldType(const AType: string; out BaseType: string; out Size: Integer);
    function  GridRowToFieldType(Row: Integer): string;
    procedure SyncFieldsFromGrid;
    procedure SetupGridColumns;
    procedure SetupTypePickList;
  public
    procedure Init(ADataSet: TDataSet; const AFileName: string);
  end;



const
  FB_DATA_TYPES: array[0..15] of string = (
    'INTEGER',
    'SMALLINT',
    'BIGINT',
    'VARCHAR',
    'CHAR',
    'CSTRING',
    'DATE',
    'TIME',
    'TIMESTAMP',
    'DOUBLE PRECISION',
    'FLOAT',
    'NUMERIC',
    'DECIMAL',
    'BLOB',
    'BOOLEAN',
    'BLOB SUB_TYPE TEXT'
  );

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
  SetupGridColumns;

  rbAllRows.OnChange := @rbAllRowsChange;
  rbRange.OnChange   := @rbAllRowsChange;   // beide RadioButtons

  edtCommitInterval.Text := '100000';       // Default

  lblHint.Alignment := taCenter;
  lblHint.Layout    := tlCenter;
  lblHint.WordWrap  := True;
  lblHint.Caption   := 'Edit the fields as needed. Use Clone Table for data transformations.';
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

  LoadFieldList;   // Grid wird hier (nach SetupGridColumns aus FormCreate) gefüllt
end;

procedure TfrmCreateTableFromDataSet.SetupGridColumns;
begin
  sgFields.FixedCols := 0;
  sgFields.FixedRows := 1;
  sgFields.ColCount := 5;
  sgFields.RowCount := 1;

  sgFields.Options := sgFields.Options +
    [goEditing, goRowSelect, goVertLine, goHorzLine];

  // Column-Objekte erzeugen
  while sgFields.Columns.Count < 5 do
    sgFields.Columns.Add;

  // Titel der FIXED ROW
  sgFields.Columns[0].Title.Caption := 'Include';
  sgFields.Columns[1].Title.Caption := 'Field Name';
  sgFields.Columns[2].Title.Caption := 'Data Type';
  sgFields.Columns[3].Title.Caption := 'Size';
  sgFields.Columns[4].Title.Caption := 'Not Null';

  // Breiten
  sgFields.ColWidths[0] := 60;
  sgFields.ColWidths[1] := 180;
  sgFields.ColWidths[2] := 160;
  sgFields.ColWidths[3] := 60;
  sgFields.ColWidths[4] := 70;

  // Spaltentypen
  sgFields.Columns[0].ButtonStyle := cbsCheckboxColumn;
  sgFields.Columns[2].ButtonStyle := cbsPickList;
  sgFields.Columns[4].ButtonStyle := cbsCheckboxColumn;

  SetupTypePickList;
end;

procedure TfrmCreateTableFromDataSet.SetupTypePickList;
var
  i: Integer;
begin
  sgFields.Columns[2].PickList.Clear;
  for i := Low(FB_DATA_TYPES) to High(FB_DATA_TYPES) do
    sgFields.Columns[2].PickList.Add(FB_DATA_TYPES[i]);
end;

procedure TfrmCreateTableFromDataSet.ParseFieldType(
  const AType: string; out BaseType: string; out Size: Integer);
var
  LParen, RParen: Integer;
begin
  Size := 0;
  LParen := Pos('(', AType);
  if LParen > 0 then
  begin
    BaseType := Trim(Copy(AType, 1, LParen - 1));
    RParen := Pos(')', AType);
    if RParen > LParen then
      Size := StrToIntDef(Trim(Copy(AType, LParen + 1, RParen - LParen - 1)), 0);
  end
  else
    BaseType := Trim(AType);
end;

function TfrmCreateTableFromDataSet.GridRowToFieldType(Row: Integer): string;
var
  BaseType: string;
  Size: Integer;
begin
  BaseType := Trim(sgFields.Cells[2, Row]);
  Size := StrToIntDef(Trim(sgFields.Cells[3, Row]), 0);

  if (BaseType = 'VARCHAR') or (BaseType = 'CHAR') or (BaseType = 'CSTRING') then
  begin
    if Size <= 0 then Size := 50;
    Result := BaseType + '(' + IntToStr(Size) + ')';
  end
  else if (BaseType = 'NUMERIC') or (BaseType = 'DECIMAL') then
  begin
    if Size <= 0 then Size := 18;
    Result := BaseType + '(' + IntToStr(Size) + ',2)';  // Scale Default 2
  end
  else
    Result := BaseType;
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
  BaseType: string;
  Size: Integer;
begin
  Gen := TGenSQLFromCSVDataset.Create(FDataSet,
           UpperCase(ChangeFileExt(ExtractFileName(FFileName), '')),
           50);
  try
    SetLength(FFields, Length(Gen.Fields));
    for i := 0 to High(Gen.Fields) do
    begin
      FFields[i].FieldName  := Gen.Fields[i].FieldName;
      FFields[i].FieldType  := Gen.Fields[i].FieldType;
      FFields[i].Checked    := True;
      FFields[i].CharLength := 0;
    end;
  finally
    Gen.Free;
  end;

  chkLstFields.Clear;
  sgFields.RowCount := 1;

  for i := 0 to High(FFields) do
  begin
    chkLstFields.Items.Add(FFields[i].FieldName);
    chkLstFields.Checked[i] := True;

    sgFields.RowCount := i + 2;
    sgFields.Cells[0, i + 1] := '1';                          // Include (checked)
    sgFields.Cells[1, i + 1] := FFields[i].FieldName;         // Field Name
    ParseFieldType(FFields[i].FieldType, BaseType, Size);     // zerlegen
    sgFields.Cells[2, i + 1] := BaseType;                     // Data Type
    if Size > 0 then
      sgFields.Cells[3, i + 1] := IntToStr(Size)              // Size
    else
      sgFields.Cells[3, i + 1] := '';
    sgFields.Cells[4, i + 1] := '0';                          // Not Null
  end;
end;

procedure TfrmCreateTableFromDataSet.SyncFieldsFromGrid;
var
  i: Integer;
begin
  if sgFields.RowCount < 2 then Exit;
  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;

    FFields[i].FieldName  := Trim(sgFields.Cells[1, i + 1]);
    FFields[i].FieldType  := GridRowToFieldType(i + 1);       // nutzt Spalten 2+3
    FFields[i].Checked    := SameText(sgFields.Cells[0, i + 1], '1');
    FFields[i].CharLength := StrToIntDef(Trim(sgFields.Cells[3, i + 1]), 0);

    if i < chkLstFields.Count then
      chkLstFields.Checked[i] := FFields[i].Checked;
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
  SyncFieldsFromGrid;
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
  for i := 0 to High(FFields) do
  begin
    FFields[i].Checked := True;
    if i + 1 < sgFields.RowCount then
      sgFields.Cells[0, i + 1] := '1';
  end;
end;

procedure TfrmCreateTableFromDataSet.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := False;
  for i := 0 to High(FFields) do
  begin
    FFields[i].Checked := False;
    if i + 1 < sgFields.RowCount then
      sgFields.Cells[0, i + 1] := '0';
  end;
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

{// ---------------------------------------------------------------
//  BATCHED INSERT via EXECUTE BLOCK mit Literal-Werten
//
//  - 256 Zeilen pro EXECUTE BLOCK (Firebird 256-Kontext-Limit)
//  - Commit-Intervall kommt aus edtCommitInterval (User-Eingabe)
//  - Zwei Progressbars: Zeilen + Commits
//  - Aussagekräftige Statistik auch bei Abbruch
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
  RemainingRows: Integer;
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
  StartTime := Now;
  EndTime := StartTime;
  CurrentRow := 0;
  RowsSinceLastCommit := 0;
  FCancelled := False;

  try
    DefaultFormatSettings.DecimalSeparator := '.';

    Query.Database := DestDB;
    Query.Transaction := DestTrans;
    Query.AllowAutoActivateTransaction := True;

    ProgressForm.FormStyle := fsNormal;
    ProgressForm.Caption := 'Copying data to ' + ATableName;
    ProgressForm.Width := 540;
    ProgressForm.Height := 320;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    // === Zeilen-Label ===
    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := Format('Total: %s rows   |   Batch: %d rows',
      [FormatFloat('#,##0', TotalRows), ROWS_PER_BATCH]);
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
    LblCommit.Caption := Format('Commits: 0 of %d   (every %s rows)',
      [TotalCommits, FormatFloat('#,##0', CommitInterval)]);
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

    FDataSet.DisableControls;
    try
      FDataSet.First;
      for i := 1 to FromRow - 1 do
        FDataSet.Next;

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

          CommitBar.Position := CommitsDone;
          LblCommit.Caption := Format('Commits: %d of %d   (every %s rows)',
            [CommitsDone, TotalCommits, FormatFloat('#,##0', CommitInterval)]);
        end;

        // Zeilen-Progress
        ProgressBar.Position := CurrentRow;
        ProgressLabel.Caption := Format('Total: %s rows   |   Batch: %d rows   |   Current: %s',
          [FormatFloat('#,##0', TotalRows), ROWS_PER_BATCH, FormatFloat('#,##0', CurrentRow)]);
        LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
          '   |   ' + FormatFloat('#,##0', Round(CurrentRow / Max(0.001, (Now - StartTime) * 86400))) +
          ' rows/sec';
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
        LblCommit.Caption := Format('Commits: %d of %d   (every %s rows)',
          [CommitsDone, TotalCommits, FormatFloat('#,##0', CommitInterval)]);
      end;

    finally
      EndTime := Now;
      FDataSet.EnableControls;
    end;

    // ============================================================
    // Statistik-Dialog
    // ============================================================
    if not FCancelled then
    begin
      // === Erfolgreich abgeschlossen ===
      ShowMessage(Format('Data copy completed!' + sLineBreak +
                         sLineBreak +
                         'Rows:     %s' + sLineBreak +
                         'Time:     %s' + sLineBreak +
                         'Speed:    %s rows/sec' + sLineBreak +
                         'Batch:    %d rows' + sLineBreak +
                         'Commits:  %d (every %s rows)',
                         [FormatFloat('#,##0', CurrentRow),
                          FormatDateTime('hh:nn:ss', EndTime - StartTime),
                          FormatFloat('#,##0', Round(CurrentRow / Max(0.001, (EndTime - StartTime) * 86400))),
                          ROWS_PER_BATCH,
                          CommitsDone,
                          FormatFloat('#,##0', CommitInterval)]));
    end
    else
    begin
      // === Abgebrochen ===
      if DestTrans.InTransaction then
        DestTrans.Rollback;

      RemainingRows := TotalRows - CurrentRow;

      ShowMessage(Format('Copy cancelled by user!' + sLineBreak +
                         sLineBreak +
                         'Rows copied:  %s of %s' + sLineBreak +
                         'Rows skipped: %s' + sLineBreak +
                         sLineBreak +
                         'Time:     %s' + sLineBreak +
                         'Speed:    %s rows/sec' + sLineBreak +
                         'Commits:  %d' + sLineBreak +
                         sLineBreak +
                         'Note: Not-committed data has been rolled back.' + sLineBreak +
                         'Please check the destination table.',
                         [FormatFloat('#,##0', CurrentRow),
                          FormatFloat('#,##0', TotalRows),
                          FormatFloat('#,##0', RemainingRows),
                          FormatDateTime('hh:nn:ss', EndTime - StartTime),
                          FormatFloat('#,##0', Round(CurrentRow / Max(0.001, (EndTime - StartTime) * 86400))),
                          CommitsDone]));
    end;

  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
    Query.Free;
    ProgressForm.Free;
  end;
end; }

{procedure TfrmCreateTableFromDataSet.RunFBInsertBatched(
  ADBIndex: Integer;
  const ATableName: string);
const
  ROWS_PER_BATCH = 256;
var
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  Query: TIBQuery;

  CheckedFields: array of Integer;
  CheckedCount: Integer;
  FieldsPerRow: Integer;

  FieldNames: string;
  InsertSQL: string;

  SourceField: TField;
  Param: TParam;

  CurrentRow: Integer;
  TotalRows: Integer;
  RowsThisBatch: Integer;
  RowsInThisBatch: Integer;

  f: Integer;
  i: Integer;

  CommitInterval: Integer;
  NextCommitRow: Integer;
  CommitCount: Integer;

  StartTime: TDateTime;
  ElapsedSeconds: Double;
  RowsPerSecond: Double;

  MsgText: string;
begin
  if FDataSet = nil then
    Exit;

  if not FDataSet.Active then
    Exit;

  if ATableName = '' then
    Exit;

  FCancelled := False;

  { ------------------------------------------------------------ }
  { Ziel-Datenbank und Transaktion                               }
  { ------------------------------------------------------------ }

  DestDB := RegisteredDatabases[ADBIndex].IBDatabase;
  if DestDB = nil then
    raise Exception.Create('Keine Zieldatenbank ausgewählt.');

  DestTrans := DestDB.DefaultTransaction;
  if DestTrans = nil then
    raise Exception.Create('Keine Transaktion für die Zieldatenbank vorhanden.');

  if not DestTrans.Active then
    DestTrans.StartTransaction;

  { ------------------------------------------------------------ }
  { Welche Felder sind im Grid ausgewählt?                       }
  { ------------------------------------------------------------ }

  CheckedCount := 0;

  for i := 0 to High(FFields) do
  begin
    if FFields[i].Checked then
    begin
      Inc(CheckedCount);
      SetLength(CheckedFields, CheckedCount);
      CheckedFields[CheckedCount - 1] := i;
    end;
  end;

  if CheckedCount = 0 then
    raise Exception.Create('Es wurde kein Feld zum Import ausgewählt.');

  FieldsPerRow := CheckedCount;

  { ------------------------------------------------------------ }
  { Feldnamen für INSERT aufbauen                                }
  { ------------------------------------------------------------ }

  FieldNames := '';

  for f := 0 to FieldsPerRow - 1 do
  begin
    if f > 0 then
      FieldNames := FieldNames + ', ';

    FieldNames := FieldNames +
      FFields[CheckedFields[f]].FieldName;
  end;

  { ------------------------------------------------------------ }
  { Anzahl Datensätze bestimmen                                  }
  { ------------------------------------------------------------ }

  TotalRows := FDataSet.RecordCount;

  if TotalRows <= 0 then
    Exit;

  { ------------------------------------------------------------ }
  { Commit-Intervall aus Formular übernehmen                     }
  { ------------------------------------------------------------ }

  CommitInterval := StrToIntDef(edtCommitInterval.Text, 1000);

  if CommitInterval <= 0 then
    CommitInterval := ROWS_PER_BATCH;

  { ------------------------------------------------------------ }
  { Prepared INSERT                                               }
  { ------------------------------------------------------------ }
  {
    WICHTIG:
    Kein EXECUTE BLOCK mehr.

    Stattdessen wird genau ein INSERT vorbereitet und für
    jeden Datensatz wiederverwendet.
  }

  Query := TIBQuery.Create(nil);
  try
    Query.Database := DestDB;
    Query.Transaction := DestTrans;
    Query.AllowAutoActivateTransaction := True;

    InsertSQL :=
      'INSERT INTO ' + ATableName +
      ' (' + FieldNames + ') VALUES (';

    for f := 0 to FieldsPerRow - 1 do
    begin
      if f > 0 then
        InsertSQL := InsertSQL + ', ';

      InsertSQL := InsertSQL +
        ':P' + IntToStr(f);
    end;

    InsertSQL := InsertSQL + ')';

    Query.SQL.Text := InsertSQL;
    Query.Prepare;

    { ---------------------------------------------------------- }
    { Startwerte                                                  }
    { ---------------------------------------------------------- }

    CurrentRow := 0;
    CommitCount := 0;
    NextCommitRow := CommitInterval;

    StartTime := Now;

    FDataSet.First;

    { ---------------------------------------------------------- }
    { Hauptschleife                                               }
    { ---------------------------------------------------------- }

    while not FDataSet.EOF do
    begin
      if FCancelled then
        Break;

      RowsThisBatch := 0;
      RowsInThisBatch := 0;

      { -------------------------------------------------------- }
      { Ein Batch abarbeiten                                     }
      { -------------------------------------------------------- }

      while (not FDataSet.EOF) and
            (RowsInThisBatch < ROWS_PER_BATCH) do
      begin
        if FCancelled then
          Break;

        { ------------------------------------------------------ }
        { Parameter mit den Werten des aktuellen Datensatzes     }
        { ------------------------------------------------------ }

        for f := 0 to FieldsPerRow - 1 do
        begin
          SourceField :=
            FDataSet.FieldByName(
              FFields[CheckedFields[f]].FieldName);

          Param := Query.Params[f];

          if SourceField.IsNull then
          begin
            Param.Clear;
          end
          else
          begin
            case SourceField.DataType of

              ftSmallint:
                Param.AsSmallInt :=
                  SourceField.AsInteger;

              ftWord:
                Param.AsInteger :=
                  SourceField.AsInteger;

              ftInteger:
                Param.AsInteger :=
                  SourceField.AsInteger;

              ftLargeint:
                Param.AsLargeInt :=
                  SourceField.AsLargeInt;

              ftAutoInc:
                Param.AsInteger :=
                  SourceField.AsInteger;

              ftFloat,
              ftCurrency,
              ftBCD,
              ftFMTBcd:
                Param.AsFloat :=
                  SourceField.AsFloat;

              ftDateTime,
              ftTimeStamp,
              ftDate,
              ftTime:
                Param.AsDateTime :=
                  SourceField.AsDateTime;

              ftBoolean:
                Param.AsBoolean :=
                  SourceField.AsBoolean;

            else
              Param.AsString :=
                SourceField.AsString;
            end;
          end;
        end;

        { ------------------------------------------------------ }
        { INSERT ausführen                                        }
        { ------------------------------------------------------ }

        Query.ExecSQL;

        Inc(CurrentRow);
        Inc(RowsInThisBatch);
        Inc(RowsThisBatch);

        FDataSet.Next;

        { ------------------------------------------------------ }
        { Fortschritt aktualisieren                              }
        { ------------------------------------------------------ }

        if (CurrentRow mod 50 = 0) or
           (CurrentRow = TotalRows) then
        begin
          if TotalRows > 0 then
            ProgressBar1.Position :=
              Round((CurrentRow / TotalRows) * 100);

          ElapsedSeconds :=
            (Now - StartTime) * 86400;

          if ElapsedSeconds > 0 then
            RowsPerSecond :=
              CurrentRow / ElapsedSeconds
          else
            RowsPerSecond := 0;

          if RowsPerSecond > 0 then
            MsgText :=
              Format(
                'Importiere Datensatz %d von %d (%.1f Datensätze/s)',
                [CurrentRow, TotalRows, RowsPerSecond])
          else
            MsgText :=
              Format(
                'Importiere Datensatz %d von %d',
                [CurrentRow, TotalRows]);

          lblStatus.Caption := MsgText;

          Application.ProcessMessages;
        end;
      end;

      { -------------------------------------------------------- }
      { CommitRetaining nach CommitInterval                      }
      { -------------------------------------------------------- }

      if (CurrentRow >= NextCommitRow) and
         DestTrans.Active and
         (not FCancelled) then
      begin
        DestTrans.CommitRetaining;

        Inc(CommitCount);

        while NextCommitRow <= CurrentRow do
          Inc(NextCommitRow, CommitInterval);

        Application.ProcessMessages;
      end;
    end;

    { ---------------------------------------------------------- }
    { Abbruch                                                    }
    { ---------------------------------------------------------- }

    if FCancelled then
    begin
      if DestTrans.Active then
        DestTrans.Rollback;

      lblStatus.Caption :=
        Format(
          'Import abgebrochen bei Datensatz %d von %d.',
          [CurrentRow, TotalRows]);

      ProgressBar1.Position :=
        IfThen(TotalRows > 0,
               Round((CurrentRow / TotalRows) * 100),
               0);

      Exit;
    end;

    { ---------------------------------------------------------- }
    { Letzten Rest endgültig committen                           }
    { ---------------------------------------------------------- }

    if DestTrans.Active then
      DestTrans.Commit;

    { ---------------------------------------------------------- }
    { Abschlussanzeige                                           }
    { ---------------------------------------------------------- }

    ProgressBar1.Position := 100;

    ElapsedSeconds :=
      (Now - StartTime) * 86400;

    if ElapsedSeconds > 0 then
      RowsPerSecond :=
        CurrentRow / ElapsedSeconds
    else
      RowsPerSecond := 0;

    if RowsPerSecond > 0 then
      lblStatus.Caption :=
        Format(
          'Import abgeschlossen: %d Datensätze in %.1f Sekunden (%.1f Datensätze/s).',
          [CurrentRow, ElapsedSeconds, RowsPerSecond])
    else
      lblStatus.Caption :=
        Format(
          'Import abgeschlossen: %d Datensätze.',
          [CurrentRow]);

  finally
    Query.Free;
  end;
end; }

procedure TfrmCreateTableFromDataSet.RunFBInsertBatched(
  ADBIndex: Integer; const ATableName: string);
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
  InsertSQL: string;

  CheckedFields: array of Integer;
  FieldsPerRow: Integer;

  FromRow, ToRow, TotalRows: Integer;
  TotalCommits: Integer;
  CommitsDone: Integer;

  i, f: Integer;
  CurrentRow: Integer;

  BatchSize: Integer;
  RowsSinceCommit: Integer;
  RemainingRows: Integer;

  SourceField: TField;
  Param: TParam;

  OldDecimalSep: Char;
begin
  { ------------------------------------------------------------- }
  { Ziel-Datenbank / Transaktion                                  }
  { ------------------------------------------------------------- }

  DestDB := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;

  if not DestDB.Connected then
    DestDB.Connected := True;

  if not DestTrans.InTransaction then
    DestTrans.StartTransaction;

  { ------------------------------------------------------------- }
  { EINZIGE Batch-Größe: Einstellung aus dem Formular             }
  { ------------------------------------------------------------- }

  BatchSize := GetCommitInterval;

  if BatchSize < 1 then
    BatchSize := 100000;

  { ------------------------------------------------------------- }
  { Felder sammeln                                                }
  { ------------------------------------------------------------- }

  SetLength(CheckedFields, 0);
  FieldNames := '';

  for i := 0 to High(FFields) do
  begin
    if FFields[i].Checked then
    begin
      SetLength(
        CheckedFields,
        Length(CheckedFields) + 1
      );

      CheckedFields[High(CheckedFields)] := i;

      if FieldNames <> '' then
        FieldNames := FieldNames + ', ';

      FieldNames := FieldNames + FFields[i].FieldName;
    end;
  end;

  FieldsPerRow := Length(CheckedFields);

  if FieldsPerRow = 0 then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  { ------------------------------------------------------------- }
  { Bereich bestimmen                                            }
  { ------------------------------------------------------------- }

  FromRow := GetFromRow;
  ToRow := GetToRow;

  if ToRow > FDataSet.RecordCount then
    ToRow := FDataSet.RecordCount;

  TotalRows := ToRow - FromRow + 1;

  if TotalRows <= 0 then
  begin
    ShowMessage('No rows to copy.');
    Exit;
  end;

  { Anzahl der erwarteten Commits nur für Anzeige }
  TotalCommits :=
    (TotalRows + BatchSize - 1) div BatchSize;

  CommitsDone := 0;

  { ------------------------------------------------------------- }
  { Query / Progress                                             }
  { ------------------------------------------------------------- }

  Query := TIBQuery.Create(nil);
  ProgressForm := TForm.Create(nil);

  OldDecimalSep :=
    DefaultFormatSettings.DecimalSeparator;

  StartTime := Now;
  EndTime := StartTime;

  CurrentRow := 0;
  RowsSinceCommit := 0;

  FCancelled := False;

  try
    DefaultFormatSettings.DecimalSeparator := '.';

    { ----------------------------------------------------------- }
    { Query konfigurieren                                        }
    { ----------------------------------------------------------- }

    Query.Database := DestDB;
    Query.Transaction := DestTrans;
    Query.AllowAutoActivateTransaction := True;

    { ----------------------------------------------------------- }
    { PARAMETRISIERTER INSERT                                    }
    { ----------------------------------------------------------- }

    InsertSQL :=
      'INSERT INTO ' + ATableName +
      ' (' + FieldNames + ') VALUES (';

    for f := 0 to FieldsPerRow - 1 do
    begin
      if f > 0 then
        InsertSQL := InsertSQL + ', ';

      InsertSQL :=
        InsertSQL + ':P' + IntToStr(f);
    end;

    InsertSQL := InsertSQL + ')';

    Query.SQL.Text := InsertSQL;
    Query.Prepare;

    { ----------------------------------------------------------- }
    { ProgressForm                                                }
    { ----------------------------------------------------------- }

    ProgressForm.FormStyle := fsNormal;
    ProgressForm.Caption :=
      'Copying data to ' + ATableName;
    ProgressForm.Width := 540;
    ProgressForm.Height := 320;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    { ----------------------------------------------------------- }
    { Zeilen-Label                                                }
    { ----------------------------------------------------------- }

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption :=
      Format(
        'Total: %s rows   |   Batch: %s rows',
        [
          FormatFloat('#,##0', TotalRows),
          FormatFloat('#,##0', BatchSize)
        ]
      );
    ProgressLabel.Width := 500;

    { ----------------------------------------------------------- }
    { Zeilen-ProgressBar                                          }
    { ----------------------------------------------------------- }

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 40;
    ProgressBar.Width := 500;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := TotalRows;
    ProgressBar.Position := 0;

    { ----------------------------------------------------------- }
    { Commit-Label                                                 }
    { ----------------------------------------------------------- }

    LblCommit := TLabel.Create(ProgressForm);
    LblCommit.Parent := ProgressForm;
    LblCommit.Left := 16;
    LblCommit.Top := 75;
    LblCommit.Caption :=
      Format(
        'Commits: 0 of %d   (every %s rows)',
        [
          TotalCommits,
          FormatFloat('#,##0', BatchSize)
        ]
      );
    LblCommit.Width := 500;

    { ----------------------------------------------------------- }
    { Commit-ProgressBar                                          }
    { ----------------------------------------------------------- }

    CommitBar := TProgressBar.Create(ProgressForm);
    CommitBar.Parent := ProgressForm;
    CommitBar.Left := 16;
    CommitBar.Top := 100;
    CommitBar.Width := 500;
    CommitBar.Height := 20;
    CommitBar.Min := 0;
    CommitBar.Max := TotalCommits;
    CommitBar.Position := 0;

    { ----------------------------------------------------------- }
    { Elapsed-Label                                               }
    { ----------------------------------------------------------- }

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 135;
    LblElapsed.Caption :=
      'Elapsed: 00:00:00   |   0 rows/sec';
    LblElapsed.Width := 500;

    { ----------------------------------------------------------- }
    { Cancel-Button                                               }
    { ----------------------------------------------------------- }

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 210;
    BtnCancel.Top := 180;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelButtonClick;

    ProgressForm.Show;
    Application.ProcessMessages;

    { ----------------------------------------------------------- }
    { Dataset vorbereiten                                         }
    { ----------------------------------------------------------- }

    FDataSet.DisableControls;

    try
      FDataSet.First;

      for i := 1 to FromRow - 1 do
        FDataSet.Next;

      { ========================================================= }
      { EINZIGE INSERT-SCHLEIFE                                  }
      { ========================================================= }

      while (not FDataSet.EOF) and
            (CurrentRow < TotalRows) do
      begin
        if FCancelled then
          Break;

        { ------------------------------------------------------- }
        { Parameter des aktuellen Datensatzes setzen             }
        { ------------------------------------------------------- }

        for f := 0 to FieldsPerRow - 1 do
        begin
          SourceField :=
            FDataSet.FieldByName(
              FFields[CheckedFields[f]].FieldName
            );

          Param := Query.Params[f];

          if SourceField.IsNull then
          begin
            Param.Clear;
          end
          else
          begin
            case SourceField.DataType of

              ftSmallint:
                Param.AsSmallInt :=
                  SourceField.AsInteger;

              ftWord:
                Param.AsInteger :=
                  SourceField.AsInteger;

              ftInteger:
                Param.AsInteger :=
                  SourceField.AsInteger;

              ftLargeint:
                Param.AsLargeInt :=
                  SourceField.AsLargeInt;

              ftAutoInc:
                Param.AsInteger :=
                  SourceField.AsInteger;

              ftFloat,
              ftCurrency,
              ftBCD,
              ftFMTBcd:
                Param.AsFloat :=
                  SourceField.AsFloat;

              ftDateTime,
              ftTimeStamp,
              ftDate,
              ftTime:
                Param.AsDateTime :=
                  SourceField.AsDateTime;

              ftBoolean:
                Param.AsBoolean :=
                  SourceField.AsBoolean;

            else
              Param.AsString :=
                SourceField.AsString;
            end;
          end;
        end;

        { ------------------------------------------------------- }
        { INSERT                                                   }
        { ------------------------------------------------------- }

        Query.ExecSQL;

        Inc(CurrentRow);
        Inc(RowsSinceCommit);

        FDataSet.Next;

        { ------------------------------------------------------- }
        { Fortschritt aktualisieren                               }
        { ------------------------------------------------------- }

        ProgressBar.Position := CurrentRow;

        ProgressLabel.Caption :=
          Format(
            'Total: %s rows   |   Batch: %s rows   |   Current: %s',
            [
              FormatFloat('#,##0', TotalRows),
              FormatFloat('#,##0', BatchSize),
              FormatFloat('#,##0', CurrentRow)
            ]
          );

        LblElapsed.Caption :=
          'Elapsed: ' +
          FormatDateTime(
            'hh:nn:ss',
            Now - StartTime
          ) +
          '   |   ' +
          FormatFloat(
            '#,##0',
            Round(
              CurrentRow /
              Max(
                0.001,
                (Now - StartTime) * 86400
              )
            )
          ) +
          ' rows/sec';

        { ------------------------------------------------------- }
        { BATCHSIZE erreicht -> CommitRetaining                  }
        { ------------------------------------------------------- }

        if RowsSinceCommit >= BatchSize then
        begin
          DestTrans.CommitRetaining;

          RowsSinceCommit := 0;

          Inc(CommitsDone);

          CommitBar.Position := CommitsDone;

          LblCommit.Caption :=
            Format(
              'Commits: %d of %d   (every %s rows)',
              [
                CommitsDone,
                TotalCommits,
                FormatFloat('#,##0', BatchSize)
              ]
            );

          Application.ProcessMessages;
        end;
      end;

      { --------------------------------------------------------- }
      { Finaler Commit für den letzten Rest                       }
      { --------------------------------------------------------- }

      if not FCancelled then
      begin
        if DestTrans.InTransaction then
          DestTrans.Commit;

        if RowsSinceCommit > 0 then
          Inc(CommitsDone);

        CommitBar.Position := CommitsDone;

        LblCommit.Caption :=
          Format(
            'Commits: %d of %d   (every %s rows)',
            [
              CommitsDone,
              TotalCommits,
              FormatFloat('#,##0', BatchSize)
            ]
          );

        Application.ProcessMessages;
      end;

    finally
      EndTime := Now;
      FDataSet.EnableControls;
    end;

    { ------------------------------------------------------------- }
    { Erfolgreich abgeschlossen                                    }
    { ------------------------------------------------------------- }

    if not FCancelled then
    begin
      ShowMessage(
        Format(
          'Data copy completed!' + sLineBreak +
          sLineBreak +
          'Rows:     %s' + sLineBreak +
          'Time:     %s' + sLineBreak +
          'Speed:    %s rows/sec' + sLineBreak +
          'Batch:    %s rows' + sLineBreak +
          'Commits:  %d',
          [
            FormatFloat('#,##0', CurrentRow),

            FormatDateTime(
              'hh:nn:ss',
              EndTime - StartTime
            ),

            FormatFloat(
              '#,##0',
              Round(
                CurrentRow /
                Max(
                  0.001,
                  (EndTime - StartTime) * 86400
                )
              )
            ),

            FormatFloat(
              '#,##0',
              BatchSize
            ),

            CommitsDone
          ]
        )
      );
    end
    else
    begin
      { ----------------------------------------------------------- }
      { Abgebrochen                                                 }
      { ----------------------------------------------------------- }

      if DestTrans.InTransaction then
        DestTrans.Rollback;

      RemainingRows :=
        TotalRows - CurrentRow;

      ShowMessage(
        Format(
          'Copy cancelled by user!' + sLineBreak +
          sLineBreak +
          'Rows copied:  %s of %s' + sLineBreak +
          'Rows skipped: %s' + sLineBreak +
          sLineBreak +
          'Time:     %s' + sLineBreak +
          'Speed:    %s rows/sec' + sLineBreak +
          'Commits:  %d' + sLineBreak +
          sLineBreak +
          'Note: Not-committed data has been rolled back.' +
          sLineBreak +
          'Please check the destination table.',
          [
            FormatFloat('#,##0', CurrentRow),

            FormatFloat('#,##0', TotalRows),

            FormatFloat('#,##0', RemainingRows),

            FormatDateTime(
              'hh:nn:ss',
              EndTime - StartTime
            ),

            FormatFloat(
              '#,##0',
              Round(
                CurrentRow /
                Max(
                  0.001,
                  (EndTime - StartTime) * 86400
                )
              )
            ),

            CommitsDone
          ]
        )
      );
    end;

  finally
    DefaultFormatSettings.DecimalSeparator :=
      OldDecimalSep;

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
