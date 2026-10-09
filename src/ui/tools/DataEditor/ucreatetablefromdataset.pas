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
  uthemeselector,
  uReport,
  uSystemInfo;

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
    edtLineBuffer: TEdit;
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
    Label5: TLabel;
    lblHint: TLabel;
    Label8: TLabel;
    lblStatus: TLabel;
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
    procedure cmbBoxDBsChange(Sender: TObject);
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

    // Statistik für den Transfer-Report
    FStats: TTransferStatistic;

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
    procedure UpdateTypePickList;

    procedure InitTransferStats(ADBIndex: Integer; const ATableName: string);
    procedure ShowTransferReport;
  public
    procedure Init(ADataSet: TDataSet; const AFileName: string);
  end;


implementation

{$R *.lfm}

// ============================================================================
// TRANSFER-REPORT
// ============================================================================

procedure TfrmCreateTableFromDataSet.InitTransferStats(
  ADBIndex: Integer; const ATableName: string);
var
  FileSize: Int64;
  FieldCount, i: Integer;
begin
  FStats := Default(TTransferStatistic);

  FStats.Kind           := tkCreateTable;
  FStats.SourceKind     := 'File';
  FStats.SourceFileName := FFileName;

  FStats.SourceFileFormat := UpperCase(ExtractFileExt(FFileName));
  if (FStats.SourceFileFormat <> '') and (FStats.SourceFileFormat[1] = '.') then
    Delete(FStats.SourceFileFormat, 1, 1);

  // Dateigröße ermitteln
  if FileExists(FFileName) then
  begin
    try
      with TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone) do
      try
        FileSize := Size;
      finally
        Free;
      end;
      FStats.SourceFileSize := FileSize;
    except
      // Datei nicht lesbar → Größe bleibt 0
    end;
  end;

  // Ziel-Datenbank
  FStats.DestKind          := 'Firebird Table';
  FStats.DestServer        := RegisteredDatabases[ADBIndex].RegRec.ServerName;
  FStats.DestDatabase      := RegisteredDatabases[ADBIndex].RegRec.Title;
  FStats.DestTable         := ATableName;
  FStats.DestServerVersion := RegisteredDatabases[ADBIndex].RegRec.ServerVersionString;

  // Optionen
  FStats.BatchSize := GetCommitInterval;
  FStats.FromRow   := GetFromRow;
  FStats.ToRow     := GetToRow;

  if FDataSet <> nil then
    FStats.UseRowRange := (FStats.FromRow > 1) or
                          ((FStats.ToRow > 0) and
                           (FStats.ToRow < FDataSet.RecordCount))
  else
    FStats.UseRowRange := (FStats.FromRow > 1) or (FStats.ToRow > 0);

  if chkboxCopyData.Checked then
    FStats.OptionsExtra := 'Create Table + Insert Data'
  else
    FStats.OptionsExtra := 'Create Table Only';

  // Ausgewählte Felder zählen
  FieldCount := 0;
  for i := 0 to High(FFields) do
    if FFields[i].Checked then
      Inc(FieldCount);
  FStats.FieldsCount := FieldCount;

  // Client-Lib-Version
  if Assigned(RegisteredDatabases[ADBIndex].IBDatabase) and
     Assigned(RegisteredDatabases[ADBIndex].IBDatabase.FirebirdAPI) then
    FStats.ClientLibVersion := 'Firebird ' +
      RegisteredDatabases[ADBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;

  // Environment (OS, CPU, RAM, Disk)
  try
    FStats.SystemInfo := GetSystemInfo(
      GetDBFileNameFromConnectionString(
        RegisteredDatabases[ADBIndex].RegRec.DatabaseName));
  except
    // Umgebungs-Info ist optional
  end;
end;

procedure TfrmCreateTableFromDataSet.ShowTransferReport;
var
  ReportForm: TfrmReport;
begin
  ReportForm := TfrmReport.Create(nil);
  try
    ReportForm.SetReportText(FormatTransferReport(FStats));
    ReportForm.ShowModal;
  finally
    ReportForm.Free;
  end;
end;

// ============================================================================
// FORM-EVENTS
// ============================================================================

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

  // PickList dynamisch nach Server/DB-Auswahl füllen
  UpdateTypePickList;

  LoadFieldList;
end;

// ============================================================================
// GRID-SETUP
// ============================================================================

procedure TfrmCreateTableFromDataSet.SetupGridColumns;
begin
  sgFields.FixedCols := 0;
  sgFields.FixedRows := 1;
  sgFields.ColCount := 5;
  sgFields.RowCount := 1;

  sgFields.Options := sgFields.Options +
    [goEditing, goRowSelect, goVertLine, goHorzLine];

  while sgFields.Columns.Count < 5 do
    sgFields.Columns.Add;

  sgFields.Columns[0].Title.Caption := 'Include';
  sgFields.Columns[1].Title.Caption := 'Field Name';
  sgFields.Columns[2].Title.Caption := 'Data Type';
  sgFields.Columns[3].Title.Caption := 'Size';
  sgFields.Columns[4].Title.Caption := 'Not Null';

  sgFields.ColWidths[0] := 60;
  sgFields.ColWidths[1] := 180;
  sgFields.ColWidths[2] := 160;
  sgFields.ColWidths[3] := 60;
  sgFields.ColWidths[4] := 70;

  sgFields.Columns[0].ButtonStyle := cbsCheckboxColumn;
  sgFields.Columns[2].ButtonStyle := cbsPickList;
  sgFields.Columns[4].ButtonStyle := cbsCheckboxColumn;

end;

procedure TfrmCreateTableFromDataSet.UpdateTypePickList;
var
  DBIndex: Integer;
  AMajor, AMinor: Word;
  AVersion: Word;
  TypeList: TStringList;
begin
  sgFields.Columns[2].PickList.Clear;

  DBIndex := GetTargetDBIndex;
  if DBIndex < 0 then
  begin
    StatusBar1.SimpleText := 'No destination database selected.';
    Exit;
  end;

  ParseFBVersionString(
    RegisteredDatabases[DBIndex].RegRec.ServerVersionString,
    AMajor, AMinor);
  AVersion := AMajor * 10 + AMinor;

  TypeList := TStringList.Create;
  try
    GetDataTypesByFBVersion(AVersion, TypeList);

    if TypeList.Count = 0 then
    begin
      StatusBar1.SimpleText :=
        'Unknown server version: FB ' + IntToStr(AMajor) + '.' +
        IntToStr(AMinor) + ' — no data types available.';
      Exit;
    end;

    sgFields.Columns[2].PickList.Assign(TypeList);

    StatusBar1.SimpleText :=
      'Data types for Firebird ' + IntToStr(AMajor) + '.' +
      IntToStr(AMinor) + ' (' + IntToStr(TypeList.Count) + ' types)';
  finally
    TypeList.Free;
  end;
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
    Result := BaseType + '(' + IntToStr(Size) + ',2)';
  end
  else
    Result := BaseType;
end;

// ============================================================================
// COMBO-BEFÜLLUNG
// ============================================================================

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

procedure TfrmCreateTableFromDataSet.cmbBoxServersChange(Sender: TObject);
begin
  if cmbBoxServers.ItemIndex < 0 then Exit;

  FillDBCombo;
  UpdateTypePickList;
end;

// ============================================================================
// FELDLISTE
// ============================================================================

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
    sgFields.Cells[0, i + 1] := '1';
    sgFields.Cells[1, i + 1] := FFields[i].FieldName;
    ParseFieldType(FFields[i].FieldType, BaseType, Size);
    sgFields.Cells[2, i + 1] := BaseType;
    if Size > 0 then
      sgFields.Cells[3, i + 1] := IntToStr(Size)
    else
      sgFields.Cells[3, i + 1] := '';
    sgFields.Cells[4, i + 1] := '0';
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
    FFields[i].FieldType  := GridRowToFieldType(i + 1);
    FFields[i].Checked    := SameText(sgFields.Cells[0, i + 1], '1');
    FFields[i].CharLength := StrToIntDef(Trim(sgFields.Cells[3, i + 1]), 0);

    if i < chkLstFields.Count then
      chkLstFields.Checked[i] := FFields[i].Checked;
  end;
end;

// ============================================================================
// SELECTION BUTTONS
// ============================================================================

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

procedure TfrmCreateTableFromDataSet.cmbBoxDBsChange(Sender: TObject);
begin
  UpdateTypePickList;
end;

// ============================================================================
// GETTER
// ============================================================================

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

procedure TfrmCreateTableFromDataSet.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

procedure TfrmCreateTableFromDataSet.btnMainCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
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

// ============================================================================
// HAUPTAKTION
// ============================================================================

procedure TfrmCreateTableFromDataSet.btnRunClick(Sender: TObject);
var
  DBIndex, i: Integer;
  TableName, SQL, FieldList: string;
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  Script: TIBXScript;
  StartTime: TDateTime;
  TableAlreadyExists: Boolean;
  DidCopyData: Boolean;
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

  // --- Statistik vorbereiten ---
  InitTransferStats(DBIndex, TableName);

  StartTime := Now;
  DidCopyData := False;

  Script := TIBXScript.Create(nil);
  try
    Script.Database := DestDB;
    Script.Transaction := DestTrans;

    TableAlreadyExists := TableExists(DestDB, TableName);

    if TableAlreadyExists then
    begin
      if MessageDlg('Table "' + TableName + '" already exists. Drop and recreate?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        SQL := 'DROP TABLE ' + TableName;
        Script.ExecSQLScript(SQL);
        DestTrans.CommitRetaining;
        TableAlreadyExists := False;  // wird jetzt neu erstellt
      end;
    end;

    // ========================================================================
    // TABLE CREATION
    // ========================================================================
    if not TableAlreadyExists then
    begin
      FieldList := '';
      for i := 0 to High(FFields) do
      begin
        if not FFields[i].Checked then Continue;
        if FieldList <> '' then
          FieldList := FieldList + ',' + sLineBreak;
        FieldList := FieldList + '  ' + FFields[i].FieldName + ' ' + FFields[i].FieldType;
      end;
      SQL := 'CREATE TABLE ' + TableName + ' (' + sLineBreak +
             FieldList + sLineBreak + ')';
      Script.ExecSQLScript(SQL);
      DestTrans.CommitRetaining;

      FStats.CreateTableSQL := SQL;
    end
    else
    begin
      // Tabelle existierte bereits und wurde NICHT neu erstellt
      FStats.OptionsExtra := 'Insert Data Only (table already existed)';
    end;

    // ========================================================================
    // DATA COPY
    // ========================================================================
    if chkboxCopyData.Checked then
    begin
      RunFBInsertBatched(DBIndex, TableName);
      DidCopyData := True;
    end;

  finally
    Script.Free;
  end;

  // --- Gesamtzeit für den Vorgang ---
  FStats.ElapsedSeconds := (Now - StartTime) * SecsPerDay;

  // --- Report anzeigen (immer wenn etwas passiert ist) ---
  if (FStats.CreateTableSQL <> '') or DidCopyData then
    ShowTransferReport;
end;

// ============================================================================
// BATCHED INSERT
// ============================================================================

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

  SourceField: TField;
  Param: TParam;

  OldDecimalSep: Char;
begin
  // --- Ziel-DB / Transaktion ---
  DestDB := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;

  if not DestDB.Connected then
    DestDB.Connected := True;

  if not DestTrans.InTransaction then
    DestTrans.StartTransaction;

  // --- Batch-Größe ---
  BatchSize := GetCommitInterval;
  if BatchSize < 1 then
    BatchSize := 100000;

  // --- Felder sammeln ---
  SetLength(CheckedFields, 0);
  FieldNames := '';

  for i := 0 to High(FFields) do
  begin
    if FFields[i].Checked then
    begin
      SetLength(CheckedFields, Length(CheckedFields) + 1);
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

  // --- Bereich ---
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

  TotalCommits := (TotalRows + BatchSize - 1) div BatchSize;
  CommitsDone := 0;

  // --- Query / Progress ---
  Query := TIBQuery.Create(nil);
  ProgressForm := TForm.Create(nil);

  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;

  StartTime := Now;
  EndTime := StartTime;

  CurrentRow := 0;
  RowsSinceCommit := 0;

  FCancelled := False;

  try
    DefaultFormatSettings.DecimalSeparator := '.';

    // --- Query konfigurieren ---
    Query.Database := DestDB;
    Query.Transaction := DestTrans;
    Query.AllowAutoActivateTransaction := True;

    // --- Parametrisierter INSERT ---
    InsertSQL :=
      'INSERT INTO ' + ATableName +
      ' (' + FieldNames + ') VALUES (';

    for f := 0 to FieldsPerRow - 1 do
    begin
      if f > 0 then
        InsertSQL := InsertSQL + ', ';
      InsertSQL := InsertSQL + ':P' + IntToStr(f);
    end;

    InsertSQL := InsertSQL + ')';

    Query.SQL.Text := InsertSQL;
    Query.Prepare;

    // --- ProgressForm ---
    ProgressForm.FormStyle := fsNormal;
    ProgressForm.Caption := 'Copying data to ' + ATableName;
    ProgressForm.Width := 540;
    ProgressForm.Height := 320;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := Format('Total: %s rows   |   Batch: %s rows',
      [FormatFloat('#,##0', TotalRows), FormatFloat('#,##0', BatchSize)]);
    ProgressLabel.Width := 500;

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 40;
    ProgressBar.Width := 500;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := TotalRows;
    ProgressBar.Position := 0;

    LblCommit := TLabel.Create(ProgressForm);
    LblCommit.Parent := ProgressForm;
    LblCommit.Left := 16;
    LblCommit.Top := 75;
    LblCommit.Caption := Format('Commits: 0 of %d   (every %s rows)',
      [TotalCommits, FormatFloat('#,##0', BatchSize)]);
    LblCommit.Width := 500;

    CommitBar := TProgressBar.Create(ProgressForm);
    CommitBar.Parent := ProgressForm;
    CommitBar.Left := 16;
    CommitBar.Top := 100;
    CommitBar.Width := 500;
    CommitBar.Height := 20;
    CommitBar.Min := 0;
    CommitBar.Max := TotalCommits;
    CommitBar.Position := 0;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 135;
    LblElapsed.Caption := 'Elapsed: 00:00:00   |   0 rows/sec';
    LblElapsed.Width := 500;

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 210;
    BtnCancel.Top := 180;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelButtonClick;

    ProgressForm.Show;
    Application.ProcessMessages;

    // --- Dataset vorbereiten ---
    FDataSet.DisableControls;

    try
      FDataSet.First;
      for i := 1 to FromRow - 1 do
        FDataSet.Next;

      // =====================================================================
      // INSERT-SCHLEIFE
      // =====================================================================
      while (not FDataSet.EOF) and (CurrentRow < TotalRows) do
      begin
        if FCancelled then
          Break;

        // --- Parameter setzen ---
        for f := 0 to FieldsPerRow - 1 do
        begin
          SourceField := FDataSet.FieldByName(FFields[CheckedFields[f]].FieldName);
          Param := Query.Params[f];

          if SourceField.IsNull then
            Param.Clear
          else
            case SourceField.DataType of
              ftSmallint:
                Param.AsSmallInt := SourceField.AsInteger;

              ftWord:
                Param.AsInteger := SourceField.AsInteger;

              ftInteger:
                Param.AsInteger := SourceField.AsInteger;

              ftLargeint:
                Param.AsLargeInt := SourceField.AsLargeInt;

              ftAutoInc:
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

        // --- INSERT ---
        Query.ExecSQL;

        Inc(CurrentRow);
        Inc(RowsSinceCommit);

        FDataSet.Next;

        // --- Fortschritt ---
        ProgressBar.Position := CurrentRow;
        ProgressLabel.Caption := Format(
          'Total: %s rows   |   Batch: %s rows   |   Current: %s',
          [FormatFloat('#,##0', TotalRows),
           FormatFloat('#,##0', BatchSize),
           FormatFloat('#,##0', CurrentRow)]);

        LblElapsed.Caption :=
          'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
          '   |   ' +
          FormatFloat('#,##0',
            Round(CurrentRow / Max(0.001, (Now - StartTime) * 86400))) +
          ' rows/sec';

        // --- CommitRetaining alle BatchSize Zeilen ---
        if RowsSinceCommit >= BatchSize then
        begin
          DestTrans.CommitRetaining;
          RowsSinceCommit := 0;
          Inc(CommitsDone);

          CommitBar.Position := CommitsDone;
          LblCommit.Caption := Format(
            'Commits: %d of %d   (every %s rows)',
            [CommitsDone, TotalCommits, FormatFloat('#,##0', BatchSize)]);

          Application.ProcessMessages;
        end;
      end;

       Application.ProcessMessages;


      // --- Finaler Commit ---
      if not FCancelled then
      begin
        if DestTrans.InTransaction then
          DestTrans.Commit;

        if RowsSinceCommit > 0 then
          Inc(CommitsDone);

        CommitBar.Position := CommitsDone;
        LblCommit.Caption := Format(
          'Commits: %d of %d   (every %s rows)',
          [CommitsDone, TotalCommits, FormatFloat('#,##0', BatchSize)]);

        Application.ProcessMessages;
      end;

    finally
      EndTime := Now;
      FDataSet.EnableControls;
    end;

    // =======================================================================
    // STATISTIK FÜLLEN (statt ShowMessage)
    // =======================================================================
    FStats.RowsProcessed := CurrentRow;

    if FStats.OptionsExtra = '' then
      FStats.OptionsExtra := Format('Batch %s rows, Commits %d',
        [FormatFloat('#,##0', BatchSize), CommitsDone])
    else
      FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
        '  ' + Format('Batch %s rows, Commits %d',
          [FormatFloat('#,##0', BatchSize), CommitsDone]);

    if FCancelled then
    begin
      // Nicht-committete Daten zurückrollen
      if DestTrans.InTransaction then
        DestTrans.Rollback;

      FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
        Format('  CANCELLED after %s of %s rows',
          [FormatFloat('#,##0', CurrentRow),
           FormatFloat('#,##0', TotalRows)]);
    end;

  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
    Query.Free;
    ProgressForm.Free;
  end;
end;

// ============================================================================
// TABELLEN-EXISTENZ
// ============================================================================

function TfrmCreateTableFromDataSet.TableExists(
  ADB: TIBDatabase; const ATableName: string): Boolean;
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

// ============================================================================
// UNUSED (bleibt für Kompatibilität drin)
// ============================================================================

procedure TfrmCreateTableFromDataSet.AssignParamFromField(
  Param: TParam; SourceField: TField);
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

end.
