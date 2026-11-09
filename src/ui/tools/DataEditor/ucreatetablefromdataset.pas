unit uCreateTableFromDataSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, CheckLst, Grids, DB, Math, DateUtils,
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
      NotNull: Boolean;
    end;
    FCancelled: Boolean;
    FStats: TTransferStatistic;
    FMarginApplied: Boolean;
    FTotalRows: Integer;
    FTotalRowsAnalyzed: Integer;

    procedure LoadFieldList;
    procedure FillServerCombo;
    procedure FillDBCombo;
    function  GetTargetDBIndex: Integer;
    function  GetTargetTable: string;
    function  GetCommitInterval: Int64;
    function  GetFromRow: Int64;
    function  GetToRow: Int64;
    procedure RunFBInsertRowByRow(ADBIndex: Integer; const ATableName: string);
    procedure AssignParamFromField(Param: TParam; SourceField: TField;
      const TargetTypeString: string);
    procedure CancelButtonClick(Sender: TObject);
    function  TableExists(ADB: TIBDatabase; const ATableName: string): Boolean;
    procedure ParseFieldType(const AType: string; out BaseType: string; out Size: Integer);
    function  GridRowToFieldType(Row: Integer): string;
    procedure SyncFieldsFromGrid;
    procedure SetupGridColumns;
    procedure UpdateTypePickList;
    procedure InitTransferStats(ADBIndex: Integer; const ATableName: string);
    procedure ShowTransferReport;
    procedure AssignParamFromString(Param: TParam; const Value: string;
      AIsNull: Boolean; const TargetTypeString: string);
    function  FieldValueToSQLLiteral(const AValue: string; AField: TField; const ADestType: string): string;
    class function QuoteFieldName(const AName: string): string;
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
  FStats.Kind            := tkCreateTable;
  FStats.SourceKind      := 'File';
  FStats.SourceFileName  := FFileName;
  FStats.SourceFileFormat := UpperCase(ExtractFileExt(FFileName));
  if (FStats.SourceFileFormat <> '') and (FStats.SourceFileFormat[1] = '.') then
    Delete(FStats.SourceFileFormat, 1, 1);

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
    end;
  end;

  FStats.DestKind          := 'Firebird Table';
  FStats.DestServer        := RegisteredDatabases[ADBIndex].RegRec.ServerName;
  FStats.DestDatabase      := RegisteredDatabases[ADBIndex].RegRec.Title;
  FStats.DestTable         := ATableName;
  FStats.DestServerVersion := RegisteredDatabases[ADBIndex].RegRec.ServerVersionString;
  FStats.BatchSize         := GetCommitInterval;
  FStats.FromRow           := GetFromRow;
  FStats.ToRow             := GetToRow;

  if FDataSet <> nil then
    FStats.UseRowRange := (FStats.FromRow > 1) or
                          ((FStats.ToRow > 0) and (FStats.ToRow < FDataSet.RecordCount))
  else
    FStats.UseRowRange := (FStats.FromRow > 1) or (FStats.ToRow > 0);

  if chkboxCopyData.Checked then
    FStats.OptionsExtra := 'Create Table + Insert Data'
  else
    FStats.OptionsExtra := 'Create Table Only';

  if FMarginApplied then
    FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
      Format('  Safety margin applied: %d of %s rows analyzed ' +
             '(INTEGER→BIGINT, VARCHAR(n)→VARCHAR(2n))',
             [FTotalRowsAnalyzed, FormatFloat('#,##0', FTotalRows)]);

  FieldCount := 0;
  for i := 0 to High(FFields) do
    if FFields[i].Checked then
      Inc(FieldCount);
  FStats.FieldsCount := FieldCount;

  if Assigned(RegisteredDatabases[ADBIndex].IBDatabase) and
     Assigned(RegisteredDatabases[ADBIndex].IBDatabase.FirebirdAPI) then
    FStats.ClientLibVersion := 'Firebird ' +
      RegisteredDatabases[ADBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;

  try
    FStats.SystemInfo := GetSystemInfo(
      GetDBFileNameFromConnectionString(
        RegisteredDatabases[ADBIndex].RegRec.DatabaseName));
  except
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
  rbRange.OnChange   := @rbAllRowsChange;
  edtCommitInterval.Text := '100000';
  lblHint.Alignment := taCenter;
  lblHint.Layout    := tlCenter;
  lblHint.WordWrap  := True;
  lblHint.Caption   := 'Edit the fields as needed. Use Clone Table for data transformations.';
end;

procedure TfrmCreateTableFromDataSet.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

procedure TfrmCreateTableFromDataSet.Init(ADataSet: TDataSet; const AFileName: string);
begin
  FDataSet  := ADataSet;
  FFileName := AFileName;
  edtDestTableName.Text := UpperCase(ChangeFileExt(ExtractFileName(AFileName), ''));
  FillServerCombo;
  if cmbBoxServers.Items.Count > 0 then
  begin
    cmbBoxServers.ItemIndex := 0;
    FillDBCombo;
  end;
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
  sgFields.ColCount  := 5;
  sgFields.RowCount  := 1;
  sgFields.Options   := sgFields.Options + [goEditing, goRowSelect, goVertLine, goHorzLine];

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

procedure TfrmCreateTableFromDataSet.FillServerCombo;
var
  AliasList: TStringList;
begin
  AliasList := GetServerAliasListFromTreeView;
  if AliasList = nil then Exit;
  try
    cmbBoxServers.Items.Assign(AliasList);
    if cmbBoxServers.Items.Count = 0 then
      cmbBoxDBs.Text := '<No database available>';
  finally
    AliasList.Free;
  end;
end;

procedure TfrmCreateTableFromDataSet.FillDBCombo;
begin
  GetDBTitlesForServer(cmbBoxServers.Text, cmbBoxDBs.Items);
  if cmbBoxDBs.Items.Count = 0 then
    cmbBoxDBs.Items.Add('<No database available>');
  cmbBoxDBs.ItemIndex := 0;
end;

procedure TfrmCreateTableFromDataSet.cmbBoxServersChange(Sender: TObject);
begin
  if cmbBoxServers.Items.Count = 0 then
    Exit;
  FillDBCombo;
  UpdateTypePickList;
  LoadFieldList;   // ← NEU: Typen für die neue DB generieren
end;

// ============================================================================
// FELDLISTE
// ============================================================================

procedure TfrmCreateTableFromDataSet.LoadFieldList;
var
  Gen: TGenSQLFromCSVDataset;
  DBIndex, AVersion, i: Integer;
  BaseType: string;
  Size: Integer;
  HintText: string;
begin
  DBIndex := GetTargetDBIndex;

  // Kein Ziel gewählt → leeres Grid + Hinweis
  if DBIndex < 0 then
  begin
    SetLength(FFields, 0);
    chkLstFields.Clear;
    sgFields.RowCount := 1;
    lblHint.Caption :=
      'Please select a target server and database to load field types.';
    lblHint.Font.Color := clGray;
    Exit;
  end;

  // Firebird-Version der Ziel-DB
  AVersion := RegisteredDatabases[DBIndex].RegRec.ServerVersionMajor;
  if AVersion = 0 then
  begin
    SetLength(FFields, 0);
    chkLstFields.Clear;
    sgFields.RowCount := 1;
    lblHint.Caption :=
      'Could not determine Firebird version of target database. ' +
      'Please connect to the server first (right-click in main tree).';
    lblHint.Font.Color := clRed;
    Exit;
  end;

  Gen := TGenSQLFromCSVDataset.Create(FDataSet,
           UpperCase(ChangeFileExt(ExtractFileName(FFileName), '')),
           50, AVersion);
  try
    SetLength(FFields, Length(Gen.Fields));
    for i := 0 to High(Gen.Fields) do
    begin
      FFields[i].FieldName  := Gen.Fields[i].FieldName;
      FFields[i].FieldType  := Gen.Fields[i].FieldType;
      FFields[i].Checked    := True;
      FFields[i].CharLength := 0;
      FFields[i].NotNull    := False;
    end;
    FMarginApplied     := Gen.AnyMarginApplied;
    FTotalRows         := Gen.TotalRows;
    FTotalRowsAnalyzed := Gen.TotalRowsAnalyzed;
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

  if FMarginApplied then
  begin
    HintText := Format(
      'Note: only the first %d of %s rows were analyzed. ' +
      'Field types were enlarged as a safety margin ' +
      '(INTEGER→BIGINT, VARCHAR(n)→VARCHAR(2n)). ' +
      'If you know the exact file structure, edit types in the grid.',
      [FTotalRowsAnalyzed, FormatFloat('#,##0', FTotalRows)]);
    lblHint.Caption    := HintText;
    lblHint.Font.Color := clMaroon;
  end
  else
  begin
    lblHint.Caption    :=
      'Edit the fields as needed. Use Clone Table for data transformations.';
    lblHint.Font.Color := clWindowText;
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
    FFields[i].NotNull    := SameText(sgFields.Cells[4, i + 1], '1');

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
  LoadFieldList;
end;

// ============================================================================
// GETTER
// ============================================================================

function TfrmCreateTableFromDataSet.GetTargetDBIndex: Integer;
begin
  Result := FindDBIndexByTitleAndServer(
              Trim(cmbBoxDBs.Text),
              Trim(cmbBoxServers.Text));
end;

function TfrmCreateTableFromDataSet.GetTargetTable: string;
begin
  Result := Trim(edtDestTableName.Text);
end;

function TfrmCreateTableFromDataSet.GetCommitInterval: Int64;
begin
  Result := StrToIntDef(edtCommitInterval.Text, 100000);
  if Result < 1 then
    Result := 100000;
end;

function TfrmCreateTableFromDataSet.GetFromRow: Int64;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtFrom.Text, 1)
  else
    Result := 1;
  if Result < 1 then
    Result := 1;
end;

function TfrmCreateTableFromDataSet.GetToRow: Int64;
begin
  if FDataSet = nil then
  begin
    Result := 0;
    Exit;
  end;

  if rbRange.Checked then
    Result := StrToIntDef(edtTo.Text, FDataSet.RecordCount)
  else
    Result := FDataSet.RecordCount;

  if Result < 0 then
    Result := 0;
end;

procedure TfrmCreateTableFromDataSet.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled   := not rbAllRows.Checked;
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

  DestDB   := RegisteredDatabases[DBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[DBIndex].IBTransaction;

  if not DestDB.Connected then
    DestDB.Connected := True;
  if not DestTrans.InTransaction then
    DestTrans.StartTransaction;

  InitTransferStats(DBIndex, TableName);
  StartTime   := Now;
  DidCopyData := False;

  Script := TIBXScript.Create(nil);
  try
    Script.Database    := DestDB;
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
        TableAlreadyExists := False;
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
        if not FFields[i].Checked then
          Continue;

        if FieldList <> '' then
          FieldList := FieldList + ',' + sLineBreak;

        FieldList := FieldList + '  ' + QuoteFieldName(FFields[i].FieldName) + ' ' + FFields[i].FieldType;
        if FFields[i].NotNull then
          FieldList := FieldList + ' NOT NULL';
      end;

      if FieldList = '' then
      begin
        ShowMessage('No fields selected for table creation.');
        Exit;
      end;

      SQL := 'CREATE TABLE ' + TableName + ' (' + sLineBreak +
             FieldList + sLineBreak + ')';

      Script.ExecSQLScript(SQL);
      DestTrans.CommitRetaining;
      FStats.CreateTableSQL := SQL;
    end
    else
    begin
      FStats.OptionsExtra := 'Insert Data Only (table already existed)';
    end;

    // ========================================================================
    // DATA COPY
    // ========================================================================
    if chkboxCopyData.Checked then
    begin
      RunFBInsertRowByRow(DBIndex, TableName);
      DidCopyData := True;
    end;

  finally
    Script.Free;
  end;

  FStats.ElapsedSeconds := (Now - StartTime) * SecsPerDay;

  if (FStats.CreateTableSQL <> '') or DidCopyData then
    ShowTransferReport;
end;

// ============================================================================
// RowByRow Insert
// ============================================================================
procedure TfrmCreateTableFromDataSet.RunFBInsertRowByRow(
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
  ParamNames: string;
  CheckedFields: array of Integer;
  CachedFields: array of TField;
  CachedTypes: array of string;
  FieldsPerRow: Integer;
  FromRow, ToRow, TotalRows: Int64;
  CurrentRow: Int64;
  OldDecimalSep: Char;
  ElapsedSec: Double;
  RowsPerSec: Double;
  CommitEveryNRows: Integer;
  RowsSinceCommit: Integer;
  f, i: Integer;
begin
  DestDB   := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;

  if not DestDB.Connected then
    DestDB.Connected := True;
  if not DestTrans.InTransaction then
    DestTrans.StartTransaction;

  // Felder sammeln
  SetLength(CheckedFields, 0);
  FieldNames := '';
  ParamNames := '';
  for i := 0 to High(FFields) do
    if FFields[i].Checked then
    begin
      SetLength(CheckedFields, Length(CheckedFields) + 1);
      CheckedFields[High(CheckedFields)] := i;
      if FieldNames <> '' then
      begin
        FieldNames := FieldNames + ', ';
        ParamNames := ParamNames + ', ';
      end;
      FieldNames := FieldNames + QuoteFieldName(FFields[i].FieldName);
      ParamNames := ParamNames + ':p' + IntToStr(High(CheckedFields));
    end;

  FieldsPerRow := Length(CheckedFields);
  if FieldsPerRow = 0 then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  // Bereich
  FromRow := GetFromRow;
  ToRow   := GetToRow;
  if (FDataSet <> nil) and (ToRow > FDataSet.RecordCount) then
    ToRow := FDataSet.RecordCount;

  TotalRows := ToRow - FromRow + 1;
  if TotalRows <= 0 then
  begin
    ShowMessage('No rows to copy.');
    Exit;
  end;

  // Caches
  SetLength(CachedFields, FieldsPerRow);
  SetLength(CachedTypes, FieldsPerRow);
  for f := 0 to FieldsPerRow - 1 do
  begin
    CachedFields[f] := FDataSet.FieldByName(FFields[CheckedFields[f]].FieldName);
    CachedTypes[f]  := FFields[CheckedFields[f]].FieldType;
  end;

  // Commit-Intervall aus dem Edit-Feld (in Zeilen)
  CommitEveryNRows := Max(1, GetCommitInterval);
  RowsSinceCommit  := 0;

  Query        := TIBQuery.Create(nil);

  ProgressForm := TForm.Create(Self);
  ProgressForm.Parent := self;
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  StartTime    := Now;
  CurrentRow   := 0;
  FCancelled   := False;

  try
    DefaultFormatSettings.DecimalSeparator := '.';

    Query.Database                   := DestDB;
    Query.Transaction                := DestTrans;
    Query.AllowAutoActivateTransaction := True;

    // Progress-Form
    ProgressForm.FormStyle   := fsNormal;
    ProgressForm.Caption     := 'Copying data to ' + ATableName;
    ProgressForm.Width       := 580;
    ProgressForm.Height      := 280;
    ProgressForm.BorderStyle := bsNone;
    ProgressForm.Left := (self.Width - ProgressForm.Width) div 2 ;
    ProgressForm.Top  := ((self.Height - ProgressForm.Height) div 2) + 20;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent  := ProgressForm;
    ProgressLabel.Left    := 16;
    ProgressLabel.Top     := 16;
    ProgressLabel.Width   := 540;
    ProgressLabel.Caption := Format(
      'Total: %s rows   |   Rows/sec: calculating...',
      [FormatFloat('#,##0', TotalRows)]);

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent   := ProgressForm;
    ProgressBar.Left     := 16;
    ProgressBar.Top      := 40;
    ProgressBar.Width    := 540;
    ProgressBar.Height   := 20;
    ProgressBar.Min      := 0;
    ProgressBar.Max      := TotalRows;
    ProgressBar.Position := 0;

    LblCommit := TLabel.Create(ProgressForm);
    LblCommit.Parent := ProgressForm;
    LblCommit.Left   := 16;
    LblCommit.Top    := 75;
    LblCommit.Width  := 540;
    LblCommit.Caption := Format('Rows: 0 of %s', [FormatFloat('#,##0', TotalRows)]);

    CommitBar := TProgressBar.Create(ProgressForm);
    CommitBar.Parent   := ProgressForm;
    CommitBar.Left     := 16;
    CommitBar.Top      := 100;
    CommitBar.Width    := 540;
    CommitBar.Height   := 20;
    CommitBar.Min      := 0;
    CommitBar.Max      := TotalRows;
    CommitBar.Position := 0;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent  := ProgressForm;
    LblElapsed.Left    := 16;
    LblElapsed.Top     := 135;
    LblElapsed.Width   := 540;
    LblElapsed.Caption := 'Elapsed: 00:00:00   |   0 rows/sec';

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent  := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left    := 230;
    BtnCancel.Top     := 180;
    BtnCancel.Width   := 100;
    BtnCancel.OnClick := @CancelButtonClick;

    ProgressLabel.Caption := 'Please wait — preparing data...';

    ProgressForm.Show;
    frmThemeSelector.btnApplyClick(ProgressForm);
    //ProgressForm.BringToFront;
    Application.ProcessMessages;
    //Sleep(30);

    // Prepared INSERT einmal aufbauen (Parameter, kein SQL-String pro Zeile)
    Query.SQL.Text := 'INSERT INTO ' + ATableName + ' (' + FieldNames +
                      ') VALUES (' + ParamNames + ')';
    Query.Prepare;

    // Row-by-Row INSERT-Schleife
    FDataSet.DisableControls;
    try
      FDataSet.First;
      for i := 1 to FromRow - 1 do
        FDataSet.Next;

      ProgressLabel.Caption := 'Please wait — preparing data...';
      Application.ProcessMessages;
      while (not FDataSet.EOF) and (CurrentRow < TotalRows) do
      begin
        if FCancelled then
          Break;

        // Parameter aus dem Dataset füllen (zieltyp-aware)
        for f := 0 to FieldsPerRow - 1 do
        begin
          if CachedFields[f].IsNull then
            AssignParamFromString(Query.Params[f], '', True, CachedTypes[f])
          else
            AssignParamFromString(Query.Params[f], CachedFields[f].AsString, False, CachedTypes[f]);
        end;

        try
          Query.ExecSQL;
        except
          on E: Exception do
          begin
            if DestTrans.InTransaction then
              DestTrans.Rollback;
            raise Exception.CreateFmt(
              'INSERT failed at row %d: %s',
              [CurrentRow + 1, E.Message]);
          end;
        end;

        Inc(CurrentRow);
        Inc(RowsSinceCommit);
        FDataSet.Next;

        // Commit nach dem konfigurierten Intervall (CommitRetaining + ProcessMessages)
        if RowsSinceCommit >= CommitEveryNRows then
        begin
          DestTrans.CommitRetaining;
          RowsSinceCommit := 0;

          CommitBar.Position := CurrentRow;
          ProgressBar.Position := CurrentRow;

          ElapsedSec := (Now - StartTime) * SecsPerDay;
          if ElapsedSec > 0.5 then
            RowsPerSec := CurrentRow / ElapsedSec
          else
            RowsPerSec := 0;

          LblElapsed.Caption :=
            'Elapsed: ' + FormatDateTime('hh:nn:ss', ElapsedSec / SecsPerDay) +
            '   |   ' + FormatFloat('#,##0', RowsPerSec) + ' rows/sec';
          LblCommit.Caption := Format('Rows: %s of %s',
            [FormatFloat('#,##0', CurrentRow), FormatFloat('#,##0', TotalRows)]);
          ProgressLabel.Caption := Format(
            'Total: %s rows   |   Current: %s',
            [FormatFloat('#,##0', TotalRows), FormatFloat('#,##0', CurrentRow)]);

          Application.ProcessMessages;
        end;
      end;

      // Finaler Commit (kein CommitRetaining) + ProcessMessages
      if not FCancelled then
      begin
        if DestTrans.InTransaction then
          DestTrans.Commit;
        Application.ProcessMessages;
      end;

    finally
      EndTime := Now;
      FDataSet.EnableControls;
      SetLength(CachedFields, 0);
      SetLength(CachedTypes, 0);
      SetLength(CheckedFields, 0);
      Query.UnPrepare;
    end;

    // Statistik
    FStats.RowsProcessed := CurrentRow;
    ElapsedSec := (EndTime - StartTime) * SecsPerDay;
    if ElapsedSec > 0 then
      RowsPerSec := CurrentRow / ElapsedSec
    else
      RowsPerSec := 0;

    if FStats.OptionsExtra = '' then
      FStats.OptionsExtra := Format(
        'Row-by-Row INSERT (prepared, %d rows), %.0f rows/sec',
        [CurrentRow, RowsPerSec])
    else
      FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
        '  ' + Format('Row-by-Row INSERT (prepared, %d rows), %.0f rows/sec',
          [CurrentRow, RowsPerSec]);

    if FCancelled then
    begin
      if DestTrans.InTransaction then
        DestTrans.Rollback;
      FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
        Format('  CANCELLED after %s of %s rows',
          [FormatFloat('#,##0', CurrentRow), FormatFloat('#,##0', TotalRows)]);
    end;

  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
    Query.Free;
    ProgressForm.Free;
  end;
end;

// ============================================================================
// FELDNAME ZU SQL-IDENTIFIER (Case-Sensitive quotes bei gemischter Case)
// ============================================================================

class function TfrmCreateTableFromDataSet.QuoteFieldName(
  const AName: string): string;
begin
  // Firebird: unquoted identifiers are case-insensitive (uppercased).
  // Names with lowercase letters need double quotes to preserve case.
  if (AName = '') or (AName <> UpperCase(AName)) then
    Result := '"' + AName + '"'
  else
    Result := AName;
end;

// ============================================================================
// FIELD VALUE TO SQL LITERAL
// ============================================================================

function TfrmCreateTableFromDataSet.FieldValueToSQLLiteral(
  const AValue: string; AField: TField; const ADestType: string): string;
var
  FSFloat: TFormatSettings;
  FVal: Double;
  DT: TDateTime;
  S: string;
begin
  // NULL-Werte werden vorher bereits behandelt, hier nur Nicht-NULL
  S := Trim(AValue);

  // Leere Strings für nicht-String-Zieltypen als NULL ausgeben
  // (Firebird kann '' nicht in DATE/TIMESTAMP/INTEGER/BOOLEAN konvertieren)
  if S = '' then
  begin
    if SameText(ADestType, 'VARCHAR') or SameText(ADestType, 'CHAR')
      or SameText(ADestType, 'TEXT') or SameText(ADestType, 'STRING') then
      Result := QuotedStr('')
    else
      Result := 'NULL';
    Exit;
  end;

  // BOOLEAN-Werte je nach Zieltyp konvertieren:
  //   - Zielspalte ist BOOLEAN -> true/false (Firebird lowercase)
  //   - Zielspalte ist SMALLINT/INTEGER -> 1/0
  if SameText(S, 'TRUE') or (S = '1') then
  begin
    if SameText(ADestType, 'SMALLINT') or SameText(ADestType, 'INTEGER')
      or SameText(ADestType, 'INT') or SameText(ADestType, 'BIGINT')
      or SameText(ADestType, 'INT64') or SameText(ADestType, 'LARGEINT') then
      Result := '1'
    else
      Result := 'true';
    Exit;
  end
  else if SameText(S, 'FALSE') or (S = '0') then
  begin
    if SameText(ADestType, 'SMALLINT') or SameText(ADestType, 'INTEGER')
      or SameText(ADestType, 'INT') or SameText(ADestType, 'BIGINT')
      or SameText(ADestType, 'INT64') or SameText(ADestType, 'LARGEINT') then
      Result := '0'
    else
      Result := 'false';
    Exit;
  end;

  case AField.DataType of
    ftString, ftWideString, ftMemo, ftWideMemo:
    begin
      // Führende/trailing Anführungszeichen entfernen (falls vom CSV-Import überliefert)
      if (Length(S) >= 2) and (S[1] = '"') and (S[Length(S)] = '"') then
        S := Copy(S, 2, Length(S) - 2);
      Result := QuotedStr(S);
    end;

    ftSmallint, ftInteger, ftWord, ftLargeInt:
      Result := S;

    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
    begin
      FSFloat := DefaultFormatSettings;
      FSFloat.DecimalSeparator := '.';
      FSFloat.ThousandSeparator := #0;
      if TryStrToFloat(S, FVal, FSFloat) then
        Result := FloatToStr(FVal, FSFloat)
      else
        Result := QuotedStr(S);
    end;

    ftDate, ftTime, ftDateTime:
    begin
      if TryParseDateTimeEx(S, DT) then
        Result := QuotedStr(FormatDateTime('YYYY-MM-DD HH:NN:SS', DT))
      else
        Result := QuotedStr(S);
    end;

    ftBlob:
      Result := QuotedStr(S);

    else
      Result := QuotedStr(S);
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
    qry.Database    := ADB;
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
// PARAMETER-ZUWEISUNG
// ============================================================================

procedure TfrmCreateTableFromDataSet.AssignParamFromField(
  Param: TParam; SourceField: TField; const TargetTypeString: string);
begin
  if SourceField.IsNull then
    AssignParamFromString(Param, '', True, TargetTypeString)
  else
    AssignParamFromString(Param, SourceField.AsString, False, TargetTypeString);
end;

procedure TfrmCreateTableFromDataSet.AssignParamFromString(
  Param: TParam; const Value: string; AIsNull: Boolean;
  const TargetTypeString: string);
var
  S, TrimmedS, BaseType: string;
  DT: TDateTime;
  I64: Int64;
  FVal: Double;
  BoolVal: Boolean;
  FSFloat: TFormatSettings;
  LParen: Integer;
  IsStringType, IsIntType, IsFloatType, IsDecimalType,
  IsDateType, IsBoolType: Boolean;
begin
  // Zuerst DataType setzen, dann Clear – verhindert Probleme bei manchen IBX-Versionen
  if AIsNull then
  begin
    if Pos('INT', UpperCase(TargetTypeString)) > 0 then
      Param.DataType := ftLargeInt
    else if Pos('VARCHAR', UpperCase(TargetTypeString)) > 0 then
      Param.DataType := ftString
    else if Pos('TIMESTAMP', UpperCase(TargetTypeString)) > 0 then
      Param.DataType := ftDateTime
    else if Pos('DATE', UpperCase(TargetTypeString)) > 0 then
      Param.DataType := ftDate
    else if Pos('FLOAT', UpperCase(TargetTypeString)) > 0 then
      Param.DataType := ftFloat
    else if (Pos('DECIMAL', UpperCase(TargetTypeString)) > 0) or
            (Pos('NUMERIC', UpperCase(TargetTypeString)) > 0) then
      Param.DataType := ftFMTBcd
    else
      Param.DataType := ftString;

    Param.Clear;
    Exit;
  end;

  S        := Value;
  TrimmedS := Trim(S);

  LParen := Pos('(', TargetTypeString);
  if LParen > 0 then
    BaseType := UpperCase(Trim(Copy(TargetTypeString, 1, LParen - 1)))
  else
    BaseType := UpperCase(Trim(TargetTypeString));

  IsStringType  := (BaseType = 'VARCHAR') or (BaseType = 'CHAR') or
                   (BaseType = 'CSTRING') or (BaseType = 'BLOB') or
                   (BaseType = 'TEXT');
  IsIntType     := (BaseType = 'SMALLINT') or (BaseType = 'INTEGER') or
                   (BaseType = 'BIGINT') or (BaseType = 'INT64');
  IsFloatType   := (BaseType = 'DOUBLE PRECISION') or (BaseType = 'DOUBLE') or
                   (BaseType = 'FLOAT');
  IsDecimalType := (BaseType = 'DECIMAL') or (BaseType = 'NUMERIC');
  IsDateType    := (BaseType = 'DATE') or (BaseType = 'TIME') or
                   (BaseType = 'TIMESTAMP');
  IsBoolType    := (BaseType = 'BOOLEAN');

  if IsStringType then
  begin
    Param.AsString := S;
    Exit;
  end;

  if TrimmedS = '' then
  begin
    Param.Clear;
    Exit;
  end;

  FSFloat := DefaultFormatSettings;
  FSFloat.DecimalSeparator  := '.';
  FSFloat.ThousandSeparator := #0;

  if IsIntType then
  begin
    if TryStrToInt64(TrimmedS, I64) then
      Param.AsLargeInt := I64
    else if SameText(TrimmedS, 'TRUE') then
      Param.AsLargeInt := 1
    else if SameText(TrimmedS, 'FALSE') then
      Param.AsLargeInt := 0
    else
      raise Exception.CreateFmt(
        'Cannot convert value "%s" to integer (target type %s).',
        [S, TargetTypeString]);
    Exit;
  end;

  if IsFloatType then
  begin
    if not (TryStrToFloat(TrimmedS, FVal, FSFloat) or TryStrToFloat(TrimmedS, FVal)) then
      raise Exception.CreateFmt(
        'Cannot convert value "%s" to float (target type %s).',
        [S, TargetTypeString]);
    Param.AsFloat := FVal;
    Exit;
  end;

  if IsDecimalType then
  begin
    if (Pos('.', TrimmedS) > 0) or (Pos(',', TrimmedS) > 0) then
    begin
      if not (TryStrToFloat(TrimmedS, FVal, FSFloat) or TryStrToFloat(TrimmedS, FVal)) then
        raise Exception.CreateFmt(
          'Cannot convert value "%s" to decimal (target type %s).',
          [S, TargetTypeString]);
      Param.AsString := FloatToStr(FVal, FSFloat);
    end
    else
      Param.AsString := TrimmedS;
    Exit;
  end;

  if IsDateType then
  begin
    if not TryParseDateTimeEx(TrimmedS, DT) then
      raise Exception.CreateFmt(
        'Cannot convert value "%s" to date/time (target type %s).',
        [S, TargetTypeString]);
    Param.AsDateTime := DT;
    Exit;
  end;

  if IsBoolType then
  begin
    if SameText(TrimmedS, 'TRUE') or (TrimmedS = '1') then
      BoolVal := True
    else if SameText(TrimmedS, 'FALSE') or (TrimmedS = '0') then
      BoolVal := False
    else
      raise Exception.CreateFmt(
        'Cannot convert value "%s" to boolean (target type %s).',
        [S, TargetTypeString]);
    Param.AsBoolean := BoolVal;
    Exit;
  end;

  // Fallback
  Param.AsString := S;
end;

end.
