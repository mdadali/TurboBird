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
  uSystemInfo,
   uib, uiblib;

type
  { TfrmCreateTableFromDataSet }

  TfrmCreateTableFromDataSet = class(TForm)
    btnExternalFile: TButton;
    btnMainCancel: TButton;
    btnDeselectAll: TButton;
    btnOK: TButton;
    btnSelectAll: TButton;
    btnRun: TButton;
    chkboxCopyData: TCheckBox;
    chkboxExternalTable: TCheckBox;
    chkLstFields: TCheckListBox;
    cmbBoxDBs: TComboBox;
    cmbBoxServers: TComboBox;
    edtRefreshInterval: TEdit;
    edtDestTableName: TEdit;
    edtExternalFile: TEdit;
    edtFrom: TEdit;
    edtWriteBuffer: TEdit;
    edtTo: TEdit;
    grBoxCopyOptions1: TGroupBox;
    grboxDestination: TGroupBox;
    grboxExternalTable: TGroupBox;
    grBoxFields: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
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
    procedure btnExternalFileClick(Sender: TObject);
    procedure btnMainCancelClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure chkboxExternalTableChange(Sender: TObject);
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
    function  GetFromRow: Int64;
    function  GetToRow: Int64;
    procedure RunFBInsertRowByRow(ADBIndex: Integer; const ATableName: string);
    procedure RunFBInsertExternal(ADBIndex: Integer; const ATableName: string);
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
    function QuoteFieldName(const AName: string): string;
    function GetRefreshInterval: Int64;
    function GetWriteBufferBytes: Integer;
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
  FStats.BatchSize         := GetRefreshInterval;
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

procedure TfrmCreateTableFromDataSet.chkboxExternalTableChange(Sender: TObject);
begin
  edtExternalFile.Enabled := chkboxExternalTable.Checked;
  btnExternalFile.Enabled := chkboxExternalTable.Checked;
  edtWriteBuffer.Enabled  := chkboxExternalTable.Checked;
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

function TfrmCreateTableFromDataSet.GetRefreshInterval: Int64;
begin
  Result := StrToIntDef(Trim(edtRefreshInterval.Text), 10000);
  if Result < 1 then
    Result := 10000;
end;

function TfrmCreateTableFromDataSet.GetWriteBufferBytes: Integer;
var
  MB: Integer;
begin
  MB := StrToIntDef(Trim(edtWriteBuffer.Text), 32);
  if MB < 1 then
    MB := 1
  else if MB > 512 then
    MB := 512;
  Result := MB * 1024 * 1024;
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

procedure TfrmCreateTableFromDataSet.btnExternalFileClick(Sender: TObject);
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(nil);
  try
    Dlg.Title   := 'Select External Table File';
    Dlg.Filter  := 'All files (*.*)|*.*';
    Dlg.Options := Dlg.Options + [ofOverwritePrompt];
    if Trim(edtExternalFile.Text) <> '' then
      Dlg.FileName := edtExternalFile.Text;
    if Dlg.Execute then
      edtExternalFile.Text := Dlg.FileName;
  finally
    Dlg.Free;
  end;
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
  TableAlreadyExists, DidCopyData, IsExternal: Boolean;
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

  IsExternal := chkboxExternalTable.Checked;

  if IsExternal and (Trim(edtExternalFile.Text) = '') then
  begin
    ShowMessage('Please select an external file path.');
    Exit;
  end;

  DestDB    := RegisteredDatabases[DBIndex].IBDatabase;
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

        FieldList := FieldList + '  ' +
          QuoteFieldName(FFields[i].FieldName) + ' ' + FFields[i].FieldType;

        // NOT NULL nur bei normalen Tabellen (External Tables erlauben kein NOT NULL)
        if FFields[i].NotNull and (not IsExternal) then
          FieldList := FieldList + ' NOT NULL';
      end;

      if FieldList = '' then
      begin
        ShowMessage('No fields selected for table creation.');
        Exit;
      end;

      if IsExternal then
      begin
        SQL := 'CREATE TABLE ' + TableName + sLineBreak +
               'EXTERNAL FILE ' + QuotedStr(Trim(edtExternalFile.Text)) + sLineBreak +
               '(' + sLineBreak +
               FieldList + sLineBreak +
               ')';
      end
      else
      begin
        SQL := 'CREATE TABLE ' + TableName + ' (' + sLineBreak +
               FieldList + sLineBreak + ')';
      end;

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
      if IsExternal then
      begin
        if MessageDlg(
             'External Tables have no transaction control.' + sLineBreak +
             'If an error occurs, partial data remains in the file.' + sLineBreak +
             sLineBreak +
             'Continue?',
             mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin
          RunFBInsertExternal(DBIndex, TableName);
          DidCopyData := True;
        end;
      end
      else
      begin
        RunFBInsertRowByRow(DBIndex, TableName);
        DidCopyData := True;
      end;
    end;

  finally
    Script.Free;
  end;

  FStats.ElapsedSeconds := (Now - StartTime) * SecsPerDay;

  if (FStats.CreateTableSQL <> '') or DidCopyData then
    ShowTransferReport;
end;

// ============================================================================
// RowByRow Insertt, firebird table
// ============================================================================
procedure TfrmCreateTableFromDataSet.RunFBInsertRowByRow(
  ADBIndex: Integer; const ATableName: string);
const
  MAX_INSERTS_PER_BLOCK = 256;
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
  CachedFields: array of TField;
  CachedTypes: array of string;
  FieldsPerRow: Integer;
  FromRow, ToRow, TotalRows: Int64;
  CurrentRow: Int64;
  RefreshEveryNRows: Int64;
  RowsSinceRefresh: Int64;
  MaxRowsPerBlock: Integer;
  BlockRowCount: Integer;
  OldDecimalSep: Char;
  ElapsedSec, RowsPerSec: Double;
  InsertPrefix: string;
  BlockSB: TStringBuilder;
  RowValues: string;
  ValLit: string;
  f, i: Integer;
begin
  DestDB    := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;

  if not DestDB.Connected then
    DestDB.Connected := True;
  if not DestTrans.InTransaction then
    DestTrans.StartTransaction;

  // ---- Felder sammeln ----
  SetLength(CheckedFields, 0);
  FieldNames := '';
  for i := 0 to High(FFields) do
    if FFields[i].Checked then
    begin
      SetLength(CheckedFields, Length(CheckedFields) + 1);
      CheckedFields[High(CheckedFields)] := i;
      if FieldNames <> '' then
        FieldNames := FieldNames + ', ';
      FieldNames := FieldNames + QuoteFieldName(FFields[i].FieldName);
    end;

  FieldsPerRow := Length(CheckedFields);
  if FieldsPerRow = 0 then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  // ---- Bereich ----
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

  // ---- Caches ----
  SetLength(CachedFields, FieldsPerRow);
  SetLength(CachedTypes, FieldsPerRow);
  for f := 0 to FieldsPerRow - 1 do
  begin
    CachedFields[f] := FDataSet.FieldByName(FFields[CheckedFields[f]].FieldName);
    CachedTypes[f]  := FFields[CheckedFields[f]].FieldType;
  end;

  // Schutz gegen Firebirds 64-KB-BLR-Limit bei sehr breiten Tabellen (max. 256 Zeilen/Block)
  MaxRowsPerBlock := Min(MAX_INSERTS_PER_BLOCK, Max(1, 3500 div Max(1, FieldsPerRow)));

  // UI-Refresh-Intervall (umbenanntes edtRefreshInterval)
  RefreshEveryNRows := Max(1, StrToIntDef(edtRefreshInterval.Text, 10000));
  RowsSinceRefresh  := 0;

  InsertPrefix := '  INSERT INTO ' + ATableName + ' (' + FieldNames + ') VALUES (';

  Query        := TIBQuery.Create(nil);
  BlockSB      := TStringBuilder.Create(65536);
  ProgressForm := TForm.Create(Self);
  ProgressForm.Parent := Self;
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  StartTime    := Now;
  CurrentRow   := 0;
  FCancelled   := False;

  try
    DefaultFormatSettings.DecimalSeparator := '.';

    Query.Database                   := DestDB;
    Query.Transaction                := DestTrans;
    Query.AllowAutoActivateTransaction := True;
    Query.ParamCheck                 := False; // Wichtig: Doppelpunkte in Strings/Zeiten nicht als Parameter parsen!

    // ---- Progress-Form ----
    ProgressForm.FormStyle   := fsNormal;
    ProgressForm.Caption     := 'Copying data to ' + ATableName;
    ProgressForm.Width       := 580;
    ProgressForm.Height      := 280;
    ProgressForm.BorderStyle := bsNone;
    ProgressForm.Left        := (Self.Width - ProgressForm.Width) div 2;
    ProgressForm.Top         := ((Self.Height - ProgressForm.Height) div 2) + 20;

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
    LblCommit.Parent  := ProgressForm;
    LblCommit.Left    := 16;
    LblCommit.Top     := 75;
    LblCommit.Width   := 540;
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

    ProgressLabel.Caption := 'Please wait — preparing EXECUTE BLOCK import...';
    ProgressForm.Show;
    frmThemeSelector.btnApplyClick(ProgressForm);
    Application.ProcessMessages;

    // ---- EXECUTE BLOCK Schleife ----
    FDataSet.DisableControls;
    try
      FDataSet.First;
      for i := 1 to FromRow - 1 do
        FDataSet.Next;

      BlockSB.Clear;
      BlockSB.AppendLine('EXECUTE BLOCK AS BEGIN');
      BlockRowCount := 0;

      while (not FDataSet.EOF) and (CurrentRow < TotalRows) do
      begin
        if FCancelled then
          Break;

        // Zeile als SQL-Literale zusammenbauen
        BlockSB.Append(InsertPrefix);
        for f := 0 to FieldsPerRow - 1 do
        begin
          if f > 0 then
            BlockSB.Append(', ');

          if CachedFields[f].IsNull then
            BlockSB.Append('NULL')
          else
          begin
            ValLit := FieldValueToSQLLiteral(
              CachedFields[f].AsString, CachedFields[f], CachedTypes[f]);
            BlockSB.Append(ValLit);
          end;
        end;
        BlockSB.AppendLine(');');

        Inc(BlockRowCount);
        Inc(CurrentRow);
        Inc(RowsSinceRefresh);
        FDataSet.Next;

        // Block voll → EXECUTE BLOCK abfeuern + CommitRetaining (ohne ProcessMessages)
        if BlockRowCount >= MaxRowsPerBlock then
        begin
          BlockSB.Append('END');
          try
            Query.SQL.Text := BlockSB.ToString;
            Query.ExecSQL;
            DestTrans.CommitRetaining;
          except
            on E: Exception do
            begin
              if DestTrans.InTransaction then
                DestTrans.Rollback;
              raise Exception.CreateFmt(
                'EXECUTE BLOCK failed around row %d: %s',
                [CurrentRow, E.Message]);
            end;
          end;

          BlockSB.Clear;
          BlockSB.AppendLine('EXECUTE BLOCK AS BEGIN');
          BlockRowCount := 0;
        end;

        // UI-Update nur im edtRefreshInterval-Takt (inkl. Application.ProcessMessages)
        if RowsSinceRefresh >= RefreshEveryNRows then
        begin
          RowsSinceRefresh := 0;

          CommitBar.Position   := CurrentRow;
          ProgressBar.Position := CurrentRow;

          ElapsedSec := (Now - StartTime) * SecsPerDay;
          if ElapsedSec > 0.2 then
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

      // Restlichen unvollständigen Block ausführen
      if (not FCancelled) and (BlockRowCount > 0) then
      begin
        BlockSB.Append('END');
        try
          Query.SQL.Text := BlockSB.ToString;
          Query.ExecSQL;
        except
          on E: Exception do
          begin
            if DestTrans.InTransaction then
              DestTrans.Rollback;
            raise Exception.CreateFmt(
              'Final EXECUTE BLOCK failed around row %d: %s',
              [CurrentRow, E.Message]);
          end;
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
    end;

    // ---- Statistik ----
    FStats.RowsProcessed := CurrentRow;
    ElapsedSec := (EndTime - StartTime) * SecsPerDay;
    if ElapsedSec > 0 then
      RowsPerSec := CurrentRow / ElapsedSec
    else
      RowsPerSec := 0;

    if FStats.OptionsExtra = '' then
      FStats.OptionsExtra := Format(
        'EXECUTE BLOCK INSERT (%d rows/block, %d rows total), %.0f rows/sec',
        [MaxRowsPerBlock, CurrentRow, RowsPerSec])
    else
      FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
        '  ' + Format('EXECUTE BLOCK INSERT (%d rows/block, %d rows total), %.0f rows/sec',

        [MaxRowsPerBlock, CurrentRow, RowsPerSec]);
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
        BlockSB.Free;
        Query.Free;
        ProgressForm.Free;
        end;
end;

procedure TfrmCreateTableFromDataSet.RunFBInsertExternal(
  ADBIndex: Integer; const ATableName: string);
type
  TFBExtColKind = (
    ekSmallInt, ekInteger, ekBigInt, ekInt128,
    ekFloat, ekDouble, ekNumeric16, ekNumeric32, ekNumeric64,
    ekChar, ekVarChar, ekBoolean,
    ekDate, ekTime, ekTimestamp, ekTimeTZ, ekTimestampTZ
  );
  TFBExtColMeta = record
    Kind: TFBExtColKind;
    Offset: Integer;      // Byte-Offset innerhalb eines Records
    ByteSize: Integer;    // Physische Byte-Größe im Record
    CharLen: Integer;     // Zeichenlänge für CHAR / VARCHAR
    Scale: Integer;       // Nachkommastellen für NUMERIC / DECIMAL
  end;

var
  StartTime, EndTime: TDateTime;
  ProgressForm: TForm;
  ProgressLabel, LblElapsed, LblCommit: TLabel;
  ProgressBar, CommitBar: TProgressBar;
  BtnCancel: TButton;
  CheckedFields: array of Integer;
  CachedFields: array of TField;
  ColMeta: array of TFBExtColMeta;
  FieldsPerRow: Integer;
  FromRow, ToRow, TotalRows: Int64;
  CurrentRow: Int64;
  UpdateEveryNRows: Int64;
  RowsSinceUpdate: Int64;
  OldDecimalSep: Char;
  FSFloat: TFormatSettings;
  DefaultFBDate: LongInt;
  ElapsedSec, RowsPerSec: Double;
  ExtFilePath: string;
  FS: TFileStream;
  WRITE_BUFFER_SIZE: Int64;
  WriteBuf: TBytes;
  WriteBufPos: Integer;
  RecBuf: TBytes;
  RecordSize, MaxAlign, ColAlign: Integer;
  f, i: Integer;

  function AlignUp(AValue, AAlign: Integer): Integer;
  begin
    if AAlign <= 1 then
      Result := AValue
    else
      Result := (AValue + AAlign - 1) and not (AAlign - 1);
  end;

  procedure ParseTypeInfo(const ATypeStr: string;
    out AKind: TFBExtColKind; out ASize, AAlign, ACharLen, AScale: Integer);
  var
    S, BaseType, Params: string;
    LParen, RParen, CommaPos, Prec: Integer;
  begin
    S := UpperCase(Trim(ATypeStr));
    ACharLen := 0;
    AScale   := 0;
    Prec     := 0;

    LParen := Pos('(', S);
    if LParen > 0 then
    begin
      BaseType := Trim(Copy(S, 1, LParen - 1));
      RParen := Pos(')', S);
      if RParen > LParen then
      begin
        Params := Trim(Copy(S, LParen + 1, RParen - LParen - 1));
        CommaPos := Pos(',', Params);
        if CommaPos > 0 then
        begin
          Prec   := StrToIntDef(Trim(Copy(Params, 1, CommaPos - 1)), 0);
          AScale := StrToIntDef(Trim(Copy(Params, CommaPos + 1, MaxInt)), 0);
        end
        else
          Prec := StrToIntDef(Params, 0);
      end;
    end
    else
      BaseType := S;

    if BaseType = 'SMALLINT' then
    begin
      AKind := ekSmallInt; ASize := 2; AAlign := 2;
    end
    else if (BaseType = 'INTEGER') or (BaseType = 'INT') then
    begin
      AKind := ekInteger; ASize := 4; AAlign := 4;
    end
    else if (BaseType = 'BIGINT') or (BaseType = 'INT64') then
    begin
      AKind := ekBigInt; ASize := 8; AAlign := 8;
    end
    else if BaseType = 'INT128' then
    begin
      AKind := ekInt128; ASize := 16; AAlign := 8;
    end
    else if BaseType = 'FLOAT' then
    begin
      AKind := ekFloat; ASize := 4; AAlign := 4;
    end
    else if (BaseType = 'DOUBLE PRECISION') or (BaseType = 'DOUBLE') or
            (BaseType = 'DECFLOAT') then
    begin
      AKind := ekDouble; ASize := 8; AAlign := 8;
    end
    else if (BaseType = 'NUMERIC') or (BaseType = 'DECIMAL') then
    begin
      if Prec <= 4 then
      begin
        AKind := ekNumeric16; ASize := 2; AAlign := 2;
      end
      else if Prec <= 9 then
      begin
        AKind := ekNumeric32; ASize := 4; AAlign := 4;
      end
      else
      begin
        AKind := ekNumeric64; ASize := 8; AAlign := 8;
      end;
    end
    else if (BaseType = 'CHAR') or (BaseType = 'CSTRING') then
    begin
      ACharLen := Max(1, Prec);
      AKind := ekChar; ASize := ACharLen; AAlign := 1;
    end
    else if (BaseType = 'VARCHAR') or (BaseType = 'TEXT') or (BaseType = 'BLOB') then
    begin
      ACharLen := Max(1, Prec);
      AKind := ekVarChar; ASize := 2 + ACharLen; AAlign := 2;
    end
    else if BaseType = 'BOOLEAN' then
    begin
      AKind := ekBoolean; ASize := 1; AAlign := 1;
    end
    else if BaseType = 'DATE' then
    begin
      AKind := ekDate; ASize := 4; AAlign := 4;
    end
    else if BaseType = 'TIME' then
    begin
      AKind := ekTime; ASize := 4; AAlign := 4;
    end
    else if BaseType = 'TIMESTAMP' then
    begin
      AKind := ekTimestamp; ASize := 8; AAlign := 4;
    end
    else if BaseType = 'TIME WITH TIME ZONE' then
    begin
      AKind := ekTimeTZ; ASize := 8; AAlign := 4;
    end
    else if BaseType = 'TIMESTAMP WITH TIME ZONE' then
    begin
      AKind := ekTimestampTZ; ASize := 12; AAlign := 4;
    end
    else
    begin
      // Fallback: als CHAR(50) behandeln
      ACharLen := 50;
      AKind := ekChar; ASize := 50; AAlign := 1;
    end;
  end;

  procedure FlushWriteBuffer;
  begin
    if WriteBufPos > 0 then
    begin
      FS.WriteBuffer(WriteBuf[0], WriteBufPos);
      WriteBufPos := 0;
    end;
  end;

  procedure WriteFieldToRecord(const AMeta: TFBExtColMeta; AField: TField);
  var
    P: PByte;
    S, TrimmedS: string;
    I64: Int64;
    F64: Double;
    DT: TDateTime;
    CopyLen: Integer;
  begin
    P := @RecBuf[AMeta.Offset];

    // Record-Bereich initialisieren (Strings mit Spaces, Zahlen/Datum mit 0)
    if AMeta.Kind = ekChar then
      FillChar(P^, AMeta.ByteSize, $20)
    else
      FillChar(P^, AMeta.ByteSize, 0);

    // NULL → Standard-Defaultwert (0 / leer / 2020-01-01) belassen
    if AField.IsNull then
    begin
      if AMeta.Kind in [ekDate, ekTimestamp, ekTimestampTZ] then
        PLongInt(P)^ := DefaultFBDate;
      Exit;
    end;

    // Fast-Path: Wenn das TField bereits numerisch/Datum/Boolean ist (kein String-Alloc!)
    case AField.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeInt, ftAutoInc:
      begin
        I64 := AField.AsLargeInt;
        case AMeta.Kind of
          ekSmallInt: PSmallInt(P)^ := SmallInt(I64);
          ekInteger:  PLongInt(P)^  := LongInt(I64);
          ekBigInt:   PInt64(P)^    := I64;
          ekInt128:
          begin
            PInt64(P)^ := I64;
            if I64 < 0 then
              PInt64(P + 8)^ := -1;
          end;
          ekFloat:    PSingle(P)^   := I64;
          ekDouble:   PDouble(P)^   := I64;
          ekBoolean:  if I64 <> 0 then P^ := 1;
        end;
        Exit;
      end;

      ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      begin
        F64 := AField.AsFloat;
        case AMeta.Kind of
          ekFloat:     PSingle(P)^   := F64;
          ekDouble:    PDouble(P)^   := F64;
          ekNumeric16: PSmallInt(P)^ := SmallInt(Round(F64 * IntPower(10, AMeta.Scale)));
          ekNumeric32: PLongInt(P)^  := LongInt(Round(F64 * IntPower(10, AMeta.Scale)));
          ekNumeric64: PInt64(P)^    := Round(F64 * IntPower(10, AMeta.Scale));
        end;
        Exit;
      end;

      ftDate, ftTime, ftDateTime, ftTimeStamp:
      begin
        DT := AField.AsDateTime;
        case AMeta.Kind of
          ekDate:
            PLongInt(P)^ := Trunc(DT) + 15018;
          ekTime, ekTimeTZ:
            PLongWord(P)^ := Round(Frac(DT) * 864000000.0);
          ekTimestamp, ekTimestampTZ:
          begin
            PLongInt(P)^ := Trunc(DT) + 15018;
            PLongWord(P + 4)^ := Round(Frac(DT) * 864000000.0);
          end;
        end;
        Exit;
      end;

      ftBoolean:
      begin
        if AField.AsBoolean then
          P^ := 1
        else
          P^ := 0;
        Exit;
      end;
    end;

    // String-Path (falls das Dataset Spalten als ftString/ftWideString hält)
    S := AField.AsString;
    if S = '' then
    begin
      if AMeta.Kind in [ekDate, ekTimestamp, ekTimestampTZ] then
        PLongInt(P)^ := DefaultFBDate;
      Exit;
    end;

    case AMeta.Kind of
      ekChar:
      begin
        CopyLen := Min(Length(S), AMeta.CharLen);
        if CopyLen > 0 then
          Move(S[1], P^, CopyLen);
      end;

      ekVarChar:
      begin
        CopyLen := Min(Length(S), AMeta.CharLen);
        PWord(P)^ := Word(CopyLen);
        if CopyLen > 0 then
          Move(S[1], (P + 2)^, CopyLen);
      end;

      ekSmallInt, ekInteger, ekBigInt, ekInt128:
      begin
        TrimmedS := Trim(S);
        if TryStrToInt64(TrimmedS, I64) then
        begin
          case AMeta.Kind of
            ekSmallInt: PSmallInt(P)^ := SmallInt(I64);
            ekInteger:  PLongInt(P)^  := LongInt(I64);
            ekBigInt:   PInt64(P)^    := I64;
            ekInt128:
            begin
              PInt64(P)^ := I64;
              if I64 < 0 then
                PInt64(P + 8)^ := -1;
            end;
          end;
        end;
      end;

      ekFloat, ekDouble, ekNumeric16, ekNumeric32, ekNumeric64:
      begin
        TrimmedS := Trim(S);
        if TryStrToFloat(TrimmedS, F64, FSFloat) or TryStrToFloat(TrimmedS, F64) then
        begin
          case AMeta.Kind of
            ekFloat:     PSingle(P)^   := F64;
            ekDouble:    PDouble(P)^   := F64;
            ekNumeric16: PSmallInt(P)^ := SmallInt(Round(F64 * IntPower(10, AMeta.Scale)));
            ekNumeric32: PLongInt(P)^  := LongInt(Round(F64 * IntPower(10, AMeta.Scale)));
            ekNumeric64: PInt64(P)^    := Round(F64 * IntPower(10, AMeta.Scale));
          end;
        end;
      end;

      ekBoolean:
      begin
        TrimmedS := Trim(S);
        if SameText(TrimmedS, 'TRUE') or (TrimmedS = '1') then
          P^ := 1;
      end;

      ekDate:
      begin
        TrimmedS := Trim(S);
        if (TrimmedS <> '') and TryParseDateTimeEx(TrimmedS, DT) then
          PLongInt(P)^ := Trunc(DT) + 15018
        else
          PLongInt(P)^ := DefaultFBDate;
      end;

      ekTime, ekTimeTZ:
      begin
        TrimmedS := Trim(S);
        if (TrimmedS <> '') and TryParseDateTimeEx(TrimmedS, DT) then
          PLongWord(P)^ := Round(Frac(DT) * 864000000.0);
      end;

      ekTimestamp, ekTimestampTZ:
      begin
        TrimmedS := Trim(S);
        if (TrimmedS <> '') and TryParseDateTimeEx(TrimmedS, DT) then
        begin
        PLongInt(P)^ := Trunc(DT) + 15018;
        PLongWord(P + 4)^ := Round(Frac(DT) * 864000000.0);
        end
        else
        PLongInt(P)^ := DefaultFBDate;
        end;
        end;
        end;

begin
        WRITE_BUFFER_SIZE := StrToIntDef(edtWriteBuffer.Text, 16) * 1024 * 1024; // 16 MB Schreibpuffer
        ExtFilePath := Trim(edtExternalFile.Text);
        if ExtFilePath = '' then
        begin
        ShowMessage('External file path is empty.');
        Exit;
        end;
        // ---- Felder sammeln ----
        SetLength(CheckedFields, 0);
        for i := 0 to High(FFields) do
        if FFields[i].Checked then
        begin
        SetLength(CheckedFields, Length(CheckedFields) + 1);
        CheckedFields[High(CheckedFields)] := i;
        end;
        FieldsPerRow := Length(CheckedFields);
        if FieldsPerRow = 0 then
        begin
        ShowMessage('No fields selected.');
        Exit;
        end;
        // ---- Bereich ----
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
        // ---- Caches & Record-Layout berechnen (Firebird C-Struct Alignment) ----
        SetLength(CachedFields, FieldsPerRow);
        SetLength(ColMeta, FieldsPerRow);
        RecordSize := 0;
        MaxAlign   := 1;
        for f := 0 to FieldsPerRow - 1 do
        begin
        CachedFields[f] := FDataSet.FieldByName(FFields[CheckedFields[f]].FieldName);
        ParseTypeInfo(
        FFields[CheckedFields[f]].FieldType,
        ColMeta[f].Kind,
        ColMeta[f].ByteSize,
        ColAlign,
        ColMeta[f].CharLen,
        ColMeta[f].Scale
        );
        RecordSize := AlignUp(RecordSize, ColAlign);
        ColMeta[f].Offset := RecordSize;
        Inc(RecordSize, ColMeta[f].ByteSize);
        if ColAlign > MaxAlign then
        MaxAlign := ColAlign;
        end;
        // Gesamten Record auf das maximale Alignment auffüllen
        RecordSize := AlignUp(RecordSize, MaxAlign);
        SetLength(RecBuf, RecordSize);
        SetLength(WriteBuf, WRITE_BUFFER_SIZE);
        WriteBufPos := 0;
        FSFloat := DefaultFormatSettings;
        FSFloat.DecimalSeparator  := '.';
        FSFloat.ThousandSeparator := #0;
        DefaultFBDate := Trunc(EncodeDate(2020, 1, 1)) + 15018;
        UpdateEveryNRows := Max(1, GetRefreshInterval);
        RowsSinceUpdate  := 0;
        ProgressForm := TForm.Create(Self);
        ProgressForm.Parent := Self;
        OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
        StartTime    := Now;
        CurrentRow   := 0;
        FCancelled   := False;
        try
        DefaultFormatSettings.DecimalSeparator := '.';
        // ---- Progress-Form ----
        ProgressForm.FormStyle   := fsNormal;
        ProgressForm.Caption     := 'Writing binary external file for ' + ATableName;
        ProgressForm.Width       := 580;
        ProgressForm.Height      := 280;
        ProgressForm.BorderStyle := bsNone;
        ProgressForm.Left        := (Self.Width - ProgressForm.Width) div 2;
        ProgressForm.Top         := ((Self.Height - ProgressForm.Height) div 2) + 20;
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
        LblCommit.Parent  := ProgressForm;
        LblCommit.Left    := 16;
        LblCommit.Top     := 75;
        LblCommit.Width   := 540;
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
        ProgressForm.Show;
        frmThemeSelector.btnApplyClick(ProgressForm);
        Application.ProcessMessages;
        // ---- Zieldatei öffnen (falls schon vorhanden, anhängen; sonst neu anlegen) ----
        if FileExists(ExtFilePath) then
        begin
        FS := TFileStream.Create(ExtFilePath, fmOpenReadWrite or fmShareDenyWrite);
        FS.Seek(0, soEnd);
        end
        else
        FS := TFileStream.Create(ExtFilePath, fmCreate or fmShareDenyWrite);
        FDataSet.DisableControls;
        try
        try
        FDataSet.First;
        for i := 1 to FromRow - 1 do
        FDataSet.Next;
        while (not FDataSet.EOF) and (CurrentRow < TotalRows) do
        begin
        if FCancelled then
        Break;
        // Alle Felder binär in RecBuf schreiben
        for f := 0 to FieldsPerRow - 1 do
        WriteFieldToRecord(ColMeta[f], CachedFields[f]);
        // Wenn RecBuf nicht mehr in WriteBuf passt → erst flushen
        if WriteBufPos + RecordSize > WRITE_BUFFER_SIZE then
        FlushWriteBuffer;
        Move(RecBuf[0], WriteBuf[WriteBufPos], RecordSize);
        Inc(WriteBufPos, RecordSize);
        Inc(CurrentRow);
        Inc(RowsSinceUpdate);
        FDataSet.Next;
        // UI & Puffer-Flush im konfigurierten Intervall
        if RowsSinceUpdate >= UpdateEveryNRows then
        begin
        FlushWriteBuffer;
        RowsSinceUpdate := 0;
        CommitBar.Position   := CurrentRow;
        ProgressBar.Position := CurrentRow;
        ElapsedSec := (Now - StartTime) * SecsPerDay;
        if ElapsedSec > 0.2 then
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
        // Restlichen Puffer auf Festplatte schreiben
        FlushWriteBuffer;
        finally
        FS.Free;
        end;
        finally
        EndTime := Now;
        FDataSet.EnableControls;
        SetLength(CachedFields, 0);
        SetLength(ColMeta, 0);
        SetLength(CheckedFields, 0);
        SetLength(RecBuf, 0);
        SetLength(WriteBuf, 0);
        end;
        // ---- Statistik ----
        FStats.RowsProcessed := CurrentRow;
        ElapsedSec := (EndTime - StartTime) * SecsPerDay;
        if ElapsedSec > 0 then
        RowsPerSec := CurrentRow / ElapsedSec
        else
        RowsPerSec := 0;
        if FStats.OptionsExtra = '' then
        FStats.OptionsExtra := Format(
        'Direct Binary External File Write (%d rows, %d bytes/rec), %.0f rows/sec',
        [CurrentRow, RecordSize, RowsPerSec])
        else
        FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
        '  ' + Format('Direct Binary External File Write (%d rows, %d bytes/rec), %.0f rows/sec',
        [CurrentRow, RecordSize, RowsPerSec]);
        if FCancelled then
        FStats.OptionsExtra := FStats.OptionsExtra + sLineBreak +
        Format('  CANCELLED after %s of %s rows',
        [FormatFloat('#,##0', CurrentRow), FormatFloat('#,##0', TotalRows)]);
        finally
        DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
        ProgressForm.Free;
        end;
        end;
// ============================================================================
// FELDNAME ZU SQL-IDENTIFIER (Case-Sensitive quotes bei gemischter Case)
// ============================================================================

function TfrmCreateTableFromDataSet.QuoteFieldName(
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
