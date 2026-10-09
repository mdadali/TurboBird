unit u_csv_bulk_exporter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Math, DateUtils, Dialogs,
  Graphics, StdCtrls, ExtCtrls,
  SynEdit, Grids, CheckLst, ComCtrls, DB, BufStream,
  IBDatabase, IBQuery, IBSQL, IBXScript, IBDataOutput,

  SysTables,
  turbocommon,
  fbcommon,
  uthemeselector,
  uFormulaPresets,
  fsimpleobjextractor,

  uSystemInfo,
  uReport;


type
  TCSVFieldInfo = record
    FieldName: string;
    FieldType: string;
    Detected: Boolean;
    MaxLength: Integer;
  end;

  TCSVFieldInfoArray = array of  TCSVFieldInfo;


  { TfrmCSVBulkExporter }

  TfrmCSVBulkExporter = class(TForm)
    btnAddToQueue: TButton;
    btnClose: TButton;
    btnDeselectAll: TButton;
    btnExecute: TButton;
    btnPreviewSQL: TButton;
    btnRefreshPresets: TButton;
    btnExportFileName: TButton;
    btnSelectAll: TButton;
    cbFormulaPreset: TComboBox;
    chkLstFields: TCheckListBox;
    comboxSourceDB: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    edtByteBuffer: TEdit;
    edtExportFileName: TEdit;
    edtBatchSize: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    edtLineBuffer: TEdit;
    grboxExportOptions: TGroupBox;
    grBoxFields: TGroupBox;
    grBoxFormulaPresets: TGroupBox;
    grBoxSource: TGroupBox;
    grBoxGeneratedQuery: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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
    procedure chkLstFieldsClickCheck(Sender: TObject);
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
    FDB: TIBDatabase;
    FTrans: TIBTransaction;
    FCancelled: Boolean;

    FInitialTableName: string;
    FInitialDBIndex: Integer;
    FUpdatingCombos: Boolean;
    FLastPresetIndex: Integer;

    procedure LoadServerList;
    procedure LoadDBList;
    procedure LoadTableList;
    procedure LoadFields;
    procedure LoadServerListFillOnly;
    procedure LoadFormulaPresets;
    procedure ApplyInitialSelection;
    procedure ApplyPresetToGrid;
    procedure ApplySkipTypes;
    function  CountManualFormulas: Integer;
    function  GetBatchSize: Integer;
    function  GetFromRow: Integer;
    function  GetToRow: Integer;
    function  GetActivePreset: TFormulaPreset;
    procedure CancelClick(Sender: TObject);
    function  BuildExportSQL: string;
    function  BuildHeaderLine: string;
    function  NeedsQuoting(const AFieldType: string): Boolean;
    function  WrapWithQuotes(const AFieldExpr, AQuoteChar: string): string;
    function  IsBooleanType(const AFieldType: string): Boolean;
    function  IsProblemField(const AFieldType: string): Boolean;
    function  BuildBooleanExpr(const AFieldExpr: string; Preset: TFormulaPreset): string;
    function  CollectProblemFields: TStringList;
    procedure DeselectProblemFields;
    function  ShowProblemDialog(const AMessage: string): Integer;
    procedure ValidateAndEnableRun;
    procedure DoBulkExport(const ASQL: string);
    function  CheckAndResolveProblemFields: Boolean;
    function  IsBlobBinaryType(const AFieldType: string): Boolean;
  public
    procedure Init(ANodeInfos: TPNodeInfos; const ATableName: string);
  end;

implementation

{$R *.lfm}

// ============================================================================
// HILFSFUNKTIONEN
// ============================================================================
function ExpandSep(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '\t', #9,  [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
end;


function TfrmCSVBulkExporter.CheckAndResolveProblemFields: Boolean;
var
  ProblemFields: TStringList;
  MsgText: string;
  Answer: Integer;
begin
  Result := True;

  ProblemFields := CollectProblemFields;
  try
    if ProblemFields.Count = 0 then
      Exit(True);

    MsgText :=
      'Bulk Export cannot export the following field types:' + sLineBreak +
      sLineBreak +
      ProblemFields.Text +
      sLineBreak +
      'These fields may cause SQL errors.' + sLineBreak +
      sLineBreak +
      'Hint:' + sLineBreak +
      '  • Arrays       → not exportable to text formats' + sLineBreak +
      '  • BLOB Binary  → use BLOB SUB_TYPE TEXT instead' + sLineBreak +
      sLineBreak +
      '─────────────────────────────────────' + sLineBreak +
      'What do you want to do?' + sLineBreak +
      sLineBreak +
      '[Deselect && Continue]' + sLineBreak +
      '  Critical fields are deselected.' + sLineBreak +
      '  The SQL is generated and shown in the editor above.' + sLineBreak +
      '  You can edit it if needed, then click "Execute" to start.' + sLineBreak +
      sLineBreak +
      '[Continue Anyway]' + sLineBreak +
      '  Critical fields stay active.' + sLineBreak +
      '  The SQL is generated and shown in the editor above.' + sLineBreak +
      '  You can edit it if needed, then click "Execute" to start.' + sLineBreak +
      '  Export may fail with SQL error.' + sLineBreak +
      sLineBreak +
      '[Cancel]' + sLineBreak +
      '  Aborts the operation.';

    Answer := ShowProblemDialog(MsgText);

    case Answer of
      mrYes:    DeselectProblemFields;   // Deselect & Continue
      mrNo:     ;                         // Continue Anyway — nichts tun
      mrCancel: Result := False;          // Cancel
    end;
  finally
    ProblemFields.Free;
  end;
end;

// ============================================================================
// INITIALISIERUNG
// ============================================================================

procedure TfrmCSVBulkExporter.Init(ANodeInfos: TPNodeInfos; const ATableName: string);
begin
  FInitialTableName := Trim(ATableName);
  FInitialDBIndex := -1;
  if Assigned(ANodeInfos) then
    FInitialDBIndex := ANodeInfos^.dbIndex;
end;

procedure TfrmCSVBulkExporter.LoadServerListFillOnly;
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

procedure TfrmCSVBulkExporter.LoadServerList;
begin
  ApplyInitialSelection;
end;

procedure TfrmCSVBulkExporter.ApplyInitialSelection;
var
  ServerName, DBTitle: string;
  Idx: Integer;
begin
  FUpdatingCombos := True;
  try
    LoadServerListFillOnly;

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

  if comboxSourceServer.ItemIndex >= 0 then
    comboxSourceServerChange(nil);

  if (FInitialDBIndex >= 0) and (FInitialDBIndex < Length(RegisteredDatabases)) then
  begin
    DBTitle := RegisteredDatabases[FInitialDBIndex].RegRec.Title;
    Idx := comboxSourceDB.Items.IndexOf(DBTitle);
    if Idx >= 0 then
    begin
      comboxSourceDB.ItemIndex := Idx;
      comboxSourceDBChange(nil);
    end;
  end;

  if (FInitialTableName <> '') and (comboxSourceTables.Items.Count > 0) then
  begin
    Idx := comboxSourceTables.Items.IndexOf(FInitialTableName);
    if Idx >= 0 then
    begin
      comboxSourceTables.ItemIndex := Idx;
      comboxSourceTablesChange(nil);
    end;
  end;
end;

procedure TfrmCSVBulkExporter.FormCreate(Sender: TObject);
begin
  sgFields.ColCount := 3;
  sgFields.Cells[0, 0] := 'Source Field';
  sgFields.Cells[1, 0] := 'Field Type';
  sgFields.Cells[2, 0] := 'Formula ($1 = value)';
  sgFields.ColWidths[0] := 150;
  sgFields.ColWidths[1] := 120;
  sgFields.ColWidths[2] := 250;

  btnExecute.Enabled := False;
  btnPreviewSQL.Enabled := False;

  FSourceDBIndex := -1;
  FDB := nil;
  FTrans := nil;

  FInitialTableName := '';
  FInitialDBIndex := -1;
  FUpdatingCombos := False;
  FLastPresetIndex := -1;

  edtBatchSize.Text := IntToStr(DefaultBatchSize);

  LoadFormulaPresets;
end;

procedure TfrmCSVBulkExporter.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
  ApplyInitialSelection;

  if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
    btnPreviewSQLClick(nil);
end;

// ============================================================================
// PRESET-MANAGEMENT
// ============================================================================

procedure TfrmCSVBulkExporter.LoadFormulaPresets;
var
  i, DefIdx: Integer;
begin
  cbFormulaPreset.Items.Clear;

  for i := 0 to FormulaPresetManager.PresetCount - 1 do
    cbFormulaPreset.Items.Add(FormulaPresetManager.PresetName(i));

  if cbFormulaPreset.Items.Count = 0 then
  begin
    cbFormulaPreset.ItemIndex := -1;
    FLastPresetIndex := -1;
    Exit;
  end;

  DefIdx := FormulaPresetManager.GetDefaultPresetIndex;
  if DefIdx < 0 then DefIdx := 0;

  cbFormulaPreset.ItemIndex := DefIdx;
  FLastPresetIndex := DefIdx;
end;

function TfrmCSVBulkExporter.GetActivePreset: TFormulaPreset;
begin
  if cbFormulaPreset.ItemIndex < 0 then
    Exit(nil);
  Result := FormulaPresetManager.GetPresetByIndex(cbFormulaPreset.ItemIndex);
end;

procedure TfrmCSVBulkExporter.ApplyPresetToGrid;
var
  Preset: TFormulaPreset;
  i: Integer;
  Formula: string;
begin
  Preset := GetActivePreset;
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;
    Formula := Preset.GetFormulaForFieldType(FFields[i].FieldType);
    FFields[i].Formula := Formula;
    sgFields.Cells[2, i + 1] := Formula;
  end;
end;

procedure TfrmCSVBulkExporter.ApplySkipTypes;
var
  Preset: TFormulaPreset;
  i: Integer;
begin
  Preset := GetActivePreset;
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    if i >= chkLstFields.Count then Break;
    if Preset.ShouldSkipType(FFields[i].FieldType) then
    begin
      FFields[i].Checked := False;
      chkLstFields.Checked[i] := False;
    end;
  end;
end;

function TfrmCSVBulkExporter.CountManualFormulas: Integer;
var
  i: Integer;
  Preset: TFormulaPreset;
  Expected, Actual: string;
begin
  Result := 0;
  if (FLastPresetIndex < 0) or (FLastPresetIndex >= FormulaPresetManager.PresetCount) then
    Exit;
  Preset := FormulaPresetManager.GetPresetByIndex(FLastPresetIndex);
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;
    Expected := Trim(Preset.GetFormulaForFieldType(FFields[i].FieldType));
    Actual := Trim(sgFields.Cells[2, i + 1]);
    if (Actual <> '') and (Actual <> Expected) then
      Inc(Result);
  end;
end;

procedure TfrmCSVBulkExporter.cbFormulaPresetChange(Sender: TObject);
var
  NewIdx, ManualCount: Integer;
begin
  if FUpdatingCombos then Exit;

  NewIdx := cbFormulaPreset.ItemIndex;
  if NewIdx = FLastPresetIndex then Exit;

  ManualCount := CountManualFormulas;
  if (ManualCount > 0) and (NewIdx >= 0) then
  begin
    if MessageDlg(
         Format('%d field(s) have manual formulas.' + sLineBreak +
                'Changing the preset will overwrite them.' + sLineBreak + sLineBreak +
                'Continue?', [ManualCount]),
         mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      FUpdatingCombos := True;
      try
        cbFormulaPreset.ItemIndex := FLastPresetIndex;
      finally
        FUpdatingCombos := False;
      end;
      Exit;
    end;
  end;

  FLastPresetIndex := NewIdx;
  ApplyPresetToGrid;
  ApplySkipTypes;
  ValidateAndEnableRun;

  if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
    btnPreviewSQLClick(nil);
end;

procedure TfrmCSVBulkExporter.chkLstFieldsClickCheck(Sender: TObject);
begin
  if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
    btnPreviewSQLClick(nil);
end;

procedure TfrmCSVBulkExporter.btnRefreshPresetsClick(Sender: TObject);
begin
  FormulaPresetManager.Reload;
  LoadFormulaPresets;
  if cbFormulaPreset.ItemIndex >= 0 then
  begin
    ApplyPresetToGrid;
    ApplySkipTypes;
    FLastPresetIndex := cbFormulaPreset.ItemIndex;
    ValidateAndEnableRun;

    if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
      btnPreviewSQLClick(nil);
  end;
end;

procedure TfrmCSVBulkExporter.sgFieldsDblClick(Sender: TObject);
var
  NewFormula: string;
  Row: Integer;
begin
  Row := sgFields.Row;
  if (Row < 1) or (Row >= sgFields.RowCount) then Exit;

  NewFormula := sgFields.Cells[2, Row];
  if InputQuery('Formula for ' + sgFields.Cells[0, Row],
                'Enter SQL expression ($1 = field value):', NewFormula) then
  begin
    sgFields.Cells[2, Row] := NewFormula;
    if Row - 1 <= High(FFields) then
      FFields[Row - 1].Formula := NewFormula;

    // NEU: Auto-Preview, wenn Felder geladen sind
    if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
      btnPreviewSQLClick(nil);
  end;
end;

// ============================================================================
// VALIDATOR
// ============================================================================

procedure TfrmCSVBulkExporter.ValidateAndEnableRun;
var
  Preset: TFormulaPreset;
  Issues: TPresetIssues;
  HasErr: Boolean;
begin
  Preset := GetActivePreset;
  if Preset = nil then
  begin
    btnPreviewSQL.Enabled := False;
    btnExecute.Enabled := False;
    Exit;
  end;

  Issues := TPresetValidator.Validate(Preset);
  HasErr := TPresetValidator.HasErrors(Issues);

  if HasErr then
  begin
    MessageDlg('Preset has errors:' + sLineBreak + sLineBreak +
               TPresetValidator.BuildMessage(Issues),
               mtError, [mbOK], 0);
    btnPreviewSQL.Enabled := False;
    btnExecute.Enabled := False;
  end
  else
  begin
    btnPreviewSQL.Enabled := True;
    if syneditGenerateQuery.Text = '' then
      btnExecute.Enabled := False;
  end;
end;

// ============================================================================
// SOURCE-KASKADE
// ============================================================================

procedure TfrmCSVBulkExporter.comboxSourceServerChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  LoadDBList;
end;

procedure TfrmCSVBulkExporter.LoadDBList;
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

procedure TfrmCSVBulkExporter.comboxSourceDBChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdatingCombos then Exit;

  // FSourceDBIndex frisch ermitteln
  FSourceDBIndex := -1;
  for i := 0 to High(RegisteredDatabases) do
    if SameText(Trim(RegisteredDatabases[i].RegRec.ServerName), Trim(comboxSourceServer.Text)) and
       SameText(Trim(RegisteredDatabases[i].RegRec.Title), Trim(comboxSourceDB.Text)) then
    begin
      FSourceDBIndex := i;
      Break;
    end;

  if FSourceDBIndex < 0 then Exit;

  // Wenn die geteilte DB nicht verbunden ist → Login-Flow auslösen
  // (zeigt Dialog, füllt Cache, setzt IBDatabase.Params)
  if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
     (not RegisteredDatabases[FSourceDBIndex].IBDatabase.Connected) then
    ConnectToDBAs(FSourceDBIndex);

  // Jetzt die Form-eigene DB aufbauen — AssignIBDatabase kopiert die
  // frisch gesetzten Credentials mit, CachePasswordAfterConnect ist damit
  // ein No-Op (Passwort ist schon in Params)
  LoadTableList;
end;

procedure TfrmCSVBulkExporter.LoadTableList;
var
  Extractor: TSimpleObjExtractor;
  TableList: TStringList;
begin
  if FSourceDBIndex < 0 then Exit;

  // ============================================================
  // Alte eigene Verbindung abbauen
  // ============================================================
  if Assigned(FDB) then
  begin
    if FDB.Connected then FDB.Connected := False;
    FreeAndNil(FDB);
  end;
  if Assigned(FTrans) then FreeAndNil(FTrans);

  // ============================================================
  // Form-eigene Verbindung aufbauen
  //
  // Voraussetzung: ConnectToDBAs wurde bereits vom Combo-Change
  // aufgerufen → RegisteredDatabases[FSourceDBIndex].IBDatabase
  // ist verbunden und Params enthält user_name + password.
  //
  // AssignIBDatabase kopiert diese Params (inkl. Credentials)
  // in die Form-eigene Instanz — keine eigene Credential-Suche nötig.
  // ============================================================
  FDB := TIBDatabase.Create(nil);
  FTrans := TIBTransaction.Create(nil);
  FDB.DefaultTransaction := FTrans;
  FTrans.DefaultDatabase  := FDB;

  AssignIBDatabase(RegisteredDatabases[FSourceDBIndex].IBDatabase, FDB);
  SetDBInstanceIndex(FDB, FSourceDBIndex);

  // UserName sicherstellen (falls die geteilte Instanz keinen hatte)
  if Trim(FDB.Params.Values['user_name']) = '' then
    FDB.Params.Values['user_name'] :=
      RegisteredDatabases[FSourceDBIndex].RegRec.UserName;

  FDB.Connected := True;

  // Post-Connect Cache-Fill — No-Op wenn Params['password'] schon
  // gefüllt ist (durch ConnectToDBAs), sonst greift es als Sicherheitsnetz
  CachePasswordAfterConnect(FSourceDBIndex, FDB);

  FTrans.StartTransaction;

  // ============================================================
  // Tabellenliste über den Extractor holen — FBIdentifierCast
  // umgeht den CHAR(31)-Abschneide-Bug auf FB 1.5
  // ============================================================
  comboxSourceTables.Items.Clear;

  Extractor := TSimpleObjExtractor.Create(FSourceDBIndex);
  try
    TableList := TStringList.Create;
    try
      Extractor.ExtractObjectNames(FSourceDBIndex, otTables, false,
        TStrings(TableList), '');
      comboxSourceTables.Items.AddStrings(TableList);
    finally
      TableList.Free;
    end;
  finally
    Extractor.Free;
  end;

  if comboxSourceTables.Items.Count > 0 then
  begin
    comboxSourceTables.ItemIndex := 0;
    LoadFields;
  end;

  if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
    btnPreviewSQLClick(nil);

  btnPreviewSQL.Enabled := True;
end;

procedure TfrmCSVBulkExporter.comboxSourceTablesChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  LoadFields;
  syneditGenerateQuery.Clear;
  btnExecute.Enabled := False;

  if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
    btnPreviewSQLClick(nil);
end;

procedure TfrmCSVBulkExporter.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FDB) then
  begin
    if FDB.Connected then FDB.Connected := False;
    FreeAndNil(FDB);
  end;
  if Assigned(FTrans) then FreeAndNil(FTrans);
end;

// ============================================================================
// FELDER LADEN
// ============================================================================

procedure TfrmCSVBulkExporter.LoadFields;
var
  RawFields: TFBFieldRawArray;
  Extractor: TSimpleObjExtractor;
  i: Integer;
  TypeStr: string;
begin
  if FSourceDBIndex < 0 then Exit;
  if Trim(comboxSourceTables.Text) = '' then Exit;

  // 1. Rohdaten holen
  Extractor := TSimpleObjExtractor.Create(FSourceDBIndex);
  try
    RawFields := Extractor.GetTableFieldsRaw(Trim(comboxSourceTables.Text));
  finally
    Extractor.Free;
  end;

  // 2. In CSV-eigenen Zustand umwandeln
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

  for i := 0 to High(RawFields) do
  begin
    SetLength(FFields, i + 1);

    // Name aus Raw
    FFields[i].FieldName := RawFields[i].FieldName;

    // Typ-String bauen
    TypeStr := GetFBTypeName(
      RawFields[i].FieldType,
      RawFields[i].FieldSubType,
      RawFields[i].FieldLength,
      RawFields[i].FieldPrecision,
      RawFields[i].FieldScale,
      RawFields[i].CharacterSetName,
      RawFields[i].CharacterLength
    );
    FFields[i].FieldType := TypeStr;

    // CSV-eigene Felder initialisieren
    FFields[i].Checked := True;
    FFields[i].Formula := '';

    // UI
    chkLstFields.Items.Add(FFields[i].FieldName);
    chkLstFields.Checked[i] := True;

    sgFields.RowCount := i + 2;
    sgFields.Cells[0, i + 1] := FFields[i].FieldName;
    sgFields.Cells[1, i + 1] := FFields[i].FieldType;
    sgFields.Cells[2, i + 1] := '';
  end;

  if cbFormulaPreset.ItemIndex >= 0 then
  begin
    ApplyPresetToGrid;
    ApplySkipTypes;
    FLastPresetIndex := cbFormulaPreset.ItemIndex;
    ValidateAndEnableRun;
  end;
end;

procedure TfrmCSVBulkExporter.btnSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := True;

  if (Length(FFields) > 0) and (chkLstFields.Count > 0) then
    btnPreviewSQLClick(nil);
end;

procedure TfrmCSVBulkExporter.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := False;

  syneditGenerateQuery.Lines.Clear;
end;

// ============================================================================
// BATCH / RANGE
// ============================================================================

procedure TfrmCSVBulkExporter.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

function TfrmCSVBulkExporter.GetBatchSize: Integer;
begin
  Result := StrToIntDef(edtBatchSize.Text, 1000000);
  if Result < 1 then Result := 1000000;
end;

function TfrmCSVBulkExporter.GetFromRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtFrom.Text, 1)
  else
    Result := 1;
end;

function TfrmCSVBulkExporter.GetToRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtTo.Text, MaxInt)
  else
    Result := MaxInt;
end;

// ============================================================================
// TYP-HELFER
// ============================================================================

function TfrmCSVBulkExporter.NeedsQuoting(const AFieldType: string): Boolean;
var
  CT: string;
begin
  CT := UpperCase(AFieldType);

  if Pos('SMALLINT', CT) > 0 then Exit(False);
  if Pos('BIGINT',   CT) > 0 then Exit(False);
  if Pos('INTEGER',  CT) > 0 then Exit(False);
  if Pos('FLOAT',    CT) > 0 then Exit(False);
  if Pos('DOUBLE',   CT) > 0 then Exit(False);
  if Pos('NUMERIC',  CT) > 0 then Exit(False);
  if Pos('DECIMAL',  CT) > 0 then Exit(False);
  if Pos('DECFLOAT', CT) > 0 then Exit(False);
  if Pos('INT128',   CT) > 0 then Exit(False);
  if Pos('BOOLEAN',  CT) > 0 then Exit(False);

  Result := True;
end;

function TfrmCSVBulkExporter.IsBooleanType(const AFieldType: string): Boolean;
begin
  Result := Pos('BOOLEAN', UpperCase(AFieldType)) > 0;
end;

function TfrmCSVBulkExporter.IsProblemField(const AFieldType: string): Boolean;
var
  CT: string;
begin
  CT := UpperCase(AFieldType);

  // Arrays (nicht exportierbar)
  if (Pos('ARRAY', CT) > 0) or (Pos('[', AFieldType) > 0) then
    Exit(True);

  // BLOB SUB_TYPE BINARY (binär, nicht als Text exportierbar)
  if (Pos('BLOB', CT) > 0) and (Pos('BINARY', CT) > 0) then
    Exit(True);

  Result := False;
end;

function TfrmCSVBulkExporter.BuildBooleanExpr(
  const AFieldExpr: string; Preset: TFormulaPreset): string;
var
  TrueLit, FalseLit: string;
begin
  if Preset = nil then
  begin
    Result := AFieldExpr;
    Exit;
  end;

  TrueLit  := Preset.BooleanTrue;
  FalseLit := Preset.BooleanFalse;

  if TrueLit = ''  then TrueLit  := 'TRUE';
  if FalseLit = '' then FalseLit := 'FALSE';

  Result := 'CASE WHEN ' + AFieldExpr + ' THEN ' + QuotedStr(TrueLit) +
            ' ELSE ' + QuotedStr(FalseLit) + ' END';
end;

function TfrmCSVBulkExporter.WrapWithQuotes(
  const AFieldExpr, AQuoteChar: string): string;
var
  QC, QC2: string;
begin
  if AQuoteChar = '' then
    Exit(AFieldExpr);

  QC  := QuotedStr(AQuoteChar);
  QC2 := QuotedStr(AQuoteChar + AQuoteChar);

  Result := QC + ' || REPLACE(' + AFieldExpr + ', ' + QC + ', ' + QC2 + ') || ' + QC;
end;

// ============================================================================
// PROBLEM-FELD-DIALOG
// ============================================================================

function TfrmCSVBulkExporter.CollectProblemFields: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to High(FFields) do
  begin
    if i >= chkLstFields.Count then Break;
    if not chkLstFields.Checked[i] then Continue;
    if IsProblemField(FFields[i].FieldType) then
      Result.Add('  • ' + FFields[i].FieldName +
                 '  (' + FFields[i].FieldType + ')');
  end;
end;

procedure TfrmCSVBulkExporter.DeselectProblemFields;
var
  i: Integer;
begin
  for i := 0 to High(FFields) do
  begin
    if i >= chkLstFields.Count then Break;
    if IsProblemField(FFields[i].FieldType) then
    begin
      FFields[i].Checked := False;
      chkLstFields.Checked[i] := False;
    end;
  end;
end;

function TfrmCSVBulkExporter.ShowProblemDialog(const AMessage: string): Integer;
var
  Dlg: TForm;
  Btn: TButton;
  i: Integer;
  YesBtn, NoBtn, CancelBtn: TButton;
begin
  Result := mrCancel;

  Dlg := CreateMessageDialog(AMessage, mtWarning, [mbYes, mbNo, mbCancel]);
  try
    Dlg.Caption := 'Bulk Export — Problem Fields';
    Dlg.Width := Dlg.Width + 250;   // breiter für Info-Text

    YesBtn := nil;
    NoBtn := nil;
    CancelBtn := nil;

    for i := 0 to Dlg.ComponentCount - 1 do
    begin
      if Dlg.Components[i] is TButton then
      begin
        Btn := TButton(Dlg.Components[i]);
        if SameText(Btn.Name, 'Yes') then YesBtn := Btn
        else if SameText(Btn.Name, 'No') then NoBtn := Btn
        else if SameText(Btn.Name, 'Cancel') then CancelBtn := Btn;
      end;
    end;

    if Assigned(YesBtn) then
    begin
      YesBtn.Caption := 'Deselect && Continue';
      YesBtn.Width := 160;
    end;
    if Assigned(NoBtn) then
    begin
      NoBtn.Caption := 'Continue Anyway';
      NoBtn.Width := 160;
    end;
    if Assigned(CancelBtn) then
    begin
      CancelBtn.Caption := 'Cancel';
      CancelBtn.Width := 100;
    end;

    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

// ============================================================================
// SQL-BAU
// ============================================================================
function TfrmCSVBulkExporter.BuildExportSQL: string;
var
  i: Integer;
  Preset: TFormulaPreset;
  FieldExpr, Formula, Separator: string;
  IsBool, IsNum, IsStr, IsBinaryBlob: Boolean;
  LineExpr: string;
begin
  Result := '';

  Preset := GetActivePreset;
  if Preset = nil then
  begin
    ShowMessage('Please select a formula preset.');
    Exit;
  end;

  Separator := ExpandSep(Preset.Separator);

  LineExpr := '';
  for i := 0 to High(FFields) do
  begin
    if i >= chkLstFields.Count then Break;
    if not chkLstFields.Checked[i] then Continue;
    if i + 1 >= sgFields.RowCount then Break;

    // Formel aus Grid (oder $1)
    Formula := Trim(sgFields.Cells[2, i + 1]);
    if Formula = '' then
      Formula := '$1';

    Formula := StringReplace(Formula, '$1', FFields[i].FieldName, [rfReplaceAll]);
    FieldExpr := '(' + Formula + ')';

    IsBool := IsBooleanType(FFields[i].FieldType);
    IsNum  := not IsBool and not NeedsQuoting(FFields[i].FieldType);
    IsStr  := not IsBool and not IsNum;

    // NEU: Binary BLOB erkennen
    IsBinaryBlob := IsBlobBinaryType(FFields[i].FieldType);

    // Boolean: CASE WHEN ...
    if IsBool then
    begin
      FieldExpr := BuildBooleanExpr(FieldExpr, Preset);
      FieldExpr := 'COALESCE(' + FieldExpr + ', '''')';
    end
    // Numerisch: CAST + COALESCE
    else if IsNum then
    begin
      FieldExpr := 'COALESCE(CAST(' + FieldExpr + ' AS VARCHAR(50)), '''')';
    end
    // String/Datum/BLOB: COALESCE
    else
    begin
      FieldExpr := 'COALESCE(' + FieldExpr + ', '''')';
    end;

    // Quote-Wrap — ABER: Binary BLOB überspringen!
    if IsStr and Preset.DataQuoted and (Preset.QuoteChar <> '') and
       (not IsBinaryBlob) then
      FieldExpr := WrapWithQuotes(FieldExpr, Preset.QuoteChar);

    // Erster Ausdruck oder Folge-Ausdruck
    if LineExpr = '' then
      LineExpr := '  ' + FieldExpr
    else
    begin
      if Separator <> '' then
        LineExpr := LineExpr + sLineBreak +
                    '  || ' + QuotedStr(Separator) + ' || ' + FieldExpr
      else
        LineExpr := LineExpr + sLineBreak +
                    '  || ' + FieldExpr;
    end;
  end;

  if LineExpr = '' then
  begin
    ShowMessage('No fields selected.');
    Exit;
  end;

  // SELECT mehrzeilig
  Result := 'SELECT' + sLineBreak +
            LineExpr + sLineBreak +
            '  AS csv_line' + sLineBreak +
            'FROM ' + MakeObjectNameQuoted(comboxSourceTables.Text);
end;

function TfrmCSVBulkExporter.IsBlobBinaryType(const AFieldType: string): Boolean;
var
  CT: string;
begin
  CT := UpperCase(AFieldType);

  // BLOB SUB_TYPE BINARY erkennen
  // Typische Schreibweisen:
  //   "BLOB SUB_TYPE 0"
  //   "BLOB SUB_TYPE BINARY"
  //   "BLOB"  (Default ist BINARY in Firebird)
  Result := (Pos('BLOB', CT) > 0) and
            ((Pos('BINARY', CT) > 0) or (Pos('SUBTYPE 0', CT) > 0));
end;

function TfrmCSVBulkExporter.BuildHeaderLine: string;
var
  i: Integer;
  Preset: TFormulaPreset;
  Sep, QC, Line: string;
begin
  Result := '';
  Preset := GetActivePreset;
  if Preset = nil then Exit;
  if not Preset.IncludeHeader then Exit;

  Sep := ExpandSep(Preset.Separator);
  QC := Preset.QuoteChar;

  Line := '';
  for i := 0 to High(FFields) do
  begin
    if i >= chkLstFields.Count then Break;
    if not chkLstFields.Checked[i] then Continue;
    if Line <> '' then Line := Line + Sep;
    if Preset.HeaderQuoted and (QC <> '') then
      Line := Line + QC + FFields[i].FieldName + QC
    else
      Line := Line + FFields[i].FieldName;
  end;

  if Line <> '' then
    Result := Line + sLineBreak;
end;

// ============================================================================
// PREVIEW SQL
// ============================================================================

procedure TfrmCSVBulkExporter.btnPreviewSQLClick(Sender: TObject);
var
  SQL: string;
begin
  if not CheckAndResolveProblemFields then
    Exit;

  SQL := BuildExportSQL;
  if SQL = '' then Exit;

  syneditGenerateQuery.Text := SQL;
  btnExecute.Enabled := True;
end;

// ============================================================================
// EXPORT AUSFÜHREN
// ============================================================================

procedure TfrmCSVBulkExporter.btnExecuteClick(Sender: TObject);
var
  ExportSQL: string;
begin
  ExportSQL := Trim(syneditGenerateQuery.Text);
  if ExportSQL = '' then
  begin
    ShowMessage('No SQL to execute. Please click "Preview SQL" first.');
    Exit;
  end;
  if Trim(edtExportFileName.Text) = '' then
  begin
    ShowMessage('Please select an export file.');
    Exit;
  end;

  DoBulkExport(ExportSQL);
end;

procedure TfrmCSVBulkExporter.btnExportFileNameClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    Filter := 'CSV files (*.csv)|*.csv|Text files (*.txt)|*.txt|All files (*.*)|*.*';
    if Execute then
      edtExportFileName.Text := FileName;
  finally
    Free;
  end;
end;

procedure TfrmCSVBulkExporter.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCSVBulkExporter.btnAddToQueueClick(Sender: TObject);
begin
  MessageDlg('Queue feature coming soon!', mtInformation, [mbOK], 0);
end;

procedure TfrmCSVBulkExporter.CancelClick(Sender: TObject);
begin
  FCancelled := True;
  if Sender is TButton then
  begin
    TButton(Sender).Enabled := False;
    TButton(Sender).Caption := 'Cancelling...';
  end;
end;

// ============================================================================
// BULK EXPORT ENGINE
// ============================================================================

{procedure TfrmCSVBulkExporter.DoBulkExport(const ASQL: string);
var
  TotalRows, ExpectedRows: Int64;
  Exported: Int64;
  FromRow, ToRow: Integer;
  UseRange: Boolean;
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
  LenLine, LenEnd: Integer;
  SQL, BaseSQL, CountSQL: string;
  FileStream: TBufferedFileStream;
  BufferBytes: Int64;
  RowsPerUpdate: Integer;
  RowsSinceUpdate: Int64;
  FileSize: Int64;
  Stats: TTransferStatistic;
  ReportForm: TfrmReport;
  i, CheckedCount: Integer;
  HeaderLine: string;

  procedure UpdateProgress;
  var
    ElapsedSec: Double;
    RowsPerSec: Double;
  begin
    if ExpectedRows > 0 then
      ProgressBar.Position := Exported;

    ElapsedSec := (Now - StartTime) * 86400;
    if ElapsedSec > 0 then
      RowsPerSec := Exported / ElapsedSec
    else
      RowsPerSec := 0;

    if ExpectedRows > 0 then
      ProgressLabel.Caption := Format('Exported %s of %s rows',
        [FormatFloat('#,##0', Exported), FormatFloat('#,##0', ExpectedRows)])
    else
      ProgressLabel.Caption := Format('Exported %s rows',
        [FormatFloat('#,##0', Exported)]);

    LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
      '   |   ' + FormatFloat('#,##0', Round(RowsPerSec)) + ' rows/sec';

    Application.ProcessMessages;
  end;

begin
  UseRange := rbRange.Checked;
  FromRow := GetFromRow;
  ToRow := GetToRow;

  // ------------------------------------------------------------------
  // Buffer-Größe (hart kodiert wie in der schnellen Version)
  // ------------------------------------------------------------------
  BufferBytes := StrToIntDef(edtByteBuffer.Text, (1024 * 1024 * 64)); //64 MB
  if BufferBytes < 64 * 1024 then BufferBytes := 64 * 1024;
  if BufferBytes > 64 * 1024 * 1024 then BufferBytes := 64 * 1024 * 1024;

  // ------------------------------------------------------------------
  // Zeilen pro GUI-Update (aus edtLineBuffer)
  // ------------------------------------------------------------------
  RowsPerUpdate := StrToIntDef(edtLineBuffer.Text, 10000);
  if RowsPerUpdate < 1 then
    RowsPerUpdate := 10000;

  // ------------------------------------------------------------------
  // Statistik
  // ------------------------------------------------------------------
  FillChar(Stats, SizeOf(Stats), 0);
  Stats.Kind             := tkExport;
  Stats.SourceKind       := 'Firebird Table';
  Stats.SourceServer     := comboxSourceServer.Text;
  Stats.SourceDatabase   := comboxSourceDB.Text;
  Stats.SourceTable      := comboxSourceTables.Text;
  Stats.DestKind         := 'File';
  Stats.DestFileName     := edtExportFileName.Text;
  Stats.BatchSize        := GetBatchSize;
  Stats.FormulaUsed      := True;
  Stats.FromRow          := FromRow;
  Stats.ToRow            := ToRow;
  Stats.UseRowRange      := UseRange;

  if (FSourceDBIndex >= 0) and (FSourceDBIndex < Length(RegisteredDatabases)) then
  begin
    Stats.SourceServerVersion := RegisteredDatabases[FSourceDBIndex].RegRec.ServerVersionString;
    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;
  end;

  Stats.SystemInfo := GetSystemInfo(edtExportFileName.Text);

  CheckedCount := 0;
  for i := 0 to High(FFields) do
    if chkLstFields.Checked[i] then Inc(CheckedCount);
  Stats.FieldsCount := CheckedCount;

  Stats.FormulasApplied := '';
  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;
    if not chkLstFields.Checked[i] then Continue;
    if Trim(sgFields.Cells[2, i + 1]) <> '' then
      Stats.FormulasApplied := Stats.FormulasApplied +
        '  • ' + FFields[i].FieldName + ' = ' + sgFields.Cells[2, i + 1] + sLineBreak;
  end;

  // ------------------------------------------------------------------
  // Eigene DB-Verbindung
  // ------------------------------------------------------------------
  DB := TIBDatabase.Create(nil);
  Trans := TIBTransaction.Create(nil);
  DB.DefaultTransaction := Trans;
  Trans.DefaultDatabase := DB;
  AssignIBDatabase(RegisteredDatabases[FSourceDBIndex].IBDatabase, DB);
  SetDBInstanceIndex(DB, FSourceDBIndex);

  DB.Params.Values['user_name'] := RegisteredDatabases[FSourceDBIndex].RegRec.UserName;

  if RegisteredDatabases[FSourceDBIndex].RegRec.Password <> '' then
    DB.Params.Values['password'] := RegisteredDatabases[FSourceDBIndex].RegRec.Password
  else
    DB.Params.Values['password'] := GetDBSessionPassword(
      RegisteredDatabases[FSourceDBIndex].RegRec.ServerName,
      RegisteredDatabases[FSourceDBIndex].RegRec.DatabaseName);

  DB.LoginPrompt := true;
  DB.OnLogin := @dmSysTables.OnDatabaseLogin;
  DB.Connected := True;
  CachePasswordAfterConnect(FSourceDBIndex, DB);
  Trans.StartTransaction;

  Q := TIBSQL.Create(nil);
  Q.Database := DB;
  Q.Transaction := Trans;

  BaseSQL := Trim(ASQL);
  if (Length(BaseSQL) >= 6) and (UpperCase(Copy(BaseSQL, 1, 6)) = 'SELECT') then
    BaseSQL := Trim(Copy(BaseSQL, 7, MaxInt));

  LineEnd := sLineBreak;
  HeaderLine := BuildHeaderLine;

  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.Width := 520;
    ProgressForm.Height := 220;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;
    ProgressForm.Caption := 'CSV Bulk Exporter';

    LblPhase := TLabel.Create(ProgressForm);
    LblPhase.Parent := ProgressForm;
    LblPhase.Left := 16;
    LblPhase.Top := 16;
    LblPhase.Caption := 'Counting rows...';
    LblPhase.Font.Style := [fsBold];
    LblPhase.Width := 480;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 42;
    ProgressLabel.Caption := 'Please wait...';
    ProgressLabel.Width := 480;

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 70;
    ProgressBar.Width := 480;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;
    ProgressBar.Style := pbstMarquee;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 100;
    LblElapsed.Caption := 'Elapsed: 00:00:00';
    LblElapsed.Width := 480;

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 200;
    BtnCancel.Top := 140;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelClick;

    ProgressForm.Show;
    ProgressForm.BringToFront;
    Application.ProcessMessages;
    Sleep(50);
    Application.ProcessMessages;

    FCancelled := False;
    Exported := 0;
    RowsSinceUpdate := 0;

    // ============================================================
    // Phase 1: Zeilen zählen
    // ============================================================
    LblPhase.Caption := 'Counting rows...';
    Application.ProcessMessages;

    TotalRows := 0;
    try
      CountSQL := 'SELECT COUNT(*) FROM (SELECT ' + BaseSQL + ') AS cnt_qry';
      Q.SQL.Text := CountSQL;
      Q.ExecQuery;
      if not Q.EOF then
        TotalRows := Q.Fields[0].AsInt64;
      Q.Close;
    except
      TotalRows := 0;
    end;

    if UseRange then
    begin
      if ToRow > TotalRows then ToRow := TotalRows;
      if FromRow > TotalRows then FromRow := TotalRows;
      ExpectedRows := ToRow - FromRow + 1;
      if ExpectedRows < 0 then ExpectedRows := 0;
    end
    else
    begin
      ToRow := TotalRows;
      ExpectedRows := TotalRows;
    end;

    if ExpectedRows > 0 then
    begin
      ProgressBar.Style := pbstNormal;
      ProgressBar.Max := ExpectedRows;
      ProgressBar.Position := 0;
    end;

    // ============================================================
    // Phase 2: Datei öffnen + Header
    // ============================================================
    LblPhase.Caption := 'Opening output file...';
    Application.ProcessMessages;

    FileStream := TBufferedFileStream.Create(edtExportFileName.Text, fmCreate, BufferBytes);

    if HeaderLine <> '' then
      FileStream.Write(HeaderLine[1], Length(HeaderLine));

    // ============================================================
    // Phase 3: Export
    // ============================================================
    StartTime := Now;
    LblPhase.Caption := 'Exporting data...';
    Application.ProcessMessages;

    if UseRange and (FromRow > 1) then
      SQL := 'SELECT SKIP ' + IntToStr(FromRow - 1) +
             ' FIRST ' + IntToStr(ExpectedRows) + ' ' + BaseSQL
    else if UseRange then
      SQL := 'SELECT FIRST ' + IntToStr(ExpectedRows) + ' ' + BaseSQL
    else
      SQL := 'SELECT ' + BaseSQL;

    try
      try
        Q.Close;
        Q.SQL.Text := SQL;
        Q.ExecQuery;

        while not Q.EOF do
        begin
          if FCancelled then Break;

          Line := Q.Fields[0].AsString;
          LenLine := Length(Line);
          LenEnd := Length(LineEnd);

          if LenLine > 0 then
          begin
            try
              FileStream.Write(Line[1], LenLine);
            except
              on E: Exception do
              begin
                // Sauber abbrechen, nicht crashen
                raise Exception.Create('Write failed: ' + E.Message);
              end;
            end;
          end;

          Inc(Exported);
          Inc(RowsSinceUpdate);

          if RowsSinceUpdate >= RowsPerUpdate then
          begin
            RowsSinceUpdate := 0;
            UpdateProgress;
          end;

          Q.Next;
        end;
      except
        on E: Exception do
        begin
          FileStream.Flush;
          FileStream.Free;
          FileStream := nil;
          if FileExists(edtExportFileName.Text) then
            DeleteFile(edtExportFileName.Text);
          MessageDlg('Export failed:' + sLineBreak + sLineBreak + E.Message,
                     mtError, [mbOK], 0);
          Exit;
        end;
      end;

    finally
      if Assigned(FileStream) then
      begin
        FileStream.Flush;
        FileStream.Free;
      end;
    end;

    // ============================================================
    // Phase 4: Finalisieren
    // ============================================================
    LblPhase.Caption := 'Finalizing...';
    UpdateProgress;
    EndTime := Now;

    Stats.RowsProcessed  := Exported;
    Stats.ElapsedSeconds := (EndTime - StartTime) * SecsPerDay;

    FileSize := 0;
    if FileExists(edtExportFileName.Text) then
    begin
      try
        with TFileStream.Create(edtExportFileName.Text, fmOpenRead or fmShareDenyNone) do
        try
          FileSize := Size;
        finally
          Free;
        end;
      except
        FileSize := 0;
      end;
    end;
    Stats.DestFileSize := FileSize;

    if FCancelled then
      Stats.OptionsExtra := Format('CANCELLED after %s rows',
        [FormatFloat('#,##0', Exported)]);

    ReportForm := TfrmReport.Create(nil);
    try
      ReportForm.SetReportText(FormatTransferReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;

  finally
    Q.Free;
    if Trans.InTransaction then Trans.Rollback;
    DB.Connected := False;
    DB.Free;
    Trans.Free;
    ProgressForm.Free;
  end;
end;  }

procedure TfrmCSVBulkExporter.DoBulkExport(const ASQL: string);
var
  TotalRows, ExpectedRows: Int64;
  Exported: Int64;
  FromRow, ToRow: Integer;
  UseRange: Boolean;
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
  LenLine, LenEnd: Integer;
  SQL, BaseSQL, CountSQL: string;
  FileStream: TBufferedFileStream;
  BufferBytes: Int64;
  RowsPerUpdate: Integer;
  RowsSinceUpdate: Int64;
  FileSize: Int64;
  Stats: TTransferStatistic;
  ReportForm: TfrmReport;
  i, CheckedCount: Integer;
  HeaderLine: string;

  procedure UpdateProgress;
  var
    ElapsedSec: Double;
    RowsPerSec: Double;
  begin
    if ExpectedRows > 0 then
      ProgressBar.Position := Exported;

    ElapsedSec := (Now - StartTime) * 86400;
    if ElapsedSec > 0 then
      RowsPerSec := Exported / ElapsedSec
    else
      RowsPerSec := 0;

    if ExpectedRows > 0 then
      ProgressLabel.Caption := Format('Exported %s of %s rows',
        [FormatFloat('#,##0', Exported), FormatFloat('#,##0', ExpectedRows)])
    else
      ProgressLabel.Caption := Format('Exported %s rows',
        [FormatFloat('#,##0', Exported)]);

    LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
      '   |   ' + FormatFloat('#,##0', Round(RowsPerSec)) + ' rows/sec';

    Application.ProcessMessages;
  end;

begin
  // ============================================================
  // Guard: FDB muss verbunden sein (wird von LoadTableList aufgebaut)
  // ============================================================
  if not Assigned(FDB) or not FDB.Connected then
  begin
    // Fallback: über ConnectToDBAs verbinden (falls Combo-Change nicht lief)
    if (FSourceDBIndex < 0) or (not ConnectToDBAs(FSourceDBIndex)) then
    begin
      ShowMessage('Source database not connected.');
      Exit;
    end;
    LoadTableList;
    if not Assigned(FDB) or not FDB.Connected then
    begin
      ShowMessage('Source database not connected.');
      Exit;
    end;
  end;

  UseRange := rbRange.Checked;
  FromRow := GetFromRow;
  ToRow := GetToRow;

  // ------------------------------------------------------------------
  // Buffer-Größe
  // ------------------------------------------------------------------
  BufferBytes := StrToIntDef(edtByteBuffer.Text, 1024 * 1024 * 64);
  if BufferBytes < 64 * 1024 then BufferBytes := 64 * 1024;
  if BufferBytes > 64 * 1024 * 1024 then BufferBytes := 64 * 1024 * 1024;

  // ------------------------------------------------------------------
  // Zeilen pro GUI-Update
  // ------------------------------------------------------------------
  RowsPerUpdate := StrToIntDef(edtLineBuffer.Text, 10000);
  if RowsPerUpdate < 1 then
    RowsPerUpdate := 10000;

  // ------------------------------------------------------------------
  // Statistik
  // ------------------------------------------------------------------
  FillChar(Stats, SizeOf(Stats), 0);
  Stats.Kind             := tkExport;
  Stats.SourceKind       := 'Firebird Table';
  Stats.SourceServer     := comboxSourceServer.Text;
  Stats.SourceDatabase   := comboxSourceDB.Text;
  Stats.SourceTable      := comboxSourceTables.Text;
  Stats.DestKind         := 'File';
  Stats.DestFileName     := edtExportFileName.Text;
  Stats.BatchSize        := GetBatchSize;
  Stats.FormulaUsed      := True;
  Stats.FromRow          := FromRow;
  Stats.ToRow            := ToRow;
  Stats.UseRowRange      := UseRange;

  if (FSourceDBIndex >= 0) and (FSourceDBIndex < Length(RegisteredDatabases)) then
  begin
    Stats.SourceServerVersion := RegisteredDatabases[FSourceDBIndex].RegRec.ServerVersionString;
    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;
  end;

  Stats.SystemInfo := GetSystemInfo(edtExportFileName.Text);

  CheckedCount := 0;
  for i := 0 to High(FFields) do
    if chkLstFields.Checked[i] then Inc(CheckedCount);
  Stats.FieldsCount := CheckedCount;

  Stats.FormulasApplied := '';
  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFields.RowCount then Break;
    if not chkLstFields.Checked[i] then Continue;
    if Trim(sgFields.Cells[2, i + 1]) <> '' then
      Stats.FormulasApplied := Stats.FormulasApplied +
        '  • ' + FFields[i].FieldName + ' = ' + sgFields.Cells[2, i + 1] + sLineBreak;
  end;

  // ============================================================
  // Verbindung wiederverwenden (kein neuer Connect!)
  // Neue Transaktion für sauberen Isolation-Snapshot
  // ============================================================
  DB := FDB;
  Trans := TIBTransaction.Create(nil);
  Trans.DefaultDatabase := DB;
  if Assigned(FTrans) then
    Trans.Params.Assign(FTrans.Params);
  Trans.StartTransaction;

  Q := TIBSQL.Create(nil);
  Q.Database := DB;
  Q.Transaction := Trans;

  BaseSQL := Trim(ASQL);
  if (Length(BaseSQL) >= 6) and (UpperCase(Copy(BaseSQL, 1, 6)) = 'SELECT') then
    BaseSQL := Trim(Copy(BaseSQL, 7, MaxInt));

  LineEnd := sLineBreak;
  HeaderLine := BuildHeaderLine;

  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.Width := 520;
    ProgressForm.Height := 220;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;
    ProgressForm.Caption := 'CSV Bulk Exporter';

    LblPhase := TLabel.Create(ProgressForm);
    LblPhase.Parent := ProgressForm;
    LblPhase.Left := 16;
    LblPhase.Top := 16;
    LblPhase.Caption := 'Counting rows...';
    LblPhase.Font.Style := [fsBold];
    LblPhase.Width := 480;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 42;
    ProgressLabel.Caption := 'Please wait...';
    ProgressLabel.Width := 480;

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 70;
    ProgressBar.Width := 480;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;
    ProgressBar.Style := pbstMarquee;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 100;
    LblElapsed.Caption := 'Elapsed: 00:00:00';
    LblElapsed.Width := 480;

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 200;
    BtnCancel.Top := 140;
    BtnCancel.Width := 100;
    BtnCancel.OnClick := @CancelClick;

    ProgressForm.Show;
    ProgressForm.BringToFront;
    Application.ProcessMessages;
    Sleep(50);
    Application.ProcessMessages;

    FCancelled := False;
    Exported := 0;
    RowsSinceUpdate := 0;

    // ============================================================
    // Phase 1: Zeilen zählen
    // ============================================================
    LblPhase.Caption := 'Counting rows...';
    Application.ProcessMessages;

    TotalRows := 0;
    try
      CountSQL := 'SELECT COUNT(*) FROM (SELECT ' + BaseSQL + ') AS cnt_qry';
      Q.SQL.Text := CountSQL;
      Q.ExecQuery;
      if not Q.EOF then
        TotalRows := Q.Fields[0].AsInt64;
      Q.Close;
    except
      TotalRows := 0;
    end;

    if UseRange then
    begin
      if ToRow > TotalRows then ToRow := TotalRows;
      if FromRow > TotalRows then FromRow := TotalRows;
      ExpectedRows := ToRow - FromRow + 1;
      if ExpectedRows < 0 then ExpectedRows := 0;
    end
    else
    begin
      ToRow := TotalRows;
      ExpectedRows := TotalRows;
    end;

    if ExpectedRows > 0 then
    begin
      ProgressBar.Style := pbstNormal;
      ProgressBar.Max := ExpectedRows;
      ProgressBar.Position := 0;
    end;

    // ============================================================
    // Phase 2: Datei öffnen + Header
    // ============================================================
    LblPhase.Caption := 'Opening output file...';
    Application.ProcessMessages;

    FileStream := TBufferedFileStream.Create(edtExportFileName.Text, fmCreate, BufferBytes);

    if HeaderLine <> '' then
      FileStream.Write(HeaderLine[1], Length(HeaderLine));

    // ============================================================
    // Phase 3: Export
    // ============================================================
    StartTime := Now;
    LblPhase.Caption := 'Exporting data...';
    Application.ProcessMessages;

    if UseRange and (FromRow > 1) then
      SQL := 'SELECT SKIP ' + IntToStr(FromRow - 1) +
             ' FIRST ' + IntToStr(ExpectedRows) + ' ' + BaseSQL
    else if UseRange then
      SQL := 'SELECT FIRST ' + IntToStr(ExpectedRows) + ' ' + BaseSQL
    else
      SQL := 'SELECT ' + BaseSQL;

    try
      try
        Q.Close;
        Q.SQL.Text := SQL;
        Q.ExecQuery;

        while not Q.EOF do
        begin
          if FCancelled then Break;

          Line := Q.Fields[0].AsString;
          LenLine := Length(Line);
          LenEnd := Length(LineEnd);

          if LenLine > 0 then
          begin
            try
              FileStream.Write(Line[1], LenLine);
            except
              on E: Exception do
              begin
                raise Exception.Create('Write failed: ' + E.Message);
              end;
            end;
          end;

          Inc(Exported);
          Inc(RowsSinceUpdate);

          if RowsSinceUpdate >= RowsPerUpdate then
          begin
            RowsSinceUpdate := 0;
            UpdateProgress;
          end;

          Q.Next;
        end;
      except
        on E: Exception do
        begin
          FileStream.Flush;
          FileStream.Free;
          FileStream := nil;
          if FileExists(edtExportFileName.Text) then
            DeleteFile(edtExportFileName.Text);
          MessageDlg('Export failed:' + sLineBreak + sLineBreak + E.Message,
                     mtError, [mbOK], 0);
          Exit;
        end;
      end;

    finally
      if Assigned(FileStream) then
      begin
        FileStream.Flush;
        FileStream.Free;
      end;
    end;

    // ============================================================
    // Phase 4: Finalisieren
    // ============================================================
    LblPhase.Caption := 'Finalizing...';
    UpdateProgress;
    EndTime := Now;

    Stats.RowsProcessed  := Exported;
    Stats.ElapsedSeconds := (EndTime - StartTime) * SecsPerDay;

    FileSize := 0;
    if FileExists(edtExportFileName.Text) then
    begin
      try
        with TFileStream.Create(edtExportFileName.Text, fmOpenRead or fmShareDenyNone) do
        try
          FileSize := Size;
        finally
          Free;
        end;
      except
        FileSize := 0;
      end;
    end;
    Stats.DestFileSize := FileSize;

    if FCancelled then
      Stats.OptionsExtra := Format('CANCELLED after %s rows',
        [FormatFloat('#,##0', Exported)]);

    ReportForm := TfrmReport.Create(nil);
    try
      ReportForm.SetReportText(FormatTransferReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;

  finally
    ProgressForm.Free;
    Q.Free;
    if Trans.InTransaction then Trans.Rollback;
    Trans.Free;
    // FDB bleibt verbunden — wird von LoadTableList weiterverwaltet
  end;
end;


end.
