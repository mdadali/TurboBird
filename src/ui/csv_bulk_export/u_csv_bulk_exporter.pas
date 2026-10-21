unit u_csv_bulk_exporter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Math, DateUtils, Dialogs,
  Graphics, StdCtrls, ExtCtrls,
  SynEdit, SynHighlighterSQL, Grids, CheckLst, ComCtrls, DB, BufStream,
  IBDatabase, IBQuery, IBSQL, IBXScript, IBDataOutput, ATSynEdit,

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
    btnEditCurrentPreset: TButton;
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
    SynSQLSyn1: TSynSQLSyn;

    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnEditCurrentPresetClick(Sender: TObject);
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
    function EditCurrentPreset: Boolean;
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
  AliasList: TStringList;
begin
  // ComboBox mit ALIASEN füllen (konsistent zu TreeView und
  // allen anderen Masken).
  AliasList := GetServerAliasListFromTreeView;
  if AliasList = nil then Exit;
  try
    comboxSourceServer.Items.Assign(AliasList);
  finally
    AliasList.Free;
  end;
end;

procedure TfrmCSVBulkExporter.LoadServerList;
begin
  ApplyInitialSelection;
end;

{ ============================================================
  ApplyInitialSelection: Setzt die Vorauswahl in den ComboBoxen
  basierend auf FInitialDBIndex.

  NEU:
    - Server-Auswahl über den ALIAS des Servers (nicht mehr
      über Rec.ServerName, das bei Legacy-Records der echte
      Name ist und nicht in der Alias-ComboBox erscheint).
    - Bei Legacy-Records (Rec.ServerName = echter Name, kein
      Alias im Record) wird der Alias aus dem Server-Record
      geholt (GetServerRecordFromFileByAlias).
  ============================================================ }
procedure TfrmCSVBulkExporter.ApplyInitialSelection;
var
  ServerRec: TServerRecord;
  DBTitle, DisplayAlias: string;
  Idx: Integer;
begin
  FUpdatingCombos := True;
  try
    LoadServerListFillOnly;

    if (FInitialDBIndex >= 0) and (FInitialDBIndex < Length(RegisteredDatabases)) then
    begin
      // Alias ermitteln (Fallback: echter Name bei Legacy-Records)
      ServerRec := GetServerRecordFromFileByAlias(
                     RegisteredDatabases[FInitialDBIndex].RegRec.ServerName);
      if Trim(ServerRec.ServerAlias) <> '' then
        DisplayAlias := Trim(ServerRec.ServerAlias)
      else
        DisplayAlias := Trim(ServerRec.ServerName);

      Idx := comboxSourceServer.Items.IndexOf(DisplayAlias);
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
  HasErr, HasWarn: Boolean;
  i: Integer;
begin
  Preset := GetActivePreset;
  if Preset = nil then
  begin
    btnPreviewSQL.Enabled := False;
    btnExecute.Enabled := False;
    Exit;
  end;

  Issues := TPresetValidator.Validate(Preset);

  HasErr := False;
  HasWarn := False;
  for i := 0 to High(Issues) do
  begin
    if Issues[i].Severity = isError then HasErr := True
    else if Issues[i].Severity = isWarning then HasWarn := True;
  end;

  // ============================================================
  // Fehler → Preset blockieren
  // ============================================================
  if HasErr then
  begin
    MessageDlg('Preset has errors:' + sLineBreak + sLineBreak +
               TPresetValidator.BuildMessage(Issues),
               mtError, [mbOK], 0);
    btnPreviewSQL.Enabled := False;
    btnExecute.Enabled := False;
    Exit;
  end;

  // ============================================================
  // Warnungen → anzeigen, aber weitermachen
  // ============================================================
  if HasWarn then
  begin
    MessageDlg('Preset has warnings:' + sLineBreak + sLineBreak +
               TPresetValidator.BuildMessage(Issues),
               mtWarning, [mbOK], 0);
  end;

  // ============================================================
  // Freigeben
  // ============================================================
  btnPreviewSQL.Enabled := True;
  if syneditGenerateQuery.Text = '' then
    btnExecute.Enabled := False;
end;

// ============================================================================
// SOURCE-KASKADE
// ============================================================================

procedure TfrmCSVBulkExporter.comboxSourceServerChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  LoadDBList;
end;

{ ============================================================
  LoadDBList: Füllt die DB-ComboBox basierend auf dem
  gewählten Server.

  NEU:
    - GetDBTitlesForServer (aus turbocommon.pas) liefert nur
      die DB-Titel, die zum Anzeige-Alias des Servers gehören.
      Dadurch gehören DBs unter 'Prod' nicht zu 'Test', auch
      wenn beide auf denselben echten Server zeigen.
  ============================================================ }
procedure TfrmCSVBulkExporter.LoadDBList;
begin
  GetDBTitlesForServer(comboxSourceServer.Text, comboxSourceDB.Items);

  if comboxSourceDB.Items.Count > 0 then
  begin
    comboxSourceDB.ItemIndex := 0;
    comboxSourceDBChange(nil);
  end;
end;

{ ============================================================
  comboxSourceDBChange: Reagiert auf DB-Wechsel in der
  Source-ComboBox.

  NEU:
    - FSourceDBIndex wird über FindDBIndexByTitleAndServer
      aufgelöst (Alias↔echter-Name-Auflösung).
    - Der manuelle Loop über RegisteredDatabases entfällt.
  ============================================================ }
procedure TfrmCSVBulkExporter.comboxSourceDBChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;

  // Alias-Lookup mit Fallback auf echten Namen
  FSourceDBIndex := FindDBIndexByTitleAndServer(
                      Trim(comboxSourceDB.Text),
                      Trim(comboxSourceServer.Text));

  if FSourceDBIndex < 0 then Exit;

  // Wenn die geteilte DB nicht verbunden ist → Login-Flow auslösen
  // (zeigt Dialog, füllt Cache, setzt IBDatabase.Params)
  if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
     (not RegisteredDatabases[FSourceDBIndex].IBDatabase.Connected) then
    ConnectToDBAs(FSourceDBIndex);

  // Form-eigene DB aufbauen + Tabellenliste laden
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
  TypeStr, ArraySuffix: string;
  IsComputed: Boolean;
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

    IsComputed := Trim(RawFields[i].ComputedSource) <> '';

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

    // Array-Suffix anhängen — wichtig für IsProblemField-Erkennung
    ArraySuffix := ArrayDimsToSuffix(RawFields[i].ArrayDims);
    if ArraySuffix <> '' then
      TypeStr := TypeStr + ArraySuffix;

    if IsComputed then
      TypeStr := TypeStr + ' (computed)';

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

    if IsComputed then
      sgFields.Cells[2, i + 1] := '(computed)'
    else
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
  QuotedFieldName: string;
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

    // Case-sensitive Identifier: quoten wenn nicht komplett UPPERCASE
    QuotedFieldName := MakeCaseSensitiveAuto(FFields[i].FieldName);

    // Formel aus Grid (oder $1)
    Formula := Trim(sgFields.Cells[2, i + 1]);
    if Formula = '' then
      Formula := '$1';

    // $1 durch den gequoteten Feldnamen ersetzen
    Formula := StringReplace(Formula, '$1', QuotedFieldName, [rfReplaceAll]);
    FieldExpr := '(' + Formula + ')';

    IsBool := IsBooleanType(FFields[i].FieldType);
    IsNum  := not IsBool and not NeedsQuoting(FFields[i].FieldType);
    IsStr  := not IsBool and not IsNum;

    // Binary BLOB erkennen
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

  // SELECT mehrzeilig — Tabellenname mit Auto-Quoting (case-sensitive)
  Result := 'SELECT' + sLineBreak +
            LineExpr + sLineBreak +
            '  AS csv_line' + sLineBreak +
            'FROM ' + MakeCaseSensitiveAuto(Trim(comboxSourceTables.Text));
end;

function TfrmCSVBulkExporter.IsBlobBinaryType(const AFieldType: string): Boolean;
var
  CT: string;
begin
  CT := UpperCase(AFieldType);

  // Kein BLOB → sofort raus
  if Pos('BLOB', CT) = 0 then
    Exit(False);

  // Explizit BINARY oder SUBTYPE 0 → binär
  if (Pos('BINARY', CT) > 0) or (Pos('SUBTYPE 0', CT) > 0) or
     (Pos('SUB_TYPE 0', CT) > 0) then
    Exit(True);

  // Explizit TEXT oder SUBTYPE 1 → text
  if (Pos('TEXT', CT) > 0) or (Pos('SUBTYPE 1', CT) > 0) or
     (Pos('SUB_TYPE 1', CT) > 0) then
    Exit(False);

  // Reines "BLOB" ohne Subtyp → Firebird-Default ist BINARY
  Result := True;
end;

function TfrmCSVBulkExporter.BuildHeaderLine: string;
var
  i: Integer;
  Preset: TFormulaPreset;
  Sep, QC, Line, LE: string;
begin
  Result := '';
  Preset := GetActivePreset;
  if Preset = nil then Exit;
  if not Preset.IncludeHeader then Exit;

  Sep := ExpandSep(Preset.Separator);
  QC  := Preset.QuoteChar;

  // LineEnding aus Preset, Fallback auf globale DataEditor-Einstellung
  LE := Preset.LineEnding;
  if LE = '' then
    LE := CSVLineEnding;

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
    Result := Line + LE;
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
  Preset: TFormulaPreset;
  PresetPath: string;
  PresetSL: TStringList;

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
  // Guard: FDB muss verbunden sein
  // ============================================================
  if not Assigned(FDB) or not FDB.Connected then
  begin
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

  // ============================================================
  // Preset holen — LineEnding wird von hier gezogen
  // ============================================================
  Preset := GetActivePreset;
  if Preset = nil then
  begin
    ShowMessage('No formula preset selected.');
    Exit;
  end;

  UseRange := rbRange.Checked;
  FromRow := GetFromRow;
  ToRow := GetToRow;

  BufferBytes := StrToIntDef(edtByteBuffer.Text, 1024 * 1024 * 64);
  if BufferBytes < 64 * 1024 then BufferBytes := 64 * 1024;
  if BufferBytes > 64 * 1024 * 1024 then BufferBytes := 64 * 1024 * 1024;

  RowsPerUpdate := StrToIntDef(edtLineBuffer.Text, 10000);
  if RowsPerUpdate < 1 then
    RowsPerUpdate := 10000;

  // ============================================================
  // Statistik — Basis
  // WICHTIG: FillChar MUSS vor allen Feldzuweisungen stehen,
  //          sonst werden sie wieder auf 0 gesetzt.
  // ============================================================
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

  // ============================================================
  // Preset-Info — NACH FillChar!
  // ============================================================
  Stats.PresetName := Preset.Name;
  Stats.PresetFileName := Preset.FileName;

  PresetPath := FormulaPresetManager.PresetDir + Preset.FileName;
  if FileExists(PresetPath) then
  begin
    PresetSL := TStringList.Create;
    try
      PresetSL.LoadFromFile(PresetPath);
      Stats.PresetFileContent := PresetSL.Text;
    finally
      PresetSL.Free;
    end;
  end;

  if Trim(Preset.Description) <> '' then
    Stats.OptionsExtra := 'Preset: ' + Preset.Name + ' — ' + Preset.Description
  else
    Stats.OptionsExtra := 'Preset: ' + Preset.Name;

  // ============================================================
  // Source-Server-Version + Client-Lib
  // ============================================================
  if (FSourceDBIndex >= 0) and (FSourceDBIndex < Length(RegisteredDatabases)) then
  begin
    Stats.SourceServerVersion := RegisteredDatabases[FSourceDBIndex].RegRec.ServerVersionString;
    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;
  end;

  Stats.SystemInfo := GetSystemInfo(edtExportFileName.Text);

  // ============================================================
  // Feld-Zähler + angewendete Formeln
  // ============================================================
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
  // Verbindung wiederverwenden
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

  // ============================================================
  // LineEnding aus Preset (Fallback: globale Einstellung)
  // ============================================================
  LineEnd := Preset.LineEnding;
  if LineEnd = '' then
    LineEnd := CSVLineEnding;

  HeaderLine := BuildHeaderLine;

  // ============================================================
  // Progress-Fenster
  // ============================================================
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

    Stats.FromRow     := FromRow;
    Stats.ToRow       := ToRow;
    Stats.UseRowRange := UseRange;

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

          // Zeileninhalt schreiben
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

          // Zeilenumbruch schreiben
          if LenEnd > 0 then
            FileStream.Write(LineEnd[1], LenEnd);

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
      Stats.OptionsExtra := Stats.OptionsExtra + sLineBreak +
        Format('  CANCELLED after %s rows',
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
  end;
end;

procedure TfrmCSVBulkExporter.btnEditCurrentPresetClick(Sender: TObject);
var
  CurrentName: string;
  Idx: Integer;
begin
  CurrentName := cbFormulaPreset.Text;

  if not EditCurrentPreset then
    Exit;   // User hat abgebrochen oder Speichern fehlgeschlagen

  // Presets neu laden
  FormulaPresetManager.Reload;
  LoadFormulaPresets;

  // Selbes Preset wieder auswählen
  if CurrentName <> '' then
  begin
    Idx := cbFormulaPreset.Items.IndexOf(CurrentName);
    if Idx >= 0 then
      cbFormulaPreset.ItemIndex := Idx;
  end;

  // Grid + Preview aktualisieren
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

function TfrmCSVBulkExporter.EditCurrentPreset: Boolean;
var
  Preset: TFormulaPreset;
  PresetPath: string;
  Dlg: TForm;
  Memo: TMemo;
  LblInfo: TLabel;
  BtnSave, BtnCancel: TButton;
  SL: TStringList;
begin
  Result := False;

  Preset := GetActivePreset;
  if Preset = nil then
  begin
    ShowMessage('No preset selected.');
    Exit;
  end;

  PresetPath := FormulaPresetManager.PresetDir + Preset.FileName;

  if not FileExists(PresetPath) then
  begin
    ShowMessage('Preset file not found:' + sLineBreak + PresetPath);
    Exit;
  end;

  Dlg := TForm.Create(nil);
  try
    Dlg.Caption := 'Edit Preset: ' + Preset.Name;
    Dlg.Width := 720;
    Dlg.Height := 600;
    Dlg.Position := poScreenCenter;
    Dlg.BorderStyle := bsSizeable;
    Dlg.FormStyle := fsStayOnTop;

    // ============================================================
    // Info-Label mit Pfad
    // ============================================================
    LblInfo := TLabel.Create(Dlg);
    LblInfo.Parent := Dlg;
    LblInfo.Left := 12;
    LblInfo.Top := 8;
    LblInfo.Width := Dlg.ClientWidth - 24;
    LblInfo.Anchors := [akLeft, akTop, akRight];
    LblInfo.Caption := 'File: ' + PresetPath;
    LblInfo.Font.Style := [fsBold];

    // ============================================================
    // Memo mit Preset-Inhalt
    // ============================================================
    Memo := TMemo.Create(Dlg);
    Memo.Parent := Dlg;
    Memo.Left := 12;
    Memo.Top := 32;
    Memo.Width := Dlg.ClientWidth - 24;
    Memo.Height := Dlg.ClientHeight - 100;
    Memo.Anchors := [akLeft, akTop, akRight, akBottom];
    Memo.ScrollBars := ssBoth;
    Memo.Font.Name := 'Courier New';
    Memo.Font.Size := 10;
    Memo.WordWrap := False;

    SL := TStringList.Create;
    try
      SL.LoadFromFile(PresetPath);
      Memo.Lines.Assign(SL);
    finally
      SL.Free;
    end;

    // ============================================================
    // Buttons — Save + Cancel
    // ============================================================
    BtnSave := TButton.Create(Dlg);
    BtnSave.Parent := Dlg;
    BtnSave.Caption := 'Save';
    BtnSave.Width := 100;
    BtnSave.Anchors := [akRight, akBottom];
    BtnSave.Left := Dlg.ClientWidth - 224;
    BtnSave.Top := Dlg.ClientHeight - 52;
    BtnSave.ModalResult := mrOk;
    BtnSave.Default := True;

    BtnCancel := TButton.Create(Dlg);
    BtnCancel.Parent := Dlg;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Width := 100;
    BtnCancel.Anchors := [akRight, akBottom];
    BtnCancel.Left := Dlg.ClientWidth - 112;
    BtnCancel.Top := Dlg.ClientHeight - 52;
    BtnCancel.ModalResult := mrCancel;
    BtnCancel.Cancel := True;

    // ============================================================
    // Anzeigen + Speichern
    // ============================================================
    if Dlg.ShowModal = mrOk then
    begin
      try
        SL := TStringList.Create;
        try
          SL.Assign(Memo.Lines);
          SL.SaveToFile(PresetPath);
        finally
          SL.Free;
        end;
        Result := True;
      except
        on E: Exception do
          ShowMessage('Failed to save preset:' + sLineBreak + sLineBreak + E.Message);
      end;
    end;

  finally
    Dlg.Free;
  end;
end;

end.
