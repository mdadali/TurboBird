unit clone_table_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, CheckLst, Menus, IB, IBQuery, IBDatabase, IBDatabaseInfo,
  IBExtract, ibxscript,

  turbocommon,

  uCopyTableDataLocal,
  uCopyTableDataCrossExecuteBlock,
  uCopyTableDataCrossRowByRow,
  uFormulaPresets,

  fmetaquerys,
  uthemeselector;


type

  TFieldInfo = record
    FieldName: string;
    FieldType: string;
    IsComputed: Boolean;
    Checked: Boolean;
    Formula: string;
  end;

  { TfrmCloneTable }

  TfrmCloneTable = class(TForm)
    btnCancel: TButton;
    btnDeselectAll: TButton;
    btnExecute: TButton;
    btnNewDB: TButton;
    btnOpenExternalFile: TButton;
    btnPreviewSQL: TButton;
    btnAddToQueue: TButton;
    btnRefreshPresets: TButton;
    btnExternalFile: TButton;
    btnSelectAll: TButton;
    btnGenTestFormulas: TButton;
    chkCopyData: TCheckBox;
    chkLstFields: TCheckListBox;
    chkUseFormula: TCheckBox;
    chkboxExternalTable: TCheckBox;
    chkCreateTable: TCheckBox;
    cbFormulaPreset: TComboBox;
    comboxDestDB: TComboBox;
    comboxSourceDB: TComboBox;
    comboxDestServer: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    Destination: TGroupBox;
    edtExternalFile: TEdit;
    edtDestTable: TEdit;
    edtBatchSize: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    grboxCopyOptions: TGroupBox;
    grBoxFields: TGroupBox;
    grBoxSource: TGroupBox;
    grBoxFormulaPresets: TGroupBox;
    grBoxCopyMethod: TGroupBox;
    IBDBDest: TIBDatabase;
    IBDBSource: TIBDatabase;
    IBQueryDest: TIBQuery;
    IBQuerySource: TIBQuery;
    IBTransDest: TIBTransaction;
    IBTransSource: TIBTransaction;
    IBXScript1: TIBXScript;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbSourceTable: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlFields: TPanel;
    pnlTop: TPanel;
    PopupMenu1: TPopupMenu;
    rbRowByRow: TRadioButton;
    rbExecuteBlock: TRadioButton;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
    sgFields: TStringGrid;
    StatusBar1: TStatusBar;
    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnExternalFileClick(Sender: TObject);
    procedure btnGenTestFormulasClick(Sender: TObject);
    procedure btnRefreshPresetsClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnPreviewSQLClick(Sender: TObject);
    procedure cbFormulaPresetChange(Sender: TObject);
    procedure chkboxExternalTableChange(Sender: TObject);
    procedure chkCreateTableChange(Sender: TObject);
    procedure chkUseFormulaChange(Sender: TObject);
    procedure comboxDestDBChange(Sender: TObject);
    procedure comboxDestServerChange(Sender: TObject);
    procedure comboxSourceDBChange(Sender: TObject);
    procedure comboxSourceServerChange(Sender: TObject);
    procedure comboxSourceTablesChange(Sender: TObject);
    procedure DestinationClick(Sender: TObject);
    procedure edtDestTableChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grBoxFieldsDblClick(Sender: TObject);
    procedure grBoxFormulaPresetsClick(Sender: TObject);
    procedure grBoxSourceClick(Sender: TObject);
    procedure IBXScript1GetParamValue(Sender: TObject; ParamName: string;
      var BlobID: TISC_QUAD);
    procedure Label5Click(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FSourceDBIndex: Integer;
    FDestDBIndex: Integer;
    FFields: array of TFieldInfo;

    function  FillSourceServerCombo: boolean;
    function  FillSourceDBCombo: boolean;
    function  FillSourceTableCombo: boolean;
    procedure FillSourceCombos;

    function  FillDestServerCombo: boolean;
    function  FillDestDBCombo: boolean;
    procedure FillDestCombos;

    function ConfigureSourceConnection: boolean;
    function ConfigureDestConnection: boolean;

    procedure LoadFields;
    function GetFieldTransforms: TFieldTransformArray;
    function GetProblemFieldTransforms: TFieldTransformArray;
    function GenerateCreateTableSQL: string;
    function GenerateInsertSQL: string;
    function GenerateCreateExternalTableSQL: string;
    function CreateExternalDestTable(DestDB: TIBDatabase;
                                     DestTrans: TIBTransaction; TableName: string): Boolean;

    function TableExists(DB: TIBDatabase; TableName: string): Boolean;
    function CreateDestTable(DestDB: TIBDatabase; DestTrans: TIBTransaction; TableName: string): Boolean;
  public
    procedure Init(ANodeInfos: TPNodeInfos);
    procedure LoadFormulaPresets;
    procedure UpdateCopyMethodAvailability;
  end;

var
  frmCloneTable: TfrmCloneTable;

implementation

{$R *.lfm}

{ TfrmCloneTable }
procedure TfrmCloneTable.UpdateCopyMethodAvailability;
var
  SameServer, SameDB: Boolean;
begin
  // Prüfen ob Quelle und Ziel auf demselben Server/DB sind
  SameServer := SameText(comboxSourceServer.Text, comboxDestServer.Text);
  SameDB := SameText(comboxSourceDB.Text, comboxDestDB.Text) and SameServer;

  if SameDB then
  begin
    // Gleiche DB → Keine Cross-Methoden nötig
    grBoxCopyMethod.Enabled := False;
    rbExecuteBlock.Checked := False;
    rbRowByRow.Checked := False;
    StatusBar1.SimpleText := 'Same database: INSERT...SELECT will be used (fastest).';
  end
  else
  begin
    // Unterschiedliche DBs → Cross-Methoden verfügbar
    grBoxCopyMethod.Enabled := True;
    if not rbExecuteBlock.Checked and not rbRowByRow.Checked then
      rbExecuteBlock.Checked := True;  // Default: Automatic
    StatusBar1.SimpleText := 'Cross-database: Select copy method.';
  end;
end;

procedure TfrmCloneTable.FormCreate(Sender: TObject);
begin
  edtBatchSize.Text := IntToStr(DefaultBatchSize);

  // StringGrid initialisieren
  sgFields.RowCount := 1;
  sgFields.ColCount := 4;
  sgFields.Cells[0, 0] := 'Copy';
  sgFields.Cells[1, 0] := 'Source Field';
  sgFields.Cells[2, 0] := 'Field Type';
  sgFields.Cells[3, 0] := 'Formula ($1 = value)';
  //sgFields.ColWidths[0] := 45;
  sgFields.ColWidths[0] := 0; //invisible
  sgFields.ColWidths[1] := 150;
  sgFields.ColWidths[2] := 120;
  sgFields.ColWidths[3] := 250;

  comboxSourceServer.OnChange := nil;
  FillSourceCombos;
  FillDestCombos;

  comboxSourceServer.OnChange := @comboxSourceServerChange;

  LoadFormulaPresets;
  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.Init(ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
end;

// ============================================================================
// SOURCE
// ============================================================================

procedure TfrmCloneTable.comboxSourceServerChange(Sender: TObject);
begin
  FillSourceDBCombo;
  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.comboxSourceDBChange(Sender: TObject);
begin
  comboxSourceTables.Items.Clear;
  if ConfigureSourceConnection then
  begin
    FillSourceTableCombo;
    comboxSourceTablesChange(nil);
    UpdateCopyMethodAvailability;
  end else
    grBoxCopyMethod.Enabled := false;
end;

procedure TfrmCloneTable.comboxSourceTablesChange(Sender: TObject);
begin
  edtDestTable.Text := Trim(comboxSourceTables.Text) + '_COPY';
  LoadFields;
end;

procedure TfrmCloneTable.DestinationClick(Sender: TObject);
begin

end;

procedure TfrmCloneTable.edtDestTableChange(Sender: TObject);
begin

end;

procedure TfrmCloneTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(IBQuerySource) and IBQuerySource.Active then
    IBQuerySource.Close;
  if Assigned(IBQueryDest) and IBQueryDest.Active then
    IBQueryDest.Close;

  if Assigned(IBTransSource) and IBTransSource.InTransaction then
    IBTransSource.Commit;
  if Assigned(IBTransDest) and IBTransDest.InTransaction then
    IBTransDest.Commit;

  // JETZT SICHER – das sind eigene Kopien!
  if Assigned(IBDBSource) and IBDBSource.Connected then
    IBDBSource.Connected := False;
  if Assigned(IBDBDest) and IBDBDest.Connected then
    IBDBDest.Connected := False;
end;

procedure TfrmCloneTable.FillSourceCombos;
begin
  FillSourceServerCombo;
  FillSourceDBCombo;
  FillSourceTableCombo;
end;

function TfrmCloneTable.FillSourceServerCombo: boolean;
var
  ServerList: TStringList;
begin
  Result := False;
  comboxSourceServer.Items.Clear;

  try
    ServerList := GetServerListFromTreeView;
    comboxSourceServer.Items.Assign(ServerList);
    if comboxSourceServer.Items.Count > 0 then
    begin
      comboxSourceServer.ItemIndex := 0;
      Result := True;
      FillSourceDBCombo;
    end;
  finally
    ServerList.Free;
  end;
end;

function TfrmCloneTable.FillSourceDBCombo: boolean;
var
  i: Integer;
begin
  Result := False;
  comboxSourceDB.Items.Clear;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxSourceServer.Text) then
      comboxSourceDB.Items.Add(RegisteredDatabases[i].RegRec.Title);

  if comboxSourceDB.Items.Count > 0 then
  begin
    comboxSourceDB.ItemIndex := 0;
    Result := True;
    if ConfigureSourceConnection then
      FillSourceTableCombo;
  end;
end;

function TfrmCloneTable.FillSourceTableCombo: boolean;
begin
  Result := False;
  comboxSourceTables.Items.Clear;

  try
    IBDBSource.GetTableNames(comboxSourceTables.Items);

    if comboxSourceTables.Items.Count > 0 then
    begin
      comboxSourceTables.ItemIndex := 0;
      edtDestTable.Text := Trim(comboxSourceTables.Text) + '_COPY';
      Result := True;
    end;
  except
  end;
end;

function TfrmCloneTable.ConfigureSourceConnection: boolean;
var
  i: Integer;
  DBRec: TDatabaseRec;
begin
  Result := False;
  FSourceDBIndex := -1;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxSourceServer.Text) and
       SameText(RegisteredDatabases[i].RegRec.Title, comboxSourceDB.Text) then
    begin
      FSourceDBIndex := i;
      Break;
    end;

  if FSourceDBIndex < 0 then Exit;

  try
    if IBDBSource.Connected then
      IBDBSource.Connected := False;

    DBRec := RegisteredDatabases[FSourceDBIndex];
    IBDBSource := DBRec.IBDatabase;
    IBTransSource := DBRec.IBTransaction;
    IBQuerySource := DBRec.IBQuery;

    IBDBSource.Connected := True;
    if not IBTransSource.InTransaction then
      IBTransSource.StartTransaction;

    Result := True;
  except
  end;
end;

// ============================================================================
// DESTINATION
// ============================================================================

procedure TfrmCloneTable.comboxDestServerChange(Sender: TObject);
begin
  FillDestDBCombo;
  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.comboxDestDBChange(Sender: TObject);
begin
  if ConfigureDestConnection then
    UpdateCopyMethodAvailability
  else
    grBoxCopyMethod.Enabled := false;
end;

procedure TfrmCloneTable.FillDestCombos;
begin
  FillDestServerCombo;
  FillDestDBCombo;
end;

function TfrmCloneTable.FillDestServerCombo: boolean;
var
  ServerList: TStringList;
begin
  Result := False;
  comboxDestServer.Items.Clear;

  try
    ServerList := GetServerListFromTreeView;
    comboxDestServer.Items.Assign(ServerList);
    if comboxDestServer.Items.Count > 0 then
    begin
      comboxDestServer.ItemIndex := 0;
      Result := True;
    end;
  finally
    ServerList.Free;
  end;
end;

function TfrmCloneTable.FillDestDBCombo: boolean;
var
  i: Integer;
begin
  Result := False;
  comboxDestDB.Items.Clear;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxDestServer.Text) then
      comboxDestDB.Items.Add(RegisteredDatabases[i].RegRec.Title);

  if comboxDestDB.Items.Count > 0 then
  begin
    comboxDestDB.ItemIndex := 0;
    Result := True;
  end;
end;

function TfrmCloneTable.ConfigureDestConnection: boolean;
var
  i: Integer;
  DBRec: TDatabaseRec;
begin
  Result := False;
  FDestDBIndex := -1;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxDestServer.Text) and
       SameText(RegisteredDatabases[i].RegRec.Title, comboxDestDB.Text) then
    begin
      FDestDBIndex := i;
      Break;
    end;

  if FDestDBIndex < 0 then Exit;

  try
    if IBDBDest.Connected then
      IBDBDest.Connected := False;

    DBRec := RegisteredDatabases[FDestDBIndex];
    IBDBDest := DBRec.IBDatabase;
    IBTransDest := DBRec.IBTransaction;
    IBQueryDest := DBRec.IBQuery;

    IBDBDest.Connected := True;
    if not IBTransDest.InTransaction then
      IBTransDest.StartTransaction;

    IBXScript1.Database := IBDBDest;
    IBXScript1.Transaction := IBTransDest;

    Result := True;
  except
  end;
end;

// ============================================================================
// FIELDER
// ============================================================================
procedure TfrmCloneTable.LoadFields;
var
  Iso: TIsolatedQuery;
  i: Integer;
  FieldName, FieldType, ComputedSource: string;
  FSize: Integer;
begin
  if FSourceDBIndex < 0 then Exit;

  SetLength(FFields, 0);
  chkLstFields.Clear;
  sgFields.RowCount := 1;

  Iso := GetFieldsIsolated(RegisteredDatabases[FSourceDBIndex].IBDatabase, Trim(comboxSourceTables.Text));
  try
    while not Iso.Query.EOF do
    begin
      FieldName := Trim(Iso.Query.FieldByName('field_name').AsString);
      GetFieldType(Iso.Query, FieldType, FSize);
      ComputedSource := Trim(Iso.Query.FieldByName('computed_source').AsString);

      i := Length(FFields);
      SetLength(FFields, i + 1);
      FFields[i].FieldName := FieldName;
      FFields[i].FieldType := FieldType;
      FFields[i].IsComputed := (ComputedSource <> '');
      FFields[i].Checked := True;
      FFields[i].Formula := '';

      chkLstFields.Items.Add(FieldName);
      chkLstFields.Checked[i] := True;

      sgFields.RowCount := i + 2;
      sgFields.Cells[0, i + 1] := '1';
      sgFields.Cells[1, i + 1] := FieldName;
      sgFields.Cells[2, i + 1] := FieldType;
      sgFields.Cells[3, i + 1] := '';

      if FFields[i].IsComputed then
        sgFields.Cells[3, i + 1] := '(computed)';

      Iso.Query.Next;
    end;
  finally
    Iso.Free;
  end;

  //btnGenTestFormulasClick(nil);
end;

procedure TfrmCloneTable.btnSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
  begin
    chkLstFields.Checked[i] := True;
    sgFields.Cells[0, i + 1] := '1';
  end;
end;

procedure TfrmCloneTable.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
  begin
    chkLstFields.Checked[i] := False;
    sgFields.Cells[0, i + 1] := '0';
  end;
end;

procedure TfrmCloneTable.sgFieldsDblClick(Sender: TObject);
var
  Row: Integer;
  NewFormula: string;
begin
  Row := sgFields.Row;
  if (Row < 1) or (Row >= sgFields.RowCount) then Exit;

  NewFormula := sgFields.Cells[3, Row];
  if InputQuery('Formula for ' + sgFields.Cells[1, Row],
                'Enter SQL expression ($1 = field value):', NewFormula) then
  begin
    sgFields.Cells[3, Row] := NewFormula;
  end;
end;

{function TfrmCloneTable.GetFieldTransforms: TFieldTransformArray;
var
  i: Integer;
begin
  SetLength(Result, chkLstFields.Count);
  for i := 0 to chkLstFields.Count - 1 do
  begin
    Result[i].SourceField := chkLstFields.Items[i];
    Result[i].DestField := chkLstFields.Items[i];
    Result[i].DestFieldType := FFields[i].FieldType;  // ← DAS FEHLT!

    if FFields[i].IsComputed then
    begin
      Result[i].Formula := '';
      Result[i].CopyField := False;
    end
    else
    begin
      Result[i].Formula := sgFields.Cells[3, i + 1];
      Result[i].CopyField := chkLstFields.Checked[i];
    end;
  end;
end;}

function TfrmCloneTable.GetFieldTransforms: TFieldTransformArray;
var
  i: Integer;
begin
  SetLength(Result, chkLstFields.Count);
  for i := 0 to chkLstFields.Count - 1 do
  begin
    Result[i].SourceField := chkLstFields.Items[i];
    Result[i].DestField := chkLstFields.Items[i];
    Result[i].DestFieldType := FFields[i].FieldType;

    if FFields[i].IsComputed then
    begin
      Result[i].Formula := '';
      Result[i].CopyField := False;
    end
    else
    begin
      // Nur wenn Checkbox aktiv ist, die Formel aus dem Grid übernehmen
      if chkUseFormula.Checked then
        Result[i].Formula := sgFields.Cells[3, i + 1]
      else
        Result[i].Formula := '';
      Result[i].CopyField := chkLstFields.Checked[i];
    end;
  end;
end;

function TfrmCloneTable.GetProblemFieldTransforms: TFieldTransformArray;
var
  i, idx: Integer;
begin
  SetLength(Result, 0);
  idx := 0;

  for i := 0 to chkLstFields.Count - 1 do
  begin
    if not chkLstFields.Checked[i] then
      Continue;

    if FFields[i].IsComputed then
      Continue;

    // Nur ARRAY und BLOB aufnehmen
    if (Pos('[', FFields[i].FieldType) > 0) or
       (Pos('BLOB', UpperCase(FFields[i].FieldType)) > 0) then
    begin
      SetLength(Result, idx + 1);
      Result[idx].SourceField := chkLstFields.Items[i];
      Result[idx].DestField := chkLstFields.Items[i];
      Result[idx].DestFieldType := FFields[i].FieldType;
      Result[idx].Formula := '';
      Result[idx].CopyField := True;
      Inc(idx);
    end;
  end;
end;

function TfrmCloneTable.GenerateCreateTableSQL: string;
var
  SL: TStringList;
  i: Integer;
  Iso: TIsolatedQuery;
  FieldName, FieldType, DefaultSource, NullFlag, ComputedSource: string;
  Line: string;
  FSize: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + edtDestTable.Text + ' (');

    Iso := GetFieldsIsolated(RegisteredDatabases[FSourceDBIndex].IBDatabase, Trim(comboxSourceTables.Text));
    try
      i := 0;
      while not Iso.Query.EOF do
      begin
        FieldName := Trim(Iso.Query.FieldByName('field_name').AsString);

        if chkLstFields.Checked[i] then
        begin
          GetFieldType(Iso.Query, FieldType, FSize);
          DefaultSource := Trim(Iso.Query.FieldByName('field_default_source').AsString);
          NullFlag := Iso.Query.FieldByName('field_not_null_constraint').AsString;
          ComputedSource := Trim(Iso.Query.FieldByName('computed_source').AsString);

          if ComputedSource <> '' then
          begin
            // COMPUTED BY: Immer anlegen bei normalen Tabellen
            Line := '  ' + FieldName + ' COMPUTED BY (' + ComputedSource + '),';
          end
          else
          begin
            Line := '  ' + FieldName + ' ' + FieldType;
            if DefaultSource <> '' then
              Line := Line + ' DEFAULT ' + DefaultSource;
            if NullFlag = '1' then
              Line := Line + ' NOT NULL';
            Line := Line + ',';
          end;

          SL.Add(Line);
        end;

        Iso.Query.Next;
        Inc(i);
      end;
    finally
      Iso.Free;
    end;

    // Letztes Komma entfernen
    if SL.Count > 1 then
    begin
      i := SL.Count - 1;
      Line := SL[i];
      if Line[Length(Line)] = ',' then
        SL[i] := Copy(Line, 1, Length(Line) - 1);
    end;

    SL.Add(');');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TfrmCloneTable.GenerateInsertSQL: string;
var
  SourceFields, DestFields: string;
  i: Integer;
begin
  SourceFields := '';
  DestFields := '';

  for i := 0 to chkLstFields.Count - 1 do
  begin
    if chkLstFields.Checked[i] and (not FFields[i].IsComputed) then
    begin
      if SourceFields <> '' then SourceFields := SourceFields + ', ';
      SourceFields := SourceFields + FFields[i].FieldName;

      if DestFields <> '' then DestFields := DestFields + ', ';
      DestFields := DestFields + FFields[i].FieldName;
    end;
  end;

  Result := 'INSERT INTO ' + edtDestTable.Text + ' (' + DestFields + ')' + sLineBreak +
            'SELECT ' + SourceFields + sLineBreak +
            'FROM ' + Trim(comboxSourceTables.Text) + ';';
end;

function TfrmCloneTable.GenerateCreateExternalTableSQL: string;
var
  SL: TStringList;
  i: Integer;
  Line: string;
  FieldName, FieldType: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + edtDestTable.Text + ' EXTERNAL FILE ''' +
           edtExternalFile.Text + ''' (');

    for i := 0 to chkLstFields.Count - 1 do
    begin
      if not chkLstFields.Checked[i] then Continue;

      FieldName := chkLstFields.Items[i];
      FieldType := FFields[i].FieldType;   // Firebird-Typ, z. B. 'VARCHAR(50)'

      // Keine BLOBs, Arrays oder Computed Fields in externen Tabellen!
      if FFields[i].IsComputed then Continue;
      if Pos('BLOB', UpperCase(FieldType)) > 0 then Continue;
      if Pos('[', FieldType) > 0 then Continue;  // Arrays

      SL.Add('  ' + FieldName + ' ' + FieldType + ',');
    end;

    // Letztes Komma entfernen
    if SL.Count > 1 then
    begin
      Line := SL[SL.Count - 1];
      if (Length(Line) > 0) and (Line[Length(Line)] = ',') then
        SL[SL.Count - 1] := Copy(Line, 1, Length(Line) - 1);
    end;

    SL.Add(');');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TfrmCloneTable.CreateExternalDestTable(DestDB: TIBDatabase;
  DestTrans: TIBTransaction; TableName: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  SQL := GenerateCreateExternalTableSQL;

  if SQL = '' then Exit;

  IBXScript1.Database := DestDB;
  IBXScript1.Transaction := DestTrans;

  try
    if not DestTrans.InTransaction then
      DestTrans.StartTransaction;

    IBXScript1.ExecSQLScript(SQL);
    DestTrans.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Failed to create external table: ' + E.Message, mtError, [mbOK], 0);
      DestTrans.Rollback;
    end;
  end;
end;

// ============================================================================
// AKTIONEN
// ============================================================================

procedure TfrmCloneTable.btnPreviewSQLClick(Sender: TObject);
var
  SQL: string;
begin
  if chkCreateTable.Checked then
    SQL := GenerateCreateTableSQL + sLineBreak + sLineBreak;
  SQL := SQL + GenerateInsertSQL;
  ShowMessage(SQL);
end;

procedure TfrmCloneTable.btnExecuteClick(Sender: TObject);
var
  Fields: TFieldTransformArray;
  CopyEngineLocal: TCopyTableDataLocal;
  CopyEngineCrossExecuteBlock: TCopyTableDataCrossExecuteBlock;
  CopyEngineCrossRowByRow: TCopyTableDataCrossRowByRow;
  DestTable: string;
  SkippedFields: string;
  FromRow, ToRow: Integer;
  i: integer;
begin
  if chkboxExternalTable.Checked and (Trim(edtExternalFile.Text) = '') then
  begin
    MessageDlg('Please select an external file for the external table.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if FSourceDBIndex < 0 then
  begin
    MessageDlg('Please select a valid source database.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if FDestDBIndex < 0 then
  begin
    MessageDlg('Please select a valid destination database.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // Quelle konfigurieren
  if not ConfigureSourceConnection then
  begin
    MessageDlg('Could not connect to source database:' + sLineBreak +
               sLineBreak +
               'Server: ' + comboxSourceServer.Text + sLineBreak +
               'Database: ' + comboxSourceDB.Text,
               mtError, [mbOK], 0);
    Exit;
  end;

  // Ziel konfigurieren
  if not ConfigureDestConnection then
  begin
    MessageDlg('Could not connect to destination database:' + sLineBreak +
               sLineBreak +
               'Server: ' + comboxDestServer.Text + sLineBreak +
               'Database: ' + comboxDestDB.Text,
               mtError, [mbOK], 0);
    Exit;
  end;

  DestTable := Trim(edtDestTable.Text);
  if DestTable = '' then
  begin
    MessageDlg('Please enter a destination table name.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // ============================================================
  // Prüfen ob mindestens eine Aktion gewählt wurde
  // ============================================================
  if not chkCreateTable.Checked and not chkCopyData.Checked then
  begin
    MessageDlg('No action selected.' + sLineBreak +
               'Please check at least one option:' + sLineBreak +
               '"Create Destination Table" or "Copy Data".', mtWarning, [mbOK], 0);
    Exit;
  end;

  // CREATE TABLE falls gewünscht
  if chkCreateTable.Checked then
  begin
    if not TableExists(IBDBDest, DestTable) then
    begin
      StatusBar1.SimpleText := 'Creating table ' + DestTable + '...';
      Application.ProcessMessages;

      if chkboxExternalTable.Checked then
        CreateExternalDestTable(IBDBDest, IBTransDest, DestTable)
      else
        CreateDestTable(IBDBDest, IBTransDest, DestTable);
    end;
  end;

  // ============================================================
  // Wenn chkCopyData NICHT angehakt ist → nur Tabelle, keine Daten
  // ============================================================
  if not chkCopyData.Checked then
  begin
    StatusBar1.SimpleText := 'Table created: ' + DestTable;
    ShowMessage('Table "' + DestTable + '" created successfully.' + sLineBreak +
                'No data copied (Copy Data is unchecked).');
    Exit;
  end;

  // ============================================================
  // Wenn Ziel externe Tabelle: Problemfelder automatisch abwählen
  // ============================================================
  if chkboxExternalTable.Checked then
  begin
    SkippedFields := '';
    for i := 0 to chkLstFields.Count - 1 do
    begin
      if chkLstFields.Checked[i] then
      begin
        if FFields[i].IsComputed or
           (Pos('BLOB', UpperCase(FFields[i].FieldType)) > 0) or
           (Pos('[', FFields[i].FieldType) > 0) then
        begin
          chkLstFields.Checked[i] := False;
          sgFields.Cells[0, i + 1] := '0';
          if SkippedFields <> '' then SkippedFields := SkippedFields + ', ';
          SkippedFields := SkippedFields + FFields[i].FieldName;
        end;
      end;
    end;
    if SkippedFields <> '' then
      MessageDlg('External tables do not support these field types.' + sLineBreak +
                 'They have been deselected automatically:' + sLineBreak +
                 SkippedFields, mtWarning, [mbOK], 0);
  end;

  // From/To
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

  // Felder aus Grid holen
  Fields := GetFieldTransforms;

  // Sicherstellen dass Transaktionen aktiv sind
  if Assigned(IBTransSource) and (not IBTransSource.InTransaction) then
    IBTransSource.StartTransaction;
  if Assigned(IBTransDest) and (not IBTransDest.InTransaction) then
    IBTransDest.StartTransaction;

  // ============================================================
  // GLEICHE DATENBANK → Immer INSERT...SELECT
  // ============================================================
  if FSourceDBIndex = FDestDBIndex then
  begin
    StatusBar1.SimpleText := 'Copying within same database (INSERT...SELECT)...';
    Application.ProcessMessages;

    CopyEngineLocal := TCopyTableDataLocal.Create(
      FSourceDBIndex, FDestDBIndex,
      Trim(comboxSourceTables.Text), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 500000),
      FromRow, ToRow
    );
    try
      CopyEngineLocal.Execute;
    finally
      CopyEngineLocal.Free;
    end;
  end

  // ============================================================
  // CROSS-DB: Automatic (EXECUTE BLOCK + UPDATE wenn nötig)
  // ============================================================
  else if rbExecuteBlock.Checked then
  begin
    StatusBar1.SimpleText := 'Copying across databases (Execute Block)...';
    Application.ProcessMessages;

    CopyEngineCrossExecuteBlock := TCopyTableDataCrossExecuteBlock.Create(
      FSourceDBIndex, FDestDBIndex,
      Trim(comboxSourceTables.Text), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 10000),
      FromRow, ToRow
    );
    try
      CopyEngineCrossExecuteBlock.Execute;
    finally
      CopyEngineCrossExecuteBlock.Free;
    end;
  end

  // ============================================================
  // CROSS-DB: Row-by-Row (komplett, alle Felder)
  // ============================================================
  else
  begin
    StatusBar1.SimpleText := 'Copying across databases (Row-by-Row)...';
    Application.ProcessMessages;

    CopyEngineCrossRowByRow := TCopyTableDataCrossRowByRow.Create(
      FSourceDBIndex, FDestDBIndex,
      Trim(comboxSourceTables.Text), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 10000),
      FromRow, ToRow
    );
    try
      CopyEngineCrossRowByRow.Execute;
    finally
      CopyEngineCrossRowByRow.Free;
    end;
  end;

  StatusBar1.SimpleText := 'Copy completed: ' + Trim(comboxSourceTables.Text) + ' → ' + DestTable;
end;

procedure TfrmCloneTable.btnExternalFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtExternalFile.Text := OpenDialog1.FileName;
end;

procedure TfrmCloneTable.btnGenTestFormulasClick(Sender: TObject);
var
  i: Integer;
  FieldType: string;
begin
  for i := 0 to High(FFields) do
  begin
    if FFields[i].IsComputed then
      Continue;

    FieldType := UpperCase(FFields[i].FieldType);

    // Standard-Formel: Feldwert unverändert
    FFields[i].Formula := '$1';

    // Spezifische Testformeln basierend NUR auf Datentypen
    if Pos('VARCHAR', FieldType) > 0 then
      FFields[i].Formula := '$1 || ''_CLONED'''

    else if Pos('CHAR', FieldType) > 0 then
      FFields[i].Formula := 'UPPER($1)'

    else if Pos('BLOB', FieldType) > 0 then
      FFields[i].Formula := '$1 || '' (cloned)'''

    else if Pos('INTEGER', FieldType) > 0 then
      FFields[i].Formula := '$1 + 10000000'

    else if Pos('SMALLINT', FieldType) > 0 then
      FFields[i].Formula := '$1 * 2'

    else if Pos('BIGINT', FieldType) > 0 then
      FFields[i].Formula := '$1 + 10000000'

    else if Pos('NUMERIC', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.1'

    else if Pos('DECIMAL', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.1'

    else if Pos('FLOAT', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.15 + 500'

    else if Pos('DOUBLE', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.05'

    else if Pos('DATE', FieldType) > 0 then
      FFields[i].Formula := '$1 + 30'

    else if Pos('TIMESTAMP', FieldType) > 0 then
      FFields[i].Formula := '$1 + 365'

    else if Pos('TIME', FieldType) > 0 then
      FFields[i].Formula := '$1 + 3600'

    else if Pos('BOOLEAN', FieldType) > 0 then
      FFields[i].Formula := 'true';

    // Formel ins Grid schreiben
    sgFields.Cells[3, i + 1] := FFields[i].Formula;
  end;

  StatusBar1.SimpleText := IntToStr(Length(FFields)) + ' test formulas generated.';
end;

procedure TfrmCloneTable.btnAddToQueueClick(Sender: TObject);
begin
  MessageDlg('Queue feature coming soon!', mtInformation, [mbOK], 0);
end;

procedure TfrmCloneTable.btnCancelClick(Sender: TObject);
begin
  Close;
end;

// ============================================================================
// HILFSFUNKTIONEN
// ============================================================================

function TfrmCloneTable.TableExists(DB: TIBDatabase; TableName: string): Boolean;
var
  Q: TIBQuery;
begin
  Result := False;
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.Transaction := DB.DefaultTransaction;
    Q.AllowAutoActivateTransaction := True;
    Q.SQL.Text :=
      'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
      'WHERE UPPER(RDB$RELATION_NAME) = :T AND RDB$VIEW_BLR IS NULL AND RDB$SYSTEM_FLAG = 0';
    Q.ParamByName('T').AsString := UpperCase(TableName);
    Q.Open;
    Result := not Q.EOF;
    Q.Close;
  finally
    Q.Free;
  end;
end;

function TfrmCloneTable.CreateDestTable(DestDB: TIBDatabase; DestTrans: TIBTransaction; TableName: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  SQL := GenerateCreateTableSQL;

  if SQL = '' then Exit;

  IBXScript1.Database := DestDB;
  IBXScript1.Transaction := DestTrans;

  try
    if not DestTrans.InTransaction then
      DestTrans.StartTransaction;

    IBXScript1.ExecSQLScript(SQL);
    DestTrans.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Failed to create table: ' + E.Message, mtError, [mbOK], 0);
      DestTrans.Rollback;
    end;
  end;
end;

procedure TfrmCloneTable.FormShow(Sender: TObject);
begin
  ////frmThemeSelector.btnApplyClick(self);    
  comboxSourceTablesChange(nil);
end;

procedure TfrmCloneTable.grBoxFieldsDblClick(Sender: TObject);
begin

end;

procedure TfrmCloneTable.grBoxFormulaPresetsClick(Sender: TObject);
begin

end;

procedure TfrmCloneTable.grBoxSourceClick(Sender: TObject);
begin

end;

procedure TfrmCloneTable.IBXScript1GetParamValue(Sender: TObject;
  ParamName: string; var BlobID: TISC_QUAD);
begin

end;

procedure TfrmCloneTable.Label5Click(Sender: TObject);
begin

end;

procedure TfrmCloneTable.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

procedure TfrmCloneTable.LoadFormulaPresets;
var
  i: Integer;
begin
  cbFormulaPreset.Items.Clear;
  cbFormulaPreset.Items.Add('None');

  for i := 0 to FormulaPresetManager.PresetCount - 1 do
    cbFormulaPreset.Items.Add(FormulaPresetManager.PresetName(i));

  cbFormulaPreset.ItemIndex := 0;
end;

procedure TfrmCloneTable.cbFormulaPresetChange(Sender: TObject);
var
  Preset: TFormulaPreset;
  i: Integer;
  Formula: string;
begin
  // "None" ausgewählt → alle Formeln löschen
  if cbFormulaPreset.ItemIndex <= 0 then
  begin
    for i := 0 to High(FFields) do
    begin
      if FFields[i].IsComputed then Continue;

      FFields[i].Formula := '';
      sgFields.Cells[3, i + 1] := '';
    end;

    StatusBar1.SimpleText := 'All formulas cleared.';
    Exit;
  end;

  // Preset ausgewählt → Formeln anwenden
  Preset := FormulaPresetManager.GetPreset(cbFormulaPreset.Text);
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    if FFields[i].IsComputed then Continue;

    Formula := Preset.GetFormulaForFieldType(FFields[i].FieldType);
    FFields[i].Formula := Formula;
    sgFields.Cells[3, i + 1] := Formula;
  end;

  StatusBar1.SimpleText := 'Preset "' + Preset.Name + '" applied to ' +
                           IntToStr(Length(FFields)) + ' fields.';
end;

procedure TfrmCloneTable.chkboxExternalTableChange(Sender: TObject);
begin
  edtExternalFile.Enabled := chkboxExternalTable.Checked;
  btnExternalFile.Enabled := chkboxExternalTable.Checked;
end;

procedure TfrmCloneTable.chkCreateTableChange(Sender: TObject);
begin

end;

procedure TfrmCloneTable.chkUseFormulaChange(Sender: TObject);
begin
  // Optional: Formel-Spalte im Grid ausgrauen, wenn deaktiviert
  if sgFields.Columns.Count < 4 then Exit;

  if chkUseFormula.Checked then
    sgFields.Columns[3].Color := clWindow
  else
    sgFields.Columns[3].Color := clBtnFace;
end;

procedure TfrmCloneTable.btnRefreshPresetsClick(Sender: TObject);
begin
  FormulaPresetManager.Reload;
  LoadFormulaPresets;
  StatusBar1.SimpleText := IntToStr(FormulaPresetManager.PresetCount) + ' presets loaded.';
end;

end.
