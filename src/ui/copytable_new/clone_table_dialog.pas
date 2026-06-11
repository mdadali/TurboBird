unit clone_table_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, CheckLst, IBQuery, IBDatabase, IBDatabaseInfo, IBExtract, ibxscript,

  turbocommon,
  uCopyTable,
  uCopyTableCross,
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
    btnExecute: TButton;
    btnNewDB: TButton;
    btnPreviewSQL: TButton;
    btnAddToQueue: TButton;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;
    chkCreateTable: TCheckBox;
    chkLstFields: TCheckListBox;
    comboxDestDB: TComboBox;
    comboxSourceDB: TComboBox;
    comboxDestServer: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    Destination: TGroupBox;
    edtDestTable: TEdit;
    edtBatchSize: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    grboxCopyOptions: TGroupBox;
    grBoxFields: TGroupBox;
    grBoxSource: TGroupBox;
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
    pnlTop: TPanel;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
    StatusBar1: TStatusBar;
    sgFields: TStringGrid;
    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnPreviewSQLClick(Sender: TObject);
    procedure comboxDestDBChange(Sender: TObject);
    procedure comboxDestServerChange(Sender: TObject);
    procedure comboxSourceDBChange(Sender: TObject);
    procedure comboxSourceServerChange(Sender: TObject);
    procedure comboxSourceTablesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grBoxFieldsDblClick(Sender: TObject);
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
    function GenerateCreateTableSQL: string;
    function GenerateInsertSQL: string;
    function TableExists(DB: TIBDatabase; TableName: string): Boolean;
    function CreateDestTable(DestDB: TIBDatabase; DestTrans: TIBTransaction; TableName: string): Boolean;
  public
    procedure Init(ANodeInfos: TPNodeInfos);
  end;

var
  frmCloneTable: TfrmCloneTable;

implementation

{$R *.lfm}

{ TfrmCloneTable }

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
  sgFields.ColWidths[0] := 45;
  sgFields.ColWidths[1] := 150;
  sgFields.ColWidths[2] := 120;
  sgFields.ColWidths[3] := 250;

  comboxSourceServer.OnChange := nil;
  FillSourceCombos;
  FillDestCombos;

  comboxSourceServer.OnChange := @comboxSourceServerChange;
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
end;

procedure TfrmCloneTable.comboxSourceDBChange(Sender: TObject);
begin
  comboxSourceTables.Items.Clear;
  if ConfigureSourceConnection then
    FillSourceTableCombo;
end;

procedure TfrmCloneTable.comboxSourceTablesChange(Sender: TObject);
begin
  edtDestTable.Text := Trim(comboxSourceTables.Text) + '_COPY';
  LoadFields;
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
end;

procedure TfrmCloneTable.comboxDestDBChange(Sender: TObject);
begin
  ConfigureDestConnection;
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

  // ============================================================
  // TESTBLOCK: Formeln für alle Felder setzen
  // ============================================================
  for i := 0 to High(FFields) do
  begin
    if FFields[i].IsComputed then
      Continue;

    if SameText(FFields[i].FieldName, 'ID') then
    begin
      FFields[i].Formula := '$1 + 10000000';
      sgFields.Cells[3, i + 1] := '$1 + 10000000';
    end
    else if SameText(FFields[i].FieldName, 'NAME') then
    begin
      FFields[i].Formula := '$1 || ''_CLONED''';
      sgFields.Cells[3, i + 1] := '$1 || ''_CLONED''';
    end
    else if SameText(FFields[i].FieldName, 'DESCRIPTION') then
    begin
      FFields[i].Formula := '$1 || '' (cloned)''';
      sgFields.Cells[3, i + 1] := '$1 || '' (cloned)''';
    end
    else if SameText(FFields[i].FieldName, 'PRICE') then
    begin
      FFields[i].Formula := '$1 * 1.1';
      sgFields.Cells[3, i + 1] := '$1 * 1.1';
    end
    else if SameText(FFields[i].FieldName, 'QUANTITY') then
    begin
      FFields[i].Formula := '$1 * 2';
      sgFields.Cells[3, i + 1] := '$1 * 2';
    end
    else if SameText(FFields[i].FieldName, 'IS_ACTIVE') then
    begin
      FFields[i].Formula := '1';
      sgFields.Cells[3, i + 1] := '1';
    end
    else if SameText(FFields[i].FieldName, 'CREATED_DATE') then
    begin
      FFields[i].Formula := '$1 + 30';
      sgFields.Cells[3, i + 1] := '$1 + 30';
    end
    else if SameText(FFields[i].FieldName, 'CREATED_AT') then
    begin
      FFields[i].Formula := '$1 + 365';
      sgFields.Cells[3, i + 1] := '$1 + 365';
    end
    else if SameText(FFields[i].FieldName, 'SALARY') then
    begin
      FFields[i].Formula := '$1 * 1.15 + 500';
      sgFields.Cells[3, i + 1] := '$1 * 1.15 + 500';
    end
    else if SameText(FFields[i].FieldName, 'RATING') then
    begin
      FFields[i].Formula := '$1 * 1.05';
      sgFields.Cells[3, i + 1] := '$1 * 1.05';
    end
    else if SameText(FFields[i].FieldName, 'CODE') then
    begin
      FFields[i].Formula := 'UPPER($1)';
      sgFields.Cells[3, i + 1] := 'UPPER($1)';
    end
    else if SameText(FFields[i].FieldName, 'DATA_ARRAY') then
    begin
      FFields[i].Formula := '';
      sgFields.Cells[3, i + 1] := '';
    end;
  end;

  StatusBar1.SimpleText := IntToStr(Length(FFields)) + ' fields loaded (with test formulas).';
end;

{procedure TfrmCloneTable.LoadFields;
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

  StatusBar1.SimpleText := IntToStr(Length(FFields)) + ' fields loaded.';
end;}

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

function TfrmCloneTable.GetFieldTransforms: TFieldTransformArray;
var
  i: Integer;
begin
  SetLength(Result, chkLstFields.Count);
  for i := 0 to chkLstFields.Count - 1 do
  begin
    Result[i].SourceField := chkLstFields.Items[i];
    Result[i].DestField := chkLstFields.Items[i];

    // COMPUTED-Felder: Keine Formel, nicht in INSERT aufnehmen
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

{procedure TfrmCloneTable.btnExecuteClick(Sender: TObject);
var
  Fields: TFieldTransformArray;
  CopyEngine: TCopyTable;
  DestTable: string;
  FromRow, ToRow: Integer;
  TempSourceDBIndex: Integer;
  SourceServer, DestServer: string;
begin
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

  DestTable := Trim(edtDestTable.Text);
  if DestTable = '' then
  begin
    MessageDlg('Please enter a destination table name.', mtWarning, [mbOK], 0);
    Exit;
  end;

  SourceServer := RegisteredDatabases[FSourceDBIndex].RegRec.ServerName;
  DestServer := RegisteredDatabases[FDestDBIndex].RegRec.ServerName;

  // Wenn Quelle und Ziel auf unterschiedlichen Servern liegen:
  // Quell-DB temporär unter dem Ziel-Server registrieren
  TempSourceDBIndex := FSourceDBIndex;
  if not SameText(SourceServer, DestServer) then
  begin
    StatusBar1.SimpleText := 'Registering source database on destination server...';
    Application.ProcessMessages;

    TempSourceDBIndex := CopyDBRegistry(FSourceDBIndex, DestServer, '', True);
    if TempSourceDBIndex < 0 then
    begin
      MessageDlg('Failed to register source database on destination server.', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  try
    // CREATE TABLE falls gewünscht
    if chkCreateTable.Checked then
    begin
      if not TableExists(IBDBDest, DestTable) then
      begin
        StatusBar1.SimpleText := 'Creating table ' + DestTable + '...';
        Application.ProcessMessages;
        CreateDestTable(IBDBDest, IBTransDest, DestTable);
      end;
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

    // Kopieren (mit temporärem Quell-Index falls nötig)
    Fields := GetFieldTransforms;
    CopyEngine := TCopyTable.Create(
      TempSourceDBIndex, FDestDBIndex,
      Trim(comboxSourceTables.Text), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 500000),
      FromRow, ToRow
    );
    try
      CopyEngine.Execute;
    finally
      CopyEngine.Free;
    end;

    StatusBar1.SimpleText := 'Copy completed: ' + Trim(comboxSourceTables.Text) + ' → ' + DestTable;

  finally
    // Temporäre Registrierung entfernen
    if TempSourceDBIndex <> FSourceDBIndex then
      RemoveDBRegistry(TempSourceDBIndex);
  end;
end;}

procedure TfrmCloneTable.btnExecuteClick(Sender: TObject);
var
  Fields: TFieldTransformArray;
  CopyEngineDirect: TCopyTable;
  CopyEngineCross: TCopyTableCross;
  DestTable: string;
  FromRow, ToRow: Integer;
begin
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

  DestTable := Trim(edtDestTable.Text);
  if DestTable = '' then
  begin
    MessageDlg('Please enter a destination table name.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // CREATE TABLE falls gewünscht
  if chkCreateTable.Checked then
  begin
    if not TableExists(IBDBDest, DestTable) then
    begin
      StatusBar1.SimpleText := 'Creating table ' + DestTable + '...';
      Application.ProcessMessages;
      CreateDestTable(IBDBDest, IBTransDest, DestTable);
    end;
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

  // Prüfen ob gleiche Datenbank
  if FSourceDBIndex = FDestDBIndex then
  begin
    // ============================================================
    // GLEICHE DB → INSERT...SELECT (70.000 rows/sec)
    // ============================================================
    StatusBar1.SimpleText := 'Copying within same database...';
    Application.ProcessMessages;

    CopyEngineDirect := TCopyTable.Create(
      FSourceDBIndex, FDestDBIndex,
      Trim(comboxSourceTables.Text), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 500000),
      FromRow, ToRow
    );
    try
      CopyEngineDirect.Execute;
    finally
      CopyEngineDirect.Free;
    end;
  end
  else
  begin
    // ============================================================
    // UNTERSCHIEDLICHE DBs → Batch-INSERT (DB-übergreifend)
    // ============================================================
    StatusBar1.SimpleText := 'Copying across databases...';
    Application.ProcessMessages;

    CopyEngineCross := TCopyTableCross.Create(
      FSourceDBIndex, FDestDBIndex,
      Trim(comboxSourceTables.Text), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 10000),
      FromRow, ToRow
    );
    try
      CopyEngineCross.Execute;
    finally
      CopyEngineCross.Free;
    end;
  end;

  StatusBar1.SimpleText := 'Copy completed: ' + Trim(comboxSourceTables.Text) + ' → ' + DestTable;
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
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfrmCloneTable.grBoxFieldsDblClick(Sender: TObject);
begin

end;

procedure TfrmCloneTable.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

end.
