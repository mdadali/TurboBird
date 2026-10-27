unit uCrossRDBMSMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Grids, SQLDB,
  SynEdit, SynHighlighterSQL,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSStrategies,
  uCrossRDBMSEngine,
  uCrossRDBMSConnection,
  uCrossRDBMSProgress,
  uCrossRDBMSReport;

type
  { TfrmCrossRDBMSMain }

  TfrmCrossRDBMSMain = class(TForm)
    btnAutoMapTypes: TButton;
    btnPreviewSQL: TButton;
    btnExecute: TButton;
    btnClose: TButton;
    chkboxUseFormulas: TCheckBox;
    CheckGroupSourceFields: TCheckGroup;
    chkboxCopyData: TCheckBox;
    cmbIfExists: TComboBox;
    cmbStrategy: TComboBox;
    comboboxSourceTables: TComboBox;
    edtRefreshInterval: TEdit;
    edtDestTable: TEdit;
    edtCommitInterval: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    edtBatchSize: TEdit;
    grboxConnections: TGroupBox;
    grboxSourceTableAndFields: TGroupBox;
    GroupBoxSQL: TGroupBox;
    GroupBoxFormulas: TGroupBox;
    GroupBoxDest: TGroupBox;
    GroupBoxSource: TGroupBox;
    GroupBoxOptions: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lblRefreshInterval: TLabel;
    Label3: TLabel;
    lblIfExists: TLabel;
    lblBatchSize: TLabel;
    PageControl1: TPageControl;
    pnlTitle: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter5: TSplitter;
    Splitter6: TSplitter;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    tsMain: TTabSheet;
    sgFormulas: TStringGrid;
    lblStrategy: TLabel;
    tsZeosToZeos: TTabSheet;
    procedure btnAutoMapTypesClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnPreviewSQLClick(Sender: TObject);
    procedure CheckGroupSourceFieldsItemClick(Sender: TObject; Index: Integer);
    procedure comboboxSourceTablesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
    procedure sgFormulasDblClick(Sender: TObject);
  private
    FFields: TCrossFieldArray;
    FUpdatingUI: Boolean;
    FSourceConnForm: TfrmCrossRDBMSConnection;
    FDestConnForm: TfrmCrossRDBMSConnection;

    procedure SourceAfterConnect(Sender: TObject);
    procedure SourceAfterDisconnect(Sender: TObject);
    procedure LoadSourceTables;
    procedure LoadFieldsFromSource;
    procedure SyncCheckGroupToFields;
    procedure SyncGridToFields;
    procedure UpdateGrid;
    procedure AutoMapAllTypes;
    function  BuildOptions: TCrossTransferOptions;
    function  AskIfExistsAction: TIfExistsAction;
    procedure UpdatePreviewSQL;
  public
  end;

implementation

{$R *.lfm}

procedure TfrmCrossRDBMSMain.FormCreate(Sender: TObject);
begin
  // StringGrid initialisieren
  sgFormulas.FixedCols := 0;
  sgFormulas.FixedRows := 1;
  sgFormulas.ColCount := 4;
  sgFormulas.RowCount := 1;
  sgFormulas.Options := sgFormulas.Options + [goEditing, goRowSelect, goVertLine, goHorzLine];

  while sgFormulas.Columns.Count < 4 do
    sgFormulas.Columns.Add;

  sgFormulas.Columns[0].Title.Caption := 'Source Field';
  sgFormulas.Columns[1].Title.Caption := 'Source Type';
  sgFormulas.Columns[2].Title.Caption := 'Dest Type';
  sgFormulas.Columns[3].Title.Caption := 'Formula';

  sgFormulas.ColWidths[0] := 180;
  sgFormulas.ColWidths[1] := 120;
  sgFormulas.ColWidths[2] := 180;
  sgFormulas.ColWidths[3] := 320;
  sgFormulas.Columns[2].ButtonStyle := cbsPickList;

  // IfExists
  cmbIfExists.Items.Clear;
  cmbIfExists.Items.Add('Ask');
  cmbIfExists.Items.Add('Recreate');
  cmbIfExists.Items.Add('Append');
  cmbIfExists.Items.Add('Abort');
  cmbIfExists.ItemIndex := 0;

  // Strategy
  cmbStrategy.Items.Clear;
  cmbStrategy.Items.Add('Auto (recommended)');
  cmbStrategy.Items.Add('Row-by-Row');
  cmbStrategy.Items.Add('Batched Inserts');
  cmbStrategy.Items.Add('INSERT … SELECT (batched)');
  cmbStrategy.ItemIndex := 0;

  // Defaults
  edtCommitInterval.Text := '10000';
  edtBatchSize.Text := '500';
  edtFrom.Text := '1';
  edtTo.Text := '0';
  edtRefreshInterval.Text := '500';
  edtDestTable.Text := '';
  chkboxCopyData.Checked := True;

  FUpdatingUI := False;

  // Source Connection einbetten
  FSourceConnForm := TfrmCrossRDBMSConnection.Create(Self);
  FSourceConnForm.Parent := GroupBoxSource;
  FSourceConnForm.Align := alClient;
  FSourceConnForm.BorderStyle := bsNone;
  FSourceConnForm.OnAfterConnect := @SourceAfterConnect;
  FSourceConnForm.OnAfterDisconnect := @SourceAfterDisconnect;
  FSourceConnForm.Show;

  // Dest Connection einbetten
  FDestConnForm := TfrmCrossRDBMSConnection.Create(Self);
  FDestConnForm.Parent := GroupBoxDest;
  FDestConnForm.Align := alClient;
  FDestConnForm.BorderStyle := bsNone;
  FDestConnForm.Show;
end;

procedure TfrmCrossRDBMSMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FSourceConnForm) then
  begin
    try
      if FSourceConnForm.Trans.Active then FSourceConnForm.Trans.Rollback;
    except end;
    try
      if FSourceConnForm.Conn.Connected then FSourceConnForm.Conn.Close;
    except end;
  end;

  if Assigned(FDestConnForm) then
  begin
    try
      if FDestConnForm.Trans.Active then FDestConnForm.Trans.Rollback;
    except end;
    try
      if FDestConnForm.Conn.Connected then FDestConnForm.Conn.Close;
    except end;
  end;
end;

procedure TfrmCrossRDBMSMain.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

procedure TfrmCrossRDBMSMain.SourceAfterConnect(Sender: TObject);
begin
  LoadSourceTables;
end;

procedure TfrmCrossRDBMSMain.SourceAfterDisconnect(Sender: TObject);
begin
  comboboxSourceTables.Items.Clear;
  CheckGroupSourceFields.Items.Clear;
  SetLength(FFields, 0);
  sgFormulas.RowCount := 1;
  SynEdit1.Lines.Clear;
end;

procedure TfrmCrossRDBMSMain.LoadSourceTables;
var
  DBMS: TCrossDBMS;
  Q: TSQLQuery;
begin
  comboboxSourceTables.Items.Clear;
  if not Assigned(FSourceConnForm) or not FSourceConnForm.Conn.Connected then Exit;

  DBMS := DBMSFromConnection(FSourceConnForm.Conn);
  Q := FSourceConnForm.Query;
  Q.Close;

  case DBMS of
    cdFirebird:
      Q.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
                    'WHERE RDB$VIEW_BLR IS NULL AND RDB$SYSTEM_FLAG = 0 ORDER BY RDB$RELATION_NAME';
    cdPostgreSQL:
      Q.SQL.Text := 'SELECT table_name FROM information_schema.tables ' +
                    'WHERE table_schema = current_schema() AND table_type = ''BASE TABLE'' ORDER BY table_name';
    cdMySQL:
      Q.SQL.Text := 'SELECT table_name FROM information_schema.tables ' +
                    'WHERE table_schema = DATABASE() AND table_type = ''BASE TABLE'' ORDER BY table_name';
    cdSQLite:
      Q.SQL.Text := 'SELECT name FROM sqlite_master WHERE type = ''table'' AND name NOT LIKE ''sqlite_%'' ORDER BY name';
  else
    Exit;
  end;

  if not FSourceConnForm.Trans.Active then
    FSourceConnForm.Trans.StartTransaction;

  Q.Open;
  while not Q.EOF do
  begin
    comboboxSourceTables.Items.Add(Trim(Q.Fields[0].AsString));
    Q.Next;
  end;
  Q.Close;

  if comboboxSourceTables.Items.Count > 0 then
  begin
    comboboxSourceTables.ItemIndex := 0;
    comboboxSourceTablesChange(nil);
  end;
end;

procedure TfrmCrossRDBMSMain.comboboxSourceTablesChange(Sender: TObject);
begin
  if FUpdatingUI then Exit;
  if Trim(comboboxSourceTables.Text) = '' then Exit;

  edtDestTable.Text := Trim(comboboxSourceTables.Text);
  LoadFieldsFromSource;
end;

procedure TfrmCrossRDBMSMain.LoadFieldsFromSource;
var
  DestDBMS, SourceDBMS: TCrossDBMS;
  i: Integer;
  FieldName, SourceType, AutoType: string;
  Q: TSQLQuery;
begin
  SetLength(FFields, 0);
  CheckGroupSourceFields.Items.Clear;
  sgFormulas.RowCount := 1;

  if Trim(comboboxSourceTables.Text) = '' then Exit;
  if not Assigned(FSourceConnForm) or not FSourceConnForm.Conn.Connected then Exit;

  if Assigned(FDestConnForm) and FDestConnForm.Conn.Connected then
    DestDBMS := DBMSFromConnection(FDestConnForm.Conn)
  else
    DestDBMS := cdUnknown;

  SourceDBMS := DBMSFromConnection(FSourceConnForm.Conn);

  Q := FSourceConnForm.Query;
  Q.Close;
  Q.SQL.Text := 'SELECT * FROM ' + QuoteIdentifier(Trim(comboboxSourceTables.Text), SourceDBMS) + ' WHERE 1=0';

  if not FSourceConnForm.Trans.Active then
    FSourceConnForm.Trans.StartTransaction;

  Q.Open;

  FUpdatingUI := True;
  try
    for i := 0 to Q.FieldCount - 1 do
    begin
      FieldName := Q.Fields[i].FieldName;
      SourceType := GetFieldTypeString(Q.Fields[i]);
      AutoType := AutoMapType(SourceType, DestDBMS);

      SetLength(FFields, Length(FFields) + 1);
      FFields[High(FFields)].SourceName := FieldName;
      FFields[High(FFields)].SourceType := SourceType;
      FFields[High(FFields)].DestName   := FieldName;
      FFields[High(FFields)].DestType   := AutoType;
      FFields[High(FFields)].Formula    := '';
      FFields[High(FFields)].Selected   := True;

      CheckGroupSourceFields.Items.Add(FieldName);
      CheckGroupSourceFields.Checked[i] := True;
    end;

    UpdateGrid;
    UpdatePreviewSQL;
  finally
    FUpdatingUI := False;
  end;

  Q.Close;
end;

procedure TfrmCrossRDBMSMain.UpdateGrid;
var
  i: Integer;
  PickList: TStringList;
  DestDBMS: TCrossDBMS;
begin
  FUpdatingUI := True;
  try
    sgFormulas.RowCount := Length(FFields) + 1;
    for i := 0 to High(FFields) do
    begin
      sgFormulas.Cells[0, i + 1] := FFields[i].SourceName;
      sgFormulas.Cells[1, i + 1] := FFields[i].SourceType;
      sgFormulas.Cells[2, i + 1] := FFields[i].DestType;
      sgFormulas.Cells[3, i + 1] := FFields[i].Formula;
    end;

    if Assigned(FDestConnForm) and FDestConnForm.Conn.Connected then
      DestDBMS := DBMSFromConnection(FDestConnForm.Conn)
    else
      DestDBMS := cdUnknown;

    PickList := GetTypeListForDBMS(DestDBMS);
    try
      sgFormulas.Columns[2].PickList.Assign(PickList);
    finally
      PickList.Free;
    end;
  finally
    FUpdatingUI := False;
  end;
end;

procedure TfrmCrossRDBMSMain.SyncGridToFields;
var
  i: Integer;
begin
  for i := 0 to High(FFields) do
  begin
    if i + 1 >= sgFormulas.RowCount then Break;
    FFields[i].DestType := sgFormulas.Cells[2, i + 1];
    FFields[i].Formula  := sgFormulas.Cells[3, i + 1];
  end;
end;

procedure TfrmCrossRDBMSMain.SyncCheckGroupToFields;
var
  i: Integer;
begin
  for i := 0 to High(FFields) do
    if i < CheckGroupSourceFields.Items.Count then
      FFields[i].Selected := CheckGroupSourceFields.Checked[i];
end;

procedure TfrmCrossRDBMSMain.CheckGroupSourceFieldsItemClick(Sender: TObject; Index: Integer);
begin
  if FUpdatingUI then Exit;
  SyncCheckGroupToFields;
  UpdatePreviewSQL;
end;

procedure TfrmCrossRDBMSMain.sgFormulasDblClick(Sender: TObject);
var
  Row: Integer;
  NewFormula, FieldName: string;
begin
  Row := sgFormulas.Row;
  if (Row < 1) or (Row > High(FFields) + 1) then Exit;

  FieldName := FFields[Row - 1].SourceName;
  NewFormula := FFields[Row - 1].Formula;

  if InputQuery('Formula for ' + FieldName,
                'Enter SQL expression ($1 = own value):',
                NewFormula) then
  begin
    FFields[Row - 1].Formula := NewFormula;
    sgFormulas.Cells[3, Row] := NewFormula;
    UpdatePreviewSQL;
  end;
end;

procedure TfrmCrossRDBMSMain.AutoMapAllTypes;
var
  DestDBMS: TCrossDBMS;
  i: Integer;
begin
  if Assigned(FDestConnForm) and FDestConnForm.Conn.Connected then
    DestDBMS := DBMSFromConnection(FDestConnForm.Conn)
  else
    DestDBMS := cdUnknown;

  for i := 0 to High(FFields) do
    FFields[i].DestType := AutoMapType(FFields[i].SourceType, DestDBMS);
  UpdateGrid;
  UpdatePreviewSQL;
end;

procedure TfrmCrossRDBMSMain.btnAutoMapTypesClick(Sender: TObject);
begin
  AutoMapAllTypes;
end;

function TfrmCrossRDBMSMain.BuildOptions: TCrossTransferOptions;
begin
  SyncGridToFields;
  SyncCheckGroupToFields;

  Result := Default(TCrossTransferOptions);

  if Assigned(FSourceConnForm) then
  begin
    Result.SourceConn  := FSourceConnForm.Conn;
    Result.SourceTrans := FSourceConnForm.Trans;
    Result.SourceQuery := FSourceConnForm.Query;
  end;

  if Assigned(FDestConnForm) then
  begin
    Result.DestConn  := FDestConnForm.Conn;
    Result.DestTrans := FDestConnForm.Trans;
    Result.DestQuery := FDestConnForm.Query;
  end;

  Result.SourceTable       := Trim(comboboxSourceTables.Text);
  Result.DestTable         := Trim(edtDestTable.Text);
  Result.Fields            := FFields;
  Result.UseFormulas       := chkboxUseFormulas.Checked;
  Result.CopyData          := chkboxCopyData.Checked;
  Result.IfExists          := TIfExistsAction(cmbIfExists.ItemIndex);
  Result.CommitInterval    := StrToIntDef(edtCommitInterval.Text, 10000);
  Result.BatchSize         := StrToIntDef(edtBatchSize.Text, 500);
  Result.RefreshIntervalMs := StrToIntDef(edtRefreshInterval.Text, 500);

  // Strategy
  case cmbStrategy.ItemIndex of
    1: Result.Strategy := csRowByRow;
    2: Result.Strategy := csBatched;
    3: Result.Strategy := csInsertSelect;
  else
    Result.Strategy := csRowByRow; // Auto wird in der Engine entschieden
  end;

  if rbAllRows.Checked then
  begin
    Result.FromRow := 1;
    Result.ToRow := 0;
  end
  else
  begin
    Result.FromRow := StrToIntDef(edtFrom.Text, 1);
    Result.ToRow := StrToIntDef(edtTo.Text, 0);
  end;
end;

function TfrmCrossRDBMSMain.AskIfExistsAction: TIfExistsAction;
var
  R: Integer;
begin
  R := MessageDlg(
    'Destination table "' + Trim(edtDestTable.Text) + '" already exists.' + sLineBreak + sLineBreak +
    'Recreate / Append / Abort ?',
    mtConfirmation, [mbYes, mbNo, mbAbort], 0);

  case R of
    mrYes:   Result := ieaRecreate;
    mrNo:    Result := ieaAppend;
    mrAbort: Result := ieaAbort;
  else
    Result := ieaAbort;
  end;
end;

procedure TfrmCrossRDBMSMain.UpdatePreviewSQL;
var
  Engine: TCrossTransferEngine;
  Options: TCrossTransferOptions;
begin
  if Trim(comboboxSourceTables.Text) = '' then Exit;
  if Trim(edtDestTable.Text) = '' then Exit;

  Options := BuildOptions;
  Engine := TCrossTransferEngine.Create(Options);
  try
    SynEdit1.Lines.Text := Engine.PreviewSQL;
  finally
    Engine.Free;
  end;
end;

procedure TfrmCrossRDBMSMain.btnPreviewSQLClick(Sender: TObject);
begin
  if Trim(comboboxSourceTables.Text) = '' then
  begin
    MessageDlg('Please select a source table.', mtWarning, [mbOK], 0);
    Exit;
  end;
  if Trim(edtDestTable.Text) = '' then
  begin
    MessageDlg('Please enter a destination table name.', mtWarning, [mbOK], 0);
    Exit;
  end;
  UpdatePreviewSQL;
end;

procedure TfrmCrossRDBMSMain.btnExecuteClick(Sender: TObject);
var
  Engine: TCrossTransferEngine;
  Options: TCrossTransferOptions;
  DestExists: Boolean;
  LocalAction: TIfExistsAction;
  Dlg: TCrossProgressDialog;
  ReportDlg: TCrossReportDialog;
begin
  if Trim(comboboxSourceTables.Text) = '' then
  begin
    MessageDlg('Please select a source table.', mtWarning, [mbOK], 0);
    Exit;
  end;
  if Trim(edtDestTable.Text) = '' then
  begin
    MessageDlg('Please enter a destination table name.', mtWarning, [mbOK], 0);
    Exit;
  end;
  if not Assigned(FSourceConnForm) or not FSourceConnForm.Conn.Connected then
  begin
    MessageDlg('Source connection is not active.', mtWarning, [mbOK], 0);
    Exit;
  end;
  if not Assigned(FDestConnForm) or not FDestConnForm.Conn.Connected then
  begin
    MessageDlg('Destination connection is not active.', mtWarning, [mbOK], 0);
    Exit;
  end;

  Options := BuildOptions;

  // IfExists = Ask auflösen
  if Options.IfExists = ieaAsk then
  begin
    Engine := TCrossTransferEngine.Create(Options);
    try
      DestExists := Engine.DestTableExists;
    finally
      Engine.Free;
    end;

    if DestExists then
    begin
      LocalAction := AskIfExistsAction;
      if LocalAction = ieaAbort then Exit;
      Options.IfExists := LocalAction;
    end
    else
      Options.IfExists := ieaRecreate;
  end;

  Engine := TCrossTransferEngine.Create(Options);
  try
    Dlg := TCrossProgressDialog.Create(Engine);
    try
      if not Dlg.Execute then Exit;
    finally
      Dlg.Free;
    end;

    // Report anzeigen
    ReportDlg := TCrossReportDialog.Create('CrossRDBMS – Transfer Report',
      FormatCrossTransferReport(Engine.Stats));
    try
      ReportDlg.ShowDialog;
    finally
      ReportDlg.Free;
    end;

  finally
    Engine.Free;
  end;
end;

procedure TfrmCrossRDBMSMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
