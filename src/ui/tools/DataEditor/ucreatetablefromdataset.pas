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
    edtBatchSize: TEdit;
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
    function  GetBatchSize: Integer;
    function  GetFromRow: integer;
    function  GetToRow: Integer;
    procedure RunFBInsert(ADBIndex: Integer; const ATableName: string);
    procedure CancelButtonClick(Sender: TObject);
    function  TableExists(ADB: TIBDatabase; const ATableName: string): Boolean;
  public
    procedure Init(ADataSet: TDataSet; const AFileName: string);
  end;

implementation

{$R *.lfm}

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
          RunFBInsert(DBIndex, TableName);
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
      RunFBInsert(DBIndex, TableName);

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

function TfrmCreateTableFromDataSet.GetBatchSize: Integer;
begin
  Result := StrToIntDef(edtBatchSize.Text, 1000000);
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
//  INSERT Zeile für Zeile aus dem Dataset in die FB-Tabelle
// ---------------------------------------------------------------
procedure TfrmCreateTableFromDataSet.RunFBInsert(ADBIndex: Integer; const ATableName: string);
var
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  Query: TIBQuery;
  StartTime, EndTime: TDateTime;
  ProgressForm: TForm;
  ProgressLabel, LblElapsed: TLabel;
  ProgressBar: TProgressBar;
  BtnCancel: TButton;
  FieldNames, Params, SQL: string;
  i, f, Total, Current, FromRow, ToRow, BatchSize, BatchCounter: Integer;
  Param: TParam;
begin
  DestDB := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;
  if not DestDB.Connected then DestDB.Connected := True;
  if not DestTrans.InTransaction then DestTrans.StartTransaction;

  FieldNames := '';
  Params := '';
  for i := 0 to High(FFields) do
    if FFields[i].Checked then
    begin
      if FieldNames <> '' then FieldNames := FieldNames + ', ';
      FieldNames := FieldNames + FFields[i].FieldName;
      if Params <> '' then Params := Params + ', ';
      Params := Params + ':' + FFields[i].FieldName;
    end;

  SQL := 'INSERT INTO ' + ATableName + ' (' + FieldNames + ') VALUES (' + Params + ')';
  Query := TIBQuery.Create(nil);
  try
    Query.Database := DestDB;
    Query.Transaction := DestTrans;
    Query.AllowAutoActivateTransaction := true;
    Query.SQL.Text := SQL;
    Query.Prepare;

    FromRow := GetFromRow;
    ToRow := GetToRow;
    Total := ToRow - FromRow + 1;
    BatchSize := GetBatchSize;

    ProgressForm := TForm.Create(nil);
    try
      ProgressForm.FormStyle := fsNormal;
      ProgressForm.Caption := 'Copying data to ' + ATableName;
      ProgressForm.Width := 520;
      ProgressForm.Height := 230;
      ProgressForm.Position := poScreenCenter;
      ProgressForm.BorderStyle := bsDialog;

      ProgressLabel := TLabel.Create(ProgressForm);
      ProgressLabel.Parent := ProgressForm;
      ProgressLabel.Left := 16;
      ProgressLabel.Top := 16;
      ProgressLabel.Caption := 'Total Records: ' + IntToStr(Total);
      ProgressLabel.Width := 460;

      ProgressBar := TProgressBar.Create(ProgressForm);
      ProgressBar.Parent := ProgressForm;
      ProgressBar.Left := 16;
      ProgressBar.Top := 45;
      ProgressBar.Width := 470;
      ProgressBar.Height := 20;
      ProgressBar.Min := 0;
      ProgressBar.Max := Total;
      ProgressBar.Position := 0;

      LblElapsed := TLabel.Create(ProgressForm);
      LblElapsed.Parent := ProgressForm;
      LblElapsed.Left := 16;
      LblElapsed.Top := 80;

      BtnCancel := TButton.Create(ProgressForm);
      BtnCancel.Parent := ProgressForm;
      BtnCancel.Caption := 'Cancel';
      BtnCancel.Left := 200;
      BtnCancel.Top := 120;
      BtnCancel.Width := 100;
      BtnCancel.OnClick := @CancelButtonClick;

      ProgressForm.Show;
      Application.ProcessMessages;

      FCancelled := False;

      FDataSet.DisableControls;
      try
        FDataSet.First;
        for i := 1 to FromRow - 1 do FDataSet.Next;

        StartTime := Now;
        Current := 0;
        BatchCounter := 0;

        for i := FromRow to ToRow do
        begin
          if FCancelled then Break;

          for f := 0 to High(FFields) do
          begin
            if not FFields[f].Checked then Continue;
            Param := Query.ParamByName(FFields[f].FieldName);
            if FDataSet.FieldByName(FFields[f].FieldName).IsNull then
              Param.Clear
            else
            begin
              case FDataSet.FieldByName(FFields[f].FieldName).DataType of
                ftSmallint: Param.AsSmallInt := FDataSet.FieldByName(FFields[f].FieldName).AsInteger;
                ftInteger, ftLargeInt: Param.AsInteger := FDataSet.FieldByName(FFields[f].FieldName).AsInteger;
                ftFloat, ftCurrency: Param.AsFloat := FDataSet.FieldByName(FFields[f].FieldName).AsFloat;
                ftDateTime, ftTimeStamp, ftDate: Param.AsDateTime := FDataSet.FieldByName(FFields[f].FieldName).AsDateTime;
                ftBoolean: Param.AsBoolean := FDataSet.FieldByName(FFields[f].FieldName).AsBoolean;
                else Param.AsString := FDataSet.FieldByName(FFields[f].FieldName).AsString;
              end;
            end;
          end;

          Query.ExecSQL;
          Inc(Current);
          Inc(BatchCounter);

          if BatchCounter >= BatchSize then
          begin
            DestTrans.CommitRetaining;
            BatchCounter := 0;
          end;

          ProgressBar.Position := Current;
          ProgressLabel.Caption := Format('Copying row %d of %d', [Current, Total]);
          LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime);
          Application.ProcessMessages;
          FDataSet.Next;
        end;

        if BatchCounter > 0 then
          DestTrans.CommitRetaining;

      finally
        FDataSet.EnableControls;
      end;

      EndTime := Now;
    finally
      ProgressForm.Free;
    end;

    if not FCancelled then
    begin
      ShowMessage(Format('Data copy completed!' + sLineBreak +
                         'Rows: %d' + sLineBreak +
                         'Time: %s' + sLineBreak +
                         'Speed: %.0f rows/sec',
                         [Current, FormatDateTime('hh:nn:ss', EndTime - StartTime),
                          Current / Max(1, (EndTime - StartTime) * 86400)]));
    end
    else
      ShowMessage('Copy cancelled by user.');
  finally
    Query.Unprepare;
    Query.Free;
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
