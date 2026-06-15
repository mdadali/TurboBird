unit uCreateTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CheckLst, Grids, ComCtrls, DB, Math, DateUtils,
  IBDatabase, IBQuery, IBXScript,
  turbocommon, fbcommon,
  uFormulaPresets,
  uCopyTableDataLocal,
  uGenSQLFromCSVDataset;

type
  TCopyDataTarget = (cdtNone, cdtFB, cdtExternal, cdtBoth);
  TTableTarget    = (ttFB, ttExternal, ttBoth);

  { TfrmCreateFirebirdTable }

  TfrmCreateFirebirdTable = class(TForm)
    btnMainCancel: TButton;
    btnDeselectAll: TButton;
    btnOK: TButton;
    btnOpenExternalFile: TButton;
    btnRefreshPresets: TButton;
    btnSelectAll: TButton;
    btnRun: TButton;
    cbFormulaPreset: TComboBox;
    chkUseFormula: TCheckBox;
    chkLstFields: TCheckListBox;
    cmbBoxServers: TComboBox;
    cmbBoxDBs: TComboBox;
    edtExternalFileName: TEdit;
    edtDestTableName: TEdit;
    edtBatchSize: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    grBoxCopyOptions1: TGroupBox;
    grBoxFields: TGroupBox;
    grBoxFormulaPresets: TGroupBox;
    grBoxTableType: TGroupBox;
    grBoxCopyOptions: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    pnlBottom: TPanel;
    pnlFields: TPanel;
    pnlHint: TPanel;
    rbAllRows: TRadioButton;
    rbCopyDataToFB: TRadioButton;
    rbCopyDataToExternal: TRadioButton;
    rbCopyDataToBothBoth: TRadioButton;
    rbCopyDataToNone: TRadioButton;
    rbRange: TRadioButton;
    rbTableFB: TRadioButton;
    rbTableExternal: TRadioButton;
    rbTableBoth: TRadioButton;
    sgFields: TStringGrid;

    procedure btnMainCancelClick(Sender: TObject);
    procedure btnRefreshPresetsClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnOpenExternalFileClick(Sender: TObject);
    procedure cmbBoxServersChange(Sender: TObject);
    procedure edtExternalFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
  private
    FDataSet: TDataSet;
    FFileName: string;
    FFields: array of record
      FieldName: string;
      FieldType: string;
      CharLength: Integer;
      Checked: Boolean;
      Formula: string;
    end;
    FExternalFileName: string;
    FCancelled: Boolean;

    procedure LoadFieldList;
    procedure FillServerCombo;
    procedure FillDBCombo;
    function  GetTargetDBIndex: Integer;
    function  GetTargetTable: string;
    function  GetCopyDataTarget: TCopyDataTarget;
    function  GetTableTarget: TTableTarget;
    function  GetBatchSize: Integer;
    function  GetFromRow: integer;
    function  GetToRow: Integer;
    procedure RunFBInsert(ADBIndex: Integer; const ATableName: string);
    procedure RunExternalExport(ADBIndex: Integer; const ATableName: string);
    procedure CancelButtonClick(Sender: TObject);

    function TableExists(ADB: TIBDatabase; const ATableName: string): Boolean;

  public
    procedure Init(ADataSet: TDataSet; const AFileName: string);
  end;

implementation

{$R *.lfm}

procedure TfrmCreateFirebirdTable.FormCreate(Sender: TObject);
begin
  sgFields.ColCount := 4;
  sgFields.Cells[0, 0] := 'Copy';
  sgFields.Cells[1, 0] := 'Source Field';
  sgFields.Cells[2, 0] := 'Field Type';
  sgFields.Cells[3, 0] := 'Formula ($1 = value)';
  sgFields.ColWidths[0] := 45;
  sgFields.ColWidths[1] := 150;
  sgFields.ColWidths[2] := 120;
  sgFields.ColWidths[3] := 250;
end;

procedure TfrmCreateFirebirdTable.Panel1Click(Sender: TObject);
begin

end;

procedure TfrmCreateFirebirdTable.Init(ADataSet: TDataSet; const AFileName: string);
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

procedure TfrmCreateFirebirdTable.FillServerCombo;
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

procedure TfrmCreateFirebirdTable.FillDBCombo;
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

procedure TfrmCreateFirebirdTable.LoadFieldList;
var
  Gen: TGenSQLFromCSVDataset;
  i: Integer;
begin
  Gen := TGenSQLFromCSVDataset.Create(FDataSet,
           UpperCase(ChangeFileExt(ExtractFileName(FFileName), '')),
           50);
  try
    // Felder aus dem Generator holen
    SetLength(FFields, Length(Gen.Fields));
    for i := 0 to High(Gen.Fields) do
    begin
      FFields[i].FieldName := Gen.Fields[i].FieldName;
      FFields[i].FieldType := Gen.Fields[i].FieldType;
      FFields[i].Checked    := True;
      FFields[i].Formula    := '';
      FFields[i].CharLength := 0;
    end;

    // GUI befüllen
    chkLstFields.Clear;
    sgFields.RowCount := 1;
    for i := 0 to High(FFields) do
    begin
      chkLstFields.Items.Add(FFields[i].FieldName);
      chkLstFields.Checked[i] := True;
      sgFields.RowCount := i + 2;
      sgFields.Cells[0, i + 1] := '1';
      sgFields.Cells[1, i + 1] := FFields[i].FieldName;
      sgFields.Cells[2, i + 1] := FFields[i].FieldType;
      sgFields.Cells[3, i + 1] := '';
    end;
  finally
    Gen.Free;
  end;

  cbFormulaPreset.Items.Clear;
  cbFormulaPreset.Items.Add('None');
end;

procedure TfrmCreateFirebirdTable.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

procedure TfrmCreateFirebirdTable.btnMainCancelClick(Sender: TObject);
begin

end;

procedure TfrmCreateFirebirdTable.btnRefreshPresetsClick(Sender: TObject);
begin

end;

procedure TfrmCreateFirebirdTable.btnRunClick(Sender: TObject);
var
  DBIndex, i: Integer;
  TableName, SQL, FieldList: string;
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  Script: TIBXScript;
  TableTarget: TTableTarget;
  CopyTarget: TCopyDataTarget;
  FBBuilt, ExtBuilt: Boolean;        // merken, ob die Tabelle bereits existierte
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

  TableTarget := GetTableTarget;
  CopyTarget := GetCopyDataTarget;

  DestDB := RegisteredDatabases[DBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[DBIndex].IBTransaction;
  if not DestDB.Connected then DestDB.Connected := True;
  if not DestTrans.InTransaction then DestTrans.StartTransaction;

  Script := TIBXScript.Create(nil);
  try
    Script.Database := DestDB;
    Script.Transaction := DestTrans;

    FBBuilt := False;
    ExtBuilt := False;

    // 1) Firebird-Tabelle erstellen (falls gewünscht)
    if TableTarget in [ttFB, ttBoth] then
    begin
      // Prüfen, ob Tabelle schon existiert
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
          // Tabelle existiert und wird nicht gedroppt → wir tun so, als sei sie gerade gebaut
          FBBuilt := True;
        end;
      end;

      if not FBBuilt then
      begin
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
        FBBuilt := True;
      end;
    end;

    // 2) Externe Tabelle erstellen (falls gewünscht)
    if TableTarget in [ttExternal, ttBoth] then
    begin
      if TableExists(DestDB, TableName + '_EXT') then
      begin
        if MessageDlg('External table "' + TableName + '_EXT" already exists. Drop and recreate?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          SQL := 'DROP TABLE ' + TableName + '_EXT';
          Script.ExecSQLScript(SQL);
          DestTrans.CommitRetaining;
        end
        else
          ExtBuilt := True;   // existiert bereits
      end;

      if not ExtBuilt then
      begin
        RunExternalExport(DBIndex, TableName + '_EXT');
        ExtBuilt := True;
      end;
    end;

    // 3) Daten in FB-Tabelle kopieren (falls gewünscht)
    if CopyTarget in [cdtFB, cdtBoth] then
    begin
      // FB-Tabelle muss existieren (ggf. wurde sie weiter oben gebaut)
      if not FBBuilt then
      begin
        ShowMessage('Firebird table not available – cannot copy data.');
        Exit;
      end;
      RunFBInsert(DBIndex, TableName);
    end;

  finally
    Script.Free;
  end;
end;

procedure TfrmCreateFirebirdTable.btnSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
  begin
    chkLstFields.Checked[i] := True;
    sgFields.Cells[0, i + 1] := '1';
  end;
end;

procedure TfrmCreateFirebirdTable.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
  begin
    chkLstFields.Checked[i] := False;
    sgFields.Cells[0, i + 1] := '0';
  end;
end;

procedure TfrmCreateFirebirdTable.btnOpenExternalFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Firebird External Table (*.bin)|*.bin|All files (*.*)|*.*';
    if Execute then
    begin
      FExternalFileName := FileName;
      edtExternalFileName.Text := FileName;
    end;
  finally
    Free;
  end;
end;

procedure TfrmCreateFirebirdTable.cmbBoxServersChange(Sender: TObject);
begin
  if cmbBoxServers.ItemIndex >= 0 then
    FillDBCombo;
end;

procedure TfrmCreateFirebirdTable.edtExternalFileNameChange(Sender: TObject);
begin
  FExternalFileName := edtExternalFileName.Text;
end;

function TfrmCreateFirebirdTable.GetTargetDBIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, cmbBoxServers.Text) and
       SameText(RegisteredDatabases[i].RegRec.Title, cmbBoxDBs.Text) then
      Exit(i);
end;

function TfrmCreateFirebirdTable.GetTargetTable: string;
begin
  Result := Trim(edtDestTableName.Text);
end;

function TfrmCreateFirebirdTable.GetCopyDataTarget: TCopyDataTarget;
begin
  if rbCopyDataToFB.Checked then Result := cdtFB
  else if rbCopyDataToExternal.Checked then Result := cdtExternal
  else if rbCopyDataToBothBoth.Checked then Result := cdtBoth
  else Result := cdtNone;
end;

function TfrmCreateFirebirdTable.GetTableTarget: TTableTarget;
begin
  if rbTableFB.Checked then Result := ttFB
  else if rbTableExternal.Checked then Result := ttExternal
  else Result := ttBoth;
end;

function TfrmCreateFirebirdTable.GetBatchSize: Integer;
begin
  Result := StrToIntDef(edtBatchSize.Text, 1000000);
end;

function TfrmCreateFirebirdTable.GetFromRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtFrom.Text, 1)
  else
    Result := 1;
end;

function TfrmCreateFirebirdTable.GetToRow: Integer;
begin
  if rbRange.Checked then
    Result := StrToIntDef(edtTo.Text, FDataSet.RecordCount)
  else
    Result := FDataSet.RecordCount;
end;

procedure TfrmCreateFirebirdTable.CancelButtonClick(Sender: TObject);
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
//  MIT eigenem, temporärem Fortschrittsdialog
// ---------------------------------------------------------------
procedure TfrmCreateFirebirdTable.RunFBInsert(ADBIndex: Integer; const ATableName: string);
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
  UseFormula: Boolean;
  Param: TParam;
  Val: string;
begin
  DestDB := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;
  if not DestDB.Connected then DestDB.Connected := True;
  if not DestTrans.InTransaction then DestTrans.StartTransaction;

  UseFormula := chkUseFormula.Checked;

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

    // ------------------------------------------------------------------
    // Temporärer Fortschrittsdialog
    // ------------------------------------------------------------------
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
              if UseFormula and (FFields[f].Formula <> '') then
              begin
                Val := StringReplace(FFields[f].Formula, '$1',
                                     FDataSet.FieldByName(FFields[f].FieldName).AsString, [rfReplaceAll]);
                Param.AsString := Val;
              end
              else
              begin
                case FDataSet.FieldByName(FFields[f].FieldName).DataType of
                  ftFloat, ftCurrency: Param.AsFloat := FDataSet.FieldByName(FFields[f].FieldName).AsFloat;
                  ftInteger, ftLargeInt, ftSmallint: Param.AsInteger := FDataSet.FieldByName(FFields[f].FieldName).AsInteger;
                  ftDateTime, ftTimeStamp, ftDate: Param.AsDateTime := FDataSet.FieldByName(FFields[f].FieldName).AsDateTime;
                  else Param.AsString := FDataSet.FieldByName(FFields[f].FieldName).AsString;
                end;
              end;
            end;
          end;

          Query.ExecSQL;
          Inc(Current);
          Inc(BatchCounter);

          // Batch-CommitRetaining
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

        // Rest committen
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

// ---------------------------------------------------------------
//  Export für externe Tabelle MIT TCopyTableDataLocal
//  Kopiert die Daten aus der bereits befüllten Firebird-Tabelle
//  in die externe Tabelle (CSV). Die FB-Tabelle muss existieren.
// ---------------------------------------------------------------
procedure TfrmCreateFirebirdTable.RunExternalExport(ADBIndex: Integer; const ATableName: string);
var
  i, Idx: Integer;
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  Script: TIBXScript;
  SQL, FieldList, FBTableName: string;
  Transforms: TFieldTransformArray;
  Engine: TCopyTableDataLocal;
  CountQry: TIBQuery;
  SrcRecordCount: Integer;
begin
  if FExternalFileName = '' then
  begin
    ShowMessage('Please select an external file first.');
    Exit;
  end;

  DestDB := RegisteredDatabases[ADBIndex].IBDatabase;
  DestTrans := RegisteredDatabases[ADBIndex].IBTransaction;
  if not DestDB.Connected then DestDB.Connected := True;
  if not DestTrans.InTransaction then DestTrans.StartTransaction;

  // Die Firebird‑Tabelle, deren Daten kopiert werden sollen,
  // ist die normale Tabelle (z. B. "MYTABLE", nicht "_EXT").
  FBTableName := edtDestTableName.Text;

  // 1) Externe Tabelle anlegen (CREATE TABLE … EXTERNAL)
  FieldList := '';
  for i := 0 to High(FFields) do
  begin
    if not FFields[i].Checked then Continue;
    if FieldList <> '' then FieldList := FieldList + ', ';
    FieldList := FieldList + FFields[i].FieldName + ' ' + FFields[i].FieldType;
  end;

  SQL := 'CREATE TABLE ' + ATableName + ' EXTERNAL ''' + FExternalFileName + ''' (' + FieldList + ')';

  Script := TIBXScript.Create(nil);
  try
    Script.Database := DestDB;
    Script.Transaction := DestTrans;

    Script.ExecSQLScript(SQL);
    DestTrans.CommitRetaining;
  finally
    Script.Free;
  end;

  // 2) Prüfen, ob die FB‑Tabelle überhaupt Daten enthält
  CountQry := TIBQuery.Create(nil);
  try
    CountQry.Database := DestDB;
    CountQry.Transaction := DestTrans;
    CountQry.AllowAutoActivateTransaction := true;
    CountQry.SQL.Text := 'SELECT COUNT(*) FROM ' + FBTableName;
    CountQry.Open;
    SrcRecordCount := CountQry.Fields[0].AsInteger;
    CountQry.Close;
  finally
    CountQry.Free;
  end;

  if SrcRecordCount = 0 then
  begin
    ShowMessage('Firebird table "' + FBTableName + '" is empty – nothing to export.');
    Exit;
  end;

  // 3) Feldtransformationen für TCopyTableDataLocal aufbauen
  SetLength(Transforms, 0);
  Idx := 0;
  for i := 0 to High(FFields) do
  begin
    if not FFields[i].Checked then Continue;
    SetLength(Transforms, Idx + 1);
    Transforms[Idx].SourceField   := FFields[i].FieldName;
    Transforms[Idx].DestField     := FFields[i].FieldName;
    Transforms[Idx].DestFieldType := FFields[i].FieldType;
    Transforms[Idx].Formula       := '';
    Transforms[Idx].CopyField     := True;
    Inc(Idx);
  end;

  // 4) TCopyTableDataLocal ausführen (mit eigenem Fortschrittsdialog)
  Engine := TCopyTableDataLocal.Create(
    ADBIndex,          // Quell‑DB = Ziel‑DB (gleiche DB)
    ADBIndex,
    FBTableName,       // Quell‑Tabelle (die FB‑Tabelle)
    ATableName,        // Ziel‑Tabelle (die externe)
    Transforms,
    GetBatchSize,
    GetFromRow,
    GetToRow
  );
  try
    Engine.Execute;    // Fortschrittsdialog + Statistik werden selbständig angezeigt
  finally
    Engine.Free;
  end;
end;

function TfrmCreateFirebirdTable.TableExists(ADB: TIBDatabase; const ATableName: string): Boolean;
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
