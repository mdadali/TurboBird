unit uCrossRDBMSMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Grids,  SynEdit, SynHighlighterSQL,
  DB,
  SQLDB,
  IB,
  IBDatabase,
  IBQuery,
  IBCustomDataset,

  turbocommon,

  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSStrategies,
  uCrossRDBMSEngine,
  uCrossRDBMSConnection,
  uCrossRDBMSProgress,
  uCrossRDBMSReport,
  uCrossRDBMSFieldAnalysis;

type
  { TfrmCrossRDBMSMain }

  TfrmCrossRDBMSMain = class(TForm)
    btnAutoMapTypes: TButton;
    btnPreviewSQL: TButton;
    btnExecute: TButton;
    btnClose: TButton;
    chkboxCopyData: TCheckBox;
    CheckGroupSourceFields: TCheckGroup;
    chkboxUseFormulas: TCheckBox;
    cmbIfExists: TComboBox;
    cmbStrategy: TComboBox;
    comboboxSourceTables: TComboBox;
    edtBatchSize: TEdit;
    edtCommitInterval: TEdit;
    edtDestTable: TEdit;
    edtFrom: TEdit;
    edtRefreshInterval: TEdit;
    edtTo: TEdit;
    grboxConnections: TGroupBox;
    grboxSourceTableAndFields: TGroupBox;
    grBoxRows: TGroupBox;
    grBoxDestTable: TGroupBox;
    grBoxCopyEngine: TGroupBox;
    grBoxIntervals: TGroupBox;
    GroupBoxSQL: TGroupBox;
    GroupBoxFormulas: TGroupBox;
    GroupBoxDest: TGroupBox;
    GroupBoxSource: TGroupBox;
    GroupBoxOptions: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblIfExists: TLabel;
    lblBatchSize: TLabel;
    lblRefreshInterval: TLabel;
    PageControl1: TPageControl;
    pnlTitle: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
    Splitter1: TSplitter;
    Splitter3: TSplitter;
    Splitter6: TSplitter;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    tsMain: TTabSheet;
    sgFormulas: TStringGrid;
    lblStrategy: TLabel;
    tsTransfer: TTabSheet;
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
    FProblemFieldsDeselected: TStringList;


    procedure SourceAfterConnect(Sender: TObject);
    procedure SourceAfterDisconnect(Sender: TObject);
    procedure LoadSourceTables;
    procedure LoadFieldsFromSource;
    procedure SyncFieldsToCheckGroup;
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
  FProblemFieldsDeselected := TStringList.Create;

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
  edtFrom.Text := '1';
  edtTo.Text := '0';
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
      if Assigned(FSourceConnForm) then
        if Assigned(FSourceConnForm.Conn)  then
          if FSourceConnForm.Conn.Connected then FSourceConnForm.Conn.Close;
    except end;
  end;

  if Assigned(FDestConnForm) then
  begin
    try
      if FDestConnForm.Trans.Active then FDestConnForm.Trans.Rollback;
    except end;
    try
      if Assigned(FDestConnForm) then
        if Assigned(FDestConnForm.Conn) then
          if FDestConnForm.Conn.Connected then FDestConnForm.Conn.Close;
    except end;
  end;

  if Assigned(FProblemFieldsDeselected) then
    FreeAndNil(FProblemFieldsDeselected);

  CloseAction := caFree;
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
  SQL: string;
  Q: TSQLQuery;
  IBQ: TIBQuery;
begin
  comboboxSourceTables.Items.Clear;
  if not Assigned(FSourceConnForm) then Exit;

  { Verbindung prüfen — IBX oder SQLDB }
  if FSourceConnForm.IsIBX then
  begin
    if not FSourceConnForm.IBXConn.Connected then Exit;
  end
  else
  begin
    if not FSourceConnForm.Conn.Connected then Exit;
  end;

  { DBMS bestimmen }
  DBMS := FSourceConnForm.ConnDBMS;
  if DBMS = cdUnknown then
  begin
    if FSourceConnForm.IsIBX then
      DBMS := cdFirebird
    else
      DBMS := DBMSFromConnection(FSourceConnForm.Conn);
  end;

  { SQL je nach DBMS }
  case DBMS of
    cdFirebird:
      SQL := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
             'WHERE RDB$VIEW_BLR IS NULL AND RDB$SYSTEM_FLAG = 0 ' +
             'ORDER BY RDB$RELATION_NAME';
    cdPostgreSQL:
      SQL := 'SELECT table_name FROM information_schema.tables ' +
             'WHERE table_schema = current_schema() ' +
             'AND table_type = ''BASE TABLE'' ORDER BY table_name';
    cdMySQL:
      SQL := 'SELECT table_name FROM information_schema.tables ' +
             'WHERE table_schema = DATABASE() ' +
             'AND table_type = ''BASE TABLE'' ORDER BY table_name';
    cdSQLite:
      SQL := 'SELECT name FROM sqlite_master ' +
             'WHERE type = ''table'' AND name NOT LIKE ''sqlite_%'' ' +
             'ORDER BY name';
    cdMSSQL:
      SQL := 'SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES ' +
             'WHERE TABLE_TYPE = ''BASE TABLE'' ORDER BY TABLE_NAME';
    cdOracle:
      SQL := 'SELECT TABLE_NAME FROM USER_TABLES ORDER BY TABLE_NAME';
    cdSybase:
      SQL := 'SELECT name FROM sysobjects ' +
             'WHERE type = ''U'' ORDER BY name';
  else
    SQL := 'SELECT table_name FROM information_schema.tables ' +
           'WHERE table_type = ''BASE TABLE'' ORDER BY table_name';
  end;

  { Query ausführen — IBX oder SQLDB }
  try
    if FSourceConnForm.IsIBX then
    begin
      IBQ := FSourceConnForm.IBXQuery;
      IBQ.Close;
      IBQ.SQL.Text := SQL;
      if not FSourceConnForm.IBXTrans.Active then
        FSourceConnForm.IBXTrans.StartTransaction;
      IBQ.Open;
      try
        while not IBQ.EOF do
        begin
          comboboxSourceTables.Items.Add(Trim(IBQ.Fields[0].AsString));
          IBQ.Next;
        end;
      finally
        IBQ.Close;
      end;
    end
    else
    begin
      Q := FSourceConnForm.Query;
      Q.Close;
      Q.SQL.Text := SQL;
      if not FSourceConnForm.Trans.Active then
        FSourceConnForm.Trans.StartTransaction;
      Q.Open;
      try
        while not Q.EOF do
        begin
          comboboxSourceTables.Items.Add(Trim(Q.Fields[0].AsString));
          Q.Next;
        end;
      finally
        Q.Close;
      end;
    end;
  except
    on E: Exception do
    begin
      MessageDlg('Could not load tables: ' + E.Message, mtWarning, [mbOK], 0);
      Exit;
    end;
  end;

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
  FieldName, RealName, SourceType, AutoType: string;
  Q: TSQLQuery;
  IBQ: TIBQuery;
  FieldCount: Integer;
  IsArr, IsComputed, IsBinaryBlob, IsProblem: Boolean;
  ComputedExprs: TStringList;
  FieldNamesOrdered: TStringList;
  MetaIBQ: TIBQuery;
  MetaSQLQ: TSQLQuery;
  MetaTrans: TIBTransaction;
  S: string;
begin
  SetLength(FFields, 0);
  CheckGroupSourceFields.Items.Clear;
  sgFormulas.RowCount := 1;
  FProblemFieldsDeselected.Clear;

  if Trim(comboboxSourceTables.Text) = '' then Exit;
  if not Assigned(FSourceConnForm) then Exit;

  if FSourceConnForm.IsIBX then
  begin
    if not Assigned(FSourceConnForm.IBXConn) or
       not FSourceConnForm.IBXConn.Connected then Exit;
    SourceDBMS := cdFirebird;
  end
  else
  begin
    if not Assigned(FSourceConnForm.Conn) or
       not FSourceConnForm.Conn.Connected then Exit;
    SourceDBMS := FSourceConnForm.ConnDBMS;
    if SourceDBMS = cdUnknown then
      SourceDBMS := DBMSFromConnection(FSourceConnForm.Conn);
  end;

  if Assigned(FDestConnForm) then
  begin
    if FDestConnForm.IsIBX then
    begin
      if Assigned(FDestConnForm.IBXConn) and FDestConnForm.IBXConn.Connected then
        DestDBMS := cdFirebird
      else
        DestDBMS := cdUnknown;
    end
    else if Assigned(FDestConnForm.Conn) and FDestConnForm.Conn.Connected then
    begin
      DestDBMS := FDestConnForm.ConnDBMS;
      if DestDBMS = cdUnknown then
        DestDBMS := DBMSFromConnection(FDestConnForm.Conn);
    end
    else
      DestDBMS := cdUnknown;
  end
  else
    DestDBMS := cdUnknown;

  { ---- Metadata: echte Feldnamen (in Reihenfolge) + Computed (optional) ---- }
  ComputedExprs := TStringList.Create;
  ComputedExprs.CaseSensitive := False;
  FieldNamesOrdered := TStringList.Create;
  try
    try
      if SourceDBMS = cdFirebird then
      begin
        if FSourceConnForm.IsIBX then
        begin
          MetaTrans := TIBTransaction.Create(nil);
          MetaIBQ := TIBQuery.Create(nil);
          try
            MetaTrans.DefaultDatabase := FSourceConnForm.IBXConn;
            MetaTrans.Params.Clear;
            MetaTrans.Params.Add('read_committed');
            MetaTrans.Params.Add('rec_version');
            MetaTrans.Params.Add('nowait');
            MetaTrans.StartTransaction;

            MetaIBQ.Database := FSourceConnForm.IBXConn;
            MetaIBQ.Transaction := MetaTrans;

            { Query 1: Feldnamen — immer, robust }
            MetaIBQ.SQL.Text :=
              'SELECT TRIM(RDB$FIELD_NAME) FROM RDB$RELATION_FIELDS ' +
              'WHERE RDB$RELATION_NAME = ' + QuotedStr(Trim(comboboxSourceTables.Text)) +
              ' ORDER BY RDB$FIELD_POSITION';
            MetaIBQ.Open;
            while not MetaIBQ.EOF do
            begin
              FieldNamesOrdered.Add(Trim(MetaIBQ.Fields[0].AsString));
              MetaIBQ.Next;
            end;
            MetaIBQ.Close;

            { Query 2: Computed — optional, in eigenem try }
            try
              MetaIBQ.SQL.Text :=
                'SELECT TRIM(RDB$FIELD_NAME), CAST(RDB$COMPUTED_SOURCE AS VARCHAR(8192)) ' +
                'FROM RDB$RELATION_FIELDS ' +
                'WHERE RDB$RELATION_NAME = ' + QuotedStr(Trim(comboboxSourceTables.Text)) +
                ' AND RDB$COMPUTED_BLR IS NOT NULL';
              MetaIBQ.Open;
              while not MetaIBQ.EOF do
              begin
                ComputedExprs.Values[Trim(MetaIBQ.Fields[0].AsString)] :=
                  Trim(MetaIBQ.Fields[1].AsString);
                MetaIBQ.Next;
              end;
              MetaIBQ.Close;
            except
              try MetaIBQ.Close; except end;
            end;

            MetaTrans.Commit;
          finally
            MetaIBQ.Free;
            MetaTrans.Free;
          end;
        end
        else if Assigned(FSourceConnForm.Conn) then
        begin
          MetaSQLQ := TSQLQuery.Create(nil);
          try
            MetaSQLQ.Database := FSourceConnForm.Conn;
            MetaSQLQ.Transaction := FSourceConnForm.Trans;
            if not FSourceConnForm.Trans.Active then
              FSourceConnForm.Trans.StartTransaction;

            MetaSQLQ.SQL.Text :=
              'SELECT TRIM(RDB$FIELD_NAME) FROM RDB$RELATION_FIELDS ' +
              'WHERE RDB$RELATION_NAME = ' + QuotedStr(Trim(comboboxSourceTables.Text)) +
              ' ORDER BY RDB$FIELD_POSITION';
            MetaSQLQ.Open;
            while not MetaSQLQ.EOF do
            begin
              FieldNamesOrdered.Add(Trim(MetaSQLQ.Fields[0].AsString));
              MetaSQLQ.Next;
            end;
            MetaSQLQ.Close;

            try
              MetaSQLQ.SQL.Text :=
                'SELECT TRIM(RDB$FIELD_NAME), CAST(RDB$COMPUTED_SOURCE AS VARCHAR(8192)) ' +
                'FROM RDB$RELATION_FIELDS ' +
                'WHERE RDB$RELATION_NAME = ' + QuotedStr(Trim(comboboxSourceTables.Text)) +
                ' AND RDB$COMPUTED_BLR IS NOT NULL';
              MetaSQLQ.Open;
              while not MetaSQLQ.EOF do
              begin
                ComputedExprs.Values[Trim(MetaSQLQ.Fields[0].AsString)] :=
                  Trim(MetaSQLQ.Fields[1].AsString);
                MetaSQLQ.Next;
              end;
              MetaSQLQ.Close;
            except
              try MetaSQLQ.Close; except end;
            end;
          finally
            MetaSQLQ.Free;
          end;
        end;
      end
      else if (not FSourceConnForm.IsIBX) and
              (SourceDBMS in [cdPostgreSQL, cdMySQL, cdMSSQL]) then
      begin
        MetaSQLQ := TSQLQuery.Create(nil);
        try
          MetaSQLQ.Database := FSourceConnForm.Conn;
          MetaSQLQ.Transaction := FSourceConnForm.Trans;
          if not FSourceConnForm.Trans.Active then
            FSourceConnForm.Trans.StartTransaction;

          case SourceDBMS of
            cdPostgreSQL:
              MetaSQLQ.SQL.Text :=
                'SELECT column_name, ' +
                '  CASE WHEN is_generated = ''ALWAYS'' THEN 1 ELSE 0 END, ' +
                '  COALESCE(generation_expression, '''') ' +
                'FROM information_schema.columns ' +
                'WHERE table_name = ' +
                QuotedStr(Trim(comboboxSourceTables.Text)) +
                ' ORDER BY ordinal_position';
            cdMySQL:
              MetaSQLQ.SQL.Text :=
                'SELECT column_name, ' +
                '  CASE WHEN extra LIKE ''%GENERATED%'' THEN 1 ELSE 0 END, ' +
                '  COALESCE(generation_expression, '''') ' +
                'FROM information_schema.columns ' +
                'WHERE table_schema = DATABASE() ' +
                'AND table_name = ' +
                QuotedStr(Trim(comboboxSourceTables.Text)) +
                ' ORDER BY ordinal_position';
            cdMSSQL:
              MetaSQLQ.SQL.Text :=
                'SELECT c.name, ' +
                '  CASE WHEN cc.object_id IS NULL THEN 0 ELSE 1 END, ' +
                '  COALESCE(cc.definition, '''') ' +
                'FROM sys.columns c ' +
                'LEFT JOIN sys.computed_columns cc ' +
                '  ON cc.object_id = c.object_id AND cc.column_id = c.column_id ' +
                'WHERE c.object_id = OBJECT_ID(' +
                QuotedStr(Trim(comboboxSourceTables.Text)) + ') ' +
                'ORDER BY c.column_id';
          else
            MetaSQLQ.SQL.Text := 'SELECT 1 WHERE 1=0';
          end;

          MetaSQLQ.Open;
          while not MetaSQLQ.EOF do
          begin
            S := Trim(MetaSQLQ.Fields[0].AsString);
            FieldNamesOrdered.Add(S);
            if MetaSQLQ.Fields[1].AsInteger = 1 then
              ComputedExprs.Values[S] := Trim(MetaSQLQ.Fields[2].AsString);
            MetaSQLQ.Next;
          end;
          MetaSQLQ.Close;
        finally
          MetaSQLQ.Free;
        end;
      end;
    except
      { Metadata-Erkennung fehlgeschlagen — Fallback auf IBQ-FieldName }
    end;

    { ---- Felder laden ---- }
    FUpdatingUI := True;
    try
      if FSourceConnForm.IsIBX then
      begin
        IBQ := FSourceConnForm.IBXQuery;
        IBQ.Close;
        IBQ.SQL.Text := 'SELECT * FROM ' +
          QuoteIdentifier(Trim(comboboxSourceTables.Text), SourceDBMS) +
          ' WHERE 1=0';
        if not FSourceConnForm.IBXTrans.Active then
          FSourceConnForm.IBXTrans.StartTransaction;
        IBQ.Open;
        try
          FieldCount := IBQ.FieldCount;
          for i := 0 to FieldCount - 1 do
          begin
            FieldName := IBQ.Fields[i].FieldName;

            { Echter Name: aus Reihenfolge, nicht aus Lookup }
            if i < FieldNamesOrdered.Count then
              RealName := FieldNamesOrdered[i]
            else
              RealName := FieldName;

            SourceType := GetFieldTypeString(IBQ.Fields[i]);
            AutoType := AutoMapType(SourceType, DestDBMS);

            IsArr := (IBQ.Fields[i].DataType = ftArray) or
                     (IBQ.Fields[i] is TIBArrayField) or
                     (Pos('[', SourceType) > 0);
            IsComputed := Trim(ComputedExprs.Values[RealName]) <> '';
            IsBinaryBlob := (IBQ.Fields[i].DataType = ftBlob);
            IsProblem := IsArr or IsBinaryBlob;

            SetLength(FFields, Length(FFields) + 1);
            FFields[High(FFields)].SourceName     := RealName;
            FFields[High(FFields)].SourceType     := SourceType;
            FFields[High(FFields)].DestName       := RealName;
            FFields[High(FFields)].DestType       := AutoType;
            FFields[High(FFields)].Formula        := '';
            FFields[High(FFields)].IsProblemField := IsProblem;
            FFields[High(FFields)].IsComputed     := IsComputed;
            if IsComputed then
              FFields[High(FFields)].ComputedExpr := ComputedExprs.Values[RealName]
            else
              FFields[High(FFields)].ComputedExpr := '';
            FFields[High(FFields)].Selected       := not IsProblem;

            CheckGroupSourceFields.Items.Add(RealName);
            CheckGroupSourceFields.Checked[i] := not IsProblem;
          end;
        finally
          IBQ.Close;
        end;
      end
      else
      begin
        Q := FSourceConnForm.Query;
        Q.Close;
        Q.SQL.Text := 'SELECT * FROM ' +
          QuoteIdentifier(Trim(comboboxSourceTables.Text), SourceDBMS) +
          ' WHERE 1=0';
        if not FSourceConnForm.Trans.Active then
          FSourceConnForm.Trans.StartTransaction;
        Q.Open;
        try
          FieldCount := Q.FieldCount;
          for i := 0 to FieldCount - 1 do
          begin
            FieldName := Q.Fields[i].FieldName;

            if i < FieldNamesOrdered.Count then
              RealName := FieldNamesOrdered[i]
            else
              RealName := FieldName;

            SourceType := GetFieldTypeString(Q.Fields[i]);
            AutoType := AutoMapType(SourceType, DestDBMS);

            IsArr := (Q.Fields[i].DataType = ftArray) or
                     (Pos('[', SourceType) > 0);
            IsComputed := Trim(ComputedExprs.Values[RealName]) <> '';
            IsBinaryBlob := (Q.Fields[i].DataType = ftBlob);
            IsProblem := IsArr or IsBinaryBlob;

            SetLength(FFields, Length(FFields) + 1);
            FFields[High(FFields)].SourceName     := RealName;
            FFields[High(FFields)].SourceType     := SourceType;
            FFields[High(FFields)].DestName       := RealName;
            FFields[High(FFields)].DestType       := AutoType;
            FFields[High(FFields)].Formula        := '';
            FFields[High(FFields)].IsProblemField := IsProblem;
            FFields[High(FFields)].IsComputed     := IsComputed;
            if IsComputed then
              FFields[High(FFields)].ComputedExpr := ComputedExprs.Values[RealName]
            else
              FFields[High(FFields)].ComputedExpr := '';
            FFields[High(FFields)].Selected       := not IsProblem;

            CheckGroupSourceFields.Items.Add(RealName);
            CheckGroupSourceFields.Checked[i] := not IsProblem;
          end;
        finally
          Q.Close;
        end;
      end;

      UpdateGrid;
      UpdatePreviewSQL;
    finally
      FUpdatingUI := False;
    end;
  finally
    FieldNamesOrdered.Free;
    ComputedExprs.Free;
  end;

  if FProblemFieldsDeselected.Count > 0 then
  begin
    S := '';
    for i := 0 to FProblemFieldsDeselected.Count - 1 do
      S := S + '  • ' + FProblemFieldsDeselected[i] + sLineBreak;

    MessageDlg(
      'The following fields were detected and automatically deselected:' +
      sLineBreak + sLineBreak + S + sLineBreak +
      'Array fields and binary BLOBs cannot be reliably transferred.' +
      sLineBreak + sLineBreak +
      'Computed fields stay selected but no values are inserted.' +
      sLineBreak +
      'All other fields are ready for transfer.',
      mtInformation, [mbOK], 0);
  end;
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

    { Dest-DBMS bestimmen — IBX oder SQLDB }
    if Assigned(FDestConnForm) then
    begin
      if FDestConnForm.IsIBX then
      begin
        if FDestConnForm.IBXConn.Connected then
          DestDBMS := cdFirebird
        else
          DestDBMS := cdUnknown;
      end
      else if Assigned(FDestConnForm.Conn) and FDestConnForm.Conn.Connected then
      begin
        DestDBMS := FDestConnForm.ConnDBMS;
        if DestDBMS = cdUnknown then
          DestDBMS := DBMSFromConnection(FDestConnForm.Conn);
      end
      else
        DestDBMS := cdUnknown;
    end
    else
      DestDBMS := cdUnknown;

    { PickList für Zieltyp-Spalte }
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

procedure TfrmCrossRDBMSMain.SyncFieldsToCheckGroup;
var
  i: Integer;
begin
  for i := 0 to High(FFields) do
    if i < CheckGroupSourceFields.Items.Count then
      CheckGroupSourceFields.Checked[i] := FFields[i].Selected;
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
  { Dest-DBMS bestimmen — IBX oder SQLDB }
  if Assigned(FDestConnForm) then
  begin
    if FDestConnForm.IsIBX then
    begin
      if FDestConnForm.IBXConn.Connected then
        DestDBMS := cdFirebird
      else
        DestDBMS := cdUnknown;
    end
    else if Assigned(FDestConnForm.Conn) and FDestConnForm.Conn.Connected then
    begin
      DestDBMS := FDestConnForm.ConnDBMS;
      if DestDBMS = cdUnknown then
        DestDBMS := DBMSFromConnection(FDestConnForm.Conn);
    end
    else
      DestDBMS := cdUnknown;
  end
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

  { ---- Source ---- }
  if Assigned(FSourceConnForm) then
  begin
    if FSourceConnForm.IsIBX then
    begin
      Result.SourceIsIBX  := True;
      Result.SourceIBXConn  := FSourceConnForm.IBXConn;
      Result.SourceIBXTrans := FSourceConnForm.IBXTrans;
      Result.SourceIBXQuery := FSourceConnForm.IBXQuery;

      { SQLDB-Felder bleiben nil }
      Result.SourceConn  := nil;
      Result.SourceTrans := nil;
      Result.SourceQuery := nil;
    end
    else
    begin
      Result.SourceIsIBX  := False;
      Result.SourceConn   := FSourceConnForm.Conn;
      Result.SourceTrans  := FSourceConnForm.Trans;
      Result.SourceQuery  := FSourceConnForm.Query;

      Result.SourceIBXConn  := nil;
      Result.SourceIBXTrans := nil;
      Result.SourceIBXQuery := nil;
    end;

    Result.SourceBackend := FSourceConnForm.ConnDBMS;
  end;

  { ---- Dest ---- }
  if Assigned(FDestConnForm) then
  begin
    if FDestConnForm.IsIBX then
    begin
      Result.DestIsIBX  := True;
      Result.DestIBXConn  := FDestConnForm.IBXConn;
      Result.DestIBXTrans := FDestConnForm.IBXTrans;
      Result.DestIBXQuery := FDestConnForm.IBXQuery;

      Result.DestConn  := nil;
      Result.DestTrans := nil;
      Result.DestQuery := nil;
    end
    else
    begin
      Result.DestIsIBX  := False;
      Result.DestConn   := FDestConnForm.Conn;
      Result.DestTrans  := FDestConnForm.Trans;
      Result.DestQuery  := FDestConnForm.Query;

      Result.DestIBXConn  := nil;
      Result.DestIBXTrans := nil;
      Result.DestIBXQuery := nil;
    end;

    Result.DestBackend := FDestConnForm.ConnDBMS;
  end;

  { ---- Tabellen / Felder / Optionen ---- }
  Result.SourceTable       := Trim(comboboxSourceTables.Text);
  Result.DestTable         := Trim(edtDestTable.Text);
  Result.Fields            := FFields;
  Result.UseFormulas       := chkboxUseFormulas.Checked;
  Result.CopyData          := chkboxCopyData.Checked;
  Result.IfExists          := TIfExistsAction(cmbIfExists.ItemIndex);
  Result.CommitInterval    := StrToIntDef(edtCommitInterval.Text, 10000);
  Result.BatchSize         := StrToIntDef(edtBatchSize.Text, 500);
  Result.RefreshIntervalMs := StrToIntDef(edtRefreshInterval.Text, 500);

  { ---- Strategie ---- }
  case cmbStrategy.ItemIndex of
    1: Result.Strategy := csRowByRow;
    2: Result.Strategy := csBatched;
    3: Result.Strategy := csInsertSelect;
  else
    Result.Strategy := csRowByRow;   { Auto wird in der Engine entschieden }
  end;

  { ---- Range ---- }
  if rbAllRows.Checked then
  begin
    Result.FromRow := 1;
    Result.ToRow   := 0;
  end
  else
  begin
    Result.FromRow := StrToIntDef(edtFrom.Text, 1);
    Result.ToRow   := StrToIntDef(edtTo.Text, 0);
  end;
end;

function TfrmCrossRDBMSMain.AskIfExistsAction: TIfExistsAction;
var
  Dlg: TForm;
  Lbl: TLabel;
  BtnRecreate, BtnAppend, BtnAbort: TButton;
  BtnWidth, BtnGap, BtnLeft: Integer;
begin
  Result := ieaAbort;

  Dlg := TForm.CreateNew(nil);
  try
    Dlg.Caption := 'Destination Table Exists';
    Dlg.Width := 520;
    Dlg.Height := 180;
    Dlg.Position := poScreenCenter;
    Dlg.BorderStyle := bsDialog;

    Lbl := TLabel.Create(Dlg);
    Lbl.Parent := Dlg;
    Lbl.Left := 16;
    Lbl.Top := 16;
    Lbl.Width := 480;
    Lbl.AutoSize := False;
    Lbl.WordWrap := True;
    Lbl.Height := 60;
    Lbl.Caption :=
      'Destination table "' + Trim(edtDestTable.Text) + '" already exists.' +
      sLineBreak + sLineBreak +
      'What do you want to do?';

    BtnWidth := 140;
    BtnGap := 12;
    { 3 Buttons nebeneinander, zentriert in 520px }
    BtnLeft := (Dlg.Width - (BtnWidth * 3 + BtnGap * 2)) div 2;

    BtnRecreate := TButton.Create(Dlg);
    BtnRecreate.Parent := Dlg;
    BtnRecreate.Caption := 'Recreate';
    BtnRecreate.Left := BtnLeft;
    BtnRecreate.Top := 100;
    BtnRecreate.Width := BtnWidth;
    BtnRecreate.ModalResult := mrYes;

    BtnAppend := TButton.Create(Dlg);
    BtnAppend.Parent := Dlg;
    BtnAppend.Caption := 'Append';
    BtnAppend.Left := BtnLeft + BtnWidth + BtnGap;
    BtnAppend.Top := 100;
    BtnAppend.Width := BtnWidth;
    BtnAppend.ModalResult := mrNo;

    BtnAbort := TButton.Create(Dlg);
    BtnAbort.Parent := Dlg;
    BtnAbort.Caption := 'Abort';
    BtnAbort.Left := BtnLeft + (BtnWidth + BtnGap) * 2;
    BtnAbort.Top := 100;
    BtnAbort.Width := BtnWidth;
    BtnAbort.ModalResult := mrAbort;

    case Dlg.ShowModal of
      mrYes:   Result := ieaRecreate;
      mrNo:    Result := ieaAppend;
      mrAbort: Result := ieaAbort;
    else
      Result := ieaAbort;
    end;
  finally
    Dlg.Free;
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
  SrcOK, DstOK: Boolean;
begin
  UpdatePreviewSQL;

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

  { Source-Verbindung prüfen — IBX oder SQLDB }
  SrcOK := False;
  if Assigned(FSourceConnForm) then
  begin
    if FSourceConnForm.IsIBX then
      SrcOK := Assigned(FSourceConnForm.IBXConn) and FSourceConnForm.IBXConn.Connected
    else
      SrcOK := Assigned(FSourceConnForm.Conn) and FSourceConnForm.Conn.Connected;
  end;

  if not SrcOK then
  begin
    MessageDlg('Source connection is not active.', mtWarning, [mbOK], 0);
    Exit;
  end;

  { Dest-Verbindung prüfen — IBX oder SQLDB }
  DstOK := False;
  if Assigned(FDestConnForm) then
  begin
    if FDestConnForm.IsIBX then
      DstOK := Assigned(FDestConnForm.IBXConn) and FDestConnForm.IBXConn.Connected
    else
      DstOK := Assigned(FDestConnForm.Conn) and FDestConnForm.Conn.Connected;
  end;

  if not DstOK then
  begin
    MessageDlg('Destination connection is not active.', mtWarning, [mbOK], 0);
    Exit;
  end;

  Options := BuildOptions;

  { ---- Pre-Check: Source = Dest (VOR Progress-Fenster) ---- }
  if Options.CopyData then
  begin
    Engine := TCrossTransferEngine.Create(Options);
    try
      if Engine.IsSameTableInSameDatabase then
      begin
        MessageDlg(
          'Source and Destination are the same table in the same database:' +
          sLineBreak + sLineBreak +
          '  Table:    ' + Options.SourceTable + sLineBreak +
          '  Database: ' + Engine.Stats.SourceDatabase + sLineBreak + sLineBreak +
          'This would cause data duplication or a database lock.' + sLineBreak +
          'Please choose a different destination table name.',
          mtError, [mbOK], 0);
        Exit;
      end;
    finally
      Engine.Free;
    end;
  end;


  { IfExists = Ask auflösen }
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

    { Report anzeigen }
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
