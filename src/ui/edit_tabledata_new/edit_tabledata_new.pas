unit edit_tabledata_new;

{$mode ObjFPC}{$H+}

interface

uses
  Math, Variants, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBGrids, DBCtrls, ComCtrls, StdCtrls, DBDateTimePicker, RxDBGrid,
  DB,
  IBArrayGrid,
  IBDatabase,
  IBCustomDataSet,
  IBQuery,
  IBTable,

  turbocommon,
  uthemeselector,
  foreign_key_table;

type

  { TfrmEditTableDataNew }

  TfrmEditTableDataNew = class(TForm)
    DBNavigator1: TDBNavigator;
    dbnavMainTableFormView: TDBNavigator;
    dsMain: TDataSource;
    IBDatabaseMain: TIBDatabase;
    IBTableMain: TIBTable;
    PageControl1: TPageControl;
    pnlRecord: TPanel;
    pnlFKTablesCaption: TPanel;
    pnlMainTable: TPanel;
    pnlMainTableCaption: TPanel;
    RxDBGridMain: TRxDBGrid;
    tsMainTableGrid: TTabSheet;
    tsFormView: TTabSheet;
    transMain: TIBTransaction;
    pnlDetailTables: TPanel;
    Splitter1: TSplitter;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBTableMainAfterScroll(DataSet: TDataSet);
    procedure IBTableMainBeforePost(DataSet: TDataSet);
  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: Integer;
    FTableName: string;
    FDBRec: TDatabaseRec;
    FDBInitialized: boolean;
    FForeignKeyInfoArray: TForeignKeyInfoArray;
    FForeignKeyForms: array of TfrmForeignKeyTable;

    function  InitDB: boolean;
    function  OpenDB: boolean;
    function  CloseDB: boolean;

    procedure CreateForeignKeyForms(
      const AFKArray: TForeignKeyInfoArray);

    function GetForeignKeys(Database: TIBDatabase;
                            TableName: string): TForeignKeyInfoArray;

    procedure ClearDynamicControls;
    procedure RebuildControls;
    procedure CreateDynamicControls;
    function  GetArrayFieldInfo(DB: TIBDatabase; Field: TIBArrayField): string;
    function  IsForeignKeyField(const AFieldName: string): Boolean;
  public
    procedure Init(ANodeInfos: TPNodeInfos; dbIndex: Integer; ATableName: string);
  end;

//var
  //frmEditTableDataNew: TfrmEditTableDataNew;

implementation

function TfrmEditTableDataNew.IsForeignKeyField(const AFieldName: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to High(FForeignKeyInfoArray) do
  begin
    if SameText(FForeignKeyInfoArray[i].ForeignField, AFieldName) then
      Exit(True);
  end;
end;

procedure TfrmEditTableDataNew.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseDB;

  if Assigned(FNodeInfos) then
    FNodeInfos^.EditorForm := nil;

  CloseAction:= caFree;
end;

procedure TfrmEditTableDataNew.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfrmEditTableDataNew.IBTableMainAfterScroll(DataSet: TDataSet);
var
  i: Integer;
  FKForm: TfrmForeignKeyTable;
  FKField: string;
  FKValue: Variant;
begin
  // Alle ForeignKey-Forms durchlaufen
  for i := 0 to High(FForeignKeyForms) do
  begin
    FKForm := FForeignKeyForms[i];

    // Foreign Key Feld in Master-Tabelle
    FKField := FForeignKeyInfoArray[i].ForeignField;

    FKValue := IBTableMain.FieldByName(FKField).Value;

    // Selektiere den passenden Datensatz im FK-Form
    if not VarIsNull(FKValue) then
      FKForm.IBTableForeingKey.Locate(
        FKForm.ForeignKeyInfo.MasterField,
        FKValue,
        []
      );
  end;
end;

{procedure TfrmEditTableDataNew.IBTableMainBeforePost(DataSet: TDataSet);
var
  i: Integer;
  FKField: string;
  FKValue: Variant;
begin
  for i := 0 to High(FForeignKeyForms) do
  begin
    FKField := FForeignKeyForms[i].GetForeignKeyField;
    FKValue := FForeignKeyForms[i].GetSelectedValue;

    if not VarIsNull(FKValue) then
      IBTableMain.FieldByName(FKField).Value := FKValue;
  end;
end;}

procedure TfrmEditTableDataNew.IBTableMainBeforePost(DataSet: TDataSet);
var
  i: Integer;
  FKField: string;
  FKValue: Variant;
  PKValue: Variant;
begin
  if not (DataSet.State in [dsInsert, dsEdit]) then Exit;

  for i := 0 to High(FForeignKeyForms) do
  begin
    FKField := FForeignKeyForms[i].GetForeignKeyField;
    FKValue := FForeignKeyForms[i].GetSelectedValue;

    { Foreign Key setzen }
    if not VarIsNull(FKValue) then
      DataSet.FieldByName(FKField).Value := FKValue;

    { Self Reference prüfen }
    if FForeignKeyInfoArray[i].ForeignTable =
       FForeignKeyInfoArray[i].MasterTable then
    begin
      PKValue := DataSet.FieldByName(
        FForeignKeyInfoArray[i].MasterField).Value;

      FKValue := DataSet.FieldByName(
        FForeignKeyInfoArray[i].ForeignField).Value;

      if not VarIsNull(PKValue) and
         not VarIsNull(FKValue) and
         (PKValue = FKValue) then
      begin
        MessageDlg(
          'Self reference is not allowed.' + sLineBreak +
          'Table: ' + FForeignKeyInfoArray[i].ForeignTable + sLineBreak +
          'Field: ' + FForeignKeyInfoArray[i].ForeignField,
          mtError,
          [mbOK],
          0);

        Abort; // verhindert Post
      end;
    end;
  end;
end;

procedure TfrmEditTableDataNew.Init(ANodeInfos: TPNodeInfos; dbIndex: Integer; ATableName: string);
begin
  FNodeInfos := ANodeInfos;
  FDBIndex   := dbIndex;
  FTableName := ATableName;

  Caption := 'Edit Tabledata ' + QuotedStr(FTableName);

  pnlMainTableCaption.Caption := FTableName;
  FDBInitialized := InitDB;

  if FDBInitialized then
  begin
    if OpenDB then
    begin
      RxDBGridMain.OptimizeColumnsWidthAll;
      //RebuildControls;
      FForeignKeyInfoArray := GetForeignKeys(IBDatabaseMain, FTableName);
      CreateForeignKeyForms(FForeignKeyInfoArray);
    end;
  end;
end;

{procedure TfrmEditTableDataNew.CreateDynamicControls;
var
  i, j, TopPos: Integer;
  fld: TField;
  lbl: TLabel;
  ctrl: TWinControl;
  Edit: TDBEdit;
  Memo: TDBMemo;
  Check: TDBCheckBox;
  IsFK: Boolean;
begin
  // alle bisherigen Controls löschen
  // Nur dynamische Controls löschen, statische behalten
  for i := tsFormView.ControlCount - 1 downto 0 do
  begin
    if (tsFormView.Controls[i].Name <> 'pnlFormFields') and
       (tsFormView.Controls[i].Name <> 'dbnavMainTableFormView') then
      tsFormView.Controls[i].Free;
  end;

  TopPos := 8; // Start oben

  for i := 0 to IBTableMain.FieldCount - 1 do
  begin
    fld := IBTableMain.Fields[i];

    // Label erstellen
    lbl := TLabel.Create(pnlRecord);
    lbl.Parent := pnlRecord;
    lbl.Caption := fld.FieldName;
    lbl.Top := TopPos;
    lbl.Left := 8;
    lbl.Width := 100;
    lbl.Font.Color := clBlack; // Standardfarbe

    ctrl := nil;

    case fld.DataType of
      ftString, ftInteger, ftFloat, ftSmallint, ftLargeint:
        begin
          Edit := TDBEdit.Create(pnlRecord);
          Edit.Parent := pnlRecord;
          Edit.DataSource := dsMain;
          Edit.DataField := fld.FieldName;
          Edit.Top := TopPos;
          Edit.Left := lbl.Left + lbl.Width + 8;
          Edit.Width := 200;
          ctrl := Edit;
        end;

      ftBoolean:
        begin
          Check := TDBCheckBox.Create(pnlRecord);
          Check.Parent := pnlRecord;
          Check.DataSource := dsMain;
          Check.DataField := fld.FieldName;
          Check.Caption := '';
          Check.Top := TopPos;
          Check.Left := lbl.Left + lbl.Width + 8;
          ctrl := Check;
        end;

      ftMemo, ftBlob:
        begin
          Memo := TDBMemo.Create(pnlRecord);
          Memo.Parent := pnlRecord;
          Memo.DataSource := dsMain;
          Memo.DataField := fld.FieldName;
          Memo.Top := TopPos;
          Memo.Left := lbl.Left + lbl.Width + 8;
          Memo.Width := 300;
          Memo.Height := 80;
          ctrl := Memo;
        end;
    end;

    // Prüfen ob Feld ein Foreign Key ist
    IsFK := False;
    for j := 0 to High(FForeignKeyInfoArray) do
    begin
      if fld.FieldName = FForeignKeyInfoArray[j].ForeignField then
      begin
        IsFK := True;
        Break;
      end;
    end;

    if IsFK then
    begin
      if Assigned(ctrl) then
        ctrl.Enabled := False;       // Feld sperren
      lbl.Font.Color := clRed;       // Label rot
    end;

    if Assigned(ctrl) then
      TopPos := TopPos + ctrl.Height + 8
    else
      TopPos := TopPos + 24; // default Höhe
  end;
end;}

procedure TfrmEditTableDataNew.RebuildControls;
begin
  Screen.Cursor := crHourGlass;
  try
    ClearDynamicControls;
    CreateDynamicControls;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmEditTableDataNew.ClearDynamicControls;
var
  i: Integer;
begin
  dsMain.DataSet.DisableControls;
  //dsMain.DataSet := nil;
  try
    for i := tsFormView.ControlCount - 1 downto 0 do
    begin
      if (tsFormView.Controls[i].Name <> 'pnlRecord') and
         (tsFormView.Controls[i].Name <> 'dbnavMainTableFormView') then
        tsFormView.Controls[i].Free;
    end;
  finally
    //dsMain.DataSet := IBTableMain;
    dsMain.DataSet.EnableControls;
  end;
end;

procedure TfrmEditTableDataNew.CreateDynamicControls;
var
  ALabel: TLabel;
  ADBEdit: TDBEdit;
  ADBMemo: TDBMemo;
  ADBDateTime: TDBDateTimePicker;
  AArrayGrid: TIBArrayGrid;
  i, AWidth, VSpacing: Integer;
  ATop: Integer;
begin

  ATop := 20;
  VSpacing := 10; // vertikaler Abstand zwischen Controls

  for i := 0 to IBTableMain.FieldCount - 1 do
  begin
    // Label für Feldnamen
    ALabel := TLabel.Create(pnlRecord);
    ALabel.Parent := pnlRecord;
    ALabel.Left := 20;
    ALabel.Top := ATop + VSpacing;
    ALabel.Caption := IBTableMain.Fields[i].FieldName;

    // FK Felder rot markieren
    if IsForeignKeyField(IBTableMain.Fields[i].FieldName) then
    begin
      ALabel.Font.Color := clRed;
      ALabel.Font.Style := [fsBold];
    end;

    // Prüfen auf Array-Feld
    if IBTableMain.Fields[i] is TIBArrayField then
    begin
      with TIBArrayField(IBTableMain.Fields[i]) do
      begin
        AArrayGrid := TIBArrayGrid.Create(pnlRecord);
        AArrayGrid.Parent := pnlRecord;
        AArrayGrid.Left := 160;
        AArrayGrid.Top := ATop + VSpacing;
        AArrayGrid.Anchors := [akLeft, akTop, akRight];

        if IsForeignKeyField(FieldName) then
          AArrayGrid.Enabled := False;

        // 1D-Array: eine Zeile, mehrere Spalten
        if ArrayDimensions = 1 then
        begin
          AArrayGrid.RowCount := 1;
          AArrayGrid.ColCount := ArrayBounds[0].UpperBound - ArrayBounds[0].LowerBound + 1;
          AArrayGrid.DefaultColWidth := Max(50, Min(80, 400 div AArrayGrid.ColCount));
          AArrayGrid.DefaultRowHeight := AArrayGrid.DefaultRowHeight + 20;
          AArrayGrid.Width := AArrayGrid.DefaultColWidth * AArrayGrid.ColCount;
          AArrayGrid.Height := AArrayGrid.DefaultRowHeight + 8;
        end
        // 2D-Array: klassisches Grid
        else if ArrayDimensions = 2 then
        begin
          AArrayGrid.ColCount := ArrayBounds[1].UpperBound - ArrayBounds[1].LowerBound + 1;
          AArrayGrid.RowCount := ArrayBounds[0].UpperBound - ArrayBounds[0].LowerBound + 1;
          AArrayGrid.DefaultColWidth := Max(50, Min(80, 400 div AArrayGrid.ColCount));
          AArrayGrid.Width := AArrayGrid.DefaultColWidth * AArrayGrid.ColCount;
          AArrayGrid.Height := AArrayGrid.DefaultRowHeight * AArrayGrid.RowCount + 8;
        end
        else
        begin
          AArrayGrid.ColCount := 1;
          AArrayGrid.RowCount := 1;
          AArrayGrid.Cells[0,0] := Format('%d-dim Array', [ArrayDimensions]);
          AArrayGrid.Width  := 200;
          AArrayGrid.Height := 30;
        end;

        // DataSource + DataField setzen
        AArrayGrid.DataSource := dsMain;
        AArrayGrid.DataField := FieldName;

        Inc(ATop, AArrayGrid.Height + 2); // Abstand für Label

        // Array Info-Label
        //ALabel := TLabel.Create(pnlRecord);
        //ALabel.Parent := pnlRecord;
        ALabel.Caption := ALabel.Caption +
                          sLineBreak + '(' +
                          GetArrayFieldInfo(IBTableMain.Database, TIBArrayField(IBTableMain.Fields[i])) + ')';

        AArrayGrid.Left := ALabel.Left + ALabel.Width + 150;;
        AArrayGrid.Width := AArrayGrid.Width - 70;
        //ALabel.Left := AArrayGrid.Left + AArrayGrid.Width + 10;
        //ALabel.Caption := GetArrayFieldInfo(ibtblForm.Database, TIBArrayField(ibtblForm.Fields[i]));
        //ALabel.Top := ATop;
        inc(ATop, 20);
        //Inc(ATop, ALabel.Height + VSpacing);
      end;

      Continue; // nächstes Feld
    end;

    // Normale Feldtypen
    case IBTableMain.Fields[i].DataType of
      ftBlob, ftMemo:
        begin
          ADBMemo := TDBMemo.Create(pnlRecord);
          ADBMemo.Parent := pnlRecord;
          ADBMemo.Left := 160;
          ADBMemo.Top := ATop + VSpacing;
          ADBMemo.Width := 400;
          ADBMemo.Height := 200;
          ADBMemo.Anchors := [akLeft, akTop, akRight];
          ADBMemo.ScrollBars := ssBoth;
          ADBMemo.DataSource := dsMain;
          ADBMemo.DataField := IBTableMain.Fields[i].FieldName;

          Inc(ATop, ADBMemo.Height + VSpacing * 2);
        end;

      ftDate, ftTime, ftDateTime:
        begin
          ADBDateTime := TDBDateTimePicker.Create(pnlRecord);
          ADBDateTime.Parent := pnlRecord;
          ADBDateTime.Left := 160;
          ADBDateTime.Top := ATop + VSpacing;
          ADBDateTime.Width := 160;
          ADBDateTime.DataSource := dsMain;
          ADBDateTime.DataField := IBTableMain.Fields[i].FieldName;

          if IsForeignKeyField(IBTableMain.Fields[i].FieldName) then
            ADBDateTime.Enabled := False;

          Inc(ATop, ADBDateTime.Height + VSpacing);
        end;

      else
        begin
          ADBEdit := TDBEdit.Create(pnlRecord);
          ADBEdit.Parent := pnlRecord;
          ADBEdit.Left := 160;
          ADBEdit.Top := ATop + VSpacing;
          ADBEdit.DataSource := dsMain;
          ADBEdit.DataField := IBTableMain.Fields[i].FieldName;

          if IsForeignKeyField(IBTableMain.Fields[i].FieldName) then
            ADBEdit.Enabled := False;

          AWidth := 80;
          if IBTableMain.Fields[i].DataType = ftString then
            AWidth := IBTableMain.Fields[i].DataSize * 10;
          if AWidth > 400 then
            AWidth := 400;
          ADBEdit.Width := AWidth;

          Inc(ATop, ADBEdit.Height + VSpacing);
        end;
    end;
  end;

  Height := ATop + VSpacing * 2;
end;

function TfrmEditTableDataNew.InitDB: boolean;
var
  user, password: string;
begin
  if  IBDatabaseMain.Connected then
    exit(true);

  Result := False;

  try
    FDBRec := RegisteredDatabases[FDBIndex];

    IBDatabaseMain.Params.Clear;
    IBDatabaseMain.LoginPrompt := False;

    // Retrieve username and password from the database configuration
    user := FDBRec.IBDatabase.Params.Values['user_name'];
    password := FDBRec.IBDatabase.Params.Values['password'];

    // Set parameters correctly
    IBDatabaseMain.Params.Values['user_name'] := user;
    IBDatabaseMain.Params.Values['password'] := password;

    IBDatabaseMain.DatabaseName := FDBRec.IBDatabase.DatabaseName;
    IBDatabaseMain.FirebirdLibraryPathName := FDBRec.IBDatabase.FirebirdLibraryPathName;

    // Test connection
    IBDatabaseMain.Connected := True;
    IBDatabaseMain.Connected := False;

    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Error initializing database:' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

function TfrmEditTableDataNew.OpenDB: boolean;
begin
  try
    if not IBDatabaseMain.Connected then
      IBDatabaseMain.Connected := true;

    if not transMain.InTransaction then
      transMain.StartTransaction;

    if IBTableMain.Active then
      IBTableMain.Close;

    IBTableMain.TableName := FTableName;
    IBTableMain.Open;

    result := true;
  except
    result := false;
  end;
end;

function TfrmEditTableDataNew.CloseDB: boolean;
begin
  try
    if IBTableMain.Active then
      IBTableMain.Close;

    if transMain.InTransaction then
      transMain.Commit;

    if IBDatabaseMain.Connected then
      IBDatabaseMain.Connected := false;

    result := true;
  except
    result := false;
  end;
end;

procedure TfrmEditTableDataNew.CreateForeignKeyForms(
  const AFKArray: TForeignKeyInfoArray);
var
  i: Integer;
  FKForm: TfrmForeignKeyTable;
  FKPanel: TPanel;
  Split: TSplitter;
  NumForms: Integer;
  CalcHeight: Integer;
  CurrentTop: Integer;
begin
  SetLength(FForeignKeyForms, Length(AFKArray));
  NumForms := Length(AFKArray);

  if NumForms > 0 then
    CalcHeight := (pnlDetailTables.Height - pnlFKTablesCaption.Height) div NumForms
  else
    CalcHeight := 0;

  CurrentTop := pnlFKTablesCaption.Height; // Start direkt unter Caption

  for i := 0 to High(AFKArray) do
  begin
    // Panel für Foreign Key Form
    FKPanel := TPanel.Create(Self);
    FKPanel.Parent := pnlDetailTables;
    FKPanel.BevelOuter := bvNone;

    if i = High(AFKArray) then
      FKPanel.Align := alClient // Letztes Panel füllt Resthöhe
    else
    begin
      FKPanel.Align := alTop;
      FKPanel.Top := CurrentTop;
      FKPanel.Height := CalcHeight;
    end;

    // Foreign Key Form einfügen
    FKForm := TfrmForeignKeyTable.Create(FKPanel);
    FKForm.Parent := FKPanel;
    FKForm.BorderStyle := bsNone;
    FKForm.Align := alClient;

    FKForm.Init(
      IBDatabaseMain,
      transMain,
      AFKArray[i]
    );

    FKForm.Visible := True;
    FForeignKeyForms[i] := FKForm;

    // Splitter zwischen Panels (außer letztes)
    if i < High(AFKArray) then
    begin
      Split := TSplitter.Create(Self);
      Split.Parent := pnlDetailTables;
      Split.Align := alTop;
      Split.Height := 4;
      Split.Cursor := crVSplit;

      // Top berechnen: direkt unter FKPanel
      Split.Top := FKPanel.Top + FKPanel.Height;

      // Update CurrentTop für nächstes Panel
      CurrentTop := Split.Top + Split.Height;
    end;
  end;

  pnlFKTablesCaption.Top := 0;
  pnlDetailTables.Visible := (NumForms > 0);
end;

function TfrmEditTableDataNew.GetForeignKeys(Database: TIBDatabase;
                        TableName: string): TForeignKeyInfoArray;
var
  Q: TIBQuery;
  Count: Integer;
begin
  SetLength(Result, 0);

  Q := TIBQuery.Create(nil);
  try
    Q.Database := Database;
    Q.Transaction := Database.DefaultTransaction;

    Q.SQL.Text :=
      'SELECT ' +
      '  TRIM(rc.RDB$CONSTRAINT_NAME) AS FK_NAME, ' +
      '  TRIM(fk_rel.RDB$RELATION_NAME) AS FK_TABLE, ' +
      '  TRIM(fk_seg.RDB$FIELD_NAME) AS FK_FIELD, ' +
      '  TRIM(pk_rel.RDB$RELATION_NAME) AS PK_TABLE, ' +
      '  TRIM(pk_seg.RDB$FIELD_NAME) AS PK_FIELD ' +
      'FROM RDB$RELATION_CONSTRAINTS rc ' +
      'JOIN RDB$REF_CONSTRAINTS ref ' +
      '  ON rc.RDB$CONSTRAINT_NAME = ref.RDB$CONSTRAINT_NAME ' +
      'JOIN RDB$RELATION_CONSTRAINTS pk ' +
      '  ON ref.RDB$CONST_NAME_UQ = pk.RDB$CONSTRAINT_NAME ' +
      'JOIN RDB$INDEX_SEGMENTS fk_seg ' +
      '  ON rc.RDB$INDEX_NAME = fk_seg.RDB$INDEX_NAME ' +
      'JOIN RDB$INDEX_SEGMENTS pk_seg ' +
      '  ON pk.RDB$INDEX_NAME = pk_seg.RDB$INDEX_NAME ' +
      'JOIN RDB$RELATIONS fk_rel ' +
      '  ON fk_rel.RDB$RELATION_NAME = rc.RDB$RELATION_NAME ' +
      'JOIN RDB$RELATIONS pk_rel ' +
      '  ON pk_rel.RDB$RELATION_NAME = pk.RDB$RELATION_NAME ' +
      'WHERE rc.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' ' +
      'AND TRIM(fk_rel.RDB$RELATION_NAME) = :TableName ' +
      'ORDER BY TRIM(fk_rel.RDB$RELATION_NAME)';

    Q.ParamByName('TableName').AsString := UpperCase(TableName);

    Q.Open;

    Count := 0;

    while not Q.EOF do
    begin
      SetLength(Result, Count + 1);

      Result[Count].ConstraintName :=
        Trim(Q.FieldByName('FK_NAME').AsString);

      Result[Count].ForeignTable :=
        Trim(Q.FieldByName('FK_TABLE').AsString);

      Result[Count].ForeignField :=
        Trim(Q.FieldByName('FK_FIELD').AsString);

      Result[Count].MasterTable :=
        Trim(Q.FieldByName('PK_TABLE').AsString);

      Result[Count].MasterField :=
        Trim(Q.FieldByName('PK_FIELD').AsString);

      Inc(Count);
      Q.Next;
    end;

  finally
    Q.Free;
  end;
end;

function TfrmEditTableDataNew.GetArrayFieldInfo(DB: TIBDatabase; Field: TIBArrayField): string;
var
  MetaQuery, DimQuery: TIBQuery;
  TableName, FieldName, TypeName: string;
  FieldType, FieldSubType, Dimensions, FieldLength: Integer;
  DimStr: string;
begin
  Result := '';
  FieldName := Field.FieldName;

  // Wir brauchen den Tabellen-Namen
  if Field.DataSet is TIBTable then
    TableName := TIBTable(Field.DataSet).TableName
  else
    Exit;

  MetaQuery := TIBQuery.Create(nil);
  DimQuery := TIBQuery.Create(nil);
  try
    MetaQuery.Database := DB;
    DimQuery.Database := DB;

    // 1. Feldmetadaten holen
    MetaQuery.SQL.Text :=
      'SELECT f.RDB$FIELD_NAME AS TYPENAME, f.RDB$FIELD_TYPE, f.RDB$FIELD_SUB_TYPE, ' +
      'f.RDB$DIMENSIONS, f.RDB$FIELD_LENGTH ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'WHERE rf.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(TableName)) +
      ' AND rf.RDB$FIELD_NAME = ' + QuotedStr(UpperCase(FieldName));
    MetaQuery.Open;
    if MetaQuery.EOF then Exit;

    TypeName := Trim(MetaQuery.FieldByName('TYPENAME').AsString);
    FieldType := MetaQuery.FieldByName('RDB$FIELD_TYPE').AsInteger;
    FieldSubType := MetaQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
    Dimensions := MetaQuery.FieldByName('RDB$DIMENSIONS').AsInteger;
    FieldLength := MetaQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger; // <-- jetzt verfügbar

    // 2. Dimensionen abfragen
    DimQuery.SQL.Text :=
      'SELECT RDB$LOWER_BOUND, RDB$UPPER_BOUND ' +
      'FROM RDB$FIELD_DIMENSIONS ' +
      'WHERE RDB$FIELD_NAME = ' + QuotedStr(TypeName) +
      ' ORDER BY RDB$DIMENSION';
    DimQuery.Open;

    DimStr := '';
    while not DimQuery.EOF do
    begin
      if DimStr <> '' then DimStr := DimStr + ' x ';
      DimStr := DimStr + Format('%d..%d', [
        DimQuery.FieldByName('RDB$LOWER_BOUND').AsInteger,
        DimQuery.FieldByName('RDB$UPPER_BOUND').AsInteger
      ]);
      DimQuery.Next;
    end;

    // 3. Datentyp bestimmen
    case FieldType of
      7: Result := Format('Array[%s] of SMALLINT', [DimStr]);       // SMALLINT
      8: Result := Format('Array[%s] of INTEGER', [DimStr]);        // INTEGER
      10: Result := Format('Array[%s] of FLOAT', [DimStr]);         // FLOAT
      12: Result := Format('Array[%s] of DATE', [DimStr]);          // DATE/TIME
      14: Result := Format('Array[%s] of CHAR', [DimStr]);          // CHAR
      37: // VARCHAR
        begin
          // Länge ermitteln
          Result := Format('Array[%s] of VARCHAR(%d)', [DimStr, MetaQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger]);
        end;
      261: Result := Format('Array[%s] of BLOB', [DimStr]);         // BLOB
      else Result := Format('Array[%s] of UNKNOWN', [DimStr]);
    end;

  finally
    MetaQuery.Free;
    DimQuery.Free;
  end;
end;


{$R *.lfm}

end.

