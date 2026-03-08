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
    pnlFKTablesCaption: TPanel;
    pnlMainTable: TPanel;
    pnlMainTableCaption: TPanel;
    RxDBGridMain: TRxDBGrid;
    pnlRecord: TScrollBox;
    tsMainTableGrid: TTabSheet;
    tsFormView: TTabSheet;
    transMain: TIBTransaction;
    pnlDetailTables: TPanel;
    Splitter1: TSplitter;
    procedure dsMainStateChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBTableMainAfterInsert(DataSet: TDataSet);
    procedure IBTableMainAfterScroll(DataSet: TDataSet);
    procedure IBTableMainBeforePost(DataSet: TDataSet);
    procedure IBTableMainBeforeRefresh(DataSet: TDataSet);
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
                            ATableName: string): TForeignKeyInfoArray;

    procedure ClearDynamicControls;
    procedure RebuildControls;
    procedure CreateDynamicControls;
    function  IsForeignKeyField(const AFieldName: string): Boolean;

    procedure LoacateForeignKeyTablesRecord(DataSet: TDataSet);
  public
    procedure Init(ANodeInfos: TPNodeInfos; dbIndex: Integer; ATableName: string);
  end;

//var
  //frmEditTableDataNew: TfrmEditTableDataNew;

implementation

procedure TfrmEditTableDataNew.Init(ANodeInfos: TPNodeInfos; dbIndex: Integer; ATableName: string);
begin
  if FDBInitialized then Exit;

  FNodeInfos := ANodeInfos;
  FDBIndex   := dbIndex;
  //FTableName := MakeCaseSensitiveAuto(ATableName);
  FTableName := ATableName;

  Caption := 'Edit Tabledata ' + QuotedStr(FTableName);
  pnlMainTableCaption.Caption := FTableName;

  FDBInitialized := InitDB;
  if not FDBInitialized then Exit;

  FDBInitialized := OpenDB;

  if FDBInitialized then
  begin
    //  Foreign Key Infos holen
    FForeignKeyInfoArray := GetForeignKeys(IBDatabaseMain, FTableName);

    // Dynamische FK-Forms erstellen
    CreateForeignKeyForms(FForeignKeyInfoArray);

    //  Controls auf Basis der Tabelle erzeugen
    RebuildControls;

    LoacateForeignKeyTablesRecord(IBTableMain);

  end else
    exit;
end;

function TfrmEditTableDataNew.InitDB: boolean;
var user, password: string;
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

    RxDBGridMain.OptimizeColumnsWidthAll;

    result := true;
  except
    on E: Exception do
    begin
      MessageDlg('Error Open database:' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
      Result := False;
    end;
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
    on E: Exception do
    begin
      MessageDlg('Error Close database:' + sLineBreak + E.Message,
                 mtError, [mbOK], 0);
      Result := False;
    end;
  end;
end;

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
var i: Integer;
begin
  dsMain.DataSet.DisableControls;
  try
    for i := pnlRecord.ControlCount - 1 downto 0 do
      pnlRecord.Controls[i].Free;
  finally
    dsMain.DataSet.EnableControls;
  end;
end;

procedure TfrmEditTableDataNew.IBTableMainBeforeRefresh(DataSet: TDataSet);
begin
  CloseDB;
  OpenDB;
  Abort;
end;

procedure TfrmEditTableDataNew.IBTableMainAfterScroll(DataSet: TDataSet);
begin
  LoacateForeignKeyTablesRecord(DataSet);
end;

procedure TfrmEditTableDataNew.IBTableMainBeforePost(DataSet: TDataSet);
var
  i, j: Integer;
  FKForm: TfrmForeignKeyTable;
  FKFields: TStringList;
  FKValues: Variant;
begin
  if not Assigned(FForeignKeyForms) then Exit;

  for i := 0 to High(FForeignKeyForms) do
  begin
    FKForm := FForeignKeyForms[i];
    if not Assigned(FKForm) then Continue;
    if not Assigned(FKForm.IBTableForeingKey) then Continue;
    if not FKForm.IBTableForeingKey.Active then Continue;

    FKFields := TStringList.Create;
    try
      FKFields.Delimiter := ';';
      FKFields.StrictDelimiter := True;
      FKFields.DelimitedText := FForeignKeyInfoArray[i].ForeignFields;

      FKValues := FKForm.GetSelectedValues;

      if VarIsArray(FKValues) and (VarArrayHighBound(FKValues,1) >= 0) then
      begin
        for j := 0 to FKFields.Count - 1 do
        begin
          if (Trim(FKFields[j]) <> '') and Assigned(DataSet.FieldByName(Trim(FKFields[j]))) then
            DataSet.FieldByName(Trim(FKFields[j])).Value := FKValues[j];
        end;
      end;

    finally
      FKFields.Free;
    end;
  end;
end;

function TfrmEditTableDataNew.IsForeignKeyField(
  const AFieldName: string): Boolean;
var
  i, j: Integer;
  FieldList: TStringList;
begin
  Result := False;

  for i := 0 to High(FForeignKeyInfoArray) do
  begin
    FieldList := TStringList.Create;
    try
      FieldList.Delimiter := ';';
      FieldList.StrictDelimiter := True;
      FieldList.DelimitedText :=
        FForeignKeyInfoArray[i].ForeignFields;

      for j := 0 to FieldList.Count - 1 do
      begin
        if SameText(
             Trim(FieldList[j]),
             AFieldName
           ) then
          Exit(True);
      end;

    finally
      FieldList.Free;
    end;
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

procedure TfrmEditTableDataNew.dsMainStateChange(Sender: TObject);
begin
  if Assigned(dsMain) and Assigned(dsMain.DataSet) then
  begin
    pnlDetailTables.Enabled := (dsMain.DataSet.State = dsBrowse);
  end;
end;

procedure TfrmEditTableDataNew.FormCreate(Sender: TObject);
begin
  FDBInitialized := false;
end;

procedure TfrmEditTableDataNew.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfrmEditTableDataNew.IBTableMainAfterInsert(DataSet: TDataSet);
var
  i: Integer;
begin
  {for i := 0 to DataSet.FieldCount - 1 do
    if DataSet.Fields[i] is TIBArrayField then
    begin
      AArrayGrid.DataSource := nil;
    end;}
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
      //ALabel.Font.Style := [fsBold];
      ALabel.Caption := ALabel.Caption + ' (Foreign Key)';
    end;

    // Prüfen auf Array-Feld
    if IBTableMain.Fields[i] is TIBArrayField then
    begin
      with TIBArrayField(IBTableMain.Fields[i]) do
      begin
        AArrayGrid := TIBArrayGrid.Create(pnlRecord);
        AArrayGrid.Hint := 'Firebird Array Field';
        AArrayGrid.ShowHint := true;
        AArrayGrid.Parent := pnlRecord;
        AArrayGrid.Left := 230;
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
          ADBMemo.Left := 230;
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
          ADBDateTime.Left := 230;
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
          ADBEdit.Left := 230;
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

function TfrmEditTableDataNew.GetForeignKeys(
  Database: TIBDatabase;
  ATableName: string): TForeignKeyInfoArray;
var
  Q: TIBQuery;
  CurrentConstraint: string;
  Index: Integer;
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
      '  AND fk_seg.RDB$FIELD_POSITION = pk_seg.RDB$FIELD_POSITION ' +
      'JOIN RDB$RELATIONS fk_rel ' +
      '  ON fk_rel.RDB$RELATION_NAME = rc.RDB$RELATION_NAME ' +
      'JOIN RDB$RELATIONS pk_rel ' +
      '  ON pk_rel.RDB$RELATION_NAME = pk.RDB$RELATION_NAME ' +
      'WHERE rc.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' ' +
      'AND TRIM(fk_rel.RDB$RELATION_NAME) = :TableName ' +
      'ORDER BY rc.RDB$CONSTRAINT_NAME, fk_seg.RDB$FIELD_POSITION';


    Q.ParamByName('TableName').AsString := ATableName;
    Q.Open;

    CurrentConstraint := '';
    Index := -1;

    while not Q.EOF do
    begin
      if CurrentConstraint <> Q.FieldByName('FK_NAME').AsString then
      begin
        CurrentConstraint := Q.FieldByName('FK_NAME').AsString;
        Inc(Index);
        SetLength(Result, Index + 1);

        Result[Index].ConstraintName := CurrentConstraint;
        Result[Index].ForeignTable :=
          Q.FieldByName('FK_TABLE').AsString;
        Result[Index].MasterTable :=
          Q.FieldByName('PK_TABLE').AsString;
        Result[Index].ForeignFields := '';
        Result[Index].MasterFields := '';
      end;

      if Result[Index].ForeignFields <> '' then
      begin
        Result[Index].ForeignFields :=
          Result[Index].ForeignFields + ';';
        Result[Index].MasterFields :=
          Result[Index].MasterFields + ';';
      end;

      Result[Index].ForeignFields :=
        Result[Index].ForeignFields +
        Q.FieldByName('FK_FIELD').AsString;

      Result[Index].MasterFields :=
        Result[Index].MasterFields +
        Q.FieldByName('PK_FIELD').AsString;

      Q.Next;
    end;

  finally
    Q.Free;
  end;
end;

procedure TfrmEditTableDataNew.LoacateForeignKeyTablesRecord(DataSet: TDataSet);
var
  i, j: Integer;
  FKForm: TfrmForeignKeyTable;
  FKFields: TStringList;
  FKValues: Variant;
begin
  if not Assigned(FForeignKeyForms) then Exit;

  for i := 0 to High(FForeignKeyForms) do
  begin
    FKForm := FForeignKeyForms[i];
    if not Assigned(FKForm) then Continue;
    if not Assigned(FKForm.IBTableForeingKey) then Continue;

    // FK-Tabelle öffnen, falls nicht aktiv
    if not FKForm.IBTableForeingKey.Active then
    begin
      try
        FKForm.IBTableForeingKey.Open;
        FKForm.RxDBGrid1.OptimizeColumnsWidthAll;
      except
        on E: Exception do
          Continue; // Fehler ignorieren, Crash vermeiden
      end;
    end;

    FKFields := TStringList.Create;
    try
      FKFields.Delimiter := ';';
      FKFields.StrictDelimiter := True;
      FKFields.DelimitedText := FForeignKeyInfoArray[i].ForeignFields;

      if FKFields.Count = 0 then Continue;

      // Variant-Array for Locate
      FKValues := VarArrayCreate([0, FKFields.Count - 1], varVariant);
      for j := 0 to FKFields.Count - 1 do
      begin
        if Assigned(DataSet.FieldByName(Trim(FKFields[j]))) then
          FKValues[j] := DataSet.FieldByName(Trim(FKFields[j])).Value
        else
          FKValues[j] := Null;
      end;

      if VarIsArray(FKValues) and (VarArrayHighBound(FKValues,1) >= 0) and not VarIsNull(FKValues[0]) then
      begin
        try
          FKForm.IBTableForeingKey.Locate(
            FForeignKeyInfoArray[i].MasterFields,
            FKValues,
            []
          );
        except
          on E: Exception do
          begin
            MessageDlg('Error Locate:' + sLineBreak + E.Message,
                       mtError, [mbOK], 0);
          end;
        end;
      end;

    finally
      FKFields.Free;
    end;
  end;
end;

{$R *.lfm}

end.

