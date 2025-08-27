unit fedittabledata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, db, sqldb, IBConnection, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, DbCtrls, DBGrids, StdCtrls, ComCtrls,
  Buttons, Menus, DBDateTimePicker, IBDynamicGrid, DBControlGrid, IBQuery,
  IBDatabase, IBTable, IBArrayGrid, IBCustomDataSet, IB, turbocommon, calen,
  dmibx;


type

  { TfmEditTable }

  TfmEditTable = class(TForm)
    btnSearch: TButton;
    cboxFields: TComboBox;
    cboxTables: TComboBox;
    chkBoxCasesensitive: TCheckBox;
    chkBoxUseFilter: TCheckBox;
    dbnavForm: TDBNavigator;
    dsForm: TDataSource;
    dsGrid: TDatasource;
    dbnavGrid: TDBNavigator;
    edtSearch: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    IBDatabase1: TIBDatabase;
    IBDynamicGrid2: TIBDynamicGrid;
    ibtblForm: TIBTable;
    ibtblGrid: TIBTable;
    ibtransForm: TIBTransaction;
    ibtransGrid: TIBTransaction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lmExport: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlRecord: TScrollBox;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    tsGridView: TTabSheet;
    tsFormView: TTabSheet;
    procedure CurrentDateClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure cboxTablesChange(Sender: TObject);
    procedure chkBoxCasesensitiveChange(Sender: TObject);
    procedure chkBoxUseFilterChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ibtblFormAfterPost(DataSet: TDataSet);
    procedure ibtblGridAfterOpen(DataSet: TDataSet);
    procedure ibtblGridAfterScroll(DataSet: TDataSet);
    procedure ibtblGridBeforePost(DataSet: TDataSet);
    procedure lmExportClick(Sender: TObject);
    procedure tsFormViewShow(Sender: TObject);
    procedure tsGridViewShow(Sender: TObject);
  private
    { private declarations }
    procedure FillTablesCombobox;
    procedure FillFieldsCombobox;
    procedure SetTableFilter;
    procedure OpenTable(ATableName: string);
    procedure CloseTable;
    procedure GenerateFormView;
    procedure RemoveDynamicControls;
    function  GetArrayFieldInfo(DB: TIBDatabase; Field: TIBArrayField): string;
    procedure  SetWindowDimensions;
    procedure RefreshTable(ATable: TIBTable);
  public
    { public declarations }
    Rec: TDatabaseRec;
    procedure Init(dbIndex: Integer; ATableName: string);
  end;

var
  fmEditTable: TfmEditTable;

implementation

{ TfmEditTable }

procedure TfmEditTable.FillTablesCombobox;
var i: integer;
begin
  IBDatabase1.GetTableNames(cboxTables.Items);
end;

procedure TfmEditTable.FillFieldsCombobox;
var i: integer;
begin
  ibtblGrid.Fields.GetFieldNames(cboxFields.Items);
  if cboxFields.Items.Count > 0 then
    cboxFields.ItemIndex := 0;
end;

procedure TfmEditTable.cboxTablesChange(Sender: TObject);
begin
  chkBoxCasesensitive.OnChange := nil;
  chkBoxUseFilter.OnChange := nil;

  chkBoxCasesensitive.Checked := false;
  chkBoxUseFilter.Checked := false;

  CloseTable;
  cboxFields.Items.Clear;
  ibtblGrid.TableName := cboxTables.Items[cboxTables.ItemIndex];
  OpenTable(ibtblGrid.TableName);
  FillFieldsCombobox;

  chkBoxUseFilter.OnChange := @chkBoxUseFilterChange;
  chkBoxCasesensitive.OnChange := @chkBoxCasesensitiveChange;
  Self.Caption := 'Edit Table: ' + ibtblGrid.TableName;
end;

procedure TfmEditTable.SetTableFilter;
begin
  if chkBoxCasesensitive.Checked then
    ibtblGrid.Filter :=
      Format('%s LIKE ''%s%%''', [cboxFields.Text, edtSearch.Text])
  else
    ibtblGrid.Filter :=
      Format('UPPER(%s) LIKE ''%s%%''', [cboxFields.Text, UpperCase(edtSearch.Text)]);
end;

procedure TfmEditTable.chkBoxUseFilterChange(Sender: TObject);
begin
  if chkBoxUseFilter.Checked then
    SetTableFilter;
  ibtblGrid.Filtered := chkBoxUseFilter.Checked;
end;

procedure TfmEditTable.btnSearchClick(Sender: TObject);
var
  Options: TLocateOptions;
begin
  if (cboxFields.ItemIndex >= 0) and (Trim(edtSearch.Text) <> '') then
  begin
    if ibtblGrid.Filtered then
    begin
      ibtblGrid.Filtered := False;
      SetTableFilter;
      ibtblGrid.Filtered := True;
    end else
    begin
      Options := [loPartialKey];
      if not chkBoxCasesensitive.Checked then
          Options := Options + [loCaseInsensitive];

      if not ibtblGrid.Locate(cboxFields.Text, edtSearch.Text, Options) then
        ShowMessage('No records found!');
    end;
  end
  else
    ShowMessage('Please specify field and search value');
end;

procedure TfmEditTable.chkBoxCasesensitiveChange(Sender: TObject);
begin
  //if ibtblGrid.Filtered then
    btnSearchClick(nil);
end;

procedure TfmEditTable.CloseTable;
begin
  ibtblGrid.Filter := '';
  ibtblGrid.Filtered := false;

  if ibtblGrid.Active then
    ibtblGrid.Close;

  if ibtransGrid.InTransaction then
  begin
    try
      ibtransGrid.Commit;   // oder Rollback
    except
      ibtransGrid.Rollback;
    end;
  end;

end;

procedure TfmEditTable.OpenTable(ATableName: string);
begin
  if not IBDatabase1.Connected then
    IBDatabase1.Connected := true;

  if not ibtransGrid.InTransaction then
    ibtransGrid.StartTransaction;

  ibtblGrid.TableName := ATableName;
  ibtblGrid.Open;

  //ibtblForm.TableName := ATableName;
  //ibtblForm.Open;

end;

procedure TfmEditTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseTable;
  if IBDatabase1.Connected then
    IBDatabase1.Connected := False;
  CloseAction := caFree;
end;

procedure TfmEditTable.RefreshTable(ATable: TIBTable);
var TmpRecNo: int64;
begin
  TmpRecNo := ibtblGrid.RecNo;
  ibtransGrid.Commit;
  IBDatabase1.Connected := false;
  ibtblGrid.Close;
  dsGrid.Enabled := false;

  IBDatabase1.Connected := true;
  ibtransGrid.StartTransaction;

  ibtblGrid.Open;
  dsGrid.Enabled := true;

  dsGrid.DataSet.Refresh;
  IBDynamicGrid2.Refresh;

  ibtblGrid.RecNo := TmpRecNo;
end;

procedure TfmEditTable.ibtblFormAfterPost(DataSet: TDataSet);
begin
  ibtransForm.Commit;
  RefreshTable(ibtblGrid);

  ibtransForm.StartTransaction;
  ibtblForm.Open;
end;

procedure TfmEditTable.RemoveDynamicControls;
var TmpRecNo, i: integer;   tmpIsOpen: boolean;
begin
  //tmpIsOpen := ibtblGrid.Active;

  {if tmpIsOpen then
  begin
    TmpRecNo := ibtblGrid.RecNo;
    ibtblGrid.Close;
  end;}

  for i := pnlRecord.ControlCount -1 downto 0 do
    pnlRecord.RemoveControl(pnlRecord.Controls[i]);

 { if tmpIsOpen then
  begin
    ibtblGrid.Open;
    ibtblGrid.RecNo := TmpRecNo;
  end;
 }
end;

procedure TfmEditTable.ibtblGridAfterOpen(DataSet: TDataSet);
begin
  //GenerateFormView;
end;

procedure TfmEditTable.CurrentDateClick(Sender: TObject);
var
  FieldNum: Integer;
begin
  FieldNum:= (Sender as TBitBtn).Tag;
  fmCalen.DateTimeValue:= ibtblGrid.Fields[FieldNum].AsDateTime;

  if fmCalen.ShowModal = mrOK then
  begin
    ibtblGrid.Edit;
    ibtblGrid.Fields[FieldNum].AsDateTime:= fmCalen.DateTimeValue;
  end;
end;

procedure TfmEditTable.lmExportClick(Sender: TObject);
begin
  if not ibtblGrid.IsEmpty then
    ExportDataSet(ibtblGrid)
  else
    ShowMessage('DataSet has no records!');
end;

procedure TfmEditTable.tsFormViewShow(Sender: TObject);
begin
  //RemoveDynamicControls;
  GenerateFormView;
  SetWindowDimensions;
end;


procedure TfmEditTable.SetWindowDimensions;
begin
  if self.WindowState = wsNormal  then
  begin
     self.Height := 600;
     self.Width  := 834;
  end;
end;

procedure TfmEditTable.tsGridViewShow(Sender: TObject);
begin
  RemoveDynamicControls;
  RefreshTable(ibtblGrid);
  SetWindowDimensions;
end;

procedure TfmEditTable.ibtblGridAfterScroll(DataSet: TDataSet);
begin
  StatusBar1.Panels[0].Text := 'Record Count = ' + IntToStr(ibtblGrid.RecordCount)
    + '  RecNo: ' +  IntTostr(ibtblGrid.RecNo);
  if Assigned(ibtblForm) then
    if ibtblForm.Active then
      ibtblForm.RecNo := ibtblGrid.RecNo;
end;

procedure TfmEditTable.ibtblGridBeforePost(DataSet: TDataSet);
begin
  try
    // Nur um Sperren beim Edit abzufangen
  except
    on E: EIBInterBaseError do
    begin
      ShowMessage('Record is locked! Please try editing later: ' + E.Message);
      Abort;
    end;
  end;
end;

procedure TfmEditTable.Init(dbIndex: Integer; ATableName: string);
begin
  chkBoxUseFilter.Hint := 'When the filter is active, all matching records are shown.' + sLineBreak +
                         'When the filter is inactive, only the first matching record is selected.';

  ibtblGrid.Close;

  if ibtransGrid.InTransaction then
    ibtransGrid.Rollback;

  if IBDatabase1.Connected then
    IBDatabase1.Close;

  IBDatabase1.DatabaseName := Rec.IBConnection.DatabaseName;
  IBDatabase1.Params.Clear;
  IBDatabase1.Params.Add('user_name=' + Rec.RegRec.UserName);
  IBDatabase1.Params.Add('password=' + Rec.RegRec.Password);
  IBDatabase1.LoginPrompt := False;
  IBDatabase1.Connected := True;

  //ibtransGrid.StartTransaction;

  FillTablesCombobox;
  OpenTable(ATableName);
  cboxTables.ItemIndex := cboxTables.Items.IndexOf(ATableName);
  FillFieldsCombobox;

  Self.Caption := 'Edit Table: ' + ATableName;
end;

function TfmEditTable.GetArrayFieldInfo(DB: TIBDatabase; Field: TIBArrayField): string;
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

procedure TfmEditTable.GenerateFormView;
var
  ALabel: TLabel;
  ADBEdit: TDBEdit;
  ADBMemo: TDBMemo;
  ADBDateTime: TDBDateTimePicker;
  AArrayGrid: TIBArrayGrid;
  i, AWidth, VSpacing: Integer;
  ATop: Integer;

begin
  if ibtransForm.InTransaction then
    ibtransForm.Commit;

  if ibtblForm.Active then
    ibtblForm.Close;

  if dsForm.Enabled then
    dsForm.Enabled := false;

  ibtblForm.Free;
  ibtblForm := nil;
  ibtblForm := TIBTable.Create(self);
  ibtblForm.Database := IBDatabase1;
  ibtblForm.TableName := ibtblGrid.TableName;

  dsForm.Free;
  dsForm := nil;

  dsForm := TDataSource.Create(self);
  dsForm.DataSet := ibtblForm;
  dsForm.Enabled := true;
  dbnavForm.DataSource := dsForm;

  ibtblForm.Transaction := ibtransForm;
  ibtransForm.StartTransaction;

  ibtblForm.Active := true;

  ibtblForm.RecNo := ibtblGrid.RecNo;

  ATop := 20;
  VSpacing := 10; // vertikaler Abstand zwischen Controls

  for i := 0 to ibtblForm.Fields.Count - 1 do
  begin
    // Label für Feldnamen
    ALabel := TLabel.Create(pnlRecord);
    ALabel.Parent := pnlRecord;
    ALabel.Left := 20;
    ALabel.Top := ATop + VSpacing;
    ALabel.Caption := ibtblForm.Fields[i].FieldName;

    // Prüfen auf Array-Feld
    if ibtblForm.Fields[i] is TIBArrayField then
    begin
      with TIBArrayField(ibtblForm.Fields[i]) do
      begin
        AArrayGrid := TIBArrayGrid.Create(pnlRecord);
        AArrayGrid.Parent := pnlRecord;
        AArrayGrid.Left := 160;
        AArrayGrid.Top := ATop + VSpacing;
        AArrayGrid.Anchors := [akLeft, akTop, akRight];

        // 1D-Array: eine Zeile, mehrere Spalten
        if ArrayDimensions = 1 then
        begin
          AArrayGrid.RowCount := 1;
          AArrayGrid.ColCount := ArrayBounds[0].UpperBound - ArrayBounds[0].LowerBound + 1;
          AArrayGrid.DefaultColWidth := Max(50, Min(80, 400 div AArrayGrid.ColCount));
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
        AArrayGrid.DataSource := dsForm;
        AArrayGrid.DataField := FieldName;

        Inc(ATop, AArrayGrid.Height + 2); // Abstand für Label

        // Array Info-Label
        //ALabel := TLabel.Create(pnlRecord);
        //ALabel.Parent := pnlRecord;
        ALabel.Caption := ALabel.Caption +
                          sLineBreak + '(' +
                          GetArrayFieldInfo(ibtblForm.Database, TIBArrayField(ibtblForm.Fields[i])) + ')';

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
    case ibtblForm.Fields[i].DataType of
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
          ADBMemo.DataSource := dsForm;
          ADBMemo.DataField := ibtblForm.Fields[i].FieldName;

          Inc(ATop, ADBMemo.Height + VSpacing * 2);
        end;

      ftDate, ftTime, ftDateTime:
        begin
          ADBDateTime := TDBDateTimePicker.Create(pnlRecord);
          ADBDateTime.Parent := pnlRecord;
          ADBDateTime.Left := 160;
          ADBDateTime.Top := ATop + VSpacing;
          ADBDateTime.Width := 160;
          ADBDateTime.DataSource := dsForm;
          ADBDateTime.DataField := ibtblForm.Fields[i].FieldName;

          Inc(ATop, ADBDateTime.Height + VSpacing);
        end;

      else
        begin
          ADBEdit := TDBEdit.Create(pnlRecord);
          ADBEdit.Parent := pnlRecord;
          ADBEdit.Left := 160;
          ADBEdit.Top := ATop + VSpacing;
          ADBEdit.DataSource := dsForm;
          ADBEdit.DataField := ibtblForm.Fields[i].FieldName;

          AWidth := 80;
          if ibtblForm.Fields[i].DataType = ftString then
            AWidth := ibtblForm.Fields[i].DataSize * 10;
          if AWidth > 400 then
            AWidth := 400;
          ADBEdit.Width := AWidth;

          Inc(ATop, ADBEdit.Height + VSpacing);
        end;
    end;
  end;

  Height := ATop + VSpacing * 2;

end;

initialization
  {$I fedittabledata.lrs}

end.

