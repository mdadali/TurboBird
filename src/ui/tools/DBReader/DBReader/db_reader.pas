unit db_reader;

(*
Database browser, main form

Author: Sergey Bodrov, 2024 Minsk
License: MIT
*)

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
  {$ifdef FPC}
  LCLType,   { for VK_F3 }
  {$else}
  Messages,
  {$endif}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Grids, DBCtrls, DBGrids, SynEdit,
  SynHighlighterSQL, ValueViewForm, DB, RFUtils, DBReaderBase, DBReaderFirebird,
  DBReaderBerkley, DBReaderMidas, DBReaderParadox, DBReaderDbf, FSReaderMtf,
  DBReaderMdf, DBReaderMdb, DBReaderEdb, DBReaderInno, DBReaderSqlite,
  DBReaderSybase, DBReaderDbisam, DBReaderTps, DBReaderRaima, DBReaderClarion,
  {$ifdef ENABLE_GSR}DBReaderGsr,{$endif}
  FSReaderBase, FSReaderPst,

  turbocommon,
  uthemeselector;

type
  TMyTreeNode = class(TTreeNode)
  public
    TableName: string;
    FileName: string;
  end;

  TGridMode = (gmTable, gmRecord);

  { TfrmDBReader }

  TfrmDBReader = class(TForm)
    memoLog: TMemo;
    panLeft: TPanel;
    spl1: TSplitter;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    TabSheet1: TTabSheet;
    tvMain: TTreeView;
    pgcMain: TPageControl;
    tsLog: TTabSheet;
    tsGrid: TTabSheet;
    dgItems: TDrawGrid;
    panFile: TPanel;
    lbFileName: TLabel;
    btnFileSelect: TButton;
    tsTableInfo: TTabSheet;
    memoInfo: TMemo;
    pmGrid: TPopupMenu;
    miExporttoCSV: TMenuItem;
    miDBGrid1: TMenuItem;
    miShowAsHex: TMenuItem;
    ProgressBar: TProgressBar;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure dgItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure FormShow(Sender: TObject);
    procedure tvMainChange(Sender: TObject; Node: TTreeNode);
    procedure btnFileSelectClick(Sender: TObject);
    procedure dgItemsDblClick(Sender: TObject);
    procedure dgItemsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure miExporttoCSVClick(Sender: TObject);
    procedure miDBGrid1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miShowAsHexClick(Sender: TObject);
    procedure dgItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }

    FLogStream: TStream;
    FSettings: TStringList;
    FDBReader: TDBReader;  // DataBase reader
    FFSReader: TFSReader;  // FileSystem reader
    FRowsList: TDbRowsList;
    FGridMode: TGridMode;
    FCurRowIndex: Integer;
    FCurColIndex: Integer;
    FDbFileName: string;
    FTableName: string;
    FShowAsHex: Boolean;  // show numbers as Hex
    FPrevUpdTC: Int64;  // for progress indicator
    function AddTreeNode(AParent: TTreeNode; AName, ATableName, AFileName: string): TMyTreeNode;
    function GetFieldColor(const AField: TDbFieldDefRec): TColor;

    procedure ExportGridToCSV();
    procedure TestDbGrid();

    procedure InitReader(AReader: TDBReader); // set reader options
    procedure FillTree();
    procedure FillTreeByFiles(AFileName: string);
    procedure ShowTable(ATableName: string);
    procedure ShowCellValue();
    // find next cell with same value and focus it
    procedure FindNextCell();

    procedure OpenBDB(AFileName: string);  // BerkleyDB (not tested)
    procedure OpenGsr(AFileName: string);
    procedure OpenTape(AFileName: string);
    procedure OpenMdf(AFileName: string; AStream: TStream = nil);
    procedure OpenPst(AFileName: string);
    procedure OpenDatabase(AFileName: string; AReaderClass: TDBReaderClass);

    procedure OnLogHandler(const S: string);
    procedure OnPageReadedHandler(Sender: TObject);
  public
    { Public declarations }
    MaxRows: Integer;
    procedure OpenDB(AFileName: string);

    procedure GenerateSQLSchemaFromDBReader(FDBReader: TDBReader; const FileName: string; Lines: TStrings; InsertData: Boolean);
  end;

//var
  //frmDBReader: TfrmDBReader;

implementation

{$R *.lfm}

{ TfrmDBReader }

procedure TfrmDBReader.GenerateSQLSchemaFromDBReader(FDBReader: TDBReader; const FileName: string; Lines: TStrings; InsertData: Boolean);
var
  i, j, k: Integer;
  Table: TDbRowsList;
  Field: TDbFieldDefRec;
  SqlType, FieldValue: string;
  DbName: string;
  Row: TDbRowItem;

  FieldNames: string;
  f: Integer;

begin
  Lines.Clear;

  //Datenbankname aus Dateiname
  DbName := ExtractFileName(FileName);
  DbName := ChangeFileExt(DbName, ''); // ohne Extension
  Lines.Add('CREATE DATABASE ' + DbName + ';');
  Lines.Add('');

  //Tabellen
  for i := 0 to FDBReader.GetTablesCount()-1 do
  begin
    Table := FDBReader.GetTableByIndex(i);

    if Table.IsSystem then Continue;
    if not Assigned(Table) then Continue;

    // optional: alle Zeilen laden
    if InsertData then
      FDBReader.ReadTable(Table.TableName, MaxInt, Table);

    Lines.Add('CREATE TABLE ' + MakeCaseSensitiveAuto(Table.TableName) + ' (');

    for j := 0 to Length(Table.FieldsDef)-1 do
    begin
      Field := Table.FieldsDef[j];

      // Feldtyp auf SQL-Typ mappen
      case Field.FieldType of
        ftString, ftFixedChar, ftWideString, ftFixedWideChar, ftMemo:
          SqlType := 'VARCHAR(' + IntToStr(Field.Size) + ')';
        ftSmallint, ftInteger: SqlType := 'INTEGER';
        ftLargeint: SqlType := 'BIGINT';
        ftFloat: SqlType := 'DOUBLE';
        ftCurrency: SqlType := 'DECIMAL(18,2)';
        ftDate: SqlType := 'DATE';
        ftTime: SqlType := 'TIME';
        ftDateTime, ftTimeStamp: SqlType := 'DATETIME';
        ftBoolean: SqlType := 'BOOLEAN';
        ftBytes, ftVarBytes, ftBlob: SqlType := 'BLOB';
      else
        SqlType := 'TEXT';
      end;

      // Komma setzen, außer beim letzten Feld
      if j < Length(Table.FieldsDef)-1 then
        Lines.Add('  ' + MakeCaseSensitiveAuto(Field.Name) + ' ' + SqlType + ',')
      else
        Lines.Add('  ' + MakeCaseSensitiveAuto(Field.Name) + ' ' + SqlType);
    end;

    Lines.Add(');');
    Lines.Add('');

    //INSERT INTO Statements (optional)
    if InsertData and (Table.Count > 0) then
    begin
      // Feldnamen zusammenbauen
      FieldNames := '';
      for f := 0 to Length(Table.FieldsDef)-1 do
      begin
        if f > 0 then
          FieldNames := FieldNames + ', ';
        FieldNames := FieldNames + MakeCaseSensitiveAuto(Table.FieldsDef[f].Name);
      end;

      for k := 0 to Table.Count-1 do
      begin
        Row := Table.GetItem(k);
        FieldValue := '';
        for j := 0 to Length(Table.FieldsDef)-1 do
        begin
          if j > 0 then FieldValue := FieldValue + ', ';

          // NULL prüfen
          if VarIsNull(Row.Values[j]) then
            FieldValue := FieldValue + 'NULL'
          else
          begin
            case Table.FieldsDef[j].FieldType of
              ftString, ftFixedChar, ftWideString, ftFixedWideChar, ftMemo:
                FieldValue := FieldValue + QuotedStr(VarToStr(Row.Values[j]));
              ftDate, ftTime, ftDateTime, ftTimeStamp:
                FieldValue := FieldValue + QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss', Row.Values[j]));
              ftBoolean:
                if Row.Values[j] then
                  FieldValue := FieldValue + '1'
                else
                  FieldValue := FieldValue + '0';
            else
              FieldValue := FieldValue + VarToStr(Row.Values[j]);
            end;
          end;
        end;

        Lines.Add('INSERT INTO ' + MakeCaseSensitiveAuto(Table.TableName) + ' (' + FieldNames + ') VALUES (' + FieldValue + ');');
      end;
      Lines.Add('');
    end;
  end;
end;

function TfrmDBReader.AddTreeNode(AParent: TTreeNode; AName, ATableName, AFileName: string): TMyTreeNode;
begin
  Result := TMyTreeNode.Create(tvMain.Items);
  Result.TableName := ATableName;
  Result.FileName := AFileName;
  if Assigned(AParent) then
    tvMain.Items.AddNode(Result, AParent, AName, nil, naAddChild)
  else
    tvMain.Items.AddNode(Result, AParent, AName, nil, naAdd);
end;

function TfrmDBReader.GetFieldColor(const AField: TDbFieldDefRec): TColor;
begin
  case AField.FieldType of
    ftString, ftWideString, ftFixedChar, ftFixedWideChar, ftMemo: Result := clGreen; // string
    ftInteger, ftSmallint, ftLargeint: Result := clNavy;  // integer
    ftFloat, ftCurrency: Result := clMaroon; // float
    ftDate, ftTime, ftDateTime, ftTimeStamp: Result := clTeal;     // date
    ftBytes, ftVarBytes, ftBlob: Result := clPurple;   // blob
  else // ftUnknown
    Result := clBlack;
  end;

end;

procedure TfrmDBReader.InitReader(AReader: TDBReader);
begin
  AReader.OnLog := OnLogHandler;
  AReader.OnPageReaded := OnPageReadedHandler;
  AReader.IsDebugPages := (FSettings.Values['DebugPages'] <> '');
  AReader.IsDebugRows := (FSettings.Values['DebugRows'] <> '');
end;

procedure TfrmDBReader.miDBGrid1Click(Sender: TObject);
begin
  TestDbGrid();
end;

procedure TfrmDBReader.miExporttoCSVClick(Sender: TObject);
begin
  ExportGridToCSV();
end;

procedure TfrmDBReader.miShowAsHexClick(Sender: TObject);
begin
  FShowAsHex := not FShowAsHex;
  miShowAsHex.Checked := FShowAsHex;
  dgItems.Invalidate();
end;

procedure TfrmDBReader.btnFileSelectClick(Sender: TObject);
var SQLLines: TStringList;
begin
  if OpenDialog.Execute then
  begin
    try
      OpenDB(OpenDialog.FileName);

      SQLLines := TStringList.Create;
      try
        GenerateSQLSchemaFromDBReader(FDBReader, FDbFileName, SQLLines, true);
        SynEdit1.Lines.Assign(SQLLines);
      finally
        SQLLines.Free;
      end;

    finally
      //
    end;
  end;
end;


procedure TfrmDBReader.dgItemsDblClick(Sender: TObject);
begin
  ShowCellValue();
end;

procedure TfrmDBReader.dgItemsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  c: TCanvas;
  //n, ii, nLen, nEnum: Integer;
  s: string;
  clText, clBack: TColor;
  {$ifdef FPC}
  ts: TTextStyle;
  {$else}
  tf: TTextFormat;
  {$endif}
  TmpField: TDbFieldDefRec;
  //TmpParam: TParam;
  TmpRow: TDbRowItem;
  TmpInt64: Int64;
  //qr: TPrQuery;
begin
  c := dgItems.Canvas;
  clText := clWindowText;
  clBack := clWindow;
  s := '';
  {$ifdef FPC}
  ts := c.TextStyle;
  {$else}
  tf := [];
  {$endif}
  if not Assigned(FRowsList) then
    Exit;

  if FGridMode = gmRecord then
  begin
    // current rec fields
    if ARow = 0 then
    begin
      // captions
      clBack := clMenu;
      case ACol of
        0: s := 'Field';
        1: s := 'Value';
      else
        s := '';
      end;
    end
    else if (ARow <= Length(FRowsList.FieldsDef)) then
    begin
      // data
      TmpRow := FRowsList.GetItem(FCurRowIndex);
      TmpField := FRowsList.FieldsDef[ARow-1];

      if ACol = 0 then // name
      begin
        s := TmpField.Name;
      end
      else
      begin
        s := TmpRow.GetFieldAsStr(ARow-1);
        if Length(s) > 100 then
          s := Copy(s, 1, 100) + '...';
        clText := GetFieldColor(TmpField);
        // align numbers to right side
        if TmpField.FieldType in [ftInteger, ftSmallint, ftLargeint, ftFloat, ftCurrency] then
        //if VarIsOrdinal(TmpRow.Values[]) or VarIsFloat(TmpField.Value) then
        {$ifdef FPC}
          ts.Alignment := taRightJustify;
        {$else}
          tf := [tfRight];
        {$endif}
      end;
    end;
  end
  else if FGridMode = gmTable then
  begin
    // == table
    if ARow = 0 then
    begin
      // captions
      clBack := clMenu;
      if (ACol >= 0) and (ACol < Length(FRowsList.FieldsDef)) then
        s := FRowsList.FieldsDef[ACol].Name;
    end
    else if (ARow <= FRowsList.Count) and (ACol < Length(FRowsList.FieldsDef)) then
    begin
      TmpField := FRowsList.FieldsDef[ACol];
      TmpRow := FRowsList.GetItem(ARow-1);
      if Assigned(TmpRow) then
      begin
        if ACol < Length(TmpRow.Values) then
        begin
          //s := VarToStrDef(TmpRow.Values[ACol], 'null');
          s := TmpRow.GetFieldAsStr(ACol);

          if Length(s) > 100 then
            s := Copy(s, 1, 100) + '...';
          clText := GetFieldColor(TmpField);
          // align numbers to right side
          if TmpField.FieldType in [ftInteger, ftSmallint, ftLargeint, ftFloat, ftCurrency] then
          //if VarIsOrdinal(TmpRow.Data[ACol]) or VarIsFloat(TmpRow.Data[ACol]) then
          {$ifdef FPC}
            ts.Alignment := taRightJustify;
          {$else}
            tf := [tfRight];
          {$endif}
          // show as hex
          if FShowAsHex
          and (TmpField.FieldType in [ftInteger, ftSmallint, ftLargeint])
          and (not VarIsNull(TmpRow.Values[ACol]))
          then
          begin
            TmpInt64 := TmpRow.Values[ACol];
            s := Format('%x', [TmpInt64]);
          end;
        end;
      end;
    end;
  end;

  if c.Brush.Color <> clBack then
  begin
    c.Brush.Color := clBack;
    c.FillRect(Rect);
  end;
  c.Font.Color := clText;
  Rect.Left := Rect.Left + 2;
  Rect.Right := Rect.Right - 2;
  {$ifdef FPC}
  c.TextRect(Rect, Rect.Left, Rect.Top, s, ts);
  {$else}
  c.TextRect(Rect, s, tf);
  {$endif}
end;

procedure TfrmDBReader.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfrmDBReader.dgItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F3 then
    FindNextCell();
end;

procedure TfrmDBReader.dgItemsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  FCurRowIndex := ARow - 1;
  FCurColIndex := ACol;
end;

procedure TfrmDBReader.ExportGridToCSV;
var
  dlg: TSaveDialog;
  sFileName: string;
  fs: TFileStream;
  i, ii: Integer;
  s, ss, sSeparator: string;
  TmpRow: TDbRowItem;
begin
  // select filename
  sFileName := '';
  sSeparator := #09; // TAB
  dlg := TSaveDialog.Create(Self);
  try
    dlg.DefaultExt := '.csv';
    dlg.FileName := FTableName + '.csv';
    dlg.Filter := 'CSV|*.csv';
    //if dlg.Execute(Self.Handle) then
    if dlg.Execute() then
    begin
      sFileName := dlg.FileName;
    end;
  finally
    dlg.Free();
  end;
  if sFileName = '' then
    Exit;

  // export to stream
  fs := TFileStream.Create(sFileName, fmCreate);
  try
    // column names
    ss := '';
    for ii := 0 to Length(FRowsList.FieldsDef) - 1 do
    begin
      s := FRowsList.FieldsDef[ii].Name;
      if ss <> '' then
        ss := ss + sSeparator;
      ss := ss + s;
    end;
    StrToStream(ss, fs);

    // rows
    for i := 0 to FRowsList.Count - 1 do
    begin
      TmpRow := FRowsList.GetItem(i);
      ss := '';
      for ii := 0 to Length(FRowsList.FieldsDef) - 1 do
      begin
        s := TmpRow.GetFieldAsStr(ii);
        if (Pos(sSeparator, s) > 0) then
          s := QuotedStr(s);
        if ss <> '' then
          ss := ss + sSeparator;
        ss := ss + s;
      end;
      StrToStream(ss, fs);

    end;
  finally
    fs.Free();
  end;
end;

procedure TfrmDBReader.FillTree();
var
  i: Integer;
  TmpTable: TDbRowsList;
  tnParent: TTreeNode;
begin
  // Fill tree
  tvMain.Items.BeginUpdate();
  tvMain.Items.Clear();
  // tables
  for i := 0 to FDBReader.GetTablesCount() - 1 do
  begin
    TmpTable := FDBReader.GetTableByIndex(i);
    if (not TmpTable.IsSystem) and (not TmpTable.IsEmpty) and (not TmpTable.IsGhost) then
      AddTreeNode(nil, TmpTable.TableName, TmpTable.TableName, '');
  end;
  // Empty tables
  tnParent := nil;
  for i := 0 to FDBReader.GetTablesCount() - 1 do
  begin
    TmpTable := FDBReader.GetTableByIndex(i);
    if (not TmpTable.IsSystem) and TmpTable.IsEmpty and (not TmpTable.IsGhost) then
    begin
      if not Assigned(tnParent) then
        tnParent := AddTreeNode(nil, 'Empty tables', '', '');
      AddTreeNode(tnParent, TmpTable.TableName, TmpTable.TableName, '');
    end;
  end;
  // Ghost tables
  tnParent := nil;
  for i := 0 to FDBReader.GetTablesCount() - 1 do
  begin
    TmpTable := FDBReader.GetTableByIndex(i);
    if (not TmpTable.IsSystem) and TmpTable.IsGhost then
    begin
      if not Assigned(tnParent) then
        tnParent := AddTreeNode(nil, 'Ghost tables', '', '');
      AddTreeNode(tnParent, TmpTable.TableName, TmpTable.TableName, '');
    end;
  end;
  // System tables
  tnParent := nil;
  for i := 0 to FDBReader.GetTablesCount() - 1 do
  begin
    TmpTable := FDBReader.GetTableByIndex(i);
    if TmpTable.IsSystem then
    begin
      if not Assigned(tnParent) then
        tnParent := AddTreeNode(nil, 'System tables', '', '');
      AddTreeNode(tnParent, TmpTable.TableName, TmpTable.TableName, '');
    end;
  end;
  tvMain.Items.EndUpdate();
end;

procedure TfrmDBReader.FillTreeByFiles(AFileName: string);
var
  sr: TSearchRec;
  sPath, sName, sCurName: string;
  i: Integer;
begin
  sCurName := FTableName;
  if Assigned(tvMain.Selected) then
    sCurName := tvMain.Selected.Text;

  sPath := ExtractFilePath(AFileName);
  sName := sPath + '*' + ExtractFileExt(AFileName);
  tvMain.Items.BeginUpdate;
  tvMain.Items.Clear();
  if SysUtils.FindFirst(sName, faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faAnyFile) > 0 then
      begin
        sName := sPath + sr.Name;
        AddTreeNode(nil, sr.Name, sr.Name, sName);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  for i := 0 to tvMain.Items.Count - 1 do
  begin
    if tvMain.Items[i].Text = sCurName then
    begin
      tvMain.OnChange := nil;
      tvMain.Selected := tvMain.Items[i];
      tvMain.OnChange := tvMainChange;
      Break;
    end;
  end;

  tvMain.Items.EndUpdate;
end;

procedure TfrmDBReader.FindNextCell;
var
  //TmpField: TDbFieldDefRec;
  TmpRow: TDbRowItem;
  //i, iOffs, iLen: Integer;
  CurRowIndex: Integer;
  v: Variant;
begin
  v := Null;
  CurRowIndex := FCurRowIndex;
  if (FCurRowIndex <= FRowsList.Count) and (FCurColIndex < Length(FRowsList.FieldsDef)) then
  begin
    TmpRow := FRowsList.GetItem(FCurRowIndex);
    if Assigned(TmpRow) then
    begin
      if FCurColIndex < Length(TmpRow.Values) then
      begin
        //s := VarToStrDef(TmpRow.Values[ACol], 'null');
        //s := TmpRow.GetFieldAsStr(FCurRowIndex);
        v := TmpRow.Values[FCurColIndex];
      end;
    end;
  end;

  Inc(CurRowIndex);
  while CurRowIndex < FRowsList.Count do
  begin
    TmpRow := FRowsList.GetItem(CurRowIndex);
    if Assigned(TmpRow)
    and (FCurColIndex < Length(TmpRow.Values))
    and (v = TmpRow.Values[FCurColIndex]) then
    begin
      // found, set focused
      FCurRowIndex := CurRowIndex;
      dgItems.Row := FCurRowIndex + 1;
      Exit;
    end;
    Inc(CurRowIndex);
  end;

  // if not found below, search from above
  CurRowIndex := 0;
  while CurRowIndex < FCurRowIndex do
  begin
    TmpRow := FRowsList.GetItem(CurRowIndex);
    if Assigned(TmpRow)
    and (FCurColIndex < Length(TmpRow.Values))
    and (v = TmpRow.Values[FCurColIndex]) then
    begin
      // found, set focused
      FCurRowIndex := CurRowIndex;
      dgItems.Row := FCurRowIndex + 1;
      Exit;
    end;
    Inc(CurRowIndex);
  end;
end;

procedure TfrmDBReader.FormCreate(Sender: TObject);
var
  s, sLogName: string;
begin
  FSettings := TStringList.Create();
  if FileExists('DBReader.ini') then
  begin
    FSettings.LoadFromFile('DBReader.ini');
    if FSettings.Values['LogToFile'] <> '' then
    begin
      sLogName := 'DBReader.log';
      if not FileExists(sLogName) then
      begin
        FLogStream := TFileStream.Create(sLogName, fmCreate);
        FLogStream.Free();
      end;
      FLogStream := TFileStream.Create(sLogName, fmOpenWrite + fmShareDenyNone);
      FLogStream.Size := 0;
    end;
  end;
  memoLog.Clear();
  FRowsList := TDbRowsList.Create;
  MaxRows := MaxInt;
  //Test();
  if ParamCount > 0 then
  begin
    s := ExpandFileName(ParamStr(1));
    OpenDB(s);
  end;
end;

procedure TfrmDBReader.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLogStream);
  FreeAndNil(FSettings);
end;

procedure TfrmDBReader.OnLogHandler(const S: string);
begin
  if Assigned(FLogStream) then
    StrToStream(S, FLogStream, True);
  if memoLog.Lines.Count < 1000  then
    memoLog.Lines.Add(S);
end;

procedure TfrmDBReader.OnPageReadedHandler(Sender: TObject);
begin
  if TickDiff(GetTickCount64, FPrevUpdTC) > 1000 then
  begin
    FPrevUpdTC := GetTickCount64;
    ProgressBar.Position := FDBReader.GetProgress();
    Application.ProcessMessages();
  end;
end;

procedure TfrmDBReader.OpenBDB(AFileName: string);
begin
  if FileExists(AFileName) then
  begin
    FDbFileName := AFileName;
    lbFileName.Caption := ExtractFileName(FDbFileName);
  end
  else
  begin
    FDbFileName := '';
    lbFileName.Caption := '<file not selected>';
    Exit;
  end;

  memoLog.Lines.BeginUpdate();
  memoLog.Lines.Clear();
  FDBReader := TDBReaderBerkley.Create(Self);
  InitReader(FDBReader);
  FDBReader.OpenFile(AFileName);
  memoLog.Lines.EndUpdate();
end;

procedure TfrmDBReader.OpenDB(AFileName: string);
var
  sExt: string;
begin
  if FileExists(AFileName) then
  begin
    FDbFileName := AFileName;
    lbFileName.Caption := ExtractFileName(FDbFileName);
  end
  else
  begin
    FDbFileName := '';
    lbFileName.Caption := '<file not selected>';
    Exit;
  end;

  sExt := LowerCase(ExtractFileExt(FDbFileName));

  ProgressBar.Visible := True;
  memoLog.Lines.BeginUpdate();
  memoLog.Lines.Clear();
  FreeAndNil(FDBReader);
  try
    if (sExt = '.gdb') or (sExt = '.fdb') then
      OpenDatabase(FDbFileName, TDBReaderFB)
    else
    if (sExt = '.cds') then
      OpenDatabase(FDbFileName, TDBReaderMidas)
    else
    if (sExt = '.db') then
      OpenDatabase(FDbFileName, TDBReaderParadox)
    else
    if (sExt = '.gsr') then
      OpenGsr(FDbFileName)
    else
    if (sExt = '.dbf') then
      OpenDatabase(FDbFileName, TDBReaderDbf)
    else
    if (sExt = '.bak') then
      OpenTape(FDbFileName)
    else
    if (sExt = '.mdf') then
      OpenMdf(FDbFileName)
    else
    if (sExt = '.mdb') or (sExt = '.accdb') then
      OpenDatabase(FDbFileName, TDBReaderMdb)
    else
    if (sExt = '.edb') then
      OpenDatabase(FDbFileName, TDBReaderEdb)
    else
    if (sExt = '.pst') then
      OpenPst(FDbFileName)
    else
    if (sExt = '.ibd') or (Pos('ibdata1', ExtractFileName(FDbFileName)) > 0) then
      OpenDatabase(FDbFileName, TDBReaderInnoDB)
    else
    if (sExt = '.db3') or (sExt = '.sqlite') then
      OpenDatabase(FDbFileName, TDBReaderSqlite)
    else
    if (sExt = '.dbs') then
      OpenDatabase(FDbFileName, TDBReaderSybase)
    else
    if (sExt = '.dat') then
      //OpenDatabase(FDbFileName, TDBReaderDbisam)
      OpenDatabase(FDbFileName, TDBReaderClarion)
    else
    if (sExt = '.tps') then
      OpenDatabase(FDbFileName, TDBReaderTps)
    else
    if (sExt = '.dbd') then
      OpenDatabase(FDbFileName, TDBReaderRaima);

  finally
    memoLog.Lines.EndUpdate();
  end;
  ProgressBar.Visible := False;
end;

procedure TfrmDBReader.OpenDatabase(AFileName: string; AReaderClass: TDBReaderClass);
begin
  FDBReader := AReaderClass.Create(Self);
  InitReader(FDBReader);
  try
    FDBReader.OpenFile(AFileName);
  except
    on E: Exception do
      memoInfo.Lines.Append(E.Message);
  end;

  if FDBReader.IsSingleTable then
  begin
    ShowTable(AFileName);

    // show other files from same path
    FillTreeByFiles(AFileName);
  end
  else
    FillTree();
end;

procedure TfrmDBReader.OpenGsr(AFileName: string);
{$ifdef ENABLE_GSR}
var
  tnSys: TMyTreeNode;
  i: Integer;
  TmpTable: TGsrTable;
{$endif}
begin
{$ifdef ENABLE_GSR}
  FDBReader := TDBReaderGsr.Create(Self);
  InitReader(FDBReader);
  try
    FDBReader.OpenFile(AFileName);
  except
    on E: Exception do
      memoInfo.Lines.Append(E.Message);
  end;

  // Fill tree
  tvMain.Items.BeginUpdate();
  tvMain.Items.Clear();
  // tables
  for i := 0 to (FDBReader as TDBReaderGsr).TableList.Count - 1 do
  begin
    TmpTable := (FDBReader as TDBReaderGsr).TableList.GetItem(i);
    if TmpTable.RowCount > 0 then
      AddTreeNode(nil, TmpTable.TableName, TmpTable.TableName, '');
  end;
  // Empty tables
  tnSys := AddTreeNode(nil, 'Empty tables', '', '');
  for i := 0 to (FDBReader as TDBReaderGsr).TableList.Count - 1 do
  begin
    TmpTable := (FDBReader as TDBReaderGsr).TableList.GetItem(i);
    if TmpTable.RowCount > 0 then
      Continue;
    AddTreeNode(tnSys, TmpTable.TableName, TmpTable.TableName, '');
  end;
  tvMain.Items.EndUpdate();
{$endif}
end;

procedure TfrmDBReader.OpenMdf(AFileName: string; AStream: TStream);
begin
  FDBReader := TDBReaderMdf.Create(Self);
  InitReader(FDBReader);
  try
    FDBReader.OpenFile(AFileName, AStream);
  except
    on E: Exception do
      memoInfo.Lines.Append(E.Message);
  end;

  FillTree();
end;

procedure TfrmDBReader.OpenPst(AFileName: string);
var
  r: TFSReaderPst;
  i, ii: Integer;
  TmpNode: TPstNode;
  tn, tnParent: TMyTreeNode;
begin
  FreeAndNil(FFSReader);

  r := TFSReaderPst.Create(Self);
  FFSReader := r;
  r.OnLog := OnLogHandler;
  r.IsDebugPages := (FSettings.Values['DebugPages'] <> '');
  //r.IsSaveStreams := (FSettings.Values['PstSaveStreams'] <> '');
  r.OpenFile(AFileName);

  // fill tree
  tvMain.Items.BeginUpdate();
  tvMain.Items.Clear();
  // tables
  for i := 0 to r.NodeList.Count - 1 do
  begin
    TmpNode := r.NodeList.GetItem(i);
    if TmpNode.Name <> '' then
    begin
      // find parent
      tnParent := nil;
      for ii := 0 to tvMain.Items.Count - 1 do
      begin
        tnParent := (tvMain.Items.Item[ii] as TMyTreeNode);
        if tnParent.Data = TmpNode.ParentNode then
          Break;
        tnParent := nil;
      end;

      tn := AddTreeNode(tnParent, TmpNode.Name, TmpNode.Name, '');
      tn.Data := TmpNode;
    end;
  end;
  tvMain.Items.EndUpdate();

  ShowTable('Messages');
end;

procedure TfrmDBReader.OpenTape(AFileName: string);
var
  r: TFSReaderMtf;
  ifs: TInnerFileStream;
begin
  r := TFSReaderMtf.Create(Self);
  try
    r.OnLog := OnLogHandler;
    r.IsSaveStreams := (FSettings.Values['MtfSaveStreams'] <> '');
    r.OpenFile(AFileName);
    if r.MdfPos > 0 then
    begin
      ifs := TInnerFileStream.Create(AFileName, r.MdfName, r.MdfPos, r.MdfSize);
      OpenMdf(r.MdfName, ifs);
    end;
  finally
    r.Free();
  end;
end;

procedure TfrmDBReader.ShowCellValue;
var
  TmpField: TDbFieldDefRec;
  TmpRow: TDbRowItem;
  i, iOffs, iLen: Integer;
begin
  if (FCurRowIndex <= FRowsList.Count) and (FCurColIndex < Length(FRowsList.FieldsDef)) then
  begin
    TmpField := FRowsList.FieldsDef[FCurColIndex];
    TmpRow := FRowsList.GetItem(FCurRowIndex);
    if Assigned(TmpRow) then
    begin
      FormRawValue.Value := Null;
      if FCurColIndex < Length(TmpRow.Values) then
      begin
        //s := VarToStrDef(TmpRow.Values[ACol], 'null');
        //s := TmpRow.GetFieldAsStr(FCurRowIndex);
        FormRawValue.Value := TmpRow.Values[FCurColIndex];
      end;
      // field offset and size
      iOffs := TmpField.RawOffset;
      iLen := TmpField.Size;
      if iOffs = 0 then
      begin
        if Length(TmpRow.RawOffs) = 0 then
        begin
          // calculate offset
          //iOffs := ((Length(FRowsList.FieldsDef) div 32) + 1) * 4;  // null bitmap
          for i := 0 to FCurColIndex-1 do
            iOffs := iOffs + FRowsList.FieldsDef[i].Size;
        end
        else if FCurColIndex < Length(TmpRow.RawOffs) then
        begin
          iOffs := TmpRow.RawOffs[FCurColIndex];
          if FCurColIndex = (Length(TmpRow.RawOffs)-1) then
            iLen := Length(TmpRow.RawData) - iOffs
          else
            iLen := TmpRow.RawOffs[FCurColIndex+1] - iOffs;
        end;
      end;
      FormRawValue.FieldName := TmpField.Name;
      FormRawValue.FieldTypeName := TmpField.TypeName;
      FormRawValue.IsBlob := TmpField.FieldType in [ftMemo, ftBlob, ftBytes, ftVarBytes];
      FormRawValue.ShowValue(TmpRow.RawData, iOffs, iLen);
    end;
  end;
end;

procedure TfrmDBReader.ShowTable(ATableName: string);
var
  i: Integer;
  TmpField: TDbFieldDefRec;
begin
  memoInfo.Clear();
  FTableName := ATableName;
  if ATableName = '' then
  begin
    tsGrid.Caption := 'Grid';
    dgItems.RowCount := 1;
    dgItems.ColCount := 0;
    Exit;
  end
  else
  begin
    if Assigned(FDBReader) then
    begin
      // table info
      FDBReader.FillTableInfoText(ATableName, memoInfo.Lines);
      // table rows
      tsGrid.Caption := ATableName;
      if FRowsList.TableName <> ATableName then
      begin
        FRowsList.Clear();
        ProgressBar.Visible := True;
        try
          FDBReader.ReadTable(ATableName, MaxRows, FRowsList);
        except
          on E: Exception do ShowException(E, nil);
        end;
        ProgressBar.Visible := False;
      end;
    end
    else if Assigned(FFSReader) then
    begin
      memoInfo.Lines.Clear();
      //FFSReader.FillNodeInfoText(ATableName, memoInfo.Lines);
      // node rows
      tsGrid.Caption := ATableName;
      if FRowsList.TableName <> ATableName then
      begin
        FRowsList.Clear();
        ProgressBar.Visible := True;
        try
          if (FFSReader is TFSReaderPst) then
            (FFSReader as TFSReaderPst).ReadTable(ATableName, MaxRows, FRowsList);
        except
          on E: Exception do ShowException(E, nil);
        end;
        ProgressBar.Visible := False;
      end;
    end
    else
    begin
      memoInfo.Lines.Clear();
      tsGrid.Caption := ATableName;
      FRowsList.Clear();
    end;

    tsGrid.Caption := ATableName + ' [' + IntToStr(FRowsList.Count) + ' rows]';

    // prepare grid (table)
    dgItems.DefaultRowHeight := dgItems.Font.Size + 8;
    dgItems.ColCount := Length(FRowsList.FieldsDef);
    dgItems.RowCount := FRowsList.Count + 1;
    for i := 0 to Length(FRowsList.FieldsDef) - 1 do
    begin
      TmpField := FRowsList.FieldsDef[i];
      //if (Pos('_name', FColNames[i]) > 0) or (ColItem.ValueSize > 40) then
      if (TmpField.Size > 40)
      or (TmpField.FieldType in [ftBlob, ftMemo])
      then
        dgItems.ColWidths[i] := 280
      {else if Length(ColItem.EnumValues) > 0 then
        dgItems.ColWidths[i] := 140
      else if ColItem.RefTableName <> '' then
        dgItems.ColWidths[i] := 40 }
      else
        dgItems.ColWidths[i] := 100;
    end;
  end;

  dgItems.FixedCols := 0;
  if dgItems.RowCount > 1 then
    dgItems.FixedRows := 1
  else
    dgItems.FixedRows := 0;
  dgItems.Invalidate();
end;

procedure TfrmDBReader.TestDbGrid;
begin
{
  FormDbGrid.DataSet.AssignRowsList(FRowsList);
  FormDbGrid.dbgr.Columns.RebuildColumns();
  FormDbGrid.Show();
}
end;

procedure TfrmDBReader.tvMainChange(Sender: TObject; Node: TTreeNode);
begin
  if not Assigned(Node) then Exit;
  if not Assigned(FDBReader) then Exit;
  
  if FDBReader.IsSingleTable then
    FDBReader.OpenFile((Node as TMyTreeNode).FileName);
  ShowTable((Node as TMyTreeNode).TableName);
end;

end.
