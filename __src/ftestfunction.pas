unit fTestFunction;
//todo: blob-editor for blob-values
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, IBConnection, SQLDB, Forms, Controls,
  Graphics, Dialogs, StdCtrls, DBCtrls, DBGrids, ExtCtrls, Buttons,  ComCtrls,
  udb_firebird_struct_helper,
  SysTables, turbocommon, fbcommon;

type

  { TfrmTestFunction }

  TfrmTestFunction = class(TForm)
    bbClose: TSpeedButton;
    bbExecute: TBitBtn;
    BufDataset: TBufDataset;
    BufDataSource: TDataSource;
    cboxPackages: TComboBox;
    DBGridParams: TDBGrid;
    DBLookupComboBox1: TDBLookupComboBox;
    DSParams: TDataSource;
    DSFuncs: TDataSource;
    GroupBox1: TGroupBox;
    IBConnection1: TIBConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbDatabase: TLabel;
    lbRoutineType: TLabel;
    lbRoutineName: TLabel;
    lbRoutineNm: TLabel;
    lbReturn: TLabel;
    Memo1: TMemo;
    MemoResult: TMemo;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    QFuncs: TSQLQuery;
    QParamInfo: TSQLQuery;
    QParams: TSQLQuery;
    rgRoutineType: TRadioGroup;
    Splitter1: TSplitter;
    SQLTransaction1: TSQLTransaction;

    procedure bbCloseClick(Sender: TObject);
    procedure bbExecuteClick(Sender: TObject);
    procedure cboxPackagesChange(Sender: TObject);
    procedure DBGridParamsKeyPress(Sender: TObject; var Key: char);
    procedure DBLookupComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgRoutineTypeClick(Sender: TObject);
  private
    FRoutineInfo: TRoutineInfo;
    procedure LoadFunctions;
    function  SetDBLookupComboBoxByDisplayText(AText: string): Boolean;

    procedure LoadParamsForFunction;
    procedure AddFieldToBufDataset(DataSet: TBufDataset;
       const FieldName: string; FieldType: TFieldType; FieldSize: Integer = 0);
    function BuildRoutineExecuteSQL(const ARoutineInfo: TRoutineInfo; const AParamList: string): string;
  public
    constructor CreateForRoutine(AOwner: TComponent; ARoutineInfo: TRoutineInfo);
    procedure Init(ARoutineInfo: TRoutineInfo);
    procedure SelectRoutineNode(ARoutineInfo: TRoutineInfo);
    procedure FillPackagesComboBox;
  end;


//var
    // frmTestFunction: TfrmTestFunction;

implementation

uses main;

{$R *.lfm}

{ TfrmTestFunction }

constructor TfrmTestFunction.CreateForRoutine(AOwner: TComponent; ARoutineInfo: TRoutineInfo);
begin
  inherited Create(AOwner);
end;

procedure TfrmTestFunction.Init(ARoutineInfo: TRoutineInfo);
begin
  FRoutineInfo := ARoutineInfo;
  AssignIBConnection(IBConnection1, FRoutineInfo.Connection);
  IBConnection1.Connected := true;

  lbDatabase.Caption := IBConnection1.DatabaseName;

  rgRoutineType.OnClick := nil;
  FillPackagesComboBox;

  if FRoutineInfo.PackageName <> '' then
    cboxPackages.ItemIndex := cboxPackages.Items.IndexOf(FRoutineInfo.PackageName);

  rgRoutineType.ItemIndex := ord(FRoutineInfo.RoutineType);
  cboxPackages.Enabled := IsPackageRoutine(StrToRoutineType(rgRoutineType.Items[rgRoutineType.ItemIndex]));

  LoadFunctions;

  rgRoutineType.OnClick := @rgRoutineTypeClick;
  SetDBLookupComboBoxByDisplayText(FRoutineInfo.RoutineName);
  LoadParamsForFunction;
end;

procedure TfrmTestFunction.FormCreate(Sender: TObject);
begin
  inherited;
end;

procedure TfrmTestFunction.FormShow(Sender: TObject);
begin
  DBGridParams.Columns[0].Width := 100;
  DBGridParams.Columns[1].Width := 100;
end;

procedure TfrmTestFunction.FillPackagesComboBox;
var TmpPackages: TStringList; Count: integer;
    i, dbIndex: Integer;
begin
  try
    cboxPackages.OnChange := nil;;
    dbIndex := TPNodeInfos(fmMain.tvMain.Selected.Data)^.dbIndex;
    TmpPackages := TStringList.Create;
    TmpPackages.CommaText := dmSysTables.GetDBObjectNames(dbIndex, otPackages, Count);
    for i := 0  to TmpPackages.Count - 1 do
      cboxPackages.Items.Add(TmpPackages[i]);
    cboxPackages.ItemIndex := 0;
  finally
    cboxPackages.OnChange := @cboxPackagesChange;
    TmpPackages.Free;
  end;
end;

procedure TfrmTestFunction.SelectRoutineNode(ARoutineInfo: TRoutineInfo);
var
  Node: TTreeNode;
  Info: TPNodeInfos;
  ExpectedObjectType: TTreeViewObjectType;

  function NodeMatches(Node: TTreeNode): Boolean;
  var
    Cur: TTreeNode;
    FoundPackage: Boolean;
  begin
    Result := False;
    if (Node = nil) or (Node.Data = nil) then Exit;

    Info := TPNodeInfos(Node.Data);
    if Info^.ObjectType <> ExpectedObjectType then Exit;
    if Info^.dbIndex <> ARoutineInfo.dbIndex then Exit;
    if not SameText(Node.Text, ARoutineInfo.RoutineName) then Exit;

    // Wenn kein Package verwendet wird, sind wir fertig
    if ARoutineInfo.PackageName = '' then
    begin
      Result := True;
      Exit;
    end;

    // Andernfalls: Package muss in der Parent-Kette liegen
    FoundPackage := False;
    Cur := Node.Parent;
    while Cur <> nil do
    begin
      if SameText(Cur.Text, ARoutineInfo.PackageName) then
      begin
        FoundPackage := True;
        Break;
      end;
      Cur := Cur.Parent;
    end;

    Result := FoundPackage;
  end;

begin
  if (fmMain.tvMain.Items.Count = 0) then Exit;

  ExpectedObjectType := RoutineTypeToTreeViewObjectType(ARoutineInfo.RoutineType);

  Node := fmMain.tvMain.Items.GetFirstNode;
  while Node <> nil do
  begin
    if NodeMatches(Node) then
    begin
      fmMain.tvMain.Selected := Node;
      fmMain.tvMain.SetFocus;
      fmMain.tvMain.Repaint;
      Exit;
    end;
    Node := Node.GetNext;
  end;
end;

procedure TfrmTestFunction.rgRoutineTypeClick(Sender: TObject);
var TreeViewObjectType: TTreeViewObjectType;
begin
  FRoutineInfo.RoutineType := StrToRoutineType(rgRoutineType.Items[rgRoutineType.ItemIndex]);
  if IsPackageRoutine(FRoutineInfo.RoutineType) then
    FRoutineInfo.PackageName := cboxPackages.Text;
  LoadFunctions;
  if QFuncs.RecordCount > 0 then
  begin
    DBLookupComboBox1.ItemIndex := 0;
    FRoutineInfo.RoutineName := DBLookupComboBox1.Text;
    LoadParamsForFunction;
    //TreeViewObjectType :=  RoutineTypeToTreeViewObjectType(FRoutineInfo.RoutineType);
    //fmMain.SelectTreeViewNode(FRoutineInfo);
    SelectRoutineNode(FRoutineInfo);
  end;
  cboxPackages.Enabled := IsPackageRoutine(StrToRoutineType(rgRoutineType.Items[rgRoutineType.ItemIndex]));
end;

procedure TfrmTestFunction.LoadFunctions;
begin
  QFuncs.Close;
  //QFuncs.SQL.Text := GetRoutineListSQL(FRoutineInfo.Connection, FRoutineInfo.RoutineType);
    QFuncs.SQL.Text := GetRoutineListSQL(IBConnection1, FRoutineInfo.RoutineType);

  if IsPackageRoutine(FRoutineInfo.RoutineType) then
    QFuncs.Params.ParamByName('PackageName').AsString := FRoutineInfo.PackageName;
  QFuncs.Open;
end;

procedure TfrmTestFunction.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  BufDataset.Close;
  BufDataset.Clear;
  QParamInfo.Close;
  QFuncs.Close;
  IBConnection1.Connected := false;
  CloseAction := caFree;
end;

procedure TfrmTestFunction.AddFieldToBufDataset(DataSet: TBufDataset;
  const FieldName: string; FieldType: TFieldType; FieldSize: Integer = 0);
begin
  with DataSet.FieldDefs.AddFieldDef do
  begin
    Name := FieldName;
    DataType := FieldType;
    Size := FieldSize; // Nur relevant für zB. ftString
  end;
end;

procedure TfrmTestFunction.LoadParamsForFunction;
var
  SQL: string;
  StrFieldType: string;
  CharSetID: Integer;
  OutSQL: string;
begin
  QParamInfo.Close;

  SQL := GetParamListSQL(IBConnection1, FRoutineInfo, ptInOnly);
  if SQL = '' then
  begin
    ShowMessage('No parameter query available for this routine type.');
    Exit;
  end;

  QParamInfo.SQL.Text := SQL;

  if IsFunctionRoutine(FRoutineInfo.RoutineType) then
    QParamInfo.ParamByName('FUNCNAME').AsString := FRoutineInfo.RoutineName
  else
    QParamInfo.ParamByName('PROCNAME').AsString := FRoutineInfo.RoutineName;

  if IsPackageRoutine(FRoutineInfo.RoutineType) then
    QParamInfo.ParamByName('PACKAGENAME').AsString := FRoutineInfo.PackageName;

  QParamInfo.Open;

  lbReturn.Caption := ''; // Initialisieren

  // Rückgabewert anzeigen (nur Funktion)
  if not QParamInfo.EOF then
  begin
    if IsCharType(QParamInfo.FieldByName('RDB$FIELD_TYPE').AsInteger) then
      CharSetID := QParamInfo.FieldByName('RDB$CHARACTER_SET_ID').AsInteger
    else
      CharSetID := -1;

    StrFieldType := ResolveFieldTypeAsString(
      Trim(QParamInfo.FieldByName('DATATYPE_SOURCE').AsString),
      QParamInfo.FieldByName('RDB$FIELD_TYPE').AsInteger,
      QParamInfo.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
      QParamInfo.FieldByName('RDB$FIELD_PRECISION').AsInteger,
      QParamInfo.FieldByName('RDB$FIELD_SCALE').AsInteger,
      QParamInfo.FieldByName('RDB$CHARACTER_LENGTH').AsInteger,
      CharSetID,
      IBConnection1
    );

    if IsPackageRoutine(FRoutineInfo.RoutineType) then
      lbRoutineName.Caption := FRoutineInfo.PackageName + '.' + FRoutineInfo.RoutineName
    else
      lbRoutineName.Caption := FRoutineInfo.RoutineName;
    lbRoutineType.Caption := RoutineTypeToStr(FRoutineInfo.RoutineType);

    if IsFunctionRoutine(FRoutineInfo.RoutineType) then
    begin
      lbReturn.Caption := StrFieldType;
      QParamInfo.Next; // skip return
    end;

  end;

  // OUT-Parameter mit Typen ergänzen
  OutSQL := GetParamListSQL(IBConnection1, FRoutineInfo, ptOutOnly);
  if OutSQL <> '' then
  begin
    QParamInfo.Close;
    QParamInfo.SQL.Text := OutSQL;

    if IsFunctionRoutine(FRoutineInfo.RoutineType) then
      QParamInfo.ParamByName('FUNCNAME').AsString := FRoutineInfo.RoutineName
    else
      QParamInfo.ParamByName('PROCNAME').AsString := FRoutineInfo.RoutineName;

    if IsPackageRoutine(FRoutineInfo.RoutineType) then
      QParamInfo.ParamByName('PACKAGENAME').AsString := FRoutineInfo.PackageName;

    QParamInfo.Open;

    while not QParamInfo.EOF do
    begin
      if IsCharType(QParamInfo.FieldByName('RDB$FIELD_TYPE').AsInteger) then
        CharSetID := QParamInfo.FieldByName('RDB$CHARACTER_SET_ID').AsInteger
      else
        CharSetID := -1;

      StrFieldType := ResolveFieldTypeAsString(
        Trim(QParamInfo.FieldByName('DATATYPE_SOURCE').AsString),
        QParamInfo.FieldByName('RDB$FIELD_TYPE').AsInteger,
        QParamInfo.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
        QParamInfo.FieldByName('RDB$FIELD_PRECISION').AsInteger,
        QParamInfo.FieldByName('RDB$FIELD_SCALE').AsInteger,
        QParamInfo.FieldByName('RDB$CHARACTER_LENGTH').AsInteger,
        CharSetID,
        IBConnection1
      );

      if not IsFunctionRoutine(FRoutineInfo.RoutineType) then
      begin
        if lbReturn.Caption <> '' then
          lbReturn.Caption := lbReturn.Caption + '  ';
        lbReturn.Caption := lbReturn.Caption + Trim(QParamInfo.FieldByName('PARAM_NAME').AsString) + ': ' + StrFieldType;
      end;

      QParamInfo.Next;
    end;

  end;

  // IN-Parameter in BufDataset übernehmen
  QParamInfo.Close;
  QParamInfo.SQL.Text := SQL;

  if IsFunctionRoutine(FRoutineInfo.RoutineType) then
    QParamInfo.ParamByName('FUNCNAME').AsString := FRoutineInfo.RoutineName
  else
    QParamInfo.ParamByName('PROCNAME').AsString := FRoutineInfo.RoutineName;

  if IsPackageRoutine(FRoutineInfo.RoutineType) then
    QParamInfo.ParamByName('PACKAGENAME').AsString := FRoutineInfo.PackageName;

  QParamInfo.Open;
  if IsFunctionRoutine(FRoutineInfo.RoutineType) then
    QParamInfo.Next; // Rückgabewert erneut überspringen, falls nötig

  BufDataset.Close;
  BufDataset.FieldDefs.Clear;
  BufDataset.FieldDefs.Add('NAME', ftString, 50);
  BufDataset.FieldDefs.Add('TYPE', ftString, 50);
  BufDataset.FieldDefs.Add('VALUE', ftWideString, 2048);
  BufDataset.CreateDataset;

  while not QParamInfo.EOF do
  begin
    if IsCharType(QParamInfo.FieldByName('RDB$FIELD_TYPE').AsInteger) then
      CharSetID := QParamInfo.FieldByName('RDB$CHARACTER_SET_ID').AsInteger
    else
      CharSetID := -1;

    StrFieldType := ResolveFieldTypeAsString(
      Trim(QParamInfo.FieldByName('DATATYPE_SOURCE').AsString),
      QParamInfo.FieldByName('RDB$FIELD_TYPE').AsInteger,
      QParamInfo.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
      QParamInfo.FieldByName('RDB$FIELD_PRECISION').AsInteger,
      QParamInfo.FieldByName('RDB$FIELD_SCALE').AsInteger,
      QParamInfo.FieldByName('RDB$CHARACTER_LENGTH').AsInteger,
      CharSetID,
      IBConnection1
    );

    BufDataset.Append;
    BufDataset.FieldByName('NAME').AsString := Trim(QParamInfo.FieldByName('PARAM_NAME').AsString);
    BufDataset.FieldByName('TYPE').AsString := StrFieldType;
    BufDataset.FieldByName('VALUE').AsString := '';
    BufDataset.Post;

    QParamInfo.Next;
  end;

  if BufDataset.Fields.Count > 1 then
  begin
    //BufDataset.Fields[0].ReadOnly := true;
    //BufDataset.Fields[1].ReadOnly := true;
  end;
  DBGridParams.Columns[0].ReadOnly := true;
  DBGridParams.Columns[1].ReadOnly := true;
  DBGridParams.Columns[0].Width := 120;
  DBGridParams.Columns[1].Width := 200;
end;

procedure TfrmTestFunction.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfrmTestFunction.bbExecuteClick(Sender: TObject);
var
  ParamList: string;
  SQLExec: string;
  OutParamNames: TStringList;
  i: Integer;
begin
  // ... (IN-Params zusammensetzen)
  BufDataset.First;
  ParamList := '';
  while not BufDataset.EOF do
  begin
    if ParamList <> '' then
      ParamList := ParamList + ', ';
    ParamList := ParamList + QuotedStr(BufDataset.FieldByName('VALUE').AsString);
    BufDataset.Next;
  end;

  SQLExec := BuildRoutineExecuteSQL(FRoutineInfo, ParamList);

  // OUT-Paramnamen ermitteln
  OutParamNames := TStringList.Create;
  try
    QParamInfo.Close;
    QParamInfo.SQL.Text := GetParamListSQL(IBConnection1, FRoutineInfo, ptOutOnly);

    if IsFunctionRoutine(FRoutineInfo.RoutineType) then
      QParamInfo.ParamByName('FUNCNAME').AsString := FRoutineInfo.RoutineName
    else
      QParamInfo.ParamByName('PROCNAME').AsString := FRoutineInfo.RoutineName;

    if IsPackageRoutine(FRoutineInfo.RoutineType) then
      QParamInfo.ParamByName('PACKAGENAME').AsString := FRoutineInfo.PackageName;

    QParamInfo.Open;
    while not QParamInfo.EOF do
    begin
      OutParamNames.Add(Trim(QParamInfo.FieldByName('PARAM_NAME').AsString));
      QParamInfo.Next;
    end;
  finally
    QParamInfo.Close;
  end;

  // Routine aufrufen und Ergebnis anzeigen
  QParamInfo.SQL.Text := SQLExec;
  try
    QParamInfo.Open;

    MemoResult.Lines.Clear;
    if not QParamInfo.EOF then
    begin
      if OutParamNames.Count > 0 then
      begin
        for i := 0 to QParamInfo.FieldCount - 1 do
          MemoResult.Lines.Add(QParamInfo.Fields[i].FieldName + ' = ' + QParamInfo.Fields[i].AsString);
        //lbReturn.Caption := lbReturn.Caption
      end
      else if QParamInfo.FieldCount > 0 then
        MemoResult.Lines.Text := QParamInfo.Fields[0].AsString
      else
        MemoResult.Lines.Text := '[Kein Ergebnis]';
    end
    else
      MemoResult.Lines.Text := '[Kein Ergebnis]';
  except
    on E: Exception do
      MemoResult.Lines.Text := '[Fehler] ' + E.Message;
  end;

  OutParamNames.Free;
end;

procedure TfrmTestFunction.cboxPackagesChange(Sender: TObject);
begin
  rgRoutineTypeClick(nil);
end;

function TfrmTestFunction.BuildRoutineExecuteSQL(const ARoutineInfo: TRoutineInfo; const AParamList: string): string;
var
  ParamPart: string;
begin
  if Trim(AParamList) = '' then
    ParamPart := ''
  else
    ParamPart := '(' + AParamList + ')';

  case ARoutineInfo.RoutineType of

    // 🔹 Klassische UDF
    rtUDF:
      Result := Format('SELECT %s%s FROM RDB$DATABASE',
        [ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Firebird native function
    rtFBFunc:
      Result := Format('SELECT %s%s FROM RDB$DATABASE',
        [ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Firebird stored procedure (SELECTable)
    rtFBProc:
      Result := Format('SELECT * FROM %s%s',
        [ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Externe UDR-Funktion
    rtUDRFunc:
      Result := Format('SELECT %s%s FROM RDB$DATABASE',
        [ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Externe UDR-Prozedur (als SELECT)
    rtUDRProc:
      Result := Format('SELECT * FROM %s%s',
        [ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Paket: native Funktion
    rtPackageFBFunc:
      Result := Format('SELECT %s.%s%s FROM RDB$DATABASE',
        [ARoutineInfo.PackageName, ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Paket: native Prozedur
    rtPackageFBProc:
      Result := Format('SELECT * FROM %s.%s%s',
        [ARoutineInfo.PackageName, ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Paket: UDR-Funktion
    rtPackageUDRFunc:
      Result := Format('SELECT %s.%s%s FROM RDB$DATABASE',
        [ARoutineInfo.PackageName, ARoutineInfo.RoutineName, ParamPart]);

    // 🔹 Paket: UDR-Prozedur (EXECUTE nötig)
    rtPackageUDRProc:
      Result := Format('EXECUTE PROCEDURE %s.%s%s',
        [ARoutineInfo.PackageName, ARoutineInfo.RoutineName, ParamPart]);

    // 🔸 Unbekannt? Fehlermeldung
    else
      raise Exception.Create('Unbekannter Routine-Typ: ' + Ord(ARoutineInfo.RoutineType).ToString);

  end;
end;


procedure TfrmTestFunction.DBGridParamsKeyPress(Sender: TObject; var Key: char);
var
  Field: TField;
begin
  // Prüfen ob aktuelles Feld das VALUE-Feld ist
  Field := BufDataset.Fields[2]; // Index 2 = VALUE

  if DBGridParams.SelectedField = Field then
  begin
    // Check ob das Zeichen im ValidChars Set ist
    if (Field.ValidChars <> []) and (not (Key in Field.ValidChars)) then
    begin
      Beep;
      Key := #0;  // Zeichen verwerfen
    end;
  end;
end;

procedure TfrmTestFunction.DBLookupComboBox1Change(Sender: TObject);
begin
  if DBLookupComboBox1.Items.Count = 0 then
    exit;
  if dsFuncs.DataSet.Locate('NAME', DBLookupComboBox1.Text, [loCaseInsensitive]) then
  begin
    FRoutineInfo.RoutineName := DBLookupComboBox1.Text;
    LoadParamsForFunction;
  end;
end;

function TfrmTestFunction.SetDBLookupComboBoxByDisplayText(AText: string): Boolean;
var
  i: Integer;
  DisplayText: string;
begin
  Result := False;
  AText := Trim(UpperCase(AText));
  for i := 0 to DBLookupComboBox1.Items.Count - 1 do
  begin
    DisplayText := Trim(UpperCase(DBLookupComboBox1.Items[i]));
    if DisplayText = AText then
    begin
      DBLookupComboBox1.ItemIndex := i;
      Result := True;
      Exit;
    end;
  end;
end;


end.

