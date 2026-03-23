unit bulk_clone;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, IBQuery, IBDatabase, IBDatabaseInfo, IBExtract, ibxscript,

  turbocommon,
  uthemeselector;

type

  TExternalFormat = (efBinary, efFixedText, efCSV);

  { TfrmBulkClone }

  TfrmBulkClone = class(TForm)
    btnCancel: TButton;
    btnExecute: TButton;
    btnOpenExternalFile: TButton;
    chkBoxOutputHeader: TCheckBox;
    chkExportData: TCheckBox;
    comboxDestDB: TComboBox;
    comboxSourceDB: TComboBox;
    comboxDestServer: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    Destination: TGroupBox;
    edtCSVSeparator: TEdit;
    edtCSVQuote: TEdit;
    edtDefaultBatchSize: TEdit;
    edtExternalFile: TEdit;
    edtExternalTableName: TEdit;
    gbFormat: TGroupBox;
    grBoxSource: TGroupBox;
    GroupBox1: TGroupBox;
    grBoxExternals: TGroupBox;
    GroupBox3: TGroupBox;
    IBDBDest: TIBDatabase;
    IBDBSource: TIBDatabase;
    IBQueryDest: TIBQuery;
    IBQuerySource: TIBQuery;
    IBTransDest: TIBTransaction;
    IBTransSource: TIBTransaction;
    IBXScript1: TIBXScript;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblExternalFile: TLabel;
    lblExternalTableName: TLabel;
    lbSourceTable: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlTop: TPanel;
    rbBinary: TRadioButton;
    rbCSV: TRadioButton;
    rbFixedText: TRadioButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnOpenExternalFileClick(Sender: TObject);
    procedure comboxDestDBChange(Sender: TObject);
    procedure comboxDestServerChange(Sender: TObject);
    procedure comboxSourceDBChange(Sender: TObject);
    procedure comboxSourceServerChange(Sender: TObject);
    procedure comboxSourceTablesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FSourceDBIndex: integer;
    FFormat: TExternalFormat;

    FSourceServerName,
    FSourceDatabaseName,
    FSourceTableName,

    FDestServerName,
    FDestDatabaseName: string;

    function  FillSourceServerCombo: boolean;
    function  FillSourceDBCombo: boolean;
    function  FillSourceTableCombo: boolean;
    procedure FillSourceCombos;

    function  FillDestServerCombo: boolean;
    function  FillDestDBCombo: boolean;
    procedure FillDestCombos;

    function ConfigureSourceConnection: boolean;
    function ConfigureDestConnection: boolean;



    function GetFormat: TExternalFormat;
    function BuildFieldList(ATable: string): string;
    function BuildCSVView(ATable: string): string;
    function BuildCSVHeader(ATable: string): string;
    function GetCSVLineLength(ATable: string): integer;
    function GenerateSQLCSV: string;
    function GenerateSQLFixedText: string;
    function GenerateSQLBinary: string;
    function GenerateSQLCSV_CreateView(SrcTable, ExtTable, Quote, Sep: string): string;
    function GenerateSQLCSV_CreateTable(ExtTable, FileName: string): string;

    function TableExists(DB: TIBDatabase; TableName: string): Boolean;
    function ExternalTableExists(DB: TIBDatabase; TableName: string): Boolean;
    function CreateExternalTableIfNotExists(DB: TIBDatabase; Transaction: TIBTransaction;
      TableName, FileName: string): Boolean;
    function CreateDestTable(DestDB: TIBDatabase; DestTrans: TIBTransaction; TableName: string): Boolean;

  public
    procedure Init(ANodeInfos: TPNodeInfos);
    function GenerateSQL: string;
  end;

//var
  //frmBulkClone: TfrmBulkClone;

var SkipImport: boolean;

implementation

{$R *.lfm}

{ TfrmBulkClone }

procedure TfrmBulkClone.FormCreate(Sender: TObject);
begin
  SkipImport := false;
  edtDefaultBatchSize.Text := IntToStr(DefaultBatchSize);    //from inifile

  comboxSourceServer.OnChange := nil;

  FillSourceCombos;
  FillDestCombos;

  comboxSourceServer.OnChange := @comboxSourceServerChange;
end;

procedure TfrmBulkClone.Init(ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
end;


//DestinationServer/////////////////////////////////////////////////////////////
procedure TfrmBulkClone.comboxSourceServerChange(Sender: TObject);
begin
  FillSourceDBCombo;
end;

procedure TfrmBulkClone.comboxSourceDBChange(Sender: TObject);
begin
  comboxSourceTables.Items.Clear;
  if ConfigureSourceConnection then
    FillSourceTableCombo;
end;

procedure TfrmBulkClone.comboxSourceTablesChange(Sender: TObject);
begin
  FSourceTableName := Trim(comboxSourceTables.Text);
  edtExternalTableName.Text := FSourceTableName + '_EXT';
end;

procedure TfrmBulkClone.FillSourceCombos;
begin
  FillSourceServerCombo;
  FillSourceDBCombo;
  FillSourceTableCombo;
end;

function  TfrmBulkClone.FillSourceServerCombo: boolean;
var ServerList: TStringList;
begin
  result := false;
  comboxSourceServer.Items.Clear;
  btnExecute.Enabled := false;

  try

    try
      ServerList := GetServerListFromTreeView;
      comboxSourceServer.Items.Assign(ServerList);

      if comboxSourceServer.Items.Count > 0 then
      begin
        comboxSourceServer.ItemIndex := 0;
        result := true;
        FillSourceDBCombo;
      end;

    except
      raise;
    end;

  finally
    ServerList.Free;
  end;
end;

function  TfrmBulkClone.FillSourceDBCombo: boolean;
var i: integer;
begin
  result := false;
  comboxSourceDB.Items.Clear;

  try
    for i := 0 to Length(RegisteredDatabases) - 1 do
      if Trim(RegisteredDatabases[i].RegRec.ServerName) = Trim(comboxSourceServer.Text) then
        comboxSourceDB.Items.Add(Trim(RegisteredDatabases[i].RegRec.Title));

    if comboxSourceDB.Items.Count > 0 then
    begin
      result := true;
      comboxSourceDB.ItemIndex := 0;

      if ConfigureSourceConnection then
        FillSourceTableCombo
      else
        comboxSourceTables.Items.Clear;

    end;

  except
    raise;
  end;

end;

function  TfrmBulkClone.FillSourceTableCombo: boolean;
begin
  result := false;
  comboxSourceTables.Items.Clear;

  try
    IBDBSource.GetTableNames(comboxSourceTables.Items);

    if comboxSourceTables.Items.Count > 0 then
    begin
      result := true;
      comboxSourceTables.ItemIndex := 0;
      FSourceTableName := Trim(comboxSourceTables.Text);
      edtExternalTableName.Text := FSourceTableName + '_EXT';

      if IBDBDest.Connected then
        btnExecute.Enabled := true;

    end else
    begin
      edtExternalTableName.Text := '';
    end;

  except
    raise;
  end;
end;

function TfrmBulkClone.ConfigureSourceConnection: boolean;
var DBRec: TDatabaseRec;
begin
   result := false;
   try
     if IBDBSource.Connected then
       IBDBSource.Connected := false;

     DBRec := GetDBRecByServerAndDBName(Trim(comboxSourceServer.Text), Trim(comboxSourceDB.Text));

     If not Assigned(DBRec.IBDatabase) then exit;

     IBDBSource    := DBRec.IBDatabase;
     IBTransSource := DBRec.IBTransaction;
     IBQuerySource := DBRec.IBQuery;

     IBDBSource.Connected := true;

     if not IBTransSource.InTransaction then
       IBTransSource.StartTransaction;

     result := true;
   except
     raise;
   end;
end;

//DestinationServer/////////////////////////////////////////////////////////////
procedure TfrmBulkClone.comboxDestServerChange(Sender: TObject);
begin
  FillDestDBCombo;
end;

procedure TfrmBulkClone.comboxDestDBChange(Sender: TObject);
begin
  ConfigureDestConnection;
end;

procedure TfrmBulkClone.FillDestCombos;
begin
  FillDestServerCombo;
  FillDestDBCombo;
end;

function TfrmBulkClone.FillDestServerCombo: boolean;
var ServerList: TStringList;
begin
  result := false;
  comboxDestServer.Items.Clear;
  btnExecute.Enabled := false;

  try

    try
      ServerList := GetServerListFromTreeView;
      comboxDestServer.Items.Assign(ServerList);

      if comboxDestServer.Items.Count > 0 then
      begin
        result := true;
        comboxDestServer.ItemIndex := 0;
      end;

    except
      raise;
    end;

  finally
    ServerList.Free;
  end;
end;

function TfrmBulkClone.FillDestDBCombo: boolean;
var i: integer;
begin
  result := false;
  comboxDestDB.Items.Clear;

  try
    for i := 0 to Length(RegisteredDatabases) - 1 do
      if Trim(RegisteredDatabases[i].RegRec.ServerName) = Trim(comboxDestServer.Text) then
        comboxDestDB.Items.Add(Trim(RegisteredDatabases[i].RegRec.Title));

    if comboxDestDB.Items.Count > 0 then
    begin
      result := true;
      comboxDestDB.ItemIndex := 0;

      if ConfigureDestConnection then
      begin
        if IBDBSource.Connected then
          btnExecute.Enabled := true;
      end;
    end;

  except
    raise;
  end;

end;

function TfrmBulkClone.ConfigureDestConnection: boolean;
var DBRec: TDatabaseRec;
begin
   result := false;
   btnExecute.Enabled := false;

   try
     if IBDBDest.Connected then
       IBDBDest.Connected := false;

     DBRec := GetDBRecByServerAndDBName(Trim(comboxDestServer.Text), Trim(comboxDestDB.Text));

     If not Assigned(DBRec.IBDatabase) then exit;

     IBDBDest    := DBRec.IBDatabase;
     IBTransDest := DBRec.IBTransaction;
     IBQueryDest := DBRec.IBQuery;

     IBDBDest.Connected := true;

     if not IBTransDest.InTransaction then
       IBTransDest.StartTransaction;

     IBXScript1.Database := IBDBDest;
     IBXScript1.Transaction := IBTransDest;

     if IBDBSource.Connected then
       btnExecute.Enabled := true;

     result := true;
   except
     raise;
   end;
end;
//end-DestinationServer

procedure TfrmBulkClone.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmBulkClone.GetFormat: TExternalFormat;
begin
  if rbBinary.Checked then
    Result := efBinary
  else if rbFixedText.Checked then
    Result := efFixedText
  else
    Result := efCSV;
end;

function TfrmBulkClone.GenerateSQL: string;
begin
  case GetFormat of
    efBinary:    Result := GenerateSQLBinary;
    efFixedText: Result := GenerateSQLFixedText;
    efCSV:       Result := GenerateSQLCSV;
  end;
end;
function TfrmBulkClone.GenerateSQLBinary: string;
var
  FieldDefs: string;
begin
  // Felder aus der Quelltabelle erzeugen
  FieldDefs := BuildFieldList(comboxSourceTables.Text);

  Result :=
    'CREATE TABLE ' + Trim(edtExternalTableName.Text) + sLineBreak +
    'EXTERNAL FILE ''' + Trim(edtExternalFile.Text) + '''' + sLineBreak +
    '(' + sLineBreak +
    FieldDefs + sLineBreak +
    ');' + sLineBreak + sLineBreak;
end;

function TfrmBulkClone.GenerateSQLFixedText: string;
begin
  Result :=
    'CREATE TABLE ' + Trim(edtExternalTableName.Text) + sLineBreak +
    'EXTERNAL FILE ''' + Trim(edtExternalFile.Text) + '''' + sLineBreak +
    '(' + sLineBreak +
    '   LINE CHAR(1024)' + sLineBreak +
    ');' + sLineBreak + sLineBreak;
end;

function TfrmBulkClone.GenerateSQLCSV: string;
var
  ExtTable, FileName, Header: string;
  LineLen: Integer;
begin
  ExtTable := Trim(edtExternalTableName.Text);
  FileName := Trim(edtExternalFile.Text);
  LineLen := GetCSVLineLength(comboxSourceTables.Text);
  Header := BuildCSVHeader(comboxSourceTables.Text);

  Result := '';

  // -----------------------------
  // CREATE EXTERNAL lbSourceTable
  // -----------------------------
  Result := Result +
    'CREATE TABLE ' + ExtTable + sLineBreak +
    'EXTERNAL FILE ''' + FileName + '''' + sLineBreak +
    '(' + sLineBreak +
    '  LINE CHAR(' + IntToStr(LineLen) + ')' + sLineBreak +
    ');' + sLineBreak + sLineBreak;

  // -----------------------------
  // Header-Zeile optional
  // -----------------------------
  if chkBoxOutputHeader.Checked then
  begin
    Result := Result +
      'INSERT INTO ' + ExtTable +
      ' VALUES (''' + Header + ''');' + sLineBreak + sLineBreak;
  end;
end;


procedure TfrmBulkClone.btnExecuteClick(Sender: TObject);
var
  SQLBatch: TStringList;
  ProgressForm: TForm;
  ProgressLabel, lblStart, lblEnd, lblElapsed: TLabel;
  ProgressBar: TProgressBar;
  StartTime, EndTime: TDateTime;
  recCount, batchSize, batchCount, batchIndex, fromRow, toRow: Integer;
  rowsPerSec: Double;
  SQL: string;
begin
  if (not IBDBSource.Connected) or (not IBDBDest.Connected) then exit;

  //DatabaseName includes ServerName (like mxlinux/3130:/opt/firebird/examples/empbuild/employee.fdb)
  SkipImport := CompareText(Trim(IBDBSource.DatabaseName), Trim(IBDBDest.DatabaseName)) = 0;

  batchSize  := StrToInt(edtDefaultBatchSize.Text);

  if not IBTransSource.InTransaction then
    IBTransSource.StartTransaction;

  SQLBatch := TStringList.Create;
  try
    // -----------------------------
    // Prüfen & ggf. Source External Table erstellen
    // -----------------------------
    if not ExternalTableExists(IBDBSource, edtExternalTableName.Text) then
      CreateExternalTableIfNotExists(IBDBSource, IBTransSource, edtExternalTableName.Text, edtExternalFile.Text);

    // -----------------------------
    // Prüfen & ggf. Destination External Table erstellen
    // -----------------------------
    if not ExternalTableExists(IBDBDest, edtExternalTableName.Text) then
      CreateExternalTableIfNotExists(IBDBDest, IBTransDest, edtExternalTableName.Text, edtExternalFile.Text);

    // -----------------------------
    // Prüfen & ggf. Destination Tabelle erstellen
    // -----------------------------
    if not TableExists(IBDBDest, comboxSourceTables.Text) then
      if not CreateDestTable(IBDBDest, IBTransDest, comboxSourceTables.Text) then
        raise Exception.Create('Could not create destination table: ' + comboxSourceTables.Text);

    // -----------------------------
    // Record Count
    // -----------------------------
    IBQuerySource.Close;
    IBQuerySource.SQL.Text := 'SELECT COUNT(*) FROM ' + comboxSourceTables.Text;
    IBQuerySource.Open;
    recCount := IBQuerySource.Fields[0].AsInteger;
    IBQuerySource.Close;

    batchCount := (recCount + batchSize - 1) div batchSize;

    // -----------------------------
    // Progress Window
    // -----------------------------
    ProgressForm := TForm.Create(nil);
    try
      ProgressForm.FormStyle := fsStayOnTop;
      ProgressForm.Caption := 'Export/Import running...';
      ProgressForm.Width := 500;
      ProgressForm.Height := 200;
      ProgressForm.Position := poScreenCenter;
      ProgressForm.BorderStyle := bsDialog;

      ProgressLabel := TLabel.Create(ProgressForm); ProgressLabel.Parent := ProgressForm; ProgressLabel.Left := 16; ProgressLabel.Top := 16;
      ProgressLabel.Caption := Format('Total Records: %d', [recCount]);

      ProgressBar := TProgressBar.Create(ProgressForm); ProgressBar.Parent := ProgressForm;
      ProgressBar.Left := 16; ProgressBar.Top := 45; ProgressBar.Width := 460; ProgressBar.Height := 20;
      ProgressBar.Min := 0; ProgressBar.Max := recCount;

      lblStart := TLabel.Create(ProgressForm); lblStart.Parent := ProgressForm; lblStart.Left := 16; lblStart.Top := 110;
      lblEnd := TLabel.Create(ProgressForm); lblEnd.Parent := ProgressForm; lblEnd.Left := 16; lblEnd.Top := 130;
      lblElapsed := TLabel.Create(ProgressForm); lblElapsed.Parent := ProgressForm; lblElapsed.Left := 16; lblElapsed.Top := 150;

      ProgressForm.Show;
      Application.ProcessMessages;

      // -----------------------------
      // 4️⃣ START EXPORT (Source DB → External Table)
      // -----------------------------
      StartTime := Now;
      lblStart.Caption := 'Start: ' + FormatDateTime('hh:nn:ss', StartTime);

      for batchIndex := 0 to batchCount - 1 do
      begin
        fromRow := batchIndex * batchSize;
        toRow := (batchIndex + 1) * batchSize;
        if toRow > recCount then toRow := recCount;

        IBQuerySource.Close;

        case GetFormat of
          efCSV:
            IBQuerySource.SQL.Text :=
              'INSERT INTO ' + edtExternalTableName.Text +
              ' SELECT FIRST ' + IntToStr(toRow - fromRow) +
              ' SKIP ' + IntToStr(fromRow) + ' ' +
              BuildCSVView(comboxSourceTables.Text) +
              ' FROM ' + comboxSourceTables.Text;
          efBinary, efFixedText:
            IBQuerySource.SQL.Text :=
              Format('INSERT INTO %s SELECT FIRST %d SKIP %d * FROM %s',
                     [edtExternalTableName.Text, toRow - fromRow, fromRow, comboxSourceTables.Text]);
        end;

        IBQuerySource.ExecSQL;
        IBTransSource.CommitRetaining;

        ProgressBar.Position := toRow;
        ProgressLabel.Caption := Format('Exported %d of %d rows...', [toRow, recCount]);
        lblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime);
        Application.ProcessMessages;
      end;

      // -----------------------------
      // 5️⃣ START IMPORT (External Table → Destination DB)
      // -----------------------------

      IBDBSource.Connected := false;
      ;
      if not SkipImport then
      for batchIndex := 0 to batchCount - 1 do
      begin
        fromRow := batchIndex * batchSize;
        toRow := (batchIndex + 1) * batchSize;
        if toRow > recCount then toRow := recCount;

        IBQueryDest.Close;

        //inser data from ext table to fb table
        IBQueryDest.SQL.Text :=
          Format('INSERT INTO %s SELECT FIRST %d SKIP %d * FROM %s',
                 [comboxSourceTables.Text, toRow - fromRow, fromRow, edtExternalTableName.Text]);

        IBQueryDest.ExecSQL;
        ShowMessage(IBQueryDest.SQL.Text);
        IBTransDest.CommitRetaining;

        ProgressBar.Position := toRow;
        ProgressLabel.Caption := Format('Imported %d of %d rows...', [toRow, recCount]);
        lblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime);
        Application.ProcessMessages;
      end;

     if SkipImport then
     begin
       MessageDlg(
         'Source and Destination database are identical.' + sLineBreak +
          'Only export to the external file was executed.' + sLineBreak +
          'Import step was skipped.',
          mtInformation, [mbOK], 0);
      end;

      // -----------------------------
      // Finish
      // -----------------------------
      EndTime := Now;
      lblEnd.Caption := 'End: ' + FormatDateTime('hh:nn:ss', EndTime);
      lblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', EndTime - StartTime);

      rowsPerSec := recCount / ((EndTime - StartTime) * 24*60*60);
      ProgressLabel.Caption :=
        Format('Export/Import complete. %d rows processed (%.2f rows/sec).', [recCount, rowsPerSec]);
      ProgressBar.Position := ProgressBar.Max;

      MessageDlg(ProgressLabel.Caption, mtInformation, [mbOK], 0);

    finally
      ProgressForm.Free;
    end;

  finally
    SQLBatch.Free;
  end;
end;

function TfrmBulkClone.ExternalTableExists(DB: TIBDatabase; TableName: string): Boolean;
var
  Q: TIBQuery;
begin
  Result := False;
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.Transaction := DB.DefaultTransaction;
    Q.AllowAutoActivateTransaction := true;
    Q.SQL.Text :=
      'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
      'WHERE UPPER(RDB$RELATION_NAME) = :T AND RDB$EXTERNAL_FILE IS NOT NULL';
    Q.ParamByName('T').AsString := UpperCase(TableName);
    Q.Open;
    Result := not Q.EOF;
    Q.Close;
  finally
    Q.Free;
  end;
end;

function TfrmBulkClone.TableExists(DB: TIBDatabase; TableName: string): Boolean;
var
  Q: TIBQuery;
begin
  Result := False;
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.Transaction := DB.DefaultTransaction;
    Q.AllowAutoActivateTransaction := true;
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

function TfrmBulkClone.CreateDestTable(DestDB: TIBDatabase; DestTrans: TIBTransaction; TableName: string): Boolean;
var
  SQL: TStringList;
  FieldDefs: string;
begin
  Result := False;

  if TableExists(DestDB, TableName) then
  begin
    Result := True;
    Exit;
  end;

  // Felder von der Source Tabelle abrufen
  FieldDefs := BuildFieldList(TableName);

  // CREATE TABLE Statement generieren
  SQL := TStringList.Create;
  try
    SQL.Text :=
      'CREATE TABLE ' + TableName + sLineBreak +
      '(' + sLineBreak +
      FieldDefs + sLineBreak +
      ');';

    IBXScript1.Database := DestDB;
    IBXScript1.Transaction := DestTrans;
    IBXScript1.RunScript(SQL);

    Result := True;
  finally
    SQL.Free;
  end;
end;

function TfrmBulkClone.CreateExternalTableIfNotExists(DB: TIBDatabase; Transaction: TIBTransaction;
  TableName, FileName: string): Boolean;
var
  SQLBatch: TStringList;
begin
  Result := False;
  if ExternalTableExists(DB, TableName) then
  begin
    Result := True;
    Exit;
  end;

  SQLBatch := TStringList.Create;
  try
    // Nutze deine GenerateSQLBinary / CSV / FixedText
    case GetFormat of
      efBinary: SQLBatch.Text := GenerateSQLBinary;
      efFixedText: SQLBatch.Text := GenerateSQLFixedText;
      efCSV: SQLBatch.Text := GenerateSQLCSV;
    end;

    IBXScript1.Database := DB;
    IBXScript1.Transaction := Transaction;
    IBXScript1.RunScript(SQLBatch);
    Result := True;
  finally
    SQLBatch.Free;
  end;
end;


procedure TfrmBulkClone.btnOpenExternalFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtExternalFile.Text := OpenDialog1.FileName;
end;

procedure TfrmBulkClone.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  CloseAction := caFree;
end;


procedure TfrmBulkClone.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

function TfrmBulkClone.BuildFieldList(ATable: string): string;
var
  FieldName : string;
  FieldType : integer;
  FieldLen  : integer;
  FieldScale: integer;
  Line      : string;
begin
  Result := '';

  IBQuerySource.Close;
  IBQuerySource.SQL.Text :=
    'SELECT '+
    'RF.RDB$FIELD_NAME, '+
    'F.RDB$FIELD_TYPE, '+
    'F.RDB$FIELD_LENGTH, '+
    'F.RDB$FIELD_SCALE '+
    'FROM RDB$RELATION_FIELDS RF '+
    'JOIN RDB$FIELDS F ON RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+
    'WHERE RF.RDB$RELATION_NAME = :T '+
    'ORDER BY RF.RDB$FIELD_POSITION';

  IBQuerySource.ParamByName('T').AsString := UpperCase(ATable);
  IBQuerySource.Open;

  while not IBQuerySource.EOF do
  begin
    FieldName  := Trim(IBQuerySource.Fields[0].AsString);
    FieldType  := IBQuerySource.Fields[1].AsInteger;
    FieldLen   := IBQuerySource.Fields[2].AsInteger;
    FieldScale := IBQuerySource.Fields[3].AsInteger;

    case FieldType of

      7:  Line := FieldName + ' SMALLINT';
      8:  Line := FieldName + ' INTEGER';
      16: Line := FieldName + ' BIGINT';

      10: Line := FieldName + ' FLOAT';
      27: Line := FieldName + ' DOUBLE PRECISION';

      12: Line := FieldName + ' DATE';
      13: Line := FieldName + ' TIME';
      35: Line := FieldName + ' TIMESTAMP';

      14: Line := FieldName + ' CHAR(' + IntToStr(FieldLen) + ')';

      37: Line := FieldName + ' CHAR(' + IntToStr(FieldLen) + ')'; // VARCHAR -> CHAR

    else
      Line := '-- unsupported field: ' + FieldName;
    end;

    if Result <> '' then
      Result := Result + ',' + sLineBreak;

    Result := Result + '   ' + Line;

    IBQuerySource.Next;
  end;

  IBQuerySource.Close;
end;

function TfrmBulkClone.BuildCSVView(ATable: string): string;
var
  FieldName : string;
  Line      : string;
  Quote     : string;
  Sep       : string;
begin
  Result := '';

  Quote := edtCSVQuote.Text;
  Sep   := edtCSVSeparator.Text;
  if Sep = '' then Sep := ',';

  IBQuerySource.Close;
  IBQuerySource.SQL.Text :=
    'SELECT RF.RDB$FIELD_NAME ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'WHERE RF.RDB$RELATION_NAME = :T ' +
    'ORDER BY RF.RDB$FIELD_POSITION';
  IBQuerySource.ParamByName('T').AsString := UpperCase(ATable);
  IBQuerySource.Open;

  while not IBQuerySource.EOF do
  begin
    FieldName := Trim(IBQuerySource.Fields[0].AsString);

    if Result <> '' then
      Result := Result + ' || ''' + Sep + ''' || ';

    // Wert in String konvertieren
    Line := 'COALESCE(CAST(' + FieldName + ' AS VARCHAR(500)), '''')';

    // Quote hinzufügen und Escapes
    if Quote <> '' then
      Line := '''' + Quote + ''' || REPLACE(' + Line + ', ''' + Quote + ''', ''' + Quote + Quote + ''') || ''' + Quote + '''';

    Result := Result + Line;

    IBQuerySource.Next;
  end;

  IBQuerySource.Close;
end;

function TfrmBulkClone.BuildCSVHeader(ATable: string): string;
var
  FieldName: string;
  Quote: string;
  Sep: string;
begin
  Result := '';

  Quote := edtCSVQuote.Text;
  Sep   := edtCSVSeparator.Text;
  if Sep = '' then Sep := ',';

  IBQuerySource.Close;
  IBQuerySource.SQL.Text :=
    'SELECT RF.RDB$FIELD_NAME ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'WHERE RF.RDB$RELATION_NAME = :T ' +
    'ORDER BY RF.RDB$FIELD_POSITION';
  IBQuerySource.ParamByName('T').AsString := UpperCase(ATable);
  IBQuerySource.Open;

  while not IBQuerySource.EOF do
  begin
    FieldName := Trim(IBQuerySource.Fields[0].AsString);

    if Result <> '' then
      Result := Result + Sep;

    if Quote <> '' then
      Result := Result + Quote + StringReplace(FieldName, Quote, Quote + Quote, [rfReplaceAll]) + Quote
    else
      Result := Result + FieldName;

    IBQuerySource.Next;
  end;

  IBQuerySource.Close;
end;

function TfrmBulkClone.GetCSVLineLength(ATable: string): integer;
var
  MaxLen: integer;
begin
  MaxLen := 0;

  // Wir nehmen die Summe der Feldgrößen + Quotes + Separatoren
  IBQuerySource.Close;
  IBQuerySource.SQL.Text :=
    'SELECT RF.RDB$FIELD_NAME, F.RDB$FIELD_LENGTH ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'JOIN RDB$FIELDS F ON RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ' +
    'WHERE RF.RDB$RELATION_NAME = :T';
  IBQuerySource.ParamByName('T').AsString := UpperCase(ATable);
  IBQuerySource.Open;

  while not IBQuerySource.EOF do
  begin
    MaxLen := MaxLen + IBQuerySource.Fields[1].AsInteger; // Feldlänge

    // Separator + Quotes berücksichtigen
    if edtCSVQuote.Text <> '' then
      MaxLen := MaxLen + 2; // Quotes vorne/hinten
    MaxLen := MaxLen + 1;   // Separator
    IBQuerySource.Next;
  end;

  IBQuerySource.Close;

  {Result := MaxLen + 100; // Puffer einbauen
  if Result < 1024 then
    Result := 1024; // Minimum sinnvoll
  }
  Result := MaxLen;
end;

//reserve for Create View
function TfrmBulkClone.GenerateSQLCSV_CreateTable(ExtTable, FileName: string): string;
begin
  Result := 'CREATE TABLE ' + ExtTable + ' EXTERNAL FILE ''' + FileName + ''' (' + sLineBreak +
            '  LINE CHAR(1024)' + sLineBreak +
            ');' + sLineBreak;
end;

//reserve for Create View
function TfrmBulkClone.GenerateSQLCSV_CreateView(SrcTable, ExtTable, Quote, Sep: string): string;
var
  Fields: TStringList;
  i: Integer;
  line: string;
begin
  Fields := TStringList.Create;
  try
    IBDBSource.GetFieldNames(SrcTable, Fields);
    Result := 'CREATE VIEW V_' + ExtTable + ' AS' + sLineBreak +
              'SELECT ';

    line := '';
    for i := 0 to Fields.Count-1 do
    begin
      if line <> '' then
        line := line + ' || ''' + Sep + ''' || ';

      line := line +
        '''' + Quote + ''' || REPLACE(COALESCE(CAST(' + Fields[i] +
        ' AS VARCHAR(500)), ''''), ''' + Quote + ''', ''' + Quote + Quote + ''') || ''' + Quote + '''';
    end;

    Result := Result + line + sLineBreak +
              'AS CSV_LINE' + sLineBreak +
              'FROM ' + SrcTable + ';';
  finally
    Fields.Free;
  end;
end;

//reserve for Create View
{procedure TfrmBulkClone.btnExecuteClick(Sender: TObject);
var
  SQLBatch: TStringList;
  ProgressForm: TForm;
  ProgressLabel, lblStart, lblEnd, lblElapsed: TLabel;
  ProgressBar: TProgressBar;
  StartTime, EndTime: TDateTime;
  recCount, batchSize, batchCount, batchIndex, fromRow, toRow: Integer;
  rowsPerSec: Double;
  SourceName: string;
  SQL: string;
begin
  batchSize := 500000;

  IBXScript1.Database := IBDBSource;
  IBXScript1.Transaction := IBTransSource;

  if not IBTransSource.InTransaction then
    IBTransSource.StartTransaction;

  SQLBatch := TStringList.Create;
  try

    // -----------------------------
    // 1️⃣ CREATE EXTERNAL lbSourceTable
    // -----------------------------
    case GetFormat of

      efBinary:
        SQL := GenerateSQLBinary;

      efFixedText:
        SQL := GenerateSQLFixedText;

      efCSV:
        SQL := GenerateSQLCSV_CreateTable(
                 edtExternalTableName.Text,
                 edtExternalFile.Text
               );
    end;

    SQLBatch.Text := SQL;
    IBXScript1.RunScript(SQLBatch);

    // -----------------------------
    // 2️⃣ CSV benötigt zusätzlich VIEW
    // -----------------------------
    if GetFormat = efCSV then
    begin
      SQLBatch.Clear;

      SQL := GenerateSQLCSV_CreateView(
               comboxSourceTables.Text,
               edtExternalTableName.Text,
               edtCSVQuote.Text,
               edtCSVSeparator.Text
             );

      SQLBatch.Text := SQL;
      IBXScript1.RunScript(SQLBatch);

      SourceName := 'V_' + edtExternalTableName.Text;
    end
    else
      SourceName := comboxSourceTables.Text;

    if not chkExportData.Checked then
    begin
      ShowMessage('External table created.');
      Exit;
    end;

    // -----------------------------
    // 3️⃣ COUNT RECORDS
    // -----------------------------
    IBQuerySource.Close;
    IBQuerySource.SQL.Text := 'SELECT COUNT(*) FROM ' + comboxSourceTables.Text;
    IBQuerySource.Open;

    recCount := IBQuerySource.Fields[0].AsInteger;

    IBQuerySource.Close;

    batchCount := (recCount + batchSize - 1) div batchSize;

    // -----------------------------
    // 4️⃣ PROGRESS WINDOW
    // -----------------------------
    ProgressForm := TForm.Create(nil);
    try
      ProgressForm.FormStyle := fsStayOnTop;
      ProgressForm.Caption := 'Export running...';
      ProgressForm.Width := 500;
      ProgressForm.Height := 200;
      ProgressForm.Position := poScreenCenter;
      ProgressForm.BorderStyle := bsDialog;

      ProgressLabel := TLabel.Create(ProgressForm);
      ProgressLabel.Parent := ProgressForm;
      ProgressLabel.Left := 16;
      ProgressLabel.Top := 16;
      ProgressLabel.Caption := Format('Total Records: %d', [recCount]);

      ProgressBar := TProgressBar.Create(ProgressForm);
      ProgressBar.Parent := ProgressForm;
      ProgressBar.Left := 16;
      ProgressBar.Top := 45;
      ProgressBar.Width := 460;
      ProgressBar.Height := 20;
      ProgressBar.Min := 0;
      ProgressBar.Max := recCount;

      lblStart := TLabel.Create(ProgressForm);
      lblStart.Parent := ProgressForm;
      lblStart.Left := 16;
      lblStart.Top := 110;

      lblEnd := TLabel.Create(ProgressForm);
      lblEnd.Parent := ProgressForm;
      lblEnd.Left := 16;
      lblEnd.Top := 130;

      lblElapsed := TLabel.Create(ProgressForm);
      lblElapsed.Parent := ProgressForm;
      lblElapsed.Left := 16;
      lblElapsed.Top := 150;

      ProgressForm.Show;
      Application.ProcessMessages;

      // -----------------------------
      // 5️⃣ START EXPORT
      // -----------------------------
      StartTime := Now;

      lblStart.Caption := 'Start: ' +
        FormatDateTime('hh:nn:ss', StartTime);

      for batchIndex := 0 to batchCount - 1 do
      begin
        fromRow := batchIndex * batchSize;
        toRow := (batchIndex + 1) * batchSize;

        if toRow > recCount then
          toRow := recCount;

        IBQuerySource.Close;

        IBQuerySource.SQL.Text :=
          Format(
            'INSERT INTO %s SELECT FIRST %d SKIP %d * FROM %s',
            [
              edtExternalTableName.Text,
              batchSize,
              fromRow,
              SourceName
            ]
          );

        IBQuerySource.ExecSQL;
        IBTransSource.CommitRetaining;

        ProgressBar.Position := toRow;

        ProgressLabel.Caption :=
          Format('Exported %d of %d rows...', [toRow, recCount]);

        lblElapsed.Caption :=
          'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime);

        Application.ProcessMessages;
      end;

      // -----------------------------
      // 6️⃣ FINISH
      // -----------------------------
      EndTime := Now;

      lblEnd.Caption :=
        'End: ' + FormatDateTime('hh:nn:ss', EndTime);

      lblElapsed.Caption :=
        'Elapsed: ' + FormatDateTime('hh:nn:ss', EndTime - StartTime);

      rowsPerSec :=
        recCount / ((EndTime - StartTime) * 24 * 60 * 60);

      ProgressLabel.Caption :=
        Format(
          'Export complete. %d rows exported (%.2f rows/sec).',
          [recCount, rowsPerSec]
        );

      ProgressBar.Position := ProgressBar.Max;

      MessageDlg(
        ProgressLabel.Caption,
        mtInformation,
        [mbOK],
        0
      );

    finally
      ProgressForm.Free;
    end;

  finally
    SQLBatch.Free;
  end;
end;}

end.
