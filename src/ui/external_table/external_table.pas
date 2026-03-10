unit external_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, IBQuery, IBDatabase, IBDatabaseInfo, IBExtract,

  turbocommon,
  uthemeselector;

type

  TExternalFormat = (efBinary, efFixedText, efCSV);

  { TfrmExternalTable }

  TfrmExternalTable = class(TForm)
    btnCancel: TButton;
    btnExecute: TButton;
    btnOpenExternalFile: TButton;
    chkBoxOutputHeader: TCheckBox;
    comboxTables: TComboBox;
    edtCSVSeparator: TEdit;
    edtCSVQuote: TEdit;
    edtExternalFile: TEdit;
    edtExternalTableName: TEdit;
    gbFormat: TGroupBox;
    GroupBox1: TGroupBox;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo1: TIBDatabaseInfo;
    IBExtract1: TIBExtract;
    IBQuery1: TIBQuery;
    IBTransaction1: TIBTransaction;
    lblExternalFile: TLabel;
    lblExternalTableName: TLabel;
    lblSourceTable: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    rbBinary: TRadioButton;
    rbCSV: TRadioButton;
    rbFixedText: TRadioButton;
    chkCreateView: TCheckBox;
    chkExportData: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnOpenExternalFileClick(Sender: TObject);
    procedure comboxTablesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: integer;
    FFormat: TExternalFormat;

    FTableName: string;
    function GetFormat: TExternalFormat;
    function BuildFieldList(ATable: string): string;
    function BuildCSVView(ATable: string): string;
    function BuildCSVHeader(ATable: string): string;
    function GetCSVLineLength(ATable: string): integer;
  public
    procedure Init(dbIndex: integer; ANodeInfos: TPNodeInfos);
    function GenerateSQL: string;
  end;

//var
  //frmExternalTable: TfrmExternalTable;

implementation

{$R *.lfm}

{ TfrmExternalTable }


procedure TfrmExternalTable.Init(dbIndex: integer; ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
  FDBIndex   := dbIndex;

  IBDatabase1 := RegisteredDatabases[FDBIndex].IBDatabase;
  IBTransaction1 := RegisteredDatabases[FDBIndex].IBTransaction;
  IBQuery1 := RegisteredDatabases[FDBIndex].IBQuery;

  if not IBDatabase1.Connected then
    IBDatabase1.Connected := true;

  IBDatabase1.GetTableNames(comboxTables.Items);
  if comboxTables.Items.Count > 0 then
    comboxTables.ItemIndex := 0;

 edtExternalTableName.Text := FTableName + '_EXT';
end;

procedure TfrmExternalTable.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmExternalTable.GetFormat: TExternalFormat;
begin
  if rbBinary.Checked then
    Result := efBinary
  else if rbFixedText.Checked then
    Result := efFixedText
  else
    Result := efCSV;
end;

procedure TfrmExternalTable.btnExecuteClick(Sender: TObject);
var
  SQL: string;
  UserChoice: Integer;
begin
  SQL := GenerateSQL;

  // Dialog anzeigen: SQL + Execute / Cancel
  UserChoice := MessageDlg(
    'Generated SQL:' + sLineBreak + sLineBreak + SQL + sLineBreak +
    'Do you want to execute this SQL now?',
    mtConfirmation,
    [mbYes, mbNo],
    0
  );

  if UserChoice = mrYes then
  begin
    try
      IBTransaction1.StartTransaction;
      IBQuery1.SQL.Text := SQL;
      IBQuery1.ExecSQL;
      IBTransaction1.Commit;
      ShowMessage('SQL executed successfully!');
    except
      on E: Exception do
      begin
        if IBTransaction1.InTransaction then
          IBTransaction1.Rollback;
        ShowMessage('Error executing SQL: ' + E.Message);
      end;
    end;
  end
  else
    ShowMessage('Execution canceled.');
end;

procedure TfrmExternalTable.btnOpenExternalFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtExternalFile.Text := OpenDialog1.FileName;
end;

procedure TfrmExternalTable.comboxTablesChange(Sender: TObject);
begin
  FTableName := Trim(comboxTables.Text);
  edtExternalTableName.Text := FTableName + '_EXT';
end;

procedure TfrmExternalTable.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  CloseAction := caFree;
end;

procedure TfrmExternalTable.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

function TfrmExternalTable.GenerateSQL: string;
var
  ExtTable : string;
  SrcTable : string;
  FileName : string;
  LineLength: integer;
begin
  SrcTable := Trim(comboxTables.Text);
  ExtTable := Trim(edtExternalTableName.Text);
  FileName := Trim(edtExternalFile.Text);

  Result := '';

  case GetFormat of
    efCSV:
      begin
        // Länge der External Table LINE CHAR berechnen
        LineLength := GetCSVLineLength(SrcTable);

        // CREATE TABLE
        Result := 'CREATE TABLE ' + ExtTable + sLineBreak +
                  'EXTERNAL FILE ''' + FileName + '''' + sLineBreak +
                  '(' + sLineBreak +
                  '   LINE CHAR(' + IntToStr(LineLength) + ')' + sLineBreak +
                  ');' + sLineBreak + sLineBreak;

        // Optional: Header als INSERT
        if chkBoxOutputHeader.Checked then
        begin
          Result := Result +
            'INSERT INTO ' + ExtTable + ' VALUES (''' +
            BuildCSVHeader(SrcTable) + ''');' + sLineBreak + sLineBreak;
        end;

        // View erzeugen für CSV Export
        Result := Result +
          'CREATE VIEW V_' + ExtTable + ' AS' + sLineBreak +
          'SELECT ' + BuildCSVView(SrcTable) + sLineBreak +
          'FROM ' + SrcTable + ';' + sLineBreak + sLineBreak;

        // Daten exportieren
        if chkExportData.Checked then
        begin
          Result := Result +
            'INSERT INTO ' + ExtTable + sLineBreak +
            'SELECT * FROM V_' + ExtTable + ';' + sLineBreak;
        end;
      end;

    efBinary, efFixedText:
      begin
        // Feldliste generieren
        Result := 'CREATE TABLE ' + ExtTable + sLineBreak +
                  'EXTERNAL FILE ''' + FileName + '''' + sLineBreak +
                  '(' + sLineBreak +
                  BuildFieldList(SrcTable) + sLineBreak +
                  ');' + sLineBreak + sLineBreak;

        // Daten exportieren
        if chkExportData.Checked then
        begin
          Result := Result +
            'INSERT INTO ' + ExtTable + sLineBreak +
            'SELECT * FROM ' + SrcTable + ';' + sLineBreak;
        end;

        // Optional: View erstellen
        if chkCreateView.Checked then
        begin
          Result := Result +
            'CREATE VIEW V_' + ExtTable + ' AS' + sLineBreak +
            'SELECT * FROM ' + ExtTable + ';' + sLineBreak;
        end;
      end;
  end;
end;

function TfrmExternalTable.BuildFieldList(ATable: string): string;
var
  FieldName : string;
  FieldType : integer;
  FieldLen  : integer;
  FieldScale: integer;
  Line      : string;
begin
  Result := '';

  IBQuery1.Close;
  IBQuery1.SQL.Text :=
    'SELECT '+
    'RF.RDB$FIELD_NAME, '+
    'F.RDB$FIELD_TYPE, '+
    'F.RDB$FIELD_LENGTH, '+
    'F.RDB$FIELD_SCALE '+
    'FROM RDB$RELATION_FIELDS RF '+
    'JOIN RDB$FIELDS F ON RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '+
    'WHERE RF.RDB$RELATION_NAME = :T '+
    'ORDER BY RF.RDB$FIELD_POSITION';

  IBQuery1.ParamByName('T').AsString := UpperCase(ATable);
  IBQuery1.Open;

  while not IBQuery1.EOF do
  begin
    FieldName  := Trim(IBQuery1.Fields[0].AsString);
    FieldType  := IBQuery1.Fields[1].AsInteger;
    FieldLen   := IBQuery1.Fields[2].AsInteger;
    FieldScale := IBQuery1.Fields[3].AsInteger;

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

    IBQuery1.Next;
  end;

  IBQuery1.Close;
end;

function TfrmExternalTable.BuildCSVView(ATable: string): string;
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

  IBQuery1.Close;
  IBQuery1.SQL.Text :=
    'SELECT RF.RDB$FIELD_NAME ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'WHERE RF.RDB$RELATION_NAME = :T ' +
    'ORDER BY RF.RDB$FIELD_POSITION';
  IBQuery1.ParamByName('T').AsString := UpperCase(ATable);
  IBQuery1.Open;

  while not IBQuery1.EOF do
  begin
    FieldName := Trim(IBQuery1.Fields[0].AsString);

    if Result <> '' then
      Result := Result + ' || ''' + Sep + ''' || ';

    // Wert in String konvertieren
    Line := 'COALESCE(CAST(' + FieldName + ' AS VARCHAR(500)), '''')';

    // Quote hinzufügen und Escapes
    if Quote <> '' then
      Line := '''' + Quote + ''' || REPLACE(' + Line + ', ''' + Quote + ''', ''' + Quote + Quote + ''') || ''' + Quote + '''';

    Result := Result + Line;

    IBQuery1.Next;
  end;

  IBQuery1.Close;
end;

function TfrmExternalTable.BuildCSVHeader(ATable: string): string;
var
  FieldName: string;
  Quote: string;
  Sep: string;
begin
  Result := '';

  Quote := edtCSVQuote.Text;
  Sep   := edtCSVSeparator.Text;
  if Sep = '' then Sep := ',';

  IBQuery1.Close;
  IBQuery1.SQL.Text :=
    'SELECT RF.RDB$FIELD_NAME ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'WHERE RF.RDB$RELATION_NAME = :T ' +
    'ORDER BY RF.RDB$FIELD_POSITION';
  IBQuery1.ParamByName('T').AsString := UpperCase(ATable);
  IBQuery1.Open;

  while not IBQuery1.EOF do
  begin
    FieldName := Trim(IBQuery1.Fields[0].AsString);

    if Result <> '' then
      Result := Result + Sep;

    if Quote <> '' then
      Result := Result + Quote + StringReplace(FieldName, Quote, Quote + Quote, [rfReplaceAll]) + Quote
    else
      Result := Result + FieldName;

    IBQuery1.Next;
  end;

  IBQuery1.Close;
end;

function TfrmExternalTable.GetCSVLineLength(ATable: string): integer;
var
  MaxLen: integer;
begin
  MaxLen := 0;

  // Wir nehmen die Summe der Feldgrößen + Quotes + Separatoren
  IBQuery1.Close;
  IBQuery1.SQL.Text :=
    'SELECT RF.RDB$FIELD_NAME, F.RDB$FIELD_LENGTH ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'JOIN RDB$FIELDS F ON RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ' +
    'WHERE RF.RDB$RELATION_NAME = :T';
  IBQuery1.ParamByName('T').AsString := UpperCase(ATable);
  IBQuery1.Open;

  while not IBQuery1.EOF do
  begin
    MaxLen := MaxLen + IBQuery1.Fields[1].AsInteger; // Feldlänge

    // Separator + Quotes berücksichtigen
    if edtCSVQuote.Text <> '' then
      MaxLen := MaxLen + 2; // Quotes vorne/hinten
    MaxLen := MaxLen + 1;   // Separator
    IBQuery1.Next;
  end;

  IBQuery1.Close;

  Result := MaxLen + 50; // Puffer einbauen
  if Result < 1024 then
    Result := 1024; // Minimum sinnvoll
end;

end.
