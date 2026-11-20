unit ForeignKeys;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Buttons,

  IBDatabase, IBXScript,

  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector,
  umessagedlg;

type

  { TfmForeignKeys }

  TfmForeignKeys = class(TForm)
    bbRefreshForeignKeys: TBitBtn;
    bbNewForeignKey: TBitBtn;
    bbDropForeignKey: TBitBtn;
    sgForeignKeys: TStringGrid;

    procedure bbRefreshClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;
    function ExecuteSQL(const ASQL: string): Boolean;
  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillForeignKeys;
  end;


implementation

{$R ForeignKeys.lfm}

uses Main, newForeignKey;

{ TfmForeignKeys }

procedure TfmForeignKeys.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Foreign Keys: ' + FTableName;
end;

procedure TfmForeignKeys.FillForeignKeys;
var
  FKs: TFBForeignKeyDefArray;
  i, Row: Integer;
begin
  sgForeignKeys.RowCount := 1;

  FKs := FExtractor.GetTableForeignKeys(FTableName);

  for i := 0 to High(FKs) do
  begin
    sgForeignKeys.RowCount := sgForeignKeys.RowCount + 1;
    Row := sgForeignKeys.RowCount - 1;

    sgForeignKeys.Cells[0, Row] := FKs[i].ConstraintName;   // Constraint Name
    sgForeignKeys.Cells[1, Row] := FKs[i].KeyName;          // Key Name
    sgForeignKeys.Cells[2, Row] := FKs[i].OnFields;         // On Fields
    sgForeignKeys.Cells[3, Row] := FKs[i].RefTable;         // Foreign Table
    sgForeignKeys.Cells[4, Row] := FKs[i].RefFields;        // Foreign Key (= RefFields)
    sgForeignKeys.Cells[5, Row] := FKs[i].UpdateRule;       // Update Rule
    sgForeignKeys.Cells[6, Row] := FKs[i].DeleteRule;       // Delete Rule
  end;

  if sgForeignKeys.RowCount > 1 then
    sgForeignKeys.Row := 1;
end;

procedure TfmForeignKeys.bbNewClick(Sender: TObject);
var
  FieldsList: TStringList;
  TableList: TStringList;
  RawFields: TFBFieldRawArray;
  i: Integer;
  ServerVersionMajor: Word;
begin
  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if ServerVersionMajor < 2 then
  begin
    MsgWarning(
      'Foreign Key creation is not supported on Firebird 1.5.' + sLineBreak +
      sLineBreak +
      'Firebird 1.5 requires exclusive database access when adding ' +
      'a new Foreign Key. TurboBird has active connections to this ' +
      'database, so the server will reject the operation.' + sLineBreak +
      sLineBreak +
      'Workaround:' + sLineBreak +
      '  Use isql or another tool with a single connection to create' + sLineBreak +
      '  the Foreign Key, then refresh this tab.');
    Exit;
  end;

  FieldsList := TStringList.Create;
  try
    RawFields := FExtractor.GetTableFieldsRaw(FTableName);
    for i := 0 to High(RawFields) do
      FieldsList.Add(RawFields[i].FieldName);

    fmNewForeignKey.clxOnFields.Clear;
    fmNewForeignKey.clxOnFields.Items.AddStrings(FieldsList);
  finally
    FieldsList.Free;
  end;

  fmNewForeignKey.edNewName.Text := 'FK_' + FTableName + '_' +
    IntToStr(sgForeignKeys.RowCount);

  TableList := TStringList.Create;
  try
    FExtractor.ExtractObjectNames(FDBIndex, otTables, false, TStrings(TableList), '');
    fmNewForeignKey.cbTables.Items.Clear;
    fmNewForeignKey.cbTables.Items.AddStrings(TableList);
  finally
    TableList.Free;
  end;

  fmNewForeignKey.DatabaseIndex := FDBIndex;
  fmNewForeignKey.laTable.Caption := FTableName;
  fmNewForeignKey.Caption := 'New Foreign Key for: ' + FTableName;

  // newForeignKey führt selbst aus; bei Erfolg mrOK
  if fmNewForeignKey.ShowModal = mrOK then
  begin
    turbocommon.MetaDataChanged := True;
    FillForeignKeys;
  end;
end;

procedure TfmForeignKeys.bbDropClick(Sender: TObject);
var
  FKName: string;
  Script: string;
begin
  if sgForeignKeys.Row <= 0 then
    Exit;

  FKName := Trim(sgForeignKeys.Cells[0, sgForeignKeys.Row]);
  if FKName = '' then
    Exit;

  if not MsgConfirm('Are you sure you want to drop ' + FKName + '?', mkWarning) then
    Exit;

  Script := 'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
            ' DROP CONSTRAINT ' + MakeCaseSensitiveAuto(FKName) + ';';

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FillForeignKeys;
  end;
end;

procedure TfmForeignKeys.bbRefreshClick(Sender: TObject);
begin
  FillForeignKeys;
end;

procedure TfmForeignKeys.FormShow(Sender: TObject);
begin
  FillForeignKeys;
  frmThemeSelector.btnApplyClick(Self);
end;

function TfmForeignKeys.ExecuteSQL(const ASQL: string): Boolean;
var
  Tr: TIBTransaction;
  ScriptEngine: TIBXScript;
  Lines: TStringList;
begin
  Result := False;

  if not RegisteredDatabases[FDBIndex].IBDatabase.Connected then
  begin
    MsgError('Database is not connected.');
    Exit;
  end;

  Tr := TIBTransaction.Create(nil);
  ScriptEngine := TIBXScript.Create(nil);
  Lines := TStringList.Create;
  try
    Tr.DefaultDatabase := RegisteredDatabases[FDBIndex].IBDatabase;
    Tr.Params.Clear;
    Tr.Params.Assign(RegisteredDatabases[FDBIndex].IBTransaction.Params);

    ScriptEngine.Transaction := Tr;
    ScriptEngine.Database    := RegisteredDatabases[FDBIndex].IBDatabase;
    Lines.Text := ASQL;

    try
      if not Tr.InTransaction then
        Tr.StartTransaction;

      ScriptEngine.ExecSQLScript(Lines.Text);

      if Tr.InTransaction then
        Tr.Commit;

      Result := True;
    except
      on E: Exception do
      begin
        if Tr.InTransaction then
          Tr.Rollback;
        MsgError('Error while executing SQL:' + sLineBreak + sLineBreak + E.Message);
        Result := False;
      end;
    end;
  finally
    Lines.Free;
    ScriptEngine.Free;
    Tr.Free;
  end;
end;

end.
