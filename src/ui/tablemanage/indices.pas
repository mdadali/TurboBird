unit Indices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, StdCtrls, CheckLst,

  IBDatabase, IBXScript,

  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector,
  umessagedlg,
  confirm_query;

type

  { TfmIndices }

  TfmIndices = class(TForm)
    bbRefreshIndices: TBitBtn;
    bbDropIndices: TBitBtn;
    sgIndices: TStringGrid;
    GroupBox2: TGroupBox;
    cbIndexType: TComboBox;
    edIndexName: TEdit;
    Label3: TLabel;
    cxUnique: TCheckBox;
    cbSortType: TComboBox;
    bbCreateIndex: TBitBtn;
    Label1: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    clbFields: TCheckListBox;

    procedure bbRefreshClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbCreateClick(Sender: TObject);
    procedure cbIndexTypeChange(Sender: TObject);
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
    procedure FillIndices;
  end;


implementation

{$R *.lfm}

uses Main;

{ TfmIndices }

procedure TfmIndices.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Indices: ' + FTableName;
end;

procedure TfmIndices.FillIndices;
var
  Items: TStringList;
  i: Integer;
  RawFields: TFBFieldRawArray;
begin
  sgIndices.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otIndexes, FTableName, [],
      AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgIndices.RowCount := i + 2;
      sgIndices.Cells[0, i + 1] := Items[i];  // "IDX_NAME: UNIQUE ON (Feld1, Feld2)"
    end;
  finally
    Items.Free;
  end;

  // Felder für "Create Index" laden — ohne BLOBs
  edIndexName.Text := 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);

  clbFields.Clear;
  RawFields := FExtractor.GetTableFieldsRaw(FTableName);
  for i := 0 to High(RawFields) do
  begin
    // BLOB-Felder können nicht in Index aufgenommen werden.
    // 261 = RDB$FIELD_TYPE für BLOB (alle Versionen FB 1.5–6)
    if RawFields[i].FieldType <> 261 then
      clbFields.Items.Add(RawFields[i].FieldName);
  end;

  if sgIndices.RowCount > 1 then
    sgIndices.Row := 1;
end;

procedure TfmIndices.bbRefreshClick(Sender: TObject);
begin
  FillIndices;
end;

procedure TfmIndices.bbDropClick(Sender: TObject);
var
  Line, IndexName: string;
  ColonPos: Integer;
  Script: string;
begin
  if sgIndices.RowCount <= 1 then
    Exit;

  Line := sgIndices.Cells[0, sgIndices.Row];
  ColonPos := Pos(':', Line);
  if ColonPos > 0 then
    IndexName := Trim(Copy(Line, 1, ColonPos - 1))
  else
    IndexName := Trim(Line);

  if IndexName = '' then
    Exit;

  if not MsgConfirm('Are you sure you want to drop index: ' + IndexName + '?',
                    mkWarning) then
    Exit;

  Script := 'DROP INDEX ' + MakeCaseSensitiveAuto(IndexName) + ';';

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FillIndices;
  end;
end;

procedure TfmIndices.bbCreateClick(Sender: TObject);
var
  Fields: string;
  i: Integer;
  Script: string;
  FirstLine: string;
  DlgCaption: string;
begin
  Fields := '';
  for i := 0 to clbFields.Count - 1 do
    if clbFields.Checked[i] then
    begin
      if Fields <> '' then
        Fields := Fields + ', ';
      Fields := Fields + MakeCaseSensitiveAuto(Trim(clbFields.Items[i]));
    end;

  if Trim(Fields) = '' then
  begin
    MsgError('You should select at least one field.');
    Exit;
  end;

  if Trim(edIndexName.Text) = '' then
  begin
    MsgError('You should enter the new index name.');
    Exit;
  end;

  FirstLine := 'CREATE ';
  if cxUnique.Checked then
    FirstLine := FirstLine + 'UNIQUE ';

  // Direction: ASCENDING / DESCENDING
  if SameText(cbSortType.Text, 'Descending') then
    FirstLine := FirstLine + 'DESCENDING '
  else
    FirstLine := FirstLine + 'ASCENDING ';

  FirstLine := FirstLine + 'INDEX ' + MakeCaseSensitiveAuto(Trim(edIndexName.Text));

  Script := FirstLine + LineEnding +
            'ON ' + MakeCaseSensitiveAuto(FTableName) + ' (' + Fields + ');';

  DlgCaption := 'Create Index on table: ' + FTableName;

  if not TfrmConfirmQuery.Execute(DlgCaption, Script) then
    Exit;

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FillIndices;
  end;
end;

procedure TfmIndices.cbIndexTypeChange(Sender: TObject);
begin
  edIndexName.Text := 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);
end;

procedure TfmIndices.FormShow(Sender: TObject);
begin
  FillIndices;
  frmThemeSelector.btnApplyClick(Self);
end;

function TfmIndices.ExecuteSQL(const ASQL: string): Boolean;
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
