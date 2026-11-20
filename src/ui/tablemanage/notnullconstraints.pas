unit NotNullConstraints;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Grids, CheckLst,

  IBSQL, IBDatabase, IBXScript,

  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector,
  umessagedlg,
  confirm_query;

type

  { TfmNotNullConstraints }

  TfmNotNullConstraints = class(TForm)
    bbClose: TBitBtn;
    bbRefresh: TBitBtn;
    bbApply: TBitBtn;
    chkLstBoxFields: TCheckListBox;
    pnBottom: TPanel;
    pnlTop: TPanel;
    sgNotNullConstraints: TStringGrid;

    procedure bbApplyClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure chkLstBoxFieldsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;
    FOrigNotNullFields: TStringList;

    procedure LoadFields;
    function HasNullValues(const AFieldName: string): Boolean;
    function ExecuteSQL(const ASQL: string): Boolean;

  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillNotNullConstraints;
  end;

implementation

{$R *.lfm}

uses Main;

{ TfmNotNullConstraints }

procedure TfmNotNullConstraints.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  if not Assigned(FOrigNotNullFields) then
    FOrigNotNullFields := TStringList.Create
  else
    FOrigNotNullFields.Clear;

  // Event zuweisen – ohne Designer / ohne LFM-Änderung
  OnDestroy := @FormDestroy;

  Caption := 'Not Null Constraints: ' + FTableName;

  sgNotNullConstraints.Cells[0, 0] := 'Constraint Definition';
  sgNotNullConstraints.ColWidths[0] := 300;
end;

procedure TfmNotNullConstraints.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOrigNotNullFields);
end;

procedure TfmNotNullConstraints.LoadFields;
var
  RawFields: TFBFieldRawArray;
  i: Integer;
  FieldName: string;
begin
  chkLstBoxFields.Clear;
  FOrigNotNullFields.Clear;

  RawFields := FExtractor.GetTableFieldsRaw(FTableName);

  for i := 0 to High(RawFields) do
  begin
    FieldName := RawFields[i].FieldName;

    chkLstBoxFields.Items.Add(FieldName);
    chkLstBoxFields.Checked[chkLstBoxFields.Count - 1] := RawFields[i].NotNull;

    if RawFields[i].NotNull then
      FOrigNotNullFields.Add(FieldName);
  end;

  bbApply.Enabled := False;
end;

procedure TfmNotNullConstraints.FillNotNullConstraints;
var
  Items: TStringList;
  i: Integer;
begin
  sgNotNullConstraints.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otNotNullConstraints, FTableName, [],
      AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgNotNullConstraints.RowCount := i + 2;
      sgNotNullConstraints.Cells[0, i + 1] := Items[i];
    end;
  finally
    Items.Free;
  end;

  LoadFields;
end;

procedure TfmNotNullConstraints.chkLstBoxFieldsClickCheck(Sender: TObject);
var
  i: Integer;
  HasChanges: Boolean;
  FieldName: string;
  CurrentlyNotNull: Boolean;
  WasNotNull: Boolean;
begin
  HasChanges := False;
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    FieldName := chkLstBoxFields.Items[i];
    CurrentlyNotNull := chkLstBoxFields.Checked[i];
    WasNotNull := (FOrigNotNullFields.IndexOf(FieldName) >= 0);

    if CurrentlyNotNull <> WasNotNull then
    begin
      HasChanges := True;
      Break;
    end;
  end;

  bbApply.Enabled := HasChanges;
end;

function TfmNotNullConstraints.HasNullValues(const AFieldName: string): Boolean;
var
  Qry: TIBSQL;
begin
  Result := False;

  Qry := TIBSQL.Create(nil);
  try
    Qry.Database := FExtractor.FIBDatabase;
    Qry.Transaction := FExtractor.FIBTransaction;
    if not Qry.Transaction.InTransaction then
      Qry.Transaction.StartTransaction;

    Qry.SQL.Text :=
      'SELECT COUNT(*) FROM ' + MakeCaseSensitiveAuto(FTableName) + ' ' +
      'WHERE ' + MakeCaseSensitiveAuto(AFieldName) + ' IS NULL';
    Qry.ExecQuery;

    if not Qry.EOF then
      Result := Qry.Fields[0].AsInteger > 0;
  finally
    Qry.Free;
  end;
end;

function TfmNotNullConstraints.ExecuteSQL(const ASQL: string): Boolean;
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

procedure TfmNotNullConstraints.bbApplyClick(Sender: TObject);
var
  SQL: TStringList;
  i: Integer;
  FieldName: string;
  CurrentlyNotNull: Boolean;
  WasNotNull: Boolean;
  ServerVersionMajor: Word;
  NullCheckFailed: string;
  Script: string;
  DlgCaption: string;
begin
  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;
  NullCheckFailed := '';

  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    FieldName := chkLstBoxFields.Items[i];
    CurrentlyNotNull := chkLstBoxFields.Checked[i];
    WasNotNull := (FOrigNotNullFields.IndexOf(FieldName) >= 0);

    if CurrentlyNotNull and (not WasNotNull) then
    begin
      if HasNullValues(FieldName) then
      begin
        if NullCheckFailed <> '' then
          NullCheckFailed := NullCheckFailed + sLineBreak;
        NullCheckFailed := NullCheckFailed + '  - ' + FieldName;
      end;
    end;
  end;

  if NullCheckFailed <> '' then
  begin
    MsgError(
      'The following fields contain NULL values and cannot be set to NOT NULL:' +
      sLineBreak + sLineBreak +
      NullCheckFailed + sLineBreak + sLineBreak +
      'Please fill these NULL values first.');
    Exit;
  end;

  SQL := TStringList.Create;
  try
    for i := 0 to chkLstBoxFields.Count - 1 do
    begin
      FieldName := chkLstBoxFields.Items[i];
      CurrentlyNotNull := chkLstBoxFields.Checked[i];
      WasNotNull := (FOrigNotNullFields.IndexOf(FieldName) >= 0);

      if CurrentlyNotNull and (not WasNotNull) then
      begin
        if ServerVersionMajor >= 3 then
          SQL.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
            ' ALTER COLUMN ' + MakeCaseSensitiveAuto(FieldName) + ' SET NOT NULL;')
        else
          SQL.Add('UPDATE RDB$RELATION_FIELDS ' +
            'SET RDB$NULL_FLAG = 1 ' +
            'WHERE RDB$RELATION_NAME = ' + QuotedStr(StripQuotes(FTableName)) + ' ' +
            '  AND RDB$FIELD_NAME = ' + QuotedStr(StripQuotes(FieldName)) + ';');
      end
      else if (not CurrentlyNotNull) and WasNotNull then
      begin
        if ServerVersionMajor >= 3 then
          SQL.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
            ' ALTER COLUMN ' + MakeCaseSensitiveAuto(FieldName) + ' DROP NOT NULL;')
        else
          SQL.Add('UPDATE RDB$RELATION_FIELDS ' +
            'SET RDB$NULL_FLAG = NULL ' +
            'WHERE RDB$RELATION_NAME = ' + QuotedStr(StripQuotes(FTableName)) + ' ' +
            '  AND RDB$FIELD_NAME = ' + QuotedStr(StripQuotes(FieldName)) + ';');
      end;
    end;

    if SQL.Count = 0 then
    begin
      MsgBox('No changes detected.', mkInfo);
      Exit;
    end;

    Script := SQL.Text;
    DlgCaption := 'Modify Not Null Constraints: ' + FTableName;

    if not TfrmConfirmQuery.Execute(DlgCaption, Script) then
      Exit;

    if ExecuteSQL(Script) then
    begin
      turbocommon.MetaDataChanged := True;
      FillNotNullConstraints;
    end;

  finally
    SQL.Free;
  end;
end;

procedure TfmNotNullConstraints.bbRefreshClick(Sender: TObject);
begin
  FillNotNullConstraints;
end;

procedure TfmNotNullConstraints.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmNotNullConstraints.FormShow(Sender: TObject);
begin
  FillNotNullConstraints;
  frmThemeSelector.btnApplyClick(Self);
end;

end.
