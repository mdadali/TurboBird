unit NotNullConstraints;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Grids, CheckLst,

  IBSQL,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,

  uthemeselector;

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

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;
    FOrigNotNullFields: TStringList;  // Ursprüngliche NOT NULL Felder

    procedure LoadFields;
    function HasNullValues(const AFieldName: string): Boolean;


  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillNotNullConstraints;

  end;


implementation

{$R *.lfm}

uses Main, QueryWindow;

{ TfmNotNullConstraints }

procedure TfmNotNullConstraints.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;
  FOrigNotNullFields := TStringList.Create;

  Caption := 'Not Null Constraints: ' + FTableName;

  sgNotNullConstraints.Cells[0, 0] := 'Constraint Definition';
  sgNotNullConstraints.ColWidths[0] := 300;
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

  // Button-Status
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

  // Felder laden
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

// ============================================================
// Vorprüfung: enthält eine Spalte NULL-Werte?
// Auf FB 1.5 / 2.x prüft Firebird beim Setzen von NOT NULL nicht
// automatisch — das machen wir hier selbst, sonst inkonsistente DB.
// ============================================================
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

procedure TfmNotNullConstraints.bbApplyClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  SQL: TStringList;
  i: Integer;
  FieldName: string;
  CurrentlyNotNull: Boolean;
  WasNotNull: Boolean;
  ServerVersionMajor: Word;
  NullCheckFailed: string;
begin
  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  NullCheckFailed := '';

  // ============================================================
  // 1. Vorprüfung — nur für Felder, die NOT NULL werden sollen
  // ============================================================
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    FieldName := chkLstBoxFields.Items[i];
    CurrentlyNotNull := chkLstBoxFields.Checked[i];
    WasNotNull := (FOrigNotNullFields.IndexOf(FieldName) >= 0);

    // Wird gerade NOT NULL gesetzt UND war es vorher nicht
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
    MessageDlg(
      'The following fields contain NULL values and cannot be set to NOT NULL:' +
      sLineBreak + sLineBreak +
      NullCheckFailed + sLineBreak + sLineBreak +
      'Please fill these NULL values first.',
      mtError, [mbOK], 0);
    Exit;
  end;

  // ============================================================
  // 2. SQL generieren — Versions-Weiche
  // ============================================================
  SQL := TStringList.Create;
  try
    for i := 0 to chkLstBoxFields.Count - 1 do
    begin
      FieldName := chkLstBoxFields.Items[i];
      CurrentlyNotNull := chkLstBoxFields.Checked[i];
      WasNotNull := (FOrigNotNullFields.IndexOf(FieldName) >= 0);

      if CurrentlyNotNull and (not WasNotNull) then
      begin
        // SET NOT NULL
        if ServerVersionMajor >= 3 then
          SQL.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
            ' ALTER COLUMN ' + MakeCaseSensitiveAuto(FieldName) + ' SET NOT NULL ^;')
        else
          // FB 1.5 / 2.x — Workaround via Systemtabelle
          SQL.Add('UPDATE RDB$RELATION_FIELDS ' +
            'SET RDB$NULL_FLAG = 1 ' +
            'WHERE RDB$RELATION_NAME = ' + QuotedStr(FTableName) + ' ' +
            '  AND RDB$FIELD_NAME = ' + QuotedStr(FieldName) + ' ^;');
      end
      else if (not CurrentlyNotNull) and WasNotNull then
      begin
        // DROP NOT NULL
        if ServerVersionMajor >= 3 then
          SQL.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
            ' ALTER COLUMN ' + MakeCaseSensitiveAuto(FieldName) + ' DROP NOT NULL ^;')
        else
          // FB 1.5 / 2.x — Workaround via Systemtabelle
          SQL.Add('UPDATE RDB$RELATION_FIELDS ' +
            'SET RDB$NULL_FLAG = NULL ' +
            'WHERE RDB$RELATION_NAME = ' + QuotedStr(FTableName) + ' ' +
            '  AND RDB$FIELD_NAME = ' + QuotedStr(FieldName) + ' ^;');
      end;
    end;

    if SQL.Count = 0 then
    begin
      ShowMessage('No changes detected.');
      Exit;
    end;

    QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Modify Not Null Constraints: ' + FTableName);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('SET TERM ^;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.AddStrings(SQL);
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('SET TERM ;^');

    QWindow.OnCommit := @bbRefreshClick;
    QWindow.Show;

    // Metadaten haben sich geändert → TreeView neu laden
    turbocommon.MetaDataChanged := True;

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
  FOrigNotNullFields.Free;
end;

procedure TfmNotNullConstraints.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

end.
