unit primarykey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Grids, ComCtrls, ExtCtrls, CheckLst,

  IBDatabase, IBXScript,

  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector,
  umessagedlg,
  confirm_query;

type

  { TfmPrimaryKey }

  TfmPrimaryKey = class(TForm)
    bbEdit: TBitBtn;
    bbRefresh: TBitBtn;
    bbClose: TButton;
    bbDrop: TBitBtn;
    chkLstBoxFields: TCheckListBox;
    pnlTop: TPanel;
    pnBottom: TPanel;
    sgPrimaryKey: TStringGrid;

    procedure bbEditClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure chkLstBoxFieldsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

    procedure LoadFields;        // Felder in CheckListBox laden
    procedure CheckPKFields;     // PK-Felder anhaken

    function ExecuteSQL(const ASQL: string): Boolean;
  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillPrimaryKey;

  end;


implementation

{$R *.lfm}

uses Main;

{ TfmPrimaryKey }

procedure TfmPrimaryKey.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Primary Key: ' + FTableName;

  sgPrimaryKey.Cells[0, 0] := 'Primary Key Definition';
  sgPrimaryKey.ColWidths[0] := 400;
end;

procedure TfmPrimaryKey.LoadFields;
var
  RawFields: TFBFieldRawArray;
  i: Integer;
begin
  chkLstBoxFields.Clear;

  RawFields := FExtractor.GetTableFieldsRaw(FTableName);
  for i := 0 to High(RawFields) do
    chkLstBoxFields.Items.Add(RawFields[i].FieldName);
end;

procedure TfmPrimaryKey.CheckPKFields;
var
  PKConstraintName, PKIndexName: string;
  FieldsList: TStringList;
  i, j: Integer;
begin
  // Alle Checkboxen deaktivieren
  for i := 0 to chkLstBoxFields.Count - 1 do
    chkLstBoxFields.Checked[i] := False;

  // Aktuelle PK-Felder ermitteln
  FieldsList := TStringList.Create;
  try
    PKIndexName := FExtractor.GetPrimaryKeyIndexName(FTableName, PKConstraintName);

    if PKIndexName <> '' then
    begin
      FExtractor.GetConstraintFields(PKIndexName, FieldsList);

      // PK-Felder anhaken
      for i := 0 to chkLstBoxFields.Count - 1 do
      begin
        for j := 0 to FieldsList.Count - 1 do
        begin
          if SameText(chkLstBoxFields.Items[i], FieldsList[j]) then
          begin
            chkLstBoxFields.Checked[i] := True;
            Break;
          end;
        end;
      end;
    end;

  finally
    FieldsList.Free;
  end;
end;

procedure TfmPrimaryKey.FillPrimaryKey;
var
  Items: TStringList;
  i: Integer;
begin
  sgPrimaryKey.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otPrimaryKeys, FTableName, [], AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgPrimaryKey.RowCount := i + 2;
      sgPrimaryKey.Cells[0, i + 1] := Items[i];
    end;

  finally
    Items.Free;
  end;

  // Felder laden und PK-Felder checken
  LoadFields;
  CheckPKFields;

  // Button-Status initial setzen
  chkLstBoxFieldsClickCheck(nil);
end;

procedure TfmPrimaryKey.bbEditClick(Sender: TObject);
var
  PKConstraintName, PKIndexName: string;
  CheckedFields: string;
  i: Integer;
  HasFields: Boolean;
  Script: string;
  DlgCaption: string;
begin
  CheckedFields := '';
  HasFields := False;
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    if chkLstBoxFields.Checked[i] then
    begin
      HasFields := True;
      if CheckedFields <> '' then
        CheckedFields := CheckedFields + ', ';
      CheckedFields := CheckedFields + MakeCaseSensitiveAuto(chkLstBoxFields.Items[i]);
    end;
  end;

  if not HasFields then
  begin
    MsgWarning('Please select at least one field for the Primary Key.');
    Exit;
  end;

  PKIndexName := FExtractor.GetPrimaryKeyIndexName(FTableName, PKConstraintName);

  Script := '';
  if PKConstraintName <> '' then
  begin
    Script := Script + '-- Drop existing Primary Key' + LineEnding;
    Script := Script + 'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
              ' DROP CONSTRAINT ' + MakeCaseSensitiveAuto(PKConstraintName) + ';' + LineEnding;
    Script := Script + LineEnding;
  end;

  Script := Script + '-- Create new Primary Key' + LineEnding;
  Script := Script + 'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
            ' ADD PRIMARY KEY (' + CheckedFields + ');' + LineEnding;

  DlgCaption := 'Edit Primary Key: ' + FTableName;

  if not TfrmConfirmQuery.Execute(DlgCaption, Script) then
    Exit;

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FillPrimaryKey;
  end;
end;

procedure TfmPrimaryKey.bbDropClick(Sender: TObject);
var
  PKConstraintName: string;
  PKIndexName: string;
  Script: string;
begin
  if sgPrimaryKey.RowCount <= 1 then
    Exit;

  if not MsgConfirm(
       'Are you sure you want to drop the Primary Key from ' + FTableName + '?',
       mkWarning) then
    Exit;

  PKIndexName := FExtractor.GetPrimaryKeyIndexName(FTableName, PKConstraintName);
  if PKConstraintName = '' then
  begin
    MsgWarning('No Primary Key constraint found.');
    Exit;
  end;

  Script := 'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
            ' DROP CONSTRAINT ' + MakeCaseSensitiveAuto(PKConstraintName) + ';';

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FillPrimaryKey;
  end;
end;

procedure TfmPrimaryKey.bbRefreshClick(Sender: TObject);
begin
  FillPrimaryKey;
end;

procedure TfmPrimaryKey.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmPrimaryKey.chkLstBoxFieldsClickCheck(Sender: TObject);
var
  i: Integer;
  HasChecked: Boolean;
  PKConstraintName, PKIndexName: string;
  HasPK: Boolean;
begin
  // Prüfen ob mindestens ein Feld gecheckt ist
  HasChecked := False;
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    if chkLstBoxFields.Checked[i] then
    begin
      HasChecked := True;
      Break;
    end;
  end;

  // Prüfen ob bereits ein PK existiert
  PKIndexName := FExtractor.GetPrimaryKeyIndexName(FTableName, PKConstraintName);
  HasPK := (PKConstraintName <> '');

  // Edit: aktiv wenn Felder gecheckt
  bbEdit.Enabled := HasChecked;

  // Drop: aktiv wenn PK existiert
  bbDrop.Enabled := HasPK;
end;

procedure TfmPrimaryKey.FormShow(Sender: TObject);
begin
  FillPrimaryKey;
  frmThemeSelector.btnApplyClick(Self);
end;

function TfmPrimaryKey.ExecuteSQL(const ASQL: string): Boolean;
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
