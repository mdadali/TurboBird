unit newForeignKey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, CheckLst,

  IBDatabase, IBXScript,

  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector,
  umessagedlg,
  confirm_query;


type

  { TfmNewForeignKey }

  TfmNewForeignKey = class(TForm)
    bbScript: TBitBtn;
    cbUpdateAction: TComboBox;
    cbTables: TComboBox;
    clxForFields: TCheckListBox;
    clxOnFields: TCheckListBox;
    edNewName: TEdit;
    cbDeleteAction: TComboBox;
    grBoxCurrentTable: TGroupBox;
    grForeignTable: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    laTable: TLabel;
    procedure bbScriptClick(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grForeignTableClick(Sender: TObject);
  private
    FExtractor: TSimpleObjExtractor;
    FExtractorDBIndex: Integer;
    procedure UpdateGroupTitles;
    function ExecuteSQL(const ASQL: string): Boolean;
  public
    DatabaseIndex: Integer;
  end;

var
  fmNewForeignKey: TfmNewForeignKey;

implementation

uses main;

{ TfmNewForeignKey }

// ============================================================
// Passt die Titel der beiden GroupBoxen an die aktuelle Auswahl an.
// - Linke Seite: "On fields of <CurrentTable>"
// - Rechte Seite: "Reference to <SelectedTable>" bzw. "(self)" bei
//                 Self-Referencing
// ============================================================
// ============================================================
// Passt die Titel der beiden GroupBoxen an die aktuelle Auswahl an.
// - Oben: aktuelle Tabelle (wo der FK angelegt wird)
// - Unten: Lookup-Tabelle (auf die referenziert wird)
// ============================================================
procedure TfmNewForeignKey.UpdateGroupTitles;
var
  CurrentTable, SelectedTable: string;
begin
  CurrentTable  := Trim(laTable.Caption);
  SelectedTable := Trim(cbTables.Text);

  // Obere GroupBox — die Tabelle, auf der der FK entsteht
  if CurrentTable <> '' then
    grBoxCurrentTable.Caption := 'Current table: ' + CurrentTable
  else
    grBoxCurrentTable.Caption := 'Current table';

  // Untere GroupBox — die Lookup-Tabelle
  if SelectedTable = '' then
    grForeignTable.Caption := 'Foreign Table'
  else if SameText(SelectedTable, CurrentTable) then
    grForeignTable.Caption := 'Foreign Table: ' + SelectedTable + ' (self)'
  else
    grForeignTable.Caption := 'Foreign Table: ' + SelectedTable;
end;

procedure TfmNewForeignKey.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);

  if not Assigned(FExtractor) or (FExtractorDBIndex <> DatabaseIndex) then
  begin
    if Assigned(FExtractor) then
      FreeAndNil(FExtractor);
    FExtractor := TSimpleObjExtractor.Create(DatabaseIndex);
    FExtractorDBIndex := DatabaseIndex;
  end;

  UpdateGroupTitles;
end;

procedure TfmNewForeignKey.grForeignTableClick(Sender: TObject);
begin

end;

procedure TfmNewForeignKey.FormDestroy(Sender: TObject);
begin
  if Assigned(FExtractor) then
    FreeAndNil(FExtractor);
end;

procedure TfmNewForeignKey.cbTablesChange(Sender: TObject);
var
  RawFields: TFBFieldRawArray;
  PKFields: TStringList;
  PKIndexName, PKConstraintName: string;
  i, idx: Integer;
begin
  clxForFields.Clear;

  if not Assigned(FExtractor) then
  begin
    UpdateGroupTitles;
    Exit;
  end;

  if Trim(cbTables.Text) = '' then
  begin
    UpdateGroupTitles;
    Exit;
  end;

  // ============================================================
  // 1. Felder der Ziel-Tabelle laden (roh, ohne Quotes)
  // ============================================================
  RawFields := FExtractor.GetTableFieldsRaw(cbTables.Text);
  for i := 0 to High(RawFields) do
    clxForFields.Items.Add(RawFields[i].FieldName);

  // ============================================================
  // 2. PK-Felder automatisch anhaken
  // ============================================================
  PKFields := TStringList.Create;
  try
    PKIndexName := FExtractor.GetPrimaryKeyIndexName(
      cbTables.Text, PKConstraintName);

    if PKIndexName <> '' then
      FExtractor.GetConstraintFields(PKIndexName, PKFields);

    for i := 0 to PKFields.Count - 1 do
    begin
      idx := clxForFields.Items.IndexOf(PKFields[i]);
      if idx >= 0 then
        clxForFields.Checked[idx] := True;
    end;
  finally
    PKFields.Free;
  end;

  // ============================================================
  // 3. Titel aktualisieren (Self-Referencing-Erkennung)
  // ============================================================
  UpdateGroupTitles;
end;

procedure TfmNewForeignKey.bbScriptClick(Sender: TObject);
var
  CurrFields, ForFields: string;
  i: Integer;
  TargetTable, RefTable: string;
  CountOn, CountFor: Integer;
  Script: string;
  DlgCaption: string;
begin
  if Trim(edNewName.Text) = '' then
  begin
    MsgWarning('Please enter a name for the Foreign Key constraint.');
    edNewName.SetFocus;
    Exit;
  end;

  if Trim(cbTables.Text) = '' then
  begin
    MsgWarning('Please select a foreign table.');
    cbTables.SetFocus;
    Exit;
  end;

  CountOn := 0;
  CountFor := 0;

  for i := 0 to clxOnFields.Count - 1 do
    if clxOnFields.Checked[i] then
      Inc(CountOn);

  for i := 0 to clxForFields.Count - 1 do
    if clxForFields.Checked[i] then
      Inc(CountFor);

  if CountOn = 0 then
  begin
    MsgWarning('Please select at least one field in "On fields".');
    clxOnFields.SetFocus;
    Exit;
  end;

  if CountFor = 0 then
  begin
    MsgWarning('Please select at least one reference field.');
    clxForFields.SetFocus;
    Exit;
  end;

  if CountOn <> CountFor then
  begin
    MsgWarning(
      'The number of fields must match on both sides.' + sLineBreak +
      sLineBreak +
      'On fields:         ' + IntToStr(CountOn) + sLineBreak +
      'Reference fields:  ' + IntToStr(CountFor));
    Exit;
  end;

  CurrFields := '';
  ForFields := '';

  for i := 0 to clxOnFields.Count - 1 do
    if clxOnFields.Checked[i] then
      CurrFields := CurrFields + MakeCaseSensitiveAuto(clxOnFields.Items[i]) + ', ';

  if CurrFields.EndsWith(', ') then
    CurrFields := Copy(CurrFields, 1, Length(CurrFields) - 2);

  for i := 0 to clxForFields.Count - 1 do
    if clxForFields.Checked[i] then
      ForFields := ForFields + MakeCaseSensitiveAuto(clxForFields.Items[i]) + ', ';

  if ForFields.EndsWith(', ') then
    ForFields := Copy(ForFields, 1, Length(ForFields) - 2);

  TargetTable := MakeCaseSensitiveAuto(laTable.Caption);
  RefTable    := MakeCaseSensitiveAuto(cbTables.Text);

  Script :=
    'ALTER TABLE ' + TargetTable +
    ' ADD CONSTRAINT ' + MakeCaseSensitiveAuto(edNewName.Text) +
    ' FOREIGN KEY (' + CurrFields + ')' +
    ' REFERENCES ' + RefTable + ' (' + ForFields + ')';

  if cbUpdateAction.Text <> 'Restrict' then
    Script := Script + LineEnding + ' ON UPDATE ' + cbUpdateAction.Text;
  if cbDeleteAction.Text <> 'Restrict' then
    Script := Script + LineEnding + ' ON DELETE ' + cbDeleteAction.Text;

  Script := Script + ';';

  DlgCaption := 'New Foreign Key on table: ' + TargetTable;

  if not TfrmConfirmQuery.Execute(DlgCaption, Script) then
    Exit;

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    ModalResult := mrOK;   // Parent refresht FillForeignKeys
  end;
end;

function TfmNewForeignKey.ExecuteSQL(const ASQL: string): Boolean;
var
  Tr: TIBTransaction;
  ScriptEngine: TIBXScript;
  Lines: TStringList;
begin
  Result := False;

  if not RegisteredDatabases[DatabaseIndex].IBDatabase.Connected then
  begin
    MsgError('Database is not connected.');
    Exit;
  end;

  Tr := TIBTransaction.Create(nil);
  ScriptEngine := TIBXScript.Create(nil);
  Lines := TStringList.Create;
  try
    Tr.DefaultDatabase := RegisteredDatabases[DatabaseIndex].IBDatabase;
    Tr.Params.Clear;
    Tr.Params.Assign(RegisteredDatabases[DatabaseIndex].IBTransaction.Params);

    ScriptEngine.Transaction := Tr;
    ScriptEngine.Database    := RegisteredDatabases[DatabaseIndex].IBDatabase;
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

initialization
  {$I newforeignkey.lrs}

end.
