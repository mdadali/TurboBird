unit newForeignKey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, CheckLst, QueryWindow,
  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector;

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
  public
    DatabaseIndex: Integer;
    QWindow: TfmQueryWindow;
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
  QWindow := nil;

  frmThemeSelector.btnApplyClick(self);

  // Extractor für diese DB-Instanz sicherstellen.
  if not Assigned(FExtractor) or (FExtractorDBIndex <> DatabaseIndex) then
  begin
    if Assigned(FExtractor) then
      FreeAndNil(FExtractor);

    FExtractor := TSimpleObjExtractor.Create(DatabaseIndex);
    FExtractorDBIndex := DatabaseIndex;
  end;

  // Titel initial setzen
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
begin
  // ============================================================
  // 1. Validierung — Namen und Felder prüfen
  // ============================================================

  // FK-Name muss da sein
  if Trim(edNewName.Text) = '' then
  begin
    ShowMessage('Please enter a name for the Foreign Key constraint.');
    edNewName.SetFocus;
    Exit;
  end;

  // Zieltabelle muss gewählt sein
  if Trim(cbTables.Text) = '' then
  begin
    ShowMessage('Please select a foreign table.');
    cbTables.SetFocus;
    Exit;
  end;

  // ============================================================
  // 2. Felder zählen (links und rechts)
  // ============================================================
  CountOn := 0;
  CountFor := 0;

  for i := 0 to clxOnFields.Count - 1 do
    if clxOnFields.Checked[i] then
      Inc(CountOn);

  for i := 0 to clxForFields.Count - 1 do
    if clxForFields.Checked[i] then
      Inc(CountFor);

  // ============================================================
  // 3. Validierung — Feldanzahl
  // ============================================================
  if CountOn = 0 then
  begin
    ShowMessage('Please select at least one field in "On fields".');
    clxOnFields.SetFocus;
    Exit;
  end;

  if CountFor = 0 then
  begin
    ShowMessage('Please select at least one reference field.');
    clxForFields.SetFocus;
    Exit;
  end;

  if CountOn <> CountFor then
  begin
    ShowMessage(
      'The number of fields must match on both sides.' + sLineBreak +
      sLineBreak +
      'On fields:         ' + IntToStr(CountOn) + sLineBreak +
      'Reference fields:  ' + IntToStr(CountFor) + sLineBreak +
      sLineBreak +
      'Please check your selection.');
    Exit;
  end;

  // ============================================================
  // 4. Felder zusammenstellen — mit Auto-Quoting für DDL
  // ============================================================
  CurrFields := '';
  ForFields := '';

  for i := 0 to clxOnFields.Count - 1 do
    if clxOnFields.Checked[i] then
      CurrFields := CurrFields + MakeCaseSensitiveAuto(clxOnFields.Items[i]) + ', ';

  if CurrFields.EndsWith(', ') then
    CurrFields := Copy(CurrFields, 1, Length(CurrFields)-2);

  for i := 0 to clxForFields.Count - 1 do
    if clxForFields.Checked[i] then
      ForFields := ForFields + MakeCaseSensitiveAuto(clxForFields.Items[i]) + ', ';

  if ForFields.EndsWith(', ') then
    ForFields := Copy(ForFields, 1, Length(ForFields)-2);

  // ============================================================
  // 5. Tabellen — Namen quoten wenn nötig
  // ============================================================
  TargetTable := MakeCaseSensitiveAuto(laTable.Caption);
  RefTable    := MakeCaseSensitiveAuto(cbTables.Text);

  // ============================================================
  // 6. SQL bauen
  // ============================================================
  QWindow := fmMain.ShowQueryWindow(DatabaseIndex,
    'New constraint on table: ' + TargetTable);
  QWindow.meQuery.Lines.Text :=
    'ALTER TABLE ' + TargetTable +
    ' ADD CONSTRAINT ' + MakeCaseSensitiveAuto(edNewName.Text) +
    ' FOREIGN KEY (' + CurrFields + ')' +
    ' REFERENCES ' + RefTable + ' (' + ForFields + ')';

  if cbUpdateAction.Text <> 'Restrict' then
    QWindow.meQuery.Lines.Add(' ON UPDATE ' + cbUpdateAction.Text);
  if cbDeleteAction.Text <> 'Restrict' then
    QWindow.meQuery.Lines.Add(' ON DELETE ' + cbDeleteAction.Text);

  fmMain.Show;
  ModalResult := mrOK;
end;

initialization
  {$I newforeignkey.lrs}

end.
