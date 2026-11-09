unit NewGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons,
  fbcommon,
  turbocommon,
  fsimpleobjextractor,
  uthemeselector;

type

  { TfmNewGen }

  TfmNewGen = class(TForm)
    bbCreateGen: TBitBtn;
    BitBtn1: TBitBtn;
    cbTables: TComboBox;
    cbFields: TComboBox;
    cxTrigger: TCheckBox;
    edGenName: TEdit;
    gbTrigger: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure bbCreateGenClick(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure cxTriggerChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDBIndex: Integer;
    FExtractor: TSimpleObjExtractor;
    FExtractorDBIndex: Integer;
  public
    procedure Init(dbIndex: Integer);
  end;

var
  fmNewGen: TfmNewGen;

implementation

{ TfmNewGen }

uses main;

procedure TfmNewGen.bbCreateGenClick(Sender: TObject);
var
  List: TStringList;
  Valid: Boolean;
  GenName, TableName, FieldName: string;
begin
  if Trim(edGenName.Text) = '' then
  begin
    MessageDlg('You should write Sequence name', mtError, [mbOK], 0);
    Exit;
  end;

  Valid := True;
  List := TStringList.Create;
  try
    // Case-Sensitivity: alle Identifier auto-quoten
    GenName   := MakeCaseSensitiveAuto(Trim(edGenName.Text));
    TableName := MakeCaseSensitiveAuto(Trim(cbTables.Text));
    FieldName := MakeCaseSensitiveAuto(Trim(cbFields.Text));

    // CREATE GENERATOR läuft auf FB 1.0 bis 6 — keine Versions-Weiche nötig
    List.Add('CREATE GENERATOR ' + GenName + ';');

    if cxTrigger.Checked then
    begin
      Valid := False;
      if (cbTables.ItemIndex = -1) or (cbFields.ItemIndex = -1) then
        MessageDlg('You should select a table and a field', mtError, [mbOk], 0)
      else
      begin
        // Versions-Weiche: FB 1.5 nutzt "FOR table", FB 2.0+ nutzt "ON table"
        if RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor < 2 then
        begin
          List.Add('CREATE TRIGGER ' + GenName + ' FOR ' + TableName);
          List.Add('ACTIVE BEFORE INSERT POSITION 0');
        end
        else
        begin
          List.Add('CREATE TRIGGER ' + GenName);
          List.Add('ACTIVE BEFORE INSERT');
          List.Add('ON ' + TableName);
          List.Add('POSITION 0');
        end;

        List.Add('AS');
        List.Add('BEGIN');
        List.Add('  IF (NEW.' + FieldName + ' IS NULL OR NEW.' + FieldName + ' = 0) THEN');
        List.Add('    NEW.' + FieldName + ' = GEN_ID(' + GenName + ', 1);');
        List.Add('END;');
        Valid := True;
      end;
    end;

    if Valid then
    begin
      fmMain.ShowCompleteQueryWindow(FDBIndex,
        'New Sequence#' + IntToStr(FDBIndex) + ':' + GenName, List.Text);
      Close;
    end;
  finally
    List.Free;
  end;
end;

procedure TfmNewGen.cbTablesChange(Sender: TObject);
var
  RawFields: TFBFieldRawArray;
  i: Integer;
  IsIntegerType: Boolean;
begin
  // 1. Feldliste immer leeren
  cbFields.Items.Clear;
  cbFields.ItemIndex := -1;

  if cbTables.ItemIndex = -1 then
    Exit;

  if not Assigned(FExtractor) then
    Exit;

  RawFields := FExtractor.GetTableFieldsRaw(Trim(cbTables.Text));

  // 2. Nur kompatible (ganzzahlige) Felder einfügen
  //    RDB$FIELD_TYPE: 7 = SMALLINT, 8 = INTEGER, 16 = BIGINT
  for i := 0 to High(RawFields) do
  begin
    IsIntegerType :=
      (RawFields[i].FieldType = 7) or
      (RawFields[i].FieldType = 8) or
      (RawFields[i].FieldType = 16);

    if IsIntegerType then
      cbFields.Items.Add(RawFields[i].FieldName);
  end;

  // 3. Erstes Feld automatisch vorauswählen
  if cbFields.Items.Count > 0 then
    cbFields.ItemIndex := 0;
end;

procedure TfmNewGen.cxTriggerChange(Sender: TObject);
begin
  gbTrigger.Enabled := cxTrigger.Checked;
end;

procedure TfmNewGen.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);

  // Extractor für diese DB-Instanz sicherstellen
  if not Assigned(FExtractor) or (FExtractorDBIndex <> FDBIndex) then
  begin
    if Assigned(FExtractor) then
      FreeAndNil(FExtractor);

    FExtractor := TSimpleObjExtractor.Create(FDBIndex);
    FExtractorDBIndex := FDBIndex;
  end;
end;

procedure TfmNewGen.FormDestroy(Sender: TObject);
begin
  if Assigned(FExtractor) then
    FreeAndNil(FExtractor);
end;

procedure TfmNewGen.Init(dbIndex: Integer);
var
  TableList: TStringList;
begin
  FDBIndex := dbIndex;
  FExtractorDBIndex := -1;   // Erzwingt Neu-Erzeugung in FormShow

  // ============================================================
  // 1. Alle Controls zurücksetzen
  // ============================================================
  edGenName.Clear;
  edGenName.Enabled := True;

  cbTables.Items.Clear;
  cbTables.ItemIndex := -1;
  cbTables.Text := '';

  cbFields.Items.Clear;
  cbFields.ItemIndex := -1;
  cbFields.Text := '';

  cxTrigger.Checked := False;
  gbTrigger.Enabled := False;

  // ============================================================
  // 2. Extractor für Tabellen-Liste vorab sicherstellen
  // ============================================================
  if Assigned(FExtractor) and (FExtractorDBIndex <> dbIndex) then
    FreeAndNil(FExtractor);

  if not Assigned(FExtractor) then
  begin
    FExtractor := TSimpleObjExtractor.Create(dbIndex);
    FExtractorDBIndex := dbIndex;
  end;

  // ============================================================
  // 3. Tabellen-Liste laden — FBIdentifierCast greift auf FB 1.5
  // ============================================================
  TableList := TStringList.Create;
  try
    FExtractor.ExtractObjectNames(dbIndex, otTables, false,
      TStrings(TableList), '');
    cbTables.Items.AddStrings(TableList);
  finally
    TableList.Free;
  end;
end;

initialization
  {$I newgen.lrs}

end.
