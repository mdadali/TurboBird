unit fields;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, StdCtrls,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,

  uthemeselector;

type

  { TfmFields }

  TfmFields = class(TForm)
    bbEdit: TBitBtn;
    bbNew: TBitBtn;
    edDrop: TBitBtn;
    bbRefreshFields: TBitBtn;
    sgFields: TStringGrid;

    procedure bbEditClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure edDropClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;
    FConstraintName: string;   // war public "ConstraintName" in TfmTableManage

  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillFields;
  end;


implementation

{$R *.lfm}

uses Main, QueryWindow, NewEditField;

{ TfmFields }

procedure TfmFields.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Fields: ' + FTableName;
end;

// ============================================================
// FillFields — 1:1 übernommen aus TfmTableManage.FillFields
// ============================================================
procedure TfmFields.FillFields;
var
  RawFields: TFBFieldRawArray;
  i: Integer;
  FieldType: string;
  CleanTypeName: string;
  PKFieldsList: TStringList;
  DefaultValue: string;
  PKIndexName: string;
  TmpConstraintName: AnsiString;
  TmpInt: integer;
  IsUUID: boolean;
  Row: Integer;
  ArraySuffix: string;
begin
  try
    sgFields.RowCount := 1;

    RawFields := FExtractor.GetTableFieldsRaw(FTableName);

    for i := 0 to High(RawFields) do
    begin
      sgFields.RowCount := sgFields.RowCount + 1;
      Row := sgFields.RowCount - 1;

      // ---------- Field Name ----------
      sgFields.Cells[1, Row] := RawFields[i].FieldName;

      // ---------- Array-Suffix vorbereiten ----------
      ArraySuffix := ArrayDimsToSuffix(RawFields[i].ArrayDims);

      // ---------- Field Type ----------
      // Domain-basiert → Domain-Name
      // Base Type     → aufgelöster Firebird-Typ
      if (RawFields[i].FieldSource <> '') and
         (not IsFieldDomainSystemGenerated(RawFields[i].FieldSource)) then
        FieldType := RawFields[i].FieldSource
      else
        FieldType := GetFBTypeName(
          RawFields[i].FieldType,
          RawFields[i].FieldSubType,
          RawFields[i].FieldLength,
          RawFields[i].FieldPrecision,
          RawFields[i].FieldScale,
          RawFields[i].CharacterSetName,
          RawFields[i].CharacterLength
        );

      // Array-Suffix anhängen
      if ArraySuffix <> '' then
        FieldType := FieldType + ArraySuffix;

      sgFields.Cells[2, Row] := FieldType;

      CleanTypeName := GetNameFromSizedTypeName(FieldType);

      // UUID-Erkennung (CHAR(16) OCTETS)
      IsUUID := (CleanTypeName = 'CHAR') and (RawFields[i].FieldLength = 16)
        and (Trim(UpperCase(RawFields[i].CharacterSetName)) = 'OCTETS');

      if IsUUID then
      begin
        FieldType := 'UUID';
        sgFields.Cells[7, Row] := '';
      end;

      // Computed Field → Ausdruck mit COMPUTED BY-Schlüsselwort anzeigen
      if RawFields[i].ComputedSource <> '' then
        sgFields.Cells[2, Row] := 'COMPUTED BY (' + RawFields[i].ComputedSource + ')';

      // ---------- Field Size ----------
      if RawFields[i].FieldType in [CharType, CStringType, VarCharType] then
      begin
        if RawFields[i].CharacterLength > 0 then
          sgFields.Cells[3, Row] := IntToStr(RawFields[i].CharacterLength)
        else
          sgFields.Cells[3, Row] := IntToStr(RawFields[i].FieldLength);
      end
      else
        sgFields.Cells[3, Row] := IntToStr(RawFields[i].FieldLength);

      // ---------- Precision / Scale ----------
      if (CleanTypeName = 'DECIMAL') or (CleanTypeName = 'NUMERIC') then
      begin
        sgFields.Cells[4, Row] := IntToStr(RawFields[i].FieldPrecision);
        TmpInt := Abs(RawFields[i].FieldScale);
        sgFields.Cells[5, Row] := IntToStr(TmpInt);
      end;

      // ---------- Charset ----------
      if (CleanTypeName = 'CHAR') or (CleanTypeName = 'VARCHAR') or (CleanTypeName = 'UUID') then
        sgFields.Cells[6, Row] := RawFields[i].CharacterSetName;

      // ---------- Collation ----------
      if ((CleanTypeName = 'CHAR') or (CleanTypeName = 'VARCHAR')) and (not IsUUID) then
        sgFields.Cells[7, Row] := RawFields[i].CollationName;

      // ---------- Nullable flag ----------
      // Grid-Konvention: '0' = NOT NULL, '1' = NULL erlaubt
      if RawFields[i].NotNull then
        sgFields.Cells[8, Row] := '0'
      else
        sgFields.Cells[8, Row] := '1';

      // ---------- Default Value ----------
      DefaultValue := ExtractDefaultValue(RawFields[i].DefaultSource);
      sgFields.Cells[9, Row] := DefaultValue;

      // ---------- Description ----------
      sgFields.Cells[10, Row] := RawFields[i].Description;
    end;

    // ---------- Primary-Key Marker ----------
    PKFieldsList := TStringList.Create;
    try
      PKIndexName := FExtractor.GetPrimaryKeyIndexName(FTableName, TmpConstraintName);
      FConstraintName := TmpConstraintName;

      if PKIndexName <> '' then
        FExtractor.GetConstraintFields(PKIndexName, PKFieldsList);

      with sgFields do
        for i := 1 to RowCount - 1 do
          if PKFieldsList.IndexOf(Cells[1, i]) <> -1 then
            Cells[0, i] := '1'
          else
            Cells[0, i] := '0';
    finally
      PKFieldsList.Free;
    end;

  except
    on E: Exception do
      MessageDlg('Error while reading table fields: ' + e.Message, mtError, [mbOk], 0);
  end;
end;

// ============================================================
// Edit — Doppelklick oder Button
// ============================================================
procedure TfmFields.bbEditClick(Sender: TObject);
var
  fmNewEditField: TfmNewEditField;
  FieldName, FieldType,
  DefaultValue, Characterset, Collation, Description: string;
  FieldOrder, FieldSize, FieldPrecision, FieldScale: Integer;
  AllowNull: Boolean;
begin
  // ============================================================
  // Arrays können derzeit nicht editiert werden
  // ============================================================
  if (sgFields.Row >= 1) and (sgFields.Row < sgFields.RowCount) then
  begin
    if Pos('[', sgFields.Cells[2, sgFields.Row]) > 0 then
    begin
      MessageDlg(
        'Array fields cannot be edited at this time.' + sLineBreak +
        sLineBreak +
        'A dedicated array editor is planned for a future release.',
        mtInformation, [mbOK], 0);
      Exit;
    end;
  end;

  // ============================================================
  // Computed-Felder können nicht editiert werden
  // ============================================================
  if Pos('COMPUTED BY', UpperCase(sgFields.Cells[2, sgFields.Row])) > 0 then
  begin
    MessageDlg(
      'Computed fields cannot be edited at this time.' + sLineBreak +
      sLineBreak +
      'Firebird does not allow modifying a computed column via ALTER.' + sLineBreak +
      'The column would need to be dropped and recreated.' + sLineBreak +
      sLineBreak +
      'A dedicated computed-field editor is planned for a future release.',
      mtInformation, [mbOK], 0);
    Exit;
  end;

  fmNewEditField := TfmNewEditField.Create(nil);

  with sgFields, fmNewEditField do
  begin
    FieldName := Trim(Cells[1, Row]);
    FieldType := Trim(Cells[2, Row]);
    FieldSize := StrToInt(Trim(Cells[3, Row]));
    if Trim(Cells[4, Row]) <> '' then
      FieldPrecision := StrToInt(Trim(Cells[4, Row]))
    else
      FieldPrecision := 0;
    if Trim(Cells[5, Row]) <> '' then
      FieldScale := StrToInt(Trim(Cells[5, Row]))
    else
      FieldScale := 0;
    Characterset := Trim(Cells[6, Row]);
    Collation := Trim(Cells[7, Row]);
    AllowNull := Boolean(StrToInt(Trim(Cells[8, Row])));
    DefaultValue := Trim(Cells[9, Row]);
    Description := Trim(Cells[10, Row]);
    FieldOrder := Row;

    fmNewEditField.Init(FDBIndex, FTableName, foEdit,
      FieldName, FieldType, Characterset, Collation,
      DefaultValue, Description,
      FieldSize, FieldPrecision, FieldScale, FieldOrder, AllowNull,
      bbRefreshFields, FExtractor);

    Caption := 'Edit field: ' + OldFieldName;

    fmNewEditField.ShowModal;
  end;
end;

// ============================================================
// New Field
// ============================================================
procedure TfmFields.bbNewClick(Sender: TObject);
var
  fmNewEditField: TfmNewEditField;
begin
  fmNewEditField := TfmNewEditField.Create(nil);
  with fmNewEditField do
  begin
    Init(FDBIndex, FTableName, foNew,
      '', '', '', '', '', '',
      0, 0, 0, 0, True, bbRefreshFields, FExtractor);
    Caption := 'Add new field to Table: ' + FTableName;
    Show;
  end;
end;

// ============================================================
// Drop Field
// ============================================================
procedure TfmFields.edDropClick(Sender: TObject);
var
  TmpFieldName: string;
begin
  if MessageDlg('Are you sure you want to delete the field: ' + sgFields.Cells[1, sgFields.Row] +
    ' with its data', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    TmpFieldName := MakeCaseSensitiveAuto(sgFields.Cells[1, sgFields.Row]);

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop field',
      'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) + ' DROP ' + TmpFieldName,
      @bbRefreshClick);
  end;
end;

procedure TfmFields.bbRefreshClick(Sender: TObject);
begin
  FillFields;
end;

procedure TfmFields.sgFieldsDblClick(Sender: TObject);
begin
  bbEditClick(Sender);
end;

procedure TfmFields.FormShow(Sender: TObject);
begin
  FillFields;
  frmThemeSelector.btnApplyClick(Self);
end;

end.
