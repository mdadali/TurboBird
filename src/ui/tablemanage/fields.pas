unit fields;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, StdCtrls,

  IBDatabase, IBXScript, IBSQL,

  SysTables,
  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector;


 type
  { TfmFields }

  TTableCreatedEvent = procedure(const ATableName: AnsiString) of object;

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
    FConstraintName: string;
    FMode: TTableManageMode;

    FOnTableCreated: TTableCreatedEvent;

 // Create-Mode Steuerelemente (werden zur Laufzeit erzeugt)
    cxCreateGen: TCheckBox;
    bbCreateScript: TBitBtn;

    procedure ApplyFieldsMode;
    procedure EnsureCreateControls;
    procedure FreeCreateControls;
    function  ValidateCreate: Boolean;
    function  GenerateCreateSQL(var KeyField, GeneratorName: string): string;
    procedure bbCreateScriptClick(Sender: TObject);

    function ExecuteCreateScript(const Script: string): Boolean;

   public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor;
                   Mode: TTableManageMode = tmmEdit);
    procedure SetMode(AMode: TTableManageMode);
    procedure FillFields;

    procedure UpdateTableName(const AName: string);

    property OnTableCreated: TTableCreatedEvent read FOnTableCreated write FOnTableCreated;
  end;

implementation

{$R *.lfm}

uses Main, QueryWindow, NewEditField;

{ TfmFields }

procedure TfmFields.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor;
  Mode: TTableManageMode);
begin
  FDBIndex   := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;
  FMode      := Mode;

  if FMode = tmmEdit then
    Caption := 'Fields: ' + FTableName
  else
    Caption := 'Define Fields';

  ApplyFieldsMode;
end;

procedure TfmFields.SetMode(AMode: TTableManageMode);
begin
  if FMode = AMode then
    Exit;

  FMode := AMode;
  ApplyFieldsMode;
end;

procedure TfmFields.ApplyFieldsMode;
var
  i: Integer;
begin
  EnsureCreateControls;

  case FMode of
    tmmCreate:
      begin
        // Edit-Buttons ausblenden
        bbNew.Visible           := False;
        bbEdit.Visible          := False;
        edDrop.Visible          := False;
        bbRefreshFields.Visible := False;

        // Create-Controls einblenden

        cxCreateGen.Visible     := True;
        bbCreateScript.Caption := 'Create Table';
        bbCreateScript.Visible  := True;

        // Grid volle Höhe (Tabellenname liegt jetzt im Parent)
        sgFields.Top    := 0;
        sgFields.Height := Self.ClientHeight - 80;

        // Grid editierbar machen
        sgFields.Options := sgFields.Options + [goEditing, goAlwaysShowEditor, goAutoAddRows];

        // Typ-Picklist laden
        sgFields.Columns[2].PickList.Clear;
        if Assigned(dmSysTables) then
        begin
          dmSysTables.GetAllTypes(sgFields.Columns[2].PickList, FDBIndex);
          dmSysTables.GetDomainTypes(FDBIndex, sgFields.Columns[2].PickList);
        end;

        // Leeres Grid vorbereiten
        sgFields.RowCount := 4;
        for i := 1 to sgFields.RowCount - 1 do
        begin
          sgFields.Cells[0, i] := '0';  // P-Key
          sgFields.Cells[1, i] := '';
          sgFields.Cells[2, i] := '';
          sgFields.Cells[3, i] := '';
          sgFields.Cells[4, i] := '';
          sgFields.Cells[5, i] := '';
          sgFields.Cells[6, i] := '';
          sgFields.Cells[7, i] := '';
          sgFields.Cells[8, i] := '1';  // Allow Null
          sgFields.Cells[9, i] := '';
          sgFields.Cells[10,i] := '';
        end;
      end;

    tmmEdit:
      begin
        // Create-Controls ausblenden
        if Assigned(cxCreateGen)    then cxCreateGen.Visible    := False;
        if Assigned(bbCreateScript) then bbCreateScript.Visible := False;

        // Edit-Buttons wieder einblenden
        bbNew.Visible           := True;
        bbEdit.Visible          := True;
        edDrop.Visible          := True;
        bbRefreshFields.Visible := True;

        // Grid volle Höhe
        sgFields.Top    := 0;
        sgFields.Height := Self.ClientHeight - 50;
        sgFields.Options := sgFields.Options - [goEditing, goAlwaysShowEditor, goAutoAddRows];

        if (FTableName <> '') and Assigned(FExtractor) then
          FillFields;
      end;
  end;
end;

procedure TfmFields.EnsureCreateControls;
begin
  if Assigned(bbCreateScript) then
    Exit;   // schon vorhanden

  // Auto-Increment Checkbox
  cxCreateGen := TCheckBox.Create(Self);
  cxCreateGen.Parent  := Self;
  cxCreateGen.Left    := 8;
  cxCreateGen.Top     := Height - 70;
  cxCreateGen.Width   := 320;
  cxCreateGen.Caption := 'Create Auto increment Generator and Trigger';
  cxCreateGen.Anchors := [akLeft, akBottom];
  cxCreateGen.Visible := False;

  // Script-Button
  bbCreateScript := TBitBtn.Create(Self);
  bbCreateScript.Parent  := Self;
  bbCreateScript.Left    := 8;
  bbCreateScript.Top     := Height - 40;
  bbCreateScript.Width   := 130;
  bbCreateScript.Height  := 32;
  bbCreateScript.Caption := 'Script / Create';
  bbCreateScript.Anchors := [akLeft, akBottom];
  bbCreateScript.OnClick := @bbCreateScriptClick;
  bbCreateScript.Visible := False;
end;

procedure TfmFields.FreeCreateControls;
begin
  FreeAndNil(bbCreateScript);
  FreeAndNil(cxCreateGen);
end;

function TfmFields.ValidateCreate: Boolean;
var
  i: Integer;
  HasFields: Boolean;
  HasPK: Boolean;
begin
  Result := False;

  if Trim(FTableName) = '' then
  begin
    MessageDlg('Warning', 'You should enter a new table name', mtWarning, [mbOk], 0);
    Exit;
  end;

  HasFields := False;
  for i := 1 to sgFields.RowCount - 1 do
    if Trim(sgFields.Cells[1, i]) <> '' then
    begin
      HasFields := True;
      Break;
    end;

  if not HasFields then
  begin
    MessageDlg('Warning', 'You should enter at least one field', mtWarning, [mbOk], 0);
    Exit;
  end;

  for i := 1 to sgFields.RowCount - 1 do
  begin
    if (Trim(sgFields.Cells[1, i]) <> '') and (Trim(sgFields.Cells[2, i]) = '') then
    begin
      MessageDlg('Warning', 'You should write a field type for the column: ' +
                 sgFields.Cells[1, i], mtWarning, [mbOk], 0);
      Exit;
    end;
    if (Trim(sgFields.Cells[1, i]) = '') and (Trim(sgFields.Cells[2, i]) <> '') then
    begin
      MessageDlg('Warning', 'You should select a field name for the column number ' +
                 IntToStr(i), mtWarning, [mbOk], 0);
      Exit;
    end;
  end;

  if Assigned(cxCreateGen) and cxCreateGen.Checked then
  begin
    HasPK := False;
    for i := 1 to sgFields.RowCount - 1 do
      if sgFields.Cells[0, i] = '1' then
      begin
        HasPK := True;
        Break;
      end;

    if not HasPK then
    begin
      MessageDlg('Warning', 'There is no primary key field selected', mtWarning, [mbOk], 0);
      Exit;
    end;
  end;

  Result := True;
end;

function TfmFields.GenerateCreateSQL(var KeyField, GeneratorName: string): string;
var
  i: Integer;
  FieldLine: string;
  FieldType: string;
  PKey: string;
  TableName: string;
  SizeStr: string;
begin
  KeyField := '';
  GeneratorName := '';
  PKey := '';

  TableName := MakeCaseSensitiveAuto(FTableName);

  Result := 'CREATE TABLE ' + TableName + ' (' + LineEnding;

  for i := 1 to sgFields.RowCount - 1 do
  begin
    if Trim(sgFields.Cells[1, i]) = '' then
      Continue;

    FieldLine := MakeCaseSensitiveAuto(Trim(sgFields.Cells[1, i]));
    FieldType := Trim(sgFields.Cells[2, i]);

    if SameText(FieldType, 'UUID') then
      FieldType := 'CHAR(16) CHARACTER SET OCTETS';

    FieldLine := FieldLine + ' ' + FieldType;

    SizeStr := Trim(sgFields.Cells[3, i]);
    if ((LowerCase(FieldType) = 'varchar') or (LowerCase(FieldType) = 'char')) and
       (SizeStr <> '') then
      FieldLine := FieldLine + '(' + SizeStr + ')';

    if sgFields.Cells[8, i] = '0' then
      FieldLine := FieldLine + ' NOT NULL';

    if Trim(sgFields.Cells[9, i]) <> '' then
    begin
      if ((Pos('CHAR', UpperCase(FieldType)) > 0) or (UpperCase(FieldType) = 'CSTRING')) and
         (Pos('''', sgFields.Cells[9, i]) = 0) then
        FieldLine := FieldLine + ' DEFAULT ' + QuotedStr(sgFields.Cells[9, i])
      else
        FieldLine := FieldLine + ' DEFAULT ' + sgFields.Cells[9, i];
    end;

    if sgFields.Cells[0, i] = '1' then
    begin
      if PKey <> '' then
        PKey := PKey + ', ';
      PKey := PKey + MakeCaseSensitiveAuto(Trim(sgFields.Cells[1, i]));

      if KeyField = '' then
      begin
        KeyField := Trim(sgFields.Cells[1, i]);
        GeneratorName := FTableName + '_' + KeyField + '_Gen';
      end;
    end;

    Result := Result + '  ' + FieldLine + ',' + LineEnding;
  end;

  if PKey <> '' then
  begin
    Result := Result + '  CONSTRAINT ' + MakeCaseSensitiveAuto(FTableName + '_PK') +
              ' PRIMARY KEY (' + PKey + ')' + LineEnding;
  end
  else
  begin
    if (Length(Result) > Length(LineEnding)) and
       (Result[Length(Result) - Length(LineEnding)] = ',') then
      Delete(Result, Length(Result) - Length(LineEnding), 1);
  end;

  Result := Result + ');' + LineEnding;
end;

procedure TfmFields.bbCreateScriptClick(Sender: TObject);
var
  List: TStringList;
  KeyField, GeneratorName, TableName: string;
  FullScript: string;
begin
  if not ValidateCreate then
    Exit;

  TableName := MakeCaseSensitiveAuto(FTableName);

  List := TStringList.Create;
  try
    List.Text := GenerateCreateSQL(KeyField, GeneratorName);

    if Assigned(cxCreateGen) and cxCreateGen.Checked and (KeyField <> '') then
    begin
      KeyField := MakeCaseSensitiveAuto(KeyField);

      List.Add('');
      List.Add('-- Generator');
      List.Add('CREATE GENERATOR ' + MakeCaseSensitiveAuto(GeneratorName) + ';');
      List.Add('');
      List.Add('-- Trigger');
      List.Add('CREATE TRIGGER ' + MakeCaseSensitiveAuto(GeneratorName) + ' FOR ' + TableName);
      List.Add('ACTIVE BEFORE INSERT POSITION 0');
      List.Add('AS');
      List.Add('BEGIN');
      List.Add('  IF (NEW.' + KeyField + ' IS NULL OR NEW.' + KeyField + ' = 0) THEN');
      List.Add('    NEW.' + KeyField + ' = GEN_ID(' + MakeCaseSensitiveAuto(GeneratorName) + ', 1);');
      List.Add('END;');
    end;

    FullScript := List.Text;

    // Lokal ausführen
    if ExecuteCreateScript(FullScript) then
    begin
      turbocommon.MetaDataChanged := True;

      // Erfolg → Parent auf Edit-Mode umschalten
      if Assigned(FOnTableCreated) then
        FOnTableCreated(FTableName);

      MessageDlg('Table "' + FTableName + '" created successfully.',
                 mtInformation, [mbOk], 0);
    end;

  finally
    List.Free;
  end;
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
  if FMode = tmmEdit then
    FillFields;

  frmThemeSelector.btnApplyClick(Self);
end;

procedure TfmFields.UpdateTableName(const AName: string);
begin
  FTableName := Trim(AName);
end;

function TfmFields.ExecuteCreateScript(const Script: string): Boolean;
var
  Tr: TIBTransaction;
  ScriptEngine: TIBXScript;
  Lines: TStringList;
begin
  Result := False;

  if not RegisteredDatabases[FDBIndex].IBDatabase.Connected then
  begin
    MessageDlg('Database is not connected.', mtError, [mbOk], 0);
    Exit;
  end;

  Tr := TIBTransaction.Create(nil);
  ScriptEngine := TIBXScript.Create(nil);
  Lines := TStringList.Create;
  try
    Tr.DefaultDatabase := RegisteredDatabases[FDBIndex].IBDatabase;
    Tr.Params.Clear;
    Tr.Params.Add('concurrency');
    Tr.Params.Add('wait');

    ScriptEngine.Transaction := Tr;
    ScriptEngine.Database    := RegisteredDatabases[FDBIndex].IBDatabase;

    // Script in Zeilen zerlegen und ausführen
    Lines.Text := Script;

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
        MessageDlg('Error while creating table:' + sLineBreak + sLineBreak + E.Message,
                   mtError, [mbOk], 0);
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
