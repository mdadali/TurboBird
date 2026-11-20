unit fields;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, StdCtrls,

  IBDatabase, IBXScript, IBSQL,

  umessagedlg,
  SysTables,
  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector,
  confirm_query;


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

    function ExtractTableNameFromCreateSQL(const ASQL: string): string;

    function ExecuteSQL(const Script: string): Boolean;
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

{$R fields.lfm}

uses Main,  NewEditField;

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
    MsgWarning('You should enter a new table name.');
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
    MsgWarning('You should enter at least one field.');
    Exit;
  end;

  for i := 1 to sgFields.RowCount - 1 do
  begin
    if (Trim(sgFields.Cells[1, i]) <> '') and (Trim(sgFields.Cells[2, i]) = '') then
    begin
      MsgWarning('You should write a field type for the column: ' + sgFields.Cells[1, i]);
      Exit;
    end;
    if (Trim(sgFields.Cells[1, i]) = '') and (Trim(sgFields.Cells[2, i]) <> '') then
    begin
      MsgWarning('You should select a field name for the column number ' + IntToStr(i));
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
      MsgWarning('There is no primary key field selected.');
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
  ExtractedName: string;
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

    // Confirm-Query nur beim Create
    if not TfrmConfirmQuery.Execute('Create Table: ' + FTableName, FullScript) then
      Exit;

    ExtractedName := ExtractTableNameFromCreateSQL(FullScript);
    if ExtractedName <> '' then
    begin
      FTableName := ExtractedName;
      UpdateTableName(ExtractedName);
    end;

    if ExecuteSQL(FullScript) then
    begin
      turbocommon.MetaDataChanged := True;

      if Assigned(FOnTableCreated) then
        FOnTableCreated(FTableName);

      MsgBox('Table "' + FTableName + '" created successfully.', mkInfo);

      fmMain.FillObjectRoot(fmMain.tvMain.Selected);
      fmMain.tvMain.Selected.Expand(False);
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
      MsgError('Error while reading table fields: ' + E.Message);
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
  S: string;
begin
  if FMode = tmmCreate then
    Exit;

  if (sgFields.Row < 1) or (sgFields.Row >= sgFields.RowCount) then
    Exit;

  if Trim(sgFields.Cells[1, sgFields.Row]) = '' then
    Exit;

  // Arrays können derzeit nicht editiert werden
  if Pos('[', sgFields.Cells[2, sgFields.Row]) > 0 then
  begin
    MsgBox(
          'Array fields cannot be edited at this time.' + sLineBreak +
          sLineBreak +
          'A dedicated array editor is planned for a future release.',
          mkInfo);
    Exit;
  end;

  // Computed-Felder können nicht editiert werden
  if Pos('COMPUTED BY', UpperCase(sgFields.Cells[2, sgFields.Row])) > 0 then
  begin
    MsgBox(
      'Computed fields cannot be edited at this time.' + sLineBreak +
      sLineBreak +
      'Firebird does not allow modifying a computed column via ALTER.' + sLineBreak +
      'The column would need to be dropped and recreated.' + sLineBreak +
      sLineBreak +
      'A dedicated computed-field editor is planned for a future release.',
      mkInfo);
    Exit;
  end;

  fmNewEditField := TfmNewEditField.Create(nil);

  with sgFields, fmNewEditField do
  begin
    FieldName := Trim(Cells[1, Row]);
    FieldType := Trim(Cells[2, Row]);

    // Size
    S := Trim(Cells[3, Row]);
    if S <> '' then
      FieldSize := StrToIntDef(S, 0)
    else
      FieldSize := 0;

    // Precision
    S := Trim(Cells[4, Row]);
    if S <> '' then
      FieldPrecision := StrToIntDef(S, 0)
    else
      FieldPrecision := 0;

    // Scale
    S := Trim(Cells[5, Row]);
    if S <> '' then
      FieldScale := StrToIntDef(S, 0)
    else
      FieldScale := 0;

    Characterset := Trim(Cells[6, Row]);
    Collation   := Trim(Cells[7, Row]);

    // Allow Null (Checkbox: '0'/'1')
    S := Trim(Cells[8, Row]);
    if S <> '' then
      AllowNull := Boolean(StrToIntDef(S, 1))
    else
      AllowNull := True;

    DefaultValue := Trim(Cells[9, Row]);
    Description  := Trim(Cells[10, Row]);
    FieldOrder   := Row;

    fmNewEditField.Init(FDBIndex, FTableName, foEdit,
      FieldName, FieldType, Characterset, Collation,
      DefaultValue, Description,
      FieldSize, FieldPrecision, FieldScale, FieldOrder, AllowNull,
      bbRefreshFields, FExtractor);

    Caption := 'Edit field: ' + FieldName;

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
  FieldName: string;
  SQL: string;
begin
  if FMode <> tmmEdit then
    Exit;

  if (sgFields.Row < 1) or (sgFields.Row >= sgFields.RowCount) then
    Exit;

  FieldName := Trim(sgFields.Cells[1, sgFields.Row]);
  if FieldName = '' then
    Exit;

  if not MsgConfirm(
       'Do you really want to drop the field "' + FieldName + '" including its data?' +
       sLineBreak + sLineBreak +
       'This action cannot be undone.',
       mkWarning) then
    Exit;

  SQL := 'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
         ' DROP ' + MakeCaseSensitiveAuto(FieldName);

  if ExecuteSQL(SQL) then
  begin
    turbocommon.MetaDataChanged := True;
    FillFields;
  end;
end;

procedure TfmFields.bbRefreshClick(Sender: TObject);
begin
  FillFields;
end;

procedure TfmFields.sgFieldsDblClick(Sender: TObject);
begin
  // Im Create-Mode kein Edit-Dialog
  if FMode = tmmCreate then
    Exit;

  // Keine gültige Datenzeile
  if (sgFields.Row < 1) or (sgFields.Row >= sgFields.RowCount) then
    Exit;

  // Leerer Feldname → nichts tun
  if Trim(sgFields.Cells[1, sgFields.Row]) = '' then
    Exit;

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

function TfmFields.ExtractTableNameFromCreateSQL(const ASQL: string): string;
var
  S, Token: string;
  P, StartPos: Integer;
begin
  Result := '';
  S := UpperCase(ASQL);

  P := Pos('CREATE TABLE', S);
  if P = 0 then
    Exit;

  // hinter "CREATE TABLE" weiter
  StartPos := P + Length('CREATE TABLE');
  while (StartPos <= Length(ASQL)) and (ASQL[StartPos] in [' ', #9, #10, #13]) do
    Inc(StartPos);

  if StartPos > Length(ASQL) then
    Exit;

  // optionalen Schema-Punkt überspringen (z.B. SCHEMA.TABLENAME)
  Token := '';
  P := StartPos;
  while (P <= Length(ASQL)) and (ASQL[P] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$', '"', '.']) do
  begin
    Token := Token + ASQL[P];
    Inc(P);
  end;

  // nur den letzten Teil nach dem Punkt nehmen (Tabellenname)
  if Pos('.', Token) > 0 then
    Token := Copy(Token, LastDelimiter('.', Token) + 1, MaxInt);

  // Anführungszeichen entfernen
  Result := AnsiDequotedStr(Token, '"');
  if Result = Token then
    Result := Trim(Token);
end;

function TfmFields.ExecuteSQL(const Script: string): Boolean;
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
