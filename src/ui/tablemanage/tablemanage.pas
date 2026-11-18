unit TableManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, ComCtrls, Grids, Buttons, StdCtrls, CheckLst, LCLType,
  ExtCtrls, types,
  IB,
  IBDatabase,
  IBQuery,

  fbcommon,
  turbocommon,

  fsimpleobjextractor,

  uthemeselector,

  edit_primarykey,
  UniqueConstraints,
  CheckConstraints,
  NotNullConstraints,
  NewTable,
  References,
  ForeignKeys,
  Triggers,
  Permissions
;

type

  { TfmTableManage }

  TfmTableManage = class(TForm)
    bbCreateIndex: TBitBtn;
    bbDropForeignKey: TBitBtn;
    bbEdit: TBitBtn;
    bbNew: TBitBtn;
    bbNewForeignKey: TBitBtn;
    bbRefreshFields: TBitBtn;
    bbRefreshForeignKeys: TBitBtn;
    bbRefreshReferences: TBitBtn;
    bbRefreshIndices: TBitBtn;
    bbRefreshTriggers: TBitBtn;
    bbNewTrigger: TBitBtn;
    bbEditTrigger: TBitBtn;
    bbDropTrigger: TBitBtn;
    bbRefreshPermissions: TBitBtn;
    bbAddUser: TBitBtn;
    bbDropIndices: TBitBtn;
    Button1: TButton;
    cbIndexType: TComboBox;
    cbSortType: TComboBox;
    clbFields: TCheckListBox;
    cxUnique: TCheckBox;
    bbEditPermission: TBitBtn;
    edDrop: TBitBtn;
    edIndexName: TEdit;
    GroupBox2: TGroupBox;
    ImageList2: TImageList;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PageControl1: TPageControl;
    sgFields: TStringGrid;
    sgIndices: TStringGrid;
    tsNewTable: TTabSheet;
    tsNotNullConstraints: TTabSheet;
    tsCheckConstraints: TTabSheet;
    tsUniqueConstraints: TTabSheet;
    tsPrimaryKey: TTabSheet;
    tsReferences: TTabSheet;
    tsPermissions: TTabSheet;
    tsTriggers: TTabSheet;
    tsIndices: TTabSheet;
    tsForeignKeys: TTabSheet;
    tsFields: TTabSheet;
    procedure bbAddUserClick(Sender: TObject);
    procedure bbCreateIndexClick(Sender: TObject);
    procedure bbDropForeignKeyClick(Sender: TObject);
    procedure bbDropIndicesClick(Sender: TObject);
    procedure bbDropTriggerClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure bbEditTriggerClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure bbNewForeignKeyClick(Sender: TObject);
    procedure bbNewTriggerClick(Sender: TObject);
    procedure bbRefreshFieldsClick(Sender: TObject);
    procedure bbRefreshForeignKeysClick(Sender: TObject);
    procedure bbRefreshIndicesClick(Sender: TObject);
    procedure bbRefreshPermissionsClick(Sender: TObject);
    procedure bbRefreshTriggersClick(Sender: TObject);
    procedure bbRefreshReferencesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbIndexTypeChange(Sender: TObject);
    procedure edDropClick(Sender: TObject);
    procedure bbEditPermissionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);
    procedure tsCheckConstraintsShow(Sender: TObject);
    procedure tsForeignKeysShow(Sender: TObject);
    procedure tsFieldsShow(Sender: TObject);
    procedure tsIndicesShow(Sender: TObject);
    procedure tsNotNullConstraintsShow(Sender: TObject);
    procedure tsPermissionsShow(Sender: TObject);
    procedure tsPrimaryKeyShow(Sender: TObject);
    procedure tsReferencesShow(Sender: TObject);
    procedure tsTriggersShow(Sender: TObject);
    procedure tsUniqueConstraintsShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: Integer;
    FTableName: string;
    FExtractor: TSimpleObjExtractor;

    fmPrimaryKey: TfmPrimaryKey;
    fmUniqueConstraints: TfmUniqueConstraints;
    fmCheckConstraints: TfmCheckConstraints;
    fmNotNullConstraints: TfmNotNullConstraints;
    fmReferences: TfmReferences;
    fmForeignKeys: TfmForeignKeys;
    fmTriggers: TfmTriggers;
    fmTablePermissions: TfmTablePermissions;
  public
    PKeyName,
    ConstraintName: string;
    procedure Init(dbIndex: Integer; TableName: string; ANodeInfos: TPNodeInfos);
    procedure FillForeignKeys;
    procedure FillIndices;
    procedure FillFields;
    // Get info on permissions and fill grid with it
    procedure FillPermissions;
    // Get info on triggers and fill grid with it
    procedure FillTriggers;
  end;

var
  fmTableManage: TfmTableManage;

implementation

{ TfmTableManage }

uses SysTables, NewEditField, Main, QueryWindow, newForeignKey, PermissionManage;


procedure TfmTableManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.EditorForm := nil;

  if Assigned(fmPrimaryKey) then
  begin
    fmPrimaryKey.Close;
    // Nicht freigeben, da es als Child von tsPrimaryKey automatisch freigegeben wird
  end;

  if Assigned(fmUniqueConstraints) then
    fmUniqueConstraints.Close;

  if Assigned(fmCheckConstraints) then
    fmCheckConstraints.Close;

  if Assigned(fmNotNullConstraints) then
    fmNotNullConstraints.Close;

  if Assigned(fmReferences) then
     fmReferences.Close;

  if Assigned(fmForeignKeys) then
    fmForeignKeys.Close;

  if Assigned(fmTriggers) then
    fmTriggers.Close;

  if Assigned(fmTablePermissions) then
     fmTablePermissions.Close;

  if Assigned(FExtractor) then
    FreeAndNil(FExtractor);

  CloseAction := caFree;
  TTabSheet(Parent).Free;
end;

procedure TfmTableManage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;

procedure TfmTableManage.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmTableManage.sgFieldsDblClick(Sender: TObject);
begin
  // Double clicking on a row lets you edit the field
  bbEditClick(Sender);
end;

procedure TfmTableManage.tsCheckConstraintsShow(Sender: TObject);
begin
  if Assigned(fmCheckConstraints) then
    fmCheckConstraints.FillCheckConstraints;
end;

procedure TfmTableManage.bbEditClick(Sender: TObject);
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


  fmNewEditField:= TfmNewEditField.Create(nil);

  with sgFields, fmNewEditField do
  begin
    FieldName:= Trim(Cells[1, Row]);
    FieldType:= Trim(Cells[2, Row]);
    FieldSize:= StrtoInt(Trim(Cells[3, Row]));
    if Trim(Cells[4, Row]) <> '' then
      FieldPrecision := StrToInt(Trim(Cells[4, Row]))
    else
      FieldPrecision := 0;
    if Trim(Cells[5, Row]) <> '' then
      FieldScale := StrToInt(Trim(Cells[5, Row]))
    else
      FieldScale := 0;
    Characterset:= Trim(Cells[6, Row]); //todo: support character set in field editing: add column to grid
    Collation:= Trim(Cells[7, Row]);//todo: support collation in field editing: add column to grid
    AllowNull:= Boolean(StrToInt(Trim(Cells[8, Row])));
    DefaultValue := Trim(Cells[9, Row]);
    Description  := Trim(Cells[10, Row]);
    FieldOrder:= Row;

    fmNewEditField.Init(FDBIndex, FTableName, foEdit,
      FieldName, FieldType, Characterset, Collation,
      DefaultValue, Description,
      FieldSize, FieldPrecision, FieldScale, FieldOrder, AllowNull,
      bbRefreshFields, FExtractor);

    Caption:= 'Edit field: ' + OldFieldName;

    fmNewEditField.ShowModal;
  end;
end;

procedure TfmTableManage.bbEditTriggerClick(Sender: TObject);
begin
  if Assigned(fmTriggers) then
    fmTriggers.bbEditClick(Sender);
end;

// ============================================================
// FillForeignKeys vereinfacht mit FExtractor:
// ============================================================
procedure TfmTableManage.FillForeignKeys;
begin
  if Assigned(fmForeignKeys) then
    fmForeignKeys.FillForeignKeys;
end;

procedure TfmTableManage.bbNewForeignKeyClick(Sender: TObject);
begin
  if Assigned(fmForeignKeys) then
    fmForeignKeys.bbNewClick(Sender);
end;

procedure TfmTableManage.bbDropForeignKeyClick(Sender: TObject);
begin
  if Assigned(fmForeignKeys) then
    fmForeignKeys.bbDropClick(Sender);
end;

procedure TfmTableManage.bbDropIndicesClick(Sender: TObject);
var
  Line, IndexName: string;
  ColonPos: Integer;
begin
  with sgIndices do
  begin
    if RowCount <= 1 then Exit;

    Line := Cells[0, Row];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
      IndexName := Trim(Copy(Line, 1, ColonPos - 1))
    else
      IndexName := Trim(Line);

    if MessageDlg('Are you sure you want to drop index: ' + IndexName, mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
    begin
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop Index on table: ' + FTableName,
        'DROP INDEX ' + IndexName, @bbRefreshIndicesClick);
    end;
  end;
end;

procedure TfmTableManage.bbDropTriggerClick(Sender: TObject);
begin
  if Assigned(fmTriggers) then
    fmTriggers.bbDropClick(Sender);
end;

procedure TfmTableManage.bbCreateIndexClick(Sender: TObject);
var
  Fields: string;
  i: Integer;
  QWindow: TfmQueryWindow;
  FirstLine: string;
begin
  Fields := '';
  for i := 0 to clbFields.Count - 1 do
    if clbFields.Checked[i] then
      Fields := Fields + Trim(clbFields.Items[i]) + ',';
  Delete(Fields, Length(Fields), 1);

  if Trim(Fields) = '' then
    MessageDlg('Error', 'You should select at least one field', mtError, [mbOk], 0)
  else if Trim(edIndexName.Text) = '' then
    MessageDlg('Error', 'You should enter the new index name', mtError, [mbOk], 0)
  else
  begin
    QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Create new index');
    QWindow.meQuery.Lines.Clear;

    FirstLine := 'CREATE ';
    if cxUnique.Checked then
      FirstLine := FirstLine + 'UNIQUE ';
    FirstLine := FirstLine + cbSortType.Text + ' INDEX ' + edIndexName.Text;
    QWindow.meQuery.Lines.Text := FirstLine + LineEnding + 'ON ' + FTableName + ' (' + Fields + ')';

    QWindow.OnCommit := @bbRefreshIndicesClick;
    QWindow.Show;
  end;
end;


procedure TfmTableManage.bbAddUserClick(Sender: TObject);
begin
  if Assigned(fmTablePermissions) then
    fmTablePermissions.bbAddUserClick(Sender);
end;

procedure TfmTableManage.bbNewClick(Sender: TObject);
var
  fmNewEditField: TfmNewEditField;
begin
  fmNewEditField:= TfmNewEditField.Create(nil);
  with fmNewEditField do
  begin
    Init(FDBIndex, FTableName, foNew,
      '', '', '', '', '', '',
      0, 0, 0, 0, True, bbRefreshFields, FExtractor);
    Caption:= 'Add new field to Table: ' + FTableName;
    Show;
  end;
end;

procedure TfmTableManage.bbNewTriggerClick(Sender: TObject);
begin
   if Assigned(fmTriggers) then
    fmTriggers.bbNewClick(Sender);
end;

procedure TfmTableManage.cbIndexTypeChange(Sender: TObject);
begin
  edIndexName.Text := 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);
end;

procedure TfmTableManage.edDropClick(Sender: TObject);
var TmpFieldName: string;
begin
  if MessageDlg('Are you sure you want to delete the field: ' + sgFields.Cells[1, sgFields.Row] +
    ' with its data', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    TmpFieldName := MakeCaseSensitiveAuto(sgFields.Cells[1, sgFields.Row]);

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop field', 'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) + ' DROP ' +
         TmpFieldName  , @bbRefreshFieldsClick);
  end;
end;

procedure TfmTableManage.bbEditPermissionClick(Sender: TObject);
begin
  if Assigned(fmTablePermissions) then
    fmTablePermissions.bbEditPermissionClick(Sender);
end;

procedure TfmTableManage.Init(dbIndex: Integer; TableName: string; ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
  FDBIndex := dbIndex;
  FTableName := TableName;

  // Extractor für konsistente Abfragen erstellen
  if Assigned(FExtractor) then
    FreeAndNil(FExtractor);
  FExtractor := TSimpleObjExtractor.Create(dbIndex);

  try
    if not Assigned(fmPrimaryKey) then
    begin
      fmPrimaryKey := TfmPrimaryKey.Create(Self);
      fmPrimaryKey.Parent := tsPrimaryKey;
      fmPrimaryKey.Align := alClient;
      fmPrimaryKey.BorderStyle := bsNone;
      fmPrimaryKey.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmPrimaryKey.Visible := true;
      fmPrimaryKey.bbClose.Visible := False;  // Close-Button ausblenden (wird über Tabs gesteuert)
    end;

    if not Assigned(fmUniqueConstraints) then
    begin
      fmUniqueConstraints := TfmUniqueConstraints.Create(Self);
      fmUniqueConstraints.Parent := tsUniqueConstraints;
      fmUniqueConstraints.Align := alClient;
      fmUniqueConstraints.BorderStyle := bsNone;
      fmUniqueConstraints.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmUniqueConstraints.Visible := true;
      fmUniqueConstraints.bbClose.Visible := false;
    end;

    if not Assigned(fmCheckConstraints) then
    begin
      fmCheckConstraints := TfmCheckConstraints.Create(Self);
      fmCheckConstraints.Parent := tsCheckConstraints;
      fmCheckConstraints.Align := alClient;
      fmCheckConstraints.BorderStyle := bsNone;
      fmCheckConstraints.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmCheckConstraints.Visible := true;
      fmCheckConstraints.bbClose.Visible := false;
    end;

    if not Assigned(fmNotNullConstraints) then
    begin
      fmNotNullConstraints := TfmNotNullConstraints.Create(Self);
      fmNotNullConstraints.Parent := tsNotNullConstraints;
      fmNotNullConstraints.Align := alClient;
      fmNotNullConstraints.BorderStyle := bsNone;
      fmNotNullConstraints.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmNotNullConstraints.Visible := true;
      fmNotNullConstraints.bbClose.Visible := false;
    end;

    if not Assigned(fmReferences) then
    begin
      fmReferences := TfmReferences.Create(Self);
      fmReferences.Parent := tsReferences;
      fmReferences.Align := alClient;
      fmReferences.BorderStyle := bsNone;
      fmReferences.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmReferences.Visible := True;
      fmReferences.bbRefreshReferences.Visible := True;
    end;

    if not Assigned(fmForeignKeys) then
    begin
      fmForeignKeys := TfmForeignKeys.Create(Self);
      fmForeignKeys.Parent := tsForeignKeys;
      fmForeignKeys.Align := alClient;
      fmForeignKeys.BorderStyle := bsNone;
      fmForeignKeys.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmForeignKeys.Visible := True;
    end;

    if not Assigned(fmTriggers) then
    begin
      fmTriggers := TfmTriggers.Create(Self);
      fmTriggers.Parent := tsTriggers;
      fmTriggers.Align := alClient;
      fmTriggers.BorderStyle := bsNone;
      fmTriggers.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmTriggers.Visible := True;
    end;

    if not Assigned(fmTablePermissions) then
    begin
      fmTablePermissions := TfmTablePermissions.Create(Self);
      fmTablePermissions.Parent := tsPermissions;
      fmTablePermissions.Align := alClient;
      fmTablePermissions.BorderStyle := bsNone;
      fmTablePermissions.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmTablePermissions.Visible := True;
    end;

  except
    on E: Exception do
    begin
      MessageDlg('Error while initializing Table Management: ' + e.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TfmTableManage.tsFieldsShow(Sender: TObject);
begin
  FillFields;
end;

procedure TfmTableManage.tsIndicesShow(Sender: TObject);
begin
  FillIndices;
end;

procedure TfmTableManage.tsNotNullConstraintsShow(Sender: TObject);
begin
  if Assigned(fmNotNullConstraints) then
    fmNotNullConstraints.FillNotNullConstraints;
end;

procedure TfmTableManage.tsForeignKeysShow(Sender: TObject);
begin
  if Assigned(fmForeignKeys) then
    fmForeignKeys.FillForeignKeys;
end;

procedure TfmTableManage.tsTriggersShow(Sender: TObject);
begin
  if Assigned(fmTriggers) then
    fmTriggers.FillTriggers;
end;

procedure TfmTableManage.tsUniqueConstraintsShow(Sender: TObject);
begin
  if Assigned(fmUniqueConstraints) then
    fmUniqueConstraints.FillUniqueConstraints;
end;

procedure TfmTableManage.tsReferencesShow(Sender: TObject);
begin
  if Assigned(fmReferences) then
    fmReferences.FillReferences;
end;

procedure TfmTableManage.tsPermissionsShow(Sender: TObject);
begin
  if Assigned(fmTablePermissions) then
    fmTablePermissions.FillPermissions;
end;

procedure TfmTableManage.tsPrimaryKeyShow(Sender: TObject);
begin
  if Assigned(fmPrimaryKey) then
    fmPrimaryKey.FillPrimaryKey;
end;

//On RefreshButton.Click
procedure TfmTableManage.bbRefreshFieldsClick(Sender: TObject);
begin
  FillFields;
end;

procedure TfmTableManage.bbRefreshIndicesClick(Sender: TObject);
begin
  FillIndices;
end;

procedure TfmTableManage.bbRefreshForeignKeysClick(Sender: TObject);
begin
  if Assigned(fmForeignKeys) then
    fmForeignKeys.FillForeignKeys;
end;

procedure TfmTableManage.bbRefreshTriggersClick(Sender: TObject);
begin
  if Assigned(fmTriggers) then
    fmTriggers.FillTriggers;
end;

procedure TfmTableManage.bbRefreshReferencesClick(Sender: TObject);
begin
  if Assigned(fmReferences) then
    fmReferences.FillReferences;
end;

procedure TfmTableManage.Button1Click(Sender: TObject);
begin
  turbocommon.MetaDataChanged := true;
  Close;
  Parent.Free;
end;

procedure TfmTableManage.bbRefreshPermissionsClick(Sender: TObject);
begin
  if Assigned(fmTablePermissions) then
    fmTablePermissions.FillPermissions;
end;

procedure TfmTableManage.FillIndices;
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

procedure TfmTableManage.FillTriggers;
begin
  if Assigned(fmTriggers) then
    fmTriggers.FillTriggers;
end;

procedure TfmTableManage.FillPermissions;
begin
  if Assigned(fmTablePermissions) then
    fmTablePermissions.FillPermissions;
end;

procedure TfmTableManage.FillFields;
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

      // Computed Field  Ausdruck mit COMPUTED BY-Schlüsselwort anzeigen
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
      ConstraintName := TmpConstraintName;

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


initialization
  {$I tablemanage.lrs}

end.

