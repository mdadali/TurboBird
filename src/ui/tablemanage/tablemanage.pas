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
  NotNullConstraints
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
    sgReferences: TStringGrid;
    sgTriggers: TStringGrid;
    sgPermissions: TStringGrid;
    sgFields: TStringGrid;
    sgIndices: TStringGrid;
    sgForeignKeys: TStringGrid;
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
    procedure sgPermissionsDblClick(Sender: TObject);
    procedure sgTriggersDblClick(Sender: TObject);
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
    procedure FillReferences;
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

procedure TfmTableManage.sgPermissionsDblClick(Sender: TObject);
begin
  // Double clicking allows user to edit permissions
  bbEditPermissionClick(Sender);
end;

procedure TfmTableManage.sgTriggersDblClick(Sender: TObject);
begin
  // Double clicking allows user to edit trigger
  bbEditTriggerClick(Sender);
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
var
  ATriggerName: string;
  List: TStringList;
begin
  if sgTriggers.RowCount > 1 then
  begin
    List := TStringList.Create;
    try
      ATriggerName := sgTriggers.Cells[0, sgTriggers.Row];
      FExtractor.GetTriggerScript(ATriggerName, List);
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Trigger', List.Text, bbRefreshTriggers.OnClick);
    finally
      List.Free;
    end;
  end;
end;

// ============================================================
// FillForeignKeys vereinfacht mit FExtractor:
// ============================================================
procedure TfmTableManage.FillForeignKeys;
var
  FKs: TFBForeignKeyDefArray;
  i, Row: Integer;
begin
  sgForeignKeys.RowCount := 1;

  FKs := FExtractor.GetTableForeignKeys(FTableName);

  for i := 0 to High(FKs) do
  begin
    sgForeignKeys.RowCount := sgForeignKeys.RowCount + 1;
    Row := sgForeignKeys.RowCount - 1;

    sgForeignKeys.Cells[0, Row] := FKs[i].ConstraintName;   // Constraint Name
    sgForeignKeys.Cells[1, Row] := FKs[i].KeyName;          // Key Name
    sgForeignKeys.Cells[2, Row] := FKs[i].OnFields;         // On Fields
    sgForeignKeys.Cells[3, Row] := FKs[i].RefTable;         // Foreign Table
    sgForeignKeys.Cells[4, Row] := FKs[i].RefFields;        // Foreign Key (= RefFields)
    sgForeignKeys.Cells[5, Row] := FKs[i].UpdateRule;       // Update Rule
    sgForeignKeys.Cells[6, Row] := FKs[i].DeleteRule;       // Delete Rule
  end;

  if sgForeignKeys.RowCount > 1 then
    sgForeignKeys.Row := 1;
end;

procedure TfmTableManage.bbNewForeignKeyClick(Sender: TObject);
var
  FieldsList: TStringList;
  TableList: TStringList;
  RawFields: TFBFieldRawArray;
  i: Integer;
  ServerVersionMajor: Word;
begin
  // ============================================================
  // Versions-Check: FK-Anlage auf FB 1.5 nicht möglich
  // (Server verlangt exklusiven Zugriff — TurboBird hält aber
  //  mehrere Verbindungen offen: Main-Tree, Extractor, Form)
  // ============================================================
  ServerVersionMajor := RegisteredDatabases[FDBIndex].RegRec.ServerVersionMajor;

  if ServerVersionMajor < 2 then
  begin
    ShowMessage(
      'Foreign Key creation is not supported on Firebird 1.5.' + sLineBreak +
      sLineBreak +
      'Firebird 1.5 requires exclusive database access when adding ' +
      'a new Foreign Key. TurboBird has active connections to this ' +
      'database, so the server will reject the operation.' + sLineBreak +
      sLineBreak +
      'Workaround:' + sLineBreak +
      '  Use isql or another tool with a single connection to create' + sLineBreak +
      '  the Foreign Key, then refresh this tab.' + sLineBreak +
      sLineBreak +
      'See "Known Limitations" in the documentation for details.'
    );
    Exit;
  end;

  // ============================================================
  // 1. Felder der aktuellen Tabelle (On-Fields)
  // ============================================================
  FieldsList := TStringList.Create;
  try
    RawFields := FExtractor.GetTableFieldsRaw(FTableName);
    for i := 0 to High(RawFields) do
      FieldsList.Add(RawFields[i].FieldName);

    fmNewForeignKey.clxOnFields.Clear;
    fmNewForeignKey.clxOnFields.Items.AddStrings(FieldsList);
  finally
    FieldsList.Free;
  end;

  fmNewForeignKey.edNewName.Text := 'FK_' + FTableName + '_' +
    IntToStr(sgForeignKeys.RowCount);

  // ============================================================
  // 2. Fremdtabellen-Liste (Ziel-Tabellen für FK)
  // ============================================================
  TableList := TStringList.Create;
  try
    FExtractor.ExtractObjectNames(FDBIndex, otTables, false, TStrings(TableList), '');

    fmNewForeignKey.cbTables.Items.Clear;
    fmNewForeignKey.cbTables.Items.AddStrings(TableList);
  finally
    TableList.Free;
  end;

  fmNewForeignKey.DatabaseIndex := FDBIndex;
  fmNewForeignKey.laTable.Caption := FTableName;
  fmNewForeignKey.Caption := 'New Foreign Key for: ' + FTableName;

  if fmNewForeignKey.ShowModal = mrOK then
  begin
    if  Assigned(fmNewForeignKey.QWindow) then
      fmNewForeignKey.QWindow.OnCommit := @bbRefreshForeignKeysClick;
  end;
end;

procedure TfmTableManage.bbDropForeignKeyClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  FKName: string;
begin
  if sgForeignKeys.Row <= 0 then
    Exit;

  FKName := sgForeignKeys.Cells[0, sgForeignKeys.Row];
  if FKName = '' then
    Exit;

  if MessageDlg('Are you sure you want to drop ' + FKName + '?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Drop Foreign Key: ' + FKName);
    QWindow.meQuery.Lines.Text :=
      'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
      ' DROP CONSTRAINT ' + MakeCaseSensitiveAuto(FKName);
    fmMain.Show;
    QWindow.OnCommit := @bbRefreshForeignKeysClick;
  end;
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
var
  ATriggerName: string;
begin
  if (sgTriggers.RowCount > 1) and
    (MessageDlg('Are You sure to drop this trigger', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ATriggerName:= sgTriggers.Cells[0, sgTriggers.Row];
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop Trigger : ' + ATriggerName,
        'drop trigger ' + ATriggerName, bbRefreshTriggers.OnClick);

  end;
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
var
  fmPermissions: TfmPermissionManage;
  dbIndex: Integer;
  ATab: TTabSheet;
  Title, FullHint, DBAlias: string;
begin
  if not Assigned(FNodeInfos) then
    Exit;

  dbIndex := FDBIndex;

  Title := 'Add User Permission: ' + FTableName;

  // Permission-Form als Tab in TableManage anlegen
  fmPermissions := TfmPermissionManage.Create(Application);
  ATab := TTabSheet.Create(Self);
  ATab.Parent := PageControl1;
  ATab.ImageIndex := TPNodeInfos(FNodeInfos)^.ImageIndex;
  fmPermissions.Parent := ATab;
  fmPermissions.Align := alClient;
  fmPermissions.BorderStyle := bsNone;

  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  ATab.Caption := Title;
  fmPermissions.Caption := Title;

  // Hint
  if Assigned(FNodeInfos^.OwnerNode) then
  begin
    DBAlias := GetAncestorNodeText(FNodeInfos^.OwnerNode, 1);
    FullHint :=
      'Server:   ' + GetAncestorNodeText(FNodeInfos^.OwnerNode, 0) + sLineBreak +
      'DBAlias:  ' + DBAlias + sLineBreak +
      'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
      'Object type: Table Permissions' + sLineBreak +
      'Table: ' + FTableName + sLineBreak +
      'Action: Add User';
    ATab.Hint := FullHint;
    ATab.ShowHint := True;
  end;

  // Init mit FNodeInfos + FExtractor (UserType = 1 für User)
  fmPermissions.Init(FNodeInfos, dbIndex, FTableName, '', 1,
    FExtractor, @bbRefreshPermissionsClick);
  fmPermissions.Show;
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
  fmMain.CreateNewTrigger(FDBIndex, FTableName, bbRefreshTriggers.OnClick);
end;

procedure TfmTableManage.FillReferences;
var
  Refs: TForeignKeyInfoArray;
  i, Row: Integer;
begin
  sgReferences.RowCount := 1;

  Refs := FExtractor.GetTableReferences(MakeCaseSensitiveAuto(FTableName));

  for i := 0 to High(Refs) do
  begin
    sgReferences.RowCount := sgReferences.RowCount + 1;
    Row := sgReferences.RowCount - 1;

    sgReferences.Cells[0, Row] := MakeCaseSensitiveAuto(Refs[i].ConstraintName);    // FK-Name
    sgReferences.Cells[1, Row] := MakeCaseSensitiveAuto(Refs[i].ForeignTable);      // referenzierende Tabelle
    sgReferences.Cells[2, Row] := MakeCaseSensitiveAuto(Refs[i].ForeignFields);     // Feld(er) dort
    sgReferences.Cells[3, Row] := MakeCaseSensitiveAuto(Refs[i].MasterFields);      // Feld(er) bei uns
  end;

  if sgReferences.RowCount > 1 then
    sgReferences.Row := 1;
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
var
  fmPermissions: TfmPermissionManage;
  UserType, dbIndex: Integer;
  ATab: TTabSheet;
  Title, FullHint, DBAlias, UserOrRole: string;
  DBNode: TTreeNode;
begin
  if sgPermissions.Row <= 0 then
  begin
    ShowMessage('There is no selected user/role');
    Exit;
  end;

  if not Assigned(FNodeInfos) then
    Exit;

  // User/Role unterscheiden
  if sgPermissions.Cells[1, sgPermissions.Row] = 'User' then
    UserType := 1
  else
    UserType := 2;
  UserOrRole := sgPermissions.Cells[0, sgPermissions.Row];

  dbIndex := FDBIndex;

  Title := 'Permissions:' + FTableName + ':' + UserOrRole;

  // Permission-Form als Tab in TableManage anlegen (nicht im Tree-Node speichern!)
  fmPermissions := TfmPermissionManage.Create(Application);
  ATab := TTabSheet.Create(Self);
  ATab.Parent := PageControl1;
  ATab.ImageIndex := TPNodeInfos(FNodeInfos)^.ImageIndex;
  fmPermissions.Parent := ATab;
  fmPermissions.Align := alClient;
  fmPermissions.BorderStyle := bsNone;

  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  ATab.Caption := Title;
  fmPermissions.Caption := Title;

  // Hint
  if Assigned(FNodeInfos^.OwnerNode) then
  begin
    DBAlias := GetAncestorNodeText(FNodeInfos^.OwnerNode, 1);
    FullHint :=
      'Server:   ' + GetAncestorNodeText(FNodeInfos^.OwnerNode, 0) + sLineBreak +
      'DBAlias:  ' + DBAlias + sLineBreak +
      'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
      'Object type: Table Permissions' + sLineBreak +
      'Table: ' + FTableName + sLineBreak +
      'Granted to: ' + UserOrRole + sLineBreak;
    if UserType = 1 then
      FullHint := FullHint + 'Type: User'
    else
      FullHint := FullHint + 'Type: Role';
    ATab.Hint := FullHint;
    ATab.ShowHint := True;
  end;

  // Init mit FNodeInfos (nicht SelNode!) und FExtractor
  fmPermissions.Init(FNodeInfos, dbIndex, FTableName, UserOrRole, UserType,
    FExtractor, @bbRefreshPermissionsClick);
  fmPermissions.Show;
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
  FillForeignKeys;
end;

procedure TfmTableManage.tsTriggersShow(Sender: TObject);
begin
  FillTriggers;
end;

procedure TfmTableManage.tsUniqueConstraintsShow(Sender: TObject);
begin
  if Assigned(fmUniqueConstraints) then
    fmUniqueConstraints.FillUniqueConstraints;
end;

procedure TfmTableManage.tsReferencesShow(Sender: TObject);
begin
  FillReferences;
end;

procedure TfmTableManage.tsPermissionsShow(Sender: TObject);
begin
  FillPermissions;
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
  FillForeignKeys;
end;

procedure TfmTableManage.bbRefreshTriggersClick(Sender: TObject);
begin
  FillTriggers;
end;

procedure TfmTableManage.bbRefreshReferencesClick(Sender: TObject);
begin
  FillReferences;
end;

procedure TfmTableManage.Button1Click(Sender: TObject);
begin
  turbocommon.MetaDataChanged := true;
  Close;
  Parent.Free;
end;

procedure TfmTableManage.bbRefreshPermissionsClick(Sender: TObject);
begin
  FillPermissions;
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
var
  Triggers: TFBTriggerRawArray;
  i, Row: Integer;
begin
  sgTriggers.RowCount := 1;

  Triggers := FExtractor.GetTableTriggersRaw(FTableName);

  for i := 0 to High(Triggers) do
  begin
    sgTriggers.RowCount := sgTriggers.RowCount + 1;
    Row := sgTriggers.RowCount - 1;

    sgTriggers.Cells[0, Row] := Triggers[i].TriggerName;
    if Triggers[i].IsActive then
      sgTriggers.Cells[1, Row] := '1'
    else
      sgTriggers.Cells[1, Row] := '0';
  end;

  if sgTriggers.RowCount > 1 then
    sgTriggers.Row := 1;
end;

procedure TfmTableManage.FillPermissions;
var
  Perms: TFBPermissionArray;
  i, Row: Integer;
  Priv: string;
begin
  sgPermissions.RowCount := 1;

  Perms := FExtractor.GetObjectPermissions(FTableName);

  for i := 0 to High(Perms) do
  begin
    sgPermissions.RowCount := sgPermissions.RowCount + 1;
    Row := sgPermissions.RowCount - 1;

    Priv := Perms[i].Privileges;

    if Perms[i].IsRole then
      sgPermissions.Cells[1, Row] := 'Role'
    else
      sgPermissions.Cells[1, Row] := 'User';

    sgPermissions.Cells[0, Row] := Perms[i].UserName;

    if PermissionHas(Priv, 'S')  then sgPermissions.Cells[2,  Row] := '1' else sgPermissions.Cells[2,  Row] := '0';
    if PermissionHas(Priv, 'I')  then sgPermissions.Cells[3,  Row] := '1' else sgPermissions.Cells[3,  Row] := '0';
    if PermissionHas(Priv, 'U')  then sgPermissions.Cells[4,  Row] := '1' else sgPermissions.Cells[4,  Row] := '0';
    if PermissionHas(Priv, 'D')  then sgPermissions.Cells[5,  Row] := '1' else sgPermissions.Cells[5,  Row] := '0';
    if PermissionHas(Priv, 'R')  then sgPermissions.Cells[6,  Row] := '1' else sgPermissions.Cells[6,  Row] := '0';
    if PermissionHas(Priv, 'SG') then sgPermissions.Cells[7,  Row] := '1' else sgPermissions.Cells[7,  Row] := '0';
    if PermissionHas(Priv, 'IG') then sgPermissions.Cells[8,  Row] := '1' else sgPermissions.Cells[8,  Row] := '0';
    if PermissionHas(Priv, 'UG') then sgPermissions.Cells[9,  Row] := '1' else sgPermissions.Cells[9,  Row] := '0';
    if PermissionHas(Priv, 'DG') then sgPermissions.Cells[10, Row] := '1' else sgPermissions.Cells[10, Row] := '0';
    if PermissionHas(Priv, 'RG') then sgPermissions.Cells[11, Row] := '1' else sgPermissions.Cells[11, Row] := '0';
  end;

  if sgPermissions.RowCount > 1 then
    sgPermissions.Row := 1;
end;

{procedure TfmTableManage.FillFields;
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

      // ---------- Field Type ----------
      // Domain-basiert → nur der Domain-Name (wie bisher)
      // Base Type     → der aufgelöste Firebird-Typ
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

      // Computed Field → Expression anzeigen statt Typ
      if RawFields[i].ComputedSource <> '' then
        sgFields.Cells[2, Row] := RawFields[i].ComputedSource;

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
end;}

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

      // Computed Field → Expression anzeigen statt Typ
      if RawFields[i].ComputedSource <> '' then
        sgFields.Cells[2, Row] := RawFields[i].ComputedSource;

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

