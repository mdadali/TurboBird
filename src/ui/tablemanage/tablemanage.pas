unit TableManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, ComCtrls, Grids, Buttons, StdCtrls, CheckLst, LCLType,
  ExtCtrls, types,
  IB,
  IBDatabase,
  IBQuery,

  fbcommon,
  turbocommon,
  fmetaquerys,

  uthemeselector;

type

  { TfmTableManage }

  TfmTableManage = class(TForm)
      bbClose: TSpeedButton;
    bbCreateIndex: TBitBtn;
    bbDrop: TBitBtn;
    bbDropConstraint: TBitBtn;
    bbEdit: TBitBtn;
    bbNew: TBitBtn;
    bbNewConstraint: TBitBtn;
    bbRefreshFields: TBitBtn;
    bbRefreshConstraint: TBitBtn;
    bbRefreshReferences: TBitBtn;
    bbRefreshIndices: TBitBtn;
    bbRefreshTriggers: TBitBtn;
    bbNewTrigger: TBitBtn;
    bbEditTrigger: TBitBtn;
    bbDropTrigger: TBitBtn;
    bbRefreshPermissions: TBitBtn;
    bbAddUser: TBitBtn;
    cbIndexType: TComboBox;
    cbSortType: TComboBox;
    clbFields: TCheckListBox;
    cxUnique: TCheckBox;
    bbEditPermission: TBitBtn;
    edDrop: TBitBtn;
    edIndexName: TEdit;
    GroupBox2: TGroupBox;
    CurrentIBDatabase: TIBDatabase;
    CurrentIBTransaction: TIBTransaction;
    SQLQuery1: TIBQuery;
    SQLQuery2: TIBQuery;
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
    sgConstraints: TStringGrid;
    tsReferences: TTabSheet;
    tsPermissions: TTabSheet;
    tsTriggers: TTabSheet;
    tsIndices: TTabSheet;
    tsConstraints: TTabSheet;
    tsFields: TTabSheet;
    procedure bbAddUserClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure bbCreateIndexClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbDropConstraintClick(Sender: TObject);
    procedure bbDropTriggerClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure bbEditTriggerClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure bbNewConstraintClick(Sender: TObject);
    procedure bbNewTriggerClick(Sender: TObject);
    procedure bbRefreshFieldsClick(Sender: TObject);
    procedure bbRefreshConstraintClick(Sender: TObject);
    procedure bbRefreshIndicesClick(Sender: TObject);
    procedure bbRefreshPermissionsClick(Sender: TObject);
    procedure bbRefreshTriggersClick(Sender: TObject);
    procedure bbRefreshReferencesClick(Sender: TObject);
    procedure cbIndexTypeChange(Sender: TObject);
    procedure edDropClick(Sender: TObject);
    procedure bbEditPermissionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);
    procedure sgPermissionsDblClick(Sender: TObject);
    procedure sgTriggersDblClick(Sender: TObject);
    procedure tsConstraintsShow(Sender: TObject);
    procedure tsFieldsContextPopup(Sender: TObject; MousePos: TPoint;
        var Handled: Boolean);
    procedure tsFieldsShow(Sender: TObject);
    procedure tsIndicesShow(Sender: TObject);
    procedure tsPermissionsShow(Sender: TObject);
    procedure tsReferencesShow(Sender: TObject);
    procedure tsTriggersShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: Integer;
    FTableName: string;
  public
    PKeyName,
    ConstraintName: string;
    procedure Init(dbIndex: Integer; TableName: string; ANodeInfos: TPNodeInfos);
    // Fill grid with constraint info
    { Todo: fillconstraints relies on a query being set correctly elsewhere; get
    rid of that - see e.g. FillPermissions }
    procedure FillConstraints;
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

uses NewEditField, Main, QueryWindow, SysTables, NewConstraint, PermissionManage;

procedure TfmTableManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.EditorForm := nil;

  if SQLQuery1.Active then
    SQLQuery1.Close;

  if SQLQuery2.Active then
    SQLQuery2.Close;

  if CurrentIBTransaction.InTransaction then
    CurrentIBTransaction.Commit;

  if CurrentIBDatabase.Connected then
    CurrentIBDatabase.Connected := false;

  CloseAction:= caFree;
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

procedure TfmTableManage.tsFieldsContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TfmTableManage.bbEditClick(Sender: TObject);
var
  fmNewEditField: TfmNewEditField;
  FieldName, FieldType,
  DefaultValue, Characterset, Collation, Description: string;
  FieldOrder, FieldSize, FieldPrecision, FieldScale: Integer;
  AllowNull: Boolean;
begin
  fmNewEditField:= TfmNewEditField.Create(nil);
  {todo: getting info from gui elements that got it from a function that got it
   from a query is awful. Rework to use e.g. the function}
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
      FieldSize, FieldPrecision, FieldScale, FieldOrder, AllowNull, bbRefreshFields);

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
    List:= TStringList.Create;
    try
      ATriggerName:= sgTriggers.Cells[0, sgTriggers.Row];
      dmSysTables.ScriptTrigger(FDBIndex, ATriggerName, List);
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Trigger ', List.Text, bbRefreshTriggers.OnClick);
    finally
      List.Free;
    end;
  end;

end;

procedure TfmTableManage.bbDropClick(Sender: TObject);
begin
  with sgIndices do
  begin
    if (RowCount > 1) and
      (MessageDlg('Are you sure you want to drop index: ' + Cells[0, Row], mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
    begin
      if Cells[0, Row] = PKeyName then // Delete primary key
        fmMain.ShowCompleteQueryWindow(FDBIndex,  'Drop Primary Key on Table: ' + FTableName,
          'alter table ' + FTableName + ' DROP CONSTRAINT ' + ConstraintName, bbRefreshIndices.OnClick)
      else // Delete normal index
        fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop Index on table: ' + FTableName,
          'DROP INDEX ' + Cells[0, Row], bbRefreshIndices.OnClick);
    end;
  end;
end;

procedure TfmTableManage.bbDropConstraintClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  ConstName: string;
begin
  if sgConstraints.Row > 0 then
  begin
    ConstName:= sgConstraints.Cells[0, sgConstraints.Row];
    if MessageDlg('Are you sure you want to drop ' + ConstName, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      QWindow:= fmMain.ShowQueryWindow(FDBIndex, 'drop constraint: ' + ConstName);
      QWindow.meQuery.Lines.Text:= 'ALTER TABLE ' + FTableName + ' DROP CONSTRAINT ' + ConstName;
      fmMain.Show;
      QWindow.OnCommit:= bbRefreshConstraint.OnClick;
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
  Fields:= '';
  for i:= 0 to clbFields.Count - 1 do
    if clbFields.Checked[i] then
      Fields:= Fields + Trim(clbFields.Items[i]) + ',';
  Delete(Fields, Length(Fields), 1);

  if Trim(Fields) = '' then
    MessageDlg('Error', 'You should select at least one field', mtError, [mbOk], 0)
  else
  if Trim(edIndexName.Text) = '' then
    MessageDlg('Error', 'You should enter the new index name', mtError, [mbOk], 0)
  else
  begin
    QWindow:= fmMain.ShowQueryWindow(FDBIndex, 'Create new index');
    QWindow.meQuery.Lines.Clear;

    if cbIndexType.ItemIndex = 0 then // primary key
    begin
      QWindow.meQuery.Lines.Text:= 'alter table ' + FTableName + LineEnding +
      'add constraint ' + edIndexName.Text + LineEnding +
      'primary key (' + Fields + ')';
    end
    else    // Normal index
    begin
      FirstLine:= 'create ';
      if cxUnique.Checked then
        FirstLine:= FirstLine + 'unique ';
      FirstLine:= FirstLine + cbSortType.Text + ' index ' + edIndexName.Text;
      QWindow.meQuery.Lines.Text:= FirstLine + LineEnding + 'on ' + FTableName + LineEnding + Fields;
    end;

    QWindow.OnCommit:= bbRefreshIndices.OnClick;
    QWindow.Show;
  end;
end;

{procedure TfmTableManage.bbAddUserClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
begin
  fmPermissions:= TfmPermissionManage.Create(nil);
  fmPermissions.Init(nil, FDBIndex, FTableName, '', 1, bbRefreshPermissions.OnClick);
  fmPermissions.Show;
end;}

procedure TfmTableManage.bbAddUserClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  dbIndex: Integer;
  ATab: TTabSheet;
  Title, FullHint, DBAlias: string;
  UserRole: string;
begin
  SelNode := fmMain.tvMain.Selected;
  if (SelNode = nil) or (SelNode.Data = nil) then Exit;
  NodeInfos := TPNodeInfos(SelNode.Data);
  dbIndex := FDBIndex;  // Tabellenkontext kommt aus Form

  Title := 'Add User Permission: ' + FTableName;

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.EditorForm) and (NodeInfos^.EditorForm is TfmPermissionManage) then
    fmPermissions := TfmPermissionManage(NodeInfos^.EditorForm)
  else
  begin
    fmPermissions := TfmPermissionManage.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    fmPermissions.Parent := ATab;
    fmPermissions.Align := alClient;
    fmPermissions.BorderStyle := bsNone;

    NodeInfos^.EditorForm := fmPermissions;
  end;

  // Tab vorbereiten
  ATab := fmPermissions.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Tab-Titel
  ATab.Caption := Title;
  fmPermissions.Caption := Title;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
    'DBAlias:  ' + DBAlias + sLineBreak +
    'DBPath:   ' + RegisteredDatabases[dbIndex].IBDatabase.DatabaseName + sLineBreak +
    'Object type: Table Permissions' + sLineBreak +
    'Table: ' + FTableName + sLineBreak +
    'Action: Add User';
  ATab.Hint := FullHint;
  ATab.ShowHint := True;
  UserRole := sgPermissions.Cells[0, sgPermissions.Row];
  // Form initialisieren (UserType = 1 für User)
  fmPermissions.Init(NodeInfos, dbIndex, FTableName, UserRole, 1, @bbRefreshPermissionsClick);
  fmPermissions.Show;
end;


procedure TfmTableManage.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
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
      0, 0, 0, 0, True, bbRefreshFields);
    Caption:= 'Add new field to Table: ' + FTableName;
    Show;
  end;
end;

procedure TfmTableManage.bbNewConstraintClick(Sender: TObject);
var
  Count: Integer;
  FieldsList: TStringList;
  Iso: TIsolatedQuery;
begin
  // Get current fields
  FieldsList:= TStringList.Create;
  try
    Iso := GetFieldsIsolated(RegisteredDatabases[FDBIndex].IBDatabase, FTableName, FieldsList);
    fmNewConstraint.clxOnFields.Clear;
    fmNewConstraint.clxOnFields.Items.AddStrings(FieldsList);
  finally
    FieldsList.Free;
    Iso.Free;
  end;
  fmNewConstraint.edNewName.Text:= 'FK_' + FTableName + '_' + IntToStr(sgConstraints.RowCount);

  // Foreign tables
  fmNewConstraint.cbTables.Items.CommaText:= dmSysTables.GetDBObjectNames(FDBIndex, otTables, Count);
  fmNewConstraint.DatabaseIndex:= FDBIndex;

  fmNewConstraint.laTable.Caption:= FTableName;
  fmNewConstraint.Caption:= 'New Constraint for : ' + FTableName;
  if fmNewConstraint.ShowModal = mrOK then
  begin
    fmNewConstraint.QWindow.OnCommit:= bbRefreshConstraint.OnClick;
  end;
end;

procedure TfmTableManage.bbNewTriggerClick(Sender: TObject);
begin
  fmMain.CreateNewTrigger(FDBIndex, FTableName, bbRefreshTriggers.OnClick);
end;

procedure TfmTableManage.FillReferences;
begin
  CurrentIBTransaction.Commit;
  dmSysTables.Init(FDBIndex);
  dmSysTables.GetConstraintsOfTable(FTableName, SQLQuery1);
  sgReferences.RowCount:= 1;

  SQLQuery1.First;
  with SQLQuery1, sgReferences do
  while not EOF do
  begin
    RowCount:= RowCount + 1;
    Cells[0, RowCount - 1]:= FieldByName('ConstName').AsString;
    Cells[1, RowCount - 1]:= FieldByName('OtherTableName').AsString;
    Cells[2, RowCount - 1]:= FieldByName('OtherFieldName').AsString;
    Cells[3, RowCount - 1]:= FieldByName('KeyName').AsString;
    Next;
  end;
  SQLQuery1.Close;
end;

procedure TfmTableManage.cbIndexTypeChange(Sender: TObject);
begin
  case cbIndexType.ItemIndex of
    0: edIndexName.Text:= 'PK_' + FTableName + '_1';
    1: edIndexName.Text:= 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);
  end;
end;

procedure TfmTableManage.edDropClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to delete the field: ' + sgFields.Cells[1, sgFields.Row] +
    ' with its data', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop field', 'ALTER TABLE ' + FTableName + ' DROP ' +
      sgFields.Cells[1, sgFields.Row], @bbRefreshFieldsClick);
  end;
end;

{procedure TfmTableManage.bbEditPermissionClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  UserType: Integer;
begin
  if sgPermissions.Row > 0 then
  begin
    if sgPermissions.Cells[1, sgPermissions.Row] = 'User' then
      UserType:= 1
    else
      UserType:= 2;
    fmPermissions:= TfmPermissionManage.Create(nil);
    fmPermissions.Init(nil, FDBIndex, FTableName, sgPermissions.Cells[0, sgPermissions.Row], UserType, @bbRefreshPermissionsClick);
    fmPermissions.Show;
  end
  else
    ShowMessage('There is no selected user/role');
end;}

procedure TfmTableManage.bbEditPermissionClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  UserType, dbIndex: Integer;
  SelNode: TTreeNode;
  NodeInfos: TPNodeInfos;
  ATab: TTabSheet;
  Title, FullHint, DBAlias, UserOrRole: string;
begin
  if sgPermissions.Row <= 0 then
  begin
    ShowMessage('There is no selected user/role');
    Exit;
  end;

  // User/Role unterscheiden
  if sgPermissions.Cells[1, sgPermissions.Row] = 'User' then
    UserType := 1
  else
    UserType := 2;
  UserOrRole := sgPermissions.Cells[0, sgPermissions.Row];

  // Aktuellen Node ermitteln
  SelNode := fmMain.tvMain.Selected;
  if (SelNode = nil) or (SelNode.Data = nil) then Exit;
  NodeInfos := TPNodeInfos(SelNode.Data);
  dbIndex := FDBIndex;  // kommt aus TfmTableManage, nicht vom Node

  Title := 'Permissions:' + FTableName + ':' + UserOrRole;

  // Prüfen, ob ViewForm schon existiert
  if Assigned(NodeInfos^.EditorForm) and (NodeInfos^.EditorForm is TfmPermissionManage) then
    fmPermissions := TfmPermissionManage(NodeInfos^.EditorForm)
  else
  begin
    fmPermissions := TfmPermissionManage.Create(Application);
    ATab := TTabSheet.Create(Self);
    ATab.Parent := PageControl1;
    ATab.ImageIndex := SelNode.ImageIndex;
    fmPermissions.Parent := ATab;
    fmPermissions.Align := alClient;
    fmPermissions.BorderStyle := bsNone;

    NodeInfos^.EditorForm := fmPermissions;
  end;

  // Tab vorbereiten
  ATab := fmPermissions.Parent as TTabSheet;
  PageControl1.ActivePage := ATab;
  ATab.Tag := dbIndex;

  // Tab-Titel
  ATab.Caption := Title;
  fmPermissions.Caption := Title;

  // Detaillierte Infos als Hint
  DBAlias := GetAncestorNodeText(SelNode, 1);
  FullHint :=
    'Server:   ' + GetAncestorNodeText(SelNode, 0) + sLineBreak +
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

  // Form initialisieren
  fmPermissions.Init(NodeInfos, dbIndex, FTableName, UserOrRole, UserType, @bbRefreshPermissionsClick);
  fmPermissions.Show;
end;

procedure TfmTableManage.Init(dbIndex: Integer; TableName: string; ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
  FDBIndex:= dbIndex;
  FTableName:= TableName;

  try
    if SQLQuery1.Active then
      SQLQuery1.Close;

    if SQLQuery2.Active then
      SQLQuery2.Close;

    if CurrentIBTransaction.InTransaction then
       CurrentIBTransaction.Commit;

    if CurrentIBDatabase.Connected then
      CurrentIBDatabase.Connected := false;

    if AreSameDB(CurrentIBDatabase, RegisteredDatabases[dbIndex].IBDatabase) then
      exit;

    AssignIBDatabase(RegisteredDatabases[dbIndex].IBDatabase, CurrentIBDatabase);
    CurrentIBDatabase.DefaultTransaction := CurrentIBTransaction;

    SQLQuery1.DataBase:= CurrentIBDatabase;
    SQLQuery1.Transaction := CurrentIBTransaction;
    SQLQuery2.DataBase:= CurrentIBDatabase;
    SQLQuery2.Transaction := CurrentIBTransaction;

    //CurrentIBDatabase.Connected := true;

  except
    on E: Exception do
    begin
      MessageDlg('Error while initalizing Table Management: ' + e.Message, mtError, [mbOk], 0);
    end;
  end;
end;

{procedure TfmTableManage.FillConstraints;
var
  IsoConstraints: TIsolatedQuery;
begin
  sgConstraints.RowCount := 1;
  IsoConstraints := GetTableConstraintsIsolated(
    RegisteredDatabases[FDBIndex].IBConnection,
    FTableName, nil
  );

  try
    with sgConstraints do
    while not IsoConstraints.Query.EOF do
    begin
      RowCount := RowCount + 1;
      Cells[0, RowCount - 1] := IsoConstraints.Query.FieldByName('ConstName').AsString;
      Cells[1, RowCount - 1] := IsoConstraints.Query.FieldByName('KeyName').AsString;
      Cells[2, RowCount - 1] := IsoConstraints.Query.FieldByName('OtherFieldName').AsString;
      Cells[3, RowCount - 1] := IsoConstraints.Query.FieldByName('CurrentTableName').AsString;
      // Neuer isolierter Aufruf statt alter Funktion
      Cells[4, RowCount - 1] := GetConstraintForeignKeyFieldsIsolated(
        RegisteredDatabases[FDBIndex].IBConnection,
        IsoConstraints.Query.FieldByName('CurrentFieldName').AsString
      );
      Cells[5, RowCount - 1] := IsoConstraints.Query.FieldByName('UpdateRule').AsString;
      Cells[6, RowCount - 1] := IsoConstraints.Query.FieldByName('DeleteRule').AsString;
      IsoConstraints.Query.Next;
    end;
  finally
    IsoConstraints.Free;
  end;
end;}

procedure TfmTableManage.FillConstraints;
var
  IsoConstraints: TIsolatedQuery;
begin
  sgConstraints.RowCount := 1;
  IsoConstraints := GetTableConstraintsIsolated( RegisteredDatabases[FDBIndex].IBDatabase, FTableName, nil);

  try
    with sgConstraints do
    while not IsoConstraints.Query.EOF do
    begin
      RowCount := RowCount + 1;
      Cells[0, RowCount - 1] := IsoConstraints.Query.FieldByName('ConstName').AsString;
      Cells[1, RowCount - 1] := IsoConstraints.Query.FieldByName('KeyName').AsString;
      Cells[2, RowCount - 1] := IsoConstraints.Query.FieldByName('OtherFieldName').AsString;
      Cells[3, RowCount - 1] := IsoConstraints.Query.FieldByName('CurrentTableName').AsString;
      // ✨ alle FK-Felder zusammenfassen
      Cells[4, RowCount - 1] := GetConstraintForeignKeyFieldsIsolated(
        RegisteredDatabases[FDBIndex].IBDatabase,
        IsoConstraints.Query.FieldByName('CurrentFieldName').AsString
      );
      Cells[5, RowCount - 1] := IsoConstraints.Query.FieldByName('UpdateRule').AsString;
      Cells[6, RowCount - 1] := IsoConstraints.Query.FieldByName('DeleteRule').AsString;

      IsoConstraints.Query.Next;
    end;
  finally
    IsoConstraints.Free;
  end;
end;

//OnTab.Show
procedure TfmTableManage.tsFieldsShow(Sender: TObject);
begin
  FillFields;
end;

procedure TfmTableManage.tsIndicesShow(Sender: TObject);
begin
  FillIndices;
end;

procedure TfmTableManage.tsConstraintsShow(Sender: TObject);
begin
  FillConstraints;
end;

procedure TfmTableManage.tsTriggersShow(Sender: TObject);
begin
  FillTriggers;
end;

procedure TfmTableManage.tsReferencesShow(Sender: TObject);
begin
  FillReferences;
end;

procedure TfmTableManage.tsPermissionsShow(Sender: TObject);
begin
  FillPermissions;
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

procedure TfmTableManage.bbRefreshConstraintClick(Sender: TObject);
begin
  FillConstraints;
end;

procedure TfmTableManage.bbRefreshTriggersClick(Sender: TObject);
begin
  FillTriggers;
end;

procedure TfmTableManage.bbRefreshReferencesClick(Sender: TObject);
begin
  FillReferences;
end;

procedure TfmTableManage.bbRefreshPermissionsClick(Sender: TObject);
begin
  FillPermissions;
end;

procedure TfmTableManage.FillIndices;
var
  i: Integer;
  IndexFields: string;
  CurrentRow: Integer;
  FieldsList: TStringList;
  TmpConstraintName: ansistring;
  IsoIndices, IsoFields, IsoIndexFields: TIsolatedQuery;
begin
  if Assigned(RegisteredDatabases[FDBIndex].IBTransaction) then
    if RegisteredDatabases[FDBIndex].IBTransaction.InTransaction then
      RegisteredDatabases[FDBIndex].IBTransaction.CommitRetaining;
  try
    sgIndices.RowCount:= 1;

    // Get primary key index name
    PKeyName := GetPrimaryKeyIndexNameIsolated(RegisteredDatabases[FDBIndex].IBDatabase, FTableName, TmpConstraintName);

    ConstraintName:= TmpConstraintName;

    // Index names
    IsoIndices := GetIndicesIsolated(RegisteredDatabases[FDBIndex].IBDatabase,  FTableName);
    if IsoIndices.Query.RecordCount > 0 then
    while not IsoIndices.Query.EOF do
    begin
      if Trim(IsoIndices.Query.FieldByName('RDB$Index_name').AsString) = PKeyName then
      begin
        sgIndices.InsertColRow(False, 1);
        CurrentRow:= 1;
      end
      else
      begin
        sgIndices.RowCount:= sgIndices.RowCount + 1;
        CurrentRow:= sgIndices.RowCount - 1;
      end;
      sgIndices.Cells[0, CurrentRow]:= Trim(IsoIndices.Query.FieldByName('RDB$Index_Name').AsString);
      if IsoIndices.Query.FieldByName('RDB$Unique_Flag').AsString = '1' then
        sgIndices.Cells[1, CurrentRow]:= '1'
      else
        sgIndices.Cells[1, CurrentRow]:= '0';

      if IsoIndices.Query.FieldByName('RDB$Index_Type').AsString = '1' then
        sgIndices.Cells[2, CurrentRow]:= 'Desc'
      else
        sgIndices.Cells[2, CurrentRow]:= 'Asc';

      if Trim(IsoIndices.Query.FieldByName('RDB$Index_Name').AsString) = PKeyName then
        sgIndices.Cells[4, CurrentRow]:= '1'
      else
        sgIndices.Cells[4, CurrentRow]:= '0';
      IsoIndices.Query.Next;
    end;

    FieldsList:= TStringList.Create;
    try
      // Index fields
      for i:= 1 to sgIndices.RowCount - 1 do
      begin
        IndexFields:= '';
        IsoIndexFields := GetIndexFieldsIsolated(RegisteredDatabases[FDBIndex].IBDatabase, sgIndices.Cells[0, i], FTableName, FieldsList);
        if IsoIndexFields.Query.RecordCount > 0 then
         begin
          IndexFields:= FieldsList.CommaText;
          sgIndices.Cells[3, i]:= IndexFields;
        end;
        IsoIndexFields.Free;
      end;
    finally
      FieldsList.Free;
    end;

    edIndexName.Text:= 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);

    IsoFields := GetFieldsIsolated(RegisteredDatabases[FDBIndex].IBDatabase, FTableName);
    with IsoFields.Query do
    begin
      clbFields.Clear;
      while not EOF do
      begin
        // Allow creating indexes on any field except blobs
        if (FieldByName('field_type_int').AsInteger <> BlobType) then
          clbFields.Items.Add(FieldByName('Field_Name').AsString);
        Next;
      end;
    end;
  finally
    IsoFields.Free;
    IsoIndices.Free;
  end;

  if sgIndices.RowCount > 1 then
    sgIndices.Row:= 1;
end;



procedure TfmTableManage.FillTriggers;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text:= Format('SELECT RDB$Trigger_Name, RDB$Trigger_Inactive FROM RDB$TRIGGERS WHERE RDB$SYSTEM_FLAG=0 ' +
    'and RDB$Relation_Name = ''%s'' ',[FTableName]);
  if not SQLQuery1.Database.Connected then
    SQLQuery1.Database.Connected := true;
  if not SQLQuery1.Transaction.InTransaction then
    SQLQuery1.Transaction.StartTransaction;
  SQLQuery1.Open;
  sgTriggers.RowCount:= 1;
  with sgTriggers, SQLQuery1 do
  while not EOF do
  begin
    RowCount:= RowCount + 1;
    Cells[0, RowCount - 1]:= SQLQuery1.Fields[0].AsString;
    if SQLQuery1.Fields[1].AsString = '1' then
      Cells[1, RowCount - 1]:= '0'
    else
      Cells[1, RowCount - 1]:= '1';
    Next;
  end;
  SQLQuery1.Close;
end;

procedure TfmTableManage.FillPermissions;
var
  UsersList: TStringList;
  i: Integer;
  UserName: string;
  ObjType: Integer;
  Permissions: string;
begin
  {todo: analyse transaction behaviour. Why do we have an explicit commit here? Also,
  get rid of the implicit rollbacks and use a separate read only transaction for extracting
  DDL and other read only info}
  if CurrentIBTransaction.InTransaction then
    CurrentIBTransaction.Commit;
  UsersList:= TStringList.Create;
  try
    UsersList.CommaText:= dmSysTables.GetDBUsers(FDBIndex, FTableName);
    sgPermissions.RowCount:= UsersList.Count + 1;
    for i:= 0 to UsersList.Count - 1 do
    begin
      UserName:= UsersList[i];
      if Pos('<R>', UserName) = 1 then
        begin
          sgPermissions.Cells[1, i + 1]:= 'Role';
          Delete(UserName, 1, 3);
        end
      else
        sgPermissions.Cells[1, i + 1]:= 'User';

      sgPermissions.Cells[0, i + 1]:= UserName;

      // Permissions
      Permissions:= dmSysTables.GetObjectUserPermission(FDBIndex, FTableName, UserName, ObjType);

      if Pos('S', Permissions) > 0 then
        sgPermissions.Cells[2, i + 1]:= '1'
      else
        sgPermissions.Cells[2, i + 1]:= '0';

      if Pos('I', Permissions) > 0 then
        sgPermissions.Cells[3, i + 1]:= '1'
      else
        sgPermissions.Cells[3, i + 1]:= '0';

      if Pos('U', Permissions) > 0 then
        sgPermissions.Cells[4, i + 1]:= '1'
      else
        sgPermissions.Cells[4, i + 1]:= '0';

      if Pos('D', Permissions) > 0 then
        sgPermissions.Cells[5, i + 1]:= '1'
      else
        sgPermissions.Cells[5, i + 1]:= '0';

      if Pos('R', Permissions) > 0 then
        sgPermissions.Cells[6, i + 1]:= '1'
      else
        sgPermissions.Cells[6, i + 1]:= '0';

      if Pos('SG', Permissions) > 0 then
        sgPermissions.Cells[7, i + 1]:= '1'
      else
        sgPermissions.Cells[7, i + 1]:= '0';

      if Pos('IG', Permissions) > 0 then
        sgPermissions.Cells[8, i + 1]:= '1'
      else
        sgPermissions.Cells[8, i + 1]:= '0';

      if Pos('UG', Permissions) > 0 then
        sgPermissions.Cells[9, i + 1]:= '1'
      else
        sgPermissions.Cells[9, i + 1]:= '0';

      if Pos('DG', Permissions) > 0 then
        sgPermissions.Cells[10, i + 1]:= '1'
      else
        sgPermissions.Cells[10, i + 1]:= '0';

      if Pos('RG', Permissions) > 0 then
        sgPermissions.Cells[11, i + 1]:= '1'
      else
        sgPermissions.Cells[11, i + 1]:= '0';
    end;
  finally
    UsersList.Free;
  end;
end;

procedure TfmTableManage.FillFields;
var
  FieldSize: integer;
  FieldType: ansistring;
  CleanTypeName: string;
  i: Integer;
  PKFieldsList: TStringList;
  DefaultValue: string;
  PKIndexName: string;
  TmpConstraintName: ansistring;
  TmpInt: integer;
  IsUUID: boolean;
  Iso: TIsolatedQuery;
begin
  try
    sgFields.RowCount:= 1;
    Iso := GetFieldsIsolated(RegisteredDatabases[FDBIndex].IBDatabase, FTableName);
    with sgFields, Iso.Query do
    while not EOF do
    begin
      RowCount:= RowCount + 1;

      // Field Name
      Cells[1, RowCount - 1]:= Trim(FieldByName('Field_Name').AsString);

      // Field Type
      GetFieldType(Iso.Query, FieldType, FieldSize);
      Cells[2, RowCount - 1]:= FieldType;

      CleanTypeName := GetNameFromSizedTypeName(FieldType);

      IsUUID := (CleanTypeName = 'CHAR') and  (FieldByName('Field_Length').AsInteger = 16)
        and (Trim(UpperCase(FieldByName('Field_Charset').AsString)) = 'OCTETS');

      If isUUID then
      begin
        FieldType := 'UUID';
        Cells[7, RowCount - 1] := '';  //collation   ignore...
      end;

        // Computed fields (Calculated)
      if Iso.Query.FieldByName('computed_source').AsString <> '' then
        Cells[2, RowCount - 1]:= Iso.Query.FieldByName('computed_source').AsString;

      // Field Size
      if Iso.Query.FieldByName('field_type_int').AsInteger in [CharType,CStringType,VarCharType] then
        Cells[3, RowCount - 1]:= Iso.Query.FieldByName('CharacterLength').AsString
      else // why show byte size for numerical fields like integer fields?
        Cells[3, RowCount - 1]:= Iso.Query.FieldByName('Field_Length').AsString;

      if (CleanTypeName = 'DECIMAL') or (CleanTypeName = 'NUMERIC') then
      begin
        Cells[4, RowCount - 1]:= Iso.Query.FieldByName('field_precision').AsString;
        TmpInt := Abs(Iso.Query.FieldByName('field_scale').AsInteger);
        Cells[5, RowCount - 1]:= IntToStr(TmpInt);
      end;

      if ((CleanTypeName = 'CHAR') or (CleanTypeName = 'VARCHAR') or (CleanTypeName = 'UUID'))  then
      begin
        Cells[6, RowCount - 1]:= Iso.Query.FieldByName('field_charset').AsString;
      end;

      if ((CleanTypeName = 'CHAR') or (CleanTypeName = 'VARCHAR')) and (not IsUUID) then
        Cells[7, RowCount - 1]:= Iso.Query.FieldByName('field_collation').AsString;

      // Null/Not null
      if Iso.Query.FieldByName('field_not_null_constraint').AsString = '1' then
        Cells[8, RowCount - 1]:= '0'
      else
        Cells[8, RowCount - 1]:= '1';

      // Default Value
      DefaultValue := Iso.Query.FieldByName('Field_Default_Source').AsString;
      Cells[9, RowCount - 1] := ExtractDefaultValue(DefaultValue);

      Cells[10, RowCount - 1]:= Iso.Query.FieldByName('Field_Description').AsString;
      Next;
    end;
    Iso.Free;
    // Primary Keys
    PKFieldsList:= TStringList.Create;
    try
      PKIndexName := GetPrimaryKeyIndexNameIsolated(RegisteredDatabases[FDBIndex].IBDatabase, FTableName, TmpConstraintName);
      ConstraintName := TmpConstraintName;

      if PKIndexName <> '' then
        fmMain.GetConstraintFields(FTableName, PKIndexName, PKFieldsList);

      with sgFields do
      for i:= 1 to RowCount - 1 do
        if PKFieldsList.IndexOf(Cells[1, i]) <> -1 then
          Cells[0, i]:= '1'
        else
          Cells[0, i]:= '0';
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

