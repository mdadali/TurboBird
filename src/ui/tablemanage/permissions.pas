unit Permissions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, ComCtrls, StdCtrls,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,

  uthemeselector,

  PermissionManage;

type

  { TfmTablePermissions }

  TfmTablePermissions = class(TForm)
  PageControl1: TPageControl;
  tsOverview: TTabSheet;
  bbRefreshPermissions: TBitBtn;
  bbEditPermission: TBitBtn;
  bbAddUser: TBitBtn;
  sgPermissions: TStringGrid;

procedure bbRefreshClick(Sender: TObject);
procedure bbAddUserClick(Sender: TObject);
procedure bbEditPermissionClick(Sender: TObject);
procedure sgPermissionsDblClick(Sender: TObject);
procedure FormShow(Sender: TObject);

private
FDBIndex: Integer;
FTableName: string;
FNodeInfos: TPNodeInfos;
FExtractor: TSimpleObjExtractor;

    /// Sucht einen Sub-Tab anhand des Captions (case-insensitive).
    /// Gibt nil zurück, wenn keiner existiert.
function FindSubTabByCaption(const ACaption: string): TTabSheet;

    /// Erzeugt einen neuen Sub-Tab ODER aktiviert einen bereits
    /// existierenden mit gleichem Caption.
    /// Rückgabe:
    ///   True  = neuer Tab erstellt → Caller muss Init+Show aufrufen
    ///   False = Tab existierte bereits → Form wurde gefreed, Caller: Exit
function CreatePermissionSubTab(
      var APermissionsForm: TfmPermissionManage;
      const ATitle, AAction, AUserOrRole, ATypeStr: string): Boolean;

public
procedure Init(ADBIndex: Integer; const ATableName: string;
               ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
procedure FillPermissions;
end;


implementation

{$R *.lfm}

uses Main, QueryWindow;

{ TfmTablePermissions }

procedure TfmTablePermissions.Init(ADBIndex: Integer; const ATableName: string;
                                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Permissions: ' + FTableName;
end;

procedure TfmTablePermissions.FillPermissions;
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

// ============================================================
// Sucht einen Sub-Tab anhand des Captions (case-insensitive)
// ============================================================
function TfmTablePermissions.FindSubTabByCaption(
  const ACaption: string): TTabSheet;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to PageControl1.PageCount - 1 do
    if SameText(PageControl1.Pages[i].Caption, ACaption) then
    begin
      Result := PageControl1.Pages[i];
      Exit;
    end;
end;

// ============================================================
// Erzeugt einen neuen Sub-Tab ODER aktiviert existierenden.
//
// Rückgabe:
//   True  = neuer Tab erstellt → Caller muss Init+Show aufrufen
//   False = Tab existierte bereits → APermissionsForm wurde
//           gefreed (auf nil gesetzt), Caller soll Exit machen
// ============================================================
function TfmTablePermissions.CreatePermissionSubTab(
  var APermissionsForm: TfmPermissionManage;
  const ATitle, AAction, AUserOrRole, ATypeStr: string): Boolean;
var
  ATab: TTabSheet;
  FullHint, DBAlias: string;
begin
  // ------------------------------------------------------------
  // 1. Existiert schon ein Tab mit gleichem Caption?
  // ------------------------------------------------------------
  ATab := FindSubTabByCaption(ATitle);
  if Assigned(ATab) then
  begin
    // Tab existiert bereits → aktivieren, neue Form verwerfen
    PageControl1.ActivePage := ATab;
    FreeAndNil(APermissionsForm);
    Result := False;
    Exit;
  end;

  // ------------------------------------------------------------
  // 2. Neuen Tab erstellen
  // ------------------------------------------------------------
  ATab := TTabSheet.Create(PageControl1);
  ATab.Parent := PageControl1;
  if Assigned(FNodeInfos) then
    ATab.ImageIndex := TPNodeInfos(FNodeInfos)^.ImageIndex;

  APermissionsForm.Parent := ATab;
  APermissionsForm.Align := alClient;
  APermissionsForm.BorderStyle := bsNone;

  PageControl1.ActivePage := ATab;
  ATab.Tag := FDBIndex;

  ATab.Caption := ATitle;
  APermissionsForm.Caption := ATitle;

  // Hint zusammenbauen
  if Assigned(FNodeInfos) and Assigned(FNodeInfos^.OwnerNode) then
  begin
    DBAlias := GetAncestorNodeText(FNodeInfos^.OwnerNode, 1);
    FullHint :=
      'Server:   ' + GetAncestorNodeText(FNodeInfos^.OwnerNode, 0) + sLineBreak +
      'DBAlias:  ' + DBAlias + sLineBreak +
      'DBPath:   ' + RegisteredDatabases[FDBIndex].IBDatabase.DatabaseName + sLineBreak +
      'Object type: Table Permissions' + sLineBreak +
      'Table: ' + FTableName + sLineBreak;

    if AUserOrRole <> '' then
      FullHint := FullHint + 'Granted to: ' + AUserOrRole + sLineBreak +
                              'Type: ' + ATypeStr + sLineBreak;

    FullHint := FullHint + AAction;

    ATab.Hint := FullHint;
    ATab.ShowHint := True;
  end;

  Result := True;
end;

// ============================================================
// Add User / Role → neuer Sub-Tab ODER existierenden aktivieren
// ============================================================
procedure TfmTablePermissions.bbAddUserClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  Title: string;
begin
  if not Assigned(FNodeInfos) then
    Exit;

  Title := 'Add User: ' + FTableName;

  fmPermissions := TfmPermissionManage.Create(Application);

  if not CreatePermissionSubTab(fmPermissions, Title, 'Action: Add User', '', '') then
    Exit;   // Tab existierte bereits → fmPermissions wurde gefreed

  fmPermissions.Init(FNodeInfos, FDBIndex, FTableName, '', 1,
                     FExtractor, @bbRefreshClick);
  fmPermissions.Show;
end;

// ============================================================
// Edit Permission → neuer Sub-Tab ODER existierenden aktivieren
// ============================================================
procedure TfmTablePermissions.bbEditPermissionClick(Sender: TObject);
var
  fmPermissions: TfmPermissionManage;
  UserType: Integer;
  UserOrRole, Title, TypeStr: string;
begin
  if sgPermissions.Row <= 0 then
  begin
    ShowMessage('There is no selected user/role');
    Exit;
  end;

  if not Assigned(FNodeInfos) then
    Exit;

  if sgPermissions.Cells[1, sgPermissions.Row] = 'User' then
  begin
    UserType := 1;
    TypeStr := 'User';
  end
  else
  begin
    UserType := 2;
    TypeStr := 'Role';
  end;
  UserOrRole := sgPermissions.Cells[0, sgPermissions.Row];

  Title := 'Permissions: ' + FTableName + ': ' + UserOrRole;

  fmPermissions := TfmPermissionManage.Create(Application);

  if not CreatePermissionSubTab(fmPermissions, Title,
                                'Action: Edit Permissions', UserOrRole, TypeStr) then
    Exit;   // Tab existierte bereits → fmPermissions wurde gefreed

  fmPermissions.Init(FNodeInfos, FDBIndex, FTableName, UserOrRole, UserType,
                     FExtractor, @bbRefreshClick);
  fmPermissions.Show;
end;

procedure TfmTablePermissions.bbRefreshClick(Sender: TObject);
begin
  FillPermissions;
end;

procedure TfmTablePermissions.sgPermissionsDblClick(Sender: TObject);
begin
  bbEditPermissionClick(Sender);
end;

procedure TfmTablePermissions.FormShow(Sender: TObject);
begin
  FillPermissions;
  frmThemeSelector.btnApplyClick(Self);
end;

end.
