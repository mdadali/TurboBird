unit PermissionManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Buttons, CheckLst, ExtCtrls,
  fbcommon,
  turbocommon,
  fsimpleobjextractor,
  uthemeselector;

type

  { TfmPermissionManage }

  TfmPermissionManage = class(TForm)
    bbApplyRoles: TBitBtn;
    bbApplyTable: TBitBtn;
    bbApplyProc: TBitBtn;
    bbApplyView: TBitBtn;
    bbClose: TSpeedButton;
    BitBtn1: TBitBtn;
    cbRolesUser: TComboBox;
    cbTables: TComboBox;
    cbViews: TComboBox;
    cbUsers: TComboBox;
    cbProcUsers: TComboBox;
    cbViewsUsers: TComboBox;
    cxViewAll: TCheckBox;
    cxViewAllGrant: TCheckBox;
    cxViewDelete: TCheckBox;
    cxViewDeleteGrant: TCheckBox;
    cxViewInsert: TCheckBox;
    cxViewInsertGrant: TCheckBox;
    cxProcGrant: TCheckBox;
    clbProcedures: TCheckListBox;
    clbRoles: TCheckListBox;
    cxViewReferences: TCheckBox;
    cxViewReferencesGrant: TCheckBox;
    cxRoleGrant: TCheckBox;
    cxSelect: TCheckBox;
    cxInsert: TCheckBox;
    cxDelete: TCheckBox;
    cxReferences: TCheckBox;
    cxAll: TCheckBox;
    cxViewSelect: TCheckBox;
    cxSelectGrant: TCheckBox;
    cxInsertGrant: TCheckBox;
    cxAllGrant: TCheckBox;
    cxViewSelectGrant: TCheckBox;
    cxViewUpdate: TCheckBox;
    cxUpdateGrant: TCheckBox;
    cxDeleteGrant: TCheckBox;
    cxReferencesGrant: TCheckBox;
    cxUpdate: TCheckBox;
    cxViewUpdateGrant: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsViews: TTabSheet;
    tsRoles: TTabSheet;
    tsProcedures: TTabSheet;
    tsTables: TTabSheet;
    procedure bbApplyProcClick(Sender: TObject);
    procedure bbApplyRolesClick(Sender: TObject);
    procedure bbApplyTableClick(Sender: TObject);
    procedure bbApplyViewClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure cbProcUsersChange(Sender: TObject);
    procedure cbRolesUserChange(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure cbViewsChange(Sender: TObject);
    procedure cbViewsUsersChange(Sender: TObject);
    procedure clbProceduresClick(Sender: TObject);
    procedure clbProceduresKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure clbRolesClick(Sender: TObject);
    procedure clbRolesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxProcGrantChange(Sender: TObject);
    procedure cxRoleGrantChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: Integer;
    FExtractor: TSimpleObjExtractor;
    FOwnsExtractor: Boolean;

    FProcList: TStringList;         // Objekte, auf die User Rechte hat
    FRoleList: TStringList;         // Rollen, die User hat
    FProcGrant: array of Boolean;   // Für jede Procedure: with grant?
    FOrigProcGrant: array of Boolean;
    FRoleGrant: array of Boolean;
    FOrigRoleGrant: array of Boolean;
    FOnCommitProcedure: TNotifyEvent;

    FOldTableSelectGrant: Boolean;
    FOldTableInsertGrant: Boolean;
    FOldTableUpdateGrant: Boolean;
    FOldTableDeleteGrant: Boolean;
    FOldTableReferencesGrant: Boolean;

    procedure UpdatePermissions;
    procedure UpdateViewsPermissions;
    procedure UpdateProcPermissions;
    procedure UpdateRolePermissions;
    procedure ComposeTablePermissionSQL(ATableName: string; OptionName: string;
      Grant, WithGrant: Boolean; var List: TStringList);
  public
    procedure Init(ANodeInfos: TPNodeInfos; dbIndex: Integer;
      ATableName, AUserName: string; UserType: Integer;
      AExtractor: TSimpleObjExtractor;
      OnCommitProcedure: TNotifyEvent = nil);
  end;

implementation

{ TfmPermissionManage }

uses main;

procedure TfmPermissionManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Nur freigeben, wenn wir ihn selbst erzeugt haben
  if FOwnsExtractor and Assigned(FExtractor) then
    FreeAndNil(FExtractor);

  if Assigned(FNodeInfos) then
    FNodeInfos^.EditorForm := nil;

  SetLength(FProcGrant, 0);
  SetLength(FOrigProcGrant, 0);
  SetLength(FRoleGrant, 0);
  SetLength(FOrigRoleGrant, 0);
  FProcList.Free;
  FRoleList.Free;
  CloseAction := caFree;
end;

procedure TfmPermissionManage.FormCreate(Sender: TObject);
begin
  FProcList := TStringList.Create;
  FRoleList := TStringList.Create;
end;

procedure TfmPermissionManage.FormDestroy(Sender: TObject);
begin
end;

procedure TfmPermissionManage.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

// ============================================================
// Tabellen-Tab: aktuelle Rechte des Users auf die gewählte Tabelle
// ============================================================
procedure TfmPermissionManage.UpdatePermissions;
var
  Perms: TFBPermissionArray;
  i: Integer;
  Priv: string;
  Found: Boolean;
  TargetUser: string;
begin
  if (cbUsers.Text = '') or (cbTables.Text = '') then
    Exit;

  TargetUser := StripQuotes(cbUsers.Text);

  // Alle Rechte auf diese Tabelle holen
  Perms := FExtractor.GetObjectPermissions(cbTables.Text);

  Priv := '';
  Found := False;
  for i := 0 to High(Perms) do
  begin
    if SameText(Perms[i].UserName, TargetUser) then
    begin
      Priv := Perms[i].Privileges;
      Found := True;
      Break;
    end;
  end;

  // Checkboxen zurücksetzen
  cxAll.Checked := False;
  cxAllGrant.Checked := False;
  cxSelect.Checked := False;
  cxInsert.Checked := False;
  cxUpdate.Checked := False;
  cxDelete.Checked := False;
  cxReferences.Checked := False;
  cxSelectGrant.Checked := False;
  cxInsertGrant.Checked := False;
  cxUpdateGrant.Checked := False;
  cxDeleteGrant.Checked := False;
  cxReferencesGrant.Checked := False;

  if Found then
  begin
    cxSelect.Checked     := PermissionHas(Priv, 'S');
    cxInsert.Checked     := PermissionHas(Priv, 'I');
    cxUpdate.Checked     := PermissionHas(Priv, 'U');
    cxDelete.Checked     := PermissionHas(Priv, 'D');
    cxReferences.Checked := PermissionHas(Priv, 'R');
    cxSelectGrant.Checked     := PermissionHas(Priv, 'SG');
    cxInsertGrant.Checked     := PermissionHas(Priv, 'IG');
    cxUpdateGrant.Checked     := PermissionHas(Priv, 'UG');
    cxDeleteGrant.Checked     := PermissionHas(Priv, 'DG');
    cxReferencesGrant.Checked := PermissionHas(Priv, 'RG');

    FOldTableSelectGrant     := cxSelectGrant.Checked;
    FOldTableInsertGrant     := cxInsertGrant.Checked;
    FOldTableUpdateGrant     := cxUpdateGrant.Checked;
    FOldTableDeleteGrant     := cxDeleteGrant.Checked;
    FOldTableReferencesGrant := cxReferencesGrant.Checked;
  end;
end;

// ============================================================
// Views-Tab: aktuelle Rechte des Users auf den gewählten View
// ============================================================
procedure TfmPermissionManage.UpdateViewsPermissions;
var
  Perms: TFBPermissionArray;
  i: Integer;
  Priv: string;
  Found: Boolean;
  TargetUser: string;
begin
  if (cbViewsUsers.Text = '') or (cbViews.Text = '') then
    Exit;

  TargetUser := StripQuotes(cbViewsUsers.Text);
  Perms := FExtractor.GetObjectPermissions(cbViews.Text);

  Priv := '';
  Found := False;
  for i := 0 to High(Perms) do
  begin
    if SameText(Perms[i].UserName, TargetUser) then
    begin
      Priv := Perms[i].Privileges;
      Found := True;
      Break;
    end;
  end;

  cxViewAll.Checked := False;
  cxViewAllGrant.Checked := False;
  cxViewSelect.Checked := False;
  cxViewInsert.Checked := False;
  cxViewUpdate.Checked := False;
  cxViewDelete.Checked := False;
  cxViewReferences.Checked := False;
  cxViewSelectGrant.Checked := False;
  cxViewInsertGrant.Checked := False;
  cxViewUpdateGrant.Checked := False;
  cxViewDeleteGrant.Checked := False;
  cxViewReferencesGrant.Checked := False;

  if Found then
  begin
    cxViewSelect.Checked     := PermissionHas(Priv, 'S');
    cxViewInsert.Checked     := PermissionHas(Priv, 'I');
    cxViewUpdate.Checked     := PermissionHas(Priv, 'U');
    cxViewDelete.Checked     := PermissionHas(Priv, 'D');
    cxViewReferences.Checked := PermissionHas(Priv, 'R');
    cxViewSelectGrant.Checked     := PermissionHas(Priv, 'SG');
    cxViewInsertGrant.Checked     := PermissionHas(Priv, 'IG');
    cxViewUpdateGrant.Checked     := PermissionHas(Priv, 'UG');
    cxViewDeleteGrant.Checked     := PermissionHas(Priv, 'DG');
    cxViewReferencesGrant.Checked := PermissionHas(Priv, 'RG');
  end;
end;

// ============================================================
// Procedures-Tab: ALLE Procedures anzeigen, die mit Rechten markieren
// ============================================================
procedure TfmPermissionManage.UpdateProcPermissions;
var
  AllProcs: TStringList;
  Grants: TFBUserGrantArray;
  i, idx: Integer;
  TargetUser: string;
begin
  clbProcedures.Items.Clear;
  SetLength(FProcGrant, 0);
  SetLength(FOrigProcGrant, 0);
  FProcList.Clear;

  if cbProcUsers.Text = '' then
    Exit;

  TargetUser := StripQuotes(cbProcUsers.Text);

  // 1. Alle Procedures laden (Option B)
  AllProcs := TStringList.Create;
  try
    FExtractor.ExtractObjectNames(FDBIndex, otProcedures, false,
      TStrings(AllProcs), '');

    for i := 0 to AllProcs.Count - 1 do
      clbProcedures.Items.Add(AllProcs[i]);
  finally
    AllProcs.Free;
  end;

  SetLength(FProcGrant, clbProcedures.Count);
  SetLength(FOrigProcGrant, clbProcedures.Count);

  // 2. Grants des Users holen
  Grants := FExtractor.GetUserObjectGrants(TargetUser, 5);

  // 3. Markieren
  for i := 0 to High(Grants) do
  begin
    idx := clbProcedures.Items.IndexOf(Grants[i].ObjectName);
    if idx >= 0 then
    begin
      clbProcedures.Checked[idx] := True;
      FProcList.Add(Grants[i].ObjectName);
      FProcGrant[idx] := Grants[i].WithGrant;
      FOrigProcGrant[idx] := Grants[i].WithGrant;
    end;
  end;
end;

// ============================================================
// Roles-Tab: alle Rollen anzeigen, die mit Grants markieren
// ============================================================
procedure TfmPermissionManage.UpdateRolePermissions;
var
  AllRoles: TStringList;
  Grants: TFBUserGrantArray;
  i, idx: Integer;
  TargetUser: string;
begin
  clbRoles.Items.Clear;
  SetLength(FRoleGrant, 0);
  SetLength(FOrigRoleGrant, 0);
  FRoleList.Clear;

  if cbRolesUser.Text = '' then
    Exit;

  TargetUser := StripQuotes(cbRolesUser.Text);

  AllRoles := TStringList.Create;
  try
    FExtractor.ExtractObjectNames(FDBIndex, otRoles, false,
      TStrings(AllRoles), '');

    for i := 0 to AllRoles.Count - 1 do
      clbRoles.Items.Add(AllRoles[i]);
  finally
    AllRoles.Free;
  end;

  SetLength(FRoleGrant, clbRoles.Count);
  SetLength(FOrigRoleGrant, clbRoles.Count);

  Grants := FExtractor.GetUserObjectGrants(TargetUser, 13);

  for i := 0 to High(Grants) do
  begin
    idx := clbRoles.Items.IndexOf(Grants[i].ObjectName);
    if idx >= 0 then
    begin
      clbRoles.Checked[idx] := True;
      FRoleList.Add(Grants[i].ObjectName);
      FRoleGrant[idx] := Grants[i].WithGrant;
      FOrigRoleGrant[idx] := Grants[i].WithGrant;
    end;
  end;
end;

// ============================================================
// SQL-Zusammenbau für Tabellen/Views
// ============================================================
procedure TfmPermissionManage.ComposeTablePermissionSQL(
  ATableName: string; OptionName: string;
  Grant, WithGrant: Boolean; var List: TStringList);
var
  Line: string;
  ToFrom: string;
  Command: string;
  QuotedUser: string;
  QuotedTable: string;
begin
  if Grant then
  begin
    ToFrom := ' to ';
    Command := 'grant ';
  end
  else
  begin
    ToFrom := ' from ';
    Command := 'revoke ';
  end;

  // Case-sensitivity: User und Table auto-quoten
  QuotedUser  := MakeCaseSensitiveAuto(cbUsers.Text);
  QuotedTable := MakeCaseSensitiveAuto(ATableName);

  Line := Command + OptionName + ' on ' + QuotedTable + ToFrom + QuotedUser;
  if Grant and WithGrant then
    Line := Line + ' with grant option';
  Line := Line + ';';

  if Grant and (not WithGrant) then
  begin
    if FOldTableSelectGrant and (not cxSelectGrant.Checked) and
       (LowerCase(OptionName) = 'select') then
      Line := Line + LineEnding + 'REVOKE GRANT OPTION FOR SELECT ON ' +
              QuotedTable + ' FROM ' + QuotedUser + ';';

    if FOldTableUpdateGrant and (not cxUpdateGrant.Checked) and
       (LowerCase(OptionName) = 'update') then
      Line := Line + LineEnding + 'REVOKE GRANT OPTION FOR UPDATE ON ' +
              QuotedTable + ' FROM ' + QuotedUser + ';';

    if FOldTableReferencesGrant and (not cxReferencesGrant.Checked) and
       (LowerCase(OptionName) = 'references') then
      Line := Line + LineEnding + 'REVOKE GRANT OPTION FOR REFERENCES ON ' +
              QuotedTable + ' FROM ' + QuotedUser + ';';

    if FOldTableDeleteGrant and (not cxDeleteGrant.Checked) and
       (LowerCase(OptionName) = 'delete') then
      Line := Line + LineEnding + 'REVOKE GRANT OPTION FOR DELETE ON ' +
              QuotedTable + ' FROM ' + QuotedUser + ';';

    if FOldTableInsertGrant and (not cxInsertGrant.Checked) and
       (LowerCase(OptionName) = 'insert') then
      Line := Line + LineEnding + 'REVOKE GRANT OPTION FOR INSERT ON ' +
              QuotedTable + ' FROM ' + QuotedUser + ';';
  end;

  List.Add(Line);
end;

procedure TfmPermissionManage.bbApplyTableClick(Sender: TObject);
var
  List: TStringList;
  TableName: string;
begin
  if (cbUsers.Text = '') or (cbTables.ItemIndex = -1) then
  begin
    ShowMessage('You should enter user/role and a table');
    Exit;
  end;

  TableName := cbTables.Text;  // schon ohne Quotes aus dem Extractor

  List := TStringList.Create;
  try
    if cxAll.Checked then
      ComposeTablePermissionSQL(TableName, 'All', cxAll.Checked, cxAllGrant.Checked, List)
    else
    begin
      ComposeTablePermissionSQL(TableName, 'Select', cxSelect.Checked, cxSelectGrant.Checked, List);
      ComposeTablePermissionSQL(TableName, 'Insert', cxInsert.Checked, cxInsertGrant.Checked, List);
      ComposeTablePermissionSQL(TableName, 'Update', cxUpdate.Checked, cxUpdateGrant.Checked, List);
      ComposeTablePermissionSQL(TableName, 'Delete', cxDelete.Checked, cxDeleteGrant.Checked, List);
      ComposeTablePermissionSQL(TableName, 'References', cxReferences.Checked, cxReferencesGrant.Checked, List);
    end;

    fmMain.ShowCompleteQueryWindow(FDBIndex,
      'Edit Permission for: ' + TableName, List.Text, FOnCommitProcedure);
  finally
    List.Free;
  end;

  Close;
  Parent.Free;
end;

procedure TfmPermissionManage.bbApplyViewClick(Sender: TObject);
var
  List: TStringList;
  ViewName: string;
begin
  if (cbViewsUsers.Text = '') or (cbViews.ItemIndex = -1) then
  begin
    ShowMessage('You should enter user/role and a view');
    Exit;
  end;

  ViewName := cbViews.Text;

  List := TStringList.Create;
  try
    if cxViewAll.Checked then
      ComposeTablePermissionSQL(ViewName, 'All', cxViewAll.Checked, cxViewAllGrant.Checked, List)
    else
    begin
      ComposeTablePermissionSQL(ViewName, 'Select', cxViewSelect.Checked, cxViewSelectGrant.Checked, List);
      ComposeTablePermissionSQL(ViewName, 'Insert', cxViewInsert.Checked, cxViewInsertGrant.Checked, List);
      ComposeTablePermissionSQL(ViewName, 'Update', cxViewUpdate.Checked, cxViewUpdateGrant.Checked, List);
      ComposeTablePermissionSQL(ViewName, 'Delete', cxViewDelete.Checked, cxViewDeleteGrant.Checked, List);
      ComposeTablePermissionSQL(ViewName, 'References', cxViewReferences.Checked, cxViewReferencesGrant.Checked, List);
    end;

    fmMain.ShowCompleteQueryWindow(FDBIndex,
      'Edit Permission for: ' + ViewName, List.Text, FOnCommitProcedure);
  finally
    List.Free;
  end;

  Close;
  Parent.Free;
end;

procedure TfmPermissionManage.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmPermissionManage.BitBtn1Click(Sender: TObject);
begin
  UpdateRolePermissions;
end;

procedure TfmPermissionManage.bbApplyProcClick(Sender: TObject);
var
  List: TStringList;
  i: Integer;
  Line: string;
  ProcName: string;
  QuotedUser: string;
  ProcHas: Boolean;
  NewGrant, NewGrantOpt: Boolean;
begin
  if Trim(cbProcUsers.Text) = '' then
    Exit;

  QuotedUser := MakeCaseSensitiveAuto(cbProcUsers.Text);

  List := TStringList.Create;
  try
    for i := 0 to clbProcedures.Items.Count - 1 do
    begin
      ProcName := clbProcedures.Items[i];
      ProcHas := FProcList.IndexOf(ProcName) <> -1;

      NewGrant := clbProcedures.Checked[i];
      NewGrantOpt := FProcGrant[i];

      if NewGrant and (not ProcHas) then
      begin
        // Neuer Grant
        Line := 'GRANT EXECUTE ON PROCEDURE ' + MakeCaseSensitiveAuto(ProcName) +
                ' TO ' + QuotedUser;
        if NewGrantOpt then
          Line := Line + ' WITH GRANT OPTION';
        List.Add(Line + ';');
      end
      else if NewGrant and ProcHas and (NewGrantOpt <> FOrigProcGrant[i]) then
      begin
        // Grant-Option geändert
        if NewGrantOpt then
          // Grant-Option hinzufügen
          List.Add('GRANT EXECUTE ON PROCEDURE ' + MakeCaseSensitiveAuto(ProcName) +
                   ' TO ' + QuotedUser + ' WITH GRANT OPTION;')
        else
          // Grant-Option entfernen
          List.Add('REVOKE GRANT OPTION FOR EXECUTE ON PROCEDURE ' +
                   MakeCaseSensitiveAuto(ProcName) +
                   ' FROM ' + QuotedUser + ';');
      end
      else if (not NewGrant) and ProcHas then
        // Grant entfernen
        List.Add('REVOKE EXECUTE ON PROCEDURE ' + MakeCaseSensitiveAuto(ProcName) +
                 ' FROM ' + QuotedUser + ';');
    end;

    if List.Count > 0 then
    begin
      fmMain.ShowCompleteQueryWindow(FDBIndex,
        'Edit Permission for: ' + cbProcUsers.Text, List.Text, FOnCommitProcedure);
      Close;
      Parent.Free;
    end
    else
      ShowMessage('There is no change');
  finally
    List.Free;
  end;
end;

procedure TfmPermissionManage.bbApplyRolesClick(Sender: TObject);
var
  List: TStringList;
  i: Integer;
  Line: string;
  RoleName: string;
  QuotedUser: string;
  RoleHas, NewGrant, NewGrantOpt: Boolean;
begin
  if Trim(cbRolesUser.Text) = '' then
    Exit;

  QuotedUser := MakeCaseSensitiveAuto(cbRolesUser.Text);

  List := TStringList.Create;
  try
    for i := 0 to clbRoles.Items.Count - 1 do
    begin
      RoleName := clbRoles.Items[i];
      RoleHas := FRoleList.IndexOf(RoleName) <> -1;

      NewGrant := clbRoles.Checked[i];
      NewGrantOpt := FRoleGrant[i];

      if NewGrant and (not RoleHas) then
      begin
        // Neue Mitgliedschaft
        Line := 'GRANT ' + MakeCaseSensitiveAuto(RoleName) + ' TO ' + QuotedUser;
        if NewGrantOpt then
          Line := Line + ' WITH ADMIN OPTION';
        List.Add(Line + ';');
      end
      else if NewGrant and RoleHas and (NewGrantOpt <> FOrigRoleGrant[i]) then
      begin
        // Admin-Option geändert
        if NewGrantOpt then
          List.Add('GRANT ' + MakeCaseSensitiveAuto(RoleName) + ' TO ' + QuotedUser +
                   ' WITH ADMIN OPTION;')
        else
          List.Add('REVOKE ADMIN OPTION FOR ' + MakeCaseSensitiveAuto(RoleName) +
                   ' FROM ' + QuotedUser + ';');
      end
      else if (not NewGrant) and RoleHas then
        // Mitgliedschaft entfernen
        List.Add('REVOKE ' + MakeCaseSensitiveAuto(RoleName) +
                 ' FROM ' + QuotedUser + ';');
    end;

    if List.Count > 0 then
    begin
      fmMain.ShowCompleteQueryWindow(FDBIndex,
        'Edit Permission for: ' + cbRolesUser.Text, List.Text, FOnCommitProcedure);
      Close;
      Parent.Free;
    end
    else
      ShowMessage('There is no change');
  finally
    List.Free;
  end;
end;

procedure TfmPermissionManage.cbProcUsersChange(Sender: TObject);
begin
  UpdateProcPermissions;
end;

procedure TfmPermissionManage.cbRolesUserChange(Sender: TObject);
begin
  UpdateRolePermissions;
end;

procedure TfmPermissionManage.cbTablesChange(Sender: TObject);
begin
  UpdatePermissions;
end;

procedure TfmPermissionManage.cbViewsChange(Sender: TObject);
begin
  UpdateViewsPermissions;
end;

procedure TfmPermissionManage.cbViewsUsersChange(Sender: TObject);
begin
  UpdateViewsPermissions;
end;

procedure TfmPermissionManage.clbProceduresClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := clbProcedures.ItemIndex;
  if Index <> -1 then
  begin
    cxProcGrant.Checked := FProcGrant[Index];
    cxProcGrant.Caption := 'With Grant for ' + clbProcedures.Items[Index];
  end;
end;

procedure TfmPermissionManage.clbProceduresKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  clbProceduresClick(nil);
end;

procedure TfmPermissionManage.clbRolesClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := clbRoles.ItemIndex;
  if Index <> -1 then
  begin
    cxRoleGrant.Checked := FRoleGrant[Index];
    cxRoleGrant.Caption := 'With Admin for ' + clbRoles.Items[Index];
  end;
end;

procedure TfmPermissionManage.clbRolesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  clbRolesClick(nil);
end;

procedure TfmPermissionManage.cxProcGrantChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := clbProcedures.ItemIndex;
  if Index <> -1 then
    FProcGrant[Index] := cxProcGrant.Checked;
end;

procedure TfmPermissionManage.cxRoleGrantChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := clbRoles.ItemIndex;
  if Index <> -1 then
    FRoleGrant[Index] := cxRoleGrant.Checked;
end;

procedure TfmPermissionManage.Init(ANodeInfos: TPNodeInfos; dbIndex: integer;
  ATableName, AUserName: string; UserType: Integer;
  AExtractor: TSimpleObjExtractor;
  OnCommitProcedure: TNotifyEvent = nil);
var
  UsersList, RolesList: TStringList;
  i: Integer;
  TargetIdx: Integer;

  // Lokale Hilfsfunktion — case-insensitive Suche in ComboBox
  function FindComboItem(ACombo: TComboBox; const AName: string): Integer;
  var
    k: Integer;
  begin
    Result := -1;
    for k := 0 to ACombo.Items.Count - 1 do
      if SameText(ACombo.Items[k], AName) then
      begin
        Result := k;
        Exit;
      end;
  end;

begin
  FNodeInfos := ANodeInfos;
  FOnCommitProcedure := OnCommitProcedure;

  // ============================================================
  // Extractor setzen — vom Aufrufer oder Fallback
  // ============================================================
  FExtractor := AExtractor;
  FOwnsExtractor := False;

  if not Assigned(FExtractor) then
  begin
    FExtractor := TSimpleObjExtractor.Create(dbIndex);
    FOwnsExtractor := True;
  end;

  FDBIndex := dbIndex;

  // ============================================================
  // ComboBoxen leeren (wichtig bei Reuse!)
  // ============================================================
  cbUsers.Items.Clear;
  cbProcUsers.Items.Clear;
  cbViewsUsers.Items.Clear;
  cbRolesUser.Items.Clear;
  cbTables.Items.Clear;
  cbViews.Items.Clear;

  // ============================================================
  // ComboBoxen füllen
  // ============================================================
  UsersList := TStringList.Create;
  RolesList := TStringList.Create;
  try
    FExtractor.ExtractObjectNames(dbIndex, otUsers, false, TStrings(UsersList), '');
    FExtractor.ExtractObjectNames(dbIndex, otRoles, false, TStrings(RolesList), '');

    // cbUsers / cbProcUsers / cbViewsUsers: Users + Roles
    for i := 0 to UsersList.Count - 1 do
    begin
      cbUsers.Items.Add(UsersList[i]);
      cbProcUsers.Items.Add(UsersList[i]);
      cbViewsUsers.Items.Add(UsersList[i]);
    end;
    for i := 0 to RolesList.Count - 1 do
    begin
      cbUsers.Items.Add(RolesList[i]);
      cbProcUsers.Items.Add(RolesList[i]);
      cbViewsUsers.Items.Add(RolesList[i]);
    end;

    // cbRolesUser: nur Users
    for i := 0 to UsersList.Count - 1 do
      cbRolesUser.Items.Add(UsersList[i]);

    // Tabellen
    UsersList.Clear;
    FExtractor.ExtractObjectNames(dbIndex, otTables, false, TStrings(UsersList), '');
    cbTables.Items.AddStrings(UsersList);

    // Views
    UsersList.Clear;
    FExtractor.ExtractObjectNames(dbIndex, otViews, false, TStrings(UsersList), '');
    cbViews.Items.AddStrings(UsersList);
  finally
    UsersList.Free;
    RolesList.Free;
  end;

  // ============================================================
  // Vorauswahl — User/Rolle
  // ============================================================
  if AUserName <> '' then
  begin
    TargetIdx := FindComboItem(cbUsers, AUserName);
    if TargetIdx >= 0 then cbUsers.ItemIndex := TargetIdx;

    TargetIdx := FindComboItem(cbProcUsers, AUserName);
    if TargetIdx >= 0 then cbProcUsers.ItemIndex := TargetIdx;

    TargetIdx := FindComboItem(cbViewsUsers, AUserName);
    if TargetIdx >= 0 then cbViewsUsers.ItemIndex := TargetIdx;

    if UserType = 1 then
    begin
      TargetIdx := FindComboItem(cbRolesUser, AUserName);
      if TargetIdx >= 0 then cbRolesUser.ItemIndex := TargetIdx;
    end;
  end;

  // ============================================================
  // Vorauswahl — Tabelle/View
  // ============================================================
  if ATableName <> '' then
    cbTables.Text := ATableName
  else if cbTables.Items.Count > 0 then
    cbTables.ItemIndex := 0;

  if cbViews.Items.Count > 0 then
    cbViews.ItemIndex := 0;

  if PageControl1.PageCount > 0 then
    PageControl1.ActivePageIndex := 0;

  // ============================================================
  // Tabs initial befüllen
  // (OnChange-Events der ComboBoxen triggern die Updates bereits,
  //  aber wir rufen sie zur Sicherheit explizit auf.)
  // ============================================================
  UpdatePermissions;
  UpdateViewsPermissions;
  UpdateProcPermissions;
  UpdateRolePermissions;
end;

initialization
  {$I permissionmanage.lrs}

end.
