unit CreateUser;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls,
  fbcommon,
  turbocommon,
  fsimpleobjextractor,
  uthemeselector;

type

  { TfmCreateUser }

  TfmCreateUser = class(TForm)
    bbCreate: TBitBtn;
    bbCanel: TBitBtn;
    cbRoles: TComboBox;
    cxGrantRole: TCheckBox;
    edUserName: TEdit;
    edPassword: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure cxGrantRoleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FExtractor: TSimpleObjExtractor;
    FOwnsExtractor: Boolean;
  public
    procedure Init(dbIndex: Integer);
  end;

var
  fmCreateUser: TfmCreateUser;

implementation

{ TfmCreateUser }

procedure TfmCreateUser.cxGrantRoleChange(Sender: TObject);
begin
  cbRoles.Visible := cxGrantRole.Checked;
end;

procedure TfmCreateUser.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  // Extractor nur freigeben, wenn wir ihn selbst erzeugt haben
  if FOwnsExtractor and Assigned(FExtractor) then
    FreeAndNil(FExtractor);

  CloseAction := caFree;
end;

procedure TfmCreateUser.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmCreateUser.Init(dbIndex: Integer);
var
  RolesList: TStringList;
  DBNode: TTreeNode;
begin
  // ============================================================
  // Extractor vom DB-Node holen (Level 1)
  // ============================================================
  FExtractor := nil;
  FOwnsExtractor := False;

  DBNode := turbocommon.FindNodeByDBIndex(Word(dbIndex));
  if Assigned(DBNode) and Assigned(DBNode.Data) then
    FExtractor := TPNodeInfos(DBNode.Data)^.SimpleObjExtractor;

  if not Assigned(FExtractor) then
  begin
    FExtractor := TSimpleObjExtractor.Create(dbIndex);
    FOwnsExtractor := True;
  end;

  // ============================================================
  // Rollen laden
  // ============================================================
  cbRoles.Items.Clear;

  RolesList := TStringList.Create;
  try
    FExtractor.ExtractObjectNames(dbIndex, otRoles, false,
      TStrings(RolesList), '');
    cbRoles.Items.AddStrings(RolesList);
  finally
    RolesList.Free;
  end;

  if cbRoles.Items.Count > 0 then
    cbRoles.ItemIndex := 0
  else
    cxGrantRole.Visible := False;
end;

initialization
  {$I createuser.lrs}

end.
