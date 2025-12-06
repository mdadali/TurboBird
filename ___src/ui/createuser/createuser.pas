unit CreateUser;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  fbcommon,
  turbocommon,
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
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    procedure Init(dbIndex: Integer);
    { public declarations }
  end; 

var
  fmCreateUser: TfmCreateUser;

implementation

{ TfmCreateUser }

uses SysTables;

procedure TfmCreateUser.cxGrantRoleChange(Sender: TObject);
begin
  cbRoles.Visible:= cxGrantRole.Checked;
end;

procedure TfmCreateUser.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmCreateUser.Init(dbIndex: Integer);
var
  Count: Integer;
begin
  cbRoles.Items.CommaText:= dmSysTables.GetDBObjectNames(dbIndex, otRoles, Count);
  if count > 0 then
    cbRoles.ItemIndex := 0
  else
    cxGrantRole.Visible := false;
end;

initialization
  {$I createuser.lrs}

end.

