unit EnterPass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons,
  uthemeselector;

type

  { TfmEnterPass }

  TfmEnterPass = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbRole: TComboBox;
    edPassword: TEdit;
    edUser: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    laDatabase: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmEnterPass: TfmEnterPass;

implementation

{ TfmEnterPass }

procedure TfmEnterPass.FormActivate(Sender: TObject);
begin
  if Showing then
    edPassword.SetFocus;
end;

procedure TfmEnterPass.BitBtn1Click(Sender: TObject);
begin

end;

procedure TfmEnterPass.FormShow(Sender: TObject);
begin
  cbRole.ItemIndex:= -1;
  frmThemeSelector.btnApplyClick(self);
end;


initialization
  {$I enterpass.lrs}

end.

