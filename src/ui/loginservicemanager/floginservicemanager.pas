unit floginservicemanager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmLoginServiceManager }

  TfrmLoginServiceManager = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    chkBoxSavePwd: TCheckBox;
    edtPassword: TEdit;
    edtUserName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbSever: TLabel;
  private

  public

  end;

//var
  //frmLoginServiceManager: TfrmLoginServiceManager;

implementation

{$R *.lfm}

end.

