unit NewDomain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons,
  //newlib
  SysTables;
  //end-newlib

type

  { TfmNewDomain }

  TfmNewDomain = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbType: TComboBox;
    edDefault: TEdit;
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    seSize: TSpinEdit;
    //newlib
    procedure FormShow(Sender: TObject);
    //end-newlib
  private
    { private declarations }
  public
    { public declarations }
    procedure Init;
  end; 

var
  fmNewDomain: TfmNewDomain;

implementation

{ TfmNewDomain }

//newlib
procedure TfmNewDomain.FormShow(Sender: TObject);
begin
  cbType.Items.Clear;
  dmSysTables.GetBasicTypes(cbType.Items);
  if cbType.Items.Count > 0 then
    cbType.ItemIndex:= 0;
end;
//end-newlib

procedure TfmNewDomain.Init;
begin
  edName.Clear;
  cbType.ItemIndex:= -1;
  edDefault.Clear;
  seSize.Value:= 0;
end;

initialization
  {$I newdomain.lrs}

end.

