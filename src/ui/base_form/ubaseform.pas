unit uBaseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, JCUIScalerComponent,
  uthemeselector;

type

  { TfrmBaseForm }

  TfrmBaseForm = class(TForm)
    JCScaler1: TJCScaler;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmBaseForm: TfrmBaseForm;

implementation

{$R *.lfm}

{ TfrmBaseForm }

procedure TfrmBaseForm.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

end.

