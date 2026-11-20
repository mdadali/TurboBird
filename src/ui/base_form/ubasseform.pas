unit uBasseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, JCUIScalerComponent,
  uthemeselector;

type

  { TfrmBasseForm }

  TfrmBasseForm = class(TForm)
    JCScaler1: TJCScaler;
    procedure FormShow(Sender: TObject);
  private
    FThemeApplied: Boolean;
  public
  end;

implementation

{$R *.lfm}

procedure TfrmBasseForm.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;


end.
