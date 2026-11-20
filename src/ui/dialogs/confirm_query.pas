unit confirm_query;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  SynHighlighterSQL,
  uBaseForm;

type

  { TfrmConfirmQuery }

  TfrmConfirmQuery = class(TfrmBaseForm)
    btnExecute: TButton;
    btnCancel: TButton;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure FormShow(Sender: TObject);
  private
  public
    class function Execute(const ACaption: string; var ASQL: string): Boolean;
  end;

var
  frmConfirmQuery: TfrmConfirmQuery;

implementation

{$R *.lfm}

{ TfrmConfirmQuery }

procedure TfrmConfirmQuery.FormShow(Sender: TObject);
begin
  inherited;
end;

class function TfrmConfirmQuery.Execute(const ACaption: string; var ASQL: string): Boolean;
var
  Frm: TfrmConfirmQuery;
begin
  Result := False;
  Frm := TfrmConfirmQuery.Create(nil);
  try
    if ACaption <> '' then
      Frm.Caption := ACaption
    else
      Frm.Caption := 'Confirm Query';

    Frm.SynEdit1.Lines.Text := ASQL;
    Frm.SynEdit1.ReadOnly := False;
    Frm.Position := poScreenCenter;

    if Frm.ShowModal = mrOk then
    begin
      ASQL := Frm.SynEdit1.Lines.Text;
      Result := True;
    end;
  finally
    Frm.Free;
  end;
end;

end.
