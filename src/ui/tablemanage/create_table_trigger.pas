unit create_table_trigger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons,
  uthemeselector,
  umessagedlg;

type

  { TfmCreateTableTrigger }

  TfmCreateTableTrigger = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbTables: TComboBox;
    cxUpdate: TCheckBox;
    cxDelete: TCheckBox;
    cxInsert: TCheckBox;
    edTriggerName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    rbAfter: TRadioButton;
    rbBefor: TRadioButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  fmCreateTableTrigger: TfmCreateTableTrigger;

implementation

{ TfmCreateTableTrigger }

procedure TfmCreateTableTrigger.BitBtn1Click(Sender: TObject);
begin
  if Trim(edTriggerName.Text) = '' then
  begin
    MsgWarning('Please enter a trigger name.');
    edTriggerName.SetFocus;
    Exit;
  end;

  if not (cxDelete.Checked or cxInsert.Checked or cxUpdate.Checked) then
  begin
    MsgWarning('Please select at least one event (Insert / Update / Delete).');
    Exit;
  end;

  if Trim(cbTables.Text) = '' then
  begin
    MsgWarning('Please select a table.');
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TfmCreateTableTrigger.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

initialization
  {$I create_table_trigger.lrs}

end.
