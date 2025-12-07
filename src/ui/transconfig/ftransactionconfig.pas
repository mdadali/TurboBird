unit ftransactionconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  uthemeselector;

type

  { TfmTransactionConfig }

  TfmTransactionConfig = class(TForm)
    chkBoxTxAutoCommit: TCheckBox;
    chkBoxTxWaitMode: TCheckBox;
    chkBoxTxAccesMode: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  fmTransactionConfig: TfmTransactionConfig;

implementation

{$R *.lfm}

{ TfmTransactionConfig }

procedure TfmTransactionConfig.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

end.

