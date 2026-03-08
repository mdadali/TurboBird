unit IBTransactionEditor;

{$mode Delphi}

interface

uses
  SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  IB, IBDatabase;

type

  TIBTransactionEditorForm = class(TForm)
    GroupIsolation: TGroupBox;
    rbConcurrency: TRadioButton;
    rbReadCommitted: TRadioButton;
    rbConsistency: TRadioButton;

    GroupAccess: TGroupBox;
    rbRead: TRadioButton;
    rbWrite: TRadioButton;

    GroupRecordVersion: TGroupBox;
    rbRecVersion: TRadioButton;
    rbNoRecVersion: TRadioButton;

    GroupLocking: TGroupBox;
    rbWait: TRadioButton;
    rbNoWait: TRadioButton;
    lblTimeout: TLabel;
    edtTimeout: TEdit;

    cbAutocommit: TCheckBox;

    btnOK: TButton;
    btnCancel: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);

  private
    FTransaction: TIBTransaction;

    procedure LoadFromTransaction;
    procedure SaveToTransaction;

  public
    function EditTransaction(ATransaction: TIBTransaction): Boolean;
  end;

implementation

{$R *.lfm}

function TIBTransactionEditorForm.EditTransaction(
  ATransaction: TIBTransaction): Boolean;
begin
  FTransaction := ATransaction;

  LoadFromTransaction;

  Result := ShowModal = 1;

  if Result then
    SaveToTransaction;
end;

procedure TIBTransactionEditorForm.FormCreate(Sender: TObject);
begin
  rbReadCommitted.Checked := True;
  rbWrite.Checked := True;
  rbRecVersion.Checked := True;
  rbWait.Checked := True;
end;

procedure TIBTransactionEditorForm.LoadFromTransaction;
var
  P: TStrings;
begin
  P := FTransaction.Params;

  rbConcurrency.Checked := P.IndexOf('concurrency') >= 0;
  rbReadCommitted.Checked := P.IndexOf('read_committed') >= 0;
  rbConsistency.Checked := P.IndexOf('consistency') >= 0;

  rbRead.Checked := P.IndexOf('read') >= 0;
  rbWrite.Checked := P.IndexOf('write') >= 0;

  rbRecVersion.Checked := P.IndexOf('rec_version') >= 0;
  rbNoRecVersion.Checked := P.IndexOf('no_rec_version') >= 0;

  rbWait.Checked := P.IndexOf('wait') >= 0;
  rbNoWait.Checked := P.IndexOf('nowait') >= 0;

  edtTimeout.Text := P.Values['lock_timeout'];

  cbAutocommit.Checked := P.IndexOf('autocommit') >= 0;
end;

procedure TIBTransactionEditorForm.SaveToTransaction;
begin
  if FTransaction.Active then
    FTransaction.Commit;

  FTransaction.Params.Clear;

  if rbConcurrency.Checked then
    FTransaction.Params.Add('concurrency');

  if rbReadCommitted.Checked then
    FTransaction.Params.Add('read_committed');

  if rbConsistency.Checked then
    FTransaction.Params.Add('consistency');

  if rbRead.Checked then
    FTransaction.Params.Add('read');

  if rbWrite.Checked then
    FTransaction.Params.Add('write');

  if rbRecVersion.Checked then
    FTransaction.Params.Add('rec_version');

  if rbNoRecVersion.Checked then
    FTransaction.Params.Add('no_rec_version');

  if rbWait.Checked then
    FTransaction.Params.Add('wait');

  if rbNoWait.Checked then
    FTransaction.Params.Add('nowait');

  if Trim(edtTimeout.Text) <> '' then
    FTransaction.Params.Add('lock_timeout=' + Trim(edtTimeout.Text));

  if cbAutocommit.Checked then
    FTransaction.Params.Add('autocommit');
end;

procedure TIBTransactionEditorForm.btnOKClick(Sender: TObject);
begin
  ModalResult := 1;
end;

end.
