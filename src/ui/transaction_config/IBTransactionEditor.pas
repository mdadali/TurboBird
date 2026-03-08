unit IBTransactionEditor;

{$mode Delphi}

interface

uses
  SysUtils, Classes, Forms, Dialogs, Controls, StdCtrls, ExtCtrls,
  IniFiles, IB, IBDatabase,

  turbocommon,
  uthemeselector;

type

  { TIBTransactionEditorForm }

  TIBTransactionEditorForm = class(TForm)
    btnSaveAsDefault: TButton;
    btnLoadPresets: TButton;
    cbReadConsistency: TCheckBox;
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

    procedure btnLoadPresetsClick(Sender: TObject);
    procedure btnSaveAsDefaultClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FNodeInfos: TPNodeInfos;
    FTransaction: TIBTransaction;

    FFirebirVersionMajor: word;

    procedure LoadFromTransaction;
    procedure SaveToTransaction;

  public
    procedure Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
    function  EditTransaction: Boolean;
  end;

implementation

{$R *.lfm}

procedure TIBTransactionEditorForm.Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
begin
  FDBIndex:= dbIndex;
  FNodeInfos := ANodeInfos;
  FTransaction := RegisteredDatabases[dbIndex].IBTransaction;
  FFirebirVersionMajor := RegisteredDatabases[dbIndex].RegRec.ServerVersionMajor;
end;

function TIBTransactionEditorForm.EditTransaction: Boolean;
begin
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

procedure TIBTransactionEditorForm.btnSaveAsDefaultClick(Sender: TObject);
begin
  // === Isolation ===
  if rbConcurrency.Checked then
    DefTxIsolation := 2          // Snapshot / Concurrency
  else if rbReadCommitted.Checked then
  begin
    if rbRecVersion.Checked then
      DefTxIsolation := 1        // ReadCommitted + rec_version
    else
      DefTxIsolation := 0;       // ReadCommitted
  end
  else if rbConsistency.Checked then
    DefTxIsolation := 3;         // Table Stability / Consistency

  // === Access Mode ===
  if rbRead.Checked and not rbWrite.Checked then
    Include(DefTxFlags, txReadOnly)
  else
    Exclude(DefTxFlags, txReadOnly);

  // === Wait Mode ===
  if rbWait.Checked then
    Include(DefTxFlags, txWait)
  else
    Exclude(DefTxFlags, txWait);

  // === Record Version ===
  if rbRecVersion.Checked then
    Include(DefTxFlags, txRecVersion)
  else
    Exclude(DefTxFlags, txRecVersion); // no_rec_version überschreibt rec_version

  // === Autocommit ===
  if cbAutocommit.Checked then
    Include(DefTxFlags, txAutoCommit)
  else
    Exclude(DefTxFlags, txAutoCommit);

  // === ReadConsistency (FB 4+) ===
  if cbReadConsistency.Checked then
    Include(DefTxFlags, txReadConsistency)
  else
    Exclude(DefTxFlags, txReadConsistency);

  // === Lock Timeout ===
  DefTxLockTimeout := StrToIntDef(edtTimeout.Text, 0);

  // === Default Tx Name ===
  DefTxName := 'DEFAULT_TRANSACTION'; // optional: aus Editfeld setzen

  WriteIniFile;

  ShowMessage('Transaction defaults saved to INI!');
end;

procedure TIBTransactionEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
end;

procedure TIBTransactionEditorForm.btnLoadPresetsClick(Sender: TObject);
var
  PresetFile: string;
  Ini: TIniFile;
  Isolation: Integer;
  Flags: TTxFlags;
  LockTimeout: Integer;
  TxName: string;
begin
  with TOpenDialog.Create(Self) do
  try
    Filter := 'INI files|*.ini|All files|*.*';
    Title := 'Select Transaction Preset';

    InitialDir :=
      IncludeTrailingPathDelimiter(GetAppConfigDir(False)) +
      'data' + DirectorySeparator +
      'transaction_presets';

    if Execute then
    begin
      PresetFile := FileName;

      Ini := TIniFile.Create(PresetFile);
      try
        // === Isolation ===
        Isolation := Ini.ReadInteger('Transaction', 'Isolation', 0);

        // === Flags ===
        Flags := [];
        if Ini.ReadBool('Transaction', 'ReadOnly', False) then
          Include(Flags, txReadOnly);
        if Ini.ReadBool('Transaction', 'Wait', True) then
          Include(Flags, txWait);
        if Ini.ReadBool('Transaction', 'RecVersion', True) then
          Include(Flags, txRecVersion);
        if Ini.ReadBool('Transaction', 'AutoCommit', False) then
          Include(Flags, txAutoCommit);
        if Ini.ReadBool('Transaction', 'ReadConsistency', False) then
          Include(Flags, txReadConsistency);

        // === LockTimeout ===
        LockTimeout := Ini.ReadInteger('Transaction', 'LockTimeout', 0);

        // === TxName ===
        TxName := Ini.ReadString('Transaction', 'TxName', 'PRESET_TX');

        // === Formular füllen ===

        // Isolation
        case Isolation of
          0: rbReadCommitted.Checked := True;
          1: begin
               rbReadCommitted.Checked := True;
               rbRecVersion.Checked := True;
             end;
          2: rbConcurrency.Checked := True;
          3: rbConsistency.Checked := True;
        else
          rbReadCommitted.Checked := True;
        end;

        // Access Mode
        if txReadOnly in Flags then
        begin
          rbRead.Checked := True;
          rbWrite.Checked := False;
        end
        else
        begin
          rbRead.Checked := False;
          rbWrite.Checked := True;
        end;

        // Record Version
        rbRecVersion.Checked := txRecVersion in Flags;
        rbNoRecVersion.Checked := not(rbRecVersion.Checked);

        // Wait Mode
        rbWait.Checked := txWait in Flags;
        rbNoWait.Checked := not rbWait.Checked;

        // Autocommit
        cbAutocommit.Checked := txAutoCommit in Flags;

        // ReadConsistency
        cbReadConsistency.Checked := txReadConsistency in Flags;

        // LockTimeout
        edtTimeout.Text := IntToStr(LockTimeout);

        ShowMessage('Preset loaded: ' + TxName);

      finally
        Ini.Free;
      end;
    end;
  finally
    Free;
  end;
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

procedure TIBTransactionEditorForm.FormShow(Sender: TObject);
begin
  cbReadConsistency.Visible := (FFirebirVersionMajor > 3);
  frmThemeSelector.btnApplyClick(self);
end;

end.
