{
ie vier Design-Regeln sind:
#	Regel	                                   Wo enforced
1	Kein DB ohne Server	                   Rechtsklick auf Server-Node (UI)
2	Kein Server ohne erfolgreichen Test	   bbReg disabled bis bbTestConnectionClick erfolgreich
3	Kein DB ohne erfolgreichen Test	           bbReg disabled bis bbTestClick erfolgreich
4	ConnStr muss zum Server passen	           ValidateDBConnectionString in bbTestClick, RegisterDatabase, EditRegisteration
}
unit Reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls, IBDatabase, IBDatabaseInfo,
  IB, IBQuery,

  turbocommon,
  fbcommon,
  fServerSession,
  uthemeselector;

type

  { TfmReg }

  TfmReg = class(TForm)
    bbCancel: TBitBtn;
    bbReg: TBitBtn;
    bbTest: TBitBtn;
    btBrowseDB: TButton;
    btnBrowseClient: TButton;
    cbCharset: TComboBox;
    cboxSQLDialect: TComboBox;
    cboxServers: TComboBox;
    chkBoxConnectDBOnStart: TCheckBox;
    chkboxOverwriteServerClientLib: TCheckBox;
    cxSavePassword: TCheckBox;
    edDatabaseName: TEdit;
    edtPassword: TEdit;
    edtRole: TEdit;
    edtFBClient: TEdit;
    edTitle: TEdit;
    edtPort: TEdit;
    edtUserName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    IBConnection1: TIBDatabase;
    IBDatabaseInfo1: TIBDatabaseInfo;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    lbPassword: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbRole: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialogClient: TOpenDialog;
    Panel1: TPanel;
    procedure bbRegClick(Sender: TObject);
    procedure bbTestClick(Sender: TObject);
    procedure btBrowseDBClick(Sender: TObject);
    procedure btnBrowseClientClick(Sender: TObject);
    procedure cboxServersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    function EditRegisteration(Index: Integer; Title, DatabaseName, UserName, Password, Charset, Role: string;
      SavePassword: Boolean; FBClient: string; SQLDialect: string; Port: string; ServerName: string;
      OverwriteLoadedClientLib: boolean; ConnectOnApplicationStart: boolean): Boolean;
  public
    { public declarations }
    NewReg: Boolean;
    RecPos: Integer;
    function RegisterDatabase(Title, DatabaseName, UserName, Password, Charset, Role: string;
      SavePassword: Boolean; FBClient: string; SQLDialect: string; Port: string; ServerName: string;
      OverwriteLoadedClientLib: boolean; ConnectOnApplicationStart: boolean): Boolean;

    function TestDBConnection(DatabaseName, UserName, Password, Charset: string;
                              FBClient: string; SQLDialect: string; Port: string;
                              ServerName: string;
                              OverwriteLoadedClientLib: boolean): Boolean; overload;
    function TestDBConnection(DatabaseName, UserName, Password, Charset: string;
                              FBClient: string; SQLDialect: string; Port: string;
                              ServerName: string;
                              OverwriteLoadedClientLib: boolean;
                              out ADBCharset: string): Boolean; overload;
    function GetEmptyRec: Integer;
    function SaveRegistrations: Boolean;
    procedure Sort;

    procedure RefreshServerCombobox;
    procedure UpdateControlsForProtocol;
  end;

var
  fmReg: TfmReg;

implementation

{ TfmReg }

{ ============================================================
  UpdateControlsForProtocol: Enabled-Zustände je nach Protocol.

  DESIGN-REGEL (neu):
    - TCP:    User, Password, SavePassword, Role, Port → alle enabled.
              User wird vom Server geerbt (Prefill), Password bleibt leer.
    - Local:  User + Password werden vom Server geerbt und sind DISABLED
              (weil Embedded keine eigenen Credentials braucht, sondern
               die des Servers verwendet).
              Role + Port sind DISABLED (bei Embedded nicht relevant).
              SavePassword ist DISABLED (kein eigenes Passwort).

  Wichtig: User/Password werden NICHT geleert, sondern nur disabled.
  Der Prefill in mnRegDBClick bzw. die Vererbung in bbRegClick
  sorgt dafür, dass die richtigen Werte drinstehen.
  ============================================================ }
procedure TfmReg.UpdateControlsForProtocol;
var
  ServerRec: TServerRecord;
  IsLocal: Boolean;
begin
  if (cboxServers.ItemIndex < 0) or (cboxServers.Items.Count = 0) then
    Exit;

  ServerRec := GetServerRecordFromFileByAlias(Trim(cboxServers.Text));
  IsLocal := ServerRec.IsEmbedded;

  if IsLocal then
  begin
    // Local: User + Password werden vom Server geerbt → disabled
    edtUserName.Enabled    := False;
    edtPassword.Enabled    := False;
    cxSavePassword.Enabled := False;
    edtRole.Enabled        := False;
    edtPort.Enabled        := False;

    // Werte vom Server übernehmen (Vererbung)
    edtUserName.Text := Trim(ServerRec.UserName);
    edtPassword.Text := Trim(ServerRec.Password);

    cxSavePassword.Checked := False;
    edtRole.Text := '';
    edtPort.Text := '';
  end
  else
  begin
    // TCP: alle Felder enabled, Password bleibt leer
    edtUserName.Enabled    := True;
    edtPassword.Enabled    := True;
    cxSavePassword.Enabled := True;
    edtRole.Enabled        := True;
    edtPort.Enabled        := True;

    // User vom Server erben (falls leer)
    if Trim(edtUserName.Text) = '' then
      edtUserName.Text := Trim(ServerRec.UserName);

    // Password bleibt leer – User muss es eingeben
    edtPassword.Text := '';

    // Port vom Server übernehmen
    if Trim(edtPort.Text) = '' then
      edtPort.Text := Trim(ServerRec.Port);

    // Role vom Server übernehmen
    if Trim(edtRole.Text) = '' then
      edtRole.Text := Trim(ServerRec.Role);

    cxSavePassword.Checked := False;
  end;
end;

{ ============================================================
  bbRegClick: Speichert die DB-Registrierung.

  NEU (Vererbung):
    - Local:  User + Password werden vom Server-Record
              übernommen (auch wenn sie im Formular disabled und
              leer sind).
    - TCP:    User wird vom Server geerbt (falls leer),
              Password bleibt was der User eingegeben hat.

  Damit funktioniert Embedded (FB 2.5+) ohne manuelles
  Passwort, weil das Server-Passwort geerbt wird.
  ============================================================ }
procedure TfmReg.bbRegClick(Sender: TObject);
var
  ServerRec: TServerRecord;
  EffectiveUser, EffectivePassword: string;
begin
  if Trim(edTitle.Text) = '' then
  begin
    ShowMessage('You should fill all fields');
    edTitle.SetFocus;
    Exit;
  end;

  // Server-Record für Vererbung holen
  ServerRec := GetServerRecordFromFileByAlias(Trim(cboxServers.Text));

  // ------------------------------------------------------------
  // Vererbung: User + Password je nach Protocol
  // ------------------------------------------------------------
  if ServerRec.IsEmbedded then
  begin
    // Local: User + Password vom Server erben
    EffectiveUser     := Trim(ServerRec.UserName);
    EffectivePassword := Trim(ServerRec.Password);
  end
  else
  begin
    // TCP: User vom Server erben (falls leer), Password was der User eingegeben hat
    EffectiveUser := Trim(edtUserName.Text);
    if EffectiveUser = '' then
      EffectiveUser := Trim(ServerRec.UserName);

    EffectivePassword := edtPassword.Text;
  end;

  if Trim(EffectivePassword) = '' then
    cxSavePassword.Checked := false;

  // Neue Registrierung
  if NewReg then
  begin
    if RegisterDatabase(edTitle.Text, edDatabaseName.Text, EffectiveUser, EffectivePassword,
      cbCharset.Text, edtRole.Text, cxSavePassword.Checked, edtFBClient.Text,
      cboxSQLDialect.Text, edtPort.Text, cboxServers.Text,
      chkboxOverwriteServerClientLib.Checked, chkBoxConnectDBOnStart.Checked) then
      ModalResult := mrOK;
  end
  else
  // Bearbeiten einer bestehenden Registrierung
  begin
    if EditRegisteration(RecPos, edTitle.Text, edDatabaseName.Text, EffectiveUser, EffectivePassword,
      cbCharset.Text, edtRole.Text, cxSavePassword.Checked, edtFBClient.Text,
      cboxSQLDialect.Text, edtPort.Text, cboxServers.Text,
      chkboxOverwriteServerClientLib.Checked, chkBoxConnectDBOnStart.Checked) then
      ModalResult := mrOK;
  end;
end;

{ ============================================================
  bbTestClick: Testet die DB-Verbindung.

  NEU (Vererbung):
    - Local:  User + Password werden vom Server-Record
              übernommen (weil die Felder disabled sind).
    - TCP:    User vom Server (falls leer), Password was der
              User eingegeben hat.

  DESIGN-REGEL 4 (ConnStr-Validierung) bleibt aktiv.
  ============================================================ }
procedure TfmReg.bbTestClick(Sender: TObject);
var
  Rec: TServerRecord;
  ConnMessage: string;
  TestUser, TestPassword, ActualCharset, SelectedCharset: string;
  CharsetMismatch: Boolean;
  Idx: Integer;
  WarningMsg: string;
begin
  // DESIGN-REGEL 4: ConnStr-Validierung — HART abbrechen
  if not ValidateDBConnectionString(edDatabaseName.Text, Trim(cboxServers.Text),
                                    WarningMsg) then
  begin
    MessageDlg(
      'The database connection string does not match the selected server:' +
      sLineBreak + sLineBreak +
      WarningMsg + sLineBreak + sLineBreak +
      'Please correct the connection string before testing.',
      mtError, [mbOK], 0);
    bbReg.Enabled := False;
    Exit;
  end;

  Rec := GetServerRecordFromFileByAlias(Trim(cboxServers.Text));

  if Trim(Rec.ServerName) = '' then
  begin
    MessageDlg('Server record not found for "' + cboxServers.Text + '".' + sLineBreak +
               'Cannot test connection without a server record.',
               mtError, [mbOK], 0);
    bbReg.Enabled := False;
    Exit;
  end;

  // ------------------------------------------------------------
  // Vererbung: TestUser + TestPassword je nach Protocol
  // ------------------------------------------------------------
  if Rec.IsEmbedded then
  begin
    // Local: User + Password vom Server erben
    TestUser     := Trim(Rec.UserName);
    TestPassword := Trim(Rec.Password);
  end
  else
  begin
    // TCP: User vom Server erben (falls leer)
    TestUser := Trim(edtUserName.Text);
    if TestUser = '' then
      TestUser := Trim(Rec.UserName);

    TestPassword := edtPassword.Text;
  end;

  // Sicherheitsnetz für FB-Client / Port / Charset
  if Trim(edtFBClient.Text) = '' then
    edtFBClient.Text := Rec.ClientLibraryPath;
  if Trim(edtPort.Text) = '' then
    edtPort.Text := Rec.Port;
  if Trim(cbCharset.Text) = '' then
    if Rec.Charset <> '' then
      if cbCharset.Items.IndexOf(Rec.Charset) >= 0 then
        cbCharset.ItemIndex := cbCharset.Items.IndexOf(Rec.Charset);

  ActualCharset := '';
  if TestDBConnection(edDatabaseName.Text, TestUser, TestPassword,
                     cbCharset.Text, edtFBClient.Text,
                     cboxSQLDialect.Text, edtPort.Text, cboxServers.Text,
                     chkboxOverwriteServerClientLib.Checked,
                     ActualCharset) then
  begin
    bbReg.Enabled := True;

    if Rec.IsEmbedded then
      ConnMessage := 'Embedded connection established successfully'
    else
      ConnMessage := IBXProtocolToString(Rec.Protocol) + ' connection established successfully';

    SelectedCharset := Trim(cbCharset.Text);
    CharsetMismatch := (Trim(ActualCharset) <> '') and
                       (not SameText(Trim(ActualCharset), SelectedCharset));

    if CharsetMismatch then
    begin
      ConnMessage := ConnMessage + sLineBreak + sLineBreak +
        'Database charset:  ' + ActualCharset + sLineBreak +
        'Selected charset:  ' + SelectedCharset + sLineBreak + sLineBreak;

      if SameText(ActualCharset, 'NONE') then
        ConnMessage := ConnMessage +
          'This database has NO default charset (CHARACTER SET NONE).' + sLineBreak +
          'Firebird does not convert between NONE and ' + SelectedCharset + ',' + sLineBreak +
          'so bytes are stored and fetched unchanged.' + sLineBreak + sLineBreak +
          'Do you want to adopt "NONE" as the connection charset?' + sLineBreak +
          '(Choose No if the data is actually UTF-8 encoded.)'
      else
        ConnMessage := ConnMessage +
          'Do you want to update the connection charset to "' + ActualCharset + '"?';

      if MessageDlg(ConnMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        Idx := cbCharset.Items.IndexOf(ActualCharset);

        if Idx >= 0 then
          cbCharset.ItemIndex := Idx
        else
          MessageDlg(
            'Charset "' + ActualCharset + '" is not in the list.' + sLineBreak +
            sLineBreak +
            'The current selection ("' + cbCharset.Text + '") has been kept.' + sLineBreak +
            sLineBreak +
            'Please choose a suitable charset manually if needed,' + sLineBreak +
            'or ask your DBA which charset matches the data.',
            mtInformation, [mbOK], 0);
      end;
    end
    else
      ShowMessage(ConnMessage);
  end
  else
    bbReg.Enabled := False;
end;

procedure TfmReg.btBrowseDBClick(Sender: TObject);
var
  Rec: TServerRecord;
begin
  if OpenDialog1.Execute then
  begin
    // ComboBox zeigt Alias
    Rec := GetServerRecordFromFileByAlias(Trim(cboxServers.Text));
    edDatabaseName.Text:=  OpenDialog1.FileName;
    if not Rec.IsEmbedded then
    begin
      // MakeConnectionString braucht den ECHTEN Servernamen,
      // nicht den Alias — daher Rec.ServerName verwenden.
      edDatabaseName.Text := MakeConnectionString(Rec.ServerName, Rec.Port, edDatabaseName.Text);
    end;
  end;
end;

procedure TfmReg.btnBrowseClientClick(Sender: TObject);
begin
  if OpenDialogClient.Execute then
    edtFBClient.Text := OpenDialogClient.FileName;
end;

{ ============================================================
  Wird ausgelöst, wenn der Anwender den Server in der ComboBox
  wechselt (oder wenn wir programmatisch ItemIndex setzen).

  Aktualisiert:
    - Enabled-Flags je nach Protokoll (UpdateControlsForProtocol)
    - Server-bezogene Felder: FBClient, Port, UserName, Role,
      Charset, SQLDialect

  NICHT aktualisiert (DB-spezifisch, bleiben beim Serverwechsel
  unverändert bzw. leer bei "neu"):
    - edTitle
    - edDatabaseName
    - edtPassword
    - cxSavePassword

  Bei "Edit" ist cboxServers.Enabled = false — dann läuft nur
  UpdateControlsForProtocol, und lmEditDBRegClick befüllt die
  Felder aus dem DB-Record.
  ============================================================ }
procedure TfmReg.cboxServersChange(Sender: TObject);
var
  ServerRec: TServerRecord;
begin
  UpdateControlsForProtocol;

  // Bei Edit ist die ComboBox deaktiviert — Felder werden
  // von lmEditDBRegClick befüllt, nicht hier.
  if not cboxServers.Enabled then
    Exit;

  if (cboxServers.ItemIndex < 0) or (cboxServers.Items.Count = 0) then
    Exit;

  // Server-Record über Alias holen (mit Fallback auf echten Namen)
  ServerRec := GetServerRecordFromFileByAlias(Trim(cboxServers.Text));
  if Trim(ServerRec.ServerName) = '' then
    Exit;

  // Server-bezogene Felder befüllen
  edtFBClient.Text := ServerRec.ClientLibraryPath;
  edtPort.Text     := ServerRec.Port;
  edtUserName.Text := ServerRec.UserName;
  edtRole.Text     := ServerRec.Role;

  if ServerRec.Charset <> '' then
  begin
    if cbCharset.Items.IndexOf(ServerRec.Charset) >= 0 then
      cbCharset.ItemIndex := cbCharset.Items.IndexOf(ServerRec.Charset);
  end;

  cboxSQLDialect.ItemIndex := 2;
end;

procedure TfmReg.FormCreate(Sender: TObject);
begin
  chkboxOverwriteServerClientLib.Hint :=
  'Use a custom Firebird client library.' + sLineBreak +
  sLineBreak +
  'If not checked,' + sLineBreak +
  'the library is inherited from the owning server''s registry settings.';
end;

procedure TfmReg.RefreshServerCombobox;
var
  AliasList: TStringList;
  i: integer;
  tmpIdxTxt: string;
begin
  tmpIdxTxt := cboxServers.Text;

  // ComboBox zeigt künftig ALIASE. GetServerAliasListFromTreeView
  // liefert den Anzeige-Namen (Alias oder Fallback: echter Name).
  AliasList := GetServerAliasListFromTreeView;
  if AliasList = nil then
    Exit;

  try
    cboxServers.Items.Clear;

    for i := 0 to AliasList.Count - 1 do
      cboxServers.Items.Add(AliasList[i]);
  finally
    AliasList.Free;
  end;

  if cboxServers.Items.IndexOf(tmpIdxTxt) > -1 then
    cboxServers.ItemIndex := cboxServers.Items.IndexOf(tmpIdxTxt)
  else if cboxServers.Items.Count > 0 then
    cboxServers.ItemIndex := 0
  else
    cboxServers.ItemIndex := -1;
end;

procedure TfmReg.FormShow(Sender: TObject);
begin
  // WICHTIG: UpdateControlsForProtocol nur aufrufen, wenn die
  // Felder noch nicht vorbefüllt sind. mnRegDBClick hat die
  // Felder (FBClient, Port, UserName, ...) bereits gesetzt.
  // Ein erneuter Aufruf würde bei IsLocal=true Port und Role
  // wieder leeren — genau der Bug, der zum Verschwinden der
  // Vorbefüllung führt.
  if Trim(edtFBClient.Text) = '' then
    UpdateControlsForProtocol;

  frmThemeSelector.btnApplyClick(self);
end;

{ ============================================================
  RegisterDatabase: Legt eine neue DB-Registrierung an.

  DESIGN-REGEL 4 (strikt):
    ConnStr muss zum ausgewählten Server passen.
    Bei Diskrepanz: HARTE Fehlermeldung, kein Speichern.
  ============================================================ }
function TfmReg.RegisterDatabase(
  Title, DatabaseName, UserName, Password, Charset, Role: string;
  SavePassword: Boolean;
  FBClient: string; SQLDialect: string; Port: string; ServerName: string;
  OverwriteLoadedClientLib: boolean;
  ConnectOnApplicationStart: boolean): Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;
  ServerRec: TServerRecord;
  WarningMsg: string;
begin
  Result := False;

  // DESIGN-REGEL 4: ConnStr-Validierung — HART abbrechen
  if not ValidateDBConnectionString(DatabaseName, ServerName, WarningMsg) then
  begin
    MessageDlg(
      'The database connection string does not match the selected server:' +
      sLineBreak + sLineBreak +
      WarningMsg + sLineBreak + sLineBreak +
      'Please correct the connection string before saving.',
      mtError, [mbOK], 0);
    Result := False;
    Exit;
  end;

  try
    ServerRec := GetServerRecordFromFileByAlias(
      cboxServers.Items[cboxServers.ItemIndex]
    );

    FileName := GetConfigurationDirectory + DatabasesRegFile;

    AssignFile(F, FileName);

    if FileExists(FileName) then
    begin
      FileMode := 2;
      Reset(F);
      Seek(F, System.FileSize(F));
    end
    else
      Rewrite(F);

    FillChar(Rec, SizeOf(Rec), 0);

    Rec.Title := Title;
    Rec.DatabaseName := DatabaseName;
    Rec.UserName := UserName;

    if SavePassword then
      Rec.Password := Password
    else
      Rec.Password := '';

    Rec.Charset := Charset;
    Rec.Role := Role;
    Rec.SavePassword := SavePassword;
    Rec.Deleted := False;
    Rec.LastOpened := Now;

    Rec.FireBirdClientLibPath := FBClient;
    Rec.SQLDialect := SQLDialect;
    Rec.Port := Port;
    Rec.ServerName := ServerName;
    Rec.OverwriteLoadedClientLib := OverwriteLoadedClientLib;
    Rec.ConnectOnApplicationStart := ConnectOnApplicationStart;

    Rec.ServerVersionString := ServerRec.VersionString;
    Rec.ServerVersionMajor  := ServerRec.VersionMajor;
    Rec.ServerVersionMinor  := ServerRec.VersionMinor;

    Rec.IsEmbedded := ServerRec.IsEmbedded;

    Write(F, Rec);
    CloseFile(F);

    Result := True;

  except
    on E: Exception do
    begin
      try
        CloseFile(F);
      except
      end;

      ShowMessage('Error registering database:' + LineEnding + E.Message);
      Result := False;
    end;
  end;
end;

{ ============================================================
  EditRegisteration: Ändert eine bestehende DB-Registrierung.

  DESIGN-REGEL 4 (strikt):
    ConnStr muss zum ausgewählten Server passen.
    Bei Diskrepanz: HARTE Fehlermeldung, kein Speichern.
  ============================================================ }
function TfmReg.EditRegisteration(Index: Integer; Title, DatabaseName, UserName, Password, Charset, Role: string;
   SavePassword: Boolean; FBClient: string; SQLDialect: string; Port: string; ServerName: string;
   OverwriteLoadedClientLib: boolean; ConnectOnApplicationStart: boolean): Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;
  ServerRec: TServerRecord;
  WarningMsg: string;
begin
  Result := False;

  // DESIGN-REGEL 4: ConnStr-Validierung — HART abbrechen
  if not ValidateDBConnectionString(DatabaseName, ServerName, WarningMsg) then
  begin
    MessageDlg(
      'The database connection string does not match the selected server:' +
      sLineBreak + sLineBreak +
      WarningMsg + sLineBreak + sLineBreak +
      'Please correct the connection string before saving.',
      mtError, [mbOK], 0);
    Result := False;
    Exit;
  end;

  try
    ServerRec := GetServerRecordFromFileByAlias(cboxServers.Items[cboxServers.ItemIndex]);
    FileName:= GetConfigurationDirectory + DatabasesRegFile;

    AssignFile(F, FileName);
    FileMode:= 2;
    Reset(F);
    Seek(F, Index);

    Rec.Title := Title;
    Rec.DatabaseName:= DatabaseName;
    Rec.UserName:= UserName;
    if SavePassword then
      Rec.Password:= Password
    else
      Rec.Password:= '';
    Rec.Charset:= Charset;
    Rec.Role:= Role;
    Rec.SavePassword:= SavePassword;
    Rec.Deleted:= False;
    Rec.FireBirdClientLibPath := FBClient;
    Rec.SQLDialect :=  SQLDialect;
    Rec.Port := Port;
    Rec.ServerName := ServerName;
    Rec.OverwriteLoadedClientLib := OverwriteLoadedClientLib;
    Rec.ConnectOnApplicationStart := ConnectOnApplicationStart;

    Rec.ServerVersionString := ServerRec.VersionString;
    Rec.ServerVersionMajor  := ServerRec.VersionMajor;
    Rec.ServerVersionMinor  := ServerRec.VersionMinor;

    Rec.IsEmbedded := ServerRec.IsEmbedded;

    Write(F, Rec);
    CloseFile(F);
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
      ShowMessage('Error: ' + e.Message);
    end;
  end;
end;

{ ============================================================
  TestDBConnection (Original-Signatur) — Wrapper.

  Ruft die neue überladene Version mit einem Dummy-out-
  Parameter auf. Damit bleiben alle bestehenden Aufrufer
  (turbocommon.pas, ConnectToDBAs) unverändert funktionsfähig.
  ============================================================ }
function TfmReg.TestDBConnection(DatabaseName, UserName, Password, Charset: string;
                                 FBClient: string; SQLDialect: string; Port: string;
                                 ServerName: string;
                                 OverwriteLoadedClientLib: boolean): Boolean;
var
  DummyCharset: string;
begin
  Result := TestDBConnection(DatabaseName, UserName, Password, Charset, FBClient,
                             SQLDialect, Port, ServerName, OverwriteLoadedClientLib,
                             DummyCharset);
end;

{ ============================================================
  TestDBConnection (neue Signatur) — prüft Verbindung UND
  ermittelt den tatsächlichen DB-Charset.

  NEU:
    - Nach erfolgreichem Connect wird
      SELECT RDB$CHARACTER_SET_NAME FROM RDB$DATABASE
      ausgeführt (in einer temporären Transaktion), und der
      Wert in ADBCharset zurückgegeben.
    - Die Charset-Abfrage ist "best effort": Wenn sie scheitert
      (z.B. Rechte fehlen), bleibt ADBCharset leer, und der
      Verbindungstest gilt dennoch als erfolgreich.
    - Cleanup: Query und Trans werden immer freigegeben, auch
      im Fehlerfall.

  WICHTIG: Die Charset-Abfrage läuft in einer EIGENEN Transaktion,
  damit der Test nicht in die Haupt-Transaktion des Aufrufers
  eingreift (z.B. bei ConnectToDBAs).
  ============================================================ }
function TfmReg.TestDBConnection(DatabaseName, UserName, Password, Charset: string;
                                 FBClient: string; SQLDialect: string; Port: string;
                                 ServerName: string;
                                 OverwriteLoadedClientLib: boolean;
                                 out ADBCharset: string): Boolean;
var
  ServerErrStr: string;
  Q: TIBQuery;
  Trans: TIBTransaction;
begin
  Result := False;
  ADBCharset := '';

  if not IsServerReachable(ServerName, ServerErrStr) then
  begin
    MessageDlg(ServerErrStr, mtError, [mbOK], 0);
    Exit;
  end;

  try
    IBConnection1.Close;
    IBConnection1.DatabaseName := DatabaseName;
    IBConnection1.LoginPrompt := False;
    IBConnection1.FirebirdLibraryPathName := FBClient;
    with IBConnection1.Params do
    begin
      Clear;
      Add('user_name=' + UserName);
      Add('password=' + Password);
      Add('lc_ctype=' + Charset);
      Add('sql_dialect=' + SQLDialect);
    end;

    IBConnection1.Open;
    if IBConnection1.Connected then
    begin
      Result := True;

      // ------------------------------------------------------
      // Charset-Abfrage (best effort)
      // ------------------------------------------------------
      Q := nil;
      Trans := nil;
      try
        Trans := TIBTransaction.Create(nil);
        Trans.DefaultDatabase := IBConnection1;
        Trans.StartTransaction;

        Q := TIBQuery.Create(nil);
        Q.Database := IBConnection1;
        Q.Transaction := Trans;
        Q.SQL.Text := 'SELECT RDB$CHARACTER_SET_NAME FROM RDB$DATABASE';
        Q.Open;
        if not Q.EOF then
          ADBCharset := Trim(Q.Fields[0].AsString);
        Q.Close;

        Trans.Commit;
      except
        // Charset-Abfrage fehlgeschlagen → kein Grund, den
        // Verbindungstest scheitern zu lassen
        ADBCharset := '';
      end;

      if Assigned(Q) then
      begin
        try
          if Q.Active then Q.Close;
        except
        end;
        Q.Free;
      end;
      if Assigned(Trans) then
      begin
        try
          if Trans.InTransaction then Trans.Rollback;
        except
        end;
        Trans.Free;
      end;

      IBConnection1.Close;
    end;
  except
    on E: EIBError do
    begin
      Result := False;
      MessageDlg(
        'Unable to connect: ' + E.Message + LineEnding +
        'SQLCode: ' + IntToStr(E.SQLCode),
        mtError, [mbOK], 0
      );
    end;
    on E: Exception do
    begin
      Result := False;
      MessageDlg('Unable to connect: ' + e.Message, mtError, [mbOK], 0);
    end;
  end;
end;

function TfmReg.GetEmptyRec: Integer;
var
  FileName: string;
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
begin
  Result:= -1;

  FileName:= GetConfigurationDirectory + DatabasesRegFile;

  AssignFile(F, FileName);
  if FileExists(FileName) then
  begin
    Reset(F);
    while not Eof(F) do
    begin
      Read(F, Rec);
      if Rec.Deleted then
      begin
        Result:= FilePos(F) - 1;
        Break;
      end;
    end;
    Closefile(F);
  end;
end;

function TfmReg.SaveRegistrations: Boolean;
var
  F: file of TRegisteredDatabase;
  FileName: string;
  i: Integer;
begin
  try
    FileName := GetConfigurationDirectory + DatabasesRegFile;

    AssignFile(F, FileName);
    FileMode := 2;
    Rewrite(F);

    for i := 0 to High(RegisteredDatabases) do
    begin
      RegisteredDatabases[i].OrigRegRec :=
        RegisteredDatabases[i].RegRec;

      Write(F, RegisteredDatabases[i].OrigRegRec);
    end;

    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TfmReg.Sort;
var
  TempRec: TRegisteredDatabase;
  Done: Boolean;
  i: Integer;
  TempIndex: Integer;
begin
  repeat
    Done:= True;
    for i:= 0 to High(RegisteredDatabases) - 1 do
      if RegisteredDatabases[i].RegRec.LastOpened < RegisteredDatabases[i + 1].RegRec.LastOpened then
      begin
        Done:= False;
        TempRec:= RegisteredDatabases[i].OrigRegRec;
        RegisteredDatabases[i].OrigRegRec:= RegisteredDatabases[i + 1].OrigRegRec;
        RegisteredDatabases[i].RegRec:= RegisteredDatabases[i + 1].RegRec;
        RegisteredDatabases[i + 1].OrigRegRec:= TempRec;
        RegisteredDatabases[i + 1].RegRec:= TempRec;

        TempIndex:= RegisteredDatabases[i].Index;
        RegisteredDatabases[i].Index:= RegisteredDatabases[i + 1].Index;
        RegisteredDatabases[i + 1].Index:= TempIndex;
      end;
  until Done;
end;

initialization
  {$I reg.lrs}

end.
