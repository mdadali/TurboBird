unit fserverregistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, Dialogs, ComCtrls,
  Graphics, ExtCtrls,
  fServerSession,
  turbocommon,
  fbcommon,
  fSetFBClient,
  uthemeselector,

  IB,
  IBDatabase,    //for test-embedded connection

  IBXServices;
type

  TEventBackup = record
     Control: TComponent;
     OnChange: TNotifyEvent;
     OnClick: TNotifyEvent;
     OnSelect: TNotifyEvent;
     OnClose: TNotifyEvent;
   end;

  { TfmServerRegistry }

  TfmServerRegistry = class(TForm)
    bbCancel: TBitBtn;
    bbReg: TBitBtn;
    bbTestConnection: TBitBtn;
    btnfirebirdconffile: TButton;
    btnAddServer: TButton;
    btnDeleteServer: TButton;
    btnOpenRootPath: TButton;
    cbServerAlias: TComboBox;
    cboxLoadRegisteredClientLib: TCheckBox;
    edtRootPath: TEdit;
    edtVersionMinor: TEdit;
    edtVersionMajor: TEdit;
    edtVersionString: TEdit;
    edtQueryTimeout: TEdit;
    edtRetryCount: TEdit;
    edtConnectionTimeout: TEdit;
    edtConfigFilePath: TEdit;
    edtPort: TEdit;
    edtServerName: TEdit;
    edtUserName: TEdit;
    edtRole: TEdit;
    cbProtocol: TComboBox;
    edtPassword: TEdit;
    cbCharset: TComboBox;
    edtClientLibraryPath: TEdit;
    btnClientLib: TButton;
    chkSavePassword: TCheckBox;
    GroupBox1: TGroupBox;
    IBXServerProperties1: TIBXServerProperties;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    lbFBConfigFile: TLabel;
    lbDescription: TLabel;
    lbPortInfo: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbPassword: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDlgLoadClientLib: TOpenDialog;
    Panel1: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;

    procedure bbCancelClick(Sender: TObject);
    procedure bbRegClick(Sender: TObject);
    procedure bbTestConnectionClick(Sender: TObject);
    procedure btnAddServerClick(Sender: TObject);
    procedure btnDeleteServerClick(Sender: TObject);
    procedure btnfirebirdconffileClick(Sender: TObject);
    procedure btnOpenRootPathClick(Sender: TObject);
    procedure cbCharsetChange(Sender: TObject);
    procedure cboxLoadRegisteredClientLibChange(Sender: TObject);
    procedure cbProtocolChange(Sender: TObject);
    procedure edtClientLibraryPathChange(Sender: TObject);
    procedure edtConfigFilePathChange(Sender: TObject);
    procedure edtPasswordChange(Sender: TObject);
    procedure edtPortChange(Sender: TObject);
    procedure edtRoleChange(Sender: TObject);
    procedure edtServerNameChange(Sender: TObject);
    procedure edtUserNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbServerAliasChange(Sender: TObject);
    procedure btnClientLibClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure lbPasswordClick(Sender: TObject);
  private
    FEventBackupList: array of TEventBackup;

    FServerSession: TServerSession;
    FSelectedServer: string;
    FIsNewServer: boolean;
    FNewServerNode: TTreeNode;
    procedure PopulateCharsets;
    procedure PopulateProtocols;

    procedure DeleteServer(const AServerName: string);
    procedure FillFormFromServerRecord(const Rec: TServerRecord);
    function  GetserverRecordFromForm: TServerRecord;
    procedure FillServerRecordFromForm(var Rec: TServerRecord);

    procedure CreateServerSession(var AServerSession: TServerSession; AServerName: string);

    procedure FillServerCombobox;

    procedure DisableFormControlsAndEvents;
    procedure EnableFormControlsAndEvents;

    function  CreateServiceUser: Boolean;
  public
    procedure init(ANode: TTreeNode);
    function  CheckPortNumber(AServerName: string): boolean;
    procedure UpdateControlsForProtocol;
    procedure ResetForm;
  end;

//var
  //fmServerRegistry: TfmServerRegistry;

implementation

{$R *.lfm}

procedure TfmServerRegistry.ResetForm;
begin
  // Server-Auswahl zurücksetzen
  //cbServerAlias.ItemIndex := -1;

  // Alle Eingabefelder leeren
  edtServerName.Clear;
  edtUserName.Text := 'SYSDBA';
  edtRole.Clear;
  edtPassword.Text := 'masterkey';
  edtPort.Text := '3050';
  edtRootPath.Text := '/opt/firebird';
  edtClientLibraryPath.Text := '/opt/firebird/lib/fbclient.so';
  edtConfigFilePath.Text := '/opt/firebird/firebird.conf';
  edtConnectionTimeout.Text := '2000';
  edtRetryCount.Text := '3';
  edtQueryTimeout.Text := '0';

  // Versionsfelder leeren
  edtVersionMajor.Text := '0';
  edtVersionMinor.Text := '0';
  edtVersionString.Clear;

  // Comboboxen auf Default setzen
  cbProtocol.ItemIndex := 0;  // TCP
  cbCharset.ItemIndex := DefaultFBCharacterSet;

  // Checkboxen zurücksetzen
  chkSavePassword.Checked := true;
  cboxLoadRegisteredClientLib.Checked := true;

  // Buttons
  bbTestConnection.Enabled := true;
  bbReg.Enabled := False;

  // Controls je nach Protokoll aktualisieren
  //UpdateControlsForProtocol;
end;

procedure TfmServerRegistry.FillServerCombobox;
var
  AliasList: TStringList;
  i: integer;
begin
  // ComboBox zeigt künftig ALIASE (GetServerAliasListFromTreeView).
  // Die internen Lookups arbeiten weiterhin mit den echten
  // Servernamen (GetServerListFromTreeView / ServerSession.ServerName).
  AliasList := GetServerAliasListFromTreeView;
  if AliasList = nil then
    exit;

  try
    cbServerAlias.Items.Clear;
    for i := 0 to AliasList.Count - 1 do
    begin
      // CheckPortNumber arbeitet auf dem Anzeige-Namen (Alias).
      // Aliase enthalten i. d. R. keinen "/port"-Suffix, daher
      // bleibt edtPort hier unverändert auf "enabled".
      CheckPortNumber(AliasList[i]);
      cbServerAlias.Items.Add(AliasList[i]);
    end;
  finally
    AliasList.Free;
  end;
end;

function TfmServerRegistry.CheckPortNumber(AServerName: string): boolean;
var PortNr: string;
begin
  result := true;
  if ServerNameContainPortNr(AServerName) then
  begin
    PortNr := GetPortNrFromServerName(AServerName);
    if IsValidPortNr(PortNr) then
    begin
      edtPort.Text := PortNr;
      edtPort.Enabled := false;
      lbPortInfo.Caption := '';
    end else
    begin
      result := false;
      edtPort.Enabled := true;
      edtPort.Text := '';
      lbPortInfo.Caption := 'Invalid Portnumber!';
    end;

  end else
  begin
    lbPortInfo.Caption := '';
    edtPort.Enabled := true;
  end;
end;

{ Form initialisieren: Serverliste + Charsets + Protocols }
procedure TfmServerRegistry.FormCreate(Sender: TObject);
begin
  //DisableFormControlsAndEvents;

  lbDescription.Caption :=
    'A server must first be registered via the menu:' + sLineBreak +
    'File → Server Registry.' + sLineBreak + sLineBreak +
    'After that, databases can be registered/created under' + sLineBreak +
    'Registered Servers.' + sLineBreak + sLineBreak +
    'After registration, the dialog can be opened' + sLineBreak + 'from the menu' + sLineBreak +
    'or by double-clicking a server node to edit the' + sLineBreak + ' settings.' + sLineBreak + sLineBreak +
    'If "Save password" is checked, the dialog will not' + sLineBreak +
    'appear automatically.' + sLineBreak +
    'It can still be opened manually at any time.';

  FIsNewServer := false;
  FillServerCombobox;

  if cbServerAlias.Items.Count > 0 then
    bbTestConnection.Enabled := true;

  PopulateCharsets;
end;

procedure TfmServerRegistry.init(ANode: TTreeNode);
var
  Rec: TServerRecord;
  SavedPwd: boolean;
  DisplayAlias: string;
begin
  if not Assigned(ANode.Data) then
    exit;

  FServerSession := TPNodeInfos(ANode.Data)^.ServerSession;

  if FServerSession = nil then
  begin
    CreateServerSession(FServerSession, Trim(ANode.Text));
    FIsNewServer := true;
    cbServerAlias.ItemIndex := -1;
  end
  else
    FIsNewServer := false;

  // FSelectedServer IMMER setzen (echter Servername)
  FSelectedServer := FServerSession.ServerName;

  // NEU: ComboBox-Index über den ALIAS setzen, nicht über den
  // echten Servernamen — die ComboBox enthält Aliase.
  if not FIsNewServer then
  begin
    if Trim(FServerSession.ServerAlias) <> '' then
      DisplayAlias := Trim(FServerSession.ServerAlias)
    else
      DisplayAlias := Trim(FServerSession.ServerName);

    cbServerAlias.ItemIndex := cbServerAlias.Items.IndexOf(DisplayAlias);
  end;

  SavedPwd := FServerSession.Password <> '';
  Rec := BuildServerRecordFromSession(FServerSession, SavedPwd);
  FillFormFromServerRecord(Rec);
end;

{ ============================================================
  FormShow: Wird NACH Init aufgerufen.

  Init hat den ComboBox-Index bereits über den ALIAS des
  aktuell bearbeiteten Servers gesetzt. FormShow darf diesen
  Wert NICHT überschreiben — sonst landet bei mehreren
  Aliasen auf demselben echten Servernamen (z.B. Prod/Test/
  Clone alle auf FB60Local) immer der erste Alias in der
  ComboBox, und die ganze Maske wird mit den falschen
  Server-Daten befüllt.

  Alte Version:
    idx := cbServerAlias.Items.IndexOf(FSelectedServer);
  FSelectedServer ist der ECHTE Servername ('FB60Local'),
  der in der Alias-Liste nicht vorkommt → idx = -1 →
  ItemIndex := 0 → FALSCHER Server.

  Neue Version: Nur nachbessern, wenn Init den Index nicht
  setzen konnte (ItemIndex < 0).
  ============================================================ }
procedure TfmServerRegistry.FormShow(Sender: TObject);
var
  idx: Integer;
  DisplayAlias: string;
begin
  UpdateControlsForProtocol;
  frmThemeSelector.btnApplyClick(self);
  cbServerAlias.SetFocus;

  if cbServerAlias.Items.Count = 0 then
    exit;

  // Wenn Init bereits einen gültigen ItemIndex gesetzt hat,
  // diesen respektieren — sonst würde der falsche Alias
  // selektiert.
  if cbServerAlias.ItemIndex < 0 then
  begin
    if Trim(FServerSession.ServerAlias) <> '' then
      DisplayAlias := Trim(FServerSession.ServerAlias)
    else
      DisplayAlias := Trim(FServerSession.ServerName);

    idx := cbServerAlias.Items.IndexOf(DisplayAlias);
    if idx >= 0 then
      cbServerAlias.ItemIndex := idx
    else
      cbServerAlias.ItemIndex := 0;
  end;

  cbServerAliasChange(nil);
end;

procedure TfmServerRegistry.GroupBox1Click(Sender: TObject);
begin

end;

procedure TfmServerRegistry.lbPasswordClick(Sender: TObject);
begin

end;

procedure TfmServerRegistry.cbCharsetChange(Sender: TObject);
begin
   //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.cboxLoadRegisteredClientLibChange(Sender: TObject);
begin
  if not Assigned(FServerSession) then
    exit;

  if  cboxLoadRegisteredClientLib.Checked then
    FServerSession.LoadRegisteredClientLib := true
  else
    FServerSession.LoadRegisteredClientLib := false;
end;

procedure TfmServerRegistry.UpdateControlsForProtocol;
var
  IsLocal: Boolean;
begin
  IsLocal := (LowerCase(Trim(cbProtocol.Text)) = 'local');

  //edtPassword.Enabled := not IsLocal;
  if not edtPassword.Enabled then
    edtPassword.Text := '';

  //chkSavePassword.Enabled := not IsLocal;
  if not chkSavePassword.Enabled then
    chkSavePassword.Checked := false;

  edtRole.Enabled := not IsLocal;
  edtPort.Enabled := not IsLocal;
end;

procedure TfmServerRegistry.cbProtocolChange(Sender: TObject);
begin
  UpdateControlsForProtocol;
end;

procedure TfmServerRegistry.edtClientLibraryPathChange(Sender: TObject);
begin
   //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.edtConfigFilePathChange(Sender: TObject);
begin
   //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.edtPasswordChange(Sender: TObject);
begin
   //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.edtPortChange(Sender: TObject);
begin
   //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.edtRoleChange(Sender: TObject);
begin
   //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.edtServerNameChange(Sender: TObject);
begin
   //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.edtUserNameChange(Sender: TObject);
begin
  //bbReg.Enabled := false;
end;

procedure TfmServerRegistry.btnfirebirdconffileClick(Sender: TObject);
var dlg: TOpenDialog;
begin
  //bbReg.Enabled := false;
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := 'Firebird Configuration Files|*.conf|All files|*.*';
    if dlg.Execute then
    begin
      edtConfigFilePath.Text := dlg.FileName;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfmServerRegistry.btnOpenRootPathClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    edtRootPath.Text := SelectDirectoryDialog1.FileName;
end;

procedure TfmServerRegistry.DeleteServer(const AServerName: string);
var
  FS: TFileStream;
  tmpList: TMemoryStream;
  Rec: TServerRecord;
  OutList: TMemoryStream;
begin
  if Trim(AServerName) = '' then
    Exit;

  if MessageDlg(
       'Do you really want to delete the server "' + AServerName + '"?' + sLineBreak +
       'This action cannot be undone.',
       mtConfirmation,
       [mbYes, mbNo],
       0
     ) <> mrYes then
    Exit;

  if not FileExists(GetConfigurationDirectory + ServersRegFile) then
    Exit;

  tmpList := TMemoryStream.Create;
  OutList := TMemoryStream.Create;
  try
    tmpList.LoadFromFile(GetConfigurationDirectory + ServersRegFile);
    tmpList.Position := 0;

    while tmpList.Position < tmpList.Size do
    begin
      FillChar(Rec, SizeOf(Rec), 0);
      tmpList.ReadBuffer(Rec, SizeOf(Rec));

      // Nur Records behalten, die nicht den gewünschten ServerName haben
      if not SameText(Trim(Rec.ServerName), Trim(AServerName)) then
        OutList.WriteBuffer(Rec, SizeOf(Rec));
    end;

    // Datei neu schreiben
    FS := TFileStream.Create(GetConfigurationDirectory + ServersRegFile, fmCreate);
    try
      if OutList.Size > 0 then
      begin
        OutList.Position := 0;
        FS.CopyFrom(OutList, OutList.Size);
      end;
    finally
      FS.Free;
    end;
  finally
    tmpList.Free;
    OutList.Free;
  end;
end;

procedure TfmServerRegistry.btnAddServerClick(Sender: TObject);
var
  NewServer: string;
  i: Integer;
  Node: TTreeNode;
  ServerList: TStringList;
  AlreadyExists: Boolean;
begin
  if not InputQuery('Add New Server', 'Enter the server alias:', NewServer) then
    Exit;

  NewServer := Trim(NewServer);
  if NewServer = '' then
  begin
    MessageDlg('Server name cannot be empty.', mtError, [mbOK], 0);
    Exit;
  end;

  // Prüfen ob schon vorhanden — über die ECHTEN Servernamen
  AlreadyExists := False;
  ServerList := GetServerListFromTreeView;
  if Assigned(ServerList) then
  begin
    try
      for i := 0 to ServerList.Count - 1 do
      begin
        if SameText(ServerList[i], NewServer) then
        begin
          AlreadyExists := True;
          Break;
        end;
      end;
    finally
      ServerList.Free;
    end;
  end;

  if AlreadyExists then
  begin
    MessageDlg('This server already exists in the list.', mtInformation, [mbOK], 0);
    Exit;
  end;

  // Neuen Server-Knoten anlegen
  Node := turbocommon.MainTreeView.Items.Add(nil, NewServer);
  TPNodeInfos(Node.Data)^.ObjectType := tvotServer;
  Node.Visible := false;

  // WICHTIG: Den neuen Knoten merken. Er wird erst in bbRegClick
  // sichtbar gemacht und auf den Alias umbenannt. Ohne diese
  // Referenz würde GetServerNodeByServerName bei zwei Servern
  // mit demselben echten Servernamen den FALSCHEN Knoten finden
  // und umbenennen.
  FNewServerNode := Node;

  bbTestConnection.Enabled := true;

  MessageDlg('New server "' + NewServer + '" has been added.' + sLineBreak +
             'Please complete the details and save.', mtInformation, [mbOK], 0);

  ResetForm;
  Init(Node);
  UpdateControlsForProtocol;

  FillServerCombobox;
  if cbServerAlias.Items.Count > 0 then
    cbServerAlias.ItemIndex := cbServerAlias.Items.Count - 1;

  btnAddServer.Enabled := false;
end;

procedure TfmServerRegistry.bbTestConnectionClick(Sender: TObject);
var
  Rec: TServerRecord;
  Msg: string;
  DlgType: TMsgDlgType;
  ODSMajorMinorStr: string;
  ODSMajor, ODSMinor: integer;
  ServerVersion: string;
  IntPort: Word;
begin
  // Aktuelle Einstellungen übernehmen
  FillServerRecordFromForm(Rec);

  // Session aktualisieren und trennen
  FServerSession.Disconnect;
  ApplyServerRecordToSession(Rec, FServerSession);

  if FServerSession.Connected then
    FServerSession.Disconnect;

  FServerSession.IBXConnect;

  if FServerSession.Connected then
  begin
    self.edtVersionMajor.Text := IntToStr(FServerSession.FBVersionMajor);
    self.edtVersionMinor.Text := IntToStr(FServerSession.FBVersionMinor);
    self.edtVersionString.Text := FServerSession.FBVersionString;

    bbReg.Enabled := true;
    MessageDlg(FServerSession.ErrorStr, mtInformation, [mbOK], 0);
  end else
  begin
    bbReg.Enabled := false;
    MessageDlg(FServerSession.ErrorStr, mtError, [mbOK], 0);
  end;
end;

function TfmServerRegistry.CreateServiceUser: Boolean;
var
  ServicesConn: TIBXServicesConnection;
  IBXSecurityService: TIBXSecurityService;
  UserExists: Boolean;
  i: Integer;
begin
  Result := False;

  ServicesConn := TIBXServicesConnection.Create(nil);
  IBXSecurityService := TIBXSecurityService.Create(nil);
  try
    //ServicesConnection auf Basis FServerSession konfigurieren
    ServicesConn.Protocol := FServerSession.Protocol;
    ServicesConn.ServerName := FServerSession.ServerName;
    ServicesConn.Params.Clear;
    ServicesConn.PortNo := FServerSession.Port;
    ServicesConn.Params.Add('user_name=' + FServerSession.UserName);
    ServicesConn.Params.Add('password=' + FServerSession.Password);
    ServicesConn.LoginPrompt := False;

    // SecurityService mit ServicesConnection verbinden
    IBXSecurityService.ServicesConnection := ServicesConn;
    IBXSecurityService.UserName := FServerSession.UserName;
    IBXSecurityService.Password := FServerSession.Password;

    // Verbindung aufbauen
    try
      ServicesConn.Connected := True;
    except
      on E: Exception do
      begin
        ShowMessage('Error connecting to Security Service: ' + E.Message);
        Exit(False);
      end;
    end;

    // UserInfo abrufen
    IBXSecurityService.DisplayUsers;

    // Prüfen, ob Dummy-User bereits existiert
    UserExists := False;
    for i := 0 to IBXSecurityService.UserInfoCount - 1 do
    begin
      if UpperCase(IBXSecurityService.UserInfo[i].UserName) = UpperCase(InitialServiceUser) then
      begin
        UserExists := True;
        Break;
      end;
    end;


    // User nur erstellen, wenn er noch nicht existiert
    {if not UserExists then
    begin
      IBXSecurityService.UserName := InitialServiceUser;
      IBXSecurityService.Password := InitialServiceUserPwd;
      IBXSecurityService.AddUser;
    end; }

    Result := false;

  finally
    if ServicesConn.Connected then
      ServicesConn.Connected := False;
    IBXSecurityService.Free;
    ServicesConn.Free;
  end;
end;

{ ============================================================
  Speichert die Server-Registrierung.

  NEU: Wenn noch keine Version bekannt ist (weder aus dem
  Record noch aus einer vorangegangenen Test-Connection),
  wird automatisch eine Verbindung aufgebaut und die Version
  ermittelt — genau der Verhalten, den der User beim Test-
  Connection-Button sieht, nur eben automatisch beim Speichern.
  Damit landen neue Server immer mit korrekter Version in
  servers.reg.

  Bei Verbindungsfehler fragt eine Meldung, ob trotzdem ohne
  Versions-Info gespeichert werden soll.
  ============================================================ }
procedure TfmServerRegistry.bbRegClick(Sender: TObject);
var
  Rec: TServerRecord;
  SavePwd: Boolean;
  tmpNode: TTreeNode;
  OldAlias, NewAlias: string;
  RealServerName: string;
  MigratedCount: Integer;
  WasNewServer: Boolean;
  NeedAutoTest: Boolean;
begin
  OldAlias       := Trim(FServerSession.ServerAlias);
  RealServerName := Trim(FServerSession.ServerName);
  WasNewServer   := FIsNewServer;

  NewAlias := Trim(cbServerAlias.Text);
  if NewAlias = '' then
  begin
    MessageDlg('Server alias cannot be empty.', mtError, [mbOK], 0);
    cbServerAlias.SetFocus;
    Exit;
  end;

  if not IsServerAliasUnique(NewAlias, OldAlias, RealServerName) then
  begin
    MessageDlg('The alias "' + NewAlias + '" is already in use by another server.' +
               sLineBreak + sLineBreak +
               'Server aliases must be system-wide unique.',
               mtError, [mbOK], 0);
    cbServerAlias.SetFocus;
    Exit;
  end;

  FillServerRecordFromForm(Rec);

  // NEU: Auto-Test auch triggern, wenn NUR der VersionString leer
  // ist (z.B. alte Registrierung ohne String, oder frische Session).
  // Vorher: AND — VersionMajor=6 und VersionString='' wurde nicht
  // erkannt, weil Major != 0 war.
  NeedAutoTest := (Rec.VersionMajor = 0) or (Trim(Rec.VersionString) = '');

  if NeedAutoTest then
  begin
    FServerSession.Disconnect;
    ApplyServerRecordToSession(Rec, FServerSession);

    try
      FServerSession.IBXConnect;

      if FServerSession.Connected then
      begin
        Rec.VersionMajor  := FServerSession.FBVersionMajor;
        Rec.VersionMinor  := FServerSession.FBVersionMinor;
        Rec.VersionString := FServerSession.FBVersionString;

        edtVersionMajor.Text  := IntToStr(Rec.VersionMajor);
        edtVersionMinor.Text  := IntToStr(Rec.VersionMinor);
        edtVersionString.Text := Rec.VersionString;
      end
      else
      begin
        if MessageDlg(
             'Automatic version detection failed:' + sLineBreak +
             sLineBreak +
             FServerSession.ErrorStr + sLineBreak +
             sLineBreak +
             'Register the server anyway (without version info)?',
             mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          FServerSession.Disconnect;
          Exit;
        end;
      end;
    except
      on E: Exception do
      begin
        if MessageDlg(
             'Automatic version detection failed:' + sLineBreak +
             sLineBreak +
             E.Message + sLineBreak +
             sLineBreak +
             'Register the server anyway (without version info)?',
             mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          FServerSession.Disconnect;
          Exit;
        end;
      end;
    end;
  end;

  FServerSession.Disconnect;
  ApplyServerRecordToSession(Rec, FServerSession);

  SavePwd := chkSavePassword.Checked;
  if SavePwd then
    SavePwd := SavePwd and (MessageDlg(
      'Warning: Your password will be stored unencrypted!' + sLineBreak +
      'Do you still want to save it?',
      mtWarning,
      [mbYes, mbNo],
      0
    ) = mrYes);

  MigratedCount := 0;

  try
    Rec := BuildServerRecordFromSession(FServerSession, SavePwd);

    if not SameText(OldAlias, NewAlias) then
    begin
      if OldAlias <> '' then
        MigratedCount := RenameDBRegistrationsServerAlias(OldAlias, NewAlias, RealServerName)
      else if RealServerName <> '' then
        MigratedCount := RenameDBRegistrationsServerAlias(RealServerName, NewAlias, RealServerName);
    end;

    if WasNewServer then
    begin
      tmpNode := FNewServerNode;

      if Assigned(tmpNode) then
      begin
        tmpNode.Visible := true;
        tmpNode.ImageIndex := 25;
        tmpNode.SelectedIndex := 25;
        TPNodeInfos(tmpNode.Data)^.ObjectType := tvotServer;

        tmpNode.Text := NewAlias;

        turbocommon.MainTreeView.Selected := tmpNode;
        turbocommon.MainTreeView.Selected := turbocommon.MainTreeView.Items[0];
      end;

      FNewServerNode := nil;
      FIsNewServer := false;
    end
    else
    begin
      if OldAlias <> '' then
        tmpNode := GetServerNodeByAlias(OldAlias)
      else
        tmpNode := GetServerNodeByServerName(RealServerName);

      if Assigned(tmpNode) then
      begin
        tmpNode.Text := NewAlias;
        if Assigned(TPNodeInfos(tmpNode.Data)^.ServerSession) then
        begin
          TPNodeInfos(tmpNode.Data)^.ServerSession.ServerAlias := NewAlias;
          TPNodeInfos(tmpNode.Data)^.ServerSession.FBVersionString := Rec.VersionString;
        end;
      end;
    end;
  finally
    SaveServerDataToFile(Rec, OldAlias);
    Application.ProcessMessages;
  end;

  FillServerCombobox;
  if cbServerAlias.Items.Count > 0 then
  begin
    cbServerAlias.ItemIndex := cbServerAlias.Items.IndexOf(NewAlias);
    if cbServerAlias.ItemIndex < 0 then
      cbServerAlias.ItemIndex := 0;
  end;

  btnAddServer.Enabled := true;

  if MigratedCount > 0 then
    MessageDlg(IntToStr(MigratedCount) + ' database registration(s) were updated to the new server alias.',
      mtInformation, [mbOK], 0);

  ModalResult := mrOk;
end;

procedure TfmServerRegistry.bbCancelClick(Sender: TObject);
var
  tmpNode: TTreeNode;
begin
  if FIsNewServer then
  begin
    // NEU: Den gemerkten Knoten verwenden — nicht über den
    // Servernamen suchen, weil das bei zwei Servern mit demselben
    // echten Servernamen den falschen Knoten finden würde.
    tmpNode := FNewServerNode;

    if Assigned(tmpNode) then
    begin
      if Assigned(tmpNode.Data) then
        if Assigned(TPNodeInfos(tmpNode.Data)^.ServerSession) then
          if TPNodeInfos(tmpNode.Data)^.ServerSession.Connected then
            TPNodeInfos(tmpNode.Data)^.ServerSession.Disconnect;

      turbocommon.MainTreeView.Items.Delete(tmpNode);
    end;

    FNewServerNode := nil;
    FIsNewServer := false;
  end;

  btnAddServer.Enabled := true;
end;

procedure TfmServerRegistry.btnDeleteServerClick(Sender: TObject);
var
  ServerName: string;
  ServerRec: TServerRecord;
  idx: Integer;
  Node: TTreeNode;
begin
  if cbServerAlias.ItemIndex < 0 then
  begin
    MessageDlg('Please select a server to delete.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // ComboBox zeigt den Alias — echten Servernamen holen
  ServerRec := GetServerRecordFromFileByAlias(cbServerAlias.Text);
  ServerName := Trim(ServerRec.ServerName);

  if ServerName = '' then
  begin
    MessageDlg('Selected server record not found.', mtError, [mbOK], 0);
    Exit;
  end;

  //bbReg.Enabled := False;
  Node := GetServerNodeByServerName(ServerName);

  // Prüfen, ob Server in TreeView noch Childnodes hat
  if Assigned(Node) then
  begin
    if Node.HasChildren then
    begin
      if MessageDlg(
           'The server "' + ServerName + '" still has registered databases.' + sLineBreak +
           'Do you still want to delete it?',
           mtWarning, [mbYes, mbNo], 0
         ) <> mrYes then
        Exit;

      // Alle DB-ChildNodes still als gelöscht markieren
      MarkAllServerDatabasesDeleted(Node.Text);
    end;
  end;

  // Sicherstellen, dass Datei konsistent bleibt
  RemoveDeletedDBRegistrationsFromFile;

  // Server wirklich löschen
  DeleteServer(ServerName);

  // Server auch aus TreeView entfernen
  turbocommon.MainTreeView.Selected := nil;
  if Assigned(Node) then
    turbocommon.MainTreeView.Items.Delete(Node);

  if Assigned(FServerSession) then
  begin
    if FServerSession.Connected then
    begin
      FServerSession.Disconnect;
      Application.ProcessMessages;
    end;
  end;

  // ComboBox neu befüllen (enthält künftig Aliase)
  FillServerCombobox;

  if cbServerAlias.Items.Count > 0 then
  begin
    cbServerAlias.ItemIndex := 0;
    cbServerAliasChange(nil);
  end else
  begin
    ResetForm;
    //bbReg.Enabled := False;
  end;

  UpdateControlsForProtocol;
  MessageDlg('Server "' + ServerName + '" has been deleted.', mtInformation, [mbOK], 0);
end;

procedure TfmServerRegistry.PopulateCharsets;
var
  i: Integer;
begin
  cbCharset.Items.Clear;
  for i := Low(FBCharacterSets) to High(FBCharacterSets) do
    cbCharset.Items.Add(FBCharacterSets[i]);
  cbCharset.ItemIndex := DefaultFBCharacterSet; // UTF8
end;

procedure TfmServerRegistry.PopulateProtocols;
begin
{  cbProtocol.Items.Clear;
  cbProtocol.Items.Add('Local');
  cbProtocol.Items.Add('TCP');
  cbProtocol.Items.Add('XNET');
  cbProtocol.ItemIndex := 1; // TCP/IP
}
end;

procedure TfmServerRegistry.cbServerAliasChange(Sender: TObject);
var
  Rec: TServerRecord;
begin
  // ComboBox zeigt den Alias — Lookup über Alias
  Rec := GetServerRecordFromFileByAlias(cbServerAlias.Text);
  cbProtocol.OnChange := nil;

  if Ord(Rec.Protocol) < cbProtocol.Items.Count then
    cbProtocol.ItemIndex := Ord(Rec.Protocol)
  else
    cbProtocol.ItemIndex := -1;

  UpdateControlsForProtocol;
  cbProtocol.OnChange := @cbProtocolChange;

  // FSelectedServer ist der ECHTE Servername — für alle
  // nachfolgenden Lookups (GetServerNodeByServerName etc.)
  FSelectedServer := Trim(Rec.ServerName);
  //bbReg.Enabled := false;

  if Rec.ServerName <> '' then
  begin
    FillFormFromServerRecord(Rec);
  end else
  begin
    ResetForm;
  end;
end;

procedure TfmServerRegistry.FillServerRecordFromForm(var Rec: TServerRecord);
begin
  // cbServerAlias  = TComboBox → zeigt den ALIAS
  // edtServerName = TEdit     → zeigt den ECHTEN Servernamen
  Rec.ServerAlias       := cbServerAlias.Text;
  Rec.ServerName        := edtServerName.Text;

  // Version aus den Formular-Feldern übernehmen (Fix aus der
  // letzten Runde: verhindert, dass ApplyServerRecordToSession
  // eine per Test Connection ermittelte Version mit 0 überschreibt).
  Rec.VersionString     := edtVersionString.Text;
  Rec.VersionMajor      := StrToIntDef(edtVersionMajor.Text, 0);
  Rec.VersionMinor      := StrToIntDef(edtVersionMinor.Text, 0);

  Rec.UserName          := edtUserName.Text;
  Rec.Role              := edtRole.Text;
  Rec.Password          := edtPassword.Text;
  Rec.Protocol := TProtocol(cbProtocol.ItemIndex);
  Rec.Charset := cbCharset.Items[cbCharset.ItemIndex];
  Rec.RootPath := edtRootPath.Text;
  Rec.Port              := edtPort.Text;
  Rec.ClientLibraryPath := edtClientLibraryPath.Text;
  Rec.ConfigFilePath    := edtConfigFilePath.Text;
  Rec.LoadRegisteredClientLib := cboxLoadRegisteredClientLib.Checked;
  Rec.IsEmbedded              :=  (LowerCase(Trim(cbProtocol.Text)) = 'local');

  Rec.ConnectTimeoutMS := StrToInt(edtConnectionTimeout.Text);
  Rec.RetryCount       := StrToInt(edtRetryCount.Text);
  Rec.QueryTimeoutMS   := StrToInt(edtQueryTimeout.Text);
end;

procedure TfmServerRegistry.CreateServerSession(var AServerSession: TServerSession; AServerName: string);
begin
  // Default-Alias LEER lassen. Der Anzeige-Name fällt dann
  // automatisch auf den echten Servernamen zurück (siehe
  // GetServerAliasFromNode / GetServerAliasListFromTreeView).
  //
  // Würde hier "Alias for " + AServerName stehen, bekämen zwei
  // Server mit demselben echten Servernamen denselben Default-
  // Alias — und IsServerAliasUnique würde den zweiten Server
  // blockieren, bis der Anwender manuell einen Alias eintippt.
  AServerSession := TServerSession.Create(
      AServerName,
      '',        //ServerAlias — leer, Anwender kann ihn im Dialog setzen
      'SYSDBA',  //UserName
      '',        //Password
      '',        //Role
      TProtocol(cbProtocol.ItemIndex),
      '3050',    //port
      'UTF8',
      '/opt/firebird',
      '/opt/firebird/lib/libfbclient.so',
      '/opt/firebird/firebird.conf',
      0,         //minor
      0,         //major
      false,
      false,
      2000,      //Connection Timeout in ms.
      3,         //RetryCount
      0          //Query Timeout in ms.
    );
end;

procedure TfmServerRegistry.FillFormFromServerRecord(const Rec: TServerRecord);
var
  DisplayAlias: string;
begin
  // cbServerAlias  = TComboBox → zeigt den ALIAS
  // edtServerName = TEdit     → zeigt den ECHTEN Servernamen
  if Trim(Rec.ServerAlias) <> '' then
    DisplayAlias := Rec.ServerAlias
  else
    DisplayAlias := Rec.ServerName;

  cbServerAlias.Text   := DisplayAlias;
  edtServerName.Text  := Rec.ServerName;

  edtVersionString.Text    := Rec.VersionString;
  edtVersionMajor.Text     := IntToStr(Rec.VersionMajor);
  edtVersionMinor.Text     := IntToStr(Rec.VersionMinor);

  edtUserName.Text         := Rec.UserName;
  edtRole.Text             := Rec.Role;
  edtPassword.Text         := Rec.Password;

  if Ord(Rec.Protocol) < cbProtocol.Items.Count then
    cbProtocol.ItemIndex := Ord(Rec.Protocol)
  else
    cbProtocol.ItemIndex := -1;

  if cbCharset.Items.IndexOf(Rec.Charset) >= 0 then
    cbCharset.ItemIndex := cbCharset.Items.IndexOf(Rec.Charset)
  else
    cbCharset.ItemIndex := -1;

  edtRootPath.Text := Rec.RootPath;
  edtClientLibraryPath.Text:= Rec.ClientLibraryPath;
  edtConfigFilePath.Text   := Rec.ConfigFilePath;

  edtPort.Text             := Rec.Port;

  cboxLoadRegisteredClientLib.Checked  := Rec.LoadRegisteredClientLib;

  chkSavePassword.Checked  := (Rec.Password <> '');

  edtConnectionTimeout.Text := IntToStr(Rec.ConnectTimeoutMS);
  edtRetryCount.Text        := IntToStr(Rec.RetryCount);
  edtQueryTimeout.Text      := IntToStr(Rec.QueryTimeoutMS);
end;

function TfmServerRegistry.GetServerRecordFromForm: TServerRecord;
begin
  // cbServerAlias  = TComboBox → zeigt den ALIAS
  // edtServerName = TEdit     → zeigt den ECHTEN Servernamen
  Result.ServerAlias       := cbServerAlias.Text;
  Result.ServerName        := edtServerName.Text;

  Result.UserName          := edtUserName.Text;
  Result.Role              := edtRole.Text;
  Result.Password          := edtPassword.Text;
  Result.Protocol := TProtocol(cbProtocol.ItemIndex);
  Result.Charset := cbCharset.Items[cbCharset.ItemIndex];
  Result.Port              := edtPort.Text;
  Result.RootPath          := edtRootPath.Text;
  Result.ClientLibraryPath := edtClientLibraryPath.Text;
  Result.ConfigFilePath    := edtConfigFilePath.Text;
  Result.LoadRegisteredClientLib := cboxLoadRegisteredClientLib.Checked;
  Result.IsEmbedded        :=  (cbProtocol.ItemIndex = 1);

  Result.ConnectTimeoutMS := StrToInt(edtConnectionTimeout.Text);
  Result.RetryCount       := StrToInt(edtRetryCount.Text);
  Result.QueryTimeoutMS   := StrToInt(edtQueryTimeout.Text);
end;

{ Button: ClientLibrary auswählen }
procedure TfmServerRegistry.btnClientLibClick(Sender: TObject);
var FBConfigFilePath: string;
begin
  try
    //Self.Enabled := false;
    //if SetFBClient(1)  then
    if OpenDlgLoadClientLib.Execute then
    begin
      edtClientLibraryPath.Text := OpenDlgLoadClientLib.FileName; //    frmSetFBClient.FBClientLibPath;
      FBConfigFilePath := GetFBConffilePathFromFBClientlibPath(edtClientLibraryPath.Text);
      edtRootPath.Text := ExtractFilePath(edtClientLibraryPath.Text);
      FServerSession.ClientLibraryPath := edtClientLibraryPath.Text;
      if FileExists(FBConfigFilePath) then
      begin
        edtConfigFilePath.Text := FBConfigFilePath ;
      end else
      begin
        edtConfigFilePath.Text := 'Config File dos not exists! Please set it manually...';
      end;
    end;
  finally
    //frmSetFBClient.Free;
    //Self.Enabled := true;
    //FServerSession.UnloadClientLibrary;
  end;
end;

procedure TfmServerRegistry.DisableFormControlsAndEvents;
var
  i, cnt: Integer;
  c: TComponent;
begin
  cnt := 0;
  SetLength(FEventBackupList, ComponentCount);

  for i := 0 to ComponentCount - 1 do
  begin
    c := Components[i];

    if c is TWinControl then
      TWinControl(c).Enabled := False;

    // Backup bekannte Ereignisse
    if c is TEdit then
    begin
      FEventBackupList[cnt].Control := c;
      FEventBackupList[cnt].OnChange := TEdit(c).OnChange;
      TEdit(c).OnChange := nil;
      Inc(cnt);
    end
    else if c is TComboBox then
    begin
      FEventBackupList[cnt].Control := c;
      FEventBackupList[cnt].OnChange := TComboBox(c).OnChange;
      TComboBox(c).OnChange := nil;
      Inc(cnt);
    end
    else if c is TCheckBox then
    begin
      FEventBackupList[cnt].Control := c;
      FEventBackupList[cnt].OnClick := TCheckBox(c).OnClick;
      TCheckBox(c).OnClick := nil;
      Inc(cnt);
    end
    else if c is TButton then
    begin
      FEventBackupList[cnt].Control := c;
      FEventBackupList[cnt].OnClick := TButton(c).OnClick;
      TButton(c).OnClick := nil;
      Inc(cnt);
    end;
    // hier können weitere Controls ergänzt werden
  end;

  SetLength(FEventBackupList, cnt); // auf tatsächliche Anzahl trimmen
end;

procedure TfmServerRegistry.EnableFormControlsAndEvents;
var
  i: Integer;
  c: TComponent;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    c := Components[i];
    if c is TWinControl then
      TWinControl(c).Enabled := True;
  end;

  // Ereignisse wiederherstellen
  for i := 0 to High(FEventBackupList) do
  begin
    c := FEventBackupList[i].Control;

    if c is TEdit then
      TEdit(c).OnChange := FEventBackupList[i].OnChange
    else if c is TComboBox then
      TComboBox(c).OnChange := FEventBackupList[i].OnChange
    else if c is TCheckBox then
      TCheckBox(c).OnClick := FEventBackupList[i].OnClick
    else if c is TButton then
      TButton(c).OnClick := FEventBackupList[i].OnClick;
  end;
end;

end.

