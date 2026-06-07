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
    cbServerName: TComboBox;
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
    edtServerAlias: TEdit;
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
    procedure edtServerAliasChange(Sender: TObject);
    procedure edtUserNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbServerNameChange(Sender: TObject);
    procedure btnClientLibClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure lbPasswordClick(Sender: TObject);
  private
    FEventBackupList: array of TEventBackup;

    FServerSession: TServerSession;
    FSelectedServer: string;
    FIsNewServer: boolean;
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
  //cbServerName.ItemIndex := -1;

  // Alle Eingabefelder leeren
  edtServerAlias.Clear;
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
var ServerList: TStringList;
  i: integer;
begin
  ServerList := GetServerListFromTreeView;
  if ServerList = nil then
    exit;

  try
    cbServerName.Items.Clear;
    for i := 0 to ServerList.Count - 1 do
    begin
      CheckPortNumber(ServerList[i]);
      cbServerName.Items.Add(ServerList[i]);
    end;
  finally
    ServerList.Free;
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

  if cbServerName.Items.Count > 0 then
    bbTestConnection.Enabled := true;

  PopulateCharsets;
end;

{procedure TfmServerRegistry.init(ANode: TTreeNode);
var Rec:TServerRecord;
    SavedPwd: boolean;
begin
  if not Assigned(ANode.Data) then
    exit;

  FServerSession := TPNodeInfos(ANode.Data)^.ServerSession;
  if FServerSession = nil then
  begin
    CreateServerSession(FServerSession, Trim(ANode.Text));
    FIsNewServer := true;
    cbServerName.ItemIndex := -1;
  end else
  begin
    cbServerName.ItemIndex := cbServerName.Items.IndexOf(FSelectedServer);
    FIsNewServer := false;
  end;

  //UpdateControlsForProtocol;
  FSelectedServer := FServerSession.ServerName;
  SavedPwd := FServerSession.Password <> '';
  Rec := BuildServerRecordFromSession(FServerSession, SavedPwd);
  FillFormFromServerRecord(Rec);
end;}

procedure TfmServerRegistry.init(ANode: TTreeNode);
var Rec:TServerRecord;
    SavedPwd: boolean;
begin
  if not Assigned(ANode.Data) then
    exit;

  FServerSession := TPNodeInfos(ANode.Data)^.ServerSession;

  if FServerSession = nil then
  begin
    CreateServerSession(FServerSession, Trim(ANode.Text));
    FIsNewServer := true;
    cbServerName.ItemIndex := -1;
  end else
  begin
    FIsNewServer := false;
  end;

  // FSelectedServer IMMER setzen (für neue UND bestehende)
  FSelectedServer := FServerSession.ServerName;

  if not FIsNewServer then
    cbServerName.ItemIndex := cbServerName.Items.IndexOf(FSelectedServer);

  SavedPwd := FServerSession.Password <> '';
  Rec := BuildServerRecordFromSession(FServerSession, SavedPwd);
  FillFormFromServerRecord(Rec);
end;

procedure TfmServerRegistry.FormShow(Sender: TObject);
var idx: Integer;
begin
  UpdateControlsForProtocol;

  frmThemeSelector.btnApplyClick(self);

  cbServerName.SetFocus;

  if cbServerName.Items.Count = 0 then
    exit;

  idx := cbServerName.Items.IndexOf(FSelectedServer);
  if idx >= 0 then
    cbServerName.ItemIndex := idx
  else
    cbServerName.ItemIndex := 0;

  cbServerNameChange(nil);
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

  edtPassword.Enabled := not IsLocal;
  if not edtPassword.Enabled then
    edtPassword.Text := '';

  chkSavePassword.Enabled := not IsLocal;
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

procedure TfmServerRegistry.edtServerAliasChange(Sender: TObject);
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
begin
  if not InputQuery('Add New Server', 'Enter the server name:', NewServer) then
    Exit; // User hat abgebrochen

  //bbReg.Enabled := false;

  NewServer := Trim(NewServer);
  if NewServer = '' then
  begin
    MessageDlg('Server name cannot be empty.', mtError, [mbOK], 0);
    Exit;
  end;

  // Prüfen ob schon vorhanden
  for i := 0 to cbServerName.Items.Count - 1 do
    if SameText(cbServerName.Items[i], NewServer) then
    begin
      MessageDlg('This server already exists in the list.', mtInformation, [mbOK], 0);
      cbServerName.ItemIndex := i;
      Exit;
    end;

  // Neuen Server hinzufügen
  Node := turbocommon.MainTreeView.Items.Add(nil, NewServer);
  TPNodeInfos(Node.Data)^.ObjectType := tvotServer;
  Node.Visible := false;
  Node.Text := NewServer;

  cbServerName.Items.Add(NewServer);
  cbServerName.ItemIndex := cbServerName.Items.Count - 1;

  bbTestConnection.Enabled := true;
  //bbReg.Enabled := true;

  MessageDlg('New server "' + NewServer + '" has been added.' + sLineBreak +
             'Please complete the details and save.', mtInformation, [mbOK], 0);

  ResetForm;
  Init(Node);
  UpdateControlsForProtocol;

  btnAddServer.Enabled := false;
end;

procedure TfmServerRegistry.CreateServerSession(var AServerSession: TServerSession; AServerName: string);
begin
  AServerSession := TServerSession.Create(
      AServerName,
      'Alias for ' + AServerName,
      'SYSDBA',  //UserName
      '',        //Password
      '',        //Role
      TProtocol(cbProtocol.ItemIndex),
      '3050',   //port
      'UTF8',
      '/opt/firebird',
      '/opt/firebird/lib/libfbclient.so',
      '/opt/firebird/firebird.conf',
      0, //minor
      0, //major
      false,
      false,
      2000, //Connection Timeout in ms.
      3,    //RetryCount
      0     //Query Timeout in ms.
    );
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
    // 1️⃣ ServicesConnection auf Basis FServerSession konfigurieren
    ServicesConn.Protocol := FServerSession.Protocol;
    ServicesConn.ServerName := FServerSession.ServerName;
    ServicesConn.Params.Clear;
    ServicesConn.PortNo := FServerSession.Port;
    ServicesConn.Params.Add('user_name=' + FServerSession.UserName);
    ServicesConn.Params.Add('password=' + FServerSession.Password);
    ServicesConn.LoginPrompt := False;

    // 2️⃣ SecurityService mit ServicesConnection verbinden
    IBXSecurityService.ServicesConnection := ServicesConn;
    IBXSecurityService.UserName := FServerSession.UserName;
    IBXSecurityService.Password := FServerSession.Password;

    // 3️⃣ Verbindung aufbauen
    try
      ServicesConn.Connected := True;
    except
      on E: Exception do
      begin
        ShowMessage('Error connecting to Security Service: ' + E.Message);
        Exit(False);
      end;
    end;

    // 4️⃣ UserInfo abrufen
    IBXSecurityService.DisplayUsers;

    // 5️⃣ Prüfen, ob Dummy-User bereits existiert
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

procedure TfmServerRegistry.bbRegClick(Sender: TObject);
var
  Rec: TServerRecord;
  SavePwd: Boolean;
  tmpNode, InfoNode: TTreeNode;
begin
  if FServerSession.FBVersionMajor = 0 then //keine Server infos
  begin
    // Aktuelle Einstellungen übernehmen
    FillServerRecordFromForm(Rec);

    // Session aktualisieren und trennen
    FServerSession.Disconnect;
    ApplyServerRecordToSession(Rec, FServerSession);
  end;

  SavePwd := chkSavePassword.Checked;

  if SavePwd then
    SavePwd := SavePwd and (MessageDlg(
      'Warning: Your password will be stored unencrypted!' + sLineBreak +
      'Do you still want to save it?',
      mtWarning,
      [mbYes, mbNo],
      0
    ) = mrYes);

  try
    Rec := BuildServerRecordFromSession(FServerSession, SavePwd);

    if FIsNewServer then
    begin
      FSelectedServer :=  Trim(cbServerName.Text);
      tmpNode := GetServerNodeByServerName(FSelectedServer);
      tmpNode.Visible := true;
      //tmpNode := turbocommon.MainTreeView.Items.Add(nil, cbServerName.Text);
      tmpNode.ImageIndex := 25;
      tmpNode.SelectedIndex := 25;
      TPNodeInfos(tmpNode.Data)^.ObjectType := tvotServer;
      turbocommon.MainTreeView.Selected := tmpNode;
      turbocommon.MainTreeView.Selected := turbocommon.MainTreeView.Items[0];
      FIsNewServer := false;
    end;
  finally
    SaveServerDataToFile(Rec);
    Application.ProcessMessages;
  end;

  btnAddServer.Enabled := true;
  ModalResult := mrOk;
end;

procedure TfmServerRegistry.bbCancelClick(Sender: TObject);
var tmpNode: TTreeNode;
begin
  if FIsNewServer then
  begin
    tmpNode := GetServerNodeByServerName(Trim(cbServerName.Text));
    if Assigned(TmpNode) then
    begin
      if  Assigned(TmpNode.Data) then
        if Assigned(TPNodeInfos(TmpNode.Data)^.ServerSession) then
          if TPNodeInfos(TmpNode.Data)^.ServerSession.Connected then
            TPNodeInfos(TmpNode.Data)^.ServerSession.Disconnect;
      turbocommon.MainTreeView.Items.Delete(tmpNode);
    end;
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
  if cbServerName.ItemIndex < 0 then
  begin
    MessageDlg('Please select a server to delete.', mtWarning, [mbOK], 0);
    Exit;
  end;

  //bbReg.Enabled := False;
  ServerName := cbServerName.Text;
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
  turbocommon.MainTreeView.Items.Delete(Node);

  if Assigned(FServerSession) then
  begin
    if FServerSession.Connected then
    begin
      FServerSession.Disconnect;
      Application.ProcessMessages;
    end;
  end;

  // Server auch aus ComboBox entfernen
  idx := cbServerName.Items.IndexOf(ServerName);
  if idx >= 0 then
  begin
    cbServerName.Items.Delete(idx);
  end;

  if cbServerName.Items.Count > 0 then
  begin
    cbServerName.ItemIndex := 0;
    Node := GetServerNodeByServerName(Trim(cbServerName.Text));
    ServerRec  := GetServerRecordFromFileByName(cbServerName.Text);
    ApplyServerRecordToSession(ServerRec, TPNodeInfos(Node.Data)^.ServerSession);
    Init(Node);
  end else
  begin
    ResetForm
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

{ Serverdaten laden oder Defaults setzen }
procedure TfmServerRegistry.cbServerNameChange(Sender: TObject);
var Rec: TServerRecord;
begin
  Rec := GetServerRecordFromFileByName(cbServerName.Text);
  cbProtocol.OnChange := nil;

  if Ord(Rec.Protocol) < cbProtocol.Items.Count then
    cbProtocol.ItemIndex := Ord(Rec.Protocol)
  else
    cbProtocol.ItemIndex := -1;

  UpdateControlsForProtocol;
  cbProtocol.OnChange := @cbProtocolChange;

  FSelectedServer := cbServerName.Text;
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
  Rec.ServerName        := cbServerName.Text;
  Rec.ServerAlias       := edtServerAlias.Text;

  {Rec.VersionString     := edtVersionString.Text;
  Rec.VersionMajor      := StrToInt(edtVersionMajor.Text);
  Rec.VersionMinor      := StrToInt(edtVersionMinor.Text);}

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

procedure TfmServerRegistry.FillFormFromServerRecord(const Rec: TServerRecord);
begin
  cbServerName.Text        := Rec.ServerName;
  edtServerAlias.Text      := Rec.ServerAlias;

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

  //ShowMessage(BoolToStr(Rec.LoadRegisteredClientLib));
  chkSavePassword.Checked  := (Rec.Password <> '');

  edtConnectionTimeout.Text := IntToStr(Rec.ConnectTimeoutMS);
  edtRetryCount.Text        := IntToStr(Rec.RetryCount);
  edtQueryTimeout.Text      := IntToStr(Rec.QueryTimeoutMS);
end;

function TfmServerRegistry.GetServerRecordFromForm: TServerRecord;
begin
  Result.ServerName        := cbServerName.Text;
  Result.ServerAlias       := edtServerAlias.Text;

  {Rec.VersionString        := edtVersionString.Text;
  Rec.VersionMajor         := StrToInt(edtVersionMajor.Text);
  Rec.VersionMinor         := StrToInt(edtVersionMinor.Text);}

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

