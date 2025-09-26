unit fserverregistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, Dialogs, ComCtrls,
  ExtCtrls, fServerSession,
  turbocommon,
  fbcommon;

type

  { TfmServerRegistry }

  TfmServerRegistry = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnTestConnection: TButton;
    btnfirebirdconffile: TButton;
    btnAddServer: TButton;
    btnDeleteServer: TButton;
    cbServerName: TComboBox;
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
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;

    procedure btnAddServerClick(Sender: TObject);
    procedure btnDeleteServerClick(Sender: TObject);
    procedure btnfirebirdconffileClick(Sender: TObject);
    procedure btnTestConnectionClick(Sender: TObject);
    procedure cbProtocolChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbServerNameChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnClientLibClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectedServer: string;
    procedure PopulateCharsets;
    procedure PopulateProtocols;

    procedure DeleteServer(const AServerName: string);
    procedure ApplyServerRecordToForm(const Rec: TServerRecord);

  public
    procedure init(AServerName: string);
  end;

//var
  //fmServerRegistry: TfmServerRegistry;

implementation

{$R *.lfm}

uses
  Main; // angenommen: frmMain mit TreeView existiert

{ Form initialisieren: Serverliste + Charsets + Protocols }
procedure TfmServerRegistry.FormCreate(Sender: TObject);
var
  ServerList: TStringList;
  i: Integer;
begin
  ServerList := GetServerListFromTreeView(fmMain.tvMain);
  try
    cbServerName.Items.Clear;
    for i := 0 to ServerList.Count - 1 do
      cbServerName.Items.Add(ServerList[i]);
  finally
    ServerList.Free;
  end;

  PopulateCharsets;
  PopulateProtocols;
end;

procedure TfmServerRegistry.btnTestConnectionClick(Sender: TObject);
var
  tmpSession: TServerSession;
  Msg: string;
  DlgType: TMsgDlgType;
begin
  tmpSession := TServerSession.Create(
    cbServerName.Text,
    edtServerAlias.Text,
    edtUserName.Text,
    edtPassword.Text,
    edtRole.Text,
    TProtocol(cbProtocol.ItemIndex),
    edtClientLibraryPath.Text,
    edtConfigFilePath.Text,
    edtPort.Text,
    cbCharset.Text
  );
  try
    if tmpSession.Connect then
    begin
      tmpSession.FetchVersion;
      Msg := 'Connection successful!' + sLineBreak +
             'Server version: ' + tmpSession.FBVersionString;
      DlgType := mtInformation;
      tmpSession.Disconnect;
    end
    else
    begin
      Msg := 'Connection failed. Please check your settings.';
      DlgType := mtError;
    end;

    MessageDlg(Msg, DlgType, [mbOK], 0);
  finally
    tmpSession.Free;
  end;
end;

procedure TfmServerRegistry.cbProtocolChange(Sender: TObject);
begin
  edtPort.Enabled := (cbProtocol.Items[cbProtocol.ItemIndex] <> 'Local');
end;

procedure TfmServerRegistry.btnfirebirdconffileClick(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := 'Firebird Configuration Files|*.conf|All files|*.*';
    if dlg.Execute then
      edtConfigFilePath.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
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

  if not FileExists(GetConfigurationDirectory + 'servers.reg') then
    Exit;

  tmpList := TMemoryStream.Create;
  OutList := TMemoryStream.Create;
  try
    tmpList.LoadFromFile(GetConfigurationDirectory + 'servers.reg');
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
    FS := TFileStream.Create(GetConfigurationDirectory + 'servers.reg', fmCreate);
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
begin
  if not InputQuery('Add New Server', 'Enter the server name:', NewServer) then
    Exit; // User hat abgebrochen

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
      Init(NewServer); // existierenden Datensatz laden
      Exit;
    end;

  // Neuen Server hinzufügen
  cbServerName.Items.Add(NewServer);
  cbServerName.ItemIndex := cbServerName.Items.Count - 1;

  // Init mit Defaultwerten
  Init(NewServer);

  MessageDlg('New server "' + NewServer + '" has been added.' + sLineBreak +
             'Please complete the details and save.', mtInformation, [mbOK], 0);
end;

procedure TfmServerRegistry.btnDeleteServerClick(Sender: TObject);
var
  ServerName: string;
  idx: Integer;
  Rec: TServerRecord;
  Node: TTreeNode;
begin
  if cbServerName.ItemIndex < 0 then
  begin
    MessageDlg('Please select a server to delete.', mtWarning, [mbOK], 0);
    Exit;
  end;

  ServerName := cbServerName.Text;

  // Prüfen, ob Server in TreeView noch Childnodes hat
  Node := fmMain.tvMain.Items.GetFirstNode;
  while Assigned(Node) do
  begin
    if (Node.Level = 0) and SameText(Node.Text, ServerName) then
    begin
      if Node.HasChildren then
      begin
        if MessageDlg(
             'The server "' + ServerName + '" still has registered databases.' + sLineBreak +
             'Do you still want to delete it?',
             mtWarning, [mbYes, mbNo], 0
           ) <> mrYes then
          Exit;
      end;
      Break;
    end;
    Node := Node.getNextSibling;
  end;

  // Server wirklich löschen
  DeleteServer(ServerName);
  // Server auch aus TreeView entfernen
  Node := fmMain.tvMain.Items.GetFirstNode;
  while Assigned(Node) do
  begin
    if (Node.Level = 0) and SameText(Node.Text, ServerName) then
    begin
      // optional: Abstandhalter löschen, falls vorhanden
      if Assigned(Node.GetPrevSibling) and (Trim(Node.GetPrevSibling.Text) = '') then
        Node.GetPrevSibling.Delete;

      Node.Delete; // entfernt Server + ChildNodes
      Break;
    end;
    Node := Node.GetNextSibling;
  end;

  // Server auch aus ComboBox entfernen
  idx := cbServerName.Items.IndexOf(ServerName);
  if idx >= 0 then
    cbServerName.Items.Delete(idx);

  if cbServerName.Items.Count > 0 then
  begin
    // auf ersten Server springen
    //cbServerName.OnChange := nil;
    cbServerName.ItemIndex := 0;
    //Application.ProcessMessages;
  end else
  begin
    // keine Server mehr -> Felder leeren
    cbServerName.ItemIndex := -1;
    edtServerAlias.Clear;
    edtUserName.Clear;
    edtRole.Clear;
    edtPassword.Clear;
    cbProtocol.ItemIndex := -1;
    cbCharset.ItemIndex := -1;
    edtPort.Text := '';
    edtClientLibraryPath.Clear;
  end;

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
  cbProtocol.Items.Clear;
  cbProtocol.Items.Add('Local');
  cbProtocol.Items.Add('TCP');
  cbProtocol.Items.Add('XNET');
  cbProtocol.ItemIndex := 1; // TCP/IP
end;

procedure TfmServerRegistry.init(AServerName: string);
begin
  FSelectedServer := AServerName;
end;

{ Serverdaten laden oder Defaults setzen }
procedure TfmServerRegistry.cbServerNameChange(Sender: TObject);
var
  Rec: TServerRecord;
begin
 Rec := GetServerRecordFromFile(cbServerName.Text);

  if Rec.ServerName <> '' then
  begin
    ApplyServerRecordToForm(Rec);
  end
  else
  begin
    edtServerAlias.Text := '';
    edtUserName.Text := 'SYSDBA';
    edtRole.Text := '';
    cbProtocol.ItemIndex := 1; // TCP
    edtPassword.Text := '';
    cbCharset.ItemIndex := DefaultFBCharacterSet;
    edtPort.Text := '3050';
    edtClientLibraryPath.Text := '';
    chkSavePassword.Checked := False;
  end;
end;

procedure TfmServerRegistry.ApplyServerRecordToForm(const Rec: TServerRecord);
begin
  cbServerName.Text        := Rec.ServerName;
  edtServerAlias.Text      := Rec.ServerAlias;
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

  edtPort.Text             := Rec.Port;
  edtClientLibraryPath.Text:= Rec.ClientLibraryPath;
  edtConfigFilePath.Text   := Rec.ConfigFilePath;
end;

{ OK → Speichern / Update in servers.reg }
procedure TfmServerRegistry.btnOKClick(Sender: TObject);
var
  Rec: TServerRecord;
  SavePwd: Boolean;
  tmpSession: TServerSession;
begin
  SavePwd := chkSavePassword.Checked;

  if SavePwd then
    SavePwd := SavePwd and (MessageDlg(
      'Warning: Your password will be stored unencrypted!' + sLineBreak +
      'Do you still want to save it?',
      mtWarning,
      [mbYes, mbNo],
      0
    ) = mrYes);

  // Session-Objekt bauen, damit BuildServerRecordFromSession arbeiten kann
  tmpSession := TServerSession.Create(
    cbServerName.Text,
    edtServerAlias.Text,
    edtUserName.Text,
    edtPassword.Text,
    edtRole.Text,
    TProtocol(cbProtocol.ItemIndex),
    edtClientLibraryPath.Text,
    edtConfigFilePath.Text,
    edtPort.Text,
    cbCharset.Text
  );
  try
    Rec := BuildServerRecordFromSession(tmpSession, SavePwd);
    SaveServerDataToFile(Rec);
  finally
    tmpSession.Free;
  end;

  ModalResult := mrOk;
end;

{ Button: ClientLibrary auswählen }
procedure TfmServerRegistry.btnClientLibClick(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := 'Firebird client library|*.dll;*.so|All files|*.*';
    if dlg.Execute then
    begin
      edtClientLibraryPath.Text := dlg.FileName;
      edtConfigFilePath.Text := GetFBConffilePathFromFBClientlibPath(edtClientLibraryPath.Text);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfmServerRegistry.FormShow(Sender: TObject);
var idx: Integer;
begin
  edtPort.Enabled := (cbProtocol.Items[cbProtocol.ItemIndex] <> 'Local');

  if cbServerName.Items.Count = 0 then
    exit;

  idx := cbServerName.Items.IndexOf(FSelectedServer);
  if idx >= 0 then
    cbServerName.ItemIndex := idx
  else
    cbServerName.ItemIndex := 0;

  cbServerNameChange(nil);
end;

end.

