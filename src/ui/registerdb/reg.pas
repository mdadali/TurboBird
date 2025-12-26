unit Reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls, IBDatabase, IBDatabaseInfo,
  IB,

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
    edPassword: TEdit;
    edRole: TEdit;
    edtFBClient: TEdit;
    edTitle: TEdit;
    edtPort: TEdit;
    edUserName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    IBConnection1: TIBDatabase;
    IBDatabaseInfo1: TIBDatabaseInfo;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialogClient: TOpenDialog;
    Panel1: TPanel;
    procedure bbRegClick(Sender: TObject);
    procedure bbTestClick(Sender: TObject);
    procedure btBrowseDBClick(Sender: TObject);
    procedure btnBrowseClientClick(Sender: TObject);
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

    function TestConnection(DatabaseName, UserName, Password, Charset: string;
                FBClient: string; SQLDialect: string; Port: string; ServerName: string;
                OverwriteLoadedClientLib: boolean): Boolean;
    function GetEmptyRec: Integer;
    function SaveRegistrations: Boolean;
    procedure Sort;

    procedure RefreshServerCombobox;
  end;

var
  fmReg: TfmReg;

implementation

{ TfmReg }

procedure TfmReg.bbRegClick(Sender: TObject);
begin
  if Trim(edTitle.Text) = '' then
  begin
    ShowMessage('You should fill all fields');
    edTitle.SetFocus;
    Exit;
  end;

  if Trim(edPassword.Text) = '' then
    cxSavePassword.Checked := false;

  // Neue Registrierung
  if NewReg then
  begin
    if RegisterDatabase(edTitle.Text, edDatabaseName.Text, edUserName.Text, edPassword.Text, cbCharset.Text,
      edRole.Text, cxSavePassword.Checked, edtFBClient.Text, cboxSQLDialect.Text, edtPort.Text,
      cboxServers.Text, chkboxOverwriteServerClientLib.Checked, chkBoxConnectDBOnStart.Checked) then
      ModalResult := mrOK;
  end
  else
  // Bearbeiten einer bestehenden Registrierung
  begin
    if EditRegisteration(RecPos, edTitle.Text, edDatabaseName.Text, edUserName.Text, edPassword.Text,
           cbCharset.Text, edRole.Text, cxSavePassword.Checked, edtFBClient.Text, cboxSQLDialect.Text, edtPort.Text,
           cboxServers.Text, chkboxOverwriteServerClientLib.Checked, chkBoxConnectDBOnStart.Checked) then
      ModalResult := mrOK;
  end;
end;

procedure TfmReg.bbTestClick(Sender: TObject);
begin
  if TestConnection(edDatabaseName.Text, edUserName.Text, edPassword.Text, cbCharset.Text, edtFBClient.Text,
                     cboxSQLDialect.Text, edtPort.Text, cboxServers.Text,
                     chkboxOverwriteServerClientLib.Checked) then
    ShowMessage('Connected successfully');
end;

procedure TfmReg.btBrowseDBClick(Sender: TObject);
var Rec: TServerRecord;
begin
  if OpenDialog1.Execute then
  begin
    Rec := GetServerRecordFromFileByName(cboxServers.Text);
    edDatabaseName.Text:=  OpenDialog1.FileName;
    if not Rec.IsEmbedded then
    begin
      edDatabaseName.Text := cboxServers.Text + '/'  + Rec.Port + ':' + edDatabaseName.Text;
    end;
  end;
end;

procedure TfmReg.btnBrowseClientClick(Sender: TObject);
begin
  if OpenDialogClient.Execute then
    edtFBClient.Text := OpenDialogClient.FileName;
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
var ServerList: TStringList;
    i: integer;
    tmpIdxTxt: string;
begin
  tmpIdxTxt := cboxServers.Text;

  ServerList := GetServerListFromTreeView;

  try
    cboxServers.Items.Clear;

    for i := 0 to ServerList.Count - 1 do
      cboxServers.Items.Add(ServerList[i]);
  finally
    ServerList.Free;
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
  frmThemeSelector.btnApplyClick(self);
end;

{function TfmReg.RegisterDatabase(Title, DatabaseName, UserName, Password, Charset, Role: string; SavePassword: Boolean;
             FBClient: string; SQLDialect: string; Port: string; ServerName: string;
             OverwriteLoadedClientLib: boolean; ConnectOnApplicationStart: boolean): Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  EmptyIndex: Integer;
  FileName: string;
  ServerRec: TServerRecord;
begin
  try
    ServerRec := GetServerRecordFromFileByName(cboxServers.Items[cboxServers.ItemIndex]);
    FileName:= GetConfigurationDirectory + DatabasesRegFile;

    AssignFile(F, FileName);
    if FileExists(FileName) then
    begin
      EmptyIndex:= GetEmptyRec;
      FileMode:= 2;

      Reset(F);
      if EmptyIndex <> -1 then
        Seek(F, EmptyIndex)
      else
        Seek(F, System.FileSize(F));
    end
    else
      Rewrite(F);

    Rec.Title:= Title;
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
    Rec.LastOpened:= Now;
    Rec.FireBirdClientLibPath := FBClient;
    Rec.SQLDialect :=  SQLDialect;
    Rec.Port := Port;
    Rec.ServerName := ServerName;
    Rec.OverwriteLoadedClientLib := OverwriteLoadedClientLib;
    Rec.ConnectOnApplicationStart := ConnectOnApplicationStart;

    Rec.ServerVersionString := ServerRec.VersionString;
    Rec.ServerVersionMajor  := ServerRec.VersionMajor;
    Rec.ServerVersionMinor  := ServerRec.VersionMinor;

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
end; }

function TfmReg.RegisterDatabase(
  Title, DatabaseName, UserName, Password, Charset, Role: string;
  SavePassword: Boolean;
  FBClient: string; SQLDialect: string; Port: string; ServerName: string;
  OverwriteLoadedClientLib: boolean;
  ConnectOnApplicationStart: boolean
): Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;
  ServerRec: TServerRecord;
begin
  Result := False;

  try
    // Server-Infos laden (wie bisher)
    ServerRec := GetServerRecordFromFileByName(
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

    // Record füllen
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

    // Server-Version übernehmen
    Rec.ServerVersionString := ServerRec.VersionString;
    Rec.ServerVersionMajor  := ServerRec.VersionMajor;
    Rec.ServerVersionMinor  := ServerRec.VersionMinor;

    // Schreiben
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

function TfmReg.EditRegisteration(Index: Integer; Title, DatabaseName, UserName, Password, Charset, Role: string;
   SavePassword: Boolean; FBClient: string; SQLDialect: string; Port: string; ServerName: string;
   OverwriteLoadedClientLib: boolean; ConnectOnApplicationStart: boolean): Boolean;
var
  Rec: TRegisteredDatabase;
  F: file of TRegisteredDatabase;
  FileName: string;

  ServerRec: TServerRecord;
begin
  result := false;
  try
    ServerRec := GetServerRecordFromFileByName(cboxServers.Items[cboxServers.ItemIndex]);
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

function TfmReg.TestConnection(DatabaseName, UserName, Password, Charset: string; FBClient: string;
                                 SQLDialect: string; Port: string; ServerName: string;
                                 OverwriteLoadedClientLib: boolean): Boolean;

var ServerErrStr: string;
begin
  result := false;

  if not IsServerReachable(ServerName, ServerErrStr) then
  begin
    MessageDlg(ServerErrStr, mtError, [mbOK], 0);
    Exit;
  end;

  try
    IBConnection1.Close;
    IBConnection1.DatabaseName:= DatabaseName;

    IBConnection1.LoginPrompt := false;

    IBConnection1.FirebirdLibraryPathName := FBClient;
    with IBConnection1.Params do
    begin
      Clear;
      Add('user_name=' + UserName);
      Add('password='  + Password);
      Add('lc_ctype='  + Charset);
      Add('sql_dialect='  + SQLDialect);
    end;

    IBConnection1.Open;
    if IBConnection1.Connected then
    begin
      Result:= True;
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
      Result:= False;
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
    //Sort;
    FileName:= GetConfigurationDirectory + DatabasesRegFile;

    AssignFile(F, FileName);
    FileMode:= 2;
    Rewrite(F);

    for i:= 0 to High(RegisteredDatabases) do
      Write(F, RegisteredDatabases[i].OrigRegRec);
    CloseFile(F);
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
    end;
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

