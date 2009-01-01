unit CreateDb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  IBDatabase,
  turbocommon,
  uthemeselector;

type

  { TfmCreateDB }

  TfmCreateDB = class(TForm)
    bbCancel: TBitBtn;
    bbCreate: TBitBtn;
    bbReg: TBitBtn;
    btBrowse: TButton;
    cbCharset: TComboBox;
    chkboxReserveSpace: TCheckBox;
    chkboxForcedWrites: TCheckBox;
    cmbBoxServers: TComboBox;
    comboxPageSize: TComboBox;
    comboxSQLDialect: TComboBox;
    edtRole: TEdit;
    edPassword: TEdit;
    edtSweepInterval: TEdit;
    edNewDatabase: TEdit;
    edtServerType: TEdit;
    edtServerVersion: TEdit;
    edUserName: TEdit;
    grboxDatabase: TGroupBox;
    grboxServer: TGroupBox;
    grboxUser: TGroupBox;
    IBDatabase1: TIBDatabase;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    SaveDialog1: TSaveDialog;
    procedure bbCreateClick(Sender: TObject);
    procedure bbRegClick(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure cmbBoxServersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FUserName,
    FPassword,
    FCharSet,
    FClientLib,
    FDatabase,
    FPort: string;


  public
    { public declarations }
     procedure Init;
  end; 

var
  fmCreateDB: TfmCreateDB;

implementation

uses Reg;

{ TfmCreateDB }

procedure TfmCreateDB.btBrowseClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    edNewDatabase.Text:= SaveDialog1.FileName;
  end;
end;

procedure TfmCreateDB.cmbBoxServersChange(Sender: TObject);
begin
  Init;
end;

procedure TfmCreateDB.FormCreate(Sender: TObject);
begin
  // Load available character sets. Because we are not connected to a server,
  // we cannot retrieve the available charater sets.
  // Perhaps this can be done through the Services API but not high priority
  CbCharSet.Items.AddStrings(FBCharacterSets);
  CbCharSet.ItemIndex:=DefaultFBCharacterSet;
end;

procedure TfmCreateDB.FormShow(Sender: TObject);
var ServerRec: TServerRecord;
begin
  ServerRec := GetServerRecordFromFileByName(cmbBoxServers.Text);
  edUserName.Text := ServerRec.UserName;
  edPassword.Text := ServerRec.Password;

  frmThemeSelector.btnApplyClick(self);
end;

procedure  TfmCreateDB.Init;
var ServerRec: TServerRecord;
begin
  ServerRec := GetServerRecordFromFileByName(cmbBoxServers.Text);
  IBDatabase1.FirebirdLibraryPathName := ServerRec.ClientLibraryPath;

  if ServerRec.IsEmbedded then
    IBDatabase1.DatabaseName := edNewDatabase.Text
  else begin
    IBDatabase1.DatabaseName := cmbBoxServers.Text;
    if StrToIntDef(ServerRec.Port, 0) <> 0 then
      IBDatabase1.DatabaseName := IncludeTrailingPathDelimiter(IBDatabase1.DatabaseName) + ServerRec.Port + ':';
    IBDatabase1.DatabaseName := IBDatabase1.DatabaseName + edNewDatabase.Text;
  end;

  FPort := ServerRec.Port;

  cbCharset.ItemIndex := fmCreateDB.cbCharset.Items.IndexOf(ServerRec.Charset);

  IBDatabase1.Params.Clear;
  IBDatabase1.LoginPrompt := false;

  IBDatabase1.Params.Add('user_name=' + edUserName.Text);
  IBDatabase1.Params.Add('password=' + edPassword.Text);
  IBDatabase1.Params.Add('lc_ctype=' + cbCharset.Text);

  IBDatabase1.SQLDialect := StrToInt(Trim(comboxSQLDialect.Text));
  //IBDatabase1.Params.Add('page_size=' + Trim(comboxPageSize.Text));
  //IBDatabase1.Params.Add('sweep_interval=' + edtSweepInterval.Text);

  IBDatabase1.CreateIfNotExists := true;
end;

procedure TfmCreateDB.bbCreateClick(Sender: TObject);
begin
  bbReg.Enabled := false;
  try
    Init;

    IBDatabase1.Open;
    IBDatabase1.Connected := false;

    ShowMessage('Successfully created');
    bbReg.Enabled := true;
  except
    on E: Exception do
    begin
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

procedure TfmCreateDB.bbRegClick(Sender: TObject);
var cboxItems: TStringList;
    FileNameOnly: string;
begin
  try
    cboxItems := GetServerListFromTreeView;

    fmReg.cboxServers.Items.Assign(cboxItems);
    fmReg.cboxServers.ItemIndex := cmbBoxServers.Items.IndexOf(cmbBoxServers.Text);
    fmReg.edtPort.Text := FPort;

    fmReg.edtFBClient.Text := IBDatabase1.FirebirdLibraryPathName;

    FileNameOnly := edNewDatabase.Text;
    FileNameOnly := ExtractFileName(FileNameOnly);
    FileNameOnly := ChangeFileExt(FileNameOnly, '');
    fmReg.edTitle.Text := FileNameOnly;

    fmReg.edDatabaseName.Text:= IBDatabase1.DatabaseName;
    fmReg.edUserName.Text:= edUserName.Text;
    fmReg.edPassword.Text:= edPassword.Text;
    fmReg.cbCharset.Text:= cbCharset.Text;
    fmReg.NewReg:= True;
    ModalResult := fmReg.ShowModal;
  finally
    cboxItems.Free;
  end;
end;


initialization
  {$I createdb.lrs}

end.

