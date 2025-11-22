unit dbInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, LCLType, ComCtrls, DBGrids, DBCtrls,
  IBDatabaseInfo, IBQuery, IBDatabase, IBCustomDataSet,

  ATBinHex, turbocommon, fbcommon,
  uthemeselector;

  //DBAdmin
  {MainFormUnit,
  DataModule,
  DBLoginDlgUnit,
  ShutdownDatabaseDlgUnit,
  ShutdownRegDlgUnit,
  BackupDlgUnit,
  RestoreDlgUnit,
  AddSecondaryFileDlgUnit,
  AddShadowFileDlgUnit,
  AddShadowSetDlgUnit,
  NewUserDlgUnit,
  ChgPasswordDlgUnit,
  ExecuteSQLScriptDlgUnit}
  //End-DBAdmin


type

  { TfmDBInfo }

  TfmDBInfo = class(TForm)
    ATBinHex1: TATBinHex;
    bbRefresh: TBitBtn;
    btnFirst: TButton;
    btnGoToPage: TButton;
    btnLast: TButton;
    btnNext: TButton;
    btnPrev: TButton;
    chkBoxEncrypted: TCheckBox;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    edDBSize: TEdit;
    edImplNo: TEdit;
    edImplNo1: TEdit;
    edServerTime: TEdit;
    edtAllocation: TEdit;
    edtBaseLevel: TEdit;
    edtCharset: TEdit;
    edtCreationDate: TEdit;
    edtCurrentMemory: TEdit;
    edtCurrentPage: TEdit;
    edtDBName: TEdit;
    edtDBSiteName: TEdit;
    edtEncKeyName: TEdit;
    edtFetches: TEdit;
    edtForcedWrites: TEdit;
    edtFreePages: TEdit;
    edtMarks: TEdit;
    edtMaxMemory: TEdit;
    edtNumBuffers: TEdit;
    edtODSVer: TEdit;
    edtPageSize: TEdit;
    edtReads: TEdit;
    edtServerString: TEdit;
    edtSQLDialect: TEdit;
    edtSweepIntervall: TEdit;
    edtTransCount: TEdit;
    edtUsedPages: TEdit;
    edtWrites: TEdit;
    GroupBox1: TGroupBox;
    IBDBInfo: TIBDatabaseInfo;
    IBDB: TIBDatabase;
    IBQry: TIBQuery;
    IBTrans: TIBTransaction;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label38: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    memoUserNames: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel7: TPanel;
    Splitter1: TSplitter;
    tsAttachments: TTabSheet;
    tsConfig: TTabSheet;
    tsInfos: TTabSheet;
    tsPages: TTabSheet;
    procedure bbRefreshClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnGoToPageClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure tsAttachmentsExit(Sender: TObject);
    procedure tsAttachmentsShow(Sender: TObject);
    procedure tsPagesShow(Sender: TObject);
  private
    { private declarations }
    FDBIndex: Integer;
    FNodeInfos: TPNodeInfos;

    procedure LoadBinHexFromDatabasePage(APageNr: integer);

    procedure SetSQLQuery;

    function OpenDB: Boolean;
    function CloseDB: Boolean;
  public
    procedure Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
    procedure FetchDatabaseInfo;


    { public declarations }
  end; 

//var
  //fmDBInfo: TfmDBInfo;

{var   IBDB:     TIBDatabase;
      IBDBInfo: TIBDatabaseInfo;
      IBTrans:  TIBTransaction;
      IBQry:    TIBQuery;}

implementation

{$R *.lfm}

{ TfmDBInfo }

uses SysTables;


procedure TfmDBInfo.btnFirstClick(Sender: TObject);
begin
  edtCurrentPage.Text := '0';
  LoadBinHexFromDatabasePage(0);
end;

procedure TfmDBInfo.btnLastClick(Sender: TObject);
begin
  edtCurrentPage.Text := IntToStr(IBDBInfo.Allocation -1);
  LoadBinHexFromDatabasePage(IBDBInfo.Allocation -1);
end;

procedure TfmDBInfo.btnNextClick(Sender: TObject);
begin
  if  StrToIntDef(edtCurrentPage.Text, IBDBInfo.Allocation -1) = (IBDBInfo.Allocation -1) then
    exit;
  edtCurrentPage.Text := IntToStr(StrToIntDef(edtCurrentPage.Text, 0) + 1);
  LoadBinHexFromDatabasePage(StrToIntDef(edtCurrentPage.Text, 0));
end;

procedure TfmDBInfo.btnPrevClick(Sender: TObject);
begin
  if StrToIntDef(edtCurrentPage.Text, 0) = 0 then
    exit;
  edtCurrentPage.Text := IntToStr(StrToIntDef(edtCurrentPage.Text, 0) - 1);
  LoadBinHexFromDatabasePage(StrToIntDef(edtCurrentPage.Text, 0));
end;

procedure TfmDBInfo.btnGoToPageClick(Sender: TObject);
var PageIndex: integer;
begin
  PageIndex := StrToIntDef(edtCurrentPage.Text, 0);

  if PageIndex > IBDBInfo.Allocation -1 then
    PageIndex := IBDBInfo.Allocation -1;

  LoadBinHexFromDatabasePage(PageIndex);
  edtCurrentPage.Text := IntToStr(PageIndex);
end;

procedure TfmDBInfo.FormActivate(Sender: TObject);
begin

end;

procedure TfmDBInfo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((key=VK_F4) or (key=VK_W)) then
  begin
    // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
    Close;
    Parent.Free;
  end;
end;

procedure TfmDBInfo.FormShow(Sender: TObject);
//var MainForm: TMainForm;
begin
  frmThemeSelector.btnApplyClick(self);

  //DBDataModule.IBXServicesConnection1.FirebirdLibraryPathName := RegisteredDatabases[self.FDBIndex].IBDatabase.FirebirdLibraryPathName;

  {//Application.CreateForm(TMainForm, MainForm);
  //
  MainForm := TMainForm.Create(ScrollBox1);
  MainForm.Parent := ScrollBox1;

  Application.CreateForm(TDBDataModule, DBDataModule);
  Application.CreateForm(TDBLoginDlg, DBLoginDlg);
  Application.CreateForm(TShutdownDatabaseDlg, ShutdownDatabaseDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TAddSecondaryFileDlg, AddSecondaryFileDlg);
  Application.CreateForm(TAddShadowFileDlg, AddShadowFileDlg);
  Application.CreateForm(TAddShadowSetDlg, AddShadowSetDlg);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TExecuteSQLScriptDlg, ExecuteSQLScriptDlg);
  MainForm.Align := alClient;

  }

  {if DBDataModule.IBDatabase1.Connected then
    DBDataModule.Disconnect;
  DBDataModule.IBDatabase1.DatabaseName := RegisteredDatabases[FDBIndex].IBDatabase.DatabaseName;
  DBDataModule.IBDatabase1.Params.Clear;
  DBDataModule.IBDatabase1.Params.Add('user_name=' + RegisteredDatabases[FDBIndex].RegRec.UserName);
  DBDataModule.IBDatabase1.Params.Add('password=' + RegisteredDatabases[FDBIndex].RegRec.Password);
  MainForm.Show; //Modal;
  //MainFormUnit.MainForm.Show; //Modal;
  }
end;

procedure TfmDBInfo.tsAttachmentsExit(Sender: TObject);
begin
  if IBQry.Active then
    IBQry.Close;
end;

procedure TfmDBInfo.tsAttachmentsShow(Sender: TObject);
begin
  if IBQry.Active then
    IBQry.Close;
  //SetSQLQuery;
  IBQry.Open;
end;

procedure TfmDBInfo.tsPagesShow(Sender: TObject);
begin
  LoadBinHexFromDatabasePage(0);
end;

procedure TfmDBInfo.LoadBinHexFromDatabasePage(APageNr: integer);
var
  DatabasePageString: string;
  MemoryStream: TMemoryStream;
begin
  try
    DatabasePageString := IBDBInfo.GetDatabasePage(APageNr);
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.Write(PChar(DatabasePageString)^, Length(DatabasePageString));
      MemoryStream.Position := 0;
      ATBinHex1.OpenStream(MemoryStream);
    finally
    MemoryStream.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error while loading binary data: ' + E.Message);
  end;
end;

procedure GetReadableDBSize(var dbSize: double; var AType: string);
begin
  // Display database size in readable format
  if dbSize > (1024*1024*1024) then
  begin
    dbSize:= ((dbSize / 1024) / 1024) / 1024;
    AType:= 'Gigabytes';
  end
  else
  if dbSize > (1024*1024) then
  begin
    dbSize:= ((dbSize / 1024) / 1024);
    AType:= 'Megabytes';
  end
  else
  if dbSize > 1024 then
  begin
    dbSize:= (dbSize / 1024);
    AType:= 'Kilobytes';
  end
  else
  begin
    AType:= 'Bytes';
  end;
end;

procedure TfmDBInfo.bbRefreshClick(Sender: TObject);
begin
  if IBDB.Connected then
    Self.CloseDB;

  //AssignIBDatabase(RegisteredDatabases[dbIndex].IBDatabase, IBDB);

  Self.OpenDB;

  FetchDatabaseInfo;
end;

procedure TfmDBInfo.FetchDatabaseInfo;
var tmpSize: Double;
    AType: string;
    ServerVersionMajor: word;
begin
  edtServerString.Text := IBDBInfo.FirebirdVersion;

  if Length(IBDBInfo.FirebirdVersion) >= 5 then
    ServerVersionMajor := StrToIntDef(IBDBInfo.FirebirdVersion[5], 0);

  edtDBSiteName.Text := IBDBInfo.DBSiteName;
  edtDBName.Text := IBDBInfo.DBFileName;

  edtODSVer.Text := IntToStr(IBDBInfo.ODSMajorVersion) + '.' + IntToStr(IBDBInfo.ODSMinorVersion);
  edtCharset.Text := IBDBInfo.Database.Params.Values['lc_ctype'];
  edtCreationDate.Text := DateTimeToStr(IBDBInfo.DateDBCreated);
  edtPageSize.Text := IntToStr(IBDBInfo.PageSize);

  edtAllocation.Text := IntToStr(IBDBInfo.Allocation);

  tmpSize := IBDBInfo.PageSize * IBDBInfo.Allocation;
  GetReadableDBSize(tmpSize, AType);
  edDBSize.Text:= Format('%3.1n %s', [tmpSize, AType]);

  if ServerVersionMajor >= 3 then
  begin
    edtUsedPages.Text := IntToStr(IBDBInfo.PagesUsed);
    edtFreePages.Text := IntToStr(IBDBInfo.PagesFree);
    edtEncKeyName.Text := IBDBInfo.EncryptionKeyName;
  end;

  edtSQLDialect.Text := IntToStr(IBDBInfo.DBSQLDialect);

  memoUserNames.Lines.Assign(IBDBInfo.UserNames);
  edtBaseLevel.Text := IntToStr(IBDBInfo.BaseLevel);
  edImplNo.Text := IntToStr(IBDBInfo.DBImplementationNo);
  edtFetches.Text := IntToStr(IBDBInfo.Fetches);
  edtMarks.Text := IntToStr(IBDBInfo.Marks);
  edtReads.Text := IntToStr(IBDBInfo.Reads);
  edtWrites.Text := IntToStr(IBDBInfo.Writes);

  tmpSize := IBDBInfo.CurrentMemory;
  GetReadableDBSize(tmpSize, AType);
  edtCurrentMemory.Text := Format('%3.1n %s', [tmpSize, AType]);

  tmpSize := IBDBInfo.MaxMemory;
  GetReadableDBSize(tmpSize, AType);
  edtMaxMemory.Text := Format('%3.1n %s', [tmpSize, AType]);

  edtForcedWrites.Text := IntToStr(IBDBInfo.ForcedWrites);
  edtNumBuffers.Text   := IntToStr(IBDBInfo.NumBuffers);
  edtSweepIntervall.Text := IntToStr(IBDBInfo.SweepInterval);
  edtTransCount.Text := IntToStr(IBDBInfo.TransactionCount);

  chkBoxEncrypted.Checked := IBDBInfo.Encrypted;


end;

procedure TfmDBInfo.FormCreate(Sender: TObject);
begin
  {IBDB     :=  TIBDatabase.Create(nil);
  IBDBInfo :=  TIBDatabaseInfo.Create(nil);
  IBTrans  :=  TIBTransaction.Create(nil);
  IBQry    :=  TIBQuery.Create(nil);

  IBDBInfo.Database := IBDB;
  IBTrans.DefaultDatabase := IBDB;
  IBDB.DefaultTransaction := IBTrans;
  IBQry.Database := IBDB;
  IBQry.Transaction := IBTrans;

  DataSource1.DataSet := IBQry;}
end;

procedure TfmDBInfo.SetSQLQuery;
begin
  IBQry.SQL.Text := 'SELECT * FROM MON$ATTACHMENTS';
end;

procedure TfmDBInfo.Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
begin
  if FNodeInfos <>  ANodeInfos then
    FNodeInfos := ANodeInfos;
  if  FDBIndex <> dbIndex then
    FDBIndex:= dbIndex;


  if IBQry.Active then
    IBQry.Close;

  //SetSQLQuery;

  if IBTrans.InTransaction then
   IBTrans.Rollback;

  if IBDB.Connected then
    CloseDB;

  AssignIBDatabase(RegisteredDatabases[dbIndex].IBDatabase, IBDB);
  IBDB.DefaultTransaction := IBTrans;

  OpenDB;

  FetchDatabaseInfo;
end;

{function TfmDBInfo.OpenDB: Boolean;
var Rec: TDatabaseRec;
begin
  Result := False;

  try
    if not Assigned(IBDB) then
      Exit;

    if not IBDB.Connected then
    begin
      try
        IBDB.Connected := True;
      except
        on E: Exception do
        begin
          ShowMessage('Failed to connect to database:'#13#10 + E.Message);
          Exit(False);
        end;
      end;
    end;

    if Assigned(IBTrans) and not IBTrans.InTransaction then
    begin
      try
        IBTrans.StartTransaction;
      except
        on E: Exception do
        begin
          ShowMessage('Failed to start transaction:'#13#10 + E.Message);
          Exit(False);
        end;
      end;
    end;

    if Assigned(IBDBInfo) then
      IBDBInfo.Database := IBDB;

    // Wenn alles geklappt hat:
    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage('Error opening database:'#13#10 + E.Message);
      Result := False;
    end;
  end;
end;}

function TfmDBInfo.OpenDB: Boolean;
var Rec: TRegisteredDatabase;
begin
  Result := False;

  try
    if not Assigned(IBDB) then
      Exit;

    Rec := RegisteredDatabases[FDBIndex].RegRec;

    if Rec.Password = '' then
      if not ConnectToDBAs(FDBIndex) then
        exit(false);

    Rec := RegisteredDatabases[FDBIndex].RegRec; // neeu einlesen

    IBDB.Params.Values['user_name'] := Rec.UserName;
    IBDB.Params.Values['password'] := Rec.Password;
    IBDB.LoginPrompt := false;
    IBDB.Connected := true;

    if Assigned(IBTrans) and not IBTrans.InTransaction then
    begin
      try
        IBTrans.StartTransaction;
      except
        on E: Exception do
        begin
          ShowMessage('Failed to start transaction:'#13#10 + E.Message);
          Exit(False);
        end;
      end;
    end;

    if Assigned(IBDBInfo) then
      IBDBInfo.Database := IBDB;

    // Wenn alles geklappt hat:
    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage('Error opening database:'#13#10 + E.Message);
      Result := False;
    end;
  end;
end;

function TfmDBInfo.CloseDB: Boolean;
begin
  Result := False;

  try
    if Assigned(IBQry) and IBQry.Active then
      IBQry.Close;

    if Assigned(IBTrans) and IBTrans.InTransaction then
    begin
      try
        IBTrans.Rollback;
      except
        // Fehler beim Rollback ignorieren, da beim Schließen
      end;
    end;

    if Assigned(IBDBInfo) then
      IBDBInfo.Database := nil;

    if Assigned(IBDB) and IBDB.Connected then
    begin
      try

        //Bug???///////////////////////////////////////////
        if StrToIntDef(edtServerString.Text[5], 0) > 3 then
          if Assigned(IBDB.Attachment) then
          begin
            //IBDB.Attachment := nil;  //?????
          end;
        ///////////////////////////////////////////////////


        IBDB.Connected := False;
      except
        on E: Exception do
        begin
          // Fehler beim Disconnect
          Exit(False);
        end;
      end;
    end;

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

procedure TfmDBInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;

  self.CloseDB;
  CloseAction := caFree;
end;

end.

