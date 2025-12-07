(*
 * MainFormUnit.pas
 * Copyright (C) 2018 Tony Whyman <tony@mwasoftware.co.uk>
 *
 * DBAdmin is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * DBAdmin is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, SynGutterCodeFolding,
  Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, ActnList, StdCtrls,
  DbCtrls, ExtCtrls, Buttons, db, IBLookupComboEditBox, IBDynamicGrid,
  IBTreeView, IBDatabaseInfo, IBXServices, IBExtract, IB,

  //DBAdmin
  //MainFormUnit,
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
  ExecuteSQLScriptDlgUnit,
  //End-DBAdmin


  turbocommon,
  fServerSession;

type

  { TMainForm }

  TMainForm = class(TForm)
    AccessRightsPopup: TPopupMenu;
    AccessRightsSource: TDataSource;
    SchemaMenuItem22 : TMenuItem;
    SchemaMenuItem23 : TMenuItem;
    SchemaMenuItem24 : TMenuItem;
    SchemaPopup : TPopupMenu;
    SaveSchema : TAction;
    SchemaSelectAll : TAction;
    ClearSchema : TAction;
    CopyText : TAction;
    ClientLibrary: TLabel;
    ConfigDataGrid: TIBDynamicGrid;
    ConfigDataLabel: TLabel;
    ConfigDataSource: TDataSource;
    Edit12: TEdit;
    DBDateCreated: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    IncludeUserGrants : TCheckBox;
    Label44: TLabel;
    ClientServerVersion: TMemo;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    OpenDatabasesList: TMemo;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    Panel10: TPanel;
    SchemaPanel : TPanel;
    RunScript: TAction;
    AutoAdmin: TCheckBox;
    DatabaseAliasName: TEdit;
    DBEdit5: TDBEdit;
    DBEdit6: TDBEdit;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    DBComments: TMemo;
    MenuItem19: TMenuItem;
    RevokeAll: TAction;
    AuthMapSource: TDataSource;
    SubjectAccessRightsSource: TDataSource;
    DBTablesSource: TDataSource;
    IBDynamicGrid5: TIBDynamicGrid;
    SubjectAccessRightsGrid: TIBDynamicGrid;
    AccessRightsTreeView: TIBTreeView;
    SelectAllTables: TCheckBox;
    Label40: TLabel;
    SelectedTablesGrid: TIBDynamicGrid;
    Label37: TLabel;
    Panel8: TPanel;
    DBTablesPanel: TPanel;
    Panel9: TPanel;
    Phase2Repair: TAction;
    ApplySelected: TAction;
    AuthMethLabel: TLabel;
    AuthMethod: TDBEdit;
    Button7: TButton;
    IgnoreChecksumsOnRepair: TCheckBox;
    DBTablesSplitter: TSplitter;
    Alltables: TRadioButton;
    SelectedTablesOnly: TRadioButton;
    MappingsTab: TTabSheet;
    AccessRightsTab: TTabSheet;
    Splitter5: TSplitter;
    SynEdit1 : TSynEdit;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    UpdateColsPanel: TPanel;
    ValidateRepairRecordFragments: TCheckBox;
    IgnoreChecksums: TCheckBox;
    Label34: TLabel;
    ReadOnlyValidation: TCheckBox;
    RecordFragments: TCheckBox;
    Commit2Phase: TAction;
    DBOwner: TEdit;
    AttmntODS12Panel: TPanel;
    Label33: TLabel;
    RepairOptionsTab: TTabSheet;
    ValidateOptions: TPageControl;
    RemoteOSLabel: TLabel;
    RemoteOSUser: TDBEdit;
    SecDatabase: TEdit;
    RollbackAll: TAction;
    CommitAll: TAction;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    IBDynamicGrid3: TIBDynamicGrid;
    Label38: TLabel;
    Label39: TLabel;
    LimboListSource: TDataSource;
    LimboReport: TMemo;
    RunRepair: TAction;
    Button2: TButton;
    SelectRepairAction: TComboBox;
    DisconnectAttachment: TAction;
    LimboTab: TTabSheet;
    ValidateOptionsTab: TTabSheet;
    MenuItem17: TMenuItem;
    AttmtPopup: TPopupMenu;
    MenuItem18: TMenuItem;
    RepairTab: TTabSheet;
    ToggleAutoRefresh: TAction;
    AttachSource: TDataSource;
    DBCheckBox1: TDBCheckBox;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit7: TDBEdit;
    DeleteTag: TAction;
    AddTag: TAction;
    AttmtGrid: TIBDynamicGrid;
    Label31: TLabel;
    Label32: TLabel;
    Label35: TLabel;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    Panel7: TPanel;
    AttDetailsPanel: TPanel;
    UserPopup: TPopupMenu;
    UserTagPopup: TPopupMenu;
    SaveChanges: TAction;
    DeleteUser: TAction;
    ChgPassword: TAction;
    AddUser: TAction;
    AddFileBtn: TButton;
    AddShadowBtn: TButton;
    AllocatedPages: TEdit;
    Button1: TButton;
    DatabaseOnline: TCheckBox;
    DBCharacterSet: TIBLookupComboEditBox;
    DBCharSetRO: TDBEdit;
    DBEdit4: TDBEdit;
    DBIsReadOnly: TCheckBox;
    DBText1: TDBText;
    DropDatabase: TAction;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    PageBuffers: TEdit;
    ODSVersionString: TEdit;
    ServerVersionNo: TEdit;
    DBSQLDialect: TEdit;
    ConnectString: TEdit;
    UserManagerTab: TTabSheet;
    FilesTab: TTabSheet;
    IBDynamicGrid1: TIBDynamicGrid;
    IBDynamicGrid2: TIBDynamicGrid;
    AttmtTimer: TTimer;
    UserManagerGrid: TIBDynamicGrid;
    RolesGrid: TIBDynamicGrid;
    TagsGrid: TIBDynamicGrid;
    IsShadowChk: TCheckBox;
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
    DateDBCreatedLabel: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label36: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LingerDelay: TEdit;
    MenuItem10: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem8: TMenuItem;
    NoReserve: TCheckBox;
    PageControl1: TPageControl;
    PagesAvail: TEdit;
    PagesUsed: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    RolesHeaderPanel: TPanel;
    TagsHeader: TPanel;
    PrimaryDBFile: TEdit;
    Properties: TTabSheet;
    RemoveShadowBtn: TButton;
    RoleSource: TDataSource;
    Save: TAction;
    IBExtract1: TIBExtract;
    SaveDialog: TSaveDialog;
    SchemaTab: TTabSheet;
    ServerLog: TMemo;
    ServerTab: TTabSheet;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    SQlSaveDialog: TSaveDialog;
    RemoveShadow: TAction;
    AddShadowSet: TAction;
    AddSecondary: TAction;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDatabase: TAction;
    CharSetSource: TDataSource;
    DatabaseSource: TDataSource;
    AttmtSource: TDataSource;
    DBCharSetSource: TDataSource;
    IBDatabaseInfo: TIBDatabaseInfo;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Restore: TAction;
    Backup: TAction;
    MenuImages: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Quit: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    SecDBFilesSource: TDataSource;
    ShadowSource: TDataSource;
    StatisticsTab: TTabSheet;
    StatsMemo: TMemo;
    StatsOptions: TComboBox;
    StatusBar1: TStatusBar;
    SweepInterval: TEdit;
    SyncWrites: TCheckBox;
    SynSQLSyn1: TSynSQLSyn;
    AttachmentsTab: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UserListSource: TDataSource;
    UserTagsSource: TDataSource;
    ValidationReport: TMemo;
    procedure AccessRightsTabHide(Sender: TObject);
    procedure AccessRightsTabShow(Sender: TObject);
    procedure AddSecondaryExecute(Sender: TObject);
    procedure AddShadowSetExecute(Sender: TObject);
    procedure AddTagExecute(Sender: TObject);
    procedure AddTagUpdate(Sender: TObject);
    procedure AddUserExecute(Sender: TObject);
    procedure AddUserUpdate(Sender: TObject);
    procedure ApplySelectedExecute(Sender: TObject);
    procedure AttachmentsTabHide(Sender: TObject);
    procedure AttachmentsTabShow(Sender: TObject);
    procedure AttmtTimerTimer(Sender: TObject);
    procedure AutoAdminChange(Sender: TObject);
    procedure BackupExecute(Sender: TObject);
    procedure ChgPasswordExecute(Sender: TObject);
    procedure ChgPasswordUpdate(Sender: TObject);
    procedure ClearSchemaExecute(Sender : TObject);
    procedure Commit2PhaseExecute(Sender: TObject);
    procedure CommitAllExecute(Sender: TObject);
    procedure CommitAllUpdate(Sender: TObject);
    procedure CopyTextExecute(Sender : TObject);
    procedure CopyTextUpdate(Sender : TObject);
    procedure DatabaseOnlineChange(Sender: TObject);
    procedure DBCharacterSetEditingDone(Sender: TObject);
    procedure DBCommentsEditingDone(Sender: TObject);
    procedure DBIsReadOnlyChange(Sender: TObject);
    procedure DBSQLDialectEditingDone(Sender: TObject);
    procedure DeleteTagExecute(Sender: TObject);
    procedure DeleteTagUpdate(Sender: TObject);
    procedure DeleteUserExecute(Sender: TObject);
    procedure DeleteUserUpdate(Sender: TObject);
    procedure DisconnectAttachmentExecute(Sender: TObject);
    procedure DisconnectAttachmentUpdate(Sender: TObject);
    procedure DropDatabaseExecute(Sender: TObject);
    procedure DropDatabaseUpdate(Sender: TObject);
    procedure AccessRightsTreeViewSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MappingsTabHide(Sender: TObject);
    procedure MappingsTabShow(Sender: TObject);
    procedure PageBuffersEditingDone(Sender: TObject);
    procedure QuitExecute(Sender: TObject);
    procedure RepairTabHide(Sender: TObject);
    procedure RepairTabShow(Sender: TObject);
    procedure RevokeAllExecute(Sender: TObject);
    procedure RevokeAllUpdate(Sender: TObject);
    procedure RunScriptExecute(Sender: TObject);
    procedure SaveSchemaExecute(Sender : TObject);
    procedure SchemaSelectAllExecute(Sender : TObject);
    procedure SelectAllTablesChange(Sender: TObject);
    procedure SelectedTablesOnlyChange(Sender: TObject);
    procedure SelectRepairActionCloseUp(Sender: TObject);
    procedure SubjectAccessRightsSourceDataChange(Sender: TObject; Field: TField
      );
    procedure ToolButton8Click(Sender: TObject);
    procedure UserManagerTabHide(Sender: TObject);
    procedure UserManagerTabShow(Sender: TObject);
    procedure FilesTabShow(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IsShadowChkChange(Sender: TObject);
    procedure LimboTabHide(Sender: TObject);
    procedure LimboTabShow(Sender: TObject);
    procedure LingerDelayEditingDone(Sender: TObject);
    procedure NoReserveChange(Sender: TObject);
    procedure OpenDatabaseExecute(Sender: TObject);
    procedure PropertiesShow(Sender: TObject);
    procedure RemoveShadowExecute(Sender: TObject);
    procedure RemoveShadowUpdate(Sender: TObject);
    procedure RestoreExecute(Sender: TObject);
    procedure RollbackAllExecute(Sender: TObject);
    procedure RunRepairExecute(Sender: TObject);
    procedure SaveChangesExecute(Sender: TObject);
    procedure SaveChangesUpdate(Sender: TObject);
    procedure SaveExecute(Sender: TObject);
    procedure SaveUpdate(Sender: TObject);
    procedure SchemaTabShow(Sender: TObject);
    procedure ServerTabHide(Sender: TObject);
    procedure ServerTabShow(Sender: TObject);
    procedure StatisticsTabHide(Sender: TObject);
    procedure StatisticsTabShow(Sender: TObject);
    procedure StatsOptionsCloseUp(Sender: TObject);
    procedure SweepIntervalEditingDone(Sender: TObject);
    procedure SyncWritesChange(Sender: TObject);
    procedure ToggleAutoRefreshExecute(Sender: TObject);
    procedure ToggleAutoRefreshUpdate(Sender: TObject);
  private
    FDBIndex: Integer;
    FNodeInfos: TPNodeInfos;

    FLoading: boolean;
    FLastStatsIndex: integer;
    FServerError: boolean;
    procedure DoDBOpen(Data: PtrInt);
    procedure LoadServerData;
    procedure DoExtract(Data: PtrInt);
    procedure ConfigureOnlineValidation;
  protected
    procedure HandleDBConnect(Sender: TObject);
    procedure HandleLoadData(Sender: TObject);
    procedure ConfigureForServerVersion; virtual;
    procedure ConnectToDatabase; virtual;
    procedure LoadData; virtual;
  public
    procedure Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
  end;

var
  MainForm: TMainForm;

{var
    DBDataModule: TDBDataModule;
    DBLoginDlg: TDBLoginDlg;
    ShutdownDatabaseDlg: TShutdownDatabaseDlg;
    ShutdownReqDlg: TShutdownReqDlg;
    BackupDlg: TBackupDlg;
    RestoreDlg: TRestoreDlg;
    AddSecondaryFileDlg: TAddSecondaryFileDlg;
    AddShadowFileDlg: TAddShadowFileDlg;
    AddShadowSetDlg: TAddShadowSetDlg;
    NewUserDlg: TNewUserDlg;
    ChgPasswordDlg: TChgPasswordDlg;
    ExecuteSQLScriptDlg: TExecuteSQLScriptDlg;
 }

implementation

uses Main;

{$R *.lfm}

{uses DataModule, ShutdownRegDlgUnit, AddSecondaryFileDlgUnit, NewUserDlgUnit,
  ChgPasswordDlgUnit, IBMessages, ExecuteSQLScriptDlgUnit;
 }
{ TMainForm }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DBDataModule.Disconnect;
end;

procedure TMainForm.Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
var DBRec: TRegisteredDatabase;
    ServerRec: TServerRecord;
begin
  if FNodeInfos <>  ANodeInfos then
    FNodeInfos := ANodeInfos;
  if  FDBIndex <> dbIndex then
    FDBIndex:= dbIndex;


  if DBDataModule.IBDatabase1.Connected then
    DBDataModule.Disconnect;

  DBRec := RegisteredDatabases[dbIndex].RegRec;

  if DBRec.Password = '' then
    if not ConnectToDBAs(dbIndex) then
      exit;

  DBRec := RegisteredDatabases[dbIndex].RegRec; // neu einlesen;

  if DBDataModule.IBDatabase1.Connected then
    DBDataModule.IBDatabase1.Connected := false;

  if DBRec.OverwriteLoadedClientLib then
    DBDataModule.IBDatabase1.FirebirdLibraryPathName :=  DBRec.FireBirdClientLibPath
  else begin
    ServerRec := GetServerRecordFromFileByName(DBRec.ServerName);
    DBDataModule.IBDatabase1.FirebirdLibraryPathName := ServerRec.ClientLibraryPath;
  end;

  DBDataModule.IBDatabase1.Params.Clear;
  DBDataModule.IBDatabase1.DatabaseName := DBRec.DatabaseName;
  DBDataModule.IBDatabase1.Params.Add('user_name=' + DBRec.UserName);
  DBDataModule.IBDatabase1.Params.Add('password=' + DBRec.Password);
  //DBDataModule.IBDatabase1.LoginPrompt := false;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  {Set IB Exceptions to only show text message - omit SQLCode and Engine Code}
  DBDataModule.IBDatabase1.FirebirdAPI.GetStatus.SetIBDataBaseErrorMessages([ShowIBMessage]);
  Application.ExceptionDialog := aedOkMessageBox;
  PageControl1.ActivePage := Properties;
  DBDataModule.AfterDBConnect := @HandleDBConnect;
  DBDataModule.AfterDataReload := @HandleLoadData;
  AccessRightsTreeView.DataSource := nil;
  AccessRightsTreeView.DataSource := AccessRightsSource;
  SubjectAccessRightsGrid.DataSource := nil;
  SubjectAccessRightsGrid.DataSource := SubjectAccessRightsSource;
  PageControl1.Visible := false;
  Application.QueueAsyncCall(@DoDBOpen,0);
end;

procedure TMainForm.IsShadowChkChange(Sender: TObject);
begin
  if FLoading then Exit;
  if not DBDataModule.IsShadowDatabase then
  begin
    MessageDlg('A Normal Database cannot be changed into a Shadow Database',mtError,[mbOK],0);
    FLoading := true;
    try
      IsShadowChk.Checked := false;
    finally
      FLoading := false;
    end;
  end
  else
    DBDataModule.ActivateShadow;
end;

procedure TMainForm.LimboTabHide(Sender: TObject);
begin
  LimboListSource.DataSet.Active := false;
end;

procedure TMainForm.LimboTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  LimboListSource.DataSet.Active := true;
end;

procedure TMainForm.LingerDelayEditingDone(Sender: TObject);
begin
  if FLoading then Exit;
  DBDataModule.LingerDelay := LingerDelay.Text;
end;

procedure TMainForm.NoReserveChange(Sender: TObject);
begin
  if FLoading then Exit;
  DBDataModule.NoReserve := NoReserve.Checked;
end;

procedure TMainForm.DBCharacterSetEditingDone(Sender: TObject);
begin
  with DBCharSetSource.Dataset do
  if State = dsEdit then
    Post;
end;

procedure TMainForm.DBCommentsEditingDone(Sender: TObject);
begin
  DBDataModule.Description := DBComments.Lines.Text;
end;

procedure TMainForm.AutoAdminChange(Sender: TObject);
begin
  if FLoading then Exit;
  try
    DBDataModule.AutoAdmin := AutoAdmin.Checked;
  except on E:Exception do
   begin
    MessageDlg(E.message,mtError,[mbOK],0);
     FLoading := true;
     try
       AutoAdmin.Checked := not AutoAdmin.Checked;
     finally
       FLoading := false;
     end;
   end;
  end;
end;

procedure TMainForm.AddSecondaryExecute(Sender: TObject);
var FileName: string;
    StartAt: integer;
    FileLength: integer;
    Pages: boolean;
begin
  StartAt := 0;
  if DBDataModule.IsDatabaseOnline then
  begin
    MessageDlg('The database must be shutdown before adding secondary files',
                 mtError,[mbOK],0);
    exit;
  end;

  if AddSecondaryFileDlg.ShowModal(FileName,StartAt,FileLength,Pages) = mrOK then
  begin
    if not Pages then
    begin
      StartAt := StartAt*1024*1024 div IBDatabaseInfo.PageSize;
      if FileLength <> -1 then
        FileLength := FileLength*1024*1024 div IBDatabaseInfo.PageSize;
    end;
    DBDataModule.AddSecondaryFile(FileName,StartAt,FileLength);
  end;
end;

procedure TMainForm.AccessRightsTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  UserListSource.DataSet.Active := true;
  AccessRightsSource.DataSet.Active := true;
  AccessRightsTreeViewSelectionChanged(nil);
end;

procedure TMainForm.AccessRightsTabHide(Sender: TObject);
begin
  SubjectAccessRightsSource.DataSet.Active := false;
  AccessRightsSource.DataSet.Active := false;
  UserListSource.DataSet.Active := PageControl1.ActivePage = UserManagerTab;
end;

procedure TMainForm.AddShadowSetExecute(Sender: TObject);
begin
  DBDataModule.AddShadowSet;
end;

procedure TMainForm.AddTagExecute(Sender: TObject);
begin
  UserTagsSource.DataSet.Append;
end;

procedure TMainForm.AddTagUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (UserTagsSource.State = dsBrowse);
end;

procedure TMainForm.AddUserExecute(Sender: TObject);
var NewUserName: string;
    NewPassword: string;
begin
  NewUserName := '';
  if NewUserDlg.ShowModal(NewUserName,NewPassword) = mrOK then
  with UserListSource.DataSet do
  begin
    Append;
    FieldByName('SEC$USER_NAME').AsString := AnsiUpperCase(NewUserName);
    FieldByName('SEC$PASSWORD').AsString := NewPassword;
  end;
end;

procedure TMainForm.AddUserUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (UserListSource.State = dsBrowse) and
     ((DBDataModule.ServiceUserName = 'SYSDBA') or DBDataModule.HasUserAdminPrivilege);
end;

procedure TMainForm.ApplySelectedExecute(Sender: TObject);
begin
  DBDataModule.LimboResolution(NoGlobalAction,LimboReport.Lines);
end;

procedure TMainForm.AttachmentsTabHide(Sender: TObject);
begin
  AttachSource.DataSet.Active := false;
  AttmtTimer.Enabled := false;
end;

procedure TMainForm.AttachmentsTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  AttachSource.DataSet.Active := true;
  AttmtGrid.ShowEditorPanel; {assume located at current connection}
end;

procedure TMainForm.AttmtTimerTimer(Sender: TObject);
begin
  DBDataModule.CurrentTransaction.Commit; {force a refresh}
end;

procedure TMainForm.ChgPasswordExecute(Sender: TObject);
var NewPassword: string;
begin
  NewPassword := '';
  if ChgPasswordDlg.ShowModal(NewPassword) = mrOK then
  with UserListSource.DataSet do
  begin
    Edit;
    FieldByName('SEC$PASSWORD').AsString := NewPassword;
    try
      Post
    except
      Cancel;
      raise;
    end;
  end;
end;

procedure TMainForm.ChgPasswordUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserListSource.DataSet.Active and (UserListSource.DataSet.RecordCount > 0);
end;

procedure TMainForm.ClearSchemaExecute(Sender : TObject);
begin
  Synedit1.Clear;
end;

procedure TMainForm.Commit2PhaseExecute(Sender: TObject);
begin
  DBDataModule.LimboResolution(RecoverTwoPhaseGlobal,LimboReport.Lines);
end;

procedure TMainForm.CommitAllExecute(Sender: TObject);
begin
  DBDataModule.LimboResolution(CommitGlobal,LimboReport.Lines);
end;

procedure TMainForm.CommitAllUpdate(Sender: TObject);
begin
  with LimboListSource.DataSet do
  (Sender as TAction).Enabled := Active and (RecordCount > 0);
end;

procedure TMainForm.CopyTextExecute(Sender : TObject);
begin
  SynEdit1.CopyToClipboard;
end;

procedure TMainForm.CopyTextUpdate(Sender : TObject);
begin
  (Sender as TAction).Enabled := SynEdit1.Lines.Text <> '';
end;

procedure TMainForm.DatabaseOnlineChange(Sender: TObject);
var ShutDownMode: TDBShutDownMode;
    Delay: integer;
begin
  if FLoading then Exit;
  ShutDownMode := DenyTransaction;
  Delay := 60;
  if DatabaseOnline.Checked then
    DBDataModule.BringDatabaseOnline
  else
  if ShutdownReqDlg.ShowModal(DatabaseAliasName.Text,ShutDownMode,Delay) = mrOK then
    DBDataModule.Shutdown(ShutdownMode,Delay);
end;

procedure TMainForm.DBIsReadOnlyChange(Sender: TObject);
begin
  if FLoading then Exit;
  try
    DBDataModule.DBReadOnly := DBIsReadOnly.Checked;
  except on E:Exception do
     MessageDlg(E.message,mtError,[mbOK],0);
  end;
end;

procedure TMainForm.DBSQLDialectEditingDone(Sender: TObject);
begin
  if FLoading then Exit;
  DBDataModule.DBSQLDialect := StrToInt(DBSQLDialect.Text);
end;

procedure TMainForm.DeleteTagExecute(Sender: TObject);
begin
  UserTagsSource.DataSet.Delete;
end;

procedure TMainForm.DeleteTagUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserTagsSource.DataSet.Active and (UserTagsSource.DataSet.RecordCount > 0);
end;

procedure TMainForm.DeleteUserExecute(Sender: TObject);
begin
  if MessageDlg('Do you really want to delete user ' + Trim(UserListSource.DataSet.FieldByName('SEC$USER_NAME').AsString),
     mtConfirmation,[mbYes,mbNo],0) = mrYes then
     UserListSource.DataSet.Delete;
end;

procedure TMainForm.DeleteUserUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := UserListSource.DataSet.Active and (UserListSource.DataSet.RecordCount > 0) and
    ((DBDataModule.ServiceUserName = 'SYSDBA') or DBDataModule.HasUserAdminPrivilege);
end;

procedure TMainForm.DisconnectAttachmentExecute(Sender: TObject);
begin
  if MessageDlg('Disconnect Attachment ID ' + AttachSource.DataSet.FieldByName('MON$ATTACHMENT_ID').AsString,
       mtConfirmation,[mbYes,mbNo],0) = mrYes then
    AttachSource.DataSet.Delete;
end;

procedure TMainForm.DisconnectAttachmentUpdate(Sender: TObject);
begin
  with AttachSource.DataSet do
  (Sender as TAction).Enabled := Active and (RecordCount > 0)
    and (FieldByName('MON$ATTACHMENT_ID').AsInteger <>
          AttmtSource.DataSet.FieldByName('MON$ATTACHMENT_ID').AsInteger);
end;

procedure TMainForm.DropDatabaseExecute(Sender: TObject);
begin
  if MessageDlg(Format('Do you really want to delete the database "%s". You will lose all your data!',
        [IBDatabaseInfo.Database.DatabaseName]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
  begin
    DBDataModule.DropDatabase;
    DBDataModule.Connect;
    if not IBDatabaseInfo.Database.Connected then Close;
  end;
end;

procedure TMainForm.DropDatabaseUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IBDatabaseInfo.Database.Connected;
end;

procedure TMainForm.AccessRightsTreeViewSelectionChanged(Sender: TObject);
begin
  if SubjectAccessRightsSource.DataSet = nil then Exit;
  if AccessRightsSource.DataSet.Active  then
  begin
    if (AccessRightsTreeView.Selected = nil) or (AccessRightsTreeView.Selected.Parent = nil) then
      SubjectAccessRightsSource.DataSet.Active := false
    else
      DBDataModule.SyncSubjectAccessRights(TIBTreeNode(AccessRightsTreeView.Selected).KeyValue);
  end;
end;

procedure TMainForm.MappingsTabHide(Sender: TObject);
begin
  AuthMapSource.DataSet.Active := false;
end;

procedure TMainForm.MappingsTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  AuthMapSource.DataSet.Active := IBDatabaseInfo.ODSMajorVersion > 11;
end;

procedure TMainForm.PageBuffersEditingDone(Sender: TObject);
begin
  DBDataModule.PageBuffers := StrToInt(PageBuffers.Text);
end;

procedure TMainForm.QuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.RepairTabHide(Sender: TObject);
begin
  DBTablesSource.DataSet.Active := false;
end;

procedure TMainForm.RepairTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  SelectRepairActionCloseUp(nil);
  ValidateOptions.ActivePage := ValidateOptionsTab;
end;

procedure TMainForm.RevokeAllExecute(Sender: TObject);
begin
  if MessageDlg('Revoke all Access Rights from User ' + Trim(AccessRightsTreeView.Selected.Text),
      mtConfirmation,[mbYes,mbNo],0) = mrYes then
    DBDataModule.RevokeAll;
end;

procedure TMainForm.RevokeAllUpdate(Sender: TObject);
begin
  with AccessRightsSource.DataSet do
  (Sender as TAction).Enabled := Active and (RecordCount > 0) and
    (FieldByName('SUBJECT_TYPE').AsInteger = 8);
end;

procedure TMainForm.RunScriptExecute(Sender: TObject);
begin
  ExecuteSQLScriptDlg.ShowModal;
end;

procedure TMainForm.SaveSchemaExecute(Sender : TObject);
begin
  if SQLSaveDialog.Execute then
    SynEdit1.Lines.SaveToFile(SQLSaveDialog.FileName);
end;

procedure TMainForm.SchemaSelectAllExecute(Sender : TObject);
begin
  SynEdit1.SelectAll;
end;

procedure TMainForm.SelectAllTablesChange(Sender: TObject);
var aBookmark: TBookmark;
begin
  with DBTablesSource.DataSet do
  if Active then
  begin
    aBookmark := Bookmark;
    DisableControls;
    try
      First;
      while not EOF do
      begin
        Edit;
        if SelectAllTables.Checked then
          FieldByName('Selected').AsInteger := 1
        else
          FieldByName('Selected').AsInteger := 0;
        Post;
        Next;
      end;
    finally
       Bookmark := aBookmark;
       EnableControls;
    end;
  end;
end;

procedure TMainForm.SelectedTablesOnlyChange(Sender: TObject);
begin
  SelectedTablesGrid.Enabled := SelectedTablesOnly.Checked;
  SelectAllTables.Enabled := SelectedTablesOnly.Checked;
end;

procedure TMainForm.SelectRepairActionCloseUp(Sender: TObject);
begin
  if (SelectRepairAction.ItemIndex = 1) and (IBDatabaseInfo.ODSMajorVersion < 12) then
  begin
    MessageDlg('Online validation is not support by Firebird prior to release 3',
               mtError,[mbOK],0);
    SelectRepairAction.ItemIndex := 2;
  end;
  ValidateOptions.Enabled := SelectRepairAction.ItemIndex = 2;
  ConfigureOnlineValidation;
end;

procedure TMainForm.SubjectAccessRightsSourceDataChange(Sender: TObject;
  Field: TField);
begin
  if (Field = nil) and (not (Sender as TDataSource).Dataset.FieldByName('UPDATE_COLUMNS').IsNull or
    not (Sender as TDataSource).Dataset.FieldByName('REFERENCE_COLUMNS').IsNull) then
    SubjectAccessRightsGrid.ShowEditorPanel;
end;

procedure TMainForm.ToolButton8Click(Sender: TObject);
begin
  OpenDatabaseExecute(nil);
end;

procedure TMainForm.UserManagerTabHide(Sender: TObject);
begin
  UserListSource.DataSet.Active := PageControl1.ActivePage = AccessRightsTab;
end;

procedure TMainForm.UserManagerTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected or DBDataModule.EmbeddedMode then Exit;
  UserListSource.DataSet.Active := true;
end;

procedure TMainForm.FilesTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  PrimaryDBFile.Text := IBDatabaseInfo.DBFileName;
  SecDBFilesSource.DataSet.Active := true;
  ShadowSource.DataSet.Active := true;
end;

procedure TMainForm.OpenDatabaseExecute(Sender: TObject);
begin
  PageControl1.ActivePage := Properties;
  DBDataModule.Connect;
  if not IBDatabaseInfo.Database.Connected then Close;
end;

procedure TMainForm.PropertiesShow(Sender: TObject);
begin
  if Visible and (IBDatabaseInfo.Database <> nil) and IBDatabaseInfo.Database.Connected then
    LoadData;
end;

procedure TMainForm.RemoveShadowExecute(Sender: TObject);
var ShadowSet: integer;
begin
  ShadowSet := ShadowSource.DataSet.FieldByName('RDB$Shadow_Number').AsInteger;
  DBDataModule.RemoveShadowSet(ShadowSet);
end;

procedure TMainForm.RemoveShadowUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=  ShadowSource.DataSet.Active and (ShadowSource.DataSet.RecordCount > 0);
end;

procedure TMainForm.BackupExecute(Sender: TObject);
begin
  fmMain.lmBackupNewClick(nil);
  //DBDataModule.BackupDatabase;
end;

procedure TMainForm.RestoreExecute(Sender: TObject);
begin
  fmMain.mnRestoreClick(nil);
  //DBDataModule.RestoreDatabase;
end;

procedure TMainForm.RollbackAllExecute(Sender: TObject);
begin
  DBDataModule.LimboResolution(RollbackGlobal,LimboReport.Lines);
end;

procedure TMainForm.RunRepairExecute(Sender: TObject);
var Options: TValidateOptions;
begin
  ValidationReport.Lines.Clear;
  case SelectRepairAction.ItemIndex of
  0: {sweep}
    Options := [SweepDB];
  1: {Online Validation }
    begin
      DBDataModule.OnlineValidation(ValidationReport.Lines,SelectedTablesOnly.Checked);
      Exit;
    end;
  2: {Full Validation}
    if ValidateOptions.ActivePage = ValidateOptionsTab then
    begin
      Options := [ValidateDB];
      if RecordFragments.Checked then
        Options += [ValidateFull];
      if ReadOnlyValidation.Checked then
        Options += [CheckDB];
      if IgnoreChecksums.Checked then
        Options += [IgnoreChecksum];
    end
    else
    begin
      Options := [MendDB];
      if ValidateRepairRecordFragments.Checked then
        Options += [ValidateFull];
      if IgnoreChecksumsOnRepair.Checked then
        Options += [IgnoreChecksum];
    end;
  3: {Kill Shadows}
    Options := [KillShadows];
  end;

  DBDataModule.DatabaseRepair(Options,ValidationReport.Lines);
  if (SelectRepairAction.ItemIndex = 2) and (ValidateDB in Options) then
    ValidateOptions.ActivePage := RepairOptionsTab
  else
    ValidateOptions.ActivePage := ValidateOptionsTab;
end;

procedure TMainForm.SaveChangesExecute(Sender: TObject);
begin
  if UserTagsSource.DataSet.State in [dsEdit,dsInsert] then
    UserTagsSource.DataSet.Post;
  if RoleSource.DataSet.State in [dsEdit,dsInsert] then
    RoleSource.DataSet.Post;
  if UserListSource.DataSet.State in [dsEdit,dsInsert] then
    UserListSource.DataSet.Post;
end;

procedure TMainForm.SaveChangesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (UserListSource.DataSet.State in [dsInsert,dsEdit]) or
                                 (RoleSource.DataSet.State in [dsInsert,dsEdit]) or
                                 (UserTagsSource.DataSet.State in [dsInsert,dsEdit]) ;
end;

procedure TMainForm.SaveExecute(Sender: TObject);
begin
  if PageControl1.ActivePage = SchemaTab then
  begin
    if SQLSaveDialog.Execute then
       SynEdit1.Lines.SaveToFile(SQLSaveDialog.FileName);
  end
  else
  if PageControl1.ActivePage = StatisticsTab then
  begin
    if SaveDialog.Execute then
      StatsMemo.Lines.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TMainForm.SaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (PageControl1.ActivePage = SchemaTab) or
                                 (PageControl1.ActivePage = StatisticsTab);
end;

procedure TMainForm.SchemaTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  SynEdit1.Lines.Clear;
  Application.QueueAsyncCall(@DoExtract,0);
end;

procedure TMainForm.ServerTabHide(Sender: TObject);
begin
  FServerError := false;
end;

procedure TMainForm.ServerTabShow(Sender: TObject);
begin
  if not Visible or FServerError then Exit;
  try
    LoadServerData;
    DBDataModule.LoadServerLog(ServerLog.Lines);
  except
   FServerError := true;
   ServerLog.Lines.Clear;
   raise;
  end;
end;

procedure TMainForm.StatisticsTabHide(Sender: TObject);
begin
  FLastStatsIndex := -1;
end;

procedure TMainForm.StatisticsTabShow(Sender: TObject);
begin
  if not Visible or not IBDatabaseInfo.Database.Connected then Exit;
  if FLastStatsIndex <> StatsOptions.ItemIndex then {avoids loops if exception raise in load stats}
    StatsOptionsCloseUp(nil);
end;

procedure TMainForm.StatsOptionsCloseUp(Sender: TObject);
begin
  StatsMemo.Lines.Clear;
  FLastStatsIndex := StatsOptions.ItemIndex;
  DBDataModule.LoadDatabaseStatistics(StatsOptions.ItemIndex,StatsMemo.Lines);
end;

procedure TMainForm.SweepIntervalEditingDone(Sender: TObject);
begin
  if FLoading then Exit;
  DBDataModule.SweepInterval := StrtoInt(SweepInterval.Text);
end;

procedure TMainForm.SyncWritesChange(Sender: TObject);
begin
  if FLoading then Exit;
  DBDataModule.ForcedWrites := SyncWrites.Checked;
end;

procedure TMainForm.ToggleAutoRefreshExecute(Sender: TObject);
begin
  AttmtTimer.Enabled := not AttmtTimer.Enabled;
end;

procedure TMainForm.ToggleAutoRefreshUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := AttachSource.DataSet.Active;
  (Sender as TAction).Checked := AttmtTimer.Enabled;
end;

procedure TMainForm.DoDBOpen(Data: PtrInt);
begin
    ConnectToDatabase;
end;

procedure TMainForm.HandleDBConnect(Sender: TObject);
begin
  ConfigureForServerVersion;
  PageControl1.ActivePage := Properties;
  ValidationReport.Lines.Clear;
  LimboReport.Lines.Clear;
  FLastStatsIndex := -1;
end;

procedure TMainForm.HandleLoadData(Sender: TObject);
begin
  if DBDataModule.EmbeddedMode then
    StatusBar1.SimpleText := Format('Database: %s - Logged in as user %s in embedded mode',
       [DBDataModule.IBDatabase1.DatabaseName,DBDataModule.IBDatabase1.Params.Values['user_name']
        ])
  else
  if DBDataModule.DBUserName = 'SYSDBA' then
  StatusBar1.SimpleText := Format('Database: %s - Logged in as user %s by %s, using %s security database.',
       [DBDataModule.IBDatabase1.DatabaseName,DBDataModule.DBUserName,
        DBDataModule.AuthMethod, DBDataModule.SecurityDatabase])
  else
    StatusBar1.SimpleText := Format('Database: %s - Logged in as user %s by %s, using %s security database. Role = %s',
         [DBDataModule.IBDatabase1.DatabaseName,DBDataModule.DBUserName,
          DBDataModule.AuthMethod, DBDataModule.SecurityDatabase,DBDataModule.RoleName]);
  if assigned(PageControl1.ActivePage.OnShow) then
    PageControl1.ActivePage.OnShow(nil);
  ClientLibrary.Caption := 'Firebird Client Library: ' + DBDataModule.IBDatabase1.FirebirdAPI.GetFBLibrary.GetLibraryFilePath +
  ' (API Version = ' + DBDataModule.IBDatabase1.FirebirdAPI.GetImplementationVersion + ')';
end;

procedure TMainForm.LoadData;
begin
  if FLoading then Exit;
  FLoading := true;
  try
    DatabaseAliasName.Text := DBDataModule.DatabaseName;
    Edit1.Text := IBDatabaseInfo.DBSiteName;
    ODSVersionString.Text :=  Format('%d.%d',[IBDatabaseInfo.ODSMajorVersion,IBDatabaseInfo.ODSMinorVersion]);
    ServerVersionNo.Text :=  IBDatabaseInfo.Version;
    DBSQLDialect.Text :=  IntToStr(DBDataModule.DBSQLDialect);
    ConnectString.Text := DBDataModule.IBDatabase1.DatabaseName;
    DBDateCreated.Text := DBDataModule.DBDateCreated;
    Edit10.Text := IntToStr(IBDatabaseInfo.CurrentMemory);
    Edit11.Text := IntToStr(IBDatabaseInfo.MaxMemory);
    PageBuffers.Text := IntToStr(DBDataModule.PageBuffers);
    AllocatedPages.Text := IntToStr(IBDatabaseInfo.Allocation);
    DBIsReadOnly.Checked := DBDataModule.DBReadOnly;
    SyncWrites.Checked := DBDataModule.ForcedWrites;
    SweepInterval.Text := IntToStr(IBDatabaseInfo.SweepInterval);
    NoReserve.Checked := DBDataModule.NoReserve;
    LingerDelay.Text := DBDataModule.LingerDelay;
    SecDatabase.Text := DBDataModule.SecurityDatabase;
    DBOwner.Text := DBDataModule.DBOwner;
    DatabaseOnline.Checked := DBDataModule.IsDatabaseOnline;
    IsShadowChk.Checked := DBDataModule.IsShadowDatabase;
    if IBDatabaseInfo.ODSMajorVersion >= 12 then
    begin
      PagesUsed.Text := IntToStr(IBDatabaseInfo.PagesUsed);
      PagesAvail.Text := IntToStr(IBDatabaseInfo.PagesFree);
      AutoAdmin.Checked := not DBDataModule.EmbeddedMode and DBDataModule.AutoAdmin;
    end
    else
    begin
      PagesUsed.Text := 'n/a';
      PagesAvail.Text := 'n/a';
      AutoAdmin.Checked :=  false;
    end;
    DBDataModule.IBDatabase1.Attachment.getFBVersion(ClientServerVersion.Lines);
    DBComments.Lines.Text := DBDataModule.Description;
  finally
    FLoading := false;
  end;
end;

procedure TMainForm.LoadServerData;
var i: integer;
begin
  with DBDataModule.IBServerProperties1 do
  begin
    Edit3.Text := VersionInfo.ServerVersion;
    Edit2.Text := IntToStr(VersionInfo.ServiceVersion);
    Edit4.Text := VersionInfo.ServerImplementation;
    OpenDatabasesList.Clear;
    for i := 0 to DatabaseInfo.NoOfDatabases - 1 do
      OpenDatabasesList.Lines.Add(DatabaseInfo.DbName[i]);
    ConfigDataGrid.Visible := DBDataModule.LoadConfigData(ConfigParams.ConfigFileData);
    ConfigDataLabel.Visible := ConfigDataGrid.Visible;
    if ConfigDataGrid.Visible then
    begin
      OpenDatabasesList.Height := ConfigDataLabel.Top - OpenDatabasesList.Top - 6;
      OpenDatabasesList.Anchors := OpenDatabasesList.Anchors - [akBottom];
    end
    else
    begin
      OpenDatabasesList.Height := ConfigDataGrid.Top - OpenDatabasesList.Top + ConfigDataGrid.Height;
      OpenDatabasesList.Anchors := OpenDatabasesList.Anchors + [akBottom];
    end;
    Edit5.Text := ConfigParams.BaseLocation;
    Edit6.Text := ConfigParams.LockFileLocation;
    Edit7.Text :=  ConfigParams.SecurityDatabaseLocation;
    Edit9.Text :=  ConfigParams.MessageFileLocation;
    Edit8.Text := ServicesConnection.ServerName;
    Edit12.Text := IntToStr(DatabaseInfo.NoOfAttachments);
  end;
end;

procedure TMainForm.DoExtract(Data: PtrInt);
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    if IncludeUserGrants.Checked then
      IBExtract1.ExtractObject(eoDatabase,'',[etGrantsToUser])
    else
      IBExtract1.ExtractObject(eoDatabase);
    SynEdit1.Lines.Assign(IBExtract1.Items);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ConfigureForServerVersion;
var i: integer;
begin
  if (IBDatabaseInfo.ODSMajorVersion >= 12) and
     ((DBDataModule.ServiceUserName = 'SYSDBA') or (DBDataModule.RoleName = 'RDB$ADMIN') or
            not DBDataModule.HasUserAdminPrivilege) then
  begin
    for i in [9,10] do
      UserManagerGrid.Columns[i].Visible := false;
      for i in [4,6,7,8] do
        UserManagerGrid.Columns[i].Visible := true ;
    UserListSource.DataSet := DBDataModule.UserList;
    TagsHeader.Visible := true;
    TagsGrid.Visible := true;
  end
  else
  begin
    for i in [4,6,7,8] do
      UserManagerGrid.Columns[i].Visible := false;
      for i in [9,10] do
        UserManagerGrid.Columns[i].Visible := true;
      UserListSource.DataSet := DBDataModule.LegacyUserList;
      TagsHeader.Visible := false;
      TagsGrid.Visible := false;
  end;

  if IBDatabaseInfo.ODSMajorVersion >= 12 then
  begin
    AttmtGrid.Columns[2].Visible := true;
    AttmntODS12Panel.Visible := true;
    DBCharacterSet.Visible := true;
    DBCharSetRO.Visible := false;
  end
  else
  begin
    AttmtGrid.Columns[2].Visible := false;
    AttmntODS12Panel.Visible := false;
    DBCharacterSet.Visible := false;
    DBCharSetRO.Visible := true;
  end;
  MappingsTab.TabVisible := not DBDataModule.EmbeddedMode and
  ((IBDatabaseInfo.ODSMajorVersion > 11) or
    ((IBDatabaseInfo.ODSMajorVersion = 11) and (IBDatabaseInfo.ODSMinorVersion > 0)));
  UserManagerTab.TabVisible := not DBDataModule.EmbeddedMode;
  AccessRightsTab.TabVisible := not DBDataModule.EmbeddedMode;
  AutoAdmin.Enabled := not DBDataModule.EmbeddedMode;
end;

procedure TMainForm.ConnectToDatabase;
begin
  if not DBDataModule.Connect then Close;
  PageControl1.Visible := true;
end;

procedure TMainForm.ConfigureOnlineValidation;
begin
  if SelectRepairAction.ItemIndex = 1 then
  begin
    DBTablesPanel.Visible := true;
    DBTablesSplitter.Visible := true;
    SelectedTablesGrid.Enabled := SelectedTablesOnly.Checked;
    SelectAllTables.Checked := true;
    DBTablesSource.DataSet.Active := true;
  end
  else
  begin
    DBTablesPanel.Visible := false;
    DBTablesSplitter.Visible := false;
    SelectAllTables.Enabled := false;
    DBTablesSource.DataSet.Active := false;
  end;
end;

end.

