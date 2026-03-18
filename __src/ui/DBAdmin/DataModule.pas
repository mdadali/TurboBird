(*
 * DataModule.pas
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
unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, db, memds, IBDatabase, IBSQL, IBQuery,
  IBCustomDataSet, IBUpdate, IBDatabaseInfo, IBXServices, IB,
  Dialogs, Controls, Forms;

type

  { TDBDataModule }

  TDBDataModule = class(TDataModule)
    AccessRightsCHILDCOUNT: TIBIntegerField;
    AccessRightsDisplayName: TStringField;
    AccessRightsID: TIBStringField;
    AccessRightsImageIndex: TLongintField;
    AccessRightsPARENT: TIBStringField;
    AccessRightsSUBJECT_NAME: TIBStringField;
    AccessRightsSUBJECT_TYPE: TIBSmallintField;
    ApplicationProperties1: TApplicationProperties;
    AttachmentsMONATTACHMENT_ID: TIBLargeIntField;
    AttachmentsMONATTACHMENT_NAME: TIBStringField;
    AttachmentsMONAUTH_METHOD: TIBStringField;
    AttachmentsMONCHARACTER_SET_ID: TIBSmallintField;
    AttachmentsMONCLIENT_VERSION: TIBStringField;
    AttachmentsMONGARBAGE_COLLECTION: TIBSmallintField;
    AttachmentsMONREMOTE_ADDRESS: TIBStringField;
    AttachmentsMONREMOTE_HOST: TIBStringField;
    AttachmentsMONREMOTE_OS_USER: TIBStringField;
    AttachmentsMONREMOTE_PID: TIBIntegerField;
    AttachmentsMONREMOTE_PROCESS: TIBStringField;
    AttachmentsMONREMOTE_PROTOCOL: TIBStringField;
    AttachmentsMONREMOTE_VERSION: TIBStringField;
    AttachmentsMONROLE: TIBStringField;
    AttachmentsMONSERVER_PID: TIBIntegerField;
    AttachmentsMONSTATE: TIBSmallintField;
    AttachmentsMONSTAT_ID: TIBIntegerField;
    AttachmentsMONSYSTEM_FLAG: TIBSmallintField;
    AttachmentsMONTIMESTAMP: TDateTimeField;
    AttachmentsMONUSER: TIBStringField;
    AttachmentsRDBBYTES_PER_CHARACTER: TIBSmallintField;
    AttachmentsRDBCHARACTER_SET_ID: TIBSmallintField;
    AttachmentsRDBCHARACTER_SET_NAME: TIBStringField;
    AttachmentsRDBDEFAULT_COLLATE_NAME: TIBStringField;
    AttachmentsRDBDESCRIPTION: TIBMemoField;
    AttachmentsRDBFORM_OF_USE: TIBStringField;
    AttachmentsRDBFUNCTION_NAME: TIBStringField;
    AttachmentsRDBNUMBER_OF_CHARACTERS: TIBIntegerField;
    AttachmentsRDBOWNER_NAME: TIBStringField;
    AttachmentsRDBSECURITY_CLASS: TIBStringField;
    AttachmentsRDBSYSTEM_FLAG: TIBSmallintField;
    CharSetLookup: TIBQuery;
    ConfigDataset: TMemDataset;
    CurrentTransaction: TIBTransaction;
    DatabaseQuery: TIBQuery;
    Attachments: TIBQuery;
    DBTables: TIBQuery;
    AuthMappings: TIBQuery;
    AccessRights: TIBQuery;
    IBConfigService1: TIBXConfigService;
    IBServerProperties1: TIBXServerProperties;
    IBLogService1: TIBXLogService;
    IBSecurityService1: TIBXSecurityService;
    IBOnlineValidationService1: TIBXOnlineValidationService;

      IBLimboTrans: TIBXLimboTransactionResolutionService;
    IBXServicesConnection1: TIBXServicesConnection;
    IBStatisticalService1: TIBXStatisticalService;
    IBValidationService1: TIBXValidationService;
    InLimboList: TIBXServicesLimboTransactionsList;
    LegacyUserList: TIBXServicesUserList;
    SubjectAccessRights: TIBQuery;
    AttUpdate: TIBUpdate;
    AdminUserQuery: TIBSQL;
    DBTablesUpdate: TIBUpdate;
    UserListGROUPID: TLongintField;
    UserListSECPASSWORD: TIBStringField;
    UserListSECUSER_NAME: TIBStringField;
    UserListSource: TDataSource;
    DBCharSet: TIBQuery;
    DBSecFiles: TIBQuery;
    ExecDDL: TIBSQL;
    IBDatabase1: TIBDatabase;
    IBDatabaseInfo: TIBDatabaseInfo;
    AttmtQuery: TIBQuery;
    RoleNameList: TIBQuery;
    TableNameLookup: TIBQuery;
    TagsUpdate: TIBUpdate;
    UpdateCharSet: TIBUpdate;
    SecGlobalAuth: TIBQuery;
    ShadowFiles: TIBQuery;
    ShadowFilesFileMode: TStringField;
    ShadowFilesRDBFILE_FLAGS: TSmallintField;
    ShadowFilesRDBFILE_LENGTH: TIntegerField;
    ShadowFilesRDBFILE_NAME: TIBStringField;
    ShadowFilesRDBFILE_SEQUENCE: TSmallintField;
    ShadowFilesRDBFILE_START: TIntegerField;
    ShadowFilesRDBSHADOW_NUMBER: TSmallintField;
    UpdateUserRoles: TIBUpdate;
    UpdateUsers: TIBUpdate;
    UserList: TIBQuery;
    UserListCURRENT_CONNECTION: TIBLargeIntField;
    UserListDBCREATOR: TBooleanField;
    UserListLOGGEDIN: TBooleanField;
    UserListSECACTIVE: TBooleanField;
    UserListSECADMIN: TBooleanField;
    UserListSECFIRST_NAME: TIBStringField;
    UserListSECLAST_NAME: TIBStringField;
    UserListSECMIDDLE_NAME: TIBStringField;
    UserListSECPLUGIN: TIBStringField;
    UserListUSERID: TLongintField;
    UserTags: TIBQuery;
    procedure AccessRightsCalcFields(DataSet: TDataSet);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure AttachmentsAfterDelete(DataSet: TDataSet);
    procedure AttachmentsAfterOpen(DataSet: TDataSet);
    procedure AttachmentsBeforeOpen(DataSet: TDataSet);
    procedure ConfigDatasetAfterClose(DataSet: TDataSet);
    procedure CurrentTransactionAfterTransactionEnd(Sender: TObject);
    procedure DatabaseQueryAfterOpen(DataSet: TDataSet);
    procedure DatabaseQueryBeforeClose(DataSet: TDataSet);
    procedure DBCharSetAfterClose(DataSet: TDataSet);
    procedure DBCharSetBeforeOpen(DataSet: TDataSet);
    procedure IBDatabase1AfterConnect(Sender: TObject);
    procedure IBDatabase1AfterDisconnect(Sender: TObject);
    procedure IBDatabase1BeforeConnect(Sender: TObject);
    procedure IBDatabase1BeforeDisconnect(Sender: TObject);
    procedure IBDatabase1Login(Database: TIBDatabase; LoginParams: TStrings);
    procedure AttUpdateApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure DBTablesUpdateApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure IBValidationService1GetNextLine(Sender: TObject; var Line: string
      );
    procedure IBXServicesConnection1AfterConnect(Sender: TObject);
    procedure IBXServicesConnection1Login(Service: TIBXServicesConnection;
      var aServerName: string; LoginParams: TStrings);
    procedure LegacyUserListAfterOpen(DataSet: TDataSet);
    procedure LegacyUserListAfterPost(DataSet: TDataSet);
    procedure LegacyUserListBeforeClose(DataSet: TDataSet);
    procedure ShadowFilesCalcFields(DataSet: TDataSet);
    procedure SubjectAccessRightsBeforeOpen(DataSet: TDataSet);
    procedure TagsUpdateApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure UpdateCharSetApplyUpdates(Sender: TObject;
      UpdateKind: TUpdateKind; Params: ISQLParams);
    procedure UpdateUserRolesApplyUpdates(Sender: TObject;
      UpdateKind: TUpdateKind; Params: ISQLParams);
    procedure UpdateUsersApplyUpdates(Sender: TObject; UpdateKind: TUpdateKind;
      Params: ISQLParams);
    procedure UserListAfterInsert(DataSet: TDataSet);
    procedure UserListAfterOpen(DataSet: TDataSet);
    procedure UserListAfterPost(DataSet: TDataSet);
    procedure UserListAfterScroll(DataSet: TDataSet);
    procedure UserListBeforeClose(DataSet: TDataSet);
    procedure UserTagsAfterInsert(DataSet: TDataSet);
  private
    FAfterDataReload: TNotifyEvent;
    FAfterDBConnect: TNotifyEvent;
    FDBHeaderScanned: boolean;
    FDisconnecting: boolean;
    FISShadowDatabase: boolean;
    FDBUserName: string;
    FDBPassword: string;
    FLocalConnect: boolean;
    FSubjectAccessRightsID: string;
    {Parsed results of connectstring;}
    FServerName: string;
    FPortNo: string;
    FProtocol: TProtocolAll;
    FDatabasePathName: string;
    FHasUserAdminPrivilege: boolean;
    function GetAuthMethod: string;
    function GetAutoAdmin: boolean;
    function GetDatabaseName: string;
    function GetDBDateCreated: string;
    procedure GetDBFlags;
    function GetDBOwner: string;
    function GetDBReadOnly: boolean;
    function GetDBSQLDialect: integer;
    function GetDBUserName: string;
    function GetDescription: string;
    function GetForcedWrites: boolean;
    function GetLingerDelay: string;
    function GetNoReserve: boolean;
    function GetPageBuffers: integer;
    function GetRoleName: string;
    function GetSecurityDatabase: string;
    function GetServerName: string;
    function GetSweepInterval: integer;
    function GetUserAdminPrivilege: boolean;
    procedure SetAutoAdmin(AValue: boolean);
    procedure SetDBReadOnly(AValue: boolean);
    procedure SetDBSQLDialect(AValue: integer);
    procedure SetDescription(AValue: string);
    procedure SetForcedWrites(AValue: boolean);
    procedure SetLingerDelay(AValue: string);
    procedure SetNoReserve(AValue: boolean);
    procedure SetPageBuffers(AValue: integer);
    procedure SetSweepInterval(AValue: integer);
    procedure ReloadData(Data: PtrInt=0);
  protected
    FServiceUserName: string;
    function GetEmbeddedMode: boolean; virtual;
    procedure ConnectServicesAPI; virtual;
    function CallLoginDlg(var aDatabaseName, aUserName, aPassword: string;
      var aCreateIfNotExist: boolean): TModalResult; virtual;
  public
    destructor Destroy; override;
    function Connect: boolean; virtual;
    procedure Disconnect; virtual;
    procedure DropDatabase;
    procedure BackupDatabase;
    procedure RestoreDatabase;
    procedure BringDatabaseOnline;
    procedure ShutDown(aShutDownmode: TDBShutdownMode; aDelay: integer);
    procedure DatabaseRepair(Options: TValidateOptions; ReportLines: TStrings);
    procedure OnlineValidation(ReportLines: TStrings; SelectedTablesOnly: boolean);
    procedure LimboResolution(ActionID: TTransactionGlobalAction; Report: TStrings);
    function IsDatabaseOnline: boolean;
    function IsShadowDatabase: boolean;
    procedure ActivateShadow;
    procedure AddSecondaryFile(aFileName: string; StartAt,FileLength: integer);
    procedure AddShadowSet;
    procedure RemoveShadowSet(ShadowSet: integer);
    procedure LoadPerformanceStatistics(Lines: TStrings);
    procedure LoadDatabaseStatistics(OptionID: integer; Lines: TStrings);
    function LoadConfigData(ConfigFileData: TConfigFileData): boolean;
    procedure LoadServerProperties(Lines: TStrings);
    procedure LoadServerLog(Lines: TStrings);
    procedure RevokeAll;
    procedure SyncSubjectAccessRights(ID: string);
    property AutoAdmin: boolean read GetAutoAdmin write SetAutoAdmin;
    property Description: string read GetDescription write SetDescription;
    property Disconnecting: boolean read FDisconnecting;
    property ForcedWrites: boolean read GetForcedWrites write SetForcedWrites;
    property LingerDelay: string read GetLingerDelay write SetLingerDelay;
    property DBReadOnly: boolean read GetDBReadOnly write SetDBReadOnly;
    property NoReserve: boolean read GetNoReserve write SetNoReserve;
    property PageBuffers: integer read GetPageBuffers write SetPageBuffers;
    property SweepInterval: integer read GetSweepInterval write SetSweepInterval;
    property DatabaseName: string read GetDatabaseName;
    property SecurityDatabase: string read GetSecurityDatabase;
    property AuthMethod: string read GetAuthMethod;
    property EmbeddedMode: boolean read GetEmbeddedMode;
    property DBUserName: string read GetDBUserName;
    property RoleName: string read GetRoleName;
    property DBOwner: string read GetDBOwner;
    property DBSQLDialect: integer read GetDBSQLDialect write SetDBSQLDialect;
    property DBDateCreated: string read GetDBDateCreated;
    property ServerName: string read GetServerName;
    property ServiceUserName: string read FServiceUserName;
    property HasUserAdminPrivilege: boolean read FHasUserAdminPrivilege;
    property AfterDBConnect: TNotifyEvent read FAfterDBConnect write FAfterDBConnect;
    property AfterDataReload: TNotifyEvent read FAfterDataReload write FAfterDataReload;
  end;

var
  DBDataModule: TDBDataModule;

implementation

{$R *.lfm}

uses DBLoginDlgUnit, IBUtils, IBMessages, ShutdownDatabaseDlgUnit,
  BackupDlgUnit, RestoreDlgUnit, AddShadowSetDlgUnit, IBErrorCodes;

const
  sAddSecondarySQL  = 'Alter Database Add File ''%s'' Starting at %d';
  sAddSecondarySQL2 = 'Alter Database Add File ''%s'' Starting at %d Length %d';
  sRemoveShadow     = 'Drop Shadow %d';
  sRemoveShadow12   = 'Drop Shadow %d DELETE FILE';
  sPreserveShadow   = 'Drop Shadow %d PRESERVE FILE';

resourcestring
  sPreserveShadowFiles = 'Preserve Shadow Set Files after drop?';

{ TDBDataModule }

procedure TDBDataModule.UpdateCharSetApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
begin
  if UpdateKind = ukModify then
  begin
    ExecDDL.SQL.Text := 'ALTER DATABASE SET DEFAULT CHARACTER SET ' +
                Params.ByName('RDB$CHARACTER_SET_NAME').AsString;
    ExecDDL.ExecQuery;
  end;
end;

procedure TDBDataModule.UpdateUserRolesApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);

  procedure Grant(Params: ISQLParams);
  begin
    ExecDDL.SQL.Text := 'Grant ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' to ' + Params.ByName('SEC$USER_NAME').AsString;
    ExecDDL.ExecQuery;
  end;

  procedure Revoke(Params: ISQLParams);
  begin
    ExecDDL.SQL.Text := 'Revoke ' + trim(Params.ByName('RDB$ROLE_NAME').AsString) + ' from ' + Params.ByName('SEC$USER_NAME').AsString;
    ExecDDL.ExecQuery;
  end;

begin
  if UpdateKind = ukModify then
  begin
    if Params.ByName('GRANTED').AsInteger = 0 then
      Revoke(Params)
    else
      Grant(Params);
  end;
end;

procedure TDBDataModule.UpdateUsersApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);

var UserName: string;

  function FormatStmtOptions: string;
  var Param: ISQLParam;
  begin
    Result := UserName;
    Param := Params.ByName('SEC$PASSWORD');
    if (Param <> nil) and not Param.IsNull  then
      Result += ' PASSWORD ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$FIRST_NAME');
    if Param <> nil then
      Result += ' FIRSTNAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$MIDDLE_NAME');
    if Param <> nil then
      Result += ' MIDDLENAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$LAST_NAME');
    if Param <> nil then
      Result += ' LASTNAME ''' + SQLSafeString(Param.AsString) + '''';
    Param := Params.ByName('SEC$ACTIVE');
    if Param <> nil then
    begin
      if Param.AsBoolean then
        Result += ' ACTIVE'
      else
        Result += ' INACTIVE';
    end;
    Param := Params.ByName('SEC$PLUGIN');
    if Param <> nil then
      Result += ' USING PLUGIN ' + QuoteIdentifierIfNeeded((Sender as TIBUpdate).DataSet.Database.SQLDialect,Param.AsString);
  end;

  function GetAlterPasswordStmt: string;
  var Param: ISQLParam;
  begin
    Result := '';
    Param := Params.ByName('SEC$PASSWORD');
    if (UpdateKind = ukModify) and not Param.IsNull then
    begin
      Result := 'ALTER USER ' + UserName +
          ' PASSWORD ''' + SQLSafeString(Param.AsString) + '''';
      Param := Params.ByName('SEC$PLUGIN');
     if Param <> nil then
       Result += ' USING PLUGIN ' + QuoteIdentifierIfNeeded((Sender as TIBUpdate).DataSet.Database.SQLDialect,Param.AsString);
    end;
  end;

begin
  UserName := Trim(Params.ByName('SEC$USER_NAME').AsString);
  {non SYSDBA user not an RDB$ADMIN can only change their password}
  if (DBUserName <> 'SYSDBA') and (RoleName <> 'RDB$ADMIN') then
  begin
   ExecDDL.SQL.Text := GetAlterPasswordStmt;
   if ExecDDL.SQL.Text <> '' then
     ExecDDL.ExecQuery;
   Exit;
  end;

  case UpdateKind of
  ukInsert:
      ExecDDL.SQL.Text := 'CREATE USER ' + FormatStmtOptions;
  ukModify:
      ExecDDL.SQL.Text := 'ALTER USER ' + FormatStmtOptions;
  ukDelete:
    ExecDDL.SQL.Text := 'DROP USER ' + UserName;
  end;
  ExecDDL.ExecQuery;

  if UpdateKind = ukInsert then
  begin
    {if new user is also given the admin role then we need to add this}
    if Params.ByName('SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + UserName + ' GRANT ADMIN ROLE';
      ExecDDL.ExecQuery;
    end;
    if Params.ByName('DBCreator').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'GRANT CREATE DATABASE TO USER ' + UserName;
      ExecDDL.ExecQuery;
    end
    else
    begin
      ExecDDL.SQL.Text := 'REVOKE CREATE DATABASE FROM USER ' + UserName;
      ExecDDL.ExecQuery;
    end
  end
  else
  if UpdateKind = ukModify then
  {Update Admin Role if allowed}
  begin
    if Params.ByName('SEC$ADMIN').AsBoolean and not Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + UserName + ' GRANT ADMIN ROLE';
      ExecDDL.ExecQuery;
    end
    else
    if not Params.ByName('SEC$ADMIN').AsBoolean and Params.ByName('OLD_SEC$ADMIN').AsBoolean then
    begin
      ExecDDL.SQL.Text := 'ALTER USER ' + UserName + ' REVOKE ADMIN ROLE';
      ExecDDL.ExecQuery;
    end
  end;

  {Update DB Creator Role}
  if Params.ByName('DBCreator').AsBoolean and not Params.ByName('OLD_DBCreator').AsBoolean then
  begin
    ExecDDL.SQL.Text := 'GRANT CREATE DATABASE TO USER ' + UserName;
    ExecDDL.ExecQuery;
  end
  else
  if not Params.ByName('DBCreator').AsBoolean and Params.ByName('OLD_DBCreator').AsBoolean then
  begin
    ExecDDL.SQL.Text := 'REVOKE CREATE DATABASE FROM USER ' + UserName;
    ExecDDL.ExecQuery;
  end
end;

procedure TDBDataModule.UserListAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('SEC$ADMIN').AsBoolean := false;
  DataSet.FieldByName('SEC$ACTIVE').AsBoolean := false;
  DataSet.FieldByName('DBCreator').AsBoolean := false;
  DataSet.FieldByName('SEC$PLUGIN').AsString := 'Srp';
//  DataSet.FieldByName('UserID').AsInteger := 0;
//  DataSet.FieldByName('GroupID').AsInteger := 0;
  DataSet.FieldByName('SEC$PASSWORD').Clear;
  RoleNameList.Active := false; {Prevent role assignments until saved}
  UserTags.Active := false; {ditto}
end;

procedure TDBDataModule.UserListAfterOpen(DataSet: TDataSet);
begin
  if UserListSource.DataSet <> DataSet then
    UserListSource.DataSet := DataSet;
  RoleNameList.Active := true;
  UserTags.Active := true;
end;

procedure TDBDataModule.UserListAfterPost(DataSet: TDataSet);
begin
  CurrentTransaction.Commit;
end;

procedure TDBDataModule.UserListAfterScroll(DataSet: TDataSet);
begin
  UserList.FieldByName('SEC$PLUGIN').ReadOnly := UserList.State <> dsInsert;
end;

procedure TDBDataModule.UserListBeforeClose(DataSet: TDataSet);
begin
  RoleNameList.Active := false;
  UserTags.Active := false;
end;

procedure TDBDataModule.UserTagsAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('SEC$USER_NAME').AsString := DataSet.DataSource.DataSet.FieldByName('SEC$USER_NAME').AsString;
end;

procedure TDBDataModule.ConnectServicesAPI;
begin
  if IBXServicesConnection1.Connected then Exit;
  try
    IBXServicesConnection1.ConnectUsing(IBDatabase1);
  except on E: Exception do
    begin
      Application.ShowException(E);
      IBDatabase1.Connected := false;
      FDBPassword := '';
      Exit;
    end;
  end;
end;

function TDBDataModule.CallLoginDlg(var aDatabaseName, aUserName,
  aPassword: string; var aCreateIfNotExist: boolean): TModalResult;
begin
  Result := DBLoginDlg.ShowModal(aDatabaseName, aUserName, aPassword, aCreateIfNotExist);
end;

procedure TDBDataModule.GetDBFlags;
var Lines: TStringList;
    i: integer;
    line: string;
begin
  if FDBHeaderScanned or not DatabaseQuery.Active or not AttmtQuery.Active then Exit;
  FIsShadowDatabase := false;

  try
    with IBStatisticalService1 do
    begin
        Options := [HeaderPages];
        Lines := TStringList.Create;
        try
          Execute(Lines);
          for i := 0 to Lines.Count - 1 do
          begin
            line := Lines[i];
             if (Pos('Attributes',Line) <> 0) and (Pos('shadow',Line) <> 0) then
             begin
               FIsShadowDatabase := true;
               break;
             end;
          end;
        finally
          Lines.Free;
        end;
        FDBHeaderScanned := true;
    end;
  except on E: Exception do
    MessageDlg('Error getting DB Header Page: ' + E.Message,mtError,[mbOK],0);
  end;
end;

function TDBDataModule.GetDBOwner: string;
var DBOField: TField;
begin
  DBOField := DatabaseQuery.FindField('MON$OWNER');
  if DBOField <> nil then
    Result := Trim(DBOField.AsString)
  else
    Result := 'n/a';
end;

function TDBDataModule.GetAutoAdmin: boolean;
begin
  Result := false;
  if not CurrentTransaction.Active then Exit;
  SecGlobalAuth.Active := true; {sets AutoAdmin}
  try
    Result := SecGlobalAuth.FieldByName('Mappings').AsInteger > 0;
  finally
    SecGlobalAuth.Active := false;
  end;
end;

function TDBDataModule.GetDatabaseName: string;
begin
  if DatabaseQuery.Active and not DatabaseQuery.FieldByName('MON$DATABASE_NAME').IsNull then
    Result := DatabaseQuery.FieldByName('MON$DATABASE_NAME').AsString
  else
    Result := FDatabasePathName;
end;

function TDBDataModule.GetDBDateCreated: string;
begin
  with DefaultFormatSettings do
  try
    Result := FormatDateTime(LongDateFormat + ' ' + LongTimeFormat,DatabaseQuery.FieldByName('MON$CREATION_DATE').AsDateTime);
  except
    Result := 'unknown';
  end;
end;

function TDBDataModule.GetDBReadOnly: boolean;
begin
  Result := DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$READ_ONLY').AsInteger  <> 0);
end;

function TDBDataModule.GetDBSQLDialect: integer;
begin
  Result := IBDatabaseInfo.DBSQLDialect;
end;

function TDBDataModule.GetDBUserName: string;
var DPB: IDPB;
    info: IDPBItem;
begin
  Result := '';
  if AttmtQuery.Active then
    Result := Trim(AttmtQuery.FieldByName('MON$USER').AsString)
  else
  if IBDatabase1.Connected then
  begin
    DPB := IBDatabase1.Attachment.getDPB;
    info := DPB.Find(isc_dpb_user_name);
    if info <> nil then
      Result := info.AsString;
  end
end;

function TDBDataModule.GetDescription: string;
begin
  if DatabaseQuery.Active then
    Result :=  DatabaseQuery.FieldByName('RDB$DESCRIPTION').AsString
  else
    Result := '';
end;

function TDBDataModule.GetEmbeddedMode: boolean;
begin
  Result := AttmtQuery.Active and AttmtQuery.FieldByName('MON$REMOTE_PROTOCOL').IsNull;
end;

function TDBDataModule.GetForcedWrites: boolean;
begin
  Result := DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$FORCED_WRITES').AsInteger  <> 0);
end;

procedure TDBDataModule.SetLingerDelay(AValue: string);
begin
  if (StrToInt(AValue) =  DatabaseQuery.FieldByName('RDB$LINGER').AsInteger) then Exit;

  if (AValue = '') or (StrToInt(AValue) = 0) then
  begin
    if MessageDlg('Turn off Linger Permanently?',mtConfirmation,[mbYes,mbNo],0) = mrNo then
    begin
      IBConfigService1.SetNoLinger;
      CurrentTransaction.Commit; {Refresh}
      Exit;
    end;
    ExecDDL.SQL.Text := 'ALTER DATABASE DROP LINGER'
  end
  else
    ExecDDL.SQL.Text := 'ALTER DATABASE SET LINGER TO ' + AValue;
  with ExecDDL do
  begin
    Transaction.Active := true;
    ExecQuery;
    Transaction.Commit;
  end;
end;


function TDBDataModule.GetAuthMethod: string;
var AuthMeth: TField;
begin
  AuthMeth := AttmtQuery.FindField('MON$AUTH_METHOD');
  if AuthMeth = nil then
    Result := 'Legacy_auth'
  else
    Result := AuthMeth.AsString;
end;

procedure TDBDataModule.SetNoReserve(AValue: boolean);
begin
  IBConfigService1.SetReserveSpace(AValue);
end;

procedure TDBDataModule.SetPageBuffers(AValue: integer);
begin
  IBDatabase1.Connected := false;
  try
    IBConfigService1.SetPageBuffers(AValue);
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBDataModule.SetSweepInterval(AValue: integer);
begin
  IBDatabase1.Connected := false;
  try
    IBConfigService1.SetSweepInterval(AValue);
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBDataModule.ReloadData(Data: PtrInt);
begin
  if csDestroying in ComponentState then Exit;
  CurrentTransaction.Active := true;
  DataBaseQuery.Active := true;
  AttmtQuery.Active := true;
  if LegacyUserList.Active then
    RoleNameList.Active := true;
  if assigned(FAfterDataReload) then
    AfterDataReload(self);
end;

destructor TDBDataModule.Destroy;
begin
  Application.RemoveAsyncCalls(self);
  inherited Destroy;
end;

function TDBDataModule.Connect: boolean;

  procedure ReportException(E: Exception);
  begin
   MessageDlg(E.Message,mtError,[mbOK],0);
   FDBPassword := '';
  end;

  procedure KillShadows;
  begin
    with IBXServicesConnection1 do
    begin
      ServerName := FServerName;
      Protocol := FProtocol;
      PortNo := FPortNo;
      Connected := true;
    end;
    try
      with IBValidationService1 do
      begin
        DatabaseName := FDatabasePathName;
        Options := [IBXServices.KillShadows];
        Execute(nil);
        MessageDlg('All Unavailable Shadows killed',mtInformation,[mbOK],0);
      end;
    finally
      IBXServicesConnection1.Connected := false;
    end;
  end;

var KillDone: boolean;
begin
  KillDone := false;
  Result := false;
  Disconnect;
  repeat
    try
      IBDatabase1.Connected := true;
    except
     on E:EIBClientError do
      begin
        Exit
      end;
    On E: EIBInterBaseError do
      begin
        FDBPassword := '';
        if (E.IBErrorCode = isc_io_error) and not KillDone then
        begin
          if MessageDlg('I/O Error reported on database file. If this is a shadow file, do you want '+
                        'to kill all unavailable shadow sets?. The original message is ' + E.Message,
                        mtInformation,[mbYes,mbNo],0) = mrNo then
            continue;
          try KillShadows except end;
          KillDone := true;
        end
        else
          ReportException(E);
      end;
    On E:Exception do
      ReportException(E);
    end;
  until IBDatabase1.Connected;

  if assigned(FAfterDBConnect) then
    AfterDBConnect(self);
  Result := IBDatabase1.Connected;
end;

procedure TDBDataModule.Disconnect;
begin
  FDBUserName := '';
  FDBPassword := '';
  FServiceUserName := '';
  FLocalConnect := false;
  IBDatabase1.Connected := false;
  IBXServicesConnection1.Connected := false;
  FDBHeaderScanned := false;
end;

procedure TDBDataModule.DropDatabase;
begin
  IBDatabase1.DropDatabase;
  Disconnect;
end;

procedure TDBDataModule.BackupDatabase;
begin
  BackupDlg.ShowModal;
end;

procedure TDBDataModule.RestoreDatabase;
var DefaultPageSize: integer;
    DefaultNumBuffers: integer;
begin
  DefaultPageSize := DatabaseQuery.FieldByName('MON$PAGE_SIZE').AsInteger;
  DefaultNumBuffers := DatabaseQuery.FieldByName('MON$PAGE_BUFFERS').AsInteger;
  IBDatabase1.Connected := false;
  try
    RestoreDlg.ShowModal(DefaultPageSize,DefaultNumBuffers);
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBDataModule.BringDatabaseOnline;
begin
  if IsDatabaseOnline then
    MessageDlg('Database is already online!',mtInformation,[mbOK],0)
  else
  begin
    IBDatabase1.Connected := false;
    try
      IBConfigService1.BringDatabaseOnline;
    finally
      IBDatabase1.Connected := true;
    end;
    if IsDatabaseOnline then
      MessageDlg('Database is back online',mtInformation,[mbOK],0)
    else
      MessageDlg('Database is still shutdown!',mtError,[mbOK],0);
  end;
end;

procedure TDBDataModule.ShutDown(aShutDownmode: TDBShutdownMode; aDelay: integer
  );
begin
  IBDatabase1.Connected := false;
  try
    ShutdownDatabaseDlg.Shutdown(aShutDownmode, aDelay);
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBDataModule.DatabaseRepair(Options: TValidateOptions; ReportLines: TStrings);

  procedure ReportOptions;
  var Line: string;
  begin
    Line := 'With Options: [';
    if (ValidateDB in Options) then Line += 'ValidateDB ';
    if (SweepDB in Options) then Line += 'SweepDB ';
    if (KillShadows in Options) then Line += 'KillShadows ';
    if (ValidateFull in Options) then Line += 'ValidateFull ';
    if (CheckDB in Options) then Line += 'CheckDB ';
    if (IgnoreChecksum in Options) then Line +='IgnoreChecksum ';
    if (MendDB in Options) then Line +='MendDB ';
    Line +=']';
    ReportLines.Add(Line);
  end;

begin
  ReportLines.Add(Format('Validation of %s started',[IBValidationService1.DatabaseName]));
  ReportOptions;
  if IBDatabase1.Connected then
  begin
    if IBDatabase1.DefaultTransaction.InTransaction then
      IBDatabase1.DefaultTransaction.Rollback;
    Application.ProcessMessages;
    IBDatabase1.Connected := false;
  end;
  with IBValidationService1 do
  try
    Execute(ReportLines);
    ReportLines.Add('Operation Completed');
    MessageDlg('Operation Completed',mtInformation,[mbOK],0);
  finally
    IBDatabase1.Params.Values['user_name'] := FDBUserName;
    IBDatabase1.Params.Values['password'] := FDBPassword;
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBDataModule.OnlineValidation(ReportLines: TStrings;
  SelectedTablesOnly: boolean);
var TableNames: string;
    Separator: string;
begin
  if IBDatabaseInfo.ODSMajorVersion < 12 then
    raise Exception.Create('Online Validation is not supported');
  with IBOnlineValidationService1 do
  begin
    if SelectedTablesOnly then
    begin
      TableNames := '';
      with DBTables do
      if Active then
      begin
        DisableControls;
        try
          Separator := '';
          First;
          while not EOF do
          begin
            if FieldByName('Selected').AsInteger <> 0 then
            begin
              TableNames += Separator + FieldByName('RDB$RELATION_NAME').AsString;
              Separator := '|';
            end;
            Next;
          end;
        finally
          EnableControls;
        end;
      end;
      IncludeTables := TableNames;
    end
    else
      IncludeTables := '';
    ReportLines.Add(Format('Online Validation of %s started',[IBOnlineValidationService1.DatabaseName]));
    Execute(ReportLines);
    ReportLines.Add('Online Validation Completed');
    MessageDlg('Online Validation Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TDBDataModule.LimboResolution(ActionID: TTransactionGlobalAction;
  Report: TStrings);
begin
  if not InLimboList.Active then
    raise Exception.Create('Limbo Transactions List not available');

  with InLimboList do
    if State = dsEdit then Post;
  Report.Clear;
  Report.Add('Starting Limbo transaction resolution');
  InLimboList.FixErrors(ActionID,Report);
  Report.Add('Limbo Transaction resolution complete');
  CurrentTransaction.Commit;
end;

function TDBDataModule.GetLingerDelay: string;
var Linger: TField;
begin
  Result := 'n/a';
  if not  DatabaseQuery.Active then exit;
  Linger := DatabaseQuery.FindField('RDB$LINGER');
  if Linger <> nil then
  begin
    if Linger.IsNull then
      Result := '0'
    else
      Result := Linger.AsString;
  end;
end;

function TDBDataModule.GetNoReserve: boolean;
begin
  Result :=  DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$RESERVE_SPACE').AsInteger <> 0);
end;

function TDBDataModule.GetPageBuffers: integer;
begin
  Result := IBDatabaseInfo.NumBuffers;
end;

function TDBDataModule.GetRoleName: string;
begin
  Result := Trim(AttmtQuery.FieldByName('MON$ROLE').AsString);
end;

function TDBDataModule.GetSecurityDatabase: string;
var SecPlugin: TField;
begin
  SecPlugin := DatabaseQuery.FindField('MON$SEC_DATABASE');
  if SecPlugin = nil then
    Result := 'Legacy'
  else
    Result := Trim(SecPlugin.AsString);
end;

function TDBDataModule.GetServerName: string;
begin
  Result := IBXServicesConnection1.ServerName;
end;

function TDBDataModule.GetSweepInterval: integer;
begin
  if DatabaseQuery.Active then
    Result :=  DatabaseQuery.FieldByName('MON$SWEEP_INTERVAL').AsInteger
  else
    Result := 0;
end;

function TDBDataModule.GetUserAdminPrivilege: boolean;
begin
  Result := IBDatabase1.RemoteProtocol = '';
  if Result then Exit;

  {For ODS 12 use SEC$USERS table}
  if IBDatabase1.Connected and (IBDatabaseInfo.ODSMajorVersion >= 12) then
  with AdminUserQuery do
  begin
    ExecQuery;
    try
      Result := not EOF and FieldByName('SEC$ADMIN').AsBoolean;
    finally
      Close;
    end;
  end
  {if need to know for ODS 11.2 then will have to use Service API}
  else
  begin
    with IBSecurityService1 do
    begin
      DisplayUser(ServiceUserName);
      Result := (UserInfoCount > 0) and UserInfo[0].AdminRole;
    end;
  end;
end;

procedure TDBDataModule.SetAutoAdmin(AValue: boolean);
begin
  IBSecurityService1.SetAutoAdmin(AValue);
  CurrentTransaction.Commit;
end;

procedure TDBDataModule.SetDBReadOnly(AValue: boolean);
begin
  IBDatabase1.Connected := false;
  try
    IBConfigService1.SetReadOnly(AValue);
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBDataModule.SetDBSQLDialect(AValue: integer);
begin
  IBDatabase1.Connected := false;
  try
    IBConfigService1.SetDBSqlDialect(AValue);
  finally
    IBDatabase1.Connected := true;
  end;
end;

procedure TDBDataModule.SetDescription(AValue: string);
begin
  with TIBSQL.Create(IBDatabase1) do
  try
    SQL.Text := 'Comment on Database is ''' + SQLSafeString(AValue) + '''';
    Transaction.Active := true;
    ExecQuery;
  finally
    Free;
  end;
  CurrentTransaction.Commit;
end;

procedure TDBDataModule.SetForcedWrites(AValue: boolean);
begin
  IBConfigService1.SetAsyncMode(not AValue);
end;

function TDBDataModule.IsDatabaseOnline: boolean;
begin
  Result := DatabaseQuery.Active and (DatabaseQuery.FieldByName('MON$SHUTDOWN_MODE').AsInteger = 0);
end;

function TDBDataModule.IsShadowDatabase: boolean;
begin
  GetDBFlags;
  Result := FIsShadowDatabase;
end;

procedure TDBDataModule.ActivateShadow;
var DBConnected: boolean;
begin
  DBConnected := IBDatabase1.Connected;
  IBDatabase1.Connected := false;
  try
    IBConfigService1.ActivateShadow;
  finally
    IBDatabase1.Connected := DBConnected;
  end;
  MessageDlg('Shadow Database activated. You should now rename the file or change the database alias name to point to the shadow',
    mtInformation,[mbOK],0);
end;

procedure TDBDataModule.AddSecondaryFile(aFileName: string; StartAt,
  FileLength: integer);
var SQLText: string;
begin
  if FileLength <> -1 then
    SQLText := Format(sAddSecondarySQL2,[aFileName,StartAt,FileLength])
  else
    SQLText := Format(sAddSecondarySQL,[aFileName,StartAt]);
  ExecDDL.SQL.Text := SQLText;
  ExecDDL.ExecQuery;
  CurrentTransaction.Commit;
end;

procedure TDBDataModule.AddShadowSet;
var CurrentLocation: TBookmark;
    ShadowSet: integer;
begin
  if ShadowFiles.RecordCount = 0 then
    ShadowSet := 1
  else
  with ShadowFiles do
  begin
    CurrentLocation := Bookmark;
    DisableControls;
    try
      Last;
      ShadowSet := FieldByName('RDB$Shadow_Number').AsInteger + 1;
    finally
      Bookmark := CurrentLocation;
      EnableControls
    end
  end;
  AddShadowSetDlg.ShowModal(ShadowSet);
  CurrentTransaction.Active := true;
end;

procedure TDBDataModule.RemoveShadowSet(ShadowSet: integer);
begin
  if IBDatabaseInfo.ODSMajorVersion < 12 then
  begin
    if MessageDlg(Format(sRemoveShadow,[ShadowSet]),mtConfirmation,[mbYes,mbNo],0) = mrYes then
       ExecDDL.SQL.Text := Format(sRemoveShadow,[ShadowSet]);
  end
  else
    case MessageDlg(Format(sPreserveShadowFiles,[ShadowSet]),mtConfirmation,[mbYes,mbNo,mbCancel],0) of
    mrNo:
      ExecDDL.SQL.Text  :=Format(sRemoveShadow12,[ShadowSet]);
    mrYes:
      ExecDDL.SQL.Text := Format(sPreserveShadow,[ShadowSet]);
    mrCancel:
      Exit;
    end;
  ExecDDL.ExecQuery;
  CurrentTransaction.Commit;
end;

procedure TDBDataModule.LoadPerformanceStatistics(Lines: TStrings);

  procedure AddPerfStats(Heading: string; stats: TStrings);
  var i: integer;
  begin
    with Lines do
    begin
      if stats.count = 0 then exit;
      Add('');
      Add(Heading);
      for i := 0 to stats.Count - 1 do
      begin
        if TableNameLookup.Locate('RDB$RELATION_ID',stats.Names[i],[]) then
          Add('  ' + TableNameLookup.FieldByName('RDB$RELATION_NAME').AsString + ' = ' + stats.ValueFromIndex[i]);
      end;
    end;
  end;

begin
  TableNameLookup.Active := true;
  with IBDatabaseInfo, Lines do
  begin
    Add(Format('Number of reads from the memory buffer cache = %d',[Fetches]));
    Add(Format('Number of writes to the memory buffer cache = %d',[Marks]));
    Add(Format('Number of page reads = %d',[Reads]));
    Add(Format('Number of page writes = %d',[Writes]));
    Add('');
    Add('Since Database last attached:');
    AddPerfStats('Number of removals of a version of a record',BackoutCount);
    AddPerfStats('Number of database deletes',DeleteCount);
    AddPerfStats('Number of removals of a committed record',ExpungeCount);
    AddPerfStats('Number of inserts',InsertCount);
    AddPerfStats('Number of removals of old versions of fully mature records',PurgeCount);
    AddPerfStats('Number of reads done via an index',ReadIdxCount);
    AddPerfStats('Number of sequential table scans',ReadSeqCount);
    AddPerfStats('Number of database updates',UpdateCount);
  end;
end;

procedure TDBDataModule.LoadDatabaseStatistics(OptionID: integer; Lines: TStrings);
begin
  if OptionID = 1 then
    LoadPerformanceStatistics(Lines)
  else
  with IBStatisticalService1 do
  begin
    case OptionID of
    0: Options := [HeaderPages];
    2: options := [DataPages];
    3: Options := [IndexPages];
    4: Options := [SystemRelations]
    end;
    Execute(Lines);
  end;
end;

function TDBDataModule.LoadConfigData(ConfigFileData: TConfigFileData): boolean;
var i: integer;
    aValue: integer;
begin
  ConfigDataset.Active := true;
  ConfigDataset.Clear(false);
  for i := 0 to Length(ConfigFileData.ConfigFileKey) - 1 do
  begin
    aValue := ConfigFileData.ConfigFileValue[i] ;
    with ConfigDataset do
    case ConfigFileData.ConfigFileKey[i] of
    ISCCFG_LOCKMEM_KEY:
      AppendRecord(['Lock mem', aValue]);
    ISCCFG_LOCKSEM_KEY:
      AppendRecord(['Lock Semaphores', aValue]);
    ISCCFG_LOCKSIG_KEY:
      AppendRecord(['Lock sig', aValue]);
    ISCCFG_EVNTMEM_KEY:
      AppendRecord(['Event mem', aValue]);
    ISCCFG_PRIORITY_KEY:
      AppendRecord(['Priority', aValue]);
    ISCCFG_MEMMIN_KEY:
      AppendRecord(['Min memory', aValue]);
    ISCCFG_MEMMAX_KEY:
      AppendRecord(['Max Memory', aValue]);
    ISCCFG_LOCKORDER_KEY:
      AppendRecord(['Lock order', aValue]);
    ISCCFG_ANYLOCKMEM_KEY:
      AppendRecord(['Any lock mem', aValue]);
    ISCCFG_ANYLOCKSEM_KEY:
      AppendRecord(['Any lock semaphore',aValue]);
    ISCCFG_ANYLOCKSIG_KEY:
      AppendRecord(['any lock sig', aValue]);
    ISCCFG_ANYEVNTMEM_KEY:
      AppendRecord(['any event mem', aValue]);
    ISCCFG_LOCKHASH_KEY:
      AppendRecord(['Lock hash', aValue]);
    ISCCFG_DEADLOCK_KEY:
      AppendRecord(['Deadlock', aValue]);
    ISCCFG_LOCKSPIN_KEY:
      AppendRecord(['Lock spin', aValue]);
    ISCCFG_CONN_TIMEOUT_KEY:
      AppendRecord(['Conn timeout', aValue]);
    ISCCFG_DUMMY_INTRVL_KEY:
      AppendRecord(['Dummy interval', aValue]);
    ISCCFG_IPCMAP_KEY:
      AppendRecord(['Map size', aValue]);
    ISCCFG_DBCACHE_KEY:
      AppendRecord(['Cache size', aValue]);
    end;
  end;
  Result := ConfigDataset.Active and (ConfigDataset.RecordCount > 0);
end;

procedure TDBDataModule.LoadServerProperties(Lines: TStrings);
var i: integer;
begin
  Lines.Clear;
  with IBServerProperties1 do
  begin
    Lines.Add('Server Version = ' + VersionInfo.ServerVersion);
    Lines.Add('Server Implementation = ' + VersionInfo.ServerImplementation);
    Lines.Add('Service Version = ' + IntToStr(VersionInfo.ServiceVersion));
    with ServicesConnection do
    Lines.Add(Format('Firebird Release = %d.%d.%d (Build no. %d)',[ServerVersionNo[1],
                                                             ServerVersionNo[2],
                                                             ServerVersionNo[3],
                                                             ServerVersionNo[4]]));
    Lines.Add('No. of attachments = ' + IntToStr(DatabaseInfo.NoOfAttachments));
    Lines.Add('No. of databases = ' + IntToStr(DatabaseInfo.NoOfDatabases));
    for i := 0 to length(DatabaseInfo.DbName) - 1 do
      Lines.Add(Format('DB Name (%d) = %s',[i+1, DatabaseInfo.DbName[i]]));
    Lines.Add('Base Location = ' + ConfigParams.BaseLocation);
    Lines.Add('Lock File Location = ' + ConfigParams.LockFileLocation);
    Lines.Add('Security Database Location = ' + ConfigParams.SecurityDatabaseLocation);
    Lines.Add('Message File Location = ' + ConfigParams.MessageFileLocation);
  end;
end;

procedure TDBDataModule.LoadServerLog(Lines: TStrings);
begin
  Lines.Clear;
  if IBLogService1.ServicesConnection.ServiceIntf.getProtocol = Local then
    Lines.Add('Server Log not available with embedded server')
  else
    IBLogService1.Execute(Lines);
end;

procedure TDBDataModule.RevokeAll;
begin
  with SubjectAccessRights do
  if Active then
  begin
    DisableControls;
    try
      First;
      while not EOF do
      begin
        if FieldByName('OBJECT_TYPE').AsInteger = 0 {relation} then
          ExecDDL.SQL.Text := Format('Revoke All on %s from %s',[
                  Trim(FieldByName('OBJECT_NAME').AsString),
                  Trim(FieldByName('SUBJECT_NAME').AsString)])
        else
        if FieldByName('OBJECT_TYPE').AsInteger = 13 {role} then
          ExecDDL.SQL.Text := Format('Revoke %s from %s',[
                  Trim(FieldByName('OBJECT_NAME').AsString),
                  Trim(FieldByName('SUBJECT_NAME').AsString)])
        else
          ExecDDL.SQL.Text := Format('Revoke All on %s %s from %s',[
                    Trim(FieldByName('OBJECT_TYPE_NAME').AsString),
                    Trim(FieldByName('OBJECT_NAME').AsString),
                    Trim(FieldByName('SUBJECT_NAME').AsString)]);
        ExecDDL.ExecQuery;
        Next;
      end;
    finally
      EnableControls;
    end;
    CurrentTransaction.Commit;
  end;
end;

procedure TDBDataModule.SyncSubjectAccessRights(ID: string);
begin
  if (FSubjectAccessRightsID = ID) and SubjectAccessRights.Active then Exit;
  SubjectAccessRights.Active := false;
  FSubjectAccessRightsID := ID;
  SubjectAccessRights.Active := true;
end;

procedure TDBDataModule.IBDatabase1Login(Database: TIBDatabase;
  LoginParams: TStrings);
var aDatabaseName: string;
    aUserName: string;
    aPassword: string;
    aCreateIfNotExist: boolean;
begin

  FDBUserName := LoginParams.Values['user_name'];
  FDBPassword := LoginParams.Values['password'];

  if FLocalConnect or (FDBPassword <> '') {reconnect}  then
  begin
    LoginParams.Values['user_name'] := FDBUserName;
    LoginParams.Values['password'] := FDBPassword;
    exit;
  end;

  aDatabaseName := Database.DatabaseName;
  aUserName := LoginParams.Values['user_name'];
  aPassword := '';
  aCreateIfNotExist := false;
  if CallLoginDlg(aDatabaseName, aUserName, aPassword, aCreateIfNotExist) = mrOK then
  begin
    FDBPassword := aPassword; {remember for reconnect}
    Database.DatabaseName := aDatabaseName;
    LoginParams.Values['user_name'] := aUserName;
    LoginParams.Values['password'] := aPassword;
    FDBUserName := aUserName;
    FDBPassword := aPassword;
    Database.CreateIfNotExists := aCreateIfNotExist;
    ParseConnectString(aDatabaseName,FServerName,FDatabasePathName,FProtocol,FPortNo);
  end
  else
    IBError(ibxeOperationCancelled, [nil]);
end;

procedure TDBDataModule.AttUpdateApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
begin
  if UpdateKind = ukDelete then
  begin
    ExecDDL.SQL.Text := 'Delete from MON$ATTACHMENTS Where MON$ATTACHMENT_ID =' +
      Params.ByName('MON$ATTACHMENT_ID').Asstring;
    ExecDDL.ExecQuery;
  end;
end;

procedure TDBDataModule.DBTablesUpdateApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
begin
  // Do nothing
end;

procedure TDBDataModule.IBValidationService1GetNextLine(Sender: TObject;
  var Line: string);
begin
  Application.ProcessMessages;
end;

procedure TDBDataModule.IBXServicesConnection1AfterConnect(Sender: TObject);
var UN: ISPBItem;
begin
  UN := IBXServicesConnection1.ServiceIntf.getSPB.Find(isc_spb_user_name);
  if UN <> nil then
    FServiceUserName := UN.AsString;
end;

procedure TDBDataModule.IBXServicesConnection1Login(
  Service: TIBXServicesConnection; var aServerName: string; LoginParams: TStrings);
begin
  LoginParams.Values['user_name'] := FDBUserName;
  LoginParams.Values['password'] := FDBPassword;
end;

procedure TDBDataModule.LegacyUserListAfterOpen(DataSet: TDataSet);
begin
  if UserListSource.DataSet <> LegacyUserList then
    UserListSource.DataSet := LegacyUserList;
  if IBDatabase1.Connected then
  begin
    CurrentTransaction.Active := true;
    RoleNameList.Active := true;
  end;
end;

procedure TDBDataModule.LegacyUserListAfterPost(DataSet: TDataSet);
begin
  if IBDatabase1.Connected then
    RoleNameList.Active := true;
end;

procedure TDBDataModule.LegacyUserListBeforeClose(DataSet: TDataSet);
begin
  RoleNameList.Active := false;
end;

procedure TDBDataModule.ShadowFilesCalcFields(DataSet: TDataSet);
var Flags: integer;
begin
  Flags := DataSet.FieldByName('RDB$FILE_FLAGS').AsInteger;
  if Flags and $10 <> 0 then
    DataSet.FieldByName('FileMode').AsString := 'C'
  else
  if Flags and $04 <> 0 then
    DataSet.FieldByName('FileMode').AsString := 'M'
  else
  if Flags and $01 <> 0 then
    if DataSet.FieldByName('RDB$FILE_SEQUENCE').AsInteger = 0 then
      DataSet.FieldByName('FileMode').AsString := 'A'
    else
      DataSet.FieldByName('FileMode').AsString := '+'
  else
    DataSet.FieldByName('FileMode').AsString := ''
end;

procedure TDBDataModule.SubjectAccessRightsBeforeOpen(DataSet: TDataSet);
begin
  SubjectAccessRights.ParamByName('ID').AsString := FSubjectAccessRightsID;
end;

procedure TDBDataModule.TagsUpdateApplyUpdates(Sender: TObject;
  UpdateKind: TUpdateKind; Params: ISQLParams);
var sql: string;
begin
  sql := '';
  case UpdateKind of
  ukInsert,
  ukModify:
    begin
      sql := 'ALTER USER ' + Trim(Params.ByName('SEC$USER_NAME').AsString)
         + ' TAGS (' + QuoteIdentifierIfNeeded(IBDatabase1.SQLDialect,Params.ByName('SEC$KEY').AsString)
         + '=''' + SQLSafeString(Params.ByName('SEC$VALUE').AsString) + '''';
      if Params.ByName('SEC$KEY').AsString <> Params.ByName('OLD_SEC$KEY').AsString then
        sql += ', DROP ' + QuoteIdentifierIfNeeded(IBDatabase1.SQLDialect,Params.ByName('OLD_SEC$KEY').AsString);
      sql +=')'
    end;

  ukDelete:
    sql := 'ALTER USER ' + Trim(Params.ByName('SEC$USER_NAME').AsString)
         + ' TAGS (DROP ' + QuoteIdentifierIfNeeded(IBDatabase1.SQLDialect,Params.ByName('SEC$KEY').AsString) + ')';
  end;
  ExecDDL.SQL.Text := sql;
  ExecDDL.ExecQuery;
end;

procedure TDBDataModule.IBDatabase1AfterConnect(Sender: TObject);
begin
  {Virtual tables did not exist prior to Firebird 2.1 - so don't bother with old version}
  with IBDatabaseInfo do
    if (ODSMajorVersion < 11) or ((ODSMajorVersion = 11) and (ODSMinorVersion < 1)) then
    begin
      IBDatabase1.Connected := false;
      raise Exception.Create('This application requires Firebird 2.1 or later');
    end
    else
    if ODSMajorVersion < 12 then
    {Don't expect to be able to find these fields}
    begin
      AttachmentsMONCLIENT_VERSION.FieldKind := fkCalculated;
      AttachmentsMONREMOTE_VERSION.FieldKind := fkCalculated;
      AttachmentsMONREMOTE_HOST.FieldKind := fkCalculated;
      AttachmentsMONREMOTE_OS_USER.FieldKind := fkCalculated;
      AttachmentsMONAUTH_METHOD.FieldKind := fkCalculated;
      AttachmentsMONSYSTEM_FLAG.FieldKind := fkCalculated;
      AttachmentsRDBSECURITY_CLASS.FieldKind := fkCalculated;
      AttachmentsRDBOWNER_NAME.FieldKind := fkCalculated;
    end
    else
    begin
      AttachmentsMONCLIENT_VERSION.FieldKind := fkData;
      AttachmentsMONREMOTE_VERSION.FieldKind := fkData;
      AttachmentsMONREMOTE_HOST.FieldKind := fkData;
      AttachmentsMONREMOTE_OS_USER.FieldKind := fkData;
      AttachmentsMONAUTH_METHOD.FieldKind := fkData;
      AttachmentsMONSYSTEM_FLAG.FieldKind := fkData;
      AttachmentsRDBSECURITY_CLASS.FieldKind := fkData;
      AttachmentsRDBOWNER_NAME.FieldKind := fkData;
    end;

  FLocalConnect := FProtocol = Local;
  ConnectServicesAPI;
  CurrentTransaction.Active := true;
  FHasUserAdminPrivilege := GetUserAdminPrivilege;
  ReloadData;
end;

procedure TDBDataModule.IBDatabase1AfterDisconnect(Sender: TObject);
begin
  FDisconnecting := false;
end;

procedure TDBDataModule.IBDatabase1BeforeConnect(Sender: TObject);
begin
end;

procedure TDBDataModule.IBDatabase1BeforeDisconnect(Sender: TObject);
begin
  FDBHeaderScanned := false;
  FDisconnecting := true;
end;

procedure TDBDataModule.DatabaseQueryAfterOpen(DataSet: TDataSet);
begin
  DBCharSet.Active := true;
end;

procedure TDBDataModule.CurrentTransactionAfterTransactionEnd(Sender: TObject);
begin
  if not Disconnecting and not (csDestroying in ComponentState) then
  begin
    CurrentTransaction.Active := true;
    Application.QueueAsyncCall(@ReloadData,0);
  end;
end;

procedure TDBDataModule.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  if E is EIBInterBaseError then
  begin
    if RoleNameList.State in [dsInsert,dsEdit] then
      RoleNameList.Cancel;
    if UserList.State in [dsInsert,dsEdit] then
      UserList.Cancel;
  end;
  MessageDlg(E.Message,mtError,[mbOK],0);
  if CurrentTransaction.Active then
    CurrentTransaction.Rollback;
end;

procedure TDBDataModule.AccessRightsCalcFields(DataSet: TDataSet);
begin
  AccessRightsDisplayName.AsString := AccessRightsSUBJECT_NAME.AsString;
  if AccessRightsSUBJECT_TYPE.AsInteger = 8 then
  begin
    if  (AccessRightsSUBJECT_NAME.AsString <> 'PUBLIC') and UserListSource.DataSet.Active and
       not UserListSource.DataSet.Locate('SEC$USER_NAME',AccessRightsSUBJECT_NAME.AsString,[]) then
    begin
      AccessRightsImageIndex.AsInteger := 4;
      AccessRightsDisplayName.AsString := AccessRightsSUBJECT_NAME.AsString + ' (stale)';
    end
    else
      AccessRightsImageIndex.AsInteger := -1
  end
  else
    AccessRightsImageIndex.AsInteger := -1;
end;

procedure TDBDataModule.AttachmentsAfterDelete(DataSet: TDataSet);
begin
  CurrentTransaction.Commit;
end;

procedure TDBDataModule.AttachmentsAfterOpen(DataSet: TDataSet);
begin
  Attachments.Locate('MON$ATTACHMENT_ID',AttmtQuery.FieldByName('MON$ATTACHMENT_ID').AsInteger,[]);
end;

procedure TDBDataModule.AttachmentsBeforeOpen(DataSet: TDataSet);
begin
  if IBDatabaseInfo.ODSMajorVersion >= 12 then
    (DataSet as TIBQuery).Parser.Add2WhereClause('r.MON$SYSTEM_FLAG = 0');
end;

procedure TDBDataModule.ConfigDatasetAfterClose(DataSet: TDataSet);
begin
  ConfigDataset.Clear(false);
end;

procedure TDBDataModule.DatabaseQueryBeforeClose(DataSet: TDataSet);
begin
  DBCharSet.Active := false;
end;

procedure TDBDataModule.DBCharSetAfterClose(DataSet: TDataSet);
begin
  CharSetLookup.Active := false;
end;

procedure TDBDataModule.DBCharSetBeforeOpen(DataSet: TDataSet);
begin
  CharSetLookup.Active := true;
end;

end.

