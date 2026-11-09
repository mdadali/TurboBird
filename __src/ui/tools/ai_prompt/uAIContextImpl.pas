unit uAIContextImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, IBDatabase, IBQuery, IB,
  fpjson, jsonparser,
  turbocommon,
  uAIContext;

type
  { Implementation of IAIContextProvider - ausgelagert aus main.pas wegen Compiler-Limit }
  TAIContextProvider = class(TInterfacedObject, IAIContextProvider)
  private
    FMainForm: TObject; // TfmMain reference
    function GetMainForm: TObject;
  public
    constructor Create(AMainForm: TObject);
    
    // ===== AKTUELLE VERBINDUNG / DATENBANK =====
    function GetCurrentDatabaseIndex: Integer;
    function GetCurrentDatabaseName: string;
    function GetCurrentServerName: string;
    function GetCurrentServerAlias: string;
    function GetIBDatabase: TIBDatabase;
    function GetIBTransaction: TIBTransaction;
    function IsConnected: Boolean;

    // Selektierte Tabelle im TreeView
    function GetSelectedTableName: string;
    function HasTableSelected: Boolean;

    // Aktuelles Query Window
    function GetActiveQueryWindow: TComponent;
    function GetCurrentQueryText: string;
    function GetActiveQueryCaption: string;

    // Schema-Informationen (aktuell verbundene DB)
    function GetTableSchema(ATableName: string; AWithIndexes: Boolean = True): string;
    function GetTableDDL(ATableName: string): string;
    function GetVisibleTableNames: TStringList;
    function GetLastError: string;

    // ===== TURBOBIRD SYSTEM: REGISTRIERTE SERVER =====
    function GetRegisteredServerCount: Integer;
    function GetServerInfo(AIndex: Integer): string;
    function GetServerInfoAsJSON(AIndex: Integer): string;
    function FindServerIndexByName(AName: string): Integer;
    function FindServerIndexByAlias(AAlias: string): Integer;

    // ===== TURBOBIRD SYSTEM: REGISTRIERTE DATENBANKEN =====
    function GetRegisteredDatabaseCount: Integer;
    function GetDatabaseInfo(AIndex: Integer): string;
    function GetDatabaseInfoAsJSON(AIndex: Integer): string;
    function GetDatabasesForServer(AServerAlias: string): string;
    function FindDatabaseIndexByTitle(ATitle: string): Integer;

    // ===== VERBINDUNGS-MANAGEMENT =====
    function ConnectToDatabase(ADBIndex: Integer): Boolean;
    function DisconnectDatabase(ADBIndex: Integer): Boolean;
    function IsDatabaseConnected(ADBIndex: Integer): Boolean;

    // ===== METADATEN (für beliebige registrierte DB) =====
    function GetDatabaseMetadata(ADBIndex: Integer; AObjectType: string = 'tables'): string;
    function GetDatabaseTableList(ADBIndex: Integer): string;
    function GetDatabaseViewList(ADBIndex: Integer): string;
    function GetDatabaseProcedureList(ADBIndex: Integer): string;
    function GetDatabaseFunctionList(ADBIndex: Integer): string;
    function GetDatabaseTriggerList(ADBIndex: Integer): string;
    function GetDatabaseGeneratorList(ADBIndex: Integer): string;
    function GetDatabaseDomainList(ADBIndex: Integer): string;
    function GetDatabaseRoleList(ADBIndex: Integer): string;
    function GetDatabaseUserList(ADBIndex: Integer): string;

    // ===== SERVER-STATUS & ADMIN =====
    function TestServerConnection(AServerIndex: Integer): string;
    function GetServerInventoryAsText: string;
    function GetServerInventoryAsJSON: string;

    // High-Level Context String für Prompt-Injection
    function BuildContextString: string;
    function BuildFullSystemContextString: string;
  end;

implementation

uses
  main, QueryWindow, SynEdit, IBExtract, IBTable, IBCustomDataSet, Forms,
  DateUtils,  // for DateTimeToStr
  Variants;   // for VarToStr

{ TAIContextProvider }

constructor TAIContextProvider.Create(AMainForm: TObject);
begin
  inherited Create;
  FMainForm := AMainForm;
end;

function TAIContextProvider.GetMainForm: TObject;
begin
  Result := FMainForm;
end;

// ===== AKTUELLE VERBINDUNG / DATENBANK =====

function TAIContextProvider.GetCurrentDatabaseIndex: Integer;
var
  NodeInfo: TPNodeInfos;
  MainForm: TfmMain;
begin
  Result := -1;
  MainForm := TfmMain(GetMainForm);
  if not Assigned(MainForm) or not Assigned(MainForm.tvMain.Selected) then Exit;
  NodeInfo := TPNodeInfos(MainForm.tvMain.Selected.Data);
  if Assigned(NodeInfo) then
    Result := NodeInfo^.dbIndex;
end;

function TAIContextProvider.GetCurrentDatabaseName: string;
var
  dbIndex: Integer;
begin
  Result := '';
  dbIndex := GetCurrentDatabaseIndex;
  if (dbIndex >= 0) and (dbIndex < Length(RegisteredDatabases)) then
    Result := RegisteredDatabases[dbIndex].RegRec.Title;
end;

function TAIContextProvider.GetCurrentServerName: string;
var
  dbIndex: Integer;
begin
  Result := '';
  dbIndex := GetCurrentDatabaseIndex;
  if (dbIndex >= 0) and (dbIndex < Length(RegisteredDatabases)) then
    Result := RegisteredDatabases[dbIndex].RegRec.ServerName;
end;

function TAIContextProvider.GetCurrentServerAlias: string;
var
  dbIndex: Integer;
begin
  Result := '';
  dbIndex := GetCurrentDatabaseIndex;
  if (dbIndex >= 0) and (dbIndex < Length(RegisteredDatabases)) then
  begin
    if RegisteredDatabases[dbIndex].RegRec.Title <> '' then
      Result := RegisteredDatabases[dbIndex].RegRec.Title
    else
      Result := RegisteredDatabases[dbIndex].RegRec.ServerName;
  end;
end;

function TAIContextProvider.GetIBDatabase: TIBDatabase;
var
  dbIndex: Integer;
begin
  Result := nil;
  dbIndex := GetCurrentDatabaseIndex;
  if (dbIndex >= 0) and (dbIndex < Length(RegisteredDatabases)) then
    Result := RegisteredDatabases[dbIndex].IBDatabase;
end;

function TAIContextProvider.GetIBTransaction: TIBTransaction;
var
  dbIndex: Integer;
begin
  Result := nil;
  dbIndex := GetCurrentDatabaseIndex;
  if (dbIndex >= 0) and (dbIndex < Length(RegisteredDatabases)) then
    Result := RegisteredDatabases[dbIndex].IBTransaction;
end;

function TAIContextProvider.IsConnected: Boolean;
var
  db: TIBDatabase;
begin
  Result := False;
  db := GetIBDatabase;
  if Assigned(db) then
    Result := db.Connected;
end;

// Selektierte Tabelle im TreeView
function TAIContextProvider.GetSelectedTableName: string;
var
  NodeInfo: TPNodeInfos;
  MainForm: TfmMain;
begin
  Result := '';
  MainForm := TfmMain(GetMainForm);
  if not Assigned(MainForm) or not Assigned(MainForm.tvMain.Selected) then Exit;
  NodeInfo := TPNodeInfos(MainForm.tvMain.Selected.Data);
  if Assigned(NodeInfo) and (NodeInfo^.ObjectType = tvotTable) then
    Result := MainForm.tvMain.Selected.Text;
end;

function TAIContextProvider.HasTableSelected: Boolean;
begin
  Result := (GetSelectedTableName <> '');
end;

// Aktuelles Query Window
function TAIContextProvider.GetActiveQueryWindow: TComponent;
var
  i: Integer;
  MainForm: TfmMain;
begin
  Result := nil;
  MainForm := TfmMain(GetMainForm);
  if not Assigned(MainForm) then Exit;
  
  for i := 0 to MainForm.PageControl1.PageCount - 1 do
  begin
    if MainForm.PageControl1.Pages[i].TabVisible and
       (MainForm.PageControl1.ActivePage = MainForm.PageControl1.Pages[i]) then
    begin
      if Assigned(MainForm.PageControl1.Pages[i].Controls[0]) and
         (MainForm.PageControl1.Pages[i].Controls[0] is TfmQueryWindow) then
      begin
        Result := MainForm.PageControl1.Pages[i].Controls[0];
        Exit;
      end;
    end;
  end;
  for i := 0 to Application.ComponentCount - 1 do
    if Application.Components[i] is TfmQueryWindow then
    begin
      Result := Application.Components[i];
      Exit;
    end;
end;

function TAIContextProvider.GetCurrentQueryText: string;
var
  QW: TfmQueryWindow;
begin
  Result := '';
  QW := TfmQueryWindow(GetActiveQueryWindow);
  if Assigned(QW) then
    Result := QW.meQuery.Text;
end;

function TAIContextProvider.GetActiveQueryCaption: string;
var
  QW: TfmQueryWindow;
begin
  Result := '';
  QW := TfmQueryWindow(GetActiveQueryWindow);
  if Assigned(QW) then
    Result := QW.Caption;
end;

// ===== TURBOBIRD SYSTEM: REGISTRIERTE SERVER =====
// Schema-Informationen (direkte Implementierung via globale Arrays)
function TAIContextProvider.GetTableSchema(ATableName: string; AWithIndexes: Boolean): string;
var
  dbIndex: Integer;
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  SL: TStringList;
  Fields, Indexes: string;
  FieldName, FieldType, SubType, DefaultVal, NotNull: string;
  IndexName, IndexFields, IndexType: string;
  NodeInfo: TPNodeInfos;
  MainForm: TfmMain;
  pkFields: TStringList;
begin
  Result := '';
  // Get current database index from selected tree node
  MainForm := TfmMain(GetMainForm);
  if not Assigned(MainForm) or not Assigned(MainForm.tvMain.Selected) then Exit;
  NodeInfo := TPNodeInfos(MainForm.tvMain.Selected.Data);
  if not Assigned(NodeInfo) then Exit;
  dbIndex := NodeInfo^.dbIndex;
  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[dbIndex].IBDatabase;
  tr := RegisteredDatabases[dbIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  q := TIBQuery.Create(nil);
  SL := TStringList.Create;
  try
    q.Database := db;
    q.Transaction := tr;

    // Get field information
    q.Close;
    q.SQL.Text :=
      'SELECT r.RDB$FIELD_NAME, f.RDB$FIELD_TYPE, f.RDB$FIELD_SUB_TYPE, ' +
      '       f.RDB$FIELD_LENGTH, f.RDB$FIELD_PRECISION, f.RDB$FIELD_SCALE, ' +
      '       f.RDB$DEFAULT_SOURCE, f.RDB$NULL_FLAG, f.RDB$CHARACTER_SET_ID, ' +
      '       f.RDB$COLLATION_ID, ' +
      '       CASE WHEN f.RDB$FIELD_TYPE IN (7,8,9,10,11,12,13,14,16,27,35,37) THEN 1 ELSE 0 END AS IsArray, ' +
      '       r.RDB$FIELD_POSITION ' +
      'FROM RDB$RELATION_FIELDS r ' +
      'JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'WHERE r.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(ATableName)) +
      ' ORDER BY r.RDB$FIELD_POSITION';
    q.Open;

    Fields := '';
    while not q.EOF do
    begin
      FieldName := Trim(q.FieldByName('RDB$FIELD_NAME').AsString);
      FieldType := '';
      case q.FieldByName('RDB$FIELD_TYPE').AsInteger of
        7: FieldType := 'SMALLINT';
        8: FieldType := 'INTEGER';
        9: FieldType := 'QUAD';
        10: FieldType := 'FLOAT';
        11: FieldType := 'DOUBLE PRECISION';
        12: FieldType := 'DATE';
        13: FieldType := 'TIME';
        14: FieldType := 'TIMESTAMP';
        16: FieldType := 'BIGINT';
        27: FieldType := 'DOUBLE PRECISION'; // INT64
        35: FieldType := 'TIMESTAMP WITH TIME ZONE';
        37: FieldType := 'TIME WITH TIME ZONE';
        261: FieldType := 'BLOB';
        else
          if q.FieldByName('RDB$FIELD_TYPE').AsInteger >= 261 then
            FieldType := 'BLOB'
          else
            FieldType := 'UNKNOWN(' + q.FieldByName('RDB$FIELD_TYPE').AsString + ')';
      end;

      SubType := q.FieldByName('RDB$FIELD_SUB_TYPE').AsString;
      if (FieldType = 'BLOB') and (SubType <> '') then
        FieldType := FieldType + ' SUB_TYPE ' + SubType;

      if q.FieldByName('RDB$FIELD_LENGTH').AsInteger > 0 then
      begin
        if FieldType = 'BLOB' then
          FieldType := FieldType + ' SEGMENT SIZE ' + q.FieldByName('RDB$FIELD_LENGTH').AsString
        else if (FieldType = 'VARCHAR') or (FieldType = 'CHAR') then
          FieldType := FieldType + '(' + q.FieldByName('RDB$FIELD_LENGTH').AsString + ')';
      end;

      DefaultVal := q.FieldByName('RDB$DEFAULT_SOURCE').AsString;
      NotNull := '';
      if q.FieldByName('RDB$NULL_FLAG').AsInteger = 1 then
        NotNull := ' NOT NULL';

      Fields := Fields + '  ' + FieldName + ' ' + FieldType + NotNull;
      if DefaultVal <> '' then
        Fields := Fields + ' DEFAULT ' + DefaultVal;
      Fields := Fields + ',' + LineEnding;

      q.Next;
    end;

    // Remove trailing comma
    if (Fields <> '') and (Fields[Length(Fields)] = ',') then
      SetLength(Fields, Length(Fields) - 1);

    // Get Primary Key
    SL.Clear;
    // Note: GetPrimaryKeyFields is in main.pas, so we query directly
    q.Close;
    q.SQL.Text :=
      'SELECT s.RDB$FIELD_NAME FROM RDB$INDEX_SEGMENTS s ' +
      'JOIN RDB$INDICES i ON s.RDB$INDEX_NAME = i.RDB$INDEX_NAME ' +
      'WHERE i.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(ATableName)) +
      '  AND i.RDB$UNIQUE_FLAG = 1 ' +
      '  AND i.RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY s.RDB$FIELD_POSITION';
    q.Open;
    pkFields := TStringList.Create;
    try
      while not q.EOF do
      begin
        pkFields.Add(Trim(q.FieldByName('RDB$FIELD_NAME').AsString));
        q.Next;
      end;
      if pkFields.Count > 0 then
      begin
        Fields := Fields + ',' + LineEnding;
        Fields := Fields + '  PRIMARY KEY (' + pkFields.CommaText + ')';
      end;
    finally
      pkFields.Free;
    end;

    // Build CREATE TABLE
    Result := 'CREATE TABLE ' + ATableName + ' (' + LineEnding + Fields + LineEnding + ');';

    // Get Indexes if requested
    if AWithIndexes then
    begin
      q.Close;
      q.SQL.Text :=
        'SELECT i.RDB$INDEX_NAME, i.RDB$INDEX_TYPE, ' +
        '       LIST(s.RDB$FIELD_NAME, '','' ORDER BY s.RDB$FIELD_POSITION) AS Fields, ' +
        '       i.RDB$UNIQUE_FLAG ' +
        'FROM RDB$INDICES i ' +
        'JOIN RDB$INDEX_SEGMENTS s ON i.RDB$INDEX_NAME = s.RDB$INDEX_NAME ' +
        'WHERE i.RDB$RELATION_NAME = ' + QuotedStr(UpperCase(ATableName)) +
        '  AND i.RDB$SYSTEM_FLAG = 0 ' +
        'GROUP BY i.RDB$INDEX_NAME, i.RDB$INDEX_TYPE, i.RDB$UNIQUE_FLAG ' +
        'ORDER BY i.RDB$INDEX_NAME';
      q.Open;

      Indexes := '';
      while not q.EOF do
      begin
        IndexName := Trim(q.FieldByName('RDB$INDEX_NAME').AsString);
        IndexFields := q.FieldByName('Fields').AsString;
        IndexType := '';
        if q.FieldByName('RDB$UNIQUE_FLAG').AsInteger = 1 then
          IndexType := 'UNIQUE ';
        Indexes := Indexes + 'CREATE ' + IndexType + 'INDEX ' + IndexName +
                   ' ON ' + ATableName + ' (' + IndexFields + ');' + LineEnding;
        q.Next;
      end;

      if Indexes <> '' then
        Result := Result + LineEnding + LineEnding + Indexes;
    end;

  finally
    q.Free;
    SL.Free;
  end;
end;

function TAIContextProvider.GetTableDDL(ATableName: string): string;
begin
  Result := GetTableSchema(ATableName, True);
end;

function TAIContextProvider.GetVisibleTableNames: TStringList;
var
  dbIndex: Integer;
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  NodeInfo: TPNodeInfos;
  MainForm: TfmMain;
begin
  Result := TStringList.Create;
  MainForm := TfmMain(GetMainForm);
  if not Assigned(MainForm) or not Assigned(MainForm.tvMain.Selected) then Exit;
  NodeInfo := TPNodeInfos(MainForm.tvMain.Selected.Data);
  if not Assigned(NodeInfo) then Exit;
  dbIndex := NodeInfo^.dbIndex;
  if (dbIndex < 0) or (dbIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[dbIndex].IBDatabase;
  tr := RegisteredDatabases[dbIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 AND RDB$RELATION_TYPE = 0 ' +
      'ORDER BY RDB$RELATION_NAME';
    q.Open;
    while not q.EOF do
    begin
      Result.Add(Trim(q.FieldByName('RDB$RELATION_NAME').AsString));
      q.Next;
    end;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetLastError: string;
begin
  Result := '';
end;

function TAIContextProvider.GetRegisteredServerCount: Integer;
begin
  Result := Length(RegisteredServers);
end;

function TAIContextProvider.GetServerInfo(AIndex: Integer): string;
var
  ServerRec: TServerRecord;
  VersionStr, ProtocolStr: string;
begin
  Result := '';
  if (AIndex < 0) or (AIndex >= Length(RegisteredServers)) then Exit;

  ServerRec := RegisteredServers[AIndex];

  if (ServerRec.VersionMajor > 0) or (ServerRec.VersionMinor > 0) then
    VersionStr := 'Firebird ' + IntToStr(ServerRec.VersionMajor) + '.' + IntToStr(ServerRec.VersionMinor)
  else
    VersionStr := 'Firebird (unknown version)';

  ProtocolStr := IBXProtocolToString(ServerRec.Protocol);

  Result := 'Server: ' + ServerRec.ServerName +
            ' | Alias: ' + ServerRec.ServerAlias +
            ' | Version: ' + VersionStr +
            ' | Protocol: ' + ProtocolStr;
  if Trim(ServerRec.Port) <> '' then
    Result := Result + ' | Port: ' + Trim(ServerRec.Port);
  if ServerRec.IsEmbedded then
    Result := Result + ' | Embedded: Yes';
end;

function TAIContextProvider.GetServerInfoAsJSON(AIndex: Integer): string;
var
  ServerRec: TServerRecord;
  VersionStr, ProtocolStr: string;
  JObj: TJSONObject;
begin
  Result := '{}';
  if (AIndex < 0) or (AIndex >= Length(RegisteredServers)) then Exit;

  ServerRec := RegisteredServers[AIndex];
  JObj := TJSONObject.Create;
  try
    JObj.Add('index', AIndex);
    JObj.Add('server_name', ServerRec.ServerName);
    JObj.Add('server_alias', ServerRec.ServerAlias);

    if (ServerRec.VersionMajor > 0) or (ServerRec.VersionMinor > 0) then
      VersionStr := 'Firebird ' + IntToStr(ServerRec.VersionMajor) + '.' + IntToStr(ServerRec.VersionMinor)
    else
      VersionStr := 'Firebird (unknown version)';
    JObj.Add('version_string', VersionStr);
    JObj.Add('version_major', ServerRec.VersionMajor);
    JObj.Add('version_minor', ServerRec.VersionMinor);

    ProtocolStr := IBXProtocolToString(ServerRec.Protocol);
    JObj.Add('protocol', ProtocolStr);
    JObj.Add('port', ServerRec.Port);
    JObj.Add('charset', ServerRec.Charset);
    JObj.Add('root_path', ServerRec.RootPath);
    JObj.Add('client_library_path', ServerRec.ClientLibraryPath);
    JObj.Add('config_file_path', ServerRec.ConfigFilePath);
    JObj.Add('load_registered_client_lib', ServerRec.LoadRegisteredClientLib);
    JObj.Add('is_embedded', ServerRec.IsEmbedded);
    JObj.Add('connect_timeout_ms', ServerRec.ConnectTimeoutMS);
    JObj.Add('retry_count', ServerRec.RetryCount);
    JObj.Add('query_timeout_ms', ServerRec.QueryTimeoutMS);
    JObj.Add('username', ServerRec.UserName);
    JObj.Add('role', ServerRec.Role);

    Result := JObj.AsJSON;
  finally
    JObj.Free;
  end;
end;

function TAIContextProvider.FindServerIndexByName(AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(RegisteredServers) do
    if SameText(RegisteredServers[i].ServerName, AName) then
    begin
      Result := i;
      Exit;
    end;
end;

function TAIContextProvider.FindServerIndexByAlias(AAlias: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(RegisteredServers) do
    if SameText(RegisteredServers[i].ServerAlias, AAlias) then
    begin
      Result := i;
      Exit;
    end;
end;

// ===== TURBOBIRD SYSTEM: REGISTRIERTE DATENBANKEN =====

function TAIContextProvider.GetRegisteredDatabaseCount: Integer;
begin
  Result := Length(RegisteredDatabases);
end;

function TAIContextProvider.GetDatabaseInfo(AIndex: Integer): string;
var
  DBRec: TRegisteredDatabase;
begin
  Result := '';
  if (AIndex < 0) or (AIndex >= Length(RegisteredDatabases)) then Exit;

  DBRec := RegisteredDatabases[AIndex].RegRec;
  Result := 'Title: ' + DBRec.Title +
            ' | Server: ' + DBRec.ServerName +
            ' | Database: ' + DBRec.DatabaseName +
            ' | User: ' + DBRec.UserName +
            ' | Charset: ' + DBRec.Charset +
            ' | Dialect: ' + DBRec.SQLDialect +
            ' | Embedded: ' + BoolToStr(DBRec.IsEmbedded, True) +
            ' | Deleted: ' + BoolToStr(DBRec.Deleted, True) +
            ' | SavePassword: ' + BoolToStr(DBRec.SavePassword, True);
  if DBRec.LastOpened > 0 then
    Result := Result + ' | LastOpened: ' + DateTimeToStr(DBRec.LastOpened);
end;

function TAIContextProvider.GetDatabaseInfoAsJSON(AIndex: Integer): string;
var
  DBRec: TRegisteredDatabase;
  JObj: TJSONObject;
begin
  Result := '{}';
  if (AIndex < 0) or (AIndex >= Length(RegisteredDatabases)) then Exit;

  DBRec := RegisteredDatabases[AIndex].RegRec;
  JObj := TJSONObject.Create;
  try
    JObj.Add('index', AIndex);
    JObj.Add('title', DBRec.Title);
    JObj.Add('database_name', DBRec.DatabaseName);
    JObj.Add('server_name', DBRec.ServerName);
    JObj.Add('username', DBRec.UserName);
    JObj.Add('role', DBRec.Role);
    JObj.Add('charset', DBRec.Charset);
    JObj.Add('sql_dialect', DBRec.SQLDialect);
    JObj.Add('deleted', DBRec.Deleted);
    JObj.Add('save_password', DBRec.SavePassword);
    JObj.Add('last_opened', DateTimeToStr(DBRec.LastOpened));
    JObj.Add('firebird_client_lib_path', DBRec.FireBirdClientLibPath);
    JObj.Add('overwrite_loaded_client_lib', DBRec.OverwriteLoadedClientLib);
    JObj.Add('port', DBRec.Port);
    JObj.Add('connect_on_app_start', DBRec.ConnectOnApplicationStart);
    JObj.Add('server_version_major', DBRec.ServerVersionMajor);
    JObj.Add('server_version_string', DBRec.ServerVersionString);
    JObj.Add('server_version_minor', DBRec.ServerVersionMinor);
    JObj.Add('is_embedded', DBRec.IsEmbedded);
    JObj.Add('tx_config', DBRec.TxConfig);

    Result := JObj.AsJSON;
  finally
    JObj.Free;
  end;
end;

function TAIContextProvider.GetDatabasesForServer(AServerAlias: string): string;
var
  i: Integer;
  DBRec: TRegisteredDatabase;
  JArr: TJSONArray;
  JObj: TJSONObject;
begin
  Result := '[]';
  JArr := TJSONArray.Create;
  try
    for i := 0 to High(RegisteredDatabases) do
    begin
      DBRec := RegisteredDatabases[i].RegRec;
      if DBRec.Deleted then Continue;

      if SameText(Trim(DBRec.ServerName), Trim(AServerAlias)) then
      begin
        JObj := TJSONObject.Create;
        JObj.Add('index', i);
        JObj.Add('title', DBRec.Title);
        JObj.Add('database_name', DBRec.DatabaseName);
        JObj.Add('username', DBRec.UserName);
        JObj.Add('charset', DBRec.Charset);
        JObj.Add('is_embedded', DBRec.IsEmbedded);
        JArr.Add(JObj);
      end;
    end;
    Result := JArr.AsJSON;
  finally
    JArr.Free;
  end;
end;

function TAIContextProvider.FindDatabaseIndexByTitle(ATitle: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.Title, ATitle) then
    begin
      Result := i;
      Exit;
    end;
end;

// ===== VERBINDUNGS-MANAGEMENT =====

function TAIContextProvider.ConnectToDatabase(ADBIndex: Integer): Boolean;
var
  Rec: TDatabaseRec;
begin
  Result := False;
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  Rec := RegisteredDatabases[ADBIndex];
  if Rec.IBDatabase.Connected then
  begin
    Result := True;
    Exit;
  end;

  try
    Rec.IBDatabase.Params.Clear;
    Rec.IBDatabase.Params.Add('user_name=' + Rec.RegRec.UserName);
    if Rec.RegRec.SavePassword and (Rec.RegRec.Password <> '') then
      Rec.IBDatabase.Params.Add('password=' + Rec.RegRec.Password);
    if Rec.RegRec.Role <> '' then
      Rec.IBDatabase.Params.Add('sql_role_name=' + Rec.RegRec.Role);
    if Rec.RegRec.Charset <> '' then
      Rec.IBDatabase.Params.Add('lc_ctype=' + Rec.RegRec.Charset);

    Rec.IBDatabase.Connected := True;
    Result := Rec.IBDatabase.Connected;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TAIContextProvider.DisconnectDatabase(ADBIndex: Integer): Boolean;
var
  Rec: TDatabaseRec;
begin
  Result := False;
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  Rec := RegisteredDatabases[ADBIndex];
  try
    if Rec.IBTransaction.InTransaction then
      Rec.IBTransaction.Rollback;
    Rec.IBDatabase.Connected := False;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TAIContextProvider.IsDatabaseConnected(ADBIndex: Integer): Boolean;
begin
  Result := False;
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;
  Result := RegisteredDatabases[ADBIndex].IBDatabase.Connected;
end;

// ===== METADATEN (für beliebige registrierte DB) =====

function TAIContextProvider.GetDatabaseMetadata(ADBIndex: Integer; AObjectType: string): string;
var
  ObjType: string;
begin
  ObjType := LowerCase(Trim(AObjectType));
  if ObjType = 'tables' then Result := GetDatabaseTableList(ADBIndex)
  else if ObjType = 'views' then Result := GetDatabaseViewList(ADBIndex)
  else if ObjType = 'procedures' then Result := GetDatabaseProcedureList(ADBIndex)
  else if ObjType = 'functions' then Result := GetDatabaseFunctionList(ADBIndex)
  else if ObjType = 'triggers' then Result := GetDatabaseTriggerList(ADBIndex)
  else if ObjType = 'generators' then Result := GetDatabaseGeneratorList(ADBIndex)
  else if ObjType = 'domains' then Result := GetDatabaseDomainList(ADBIndex)
  else if ObjType = 'roles' then Result := GetDatabaseRoleList(ADBIndex)
  else if ObjType = 'users' then Result := GetDatabaseUserList(ADBIndex)
  else Result := 'error: unknown object type. Use: tables, views, procedures, functions, triggers, generators, domains, roles, users';
end;

function TAIContextProvider.GetDatabaseTableList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 AND RDB$RELATION_TYPE = 0 ' +
      'ORDER BY RDB$RELATION_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$RELATION_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseViewList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 AND RDB$RELATION_TYPE = 1 ' +
      'ORDER BY RDB$RELATION_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$RELATION_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseProcedureList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$PROCEDURE_NAME FROM RDB$PROCEDURES ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY RDB$PROCEDURE_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$PROCEDURE_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseFunctionList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$FUNCTION_NAME FROM RDB$FUNCTIONS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY RDB$FUNCTION_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$FUNCTION_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseTriggerList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$TRIGGER_NAME FROM RDB$TRIGGERS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY RDB$TRIGGER_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$TRIGGER_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseGeneratorList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY RDB$GENERATOR_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$GENERATOR_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseDomainList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$FIELD_NAME FROM RDB$FIELDS ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY RDB$FIELD_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$FIELD_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseRoleList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    q.Database := db;
    q.Transaction := tr;
    q.Close;
    q.SQL.Text :=
      'SELECT RDB$ROLE_NAME FROM RDB$ROLES ' +
      'WHERE RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY RDB$ROLE_NAME';
    q.Open;
    while not q.EOF do
    begin
      JArr.Add(Trim(q.FieldByName('RDB$ROLE_NAME').AsString));
      q.Next;
    end;
    Result := JArr.AsJSON;
  finally
    q.Free;
  end;
end;

function TAIContextProvider.GetDatabaseUserList(ADBIndex: Integer): string;
var
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  JArr: TJSONArray;
begin
  Result := '[]';
  if (ADBIndex < 0) or (ADBIndex >= Length(RegisteredDatabases)) then Exit;

  db := RegisteredDatabases[ADBIndex].IBDatabase;
  tr := RegisteredDatabases[ADBIndex].IBTransaction;
  if not Assigned(db) or not Assigned(tr) then Exit;

  if not db.Connected then
  begin
    Result := 'error: database not connected';
    Exit;
  end;

  JArr := TJSONArray.Create;
  q := TIBQuery.Create(nil);
  try
    try
      q.Database := db;
      q.Transaction := tr;
      q.Close;
      // Firebird 3+: RDB$USERS, older: need service manager
      q.SQL.Text :=
        'SELECT RDB$USER_NAME FROM RDB$USERS ' +
        'ORDER BY RDB$USER_NAME';
      q.Open;
      while not q.EOF do
      begin
        JArr.Add(Trim(q.FieldByName('RDB$USER_NAME').AsString));
        q.Next;
      end;
      Result := JArr.AsJSON;
    except
      on E: Exception do
        Result := 'error: ' + E.Message + ' (requires Firebird 3+ or service manager)';
    end;
  finally
    q.Free;
  end;
end;

// ===== SERVER-STATUS & ADMIN =====

function TAIContextProvider.TestServerConnection(AServerIndex: Integer): string;
var
  ServerRec: TServerRecord;
  JObj: TJSONObject;
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  Connected: Boolean;
  DBIndex: Integer;
  DBRec: TRegisteredDatabase;
  FoundDB: Boolean;
begin
  Result := '{"success":false,"error":"Invalid server index"}';
  if (AServerIndex < 0) or (AServerIndex >= Length(RegisteredServers)) then Exit;

  ServerRec := RegisteredServers[AServerIndex];
  JObj := TJSONObject.Create;

  // Find a database for this server to test connection
  DBIndex := -1;
  FoundDB := False;
  for DBIndex := 0 to High(RegisteredDatabases) do
  begin
    DBRec := RegisteredDatabases[DBIndex].RegRec;
    if DBRec.Deleted then Continue;
    if SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerAlias)) or
       ((Trim(ServerRec.ServerAlias) = '') and SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerName))) then
    begin
      FoundDB := True;
      Break;
    end;
  end;

  if not FoundDB then
  begin
    JObj.Add('success', False);
    JObj.Add('error', 'No database registered for this server');
    Result := JObj.AsJSON;
    JObj.Free;
    Exit;
  end;

  DBRec := RegisteredDatabases[DBIndex].RegRec;
  db := TIBDatabase.Create(nil);
  tr := TIBTransaction.Create(nil);
  q := TIBQuery.Create(nil);
  Connected := False;
  try
    try
      db.DatabaseName := DBRec.DatabaseName;
      db.Params.Clear;
      db.Params.Add('user_name=' + DBRec.UserName);
      if DBRec.Password <> '' then
        db.Params.Add('password=' + DBRec.Password);
      if DBRec.Role <> '' then
        db.Params.Add('sql_role_name=' + DBRec.Role);
      if DBRec.Charset <> '' then
        db.Params.Add('lc_ctype=' + DBRec.Charset);

      db.Connected := True;
      Connected := True;

      // Get server version via query
      q.Database := db;
      q.Transaction := tr;
      q.SQL.Text := 'SELECT RDB$GET_CONTEXT(''SYSTEM'', ''ENGINE_VERSION'') AS VERSION FROM RDB$DATABASE';
      q.Open;
      if not q.EOF then
        JObj.Add('version', q.FieldByName('VERSION').AsString)
      else
        JObj.Add('version', 'unknown');

      q.Close;
      JObj.Add('success', True);
      JObj.Add('server_name', ServerRec.ServerName);
      JObj.Add('server_alias', ServerRec.ServerAlias);
      JObj.Add('tested_via_database', DBRec.Title);
    except
      on E: Exception do
      begin
        JObj.Add('success', False);
        JObj.Add('error', E.Message);
      end;
    end;
    Result := JObj.AsJSON;
  finally
    if Connected then
    begin
      if tr.InTransaction then tr.Rollback;
      db.Connected := False;
    end;
    q.Free;
    tr.Free;
    db.Free;
    JObj.Free;
  end;
end;

function TAIContextProvider.GetServerInventoryAsText: string;
begin
  // Use the existing function from turbocommon
  Result := turbocommon.GetServerInventoryAsText;
end;

function TAIContextProvider.GetServerInventoryAsJSON: string;
var
  i, j: Integer;
  ServerRec: TServerRecord;
  DBRec: TRegisteredDatabase;
  JArrServers, JArrDBs: TJSONArray;
  JObjServer, JObjDB: TJSONObject;
  VersionStr, ProtocolStr: string;
  HasDBs: Boolean;
begin
  JArrServers := TJSONArray.Create;
  try
    for i := 0 to High(RegisteredServers) do
    begin
      ServerRec := RegisteredServers[i];

      if (ServerRec.VersionMajor > 0) or (ServerRec.VersionMinor > 0) then
        VersionStr := 'Firebird ' + IntToStr(ServerRec.VersionMajor) + '.' + IntToStr(ServerRec.VersionMinor)
      else
        VersionStr := 'Firebird (unknown version)';

      ProtocolStr := IBXProtocolToString(ServerRec.Protocol);

      JObjServer := TJSONObject.Create;
      JObjServer.Add('index', i);
      JObjServer.Add('server_name', ServerRec.ServerName);
      JObjServer.Add('server_alias', ServerRec.ServerAlias);
      JObjServer.Add('version_string', VersionStr);
      JObjServer.Add('version_major', ServerRec.VersionMajor);
      JObjServer.Add('version_minor', ServerRec.VersionMinor);
      JObjServer.Add('protocol', ProtocolStr);
      JObjServer.Add('port', ServerRec.Port);
      JObjServer.Add('charset', ServerRec.Charset);
      JObjServer.Add('is_embedded', ServerRec.IsEmbedded);
      JObjServer.Add('username', ServerRec.UserName);
      JObjServer.Add('role', ServerRec.Role);

      JArrDBs := TJSONArray.Create;
      HasDBs := False;
      for j := 0 to High(RegisteredDatabases) do
      begin
        DBRec := RegisteredDatabases[j].RegRec;
        if DBRec.Deleted then Continue;

        if SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerAlias)) or
           ((Trim(ServerRec.ServerAlias) = '') and SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerName))) then
        begin
          JObjDB := TJSONObject.Create;
          JObjDB.Add('index', j);
          JObjDB.Add('title', DBRec.Title);
          JObjDB.Add('database_name', DBRec.DatabaseName);
          JObjDB.Add('username', DBRec.UserName);
          JObjDB.Add('charset', DBRec.Charset);
          JObjDB.Add('is_embedded', DBRec.IsEmbedded);
          JArrDBs.Add(JObjDB);
          HasDBs := True;
        end;
      end;
      JObjServer.Add('databases', JArrDBs);
      JObjServer.Add('has_databases', HasDBs);

      JArrServers.Add(JObjServer);
    end;

    Result := JArrServers.AsJSON;
  finally
    JArrServers.Free;
  end;
end;

// High-Level Context String für Prompt-Injection
function TAIContextProvider.BuildContextString: string;
var
  Parts: TStringList;
  TableName, QueryText: string;
  Tables: TStringList;
  NodeInfo: TPNodeInfos;
  dbIndex: Integer;
  db: TIBDatabase;
  QW: TfmQueryWindow;
  i: Integer;
  MainForm: TfmMain;
begin
  Parts := TStringList.Create;
  try
    Parts.Add('=== TURBOBIRD CONTEXT ===');

    // Database connection info - get from selected tree node
    MainForm := TfmMain(GetMainForm);
    dbIndex := -1;
    if Assigned(MainForm) and Assigned(MainForm.tvMain.Selected) then
    begin
      NodeInfo := TPNodeInfos(MainForm.tvMain.Selected.Data);
      if Assigned(NodeInfo) then
        dbIndex := NodeInfo^.dbIndex;
    end;

    if (dbIndex >= 0) and (dbIndex < Length(RegisteredDatabases)) then
    begin
      db := RegisteredDatabases[dbIndex].IBDatabase;
      if Assigned(db) and db.Connected then
      begin
        Parts.Add('Database: ' + RegisteredDatabases[dbIndex].RegRec.Title + ' @ ' + 
                  RegisteredDatabases[dbIndex].RegRec.Title);
        Parts.Add('Server: ' + RegisteredDatabases[dbIndex].RegRec.ServerName);
        Parts.Add('Connection: ACTIVE');
      end
      else
        Parts.Add('Connection: NOT CONNECTED');
    end
    else
      Parts.Add('Connection: NOT CONNECTED');

    // Selected table
    TableName := '';
    if Assigned(MainForm) and Assigned(MainForm.tvMain.Selected) then
    begin
      NodeInfo := TPNodeInfos(MainForm.tvMain.Selected.Data);
      if Assigned(NodeInfo) and (NodeInfo^.ObjectType = tvotTable) then
        TableName := MainForm.tvMain.Selected.Text;
    end;
    if TableName <> '' then
      Parts.Add('Selected Table: ' + TableName);

    // Active query
    QW := nil;
    if Assigned(MainForm) then
    begin
      for i := 0 to MainForm.PageControl1.PageCount - 1 do
      begin
        if MainForm.PageControl1.Pages[i].TabVisible and
           (MainForm.PageControl1.ActivePage = MainForm.PageControl1.Pages[i]) then
        begin
          if Assigned(MainForm.PageControl1.Pages[i].Controls[0]) and
             (MainForm.PageControl1.Pages[i].Controls[0] is TfmQueryWindow) then
          begin
            QW := TfmQueryWindow(MainForm.PageControl1.Pages[i].Controls[0]);
            Break;
          end;
        end;
      end;
      if not Assigned(QW) then
        for i := 0 to Application.ComponentCount - 1 do
          if Application.Components[i] is TfmQueryWindow then
          begin
            QW := TfmQueryWindow(Application.Components[i]);
            Break;
          end;
    end;

    if Assigned(QW) then
    begin
      QueryText := QW.meQuery.Text;
      if QueryText <> '' then
      begin
        Parts.Add('Active Query Window: ' + QW.Caption);
        Parts.Add('Current SQL:');
        Parts.Add(QueryText);
      end;
    end;

    // Available tables (truncated)
    Tables := GetVisibleTableNames;
    try
      if Tables.Count > 0 then
      begin
        Parts.Add('Available Tables (' + IntToStr(Tables.Count) + '):');
        if Tables.Count <= 30 then
          Parts.Add(Tables.CommaText)
        else
          Parts.Add(Tables.CommaText + ' ... (truncated)');
      end;
    finally
      Tables.Free;
    end;

    Parts.Add('=== END CONTEXT ===');
    Result := Parts.Text;
  finally
    Parts.Free;
  end;
end;

function TAIContextProvider.BuildFullSystemContextString: string;
var
  Parts: TStringList;
  i, j: Integer;
  ServerRec: TServerRecord;
  DBRec: TRegisteredDatabase;
  VersionStr, ProtocolStr: string;
  HasDBs: Boolean;
begin
  Parts := TStringList.Create;
  try
    Parts.Add('=== TURBOBIRD FULL SYSTEM CONTEXT ===');
    Parts.Add('');

    // All registered servers
    Parts.Add('REGISTERED SERVERS (' + IntToStr(Length(RegisteredServers)) + '):');
    for i := 0 to High(RegisteredServers) do
    begin
      ServerRec := RegisteredServers[i];

      if (ServerRec.VersionMajor > 0) or (ServerRec.VersionMinor > 0) then
        VersionStr := 'Firebird ' + IntToStr(ServerRec.VersionMajor) + '.' + IntToStr(ServerRec.VersionMinor)
      else
        VersionStr := 'Firebird (unknown version)';

      ProtocolStr := IBXProtocolToString(ServerRec.Protocol);

      Parts.Add('  Server #' + IntToStr(i) + ': ' + ServerRec.ServerName +
                ' [Alias: ' + ServerRec.ServerAlias + ']');
      Parts.Add('    Version: ' + VersionStr + ' | Protocol: ' + ProtocolStr +
                ' | Port: ' + ServerRec.Port);
      Parts.Add('    RootPath: ' + ServerRec.RootPath);
      Parts.Add('    ClientLib: ' + ServerRec.ClientLibraryPath);
      Parts.Add('    Embedded: ' + BoolToStr(ServerRec.IsEmbedded, True));

      // Databases under this server
      Parts.Add('    Databases:');
      HasDBs := False;
      for j := 0 to High(RegisteredDatabases) do
      begin
        DBRec := RegisteredDatabases[j].RegRec;
        if DBRec.Deleted then Continue;

        if SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerAlias)) or
           ((Trim(ServerRec.ServerAlias) = '') and SameText(Trim(DBRec.ServerName), Trim(ServerRec.ServerName))) then
        begin
          Parts.Add('      - ' + DBRec.Title + ': ' + DBRec.DatabaseName +
                    ' (User: ' + DBRec.UserName + ', Charset: ' + DBRec.Charset +
                    ', Dialect: ' + DBRec.SQLDialect + ')');
          HasDBs := True;
        end;
      end;
      if not HasDBs then
        Parts.Add('      (no databases registered)');
      Parts.Add('');
    end;

    Parts.Add('=== END FULL SYSTEM CONTEXT ===');
    Result := Parts.Text;
  finally
    Parts.Free;
  end;
end;

end.
