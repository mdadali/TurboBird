unit uOllamaAgent;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser,
  opensslsockets,
  uAiSettings,
  uSystemTools,
  uWebTools,
  uAIContext,
  IBDatabase, IBQuery, IB, DB,
  turbocommon;
type
  TOllamaAgentThread = class(TThread)
  private
    FSettings: TAiSettings;
    FPrompt: string;
    FResultText: string;
    FErrorMsg: string;
    FToolCallCount: Integer;
    function GetToolCallCount: Integer;
    procedure SendOllama(AClient: TFPHTTPClient);
    procedure SendOpenAI(AClient: TFPHTTPClient);
    procedure SendGemini(AClient: TFPHTTPClient);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; ASettings: TAiSettings);
    property Settings: TAiSettings read FSettings;
    property Prompt: string read FPrompt write FPrompt;
    property ResultText: string read FResultText;
    property ErrorMsg: string read FErrorMsg;
    property ToolCallCount: Integer read GetToolCallCount;
  end;
implementation
constructor TOllamaAgentThread.Create(CreateSuspended: Boolean;
  ASettings: TAiSettings);
begin
  inherited Create(CreateSuspended);
  FSettings := ASettings;
  FToolCallCount := 0;
end;
function TOllamaAgentThread.GetToolCallCount: Integer;
begin
  Result := FToolCallCount;
end;
function StripTrailingSlash(const S: string): string;
begin
  Result := S;
  while (Result <> '') and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;
{ ---------- Tool implementations ---------- }
function ToolGetCurrentTime: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;
function EvalExpr(const S: string): Double; forward;
function ParseNumber(const S: string; var Pos: Integer): Double; forward;
function ParseFactor(const S: string; var Pos: Integer): Double; forward;
function ParseTerm(const S: string; var Pos: Integer): Double; forward;
function ParseExpr(const S: string; var Pos: Integer): Double; forward;
function SkipWS(const S: string; var Pos: Integer): Integer;
begin
  while (Pos <= Length(S)) and (S[Pos] in [' ', #9]) do Inc(Pos);
  Result := Pos;
end;
function ParseNumber(const S: string; var Pos: Integer): Double;
var
  Start: Integer;
  Tmp: string;
begin
  SkipWS(S, Pos);
  Start := Pos;
  while (Pos <= Length(S)) and (S[Pos] in ['0'..'9', '.', 'e', 'E', '+', '-']) do
  begin
    { sign only allowed at start of number }
    if (Pos > Start) and (S[Pos] in ['+', '-']) then Break;
    Inc(Pos);
  end;
  Tmp := Trim(Copy(S, Start, Pos - Start));
  if Tmp = '' then raise Exception.Create('number expected');
  Result := StrToFloat(Tmp);
end;
function ParseFactor(const S: string; var Pos: Integer): Double;
var
  Neg: Boolean;
begin
  SkipWS(S, Pos);
  Neg := False;
  if Pos <= Length(S) then
  begin
    if S[Pos] = '-' then begin Neg := True; Inc(Pos); end
    else if S[Pos] = '+' then Inc(Pos);
  end;
  SkipWS(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = '(') then
  begin
    Inc(Pos);
    Result := ParseExpr(S, Pos);
    SkipWS(S, Pos);
    if (Pos <= Length(S)) and (S[Pos] = ')') then Inc(Pos) else raise Exception.Create('missing )');
  end
  else
    Result := ParseNumber(S, Pos);
  if Neg then Result := -Result;
end;
function ParseTerm(const S: string; var Pos: Integer): Double;
var
  Op: Char;
  R: Double;
begin
  Result := ParseFactor(S, Pos);
  while True do
  begin
    SkipWS(S, Pos);
    if (Pos > Length(S)) or not (S[Pos] in ['*', '/']) then Break;
    Op := S[Pos]; Inc(Pos);
    R := ParseFactor(S, Pos);
    if Op = '*' then Result := Result * R else
      if R = 0 then raise Exception.Create('division by zero') else Result := Result / R;
  end;
end;
function ParseExpr(const S: string; var Pos: Integer): Double;
var
  Op: Char;
  R: Double;
begin
  Result := ParseTerm(S, Pos);
  while True do
  begin
    SkipWS(S, Pos);
    if (Pos > Length(S)) or not (S[Pos] in ['+', '-']) then Break;
    Op := S[Pos]; Inc(Pos);
    R := ParseTerm(S, Pos);
    if Op = '+' then Result := Result + R else Result := Result - R;
  end;
end;
function EvalExpr(const S: string): Double;
var
  Pos: Integer;
begin
  Pos := 1;
  Result := ParseExpr(S, Pos);
  SkipWS(S, Pos);
  if Pos <= Length(S) then raise Exception.Create('trailing garbage');
end;
function ToolCalculate(const ArgsJSON: string): string;
var
  Data: TJSONData;
  Obj: TJSONObject;
  Expr: string;
  Val: Double;
begin
  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('expression') <> nil then Expr := Obj.Strings['expression'] else Expr := '';
    Data.Free;
    if Trim(Expr) = '' then raise Exception.Create('missing expression');
    Val := EvalExpr(Expr);
    Result := FloatToStr(Val);
  except
    on E: Exception do Result := 'error: ' + E.Message;
  end;
end;
function ToolGetSystemInfo: string;
begin
  Result := 'OS: ' + {$I %FPCTARGETOS%} + '; CPU: ' + {$I %FPCTARGETCPU%} +
            '; FPC: ' + {$I %FPCVERSION%};
end;

{ ---------- TurboBird-specific Tools ---------- }
function ToolRunFirebirdQuery(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  SQL: string;
  db: TIBDatabase;
  tr: TIBTransaction;
  q: TIBQuery;
  Row, Col: Integer;
  FieldName, FieldVal: string;
  Output: TStringList;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  if not Provider.IsConnected then
  begin
    Result := 'error: Not connected to a Firebird database';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('sql') <> nil then
      SQL := Obj.Strings['sql']
    else
      SQL := '';
    Data.Free;

    if Trim(SQL) = '' then
    begin
      Result := 'error: missing sql parameter';
      Exit;
    end;

    // Safety: Only allow SELECT statements
    if Pos('SELECT', UpperCase(Trim(SQL))) <> 1 then
    begin
      Result := 'error: Only SELECT statements are allowed for safety';
      Exit;
    end;

    db := Provider.GetIBDatabase;
    tr := Provider.GetIBTransaction;
    if not Assigned(db) or not Assigned(tr) then
    begin
      Result := 'error: Database connection not available';
      Exit;
    end;

    q := TIBQuery.Create(nil);
    Output := TStringList.Create;
    try
      q.Database := db;
      q.Transaction := tr;
      q.Close;
      q.SQL.Text := SQL;
      // Ensure transaction is active
      if not tr.InTransaction then
        tr.StartTransaction;
      q.Open;

      // Header
      for Col := 0 to q.FieldCount - 1 do
      begin
        if Col > 0 then Output.Add(' | ');
        Output.Add(q.Fields[Col].FieldName);
      end;
      Output.Add(LineEnding + StringOfChar('-', 80));

      // Data rows (limit to 100 rows)
      Row := 0;
      while not q.EOF and (Row < 100) do
      begin
        for Col := 0 to q.FieldCount - 1 do
        begin
          if Col > 0 then Output.Add(' | ');
          FieldVal := q.Fields[Col].AsString;
          if Length(FieldVal) > 100 then
            FieldVal := Copy(FieldVal, 1, 97) + '...';
          Output.Add(FieldVal);
        end;
        Output.Add(LineEnding);
        q.Next;
        Inc(Row);
      end;

      if Row >= 100 then
        Output.Add('... (truncated at 100 rows)');

      Result := Output.Text;
    finally
      q.Free;
      Output.Free;
    end;
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolGetTableSchema(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  TableName: string;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('table_name') <> nil then
      TableName := Obj.Strings['table_name']
    else
      TableName := '';
    Data.Free;

    if Trim(TableName) = '' then
    begin
      Result := 'error: missing table_name parameter';
      Exit;
    end;

    Result := Provider.GetTableSchema(TableName, True);
    if Result = '' then
      Result := 'error: Could not retrieve schema for table ' + TableName;
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolListDatabaseTables(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Tables: TStringList;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  Tables := Provider.GetVisibleTableNames;
  try
    if Tables.Count = 0 then
      Result := 'No user tables found in current database'
    else
    begin
      Result := 'Tables in current database (' + IntToStr(Tables.Count) + '):' + LineEnding;
      Result := Result + Tables.CommaText;
    end;
  finally
    Tables.Free;
  end;
end;

{ ---------- TurboBird System Tools (Full System Access) ---------- }

function ToolListRegisteredServers(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Count, i: Integer;
  JArr: TJSONArray;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  Count := Provider.GetRegisteredServerCount;
  JArr := TJSONArray.Create;
  try
    for i := 0 to Count - 1 do
      JArr.Add(GetJSON(Provider.GetServerInfoAsJSON(i)));
    Result := JArr.AsJSON;
  finally
    JArr.Free;
  end;
end;

function ToolGetServerInfo(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  Index: Integer;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('index') <> nil then
      Index := Obj.Get('index', -1)
    else if Obj.Find('name') <> nil then
      Index := Provider.FindServerIndexByName(Obj.Strings['name'])
    else if Obj.Find('alias') <> nil then
      Index := Provider.FindServerIndexByAlias(Obj.Strings['alias'])
    else
      Index := -1;
    Data.Free;

    if Index < 0 then
      Result := 'error: server not found (provide index, name, or alias)'
    else
      Result := Provider.GetServerInfoAsJSON(Index);
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolListRegisteredDatabases(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Count, i: Integer;
  JArr: TJSONArray;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  Count := Provider.GetRegisteredDatabaseCount;
  JArr := TJSONArray.Create;
  try
    for i := 0 to Count - 1 do
      JArr.Add(GetJSON(Provider.GetDatabaseInfoAsJSON(i)));
    Result := JArr.AsJSON;
  finally
    JArr.Free;
  end;
end;

function ToolGetDatabaseInfo(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  Index: Integer;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('index') <> nil then
      Index := Obj.Get('index', -1)
    else if Obj.Find('title') <> nil then
      Index := Provider.FindDatabaseIndexByTitle(Obj.Strings['title'])
    else
      Index := -1;
    Data.Free;

    if Index < 0 then
      Result := 'error: database not found (provide index or title)'
    else
      Result := Provider.GetDatabaseInfoAsJSON(Index);
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolGetDatabasesForServer(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  ServerAlias: string;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('server_alias') <> nil then
      ServerAlias := Obj.Strings['server_alias']
    else
      ServerAlias := '';
    Data.Free;

    if ServerAlias = '' then
      Result := 'error: missing server_alias parameter'
    else
      Result := Provider.GetDatabasesForServer(ServerAlias);
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolConnectDatabase(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  Index: Integer;
  Success: Boolean;
  JObj: TJSONObject;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('index') <> nil then
      Index := Obj.Get('index', -1)
    else if Obj.Find('title') <> nil then
      Index := Provider.FindDatabaseIndexByTitle(Obj.Strings['title'])
    else
      Index := -1;
    Data.Free;

    if Index < 0 then
      Result := 'error: database not found (provide index or title)'
    else
    begin
      Success := Provider.ConnectToDatabase(Index);
      JObj := TJSONObject.Create;
      try
        JObj.Add('success', Success);
        JObj.Add('database_index', Index);
        if Success then
          JObj.Add('message', 'Connected successfully')
        else
          JObj.Add('error', 'Failed to connect');
        Result := JObj.AsJSON;
      finally
        JObj.Free;
      end;
    end;
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolDisconnectDatabase(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  Index: Integer;
  Success: Boolean;
  JObj: TJSONObject;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('index') <> nil then
      Index := Obj.Get('index', -1)
    else if Obj.Find('title') <> nil then
      Index := Provider.FindDatabaseIndexByTitle(Obj.Strings['title'])
    else
      Index := -1;
    Data.Free;

    if Index < 0 then
      Result := 'error: database not found (provide index or title)'
    else
    begin
      Success := Provider.DisconnectDatabase(Index);
      JObj := TJSONObject.Create;
      try
        JObj.Add('success', Success);
        JObj.Add('database_index', Index);
        if Success then
          JObj.Add('message', 'Disconnected successfully')
        else
          JObj.Add('error', 'Failed to disconnect');
        Result := JObj.AsJSON;
      finally
        JObj.Free;
      end;
    end;
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolGetDatabaseMetadata(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  Index: Integer;
  ObjectType: string;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('index') <> nil then
      Index := Obj.Get('index', -1)
    else if Obj.Find('title') <> nil then
      Index := Provider.FindDatabaseIndexByTitle(Obj.Strings['title'])
    else
      Index := -1;

    if Obj.Find('object_type') <> nil then
      ObjectType := Obj.Strings['object_type']
    else
      ObjectType := 'tables';

    Data.Free;

    if Index < 0 then
      Result := 'error: database not found (provide index or title)'
    else
      Result := Provider.GetDatabaseMetadata(Index, ObjectType);
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolTestServerConnection(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  Index: Integer;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    if not (Data is TJSONObject) then raise Exception.Create('bad args');
    Obj := TJSONObject(Data);
    if Obj.Find('index') <> nil then
      Index := Obj.Get('index', -1)
    else if Obj.Find('name') <> nil then
      Index := Provider.FindServerIndexByName(Obj.Strings['name'])
    else if Obj.Find('alias') <> nil then
      Index := Provider.FindServerIndexByAlias(Obj.Strings['alias'])
    else
      Index := -1;
    Data.Free;

    if Index < 0 then
      Result := 'error: server not found (provide index, name, or alias)'
    else
      Result := Provider.TestServerConnection(Index);
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolGetServerInventory(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
  Data: TJSONData;
  Obj: TJSONObject;
  AsJSON: Boolean;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  try
    Data := GetJSON(ArgsJSON);
    AsJSON := False;
    if (Data is TJSONObject) then
    begin
      Obj := TJSONObject(Data);
      if Obj.Find('format') <> nil then
        AsJSON := SameText(Obj.Strings['format'], 'json');
      Data.Free;
    end;

    if AsJSON then
      Result := Provider.GetServerInventoryAsJSON
    else
      Result := Provider.GetServerInventoryAsText;
  except
    on E: Exception do
      Result := 'error: ' + E.Message;
  end;
end;

function ToolGetFullSystemContext(const ArgsJSON: string): string;
var
  Provider: IAIContextProvider;
begin
  Result := '';
  Provider := GetAIContextProvider;
  if not Assigned(Provider) then
  begin
    Result := 'error: No TurboBird context available';
    Exit;
  end;

  Result := Provider.BuildFullSystemContextString;
end;

function ExecuteTool(const Name, ArgsJSON: string): string;
var
  Data: TJSONData;
  Cmd, WD, FPath, FContent, FPattern, ToolOut, ToolErr: string;
  ExitCode: Integer;
begin
  if Name = 'get_current_time' then Result := ToolGetCurrentTime
  else if Name = 'calculate' then Result := ToolCalculate(ArgsJSON)
  else if Name = 'get_system_info' then Result := ToolGetSystemInfo
  else if Name = 'run_firebird_query' then Result := ToolRunFirebirdQuery(ArgsJSON)
  else if Name = 'get_table_schema' then Result := ToolGetTableSchema(ArgsJSON)
  else if Name = 'list_database_tables' then Result := ToolListDatabaseTables(ArgsJSON)
  else if Name = 'list_registered_servers' then Result := ToolListRegisteredServers(ArgsJSON)
  else if Name = 'get_server_info' then Result := ToolGetServerInfo(ArgsJSON)
  else if Name = 'list_registered_databases' then Result := ToolListRegisteredDatabases(ArgsJSON)
  else if Name = 'get_database_info' then Result := ToolGetDatabaseInfo(ArgsJSON)
  else if Name = 'get_databases_for_server' then Result := ToolGetDatabasesForServer(ArgsJSON)
  else if Name = 'connect_database' then Result := ToolConnectDatabase(ArgsJSON)
  else if Name = 'disconnect_database' then Result := ToolDisconnectDatabase(ArgsJSON)
  else if Name = 'get_database_metadata' then Result := ToolGetDatabaseMetadata(ArgsJSON)
  else if Name = 'test_server_connection' then Result := ToolTestServerConnection(ArgsJSON)
  else if Name = 'get_server_inventory' then Result := ToolGetServerInventory(ArgsJSON)
  else if Name = 'get_full_system_context' then Result := ToolGetFullSystemContext(ArgsJSON)
  else if Name = 'run_shell_command' then
  begin
    // Args: command, working_dir (optional)
    Data := GetJSON(ArgsJSON);
    Cmd := '';
    WD := '';
    if Data is TJSONObject then
    begin
      if TJSONObject(Data).Find('command') <> nil then Cmd := TJSONObject(Data).Strings['command'];
      if TJSONObject(Data).Find('working_dir') <> nil then WD := TJSONObject(Data).Strings['working_dir'];
    end;
    Data.Free;
    ExitCode := ExecuteShellCommand(Cmd, WD, ToolOut, ToolErr);
    if ToolErr <> '' then
      Result := Format('Exit code: %d%sStdout:%s%sStderr/Error:%s%s', [ExitCode, LineEnding, LineEnding, ToolOut, LineEnding, ToolErr])
    else
      Result := Format('Exit code: %d%sOutput:%s%s', [ExitCode, LineEnding, LineEnding, ToolOut]);
  end
  else if Name = 'read_file' then
  begin
    Data := GetJSON(ArgsJSON);
    FPath := '';
    if Data is TJSONObject then
      if TJSONObject(Data).Find('path') <> nil then FPath := TJSONObject(Data).Strings['path'];
    Data.Free;
    if ReadFileContent(FPath, ToolOut, ToolErr) then
      Result := ToolOut
    else
      Result := 'error: ' + ToolErr;
  end
  else if Name = 'write_file' then
  begin
    Data := GetJSON(ArgsJSON);
    FPath := '';
    FContent := '';
    if Data is TJSONObject then
    begin
      if TJSONObject(Data).Find('path') <> nil then FPath := TJSONObject(Data).Strings['path'];
      if TJSONObject(Data).Find('content') <> nil then FContent := TJSONObject(Data).Strings['content'];
    end;
    Data.Free;
    if WriteFileContent(FPath, FContent, ToolErr) then
      Result := 'Successfully wrote ' + IntToStr(Length(FContent)) + ' bytes to ' + FPath
    else
      Result := 'error: ' + ToolErr;
  end
  else if Name = 'list_directory' then
  begin
    Data := GetJSON(ArgsJSON);
    FPath := '.';
    if Data is TJSONObject then
      if TJSONObject(Data).Find('path') <> nil then FPath := TJSONObject(Data).Strings['path'];
    Data.Free;
    if ListDirectory(FPath, ToolOut, ToolErr) then
      Result := ToolOut
    else
      Result := 'error: ' + ToolErr;
  end
  else if Name = 'search_files' then
  begin
    Data := GetJSON(ArgsJSON);
    FPattern := '';
    FPath := '.';
    if Data is TJSONObject then
    begin
      if TJSONObject(Data).Find('pattern') <> nil then FPattern := TJSONObject(Data).Strings['pattern'];
      if TJSONObject(Data).Find('path') <> nil then FPath := TJSONObject(Data).Strings['path'];
    end;
    Data.Free;
    if SearchFiles(FPattern, FPath, ToolOut, ToolErr) then
      Result := ToolOut
    else
      Result := 'error: ' + ToolErr;
  end
  else if Name = 'web_search' then
  begin
    Data := GetJSON(ArgsJSON);
    FPattern := '';
    if Data is TJSONObject then
      if TJSONObject(Data).Find('query') <> nil then FPattern := TJSONObject(Data).Strings['query'];
    Data.Free;
    if WebSearch(FPattern, ToolOut, ToolErr) then
      Result := ToolOut
    else
      Result := 'error: ' + ToolErr;
  end
  else if Name = 'web_fetch' then
  begin
    Data := GetJSON(ArgsJSON);
    FPath := '';
    if Data is TJSONObject then
      if TJSONObject(Data).Find('url') <> nil then FPath := TJSONObject(Data).Strings['url'];
    Data.Free;
    if WebFetch(FPath, ToolOut, ToolErr) then
      Result := ToolOut
    else
      Result := 'error: ' + ToolErr;
  end
  else
  begin
    { Fallback: unbekanntes Tool → als Shell-Befehl interpretieren }
    Data := GetJSON(ArgsJSON);
    Cmd := Name;
    if Data is TJSONObject then
    begin
      if TJSONObject(Data).Find('command') <> nil then
        Cmd := TJSONObject(Data).Strings['command']
      else
      begin
        { Args-Werte anhaengen falls vorhanden }
        // einfache Heuristik: alle String-Werte sammeln
        // (fuer Tools wie 'git' mit {"subcommand":"status"})
      end;
    end;
    Data.Free;
    { Falls Cmd nur ein Tool-Name ist (kein Leerzeichen), versuche es direkt }
    ExitCode := ExecuteShellCommand(Cmd, '', ToolOut, ToolErr);
    if ToolErr <> '' then
      Result := Format('Exit code: %d%sStdout:%s%sStderr/Error:%s%s', [ExitCode, LineEnding, LineEnding, ToolOut, LineEnding, ToolErr])
    else
      Result := Format('Exit code: %d%sOutput:%s%s', [ExitCode, LineEnding, LineEnding, ToolOut]);
  end;
end;
{ ---------- Tool definitions (OpenAI format) ---------- }
function ToolDefinitionsJSON: string;
begin
  Result := '[' +
    '{"type":"function","function":{"name":"get_current_time","description":"Returns the current date and time on the server.","parameters":{"type":"object","properties":{},"required":[]}}},' +
    '{"type":"function","function":{"name":"calculate","description":"Evaluates a mathematical expression with +,-,*,/, parentheses and decimals.","parameters":{"type":"object","properties":{"expression":{"type":"string","description":"Math expression to evaluate"}},"required":["expression"]}}},' +
    '{"type":"function","function":{"name":"get_system_info","description":"Returns basic system information (OS, CPU, compiler).","parameters":{"type":"object","properties":{},"required":[]}}},' +
    '{"type":"function","function":{"name":"run_firebird_query","description":"Executes a read-only SELECT query on the current Firebird database connection. Only SELECT statements allowed. Returns up to 100 rows.","parameters":{"type":"object","properties":{"sql":{"type":"string","description":"SELECT query to execute"}},"required":["sql"]}}},' +
    '{"type":"function","function":{"name":"get_table_schema","description":"Returns the complete CREATE TABLE DDL including indexes for a table in the current database.","parameters":{"type":"object","properties":{"table_name":{"type":"string","description":"Name of the table to get schema for"}},"required":["table_name"]}}},' +
    '{"type":"function","function":{"name":"list_database_tables","description":"Lists all user tables in the current Firebird database.","parameters":{"type":"object","properties":{},"required":[]}}},' +
    '{"type":"function","function":{"name":"list_registered_servers","description":"Lists all registered Firebird servers in TurboBird with their details (name, alias, version, protocol, port, etc.).","parameters":{"type":"object","properties":{},"required":[]}}},' +
    '{"type":"function","function":{"name":"get_server_info","description":"Gets detailed information about a specific registered server.","parameters":{"type":"object","properties":{"index":{"type":"integer","description":"Server index (0-based)"},"name":{"type":"string","description":"Server name"},"alias":{"type":"string","description":"Server alias"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"list_registered_databases","description":"Lists all registered databases in TurboBird with their details (title, server, path, user, charset, etc.).","parameters":{"type":"object","properties":{},"required":[]}}},' +
    '{"type":"function","function":{"name":"get_database_info","description":"Gets detailed information about a specific registered database.","parameters":{"type":"object","properties":{"index":{"type":"integer","description":"Database index (0-based)"},"title":{"type":"string","description":"Database title"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"get_databases_for_server","description":"Lists all databases registered under a specific server alias.","parameters":{"type":"object","properties":{"server_alias":{"type":"string","description":"Server alias to filter databases"}},"required":["server_alias"]}}},' +
    '{"type":"function","function":{"name":"connect_database","description":"Connects to a registered database by index or title.","parameters":{"type":"object","properties":{"index":{"type":"integer","description":"Database index (0-based)"},"title":{"type":"string","description":"Database title"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"disconnect_database","description":"Disconnects from a registered database by index or title.","parameters":{"type":"object","properties":{"index":{"type":"integer","description":"Database index (0-based)"},"title":{"type":"string","description":"Database title"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"get_database_metadata","description":"Gets metadata for a registered database (tables, views, procedures, functions, triggers, generators, domains, roles, users). Requires database to be connected.","parameters":{"type":"object","properties":{"index":{"type":"integer","description":"Database index (0-based)"},"title":{"type":"string","description":"Database title"},"object_type":{"type":"string","description":"Type: tables, views, procedures, functions, triggers, generators, domains, roles, users (default: tables)"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"test_server_connection","description":"Tests connection to a registered Firebird server.","parameters":{"type":"object","properties":{"index":{"type":"integer","description":"Server index (0-based)"},"name":{"type":"string","description":"Server name"},"alias":{"type":"string","description":"Server alias"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"get_server_inventory","description":"Gets complete inventory of all registered servers and their databases.","parameters":{"type":"object","properties":{"format":{"type":"string","description":"Output format: text (default) or json"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"get_full_system_context","description":"Gets complete TurboBird system context including all servers, databases, and connection info.","parameters":{"type":"object","properties":{},"required":[]}}},' +
    '{"type":"function","function":{"name":"run_shell_command","description":"Executes a shell command and returns stdout, stderr, and exit code. Dangerous commands (disk formatting, system file deletion, shutdown, etc.) are blocked. All other commands are allowed without restriction.","parameters":{"type":"object","properties":{"command":{"type":"string","description":"The shell command to execute"},"working_dir":{"type":"string","description":"Optional working directory"}},"required":["command"]}}},' +
    '{"type":"function","function":{"name":"read_file","description":"Reads the content of a file at the given path. No size limits.","parameters":{"type":"object","properties":{"path":{"type":"string","description":"File path to read"}},"required":["path"]}}},' +
    '{"type":"function","function":{"name":"write_file","description":"Writes content to a file at the given path. Creates parent directories if needed. No size limits.","parameters":{"type":"object","properties":{"path":{"type":"string","description":"File path to write"},"content":{"type":"string","description":"Content to write"}},"required":["path","content"]}}},' +
    '{"type":"function","function":{"name":"list_directory","description":"Lists contents of a directory (files and subdirectories with sizes).","parameters":{"type":"object","properties":{"path":{"type":"string","description":"Directory path to list (default: current directory)"}},"required":[]}}},' +
    '{"type":"function","function":{"name":"search_files","description":"Searches for files matching a pattern (name contains pattern) recursively in a directory.","parameters":{"type":"object","properties":{"pattern":{"type":"string","description":"Search pattern (matched against file names)"},"path":{"type":"string","description":"Starting directory (default: current directory)"}},"required":["pattern"]}}},' +
    '{"type":"function","function":{"name":"web_search","description":"Searches the web using DuckDuckGo and returns titles, URLs, and snippets. Use to find information, documentation, or answers.","parameters":{"type":"object","properties":{"query":{"type":"string","description":"Search query"}},"required":["query"]}}},' +
    '{"type":"function","function":{"name":"web_fetch","description":"Fetches a web page and returns its text content (HTML stripped). Use to read articles, documentation, or any web page.","parameters":{"type":"object","properties":{"url":{"type":"string","description":"URL to fetch"}},"required":["url"]}}}' +
    ']';
end;
{ ---------- Ollama ---------- }
procedure TOllamaAgentThread.SendOllama(AClient: TFPHTTPClient);
var
  Req, Msg, Opts: TJSONObject;
  Msgs: TJSONArray;
  ReqStream, RespStream: TStringStream;
  RespJSON, Node: TJSONData;
  AssistantMsg: TJSONObject;
  URL: string;
  ToolCalls: TJSONArray;
  i: Integer;
  TC, Func: TJSONObject;
  ToolName, ToolArgs, ToolResult, ToolId: string;
  Iter: Integer;
  HasToolCalls: Boolean;
begin
  URL := StripTrailingSlash(FSettings.BaseURL) + '/api/chat';
  Msgs := TJSONArray.Create;
  if Trim(FSettings.SystemPrompt) <> '' then
  begin
    Msg := TJSONObject.Create;
    Msg.Add('role', 'system');
    Msg.Add('content', FSettings.SystemPrompt);
    Msgs.Add(Msg);
  end;
  Msg := TJSONObject.Create;
  Msg.Add('role', 'user');
  Msg.Add('content', FPrompt);
  Msgs.Add(Msg);
  Iter := 0;
  while Iter < FSettings.MaxIterations do
  begin
    Inc(Iter);
    ReqStream := TStringStream.Create('');
    RespStream := TStringStream.Create('');
    RespJSON := nil;
    try
      Req := TJSONObject.Create;
      Req.Add('model', FSettings.Model);
      { re-add current messages }
      Req.Add('messages', Msgs.Clone as TJSONArray);
      Req.Add('stream', False);
      Opts := TJSONObject.Create;
      Opts.Add('temperature', FSettings.Temperature);
      Req.Add('options', Opts);
      { tools }
      Req.Add('tools', GetJSON(ToolDefinitionsJSON));
      ReqStream.WriteString(Req.AsJSON);
      ReqStream.Position := 0;
      Req.Free;
      AClient.RequestBody := ReqStream;
      try
        AClient.Post(URL, RespStream);
      except
        on E: Exception do
        begin
          FErrorMsg := 'Verbindungsfehler: ' + E.Message;
          Exit;
        end;
      end;
      if AClient.ResponseStatusCode <> 200 then
      begin
        FErrorMsg := Format('HTTP %d: %s', [AClient.ResponseStatusCode, Copy(RespStream.DataString, 1, 500)]);
        Exit;
      end;
      try
        RespJSON := GetJSON(RespStream.DataString);
      except
        on E: Exception do
        begin
          FErrorMsg := 'JSON-Parse-Fehler: ' + E.Message;
          Exit;
        end;
      end;
      if not (RespJSON is TJSONObject) then
      begin
        FErrorMsg := 'Antwort ist kein JSON-Objekt';
        Exit;
      end;
      Node := TJSONObject(RespJSON).Find('error');
      if Assigned(Node) then
      begin
        FErrorMsg := 'API-Fehler: ' + Node.AsString;
        Exit;
      end;
      AssistantMsg := nil;
      Node := TJSONObject(RespJSON).Find('message');
      if Assigned(Node) and (Node is TJSONObject) then
        AssistantMsg := TJSONObject(Node);
      if not Assigned(AssistantMsg) then
      begin
        FErrorMsg := 'Keine message in Antwort';
        Exit;
      end;
      { append assistant message to history }
      Msg := TJSONObject(AssistantMsg.Clone);
      Msgs.Add(Msg);
      { check tool_calls }
      ToolCalls := nil;
      Node := AssistantMsg.Find('tool_calls');
      if Assigned(Node) and (Node is TJSONArray) then ToolCalls := TJSONArray(Node);
      HasToolCalls := Assigned(ToolCalls) and (ToolCalls.Count > 0);
      if not HasToolCalls then
      begin
        Node := AssistantMsg.Find('content');
        if Assigned(Node) then FResultText := Node.AsString;
        if Trim(FResultText) = '' then FResultText := '(leere Antwort vom Modell)';
        RespJSON.Free;
        ReqStream.Free;
        RespStream.Free;
        Exit;
      end;
      { execute tools }
      for i := 0 to ToolCalls.Count - 1 do
      begin
        TC := TJSONObject(ToolCalls.Items[i]);
        Func := nil;
        Node := TC.Find('function');
        if Assigned(Node) and (Node is TJSONObject) then Func := TJSONObject(Node);
        ToolName := '';
        ToolArgs := '{}';
        if Assigned(Func) then
        begin
          if Func.Find('name') <> nil then ToolName := Func.Strings['name'];
          if Func.Find('arguments') <> nil then
            if Func.Find('arguments') is TJSONObject then
              ToolArgs := Func.Find('arguments').AsJSON
            else
              ToolArgs := Func.Find('arguments').AsString;
        end;
        ToolId := '';
        if TC.Find('id') <> nil then ToolId := TC.Strings['id'];
        Inc(FToolCallCount);
        ToolResult := ExecuteTool(ToolName, ToolArgs);
        Msg := TJSONObject.Create;
        Msg.Add('role', 'tool');
        if ToolId <> '' then Msg.Add('tool_call_id', ToolId);
        Msg.Add('content', ToolResult);
        Msgs.Add(Msg);
      end;
      RespJSON.Free;
      ReqStream.Free;
      RespStream.Free;
    except
      on E: Exception do
      begin
        FErrorMsg := 'Fehler: ' + E.Message;
        Exit;
      end;
    end;
  end;
  if Trim(FResultText) = '' then FResultText := '(max iterations reached - task may be incomplete)';
end;
{ ---------- OpenAI compat ---------- }
procedure TOllamaAgentThread.SendOpenAI(AClient: TFPHTTPClient);
var
  Req, Msg: TJSONObject;
  Msgs: TJSONArray;
  ReqStream, RespStream: TStringStream;
  RespJSON, Node: TJSONData;
  Choices: TJSONArray;
  Choice, MsgObj: TJSONObject;
  URL: string;
  ToolCalls: TJSONArray;
  i: Integer;
  TC, Func: TJSONObject;
  ToolName, ToolArgs, ToolResult, ToolId: string;
  Iter: Integer;
  HasToolCalls: Boolean;
  UseTools: Boolean;
begin
  URL := StripTrailingSlash(FSettings.BaseURL) + '/v1/chat/completions';
  Msgs := TJSONArray.Create;
  if Trim(FSettings.SystemPrompt) <> '' then
  begin
    Msg := TJSONObject.Create;
    Msg.Add('role', 'system');
    Msg.Add('content', FSettings.SystemPrompt);
    Msgs.Add(Msg);
  end;
  Msg := TJSONObject.Create;
  Msg.Add('role', 'user');
  Msg.Add('content', FPrompt);
  Msgs.Add(Msg);
  UseTools := True;
  Iter := 0;
  while Iter < 50 do
  begin
    Inc(Iter);
    ReqStream := TStringStream.Create('');
    RespStream := TStringStream.Create('');
    RespJSON := nil;
    try
      Req := TJSONObject.Create;
      Req.Add('model', FSettings.Model);
      Req.Add('messages', Msgs.Clone as TJSONArray);
      Req.Add('stream', False);
      Req.Add('temperature', FSettings.Temperature);
      if UseTools then
        Req.Add('tools', GetJSON(ToolDefinitionsJSON));
      ReqStream.WriteString(Req.AsJSON);
      ReqStream.Position := 0;
      Req.Free;
      AClient.RequestBody := ReqStream;
      try
        AClient.Post(URL, RespStream);
      except
        on E: Exception do
        begin
          FErrorMsg := 'Verbindungsfehler: ' + E.Message;
          Exit;
        end;
      end;
      if AClient.ResponseStatusCode <> 200 then
      begin
        { fallback: retry without tools once }
        if UseTools and (AClient.ResponseStatusCode = 400) then
        begin
          UseTools := False;
          ReqStream.Free;
          RespStream.Free;
          Continue;
        end;
        FErrorMsg := Format('HTTP %d: %s', [AClient.ResponseStatusCode, Copy(RespStream.DataString, 1, 500)]);
        Exit;
      end;
      try
        RespJSON := GetJSON(RespStream.DataString);
      except
        on E: Exception do
        begin
          FErrorMsg := 'JSON-Parse-Fehler: ' + E.Message;
          Exit;
        end;
      end;
      if not (RespJSON is TJSONObject) then
      begin
        FErrorMsg := 'Antwort ist kein JSON-Objekt';
        Exit;
      end;
      Node := TJSONObject(RespJSON).Find('error');
      if Assigned(Node) then
      begin
        FErrorMsg := 'API-Fehler: ' + Node.AsString;
        Exit;
      end;
      Choices := TJSONObject(RespJSON).Arrays['choices'];
      if (not Assigned(Choices)) or (Choices.Count = 0) then
      begin
        FErrorMsg := 'Keine choices in Antwort';
        Exit;
      end;
      if not (Choices.Items[0] is TJSONObject) then
      begin
        FErrorMsg := 'choices[0] ist kein Objekt';
        Exit;
      end;
      Choice := TJSONObject(Choices.Items[0]);
      Node := Choice.Find('message');
      if not (Assigned(Node) and (Node is TJSONObject)) then
      begin
        FErrorMsg := 'Kein message in choices[0]';
        Exit;
      end;
      MsgObj := TJSONObject(Node);
      { append assistant message (clone) }
      Msg := TJSONObject(MsgObj.Clone);
      Msgs.Add(Msg);
      { tool_calls? }
      ToolCalls := nil;
      Node := MsgObj.Find('tool_calls');
      if Assigned(Node) and (Node is TJSONArray) then ToolCalls := TJSONArray(Node);
      HasToolCalls := Assigned(ToolCalls) and (ToolCalls.Count > 0);
      if not HasToolCalls then
      begin
        Node := MsgObj.Find('content');
        if Assigned(Node) and not (Node is TJSONNull) then FResultText := Node.AsString;
        if Trim(FResultText) = '' then FResultText := '(leere Antwort vom Modell)';
        RespJSON.Free;
        ReqStream.Free;
        RespStream.Free;
        Exit;
      end;
      for i := 0 to ToolCalls.Count - 1 do
      begin
        TC := TJSONObject(ToolCalls.Items[i]);
        Func := nil;
        Node := TC.Find('function');
        if Assigned(Node) and (Node is TJSONObject) then Func := TJSONObject(Node);
        ToolName := '';
        ToolArgs := '{}';
        if Assigned(Func) then
        begin
          if Func.Find('name') <> nil then ToolName := Func.Strings['name'];
          if Func.Find('arguments') <> nil then
          begin
            if Func.Find('arguments') is TJSONObject then
              ToolArgs := Func.Find('arguments').AsJSON
            else
              ToolArgs := Func.Find('arguments').AsString;
          end;
        end;
        ToolId := '';
        if TC.Find('id') <> nil then ToolId := TC.Strings['id'];
        Inc(FToolCallCount);
        ToolResult := ExecuteTool(ToolName, ToolArgs);
        Msg := TJSONObject.Create;
        Msg.Add('role', 'tool');
        if ToolId <> '' then Msg.Add('tool_call_id', ToolId);
        Msg.Add('content', ToolResult);
        Msgs.Add(Msg);
      end;
      RespJSON.Free;
      ReqStream.Free;
      RespStream.Free;
    except
      on E: Exception do
      begin
        FErrorMsg := 'Fehler: ' + E.Message;
        Exit;
      end;
    end;
  end;
  if Trim(FResultText) = '' then FResultText := '(max iterations reached - task may be incomplete)';
end;
{ ---------- Gemini (unchanged, no tool calling) ---------- }
procedure TOllamaAgentThread.SendGemini(AClient: TFPHTTPClient);
var
  Req: TJSONObject;
  ReqStream, RespStream: TStringStream;
  RespJSON, Node: TJSONData;
  Steps, ContentArr: TJSONArray;
  Step, ContentObj: TJSONObject;
  StepType, Accum, ModelName, SystemPrefixed, StatusStr: string;
  URL: string;
  i, j: Integer;
begin
  URL := StripTrailingSlash(FSettings.BaseURL) + '/v1beta/interactions';
  ReqStream := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  RespJSON := nil;
  try
    Req := TJSONObject.Create;
    ModelName := FSettings.Model;
    if ModelName = '' then ModelName := 'gemini-3.8-flash';
    Req.Add('model', ModelName);
    if Trim(FSettings.SystemPrompt) <> '' then
      SystemPrefixed := FSettings.SystemPrompt + LineEnding + LineEnding + FPrompt
    else
      SystemPrefixed := FPrompt;
    Req.Add('input', SystemPrefixed);
    ReqStream.WriteString(Req.AsJSON);
    ReqStream.Position := 0;
    Req.Free;
    AClient.RequestBody := ReqStream;
    try
      AClient.Post(URL, RespStream);
    except
      on E: Exception do
      begin
        FErrorMsg := 'Verbindungsfehler: ' + E.Message;
        Exit;
      end;
    end;
    if AClient.ResponseStatusCode <> 200 then
    begin
      FErrorMsg := Format('HTTP %d: %s', [AClient.ResponseStatusCode, Copy(RespStream.DataString, 1, 500)]);
      Exit;
    end;
    try
      RespJSON := GetJSON(RespStream.DataString);
    except
      on E: Exception do
      begin
        FErrorMsg := 'JSON-Parse-Fehler: ' + E.Message;
        Exit;
      end;
    end;
    if not (RespJSON is TJSONObject) then
    begin
      FErrorMsg := 'Antwort ist kein JSON-Objekt';
      Exit;
    end;
    Node := TJSONObject(RespJSON).Find('error');
    if Assigned(Node) then
    begin
      FErrorMsg := 'Gemini-Fehler: ' + Node.AsJSON;
      Exit;
    end;
    StatusStr := '';
    Node := TJSONObject(RespJSON).Find('status');
    if Assigned(Node) then StatusStr := Node.AsString;
    if (StatusStr <> '') and (StatusStr <> 'completed') then
    begin
      FErrorMsg := 'Gemini-Status: ' + StatusStr + ' (erwartet: completed)';
      Exit;
    end;
    Steps := TJSONObject(RespJSON).Arrays['steps'];
    if (not Assigned(Steps)) or (Steps.Count = 0) then
    begin
      FErrorMsg := 'Keine steps in Antwort';
      Exit;
    end;
    Accum := '';
    for i := 0 to Steps.Count - 1 do
    begin
      if not (Steps.Items[i] is TJSONObject) then Continue;
      Step := TJSONObject(Steps.Items[i]);
      StepType := '';
      Node := Step.Find('type');
      if Assigned(Node) then StepType := Node.AsString;
      if StepType <> 'model_output' then Continue;
      ContentArr := Step.Arrays['content'];
      if not Assigned(ContentArr) then Continue;
      for j := 0 to ContentArr.Count - 1 do
      begin
        if not (ContentArr.Items[j] is TJSONObject) then Continue;
        ContentObj := TJSONObject(ContentArr.Items[j]);
        Node := ContentObj.Find('type');
        if (not Assigned(Node)) or (Node.AsString <> 'text') then Continue;
        Node := ContentObj.Find('text');
        if Assigned(Node) then Accum := Accum + Node.AsString;
      end;
    end;
    FResultText := Accum;
    if Trim(FResultText) = '' then FResultText := '(leere Antwort vom Modell)';
  finally
    if Assigned(RespJSON) then RespJSON.Free;
    ReqStream.Free;
    RespStream.Free;
  end;
end;
procedure TOllamaAgentThread.Execute;
var
  Client: TFPHTTPClient;
  Info: TProviderInfo;
begin
  FResultText := '';
  FErrorMsg := '';
  if not Assigned(FSettings) then
  begin
    FErrorMsg := 'Keine Einstellungen';
    Exit;
  end;
  if Trim(FPrompt) = '' then
  begin
    FErrorMsg := 'Leerer Prompt';
    Exit;
  end;
  try
    Client := TFPHTTPClient.Create(nil);
    try
      Client.HTTPVersion := '1.0';
      Client.ConnectTimeout := 5000;
      Client.IOTimeout := 300000;
      Client.AddHeader('Content-Type', 'application/json');
      Client.AddHeader('Connection', 'close');
      Info := FSettings.ProviderInfo;
      if Info.NeedsKey and (Trim(FSettings.ApiKey) <> '') then
      begin
        if Info.Style = asGemini then
          Client.AddHeader('x-goog-api-key', FSettings.ApiKey)
        else
          Client.AddHeader('Authorization', 'Bearer ' + FSettings.ApiKey);
      end;
      case Info.Style of
        asOllama:       SendOllama(Client);
        asOpenAICompat: SendOpenAI(Client);
        asGemini:       SendGemini(Client);
      end;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      FErrorMsg := 'Thread-Fehler: ' + E.Message;
  end;
end;
end.
