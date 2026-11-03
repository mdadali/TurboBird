unit uOllamaAgent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser,
  opensslsockets,
  uAiSettings;

type
  TOllamaAgentThread = class(TThread)
  private
    FSettings: TAiSettings;
    FPrompt: string;
    FResultText: string;
    FErrorMsg: string;
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
end;

function TOllamaAgentThread.GetToolCallCount: Integer;
begin
  Result := 0;
end;

function StripTrailingSlash(const S: string): string;
begin
  Result := S;
  while (Result <> '') and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

procedure TOllamaAgentThread.SendOllama(AClient: TFPHTTPClient);
var
  Req, Msg, Opts: TJSONObject;
  Msgs: TJSONArray;
  ReqStream, RespStream: TStringStream;
  RespJSON, Node: TJSONData;
  AssistantMsg: TJSONObject;
  URL: string;
begin
  URL := StripTrailingSlash(FSettings.BaseURL) + '/api/chat';
  ReqStream := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  RespJSON := nil;
  try
    Req := TJSONObject.Create;
    try
      Req.Add('model', FSettings.Model);
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
      Req.Add('messages', Msgs);
      Req.Add('stream', False);
      Opts := TJSONObject.Create;
      Opts.Add('temperature', FSettings.Temperature);
      Req.Add('options', Opts);
      ReqStream.WriteString(Req.AsJSON);
      ReqStream.Position := 0;
    finally
      Req.Free;
    end;

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
      FErrorMsg := Format('HTTP %d: %s',
        [AClient.ResponseStatusCode, Copy(RespStream.DataString, 1, 500)]);
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
    Node := AssistantMsg.Find('content');
    if Assigned(Node) then
      FResultText := Node.AsString;
    if Trim(FResultText) = '' then
      FResultText := '(leere Antwort vom Modell)';
  finally
    RespJSON.Free;
    ReqStream.Free;
    RespStream.Free;
  end;
end;

procedure TOllamaAgentThread.SendOpenAI(AClient: TFPHTTPClient);
var
  Req, Msg: TJSONObject;
  Msgs: TJSONArray;
  ReqStream, RespStream: TStringStream;
  RespJSON, Node: TJSONData;
  Choices: TJSONArray;
  Choice, MsgObj: TJSONObject;
  URL: string;
begin
  URL := StripTrailingSlash(FSettings.BaseURL) + '/v1/chat/completions';
  ReqStream := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  RespJSON := nil;
  try
    Req := TJSONObject.Create;
    try
      Req.Add('model', FSettings.Model);
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
      Req.Add('messages', Msgs);
      Req.Add('stream', False);
      Req.Add('temperature', FSettings.Temperature);
      ReqStream.WriteString(Req.AsJSON);
      ReqStream.Position := 0;
    finally
      Req.Free;
    end;

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
      FErrorMsg := Format('HTTP %d: %s',
        [AClient.ResponseStatusCode, Copy(RespStream.DataString, 1, 500)]);
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
    Node := MsgObj.Find('content');
    if Assigned(Node) then
      FResultText := Node.AsString;
    if Trim(FResultText) = '' then
      FResultText := '(leere Antwort vom Modell)';
  finally
    RespJSON.Free;
    ReqStream.Free;
    RespStream.Free;
  end;
end;

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
    try
      ModelName := FSettings.Model;
      if ModelName = '' then
        ModelName := 'gemini-3.8-flash';
      Req.Add('model', ModelName);

      { System-Prompt vorne anhaengen (Interactions API hat kein
        dokumentiertes systemInstruction-Feld im v1beta-REST-Format). }
      if Trim(FSettings.SystemPrompt) <> '' then
        SystemPrefixed := FSettings.SystemPrompt + LineEnding + LineEnding + FPrompt
      else
        SystemPrefixed := FPrompt;
      Req.Add('input', SystemPrefixed);

      ReqStream.WriteString(Req.AsJSON);
      ReqStream.Position := 0;
    finally
      Req.Free;
    end;

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
      FErrorMsg := Format('HTTP %d: %s',
        [AClient.ResponseStatusCode, Copy(RespStream.DataString, 1, 500)]);
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

    { Alle model_output-Steps durchsuchen, text-Content zusammensetzen }
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
        if Assigned(Node) then
          Accum := Accum + Node.AsString;
      end;
    end;

    FResultText := Accum;
    if Trim(FResultText) = '' then
      FResultText := '(leere Antwort vom Modell)';
  finally
    RespJSON.Free;
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
