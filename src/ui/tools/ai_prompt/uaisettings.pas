unit uAiSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  TApiStyle = (asOllama, asOpenAICompat, asGemini);

  TProviderInfo = record
    Name: string;
    DisplayName: string;
    DefaultURL: string;
    NeedsKey: Boolean;
    Style: TApiStyle;
    ModelHint: string;
  end;

  TAiSettings = class
  private
    FActiveProvider: string;
    FBaseURL: string;
    FApiKey: string;
    FModel: string;
    FTemperature: Double;
    FMaxIterations: integer;
    FSystemPrompt: string;
    FConfigDir: string;
    function GetProviderInfo: TProviderInfo;
    function ProviderFilePath(const AName: string): string;
    procedure WriteProviderFile(const AName: string);
    function ReadProviderFile(const AName: string): Boolean;
    procedure ApplyDefaultsForProvider(const AName: string);
  public
    constructor Create;
    procedure Load;
    procedure Save;
    procedure SetActiveProvider(const AName: string);
    function ProviderConfigExists(const AName: string): Boolean;
    property Provider: string read FActiveProvider;
    property BaseURL: string read FBaseURL write FBaseURL;
    property ApiKey: string read FApiKey write FApiKey;
    property Model: string read FModel write FModel;
    property Temperature: Double read FTemperature write FTemperature;
    property MaxIterations: integer read FMaxIterations write FMaxIterations default 50;
    property SystemPrompt: string read FSystemPrompt write FSystemPrompt;
    property ConfigDir: string read FConfigDir;
    property ProviderInfo: TProviderInfo read GetProviderInfo;
  end;

function GetProviderCount: Integer;
function GetProviderByIndex(AIndex: Integer): TProviderInfo;
function FindProvider(const AName: string): TProviderInfo;

implementation

const
  PROVIDERS: array[0..6] of TProviderInfo = (
    (Name: 'ollama'; DisplayName: 'Ollama (lokal)';
     DefaultURL: 'http://localhost:11434';
     NeedsKey: False; Style: asOllama;
     ModelHint: 'qwen2.5:3b'),
    (Name: 'openai'; DisplayName: 'OpenAI';
     DefaultURL: 'https://api.openai.com';
     NeedsKey: True; Style: asOpenAICompat;
     ModelHint: 'gpt-4o-mini'),
     (Name: 'groq'; DisplayName: 'Groq (kostenloses Tier)';
      DefaultURL: 'https://api.groq.com/openai';
      NeedsKey: True; Style: asOpenAICompat;
      ModelHint: 'openai/gpt-oss-120b'),
      (Name: 'deepseek'; DisplayName: 'DeepSeek (guenstig)';
     DefaultURL: 'https://api.deepseek.com';
     NeedsKey: True; Style: asOpenAICompat;
     ModelHint: 'deepseek-chat'),
     (Name: 'openrouter'; DisplayName: 'OpenRouter (free)';
      DefaultURL: 'https://openrouter.ai/api';
      NeedsKey: True; Style: asOpenAICompat;
      ModelHint: 'nvidia/nemotron-3-ultra-550b-a55b:free'),
      (Name: 'mistral'; DisplayName: 'Mistral';
     DefaultURL: 'https://api.mistral.ai';
     NeedsKey: True; Style: asOpenAICompat;
     ModelHint: 'mistral-large-latest'),
    (Name: 'gemini'; DisplayName: 'Google Gemini (Free Tier)';
     DefaultURL: 'https://generativelanguage.googleapis.com';
     NeedsKey: True; Style: asGemini;
     ModelHint: 'gemini-3-flash-preview')
  );

const
  DEFAULT_SYSTEM_PROMPT =
    'Du bist ein leistungsfaehiger Assistent mit Zugriff auf System-Tools. ' +
    'Verfuegbare Tools: get_current_time, calculate, get_system_info, ' +
    'run_shell_command, read_file, write_file, list_directory, search_files, ' +
    'web_search, web_fetch. ' +
    'Du hast vollen Dateisystem-, Shell- und Internet-Zugriff (ausser ' +
    'gefaehrliche Befehle wie mkfs, rm -rf /, shutdown etc.). ' +
    'Nutze die Tools aktiv, wenn sie bei der Aufgabe helfen. ' +
    'Antworte in der Sprache des Benutzers, praezise und ohne Wiederholungen.';

function GetProviderCount: Integer;
begin
  Result := Length(PROVIDERS);
end;

function GetProviderByIndex(AIndex: Integer): TProviderInfo;
begin
  if (AIndex >= 0) and (AIndex < Length(PROVIDERS)) then
    Result := PROVIDERS[AIndex]
  else
    Result := PROVIDERS[0];
end;

function FindProvider(const AName: string): TProviderInfo;
var
  i: Integer;
begin
  for i := 0 to High(PROVIDERS) do
    if LowerCase(PROVIDERS[i].Name) = LowerCase(AName) then
    begin
      Result := PROVIDERS[i];
      Exit;
    end;
  Result := PROVIDERS[0];
end;

constructor TAiSettings.Create;
var
  AppRoot, DataDir: string;
begin
  { AppRoot = Verzeichnis, in dem die Executable liegt }
  AppRoot := ExtractFilePath(ExpandFileName(ParamStr(0)));
  DataDir := IncludeTrailingPathDelimiter(AppRoot) + 'data' + PathDelim + 'ai';
  if not DirectoryExists(DataDir) then
    ForceDirectories(DataDir);

  FConfigDir := DataDir;

  FActiveProvider := PROVIDERS[0].Name;
  ApplyDefaultsForProvider(FActiveProvider);
end;

function TAiSettings.ProviderFilePath(const AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FConfigDir) +
            'provider_' + LowerCase(AName) + '.json';
end;

procedure TAiSettings.ApplyDefaultsForProvider(const AName: string);
var
  Info: TProviderInfo;
begin
  Info := FindProvider(AName);
  FBaseURL := Info.DefaultURL;
  FApiKey := '';
  FModel := Info.ModelHint;
  FTemperature := 0.3;
  FSystemPrompt := DEFAULT_SYSTEM_PROMPT;
end;

function TAiSettings.ReadProviderFile(const AName: string): Boolean;
var
  Path: string;
  SL: TStringList;
  Data: TJSONData;
  Obj: TJSONObject;
  N: TJSONData;
begin
  Result := False;
  Path := ProviderFilePath(AName);
  if not FileExists(Path) then Exit;

  SL := TStringList.Create;
  try
    try SL.LoadFromFile(Path); except Exit; end;
    Data := nil;
    try
      try Data := GetJSON(SL.Text); except Exit; end;
      if not (Data is TJSONObject) then Exit;
      Obj := TJSONObject(Data);

      N := Obj.Find('base_url');      if Assigned(N) then FBaseURL := N.AsString;
      N := Obj.Find('api_key');       if Assigned(N) then FApiKey := N.AsString;
      N := Obj.Find('model');         if Assigned(N) then FModel := N.AsString;
      N := Obj.Find('temperature');   if Assigned(N) then FTemperature := N.AsFloat;
      N := Obj.Find('system_prompt'); if Assigned(N) then FSystemPrompt := N.AsString;
      Result := True;
    finally
      Data.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TAiSettings.WriteProviderFile(const AName: string);
var
  Path: string;
  Obj: TJSONObject;
  SL: TStringList;
  S: string;
begin
  Path := ProviderFilePath(AName);
  Obj := TJSONObject.Create;
  try
    Obj.Add('base_url', FBaseURL);
    Obj.Add('api_key', FApiKey);
    Obj.Add('model', FModel);
    Obj.Add('temperature', FTemperature);
    Obj.Add('system_prompt', FSystemPrompt);
    S := Obj.FormatJSON;
  finally
    Obj.Free;
  end;

  SL := TStringList.Create;
  try
    SL.Text := S;
    SL.SaveToFile(Path);
  finally
    SL.Free;
  end;

  {$IFDEF UNIX}
  fpChmod(Path, &600);
  {$ENDIF}
end;

function TAiSettings.ProviderConfigExists(const AName: string): Boolean;
begin
  Result := FileExists(ProviderFilePath(AName));
end;

procedure TAiSettings.Load;
var
  MasterPath: string;
  SL: TStringList;
  Data: TJSONData;
  Obj: TJSONObject;
  N: TJSONData;
  OldProvider, OldBaseURL, OldApiKey, OldModel, OldPrompt: string;
  OldTemp: Double;
  HasOldFormat: Boolean;
begin
  MasterPath := IncludeTrailingPathDelimiter(FConfigDir) + 'settings.json';
  HasOldFormat := False;
  OldProvider := '';
  OldBaseURL := '';
  OldApiKey := '';
  OldModel := '';
  OldPrompt := '';
  OldTemp := 0.3;

  if FileExists(MasterPath) then
  begin
    SL := TStringList.Create;
    try
      try SL.LoadFromFile(MasterPath); except SL.Text := ''; end;
      if Trim(SL.Text) <> '' then
      begin
        Data := nil;
        try
          try Data := GetJSON(SL.Text); except Data := nil; end;
          if Assigned(Data) and (Data is TJSONObject) then
          begin
            Obj := TJSONObject(Data);
            N := Obj.Find('active_provider');
            if Assigned(N) then
              FActiveProvider := N.AsString
            else
            begin
              { Alt-Format: 'provider' direkt in der Datei }
              N := Obj.Find('provider');
              if Assigned(N) then
              begin
                OldProvider := N.AsString;
                HasOldFormat := True;
              end;
            end;
            N := Obj.Find('base_url');
            if Assigned(N) then begin OldBaseURL := N.AsString; HasOldFormat := True; end;
            N := Obj.Find('api_key');       if Assigned(N) then OldApiKey := N.AsString;
            N := Obj.Find('model');         if Assigned(N) then OldModel := N.AsString;
            N := Obj.Find('temperature');   if Assigned(N) then OldTemp := N.AsFloat;
            N := Obj.Find('system_prompt'); if Assigned(N) then OldPrompt := N.AsString;
          end;
        finally
          Data.Free;
        end;
      end;
    finally
      SL.Free;
    end;
  end;

  { Provider-Namen validieren }
  if FindProvider(FActiveProvider).Name <> LowerCase(FActiveProvider) then
    FActiveProvider := PROVIDERS[0].Name;

  { Defaults + vorhandene Datei }
  ApplyDefaultsForProvider(FActiveProvider);
  ReadProviderFile(FActiveProvider);

  { Migration: alte settings.json mit allen Feldern drin }
  if HasOldFormat and (OldProvider <> '') and not ProviderConfigExists(OldProvider) then
  begin
    FActiveProvider := FindProvider(OldProvider).Name;
    FBaseURL := OldBaseURL;
    FApiKey := OldApiKey;
    FModel := OldModel;
    FTemperature := OldTemp;
    FSystemPrompt := OldPrompt;
    if FSystemPrompt = '' then FSystemPrompt := DEFAULT_SYSTEM_PROMPT;
    WriteProviderFile(FActiveProvider);
    Save;
  end;
end;

procedure TAiSettings.Save;
var
  MasterPath: string;
  Obj: TJSONObject;
  SL: TStringList;
  S: string;
begin
  if not DirectoryExists(FConfigDir) then
    ForceDirectories(FConfigDir);

  { Aktuellen Provider in seine eigene Datei }
  WriteProviderFile(FActiveProvider);

  { Master-Datei: nur active_provider }
  MasterPath := IncludeTrailingPathDelimiter(FConfigDir) + 'settings.json';
  Obj := TJSONObject.Create;
  try
    Obj.Add('active_provider', FActiveProvider);
    S := Obj.FormatJSON;
  finally
    Obj.Free;
  end;

  SL := TStringList.Create;
  try
    SL.Text := S;
    SL.SaveToFile(MasterPath);
  finally
    SL.Free;
  end;

  {$IFDEF UNIX}
  fpChmod(MasterPath, &600);
  {$ENDIF}
end;

procedure TAiSettings.SetActiveProvider(const AName: string);
var
  Normalized: string;
begin
  Normalized := FindProvider(AName).Name;
  if Normalized = FActiveProvider then Exit;

  { Aktuellen Zustand in die Datei des alten Providers }
  WriteProviderFile(FActiveProvider);

  { Wechsel }
  FActiveProvider := Normalized;

  { Defaults laden, dann Datei des neuen Providers (falls existiert) }
  ApplyDefaultsForProvider(FActiveProvider);
  ReadProviderFile(FActiveProvider);
end;

function TAiSettings.GetProviderInfo: TProviderInfo;
begin
  Result := FindProvider(FActiveProvider);
end;

end.
