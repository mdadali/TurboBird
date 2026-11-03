unit fmAIPrompt;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, Buttons, DBCtrls, DBGrids, SynEdit,
  HtmlView, RxDBGrid, IBQuery, IBDatabase, fphttpclient, fpjson, jsonparser, DB,
  uAiSettings, uOllamaAgent;

type

  { TfmAIPromptForm }

  TfmAIPromptForm = class(TForm)
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    FPriceBtn: TButton;
    FResponseMemo: TMemo;
    HtmlViewer1: THtmlViewer;
    IBDatabase1: TIBDatabase;
    IBQuery1: TIBQuery;
    IBTransaction1: TIBTransaction;
    pgResults: TPageControl;
    RxDBGrid1: TRxDBGrid;
    seCode: TSynEdit;
    tsData: TTabSheet;
    tsWeb: TTabSheet;
    tsCode: TTabSheet;
    tsChat: TTabSheet;
    procedure FCloseBtnClick(Sender: TObject);
    procedure FModelComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FPriceBtnClick(Sender: TObject);
    procedure FProviderComboChange(Sender: TObject);
    procedure FSaveBtnClick(Sender: TObject);
    procedure FSendBtnClick(Sender: TObject);
    procedure FTestBtnClick(Sender: TObject);
  private
    FAgentThread: TOllamaAgentThread;
    FCheckTimer: TTimer;
    FSettings: TAiSettings;
    FUpdatingSettings: Boolean;
    procedure LoadOllamaModels(const AURL: string);
    procedure LoadSettingsIntoUI;
    procedure SaveUIToCurrentSettings;
    procedure CloseClick(Sender: TObject);
    procedure PresetChange(Sender: TObject);
    procedure CheckAgentDone(Sender: TObject);
    procedure ShowKeyCheckChange(Sender: TObject);
    procedure ChatModelChanged(Sender: TObject);
  published
    Container: TPanel;
    FPageControl: TPageControl;
    TabChat: TTabSheet;
    TabSettings: TTabSheet;
    FStatusBar: TStatusBar;

    { Chat-Tab }
    FTopPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    FModelCombo: TComboBox;
    FPresetCombo: TComboBox;
    FTempEdit: TSpinEdit;
    FSendBtn: TBitBtn;
    FCloseBtn: TBitBtn;
    FPromptMemo: TMemo;
    FSplitter: TSplitter;

    { Settings-Tab }
    FProviderLabel: TLabel;
    FBaseURLLabel: TLabel;
    FApiKeyLabel: TLabel;
    FSystemLabel: TLabel;
    FProviderCombo: TComboBox;
    FBaseURLEdit: TEdit;
    FApiKeyEdit: TEdit;
    FShowKeyCheck: TCheckBox;
    FSystemPromptMemo: TMemo;
    FSaveBtn: TBitBtn;
    FTestBtn: TBitBtn;
  public
    procedure SetPrompt(const AText: string);
  end;

function ShowAIPromptForm(const AInitialPrompt: string = ''): TfmAIPromptForm;
function GetPricingURL(const AProvider, AModel: string): string;

implementation

{$R *.lfm}

function ShowAIPromptForm(const AInitialPrompt: string): TfmAIPromptForm;
begin
  Result := TfmAIPromptForm.Create(Application);
  Result.SetPrompt(AInitialPrompt);
  Result.Show;
end;

procedure TfmAIPromptForm.FCloseBtnClick(Sender: TObject);
begin

end;

procedure TfmAIPromptForm.FModelComboChange(Sender: TObject);
begin

end;

procedure TfmAIPromptForm.FormCreate(Sender: TObject);
var
  i: Integer;
  Info: TProviderInfo;
begin
  Caption := 'KI Prompt (Agent)';
  FormStyle := fsStayOnTop;

  FCheckTimer := TTimer.Create(Self);
  FCheckTimer.Interval := 100;
  FCheckTimer.Enabled := False;
  FCheckTimer.OnTimer := @CheckAgentDone;

  FPresetCombo.OnChange := @PresetChange;
  FCloseBtn.OnClick := @CloseClick;
  FShowKeyCheck.OnChange := @ShowKeyCheckChange;
  FModelCombo.OnChange := @ChatModelChanged;

  KeyPreview := True;

  FPresetCombo.Items.Clear;
  FPresetCombo.Items.Add('Kein Preset');
  FPresetCombo.Items.Add('SQL erklaeren');
  FPresetCombo.Items.Add('SQL optimieren');
  FPresetCombo.Items.Add('SQL Indizes');
  FPresetCombo.ItemIndex := 0;

  FProviderCombo.Items.Clear;
  for i := 0 to GetProviderCount - 1 do
  begin
    Info := GetProviderByIndex(i);
    FProviderCombo.Items.Add(Info.DisplayName);
  end;

  FSettings := TAiSettings.Create;
  FSettings.Load;

  LoadSettingsIntoUI;
  FStatusBar.SimpleText := 'Bereit. Konfig: ' + FSettings.ConfigDir;
end;

procedure TfmAIPromptForm.FormShow(Sender: TObject);
begin
end;

procedure TfmAIPromptForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FCheckTimer) then
  begin
    FCheckTimer.Enabled := False;
    FreeAndNil(FCheckTimer);
  end;
  if Assigned(FAgentThread) then
  begin
    FAgentThread.Terminate;
    FAgentThread.WaitFor;
    FreeAndNil(FAgentThread);
  end;
  FreeAndNil(FSettings);
end;

procedure TfmAIPromptForm.FProviderComboChange(Sender: TObject);
var
  NewProviderName: string;
  OldProviderName: string;
begin
  if FUpdatingSettings then Exit;
  if FProviderCombo.ItemIndex < 0 then Exit;

  NewProviderName := GetProviderByIndex(FProviderCombo.ItemIndex).Name;
  OldProviderName := FSettings.Provider;

  { Nichts zu tun, wenn derselbe Provider gewaehlt wurde }
  if LowerCase(NewProviderName) = LowerCase(OldProviderName) then Exit;

  try
    { 1) Aktuelle UI-Werte in FSettings retten - landen beim naechsten
         Schritt automatisch in der Datei des ALTEN Providers. }
    SaveUIToCurrentSettings;

    { 2) Wechsel: schreibt alten Zustand in provider_<alt>.json,
         laedt provider_<neu>.json (oder Defaults falls nicht vorhanden). }
    FSettings.SetActiveProvider(NewProviderName);

    { 3) UI mit den Werten des NEUEN Providers fuellen }
    LoadSettingsIntoUI;

    FStatusBar.SimpleText := 'Provider: ' +
      FSettings.ProviderInfo.DisplayName + ' | ' + FSettings.Model;
  except
    on E: Exception do
    begin
      { Bei Fehler: UI auf alten Provider zurueckrollen }
      FUpdatingSettings := True;
      try
        FProviderCombo.ItemIndex :=
          FProviderCombo.Items.IndexOf(FSettings.ProviderInfo.DisplayName);
        if FProviderCombo.ItemIndex < 0 then
          FProviderCombo.ItemIndex := 0;
      finally
        FUpdatingSettings := False;
      end;
      FStatusBar.SimpleText := 'Provider-Wechsel fehlgeschlagen: ' + E.Message;
    end;
  end;
end;

procedure TfmAIPromptForm.FSaveBtnClick(Sender: TObject);
begin
  SaveUIToCurrentSettings;
  try
    FSettings.Save;
    FStatusBar.SimpleText := 'Gespeichert (' + FSettings.Provider + '): ' +
      FSettings.ConfigDir;
  except
    on E: Exception do
      FStatusBar.SimpleText := 'Fehler beim Speichern: ' + E.Message;
  end;
end;

procedure TfmAIPromptForm.LoadOllamaModels(const AURL: string);
var
  Client: TFPHTTPClient;
  Resp: TStringStream;
  Data: TJSONData;
  Arr: TJSONArray;
  i: Integer;
  URL: string;
begin
  URL := AURL;
  while (URL <> '') and (URL[Length(URL)] = '/') do
    Delete(URL, Length(URL), 1);
  URL := URL + '/api/tags';

  FModelCombo.Items.Clear;
  Client := TFPHTTPClient.Create(nil);
  Resp := TStringStream.Create('');
  try
    Client.ConnectTimeout := 2000;
    Client.IOTimeout := 5000;
    try
      Client.Get(URL, Resp);
      if Client.ResponseStatusCode = 200 then
      begin
        Data := GetJSON(Resp.DataString);
        try
          if Data is TJSONObject then
          begin
            Arr := TJSONObject(Data).Arrays['models'];
            if Assigned(Arr) then
              for i := 0 to Arr.Count - 1 do
                FModelCombo.Items.Add(Arr.Objects[i].Strings['name']);
          end;
        finally
          Data.Free;
        end;
      end;
    except
      on E: Exception do
        FStatusBar.SimpleText := 'Modell-Liste fehlgeschlagen: ' + E.Message;
    end;
  finally
    Resp.Free;
    Client.Free;
  end;
end;

procedure TfmAIPromptForm.LoadSettingsIntoUI;
var
  Info: TProviderInfo;
  i: Integer;
  FoundIdx: Integer;
begin
  FUpdatingSettings := True;
  try
    { 1) Provider-Combo }
    FoundIdx := 0;
    for i := 0 to GetProviderCount - 1 do
      if LowerCase(GetProviderByIndex(i).Name) = LowerCase(FSettings.Provider) then
      begin
        FoundIdx := i;
        Break;
      end;
    FProviderCombo.ItemIndex := FoundIdx;
    Info := GetProviderByIndex(FoundIdx);

    { 2) URL, Key, System-Prompt, Temperatur }
    FBaseURLEdit.Text := FSettings.BaseURL;
    FApiKeyEdit.Text := FSettings.ApiKey;
    FApiKeyEdit.Enabled := Info.NeedsKey;
    FShowKeyCheck.Enabled := Info.NeedsKey;
    FShowKeyCheck.Checked := False;
    if Info.NeedsKey then
      FApiKeyEdit.PasswordChar := '*'
    else
      FApiKeyEdit.PasswordChar := #0;

    FSystemPromptMemo.Text := FSettings.SystemPrompt;
    FTempEdit.Value := Round(FSettings.Temperature * 10);

    { 3) Modell-Liste je nach Provider }
    FModelCombo.Items.Clear;
    case Info.Style of
      asOllama:
        LoadOllamaModels(FSettings.BaseURL);

      asGemini:
        begin
          FModelCombo.Items.Add('gemini-3-flash-preview');
          FModelCombo.Items.Add('gemini-3.1-flash-lite');
          FModelCombo.Items.Add('gemini-3.1-flash-lite-preview');
        end;

      asOpenAICompat:
        begin
          if LowerCase(Info.Name) = 'groq' then
          begin
            FModelCombo.Items.Add('openai/gpt-oss-120b');   { Standard, stärkstes }
            FModelCombo.Items.Add('openai/gpt-oss-20b');    { schneller, etwas kleiner }
            FModelCombo.Items.Add('qwen/qwen3.8-27b');      { Alternative }
          end
          else if LowerCase(Info.Name) = 'openrouter' then
          begin
            { Stand 2026-11 — bei Aenderungen /v1/models abfragen }
            FModelCombo.Items.Add('nvidia/nemotron-3-ultra-550b-a55b:free');    { getestet }
            FModelCombo.Items.Add('google/gemma-4-31b-it:free');                { manchmal 429 }
            FModelCombo.Items.Add('cohere/north-mini-code:free');               { Code-Modell }
            FModelCombo.Items.Add('nvidia/nemotron-3-nano-omni-30b-a3b-reasoning:free');
            FModelCombo.Items.Add('google/gemma-4-26b-a4b-it:free');
          end
          else
            { OpenAI, DeepSeek, Mistral, ... }
            FModelCombo.Items.Add(Info.ModelHint);
        end;
    end;

    { 4) Gespeichertes Modell selektieren }
    if Trim(FSettings.Model) <> '' then
    begin
      if FModelCombo.Items.IndexOf(FSettings.Model) < 0 then
        FModelCombo.Items.Insert(0, FSettings.Model);
      FModelCombo.ItemIndex := FModelCombo.Items.IndexOf(FSettings.Model);
    end
    else if FModelCombo.Items.Count > 0 then
    begin
      FModelCombo.ItemIndex := 0;
      FSettings.Model := FModelCombo.Items[0];
    end;
  finally
    FUpdatingSettings := False;
  end;
end;

procedure TfmAIPromptForm.SaveUIToCurrentSettings;
begin
  { Provider wird NICHT hier gesetzt - das macht SetActiveProvider.
    Nur die Wert-Felder werden aus der UI in FSettings geschrieben. }
  FSettings.BaseURL := FBaseURLEdit.Text;
  FSettings.ApiKey := FApiKeyEdit.Text;

  if FModelCombo.ItemIndex >= 0 then
    FSettings.Model := FModelCombo.Items[FModelCombo.ItemIndex]
  else
    FSettings.Model := FModelCombo.Text;

  FSettings.Temperature := FTempEdit.Value / 10.0;
  FSettings.SystemPrompt := FSystemPromptMemo.Text;
end;

procedure TfmAIPromptForm.ChatModelChanged(Sender: TObject);
begin
  { Nichts tun — wird beim Senden/Speichern ausgelesen. }
end;

procedure TfmAIPromptForm.ShowKeyCheckChange(Sender: TObject);
begin
  if FShowKeyCheck.Checked then
    FApiKeyEdit.PasswordChar := #0
  else
    FApiKeyEdit.PasswordChar := '*';
end;

procedure TfmAIPromptForm.FSendBtnClick(Sender: TObject);
var
  Prompt: string;
begin
  if Trim(FPromptMemo.Text) = '' then
  begin
    FStatusBar.SimpleText := 'Kein Prompt eingegeben.';
    Exit;
  end;
  if (FModelCombo.ItemIndex < 0) and (Trim(FModelCombo.Text) = '') then
  begin
    FStatusBar.SimpleText := 'Kein Modell ausgewaehlt.';
    Exit;
  end;
  if not FSendBtn.Enabled then Exit;

  if Assigned(FAgentThread) then
  begin
    if FAgentThread.Finished then
      FreeAndNil(FAgentThread)
    else
    begin
      FStatusBar.SimpleText := 'Agent laeuft noch, bitte warten.';
      Exit;
    end;
  end;

  SaveUIToCurrentSettings;

  FSendBtn.Enabled := False;
  FResponseMemo.Clear;
  FStatusBar.SimpleText := 'Agent laeuft (' + FSettings.Provider + '/' +
    FSettings.Model + ')...';
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;

  Prompt := FPromptMemo.Text;

  FAgentThread := TOllamaAgentThread.Create(True, FSettings);
  try
    FAgentThread.FreeOnTerminate := False;
    FAgentThread.Prompt := Prompt;
    FAgentThread.Start;
    FCheckTimer.Enabled := True;
  except
    on E: Exception do
    begin
      FreeAndNil(FAgentThread);
      FStatusBar.SimpleText := 'Fehler beim Start: ' + E.Message;
      FSendBtn.Enabled := True;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfmAIPromptForm.FTestBtnClick(Sender: TObject);
var
  Client: TFPHTTPClient;
  Resp, ReqStream: TStringStream;
  Req, Msg, Part, Content, SysInstr, GenCfg: TJSONObject;
  Msgs, Parts, Contents: TJSONArray;
  URL: string;
  Info: TProviderInfo;
begin
  SaveUIToCurrentSettings;
  Info := FSettings.ProviderInfo;
  URL := FSettings.BaseURL;
  while (URL <> '') and (URL[Length(URL)] = '/') do
    Delete(URL, Length(URL), 1);

  Client := TFPHTTPClient.Create(nil);
  Resp := TStringStream.Create('');
  ReqStream := TStringStream.Create('');
  try
    Client.ConnectTimeout := 5000;
    Client.IOTimeout := 30000;
    Client.AddHeader('Content-Type', 'application/json');
    if Info.NeedsKey and (Trim(FSettings.ApiKey) <> '') then
    begin
      if Info.Style = asGemini then
        Client.AddHeader('x-goog-api-key', FSettings.ApiKey)
      else
        Client.AddHeader('Authorization', 'Bearer ' + FSettings.ApiKey);
    end;

    Req := TJSONObject.Create;
    try
      case Info.Style of
        asOllama:
        begin
          Req.Add('model', FSettings.Model);
          Msgs := TJSONArray.Create;
          Msg := TJSONObject.Create;
          Msg.Add('role', 'user');
          Msg.Add('content', 'ping');
          Msgs.Add(Msg);
          Req.Add('messages', Msgs);
          Req.Add('stream', False);
          Req.Add('options', TJSONObject.Create);
          URL := URL + '/api/chat';
        end;
        asOpenAICompat:
        begin
          Req.Add('model', FSettings.Model);
          Msgs := TJSONArray.Create;
          Msg := TJSONObject.Create;
          Msg.Add('role', 'user');
          Msg.Add('content', 'ping');
          Msgs.Add(Msg);
          Req.Add('messages', Msgs);
          Req.Add('stream', False);
          URL := URL + '/v1/chat/completions';
        end;
        asGemini:
        begin
          Req.Add('model', FSettings.Model);
          Req.Add('input', 'ping');
          URL := URL + '/v1beta/interactions';
        end;
      end;
      ReqStream.WriteString(Req.AsJSON);
      ReqStream.Position := 0;
    finally
      Req.Free;
    end;

    Client.RequestBody := ReqStream;
    try
      Client.Post(URL, Resp);
      if Client.ResponseStatusCode = 200 then
        FStatusBar.SimpleText := 'Verbindung OK (HTTP 200)'
      else
        FStatusBar.SimpleText := 'HTTP ' + IntToStr(Client.ResponseStatusCode) +
          ': ' + Copy(Resp.DataString, 1, 200);
    except
      on E: Exception do
        FStatusBar.SimpleText := 'Test fehlgeschlagen: ' + E.Message;
    end;
  finally
    ReqStream.Free;
    Resp.Free;
    Client.Free;
  end;
end;

procedure TfmAIPromptForm.CheckAgentDone(Sender: TObject);
var
  Agent: TOllamaAgentThread;
  ResponseText: string;
  IsCode, IsWeb: Boolean;
  HtmlText: string;
  i, j, k: Integer;
  URL: string;
begin
  if not Assigned(FAgentThread) then
  begin
    FCheckTimer.Enabled := False;
    Exit;
  end;
  if not FAgentThread.Finished then Exit;

  FCheckTimer.Enabled := False;
  Agent := FAgentThread;
  FAgentThread := nil;

  try
    if Agent.ErrorMsg <> '' then
    begin
      FResponseMemo.Font.Color := clRed;
      FResponseMemo.Lines.Text := Agent.ErrorMsg;
      FStatusBar.SimpleText := 'Fehler';
    end
    else
    begin
      ResponseText := Agent.ResultText;
      FResponseMemo.Font.Color := clWindowText;
      FResponseMemo.Lines.Text := ResponseText;

      IsCode := (Pos('```', ResponseText) > 0) or
                (Pos('procedure ', ResponseText) > 0) or
                (Pos('function ', ResponseText) > 0) or
                (Pos('unit ', ResponseText) > 0) or
                (Pos('program ', ResponseText) > 0);

      IsWeb := (Pos('http://', ResponseText) > 0) or
               (Pos('https://', ResponseText) > 0) or
               (Pos('Search results for', ResponseText) > 0) or
               (Pos('URL:', ResponseText) > 0);

      if IsCode then
      begin
        seCode.Text := ResponseText;
        pgResults.ActivePage := tsCode;
      end
      else if IsWeb then
      begin
        { Einfache HTML-Ausgabe ohne URL-Manipulation (verhindert Endlosschleife) }
        HtmlText := StringReplace(StringReplace(ResponseText, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
        HtmlText := StringReplace(HtmlText, LineEnding, '<br>', [rfReplaceAll]);
        HtmlText := '<html><body style="font-family: sans-serif; padding: 15px; line-height: 1.6;">' +
                    '<div style="white-space: pre-wrap; word-wrap: break-word;">' +
                    HtmlText +
                    '</div></body></html>';
        HtmlViewer1.LoadFromString(HtmlText);
        pgResults.ActivePage := tsWeb;
      end
      else
      begin
        pgResults.ActivePage := tsChat;
      end;

      if Agent.ToolCallCount > 0 then
        FStatusBar.SimpleText := Format('Fertig (%s/%s) - %d Tool-Call(s)',
          [FSettings.Provider, FSettings.Model, Agent.ToolCallCount])
      else
        FStatusBar.SimpleText := Format('Fertig (%s/%s)',
          [FSettings.Provider, FSettings.Model]);
    end;
  finally
    Agent.Free;
    FSendBtn.Enabled := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmAIPromptForm.PresetChange(Sender: TObject);
var
  SQLBody, Prefix: string;
begin
  if FPromptMemo.Text = '' then
    SQLBody := ''
  else
  begin
    SQLBody := FPromptMemo.Text;
    Prefix := '---SQL---' + LineEnding;
    if Pos(Prefix, SQLBody) > 0 then
      SQLBody := Copy(SQLBody, Pos(Prefix, SQLBody) + Length(Prefix), MaxInt);
  end;
  case FPresetCombo.ItemIndex of
    1: FPromptMemo.Text :=
         'Erklaere diese Firebird-SQL kurz und auf Deutsch. ' +
         'Antworte in 3-5 Saetzen, ohne den SQL-Text zu wiederholen.' +
         LineEnding + LineEnding + '---SQL---' + LineEnding + SQLBody;
    2: FPromptMemo.Text :=
         'Optimiere diese Firebird-SQL. Nenne konkrete Vorschlaege ' +
         '(Indizes, Umformulierungen, EXISTS vs. IN usw.). ' +
         'Gib am Ende die optimierte Query aus.' +
         LineEnding + LineEnding + '---SQL---' + LineEnding + SQLBody;
    3: FPromptMemo.Text :=
         'Welche Indizes wuerden diese Firebird-SQL beschleunigen? ' +
         'Gib die CREATE INDEX-Statements an und begruende kurz.' +
         LineEnding + LineEnding + '---SQL---' + LineEnding + SQLBody;
  end;
end;

procedure TfmAIPromptForm.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmAIPromptForm.SetPrompt(const AText: string);
begin
  FPromptMemo.Text := AText;
  FPromptMemo.SelStart := Length(AText);
end;

{ Liefert die passende Preis-Seite fuer den aktuellen Provider + Modell.
  Bei OpenRouter mit spezifischer Modell-Seite, sonst generische
  Provider-Pricing-Page. Leerer String = kein Pricing verfuegbar
  (z.B. lokales Ollama). }
function GetPricingURL(const AProvider, AModel: string): string;
var
  P, M: string;
begin
  P := LowerCase(AProvider);
  M := AModel;

  if P = 'ollama' then
    { Lokal - keine Kosten }
    Result := 'https://ollama.com/'
  else if P = 'openai' then
    Result := 'https://openai.com/api/pricing/'
  else if P = 'groq' then
    Result := 'https://groq.com/pricing/'
  else if P = 'deepseek' then
    Result := 'https://api-docs.deepseek.com/quick_start/pricing'
  else if P = 'mistral' then
    Result := 'https://mistral.ai/pricing'
  else if P = 'gemini' then
    Result := 'https://ai.google.dev/pricing'
  else if P = 'openrouter' then
  begin
    { OpenRouter hat pro Modell eine eigene Seite:
        https://openrouter.ai/<provider>/<model>
      Beispiel: nemotron-3-ultra-550b-a55b:free
        -> https://openrouter.ai/nvidia/nemotron-3-ultra-550b-a55b
      Der :free-Suffix muss weg, sonst 404. }
    M := StringReplace(M, ':free', '', [rfReplaceAll]);
    M := StringReplace(M, ':beta', '', [rfReplaceAll]);
    M := StringReplace(M, ':extended', '', [rfReplaceAll]);
    Result := 'https://openrouter.ai/' + M;
  end
  else
    { Unbekannter Provider -> Fallback auf OpenRouter-Modellliste }
    Result := 'https://openrouter.ai/models';
end;

procedure TfmAIPromptForm.FPriceBtnClick(Sender: TObject);
var
  ProviderName, ModelName, URL: string;
begin
  { Aktuellen Stand aus der UI holen - nicht aus FSettings,
    weil der User zwischen Provider-Wechsel und Klick nichts
    gespeichert haben muss. }
  if FProviderCombo.ItemIndex >= 0 then
    ProviderName := GetProviderByIndex(FProviderCombo.ItemIndex).Name
  else
    ProviderName := FSettings.Provider;

  if FModelCombo.ItemIndex >= 0 then
    ModelName := FModelCombo.Items[FModelCombo.ItemIndex]
  else
    ModelName := FModelCombo.Text;

  { Ollama: Hinweis statt Browser }
  if LowerCase(ProviderName) = 'ollama' then
  begin
    MessageDlg('Lokales Modell',
      'Ollama laeuft lokal auf deinem Rechner - keine Kosten, keine API-Gebuehren.',
      mtInformation, [mbOK], 0);
    Exit;
  end;

  URL := GetPricingURL(ProviderName, ModelName);

  { Sicherheitscheck: URL muss mit http(s) beginnen }
  if (Pos('http://', URL) <> 1) and (Pos('https://', URL) <> 1) then
  begin
    FStatusBar.SimpleText := 'Ungueltige Pricing-URL: ' + URL;
    Exit;
  end;

  try
    OpenURL(URL);
    FStatusBar.SimpleText := 'Preis-Seite geoeffnet: ' + URL;
  except
    on E: Exception do
      FStatusBar.SimpleText := 'Browser konnte nicht geoeffnet werden: ' + E.Message;
  end;
end;


initialization

end.
