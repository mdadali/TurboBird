unit fmAIPrompt;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, ComCtrls, Buttons, fphttpclient, fpjson, jsonparser;

type
  TfmAIPromptForm = class(TForm)
  private
    FTopPanel: TPanel;
    FModelCombo: TComboBox;
    FPresetCombo: TComboBox;
    FTempEdit: TSpinEdit;
    FSendBtn: TBitBtn;
    FCloseBtn: TBitBtn;
    FPromptMemo: TMemo;
    FSplitter: TSplitter;
    FResponseMemo: TMemo;
    FStatusBar: TStatusBar;

    procedure BuildUI;
    procedure LoadModels;
    procedure SendClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure PresetChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function AskOllama(const Prompt, Model: string; Temperature: Double;
      out ErrMsg: string; out EvalCount: Integer): string;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    procedure SetPrompt(const AText: string);
  end;

function ShowAIPromptForm(const AInitialPrompt: string = ''): TfmAIPromptForm;

implementation

function ShowAIPromptForm(const AInitialPrompt: string): TfmAIPromptForm;
begin
  Result := TfmAIPromptForm.CreateNew(Application);
  Result.SetPrompt(AInitialPrompt);
  Result.Show;
end;

{ TfmAIPromptForm }

constructor TfmAIPromptForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Caption := 'KI Prompt';
  Width   := 900;
  Height  := 720;
  Position := poScreenCenter;
  FormStyle := fsStayOnTop;
  BuildUI;
  LoadModels;
end;

procedure TfmAIPromptForm.BuildUI;
var
  lbl: TLabel;
  Container: TPanel;
begin
  Container := TPanel.Create(Self);
  Container.Parent := Self;
  Container.Align := alClient;
  Container.BevelOuter := bvNone;

  // --- Toolbar ---
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Container;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 44;
  FTopPanel.BevelOuter := bvNone;

  lbl := TLabel.Create(Self);
  lbl.Parent := FTopPanel;
  lbl.Caption := 'Modell:';
  lbl.Left := 8;
  lbl.Top := 14;

  FModelCombo := TComboBox.Create(Self);
  FModelCombo.Parent := FTopPanel;
  FModelCombo.Left := 65;
  FModelCombo.Top := 10;
  FModelCombo.Width := 220;
  FModelCombo.Style := csDropDownList;

  lbl := TLabel.Create(Self);
  lbl.Parent := FTopPanel;
  lbl.Caption := 'Preset:';
  lbl.Left := 300;
  lbl.Top := 14;

  FPresetCombo := TComboBox.Create(Self);
  FPresetCombo.Parent := FTopPanel;
  FPresetCombo.Left := 350;
  FPresetCombo.Top := 10;
  FPresetCombo.Width := 140;
  FPresetCombo.Style := csDropDownList;
  FPresetCombo.Items.Add('(frei)');
  FPresetCombo.Items.Add('Erklären');
  FPresetCombo.Items.Add('Optimieren');
  FPresetCombo.Items.Add('Indizes');
  FPresetCombo.ItemIndex := 0;
  FPresetCombo.OnChange := @PresetChange;

  lbl := TLabel.Create(Self);
  lbl.Parent := FTopPanel;
  lbl.Caption := 'Temp:';
  lbl.Left := 505;
  lbl.Top := 14;

  FTempEdit := TSpinEdit.Create(Self);
  FTempEdit.Parent := FTopPanel;
  FTempEdit.Left := 545;
  FTempEdit.Top := 10;
  FTempEdit.Width := 65;
  FTempEdit.MinValue := 0;
  FTempEdit.MaxValue := 20;
  FTempEdit.Value := 3;              // 0.3 ist gut für SQL

  FSendBtn := TBitBtn.Create(Self);
  FSendBtn.Parent := FTopPanel;
  FSendBtn.Left := 630;
  FSendBtn.Top := 7;
  FSendBtn.Width := 110;
  FSendBtn.Height := 30;
  FSendBtn.Caption := 'Senden (F9)';
  FSendBtn.OnClick := @SendClick;

  FCloseBtn := TBitBtn.Create(Self);
  FCloseBtn.Parent := FTopPanel;
  FCloseBtn.Left := 750;
  FCloseBtn.Top := 7;
  FCloseBtn.Width := 100;
  FCloseBtn.Height := 30;
  FCloseBtn.Caption := 'Schließen';
  FCloseBtn.OnClick := @CloseClick;

  // --- Prompt-Memo ---
  FPromptMemo := TMemo.Create(Self);
  FPromptMemo.Parent := Container;
  FPromptMemo.Align := alTop;
  FPromptMemo.Height := 240;
  FPromptMemo.ScrollBars := ssBoth;
  FPromptMemo.Font.Name := 'Monospace';
  FPromptMemo.Font.Size := 10;
  FPromptMemo.WordWrap := True;

  // --- Splitter ---
  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Container;
  FSplitter.Align := alTop;
  FSplitter.Height := 6;
  FSplitter.Cursor := crVSplit;

  // --- Antwort-Memo ---
  FResponseMemo := TMemo.Create(Self);
  FResponseMemo.Parent := Container;
  FResponseMemo.Align := alClient;
  FResponseMemo.ScrollBars := ssBoth;
  FResponseMemo.Font.Name := 'Monospace';
  FResponseMemo.Font.Size := 10;
  FResponseMemo.ReadOnly := True;
  FResponseMemo.WordWrap := True;

  // --- Statusbar ---
  FStatusBar := TStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.SimplePanel := True;
  FStatusBar.SimpleText := 'Bereit';

  KeyPreview := True;
  OnKeyDown := @FormKeyDown;
end;

procedure TfmAIPromptForm.LoadModels;
var
  Client: TFPHTTPClient;
  Resp: TStringStream;
  Data: TJSONData;
  Arr: TJSONArray;
  i, DefaultIdx: Integer;
begin
  FModelCombo.Items.Clear;
  DefaultIdx := -1;

  Client := TFPHTTPClient.Create(nil);
  Resp   := TStringStream.Create('');
  try
    Client.ConnectTimeout := 2000;
    Client.IOTimeout      := 5000;
    try
      Client.Get('http://localhost:11434/api/tags', Resp);
      if Client.ResponseStatusCode = 200 then
      begin
        Data := GetJSON(Resp.DataString);
        try
          if Data is TJSONObject then
          begin
            Arr := TJSONObject(Data).Arrays['models'];
            if Assigned(Arr) then
              for i := 0 to Arr.Count - 1 do
              begin
                FModelCombo.Items.Add(Arr.Objects[i].Strings['name']);
                if Pos('qwen2.5-coder', Arr.Objects[i].Strings['name']) = 1 then
                  DefaultIdx := i;
              end;
          end;
        finally
          Data.Free;
        end;
      end;
    except
      // still
    end;
  finally
    Resp.Free;
    Client.Free;
  end;

  if FModelCombo.Items.Count = 0 then
  begin
    FModelCombo.Items.Add('(Ollama nicht erreichbar)');
    FModelCombo.ItemIndex := 0;
    FSendBtn.Enabled := False;
  end
  else
  begin
    if DefaultIdx < 0 then DefaultIdx := 0;
    FModelCombo.ItemIndex := DefaultIdx;
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
    // Alles ab der ersten leeren Zeile nach dem Präfix als SQL betrachten
    // Einfacher: aktuellen Inhalt hinter einem Marker suchen
    SQLBody := FPromptMemo.Text;
    Prefix := '---SQL---' + LineEnding;
    if Pos(Prefix, SQLBody) > 0 then
      SQLBody := Copy(SQLBody, Pos(Prefix, SQLBody) + Length(Prefix), MaxInt);
  end;

  case FPresetCombo.ItemIndex of
    1: FPromptMemo.Text :=
         'Erkläre diese Firebird-SQL kurz und auf Deutsch. ' +
         'Antworte in 3-5 Sätzen, ohne den SQL-Text zu wiederholen.' +
         LineEnding + LineEnding + '---SQL---' + LineEnding + SQLBody;
    2: FPromptMemo.Text :=
         'Optimiere diese Firebird-SQL. Nenne konkrete Vorschläge ' +
         '(Indizes, Umformulierungen, EXISTS vs. IN usw.). ' +
         'Gib am Ende die optimierte Query aus.' +
         LineEnding + LineEnding + '---SQL---' + LineEnding + SQLBody;
    3: FPromptMemo.Text :=
         'Welche Indizes würden diese Firebird-SQL beschleunigen? ' +
         'Gib die CREATE INDEX-Statements an und begründe kurz.' +
         LineEnding + LineEnding + '---SQL---' + LineEnding + SQLBody;
  end;
end;

procedure TfmAIPromptForm.SendClick(Sender: TObject);
var
  Prompt, Antwort, ErrMsg: string;
  EvalCount, ElapsedMs: Integer;
  StartTime: TDateTime;
begin
  if Trim(FPromptMemo.Text) = '' then
  begin
    FStatusBar.SimpleText := 'Kein Prompt eingegeben.';
    Exit;
  end;

  if (FModelCombo.ItemIndex < 0) or (not FSendBtn.Enabled) then
  begin
    FStatusBar.SimpleText := 'Kein Modell ausgewählt.';
    Exit;
  end;

  FSendBtn.Enabled := False;
  Screen.Cursor := crHourGlass;
  FStatusBar.SimpleText := 'Anfrage läuft...';
  FResponseMemo.Font.Color := clWindowText;
  FResponseMemo.Clear;
  Application.ProcessMessages;
  StartTime := Now;

  try
    Prompt := FPromptMemo.Text;
    Antwort := AskOllama(
      Prompt,
      FModelCombo.Items[FModelCombo.ItemIndex],
      FTempEdit.Value / 10.0,
      ErrMsg,
      EvalCount);

    ElapsedMs := Round((Now - StartTime) * 24 * 60 * 60 * 1000);

    if ErrMsg <> '' then
    begin
      FResponseMemo.Font.Color := clRed;
      FResponseMemo.Lines.Text := ErrMsg;
      FStatusBar.SimpleText := Format('Fehler nach %d ms', [ElapsedMs]);
    end
    else
    begin
      FResponseMemo.Lines.Text := Antwort;
      FStatusBar.SimpleText := Format(
        'Fertig in %d ms · %d Tokens generiert',
        [ElapsedMs, EvalCount]);
    end;
  finally
    Screen.Cursor := crDefault;
    FSendBtn.Enabled := True;
  end;
end;

procedure TfmAIPromptForm.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmAIPromptForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F9 then
  begin
    SendClick(nil);
    Key := 0;
  end;
end;

procedure TfmAIPromptForm.SetPrompt(const AText: string);
begin
  FPromptMemo.Text := AText;
  FPromptMemo.SelStart := Length(AText);
end;

function TfmAIPromptForm.AskOllama(const Prompt, Model: string;
  Temperature: Double; out ErrMsg: string; out EvalCount: Integer): string;
var
  Client: TFPHTTPClient;
  Req, Opts: TJSONObject;
  ReqStream, RespStream: TStringStream;
  RespJSON: TJSONData;
  Node: TJSONData;
begin
  Result    := '';
  ErrMsg    := '';
  EvalCount := 0;

  Client     := TFPHTTPClient.Create(nil);
  Req        := TJSONObject.Create;
  ReqStream  := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  try
    Client.HTTPVersion    := '1.0';
    Client.ConnectTimeout := 5000;
    Client.IOTimeout      := 180000;
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('Connection', 'close');

    Opts := TJSONObject.Create;
    try
      Opts.Add('temperature', Temperature);
      Req.Add('model', Model);
      Req.Add('prompt', Prompt);
      Req.Add('stream', False);
      Req.Add('keep_alive', '30m');
      Req.Add('options', Opts);
      Opts := nil; // Req owns it now
    finally
      Opts.Free;
    end;

    ReqStream.WriteString(Req.AsJSON);
    ReqStream.Position := 0;

    try
      Client.RequestBody := ReqStream;
      Client.Post('http://localhost:11434/api/generate', RespStream);
    except
      on E: Exception do
      begin
        ErrMsg := 'Verbindungsfehler: ' + E.Message +
                  ' (HTTP ' + IntToStr(Client.ResponseStatusCode) + ')';
        Exit;
      end;
    end;

    if Client.ResponseStatusCode <> 200 then
    begin
      ErrMsg := 'HTTP ' + IntToStr(Client.ResponseStatusCode) + ': ' +
                Copy(RespStream.DataString, 1, 500);
      Exit;
    end;

    try
      RespJSON := GetJSON(RespStream.DataString);
    except
      on E: Exception do
      begin
        ErrMsg := 'JSON-Parse-Fehler: ' + E.Message +
                  ' – Roh: ' + Copy(RespStream.DataString, 1, 200);
        Exit;
      end;
    end;

    try
      if not (RespJSON is TJSONObject) then
      begin
        ErrMsg := 'Antwort ist kein JSON-Objekt.';
        Exit;
      end;

      Node := TJSONObject(RespJSON).Find('error');
      if Assigned(Node) then
      begin
        ErrMsg := 'Ollama-Fehler: ' + Node.AsString;
        Exit;
      end;

      Node := TJSONObject(RespJSON).Find('response');
      if Assigned(Node) then
        Result := Node.AsString;

      Node := TJSONObject(RespJSON).Find('eval_count');
      if Assigned(Node) then
        EvalCount := Node.AsInteger;
    finally
      RespJSON.Free;
    end;
  finally
    RespStream.Free;
    ReqStream.Free;
    Req.Free;
    Client.Free;
  end;
end;

end.
