unit updatechecker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, httpsend,
  StrUtils, math, fpjson, jsonparser, ExtCtrls;


type

  { TfrmUpdateChecker }

  TfrmUpdateChecker = class(TForm)
    btnCheck: TButton;
    btnDownload: TButton;
    Label1: TLabel;
    lblStatus: TLabel;
    lblCurrentVersion: TLabel;
    Timer1: TTimer;
    procedure btnCheckClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLocalVersionStr: string;
    FRemoteFileName: string;
    FRemoteVersionStr: string;
    procedure ParseVersionFromFileName(const FileName: string; out Prefix, Version: string);
    function IsNewerVersion(const NewVersion, OldVersion: string): Boolean;
    procedure DownloadFile(const URL, TargetPath: string);
  public

  end;


implementation

{$R *.lfm}

{ TfrmUpdateChecker }

procedure TfrmUpdateChecker.btnCheckClick(Sender: TObject);
var
  URL, JSONText, Prefix, LocalVersion, RemoteVersion, FileName: string;
  HTTP: THTTPSend;
  JSON: TJSONData;
  i: Integer;
begin
  lblStatus.Caption := 'Checking GitHub for updates...';
  Application.ProcessMessages;

  ParseVersionFromFileName(ExtractFileName(ParamStr(0)), Prefix, LocalVersion);
  FLocalVersionStr := LocalVersion;

  HTTP := THTTPSend.Create;
  try
    HTTP.HTTPMethod('GET', 'https://api.github.com/repos/mdadali/TurboBird/releases/latest');
    if HTTP.ResultCode = 200 then
    begin
      SetLength(JSONText, HTTP.Document.Size);
      HTTP.Document.Read(Pointer(JSONText)^, Length(JSONText));
      JSON := GetJSON(JSONText);
      try
        for i := 0 to JSON.FindPath('assets').Count - 1 do
        begin
          FileName := JSON.FindPath('assets[' + IntToStr(i) + '].name').AsString;
          if AnsiStartsStr(Prefix, FileName) then
          begin
            ParseVersionFromFileName(FileName, Prefix, RemoteVersion);
            if IsNewerVersion(RemoteVersion, LocalVersion) then
            begin
              FRemoteFileName := FileName;
              FRemoteVersionStr := RemoteVersion;
              lblStatus.Caption := 'New version found: ' + RemoteVersion;
              btnDownload.Enabled := True;
              Exit;
            end;
          end;
        end;
        lblStatus.Caption := 'You have the latest version.';
      finally
        JSON.Free;
      end;
    end
    else
      lblStatus.Caption := 'Failed to connect to GitHub.';
  finally
    HTTP.Free;
  end;
end;

procedure TfrmUpdateChecker.ParseVersionFromFileName(const FileName: string; out Prefix, Version: string);
var
  S: string;
  p: Integer;
begin
  S := FileName;
  if RightStr(S, 3) = '.gz' then
    S := LeftStr(S, Length(S) - 3);
  p := RPos('-v', S);
  if p > 0 then
  begin
    Prefix := Copy(S, 1, p - 1);
    Version := Copy(S, p + 2, MaxInt);
  end
  else
  begin
    Prefix := S;
    Version := '';
  end;
end;

procedure TfrmUpdateChecker.btnDownloadClick(Sender: TObject);
var
  URL, TargetPath: string;
begin
  if FRemoteFileName = '' then Exit;
  URL := Format('https://github.com/mdadali/TurboBird/releases/download/TurboBird_v%s/%s',
                [FRemoteVersionStr, FRemoteFileName]);
  TargetPath := ExtractFilePath(ParamStr(0)) + FRemoteFileName;
  lblStatus.Caption := 'Downloading...';
  Application.ProcessMessages;
  try
    DownloadFile(URL, TargetPath);
    lblStatus.Caption := 'Download completed: ' + FRemoteFileName;
  except
    on E: Exception do
      lblStatus.Caption := 'Download failed: ' + E.Message;
  end;
end;

procedure TfrmUpdateChecker.FormCreate(Sender: TObject);
begin
  Caption := 'Update Checker';
  Width := 400;
  Height := 180;

  lblCurrentVersion := TLabel.Create(Self);
  lblCurrentVersion.Parent := Self;
  lblCurrentVersion.Caption := 'Current Version: ?';
  lblCurrentVersion.Top := 16;
  lblCurrentVersion.Left := 16;

  lblStatus := TLabel.Create(Self);
  lblStatus.Parent := Self;
  lblStatus.Caption := 'Status: Ready';
  lblStatus.Top := 44;
  lblStatus.Left := 16;

  btnCheck := TButton.Create(Self);
  btnCheck.Parent := Self;
  btnCheck.Caption := 'Check for Update';
  btnCheck.Top := 80;
  btnCheck.Left := 16;
  btnCheck.OnClick := @btnCheckClick;

  btnDownload := TButton.Create(Self);
  btnDownload.Parent := Self;
  btnDownload.Caption := 'Download';
  btnDownload.Top := 80;
  btnDownload.Left := 160;
  btnDownload.Enabled := False;
  btnDownload.OnClick := @btnDownloadClick;

  // Extract version info from exe name
  ParseVersionFromFileName(ExtractFileName(ParamStr(0)), FLocalVersionStr, FLocalVersionStr);
  lblCurrentVersion.Caption := 'Current Version: ' + FLocalVersionStr;
end;

function TfrmUpdateChecker.IsNewerVersion(const NewVersion, OldVersion: string): Boolean;
var
  n, o: TStringArray;
  i: Integer;
begin
  n := NewVersion.Split('.');
  o := OldVersion.Split('.');
  for i := 0 to Min(High(n), High(o)) do
  begin
    if StrToIntDef(n[i], 0) > StrToIntDef(o[i], 0) then Exit(True)
    else if StrToIntDef(n[i], 0) < StrToIntDef(o[i], 0) then Exit(False);
  end;
  Result := Length(n) > Length(o);
end;

procedure TfrmUpdateChecker.DownloadFile(const URL, TargetPath: string);
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    if HTTP.HTTPMethod('GET', URL) then
      HTTP.Document.SaveToFile(TargetPath)
    else
      raise Exception.Create('HTTP Error: ' + IntToStr(HTTP.ResultCode));
  finally
    HTTP.Free;
  end;
end;


end.
