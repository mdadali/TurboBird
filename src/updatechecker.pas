unit UpdateChecker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, math,
  fphttpclient, LazFileUtils, Process, opensslsockets, jsonparser, fpjson,
  strutils, LResources,
  turbocommon;

type

  { TfrmUpdateChecker }

  TfrmUpdateChecker = class(TForm)
    btnCheck: TButton;
    btnDownload: TButton;
    Label1: TLabel;
    lbCurrentVersion: TLabel;
    lblStatus: TLabel;
    pnlColor: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
  private
    FDownloadURL: string;
    FFileName: string;
    procedure SetStatus(const Msg: string);
    function GetCurrentVersionInfo(out BaseName, Version: string): Boolean;
    function CompareVersions(const CurrentVer, NewVer: string): Integer;
  public
  end;

implementation

procedure TfrmUpdateChecker.FormCreate(Sender: TObject);
begin
  lbCurrentVersion.Caption := ExtractVersionFromName(Application.ExeName);
  btnDownload.Enabled := False;
  SetStatus('Idle');
end;

procedure TfrmUpdateChecker.SetStatus(const Msg: string);
begin
  lblStatus.Caption := Msg;
  Application.ProcessMessages;
end;

function TfrmUpdateChecker.GetCurrentVersionInfo(out BaseName, Version: string): Boolean;
var
  FullName, NameOnly: string;
  PosV: Integer;
begin
  Result := False;
  FullName := ExtractFileName(Application.ExeName);
  NameOnly := FullName;

  // Suche nach '-v' oder '-V' case-insensitive
  PosV := Pos('-v', LowerCase(NameOnly));
  if PosV = 0 then Exit;

  BaseName := Copy(NameOnly, 1, PosV - 1); // alles vor '-v'
  Version := Copy(NameOnly, PosV + 2, MaxInt); // alles nach '-v'

  Result := Version <> '';
end;

function TfrmUpdateChecker.CompareVersions(const CurrentVer, NewVer: string): Integer;
var
  CurrParts, NewParts: TStringArray;
  I, MaxLen, C, N: Integer;
begin
  CurrParts := SplitString(CurrentVer, '.');
  NewParts := SplitString(NewVer, '.');
  MaxLen := Max(Length(CurrParts), Length(NewParts));

  for I := 0 to MaxLen - 1 do
  begin
    if I < Length(CurrParts) then
      C := StrToIntDef(CurrParts[I], 0)
    else
      C := 0;

    if I < Length(NewParts) then
      N := StrToIntDef(NewParts[I], 0)
    else
      N := 0;

    if C > N then
      Exit(1)
    else if C < N then
      Exit(-1);
  end;

  Result := 0;
end;

procedure TfrmUpdateChecker.btnCheckClick(Sender: TObject);
var
  Client: TFPHTTPClient;
  Response: string;
  Data: TJSONData;
  Assets: TJSONArray;
  I: Integer;
  DownloadName, NewVer, BaseName, CurrentVer, AssetVer: string;
  Asset: TJSONData;
  CompareResult: integer;
begin
  pnlColor.Color := clYellow;
  SetStatus('Checking for update...');
  Application.ProcessMessages;
  btnDownload.Enabled := False;

  if not GetCurrentVersionInfo(BaseName, CurrentVer) then
  begin
    SetStatus('Could not determine current version');
    Exit;
  end;

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('User-Agent', 'TurboBird-Updater');
    Client.AllowRedirect := True;
    Client.AddHeader('Accept', 'application/vnd.github+json');
    Response := Client.Get('https://api.github.com/repos/mdadali/TurboBird/releases/latest');
    Data := GetJSON(Response);
    try
      NewVer := Data.FindPath('tag_name').AsString;
      NewVer := StringReplace(NewVer, 'TurboBird_v', '', []);

      CompareResult := CompareVersions(CurrentVer, NewVer);
      if  CompareResult < 0 then
      begin
        SetStatus('New version found!');
        pnlColor.Color := clGreen;
        Application.ProcessMessages;
      end else
      begin
        SetStatus('You have the latest version: ' + sLineBreak + CurrentVer);
        pnlColor.Color := clDefault;
        Application.ProcessMessages;
        Exit;
      end;

      Assets := TJSONArray(Data.FindPath('assets'));
      for I := 0 to Assets.Count - 1 do
      begin
        Asset := Assets[I];
        DownloadName := Asset.FindPath('name').AsString;

        // Unterstütze jetzt .zip statt .gz, unterscheide Windows und Linux
        if StartsText(BaseName, DownloadName) and EndsText('.zip', DownloadName) then
        begin
          AssetVer := ExtractVersionFromName(DownloadName);
          if CompareVersions(CurrentVer, AssetVer) < 0 then
          begin
            FDownloadURL := Asset.FindPath('browser_download_url').AsString;
            FFileName := DownloadName;
            btnDownload.Enabled := True;
            SetStatus('New version found:' + sLineBreak + AssetVer);
            pnlColor.Color := clGreen;
            Application.ProcessMessages;
            pnlColor.Color := clGreen;
            Exit;
          end;
        end;
      end;

      SetStatus('No matching file found in release.');
      pnlColor.Color := clDefault;
      Application.ProcessMessages;

    finally
      Data.Free;
    end;
  except
    on E: Exception do
    begin
      SetStatus('Failed to connect to GitHub: ' + sLineBreak + E.Message);
      pnlColor.Color := clDefault;
      Application.ProcessMessages;
    end;
  end;
  Client.Free;
end;

procedure TfrmUpdateChecker.btnDownloadClick(Sender: TObject);
var
  Client: TFPHTTPClient;
  SavePath: string;
begin
  if FDownloadURL = '' then Exit;
  SetStatus('Downloading ' + sLineBreak + FFileName + '...');
  pnlColor.Color := clRed;
  Application.ProcessMessages;

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('User-Agent', 'TurboBird-Updater');
    Client.AllowRedirect := True;
    SavePath := AppendPathDelim(ExtractFilePath(Application.ExeName)) + FFileName;
    Client.Get(FDownloadURL, SavePath);
    SetStatus('Download completed: ' + sLineBreak + FFileName);
    pnlColor.Color := clDefault;
  except
    on E: Exception do
      SetStatus('Download failed: ' + sLineBreak + E.Message);
  end;
  Client.Free;
end;

initialization
  {$I UpdateChecker.lrs}

end.

