unit UpdateChecker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, math,
  fphttpclient, LazFileUtils, Process, opensslsockets, jsonparser, fpjson,
  strutils, LResources, AbUnzper,
  turbocommon,
  uthemeselector;

type

  { TfrmUpdateChecker }

  TfrmUpdateChecker = class(TForm)
    AbUnZipper1: TAbUnZipper;
    btnCheck: TButton;
    btnDownload: TButton;
    chkBoxDeleteCurrentVersionOnNextStart: TCheckBox;
    chkBoxCloseCurrentInstance: TCheckBox;
    chkBoxAutoSearch: TCheckBox;
    chkBoxRunNewVersion: TCheckBox;
    chkBoxUnzip: TCheckBox;
    grBoxOptions: TGroupBox;
    grBoxSearch: TGroupBox;
    grBoxAfterDownload: TGroupBox;
    Label1: TLabel;
    lbCurrentVersion: TLabel;
    lblStatus: TLabel;
    pnlColor: TPanel;
    procedure chkBoxAutoSearchChange(Sender: TObject);
    procedure chkBoxCloseCurrentInstanceChange(Sender: TObject);
    procedure chkBoxRunNewVersionChange(Sender: TObject);
    procedure chkBoxUnzipChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDownloadURL: string;
    FFileName: string;
    FSilentMode: Boolean;  // true = im Hintergrund, keine Meldung wenn aktuell
    procedure SetStatus(const Msg: string);
    function GetCurrentVersionInfo(out BaseName, Version: string): Boolean;
    procedure UnzipDownloadedFile;
    function CheckForUpdate: Boolean;  // true wenn neue Version gefunden
  public
    function CompareVersions(const CurrentVer, NewVer: string): Integer;
    procedure PerformAutoSearch;
  end;

{$I version.inc}

var
  frmUpdateChecker: TfrmUpdateChecker;

implementation

procedure TfrmUpdateChecker.FormCreate(Sender: TObject);
begin
  lbCurrentVersion.Caption := VERSION;
  btnDownload.Enabled := False;
  SetStatus('Idle');
  FSilentMode := False;

  chkBoxAutoSearch.Checked    := AutoSearchOnProgramStart;
  chkBoxUnzip.Checked         := UnzipAfterDownload;
  chkBoxRunNewVersion.Checked := RunNewVersionAfterUnzip;
  chkBoxDeleteCurrentVersionOnNextStart.Checked := DeleteOldVersions;
end;

procedure TfrmUpdateChecker.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmUpdateChecker.chkBoxAutoSearchChange(Sender: TObject);
begin
  AutoSearchOnProgramStart := chkBoxAutoSearch.Checked;
end;

procedure TfrmUpdateChecker.chkBoxCloseCurrentInstanceChange(Sender: TObject);
begin
  DeleteOldVersions := chkBoxDeleteCurrentVersionOnNextStart.Checked;
end;

procedure TfrmUpdateChecker.chkBoxRunNewVersionChange(Sender: TObject);
begin
  RunNewVersionAfterUnzip := chkBoxRunNewVersion.Checked;
end;

procedure TfrmUpdateChecker.chkBoxUnzipChange(Sender: TObject);
begin
  UnzipAfterDownload := chkBoxUnzip.Checked;

  if not chkBoxUnzip.Checked then
  begin
    chkBoxRunNewVersion.Checked := false;
    chkBoxCloseCurrentInstance.Checked := false;
    chkBoxDeleteCurrentVersionOnNextStart.Checked := false;

    chkBoxRunNewVersion.Enabled := false;
    chkBoxCloseCurrentInstance.Enabled := false;
    chkBoxDeleteCurrentVersionOnNextStart.Enabled := false;
  end else
  begin
    chkBoxRunNewVersion.Enabled := true;
    chkBoxCloseCurrentInstance.Enabled := true;
    chkBoxDeleteCurrentVersionOnNextStart.Enabled := true;
  end;
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

  PosV := Pos('-v', LowerCase(NameOnly));
  if PosV = 0 then Exit;

  BaseName := Copy(NameOnly, 1, PosV - 1);
  Version := Copy(NameOnly, PosV + 2, MaxInt);

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

function TfrmUpdateChecker.CheckForUpdate: Boolean;
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
  Result := False;
  FDownloadURL := '';
  FFileName := '';
  btnDownload.Enabled := False;
  chkBoxUnzip.Enabled := False;

  if not FSilentMode then
  begin
    pnlColor.Color := clYellow;
    SetStatus('Checking for update...');
    Application.ProcessMessages;
  end;

  if not GetCurrentVersionInfo(BaseName, CurrentVer) then
  begin
    if not FSilentMode then
      SetStatus('Could not determine current version');
    Exit;
  end;

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('User-Agent', 'TurboBird-Updater');
    Client.AllowRedirect := True;
    Client.AddHeader('Accept', 'application/vnd.github+json');

    try
      Response := Client.Get('https://api.github.com/repos/mdadali/TurboBird/releases/latest');
    except
      on E: Exception do
      begin
        if not FSilentMode then
        begin
          SetStatus('Failed to connect to GitHub: ' + sLineBreak + E.Message);
          pnlColor.Color := clDefault;
          Application.ProcessMessages;
        end;
        Exit;
      end;
    end;

    Data := GetJSON(Response);
    try
      NewVer := Data.FindPath('tag_name').AsString;
      NewVer := StringReplace(NewVer, 'TurboBird_v', '', []);

      CompareResult := CompareVersions(CurrentVer, NewVer);
      if CompareResult >= 0 then
      begin
        if not FSilentMode then
        begin
          SetStatus('You have the latest version: ' + sLineBreak + CurrentVer);
          pnlColor.Color := clDefault;
          Application.ProcessMessages;
        end;
        Exit;
      end;

      // Neue Version gefunden
      Assets := TJSONArray(Data.FindPath('assets'));
      for I := 0 to Assets.Count - 1 do
      begin
        Asset := Assets[I];
        DownloadName := Asset.FindPath('name').AsString;

        if StartsText(BaseName, DownloadName) and EndsText('.zip', DownloadName) then
        begin
          AssetVer := ExtractVersionFromName(DownloadName);
          if CompareVersions(CurrentVer, AssetVer) < 0 then
          begin
            FDownloadURL := Asset.FindPath('browser_download_url').AsString;
            FFileName := DownloadName;
            btnDownload.Enabled := True;
            chkBoxUnzip.Enabled := True;
            Result := True;

            SetStatus('New version found:' + sLineBreak + AssetVer);
            pnlColor.Color := clGreen;
            Application.ProcessMessages;
            Exit;
          end;
        end;
      end;

      if not FSilentMode then
      begin
        SetStatus('No matching file found in release.');
        pnlColor.Color := clDefault;
        Application.ProcessMessages;
      end;

    finally
      Data.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TfrmUpdateChecker.PerformAutoSearch;
begin
  if not AutoSearchOnProgramStart then Exit;

  FSilentMode := True;
  try
    if CheckForUpdate then
    begin
      // Update gefunden → Formular sichtbar machen
      Show;
      BringToFront;
    end;
  finally
    FSilentMode := False;
  end;
end;

procedure TfrmUpdateChecker.btnCheckClick(Sender: TObject);
begin
  FSilentMode := False;
  CheckForUpdate;
end;

procedure TfrmUpdateChecker.UnzipDownloadedFile;
begin
  SetStatus('Extracting...');
  pnlColor.Color := clYellow;
  Application.ProcessMessages;

  AbUnZipper1.FileName := AppendPathDelim(ExtractFilePath(Application.ExeName)) + FFileName;
  AbUnZipper1.BaseDirectory := ExtractFilePath(Application.ExeName);
  AbUnZipper1.ExtractFiles('*.*');

  SetStatus('Extraction completed.');
  pnlColor.Color := clGreen;
  Application.ProcessMessages;
end;

{procedure TfrmUpdateChecker.btnDownloadClick(Sender: TObject);
var
  Client: TFPHTTPClient;
  SavePath, NewExePath: string;
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

    if chkBoxUnzip.Checked then
    begin
      UnzipDownloadedFile;

      // Neue Version starten wenn gewünscht
      if chkBoxRunNewVersion.Checked then
      begin
        // Pfad zur neuen EXE ermitteln
        NewExePath := ExtractFilePath(Application.ExeName) +
                      ChangeFileExt(FFileName, '');  // .zip entfernen

        if FileExists(NewExePath) then
        begin
          SetStatus('Starting new version...');
          Application.ProcessMessages;

          // Neue Version starten
          ExecuteProcess(NewExePath, '', []);

          // Aktuelle Instanz beenden wenn gewünscht
          if chkBoxCloseCurrentInstance.Checked then
            Application.Terminate;
        end
        else
        begin
          SetStatus('Error: new executable not found');
          pnlColor.Color := clRed;
        end;
      end;
    end;
  except
    on E: Exception do
      SetStatus('Download failed: ' + sLineBreak + E.Message);
  end;
  Client.Free;
end;}

procedure TfrmUpdateChecker.btnDownloadClick(Sender: TObject);
var
  Client: TFPHTTPClient;
  SavePath, NewExePath: string;
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

    if chkBoxUnzip.Checked then
    begin
      UnzipDownloadedFile;

      // Neue Version starten wenn gewünscht
      if chkBoxRunNewVersion.Checked then
      begin
        // Pfad zur neuen EXE ermitteln (.zip entfernen)
        NewExePath := ExtractFilePath(Application.ExeName) +
                      ChangeFileExt(FFileName, '');

        if FileExists(NewExePath) then
        begin
          SetStatus('Starting new version...');
          Application.ProcessMessages;

          // Unter Linux: ausführbar machen
          {$IFDEF LINUX}
          ExecuteProcess('/bin/chmod', '+x ' + NewExePath, []);
          {$ENDIF}

          // Neue Version mit TProcess starten
          with TProcess.Create(nil) do
          try
            Executable := NewExePath;
            CurrentDirectory := ExtractFilePath(NewExePath);
            Options := [poNoConsole];
            Execute;
          finally
            Free;
          end;

          // Aktuelle Instanz beenden wenn gewünscht
          if chkBoxCloseCurrentInstance.Checked then
          begin
            Sleep(500);
            Application.Terminate;
          end;
        end
        else
        begin
          SetStatus('Error: new executable not found');
          pnlColor.Color := clRed;
        end;
      end;
    end;
  except
    on E: Exception do
      SetStatus('Download failed: ' + sLineBreak + E.Message);
  end;
  Client.Free;
end;

procedure TfrmUpdateChecker.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

initialization
  {$I UpdateChecker.lrs}

end.
