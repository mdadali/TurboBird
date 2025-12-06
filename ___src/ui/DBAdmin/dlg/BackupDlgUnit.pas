(*
 * BackupDlgUnit.pas
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
unit BackupDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, IBXServices;

type

  { TBackupDlg }

  TBackupDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    IBXClientSideBackupService1: TIBXClientSideBackupService;
    IBXServerSideBackupService1: TIBXServerSideBackupService;
    IBXServicesConnection1: TIBXServicesConnection;
    NoDBTriggers: TCheckBox;
    NoGarbageCollection: TCheckBox;
    MetadataOnly: TCheckBox;
    IgnoreLimboTransactions: TCheckBox;
    IgnoreChecksums: TCheckBox;
    ProgressBar1: TProgressBar;
    ServerName: TEdit;
    DBName: TEdit;
    BackupFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Report: TMemo;
    PageControl1: TPageControl;
    ServerSideBtn: TRadioButton;
    ClientSideBtn: TRadioButton;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SelectTab: TTabSheet;
    ReportTab: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBXClientSideBackupService1GetNextLine(Sender: TObject;
      var Line: string);
    procedure ReportTabShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    procedure DoClientBackup(Data: PtrInt);
    procedure DoServerBackup(Data: PtrInt);
  public
    { public declarations }
 end;

var
  BackupDlg: TBackupDlg;

implementation

{$R *.lfm}

{ TBackupDlg }

procedure TBackupDlg.SpeedButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    BackupFileName.Text := SaveDialog1.Filename;
end;

procedure TBackupDlg.DoClientBackup(Data: PtrInt);
var BackupCount: integer;
begin
  ProgressBar1.Visible := true;
  with IBXClientSideBackupService1 do
  begin
    Options := [];
    if IgnoreChecksums.Checked then
      Options := Options + [IBXServices.IgnoreChecksums];
    if IgnoreLimboTransactions.Checked then
      Options := Options + [IgnoreLimbo];
    if MetadataOnly.Checked then
      Options := Options + [IBXServices.MetadataOnly];
    if NoGarbageCollection.Checked then
      Options := Options + [IBXServices.NoGarbageCollection];
    if NoDBTriggers.Checked then
      Options := Options + [IBXServices.NoDBTriggers];

    Report.Lines.Add('Starting Backup');
    BackupToFile(BackupFileName.Text, BackupCount);
  end;
  Report.Lines.Add(Format('Backup Completed - File Size = %d bytes',[BackupCount]));
  ProgressBar1.Visible := false;
  MessageDlg(Format('Backup Completed - File Size = %d bytes',[BackupCount]),mtInformation,[mbOK],0);
end;

procedure TBackupDlg.DoServerBackup(Data: PtrInt);
begin
  ProgressBar1.Visible := true;
  with IBXServerSideBackupService1 do
  begin
    BackupFiles.Clear;
    BackupFiles.Add(BackupFileName.Text);
    Options := [];
    if IgnoreChecksums.Checked then
      Options := Options + [IBXServices.IgnoreChecksums];
    if IgnoreLimboTransactions.Checked then
      Options := Options + [IgnoreLimbo];
    if MetadataOnly.Checked then
      Options := Options + [IBXServices.MetadataOnly];
    if NoGarbageCollection.Checked then
      Options := Options + [IBXServices.NoGarbageCollection];
    if NoDBTriggers.Checked then
      Options := Options + [IBXServices.NoDBTriggers];
    Report.Lines.Add('Starting Backup');
    Execute(Report.Lines);
    Report.Lines.Add('Backup Completed');
    ProgressBar1.Visible := false;
    MessageDlg('Backup Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TBackupDlg.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := SelectTab;
  ServerName.Text := IBXClientSideBackupService1.ServicesConnection.ServerName;
  DBName.Text := IBXClientSideBackupService1.DatabaseName;
  //BackupFileName.Text := '';
  Caption := 'Backup Database: ' + IBXClientSideBackupService1.DatabaseName;
end;

procedure TBackupDlg.IBXClientSideBackupService1GetNextLine(Sender: TObject;
  var Line: string);
begin
  Application.ProcessMessages;
end;

procedure TBackupDlg.ReportTabShow(Sender: TObject);
begin
  Report.Lines.Clear;
end;

procedure TBackupDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;

  if PageControl1.ActivePage = SelectTab then
  begin
    CloseAction := caNone;
    if BackupFileName.Text = '' then
      raise Exception.Create('A Backup File Name must be given');
    PageControl1.ActivePage := ReportTab;
    Application.ProcessMessages;
    if ServerSideBtn.Checked then
      Application.QueueAsyncCall(@DoServerBackup,0)
    else
      Application.QueueAsyncCall(@DoClientBackup,0);
  end;
end;

end.

