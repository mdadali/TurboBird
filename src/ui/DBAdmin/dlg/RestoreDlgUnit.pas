(*
 * RestoreDlgUnit.pas
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
unit RestoreDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, IBXServices;

type

  { TRestoreDlg }

  TRestoreDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DeActivateIndexes: TCheckBox;
    IBXClientSideRestoreService1: TIBXClientSideRestoreService;
    IBXServerSideRestoreService1: TIBXServerSideRestoreService;
    PageBuffers: TEdit;
    Label6: TLabel;
    NoShadow: TCheckBox;
    NoValidityCheck: TCheckBox;
    OneRelationAtATime: TCheckBox;
    ProgressBar1: TProgressBar;
    RestoreMetaDataOnly: TCheckBox;
    UseAllSpace: TCheckBox;
    ServerName: TEdit;
    DBName: TEdit;
    SourceArchive: TEdit;
    PageSize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Report: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    ServerSideBtn: TRadioButton;
    ClientSideBtn: TRadioButton;
    SpeedButton1: TSpeedButton;
    SelectTab: TTabSheet;
    ReportTab: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBXClientSideRestoreService1GetNextLine(Sender: TObject;
      var Line: string);
    procedure ReportTabShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    procedure DoClientRestore(Data: PtrInt);
    procedure DoServerRestore(Data: PtrInt);
  public
    { public declarations }
     function ShowModal(DefaultPageSize, DefaultNumBuffers: integer): TModalResult;
 end;

var
  RestoreDlg: TRestoreDlg;

implementation

{$R *.lfm}

{ TRestoreDlg }

procedure TRestoreDlg.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    SourceArchive.Text := OpenDialog1.Filename;
end;

procedure TRestoreDlg.DoClientRestore(Data: PtrInt);
begin
  Progressbar1.Visible := true;
  with IBXClientSideRestoreService1 do
  begin
    PageSize := StrToInt(self.PageSize.Text);
    PageBuffers := StrToInt(self.PageBuffers.Text);
    Options := [Replace];
    if DeactivateIndexes.Checked then
      Options := Options + [IBXServices.DeactivateIndexes];
    if NoShadow.Checked then
      Options := Options + [IBXServices.NoShadow];
    if NoValidityCheck.Checked then
      Options := Options + [IBXServices.NoValidityCheck];
    if OneRelationAtATime.Checked then
      Options := Options + [IBXServices.OneRelationAtATime];
    if RestoreMetaDataOnly.Checked then
      Options := Options + [IBXServices.RestoreMetaDataOnly];
    if UseAllSpace.Checked then
      Options := Options + [IBXServices.UseAllSpace];
    Report.Lines.Add('Restore Started');
    RestoreFromFile(SourceArchive.Text,Report.Lines);
    Report.Lines.Add('Restore Completed');
    Progressbar1.Visible := false;
    MessageDlg('Restore Completed',mtInformation,[mbOK],0);
  end;
end;

procedure TRestoreDlg.DoServerRestore(Data: PtrInt);
begin
  Progressbar1.Visible := true;
  with IBXServerSideRestoreService1 do
  begin
    PageSize := StrToInt(self.PageSize.Text);
    PageBuffers := StrToInt(self.PageBuffers.Text);
    Options := [Replace];
    if DeactivateIndexes.Checked then
      Options := Options + [IBXServices.DeactivateIndexes];
    if NoShadow.Checked then
      Options := Options + [IBXServices.NoShadow];
    if NoValidityCheck.Checked then
      Options := Options + [IBXServices.NoValidityCheck];
    if OneRelationAtATime.Checked then
      Options := Options + [IBXServices.OneRelationAtATime];
    if RestoreMetaDataOnly.Checked then
      Options := Options + [IBXServices.RestoreMetaDataOnly];
    if UseAllSpace.Checked then
      Options := Options + [IBXServices.UseAllSpace];
    BackupFiles.Clear;
    BackupFiles.Add(SourceArchive.Text);
    Report.Lines.Add('Restore Started');
    Execute(Report.Lines);
    Report.Lines.Add('Restore Completed');
    Progressbar1.Visible := false;
    MessageDlg('Restore Completed',mtInformation,[mbOK],0);
  end;
end;

function TRestoreDlg.ShowModal(DefaultPageSize,
  DefaultNumBuffers: integer): TModalResult;
begin
  PageSize.Text := IntToStr(DefaultPageSize);
  PageBuffers.Text := IntToStr(DefaultNumBuffers);
  Result := inherited ShowModal;
end;

procedure TRestoreDlg.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := SelectTab;
  ServerName.Text := IBXClientSideRestoreService1.ServicesConnection.ServerName;
  DBName.Text := IBXClientSideRestoreService1.DatabaseFiles[0];
  SourceArchive.SetFocus;
end;

procedure TRestoreDlg.IBXClientSideRestoreService1GetNextLine(Sender: TObject;
  var Line: string);
begin
  Report.VertScrollBar.Position := (Report.Lines.Count-1)*20;
  Application.ProcessMessages;
end;

procedure TRestoreDlg.ReportTabShow(Sender: TObject);
begin
  Report.Lines.Clear;
end;

procedure TRestoreDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;

  if PageControl1.ActivePage = SelectTab then
  begin
    CloseAction := caNone;
    if SourceArchive.Text = '' then
      raise Exception.Create('A Backup File Name must be given');
      if ServerSideBtn.Checked then
        Application.QueueAsyncCall(@DoServerRestore,0)
      else
        Application.QueueAsyncCall(@DoClientRestore,0);
    PageControl1.ActivePage := ReportTab;
  end;

end;

end.

