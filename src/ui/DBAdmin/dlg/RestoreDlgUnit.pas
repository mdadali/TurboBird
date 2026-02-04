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
  StdCtrls, Buttons, ComCtrls, Grids, IBXServices,
  turbocommon;

type

  { TRestoreDlg }

  TRestoreDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cmbBoxServers: TComboBox;
    DeActivateIndexes: TCheckBox;
    IBXClientSideRestoreService1: TIBXClientSideRestoreService;
    IBXServerSideRestoreService1: TIBXServerSideRestoreService;
    IBXServicesConnection1: TIBXServicesConnection;
    Label7: TLabel;
    PageBuffers: TEdit;
    Label6: TLabel;
    NoShadow: TCheckBox;
    NoValidityCheck: TCheckBox;
    OneRelationAtATime: TCheckBox;
    ProgressBar1: TProgressBar;
    RestoreMetaDataOnly: TCheckBox;
    sbPrimDBFileName: TSpeedButton;
    sbSegments: TSpeedButton;
    StringGrid1: TStringGrid;
    UseAllSpace: TCheckBox;
    __DBName: TEdit;
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
    sbBackupFile: TSpeedButton;
    SelectTab: TTabSheet;
    ReportTab: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBXClientSideRestoreService1GetNextLine(Sender: TObject;
      var Line: string);
    procedure ReportTabShow(Sender: TObject);
    procedure sbBackupFileClick(Sender: TObject);
    procedure sbPrimDBFileNameClick(Sender: TObject);
    procedure sbSegmentsChangeBounds(Sender: TObject);
    procedure sbSegmentsClick(Sender: TObject);
  private
    { private declarations }
    FServerName,
    FDatabaseName: string;
    FDefaultPageSize,
    FDefaultNumBuffers: Integer;
    procedure DoClientRestore(Data: PtrInt);
    procedure DoServerRestore(Data: PtrInt);
  public
    { public declarations }
    procedure Init(AServerName, ADatabaseName: string; ADefaultPageSize, ADefaultNumBuffers: Integer);
    function ShowModal(DefaultPageSize, DefaultNumBuffers: integer): TModalResult;
 end;

var
  RestoreDlg: TRestoreDlg;

implementation

{$R *.lfm}

{ TRestoreDlg }

procedure TRestoreDlg.sbBackupFileClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Backup files (*.fbk; *.gbk)|*.fbk;*.gbk|All files (*.*)|*.*';
  if OpenDialog1.Execute then
  begin
    SourceArchive.Text := OpenDialog1.Filename;
    if Pos('RestoredDB_at_', __DBName.Text) > 0 then
      __DBName.Text := ChangeFileExt(SourceArchive.Text, '.fdb');
  end;
end;

procedure TRestoreDlg.sbPrimDBFileNameClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Database Files|*.fdb|All Files|*.*';
  if OpenDialog1.Execute then
  begin
    __DBName.Text := OpenDialog1.Filename;
    StringGrid1.RowCount := StringGrid1.RowCount + 1;
    StringGrid1.Cells[0, StringGrid1.RowCount - 1] := __DBName.Text;
  end;
end;

procedure TRestoreDlg.sbSegmentsChangeBounds(Sender: TObject);
begin
end;

procedure TRestoreDlg.sbSegmentsClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Database Files|*.fdb|All Files|*.*';
  if OpenDialog1.Execute then
  begin
    StringGrid1.RowCount := StringGrid1.RowCount + 1;
    //StringGrid1.Cells[0, StringGrid1.RowCount - 1] := OpenDialog1.Filename + ' 50 MB';
    StringGrid1.Cells[0, StringGrid1.RowCount - 1] := OpenDialog1.Filename;
    StringGrid1.Cells[1, StringGrid1.RowCount - 1] :=  PageSize.Text;
  end;
end;

procedure TRestoreDlg.Init(AServerName, ADatabaseName: string; ADefaultPageSize, ADefaultNumBuffers: Integer);
var ServerList: TStringList;
    ServerRec: TServerRecord;
begin
  FServerName        := AServerName;
  FDatabaseName      := ADatabaseName;
  FDefaultPageSize   := ADefaultPageSize;
  FDefaultNumBuffers := ADefaultNumBuffers;

  ServerList := GetServerListFromTreeView;
  cmbBoxServers.Items.Assign(ServerList);

  if cmbBoxServers.Items.Count > 0 then
    cmbBoxServers.ItemIndex := cmbBoxServers.Items.IndexOf(FServerName);

  __DBName.Text      := FDatabaseName;
  PageSize.Text    := IntToStr(FDefaultPageSize);
  PageBuffers.Text := IntToStr(FDefaultNumBuffers);


  ServerRec := GetServerRecordFromFileByName(FServerName);

  if IBXServicesConnection1.Connected then
    IBXServicesConnection1.Connected := False;

  IBXServicesConnection1.ServerName := ServerRec.ServerName;
  IBXServicesConnection1.PortNo := ServerRec.Port;
  IBXServicesConnection1.Protocol := ServerRec.Protocol;

  IBXServicesConnection1.Params.Clear;
  IBXServicesConnection1.Params.Add('user_name=' + ServerRec.UserName);
  IBXServicesConnection1.Params.Add('password=' + ServerRec.Password);
  IBXServicesConnection1.LoginPrompt := False;
  IBXServicesConnection1.Connected := True;

  ServerList.Free;
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
  //ServerName.Text := IBXClientSideRestoreService1.ServicesConnection.ServerName;
  //__DBName.Text := IBXClientSideRestoreService1.DatabaseFiles[0];
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
var i: integer;
begin
  if ModalResult <> mrOK then Exit;

  if PageControl1.ActivePage = SelectTab then
  begin
    CloseAction := caNone;
    if SourceArchive.Text = '' then
      raise Exception.Create('A Backup File Name must be given');
      if ServerSideBtn.Checked then
      begin
        IBXServerSideRestoreService1.DatabaseFiles.Clear;
        //IBXServerSideRestoreService1.DatabaseFiles.Add(__DBName.Text);
        for i := 0 to StringGrid1.RowCount - 1 do
        begin
          if Trim(StringGrid1.Cells[0, i]) <> '' then
          begin
            IBXServerSideRestoreService1.DatabaseFiles.Add(StringGrid1.Cells[0, i]);

            if i <  StringGrid1.RowCount - 1 then
              IBXServerSideRestoreService1.DatabaseFiles.Add(StringGrid1.Cells[1, i]);

          end;
        end;
        Application.QueueAsyncCall(@DoServerRestore,0)
      end else
      begin
        IBXClientSideRestoreService1.DatabaseFiles.Clear;

        //IBXClientSideRestoreService1.DatabaseFiles.Add(__DBName.Text);
        //IBXClientSideRestoreService1.DatabaseFiles.Add('20000');

        if StringGrid1.RowCount > 0 then
          for i := 0 to StringGrid1.RowCount - 1 do
          begin

            if Trim(StringGrid1.Cells[0, i]) <> '' then
            begin
              IBXClientSideRestoreService1.DatabaseFiles.Add(StringGrid1.Cells[0, i]);

              if i <  StringGrid1.RowCount - 1 then
                IBXClientSideRestoreService1.DatabaseFiles.Add(StringGrid1.Cells[1, i]);
            end;

          end;


        Application.QueueAsyncCall(@DoClientRestore,0);
      end;
    PageControl1.ActivePage := ReportTab;
  end;

end;

procedure TRestoreDlg.Button1Click(Sender: TObject);
begin

end;


end.

