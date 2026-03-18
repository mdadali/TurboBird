(*
 * ExecuteSQLScriptDlgUnit.pas
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
unit ExecuteSQLScriptDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtDlgs, ibxscript, IBDatabase, IB, DB;

type

  { TExecuteSQLScriptDlg }

  TExecuteSQLScriptDlg = class(TForm)
    IBTransaction1: TIBTransaction;
    OpenPictureDialog1: TOpenPictureDialog;
    ShowAffectedRows: TCheckBox;
    OpenDialog1: TOpenDialog;
    SourceFileName: TEdit;
    Button1: TButton;
    Button2: TButton;
    AutoDDL: TCheckBox;
    IgnoreGrants: TCheckBox;
    ShowPerformanceStats: TCheckBox;
    StopOnFirstError: TCheckBox;
    DBName: TEdit;
    IBXScript1: TIBXScript;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    Report: TMemo;
    ReportTab: TTabSheet;
    SelectTab: TTabSheet;
    ServerName: TEdit;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBXScript1ErrorLog(Sender: TObject; Msg: string);
    procedure IBXScript1GetParamValue(Sender: TObject; ParamName: string;
      var aBlobID: TISC_QUAD);
    procedure IBXScript1SelectSQL(Sender: TObject; SQLText: string);
    procedure SpeedButton1Click(Sender: TObject);
  private
    procedure DoExecScript(Data: PtrInt);
  public

  end;

var
  ExecuteSQLScriptDlg: TExecuteSQLScriptDlg;

implementation

uses SelectSQLResultsUnit, DataModule, IBBlob;

{$R *.lfm}

{ TExecuteSQLScriptDlg }

procedure TExecuteSQLScriptDlg.IBXScript1SelectSQL(Sender: TObject;
  SQLText: string);
begin
  with TSelectSQLResults.Create(Application) do
    Show(IBXScript1,SQLText,IBXScript1.ShowPerformanceStats);
end;

procedure TExecuteSQLScriptDlg.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog1.FileName := SourceFileName.Text;
  if OpenDialog1.Execute then
    SourceFileName.Text := OpenDialog1.FileName;
end;

procedure TExecuteSQLScriptDlg.DoExecScript(Data: PtrInt);
begin
  IBXScript1.AutoDDL := AutoDDl.Checked;
  IBXScript1.StopOnFirstError := StopOnFirstError.Checked;
  IBXScript1.ShowPerformanceStats := ShowPerformanceStats.Checked;
  IBXScript1.ShowAffectedRows := ShowAffectedRows.Checked;
  IBXScript1.IgnoreGrants := IgnoreGrants.Checked;
  IBXScript1.RunScript(SourceFileName.Text);
end;

procedure TExecuteSQLScriptDlg.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := SelectTab;
  ServerName.Text := DBDataModule.ServerName;
  DBName.Text := DBDataModule.DatabaseName;
  SourceFileName.Text := '';
  Report.Lines.Clear;
end;

procedure TExecuteSQLScriptDlg.IBXScript1ErrorLog(Sender: TObject; Msg: string);
begin
  Report.Lines.Add(Msg);
  Report.VertScrollBar.Position:=10000;
  Application.ProcessMessages;
end;

procedure TExecuteSQLScriptDlg.IBXScript1GetParamValue(Sender: TObject;
  ParamName: string; var aBlobID: TISC_QUAD);
begin
  OpenPictureDialog1.Title := 'Select Source File for ' + ParamName;
  if OpenPictureDialog1.Execute then
    with TIBBlobStream.Create do
    try
      Database := IBXScript1.Database;
      Transaction := IBTransaction1;
      Mode := bmWrite;
      LoadFromFile(OpenPictureDialog1.FileName);
      Finalize;
      aBlobID := BlobID;
    finally
      Free
    end;
end;

procedure TExecuteSQLScriptDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then Exit;

  if PageControl1.ActivePage = SelectTab then
  begin
    CloseAction := caNone;
    if SourceFileName.Text = '' then
      raise Exception.Create('A Source File Name must be given');
    PageControl1.ActivePage := ReportTab;
    Application.ProcessMessages;
    Application.QueueAsyncCall(@DoExecScript,0)
  end;
end;

end.

