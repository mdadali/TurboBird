{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{                                                                              }
{ Unit owner:    Olivier GUILBAUD <golivier@free.fr>                           }
{ Last modified: Jun 17, 2003                                                  }
{                                                                              }
{------------------------------------------------------------------------------}
{ Sample for use the components TUIBBakcup                                     }
{ Warning : you must use TcpIp protocol and not local protocol otherwise the   }
{           bacup service dont work properly (Linux, windows not tested)       }
{******************************************************************************}
unit frm_bakcupsample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, UIB,keyboard;

type

  { TfrmBakcup }

  TfrmBakcup = class(TForm)
    btnStart: TSpeedButton;
    btnOpenSourceDB: TButton;
    btnOpenBackupFile: TButton;
    chkExpand: TCheckBox;
    chkConvert: TCheckBox;
    chkTransportable: TCheckBox;
    chkMetadatadesc: TCheckBox;
    Label1: TLabel;
    odSourceDB: TOpenDialog;
    odDestBackFile: TOpenDialog;
    optGarb: TCheckBox;
    OptMetadataOnly: TCheckBox;
    OptLimbo: TCheckBox;
    optChksum: TCheckBox;
    edDatabase: TEdit;
    edDest: TEdit;
    Bak: TUIBBackup;
    Label2: TLabel;
    Label3: TLabel;
    moResult: TMemo;
    Panel1: TPanel;
    pnlBottom: TPanel;
    panOptions: TPanel;
    UIBRestore1: TUIBRestore;
    procedure BakVerbose(Sender: TObject; Message: string);
    procedure btnOpenBackupFileClick(Sender: TObject);
    procedure btnOpenSourceDBClick(Sender: TObject);
    procedure Form1Show(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure pnlBottomClick(Sender: TObject);
  private
    { private declarations }
    function GetOptionBak : TBackupOptions;
    
  public
    { public declarations }
  end; 

var
  frmBakcup: TfrmBakcup;

implementation

{ TfrmBakcup }

procedure TfrmBakcup.btnStartClick(Sender: TObject);
begin
  Try
    moResult.Lines.Clear;
    moResult.Repaint;

    Screen.Cursor:=crHourglass;
    btnStart.Enabled:=False;
    try
      Bak.Database := edDatabase.Text;
      Bak.BackupFiles.Text := edDest.Text;
      moResult.Lines.Clear;
      Bak.Options := self.GetOptionBak;
      Bak.Run;
    except
      On E:Exception do
        ShowMessage(E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
    btnStart.Enabled := True;
  end;
end;

procedure TfrmBakcup.pnlBottomClick(Sender: TObject);
begin

end;

//Get the options of backup
function TfrmBakcup.GetOptionBak: TBackupOptions;
Var i : Integer;
    c : TCheckBox;
begin
  FillChar(Result,SizeOf(Result),0);
  for i := 0 to panOptions.ControlCount-1 do
  begin
    If panOptions.Controls[i] is TCheckBox then
    begin
      C:=TCheckBox(panOptions.Controls[i]);
      If C.Checked then
        Include(Result,TBackupOption(C.Tag));
    end;
  end;
end;


//Write the log message into the moResult
procedure TfrmBakcup.BakVerbose(Sender: TObject; Message: string);
Var j : Integer;
begin
  j := moResult.VertScrollBar.Position;
  moResult.Append(Message);
  Inc(j);
  Application.ProcessMessages;
end;

procedure TfrmBakcup.btnOpenSourceDBClick(Sender: TObject);
begin
  if  odSourceDB.Execute then
    edDatabase.Text := odSourceDB.FileName;
end;

procedure TfrmBakcup.btnOpenBackupFileClick(Sender: TObject);
begin
  if  odDestBackFile.Execute then
    edDest.Text := odDestBackFile.FileName;
end;

procedure TfrmBakcup.Form1Show(Sender: TObject);
begin
  moResult.Clear;
end;

initialization
  {$I frm_bakcupsample.lrs}

end.

