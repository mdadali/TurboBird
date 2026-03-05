{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{************************************************************************}

unit IBDSDialogs;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Dialogs,
  Forms, StdCtrls, ExtCtrls, Buttons, IB, IBDialogs;

type
  { TIBDSLCLInterface }

  TIBDSLCLInterface = class(TIBLCLInterface)
  private
    function GetProjectName: string;
    procedure GetDatabaseName(DefaultDBName, DefaultUserName: string; var DBName: string;
      var UserName: string);
    procedure GetServerName(DefaultServerName, DefaultUserName: string;
              var ServerName, UserName: string);
    procedure SaveDatabaseParams(DatabaseName, UserName: string);
    procedure SaveServerParams(ServerName, UserName: string);
 public
    function ServerLoginDialog(var AServerName: string;
                               var AUserName, APassword: string): Boolean;  override;
    function LoginDialogEx(var ADatabaseName: string;
                               var AUserName, APassword: string;
                               NameReadOnly: Boolean): Boolean; override;
  end;

implementation

{$R *.lfm}

uses BaseIDEIntf, LazIDEIntf;

const
  ConfigFile = 'ibx.xml';

type
  { TIBXDSLoginDlg }

  TIBXDSLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    ProjectName: TLabel;
    SpeedButton1: TSpeedButton;
    TargetCaption: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
    DatabaseName: TEdit;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


{ TIBXDSLoginDlg }

procedure TIBXDSLoginDlg.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFileDir(DatabaseName.Text);
  if OpenDialog1.Execute then
    DatabaseName.Text := OpenDialog1.FileName;
end;


function TIBDSLCLInterface.GetProjectName: string;
begin
  Result := ChangeFileExt(ExtractFileName(LazarusIDE.ActiveProject.MainFile.FileName),'');
end;

procedure TIBDSLCLInterface.GetDatabaseName(DefaultDBName,
  DefaultUserName: string; var DBName: string; var UserName: string);
begin
  With GetIDEConfigStorage(ConfigFile,True) do
  try
    DBName := GetValue(GetProjectName + '/Database',DefaultDBName);
    UserName := GetValue(GetProjectName + '/UserName',DefaultUserName);
  finally
    Free
  end;
end;

procedure TIBDSLCLInterface.GetServerName(DefaultServerName,
  DefaultUserName: string; var ServerName, UserName: string);
begin
  With GetIDEConfigStorage(ConfigFile,True) do
  try
    ServerName := GetValue(GetProjectName + '/Server',DefaultServerName);
    UserName := GetValue(GetProjectName + '/UserName',DefaultUserName);
  finally
    Free
  end;
end;

procedure TIBDSLCLInterface.SaveDatabaseParams(DatabaseName, UserName: string);
var ProjectName: string;
begin
  ProjectName := StringReplace(GetProjectName,' ','_',[rfReplaceAll]);
  With GetIDEConfigStorage(ConfigFile,True) do
  try
    SetValue(ProjectName + '/Database',DatabaseName);
    SetValue(ProjectName + '/UserName',UserName);
    WriteToDisk;
  finally
    Free
  end;
end;

procedure TIBDSLCLInterface.SaveServerParams(ServerName, UserName: string);
begin
  With GetIDEConfigStorage(ConfigFile,True) do
  try
    SetValue(GetProjectName + '/Server',ServerName);
    SetValue(GetProjectName + '/UserName',UserName);
    WriteToDisk;
  finally
    Free
  end;
end;

function TIBDSLCLInterface.ServerLoginDialog(var AServerName: string;
  var AUserName, APassword: string): Boolean;
var ActiveForm: TCustomForm;
begin
  ActiveForm := Screen.ActiveCustomForm;
  with TIBXDSLoginDlg.Create(nil) do
  try
    Caption := 'Firebird Server Login';
    TargetCaption.Caption := 'Server Name: ';
    GetServerName(AServerName,AUserName,AServerName,AUserName);
    ProjectName.Caption := GetProjectName;
    DatabaseName.Text := AServerName;
    UserName.Text := AUserName;
    Result := False;
    if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AServerName :=  DatabaseName.Text;
      AUserName := UserName.Text;
      APassword := Password.Text;
      SaveServerParams(DatabaseName.Text,UserName.Text);
      Result := True;
    end;
  finally
    Free;
  end;
  if ActiveForm <> nil then
  begin
    ActiveForm.SetFocus;
    Application.ProcessMessages;
  end;
end;

function TIBDSLCLInterface.LoginDialogEx(var ADatabaseName: string;
  var AUserName, APassword: string; NameReadOnly: Boolean): Boolean;
var ActiveForm: TCustomForm;
begin
 try
  ActiveForm := Screen.ActiveCustomForm;
  with TIBXDSLoginDlg.Create(Application) do
  try
    ProjectName.Caption := GetProjectName;
    GetDatabaseName(ADatabaseName, AUserName, ADatabaseName,AUserName);
    DatabaseName.Text := ADatabaseName;
    UserName.Text := AUserName;
    Result := False;
    if NameReadOnly then
      UserName.Enabled := False
    else
      if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      ADatabaseName :=  DatabaseName.Text;
      AUserName := UserName.Text;
      APassword := Password.Text;
      SaveDatabaseParams(DatabaseName.Text,UserName.Text);
      Result := True;
    end
  finally
    Free;
  end;
  if ActiveForm <> nil then
  begin
    ActiveForm.SetFocus;
    Application.ProcessMessages;
  end;
 except On E:Exception do
   MessageDlg('Unable to Load Login Dialog ' + E.Message,mtError,[mbOK],0);
 end;
end;

end.
