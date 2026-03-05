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

unit IBDatabaseEdit;

{$MODE Delphi}

{$A+}                           (* Aligned records: On *)
{$B-}                           (* Short circuit boolean expressions: Off *)
{ $G+}                           (* Imported data: On *)
{$H+}                           (* Huge Strings: On *)
{$J-}                           (* Modification of Typed Constants: Off *)
{$M+}                           (* Generate run-time type information: On *)
{$O+}                           (* Optimization: On *)
{$Q-}                           (* Overflow checks: Off *)
{$R-}                           (* Range checks: Off *)
{$T+}                           (* Typed address: On *)
{ $U+}                           (* Pentim-safe FDIVs: On *)
{$W-}                           (* Always generate stack frames: Off *)
{$X+}                           (* Extended syntax: On *)
{$Z1}                           (* Minimum Enumeration Size: 1 Byte *)

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,  IBDatabase, IB,  LResources;

type

  { TIBDatabaseEditForm }

  TIBDatabaseEditForm = class(TForm)
    Browse: TButton;
    UseWireCompression: TCheckBox;
    DatabasePath: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ConfigOverrides: TMemo;
    PortNo: TEdit;
    Protocol: TComboBox;
    ConnectionTypeBtn: TRadioGroup;
    ServerName: TEdit;
    UseSystemDefaultCS: TCheckBox;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    UserName: TEdit;
    Password: TEdit;
    SQLRole: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DatabaseParams: TMemo;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label5: TLabel;
    LoginPrompt: TCheckBox;
    Label6: TLabel;
    CharacterSet: TComboBox;
    Test: TButton;
    procedure BrowseClick(Sender: TObject);
    procedure ConfigOverridesEditingDone(Sender: TObject);
    procedure DatabaseParamsEditingDone(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure PasswordEditingDone(Sender: TObject);
    procedure ProtocolCloseUp(Sender: TObject);
    procedure ConnectionTypeBtnSelectionChanged(Sender: TObject);
    procedure SQLRoleEditingDone(Sender: TObject);
    procedure CharacterSetChange(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure UserNameEditingDone(Sender: TObject);
    procedure UseSystemDefaultCSChange(Sender: TObject);
    procedure UseWireCompressionEditingDone(Sender: TObject);
  private
    { Private declarations }
    Database: TIBDatabase;
    FChanging: boolean;
    function Edit: Boolean;
    procedure AddParam(aName, aValue: string);
    procedure DeleteParam(aName: string);
    procedure UpdateParamEditBoxes;
  public
    { Public declarations }
  end;

var
  IBDatabaseEditForm: TIBDatabaseEditForm;

  function EditIBDatabase(ADatabase: TIBDatabase): Boolean;

implementation

{$R *.lfm}

uses TypInfo, IBMessages, IBUtils;

function EditIBDatabase(ADatabase: TIBDatabase): Boolean;
begin
  with TIBDatabaseEditForm.Create(Application) do
  try
    Database := ADatabase;
    Result := Edit;
  finally
    Free;
  end;
end;

procedure TIBDatabaseEditForm.AddParam(aName, aValue: string);
begin
  if Trim(aValue) = '' then
    DeleteParam(aName)
  else
    DatabaseParams.Lines.Values[aName] := Trim(aValue);
end;

procedure TIBDatabaseEditForm.DeleteParam(aName: string);
var
  i: Integer;
begin
    for i := 0 to DatabaseParams.Lines.Count - 1 do
    begin
      if (Pos(aName, LowerCase(DatabaseParams.Lines.Names[i])) = 1) then {mbcs ok}
      begin
        DatabaseParams.Lines.Delete(i);
        break;
      end;
    end;
end;

procedure TIBDatabaseEditForm.UpdateParamEditBoxes;
var st: string;
begin
  UserName.Text := DatabaseParams.Lines.Values['user_name'];
  Password.Text := DatabaseParams.Lines.Values['password'];
  SQLRole.Text := DatabaseParams.Lines.Values['sql_role_name'];
  st := DatabaseParams.Lines.Values['lc_ctype'];
  if (st <> '') then
    CharacterSet.ItemIndex := CharacterSet.Items.IndexOf(st)
  else
    CharacterSet.ItemIndex := -1;
end;

function TIBDatabaseEditForm.Edit: Boolean;
var
  aServerName: string;
  aDatabaseName: string;
  aProtocol: TProtocolAll;
  aPortNo: string;

begin
  if ParseConnectString(Database.DatabaseName, aServerName, aDatabaseName, aProtocol, aPortNo) then
  begin
    ServerName.Text := aServerName;
    DatabasePath.Text := aDatabaseName;
    Protocol.ItemIndex := ord(aProtocol);
    PortNo.Text := aPortNo;
  end;
  ProtocolCloseUp(nil);
  ConnectionTypeBtnSelectionChanged(nil);
  DatabaseParams.Lines.Assign(Database.Params);
  ConfigOverrides.Lines.Assign(Database.ConfigOverrides);
  LoginPrompt.Checked := Database.LoginPrompt;
  UpdateParamEditBoxes;
  UseSystemDefaultCS.Checked := Database.UseDefaultSystemCodePage;
  UseWireCompression.Checked := Database.WireCompression;
  Result := False;
  if ShowModal = mrOk then
  begin
    Database.DatabaseName := MakeConnectString(ServerName.Text, DatabasePath.Text,
      TProtocolAll(Protocol.ItemIndex), PortNo.Text);
    Database.Params.Assign(DatabaseParams.Lines);
    Database.ConfigOverrides.Assign(ConfigOverrides.Lines);
    Database.LoginPrompt := LoginPrompt.Checked;
    Database.UseDefaultSystemCodePage := UseSystemDefaultCS.Checked;
    Result := True;
  end;
end;

procedure TIBDatabaseEditForm.BrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(Application) do
    try
      InitialDir := ExtractFilePath(DatabasePath.Text);
      Filter := SDatabaseFilter;
      if Execute then
        DatabasePath.Text := FileName;
    finally
      Free
    end;
end;

procedure TIBDatabaseEditForm.ConfigOverridesEditingDone(Sender: TObject);
begin
  FChanging := true;
  try
    UseWireCompression.Checked := CompareText(ConfigOverrides.Lines.Values['WireCompression'],'true') = 0;
  finally
    FChanging := false;
  end;
end;

procedure TIBDatabaseEditForm.DatabaseParamsEditingDone(Sender: TObject);
begin
  UpdateParamEditBoxes;
end;

procedure TIBDatabaseEditForm.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if Database.Connected then
  begin
    if MessageDlg(SDisconnectDatabase, mtConfirmation,
      mbOkCancel, 0) <> mrOk then Exit;
    Database.Close;
  end;
  ModalResult := mrOk;
end;

procedure TIBDatabaseEditForm.FormCreate(Sender: TObject);
begin
//  HelpContext := hcDIBDataBaseEdit;
end;

procedure TIBDatabaseEditForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TIBDatabaseEditForm.PasswordEditingDone(Sender: TObject);
begin
  AddParam('password', Password.Text);
end;

procedure TIBDatabaseEditForm.ProtocolCloseUp(Sender: TObject);
begin
  if Protocol.ItemIndex = 3 then
    ConnectionTypeBtn.ItemIndex := 0
  else
    ConnectionTypeBtn.ItemIndex := 1;
end;

procedure TIBDatabaseEditForm.ConnectionTypeBtnSelectionChanged(Sender: TObject);
begin
  if ConnectionTypeBtn.ItemIndex > 0 then
  begin
    Browse.Enabled := False;
    Label7.Enabled := True;
    Label8.Enabled := True;
    Protocol.Enabled := True;
    ServerName.Enabled := True;
    if Protocol.ItemIndex = 3 then
      Protocol.ItemIndex := 4;
  end
  else
  begin
    Browse.Enabled := True;
    Label7.Enabled := False;
    Label8.Enabled := true;
    ServerName.Text := '';
    ServerName.Enabled := False;
    Protocol.Enabled := true;
    Protocol.ItemIndex := 3;
  end;
end;

procedure TIBDatabaseEditForm.SQLRoleEditingDone(Sender: TObject);
begin
  AddParam('sql_role_name', SQLRole.Text);
end;

procedure TIBDatabaseEditForm.CharacterSetChange(Sender: TObject);
begin
  if (CharacterSet.ItemIndex <> -1 ) then {do not localize}
    AddParam('lc_ctype', CharacterSet.Text)
  else
    DeleteParam('lc_ctype');
end;

procedure TIBDatabaseEditForm.TestClick(Sender: TObject);
var
  tempDB : TIBDatabase;
begin
  Test.Enabled := false;
  tempDB := TIBDatabase.Create(nil);
  try
    tempDB.DatabaseName := MakeConnectString(ServerName.Text, DatabasePath.Text,
      TProtocolAll(Protocol.ItemIndex), PortNo.Text);
    tempDB.Params.Assign(DatabaseParams.Lines);
    tempDB.LoginPrompt := LoginPrompt.Checked;
    try
      tempDB.Connected := true;
      ShowMessage('Successful Connection');
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  finally
    tempDB.Free;
    Test.Enabled := true;
  end;
end;

procedure TIBDatabaseEditForm.UserNameEditingDone(Sender: TObject);
begin
  AddParam('user_name', UserName.Text);
end;

procedure TIBDatabaseEditForm.UseSystemDefaultCSChange(Sender: TObject);
begin
  CharacterSet.Enabled := not UseSystemDefaultCS.Checked;
  if UseSystemDefaultCS.Checked then
    DeleteParam('lc_ctype')
  else
  if (CharacterSet.Text <> 'None') then {do not localize}
      AddParam('lc_ctype', CharacterSet.Text)
end;

procedure TIBDatabaseEditForm.UseWireCompressionEditingDone(Sender: TObject);
var Index: integer;
begin
  if FChanging then Exit;
  if UseWireCompression.Checked then
    ConfigOverrides.Lines.Values['WireCompression'] := 'true'
  else
  begin
    Index := ConfigOverrides.Lines.IndexOfName('WireCompression');
    if Index <> -1 then
      ConfigOverrides.Lines.Delete(Index);
  end;
end;


end.
