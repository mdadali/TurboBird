(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2011 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit ibserviceeditor;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, IBServices, IB;

type

  { TIBServiceEditorForm }

  TIBServiceEditorForm = class(TForm)
    Bevel1: TBevel;
    CancelBtn: TButton;
    PortNo: TEdit;
    Label1: TLabel;
    ServiceParams: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LocalRbtn: TRadioButton;
    LoginPrompt: TCheckBox;
    OKBtn: TButton;
    Password: TEdit;
    Protocol: TComboBox;
    RemoteRbtn: TRadioButton;
    ServerName: TEdit;
    Test: TButton;
    UserName: TEdit;
    procedure LocalRbtnClick(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure RemoteRbtnClick(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure UserNameChange(Sender: TObject);
  private
    { private declarations }
    function Edit: Boolean;
    function GetParam(aName: string): string;
    procedure AddParam(aName, Value: string);
    procedure DeleteParam(aName: string);
  public
    { public declarations }
    Service: TIBCustomService;
  end;

function EditIBService(aService: TIBCustomService): boolean;

var
  IBServiceEditorForm: TIBServiceEditorForm;

implementation

{$R *.lfm}

function EditIBService(aService: TIBCustomService): boolean;
begin
  with TIBServiceEditorForm.Create(Application) do
  try
    Service := aService;
    Result := Edit
  finally
    Free
  end
end;

{ TIBServiceEditorForm }

procedure TIBServiceEditorForm.LocalRbtnClick(Sender: TObject);
begin
  Label7.Enabled := False;
  Label8.Enabled := False;
  ServerName.Enabled := False;
  Protocol.Enabled := False;
end;

procedure TIBServiceEditorForm.PasswordChange(Sender: TObject);
begin
  AddParam('password', Password.Text);
end;

procedure TIBServiceEditorForm.RemoteRbtnClick(Sender: TObject);
begin
  Label7.Enabled := True;
  Label8.Enabled := True;
  Protocol.Enabled := True;
  ServerName.Enabled := True;
  if Protocol.Text = '' then
    Protocol.Text := 'TCP';
end;

procedure TIBServiceEditorForm.TestClick(Sender: TObject);
var tempService: TIBControlService; {Use as example for test}
begin
  Test.Enabled := false;
  tempService := TIBControlService.Create(nil);
  try
    if LocalRbtn.Checked then
    begin
      tempService.ServerName := '';
      tempService.Protocol := Local;
    end
    else
    begin
      case Protocol.ItemIndex of
      0: tempService.Protocol := TCP;
      1: tempService.Protocol := NamedPipe;
      2: tempService.Protocol := SPX;
      end;
      tempService.ServerName := ServerName.Text
    end;
    tempService.PortNo := PortNo.Text;
    tempService.Params.Assign(ServiceParams.Lines);
    tempService.LoginPrompt := true;
    try
      tempService.Active := true;
      ShowMessage('Successful Connection');
      tempService.Active := false;
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  finally
    tempService.Free;
    Test.Enabled := true;
  end;
end;

procedure TIBServiceEditorForm.UserNameChange(Sender: TObject);
begin
  AddParam('user_name', UserName.Text);
end;

function TIBServiceEditorForm.Edit: Boolean;
begin
  if Trim(Service.Params.Text) = '' then
    ServiceParams.Clear
  else
    ServiceParams.Lines.Assign(Service.Params);

  ServerName.Text := Service.ServerName;
  LoginPrompt.Checked := Service.LoginPrompt;
  PortNo.Text := Service.PortNo;
  UserName.Text := GetParam('user_name');
  Password.Text := GetParam('password');
  RemoteRbtn.Checked := (Service.Protocol <> Local);
  Result := False;
  if ShowModal = mrOk then
  begin
    if LocalRbtn.Checked then
    begin
      Service.Protocol := Local;
      Service.ServerName := ''
    end
    else
    begin
      case Protocol.ItemIndex of
      0: Service.Protocol := TCP;
      1: Service.Protocol := NamedPipe;
      2: Service.Protocol := SPX;
      end;
      Service.ServerName := ServerName.Text
    end;
    Service.Params := ServiceParams.Lines;
    Service.LoginPrompt := LoginPrompt.Checked;
    Service.PortNo := PortNo.Text;
    Result := True;
  end;
end;

function TIBServiceEditorForm.GetParam(aName: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to ServiceParams.Lines.Count - 1 do
  begin
    if (Pos(aName, LowerCase(ServiceParams.Lines.Names[i])) = 1) then
    begin
      Result := ServiceParams.Lines.Values[ServiceParams.Lines.Names[i]];
      break;
    end;
  end;
end;

procedure TIBServiceEditorForm.AddParam(aName, Value: string);
var
  i: Integer;
  found: boolean;
begin
  found := False;
  if Trim(Value) <> '' then
  begin
    ServiceParams.Lines.NameValueSeparator := '=';
    for i := 0 to ServiceParams.Lines.Count - 1 do
    begin
      if (Pos(aName, LowerCase(ServiceParams.Lines.Names[i])) = 1) then {mbcs ok}
      begin
        ServiceParams.Lines.Values[ServiceParams.Lines.Names[i]] := Value;
        found := True;
        break;
      end;
    end;
    if not found then
      ServiceParams.Lines.Add(aName + '=' + Value);
  end
  else
    DeleteParam(Name);
end;

procedure TIBServiceEditorForm.DeleteParam(aName: string);
var
  i: Integer;
begin
    for i := 0 to ServiceParams.Lines.Count - 1 do
    begin
      if (Pos(aName, LowerCase(ServiceParams.Lines.Names[i])) = 1) then {mbcs ok}
      begin
        ServiceParams.Lines.Delete(i);
        break;
      end;
    end;
end;


end.
