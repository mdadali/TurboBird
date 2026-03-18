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

unit IBXServiceEditor;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, IBXServices, IB;

type

  { TIBXServiceEditorForm }

  TIBXServiceEditorForm = class(TForm)
    Bevel1: TBevel;
    CancelBtn: TButton;
    ConfigOverrides: TMemo;
    ConnectionTypeBtn: TRadioGroup;
    Label10: TLabel;
    PortNo: TEdit;
    Label1: TLabel;
    ServiceParams: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LoginPrompt: TCheckBox;
    OKBtn: TButton;
    Password: TEdit;
    Protocol: TComboBox;
    ServerName: TEdit;
    Test: TButton;
    UserName: TEdit;
    UseWireCompression: TCheckBox;
    procedure ConfigOverridesEditingDone(Sender: TObject);
    procedure ConnectionTypeBtnSelectionChanged(Sender: TObject);
    procedure PasswordEditingDone(Sender: TObject);
    procedure ProtocolCloseUp(Sender: TObject);
    procedure ServiceParamsEditingDone(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure UserNameEditingDone(Sender: TObject);
    procedure UseWireCompressionEditingDone(Sender: TObject);
  private
    { private declarations }
    FChanging: boolean;
    function Edit: Boolean;
    procedure AddParam(aName, Value: string);
    procedure DeleteParam(aName: string);
    procedure UpdateParamEditBoxes;
  public
    { public declarations }
    Service: TIBXServicesConnection;
  end;

function EditIBXService(aService: TIBXServicesConnection): boolean;

var
  IBXServiceEditorForm: TIBXServiceEditorForm;

implementation

{$R *.lfm}

function EditIBXService(aService: TIBXServicesConnection): boolean;
begin
  with TIBXServiceEditorForm.Create(Application) do
  try
    Service := aService;
    Result := Edit
  finally
    Free
  end
end;

{ TIBXServiceEditorForm }

procedure TIBXServiceEditorForm.ConnectionTypeBtnSelectionChanged(
  Sender: TObject);
begin
  if ConnectionTypeBtn.ItemIndex = 0 then
  begin
    Label7.Enabled := False;
    ServerName.Text := '';
    ServerName.Enabled := False;
    Protocol.ItemIndex := 3;
  end
  else
  begin
    Label7.Enabled := True;
    ServerName.Enabled := True;
    if Protocol.ItemIndex = 3 then
      Protocol.ItemIndex := 4;
  end;
end;

procedure TIBXServiceEditorForm.ConfigOverridesEditingDone(Sender: TObject);
begin
  FChanging := true;
  try
    UseWireCompression.Checked := CompareText(ConfigOverrides.Lines.Values['WireCompression'],'true') = 0;
  finally
    FChanging := false;
  end;
end;

procedure TIBXServiceEditorForm.PasswordEditingDone(Sender: TObject);
begin
  AddParam('password', Password.Text);
end;

procedure TIBXServiceEditorForm.ProtocolCloseUp(Sender: TObject);
begin
  if Protocol.ItemIndex = 3 then
    ConnectionTypeBtn.ItemIndex := 0
  else
    ConnectionTypeBtn.ItemIndex := 1;
end;

procedure TIBXServiceEditorForm.ServiceParamsEditingDone(Sender: TObject);
begin
  UpdateParamEditBoxes;
end;

procedure TIBXServiceEditorForm.TestClick(Sender: TObject);
var tempService: TIBXServicesConnection; {Use as example for test}
begin
  Test.Enabled := false;
  tempService := TIBXServicesConnection.Create(nil);
  try
    tempService.Protocol := TProtocol(Protocol.ItemIndex);
    tempService.ServerName := ServerName.Text;
    tempService.PortNo := PortNo.Text;
    tempService.Params.Assign(ServiceParams.Lines);
    tempService.LoginPrompt := true;
    try
      tempService.Connected := true;
      ShowMessage('Successful Connection');
      tempService.Connected := false;
    except on E: Exception do
      ShowMessage(E.Message)
    end;
  finally
    tempService.Free;
    Test.Enabled := true;
  end;
end;

procedure TIBXServiceEditorForm.UserNameEditingDone(Sender: TObject);
begin
  AddParam('user_name', UserName.Text);
end;

procedure TIBXServiceEditorForm.UseWireCompressionEditingDone(Sender: TObject);
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

function TIBXServiceEditorForm.Edit: Boolean;
begin
  ServiceParams.Lines.Assign(Service.Params);
  ConfigOverrides.Lines.Assign(Service.ConfigOverrides);

  ServerName.Text := Service.ServerName;
  Protocol.ItemIndex := ord(Service.Protocol);
  LoginPrompt.Checked := Service.LoginPrompt;
  UseWireCompression.Checked := Service.WireCompression;
  PortNo.Text := Service.PortNo;
  ProtocolCloseUp(nil);
  ConnectionTypeBtnSelectionChanged(nil);
  UpdateParamEditBoxes;
  Result := False;
  if ShowModal = mrOk then
  begin
    Service.Protocol := TProtocol(Protocol.ItemIndex);
    Service.ServerName := ServerName.Text;
    Service.PortNo := PortNo.Text;
    Service.Params.Assign(ServiceParams.Lines);
    Service.ConfigOverrides.Assign(ConfigOverrides.Lines);
    Service.LoginPrompt := LoginPrompt.Checked;
    Result := True;
  end;
end;

procedure TIBXServiceEditorForm.AddParam(aName, Value: string);
begin
  if Trim(Value) = '' then
    DeleteParam(aName)
  else
    ServiceParams.Lines.Values[aName] := Trim(Value);
end;

procedure TIBXServiceEditorForm.DeleteParam(aName: string);
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

procedure TIBXServiceEditorForm.UpdateParamEditBoxes;
begin
  UserName.Text := ServiceParams.Lines.Values['user_name'];
  Password.Text := ServiceParams.Lines.Values['password'];
end;


end.
