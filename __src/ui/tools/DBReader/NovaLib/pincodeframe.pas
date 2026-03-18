unit PinCodeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons;

type

  { TFramePinCode }

  TFramePinCode = class(TFrame)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edValue: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
  private
    FAllowedChars: string;
    FMaxLength: Integer;
    FMaxValue: Int64;
    FHideChars: Boolean;
    FOkPressed: Boolean;

    function GetValue: Int64;
    procedure SetValue(AValue: Int64);
  public
    procedure Clear();
    procedure OnShowHandler(Sender: TObject);

    property AllowedChars: string read FAllowedChars write FAllowedChars;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    property HideChars: Boolean read FHideChars write FHideChars;
    property Value: Int64 read GetValue write SetValue;
    property OkPressed: Boolean read FOkPressed;
  end;

function ShowPinCodeQuery(AOwner: TControl;
  var AValue: Int64;
  ACaption: string;
  AMaxLength: Integer = 4;
  AAllowedChars: string = '1234567890';
  AHideChars: Boolean = True): Boolean;

implementation

{$R *.lfm}

function ShowPinCodeQuery(AOwner: TControl;
  var AValue: Int64;
  ACaption: string;
  AMaxLength: Integer = 4;
  AAllowedChars: string = '1234567890';
  AHideChars: Boolean = True): Boolean;
var
  TmpForm: TForm;
  TmpFrame: TFramePinCode;
begin
  Result := False;
  TmpForm := TForm.Create(AOwner);
  TmpFrame := TFramePinCode.Create(TmpForm);
  try
    TmpForm.Width := TmpFrame.Width;
    TmpForm.Height := TmpFrame.Height;
    TmpForm.Position := poOwnerFormCenter;
    TmpForm.Caption := ACaption;

    TmpFrame.Parent := TmpForm;
    TmpFrame.Align := alClient;
    if AValue >=0 then
      TmpFrame.Value := AValue
    else
      TmpFrame.Clear();
    TmpFrame.MaxLength := AMaxLength;
    TmpFrame.AllowedChars := AAllowedChars;
    TmpFrame.HideChars := AHideChars;

    TmpForm.ShowModal();

    if TmpFrame.OkPressed then
    begin
      Result := True;
      AValue := TmpFrame.Value;
    end;
  finally
    TmpForm.Free();
  end;
end;

{ TFramePinCode }

procedure TFramePinCode.btnOKClick(Sender: TObject);
begin
  FOkPressed := True;
  if Assigned(Parent) and (Parent is TCustomForm) then
    (Parent as TCustomForm).Close();
end;

procedure TFramePinCode.btnCancelClick(Sender: TObject);
begin
  FOkPressed := False;
  if Assigned(Parent) and (Parent is TCustomForm) then
    (Parent as TCustomForm).Close();
end;

function TFramePinCode.GetValue: Int64;
begin
  Result := StrToInt64Def(Trim(edValue.Text), -1);
end;

procedure TFramePinCode.SetValue(AValue: Int64);
begin
  edValue.Text := IntToStr(AValue);
end;

procedure TFramePinCode.Clear();
begin
  edValue.Text := '';
end;

procedure TFramePinCode.OnShowHandler(Sender: TObject);
begin
  edValue.SetFocus();
end;

procedure TFramePinCode.edValueChange(Sender: TObject);
var
  nn: Int64;
  s: string;
begin
  nn := StrToInt64Def((Sender as TCustomEdit).Text, -1);
  if nn > FMaxValue then
    s := IntToStr(FMaxValue)
  else if nn < 0 then
    s := ''
  else
    s := IntToStr(nn);

  if (Sender as TCustomEdit).Text <> s then
    (Sender as TCustomEdit).Text := s;
end;

procedure TFramePinCode.edValueKeyPress(Sender: TObject; var Key: Char);
var
  ed: TCustomEdit;
  s: string;
  nn: Int64;
begin
  ed := (Sender as TCustomEdit);
  if Pos(Key, FAllowedChars+#8) > 0 then
  begin
    // symbol insert imitation
    s := ed.Text;
    if ed.SelLength > 0 then
      Delete(s, ed.SelStart+1, ed.SelLength);
    Insert(Key, s, ed.SelStart+1);
    nn := StrToInt64Def(s, 0);
    if nn > FMaxValue then
      Key := #0;
  end
  else
  begin
    Key := #0;
    //MessageBeep(MB_OK);
  end;
end;

end.

