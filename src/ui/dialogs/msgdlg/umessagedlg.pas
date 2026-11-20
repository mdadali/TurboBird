unit umessagedlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  uBasseForm;

type
  TMsgKind = (mkInfo, mkWarning, mkError, mkQuestion);
  TMsgButtons = (mbOK, mbOKCancel, mbYesNo, mbYesNoCancel);

  { TfrmMessageDlg }

  TfrmMessageDlg = class(TfrmBasseForm)
    Memo1: TMemo;
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnYes: TButton;
    btnNo: TButton;
  private
    procedure Setup(const AMessage: string; AKind: TMsgKind; AButtons: TMsgButtons);
    procedure ArrangeButtons;
  end;

function MsgBox(const AMessage: string; AKind: TMsgKind = mkInfo): TModalResult;
function MsgConfirm(const AMessage: string; AKind: TMsgKind = mkQuestion): Boolean;
function MsgYesNoCancel(const AMessage: string; AKind: TMsgKind = mkQuestion): TModalResult;
function MsgError(const AMessage: string): TModalResult;
function MsgWarning(const AMessage: string): TModalResult;

implementation

{$R *.lfm}

procedure TfrmMessageDlg.Setup(const AMessage: string; AKind: TMsgKind;
  AButtons: TMsgButtons);
begin
  Memo1.Lines.Text := AMessage;

  case AKind of
    mkInfo:     Caption := 'Information';
    mkWarning:  Caption := 'Warnung';
    mkError:    Caption := 'Fehler';
    mkQuestion: Caption := 'Frage';
  end;

  btnOK.Visible     := False;
  btnCancel.Visible := False;
  btnYes.Visible    := False;
  btnNo.Visible     := False;

  case AButtons of
    mbOK:
      begin
        btnOK.Visible := True;
        btnOK.Default := True;
        btnOK.Cancel := True;
        ActiveControl := btnOK;
      end;
    mbOKCancel:
      begin
        btnOK.Visible := True;
        btnCancel.Visible := True;
        btnOK.Default := True;
        btnCancel.Cancel := True;
        ActiveControl := btnOK;
      end;
    mbYesNo:
      begin
        btnYes.Visible := True;
        btnNo.Visible := True;
        btnYes.Default := True;
        ActiveControl := btnYes;
      end;
    mbYesNoCancel:
      begin
        btnYes.Visible := True;
        btnNo.Visible := True;
        btnCancel.Visible := True;
        btnYes.Default := True;
        btnCancel.Cancel := True;
        ActiveControl := btnYes;
      end;
  end;

  AdjustSize;
  ArrangeButtons;
end;

procedure TfrmMessageDlg.ArrangeButtons;
var
  i, x, y: Integer;
  VisibleButtons: array of TButton;
begin
  SetLength(VisibleButtons, 0);
  for i := 0 to pnlBottom.ControlCount - 1 do
    if (pnlBottom.Controls[i] is TButton) and TButton(pnlBottom.Controls[i]).Visible then
    begin
      SetLength(VisibleButtons, Length(VisibleButtons) + 1);
      VisibleButtons[High(VisibleButtons)] := TButton(pnlBottom.Controls[i]);
    end;

  x := pnlBottom.Width - 12;
  y := (pnlBottom.Height - 32) div 2;

  for i := High(VisibleButtons) downto 0 do
  begin
    x := x - VisibleButtons[i].Width;
    VisibleButtons[i].SetBounds(x, y, VisibleButtons[i].Width, VisibleButtons[i].Height);
    x := x - 8;
  end;
end;

function MsgBox(const AMessage: string; AKind: TMsgKind): TModalResult;
var
  Dlg: TfrmMessageDlg;
begin
  Dlg := TfrmMessageDlg.Create(nil);
  try
    Dlg.Setup(AMessage, AKind, mbOK);
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function MsgConfirm(const AMessage: string; AKind: TMsgKind): Boolean;
var
  Dlg: TfrmMessageDlg;
begin
  Dlg := TfrmMessageDlg.Create(nil);
  try
    Dlg.Setup(AMessage, AKind, mbYesNo);
    Result := Dlg.ShowModal = mrYes;
  finally
    Dlg.Free;
  end;
end;

function MsgYesNoCancel(const AMessage: string; AKind: TMsgKind): TModalResult;
var
  Dlg: TfrmMessageDlg;
begin
  Dlg := TfrmMessageDlg.Create(nil);
  try
    Dlg.Setup(AMessage, AKind, mbYesNoCancel);
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function MsgError(const AMessage: string): TModalResult;
begin
  Result := MsgBox(AMessage, mkError);
end;

function MsgWarning(const AMessage: string): TModalResult;
begin
  Result := MsgBox(AMessage, mkWarning);
end;

end.
