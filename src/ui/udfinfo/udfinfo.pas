unit UDFInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LCLType, ExtCtrls, ComCtrls,
  turbocommon,
  uthemeselector;

type

  { TfmUDFInfo }

  TfmUDFInfo = class(TForm)
    bbClose: TSpeedButton;
    edEntry: TEdit;
    edModule: TEdit;
    edName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    meBody: TMemo;
    Panel1: TPanel;
    Panel3: TPanel;
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FNodeInfos: TPNodeInfos;
  public
    { public declarations }
    procedure Init(ANodeInfos: TPNodeInfos);
  end;


implementation

{ TfmUDFInfo }

procedure TfmUDFInfo.Init(ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
end;

procedure TfmUDFInfo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;

procedure TfmUDFInfo.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmUDFInfo.bbCloseClick(Sender: TObject);
begin
  TTabSheet(Parent).Free;
  Close;
end;

procedure TfmUDFInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  CloseAction := caFree;
end;


initialization
  {$I udfinfo.lrs}

end.

