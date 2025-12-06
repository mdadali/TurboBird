unit ViewGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LCLType,
  turbocommon,
  uthemeselector;

type

  { TfmViewGen }

  TfmViewGen = class(TForm)
      bbClose: TSpeedButton;
      edGenName: TEdit;
      edValue: TEdit;
      GroupBox1: TGroupBox;
      Label1: TLabel;
      Label3: TLabel;
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

var
  fmViewGen: TfmViewGen;

implementation

{ TfmViewGen }

procedure TfmViewGen.Init(ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
end;


procedure TfmViewGen.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FNodeInfos^.ViewForm := nil;
  CloseAction:= caFree;
end;

procedure TfmViewGen.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TfmViewGen.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmViewGen.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free
end;

initialization
  {$I viewgen.lrs}

end.

