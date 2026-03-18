unit ViewDomain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LCLType,
  turbocommon,
  uthemeselector;

type

  { TfmViewDomain }

  TfmViewDomain = class(TForm)
      bbClose: TSpeedButton;
      edName: TEdit;
      GroupBox1: TGroupBox;
      Label1: TLabel;
      Label3: TLabel;
      Label4: TLabel;
      Label5: TLabel;
      Label6: TLabel;
      Label7: TLabel;
      Label8: TLabel;
      laCharacterSet: TLabel;
      laCheckConstraint: TLabel;
      laCollation: TLabel;
      laDefault: TLabel;
      laSize: TLabel;
      laType: TLabel;
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    { private declarations }
  public
    { public declarations }
    procedure Init(ANodeInfos: TPNodeInfos);
  end; 

var
  fmViewDomain: TfmViewDomain;

implementation

{ TfmViewDomain }

procedure TfmViewDomain.Init(ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
end;

procedure TfmViewDomain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FNodeInfos^.ViewForm := nil;
  CloseAction:= caFree;
end;

procedure TfmViewDomain.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TfmViewDomain.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmViewDomain.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

initialization
  {$I viewdomain.lrs}

end.

