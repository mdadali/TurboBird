unit ViewSProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL, LCLType, ExtCtrls, IniFiles,
  turbocommon,
  uthemeselector;

type

  { TfmViewSProc }

  TfmViewSProc = class(TForm)
    edName: TEdit;
    edOwner: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    seScript: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
  fmViewSProc: TfmViewSProc;

implementation

{ TfmViewSProc }

procedure TfmViewSProc.Init(ANodeInfos: TPNodeInfos);
begin
   FNodeInfos := ANodeInfos;
end;

procedure TfmViewSProc.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FNodeInfos^.ViewForm := nil;
  CloseAction:= caFree;
end;

procedure TfmViewSProc.FormCreate(Sender: TObject);
begin
end;

procedure TfmViewSProc.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TfmViewSProc.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmViewSProc.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

initialization
  {$I viewsproc.lrs}

end.

