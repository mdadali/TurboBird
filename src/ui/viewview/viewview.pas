unit ViewView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL, LCLType, ExtCtrls, IniFiles,
  uthemeselector;

type

  { TfmViewView }

  TfmViewView = class(TForm)
    bbClose: TSpeedButton;
    edName: TEdit;
      GroupBox1: TGroupBox;
      Label1: TLabel;
      Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    seScript: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure bbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fmViewView: TfmViewView;

implementation

{ TfmViewView }

procedure TfmViewView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmViewView.FormCreate(Sender: TObject);
var
   configFile: TIniFile;
   configFilePath: String;
begin
     // Set the editor font from config.ini
    configFilePath:= ConcatPaths([ExtractFilePath(Application.ExeName), 'config.ini']);
    configFile:= TIniFile.Create(configFilePath);
    seScript.Font.Name:=configFile.ReadString('Editor Font', 'font_name', 'Monospace');
    seScript.Font.Size:=configFile.ReadInteger('Editor Font', 'font_size', 11);
    configFile.Free;
end;

procedure TfmViewView.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TfmViewView.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmViewView.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

initialization
  {$I viewview.lrs}

end.

