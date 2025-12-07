unit ViewTrigger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL, LCLType, ExtCtrls, IniFiles,
  turbocommon,
  uthemeselector;

type

  { TfmViewTrigger }

  TfmViewTrigger = class(TForm)
    bbClose: TButton;
      edName: TEdit;
      edOnTable: TEdit;
      GroupBox1: TGroupBox;
      Label1: TLabel;
      Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    laEnabled: TLabel;
    laEvent: TLabel;
    laPos: TLabel;
    laType: TLabel;
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
    FNodeInfos: TPNodeInfos;
  public
    { public declarations }
    procedure Init(ANodeInfos: TPNodeInfos);
  end; 

//var
  //fmViewTrigger: TfmViewTrigger;

implementation

{ TfmViewTrigger }

procedure TfmViewTrigger.Init(ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
end;

procedure TfmViewTrigger.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FNodeInfos^.ViewForm := nil;
  CloseAction:= caFree;
end;

procedure TfmViewTrigger.FormCreate(Sender: TObject);
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

procedure TfmViewTrigger.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TfmViewTrigger.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmViewTrigger.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

initialization
  {$I viewtrigger.lrs}

end.

