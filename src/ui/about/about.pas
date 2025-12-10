unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, LCLIntf, ComCtrls, GifAnim,
  turbocommon,
  updatechecker,
  uthemeselector;


{$i turbocommon.inc}

type

  { TfmAbout }

  TfmAbout = class(TForm)
    bbtnClose: TBitBtn;
    GifAnim1: TGifAnim;
    Image1: TImage;
    Image3: TImage;
    Image4: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    lbPowered: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    laUpdate: TLabel;
    laWebSite: TLabel;
    lbFPCVersion: TLabel;
    lbLazarus: TLabel;
    lbOSInfo: TLabel;
    lbPgmVersion: TLabel;
    lbVersionDate: TLabel;
    lbVersionTime: TLabel;
    lbWidgetSet: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnlMWA: TPanel;
    ProgressBar1: TProgressBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    procedure bbtnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GifAnim1Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure laUpdateClick(Sender: TObject);
    procedure laWebSiteClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    procedure Init;
    { public declarations }
  end; 

var
  fmAbout: TfmAbout;

implementation

{ TfmAbout }

uses Main;

procedure TfmAbout.laWebSiteClick(Sender: TObject);
begin
  OpenURL(laWebSite.Caption);
end;

procedure TfmAbout.Timer1Timer(Sender: TObject);
begin
  GifAnim1.Left := GifAnim1.Left + 20;
  if  GifAnim1.Left > self.Width then
    GifAnim1.Left := 0 - GifAnim1.Width;
end;

procedure TfmAbout.Init;
begin
  {lbPgmVersion.Caption:= 'Version ' + fmMain.Version;
  lbVersionDate.Caption:= fmMain.VersionDate;
  laTarget.Caption:= 'Target : ' + Target + '-' + Arch;
  }
  //lbPgmVersion.Caption := GetProgramVersion;
  lbPgmVersion.Caption := VERSION; //from version.inc
  //lbPgmVersion.Caption := ExtractVersionFromName(Application.ExeName);
  lbLazarus.Caption    := GetLazarusVersion;
  lbFPCVersion.Caption := GetFPCVersion;
  lbOSInfo.Caption     := GetOSInfo;
  lbWidgetSet.Caption  := GetLCLWidgetSet;
  lbVersionDate.Caption:= GetProgramBuildDate;
  lbVersionTime.Caption:= GetProgramBuildTime;
end;

procedure TfmAbout.Label6Click(Sender: TObject);
begin
  OpenURL('http://lazarus.freepascal.org');
end;

procedure TfmAbout.laUpdateClick(Sender: TObject);
var frmUpdateChecker: TfrmUpdateChecker;
begin
  try
    frmUpdateChecker := TfrmUpdateChecker.Create(self);
    frmUpdateChecker.ShowModal;
  finally
    frmUpdateChecker.Free;
  end;
end;

procedure TfmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := false;
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  GifAnim1.Width := self.Width;
  pnlMWA.Width := self.Width;
end;

procedure TfmAbout.FormShow(Sender: TObject);
var BasePath, ImagesPath, GifAnimFile: string;
begin
  GifAnim1.Left := -150;
  BasePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  GifAnimFile  := BasePath + PathDelim + 'system' + PathDelim + 'images' + PathDelim + 'fpc_running_logo_backup.gif';
  if FileExists(GifAnimFile) then
  begin
    Timer1.Enabled := true;
    GifAnim1.FileName := GifAnimFile;
    GifAnim1.Animate := true;
  end;
end;

procedure TfmAbout.GifAnim1Click(Sender: TObject);
begin
  OpenURL('https://www.lazarus-ide.org');
end;

procedure TfmAbout.Image4Click(Sender: TObject);
begin
  OpenURL('https://www.mwasoftware.co.uk');
end;

procedure TfmAbout.bbtnCloseClick(Sender: TObject);
begin
  Timer1.Enabled := false;
  GifAnim1.Animate := false;
  Close;
end;

initialization
  {$I about.lrs}

end.

