unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, LCLIntf, ComCtrls,
  turbocommon,
  updatechecker,
  uthemeselector;


{$i turbocommon.inc}

type

  { TfmAbout }

  TfmAbout = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
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
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure laUpdateClick(Sender: TObject);
    procedure laWebSiteClick(Sender: TObject);
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
  frmUpdateChecker := TfrmUpdateChecker.Create(self);
  frmUpdateChecker.ShowModal;
  frmUpdateChecker.Free;
  //fmUpdate:= TfmUpdate.Create(nil);
  //fmUpdate.Init(fmMain.Major, fmMain.Minor, fmMain.ReleaseVersion);
  //fmUpdate.Init(VERSION_MAJOR, VERSION_MINOR, VERSION_REVISION, VERSION_BUILD);
  //fmUpdate.Show;
  //Application.ProcessMessages;
end;

procedure TfmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //CloseAction:= caFree;
end;

procedure TfmAbout.FormShow(Sender: TObject);
begin
  if Assigned(frmThemeSelector) then
    frmThemeSelector.btnApplyClick(self);
end;

procedure TfmAbout.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$I about.lrs}

end.

