unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, LCLIntf,
  turbocommon,
  updatechecker;


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
    lbVersionTime: TLabel;
    lbWidgetSet: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbOSInfo: TLabel;
    lbFPCVersion: TLabel;
    lbLazarus: TLabel;
    lbPgmVersion: TLabel;
    Label4: TLabel;
    laWebSite: TLabel;
    Label6: TLabel;
    lbVersionDate: TLabel;
    laUpdate: TLabel;
    Shape1: TShape;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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

procedure TfmAbout.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$I about.lrs}

end.

