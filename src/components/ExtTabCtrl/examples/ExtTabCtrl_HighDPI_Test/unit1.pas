unit Unit1;

{ Darkmode:
  - Windows: Comment/uncomment the line in the initialization part.
  - other OSs: the application should follow the system settings.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls,
 {$IFDEF MSWINDOWS}
  uDarkStyleParams, uWin32WidgetSetDark, uDarkStyleSchemes, uMetaDarkStyle,
 {$ENDIF}
  ExtTabCtrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbRotateNewTabBtn: TCheckBox;
    cbUseExternalImages: TCheckBox;
    cbShowNewTabBtn: TCheckBox;
    cbRotateTabIcons: TCheckBox;
    ExtTabCtrl1: TExtTabCtrl;
    ImageList1: TImageList;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    procedure cbRotateNewTabBtnChange(Sender: TObject);
    procedure cbUseExternalImagesChange(Sender: TObject);
    procedure cbShowNewTabBtnChange(Sender: TObject);
    procedure cbRotateTabIconsChange(Sender: TObject);
    procedure ExtTabCtrl1TabCreated(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  ExtTabCtrl1.TabPosition := TTabPosition(RadioGroup1.ItemIndex);
  ExtTabCtrl1.Align := TAlign(RadioGroup1.ItemIndex + 1);
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  ExtTabCtrl1.TabStyle := TTabStyle(RadioGroup2.ItemIndex);
end;

procedure TForm1.cbRotateNewTabBtnChange(Sender: TObject);
begin
  if cbRotateNewTabBtn.Checked then
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions + [toRotateAddImage]
  else
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions - [toRotateAddImage];
end;

procedure TForm1.cbUseExternalImagesChange(Sender: TObject);
begin
  if cbUseExternalImages.Checked then
    ExtTabCtrl1.Images := ImageList1
  else
    ExtTabCtrl1.Images := nil;
end;

procedure TForm1.cbShowNewTabBtnChange(Sender: TObject);
begin
  if cbShowNewTabBtn.Checked then
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions + [toShowAddButton]
  else
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions - [toShowAddButton];
end;

procedure TForm1.cbRotateTabIconsChange(Sender: TObject);
begin
  if cbRotateTabIcons.Checked then
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions + [toRotateTabImages]
  else
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions - [toRotateTabImages];
end;

procedure TForm1.ExtTabCtrl1TabCreated(Sender: TObject);
var
  tab: TExtTab;
begin
  tab := ExtTabCtrl1.Tabs[ExtTabCtrl1.Tabs.Count-1];
  tab.ImageIndex := (tab.Index + 4) mod ImageList1.Count;
end;

{$IFDEF MSWINDOWS}
procedure SetDarkStyle;
begin
  // Settings from MetaDarkStyle:
  PreferredAppMode:=pamForceDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
end;

initialization
  SetDarkStyle;
{$ENDIF}

end.

