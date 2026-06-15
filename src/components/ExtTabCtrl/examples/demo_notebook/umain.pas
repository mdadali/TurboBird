unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ValEdit,
  StdCtrls, Buttons, ExtDlgs, ExtTabCtrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    cbShowTabStripes: TCheckBox;
    cbShowAddButton: TCheckBox;
    cbShowCloseButtons: TCheckBox;
    CmbTabStyle: TComboBox;
    ExtTabCtrl1: TExtTabCtrl;
    Image1: TImage;
    ImageList1: TImageList;
    LblTabStyle: TLabel;
    Notebook1: TNotebook;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel2: TPanel;
    PgSettings: TPage;
    PgInfo: TPage;
    PgImage: TPage;
    Panel1: TPanel;
    ValueListEditor1: TValueListEditor;
    procedure Button1Click(Sender: TObject);
    procedure cbShowAddButtonChange(Sender: TObject);
    procedure cbShowCloseButtonsChange(Sender: TObject);
    procedure cbShowTabStripesChange(Sender: TObject);
    procedure CmbTabStyleChange(Sender: TObject);
    procedure ExtTabCtrl1TabChanged(Sender: TObject; NewIndex: Integer);
    procedure ExtTabCtrl1TabCreated(Sender: TObject);
    procedure ExtTabCtrl1TabCreating(Sender: TObject; var ACaption: String;
      var Data: TObject; var Allow: Boolean);
    procedure ExtTabCtrl1TabDeleting(Sender: TObject; Index: Integer;
      var Allow: Boolean);
    procedure ExtTabCtrl1TabReordered(Sender: TObject; OldIndex,
      NewIndex: Integer);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadImage(const AFileName: String);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  CmbTabStyle.ItemIndex := ord(ExtTabCtrl1.TabStyle);
  ExtTabCtrl1.AutoSize := True;
  ExtTabCtrl1.TabIndex := 0;
end;

procedure TForm1.ExtTabCtrl1TabChanged(Sender: TObject; NewIndex: Integer);
begin
  if (NewIndex >= 0) and (NewIndex < Notebook1.PageCount) then
    Notebook1.PageIndex := NewIndex;
end;

procedure TForm1.ExtTabCtrl1TabCreated(Sender: TObject);
begin
  if cbShowTabStripes.Checked then
    ExtTabCtrl1.Tabs[ExtTabCtrl1.Tabs.Count-1].StripeColor := Random($FFFFFF);
end;

procedure TForm1.ExtTabCtrl1TabCreating(Sender: TObject; var ACaption: String;
  var Data: TObject; var Allow: Boolean);
var
  idx: Integer;
begin
  idx := Notebook1.Pages.Add(ACaption);
  Notebook1.Page[idx].Color := Random($FFFFFF);
  Allow := True;
end;

procedure TForm1.ExtTabCtrl1TabDeleting(Sender: TObject; Index: Integer;
  var Allow: Boolean);
begin
  if (Index >= 0) and (Index < Notebook1.PageCount) then
  begin
    Notebook1.Pages.Delete(Index);
    Allow := True;
  end;
end;

procedure TForm1.ExtTabCtrl1TabReordered(Sender: TObject; OldIndex,
  NewIndex: Integer);
begin
  if (OldIndex >= 0) and (OldIndex < Notebook1.PageCount) and
     (NewIndex >= 0) and (NewIndex < Notebook1.PageCount)
  then
    Notebook1.Pages.Move(OldIndex, NewIndex);
end;

procedure TForm1.CmbTabStyleChange(Sender: TObject);
begin
  ExtTabCtrl1.TabStyle := TTabStyle(CmbTabStyle.ItemIndex);
end;

procedure TForm1.cbShowAddButtonChange(Sender: TObject);
begin
  if cbShowAddButton.Checked then
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions + [toShowAddButton]
  else
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions - [toShowAddButton];
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    LoadImage(OpenPictureDialog1.FileName);
end;

procedure TForm1.cbShowCloseButtonsChange(Sender: TObject);
begin
  if cbShowCloseButtons.Checked then
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions + [toShowCloseButton]
  else
    ExtTabCtrl1.TabOptions := ExtTabCtrl1.TabOptions - [toShowCloseButton];
end;

procedure TForm1.cbShowTabStripesChange(Sender: TObject);
const
  COLORS: array[0..5] of TColor = (clRed, clLime, clYellow, clBlue, clFuchsia, clAqua);
var
  i: Integer;
  tab: TExtTab;
begin
  for i := 0 to ExtTabCtrl1.Tabs.Count-1 do
  begin
    tab := ExtTabCtrl1.Tabs[i];
    if cbShowTabStripes.Checked then
      tab.StripeColor := COLORS[i mod Length(COLORS)]
    else
      tab.StripeColor := clNone;
  end;
end;

procedure TForm1.LoadImage(const AFileName: String);
begin
  Image1.Picture.LoadFromFile(AFileName);
  if Lowercase(ExtractFileExt(AFileName)) = '.ico' then
    Image1.Picture.Icon.Current := 0;
  ValueListEditor1.Clear;
  ValueListEditor1.InsertRow('FileName', ExtractFileName(AFileName), true);
  ValueListEditor1.InsertRow('Path', ExtractFileDir(ExpandFileName(AFileName)), true);
  ValueListEditor1.InsertRow('Width', IntToStr(Image1.Picture.Width), true);
  ValueListEditor1.InsertRow('Height', IntToStr(Image1.Picture.Height), true);
  ValueListEditor1.Row := 1;
end;

end.

