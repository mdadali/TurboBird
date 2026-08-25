unit DefLayersFrm;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Spin, StdCtrls,
  Buttons, CADSys4;

type

  { TDefLayersForm }

  TDefLayersForm = class(TForm)
    PenColorButton: TColorButton;
    BrushColorButton: TColorButton;
    LayersList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TransparentChk: TCheckBox;
    OKBtn: TBitBtn;
    NameEdt: TEdit;
    PenSizeEdt: TSpinEdit;
    ActiveLayerCBox: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    ActiveChk: TCheckBox;
    VisibleChk: TCheckBox;
    procedure LayersListClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    fCurrLayer: Integer;
    fCADCmp: TCADCmp;

    procedure SetCurrLayer(L: Integer);
    procedure SaveCurrLayer;
  public
    { Public declarations }
    procedure Execute(CADCmp: TCADCmp);
  end;

var
  DefLayersForm: TDefLayersForm;

implementation

{$R *.lfm}

procedure TDefLayersForm.Execute;
var
  Cont: Integer;
  TmpStr: String;
begin
  LayersList.Items.Clear;
  fCADCmp := CADCmp;
  with fCADCmp do
   begin
     for Cont := 0 to 255 do
      begin
        TmpStr := Format('%d - %s', [Cont, Layers[Cont].Name]);
        LayersList.Items.Add(TmpStr);
        ActiveLayerCBox.Items.Add(TmpStr);
      end;
     SetCurrLayer(0);
     ActiveLayerCBox.ItemIndex := fCADCmp.CurrentLayer;
   end;
  ShowModal;
end;

procedure TDefLayersForm.SaveCurrLayer;
begin
  with fCADCmp do
   begin
     Layers[fCurrLayer].Name := NameEdt.Text;
     Layers[fCurrLayer].Pen.Width := PenSizeEdt.Value;
     Layers[fCurrLayer].Pen.Color := PenColorButton.ButtonColor;
     Layers[fCurrLayer].Opaque := not TransparentChk.Checked;
     Layers[fCurrLayer].Visible := VisibleChk.Checked;
     Layers[fCurrLayer].Active := ActiveChk.Checked;
     Layers[fCurrLayer].Brush.Color := BrushColorButton.ButtonColor;
   end;
end;

procedure TDefLayersForm.SetCurrLayer(L: Integer);
begin
  with fCADCmp do
   begin
     fCurrLayer := L;
     NameEdt.Text := Layers[fCurrLayer].Name;
     PenSizeEdt.Value := Layers[fCurrLayer].Pen.Width;
     // It's strange but its' work :(
     //if PenColorGrid.ColorToIndex(Layers[fCurrLayer].Pen.Color) < 8 then
     // PenColorGrid.ForeGroundIndex := PenColorGrid.ColorToIndex(Layers[fCurrLayer].Pen.Color)
     //else
     // PenColorGrid.ForeGroundIndex := PenColorGrid.ColorToIndex(Layers[fCurrLayer].Pen.Color) - 1;
     //if BrushColorGrid.ColorToIndex(Layers[fCurrLayer].Brush.Color) < 8 then
     // BrushColorGrid.ForeGroundIndex := BrushColorGrid.ColorToIndex(Layers[fCurrLayer].Brush.Color)
     //else
     // BrushColorGrid.ForeGroundIndex := BrushColorGrid.ColorToIndex(Layers[fCurrLayer].Brush.Color) - 1;
     PenColorButton.ButtonColor := Layers[fCurrLayer].Pen.Color;
     BrushColorButton.ButtonColor := Layers[fCurrLayer].Brush.Color;
     TransparentChk.Checked := not Layers[fCurrLayer].Opaque;
     ActiveChk.Checked := Layers[fCurrLayer].Active;
     VisibleChk.Checked := Layers[fCurrLayer].Visible;
   end;
end;

procedure TDefLayersForm.LayersListClick(Sender: TObject);
begin
  SaveCurrLayer;
  SetCurrLayer(LayersList.ItemIndex);
end;

procedure TDefLayersForm.OKBtnClick(Sender: TObject);
begin
  SaveCurrLayer;
  if ActiveLayerCBox.ItemIndex >= 0 then
   fCADCmp.CurrentLayer := ActiveLayerCBox.ItemIndex;
end;

end.
