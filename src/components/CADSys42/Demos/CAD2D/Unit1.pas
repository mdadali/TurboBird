unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls,
  Printers, ClipBrd, CADSys4, CS4BaseTypes, CS4Tasks, CS4Shapes, CS4DXFModule,
  ExtCtrls, ImgList, PrintersDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    ToolBar1: TToolBar;
    LocalCAD: TCADCmp2D;
    StatusBar1: TStatusBar;
    LocalPopUp: TPopupMenu;
    LocalPrg: TCADPrg2D;
    AddLineBtn: TToolButton;
    AddFrameBtn: TToolButton;
    AddRectangleBtn: TToolButton;
    AddEllipseBtn: TToolButton;
    AddFillEllipseBtn: TToolButton;
    AddArcBtn: TToolButton;
    AddPolylineBtn: TToolButton;
    AddPolygonBtn: TToolButton;
    AddSplineBtn: TToolButton;
    Accept1: TMenuItem;
    Cancel1: TMenuItem;
    N1: TMenuItem;
    Zoomarea1: TMenuItem;
    Zoomin1: TMenuItem;
    Zoomout1: TMenuItem;
    Zoomall1: TMenuItem;
    Panning1: TMenuItem;
    N2: TMenuItem;
    Showgrid1: TMenuItem;
    Keepaspect1: TMenuItem;
    Usesnap1: TMenuItem;
    Useorto1: TMenuItem;
    File1: TMenuItem;
    ImageList1: TImageList;
    ToolButton5: TToolButton;
    MoveBtn: TToolButton;
    RotateBtn: TToolButton;
    EditBtn: TToolButton;
    Useareatoselectobjects1: TMenuItem;
    Layers1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    Merge1: TMenuItem;
    ImportDXF1: TMenuItem;
    ExportDXF1: TMenuItem;
    CADOpenDlg: TOpenDialog;
    CADSaveDlg: TSaveDialog;
    New1: TMenuItem;
    DXFOpenDlg: TOpenDialog;
    DXFSaveDlg: TSaveDialog;
    Alligment1: TMenuItem;
    Left1: TMenuItem;
    Center1: TMenuItem;
    Right1: TMenuItem;
    TextBtn: TToolButton;
    DefBlockBtn: TToolButton;
    AddBlockBtn: TToolButton;
    N3: TMenuItem;
    Exit1: TMenuItem;
    N4: TMenuItem;
    Print1: TMenuItem;
    Actualview1: TMenuItem;
    Fit1: TMenuItem;
    Scale1: TMenuItem;
    PrintDialog1: TPrintDialog;
    Copytoclipboard1: TMenuItem;
    Setpoint1: TMenuItem;
    LocalView: TCADViewport2D;
    Test1: TMenuItem;
    procedure AddLineBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AddArcBtnClick(Sender: TObject);
    procedure AddPolylineBtnClick(Sender: TObject);
    procedure Accept1Click(Sender: TObject);
    procedure LocalPrgStartOperation(Sender: TObject; const Operation: TCADStateClass;
      const Param: TCADPrgParam);
    procedure LocalPrgEndOperation(Sender: TObject; const Operation: TCADStateClass;
      const Param: TCADPrgParam);
    procedure LocalPrgStopOperation(Sender: TObject; const Operation: TCADStateClass;
      const Param: TCADPrgParam);
    procedure Cancel1Click(Sender: TObject);
    procedure Zoomarea1Click(Sender: TObject);
    procedure Zoomout1Click(Sender: TObject);
    procedure Zoomin1Click(Sender: TObject);
    procedure Zoomall1Click(Sender: TObject);
    procedure Panning1Click(Sender: TObject);
    procedure LocalViewMouseMove2D(Sender: TObject; Shift: TShiftState; WX,
      WY: Single; X, Y: Integer);
    procedure LocalPrgDescriptionChanged(Sender: TObject);
    procedure LocalPopUpPopup(Sender: TObject);
    procedure Showgrid1Click(Sender: TObject);
    procedure Keepaspect1Click(Sender: TObject);
    procedure Usesnap1Click(Sender: TObject);
    procedure Useorto1Click(Sender: TObject);
    procedure MoveBtnClick(Sender: TObject);
    procedure RotateBtnClick(Sender: TObject);
    procedure Useareatoselectobjects1Click(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure Layers1Click(Sender: TObject);
    procedure AddFrameBtnClick(Sender: TObject);
    procedure AddRectangleBtnClick(Sender: TObject);
    procedure AddEllipseBtnClick(Sender: TObject);
    procedure AddFillEllipseBtnClick(Sender: TObject);
    procedure AddPolygonBtnClick(Sender: TObject);
    procedure AddSplineBtnClick(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Merge1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure ImportDXF1Click(Sender: TObject);
    procedure ExportDXF1Click(Sender: TObject);
    procedure TextBtnClick(Sender: TObject);
    procedure Left1Click(Sender: TObject);
    procedure Center1Click(Sender: TObject);
    procedure Right1Click(Sender: TObject);
    procedure LocalViewDblClick(Sender: TObject);
    procedure DefBlockBtnClick(Sender: TObject);
    procedure AddBlockBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Exit1Click(Sender: TObject);
    procedure Actualview1Click(Sender: TObject);
    procedure Fit1Click(Sender: TObject);
    procedure Scale1Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Setpoint1Click(Sender: TObject);
    procedure LocalViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Test1Click(Sender: TObject);
  private
    { Private declarations }
    fCurrentOpBtn: TToolButton;

    procedure OnSelectedObj(Sender: TCAD2DSelectObjectsParam; Obj: TObject2D; CtrlPt: Integer; Added: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DefLayersFrm;

{$R *.lfm}

type
  TMyCADCreateTheSourceBlock = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

constructor TMyCADCreateTheSourceBlock.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  TmpStr: String;
  TmpIter: TExclusiveGraphicObjIterator;
  TmpBlk: TSourceBlock2D;
begin
  inherited;
  if Param is TCAD2DSelectObjectsParam then
   with TCAD2DSelectObjectsParam(Param), TCADCmp2D(CADPrg.Viewport.CADCmp) do
    begin
      if not InputQuery('Define block', 'Name', TmpStr) then
       begin
         NextState := CADPrg.DefaultState;
         Param.Free;
         Param := nil;
       end;
      TmpIter := SelectedObjects.GetExclusiveIterator;
      try
        TmpIter.First;
        while TmpIter.Current <> nil do
         begin
           RemoveObject(Tmpiter.Current.ID);
           TmpIter.Next;
         end;
        TmpBlk := BlockObjects(StringToBlockName(TmpStr), TmpIter);
        if Assigned(TmpBlk) then
         TmpBlk.IsLibraryBlock := True;
      finally
        TmpIter.Free;
      end;
      CADPrg.RepaintAfterOperation;
    end;
  NextState := CADPrg.DefaultState;
  Param.Free;
  Param := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  TmpStr: TFileStream;
begin
  if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Library.blk') then
   begin
     TmpStr := TFileStream.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Library.blk', fmOpenRead);
     try
      LocalCAD.LoadLibrary(TmpStr);
     finally
      TmpStr.Free;
     end;
   end;
  LocalCAD.DefaultLayersColor := clBlack;
  with LocalView do
   begin
     UsePaintingThread := False;
     LocalView.ShowGrid := Showgrid1.Checked;
     if Keepaspect1.Checked then
      LocalView.AspectRatio := 1.0
     else
      LocalView.AspectRatio := 0.0;
     ZoomWindow(Rect2D(-50, -50, 50, 50));
   end;
  with LocalPrg do
   begin
     ShowCursorCross := True;
     XSnap := 1.0;
     YSnap := 1.0;
     UseSnap := Usesnap1.Checked;
     UseOrto := Useorto1.Checked;
   end;
end;

procedure TForm1.AddLineBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddLineBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TLine2D.Create(-1, Point2D(0, 0), Point2D(0, 0)), 0, True));
   end;
end;

procedure TForm1.AddArcBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddArcBtn;
     StartOperation(TCAD2DDrawArcPrimitive, TCAD2DDrawArcPrimitiveParam.Create(nil,
        TArc2D.Create(-1, Point2D(0, 0), Point2D(0, 0), 0, 0)));
   end;
end;

procedure TForm1.AddPolylineBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddPolylineBtn;
     StartOperation(TCAD2DDrawUnsizedPrimitive, TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
        TPolyline2D.Create(-1, [Point2D(0, 0)]), 0, True));
   end;
end;

procedure TForm1.AddFrameBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddFrameBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TFrame2D.Create(-1, Point2D(0, 0), Point2D(0, 0)), 0, True));
   end;
end;

procedure TForm1.AddRectangleBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddRectangleBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TRectangle2D.Create(-1, Point2D(0, 0), Point2D(0, 0)), 0, True));
   end;
end;

procedure TForm1.AddEllipseBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddEllipseBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TEllipse2D.Create(-1, Point2D(0, 0), Point2D(0, 0)), 0, True));
   end;
end;

procedure TForm1.AddFillEllipseBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddFillEllipseBtn;
     StartOperation(TCAD2DDrawSizedPrimitive, TCAD2DDrawSizedPrimitiveParam.Create(nil,
        TFilledEllipse2D.Create(-1, Point2D(0, 0), Point2D(0, 0)), 0, True));
   end;
end;

procedure TForm1.AddPolygonBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddPolygonBtn;
     StartOperation(TCAD2DDrawUnsizedPrimitive, TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
        TPolygon2D.Create(-1, [Point2D(0, 0)]), 0, True));
   end;
end;

procedure TForm1.AddSplineBtnClick(Sender: TObject);
var
  TmpSpline: TBSpline2D;
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := AddSplineBtn;
     TmpSpline := TBSpline2D.Create(-1, [Point2D(0, 0)]);
     TmpSpline.SavingType := stSpace;
     StartOperation(TCAD2DDrawUnsizedPrimitive, TCAD2DDrawUnsizedPrimitiveParam.Create(nil,
        TmpSpline, 0, True));
   end;
end;

procedure TForm1.DefBlockBtnClick(Sender: TObject);
var
  TmpPar: TCADPrgParam;
begin
  if Useareatoselectobjects1.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TMyCADCreateTheSourceBlock);
     with LocalPrg do
      StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end
  else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TMyCADCreateTheSourceBlock);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := OnSelectedObj;
     with LocalPrg do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TForm1.AddBlockBtnClick(Sender: TObject);
var
  TmpStr: String;
  SrcBlk: TSourceBlock2D;
begin
  if not InputQuery('Add block', 'Name', TmpStr) then
   Exit;
  SrcBlk := LocalCAD.FindSourceBlock(StringToBlockName(TmpStr));
  if SrcBlk <> nil then
   LocalPrg.StartOperation(TCAD2DPositionObject,
       TCAD2DPositionObjectParam.Create(nil, TBlock2D.Create(-1, SrcBlk)));
end;

procedure TForm1.TextBtnClick(Sender: TObject);
var
  TmpText: TJustifiedVectText2D;
  TmpStr: String;
  TmpH: TRealType;
begin
  if not InputQuery('Add Text', 'Height', TmpStr) then
   Exit;
  TmpH := StrToFloat(TmpStr);
  if not InputQuery('Add Text', 'String', TmpStr) then
   Exit;
  TmpText := TJustifiedVectText2D.Create(-1, CADSysFindFontByIndex(0), Rect2D(0, 0, 0, 0), TmpH, TmpStr);
  if Left1.Checked then
   TmpText.HorizontalJust := jhLeft
  else if Right1.Checked then
   TmpText.HorizontalJust := jhRight
  else if Center1.Checked then
   TmpText.HorizontalJust := jhCenter;
  LocalPrg.StartOperation(TCAD2DPositionObject, TCAD2DPositionObjectParam.Create(nil, TmpText));
end;

procedure TForm1.MoveBtnClick(Sender: TObject);
var
  TmpPar: TCADPrgParam;
begin
  if Useareatoselectobjects1.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DMoveSelectedObjects);
     with LocalPrg do
      StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end
  else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DMoveSelectedObjects);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := OnSelectedObj;
     with LocalPrg do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TForm1.RotateBtnClick(Sender: TObject);
var
  TmpPar: TCADPrgParam;
begin
  if Useareatoselectobjects1.Checked then
   begin
     TmpPar := TCAD2DSelectObjectsInAreaParam.Create(gmAllInside, TCAD2DRotateSelectedObjects);
     with LocalPrg do
      StartOperation(TCAD2DSelectObjectsInArea, TmpPar);
   end
  else
   begin
     TmpPar := TCAD2DSelectObjectsParam.Create(5, TCAD2DRotateSelectedObjects);
     TCAD2DSelectObjectsParam(TmpPar).OnObjectSelected := OnSelectedObj;
     with LocalPrg do
      StartOperation(TCAD2DSelectObjects, TmpPar);
   end;
end;

procedure TForm1.EditBtnClick(Sender: TObject);
begin
  with LocalPrg do
   begin
     if IsBusy then
      StopOperation;
     fCurrentOpBtn := EditBtn;
     StartOperation(TCAD2DSelectObject, TCAD2DSelectObjectsParam.Create(5, TCAD2DEditSelectedPrimitive));
   end;
end;

procedure TForm1.Accept1Click(Sender: TObject);
begin
  with LocalPrg do
   begin
     SendUserEvent(CADPRG_ACCEPT);
   end;
end;

procedure TForm1.Cancel1Click(Sender: TObject);
begin
  with LocalPrg do
   begin
     SendUserEvent(CADPRG_CANCEL);
     StopOperation;
   end;
end;

procedure TForm1.Zoomarea1Click(Sender: TObject);
begin
  with LocalPrg do
   SuspendOperation(TCADPrgZoomArea, nil);
end;

procedure TForm1.Zoomin1Click(Sender: TObject);
begin
  LocalView.ZoomIn;
end;

procedure TForm1.Zoomout1Click(Sender: TObject);
begin
  LocalView.ZoomOut;
end;

procedure TForm1.Zoomall1Click(Sender: TObject);
begin
  LocalView.ZoomToExtension;
end;

procedure TForm1.Panning1Click(Sender: TObject);
begin
  with LocalPrg do
   SuspendOperation(TCADPrgRealTimePan, nil);
end;

procedure TForm1.Showgrid1Click(Sender: TObject);
begin
  Showgrid1.Checked := not Showgrid1.Checked;
  LocalView.ShowGrid := Showgrid1.Checked;
end;

procedure TForm1.Keepaspect1Click(Sender: TObject);
begin
  Keepaspect1.Checked := not Keepaspect1.Checked;
  if Keepaspect1.Checked then
   LocalView.AspectRatio := 1.0
  else
   LocalView.AspectRatio := 0.0;
  LocalView.ZoomWindow(LocalView.VisualRect);
end;

procedure TForm1.Usesnap1Click(Sender: TObject);
begin
  Usesnap1.Checked := not Usesnap1.Checked;
  LocalPrg.UseSnap := Usesnap1.Checked;
end;

procedure TForm1.Useorto1Click(Sender: TObject);
begin
  Useorto1.Checked := not Useorto1.Checked;
  LocalPrg.UseOrto := Useorto1.Checked;
end;

procedure TForm1.Useareatoselectobjects1Click(Sender: TObject);
begin
  Useareatoselectobjects1.Checked := not Useareatoselectobjects1.Checked;
end;

procedure TForm1.Layers1Click(Sender: TObject);
begin
  DefLayersForm := TDefLayersForm.Create(Self);
  try
    DefLayersForm.Execute(LocalCAD);
    LocalView.Repaint;
  finally
    DefLayersForm.Free;
  end;
end;

procedure TForm1.Load1Click(Sender: TObject);
begin
  if CADOpenDlg.Execute then
   LocalCAD.LoadFromFile(CADOpenDlg.FileName);
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if CADSaveDlg.Execute then
   LocalCAD.SaveToFile(CADSaveDlg.FileName);
end;

procedure TForm1.Merge1Click(Sender: TObject);
begin
  if CADOpenDlg.Execute then
   LocalCAD.MergeFromFile(CADOpenDlg.FileName);
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  LocalCAD.DeleteAllObjects;
  LocalCAD.DeleteSavedSourceBlocks;
  LocalCAD.RepaintViewports;
end;

procedure TForm1.ImportDXF1Click(Sender: TObject);
begin
  if DXFOpenDlg.Execute then
   with TDXF2DImport.Create(DXFOpenDlg.FileName, LocalCAD) do
    try
      SetTextFont(CADSysFindFontByIndex(0));
      ReadDXF;
      LocalView.ZoomToExtension;
    finally
      Free;
    end;
end;

procedure TForm1.ExportDXF1Click(Sender: TObject);
begin
  if DXFSaveDlg.Execute then
   with TDXF2DExport.Create(DXFSaveDlg.FileName, LocalCAD) do
    try
      WriteDXF;
    finally
      Free;
    end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Actualview1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
   begin
     Printer.BeginDoc;
     try
      LocalView.CopyToCanvas(Printer.Canvas, cmAspect, cvActual, 1, 1);
     finally
      Printer.EndDoc;
     end;
   end;
end;

procedure TForm1.Fit1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
   begin
     Printer.BeginDoc;
     try
      LocalView.CopyToCanvas(Printer.Canvas, cmAspect, cvExtension, 1, 1);
     finally
      Printer.EndDoc;
     end;
   end;
end;

procedure TForm1.Scale1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
   begin
     Printer.BeginDoc;
     try
      LocalView.CopyToCanvas(Printer.Canvas, cmAspect, cvScale, 1, 1);
     finally
      Printer.EndDoc;
     end;
   end;
end;

procedure TForm1.Copytoclipboard1Click(Sender: TObject);
begin
  LocalView.CopyToClipboard(Clipboard);
end;

procedure TForm1.Left1Click(Sender: TObject);
begin
  Left1.Checked := True;
end;

procedure TForm1.Center1Click(Sender: TObject);
begin
  Center1.Checked := True;
end;

procedure TForm1.Right1Click(Sender: TObject);
begin
  Right1.Checked := True;
end;

procedure TForm1.LocalPrgStartOperation(Sender: TObject; const Operation: TCADStateClass;
      const Param: TCADPrgParam);
begin
  if Assigned(fCurrentOpBtn) then
   fCurrentOpBtn.Indeterminate := True;
end;

procedure TForm1.LocalPrgEndOperation(Sender: TObject; const Operation: TCADStateClass;
      const Param: TCADPrgParam);
begin
  if Assigned(fCurrentOpBtn) then
   begin
     fCurrentOpBtn.Indeterminate := False;
     fCurrentOpBtn := nil;
   end;
end;

procedure TForm1.LocalPrgStopOperation(Sender: TObject; const Operation: TCADStateClass;
      const Param: TCADPrgParam);
begin
  if Assigned(fCurrentOpBtn) then
   begin
     fCurrentOpBtn.Indeterminate := False;
     fCurrentOpBtn := nil;
   end;
end;

procedure TForm1.LocalPrgDescriptionChanged(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := TCADState(Sender).Description;
end;

procedure TForm1.LocalViewMouseMove2D(Sender: TObject; Shift: TShiftState;
  WX, WY: Single; X, Y: Integer);
begin
  with LocalPrg.CurrentViewportSnappedPoint do
   StatusBar1.Panels[0].Text := Format('X: %6.3f Y: %6.3f', [X, Y]);
  LocalView.SetFocus; 
end;

procedure TForm1.LocalPopUpPopup(Sender: TObject);
var
  IsZoomingOperation: Boolean;
begin
  Accept1.Enabled := LocalPrg.IsBusy;
  Cancel1.Enabled := LocalPrg.IsBusy;
  IsZoomingOperation := (LocalPrg.CurrentOperation = TCADPrgZoomArea)
                        or (LocalPrg.CurrentOperation = TCADPrgRealTimePan)
                        or LocalPrg.IsSuspended;
  Zoomarea1.Enabled := not IsZoomingOperation;
  ZoomIn1.Enabled := not IsZoomingOperation;
  Zoomout1.Enabled := not IsZoomingOperation;
  Zoomall1.Enabled := not IsZoomingOperation;
  Panning1.Enabled := not IsZoomingOperation;
end;

procedure TForm1.OnSelectedObj(Sender: TCAD2DSelectObjectsParam; Obj: TObject2D; CtrlPt: Integer; Added: Boolean);
begin
  if Assigned(Obj) then
   LocalView.DrawObject2DWithRubber(Obj, True);
end;

procedure TForm1.LocalViewDblClick(Sender: TObject);
var
  TmpPt: TPoint2D;
  TmpObj: TObject2D;
  TmpN: Integer;
  TmpStr: String;
begin
  TmpPt := LocalPrg.CurrentViewportSnappedPoint;
  TmpObj := LocalView.PickObject(TmpPt, 5, False, TmpN);
  if TmpObj is TJustifiedVectText2D then
   with TJustifiedVectText2D(TmpObj) do
    begin
      TmpStr := Text;
      if not InputQuery('Edit Text', 'String', TmpStr) then
       Exit;
      Text := TmpStr;
      LocalView.Repaint;
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  TmpStr: TFileStream;
begin
  if LocalCAD.SourceBlocksCount = 0 then
   Exit;
  TmpStr := TFileStream.Create(ExtractFilePath(Application.ExeName) + '\Library.blk', fmOpenWrite or fmCreate);
  try
   LocalCAD.SaveLibrary(TmpStr);
  finally
   TmpStr.Free;
  end;
end;

procedure TForm1.Setpoint1Click(Sender: TObject);
var
  TmpStrX, TmpStrY: String;
begin
  TmpStrX := Format('%6.3f', [LocalPrg.CurrentViewportPoint.X]);
  TmpStrY := Format('%6.3f', [LocalPrg.CurrentViewportPoint.Y]);
  if not InputQuery('Insert point', 'X', TmpStrX) then
   Exit;
  if not InputQuery('Insert point', 'Y', TmpStrY) then
   Exit;
  LocalPrg.CurrentViewportPoint := Point2D(StrToFloat(TmpStrX), StrToFloat(TmpStrY));
  LocalPrg.SendCADEvent(ceMouseDown, mbLeft, [], 0);
end;

procedure TForm1.LocalViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 104 then
   with LocalPrg.CurrentViewportPoint do
    Y := Y + 10.0;
  LocalPrg.SendCADEvent(ceMouseMove, mbLeft, [], 0);
end;

procedure TForm1.Test1Click(Sender: TObject);
var
  ST, ET: TDateTime;
  H, M, S, MS: Word;
begin
  ST := Now;
  LocalView.Repaint;
  ET := Now;
  DecodeTime(ET-ST, H, M, S, MS);
  ShowMessage(Format('%d:%d - %d', [S, MS, LocalCAD.ObjectsCount]));
end;

initialization
  CADSysRegisterFontFromFile(0, IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'RomanC.fnt');
end.
