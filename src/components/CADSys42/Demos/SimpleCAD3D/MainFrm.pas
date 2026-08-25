unit MainFrm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CADSys4,
  ExtCtrls, StdCtrls, ToolWin, ComCtrls, CS4Shapes, CS4Tasks, CS4DXFModule,
  SphereShape, ImgList;

type

  { TMainForm }

  TMainForm = class(TForm)
    InfoPnl: TPanel;
    ToolButton3: TToolButton;
    ToolPnl: TPanel;
    CmdPnl: TPanel;
    LocalCAD: TCADCmp3D;
    CurPosLbl: TLabel;
    CursorPosSLbl: TStaticText;
    WPlanePosLbl: TLabel;
    WPlaneNrmLbl: TLabel;
    WPlaneUP: TLabel;
    SnapLbl: TLabel;
    GridStepLbl: TLabel;
    UseSnapChk: TCheckBox;
    UseOrtoChk: TCheckBox;
    ShowGrdChk: TCheckBox;
    Bevel1: TBevel;
    SnapSLbl: TStaticText;
    GridStepSLbl: TStaticText;
    WPlanePosSLbl: TStaticText;
    WPlaneNrmSLbl: TStaticText;
    WPlaneUpSLbl: TStaticText;
    Splitter1: TSplitter;
    ViewPnl: TPanel;
    LocalCADPrg: TCADPrg3D;
    ToolsBar: TToolBar;
    ToolsImages: TImageList;
    CmdsBar: TToolBar;
    CmdsImages: TImageList;
    NewBtn: TToolButton;
    MoveBtn: TToolButton;
    RotateBtn: TToolButton;
    ScaleBtn: TToolButton;
    EditPrimBtn: TToolButton;
    FaceBtn: TToolButton;
    PolyExtrBtn: TToolButton;
    CuboBtn: TToolButton;
    ToolButton8: TToolButton;
    CilBtn: TToolButton;
    SplineExtrBtn: TToolButton;
    SphereBtn: TToolButton;
    TaskInfoLbl: TLabel;
    ToolButton1: TToolButton;
    PageControl1: TPageControl;
    TabPerspective: TTabSheet;
    LocalPerspView: TCADPerspectiveViewport3D;
    TabOrto: TTabSheet;
    LocalOrtoView: TCADParallelViewport3D;
    LoadBtn: TToolButton;
    SaveBtn: TToolButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ImportBtn: TToolButton;
    OpenDialog2: TOpenDialog;
    ZoomInBtn: TToolButton;
    ZoomOutBtn: TToolButton;
    ZoomAllBtn: TToolButton;
    PanBtn: TToolButton;
    ToolButton2: TToolButton;
    OrbitBtn: TToolButton;
    DollyBtn: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure LocalViewMouseMove3D(Sender: TObject; Shift: TShiftState; WX,
      WY, WZ: Single; X, Y: Integer);
    procedure ModeModifiersChkClick(Sender: TObject);
    procedure MoveBtnClick(Sender: TObject);
    procedure RotateBtnClick(Sender: TObject);
    procedure ScaleBtnClick(Sender: TObject);
    procedure LocalViewBeginRedraw(Sender: TObject);
    procedure FaceBtnClick(Sender: TObject);
    procedure LocalViewMouseEnter(Sender: TObject);
    procedure LocalCADPrgDescriptionChanged(Sender: TObject);
    procedure LocalViewMouseUp3D(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; WX, WY, WZ: Single; X, Y: Integer);
    procedure PolyExtrBtnClick(Sender: TObject);
    procedure CuboBtnClick(Sender: TObject);
    procedure CilBtnClick(Sender: TObject);
    procedure SplineExtrBtnClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure SphereBtnClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure LocalCADLoadProgress(Sender: TObject; ReadPercent: Byte);
    procedure LocalCADSaveProgress(Sender: TObject; SavePercent: Byte);
    procedure ImportBtnClick(Sender: TObject);
    procedure ZoomInBtnClick(Sender: TObject);
    procedure ZoomOutBtnClick(Sender: TObject);
    procedure ZoomAllBtnClick(Sender: TObject);
    procedure PanBtnClick(Sender: TObject);
    procedure LocalViewEndRedraw(Sender: TObject);
    procedure EditPrimBtnClick(Sender: TObject);
    procedure DollyBtnClick(Sender: TObject);
    procedure OrbitBtnClick(Sender: TObject);
    procedure LocalCADPrgIdle(Sender: TObject);
    procedure LocalCADPrgEnterState(Sender: TObject;
      const State: TCADState);
    procedure LocalCADPrgExitState(Sender: TObject;
      const State: TCADState);
  private
    { Private declarations }
    fGridObject: TPlanarFieldGrid3D;

    procedure OnSelectObject(Sender: TCAD3DSelectObjectsParam; Obj: TObject3D; CtrlPt: Integer; Added: Boolean);
    procedure UpdateToolBars;
    procedure SetInfoPanel;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TMainForm.SetInfoPanel;
begin
  with LocalCADPrg do
   begin
     UseSnapChk.Checked := UseSnap;
     UseOrtoChk.Checked := UseOrto;
     SnapSLbl.Caption := Format('X:%6.2f Y:%6.2f', [XSnap, YSnap]);
     GridStepSLbl.Caption := Format('X:%6.2f Y:%6.2f', [fGridObject.DeltaX, fGridObject.DeltaY]);
     WPlanePosSLbl.Caption := Format('X:%.2f Y:%.2f Z: %.2f', [WorkingPlaneOrigin.X,
                                                               WorkingPlaneOrigin.Y,
                                                               WorkingPlaneOrigin.Z]);
     WPlaneNrmSLbl.Caption := Format('X:%.2f Y:%.2f Z: %.2f', [WorkingPlaneNormal.X,
                                                               WorkingPlaneNormal.Y,
                                                               WorkingPlaneNormal.Z]);
     WPlaneUPSLbl.Caption := Format('X:%.2f Y:%.2f Z: %.2f', [WorkingPlaneUP.X,
                                                              WorkingPlaneUP.Y,
                                                              WorkingPlaneUP.Z]);
   end;
end;

procedure TMainForm.UpdateToolBars;
var
  Enbl: Boolean;
begin
  Enbl := (not Assigned(LocalCADPrg.CurrentState)) or
          (LocalCADPrg.CurrentState.CanBeSuspended and
           (not LocalCADPrg.IsSuspended));
  PanBtn.Enabled := Enbl;
  OrbitBtn.Enabled := Enbl;
  DollyBtn.Enabled := Enbl;
end;

procedure TMainForm.OnSelectObject(Sender: TCAD3DSelectObjectsParam; Obj: TObject3D; CtrlPt: Integer; Added: Boolean);
begin
  with Sender do
   if Assigned(Obj) then
    begin
      LocalCADPrg.Viewport3D.RubberPenColor := clBlue;
      LocalCADPrg.Viewport3D.DrawObject3DWithRubber(Obj, True);
      LocalCADPrg.Viewport3D.RubberPenColor := clRed;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialize global data.
  PageControl1.ActivePage := TabPerspective;
  with LocalCADPrg do
   begin
     Viewport3D := LocalPerspView;
     WorkingPlaneOrigin := Point3D(0, 0, 0);
     WorkingPlaneNormal := Versor3D(0, 0, 1);
     WorkingPlaneUP := Versor3D(0, 1, 0);
     fGridObject := TPlanarFieldGrid3D.Create(0, WorkingPlaneOrigin,
                                                 WorkingPlaneNormal, WorkingPlaneUP);
     fGridObject.DeltaX := XSnap * 10;
     fGridObject.DeltaY := YSnap * 10;
     fGridObject.FieldExtension := 500;
     fGridObject.GridColor := clSilver;
   end;
  // Initialize starting viewpoint.
  LocalPerspView.BeginUpdate;
  with LocalPerspView do
   try
     RubberPen.Style := psDot;
     FrontClip := 10000;
     BackClip := -10000;
     SetCamera(Point3D(-100, -100, 80), Point3D(0, 0, 0), Versor3D(0, 0, 1));
     SetFieldOfView(DegToRad(30.0), 1.0);
   finally
     EndUpdate;
   end;
  LocalOrtoView.BeginUpdate;
  with LocalOrtoView do
   try
     RubberPen.Style := psDot;
     FrontClip := 10000;
     BackClip := -10000;
     SetCamera(Point3D(-100, -100, 80), Point3D(0, 0, 0), Versor3D(0, 0, 1));
     ZoomWindow(Rect2D(-50, -50, 50, 50));
   finally
     EndUpdate;
   end;
  // Update the visual informations.
  SetInfoPanel;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fGridObject.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  LocalPerspView.Repaint;
  LocalOrtoView.Repaint;
end;

procedure TMainForm.LocalCADLoadProgress(Sender: TObject;
  ReadPercent: Byte);
begin
  TaskInfoLbl.Caption := Format('Loaded %d %%', [ReadPercent]);
  TaskInfoLbl.Repaint;
end;

procedure TMainForm.LocalCADSaveProgress(Sender: TObject;
  SavePercent: Byte);
begin
  TaskInfoLbl.Caption := Format('Saved %d %%', [SavePercent]);
  TaskInfoLbl.Repaint;
end;

procedure TMainForm.LocalViewBeginRedraw(Sender: TObject);
begin
  if ShowGrdChk.Checked then
    case PageControl1.ActivePageIndex of
      0: LocalPerspView.DrawObject3D(fGridObject, False);
      1: LocalOrtoView.DrawObject3D(fGridObject, False);
      else begin
       //
      end;
    end;
  Screen.Cursor := crAppStart;
end;

procedure TMainForm.LocalViewEndRedraw(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TMainForm.LocalViewMouseEnter(Sender: TObject);
begin
  LocalCADPrg.Viewport3D.SetFocus;
end;

procedure TMainForm.LocalViewMouseUp3D(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; WX, WY, WZ: Single; X,
  Y: Integer);
begin
  if Button = mbRight then
   LocalCADPrg.SendUserEvent(CADPRG_ACCEPT);
end;

procedure TMainForm.LocalViewMouseMove3D(Sender: TObject;
  Shift: TShiftState; WX, WY, WZ: Single; X, Y: Integer);
begin
  // Update the cursor position label with the working plane position.
  with LocalCADPrg.CurrentWorkingPlaneSnappedPoint do
   CursorPosSLbl.Caption := Format('X:%.2f Y:%.2f Z:%.2f', [X, Y, Z]);
end;

procedure TMainForm.LocalCADPrgDescriptionChanged(Sender: TObject);
begin
  if Assigned(Sender) then
   TaskInfoLbl.Caption := TCADState(Sender).Description + ' Right mouse click to accept.';
end;

procedure TMainForm.LocalCADPrgIdle(Sender: TObject);
begin
  UpdateToolBars;
  TaskInfoLbl.Caption := '';
end;

procedure TMainForm.LocalCADPrgEnterState(Sender: TObject;
  const State: TCADState);
begin
  UpdateToolBars;
end;

procedure TMainForm.LocalCADPrgExitState(Sender: TObject;
  const State: TCADState);
begin
  UpdateToolBars;
end;

procedure TMainForm.ModeModifiersChkClick(Sender: TObject);
begin
  LocalCADPrg.UseSnap := UseSnapChk.Checked;
  LocalCADPrg.UseOrto := UseOrtoChk.Checked;
  if Sender = ShowGrdChk then
   LocalCAD.RepaintViewports;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePage.TabIndex of
   0: LocalCADPrg.Viewport3D := LocalPerspView;
   1: LocalCADPrg.Viewport3D := LocalOrtoView;
  end;
  SetInfoPanel;
  if ShowGrdChk.Checked then
   LocalCADPrg.Viewport3D.DrawObject3D(fGridObject, False);
  LocalCADPrg.Viewport3D.Refresh;
end;

procedure TMainForm.MoveBtnClick(Sender: TObject);
var
  TmpParam: TCAD3DSelectObjectsParam;
begin
  TmpParam := TCAD3DSelectObjectsParam.Create(6, TCAD3DMoveSelectedObjects, 0);
  TmpParam.OnObjectSelected := OnSelectObject;
  with LocalCADPrg do
   StartOperation(TCAD3DExtendedSelectObjects, TmpParam);
end;

procedure TMainForm.RotateBtnClick(Sender: TObject);
var
  TmpParam: TCAD3DSelectObjectsParam;
begin
  TmpParam := TCAD3DSelectObjectsParam.Create(6, TCAD3DRotateOnWPSelectedObjects, 0);
  TmpParam.OnObjectSelected := OnSelectObject;
  with LocalCADPrg do
   StartOperation(TCAD3DExtendedSelectObjects, TmpParam);
end;

procedure TMainForm.ScaleBtnClick(Sender: TObject);
var
  TmpParam: TCAD3DSelectObjectsParam;
begin
  TmpParam := TCAD3DSelectObjectsParam.Create(6, TCAD3DScaleSelectedObjects, 0);
  TmpParam.OnObjectSelected := OnSelectObject;
  with LocalCADPrg do
   StartOperation(TCAD3DExtendedSelectObjects, TmpParam);
end;

procedure TMainForm.FaceBtnClick(Sender: TObject);
begin
  with LocalCADPrg do
   StartOperation(TCAD3DDrawUnSizedPrimitive,
     TCAD3DDrawUnSizedPrimitiveParam.Create(nil, TPlanarFace3D.Create(-1, WorkingPlaneOrigin,
        WorkingPlaneXDir, WorkingPlaneUP, [WorkingPlaneOrigin]), 0, True));
end;

procedure TMainForm.PolyExtrBtnClick(Sender: TObject);
var
  TmpExtr: TExtrudedOutline3D;
begin
  with LocalCADPrg do
   begin
     TmpExtr := TExtrudedOutline3D.Create(-1,
        TPlanarFace3D.Create(-1, WorkingPlaneOrigin, WorkingPlaneXDir, WorkingPlaneUP, [WorkingPlaneOrigin]),
        10, WorkingPlaneNormal);
     StartOperation(TCAD3DDrawUnSizedPrimitive,
       TCAD3DDrawUnSizedPrimitiveParam.Create(nil, TmpExtr, 0, True));
   end;
end;

procedure TMainForm.CuboBtnClick(Sender: TObject);
var
  TmpExtr: TExtrudedOutline3D;
begin
  with LocalCADPrg do
   begin
     TmpExtr := TExtrudedOutline3D.Create(-1,
        TFrame3D.Create(-1, WorkingPlaneOrigin, WorkingPlaneXDir, WorkingPlaneUP, WorkingPlaneOrigin, WorkingPlaneOrigin),
        10, WorkingPlaneNormal);
     StartOperation(TCAD3DDrawSizedPrimitive,
       TCAD3DDrawSizedPrimitiveParam.Create(nil, TmpExtr, 0, False));
   end;
end;

procedure TMainForm.CilBtnClick(Sender: TObject);
var
  TmpExtr: TExtrudedOutline3D;
begin
  with LocalCADPrg do
   begin
     TmpExtr := TExtrudedOutline3D.Create(-1,
        TEllipse3D.Create(-1, WorkingPlaneOrigin, WorkingPlaneXDir, WorkingPlaneUP, WorkingPlaneOrigin, WorkingPlaneOrigin),
        10, WorkingPlaneNormal);
     StartOperation(TCAD3DDrawSizedPrimitive,
       TCAD3DDrawSizedPrimitiveParam.Create(nil, TmpExtr, 0, False));
   end;
end;

procedure TMainForm.SplineExtrBtnClick(Sender: TObject);
var
  TmpExtr: TExtrudedOutline3D;
begin
  with LocalCADPrg do
   begin
     TmpExtr := TExtrudedOutline3D.Create(-1,
        TPlanarSpline3D.Create(-1, WorkingPlaneOrigin, WorkingPlaneXDir, WorkingPlaneUP, [WorkingPlaneOrigin]),
        10, WorkingPlaneNormal);
     StartOperation(TCAD3DDrawUnSizedPrimitive,
       TCAD3DDrawUnSizedPrimitiveParam.Create(nil, TmpExtr, 0, True));
   end;
end;

procedure TMainForm.SphereBtnClick(Sender: TObject);
var
  TmpSph: TSphere3D;
begin
  with LocalCADPrg do
   begin
     TmpSph := TSphere3D.Create(-1, WorkingPlaneOrigin, WorkingPlaneOrigin, 10);
     StartOperation(TCAD3DDrawSizedPrimitive,
       TCAD3DDrawSizedPrimitiveParam.Create(nil, TmpSph, 0, False));
   end;
end;

procedure TMainForm.NewBtnClick(Sender: TObject);
begin
  LocalCAD.DeleteAllObjects;
  LocalCAD.RepaintViewports;
end;

procedure TMainForm.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
   LocalCAD.LoadFromFile(OpenDialog1.FileName);
end;

procedure TMainForm.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
   LocalCAD.SaveToFile(SaveDialog1.FileName);
end;

procedure TMainForm.ImportBtnClick(Sender: TObject);
var
  DXFImp: TDXF3DImport;
begin
  if OpenDialog2.Execute then
   begin
     DXFImp := TDXF3DImport.Create(OpenDialog2.FileName, LocalCAD);
     try
       DXFImp.SetTextFont(CADSysFindFontByIndex(0));
       DXFImp.ReadDXF;
     finally
       DXFImp.Free;
     end;
     LocalCAD.RepaintViewports;
   end;
end;

procedure TMainForm.ZoomInBtnClick(Sender: TObject);
begin
  LocalCADPrg.Viewport3D.ZoomIn;
end;

procedure TMainForm.ZoomOutBtnClick(Sender: TObject);
begin
  LocalCADPrg.Viewport3D.ZoomOut;
end;

procedure TMainForm.ZoomAllBtnClick(Sender: TObject);
begin
  LocalCADPrg.Viewport3D.ZoomToExtension;
end;

procedure TMainForm.PanBtnClick(Sender: TObject);
begin
  LocalCADPrg.SuspendOperation(TCADPrgRealTimePan, nil);
end;

procedure TMainForm.EditPrimBtnClick(Sender: TObject);
var
  TmpParam: TCAD3DSelectObjectsParam;
begin
  TmpParam := TCAD3DSelectObjectsParam.Create(6, TCAD3DEditSelectedObject, 0);
  with LocalCADPrg do
   StartOperation(TCAD3DSelectObject, TmpParam);
end;

procedure TMainForm.DollyBtnClick(Sender: TObject);
begin
  LocalCADPrg.SuspendOperation(TCAD3DDeeptDolly,
                             TCAD3DDeeptDollyParam.Create(LocalCADPrg.Viewport3D.CameraProjectionPlanePosition,
                                LocalCADPrg.Viewport3D.CameraViewPoint));
end;

procedure TMainForm.OrbitBtnClick(Sender: TObject);
begin
  LocalCADPrg.SuspendOperation(TCAD3DRotationalDolly,
                               TCAD3DRotationalDollyParam.Create(LocalCADPrg.Viewport3D.CameraProjectionPlanePosition,
                                  LocalCADPrg.Viewport3D.CameraViewPoint));
end;

initialization
  //CADSysRegisterFontFromFile(0, 'C:/Sviluppo/Lazarus/projects/CADSys42_3D/Demos/SimpleCAD3D/verdana.fnt');
end.
