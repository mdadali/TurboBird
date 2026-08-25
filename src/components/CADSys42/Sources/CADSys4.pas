{: This help file explain all the classes and functions defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes and functions are defined in the CADSys4 unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types and functions of the library.
   For the forms there is no need to manually add CADSys4 if you
   place the controls of the library on them.

   <See=Classes@CS4_CLASSLIST><BR>
   <See=Types@CS4_TYPELIST><BR>
   <See=Functions@CS4_FUNCTIONLIST><BR>

}
{:<New topic=MAIN@CADSys 4 library>
   <Center><Image=cadsys4.bmp><Left>

   <Title=CADSys 4.0 v 2.0>
   <H1=This help file was created by HelpCreator by Piero Valagussa,
   using an automatic in-source documentation extractor for Delphi.>

   <B=Units><BR>
   <See Unit=CadSys4><BR>
   <See Unit=CS4BaseTypes><BR>
   <See Unit=CS4Shapes><BR>
   <See Unit=CS4Tasks><BR>

   This is the help file for the CADSys 4.0 library, a complete
   2D/3D vectorial graphic library in floating point precision.
   By using the library you can create exciting and powerful
   CAD-like interfaces to your programs.

   So you can present application data to the user in a more
   abstract ways like flow-charts, virtual enviroment, command
   and control and so on.
   The flexibility of the library let you to define new shapes
   and interactive operations with a little effort (compared to
   create a new library from scratch).

   The library cames with all the sources of the controls, components,
   graphical objects and tasks, and only need a separate DLL (CADSys4.dll)
   to work.
   This is not a library that do all the things for you, you have
   to write simple Object Pascal code in order to create the
   CAD-like interface. As you may seen from the accompaning examples,
   however, this is not a difficult thing to do.

   I hope the library will fit your requirements but if you
   have suggestion or bugs reports please contact me at the
   address <Code=pivalag@@tin.it>.

   <BOX=<I=Piero Valagussa owns this help file and the library itself, and
   retains all the rights.>.>

}
{:<New topic=CS4_CLASSLIST@List of classes>
  These are the classes defined in the CADSys4 unit:

  <See Class=ECADSysException><BR>
  <See Class=ECADOutOfBound><BR>
  <See Class=ECADObjClassNotFound><BR>
  <See Class=ECADSourceBlockIsReferenced><BR>
  <See Class=ECADFileNotValid><BR>
  <See Class=ECADListObjNotFound><BR>
  <See Class=ECADListBlocked><BR>
  <See Class=TPointsSet2D><BR>
  <See Class=TPointsSet3D><BR>
  <See Class=TGraphicObject><BR>
  <See Class=TGraphicObjectClass class of TGraphicObject><BR>
  <See Class=TCADSysSynchroObject><BR>
  <See Class=TCADSysCriticalSection><BR>
  <See Class=TGraphicObjIterator><BR>
  <See Class=TExclusiveGraphicObjIterator><BR>
  <See Class=TGraphicObjList><BR>
  <See Class=TIndexedObjectList><BR>
  <See Class=TLayer><BR>
  <See Class=TLayers><BR>
  <See Class=TCADCmp><BR>
  <See Class=TCADViewport><BR>
  <See Class=TRuler><BR>
  <See Class=TObject2DHandler><BR>
  <See Class=TObject2DHandlerClass class of TObject2DHandler><BR>
  <See Class=TObject2D><BR>
  <See Class=TObject2DClass class of TObject2D><BR>
  <See Class=TContainer2D><BR>
  <See Class=TSourceBlock2D><BR>
  <See Class=TBlock2D><BR>
  <See Class=TCADCmp2D><BR>
  <See Class=TCADViewport2D><BR>
  <See Class=IRenderObject3D><BR>
  <See Class=TObject3DHandler><BR>
  <See Class=TObject3DHandlerClass class of TObject3DHandler><BR>
  <See Class=TObject3D><BR>
  <See Class=TObject3DClass class of TObject3D><BR>
  <See Class=TContainer3D><BR>
  <See Class=TSourceBlock3D><BR>
  <See Class=TBlock3D><BR>
  <See Class=TCADCmp3D><BR>
  <See Class=TCAD3DAxis><BR>
  <See Class=TCADViewport3D><BR>
  <See Class=TCADParallelViewport3D><BR>
  <See Class=TCADOrtogonalViewport3D><BR>
  <See Class=TCADPerspectiveViewport3D><BR>
  <See Class=TCADStateClass class of TCADState><BR>
  <See Class=TCADPrgParam><BR>
  <See Class=TCADState><BR>
  <See Class=TCADIdleState><BR>
  <See Class=TCADPrg><BR>
  <See Class=TCADStateClass2D class of TCADState2D><BR>
  <See Class=TCADState2D><BR>
  <See Class=TCADPrg2D><BR>
  <See Class=TCADStateClass3D class of TCADState3D><BR>
  <See Class=TCADState3D><BR>
  <See Class=TCADPrg3D><BR>

  These are the classes defined in the CS4BaseTypes unit:

  <See Class=TDecorativePen><BR>
  <See Class=TDecorativeCanvas><BR>

  These are the classes defined in the CS4Shapes unit:

  <See Class=TExtendedFont><BR>
  <See Class=TPrimitive2DClass class of TPrimitive2D><BR>
  <See Class=TPrimitive2DHandler><BR>
  <See Class=TPrimitive2D><BR>
  <See Class=TLine2D><BR>
  <See Class=TOutline2D><BR>
  <See Class=TPolyline2D><BR>
  <See Class=TPolygon2D><BR>
  <See Class=TCurve2D><BR>
  <See Class=TFrame2D><BR>
  <See Class=TRectangle2D><BR>
  <See Class=TArc2D><BR>
  <See Class=TEllipse2D><BR>
  <See Class=TFilledEllipse2D><BR>
  <See Class=TBSpline2D><BR>
  <See Class=TText2D><BR>
  <See Class=TBitmap2D><BR>
  <See Class=TVectChar><BR>
  <See Class=TVectFont><BR>
  <See Class=TJustifiedVectText2D><BR>
  <See Class=TPlanarObject3D><BR>
  <See Class=TPlanar2DObject3D><BR>
  <See Class=TPlanarFieldGrid3D><BR>
  <See Class=TPrimitive3DHandler><BR>
  <See Class=TPrimitive3D><BR>
  <See Class=TLine3D><BR>
  <See Class=TOutline3D><BR>
  <See Class=TPolyline3D><BR>
  <See Class=TFace3D><BR>
  <See Class=TPlanarPrimitive3D><BR>
  <See Class=TJustifiedVectText3D><BR>
  <See Class=TPlanarOutline3D><BR>
  <See Class=TPlanarPolyline3D><BR>
  <See Class=TPlanarFace3D><BR>
  <See Class=TCurve3D><BR>
  <See Class=TPlanarCurve3D><BR>
  <See Class=TFrame3D><BR>
  <See Class=TArc3D><BR>
  <See Class=TEllipse3D><BR>
  <See Class=TPlanarSpline3D><BR>
  <See Class=TMesh3D><BR>
  <See Class=TQuadEdge3D><BR>
  <See Class=TPolyface3D><BR>
  <See Class=TPolyface3DClass class of TPolyface3D><BR>
  <See Class=TSweepedOutline3D><BR>
  <See Class=TExtrudedOutline3D><BR>
  <See Class=TRotationalOutline3D><BR>
  <See Class=TCameraObject3D><BR>

  These are the classes defined in the CS4Tasks unit:

  <See Class=TCADPrgZoomParam><BR>
  <See Class=TCADPrgZoomState><BR>
  <See Class=TCADPrgZoomArea><BR>
  <See Class=TCADPrgZoomInOut><BR>
  <See Class=TCADPrgPan><BR>
  <See Class=TCADPrgRealTimePan><BR>
  <See Class=TCADPrgSelectionState><BR>
  <See Class=TCADPrgSelectAreaParam><BR>
  <See Class=TCADPrgSelectArea><BR>
  <See Class=TCAD2DCommonParam><BR>
  <See Class=TCAD2DPositionObjectParam><BR>
  <See Class=TCAD2DDrawUnSizedPrimitiveParam><BR>
  <See Class=TCAD2DDrawSizedPrimitiveParam><BR>
  <See Class=TCAD2DPositionObject><BR>
  <See Class=TCAD2DDrawSizedPrimitive><BR>
  <See Class=TCAD2DDrawUnSizedPrimitive><BR>
  <See Class=TCAD2DDrawArcPrimitiveParam><BR>
  <See Class=TCAD2DDrawArcPrimitive><BR>
  <See Class=TCAD2DSelectObjectsParam><BR>
  <See Class=TCAD2DSelectObject><BR>
  <See Class=TCAD2DSelectObjects><BR>
  <See Class=TCAD2DExtendedSelectObjects><BR>
  <See Class=TCAD2DSelectObjectsInAreaParam><BR>
  <See Class=TCAD2DSelectObjectsInArea><BR>
  <See Class=TCAD2DTransformObjectsParam><BR>
  <See Class=TCAD2DMoveObjectsParam><BR>
  <See Class=TCAD2DRotateObjectsParam><BR>
  <See Class=TCAD2DTransformObjects><BR>
  <See Class=TCAD2DMoveSelectedObjects><BR>
  <See Class=TCAD2DRotateSelectedObjects><BR>
  <See Class=TCAD2DEditPrimitive><BR>
  <See Class=TCAD2DEditSelectedPrimitive><BR>
  <See Class=TCAD3DCommonParam><BR>
  <See Class=TCAD3DSelectSegmentParam><BR>
  <See Class=TCAD3DSelectSegment><BR>
  <See Class=TCAD3DRotationalDollyParam><BR>
  <See Class=TCAD3DRotationalDolly><BR>
  <See Class=TCAD3DPositionalDollyParam><BR>
  <See Class=TCAD3DPositionalDolly><BR>
  <See Class=TCAD3DDeeptDollyParam><BR>
  <See Class=TCAD3DDeeptDolly><BR>
  <See Class=TCAD3DPositionObjectParam><BR>
  <See Class=TCAD3DDrawUnSizedPrimitiveParam><BR>
  <See Class=TCAD3DDrawSizedPrimitiveParam><BR>
  <See Class=TCAD3DPositionObject><BR>
  <See Class=TCAD3DDrawSizedPrimitive><BR>
  <See Class=TCAD3DDrawUnSizedPrimitive><BR>
  <See Class=TCAD3DDrawArcPrimitiveParam><BR>
  <See Class=TCAD3DDrawArcPrimitive><BR>
  <See Class=TCAD3DSelectObjectsParam><BR>
  <See Class=TCAD3DSelectObject><BR>
  <See Class=TCAD3DSelectObjects><BR>
  <See Class=TCAD3DExtendedSelectObjects><BR>
  <See Class=TCAD3DSelectObjectsInAreaParam><BR>
  <See Class=TCAD3DSelectObjectsInArea><BR>
  <See Class=TCAD3DTransformObjectsParamClass class of TCAD3DTransformObjectsParam><BR>
  <See Class=TCAD3DTransformObjectsParam><BR>
  <See Class=TCAD3DMoveObjectsParam><BR>
  <See Class=TCAD3DRotateOnXObjectsParam><BR>
  <See Class=TCAD3DRotateOnYObjectsParam><BR>
  <See Class=TCAD3DRotateOnZObjectsParam><BR>
  <See Class=TCAD3DRotateOnPlaneObjectsParam><BR>
  <See Class=TCAD3DScaleObjectsParam><BR>
  <See Class=TCAD3DTransformObjects><BR>
  <See Class=TCAD3DMoveSelectedObjects><BR>
  <See Class=TCAD3DRotateOnXSelectedObjects><BR>
  <See Class=TCAD3DRotateOnYSelectedObjects><BR>
  <See Class=TCAD3DRotateOnZSelectedObjects><BR>
  <See Class=TCAD3DRotateOnWPSelectedObjects><BR>
  <See Class=TCAD3DScaleSelectedObjects><BR>
  <See Class=TCAD3DEditPrimitiveParam><BR>
  <See Class=TCAD3DEditPrimitive><BR>
  <See Class=TCAD3DEditSelectedObject><BR>

  These are the classes defined in the CS4DXFModule unit:

  <See Class=TDXFRead><BR>
  <See Class=TDXFWrite><BR>
  <See Class=TDXF2DImport><BR>
  <See Class=TDXF2DExport><BR>
  <See Class=TDXF3DImport><BR>

}
{:<New topic=CS4_TYPELIST@List of types>
  These are the types defined in the CADSys4 unit:

  <See Type=TCADVersion><BR>
  <See Type=TSourceBlockName><BR>
  <See Type=TCanvasCopyMode><BR>
  <See Type=TCanvasCopyView><BR>
  <See Type=TGroupMode><BR>
  <See Type=TLayerName><BR>
  <See Type=TRulerOrientationType><BR>
  <See Type=TOrtoViewType><BR>
  <See Type=TOrtoDirectionType><BR>
  <See Type=TStreamType><BR>
  <See Type=TOnChangePointsSet><BR>
  <See Type=TAddObjectEvent><BR>
  <See Type=TBadVersionEvent><BR>
  <See Type=TBadVersionExEvent><BR>
  <See Type=TOnLoadProgress><BR>
  <See Type=TOnSaveProgress><BR>
  <See Type=TMouseEvent2D><BR>
  <See Type=TMouseMoveEvent2D><BR>
  <See Type=TMouseEvent3D><BR>
  <See Type=TMouseMoveEvent3D><BR>
  <See Type=TClearCanvas><BR>
  <See Type=TCADPrgEvent><BR>
  <See Type=TCS4MouseButton><BR>
  <See Type=TCADPrgOnChangeState><BR>
  <See Type=TCADPrgOnOperation><BR>
  <See Type=TMouse2DButtonFilter><BR>
  <See Type=TMouse2DMoveFilter><BR>
  <See Type=TCADPrg2DSnapFilter><BR>
  <See Type=TMouse3DButtonFilter><BR>
  <See Type=TMouse3DMoveFilter><BR>
  <See Type=TCADPrg3DSnapFilter><BR>

  These are the types defined in the CS4BaseTypes unit:

  <See Type=TRealType><BR>
  <See Type=TClipCode><BR>
  <See Type=TClipResult><BR>
  <See Type=TOutPos><BR>
  <See Type=TOutCode><BR>
  <See Type=TPoint2D><BR>
  <See Type=TVector2D><BR>
  <See Type=TRect2D><BR>
  <See Type=TTransf2D><BR>
  <See Type=THandleRule><BR>
  <See Type=TPoint3D><BR>
  <See Type=TVector3D><BR>
  <See Type=TRect3D><BR>
  <See Type=TTransf3D><BR>
  <See Type=TVectPoints2D><BR>
  <See Type=PVectPoints2D><BR>
  <See Type=TVectPoints3D><BR>
  <See Type=PVectPoints3D><BR>

  These are the types defined in the CS4Shapes unit:

  <See Type=TFaceName><BR>
  <See Type=TPrimitiveSavingType><BR>
  <See Type=TArcDirection><BR>
  <See Type=THJustification><BR>
  <See Type=TVJustification><BR>

  These are the types defined in the CS4Tasks unit:

  <See Type=TSelection2DEvent><BR>
  <See Type=TSelection3DEvent><BR>
  <See Type=TCAD3DEditPrimMode><BR>

  These are the types defined in the CS4DXFModule unit:

  <See Type=TSections><BR>
  <See Type=TGroupTable><BR>
  <See Type=EDXFException><BR>
  <See Type=EDXFEndOfFile><BR>
  <See Type=EDXFInvalidDXF><BR>

}
{:<New topic=CS4_FUNCTIONLIST@List of functions/procedures>
  These are the functions defined in the CADSys4 unit:

  <See Procedure=MakeOrto2D><BR>
  <See Procedure=MakeOrto3D><BR>
  <See Procedure=SetHandleRuleForNormals><BR>
  <See Procedure=ViewPlane><BR>
  <See Procedure=NormalPtPlaneToEq3D><BR>
  <See Procedure=PlaneEqToNormalPt3D><BR>
  <See Procedure=Draw2DSubSetAsPolygon><BR>
  <See Procedure=Draw2DSubSetAsPolyline><BR>
  <See Procedure=DrawPlaceHolder><BR>
  <See Procedure=DrawRoundPlaceHolder><BR>
  <See Procedure=DrawRect2DAsPolyline><BR>
  <See Procedure=DrawRect2DAsPolygon><BR>
  <See Procedure=DrawBoundingBox2D><BR>
  <See Procedure=Draw3DSubSetAsPolyline><BR>
  <See Procedure=DrawBoundingBox3D><BR>
  <See Procedure=CADSysInitClassRegister><BR>
  <See Procedure=CADSysRegisterClass><BR>
  <See Procedure=CADSysUnregisterClass><BR>
  <See Procedure=Register><BR>
  <See Function=DegToRad><BR>
  <See Function=RadToDeg><BR>
  <See Function=HSVToRGB><BR>
  <See Function=Point2D><BR>
  <See Function=Rect2D><BR>
  <See Function=Versor2D><BR>
  <See Function=NormalizeVector2D><BR>
  <See Function=Direction2D><BR>
  <See Function=Vector2D><BR>
  <See Function=VectorLength2D><BR>
  <See Function=IsSamePoint2D><BR>
  <See Function=IsSameVector2D><BR>
  <See Function=IsSameTransform2D><BR>
  <See Function=IsCartesianTransform2D><BR>
  <See Function=CartesianPoint2D><BR>
  <See Function=CartesianRect2D><BR>
  <See Function=ReorderRect2D><BR>
  <See Function=Point2DToPoint><BR>
  <See Function=PointToPoint2D><BR>
  <See Function=RectToRect2D><BR>
  <See Function=Rect2DToRect><BR>
  <See Function=EnlargeBoxPerc2D><BR>
  <See Function=EnlargeBoxDelta2D><BR>
  <See Function=MultiplyTransform2D><BR>
  <See Function=InvertTransform2D><BR>
  <See Function=TransformPoint2D><BR>
  <See Function=TransformVector2D><BR>
  <See Function=TransformRect2D><BR>
  <See Function=TransformBoundingBox2D><BR>
  <See Function=Translate2D><BR>
  <See Function=Rotate2D><BR>
  <See Function=Scale2D><BR>
  <See Function=GetVisualTransform2D><BR>
  <See Function=DotProduct2D><BR>
  <See Function=Perpendicular2D><BR>
  <See Function=Reflect2D><BR>
  <See Function=ClipLine2D><BR>
  <See Function=ClipLineLeftRight2D><BR>
  <See Function=ClipLineUpBottom2D><BR>
  <See Function=IsPointOnSegment2D><BR>
  <See Function=PointLineDistance2D><BR>
  <See Function=PointDistance2D><BR>
  <See Function=NearPoint2D><BR>
  <See Function=IsPointOnPolyLine2D><BR>
  <See Function=IsPointInPolygon2D><BR>
  <See Function=IsPointOnLine2D><BR>
  <See Function=IsPointOnRect2D><BR>
  <See Function=IsPointInCartesianBox2D><BR>
  <See Function=IsPointInBox2D><BR>
  <See Function=PointOutBox2D><BR>
  <See Function=IsBoxInBox2D><BR>
  <See Function=IsBoxAllInBox2D><BR>
  <See Function=IsBoxAllInCartesianBox2D><BR>
  <See Function=BoxOutBox2D><BR>
  <See Function=BoxFillingCartesian2D><BR>
  <See Function=Point3D><BR>
  <See Function=Rect3D><BR>
  <See Function=NormalizeVector3D><BR>
  <See Function=Versor3D><BR>
  <See Function=Direction3D><BR>
  <See Function=Vector3D><BR>
  <See Function=RowVector3D><BR>
  <See Function=ColumnVector3D><BR>
  <See Function=VectorLength3D><BR>
  <See Function=IsSamePoint3D><BR>
  <See Function=IsSameVector3D><BR>
  <See Function=IsSameTransform3D><BR>
  <See Function=IsCartesianTransform3D><BR>
  <See Function=CartesianPoint3D><BR>
  <See Function=CartesianRect3D><BR>
  <See Function=ReOrderRect3D><BR>
  <See Function=Point3DToPoint2D><BR>
  <See Function=Point2DToPoint3D><BR>
  <See Function=Rect3DToRect2D><BR>
  <See Function=Rect2DToRect3D><BR>
  <See Function=EnlargeBoxPerc3D><BR>
  <See Function=EnlargeBoxDelta3D><BR>
  <See Function=MultiplyTransform3D><BR>
  <See Function=InvertTransform3D><BR>
  <See Function=TransformPoint3D><BR>
  <See Function=TransformVector3D><BR>
  <See Function=TransformNormalVector3D><BR>
  <See Function=TransformRect3D><BR>
  <See Function=TransformBoundingBox3D><BR>
  <See Function=Translate3D><BR>
  <See Function=Rotate3DX><BR>
  <See Function=Rotate3DY><BR>
  <See Function=Rotate3DZ><BR>
  <See Function=RotateOnAxis3D><BR>
  <See Function=Scale3D><BR>
  <See Function=ViewOrientationTransform3D><BR>
  <See Function=ParallelViewNormalization3D><BR>
  <See Function=PerspectiveViewNormalization3D><BR>
  <See Function=Reflect3D><BR>
  <See Function=CrossProd3D><BR>
  <See Function=DotProduct3D><BR>
  <See Function=PointDistance3D><BR>
  <See Function=GetHandleRuleForNormals><BR>
  <See Function=GetVectNormal><BR>
  <See Function=ExtrudePoint3D><BR>
  <See Function=WorldPointToPlane3D><BR>
  <See Function=PlanePointToWorld3D><BR>
  <See Function=CastPointOnPlane3D><BR>
  <See Function=CastVectorOnPlane3D><BR>
  <See Function=ClipLine3D><BR>
  <See Function=NearPoint3D><BR>
  <See Function=IsPointOnPolyLine3D><BR>
  <See Function=IsPointOnLine3D><BR>
  <See Function=IsPointInBox3D><BR>
  <See Function=PointOutBox3D><BR>
  <See Function=IsBoxInBox3D><BR>
  <See Function=IsBoxAllInBox3D><BR>
  <See Function=BoxOutBox3D><BR>
  <See Function=BoxFilling3D><BR>
  <See Function=GetBox2DExtension3D><BR>
  <See Function=IsPointInBoxNRC3D><BR>
  <See Function=IsVisibleBoxNRC3D><BR>
  <See Function=IsBoxAllInBoxNRC3D><BR>
  <See Function=IsBoxAllInFrontNRC3D><BR>
  <See Function=BoxFillingNRC3D><BR>
  <See Function=DrawLine2D><BR>
  <See Function=DrawLine3D><BR>
  <See Function=CADSysFindClassIndex><BR>
  <See Function=CADSysFindClassByName><BR>
  <See Function=CADSysFindClassByIndex><BR>
  <See Function=StringToBlockName><BR>

  These are the functions defined in the CS4Shapes unit:

  <See Procedure=SetCamerasViewport><BR>
  <See Procedure=CADSysSetDefaultFont><BR>
  <See Procedure=CADSysInitFontList><BR>
  <See Procedure=CADSysClearFontList><BR>
  <See Procedure=CADSysRegisterFontFromFile><BR>
  <See Procedure=CADSysRegisterFont><BR>
  <See Procedure=CADSysUnregisterFont><BR>
  <See Function=CADSysGetDefaultFont><BR>
  <See Function=CADSysFindFontIndex><BR>
  <See Function=CADSysFindFontByIndex><BR>
  
}
Unit CADSys4;

{$mode delphi}

Interface

uses Math, SysUtils, Classes, LCLType, Graphics, Controls, ClipBrd, ComCtrls,
  CS4BaseTypes, LMessages, LCLIntf;

type
{: This type is used by the library for versioning control.
   You doesn't need to change it but you may want to use it
   if you are defining new shapes classes and want to make them
   version indipendent.

   At the moment the version of the library is 'CAD423'.
}
  TCADVersion = array[1..6] of Char;
{: This type define the general type for source block names.
}
  TSourceBlockName = array[0..12] of Char;
{: This type defines the two modality used by <See Class=TCADViewport> to
   copy the viewport drawing area to another canvas with the
   <See Method=TCADViewport@CopyToCanvas> and
   <See Method=TCADViewport@CopyRectToCanvas>.

   These are the modes avaiable:

   <LI=<I=cmNone> ignore the aspect ratio setting.>
   <LI=<I=cmAspect> will use th aspect ratio setting for the copy.>
   <Par>
   <RedBox=<B=Changed>: This type was the TCopyingMode type in the
   old library>.
}
  TCanvasCopyMode =(cmNone, cmAspect);
{: This type defines the modes that can be used by <See Class=TCADViewport> to
   copy the viewport drawing area to another canvas with the
   <See Method=TCADViewport@CopyToCanvas> and
   <See Method=TCADViewport@CopyRectToCanvas>.

   These are the modes avaiable:

   <LI=<I=cvActual> copy the current contents of the viewport.>
   <LI=<I=cvExtension> copy the whole draw.>
   <LI=<I=cvScale> scale the current contents with the given scale factors.>
   <Par>
   <RedBox=<B=Changed>: This type was the TCopyingView type in the
   old library>.
}
  TCanvasCopyView =(cvActual, cvExtension, cvScale);
{: This type defines the modes used to collect a set of objects using the
   <See Method=TCADViewport2D@GroupObjects> and
   <See Method=TCADViewport3D@GroupObjects> methods.

   These are the modes avaiable:

   <LI=<I=gmAllInside> all the objects wholy inside the area are collected.>
   <LI=<I=gmCrossFrame> all the objects wholy or partially inside the area are collected.>
}
  TGroupMode = (gmAllInside, gmCrossFrame);
{: This type defines the string used to name a layer.
}
  TLayerName = String[31];
{: This type defines the possible orientations for a <See Class=TRuler>
   control.

   There are two types of orientations:

   <LI=<I=otOrizontal> the ruler is alligned horizontally.>
   <LI=<I=otVertical> the ruler is alligned vertically.>

   The horizontal ruler is drawed on the left side of the linked
   Viewport and the vertical ruler is drawed on the down side of
   the linked Viewport.
}
  TRulerOrientationType = (otOrizontal, otVertical);
{: This type defines which principal axis is used in a
   <See Class=TCADOrtogonalViewport3D> control.

   The possible combinations are:

   <LI=<I=vtFront> for a view plane with normal along the
     positive (if <See Property=TCADOrtogonalViewport3D@Direction>
     is <I=dtAhead>) or negative Y axis>
   <LI=<I=vtSide> for a view plane with normal along the
     positive (if <See Property=TCADOrtogonalViewport3D@Direction>
     is <I=dtAhead>) or negative X axis>
   <LI=<I=vtFront> for a view plane with normal along the
     positive (if <See Property=TCADOrtogonalViewport3D@Direction>
     is <I=dtAhead>) or negative Z axis>
}
  TOrtoViewType = (vtFront, vtSide, vtTop);
{: This type defines the direction of axis that is used in a
   <See Class=TCADOrtogonalViewport3D> control as the view plane
   normal.

   The possible combinations are:

   <LI=<I=vtFront> for a view plane with normal along the
     positive (if <See Property=TCADOrtogonalViewport3D@Direction>
     is <I=dtAhead>) or negative Y axis>
   <LI=<I=vtSide> for a view plane with normal along the
     positive (if <See Property=TCADOrtogonalViewport3D@Direction>
     is <I=dtAhead>) or negative X axis>
   <LI=<I=vtFront> for a view plane with normal along the
     positive (if <See Property=TCADOrtogonalViewport3D@Direction>
     is <I=dtAhead>) or negative Z axis>
}
  TOrtoDirectionType = (dtAhead, dtBehind);
{: This type identifies the two types of files used by the library:
   <LI=<I=stDrawing> means a drawing file>
   <LI=<I=stLibrary> means a library file>
}
  TStreamType = (stDrawing, stLibrary);

{: This is the base class from which any exception raised by
   library's functions are derived.

   If you want to define your own exception consider to derive
   it from this class.
}
  ECADSysException = class(Exception);
{: This exception is raised when a <See Class=TPointsSet2D> or <See Class=TPointsSet3D>
  object is accessed with an out of bound index.
}
  ECADOutOfBound = class(ECADSysException);
{: This exception is raised when an attempt is made to use an
   unregistered shape class in a method that need a registered
   one.

   <B=Note>: When this exception is raised add the following code
   in the initialization section of one of your units:

   <Code=
    CADSysRegisterClass(<<unused index>>, <<unregistered class type>>);
   >

   Where <I=unused index> must be an unused index above 150.
}
  ECADObjClassNotFound = class(ECADSysException);
{: This exception is raised when you try to delete an source block
   that has references (that has linked instances of <See Class=TBlock2D>
   or <See Class=TBlock3D>).

   In the case of this error remove all the references to the
   source block before retry to delete it.
}
  ECADSourceBlockIsReferenced = class(ECADSysException);
{: This exception is raised when you try to load an invalid drawing
   in a <See Class=TCADCmp> instance.

   <B=Note> that this exception is not raised when an old drawing
    file is loaded, in which case a <See Class=TBadVersionEvent> and <See Class=TBadVersionExEvent> is fired.
}
  ECADFileNotValid = class(ECADSysException);
{: This exception is raised when a specified object is not found
    in the list.
}
  ECADListObjNotFound = class(ECADSysException);
{: This exception is raised when you ask a
   <See Class=TExclusiveGraphicObjIterator> for a list that has already
   active iterators on it.
   Remove the other iterators before retry.

   <B=Note>: When a list has an active iterators that cannot
    be deleted (because you have lost the reference to it), you
    have to call <See Method=TGraphicObjList@RemoveAllIterators> to remove
    all the pending iterators (but not the memory used by them).
    However use this function with care expecially in a multi thread
    application.
}
  ECADListBlocked = class(ECADSysException);

  TCADCmp = class;
  TCADCmp2D = class;
  TCADViewport = class;
  TCADViewport2D = class;
  TObject2D = class;
  TCADCmp3D = class;
  TCADViewport3D = class;
  TObject3D = class;
  TGraphicObjList = class;
  TGraphicObject = class;

{: This type defines a message handler function that is called
   when a <See Class=TPointsSet2D> object is changed.

   It has as arguments the vector that has called the handler.
}
  TOnChangePointsSet = procedure(Sender: TObject) of Object;
{: This type defines the event that is fired when an object is added to
   a <See Class=TCADCmp> control.

   <I=Sender> is the control in which the object is added;
   <I=Obj> is the object being added.

   It is useful when you want to set some object properties to a default
   value regardless of the procedure that added the object.
}
  TAddObjectEvent = procedure(Sender: TObject; Obj: TGraphicObject) of object;
{: This type defines the event that is fired when a drawing created
   with a previous version is being loaded.

   <I=Sender> is the control that tried to load the file;
   <I=Stream> is the stream opened for the file and <I=Version> is the
   version of the library that created the file.
   <I=StreamType> is the type of stream to read.
   <I=Version> is the version read from stream, may be changed.
   <I=Resume> is a flag that force the library to continue to
   loading of the drawing. If this is set to True, the normal
   loading procedure will start at the exit of the event. At
   the call of the procedure it is set to False.

   If you set an handler for this event (see
   <See Property=TCADCmp@OnInvalidFileVersion>) you have to do all the
   necessary operation to handle the file.
}
  TBadVersionEvent = procedure(Sender: TObject; const StreamType: TStreamType; const Stream: TStream; var Resume: Boolean) of object;
  TBadVersionExEvent = procedure(Sender: TObject; const StreamType: TStreamType; const Stream: TStream; var Version: TCADVersion; var Resume: Boolean) of object;
{: This type defines the event that is fired when an object is loaded from
   a drawing.

   <I=Sender> is the control that is loading the file;
   <I=ReadPercent> is percentual of the drawing loaded so far.

   This event is useful when you want to inform the user of the progress
   of the loading (see <See Property=TCADCmp@OnLoadProgress>).
}
  TOnLoadProgress = procedure(Sender: TObject; ReadPercent: Byte) of object;
{: This type defines the event that is fired when an object is saved to
   a drawing.

   <I=Sender> is the control that is loading the file;
   <I=SavePercent> is percentual of the drawing saved so far.

   This event is useful when you want to inform the user of the progress
   of the saving (see <See Property=TCADCmp@OnSaveProgress>).
}
  TOnSaveProgress = procedure(Sender: TObject; SavedPercent: Byte) of object;
{: This is the type of an event handler for the mouse button event.

   <I=Sender> is the component that has raised the event, <I=Button>
   is the mouse button that has caused the event, <I=Shift> was
   the key configuration when the event was raised.
   <I=WX> and <I=WY> are the X and Y mouse coordinate in the world
   coordinate system ans X, Y are the X and Y mouse coordinate in
   the Windows screen coordinate system.
}
  TMouseEvent2D = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer) of Object;
{: This is the type of an event handler for the mouse move event.

   <I=Sender> is the component that has raised the event, <I=Shift> was
   the key configuration when the event was raised.
   <I=WX> and <I=WY> are the X and Y mouse coordinate in the world
   coordinate system ans X, Y are the X and Y mouse coordinate in
   the Windows screen coordinate system.
}
  TMouseMoveEvent2D = procedure(Sender: TObject; Shift: TShiftState; WX, WY: TRealType; X, Y: Integer) of Object;
{: This is the type of the event handler for the mouse button events.

   <I=Sender> is the component that has raised the event, <I=Button>
   is the mouse button that has caused the event, <I=Shift> was
   the key configuration when the event was raised.
   <I=WX>, <I=WY> and <I=WZ> are the X, Y and Z mouse coordinates in
   the world coordinate system ans X, Y are the X and Y mouse coordinates in
   the Windows screen coordinate system.
}
  TMouseEvent3D = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; WX, WY, WZ: TRealType; X, Y: Integer) of Object;
{: This is the type of the event handler for the mouse move event.

   <I=Sender> is the component that has raised the event, <I=Shift> was
   the key configuration when the event was raised.
   <I=WX>, <I=WY> and <I=WZ> are the X, Y and Z mouse coordinates in the world
   coordinate system ans X, Y are the X and Y mouse coordinates in
   the Windows screen coordinate system.
}
  TMouseMoveEvent3D = procedure(Sender: TObject; Shift: TShiftState; WX, WY, WZ: TRealType; X, Y: Integer) of Object;

{: This class defines a set of points that can be drawed as
   a polyline or filled polygon.

   The points in the set are single precision points (<See Class=TPoint2D>)
   that are clipped against a 2D axis aligned rectangle before drawed
   on a Canvas.

   The class has also picking capabilities as well as a method
   to apply a trasform to all the points.

   The may let the vector to grow when new points are added it; also
   the vector can call a user defined function whenever this occour.

   This class is very useful in defining new shapes to be used with
   the library (see also <See Class=TPrimitive2D>, <See Class=TCurve2D>).
}
  TPointsSet2D = class(TObject)
  private
    fPoints: Pointer;
    fCapacity, fCount: Word;
    fGrownEnabled, fDisableEvents: Boolean;
    fOnChange: TOnChangePointsSet;
    fTag: Integer;

    procedure SetDisableEvents(B: Boolean);
    procedure CallOnChange;
    function  GetExtension: TRect2D;
    procedure PutProp(Index: Word; const Item: TPoint2D);
    procedure Expand(const NewCapacity: Integer);
  protected
    {: This method is called whenever a point is requested from the set.

       <I=Index> is the index of the point to be extracted (the set
       is like an array so the inserted points are referred to by a
       numeric index value from 0 to Count - 1).

       The method must return a point or raise a <See Class=ECADOutOfBound>
       exception.
    }
    function  Get(Index: Word): TPoint2D; virtual;
    {: This method is called whenever a point in the set is to be replaced.

       <I=PutIndex> is the index of point where to store
       <I=Item>; <I=ItemIndex> is the index of <I=Item> if
       it is already present in the list (so it will be replaced by
       <I=Item>); <I=Item> is the modified point.

       <B=Note>: You may want to redefine this method if you want
       to customize the set by adding new informations to all the
       points. For instance if you want to add a type specifier to
       all the points you have to override this method and use the
       specified indexes to manage the extra infos. For these
       remember that:

       <LI=<I=PutIndex> is equal to <I=ItemIndex> if
            a point is being replaced with a new one.>
       <LI=<I=ItemIndex>=<I=PutIndex> - 1 if the point
            at <I=PutIndex> is being replaced with the previous
            point in the set (this happens when <See Method=TPointsSet2D@Insert> is
            called)>
       <LI=<I=ItemIndex>=<I=PutIndex> + 1 if the point
            at <I=PutIndex> is being replaced with the next
            point in the set (this happens when <See Method=TPointsSet2D@Delete> is
            called).>
    }
    procedure Put(PutIndex, {%H-}ItemIndex: Word; const Item: TPoint2D); virtual;
  public
    {: This method create a new instance of the class setting its capacity
       to <I=_Capacity>.

       <I=_Capacity> is the initial number of points that can be stored
       in the set. If <See Property=TPointsSet2D@GrowingEnabled> is <B=True>
       you can add more that <I=_Capacity> points to the set, otherwise
       a <See Class=ECADOutOfBound> exception will be raised if
       you try to do so.

       Setting <I=_Capacity> to the real number of points in the set
       will speed up the insertion of the point in the set.
    }
    constructor Create(const _Capacity: Word); virtual;
    {: Free the memory used by the instance of the class.

       Remember to call the <I=Free> method of the class when done
       with the class or memory leak will result.
    }
    destructor Destroy; override;
    {: Clear the set.

       <B=Note> that the memory used by the set is not freed for
       optimization. Only the <See Property=TPointsSet2D@Count> is reset to zero.
    }
    procedure Clear;
    {: Copy a subset of the points from another set. The
       points are copied in the respective positions, so
       the second point of S is copied on the second point
       of Self.

       <I=S> is the source set from which the points will be
       copied; <I=StIdx> and <I=EndIdx> are the start and end
       index of the points in <I=S> to be copied respectively.

       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the points, otherwise only the points that sit into
       the vect are copied and a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure Copy(const S: TPointsSet2D; const StIdx, EndIdx: Integer);
    {: Add an item at the end of the set.

       <I=Item> is the point to be added. The point is added
       after any other point.

       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the point, otherwise a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure Add(const Item: TPoint2D);
    {: Add a set all the points of a set to the current one.

       <I=Items> is the set from which copy the points. The
       points will be added in the same order the have in <I=Items>
       at the end of the current set.

       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the points, otherwise only the points that sit into
       the vect are copied and a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure AddPoints(const Items: array of TPoint2D);
    procedure AddPoints(const SourceSet: TPointsSet2D); overload;
    {: Transform all the points in the set which a transformation matrix.

       <I=T> is the transformation matrix that may be created
       whith the functions <See Function=Translate2D>,
       <See Function=Rotate2D>, <See Function=Scale2D>,
       <See Function=MultiplyTransform2D>,
       <See Function=InvertTransform2D>.

       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.
    }
    procedure TransformPoints(const T: TTransf2D);
    {: Remove a point from the set.

       <I=Index> is the index value of the point to be removed.
       After the copy the <See Property=TPointsSet2D@OnChange> event will
       be fired.

       <B=Note 1>: Removing a point from a set is a time consuming
       operation (or better it may be so if the set is a very large
       one).

       <B=Note 2>: If the <I=Index> is not in the vector a
       <See Class=ECADOutOfBound> exception will be raised.
    }
    procedure Delete(const Index: Word);
    {: Insert a point into the set.

       <I=Index> is the index value at which the point will be
       inserted. All the points in the set from the one next of
       <I=Index> position to the end will be moved by one
       to make space for the new point. <I=Item> is the new point to be
       inserted.

       <B=Note 1>: Insert a point into a set is a time consuming
       operation (or better it may be so if the set is a very large
       one).

       <B=Note 2>: If <See Property=TPointsSet2D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the point, otherwise a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure Insert(const Index: Word; const Item: TPoint2D);
    {: Draw the entire set of the points in the set onto a Canvas as
       a closed polygon filled with the current brush of the Canvas.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.

       The set will be drawed as a closed polyline (it will be
       closed automatically by the method) with the outline drawed
       with the current pen and the interior filled with the
       current brush of the canvas.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
    procedure DrawAsPolygon(const Cnv: TDecorativeCanvas; const Clip, Extent: TRect2D; const S: TTransf2D);
    {: Draw the entire set of the points in the set onto a Canvas
       as a polyline.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.

       The set will be drawed as a polyline (not closed by the
       method) with the outline drawed with the current pen of
       the canvas and not filled.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note 2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
    procedure DrawAsPolyline(const Cnv: TDecorativeCanvas; const Clip, Extent: TRect2D; const S: TTransf2D);
    {: Draw a subset of points onto a Canvas as a closed
       polygon filled with the current brush of the Canvas.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.
       <I=StartIdx> is the index of first point of the set to be
       drawed; <I=EndIdx> is the index of last point of the
       set to be drawed. <I=EndIdx> must be greater than
       <I=StartIdx>.

       The set will be drawed as a closed polyline (it will be
       closed automatically by the method) from the point at
       <I=StartIdx> and the last point at <I=EndIdx>, with the
       outline drawed with the current pen and the interior filled
       with the current brush of the canvas.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
    procedure DrawSubsetAsPolygon(const Cnv: TDecorativeCanvas; const Clip, Extent: TRect2D; const S: TTransf2D; const StartIdx, EndIdx: Integer);
    {: Draw a subset of the points in the set onto a Canvas as a polyline.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Clip> is the clipping rectangle in canvas
       coordinates used to clip the output; <I=Extent> is the
       extension of the set as returned from <See Property=TPointsSet2D@Extension>;
       <I=S> is the viewport transformation as returned from
       <See Property=TCADViewport@ViewportToScreenTransform>.
       <I=StartIdx> is the index of first point of the set to be
       drawed; <I=EndIdx> is the index of last point of the
       set to be drawed. <I=EndIdx> must be greater than
       <I=StartIdx>.

       The set will be drawed as a polyline (not closed by the
       method) from the point at <I=StartIdx> and the last
       point at <I=EndIdx>, with the outline drawed with
       the current pen of the canvas and not filled.

       <B=Note 1>: The <I=Clip> parameter is of type <See Type=TRect2D>
       but it is referred to convas coordinates that are of type <I=TRect>.
       For this you have to use the <See Function=RectToRect2D> function
       to pass the <I=ClipRect> property of the canvas.

       <B=Note2>: The clipping region of the canvas have to be the
       entire canvas extension (that is no user defined clipping region
       must be is set).
    }
    procedure DrawSubsetAsPolyline(const Cnv: TDecorativeCanvas; const Clip, Extent: TRect2D; const S: TTransf2D; const StartIdx, EndIdx: Integer; const ToBeClosed: Boolean);
    {: This property contains the extension of the set, that is
       the smaller axis-alligned rectangle that fully contains the
       points in the set.

       <B=Note>: Because this method compute the extension every
       time it is called you may want to store it in a temporary
       variable if you want to use it in different part of your
       function (obviously if you don't change the set between
       uses of the extension).
    }
    property Extension: TRect2D read GetExtension;
    {: This property contains the points in the set in an array-like
       fashion.

       <I=Index> is the index of the point to be accessed and it
       is zero-based (that is the points in the set have
       indexes 0,1,2,...).

       <B=Note 1>: This is a default property so you can drop the
       name of the property, for instance you may use:

       <Code=AVect[2]>

       instead of:

       <Code=AVect.Points[2]>

       <B=Note 2>: If <I=Index> is greater than
       <See Property=TPointsSet2D@Count> a <See Class=ECADOutOfBound> exception
       will be raised.
    }
    property Points[Index: Word]: TPoint2D read Get write PutProp; default;
    {: This property contains the number of points in the set.

       You may use this property to iterate the points in the set.

       <B=Note>: This property is always less than <See Property=TPointsSet2D@Capacity>.
    }
    property Count: Word read fCount;
    {: This property contains the maximun number of points
       that can be added or inserted into the set without growing
       it (if possible).
    }
    property Capacity: Word read fCapacity;
    {: If this property is set to <B=True> then the
       <See Property=TPointsSet2D@OnChange> event will not be fired when
       the set change.
    }
    property DisableEvents: Boolean read fDisableEvents write SetDisableEvents;
    {: If this property is set to <B=True> then the set can grow
       if you add points when <See Property=TPointsSet2D@Count> is equal to
       <See Property=TPointsSet2D@Capacity>, otherwise a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    property GrowingEnabled: Boolean read fGrownEnabled write fGrownEnabled;
    {: This property contains the points reference, that is a pointer
       to the underling set of points. You will need to use it in the
       functions:
       <See Function=IsPointOnPolyLine2D>
       <See Function=IsPointInPolygon2D>
       <See Function=Draw2DSubSetAsPolygon>
       <See Function=Draw2DSubSetAsPolyline>
    }
    property PointsReference: Pointer read fPoints;
    {: The Tag property for user's information.

       It is not saved.
    }
    property Tag: Integer read fTag write fTag;
    {: EVENTS}
    {: This event is fired whenever the set is changed.

       In the <I=Sender> argument of the event handler the changed
       set is passed.

       <B=Note>: If you change the vector in the event handler no
       new event will be fired.
    }
    property OnChange: TOnChangePointsSet read fOnChange write fOnChange;
  end;

{: This class defines a set of points that can be drawed as
   a polyline (no filling is possible in 3D output of the library).

   The points in the set are single precision points (<See Class=TPoint3D>)
   that are clipped against a 3D normalized view volume in homegeneous
   coordinates. This rect is an axis aligned box from X:-1 to 1,
   Y:-1 to 1, Z:-1 to 0.

   The class has also picking capabilities as well as a method
   to apply a trasform matrix to all the points.

   The may let the vector to grow when new points are added it; also
   the vector can call a user defined function whenever this occour.

   This class is very useful in defining new shapes to be used with
   the library (see also <See Class=TPrimitive3D>, <See Class=TPlanarPrimitive3D>,
   <See Class=TPlanarCurve3D>).
}
  TPointsSet3D = class(TObject)
  private
    fPoints: Pointer;
    fCapacity, fCount: Word;
    fGrownEnabled, fDisableEvents: Boolean;
    fTag: Integer;
    fOnChange: TOnChangePointsSet;

    procedure CallOnChange;
    function  GetExtension: TRect3D;
    procedure PutProp(Index: Word; const Item: TPoint3D);
    procedure Expand(const NewCapacity: Integer);
  protected
    {: This method is called whenever a point is requested from the set.

       <I=Index> is the index of the point to be extracted (the set
       is like an array so the inserted points are referred to by a
       numeric index value from 0 to Count - 1).

       The method must return a point or raise a <See Class=ECADOutOfBound>
       exception.
    }
    function  Get(Index: Word): TPoint3D; virtual;
    {: This method is called whenever a point in the set is to be replaced.

       <I=PutIndex> is the index of point where to store
       <I=Item>; <I=ItemIndex> is the index of <I=Item> if
       it is already present in the list (so it will be replaced by
       <I=Item>); <I=Item> is the modified point.

       <B=Note>: You may want to redefine this method if you want
       to customize the set by adding new informations to all the
       points. For instance if you want to add a type specifier to
       all the points you have to override this method and use the
       specified indexes to manage the extra infos. For these
       remember that:

       <LI=<I=PutIndex> is equal to <I=ItemIndex> if
            a point is being replaced with a new one.>
       <LI=<I=ItemIndex>=<I=PutIndex> - 1 if the point
            at <I=PutIndex> is being replaced with the previous
            point in the set (this happens when <See Method=TPointsSet3D@Insert> is
            called)>
       <LI=<I=ItemIndex>=<I=PutIndex> + 1 if the point
            at <I=PutIndex> is being replaced with the next
            point in the set (this happens when <See Method=TPointsSet3D@Delete> is
            called).>
    }
    procedure Put(PutIndex, {%H-}ItemIndex: Word; const Item: TPoint3D); virtual;
  public
    {: This method create a new instance of the class setting its size
       to <I=_Capacity>.

       <I=_Capacity> is the initial number of points that can be stored
       in the set. If <See Property=TPointsSet3D@GrowingEnabled> is <B=True>
       you can add more that <I=_Capacity> points to the set, otherwise
       a <See Class=ECADOutOfBound> exception will be raised if
       you try to do so.

       Setting <I=_Capacity> to the real number of points in the set
       will speed up the insertion of the point in the set.
    }
    constructor Create(const _Capacity: Word); virtual;
    {: Free the memory used by the instance of the class.

       Remember to call the <I=Free> method of the class when done
       with the class or memory leak will result.
    }
    destructor Destroy; override;
    {: Clear the set.

       <B=Note> that the memory used by the set is not freed for
       optimization. Only the <See Property=TPointsSet3D@Count> is reset to zero.
    }
    procedure Clear;
    {: Copy a subset of the points from another set. The
       points are copied in the respective positions, so
       the second point of S is copied on the second point
       of Self.

       <I=S> is the source set from which the points will be
       copied; <I=StIdx> and <I=EndIdx> are the start and end
       index of the points in <I=S> to be copied respectively.

       After the copy the <See Property=TPointsSet3D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet3D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the points, otherwise only the points that sit into
       the vect are copied and a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure Copy(const S: TPointsSet3D; const StIdx, EndIdx: Integer);
    {: Add an item at the end of the set.

       <I=Item> is the point to be added. The point is added
       after any other point.

       After the copy the <See Property=TPointsSet3D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet3D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the point, otherwise a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure Add(const Item: TPoint3D);
    {: Add all the points of a set to the current one.

       <I=Items> is the set from which copy the points. The
       points will be added in the same order the have in <I=Items>
       at the end of the current set.

       After the copy the <See Property=TPointsSet3D@OnChange> event will
       be fired.

       <B=Note>: If <See Property=TPointsSet3D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the points, otherwise only the points that sit into
       the vect are copied and a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure AddPoints(const Items: array of TPoint3D);
    procedure AddPoints(const SourceSet: TPointsSet3D); overload;
    {: Transform all the points in the set with a transform matrix.

       <I=T> is the transformation matrix that may be created
       whith the functions <See Function=Translate3D>,
       <See Function=Rotate3DX>, <See Function=Rotate3DY>,
       <See Function=Rotate3DZ>, <See Function=Scale3D>,
       <See Function=MultiplyTransform3D>,
       <See Function=InvertTransform3D>.

       After the copy the <See Property=TPointsSet3D@OnChange> event will
       be fired.
    }
    procedure TransformPoints(const T: TTransf3D);
    {: Remove a point from the set.

       <I=Index> is the index value of the point to be removed.
       After the copy the <See Property=TPointsSet3D@OnChange> event will
       be fired.

       <B=Note 1>: Removing a point from a set is a time consuming
       operation (or better it may be so if the set is a very large
       one).

       <B=Note 2>: If the <I=Index> is not in the vector a
       <See Class=ECADOutOfBound> exception will be raised.
    }
    procedure Delete(const Index: Word);
    {: Insert a point into the set.

       <I=Index> is the index value at which the point will be
       inserted. All the points in the set from the one at
       <I=Index> position will be moved by one position to make
       space for the new point. <I=Item> is the new point to be
       inserted.

       <B=Note 1>: Insert a point into a set is a time consuming
       operation (or better it may be so if the set is a very large
       one).

       <B=Note 2>: If <See Property=TPointsSet3D@GrowingEnabled> is
       <B=True> the size of the vector may grow if there is no
       space for the point, otherwise a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    procedure Insert(const Index: Word; const Item: TPoint3D);
    {: Draw the entire set of the points in the set onto a Canvas
       as a polyline.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Extent> is the extension of the set as
       returned from <See Property=TPointsSet3D@Extension>;
       <I=NT> is the normalizing transform of the view as
       returned by <See Property=TCADViewport3D@ViewNormalization> or
       by the functions <See Function=ParallelViewNormalization3D>,
       <See Function=PerspectiveViewNormalization3D> and must include
       the orientation trasform and the normalization transform.
       <I=VT> is the mapping transform from the normalized view rect
       to the Canvas rect as returned by
       <See Property=TCADViewport3D@ViewMapping>.

       The set will be drawed as a polyline (not closed by the
       method) with the outline drawed with the current pen of
       the canvas and not filled.

       <B=Note 1>: The clipping is perfomed in homogeneous
       coordinates.
    }
    procedure DrawAsPolyline(const Cnv: TDecorativeCanvas; const Extent: TRect3D; const NT: TTransf3D; const VT: TTransf2D);
    {: Draw a subset of the points in the set onto a Canvas as a polyline.

       <I=Cnv> is the destination canvas on which the set will
       be drawed <See Class=TDecorativeCanvas@TDecorativeCanvas>; <I=Extent> is the extension of the set as
       returned from <See Property=TPointsSet3D@Extension>;
       <I=NT> is the normalizing transform of the view as
       returned by <See Property=TCADViewport3D@ViewNormalization> or
       by the functions <See Function=ParallelViewNormalization3D>,
       <See Function=PerspectiveViewNormalization3D> and must include
       the orientation trasform and the normalization transform.
       <I=VT> is the mapping transform from the normalized view rect
       to the Canvas rect as returned by
       <See Property=TCADViewport3D@ViewMapping>.
       <I=StartIdx> is the index of first point of the set to be
       drawed; <I=EndIdx> is the index of last point of the
       set to be drawed. <I=EndIdx> must be greater than
       <I=StartIdx>.

       The set will be drawed as a polyline (not closed by the
       method) from the point at <I=StartIdx> and the last
       point at <I=EndIdx>, with the outline drawed with
       the current pen of the canvas and not filled.

       <B=Note 1>: The clipping is perfomed in homogeneous
       coordinates.
    }
    procedure DrawSubSetAsPolyline(const Cnv: TDecorativeCanvas; const Extent: TRect3D; const NT: TTransf3D; const VT: TTransf2D; const StartIdx, EndIdx: Integer; const ToBeClosed: Boolean);
    {: Change the orientation of the polyline.

       This method is useful only when the set of points
       rapresent a planar polyline (or it is a good aproximation
       of a planar polyline).
       The normal of the surface defined by the polyline is
       set to <I=N> and the order of the points is updated to
       be consistent with this normal.
       This order is specified by value returned by <See Function=GetHandleRuleForNormals>
       that can be set with <See Function=SetHandleRuleForNormals>
       (by default its value is hrRightHand).
    }
    procedure FrontFace(const N: TVector3D);
    {: This property contains the extension of the set, that is
       the smaller axis-alligned rectangle that fully contains the
       points in the set.

       <B=Note>: Because this method compute the extension every
       time it is called you may want to store it in a temporary
       variable if you want to use it in different part of your
       function (obviously if you don't change the set between
       uses of the extension).
    }
    property Extension: TRect3D read GetExtension;
    {: This property contains the points in the set in an array-like
       mode.

       <I=Index> is the index of the point to be accessed and it
       is zero-based (that is the points in the set have
       indexes 0,1,2,...).

       <B=Note 1>: This is a default property so you can drop the
       name of the property, for instance you may use:

       <Code=AVect[2]>

       instead of:

       <Code=AVect.Points[2]>

       <B=Note 2>: If <I=Index> is greater than
       <See Property=TPointsSet3D@Count> a <See Class=ECADOutOfBound> exception
       will be raised.
    }
    property Points[Index: Word]: TPoint3D read Get write PutProp; default;
    {: This property contains the number of points in the set.

       You may use this property to iterate the points in the set.

       <B=Note>: This property is always less than <See Property=TPointsSet3D@Capacity>.
    }
    property Count: Word read fCount;
    {: This property contains the maximun number of points
       that can be added or inserted into the set without growing
       it (if possible).
    }
    property Capacity: Word read fCapacity;
    {: If this property is set to <B=True> then the
       <See Property=TPointsSet3D@OnChange> event will not be fired when
       the set change.
    }
    property DisableEvents: Boolean read fDisableEvents write fDisableEvents;
    {: If this property is set to <B=True> then the set can grow
       if you add points when <See Property=TPointsSet3D@Count> is equal to
       <See Property=TPointsSet3D@Capacity>, otherwise a <See Class=ECADOutOfBound>
       exception will be raised if you try to do so.
    }
    property GrowingEnabled: Boolean read fGrownEnabled write fGrownEnabled;
    {: This property contains the points reference, that is a pointer
       to the underling set of points. You will need to use it in the
       functions:
       <See Function=GetVectNormal>
       <See Function=IsPointOnPolyLine3D>
       <See Function=Draw3DSubSetAsPolyline>
    }
    property PointsReference: Pointer read fPoints;
    {: The Tag property for user's information.

       It is not saved.
    }
    property Tag: Integer read fTag write fTag;
    {: EVENTS}
    {: This event is fired whenever the set is changed.

       In the <I=Sender> argument of the event handler the changed
       set is passed.

       <B=Note>: If you change the vector in the event handler no
       new event will be fired.
    }
    property OnChange: TOnChangePointsSet read FOnChange write FOnChange;
  end;

{: This type defines the procedure used by a <See Class=TCADViewport>
   controll to clear the canvas before drawing on it.

   <I=Sender> is the viewport, <I=Cnv> canvas to be cleared,
   <I=ARect> is the rectangle of the viewplane that will be drawed onto
   the canvas and <I=BackCol> is the background color to be used to clear
   the canvas.

   By default the canvas is simply cleared with the background color. This
   procedure is useful to draw a raster image onto which you want to
   do readlining.
}
  TClearCanvas = procedure(Sender: TObject; Cnv: TCanvas; const ARect: TRect2D; const BackCol: TColor) of Object;

{: This is the base class for all kinds of graphical objects.

   A graphical object is defined here as an object that:

   <LI=is identifiable by means of an ID number (<See Property=TGraphicObject@ID>)>
   <LI=is on a layer from which inherited a pen and brush for drawing (<See Property=TGraphicObject@Layer>)>
   <LI=may be visibility or not (<See Property=TGraphicObject@Visible>)>
   <LI=may be enabled or not for picking (<See Property=TGraphicObject@Enabled>)>
   <LI=may be saved or not to a stream (<See Property=TGraphicObject@ToBeSaved>)>

   A graphic object define also a common interface for management and storing
   that every object that must be drawed on a viewport must specialize.

   This class cannot be instantiated directly (and even derived from) but
   you must use instead the classes <See Class=TObject2D>, <See Class=TObject3D>
   and the classes derived from them. See the unit <See unit=CS4Shapes>.
}
  TGraphicObject = class(TInterfacedObject)
  private
    fID, fTag: LongInt;
    fLayer: Byte;
    fVisible, fEnabled, fToBeSaved: Boolean;
    fOnChange: TNotifyEvent;
    fOwnerCAD: TCADCmp;
  protected
{: This method is called when the extensions and shape of the object must
   be updated after some change to the object state was done.

   When you define a new shape you may want to redefine this method in
   order to keep the object in a coerent state. The library call this method
   whenever it has changed the object. Also when you manipulate object you
   must call the <See Method=TGraphicObject@UpdateExtension>, that in order
   call this method, after the object is changed.

   <B=Note>: Some of the objects already defined in the library define this
   method to compute the bounding box of the object and to set some object's
   specific properties, so don't forget to call the inherited method somewhere
   in you implementation.

   <B=Note>: In this method don't fire the <See Property=TGraphicObject@OnChange>
   event.
}
    procedure _UpdateExtension; virtual; abstract;
  public
{: This is the constructor of the graphic object.

   <I=ID> is the identifier of the the object (<See Property=TGraphicObject@ID>).

   The constructor also set:

   <LI=<See Property=TGraphicObject@Layer> to 0>
   <LI=<See Property=TGraphicObject@Visible> to <B=True> >
   <LI=<See Property=TGraphicObject@Enabled> to <B=True> >
   <LI=<See Property=TGraphicObject@ToBeSaved> to <B=True> >
}
    constructor Create(ID: LongInt);
{: This constructor is used to create an instance of the class and initialize
   its state with the one saved to a stream with a previous call to
   <See Method=TGraphicObject@SaveToStream>.

   <I=Stream> is the stream that contains the saved instance's state.
   This stream must be positioned at the starting of the saved image. Note
   that the saved image must be made with the current library version.
   This method is automatically called by the library itself whenever a
   drawing is loaded.

   If the saved object doesn't correspond to the one that the method expect,
   the library may be left in a corrupted state and normally one or more
   <I=EAccessViolation> exceptions will be raised. To resolve this problem
   the library save an header at the start of a drawing to ensure
   the version correctness of the file.

   When you derive a new shape you have to override this method and
   give your own implementation. However remember to call the inherited
   method at the start of your implementation.

   Following the above rules you are ensured that your drawings are
   always readed by the library.

   See <See=Object's Persistance@PERSISTANCE> for details about the PERSISTANCE mecanism of
   the library.
}
{: <New topic=PERSISTANCE@CADSys 4 - Drawing persistance>
   The library is able to save and retrive the current drawing in a
   <See Class=TCADCmp> control into a stream or file.

   In order to keep the library customizable and easy to extend, this
   streaming facility is able to save user defined object as well as
   library ones. What you need to do is to simply define how to save
   the state and retrive it from a stream by defing the
   <See Method=TGraphicObject@CreateFromStream> and
   <See Method=TGraphicObject@SaveToStream>. After that you have to
   register your class when your application start by using the
   <See Function=CADSysRegisterClass> procedure.
   With these steps your objects will be saved and retrived automatically
   by the library.

   Simply the library save a <I=class index> before saving the object,
   and then use this index to obtain the <I=class reference> to create
   the object when it must be loaded.

   Using this method the library is also able to automatically load and
   convert drawing created with old version of the library without
   need to create an explicit version converter.

   Most of the shapes in the library use this method extensively and
   you have only to save you own datas if you need by using simply
   <I=Read> and <I=Write> method of <I=TStream>.
   Also you don't need to know if your drawing is to be saved in a
   file, in a memory stream or also into a database record.
}
    constructor CreateFromStream(const Stream: TStream; const {%H-}Version: TCADVersion); virtual;
{: This method save an image of the current state of the object to a stream.

   <I=Stream> is the stream on which save the image.

   When you derive a new shape you have to override this method and
   augument it with what you need to save the object state. This method
   will be called automatically by the library when the current drawing
   is being saved but you may want to call it directly to save a contained
   shape of another object. If the <See Property=TGraphicObject@ToBeSaved>
   property is set to <B=False> the object will not be saved by the library.

   See <See=Object's Persistance@PERSISTANCE> for details about the PERSISTANCE mecanism of
   the library.
}
    procedure SaveToStream(const Stream: TStream); virtual;
{: This method is used to make deep copy of the object by obtaining state
   informations from another.

   <I=Obj> is the object from which copy the needed informations. Note that
   this object doen't need to be of the same class of the object into which
   copy these informations. You have to check this class before accessing
   the informations. You have also to call the inherited method if you
   plan to override this method so the responsability chain is maintained.

   The parents classes of the objects from which this method is called
   are traversed to copy as much as possible from the given <I=Obj>.

   <B=Note>: If you use memory allocation remember to copy the memory
   and not simply copy the reference. This is also true with contained
   objects.
}
    procedure Assign(const Obj: TGraphicObject); virtual;
{: This method is called when the extensions and shape of the object must
   be updated after some change to the object state was done.

   When you define a new shape you may want to redefine this method in
   order to keep the object in a coerent state. The library call this method
   whenever it has changed the object. Also when you manipulate object you
   must call this method after the object is changed.
   You may want to override the <See Method=TGraphicObject@_UpdateExtension>
   to give special updates for your shape.

   This method fires a <See Property=TGraphicObject@OnChange> event.
}
    procedure UpdateExtension({%H-}Sender: TObject);
{: This property defines the ID of the object.

   Any graphic object is identified through th library by means of a
   <I=LongInt> number that must univocally identify it. The library by
   itself doesn't check for uniqueness of these IDs so you have to ensure
   it by yourself.
}
    property ID: LongInt read fID write fID;
{: This property contains the layer on which the object lies.

   Any graphic object is associated with a layer from which they get
   their drawing caracteristic such as color, line style, fill color
   and so on.

   An object also get the behavior of this layer such us visibility
   and enabling to picking that have priority on the ones of the
   object.

   If you want to have object specific color you have to change the
   color in the <I=Draw> method of the shape. You don't need to
   revert the object to the one of the layer after you have modified the
   current pen color, because the library will reset this color before
   the <I=Draw> method is called.

   By Dafault this property is set to <B=0>.
}
    property Layer: Byte read fLayer write fLayer;
{: This property specify the visibility of the object.

   If it is <B=True> then the object is drawed on the viewport otherwise
   it will not be drawed (the viewport on which it will be drawed depend
   on the <See Class=TCADCmp> control into which the object is stored).

   The <See Property=TLayer@Visible> setting of the layer on which the
   object lies has the priority on this property.

   By Dafault this property is set to <B=True>.
}
    property Visible: Boolean read fVisible write fVisible;
{: This property specify the behaviour of the object when it is picked.

   If it is <B=False> then the object will not be considered by the picking
   methods, otherwise it may be picked.

   The <See Property=TLayer@Active> setting of the layer on which the
   object lies has the priority on this property.

   By Dafault this property is set to <B=True>.
}
    property Enabled: Boolean read fEnabled write fEnabled;
{: This property specify if the object is to be saved in a drawing.

   If it is <B=False> then the object will not be saved into a drawing stream,
   otherwise it will be.

   The <See Property=TLayer@Streamable> setting of the layer on which the
   object lies has the priority on this property.

   By Dafault this property is set to <B=True>.
}
    property ToBeSaved: Boolean read fToBeSaved write fToBeSaved;
{: This read-only property contains the CAD in which the object was added.
   Although you can add the same object to more than one CAD (and you can
   do that safely by removing all the references from the CADs without deleting
   the object) it isn't a good practice and it is discouraged because this
   property will not still have any meaning.
}
    property OwnerCAD: TCADCmp read fOwnerCAD;
{: This property may be used to store some user's defined data
   like object references.
   It is not used by the library.

   One of the most useful use of this property is when you need to
   link an object to a shape. Consider for example that you have
   a class TSurface in which you store informations like the
   reflectivity coefficent, the color for rendering, the adiacent
   surfaces and the points of the surface. The instances of this class
   are managed by the program that save them and so on. You need of
   course to draw them and manage them interactively (you may want to
   do that instead to change them through the list of points).

   The library can help you in this, bacause you can use the <See Class=TFace3D>
   shape that cames whith the library and that has a list of points and
   that is drawed and managed interactively by the various interaction tasks
   of the library. However if you naive add an instance of TFace3D and
   an instance of TSurface and link them through the ID property of TFace3D,
   you will have duplication of data and a difficult task to syncronize the
   two instances. A better approach is to store the TSurface reference into
   the Tag property and a TFace3D reference in the TSurface instance.
   After that you have to move any geometric datas from TSurface to the TFace3D.
   In our example simply use the TFace3D to store the points and normal, instead
   use the TSurface to store color and coefficients.
   In this way whenever the user modify the shape, the TSurface is automatically
   updated. If there is some data in TSurface that depends on the datas of TFace3D
   (for instance a baricentric point) you have two way to update this data:

   <LI=Derive from TFace3D a new shape with the added data>
   <LI=Put the data in TSurface and the add an event handler that will be
   used for the <See Property=TGraphicObject@OnChange> event>

   With the second solution whenever the points are moved the new
   baricentric point will be calculated.

   This is only a simple and not optimized way to handle these problems.
}
    property Tag: LongInt read fTag write fTag;
{: This is property can store an event-handler for the event <I=OnChange>
   that occour whenever an object is modified (actually the event is
   fired only when the user or the library call <See Method=TGraphicObject@UpdateExtension>
   method).

   You may want to use this event as described in the description of the
   <See Property=TGraphicObject@Tag> property.
}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TGraphicObjectClass = class of TGraphicObject;

{ Syncronization object to prevent linking of Delphi's packages. }
{: For Internal use of CADSys library.
}
  TCADSysSynchroObject = class(TObject)
  public
    procedure Acquire; virtual;
    procedure Release; virtual;
  end;

{: For Internal use of CADSys library.
}
  TCADSysCriticalSection = class(TCADSysSynchroObject)
  private
    FSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Acquire; override;
    procedure Release; override;
    procedure Enter;
    procedure Leave;
  end;

  { Lists of graphical objects. }
{: This class defines an iterator for a list of graphical objects
   (<See class=TGraphicObjList>).

   The library can use a <I=thread> to paint the drawings and so
   a way is needed to ensure that a list is not changed when it is
   in use by another thread.

   To do so the concept of <I=iterator> is used. An iterator is
   an object that is used to traverse a list of objects. When you
   ask and obtain an iterator on a list, you can access the elements
   in the list through the iterator. You cannot modify a list if you
   doesn't have an <I=exclusive iterator> that let you to modify the
   list as well as iterate it (<See Class=TExclusiveGraphicObjIterator>).

   As soon as you obtain an iterator the list cannot be modified and
   so you are ensured that it will remain the same during the iteration.
   Of course if you doen't release an iterator you cannot modify the list
   until the end of the program, so attention is needed when using an
   iterator.

   For backward compatibility some operations on the list are avaiable
   with the list itself (and these are however implemented with temporary
   iterators so using them is the better approach).

   See also <See Class=TExclusiveGraphicObjIterator> and
   <See Class=TGraphicObjList>.

   <B=Note>: To release an iterator simply free it. If you lost some
   reference to iterators (and so the list is blocked) you can unblock
   the list with the method <See Method=TGraphicObjList@RemoveAllIterators>.

   <B=Note>: An iterator cannot be created directly but only with the
   use of the methods <See Method=TGraphicObjList@GetIterator>,
   <See Method=TGraphicObjList@GetExclusiveIterator> and
   <See Method=TGraphicObjList@GetPrivilegedIterator>.
}
  TGraphicObjIterator = class(TObject)
  private
    fCurrent: Pointer;
    fSourceList: TGraphicObjList;

    function GetCurrentObject: TGraphicObject;
    function SearchBlock(ID: LongInt): Pointer;
    function GetCount: Integer;
  public
    constructor Create(const Lst: TGraphicObjList); virtual;
{: This is the descructor of the iterator.

   When an iterator is freed (with the use of <I=Free>) the iterator's
   count in the list from which the iterator was obtained will be decremented
   and the list will be unblocked if it was.

   <B=Remeber to free all the iterators you have when you finish with
   them>.
}
    destructor Destroy; override;
{: Return the object with the specified <I=ID> if found. If that object
   doen't exist in the list <B=nil> will be returned.

   If an object is found then the position in the list will be moved
   to it, if no it will not be moved.
   No exception is raised if no object is found.
}
    function Search(const ID: LongInt): TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the next object in the list, and return it.

   If the current position is at the end of the list the current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function Next: TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the previous object in the list, and return it.

   If the current position is at the beginnig of the list the current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function Prev: TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the first object in the list, and return it.

   If the list is empty current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function First: TGraphicObject;
{: Move the current position (<See Property=TGraphicObjIterator@Current>
   to the last object in the list, and return it.

   If the list is empty current position
   will be set to <B=nil> and <B=nil> will be returned.
}
    function Last: TGraphicObject;
{: This property contains the number of items in the list.

   You may want to use it in combination with
   <See Property=TGraphicObjIterator@Items> to iterate the list
   in an array-like mode.
}
    property Count: LongInt read GetCount;
{: This property contains the current object onto which the iterator
   is positioned. If the position is invalid it will be <B=nil>.
}
    property Current: TGraphicObject read GetCurrentObject;
{: This property contains the list linked to the iterator.
}
    property SourceList: TGraphicObjList read fSourceList;
{: This property contains the items in the list.

   You may want to use it in combination with
   <See Property=TGraphicObjIterator@Count> to iterate the list
   in an array-like mode.

   <I=ID> is zero based so the bounds of the array are from <I=0> to
   <See Property=TGraphicObjIterator@Count>.
}
    property Items[const ID: LongInt]: TGraphicObject read Search; default;
  end;

{: This class defines an exclusive iterator for a list of graphical objects
   (<See class=TGraphicObjList>).

   The library can use a <I=thread> to paint the drawings and so
   a way is needed to ensure that a list is not changed when it is
   in use by another thread.

   To do so the concept of <I=iterator> is used. An iterator is
   an object that is used to traverse a list of objects. When you
   ask and obtain an iterator on a list, you can access the elements
   in the list through the iterator. You cannot modify a list if you
   doesn't have an <I=exclusive iterator> that let you to modify the
   list as well as iterate it (<See Class=TExclusiveGraphicObjIterator>).

   As soon as you obtain an iterator the list cannot be modified and
   so you are ensured that it will remain the same during the iteration.
   Of course if you doen't release an iterator you cannot modify the list
   until the end of the program, so attention is needed when using an
   iterator.

   For backward compatibility some operations on the list are avaiable
   with the list itself (and these are however implemented with temporary
   iterators so using them is the better approach).

   See also <See Class=TExclusiveGraphicObjIterator> and
   <See Class=TGraphicObjList>.

   <B=Note>: To release an iterator simply free it. If you lost some
   reference to iterators (and so the list is blocked) you can unblock
   the list with the method <See Method=TGraphicObjList@RemoveAllIterators>.

   <B=Note>: An iterator cannot be created directly but only with the
   use of the methods <See Method=TGraphicObjList@GetIterator>,
   <See Method=TGraphicObjList@GetExclusiveIterator> and
   <See Method=TGraphicObjList@GetPrivilegedIterator>.
}
  TExclusiveGraphicObjIterator = class(TGraphicObjIterator)
  public
    constructor Create(const Lst: TGraphicObjList); override;
    destructor Destroy; override;
{: This method delete the current object from the source list.
   After the object is deleted the current position in the list
   will be set to the next object in it (if it is at the end
   of the list it will be set to the previous object).

   This method is affected by the state of the
   <See Property=TGraphicObjList@FreeOnClear> property.
}
    procedure DeleteCurrent;
{: This method remove the current object from the source list but
   doesn't free it.
   After the object is removed the current position in the list
   will be set to the next object in it (if it is at the end
   of the list it will be set to the previous object).

   This method is <B=NOT> affected by the state of the
   <See Property=TGraphicObjList@FreeOnClear> property.
}
    procedure RemoveCurrent;
  end;

{: This class defines a double linked list of graphic objects.

   The objects are referenced in the list through their
   identifier number (<See Property=TGraphicObject@ID>). The list doens't
   check for duplicated <I=ID> so you have to ensure uniqueness by yourself.

   To traverse the list you have to use instances of
   <See Class=TGraphicObjIterator> and <See Class=TExclusiveGraphicObjIterator>
   that can be obtained with the methods <See Method=TGraphicObjList@GetIterator>,
   <See Method=TGraphicObjList@GetExclusiveIterator> and <See Method=TGraphicObjList@GetPrivilegedIterator>.

   All of the methods that modify the list can be used only if no iterators
   are attached to the list. If this is not the case an
   <See Class=ECADListBlocked> exception will be raised.

   The list can own the object it contains depending on the setting of
   the property <See Property=TGraphicObjList@FreeOnClear>.
}
  TGraphicObjList = class(TObject)
  private
    fHead, fTail: Pointer;
    fHasExclusive, fFreeOnClear: Boolean;
    fIterators: Integer; { Usato come semaforo. }
    fCount: LongInt;
    fListGuard: TCADSysCriticalSection;

    procedure DeleteBlock(ObjToDel: Pointer);
    procedure RemoveBlock(ObjToDel: Pointer);
    function  GetHasIter: Boolean;
  public
{: This is the constructor of the list. It creates a new empty list.

  It sets <See Property=TGraphicObjList@FreeOnClear> to <B=True>.
}
    constructor Create;
{: This is the destructor of the list.

  If the property <See Property=TGraphicObjList@FreeOnClear> is <B=True>
  the the objects in the list will also be freed.
}
    destructor Destroy; override;
{: This method adds an object to the list.

   The added object <I=Obj> will be placed at the end of the list, after
   any other object already present in the list. The list doesn't check
   for uniqueness of the object's <See Property=TGraphicObject@ID>.

   If the list has some iterators active on it a
   <See Class=ECADListBlocked> exception will be raised.
}
    procedure Add(const Obj: TGraphicObject);
{: This method adds all the object in another list <I=Lst> to the list.

   The added objects will be placed at the end of the list, after
   any other object already present in the list. The list doesn't check
   for uniqueness of the object's <See Property=TGraphicObject@ID>.

   If the list has some iterators active it a
   <See Class=ECADListBlocked> exception will be raised.
}
    procedure AddFromList(const Lst: TGraphicObjList);
{: This method inserts an object into the list.

   The added object <I=Obj> will be inserted after the object in the
   list with ID equal to <I=IDInsertPoint>. If no such object is present
   a <See Class=ECADListObjNotFound> exception will be raised.
   The list doesn't check for uniqueness of the inserted object.

   If the list has some iterators active on it a
   <See Class=ECADListBlocked> exception will be raised.
}
    procedure Insert(const IDInsertPoint: LongInt; const Obj: TGraphicObject);
{: This method moves an object into the list.

   The object with ID equal to <I=IDToMove> will be moved before the
   object in the list with ID equal to <I=IDInsertionPoint> .
   If no such object is present a <See Class=ECADListObjNotFound>
   exception will be raised.

   If the list has some iterators active it a
   <See Class=ECADListBlocked> exception will be raised.

   <B=Note>: This method is useful if you want to change the
   drawing order in the list of object of a <See Class=TCADCmp>
   control.
}
    procedure Move(const IDToMove, IDInsertPoint: LongInt);
{: This method deletes an object from the list.

   The object with ID equal to <I=ID> will be deleted.
   If no such object is present a <See Class=ECADListObjNotFound>
   exception will be raised.

   If the list has the property <See Property=TGraphicObjList@FreeOnClear>
   set to <B=True> then the object will be deleted by calling its <I=Free>
   method.

   If the list has some iterators active on it a
   <See Class=ECADListBlocked> exception will be raised.

   <B=Note>: If you want to delete more that one object from the
   list use an exclusive iterator for better performances.
}
    function  Delete(const ID: LongInt): Boolean;
{: This method removes an object from the list.

   The object with ID equal to <I=ID> will be removed.
   If no such object is present a <See Class=ECADListObjNotFound>
   exception will be raised.

   The object will not also be deleted if the property <See Property=TGraphicObjList@FreeOnClear>
   is set to <B=True>.

   If the list has some iterators active on it a
   <See Class=ECADListBlocked> exception will be raised.

   <B=Note>: If you want to remove more that one object from the
   list use an exclusive iterator for better performances.
}
    function  Remove(const ID: LongInt): Boolean;
{: This method returns the object with the gived Id.

   The object with ID equal to <I=ID> will be returned if found, or <B=nil>
   will be returned if no object is found.

   If the list has some exclusive iterators active on it a
   <See Class=ECADListBlocked> exception will be raised.

   <B=Note>: If you have an iterator on the list don't use this method.
   Use it only if you want to find a single object.
}
    function  Find(const ID: LongInt): TGraphicObject;
{: This method remove all the object from the list.

   The objects will be deleted if the property <See Property=TGraphicObjList@FreeOnClear>
   is set to <B=True>.

   If the list has some iterators active on it a
   <See Class=ECADListBlocked> exception will be raised.
}
    procedure Clear;
{: This method set a new iterator on the list.

   The new iterator will be returned and you can start to use it.
   After you have used it remember to <B=Free> it. It is better to
   protect the use of the iterator in a <B=try-finally> block.

   If the list has an exclusive iterator active on it a
   <See Class=ECADListBlocked> exception will be raised.
}
    function  GetIterator: TGraphicObjIterator;
{: This method set a new exclusive iterator on the list.

   The new iterator will be returned and you can start to use it.
   After you have used it remember to <B=Free> it. It is better to
   protect the use of the iterator in a <B=try-finally> block.

   An exclusive iterator prevent any other thread to modify the list,
   so grant you an exclusive use of the list. With this kind of
   iterator you can remove objects from the list.

   If the list has any iterators active on it a
   <See Class=ECADListBlocked> exception will be raised.
}
    function  GetExclusiveIterator: TExclusiveGraphicObjIterator;
{: This method set a new exclusive iterator on the list ignoring
   any other pending iterator already present.

   The new iterator will be returned and you can start to use it.
   After you have used it remember to <B=Free> it. It is better to
   protect the use of the iterator in a <B=try-finally> block.

   A priviliged iterator is somewhat dangerous and must be used only
   in case of error when an iterator was active on the list and cannot
   be freed (for instance it is useful when the application must be
   aborted in presence of errors but you want to save any other
   present in the CAD).
}
    function  GetPrivilegedIterator: TExclusiveGraphicObjIterator;
{: This method removes any pending iterator active on the list.

   This method is somewhat dangerous and must be used only
   in case of error when an iterator was active on the list and cannot
   be freed (for instance it is useful when the application must be
   aborted in presence of errors but you want to save any other
   present in the CAD).
}
    procedure RemoveAllIterators;
{: This property returns the number of objects present in the list.

   It is useful when you want to iterate through the object in the
   list with an iterators.
}
    property Count: LongInt read fCount;
{: This property returns <B=True> if the list has some iterator active on
   it.

   Check this property before asking for an iterator.
}
    property HasIterators: Boolean read GetHasIter;
{: This property returns <B=True> if the list has an exclusive iterator
   active on it.

   Check this property before asking for an exclusive iterator.
}
    property HasExclusiveIterators: Boolean read fHasExclusive;
{: This property tells the list if the objects in the list must
   be freed when removed from it.

   If it is set to <B=True> the objects in the list are deleted
   (by calling their <I=Free> method) when they are removed from
   the list by using the <See Method=TGraphicObjList@Delete>,
   <See Method=TGraphicObjList@Clear> methods or when the list
   is deleted.
}
    property FreeOnClear: Boolean read fFreeOnClear write fFreeOnClear;
  end;

{: It is a indexed list of objects, that can be accessed in an array-like
   mode.
}
  TIndexedObjectList = class(TObject)
  private
    fListMemory: Pointer;
    fNumOfObject: Integer;
    fFreeOnClear: Boolean;

    procedure SetListSize(N: Integer);
    procedure SetObject(Idx: Integer; Obj: TObject);
    function GetObject(Idx: Integer): TObject;
  public
    {: Create a new empty list that can store no more that <I=Size> objects.
    }
    constructor Create(const Size: Integer);
    {: This is the destructor of the list.

       If the property <See Property=TIndexedObjectList@FreeOnClear> is <B=True>
       the the objects in the list will also be freed.
    }
    destructor Destroy; override;
    {: This method remove all the object from the list.

      The objects will be deleted if the property <See Property=TIndexedObjectList@FreeOnClear>
      is set to <B=True>.
    }
    procedure Clear;
    {: This property contains the objects in the list.

       <I=Idx> goes from 0 to <See Property=TIndexedObjectList@NumberOfObjects>.
    }
    property Objects[Idx: Integer]: TObject read GetObject write SetObject; default;
    {: This property contains the number of objects in the list.
    }
    property NumberOfObjects: Integer read fNumOfObject write SetListSize;
    {: This property tells the list if the objects in the list must
       be freed when removed from it.

       If it is set to <B=True> the objects in the list are deleted
       (by calling their <I=Free> method) when they are removed from
       the list by using the <See Method=TGraphicObjList@Delete>,
       <See Method=TGraphicObjList@Clear> methods or when the list
       is deleted.
    }
    property FreeOnClear: Boolean read fFreeOnClear write fFreeOnClear;
  end;

{: This type defines the layer used by the library.

   Any graphic object is associated with a layer from which they get
   their drawing caracteristic such as color, line style, fill color
   and so on.

   An object also get the behavior of this layer such us visibility
   and enabling to picking that have priority on the ones of the
   object.
}
  TLayer = class(TObject)
  private
    fPen: TPen;
    fBrush: TBrush;
    fDecorativePen: TDecorativePen;
    fName: TLayerName;
    fActive: Boolean;
    fVisible: Boolean;
    fOpaque: Boolean;
    fModified: Boolean;
    fStreamable: Boolean;
    fIdx: Byte;
    fTag: LongInt;

    procedure SetName(Nm: TLayerName);
    procedure SetPen(Pn: TPen);
    procedure SetBrush(Br: TBrush);
    procedure Changed({%H-}Sender: TObject);
  public
{: This is the constructor of the layer.

   <I=Idx> is the index number of the layer. The layers are indexed from
   0 to 255.

   The constructor sets:

   <LI=<See Property=TLayer@Name> to <I=Layer<<Idx>> > >
   <LI=<See Property=TLayer@Pen> to the pen <B=clBlack> and <B=psSolid> >.
   <LI=<See Property=TLayer@Brush> to the brush <B=clWhite> and <B=psSolid> >
   <LI=<See Property=TLayer@Active> to <B=True> >
   <LI=<See Property=TLayer@Visible> to <B=True> >
   <LI=<See Property=TLayer@Opaque> to <B=False> >
   <LI=<See Property=TLayer@Streamable> to <B=True> >
   <LI=<See Property=TLayer@Modified> to <B=False> >
}
    constructor Create(Idx: Byte);
{: This is the destructor of the layer, it destroys the pen and brush
   objects.
}
    destructor Destroy; override;
{: This method saves the layer settings to a stream.

   <I=Stream> is the stream onto which save the settings.
}
    procedure SaveToStream(const Stream: TStream); virtual;
{: This method retrieves the layer settings from a stream.

   <I=Cont> is the index of the layer, <I=Stream> is the stream that
   contains the settings and <I=Version> is the version of the library that
   has saved the layer.
}
    procedure LoadFromStream(const Stream: TStream; const Version: TCADVersion); virtual;
{: This method set the canvas pen and brush values to the one defined by
   the layer.

   <I=Cnv> is the Canvas to be modified <See Class=TDecorativeCanvas@TDecorativeCanvas>.
   The method returns <B=True> if the layer
   is visible and the pen and brush of the canvas are modified.
}
    function  SetCanvas(const Cnv: TDecorativeCanvas): Boolean;
{: This property contains the name of the layer.
}
    property Name: TLayerName read fName write SetName;
{: This property contains the decorative pen object used to draw the objects
   on the layer.

   See also <See Class=TDecorativePen> for details.
}
    property DecorativePen: TDecorativePen read fDecorativePen;
{: This property contains the pen object used to draw the objects on the
   layer.
}
    property Pen: TPen read fPen write SetPen;
{: This property contains the brush object used to draw the objects on the
   layer.

   If you want to change the default pen of the layers use the
   <See Method=TCADCmp@SetDefaultPen>.
}
    property Brush: TBrush read fBrush write SetBrush;
{: This property tells if the objects on the layer are considered for the
   picking operations.

   If you want to change the default brush of the layers use the
   <See Method=TCADCmp@SetDefaultBrush>.
}
    property Active: Boolean read fActive write FActive;
{: This property tells if the objects on the layer are drawed or not.
}
    property Visible: Boolean read fVisible write fVisible;
{: This property tells if the objects on the layer are opaque (<B=True>) or
   transparent.

   If an object is transparent then the brush will not cover the background.
}
    property Opaque: Boolean read fOpaque write FOpaque;
{: This property is <B=True> if the layer was modified and so it must be
   saved, otherwise it will not be saved to a drawing.

   If a layer is not saved it will get the default settings.
}
    property Modified: Boolean read fModified;
{: This property is <B=True> if the objects on the layer must be saved into
   a drawing.

   If this property is <B=False> the objects on the layer are not saved. This
   is useful if you want to save some object into another stream (or a container
   object save it as part of its state).
}
    property Streamable: Boolean read fStreamable write fStreamable;
{: This property is index of the layer that ranges from 0 to 255.
}
    property LayerIndex: Byte read fIdx;
{: This property is not used by the library and so can be used to store
   user defined values like object references.
}
    property Tag: LongInt read fTag write fTag;
  end;

{: This class defines a set of 256 layers (from 0 to 255).
   Every <See Class=TCADCmp> control has an instance of this class.

   This class manage all the layers and stream them on a file.
}
  TLayers = class(TObject)
  private
    fLayers: TList;

    function GetLayer(Index: Byte): TLayer;
    function GetLayerByName(const Nm: TLayerName): TLayer;
  public
{: This is the constructor of the class. It creates a new istance of the set of layers and initialize all
   the layers to the default settings.
}
    constructor Create;
{: This is the destructor of the class. It destroy the set and all of the
   contained layers.
}
    destructor Destroy; override;
{: This method saves only the modified layers into a drawing stream.
}
    procedure SaveToStream(const Stream: TStream);
{: This method retrieve the layers from a drawing stream.
}
    procedure LoadFromStream(const Stream: TStream; const Version: TCADVersion);
{: This method set the canvas pen and brush values to the one defined by
   a layer.

   <I=Cnv> is the Canvas to be modified <See Class=TDecorativeCanvas@TDecorativeCanvas>,
   <I=Index> is the index of the layer
   to be used. The method returns <B=True> if the layer is visible and the
   pen and brush of the canvas are modiefied.
}
    function  SetCanvas(const Cnv: TDecorativeCanvas; const Index: Byte): Boolean;
{: This property contains the set of 256 layers.

   Use this property to change the setting of a layer for a <See Class=TCADCmp>.
}
    property Layers[Index: Byte]: TLayer read GetLayer; default;
{: This property contains the set of 256 layers that can be accessed through
   their names.

   If <I=Nm> doesn't correspond to any of the layers it returns <B=nil>.
   Use this property to change the setting of a layer for a <See Class=TCADCmp>.
}
    property LayerByName[const Nm: TLayerName]: TLayer read GetLayerByName;
  end;

{: This is a non visual control that is used to store and manage the list
   of graphic objects that defines a draw.

   TCADCmp is an asbtract class use to define a common interface for
   drawing management. In fact the control is not useble by itself
   because it doesn't specify the kind of objects that can be stored,
   but only assest what kind of operation are necessary for storing
   general graphic objects.

   Basically this control is used to store a list of objects that must
   be drawed. Until you define a viewport from which see the objects
   these objects are only stored in a list that will be traversed to
   produce a draw as soon as you have defined such a viewport (see
   <See Class=TCADViewport>) by linking a viewport control to the
   TCADCmp (see <See Property=TCADViewport@CADCmp>).

   The first thing you must define is the kind of objects you want to
   use:

   <LI=If you want to create 2D drawings use the control
       <See Class=TCADCmp2D> >
   <LI=If you want to create 3D drawings use the control
       <See Class=TCADCmp3D> >

   these specific controls are derived from this one implementing
   the abstract method to handle 2D or 3D objects.

   This control handles also the PERSISTANCE of the draw, giving
   method to save and retrive the list of objects from a stream or
   a file (see <See=Object's Persistance@PERSISTANCE>). The use of polymorphism and
   class references made possible to add your own shape classes
   that will be considered as much the same as the native ones.

   Think about this control as a TDataSet for a Database application.

   <B=Note>: You don't need to derive from this control because the
   library has already the two controls you need to handle 2D and
   3D drawing. You may want to derive from this control to improve
   its list management or to implement ordered display lists for
   special 3D operation (like implementing spatial partitioning
   with BSP).
}
  TCADCmp = class(TComponent)
  private
    fVersion: TCADVersion; { The version info is used in file I/O. }
    fListOfObjects, fListOfBlocks: TGraphicObjList; { There are two list. The one of the objects and the one of the blocks. }
    fNextID, fNextBlockID: LongInt; { Contain the next ID to assign. }
    fListOfViewport: TList; { Contains all the Viewports linked. }
    fLayers: TLayers; { Layers of the component. }
    fCurrentLayer: Word;
    fDrawOnAdd: Boolean; { If true the object will be drawn on all viewports and the viewports will be refreshed if an object is added. }
    fRepaintAfterTransform: Boolean; { Repaint all the viewports if an object will be transformed. }

    { event handlers. }
    fOnAddObject: TAddObjectEvent;
    fOnVerError: TBadVersionEvent;
    fOnVerErrorEx: TBadVersionExEvent;
    fOnLoadProgress: TOnLoadProgress;
    fOnSaveProgress: TOnSaveProgress;

    function  GetListOfObjects: TGraphicObjIterator;
    function  GetListOfBlocks: TGraphicObjIterator;
    procedure SetListOfObjects(NL: TGraphicObjList);
    procedure SetListOfBlocks(NL: TGraphicObjList);

    function  GetExclusiveListOfObjects: TExclusiveGraphicObjIterator;
    function  GetExclusiveListOfBlocks: TExclusiveGraphicObjIterator;
    function  GetObjectsCount: Integer;
    function  GetSourceBlocksCount: Integer;
    function  GetIsBlocked: Boolean;
    function  GetHasIterators: Boolean;
    procedure SetDefaultLayersColor(const C: TColor);
    function  GetDefaultLayersColor: TColor;
    { Add the indicated Viewport. This function is automatically called by
      a Viewport so the user have no need to call it directly. }
    procedure AddViewports(const VP: TCADViewport);
    { Delete the indicated Viewport. This function is automatically called
      by a Viewport so the user have no need to call it directly. }
    procedure DelViewports(const VP: TCADViewport);
    function  GetViewport(Idx: Integer): TCADViewport;
    function  GetViewportsCount: Integer;
  protected
    {: This method loads the blocks definitions (see <See Class=TSourceBlock2D> and
       <See Class=TSourceBlock3D>) from a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream that contains the blocks (and it must be
       positioned on the first block present). The stream must be
       created with the current version of the library.
       The blocks are created by reading the shape class registration index
       and creating the correct shape instance with the state present in the
       stream. Then this instance is saved in the list of objects of the
       source block.

       When a source block is loaded the objects that are contained in the
       source block are checked for recursive nesting of blocks. It is
       allowed to have a source block that use in its definition another
       source block, BUT this source block must be loaded before the
       source block that use it (this is automatically checked when you
       define a block). When the source block is loaded its
       <See Method=TContainer2D@UpdateSourceReferences>
       (<See Method=TContainer3D@UpdateSourceReferences>) is called to
       update the references.

       If there is a block that references to a not defined source block
       a message will be showed and the source block will not be loaded.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure LoadBlocksFromStream(const Stream: TStream; const Version: TCADVersion); virtual; abstract;
    {: This method saves the blocks definitions (see <See Class=TSourceBlock2D> and
       <See Class=TSourceBlock3D>) to a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream into which save the source blocks,
       <I=AsLibrary> tells if list of source blocks must be saved as
       a library. A Library will contains only the source blocks that
       have the <See Property=TGraphicObject@ToBeSaved> property set to
       <B=True> and the <See Property=TSourceBlock2D@IsLibraryBlock> (
       <See Property=TSourceBlock3D@IsLibraryBlock>) property set to
       <B=True>. This is useful to create library of blocks definition
       to be shared beetwen drawing.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure SaveBlocksToStream(const Stream: TStream; const AsLibrary: Boolean); virtual; abstract;
    {: This method loads the objects saved in a drawing stream.

       <I=Stream> is the stream that contains the objects (and it must be
       positioned on the first object present). The stream must be
       created with the current version of the library.
       The objects are created by reading the shape class registration
       index and creating the correct shape instance with the state present
       in the stream. Then this instance is stored in the list of object of
       the control.

       When a block instance (see <See Class=TBlock2D> and <See Class=TBlock3D>)
       is loaded the source block reference is updated with the current list
       of source block by calling the <See Method=TBlock2D@UpdateReference>
       (<See Method=TBlock3D@UpdateReference>) method. If this source block
       is not present a message will be showed and the source object discarded.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure LoadObjectsFromStream(const Stream: TStream; const Version: TCADVersion); virtual; abstract;
    {: This method saves the objects in the display list to a drawing stream.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream into which save the objects.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure SaveObjectsToStream(const Stream: TStream); virtual; abstract;
    {: This method adds a new source block.

       <I=ID> is the identifier number of the source block and Obj is the
       source block object. The library doesn't check for uniqueness of
       source block's ID. The given ID substitute the ID of Obj. A source
       block can also be referenced by its name and again the library
       doesn't check for its uniqueness.

       A source block is a collection of objects that are drawed at a whole
       on the viewport. A source block must be istantiated before the
       drawing occourse by define a block that references it. The instance
       has a position so you can share the same collection of objects and
       draw it in different places on the same drawing. This allows the
       efficent use of memory. If a source block is modified, all the
       instances are modified accordingly.

       The method returns the added block.

       See also <See Class=TSourceBlock2D>, <See Class=TSourceBlock3D>,
       <See Class=TBlock2D> and <See Class=TBlock3D>.
    }
    function AddSourceBlock(ID: LongInt; const Obj: TGraphicObject): TGraphicObject;
    {: This method returns the source block with the given name or
       <B=nil> if no such object is found.

       The returned reference is of type <See Class=TGraphicObject>, you must
       up-cast it to the appropriate class.

       See also <See Method=TCADCmp@GetSourceBlock>.
    }
    function  FindSourceBlock(const {%H-}SrcName: TSourceBlockName): TGraphicObject;
    {: This method returns the source block with the given ID or
       <B=nil> if no such object is found.

       The returned reference is of type <See Class=TGraphicObject>, you must
       up-cast it to the appropriate class.

       See also <See Method=TCADCmp@FindSourceBlock>.
    }
    function  GetSourceBlock(ID: LongInt): TGraphicObject;
    {: This method adds a new object.

       <I=ID> is the identifier number of the object to add and Obj is the
       object to add. The library doesn't check for uniqueness of
       object's ID. The given ID substitute the ID of Obj. The object
       will be placed on the current layer (see <See Property=TCADCmp@CurrentLayer>).

       The object is added after all the objects in the control, and will be
       drawed in front of them. Use <See Method=TGraphicObjList@Move> method
       on <See Property=TCADCmp@ObjectList> to change the this order; or use
       <See Method=TCADCmp@InsertObject> to add the object in a specified
       position in the list.

       The method returns the added object.
    }
    function AddObject(ID: LongInt; const Obj: TGraphicObject): TGraphicObject;
    {: This method insert a new object in a given position.

       <I=ID> is the identifier number of the object to insert and Obj is the
       object to insert. The library doesn't check for uniqueness of
       object's ID. The given ID substitute the ID of Obj. The object
       will be placed on the current layer (see <See Property=TCADCmp@CurrentLayer>).
       <I=IDInsertPoint> is the object's ID of the object in front of which <I=Obj>
       will be inserted. So <I=Obj> is drawed before this object and appear under it.
       The objects are drawed in the order in which they appear in the list.

       If the object with <I=IDInsertPoint> doesn't exist in the list a
       <See Class=ECADListObjNotFound> exception will be raised.

       The method returns the added object.
    }
    function InsertObject(ID, IDInsertPoint: LongInt; const Obj: TGraphicObject): TGraphicObject;
    {: This method returns the object with the given ID or
       <B=nil> if no such object is found.

       The returned reference is of type <See Class=TGraphicObject>, you must
       up-cast it to the appropriate class before use.
    }
    function  GetObject(ID: LongInt): TGraphicObject;
    {: This method draw the given object on all the linked viewports.

       <I=Obj> is the object to be drawed. This object will be drawed with
       the appropriate colour on all the viewports linked to the control
       (see <See Property=TCADCmp@Viewports>).

       The viewports will also be refreshed after the drawing
       (see <See Method=TCADViewport@Refresh>).
    }
    procedure RedrawObject(const Obj: TGraphicObject);
    {: This property contains the list of objects of the control.

       Use this property if you want to change the display list through
       the methods of the list of graphic objects (see <See Class=TGraphicObjList>).

       If you want to traverse the list of object use the methods
       <See Method=TCADCmp@ObjectsIterator>,
       <See Method=TCADCmp@ObjectsExclusiveIterator> to obtain iterators on the
       display list.

       This property is useful if you want to use an optimized list instead of the
       default one (it must be derived from <See Class=TGraphicObjList> class).
       In this case use this property just after the creation of the control and
       before adding objects to it.
    }
    property ObjectList: TGraphicObjList read fListOfObjects write SetListOfObjects;
    {: This property contains the list of source block of the control.

       Use this property if you want to change the display list through
       the methods of the list of graphic objects (see <See Class=TGraphicObjList>).

       If you want to traverse the list of source blocks use the methods
       <See Method=TCADCmp@SourceBlocksIterator>,
       <See Method=TCADCmp@SourceBlocksExclusiveIterator> to obtain iterators on the
       source block list.

       This property is useful if you want to use an optimized list instead of the
       default one (it must be derived from <See Class=TGraphicObjList> class).
       In this case use this property just after the creation of the control and
       before adding any source blocks to it.
    }
    property BlockList: TGraphicObjList read fListOfBlocks write SetListOfBlocks;
  public
    {: This is the constructor of the control.

       It sets the control in the following state:

       <LI=<See property=TCADCmp@CurrentLayer> set to 0>
       <LI=<See property=TCADCmp@DrawOnAdd> set to <B=False> >
       <LI=<See property=TCADCmp@RepaintAfterTransform> set to <B=True> >
       <LI=<See property=TCADCmp@DefaultLayersColor> set to <B=clBlack> >
    }
    constructor Create(AOwner: TComponent); override;
    {: This is the destructor of the control.

       It destroy the control and all the objects and source blocks present
       in its display list. After that you are not allowed to reference any
       of its objects.
    }
    destructor Destroy; override;
    {: This method loads a drawing in the current one.

       <I=Stream> is the stream that contains the drawing to merge. No
       check is made to ensure that object's IDs are unique also in
       the merged drawing nor duplicate source blocks. If the stream
       doesn't contains a valid drawing then a <See Class=ECADFileNotValid>
       exception will be raised. If the drawing was created with a
       different version that the current one the drawing will be converted or
       the event <See Property=TCADCmp@OnInvalidFileVersion> will be fired.

       Use this method to mix one or more drawing in one.

       See also <See Method=TCADCmp@MergeFromFile>.

       <B=Note>: If the two drawings have different layer tables, then
       the one in the draw to merge will be used.
    }
    procedure MergeFromStream(const Stream: TStream);
    {: This method loads a drawing from a stream.

       <I=Stream> is the stream that contains the drawing to load. If
       the stream doesn't contains a valid drawing then a
       <See Class=ECADFileNotValid>
       exception will be raised. If the drawing was created with a
       different version that the current one the drawing will be converted or
       the event <See Property=TCADCmp@OnInvalidFileVersion> will be fired.

       The current drawing is abbandoned when this method is called. The
       source blocks that are not library block will be also abbandoned.

       See also <See Method=TCADCmp@LoadFromFile>.
    }
    procedure LoadFromStream(const Stream: TStream);
    {: This method saves a drawing into a stream.

       <I=Stream> is the destination stream. Only the source blocks that
       are no library blocks are saved with the drawing. Only the layers
       modified will be saved with the drawing.

       See also <See Method=TCADCmp@SaveToFile>.
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method loads the source blocks saved to a librarian stream.

       <I=Stream> is the stream that contains the library. If
       the stream doesn't contains a valid drawing then a
       <See Class=ECADFileNotValid>
       exception will be raised. If the library file was created with a
       different version that the current one the library will be converted or
       the event <See Property=TCADCmp@OnInvalidFileVersion> will be fired.

       The source blocks will be added to the current ones.
    }
    procedure LoadLibrary(const Stream: TStream);
    {: This method saves a library (a group of source blocks) into a stream.

       <I=Stream> is the destination stream. Only the source blocks that
       are library blocks are saved.

       See also <See Method=TCADCmp@SaveToFile>.
    }
    procedure SaveLibrary(const Stream: TStream);
    {: This method loads a drawing in the current one.

       <I=FileName> is the file name that contains the drawing to merge. No
       check is made to ensure that object's IDs are unique also in
       the merged drawing nor duplicate source blocks. If the file
       doesn't contains a valid drawing then a <See Class=ECADFileNotValid>
       exception will be raised. If the drawing was created with a
       different version that the current one the drawing will be converted or
       the event <See Property=TCADCmp@OnInvalidFileVersion> will be fired.

       Use this method to mix one or more drawing in one.

       See also <See Method=TCADCmp@MergeFromStream>.

       <B=Note>: If the two drawings have different layer tables, then
       the one in the draw to merge will be used.
    }
    procedure MergeFromFile(const FileName: String);
    {: This method loads a drawing from a file.

       <I=FileName> is the file name of the file that contains the drawing
       to load. If the file doesn't contains a valid drawing then a
       <See Class=ECADFileNotValid>
       exception will be raised. If the drawing was created with a
       different version that the current one the drawing will be converted or
       the event <See Property=TCADCmp@OnInvalidFileVersion> will be fired.

       The current drawing is abbandoned when this method is called. The
       source blocks that are not library block will be also abbandoned.

       See also <See Method=TCADCmp@LoadFromStream>.
    }
    procedure LoadFromFile(const FileName: String);
    {: This method saves a drawing into a file.

       <I=Stream> is the destination file name of the file. Only the source
       blocks that are no library blocks are saved with the drawing. Only
       the layers modified will be saved with the drawing.

       See also <See Method=TCADCmp@SaveToStream>.
    }
    procedure SaveToFile(const FileName: String);
    {: This method deletes the source block with the specified ID.

      <I=ID> is the identification number of the source block. If no such
      block is found a <See Class=ECADListObjNotFound> exception will be raised.

      If the source block to be delete is in use (that is it is referenced
      by some <See Class=TBlock2D>, <See Class=TBlock3D> object) a
      <See Class=ECADSourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteSourceBlockByID(const ID: LongInt);
    {: This method deletes all the source block currently defined.

      If some of the source blocks is in use (that is it is referenced
      by some <See Class=TBlock2D>, <See Class=TBlock3D> object) a
      <See Class=ECADSourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.

      This method is called when the CADCmp is freed, so you may see an
      error before the application is closed that inform you that some
      source blocks are not deleted. You can simply ignore this error
      that indicate a problem in the construction of the drawing.
    }
    procedure DeleteAllSourceBlocks;
    {: This method deletes all the source blocks that were saved into
       a library stream (that is they have the property
       ToBeSaved set to true and they are not library blocks).

      Use it to delete all the source blocks that belong to a
      drawing keeping only the ones that are not saved to a stream.

      If some of the source blocks is in use (that is it is referenced
      by some <See Class=TBlock2D>, <See Class=TBlock3D> object) a
      <See Class=ECADSourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteSavedSourceBlocks; virtual; abstract;
    {: This method deletes all the source blocks that are
       library stream (saved or not saved !).

      Use it to undload a library if you need this or before load
      another one.

      If some of the source blocks is in use (that is it is referenced
      by some <See Class=TBlock2D>, <See Class=TBlock3D> object) a
      <See Class=ECADSourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteLibrarySourceBlocks; virtual; abstract;
    {: This method searches for an object with a given ID, and if
       found changes the layer on which it lies.

       <I=ID> is the identification number of the object;
       <I=NewLayer> is the new layer number on which the object must reside.

      If the object is not found a <See Class=ECADListObjNotFound> exception
      will be raised.
    }
    procedure ChangeObjectLayer(const ID: LongInt; const NewLayer: Byte);
    {: This method moves an object in the display list.

       <I=IDOrigin> is the identification number of the object to be moved;
       <I=IDDestination> is the identification number of the object before which
       the object will be moved.

       This method is useful to change the order of the object. After the
       moving the object with ID equal to <I=IDOrigin> will be under the
       one with ID equal to <I=IDDestination>.

      If the object is not found a <See Class=ECADListObjNotFound> exception
      will be raised.
    }
    procedure MoveObject(const IDOrigin, IDDestination: LongInt);
    {: This method deletes an object of the display list.

       <I=ID> is the identification number of the object to be deleted.

      If the object is not found a <See Class=ECADListObjNotFound> exception
      will be raised.
    }
    procedure DeleteObject(const ID: LongInt);
    {: This method removes an object of the display list but doesn't free
       its reference.

       <I=ID> is the identification number of the object to be removed.

      If the object is not found a <See Class=ECADListObjNotFound> exception
      will be raised.
    }
    procedure RemoveObject(const ID: LongInt);
    {: This method removes all the objects of the display list.
    }
    procedure DeleteAllObjects;
    {: This method repaint all the linked viewports
       (see <See Property=TCADCmp@Viewports>).

       This method is useful to regenerate all the viewports that display the
       objects.
    }
    procedure RepaintViewports; virtual;
    {: This method refresh all the linked viewports
       (see <See Property=TCADCmp@Viewports>).

       This method is useful to update the viewports' images without
       redraw all the objects, but simply by copying the off-screen
       buffer on the canvases.
    }
    procedure RefreshViewports; virtual;
    {: This method sets the default brush of the unmodified layers.

       <I=Brush> is the brush that will be assigned to the brush of the
       unmodified layers. The reference will be assigned (deep-copied) and
       not simply stored in the CADCmp, so you have to delete it by yourself.

       This method is useful when you change the background color of the viewports
       and the default color (clWhite) will be difficul to see.
    }
    procedure SetDefaultBrush(const Brush: TBrush);
    {: This method sets the default pen of the unmodified layers.

       <I=Pen> is the pen that will be assigned to the pen of the
       unmodified layers. The reference will be assigned (deep-copied) and
       not simply stored in the CADCmp, so you have to delete it by yourself.

       This method is useful when you change the background color of the viewports
       and the default color (clBlack) will be difficul to see.
    }
    procedure SetDefaultPen(const Pen: TPen);
    {: This property must be used to obtain an iterator on the display list
       of the objects.

       The reference that will be obtained must be freed when you finish to
       use it.

       See also <See Class=TGraphicObjIterator>.
    }
    property ObjectsIterator: TGraphicObjIterator read GetListOfObjects;
    {: This property must be used to obtain an iterator on the list
       of the source blocks.

       The reference that will be obtained must be freed when you finish to
       use it.

       See also <See Class=TGraphicObjIterator>.
    }
    property SourceBlocksIterator: TGraphicObjIterator read GetListOfBlocks;
    {: This property must be used to obtain an exclusive iterator on the
       display list of the objects.

       The reference that will be obtained must be freed when you finish to
       use it.

       See also <See Class=TExclusiveGraphicObjIterator>.
    }
    property ObjectsExclusiveIterator: TExclusiveGraphicObjIterator read GetExclusiveListOfObjects;
    {: This property must be used to obtain an exclusive iterator on the list
       of the source blocks.

       The reference that will be obtained must be freed when you finish to
       use it.

       See also <See Class=TExclusiveGraphicObjIterator>.
    }
    property SourceBlocksExclusiveIterator: TExclusiveGraphicObjIterator read GetExclusiveListOfBlocks;
    {: This property contains the layer table of the control.
       Use it to modify the visual aspect of the objects.

       See also <See Class=TLayers>.
    }
    property Layers: TLayers read fLayers;
    {: This property is the current layer on which any object added to the
       CAD resides.

       When you add on object to the display list of the control, its layer
       will be set to the value of this property (by default 0).
       You can modify the layer of the added object by using the reference
       to the object AFTER it is added to the control.
    }
    property CurrentLayer: Word read fCurrentLayer write fCurrentLayer;
    {: This property contains the current version of the library.

       It is used to check the versions of the drawing files.
    }
    property Version: TCADVersion read fVersion write fVersion;
    {: This property contains the number of objects present in the display list.
    }
    property ObjectsCount: Integer read GetObjectsCount;
    {: This property contains the number of source blocks present in the source blocks list.
    }
    property SourceBlocksCount: Integer read GetSourceBlocksCount;
    {: This property is <B=True> when one of the lists (display list or
       source blocks list) is blocked, that is it has an active exclusive
       iterator on it.

       When a list is blocked you cannot add or remove any items from it.
       So check this property before asking for an iterator.
    }
    property IsBlocked: Boolean read GetIsBlocked;
    {: This property is <B=True> when one of the lists (display list or
       source blocks list) has an active iterator on it.

       When a list has an active iterator, you can ask for another iterator
       but not for an exclusive iterator.
       So check this property before asking for an exclusive iterator.
    }
    property HasIterators: Boolean read GetHasIterators;
    {: This property contains all the viewports that are linked to the
       control.

       A viewport is a visual components that display the world defined
       by the CADCmp control. <I=Idx> is the index of the viewport from
       0 to <See Property=TCADCmp@ViewportsCount> - 1.

       Use this property if you want to change some of the properties of
       the viewports that use the CADCmp control or if you want to apply
       to all of them an operation.

       See also <See Class=TCADViewport>.
    }
    property Viewports[Idx: Integer]: TCADViewport read GetViewport;
    {: This property contains the number of viewport that are linked
       to the control.

       See also <See Class=TCADCmp@Viewports>.
    }
    property ViewportsCount: Integer read GetViewportsCount;
  published
    {: If this property is <B=True> then when an object is added to the
       control it is also drawed on all the linked viewports.

       By default it is <B=False>.
    }
    property DrawOnAdd: Boolean read fDrawOnAdd write fDrawOnAdd default False;
    {: If this property is <B=True> then when an object is transformed
       then the linked viewports will be repainted.

       By default it is <B=False>.
    }
    property RepaintAfterTransform: Boolean read fRepaintAfterTransform write fRepaintAfterTransform default True;
    {: This property contains (and sets) the default colours for the
       pen of all the unmodified layers.

       Modified layers are unchanged.
    }
    property DefaultLayersColor: TColor read GetDefaultLayersColor write SetDefaultLayersColor default clBlack;
    {: EVENTS}
    {: This property may contain an event handler that is
       called when an object is loaded into the control.

       See also <See Type=TOnLoadProgress>.
    }
    property OnLoadProgress: TOnLoadProgress read fOnLoadProgress write fOnLoadProgress;
    {: This property may contain an event handler that is
       called when an object is saved into a stream.

       See also <See Type=TOnSaveProgress>.
    }
    property OnSaveProgress: TOnSaveProgress read fOnSaveProgress write fOnSaveProgress;
    {: This property may contain an event handler that is
       called when an object is added to the control.

       See also <See Type=TAddObjectEvent>.
    }
    property OnAddObject: TAddObjectEvent read fOnAddObject write fOnAddObject;
    {: This property may contain an event handler that is
       called when a drawing created with an older version
       of the library is loaded.

       See also <See Type=TBadVersionEvent>, <See Type=TBadVersionExEvent>, .
    }
    property OnInvalidFileVersion: TBadVersionEvent read fOnVerError write fOnVerError;
    property OnInvalidFileVersionEx: TBadVersionExEvent read fOnVerErrorEx write fOnVerErrorEx;
  end;

  {: This class defines a viewport with which it is possible to render
     the contents of a CAD control. This is a visual component.

     The viewport is a 2D windows through which you can see a virtual world.
     The world is stored in a <See Class=TCADCmp> control as a list (display list)
     of graphic objects. The world used here is dimension less, so this component
     is not directly usable but it defines only an abstract interface.

     Regardless the kind of world (2d, 3d or more) a viewport is always a window
     on which the world is projected. The view trasformation has two
     components:

     <LI=the first one project the world on the window plane. If the world
         is already 2D this is a simple identity transform (Projection) >
     <LI=the second one project the window plane onto a portion on the canvas
         of the control (Mapping).>

     This control defines and uses only the second component of the transformation
     (see <See Method=TCADViewport@BuildViewportTransform>). The other one is
     dimension dependent and so must be defined in derived classes.

     You manage the window position and size (that is modify the second component
     of the above transformations) using the property
     <See Property=TCADViewport@VisualRect>, that is the portion of the
     window plane that you see on the canvas of the control.
     You change the VisualRect using the zooming methods (see
     <See Method=TCADViewport@ZoomIn>, <See Method=TCADViewport@ZoomOut>,
     <See Method=TCADViewport@ZoomWindow>, <See Method=TCADViewport@ZoomToExtension>
     <See Method=TCADViewport@PanWindow> and <See Method=TCADViewport@MoveWindow>).
     All of these changes the mapping transformation (the second component explained
     above).

     A viewport use a back buffer to store the current rapresentation of the
     CADCmp display list. When you start a <See Method=TCADViewport@Repaint>,
     this buffer is created and copied on the canvas of the control.
     The repaint can use a thread (painting thread) to allow the user to stop
     the operation at any time. However using a thread require some additional
     steps that require a bit of time, so it is advisable to use the thread
     (see <See Property=TCADViewport@UsePaintingThread>) only when you have
     a complex draw made up of thousand of objects.
     During the repaint process you can also copy the buffer content on the
     on screen canvas when a prestabilited number of objects (see
     <See Property=TCADViewport@CopingFrequency> property) is drawed. This
     gives a useful feedback to the user.

     See also <See Class=TCADViewport2D> and <See Class=TCADViewport3D>.
}

  { TCADViewport }

  TCADViewport = class(TCustomControl)
  private
    fViewGuard: TCADSysCriticalSection;
    fCADCmp: TCADCmp;
    fDrawMode: Cardinal;
    fVisualWindow: TRect2D; { The current viewport on the view plane. }
    fViewportToScreen, fScreenToViewport: TTransf2D; { To speed the inversion of the matrix I keep either. }
    fAspectRatio: TRealType;
    fRubberPenColor, fBackGroundColor, fGridColor, fControlPointsColor: TColor;
    fRubberPen: TPen;
    { FOffScreenBitmap contain the off-screen bitmap used by the Viewport. The
      Viewport use FOffScreenBitmap to store the actual view of the draw. When
      the draw change the FOffScreenBitmap is redrawed. During a repaint the
      FOffScreenBitmap is put on the canvas. }
    fOffScreenBitmap: TBitmap;
    fOffScreenCanvas: TDecorativeCanvas;
    fOnScreenCanvas: TDecorativeCanvas;
    fCopingFrequency: Integer; { Indica ogni quanto copiare il buffbitmap sul canvas quando si usa il threading. }
    fShowControlPoints, fShowGrid, fInUpdate: Boolean;
    fControlPointsWidth: Byte;
    fGridDeltaX,
    fGridDeltaY,
    fGridSubX,
    fGridSubY: TRealType;
    fGridOnTop: Boolean;
    fTransparent: Boolean;
    { If fViewportObjects is nil then the component will use the CADCmp. Else
      this list is used. Note: The list is not managed by the viewport. }
    fViewportObjects: TGraphicObjList;
    { Event handlers }
    fOnClear: TClearCanvas;
    fDisablePaintEvent, fDisableMouseEvents: Boolean;
    fOnPaint, fOnResize, fOnBeginRedraw, fOnEndRedraw: TNotifyEvent;
    fOnViewMappingChanged: TNotifyEvent;
    fOnMouseEnter, fOnMouseLeave: TNotifyEvent;

    { Multithread support. }
    fPaintingThread: TObject;
    fUseThread: Boolean;

    { Set method for the property. }
    procedure SetRubberColor(const Cl: TColor);
    procedure SetBackColor(const Cl: TColor);
    procedure SetTransparent(const B: Boolean);
    procedure SetGridColor(const Cl: TColor);
    procedure SetShowGrid(const B: Boolean);
    procedure SetOnClearCanvas(const H: TClearCanvas);
    procedure ClearCanvas({%H-}Sender: TObject; Cnv: TCanvas; const ARect: TRect2D; const BackCol: TColor);
    procedure DoCopyCanvas(const GenEvent: Boolean);
    procedure DoCopyCanvasThreadSafe;
    procedure CalibrateCnv(const Cnv: TCanvas; XScale, YScale: TRealType);
    { NEW. Cambia il mapping chiamando BuildViewportTransform. }
    procedure ChangeViewportTransform(ViewWin: TRect2D);

    { Per il thread }
    procedure StopPaintingThread;
    { Se sono in repainting lo blocco e ricomincio. }
    procedure UpdateViewport(const ARect: TRect2D); { repaint all the objects contained in ARect. }
    function  GetInRepaint: Boolean;
    procedure DoResize;
    procedure CopyBitmapOnCanvas(const DestCnv: TCanvas; const Bmp: TBitmap; IRect: TRect; IsTransparent: Boolean; TransparentColor: TColor);
    procedure SetDeltaX(const V: TRealType);
    procedure SetDeltaY(const V: TRealType);
    procedure SetSubX(const V: TRealType);
    procedure SetSubY(const V: TRealType);
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    {: This method is called at the creation of the control to
       create the decorative canvas for it. You can override it
       to use your specialized decorative canvas.

       Note that only the off screen canvas can be personalized.
    }
    function CreateOffScreenCanvas(const Cnv: TCanvas): TDecorativeCanvas; dynamic;
    {: This method is called when a painting thread (if used) is terminated.

       It sets the thread instance reference to nil (the thread is freed by
       itself). It also copy the backbuffer on the canvas of the control.

       See also <See Property=TCADViewport@UsePaintingThread>.
    }
    procedure OnThreadEnded({%H-}Sender: TObject); dynamic;
    {: This method copies the backbuffer image on the canvas of the control.

       <I=Rect> is the rectangle of the backbuffer to be copied on the same
       rectangle on the canvas of the control (remember that the two canvases
       have the same size); if <I=GenEvent> is <B=True> the an
       <See Property=TCADViewport@OnPaint> event will be fired after the copy.
    }
    procedure CopyBackBufferRectOnCanvas(const Rect: TRect; const GenEvent: Boolean); dynamic;
    {: This method sets the <See Class=TCADCmp> control that contains the display list
       to be drawed.

       <I=Cad> is the control reference to which the viewport will be linked.
       If it is <B=nil> the viewport will be removed from the current CAD
       to which it is linked.

       See also <See Property=TCADCmp@Viewports> property.
    }
    procedure SetCADCmp(Cad: TCADCmp); virtual;
    {: This method handles the WM_PAINT message of Windows.

       It firstly check to see if the dimension of the component are
       different with the ones of the back buffer canvas. In this case
       the back buffer dimensions are changed accordingly and the
       display list is rendered on it.

       After that the back buffer image is simply copied on the control
       canvas and an <See Property=TCADViewport@OnPaint> event is fired.
    }
    procedure Paint; override; { Repaint all the objects. }
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    //procedure WMSize(var Message: TLMSize); message LM_SIZE;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  protected
    procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    {: This method is called by the viewport whenever a changing in the
       mapping transform from window plane (view plane) and canvas
       rectangle is required.

       It must returns a 2d transform matrix that transforms the 2d points
       in the visual rect (see <See Property=TCADViewport@VisualRect>)
       in the 2d points in the client rectangle of the control Canvas.

       <I=ViewWin> is the visual rect (that is the portion of
       the world currently in view); <I=ScreenWin> is the portion of the
       control's canvas that must contains the drawing and <I=AspectRatio> is
       an optional aspect ratio (Width/Heigth) to be preserved in the mapping.

       If AspectRatio is zero then no aspect ratio is specified, otherwise the
       aspect ratio must modify the <I=ViewWin> dimensions so that they preserve
       this aspect ratio. The new ViewWin will became the <See Property=TCADViewport@VisualRect>
       of the viewport.

       You must redefine this method if you want to create a new viewport,
       for example you can also change any other projection transform when
       this method is called; by default it returns <See const=IdentityTransf2D>.

       <B=Note>: You may want to use the <See function=GetVisualTransform2D> function
       to obtain the mapping transform.
    }
    function  BuildViewportTransform(var {%H-}ViewWin: TRect2D; const {%H-}ScreenWin: TRect; const {%H-}AspectRatio: TRealType): TTransf2D; virtual;
    {: This method draws a 2D rectangular grid on the viewport.

       <I=ARect> is the portion of the window plane that is currently viewed in
       the control; <I=Cnv> is the canvas on which draw the grid.

       By default it draws a grid that originates in (0, 0) and has an X step
       and Y step specified by the <See Property=TCADViewport@GridDeltaX>,
       <See Property=TCADViewport@GridDeltaY> properties.
    }
    procedure DrawGrid(const ARect: TRect2D; const Cnv: TCanvas); virtual;
    {: This method draws an object on a canvas, by transforming it with the
       view transform.

       This method must be specilized to manages the objects of the correct type
       in the display list. This method then calls the appropriate drawing
       method of the specialized object. You have also to check for
       visibility in this method, to reduce the time of drawing.

       Before any drawing takes place the Canvas is modified with the
       object's layer properties.

       <I=Obj> is the object to be drawed and <I=Cnv> is the canvas on
       which to draw the object. <I=ClipRect2D> is the clipping rectangle
       in 2D coordinates (of the canvas).
    }
    procedure DrawObject(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D); virtual; abstract;
    {: This method draws an object on a canvas, by transforming it with the
       view transform.

       This method must be specilized to manages the objects of the correct type
       in the display list. This method then calls the appropriate drawing
       method of the specialized object. You have also to check for
       visibility in this method, to reduce the time of drawing.

       The object is drawed with the <See Property=TCADViewport@RubberPen>
       pen. This is useful when you want to simulate the rubber band method
       , so when the same object is drawed twice the second time remove the
       object from the canvas leaving it as it was before any draw taken place.
       This is the default beaviour. See the <See Property=TCADViewport@RubberPenColor>
       property for a way to change the color of the rubber pen.

       The method draws also the control points of the object if any control
       points is defined.

       <I=Obj> is the object to be drawed and <I=Cnv> is the canvas on
       which to draw the object. <I=ClipRect2D> is the clipping rectangle
       in 2D coordinates.
    }
    procedure DrawObjectWithRubber(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D); virtual; abstract;
    {: This method copy a portion of the back buffer canvas on another canvas.
       It is useful to copy the drawing in the clipboard or to print it.

       <I=CADRect> is the portion (in view plane coordinates) of the view plane
       that must be copied; <I=CanvasRect> is the rectangle into which copy that
       portion and <I=Cnv> is the destination canvas. <I=CopyMode> specify the
       type of the copy (see <See Type=TCanvasCopyMode>).
    }
    procedure CopyRectToCanvas(CADRect: TRect2D; const CanvasRect: TRect; const Cnv: TCanvas; const Mode: TCanvasCopyMode); virtual; abstract;
    {: This method returns the mapping transform matrix that is used by the
       <See Method=TCADViewport@CopyRectToCanvas> method to copy a portion of
       the back buffer canvas on another canvas.

       <I=CADRect> is the portion (in view plane coordinates) of the view plane
       that must be copied; <I=CanvasRect> is the rectangle into which copy that
       portion and <I=Cnv> is the destination canvas. <I=CopyMode> specify the
       type of the copy (see <See Type=TCanvasCopyMode>).
    }
    function  GetCopyRectViewportToScreen({%H-}CADRect: TRect2D; const {%H-}CanvasRect: TRect; const {%H-}Mode: TCanvasCopyMode): TTransf2D; virtual;
    {: This method returns the mapping transform matrix that maps the
       visual rect portion of the view plane in the client area of the
       control.

       You may want to use it to mimics viewport view transform. For
       instance with this transformation you can found where a point in
       viewplane coordinates will be transformed on the control's canvas.

       <B=Note>: This matrix models only the mapping part of the view
       transformation. You may also want to add the projection part, that
       is viewport dependent.
    }
    function  GetViewportToScreen: TTransf2D; virtual;
    {: This method returns the invers of the mapping transform matrix that
       maps the client area of the control in the visual rect portion of
       the view plane.

       You may want to use it to mimics viewport view transform. For
       instance with this transformation you can found where a point in
       canvas of the control will be transformed in the view plane.

       <B=Note>: This matrix models only the mapping part of the view
       transformation. You may also want to add the projection part, that
       is viewport dependent.
    }
    function  GetScreenToViewport: TTransf2D; virtual;
    {: This method returns the width of a pixel in world coordinates.
       This method is used
       by the <See Method=TCADViewport2D@PickObject> (<See Method=TCADViewport3D@PickObject>)
       method.

       The point returned must be interpreted as:

       <LI=The X coordinate of the point rapresent the width of the square
       into which a pixel is transformed by the viewport.>
       <LI=The Y coordinate of the point rapresent the height of the square
       into which a pixel is transformed by the viewport.>
    }
    function  GetPixelAperture: TPoint2D; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    {: The back-buffer bitmap. }
    property OffScreenBitmap: TBitmap read fOffScreenBitmap;
  public
    { Public declarations }
    {: This is the constructor of the control.

       It creates a new viewport with the following properties:

       <LI=<See Property=TCADViewport@VisualRect> is set to (0, 0)-(100, 100)>
       <LI=<See Property=TCADViewport@DisableMouseEvents> is set to <B=False> >
       <LI=<See Property=TCADViewport@DisableRepaintEvents> is set to <B=False> >
       <LI=<See Property=TCADViewport@AspectRatio> is set to 0.0>
       <LI=<See Property=TCADViewport@RubberPenColor> is set to clRed>
       <LI=<See Property=TCADViewport@BackGroundColor> is set to clSilver>
       <LI=<See Property=TCADViewport@GridColor> is set to clGray>
       <LI=<See Property=TCADViewport@GridDeltaX> is set to 10>
       <LI=<See Property=TCADViewport@GridDeltaY> is set to 10>
       <LI=<See Property=TCADViewport@ControlPointsWidth> is set to 4>
       <LI=<See Property=TCADViewport@ControlPointsColor> is set to <B=clRed> >
       <LI=<See Property=TCADViewport@ShowControlPoints> is set to <B=True> >
       <LI=<See Property=TCADViewport@ShowGrid> is set to <B=True> >
       <LI=<See Property=TCADViewport@UsePaintingThread> is set to <B=False> >
       <LI=<See Property=TCADViewport@CopingFrequency> is set to 100>
    }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: This method forces to rebuild the mapping tranform.

       When the visual rect change this method will be called and the
       viewport repainted. If you want to force the mapping tranform
       to be updated call this method (this is useful when you derive a
       new viewport that has its own projection transform).
    }
    procedure UpdateViewportTransform;
    {: This method scale the visual rect to correspond to a speficied
       dimension in millimeters.

       <I=XScale> is the horizontal scale so that 1 drawing unit
       will be 1/XScale millimeters, <I=YScale> is the vertical scale
       so that 1 drawing unit will be 1/YScale millimeters. For example
       if XScale=2 then a rectangle wide 50 drawing units will be
       25 mm wide.

       <B=Note>: The method relies on the Windows setting for
       HORZRES and VERTRES of a device. Some times (this is true
       most of the time for a screen) these values are not
       correctly set and so the resulting drawing might have
       an incorrect dimension.
    }
    procedure Calibrate(const XScale, YScale: TRealType);
    {: This method set the visual rect to a specified rectangle.

       <I=NewWindow> is the new visual rect. This correspond to
       zoom the viewport to a specified portion of the view plane.
       The rectangle is in view plane coordinates.
    }
    procedure ZoomWindow(const NewWindow: TRect2D);
    {: This method moves the current visual rect to a specified
       position.

       <I=NewStartX> is the new X position of the lower-left corner
       of visual rect; <I=NewStartY> is the new y position of the lower-left corner
       of visual rect.

       The two coordinates are referred to view plane coordinates.
    }
    procedure MoveWindow(const NewStartX, NewStartY: TRealType);
    {: This method enlarges the current visual rect.

       The actual visual rect is doubled on both X and Y directions,
       resulting in a magnification of the current view of the drawing.
    }
    procedure ZoomIn;
    {: This method reduces the current visual rect.

       The actual visual rect is halfed on both X and Y directions,
       resulting in a reduction of the current view of the drawing.
    }
    procedure ZoomOut;
    {: This method zooms the drawing so that all of it is visible.

       Because the extension of a drawing depends on the dimension of
       it (2d, 3d and so on), this is an abstract method that must be
       implemented in all derived components (if applicable).

       <B=Note>: if the aspect ratio of the viewport is specified then
       the visual rect may be greater than the extension of the drawing.
    }
    procedure ZoomToExtension; virtual; abstract;
    {: This method pans the visual rect by a specified ammount in
       both X and Y directions.

       <I=DeltaX> is the ammount of pan in the X direction and
       <I=DeltaY> is the ammount of pan in the Y direction. Both
       of them are specified in view plane coordinates.
    }
    procedure PanWindow(const DeltaX, DeltaY: TRealType);
    {: This method copy the current viewport contents in another canvas.

       <I=Cnv> is the destination canvas; <I=Mode> is the mode of the
       copy (see <See Type=TCanvasCopyMode>); <I=View> is the view to
       be copied (see <See Type=TCanvasCopyView>); <I=XScale> and
       <I=YScale> are used only if <I=View> is <I=cvScale> and have
       the same meaning of <I=XScale> and <I=YScale> of the
       <See Method=TCADViewport@Calibrate> method.

       The diplay list is traversed and the objects are drawed onto
       the destination canvas. This method is useful to copy the
       current view (or the extension of the drawing) to the clipboard
       as a bitmap or metafile (depending on the type of canvas) picture.
       It is also useful in printing the drawing on the printer, when
       the destination canvas is the canvas of the printer.

       See also <See Method=TCADViewport@CopyToClipboard>.
    }
    procedure CopyToCanvas(const Cnv: TCanvas; const Mode: TCanvasCopyMode; const View: TCanvasCopyView; const XScale, YScale: TRealType); virtual;
    {: This method copy the current viewport contents to the clipboard.

       The backbuffer is copied into the clipboard as a bitmap data.

       See also <See Method=TCADViewport@CopyToCanvas>.
    }
    procedure CopyToClipboard(const Clp: TClipboard);
    {: This method start a group of updates so that only one repainting
       of the viewport take place at the end of the group.

       Normally the viewport is repainted as soon as the visual rect is
       changed. If you want to perform a group of such changes to obtain
       a special visual rect, the viewport is repainted a lot of times,
       reducing the performance of your application (specially if the
       drawing is complex). To resolve this problem you may want to call
       this method just before starting to changes to the visual rect, and
       then calling <See Method=TCADViewport@EndUpdate> to end the group.
       When you call <See Method=TCADViewport@EndUpdate> the visual rect is
       changed and the viewport is repainted. Only one
       <See Property=TCADViewport@OnPaint> event is fired.
    }
    procedure BeginUpdate;
    {: This method end a group of updates so that only one repainting
       of the viewport take place at the end of the group.

       Normally the viewport is repainted as soon as the visual rect is
       changed. If you want to perform a group of such changes to obtain
       a special visual rect, the viewport is repainted a lot of times,
       reducing the performance of your application (specially if the
       drawing is complex). To resolve this problem you may want to call
       <See Method=TCADViewport@BeginUpdate> just before starting to changes to
       the visual rect, and then calling this method to end the group.
       When you call this method the visual rect is
       changed and the viewport is repainted. Only one
       <See Property=TCADViewport@OnPaint> event is fired.
    }
    procedure EndUpdate;
    {: This method repaints the viewport contents.

       When you call this method the display list, of the <See Class=TCADCmp>
       control associated to the viewport, is traversed and the objects contained
       in it are drawed on the viewport's off-screen buffer.

       At the end of the traversion a <See Property=TCADViewport@OnPaint> event
       is fired.

       The redrawing process cannot be interrupted if you don't use a
       painting thread. If you use it then you can interrupt the process
       by calling <See Method=TCADViewport@StopRepaint>. However if you
       use a painting thread the process takes a longer time.
    }
    procedure Repaint; override;
    procedure Invalidate; override;
    {: This method refreshes the viewport contents.

       The refresh consist of a copy of the off screen buffer on the
       canvas of the control. This refresh is very useful to remove
       spurious drawings of objects drawed directly on the canvas of
       the control.

       This process is faster than repainting the viewport but
       don't reflect changing in the objects already drawed by a repaint.
    }
    procedure Refresh;
    {: This method repaints a partion of the visual rect.

       <I=ARect> is the portion of the visual rect to be repainted.
       When you call this method the display list, of the <See Class=TCADCmp>
       control associated to the viewport, is traversed and the objects contained
       , also partially, in the <I=ARect> portion of the visual rect
       are drawed on the viewport's off-screen buffer.

       At the end of the traversion a <See Property=TCADViewport@OnPaint> event
       is fired.

       The redrawing process cannot be interrupted if you don't use a
       painting thread. If you use it then you can interrupt the process
       by calling <See Method=TCADViewport@StopRepaint>. However if you
       use a painting thread the process takes a longer time.
    }
    procedure RepaintRect(const ARect: TRect2D);
    {: This method refreshes a portion of the viewport contents.

       The refresh consist of a copy of the off screen buffer on the
       canvas of the control. This refresh is very useful to remove
       spurious drawings of objects drawed directly on the canvas of
       the control. Only the portion specified by <I=ARect> is
       copied.

       This process is faster than repainting the viewport but
       don't reflect changing in the objects already drawed by a repaint.
    }
    procedure RefreshRect(const ARect: TRect);
    {: This method interrupts a repaint process.

       Only if you are using the painting threads this method is
       able to interrupt a repaint process. Otherwise it does nothing.
    }
    procedure StopRepaint;
    {: Wait for the painting thread to be finished.

       When you use the painting thread to repaint the viewport, you
       may want to wait for it to be finished. This method returns
       only when the active painting thread (if any) is finished.

       If you don't use the painting threads to repaint the viewport
       this method does nothing.
    }
    procedure WaitForRepaintEnd;
    {: This method returns the 2D point in view plane coordinates that
       correspond to the specified point in Windows screen coordinates.

       <I=SPt> is the 2D point in Windows screen coordinates to be
       transformed. It is of type <See Type=TPoint2D> but in fact it
       corresponds to a TPoint. Use <See function=PointToPoint2D>
       function to obtain this value.

       This method is useful to mimics the viewport mapping to the screen.
    }
    function  ScreenToViewport(const SPt: TPoint2D): TPoint2D; virtual;
    {: This method returns the point in Windows screen coordinates
       that correspond to a specified point in view plane coordinates.

       <I=WPt> is the 2D point in view plane coordinates to be
       transformed. The resulting point is of type <See Type=TPoint2D>
       but in fact it corresponds to a TPoint. Use <See function=Point2DToPoint>
       function to obtain the TPoint value from the result.
    }
    function  ViewportToScreen(const WPt: TPoint2D): TPoint2D; virtual;
    {: This method returns the width of a square in pixel in view plane coordinates.
       This method is used
       by the <See Method=TCADViewport2D@PickObject> (<See Method=TCADViewport3D@PickObject>)
       method.

       <I=L> is the width of the square in pixel that must be trasformed in
       view plane coordinates.
    }
    function  GetAperture(const L: Word): TRealType;
    {: This method sets the viewport with the settings of the specified
       layer.

       The method returns <B=True> if the layer is visible.
    }
    function SetLayer(const L: TLayer): Boolean;
    {: This property contains the <See Class=TCADCmp> control that
       acts as the source for the drawing to be painted in the
       viewport.

       The CADCmp contains the display list of the drawing that will
       be rendered in the canvas of the viewport control.

       You must assign it before using the viewport.
    }
    property CADCmp: TCADCmp read fCADCmp write SetCADCmp;
    {: This property contains the off screen canvas used to
       store the drawing before copying it to the canvas of the
       control.

       By using an off screen buffer the flickering is greatly
       reduced.

       See also <See Class=TDecorativeCanvas@TDecorativeCanvas>.
    }
    property OffScreenCanvas: TDecorativeCanvas read fOffScreenCanvas;
    {: This property contains the on-screen canvas used to
       draw directly on screen.

       See also <See Class=TDecorativeCanvas@TDecorativeCanvas>.
    }
    property OnScreenCanvas: TDecorativeCanvas read fOnScreenCanvas;
    {: This property contains the mapping transform from the
       view plane coordinate system to the Windows screen coordinate
       system.

       This transform matrix is computed in the
       <See Method=TCADViewport@BuildViewportTransform> method.
    }
    property ViewportToScreenTransform: TTransf2D read GetViewportToScreen;
    {: This property contains the mapping transform from the
       Windows screen coordinate system to the view plane coordinate system.

       This transform matrix is computed as the inverse of the
       transform returned by the
       <See Method=TCADViewport@BuildViewportTransform> method.
    }
    property ScreenToViewportTransform: TTransf2D read GetScreenToViewport;
    {: This property contains the portion of the view plane that is
       rendered in the canvas of the control.

       Only the objects contained in this portion of the plane are
       drawed on the screen (by using clipping).
    }
    property VisualRect: TRect2D read fVisualWindow write ZoomWindow;
    {: This property may contains a <See Class=TGraphicObjList> instance
       to be used instead of the display list of the associated TCADCmp
       control.

       If this property references an instance then it will be used
       in the <See Method=TCADViewport@Repaint> method. Otherwise the
       object's list of the associated TCADCmp is used.
    }
    property ViewportObjects: TGraphicObjList read fViewportObjects write fViewportObjects;
    {: This property is <B=True> when the viewport is inside an
       update block.

       See also <See Method=TCADViewport@BeginUpdate> and
       <See Method=TCADViewport@EndUpdate>.
    }
    property InUpdating: Boolean read fInUpdate;
    {: This property is <B=True> when the viewport is traversing the
       diplay list.

       When you call the <See Method=TCADViewport@Repaint> method this
       property becames <B=True>.
    }
    property InRepainting: Boolean read GetInRepaint;
    {: This property is used to inhibit the mouse events fired by the
       viewport.

       If it is <B=False> (the default) the mouse event are fired by
       the control, otherwise they are not.
    }
    property DisableMouseEvents: Boolean read fDisableMouseEvents write fDisableMouseEvents;
    {: This property is used to inhibit the repaint event fired by the
       viewport when it has repainted.

       If it is <B=False> (the default) the <See Property=TCADViewport@OnPaint>
       event is fired when the repaint process is finished, otherwise it is not.
    }
    property DisableRepaintEvents: Boolean read fDisablePaintEvent write fDisablePaintEvent;
    {: This property may contains an integer value that is passed
       to the drawing methods of the graphics object being rendered on
       the viewport.

       This is used to change the way in which the objects are drawed
       (for instance backface culled, only bounding boxes and so on).

       Normally this value contains a bit field that is checked by
       the object.
    }
    property DrawMode: Cardinal read fDrawMode write fDrawMode;
    {: This property contains the pen istance used in the rubber
       pen method.

       When the <See Method=TCADViewport@DrawObjectWithRubber> method
       is called the canvas on which draw the object is set with
       this pen. By default this method contains a pen with the
       style property equal to <B=psXOr> and color <B=clRed>.

       See also <See Property=TCADViewport@RubberPenColor> to change the
       color of the pen.
    }
    property RubberPen: TPen read fRubberPen;
  published
    { Published declarations }
    property Align;
    property Enabled;
    property Visible;
    property PopupMenu;
    property Height default 50;
    property Width default 50;
    {: This property contains the aspect ratio of the viewport.

       The aspect ratio is the ratio Width/Heigth of the visual rect
       that must be preserved.

       If it is 0.0 (the default) no aspect ration is preserved and
       the visual rect may take any shape. Otherwise the visual rect
       is forced to have the given aspect ratio.

       <B=Note>: This aspect is not the one of the screen that is taken
       into account automatically.
    }
    property AspectRatio: TRealType read FAspectRatio write FAspectRatio;
    {: This property contains the color of the <See Property=TCADViewport@RubberPen>
       pen.

       This color take into account the fact that the rubber pen is in
       XOr style.

       By default it is <B=clRed>.
    }
    property RubberPenColor: TColor read fRubberPenColor write SetRubberColor default clRed;
    {: This property contains the filling color of the control
       points.

       By default it is <B=clRed>.
    }
    property ControlPointsColor: TColor read fControlPointsColor write fControlPointsColor default clRed;
    {: This property contains the color of the background of the viewport.

       By default it is <B=clSilver>.
    }
    property BackGroundColor: TColor read FBackGroundColor write SetBackColor default clSilver;
    {: This property contains the color of reference grid of the viewport.

       By default it is <B=clGray>.
    }
    property GridColor: TColor read FGridColor write SetGridColor default clGray;
    {: This property specify the Z-order of the grid respect to
       the drawing. If it is false (the default) the grid will be
       drawed before any other shape and so will be covered by
       the drawing, otherwise it will be on top.
    }
    property GridOnTop: Boolean read fGridOnTop write fGridOnTop default False;
    {: This property contains the main step of the reference grid along
       the X axis.

       The grid property is now subdivided in four different parts:
       - GridDeltaX, GridDeltaY are equivalent to the old GridStep property
       - GridSubX, GridSubY are the number of subdivision of GridDelta?

       For example the following configuration:
       - GridDeltaX=10, GridDeltaY=10, GridSubX=5, GridSubY=5
       means a grid with a step of 10units along X and Y, and every 2
       units a dotted line.
    }
    property GridDeltaX: TRealType read fGridDeltaX write SetDeltaX;
    {: This property contains the main step of the reference grid along
       the Y axis.

       The grid property is now subdivided in four different parts:
       - GridDeltaX, GridDeltaY are equivalent to the old GridStep property
       - GridSubX, GridSubY are the number of subdivision of GridDelta?

       For example the following configuration:
       - GridDeltaX=10, GridDeltaY=10, GridSubX=5, GridSubY=5
       means a grid with a step of 10units along X and Y, and every 2
       units a dotted line.
    }
    property GridDeltaY: TRealType read fGridDeltaY write SetDeltaY;
    {: This property contains the sub step of the reference grid along
       the X axis.

       The grid property is now subdivided in four different parts:
       - GridDeltaX, GridDeltaY are equivalent to the old GridStep property
       - GridSubX, GridSubY are the number of subdivision of GridDelta?

       For example the following configuration:
       - GridDeltaX=10, GridDeltaY=10, GridSubX=5, GridSubY=5
       means a grid with a step of 10units along X and Y, and every 2
       units a dotted line.
    }
    property GridSubX: TRealType read fGridSubX write SetSubX;
    {: This property contains the sub step of the reference grid along
       the Y axis.

       The grid property is now subdivided in four different parts:
       - GridDeltaX, GridDeltaY are equivalent to the old GridStep property
       - GridSubX, GridSubY are the number of subdivision of GridDelta?

       For example the following configuration:
       - GridDeltaX=10, GridDeltaY=10, GridSubX=5, GridSubY=5
       means a grid with a step of 10units along X and Y, and every 2
       units a dotted line.
    }
    property GridSubY: TRealType read fGridSubY write SetSubY;
    {: An object can have control points. This property sets the
       dimension in pixels of these control points when they are
       drawed.
    }
    property ControlPointsWidth: Byte read FControlPointsWidth write FControlPointsWidth default 6;
    {: An object can have control points. If this property is <B=True> the
       control points are drawed with the object (if it has them).
    }
    property ShowControlPoints: Boolean read FShowControlPoints write FShowControlPoints default False;
    {: If this property is <B=True> the reference grid of the viewport
       will be drawed.
    }
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default False;
    {: If this property is <B=True> a painting thread is used for
       the repaint process.

       The library can use a thread for the traversing of display list.
       Such a painting thread is a specilized process that runs parallel to
       the main thread of the application. A viewport can have only one
       running thread that can be interrupted at any moment by calling the
       <See Method=TCADViewport@StopRepaint>.

       Different viewports can have their own threads running
       concurrently, fasting the repainting process. They are also useful
       to allow the user to change the view parameters dinamically in
       real time.

       However when a painting thread is used, the repaint process take
       more time than if the thread use is disabled. For this the use
       of a painting thread is advisable only when there are more than
       an hundred of object to be drawed.

       By default it is <B=False>.
    }
    property UsePaintingThread: Boolean read fUseThread write fUseThread default False;
    {: If this property is <B=True> the the viewport will be transparent.

       By default it is <B=False>.

      <B=Note>: the transparency is avaiable only at run-time.
    }
    property IsTransparent: Boolean read fTransparent write SetTransparent default False;
    {: This property contains the number of objects that are drawed
       before the off screen buffer is copied onto the canvas of the
       control.

       By default its value is 100.

       Lower values make the repainting process slower.
    }
    property CopingFrequency: Integer read fCopingFrequency write fCopingFrequency default 0;
    {: This property may contain an event handler that is called
       just before a repaint operation is being started.
    }
    property OnBeginRedraw: TNotifyEvent read FOnBeginRedraw write FOnBeginRedraw;
    {: This property may contain an event handler that is called
       called after a repaint is finished (after the coping of the back buffer
       onto the on screen canvas).

       This event is fired before the <See Property=TCADViewport@OnPaint> event.
    }
    property OnEndRedraw: TNotifyEvent read FOnEndRedraw write FOnEndRedraw;
    {: This property may contain an event handler that is called
       after the back buffer is copied onto the on screen canvas.

       This event is fired after the <See Property=TCADViewport@OnEndRedraw>
       event.
    }
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    {: This property may contais an event handler that is called
       when the dimensions of the control are changed (and the
       off screen buffer is changed accordingly).

       The event is fired after the dimensions and the mapping
       trasforms are updated but before the repaint process take
       in place (and so before the
       <See Property=TCADViewport@OnPaint> event).
    }
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    {: This property may contains an event handler that is called when
       the view mapping transform is changed.

       This event is fired before the <See Property=TCADViewport@OnPaint> event.

       See also <See Method=TCADViewport@BuildViewportTransform>.
    }
    property OnViewMappingChanged: TNotifyEvent read fOnViewMappingChanged write fOnViewMappingChanged;
    property OnEnter;
    property OnExit;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    {: This property may contains an event handler that is called when
       the off screen buffer must be cleared before the drawing process.

       By default the off screen buffer is cleared with an uniform
       filling with the <See Property=TCADViewport@BackgroundColor> color.

       See also <See Type=TClearCanvas>.
    }
    property OnClearCanvas: TClearCanvas read fOnClear write SetOnClearCanvas;
  end;

  {: This class defines a ruler that can be linked to a Viewport to
     show the extension of its <See Property=TCADViewport@VisualRect>.

     The components isn't updated automatically, instead it defines
     methods that must be called from the application to keep it
     syncronized with the viewport.

     Normally the following events are used for updating the ruler:

     <LI=<I=OnEndRepaint> is used to redraw the ruler and to update
     its dimensions.>
     <LI=<I=OnMouseMove> is used to update the mark position on the
     ruler <See Method=TRuler@SetMark>.>
  }
  TRuler = class(TCustomControl)
  private
    fOwnerView: TCADViewport;
    fStepSize: TRealType;
    fFontSize, fSize, fStepDivisions: Integer;
    fOrientation: TRulerOrientationType;
    fTicksColor: TColor;

    procedure SetTicksColor(C: TColor);
    procedure SetOwnerView(V: TCADViewport);
    procedure SetOrientation(O: TRulerOrientationType);
    procedure SetSize(S: Integer);
    procedure SetFontSize(S: Integer);
    procedure SetStepSize(S: TRealType);
    procedure SetStepDivisions(D: Integer);
  protected
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    {: This is the constructor of the control.

       It creates a vertical white ruler with size of 20 and
       a step division of 5.
    }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    {: This method is used to move the position marker of the ruler.

       A ruler may have a position marker that is used to show the
       current viewport position on the ruler.

       <I=Value> is the position on the ruler of the mouse in view
       plane reference coordinate. The value to be passed depends
       on the orientation of the ruler. For example a vertical ruler
       may want to show the Y coordinates of the current mouse
       position on the view plane.
    }
    procedure SetMark(Value: TRealType);
  published
    property Align;
    {: This property contains the linked viewport used for the alignment
       of the ruler

       The ruler adjust its range by using the <See Property=TCADViewport@VisualRect>
       property.
    }
    property LinkedViewport: TCADViewport read fOwnerView write SetOwnerView;
    {: This property contains the background color used to paint the
       ruler background.

       By default its value is clWhite.
    }
    property Color default clWhite;
    {: This property contains the color of the thick mark used to
       show the step division of the ruler.

       By default it is clBlack.
    }
    property TicksColor: TColor read fTicksColor write SetTicksColor default clBlack;
    {: This property contains the orientation of the ruler.

       See <See Type=TRulerOrientationType> for details.

       By default it is <I=otVertical>.
    }
    property Orientation: TRulerOrientationType read fOrientation write SetOrientation default otVertical;
    {: This property contains the step between two ticks. If the
       zoom is too close, this value is automatically adjusted so the
       ticks are always visible.

       By default it is 20.
    }
    property StepSize: TRealType read fStepSize write SetStepSize;
    {: This property defines the division on the ruler. The
       division are showed with a large tick and with the number of
       its value.

       By default it is 5.
    }
    property StepDivisions: Integer read fStepDivisions write SetStepDivisions default 5;
    {: This property contains the width (height) of the vertical
       (horizontal) ruler.

       If the value is small it will be difficulty to read the values
       of the divisions.

       By default it is 20.
    }
    property Size: Integer read fSize write SetSize default 20;
    {: This property contains the size of the font used to show
       the values of the divisions.

       By default it is 6.
    }
    property FontSize: Integer read fFontSize write SetFontSize default 6;
  end;

  {: This class defines an object that can be used to extend the handling function
     of a 2D shape object. The methods <I=DrawControlPoints> and <I=OnMe> of the class
     are called to respectively to shows the control points (handling points) and
     to returns picking information.
     For instance it is possible to defines handlers to move, rotate objects and
     so on, having them active in different moments allowing different manipulation
     modes on the same shape.

     At the moment only the old way to edit shapes is defined, but it is now more
     easy to add other way to handle shapes.

     An handler can be reused among different shapes or have it associated
     on a particular shape (in this case the HandledObject property will
     be the same as the associated shape.
  }
  TObject2DHandler = class(TObject)
  private
    fHandledObject: TObject2D;
    fRefCount: Integer;
  public
    {: This constructor create a new handler and link it to AObject.
       After the creation of the handler it is possible to show the control points
       defined by the handler and to operate on it.

       <I=AObject> will assume ownership of the handler and so you don't have to
       free it by yourself (it is deleted when the linked object is deleted or
       the handler is unlinked from the shape).

       <B=NOTE>: You will use the <See Class=TObject2D@SetHandler> method to create and
       link the handler to a 2D shape object.
    }
    constructor Create(AObject: TObject2D);
    procedure FreeInstance; override;
    destructor  Destroy; override;
    {: This method is called by the library when it is necessary to show the control
       points.

       An 2D object may have a set of points that are called
       <I=control points> and that control the shape of the
       object. For example a circle may have two control points;
       the center of the circle and a point on the circle that
       specify the radious of it. At the same time it is possible to have
       control points (or better handling points) to operate on an object, like
       a baricentric handler used to move the object.

       <I=VT> is the the mapping transform that can be obtained
       with the <See Property=TCADViewport@ViewportToScreenTransform> property.
       <I=Cnv> is the canvas on which draw the control points, and
       <I=Width> is the width in pixel of the control points (that are
       drawed as a square).
    }
    procedure DrawControlPoints(const Sender: TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); dynamic; abstract;
    {: This returns the part of the object that is picked by a point.

       <I=Pt> is the picking point, <I=Aperture> is the picking aperture
       and <I=Distance> will contains the distance from <I=Pt> to
       the object.

       The method must returns one of these values (ordered from the highest
       priority to the lowest priority):

       <LI=<I=PICK_NOOBJECT> if <I=Pt> is not on the object. Distance
       will became <See const=MaxCoord>.>
       <LI=<I=PICK_INBBOX> if <I=Pt> is inside the bounding box of the
       object. Distance will be equal to Aperture.>
       <LI=<I=PICK_ONOBJECT> if <I=Pt> is on the outline of the object
       with a distance less than Aperture. Distance will be that
       distance.>
       <LI=<I=PICK_INOBJECT> if <I=Pt> is inside the outline of the
       object. Distance will be set to Aperture.>
       <LI=A value greater than or equal to zero if <I=Pt> is on any of
       the control points, in this case the resulting value is that
       control point. Distance will be the distance form the control
       point.
       <LI=A value between -100 and -3. Use this range to define your specific
       pick values to use in your operation tasks.>

       You must use this convention in your implementations of
       the method. (You can use the inherited method if some of these
       condition is the same as in the base class).
    }
    function OnMe(const Sender: TObject2D; Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; dynamic; abstract;
    {: This property contains the handled object of the handler.

       If this property is nil the handler is shared among different
       shapes.
    }
    property HandledObject: TObject2D read fHandledObject;
  end;

  TObject2DHandlerClass = class of TObject2DHandler;

  {: A <I=2D graphic object> is a graphic object that is defined on
     a 2D plane.

     The 2D object is defined in a special coordinate system referred
     here as the <I=model coordinate system or model system>. When an object is used by the
     library, the world coordinate system (or world system) are
     needed instead. So the object in model system is transformed by an
     matrix transform to obtain the object in the world system.
     This transform is called the <See Property=TObject2D@ModelTransform>.
     So when an object is drawed, for example, its points are first
     transformed from model system to world system by using this matrix,
     then the view transform (that the concatenation of the
     projection matrix with the mapping matrix) tranform these points
     to obtain the points in the screen system that are easily displayed.

     The 2D object has a 2D bounding box, that is the smallest
     axis aligned rectangle that fully contains the object.
     This rectangle is always in the world coordinate system and
     so it doesn't depend on the model transform. The bounding box
     must be computed in the
     <See Property=TGraphicObject@_UpdateExtension> method that have
     to be redefined.

     The model matrix is split in two parts. The first one, called
     the <I=current model matrix>, can be removed at any time or
     can be post multiplied with a new one.

     The second one, called the <I=saved model matrix>, is stored
     permanently inside the object and can only be removed
     by using the <See Method=TObject2D@RemoveTransform> method
     or substituted with the actual model matrix with the
     <See Method=TObject2D@ApplyTransform> method.
     The actual model matrix is the concatenation of the
     <I=saved model matrix> with the <I=current model matrix> in
     this order (remember that the order of concatenation of transform matrix
     is important).

     The presence of the <I=current model matrix> is helpfull to
     create operations that transform an object interactilvely and
     that can be cancelled if they are not what the user want to do.

     <B=Note>: To save space the <I=current model matrix> and the
     <I=saved model matrix> are stored in the object only if they
     are not equal to the <See const=IdentityTransf2D> constant.
  }
  TObject2D = class(TGraphicObject)
  private
    fSavedTransform, fModelTransform: ^TTransf2D;
    fDrawBoundingBox: Boolean;
    fBox: TRect2D;
    fHandler: TObject2DHandler;

    { Non modifica ModelTransform. }
    procedure SaveTransform(const T: TTransf2D);
  protected
    {: This method returns the <I=actual model matrix>.

       The actual model matrix is the concatenation of the
       <I=saved model matrix> with the <I=current model matrix> in
       this order (remember that the order of concatenation of transform matrix
       is important).

       See the <See Class=TObject2D> for details.
    }
    function  GetModelTransform: TTransf2D; virtual;
    {: This method change the <I=current model matrix> of the object.

       The method calls the <See Method=TGraphicObject@UpdateExtension> method
       to update the bounding box of the object.
       <I=Transf> is the ransform matrix that substituite the
       <I=current model matrix>.

       See the <See Class=TObject2D> for details about the model matrix
       of a 2D object.
    }
    procedure SetModelTransform(Transf: TTransf2D); virtual;
    {: This field contains the bounding box of the object.

       A bounding box is the smallest axis aligned rectangle that fully contains the object.
       This rectangle is always in the world coordinate system and
       so it doesn't depend on the model transform. The bounding box
       must be computed in the
       <See Property=TGraphicObject@_UpdateExtension> method that have
       to be redefined.

       Use this field directly to assign the computed bunding box.
    }
    property  WritableBox: TRect2D read fBox write fBox;
  public
    constructor Create(ID: LongInt);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method transform the object with a give transformation matrix.

       The <I=T> transform matrix will be post multiplied with the
       <I=current model matrix> and the result will take the place of
       the <I=current model matrix>.

       See the <See Class=TObject2D> class for details about the
       model matrix of a 2D object.
    }
    procedure Transform(const T: TTransf2D); dynamic;
    {: This method removes the <I=actual model transform>.

       By using this method you can remove both the <I=current model matrix>
       and the <I=saved model matrix>.
       Using this method is the only way to remove the <I=saved model matrix>
       and free all the memory used to store the transformation matrix.

       See the <See Class=TObject2D> class for details about the
       model matrix of a 2D object.
    }
    procedure RemoveTransform;
    {: This method saves the <I=current model transform>.

       The method saves the model matrix by setting the
       <I=saved model transform> to the result of the multiplication
       of the <I=saved model transform> with the
       <I=current model transform> in this order.

       After the call to the method the <I=current model transform> is
       set to the <See const=IdentityTransf2D> constant.

       This method is automatically called when the object is
       streamed. The method calls
       <See Method=TGraphicObject@UpdateExtension>.
    }
    procedure ApplyTransform; dynamic;
    {: This method moves the object.

       <I=DragPt> is the base point of the movement and <I=ToPt>
       is the destination point. The <I=actual model transform> will
       be substituited with a translation matrix from <I=DragPt> to
       <I=ToPt>.
    }
    procedure MoveTo(ToPt, DragPt: TPoint2D);
    {: This method transforms a point in world coordinate system in
       the corrisponding point in model coordinate system.

       The resulting point is the point that is transformed by
       the <I=actual model transform> into <I=Pt>.
    }
    function  WorldToObject(const Pt: TPoint2D): TPoint2D;
    {: This method return <B=True> if the object has an
       <I=actual model transform> different from <See const=IdentityTransf2D>.
    }
    function  HasTransform: Boolean; virtual;
    {: This method draws the object on a canvas.

       A 2D object have to implement this method to draw itself
       appropriately.

       <I=VT> is the the mapping transform that can be obtained
       with the <See Property=TCADViewport@ViewportToScreenTransform> property.
       <I=Cnv> is the canvas on which draw the control points
       <See Class=TDecorativeCanvas@TDecorativeCanvas>, and
       <I=DrawMode> is a constant that correspond to the value
       of the <See Property=TCADViewport@DrawMode> property of
       the CADViewport that calls this method. It may be used to
       change the behaviour of the drawing method.
       <I=ClipRect> is the 2D pixel drawing area in 2D coordinates, you
       can use it in all of the drawing methods.
    }
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); virtual; abstract;
    {: This method returns <B=True> if the object is visible in the
       portion of the view plane given by the
       <See Property=TCADViewport@VisualRect> property of the viewport that
       called the method.

       This method is used to prune the object that must not be
       drawed to save time in the drawing process.

       <I=Clip> is the portion of the view plane that must be rendered, and
       <I=DrawMode> is a constant that correspond to the value
       of the <See Property=TCADViewport@DrawMode> property of
       the CADViewport that calls this method. It may be used to
       change the behaviour of the drawing method.

       By default this method returns <B=True> if the bounding box
       of the object is contained (also partially) in the <I=Clip>
       rectangle.
    }
    function  IsVisible(const Clip: TRect2D; const {%H-}DrawMode: Integer): Boolean; virtual;
    {: This method draws only the control points of the object.

       An 2D object may have a set of points that are called
       <I=control points> and that control the shape of the
       object. For example a circle may have two control points;
       the center of the circle and a point on the circle that
       specify the radious of it.

       <I=VT> is the the mapping transform that can be obtained
       with the <See Property=TCADViewport@ViewportToScreenTransform> property.
       <I=Cnv> is the canvas on which draw the control points
       <See Class=TDecorativeCanvas@TDecorativeCanvas>, and
       <I=Width> is the width in pixel of the control points (that are
       drawed as a square).
       <I=ClipRect> is the 2D pixel drawing area in 2D coordinates, you
       can use it in all of the drawing methods.

       This method uses the linked <See Class=TObject2DHandler> to
       draw the control points. If no handler is linked then no control point
       is drawed.
    }
    procedure DrawControlPoints(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const Width: Integer); dynamic;
    {: This returns the part of the object that is picked by a point.

       <I=Pt> is the picking point, <I=Aperture> is the picking aperture
       and <I=Distance> will contains the distance from <I=Pt> to
       the object.

       The method returns one of these values (ordered from the highest
       priority to the lowest priority):

       <LI=<I=PICK_NOOBJECT> if <I=Pt> is not on the object. Distance
       will became <See const=MaxCoord>.>
       <LI=<I=PICK_INBBOX> if <I=Pt> is inside the bounding box of the
       object. Distance will be equal to Aperture.>
       <LI=<I=PICK_ONOBJECT> if <I=Pt> is on the outline of the object
       with a distance less than Aperture. Distance will be that
       distance.>
       <LI=<I=PICK_INOBJECT> if <I=Pt> is inside the outline of the
       object. Distance will be set to Aperture.>
       <LI=A value greater than or equal to zero if <I=Pt> is on any of
       the control points, in this case the resulting value is that
       control point. Distance will be the distance form the control
       point.

       You must use this convention in your implementations of
       the method. (You can use the inherited method if some of these
       condition is the same as in the base class).
    }
    function  OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; dynamic;
    {: This method set a new handler for the object to be used to handle it.
       Use this method for an handler that will be shared among different
       shapes.

       Call this method with <B=nil> to remove the current handler.
    }
    procedure SetSharedHandler(const Hndl: TObject2DHandler);
    {: This method set a new handler for the object to be used to handle it.
       A new handler will be created.

       Call this method with <B=nil> to remove the current handler.
    }
    procedure SetHandler(const Hndl: TObject2DHandlerClass);
    {: This property contains the <I=actual model transform> of the object.

       The 2D object is defined in a special coordinate system referred
       here as the <I=model coordinate system or model system>. When an object is used by the
       library, the world coordinate system (or world system) are
       needed instead. So the object in model system is transformed by an
       matrix transform to obtain the object in the world system.
       This transform is called the <I=ModelTransform>.
       So when an object is drawed, for example, its points are first
       transformed from model system to world system by using this matrix,
       then the view transform (that the concatenation of the
       projection matrix with the mapping matrix) tranform these points
       to obtain the points in the screen system that are easily displayed.
    }
    property ModelTransform: TTransf2D read GetModelTransform write SetModelTransform;
    {: This property contains the bounding box of the object.

       The 2D object has a 2D bounding box, that is the smallest
       axis aligned rectangle that fully contains the object.
       This rectangle is always in the world coordinate system and
       so it doesn't depend on the model transform. The bounding box
       must be computed in the
       <See Property=TGraphicObject@_UpdateExtension> method that have
       to be redefined.
    }
    property Box: TRect2D read fBox;
    {: If this property is <B=True> then the bounding box is drawed
       in the <See Method=TObject2D@DrawControlPoints>.
    }
    property DrawBoundingBox: Boolean read fDrawBoundingBox write fDrawBoundingBox;
    {: This property contains the current handler object.

       See <See Class=TObject2DHandler@TObject2DHandler> for details.
    }
    property Handler: TObject2DHandler read fHandler;
  end;

  TObject2DClass = class of TObject2D;

  {: This class defines a group of 2D objects, that is a close set
     of objects that share the same model transform.

     A container has a special behaviour for the picking operation.
     Since a container embrace a group of objects, the picking take
     effect at group level returning the ID of the selected object
     instead of its control point.

     A container can be used to group objects that must be moved or
     transformed as a whole. On the other hand, if you want to reuse
     a set of objects in different place on the drawing use the
     <See Class=TSourceBlock2D> class.
  }
  TContainer2D = class(TObject2D)
  private
    { List of objects in the container. }
    fObjects: TGraphicObjList;
  protected
    procedure _UpdateExtension; override;
  public
    {: This is the constructor for the class.

       It creates an instance of the container. This constructor needs:

       <LI=the ID of the container. This identifier univocally identifies
       the object in the CAD. See also <See Method=TCADCmp@AddObject>.>
       <LI=the array of objects that must be added to the set. After
       you have created the container it is possible to add and
       remove objects throught the <See Property=TContainer2D@Objects> property.>

       If you want to create a void container you have to supply an
       array with only one item set to <B=nil> (ie <B=[nil]>).

       The objects in the container are owner by the container
       itself and will be freed when the container is deleted. So it
       isn't possible to share objects between containers.
    }
    constructor Create(ID: LongInt; const Objs: array of TObject2D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method updates the references of the source blocks
       that are used in the container.

       This method is called automatically at the end of the loading
       process from a stream. Indeed when you save a container it
       may contains <See Class=TBlock2D> instances that references
       to <See Class=TSourceBlock2D> instances. When the application
       is closed these references are no longer valid, so the blocks
       must be relinked to the corrent source blocks.

       The method needs the iterator of source blocks list in order
       to relink the blocks. This iterator can be obtained from
       <See Property=TCADCmp@SourceBlocksIterator>.

       The method call the <See Method=TGraphicObject@UpdateExtension>.
    }
    procedure UpdateSourceReferences(const BlockList: TGraphicObjIterator);
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    procedure DrawControlPoints(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const Width: Integer); override;
    function  OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the list that contians the object in the container.

       The objects in the container are owned by the container itself. Use
       this property to manage these objects.
    }
    property Objects: TGraphicObjList read fObjects;
  end;

  {: This class defines a group of 2D objects that can be placed
     in different points of the drawing. This is the efficent
     way to compose a drawing from repeated part of it.

     This class defines a template that can be instantiated,
     and any instance has its own model transformation but share
     the same objects of the template. The instances of this
     template are obtained with <See Class=TBlock2D> class.

     This kind of object must be added to the CAD using the
     <See Method=TCADCmp2D@AddSourceBlock> and
     <See Class=TCADCmp2D@BlockObjects> methods.
  }
  TSourceBlock2D = class(TContainer2D)
  private
    fNReference: Word; { Number of reference }
    fName: TSourceBlockName;
    fLibraryBlock: Boolean;
  public
    {: This is the constructor of the class.

       A source block is referenced either by use of its ID or by use
       of its name.
       This constructor needs:

       <LI=the ID of the source block. This identifier univocally
       identify the source block in the CAD.>
       <LI=the name of the source block. The name is of type
       <See Type=TSourceBlockName>.>
       <LI=the array of objects that must be added to the source
       block. After you have created the source blocks it is possible
       to add and remove objects afterward throught the
       <See Property=TContainer2D@Objects> property.>

       The objects in the source block are owner by the source block
       itself and they will be freed when the source block is deleted.
       So it isn't possible to share objects between source block, but
       it is possible to share these objects using the <See Class=TBlock2D>
       class.
    }
    constructor Create(ID: LongInt; const Name: TSourceBlockName; const Objs: array of TObject2D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor Destroy; override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This property contains the name of the source block.

       See also <See Type=TSourceBlockName>.
    }
    property Name: TSourceBlockName read fName write fName;
    {: This property contains the number of instances of the
       source block, that is the number of <See Class=TBlock2D>
       objects that are linked to it.

       When a new block is created to reference a source block,
       this property is incremented by one. When the block is
       deleted this numeber is decremented by one.

       Only if this value is zero the source block can be deleted.
    }
    property NumOfReferences: Word read fNReference;
    {: This property tells if the source block is a library source
       block.

       A library source block is a source block that is shared among
       differert drawings, made up a library of symbols.

       When this property is <B=True>, the source block will not be
       saved in a drawing file, but can be stored in a library file
       by using the methods <See Method=TCADCmp@SaveLibrary>
       and <See Method=TCADCmp@LoadLibrary>.

       If this property is <B=False>, the source block will be
       saved in the drawing file.

       If <See Property=TGraphicObject@ToBeSaved> is <B=False> the
       source block will not be saved anyway.
    }
    property IsLibraryBlock: Boolean read fLibraryBlock write fLibraryBlock;
  end;

  {: This class defines an instance of a <See Class=TSourceBlock2D>.

     A block is an istance of a source block, namely a copy of the
     source block that has its own model transform but share the
     same objects that define the source block.

     A block is an indipendent entity when it is concerned with
     the model transform, but it depends on the source block for
     its shape.
  }
  TBlock2D = class(TObject2D)
  private
    { Reference to the source block. }
    fSourceBlock: TSourceBlock2D;
    fSourceName: TSourceBlockName;
    { Origin of the block. }
    fOriginPoint: TPoint2D;

    procedure SetSourceBlock(const Source: TSourceBlock2D);
    procedure SetOriginPoint(Pt: TPoint2D);
  protected
    procedure _UpdateExtension; override;
  public
    {: This is the constructor of the class.

       <I=Source> is the source block that is used to define
       the instance.
    }
    constructor Create(ID: LongInt; const Source: TSourceBlock2D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method updates the references of the block.

       This method is called automatically at the end of the loading
       process of the block from a stream. Indeed when you save a
       block the reference to the source block is no longer valid,
       so the block must be relinked to the correct source blocks
       instance.

       The method needs the iterator of the source blocks of a CAD in
       order to relink the block's source block. This iterator can be
       obtained by <See Property=TCADCmp@SourceBlocksIterator>
    }
    procedure UpdateReference(const BlockList: TGraphicObjIterator);
    procedure DrawControlPoints(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const Width: Integer); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the base position of the block.

       The origin point is the position of the (0, 0) inside of
       the source block that is referenced by the block.

       The origin point is only a visual reference and doesn't
       alter the model transform (but it is altered from it).
    }
    property OriginPoint: TPoint2D read fOriginPoint write SetOriginPoint;
    {: This property contains the reference to the source block used by the block.

       See also <See Class=TSourceBlock2D>.
    }
    property SourceBlock: TSourceBlock2D read fSourceBlock write SetSourceBlock;
    {: This property contains the name of the source block
       referenced by the block.

       It is used to save the source block reference into a stream,
       and it is used to relink the source block reference when the
       block is loaded back.
    }
    property SourceName: TSourceBlockName read fSourceName;
  end;

  {: This class defines a specialization of a <See Class=TCADCmp>
     control (see it for details).

     This CADCmp handles 2D objects and source blocks.
     This component must be used when you need to store 2D drawing.
  }
  TCADCmp2D = class(TCADCmp)
  private
    function GetExtension: TRect2D;
  protected
    procedure LoadObjectsFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveObjectsToStream(const Stream: TStream); override;
  public
    {: This method loads the blocks definitions (see <See Class=TSourceBlock2D>) from a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream that contains the blocks (and it must be
       positioned on the first block present). The stream must be
       created with the current version of the library.
       The blocks are created by reading the shape class registration index
       and creating the correct shape instance with the state present in the
       stream. Then this instance is saved in the list of objects of the
       source block.

       When a source block is loaded the objects that are contained in the
       source block are checked for recursive nesting of blocks. It is
       allowed to have a source block that use in its definition another
       source block, BUT this source block must be loaded before the
       source block that use it (this is automatically checked when you
       define a block). When the source block is loaded its
       <See Method=TContainer2D@UpdateSourceReferences> is called to
       update the references.

       If there is a block that references to a not defined source block
       a message will be showed and the source block will not be loaded.

       See also <See=oObject's Persistance@PERSISTANCE>.
    }
    procedure LoadBlocksFromStream(const Stream: TStream; const Version: TCADVersion); override;
    {: This method saves the blocks definitions (see <See Class=TSourceBlock2D>) to a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream into which save the source blocks,
       <I=AsLibrary> tells if list of source blocks must be saved as
       a library. A Library will contains only the source blocks that
       have the <See Property=TGraphicObject@ToBeSaved> property set to
       <B=True> and the <See Property=TSourceBlock2D@IsLibraryBlock>
       property set to <B=True>. This is useful to create library of
       blocks definition to be shared beetwen drawing.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure SaveBlocksToStream(const Stream: TStream; const AsLibrary: Boolean); override;
    {: This method adds a new source block.

       <I=ID> is the identifier number of the source block and Obj is the
       source block object. The library doesn't check for uniqueness of
       source block's ID. The given ID substitute the ID of Obj. A source
       block can also be referenced by its name and again the library
       doesn't check for its uniqueness.

       A source block is a collection of objects that are drawed at a whole
       on the viewport. A source block must be istantiated before the
       drawing occourse by define a block that references it. The instance
       has a position so you can share the same collection of objects and
       draw it in different places on the same drawing. This allows the
       efficent use of memory. If a source block is modified, all the
       instances are modified accordingly.

       The method returns the added block.

       See also <See Class=TSourceBlock2D> and
       <See Class=TBlock2D>.
    }
    function AddSourceBlock(const Obj: TSourceBlock2D): TSourceBlock2D;
    {: This method deletes the source block with the specified Name.

      <I=ID> is the identification number of the source block. If no such
      block is found a <See Class=ECADListObjNotFound> exception will be raised.

      If the source block to be delete is in use (that is it is referenced
      by some <See Class=TBlock2D> object) a
      <See Class=ECADSourceBlockIsReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteSourceBlock(const SrcName: TSourceBlockName);
    function  GetSourceBlock(const ID: LongInt): TSourceBlock2D;
    function  FindSourceBlock(const SrcName: TSourceBlockName): TSourceBlock2D;
    {: This method creates a source block (and add it to the CAD) by
       grouping a list of objects.

       <I=ScrName> will be the name of the source block; <I=Objs> is
       an iterator on the list that contains the objects to be added.
       That list must have the <See Property=TGraphicObjList@FreeOnClear>
       property set to <B=False>.
    }
    function  BlockObjects(const SrcName: TSourceBlockName; const Objs: TGraphicObjIterator): TSourceBlock2D;
    procedure DeleteLibrarySourceBlocks; override;
    procedure DeleteSavedSourceBlocks; override;
    function AddObject(const ID: LongInt; const Obj: TObject2D): TObject2D;
    function InsertObject(const ID, IDInsertPoint: LongInt; const Obj: TObject2D): TObject2D;
    {: This method adds a new block (instance of a source block) into
       the CAD.

       <I=ID> will be the identifier of the new block,
       and <I=SrcName> is the name of the source block used to
       define the block.

       If the source block isn't in the CAD an exception will be
       raised. The method returns the reference of the added block.
    }
    function  AddBlock(const ID: LongInt; const SrcName: TSourceBlockName): TObject2D;
    function  GetObject(const ID: LongInt): TObject2D;
    {: This method transforms a set of object or all the objects
       present in the CAD.

       The method applies the <I=T> transform matrix to the objects
       with the <I=ID> that are present in <I=ListOfObj>. This
       parameter is an array that may contains:

       <LI=only one element equal to -1. In this case <I=T> will be
       applied to all the objects present in the CAD.
       <LI=a list of integers greater or equal to zero. In this case
       <I=T> will be applied to the objects with the specified
       <I=IDs>. The array must not have duplicated IDs.

       If some ID in the array doesn't refer to an object in the CAD
       an <See Class=ECADListObjNotFound> exception will be raised
       and the operation stopped.
    }
    procedure TransformObjects(const ListOfObj: array of LongInt; const T: TTransf2D);
    procedure RedrawObject(const Obj: TObject2D);
    {: This property contains the extension of the drawing.

       The drawing extension is the smallest axis aligned
       rectangle that bounds all the objects in the CAD.
       When you refer to the property the extension rectangle is
       computed and this may require a bit of time for a complex draw.

       If you want to use this value more than once in a series of
       computations, it is better to store it in a local variable
       instead of use this property in all the computations.
    }
    property DrawingExtension: TRect2D read GetExtension;
  end;

  {: This component derives from <See Class=TCADViewport> and
     specialize it to handle 2D objects.

     In this case the world is the view plane of TCADViewport
     and so the projection transform is simply an indentity tranform matrix.

     See <See Class=TCADViewport> for details.
  }
  TCADViewport2D = class(TCADViewport)
  private
    fPickFilter: TObject2DClass; { consider only the objects with this type during the picking. }
    fCADCmp2D: TCADCmp2D;
    { Event handlers }
    fOnMouseDown2D, fOnMouseUp2D: TMouseEvent2D;
    fOnMouseMove2D: TMouseMoveEvent2D;

    procedure SetCADCmp2D(CAD2D: TCADCmp2D);
  protected
    procedure SetCADCmp(Cad: TCADCmp); override;
    procedure DrawObject(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D); override;
    procedure DrawObjectWithRubber(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D); override;
    function  BuildViewportTransform(var ViewWin: TRect2D; const ScreenWin: TRect; const AspectRatio: TRealType): TTransf2D; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    {: This method draws a 2D object on the viewport.

       <I=Obj> is that object to be drawed. If <I=CtrlPts> is <B=True>
       the control points of the object will also be drawed.

       The object is drawed by calling its <See Method=TObject2D@Draw>
       method if it is visible (that is it is contained in the
       <See Property=TCADViewport@VisualRect>)).
    }
    procedure DrawObject2D(const Obj: TObject2D; const CtrlPts: Boolean);
    {: This method draws a 2D object on the viewport.

       <I=Obj> is that object to be drawed. If <I=CtrlPts> is <B=True>
       the control points of the object will also be drawed.

       The object is drawed by calling its <See Method=TObject2D@Draw>
       method if it is visible (that is it is contained in the
       <See Property=TCADViewport@VisualRect>)).

       This method draws the object only on the canvas of the control
       and not in the off-screen buffer. So if you call the
       <See Method=TCADViewport@Refresh> method the effect of this
       method are removed without the need to a complete Repaint.
    }
    procedure DrawObject2DWithRubber(const Obj: TObject2D; const CtrlPts: Boolean);
    procedure CopyRectToCanvas(CADRect: TRect2D; const CanvasRect: TRect; const Cnv: TCanvas; const Mode: TCanvasCopyMode); override;
    function  GetCopyRectViewportToScreen(CADRect: TRect2D; const CanvasRect: TRect; const Mode: TCanvasCopyMode): TTransf2D; override;
    procedure ZoomToExtension; override;
    {: This method fill in a list with the objects that are contained
       in a rectangular region of the view plane.

       <I=ResultLst> is the list that will contain the objects found;
       <I=Frm> is a rectangle in view plane coordinates.
       <I=Mode> may be one of the following values:

       <LI=if it is <I=gmAllInside> then only the objects that
         are fully contained in the rectangle are inserted into the list.>
       <LI=if it is <I=gmCrossFrame> then the objects that are
         fully or partially contained in the rectangle are inserted
         into the list.>

       If <I=RemoveFromCAD> is <B=True> then the objects added to
       the list are also removed from the CAD.

       This method is useful for create a pick-in-region function,
       or to create a source block with the objects in a region
       by removing the objects found from the CAD.
    }
    procedure GroupObjects(const ResultLst: TGraphicObjList; Frm: TRect2D; const Mode: TGroupMode; const RemoveFromCAD: Boolean);
    {: This method returns an object that has the point <I=Pt> on it.

       The method traverses the list of objects to find an object
       that is at a distance from <I=Pt> less than <I=Aperture>. If
       that object is found then it will be returned by the method,
       otherwise the method will returns <B=nil>.

       <I=Pt> is the point of the picking; <I=Aperture> is the width
       of the picking region in <B=pixel>.

       <I=NPoint> will contains (if an object will be picked) one of
       the following values:

       <LI=<I=PICK_NOOBJECT> if no object is found at <I=Pt>.>
       <LI=<I=PICK_INBBOX> if an object is found that has <I=Pt> in its bounding box.>
       <LI=<I=PICK_ONOBJECT> if an object is found that has <I=Pt> on its outline.>
       <LI=<I=PICK_INOBJECT> if an object is found that has <I=Pt> in its interior.>
       <LI=a value equal or greater than zero if an object is found
         that has <I=Pt> on one of its control points.>

       The above value are ordered from the lowest priority to the
       greater priority. If more than one of the above condition is
       matched the value with the greater priority is returned.

       If there are more than one object at the point <I=Pt> the
       following rules are applied:

       <LI=The object with the greater priority value is returned>
       <LI=The last object added to the CAD is returned.>

       The display list is traversed until the last object is
       encountered and this may require a bit of time in the
       case of a complex object. If you are satisfied to found
       the first encountered object that is picked by the point
       <I=Pt> you may want to set <I=FirstFound> to <B=True>.

       <B=Note>: The method use the <See Method=TObject2D@OnMe> method
       to check for the picking. If you need a special picking function
       you may want to call that method.
    }
    function  PickObject(Pt: TPoint2D; Aperture: Word; FirstFound: Boolean; var NPoint: Integer): TObject2D;
    {: This method fills a list with the object that are at a distance
       from <I=Pt> less than <I=Aperture>.

       The method perform the same operation of <See Method=TCADViewport2D@PickObject>
       method but the objects that are picked by the point <I=Pt> are all
       added to the list <I=PickedObjects> in the same order as they
       are encountered.
    }
    function  PickListOfObjects(const PickedObjects: TList; Pt: TPoint2D; Aperture: Word): Integer;
    {: This method returns the point <I=WPt> in world coordinate system
       transformed by the inverse of the object model trasform of <I=Obj>.

       This method simply obtain the point <I=WPt> referenced in
       the object coordinate system of <I=Obj>.
    }
    function  WorldToObject(const Obj: TObject2D; WPt: TPoint2D): TPoint2D;
    {: This method returns the point <I=OPt> in object coordinate system
       transformed by the object model trasform of <I=Obj>.

       This method simply obtain the point <I=OPt> referenced in
       the world coordinate system.
    }
    function  ObjectToWorld(const Obj: TObject2D; OPt: TPoint2D): TPoint2D;
    {: This property may contains a class variable that is used to limit
       the picking to only the objects of that class.

       This property is used in <See Method=TCADViewport2D@PickObject>,
       <See Method=TCADViewport2D@PickListOfObjects> and
       <See Method=TCADViewport2D@GroupObjects>.
    }
    property  PickFilter: TObject2DClass read fPickFilter write fPickFilter;
  published
    {: This property contains the <See Class=TCADCmp2D> control that
       acts as the source for the drawing to be painted in the
       viewport.

       The CADCmp2D contains the display list of the drawing that will
       be rendered in the canvas of the viewport control.

       You must assign it before using the viewport.
    }
    property CADCmp2D: TCADCmp2D read fCADCmp2D write SetCADCmp2D;
    {: EVENTS}
    {: This property may contain an event-handler that will be
       called when the mouse in moved on the control.

       See <See Type=TMouseMoveEvent2D> for details on the event
       handler.
    }
    property OnMouseMove2D: TMouseMoveEvent2D read FOnMouseMove2D write FOnMouseMove2D;
    {: This property may contain an event-handler that will be
       called when the user presses a mouse button with the mouse pointer
       over the viewport.

       See <See Type=TMouseEvent2D> for details on the event
       handler.
    }
    property OnMouseDown2D: TMouseEvent2D read FOnMouseDown2D write FOnMouseDown2D;
    {: This property may contain an event-handler that will be
       called when the user releases a mouse button with the mouse pointer
       over the viewport.

       See <See Type=TMouseEvent2D> for details on the event
       handler.
    }
    property OnMouseUp2D: TMouseEvent2D read FOnMouseUp2D write FOnMouseUp2D;
  end;

  { Questa classe definisce un'ogetto che può essere utilizzato per estendere
    le modalità di manipolazione di un oggetto 2D. I metodi DrawControlPoints
    e OnMe di questo oggetto sono chiamati rispettivamente per visualizzare e
    restituire informazioni di picking in modo differente. Per esempio è possibile
    definire degli handlers specifici per spostare l'oggetto, per ruotarlo e via
    dicendo. In un qualsiasi momento è possibile cambiare l'handler attivo in
    modo da poter avere diverse modalità di gestione dell'oggetto.
    E' una classe astratta. }
  TObject3DHandler = class(TObject)
  private
    fHandledObject: TObject3D;
    fRefCount: Integer;
  public
    constructor Create(AObject: TObject3D);
    destructor  Destroy; override;
    procedure FreeInstance; override;

    procedure DrawControlPoints(const Sender: TObject3D; const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); dynamic; abstract;
    function  OnMe(const Sender: TObject3D; P: TPoint3D; const NormTransf: TTransf3D; Aperture: TRealType; var Distance: TRealType): Integer; dynamic; abstract;

    property HandledObject: TObject3D read fHandledObject;
  end;

  TObject3DHandlerClass = class of TObject3DHandler;
  
   {: A <I=3D graphic object> is a graphic object that is defined in
     a 3D world.

     The 3D object is defined in a special coordinate system referred
     here as the <I=model coordinate system or model system>. When an object
     is used by the library, the <I=world coordinate system (or world system)>
     are needed instead. So the object in model system is transformed
     by a transform matrix to obtain the object in the world system.
     This transform is called the <See Property=TObject3D@ModelTransform>.
     So when an object is drawed, for example, its points are first
     transformed from model system to world system by using this matrix,
     then the points are projected on the view plane and normalized
     (see <See Class=TCADViewport3D> for details),
     and finally they are mapped to the screen system and displayed
     (by using clipping in homogeneous coordinates).

     The 3D object has a 3D bounding box, that is the smallest
     axis aligned box that fully contains the object.
     This box is always in the world coordinate system and
     so it doesn't depend on the model transform. The bounding box
     must be computed in the
     <See Property=TGraphicObject@_UpdateExtension> method that have
     to be redefined.

     The model matrix is split in two parts. The first one, called
     the <I=current model matrix>, can be removed at any time or
     can be post multiplied with a new one.

     The second one, called the <I=saved model matrix>, is stored
     permanently inside the object and can only be removed
     by using the <See Method=TObject3D@RemoveTransform> method
     or substituted with the actual model matrix with the
     <See Method=TObject3D@ApplyTransform> method.
     The actual model matrix is the concatenation of the
     <I=saved model matrix> with the <I=current model matrix> in
     this order (remember that the order of concatenation of transform matrix
     is important).

     The presence of the <I=current model matrix> is helpfull to
     create operations that transform an object interactilvely and
     that can be cancelled if they are not what the user want to do.

     <B=Note>: To save space the <I=current model matrix> and the
     <I=saved model matrix> are stored in the object only if they
     are not equal to the <See const=IdentityTransf3D> constant.
  }
 TObject3D = class(TGraphicObject)
  private
    fSavedTransform, fModelTransform: ^TTransf3D;
    fDrawBoundingBox: Boolean;
    fBox: TRect3D;
    fHandler: TObject3DHandler;

    { Non modifica ModelTransform. }
    procedure SaveTransform(const T: TTransf3D);
  protected
    {: This method returns the <I=actual model matrix>.

       The actual model matrix is the concatenation of the
       <I=saved model matrix> with the <I=current model matrix> in
       this order (remember that the order of concatenation of transform matrix
       is important).

       See the <See Class=TObject3D> for details.
    }
    function GetModelTransform: TTransf3D; virtual;
    {: This method change the <I=current model matrix> of the object.

       The method calls the <See Method=TGraphicObject@UpdateExtension> method
       to update the bounding box of the object.
       <I=Transf> is the ransform matrix that substituite the
       <I=current model matrix>.

       See the <See Class=TObject3D> for details about the model matrix
       of a 3D object.
    }
    procedure SetModelTransform(Transf: TTransf3D); virtual;
    {: This field contains the bounding box of the object.

       A bounding box is the smallest axis aligned box that
       fully contains the object.
       This box is always in the world coordinate system and
       so it doesn't depend on the model transform. The bounding box
       must be computed in the
       <See Property=TGraphicObject@_UpdateExtension> method that have
       to be redefined.

       Use this field directly to assign the computed bunding box.
    }
    property WritableBox: TRect3D read fBox write fBox;
  public
    constructor Create(ID: LongInt);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    { Ritorna la descrizione a faccie dell'oggetto. FacePts deve essere già creato e viene cancellato dalla funzione.
      I indica l'indice della faccia. Ritorna False se non ci sono più faccie. }
    function GetObjectFaces(var {%H-}I: Integer; {%H-}FacePts: TPointsSet3D; var {%H-}FaceNormal: TVector3D; var {%H-}FaceID: Integer): Boolean; virtual;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method transform the object with a give transformation matrix.

       The <I=T> transform matrix will be post multiplied with the
       <I=current model matrix> and the result will take the place of
       the <I=current model matrix>.

       See the <See Class=TObject3D> class for details about the
       model matrix of a 3D object.
    }
    procedure Transform(T: TTransf3D); dynamic;
    {: This method removes the <I=actual model transform>.

       By using this method you can remove both the <I=current model matrix>
       and the <I=saved model matrix>.
       Using this method is the only way to remove the <I=saved model matrix>
       and free all the memory used to store the transformation matrix.

       See the <See Class=TObject3D> class for details about the
       model matrix of a 3D object.
    }
    procedure RemoveTransform;
    {: This method saves the <I=current model transform>.

       The method saves the model matrix by setting the
       <I=saved model transform> to the result of the multiplication
       of the <I=saved model transform> with the
       <I=current model transform> in this order.

       After the call to the method the <I=current model transform> is
       set to the <See const=IdentityTransf3D> constant.

       This method is automatically called when the object is
       streamed. The method calls
       <See Method=TGraphicObject@UpdateExtension>.
    }
    procedure ApplyTransform; dynamic;
    {: This method draws the object on a canvas.

       A 3D object have to implement this method to draw itself
       appropriately.

       <I=NormTransf> is the <I=normalization transform>
       (see <See Class=TCADViewport3D> for details>) that can be
       obtained with the <See Property=TCADViewport3D@ViewNormalization>
       property;
       <I=VT> is the the mapping transform that can be obtained
       with the <See Property=TCADViewport@ViewportToScreenTransform> property.
       <I=Cnv> is the canvas on which draw the control points, and
       <I=VRP> is the view reference point that is a point centered on the
       view plane and that may be useful to implement operations such
       as back face culling.

       <I=DrawMode> is a constant that correspond to the value
       of the <See Property=TCADViewport@DrawMode> property of
       the CADViewport that calls this method. It may be used to
       change the behaviour of the drawing method.
    }
    procedure {%H-}Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); virtual; abstract;
    {: This method returns <B=True> if the object is contained
       in the normalized view volume (see <See Class=TCADViewport3D>).

       This method is used to prune the object that must not be
       drawed to save time in the drawing process.

       <I=NormalTransf> is the projection-normalization matrix
       as given by <See Property=TCADViewport3D@ViewNormalization>.
       <I=VRP> is the view reference point that is a point centered on the
       view plane and that may be useful to implement operations such
       as back face culling.

       <I=DrawMode> is a constant that correspond to the value
       of the <See Property=TCADViewport@DrawMode> property of
       the CADViewport that calls this method. It may be used to
       change the behaviour of the drawing method.

       By default this method returns <B=True> if the bounding box
       of the object is contained (also partially) in normalized view
       volume.
    }
    function  IsVisible(const NormTransf: TTransf3D; const {%H-}VRP: TPoint3D; const {%H-}DrawMode: Integer): Boolean; virtual;
    {: This method moves the object.

       <I=DragPt> is the base point of the movement and <I=ToPt>
       is the destination point. The <I=actual model transform> will
       be substituited with a translation matrix from <I=DragPt> to
       <I=ToPt>.
    }
    procedure MoveTo(ToPt, DragPt: TPoint3D);
    {: This method transforms a point in world coordinate system in
       the corrisponding point in model coordinate system.

       The resulting point is the point that is transformed by
       the <I=actual model transform> into <I=Pt>.
    }
    function  WorldToObject(const Pt: TPoint3D): TPoint3D;
    {: This property contains the <I=actual model transform> of the object.

       The 3D object is defined in a special coordinate system referred
       here as the <I=model coordinate system or model system>. When an object is used by the
       library, the world coordinate system (or world system) are
       needed instead. So the object in model system is transformed by an
       matrix transform to obtain the object in the world system.
       This transform is called the <See Property=TObject2D@ModelTransform>.
       So when an object is drawed, for example, its points are first
       transformed from model system to world system by using this matrix,
       then the view transform (that the concatenation of the
       projection matrix with the mapping matrix) tranform these points
       to obtain the points in the screen system that are easily displayed.
    }
    function  HasTransform: Boolean; virtual;
    {: This method draws only the control points of the object.

       An 3D object may have a set of points that are called
       <I=control points> and that control the shape of the
       object. For example a cilinder may have two control points;
       the center of the base circle and a point on the base circle
       that specify the radious of it.

       <I=NormTransf> is the <I=normalization transform>
       (see <See Class=TCADViewport3D> for details>) that can be
       obtained with the <See Property=TCADViewport3D@ViewNormalization>
       property;
       <I=VT> is the the mapping transform that can be obtained
       with the <See Property=TCADViewport@ViewportToScreenTransform> property.
       <I=Cnv> is the canvas on which draw the control points, and
       <I=Width> is the width in pixel of the control points (that are
       drawed as a square).
       <I=VRP> is the view reference point that is a point centered on the
       view plane and that may be useful to implement operations such
       as back face culling.
    }
    procedure DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); dynamic;
    {: This method returns the object in the drawing that is at a distance
       from <I=P> less than <I=Aperture>.

       <I=Pt> is the picking point in normalized coordinate system,
       <I=Aperture> is the picking aperture
       and <I=Distance> will contains the distance from <I=Pt> to
       the object.

       <I=NormTransf> is the projection-normalization transform that
       project points in the world coordinates system in the points
       in the normalized view volume (that can be pojected on the
       view plane by calling <See Function=Point3DToPoint2D>. This
       matrix is used to transform the object's points before the
       check against <I=P> (that is already in the normalized
       view volume).

       The method returns one of these values (ordered from the highest
       priority to the lowest priority):

       <LI=<I=PICK_NOOBJECT> if <I=Pt> is not on the object. Distance
       will became <See const=MaxCoord>.>
       <LI=<I=PICK_INBBOX> if <I=Pt> is inside the bounding box of the
       object. Distance will be equal to Aperture.>
       <LI=<I=PICK_ONOBJECT> if <I=Pt> is on the outline of the object
       with a distance less than Aperture. Distance will be that
       distance.>
       <LI=<I=PICK_INOBJECT> if <I=Pt> is inside the outline of the
       object. Distance will be set to Aperture.>
       <LI=A value greater than or equal to zero if <I=Pt> is on any of
       the control points, in this case the resulting value is that
       control point. Distance will be the distance form the control
       point.

       You must use this convention in your implementations of
       the method. (You can use the inherited method if some of these
       condition is the same as in the base class).

       <B=Note>: This method implements a 2D picking on the
       projected points.
    }
    function  OnMe(P: TPoint3D; const NormTransf: TTransf3D; Aperture: TRealType; var Distance: TRealType): Integer; dynamic;
    procedure SetSharedHandler(const Hndl: TObject3DHandler);
    procedure SetHandler(const Hndl: TObject3DHandlerClass);

    {: This property contains the <I=actual model transform> of the object.

       The 2D object is defined in a special coordinate system referred
       here as the <I=model coordinate system or model system>. When an object is used by the
       library, the world coordinate system (or world system) are
       needed instead. So the object in model system is transformed by an
       matrix transform to obtain the object in the world system.
       This transform is called the <I=ModelTransform>.
       So when an object is drawed, for example, its points are first
       transformed from model system to world system by using this matrix,
       then the view transform (that the concatenation of the
       projection matrix with the mapping matrix) tranform these points
       to obtain the points in the screen system that are easily displayed.
    }
    property ModelTransform: TTransf3D read GetModelTransform write SetModelTransform;
    {: This property contains the bounding box of the object.

       The 3D object has a 3D bounding box, that is the smallest
       axis aligned box that fully contains the object.
       This box is always in the world coordinate system and
       so it doesn't depend on the model transform. The bounding box
       must be computed in the
       <See Property=TGraphicObject@_UpdateExtension> method that have
       to be redefined.
    }
    property Box: TRect3D read FBox;
    {: If this property is <B=True> then the bounding box is drawed
       in the <See Method=TObject3D@DrawControlPoints>.
    }
    property DrawBoundingBox: Boolean read fDrawBoundingBox write fDrawBoundingBox;
    property Handler: TObject3DHandler read fHandler;
  end;

  TObject3DClass = class of TObject3D;

  {: This class defines a group of 3D objects, that is a close set
     of objects that share the same model transform.

     A container has a special behaviour for the picking operation.
     Since a container embrace a group of objects, the picking take
     effect at group level returning the ID of the selected object
     instead of its control point.

     A container can be used to group objects that must be moved or
     transformed as a whole. On the other hand, if you want to reuse
     a set of objects in different place on the drawing use the
     <See Class=TSourceBlock3D> class.
  }
  TContainer3D = class(TObject3D)
  private
    { List of objects in the container. }
    FObjects: TGraphicObjList;
  protected
    procedure _UpdateExtension; override;
  public
    {: This is the constructor for the class.

       It creates an instance of the container. This constructor needs:

       <LI=the ID of the container. This identifier univocally identifies
       the object in the CAD. See also <See Method=TCADCmp@AddObject>.>
       <LI=the array of objects that must be added to the set. After
       you have created the container it is possible to add and
       remove objects throught the <See Property=TContainer3D@Objects> property.>

       If you want to create a void container you have to supply an
       array with only one item set to <B=nil> (ie <B=[nil]>).

       The objects in the container are owner by the container
       itself and will be freed when the container is deleted. So it
       isn't possible to share objects between containers.
    }
    constructor Create(ID: LongInt; const Objs: array of TObject3D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method updates the references of the source blocks
       that are used in the container.

       This method is called automatically at the end of the loading
       process from a stream. Indeed when you save a container it
       may contains <See Class=TBlock3D> instances that references
       to <See Class=TSourceBlock3D> instances. When the application
       is closed these references are no longer valid, so the blocks
       must be relinked to the corrent source blocks.

       The method needs the iterator of source blocks list in order
       to relink the blocks. This iterator can be obtained from
       <See Property=TCADCmp@SourceBlocksIterator>.

       The method call the <See Method=TGraphicObject@UpdateExtension>.
    }
    procedure UpdateSourceReferences(const BlockList: TGraphicObjIterator);
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    procedure DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); override;
    function  OnMe(P: TPoint3D; const NT: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;

    {: This property contains the list that contians the object in the container.

       The objects in the container are owned by the container itself. Use
       this property to manage these objects.
    }
    property Objects: TGraphicObjList read FObjects;
  end;

  {: This class defines a group of 3D objects that can be placed
     in different points of the drawing. This is the efficent
     way to compose a drawing from repeated part of it.

     This class defines a template that can be instantiated,
     and any instance has its own model transformation but share
     the same objects of the template. The instances of this
     template are obtained with <See Class=TBlock3D> class.

     This kind of object must be added to the CAD using the
     <See Method=TCADCmp3D@AddSourceBlock> and
     <See Class=TCADCmp3D@BlockObjects> methods.
  }
  TSourceBlock3D = class(TContainer3D)
  private
    FNReference: Word; { Number of reference }
    FName: TSourceBlockName;
    fLibraryBlock: Boolean;
  public
    {: This is the constructor of the class.

       A source block is referenced either by use of its ID or by use
       of its name.
       This constructor needs:

       <LI=the ID of the source block. This identifier univocally
       identify the source block in the CAD.>
       <LI=the name of the source block. The name is of type
       <See Type=TSourceBlockName>.>
       <LI=the array of objects that must be added to the source
       block. After you have created the source blocks it is possible
       to add and remove objects afterward throught the
       <See Property=TContainer3D@Objects> property.>

       The objects in the source block are owner by the source block
       itself and they will be freed when the source block is deleted.
       So it isn't possible to share objects between source block, but
       it is possible to share these objects using the <See Class=TBlock3D>
       class.
    }
    constructor Create(ID: LongInt; const Name: TSourceBlockName; const Objs: array of TObject3D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;

    {: This property contains the name of the source block.

       See also <See Type=TSourceBlockName>.
    }
    property Name: TSourceBlockName read FName write FName;
    {: This property contains the number of instances of the
       source block, that is the number of <See Class=TBlock3D>
       objects that are linked to it.

       When a new block is created to reference a source block,
       this property is incremented by one. When the block is
       deleted this numeber is decremented by one.

       Only if this value is zero the source block can be deleted.
    }
    property NumberOfReferences: Word read FNReference;
    {: This property tells if the source block is a library source
       block.

       A library source block is a source block that is shared among
       differert drawings, made up a library of symbols.

       When this property is <B=True>, the source block will not be
       saved in a drawing file, but can be stored in a library file
       by using the methods <See Method=TCADCmp@SaveLibrary>
       and <See Method=TCADCmp@LoadLibrary>.

       If this property is <B=False>, the source block will be
       saved in the drawing file.

       If <See Property=TGraphicObject@ToBeSaved> is <B=False> the
       source block will not be saved anyway.
    }
    property IsLibraryBlock: Boolean read fLibraryBlock write fLibraryBlock;
  end;

  {: This class defines an instance of a <See Class=TSourceBlock3D>.

     A block is an istance of a source block, namely a copy of the
     source block that has its own model transform but share the
     same objects that define the source block.

     A block is an indipendent entity when it is concerned with
     the model transform, but it depends on the source block for
     its shape.
  }
  TBlock3D = class(TObject3D)
  private
    { Reference to the source block. }
    FSourceBlock: TSourceBlock3D;
    FSourceName: TSourceBlockName;
    { Origin of the block. }
    FOriginPoint: TPoint3D;
    procedure SetSourceBlock(Source: TSourceBlock3D);
    procedure SetOriginPoint(Pt: TPoint3D);
  protected
    procedure _UpdateExtension; override;
  public
    {: This is the constructor of the class.

       <I=Source> is the source block that is used to define
       the instance.
    }
    constructor Create(ID: LongInt; Source: TSourceBlock3D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method updates the references of the block.

       This method is called automatically at the end of the loading
       process of the block from a stream. Indeed when you save a
       block the reference to the source block is no longer valid,
       so the block must be relinked to the correct source blocks
       instance.

       The method needs the iterator of the source blocks of a CAD in
       order to relink the block's source block. This iterator can be
       obtained by <See Property=TCADCmp@SourceBlocksIterator>
    }
    procedure UpdateReference(const BlockList: TGraphicObjIterator);
    procedure DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D;
                                const Cnv: TDecorativeCanvas; const Width: Integer); override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const NT: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;

    {: This property contains the base position of the block.

       The origin point is the position of the (0, 0, 0) inside of
       the source block that is referenced by the block.

       The origin point is only a visual reference and doesn't
       alter the model transform (but it is altered from it).
    }
    property OriginPoint: TPoint3D read FOriginPoint write SetOriginPoint;
    {: This property contains the reference to the source block used by the block.

       See also <See Class=TSourceBlock3D>.
    }
    property SourceBlock: TSourceBlock3D read FSourceBlock write SetSourceBlock;
    {: This property contains the name of the source block
       referenced by the block.

       It is used to save the source block reference into a stream,
       and it is used to relink the source block reference when the
       block is loaded back.
    }
    property SourceName: TSourceBlockName read FSourceName;
  end;

  {: This class defines a specialization of a <See Class=TCADCmp>
     control (see it for details).

     This CADCmp handles 3D objects and source blocks.
     This component must be used when you need to store 3D drawing.
  }
  TCADCmp3D = class(TCADCmp)
  private
    function GetExtension: TRect3D;
  protected
    procedure LoadObjectsFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveObjectsToStream(const Stream: TStream); override;
  public
    {: This method loads the blocks definitions (see <See Class=TSourceBlock3D>) from a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream that contains the blocks (and it must be
       positioned on the first block present). The stream must be
       created with the current version of the library (see also
       <See Method=TCADCmp@LoadObjectsFromOldStream>).
       The blocks are created by reading the shape class registration index
       and creating the correct shape instance with the state present in the
       stream. Then this instance is saved in the list of objects of the
       source block.

       When a source block is loaded the objects that are contained in the
       source block are checked for recursive nesting of blocks. It is
       allowed to have a source block that use in its definition another
       source block, BUT this source block must be loaded before the
       source block that use it (this is automatically checked when you
       define a block). When the source block is loaded its
       <See Method=TContainer3D@UpdateSourceReferences> is called to
       update the references.

       If there is a block that references to a not defined source block
       a message will be showed and the source block will not be loaded.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure LoadBlocksFromStream(const Stream: TStream; const Version: TCADVersion); override;
    {: This method saves the blocks definitions (see <See Class=TSourceBlock3D>) to a drawing.
       This is an abstract method that must be implemented in a concrete control.

       <I=Stream> is the stream into which save the source blocks,
       <I=AsLibrary> tells if list of source blocks must be saved as
       a library. A Library will contains only the source blocks that
       have the <See Property=TGraphicObject@ToBeSaved> property set to
       <B=True> and the <See Property=TSourceBlock3D@IsLibraryBlock>
       property set to <B=True>. This is useful to create library of
       blocks definition to be shared beetwen drawing.

       See also <See=Object's Persistance@PERSISTANCE>.
    }
    procedure SaveBlocksToStream(const Stream: TStream; const AsLibrary: Boolean); override;
    {: This method adds a new source block.

       <I=ID> is the identifier number of the source block and Obj is the
       source block object. The library doesn't check for uniqueness of
       source block's ID. The given ID substitute the ID of Obj. A source
       block can also be referenced by its name and again the library
       doesn't check for its uniqueness.

       A source block is a collection of objects that are drawed at a whole
       on the viewport. A source block must be istantiated before the
       drawing occourse by define a block that references it. The instance
       has a position so you can share the same collection of objects and
       draw it in different places on the same drawing. This allows the
       efficent use of memory. If a source block is modified, all the
       instances are modified accordingly.

       See also <See Class=TSourceBlock3D> and
       <See Class=TBlock3D>.
    }
    function AddSourceBlock(Obj: TSourceBlock3D): TSourceBlock3D;
    {: This method deletes the source block with the specified ID.

      <I=ID> is the identification number of the source block. If no such
      block is found a <See Class=ECADListObjNotFound> exception will be raised.

      If the source block to be delete is in use (that is it is referenced
      by some <See Class=TBlock3D> object) a
      <See Class=ECADSourceBlockReferenced> exception will be raised and the
      operation aborted. In this case you must delete the objects that
      reference the source block before retry.
    }
    procedure DeleteSourceBlock(const SrcName: TSourceBlockName);
    function  GetSourceBlock(ID: LongInt): TSourceBlock3D;
    function  FindSourceBlock(const SrcName: TSourceBlockName): TSourceBlock3D;
    {: This method creates a source block (and add it to the CAD) by
       grouping a list of objects.

       <I=ScrName> will be the name of the source block; <I=Objs> is
       an iterator on the list that contains the objects to be added.
       That list must have the <See Property=TGraphicObjList@FreeOnClear>
       property set to <B=False>.
    }
    function  BlockObjects(const SrcName: TSourceBlockName; const Objs: TGraphicObjIterator): TSourceBlock3D;
    procedure DeleteLibrarySourceBlocks; override;
    procedure DeleteSavedSourceBlocks; override;
    function AddObject(ID: LongInt; const Obj: TObject3D): TObject3D;
    {: This method adds a new block (instance of a source block) into
       the CAD.

       <I=ID> will be the identifier of the new block,
       and <I=SrcName> is the name of the source block used to
       define the block.

       If the source block isn't in the CAD an exception will be
       raised. The method returns the reference of the added block.
    }
    function  AddBlock(ID: LongInt; const SrcName: TSourceBlockName): TObject3D;
    function InsertObject(ID, IDInsertPoint: LongInt; Obj: TObject3D): TObject3D;
    function  GetObject(ID: LongInt): TObject3D;
    {: This method transforms a set of object or all the objects
       present in the CAD.

       The method applies the <I=T> transform matrix to the objects
       with the <I=ID> that are present in <I=ListOfObj>. This
       parameter is an array that may contains:

       <LI=only one element equal to -1. In this case <I=T> will be
       applied to all the objects present in the CAD.
       <LI=a list of integers greater or equal to zero. In this case
       <I=T> will be applied to the objects with the specified
       <I=IDs>. The array must not have duplicated IDs.

       If some ID in the array doesn't refer to an object in the CAD
       an <See Class=ECADListObjNotFound> exception will be raised
       and the operation stopped.
    }
    procedure TransformObjects(ListOfObj: array of LongInt; T: TTransf3D);
    procedure RedrawObject(Obj: TObject3D);

    {: This property contains the extension of the drawing.

       The drawing extension is the smallest axis aligned
       box that bounds all the objects in the CAD.
       When you refer to the property the extension box is
       computed and this may require a bit of time for a complex draw.

       If you want to use this value more than once in a series of
       computations, it is better to store it in a local variable
       instead of use this property in all the computations.
    }
    property DrawingExtension: TRect3D read GetExtension;
  end;

  {: This class defines an axis indicator that is drawed on the
     lower-left bound of a 3D viewport.

     It is used to show the axis orientation and it is useful when
     the view point changes dynamically.

     The indicator keeps the same size and position regardless of
     the current zooming factor.

     The axis on the indicator are drawed as line with different
     colors. The colors can be changed but as default the have
     the following meanings:

     <LI=X axis is blue>
     <LI=Y axis is red>
     <LI=Z axis is green>
  }
  TCAD3DAxis = class(TPersistent)
  private
    fXColor, fYColor, fZColor: TColor;
    fVisible: Boolean;
    fSize: Integer;
    fOwnerView: TCADViewport3D;

    procedure SetXColor(Cl: TColor);
    procedure SetYColor(Cl: TColor);
    procedure SetZColor(Cl: TColor);
    procedure SetSize(Sz: Integer);
    procedure SetVisible(V: Boolean);
  public
    constructor Create(Owner: TCADViewport3D);
    {: This method draws the axis indicator on the <I=Cnv> canvas at
       the point <I=Pt>.
    }
    procedure PaintAxes(const P: TPoint; Cnv: TCanvas);
  published
    {: This property defines the color for the X axis.

       By default it is blue.
    }
    property XColor: TColor read fXColor write SetXColor default clRed;
    {: This property defines the color for the Y axis. 

       By default it is red.
    }
    property YColor: TColor read fYColor write SetYColor default clBlue;
    {: This property defines the color for the Z axis.

       By default it is green.
    }
    property ZColor: TColor read fZColor write SetZColor default clGreen;
    {: This property defines the size of the indicator in pixels.
    }
    property Size: Integer read fSize write SetSize default 30;
    {: If this property is <B=True> the indicator will be drawed,
       otherwise it will not be drawed.
    }
    property Visible: Boolean read fVisible write SetVisible default True;
  end;

  {: This component implements a 3D viewport that is control that
     display the contents of a <See Class=TCADCmp3D> component.

     A 3D viewport acts as a <I=camera> that can be positioned and
     aimed in the world, and when it is positioned you can shoot
     photos. As with a camera you have to select where the camera is
     and where it look, as well as where is its up vector.
     In this control all of these property must be set before a
     scene has to be viewed.

     A way to setup the camera is to specify the parameters
     for a 3D plane on which the CAD contents will be projected
     (called here <I=projection plane or view plane>). When the
     objects are projected, the projection plane is mapped on
     the screen and you can see a 2D representation of the world.

     To define the plane are needed:
     <LI=A point on the projection plane (<I=VRP>).
       This point is the origin of the coordinate system on the plane
       (or <I=view plane coordinate system>). This specifies also
       the <I=center of projection>.>
     <LI=A normal to the plane (<I=VPN>).
       This versor points in the opposite direction to the view
       direction. That is if it is set to (-1, 0, 0) we are looking
       forward the X axis in the direction (1, 0, 0).>
     <LI=A UP versor (<I=VUP>). It defines the Y direction of the
     view plane coordinate system. It can be thought as the vertical
     side of the postcard obtained through the camera.>

     The dimension of the postcard are specified by the
     <See Property=TCADViewport@VisualRect> property.

     The other way to setup the camera is by means of the
     <See Method=TCADViewport3D@SetCamera> method and
     <See Method=TCADPerspectiveViewport3D@SetFieldOfView> method.

     In addition to the above parameters the following parameters
     are also needed:
     <LI=<See Property=TCADViewport3D@FrontClip>. It defines
     the fartest distance besides which the objects are no
     drawed. If an object crosses this plane it will be clipped.>
     <LI=<See Property=TCADViewport3D@BackClip>. It defines
     the nearest distance in front of which the objects are
     no drawed. If an object crosses this plane it will be clipped.>

     Both distances are positive away the view direction.

     In order to draw an object, the following operation are
     performed by the viewport and the object:

     <LI=The object is transformed by a roto-translation
     (<I=Orientation transform matrix>) that
     position it with respect to the view plane coordinate
     system (<See Property=TCADViewport3D@ViewOrientation>) transform.
     This transform the world coordinate system into the
     view plane coordinate system.>
     <LI=The object is projected and normalized using the
     <See Property=TCADViewport3D@ViewNormalization> transform. This
     transform the view plane coordinate system into the
     normalized view volume.>
     <LI=The homogeneous clipping is applied to the object.>
     <LI=The clipped object is mapped on the control canvas
     using the <See Property=TCADViewport3D@ViewMapping> transform.>

     See also <See Class=TCADViewport>.
  }
  TCADViewport3D = class(TCADViewport)
  private
    fPickFilter: TObject3DClass;
    { ViewOrientation è uguale per tutti.
      ViewNormalization porta da Wrld a NRC.
      E' quindi la combinazione tra ViewOrientation
      e la normalizzazione ritornata da BuildViewNormalization.
      (che dipende dalla proiezione).
      Le normalizzazioni devono portare al
      rettangolo normalizzato. }
    fViewOrientation, fViewNormalization: TTransf3D;
    { ViewMapping porta da NRC a schermo. }
    fViewMapping: TTransf2D;
    fCADCmp3D: TCADCmp3D;
    fAxis: TCAD3DAxis;

    fBackPlane, fFrontPlane: TRealType; { in VRC. Sono entrambi positivi ma Front è nella direzione di VPN e back di -VPN. }
    fVRP: TPoint3D;
    fVPN, fVUP: TVector3D;
    fPRPlaneViewPointDistance: TRealType;

    { Event handlers }
    FOnMouseDown3D, FOnMouseUp3D: TMouseEvent3D;
    FOnMouseMove3D: TMouseMoveEvent3D;
    fOnViewProjectionChanged: TNotifyEvent;

    procedure SetCADCmp3D(Cad: TCADCmp3D);
    procedure SetVPN(V: TVector3D);
    procedure SetVRP(P: TPoint3D);
    procedure SetVUP(V: TVector3D);
    procedure SetFClip(F: TRealType); virtual;
    procedure SetBClip(B: TRealType); virtual;
    function  GetPRPlanePosition: TPoint3D;
    function  GetViewPoint: TPoint3D;
    function  GetCameraUP: TVector3D;
    procedure _DrawObject2D(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas);
    procedure _DrawObjectWithRubber2D(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas);
  protected
    procedure SetCADCmp(Cad: TCADCmp); override;
    procedure CopyBackBufferRectOnCanvas(const Rect: TRect; const GenEvent: Boolean); override;
    procedure DrawObject(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D); override;
    procedure DrawObjectWithRubber(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D); override;
    {: This method must returns a transform matrix that is used to
       transform the <I=world coordinate system> into the
       <I=view plane coordinate system>.

       <I=LVRP> is a reference point on the view plane used as
       the oringin of the <I=view plane coordinate system>.
       <I=LVUP> is the up versor of the <I=view plane> and
       <I=LVPN> is the normal versor of the <I=view plane>.

       See <See Class=TCADViewport3D> for details.
    }
    function  BuildViewOrientationTransform(const LVRP: TPoint3D; const LVUP, LVPN: TVector3D): TTransf3D; virtual;
    {: This method is called by the viewport whenever a changing in the
       mapping transform from window plane (view plane) and canvas
       rectangle is required.

       It must returns a 2d transform matrix that transforms the 2d points
       in the visual rect (see <See Property=TCADViewport@VisualRect>)
       in the 2d points in the client rectangle of the control Canvas.

       <I=ViewWin> is the visual rect (that is the portion of
       the world currently in view); <I=ScreenWin> is the portion of the
       control's canvas that must contains the drawing and <I=AspectRatio> is
       an optional aspect ratio (Width/Heigth) to be preserved in the mapping.

       If AspectRatio is zero then no aspect ratio is specified, otherwise the
       aspect ratio must modify the <I=ViewWin> dimensions so that they preserve
       this aspect ratio. The new ViewWin will became the <See Property=TCADViewport@VisualRect>
       of the viewport.

       You must redefine this method if you want to create a new viewport,
       for example you can also change any other projection transform when
       this method is called; by default it returns <See const=IdentityTransf2D>.

       <B=Note>: For a TCADViewport3D this method updates also the
       projection-normalization matrix by calling the
       <See Method=TCADViewport3D@BuildViewNormalizationTransform> method.
    }
    function  BuildViewportTransform(var ViewWin: TRect2D; const ScreenWin: TRect; const AspectRatio: TRealType): TTransf2D; override;
    {: This method must returns a transform matrix that is used to
       project and normalize points defined in the
       <I=view plane coordinates system>.

       The returned transformation includes the projection matrix
       and the normalization matrix. This matrix
       translates the <I=view plane coordinate system> into the
       <I=normalized view volume>. See <See Class=TCADViewport3D>
       for details.

       <I=View> is the visual rect (that is the portion of
       the view plane currently in view); <I=FP> and <I=BP>
       are, respectively, the front clipping plane and the
       back clipping plane.

       <B=Note>: This method must be specified in the derived
       viewports to implement a specific view projection.
       Actually the projection is done after the clipping by
       a division by the homogeneous coordinate <I=W>.
    }
    function  BuildViewNormalizationTransform(View: TRect2D; FP, BP: TRealType): TTransf3D; virtual; abstract;
    function  GetPixelAperture: TPoint2D; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: This method updates the <I=orientation matrix> by calling
       the <SeeMethod=TCADViewport3D@BuildViewOrientationTransform> method.

       You may want to use this method to force the updates of the
       orientation transform when the camera is moved

       <B=Note>: the method is automatically called when you change the plane
        properties or call the camera methods.
    }
    procedure UpdateViewportOrientation;
    {: This method draws a 2D object on the 3D viewport.

       <I=Obj> is that object to be drawed and have to be
       of <See Class=TObject2D> type or an exception will be
       raised. If <I=CtrlPts> is <B=True>
       the control points of the object will also be drawed.

       The object is drawed by calling its <See Method=TObject2D@Draw>
       method if it is visible (that is it is contained in the
       <See Property=TCADViewport@VisualRect>)).

       The object is drawed using the
       <See Property=TCADViewport@ViewportToScreenTransform> transform
       matrix.

       <B=Note>: The method use the 3D viewport as it is a 2D viewport.
    }
    procedure DrawObject2D(const Obj: TObject2D; const CtrlPts: Boolean);
    {: This method draws a 2D object on the viewport.

       <I=Obj> is that object to be drawed. If <I=CtrlPts> is <B=True>
       the control points of the object will also be drawed.

       The object is drawed by calling its <See Method=TObject2D@Draw>
       method if it is visible (that is it is contained in the
       <See Property=TCADViewport@VisualRect>)).

       This method draws the object only on the canvas of the control
       and not in the off-screen buffer. So if you call the
       <See Method=TCADViewport@Refresh> method the effect of this
       method are removed without the need to a complete Repaint.

       <B=Note>: The method use the 3D viewport as it is a 2D viewport.
    }
    procedure DrawObject2DWithRubber(const Obj: TObject2D; const CtrlPts: Boolean);
    {: This method draws a 3D object on the viewport.

       <I=Obj> is that object to be drawed. If <I=CtrlPts> is
       <B=True> the control points of the object will
       also be drawed.

       The object is drawed by calling its <See Method=TObject3D@Draw>
       method if it is visible (that is it is contained in the
       normalized view volume after the projection-normalization
       matrix if applied to it).

       See <See Class=TCADViewport3D> for details.
    }
    procedure DrawObject3D(const Obj: TObject3D; const CtrlPts: Boolean);
    {: This method draws a 3D object on the viewport.

       <I=Obj> is that object to be drawed. If <I=CtrlPts> is <B=True>
       the control points of the object will also be drawed.

       The object is drawed by calling its <See Method=TObject3D@Draw>
       method if it is visible (that is it is contained in the
       normalized view volume after the projection-normalization
       matrix if applied to it).

       This method draws the object only on the canvas of the control
       and not in the off-screen buffer. So if you call the
       <See Method=TCADViewport@Refresh> method the effect of this
       method are removed without the need to a complete Repaint.
    }
    procedure DrawObject3DWithRubber(const Obj: TObject3D; const CtrlPts: Boolean);
    procedure CopyRectToCanvas({%H-}CADRect: TRect2D; const CanvasRect: TRect; const Cnv: TCanvas; const Mode: TCanvasCopyMode); override;
    procedure ZoomToExtension; override;
    {: This method changes the camera properties and the visual rect
       dimension to see the <I=Box> from its upper-left-front
       corner.

       Use this method to see a region of the world without
       computing the camera position and aiming.

       This method repaint the viewport.
    }
    procedure ZoomToBox(const Box: TRect3D);
    {: This method change the camera position and aiming.

       <I=ProjectionPlaneP> is a point on the camera view plane
       (consider it as the projection of the <I=center of projection>
       on the view plane of the camera). <I=ViewP> is the point
       at which the camera is looking. <I=UP> is the camera's UP versor.

       This method repaint the viewport.

       See also <See Method=TCADPerspectiveViewport3D@SetFieldOfView>.
    }
    procedure SetCamera(const ProjectionPlaneP, ViewP: TPoint3D; const Up: TVector3D);
    {: This method returns the versor that goes from the center of
       projection to the <I=WPt> point.

       The center of projection is the point at which the projection
       rays intersect (it may be at infinitum).
    }
    function  GetRayVersor(const WPt: TPoint3D): TVector3D; dynamic; abstract;
    {: This method fill in a list with the objects that are contained
       in a rectangular region of the view plane.

       <I=ResultLst> is the list that will contain the objects found;
       <I=Frm> is a rectangle in view plane coordinates.
       <I=Mode> may be one of the following values:

       <LI=if it is <I=gmAllInside> then only the objects that
         are fully contained in the rectangle are inserted into the list.>
       <LI=if it is <I=gmCrossFrame> then the objects that are
         fully or partially contained in the rectangle are inserted
         into the list.>

       If <I=RemoveFromCAD> is <B=True> then the objects added to
       the list are also removed from the CAD.

       This method is useful for create a pick-in-region function,
       or to create a source block with the objects in a region
       by removing the objects found from the CAD.
    }
    procedure GroupObjects(const ResultLst: TGraphicObjList; Frm: TRect2D; const Mode: TGroupMode; const RemoveFromCAD: Boolean);
    {: This method returns an object that has the point <I=Pt> on it.

       The method traverses the list of objects to find an object
       that is at a distance from <I=Pt> less than <I=Aperture>. If
       that object is found then it will be returned by the method,
       otherwise the method will returns <B=nil>.

       <I=Pt> is the point of the picking; <I=Aperture> is the width
       of the picking region in <B=pixel>.

       <I=NPoint> will contains (if an object will be picked) one of
       the following values:

       <LI=<I=PICK_NOOBJECT> if no object is found at <I=Pt>.>
       <LI=<I=PICK_INBBOX> if an object is found that has <I=Pt> in its bounding box.>
       <LI=<I=PICK_ONOBJECT> if an object is found that has <I=Pt> on its outline.>
       <LI=<I=PICK_INOBJECT> if an object is found that has <I=Pt> in its interior.>
       <LI=a value equal or greater than zero if an object is found
         that has <I=Pt> on one of its control points.>

       The above value are ordered from the lowest priority to the
       greater priority. If more than one of the above condition is
       matched the value with the greater priority is returned.

       If there are more than one object at the point <I=Pt> the
       following rules are applied:

       <LI=The object with the greater priority value is returned>
       <LI=The last object added to the CAD is returned.>

       The display list is traversed until the last object is
       encountered and this may require a bit of time in the
       case of a complex object. If you are satisfied to found
       the first encountered object that is picked by the point
       <I=Pt> you may want to set <I=FirstFound> to <B=True>.

       <B=Note>: The method use the <See Method=TObject3D@OnMe> method
       to check for the picking. If you need a special picking function
       you may want to call that method.

       The point <I=Pt> must be specified in the <I=world coordinate system>
       and will be projected and normalized before the picking. The
       picking itself is made in the <I=normalized view volume> as a 2D
       picking. For this the picking ignores the depth information.
    }
    function  PickObject(Pt: TPoint3D; Aperture: Word; FirstFound: Boolean; var NPoint: Integer): TObject3D;
    {: This method fills a list with the object that are at a distance
       from <I=Pt> less than <I=Aperture>.

       The method perform the same operation of <See Method=TCADViewport3D@PickObject>
       method but the objects that are picked by the point <I=Pt> are all
       added to the list <I=PickedObjects> in the same order as they
       are encountered.
    }
    function  PickListOfObjects(const PickedObjects: TList; Pt: TPoint3D; Aperture: Word): Integer;
    {: This method returns the point <I=WPt> (specified in world
       coordinate system) referenced in the view plane coordinate system.

       This is a 2D point that correspond to the projection of <I=WPt> on
       the view plane.
    }
    function  WorldToViewport(WPt: TPoint3D): TPoint2D; virtual;
    {: This method returns the point <I=VPt> (specified in view plane
       coordinate system) referenced in the world coordinate system.

       This is a 3D point that correspond to the point <I=VPt> in
       the world.
    }
    function  ViewportToWorld(VPt: TPoint2D): TPoint3D; virtual;
    {: This method returns the point <I=WPt> (specified in world
       coordinate system) referenced in the normalized view volume
       coordinate system.

       This is a 3D point with its X coordiate ranging from -1 to 1,
       its Y coordinate ranging from -1 to 1 and its Z coordinate
       ranging from -1 to 0.

       The point <I=WPt> is firstly transformed in the view plane
       coordinate system, then projected on the view plane and
       finally normalized to the canonical view volume, that
       correspond to the view frustum normalized to the box
       from (-1, -1, -1) to (1, 1, 0).
    }
    function  WorldToNRC(WPt: TPoint3D): TPoint3D; virtual;
    {: This method returns the point <I=NPt> (specified in normalize volume
       coordinate system) referenced in the screen coordinate system.

       <I=NPt> is a 3D point with its X coordiate ranging from -1 to 1,
       its Y coordinate ranging from -1 to 1 and its Z coordinate
       ranging from -1 to 0. This point is mapped on the screen coordinate
       system.
    }
    function  NRCToScreen(NPt: TPoint3D): TPoint; virtual;
    {: This method returns the point <I=WPt> in world coordinate system
       transformed by the inverse of the object model trasform of <I=Obj>.

       This method simply obtain the point <I=WPt> referenced in
       the object coordinate system of <I=Obj>.
    }
    function  WorldToObject(Obj: TObject3D; WPt: TPoint3D): TPoint3D;
    {: This method returns the point <I=OPt> in object coordinate system
       transformed by the object model trasform of <I=Obj>.

       This method simply obtain the point <I=OPt> referenced in
       the world coordinate system.
    }
    function  ObjectToWorld(Obj: TObject3D; OPt: TPoint3D): TPoint3D;

    {: This property may contains a class variable that is used to limit
       the picking to only the objects of that class.

       This property is used in <See Method=TCADViewport3D@PickObject>,
       <See Method=TCADViewport3D@PickListOfObjects> and
       <See Method=TCADViewport3D@GroupObjects>.
    }
    property PickFilter: TObject3DClass read FPickFilter write FPickFilter;
    {: This property contains the view plane normal versor.

       This versor specify where the camera is looking. This versor
       is in the opposite direction of the camera aiming. So if
       this versor is equals to (-1, 0, 0), the camera is lookin
       downward the positive X axis.
    }
    property VPN: TVector3D read fVPN write SetVPN;
    {: This property contains a point on the view plane.

       This point is the origin of the coordinate system on the plane
       (or <I=view plane coordinate system>). This specifies also
       the <I=center of projection>.>
    }
    property VRP: TPoint3D read fVRP write SetVRP;
    {: This property contains the UP versor of the view plane.

       This versor defines the Y direction of the
       view plane coordinate system. It can be thought as the
       vertical side of the postcard obtained through the camera.
    }
    property VUP: TVector3D read fVUP write SetVUP;
    {: This property contains the Z coordinate of the
       front clipping plane in view plane coordinate system.

       This value defines the fartest distance besides which the
       objects are no drawed. If an object crosses this plane it
       will be clipped.

       This distances is positive away the view direction.

       <B=Note>: If you don't want to clip any object against
       the clipping planes set FrontClip to a big positive value
       and BackClip to a big negative value.
    }
    property FrontClip: TRealType read fFrontPlane write SetFClip;
    {: This property contains the Z coordinate of the
       back clipping plane in view plane coordinate system.

       This value defines the nearest distance in front of which the
       objects are no drawed. If an object crosses this plane it
       will be clipped.

       This distances is positive away the view direction.

       <B=Note>: If you don't want to clip any object against
       the clipping planes set FrontClip to a big positive value
       and BackClip to a big negative value.
    }
    property BackClip: TRealType read fBackPlane write SetBClip;
    {: This property contains the view orientation transform matrix.

       See <See Class=TCADViewport3D> for details.
    }
    property ViewOrientation: TTransf3D read fViewOrientation;
    {: This property contains the view projection-normalization
       transform matrix.

       See <See Class=TCADViewport3D> for details.
    }
    property ViewNormalization: TTransf3D read fViewNormalization;
    {: This property contains the normalized view volume mapping
       transform that maps a normalized point into a point in the
       screen coordinate system.
    }
    property ViewMapping: TTransf2D read fViewMapping;
    {: This property contains the camera view point, that is
       the point in the world to which the camera is looking.

       This value is correct only if you have called
       <See Method=TCADViewport3D@SetCamera> to position and aim the camera.
    }
    property CameraViewPoint: TPoint3D read GetViewPoint;
    {: This property contains the camera view plane (or projection plane)
       center, that is the point in the world to which the camera is
       positioned.

       This value is correct only if you have called
       <See Method=TCADViewport3D@SetCamera> to position and aim the camera.
    }
    property CameraProjectionPlanePosition: TPoint3D read GetPRPlanePosition;
    {: This property contains the camera view plane (or projection plane)
       UP direction, that is the direction in the world that rapresent
       the vertical side of the imaginari postcard made by the camera.

       This value is correct only if you have called
       <See Method=TCADViewport3D@SetCamera> to position and aim the camera.
    }
    property CameraUP: TVector3D read GetCameraUP;
  published
    {: This property contains an instance of the <See Class=TCAD3DAxis> class.
    }
    property Axis: TCAD3DAxis read fAxis write fAxis;
    {: This property contains the <See Class=TCADCmp3D> control that
       acts as the source for the drawing to be painted in the
       viewport.

       The CADCmp3D contains the display list of the drawing that will
       be rendered in the canvas of the viewport control.

       You must assign it before using the viewport.
    }
    property CADCmp3D: TCADCmp3D read fCADCmp3D write SetCADCmp3D;
    {: EVENTS}
    {: This property may contain an event-handler that will be
       called when the mouse in moved on the control.

       See <See Type=TMouseMoveEvent3D> for details on the event
       handler.
    }
    property OnMouseMove3D: TMouseMoveEvent3D read FOnMouseMove3D write FOnMouseMove3D;
    {: This property may contain an event-handler that will be
       called when the user presses a mouse button with the mouse pointer
       over the viewport.

       See <See Type=TMouseEvent3D> for details on the event
       handler.
    }
    property OnMouseDown3D: TMouseEvent3D read FOnMouseDown3D write FOnMouseDown3D;
    {: This property may contain an event-handler that will be
       called when the user releases a mouse button with the mouse pointer
       over the viewport.

       See <See Type=TMouseEvent3D> for details on the event
       handler.
    }
    property OnMouseUp3D: TMouseEvent3D read FOnMouseUp3D write FOnMouseUp3D;
    {: This property may contains an event-handler that will be
       called when the view orientation and/or projection normalization
       transforms are changed.
    }
    property OnViewProjectionChanged: TNotifyEvent read fOnViewProjectionChanged write fOnViewProjectionChanged;
  end;

  {: This control defines a parallel viewport 3D.

     This kind of viewport use projection rays that are parallel
     to the view plane normal to project points in the world.

     This control is well suited when you don't need the
     <I=perspective foreshortening> effect.
  }
  TCADParallelViewport3D = class(TCADViewport3D)
  protected
    function BuildViewNormalizationTransform(View: TRect2D; FP, BP: TRealType): TTransf3D; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRayVersor(const {%H-}WPt: TPoint3D): TVector3D; override;
  end;

  {: This control defines a parallel viewport 3D that has
     the view plane normal along the principal axis.

     This control is well suited when you need to see the
     side, front and top view of the world.
  }
  TCADOrtogonalViewport3D = class(TCADParallelViewport3D)
  private
    fView: TOrtoViewType;
    fDirection: TOrtoDirectionType;
    fElevation: TRealType;

    procedure SetView(V: TOrtoViewType);
    procedure SetDirection(D: TOrtoDirectionType);
    procedure SetElevation(E: TRealType);
  protected
    function BuildViewOrientationTransform(const {%H-}LVRP: TPoint3D; const {%H-}LVUP, {%H-}LVPN: TVector3D): TTransf3D; override;
  public
    constructor Create(AOwner: TComponent); override;
    {: This property contains the elevation of the view plane respect
       to the same view plane that has the view reference point (VRP)
       at the origin of the world.

       By default its value is 0.
    }
    property Elevation: TRealType read fElevation write SetElevation;
  published
    {: This property contains the view plane normal of the viewport.

       See <See Type=TOrtoViewType> for details.
    }
    property View: TOrtoViewType read fView write SetView;
    {: This property contains the view plane normal direction
       of the viewport.

       See <See Type=TOrtoDirectionType> for details.
    }
    property Direction: TOrtoDirectionType read fDirection write SetDirection;
  end;

  {: This control defines a perspective projection viewport.

     Use this control when you want the
     <I=perspective foreshortening> effect.
  }
  TCADPerspectiveViewport3D = class(TCADViewport3D)
  private
    fPlaneDistance: TRealType;

    procedure SetPlaneDistance(D: TRealType);
    procedure SetBClip(B: TRealType); override;
    function GetCenterOfProjection: TPoint3D;
  protected
    function BuildViewNormalizationTransform(View: TRect2D; FP, BP: TRealType): TTransf3D; override;
  public
    constructor Create(AOwner: TComponent); override;
    function  GetRayVersor(const WPt: TPoint3D): TVector3D; override;
    {: This methods change the field of view of the perspective
       projection.

       The method changes the view frustum through which the world
       is seen.

       <I=Angle> is the angle of the field of view in radiants,
       and <I=Aspect> is the aspect ratio (Width/Height) of the
       view frustum.

       <IMAGE=viewfrust.bmp>

       This method is useful to simulate the camera's lens.
    }
    procedure SetFieldOfView(const Angle, Aspect: TRealType);

    {: This property contains the center of proejction of the
       perspective viewport.

       This is the point where the projection rays intersect.
    }
    property CenterOfProjection: TPoint3D read GetCenterOfProjection;
    {: This property contains the plane distance, in view plane coordinate
       system, of the view plane from the center of projection.
    }
    property PlaneDistance: TRealType read fPlaneDistance write SetPlaneDistance;
  end;

{ -----===== Starting Cs4CADPrgClass.pas =====----- }
  {: This type defines the events that a <See Class=TCADPrg> manages.

     These events are to be considered in the implementation of the
     <See Method=TCADState@OnEvent> method. See it for details.

     These are the events and their parameters:

     <LI=<I=ceMouseMove>, <I=ceMouseDown>, <I=ceMouseUp> and <I=ceMouseDblClick>
       are mouse events. The various points properties (<I=Current*Point>)
       are the coordinates of the mouse position.>
     <LI=<I=ceKeyDown> and <I=ceKeyUp> are the keyboard events.>
     <LI=<I=cePaint> is the event fired when the viewport is repainted.>
     <LI=<I=ceUserDefined> is a user defined event. Its kind is in the
       <I=Key> parameters of the <See Method=TCADState@OnEvent> method.>
  }
  TCADPrgEvent = (ceMouseMove,
                  ceMouseDown,
                  ceMouseUp,
                  ceMouseDblClick,
                  ceKeyDown,
                  ceKeyUp,
                  cePaint,
                  ceUserDefined);

  TCADPrg = class;

  {: This type define the class type used to specify a <See Class=TCADState> class.
  }
  TCADStateClass = class of TCADState;

  {: This class defines the common interface for the parameter
     used in a task.

     A state of a task can receive an instance of this type
     as parameter for its operation.
     The <B=task> can then accomplish its operation by using the
     information in the parameter. A task may dont need a parameter.

     All parameter types have a field that can store a state
     class reference: <I=AfterState>. This state will be activated
     when the task is ended, passing it the current parameter.
     If you define a new state you have to implement this behaviour.

     For example a common code to end an operation in the
     OnEvent method is the follow:

     <CODE=if Assigned(Param.AfterState) then<BR>
           <TAB> NextState := Param.AfterState<BR>
           else <BR>
           <TAB> NextState := CADPrg.DefaultSatate; <BR>
           Result := True;
     >
  }
  TCADPrgParam = class(TObject)
  private
    fAfterState: TCADStateClass;
    fUserObject: TObject;
  public
    {: This is the constructor of the parameter.

       <I=AfterS> is the class reference of the state to be
       started at the end of the current one.

       All parameter types have a field that can store a state
       class reference: <I=AfterState>. This state will be activated
       when the <B=task> is ended, passing it the current parameter.
       If you define a new state you have to implement this behaviour.
    }
    constructor Create(AfterS: TCADStateClass);
    {: This property contains a class reference of the state to be
       started at the end of the current one.

       All parameter types have a field that can store a state
       class reference: <I=AfterState>. This state will be activated
       when the <B=task> is ended, passing it the current parameter.
       If you define a new state you have to implement this behaviour.
    }
    property AfterState: TCADStateClass read fAfterState write fAfterState;
    {: This property may contain a user object reference interpreted
       by the current task.
    }
    property UserObject: TObject read fUserObject write fUserObject;
  end;

  TCS4MouseButton = (cmbNone, cmbLeft, cmbRight, cmbMiddle);
  {: This class defines a state of a interaction task.

     <See Class=TCADPrg> is based on a <I=FSM model> (finite state
     machine) in which an interaction task is is a set of states,
     each of them execute some part of the whole task.
     The interaction task (from now on simply a task) evolves through
     the states in respons to <I=events>. An <I=event> is some action
     on the CADPrg or the linked Viewport performed by the user or
     by the control itself.
     A task is an <I=active state>, that is the state with receives
     the events (through the implementation of the
     <See Method=TCADState@OnEvent>).
     The active state decides what operation performs in response to
     the event. See <See Method=TCADState@OnEvent> for details.

     The events are (see also TCADPrgEvent) <I=mouse events, keyboard
     events, expose events and user events>.

     The user must derives its states from this class to implement
     an interaction task. A state may be reused in more that one
     interaction task.

     An interaction task is started by using the
     <See Method=TCADPrg@StartOperation>,
     <See Method=TCADPrg@SuspendOperation> and passing it the first
     state of it.

     To define a state you may want to implement the following
     methods:

     <LI=<See Method=TCADState@Create> in which you place the
       initialization code for the state>
     <LI=<See Method=TCADState@OnEvent> in which you place the
       code to handle the events>
     <LI=<See Method=TCADState@OnStop> in which you plane the
       finalization code for the interaction task (this method
       is called when an interaction task is stopped, no when
       the state instace is destroyed.>

     <B=Note>: The instance of a state is created by the <See Class=TCADPrg>
      control.
  }
  TCADState = class(TObject)
  private
    fCAD: TCADPrg; { Used to store the CAD that is using the state. }
    fParam: TCADPrgParam; { A state as a parameter when it is created. }
    fCanBeSuspended: Boolean; { default True. If false the state cannot be suspended. }
    fDescription: String; { Contains a description of the state. }

    procedure SetDescription(D: String);
  protected
    property CADPrg: TCADPrg read fCAD;
  public
    {: This is the contructor of the state in which you may place
       the code for the initialization of the state.

       <I=CADPrg> is the <See Class=TCADPrg> control that has called
       the constructor; <I=StateParam> is an optional parameter
       passed to the state (it may be <B=nil>), see <See Class=TCADPrgParam>.

       <I=NextState> may contains a class reference to a state class
       that will be the next state when the constructor terminate.
       Set this parameter if you want a state that only change the
       parameter and leave the actual task implementation to other
       states.
    }
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); virtual;
    {: This method is called by the CADPrg when an event is pending for
       the state. See <See Class=TCADState> for details.

       <I=Event> is the event to be handled; <I=MouseButton> is the
       mouse button that caused the event (if applicable);
       <I=Shift> is the special keys configurations when the event
       was created (if applicable); <I=Key> is the key that is currently
       pressed by the user.

       <I=NextState> may contain a class reference value to a state class
       that will became the active state at the moment the method returns.
       If you put a value into this parameter you also need to return
       <B=True> from the method. Otherwise you must return <B=False>.

       Use this method to handle the events and perform operations
       or change the active state.
    }
    function OnEvent({%H-}Event: TCADPrgEvent; {%H-}MouseButton: TCS4MouseButton;
                     {%H-}Shift: TShiftState; {%H-}Key: Word;
                     var {%H-}NextState: TCADStateClass): Boolean; dynamic;
    {: This method is called when the current interactive task is
       stopped by calling the <See Method=TCADPrg@StopOperation> method.

       Use it to free any resource in the interactive task and
       in the parameter of the state.
    }
    procedure OnStop; dynamic;
    {: This method is called when the current interactive task is
       suspended by calling the <See Method=TCADPrg@SuspendOperation>
       method.

       <I=State> is the suspended state and <I=SusParam> is the
       parameter of the suspended state.
    }
    procedure OnResume(const {%H-}State: TClass; const {%H-}SusParam: TCADPrgParam); dynamic;
    {: This property contains an optional description of the
       interaction task.

       It is useful to inform the user to which operations are
       needed to complete the interaction task. When this
       property is changed an <See Property=TCADPrg@OnDescriptionChanged>
       event is fired.
    }
    property Description: String read fDescription write SetDescription;
    {: If this property is <B=True> then the iteraction task to which
       the state belonging can be interrupted with the
       <See Method=TCADPrg@StopOperation>.
    }
    property CanBeSuspended: Boolean read fCanBeSuspended write fCanBeSuspended;
    {: This property contains the parameter of the state. It is
       set by the TCADPrg when the state is activated but you
       may (and sometimes is very useful) be changed through this
       property.
    }
    property Param: TCADPrgParam read fParam write fParam;
  end;

  {: This class implements a state that does nothing.

     It is useful for a default state.
  }
  TCADIdleState = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This type defines an event-handler for the event fired when
     the active state is changed.

     <I=Sender> is the TCADPrg that create the instance of the state;
     <I=Sender> is the changed state (if the event is a
     <See Property=TCADPrg@OnEnterState> this is the entering state,
     if the event is a <See Property=TCADPrg@OnExitState> this is
     the leaving state).
  }
  TCADPrgOnChangeState = procedure(Sender: TObject; const State: TCADState) of object;
  {: This type defines an event-handler for the event fired when
     the current interaction task of a TCADPrg is aborted, ended
     or started.

     <I=Sender> is the TCADPrg instance that has started the
     interaction task. <I=Operation> is the initial state
     of the interaction task and <I=Param> is the parameter of
     the interaction task.
  }
  TCADPrgOnOperation = procedure(Sender: TObject; const Operation: TCADStateClass; const Param: TCADPrgParam) of object;

  {: This class defines the control used by the library to handle
     interaction tasks. An interaction task is an interactive
     visual sequence of operation on a Viewport.

     The control implement a FSM (Finite state machine) model
     that receives events from the user and the Viewport.

     See also <See Class=TCADState> class for details.
  }
  TCADPrg = class(TComponent)
  private
    fLinkedViewport: TCADViewport;
    fIsBusy, fIsSuspended: Boolean;
    fUseSnap, fUseOrto: Boolean;
    fSnapX, fSnapY: TRealType;
    fRepaintRect: TRect2D;
    fCursorColor: TColor;
    fMustRepaint, fRefreshAfterOp: Boolean;
    fIgnoreEvents: Boolean;
    fCurrentKey: Word;
    fDefaultState: TCADStateClass;
    fCurrentState, fSuspendedState: TCADState;
    fCurrentOperation, fSuspendedOperation: TCADStateClass;
    fOnEntState, fOnExState: TCADPrgOnChangeState;
    fOnEndOperation, fOnStart, fOnStop: TCADPrgOnOperation;
    fOldOnPaint, fOnIdle: TNotifyEvent;
    fOnDescriptionChanged: TNotifyEvent;
    fShowCursorCross: Boolean;
    fCurrentMousePoint: TPoint;
    //maurog
    {$IFDEF windows}
    fNewWndProc, fOldWndProc: Pointer;
    {$ELSE}
    fNewWndProc, fOldWndProc: TWndMethod;
    {$ENDIF}

    procedure SetShowCursorCross(B: Boolean);
    procedure SetCursorColor(C: TColor);
    procedure SetDefaultState(DefState: TCADStateClass);
    procedure SetLinkedViewport(V: TCADViewport);
    procedure GoToDefaultState(const LastState: TClass; const LastParam: TCADPrgParam);
  protected
    procedure SubclassedWinProc(var Msg: TLMessage); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {: This property must returns the current viewport point at which
       the mouse is positioned.

       The control doesn't send the mouse coordinates with the events,
       but the user must use the <I=Current*Point> properties defined
       for <See Class=TCADPrg2D> and <See Class=TCADPrg3D>.
    }
    function GetVPPoint: TPoint2D; virtual; abstract;
    {: This property is called whenever the mouse is moved and the
       current viewport point stored in the control must be updated.

       <I=Pt> is the 2D mouse position in the view plane coordinate system.

       The control doesn't send the mouse coordinates with the events,
       but the user must use the <I=Current*Point> properties defined
       for <See Class=TCADPrg2D> and <See Class=TCADPrg3D>.
    }
    procedure SetVPPoint(const Pt: TPoint2D); virtual; abstract;
    {: Use this method to draw the cursor at the mouse position.

       You have to implement this property also if you don't want
       a cursor (in this case simple do nothing).
    }
    procedure DrawCursorCross; virtual; abstract;
    {: Use this method to hide the cursor at the mouse position.

       You have to implement this property also if you don't want
       a cursor (in this case simple do nothing).
    }
    procedure HideCursorCross; virtual; abstract;
    {: This method is called whenever a point event is received.

       You don't need to use it.
    }
    procedure ViewOnPaint(Sender: TObject); dynamic;
    {: This method is called whenever a key event is received.

       You don't need to use it.
    }
    function ViewOnKeyDown({%H-}Sender: TObject; var Key: Word; Shift: TShiftState): Boolean; dynamic;
    {: This method is called whenever a key event is received.

       You don't need to use it.
    }
    function ViewOnKeyUp({%H-}Sender: TObject; var Key: Word; Shift: TShiftState): Boolean; dynamic;
    {: This method is called whenever a mouse event is received.

       You don't need to use it.
    }
    function ViewOnDblClick({%H-}Sender: TObject): Boolean; dynamic;
    {: This method is called whenever a mouse event is received.

       You don't need to use it.
    }
    function ViewOnMouseMove({%H-}Sender: TObject; Shift: TShiftState; var {%H-}X, {%H-}Y: SmallInt): Boolean; dynamic;
    {: This method is called whenever a mouse event is received.

       You don't need to use it.
    }
    function ViewOnMouseDown({%H-}Sender: TObject; Button: TMouseButton; Shift: TShiftState; var {%H-}X, {%H-}Y: SmallInt): Boolean; dynamic;
    {: This method is called whenever a mouse event is received.

       You don't need to use it.
    }
    function ViewOnMouseUp({%H-}Sender: TObject; Button: TMouseButton; Shift: TShiftState; var {%H-}X, {%H-}Y: SmallInt): Boolean; dynamic;
    {: This method is used to send an event to the current state.

       You don't need to call it directly.
    }
    procedure SendEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word); virtual;
    {: This property contains the current key pressed by the user and
       sent as the <I=Key> parameter of the <See Method=TCADState@OnEvent>.
    }
    property CurrentKey: Word read fCurrentKey write fCurrentKey;
    {: This property contains the linked viewport that is used to
       interact with the user.
    }
    property LinkedViewport: TCADViewport read fLinkedViewport write SetLinkedViewport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: This method starts a new interaction task.

       <I=StartState> is the class reference of the starting
       state class for the interaction task; <I=Param> is an instance
       of <See Class=TCADPrgParam> that will be the parameter
       of the interaction task (and it will be passed to all
       the state of it).

       If an interaction task is already active it will be stopped
       and the new one started. If it has a suspended interaction task
       the method returns <I=False> and the interaction start will not
       be started.

       The method returns <I=True> if the new interaction task is
       started.
    }
    function StartOperation(const StartState: TCADStateClass; Param: TCADPrgParam): Boolean;
    {: This method suspends the current interaction task and
       starts a new one.

       <I=StartState> is the class reference of the starting
       state class for the interaction task; <I=Param> is an instance
       of <See Class=TCADPrgParam> that will be the parameter
       of the interaction task (and it will be passed to all
       the state of it).

       If there is no active interaction task the new task will be
       simply started. If an interaction task was already suspended
       the method returns <I=False> and the new interaction start will
       not be started.

       The method returns <I=True> if the new interaction task is
       started.
    }
    function SuspendOperation(const StartState: TCADStateClass; Param: TCADPrgParam): Boolean;
    {: This method stops the current interaction task.

       If no interaction task is active the method does nothing.
    }
    procedure StopOperation;
    {: This method resets the FSM.

       Everithing is resetted as the control is created. Use this
       method only in case of unrecoverable errors.
    }
    procedure Reset;
    {: This method sends CAD event to the current state.

       It may be used to simulate mouse movements and operation
       (for instance to set a point with a value, you can set the
       <See Property=TCADPrg@CurrentViewportPoint> property to the
       point and send the <I=ceMouseDown> event).

       See <See Class=TCADState> for details.
    }
    procedure SendCADEvent(Event: TCADPrgEvent; MouseButton: TMouseButton; Shift: TShiftState; Key: Word);
    {: This method sends a user defined event to the current state.

      <I=Code> is the code for the user defined event. This code will
      be passed as the <I=Key> parameter to the
      <See Method=TCADState@OnEvent> method. The event can be identified
      in the code as a <I=ceUserDefined>.
    }
    procedure SendUserEvent(const Code: Word);
    {: If this method is called, the linked viewport will be
      repainted when the current interaction task will finish.

      <I=ARect> is the portion of the view plane to be repainted.

      By default when the current interaction task finishes only
      a refresh of the linked viewport is performed.

      See also <See Method=TCADPrg@RepaintAfterOperation>.
    }
    procedure RepaintRectAfterOperation(const ARect: TRect2D);
    {: If this method is called, the linked viewport will be
      repainted when the current interaction task will finish.

      By default when the current interaction task finishes only
      a refresh of the linked viewport is performed.

      See also <See Method=TCADPrg@RepaintRectAfterOperation>.
    }
    procedure RepaintAfterOperation;
    {: This property contains the current mouse position.
       The position is a 2D point that lies on the view plane of
       the linked viewport.

       Use this property inside the states to obtain the mouse
       position or set it to simulate mouse event by code.

       Other mouse position are defined depending on the type of
       the <I=CADPrg> (see also <See Class=TCADPrg2D> and
       <See Class=TCADPrg3D>).
    }
    property CurrentViewportPoint: TPoint2D read GetVPPoint write SetVPPoint;
    {: This property contains the current mouse position in screen coordinates.
    }
    property CurrentMousePoint: TPoint read fCurrentMousePoint;
    {: This property is <B=True> when there is an active interaction
       task in the control (that is its current state is not equal
       to the default state).
    }
    property IsBusy: Boolean read fIsBusy;
    {: This property is <B=True> when there is a suspended interaction
       task in the control.
    }
    property IsSuspended: Boolean read fIsSuspended;
    {: If this property is set to <B=True> then no event is sent to
       the active state of the control.

       This is very useful when you want to repaint a viewport inside
       a state but don't want to receive the <I=OnPaint> event.

       By default it is <B=False>.
    }
    property IgnoreEvents: Boolean read fIgnoreEvents write fIgnoreEvents;
    {: This propery contains the active state instance of the current
       interaction task.
    }
    property CurrentState: TCADState read fCurrentState;
    {: This propery contains the class reference type of the
       first state (that is the state passed to the
       <See Method=TCADPrg@StartOperation> or
       <See Method=TCADPrg@SuspendOperation> methods) for the
       current interaction task.
    }
    property CurrentOperation: TCADStateClass read fCurrentOperation write fCurrentOperation;
    {: This property contains a class reference value of the
       default state for the FSM.

       This is the state that will be instantiated whenever no
       interaction task is running. This state may be itself an
       interaction task that must be used to start other interaction
       tasks.

       By default the default state is <See Class=TCADIdleState>.
    }
    property DefaultState: TCADStateClass read fDefaultState write SetDefaultState;
    {: This is the linked viewport of the control.

       The linked viewport is the one that is able to send mouse
       and keyboard event to the <I=TCadPrg>. This control is used
       also as the output of the interaction task feedback and for
       the cursor visualization.
    }
    property Viewport: TCADViewport read fLinkedViewport;
  published
    {: If this property is <B=True> then the snapping constraint is
       enabled.

       The snapping used by the <I=TCADPrg> control is a 2D snapping
       on a plane. The plane on which it works depends on the kind
       of the control (see <See Class=TCADPrg2D> and <See Class=TCADPrg3D>).
    }
    property UseSnap: Boolean read fUseSnap write fUseSnap default False;
    {: If this property is <B=True> that the ortogonal constraint is
       enabled.

       The ortogonal constrain forces the drawing to be
       parallel to the X and Y axis of a plane. Not all
       interaction tasks uses this contraint that is any interaction
       tasks must implement its ortogonal contraint.
       The plane on which it works depends on the kind
       of the control (see <See Class=TCADPrg2D> and <See Class=TCADPrg3D>).
    }
    property UseOrto: Boolean read fUseOrto write fUseOrto default False;
    {: This property specifies the snapping step on the X axis of the
       interaction plane.

       See also <See Property=TCADPrg@UseSnap>.
    }
    property XSnap: TRealType read fSnapX write fSnapX;
    {: This property specifies the snapping step on the Y axis of the
       interaction plane.

       See also <See Property=TCADPrg@UseSnap>.
    }
    property YSnap: TRealType read fSnapY write fSnapY;
    {: This property contains the color of the cursor.

       The cursor is a visual feedback of the mouse position and
       must be implemented in specific <I=TCADPrg> controls
       (see <See Class=TCADPrg2D> and <See Class=TCADPrg3D>).
    }
    property CursorColor: TColor read fCursorColor write SetCursorColor default clGray;
    {: If this property is <B=True> then the cursor will be
       showed.

       The cursor is a visual feedback of the mouse position and
       must be implemented in specific <I=TCADPrg> controls
       (see <See Class=TCADPrg2D> and <See Class=TCADPrg3D>).
    }
    property ShowCursorCross: Boolean read fShowCursorCross write SetShowCursorCross default False;
    {: If this operation is <B=True> then the linked viewport of
       the control will be refreshed when the current operation
       finish. By default it is <B=True>.
    }
    property RefreshAfterOperation: Boolean read fRefreshAfterOp write fRefreshAfterOp default True;
    {: EVENTS}
    {: This property may contain an event-handler that is called
       when the current state is changed.

       It is called before the new state is entered.

       See also <See Type=TCADPrgOnChangeState>.
    }
    property OnEnterState: TCADPrgOnChangeState read fOnEntState write fOnEntState;
    {: This property may contain an event-handler that is called
       when the current state is changed.

       It is called after the state has exited.

       See also <See Type=TCADPrgOnChangeState>.
    }
    property OnExitState: TCADPrgOnChangeState read fOnExState write fOnExState;
    {: This property may contains an event-handler that is called
       when the current interaction task is ended.

       See also <See Type=TCADPrgOnOperation>.
    }
    property OnEndOperation: TCADPrgOnOperation read fOnEndOperation write fOnEndOperation;
    {: This property may contains an event-handler that is called
       when the current interaction task is stopped.

       See also <See Type=TCADPrgOnOperation>.
    }
    property OnStopOperation: TCADPrgOnOperation read fOnStop write fOnStop;
    {: This property may contains an event-handler that is called
       when a new interaction task is started.

       See also <See Type=TCADPrgOnOperation>.
    }
    property OnStartOperation: TCADPrgOnOperation read fOnStart write fOnStart;
    {: This property may contains an event-handler that is called
       when the control enter in the default state.
    }
    property OnIdle: TNotifyEvent read fOnIdle write fOnIdle;
    {: This property may contains an event-handler that is called
       when the description of the current state is changed.

       Use it to update the user information on the screen.
    }
    property OnDescriptionChanged: TNotifyEvent read fOnDescriptionChanged write fOnDescriptionChanged;
  end;

  TCADPrg2D = class;
  {: This type defines the type of a <I=2D mouse button filter>.

     A filter is a procedure that is called by a <I=TCADPrg> control
     to update the current mouse position of the control.

     <I=Sender> is the TCADPrg instance that has called the
     filter; <I=CurrentState> is the current state of the control;
     <I=Button> is the mouse button's state at the moment of the
     event; <I=WPt> must be set to new world point of the mouse
     position; <I=X, Y> are the mouse position in screen coordinates.

     A filter is useful to change the position of a mouse event
     before the event is handled by the current state. For example
     it may be used to implement an object gravity function by
     calling in the filter the PickObject method of the linked
     viewport and if the WPt point is on an object change it to
     the nearest control point of the object.

     <B=Note>: <I=WPt> contains the current world point of the
      mouse position at the moment of the activation of the filter.
      So if you don't need to change it simply left it unchanged.
  }
  TMouse2DButtonFilter = procedure(Sender: TCADPrg2D; CurrentState: TCADState; Button: TMouseButton; var WPt: TPoint2D; X, Y: Integer) of Object;
  {: This type defines the type of a <I=2D mouse move filter>.

     A filter is a procedure that is called by a <I=TCADPrg> control
     to update the current mouse position of the control.

     <I=Sender> is the TCADPrg instance that has called the
     filter; <I=CurrentState> is the current state of the control;
     <I=Button> is the mouse button's state at the moment of the
     event; <I=WPt> must be set to new world point of the mouse
     position; <I=X, Y> are the mouse position in screen coordinates.

     A filter is useful to change the position of a mouse event
     before the event is handled by the current state. For example
     it may be used to implement an object gravity function by
     calling in the filter the PickObject method of the linked
     viewport and if the WPt point is on an object change it to
     the nearest control point of the object.

     <B=Note>: <I=WPt> contains the current world point of the
      mouse position at the moment of the activation of the filter.
      So if you don't need to change it simply left it unchanged.
  }
  TMouse2DMoveFilter = procedure(Sender: TCADPrg2D; CurrentState: TCADState; var WPt: TPoint2D; X, Y: Integer) of Object;
  {: This type defines the type of a <I=2D snap filter>.

     A filter is a procedure that is called by a <I=TCADPrg> control
     to update the current mouse position of the control.

     <I=Sender> is the TCADPrg instance that has called the
     filter; <I=CurrentState> is the current state of the control;
     <I=LastPt> contains the snap origin for the snapping;
     <I=CurrSnappedPt> must be set to the new snapped point.

     A snap filter is useful to implement complex snapping
     constraint. For instance you may create an angular snapping
     by using the <I=LastPt> and <I=CurrSnappedPt> to find the
     current angle and then forces it to the nearest allowed angle.

     When the filter is activated <I=LastPt> contains the snap origin,
     that is the value assigned to the <See Property=TCADPrg2D@SnapOriginPoint>
     property. If there is no valid snap origin then
     it will be (<I=MaxCoord>, <I=MaxCoord>). You must check for this
     value when implement a snap filter.

     By default the <I=CurrSnappedPt> will be the standar snapped
     point that is the point with the X and Y coordinates at integer
     multiplies of <I=XSnap> and <I=YSnap>.
  }
  TCADPrg2DSnapFilter = procedure(Sender: TCADPrg2D; CurrentState: TCADState; const LastPt: TPoint2D; var CurrSnappedPt: TPoint2D) of Object;

  {: This type define the class type used to specify a <See Class=TCADState2D> class.
  }
  TCADStateClass2D = class of TCADState2D;

  {: This class defines a state of a interaction task.

     <See Class=TCADPrg> is based on a <I=FSM model> (finite state
     machine) in which an interaction task is is a set of states,
     each of them execute some part of the whole task.
     The interaction task (from now on simply a task) evolves through
     the states in respons to <I=events>. An <I=event> is some action
     on the CADPrg or the linked Viewport performed by the user or
     by the control itself.
     A task is an <I=active state>, that is the state with receives
     the events (through the implementation of the
     <See Method=TCADState@OnEvent>).
     The active state decides what operation performs in response to
     the event. See <See Method=TCADState@OnEvent> for details.

     The events are (see also TCADPrgEvent) <I=mouse events, keyboard
     events, expose events and user events>.

     The user must derives its states from this class to implement
     an interaction task. A state may be reused in more that one
     interaction task.

     An interaction task is started by using the
     <See Method=TCADPrg@StartOperation>,
     <See Method=TCADPrg@SuspendOperation> and passing it the first
     state of it.

     To define a state you may want to implement the following
     methods:

     <LI=<See Method=TCADState@Create> in which you place the
       initialization code for the state>
     <LI=<See Method=TCADState@OnEvent> in which you place the
       code to handle the events>
     <LI=<See Method=TCADState@OnStop> in which you plane the
       finalization code for the interaction task (this method
       is called when an interaction task is stopped, no when
       the state instace is destroyed.>

     <B=Note>: The instance of a state is created by the <See Class=TCADPrg>
      control.
  }
  TCADState2D = class(TCADState);

  {: This class defines the control used by the library to handle 2D
     interaction tasks. An interaction task is an interactive
     visual sequence of operation on a Viewport.

     The control implement a FSM (Finite state machine) model
     that receives events from the user and the Viewport.

     The <I=TCADPrg2D> control use points on the view plane of
     the linked viewport.

     See also <See Class=TCADState2D> class for details.
  }
  TCADPrg2D = class(TCADPrg)
  private
    fCurrentViewportPoint: TPoint2D;
    fLastCursorPos, fSnapOriginPoint: TPoint2D;
    fMouseButtonFilter: TMouse2DButtonFilter;
    fMouseMoveFilter: TMouse2DMoveFilter;
    fSnapFilter: TCADPrg2DSnapFilter;

    procedure SetViewport2D(View2D: TCADViewport2D);
    function GetViewport2D: TCADViewport2D;
  protected
    function ViewOnMouseMove(Sender: TObject; Shift: TShiftState; var X, Y: SmallInt): Boolean; override;
    function ViewOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean; override;
    function ViewOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean; override;
    {: This method draws an hair crossed 2D cursor on the view plane
       in xor mode.
    }
    procedure DrawCursorCross; override;
    {: This method hides an hair crossed 2D cursor on the view plane
       in xor mode.
    }
    procedure HideCursorCross; override;
    function GetVPPoint: TPoint2D; override;
    procedure SetVPPoint(const Pt: TPoint2D); override;
    function GetVPSnappedPoint: TPoint2D;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: This property contains the current snapped position of the
       mouse.

       Use this property to know the mouse position on the view
       plane of the last mouse event.

       <B=Note>: It is better to use this property in your interaction
       tasks instead of the <See Property=TCADPrg@CurrentViewportPoint>
       property because by using this property you automatically apply
       the snapping if enabled.
    }
    property CurrentViewportSnappedPoint: TPoint2D read GetVPSnappedPoint;
    {: This property contains the snap origin passed to the snap filter.

       By default its value is (<I=MaxCoord>, <I=MaxCoord>).

       See <See Type=TCADPrg2DSnapFilter> for details.
    }
    property SnapOriginPoint: TPoint2D read fSnapOriginPoint write fSnapOriginPoint;
  published
    {: This property contains the linked viewport.

       See also <See Property=TCADPrg@Viewport>.
    }
    property Viewport2D: TCADViewport2D read GetViewport2D write SetViewport2D;
    {: This property may contains a filter that will be called
       when a mouse button event is fired.

       See also <See Type=TMouse2DButtonFilter>.
    }
    property MouseButtonFilter: TMouse2DButtonFilter read fMouseButtonFilter write fMouseButtonFilter;
    {: This property may contains a filter that will be called
       when a mouse movement event is fired.

       See also <See Type=TMouse2DMoveFilter>.
    }
    property MouseMoveFilter: TMouse2DMoveFilter read fMouseMoveFilter write fMouseMoveFilter;
    {: This property may contains a filter that will be called
       when a the snapped point of the current mouse position
       is being computed.

       See also <See Type=TCADPrg2DSnapFilter>.
    }
    property SnapFilter: TCADPrg2DSnapFilter read fSnapFilter write fSnapFilter;
  end;

  TCADPrg3D = class;

  {: This type defines the type of a <I=3D mouse button filter>.

     A filter is a procedure that is called by a <I=TCADPrg> control
     to update the current mouse position of the control.

     <I=Sender> is the TCADPrg instance that has called the
     filter; <I=CurrentState> is the current state of the control;
     <I=Button> is the mouse button's state at the moment of the
     event; <I=WPt> must be set to new world point of the mouse
     position; <I=X, Y> are the mouse position in screen coordinates.

     A filter is useful to change the position of a mouse event
     before the event is handled by the current state. For example
     it may be used to implement an object gravity function by
     calling in the filter the PickObject method of the linked
     viewport and if the WPt point is on an object change it to
     the nearest control point of the object.

     <B=Note>: <I=WPt> contains the current world point of the
      mouse position at the moment of the activation of the filter.
      So if you don't need to change it simply left it unchanged.
  }
  TMouse3DButtonFilter = procedure(Sender: TCADPrg3D; CurrentState: TCADState; Button: TMouseButton; var WPt: TPoint3D; X, Y: Integer) of Object;
  {: This type defines the type of a <I=3D mouse move filter>.

     A filter is a procedure that is called by a <I=TCADPrg> control
     to update the current mouse position of the control.

     <I=Sender> is the TCADPrg instance that has called the
     filter; <I=CurrentState> is the current state of the control;
     <I=Button> is the mouse button's state at the moment of the
     event; <I=WPt> must be set to new world point of the mouse
     position; <I=X, Y> are the mouse position in screen coordinates.

     A filter is useful to change the position of a mouse event
     before the event is handled by the current state. For example
     it may be used to implement an object gravity function by
     calling in the filter the PickObject method of the linked
     viewport and if the WPt point is on an object change it to
     the nearest control point of the object.

     <B=Note>: <I=WPt> contains the current world point of the
      mouse position at the moment of the activation of the filter.
      So if you don't need to change it simply left it unchanged.
  }
  TMouse3DMoveFilter = procedure(Sender: TCADPrg3D; CurrentState: TCADState; var WPt: TPoint3D; X, Y: Integer) of Object;
  {: This type defines the type of a <I=3D snap filter>.

     A filter is a procedure that is called by a <I=TCADPrg> control
     to update the current mouse position of the control.

     <I=Sender> is the TCADPrg instance that has called the
     filter; <I=CurrentState> is the current state of the control;
     <I=LastPt> contains the snap origin for the snapping;
     <I=CurrSnappedPt> must be set to the new snapped point.

     A snap filter is useful to implement complex snapping
     constraint. For instance you may create an angular snapping
     by using the <I=LastPt> and <I=CurrSnappedPt> to find the
     current angle and then forces it to the nearest allowed angle.

     When the filter is activated <I=LastPt> contains the snap origin,
     that is the value assigned to the <See Property=TCADPrg2D@SnapOriginPoint>
     property. If there is no valid snap origin then
     it will be (<I=MaxCoord>, <I=MaxCoord>). You must check for this
     value when implement a snap filter.

     By default the <I=CurrSnappedPt> will be the standar snapped
     point that is the point with the X and Y coordinates at integer
     multiplies of <I=XSnap> and <I=YSnap>.

     <B=Note>: The snapping is always on 2D points on the current working
     plane (see <See Class=TCADPrg3D>).
  }
  TCADPrg3DSnapFilter = procedure(Sender: TCADPrg3D; CurrentState: TCADState; const LastPt: TPoint2D; var CurrSnappedPt: TPoint2D) of Object;

  TCADStateClass3D = class of TCADState3D;

  {: This class defines a state of a interaction task.

     <See Class=TCADPrg> is based on a <I=FSM model> (finite state
     machine) in which an interaction task is is a set of states,
     each of them execute some part of the whole task.
     The interaction task (from now on simply a task) evolves through
     the states in respons to <I=events>. An <I=event> is some action
     on the CADPrg or the linked Viewport performed by the user or
     by the control itself.
     A task is an <I=active state>, that is the state with receives
     the events (through the implementation of the
     <See Method=TCADState@OnEvent>).
     The active state decides what operation performs in response to
     the event. See <See Method=TCADState@OnEvent> for details.

     The events are (see also TCADPrgEvent) <I=mouse events, keyboard
     events, expose events and user events>.

     The user must derives its states from this class to implement
     an interaction task. A state may be reused in more that one
     interaction task.

     An interaction task is started by using the
     <See Method=TCADPrg@StartOperation>,
     <See Method=TCADPrg@SuspendOperation> and passing it the first
     state of it.

     To define a state you may want to implement the following
     methods:

     <LI=<See Method=TCADState@Create> in which you place the
       initialization code for the state>
     <LI=<See Method=TCADState@OnEvent> in which you place the
       code to handle the events>
     <LI=<See Method=TCADState@OnStop> in which you plane the
       finalization code for the interaction task (this method
       is called when an interaction task is stopped, no when
       the state instace is destroyed.>

     <B=Note>: The instance of a state is created by the <See Class=TCADPrg>
      control.
  }
  TCADState3D = class(TCADState);

  {: This class defines the control used by the library to define
     interaction tasks. An interaction task is an interactive
     visual sequence of operation on a Viewport.

     The control implement a FSM (Finite state machine) model
     that receives events from the user and the Viewport.

     The control also use the concept of <See=Working plane@WORKPLANE> that
     is a special plane on which the mouse movements are
     constrained. The snap and ortogonal constraints also
     work on this plane.

     See also <See Class=TCADState> class for details.
  }
  {: <New topic=WORKPLANE@The working plane>
     To make easy drawing in 3D using only a 2D projection,
     the <See Class=TCADPrg3D> constrains the mouse on a 2D plane
     that doesn't need to be the pojection plane. This plane
     is called <I=the Working Plane>.

     A working plane is a special plane oriented in the space
     that is used as a reference for all drawings on the viewport.
     If a working plane is in use, the mouse position is
     projected on that plane using the <I=projectors> of the
     projection and so you are sure that any changing in the
     view point doesn't change the plane on which you are
     drawing.

     For instance if you are looking from a point that forms
     an angle 45° with the X-Y plane, if you set the working
     plane to be equal to the X-Y plane all the mouse movements
     will lies on that plane. If you change the angle of view,
     the mouse will again be keept on that plane.

     Of course if you set the working plane to be equal to
     the X-Y plane and you are looking from the X axis, unexpected
     results may be possible.
  }
  TCADPrg3D = class(TCADPrg)
  private
    fCurrentWorldPoint: TPoint3D;
    fCurrentViewportPoint: TPoint2D;
    fMouseButtonFilter: TMouse3DButtonFilter;
    fMouseMoveFilter: TMouse3DMoveFilter;
    fSnapFilter: TCADPrg3DSnapFilter;
    fLastCursorPos: TPoint3D;
    { NEW: Definisce un piano di lavoro su cui è possibile vincolare le coordinate. }
    fWorkingPlaneNormal, fWorkingPlaneUP: TVector3D;
    fWorkingPlaneOrigin, fSnapOriginPoint: TPoint3D;
    fCursorSize: TRealType;

    function GetCurrentWorkPlanePoint: TPoint3D;
    function GetWPSnappedPoint: TPoint3D;
    procedure SetWorldPoint(Pt: TPoint3D);
    procedure SetViewport3D(View3D: TCADViewport3D);
    function GetViewport3D: TCADViewport3D;
    function GetWorkingPlaneXDir: TVector3D;
  protected
    function ViewOnMouseMove(Sender: TObject; Shift: TShiftState; var X, Y: SmallInt): Boolean; override;
    function ViewOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean; override;
    function ViewOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean; override;

    function GetVPPoint: TPoint2D; override;
    procedure SetVPPoint(const Pt: TPoint2D); override;

    procedure DrawCursorCross; override;
    procedure HideCursorCross; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {: This method returns the point on the current working
       plane that correspond to the point <I=WPt> in the world
       coordinate system.

       See also <See=Working plane@WORKPLANE>.
    }
    function WorldToWorkingPlane(WPt: TPoint3D): TPoint3D;
    {: This property contains the world point of the
       current position of the mouse in the world coordinate system.

       This point lies on the projection plane regardless of the
       current working plane settings.
    }
    property CurrentWorldPoint: TPoint3D read fCurrentWorldPoint write SetWorldPoint;
    {: This property contains the world point of the
       current position of the mouse referred to
       the <See=Working plane@WORKPLANE>.

       This point is affected by the current working plane
       settings.
    }
    property CurrentWorkingPlanePoint: TPoint3D read GetCurrentWorkPlanePoint;
    {: This property contains the world point of the
       current position of the mouse referred to
       the <See=Working plane@WORKPLANE>.

       The point is snapped using the current snap setting if
       <See Property=TCADPrg@UseSnap> is <B=True>.
    }
    property CurrentWorkingPlaneSnappedPoint: TPoint3D read GetWPSnappedPoint;
    {: This property contains the normal to the
       <See=Working plane@WORKPLANE>.

       By default it is <I=(0, 0, 1).
    }
    property WorkingPlaneNormal: TVector3D read fWorkingPlaneNormal write fWorkingPlaneNormal;
    {: This property contains the UP versor (Y axis direction)
       of the <See=Working plane@WORKPLANE>.

       By default it is <I=(0, 1, 0).
    }
    property WorkingPlaneUP: TVector3D read fWorkingPlaneUp write fWorkingPlaneUp;
    {: This property contains the X axis direction of the
       <See=Working plane@WORKPLANE>.

       By default it is <I=(1, 0, 0).
    }
    property WorkingPlaneXDir: TVector3D read GetWorkingPlaneXDir;
    {: This property contains the origin of the 2D coordinate
       system of the <See=Working plane@WORKPLANE>.

       By default it is <I=(0, 0, 0).
    }
    property WorkingPlaneOrigin: TPoint3D read fWorkingPlaneOrigin write fWorkingPlaneOrigin;
    {: This property contains the snap origin passed to the snap filter.

       By default its value is (<I=MaxCoord>, <I=MaxCoord>).

       See <See Type=TCADPrg2DSnapFilter> for details.
    }
    property SnapOriginPoint: TPoint3D read fSnapOriginPoint write fSnapOriginPoint;
  published
    {: This property contains the linked viewport.

       See also <See Property=TCADPrg@Viewport>.
    }
    property Viewport3D: TCADViewport3D read GetViewport3D write SetViewport3D;
    {: This property contains the dimension in drawing units of
       the mouse cursor.

       A CADPrg3D use a special mouse cursor that show the
       grid spacinga and the working plane orientation.
    }
    property CursorSize: TRealType read fCursorSize write fCursorSize;
    {: This property may contains a filter that will be called
       when a mouse button event is fired.

       See also <See Type=TMouse3DButtonFilter>.
    }
    property MouseButtonFilter: TMouse3DButtonFilter read fMouseButtonFilter write fMouseButtonFilter;
    {: This property may contains a filter that will be called
       when a mouse movement event is fired.

       See also <See Type=TMouse3DMoveFilter>.
    }
    property MouseMoveFilter: TMouse3DMoveFilter read fMouseMoveFilter write fMouseMoveFilter;
    {: This property may contains a filter that will be called
       when a the snapped point of the current mouse position
       is being computed.

       See also <See Type=TCADPrg3DSnapFilter>.
    }
    property SnapFilter: TCADPrg3DSnapFilter read fSnapFilter write fSnapFilter;
  end;

  {: This function returns the angle in radiants that
     corresponds to <I=A> in degrees.
  }
  function DegToRad(const A: TRealType): TRealType;
  {: This function returns the angle in degree that
     corresponds to <I=A> in angles.

     The resulting angle is in the range <I=0-360°>.
  }
  function RadToDeg(const A: TRealType): TRealType;
  function HSVToRGB(Hue, Sat, V: TRealType): TColor;

  { 2D functions. }

  {: Use this function to initialize a 2D point variable.

     This function returns always a cartesing point (W=1).

     Parameters:

     <LI=<I=X> is the X coordinate of the point.>
     <LI=<I=Y> is the Y coordinate of the point.>
  }
  function Point2D(const X, Y: TRealType): TPoint2D;
  {: Use this function to initialize a 2D rectangle variable.

     This function returns always a cartesian rectangle (with W=1).

     Parameters:

     <LI=<I=Left> is the left X coordinate of the rectangle.>
     <LI=<I=Bottom> is the bottom Y coordinate of the rectangle.>
     <LI=<I=Right> is the right X coordinate of the rectangle.>
     <LI=<I=Top> is the top Y coordinate of the rectangle.>
  }
  function Rect2D(const Left, Bottom, Right, Top: TRealType): TRect2D;
  {: Use this function to create a versor.

     A versor is vector whose length is 1. This function compute
     this kind of vector by dividing the vector by its length.
     If the lenght is zero an exception will be raised.

     Parameters:

     <LI=<I=X> is the X coordinate of the versor.>
     <LI=<I=Y> is the Y coordinate of the versor.>
  }
  function Versor2D(const X, Y: TRealType): TVector2D;
  {: This function normalizes a vector.

     Normalizing a Vector means that the vector coordinates
     are divided by the length of the vector. If the lenght is
     zero an exception will be raised.

     Parameters:

     <LI=<I=V> is the vector being normalized.>
  }
  function NormalizeVector2D(const V: TVector2D): TVector2D;
  {: This function returns a versor that rapresent the direction from
     the point <I=PFrom> to the point <I=PTo>.

     The function computes the vector beetwen the two points
     and then normalizes it.

     Parameters:

     <LI=<I=PFrom> is the starting point of the direction.>
     <LI=<I=PTo> is the ending point of the direction.>
  }
  function Direction2D(const PFrom, PTo: TPoint2D): TVector2D;
  {: This function returns the vector from the point
     <I=PFrom> to the point <I=PTo>.

     A vector rapresents the difference of the coordinates
     from the two points.

     The points are made cartesian before the creation of the vector.
  }
  function Vector2D(PFrom, PTo: TPoint2D): TVector2D;
  {: This function returns the modulus of the vector, that
     is its length.

     This value is also called the <I=Euclidian Norm> of the vector.

     Parameters:

     <LI=<I=V> is the versor on which the function is applied.>
  }
  function VectorLength2D(const V: TVector2D): TRealType;
  {: This function returns <B=True> if two points are equal
     and <B=False> otherwise.

     All the coordinates (also W) are tested.

     Parameters:

     <LI=<I=P1> is the first point.>
     <LI=<I=P2> is the second point.>
  }
  function IsSamePoint2D(P1, P2: TPoint2D): Boolean;
  {: This function returns <B=True> if two vectors are equal
     and <I=False> otherwise.

     All the coordinates (also W) are tested.

     Parameters:

     <LI=<I=V1> is the first vector.>
     <LI=<I=V2> is the second vector.>
     <LI=<I=Digits> if specified is the digits at witch stop test (ex: 2=at the second digit).>
  }
  function IsSameVector2D(const V1, V2: TVector2D; const Digits: TRoundToRange): Boolean; overload;
  function IsSameVector2D(const V1, V2: TVector2D): Boolean; overload;
  {: This function returns <B=True> if two transform matrices
     are equal and <B=False> otherwise.

     The function use optimizations if the two matrices are
     cartesian matrices.

     Parameters:

     <LI=<I=T1> is the first matrix.>
     <LI=<I=T2> is the second matrix.>
  }
  function IsSameTransform2D(const T1, T2: TTransf2D): Boolean;
  {: This function returns <B=True> if a transform matrix is
     a cartesian matrix, that is a matrix with the third column
     equal to [0, 0, 1].

     Parameters:

     <LI=<I=T> is the matrix being tested.>
  }
  function IsCartesianTransform2D(const T: TTransf2D): Boolean;
  {: This function returns the cartesian point equivalent
     to <I=P>.

     A cartesian point has the W coordinate equals to 1.
     To convert the point all the coordinates are divided by W.

     When a point is made cartesian, there is no way to obtain
     the original point back.

     Parameters:

     <LI=<I=P> is the point being transformed.>
  }
  function CartesianPoint2D(const P: TPoint2D): TPoint2D;
  {: This function returns the cartesian rectangle equivalent
     to <I=R>.

     A cartesian rectangle has the W coordinate of its corners
     equals to 1. To convert the rectangle all the coordinates
     of the two corners are divided by their W.

     When a rectangle is made cartesian, there is no way to
     obtain the original rectangle back.

     Parameters:

     <LI=<I=R> is the rectangle being transformed.>
  }
  function CartesianRect2D(const R: TRect2D): TRect2D;
  {: This function returns an ordered rectangle in which the
     first point is the left-botton one and the second point
     is the right-top one.

     The returned rectangle is always cartesian.

     Parameters:

     <LI=<I=R> is the rectangle being ordered.>
  }
  function ReorderRect2D(const R: TRect2D): TRect2D;
  {: This function transforms a point into a point in
     which the coordinates are integers. To do the
     transformation the function <I=Round> is used.

     The resulting point is specified in Windows screen coordinate
     system.

     Parameters:

     <LI=<I=P2D> is the point being transformed.>
  }
  function Point2DToPoint(const P2D: TPoint2D): TPoint;
  {: This function transforms a point with integer
     coordinates into a point with real coordinates.

     <I=P> is considered as specified in Windows screen
     coordinate system.

     Parameters:

     <LI=<I=P> is the point being transformed.>
  }
  function PointToPoint2D(const P: TPoint): TPoint2D;
  {: This function transforms a rectangle with integer
     coordinates into a rectangle with real coordinates.

     <I=R> is considered as specified in Windows screen
     coordinate system.

     Parameters:

     <LI=<I=R> is the rectangle being transformed.>
  }
  function RectToRect2D(const R: TRect): TRect2D;
  {: This function transforms a rectangle with real coordinates
     into a rectangle with integer coordinates.
     To do the transformation the function <I=Round> is used.

     The resulting rectangle is specified in Windows screen coordinate
     system.

     Parameters:

     <LI=<I=R> is the rectangle being transformed.>
  }
  function Rect2DToRect(R: TRect2D): TRect;
  {: This function enlarges a rectangle by the specified
     percentual.

     The resulting rectangle is always cartesian.

     Parameters:

     <LI=<I=R> is the box being enlarged.>
     <LI=<I=Perc> is the percentual by which enlarge the box.
     A value of 0.5 means the the rectangle is doubled (50%).>
  }
  function EnlargeBoxPerc2D(R: TRect2D; const Perc: TRealType): TRect2D;
  {: This function enlarges a rectangle by the specified
     delta in all directions.

     The resulting rectangle is always cartesian.

     Parameters:

     <LI=<I=R> is the box being enlarged.>
     <LI=<I=Delta> is the amount by which enlarge the box.
     A value of 0.1 means the the rectangle is enlarged by
     0.1 along the X-Y directions.
  }
  function EnlargeBoxDelta2D(R: TRect2D; const Delta: TRealType): TRect2D;
  {: This function multiplies two affine matrix. It
     returns the resulting matrix.

     The order of multiplication is important for matrixes.

     This function may be useful to concatenate transformation
     matrixes.

     Parameters:

     <LI=<I=M1> is the first matrix.>
     <LI=<I=M2> is the second matrix.>
  }
  function MultiplyTransform2D(const M1, M2: TTransf2D): TTransf2D;
  {: This function returns the invers of a matrix.

     The matrix must be non singular (that is its determinant
     is non zero).

     This function may be useful to obtain the inverse of
     a transformation.

     Parameters:
     <LI=<I=M> is the matrix to be inverted.>
  }
  function InvertTransform2D(const M1: TTransf2D): TTransf2D;
  {: This function transforms a points by using a
     transformation matrix.

     Use <See function=Translate2D>, <See function=Rotate2D>
     and <See function=Scale2D> to obtain a transformation
     matrix for the most common transforms.

     Parameters:

     <LI=<I=P> is the point to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformPoint2D(const P: TPoint2D; const T: TTransf2D): TPoint2D;
  {: This function transforms a vector by using a transformation
     matrix.

     Use <See function=Translate2D>, <See function=Rotate2D> and
     <See function=Scale2D> to obtain a transformation matrix
     for the most common transforms.

     Parameters:

     <LI=<I=V> is the vector to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformVector2D(const V: TVector2D; const T: TTransf2D): TVector2D;
  {: This function transforms a rectangle by using a
     transformation matrix. The first corner and the second corner
     of the rectangle are transformed.

     Use <See function=Translate2D>, <See function=Rotate2D> and
     <See function=Scale2D> to obtain a transformation matrix
     for the most common transforms.

     Parameters:

     <LI=<I=R> is the rectangle to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformRect2D(const R: TRect2D; const T: TTransf2D): TRect2D;
  {: This function transforms a bounding box by using a
     transformation matrix. The resulting rectangle is a the
     bounding box that fully contains the passed rectangle.

     Use <See function=Translate2D>, <See function=Rotate2D> and
     <See function=Scale2D> to obtain a transformation matrix
     for the most common transforms.

     Parameters:

     <LI=<I=R> is the rectangle to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformBoundingBox2D(const Box: TRect2D; const Transf: TTransf2D): TRect2D;
  {: This function returns a transformation matrix that
     correspond to a 2D translation.

     Parameters:

     <LI=<I=Tx> is the translation among the X axis.>
     <LI=<I=Ty> is the translation among the Y axis.>
  }
  function Translate2D(const Tx, Ty: TRealType): TTransf2D;
  {: This function returns a transformation matrix that
     correspond to a 2D rotation.

     The center of rotation is in (0, 0). To rotate along a
     point (px, py), concatenate the following matrixes:

     <Code=
       Translate(-px, -py) -> Rotate2D(A) -> Translate(px, py)
     >

     Parameters:

     <LI=<I=R> is the angle of rotation in radiants.>
  }
  function Rotate2D(const R: TRealType): TTransf2D;
  {: This function returns a transformation matrix that
     correspond to a 2D scaling.

     Parameters:

     <LI=<I=Sx> is the scaling among the X axis.>
     <LI=<I=Sy> is the scaling among the Y axis.>
  }
  function Scale2D(const Sx, Sy: TRealType): TTransf2D;
  {: This function creates a matrix transform that maps
     the points in the W window, in points in the V window.

     The function mantains the aspect ratio desired (if non zero).

     Parameters:

     <LI=<I=W> is the source rectangle>
     <LI=<I=V> is the destionation rectangle>
     <LI=<I=Aspect> is the aspect ratio, that is <I=XW/YW>.>
  }
  function GetVisualTransform2D(var W: TRect2D; const V: TRect; Aspect: TRealType): TTransf2D;
  {: This procedure may be used to apply an ortogonal
     constraint to a point.

     The parameter <I=CurrPt> is modified so that the
     ray beetwen <I=LastPt> and <I=CurrPt> is parallel
     to the axis <I=X> or <I=Y>. The new <I=CurrPt> will
     be on that ray, and the ray will be parallel to the
     axis that lead to the greter lenght on the ray.

     Parameters:

     <LI=<I=LastPt> is the reference point>
     <LI=<I=CurrPt> is the point to be constrained.>
  }
  procedure MakeOrto2D(LastPt: TPoint2D; var CurrPt: TPoint2D);
  {: This function returns the dot product of two vectors.
     The resulting value is given by <I=S=a.X*b.X+a.Y*b.Y>.

     If the vectors are versors, then the result value is
     the coseno of the angle beetwen the versors.

     Parameters:

     <LI=<I=A> is the first vector>
     <LI=<I=B> is the second vector>
  }
  function DotProduct2D(const A, B: TVector2D): TRealType;
  {: This function returns the vector that is
     perpendicular to the given one.

     Parameters:

     <LI=<I=V> is the reference vector>
  }
  function Perpendicular2D(const V: TVector2D): TVector2D;
  {: This function returns the vector that rapresent
     the opposite of the given one.

     The result will be <I=(-V.X, -V.Y)>.

     Parameters:

     <LI=<I=V> is the reference vector>
  }
  function Reflect2D(const V: TVector2D): TVector2D;
  {: This function clips a segment against a rectangular
     clipping region.

     The function returns the clipping result code
     and change <I=Pt1> and <I=Pt2> on the base of the clipping.

     The resulting value can be one of the following:

     <LI=<I=[ccNotVisible]> if the segment is not visible
      in the clipping region.>
     <LI=<I=[ccVisible]> if the segment is fully contained
      in the clipping region.>
     <LI=<I=[ccFirstEdge]> if the first point of segment
      was clipped.>
     <LI=<I=[ccSecondEdge]> if the second point of segment
      was clipped.>
     <LI=<I=[ccFirstEdge, ccSecondEdge]> if the bothe point
      of segment were clipped.>

     Parameters:

     <LI=<I=Clip> is the clipping region.>
     <LI=<I=Pt1> is the first point of the segment.>
     <LI=<I=Pt2> is the second point of the segment.>
  }
  function ClipLine2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
  {: This function clips a segment against the left and right
     edges of a rectangular clipping region.

     The function returns the clipping result code and change
     <I=Pt1> and <I=Pt2> on the base of the clipping.

     The resulting value can be one of the following:

     <LI=<I=[ccNotVisible]> if the segment is not visible
      in the clipping region.>
     <LI=<I=[ccVisible]> if the segment is fully contained in
      the clipping region.>
     <LI=<I=[ccFirstEdge]> if the first point of segment
      was clipped.>
     <LI=<I=[ccSecondEdge]> if the second point of segment was
      clipped.>
     <LI=<I=[ccFirstEdge, ccSecondEdge]> if both the point
      of segment were clipped.>

     Parameters:

     <LI=<I=Clip> is the clipping region.>
     <LI=<I=Pt1> is the first point of the segment.>
     <LI=<I=Pt2> is the second point of the segment.>
  }
  function ClipLineLeftRight2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
  {: This function clips a segment against the upper and
     bottom edges of a rectangular clipping region.

     The function returns the clipping result code and
     change <I=Pt1> and <I=Pt2> on the base of the clipping.

     The resulting value can be one of the following:

     <LI=<I=[ccNotVisible]> if the segment is not visible in
      the clipping region.>
     <LI=<I=[ccVisible]> if the segment is fully contained in
      the clipping region.>
     <LI=<I=[ccFirstEdge]> if the first point of segment was
      clipped.>
     <LI=<I=[ccSecondEdge]> if the second point of segment
      was clipped.>
     <LI=<I=[ccFirstEdge, ccSecondEdge]> if both the point
      of segment were clipped.>

     Parameters:

     <LI=<I=Clip> is the clipping region.>
     <LI=<I=Pt1> is the first point of the segment.>
     <LI=<I=Pt2> is the second point of the segment.>
  }
  function ClipLineUpBottom2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
  {: This function returns <B=True> if the point <I=P> lies
     on the segment from <I=P1> to <I=P2>.

     Parameters:

     <LI=<I=P> is the testing point>
     <LI=<I=P1> is the first point of the segment.>
     <LI=<I=P2> is the second point of the segment.>
  }
  function IsPointOnSegment2D(P, P1, P2: TPoint2D): Boolean;
  {: This function returns the distance of <I=P> from the
     segment from <I=P1> to <I=P2>.

     Parameters:

     <LI=<I=P> is the testing point>
     <LI=<I=P1> is the first point of the segment.>
     <LI=<I=P2> is the second point of the segment.>
  }
  function PointLineDistance2D(P, P1, P2: TPoint2D): TRealType;
  {: This function returns the distance beetwen two points.

     The resulting value is always positive.

     Parameters:

     <LI=<I=P1> is the first point>
     <LI=<I=P2> is the second point>
  }
  function PointDistance2D(const P1, P2: TPoint2D): TRealType;
  {: This function returns <B=True> if the point P is in the axis aligned
     box with the side equal to 2*Aperture and centered at the point <I=RP>.
     So a point may be considered near also if its distance from RP is
     greater than Aperture.

     Parameters:

     <LI=<I=RP> is the reference point>
     <LI=<I=P> is the testing point>
     <LI=<I=Aperture> is the half side of reference box>
     <LI=<I=Dist> will contains the distance of <I=P>
      from <I=RP>. If the function returns <B=False>,
      <I=Dist> will be set to <See const=MaxCoord>.
  }
  function NearPoint2D(RP, P: TPoint2D; const Aperture: TRealType; var Dist: TRealType): Boolean;
  {: This function returns a position code that rapresent the
     position of a point respect to the given polyline.

     The result value is one of the following:

     <LI=<I=PICK_NOOBJECT> if the point <I=P> isn't near to
      the polyline>
     <LI=<I=PICK_ONOBJECT> if the point <I=P> is near to
      the passed polyline by a distance less than <I=Aperture>.>

     Parameters:

     <LI=<I=Vect> is the reference polyline. It is a PVectPoints2D
         and is returned by <See Property=TPointsSet2D@PointsReference> >
     <LI=<I=Count> is the number of points of the polyline >
     <LI=<I=P> is the testing point>
     <LI=<I=Dist> will contains the real distance of <I=P>
      from the polyline. If the function returns
      <I=PICK_NOOBJECT>, <I=Dist> will be set to
      <See const=MaxCoord>.>
     <LI=<I=Aperture> is the reference distance>
     <LI=<I=T> is an optional transformation matrix.
      The polyline is transformed by this matrix before
      the testing.>
     <LI=<I=MustClose>. If it is <B=True> the polyline will
      be closed (if it is not already closed).>
  }
  function IsPointOnPolyLine2D(const Vect: Pointer; Count: Integer;
                               P: TPoint2D; var Dist: TRealType;
                               const Aperture: TRealType; const T: TTransf2D;
                               const MustClose: Boolean): Integer;
  {: This function returns a position code that rapresent the
     position of a point respect to the given closed polygon.

     The result value is one of the following:

     <LI=<I=PICK_NOOBJECT> if the point <I=P> isn't near to the
      polygon>
     <LI=<I=PICK_ONOBJECT> if the point <I=P> is near to the
      passed polygon by a distance less than <I=Aperture>.>
     <LI=<I=PICK_INOBJECT> if the point is inside the polygon.

     Parameters:

     <LI=<I=Vect> is the reference polyline. It is a PVectPoints2D
         and is returned by <See Property=TPointsSet2D@PointsReference> >
     <LI=<I=Count> is the number of points of the polyline >
     <LI=<I=P> is the testing point>
     <LI=<I=Dist> will contains the real distance of <I=P>
      from the polyline. If the function returns
      <I=PICK_NOOBJECT>, <I=Dist> will be set to
      <See const=MaxCoord>.>
     <LI=<I=Aperture> is the reference distance>
     <LI=<I=T> is an optional transformation matrix. The
      polyline is transformed by this matrix before the testing.>
  }
  function IsPointInPolygon2D(const Vect: Pointer; Count: Integer;
                              P: TPoint2D; var Dist: TRealType;
                              const Aperture: TRealType; const T: TTransf2D): Integer;
  {: This function returns a position code that rapresent
     the position of a point respect to the given line.

     The result value is one of the following:

     <LI=<I=PICK_NOOBJECT> if the point <I=P> isn't near to
      the segment>
     <LI=<I=PICK_ONOBJECT> if the point <I=P> is near to
      the passed segment by a distance less than <I=Aperture>.>

     Parameters:

     <LI=<I=A> is the first point of the segment>
     <LI=<I=B> is the second point of the segment>
     <LI=<I=P> is the testing point>
     <LI=<I=Dist> will contains the real distance of
      <I=P> from the segment. If the function returns
      <I=PICK_NOOBJECT>, <I=Dist> will be set to <See const=MaxCoord>.>
     <LI=<I=Aperture> is the reference distance>
     <LI=<I=T> is an optional transformation matrix.
      The line is transformed by this matrix before the testing.>
  }
  function IsPointOnLine2D(A, B, P: TPoint2D; var Dist: TRealType;
                           const Aperture: TRealType; const T: TTransf2D): Integer;
  {: This function returns a position code that rapresent the
    position of a point respect to the given rectangle.

    The result value is one of the following:

    <LI=<I=PICK_NOOBJECT> if the point <I=P> isn't near to
     the rectangle>
    <LI=<I=PICK_ONOBJECT> if the point <I=P> is near to the
     passed rectangle by a distance less than <I=Aperture>.>
    <LI=<I=PICK_INOBJECT> if the point is inside the rectangle.>

    Parameters:

    <LI=<I=Box> is the axis alligned rectangle>
    <LI=<I=P> is the testing point>
    <LI=<I=Dist> will contains the real distance of <I=P> from
     the box. If the function returns
     <I=PICK_NOOBJECT>, <I=Dist> will be set to <See const=MaxCoord>.>
    <LI=<I=Aperture> is the reference distance>
    <LI=<I=T> is an optional transformation matrix. The
     box is transformed by this matrix before the testing.>
  }
  function IsPointOnRect2D(const Box: TRect2D; P: TPoint2D; var Dist: TRealType;
                           const Aperture: TRealType; const T: TTransf2D): Integer;
  {: This function returns <B=True> if the testing point is
     inside the reference rectangle.

     The function works on the assumption that the parameters
     are cartesian points (W = 1.0).

     Parameters:

     <LI=<I=Box> is the axis alligned rectangle>
     <LI=<I=P> is the testing point>
  }
  function IsPointInCartesianBox2D(const Pt: TPoint2D; const Box: TRect2D): Boolean;
  {: This function returns <B=True> if the testing point is
     inside the reference rectangle.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary (W=1.0)
     before do the test.

     Parameters:

     <LI=<I=Pt> is the testing point>
     <LI=<I=Box> is the axis alligned rectangle>
  }
  function IsPointInBox2D(Pt: TPoint2D; Box: TRect2D): Boolean;
  {: This function returns the smallest axis alligned rectangle
     that contains the given points and the given rectangle.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary (W=1.0)
     before do its job.

     Parameters:

     <LI=<I=Pt> is the point>
     <LI=<I=Box> is the axis alligned rectangle>
  }
  function PointOutBox2D(Pt: TPoint2D; Box: TRect2D): TRect2D;
  {: This function returns <B=True> if Box1 is completely or
     partially contained in Box2.

    The function works on both ordinary and non ordinary
    points, for the function makes the points ordinary
    (W=1.0) before do its job.

    Parameters:

    <LI=<I=Box1> is the first axis alligned rectangle>
    <LI=<I=Box2> is the second axis alligned rectangle>
  }
  function IsBoxInBox2D(Box1, Box2: TRect2D): Boolean;
  {: This function returns <B=True> if <I=Box2> is fully
     contained into <I=Box2>.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary
     (W=1.0) before do its job.

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
  function IsBoxAllInBox2D(Box1, Box2: TRect2D): Boolean;
  {: This function returns <B=True> if <I=Box2> is fully
     contained into <I=Box2>.

     The function works on the assumption that the parameters
     are ordinary points (W = 1.0).

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
  function IsBoxAllInCartesianBox2D(const Box1, Box2: TRect2D): Boolean;
  {: This function returns the smallest axis alligned rectangle
     that contains the given rectangles.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary (W=1.0)
     before do its job.

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
  function BoxOutBox2D(Box1, Box2: TRect2D): TRect2D;
  function BoxFillingCartesian2D(const Box1, Box2: TRect2D): Word;

  {: Use this function to initialize a 3D point variable.

     This function returns always a cartesing point (W=1).

     Parameters:

     <LI=<I=X> is the X coordinate of the point.>
     <LI=<I=Y> is the Y coordinate of the point.>
  }
  function Point3D(const X, Y, Z: TRealType): TPoint3D;
  {: Use this function to initialize a 3D rectangle variable.

     This function returns always a cartesian rectangle (with W=1).

     Parameters:

     <LI=<I=Left> is the left X coordinate of the rectangle.>
     <LI=<I=Bottom> is the bottom Y coordinate of the rectangle.>
     <LI=<I=Right> is the right X coordinate of the rectangle.>
     <LI=<I=Top> is the top Y coordinate of the rectangle.>
  }
  function Rect3D(const Left, Bottom, Front, Right, Top, Back: TRealType): TRect3D;
  {: This function normalizes a vector.

     Normalizing a Vector means that the vector coordinates
     are divided by the length of the vector. If the lenght is
     zero an exception will be raised.

     Parameters:

     <LI=<I=V> is the vector being normalized.>
  }
  function NormalizeVector3D(const V: TVector3D): TVector3D;
  {: Use this function to create a versor.

     A versor is vector whose length is 1. This function compute
     this kind of vector by dividing the vector by its length.
     If the lenght is zero an exception will be raised.

     Parameters:

     <LI=<I=X> is the X coordinate of the versor.>
     <LI=<I=Y> is the Y coordinate of the versor.>
  }
  function Versor3D(const X, Y, Z: TRealType): TVector3D;
  {: This function returns a versor that rapresent the direction from
     the point <I=PFrom> to the point <I=PTo>.

     The function computes the vector beetwen the two points
     and then normalizes it.

     Parameters:

     <LI=<I=PFrom> is the starting point of the direction.>
     <LI=<I=PTo> is the ending point of the direction.>
  }
  function Direction3D(const PFrom, PTo: TPoint3D): TVector3D;
  {: This function returns the vector from the point
     <I=PFrom> to the point <I=PTo>.

     A vector rapresents the difference of the coordinates
     from the two points.

     The points are made cartesian before the creation of the vector.
  }
  function Vector3D(PFrom, PTo: TPoint3D): TVector3D;
  {: This function return the matrix <I=M> in which the
     row <I=Row> is substituted with the vector <I=V>.
  }
  function RowVector3D(const M: TTransf3D; Row: Byte; const V: TVector3D): TTransf3D;
  {: This function return the matrix <I=M> in which the
     column <I=Col> is substituted with the vector <I=V>.
  }
  function ColumnVector3D(const M: TTransf3D; Col: Byte; const V: TVector3D): TTransf3D;
  {: This function returns the modulus of the vector, that
     is its length.

     This value is also called the <I=Euclidian Norm> of the vector.

     Parameters:

     <LI=<I=V> is the versor on which the function is applied.>
  }
  function VectorLength3D(V: TVector3D): TRealType;
  {: This function returns <B=True> if two points are equal
     and <B=False> otherwise.

     All the coordinates (also W) are tested.

     Parameters:

     <LI=<I=P1> is the first point.>
     <LI=<I=P2> is the second point.>
  }
  function IsSamePoint3D(P1, P2: TPoint3D): Boolean;
  {: This function returns <B=True> if two vectors are equal
     and <I=False> otherwise.

     All the coordinates (also W) are tested.

     Parameters:

     <LI=<I=V1> is the first vector.>
     <LI=<I=V2> is the second vector.>
     <LI=<I=Digits> if specified is the digits at witch stop test (ex: 2=at the second digit).>
  }
  function IsSameVector3D(const V1, V2: TVector3D; const Digits: TRoundToRange): Boolean; overload;
  function IsSameVector3D(const V1, V2: TVector3D): Boolean; overload;
  {: This function returns <B=True> if two transform matrices
     are equal and <B=False> otherwise.

     The function use optimizations if the two matrices are
     cartesian matrices.

     Parameters:

     <LI=<I=T1> is the first matrix.>
     <LI=<I=T2> is the second matrix.>
  }
  function IsSameTransform3D(const T1, T2: TTransf3D): Boolean;
  {: This function returns <B=True> if a transform matrix is
     a cartesian matrix, that is a matrix with the third column
     equal to [0, 0, 0, 1].

     Parameters:

     <LI=<I=T> is the matrix being tested.>
  }
  function IsCartesianTransform3D(const T: TTransf3D): Boolean;
  {: This function returns the cartesian point equivalent
     to <I=P>.

     A cartesian point has the W coordinate equals to 1.
     To convert the point all the coordinates are divided by W.

     When a point is made cartesian, there is no way to obtain
     the original point back.

     Parameters:

     <LI=<I=P> is the point being transformed.>
  }
  function CartesianPoint3D(const P: TPoint3D): TPoint3D;
  {: This function returns the cartesian rectangle equivalent
     to <I=R>.

     A cartesian rectangle has the W coordinate of its corners
     equals to 1. To convert the rectangle all the coordinates
     of the two corners are divided by their W.

     When a rectangle is made cartesian, there is no way to
     obtain the original rectangle back.

     Parameters:

     <LI=<I=R> is the rectangle being transformed.>
  }
  function CartesianRect3D(const R: TRect3D): TRect3D;
  {: This function returns an ordered rectangle in which the
     first point is the left-botton one and the second point
     is the right-top one.

     The returned rectangle is always cartesian.

     Parameters:

     <LI=<I=R> is the rectangle being ordered.>
  }
  function ReOrderRect3D(const R: TRect3D): TRect3D;
  {: This function transforms a 3D point into the
     equivalent 2D point.

     The resulting point is <I=P3D> projected onto the
     X, Y plane. The coordinates are divided by the
     <I=W> coordinate.

     Parameters:

     <LI=<I=P3D> is the point being transformed.>
  }
  function Point3DToPoint2D(const P3D: TPoint3D): TPoint2D;
  {: This function transforms a 2D point into the
     equivalent 3D point.

     Parameters:

     <LI=<I=P2D> is the point being transformed.>
  }
  function Point2DToPoint3D(const P2D: TPoint2D): TPoint3D;
  {: This function transforms a 3D box into the
     equivalent 2D box.

     The resulting box is <I=R> projected onto the
     X, Y plane. The coordinates are divided by the
     <I=W> coordinate.

     Parameters:

     <LI=<I=R> is the point being transformed.>
  }
  function Rect3DToRect2D(const R: TRect3D): TRect2D;
  {: This function transforms a 2D box into the
     equivalent 3D box.

     Parameters:

     <LI=<I=R> is the point being transformed.>
  }
  function Rect2DToRect3D(const R: TRect2D): TRect3D;
  {: This function enlarges a box by the specified
     percentual.

     The resulting box is always cartesian.

     Parameters:

     <LI=<I=R> is the box being enlarged.>
     <LI=<I=Perc> is the percentual by which enlarge the box.
     A value of 0.5 means the the rectangle is doubled (50%).>
  }
  function EnlargeBoxPerc3D(R: TRect3D; Perc: TRealType): TRect3D;
  {: This function enlarges a box by the specified
     delta in all directions.

     The resulting box is always cartesian.

     Parameters:

     <LI=<I=R> is the box being enlarged.>
     <LI=<I=Delta> is the amount by which enlarge the box.
     A value of 0.1 means the the rectangle is enlarged by
     0.1 along the X-Y-Z directions.
  }
  function EnlargeBoxDelta3D(R: TRect3D; Delta: TRealType): TRect3D;
  {: This function multiplies two affine matrix. It returns the
     resulting matrix.

     The order of multiplication is important for matrixes.

     This function may be useful to concatenate transformation
     matrixes.

     Parameters:

     <LI=<I=M1> is the first matrix.>
     <LI=<I=M2> is the second matrix.>
  }
  function MultiplyTransform3D(const M1, M2: TTransf3D): TTransf3D;
  {: This function returns the invers of a matrix.

     The matrix must be non singular (that is its determinant is
     non zero).

     This function may be useful to obtain the inverse of a
     transformation.

     Parameters:

     <LI=<I=M> is the matrix to be inverted.>
  }
  function InvertTransform3D(const M1: TTransf3D): TTransf3D;
  {: This function transforms a points by using a transformation
     matrix.

     Use <See function=Translate3D>, <See function=Rotate3DX>,
     <See function=Rotate3DY>, <See function=Rotate3DZ> and
     <See function=Scale3D> to obtain a transformation matrix for
     the most common transforms.

     Parameters:

     <LI=<I=P> is the point to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformPoint3D(const P: TPoint3D; const T: TTransf3D): TPoint3D;
  {: This function transforms a vector by using a transformation
     matrix.

     Use <See function=Translate3D>, <See function=Rotate3DX>,
     <See function=Rotate3DY>, <See function=Rotate3DZ> and
     <See function=Scale3D> to obtain a transformation matrix
     for the most common transforms.

     Parameters:

     <LI=<I=V> is the vector to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformVector3D(const V: TVector3D; const T: TTransf3D): TVector3D;
  {: This function transforms a normal vector by using a transformation
     matrix.

     Use <See function=Translate3D>, <See function=Rotate3DX>,
     <See function=Rotate3DY>, <See function=Rotate3DZ> and
     <See function=Scale3D> to obtain a transformation matrix
     for the most common transforms.

     Parameters:

     <LI=<I=V> is the normal vector to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformNormalVector3D(const N: TVector3D; const T: TTransf3D): TVector3D;
  {: This function transforms a rectangle by using a
     transformation matrix. The first corner and the second
     corner of the rectangle are transformed.

     Use <See function=Translate3D>, <See function=Rotate3DX>,
     <See function=Rotate3DY>, <See function=Rotate3DZ> and
     <See function=Scale3D> to obtain a transformation matrix for
     the most common transforms.

     Parameters:

     <LI=<I=R> is the rectangle to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformRect3D(const R: TRect3D; const T: TTransf3D): TRect3D;
  {: This function transforms a bounding box by using a
     transformation matrix. The resulting rectangle is a the
     bounding box that fully contains the passed rectangle.

     Use <See function=Translate3D>, <See function=Rotate3DX>,
     <See function=Rotate3DY>, <See function=Rotate3DZ> and
     <See function=Scale3D> to obtain a transformation matrix for
     the most common transforms.

     Parameters:

     <LI=<I=R> is the rectangle to be transformed.>
     <LI=<I=T> is the transformation matrix.>
  }
  function TransformBoundingBox3D(Box: TRect3D; const Transf: TTransf3D): TRect3D;
  {: This function returns a transformation matrix that
     correspond to a 3D translation.

     Parameters:

     <LI=<I=Tx> is the translation among the X axis.>
     <LI=<I=Ty> is the translation among the Y axis.>
     <LI=<I=Tz> is the translation among the Y axis.>
  }
  function Translate3D(const Tx, Ty, Tz: TRealType): TTransf3D;
  {: This function returns a transformation matrix that
     correspond to a 3D rotation along the X axis.

     The center of rotation is in (0, 0, 0). To rotate along a
     point (px, py, pz), concatenate the following matrixes:

     <Code=
      Translate(-px, -py, -pz) -> Rotate(A) -> Translate(px, py, pz)
     >

     Parameters:

     <LI=<I=R> is the angle of rotation in radiants.>
  }
  function Rotate3DX(const R: TRealType): TTransf3D;
  {: This function returns a transformation matrix that
     correspond to a 3D rotation along the Y axis.

     The center of rotation is in (0, 0, 0). To rotate along a
     point (px, py, pz), concatenate the following matrixes:

     <Code=
      Translate(-px, -py, -pz) -> Rotate(A) -> Translate(px, py, pz)
     >

     Parameters:

     <LI=<I=R> is the angle of rotation in radiants.>
  }
  function Rotate3DY(const R: TRealType): TTransf3D;
  {: This function returns a transformation matrix that
     correspond to a 3D rotation along the Z axis.

     The center of rotation is in (0, 0, 0). To rotate along a
     point (px, py, pz), concatenate the following matrixes:

     <Code=
      Translate(-px, -py, -pz) -> Rotate(A) -> Translate(px, py, pz)
     >

     Parameters:

     <LI=<I=R> is the angle of rotation in radiants.>
  }
  function Rotate3DZ(const R: TRealType): TTransf3D;
  {: This function returns a transformation matrix that
     correspond to a 3D rotation along an axis.

     Parameters:

     <LI=<I=Center> is the center of rotation>
     <LI=<I=Axis> is the axis of rotation>
     <LI=<I=Angle> is the angle of rotation in radiants.>
  }
  function RotateOnAxis3D(const Center: TPoint3D; const Axis: TVector3D; const Angle: TRealType): TTransf3D;
  {: This function returns a transformation matrix that
     correspond to a 3D scaling.

     Parameters:

     <LI=<I=Sx> is the scaling among the X axis.>
     <LI=<I=Sy> is the scaling among the Y axis.>
     <LI=<I=Sz> is the scaling among the Z axis.>
  }
  function Scale3D(const Sx, Sy, Sz: TRealType): TTransf3D;
  {: This function returns a matrix that transform the
     world coordinate system in the view plane coordinate system.

     Parameters:

     <LI=<I=VRP> is the origin of the view plane coordinate system>
     <LI=<I=VPN> is the normal direction of the view plane>
     <LI=<I=VUP> is the up direction of the projection plane>
  }
  function ViewOrientationTransform3D(const VRP: TPoint3D; const VPN, VUP: TVector3D): TTransf3D;
  {: This function returns a transformation matrix that may be
     used to project and normalize points in a parallel projection view.

     Parameters:

     <LI=<I=ViewWindow> is the window on the projection plane>
     <LI=<I=F> is the front clipping plane (referenced to the
      positive normal direction)>
     <LI=<I=B> is the back clipping plane (referenced to the
      positive normal direction)>
  }
  function ParallelViewNormalization3D(ViewWindow: TRect2D; const F, B: TRealType): TTransf3D;
  {: This function returns a transformation matrix that may be
     used to project and normalize points in a perspective projection view.

     Parameters:

     <LI=<I=ViewWindow> is the window on the projection plane that will be
      visualized on the screen>
     <LI=<I=PlaneDistance> is the distance from the projection plane and
      the center of projection>
     <LI=<I=F> is the front clipping plane (referenced to
      the positive normal direction)>
     <LI=<I=B> is the back clipping plane (referenced to the
      positive normal direction)>
  }
  function PerspectiveViewNormalization3D(ViewWindow: TRect2D; const PlaneDistance, F, B: TRealType): TTransf3D;
  {: This function returns the vector in the opposite direction
     specified by <I=V>.

     The result will be <I=(-V.X, -V.Y, -V.Z)>.

     Parameters:

     <LI=<I=V> is a vector>
  }
  function Reflect3D(const V: TVector3D): TVector3D;
  {: This function returns the cross product of two vectors.

     This function may be used to obtain a vector that is
     perpendicular to the plane defined by <I=V1> and <I=V2>.
  }
  function CrossProd3D(const V1, V2: TVector3D): TVector3D;
  {: This function returns the dot product of two vectors. The
     resulting value is given by <I=a.X*b.X+a.Y*b.Y+a.Z*b.Z>.

     If the vectors are versors, then the result value is the
     coseno of the angle beetwen the versors.

     Parameters:

     <LI=<I=A> is the first vector>
     <LI=<I=B> is the second vector>
  }
  function DotProduct3D(const V1, V2: TVector3D): TRealType;
  {: This function returns the distance beetwen two points.

     The resulting value is always positive.

     Parameters:

     <LI=<I=P1> is the first point>
     <LI=<I=P2> is the second point>
  }
  function PointDistance3D(const P1, P2: TPoint3D): TRealType;
  {: This procedure may be used to apply an ortogonal contrain
     to a point.

     The parameter <I=CurrPt> is modified so that the ray
     from <I=LastPt> to <I=CurrPt> is parallel to the
     axis X or Y. The new <I=CurrPt> will be on that ray,
     and the ray will be parallel to the axis that leads to
     the greter lenght on the ray.

     The constrain is always in 2D coordinates, and so you need
     to specify the plane of the 2D coordinates system.

     Parameters:

     <LI=<I=LastPt> is the reference point>
     <LI=<I=CurrPt> is the point to be constrained.>
     <LI=<I=PlaneOrigin> is the origin of the 2D reference plane>
     <LI=<I=PlaneNormal> is the normal direction of the 2D
      reference plane>
     <LI=<I=PlaneUP> is the up direction (Y axis) of the 2D
      reference plane.>
  }
  procedure MakeOrto3D(LastPt: TPoint3D; var CurrPt: TPoint3D; PlaneOrigin: TPoint3D; PlaneNormal, PlaneUP: TVector3D);
  {: This function set the <I=handle rule> for the computation
     of normals.

     The rule will affect the function <See function=GetVectNormal>
     and is a normal setting.

     See also <See Type=THandleRule>.
  }
  procedure SetHandleRuleForNormals(const HRule: THandleRule);
  {: This function returns the current <I=handle rule> setting
     for the computation of normals.

     See also <See Type=THandleRule>.
  }
  function  GetHandleRuleForNormals: THandleRule;
  {: This function returns the normal direction of a set of
     points.

     If the set of points is planar (that is all the points
     in the set lie on a plane) the function returns the
     normal direction of that plane.

     Otherwise a plane is constructed from the set of points
     and its normal direction is returned. The constructed
     plane is the plane that minimize the summation of the
     distances of the points from it.

     Parameters:

     <LI=<I=Vect> is the reference polyline. It is a PVectPoints3D
         and is returned by <See Property=TPointsSet3D@PointsReference> >
     <LI=<I=Count> is the number of points of the polyline >

     See also <See function=SetHandleRuleForNormals> and
     <See Type=THandleRule>.
  }
  function GetVectNormal(const Vect: Pointer; Count: Integer): TVector3D;
  {: This function returns a 3D point that is the extrusion
     of another point along a direction. The resulting point is an ordinary point.

     Parameters:

     <LI=<I=Pt> is the point to be extruded>
     <LI=<I=Dir> is the extrusion direction>
     <LI=<I=Size> is the extrusion length. The resulting point
      will have a distance from <I=Pt> equal to size.>
  }
  function ExtrudePoint3D(Pt: TPoint3D; const Dir: TVector3D; Size: TRealType): TPoint3D;
  {: This function forces a 3D point on a plane and returns it.
     The returning point is in 2D plane reference coordinates
     (with W=1.0).

     The point is projected on the plane along a ray that
     originates from the point and that is parallel to the
     given ray vector (<I=CastDir>).

     Parameters:

     <LI=<I=WPt> is the world point>
     <LI=<I=CastDir> is the ray vector (projector) used to
      project <I=WPt> on the plane>
     <LI=<I=PlaneNorm> is the normal direction of the plane>
     <LI=<I=PlaneOrg> is the origin of the coordinate system
      of the plane.>
     <LI=<I=PlaneUp> is the UP direction of the plane>
  }
  function WorldPointToPlane3D(const WPt: TPoint3D; const CastDir, PlaneNorm, PlaneUP: TVector3D; const PlaneOrg: TPoint3D): TPoint2D;
  {: This function returns a 3D point that is the equivalent
     of a 2D point on a plane.

     Parameters:

     <LI=<I=PPt> is the plane point in plane reference coordinates>
     <LI=<I=PlaneNorm> is the normal direction of the plane>
     <LI=<I=PlaneOrg> is the origin point of the plane.>
     <LI=<I=PlaneUp> is the UP direction of the plane>
  }
  function PlanePointToWorld3D(const PPt: TPoint2D; const PlaneNorm, PlaneUP: TVector3D; const PlaneOrg: TPoint3D): TPoint3D;
  {: This function forces a 3D point on a plane and returns it.

     The point is projected on the plane along a ray that
     originates from the point and that is parallel to the given
     ray vector (<I=CastDir>).

     Parameters:

     <LI=<I=WPt> is the world point>
     <LI=<I=CastDir> is the ray vector (projector) used to
      project WPt on the plane>
     <LI=<I=PlaneNorm> is the normal direction of the plane>
     <LI=<I=PlaneOrg> is the origin point of the plane.>
  }
  function CastPointOnPlane3D(const WPt: TPoint3D; const CastDir, PlaneNorm: TVector3D; const PlaneOrg: TPoint3D): TPoint3D;
  {: This function returns the <I=V> vector forced on the plane
     identified by the normal <I=PlaneNorm>.

     Parameters:

     <LI=<I=V> is the vector to project>
     <LI=<I=PlaneNorm> is the normal direction of the plane>
  }
  function CastVectorOnPlane3D(const V: TVector3D; const PlaneNorm: TVector3D): TVector3D;
  {: This procedure returns the parameters of the plane (that may
     be used for <See function=ViewOrientationTransform3D>) that
     rapresent the projection plane for a visualization system
     with the given camera parameters.

     Parameters:

     <LI=<I=CameraPos> is the camera position of the
      visualization system.>
     <LI=<I=CameraView> is the camera view point of the
      visualization system.>
     <LI=<I=CameraUp> is the camera up direction of the
      visualization system.>
     <LI=<I=PlaneNorm> is the normal direction of the projection
      plane>
     <LI=<I=PlaneUp> is the UP direction of the projection plane>
     <LI=<I=PlaneOrg> is the origin point of the projection plane.>
  }
  procedure ViewPlane(const CameraPos, CameraView: TPoint3D; CameraUp: TVector3D; var PlaneNorm, PlaneUp: TVector3D; var PlaneOrg: TPoint3D);
  {: This procedure returns the coefficients fot the plane equation
     <I=A*X+B*Y+C*Z=D>.

     The plane is identified by a point on it and its normal
     direction.

     Parameters:

     <LI=<I=A, B, C, D> will be the coefficients for the plane
      equation>
     <LI=<I=Normal> is the normal direction of the plane>
     <LI=<I=Org> is a point on the plane>
  }
  procedure NormalPtPlaneToEq3D(const Normal: TVector3D; const Pt: TPoint3D; var A, B, C, D: TRealType);
  {: This procedure returns a point and a vector that are the
     coordinate system origin and the normal direction of the
     plane with equation <LI=A*X+B*Y+C*Z=D>.

     Parameters:

     <LI=<I=A, B, C, D> are the parameters of the plane equation>
     <LI=<I=Normal> will be the normal direction of the plane>
     <LI=<I=Org> will be a point on the plane>
  }
  procedure PlaneEqToNormalPt3D(A, B, C, D: TRealType; var Normal: TVector3D; var Org: TPoint3D);
  {: This function clip a segment against the normalized
     clipping box. This box is axis alligned and its extensions
     are from the point (-1, -1, -1) to the point (1, 1, 0).

     The function returns the clipping result code and change
     <I=Pt1> and <I=Pt2> on the base of the clipping.

     The resulting value can be one of the following:

     <LI=<I=[ccNotVisible]> if the segment is not visible in
      the clipping region.>
     <LI=<I=[ccVisible]> if the segment is fully contained in
      the clipping region.>
     <LI=<I=[ccFirstEdge]> if the first point of segment was
      clipped.>
     <LI=<I=[ccSecondEdge]> if the second point of segment was
      clipped.>
     <LI=<I=[ccFirstEdge, ccSecondEdge]> if the bothe point of
      segment were clipped.>

     Parameters:

     <LI=<I=Pt1> is the first point of the segment (in normalized
      coordinates).>
     <LI=<I=Pt2> is the second point of the segment (in normalized
      coordinates).>
  }
  function ClipLine3D(var Pt1, Pt2: TPoint3D): TClipResult;
  {: This function returns <B=True> if the point <I=P> is near
     to the point <I=RP> for a distance less than <I=Aperture>.

     The method considers the normalized and projected
     coordinates of the points and then uses a 2D algorithm.

     Parameters:

     <LI=<I=RP> is the reference point in world coordinates>
     <LI=<I=P> is the testing point in normalized coordinates
      (obtained previously by means of NT).>
     <LI=<I=Aperture> is the reference distance>
     <LI=<I=Dist> will contains the real distance of <I=P> from
      <I=RP>. If the function returns False, Dist will be set to
      MaxCoord.>
     <LI=<I=NT> is the normalization matrix>
  }
  function NearPoint3D(RP, P: TPoint3D; const Aperture: TRealType;
                       var Dist: TRealType; const NT: TTransf3D): Boolean;
  {: This function returns a <I=position code> that rapresents
     the position of a point respect to the given polyline.

     The result value is one of the following:

     <LI=<I=PICK_NOOBJECT> if the point <I=P> isn't near to the
      polyline>
     <LI=<I=PICK_ONOBJECT> if the point <I=P> is near to the
      passed polyline by a distance less than <I=Aperture>.

     Parameters:

     <LI=<I=Vect> is the reference polyline (in world
                  coordinate system). It is a PVectPoints3D
         and is returned by <See Property=TPointsSet3D@PointsReference> >
     <LI=<I=Count> is the number of points of the polyline >
     <LI=<I=P> is the testing point in normalized coordinates
      (obtained previously by means of NT)>
     <LI=<I=Dist> will contains the real distance of <I=P> from
      the polyline. If the function returns <I=PICK_NOOBJECT>,
      <I=Dist> will be set to MaxCoord.>
     <LI=<I=Aperture> is the reference distance>
     <LI=<I=NT> is the normalization matrix. The polyline is
      transformed by this matrix before the testing.>
     <LI=<I=MustClose>. If it is <B=True> the polyline will
      be closed (if not already).>
  }
  function IsPointOnPolyLine3D(const Vect: Pointer; Count: Integer;
                               const P: TPoint3D; var Dist: TRealType;
                               const Aperture: TRealType; const NT: TTransf3D; const MustClose: Boolean): Integer;
  {: This function returns a <I=position code> that rapresents
     the position of a point respect to the given line.

     The result value is one of the following:

     <LI=<I=PICK_NOOBJECT> if the point <I=P> isn't near to the
      segment>
     <LI=<I=PICK_ONOBJECT> if the point <I=P> is near to the
      passed segment by a distance less than <I=Aperture>.>

     Parameters:

     <LI=<I=A> is the first point of the segment (in world
      coordinates)>
     <LI=<I=B> is the second point of the segment (in world
      coordinates)>
     <LI=<I=P> is the testing point in normalized coordinates
      (obtained previously with NT)>
     <LI=<I=Dist> will contains the real distance of <I=P> from
      the segement. If the function returns <I=PICK_NOOBJECT>,
      <I=Dist> will be set to <I=MaxCoord>.>
     <LI=<I=Aperture> is the reference distance>
     <LI=<I=NT> is the normalization matrix. The polyline is
      transformed by this matrix before the testing.>
  }
  function IsPointOnLine3D(const A, B, P: TPoint3D; var Dist: TRealType;
                           const Aperture: TRealType; const NT: TTransf3D): Integer;
  {: This function returns <B=True> if the testing point is
     inside the reference rectangle.

     The function works on both ordinary and non ordinary points,
     for the function makes the points ordinary (W=1.0) before do
     the test.

     Parameters:

     <LI=<I=Pt> is the testing point>
     <LI=<I=Box> is the axis alligned rectangle>
  }
  function IsPointInBox3D(const Pt: TPoint3D; const Box: TRect3D): Boolean;
  {: This function returns the smallest axis alligned rectangle
     that contains the given points and the given rectangle.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary (W=1.0)
     before do its job.

     Parameters:

     <LI=<I=Pt> is the point>
     <LI=<I=Box> is the axis alligned rectangle>
  }
  function PointOutBox3D(Pt: TPoint3D; Box: TRect3D): TRect3D;
  {: This function returns <B=True> if Box1 is completely or
     partially contained in Box2.

     The function works on both ordinary and non ordinary
     points, for the function makes the points ordinary (W=1.0)
     before do its job.

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
  function IsBoxInBox3D(Box1, Box2: TRect3D): Boolean;
  {: This function returns <B=True> if <I=Box2> is fully
     contained into <I=Box2>.

     The function works on both ordinary and non ordinary points,
     for the function makes the points ordinary (W=1.0) before do
     its job.

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
  function IsBoxAllInBox3D(Box1, Box2: TRect3D): Boolean;
  {: This function returns the smallest axis alligned rectangle
     that contains the given rectangles.

     The function works on both ordinary and non ordinary points,
     for the function makes the points ordinary (W=1.0) before do
     its job.

     Parameters:

     <LI=<I=Box1> is the first axis alligned rectangle>
     <LI=<I=Box2> is the second axis alligned rectangle>
  }
  function BoxOutBox3D(Box1, Box2: TRect3D): TRect3D;
  function BoxFilling3D(Box1, Box2: TRect3D): Word;
  {: This function returns the 2D axis alligned rectangle that
     fully contains the projection of a 3D box on a plane.

     The resulting rect is an ordinary one.

     Parameters:

     <LI=<I=Box> is the 3D rectangle to be projected>
     <LI=<I=WorldToPlane> is a transformation that maps the
      world reference system to the plane reference system (the
      <See function=ViewOrientationTransform3D> may be used to get
      this parameter).>
  }
  function GetBox2DExtension3D(Box: TRect3D; const WorldToPlane: TTransf3D): TRect2D;
  {: This function returns <B=True> if the testing point is
     inside the normalized clipping rectangle. This box is axis
     alligned and its extensions are from (-1, -1, -1) to
     (1, 1, 0).

     The function works on both ordinary and non ordinary points,
     for the function makes the points ordinary (W=1.0) before do
     the test.

     Parameters:

     <LI=<I=Pt> is the testing point in normalized coordinates>
  }
  function IsPointInBoxNRC3D(const Pt: TPoint3D): Boolean;
  {: This function returns <B=True> if <I=Box> is fully
     contained into the normalized clipping rectangle. This box
     is axis alligned and its extensions are from (-1, -1, -1)
     to (1, 1, 0).

     The function works on both ordinary and non ordinary points,
     for the function makes the points ordinary (W=1.0) before do
     its job.

     Parameters:

     <LI=<I=Box> is axis alligned rectangle already normalized
     (and ordered)>
  }
  function IsVisibleBoxNRC3D(Box: TRect3D): Boolean;
  {: This function returns <B=True> if <I=Box> is completely or
     partially contained in the normalized clipping rectangle.
     This box is axis alligned and its extensions are from
     (-1, -1, -1) to (1, 1, 0).

     The function works on both ordinary and non ordinary points,
     for the function makes the points ordinary (W=1.0) before do
     its job.

     Parameters:

     <LI=<I=Box> is axis alligned rectangle already normalized
      (and ordered)>
  }
  function IsBoxAllInBoxNRC3D(Box: TRect3D): Boolean;
  function IsBoxAllInFrontNRC3D(Box: TRect3D): Boolean;
  function BoxFillingNRC3D(Box: TRect3D): Word;

  { Drawing functions
    Vect deve essere di tipo PVectPoints2D, Count è il numero di punti. }
  procedure Draw2DSubSetAsPolygon(const Vect: Pointer; Count: Integer;
                                  const Cnv: TDecorativeCanvas;
                                  const Clip, Extent: TRect2D;
                                  const S: TTransf2D;
                                  const StartIdx, EndIdx: Integer);
  procedure Draw2DSubSetAsPolyline(const Vect: Pointer; Count: Integer;
                                   const Cnv: TDecorativeCanvas;
                                   const Clip, Extent: TRect2D;
                                   const S: TTransf2D;
                                   const StartIdx, EndIdx: Integer;
                                   const ToBeClosed: Boolean);
  {: This is an utility procedure useful when you want to mark a control point of
     a graphic object.

     It draws a small square of size <I=Wdt> centered at <I=X,Y> on the
     canvas <I=Cnv>. All the parameters are refered to window's screen
     coordinates. <I=Wdt> is half of the side of the square.
  }
  procedure DrawPlaceHolder(const Cnv: TDecorativeCanvas; const X, Y, Wdt: Integer);
  procedure DrawRoundPlaceHolder(const Cnv: TDecorativeCanvas; const X, Y, Wdt: Integer);
  {: This function can be used to draw a rectangle onto a
     canvas.

     The rectangle being drawed is first transformed by <I=MT>,
     then it is clipped within the specified region and
     then mapped on the screen with the matrix <I=S>.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to Box.>
     <LI=<I=S> is the mapping matrix used to map the clipped Box to the screen.>
  }
  procedure DrawRect2DAsPolyline(const Cnv: TDecorativeCanvas; const Box, Clip: TRect2D; const MT, S: TTransf2D);
  {: This function can be used to draw a box onto a canvas.

     The box being drawed is first transformed by
     <I=MT>, then it is clipped within the specified region and
     then mapped on the screen with the matrix <I=S>.

     The rectangle is filled with the canvas brush.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to Box>
     <LI=<I=S> is the mapping matrix used to map the clipped Boxto the screen.>
  }
  procedure DrawRect2DAsPolygon(const Cnv: TDecorativeCanvas; const Box, Clip: TRect2D; const MT, S: TTransf2D);
  {: This function can be used to draw a line onto a canvas.

     The line being drawed is clipped within the specified
     region and then mapped on the screen with the matrix <I=S>.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the line is drawed.>
     <LI=<I=P1> is the first point of the line.>
     <LI=<I=P2> is the second point of the line.>
     <LI=<I=Clip>. This is the clipping rectangle.>
     <LI=<I=S> is the mapping matrix used to map the clipped line to the screen.>
  }
  function DrawLine2D(const Cnv: TDecorativeCanvas; P1, P2: TPoint2D; const Clip: TRect2D; const S: TTransf2D): Boolean;
  {: This function can be used to draw a bounding box onto
     a canvas.

     The box being drawed is first transformed by <I=MT>.
     This transformation keep the bounding box nature of
     the rectangle, so the transformed box has the edges
     parallel to the axis of the world.

     After that it is mapped onto the screen and clipped
     against the clipping region.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=Clip> is the clipping rectangle.>
     <LI=<I=MT> is the transform matrix to be applied to <I=Box>.>
     <LI=<I=S> is the mapping matrix used to map the clipped
      <I=Box> to the screen.>
  }
  procedure DrawBoundingBox2D(const Cnv: TDecorativeCanvas; Box, Clip: TRect2D; const S: TTransf2D);

  { Vect deve essere di tipo PVectPoints3D, Count è il numero di punti. }
  procedure Draw3DSubSetAsPolyline(const Vect: Pointer; Count: Integer;
                                   const Cnv: TDecorativeCanvas;
                                   const {%H-}Extent: TRect3D;
                                   const NT: TTransf3D;
                                   const VT: TTransf2D;
                                   const StartIdx, EndIdx: Integer;
                                   const ToBeClosed: Boolean);
  {: This function can be used to draw a line onto a canvas.

     The line being drawed is normalized with <I=NT>, then it
     is clipped within the specified region and mapped on the
     screen with the matrix <I=VT>.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the line is drawed.>
     <LI=<I=P1> is the first point of the line.>
     <LI=<I=P2> is the second point of the line.>
     <LI=<I=NT> is the normalization matrix used to project and
      normalize the line to the screen. (See
      <See Method=TCADViewport3D@ViewNormalization>.
     <LI=<I=VT> is the mapping matrix used to map the clipped line to the screen.>
  }
  function DrawLine3D(const Cnv: TDecorativeCanvas; P1, P2: TPoint3D; const NT: TTransf3D; const VT: TTransf2D): Boolean;
  {: This function can be used to draw a bounding box onto a
     canvas.

     It draws a box with hidden lines removed.

     The box being drawed is first transformed by <I=NT>. This
     transformation keep the bounding box nature of the
     rectangle, so the normalized box has the edges parallel
     to the axis of the world.

     After that the box is clipped to the canonical bounding
     box, it is mapped onto the screen and clipped against
     the clipping region.

     Parameters:

     <LI=<I=Cnv> is the canvas on which the rectangle is drawed.>
     <LI=<I=VRP> is the view reference point (at the
      center of the current viewport) used to remove the hidden
      lines.>
     <LI=<I=Box> is the rectangle to be drawed.>
     <LI=<I=NT> is the normalization matrix used
      to project and normalize the line to the screen.
      (See <See Method=TCADViewport3D@ViewNormalization>).>
     <LI=<I=VT> is the mapping matrix used to map the clipped line
     to the screen.>
  }
  procedure DrawBoundingBox3D(const Cnv: TDecorativeCanvas; const {%H-}VRP: TPoint3D; Box: TRect3D; const NT: TTransf3D; const VT: TTransf2D);

  { The below class utilities are for graphics object registration. }
  {: This procedure resets the list of graphic object registrations.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
  procedure CADSysInitClassRegister;
  {: This function search for the index of the registered
     graphic object. If no match is found it returns -1.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
  function  CADSysFindClassIndex(const Name: String): Word;
  {: This function search for the index of the registered
     graphic object by using its class name. If no match is
     found it returns -1.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
  function  CADSysFindClassByName(const Name: String): TGraphicObjectClass;
  {: This function returns the class reference of the graphic
     object which the specified registration index.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
  function  CADSysFindClassByIndex(Index: Word): TGraphicObjectClass;
  {: This procedure registers a new graphic object class in the
     library PERSISTANCE system.

     This method must be used to register your owns graphical
     object classes so that they can be streamed to a drawing
     file automatically by the library.

     <I=Index> is the registration index you choose.
     If a class is already registered in that slot an exception
     will be raised. It is better to register your classe from
     the index 150 on.

     There are <I=MAX_REGISTERED_CLASSES> slots in the
     registration list.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
  procedure CADSysRegisterClass(Index: Word; const GraphClass: TGraphicObjectClass);
  {: This procedure unregisters a graphic object class in the
     library PERSISTANCE system.

     <I=Index> is the registration index to clear.

     See also <See=Object's Persistance@PERSISTANCE>.
  }
  procedure CADSysUnregisterClass(Index: Word);
  {: This function converts a pascal string to a <I=source block>
     name type.

     See also <See Type=TSourceBlockName>
  }
  function StringToBlockName(const Str: String): TSourceBlockName;

const
  {: This constant contains the version number of the library
     used as drawing file's header.
  }
  CADSysVersion: TCADVersion = 'CAD423';
  {: This constant is used as <I=drawing mode> value for
     the <See Property=TCADViewport@DrawMode>.
  }
  DRAWMODE_NORMAL = 0;
  {: This constant is used as <I=drawing mode> value for
     the <See Property=TCADViewport@DrawMode>.

     The <I=draw mode> values used as toogle flags. If this
     value is present in the DrawMode property no drawing
     will produced.
  }
  DRAWMODE_NODRAW = 1;
  {: This constant is used as <I=drawing mode> value for
     the <See Property=TCADViewport@DrawMode>.

     The <I=draw mode> values used as toogle flags. If this
     value is present in the DrawMode property the object
     are drawed with their control points.

     <B=Note>: Your drawing shape classess must understands these
     flags and behaves accordingly.
  }
  DRAWMODE_SHOWCTRLPOINTS = 2;
  {: This constant is used as <I=drawing mode> value for
     the <See Property=TCADViewport@DrawMode>.

     The <I=draw mode> values used as toogle flags. If this
     value is present in the DrawMode property only front
     facing planar object will be drawed (only for 3D viewports).

     The font facing check is <B=True> if the ray from the
     camera position to the view point is opposite to the face
     normal (ie you are looking the font face).

     <B=Note>: Your drawing shape classess must understands these
     flags and behaves accordingly.
  }

  DRAWMODE_BACKFACECULLING = 4;
  {: This constant is used as <I=drawing mode> value for
     the <See Property=TCADViewport@DrawMode>.

     The <I=draw mode> values used as toogle flags. If this
     value is present in the DrawMode property the front
     facing objects will be drawed in green and the back
     facing objects will be drawed in red (only for 3D viewports).

     The font facing check is <B=True> if the ray from the
     camera position to the view point is opposite to the face
     normal (ie you are looking the font face).

     <B=Note>: Your drawing shape classess must understands these
     flags and behaves accordingly.
  }
  DRAWMODE_SHOWORIENTATION = 8;
  {: This constant is used as <I=drawing mode> value for
     the <See Property=TCADViewport@DrawMode>.

     The <I=draw mode> values used as toogle flags. If this
     value is present in the DrawMode property only the
     bounding boxes of the objects are drawed.

     <B=Note>: Your drawing shape classess must understands these
     flags and behaves accordingly.
  }
  DRAWMODE_ONLYBOUNDINGBOX = 16;

  {: This constant is used as <I=drawing mode> value for
     the <See Property=TCADViewport@DrawMode>.

     The <I=draw mode> values used as toogle flags. If this
     value is present in the DrawMode property the vectorial
     text objects will be drawed with only their bounding
     boxes.

     <B=Note>: Your drawing shape classess must understands these
     flags and behaves accordingly.
  }
  DRAWMODE_VECTTEXTONLYBOX = 32;

  {:
  }
  MAX_REGISTERED_CLASSES = 512;

  {: This commands stops the current task with success.

     See also <See Method=TCADPrg@SendUserEvent>.
  }
  CADPRG_ACCEPT = 1;
  {: This commands aborts the current task.

     See also <See Method=TCADPrg@SendUserEvent>.
  }
  CADPRG_CANCEL = 2;

Implementation

uses Dialogs, Forms, CS4Shapes;

type
  TPaintingThread = class(TThread)
  private
    { Private declarations }
    fOwner: TCADViewport;
    fRect: TRect2D;
  public
    // Assign also the OnTerminate event.
    constructor Create(const Owner: TCADViewport; const ARect: TRect2D);
    destructor Destroy; override;

    procedure Execute; override;
  end;

//  TGraphicClassRegistered = ;

  PObjBlock = ^TObjBlock;

  TObjBlock = record
    Obj: TGraphicObject; { Graphic object. }
    Next, Prev: PObjBlock; { Linked list pointer. }
  end;

var
  GraphicObjectsRegistered: array[0..512] of TGraphicObjectClass;
  _HandleRuleForNormal: THandleRule = hrRightHand;

// 2D Clipping functions.

{ This function returns the position code of P against Clip.
  P and Clip must be cartesian point, Clip must be ordered.
}
function _PositionCode2D(const Clip: TRect2D; const P: TPoint2D): TOutCode;
begin
  Result := [];
  if P.X < Clip.Left then
   Result := [left]
  else if P.X > Clip.Right then
   Result := [right];
  if P.Y < Clip.Bottom then
   Result := Result + [bottom]
  else if P.Y > Clip.Top then
   Result := Result + [top];
end;

{ This function is used to implement the Liang-Barsky clipping method. }
function _ClipPt(const Denom, Num: Extended; var tE, tL: Extended): Boolean;
var
  T: Extended;
begin
  Result := False;
  if Denom > 0 then
   begin
     T := Num / Denom;
     if T > tL then
      Exit
     else if T > tE then
      tE := T;
   end
  else if Denom < 0 then
   begin
     T := Num / Denom;
     if T < tE then
      Exit
     else if T < tL then
      tL := T;
   end
  else if Num > 0 then
   Exit;
  Result := True;
end;

{ Implement the Liang-Barsky algoritm. }
function _ClipLine2D(const Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
var
  DX, DY, tE, tL: Extended;
begin
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip) then
   begin
     Result := [ccVisible];
     Exit;
   end;
  tE := 0.0;
  tL := 1.0;
  // 0.9 in 1.
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, tL) then
   if _ClipPt(-DX, Pt1.X - Clip.Right, tE, tL) then
    if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, tL) then
     if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, tL) then
      begin
        Result := [];
        if tL < 1 then
         begin
           Pt2.X := Pt1.X + tL * DX;
           Pt2.Y := Pt1.Y + tL * DY;
           Result := [ccSecond];
         end;
        if tE > 0 then
         begin
           Pt1.X := Pt1.X + tE * DX;
           Pt1.Y := Pt1.Y + tE * DY;
           Result := Result + [ccFirst];
         end;
        if Result = [] then
         Result := [ccVisible];
      end;
end;

{ Implement the Liang-Barsky algoritm. }
function _ClipLineLeftRight2D(const Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
var
  DX, DY, tE, tL: Extended;
begin
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip) then
   begin
     Result := [ccVisible];
     Exit;
   end;
  tE := 0.0;
  tL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, tL) then
   if _ClipPt(-DX, Pt1.X - Clip.Right, tE, tL) then
    begin
      Result := [];
      if tL < 1 then
       begin
         Pt2.X := Pt1.X + tL * DX;
         Pt2.Y := Pt1.Y + tL * DY;
         Result := [ccSecond];
       end;
      if tE > 0 then
       begin
         Pt1.X := Pt1.X + tE * DX;
         Pt1.Y := Pt1.Y + tE * DY;
         Result := Result + [ccFirst];
       end;
      if Result = [] then
       Result := [ccVisible];
    end;
end;

{ Implement the Liang-Barsky algoritm. }
function _ClipLineUpBottom2D(const Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
var
  DX, DY, tE, tL: Extended;
begin
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip) then
   begin
     Result := [ccVisible];
     Exit;
   end;
  tE := 0.0;
  tL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, tL) then
   if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, tL) then
    begin
      Result := [];
      if tL < 1 then
       begin
         Pt2.X := Pt1.X + tL * DX;
         Pt2.Y := Pt1.Y + tL * DY;
         Result := [ccSecond];
       end;
      if tE > 0 then
       begin
         Pt1.X := Pt1.X + tE * DX;
         Pt1.Y := Pt1.Y + tE * DY;
         Result := Result + [ccFirst];
       end;
      if Result = [] then
       Result := [ccVisible];
    end;
end;

{ General functions }

function DegToRad(const A: TRealType): TRealType;
begin
  Result := A * Pi / 180.0;
end;

function RadToDeg(const A: TRealType): TRealType;
begin
  Result := A * 180.0 / Pi;
end;

// Hue: 0-360
function HSVToRGB(Hue, Sat, V: TRealType): TColor;
var
  i: Integer;
  R, G, B: Integer;
  f, p, q, t: TRealType;
begin
  Hue := (Hue / 60.0);
  i := Trunc(Hue);
  f := Hue - i;
  p := V * (1.0 - Sat);
  q := V * (1.0 - (Sat * f));
  t := V * (1.0 - (Sat * (1.0 - f)));
  R := 0; G := 0; B := 0;
  case i of
   0: begin
     R := Trunc(V * 255);
     G := Trunc(t * 255);
     B := Trunc(p * 255);
   end;
   1: begin
     R := Trunc(q * 255);
     G := Trunc(V * 255);
     B := Trunc(p * 255);
   end;
   2: begin
     R := Trunc(p * 255);
     G := Trunc(V * 255);
     B := Trunc(t * 255);
   end;
   3: begin
     R := Trunc(p * 255);
     G := Trunc(q * 255);
     B := Trunc(V * 255);
   end;
   4: begin
     R := Trunc(t * 255);
     G := Trunc(p * 255);
     B := Trunc(V * 255);
   end;
   5: begin
     R := Trunc(V * 255);
     G := Trunc(p * 255);
     B := Trunc(q * 255);
   end;
  end;
  Result := RGB(R, G, B);
end;

{ 2D functions }

function IsSamePoint2D(P1, P2: TPoint2D): Boolean;
begin
  if( P1.W <> P2.W ) then
   begin
     P1 := CartesianPoint2D(P1);
     P2 := CartesianPoint2D(P2);
   end;
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function IsSameVector2D(const V1, V2: TVector2D; const Digits: TRoundToRange): Boolean; overload;
begin
  Result := (RoundTo(V1.X, Digits) = RoundTo(V2.X, Digits)) and (RoundTo(V1.Y, Digits) = RoundTo(V2.Y, Digits));
end;

function IsSameVector2D(const V1, V2: TVector2D): Boolean; overload;
begin
  Result := (V1.X = V2.X) and (V1.Y = V2.Y);
end;

function IsSameTransform2D(const T1, T2: TTransf2D): Boolean;
begin
  Result := (T1[1, 1] = T2[1, 1])
            and (T1[1, 2] = T2[1, 2])
            and (T1[1, 3] = T2[1, 3])
            and (T1[2, 1] = T2[2, 1])
            and (T1[2, 2] = T2[2, 2])
            and (T1[2, 3] = T2[2, 3])
            and (T1[3, 1] = T2[3, 1])
            and (T1[3, 2] = T2[3, 2])
            and (T1[3, 3] = T2[3, 3]);
end;

function IsCartesianTransform2D(const T: TTransf2D): Boolean;
begin
  Result := (T[1, 3] = 0.0) and (T[2, 3] = 0.0) and (T[3, 3] = 1.0);
end;

function Point2D(const X, Y: TRealType): TPoint2D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.W := 1.0;
end;

function VectorLength2D(const V: TVector2D): TRealType;
begin
  Result := Sqrt(V.X * V.X + V.Y * V.Y);
end;

function Versor2D(const X, Y: TRealType): TVector2D;
begin
  Result.X := X;
  Result.Y := Y;
  Result := NormalizeVector2D(Result);
end;

function NormalizeVector2D(const V: TVector2D): TVector2D;
var
  Modul: TRealType;
begin
  Modul := VectorLength2D(V);
  Result := V;
  if (Modul <> 1.0) and (Modul <> 0.0) then
   begin
     Modul := 1.0 / Modul;
     Result.X := V.X * Modul;
     Result.Y := V.Y * Modul;
   end;
end;

function Vector2D(PFrom, PTo: TPoint2D): TVector2D;
begin
  if PFrom.W <> PTo.W then
   begin
     PFrom := CartesianPoint2D(PFrom);
     PTo := CartesianPoint2D(PTo);
   end;
  Result.X := PTo.X - PFrom.X;
  Result.Y := PTo.Y - PFrom.Y;
end;

function Direction2D(const PFrom, PTo: TPoint2D): TVector2D;
begin
  Result := NormalizeVector2D(Vector2D(PFrom, PTo));
end;

function ReOrderRect2D(const R: TRect2D): TRect2D;
begin
  Result := CartesianRect2D(R);
  if R.Left > R.Right then
   begin
     Result.Left := R.Right;
     Result.Right := R.Left;
   end;
  if R.Bottom > R.Top then
   begin
     Result.Bottom := R.Top;
     Result.Top := R.Bottom;
   end;
end;

function Rect2D(const Left, Bottom, Right, Top: TRealType): TRect2D;
begin
  Result.Left := Left;
  Result.Right := Right;
  Result.W1 := 1.0;
  Result.Bottom := Bottom;
  Result.Top := Top;
  Result.W2 := 1.0;
end;

function CartesianPoint2D(const P: TPoint2D): TPoint2D;
begin
  if (P.W <> 1.0) and (P.W <> 0.0) then
   begin
     Result.X := P.X / P.W;
     Result.Y := P.Y / P.W;
     Result.W := 1.0;
   end
  else
   Result := P;
end;

function CartesianRect2D(const R: TRect2D): TRect2D;
begin
  Result.FirstEdge := CartesianPoint2D(R.FirstEdge);
  Result.SecondEdge := CartesianPoint2D(R.SecondEdge);
end;

function Point2DToPoint(const P2D: TPoint2D): TPoint;
begin
  if (P2D.W <> 1.0) and (P2D.W <> 0.0) then
   begin
     Result.X := Round(P2D.X / P2D.W);
     Result.Y := Round(P2D.Y / P2D.W);
   end
  else
   begin
     Result.X := Round(P2D.X);
     Result.Y := Round(P2D.Y);
   end
end;

function PointToPoint2D(const P: TPoint): TPoint2D;
begin
  Result.X := P.X;
  Result.Y := P.Y;
  Result.W := 1.0;
end;

function RectToRect2D(const R: TRect): TRect2D;
begin
  Result := Rect2D(R.Left, R.Top, R.Right, R.Bottom);
end;

function Rect2DToRect(R: TRect2D): TRect;
begin
  R := ReorderRect2D(R);
  Result.Left := Round(R.Left);
  Result.Right := Round(R.Right);
  Result.Top := Round(R.Bottom);
  Result.Bottom := Round(R.Top);
end;

function EnlargeBoxDelta2D(R: TRect2D; const Delta: TRealType): TRect2D;
begin
  R := CartesianRect2D(R);
  Result.Left := R.Left - Delta;
  Result.Right := R.Right + Delta;
  Result.W1 := 1.0;
  Result.Bottom := R.Bottom - Delta;
  Result.Top := R.Top + Delta;
  Result.W2 := 1.0;
end;

function EnlargeBoxPerc2D(R: TRect2D; const Perc: TRealType): TRect2D;
var
  Marg: TRealType;
begin
  R := CartesianRect2D(R);
  Marg := Abs(R.Right - R.Left) * Perc;
  Result.Left := R.Left - Marg;
  Result.Right := R.Right + Marg;
  Marg := Abs(R.Top - R.Bottom) * Perc;
  Result.Bottom := R.Bottom - Marg;
  Result.Top := R.Top + Marg;
  Result.W1 := 1.0;
  Result.W2 := 1.0;
end;

function Point3D(const X, Y, Z: TRealType): TPoint3D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := 1.0;
end;

function Rect3D(const Left, Bottom, Front, Right, Top, Back: TRealType): TRect3D;
begin
  Result.FirstEdge := Point3D(Left, Bottom, Front);
  Result.SecondEdge := Point3D(Right, Top, Back);
end;

function VectorLength3D(V: TVector3D): TRealType;
begin
  Result := Sqrt(V.X * V.X + V.Y * V.Y + V.Z * V.Z);
end;

function Versor3D(const X, Y, Z: TRealType): TVector3D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result := NormalizeVector3D(Result);
end;

function NormalizeVector3D(const V: TVector3D): TVector3D;
var
  Modul: TRealType;
begin
  Modul := VectorLength3D(V);
  Result := V;
  if (Modul <> 1.0) and (Modul <> 0.0) then
   begin
     Result.X := V.X / Modul;
     Result.Y := V.Y / Modul;
     Result.Z := V.Z / Modul;
   end;
end;

function Vector3D(PFrom, PTo: TPoint3D): TVector3D;
begin
  if PFrom.W <> PTo.W then
   begin
     PFrom := CartesianPoint3D(PFrom);
     PTo := CartesianPoint3D(PTo);
   end;
  Result.X := PTo.X - PFrom.X;
  Result.Y := PTo.Y - PFrom.Y;
  Result.Z := PTo.Z - PFrom.Z;
end;

function ColumnVector3D(const M: TTransf3D; Col: Byte; const V: TVector3D): TTransf3D;
begin
  Result := M;
  Result[1, Col] := V.X;
  Result[2, Col] := V.Y;
  Result[3, Col] := V.Z;
end;

function RowVector3D(const M: TTransf3D; Row: Byte; const V: TVector3D): TTransf3D;
begin
  Result := M;
  Result[Row, 1] := V.X;
  Result[Row, 2] := V.Y;
  Result[Row, 3] := V.Z;
end;

function Direction3D(const PFrom, PTo: TPoint3D): TVector3D;
begin
  Result := NormalizeVector3D(Vector3D(PFrom, PTo));
end;

function Point3DToPoint2D(const P3D: TPoint3D): TPoint2D;
begin
  if (P3D.W = 1.0) or (P3D.W = 0.0) then
   begin
     Result.X := P3D.X;
     Result.Y := P3D.Y;
   end
  else
   begin
     Result.X := P3D.X / P3D.W;
     Result.Y := P3D.Y / P3D.W;
   end;
  Result.W := 1.0;
end;

function Point2DToPoint3D(const P2D: TPoint2D): TPoint3D;
begin
  Result.X := P2D.X;
  Result.Y := P2D.Y;
  Result.Z := 0.0;
  Result.W := P2D.W;
end;

function Rect3DToRect2D(const R: TRect3D): TRect2D;
begin
  Result.FirstEdge := Point3DToPoint2D(R.FirstEdge);
  Result.SecondEdge := Point3DToPoint2D(R.SecondEdge);
end;

function Rect2DToRect3D(const R: TRect2D): TRect3D;
begin
  Result.FirstEdge := Point3D(R.FirstEdge.X, R.FirstEdge.Y, 0);
  Result.SecondEdge := Point3D(R.SecondEdge.X, R.SecondEdge.Y, 0);
end;

function CartesianPoint3D(const P: TPoint3D): TPoint3D;
begin
  if (P.W <> 1.0) and (P.W > 1.0E-8) then
   begin
     Result.X := P.X / P.W;
     Result.Y := P.Y / P.W;
     Result.Z := P.Z / P.W;
     Result.W := 1.0;
   end
  else
   Result := P;
end;

function CartesianRect3D(const R: TRect3D): TRect3D;
begin
  Result.FirstEdge := CartesianPoint3D(R.FirstEdge);
  Result.SecondEdge := CartesianPoint3D(R.SecondEdge);
end;

function ReOrderRect3D(const R: TRect3D): TRect3D;
begin
  Result := CartesianRect3D(R);
  if R.FirstEdge.X > R.SecondEdge.X then
   begin
     Result.FirstEdge.X := R.SecondEdge.X;
     Result.SecondEdge.X := R.FirstEdge.X;
   end;
  if R.FirstEdge.Y > R.SecondEdge.Y then
   begin
     Result.FirstEdge.Y := R.SecondEdge.Y;
     Result.SecondEdge.Y := R.FirstEdge.Y;
   end;
  if R.FirstEdge.Z > R.SecondEdge.Z then
   begin
     Result.FirstEdge.Z := R.SecondEdge.Z;
     Result.SecondEdge.Z := R.FirstEdge.Z;
   end;
end;

function IsSameTransform3D(const T1, T2: TTransf3D): Boolean;
begin
  Result := (T1[1, 1] = T2[1, 1])
            and (T1[1, 2] = T2[1, 2])
            and (T1[1, 3] = T2[1, 3])
            and (T1[1, 4] = T2[1, 4])
            and (T1[2, 1] = T2[2, 1])
            and (T1[2, 2] = T2[2, 2])
            and (T1[2, 3] = T2[2, 3])
            and (T1[2, 4] = T2[2, 4])
            and (T1[3, 1] = T2[3, 1])
            and (T1[3, 2] = T2[3, 2])
            and (T1[3, 3] = T2[3, 3])
            and (T1[3, 4] = T2[3, 4])
            and (T1[4, 1] = T2[4, 1])
            and (T1[4, 2] = T2[4, 2])
            and (T1[4, 3] = T2[4, 3])
            and (T1[4, 4] = T2[4, 4]);
end;

function IsCartesianTransform3D(const T: TTransf3D): Boolean;
begin
  Result := (T[1, 4] = 0.0) and (T[2, 4] = 0.0) and (T[3, 4] = 0.0) and (T[4, 4] = 1.0);
end;

function IsSamePoint3D(P1, P2: TPoint3D): Boolean;
begin
  if( P1.W <> P2.W ) then
   begin
     P1 := CartesianPoint3D(P1);
     P2 := CartesianPoint3D(P2);
   end;
  Result := (P1.X = P2.X) and (P1.Y = P2.Y) and (P1.Z = P2.Z);
end;

function IsSameVector3D(const V1, V2: TVector3D; const Digits: TRoundToRange): Boolean; overload;
begin
  Result := RoundTo(V1.X, Digits) = RoundTo(V2.X, Digits);
  Result := Result and (RoundTo(V1.Y, Digits) = RoundTo(V2.Y, Digits));
  Result := Result and (RoundTo(V1.Z, Digits) = RoundTo(V2.Z, Digits));
end;

function IsSameVector3D(const V1, V2: TVector3D): Boolean; overload;
begin
  Result := V1.X = V2.X;
  Result := Result and (V1.Y = V2.Y);
  Result := Result and (V1.Z = V2.Z);
end;

function EnlargeBoxDelta3D(R: TRect3D; Delta: TRealType): TRect3D;
begin
  R := CartesianRect3D(R);
  Result.FirstEdge.X := R.FirstEdge.X - Delta;
  Result.SecondEdge.X := R.SecondEdge.X + Delta;
  Result.FirstEdge.Y := R.FirstEdge.Y - Delta;
  Result.SecondEdge.Y := R.SecondEdge.Y + Delta;
  Result.FirstEdge.Z := R.FirstEdge.Z - Delta;
  Result.SecondEdge.Z := R.SecondEdge.Z + Delta;
  Result.FirstEdge.W := 1.0;
  Result.SecondEdge.W := 1.0;
end;

function EnlargeBoxPerc3D(R: TRect3D; Perc: TRealType): TRect3D;
var
  Marg: TRealType;
begin
  R := CartesianRect3D(R);
  Marg := Abs(R.FirstEdge.X - R.SecondEdge.X) * Perc;
  Result.FirstEdge.X := R.FirstEdge.X - Marg;
  Result.SecondEdge.X := R.SecondEdge.X + Marg;
  Marg := Abs(R.FirstEdge.Y - R.SecondEdge.Y) * Perc;
  Result.FirstEdge.Y := R.FirstEdge.Y - Marg;
  Result.SecondEdge.Y := R.SecondEdge.Y + Marg;
  Marg := Abs(R.FirstEdge.Z - R.SecondEdge.Z) * Perc;
  Result.FirstEdge.Z := R.FirstEdge.Z - Marg;
  Result.SecondEdge.Z := R.SecondEdge.Z + Marg;
  Result.FirstEdge.W := 1.0;
  Result.SecondEdge.W := 1.0;
end;

function GetVisualTransform2D(var W: TRect2D; const V: TRect;
                              Aspect: TRealType): TTransf2D;
var
  TmpAsp: TRealType;
begin
  {
    | Sx                     0                          0 |
    | 0                      Sy                         0 |
    | V.Left-(Sx*W.Left)+0.5 V.Bottom-(Sy*W.Bottom)+0.5 1 |

    Sx=(V.Right-V.Left-1.0)/(W.Right-W.Left)
    Sy=(V.Top-V.Bottom-1.0)/(W.Top-W.Bottom)
    Se pero' AspectRatio=Sx/Sy > 0 allora Sy=Sx/AspectRatio.
  }
  try
    if Aspect > 0.0 then
     begin
       TmpAsp := (V.Right - V.Left) / (V.Bottom - V.Top) * Aspect;
       if (W.Top - W.Bottom) > (W.Right - W.Left) / TmpAsp then
        W.Right := W.Left + (W.Top - W.Bottom) * TmpAsp
       else
        W.Top := W.Bottom + (W.Right - W.Left) / TmpAsp;
     end;
    Result[1, 1] := (V.Right - V.Left) / (W.Right - W.Left);
    Result[1, 2] := 0.0;
    Result[1, 3] := 0.0;
    Result[2, 1] := 0.0;
    Result[2, 2] := (V.Top - V.Bottom) / (W.Top - W.Bottom);
    Result[2, 3] := 0.0;
    Result[3, 1] := V.Left - Result[1, 1] * W.Left - 0.5;
    Result[3, 2] := V.Bottom - Result[2, 2] * W.Bottom - 0.5;
    Result[3, 3] := 1.0;
  except
   on EZeroDivide do Result := IdentityTransf2D;
  end;
end;

function MultiplyTransform2D(const M1, M2: TTransf2D): TTransf2D;
var
  B1, B2: Boolean;
begin
  B1 := (M1[1, 3] = 0.0) and (M1[2, 3] = 0.0) and (M1[3, 3] = 1.0);
  B2 := (M2[1, 3] = 0.0) and (M2[2, 3] = 0.0) and (M2[3, 3] = 1.0);
  if B1 and B2 then
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2];
     Result[1, 3] := 0.0;

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2];
     Result[2, 3] := 0.0;

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M2[3, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M2[3, 2];
     Result[3, 3] := 1.0;
   end
  else if B1 and (not B2) then
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2];
     Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3];

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2];
     Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3];

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M2[3, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M2[3, 2];
     Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] + M2[3, 3];
   end
  else if (not B1) and B2 then
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] + M1[1, 3] * M2[3, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] + M1[1, 3] * M2[3, 2];
     Result[1, 3] := M1[1, 3];

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] + M1[2, 3] * M2[3, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] + M1[2, 3] * M2[3, 2];
     Result[2, 3] := M1[2, 3];

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M1[3, 3] * M2[3, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M1[3, 3] * M2[3, 2];
     Result[3, 3] := M1[3, 3];
   end
  else
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] + M1[1, 3] * M2[3, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] + M1[1, 3] * M2[3, 2];
     Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3] + M1[1, 3] * M2[3, 3];

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] + M1[2, 3] * M2[3, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] + M1[2, 3] * M2[3, 2];
     Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3] + M1[2, 3] * M2[3, 3];

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M1[3, 3] * M2[3, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M1[3, 3] * M2[3, 2];
     Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] + M1[3, 3] * M2[3, 3];
   end;
end;

{
  a11 a12
  a21 a22
}
function _Det2(a11, a21, a12, a22: TRealType): TRealType;
begin
  Result := a11*a22-a12*a21;
end;

function _Det3_2D(const M: TTransf2D): TRealType;
begin
  Result := M[1, 1] * M[2, 2] * M[3, 3] +
            M[1, 3] * M[2, 1] * M[3, 2] +
            M[1, 2] * M[2, 3] * M[3, 1] -
            M[1, 3] * M[2, 2] * M[3, 1] -
            M[1, 1] * M[3, 2] * M[2, 3] -
            M[1, 2] * M[2, 1] * M[3, 3];
end;

function _Adjoint3(const M: TTransf2D): TTransf2D;
var
  a1,a2,a3,b1,b2,b3,c1,c2,c3: TRealType;
begin
  a1 := M[1,1];
  a2 := M[2,1];
  a3 := M[3,1];
  b1 := M[1,2];
  b2 := M[2,2];
  b3 := M[3,2];
  c1 := M[1,3];
  c2 := M[2,3];
  c3 := M[3,3];

  Result[1, 1] := _Det2(b2,b3,c2,c3);
  Result[2, 1] := -_Det2(a2,a3,c2,c3);
  Result[3, 1] := _Det2(a2,a3,b2,b3);

  Result[1, 2] := -_Det2(b1,b3,c1,c3);
  Result[2, 2] := _Det2(a1,a3,c1,c3);
  Result[3, 2] := -_Det2(a1,a3,b1,b3);

  Result[1, 3] := _Det2(b1,b2,c1,c2);
  Result[2, 3] := -_Det2(a1,a2,c1,c2);
  Result[3, 3] := _Det2(a1,a2,b1,b2);
end;

function InvertTransform2D(const M1: TTransf2D): TTransf2D;
var
  Divisor: TRealType;
begin
  if (M1[1, 3] = 0.0) and (M1[2, 3] = 0.0) and (M1[3, 3] = 1.0) then
   begin // Cartesian
     if (M1[1, 1] * M1[2, 2] - M1[2, 1] * M1[1, 2]) = 0.0 then
      begin
        Result := NullTransf2D;
        Exit;
      end;
     Divisor := 1.0 / (M1[1, 1] * M1[2, 2] - M1[2, 1] * M1[1, 2]);
     Result[1, 1] := M1[2, 2] * Divisor;
     Result[1, 2] := - M1[1, 2] * Divisor;
     Result[1, 3] := 0.0;
     Result[2, 1] := - M1[2, 1] * Divisor;
     Result[2, 2] := M1[1, 1] * Divisor;
     Result[2, 3] := 0.0;
     Result[3, 1] := (M1[2, 1] * M1[3, 2] - M1[2, 2] * M1[3, 1]) * Divisor;
     Result[3, 2] := (M1[1, 2] * M1[3, 1] - M1[1, 1] * M1[3, 2]) * Divisor;
     Result[3, 3] := 1.0;
   end
  else
   begin
     Divisor := _Det3_2D(M1);
     if Divisor = 0 then
      begin
        Result := NullTransf2D;
        Exit;
      end;
     Result := _Adjoint3(M1);

     Result[1, 1] := Result[1, 1] / Divisor;
     Result[1, 2] := Result[1, 2] / Divisor;
     Result[1, 3] := Result[1, 3] / Divisor;
     Result[2, 1] := Result[2, 1] / Divisor;
     Result[2, 2] := Result[2, 2] / Divisor;
     Result[2, 3] := Result[2, 3] / Divisor;
     Result[3, 1] := Result[3, 1] / Divisor;
     Result[3, 2] := Result[3, 2] / Divisor;
     Result[3, 3] := Result[3, 3] / Divisor;
   end;
end;

function TransformPoint2D(const P: TPoint2D; const T: TTransf2D): TPoint2D;
begin
  Result.X := P.X * T[1, 1] + P.Y * T[2, 1] + P.W * T[3, 1];
  Result.Y := P.X * T[1, 2] + P.Y * T[2, 2] + P.W * T[3, 2];
  Result.W := P.X * T[1, 3] + P.Y * T[2, 3] + P.W * T[3, 3];
end;

function TransformVector2D(const V: TVector2D; const T: TTransf2D): TVector2D;
begin
  Result.X := V.X * T[1, 1] + V.Y * T[2, 1];
  Result.Y := V.X * T[1, 2] + V.Y * T[2, 2];
end;

function TransformRect2D(const R: TRect2D; const T: TTransf2D): TRect2D;
begin
  Result.FirstEdge := TransformPoint2D(R.FirstEdge, T);
  Result.SecondEdge := TransformPoint2D(R.SecondEdge, T);
end;

function TransformBoundingBox2D(const Box: TRect2D; const Transf: TTransf2D): TRect2D;
var
  Box1, Box2: TRect2D;
begin
  Box1 := TransformRect2D(Box, Transf);
  Box2 := TransformRect2D(Rect2D(Box.Left, Box.Top, Box.Right, Box.Bottom), Transf);
  Result := BoxOutBox2D(Box1, Box2);
end;

function Translate2D(const Tx, Ty: TRealType): TTransf2D;
begin
  {
    | 1  0  0 |
    | 0  1  0 |
    | Tx Ty 1 |
  }
  Result := IdentityTransf2D;
  Result[3, 1] := Tx;
  Result[3, 2] := Ty;
end;

function Rotate2D(const R: TRealType): TTransf2D;
begin
  {
    | cos(R)  sin(R)   0 |
    | -sin(R) cos(R)   0 |
    | 0       0        1 |
  }
  Result := IdentityTransf2D;
  Result[1, 1] := cos(R);
  Result[1, 2] := sin(R);
  Result[2, 1] := -Result[1, 2];
  Result[2, 2] := Result[1, 1];
end;

function Scale2D(const Sx, Sy: TRealType): TTransf2D;
begin
  {
    | Sx  0  0 |
    | 0   Sy 0 |
    | 0   0  1 |
  }
  Result := IdentityTransf2D;
  Result[1, 1] := Sx;
  Result[2, 2] := Sy;
end;

function IsBoxInBox2D(Box1, Box2: TRect2D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  Box1 := ReorderRect2D(Box1);
  Box2 := ReorderRect2D(Box2);
  FCode := _PositionCode2D(Box2, Box1.FirstEdge);
  SCode := _PositionCode2D(Box2, Box1.SecondEdge);
  Result := (FCode * SCode) = [];
end;

function IsPointInCartesianBox2D(const Pt: TPoint2D; const Box: TRect2D): Boolean;
begin
  Result := _PositionCode2D(Box, Pt) = [];
end;

function IsPointInBox2D(Pt: TPoint2D; Box: TRect2D): Boolean;
begin
  Pt := CartesianPoint2D(Pt);
  Box := CartesianRect2D(Box);
  Result := _PositionCode2D(Box, Pt) = [];
end;

function BoxFillingCartesian2D(const Box1, Box2: TRect2D): Word;
var
  Tmp1, Tmp2: Byte;
begin
  try
    Tmp1 := Round((Box1.Right - Box1.Left) / (Box2.Right - Box2.Left)) * 1000;
    Tmp2 := Round((Box1.Top - Box1.Bottom) / (Box2.Top - Box2.Bottom)) * 1000;
    if Tmp1 > Tmp2 then
     Result := Tmp1
    else
     Result := Tmp2;
  except
    on EZeroDivide do Result := 1000;
  end;
end;

function IsBoxAllInBox2D(Box1, Box2: TRect2D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  Box1 := CartesianRect2D(Box1);
  Box2 := CartesianRect2D(Box2);
  FCode := _PositionCode2D(Box2, Box1.FirstEdge);
  SCode := _PositionCode2D(Box2, Box1.SecondEdge);
  Result := (FCode = []) and (SCode = []);
end;

function IsBoxAllInCartesianBox2D(const Box1, Box2: TRect2D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  FCode := _PositionCode2D(Box2, Box1.FirstEdge);
  SCode := _PositionCode2D(Box2, Box1.SecondEdge);
  Result := (FCode = []) and (SCode = []);
end;

function BoxOutBox2D(Box1, Box2: TRect2D): TRect2D;
begin
  Box1 := ReorderRect2D(Box1);
  Box2 := ReorderRect2D(Box2);
  Result := Box1;
  if Box2.Left < Box1.Left then
   Result.Left := Box2.Left;
  if Box2.Right > Box1.Right then
   Result.Right := Box2.Right;
  if Box2.Bottom < Box1.Bottom then
   Result.Bottom := Box2.Bottom;
  if Box2.Top > Box1.Top then
   Result.Top := Box2.Top;
end;

procedure MakeOrto2D(LastPt: TPoint2D; var CurrPt: TPoint2D);
var
  DeltaPt: TVector2D;
begin
  LastPt := CartesianPoint2D(LastPt);
  CurrPt := CartesianPoint2D(CurrPt);
  DeltaPt := Vector2D(LastPt, CurrPt);
  if Abs(DeltaPt.X) > Abs(DeltaPt.Y) then
   CurrPt.Y := LastPt.Y
  else
   CurrPt.X := LastPt.X;
end;

function DotProduct2D(const A, B: TVector2D): TRealType;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

function Perpendicular2D(const V: TVector2D): TVector2D;
begin
  Result.X := -V.Y;
  Result.Y := V.X;
end;

function Reflect2D(const V: TVector2D): TVector2D;
begin
  Result.X := -V.X;
  Result.Y := -V.Y;
end;

function IsPointOnSegment2D(P, P1, P2: TPoint2D): Boolean;
var
  r, L, LQ, DX, DY: Double;
begin
  P := CartesianPoint2D(P);
  P1 := CartesianPoint2D(P1);
  P2 := CartesianPoint2D(P2);
  Result := False;
  DX := P2.X - P1.X;
  DY := P2.Y - P1.Y;
  L := Sqrt(DX * DX + DY * DY);
  if L = 0 then Exit;
  LQ := L * L;
  r := ((P1.Y - P.Y) * (-DY) - (P1.X - P.X) * DX) / LQ;
  Result := (r >= 0) and (r <= 1);
end;

function PointDistance2D(const P1, P2: TPoint2D): TRealType;
begin
  Result := VectorLength2D(Vector2D(P1, P2));
end;

function PointLineDistance2D(P, P1, P2: TPoint2D): TRealType;
var
  L, DX, DY: Double;
begin
  P := CartesianPoint2D(P);
  P1 := CartesianPoint2D(P1);
  P2 := CartesianPoint2D(P2);
  Result := -1.0;
  DX := P2.X - P1.X;
  DY := P2.Y - P1.Y;
  L := Sqrt(DX * DX + DY * DY);
  if L = 0 then Exit;
  Result := Abs(((P1.Y - P.Y) * DX - (P1.X - P.X) * DY) / L);
end;

{ Dist is valid only if the P projection lies on the line.
  Parametri già omogeneizzati. }
function _PointSegmentDistance2D(const P, P1, P2: TPoint2D; var Dist: TRealType): Boolean;
var
  r, L, LQ, DX, DY: Double;
begin
  Result := False;
  Dist := MaxCoord;
  DX := P2.X - P1.X;
  DY := P2.Y - P1.Y;
  L := Sqrt(DX * DX + DY * DY);
  if L = 0 then Exit;
  LQ := L * L;
  r := ((P1.Y - P.Y) * (-DY) - (P1.X - P.X) * DX) / LQ;
  Result := (r >= 0) and (r <= 1);
  if not Result then Exit;
  Dist := Abs(((P1.Y - P.Y) * DX - (P1.X - P.X) * DY) / L);
end;

function IsPointOnRect2D(const Box: TRect2D; P: TPoint2D; var Dist: TRealType;
                       const Aperture: TRealType; const T: TTransf2D): Integer;
var
  TmpDist: TRealType;
  TmpPt1, TmpPt2: TPoint2D;
begin
  Dist := MaxCoord;
  Result := PICK_NOOBJECT;
  P := CartesianPoint2D(P);
  TmpPt1 := CartesianPoint2D(TransformPoint2D(Box.FirstEdge, T));
  TmpPt2 := CartesianPoint2D(TransformPoint2D(Point2D(Box.Left, Box.Top), T));
  TmpDist := 0;
  if _PointSegmentDistance2D(P, TmpPt1, TmpPt2, TmpDist) and (TmpDist <= Aperture) then
   begin
     Result := PICK_ONOBJECT;
     Dist := TmpDist;
     Exit;
   end;
  TmpPt1 := TmpPt2;
  TmpPt2 := CartesianPoint2D(TransformPoint2D(Box.SecondEdge, T));
  if _PointSegmentDistance2D(P, TmpPt1, TmpPt2, TmpDist) and (TmpDist <= Aperture) then
   begin
     Result := PICK_ONOBJECT;
     Dist := TmpDist;
     Exit;
   end;
  TmpPt1 := TmpPt2;
  TmpPt2 := CartesianPoint2D(TransformPoint2D(Point2D(Box.Right, Box.Bottom), T));
  if _PointSegmentDistance2D(P, TmpPt1, TmpPt2, TmpDist) and (TmpDist <= Aperture) then
   begin
     Result := PICK_ONOBJECT;
     Dist := TmpDist;
     Exit;
   end;
  TmpPt1 := TmpPt2;
  TmpPt2 := CartesianPoint2D(TransformPoint2D(Box.FirstEdge, T));
  if _PointSegmentDistance2D(P, TmpPt1, TmpPt2, TmpDist) and (TmpDist <= Aperture) then
   begin
     Result := PICK_ONOBJECT;
     Dist := TmpDist;
     Exit;
   end;
end;

function PointOutBox2D(Pt: TPoint2D; Box: TRect2D): TRect2D;
begin
  Pt := CartesianPoint2D(Pt);
  Result := Box;
  if Pt.X > Result.Right then
   Result.Right := Pt.X
  else if Pt.X < Result.Left then
   Result.Left := Pt.X;
  if Pt.Y > Result.Top then
   Result.Top := Pt.Y
  else if Pt.Y < Result.Bottom then
   Result.Bottom := Pt.Y;
end;

function NearPoint2D(RP, P: TPoint2D; const Aperture: TRealType; var Dist: TRealType): Boolean;
var
  TmpBox: TRect2D;
begin
  RP := CartesianPoint2D(RP);
  P := CartesianPoint2D(P);
  TmpBox.FirstEdge := Point2D(RP.X - Aperture, RP.Y - Aperture);
  TmpBox.SecondEdge := Point2D(RP.X + Aperture, RP.Y + Aperture);
  Result := _PositionCode2D(TmpBox, P) = [];
  if Result then
   Dist := Sqrt(Power(P.X - RP.X, 2) + Power(P.Y - RP.Y, 2))
  else
   Dist := MaxCoord;
end;

function IsPointOnPolyLine2D(const Vect: Pointer; Count: Integer;
                             P: TPoint2D; var Dist: TRealType;
                             const Aperture: TRealType; const T: TTransf2D; const MustClose: Boolean): Integer;
var
  MinDist, TmpDist: TRealType;
  TmpPt1, TmpPt2: TPoint2D;
  Cont, Max: Integer;
begin
  Result := PICK_NOOBJECT;
  Dist := MaxCoord;
  if Count = 0 then
   Exit;
  MinDist := Aperture;
  P := CartesianPoint2D(P);
  if MustClose then
   Max := Count
  else
   Max := Count - 1;
  TmpPt1 := CartesianPoint2D(TransformPoint2D(PVectPoints2D(Vect)^[0], T));
  for Cont := 1 to Max do
   begin
     if Cont = Count then
      TmpPt2 := CartesianPoint2D(TransformPoint2D(PVectPoints2D(Vect)^[0], T))
     else
      TmpPt2 := CartesianPoint2D(TransformPoint2D(PVectPoints2D(Vect)^[Cont], T));
     TmpDist := 0;
     if _PointSegmentDistance2D(P, TmpPt1, TmpPt2, TmpDist) and (TmpDist <= MinDist) then
      begin
        Result := PICK_ONOBJECT;
        Dist := TmpDist;
        MinDist := Dist;
      end;
     TmpPt1 := TmpPt2;
   end;
end;

function IsPointOnLine2D(A, B, P: TPoint2D; var Dist: TRealType;
                       const Aperture: TRealType; const T: TTransf2D): Integer;
var
  TmpDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  Dist := MaxCoord;
  P := CartesianPoint2D(P);
  A := CartesianPoint2D(TransformPoint2D(A, T));
  B := CartesianPoint2D(TransformPoint2D(B, T));
  TmpDist := 0;
  if _PointSegmentDistance2D(P, A, B, TmpDist) and (TmpDist <= Aperture) then
   begin
     Result := PICK_ONOBJECT;
     Dist := TmpDist;
   end;
end;

function _RightIntersection2D(const P, P1, P2: TPoint2D): Boolean;
var
  R: Double;
begin
  Result := ((P.Y >= P1.Y) and (P.Y < P2.Y)) or ((P.Y < P1.Y) and (P.Y >= P2.Y));
  if not Result then Exit;
  R := (P.Y - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y) + P1.X;
  Result := P.X <= R;
end;

function IsPointInPolygon2D(const Vect: Pointer; Count: Integer;
                            P: TPoint2D; var Dist: TRealType;
                            const Aperture: TRealType; const T: TTransf2D): Integer;
var
  Cont, NInters: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  Dist := Aperture;
  if Count = 0 then
   Exit;
  NInters := 0;
  P := CartesianPoint2D(P);
  TmpPt1 := CartesianPoint2D(TransformPoint2D(PVectPoints2D(Vect)^[0], T));
  for Cont := 1 to Count do
   begin
     if Cont = Count then
      TmpPt2 := CartesianPoint2D(TransformPoint2D(PVectPoints2D(Vect)^[0], T))
     else
      TmpPt2 := CartesianPoint2D(TransformPoint2D(PVectPoints2D(Vect)^[Cont], T));
     TmpDist := 0;
     if _PointSegmentDistance2D(P, TmpPt1, TmpPt2, TmpDist) and (TmpDist <= Dist) then
      begin
        if TmpDist < Aperture then
         Result := PICK_ONOBJECT;
        Dist := TmpDist;
      end
     else if _RightIntersection2D(P, TmpPt1, TmpPt2) then
      Inc(NInters);
     TmpPt1 := TmpPt2;
   end;
  if Odd(NInters) and (Result = PICK_NOOBJECT) then
   begin
     Result := PICK_INOBJECT;
     Dist := Aperture;
   end
  else if (Result = PICK_NOOBJECT) then
   Dist := MaxCoord;
end;

{ 2D clipping functions. }

{ Use the Liang-Barsky algoritm. }
function ClipLine2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
var
  DX, DY, tE, tL: Extended;
begin
  Result := [ccNotVisible];
  Clip := ReorderRect2D(Clip);
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip) then
   begin
     Result := [ccVisible];
     Exit;
   end;
  tE := 0.0;
  tL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, tL) then
   if _ClipPt(-DX, Pt1.X - Clip.Right, tE, tL) then
    if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, tL) then
     if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, tL) then
      begin
        Result := [];
        if tL < 1 then
         begin
           Pt2.X := Pt1.X + tL * DX;
           Pt2.Y := Pt1.Y + tL * DY;
           Result := [ccSecond];
         end;
        if tE > 0 then
         begin
           Pt1.X := Pt1.X + tE * DX;
           Pt1.Y := Pt1.Y + tE * DY;
           Result := Result + [ccFirst];
         end;
        if Result = [] then
         Result := [ccVisible];
      end;
end;

{ Use the Liang-Barsky algoritm. }
function ClipLineLeftRight2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
var
  DX, DY, tE, tL: Extended;
begin
  Clip := ReorderRect2D(Clip);
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip) then
   begin
     Result := [ccVisible];
     Exit;
   end;
  tE := 0.0;
  tL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DX, Clip.Left - Pt1.X, tE, tL) then
   if _ClipPt(-DX, Pt1.X - Clip.Right, tE, tL) then
    begin
      Result := [];
      if tL < 1 then
       begin
         Pt2.X := Pt1.X + tL * DX;
         Pt2.Y := Pt1.Y + tL * DY;
         Result := [ccSecond];
       end;
      if tE > 0 then
       begin
         Pt1.X := Pt1.X + tE * DX;
         Pt1.Y := Pt1.Y + tE * DY;
         Result := Result + [ccFirst];
       end;
      if Result = [] then
       Result := [ccVisible];
    end;
end;

function ClipLineUpBottom2D(Clip: TRect2D; var Pt1, Pt2: TPoint2D): TClipResult;
var
  DX, DY, tE, tL: Extended;
begin
  Clip := ReorderRect2D(Clip);
  Pt1 := CartesianPoint2D(Pt1);
  Pt2 := CartesianPoint2D(Pt2);
  DX := Pt2.X - Pt1.X;
  DY := Pt2.Y - Pt1.Y;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInCartesianBox2D(Pt1, Clip) then
   begin
     Result := [ccVisible];
     Exit;
   end;
  tE := 0.0;
  tL := 1.0;
  { 0.9 in 1. }
  if _ClipPt(DY, Clip.Bottom - Pt1.Y, tE, tL) then
   if _ClipPt(-DY, Pt1.Y - Clip.Top, tE, tL) then
    begin
      Result := [];
      if tL < 1 then
       begin
         Pt2.X := Pt1.X + tL * DX;
         Pt2.Y := Pt1.Y + tL * DY;
         Result := [ccSecond];
       end;
      if tE > 0 then
       begin
         Pt1.X := Pt1.X + tE * DX;
         Pt1.Y := Pt1.Y + tE * DY;
         Result := Result + [ccFirst];
       end;
      if Result = [] then
       Result := [ccVisible];
    end;
end;

function MultiplyTransform3D(const M1, M2: TTransf3D): TTransf3D;
var
  B1, B2: Boolean;
begin
  B1 := (M1[1, 4] = 0.0) and (M1[2, 4] = 0.0) and (M1[3, 4] = 0.0) and (M1[4, 4] = 1.0);
  B2 := (M2[1, 4] = 0.0) and (M2[2, 4] = 0.0) and (M2[3, 4] = 0.0) and (M2[4, 4] = 1.0);
  if B1 and B2 then
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] + M1[1, 3] * M2[3, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] + M1[1, 3] * M2[3, 2];
     Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3] + M1[1, 3] * M2[3, 3];
     Result[1, 4] := 0.0;

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] + M1[2, 3] * M2[3, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] + M1[2, 3] * M2[3, 2];
     Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3] + M1[2, 3] * M2[3, 3];
     Result[2, 4] := 0.0;

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M1[3, 3] * M2[3, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M1[3, 3] * M2[3, 2];
     Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] + M1[3, 3] * M2[3, 3];
     Result[3, 4] := 0.0;

     Result[4, 1] := M1[4, 1] * M2[1, 1] + M1[4, 2] * M2[2, 1] + M1[4, 3] * M2[3, 1] + M2[4, 1];
     Result[4, 2] := M1[4, 1] * M2[1, 2] + M1[4, 2] * M2[2, 2] + M1[4, 3] * M2[3, 2] + M2[4, 2];
     Result[4, 3] := M1[4, 1] * M2[1, 3] + M1[4, 2] * M2[2, 3] + M1[4, 3] * M2[3, 3] + M2[4, 3];
     Result[4, 4] := 1.0;
   end
  else if B1 and (not B2) then
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] + M1[1, 3] * M2[3, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] + M1[1, 3] * M2[3, 2];
     Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3] + M1[1, 3] * M2[3, 3];
     Result[1, 4] := M1[1, 1] * M2[1, 4] + M1[1, 2] * M2[2, 4] + M1[1, 3] * M2[3, 4];

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] + M1[2, 3] * M2[3, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] + M1[2, 3] * M2[3, 2];
     Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3] + M1[2, 3] * M2[3, 3];
     Result[2, 4] := M1[2, 1] * M2[1, 4] + M1[2, 2] * M2[2, 4] + M1[2, 3] * M2[3, 4];

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M1[3, 3] * M2[3, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M1[3, 3] * M2[3, 2];
     Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] + M1[3, 3] * M2[3, 3];
     Result[3, 4] := M1[3, 1] * M2[1, 4] + M1[3, 2] * M2[2, 4] + M1[3, 3] * M2[3, 4];

     Result[4, 1] := M1[4, 1] * M2[1, 1] + M1[4, 2] * M2[2, 1] + M1[4, 3] * M2[3, 1] + M2[4, 1];
     Result[4, 2] := M1[4, 1] * M2[1, 2] + M1[4, 2] * M2[2, 2] + M1[4, 3] * M2[3, 2] + M2[4, 2];
     Result[4, 3] := M1[4, 1] * M2[1, 3] + M1[4, 2] * M2[2, 3] + M1[4, 3] * M2[3, 3] + M2[4, 3];
     Result[4, 4] := M1[4, 1] * M2[1, 4] + M1[4, 2] * M2[2, 4] + M1[4, 3] * M2[3, 4] + M2[4, 4];
   end
  else if (not B1) and B2 then
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] + M1[1, 3] * M2[3, 1] + M1[1, 4] * M2[4, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] + M1[1, 3] * M2[3, 2] + M1[1, 4] * M2[4, 2];
     Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3] + M1[1, 3] * M2[3, 3] + M1[1, 4] * M2[4, 3];
     Result[1, 4] := M1[1, 4];

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] + M1[2, 3] * M2[3, 1] + M1[2, 4] * M2[4, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] + M1[2, 3] * M2[3, 2] + M1[2, 4] * M2[4, 2];
     Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3] + M1[2, 3] * M2[3, 3] + M1[2, 4] * M2[4, 3];
     Result[2, 4] := M1[2, 4];

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M1[3, 3] * M2[3, 1] + M1[3, 4] * M2[4, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M1[3, 3] * M2[3, 2] + M1[3, 4] * M2[4, 2];
     Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] + M1[3, 3] * M2[3, 3] + M1[3, 4] * M2[4, 3];
     Result[3, 4] := M1[3, 4];

     Result[4, 1] := M1[4, 1] * M2[1, 1] + M1[4, 2] * M2[2, 1] + M1[4, 3] * M2[3, 1] + M1[4, 4] * M2[4, 1];
     Result[4, 2] := M1[4, 1] * M2[1, 2] + M1[4, 2] * M2[2, 2] + M1[4, 3] * M2[3, 2] + M1[4, 4] * M2[4, 2];
     Result[4, 3] := M1[4, 1] * M2[1, 3] + M1[4, 2] * M2[2, 3] + M1[4, 3] * M2[3, 3] + M1[4, 4] * M2[4, 3];
     Result[4, 4] := M1[4, 4];
   end
  else
   begin
     Result[1, 1] := M1[1, 1] * M2[1, 1] + M1[1, 2] * M2[2, 1] + M1[1, 3] * M2[3, 1] + M1[1, 4] * M2[4, 1];
     Result[1, 2] := M1[1, 1] * M2[1, 2] + M1[1, 2] * M2[2, 2] + M1[1, 3] * M2[3, 2] + M1[1, 4] * M2[4, 2];
     Result[1, 3] := M1[1, 1] * M2[1, 3] + M1[1, 2] * M2[2, 3] + M1[1, 3] * M2[3, 3] + M1[1, 4] * M2[4, 3];
     Result[1, 4] := M1[1, 1] * M2[1, 4] + M1[1, 2] * M2[2, 4] + M1[1, 3] * M2[3, 4] + M1[1, 4] * M2[4, 4];

     Result[2, 1] := M1[2, 1] * M2[1, 1] + M1[2, 2] * M2[2, 1] + M1[2, 3] * M2[3, 1] + M1[2, 4] * M2[4, 1];
     Result[2, 2] := M1[2, 1] * M2[1, 2] + M1[2, 2] * M2[2, 2] + M1[2, 3] * M2[3, 2] + M1[2, 4] * M2[4, 2];
     Result[2, 3] := M1[2, 1] * M2[1, 3] + M1[2, 2] * M2[2, 3] + M1[2, 3] * M2[3, 3] + M1[2, 4] * M2[4, 3];
     Result[2, 4] := M1[2, 1] * M2[1, 4] + M1[2, 2] * M2[2, 4] + M1[2, 3] * M2[3, 4] + M1[2, 4] * M2[4, 4];

     Result[3, 1] := M1[3, 1] * M2[1, 1] + M1[3, 2] * M2[2, 1] + M1[3, 3] * M2[3, 1] + M1[3, 4] * M2[4, 1];
     Result[3, 2] := M1[3, 1] * M2[1, 2] + M1[3, 2] * M2[2, 2] + M1[3, 3] * M2[3, 2] + M1[3, 4] * M2[4, 2];
     Result[3, 3] := M1[3, 1] * M2[1, 3] + M1[3, 2] * M2[2, 3] + M1[3, 3] * M2[3, 3] + M1[3, 4] * M2[4, 3];
     Result[3, 4] := M1[3, 1] * M2[1, 4] + M1[3, 2] * M2[2, 4] + M1[3, 3] * M2[3, 4] + M1[3, 4] * M2[4, 4];

     Result[4, 1] := M1[4, 1] * M2[1, 1] + M1[4, 2] * M2[2, 1] + M1[4, 3] * M2[3, 1] + M1[4, 4] * M2[4, 1];
     Result[4, 2] := M1[4, 1] * M2[1, 2] + M1[4, 2] * M2[2, 2] + M1[4, 3] * M2[3, 2] + M1[4, 4] * M2[4, 2];
     Result[4, 3] := M1[4, 1] * M2[1, 3] + M1[4, 2] * M2[2, 3] + M1[4, 3] * M2[3, 3] + M1[4, 4] * M2[4, 3];
     Result[4, 4] := M1[4, 1] * M2[1, 4] + M1[4, 2] * M2[2, 4] + M1[4, 3] * M2[3, 4] + M1[4, 4] * M2[4, 4];
   end
end;

function _Det3_3D(a11, a21, a31, a12, a22, a32, a13, a23, a33: TRealType): TRealType;
begin
  Result := a11*a22*a33+a13*a21*a32+a12*a23*a31-
            a13*a22*a31-a11*a32*a23-a12*a21*a33;
end;

function _Det4(const M: TTransf3D): TRealType;
var
  a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3: TRealType;
begin
  a1 := M[1,1];
  a2 := M[2,1];
  a3 := M[3,1];
  b1 := M[1,2];
  b2 := M[2,2];
  b3 := M[3,2];
  c1 := M[1,3];
  c2 := M[2,3];
  c3 := M[3,3];
  d1 := M[1,4];
  d2 := M[2,4];
  d3 := M[3,4];
  Result := -M[4, 1] * _Det3_3D(b1,b2,b3,c1,c2,c3,d1,d2,d3);
  Result := Result+M[4, 2] * _Det3_3D(a1,a2,a3,c1,c2,c3,d1,d2,d3);
  Result := Result-M[4, 3] * _Det3_3D(a1,a2,a3,b1,b2,b3,d1,d2,d3);
  Result := Result+M[4, 4] * _Det3_3D(a1,a2,a3,b1,b2,b3,c1,c2,c3);
end;

function _Adjoint4(const M: TTransf3D): TTransf3D;
var
  a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4: TRealType;
begin
  a1 := M[1,1];
  a2 := M[2,1];
  a3 := M[3,1];
  a4 := M[4,1];
  b1 := M[1,2];
  b2 := M[2,2];
  b3 := M[3,2];
  b4 := M[4,2];
  c1 := M[1,3];
  c2 := M[2,3];
  c3 := M[3,3];
  c4 := M[4,3];
  d1 := M[1,4];
  d2 := M[2,4];
  d3 := M[3,4];
  d4 := M[4,4];

  Result[1, 1] := _Det3_3D(b2,b3,b4,c2,c3,c4,d2,d3,d4);
  Result[2, 1] := -_Det3_3D(a2,a3,a4,c2,c3,c4,d2,d3,d4);
  Result[3, 1] := _Det3_3D(a2,a3,a4,b2,b3,b4,d2,d3,d4);
  Result[4, 1] := -_Det3_3D(a2,a3,a4,b2,b3,b4,c2,c3,c4);

  Result[1, 2] := -_Det3_3D(b1,b3,b4,c1,c3,c4,d1,d3,d4);
  Result[2, 2] := _Det3_3D(a1,a3,a4,c1,c3,c4,d1,d3,d4);
  Result[3, 2] := -_Det3_3D(a1,a3,a4,b1,b3,b4,d1,d3,d4);
  Result[4, 2] := _Det3_3D(a1,a3,a4,b1,b3,b4,c1,c3,c4);

  Result[1, 3] := _Det3_3D(b1,b2,b4,c1,c2,c4,d1,d2,d4);
  Result[2, 3] := -_Det3_3D(a1,a2,a4,c1,c2,c4,d1,d2,d4);
  Result[3, 3] := _Det3_3D(a1,a2,a4,b1,b2,b4,d1,d2,d4);
  Result[4, 3] := -_Det3_3D(a1,a2,a4,b1,b2,b4,c1,c2,c4);

  Result[1, 4] := -_Det3_3D(b1,b2,b3,c1,c2,c3,d1,d2,d3);
  Result[2, 4] := _Det3_3D(a1,a2,a3,c1,c2,c3,d1,d2,d3);
  Result[3, 4] := -_Det3_3D(a1,a2,a3,b1,b2,b3,d1,d2,d3);
  Result[4, 4] := _Det3_3D(a1,a2,a3,b1,b2,b3,c1,c2,c3);
end;

function InvertTransform3D(const M1: TTransf3D): TTransf3D;
var
  Divisor: TRealType;
begin
  Divisor := _Det4(M1);
  if Divisor = 0.0 then
   begin
     Result := NullTransf3D;
     Exit;
   end;
  Result := _Adjoint4(M1);
  if Divisor = 1.0 then
   begin
     Result := NullTransf3D;
     Exit;
   end;
  Result[1, 1] := Result[1, 1] / Divisor;
  Result[1, 2] := Result[1, 2] / Divisor;
  Result[1, 3] := Result[1, 3] / Divisor;
  Result[1, 4] := Result[1, 4] / Divisor;
  Result[2, 1] := Result[2, 1] / Divisor;
  Result[2, 2] := Result[2, 2] / Divisor;
  Result[2, 3] := Result[2, 3] / Divisor;
  Result[2, 4] := Result[2, 4] / Divisor;
  Result[3, 1] := Result[3, 1] / Divisor;
  Result[3, 2] := Result[3, 2] / Divisor;
  Result[3, 3] := Result[3, 3] / Divisor;
  Result[3, 4] := Result[3, 4] / Divisor;
  Result[4, 1] := Result[4, 1] / Divisor;
  Result[4, 2] := Result[4, 2] / Divisor;
  Result[4, 3] := Result[4, 3] / Divisor;
  Result[4, 4] := Result[4, 4] / Divisor;
end;

function TransformPoint3D(const P: TPoint3D; const T: TTransf3D): TPoint3D;
begin
  if (T[1, 4] = 0.0) and (T[2, 4] = 0.0) and (T[3, 4] = 0.0) and (T[4, 4] = 1.0) then
   begin
     Result.X := P.X * T[1, 1] + P.Y * T[2, 1] + P.Z * T[3, 1] + P.W * T[4, 1];
     Result.Y := P.X * T[1, 2] + P.Y * T[2, 2] + P.Z * T[3, 2] + P.W * T[4, 2];
     Result.Z := P.X * T[1, 3] + P.Y * T[2, 3] + P.Z * T[3, 3] + P.W * T[4, 3];
     Result.W := P.W;
   end
  else
   begin
     Result.X := P.X * T[1, 1] + P.Y * T[2, 1] + P.Z * T[3, 1] + P.W * T[4, 1];
     Result.Y := P.X * T[1, 2] + P.Y * T[2, 2] + P.Z * T[3, 2] + P.W * T[4, 2];
     Result.Z := P.X * T[1, 3] + P.Y * T[2, 3] + P.Z * T[3, 3] + P.W * T[4, 3];
     Result.W := P.X * T[1, 4] + P.Y * T[2, 4] + P.Z * T[3, 4] + P.W * T[4, 4];
   end;
end;

function TransformVector3D(const V: TVector3D; const T: TTransf3D): TVector3D;
begin
  Result.X := V.X * T[1, 1] + V.Y * T[2, 1] + V.Z * T[3, 1];
  Result.Y := V.X * T[1, 2] + V.Y * T[2, 2] + V.Z * T[3, 2];
  Result.Z := V.X * T[1, 3] + V.Y * T[2, 3] + V.Z * T[3, 3];
end;

function TransposeMatrix3D(const M: TTransf3D): TTransf3D;
begin
  Result := M;
  Result[2, 1] := M[1, 2];
  Result[3, 1] := M[1, 3];
  Result[4, 1] := M[1, 4];

  Result[1, 2] := M[2, 1];
  Result[3, 2] := M[2, 3];
  Result[4, 2] := M[2, 4];

  Result[1, 3] := M[3, 1];
  Result[2, 3] := M[3, 2];
  Result[4, 3] := M[3, 4];

  Result[1, 4] := M[4, 1];
  Result[2, 4] := M[4, 2];
  Result[3, 4] := M[4, 3];
end;

function TransformNormalVector3D(const N: TVector3D; const T: TTransf3D): TVector3D;
var
  TmpMat: TTransf3D;
begin
  TmpMat := InvertTransform3D(T);
  TmpMat := TransposeMatrix3D(TmpMat);
  Result := TransformVector3D(N, TmpMat);
end;

function TransformRect3D(const R: TRect3D; const T: TTransf3D): TRect3D;
begin
  Result.FirstEdge := TransformPoint3D(R.FirstEdge, T);
  Result.SecondEdge := TransformPoint3D(R.SecondEdge, T);
end;

function Translate3D(const Tx, Ty, Tz: TRealType): TTransf3D;
begin
  Result := IdentityTransf3D;
  Result[4, 1] := Tx;
  Result[4, 2] := Ty;
  Result[4, 3] := Tz;
end;

function Rotate3DX(const R: TRealType): TTransf3D;
begin
  Result := IdentityTransf3D;
  Result[2, 2] := cos(R);
  Result[2, 3] := sin(R);
  Result[3, 2] := -sin(R);
  Result[3, 3] := cos(R);
end;

function Rotate3DY(const R: TRealType): TTransf3D;
begin
  Result := IdentityTransf3D;
  Result[1, 1] := cos(R);
  Result[1, 3] := -sin(R);
  Result[3, 1] := sin(R);
  Result[3, 3] := cos(R);
end;

function Rotate3DZ(const R: TRealType): TTransf3D;
begin
  Result := IdentityTransf3D;
  Result[1, 1] := cos(R);
  Result[1, 2] := sin(R);
  Result[2, 1] := -sin(R);
  Result[2, 2] := cos(R);
end;

// From Graphics FAQ :)
function RotateOnAxis3D(const Center: TPoint3D; const Axis: TVector3D; const Angle: TRealType): TTransf3D;
var
  Quaternion: TPoint3D;
  XX, ZZ, XY, WZ, XZ, WY, YZ, WX, YY: TRealType;
  CosHalfAngle, SinHalfAngle: TRealType;
begin
  CosHalfAngle := Cos(Angle / 2.0);
  SinHalfAngle := Sin(Angle / 2.0);

  Quaternion.X := Axis.X * SinHalfAngle;
  Quaternion.Y := Axis.Y * SinHalfAngle;
  Quaternion.Z := Axis.Z * SinHalfAngle;
  Quaternion.W := CosHalfAngle;

  // Uso un punto 2D per risparmiare tre variabili.
  XX := Quaternion.X * Quaternion.X;
  YY := Quaternion.Y * Quaternion.Y;
  ZZ := Quaternion.Z * Quaternion.Z;

  WX := Quaternion.W * Quaternion.X;
  WZ := Quaternion.W * Quaternion.Z;
  WY := Quaternion.W * Quaternion.Y;

  XY := Quaternion.X * Quaternion.Y;
  XZ := Quaternion.X * Quaternion.Z;
  YZ := Quaternion.Y * Quaternion.Z;

  Result := IdentityTransf3D;

  Result[1, 1] := 1.0 - 2.0 * YY - 2.0 * ZZ;
  Result[2, 1] := 2.0 * XY - 2.0 * WZ;
  Result[3, 1] := 2.0 * XZ + 2.0 * WY;

  Result[1, 2] := 2.0 * XY + 2.0 * WZ;
  Result[2, 2] := 1.0 - 2.0 * XX - 2.0 * ZZ;
  Result[3, 2] := 2.0 * YZ - 2.0 * WX;

  Result[1, 3] := 2.0 * XZ - 2.0 * WY;
  Result[2, 3] := 2.0 * YZ + 2.0 * WX;
  Result[3, 3] := 1.0 - 2.0 * XX - 2.0 * YY;

  Result[4, 1] := Center.X - Result[1, 1] * Center.X - Result[2, 1] * Center.Y - Result[3, 1] * Center.Z;
  Result[4, 2] := Center.Y - Result[1, 2] * Center.X - Result[2, 2] * Center.Y - Result[3, 2] * Center.Z;
  Result[4, 3] := Center.Z - Result[1, 3] * Center.X - Result[2, 3] * Center.Y - Result[3, 3] * Center.Z;
end;

function Scale3D(const Sx, Sy, Sz: TRealType): TTransf3D;
begin
  Result := IdentityTransf3D;
  Result[1, 1] := Sx;
  Result[2, 2] := Sy;
  Result[3, 3] := Sz;
end;

{
  The 3D world is projected on a plane called the view-plane. Is
  also defined a view-volume, that is a volume that bound the object
  projected on the view-plane.

  VRP is the view-reference-point, that is a point on the view plane.
      It can be any point in the 3D space.
  VPN is the view-plane-normal, that is a versor (direction) that
      orients the view-plane.
  VUP is the up vector (see Window).
  Window is a viewport on the view-plane. It's also define the view-volume.
         The window is defined on the view-plane coordinates. The origin
         of this coordinates is VRP.
         VPN is the z axis of the view-plane coords. VUP is the y axis.
         The x axis is found considering a right-handed system.
  PRP is the center of projection in the case of perspective projection.
  F is the front clipping plane that is the first plane encoutered along
    VPN. If it is referred to VRP. If it is positive the the objects
    behind the observer are projected.
  B is the back clipping plane. A large value ensure that all the object
    in front of the observer are displaied.

  At the moment only non-oblique projection are considered.
  Guardo nel verso negativo dell'asse z.
}

function ViewOrientationTransform3D(const VRP: TPoint3D; const VPN, VUP: TVector3D): TTransf3D;
var
  RX, RY, RZ: TVector3D;
  TmpUP: TVector3D;
  TmpReal: TRealType;
  R: TTransf3D;
begin
  // Make UP normal to VPN.
  TmpReal := DotProduct3D(VUP, VPN);
  if Abs(TmpReal) = 1.0 then
   Exit;
  TmpUP.X := VUP.X - VPN.X * TmpReal;
  TmpUP.Y := VUP.Y - VPN.Y * TmpReal;
  TmpUP.Z := VUP.Z - VPN.Z * TmpReal;
  TmpUP := NormalizeVector3D(TmpUP);
  RZ := VPN;
  RX := CrossProd3D(TmpUP,RZ);
  RY := CrossProd3D(RZ, RX);
  R := IdentityTransf3D;
  R := ColumnVector3D(R, 1, RX);
  R := ColumnVector3D(R, 2, RY);
  R := ColumnVector3D(R, 3, RZ);

  { translate in VRP and rotate (VPN == z). }
  Result := MultiplyTransform3D(Translate3D(-VRP.X, -VRP.Y, -VRP.Z), R);
end;

{ F, B entrambi positivi se nella direzione di vista. }
function ParallelViewNormalization3D(ViewWindow: TRect2D; const F, B: TRealType): TTransf3D;
var
  T, S: TTransf3D;
begin
  T := Translate3D(-(ViewWindow.Right + ViewWindow.Left) / 2.0, -(ViewWindow.Top + ViewWindow.Bottom) / 2.0, F);
  S := Scale3D(2 / (ViewWindow.Right - ViewWindow.Left), 2 / (ViewWindow.Top - ViewWindow.Bottom), 1 / (B - F));
  Result := MultiplyTransform3D(T, S);
end;

function PerspectiveViewNormalization3D(ViewWindow: TRect2D; const PlaneDistance, F, B: TRealType): TTransf3D;
var
  SH, S, M: TTransf3D;
  TmpFactor, VRPZ: Extended;
begin
  if (PlaneDistance = 0) or (PlaneDistance = B) then
   Exit;
  { translate in PRP. }
  VRPZ := -PlaneDistance;
  Result := Translate3D(0, 0, VRPZ);

  { Appy the shear. }
  SH := IdentityTransf3D;
  SH[3, 1] := -(ViewWindow.Right + ViewWindow.Left) / (2.0 * VRPZ);
  SH[3, 2] := -(ViewWindow.Top + ViewWindow.Bottom) / (2.0 * VRPZ);
  Result := MultiplyTransform3D(Result, SH);

  TmpFactor := 2 * VRPZ / (VRPZ - B);
  S := IdentityTransf3D;
  S[1, 1] := TmpFactor / (ViewWindow.Right - ViewWindow.Left);
  S[2, 2] := TmpFactor / (ViewWindow.Top - ViewWindow.Bottom);
  S[3, 3] := -1 / (VRPZ - B);
  Result := MultiplyTransform3D(Result, S);

  M := IdentityTransf3D;
  TmpFactor := -(VRPZ - F) / (VRPZ - B);
  M[4, 4] := 0;
  M[3, 4] := -1;
  M[3, 3] := 1 / (1 + TmpFactor);
  M[4, 3] := -TmpFactor / (1 + TmpFactor);
  Result := MultiplyTransform3D(Result, M);
end;

function _PositionCodeCartesian3D(Clip: TRect3D; P: TPoint3D): TOutCode;
begin
  Result := [];
  P := CartesianPoint3D(P);
  if P.X < Clip.FirstEdge.X then
   Result := [left]
  else if P.X > Clip.SecondEdge.X then
   Result := [right];
  if P.Y < Clip.FirstEdge.Y then
   Result := Result + [bottom]
  else if P.Y > Clip.SecondEdge.Y then
   Result := Result + [top];
  if P.Z < Clip.FirstEdge.Z then
   Result := Result + [fareye]
  else if P.Z > Clip.SecondEdge.Z then
   Result := Result + [neareye];
end;

function _PositionCode3D(Clip: TRect3D; P: TPoint3D): TOutCode;
begin
  Result := [];
  P := CartesianPoint3D(P);
  Clip := ReorderRect3D(Clip);
  if P.X < Clip.FirstEdge.X then
   Result := [left]
  else if P.X > Clip.SecondEdge.X then
   Result := [right];
  if P.Y < Clip.FirstEdge.Y then
   Result := Result + [bottom]
  else if P.Y > Clip.SecondEdge.Y then
   Result := Result + [top];
  if P.Z < Clip.FirstEdge.Z then
   Result := Result + [fareye]
  else if P.Z > Clip.SecondEdge.Z then // To be tested !! Corretto il 8/10/99 togliendo l'=. 
   Result := Result + [neareye];
end;

function _PositionCodeNRC3D(const P: TPoint3D): TOutCode;
begin
  Result := [];
  if P.W < 0.0 then
   begin
     if P.X > -P.W then
      Result := [left]
     else if P.X < P.W then
      Result := [right];
     if P.Y > -P.W then
      Result := Result + [bottom]
     else if P.Y < P.W then
      Result := Result + [top];
     if P.Z > -P.W then
      Result := Result + [fareye]
     else if P.Z <= 0.0 then
      Result := Result + [neareye]
   end
  else
   begin
     if P.X < -P.W then
      Result := [left]
     else if P.X > P.W then
      Result := [right];
     if P.Y < -P.W then
      Result := Result + [bottom]
     else if P.Y > P.W then
      Result := Result + [top];
     if P.Z < -P.W then
      Result := Result + [fareye]
     else if P.Z >= 0.0 then
      Result := Result + [neareye]
   end;
end;

function IsBoxInBox3D(Box1, Box2: TRect3D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  Box2 := ReOrderRect3D(Box2);
  FCode := _PositionCodeCartesian3D(Box2, Box1.FirstEdge);
  SCode := _PositionCodeCartesian3D(Box2, Box1.SecondEdge);
  Result := (FCode * SCode) = [];
end;

function IsPointInBox3D(const Pt: TPoint3D; const Box: TRect3D): Boolean;
begin
  Result := _PositionCode3D(Box, Pt) = [];
end;

function IsPointInBoxNRC3D(const Pt: TPoint3D): Boolean;
begin
  Result := _PositionCodeNRC3D(Pt) = [];
end;

function IsVisibleBoxNRC3D(Box: TRect3D): Boolean;
begin
  Result := _PositionCodeNRC3D(Box.FirstEdge) * _PositionCodeNRC3D(Box.SecondEdge) = [];
  Result := Result or
    (_PositionCodeNRC3D(Point3D(Box.Left, Box.Top, Box.Front)) * _PositionCodeNRC3D(Point3D(Box.Right, Box.Bottom, Box.Back)) = []);
end;

function BoxFilling3D(Box1, Box2: TRect3D): Word;
var
  Tmp1, Tmp2, Tmp3: Byte;
begin
  try
    Box1 := ReOrderRect3D(Box1);
    Box2 := ReOrderRect3D(Box2);
    Tmp1 := Round((Box1.SecondEdge.X - Box1.FirstEdge.X) / (Box2.SecondEdge.X - Box2.FirstEdge.X) * 1000);
    Tmp2 := Round((Box1.SecondEdge.Y - Box1.FirstEdge.Y) / (Box2.SecondEdge.Y - Box2.FirstEdge.Y) * 1000);
    Tmp3 := Round((Box1.SecondEdge.Z - Box1.FirstEdge.Z) / (Box2.SecondEdge.Z - Box2.FirstEdge.Z) * 1000);
    if Tmp1 > Tmp2 then
     Result := Tmp1
    else if Tmp2 > Tmp3 then
     Result := Tmp2
    else
     Result := Tmp3;
  except
    on Exception do Result := 1000;
  end;
end;

function BoxFillingNRC3D(Box: TRect3D): Word;
var
  Tmp1, Tmp2, Tmp3: Byte;
begin
  try
    Box := ReorderRect3D(Box);
    Tmp1 := Round((Box.SecondEdge.X - Box.FirstEdge.X) * 500);
    Tmp2 := Round((Box.SecondEdge.Y - Box.FirstEdge.Y) * 500);
    Tmp3 := Round((Box.SecondEdge.Z - Box.FirstEdge.Z) * 1000);
    if Tmp1 > Tmp2 then
     Result := Tmp1
    else if Tmp2 > Tmp3 then
     Result := Tmp2
    else
     Result := Tmp3;
  except
    on Exception do Result := 1000;
  end;
end;

function IsBoxAllInBox3D(Box1, Box2: TRect3D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  Box2 := ReOrderRect3D(Box2);
  FCode := _PositionCodeCartesian3D(Box2, Box1.FirstEdge);
  SCode := _PositionCodeCartesian3D(Box2, Box1.SecondEdge);
  Result := (FCode = []) and (SCode = []);
end;

function IsBoxAllInBoxNRC3D(Box: TRect3D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  FCode := _PositionCodeNRC3D(Box.FirstEdge);
  SCode := _PositionCodeNRC3D(Box.SecondEdge);
  Result := (FCode = []) and (SCode = []);
end;

function IsBoxAllInFrontNRC3D(Box: TRect3D): Boolean;
var
  FCode, SCode: TOutCode;
begin
  FCode := _PositionCodeNRC3D(Box.FirstEdge);
  SCode := _PositionCodeNRC3D(Box.SecondEdge);
  Result := not (neareye in FCode) and not (neareye in SCode);
end;

function BoxOutBox3D(Box1, Box2: TRect3D): TRect3D;
begin
  Box1 := ReOrderRect3D(Box1);
  Box2 := ReOrderRect3D(Box2);
  Result := Box1;
  if Box2.FirstEdge.X < Box1.FirstEdge.X then
   Result.FirstEdge.X := Box2.FirstEdge.X;
  if Box2.SecondEdge.X > Box1.SecondEdge.X then
   Result.SecondEdge.X := Box2.SecondEdge.X;
  if Box2.FirstEdge.Y < Box1.FirstEdge.Y then
   Result.FirstEdge.Y := Box2.FirstEdge.Y;
  if Box2.SecondEdge.Y > Box1.SecondEdge.Y then
   Result.SecondEdge.Y := Box2.SecondEdge.Y;
  if Box2.FirstEdge.Z < Box1.FirstEdge.Z then
   Result.FirstEdge.Z := Box2.FirstEdge.Z;
  if Box2.SecondEdge.Z > Box1.SecondEdge.Z then
   Result.SecondEdge.Z := Box2.SecondEdge.Z;
end;

function NearPoint3D(RP, P: TPoint3D; const Aperture: TRealType;
                     var Dist: TRealType; const NT: TTransf3D): Boolean;
var
  P2D, RP2D: TPoint2D;
begin
  RP2D := Point3DToPoint2D(TransformPoint3D(RP, NT));
  P2D := Point3DToPoint2D(P);
  Result := NearPoint2D(RP2D, P2D, Aperture, Dist);
end;

function IsPointOnPolyLine3D(const Vect: Pointer; Count: Integer;
                             const P: TPoint3D; var Dist: TRealType;
                             const Aperture: TRealType; const NT: TTransf3D; const MustClose: Boolean): Integer;
var
  MinDist, TmpDist: TRealType;
  TmpPt12D, TmpPt22D, TmpPt2D: TPoint2D;
  Cont, Max: Integer;
begin
  Result := PICK_NOOBJECT;
  Dist := MaxCoord;
  if Count = 0 then
   Exit;
  MinDist := Aperture;
  TmpPt12D := Point3DToPoint2D(TransformPoint3D(PVectPoints3D(Vect)^[0], NT));
  TmpPt2D := Point3DToPoint2D(P);
  if MustClose then
   Max := Count
  else
   Max := Count - 1;
  for Cont := 1 to Max do
   begin
     if Cont = Count then
      TmpPt22D := Point3DToPoint2D(TransformPoint3D(PVectPoints3D(Vect)^[0], NT))
     else
      TmpPt22D := Point3DToPoint2D(TransformPoint3D(PVectPoints3D(Vect)^[Cont], NT));
     TmpDist := 0;
     if _PointSegmentDistance2D(TmpPt2D, TmpPt12D, TmpPt22D, TmpDist) and (TmpDist <= MinDist) then
      begin
        Result := PICK_ONOBJECT;
        Dist := TmpDist;
        MinDist := Dist;
      end;
     TmpPt12D := TmpPt22D;
   end;
end;

function IsPointOnLine3D(const A, B, P: TPoint3D; var Dist: TRealType;
                       const Aperture: TRealType; const NT: TTransf3D): Integer;
var
  TmpDist: TRealType;
  TmpPt12D, TmpPt22D, TmpPt2D: TPoint2D;
begin
  Result := PICK_NOOBJECT;
  Dist := MaxCoord;
  TmpPt12D := Point3DToPoint2D(TransformPoint3D(A, NT));
  TmpPt22D := Point3DToPoint2D(TransformPoint3D(B, NT));
  TmpPt2D := Point3DToPoint2D(P);
  TmpDist := 0;
  if _PointSegmentDistance2D(TmpPt2D, TmpPt12D, TmpPt22D, TmpDist) and (TmpDist <= Aperture) then
   begin
     Result := PICK_ONOBJECT;
     Dist := TmpDist;
   end;
end;

function GetBox2DExtension3D(Box: TRect3D; const WorldToPlane: TTransf3D): TRect2D;
var
  TmpPt: TPoint3D;
  TmpBox: TRect2D;
begin
  Result.FirstEdge := Point3DToPoint2D(TransformPoint3D(Box.FirstEdge, WorldToPlane));
  Result.SecondEdge := Point3DToPoint2D(TransformPoint3D(Box.SecondEdge, WorldToPlane));

  TmpPt := Point3D(Box.FirstEdge.X, Box.FirstEdge.Y, Box.SecondEdge.Z);
  TmpBox.FirstEdge := Point3DToPoint2D(TransformPoint3D(TmpPt, WorldToPlane));
  TmpPt := Point3D(Box.FirstEdge.X, Box.SecondEdge.Y, Box.FirstEdge.Z);
  TmpBox.SecondEdge := Point3DToPoint2D(TransformPoint3D(TmpPt, WorldToPlane));
  Result := BoxOutBox2D(Result, TmpBox);

  TmpPt := Point3D(Box.SecondEdge.X, Box.SecondEdge.Y, Box.FirstEdge.Z);
  TmpBox.FirstEdge := Point3DToPoint2D(TransformPoint3D(TmpPt, WorldToPlane));
  TmpPt := Point3D(Box.SecondEdge.X, Box.FirstEdge.Y, Box.SecondEdge.Z);
  TmpBox.SecondEdge := Point3DToPoint2D(TransformPoint3D(TmpPt, WorldToPlane));
  Result := BoxOutBox2D(Result, TmpBox);

  TmpPt := Point3D(Box.FirstEdge.X, Box.SecondEdge.Y, Box.SecondEdge.Z);
  TmpBox.FirstEdge := Point3DToPoint2D(TransformPoint3D(TmpPt, WorldToPlane));
  TmpPt := Point3D(Box.SecondEdge.X, Box.FirstEdge.Y, Box.FirstEdge.Z);
  TmpBox.SecondEdge := Point3DToPoint2D(TransformPoint3D(TmpPt, WorldToPlane));
  Result := BoxOutBox2D(Result, TmpBox);
end;

function _PointOutBox3D(const TmpPt: TPoint3D; const Box: TRect3D): TRect3D;
begin
  Result := Box;
  if TmpPt.X > Result.SecondEdge.X then
   Result.SecondEdge.X := TmpPt.X
  else if TmpPt.X < Result.FirstEdge.X then
   Result.FirstEdge.X := TmpPt.X;
  if TmpPt.Y > Result.SecondEdge.Y then
   Result.SecondEdge.Y := TmpPt.Y
  else if TmpPt.Y < Result.FirstEdge.Y then
   Result.FirstEdge.Y := TmpPt.Y;
  if TmpPt.Z > Result.SecondEdge.Z then
   Result.SecondEdge.Z := TmpPt.Z
  else if TmpPt.Z < Result.FirstEdge.Z then
   Result.FirstEdge.Z := TmpPt.Z;
end;

function TransformBoundingBox3D(Box: TRect3D; const Transf: TTransf3D): TRect3D;
var
  TmpPt: TPoint3D;
begin
  Box := CartesianRect3D(Box);
  TmpPt := TransformPoint3D(Point3D(Box.FirstEdge.X, Box.FirstEdge.Y, Box.FirstEdge.Z), Transf);
  Result.FirstEdge := TmpPt;
  Result.SecondEdge := TmpPt;
  TmpPt := TransformPoint3D(Point3D(Box.FirstEdge.X, Box.SecondEdge.Y, Box.FirstEdge.Z), Transf);
  Result := _PointOutBox3D(CartesianPoint3D(TmpPt), Result);
  TmpPt := TransformPoint3D(Point3D(Box.SecondEdge.X, Box.SecondEdge.Y, Box.FirstEdge.Z), Transf);
  Result := _PointOutBox3D(CartesianPoint3D(TmpPt), Result);
  TmpPt := TransformPoint3D(Point3D(Box.SecondEdge.X, Box.FirstEdge.Y, Box.FirstEdge.Z), Transf);
  Result := _PointOutBox3D(CartesianPoint3D(TmpPt), Result);
  TmpPt := TransformPoint3D(Point3D(Box.FirstEdge.X, Box.FirstEdge.Y, Box.SecondEdge.Z), Transf);
  Result := _PointOutBox3D(CartesianPoint3D(TmpPt), Result);
  TmpPt := TransformPoint3D(Point3D(Box.FirstEdge.X, Box.SecondEdge.Y, Box.SecondEdge.Z), Transf);
  Result := _PointOutBox3D(CartesianPoint3D(TmpPt), Result);
  TmpPt := TransformPoint3D(Point3D(Box.SecondEdge.X, Box.SecondEdge.Y, Box.SecondEdge.Z), Transf);
  Result := _PointOutBox3D(CartesianPoint3D(TmpPt), Result);
  TmpPt := TransformPoint3D(Point3D(Box.SecondEdge.X, Box.FirstEdge.Y, Box.SecondEdge.Z), Transf);
  Result := _PointOutBox3D(CartesianPoint3D(TmpPt), Result);
end;

function ExtrudePoint3D(Pt: TPoint3D; const Dir: TVector3D; Size: TRealType): TPoint3D;
begin
  Pt := CartesianPoint3D(Pt);
  Result.X := Pt.X + Dir.X * Size;
  Result.Y := Pt.Y + Dir.Y * Size;
  Result.Z := Pt.Z + Dir.Z * Size;
  Result.W := Pt.W;
end;

function WorldPointToPlane3D(const WPt: TPoint3D; const CastDir, PlaneNorm, PlaneUP: TVector3D; const PlaneOrg: TPoint3D): TPoint2D;
var
  RefX, NewPt, ResPt: TVector3D;
  TmpPt: TPoint3D;
begin
  RefX := CrossProd3D(PlaneUp, PlaneNorm);
  TmpPt := CastPointOnPlane3D(WPt, CastDir, PlaneNorm, PlaneOrg);
  NewPt := Vector3D(CartesianPoint3D(PlaneOrg), CartesianPoint3D(TmpPt));
  ResPt.X := DotProduct3D(NewPt, RefX);
  ResPt.Y := DotProduct3D(NewPt, PlaneUP);
  Result.X := ResPt.X;
  Result.Y := ResPt.Y;
  Result.W := 1.0;
end;

function PlanePointToWorld3D(const PPt: TPoint2D; const PlaneNorm, PlaneUP: TVector3D; const PlaneOrg: TPoint3D): TPoint3D;
var
  RefX, RefY: TVector3D;
begin
  RefX := CrossProd3D(PlaneUp, PlaneNorm);
  RefY := CrossProd3D(PlaneNorm, RefX);
  Result.X := PlaneOrg.X + RefX.X * PPt.X + RefY.X * PPt.Y;
  Result.Y := PlaneOrg.Y + RefX.Y * PPt.X + RefY.Y * PPt.Y;
  Result.Z := PlaneOrg.Z + RefX.Z * PPt.X + RefY.Z * PPt.Y;
  Result.W := PlaneOrg.W;
end;

procedure ViewPlane(const CameraPos, CameraView: TPoint3D; CameraUp: TVector3D; var PlaneNorm, PlaneUp: TVector3D; var PlaneOrg: TPoint3D);
begin
  PlaneNorm := Direction3D(CameraPos, CameraView);
  PlaneUp := CameraUp;
  PlaneOrg := CameraView;
end;

function CastVectorOnPlane3D(const V: TVector3D; const PlaneNorm: TVector3D): TVector3D;
var
  t: TRealType;
  TmpPt: TPoint3D;
begin
  t := DotProduct3D(V, PlaneNorm);
  TmpPt := ExtrudePoint3D(Point3D(V.X, V.Y, V.Z), PlaneNorm, t);
  Result := Versor3D(TmpPt.X, TmpPt.Y, TmpPt.Z);
end;

function CastPointOnPlane3D(const WPt: TPoint3D; const CastDir, PlaneNorm: TVector3D; const PlaneOrg: TPoint3D): TPoint3D;
var
  t: TRealType;
begin
  if DotProduct3D(CastDir, PlaneNorm) = 0 then
   begin // Perpendicular, use the plane normal to cast.
     t := DotProduct3D(Vector3D(WPt, PlaneOrg), PlaneNorm);
     Result := ExtrudePoint3D(WPt, PlaneNorm, t);
   end
  else
   begin
     t := DotProduct3D(Vector3D(WPt, PlaneOrg), PlaneNorm) / DotProduct3D(CastDir, PlaneNorm);
     Result := ExtrudePoint3D(WPt, CastDir, t);
   end;
end;

procedure NormalPtPlaneToEq3D(const Normal: TVector3D; const Pt: TPoint3D; var A, B, C, D: TRealType);
begin
  A := Normal.X;
  B := Normal.Y;
  C := Normal.Z;
  with CartesianPoint3D(Pt) do
   D := -(A * X + B * Y + C * Z);
end;

procedure PlaneEqToNormalPt3D(A, B, C, D: TRealType; var Normal: TVector3D; var Org: TPoint3D);
begin
  Normal.X := A;
  Normal.Y := B;
  Normal.Z := C;
  Org := ExtrudePoint3D(Point3D(0, 0, 0), Normal, D);
end;

function Reflect3D(const V: TVector3D): TVector3D;
begin
  Result.X := -V.X;
  Result.Y := -V.Y;
  Result.Z := -V.Z;
end;

procedure MakeOrto3D(LastPt: TPoint3D; var CurrPt: TPoint3D; PlaneOrigin: TPoint3D; PlaneNormal, PlaneUP: TVector3D);
var
  DeltaPt: TVector2D;
  CurrPt2D, LastPt2D: TPoint2D;
begin
  CurrPt2D := WorldPointToPlane3D(CurrPt, PlaneNormal, PlaneNormal, PlaneUP, PlaneOrigin);
  LastPt2D := WorldPointToPlane3D(LastPt, PlaneNormal, PlaneNormal, PlaneUP, PlaneOrigin);
  LastPt2D := CartesianPoint2D(LastPt2D);
  CurrPt2D := CartesianPoint2D(CurrPt2D);
  DeltaPt := Vector2D(LastPt2D, CurrPt2D);
  if Abs(DeltaPt.X) > Abs(DeltaPt.Y) then
   CurrPt2D.Y := LastPt2D.Y
  else
   CurrPt2D.X := LastPt2D.X;
  CurrPt := PlanePointToWorld3D(CurrPt2D, PlaneNormal, PlaneUP, PlaneOrigin);
end;

function GetHandleRuleForNormals: THandleRule;
begin
  Result := _HandleRuleForNormal;
end;

procedure SetHandleRuleForNormals(const HRule: THandleRule);
begin
  _HandleRuleForNormal := HRule;
end;

function GetVectNormal(const Vect: Pointer; Count: Integer): TVector3D;
var
  Max, Cont: Integer;
  X, Y, Z: Extended;
begin
  Result.X := 0.0;
  Result.Y := 0.0;
  Result.Z := 0.0;
  Max := Count;
  if Max < 3 then
   Exit;
  X := 0.0;
  Y := 0.0;
  Z := 0.0;
  for Cont := 0 to Max - 2 do
   begin
     X := X + (PVectPoints3D(Vect)^[Cont].Z + PVectPoints3D(Vect)^[Cont + 1].Z) * (PVectPoints3D(Vect)^[Cont + 1].Y - PVectPoints3D(Vect)^[Cont].Y);
     Y := Y + (PVectPoints3D(Vect)^[Cont].X + PVectPoints3D(Vect)^[Cont + 1].X) * (PVectPoints3D(Vect)^[Cont + 1].Z - PVectPoints3D(Vect)^[Cont].Z);
     Z := Z + (PVectPoints3D(Vect)^[Cont].Y + PVectPoints3D(Vect)^[Cont + 1].Y) * (PVectPoints3D(Vect)^[Cont + 1].X - PVectPoints3D(Vect)^[Cont].X);
   end;
  X := X + (PVectPoints3D(Vect)^[Max - 1].Z + PVectPoints3D(Vect)^[0].Z) * (PVectPoints3D(Vect)^[0].Y - PVectPoints3D(Vect)^[Max - 1].Y);
  Y := Y + (PVectPoints3D(Vect)^[Max - 1].X + PVectPoints3D(Vect)^[0].X) * (PVectPoints3D(Vect)^[0].Z - PVectPoints3D(Vect)^[Max - 1].Z);
  Z := Z + (PVectPoints3D(Vect)^[Max - 1].Y + PVectPoints3D(Vect)^[0].Y) * (PVectPoints3D(Vect)^[0].X - PVectPoints3D(Vect)^[Max - 1].X);
  Result := Versor3D(X, Y, Z);
  if _HandleRuleForNormal = hrRightHand then
   Result := Reflect3D(Result);
end;

function PointDistance3D(const P1, P2: TPoint3D): TRealType;
begin
  Result := VectorLength3D(Vector3D(P1, P2));
end;

{$R+}
function DotProduct3D(const V1, V2: TVector3D): TRealType;
var
  Res: Extended;
begin
  Res := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;
  Result := Res;
end;

function CrossProd3D(const V1, V2: TVector3D): TVector3D;
var
  ResX, ResY, ResZ: Extended;
begin
  ResX := V1.Y * V2.Z - V1.Z * V2.Y;
  ResY := V1.Z * V2.X - V1.X * V2.Z;
  ResZ := V1.X * V2.Y - V1.Y * V2.X;
  Result.X := ResX;
  Result.Y := ResY;
  Result.Z := ResZ;
end;
{$R-}

function PointOutBox3D(Pt: TPoint3D; Box: TRect3D): TRect3D;
begin
  Pt := CartesianPoint3D(Pt);
  Result := Box;
  if Pt.X > Result.SecondEdge.X then
   Result.SecondEdge.X := Pt.X
  else if Pt.X < Result.FirstEdge.X then
   Result.FirstEdge.X := Pt.X;
  if Pt.Y > Result.SecondEdge.Y then
   Result.SecondEdge.Y := Pt.Y
  else if Pt.Y < Result.FirstEdge.Y then
   Result.FirstEdge.Y := Pt.Y;
  if Pt.Z > Result.SecondEdge.Z then
   Result.SecondEdge.Z := Pt.Z
  else if Pt.Z < Result.FirstEdge.Z then
   Result.FirstEdge.Z := Pt.Z;
end;

{ 3D clipping functions }

function _DotW(const P1, P2: TPoint3D): TRealType;
begin
  Result := P1.X * P2.X + P1.Y * P2.Y + P1.Z * P2.Z + P1.W * P2.W;
end;

// P1 - P2
function _PointDifference3D(const P1, P2: TPoint3D): TPoint3D;
begin
  Result.X := P1.X - P2.X;
  Result.Y := P1.Y - P2.Y;
  Result.Z := P1.Z - P2.Z;
  Result.W := P1.W - P2.W;
end;

{
  S è il punto iniziale del segmento
  E è il punto finale del segmento
  Normal è la normale del piano di clipping
  PE è un punto sul piano di clipping
  R è la differenza E - S
  tE è il parametro di intersezione iniziale
  tL è il parametro di intersezione finale
}
function _Intersect(const S, {%H-}E, Normal, PE, R: TPoint3D; var tE, tL: Extended): Boolean;
var
  t, d: Extended;
begin
  Result := False;
  d := -_DotW(Normal, R);

  if d > 0 then
   begin
     t := _DotW(Normal, _PointDifference3D(S, PE)) / d;
     if T > tL then
      Exit
     else if T > tE then
      tE := T;
   end
  else if d < 0 then
   begin
     t := _DotW(Normal, _PointDifference3D(S, PE)) / d;
     if T < tE then
      Exit
     else if T < tL then
      tL := T;
   end
  else if _DotW(Normal, _PointDifference3D(S, PE)) > 0 then
   Exit;
  Result := True;
end;

const
  FAR_NORMAL: TPoint3D = (X: 0.0; Y: 0.0; Z: 1.0; W: 0.0);
  FAR_CLIPPOINT: TPoint3D = (X: 0.0; Y: 0.0; Z:0.0; W: 1.0);

  NEAR_NORMAL: TPoint3D = (X: 0.0; Y: 0.0; Z:-0.7071067811865; W:-0.7071067811865);
  NEAR_CLIPPOINT: TPoint3D = (X: 0.0; Y: 0.0; Z:-1.0; W: 1.0);

  LEFT_NORMAL: TPoint3D = (X:-0.7071067811865; Y: 0.0; Z: 0.0; W:-0.7071067811865);
  LEFT_CLIPPOINT: TPoint3D = (X:-1.0; Y: 0.0; Z: 0.0; W: 1.0);

  RIGHT_NORMAL: TPoint3D = (X:0.7071067811865; Y: 0.0; Z: 0.0; W:-0.7071067811865);
  RIGHT_CLIPPOINT: TPoint3D = (X: 1.0; Y: 0.0; Z: 0.0; W: 1.0);

  TOP_NORMAL: TPoint3D = (X: 0.0; Y:0.7071067811865; Z: 0.0; W:-0.7071067811865);
  TOP_CLIPPOINT: TPoint3D = (X: 0.0; Y: 1.0; Z: 0.0; W: 1.0);

  BOTTOM_NORMAL: TPoint3D = (X: 0.0; Y:-0.7071067811865; Z: 0.0; W:-0.7071067811865);
  BOTTOM_CLIPPOINT: TPoint3D = (X: 0.0; Y:-1.0; Z: 0.0; W: 1.0);

function ClipLine3D(var Pt1, Pt2: TPoint3D): TClipResult;
var
  tE, tL: Extended;
  D: TPoint3D;
begin
  Result := [ccNotVisible];
  D := _PointDifference3D(Pt2, Pt1);
  if (D.X = 0.0) and (D.Y = 0.0) and (D.Z = 0.0) and IsPointInBoxNRC3D(Pt1) then
   begin
     Result := [ccVisible];
     Exit;
   end;
  tE := 0.0;
  tL := 1.0;
  if _Intersect(Pt1, Pt2, FAR_NORMAL, FAR_CLIPPOINT, D, tE, tL) then // Front
   if _Intersect(Pt1, Pt2, NEAR_NORMAL, NEAR_CLIPPOINT, D, tE, tL) then // Back
    if _Intersect(Pt1, Pt2, LEFT_NORMAL, LEFT_CLIPPOINT, D, tE, tL) then // Left
     if _Intersect(Pt1, Pt2, RIGHT_NORMAL, RIGHT_CLIPPOINT, D, tE, tL) then // Right
      if _Intersect(Pt1, Pt2, TOP_NORMAL, TOP_CLIPPOINT, D, tE, tL) then // Top
       if _Intersect(Pt1, Pt2, BOTTOM_NORMAL, BOTTOM_CLIPPOINT, D, tE, tL) then // Bottom
        begin
          Result := [];
          if tL < 1 then
           begin
             Pt2.X := Pt1.X + tL * D.X;
             Pt2.Y := Pt1.Y + tL * D.Y;
             Pt2.Z := Pt1.Z + tL * D.Z;
             Pt2.W := Pt1.W + tL * D.W;
             Result := [ccSecond];
           end;
          if tE > 0 then
           begin
             Pt1.X := Pt1.X + tE * D.X;
             Pt1.Y := Pt1.Y + tE * D.Y;
             Pt1.Z := Pt1.Z + tE * D.Z;
             Pt1.W := Pt1.W + tE * D.W;
             Result := Result + [ccFirst];
           end;
          if Result = [] then
           Result := [ccVisible];
        end;
end;

procedure Draw2DSubSetAsPolyline(const Vect: Pointer; Count: Integer;
                                 const Cnv: TDecorativeCanvas;
                                 const Clip, Extent: TRect2D;
                                 const S: TTransf2D;
                                 const StartIdx, EndIdx: Integer;
                                 const ToBeClosed: Boolean);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, Cont, AllocatedMem: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts: ^TPoints;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  if (Count = 0) or (EndIdx > Count) or (StartIdx > EndIdx) then
   Exit;
  if ToBeClosed then
   AllocatedMem := (Count + 1) * SizeOf(TPoint)
  else
   AllocatedMem := Count * SizeOf(TPoint);
  GetMem(TmpPts, AllocatedMem);
  try
    VisPoints := 0;
    if IsBoxAllInBox2D(TransformRect2D(Extent, S), Clip) then
     begin
       for Cont := StartIdx to EndIdx do
        begin
          TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont], S);
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
       if ToBeClosed then
        begin
          TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S);
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
          Inc(VisPoints);
        end;
       ClipRes := [ccVisible];
     end
    else if ToBeClosed then
     begin
       TmpClip := ReorderRect2D(Clip);
       for Cont := StartIdx + 1 to EndIdx + 1 do
        begin
          TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont - 1], S);
          if Cont > EndIdx then
           TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S)
          else
           TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont], S);
          ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
             Inc(VisPoints);
           end;
          if ccSecond in ClipRes then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
             Inc(VisPoints);
             Cnv.Polyline(TmpPts, VisPoints);
             VisPoints := 0;
           end;
        end;
       if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
        end;
      end
    else
     begin
       TmpClip := ReorderRect2D(Clip);
       for Cont := StartIdx + 1 to EndIdx do
        begin
          TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont - 1], S);
          TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont], S);
          ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
             Inc(VisPoints);
           end;
          if ccSecond in ClipRes then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
             Inc(VisPoints);
             Cnv.Polyline(TmpPts, VisPoints);
             VisPoints := 0;
           end;
        end;
       if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
        end;
      end;
     if (VisPoints > 0) then
      Cnv.Polyline(TmpPts, VisPoints);
  finally
    FreeMem(TmpPts, AllocatedMem);
  end;
end;

procedure Draw2DSubSetAsPolygon(const Vect: Pointer; Count: Integer;
                                const Cnv: TDecorativeCanvas;
                                const Clip, Extent: TRect2D;
                                const S: TTransf2D;
                                const StartIdx, EndIdx: Integer);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, VisPoints1, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts, FirstClipPts: ^TPoints;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  if (Count = 0) or (EndIdx > Count) or (StartIdx > EndIdx) then
   Exit;
  GetMem(TmpPts, Count * 3 * SizeOf(TPoint));
  GetMem(FirstClipPts, Count * 3 * SizeOf(TPoint));
  try
    VisPoints := 0;
    VisPoints1 := 0;
    if IsBoxAllInBox2D(TransformRect2D(Extent, S), Clip) then
     for Cont := StartIdx to EndIdx do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont], S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end
    else
     begin
       TmpClip := ReorderRect2D(Clip);
       for Cont := StartIdx to EndIdx do
        begin
          TmpPt1 := TransformPoint2D(PVectPoints2D(Vect)^[Cont], S);
          if Cont < EndIdx then
           TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[Cont + 1], S)
          else
           TmpPt2 := TransformPoint2D(PVectPoints2D(Vect)^[StartIdx], S);
          ClipRes := _ClipLineLeftRight2D(TmpClip, TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt1);
             Inc(VisPoints1);
           end;
          if ccSecond in ClipRes then
           begin
             FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt2);
             Inc(VisPoints1);
           end;
        end;
       FirstClipPts^[VisPoints1] := FirstClipPts^[0];
       Inc(VisPoints1);
       VisPoints := 0;
       for Cont := 0 to VisPoints1 - 2 do
        begin
          TmpPt1 := PointToPoint2D(FirstClipPts^[Cont]);
          TmpPt2 := PointToPoint2D(FirstClipPts^[Cont + 1]);
          ClipRes := _ClipLineUpBottom2D(TmpClip, TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
             Inc(VisPoints);
           end;
          if ccSecond in ClipRes then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
             Inc(VisPoints);
           end;
        end;
     end;
     if (VisPoints > 0) then
      Polygon(Cnv.Canvas.Handle, TmpPts^, VisPoints, {!!!}False);
  finally
    FreeMem(TmpPts, Count * 3 * SizeOf(TPoint));
    FreeMem(FirstClipPts, Count * 3 * SizeOf(TPoint));
  end;
end;

procedure DrawPlaceHolder(const Cnv: TDecorativeCanvas; const X, Y, Wdt: Integer);
var
  TmpWdt: Integer;
begin
  with Cnv.Canvas do
   begin
     Brush.Style := bsSolid;
     Brush.Color := clSilver;
     Pen.Style := psSolid;
     TmpWdt := Wdt div 2;
     LCLIntf.Rectangle(Handle, X - TmpWdt, Y - TmpWdt, X + TmpWdt, Y + TmpWdt);
   end;
end;

procedure DrawRoundPlaceHolder(const Cnv: TDecorativeCanvas; const X, Y, Wdt: Integer);
var
  TmpWdt: Integer;
begin
  with Cnv.Canvas do
   begin
     Brush.Style := bsSolid;
     Brush.Color := clSilver;
     Pen.Style := psSolid;
     TmpWdt := Wdt div 2;
     LCLIntf.Ellipse(Handle, X - TmpWdt, Y - TmpWdt, X + TmpWdt, Y + TmpWdt);
   end;
end;

procedure DrawRect2DAsPolyline(const Cnv: TDecorativeCanvas; const Box, Clip: TRect2D; const MT, S: TTransf2D);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts: ^TPoints;
  BoxPts: ^TVectPoints2D;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  GetMem(TmpPts, 5 * SizeOf(TPoint));
  GetMem(BoxPts, 5 * SizeOf(TPoint2D));
  try
    VisPoints := 0;
    Cont := 0;
    BoxPts^[Cont] := TransformPoint2D(Box.FirstEdge, MT);
    BoxPts^[Cont + 1] := TransformPoint2D(Point2D(Box.Left, Box.Top), MT);
    BoxPts^[Cont + 2] := TransformPoint2D(Box.SecondEdge, MT);
    BoxPts^[Cont + 3] := TransformPoint2D(Point2D(Box.Right, Box.Bottom), MT);
    BoxPts^[Cont + 4] := TransformPoint2D(Box.FirstEdge, MT);
    if IsBoxAllInBox2D(TransformRect2D(Box, S), Clip) then
     for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont], S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
        ClipRes := [ccVisible];
      end
    else
     begin
       TmpClip := ReorderRect2D(Clip);
       for Cont := 1 to 4 do
        begin
          TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont - 1], S);
          TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont], S);;
          ClipRes := _ClipLine2D(TmpClip, TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
             Inc(VisPoints);
           end;
          if ccSecond in ClipRes then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
             Inc(VisPoints);
             Cnv.Polyline(TmpPts, VisPoints);
             VisPoints := 0;
           end;
        end;
       if not (ccNotVisible in ClipRes) then
        begin
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
          Inc(VisPoints);
        end;
      end;
     if (VisPoints > 0) then
      Cnv.Polyline(TmpPts, VisPoints)
  finally
    FreeMem(BoxPts, 5 * SizeOf(TPoint2D));
    FreeMem(TmpPts, 5 * SizeOf(TPoint));
  end;
end;

procedure DrawRect2DAsPolygon(const Cnv: TDecorativeCanvas; const Box, Clip: TRect2D; const MT, S: TTransf2D);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, VisPoints1, Cont: Integer;
  TmpPt1, TmpPt2: TPoint2D;
  TmpPts, FirstClipPts: ^TPoints;
  BoxPts: ^TVectPoints2D;
  ClipRes: TClipResult;
  TmpClip: TRect2D;
begin
  GetMem(TmpPts, 15 * SizeOf(TPoint));
  GetMem(FirstClipPts, 15 * SizeOf(TPoint));
  GetMem(BoxPts, 5 * SizeOf(TPoint2D));
  try
    Cont := 0;
    BoxPts^[Cont] := TransformPoint2D(Box.FirstEdge, MT);
    BoxPts^[Cont + 1] := TransformPoint2D(Point2D(Box.Left, Box.Top), MT);
    BoxPts^[Cont + 2] := TransformPoint2D(Box.SecondEdge, MT);
    BoxPts^[Cont + 3] := TransformPoint2D(Point2D(Box.Right, Box.Bottom), MT);
    BoxPts^[Cont + 4] := TransformPoint2D(Box.FirstEdge, MT);
    VisPoints := 0;
    VisPoints1 := 0;
    if IsBoxAllInBox2D(TransformRect2D(Box, S), Clip) then
     for Cont := 0 to 4 do
      begin
        TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont], S);
        TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
        Inc(VisPoints);
      end
    else
     begin
       TmpClip := ReorderRect2D(Clip);
       for Cont := 0 to 4 do
        begin
          TmpPt1 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont], S);
          if Cont < 4 then
           TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[Cont + 1], S)
          else
           TmpPt2 := TransformPoint2D(PVectPoints2D(BoxPts)^[0], S);
          ClipRes := _ClipLineLeftRight2D(TmpClip, TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt1);
             Inc(VisPoints1);
           end;
          if ccSecond in ClipRes then
           begin
             FirstClipPts^[VisPoints1] := Point2DToPoint(TmpPt2);
             Inc(VisPoints1);
           end;
        end;
       FirstClipPts^[VisPoints1].X := FirstClipPts^[0].X;
       FirstClipPts^[VisPoints1].Y := FirstClipPts^[0].Y;
       Inc(VisPoints1);
       VisPoints := 0;
       for Cont := 0 to VisPoints1 - 2 do
        begin
          TmpPt1 := PointToPoint2D(FirstClipPts^[Cont]);
          TmpPt2 := PointToPoint2D(FirstClipPts^[Cont + 1]);
          ClipRes := _ClipLineUpBottom2D(TmpClip, TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt1);
             Inc(VisPoints);
           end;
          if ccSecond in ClipRes then
           begin
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt2);
             Inc(VisPoints);
           end;
        end;
     end;
     if (VisPoints > 0) then
      Polygon(Cnv.Canvas.Handle, TmpPts^, VisPoints, {!!!}False);
  finally
    FreeMem(BoxPts, 5 * SizeOf(TPoint2D));
    FreeMem(TmpPts, 15 * SizeOf(TPoint));
    FreeMem(FirstClipPts, 15 * SizeOf(TPoint));
  end;
end;

function DrawLine2D(const Cnv: TDecorativeCanvas; P1, P2: TPoint2D; const Clip: TRect2D; const S: TTransf2D): Boolean;
var
  TmpPt1, TmpPt2: TPoint;
  TmpPt12D, TmpPt22D: TPoint2D;
  ClipRes: TClipResult;
  TmpBox: TRect2D;
  TmpClip: TRect2D;
begin
  TmpBox.FirstEdge := P1;
  TmpBox.SecondEdge := P2;
  TmpBox := ReorderRect2D(TmpBox);
  if IsBoxAllInBox2D(TransformRect2D(TmpBox, S), Clip) then
   begin
     Result := True;
     TmpPt1 := Point2DToPoint(TransformPoint2D(P1, S));
     TmpPt2 := Point2DToPoint(TransformPoint2D(P2, S));
     Cnv.MoveTo(TmpPt1.X, TmpPt1.Y);
     Cnv.LineTo(TmpPt2.X, TmpPt2.Y);
   end
  else
   begin
     TmpClip := ReorderRect2D(Clip);
     TmpPt12D := TransformPoint2D(P1, S);
     TmpPt22D := TransformPoint2D(P2, S);;
     ClipRes := _ClipLine2D(TmpClip, TmpPt12D, TmpPt22D);
     Result := not (ccNotVisible in ClipRes);
     if Result then
      begin
        TmpPt1 := Point2DToPoint(TmpPt12D);
        TmpPt2 := Point2DToPoint(TmpPt22D);
        Cnv.MoveTo(TmpPt1.X, TmpPt1.Y);
        Cnv.LineTo(TmpPt2.X, TmpPt2.Y);
      end;
   end;
end;

procedure DrawBoundingBox2D(const Cnv: TDecorativeCanvas; Box, Clip: TRect2D; const S: TTransf2D);
begin
  DrawRect2DAsPolyline(Cnv, Box, Clip, IdentityTransf2D, S)
end;

procedure Draw3DSubSetAsPolyline(const Vect: Pointer; Count: Integer;
                                 const Cnv: TDecorativeCanvas;
                                 const Extent: TRect3D;
                                 const NT: TTransf3D;
                                 const VT: TTransf2D;
                                 const StartIdx, EndIdx: Integer;
                                 const ToBeClosed: Boolean);
type
  TPoints = array[0..0] of TPoint;
var
  VisPoints, Cont, AllocatedMem: Integer;
  TmpPt1, TmpPt2: TPoint3D;
  TmpPt12D, TmpPt22D: TPoint2D;
  TmpPts: ^TPoints;
  ClipRes: TClipResult;
begin
  if (Count = 0) or (EndIdx >= Count) or (StartIdx >= EndIdx) then
   Exit;
  if ToBeClosed then
   AllocatedMem := (Count + 1) * SizeOf(TPoint)
  else
   AllocatedMem := Count * SizeOf(TPoint);
  GetMem(TmpPts, AllocatedMem);
  try
    VisPoints := 0;
    if ToBeClosed then
     begin
       for Cont := StartIdx + 1 to EndIdx + 1 do
        begin
          TmpPt1 := TransformPoint3D(PVectPoints3D(Vect)^[Cont - 1], NT);
          if Cont > EndIdx then
           TmpPt2 := TransformPoint3D(PVectPoints3D(Vect)^[StartIdx], NT)
          else
           TmpPt2 := TransformPoint3D(PVectPoints3D(Vect)^[Cont], NT);
          ClipRes := ClipLine3D(TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             TmpPt12D := TransformPoint2D(Point3DToPoint2D(TmpPt1), VT);
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt12D);
             Inc(VisPoints);
           end;
          if ccSecond in ClipRes then
           begin
             TmpPt22D := TransformPoint2D(Point3DToPoint2D(TmpPt2), VT);
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt22D);
             Inc(VisPoints);
             Cnv.Polyline(TmpPts, VisPoints);
             VisPoints := 0;
           end;
        end;
       if not (ccNotVisible in ClipRes) then
        begin
          TmpPt22D := TransformPoint2D(Point3DToPoint2D(TmpPt2), VT);
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt22D);
          Inc(VisPoints);
        end;
     end
    else
     begin
       for Cont := StartIdx + 1 to EndIdx do
        begin
          TmpPt1 := TransformPoint3D(PVectPoints3D(Vect)^[Cont - 1], NT);
          TmpPt2 := TransformPoint3D(PVectPoints3D(Vect)^[Cont], NT);
          ClipRes := ClipLine3D(TmpPt1, TmpPt2);
          if not (ccNotVisible in ClipRes) then
           begin
             TmpPt12D := TransformPoint2D(Point3DToPoint2D(TmpPt1), VT);
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt12D);
             Inc(VisPoints);
           end;
          if ccSecond in ClipRes then
           begin
             TmpPt22D := TransformPoint2D(Point3DToPoint2D(TmpPt2), VT);
             TmpPts^[VisPoints] := Point2DToPoint(TmpPt22D);
             Inc(VisPoints);
             Cnv.Polyline(TmpPts, VisPoints);
             VisPoints := 0;
           end;
        end;
       if not (ccNotVisible in ClipRes) then
        begin
          TmpPt22D := TransformPoint2D(Point3DToPoint2D(TmpPt2), VT);
          TmpPts^[VisPoints] := Point2DToPoint(TmpPt22D);
          Inc(VisPoints);
        end;
      end;
     if (VisPoints > 0)  then
      Cnv.Polyline(TmpPts, VisPoints);
  finally
    FreeMem(TmpPts, AllocatedMem);
  end;
end;

function DrawLine3D(const Cnv: TDecorativeCanvas; P1, P2: TPoint3D; const NT: TTransf3D; const VT: TTransf2D): Boolean;
var
  TmpPt13D, TmpPt23D: TPoint3D;
  TmpPt1, TmpPt2: TPoint;
  ClipRes: TClipResult;
begin
  TmpPt13D := TransformPoint3D(P1, NT);
  TmpPt23D := TransformPoint3D(P2, NT);
  ClipRes := ClipLine3D(TmpPt13D, TmpPt23D);
  Result := not (ccNotVisible in ClipRes);
  if Result then
   begin
     TmpPt1 := Point2DToPoint(TransformPoint2D(Point3DToPoint2D(TmpPt13D), VT));
     TmpPt2 := Point2DToPoint(TransformPoint2D(Point3DToPoint2D(TmpPt23D), VT));
     Cnv.MoveTo(TmpPt1.X, TmpPt1.Y);
     Cnv.LineTo(TmpPt2.X, TmpPt2.Y);
   end;
end;

procedure DrawBoundingBox3D(const Cnv: TDecorativeCanvas; const VRP: TPoint3D; Box: TRect3D; const NT: TTransf3D; const VT: TTransf2D);
var
  Cont: Integer;
  VPN, N001, N010, N100: TVector3D;
  BoxSegments: array [1..12, 1..2] of TPoint3D;
  ViewedSegs: array [1..12] of Boolean;
begin
  // Determina i lati visibili

  // Creo i segmenti del box.
  with Box do
   begin
     BoxSegments[1][1] := Point3D(SecondEdge.X, SecondEdge.Y, SecondEdge.Z);
     BoxSegments[1][2] := Point3D(FirstEdge.X, SecondEdge.Y, SecondEdge.Z);
     BoxSegments[2][1] := BoxSegments[1][2];
     BoxSegments[2][2] := Point3D(FirstEdge.X, FirstEdge.Y, SecondEdge.Z);

     BoxSegments[3][1] := BoxSegments[2][2];
     BoxSegments[3][2] := Point3D(SecondEdge.X, FirstEdge.Y, SecondEdge.Z);

     BoxSegments[4][1] := BoxSegments[3][2];
     BoxSegments[4][2] := Point3D(SecondEdge.X, SecondEdge.Y, SecondEdge.Z);

     BoxSegments[9][1] := Point3D(SecondEdge.X, SecondEdge.Y, FirstEdge.Z);
     BoxSegments[9][2] := Point3D(FirstEdge.X, SecondEdge.Y, FirstEdge.Z);

     BoxSegments[10][1] := BoxSegments[9][2];
     BoxSegments[10][2] := Point3D(FirstEdge.X, FirstEdge.Y, FirstEdge.Z);

     BoxSegments[11][1] := BoxSegments[10][2];
     BoxSegments[11][2] := Point3D(SecondEdge.X, FirstEdge.Y, FirstEdge.Z);

     BoxSegments[12][1] := BoxSegments[11][2];
     BoxSegments[12][2] := Point3D(SecondEdge.X, SecondEdge.Y, FirstEdge.Z);

     BoxSegments[5][1] := BoxSegments[1][2];
     BoxSegments[5][2] := BoxSegments[9][2];

     BoxSegments[6][1] := BoxSegments[2][2];
     BoxSegments[6][2] := BoxSegments[10][2];

     BoxSegments[7][1] := BoxSegments[3][2];
     BoxSegments[7][2] := BoxSegments[11][2];

     BoxSegments[8][1] := BoxSegments[4][2];
     BoxSegments[8][2] := BoxSegments[12][2];
   end;

  if Cnv.Canvas.Pen.Mode = pmXOr then
   begin
     for Cont := 1 to 12 do
      ViewedSegs[Cont] := False;

     N001 := NormalizeVector3D(TransformVector3D(Versor3D(0, 0, 1), NT));
     N010 := NormalizeVector3D(TransformVector3D(Versor3D(0, 1, 0), NT));
     N100 := NormalizeVector3D(TransformVector3D(Versor3D(1, 0, 0), NT));
     { Casi particolari }
     if IsSamePoint3D(BoxSegments[1][1], BoxSegments[1][2]) then
      begin // Superfice in YZ
        VPN := N010;
        ViewedSegs[6] := VPN.Z <> -1.0;
        ViewedSegs[5] := VPN.Z <> 1.0;
        VPN := N001;
        ViewedSegs[10] := VPN.Z <> -1.0;
        ViewedSegs[2] := VPN.Z <> 1.0;
      end
     else if IsSamePoint3D(BoxSegments[2][1], BoxSegments[2][2]) then
      begin // Superfice in XZ
        VPN := N100;
        ViewedSegs[6] := VPN.Z <> -1.0;
        ViewedSegs[7] := VPN.Z <> 1.0;
        VPN := N001;
        ViewedSegs[11] := VPN.Z <> -1.0;
        ViewedSegs[3] := VPN.Z <> 1.0;
      end
     else if IsSamePoint3D(BoxSegments[5][1], BoxSegments[5][2]) then
      begin // Superfice in XY
        VPN := N100;
        ViewedSegs[10] := VPN.Z <> -1.0;
        ViewedSegs[12] := VPN.Z <> 1.0;
        VPN := N001;
        ViewedSegs[11] := VPN.Z <> -1.0;
        ViewedSegs[9] := VPN.Z <> 1.0;
      end
     else
      begin
        VPN := Reflect3D(N100);
        if VPN.Z < 0 then
         begin
           ViewedSegs[2] := True;
           ViewedSegs[6] := True;
           ViewedSegs[10] := True;
           ViewedSegs[5] := True;
         end
        else if VPN.Z > 0 then
         begin
           ViewedSegs[4] := True;
           ViewedSegs[7] := True;
           ViewedSegs[12] := True;
           ViewedSegs[8] := True;
         end;
        VPN := Reflect3D(N010);
        if VPN.Z < 0 then
         begin
           ViewedSegs[6] := True;
           ViewedSegs[3] := True;
           ViewedSegs[7] := True;
           ViewedSegs[11] := True;
         end
        else if VPN.Z > 0 then
         begin
           ViewedSegs[5] := True;
           ViewedSegs[1] := True;
           ViewedSegs[8] := True;
           ViewedSegs[9] := True;
         end;
        VPN := Reflect3D(N001);
        if VPN.Z < 0 then
         begin
           ViewedSegs[9] := True;
           ViewedSegs[10] := True;
           ViewedSegs[11] := True;
           ViewedSegs[12] := True;
         end
        else if VPN.Z > 0 then
         begin
           ViewedSegs[1] := True;
           ViewedSegs[2] := True;
           ViewedSegs[3] := True;
           ViewedSegs[4] := True;
         end;
      end;
   end
  else
   for Cont := 1 to 12 do
    ViewedSegs[Cont] := True;

  // Disegno i lati.
  for Cont := 1 to 12 do
   if ViewedSegs[Cont] then
    DrawLine3D(Cnv, BoxSegments[Cont][1], BoxSegments[Cont][2],  NT, VT);
end;

// =====================================================================
// TPointsSet2D
// =====================================================================

procedure TPointsSet2D.Expand(const NewCapacity: Integer);
var
  Cont: Integer;
begin
  if NewCapacity <= fCapacity then
   Exit;
  ReAllocMem(fPoints, NewCapacity * SizeOf(TPoint2D));
  for Cont := fCapacity to NewCapacity - 1 do
   with PVectPoints2D(FPoints)^[Cont] do
    begin
      X := 0.0;
      Y := 0.0;
      W := 1.0;
    end;
  fCapacity := NewCapacity;
end;

procedure TPointsSet2D.SetDisableEvents(B: Boolean);
begin
  fDisableEvents := B;
end;

procedure TPointsSet2D.CallOnChange;
begin
  // Call onChange only if the events are enabled.
  if (not fDisableEvents) and Assigned(fOnChange) then
   begin
     fDisableEvents := True;
     try
      fOnChange(Self);
     finally
      fDisableEvents := False;
     end;
   end;
end;

constructor TPointsSet2D.Create(const _Capacity: Word);
begin
  inherited Create;

  fCount := 0;
  fCapacity := _Capacity;
  GetMem(fPoints, fCapacity * SizeOf(TPoint2D));
  fDisableEvents := False;
  fGrownEnabled := True;
  fTag := 0;
end;

destructor TPointsSet2D.Destroy;
begin
  FreeMem(fPoints, fCapacity * SizeOf(TPoint2D));
  inherited Destroy;
end;

function TPointsSet2D.Get(Index: Word): TPoint2D;
begin
  if Index < fCount then
   Result := PVectPoints2D(FPoints)^[Index]
  else
   Raise ECADOutOfBound.Create('TPointsSet2D.Get: Index out of bound');
end;

function TPointsSet2D.GetExtension: TRect2D;
var
  Cont: Integer;
  TmpPt: TPoint2D;
begin
  Result := Rect2D(0, 0, 0, 0);
  if fCount = 0 then
   Exit;
  TmpPt := CartesianPoint2D(PVectPoints2D(fPoints)^[0]);
  Result.FirstEdge := TmpPt;
  Result.SecondEdge := TmpPt;
  for Cont := 1 to fCount - 1 do
   begin
     TmpPt := CartesianPoint2D(PVectPoints2D(fPoints)^[Cont]);
     if TmpPt.X > Result.Right then
      Result.Right := TmpPt.X
     else if TmpPt.X < Result.Left then
      Result.Left := TmpPt.X;
     if TmpPt.Y > Result.Top then
      Result.Top := TmpPt.Y
     else if TmpPt.Y < Result.Bottom then
      Result.Bottom := TmpPt.Y;
   end;
end;

procedure TPointsSet2D.PutProp(Index: Word; const Item: TPoint2D);
begin
  Put(Index, Index, Item);
  CallOnChange;
end;

procedure TPointsSet2D.Put(PutIndex, ItemIndex: Word; const Item: TPoint2D);
begin
  if PutIndex < fCapacity then
   begin
     PVectPoints2D(fPoints)^[PutIndex] := Item;
     if PutIndex >= fCount then
      fCount := PutIndex + 1;
   end
  else if fGrownEnabled then
   begin
     Expand(MaxIntValue([PutIndex + 1, fCapacity * 2 + 1]));
     PVectPoints2D(fPoints)^[PutIndex] := Item;
     Inc(fCount);
   end
  else
   Raise ECADOutOfBound.Create('TPointsSet2D.Put: Vector out of bound');
end;

procedure TPointsSet2D.Add(const Item: TPoint2D);
begin
  PutProp(fCount, Item);
end;

procedure TPointsSet2D.AddPoints(const Items: array of TPoint2D);
var
  Cont: Integer;
begin
  if (not fGrownEnabled) and (fCount + High(Items) - Low(Items) > fCapacity) then
   Raise ECADOutOfBound.Create('TPointsSet2D.AddPoints: Vector out of bound');
  Expand(fCount + High(Items) - Low(Items) + 1);
  for Cont := Low(Items) to High(Items) do
   Put(fCount, fCount, Items[Cont]);
  CallOnChange;
end;

procedure TPointsSet2D.AddPoints(const SourceSet: TPointsSet2D); overload;
var
  Cont: Integer;
begin
  if (not fGrownEnabled) and (fCount + SourceSet.Count > fCapacity) then
   Raise ECADOutOfBound.Create('TPointsSet2D.AddPoints: Vector out of bound');
  Expand(fCount + SourceSet.Count + 1);
  for Cont := 0 to SourceSet.Count - 1 do
   Put(fCount, fCount, SourceSet[Cont]);
  CallOnChange;
end;

procedure TPointsSet2D.Clear;
begin
  fCount := 0;
end;

procedure TPointsSet2D.Delete(const Index: Word);
var
  Cont: Integer;
begin
  if Index < fCount then
   begin
     for Cont := Index to fCount - 2 do
      Put(Cont, Cont + 1, PVectPoints2D(fPoints)^[Cont + 1]);
     Dec(fCount);
     CallOnChange;
   end
  else
   Raise ECADOutOfBound.Create('TPointsSet2D.Delete: Vector out of bound');
end;

procedure TPointsSet2D.Insert(const Index: Word; const Item: TPoint2D);
var
  Cont: Integer;
begin
  if Index >= fCount then
   Raise ECADOutOfBound.Create('TPointsSet2D.Insert: Vector out of bound');
  Put(fCount, fCount, Item);
  for Cont := fCount - 1 downto Index + 1 do
   Put(Cont, Cont - 1, PVectPoints2D(fPoints)^[Cont - 1]);
  Put(Index, Index, Item);
  CallOnChange;
end;

procedure TPointsSet2D.TransformPoints(const T: TTransf2D);
var
  Cont: Integer;
  TmpPt: TPoint2D;
begin
  for Cont := 0 to fCount - 1 do
   begin
     TmpPt := TransformPoint2D(PVectPoints2D(fPoints)^[Cont], T);
     PVectPoints2D(fPoints)^[Cont] := TmpPt;
   end;
  CallOnChange;
end;

procedure TPointsSet2D.Copy(const S: TPointsSet2D; const StIdx, EndIdx: Integer);
var
  Cont: Integer;
begin
  try
    Expand(EndIdx + 1);
    for Cont := StIdx to EndIdx do
     Put(Cont, Cont, S[Cont]);
  except
  end;
  CallOnChange;
end;

procedure TPointsSet2D.DrawAsPolygon(const Cnv: TDecorativeCanvas;
                                      const Clip, Extent: TRect2D;
                                      const S: TTransf2D);
begin
  Draw2DSubSetAsPolygon(fPoints, fCount, Cnv, Clip, Extent, S, 0, fCount - 1);
end;

procedure TPointsSet2D.DrawAsPolyline(const Cnv: TDecorativeCanvas;
                                       const Clip, Extent: TRect2D;
                                       const S: TTransf2D);
begin
  Draw2DSubSetAsPolyline(fPoints, fCount, Cnv, Clip, Extent, S, 0, fCount - 1, False);
end;

procedure TPointsSet2D.DrawSubSetAsPolygon(const Cnv: TDecorativeCanvas;
                                            const Clip, Extent: TRect2D;
                                            const S: TTransf2D;
                                            const StartIdx, EndIdx: Integer);
begin
  Draw2DSubSetAsPolygon(fPoints, fCount, Cnv, Clip, Extent, S, StartIdx, EndIdx);
end;

procedure TPointsSet2D.DrawSubSetAsPolyline(const Cnv: TDecorativeCanvas;
                                             const Clip, Extent: TRect2D;
                                             const S: TTransf2D;
                                             const StartIdx, EndIdx: Integer;
                                             const ToBeClosed: Boolean);
begin
  Draw2DSubSetAsPolyline(fPoints, fCount, Cnv, Clip, Extent, S, StartIdx, EndIdx, ToBeClosed);
end;

// =====================================================================
// TPointsSet3D
// =====================================================================

procedure TPointsSet3D.Expand(const NewCapacity: Integer);
var
  Cont: Integer;
begin
  if NewCapacity <= fCapacity then
   Exit;
  ReAllocMem(fPoints, NewCapacity * SizeOf(TPoint3D));
  for Cont := fCapacity to NewCapacity - 1 do
   with PVectPoints3D(FPoints)^[Cont] do
    begin
      X := 0;
      Y := 0;
      X := 0;
      W := 1.0;
    end;
  fCapacity := NewCapacity;
end;

procedure TPointsSet3D.CallOnChange;
begin
  if (not fDisableEvents) and Assigned(fOnChange) then
   begin
     fDisableEvents := True;
     try
      fOnChange(Self);
     finally
      fDisableEvents := False;
     end;
   end;
end;

constructor TPointsSet3D.Create(const _Capacity: Word);
begin
  inherited Create;
  fCount := 0;
  fCapacity := _Capacity;
  GetMem(fPoints, fCapacity * SizeOf(TPoint3D));
  fDisableEvents := False;
  fGrownEnabled := True;
  fTag := 0;
end;

destructor TPointsSet3D.Destroy;
begin
  FreeMem(fPoints, fCapacity * SizeOf(TPoint3D));
  inherited Destroy;
end;

function TPointsSet3D.Get(Index: Word): TPoint3D;
begin
  if Index < fCount then
   Result := PVectPoints3D(FPoints)^[Index]
  else
   Raise ECADOutOfBound.Create('TPointsSet3D.Get: Vector out of bound');
end;

function TPointsSet3D.GetExtension: TRect3D;
var
  Cont: Integer;
  TmpPt: TPoint3D;
begin
  Result := Rect3D(0, 0, 0, 0, 0, 0);
  if FCount = 0 then
   Exit;
  TmpPt := CartesianPoint3D(PVectPoints3D(FPoints)^[0]);
  Result.FirstEdge := TmpPt;
  Result.SecondEdge := TmpPt;
  for Cont := 1 to FCount - 1 do
   begin
     TmpPt := CartesianPoint3D(PVectPoints3D(FPoints)^[Cont]);
     if TmpPt.X > Result.SecondEdge.X then
      Result.SecondEdge.X := TmpPt.X
     else if TmpPt.X < Result.FirstEdge.X then
      Result.FirstEdge.X := TmpPt.X;
     if TmpPt.Y > Result.SecondEdge.Y then
      Result.SecondEdge.Y := TmpPt.Y
     else if TmpPt.Y < Result.FirstEdge.Y then
      Result.FirstEdge.Y := TmpPt.Y;
     if TmpPt.Z > Result.SecondEdge.Z then
      Result.SecondEdge.Z := TmpPt.Z
     else if TmpPt.Z < Result.FirstEdge.Z then
      Result.FirstEdge.Z := TmpPt.Z;
   end;
end;

procedure TPointsSet3D.PutProp(Index: Word; const Item: TPoint3D);
begin
  Put(Index, Index, Item);
  CallOnChange;
end;

procedure TPointsSet3D.Put(PutIndex, ItemIndex: Word; const Item: TPoint3D);
begin
  if PutIndex < fCapacity then
   begin
     PVectPoints3D(FPoints)^[PutIndex] := Item;
     if PutIndex >= FCount then FCount := PutIndex + 1;
   end
  else if FGrownEnabled then
   begin
     { Grow the vector }
     Expand(MaxIntValue([PutIndex + 1, fCapacity * 2 + 1]));
     PVectPoints3D(FPoints)^[PutIndex] := Item;
     Inc(FCount);
   end
  else
   Raise ECADOutOfBound.Create('TPointsSet3D.Put: Vector out of bound');
end;

procedure TPointsSet3D.Add(const Item: TPoint3D);
begin
  PutProp(FCount, Item);
end;

procedure TPointsSet3D.AddPoints(const Items: array of TPoint3D);
var
  Cont: Integer;
begin
  if (not FGrownEnabled) and (fCount + High(Items) - Low(Items) > fCapacity) then
   Raise ECADOutOfBound.Create('TPointsSet3D.AddPoints: Vector out of bound');
  Expand(fCount + High(Items) - Low(Items) + 1);
  for Cont := Low(Items) to High(Items) do
   Put(FCount, FCount, Items[Cont]);
  { Now post only one OnChange event. }
  CallOnChange;
end;

procedure TPointsSet3D.AddPoints(const SourceSet: TPointsSet3D); overload;
var
  Cont: Integer;
begin
  if (not fGrownEnabled) and (fCount + SourceSet.Count > fCapacity) then
   Raise ECADOutOfBound.Create('TPointsSet3D.AddPoints: Vector out of bound');
  Expand(fCount + SourceSet.Count + 1);
  for Cont := 0 to SourceSet.Count - 1 do
   Put(fCount, fCount, SourceSet[Cont]);
  CallOnChange;
end;

procedure TPointsSet3D.Clear;
begin
  FCount := 0;
end;

procedure TPointsSet3D.Delete(const Index: Word);
var
  Cont: Integer;
begin
  if Index < FCount then
   begin
     for Cont := Index to FCount - 2 do
      Put(Cont, Cont + 1, PVectPoints3D(FPoints)^[Cont + 1]);
     Dec(FCount);
     CallOnChange;
   end
  else
   Raise ECADOutOfBound.Create('TPointsSet3D.Delete: Vector out of bound');
end;

procedure TPointsSet3D.Insert(const Index: Word; const Item: TPoint3D);
var
  Cont: Integer;
begin
  if Index >= FCount then
   Raise ECADOutOfBound.Create('TPointsSet3D.Insert: Vector out of bound');
  { Make the space for the new item. }
  Put(FCount, FCount, Item);
  { Insert the item. }
  for Cont := FCount - 1 downto Index + 1 do
   Put(Cont, Cont - 1, PVectPoints3D(FPoints)^[Cont - 1]);
  Put(Index, Index, Item);
  CallOnChange;
end;

procedure TPointsSet3D.TransformPoints(const T: TTransf3D);
var
  Cont: Integer;
  TmpPt: TPoint3D;
begin
  for Cont := 0 to fCount - 1 do
   begin
     TmpPt := TransformPoint3D(PVectPoints3D(fPoints)^[Cont], T);
     PVectPoints3D(fPoints)^[Cont] := TmpPt;
   end;
  CallOnChange;
end;

procedure TPointsSet3D.FrontFace(const N: TVector3D);
var
  Cont, Max: Integer;
  SwapPt: TPoint3D;
begin
  if DotProduct3D(GetVectNormal(fPoints, fCount), N) < 0 then
   begin
     Max := fCount - 1;
     for Cont := 0 to Max div 2 do
      begin
        SwapPt := PVectPoints3D(fPoints)^[Cont];
        Put(Cont, Max - Cont, PVectPoints3D(fPoints)^[Max - Cont]);
        Put(Max - Cont, Cont, SwapPt);
      end;
   end;
end;

procedure TPointsSet3D.Copy(const S: TPointsSet3D; const StIdx, EndIdx: Integer);
var
  Cont: Integer;
begin
  try
    Expand(EndIdx + 1);
    for Cont := StIdx to EndIdx do
     Put(Cont, Cont, S[Cont]);
    CallOnChange;
  except
  end;
end;

procedure TPointsSet3D.DrawAsPolyline(const Cnv: TDecorativeCanvas;
                                 const Extent: TRect3D;
                                 const NT: TTransf3D;
                                 const VT: TTransf2D);
begin
  Draw3DSubSetAsPolyline(fPoints, fCount, Cnv, Extent, NT, VT, 0, fCount - 1, False);
end;

procedure TPointsSet3D.DrawSubSetAsPolyline(const Cnv: TDecorativeCanvas;
                                             const Extent: TRect3D;
                                             const NT: TTransf3D;
                                             const VT: TTransf2D;
                                             const StartIdx, EndIdx: Integer;
                                             const ToBeClosed: Boolean);
begin
  Draw3DSubSetAsPolyline(fPoints, fCount, Cnv, Extent, NT, VT, StartIdx, EndIdx, ToBeClosed);
end;

// =====================================================================
// TGraphicObject 
// =====================================================================

constructor TGraphicObject.Create(ID: LongInt);
begin
  inherited Create;

  fID := ID;
  fLayer := 0;
  fVisible := True;
  fEnabled := True;
  fToBeSaved := True;
  fTag := 0;
  fOnChange := nil;
end;

constructor TGraphicObject.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  BitMask: Byte;
begin
  inherited Create;
  with Stream do
   begin
     Read(fID, SizeOf(fID));
     Read(fLayer, SizeOf(fLayer));
     BitMask := 0;
     Read(BitMask, SizeOf(BitMask))
   end;
  fVisible := (BitMask and 1) = 1;
  fEnabled := (BitMask and 2) = 2;
  fToBeSaved := not ((BitMask and 4) = 4);
  fTag := 0;
  fOnChange := nil;
end;

procedure TGraphicObject.SaveToStream(const Stream: TStream);
var
  BitMask: Byte;
begin
  BitMask := 0;
  if fVisible then BitMask := BitMask or 1;
  if fEnabled then BitMask := BitMask or 2;
  if not fToBeSaved then
   BitMask := BitMask or 4; // Uso il not per compatibilità con le versioni precedenti.
  with Stream do
   begin
     Write(fID, SizeOf(fID));
     Write(fLayer, SizeOf(fLayer));
     Write(BitMask, SizeOf(BitMask));
   end;
end;

procedure TGraphicObject.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  fVisible := Obj.Visible;
  fEnabled := Obj.Enabled;
  fToBeSaved := Obj.ToBeSaved;
end;

procedure TGraphicObject.UpdateExtension(Sender: TObject);
begin
  _UpdateExtension;
  if Assigned(fOnChange) then
   fOnChange(Self);
end;

// =====================================================================
// Registration functions
// =====================================================================

function CADSysFindClassIndex(const Name: String): Word;
var
  Cont: Integer;
begin
  for Cont := 0 to MAX_REGISTERED_CLASSES - 1 do
    if Assigned(GraphicObjectsRegistered[Cont]) and
       (GraphicObjectsRegistered[Cont].ClassName = Name) then
     begin
       Result := Cont;
       Exit;
     end;
  Raise ECADObjClassNotFound.Create('CADSysFindClassIndex: ' + Name + ' graphic class not found');
end;

function CADSysFindClassByName(const Name: String): TGraphicObjectClass;
begin
  Result := GraphicObjectsRegistered[CADSysFindClassIndex(Name)];
end;

function CADSysFindClassByIndex(Index: Word): TGraphicObjectClass;
begin
  if Index >= MAX_REGISTERED_CLASSES then
   Raise ECADOutOfBound.Create('CADSysRegisterClass: Out of bound registration index');
  if not Assigned(GraphicObjectsRegistered[Index]) then
   Raise ECADObjClassNotFound.Create('CADSysFindClassByIndex: Index not registered');
  Result := GraphicObjectsRegistered[Index];
end;

procedure CADSysRegisterClass(Index: Word; const GraphClass: TGraphicObjectClass);
begin
  if Index >= MAX_REGISTERED_CLASSES then
   Raise ECADOutOfBound.Create('CADSysRegisterClass: Out of bound registration index');
  if Assigned(GraphicObjectsRegistered[Index]) then
   Raise ECADObjClassNotFound.Create(Format('CADSysRegisterClass: Index %d already allocated by %s', [Index, GraphicObjectsRegistered[Index].ClassName]));
  GraphicObjectsRegistered[Index] := GraphClass;
end;

procedure CADSysUnregisterClass(Index: Word);
begin
  if Index >= MAX_REGISTERED_CLASSES then
   Raise ECADOutOfBound.Create('CADSysRegisterClass: Out of bound registration index');
  if Assigned(GraphicObjectsRegistered[Index]) then
   GraphicObjectsRegistered[Index] := nil;
end;

procedure CADSysInitClassRegister;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_CLASSES do
   GraphicObjectsRegistered[Cont] := nil;
end;

// =====================================================================
// TGraphicObjList
// =====================================================================

constructor TGraphicObjList.Create;
begin
  inherited Create;

  fListGuard := TCADSysCriticalSection.Create;
  fHead := nil;
  fTail := nil;
  fIterators := 0;
  fHasExclusive := False;
  fCount := 0;
  fFreeOnClear := True;
end;

destructor TGraphicObjList.Destroy;
var
  TmpBlock: PObjBlock;
begin
  while fHead <> nil do
   begin
     try
       if fFreeOnClear and Assigned(TObjBlock(fHead^).Obj) then
        TObjBlock(FHead^).Obj.Free;
     except
     end;
     TmpBlock := fHead;
     fHead := TObjBlock(fHead^).Next;
     FreeMem(TmpBlock, SizeOf(TObjBlock));
   end;
  fListGuard.Free;
  inherited;
end;

function TGraphicObjList.GetHasIter: Boolean;
begin
  Result := fIterators > 0;
end;

function TGraphicObjList.GetIterator: TGraphicObjIterator;
begin
  if fHasExclusive then
   Raise ECADListBlocked.Create('TGraphicObjList.GetIterator: The list has exclusive iterator.');
  Result := TGraphicObjIterator.Create(Self);
end;

function TGraphicObjList.GetExclusiveIterator: TExclusiveGraphicObjIterator;
begin
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.GetExclusiveIterator: The list has active iterators.');
  Result := TExclusiveGraphicObjIterator.Create(Self);
end;

function TGraphicObjList.GetPrivilegedIterator: TExclusiveGraphicObjIterator;
begin
  Result := TExclusiveGraphicObjIterator.Create(Self);
end;

procedure TGraphicObjList.RemoveAllIterators;
begin
  fListGuard.Enter;
  try
    fIterators := 0;
    fHasExclusive := False;
  finally
    fListGuard.Leave;
  end;
end;

procedure TGraphicObjList.Add(const Obj: TGraphicObject);
var
  NewBlock: PObjBlock;
begin
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.Add: The list has active iterators.');
  fListGuard.Enter;
  try
    { Allocate new blocks. }
    GetMem(NewBlock, SizeOf(TObjBlock));
    { Initialize the block. }
    NewBlock^.Prev := fTail;
    NewBlock^.Next := nil;
    NewBlock^.Obj := Obj;
    if fHead = nil then
     fHead := NewBlock;
    if fTail <> nil then
     TObjBlock(fTail^).Next := NewBlock;
    fTail := NewBlock;
    Inc(fCount);
  finally
    fListGuard.Leave;
  end;
end;

procedure TGraphicObjList.AddFromList(const Lst: TGraphicObjList);
var
  TmpIter: TGraphicObjIterator;
begin
  if Lst.Count = 0 then
   Exit;
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.AddFromList: The list has active iterators.');
  // Alloca un iterator locale
  fListGuard.Enter;
  try
    TmpIter := Lst.GetIterator;
    try
      repeat
       Add(TmpIter.Current);
      until TmpIter.Next = nil;
    finally
      TmpIter.Free;
    end;
  finally
    fListGuard.Leave;
  end;
end;

procedure TGraphicObjList.Insert(const IDInsertPoint: LongInt; const Obj: TGraphicObject);
var
  NewBlock: PObjBlock;
  InsertPoint: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  { Safe guard. }
  if fHead = nil then
   Raise ECADSysException.Create('TGraphicObjList.Insert: No objects in the list');
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.Insert: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    InsertPoint := TmpIter.SearchBlock(IDInsertPoint);
    if (InsertPoint = nil) then
     Raise ECADListObjNotFound.Create('TGraphicObjList.Insert: Object not found');
    { Allocate new blocks. }
    GetMem(NewBlock, SizeOf(TObjBlock));
    { Initialize the block. }
    NewBlock^.Prev := InsertPoint^.Prev;
    NewBlock^.Next := InsertPoint;
    NewBlock^.Obj := Obj;
    if InsertPoint^.Prev <> nil then
     InsertPoint^.Prev^.Next := NewBlock
    else
     fHead := NewBlock;
    InsertPoint^.Prev := NewBlock;
    Inc(fCount);
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.Move(const IDToMove, IDInsertPoint: LongInt);
var
  InsertPoint, ToMove: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  { Safe guard. }
  if fHead = nil then
   Raise ECADSysException.Create('TGraphicObjList.Move: No objects in the list');
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.Move: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    { Check if the current and insert point are the same. }
    if IDInsertPoint = IDToMove then
     Raise ECADSysException.Create('TGraphicObjList.Move: Bad object to move');
    InsertPoint := TmpIter.SearchBlock(IDInsertPoint);
    ToMove := TmpIter.SearchBlock(IDToMove);
    if (InsertPoint = nil) then
     Raise ECADListObjNotFound.Create('TGraphicObjList.Move: Insertion point not found');
    if (ToMove = nil) then
     Raise ECADListObjNotFound.Create('TGraphicObjList.Move: Object to move not found');
    { Now I have the block to move and the insertion point. }
    if ToMove^.Prev <> nil then
     ToMove^.Prev^.Next := ToMove^.Next
    else
     fHead := ToMove^.Next;
    if ToMove^.Next <> nil then
     ToMove^.Next^.Prev := ToMove^.Prev
    else
     fTail := ToMove^.Prev;
    { Set new link. }
    if InsertPoint^.Prev <> nil then
     InsertPoint^.Prev^.Next := ToMove
    else
     fHead := ToMove;
    ToMove^.Next := InsertPoint;
    ToMove^.Prev := InsertPoint^.Prev;
    InsertPoint^.Prev := ToMove;
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.DeleteBlock(ObjToDel: Pointer);
begin
  fListGuard.Enter;
  try
    // Free the first object. So if it cannot be deleted it will be later.
    if Assigned(TObjBlock(ObjToDel^).Obj) and FFreeOnClear then
     TObjBlock(ObjToDel^).Obj.Free;
    // First extract the block from all list.
    if TObjBlock(ObjToDel^).Next <> nil then
     TObjBlock(ObjToDel^).Next^.Prev := TObjBlock(ObjToDel^).Prev
    else
     { I reached the Last block. }
     fTail := TObjBlock(ObjToDel^).Prev;
    if TObjBlock(ObjToDel^).Prev <> nil then
     TObjBlock(ObjToDel^).Prev^.Next := TObjBlock(ObjToDel^).Next
    else
     { I reached the head. }
     fHead := TObjBlock(ObjToDel^).Next;
    FreeMem(ObjToDel, SizeOf(TObjBlock));
    Dec(fCount);
  finally
    fListGuard.Leave;
  end;
end;

function TGraphicObjList.Delete(const ID: LongInt): Boolean;
var
  ObjToDel: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  Result := False;
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.Delete: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    ObjToDel := TmpIter.SearchBlock(ID);
    if ObjToDel = nil then
     Raise ECADListObjNotFound.Create('TGraphicObjList.Delete: Object not found');
    DeleteBlock(ObjToDel);
    Result := True;
  finally
    TmpIter.Free;
  end;
end;

function  TGraphicObjList.Find(const ID: LongInt): TGraphicObject;
var
  FoundObj: PObjBlock;
  TmpIter: TGraphicObjIterator;
begin
  Result := nil;
  // Alloca un iterator locale
  TmpIter := GetIterator;
  try
    FoundObj := TmpIter.SearchBlock(ID);
    if FoundObj = nil then
     Exit;
    Result := TObjBlock(FoundObj^).Obj;
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.RemoveBlock(ObjToDel: Pointer);
begin
  fListGuard.Enter;
  try
    // First extract the block from all list.
    if TObjBlock(ObjToDel^).Next <> nil then
     TObjBlock(ObjToDel^).Next^.Prev := TObjBlock(ObjToDel^).Prev
    else
     { I reached the Last block. }
     fTail := TObjBlock(ObjToDel^).Prev;
    if TObjBlock(ObjToDel^).Prev <> nil then
     TObjBlock(ObjToDel^).Prev^.Next := TObjBlock(ObjToDel^).Next
    else
     { I reached the head. }
     fHead := TObjBlock(ObjToDel^).Next;
    FreeMem(ObjToDel, SizeOf(TObjBlock));
    Dec(fCount);
  finally
   fListGuard.Leave;
  end;
end;

function TGraphicObjList.Remove(const ID: LongInt): Boolean;
var
  ObjToDel: PObjBlock;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  Result := False;
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.Remove: The list has active iterators.');
  // Alloca un iterator locale
  TmpIter := GetExclusiveIterator;
  try
    ObjToDel := TmpIter.SearchBlock(ID);
    if ObjToDel = nil then
     Raise ECADListObjNotFound.Create('TGraphicObjList.Remove: No object found');
    RemoveBlock(ObjToDel);
    Result := True;
  finally
    TmpIter.Free;
  end;
end;

procedure TGraphicObjList.Clear;
var
  TmpBlock: PObjBlock;
begin
  if fIterators > 0 then
   Raise ECADListBlocked.Create('TGraphicObjList.Clear: The list has active iterators.');
  fListGuard.Enter;
  try
    while fHead <> nil do
     begin
       try
         if fFreeOnClear and Assigned(TObjBlock(fHead^).Obj) then
          TObjBlock(FHead^).Obj.Free;
       except
       end;
       TmpBlock := fHead;
       fHead := TObjBlock(fHead^).Next;
       FreeMem(TmpBlock, SizeOf(TObjBlock));
     end;
    fHead := nil;
    fTail := nil;
    fCount := 0;
  finally
    fListGuard.Leave;
  end;
end;

// =====================================================================
// TGraphicObjIterator
// =====================================================================

{ Search the block corresponding to ID. }
function TGraphicObjIterator.SearchBlock(ID: LongInt): Pointer;
begin
  Result := fSourceList.fHead;
  while (Result <> nil) and (TObjBlock(Result^).Obj.ID <> ID) do
   begin
     if TObjBlock(Result^).Prev = fSourceList.fTail then
      begin
        Result := nil;
        Break;
      end;
     Result := TObjBlock(Result^).Next;
   end;
end;

function TGraphicObjIterator.GetCurrentObject: TGraphicObject;
begin
  if fCurrent <> nil then
   Result := TObjBlock(fCurrent^).Obj
  else
   Result := nil;
end;

constructor TGraphicObjIterator.Create(const Lst: TGraphicObjList);
begin
  inherited Create;

  Lst.fListGuard.Enter;
  try
    fCurrent := nil;
    fSourceList := Lst;
    if fSourceList <> nil then
     begin
       Inc(fSourceList.fIterators);
       fCurrent := fSourceList.fHead;
     end;
  finally
    Lst.fListGuard.Leave;
  end;
end;

destructor TGraphicObjIterator.Destroy;
begin
  fSourceList.fListGuard.Enter;
  try
    if fSourceList <> nil then
     begin
       Dec(fSourceList.fIterators);
       if fSourceList.fIterators < 0 then
        fSourceList.fIterators := 0;
     end;
  finally
    fSourceList.fListGuard.Leave;
  end;
  inherited;
end;

function TGraphicObjIterator.GetCount: Integer;
begin
  if (fSourceList = nil) then
   Result := 0
  else
   Result := fSourceList.Count;
end;

function TGraphicObjIterator.Search(const ID: LongInt): TGraphicObject;
var
  TmpBlock: PObjBlock;
begin
  Result := nil;
  if (fSourceList = nil) then
   Exit;
  TmpBlock := SearchBlock(ID);
  if TmpBlock = nil then
   Exit;
  Result := TmpBlock^.Obj;
  fCurrent := TmpBlock;
end;

function TGraphicObjIterator.Next: TGraphicObject;
begin
  Result := nil;
  if (fSourceList = nil) and (fCurrent = nil) then
   Exit;
  { Check if the End of the list is reached. }
  if fCurrent = Pointer(fSourceList.fTail) then
   begin
     fCurrent := nil;
     Exit;
   end;
  fCurrent := TObjBlock(fCurrent^).Next;
  Result := TObjBlock(fCurrent^).Obj;
end;

function TGraphicObjIterator.Prev: TGraphicObject;
begin
  Result := nil;
  if (fSourceList = nil) and (fCurrent = nil) then
   Exit;
  { Check if the start of the list is reached. }
  if fCurrent = Pointer(fSourceList.fHead) then
   begin
     fCurrent := nil;
     Exit;
   end;
  fCurrent := TObjBlock(fCurrent^).Prev;
  Result := TObjBlock(fCurrent^).Obj;
end;

function TGraphicObjIterator.First: TGraphicObject;
begin
  Result := nil;
  if fSourceList = nil then
   Exit;
  fCurrent := fSourceList.fHead;
  if fCurrent = nil then
   Exit;
  Result := TObjBlock(fCurrent^).Obj;
end;

function TGraphicObjIterator.Last: TGraphicObject;
begin
  Result := nil;
  if fSourceList = nil then
   Exit;
  fCurrent := fSourceList.fTail;
  if fCurrent = nil then
   Exit;
  Result := TObjBlock(FCurrent^).Obj;
end;

// =====================================================================
// TExclusiveGraphicObjIterator
// =====================================================================

constructor TExclusiveGraphicObjIterator.Create(const Lst: TGraphicObjList);
begin
  inherited Create(Lst);
  Lst.fListGuard.Enter;
  try
    if fSourceList <> nil then
     fSourceList.fHasExclusive := True;
  finally
    Lst.fListGuard.Leave;
  end;
end;

destructor TExclusiveGraphicObjIterator.Destroy;
begin
  fSourceList.fListGuard.Enter;
  try
    if fSourceList <> nil then
     fSourceList.fHasExclusive := False;
  finally
    fSourceList.fListGuard.Leave;
  end;
  inherited;
end;

procedure TExclusiveGraphicObjIterator.DeleteCurrent;
var
  TmpObj: Pointer;
begin
  if (fSourceList <> nil) and (fCurrent <> nil) then
   begin
     if TObjBlock(fCurrent^).Next <> nil then
      TmpObj := TObjBlock(fCurrent^).Next
     else
      TmpObj := TObjBlock(fCurrent^).Prev;
     fSourceList.DeleteBlock(fCurrent);
     fCurrent := TmpObj;
   end;
end;

procedure TExclusiveGraphicObjIterator.RemoveCurrent;
var
  TmpObj: Pointer;
begin
  if (fSourceList <> nil) and (fCurrent <> nil) then
   begin
     if TObjBlock(fCurrent^).Next <> nil then
      TmpObj := TObjBlock(fCurrent^).Next
     else
      TmpObj := TObjBlock(fCurrent^).Prev;
     fSourceList.RemoveBlock(fCurrent);
     fCurrent := TmpObj;
   end;
end;

// =====================================================================
// TIndexedObjectList
// =====================================================================

type
  TIdxObjListArray = array[0..0] of TObject;

procedure TIndexedObjectList.SetListSize(N: Integer);
var
  Cont: Integer;
begin
  if N <> fNumOfObject then
   begin
     if fFreeOnClear then
      for Cont := N to fNumOfObject - 1 do
       GetObject(Cont).Free;
     ReallocMem(fListMemory, N * SizeOf(TObject));
     for Cont := fNumOfObject to N - 1 do
      TIdxObjListArray(fListMemory^)[Cont] := nil;
     if N = 0 then
      fListMemory := nil;
     fNumOfObject := N;
   end;
end;

procedure TIndexedObjectList.SetObject(Idx: Integer; Obj: TObject);
begin
  if not Assigned(fListMemory) then
   Exit;
  if Idx < fNumOfObject then
   TIdxObjListArray(fListMemory^)[Idx] := Obj
  else
   Raise ECADOutOfBound.Create('TIndexedObjectList.SetObject: Index out of bounds.');
end;

function TIndexedObjectList.GetObject(Idx: Integer): TObject;
begin
  Result := nil;
  if not Assigned(fListMemory) then
   Exit;
  if Idx < fNumOfObject then
   Result := TIdxObjListArray(fListMemory^)[Idx]
  else
   Raise ECADOutOfBound.Create('TIndexedObjectList.GetObject: Index out of bounds.');
end;

constructor TIndexedObjectList.Create(const Size: Integer);
begin
  inherited Create;

  fFreeOnClear := True;
  SetListSize(Size);
end;

destructor TIndexedObjectList.Destroy;
begin
  Clear;
  FreeMem(fListMemory, fNumOfObject * SizeOf(TObject));
  inherited;
end;

procedure TIndexedObjectList.Clear;
var
  Cont: Integer;
begin
  if fFreeOnClear then
   for Cont := 0 to fNumOfObject - 1 do
    begin
      GetObject(Cont).Free;
      SetObject(Cont, nil);
    end;
end;

// =====================================================================
// TPaintingThread
// =====================================================================

constructor TPaintingThread.Create(const Owner: TCADViewport; const ARect: TRect2D);
begin
  inherited Create(True); // Sempre disattivo alla partenza.

  FreeOnTerminate := True;
  fOwner := Owner;
  fRect := ARect;
end;

destructor TPaintingThread.Destroy;
begin
  DoTerminate;
  inherited;
end;

procedure TPaintingThread.Execute;
var
  Tmp: TGraphicObject;
  TmpIter: TGraphicObjIterator;
  Cont: Integer;
  TmpCanvas: TCanvas;
  TmpClipRect: TRect2D;
begin
  if not Assigned(fOwner) then
   Exit;
  with fOwner do
   try
     TmpCanvas := OffScreenCanvas.Canvas;
     TmpClipRect := RectToRect2D(ClientRect);
     TmpCanvas.Lock;
     try
       if fShowGrid and not fGridOnTop then
        DrawGrid(fRect, TmpCanvas);
       if (fViewportObjects <> nil) then
        TmpIter := fViewportObjects.GetIterator
       else if not Assigned(fCADCmp) or fCADCmp.IsBlocked then
        Exit
       else
        TmpIter := FCADCmp.GetListOfObjects;
       try
         Tmp := TmpIter.First;
         if fDrawMode = DRAWMODE_NODRAW then
          Exit;
         Cont := 0;
         if fCopingFrequency > 0 then
          while Tmp <> nil do
           begin
             DrawObject(Tmp, OffScreenCanvas, TmpClipRect);
             Inc(Cont);
             if Cont >= fCopingFrequency then
              begin
                Synchronize(DoCopyCanvasThreadSafe);
                Cont := 0;
              end;
             Tmp := TmpIter.Next;
             if Terminated then
              Break;
           end
         else
          while Tmp <> nil do
           begin
             DrawObject(Tmp, OffScreenCanvas, TmpClipRect);
             Tmp := TmpIter.Next;
             if Terminated then
              Break;
           end;
       finally
         TmpIter.Free;
       end;
       if fShowGrid and fGridOnTop then
        DrawGrid(fRect, TmpCanvas);
     finally
       TmpCanvas.UnLock;
     end;
   except
   end;
  Synchronize(fOwner.DoCopyCanvasThreadSafe);
end;

// =====================================================================
// TLayer
// =====================================================================

procedure TLayer.Changed(Sender: TObject);
begin
  FModified := True;
end;

procedure TLayer.SetName(Nm: TLayerName);
begin
  if fName <> Nm then
   begin
     FName := Nm;
     FModified := True;
   end;
end;

procedure TLayer.SetPen(Pn: TPen);
begin
  if Assigned(Pn) then
   begin
     FPen.Assign(Pn);
     FModified := True;
   end;
end;

procedure TLayer.SetBrush(Br: TBrush);
begin
  if Assigned(Br) then
   begin
     FBrush.Assign(Br);
     FModified := True;
   end;
end;

constructor TLayer.Create(Idx: Byte);
begin
  inherited Create;
  fIdx := Idx;
  fName := Format('Layer %d', [Idx]);
  FPen := TPen.Create;
  FPen.Color := clBlack;
  FPen.Style := psSolid;
  FBrush := TBrush.Create;
  FBrush.Color := clWhite;
  FBrush.Style := bsSolid;
  fDecorativePen := TDecorativePen.Create;
  FActive := True;
  fVisible := True;
  FOpaque := False;
  fStreamable := True;
  FModified := False;
  FPen.OnChange := Changed;
  FBrush.OnChange := Changed;
  fTag := 0;
end;

destructor TLayer.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  fDecorativePen.Free;
  inherited Destroy;
end;

procedure TLayer.SaveToStream(const Stream: TStream);
var
  TmpColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenMode: TPenMode;
  TmpWidth: Integer;
  TmpBrushStyle: TBrushStyle;
  TmpBool: Boolean;
begin
  TmpColor := fPen.Color;
  Stream.Write(TmpColor, SizeOf(TmpColor));
  TmpPenStyle := fPen.Style;
  Stream.Write(TmpPenStyle, SizeOf(TmpPenStyle));
  TmpPenMode := fPen.Mode;
  Stream.Write(TmpPenMode, SizeOf(TmpPenMode));
  TmpWidth := fPen.Width;
  Stream.Write(TmpWidth, SizeOf(TmpWidth));

  TmpColor := fBrush.Color;
  Stream.Write(TmpColor, SizeOf(TmpColor));
  TmpBrushStyle := fBrush.Style;
  Stream.Write(TmpBrushStyle, SizeOf(TmpBrushStyle));

  TmpWidth := fDecorativePen.PatternLenght;
  Stream.Write(TmpWidth, SizeOf(TmpWidth));
  for TmpWidth := 0 to fDecorativePen.PatternLenght - 1 do
   begin
     TmpBool := fDecorativePen.PenStyle[TmpWidth];
     Stream.Write(TmpBool, SizeOf(TmpBool));
   end;

  Stream.Write(fActive, SizeOf(Boolean));
  Stream.Write(fVisible, SizeOf(Boolean));
  Stream.Write(fOpaque, SizeOf(Boolean));
  Stream.Write(fStreamable, SizeOf(Boolean));
  Stream.Write(fName, SizeOf(TLayerName));
end;

procedure TLayer.LoadFromStream(const Stream: TStream;
                                const Version: TCADVersion);
var
  TmpColor: TColor;
  TmpPenStyle: TPenStyle;
  TmpPenMode: TPenMode;
  TmpCont, TmpWidth: Integer;
  TmpBrushStyle: TBrushStyle;
  TmpBool: Boolean;
begin
  Stream.Read({%H-}TmpColor, SizeOf(TmpColor));
  fPen.Color := TmpColor;
  Stream.Read({%H-}TmpPenStyle, SizeOf(TmpPenStyle));
  fPen.Style := TmpPenStyle;
  Stream.Read({%H-}TmpPenMode, SizeOf(TmpPenMode));
  fPen.Mode := TmpPenMode;
  TmpWidth := 0;
  Stream.Read(TmpWidth, SizeOf(TmpWidth));
  fPen.Width := TmpWidth;

  Stream.Read(TmpColor, SizeOf(TmpColor));
  fBrush.Color := TmpColor;
  Stream.Read({%H-}TmpBrushStyle, SizeOf(TmpBrushStyle));
  fBrush.Style := TmpBrushStyle;
  if( Version < 'CAD422' ) then
   Stream.Read(TmpColor, SizeOf(TmpColor));

  if (Version >= 'CAD41') then
   begin
     Stream.Read(TmpWidth, SizeOf(TmpWidth));
     for TmpCont := 0 to TmpWidth - 1 do
      begin
        TmpBool := False;
        Stream.Read(TmpBool, SizeOf(TmpBool));
        fDecorativePen.PenStyle[TmpCont] := TmpBool;
      end;
   end;

  Stream.Read(fActive, SizeOf(Boolean));
  if (Version >= 'CAD4') then
   Stream.Read(fVisible, SizeOf(Boolean))
  else
   fVisible := True;
  Stream.Read(fOpaque, SizeOf(Boolean));
  if (Version = 'CAD3  ') then
   begin
     Stream.Read(fStreamable, SizeOf(Boolean));
     fName := Format('Layer %d', [fIdx]);
   end
  else if Version >= 'CAD33' then
   begin
     Stream.Read(fStreamable, SizeOf(Boolean));
     Stream.Read(fName, SizeOf(TLayerName));
   end
  else
   begin
     fStreamable := True;
     fName := Format('Layer %d', [fIdx]);
   end;
  fModified := True;
end;

function TLayer.SetCanvas(const Cnv: TDecorativeCanvas): Boolean;
begin
  Result := fVisible;
  if not fVisible then
   Exit;
  with Cnv do
   begin
     Canvas.Pen.Assign(Pen);
     Canvas.Brush.Assign(Brush);
     Canvas.Font.Color := Pen.Color;
     if not fOpaque then
      LCLIntf.SetBkMode(Canvas.Handle, LCLType.TRANSPARENT)
     else
      LCLIntf.SetBkMode(Canvas.Handle, LCLType.OPAQUE);
   end;
  Cnv.DecorativePen.Assign(fDecorativePen);
end;

function TLayers.GetLayerByName(const Nm: TLayerName): TLayer;
var
  Cont: Integer;
begin
  Result := nil;
  for Cont := 0 to 255 do
   if Nm = TLayer(Flayers.Items[Cont]).fName then
    Result := TLayer(Flayers.Items[Cont]);
end;

function TLayers.GetLayer(Index: Byte): TLayer;
begin
  Result := TLayer(Flayers.Items[Index]);
end;

constructor TLayers.Create;
var
  Cont: Byte;
  TmpLay: TLayer;
begin
  inherited Create;
  FLayers := TList.Create;
  for Cont := 0 to 255 do
   begin
     TmpLay := TLayer.Create(Cont);
     TmpLay.fName := Format('Layer %d', [Cont]);
     FLayers.Add(TmpLay);
   end;
end;

destructor TLayers.Destroy;
begin
  while FLayers.Count > 0 do
   begin
     TLayer(FLayers.Items[0]).Free;
     FLayers.Delete(0);
   end;
  FLayers.Free;
  inherited Destroy;
end;

procedure TLayers.SaveToStream(const Stream: TStream);
var
  Cont: Word;
begin
  Cont := 1;
  Stream.Write(Cont, SizeOf(Cont));
  for Cont := 0 to 255 do
   with Stream do
    if TLayer(FLayers[Cont]).fModified then
     begin
       Write(Cont, SizeOf(Cont));
       TLayer(FLayers[Cont]).SaveToStream(Stream);
     end;
  Cont := 256;
  Stream.Write(Cont, SizeOf(Cont));
end;

procedure TLayers.LoadFromStream(const Stream: TStream;
                                 const Version: TCADVersion);
var
  Cont: Word;
begin
  Cont := 0;
  Stream.Read(Cont, SizeOf(Cont));
  if Cont <> 1 then Raise
   ECADFileNotValid.Create('TLayers.LoadFromStream: No layers found');
  while Stream.Position < Stream.Size do
   with Stream do
    begin
      Read(Cont, SizeOf(Cont));
      if Cont = 256 then Break;
      TLayer(FLayers[Cont]).LoadFromStream(Stream, Version);
    end;
end;

function TLayers.SetCanvas(const Cnv: TDecorativeCanvas;
                           const Index: Byte): Boolean;
begin
  Result := TLayer(Flayers.Items[Index]).SetCanvas(Cnv);
end;

// =====================================================================
// TCADCmp
// =====================================================================

function TCADCmp.GetObjectsCount: Integer;
begin
  Result := fListOfObjects.Count;
end;

function TCADCmp.GetSourceBlocksCount: Integer;
begin
  Result := fListOfBlocks.Count;
end;

function TCADCmp.GetListOfObjects: TGraphicObjIterator;
begin
  Result := fListOfObjects.GetIterator;
end;

function TCADCmp.GetListOfBlocks: TGraphicObjIterator;
begin
  Result := fListOfBlocks.GetIterator;
end;

procedure TCADCmp.SetListOfObjects(NL: TGraphicObjList);
begin
  if HasIterators then
   Raise ECADSysException.Create('TCADCmp.SetListOfObjects: Cannot change ObjectList when the current one has active iterators.');
  if Assigned(NL) then
   begin
     DeleteAllObjects;
     fListOfObjects.Free;
     fListOfObjects := NL;
   end;
end;

procedure TCADCmp.SetListOfBlocks(NL: TGraphicObjList);
begin
  if HasIterators then
   Raise ECADSysException.Create('TCADCmp.SetListOfBlocks: Cannot change BlocksList when the current one has active iterators.');
  if Assigned(NL) then
   begin
     DeleteAllSourceBlocks;
     fListOfBlocks.Free;
     fListOfBlocks := NL;
   end;
end;

function TCADCmp.GetExclusiveListOfObjects: TExclusiveGraphicObjIterator;
begin
  Result := fListOfObjects.GetExclusiveIterator;
end;

function TCADCmp.GetExclusiveListOfBlocks: TExclusiveGraphicObjIterator;
begin
  Result := fListOfBlocks.GetExclusiveIterator;
end;

function TCADCmp.GetIsBlocked: Boolean;
begin
  Result := fListOfObjects.HasExclusiveIterators or fListOfBlocks.HasExclusiveIterators;
end;

function TCADCmp.GetHasIterators: Boolean;
begin
  Result := fListOfObjects.HasIterators or fListOfBlocks.HasIterators;
end;

function TCADCmp.GetViewport(Idx: Integer): TCADViewport;
begin
  Result := TCADViewport(fListOfViewport[Idx]);
end;

function TCADCmp.GetViewportsCount: Integer;
begin
  Result := fListOfViewport.Count;
end;

constructor TCADCmp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fListOfObjects := TGraphicObjList.Create;
  fListOfObjects.FreeOnClear := True;
  fListOfBlocks := TGraphicObjList.Create;
  fListOfBlocks.FreeOnClear := True;

  fListOfViewport := TList.Create;
  fLayers := TLayers.Create;
  fCurrentLayer := 0;
  fRepaintAfterTransform := True;
  fDrawOnAdd := False;
  fNextID := 0;
  fNextBlockID := 0;
  fVersion := CADSysVersion;
end;

// Used because BlockList can contain circular references.
procedure TCADCmp.DeleteAllSourceBlocks;
var
  TmpObj: TGraphicObject;
  Deleted: Integer;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fListOfBlocks.GetExclusiveIterator;
  try
   repeat
     Deleted := 0;
     TmpObj := TmpIter.Last;
     while Assigned(TmpObj) do
      begin
       try
         TmpIter.DeleteCurrent;
         Inc(Deleted);
         TmpObj := TmpIter.Current;
       except
         TmpObj := TmpIter.Prev;
       end;
     end;
     if (Deleted = 0) and (fListOfBlocks.Count > 0) then
      begin
        Raise ECADSourceBlockIsReferenced.Create('TCADCmp.FreeSourceBlocks: CADSys 4.0 Severe error'#10#13'The drawing contains circular reference of source blocks. They will be not deleted !');
        Exit;
      end;
   until TmpIter.Count = 0;
  finally
    TmpIter.Free;
  end;
end;

procedure TCADCmp.DeleteAllObjects;
begin
  try
   fListOfObjects.Clear;
  except
  end;
  fNextID := 0;
end;

destructor TCADCmp.Destroy;
var
  Cont: Integer;
begin
  DeleteAllObjects;
  FListOfObjects.Free;
  DeleteAllSourceBlocks;
  FListOfBlocks.Free;
  for Cont := 0 to fListOfViewport.Count - 1 do
   TCADViewport(FListOfViewport[Cont]).fCADCmp := nil;
  FListOfViewport.Free;
  FLayers.Free;
  inherited Destroy;
end;

procedure TCADCmp.SaveToStream(const Stream: TStream);
var
  TmpByte: Byte;
begin
  with Stream do
   begin
     { Write the signature for the version. }
     Write(FVersion, SizeOf(FVersion));
     { Save the layer informations. }
     FLayers.SaveToStream(Stream);
     { Save the source blocks. }
     TmpByte := 2;
     Write(TmpByte, SizeOf(TmpByte));
     SaveBlocksToStream(Stream, False);
     { Save the objects. }
     TmpByte := 3;
     Write(TmpByte, SizeOf(TmpByte));
     SaveObjectsToStream(Stream);
   end;
end;

procedure TCADCmp.MergeFromStream(const Stream: TStream);
var
  TmpVersion: TCADVersion;
  TmpByte: Byte;
  TmpBool: Boolean;
begin
  with Stream do
   begin
     Read({%H-}TmpVersion, SizeOf(TmpVersion));
     TmpBool := TmpVersion = CADSysVersion;
     if (not TmpBool) and Assigned(fOnVerError) then
      fOnVerError(Self, stDrawing, Stream, TmpBool)
     else if (not TmpBool) and Assigned(fOnVerErrorEx) then
      fOnVerErrorEx(Self, stDrawing, Stream, TmpVersion, TmpBool)
     else if (not TmpBool) and (Copy(TmpVersion, 1, 3) = 'CAD') then
      begin
        { Se la versione del file è CAD422 allora eseguo la conversione a CAD423 automaticamente.
          Funziona solo per la versione CAD422 con TRealType=Single }
        TmpBool := True;
      end;
     if TmpBool then
      begin
        { Load the layer informations. }
        FLayers.LoadFromStream(Stream, TmpVersion);
        { Load the source blocks. }
        TmpByte := 0;
        Read(TmpByte, SizeOf(TmpByte));
        if TmpByte <> 2 then
         Raise ECADFileNotValid.Create('TCADCmp.MergeFromStream: no blocks found');
        LoadBlocksFromStream(Stream, TmpVersion);
        { Load the objects. }
        Read(TmpByte, SizeOf(TmpByte));
        if TmpByte <> 3 then
         Raise ECADFileNotValid.Create('TCADCmp.MergeFromStream: no objects found');
        LoadObjectsFromStream(Stream, TmpVersion);
      end
     else
      Raise ECADFileNotValid.Create('TCADCmp.MergeFromStream: Invalid stream version.');
   end;
end;

procedure TCADCmp.LoadFromStream(const Stream: TStream);
begin
  { Delete all objects. }
  DeleteAllObjects;
  DeleteSavedSourceBlocks;
  MergeFromStream(Stream);
end;

procedure TCADCmp.LoadLibrary(const Stream: TStream);
var
  TmpVersion: TCADVersion;
  TmpByte: Byte;
  TmpBool: Boolean;
begin
  with Stream do
   begin
     Read({%H-}TmpVersion, SizeOf(TmpVersion));
     TmpBool := TmpVersion = CADSysVersion;
     if (not TmpBool) and Assigned(fOnVerError) then
      fOnVerError(Self, stLibrary, Stream, TmpBool)
     else if (not TmpBool) and Assigned(fOnVerErrorEx) then
      fOnVerErrorEx(Self, stLibrary, Stream, TmpVersion, TmpBool)
     else if (not TmpBool) and (Copy(TmpVersion, 1, 3) = 'CAD') then
      begin
        { Se la versione del file è CAD422 allora eseguo la conversione a CAD423 automaticamente.
          Funziona solo per la versione CAD422 con TRealType=Single }
        TmpBool := True;
      end;
     if TmpBool then
      begin
        { Load the source blocks. }
        TmpByte := 0;
        Read(TmpByte, SizeOf(TmpByte));
        if TmpByte <> 2 then
         Raise ECADFileNotValid.Create('TCADCmp.LoadLibrary: no blocks found');
        LoadBlocksFromStream(Stream, TmpVersion);
      end
     else
      Raise ECADFileNotValid.Create('TCADCmp.LoadLibrary: Invalid stream version.');
   end;
end;

procedure TCADCmp.SaveLibrary(const Stream: TStream);
var
  TmpByte: Byte;
begin
  with Stream do
   begin
     { Write the signature for the version. }
     Write(FVersion, SizeOf(FVersion));
     { Save the source blocks. }
     TmpByte := 2;
     Write(TmpByte, SizeOf(TmpByte));
     SaveBlocksToStream(Stream, True);
   end;
end;

procedure TCADCmp.MergeFromFile(const FileName: String);
var
  TmpStr: TFileStream;
begin
  TmpStr := TFileStream.Create(FileName, fmOpenRead);
  try
    MergeFromStream(TmpStr);
    RepaintViewports;
  finally
    TmpStr.Free;
  end;
end;

procedure TCADCmp.LoadFromFile(const FileName: String);
var
  TmpStr: TFileStream;
begin
  TmpStr := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(TmpStr);
    RepaintViewports;
  finally
    TmpStr.Free;
  end;
end;

procedure TCADCmp.SaveToFile(const FileName: String);
var
  TmpStr: TFileStream;
begin
  TmpStr := TFileStream.Create(FileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(TmpStr);
  finally
    TmpStr.Free;
  end;
end;

function TCADCmp.AddSourceBlock(ID: LongInt; const Obj: TGraphicObject): TGraphicObject;
begin
  Result := Obj;
  if ID < 0 then
   begin
     ID := FNextBlockID;
     Inc(FNextBlockID);
   end
  else
   if ID >= FNextBlockID then FNextBlockID := ID + 1;
  Obj.fOwnerCAD := Self;
  Obj.Layer := FCurrentLayer;
  Obj.ID := ID;
  Obj.UpdateExtension(Self);
  fListOfBlocks.Add(Obj);
end;

function TCADCmp.GetSourceBlock(ID: LongInt): TGraphicObject;
var
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := GetListOfBlocks;
  try
   Result := TmpIter.Search(ID);
  finally
   TmpIter.Free;
  end;
end;

function TCADCmp.FindSourceBlock(const SrcName: TSourceBlockName): TGraphicObject;
begin
  Result := nil;
end;

procedure TCADCmp.DeleteSourceBlockByID(const ID: LongInt);
var
  TmpObj: TGraphicObject;
begin
  TmpObj := GetSourceBlock(ID);
  if TmpObj = nil then
   Raise ECADListObjNotFound.Create('TCADCmp.DeleteSourceBlock: No source block found');
  try
   fListOfBlocks.Delete(ID);
  except
  end;
end;

function TCADCmp.AddObject(ID: LongInt; const Obj: TGraphicObject): TGraphicObject;
begin
  Result := Obj;
  if ID < 0 then
   begin
     ID := FNextID;
     Inc(FNextID);
   end
  else
   if ID >= FNextID then FNextID := ID + 1;
  Obj.fOwnerCAD := Self;
  Obj.Layer := FCurrentLayer;
  Obj.ID := ID;
  Obj.UpdateExtension(Self);
  FListOfObjects.Add(Obj);
  if FDrawOnAdd then
   RedrawObject(Obj);
  if Assigned(FOnAddObject) then
   FOnAddObject(Self, Obj);
end;

function TCADCmp.InsertObject(ID, IDInsertPoint: LongInt; const Obj: TGraphicObject): TGraphicObject;
begin
  Result := Obj;
  FListOfObjects.Insert(IDInsertPoint, Obj);
  if ID < 0 then
   begin
     ID := FNextID;
     Inc(FNextID);
   end
  else
   if ID > FNextID then FNextID := ID;
  Obj.fOwnerCAD := Self;
  Obj.Layer := FCurrentLayer;
  Obj.ID := ID;
  Obj.UpdateExtension(Self);
  if FDrawOnAdd then
   RedrawObject(Obj);
  if Assigned(FOnAddObject) then
   FOnAddObject(Self, Obj);
end;

procedure TCADCmp.MoveObject(const IDOrigin, IDDestination: LongInt);
begin
  FListOfObjects.Move(IDOrigin, IDDestination);
end;

procedure TCADCmp.RemoveObject(const ID: LongInt);
var
  TmpObj: TGraphicObject;
begin
  TmpObj := GetObject(ID);
  if TmpObj = nil then
   Raise ECADListObjNotFound.Create('TCADCmp.RemoveObject: No object found');
  TmpObj.fOwnerCAD := nil;
  fListOfObjects.Remove(ID);
end;

procedure TCADCmp.DeleteObject(const ID: LongInt);
var
  TmpObj: TGraphicObject;
begin
  TmpObj := GetObject(ID);
  if TmpObj = nil then
   Raise ECADListObjNotFound.Create('TCADCmp.DeleteObject: No object found');
  fListOfObjects.Delete(ID);
end;

procedure TCADCmp.ChangeObjectLayer(const ID: LongInt; const NewLayer: Byte);
var
  TmpObj: TGraphicObject;
begin
  TmpObj := GetObject(ID);
  if TmpObj = nil then
   Raise ECADListObjNotFound.Create('TCADCmp.ChangeObjectLayer: No object found');
  TmpObj.Layer := NewLayer;
end;

function  TCADCmp.GetObject(ID: LongInt): TGraphicObject;
var
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := GetListOfObjects;
  try
   Result := TmpIter.Search(ID);
  finally
   TmpIter.Free;
  end;
end;

procedure TCADCmp.AddViewports(const VP: TCADViewport);
begin
  if not Assigned(VP) then Exit;
  FListOfViewport.Add(VP);
end;

procedure TCADCmp.DelViewports(const VP: TCADViewport);
var
  Cont: Integer;
begin
  if not Assigned(VP) then Exit;
  Cont := FListOfViewport.IndexOf(VP);
  if Cont >= 0 then FListOfViewport.Delete(Cont);
end;

procedure TCADCmp.RepaintViewports;
var
  Cont: Integer;
begin
  for Cont := 0 to FListOfViewport.Count - 1 do
   TCADViewport(FListOfViewport.Items[Cont]).RePaint;
end;

procedure TCADCmp.RefreshViewports;
var
  Cont: Integer;
begin
  for Cont := 0 to FListOfViewport.Count - 1 do
   TCADViewport(FListOfViewport.Items[Cont]).Refresh;
end;

procedure TCADCmp.RedrawObject(const Obj: TGraphicObject);
var
  Cont: Integer;
begin
  for Cont := 0 to FListOfViewport.Count - 1 do
   with TCADViewport(FListOfViewport.Items[Cont]) do
    begin
      DrawObject(Obj, OffScreenCanvas, RectToRect2D(ClientRect));
      Refresh;
    end;
end;

procedure TCADCmp.SetDefaultBrush(const Brush: TBrush);
var
  Cont: Byte;
begin
  for Cont := 0 to 255 do
   if not fLayers[Cont].Modified then
    begin
      fLayers[Cont].Brush.Assign(Brush);
      fLayers[Cont].fModified := False;
    end;
end;

procedure TCADCmp.SetDefaultPen(const Pen: TPen);
var
  Cont: Byte;
begin
  for Cont := 0 to 255 do
   if not fLayers[Cont].Modified then
    begin
      fLayers[Cont].Pen.Assign(Pen);
      fLayers[Cont].fModified := False;
    end;
end;

procedure TCADCmp.SetDefaultLayersColor(const C: TColor);
var
  Cont: Byte;
begin
  for Cont := 0 to 255 do
   if not fLayers[Cont].Modified then
    begin
      fLayers[Cont].Pen.Color := C;
      fLayers[Cont].fModified := False;
    end;
end;

function TCADCmp.GetDefaultLayersColor: TColor;
begin
  Result := fLayers[0].Pen.Color;
end;

// =====================================================================
// TCADViewport
// =====================================================================

procedure TCADViewport.CopyBitmapOnCanvas(const DestCnv: TCanvas; const Bmp: TBitmap; IRect: TRect; IsTransparent: Boolean; TransparentColor: TColor);
begin
  if( IsTransparent and
      not (csDesigning in ComponentState) ) then
   begin
     DestCnv.Brush.Style := bsClear;
     DestCnv.BrushCopy(IRect, Bmp, IRect, TransparentColor);
   end
  else
   DestCnv.CopyRect(IRect, Bmp.Canvas, IRect);
end;

procedure TCADViewport.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
   begin
     Style := Style and not (CS_HREDRAW or CS_VREDRAW);
     if fTransparent and not(csDesigning in ComponentState) then
      begin
        Style := Style and not WS_CLIPCHILDREN;
        Style := Style and not WS_CLIPSIBLINGS;
        Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
      end;
   end;
end;

function TCADViewport.CreateOffScreenCanvas(const Cnv: TCanvas): TDecorativeCanvas;
begin
  Result := TDecorativeCanvas.Create(Cnv);
end;

procedure TCADViewport.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if (not Assigned(fOnClear)) and
     (not fTransparent) or
     (csDesigning in ComponentState) then
   inherited
  else
   Message.Result := 1;
end;

//procedure TCADViewport.WMSize(var Message: TLMSize);
//begin
//  inherited;
//  DoResize;
//end;

procedure TCADViewport.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DoResize;
end;

procedure TCADViewport.CMMouseEnter(var Message: TLMessage);
begin
  inherited;
  if Assigned(fOnMouseEnter) then
   fOnMouseEnter(Self);
end;

procedure TCADViewport.CMMouseLeave(var Message: TLMessage);
begin
  inherited;
  if Assigned(fOnMouseLeave) then
   fOnMouseLeave(Self);
end;

procedure TCADViewport.SetDeltaX(const V: TRealType);
begin
  if fGridDeltaX <> V then
   begin
     StopRepaint;
     fGridDeltaX := V;
     Repaint;
   end;
end;

procedure TCADViewport.SetDeltaY(const V: TRealType);
begin
  if fGridDeltaY <> V then
   begin
     StopRepaint;
     fGridDeltaY := V;
     Repaint;
   end;
end;

procedure TCADViewport.SetSubX(const V: TRealType);
begin
  if fGridSubX <> V then
   begin
     StopRepaint;
     fGridSubX := V;
     Repaint;
   end;
end;

procedure TCADViewport.SetSubY(const V: TRealType);
begin
  if fGridSubY <> V then
   begin
     StopRepaint;
     fGridSubY := V;
     Repaint;
   end;
end;

procedure TCADViewport.SetBackColor(const Cl: TColor);
begin
  if FBackGroundColor <> Cl then
   begin
     StopRepaint;
     FBackGroundColor := Cl;
     fRubberPen.Color := fRubberPenColor xor Cl;
     Repaint;
   end;
end;

procedure TCADViewport.SetTransparent(const B: Boolean);
begin
  if( fTransparent <> B ) then
   begin
     StopRepaint;
     fTransparent := B;
     Repaint;
   end;
end;

procedure TCADViewport.SetOnClearCanvas(const H: TClearCanvas);
begin
  StopRepaint;
  fOnClear := H;
  Repaint;
end;

procedure TCADViewport.SetShowGrid(const B: Boolean);
begin
  if fShowGrid <> B then
   begin
     StopRepaint;
     fShowGrid := B;
     Repaint;
   end;
end;

procedure TCADViewport.SetGridColor(const Cl: TColor);
begin
  if FGridColor <> Cl then
   begin
     StopRepaint;
     FGridColor := Cl;
     Repaint;
   end;
end;

procedure TCADViewport.SetCADCmp(Cad: TCADCmp);
begin
  if Cad <> fCADCmp then
   begin
     StopRepaint;
     if Assigned(Cad) and not (csDesigning in ComponentState) then
      begin
        if Assigned(FCADCmp) then
         FCADCmp.DelViewports(Self);
        Cad.AddViewports(Self);
      end;
     fCADCmp := Cad;
   end;
end;

procedure TCADViewport.ClearCanvas(Sender: TObject; Cnv: TCanvas; const ARect: TRect2D; const BackCol: TColor);
begin
  with Cnv do
   begin
     Brush.Color := BackCol;
     Brush.Style := bsSolid;
     with TransformRect2D(ARect, fViewportToScreen) do
      FillRect(Rect(Round(Left), Round(Top), Round(Right) + 1, Round(Bottom) + 1));
   end;
end;

procedure TCADViewport.DoCopyCanvas(const GenEvent: Boolean);
begin
  CopyBackBufferRectOnCanvas(Rect(0, 0, Width, Height), GenEvent);
end;

procedure TCADViewport.DoCopyCanvasThreadSafe;
begin
  CopyBackBufferRectOnCanvas(Rect(0, 0, Width, Height), False);
end;

procedure TCADViewport.CopyBackBufferRectOnCanvas(const Rect: TRect; const GenEvent: Boolean);
begin
  if HandleAllocated then
   begin
     fOffScreenCanvas.Canvas.Lock;
     try
       CopyBitmapOnCanvas(Canvas, fOffScreenBitmap, Rect, fTransparent, fBackGroundColor);
     finally
       fOffScreenCanvas.Canvas.UnLock;
     end;
   end;
  if Assigned(fOnPaint) and GenEvent and (not fDisablePaintEvent) then
   try
     fDisablePaintEvent := True;
     fOnPaint(Self);
   finally
    fDisablePaintEvent := False;
   end;
end;

constructor TCADViewport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fViewGuard := TCADSysCriticalSection.Create;
  ControlStyle := ControlStyle - [csOpaque];
  ControlStyle := ControlStyle + [csClickEvents, csSetCaption, csDoubleClicks];
  fPaintingThread := nil;
  CopingFrequency := 0;
  fDrawMode := DRAWMODE_NORMAL;
  fOffScreenBitmap := TBitmap.Create;
  fOffScreenBitmap.PixelFormat := pf24bit;
  Height := 50;
  Width := 50;
  FOnClear := nil;
  FBackGroundColor := clSilver;
  FGridColor := clGray;
  fShowGrid := False;
  fTransparent := False;
  fInUpdate := False;
  fGridDeltaX := 0.0;
  fGridDeltaY := 0.0;
  fGridSubX := 0.0;
  fGridSubY := 0.0;
  fViewportObjects := nil;
  fDisableMouseEvents := False;
  FCADCmp := nil;
  FShowControlPoints := False;
  FControlPointsWidth := 6;
  FVisualWindow := Rect2D(0.0, 0.0, 100.0, 100.0);
  FScreenToViewport := IdentityTransf2D;
  FViewportToScreen := IdentityTransf2D;
  fRubberPen := TPen.Create;
  fUseThread := False;
  with fRubberPen do
   begin
     Mode := pmXOr;
     Style := psSolid;
     Width := 2;
   end;
  SetRubberColor(clRed);
  FAspectRatio := 0.0;
  fGridOnTop := False;
  //fOffScreenBitmap.Height := Height;
  //fOffScreenBitmap.Width := Width;
  fOffScreenCanvas := CreateOffScreenCanvas(fOffScreenBitmap.Canvas);
  fOnScreenCanvas := TDecorativeCanvas.Create(Canvas);
end;

destructor TCADViewport.Destroy;
begin
  StopPaintingThread;
  fRubberPen.Free;
  if Assigned(fCADCmp) and (not (csDesigning in ComponentState)) then
   fCADCmp.DelViewports(Self);
  fViewGuard.Free;
  fOffScreenCanvas.Free;
  fOnScreenCanvas.Free;
  fOffScreenCanvas := nil;
  fOnScreenCanvas := nil;
  fOffScreenBitmap.Free;
  fOffScreenBitmap := nil;
  inherited Destroy;
end;

procedure TCADViewport.BeginUpdate;
begin
  StopRepaint;
  fInUpdate := True;
end;

procedure TCADViewport.EndUpdate;
begin
  fInUpdate := False;
  RePaint;
end;

procedure TCADViewport.SetRubberColor(const Cl: TColor);
begin
  if Cl <> fRubberPenColor then
   begin
     StopRepaint;
     fRubberPenColor := Cl;
     fRubberPen.Color := Cl xor fBackgroundColor;
   end;
end;

procedure TCADViewport.Paint;
begin
  if fPaintingThread = nil then
   DoCopyCanvas(True);
end;

procedure TCADViewport.RefreshRect(const ARect: TRect);
var
  TmpRect: TRect;
begin
  if fPaintingThread <> nil then
   Exit;
  TmpRect := Rect(ARect.Left - 1, ARect.Top - 1, ARect.Right + 1, ARect.Bottom + 1);
  CopyBackBufferRectOnCanvas(TmpRect, True);
end;

procedure TCADViewport.DoResize;
begin
  StopRepaint;
  fOffScreenBitmap.Height := Height;
  fOffScreenBitmap.Width := Width;
  ChangeViewportTransform(fVisualWindow);
  if Assigned(fOnResize) then
   FOnResize(Self);
end;

procedure TCADViewport.StopPaintingThread;
begin
  if Assigned(fPaintingThread) then
   try
     TPaintingThread(fPaintingThread).Terminate;
     if Assigned(fPaintingThread) then
      TPaintingThread(fPaintingThread).WaitFor;
     fPaintingThread := nil;
     if Assigned(fOnPaint) and (not fDisablePaintEvent) then
      try
        fDisablePaintEvent := True;
        fOnPaint(Self);
      finally
       fDisablePaintEvent := False;
      end;
   finally
     fPaintingThread := nil;
   end;
end;

procedure TCADViewport.OnThreadEnded(Sender: TObject);
begin
  fPaintingThread := nil;
  if Assigned(fOnPaint) and (not fDisablePaintEvent) then
   try
     fDisablePaintEvent := True;
     fOnPaint(Self);
   finally
    fDisablePaintEvent := False;
   end;
  if Assigned(fOnEndRedraw) then
   fOnEndRedraw(Self);
end;

procedure TCADViewport.UpdateViewport(const ARect: TRect2D);
var
  Tmp: TGraphicObject;
  TmpIter: TGraphicObjIterator;
  TmpCanvas: TCanvas;
  TmpClipRect: TRect2D;
  Cont: Integer;
begin
  if fInUpdate then
   Exit;
  StopRepaint;
  if Assigned(fOnClear) then
   fOnClear(Self, fOffScreenCanvas.Canvas, ARect, fBackGroundColor)
  else
   ClearCanvas(Self, fOffScreenCanvas.Canvas, ARect, fBackGroundColor);
  if Assigned(FOnBeginRedraw) then
   fOnBeginRedraw(Self);
  if fUseThread then
   begin
     fPaintingThread := TPaintingThread.Create(Self, ARect);
     TPaintingThread(fPaintingThread).OnTerminate := OnThreadEnded;
     TPaintingThread(fPaintingThread).Start;
   end
  else
   try
    try
      TmpCanvas := fOffScreenCanvas.Canvas;
      TmpClipRect := RectToRect2D(ClientRect);
      TmpCanvas.Lock;
      try
        if fShowGrid and not fGridOnTop then
         DrawGrid(ARect, TmpCanvas);
        if fDrawMode = DRAWMODE_NODRAW then
         Exit;
        if (fViewportObjects <> nil) then
         TmpIter := fViewportObjects.GetIterator
        else if not Assigned(fCADCmp) or fCADCmp.IsBlocked then
         Exit
        else
         TmpIter := FCADCmp.GetListOfObjects;
        Tmp := TmpIter.First;
        try
          Cont := 0;
          if fCopingFrequency > 0 then
           while Tmp <> nil do
            begin
              DrawObject(Tmp, fOffScreenCanvas, TmpClipRect);
              Inc(Cont);
              if Cont = fCopingFrequency then
               begin
                 DoCopyCanvas(False);
                 Cont := 0;
               end;
              Tmp := TmpIter.Next;
            end
          else
           while Tmp <> nil do
            begin
              DrawObject(Tmp, fOffScreenCanvas, TmpClipRect);
              Tmp := TmpIter.Next;
            end;
        finally
          TmpIter.Free;
        end;
        if fShowGrid and fGridOnTop then
         DrawGrid(ARect, TmpCanvas);
      finally
        TmpCanvas.UnLock;
      end;
    except
    end;
   finally
     DoCopyCanvas(True);
     if Assigned(fOnEndRedraw) then
      fOnEndRedraw(Self);
   end;
end;

procedure TCADViewport.RepaintRect(const ARect: TRect2D);
begin
  UpdateViewport(ARect);
end;

procedure TCADViewport.DrawGrid(const ARect: TRect2D; const Cnv: TCanvas);
var
  CurrX, CurrY: TRealType;
  Pnt1, Pnt2: TPoint2D;
procedure DrawGridLines(const DecCnv: TDecorativeCanvas; const DX, DY: TRealType);
begin
  CurrX := Round(ARect.Left / DX) * DX;
  CurrY := Round(ARect.Bottom / DY) * DY;
  Pnt1 := ViewportToScreen(Point2D(CurrX, CurrY));
  Pnt2 := ViewportToScreen(Point2D(CurrX + DX, CurrY + DY));
  if (Pnt2.X - Pnt1.X > 10.0) and (Pnt1.Y - Pnt2.Y > 10.0) then
   begin
     while (CurrY <= ARect.Top) do
      begin
        Pnt1 := ViewportToScreen(Point2D(ARect.Left, CurrY));
        Pnt2 := ViewportToScreen(Point2D(ARect.Right, CurrY));
        DecCnv.MoveTo(Round(Pnt1.X), Round(Pnt1.Y));
        DecCnv.LineTo(Round(Pnt2.X), Round(Pnt2.Y));
        CurrY := CurrY + DY;
      end;
     while (CurrX <= ARect.Right) do
      begin
        Pnt1 := ViewportToScreen(Point2D(CurrX, ARect.Top));
        Pnt2 := ViewportToScreen(Point2D(CurrX, ARect.Bottom));
        DecCnv.MoveTo(Round(Pnt1.X), Round(Pnt1.Y));
        DecCnv.LineTo(Round(Pnt2.X), Round(Pnt2.Y));
        CurrX := CurrX + DX;
      end;
   end;
end;
var
  TmpCnv: TDecorativeCanvas;
begin
  if (fGridDeltaX <= 0.0) or (fGridDeltaY <= 0.0) then
   Exit;
  TmpCnv := TDecorativeCanvas.Create(Cnv);
  with Cnv do
   try
     SetBkMode(Handle, TRANSPARENT);
     Pen.Color := fGridColor;
     Pen.Width := 1;
     Pen.Mode := pmCopy;
     Pen.Style := psSolid;
     Brush.Color := fBackGroundColor;
     Brush.Style := bsSolid;
     // Draw the grid subdivisions if any.
     if( (fGridSubX > 0) and (fGridSubY > 0) ) then
      begin
        TmpCnv.DecorativePen.SetPenStyle('10');
        DrawGridLines(TmpCnv, fGridDeltaX / fGridSubX, fGridDeltaY / fGridSubY);
        TmpCnv.DecorativePen.SetPenStyle('');
      end;
     // Draw the grid main divisions.
     Pen.Style := psSolid;
     DrawGridLines(TmpCnv, fGridDeltaX, fGridDeltaY);
     // Draw the main axes.
     Pen.Width := 2;
     Pnt1 := ViewportToScreen(Point2D(ARect.Left, 0.0));
     Pnt2 := ViewportToScreen(Point2D(ARect.Right, 0.0));
     MoveTo(Round(Pnt1.X), Round(Pnt1.Y));
     LineTo(Round(Pnt2.X), Round(Pnt2.Y));
     Pnt1 := ViewportToScreen(Point2D(0.0, ARect.Bottom));
     Pnt2 := ViewportToScreen(Point2D(0.0, ARect.Top));
     MoveTo(Round(Pnt1.X), Round(Pnt1.Y));
     LineTo(Round(Pnt2.X), Round(Pnt2.Y));
   finally
     TmpCnv.Free;
   end;
end;

function TCADViewport.GetInRepaint: Boolean;
begin
  Result := Assigned(fPaintingThread);
end;

procedure TCADViewport.Repaint;
begin
  if (csReadingState in ControlState) then
   Exit;
  if fTransparent and Assigned(Parent) then
   Parent.Repaint;
  RepaintRect(FVisualWindow);
end;

procedure TCADViewport.WaitForRepaintEnd;
begin
  if Assigned(fPaintingThread) then
   try
     // Devo aspettare.
     TPaintingThread(fPaintingThread).WaitFor;
   finally
     fPaintingThread := nil;
   end;
end;

procedure TCADViewport.StopRepaint;
begin
  StopPaintingThread;
end;

procedure TCADViewport.Refresh;
begin
  Invalidate;
end;

procedure TCADViewport.Invalidate;
begin
  if (csReadingState in ControlState) or
     (csLoading in ComponentState) then
   begin
     inherited;
     Exit;
   end;
  if( csDesigning in ComponentState ) then
   inherited;
  if HandleAllocated then
   Paint;
end;

procedure TCADViewport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = CADCmp) and (Operation = opRemove) then
   begin
     StopPaintingThread;
     CADCmp := nil;
   end;
  inherited Notification(AComponent, Operation);
end;

procedure TCADViewport.ChangeViewportTransform(ViewWin: TRect2D);
var
  OldView: TRect2D;
begin
  StopRepaint;
  ViewWin := ReOrderRect2D(ViewWin);
  OldView := fVisualWindow;
  if (ViewWin.Bottom = ViewWin.Top) or (ViewWin.Right = ViewWin.Left) then
   Exit;
  try
    fViewGuard.Enter;
    try
      fVisualWindow := ViewWin;
      UpdateViewportTransform;
    finally
     fViewGuard.Leave;
    end;
  except
   on Exception do begin
     fVisualWindow := OldView;
     try
      UpdateViewportTransform;
     except
     end;
   end;
  end;
end;

procedure TCADViewport.UpdateViewportTransform;
var
  NewTransf: TTransf2D;
begin
  StopRepaint;
  try
    NewTransf := BuildViewportTransform(fVisualWindow, ClientRect, fAspectRatio);
    fScreenToViewport := InvertTransform2D(NewTransf);
    fViewportToScreen := NewTransf;
    if Assigned(fOnViewMappingChanged) then
     fOnViewMappingChanged(Self);
  except
  end;
  Repaint;
end;

function TCADViewport.BuildViewportTransform(var ViewWin: TRect2D;
                                             const ScreenWin: TRect;
                                             const AspectRatio: TRealType): TTransf2D;
begin
  Result := IdentityTransf2D;
end;

procedure TCADViewport.ZoomWindow(const NewWindow: TRect2D);
begin
  ChangeViewportTransform(NewWindow);
end;

function TCADViewport.GetAperture(const L: Word): TRealType;
begin
  Result := GetPixelAperture.X * L;
end;

function TCADViewport.SetLayer(const L: TLayer): Boolean;
begin
  if( L <> nil ) then
   Result := L.SetCanvas(fOffScreenCanvas)
  else
   Result := False;
end;

procedure TCADViewport.CalibrateCnv(const Cnv: TCanvas; XScale, YScale: TRealType);
var
  TmpWin: TRect2D;
  TmpAspect, LogWidth, LogHeight: TRealType;
begin
  StopRepaint;
  if (XScale = 0) or (YScale = 0) then Exit;
  TmpWin := fVisualWindow;
  // Questa è la dimensione di un pixel in mm.
  LogHeight := GetDeviceCaps(Cnv.Handle, VERTSIZE) / GetDeviceCaps(Cnv.Handle, VERTRES);
  LogWidth := GetDeviceCaps(Cnv.Handle, HORZSIZE) / GetDeviceCaps(Cnv.Handle, HORZRES);
  try
    TmpAspect := LogHeight / LogWidth;
    TmpWin.Right := fVisualWindow.Left + LogWidth * Width * XScale;
    TmpWin.Top := fVisualWindow.Bottom + LogHeight * TmpAspect * Height * YScale;
    ChangeViewportTransform(TmpWin);
  except
  end;
end;

procedure TCADViewport.Calibrate(const XScale, YScale: TRealType);
begin
  CalibrateCnv(Canvas, XScale, YScale);
end;

procedure TCADViewport.MoveWindow(const NewStartX, NewStartY: TRealType);
var
  TmpWin: TRect2D;
  Temp: TRealType;
begin
  StopRepaint;
  Temp := (FVisualWindow.Right - FVisualWindow.Left);
  TmpWin.Left := NewStartX;
  TmpWin.Right := NewStartX + Temp;
  TmpWin.W1 := 1.0;
  Temp := FVisualWindow.Top - FVisualWindow.Bottom;
  TmpWin.Bottom := NewStartY;
  TmpWin.Top := NewStartY + Temp;
  TmpWin.W2 := 1.0;
  ChangeViewportTransform(TmpWin);
end;

procedure TCADViewport.ZoomIn;
var
  TmpWin: TRect2D;
  Temp: TRealType;
begin
  StopRepaint;
  Temp := (FVisualWindow.Right - FVisualWindow.Left) / 4.0;
  TmpWin.Left := FVisualWindow.Left + Temp;
  TmpWin.Right := FVisualWindow.Right - Temp;
  TmpWin.W1 := 1.0;
  Temp := (FVisualWindow.Top - FVisualWindow.Bottom) / 4.0;
  TmpWin.Bottom := FVisualWindow.Bottom + Temp;
  TmpWin.Top := FVisualWindow.Top - Temp;
  TmpWin.W2 := 1.0;
  ChangeViewportTransform(TmpWin);
end;

procedure TCADViewport.ZoomOut;
var
  TmpWin: TRect2D;
  Temp: TRealType;
begin
  StopRepaint;
  Temp := (FVisualWindow.Right - FVisualWindow.Left) / 4.0;
  TmpWin.Left := FVisualWindow.Left - Temp;
  TmpWin.Right := FVisualWindow.Right + Temp;
  TmpWin.W1 := 1.0;
  Temp := (FVisualWindow.Top - FVisualWindow.Bottom) / 4.0;
  TmpWin.Bottom := FVisualWindow.Bottom - Temp;
  TmpWin.Top := FVisualWindow.Top + Temp;
  TmpWin.W2 := 1.0;
  ChangeViewportTransform(TmpWin);
end;

procedure TCADViewport.PanWindow(const DeltaX, DeltaY: TRealType);
var
  TmpWin: TRect2D;
begin
  StopRepaint;
  TmpWin.Left := FVisualWindow.Left + DeltaX;
  TmpWin.Right := FVisualWindow.Right + DeltaX;
  TmpWin.W1 := 1.0;
  TmpWin.Bottom := FVisualWindow.Bottom + DeltaY;
  TmpWin.Top := FVisualWindow.Top + DeltaY;
  TmpWin.W2 := 1.0;
  if Round(TmpWin.Right - TmpWin.Left) > 300 then
   TmpWin.W2 := 1.0;
  ChangeViewportTransform(TmpWin);
end;

procedure TCADViewport.CopyToClipboard(const Clp: TClipboard);
begin
  StopRepaint;
  Clp.Assign(fOffScreenBitmap);
end;

procedure TCADViewport.CopyToCanvas(const Cnv: TCanvas;
                                    const Mode: TCanvasCopyMode;
                                    const View: TCanvasCopyView;
                                    const XScale, YScale: TRealType);
var
  OldView: TRect2D;
  fOldAspect: TRealType;
begin
  if not Assigned(fCADCmp) then
   Exit;
  StopRepaint;
  OldView := VisualRect;
  BeginUpdate;
  fOldAspect := fAspectRatio;
  try
    fAspectRatio := 0;
    case View of
     cvExtension: ZoomToExtension;
     cvScale: CalibrateCnv(Cnv, XScale, YScale);
    end;
    fAspectRatio := fOldAspect;
    CopyRectToCanvas(VisualRect, ClientRect, Cnv, Mode);
  finally
    fAspectRatio := fOldAspect;
    ZoomWindow(OldView);
    EndUpdate;
  end;
end;

function TCADViewport.GetCopyRectViewportToScreen(CADRect: TRect2D;
                                                  const CanvasRect: TRect;
                                                  const Mode: TCanvasCopyMode): TTransf2D;
begin
  Result := GetViewportToScreen;
end;

function TCADViewport.ScreenToViewport(const SPt: TPoint2D): TPoint2D;
begin
  Result := TransformPoint2D(SPt, fScreenToViewport);
end;

function TCADViewport.ViewportToScreen(const WPt: TPoint2D): TPoint2D;
begin
  Result := CartesianPoint2D(TransformPoint2D(WPt, fViewportToScreen));
end;

function TCADViewport.GetViewportToScreen: TTransf2D;
begin
  Result := fViewportToScreen;
end;

function TCADViewport.GetScreenToViewport: TTransf2D;
begin
  Result := fScreenToViewport;
end;

function TCADViewport.GetPixelAperture: TPoint2D;
begin
  Result.X := Abs(fScreenToViewport[1, 1]);
  Result.Y := Abs(fScreenToViewport[2, 2]);
  Result.W := 1.0;
end;

// =====================================================================
// TRuler
// =====================================================================

procedure TRuler.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TRuler.SetOwnerView(V: TCADViewport);
begin
  if V <> fOwnerView then
   begin
     fOwnerView := V;
     Invalidate;
   end;
end;

procedure TRuler.SetOrientation(O: TRulerOrientationType);
begin
  if fOrientation <> O then
   begin
     fOrientation := O;
     Invalidate;
   end;
end;

procedure TRuler.SetSize(S: Integer);
begin
  if S <> fSize then
   begin
     fSize := S;
     Invalidate;
   end;
end;

procedure TRuler.SetFontSize(S: Integer);
begin
  if S <> fFontSize then
   begin
     fFontSize := S;
     Invalidate;
   end;
end;

procedure TRuler.SetStepSize(S: TRealType);
begin
  if S <> fStepSize then
   begin
     fStepSize := S;
     Invalidate;
   end;
end;

procedure TRuler.SetStepDivisions(D: Integer);
begin
  if D <> fStepDivisions then
   begin
     fStepDivisions := D;
     Invalidate;
   end;
end;

procedure TRuler.SetTicksColor(C: TColor);
begin
  if C <> fTicksColor then
   begin
     fTicksColor := C;
     Invalidate;
   end;
end;

constructor TRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fOwnerView := nil;
  fSize := 20;
  fFontSize := 6;
  fStepSize := 10.0;
  fStepDivisions := 5;
  fOrientation := otVertical;
  fTicksColor := clBlack;
  Color := clWhite;
  Canvas.Font.Name := 'Verdana';
  Width := fSize;
  Height := fSize * 4;
end;

procedure TRuler.Paint;
var
  TmpStep, TmpVal: TRealType;
  TmpPt: TPoint;
  LastPt, MinSize: Integer;
begin
  if fOwnerView = nil then
   Exit;
  with Canvas do
   begin
     Font.Size := fFontSize;
     Font.Color := fTicksColor;
     Brush.Color := Color;
     Pen.Color := fTicksColor;
     Pen.Width := 1;
     FillRect(ClientRect);
     SetBkMode(Handle, TRANSPARENT);
     case fOrientation of
      otOrizontal: begin
        TmpVal := Trunc(fOwnerView.VisualRect.Left / fStepSize) * fStepSize;
        TmpStep := fStepSize;
        MinSize := TextWidth(Format('%12.2f', [TmpVal]));
        // Trova lo step ottimo.
        TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(TmpVal - fStepSize, 0)));
        LastPt := TmpPt.X;
        while TmpVal <= fOwnerView.VisualRect.Right do
         begin
           TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(TmpVal, 0)));
           if Abs(TmpPt.X - LastPt) > MinSize then
            Break;
           TmpVal := TmpVal + TmpStep;
           TmpStep := TmpStep * 2.0;
         end;
        // Disegna il righello.
        TmpVal := Trunc(fOwnerView.VisualRect.Left / TmpStep) * TmpStep;
        if TmpStep / fOwnerView.VisualRect.Right < 0.01 then
         Exit;
        while TmpVal <= fOwnerView.VisualRect.Right do
         begin
           TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(TmpVal, 0)));
           MoveTo(TmpPt.X, ClientRect.Top);
           LineTo(TmpPt.X, ClientRect.Bottom);
           TextOut(TmpPt.X, ClientRect.Bottom - Font.Size - 2, Format('%6.2f', [TmpVal]));
           TmpVal := TmpVal + TmpStep;
         end;
        // Subdivisions.
        if fStepDivisions > 0 then
         begin
           TmpStep := TmpStep / fStepDivisions;
           TmpVal := Trunc(fOwnerView.VisualRect.Left / TmpStep) * TmpStep;
           while TmpVal <= fOwnerView.VisualRect.Right do
            begin
              TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(TmpVal, 0)));
              MoveTo(TmpPt.X, ClientRect.Top);
              LineTo(TmpPt.X, ClientRect.Top + fSize div 2);
              TmpVal := TmpVal + TmpStep;
            end;
         end;
      end;
      otVertical: begin
        TmpVal := Trunc(fOwnerView.VisualRect.Bottom / fStepSize) * fStepSize;
        TmpStep := fStepSize;
        MinSize := 2 * Font.Size;
        // Trova lo step ottimo.
        TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(0, TmpVal - fStepSize)));
        LastPt := TmpPt.Y;
        while TmpVal <= fOwnerView.VisualRect.Top do
         begin
           TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(0, TmpVal)));
           if Abs(TmpPt.Y - LastPt) > MinSize then
            Break;
           TmpVal := TmpVal + TmpStep;
           TmpStep := TmpStep * 2.0;
         end;
        // Disegna il righello.
        TmpVal := Trunc(fOwnerView.VisualRect.Bottom / TmpStep) * TmpStep;
        if TmpStep / fOwnerView.VisualRect.Right < 0.01 then
         Exit;
        while TmpVal <= fOwnerView.VisualRect.Top do
         begin
           TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(0, TmpVal)));
           MoveTo(ClientRect.Left, TmpPt.Y);
           LineTo(ClientRect.Right, TmpPt.Y);
           TextOut(ClientRect.Left + fSize div 2, TmpPt.Y, Format('%-6.2f', [TmpVal]));
           TmpVal := TmpVal + TmpStep;
         end;
        // Subdivisions.
        if fStepDivisions > 0 then
         begin
           TmpStep := TmpStep / fStepDivisions;
           TmpVal := Trunc(fOwnerView.VisualRect.Bottom / TmpStep) * TmpStep;
           while TmpVal <= fOwnerView.VisualRect.Top do
            begin
              TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(0, TmpVal)));
              MoveTo(ClientRect.Left, TmpPt.Y);
              LineTo(ClientRect.Left + fSize div 2, TmpPt.Y);
              TmpVal := TmpVal + TmpStep;
            end;
         end;
      end;
     end;
   end;
end;

procedure TRuler.SetMark(Value: TRealType);
var
  TmpPt: TPoint;
begin
  Paint;
  if fOwnerView = nil then
   Exit;
  with Canvas do
   begin
     Pen.Color := fTicksColor;
     Pen.Width := 3;
     case fOrientation of
      otOrizontal: begin
        TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(Value, 0)));
        MoveTo(TmpPt.X, ClientRect.Top);
        LineTo(TmpPt.X, ClientRect.Bottom);
      end;
      otVertical: begin
        TmpPt := Point2DToPoint(fOwnerView.ViewportToScreen(Point2D(0, Value)));
        MoveTo(ClientRect.Left, TmpPt.Y);
        LineTo(ClientRect.Right, TmpPt.Y);
      end;
     end;
   end;
end;

// =====================================================================
// TObject2DHandler
// =====================================================================

constructor TObject2DHandler.Create(AObject: TObject2D);
begin
  inherited Create;

  fHandledObject := AObject;
  fRefCount := 1;
end;

destructor TObject2DHandler.Destroy;
begin
  if Assigned(fHandledObject) then
   fHandledObject.fHandler := nil;
  inherited;
end;

procedure TObject2DHandler.FreeInstance;
begin
  Dec(fRefCount);
  if fRefCount > 0 then
   Exit;
  inherited;
end;

// =====================================================================
// TObject2D
// =====================================================================

constructor TObject2D.Create(ID: LongInt);
begin
  inherited Create(ID);
  fBox := Rect2D(0, 0, 0, 0);
  fModelTransform := nil;
  fSavedTransform := nil;
  fDrawBoundingBox := False;
  fHandler := nil;
end;

destructor TObject2D.Destroy;
begin
  if Assigned(fHandler) then
   fHandler.Free;
  if fModelTransform <> nil then
   FreeMem(fModelTransform, SizeOf(TTransf2D));
  if fSavedTransform <> nil then
   FreeMem(fSavedTransform, SizeOf(TTransf2D));
  inherited Destroy;
end;

constructor TObject2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpTransf: TTransf2D;
  TmpTransfS: TTransf2DSingle;
begin
  inherited;
  with Stream do
   begin
     if( Version >= 'CAD423' ) then
      begin
        Read({%H-}TmpTransf, sizeof(TTransf2D));
        SaveTransform(TmpTransf);
      end
     else
      begin
        Read({%H-}TmpTransfS, sizeof(TTransf2DSingle));
        TmpTransf[1, 1] := TmpTransfS[1, 1]; TmpTransf[1, 2] := TmpTransfS[1, 2]; TmpTransf[1, 3] := TmpTransfS[1, 3];
        TmpTransf[2, 1] := TmpTransfS[2, 1]; TmpTransf[2, 2] := TmpTransfS[2, 2]; TmpTransf[2, 3] := TmpTransfS[2, 3];
        TmpTransf[3, 1] := TmpTransfS[3, 1]; TmpTransf[3, 2] := TmpTransfS[3, 2]; TmpTransf[3, 3] := TmpTransfS[3, 3];
        SaveTransform(TmpTransf);
      end;
   end;
  fHandler := nil;
end;

procedure TObject2D.SaveToStream(const Stream: TStream);
var
  TmpTransf: TTransf2D;
begin
  inherited SaveToStream(Stream);
  with Stream do
   begin
     SaveTransform(ModelTransform);
     TmpTransf := ModelTransform;
     Write(TmpTransf, SizeOf(TTransf2D));
   end;
end;

procedure TObject2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if Obj is TObject2D then
   begin
     SaveTransform(TObject2D(Obj).ModelTransform);
     fBox := TObject2D(Obj).fBox;
     if fHandler <> TObject2D(Obj).fHandler then
      SetSharedHandler(TObject2D(Obj).fHandler);
   end;
end;

function TObject2D.WorldToObject(const Pt: TPoint2D): TPoint2D;
begin
  Result := TransformPoint2D(Pt, InvertTransform2D(ModelTransform));
end;

procedure TObject2D.MoveTo(ToPt, DragPt: TPoint2D);
var
  TmpTransf: TTransf2D;
begin
  ToPt := CartesianPoint2D(ToPt);
  DragPt := CartesianPoint2D(DragPt);
  TmpTransf := Translate2D(ToPt.X - DragPt.X, ToPt.Y - DragPt.Y);
  Transform(TmpTransf);
end;

function TObject2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
 TmpBox: TRect2D;
begin
  TmpBox := EnlargeBoxDelta2D(Box, Aperture);
  Distance := MaxCoord;
  Result := PICK_NOOBJECT;
  if not FEnabled then
   Exit;
  if IsPointInCartesianBox2D(Pt, TmpBox) then
   begin
     Distance := Aperture;
     Result := PICK_INBBOX;
   end;
  if Assigned(fHandler) then
   Result := MaxIntValue([Result, fHandler.OnMe(Self, Pt, Aperture, Distance)]);
end;

procedure TObject2D.DrawControlPoints(const VT: TTransf2D; const Cnv: TDecorativeCanvas;
                                      const ClipRect2D: TRect2D; const Width: Integer);
begin
  // Draw the bounding box.
  if fDrawBoundingBox and (Cnv.Canvas.Pen.Mode <> pmXOr) then
   DrawBoundingBox2D(Cnv, Box, ClipRect2D, VT);
  if Assigned(fHandler) then
   fHandler.DrawControlPoints(Self, VT, Cnv, Width);
end;

procedure TObject2D.SetModelTransform(Transf: TTransf2D);
begin
  if IsSameTransform2D(Transf, IdentityTransf2D) then
   begin
     if (fModelTransform <> nil) then
      FreeMem(fModelTransform, SizeOf(TTransf2D));
     fModelTransform := nil;
   end
  else if fModelTransform = nil then
   begin
     GetMem(fModelTransform, SizeOf(TTransf2D));
     fModelTransform^ := Transf;
   end
  else
   fModelTransform^ := Transf;
  UpdateExtension(Self);
end;

procedure TObject2D.SaveTransform(const T: TTransf2D);
begin
  if IsSameTransform2D(T, IdentityTransf2D) then
   begin
     if (fSavedTransform <> nil) then
      FreeMem(fSavedTransform, SizeOf(TTransf2D));
     fSavedTransform := nil;
   end
  else if fSavedTransform = nil then
   begin
     GetMem(fSavedTransform, SizeOf(TTransf2D));
     fSavedTransform^ := T;
   end
  else
   fSavedTransform^ := T;
  if (fModelTransform <> nil) then
   FreeMem(fModelTransform, SizeOf(TTransf2D));
  fModelTransform := nil;
end;

procedure TObject2D.RemoveTransform;
begin
  if (fSavedTransform <> nil) then
   FreeMem(fSavedTransform, SizeOf(TTransf2D));
  fSavedTransform := nil;
  SetModelTransform(IdentityTransf2D);
end;

function TObject2D.GetModelTransform: TTransf2D;
begin
  if fModelTransform <> nil then
   Result := fModelTransform^
  else
   Result := IdentityTransf2D;
  if Assigned(fSavedTransform) then
   Result := MultiplyTransform2D(fSavedTransform^, Result);
end;

function TObject2D.HasTransform: Boolean;
begin
  Result := (fModelTransform <> nil) or (fSavedTransform <> nil);
end;

procedure TObject2D.Transform(const T: TTransf2D);
begin
  if fModelTransform <> nil then
   SetModelTransform(MultiplyTransform2D(fModelTransform^, T))
  else
   SetModelTransform(T);
end;

procedure TObject2D.ApplyTransform;
begin
  SaveTransform(ModelTransform);
end;

function TObject2D.IsVisible(const Clip: TRect2D; const DrawMode: Integer): Boolean;
begin
  Result := False;
  if not Visible then
   Exit;
  if fBox.Left > Clip.Right then
   Exit
  else if fBox.Right < Clip.Left then
   Exit;
  if fBox.Bottom > Clip.Top then
   Exit
  else if fBox.Top < Clip.Bottom then
   Exit;
  Result := True;
end;

procedure TObject2D.SetSharedHandler(const Hndl: TObject2DHandler);
begin
  if Assigned(fHandler) then
   fHandler.Free;
  fHandler := Hndl;
  if( Hndl <> nil ) then
   Inc(fHandler.fRefCount);
end;

procedure TObject2D.SetHandler(const Hndl: TObject2DHandlerClass);
begin
  if Assigned(fHandler) then
   fHandler.Free;
  if( Hndl <> nil ) then
   fHandler := Hndl.Create(Self)
  else
   fHandler := nil;
end;

// =====================================================================
// TContainer2D
// =====================================================================

function StringToBlockName(const Str: String): TSourceBlockName;
var
  Cont: Integer;
begin
  for Cont := 0 to 12 do
   begin
     if Cont < Length(Str) then
      Result[Cont] := Str[Cont + 1]
     else
      Result[Cont] := #0;
   end;
end;

constructor TContainer2D.Create(ID: LongInt; const Objs: array of TObject2D);
var
  Cont: Word;
begin
  inherited Create(ID);

  fObjects := TGraphicObjList.Create;
  fObjects.FreeOnClear := True;
  for Cont := Low(Objs) to High(Objs) do
   if Objs[Cont] <> nil then fObjects.Add(Objs[Cont]);
  UpdateExtension(Self);
end;

procedure TContainer2D._UpdateExtension;
var
  TmpIter: TGraphicObjIterator;
begin
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  try
    if TmpIter.Count = 0 then
     begin
       fBox := Rect2D(0, 0, 0, 0);
       Exit;
     end;
    fBox := TObject2D(TmpIter.First).Box;
    while TmpIter.Next <> nil do
     fBox := BoxOutBox2D(fBox, TObject2D(TmpIter.Current).Box);
    if HasTransform then
     fBox := TransformBoundingBox2D(fBox, ModelTransform);
  finally // Libera l'iterator
    TmpIter.Free;
  end;
end;

destructor TContainer2D.Destroy;
begin
  fObjects.Free;
  inherited Destroy;
end;

procedure TContainer2D.SaveToStream(const Stream: TStream);
var
  TmpObj: TObject2D;
  TmpLong: LongInt;
  TmpWord: Word;
  TmpIter: TGraphicObjIterator;
begin
  inherited SaveToStream(Stream);
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  with Stream do
   try
     { Write the number of objects in the container. }
     TmpLong := FObjects.Count;
     Write(TmpLong, SizeOf(TmpLong));
     { Now write the objects in the container. }
     TmpObj := TObject2D(TmpIter.First);
     while TmpObj <> nil do
      begin
        TmpWord := CADSysFindClassIndex(TmpObj.ClassName);
        { Save the class index. }
        Write(TmpWord, SizeOf(TmpWord));
        { Save the object. }
        TmpObj.SaveToStream(Stream);
        TmpObj := TObject2D(TmpIter.Next);
     end;
   finally // Libera l'iterator
     TmpIter.Free;
   end;
end;

constructor TContainer2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
  TmpLong: LongInt;
  TmpWord: Word;
begin
  inherited;
  with Stream do
   begin
     { Read the number of objects in the container. }
     TmpLong := 0;
     Read(TmpLong, SizeOf(TmpLong));
     fObjects := TGraphicObjList.Create;
     fObjects.FreeOnClear := True;
     { Now read the objects for the container. }
     while TmpLong > 0 do
      begin
        { Read the type of object. }
        TmpWord := 0;
        Read(TmpWord, SizeOf(TmpWord));
        { Retrive the class type from the registered classes. }
        TmpClass := CADSysFindClassByIndex(TmpWord);
        TmpObj := TmpClass.CreateFromStream(Stream, Version);
        TmpObj.UpdateExtension(Self);
        fObjects.Add(TmpObj);
        Dec(TmpLong);
     end;
   end;
  UpdateExtension(Self);
end;

procedure TContainer2D.Assign(const Obj: TGraphicObject);
var
  TmpIter: TGraphicObjIterator;
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TContainer2D then
   begin
     if fObjects = nil then
      begin
        fObjects := TGraphicObjList.Create;
        fObjects.FreeOnClear := True;
      end
     else
      fObjects.Clear;
     // Copia creando gli oggetti contenuti.
     if TContainer2D(Obj).fObjects.HasExclusiveIterators then
      Raise ECADListBlocked.Create('TContainer2D.Assign: The list has an exclusive iterator.');
     // Alloca un iterator locale
     TmpIter := TContainer2D(Obj).fObjects.GetIterator;
     try
      repeat
        TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
        TmpObj := TmpClass.Create(TmpIter.Current.ID);
        TmpObj.Assign(TmpIter.Current);
        fObjects.Add(TmpObj);
      until TmpIter.Next = nil;
     finally
      TmpIter.Free;
     end;
   end;
end;

procedure TContainer2D.UpdateSourceReferences(const BlockList: TGraphicObjIterator);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  try
    TmpObj := TObject2D(TmpIter.First);
    while TmpObj <> nil do
     begin
       if(TmpObj is TSourceBlock2D) then
        TSourceBlock2D(TmpObj).UpdateSourceReferences(BlockList)
       else if(TmpObj is TBlock2D) then
        TBlock2D(TmpObj).UpdateReference(BlockList);
       TmpObj := TObject2D(TmpIter.Next);
     end;
  finally // Libera l'iterator
    TmpIter.Free;
  end;
  UpdateExtension(Self);
end;

procedure TContainer2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas;
                            const ClipRect2D: TRect2D; const DrawMode: Integer);
var
  TmpObj: TObject2D;
  TmpTransf: TTransf2D;
  TmpIter: TGraphicObjIterator;
  TmpPen: TPen;
  TmpBrush: TBrush;
begin
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  TmpPen := TPen.Create;
  TmpBrush := TBrush.Create;
  try
    { Transform the container. }
    TmpTransf := MultiplyTransform2D(ModelTransform, VT);
    TmpObj := TObject2D(TmpIter.First);
    TmpPen.Assign(Cnv.Canvas.Pen);
    TmpBrush.Assign(Cnv.Canvas.Brush);
    while TmpObj <> nil do
     begin
       Cnv.Canvas.Pen.Assign(TmpPen);
       Cnv.Canvas.Brush.Assign(TmpBrush);
       { Transform the object in the container. }
       TmpObj.Draw(TmpTransf, Cnv, ClipRect2D, DrawMode);
       TmpObj := TObject2D(TmpIter.Next);
     end;
  finally
    TmpPen.Free;
    TmpBrush.Free;
    TmpIter.Free;
  end;
end;

function TContainer2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                           var Distance: TRealType): Integer;
var
  TmpObj: TObject2D;
  MinDist: TRealType;
  TmpIter: TGraphicObjIterator;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     // Crea un iterator temporaneo.
     TmpIter := fObjects.GetIterator;
     try
       { Make Pt in container coordinate. }
       if HasTransform then
        Pt := TransformPoint2D(Pt, InvertTransform2D(ModelTransform));
       { Check all the objects in the container. }
       TmpObj := TObject2D(TmpIter.First);
       MinDist := 2.0 * Aperture;
       while TmpObj <> nil do
        begin
          if (TmpObj.OnMe(Pt, Aperture, Distance) >= PICK_INOBJECT) and
             (Distance < MinDist) then
           begin
             MinDist := Distance;
             Result := TmpObj.ID;
           end;
          TmpObj := TObject2D(TmpIter.Next);
        end;
       Distance := MinDist;
     finally
       TmpIter.Free;
     end;
   end;
end;

procedure TContainer2D.DrawControlPoints(const VT: TTransf2D; const Cnv: TDecorativeCanvas;
                                         const ClipRect2D: TRect2D; const Width: Integer);
begin
  if fObjects.Count > 0 then
   inherited;
end;

// =====================================================================
// TSourceBlock2D
// =====================================================================

constructor TSourceBlock2D.Create(ID: LongInt; const Name: TSourceBlockName; const Objs: array of TObject2D);
begin
  inherited Create(ID, Objs);

  fName := Name;
  fLibraryBlock := False;
  fNReference := 0;
end;

destructor TSourceBlock2D.Destroy;
begin
  if fNReference > 0 then
   Raise ECADSourceBlockIsReferenced.Create('TSourceBlock2D.Destroy: This source block is referenced and cannot be deleted');
  inherited Destroy;
end;

constructor TSourceBlock2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  Stream.Read(fToBeSaved, SizeOf(fToBeSaved));
  Stream.Read(fLibraryBlock, SizeOf(fLibraryBlock));
  Stream.Read(fName, SizeOf(fName));
  fNReference := 0;
end;

procedure TSourceBlock2D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(fToBeSaved, SizeOf(fToBeSaved));
  Stream.Write(fLibraryBlock, SizeOf(fLibraryBlock));
  Stream.Write(FName, SizeOf(FName));
end;

procedure TSourceBlock2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TSourceBlock2D then
   fToBeSaved := TSourceBlock2D(Obj).fToBeSaved;
end;

// =====================================================================
// TBlock2D
// =====================================================================

procedure TBlock2D.SetSourceBlock(const Source: TSourceBlock2D);
begin
  if not Assigned(Source) then
   Raise Exception.Create('TBlock2D.SetSourceBlock: Invalid parameter');
  if (fSourceBlock <> Source) then
   begin
     if Assigned(fSourceBlock) and (fSourceBlock.FNReference > 0) then
      Dec(fSourceBlock.FNReference);
     fSourceBlock := Source;
     fSourceName := Source.Name;
     Inc(fSourceBlock.FNReference);
     UpdateExtension(Self);
   end;
end;

procedure TBlock2D.SetOriginPoint(Pt: TPoint2D);
begin
  fOriginPoint := Pt;
  UpdateExtension(Self);
end;

constructor TBlock2D.Create(ID: LongInt; const Source: TSourceBlock2D);
begin
  inherited Create(ID);

  if not Assigned(Source) then
   Raise Exception.Create('TBlock2D.Create: Invalid parameter');
  fOriginPoint := Point2D(0, 0);
  fSourceName := '';
  fSourceBlock := Source;
  fSourceName := Source.Name;
  fBox := Source.Box;
  Inc(fSourceBlock.fNReference);
  UpdateExtension(Self);
end;

destructor TBlock2D.Destroy;
begin
  if Assigned(fSourceBlock) and (fSourceBlock.fNReference > 0) then
   Dec(fSourceBlock.fNReference);
  inherited Destroy;
end;

constructor TBlock2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  origPS: TPoint2DSingle;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     { TCADCmp will use the value of FSourceName to find out the reference of the source block. }
     Read(fSourceName, SizeOf(fSourceName));
     if( Version >= 'CAD423' ) then
      begin
        Read(fOriginPoint, SizeOf(fOriginPoint));
      end
     else
      begin
        origPS.X:=0;origPS.Y:=0;origPS.W:=0;
        Read(origPS, SizeOf(origPS));
        fOriginPoint.X := origPS.X;
        fOriginPoint.Y := origPS.Y;
        fOriginPoint.W := origPS.W;
      end;
   end;
  fSourceBlock := nil;
end;

procedure TBlock2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     { Save the ID of the source block. }
     Write(fSourceName, SizeOf(fSourceName));
     Write(fOriginPoint, SizeOf(fOriginPoint));
   end;
end;

procedure TBlock2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TBlock2D then
   begin
     if Assigned(fSourceBlock) and (fSourceBlock.FNReference > 0) then
      Dec(fSourceBlock.FNReference);
     fSourceBlock := TBlock2D(Obj).fSourceBlock;
     fSourceName := fSourceBlock.Name;
     fOriginPoint := TBlock2D(Obj).OriginPoint;
     Inc(fSourceBlock.FNReference);
     UpdateExtension(Self);
   end;
end;

procedure TBlock2D.UpdateReference(const BlockList: TGraphicObjIterator);
var
  TmpSource: TSourceBlock2D;
begin
  TmpSource := BlockList.First as TSourceBlock2D;
  while TmpSource <> nil do
   begin
     if TmpSource.Name = fSourceName then
      begin
        fSourceBlock := TmpSource;
        UpdateExtension(Self);
        Exit;
      end;
     TmpSource := BlockList.Next as TSourceBlock2D;
   end;
  Raise ECADListObjNotFound.Create('TBlock2D.UpdateReference: Source block not found');
end;

procedure TBlock2D._UpdateExtension;
begin
  if not Assigned(fSourceBlock) then Exit;
  fSourceBlock.UpdateExtension(Self);
  if HasTransform then
   fBox := TransformBoundingBox2D(fSourceBlock.Box, ModelTransform)
  else
   fBox := fSourceBlock.Box;
end;

procedure TBlock2D.DrawControlPoints(const VT: TTransf2D; const Cnv: TDecorativeCanvas;
                                     const ClipRect2D: TRect2D; const Width: Integer);
var
  TmpPt1: TPoint2D;
begin
  inherited;
  if HasTransform then
   TmpPt1 := TransformPoint2D(TransformPoint2D(FOriginPoint, ModelTransform), VT)
  else
   TmpPt1 := TransformPoint2D(FOriginPoint, VT);
  with Point2DToPoint(TmpPt1) do
   DrawPlaceHolder(Cnv, X, Y, Width);
end;

procedure TBlock2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas;
                        const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  if not Assigned(FSourceBlock) then Exit;
  if HasTransform then
   FSourceBlock.Draw(MultiplyTransform2D(ModelTransform, VT), Cnv, ClipRect2D, DrawMode)
  else
   FSourceBlock.Draw(VT, Cnv, ClipRect2D, DrawMode);
end;

function TBlock2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  TmpPt: TPoint2D;
begin
  Result := PICK_NOOBJECT;
  if not Assigned(FSourceBlock) then
   Exit;
  Result := inherited OnMe(Pt, Aperture, Distance);
  if (Result = PICK_INBBOX) and NearPoint2D(Pt, TransformPoint2D(FOriginPoint, ModelTransform), Aperture, Distance) then
   { the origin of the block was picked. }
   Result := PICK_ONOBJECT
  else
   begin
     { Make Pt in Object coordinates }
     if HasTransform then
      TmpPt := TransformPoint2D(Pt, InvertTransform2D(ModelTransform))
     else
      TmpPt := Pt;
     Result := FSourceBlock.OnMe(TmpPt, Aperture, Distance);
   end;
end;

// =====================================================================
// TCADCmp2D
// =====================================================================

procedure TCADCmp2D.SaveObjectsToStream(const Stream: TStream);
var
  TmpObj: TObject2D;
  TmpWord: Word;
  TmpLong, TmpObjPerc: LongInt;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := ObjectList.GetPrivilegedIterator;
  with Stream do
   try
     { Save the objects. }
     TmpLong := TmpIter.Count;
     if TmpLong > 0 then
      TmpObjPerc := 100 div TmpLong
     else
      TmpObjPerc := 0;
     Write(TmpLong, SizeOf(TmpLong));
     TmpObj := TmpIter.First as TObject2D;
     while TmpObj <> nil do
      begin
        if Layers[TmpObj.Layer].Streamable and TmpObj.fToBeSaved then
         begin
           TmpWord := CADSysFindClassIndex(TmpObj.ClassName);
           { Save the class index. }
           Write(TmpWord, SizeOf(TmpWord));
           TmpObj.SaveToStream(Stream);
           if Assigned(OnSaveProgress) then
            OnSaveProgress(Self, 100 - TmpObjPerc * TmpLong);
           Dec(TmpLong);
         end;
        TmpObj := TmpIter.Next as TObject2D;
      end;
     { End the list of objects if not all objects were saved. }
     if TmpLong > 0 then
      begin
        TmpWord := 65535;
        Write(TmpWord, SizeOf(TmpWord));
      end;
   finally
     TmpIter.Free;
   end;
end;

{$WARNINGS OFF}
procedure TCADCmp2D.LoadObjectsFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
  TmpLong, TmpObjPerc: LongInt;
  TmpWord: Word;
  TmpBlocksIter: TExclusiveGraphicObjIterator;
begin
  TmpBlocksIter := SourceBlocksExclusiveIterator;
  with Stream do
   try
     TmpLong := 0;
     Read(TmpLong, SizeOf(TmpLong));
     if TmpLong > 0 then
      TmpObjPerc := 100 div TmpLong
     else
      TmpObjPerc := 0;
     while TmpLong > 0 do
      begin
        { Read the type of object and the length of the record. }
        TmpWord := 0;
        Read(TmpWord, SizeOf(TmpWord));
        if TmpWord = 65535 then
         { End prematurely. }
         Break;
        Dec(TmpLong);
        { Retrive the class type from the registered classes. }
        try
         TmpClass := CADSysFindClassByIndex(TmpWord);
        except
         on ECADObjClassNotFound do
          begin
            ShowMessage('Object class not found. Object not load');
            Break;
          end;
        end;
        TmpObj := TmpClass.CreateFromStream(Stream, Version);
        if Assigned(OnLoadProgress) then
         OnLoadProgress(Self, TmpObjPerc);
        if not (TmpObj is TObject2D) then
         begin
           ShowMessage('Not 2D Object. Object discarded.');
           TmpObj.Free;
           Continue;
         end;
        if TmpObj is TContainer2D then
         try
           TContainer2D(TmpObj).UpdateSourceReferences(TmpBlocksIter);
         except
          on ECADListObjNotFound do
           begin
             ShowMessage('Source block not found. The block will not be loaded');
             TmpObj.Free;
             Continue;
           end;
         end
        else if TmpObj is TBlock2D then
         try
           TBlock2D(TmpObj).UpdateReference(TmpBlocksIter);
         except
          on ECADListObjNotFound do
           begin
             ShowMessage('Source block not found. The block will not be loaded');
             TmpObj.Free;
             Continue;
           end;
         end;
        CurrentLayer := TmpObj.Layer;
        inherited AddObject(-1, TGraphicObject(TmpObj));
      end;
   finally
     TmpBlocksIter.Free;
   end;
end;
{$WARNINGS ON}

procedure TCADCmp2D.SaveBlocksToStream(const Stream: TStream; const AsLibrary: Boolean);
var
  TmpObj: TSourceBlock2D;
  TmpWord: Word;
  TmpPos, TmpLong: LongInt;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := BlockList.GetPrivilegedIterator;
  with Stream do
   try
     TmpLong := SourceBlocksCount;
     TmpPos := Stream.Position;
     Write(TmpLong, SizeOf(TmpLong));
     TmpObj := TmpIter.First as TSourceBlock2D;
     TmpLong := 0;
     while TmpObj <> nil do
      begin
        if TmpObj.ToBeSaved and
           not(TmpObj.IsLibraryBlock xor AsLibrary) then
         begin
           TmpWord := CADSysFindClassIndex(TmpObj.ClassName);
           { Save the class index. }
           Write(TmpWord, SizeOf(TmpWord));
           TmpObj.SaveToStream(Stream);
           Inc(TmpLong);
         end;
        TmpObj := TmpIter.Next as TSourceBlock2D;
      end;
     Seek(TmpPos, soFromBeginning);
     Write(TmpLong, SizeOf(TmpLong));
     Seek(0, soFromEnd);
   finally
     TmpIter.Free;
   end;
end;

{$WARNINGS OFF}
procedure TCADCmp2D.LoadBlocksFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
  TmpLong: LongInt;
  TmpWord: Word;
  TmpBlocksIter: TGraphicObjIterator;
begin
  with Stream do
   begin
     { Load the source blocks. }
     TmpLong := 0;
     Read(TmpLong, SizeOf(TmpLong));
     while TmpLong > 0 do
      begin
        { Read the type of object and the length of the record. }
        TmpWord := 0;
        Read(TmpWord, SizeOf(TmpWord));
        Dec(TmpLong);
        { Retrive the class type from the registered classes. }
        try
         TmpClass := CADSysFindClassByIndex(TmpWord);
        except
         on ECADObjClassNotFound do
          begin
            ShowMessage('Object class not found. Object not load');
            Continue;
          end;
        end;
        TmpObj := TmpClass.CreateFromStream(Stream, Version);
        TmpBlocksIter := SourceBlocksIterator;
        try
         TSourceBlock2D(TmpObj).UpdateSourceReferences(TmpBlocksIter);
        except
         on ECADListObjNotFound do
          begin
            ShowMessage('Source block not found. The block will not be loaded');
            TmpObj.Free;
            TmpBlocksIter.Free;
            Continue;
          end;
        end;
        TmpBlocksIter.Free;
        AddSourceBlock(TSourceBlock2D(TmpObj));
      end;
   end;
end;
{$WARNINGS ON}

function TCADCmp2D.AddSourceBlock(const Obj: TSourceBlock2D): TSourceBlock2D;
begin
  Result := Obj;
  inherited AddSourceBlock(-1, Obj);
end;

procedure TCADCmp2D.DeleteSourceBlock(const SrcName: TSourceBlockName);
var
  TmpSource: TSourceBlock2D;
begin
  TmpSource := TSourceBlock2D(FindSourceBlock(SrcName));
  if TmpSource.NumOfReferences > 0 then
   Raise ECADSysException.Create('TCADCmp2D.DeleteSourceBlock: Remove the references before the source');
  DeleteSourceBlockByID(TmpSource.ID);
end;

function TCADCmp2D.GetSourceBlock(const ID: LongInt): TSourceBlock2D;
begin
  Result := inherited GetSourceBlock(ID) as TSourceBlock2D;
end;

function TCADCmp2D.FindSourceBlock(const SrcName: TSourceBlockName): TSourceBlock2D;
var
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := SourceBlocksIterator;
  try
    Result := TmpIter.First as TSourceBlock2D;
    while Result <> nil do
     begin
       if SrcName = Result.Name then
        Exit;
       Result := TmpIter.Next as TSourceBlock2D;
     end;
  finally
    TmpIter.Free;
  end;
  Raise ECADListObjNotFound.Create(Format('TCADCmp2D.FindSourceBlock: Source block %s not found', [SrcName]));
end;

function TCADCmp2D.BlockObjects(const SrcName: TSourceBlockName; const Objs: TGraphicObjIterator): TSourceBlock2D;
var
  TmpObj: TObject2D;
begin
  Result := TSourceBlock2D.Create(0, SrcName, [nil]);
  try
    TmpObj := Objs.First as TObject2D;
    while TmpObj <> nil do
     begin
       Result.Objects.Add(TmpObj);
       TmpObj := Objs.Next as TObject2D;
     end;
    AddSourceBlock(Result);
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TCADCmp2D.DeleteSavedSourceBlocks;
var
  TmpObj: TGraphicObject;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := SourceBlocksExclusiveIterator;
  try
    TmpObj := TmpIter.First;
    while Assigned(TmpObj) do
     if TSourceBlock2D(TmpObj).ToBeSaved and not TSourceBlock2D(TmpObj).IsLibraryBlock then
      try
        TmpIter.DeleteCurrent;
        TmpObj := TmpIter.Current;
      except
        TmpObj := TmpIter.Next;
      end
     else
      TmpObj := TmpIter.Next;
  finally
    TmpIter.Free;
  end;
end;

procedure TCADCmp2D.DeleteLibrarySourceBlocks;
var
  TmpObj: TGraphicObject;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := SourceBlocksExclusiveIterator;
  try
    TmpObj := TmpIter.First;
    while Assigned(TmpObj) do
     if TSourceBlock2D(TmpObj).IsLibraryBlock then
      try
        TmpIter.DeleteCurrent;
        TmpObj := TmpIter.Current;
      except
        TmpObj := TmpIter.Next;
      end
     else
      TmpObj := TmpIter.Next;
  finally
    TmpIter.Free;
  end;
end;

function TCADCmp2D.AddObject(const ID: LongInt; const Obj: TObject2D): TObject2D;
begin
  Result := Obj;
  inherited AddObject(ID, TGraphicObject(Obj));
end;

function TCADCmp2D.InsertObject(const ID, IDInsertPoint: LongInt; const Obj: TObject2D): TObject2D;
begin
  Result := Obj;
  inherited InsertObject(ID, IDInsertPoint, TGraphicObject(Obj));
end;

function TCADCmp2D.AddBlock(const ID: LongInt; const SrcName: TSourceBlockName): TObject2D;
var
  Tmp: TBlock2D;
  TmpSource: TSourceBlock2D;
begin
  TmpSource := FindSourceBlock(SrcName);
  Tmp := TBlock2D.Create(ID, TmpSource);
  try
    AddObject(ID, Tmp);
    Result := Tmp;
  except
    Tmp.Free;
    Result := nil;
  end;
end;

function TCADCmp2D.GetObject(const ID: LongInt): TObject2D;
begin
  Result := inherited GetObject(ID) as TObject2D;
end;

procedure TCADCmp2D.TransformObjects(const ListOfObj: array of LongInt; const T: TTransf2D);
var
  Cont: LongInt;
  Tmp: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := ObjectsExclusiveIterator;
  try
    if ListOfObj[Low(ListOfObj)] < 0 then
     begin
       { Apply trasform to all objects. }
       Tmp := TmpIter.First as TObject2D;
       while Tmp <> nil do
        begin
          Tmp.Transform(T);
          Tmp.ApplyTransform;
          Tmp := TmpIter.Next as TObject2D;
        end;
     end
    else
     for Cont := Low(ListOfObj) to High(ListOfObj) do
      begin
        try
         Tmp := TObject2D(TmpIter.Search(ListOfObj[Cont]));
        except
         on Exception do Raise ECADListObjNotFound.Create('TCADCmp2D.TransformObjects: Object not found');
        end;
        if Tmp <> nil then
         begin
           Tmp.Transform(T);
           Tmp.ApplyTransform;
         end;
      end;
  finally
    TmpIter.Free;
  end;
  if RepaintAfterTransform then RepaintViewports;
end;

procedure TCADCmp2D.RedrawObject(const Obj: TObject2D);
begin
  inherited RedrawObject(Obj);
end;

function TCADCmp2D.GetExtension: TRect2D;
var
  Tmp: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  if ObjectsCount = 0 then
   Exit;
  TmpIter := ObjectsIterator;
  try
    Result := Rect2D(MaxCoord, MaxCoord, MinCoord, MinCoord);
    Tmp := TmpIter.First as TObject2D;
    while Tmp <> nil do
     begin
       if fLayers[Tmp.Layer].fVisible then
        begin
          try
           Tmp.UpdateExtension(Self);
          finally
          end;
          { Set the new extension if necessary. }
          if Tmp.Box.Left < Result.Left then
           Result.Left := Tmp.Box.Left;
          if Tmp.Box.Right > Result.Right then
           Result.Right := Tmp.Box.Right;
          if Tmp.Box.Bottom < Result.Bottom then
           Result.Bottom := Tmp.Box.Bottom;
          if Tmp.Box.Top > Result.Top then
           Result.Top := Tmp.Box.Top;
        end;
       Tmp := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
  end;
end;

// =====================================================================
// TCADViewport2D
// =====================================================================

procedure TCADViewport2D.SetCADCmp(CAD: TCADCmp);
begin
  if( CAD <> fCADCmp2D ) then
   begin
     inherited SetCADCmp(CAD);
     fCADCmp2D := CAD as TCADCmp2D;
   end;
end;

procedure TCADViewport2D.SetCADCmp2D(CAD2D: TCADCmp2D);
begin
  SetCADCmp(CAD2D);
end;

constructor TCADViewport2D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fPickFilter := TObject2D;
end;

procedure TCADViewport2D.DrawObject(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D);
begin
  if not TObject2D(Obj).IsVisible(VisualRect, DrawMode) then
   Exit;
  with Obj as TObject2D do
   try
     if (CADCmp.Layers.SetCanvas(Cnv, Layer)) then
      begin
        Draw(ViewportToScreenTransform, Cnv, ClipRect2D, DrawMode);
        if ShowControlPoints then
         begin
           Cnv.Canvas.Pen.Color := RubberPenColor;
           Cnv.Canvas.Pen.Width := 1;
           Cnv.Canvas.Brush.Color := ControlPointsColor;
           DrawControlPoints(ViewportToScreenTransform, Cnv, ClipRect2D, ControlPointsWidth);
         end;
      end;
   finally
   end;
end;

procedure TCADViewport2D.DrawObjectWithRubber(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D);
begin
  if not TObject2D(Obj).IsVisible(VisualRect, DrawMode) then
   Exit;
  with Obj as TObject2D do
   try
     Cnv.Canvas.Pen.Assign(RubberPen);
     Cnv.Canvas.Brush.Color := RubberPen.Color;
     Cnv.Canvas.Brush.Style := bsSolid;
     Draw(ViewportToScreenTransform, Cnv, ClipRect2D, DrawMode);
     if ShowControlPoints then
      begin
        Cnv.Canvas.Pen.Assign(RubberPen);
        Cnv.Canvas.Pen.Width := 1;
        Cnv.Canvas.Brush.Color := ControlPointsColor;
        DrawControlPoints(ViewportToScreenTransform, Cnv, ClipRect2D, ControlPointsWidth);
      end;
   finally
   end;
end;

procedure TCADViewport2D.DrawObject2D(const Obj: TObject2D; const CtrlPts: Boolean);
var
  TmpFlag: Boolean;
begin
  if not Assigned(fCADCmp2D) then Exit;
  TmpFlag := ShowControlPoints;
  ShowControlPoints := ShowControlPoints or CtrlPts;
  try
    DrawObject(Obj, OffScreenCanvas, RectToRect2D(ClientRect));
    DrawObject(Obj, OnScreenCanvas, RectToRect2D(ClientRect));
  finally
    ShowControlPoints := TmpFlag;
  end;
end;

procedure TCADViewport2D.DrawObject2DWithRubber(const Obj: TObject2D; const CtrlPts: Boolean);
var
  TmpFlag: Boolean;
begin
  if not Assigned(fCADCmp2D) then Exit;
  TmpFlag := ShowControlPoints;
  ShowControlPoints := ShowControlPoints or CtrlPts;
  try
    DrawObjectWithRubber(Obj, OnScreenCanvas, RectToRect2D(ClientRect));
  finally
    ShowControlPoints := TmpFlag;
  end;
end;

procedure TCADViewport2D.CopyRectToCanvas(CADRect: TRect2D;
                                          const CanvasRect: TRect;
                                          const Cnv: TCanvas;
                                          const Mode: TCanvasCopyMode);
var
  Tmp: TObject2D;
  TmpTransform: TTransf2D;
  TmpFlag: Boolean;
  TmpIter: TGraphicObjIterator;
  TmpCanvas: TDecorativeCanvas;
  TmpClipRect: TRect2D;
begin
  StopRepaint;
  if not Assigned(fCADCmp2D) then
   Exit;
  TmpCanvas := TDecorativeCanvas.Create(Cnv);
  if (ViewportObjects <> nil) then
   TmpIter := ViewportObjects.GetIterator
  else
   TmpIter := fCADCmp2D.ObjectsIterator;
  try
    case Mode of
     cmNone: TmpTransform := GetVisualTransform2D(CADRect, CanvasRect, 0);
     cmAspect: TmpTransform := GetVisualTransform2D(CADRect, CanvasRect, AspectRatio);
    else
     TmpTransform := ViewportToScreenTransform;
    end;
    Tmp := TObject2D(TmpIter.First);
    TmpFlag := ShowControlPoints;
    ShowControlPoints := False;
    TmpClipRect := RectToRect2D(ClientRect);
    while Tmp <> nil do
     begin
       if Tmp.IsVisible(CADRect, DrawMode) then
        begin
          fCADCmp2D.Layers.SetCanvas(TmpCanvas, Tmp.Layer);
          Tmp.Draw(TmpTransform, TmpCanvas, TmpClipRect, DrawMode);
        end;
       Tmp := TObject2D(TmpIter.Next);
     end;
    ShowControlPoints := TmpFlag;
  finally
    TmpIter.Free;
    TmpCanvas.Free;
  end;
end;

function TCADViewport2D.GetCopyRectViewportToScreen(CADRect: TRect2D;
                                                    const CanvasRect: TRect;
                                                    const Mode: TCanvasCopyMode): TTransf2D;
begin
  case Mode of
   cmNone: Result := GetVisualTransform2D(CADRect, CanvasRect, 0);
   cmAspect: Result := GetVisualTransform2D(CADRect, CanvasRect, AspectRatio);
  else
   Result := ViewportToScreenTransform;
  end;
end;

procedure TCADViewport2D.ZoomToExtension;
var
  NewWindow2D: TRect2D;
  Marg: TRealType;
begin
  if not Assigned(fCADCmp2D) then
   Exit;
  StopRepaint;
  if CADCmp2D.ObjectsCount = 0 then
   begin
     Repaint;
     Exit;
   end;
  NewWindow2D := fCADCmp2D.DrawingExtension;
  Marg := Abs(NewWindow2D.Right - NewWindow2D.Left) / 20;
  Marg := {%H-}MaxValue([Marg, Abs(NewWindow2D.Top - NewWindow2D.Bottom) / 20]);
  NewWindow2D.Left := NewWindow2D.Left - Marg;
  NewWindow2D.Right := NewWindow2D.Right + Marg;
  NewWindow2D.Bottom := NewWindow2D.Bottom - Marg;
  NewWindow2D.Top := NewWindow2D.Top + Marg;
  ZoomWindow(NewWindow2D);
end;

procedure TCADViewport2D.GroupObjects(const ResultLst: TGraphicObjList;
                                      Frm: TRect2D;
                                      const Mode: TGroupMode;
                                      const RemoveFromCAD: Boolean);
var
  Tmp: TObject2D;
  TmpObjectsIter: TExclusiveGraphicObjIterator;
begin
  if not Assigned(fCADCmp2D) or CADCmp.HasIterators then
   Exit;
  StopRepaint;
  Frm := ReOrderRect2D(Frm);
  ResultLst.FreeOnClear := RemoveFromCAD;
  TmpObjectsIter := fCADCmp2D.ObjectsExclusiveIterator;
  try
    Tmp := TmpObjectsIter.First as TObject2D;
    while Tmp <> nil do
     with (CADCmp.Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp.Enabled) and (Tmp is FPickFilter) and Tmp.IsVisible(VisualRect, DrawMode) then
         begin
           if (Mode = gmAllInside) and IsBoxAllInBox2D(Tmp.Box, Frm) then
            { The object is in the frame. }
            begin
              ResultLst.Add(Tmp);
              if RemoveFromCAD then
               begin
                 TmpObjectsIter.RemoveCurrent;
                 Tmp := TObject2D(TmpObjectsIter.Current);
                 Continue;
               end;
            end
           else if (Mode = gmCrossFrame) and IsBoxInBox2D(Tmp.Box, Frm) then
            begin
              ResultLst.Add(Tmp);
              if RemoveFromCAD then
               begin
                 TmpObjectsIter.RemoveCurrent;
                 Tmp := TObject2D(TmpObjectsIter.Current);
                 Continue;
               end;
            end;
         end;
        Tmp := TmpObjectsIter.Next as TObject2D;
      end;
  finally
    TmpObjectsIter.Free;
  end;
end;

function TCADViewport2D.PickObject(Pt: TPoint2D; Aperture: Word; FirstFound: Boolean; var NPoint: Integer): TObject2D;
var
  TmpNPoint: Integer;
  Tmp: TObject2D;
  WAperture, MinDist, Distance: TRealType;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  Result := nil;
  if (not Assigned(fCADCmp2D)) and (not Assigned(ViewportObjects)) then
   Exit;
  if CADCmp.HasIterators then
   Exit;
  if (ViewportObjects <> nil) then
   TmpIter := ViewportObjects.GetExclusiveIterator
  else
   TmpIter := fCADCmp2D.ObjectsExclusiveIterator;
  StopRepaint;
  try
    // Trasforma l'apertura in coordinate mondo.
    WAperture := GetPixelAperture.X * Aperture;
    MinDist := WAperture;
    NPoint := PICK_NOOBJECT;
    Tmp := TmpIter.Current as TObject2D;
    while Tmp <> nil do
     with (CADCmp.Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp is FPickFilter) and Tmp.IsVisible(VisualRect, DrawMode) then
         begin
           Distance := 0;
           TmpNPoint := Tmp.OnMe(Pt, WAperture, Distance);
           if (TmpNPoint >= NPoint) and (Distance <= MinDist) then
            begin
              Result := Tmp;
              NPoint := TmpNPoint;
              MinDist := Distance;
              if FirstFound then
               Break;
            end;
         end;
        Tmp := TmpIter.Next as TObject2D;
      end;
  finally
    TmpIter.Free;
  end;
end;

function TCADViewport2D.PickListOfObjects(const PickedObjects: TList; Pt: TPoint2D; Aperture: Word): Integer;
var
  Tmp: TObject2D;
  WAperture, Distance: TRealType;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  Result := 0;
  if (not Assigned(fCADCmp2D)) and (not Assigned(ViewportObjects)) then
   Exit;
  if CADCmp.HasIterators then
   Exit;
  if (ViewportObjects <> nil) then
   TmpIter := ViewportObjects.GetExclusiveIterator
  else
   TmpIter := fCADCmp2D.ObjectsExclusiveIterator;
  StopRepaint;
  try
    // Trasforma l'apertura in coordinate mondo.
    WAperture := GetPixelAperture.X * Aperture;
    Tmp := TmpIter.Current as TObject2D;
    while Tmp <> nil do
     with (CADCmp.Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp is FPickFilter) and Tmp.IsVisible(VisualRect, DrawMode) then
         begin
           Distance := 0;
           if Tmp.OnMe(Pt, WAperture, Distance) > PICK_NOOBJECT then
            begin
              PickedObjects.Add(Tmp);
              Inc(Result);
            end;
         end;
        Tmp := TmpIter.Next as TObject2D;
      end;
  finally
    TmpIter.Free;
  end;
end;

function TCADViewport2D.WorldToObject(const Obj: TObject2D; WPt: TPoint2D): TPoint2D;
begin
  WPt := CartesianPoint2D(WPt);
  if Obj.HasTransform then
   Result := TransformPoint2D(WPt, InvertTransform2D(Obj.ModelTransform))
  else
   Result := WPt;
end;

function TCADViewport2D.ObjectToWorld(const Obj: TObject2D; OPt: TPoint2D): TPoint2D;
begin
  OPt := CartesianPoint2D(OPt);
  if Obj.HasTransform then
   Result := TransformPoint2D(OPt, Obj.ModelTransform)
  else
   Result := OPt;
end;

procedure TCADViewport2D.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 Pos2D: TPoint2D;
begin
  Pos2D := ScreenToViewport(Point2D(X, Y));
  if (not DisableMouseEvents) and Assigned(FOnMouseMove2D) then
   FOnMouseMove2D(Self, Shift, Pos2D.X, Pos2D.Y, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TCADViewport2D.MouseDown(Button: TMouseButton; Shift: TShiftState;
                             X, Y: Integer);
var
 Pos2D: TPoint2D;
begin
  if (not DisableMouseEvents) and Assigned(FOnMouseDown2D) then
   begin
     Pos2D := ScreenToViewport(Point2D(X, Y));
     FOnMouseDown2D(Self, Button, Shift, Pos2D.X, Pos2D.Y, X, Y);
   end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCADViewport2D.MouseUp(Button: TMouseButton; Shift: TShiftState;
                            X, Y: Integer);
var
 Pos2D: TPoint2D;
begin
  if (not DisableMouseEvents) and Assigned(FOnMouseUp2D) then
   begin
     Pos2D := ScreenToViewport(Point2D(X, Y));
     FOnMouseUp2D(Self, Button, Shift, Pos2D.X, Pos2D.Y, X, Y);
   end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TCADViewport2D.BuildViewportTransform(var ViewWin: TRect2D;
                                               const ScreenWin: TRect;
                                               const AspectRatio: TRealType): TTransf2D;
begin
  Result := GetVisualTransform2D(ViewWin, ScreenWin, AspectRatio);
end;

// =====================================================================
// TObject3DHandler
// =====================================================================

constructor TObject3DHandler.Create(AObject: TObject3D);
begin
  inherited Create;

  fHandledObject := AObject;
  fRefCount := 1;
end;

destructor TObject3DHandler.Destroy;
begin
  if Assigned(fHandledObject) then
   fHandledObject.fHandler := nil;
  inherited;
end;

procedure TObject3DHandler.FreeInstance;
begin
  Dec(fRefCount);
  if fRefCount > 0 then
   Exit;
  inherited;
end;

// =====================================================================
// TObject3D
// =====================================================================

constructor TObject3D.Create(ID: LongInt);
begin
  { Initialize the object. }
  inherited Create(ID);
  FBox := Rect3D(0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
  FModelTransform := nil;
  fSavedTransform := nil;
  fDrawBoundingBox := True;
end;

destructor TObject3D.Destroy;
begin
  if Assigned(fHandler) then
   fHandler.Free;
  if fModelTransform <> nil then
   FreeMem(fModelTransform, SizeOf(TTransf3D));
  if fSavedTransform <> nil then
   FreeMem(fSavedTransform, SizeOf(TTransf2D));
  inherited Destroy;
end;

constructor TObject3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpTransf: TTransf3D;
  TmpTransfS: TTransf3DSingle;
begin
  inherited;
  with Stream do
   begin
     if( Version >= 'CAD423' ) then
      begin
        Read({%H-}TmpTransf, sizeof(TTransf3D));
        SaveTransform(TmpTransf);
      end
     else
      begin
        Read({%H-}TmpTransfS, sizeof(TTransf3DSingle));
        TmpTransf[1, 1] := TmpTransfS[1, 1]; TmpTransf[1, 2] := TmpTransfS[1, 2]; TmpTransf[1, 3] := TmpTransfS[1, 3]; TmpTransf[1, 4] := TmpTransfS[1, 4];
        TmpTransf[2, 1] := TmpTransfS[2, 1]; TmpTransf[2, 2] := TmpTransfS[2, 2]; TmpTransf[2, 3] := TmpTransfS[2, 3]; TmpTransf[2, 4] := TmpTransfS[2, 4];
        TmpTransf[3, 1] := TmpTransfS[3, 1]; TmpTransf[3, 2] := TmpTransfS[3, 2]; TmpTransf[3, 3] := TmpTransfS[3, 3]; TmpTransf[3, 4] := TmpTransfS[3, 4];
        TmpTransf[4, 1] := TmpTransfS[4, 1]; TmpTransf[4, 2] := TmpTransfS[4, 2]; TmpTransf[4, 3] := TmpTransfS[4, 3]; TmpTransf[4, 4] := TmpTransfS[4, 4];
        SaveTransform(TmpTransf);
      end;
   end;
  FBox := Rect3D(MinCoord, MinCoord, MinCoord, MaxCoord, MaxCoord, MaxCoord);
  fDrawBoundingBox := True;
end;

procedure TObject3D.SaveToStream(const Stream: TStream);
var
  TmpTransf: TTransf3D;
begin
  inherited SaveToStream(Stream);
  with Stream do
   begin
     SaveTransform(ModelTransform);
     TmpTransf := ModelTransform;
     Write(TmpTransf, SizeOf(TTransf3D));
   end;
end;

procedure TObject3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if Obj is TObject3D then
   begin
     SaveTransform(TObject3D(Obj).ModelTransform);
     fBox := TObject3D(Obj).fBox;
     if fHandler <> TObject3D(Obj).fHandler then
      SetSharedHandler(TObject3D(Obj).fHandler);
   end;
end;

function TObject3D.OnMe(P: TPoint3D; const NormTransf: TTransf3D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  TmpBox: TRect2D;
begin
  TmpBox := EnlargeBoxDelta2D(GetBox2DExtension3D(Box, NormTransf), Aperture);
  Distance := MaxCoord;
  Result := PICK_NOOBJECT;
  if not FEnabled then
   Exit;
  if IsPointInCartesianBox2D(Point3DToPoint2D(P), TmpBox) then
   begin
     Result := PICK_INBBOX;
     Distance := Aperture;
   end;
  if Assigned(fHandler) then
   Result := MaxIntValue([Result, fHandler.OnMe(Self, P, NormTransf, Aperture, Distance)]);
end;

procedure TObject3D.DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D;
                                      const Cnv: TDecorativeCanvas; const Width: Integer);
begin
  { Draw the bounding box. }
  with Cnv do
   if fDrawBoundingBox and (Canvas.Pen.Mode <> pmXOr) then
    DrawBoundingBox3D(Cnv, VRP, fBox, NormTransf, VT);
  if Assigned(fHandler) then
   fHandler.DrawControlPoints(Self, NormTransf, VRP, VT, Cnv, Width);
end;

function TObject3D.WorldToObject(const Pt: TPoint3D): TPoint3D;
begin
  if HasTransform then
   Result := TransformPoint3D(Pt, InvertTransform3D(ModelTransform))
  else
   Result := Pt;
end;

procedure TObject3D.MoveTo(ToPt, DragPt: TPoint3D);
var
  TmpTransf: TTransf3D;
begin
  { Traslate the bounds rect. }
  ToPt := CartesianPoint3D(ToPt);
  DragPt := CartesianPoint3D(DragPt);
  TmpTransf := Translate3D(ToPt.X - DragPt.X, ToPt.Y - DragPt.Y, ToPt.Z - DragPt.Z);
  { Traslate the object. }
  Transform(TmpTransf);
end;

procedure TObject3D.SetModelTransform(Transf: TTransf3D);
begin
  if IsSameTransform3D(Transf, IdentityTransf3D) then
   begin
     if (fModelTransform <> nil) then
      FreeMem(fModelTransform, SizeOf(TTransf3D));
     fModelTransform := nil;
   end
  else if fModelTransform = nil then
   begin
     GetMem(fModelTransform, SizeOf(TTransf3D));
     fModelTransform^ := Transf;
   end
  else
   fModelTransform^ := Transf;
  UpdateExtension(Self);
end;

function TObject3D.GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean;
begin
  Result := False;
end;

procedure TObject3D.RemoveTransform;
begin
  if (fSavedTransform <> nil) then
   FreeMem(fSavedTransform, SizeOf(TTransf3D));
  fSavedTransform := nil;
  SetModelTransform(IdentityTransf3D);
end;

procedure TObject3D.SaveTransform(const T: TTransf3D);
begin
  if IsSameTransform3D(T, IdentityTransf3D) then
   begin
     if (fSavedTransform <> nil) then
      FreeMem(fSavedTransform, SizeOf(TTransf3D));
     fSavedTransform := nil;
   end
  else if fSavedTransform = nil then
   begin
     GetMem(fSavedTransform, SizeOf(TTransf3D));
     fSavedTransform^ := T;
   end
  else
   fSavedTransform^ := T;
  if (fModelTransform <> nil) then
   FreeMem(fModelTransform, SizeOf(TTransf3D));
  fModelTransform := nil;
end;

function TObject3D.GetModelTransform: TTransf3D;
begin
  if fModelTransform <> nil then
   Result := fModelTransform^
  else
   Result := IdentityTransf3D;
  if Assigned(fSavedTransform) then
   Result := MultiplyTransform3D(fSavedTransform^, Result);
end;

function TObject3D.HasTransform: Boolean;
begin
  Result := (fModelTransform <> nil) or (fSavedTransform <> nil);
end;

procedure TObject3D.Transform(T: TTransf3D);
begin
  if fModelTransform <> nil then
   SetModelTransform(MultiplyTransform3D(fModelTransform^, T))
  else
   SetModelTransform(T);
end;

procedure TObject3D.ApplyTransform;
begin
  SaveTransform(ModelTransform);
end;

function TObject3D.IsVisible(const NormTransf: TTransf3D; const VRP: TPoint3D; const DrawMode: Integer): Boolean;
var
  TmpBox: TRect3D;
begin
  Result := False;
  if not Visible then
   Exit;
  TmpBox := TransformBoundingBox3D(FBox, NormTransf);
  Result := IsVisibleBoxNRC3D(TmpBox);
end;

procedure TObject3D.SetSharedHandler(const Hndl: TObject3DHandler);
begin
  if Assigned(fHandler) then
   fHandler.Free;
  fHandler := Hndl;
  if( Hndl <> nil ) then
   Inc(fHandler.fRefCount);
end;

procedure TObject3D.SetHandler(const Hndl: TObject3DHandlerClass);
begin
  if Assigned(fHandler) then
   fHandler.Free;
  if( Hndl <> nil ) then
   fHandler := Hndl.Create(Self)
  else
   fHandler := nil;
end;

// =====================================================================
// TContainer3D
// =====================================================================

constructor TContainer3D.Create(ID: LongInt; const Objs: array of TObject3D);
var
  Cont: Word;
begin
  inherited Create(ID);
  FObjects := TGraphicObjList.Create;
  FObjects.FreeOnClear := True;
  if Objs[Low(Objs)] <> nil then
   FBox := Objs[Low(Objs)].Box;
  { add eventually objects. }
  for Cont := Low(Objs) to High(Objs) do
   if Objs[Cont] <> nil then
    begin
      FObjects.Add(Objs[Cont]);
      { Get new FBox. }
      FBox := BoxOutBox3D(FBox, Objs[Cont].Box);
    end;
end;

procedure TContainer3D._UpdateExtension;
var
  TmpIter: TGraphicObjIterator;
begin
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  try
    if TmpIter.Count = 0 then
     begin
       fBox := Rect3D(0, 0, 0, 0, 0 , 0);
       Exit;
     end;
    fBox := TObject3D(TmpIter.Current).Box;
    while TmpIter.Next <> nil do
     { Get new FBox. }
     fBox := BoxOutBox3D(FBox, TObject3D(TmpIter.Current).Box);
    if HasTransform then
     fBox := TransformBoundingBox3D(fBox, ModelTransform);
  finally // Libera l'iterator
    TmpIter.Free;
  end;
end;

destructor TContainer3D.Destroy;
begin
  fObjects.Free;
  inherited Destroy;
end;

procedure TContainer3D.SaveToStream(const Stream: TStream);
var
  TmpObj: TObject3D;
  TmpLong: LongInt;
  TmpWord: Word;
  TmpIter: TGraphicObjIterator;
begin
  inherited SaveToStream(Stream);
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  with Stream do
   try
     { Write the number of objects in the container. }
     TmpLong := fObjects.Count;
     Write(TmpLong, SizeOf(TmpLong));
     { Now write the objects in the container. }
     TmpObj := TObject3D(TmpIter.First);
     while TmpObj <> nil do
      begin
        { Save the object. }
        TmpWord := CADSysFindClassIndex(TmpObj.ClassName);
        { Save the class index. }
        Write(TmpWord, SizeOf(TmpWord));
        TmpObj.SaveToStream(Stream);
        TmpObj := TObject3D(TmpIter.Next);
     end;
   finally
     TmpIter.Free;
   end;
end;

constructor TContainer3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
  TmpLong: LongInt;
  TmpWord: Word;
begin
  inherited;
  with Stream do
   begin
     { Read the number of objects in the container. }
     TmpLong := 0;
     Read(TmpLong, SizeOf(TmpLong));
     fObjects := TGraphicObjList.Create;
     fObjects.FreeOnClear := True;
     { Now read the object for the container. }
     while TmpLong > 0 do
      begin
        { Read the type of object. }
        TmpWord := 0;
        Read(TmpWord, SizeOf(TmpWord));
        { Retrive the class type from the registered classes. }
        TmpClass := CADSysFindClassByIndex(TmpWord);
        TmpObj := TmpClass.CreateFromStream(Stream, Version);
        TmpObj.UpdateExtension(Self);
        fObjects.Add(TmpObj);
        Dec(TmpLong);
     end;
   end;
  UpdateExtension(Self);
end;

procedure TContainer3D.Assign(const Obj: TGraphicObject);
var
  TmpIter: TGraphicObjIterator;
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TContainer3D then
   begin
     if fObjects = nil then
      begin
        fObjects := TGraphicObjList.Create;
        fObjects.FreeOnClear := True;
      end
     else
      fObjects.Clear;
     // Alloca un iterator locale
     TmpIter := TContainer3D(Obj).fObjects.GetIterator;
     try
      repeat
        TmpClass := TGraphicObjectClass(TmpIter.Current.ClassType);
        TmpObj := TmpClass.Create(TmpIter.Current.ID);
        TmpObj.Assign(TmpIter.Current);
        fObjects.Add(TmpObj);
      until TmpIter.Next = nil;
     finally
      TmpIter.Free;
     end;
   end;
end;

procedure TContainer3D.UpdateSourceReferences(const BlockList: TGraphicObjIterator);
var
  TmpObj: TObject3D;
  TmpIter: TGraphicObjIterator;
begin
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  try
    TmpObj := TObject3D(TmpIter.First);
    while TmpObj <> nil do
     begin
       if(TmpObj is TSourceBlock3D) then
        TSourceBlock3D(TmpObj).UpdateSourceReferences(BlockList)
       else if(TmpObj is TBlock3D) then
        TBlock3D(TmpObj).UpdateReference(BlockList);
       TmpObj := TObject3D(TmpIter.Next);
     end;
  finally
    TmpIter.Free;
  end;
  UpdateExtension(Self);
end;

procedure TContainer3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D;
                            const VT: TTransf2D; const Cnv: TDecorativeCanvas;
                            const DrawMode: Integer);
var
  TmpObj: TObject3D;
  TmpIter: TGraphicObjIterator;
  TmpTransf: TTransf3D;
  TmpPen: TPen;
  TmpBrush: TBrush;
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     with Cnv do
      DrawBoundingBox3D(Cnv, VRP, fBox, NormTransf, VT);
     Exit;
   end;
  // Crea un iterator temporaneo.
  TmpIter := fObjects.GetIterator;
  TmpPen := TPen.Create;
  TmpBrush := TBrush.Create;
  try
    if HasTransform then
     TmpTransf := MultiplyTransform3D(ModelTransform, NormTransf)
    else
     TmpTransf := NormTransf;
    TmpObj := TObject3D(TmpIter.First);
    with Cnv do
     begin
       TmpPen.Assign(Canvas.Pen);
       TmpBrush.Assign(Canvas.Brush);
       while TmpObj <> nil do
        begin
          Canvas.Pen.Assign(TmpPen);
          Canvas.Brush.Assign(TmpBrush);
          TmpObj.Draw(TmpTransf, VRP, VT, Cnv, DrawMode);
          TmpObj := TObject3D(TmpIter.Next);
        end;
     end;
  finally
    TmpPen.Free;
    TmpBrush.Free;
    TmpIter.Free;
  end;
end;

function TContainer3D.OnMe(P: TPoint3D; const NT: TTransf3D; Aperture: TRealType;
                           var Distance: TRealType): Integer;
var
  TmpObj: TObject3D;
  MinDist: TRealType;
  TmpIter: TGraphicObjIterator;
  TmpTransf: TTransf3D;
begin
  Result := inherited OnMe(P, NT, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     // Crea un iterator temporaneo.
     TmpIter := fObjects.GetIterator;
     try
       if HasTransform then
        TmpTransf := MultiplyTransform3D(ModelTransform, NT)
       else
        TmpTransf := NT;
       { Check all the objects in the container. }
       TmpObj := TObject3D(TmpIter.First);
       MinDist := 2.0 * Aperture;
       while TmpObj <> nil do
        begin
          if (TmpObj.OnMe(P, TmpTransf, Aperture, Distance) >= PICK_INOBJECT) and
             (Distance < MinDist) then
           begin
             MinDist := Distance;
             Result := TmpObj.ID;
           end;
          TmpObj := TObject3D(TmpIter.Next);
        end;
       Distance := MinDist;
     finally
       TmpIter.Free;
     end;
  end;
end;

procedure TContainer3D.DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D;
                                         const VT: TTransf2D; const Cnv: TDecorativeCanvas;
                                         const Width: Integer);
begin
  if fObjects.Count > 0 then
   inherited;
end;

// =====================================================================
// TSourceBlock3D
// =====================================================================

constructor TSourceBlock3D.Create(ID: LongInt; const Name: TSourceBlockName; const Objs: array of TObject3D);
begin
  inherited Create(ID, Objs);
  FName := Name;
  FNReference := 0;
end;

destructor TSourceBlock3D.Destroy;
begin
  if fNReference > 0 then
   Raise ECADSourceBlockIsReferenced.Create('TSourceBlock3D.Destroy: This source block is referenced and cannot be deleted');
  inherited Destroy;
end;

constructor TSourceBlock3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  Stream.Read(fToBeSaved, SizeOf(fToBeSaved));
  Stream.Read(fLibraryBlock, SizeOf(fLibraryBlock));
  Stream.Read(fName, SizeOf(fName));
  fNReference := 0;
end;

procedure TSourceBlock3D.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(fToBeSaved, SizeOf(fToBeSaved));
  Stream.Write(fLibraryBlock, SizeOf(fLibraryBlock));
  Stream.Write(FName, SizeOf(FName));
end;

procedure TSourceBlock3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TSourceBlock3D then
   fToBeSaved := TSourceBlock3D(Obj).fToBeSaved;
end;

// =====================================================================
// TBlock3D
// =====================================================================

procedure TBlock3D.SetSourceBlock(Source: TSourceBlock3D);
begin
  if not Assigned(Source) then
   Raise Exception.Create('TBlock3D.SetSourceBlock: Invalid parameter');
  if (FSourceBlock <> Source) then
   begin
     if Assigned(FSourceBlock) and (fSourceBlock.fNReference > 0) then
      Dec(FSourceBlock.FNReference);
     FSourceBlock := Source;
     FSourceName := Source.Name;
     Inc(FSourceBlock.FNReference);
     UpdateExtension(Self);
   end;
end;

procedure TBlock3D.SetOriginPoint(Pt: TPoint3D);
begin
  FOriginPoint := Pt;
  UpdateExtension(Self);
end;

constructor TBlock3D.Create(ID: LongInt; Source: TSourceBlock3D);
begin
  inherited Create(ID);

  if not Assigned(Source) then
   Raise Exception.Create('TBlock3D.Create: Invalid parameter');
  FOriginPoint := Point3D(0, 0, 0);
  FSourceBlock := Source;
  FSourceName := Source.Name;
  FBox := Source.Box;
  Inc(FSourceBlock.FNReference);
  UpdateExtension(Self);
end;

destructor TBlock3D.Destroy;
begin
  if Assigned(FSourceBlock) and (fSourceBlock.fNReference > 0) then
   Dec(FSourceBlock.FNReference);
  inherited Destroy;
end;

constructor TBlock3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpPt: TPoint3D;
  TmpPtS: TPoint3DSingle;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     { TCADCmp will use the value of FSourceID
       to find out the reference of the source block. }
     Read(FSourceName, SizeOf(FSourceName));
     if( Version >= 'CAD423' ) then
      begin
        Read(FOriginPoint, SizeOf(FOriginPoint));
      end
     else
      begin
        TmpPts.X:=0;TmpPts.Y:=0;TmpPts.Z:=0;TmpPts.W:=0;
        Read(TmpPtS, SizeOf(TmpPtS));
        FOriginPoint.X := TmpPtS.X;
        FOriginPoint.Y := TmpPtS.Y;
        FOriginPoint.Z := TmpPtS.Z;
        FOriginPoint.W := TmpPtS.W;
      end;
   end;
  if HasTransform then
   TmpPt := TransformPoint3D(FOriginPoint, ModelTransform)
  else
   TmpPt := FOriginPoint;
  FBox := BoxOutBox3D(FBox, Rect3D(TmpPt.X, TmpPt.Y, TmpPt.X, TmpPt.Y, TmpPt.Z, TmpPt.Z));
end;

procedure TBlock3D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     { Save the ID of the source block. }
     Write(FSourceName, SizeOf(FSourceName));
     Write(FOriginPoint, SizeOf(FOriginPoint));
   end;
end;

procedure TBlock3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TBlock3D then
   begin
     if Assigned(fSourceBlock) and (fSourceBlock.FNReference > 0) then
      Dec(fSourceBlock.FNReference);
     fSourceBlock := TBlock3D(Obj).fSourceBlock;
     fSourceName := fSourceBlock.Name;
     fOriginPoint := TBlock3D(Obj).OriginPoint;
     Inc(fSourceBlock.FNReference);
     UpdateExtension(Self);
   end;
end;

procedure TBlock3D.UpdateReference(const BlockList: TGraphicObjIterator);
var
  TmpSource: TSourceBlock3D;
begin
  TmpSource := BlockList.First as TSourceBlock3D;
  while TmpSource <> nil do
   begin
     if TmpSource.Name = FSourceName then
      begin
        FSourceBlock := TmpSource;
        UpdateExtension(Self);
        Exit;
      end;
     TmpSource := BlockList.Next as TSourceBlock3D;
   end;
  Raise ECADListObjNotFound.Create('TBlock3D.UpdateReference: Source block not found');
end;

procedure TBlock3D._UpdateExtension;
begin
  if not Assigned(FSourceBlock) then Exit;
  if HasTransform then
   fBox := TransformBoundingBox3D(fSourceBlock.Box, ModelTransform)
  else
   fBox := fSourceBlock.Box;
end;

procedure TBlock3D.DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D;
                                     const Cnv: TDecorativeCanvas; const Width: Integer);
var
  TmpPoint3D: TPoint3D;
  TmpPoint2D: TPoint2D;
begin
  inherited;
  if HasTransform then
   TmpPoint3D := TransformPoint3D(fOriginPoint, MultiplyTransform3D(ModelTransform, NormTransf))
  else
   TmpPoint3D := TransformPoint3D(fOriginPoint, NormTransf);
  TmpPoint2D := TransformPoint2D(Point3DToPoint2D(TmpPoint3D), VT);
  DrawPlaceHolder(Cnv, Round(TmpPoint2D.X), Round(TmpPoint2D.Y), Width);
end;

procedure TBlock3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
begin
  if not Assigned(FSourceBlock) then
   Exit;
  if HasTransform then
   FSourceBlock.Draw(MultiplyTransform3D(ModelTransform, NormTransf), VRP, VT, Cnv, DrawMode)
  else
   FSourceBlock.Draw(NormTransf, VRP, VT, Cnv, DrawMode)
end;

function TBlock3D.OnMe(P: TPoint3D; const NT: TTransf3D; Aperture: TRealType;
                       var Distance: TRealType): Integer;
var
  TmpPt3D: TPoint3D;
  TmpTransf: TTransf3D;
begin
  Result := PICK_NOOBJECT;
  if not Assigned(FSourceBlock) then Exit;
  Result := inherited OnMe(P, NT, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if HasTransform then
      TmpTransf := MultiplyTransform3D(ModelTransform, NT)
     else
      TmpTransf := NT;
     Result := FSourceBlock.OnMe(P, TmpTransf, Aperture, Distance);
   end
  else
   begin
     if HasTransform then
      TmpPt3D := TransformPoint3D(FOriginPoint, ModelTransform)
     else
      TmpPt3D := FOriginPoint;
     if NearPoint3D(TmpPt3D, P, Aperture, Distance, NT) then
      Result := -1
   end;
end;

// =====================================================================
// TCADCmp3D
// =====================================================================

procedure TCADCmp3D.SaveObjectsToStream(const Stream: TStream);
var
  TmpObj: TObject3D;
  TmpWord: Word;
  TmpLong, TmpObjPerc: LongInt;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := ObjectList.GetPrivilegedIterator;
  with Stream do
   try
     { Save the objects. }
     TmpLong := ObjectsCount;
     if TmpLong > 0 then
      TmpObjPerc := 100 div TmpLong
     else
      TmpObjPerc := 0;
     Write(TmpLong, SizeOf(TmpLong));
     TmpObj := TmpIter.First as TObject3D;
     while TmpObj <> nil do
      begin
        if Layers[TmpObj.Layer].Streamable and TmpObj.fToBeSaved then
         begin
           TmpWord := CADSysFindClassIndex(TmpObj.ClassName);
           { Save the class index. }
           Write(TmpWord, SizeOf(TmpWord));
           TmpObj.SaveToStream(Stream);
           if Assigned(OnSaveProgress) then
            OnSaveProgress(Self, 100 - TmpObjPerc * TmpLong);
           Dec(TmpLong);
         end;
        TmpObj := TmpIter.Next as TObject3D;
      end;
     { End the list of objects if not all objects were saved. }
     if TmpLong > 0 then
      begin
        TmpWord := 65535;
        Write(TmpWord, SizeOf(TmpWord));
      end;
   finally
     TmpIter.Free;
   end;
end;

{$WARNINGS OFF}
procedure TCADCmp3D.LoadObjectsFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
  TmpLong, TmpObjPerc: LongInt;
  TmpWord: Word;
  TmpBlocksIter: TExclusiveGraphicObjIterator;
begin
  TmpBlocksIter := SourceBlocksExclusiveIterator;
  with Stream do
   try
     TmpLong := 0;
     Read(TmpLong, SizeOf(TmpLong));
     if TmpLong > 0 then
      TmpObjPerc := 100 div TmpLong
     else
      TmpObjPerc := 0;
     while TmpLong > 0 do
      begin
        { Read the type of object and the length of the record. }
        TmpWord := 0;
        Read(TmpWord, SizeOf(TmpWord));
        if TmpWord = 65535 then
         { End prematurely. }
         Break;
        Dec(TmpLong);
        { Retrive the class type from the registered classes. }
        try
         TmpClass := CADSysFindClassByIndex(TmpWord);
        except
         on ECADObjClassNotFound do
          begin
            ShowMessage('Object class not found. Object not load');
            Break;
          end;
        end;
        TmpObj := TmpClass.CreateFromStream(Stream, Version);
        if Assigned(OnLoadProgress) then
         OnLoadProgress(Self, TmpObjPerc);
        if TmpObj is TContainer3D then
         try
           TContainer3D(TmpObj).UpdateSourceReferences(TmpBlocksIter);
         except
          on ECADListObjNotFound do
           begin
             ShowMessage('Source block not found. The block will not be loaded');
             TmpObj.Free;
             Continue;
           end;
         end
        else if TmpObj is TBlock3D then
         try
           TBlock3D(TmpObj).UpdateReference(TmpBlocksIter);
         except
          on ECADListObjNotFound do
           begin
             ShowMessage('Source block not found. The block will not be loaded');
             TmpObj.Free;
             Continue;
           end;
         end;
        CurrentLayer := TmpObj.Layer;
        inherited AddObject(-1, TGraphicObject(TmpObj));
     end;
   finally
     TmpBlocksIter.Free;
   end;
end;
{$WARNINGS ON}

procedure TCADCmp3D.SaveBlocksToStream(const Stream: TStream; const AsLibrary: Boolean);
var
  TmpObj: TSourceBlock3D;
  TmpWord: Word;
  TmpLong, TmpPos: LongInt;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := BlockList.GetPrivilegedIterator;
  with Stream do
   try
     TmpLong := SourceBlocksCount;
     TmpPos := Stream.Position;
     Write(TmpLong, SizeOf(TmpLong));
     TmpObj := TmpIter.First as TSourceBlock3D;
     TmpLong := 0;
     while TmpObj <> nil do
      begin
        if TmpObj.ToBeSaved and
           not(TmpObj.IsLibraryBlock xor AsLibrary) then
         begin
           TmpWord := CADSysFindClassIndex(TmpObj.ClassName);
           { Save the class index. }
           Write(TmpWord, SizeOf(TmpWord));
           TmpObj.SaveToStream(Stream);
           Inc(TmpLong);
         end;
        TmpObj := TmpIter.Next as TSourceBlock3D;
      end;
     Seek(TmpPos, soFromBeginning);
     Write(TmpLong, SizeOf(TmpLong));
     Seek(0, soFromEnd);
   finally
     TmpIter.Free;
   end;
end;

{$WARNINGS OFF}
procedure TCADCmp3D.LoadBlocksFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
  TmpLong: LongInt;
  TmpWord: Word;
  TmpBlocksIter: TGraphicObjIterator;
begin
  with Stream do
   begin
     { Load the source blocks. }
     TmpLong := 0;
     Read(TmpLong, SizeOf(TmpLong));
     while TmpLong > 0 do
      begin
        { Read the type of object and the length of the record. }
        TmpWord := 0;
        Read(TmpWord, SizeOf(TmpWord));
        Dec(TmpLong);
        { Retrive the class type from the registered classes. }
        try
         TmpClass := CADSysFindClassByIndex(TmpWord);
        except
         on ECADObjClassNotFound do
          begin
            ShowMessage('Object class not found. Object not load');
            Continue;
          end;
        end;
        TmpObj := TmpClass.CreateFromStream(Stream, Version);
        TmpBlocksIter := SourceBlocksIterator;
        try
         TSourceBlock3D(TmpObj).UpdateSourceReferences(TmpBlocksIter);
        except
         on ECADListObjNotFound do
          begin
            ShowMessage('Source block not found. The block will not be loaded');
            TmpObj.Free;
            TmpBlocksIter.Free;
            Continue;
          end;
        end;
        TmpBlocksIter.Free;
        AddSourceBlock(TSourceBlock3D(TmpObj));
      end;
   end;
end;
{$WARNINGS ON}

function TCADCmp3D.AddSourceBlock(Obj: TSourceBlock3D): TSourceBlock3D;
begin
  Result := Obj;
  inherited AddSourceBlock(-1, Obj);
end;

procedure TCADCmp3D.DeleteSourceBlock(const SrcName: TSourceBlockName);
var
  TmpSource: TSourceBlock3D;
begin
  TmpSource := TSourceBlock3D(FindSourceBlock(SrcName));
  if TmpSource.NumberOfReferences > 0 then
   Raise ECADSysException.Create('TCADCmp3D.DeleteSourceBlock: Remove the references before the source');
  DeleteSourceBlockByID(TmpSource.ID);
end;

function TCADCmp3D.GetSourceBlock(ID: LongInt): TSourceBlock3D;
begin
  Result := inherited GetSourceBlock(ID) as TSourceBlock3D;
end;

function TCADCmp3D.FindSourceBlock(const SrcName: TSourceBlockName): TSourceBlock3D;
var
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := SourceBlocksIterator;
  try
    Result := TmpIter.First as TSourceBlock3D;
    while Result <> nil do
     begin
       if SrcName = Result.Name then
        Exit;
       Result := TmpIter.Next as TSourceBlock3D;
     end;
  finally
    TmpIter.Free;
  end;
  Raise ECADListObjNotFound.Create('TCADCmp3D.FindSourceBlock: Source block not found');
end;

function TCADCmp3D.BlockObjects(const SrcName: TSourceBlockName; const Objs: TGraphicObjIterator): TSourceBlock3D;
var
  TmpObj: TObject3D;
begin
  Result := TSourceBlock3D.Create(0, SrcName, [nil]);
  try
    TmpObj := Objs.First as TObject3D;
    while TmpObj <> nil do
     begin
       Result.Objects.Add(TmpObj);
       TmpObj := Objs.Next as TObject3D;
     end;
    AddSourceBlock(Result);
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TCADCmp3D.DeleteSavedSourceBlocks;
var
  TmpObj: TGraphicObject;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := SourceBlocksExclusiveIterator;
  try
    TmpObj := TmpIter.First;
    while Assigned(TmpObj) do
     if TSourceBlock3D(TmpObj).ToBeSaved and not TSourceBlock3D(TmpObj).IsLibraryBlock then
      try
        TmpIter.DeleteCurrent;
        TmpObj := TmpIter.Current;
      except
        TmpObj := TmpIter.Next;
      end
     else
      TmpObj := TmpIter.Next;
  finally
    TmpIter.Free;
  end;
end;

procedure TCADCmp3D.DeleteLibrarySourceBlocks;
var
  TmpObj: TGraphicObject;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := SourceBlocksExclusiveIterator;
  try
    TmpObj := TmpIter.First;
    while Assigned(TmpObj) do
     if TSourceBlock3D(TmpObj).IsLibraryBlock then
      try
        TmpIter.DeleteCurrent;
        TmpObj := TmpIter.Current;
      except
        TmpObj := TmpIter.Next;
      end
     else
      TmpObj := TmpIter.Next;
  finally
    TmpIter.Free;
  end;
end;

function TCADCmp3D.AddObject(ID: LongInt; const Obj: TObject3D): TObject3D;
begin
  Result := Obj;
  inherited AddObject(ID, TGraphicObject(Obj));
end;

function TCADCmp3D.InsertObject(ID, IDInsertPoint: LongInt; Obj: TObject3D): TObject3D;
begin
  Result := Obj;
  inherited InsertObject(ID, IDInsertPoint, TGraphicObject(Obj));
end;

function TCADCmp3D.AddBlock(ID: LongInt; const SrcName: TSourceBlockName): TObject3D;
var
  Tmp: TBlock3D;
  TmpSource: TSourceBlock3D;
begin
  TmpSource := FindSourceBlock(SrcName);
  Tmp := TBlock3D.Create(ID, TmpSource);
  try
    AddObject(ID, Tmp);
    Result := Tmp;
  except
    Tmp.Free;
    Result := nil;
  end;
end;

function TCADCmp3D.GetObject(ID: LongInt): TObject3D;
begin
  Result := inherited GetObject(ID) as TObject3D;
end;

procedure TCADCmp3D.TransformObjects(ListOfObj: array of LongInt; T: TTransf3D);
var
  Cont: LongInt;
  Tmp: TObject3D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := ObjectsExclusiveIterator;
  try
    if ListOfObj[Low(ListOfObj)] < 0 then
     begin
       { Apply trasform to all objects. }
       Tmp := TmpIter.First as TObject3D;
       while Tmp <> nil do
        begin
          Tmp.Transform(T);
          Tmp.ApplyTransform;
          Tmp := TmpIter.Next as TObject3D;
        end;
     end
    else
     for Cont := Low(ListOfObj) to High(ListOfObj) do
      begin
        try
         Tmp := TObject3D(TmpIter.Search(ListOfObj[Cont]));
        except
         on Exception do Raise ECADListObjNotFound.Create('TCADCmp3D.TransformObjects: Object not found');
        end;
        if Tmp <> nil then
         begin
           Tmp.Transform(T);
           Tmp.ApplyTransform;
         end;
      end;
  finally
    TmpIter.Free;
  end;
  if RepaintAfterTransform then RepaintViewports;
end;

procedure TCADCmp3D.RedrawObject(Obj: TObject3D);
begin
  inherited RedrawObject(Obj);
end;

function TCADCmp3D.GetExtension: TRect3D;
var
  Tmp: TObject3D;
  TmpIter: TGraphicObjIterator;
begin
  if (ObjectsCount = 0) or (IsBlocked) then
   Exit;
  TmpIter := ObjectsIterator;
  try
    Tmp := TmpIter.First as TObject3D;
    Result := Rect3D(MaxCoord, MaxCoord, MaxCoord, MinCoord, MinCoord, MinCoord);
    while Tmp <> nil do
     begin
       if fLayers[Tmp.Layer].fVisible then
        begin
          try
           Tmp.UpdateExtension(Self);
          finally
          end;
          { Set the new extension if necessary. }
          if Tmp.Box.FirstEdge.X < Result.FirstEdge.X then
           Result.FirstEdge.X := Tmp.Box.FirstEdge.X;
          if Tmp.Box.SecondEdge.X > Result.SecondEdge.X then
           Result.SecondEdge.X := Tmp.Box.SecondEdge.X;
          if Tmp.Box.FirstEdge.Y < Result.FirstEdge.Y then
           Result.FirstEdge.Y := Tmp.Box.FirstEdge.Y;
          if Tmp.Box.SecondEdge.Y > Result.SecondEdge.Y then
           Result.SecondEdge.Y := Tmp.Box.SecondEdge.Y;
          if Tmp.Box.FirstEdge.Z < Result.FirstEdge.Z then
           Result.FirstEdge.Z := Tmp.Box.FirstEdge.Z;
          if Tmp.Box.SecondEdge.Z > Result.SecondEdge.Z then
           Result.SecondEdge.Z := Tmp.Box.SecondEdge.Z;
        end;
       Tmp := TmpIter.Next as TObject3D;
     end;
  finally
    TmpIter.Free;
  end;
end;

// =====================================================================
// TCADViewport3D
// =====================================================================

procedure TCADViewport3D.SetCADCmp(Cad: TCADCmp);
begin
  if (CAD <> fCADCmp3D) then
   begin
     inherited SeTCADCmp(Cad);
     fCADCmp3D := Cad as TCADCmp3D;
   end;
end;

procedure TCADViewport3D.CopyBackBufferRectOnCanvas(const Rect: TRect; const GenEvent: Boolean);
begin
  inherited;
  if fAxis.Visible and HandleAllocated then
   fAxis.PaintAxes(Point(ClientRect.Left, ClientRect.Bottom), Canvas);
end;

procedure TCADViewport3D.SetCADCmp3D(Cad: TCADCmp3D);
begin
  SetCADCmp(Cad);
end;

// Trasforma da WRC a VRC. Aggiorna fViewOrientation.
function TCADViewport3D.BuildViewOrientationTransform(const LVRP: TPoint3D; const LVUP, LVPN: TVector3D): TTransf3D;
begin
  Result := ViewOrientationTransform3D(LVRP, LVPN, LVUP);
end;

procedure TCADViewport3D.UpdateViewportOrientation;
var
  OldHand: TNotifyEvent;
begin
  StopRepaint;
  try
    fViewOrientation := BuildViewOrientationTransform(fVRP, fVUP, fVPN);
    if Assigned(fOnViewProjectionChanged) then
     fOnViewProjectionChanged(Self);
  except
  end;
  OldHand := OnViewMappingChanged;
  try
    // in questo caso non sto effettivamente cambiando il mapping.
    OnViewMappingChanged := nil;
    UpdateViewportTransform;
  finally
    OnViewMappingChanged := OldHand;
  end;
  if HandleAllocated then
   Repaint;
end;

procedure TCADViewport3D.SetVPN(V: TVector3D);
begin
  if not IsSameVector3D(V, fVPN) then
   begin
     fVPN := NormalizeVector3D(V);
     UpdateViewportOrientation;
   end;
end;

procedure TCADViewport3D.SetVRP(P: TPoint3D);
begin
  if not IsSamePoint3D(P, fVRP) then
   begin
     fVRP := P;
     UpdateViewportOrientation;
   end;
end;

procedure TCADViewport3D.SetVUP(V: TVector3D);
begin
  if not IsSameVector3D(V, fVUP) then
   begin
     fVUP := NormalizeVector3D(V);
     UpdateViewportOrientation;
   end;
end;

procedure TCADViewport3D.SetFClip(F: TRealType);
begin
  if F <> fFrontPlane then
   begin
     fFrontPlane := F;
     if fFrontPlane < fBackPlane then
      fBackPlane := F;
     UpdateViewportTransform;
   end;
end;

procedure TCADViewport3D.SetBClip(B: TRealType);
begin
  if B <> fBackPlane then
   begin
     fBackPlane := B;
     if fFrontPlane < fBackPlane then
      fFrontPlane := B;
     UpdateViewportTransform;
   end;
end;

function TCADViewport3D.GetPRPlanePosition: TPoint3D;
begin
  Result := fVRP;
end;

function TCADViewport3D.GetViewPoint: TPoint3D;
begin
  Result := ExtrudePoint3D(fVRP, fVPN, -fPRPlaneViewPointDistance);
end;

function TCADViewport3D.GetCameraUP: TVector3D;
begin
  Result := fVUP;
end;

constructor TCADViewport3D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fPickFilter := TObject3D;
  fAxis := TCAD3DAxis.Create(Self);

  fVRP := Point3D(0, 0, 0);
  fVPN := Versor3D(0, -1, 0);
  fVUP := Versor3D(0, 0, 1);
  fBackPlane := -0.99;
  fFrontPlane := 10000;
end;

destructor TCADViewport3D.Destroy;
begin
  fAxis.Free;

  inherited;
end;

function TCADViewport3D.BuildViewportTransform(var ViewWin: TRect2D;
                                               const ScreenWin: TRect;
                                               const AspectRatio: TRealType): TTransf2D;
var
  TmpS: TRect2D;
begin
  // Crea la trasformazione da ViewPlane a Schermo. Viene usata per zoom e interazioni con l'utente.
  Result := GetVisualTransform2D(ViewWin, ScreenWin, AspectRatio);
  // Crea la trasformazione di normalizzazione. Usata per disegnare.
  if fBackPlane <> fFrontPlane then
   fViewNormalization := MultiplyTransform3D(fViewOrientation, BuildViewNormalizationTransform(ViewWin, fFrontPlane, fBackPlane));
  // Aggiunge la mappatura da NRC a Schermo.
  TmpS := Rect2D(-1.0, -1.0, 1.0, 1.0);
  fViewMapping := GetVisualTransform2D(TmpS, ScreenWin, 0.0);
end;

procedure TCADViewport3D.DrawObject(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D);
begin
  if not TObject3D(Obj).IsVisible(fViewNormalization, fVRP, DrawMode) then
   Exit;
  with Obj as TObject3D do
   try
     if (CADCmp.Layers.SetCanvas(Cnv, Layer)) then
      begin
        Draw(fViewNormalization, fVRP, fViewMapping, Cnv, DrawMode);
        if ShowControlPoints then
         begin
           Cnv.Canvas.Pen.Color := RubberPenColor;
           Cnv.Canvas.Pen.Width := 1;
           Cnv.Canvas.Brush.Color := ControlPointsColor;
           DrawControlPoints(fViewNormalization, fVRP, fViewMapping, Cnv, ControlPointsWidth);
         end;
      end;
   finally
   end;
end;

procedure TCADViewport3D.DrawObjectWithRubber(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D);
begin
  if not TObject3D(Obj).IsVisible(fViewNormalization, fVRP, DrawMode) then
   Exit;
  with (Obj as TObject3D) do
   try
     Cnv.Canvas.Pen.Assign(RubberPen);
     Cnv.Canvas.Brush.Color := RubberPen.Color;
     Cnv.Canvas.Brush.Style := bsSolid;
     Draw(fViewNormalization, fVRP, fViewMapping, Cnv, DrawMode);
     if ShowControlPoints then
      begin
        Cnv.Canvas.Pen.Assign(RubberPen);
        Cnv.Canvas.Brush.Color := ControlPointsColor;
        DrawControlPoints(fViewNormalization, fVRP, fViewMapping, Cnv, ControlPointsWidth);
      end;
   finally
   end;
end;

procedure TCADViewport3D._DrawObject2D(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas);
var
  TmpClipRect: TRect2D;
begin
  if not TObject2D(Obj).IsVisible(VisualRect, DrawMode) then
   Exit;
  TmpClipRect := RectToRect2D(ClientRect);
  with Obj as TObject2D do
   try
     if (CADCmp.Layers.SetCanvas(Cnv, Layer)) then
      begin
        Draw(ViewportToScreenTransform, Cnv, TmpClipRect, DrawMode);
        if ShowControlPoints then
         begin
           Cnv.Canvas.Pen.Color := RubberPenColor;
           Cnv.Canvas.Pen.Width := 1;
           Cnv.Canvas.Brush.Color := ControlPointsColor;
           DrawControlPoints(ViewportToScreenTransform, Cnv, TmpClipRect, ControlPointsWidth);
         end;
      end;
   finally
   end;
end;

procedure TCADViewport3D._DrawObjectWithRubber2D(const Obj: TGraphicObject; const Cnv: TDecorativeCanvas);
var
  TmpClipRect: TRect2D;
begin
  if not TObject2D(Obj).IsVisible(VisualRect, DrawMode) then
   Exit;
  with Obj as TObject2D, Cnv do
   try
     TmpClipRect := RectToRect2D(ClientRect);
     Canvas.Pen.Assign(RubberPen);
     Canvas.Brush.Color := RubberPen.Color;
     Canvas.Brush.Style := bsSolid;
     Draw(ViewportToScreenTransform, Cnv, TmpClipRect, DrawMode);
     if ShowControlPoints then
      begin
        Cnv.Canvas.Pen.Assign(RubberPen);
        Cnv.Canvas.Pen.Width := 1;
        Cnv.Canvas.Brush.Color := ControlPointsColor;
        DrawControlPoints(ViewportToScreenTransform, Cnv, TmpClipRect, ControlPointsWidth);
      end;
   finally
   end;
end;

procedure TCADViewport3D.DrawObject2D(const Obj: TObject2D; const CtrlPts: Boolean);
var
  TmpFlag: Boolean;
begin
  if not Assigned(fCADCmp3D) then Exit;
  TmpFlag := ShowControlPoints;
  ShowControlPoints := ShowControlPoints or CtrlPts;
  try
    DrawObject(Obj, OffScreenCanvas, RectToRect2D(ClientRect));
    _DrawObject2D(Obj, OnScreenCanvas);
  finally
    ShowControlPoints := TmpFlag;
  end;
end;

procedure TCADViewport3D.DrawObject2DWithRubber(const Obj: TObject2D; const CtrlPts: Boolean);
var
  TmpFlag: Boolean;
begin
  if not Assigned(fCADCmp3D) then Exit;
  TmpFlag := ShowControlPoints;
  ShowControlPoints := ShowControlPoints or CtrlPts;
  try
    _DrawObjectWithRubber2D(Obj, OnScreenCanvas);
  finally
    ShowControlPoints := TmpFlag;
  end;
end;

procedure TCADViewport3D.DrawObject3D(const Obj: TObject3D; const CtrlPts: Boolean);
var
  TmpFlag: Boolean;
begin
  if not Assigned(fCADCmp3D) then Exit;
  TmpFlag := ShowControlPoints;
  ShowControlPoints := ShowControlPoints or CtrlPts;
  try
    DrawObject(Obj, OffScreenCanvas, RectToRect2D(ClientRect));
    DrawObject(Obj, OnScreenCanvas, RectToRect2D(ClientRect));
  finally
    ShowControlPoints := TmpFlag;
  end;
end;

procedure TCADViewport3D.DrawObject3DWithRubber(const Obj: TObject3D; const CtrlPts: Boolean);
var
  TmpFlag: Boolean;
begin
  if not Assigned(fCADCmp3D) then Exit;
  TmpFlag := ShowControlPoints;
  ShowControlPoints := ShowControlPoints or CtrlPts;
  try
    DrawObjectWithRubber(Obj, OnScreenCanvas, RectToRect2D(ClientRect));
  finally
    ShowControlPoints := TmpFlag;
  end;
end;

procedure TCADViewport3D.CopyRectToCanvas(CADRect: TRect2D;
                                          const CanvasRect: TRect;
                                          const Cnv: TCanvas;
                                          const Mode: TCanvasCopyMode);
var
  Tmp: TObject3D;
  TmpTransform: TTransf2D;
  TmpWin: TRect2D;
  TmpFlag: Boolean;
  TmpIter: TGraphicObjIterator;
  TmpCanvas: TDecorativeCanvas;
begin
  if not Assigned(fCADCmp3D) or fCADCmp3D.IsBlocked then
   Exit;
  TmpCanvas := TDecorativeCanvas.Create(Cnv);
  if (ViewportObjects <> nil) then
   TmpIter := ViewportObjects.GetIterator
  else
   TmpIter := fCADCmp3D.ObjectsIterator;
  TmpWin := Rect2D(-1, -1, 1, 1);
  try
    case Mode of
     cmNone: TmpTransform := GetVisualTransform2D(TmpWin, CanvasRect, 0);
     cmAspect: TmpTransform := GetVisualTransform2D(TmpWin, CanvasRect, AspectRatio);
    else
     TmpTransform := fViewMapping;
    end;
    Tmp := TObject3D(TmpIter.First);
    TmpFlag := ShowControlPoints;
    ShowControlPoints := False;
    while Tmp <> nil do
     begin
       if Tmp.IsVisible(fViewNormalization, fVRP, DrawMode) then
        begin
          fCADCmp3D.Layers.SetCanvas(TmpCanvas, Tmp.Layer);
          if Cnv.Pen.Color = clWhite then
           Cnv.Pen.Color := clBlack;
          if Cnv.Brush.Color = clWhite then
           Cnv.Brush.Color := clBlack;
          Tmp.Draw(fViewNormalization, fVRP, TmpTransform, TmpCanvas, DrawMode);
        end;
       Tmp := TObject3D(TmpIter.Next);
     end;
    ShowControlPoints := TmpFlag;
  finally
    TmpIter.Free;
    TmpCanvas.Free;
  end;
end;

procedure TCADViewport3D.SetCamera(const ProjectionPlaneP, ViewP: TPoint3D; const Up: TVector3D);
begin
  StopRepaint;
  fVUP := Up;
  fVRP := ProjectionPlaneP;
  fVPN := Direction3D(ViewP, ProjectionPlaneP);
  fPRPlaneViewPointDistance := PointDistance3D(ProjectionPlaneP, ViewP);
  UpdateViewportOrientation;
end;

procedure TCADViewport3D.ZoomToBox(const Box: TRect3D);
var
  ExtWin: TRect2D;
  ExtBox: TRect3D;
  Marg: TRealType;
  TmpBool: Boolean;
begin
  if not Assigned(fCADCmp3D) then
   Exit;
  StopRepaint;
  // Project box on NRC plane with a margin.
  ExtBox := Box;
  Marg := Abs(ExtBox.SecondEdge.X - ExtBox.FirstEdge.X) / 2.0;
  Marg := {%H-}MaxValue([Marg, Abs(ExtBox.SecondEdge.Y - ExtBox.FirstEdge.Y) / 2.0]);
  Marg := {%H-}MaxValue([Marg, Abs(ExtBox.SecondEdge.Z - ExtBox.FirstEdge.Z) / 2.0]);
  if Marg = 0.0 then
   Marg := 1.0;
  ExtBox.FirstEdge.X := ExtBox.FirstEdge.X - Marg;
  ExtBox.FirstEdge.Y := ExtBox.FirstEdge.Y - Marg;
  ExtBox.FirstEdge.Z := ExtBox.FirstEdge.Z - Marg;
  ExtBox.SecondEdge.X := ExtBox.SecondEdge.X + Marg;
  ExtBox.SecondEdge.Y := ExtBox.SecondEdge.Y + Marg;
  ExtBox.SecondEdge.Z := ExtBox.SecondEdge.Z + Marg;
  // Change the viewpoint to view all.
  TmpBool := InUpdating;
  BeginUpdate;
  try
    SetCamera(ExtBox.SecondEdge,
              ExtBox.FirstEdge, VUP);
    // Extract the view window.
    ExtWin := GetBox2DExtension3D(ExtBox, fViewNormalization);
    // Project the Extesion win on Screen.
    ExtWin := TransformBoundingBox2D(ExtWin, fViewMapping);
    // Retrieve the Extension win on the viewport.
    ExtWin := TransformBoundingBox2D(ExtWin, ScreenToViewportTransform);
    // Zoom to extension.
    ZoomWindow(ExtWin);
  finally
    if not TmpBool then
     EndUpdate;
    Repaint;
  end;
end;

procedure TCADViewport3D.ZoomToExtension;
var
  ExtWin: TRect2D;
  ExtBox: TRect3D;
  Marg: TRealType;
  TmpBool: Boolean;
begin
  if not Assigned(fCADCmp3D) then
   Exit;
  StopRepaint;
  if CADCmp3D.ObjectsCount = 0 then
   begin
     Repaint;
     Exit;
   end;
  // Project Extension box on NRC plane with a margin.
  // Project box on NRC plane with a margin.
  ExtBox := fCADCmp3D.DrawingExtension;
  Marg := Abs(ExtBox.SecondEdge.X - ExtBox.FirstEdge.X) / 20.0;
  Marg := {%H-}MaxValue([Marg, Abs(ExtBox.SecondEdge.Y - ExtBox.FirstEdge.Y) / 20.0]);
  Marg := {%H-}MaxValue([Marg, Abs(ExtBox.SecondEdge.Z - ExtBox.FirstEdge.Z) / 20.0]);
  if Marg = 0.0 then
   Marg := 1.0;
  ExtBox.FirstEdge.X := ExtBox.FirstEdge.X - Marg;
  ExtBox.FirstEdge.Y := ExtBox.FirstEdge.Y - Marg;
  ExtBox.FirstEdge.Z := ExtBox.FirstEdge.Z - Marg;
  ExtBox.SecondEdge.X := ExtBox.SecondEdge.X + Marg;
  ExtBox.SecondEdge.Y := ExtBox.SecondEdge.Y + Marg;
  ExtBox.SecondEdge.Z := ExtBox.SecondEdge.Z + Marg;
  TmpBool := InUpdating;
  BeginUpdate;
  try
    // Extract the view window.
    ExtWin := GetBox2DExtension3D(ExtBox, fViewNormalization);
    // Project the Extesion win on Screen.
    ExtWin := TransformBoundingBox2D(ExtWin, fViewMapping);
    // Retrieve the Extension win on the viewport.
    ExtWin := TransformBoundingBox2D(ExtWin, ScreenToViewportTransform);
    // Zoom to extension.
    ZoomWindow(ExtWin);
  finally
    if not TmpBool then
     EndUpdate;
  end;
end;

procedure TCADViewport3D.GroupObjects(const ResultLst: TGraphicObjList;
                                      Frm: TRect2D;
                                      const Mode: TGroupMode;
                                      const RemoveFromCAD: Boolean);
var
  Tmp: TObject3D;
  TmpBox: TRect2D;
  TmpObjectsIter: TExclusiveGraphicObjIterator;
begin
  if not Assigned(CADCmp) or CADCmp.HasIterators then
   Exit;
  StopRepaint;
  // Lo porta da VisualRect a schermo.
  Frm := TransformRect2D(Frm, ViewportToScreenTransform);
  // E poi a normalizzate.
  Frm := ReorderRect2D(TransformRect2D(Frm, InvertTransform2D(fViewMapping)));
  ResultLst.FreeOnClear := RemoveFromCAD;
  TmpObjectsIter := fCADCmp3D.ObjectsExclusiveIterator;
  try
    Tmp := TmpObjectsIter.First as TObject3D;
    while Tmp <> nil do
     with (CADCmp.Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp.Enabled) and (Tmp is FPickFilter) and Tmp.IsVisible(fViewNormalization, fVRP, DrawMode) then
         begin
           TmpBox := GetBox2DExtension3D(Tmp.Box, fViewNormalization);
           if (Mode = gmAllInside) and IsBoxAllInBox2D(TmpBox, Frm) then
            { The object is in the frame. }
            begin
              ResultLst.Add(Tmp);
              if RemoveFromCAD then
               begin
                 TmpObjectsIter.RemoveCurrent;
                 Tmp := TObject3D(TmpObjectsIter.Current);
                 Continue;
               end;
            end
           else if (Mode = gmCrossFrame) and IsBoxInBox2D(TmpBox, Frm) then
             { The object cross the frame. }
            begin
              ResultLst.Add(Tmp);
              if RemoveFromCAD then
               begin
                 TmpObjectsIter.RemoveCurrent;
                 Tmp := TObject3D(TmpObjectsIter.Current);
                 Continue;
               end;
            end;
          end;
        Tmp := TmpObjectsIter.Next as TObject3D;
      end;
  finally
    TmpObjectsIter.Free;
  end;
end;

function TCADViewport3D.GetPixelAperture: TPoint2D;
begin
  Result.X := 1 / fViewMapping[1, 1];
  Result.Y := 1 / fViewMapping[2, 2];
  Result.W := 1.0;
end;

function TCADViewport3D.PickObject(Pt: TPoint3D; Aperture: Word; FirstFound: Boolean; var NPoint: Integer): TObject3D;
var
  TmpNPoint: Integer;
  Tmp: TObject3D;
  WAperture, MinDist, Distance: TRealType;
  TmpIter: TGraphicObjIterator;
begin
  Result := nil;
  if (not Assigned(CADCmp)) and (not Assigned(ViewportObjects)) then
   Exit;
  if CADCmp.HasIterators then
   Exit;
  if (ViewportObjects <> nil) then
   TmpIter := ViewportObjects.GetIterator
  else
   TmpIter := fCADCmp3D.ObjectsIterator;
  StopRepaint;
  try
    { Normalizing Pt. }
    Pt := TransformPoint3D(Pt, fViewNormalization);
    { Normalizing Aperture. }
    WAperture := GetPixelAperture.X * Aperture;
    MinDist := WAperture;
    NPoint := PICK_NOOBJECT;
    Tmp := TmpIter.First as TObject3D;
    while Tmp <> nil do
     with (CADCmp.Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp is FPickFilter) and Tmp.IsVisible(fViewNormalization, fVRP, DrawMode) then
         begin
           Distance := 0;
           TmpNPoint := Tmp.OnMe(Pt, fViewNormalization, WAperture, Distance);
           if (TmpNPoint >= NPoint) and (Distance <= MinDist) then
            begin
              Result := Tmp;
              NPoint := TmpNPoint;
              MinDist := Distance;
              if FirstFound then
               Break;
            end;
         end;
        Tmp := TmpIter.Next as TObject3D;
      end;
  finally
    TmpIter.Free;
  end;
end;

function TCADViewport3D.PickListOfObjects(const PickedObjects: TList; Pt: TPoint3D; Aperture: Word): Integer;
var
  Tmp: TObject3D;
  WAperture, Distance: TRealType;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  Result := 0;
  if (not Assigned(CADCmp)) and (not Assigned(ViewportObjects)) then
   Exit;
  if CADCmp.HasIterators then
   Exit;
  if (ViewportObjects <> nil) then
   TmpIter := ViewportObjects.GetExclusiveIterator
  else
   TmpIter := fCADCmp3D.ObjectsExclusiveIterator;
  StopRepaint;
  try
    { Normalizing Pt. }
    Pt := TransformPoint3D(Pt, fViewNormalization);
    { Normalizing Aperture. }
    WAperture := GetPixelAperture.X * Aperture;
    Tmp := TmpIter.First as TObject3D;
    while Tmp <> nil do
     with (CADCmp.Layers[Tmp.Layer]) do
      begin
        if Active and Visible and (Tmp is FPickFilter) and Tmp.IsVisible(fViewNormalization, fVRP, DrawMode) then
         begin
           Distance := 0;
           if Tmp.OnMe(Pt, fViewNormalization, WAperture, Distance) > PICK_NOOBJECT then
            begin
              PickedObjects.Add(Tmp);
              Inc(Result);
            end;
         end;
        Tmp := TmpIter.Next as TObject3D;
      end;
  finally
    TmpIter.Free;
  end;
end;

procedure TCADViewport3D.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 Pos2D: TPoint2D;
 Pos3D: TPoint3D;
begin
  if (not DisableMouseEvents) and Assigned(FOnMouseMove3D) then
   begin
     Pos2D := TransformPoint2D(Point2D(X, Y), ScreenToViewportTransform);
     Pos3D := ViewportToWorld(Pos2D);
     FOnMouseMove3D(Self, Shift, Pos3D.X, Pos3D.Y, Pos3D.Z, X, Y);
   end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCADViewport3D.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                X, Y: Integer);
var
 Pos2D: TPoint2D;
 Pos3D: TPoint3D;
begin
  if (Button = mbRight) and (Shift = [ssAlt, ssShift, ssRight]) then
   ShowMessage('TCADCmp3D developed by PV :)');
  if (not DisableMouseEvents) and Assigned(FOnMouseDown3D) then
   begin
     Pos2D := TransformPoint2D(Point2D(X, Y), ScreenToViewportTransform);
     Pos3D := ViewportToWorld(Pos2D);
     FOnMouseDown3D(Self, Button, Shift, Pos3D.X, Pos3D.Y, Pos3D.Z, X, Y);
   end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCADViewport3D.MouseUp(Button: TMouseButton; Shift: TShiftState;
                            X, Y: Integer);
var
 Pos2D: TPoint2D;
 Pos3D: TPoint3D;
begin
  if (not DisableMouseEvents) and Assigned(FOnMouseUp3D) then
   begin
     Pos2D := TransformPoint2D(Point2D(X, Y), ScreenToViewportTransform);
     Pos3D := ViewportToWorld(Pos2D);
     FOnMouseUp3D(Self, Button, Shift, Pos3D.X, Pos3D.Y, Pos3D.Z, X, Y);
   end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TCADViewport3D.WorldToObject(Obj: TObject3D; WPt: TPoint3D): TPoint3D;
begin
  WPt := CartesianPoint3D(WPt);
  if Obj.HasTransform then
   Result := TransformPoint3D(WPt, InvertTransform3D(Obj.ModelTransform))
  else
   Result := WPt;
end;

function TCADViewport3D.ObjectToWorld(Obj: TObject3D; OPt: TPoint3D): TPoint3D;
begin
  OPt := CartesianPoint3D(OPt);
  if Obj.HasTransform then
   Result := TransformPoint3D(OPt, Obj.ModelTransform)
  else
   Result := OPt;
end;

function TCADViewport3D.WorldToViewport(WPt: TPoint3D): TPoint2D;
begin
  Result := Point3DToPoint2D(TransformPoint3D(WPt, fViewOrientation));
end;

function TCADViewport3D.ViewportToWorld(VPt: TPoint2D): TPoint3D;
var
  Pt3D: TPoint3D;
begin
  Pt3D := Point2DToPoint3D(VPt);
  Result := CartesianPoint3D(TransformPoint3D(Pt3D, InvertTransform3D(fViewOrientation)));
end;

function TCADViewport3D.WorldToNRC(WPt: TPoint3D): TPoint3D;
begin
  Result := CartesianPoint3D(TransformPoint3D(WPt, fViewNormalization));
end;

function TCADViewport3D.NRCToScreen(NPt: TPoint3D): TPoint;
begin
  Result := Point2DToPoint(TransformPoint2D(Point3DToPoint2D(NPt), fViewMapping));
end;

// =====================================================================
// TCADParallelViewport3D
// =====================================================================

function TCADParallelViewport3D.BuildViewNormalizationTransform(View: TRect2D; FP, BP: TRealType): TTransf3D;
begin
  Result := ParallelViewNormalization3D(View, FP, BP);
end;

function TCADParallelViewport3D.GetRayVersor(const WPt: TPoint3D): TVector3D;
begin
  Result.X := -VPN.X;
  Result.Y := -VPN.Y;
  Result.Z := -VPN.Z;
end;

constructor TCADParallelViewport3D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetCamera(Point3D(-100, -100, 100), Point3D(0, 0, 0), Versor3D(0, 0, 1));
end;

// =====================================================================
// TCADOrtogonalViewport3D
// =====================================================================

constructor TCADOrtogonalViewport3D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fElevation := 0.0;
  fView := vtTop;
  fDirection := dtAhead;
  UpdateViewportOrientation;
end;

function TCADOrtogonalViewport3D.BuildViewOrientationTransform(const LVRP: TPoint3D; const LVUP, LVPN: TVector3D): TTransf3D;
var
  TmpUp, TmpN: TVector3D;
  TmpVRP: TPoint3D;
begin
  case fView of
   vtFront: begin
     TmpUP := Versor3D(0, 0, 1);
     TmpVRP := Point3D(0, fElevation, 0);
     if fDirection = dtAhead then
      TmpN := Versor3D(0, 1, 0)
     else
      TmpN := Versor3D(0, -1, 0)
   end;
   vtSide: begin
     TmpUP := Versor3D(0, 0, 1);
     TmpVRP := Point3D(0, fElevation, 0);
     if fDirection = dtAhead then
      TmpN := Versor3D(1, 0, 0)
     else
      TmpN := Versor3D(-1, 0, 0);
   end;
   vtTop: begin
     TmpUP := Versor3D(0, 1, 0);
     TmpVRP := Point3D(0, 0, fElevation);
     if fDirection = dtAhead then
      TmpN := Versor3D(0, 0, 1)
     else
      TmpN := Versor3D(0, 0, -1);
   end;
  end;
  // Chiama il metodo ereditato.
  fVPN := TmpN;
  fVUP := TmpUP;
  FVRP := TmpVRP;
  Result := inherited BuildViewOrientationTransform(TmpVRP, TmpUP, TmpN);
end;

procedure TCADOrtogonalViewport3D.SetView(V: TOrtoViewType);
begin
  if V <> fView then
   begin
     fView := V;
     UpdateViewportOrientation;
   end;
end;

procedure TCADOrtogonalViewport3D.SetDirection(D: TOrtoDirectionType);
begin
  if D <> fDirection then
   begin
     fDirection := D;
     UpdateViewportOrientation;
   end;
end;

procedure TCADOrtogonalViewport3D.SetElevation(E: TRealType);
begin
  if E <> fElevation then
   begin
     fElevation := E;
     UpdateViewportOrientation;
   end;
end;

// =====================================================================
// TCADPerspectiveViewport3D
// =====================================================================

procedure TCADPerspectiveViewport3D.SetBClip(B: TRealType);
begin
  if B <= -fPlaneDistance then
   B := -fPlaneDistance + 0.1;
  inherited SetBClip(B);
end;

procedure TCADPerspectiveViewport3D.SetPlaneDistance(D: TRealType);
begin
  if D <> fPlaneDistance then
   begin
     fPlaneDistance := D;
     if fBackPlane <= -fPlaneDistance then
      fBackPlane := -fPlaneDistance + 0.1;
     UpdateViewportOrientation;
   end;
end;

function TCADPerspectiveViewport3D.GetCenterOfProjection: TPoint3D;
begin
  Result := ExtrudePoint3D(VRP, VPN, fPlaneDistance);
end;

function TCADPerspectiveViewport3D.BuildViewNormalizationTransform(View: TRect2D; FP, BP: TRealType): TTransf3D;
begin
  Result := PerspectiveViewNormalization3D(View, fPlaneDistance, FP, BP);
end;

constructor TCADPerspectiveViewport3D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetCamera(Point3D(-100, -100, 100), Point3D(0, 0, 0), Versor3D(0, 0, 1));
  fPlaneDistance := 1.0;
  SetFieldOfView(DegToRad(30.0), 1.0);
end;

function TCADPerspectiveViewport3D.GetRayVersor(const WPt: TPoint3D): TVector3D;
var
  COP: TPoint3D;
begin
  COP := ExtrudePoint3D(VRP, VPN, fPlaneDistance);
  Result := Direction3D(COP, WPt);
end;

procedure TCADPerspectiveViewport3D.SetFieldOfView(const Angle, Aspect: TRealType);
var
  VW: TRect2D;
begin
  StopRepaint;
  VW := Rect2D(0.0, 0.0, 0.0, 0.0);
  VW.Top := fPlaneDistance * Tan(Angle / 2.0);
  VW.Bottom := -VW.Top;
  VW.Left := VW.Top * Aspect;
  VW.Right := -VW.Left;
  ZoomWindow(VW);
end;

// =====================================================================
// TCAD3DAxis
// =====================================================================

procedure DrawAx(Cnv: TCanvas; Org: TPoint; Dir: TVector2D; Sz: Integer; Cl: TColor; AxT: Byte);
var
  NormDir: TVector2D;
  First: TPoint;
  DDX, DDY, DNX, DNY: Integer;
begin
  with Cnv do
   begin
     Pen.Color := Cl;
     Pen.Mode := pmCopy;
     Pen.Style := psSolid;
     Pen.Width := 1;
     NormDir := Perpendicular2D(Dir);
     First.X := Org.X + Trunc(Sz * 1.5);
     First.Y := Org.Y - Trunc(Sz * 1.5);
     DNX := Trunc(NormDir.X * Sz / 8);
     DNY := Trunc(NormDir.Y * Sz / 8);
     DDX := Trunc(Dir.X * Sz / 4);
     DDY := Trunc(Dir.Y * Sz / 4);
     MoveTo(First.X, First.Y);
     LineTo(PenPos.X + Trunc(Dir.X * Sz), PenPos.Y + Trunc(Dir.Y * Sz));
     LineTo(PenPos.X + DNX - DDX, PenPos.Y + DNY - DDY);
     LineTo(PenPos.X - 2 * DNX, PenPos.Y - 2 * DNY);
     LineTo(First.X + Trunc(Dir.X * Sz), First.Y + Trunc(Dir.Y * Sz));
     // Label.
     First.X := First.X + DDX + LongInt(Trunc(Dir.X * Sz));
     First.Y := First.Y + DDY + LongInt(Trunc(Dir.Y * Sz));
     SetBkMode(Handle, TRANSPARENT);
     Font.Color := Cl;
     Font.Name := 'Arial';
     Font.Size := 7;
     case AxT of
      0: // Asse X.
       TextOut(First.X, First.Y, 'X');
      1: // Asse Y.
       TextOut(First.X, First.Y, 'Y');
      2: // Asse Z.
       TextOut(First.X, First.Y, 'Z');
     end;
   end;
end;

constructor TCAD3DAxis.Create(Owner: TCADViewport3D);
begin
  inherited Create;

  fOwnerView := Owner;
  fXColor := clRed;
  fYColor := clBlue;
  fZColor := clGreen;
  fVisible := True;
  fSize := 30;
end;

procedure TCAD3DAxis.SetXColor(Cl: TColor);
begin
  if Cl <> fXColor then
   begin
     fXColor := Cl;
     fOwnerView.Repaint;
   end;
end;

procedure TCAD3DAxis.SetYColor(Cl: TColor);
begin
  if Cl <> fYColor then
   begin
     fYColor := Cl;
     fOwnerView.Repaint;
   end;
end;

procedure TCAD3DAxis.SetZColor(Cl: TColor);
begin
  if Cl <> fZColor then
   begin
     fZColor := Cl;
     fOwnerView.Repaint;
   end;
end;

procedure TCAD3DAxis.SetSize(Sz: Integer);
begin
  if Sz <> fSize then
   begin
     fSize := Sz;
     fOwnerView.Repaint;
   end;
end;

procedure TCAD3DAxis.SetVisible(V: Boolean);
begin
  if V <> fVisible then
   begin
     fVisible := V;
     fOwnerView.Repaint;
   end;
end;

procedure TCAD3DAxis.PaintAxes;
var
  Dir: TVector2D;
  ZeroPt2D, OnePt2D: TPoint2D;
begin
  with fOwnerView do
   try
     ZeroPt2D := ViewportToScreen(WorldToViewport(Point3D(0, 0, 0)));
     { X }
     OnePt2D := ViewportToScreen(WorldToViewport(Point3D(1, 0, 0)));
     Dir := Direction2D(ZeroPt2D, OnePt2D);
     DrawAx(Cnv, P, Dir, fSize, fXColor, 0);
     { Y }
     OnePt2D := ViewportToScreen(WorldToViewport(Point3D(0, 1, 0)));
     Dir := Direction2D(ZeroPt2D, OnePt2D);
     DrawAx(Cnv, P, Dir, fSize, fYColor, 1);
     { Z }
     OnePt2D := ViewportToScreen(WorldToViewport(Point3D(0, 0, 1)));
     Dir := Direction2D(ZeroPt2D, OnePt2D);
     DrawAx(Cnv, P, Dir, fSize, fZColor, 2);
   except
   end;
end;

// =====================================================================
// TCADPrgParam
// =====================================================================

constructor TCADPrgParam.Create(AfterS: TCADStateClass);
begin
  inherited Create;

  fAfterState := AfterS;
end;

// =====================================================================
// TCADState
// =====================================================================

constructor TCADState.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create;
  fCAD := CADPrg;
  fParam := StateParam;
  fCanBeSuspended := True;
  Description := '';
  NextState := nil;
end;

procedure TCADState.SetDescription(D: String);
begin
  fDescription := D;
  if Assigned(fCAD) and Assigned(fCAD.fOnDescriptionChanged) then
   fCAD.fOnDescriptionChanged(Self);
end;

function TCADState.OnEvent(Event: TCADPrgEvent;
                           MouseButton: TCS4MouseButton;
                           Shift: TShiftState;
                           Key: Word;
                           var NextState: TCADStateClass): Boolean;
begin
  Result := False;
end;

procedure TCADState.OnStop;
begin
  Param.Free;
  Param := nil;
end;

procedure TCADState.OnResume;
begin
end;

// =====================================================================
// TCADIdleState.
// =====================================================================

constructor TCADIdleState.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if Param <> nil then
   Raise Exception.Create('TCADIdleState.Create: Non all objects were destroied');
  NextState := nil;
end;

// =====================================================================
// TCADPrg
// =====================================================================

procedure TCADPrg.SetLinkedViewport(V: TCADViewport);
begin
  if V = fLinkedViewport then
   Exit;
  { Subclassing component. Charles F. I. Savage gave my this idea from its
    TCADViewport subclassing. Thanks Charles. }
  if Assigned(fNewWndProc) then
   begin
     {$IFDEF windows}
     if not (csDestroying in fLinkedViewport.ComponentState) then
      SetWindowLong(fLinkedViewport.Handle, gwl_wndProc, {%H-}LongInt(fOldWndProc));
     FreeObjectInstance(fNewWndProc);
     {$ELSE}
     if not (csDestroying in fLinkedViewport.ComponentState) then
      fLinkedViewport.WindowProc := fOldWndProc;
     {$ENDIF}
     fNewWndProc := nil;
   end;
  // Reassign the old on paint.
  if Assigned(fLinkedViewport) then
   begin
     fLinkedViewport.OnPaint := fOldOnPaint;
     if fShowCursorCross then
      HideCursorCross;
   end;
  fLinkedViewport := V;
  if Assigned(V) then
   begin
     V.FreeNotification(Self);
     {$IFDEF windows}
     fNewWndProc := MakeObjectInstance(SubclassedWinProc);
     fOldWndProc := {%H-}Pointer(SetWindowLong(V.Handle, gwl_wndProc, {%H-}LongInt(fNewWndProc)));
     {$ELSE}
     fNewWndProc := SubclassedWinProc;
     fOldWndProc := V.WindowProc;
     V.WindowProc := fNewWndProc;
     {$ENDIF}
     // Assign the new OnPaint because it is called not only by window msgs.
     fOldOnPaint := V.OnPaint;
     V.OnPaint := ViewOnPaint;
     if fShowCursorCross then
      HideCursorCross;
   end;
end;

procedure TCADPrg.SetDefaultState(DefState: TCADStateClass);
var
  TmpState: TCADStateClass;
begin
  if CurrentState is fDefaultState then
   begin
     CurrentState.OnStop;
     CurrentState.Free;
     fCurrentState := nil;
     fDefaultState := DefState;
     fCurrentState := fDefaultState.Create(Self, nil, {%H-}TmpState);
   end
  else
   fDefaultState := DefState;
end;

procedure TCADPrg.SendEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton;
                            Shift: TShiftState; Key: Word);
var
  NextState: TCADStateClass;
  Res, TmpBool: Boolean;
  LastParam: TCADPrgParam;
  LastState: TClass;
begin
  if fIgnoreEvents then
   Exit;
  NextState := nil;
  Res := fCurrentState.OnEvent(Event, MouseButton, Shift, Key, NextState);
  if Res then
   begin
     if Assigned(fOnExState) then
      fOnExState(Self, fCurrentState);
     repeat
       LastParam := fCurrentState.fParam;
       LastState := fCurrentState.ClassType;
       if (NextState = fDefaultState) or (NextState = nil) then
        begin
          if Assigned(fOnEndOperation) then
           fOnEndOperation(Self, fCurrentOperation, fCurrentState.fParam);
          GoToDefaultState(LastState, LastParam);
          Break;
        end;
       TmpBool := fCurrentState.CanBeSuspended;
       fCurrentState.Free;
       fCurrentState := nil;
       try
         fCurrentState := NextState.Create(Self, LastParam, NextState);
         fCurrentState.CanBeSuspended := TmpBool;
       except
        Reset;
        Break;
       end;
       if Assigned(fOnEntState) then
        fOnEntState(Self, fCurrentState);
     until not Assigned(NextState);
   end;
end;

procedure TCADPrg.GoToDefaultState(const LastState: TClass; const LastParam: TCADPrgParam);
var
  TmpState: TCADStateClass;
begin
  if not Assigned(fLinkedViewport) then Exit;
  if not Assigned(fCurrentState) then
   begin
     fCurrentState := fDefaultState.Create(Self, nil, {%H-}TmpState);
     Exit;
   end;
  if fIsSuspended then
   begin
     fCurrentState.Free;
     fCurrentState := nil;
     fCurrentState := fSuspendedState;
     fSuspendedState := nil;
     fCurrentOperation := fSuspendedOperation;
     fSuspendedOperation := nil;
     // Force the OnDescriptionChanged event.
     fCurrentState.Description := fCurrentState.Description;
     fIsSuspended := False;
     if fShowCursorCross then
      HideCursorCross;
     if fMustRepaint and not IsBoxInBox2D(Rect2D(0.0, 0.0, 0.0, 0.0), fRepaintRect) then
      fLinkedViewport.RepaintRect(fRepaintRect)
     else if fMustRepaint then
      fLinkedViewport.Repaint
     else if fRefreshAfterOp then
      fLinkedViewport.Refresh;
     if fShowCursorCross then
      DrawCursorCross;
     fMustRepaint := False;
     fCurrentState.OnResume(LastState, LastParam);
     if Assigned(LastParam) and (LastParam <> fCurrentState.fParam) then
      LastParam.Free;
     Exit;
   end;
  fIsBusy := False;
  if Assigned(fCurrentState.fParam) then
   fCurrentState.fParam.Free;
  fCurrentState.Free;
  fCurrentState := nil;
  fCurrentOperation := nil;
  if fShowCursorCross then
   HideCursorCross;
  if fMustRepaint and not IsBoxInBox2D(Rect2D(0.0, 0.0, 0.0, 0.0), fRepaintRect) then
   fLinkedViewport.RepaintRect(fRepaintRect)
  else if fMustRepaint then
   fLinkedViewport.Repaint
  else if fRefreshAfterOp then
   fLinkedViewport.Refresh;
  if fShowCursorCross then
   DrawCursorCross;
  fMustRepaint := False;
  fIgnoreEvents := False;
  StartOperation(fDefaultState, nil);
  if (fDefaultState = TCADIdleState) and Assigned(fOnIdle) then
   fOnIdle(Self);
end;

procedure TCADPrg.ViewOnPaint(Sender: TObject);
begin
  if fShowCursorCross then
   HideCursorCross;
  if Assigned(CurrentState) then
   SendEvent(cePaint, cmbNone, [], 0);
  if Assigned(fOldOnPaint) then
   fOldOnPaint(Sender);
  if fShowCursorCross then
   DrawCursorCross;
end;

function TCADPrg.ViewOnDblClick(Sender: TObject): Boolean;
begin
  if Assigned(CurrentState) then
   SendEvent(ceMouseDblClick, cmbNone, [], 0);
  Result := True;
end;

function TCADPrg.ViewOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState): Boolean;
var
  MouseButton: TCS4MouseButton;
begin
  CurrentKey := Key;
  if ssLeft in Shift then
   MouseButton := cmbLeft
  else if ssRight in Shift then
   MouseButton := cmbRight
  else if ssMiddle in Shift then
   MouseButton := cmbMiddle
  else
   MouseButton := cmbNone;
  if Assigned(CurrentState) then
   SendEvent(ceKeyDown, MouseButton, Shift, CurrentKey);
  Result := True;
end;

function TCADPrg.ViewOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState): Boolean;
var
  MouseButton: TCS4MouseButton;
begin
  if ssLeft in Shift then
   MouseButton := cmbLeft
  else if ssRight in Shift then
   MouseButton := cmbRight
  else if ssMiddle in Shift then
   MouseButton := cmbMiddle
  else
   MouseButton := cmbNone;
  if Assigned(CurrentState) then
   SendEvent(ceKeyUp, MouseButton, Shift, Key);
  CurrentKey := 0;
  Result := True;
end;

function TCADPrg.ViewOnMouseMove(Sender: TObject; Shift: TShiftState; var X, Y: SmallInt): Boolean;
var
  MouseButton: TCS4MouseButton;
begin
  fCurrentMousePoint := Point(X, Y);
  if ssLeft in Shift then
   MouseButton := cmbLeft
  else if ssRight in Shift then
   MouseButton := cmbRight
  else if ssMiddle in Shift then
   MouseButton := cmbMiddle
  else
   MouseButton := cmbNone;
  if fShowCursorCross then
   DrawCursorCross;
  if Assigned(CurrentState) then
   SendEvent(ceMouseMove, MouseButton, Shift, CurrentKey);
  Result := True;
end;

function TCADPrg.ViewOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean;
var
  MouseButton: TCS4MouseButton;
begin
  fCurrentMousePoint := Point(X, Y);
  if Button = mbLeft then
   MouseButton := cmbLeft
  else if Button = mbRight then
   MouseButton := cmbRight
  else if Button = mbMiddle then
   MouseButton := cmbMiddle
  else
   MouseButton := cmbNone;
  if Assigned(CurrentState) then
   SendEvent(ceMouseDown, MouseButton, Shift, CurrentKey);
  Result := True;
end;

function TCADPrg.ViewOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean;
var
  MouseButton: TCS4MouseButton;
begin
  fCurrentMousePoint := Point(X, Y);
  if Button = mbLeft then
   MouseButton := cmbLeft
  else if Button = mbRight then
   MouseButton := cmbRight
  else if Button = mbMiddle then
   MouseButton := cmbMiddle
  else
   MouseButton := cmbNone;
  if Assigned(CurrentState) then
   SendEvent(ceMouseUp, MouseButton, Shift, CurrentKey);
  Result := True;
end;

constructor TCADPrg.Create(AOwner: TComponent);
var
  TmpState: TCADStateClass;
begin
  inherited Create(AOwner);
  fCursorColor := clGray;
  fShowCursorCross := False;
  fLinkedViewport := nil;
  fNewWndProc := nil;
  fOldWndProc := nil;
  fDefaultState := TCADIdleState;
  fCurrentState := fDefaultState.Create(Self, nil, {%H-}TmpState);
  fIgnoreEvents := False;
  fRefreshAfterOp := True;
end;

destructor TCADPrg.Destroy;
begin
  if Assigned(fCurrentState) then
   begin
     fCurrentState.OnStop;
     fCurrentState.Free;
   end;
  if fIsSuspended then
   begin
     fSuspendedState.OnStop;
     fSuspendedState.Free;
   end;
  fCurrentState := nil;
  LinkedViewport := nil;
  inherited Destroy;
end;

procedure TCADPrg.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = LinkedViewport) and (Operation = opRemove) then
   LinkedViewport := nil;
end;

procedure TCADPrg.SubclassedWinProc(var Msg: TLMessage);
var
  LastSet, Propagate: Boolean;
begin
  Propagate := True;
  if not (csDesigning in ComponentState) then
   case Msg.Msg of
   LM_MOUSEMOVE:
    with TLMMouseMove(Msg) do
     Propagate := ViewOnMouseMove(LinkedViewport, KeysToShiftState(Keys), XPos, YPos);
   CM_INVALIDATE:
    begin
      {$IFDEF windows}
      Msg.Result := CallWindowProc(fOldWndProc, fLinkedViewport.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
      {$ELSE}
      if Assigned(fOldWndProc) then
       fOldWndProc(Msg);
      {$ENDIF}
      ViewOnPaint(Self);
      Propagate := False;
    end;
   LM_LBUTTONDOWN:
    with TLMMouse(Msg) do
     Propagate := ViewOnMouseDown(LinkedViewport, mbLeft, KeysToShiftState(Keys), XPos, YPos);
   LM_RBUTTONDOWN:
    with TLMMouse(Msg) do
     Propagate := ViewOnMouseDown(LinkedViewport, mbRight, KeysToShiftState(Keys), XPos, YPos);
   LM_LBUTTONUP:
    with TLMMouse(Msg) do
     Propagate := ViewOnMouseUp(LinkedViewport, mbLeft, KeysToShiftState(Keys), XPos, YPos);
   LM_RBUTTONUP:
    with TLMMouse(Msg) do
     Propagate := ViewOnMouseUp(LinkedViewport, mbRight, KeysToShiftState(Keys), XPos, YPos);
   LM_RBUTTONDBLCLK:
    begin
      ViewOnDblClick(LinkedViewport);
      with TLMMouse(Msg) do
       Propagate := ViewOnMouseDown(LinkedViewport, mbRight, KeysToShiftState(Keys), XPos, YPos);
    end;
   LM_LBUTTONDBLCLK:
    begin
      ViewOnDblClick(LinkedViewport);
      with TLMMouse(Msg) do
       Propagate := ViewOnMouseDown(LinkedViewport, mbLeft, KeysToShiftState(Keys), XPos, YPos);
    end;
   LM_KEYDOWN:
    with TLMKeyDown(Msg) do
     Propagate := ViewOnKeyDown(LinkedViewport, CharCode, KeyDataToShiftState(KeyData));
   LM_KEYUP:
    with TLMKeyUp(Msg) do
     Propagate := ViewOnKeyUp(LinkedViewport, CharCode, KeyDataToShiftState(KeyData));
   end;
  LastSet := fLinkedViewport.DisableMouseEvents;
  try
    fLinkedViewport.DisableMouseEvents := not Propagate;
    {$IFDEF windows}
    Msg.Result := CallWindowProc(fOldWndProc, fLinkedViewport.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
    {$ELSE}
    if Assigned(fOldWndProc) then
     fOldWndProc(Msg);
    {$ENDIF}
  finally
    fLinkedViewport.DisableMouseEvents := LastSet;
  end;
end;

function TCADPrg.StartOperation(const StartState: TCADStateClass; Param: TCADPrgParam): Boolean;
var
  NewNext: TCADStateClass;
  TmpBool: Boolean;
begin
  Result := False;
  if not Assigned(fLinkedViewport) then
   Exit;
  if fLinkedViewport.InRepainting then
   fLinkedViewport.WaitForRepaintEnd;
  if fIsSuspended then
   Exit;
  if fIsBusy then
   StopOperation;
  fIsBusy := (StartState <> fDefaultState);
  NewNext := StartState;
  fCurrentOperation := StartState;
  if Assigned(fOnStart) then
   fOnStart(Self, fCurrentOperation, Param);
  TmpBool := True;
  repeat
    if (StartState <> fDefaultState) and (NewNext = fDefaultState) then
     begin
       if Assigned(fOnEndOperation) then
        fOnEndOperation(Self, fCurrentOperation, fCurrentState.fParam);
       GoToDefaultState(nil, nil);
       Break;
     end;
    fCurrentState.Free;
    fCurrentState := nil;
    try
      fCurrentState := NewNext.Create(Self, Param, NewNext);
      if fCurrentState.ClassType = StartState then
       TmpBool := fCurrentState.CanBeSuspended; // e' il primo stato a det. la sospendibilità
      fCurrentState.CanBeSuspended := TmpBool;
    except
      Reset;
      Break;
    end;
    Param := fCurrentState.fParam;
    if Assigned(fOnEntState) then
     fOnEntState(Self, fCurrentState);
  until not Assigned(NewNext);
  Result := True;
end;

procedure TCADPrg.StopOperation;
begin
  if not fIsBusy or (fCurrentState = nil) then
   Exit;
  fCurrentState.OnStop;
  GoToDefaultState(fCurrentState.ClassType, fCurrentState.Param);
  if Assigned(fOnStop) then
   fOnStop(Self, fCurrentOperation, nil);
end;

procedure TCADPrg.Reset;
begin
  if Assigned(fCurrentState) then
   begin
     fIgnoreEvents := False;
     fCurrentState.OnStop;
     GoToDefaultState(fCurrentState.ClassType, fCurrentState.Param)
   end
  else
   GoToDefaultState(nil, nil)
end;

function TCADPrg.SuspendOperation(const StartState: TCADStateClass; Param: TCADPrgParam): Boolean;
var
  NewNext: TCADStateClass;
  TmpBool: Boolean;
begin
  Result := False;
  if fIsSuspended then
   Exit;
  if not CurrentState.CanBeSuspended then
   Exit;
  if not fIsBusy then
   begin
     StartOperation(StartState, Param);
     Exit;
   end;
  if fLinkedViewport.InRepainting then
   fLinkedViewport.WaitForRepaintEnd;
  fSuspendedState := CurrentState;
  fSuspendedOperation := fCurrentOperation;
  fIsSuspended := True;
  if not Assigned(Param) then
   Param := fSuspendedState.fParam;
  NewNext := StartState;
  TmpBool := True;
  repeat
    if (StartState <> fDefaultState) and (NewNext = fDefaultState) then
     begin
       GoToDefaultState(StartState, Param);
       Break;
     end;
    if fCurrentState <> fSuspendedState then
     fCurrentState.Free;
    fCurrentState := nil;
    try
      fCurrentState := NewNext.Create(Self, Param, NewNext);
      if fCurrentState.ClassType = StartState then
       TmpBool := fCurrentState.CanBeSuspended; // e' il primo stato a det. la sospendibilità
      fCurrentState.CanBeSuspended := TmpBool;
    except
      Reset;
      Break;
    end;
    Param := fCurrentState.fParam;
    if Assigned(fOnEntState) then
     fOnEntState(Self, fCurrentState);
  until not Assigned(NewNext);
  Result := True;
end;

procedure TCADPrg.SendUserEvent(const Code: Word);
begin
  if not Assigned(fLinkedViewport) then Exit;
  try
   SendEvent(ceUserDefined, cmbNone, [], Code);
  except
   Reset;
  end;
end;

procedure TCADPrg.SendCADEvent(Event: TCADPrgEvent; MouseButton: TMouseButton; Shift: TShiftState; Key: Word);
var
  Button: TCS4MouseButton;
begin
  if MouseButton = mbLeft then
   Button := cmbLeft
  else if MouseButton = mbRight then
   Button := cmbRight
  else if MouseButton = mbMiddle then
   Button := cmbMiddle
  else
   Button := cmbNone;
  if not Assigned(fLinkedViewport) then Exit;
  try
   SendEvent(Event, Button, Shift, Key);
  except
   Reset;
  end;
end;

procedure TCADPrg.RepaintAfterOperation;
begin
  fMustRepaint := True;
  fRepaintRect := Rect2D(0.0, 0.0, 0.0, 0.0);
end;

procedure TCADPrg.RepaintRectAfterOperation(const ARect: TRect2D);
begin
  fMustRepaint := True;
  fRepaintRect := ARect;
end;

procedure TCADPrg.SetShowCursorCross(B: Boolean);
begin
  if fShowCursorCross <> B then
   begin
     fShowCursorCross := B;
     if not B then
      HideCursorCross;
   end;
end;

procedure TCADPrg.SetCursorColor(C: TColor);
begin
  if C = fCursorColor then
   Exit;
  if fShowCursorCross then
   DrawCursorCross;
  fCursorColor := C;
  if fShowCursorCross then
   DrawCursorCross;
end;

// =====================================================================
// TCADPrg2D
// =====================================================================

function TCADPrg2D.GetVPPoint: TPoint2D;
begin
  Result := fCurrentViewportPoint;
end;

procedure TCADPrg2D.SetVPPoint(const Pt: TPoint2D);
begin
  fCurrentViewportPoint := CartesianPoint2D(Pt);
end;

function TCADPrg2D.GetVPSnappedPoint: TPoint2D;
begin
  Result := GetVPPoint;
  if not UseSnap then
   Exit;
  with Result do
   begin
     if XSnap <> 0 then
      Result.X := Round(X / XSnap) * XSnap;
     if YSnap <> 0 then
      Result.Y := Round(Y / YSnap) * YSnap;
     Result.W := 1.0;
   end;
  if Assigned(fSnapFilter) then
   fSnapFilter(Self, CurrentState, fSnapOriginPoint, Result);
end;

function TCADPrg2D.ViewOnMouseMove(Sender: TObject; Shift: TShiftState; var X, Y: SmallInt): Boolean;
begin
  fCurrentViewportPoint := Viewport2D.ScreenToViewport(Point2D(X, Y));
  if Assigned(fMouseMoveFilter) then
   begin
     fMouseMoveFilter(Self, CurrentState, fCurrentViewportPoint, X, Y);
     if Assigned(Viewport2D.OnMouseMove2D) then
      // if there will be a friend directive like in C++, it will be better
      // to call the MouseMove handler directly :)
      Viewport2D.OnMouseMove2D(Sender, Shift, fCurrentViewportPoint.X, fCurrentViewportPoint.Y, X, Y);
     inherited ViewOnMouseMove(Sender, Shift, X, Y);
     Result := False;
   end
  else
   Result := inherited ViewOnMouseMove(Sender, Shift, X, Y);
end;

function TCADPrg2D.ViewOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean;
begin
  fCurrentViewportPoint := Viewport2D.ScreenToViewport(Point2D(X, Y));
  if Assigned(fMouseButtonFilter) then
   begin
     fMouseButtonFilter(Self, CurrentState, Button, fCurrentViewportPoint, X, Y);
     if Assigned(Viewport2D.OnMouseDown2D) then
      // if there will be a friend directive like in C++, it will be better
      // to call the MouseMove handler directly :)
      Viewport2D.OnMouseDown2D(Sender, Button, Shift, fCurrentViewportPoint.X, fCurrentViewportPoint.Y, X, Y);
     inherited ViewOnMouseDown(Sender, Button, Shift, X, Y);
     Result := False;
   end
  else
   Result := inherited ViewOnMouseDown(Sender, Button, Shift, X, Y);
end;

function TCADPrg2D.ViewOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean;
begin
  fCurrentViewportPoint := Viewport2D.ScreenToViewport(Point2D(X, Y));
  if Assigned(fMouseButtonFilter) then
   begin
     fMouseButtonFilter(Self, CurrentState, Button, fCurrentViewportPoint, X, Y);
     if Assigned(Viewport2D.OnMouseUp2D) then
      // if there will be a friend directive like in C++, it will be better
      // to call the MouseMove handler directly :)
      Viewport2D.OnMouseUp2D(Sender, Button, Shift, fCurrentViewportPoint.X, fCurrentViewportPoint.Y, X, Y);
     inherited ViewOnMouseUp(Sender, Button, Shift, X, Y);
     Result := False;
   end
  else
   Result := inherited ViewOnMouseUp(Sender, Button, Shift, X, Y);
end;

function TCADPrg2D.GetViewport2D: TCADViewport2D;
begin
  Result := LinkedViewport as TCADViewport2D;
end;

procedure TCADPrg2D.SetViewport2D(View2D: TCADViewport2D);
begin
  LinkedViewport := View2D;
end;

constructor TCADPrg2D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  XSnap := 10.0;
  YSnap := 10.0;
  fLastCursorPos := Point2D(0.0, 0.0);
  fSnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

destructor TCADPrg2D.Destroy;
begin
  SetViewport2D(nil);
  inherited Destroy;
end;

procedure _DrawCursorCross2D(const Viewp: TCADViewport2D; const Pt: TPoint2D);
var
  ScrPt: TPoint;
begin
  with Viewp do
   begin
     ScrPt := Point2DToPoint(ViewportToScreen(Pt));
     Canvas.MoveTo(ScrPt.X, ClientRect.Top);
     Canvas.LineTo(ScrPt.X, ClientRect.Bottom);
     Canvas.MoveTo(ClientRect.Left, ScrPt.Y);
     Canvas.LineTo(ClientRect.Right, ScrPt.Y);
   end;
end;

procedure TCADPrg2D.DrawCursorCross;
begin
  if (not Assigned(Viewport))  or (Viewport.HandleAllocated = False) or (Viewport.InRepainting) then
   Exit;
  with Viewport2D do
   try
     Canvas.Pen.Mode := pmXOr;
     Canvas.Pen.Color := CursorColor xor BackGroundColor;
     Canvas.Pen.Style := psSolid;
     Canvas.Pen.Width := 1;
     _DrawCursorCross2D(Viewport2D, fLastCursorPos);
     fLastCursorPos := CurrentViewportSnappedPoint;
     _DrawCursorCross2D(Viewport2D, fLastCursorPos);
   except
   end;
end;

procedure TCADPrg2D.HideCursorCross;
begin
  if (not Assigned(Viewport))  or (Viewport.HandleAllocated = False)  or (Viewport.InRepainting) then
   Exit;
  with Viewport2D do
   try
     Canvas.Pen.Mode := pmXOr;
     Canvas.Pen.Color := CursorColor xor BackGroundColor;
     Canvas.Pen.Style := psSolid;
     Canvas.Pen.Width := 1;
     _DrawCursorCross2D(Viewport2D, fLastCursorPos);
   except
   end;
end;

// =====================================================================
// TCADPrg3D
// =====================================================================

function TCADPrg3D.GetVPPoint: TPoint2D;
begin
  Result := fCurrentViewportPoint;
end;

procedure TCADPrg3D.SetVPPoint(const Pt: TPoint2D);
begin
  fCurrentViewportPoint := CartesianPoint2D(Pt);
  fCurrentWorldPoint := Viewport3D.ViewportToWorld(Pt);
end;

procedure TCADPrg3D.SetWorldPoint(Pt: TPoint3D);
begin
  fCurrentWorldPoint := CartesianPoint3D(Pt);
  fCurrentViewportPoint := Viewport3D.WorldToViewport(Pt);
end;

function TCADPrg3D.WorldToWorkingPlane(WPt: TPoint3D): TPoint3D;
begin
  Result := CastPointOnPlane3D(WPt, Viewport3D.GetRayVersor(WPt), fWorkingPlaneNormal, fWorkingPlaneOrigin);
end;

function TCADPrg3D.GetCurrentWorkPlanePoint: TPoint3D;
begin
  Result := CastPointOnPlane3D(fCurrentWorldPoint, Viewport3D.GetRayVersor(fCurrentWorldPoint), fWorkingPlaneNormal, fWorkingPlaneOrigin);
end;

function TCADPrg3D.GetWorkingPlaneXDir: TVector3D;
begin
  Result := CrossProd3D(fWorkingPlaneUP, fWorkingPlaneNormal);
end;

function TCADPrg3D.GetWPSnappedPoint: TPoint3D;
var
  TmpPt, TmpOrg: TPoint2D;
  TmpPt3D: TPoint3D;
  TmpDir: TVector3D;
begin
  TmpDir := Viewport3D.GetRayVersor(fCurrentWorldPoint);
  Result := CastPointOnPlane3D(fCurrentWorldPoint, TmpDir, fWorkingPlaneNormal, fWorkingPlaneOrigin);
  if not UseSnap then
   Exit;
  TmpPt := WorldPointToPlane3D(Result, TmpDir, fWorkingPlaneNormal, fWorkingPlaneUP, fWorkingPlaneOrigin);
  with TmpPt do
   begin
     if XSnap <> 0 then
      TmpPt.X := Round(X / XSnap) * XSnap;
     if YSnap <> 0 then
      TmpPt.Y := Round(Y / YSnap) * YSnap;
   end;
  // Sul work plane i punti sono vincolati sul piano.
  if Assigned(fSnapFilter) then
   begin
     if IsSamePoint3D(fSnapOriginPoint, Point3D(MaxCoord, MaxCoord, MaxCoord)) then
      TmpOrg := Point2D(MaxCoord, MaxCoord)
     else
      begin
        TmpPt3D := CastPointOnPlane3D(fSnapOriginPoint, TmpDir, fWorkingPlaneNormal, fWorkingPlaneOrigin);
        TmpOrg := WorldPointToPlane3D(TmpPt3D, TmpDir, fWorkingPlaneNormal, fWorkingPlaneUP, fWorkingPlaneOrigin);
      end;
     fSnapFilter(Self, CurrentState, TmpOrg, TmpPt);
   end;
  Result := PlanePointToWorld3D(TmpPt, fWorkingPlaneNormal, fWorkingPlaneUP, fWorkingPlaneOrigin);
end;

function TCADPrg3D.ViewOnMouseMove(Sender: TObject; Shift: TShiftState; var X, Y: SmallInt): Boolean;
begin
  fCurrentViewportPoint := Viewport3D.ScreenToViewport(Point2D(X, Y));
  fCurrentWorldPoint := Viewport3D.ViewportToWorld(fCurrentViewportPoint);
  if Assigned(fMouseMoveFilter) then
   begin
     fMouseMoveFilter(Self, CurrentState, fCurrentWorldPoint, X, Y);
     fCurrentViewportPoint := Viewport3D.WorldToViewport(fCurrentWorldPoint);
     if Assigned(Viewport3D.OnMouseMove3D) then
      // if there will be a friend directive like in C++, it will be better
      // to call the MouseMove handler directly :)
      Viewport3D.OnMouseMove3D(Sender, Shift, fCurrentWorldPoint.X, fCurrentWorldPoint.Y, fCurrentWorldPoint.Z, X, Y);
     inherited ViewOnMouseMove(Sender, Shift, X, Y);
     Result := False;
   end
  else
   Result := inherited ViewOnMouseMove(Sender, Shift, X, Y);
end;

function TCADPrg3D.ViewOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean;
begin
  fCurrentViewportPoint := Viewport3D.ScreenToViewport(Point2D(X, Y));
  fCurrentWorldPoint := Viewport3D.ViewportToWorld(fCurrentViewportPoint);
  if Assigned(fMouseButtonFilter) then
   begin
     fMouseButtonFilter(Self, CurrentState, Button, fCurrentWorldPoint, X, Y);
     fCurrentViewportPoint := Viewport3D.WorldToViewport(fCurrentWorldPoint);
     if Assigned(Viewport3D.OnMouseDown3D) then
      // if there will be a friend directive like in C++, it will be better
      // to call the MouseMove handler directly :)
      Viewport3D.OnMouseDown3D(Sender, Button, Shift, fCurrentWorldPoint.X, fCurrentWorldPoint.Y, fCurrentWorldPoint.Z, X, Y);
     inherited ViewOnMouseDown(Sender, Button, Shift, X, Y);
     Result := False;
   end
  else
   Result := inherited ViewOnMouseDown(Sender, Button, Shift, X, Y);
end;

function TCADPrg3D.ViewOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; var X, Y: SmallInt): Boolean;
begin
  fCurrentViewportPoint := Viewport3D.ScreenToViewport(Point2D(X, Y));
  fCurrentWorldPoint := Viewport3D.ViewportToWorld(fCurrentViewportPoint);
  if Assigned(fMouseButtonFilter) then
   begin
     fMouseButtonFilter(Self, CurrentState, Button, fCurrentWorldPoint, X, Y);
     fCurrentViewportPoint := Viewport3D.WorldToViewport(fCurrentWorldPoint);
     if Assigned(Viewport3D.OnMouseUp3D) then
      // if there will be a friend directive like in C++, it will be better
      // to call the MouseMove handler directly :)
      Viewport3D.OnMouseUp3D(Sender, Button, Shift, fCurrentWorldPoint.X, fCurrentWorldPoint.Y, fCurrentWorldPoint.Z, X, Y);
     inherited ViewOnMouseUp(Sender, Button, Shift, X, Y);
     Result := False;
   end
  else
   Result := inherited ViewOnMouseUp(Sender, Button, Shift, X, Y);
end;

function TCADPrg3D.GetViewport3D: TCADViewport3D;
begin
  Result := LinkedViewport as TCADViewport3D;
end;

procedure TCADPrg3D.SetViewport3D(View3D: TCADViewport3D);
begin
  LinkedViewport := View3D;
end;

constructor TCADPrg3D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  XSnap := 10.0;
  YSnap := 10.0;
  fCursorSize := 10.0;
  fWorkingPlaneNormal := Versor3D(0, 0, 1);
  fWorkingPlaneUp := Versor3D(0, 1, 0);
  fWorkingPlaneOrigin := Point3D(0, 0, 0);
  fSnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
end;

destructor TCADPrg3D.Destroy;
begin
  SetViewport3D(nil);
  inherited Destroy;
end;

procedure _DrawWorkPlaneCursor(Viewport: TCADViewport3D; const PosWPt: TPoint3D; const X, Y, Z: TVector3D; const Siz: TRealType);
var
  SPt, AxSPt: TPoint;
  AxPt: TPoint3D;
begin
  with Viewport do
   begin
     // Controlla se è visibile.
     AxPt := ExtrudePoint3D(PosWPt, Z, Siz / 4.0);
     AxPt := WorldToNRC(AxPt);
     if AxPt.W <> 1.0 then
      Exit;
     // Disegna una griglietta
     Canvas.Pen.Color := GridColor xor BackGroundColor;
     Canvas.Pen.Style := psDot;
     SetBkMode(Canvas.Handle, TRANSPARENT);
     AxPt := ExtrudePoint3D(PosWPt, X, -Siz / 2.0);
     AxPt := ExtrudePoint3D(AxPt, Y, -Siz / 2.0);
     SPt := NRCToScreen(WorldToNRC(AxPt));
     AxPt := ExtrudePoint3D(AxPt, Y, Siz);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
     AxPt := PosWPt;
     AxPt := ExtrudePoint3D(AxPt, Y, -Siz / 2.0);
     SPt := NRCToScreen(WorldToNRC(AxPt));
     AxPt := ExtrudePoint3D(AxPt, Y, Siz / 2.0);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
     AxPt := ExtrudePoint3D(PosWPt, X, Siz / 2.0);
     AxPt := ExtrudePoint3D(AxPt, Y, -Siz / 2.0);
     SPt := NRCToScreen(WorldToNRC(AxPt));
     AxPt := ExtrudePoint3D(AxPt, Y, Siz);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);

     AxPt := ExtrudePoint3D(PosWPt, Y, -Siz / 2.0);
     AxPt := ExtrudePoint3D(AxPt, X, -Siz / 2.0);
     SPt := NRCToScreen(WorldToNRC(AxPt));
     AxPt := ExtrudePoint3D(AxPt, X, Siz);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
     AxPt := PosWPt;
     AxPt := ExtrudePoint3D(AxPt, X, -Siz / 2.0);
     SPt := NRCToScreen(WorldToNRC(AxPt));
     AxPt := ExtrudePoint3D(AxPt, X, Siz / 2.0);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
     AxPt := ExtrudePoint3D(PosWPt, Y, Siz / 2.0);
     AxPt := ExtrudePoint3D(AxPt, X, -Siz / 2.0);
     SPt := NRCToScreen(WorldToNRC(AxPt));
     AxPt := ExtrudePoint3D(AxPt, X, Siz);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
     // Disegna gli assi.
     Canvas.Pen.Style := psSolid;
     SPt := NRCToScreen(WorldToNRC(PosWPt));
     // Asse X
     if Assigned(fAxis) then
      Canvas.Pen.Color := fAxis.fXColor xor BackGroundColor
     else
      Canvas.Pen.Color := clRed xor BackGroundColor;
     AxPt := ExtrudePoint3D(PosWPt, X, Siz / 2.0);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     // Disegna il segmento.
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
     // Asse Y
     if Assigned(fAxis) then
      Canvas.Pen.Color := fAxis.fYColor xor BackGroundColor
     else
      Canvas.Pen.Color := clBlue xor BackGroundColor;
     AxPt := ExtrudePoint3D(PosWPt, Y, Siz / 2.0);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));
     // Disegna il segmento.
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
     // Asse Z
     if Assigned(fAxis) then
      Canvas.Pen.Color := fAxis.fZColor xor BackGroundColor
     else
      Canvas.Pen.Color := clGreen xor BackGroundColor;
     AxPt := ExtrudePoint3D(PosWPt, Z, Siz / 4.0);
     AxSPt := NRCToScreen(WorldToNRC(AxPt));

     // Disegna il segmento.
     Canvas.MoveTo(SPt.X, SPt.Y);
     Canvas.LineTo(AxSpt.X, AxSpt.Y);
   end;
end;

procedure TCADPrg3D.DrawCursorCross;
begin
  if (not Assigned(Viewport))  or (Viewport.HandleAllocated = False) or (Viewport.InRepainting) then
   Exit;
  with Viewport3D do
   try
     Canvas.Pen.Mode := pmXOr;
     Canvas.Pen.Width := 1;

     // Cancella quello vecchio.
     _DrawWorkPlaneCursor(Viewport3D, fLastCursorPos,
                   CrossProd3D(fWorkingPlaneUP, fWorkingPlaneNormal),
                   fWorkingPlaneUP,
                   fWorkingPlaneNormal,
                   fCursorSize);
     // Disegna quello nuovo
     fLastCursorPos := CurrentWorkingPlanePoint;
     _DrawWorkPlaneCursor(Viewport3D, fLastCursorPos,
                   CrossProd3D(fWorkingPlaneUP, fWorkingPlaneNormal),
                   fWorkingPlaneUP,
                   fWorkingPlaneNormal,
                   fCursorSize);
   except
   end;
end;

procedure TCADPrg3D.HideCursorCross;
begin
  if (not Assigned(Viewport)) or (Viewport.HandleAllocated = False)  or (Viewport.InRepainting) then
   Exit;
  with Viewport3D do
   try
     Canvas.Pen.Mode := pmXOr;
     Canvas.Pen.Width := 1;

     // Cancella il cursore
     _DrawWorkPlaneCursor(Viewport3D, fLastCursorPos,
                   CrossProd3D(fWorkingPlaneUP, fWorkingPlaneNormal),
                   fWorkingPlaneUP,
                   fWorkingPlaneNormal,
                   fCursorSize);
   except
   end;
end;

// =====================================================================
// TCADSysSynchroObject
// =====================================================================

procedure TCADSysSynchroObject.Acquire;
begin
end;

procedure TCADSysSynchroObject.Release;
begin
end;

// =====================================================================
// TCADSysCriticalSection
// =====================================================================

constructor TCADSysCriticalSection.Create;
begin
  inherited Create;
  InitializeCriticalSection(FSection);
end;

destructor TCADSysCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

procedure TCADSysCriticalSection.Acquire;
begin
  EnterCriticalSection(FSection);
end;

procedure TCADSysCriticalSection.Release;
begin
  LeaveCriticalSection(FSection);
end;

procedure TCADSysCriticalSection.Enter;
begin
  Acquire;
end;

procedure TCADSysCriticalSection.Leave;
begin
  Release;
end;

initialization
finalization
end.

