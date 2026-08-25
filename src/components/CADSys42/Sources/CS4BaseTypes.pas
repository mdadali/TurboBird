{: This help file explain all the base types defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These types are defined in the CS4BaseTypes unit file
   that you must include in the <B=uses> clause in all of your units
   that use CADSys.
}
unit CS4BaseTypes;

{$mode delphi}

interface

uses Classes, Graphics, Types, LCLIntf;

type
{: To let the library to use any floating point precision value
   in defining the coordinates of points and so on, all
   floating point number used by the library is of this type.

   At the moment the precision is set to Single but you don't
   rely on this assumption when you create new kind of shapes
   classes or use the library.

   <B=Note>: I don't think that this type will change due to storage
   and speed efficency.
}
  TRealType = Double; //maurog.
{: This type is the result information of a clipping method. The
   clipping functions are used internally by the library and you
   don't need to use them directly.

   The tags have the following meanings:

   <LI=<I=ccFirst> the clipping function has modified the first point of the segment>
   <LI=<I=ccSecond> the clipping function has modified the second point of the segment>
   <LI=<I=ccNotVisible> the segment to be clipped is not visible>
   <LI=<I=ccVisible> the segment to be clipped is fully visible>
}
  TClipCode = (ccFirst, ccSecond, ccNotVisible, ccVisible);
{: This type is a set of <See Type=TClipCode> tags. A clipping function may
   return such a set.
}
  TClipResult = set of TClipCode;
{: This type define the point position against view frustum.
}
  TOutPos = (left, bottom, right, top, neareye, fareye);
{: This type define the point position against view frustum.
}
  TOutCode = set of TOutPos;

{: This type defines a 2D point in homogeneous coordinates.

   All point informations in the library are in homogeneous
   coordinates that is they have a third coordinate W. This
   coordinate may be treated as divisor coefficient for the X and Y
   coordinates.

   A 2D point in the euclidean space (the normally used point) can
   be obtained by dividing each X and Y coordinates by W:

   <Code=
     Xe := X / W;<BR>
     Ye := Y / W;<BR>.
   >

   A point to be valid must have at least one of its coordinates
   not zero. If a point has W = 0 it is called a point at infinitum
   and it is not allowed in the library. Normally this kind of
   points is used to rapresent a direction but in the library
   the <See Type=TVector2D> type is used instead.
}
  TPoint2D = record
   X, Y, W: TRealType;
  end;

{: This type defines a 2D vector or direction.

   Use this type when you need to defines directions in the
   2D space. In the case of 2D application this type may be
   used in defining parametric segments or to specify
   ortogonal segments.
}
  TVector2D = record
   X, Y: TRealType;
  end;

{: This type defines a 2D axis aligned rectangle.

   The rectangle is specified by two of its corners, the
   lower-left ones and the upper-right ones. Consider that
   the origin of the coordinates of the library is different
   from the Windows' one. Use <See Function=Rect2DToRect> and
   <See Function=RectToRect2D> functions to convert from the
   two coordinate systems.

   This type is useful to defines bounding boxes of shapes.
}
  TRect2D = record
   case Byte of
    0: (Left, Bottom, W1, Right, Top, W2: TRealType);
    1: (FirstEdge, SecondEdge: TPoint2D);
  end;

{: This type defines a 2D transformation for homogeneous points
   and vectors.

   The convention used by the library is that a matrix premultiply
   a point, that is:

   <Code=TP = M * T>

   where <I=TP> and <I=T> are points and <I=M> is a matrix.

   The matrix is specified by columns, that is <I=M[2, 1]> is
   the element at row 2 and column 1 and <I=M[1, 2]> is the
   element at row 1 anc column 2.
}
  TTransf2D = array[1..3, 1..3] of TRealType;

{: This type defines the new way of considering a normal vector to a
   plane. In <I=hrRightHand mode> the normal vector is computed by using
   the right hand rule, in the <I=hrLeftHand mode> the normal vector is
   computed by using the left hand rule.
}
  THandleRule = (hrRightHand, hrLeftHand);
{: This type defines a 3D point in homogeneous coordinates.

   All point informations in the library are in homogeneous
   coordinates that is they have a third coordinate W. This
   coordinate may be treated as divisor coefficient for the X, Y and Z
   coordinates.

   A 3D point in the euclidean space (the normally used point) can
   be obtained by dividing each X, Y and Z coordinates by W:

   <Code=
     Xe := X / W;
     Ye := Y / W;
     Ze := Z / W;
   >

   A point to be valid must have at least one of its coordinates
   not zero. If a point has W = 0 it is called a point at infinitum
   and it is not allowed in the library. Normally this kind of
   points is used to rapresent a direction but in the library
   the <See Type=TVector3D> type is used instead.

   Use <See Function=Point3DToPoint2D> and
   <See Function=Point2DToPoint3D> functions to convert from 2D
   points to 3D points.
}
  TPoint3D = record
    X, Y, Z, W: TRealType;
  end;
{: This type defines a 3D vector or direction.

   Use this type when you need to defines directions in the
   3D space. In the case of 3D application this type may be
   used in defining parametric segments or to specify
   normal surface vectors.
}
  TVector3D = record
    X, Y, Z: TRealType;
  end;
{: This type defines a 3D axis aligned rectangle.

   The rectangle is specified by two of its corners, the
   lower-left-front ones and the upper-right-back ones.
   Use <See Function=Rect3DToRect2D> and
   <See Function=Rect2DToRect3D> functions to convert from 2D
   boxes to 3D boxes.

   This type is useful to defines bounding boxes of shapes.
}
  TRect3D = record
   case Byte of
    0: (Left, Bottom, Front, W1, Right, Top, Back, W2: TRealType);
    1: (FirstEdge, SecondEdge: TPoint3D);
  end;
{: This type defines a 3D transformation for homogeneous points
   and vectors.

   The convention used by the library is that a matrix premultiply
   a point, that is:

   <Code=TP = M * T>

   where <I=TP> and <I=T> are points and <I=M> is a matrix.

   The matrix is specified by columns, that is <I=M[2, 1]> is
   the element at row 2 and column 1 and <I=M[1, 2]> is the
   element at row 1 anc column 2.
}
  TTransf3D = array[1..4, 1..4] of TRealType;

  {: Vector of 2D points. }
  TVectPoints2D = array [0..0] of TPoint2D;
  {: Pointer to vector of 2D points. }
  PVectPoints2D = ^TVectPoints2D;

  {: Vector of 3D points. }
  TVectPoints3D = array [0..0] of TPoint3D;
  {: Pointer to vector of 3D points. }
  PVectPoints3D = ^TVectPoints3D;

{: This class defines a decorative pen, that is a special Window pen that
   can have any pattern. The pattern is defined by a string of bits like
   '1110001100', in which a one rapresent a colored pixel and a zero
   rapresent a transparent pixel. By using this pen the redrawing of the
   image will be slower, so use it only where necessary. Because a decorative
   pen is associated to a layer you can manage it better.

   <B=Note>: The decorative pen use LineDDA functions to draw the lines and
   can only be used for lines and polylines (so no ellipses are drawed using
   the pattern).
}
  TDecorativePen = class(TObject)
  private
    fPStyle: TBits;
    fCnv: TCanvas;
    fCurBit: Word;
    fStartPt, fEndPt, fLastPt: TPoint;

    procedure SetBit(const Idx: Word; const B: Boolean);
    function GetBit(const Idx: Word): Boolean;
    function GetMaxBit: Word;
    procedure CallLineDDA;
  public
    constructor Create;
    destructor Destroy; override;
    {: This method is used to make deep copy of the object by obtaining state
       informations from another.

       <I=Obj> is the object from which copy the needed informations.
    }
    procedure Assign(Source: TObject);
    {: Move the pen for the canvas <I=Cnv> to the position <I=X,Y>.
    }
    procedure MoveTo(Cnv: TCanvas; X, Y: Integer);
    {: Move the pen for the canvas <I=Cnv> to the position <I=X,Y>.
       This will not reset the current pattern bit. It is useful
       when you are drawing a shapes made by segment.
    }
    procedure MoveToNotReset(Cnv: TCanvas; X, Y: Integer);
    {: Draw a line using the current pattern and color to the position <I=X,Y>.
    }
    procedure LineTo(Cnv: TCanvas; X, Y: Integer);
    {: Draw a polyline using the current pattern and color.
       <I=Pts> are the points of the polyline and <I=NPts> is the number of points.
    }
    procedure Polyline(Cnv: TCanvas; Pts: Pointer; NPts: Integer);
    {: Specify the pattern for lines.
       The pattern is defined by a string of bits like
       '1110001100', in which a one rapresent a colored pixel and a zero
       rapresent a transparent pixel. By using this pen the redrawing of the
       image will be slower, so use it only where necessary. Because a decorative
       pen is associated to a layer you can manage it better.
    }
    procedure SetPenStyle(const SString: String);
    {: Contains the patter for lines.
       The pattern is defined by a string of bits like
       '1110001100', in which a one rapresent a colored pixel and a zero
       rapresent a transparent pixel. By using this pen the redrawing of the
       image will be slower, so use it only where necessary. Because a decorative
       pen is associated to a layer you can manage it better.
    }
    property PenStyle[const Idx: Word]: Boolean read GetBit write SetBit;
    {: Contains the pattern length.
    }
    property PatternLenght: Word read GetMaxBit;
  end;

{: Defines an object that contains a DecorativePen and a Canvas.
   By using the decorative pen methods you can draw patterned lines,
   and by using the Canvas property you can draw using the
   canvas.

   See also <See Class=TDecorativePen> for details.
}
  TDecorativeCanvas = class(TObject)
  private
    fDecorativePen: TDecorativePen;
    fCanvas: TCanvas;
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;

    {: This method is a shortcut to the MoveTo method of the
       owned decorative pen. I suggest to use it in all of your
       shapes to draw lines.
    }
    procedure MoveTo(X, Y: Integer);
    {: This method is a shortcut to the LineTo method of the
       owned decorative pen. I suggest to use it in all of your
       shapes to draw lines.
    }
    procedure LineTo(X, Y: Integer);
    {: This method is a shortcut to the Polyline method of the
       owned decorative pen. I suggest to use it in all of your
       shapes to draw polylines.
    }
    procedure Polyline(Points: Pointer; NPts: Integer);
    {: Contains the decorative pen.
    }
    property DecorativePen: TDecorativePen read fDecorativePen;
    {: Contains the underling convas used to draw.
    }
    property Canvas: TCanvas read fCanvas write fCanvas;
  end;

  // For stream operation backward compatibility
  TRealTypeSingle = Single;
  TTransf2DSingle = array[1..3, 1..3] of TRealTypeSingle;
  TTransf3DSingle = array[1..4, 1..4] of TRealTypeSingle;
  TPoint2DSingle = record
    X, Y, W: TRealTypeSingle;
  end;
  TPoint3DSingle = record
    X, Y, Z, W: TRealTypeSingle;
  end;
  TVector3DSingle = record
    X, Y, Z: TRealTypeSingle;
  end;

const
  TWOPI = 2 * Pi;
  SQRT2 = 1.414213562373;

  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_NOOBJECT = -200;
  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_INBBOX = -100;
  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_ONOBJECT = -1;
  {: Constant used with the picking functions.

     See also <See Method=TCADViewport2D@PickObject> and
     <See Method=TCADViewport3D@PickObject>.
  }
  PICK_INOBJECT = -2;
  {: This is the identity matrix for 2D transformation.
  }
  IdentityTransf2D: TTransf2D = ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0));
  {: This is the null matrix for 2D transformation.
  }
  NullTransf2D: TTransf2D = ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 1.0));
  {: This is the identity matrix for 3D transformation.
  }
  IdentityTransf3D: TTransf3D = ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0));
  {: This is the null matrix for 3D transformation.
  }
  NullTransf3D: TTransf3D = ((0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 1.0));
  {: This is the minimum value for coordinates.
  }
  MinCoord = -1.0E8;
  {: This is the maximun value for coordinates.
  }
  MaxCoord = 1.0E8;

implementation

{ TDecorativeCanvas }

constructor TDecorativeCanvas.Create(ACanvas: TCanvas);
begin
  inherited Create;
  fCanvas := ACanvas;
  fDecorativePen := TDecorativePen.Create;
end;

destructor TDecorativeCanvas.Destroy;
begin
  fDecorativePen.Free;
  inherited Destroy;
end;

procedure TDecorativeCanvas.MoveTo(X, Y: Integer);
begin
  fDecorativePen.MoveTo(fCanvas, X, Y);
end;

procedure TDecorativeCanvas.LineTo(X, Y: Integer);
begin
  fDecorativePen.LineTo(fCanvas, X, Y);
end;

procedure TDecorativeCanvas.Polyline(Points: Pointer; NPts: Integer);
begin
  fDecorativePen.Polyline(fCanvas, Points, NPts);
end;

{ TDecorativePen }

procedure LineDDAMethod1(X, Y: Integer; lpData: Pointer); stdcall;
var
  NextBit: Integer;
begin
  with TDecorativePen(lpData) do
   begin
     NextBit := (fCurBit + Abs(X - fLastPt.X)) mod GetMaxBit;
     fLastPt := Types.Point(X, Y);
     if (fCurBit < GetMaxBit) and
        (fPStyle[fCurBit] and
        not fPStyle[NextBit]) then
      fCnv.Polyline([Types.Point(fStartPt.X, fStartPt.Y), Types.Point(X, Y)])
     else if not fPStyle[fCurBit] then
      fStartPt := Types.Point(X, Y);
     if (X = fEndPt.X - 1) or (X = fEndPt.X + 1) then
      fCnv.Polyline([Types.Point(fStartPt.X, fStartPt.Y), Types.Point(X, Y)]);
     fCurBit := NextBit;
   end;
end;

procedure LineDDAMethod2(X, Y: Integer; lpData: Pointer); stdcall;
var
  NextBit: Integer;
begin
  with TDecorativePen(lpData) do
   begin
     NextBit := (fCurBit + Abs(Y - fLastPt.Y)) mod GetMaxBit;
     fLastPt := Types.Point(X, Y);
     if (fCurBit < GetMaxBit) and
        (fPStyle[fCurBit] and not fPStyle[NextBit]) then
      fCnv.Polyline([Types.Point(fStartPt.X, fStartPt.Y), Types.Point(X, Y)])
     else if not fPStyle[fCurBit] then
      fStartPt := Types.Point(X, Y);
     if (Y = fEndPt.Y - 1) or (Y = fEndPt.Y + 1) then
      fCnv.Polyline([Types.Point(fStartPt.X, fStartPt.Y), Types.Point(X, Y)]);
     fCurBit := NextBit;
   end
end;

procedure TDecorativePen.SetBit(const Idx: Word; const B: Boolean);
begin
  fPStyle[Idx] := B;
end;

function TDecorativePen.GetBit(const Idx: Word): Boolean;
begin
  Result := fPStyle[Idx];
end;

function TDecorativePen.GetMaxBit: Word;
begin
  Result := fPStyle.Size;
end;

procedure TDecorativePen.CallLineDDA;
begin
  //if (Abs(fEndPt.X - fStartPt.X) > Abs(fEndPt.Y - fStartPt.Y)) then
  // LineDDA(fStartPt.X, fStartPt.Y, fEndPt.X, fEndPt.Y, @LineDDAMethod1, Integer(Self))
  //else
  // LineDDA(fStartPt.X, fStartPt.Y, fEndPt.X, fEndPt.Y, @LineDDAMethod2, Integer(Self));
  fCnv.Line(fStartPt.X, fStartPt.Y, fEndPt.X, fEndPt.Y);
end;

constructor TDecorativePen.Create;
begin
  inherited;

  fPStyle := TBits.Create;
end;

destructor TDecorativePen.Destroy;
begin
  fPStyle.Free;
  inherited;
end;

procedure TDecorativePen.Assign(Source: TObject);
var
  Cont: Integer;
begin
  if (Source = Self) then
   Exit;
  if Source is TDecorativePen then
   begin
     fPStyle.Size := 0;
     for Cont := 0 to TDecorativePen(Source).fPStyle.Size - 1 do
      fPStyle[Cont] := TDecorativePen(Source).fPStyle[Cont];
   end;
end;

procedure TDecorativePen.MoveTo(Cnv: TCanvas; X, Y: Integer);
begin
  if( fPStyle.Size > 0 ) then
   begin
     fStartPt := Types.Point(X, Y);
     fEndPt := Types.Point(X, Y);
     fLastPt := fStartPt;
     fCurBit := 0;
   end
  else
   Cnv.MoveTo(X, Y);
end;

procedure TDecorativePen.MoveToNotReset(Cnv: TCanvas; X, Y: Integer);
begin
  if( fPStyle.Size > 0 ) then
   begin
     fStartPt := Types.Point(X, Y);
     fEndPt := Types.Point(X, Y);
     fLastPt := fStartPt;
   end
  else
   Cnv.MoveTo(X, Y);
end;

procedure TDecorativePen.LineTo(Cnv: TCanvas; X, Y: Integer);
begin
  if( fPStyle.Size > 0 ) then
   begin
     fEndPt := Types.Point(X, Y);
     fCnv := Cnv;
     CallLineDDA;
     fStartPt := Types.Point(X, Y);
     fLastPt := fStartPt;
   end
  else
   Cnv.LineTo(X, Y);
end;

procedure TDecorativePen.Polyline(Cnv: TCanvas; Pts: Pointer; NPts: Integer);
type
  TPoints = array[0..0] of TPoint;
var
  Cont: Integer;
  TmpPts: ^TPoints;
begin
  if NPts <= 1 then
   Exit;
  TmpPts := Pts;
  if( fPStyle.Size > 0 ) then
   begin
     fCnv := Cnv;
     fCurBit := 0;
     fLastPt := TmpPts^[0];
     for Cont := 0 to NPts - 2 do
      begin
        if Cont > 0 then
         fStartPt := fLastPt
        else
         fStartPt := TmpPts^[Cont];
        fEndPt := TmpPts^[Cont + 1];
        CallLineDDA;
      end;
   end
  else
   LCLIntf.Polyline(Cnv.Handle, TmpPts^, NPts);
end;

procedure TDecorativePen.SetPenStyle(const SString: String);
var
  Cont: Integer;
begin
  fPStyle.Size := Length(SString);
  for Cont := 1 to fPStyle.Size do
   if SString[Cont] = '1' then
    SetBit(Cont - 1, True)
   else
    SetBit(Cont - 1, False);
end;

end.

