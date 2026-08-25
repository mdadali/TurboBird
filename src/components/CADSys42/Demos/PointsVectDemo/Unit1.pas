unit Unit1;

{$MODE Delphi}

{: This demo shows all the methods of TPointsSet2D.

   TPointsSet2D is a fundamental type of CADSys and it is
   used to contains a list of points that are used for
   drawing and picking. You need to understand it before
   use the library due its use throughout CADSys.

   Copyright 1998-2000 Piero Valagussa.
}
interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CS4BaseTypes, CADSys4,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CSViewport: TCADViewport2D;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  TmpPointsVect, OtherVect: TPointsSet2D;
  R: TRect2D;
  I: Integer;
  Dist: TRealType;
begin
  {: You can create a new PointsVect by using its constructor.
     Because it is a class this is the only way that you can
     use to obtain a PointsVect.
     The constructor requires the initial size of the vector;
     in this example the vector will be able to contains 10
     points. This value is the number of points that the
     vector can contain before expanding itself (if possible,
     see below). The vector however is empty until you add
     new points.
     Although you can give zero as the initial size, by giving
     the needed capacity your code speeds up.
  }
  TmpPointsVect := TPointsSet2D.Create(10);
  {: To add points to you vector you can use Add or AddPoints
     You can use the function Point2D to create a point on the fly
     to be added to the vector.
  }
  TmpPointsVect.Add(Point2D(0, 0));
  TmpPointsVect.AddPoints([Point2D(10, 0),
                           Point2D(10, 10),
                           Point2D(0, 10)]);
  {: Now we have a vector that defines a square of 10 units large.
     Remember that the order of insertion of the points is important.
     Remeber also that the vector is not closed automatically. Actually
     we have an open square.
     We can close the square by adding a new point.
  }
  TmpPointsVect.Add(Point2D(0, 0));
  {: If you want to insert a new point you can use the Insert method.
     The next line of code will insert a new point between the second
     and the third point, defining the shape of an home.
  }
  TmpPointsVect.Insert(2, Point2D(5, 15));
  { You can remove a point by using the Delete method.
    With the next line we get the square again. }
  TmpPointsVect.Delete(2);
  { You can get the bounding box of the set throught the
    Extension property. The extension is computed each time
    the function is called so if you need the bounding box
    in different places cache it in a variable.
  }
  R := TmpPointsVect.Extension;
  { The property Count contains the number of points in the set,
    and the Capacity property contains the max number of points
    that can be added to the set before resizing. If the property
    GrowingEnabled is False the set will never be resized and an
    exception is raised if more that Capacity points are added to it.
    By default GrowingEnabled is True.
    You can access the points in the set by using the Points
    array property or simply (because it is a default property) by
    accessing the set as an array.
  }
  for I := 0 to TmpPointsVect.Count - 1 do
   ShowMessage(Format('%f, %f', [TmpPointsVect[I].X, TmpPointsVect.Points[I].Y]));
  { The DisableEvents property is used to disable/enable the OnChange
    event notification of the set. If the OnChange event is assigned
    for a set, it will be called every time the set is changed. But
    if DisableEvents is True then it will not be called.
  }
  { The set has a new property, PointsReference that is a pointer to
    the array of points in the set. You can use it when you want to
    access points directly or when a procedure or functions needs
    this kind of pointer. See the help for details.
    The pointer is of kind PVectPoints2D.
  }
  for I := 0 to TmpPointsVect.Count - 1 do
   with PVectPoints2D(TmpPointsVect.PointsReference)^[I] do
    ShowMessage(Format('%f, %f', [X, Y]));
  { When you want a deep copy of a set use the Copy method.
  }
  OtherVect := TPointsSet2D.Create(TmpPointsVect.Capacity);
  try
    OtherVect.Copy(TmpPointsVect, 0, TmpPointsVect.Count - 1);
  finally
    OtherVect.Free;
  end;
  { You can test a point against a set using the IsPointOnPolyLine2D method.
  }
  if( IsPointOnPolyLine2D(TmpPointsVect.PointsReference, TmpPointsVect.Count,
                           Point2D(10, 10), Dist, 1.0, IdentityTransf2D, False) = PICK_ONOBJECT ) then
   ShowMessage('The point is on the set.')
  else
   ShowMessage('The point is not on the set.');
  { You can also test if a point is in the set (considered as a closed polyline).
  }
  I := IsPointInPolygon2D(TmpPointsVect.PointsReference, TmpPointsVect.Count,
                           Point2D(5, 5), Dist, 1.0, IdentityTransf2D);
  if( I = PICK_ONOBJECT ) then
   ShowMessage('The point is on the set.')
  else if( I = PICK_INOBJECT ) then
   ShowMessage('The point is in the set.')
  else
   ShowMessage('The point is not in the set.');
  { If you want to transform all the points in the set you can
    use the TransformPoints method.
  }
  TmpPointsVect.TransformPoints(Rotate2D(DegToRad(45)));
  { If you want to remove all the points in the vector use the
    Clear method.
  }
  TmpPointsVect.Clear;
  {: Don't forget to delete you vector after you have done your job.
  }
  TmpPointsVect.Free;
end;

{ For ADVANCED USE }

{ A set of points is not limited to contains only 2D points. For
  example it may be useful to create a set of 2D points that have
  also a Tag property for every point (to contain sharing information
  for instance). To do that you must derive the new set from TPointsSet2D
  as in the following example.
  The new set may be used in the 2D shapes that allows the user to
  modify the kind of set to use.
}
type
  { Define a record that contains the new informations. }
  TTagsPoints2D = class
    Tag: Integer;
    constructor Create(T: Integer);
  end;

  { Define the new set. }
  TTaggedPointsVect2D = class(TPointsSet2D)
  private
    fTags: TStringList; // I use a string list but its better to use a faster approach.
  protected
    { This method is called when a new point is set. }
    procedure Put(PutIndex, ItemIndex: Word; const Item: TPoint2D); override;
  public
    constructor Create(const _Capacity: Word); override;
    destructor Destroy; override;

    { You have to give a way to access the extra datas. }
    property Tags: TStringList read fTags;
  end;

{ Implement the new set. }

constructor TTagsPoints2D.Create(T: Integer);
begin
  inherited Create;
  Tag := T;
end;

procedure TTaggedPointsVect2D.Put(PutIndex, ItemIndex: Word; const Item: TPoint2D);
begin
  { First of all call the inherited. }
  inherited;
  { Then you must manage the additional infos (set with Tags property)
    when a point is inserted, moved or deleted.
  }
  if ItemIndex = PutIndex then
   begin
     { The point in ItemIndex is replaced with a new point. In this
       case the old tag must be deleted because it is now invalid.
       Normally the user must set the new Tag after setted the new
       point. This will be the case when the used use the following
       code:
         aSet[I] := Point2D(x, y);
         aSet.Tags[I] := TTagsPoint2D.Create(20);
       PutIndex will be equal to I.
     }
     fTags.Objects[PutIndex].Free;
     fTags.Objects[PutIndex] := nil;
   end
  else if ItemIndex = PutIndex - 1 then
   begin
     { This case happens when an insert process is scheduled.
       You have to replace the Tag in PutIndex with the Tag in ItemIndex.
     }
     fTags.Objects[PutIndex].Free;
     fTags.Objects[PutIndex] := nil;
     fTags.Objects[PutIndex] := fTags.Objects[ItemIndex];
     fTags.Objects[ItemIndex] := nil;
   end
  else if ItemIndex = PutIndex + 1 then
   begin
     { This case happens when a delete process is scheduled.
       You have to replace the Tag in ItemIdex with the Tag in PutIndex.
     }
     fTags.Objects[PutIndex].Free;
     fTags.Objects[PutIndex] := nil;
     fTags.Objects[PutIndex] := fTags.Objects[ItemIndex];
     fTags.Objects[ItemIndex] := nil;
   end;
end;

constructor TTaggedPointsVect2D.Create(const _Capacity: Word);
var
  I: Integer;
begin
  { First of all call the inherited. }
  inherited;
  { Create the additional container. }
  fTags := TStringList.Create;
  for I := 0 to _Capacity - 1 do
   fTags.AddObject('', nil);
end;

destructor TTaggedPointsVect2D.Destroy;
var
  I: Integer;
begin
  { Destroy the additional container and the call the inherited. }
  for I := 0 to fTags.Count - 1 do
   fTags.Objects[I].Free;
  fTags.Free;
  inherited;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  TagSet: TTaggedPointsVect2D;
  I: Integer;
begin
  { Create a new set. }
  TagSet := TTaggedPointsVect2D.Create(5);
  { Add a new point and its tag. }
  TagSet.Add(Point2D(0, 0));
  TagSet.Tags.Objects[TagSet.Count - 1] := TTagsPoints2D.Create(0);
  { Add a two points. }
  TagSet.AddPoints([Point2D(10, 0), Point2D(0, 10)]);
  { Set the tag for the second added point. }
  TagSet.Tags.Objects[2] := TTagsPoints2D.Create(2);
  { Insert a point in the third position. }
  TagSet.Insert(2, Point2D(10, 10));
  TagSet.Tags.Objects[2] := TTagsPoints2D.Create(3);
  { Delete the second point. }
  TagSet.Delete(1);
  { Show the points and tags. }
  for I := 0 to TagSet.Count - 1 do
   with TagSet.Points[I] do
    if TagSet.Tags.Objects[I] = nil then
     ShowMessage(Format('%f, %f', [X, Y]))
    else
     ShowMessage(Format('%f, %f - %d', [X, Y, TTagsPoints2D(TagSet.Tags.Objects[I]).Tag]));
  { Delete the set. }
  TagSet.Free;
  { As you can see you can use the new set as any other set if you omit the assignment
    of the Tags. }
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CSViewport.VisualRect := Rect2D(-5, -5, 20, 20);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  TmpSet: TPointsSet2D;
begin
  { If you want to draw a set on a Canvas you have to supply the canvas (a decorative
    canvas) and a visual transformation. Normally you may need to draw a set when
    you are implementing a new shape in its Draw method or DrawControlPoints, in this
    case you have already these two parameters.
    Otherwise you can draw a set directly on a viewport as showed in the following example.
  }
  TmpSet := TPointsSet2D.Create(7);
  TmpSet.AddPoints([Point2D(0, 0), Point2D(0, 10), Point2D(5, 15), Point2D(10, 10), Point2D(10, 0), Point2D(0, 0)]);
  with CSViewport do
   try
     { First set the canvas of the viewport. See also the help file on the DecorativeCanvas.
     }
     OnScreenCanvas.Canvas.Pen.Color := clRed;
     OnScreenCanvas.Canvas.Brush.Color := clWhite;
     { Now you have to draw the set. You can draw the set as a polyline.
       Please note that the Clip parameter (the second) is the clipping rectangle
       in Windows Coordinate and not the visual rectangle of the viewport. So you
       have to use the ClientRect or better the ClippingRect of the control or the
       Canvas, and you must convert it to 2D coordinates using RectToRect2D.
     }
     TmpSet.DrawAsPolyline(OnScreenCanvas, RectToRect2D(ClientRect), TmpSet.Extension, ViewportToScreenTransform);
     Sleep(1000);
     { You can also draw the set as a polygon.
     }
     Refresh;
     TmpSet.TransformPoints(Rotate2D(DegToRad(10)));
     TmpSet.DrawAsPolygon(OnScreenCanvas, RectToRect2D(ClientRect), TmpSet.Extension, ViewportToScreenTransform);
     Sleep(1000);
     { Finally you can draw only a subset of the set as a polyline using the
       DrawSubsetAsPolyline ...
     }
     Refresh;
     TmpSet.TransformPoints(Rotate2D(DegToRad(10)));
     TmpSet.DrawSubsetAsPolyline(OnScreenCanvas, RectToRect2D(ClientRect),
                                 TmpSet.Extension, ViewportToScreenTransform,
                                 0, 3, True);
     { ... or as a polygon using DrawSubsetAsPolygon.
     }
     Sleep(1000);
     Refresh;
     TmpSet.TransformPoints(Rotate2D(DegToRad(-10)));
     TmpSet.DrawSubsetAsPolygon(OnScreenCanvas, RectToRect2D(ClientRect),
                                TmpSet.Extension, ViewportToScreenTransform,
                                0, 3);
   finally
     TmpSet.Free;
   end;
end;

end.

