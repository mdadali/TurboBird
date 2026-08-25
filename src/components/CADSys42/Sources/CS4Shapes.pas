{: This help file explain all the entities classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Shapes unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.
}
unit CS4Shapes;

{$mode delphi}

interface

uses SysUtils, Classes, Graphics,
     CADSys4, CS4BaseTypes, LCLType, LCLIntf;

type
  {: This type defines the type used to specify the name of
     a <I=font type face> (like Times New Roman).
  }
  TFaceName = string[LF_FACESIZE];
  {: This class encapsulates the interface for the GDI font of
     Windows as defined by <Code=TLOGFONT> structure.

     This font is used by the library for the <See Class=TText2D>
     shape class. The use of it is somewhat difficult in the
     context of a 2D or 3D drawing so it is better to use
     the vectorial text shape <See Class=TJustifiedVectText2D> and
     <See Class=TJustifiedVectText3D>.
  }
  TExtendedFont = class(TObject)
  private
    LogFont: TLOGFONT;
    FHandle: HFONT;
    FCanvas: TCanvas;
    procedure SetNewValue;
    procedure SetCanvas(Cnv: TCanvas);
    procedure SetHeight(Value: Word);
    function GetHeight: Word;
    procedure SetWidth(Value: Word);
    function GetWidth: Word;
    procedure SetEscapement(Value: Word);
    function GetEscapement: Word;
    procedure SetWeight(Value: Word);
    function GetWeight: Word;
    procedure SetItalic(Value: Byte);
    function GetItalic: Byte;
    procedure SetUnderline(Value: Byte);
    function GetUnderline: Byte;
    procedure SetStrikeOut(Value: Byte);
    function GetStrikeOut: Byte;
    procedure SetCharSet(Value: Byte);
    function GetCharSet: Byte;
    procedure SetOutPrecision(Value: Byte);
    function GetOutPrecision: Byte;
    procedure SetClipPrecision(Value: Byte);
    function GetClipPrecision: Byte;
    procedure SetQuality(Value: Byte);
    function GetQuality: Byte;
    procedure SetPicthAndFamily(Value: Byte);
    function GetPicthAndFamily: Byte;
    procedure SetFaceName(Value: TFaceName);
    function GetFaceName: TFaceName;
  public
    {: This is the constructor that creates an instance of a
       font.

       When a new font is created it is set using the
       <I=DEFAULT_GUI_FONT> as defined in Windows specifications.
    }
    constructor Create;
    {: This destructor frees the font informations.

       It also detaches the font form a <I=Canvas>, if the font is
       currently in use by it.
    }
    destructor Destroy; override;
    {: This method assign the font data by using another font
       class as a prototype.

       Parameters:

       <LI=<I=Obj> is the font being used as a prototype.>
    }
    procedure Assign(Obj: TExtendedFont);
    {: This method saves the font informations into a stream.

       Parameters:

       <LI=<I=Strm> is the stream on which save the font structure.>
    }
    procedure SaveToStream(Strm: TStream);
    {: This method retrieves the font informations from a stream.

       Parameters:

       <LI=<I=Strm> is the stream from which retrieve the font
        structure.>
    }
    procedure LoadFromStream(Strm: TStream);
    {: This property attaches the font to a Canvas.

       If you want to use the font on a Canvas you must use this
       property. After you have setted this propery, to detach
       the font from the Canvas assign <B=nil> to this property.
    }
    property Canvas: TCanvas read FCanvas write SetCanvas;
    {: This property contains the handle for the
       <Code=TLOGFONT> structure.
    }
    property Handle: HFONT read FHandle;
    {: This property specifies the <I=lfHeight> field of <Code=TLOGFONT>.
    }
    property Height: Word read GetHeight write SetHeight;
    {: This property specifies the <I=lfWidth> field of <Code=TLOGFONT>.
    }
    property Width: Word read GetWidth write SetWidth;
    {: This property specifies the <I=lfEscapement> field of
       <Code=TLOGFONT>.
    }
    property Escapement: Word read GetEscapement write SetEscapement;
    {: This property specifies the <I=lfWeight> field of
       <Code=TLOGFONT>.
    }
    property Weight: Word read GetWeight write SetWeight;
    {: This property specifies the <I=lfItalic> field of
       <Code=TLOGFONT>.
    }
    property Italic: Byte read GetItalic write SetItalic;
    {: This property specifies the <I=lfUnderline> field of
       <Code=TLOGFONT>.
    }
    property Underline: Byte read GetUnderline write SetUnderline;
    {: This property specifies the <I=lfStrikeOut> field of
       <Code=TLOGFONT>.
    }
    property StrikeOut: Byte read GetStrikeOut write SetStrikeOut;
    {: This property specifies the <I=lfCharSet> field of
       <Code=TLOGFONT>.
    }
    property CharSet: Byte read GetCharSet write SetCharSet;
    {: This property specifies the <I=lfOutPrecision> field of
       <Code=TLOGFONT>.
    }
    property OutPrecision: Byte read GetOutPrecision write SetOutPrecision;
    {: This property specifies the <I=lfClipPrecision> field of
       <Code=TLOGFONT>.
    }
    property ClipPrecision: Byte read GetClipPrecision write SetClipPrecision;
    {: This property specifies the <I=lfQuality> field of
       <Code=TLOGFONT>.
    }
    property Quality: Byte read GetQuality write SetQuality;
    {: This property specifies the <I=lfPitchAndFamily> field of
       <Code=TLOGFONT>.
    }
    property PicthAndFamily: Byte read GetPicthAndFamily write SetPicthAndFamily;
    {: This property specify the <I=lfFaceName> field of
       <Code=TLOGFONT>.
    }
    property FaceName: TFaceName read GetFaceName write SetFaceName;
  end;

  {: This type defines the <I=saving mode> used by the <I=outline> primitive
     shapes.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     This type is used to specify beetwen the two modes:

     <LI=<I=stSpace> forces an <I=outline> to recompute the
      shape's points whenever it must be drawed.>
     <LI=<I=stTime> tell to the <I=outline> to store the
      shape's points and compute them only when the control
      points are changed.>
  }
  TPrimitiveSavingType = (stSpace, stTime);

  {: This type specifies how to draw an arc segment:

     <LI=in the <I=adClockwise> mode the arc segment is
      drawed clockwise>.
     <LI=in the <I=adCounterClockwise> mode the arc segment is
      drawed counter clockwise>.
  }
  TArcDirection = (adClockwise, adCounterClockwise);

  {: This is the class reference type for the
     <See Class=TPrimitive2D> shape class.
  }
  TPrimitive2DClass = class of TPrimitive2D;

  {: This handler can be used to modify a primitive by dragging its
     control points.

     See also <See Class=TObject2DHandler>.
  }
  TPrimitive2DHandler = class(TObject2DHandler)
  public
    procedure DrawControlPoints(const Sender: TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); override;
    function OnMe(const Sender: TObject2D; Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a <I=2D primitive>.

     A primitive shape is an entity which shape is controlled
     by a set of <I=control points>. This class stores and
     handles this set of point allowing the developer to
     focus on the way to draw the shape from these control
     points.

     This is the right class from which derive your own
     entity classes.

     See also <See Class=TOutline2D> and <See Class=TCurve2D>.

     <B=Warning>: This is an abstract class that cannot be used
     directly.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TPrimitive2D = class(TObject2D)
  private
    fPoints: TPointsSet2D;
  protected
    procedure _UpdateExtension; override;
    {: This method allows you to change the type of the set
       of points used to store the <I=control points> of the
       primitive.

       When the entity is created this method is called to
       create the set of <I=control points> that defines
       the shape of the entity.

       By default a <See Class=TPointsSet2D> instance is created
       to store a maximum of <I=Size> points.

       You may want to override this property to create
       a special set of points that is able to store more
       information than a simple point. For instance you can
       derive a new set from <See Class=TPointsSet2D> that for
       every point store the kind of the point. This is the
       first step to create a <I=path shape> that draw arc
       segment as well as straight lines.
    }
    function CreateVect(const Size: Integer): TPointsSet2D; dynamic;
  public
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TCADCmp2D>.

       If the object is added with the method <See Method=TCADCmp@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet2D@GrowingEnabled>
       of <See Property=TPrimitive2D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
    }
    constructor Create(ID: LongInt; NPts: Integer);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the set of <I=control points> used
       to define the shape of the entity.

       See the introduction of <See Class=TPrimitive2D> for details.
    }
    property Points: TPointsSet2D read fPoints write fPoints;
  end;

  {: This class defines a 2D line segment.

     The entity has two <I=control points> that are the extremes of
     the segment.
  }
  TLine2D = class(TPrimitive2D)
  public
    {: This constructor creates a new 2D line segment.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the starting point of the segment.>
       <LI=<I=P2> is the ending point of the segment.>
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a <I=2D outline>.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     For an <I=outline> the control points are the same as
     the <I=profile points>.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     See also <See Class=TCurve2D>.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TOutline2D = class(TPrimitive2D)
  protected
    {: This method is called when the <I=profile points>
       are needed.

       It must returns the set of the <I=profile points>
       created by the class.

       <B=Warning>: You don't have to delete the set of points
       returned by the method.
    }
    function GetProfilePoints: TPointsSet2D; virtual; abstract;
    {: This method returns the number of <I=profile points>
       that is equal (for an <I=outline>) to the number
       of <I=control points>.
    }
    function GetNPts: Integer; virtual; abstract;
    {: This method returns <B=True> if the set of <I=profile points>
       is closed.

       The fact that the set is closed influences the way in which
       it is drawed and picked.
    }
    function GetIsClosed: Boolean; virtual;
  public
    {: This method initializes the profile points vector for using.

       You must call this method before any use of the methods that
       work on the set of <I=profile points>.

       It is better to call this method in a <Code=try-finally>
       block with <See Method=TOutline2D@EndUseProfilePoints>
       in the finally part.
    }
    procedure BeginUseProfilePoints; dynamic;
    {: This method finalizes the <I=profile points> when you
       finish to use them.

       You must call this method when you no longer need to use
       the set of <I=profile points>. This allow the library
       to eventually saves the memory used by the entity.

       It is better to call this method in a <Code=try-finally>
       block with this method in the finally part.

       <B=Note>: This method must be called after the
       <See Method=TOutline2D@BeginUseProfilePoints>.
    }
    procedure EndUseProfilePoints; dynamic;
    {: This is the set of <I=profile points> that is used to
       draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property ProfilePoints: TPointsSet2D read GetProfilePoints;
    {: This property contains the size of the set of
       <I=profile points> that is used to draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property NumberOfProfilePts: Integer read GetNPts;
    {: This property is <B=True> when the shape of the
       entity must be considere as closed.
    }
    property IsClosed: Boolean read GetIsClosed;
  end;

  {: This class defines a 2D polyline.

     A polyline is obtained by connecting the <I=profile points> (in
     this case are the same as the <I=control points>) with straight
     line segments.
  }
  TPolyline2D = class(TOutline2D)
  protected
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
  public
    {: This constructor creates a new 2D polyline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        the polyline. If you want to create a pointless polyline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive2D@Points>.>
    }
    constructor Create(ID: LongInt; const Pts: array of TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 2D polygon.

     A polygon is obtained by connecting the <I=profile points> (
     in this case they are the same as the <I=profile points>)
     with straight segments and filling the shape with the current
     brush of the Canvas.
  }
  TPolygon2D = class(TPolyline2D)
  protected
    function GetIsClosed: Boolean; override;
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 2D curve.

     A <I=curve> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     A curve is a 2D polyline in which the points that define
     the shape (<I=profile points>)are not the same as the
     <I=control points> of the entity. In this case the control
     points only control the shape of the curve.

     For this class the <See property=TCurve2D@SavingType> property
     defines the mode of saving used by the entity.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     You have to put the code that defines the <I=profile points>
     from the <I=control points> in the
     <See Method=TCurve2D@PopulateCurvePoints> method.

     See also <See Class=TOutline2D>.

     <B=Note>: The control points and the profile points are
     always in the object model coordinate system !
  }
  TCurve2D = class(TOutline2D)
  private
    fSavingType: TPrimitiveSavingType;
    fCurvePrecision: Word;
    fCurvePoints: TPointsSet2D;
    fCountReference: Integer;

    procedure SetCurvePrecision(N: Word);
    procedure SetPrimitiveSavingType(S: TPrimitiveSavingType);
    procedure FreeCurvePoints;
  protected
    procedure _UpdateExtension; override;
    {: This method is called whenever the <I=profile points>
       must be computed.

       You must redefine this method to fill the set of
       <I=control points> by using the actual set of
       <I=control points>. In defining this method you have
       to call the inherited method passing it the
       right number of <I=profile points> used by the
       entity as the <I=N> parameter.

       In the method you may use the
       <See Property=TOutline2D@ProfilePoints> to add the
       points to the set of <I=profile points>.

       <B=Warning>: Don't call
       <See Method=TOutline2D@BeginUseProfilePoints> nor
       <See Method=TOutline2D@EndUseProfilePoints> in this method.
       Also don't access the <See Property=TOutline2D@NumberOfProfilePts>
       property but use the number of points that you have
       computed.
    }
    function PopulateCurvePoints(N: Word): TRect2D; dynamic;
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
  public
    constructor Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    procedure BeginUseProfilePoints; override;
    procedure EndUseProfilePoints; override;
    {: This property may be used in the
       <See Method=TCurve2D@PopulateCurvePoints> as a parameters
       to control the precision (ie the number of <I=profile points>)
       used to draw the curve profile.

       By default it is 50.
    }
    property CurvePrecision: Word read fCurvePrecision write SetCurvePrecision;
    {: This property specify the saving mode used by the
       curve.

       By default it is set to <I=stTime>.

       See also <See Type=TPrimitiveSavingType>.
    }
    property SavingType: TPrimitiveSavingType read fSavingType write SetPrimitiveSavingType;
  end;

  {: This class defines a 2D rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle.
  }
  TFrame2D = class(TCurve2D)
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D frame.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the bottom-left corner of the frame.>
       <LI=<I=P2> is the upper-right corner of the frame.>
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  {: This class defines a 2D filled rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle (these are in the object model space).
  }
  TRectangle2D = class(TFrame2D)
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines an arc segment of a 2D ellipse.

     The arc is defined by the two corner of the box that
     contains the arc's ellipse, and the starting and ending
     angles of the arc.
  }
  TArc2D = class(TCurve2D)
  private
    FStartAngle, FEndAngle: TRealType;
    FDirection: TArcDirection;

    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
    procedure SetArcDirection(D: TArcDirection);
    procedure GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect2D; override;
  public
    {: This constructor  creates a new arc of a 2D ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that defines the arc's ellipse.>
       <LI=<I=SA> is the starting angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>
       <LI=<I=SA> is the ending angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>

       Note: Once created, the arc has four control points. The
       first two are <I=P1> and <I=P2>; the third is the point
       that lies on the segment from the center of the arc's ellipse
       and the starting point of the arc; the fourth is the point
       that lies on the segment from the center of the arc's ellipse and
       the ending point of the arc.
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the starting angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property StartAngle: TRealType read FStartAngle write SetStartAngle;
    {: This property contains the ending angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property EndAngle: TRealType read FEndAngle write SetEndAngle;
    {: This property contains the direction used to draw the arc.

       See <See Type=TArcDirection> for details.
    }
    property Direction: TArcDirection read FDirection write SetArcDirection;
  end;

  {: This class defines a 2D ellipse.

     The ellipse is defined by the two corner of the box that
     contains it.
  }
  TEllipse2D = class(TCurve2D)
  private
    procedure GetEllipseParams(var CX, CY, RX, RY: TRealType);
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect2D; override;
  public
    {: This constructor creates a new ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that contains the ellipse.>
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  {: This class defines a 2D filled ellipse.

     The ellipse is defined by the two corner of the box that
     contains it.
  }
  TFilledEllipse2D = class(TEllipse2D)
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 2D B-Spline curve.

     The B-Spline is defined by its control points.
     The order of the spline is 3 but you can change it.
  }
  TBSpline2D = class(TCurve2D)
  private
    fOrder: Byte;
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D spline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the control points
        of the spline. If you want to create a pointless spline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction.>
    }
    constructor Create(ID: LongInt; const Pts: array of TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the order of the spline.

       By default it is three (cubic spline).
    }
    property Order: Byte read FOrder write FOrder;
  end;

  {: This class defines a 2D text object that uses the Windows
     font for drawing.

     I created a new text object that is more suitable for a
     2D/3D CAD programs. By the fact that any TTF may be
     converted into the new font format, I discourage the use
     of this object that is keept in the library only for
     backward compatibility.

     See <See Class=TVectFont> for information on the
     new type of Text object.

     <B=Note>: The new text object is not able to fill the
      interior of characters. If you need this capability you
      still have to use this kind of Text object.
  }
  TText2D = class(TPrimitive2D)
  private
    fText: AnsiString;
    fHeight: TRealType;
    fExtFont: TExtendedFont;
    fDrawBox, fRecalcBox: Boolean;
    fClippingFlags: Integer; // Win32s DrawText flags.
  public
    {: Create a new text entity in the rectangle <I=Rect1>, with
       the given <I=Height> and <I=Text>.

       Points[0] will be the left-bottom corner of the text
       bounding box (also used as clipping box for the text)
       and Points[1] the right-up corner.

       The <I=width> of the text is set to the width of <I=Rect1>.
       If you dont know the dimension of the Text on screen, set
       the <See Property=TText2D@AutoSize> property to <B=True>.
       The first time the text will be drawed the bounding box will
       be adjusted automatically.

       The rectangle will be drawed in the current brush and pen
       if <See Property=TText2D@DrawBox> property is <B=True>.
    }
    constructor Create(ID: LongInt; Rect1: TRect2D; Height: TRealType; Txt: AnsiString);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the heigth of the text in world
       units.

       This is different from the Heigth of font used to render
       the text object.
    }
    property Height: TRealType read fHeight write fHeight;
    {: This property contains the <See Class=TExtendedFont>
       instance used to render the text.

       Use it to change the font.
    }
    property LogFont: TExtendedFont read FExtFont;
    {: If this property is set to <B=True> the text box is
       drawed below the text. Otherwise only the text is drawed.

       By default it is <B=False>.
    }
    property DrawBox: Boolean read FDrawBox write FDrawBox;
    {: If this property is <B=True>, the bounding box is changed
       when the object is drawed so it contains the whole text.

       By default it is <B=False>.
    }
    property AutoSize: Boolean read fRecalcBox write fRecalcBox;
    {: This property contains the text string used by the
       text entity.

       You may include more than one line of text simply
       adding <Code=#10#13> beetwen lines.
    }
    property Text: AnsiString read FText write FText;
    {: This property contains the <I=clipping flags> used
       by drawing the text with the <I=DrawText> API function.

       By default the are setted to <I=DT_NOCLIP>.
    }
    property ClippingFlags: Integer read FClippingFlags write FClippingFlags;
  end;

  {: This class rapresents a scalable raster bitmap.

     This object is useful when you want a bitmap with world
     dimension that is scaled when you zoom in a portion of the
     drawing. For instance this can be useful for GIS
     applications. However this object isn?t fast and sometimes
     the bitmap is not drawed. Maybe the problem is the Windows
     95 bitmap support. I will add faster and reliable
     raster capability to the library as soon as I have time.

     You can however use a thirdy part library to enhance this
     object by using this class as a blueprint for your
     specific bitmap entity.
  }
  TBitmap2D = class(TPrimitive2D)
  private
    fBitmap: TBitmap;
    fScaleFactor: TRealType;
    fAspectRatio: TRealType;
    fCopyMode: TCopyMode;

    procedure SetScaleFactor(SF: TRealType);
    procedure SetAspectRatio(AR: TRealType);
  public
    {: This constructor creates a new bitmap object.

       <I=Bmp> is the bitmap to be drawed and it will be freed
        by the object.

       <I=P1> and <I=P2> are the corner points of the bitmap
       in world coordinates (and the bitmap will be stretched
       to fit in).

       <B=Note>: The bitmap cannot be rotated !
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D; Bmp: TBitmap);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const {%H-}DrawMode: Integer); override;
    {: This property contains the bitmap to be drawed.

       It will be freed by the object.
    }
    property Bitmap: TBitmap read FBitmap;
    {: This property may contains the scale factor to be used for the
       bitmap.

       If you set this property the bounding box is recomputed and
       the bitmap is not stretched. The first Points will remain
       in the position specified but the second point will be
       repositioned using the ScaleFactor.

       The value rapresent how many drawing unit correspond to
       one pixel in the image. So an image of 50x50 pixels with
       a ScaleFactor of 2.0 will be 100x100 drawing units large.
       How many application units (cm, inch, etc) is a drawing
       unit is left to you.
       
       Setting it to zero (the default) means that no scale is
       needed.
    }
    property ScaleFactor: TRealType read fScaleFactor write SetScaleFactor;
    {: This property may contains the aspect ratio (Width/Heigth) to
       be used for the bitmap.

       This property is used only if ScaleFactor is not 0.0

       Setting it to zero (the default) means that no aspect ratio is
       needed.
    }
    property AspectRatio: TRealType read fAspectRatio write SetAspectRatio;
    {: This property contains the CopyMode used to copy the
       bitmap.
    }
    property CopyMode: TCopyMode read fCopyMode write fCopyMode;
  end;

  {: This class defines a 2D/3D vectorial char as a set of
     polylines.

     The number of polylines that can be used to define a char
     is limited by the size given at the moment of creation.

     In the char definition the polylines must be defined in the
     unitary square that ranges from X:0,1 and Y:0,1, so that
     the char may be scaled with a scale transformation.

     When the char is drawed inside a string, the real dimension
     of the char is used (the font is a proportional one).

     <B=Note>: The vectorial character may be created with
     conversion from Windows True Type Font with a
     separate utility given with the library.
  }
  TVectChar = class(TObject)
  private
    fSubVects: TIndexedObjectList;
    fExtension: TRect2D;

    function GetVect(Idx: Integer): TPointsSet2D;
    function GetVectCount: Integer;
  public
    {: This constructor creates an new instance of the vectorial char.

       Parameters:

       <LI=<I=NSubVect> is the number of polylines that defines
        the char.>
    }
    constructor Create(NSubVect: Integer);
    destructor Destroy; override;
    {: This method create a new instance of a char definition by
       retrieves its datas from a stream.

       Parameters:

       <LI=<I=Stream> is the stream that contains the char
        definition (previously saved with
        <See Method=TVectChar@SaveToStream>).>
    }
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion);
    {: This method saves a char definition into a stream.

       Parameters:

       <LI=<I=Stream> is the stream into which save the char
        definition (that can be retrived with
        <See Method=TVectCharCreateFromStream>).>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method is used computes the real dimension of the
       char.

       This method <B=MUST> be called when the definition of
       the char is finished (that is when you finish to
       create the polylines of the char).
    }
    procedure UpdateExtension({%H-}Sender: TObject);
    {: This property contains the set of polylines that defines
       the char.

       The polylines are stored in instances of
       <See Class=TPointsSet2D>.

       <I=Idx> is the index of the polyline in the set.
    }
    property Vectors[Idx: Integer]: TPointsSet2D read GetVect;
    {: This property contains the number of polylines that
       defines the char.

       This property cannot be changed.
    }
    property VectorCount: Integer read GetVectCount;
    {: This property contains the bounding box of the char.

       This value is updated by calling
       <See Method=TVectChar@UpdateExtension>. You must call
       that method before use this property.
    }
    property Extension: TRect2D read fExtension;
  end;

  {: This class defines a 2D vectorial font typeface.

     The typeface is made up of 256 instances of
     <See Class=TVectChar> (so no unicode may be used). The font
     may be stored and retrived from a font file.

     The easy way to define a font typeface is by using the
     <I=True Type Font> converter that create it from a TTF font.

     The chars are indexed by their <I=ASCII> value. If a char
     is not defined, it will not be drawed and an underline will
     be shown.

     The space and carriage returns are handled automatically,
     but you can change their shape.

     Any font used by the application is registered in the
     system with an <I=Index>, that is used to save and retrieve
     the font from the disk and associate the correct font to
     the texts.

     If you change the indexes among different drawings, you
     could load the wrong file. In this case you will notice that
     the text is drawed with a different font. If the index
     doesn't correspond to any font and no default font is
     defined an exception will be raised and the drawing will not
     be loaded.

     See <See Function=CADSysSetDefaultFont> and the other
     registration functions for details.
  }
  TVectFont = class(TObject)
  private
    fVects: TIndexedObjectList;

    function GetChar(Ch: Char): TVectChar;
  public
    constructor Create;
    destructor Destroy; override;
    {: This method creates an instance of the font and retrieves
       its definition from a Stream.

       Parameters:

       <LI=<I=Stream> is the stream from which retrieve the
        font definition.>
    }
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion);
    {: This method saves an instance of the font into a Stream.

       Parameters:

       <LI=<I=Stream> is the stream to which saves the font
        definition.>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method draws a char of the font on a canvas using a
       transform mapping from a 2D viewing system.

       Parameters:

       <LI=<I=Ch> is the index (ASCII) of the char to be drawed.>
       <LI=<I=DrawPoint> is the 2D point in world coordinates at
        which draw the char (it is the botton-left corner of the
        bounding box of the char). The point is changed by the
        method into the position for the next char on the same
        text line.>
       <LI=<I=H> is the size of the char in world units. This value
        is used to scale the char definition.>
       <LI=<I=ICS> is the space between two chars (in normalized
        units). For example a value of 0.2 means a space of
        20% of H.>
       <LI=<I=VT> is the mapping transform from world to screen.
        It may be obtained from the
        <See Property=TCADViewport@ViewportToScreenTransform>
        property.>
       <LI=<I=Cnv> is the canvas on which draw the char.>
    }
    procedure DrawChar2D(Ch: Char; var DrawPoint: TPoint2D; const H, ICS: TRealType; const VT: TTransf2D; Cnv: TDecorativeCanvas);
    {: This method draws a char of the font on a canvas using a
       normalizing-projection and mapping transforms from a 3D
       viewing system.

       Parameters:

       <LI=<I=Ch> is the index (ASCII) of the char to be drawed.>
       <LI=<I=DrawPoint> is the 2D point in world coordinates at
        which draw the char (it is the botton-left corner of the
        bounding box of the char). The point is changed by the
        method into the position for the next char on the same
        text line. This point is referenced on the X-Y
        plane. >
       <LI=<I=H> is the size of the char in world units. This value
        is used to scale the char definition.>
       <LI=<I=ICS> is the space between two chars (in normalized
        units). For example a value of 0.2 means a space of
        20% of H.>
       <LI=<I=NT> is the projection-normalizing transform from
        world to normalized view volume.
        It may be obtained from the
        <See Property=TCADViewport3D@ViewNormalization>
        property.>
       <LI=<I=VT> is the mapping transform from the normalized
        view volume to  the screen.
        It may be obtained from the
        <See Property=TCADViewport3D@ViewMapping>
        property.>
       <LI=<I=Cnv> is the canvas on which draw the char.>
    }
    procedure DrawChar3D(Ch: Char; var DrawPoint: TPoint2D; const H, ICS: TRealType; const NT: TTransf3D; const VT: TTransf2D; Cnv: TDecorativeCanvas);

    {: This method returns the bounding box of a vectorial text string
       when the current font is used to draw it.

       Parameters:

       <LI=<I=Str> is the string.>
       <LI=<I=H> is the size of the char in world units.>
       <LI=<I=InterChar> is the space beetwen two chars
        (in normalized units). For example a value of 0.2 means
        a space of 20% of H.>
       <LI=<I=InterLine> is the space beetwen two lines of the
        text (in normalizaed units). For example a value of 0.2
        means a space of 20% of H.>
    }
    function GetTextExtension(Str: AnsiString; H, InterChar, InterLine: TRealType): TRect2D;
    {: This method creates a new char definition and returns the
       <See Class=TVectChar> that rapresents it.

       You must use the returned value to define the char.

       If the char is already present it will be deleted.

       Parameters:

       <LI=<I=Ch> is the characted to be defined.>
       <LI=<I=N> is the number of polylines used to draw the char.>
    }
    function CreateChar(Ch: Char; N: Integer): TVectChar;
    {: This property contains the set of chars of the font.

       The characters are indexed by their <I=ASCII> code.
    }
    property Chars[Ch: Char]: TVectChar read GetChar;
  end;

  {: This type defines the horizontal justification mode for a
     2D/3D vectorial text:

     <LI=<I=jhLeft> means left justification.>
     <LI=<I=jhRight> means right justification.>
     <LI=<I=jhCenter> means center justification.>
  }
  THJustification = (jhLeft, jhRight, jhCenter);
  {: This type defines the vertical justification mode for a
     2D/3D vectorial text:

     <LI=<I=jvTop> means top justification.>
     <LI=<I=jvBottom> means bottom justification.>
     <LI=<I=jvCenter> means center justification.>
  }
  TVJustification = (jvTop, jvBottom, jvCenter);

  {: This class defines the 2D vectorial text.

     The text may be multilines and justified. It uses an
     instance of <See Class=TVectFont> to extract the typeface
     to be used.
  }
  TJustifiedVectText2D = class(TPrimitive2D)
  private
    fVectFont: TVectFont;
    fText: AnsiString;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fBasePoint: TPoint2D;
    fHeight, fCharSpace, fInterLine: TRealType;
    fDrawBox: Boolean;

    procedure SetHeight(H: TRealType);
    procedure SetCharSpace(S: TRealType);
    procedure SetInterLine(S: TRealType);
    procedure SetText(T: String);
    function GetTextExtension: TRect2D;
    procedure DrawText(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
  protected
    procedure _UpdateExtension; override;
  public
    {: This constructor creates a new instance of the class.

       Parameters:

       <LI=<I=ID> is identifier that univocally identify the
        object in the CAD. By means of the method used to add the
        object to the CAD, the <I=ID> of the object might be different
        from the one supplied here. See <See Method=TCADCmp@AddObject>
        for details.>
       <LI=<I=FontVect> is the font typeface. Use
        <See Function=CADSysFindFontByIndex> and
        <See Function=CADSysFindFontIndex> for details.>
       <LI=<I=TextBox> is the rectangle used to justify the text.
        The string is drawed from the upper-left corner of this
        box.>
       <LI=<I=Height> is the size of the font in world units.>
       <LI=<I=Txt> is the text to be drawed.>
    }
    constructor Create(ID: LongInt; FontVect: TVectFont; TextBox: TRect2D; Height: TRealType; Txt: AnsiString);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the size of the font in world unit.
    }
    property Height: TRealType read fHeight write SetHeight;
    {: This property contains the spacing beetwen chars in
       normalized unit.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property CharSpace: TRealType read fCharSpace write SetCharSpace;
    {: This property contains the spacing beetwen lines of the
       text.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property InterLine: TRealType read fInterLine write SetInterLine;
    {: This property contains the instance of the font that is
       used to draw the text.
    }
    property VectFont: TVectFont read fVectFont write fVectFont;
    {: If this property is <B=True>, a frame is drawed around
       the text.
    }
    property DrawBox: Boolean read fDrawBox write fDrawBox;
    {: This property contains the text to be drawed.
    }
    property Text: AnsiString read FText write SetText;
    {: This property specifies the horizontal justification.
    }
    property HorizontalJust: THJustification read fHJustification write fHJustification;
    {: This property specifies the vertical justification.
    }
    property VerticalJust: TVJustification read fVJustification write fVJustification;
  end;

  {: This class defines a <I=planar 3D object>.

     A <I=planar 3D object> is a 3D entity that is defined on a
     plane in the world. That plane is defined by the plane
     normal, a point on the plane and a direction on the plane
     (that is the Y direction of the 2D coordinate system on the
     plane).

     By deriving an object from this class, you have to define
     the object as a 2D object and use the tranformation
     maintained by this class to map the 2D points of the object
     into the equivalents 3D points in the world. See the
     <See Method=TPlanarObject3D@PlaneToWorld> and
     <See Method=TPlanarObject3D@WorldToPlane> methods.

     If the object definition contains 3D points, these must be
     constrained to lies on the plane.

     The <I=model transform> of the graphic object trasform the
     plane parameters that are specified in the object coordinate
     system.
  }
  TPlanarObject3D = class(TObject3D)
  private
    fPlaneReference: TPoint3D;
    fPlaneNormal, fPlaneUP, fWorldPlaneNormal: TVector3D;
    fPlaneToWorldTr, fWorldToPlaneTr: TTransf3D;
    fPlaneTransform: TTransf3D;

    procedure SetPlaneNorm(N: TVector3D);
    procedure SetPlaneUP(U: TVector3D);
    procedure SetPlaneRef(R: TPoint3D);
    procedure UpdateTransforms;
  protected
    procedure _UpdateExtension; override;
  public
    {: This constructor create a new planar object. Because this
       class contains abstract methods not implemented, you
       cannot instantiate it directly.

       Parameters: 

       <LI=<I=ID> is the identifier of the object.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system on the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal of
        the plane. This is used to orientate the plane in the
        world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system on
        the plane.>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This method transforms a 3D point (expressed in world
       coordinate) into a 2D point on the plane.

       The resulting points is always cartesian (W=1.0).

       <B=Note>: It consider also the <I=model transform> of
        the object.
    }
    function WorldToPlane(WPt: TPoint3D): TPoint3D;
    {: This method transforms a 2D point on the plane into
       a 3D points in world coordinates.

       The resulting points is always cartesian (W=1.0).

       <B=Note>: It consider also the <I=model transform> of
        the object.
    }
    function PlaneToWorld(PPt: TPoint3D): TPoint3D;
    function HasTransform: Boolean; override;
    {: This property contains the transform matrix that convert the
       world coordinate system into the plane coordinate system.

       This transform contains also the <I=model transform> of
       the object.

       See also <See Method=TPlanarObject3D@WorldToPlane> and
       <See Method=TPlanarObject3D@PlaneToWorld> methods.
    }
    property WorldToPlaneTransform: TTransf3D read fWorldToPlaneTr;
    {: This property contains the transform coordinate system of the
       plane into the world coordinate system.

       This transform contains also the <I=model transform> of
       the object.

       See also <See Method=TPlanarObject3D@WorldToPlane> and
       <See Method=TPlanarObject3D@PlaneToWorld> methods.
    }
    property PlaneToWorldTransform: TTransf3D read fPlaneToWorldTr;
    {: This property contains the plane orientation transform of
       the object.

       This transform is used to convert the plane coordinate
       system into the model coordinate system. Therefore
       the <I=model transform> of the object is not
       contained in this transform matrix.

       See also <See Property=TPlanarObject3D@PlaneToWorldTransform>
       property.
    }
    property PlaneTransform: TTransf3D read fPlaneTransform;
    {: This property contains a point on the plane that defines
       the origin of the 2D coordinate system of the plane.

       This parameter is in the model coordinates system.
    }
    property PlaneReference: TPoint3D read fPlaneReference write SetPlaneRef;
    {: This property contains a versor that defines the normal
       to the plane.

       This is used to orientate the plane in the world.

       This parameter is in the model coordinates system.
    }
    property PlaneNormal: TVector3D read fPlaneNormal write SetPlaneNorm;
    {: This property contains a versor that must lies on the
       plane and that defines the Y axis for the 2D coordinate
       system of the plane.

       This parameter is in the model coordinates system.
    }
    property PlaneUP: TVector3D read fPlaneUP write SetPlaneUP;
    {: This property contains the versor that defines the normal
       to the plane in the world coordinate system.

       This parameter is in world coordinates. It is the
       same as the <See Property=TPlanarObject3D@PlaneNormal>
       but transformed by the <I=model transform> of the
       object.

       It is useful for ligthing and visibility calculations.
    }
    property WorldPlaneNormal: TVector3D read fWorldPlaneNormal;
  end;

  {: This class defines a 3D planar object that is defined by means
    of a 2D object derived from <See Class=TObject2D>.

    This class allow you to use the same objects you have defined
    in the 2D (Lite) version of the library in the 3D version.

    The 2D object will be drawed, picked and so on automatically
    by the library. Consider that the use of this object gives
    problems with the clipping system. Infact the clipping for
    2D object is different from the one of the 3D system (in the
    latter case clipping in homogeneous coordinates is used) and
    so spurious results may be present.

    Normally it is better to redefine the 2D object as fully 3D
    objects by deriving them from the various planar primitives.
  }
  TPlanar2DObject3D = class(TPlanarObject3D)
  private
    fObject: TObject2D;

    procedure SetObject(O: TObject2D);
  protected
    procedure _UpdateExtension; override;
  public
    {: This constructor creates a new instance of the class.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Obj> is the 2D object used to define the 3D one.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system of the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal to
        the plane. This is used to orientate the plane in the world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system of the
        plane.>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D; const Obj: TObject2D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    procedure ApplyTransform; override;
    {: This property contains the 2D object that is used to
       defines the 3D one.

       This object must not be freed by the user.
    }
    property Object2D: TObject2D read fObject write SetObject;
  end;

  {: This class defines a 3D planar grid.

     It may be used to show the working plane or give a visual
     rapresentation of a 2D plane reference coordinate system.
  }
  TPlanarFieldGrid3D = class(TPlanarObject3D)
  private
    fDeltaX, fDeltaY: TRealType;
    fFieldExt: TRealType; // Indica l'estensione del campo. Non può essere infinito.
    fColor, fXColor, fYColor: TColor;
    fPlaneRef: TPoint3D;
    fPlaneUp, fPlaneNorm: TVector3D;
  protected
    procedure _UpdateExtension; override;
  public
    {: This constructor creates a new instance of the class.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system of the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal to
        the plane. This is used to orientate the plane in the world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system of the
        plane.>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const NormTransf: TTransf3D; const {%H-}VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}DrawMode: Integer); override;
    {: This property contains the distance beetwen two vertical
       division of the grid (on the Y axis).

       This property is in world units and by default it is 50.
    }
    property DeltaX: TRealType read fDeltaX write fDeltaX;
    {: This property contains the distance beetwen two horizontal
       division of the grid (on the X axis).

       This property is in world units and by default it is 50.
    }
    property DeltaY: TRealType read fDeltaY write fDeltaY;
    {: This property contains the grid extension.

       This value rapresents the maximum value for both X and Y
       direction that will be showed in the grid.

       For instance if this value is 1e4 (the default) the
       grid will range from -1e4 to 1e4 in the X and Y directions.
    }
    property FieldExtension: TRealType read fFieldExt write fFieldExt; // default 1e6
    {: This property contains the color of the X axis mark
       on the grid.

       By default it is <I=clRed>, and it will not be streamed.
    }
    property XColor: TColor read fXColor write fXColor;
    {: This property contains the color of the Y axis mark
       on the grid.

       By default it is <I=clBlue>, and it will not be streamed.
    }
    property YColor: TColor read fYColor write fYColor;
    {: This property contains the color of the grid.

       By default it is <I=clGray>, and it will not be streamed.
    }
    property GridColor: TColor read fColor write fColor;
  end;

  {: This handler can be used to modify a primitive by dragging its
     control points.

     See also <See Class=TObject3DHandler>.
  }
  TPrimitive3DHandler = class(TObject3DHandler)
  public
    procedure DrawControlPoints(const Sender: TObject3D; const NormTransf: TTransf3D; const {%H-}VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); override;
    function  OnMe(const Sender: TObject3D; P: TPoint3D; const NormTransf: TTransf3D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a <I=3D primitive>.

     A primitive shape is an entity which shape is controlled
     by a set of <I=control points>. This class stores and
     handles this set of point allowing the developer to
     focus on the way to draw the shape from these control
     points.

     This is the right class from which derive your own
     entity classes.

     See also <See Class=TOutline3D> and <See Class=TCurve3D>.

     <B=Warning>: This is an abstract class that cannot be used
     directly.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TPrimitive3D = class(TObject3D)
  private
    fPoints: TPointsSet3D;
  protected
    {: This method allows you to change the type of the set
       of points used to store the <I=control points> of the
       primitive.

       When the entity is created this method is called to
       create the set of <I=control points> that defines
       the shape of the entity.

       By default a <See Class=TPointsSet3D> instance is created
       to store a maximum of <I=Size> points.

       You may want to override this property to create
       a special set of points that is able to store more
       information than a simple point. For instance you can
       derive a new set from <See Class=TPointsSet3D> that for
       every point store the kind of the point. This is the
       first step to create a <I=path shape> that draw arc
       segment as well as straight lines.
    }
    function CreateVect(const Size: Integer): TPointsSet3D; dynamic;
    procedure _UpdateExtension; override;
  public
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TCADCmp3D>.

       If the object is added with the method <See Method=TCADCmp@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet3D@GrowingEnabled>
       of <See Property=TPrimitive3D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
    }
    constructor Create(ID: LongInt; NPts: Integer);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This property contains the set of <I=control points> used
       to define the shape of the entity.

       See the introduction of <See Class=TPrimitive3D> for details.
    }
    property Points: TPointsSet3D read fPoints write fPoints;
  end;

  {: This class defines a 3D line segment.

     The entity has two <I=control points> that are the extremes of
     the segment.
  }
  TLine3D = class(TPrimitive3D)
  public
    {: This constructor creates a new 3D line segment.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the starting point of the segment.>
       <LI=<I=P2> is the ending point of the segment.>
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint3D);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const NormTransf: TTransf3D; const {%H-}VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
  end;

  {: This class defines a <I=3D outline>.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     For an <I=outline> the control points are the same as
     the <I=profile points>.

     Before using the <I=profile points> you must call the
     <See Method=TOutline3D@BeginUseProfilePoints> and after the
     use you must call the
     <See Method=TOutline3D@EndUseProfilePoints> method.

     See also <See Class=TCurve3D>.

     <B=Note>: This shape utilizes the following flags for
      <See Property=TCADViewport@DrawMode>:

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TOutline3D = class(TPrimitive3D)
  protected
    {: This method is called when the <I=profile points>
       are needed.

       It must returns the set of the <I=profile points>
       created by the class.

       <B=Warning>: You don't have to delete the set of points
       returned by the method.
    }
    function GetProfilePoints: TPointsSet3D; virtual; abstract;
    {: This method returns the number of <I=profile points>
       that is equal (for an <I=outline>) to the number
       of <I=control points>.
    }
    function GetNPts: Integer; virtual; abstract;
    {: This method returns <B=True> if the set of <I=profile points>
       is closed.

       The fact that the set is closed influences the way in which
       it is drawed and picked.
    }
    function GetIsClosed: Boolean; virtual;
  public
    {: This method initializes the profile points vector for using.

       You must call this method before any use of the methods that
       work on the set of <I=profile points>.

       It is better to call this method in a <Code=try-finally>
       block with <See Method=TOutline3D@EndUseProfilePoints>
       in the finally part.
    }
    procedure BeginUseProfilePoints; dynamic;
    {: This method finalizes the <I=profile points> when you
       finish to use them.

       You must call this method when you no longer need to use
       the set of <I=profile points>. This allow the library
       to eventually saves the memory used by the entity.

       It is better to call this method in a <Code=try-finally>
       block with this method in the finally part.

       <B=Note>: This method must be called after the
       <See Method=TOutline3D@BeginUseProfilePoints>.
    }
    procedure EndUseProfilePoints; dynamic;
    {: This is the set of <I=profile points> that is used to
       draw the entity.

       See the class description of <See Class=TOutline3D>.
    }
    property ProfilePoints: TPointsSet3D read GetProfilePoints;
    {: This property contains the size of the set of
       <I=profile points> that is used to draw the entity.

       See the class description of <See Class=TOutline3D>.
    }
    property NumberOfProfilePts: Integer read GetNPts;
    {: This property is <B=True> when the shape of the
       entity must be considere as closed.
    }
    property IsClosed: Boolean read GetIsClosed;
  end;

  {: This class defines a 3D polyline.

     A polyline is obtained by connecting the <I=profile points> (in
     this case are the same as the <I=control points>) with straight
     line segments.

     <B=Note>: If you want a planar polyline, that will be forced to
      lies on the definition plane, you may want to use the
      <See Class=TPlanarPolyline3D> shape class.

     <B=Note>: This shape utilizes the following flags for
      <See Property=TCADViewport@DrawMode>:
  }
  TPolyline3D = class(TOutline3D)
  protected
    function GetProfilePoints: TPointsSet3D; override;
    function GetNPts: Integer; override;
  public
    {: This constructor creates a new 3D polyline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        the polyline. If you want to create a pointless polyline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive3D@Points>.>
    }
    constructor Create(ID: LongInt; const Pts: array of TPoint3D);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
  end;

  {: This calls defines a 3D polygonal face (that is a closed set
     of points).

     This primitive is a closed polyline (you don't need to close
     the polyline) that has two sides. One side faces out in the
     semispace that contains the normal to the face, and the
     other side faces out on the complementary semispace.

     <B=Note>: If you want a planar face, that will be forced to
      lies on the definition plane, you may want to use the
      <See Class=TPlanarFace3D> shape class.

     <B=Note>: This shape utilizes the following flags for
      <See Property=TCADViewport@DrawMode>:
  }
  TFace3D = class(TPolyline3D)
  private
    fNormal: TVector3D;
  protected
    function GetIsClosed: Boolean; override;
    procedure _UpdateExtension; override;
  public
    {: This constructor creates a new 3D face.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        the face. If you want to create a pointless face
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive3D@Points>.>
    }
    constructor Create(ID: LongInt; const Pts: array of TPoint3D);
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    function IsVisible(const NormTransf: TTransf3D; const VRP: TPoint3D; const DrawMode: Integer): Boolean; override;
    {: This property contains the versor that defines the normal
       to the face.

       The normal is in world coordinates and it is computed with
       an approximate method.
    }
    property WorldNormal: TVector3D read fNormal;
  end;

  {: This class defines a 3D planar primitive.

     A <I=planar primitive> shape is an entity which shape is
     controlled by a set of <I=control points> that are
     forced to lies on a plane. This class
     stores and handles this set of point allowing the developer
     to focus on the way to draw the shape from these control
     points.

     This is the right class from which derive your own
     entity classes.

     See also <See Class=TPlanarOutline3D> and
     <See Class=TPlanarCurve3D>.

     <B=Warning>: This is an abstract class that cannot be used
     directly.

     <B=Note>: To apply the planar constraint it uses the
     <See Class=TPlanarObject3D> class. This instance maintains
     the <I=model transform> of the object. The
     <I=control points> are automatically forced on the
     definition plane. The object model
     transform contains also the plane to world transform.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TPlanarPrimitive3D = class(TPrimitive3D)
  private
    fPlanarObj: TPlanarObject3D;
  protected
    function  GetModelTransform: TTransf3D; override;
    procedure SetModelTransform(Transf: TTransf3D); override;
    procedure _UpdateExtension; override;
  public
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TCADCmp3D>.

       If the object is added with the method <See Method=TCADCmp@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet3D@GrowingEnabled>
       of <See Property=TPrimitive3D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system of the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal to
        the plane. This is used to orientate the plane in the world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system of the
        plane.>
    }
    constructor Create(ID: LongInt; NPts: Integer; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure ApplyTransform; override;
    function  HasTransform: Boolean; override;
    {: This method may be used to change the plane parameters of the primitive.

       Parameters:

       <LI=<I=PlaneR> is a point on the plane that defines the
        origin of the 2D reference of the plane.>
       <LI=<I=PlaneN> is a versor that defines the normal of the
        plane. This is used to orientate the plane in the world.
       <LI=<I=PlaneU> is a versor that must lies on the plane and that defines
        the Y axis for the 2D reference of the plane.>
    }
    procedure SetPlaneParameters(const PlaneR: TPoint3D; const PlaneN, PlaneU: TVector3D);
    {: This property contains the instance of
       <See Class=TPlanarObject3D> used by the class as the
       definition plane of the entity.

       Use it to access the planar properties of the object.
       However to change the plane properties use the
       <See Method=TPlanarPrimitive3D@SetPlaneParameters>.
    }
    property PlanarObject: TPlanarObject3D read fPlanarObj;
  end;

  {: This class defines the 3D planar vectorial text.

     The text is drawed as a sequence of planar character
     that have the same plane.

     The text may be multilines and justified. It uses an
     instance of <See Class=TVectFont> to extract the typeface
     to be used.
  }
  TJustifiedVectText3D = class(TPlanarPrimitive3D)
  private
    fVectFont: TVectFont;
    fText: AnsiString;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fBasePoint: TPoint2D;
    fHeight, fCharSpace, fInterLine: TRealType;
    fDrawBox: Boolean;
    procedure SetHeight(H: TRealType);
    procedure SetCharSpace(S: TRealType);
    procedure SetInterLine(S: TRealType);
    procedure SetText(T: String);
    function GetTextExtension: TRect2D;
    procedure DrawText(const NT: TTransf3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
  protected
    procedure _UpdateExtension; override;
  public
    {: This constructor creates a new instance of the class.

       Parameters:

       <LI=<I=ID> is identifier that univocally identify the
        object in the CAD. By means of the method used to add the
        object to the CAD, the <I=ID> of the object might be different
        from the one supplied here. See <See Method=TCADCmp@AddObject>
        for details.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system of the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal to
        the plane. This is used to orientate the plane in the world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system of the
        plane. The lines of text are in this direction. >
       <LI=<I=FontVect> is the font typeface. Use
        <See Function=CADSysFindFontByIndex> and
        <See Function=CADSysFindFontIndex> for details.>
       <LI=<I=TextBox> is the rectangle used to justify the text.
        The string is drawed from the upper-left corner of this
        box. The box is in the 2D plane coordinate system of the
        object.>
       <LI=<I=Height> is the size of the font in world units.>
       <LI=<I=Txt> is the text to be drawed.>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D;
                       const PlaneNorm, PlUp: TVector3D;
                       const FontVect: TVectFont;
                       const TextBox: TRect2D;
                       const Height: TRealType;
                       const Txt: AnsiString);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const NormTransf: TTransf3D; const {%H-}VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    {: This property contains the size of the font in world unit.
    }
    property Height: TRealType read fHeight write SetHeight;
    {: This property contains the spacing beetwen chars in
       normalized unit.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText3D@Height>.
    }
    property CharSpace: TRealType read fCharSpace write SetCharSpace;
    {: This property contains the spacing beetwen lines of the
       text.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText3D@Height>.
    }
    property InterLine: TRealType read fInterLine write SetInterLine;
    {: This property contains the instance of the font that is
       used to draw the text.
    }
    property VectFont: TVectFont read fVectFont write fVectFont;
    {: If this property is <B=True>, a frame is drawed around
       the text.
    }
    property DrawBox: Boolean read fDrawBox write fDrawBox;
    {: This property contains the text to be drawed.
    }
    property Text: AnsiString read FText write SetText;
    {: This property specifies the horizontal justification.
    }
    property HorizontalJust: THJustification read fHJustification write fHJustification;
    {: This property specifies the vertical justification.
    }
    property VerticalJust: TVJustification read fVJustification write fVJustification;
  end;

  {: This class defines a 3D planar outline.

     A <I=planar outline> primitive shape is an entity that is
     drawed as a connected set of points (<I=profile points>)
     which displacement is controlled by a set of <I=control points>.
     The two sets of points are constrained to lie on the definition
     plane.

     For a <I=planar outline> the control points are the same as
     the <I=profile points>.

     Before using the <I=profile points> you must call the
     <See Method=TOutline3D@BeginUseProfilePoints> and after the
     use you must call the
     <See Method=TOutline3D@EndUseProfilePoints> method.

     See also <See Class=TCurve3D>.

     <B=Note>: To apply the planar constraint it uses the
     <See Class=TPlanarObject3D> class. This instance maintains
     the <I=model transform> of the object. The
     <I=control points> are automatically forced on the
     definition plane. The object model
     transform contains also the plane to world transform.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TPlanarOutline3D = class(TOutline3D)
  private
    fPlanarObj: TPlanarObject3D;
  protected
    function  GetModelTransform: TTransf3D; override;
    procedure SetModelTransform(Transf: TTransf3D); override;
    procedure _UpdateExtension; override;
  public
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TCADCmp3D>.

       If the object is added with the method <See Method=TCADCmp@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet3D@GrowingEnabled>
       of <See Property=TPrimitive3D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system of the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal to
        the plane. This is used to orientate the plane in the world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system of the
        plane.>
    }
    constructor Create(ID: LongInt; NPts: Integer; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure ApplyTransform; override;
    function  HasTransform: Boolean; override;
    {: This method may be used to change the plane parameters of the primitive.

       Parameters:

       <LI=<I=PlaneR> is a point on the plane that defines the
        origin of the 2D reference of the plane.>
       <LI=<I=PlaneN> is a versor that defines the normal of the
        plane. This is used to orientate the plane in the world.
       <LI=<I=PlaneU> is a versor that must lies on the plane and that defines
        the Y axis for the 2D reference of the plane.>
    }
    procedure SetPlaneParameters(const PlaneR: TPoint3D; const PlaneN, PlaneU: TVector3D);
    function IsVisible(const NormTransf: TTransf3D; const VRP: TPoint3D; const DrawMode: Integer): Boolean; override;
    function GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean; override;
    {: This property contains the instance of
       <See Class=TPlanarObject3D> used by the class as the
       definition plane of the entity.

       Use it to access the planar properties of the object.
       However to change the plane properties use the
       <See Method=TPlanarOutline3D@SetPlaneParameters>.
    }
    property PlanarObject: TPlanarObject3D read fPlanarObj;
  end;

  {: This class defines a 3D planar polyline.

     A planar polyline is obtained by connecting the
     <I=profile points> (in this case are the same as the
     <I=control points>) with straight line segments on the
     same plane.

     <B=Note>: If you want a non-planar polyline use the
      <See Class=TPolyline3D> shape class.

     <B=Note>: This shape utilizes the following flags for
      <See Property=TCADViewport@DrawMode>:
  }
  TPlanarPolyline3D = class(TPlanarOutline3D)
  protected
    function GetProfilePoints: TPointsSet3D; override;
    function GetNPts: Integer; override;
  public
    {: This constructor creates a new 3D planar polyline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        the polyline. If you want to create a pointless polyline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive3D@Points>.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system of the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal to
        the plane. This is used to orientate the plane in the world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system of the
        plane.>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const Pts: array of TPoint3D);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
  end;

  {: This calls defines a plannar 3D polygonal face (that is a
     closed set of points on the same plane).

     This primitive is a closed polyline (you don't need to close
     the polyline) that has two sides. One side faces out in the
     semispace that contains the normal to the face, and the
     other side faces out on the complementary semispace.

     <B=Note>: If you want a non-planar face use the
      <See Class=TFace3D> shape class.

     <B=Note>: This shape utilizes the following flags for
      <See Property=TCADViewport@DrawMode>:
  }
  TPlanarFace3D = class(TPlanarPolyline3D)
  protected
    function GetIsClosed: Boolean; override;
  public
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 3D curve.

     A <I=curve> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     A curve is a 3D polyline in which the points that define
     the shape (<I=profile points>)are not the same as the
     <I=control points> of the entity. In this case the control
     points only control the shape of the curve.

     For this class the <See property=TCurve3D@SavingType> property
     defines the mode of saving used by the entity.

     Before using the <I=profile points> you must call the
     <See Method=TOutline3D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline3D@EndUseProfilePoints> method.

     You have to put the code that defines the <I=profile points>
     from the <I=control points> in the
     <See Method=TCurve3D@PopulateCurvePoints> method.

     See also <See Class=TOutline3D>.

     <B=Note>: The control points and profile points are always
     in the object model coordinate system !
  }
  TCurve3D = class(TOutline3D)
  private
    fSavingType: TPrimitiveSavingType;
    fCurvePrecision: Word;
    fCurvePoints: TPointsSet3D;
    fCountReference: Integer;
    procedure SetCurvePrecision(N: Word);
    procedure SetPrimitiveSavingType(S: TPrimitiveSavingType);
    procedure FreeCurvePoints;
  protected
    {: This method is called whenever the <I=profile points>
       must be computed.

       You must redefine this method to fill the set of
       <I=control points> by using the actual set of
       <I=control points>. In defining this method you have
       to call the inherited method passing it the
       right number of <I=profile points> used by the
       entity as the <I=N> parameter.

       In the method you may use the
       <See Property=TOutline3D@ProfilePoints> to add the
       points to the set of <I=profile points>.

       <B=Warning>: Don't call
       <See Method=TOutline3D@BeginUseProfilePoints> nor
       <See Method=TOutline3D@EndUseProfilePoints> in this method.
       Also don't access the <See Property=TOutline3D@NumberOfProfilePts>
       property but use the number of points that you have
       computed.
    }
    function PopulateCurvePoints(N: Word): TRect3D; dynamic;
    function GetProfilePoints: TPointsSet3D; override;
    function GetNPts: Integer; override;
    procedure _UpdateExtension; override;
  public
    constructor Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    procedure BeginUseProfilePoints; override;
    procedure EndUseProfilePoints; override;
    {: This property may be used in the
       <See Method=TCurve3D@PopulateCurvePoints> as a parameters
       to control the precision (ie the number of <I=profile points>)
       used to draw the curve profile.

       By default it is 50.
    }
    property CurvePrecision: Word read fCurvePrecision write SetCurvePrecision;
    {: This property specify the saving mode used by the
       curve.

       By default it is set to <I=stTime>.

       See also <See Type=TPrimitiveSavingType>.
    }
    property SavingType: TPrimitiveSavingType read fSavingType write SetPrimitiveSavingType;
  end;

  {: This class defines a 3D planar curve.

     A <I=planar curve> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>.
     The two sets of points are forced to lies on a <I=definition
     plane> so that the curve is a planar one.
     The points used to draw the entity may be keept in memory or
     recomputed from the control points whenever they are needed
     to draw the shape. In the former a lot of memory may be used
     but the drawing is faster, in the latter the drawing of the
     shape may take a quite longer time but the memory is used
     efficently.

     A planar curve is a 3D polyline in which the points that define
     the shape (<I=profile points>)are not the same as the
     <I=control points> of the entity. In this case the control
     points only control the shape of the curve.

     For this class the <See property=TCurve3D@SavingType> property
     defines the mode of saving used by the entity.

     Before using the <I=profile points> you must call the
     <See Method=TOutline3D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline3D@EndUseProfilePoints> method.

     You have to put the code that defines the <I=profile points>
     from the <I=control points> in the
     <See Method=TCurve3D@PopulateCurvePoints> method.

     See also <See Class=TPlanarOutline3D>.

     <B=Note>: By the fact that the control points and profile
     points are forced on a plane, you have to use them as 2D
     points regardless the fact that they are 3D points. If you
     still need to know the control points in world coordinates
     use the <See Method=TPlanarObject3D@PlaneToWorld> method
     on <See Property=TPlanarCurve3D@PlanarObject>. The object model
     transform contains also the plane to world transform.

     <B=Note>: The control points and profile points are always
     in the object model coordinate system !
  }
  TPlanarCurve3D = class(TCurve3D)
  private
    fPlanarObj: TPlanarObject3D;
  protected
    function  GetModelTransform: TTransf3D; override;
    procedure SetModelTransform(Transf: TTransf3D); override;
    procedure _UpdateExtension; override;
  public
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TCADCmp3D>.

       If the object is added with the method <See Method=TCADCmp@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet3D@GrowingEnabled>
       of <See Property=TPrimitive3D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
       <LI=<I=PlaneRef> is a point on the plane that defines the
        origin of the 2D coordinate system of the plane.>
       <LI=<I=PlaneNorm> is a versor that defines the normal to
        the plane. This is used to orientate the plane in the world.>
       <LI=<I=PlUp> is a versor that must lies on the plane and
        that defines the Y axis for the 2D coordinate system of the
        plane.>
    }
    constructor Create(ID: LongInt; NPts: Integer; CurvePrec: Word; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure ApplyTransform; override;
    function  HasTransform: Boolean; override;
    function IsVisible(const NormTransf: TTransf3D; const VRP: TPoint3D; const DrawMode: Integer): Boolean; override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    {: This method may be used to change the plane parameters of the primitive.

       Parameters:

       <LI=<I=PlaneR> is a point on the plane that defines the
        origin of the 2D reference of the plane.>
       <LI=<I=PlaneN> is a versor that defines the normal of the
        plane. This is used to orientate the plane in the world.
       <LI=<I=PlaneU> is a versor that must lies on the plane and that defines
        the Y axis for the 2D reference of the plane.>
    }
    procedure SetPlaneParameters(const PlaneR: TPoint3D; const PlaneN, PlaneU: TVector3D);
    function GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean; override;
    {: This property contains the instance of
       <See Class=TPlanarObject3D> used by the class as the
       definition plane of the entity.

       Use it to access the planar properties of the object.
       However to change the plane properties use the
       <See Method=TPlanarCurve3D@SetPlaneParameters>.
    }
    property PlanarObject: TPlanarObject3D read fPlanarObj;
  end;

  {: This class defines a 3D rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle.
  }
  TFrame3D = class(TPlanarCurve3D)
  private
    procedure GetFramePoints2D(var P0, P1: TPoint2D);
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect3D; override;
  public
    {: This constructor creates a new 3D frame.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=XDir> is the X direction for the X axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=YDir> is the X direction for the Y axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=PlaneRef> is the plane reference point for the
        object. Because a planar outline has its control points
        forced to lie on the construction plane, the control
        points are referenced about this point.>
       <LI=<I=P1> is the bottom-left corner of the frame in the
        2D plane coordinate system.>
       <LI=<I=P2> is the upper-right corner of the frame in the
        2D plane coordinate system .>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const P1, P2: TPoint3D);
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  {: This class defines an arc segment of a 3D ellipse.

     The arc is defined by the two corner of the box that
     contains the arc's ellipse, and the starting and ending
     angles of the arc.
  }
  TArc3D = class(TPlanarCurve3D)
  private
    fStartAngle, fEndAngle: TRealType;
    fDirection: TArcDirection;

    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
    procedure SetArcDirection(D: TArcDirection);
    procedure GetArcPoints2D(var P0, P1, P2, P3: TPoint2D);
    procedure GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect3D; override;
  public
    {: This constructor  creates a new arc of a 3D ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=XDir> is the X direction for the X axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=YDir> is the X direction for the Y axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=PlaneRef> is the plane reference point for the
        object. Because a planar outline has its control points
        forced to lie on the construction plane, the control
        points are referenced about this point.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that defines the arc's ellipse in the 2D plane
        coordinate system.>
       <LI=<I=SA> is the starting angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>
       <LI=<I=SA> is the ending angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>

       Note: Once created, the arc has four control points. The
       first two are <I=P1> and <I=P2>; the third is the point
       that lies on the segment from the center of the arc's ellipse
       and the starting point of the arc; the fourth is the point
       that lies on the segment from the center of the arc's ellipse and
       the ending point of the arc.
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const P1, P2: TPoint3D; SA, EA: TRealType);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the starting angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property StartAngle: TRealType read fStartAngle write SetStartAngle;
    {: This property contains the ending angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property EndAngle: TRealType read fEndAngle write SetEndAngle;
    {: This property contains the direction used to draw the arc.

       See <See Type=TArcDirection> for details.
    }
    property Direction: TArcDirection read fDirection write SetArcDirection;
  end;

  {: This class defines a 3D ellipse.

     The ellipse is defined by the two corner of the box that
     contains it.
  }
  TEllipse3D = class(TPlanarCurve3D)
  private
    procedure GetEllipsePoints2D(var P0, P1: TPoint2D);
    procedure GetEllipseParams(var CX, CY, RX, RY: TRealType);
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect3D; override;
  public
    {: This constructor creates a new ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=XDir> is the X direction for the X axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=YDir> is the X direction for the Y axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=PlaneRef> is the plane reference point for the
        object. Because a planar outline has its control points
        forced to lie on the construction plane, the control
        points are referenced about this point.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that contains the ellipse in the 2D plane coordinate
        system.>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const P1, P2: TPoint3D);
    procedure Assign(const Obj: TGraphicObject); override;
    function GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean; override;
  end;

  {: This class defines a 3D B-Spline curve.

     The B-Spline is defined by its control points.
     The order of the spline is 3 but you can change it.
  }
  TPlanarSpline3D = class(TPlanarCurve3D)
  private
    fOrder: Byte;
    procedure GetSplineCtrlPoints2D(const V: TPointsSet2D);
  protected
    function PopulateCurvePoints({%H-}N: Word): TRect3D; override;
  public
    {: This constructor creates a new 3D spline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=PlaneRef> is the plane reference point for the
        object. Because a planar outline has its control points
        forced to lie on the construction plane, the control
        points are referenced about this point.>
       <LI=<I=XDir> is the X direction for the X axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=YDir> is the X direction for the Y axis of the
        2D plane coordinate system of the shape.>
       <LI=<I=Pts> is an array that contains the control points
        of the spline. If you want to create a pointless spline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction. These points are in the 2D plane coordinate
        system>
    }
    constructor Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const Pts: array of TPoint3D);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the order of the spline.

       By default it is three (cubic spline).
    }
    property Order: Byte read FOrder write FOrder;
  end;

  {: This class defines a 3D mesh.

     A 3D mesh is a web of points that are connected to create a
     surface. The array of points forms a 2D regular grid with
     <See Property=TMesh3D@MDimension> on one axis (row axis) and
     <See Property=TMesh3D@NDimension> on the other axis
     (columns axis). The two axis of the grid need not to be
     ortogonal.

     Although the points of the regular grid are stored as
     <I=control points>, it is better to access them through the
     <See Property=TMesh3D@MeshPoints> property.
  }
  TMesh3D = class(TPrimitive3D)
  private
    fN, fM: Integer;
    function GetMeshPt(M, N: Integer): TPoint3D;
    procedure SetMeshPt(M, N: Integer; Pt: TPoint3D);
  public
    {: This method creates a new instance of the class:

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=M> is the number of rows of the mesh>
       <LI=<I=N> is the number of columns of the mesh>
       <LI=<I=Pts> contains the points of the mesh. This set of
        points must have <Code=M*N points>. The points are
        ordered for rows, and so the first points is the point
        at row 0 and column 0, then the points at row 0 and
        column 1 and so on.>
    }
    constructor Create(ID: LongInt; M, N: Integer; const Pts: array of TPoint3D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    function GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean; override;
    {: This property contains the points of the mesh.

       For instance <Code=MeshPoints[2, 5]> is the point in
       row 2 and column 5.
    }
    property MeshPoints[M, N: Integer]: TPoint3D read GetMeshPt write SetMeshPt;
    {: This property contains the number of columns of the mesh.
    }
    property NDimension: Integer read fN;
    {: This property contains the number of rows of the mesh.
    }
    property MDimension: Integer read fM;
  end;

  {: This class rapresent a <I=quad edge> used by the
     <See Class=TPolyface3D> class.

     You don't need to use this class directly.
  }
  TQuadEdge3D = class(TObject)
  private
    fV1, fV2, fV3, fV4: SmallInt;
    fFullQuad, fFullTri: Boolean;
    fPtsList: TPointsSet3D;
    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromStream(const Stream: TStream);
    procedure SetFace(const P1, P2, P3, P4: SmallInt);
    procedure DrawFace(const DrawMode: Integer; const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas);
    function OnFace(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                    var Distance: TRealType): Integer;
  public
    constructor Create(const Pts: TPointsSet3D; const P1, P2, P3, P4: SmallInt);

    property V1: SmallInt read fV1;
    property V2: SmallInt read fV2;
    property V3: SmallInt read fV3;
    property V4: SmallInt read fV4;
  end;

  {: This class rapresents a <I=3D polyface>.

     A <I=3D polyface> is a 3D surface made up from
     planar squares (quad edges) that shares one or more
     sides.

     The class may be used to create complex solids that have
     also holes in them.
  }
  TPolyface3D = class(TPrimitive3D)
  private
    fFaces: TIndexedObjectList;
    function GetFace(FI: Integer): TQuadEdge3D;
    procedure SetFace(FI: Integer; Q: TQuadEdge3D);
    function GetNFaces: Integer;
  public
    {: This constructors create a new polyface.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of points used by the
        faces.>
       <LI=<I=NFaces> is the number of faces that made up
        the surface.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        polyface. If you want to create a pointless polyface
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive3D@Points>.>
    }
    constructor Create(ID: LongInt; NPts, NFaces: Integer; const Pts: array of TPoint3D);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This method adds a face to the polyface.

       <I=P1>, <I=P2>, <I=P3> and <I=P4> are the indeces of
       the control points of the shape.
       Any of these point may assume the following special
       values:

       <LI=<I=0> if the face have less that four sides.
        In this case the side that use this point is not
        present. At least two indeces must be not zero.>
       <LI=<I=0> if the side of the face that connect the
        control point with its successor must not be drawed.>

       <I=FI> is the face index.
    }
    procedure AddFace(FI: Integer; P1, P2, P3, P4: Integer);
    {: This method deletes the specified face from the shape.

       <I=FI> is the face index to be deleted.
    }
    procedure DeleteFace(FI: Integer);
    function GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean; override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    {: This property contains the faces of the shape.

       <I=FI> is the face index.

       You must create the faces before asking for them
       by using the <See Method=TPolyface3D@AddFace>.
    }
    property Faces[FI: Integer]: TQuadEdge3D read GetFace write SetFace;
    {: This property contains the number of faces of the shape.
    }
    property NumOfFaces: Integer read GetNFaces;
  end;

  TPolyface3DClass = class of TPolyface3D;

  {: This class defines an abstract class used to set up a
     common interfaces for all kind of objects that are made
     from an transformed outline.

     For example a cilinder is made up from a circle extruded
     along its normal. This kind of objects can be created by
     deriving it from this class.

     The solid is made up from a <I=base profile> and a <I=polyface>
     that defines the solid's surface. You have only to define
     the transformations to be applied to the base profile to
     create the solid.
  }
  TSweepedOutline3D = class(TOutline3D)
  private
    fPolyface: TPolyface3D;
    fBaseOutline: TOutline3D;
    fFinalTransf: TTransf3D;
    fStartSweepDir, fEndSweepDir: TVector3D;
    procedure BaseOutlineChanged({%H-}O: TObject);
    function CreateSolidPolyface(PolyfaceClass: TPolyface3DClass): TRect3D;
  protected
    procedure _UpdateExtension; override;
    function  GetModelTransform: TTransf3D; override;
    procedure SetModelTransform(Transf: TTransf3D); override;
    function GetProfilePoints: TPointsSet3D; override;
    function GetNPts: Integer; override;
    function GetIsClosed: Boolean; override;
    {: This method must returns (in your implementations) the
       number of iterations needed to create the solid from the
       base profile.

       The solid will be created by transforming the base profile
       by your transformations. These transformation must be
       returned by the
       <See Method=TSweepedOutline3D@GetSweepTransform> method,
       that will be called a number of times returned by this
       method.

       <B=Note>: The total number of iterations will be one
       more than the returned number.
    }
    function GetSweepIterations: Integer; virtual; abstract;
    {: This method must returns the transformation to be
       applied to the base profile at the <I=NIter> iteration.
       <I=NIter> ranges from 0 to
       <See Method=TSweepedOutline3D@GetSweepIterations>.

       The solid will be created by transforming the base profile
       by the transformations returned by this method.
    }
    function GetSweepTransform(const NIter: Integer): TTransf3D; virtual; abstract;
    {: This method is called whenever the solid must be recomputed.
    }
    procedure UpdateSolid; dynamic;
    {: This property contains the last transformation applied to
       the base profile.
    }
    property FinalTransformation: TTransf3D read fFinalTransf;
  public
    {: This constructor creates a new <I=sweeped solid>.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=BaseProfile> is the base profile of the solid.>

       See the class description for details.
    }
    constructor Create(ID: LongInt; const BaseProfile: TOutline3D);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    function  HasTransform: Boolean; override;
    procedure ApplyTransform; override;
    procedure BeginUseProfilePoints; override;
    procedure EndUseProfilePoints; override;
    function GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean; override;
    procedure Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer); override;
    procedure DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D;
                                const Cnv: TDecorativeCanvas; const Width: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    {: This property contains the <I=base profile> of the solid.

       See the class description for details.
    }
    property BaseOutline: TOutline3D read fBaseOutline;
    {: This property contains the solid's surface created by the
       class.

       See the class description for details.
    }
    property Polyface: TPolyface3D read fPolyface;
  end;

  {: This class rapresents an extruded outline.

     This solid is made up from an outline that will be extruded
     along a direction by a given length.

     The profile points are extruded to form a series of lateral
     edges.
  }
  TExtrudedOutline3D = class(TSweepedOutline3D)
  private
    fExtrudeDir: TVector3D;
    fExtrudeLen: TRealType;
  protected
    function GetSweepIterations: Integer; override;
    function GetSweepTransform(const NIter: Integer): TTransf3D; override;
  public
    {: This constructor creates a new extruded outline.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=Outline> is the base profile of the solid.>
       <LI=<I=Len> is the len of extrusion.>
       <LI=<I=ExtDir> is the direction of extrusion in
        object model coordinates.>
    }
    constructor Create(ID: LongInt; const Outline: TOutline3D; Len: TRealType; const ExtDir: TVector3D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This property contains the extrusion direction of the
       solid.

       This direction is in object model coordinates.
    }
    property ExtrudeDir: TVector3D read fExtrudeDir write fExtrudeDir;
    {: This property contains the extrusion length of the solid.
    }
    property ExtrudeLen: TRealType read fExtrudeLen write fExtrudeLen;
  end;

  {: This class rapresents a rotational outline.

     This solid is made up from an outline that will be rotated
     along a direction by a given delta angle. Any time the
     outline is rotated, the profile points of the last rotated
     outline and the profile points of the currently rotated outline
     are connected to form lateral edges.
  }
  TRotationalOutline3D = class(TSweepedOutline3D)
  private
    fRotationCenter: TPoint3D;
    fRotationAx: TVector3D;
    fStartAngle, fEndAngle, fDeltaAngle: TRealType;
  protected
    function GetSweepIterations: Integer; override;
    function GetSweepTransform(const NIter: Integer): TTransf3D; override;
  public
    {: This constructor creates a new extruded outline.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=Outline> is the base profile of the solid.>
       <LI=<I=StartA> is the staring angle of rotation.>
       <LI=<I=EndA> is the ending angle of rotation>
       <LI=<I=DA> is the delta angle of rotation>
       <LI=<I=RotAx> is the axis of rotation in object model
        coordinates>
       <LI=<I=RotC> is the center of rotation in object model
        coordinates>
    }
    constructor Create(ID: LongInt; const Outline: TOutline3D; StartA, EndA, DA: TRealType; const RotAx: TVector3D; const RotC: TPoint3D);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Assign(const Obj: TGraphicObject); override;
    {: This property contains the center of rotation of the solid.

       This point is in object model coordinates.
    }
    property RotationCenter: TPoint3D read fRotationCenter write fRotationCenter;
    {: This property contains the axis of rotation of the solid.

       This axis is in object model coordinates.
    }
    property RotationAxis: TVector3D read fRotationAx write fRotationAx;
    {: This property contains the start angle (in radiants) of
       rotation of the solid.
    }
    property StartAngle: TRealType read fStartAngle write fStartAngle;
    {: This property contains the end angle (in radiants) of
       rotation of the solid.
    }
    property EndAngle: TRealType read fEndAngle write fEndAngle;
    {: This property contains the delta angle (in radiants) of
       rotation of the solid.
    }
    property DeltaAngle: TRealType read fDeltaAngle write fDeltaAngle;
  end;

  TCameraObject3D = class(TPrimitive3D)
  private
    fCameraPosition, fCameraViewPoint: TPoint3D;
    fCameraUP: TVector3D;
    fViewport: TCADViewport3D;
    fP1, fP2, fP3, fP4: TPoint3D;
    fAperture, fPlaneDistance, fAspect: TRealType;
    fUpdating: Boolean;
    procedure SetCameraPosition(P: TPoint3D);
    procedure SetCameraViewPoint(P: TPoint3D);
    procedure SetCameraUP(U: TVector3D);
    procedure SetAperture(A: TRealType);
    function GetAperture: TRealType;
    procedure SetAspect(A: TRealType);
    procedure SetPlaneDistance(D: TRealType);
    procedure SetViewport(V: TCADViewport3D);
    function GetCameraPlanePosition: TPoint3D;
    function UpdateViewFrustum: TRect3D;
  protected
    procedure _UpdateExtension; override;
  public
    constructor Create(ID: LongInt; const CamPos, CamView: TPoint3D; const CamUP: TVector3D; const Ap, PDist: TRealType);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const NormTransf: TTransf3D; const {%H-}VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const {%H-}DrawMode: Integer); override;
    function OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property CameraPosition: TPoint3D read fCameraPosition write SetCameraPosition;
    property CameraPlanePosition: TPoint3D read GetCameraPlanePosition;
    property CameraViewPoint: TPoint3D read fCameraViewPoint write SetCameraViewPoint;
    property CameraUP: TVector3D read fCameraUP write SetCameraUP;
    property Aperture: TRealType read GetAperture write SetAperture;
    property AspectRatio: TRealType read fAspect write SetAspect;
    property PlaneDistance: TRealType read fPlaneDistance write SetPlaneDistance;
    property LinkedViewport: TCADViewport3D read fViewport write SetViewport;
  end;

  procedure SetCamerasViewport(V: TCADPerspectiveViewport3D);

  {: This procedure sets the default font.
  }
  procedure CADSysSetDefaultFont(const Font: TVectFont);
  {: This function returns the default font.
  }
  function  CADSysGetDefaultFont: TVectFont;
  {: This function initializes the list of registered fonts.
  }
  procedure CADSysInitFontList;
  {: This function clears the list of registered fonts.
  }
  procedure CADSysClearFontList;
  {: This function returns the index of a font.

     If the font is not registered an exception will be raised.

     Parameters:

     <LI=<I=Font> is the font to be searched.>
  }
  function  CADSysFindFontIndex(const Font: TVectFont): Word;
  {: This function returns the font that was registered with
     the specified index.

     If the index is not used and a default font is defined,
     the function returns the default font. Otherwise an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the index of the font to be searched for.>
  }
  function  CADSysFindFontByIndex(Index: Word): TVectFont;
  {: This function register a font by retriving it from
     a file.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=FileName> is name of the file that contains the
      font.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
  procedure CADSysRegisterFontFromFile(Index: Word; const FileName: String);
  {: This function register a font.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=Font> is the font to be registered.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
  procedure CADSysRegisterFont(Index: Word; const Font: TVectFont);
  {: This function clear the registration of a font.

     Parameters:

     <LI=<I=Index> is the registration index.>
  }
  procedure CADSysUnregisterFont(Index: Word);

const
  {: This constats rapresent the maximum number of vectorial fonts
     that may be used in the library.
  }
  MAX_REGISTERED_FONTS = 512;

implementation

uses Math, Dialogs;

var
  VectFonts2DRegistered: array[0..MAX_REGISTERED_FONTS] of TVectFont;
  _NullChar: TVectChar;
  _DefaultFont: TVectFont;
  _DefaultHandler2D: TPrimitive2DHandler;
  _DefaultHandler3D: TPrimitive3DHandler;
  fCamerasViewports: TCADViewport3D;

// =====================================================================
// TExtendedFont
// =====================================================================

procedure TExtendedFont.SetHeight(Value: Word);
begin
  LogFont.lfHeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetHeight: Word;
begin
  Result := LogFont.lfHeight;
end;

procedure TExtendedFont.SetWidth(Value: Word);
begin
  LogFont.lfWidth := Value;
  SetNewValue;
end;

function TExtendedFont.GetWidth: Word;
begin
  Result := LogFont.lfWidth;
end;

procedure TExtendedFont.SetEscapement(Value: Word);
begin
  LogFont.lfEscapement := Value;
  SetNewValue;
end;

function TExtendedFont.GetEscapement: Word;
begin
  Result := LogFont.lfEscapement;
end;

procedure TExtendedFont.SetWeight(Value: Word);
begin
  LogFont.lfWeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetWeight: Word;
begin
  Result := LogFont.lfWeight;
end;

procedure TExtendedFont.SetItalic(Value: Byte);
begin
  LogFont.lfItalic := Value;
  SetNewValue;
end;

function TExtendedFont.GetItalic: Byte;
begin
  Result := LogFont.lfItalic;
end;

procedure TExtendedFont.SetUnderline(Value: Byte);
begin
  LogFont.lfUnderline := Value;
  SetNewValue;
end;

function TExtendedFont.GetUnderline: Byte;
begin
  Result := LogFont.lfUnderline;
end;

procedure TExtendedFont.SetStrikeOut(Value: Byte);
begin
  LogFont.lfStrikeOut := Value;
  SetNewValue;
end;

function TExtendedFont.GetStrikeOut: Byte;
begin
  Result := LogFont.lfStrikeOut;
end;

procedure TExtendedFont.SetCharSet(Value: Byte);
begin
  LogFont.lfCharSet := Value;
  SetNewValue;
end;

function TExtendedFont.GetCharSet: Byte;
begin
  Result := LogFont.lfCharSet;
end;

procedure TExtendedFont.SetOutPrecision(Value: Byte);
begin
  LogFont.lfOutPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetOutPrecision: Byte;
begin
  Result := LogFont.lfOutPrecision;
end;

procedure TExtendedFont.SetClipPrecision(Value: Byte);
begin
  LogFont.lfClipPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetClipPrecision: Byte;
begin
  Result := LogFont.lfClipPrecision;
end;

procedure TExtendedFont.SetQuality(Value: Byte);
begin
  LogFont.lfQuality := Value;
  SetNewValue;
end;

function TExtendedFont.GetQuality: Byte;
begin
  Result := LogFont.lfQuality;
end;

procedure TExtendedFont.SetPicthAndFamily(Value: Byte);
begin
  LogFont.lfPitchAndFamily := Value;
  SetNewValue;
end;

function TExtendedFont.GetPicthAndFamily: Byte;
begin
  Result := LogFont.lfPitchAndFamily;
end;

procedure TExtendedFont.SetFaceName(Value: TFaceName);
var
  Cont: Byte;
begin
  for Cont := 1 to Length(Value) do
   LogFont.lfFaceName[Cont - 1] := Value[Cont];
  LogFont.lfFaceName[Length(Value)] := #0;
  SetNewValue;
end;

function TExtendedFont.GetFaceName: TFaceName;
begin
  Result := LogFont.lfFaceName;
end;

procedure TExtendedFont.SetNewValue;
var
  TmpHandle: HFONT;
begin
  TmpHandle := CreateFontIndirect(LogFont);
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, TmpHandle);
  DeleteObject(FHandle);
  FHandle := TmpHandle;
end;

procedure TExtendedFont.SetCanvas(Cnv: TCanvas);
begin
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, FCanvas.Font.Handle);
  FCanvas := Cnv;
  if Assigned(FCanvas) then SelectObject(FCanvas.Handle, FHandle);
end;

constructor TExtendedFont.Create;
begin
  inherited Create;
  GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(LogFont), @LogFont);
  LogFont.lfFaceName := 'Small Font';
  FHandle := CreateFontIndirect(LogFont);
end;

procedure TExtendedFont.Assign(Obj: TExtendedFont);
begin
  if Obj = Self then
   Exit;
  LogFont := TExtendedFont(Obj).LogFont;
  SetNewValue;
end;

destructor TExtendedFont.Destroy;
begin
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, FCanvas.Font.Handle);
  DeleteObject(FHandle);
  inherited Destroy;
end;

procedure TExtendedFont.SaveToStream(Strm: TStream);
begin
  with Strm do
   Write(LogFont, SizeOf(LogFont));
end;

procedure TExtendedFont.LoadFromStream(Strm: TStream);
begin
  with Strm do
   begin
     Read(LogFont, SizeOf(LogFont));
     SetNewValue;
   end;
end;

// =====================================================================
// TPrimitive2DHandler
// =====================================================================

procedure TPrimitive2DHandler.DrawControlPoints(const Sender: TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer);
var
  TmpPt: TPoint2D;
  Cont: Integer;
begin
  if Sender is TPrimitive2D then
   with TPrimitive2D(Sender) do
    if not HasTransform then
     for Cont := 0 to Points.Count - 1 do
      begin
        TmpPt := TransformPoint2D(Points[Cont], VT);
        DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y), Width);
      end
    else
     for Cont := 0 to Points.Count - 1 do
      begin
        TmpPt := TransformPoint2D(Points[Cont], MultiplyTransform2D(ModelTransform, VT));
        DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y), Width);
      end;
end;

function TPrimitive2DHandler.OnMe(const Sender: TObject2D; Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  Cont: Integer;
  ResDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  if Sender is TPrimitive2D then
   with TPrimitive2D(Sender) do
    if HasTransform then
     begin
       for Cont := 0 to Points.Count - 1 do
        if NearPoint2D(Pt, TransformPoint2D(Points[Cont], ModelTransform), Aperture, {%H-}ResDist) and
           (ResDist <= Distance) then
         begin
           Result := Cont;
           Distance := ResDist;
         end;
     end
    else
     begin
       for Cont := 0 to Points.Count - 1 do
        if NearPoint2D(Pt, Points[Cont], Aperture, ResDist) and
           (ResDist <= Distance) then
         begin
           Result := Cont;
           Distance := ResDist;
         end;
     end;
end;

// =====================================================================
// TPrimitive2D
// =====================================================================

procedure TPrimitive2D._UpdateExtension;
begin
  if not Assigned(fPoints) or (fPoints.Count = 0) then
   WritableBox := Rect2D(0, 0, 0, 0)
  else
   { Change the extension. }
   WritableBox := TransformBoundingBox2D(fPoints.Extension, ModelTransform);
end;

function TPrimitive2D.CreateVect(const Size: Integer): TPointsSet2D;
begin
  Result := TPointsSet2D.Create(Size);
end;

constructor TPrimitive2D.Create(ID: LongInt; NPts: Integer);
begin
  inherited Create(ID);
  { Create the internal vector. }
  fPoints := CreateVect(NPts);
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
end;

procedure TPrimitive2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive2D) then
   begin // Per default non aggiunge i punti.
     if not Assigned(fPoints) then
      begin
        fPoints := CreateVect(0);
        fPoints.GrowingEnabled := True;
        fPoints.OnChange := UpdateExtension;
      end;
     fPoints.Clear;
   end;
end;

constructor TPrimitive2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
  TmpPtSingle: TPoint2DSingle;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     TmpWord := 0;
     Read(TmpWord, SizeOf(TmpWord));
     fPoints := CreateVect(TmpWord);
     { Read all the points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        TmpPt.X := 0; TmpPt.Y := 0;
        if( Version >= 'CAD423' ) then
         Read(TmpPt, SizeOf(TmpPt))
        else
        begin
          Read(TmpPtSingle, SizeOf(TmpPtSingle));
          TmpPt.X := TmpPtSingle.X; TmpPt.Y := TmpPtSingle.Y; TmpPt.W := TmpPtSingle.W;
        end;
        fPoints.Points[Cont] := TmpPt;
      end;
     TmpBoolean := False;
     Read(TmpBoolean, SizeOf(TmpBoolean));
     fPoints.GrowingEnabled := TmpBoolean;
   end;
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
end;

procedure TPrimitive2D.SaveToStream(const Stream: TStream);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     TmpWord := fPoints.Count;
     Write(TmpWord, SizeOf(TmpWord));
     { Write all points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        TmpPt := fPoints.Points[Cont];
        Write(TmpPt, SizeOf(TmpPt));
      end;
     TmpBoolean := fPoints.GrowingEnabled;
     Write(TmpBoolean, SizeOf(TmpBoolean));
   end;
end;

destructor TPrimitive2D.Destroy;
begin
  if Assigned(fPoints) then
   fPoints.Free;
  inherited Destroy;
end;

// =====================================================================
// TLine2D
// =====================================================================

constructor TLine2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TLine2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

procedure TLine2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  if not HasTransform then
   DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, VT)
  else
   DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, MultiplyTransform2D(ModelTransform, VT));
end;

function TLine2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                      var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(Points.PointsReference, Points.Count, Pt, {%H-}TmpDist, Aperture, ModelTransform, False)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

// =====================================================================
// TOutline2D
// =====================================================================

function TOutline2D.GetIsClosed: Boolean;
begin
  BeginUseProfilePoints;
  try
   Result := (ProfilePoints.Count > 2) and
              IsSamePoint2D(ProfilePoints[0], ProfilePoints[ProfilePoints.Count - 1]);
  finally
   EndUseProfilePoints;
  end;
end;

procedure TOutline2D.BeginUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do initialization here.
end;

procedure TOutline2D.EndUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do finalization here.
end;

// =====================================================================
// TPolyline2D
// =====================================================================

constructor TPolyline2D.Create(ID: LongInt; const Pts: array of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1);
  Points.AddPoints(Pts);
end;

procedure TPolyLine2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine2D) or (Obj is TPolyline2D) or (Obj is TPolygon2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, TPrimitive2D(Obj).Points.Count - 1);
     Points.GrowingEnabled := True;
   end;
end;

procedure TPolyLine2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  if not HasTransform then
   Points.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
  else
   Points.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
end;

function TPolyline2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                          var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(Points.PointsReference, Points.Count, Pt, {%H-}TmpDist, Aperture, ModelTransform, False)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

function TPolyline2D.GetProfilePoints: TPointsSet2D;
begin
  Result := Points;
end;

function TPolyline2D.GetNPts: Integer;
begin
  Result := Points.Count;
end;

// =====================================================================
// TPolygon2D
// =====================================================================

function TPolygon2D.GetIsClosed: Boolean;
begin
  Result := True; // Sempre chiuso.
end;

procedure TPolygon2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  { Draw the polygon. }
  if not HasTransform then
   Points.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
  else
   Points.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT))
end;

function TPolygon2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                         var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if (Result = PICK_INBBOX) then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(Points.PointsReference, Points.Count, Pt, {%H-}TmpDist, Aperture, ModelTransform)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

// =====================================================================
// TCurve2D
// =====================================================================

procedure TCurve2D.SetPrimitiveSavingType(S: TPrimitiveSavingType);
begin
  if S <> fSavingType then
   begin
     fSavingType := S;
     UpdateExtension(Self);
   end;
end;

procedure TCurve2D.SetCurvePrecision(N: Word);
begin
  if fCurvePrecision <> N then
   fCurvePrecision := N;
end;

function TCurve2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if not Assigned(fCurvePoints) then
   fCurvePoints := TPointsSet2D.Create(N)
  else
   fCurvePoints.Clear;
  Inc(fCountReference);
  fCurvePoints.GrowingEnabled := True;
  Result := Rect2D(0, 0, 0, 0);
end;

procedure TCurve2D.FreeCurvePoints;
begin
  Dec(fCountReference);
  if fCountReference <= 0 then
   begin
     fCurvePoints.Free;
     fCurvePoints := nil;
     fCountReference := 0;
   end;
end;

constructor TCurve2D.Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
begin
  inherited Create(ID, NPts);

  fCurvePrecision := CurvePrec;
  fCurvePoints := nil;
  fCountReference := 0;
  fSavingType := stTime;
end;

destructor TCurve2D.Destroy;
begin
  fCountReference := 0;
  FreeCurvePoints;
  inherited;
end;

procedure TCurve2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TCurve2D then
   begin
     CurvePrecision := TCurve2D(Obj).fCurvePrecision;
     SavingType := TCurve2D(Obj).fSavingType;
   end;
end;

constructor TCurve2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  with Stream do
   begin
     Read(fCurvePrecision, SizeOf(fCurvePrecision));
     Read(fSavingType, SizeOf(fSavingType));
     fCountReference := 0;
   end;
end;

procedure TCurve2D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fCurvePrecision, SizeOf(fCurvePrecision));
     Write(fSavingType, SizeOf(fSavingType));
   end;
end;

procedure TCurve2D._UpdateExtension;
begin
  if not Assigned(Points) or (Points.Count = 0) then
   Exit;
  case fSavingType of
   stSpace: begin
     WritableBox := PopulateCurvePoints(0);
     FreeCurvePoints;
   end;
   stTime: begin
     FreeCurvePoints;
     WritableBox := PopulateCurvePoints(0);
   end;
  end;
end;

procedure TCurve2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
    if Assigned(fCurvePoints) then
     begin
      if not HasTransform then
       fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
      else
       fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
     end;
  finally
    EndUseProfilePoints;
  end;
end;

function TCurve2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                       var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     BeginUseProfilePoints;
     try
       if not Assigned(fCurvePoints) then
        Exit;
       Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(fCurvePoints.PointsReference, fCurvePoints.Count, Pt, {%H-}TmpDist, Aperture, ModelTransform, False)]);
       Distance := {%H-}MinValue([Aperture, TmpDist]);
     finally
       EndUseProfilePoints;
     end;
   end;
end;

function TCurve2D.GetProfilePoints: TPointsSet2D;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints;
end;

function TCurve2D.GetNPts: Integer;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints.Count;
end;

procedure TCurve2D.BeginUseProfilePoints;
begin
  if fSavingType = stSpace then
   WritableBox := PopulateCurvePoints(0);
end;

procedure TCurve2D.EndUseProfilePoints;
begin
  if fSavingType = stSpace then
   FreeCurvePoints;
end;

// =====================================================================
// TArc2D
// =====================================================================

procedure TArc2D.GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
var
  P1, P0: TPoint2D;
begin
  P0:= CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  RX := Abs(P1.X - P0.X) / 2.0;
  RY := Abs(P1.Y - P0.Y) / 2.0;
  if Points.Count < 3 then
   Exit;
  P0:= CartesianPoint2D(Points[2]);
  P1 := CartesianPoint2D(Points[3]);
  case FDirection of
   adClockwise: begin
     SA := ArcTan2(CY - P0.Y, P0.X - CX);
     EA := ArcTan2(CY - P1.Y, P1.X - CX);
   end;
   adCounterClockwise: begin
     SA := ArcTan2(P0.Y - CY, P0.X - CX);
     EA := ArcTan2(P1.Y - CY, P1.X - CX);
   end;
  end;
end;

procedure TArc2D.SetStartAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fStartAngle <> A then
   begin
     fStartAngle := A;
     CX := 0; CY := 0; RX := 0; RY := 0; SA := 0; EA := 0;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[2] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TArc2D.SetEndAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fEndAngle <> A then
   begin
     fEndAngle := A;
     CX := 0; CY := 0; RX := 0; RY := 0; SA := 0; EA := 0;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[3] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TArc2D.SetArcDirection(D: TArcDirection);
begin
  if D <> FDirection then
   begin
     FDirection := D;
     UpdateExtension(Self);
   end;
end;

function TArc2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont, NPts: Integer;
  CX, RX, CY, RY: TRealType;
  Delta, CurrAngle: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  CX := 0; CY := 0; RX := 0; RY := 0;
  GetArcParams(CX, CY, RX, RY, fStartAngle, fEndAngle);
  // Calcola il numero di punti effetivi nella curva
  NPts := CurvePrecision;
  // Calcola il delta angolare tra due punti
  if fStartAngle < fEndAngle then
   Delta := (fEndAngle - fStartAngle) / (NPts + 1)
  else
   Delta := (TWOPI - fStartAngle + fEndAngle) / (NPts + 1);
  // Crea il vettore curvilineo.
  inherited PopulateCurvePoints(NPts + 1);
  // Popola il vettore curvilineo.
  if fDirection = adClockwise then
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts - 1 do
      begin
        ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle)));
        CurrAngle := CurrAngle + Delta
      end;
     ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY - RY * Sin(fEndAngle)));
   end
  else
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts - 1 do
      begin
        ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY + RY * Sin(CurrAngle)));
        CurrAngle := CurrAngle + Delta
      end;
     ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY + RY * Sin(fEndAngle)));
   end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

{ Angles are in radiants. }
constructor TArc2D.Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
begin
  inherited Create(ID, 4, 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.Add(Point2D(0, 0));
    Points.Add(Point2D(0, 0));
    fDirection := adClockwise;
    StartAngle := SA;
    EndAngle := EA;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TArc2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
       Points.Add(Point2D(0, 0));
       Points.Add(Point2D(0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TArc2D then
   begin
     fStartAngle := (Obj as TArc2D).StartAngle;
     fEndAngle := (Obj as TArc2D).EndAngle;
     fDirection := (Obj as TArc2D).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end;
end;

constructor TArc2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  Points.DisableEvents := True;
  with Stream do
   try
     Read(FDirection, SizeOf(FDirection));
   finally
     Points.DisableEvents := False;
   end;
end;

procedure TArc2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;

// =====================================================================
// TFrame2D
// =====================================================================

function TFrame2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);

  ProfilePoints.Add(Points[0]);
  ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
  ProfilePoints.Add(Points[1]);
  ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
  ProfilePoints.Add(Points[0]);
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

constructor TFrame2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2, 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TFrame2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TFrame2D) or (Obj is TEllipse2D) or (Obj is TArc2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TRectangle2D
// =====================================================================

procedure TRectangle2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
   if not HasTransform then
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
   else
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
  finally
   EndUseProfilePoints;
  end;
end;

function TRectangle2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                           var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     { Consider all segments in the arc. }
     BeginUseProfilePoints;
     try
       TmpDist := 0;
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(ProfilePoints.PointsReference, ProfilePoints.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := {%H-}MinValue([Aperture, TmpDist]);
     finally
      EndUseProfilePoints;
     end;
   end;
end;

// =====================================================================
// TEllipse2D
// =====================================================================

procedure TEllipse2D.GetEllipseParams(var CX, CY, RX, RY: TRealType);
var
  P1, P0: TPoint2D;
begin
  P0:= CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  RX := Abs(P1.X - P0.X) / 2.0 ;
  RY := Abs(P1.Y - P0.Y) / 2.0;
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
end;

function TEllipse2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
  Delta, CurrAngle, CX, RX, CY, RY: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);
  CX := 0; CY := 0; RX := 0; RY := 0;
  GetEllipseParams(CX, CY, RX, RY);
  Delta := TWOPI / CurvePrecision;
  ProfilePoints.Add(Point2D(CX + RX, CY));
  CurrAngle := Delta;
  for Cont := 1 to CurvePrecision - 1 do
   begin
     ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle)));
     CurrAngle := CurrAngle + Delta
   end;
  ProfilePoints.Add(Point2D(CX + RX, CY));
  Result := TransformBoundingBox2D(Points.Extension, ModelTransform);
end;

constructor TEllipse2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2, 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TEllipse2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) or (Obj is TArc2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TFilledEllipse2D
// =====================================================================

procedure TFilledEllipse2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
   if not HasTransform then
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
   else
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
  finally
   EndUseProfilePoints;
  end;
end;

function TFilledEllipse2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                     var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     { Consider all segments in the arc. }
     BeginUseProfilePoints;
     try
       TmpDist := 0;
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(ProfilePoints.PointsReference, ProfilePoints.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := {%H-}MinValue([Aperture, TmpDist]);
     finally
      EndUseProfilePoints;
     end;
   end;
end;

// =====================================================================
// TBSpline2D
// =====================================================================

function Knot(I, K, N: Integer): Integer;
begin
  if I < K then
   Result := 0
  else if I > N then
   Result := N - K + 2
  else
   Result := I - K + 1;
end;

function NBlend(I, K, OK, N: Integer; U: TRealType): TRealType;
var
  T: Integer;
  V: TRealType;
begin
  if K = 1 then
   begin
     V := 0;
     if (Knot(i, OK, N) <= u) and (u < Knot(i + 1, OK, N)) then
      V := 1;
   end
  else
   begin
     V := 0;
     T := Knot(I + K - 1, OK, N) - Knot(I, OK, N);
     if T <> 0 then
      V := (U - Knot(I, OK, N)) * NBlend(I, K -1, OK, N, U) / T;
     T := Knot(I + K, OK, N) - Knot(I + 1, OK, N);
     if T <> 0 then
      V := V + (Knot(I + K, OK, N) - U) * NBlend(I + 1, K - 1, OK, N, U) / T;
   end;
  Result := V;
end;

function BSpline2D(U: TRealType; N, K: Integer; const Points: TPointsSet2D): TPoint2D;
var
  I: Integer;
  B: TRealType;
  TmpPt: TPoint2D;
begin
  Result := Point2D(0.0, 0.0);
  for I := 0 to N do
   begin
     B := NBlend(I, K, K, N, U);
     TmpPt := CartesianPoint2D(Points[I]);
     Result.X := Result.X + TmpPt.X * B;
     Result.Y := Result.Y + TmpPt.Y * B;
     Result.W := 1.0;
   end;
end;

function TBSpline2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  if Points.Count < FOrder then
   begin
     inherited PopulateCurvePoints(Points.Count);
     ProfilePoints.Copy(Points, 0, Points.Count - 1);
   end
  else
   begin
     inherited PopulateCurvePoints(CurvePrecision + 1);
     for Cont := 0 to CurvePrecision - 1 do
      ProfilePoints.Add(BSpline2D(Cont / CurvePrecision * (Points.Count - 2),
                                Points.Count - 1, FOrder, Points));
     ProfilePoints.Add(Points[Points.Count - 1]);
   end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

constructor TBSpline2D.Create(ID: LongInt; const Pts: array of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, 50);
  fOrder := 3; { The default spline is cubic. }
  Points.AddPoints(Pts);
  Points.GrowingEnabled := True;
end;

constructor TBSpline2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  with Stream do
   { Load the particular properties. }
   Read(fOrder, SizeOf(fOrder));
end;

procedure TBSpline2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fOrder, SizeOf(fOrder));
end;

procedure TBSpline2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TBSpline2D) then
   begin
     fOrder := TBSpline2D(Obj).fOrder;
     Points.Copy(TPrimitive2D(Obj).Points, 0, TPrimitive2D(Obj).Points.Count - 1);
     Points.GrowingEnabled := True;
   end;
end;

// =====================================================================
// TText2D
// =====================================================================

constructor TText2D.Create(ID: LongInt; Rect1: TRect2D; Height: TRealType;
                           Txt: AnsiString);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    fHeight := Height;
    fText := Txt;
    fRecalcBox := False;
    fDrawBox := False;
    fExtFont := TExtendedFont.Create;
    WritableBox := Rect1;
    fClippingFlags := 0;
    Points.Add(Rect1.FirstEdge);
    Points.Add(Rect1.SecondEdge);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

destructor TText2D.Destroy;
begin
  fExtFont.Free;
  inherited Destroy;
end;

procedure TText2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var
  TmpBox: TRect2D;
  TmpHeight: Integer;
  TmpRect: TRect;
  TmpTransf: TTransf2D;
begin
  { Find the correct size. }
  TmpBox.FirstEdge := Point2D(0, 0);
  TmpBox.SecondEdge := Point2D(0, fHeight);
  TmpBox := TransformRect2D(TmpBox, VT);
  TmpHeight := Round(PointDistance2D(TmpBox.FirstEdge, TmpBox.SecondEdge));
  if TmpHeight <= 0 then
   Exit;
  { Build up the DrawText rect. }
  TmpRect := Rect2DToRect(TransformRect2D(Box, VT));
  FExtFont.Canvas := Cnv.Canvas;
  try
    FExtFont.Height := TmpHeight;
    if fRecalcBox then
     begin
       LCLIntf.DrawText(Cnv.Canvas.Handle, PChar(FText), Length(FText), TmpRect, DT_CALCRECT);
       if not HasTransform then
        TmpTransf := VT
       else
        TmpTransf := MultiplyTransform2D(ModelTransform, VT);
       TmpBox := TransformBoundingBox2D(RectToRect2D(TmpRect), InvertTransform2D(TmpTransf));
       Points.DisableEvents := True;
       try
         Points[0] := TmpBox.FirstEdge;
         Points[1] := TmpBox.SecondEdge;
       finally
         Points.DisableEvents := False;
         UpdateExtension(Self);
       end;
     end;
    if (Cnv.Canvas.Pen.Mode <> pmXor) then
     LCLIntf.DrawText(Cnv.Canvas.Handle, PChar(FText), Length(FText), TmpRect, FClippingFlags);
    if FDrawBox or (Cnv.Canvas.Pen.Mode = pmXor) then
     DrawRect2DAsPolyline(Cnv, Box, RectToRect2D(Cnv.Canvas.ClipRect), IdentityTransf2D, VT);
  finally
    FExtFont.Canvas := nil;
  end;
end;

function TText2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   Result := PICK_INOBJECT;
end;

constructor TText2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
  TmpS: TRealTypeSingle;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     TmpInt := 0;
     Read(TmpInt, SizeOf(TmpInt));
     SetString(FText, nil, TmpInt);
     Read(Pointer(FText)^, TmpInt);
     FExtFont := TExtendedFont.Create;
     FExtFont.LoadFromStream(Stream);
     Read(FClippingFlags, SizeOf(FClippingFlags));
     Read(FDrawBox, SizeOf(FDrawBox));
     if( Version >= 'CAD423' ) then
      begin
        Read(fHeight, SizeOf(fHeight));
      end
     else
      begin
        Read({%H-}TmpS, SizeOf(TmpS));
        fHeight := TmpS;
      end;
   end;
end;

procedure TText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     TmpInt := Length(FText);
     Write(TmpInt, SizeOf(TmpInt));
     Write(Pointer(FText)^, Length(FText));
     FExtFont.SaveToStream(Stream);
     Write(FClippingFlags, SizeOf(FClippingFlags));
     Write(FDrawBox, SizeOf(FDrawBox));
     Write(fHeight, SizeOf(fHeight));
   end;
end;

procedure TText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TText2D) or (Obj is TFrame2D) then
   begin
     if Obj is TText2D then
      begin
        fText := (Obj as TText2D).Text;
        fHeight := (Obj as TText2D).Height;
        fDrawBox := (Obj as TText2D).DrawBox;
        fRecalcBox := (Obj as TText2D).fRecalcBox;
        fClippingFlags := (Obj as TText2D).ClippingFlags;
        if not Assigned(FExtFont) then
         FExtFont := TExtendedFont.Create;
        fExtFont.Assign(TText2D(Obj).fExtFont);
      end;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TBitmap2D
// =====================================================================

procedure TBitmap2D.SetScaleFactor(SF: TRealType);
var
  TmpPt: TPoint2D;
begin
  if( fScaleFactor <> SF ) then
   begin
     fScaleFactor := SF;
     if( fScaleFactor <> 0.0 ) then
      begin
        if( fAspectRatio <> 0.0 ) then
         TmpPt.X := Points[0].X + fBitmap.Height * fScaleFactor / fAspectRatio
        else
         TmpPt.X := Points[0].X + fBitmap.Width * fScaleFactor;
        TmpPt.Y := Points[0].Y + fBitmap.Height * fScaleFactor;
        TmpPt.W := 1.0;
        Points[1] := TmpPt;
      end;
   end;
end;

procedure TBitmap2D.SetAspectRatio(AR: TRealType);
var
  TmpPt: TPoint2D;
begin
  if( fAspectRatio <> AR ) then
   begin
     fAspectRatio := AR;
     if( fScaleFactor <> 0.0 ) then
      begin
        if( fAspectRatio <> 0.0 ) then
         TmpPt.X := Points[0].X + fBitmap.Height * fScaleFactor / fAspectRatio
        else
         TmpPt.X := Points[0].X + fBitmap.Width * fScaleFactor;
        TmpPt.Y := Points[0].Y + fBitmap.Height * fScaleFactor;
        TmpPt.W := 1.0;
        Points[1] := TmpPt;
      end;
   end;
end;

constructor TBitmap2D.Create(ID: LongInt; const P1, P2: TPoint2D; Bmp: TBitmap);
begin
  inherited Create(ID, 2);
  fScaleFactor := 0.0;
  fAspectRatio := 0.0;
  fCopyMode := cmSrcCopy;
  Points.DisableEvents := True;
  try
    fBitmap := TBitmap.Create;
    fBitmap.Assign(Bmp);
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TBitmap2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if (Obj is TBitmap2D) or (Obj is TFrame2D) then
   begin
     fScaleFactor := TBitmap2D(Obj).ScaleFactor;
     fAspectRatio := TBitmap2D(Obj).AspectRatio;
     fCopyMode := TBitmap2D(Obj).CopyMode;
     if Obj is TBitmap2D then
      fBitmap.Assign(TBitmap2D(Obj).fBitmap);
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := True;
   end;
end;

destructor TBitmap2D.Destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;

constructor TBitmap2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  tmpSingle: TRealTypeSingle;
begin
  { Load the standard properties }
  inherited;
  fBitmap := TBitmap.Create;
  fBitmap.LoadFromStream(Stream);
  if( Version >= 'CAD423' ) then
   begin
     Stream.Read(fScaleFactor, SizeOf(fScaleFactor));
     Stream.Read(fAspectRatio, SizeOf(fAspectRatio));
     Stream.Read(fCopyMode, SizeOf(fCopyMode));
   end
  else if( Version >= 'CAD421' ) then
   begin
     Stream.Read({%H-}tmpSingle, SizeOf(tmpSingle));
     fScaleFactor := tmpSingle;
     Stream.Read(tmpSingle, SizeOf(tmpSingle));
     fAspectRatio := tmpSingle;
     Stream.Read(fCopyMode, SizeOf(fCopyMode));
   end;
end;

procedure TBitmap2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  fBitmap.SaveToStream(Stream);
  Stream.Write(fScaleFactor, SizeOf(fScaleFactor));
  Stream.Write(fAspectRatio, SizeOf(fAspectRatio));
  Stream.Write(fCopyMode, SizeOf(fCopyMode));
end;

procedure TBitmap2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var
  TmpPt1, TmpPt2: TPoint2D;
  TmpTransf: TTransf2D;
  TmpRect: TRect;
  OldMode: TCopyMode;
begin
  if not HasTransform then
   TmpTransf := VT
  else
   TmpTransf := MultiplyTransform2D(ModelTransform, VT);
  TmpPt1 := TransformPoint2D(Points[0], TmpTransf);
  TmpPt2 := TransformPoint2D(Points[1], TmpTransf);
  TmpRect := Rect2DToRect(Rect2D(TmpPt1.X, TmpPt1.Y, TmpPt2.X, TmpPt2.Y));
  OldMode := Cnv.Canvas.CopyMode;
  Cnv.Canvas.CopyMode := fCopyMode;
  Cnv.Canvas.StretchDraw(TmpRect, fBitmap);
  Cnv.Canvas.CopyMode := OldMode;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectChar.GetVect(Idx: Integer): TPointsSet2D;
begin
  Result := TPointsSet2D(fSubVects[Idx]);
end;

function TVectChar.GetVectCount: Integer;
begin
  Result := fSubVects.NumberOfObjects;
end;

constructor TVectChar.Create(NSubVect: Integer);
var
  Cont: Integer;
begin
  inherited Create;

  fSubVects := TIndexedObjectList.Create(NSubVect);
  fSubVects.FreeOnClear := True;
  for Cont := 0 to NSubVect - 1 do
   fSubVects.Objects[Cont] := TPointsSet2D.Create(0);
end;

destructor TVectChar.Destroy;
begin
  fSubVects.Free;

  inherited;
end;

procedure TVectChar.UpdateExtension;
var
  Cont: Integer;
begin
  fExtension := Rect2D(0.0, 0.0, 0.0, 0.0);
  for Cont := 0 to fSubVects.NumberOfObjects - 1 do
   fExtension := BoxOutBox2D(fExtension, TPointsSet2D(fSubVects.Objects[Cont]).Extension);
end;

constructor TVectChar.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt, Cont: Integer;
  TmpWord: Word;
  TmpPt: TPoint2D;
  TmpPtS: TPoint2DSingle;
begin
  inherited Create;
  if( Version >= 'CAD4  ' ) then
   with Stream do
    begin
      TmpInt := 0;
      Read(TmpInt, SizeOf(TmpInt));
      fSubVects := TIndexedObjectList.Create(TmpInt);
      // Lettura vettori.
      for Cont := 0 to fSubVects.NumberOfObjects - 1 do
       begin
         TmpWord := 0;
         Read(TmpWord, SizeOf(TmpWord));
         fSubVects.Objects[Cont] := TPointsSet2D.Create(TmpWord);
         while TmpWord > 0 do
          begin
            if( Version >= 'CAD423' ) then
             begin
               TmpPt.X := 0; TmpPt.Y := 0;
               Read(TmpPt, SizeOf(TmpPt));
             end
            else
             begin
               TmpPtS.X := 0; TmpPtS.Y := 0;
               Read(TmpPtS, SizeOf(TmpPtS));
               TmpPt.X := TmpPtS.X;
               TmpPt.Y := TmpPtS.Y;
               TmpPt.W := TmpPtS.W;
             end;
            TPointsSet2D(fSubVects.Objects[Cont]).Add(TmpPt);
            Dec(TmpWord);
          end;
       end;
      UpdateExtension(Self);
    end;
end;

procedure TVectChar.SaveToStream(const Stream: TStream);
var
  TmpInt, Cont: Integer;
  TmpWord: Word;
  TmpPt: TPoint2D;
begin
  with Stream do
   begin
     TmpInt := fSubVects.NumberOfObjects;
     Write(TmpInt, SizeOf(TmpInt));
     // Scrittura vettori.
     for Cont := 0 to fSubVects.NumberOfObjects - 1 do
      begin
        TmpWord := TPointsSet2D(fSubVects.Objects[Cont]).Count;
        Write(TmpWord, SizeOf(TmpWord));
        while TmpWord > 0 do
         begin
           TmpPt := TPointsSet2D(fSubVects.Objects[Cont]).Points[TmpWord - 1];
           Write(TmpPt, SizeOf(TmpPt));
           Dec(TmpWord);
         end;
      end;
   end;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectFont.GetChar(Ch: Char): TVectChar;
begin
  Result := TVectChar(fVects[Ord(Ch)]);
end;

constructor TVectFont.Create;
begin
  inherited;

  // Al massimo ci sono 256 caratteri.
  fVects := TIndexedObjectList.Create(256);
  fVects.FreeOnClear := True;
end;

destructor TVectFont.Destroy;
begin
  fVects.Free;

  inherited;
end;

constructor TVectFont.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  inherited Create;
  with Stream do
   begin
     fVects := TIndexedObjectList.Create(256);
     // Lettura caratteri.
     while True do
      begin
        // Lettura numero carattere.
        TmpInt := 0;
        Read(TmpInt, SizeOf(TmpInt));
        if TmpInt = -1 then
         Break; // Fine.
        fVects[TmpInt] := TVectChar.CreateFromStream(Stream, Version);
      end;
   end;
end;

procedure TVectFont.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  with Stream do
   begin
     // Scrittura caratteri.
     for TmpInt := 0 to fVects.NumberOfObjects - 1 do
      if fVects[TmpInt] <> nil then
       begin
         // Scrittura numero carattere.
         Write(TmpInt, SizeOf(TmpInt));
         TVectChar(fVects[TmpInt]).SaveToStream(Stream);
       end;
     // Fine
     TmpInt := -1;
     Write(TmpInt, SizeOf(TmpInt));
   end;
end;

function TVectFont.CreateChar(Ch: Char; N: Integer): TVectChar;
begin
  if fVects[Ord(Ch)] <> nil then
   fVects[Ord(Ch)].Free;
  Result := TVectChar.Create(N);
  fVects[Ord(Ch)] := Result;
end;

procedure TVectFont.DrawChar2D(Ch: Char; var DrawPoint: TPoint2D; const H, ICS: TRealType; const VT: TTransf2D; Cnv: TDecorativeCanvas);
var
  Cont: Integer;
  TmpTransf: TTransf2D;
  TmpExt: TRect2D;
  TmpCh: TVectChar;
begin
  TmpCh := nil;
  if (Ch = ' ') then
   begin
     DrawPoint.X := DrawPoint.X + (0.4 + ICS) * H;
     Exit;
   end
  else if fVects[Ord(Ch)] <> nil then
   TmpCh := GetChar(Ch)
  else if (Ch <> #13) then
   TmpCh := _NullChar;
  if TmpCh <> nil then
   begin
     TmpTransf := IdentityTransf2D;
     TmpTransf[1, 1] := H;
     TmpTransf[2, 2] := H;
     TmpTransf[3, 1] := DrawPoint.X;
     TmpTransf[3, 2] := DrawPoint.Y;
     TmpTransf[3, 3] := DrawPoint.W;
     with TmpCh do
      begin
        TmpExt := Extension;
        for Cont := 0 to VectorCount - 1 do
         Vectors[Cont].DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), TmpExt, MultiplyTransform2D(TmpTransf, VT));
        DrawPoint.X := DrawPoint.X + (TmpExt.Right + ICS) * H;
      end;
   end;
end;

procedure TVectFont.DrawChar3D(Ch: Char; var DrawPoint: TPoint2D;
                               const H, ICS: TRealType;
                               const NT: TTransf3D;
                               const VT: TTransf2D; Cnv: TDecorativeCanvas);
var
  Cont, ICont: Integer;
  TmpTransf: TTransf2D;
  TmpExt: TRect2D;
  TmpCh: TVectChar;
  TmpVect: TPointsSet3D;
  TmpPt: TPoint2D;
begin
  TmpCh := nil;
  if (Ch = ' ') then
   begin
     DrawPoint.X := DrawPoint.X + (0.4 + ICS) * H;
     Exit;
   end
  else if fVects[Ord(Ch)] <> nil then
   TmpCh := GetChar(Ch)
  else if (Ch <> #13) then
   TmpCh := _NullChar;
  if TmpCh <> nil then
   begin
     TmpTransf := IdentityTransf2D;
     TmpTransf[1, 1] := H;
     TmpTransf[2, 2] := H;
     TmpTransf[3, 1] := DrawPoint.X;
     TmpTransf[3, 2] := DrawPoint.Y;
     TmpTransf[3, 3] := DrawPoint.W;
     TmpVect := TPointsSet3D.Create(10);
     with TmpCh do
      try
        TmpExt := Extension;
        for Cont := 0 to VectorCount - 1 do
         begin
           TmpVect.Clear;
           for ICont := 0 to Vectors[Cont].Count - 1 do
            begin
              TmpPt := TransformPoint2D(Vectors[Cont][ICont], TmpTransf);
              TmpVect.Add(Point2DToPoint3D(TmpPt));
            end;
           TmpVect.DrawAsPolyline(Cnv, TmpVect.Extension, NT, VT);
         end;
        DrawPoint.X := DrawPoint.X + (TmpExt.Right + ICS) * H;
      finally
        TmpVect.Free;
      end;
   end;
end;

function TVectFont.GetTextExtension(Str: AnsiString; H, InterChar, InterLine: TRealType): TRect2D;
var
  Cont: Integer;
  RowLen, RowHeight, MaxRowLen: TRealType;
begin
  // Per i caratteri con la gambetta (come g) parto con Bottom = 1.0
  Result := Rect2D(0.0, 1.0, 0.0, H);
  MaxRowLen := 0.0;
  RowLen := 0.0;
  RowHeight := 0.0;
  for Cont := 1 to Length(Str) do
   begin
     if fVects[Ord(Str[Cont])] <> nil then
      with GetChar(Str[Cont]).Extension do
       begin
         RowLen := RowLen + (Right + InterChar);
         // Bottom contiene l'eventuale gambetta. 
         RowHeight := {%H-}MaxValue([RowHeight, 1.0 - Bottom]);
       end
     else if Str[Cont] = ' ' then
      // Space.
      RowLen := RowLen + (0.5 + InterChar)
     else if Str[Cont] <> #13 then
      // Carattere nullo.
      with _NullChar.Extension do
       RowLen := RowLen + (Right + InterChar);
     if Str[Cont] = #13 then
      begin
        // New line. L'altezza è 1.3 per via delle gambette.
        MaxRowLen := {%H-}MaxValue([MaxRowLen, RowLen - InterChar]);
        Result.Bottom := Result.Bottom - (InterLine + RowHeight);
        RowLen := 0.0;
        RowHeight := 0.0;
      end;
   end;
  MaxRowLen := {%H-}MaxValue([MaxRowLen, RowLen - InterChar]);
  Result.Left := 0.0;
  Result.Bottom := (Result.Bottom - RowHeight) * H;
  Result.Right := MaxRowLen * H;
end;

// =====================================================================
// Registration functions
// =====================================================================

function CADSysFindFontIndex(const Font: TVectFont): Word;
var
  Cont: Integer;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   if Assigned(VectFonts2DRegistered[Cont]) and
      (VectFonts2DRegistered[Cont] = Font) then
    begin
      Result := Cont;
      Exit;
    end;
  Raise ECADObjClassNotFound.Create('CADSysFindFontIndex: Font not found');
end;

function CADSysFindFontByIndex(Index: Word): TVectFont;
begin
  if not Assigned(VectFonts2DRegistered[Index]) then
   begin
     if Assigned(_DefaultFont) then
      Result := _DefaultFont
     else
      Raise ECADObjClassNotFound.Create('CADSysFindFontByIndex: Font not registered');
   end
  else
   Result := VectFonts2DRegistered[Index];
end;

procedure CADSysRegisterFont(Index: Word; const Font: TVectFont);
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysRegisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   Raise ECADObjClassNotFound.Create('CADSysRegisterFont: Font index already allocated');
  VectFonts2DRegistered[Index] := Font;
end;

procedure CADSysUnregisterFont(Index: Word);
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysUnregisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   begin
     VectFonts2DRegistered[Index].Free;
     VectFonts2DRegistered[Index] := nil;
   end;
end;

procedure CADSysRegisterFontFromFile(Index: Word; const FileName: String);
var
  TmpStream: TFileStream;
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysRegisterFontFromFile: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   Raise ECADObjClassNotFound.Create('CADSysRegisterFontFromFile: Font index already allocated');
  if not FileExists(FileName) then
   Raise ECADObjClassNotFound.Create('CADSysRegisterFontFromFile: File not found');
  TmpStream := TFileStream.Create(FileName, fmOpenRead);
  try
   VectFonts2DRegistered[Index] := TVectFont.CreateFromStream(TmpStream, 'CAD422'); // Uso sempre la versione CAD422
  finally
   TmpStream.Free;
  end;
end;

procedure CADSysInitFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   VectFonts2DRegistered[Cont] := nil;
end;

procedure CADSysClearFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   if Assigned(VectFonts2DRegistered[Cont]) then
    VectFonts2DRegistered[Cont].Free;
end;

function CADSysGetDefaultFont: TVectFont;
begin
  Result := _DefaultFont;
end;

procedure CADSysSetDefaultFont(const Font: TVectFont);
begin
  _DefaultFont := Font;
end;

// =====================================================================
// TJustifiedVectText2D
// =====================================================================

procedure TJustifiedVectText2D.SetHeight(H: TRealType);
begin
  fHeight := H;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetCharSpace(S: TRealType);
begin
  fCharSpace := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetInterLine(S: TRealType);
begin
  fInterLine := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetText(T: String);
begin
  fText := T;
  UpdateExtension(Self);
end;

function TJustifiedVectText2D.GetTextExtension: TRect2D;
var
  TmpRect: TRect2D;
  CurrRect: TRect2D;
  DX, TX, TY: TRealType;
begin
  if Assigned(fVectFont) then
   TmpRect := fVectFont.GetTextExtension(fText, fHeight, fCharSpace, fInterLine)
  else
   TmpRect := Rect2D(0, 0, 0, 0);
  CurrRect.FirstEdge := Points[0];
  CurrRect.SecondEdge := Points[1];
  CurrRect := ReorderRect2D(CurrRect);
  fBasePoint := Point2D(CurrRect.Left, CurrRect.Top - TmpRect.Top);
  DX := TmpRect.Right - TmpRect.Left;
  case fHJustification of
   jhLeft: TX := CurrRect.Left;
   jhRight: TX := CurrRect.Right - DX;
   jhCenter: TX := (CurrRect.Left + CurrRect.Right - DX) / 2.0;
  else
   TX := CurrRect.Left;
  end;
  case fVJustification of
   jvTop: TY := CurrRect.Top - TmpRect.Top;
   jvBottom: TY := CurrRect.Bottom - TmpRect.Bottom;
   jvCenter: TY := (CurrRect.Top + CurrRect.Bottom) / 2.0;
  else
   TY := CurrRect.Top - TmpRect.Top;
  end;
  Result := TransformRect2D(TmpRect, Translate2D(TX, TY));
end;

procedure TJustifiedVectText2D.DrawText(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
  procedure DrawTextLine(BasePt: TPoint2D; Str: String; ObjTransf: TTransf2D);
  var
    Cont: Integer;
  begin
    for Cont := 1 to Length(Str) do
     // Disegno il carattere.
     fVectFont.DrawChar2D(Str[Cont], BasePt, fHeight, fCharSpace, ObjTransf, Cnv);
  end;
var
  TmpTransf: TTransf2D;
  TmpStr, TmpRow: String;
  CurrBasePt: TPoint2D;
  TmpPos: Integer;
  TmpTExt: TRect2D;
begin
  if not Assigned(fVectFont) then
   Exit;
  // sposto il testo, applico la trasformazione oggetto al testo e trasformo nel viewport.
  TmpTransf := MultiplyTransform2D(ModelTransform, VT);
  try
    TmpTExt := GetTextExtension;
    if fDrawBox then
     DrawRect2DAsPolyline(Cnv, TmpTExt, RectToRect2D(Cnv.Canvas.ClipRect), ModelTransform, VT);
    if DrawMode and DRAWMODE_VECTTEXTONLYBOX = DRAWMODE_VECTTEXTONLYBOX then
     Exit;
    CurrBasePt.X := TmpTExt.Left;
    CurrBasePt.Y := TmpTExt.Top - fHeight;
    CurrBasePt.W := 1.0;
    // Estraggo le righe.
    TmpStr := fText;
    TmpPos := Pos(#13, TmpStr);
    while TmpPos > 0 do
     begin
       TmpRow := Copy(TmpStr, 1, TmpPos - 1);
       Delete(TmpStr, 1, TmpPos);
       if TmpStr[1] = #10 then
        Delete(TmpStr, 1, 1);
       TmpPos := Pos(#13, TmpStr);
       // Draw the string.
       TmpTExt := fVectFont.GetTextExtension(TmpRow, fHeight, fCharSpace, fInterLine);
       DrawTextLine(CurrBasePt, TmpRow, TmpTransf);
       CurrBasePt.Y := CurrBasePt.Y - (TmpTExt.Top - TmpTExt.Bottom) - fHeight * fInterLine;
     end;
    // Draw the string.
    DrawTextLine(CurrBasePt, TmpStr, TmpTransf);
  finally
  end;
end;

constructor TJustifiedVectText2D.Create(ID: LongInt; FontVect: TVectFont; TextBox: TRect2D; Height: TRealType; Txt: AnsiString);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    Points.Add(TextBox.FirstEdge);
    Points.Add(TextBox.SecondEdge);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
  end;

  fHeight := Height;
  fCharSpace := 0.1;
  fInterLine := 0.02;
  fText := Txt;
  fDrawBox := False;
  fVectFont := FontVect;
  fHJustification := jhLeft;
  fVJustification := jvTop;

  UpdateExtension(Self);
end;

constructor TJustifiedVectText2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
  TmpS: TRealTypeSingle;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     TmpInt := 0;
     Read(TmpInt, SizeOf(TmpInt));
     SetString(fText, nil, TmpInt);
     Read(Pointer(fText)^, TmpInt);
     // Lettura indice font.
     Read(TmpInt, SizeOf(TmpInt));
     try
      fVectFont := CADSysFindFontByIndex(TmpInt);
     except
      on ECADObjClassNotFound do
       begin
         ShowMessage('Font class not found. Font not assigned');
         fVectFont := nil;
       end;
     end;
     Read(fHJustification, SizeOf(fHJustification));
     Read(fVJustification, SizeOf(fVJustification));
     Read(fDrawBox, SizeOf(fDrawBox));
     if( Version >= 'CAD423' ) then
      begin
        Read(fHeight, SizeOf(fHeight));
        Read(fInterLine, SizeOf(fInterLine));
        Read(fCharSpace, SizeOf(fCharSpace));
      end
     else
      begin
        Read({%H-}TmpS, SizeOf(TmpS));
        fHeight := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fInterLine := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fCharSpace := TmpS;
      end;
   end;
end;

procedure TJustifiedVectText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TJustifiedVectText2D) or (Obj is TText2D) then
   begin
     if not Assigned(Points) then
      begin
        Points := CreateVect(2);
        Points.GrowingEnabled := False;
        Points.OnChange := UpdateExtension;
      end;
     if Obj is TJustifiedVectText2D then
      begin
        fText := (Obj as TJustifiedVectText2D).Text;
        fHeight := (Obj as TJustifiedVectText2D).Height;
        fInterLine := (Obj as TJustifiedVectText2D).InterLine;
        fCharSpace := (Obj as TJustifiedVectText2D).CharSpace;
        fDrawBox := (Obj as TJustifiedVectText2D).DrawBox;
        fVectFont := (Obj as TJustifiedVectText2D).fVectFont;
        fHJustification := (Obj as TJustifiedVectText2D).fHJustification;
        fVJustification := (Obj as TJustifiedVectText2D).fVJustification;
      end
     else if Obj is TText2D then
      begin
        fText := (Obj as TText2D).Text;
        fHeight := (Obj as TText2D).Height;
        fDrawBox := (Obj as TText2D).DrawBox;
      end;
     Points.Clear;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
   end;
end;

procedure TJustifiedVectText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited;
  with Stream do
   begin
     TmpInt := Length(fText);
     Write(TmpInt, SizeOf(TmpInt));
     Write(Pointer(fText)^, TmpInt);
     // Scrittura indice font.
     TmpInt := CADSysFindFontIndex(fVectFont);
     Write(TmpInt, SizeOf(TmpInt));
     Write(fHJustification, SizeOf(fHJustification));
     Write(fVJustification, SizeOf(fVJustification));
     Write(fDrawBox, SizeOf(fDrawBox));
     Write(fHeight, SizeOf(fHeight));
     Write(fInterLine, SizeOf(fInterLine));
     Write(fCharSpace, SizeOf(fCharSpace));
   end;
end;

procedure TJustifiedVectText2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  DrawText(VT, Cnv, DrawMode);
end;

function TJustifiedVectText2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  TmpVect: TPointsSet2D;
  TmpDist: TRealType;
  TmpBox: TRect2D;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if (Result = PICK_INBBOX) then
   begin
     TmpBox := GetTextExtension;
     TmpVect := TPointsSet2D.Create(4);
     try
       TmpVect.Add(TmpBox.FirstEdge);
       TmpVect.Add(Point2D(TmpBox.Left, TmpBox.Top));
       TmpVect.Add(TmpBox.SecondEdge);
       TmpVect.Add(Point2D(TmpBox.Right, TmpBox.Bottom));
       TmpDist := 0;
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(TmpVect.PointsReference, TmpVect.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := {%H-}MinValue([Aperture, TmpDist]);
     finally
       TmpVect.Free;
     end;
   end;
end;

procedure TJustifiedVectText2D._UpdateExtension;
begin
  inherited;
  WritableBox := TransformBoundingBox2D(GetTextExtension, ModelTransform);
end;

// =====================================================================
// TPlanarObject3D
// =====================================================================

procedure TPlanarObject3D.UpdateTransforms;
begin
  fWorldPlaneNormal := NormalizeVector3D(TransformNormalVector3D(fPlaneNormal, ModelTransform));
  fPlaneTransform := IdentityTransf3D;
  fPlaneTransform := RowVector3D(fPlaneTransform, 1, CrossProd3D(fPlaneUP, fPlaneNormal));
  fPlaneTransform := RowVector3D(fPlaneTransform, 2, fPlaneUP);
  fPlaneTransform := RowVector3D(fPlaneTransform, 3, fPlaneNormal);
  fPlaneTransform := MultiplyTransform3D(fPlaneTransform, Translate3D(fPlaneReference.X, fPlaneReference.Y, fPlaneReference.Z));
  fPlaneToWorldTr := MultiplyTransform3D(fPlaneTransform, ModelTransform);
  fWorldToPlaneTr := InvertTransform3D(fPlaneToWorldTr);
end;

function TPlanarObject3D.WorldToPlane(WPt: TPoint3D): TPoint3D;
begin
  Result := TransformPoint3D(WPt, fWorldToPlaneTr);
end;

function TPlanarObject3D.PlaneToWorld(PPt: TPoint3D): TPoint3D;
begin
  Result := TransformPoint3D(PPt, fPlaneToWorldTr);
end;

procedure TPlanarObject3D.SetPlaneRef(R: TPoint3D);
begin
  fPlaneReference := R;
  UpdateExtension(Self);
end;

procedure TPlanarObject3D.SetPlaneNorm(N: TVector3D);
var
  OldNorm, TmpUP: TVector3D;
  TmpReal: TRealType;
begin
  N := NormalizeVector3D(N);
  if not IsSameVector3D(N, fPlaneNormal) then
   begin
     OldNorm := fPlaneNormal;
     fPlaneNormal := N;
     // Force UP on the plane.
     TmpReal := DotProduct3D(fPlaneUP, fPlaneNormal);
     if Abs(TmpReal) = 1.0 then
      // Swap the Up with the old normal
      fPlaneUp := OldNorm
     else
      begin
        TmpUP.X := fPlaneUP.X - fPlaneNormal.X * TmpReal;
        TmpUP.Y := fPlaneUP.Y - fPlaneNormal.Y * TmpReal;
        TmpUP.Z := fPlaneUP.Z - fPlaneNormal.Z * TmpReal;
        fPlaneUP := NormalizeVector3D(TmpUP);
      end;
     UpdateExtension(Self);
   end;
end;

procedure TPlanarObject3D.SetPlaneUP(U: TVector3D);
var
  TmpUP: TVector3D;
  TmpReal: TRealType;
begin
  U := NormalizeVector3D(U);
  if not IsSameVector3D(U, fPlaneUP) then
   begin
     fPlaneUP := U;
     // Force UP on the plane.
     TmpReal := DotProduct3D(fPlaneUP, fPlaneNormal);
     if Abs(TmpReal) = 1.0 then
      Raise ECADSysException.Create('TPlanarObject3D: Invalid UP versor.');
     TmpUP.X := fPlaneUP.X - fPlaneNormal.X * TmpReal;
     TmpUP.Y := fPlaneUP.Y - fPlaneNormal.Y * TmpReal;
     TmpUP.Z := fPlaneUP.Z - fPlaneNormal.Z * TmpReal;
     fPlaneUP := NormalizeVector3D(TmpUP);
     UpdateExtension(Self);
   end;
end;

constructor TPlanarObject3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
var
  TmpUP: TVector3D;
  TmpReal: TRealType;
begin
  inherited Create(ID);

  fPlaneReference := PlaneRef;
  fPlaneNormal := NormalizeVector3D(PlaneNorm);
  // Force UP on the plane.
  TmpReal := DotProduct3D(PlUP, fPlaneNormal);
  if Abs(TmpReal) = 1.0 then
   Raise ECADSysException.Create('TPlanarObject3D: Invalid plane parameters.');
  TmpUP.X := PlUP.X - fPlaneNormal.X * TmpReal;
  TmpUP.Y := PlUP.Y - fPlaneNormal.Y * TmpReal;
  TmpUP.Z := PlUP.Z - fPlaneNormal.Z * TmpReal;
  fPlaneUP := NormalizeVector3D(TmpUP);
  UpdateTransforms;
end;

procedure TPlanarObject3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TPlanarObject3D then
   begin
     fPlaneReference := TPlanarObject3D(Obj).fPlaneReference;
     fPlaneNormal := TPlanarObject3D(Obj).fPlaneNormal;
     fPlaneUP := TPlanarObject3D(Obj).fPlaneUP;
     UpdateTransforms;
   end;
end;

constructor TPlanarObject3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  tmpPtS: TPoint3DSingle;
  tmpVS: TVector3DSingle;
begin
  inherited;
  with Stream do
   begin
     if( Version >= 'CAD423' ) then
      begin
        Read(fPlaneReference, SizeOf(fPlaneReference));
        Read(fPlaneNormal, SizeOf(fPlaneNormal));
        Read(fPlaneUP, SizeOf(fPlaneUP));
      end
     else
      begin
        Read({%H-}tmpPtS, SizeOf(tmpPtS));
        fPlaneReference.X := tmpPtS.X;
        fPlaneReference.Y := tmpPtS.Y;
        fPlaneReference.Z := tmpPtS.Z;
        fPlaneReference.W := tmpPtS.W;
        Read(tmpVS, SizeOf(tmpVS));
        fPlaneNormal.X := tmpVS.X;
        fPlaneNormal.Y := tmpVS.Y;
        fPlaneNormal.Z := tmpVS.Z;
        Read(tmpVS, SizeOf(tmpVS));
        fPlaneUP.X := tmpVS.X;
        fPlaneUP.Y := tmpVS.Y;
        fPlaneUP.Z := tmpVS.Z;
      end;
     UpdateTransforms;
   end;
end;

procedure TPlanarObject3D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fPlaneReference, SizeOf(fPlaneReference));
     Write(fPlaneNormal, SizeOf(fPlaneNormal));
     Write(fPlaneUP, SizeOf(fPlaneUP));
   end;
end;

procedure TPlanarObject3D._UpdateExtension;
begin
  UpdateTransforms;
end;

function TPlanarObject3D.HasTransform: Boolean;
begin
  Result := not IsSameTransform3D(fPlaneToWorldTr, IdentityTransf3D);
end;

// =====================================================================
// TPlanar2DObject3D
// =====================================================================

procedure TPlanar2DObject3D.SetObject(O: TObject2D);
begin
  fObject.Free;
  fObject := O;
  UpdateExtension(Self);
end;

constructor TPlanar2DObject3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D; const Obj: TObject2D);
begin
  inherited Create(ID, PlaneRef, PlaneNorm, PlUp);

  fObject := Obj;
  UpdateExtension(Self);
end;

destructor TPlanar2DObject3D.Destroy;
begin
  fObject.Free;
  inherited;
end;

procedure TPlanar2DObject3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  TmpTransf: TTransf3D;
  TmpProject: TTransf2D;
  TmpBox: TRect3D;
begin
  if not Assigned(fObject) then
   Exit;
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end;
  // Trova la trasformazione di normalizzazione.
  TmpTransf := MultiplyTransform3D(PlaneToWorldTransform, NormTransf);
  TmpBox := TransformBoundingBox3D(Box, TmpTransf);
  if (TmpTransf[4, 4] <> 1.0) and not IsBoxAllInFrontNRC3D(TmpBox) then
   begin
     DrawBoundingBox3D(Cnv, VRP, Rect2DToRect3D(fObject.Box), NormTransf, VT);
     Exit;
   end;
  // Ignoro la componente lungo z.
  TmpProject[1, 1] := TmpTransf[1, 1];
  TmpProject[1, 2] := TmpTransf[1, 2];
  TmpProject[1, 3] := TmpTransf[1, 4];
  TmpProject[2, 1] := TmpTransf[2, 1];
  TmpProject[2, 2] := TmpTransf[2, 2];
  TmpProject[2, 3] := TmpTransf[2, 4];
  TmpProject[3, 1] := TmpTransf[4, 1];
  TmpProject[3, 2] := TmpTransf[4, 2];
  TmpProject[3, 3] := TmpTransf[4, 4];

  TmpProject := MultiplyTransform2D(TmpProject, VT);
  fObject.Draw(TmpProject, Cnv, RectToRect2D(Cnv.Canvas.ClipRect), DrawMode);
end;

function TPlanar2DObject3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                                var Distance: TRealType): Integer;
var
  TmpProject, OldTransf: TTransf2D;
  TmpTransf: TTransf3D;
  TmpDist: TRealType;
  TmpBox: TRect3D;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if not Assigned(fObject) then
   Exit;
  if Result = PICK_INBBOX then
   begin
     // Trova la trasformazione di normalizzazione.
     TmpTransf := MultiplyTransform3D(PlaneToWorldTransform, N);
     TmpBox := TransformBoundingBox3D(Box, TmpTransf);
     if (TmpTransf[4, 4] <> 1.0) and not IsBoxAllInFrontNRC3D(TmpBox) then
      begin
        Result := PICK_INBBOX;
        Exit;
      end;
     // Ignoro la componente lungo z.
     TmpProject[1, 1] := TmpTransf[1, 1];
     TmpProject[1, 2] := TmpTransf[1, 2];
     TmpProject[1, 3] := TmpTransf[1, 4];
     TmpProject[2, 1] := TmpTransf[2, 1];
     TmpProject[2, 2] := TmpTransf[2, 2];
     TmpProject[2, 3] := TmpTransf[2, 4];
     TmpProject[3, 1] := TmpTransf[4, 1];
     TmpProject[3, 2] := TmpTransf[4, 2];
     TmpProject[3, 3] := TmpTransf[4, 4];

     OldTransf := fObject.ModelTransform;
     fObject.Transform(TmpProject);
     try
       TmpDist := 0;
       Result := fObject.OnMe(Point3DToPoint2D(P), Aperture, TmpDist);
       if Result = PICK_NOOBJECT then
        Result := PICK_INBBOX
       else
        Distance := TmpDist;
     finally
       fObject.RemoveTransform;
       fObject.ModelTransform := OldTransf;
     end;
   end;
end;

procedure TPlanar2DObject3D._UpdateExtension;
begin
  inherited;
  if not Assigned(fObject) then
   Exit;
  fObject.UpdateExtension(Self);
  WritableBox := TransformBoundingBox3D(Rect2DToRect3D(fObject.Box), PlaneToWorldTransform);
end;

constructor TPlanar2DObject3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpWord: Word;
  TmpClass: TGraphicObjectClass;
begin
  inherited;
  with Stream do
   begin
     { Read the type of object. }
     TmpWord := 0;
     Read(TmpWord, SizeOf(TmpWord));
     if TmpWord <= MAX_REGISTERED_CLASSES then
      begin
        { Retrive the class type from the registered classes. }
        TmpClass := CADSysFindClassByIndex(TmpWord);
        fObject := (TmpClass.CreateFromStream(Stream, Version)) as TObject2D;
        fObject.UpdateExtension(Self);
      end
     else
      fObject := nil;
   end;
end;

procedure TPlanar2DObject3D.SaveToStream(const Stream: TStream);
var
  TmpWord: Word;
begin
  inherited;
  with Stream do
   begin
     { Now write the contained object. }
     if Assigned(fObject) then
      begin
        TmpWord := CADSysFindClassIndex(fObject.ClassName);
        { Save the class index. }
        Write(TmpWord, SizeOf(TmpWord));
        fObject.SaveToStream(Stream);
      end
     else
      begin
        TmpWord := MAX_REGISTERED_CLASSES + 1;
        { Save the class index. }
        Write(TmpWord, SizeOf(TmpWord));
      end;
   end;
end;

procedure TPlanar2DObject3D.ApplyTransform;
var
  XScale, YScale: TRealType;
  TmpV: TVector3D;
begin
  // Calcolo la scalatura lungo X.
  TmpV := CrossProd3D(PlaneUP, PlaneNormal);
  TmpV := TransformVector3D(TmpV, ModelTransform);
  XScale := VectorLength3D(TmpV);
  // Calcolo la scalatura lungo Y.
  TmpV := TransformVector3D(PlaneUP, ModelTransform);
  YScale := VectorLength3D(TmpV);
  fObject.Transform(Scale2D(XScale, YScale));
  fObject.ApplyTransform;
  inherited;
end;

// =====================================================================
// TPlanarFieldGrid3D
// =====================================================================

constructor TPlanarFieldGrid3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
begin
  inherited;

  fDeltaX := 50.0;
  fDeltaY := 50.0;
  fFieldExt := 1e4;
  fXColor := clRed;
  fYColor := clBlue;
  fColor := clSilver;
  fPlaneRef := PlaneRef;
  fPlaneUp := PlUp;
  fPlaneNorm := PlaneNorm;
  UpdateExtension(Self);
end;

procedure TPlanarFieldGrid3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  TmpTransf: TTransf3D;
  TmpPt1, TmpPt2: TPoint3D;
begin
  // Trova la trasformazione di normalizzazione.
  if HasTransform then
    TmpTransf := MultiplyTransform3D(ModelTransform, NormTransf)
  else
    TmpTransf := NormTransf;

  // Linee orizzontali (divisioni lungo Y).
  // Se la normale è parallela al piano non itero mai.
  if DotProduct3D(fPlaneNorm, PlaneNormal) <> 0 then
   begin
     // Verso le Y negative.
     Cnv.Canvas.Pen.Color := fColor;
     TmpPt1 := Point3D(-fFieldExt, fPlaneRef.Y, 0);
     TmpPt2 := Point3D(fFieldExt, fPlaneRef.Y, 0);
     while (TmpPt1.Y >= -fFieldExt) do
      begin
        DrawLine3D(Cnv, TmpPt1, TmpPt2, TmpTransf, VT);
        TmpPt1.Y := TmpPt1.Y - fDeltaY;
        TmpPt2.Y := TmpPt2.Y - fDeltaY;
      end;
     // Verso le Y positive.
     Cnv.Canvas.Pen.Color := fXColor;
     TmpPt1 := Point3D(-fFieldExt, fPlaneRef.Y, 0);
     TmpPt2 := Point3D(fFieldExt, fPlaneRef.Y, 0);
     while (TmpPt1.Y <= fFieldExt) do
      begin
        DrawLine3D(Cnv, TmpPt1, TmpPt2, TmpTransf, VT);
        TmpPt1.Y := TmpPt1.Y + fDeltaY;
        TmpPt2.Y := TmpPt2.Y + fDeltaY;
        Cnv.Canvas.Pen.Color := fColor;
      end;
   end;
  // Linee verticali (divisioni lungo X).
  // Se la normale è parallela al piano non itero mai.
  if DotProduct3D(fPlaneNorm, PlaneNormal) <> 0 then
   begin
     Cnv.Canvas.Pen.Color := fColor;
     // Verso le X negative.
     TmpPt1 := Point3D(fPlaneRef.X, -fFieldExt, 0);
     TmpPt2 := Point3D(fPlaneRef.X, fFieldExt, 0);
     while (TmpPt1.X >= -fFieldExt) do
      begin
        DrawLine3D(Cnv, TmpPt1, TmpPt2, TmpTransf, VT);
        TmpPt1.X := TmpPt1.X - fDeltaX;
        TmpPt2.X := TmpPt2.X - fDeltaX;
      end;
     // Verso le X positive.
     Cnv.Canvas.Pen.Color := fYColor;
     TmpPt1 := Point3D(fPlaneRef.X, -fFieldExt, 0);
     TmpPt2 := Point3D(fPlaneRef.X, fFieldExt, 0);
     while (TmpPt1.X <= fFieldExt) do
      begin
        DrawLine3D(Cnv, TmpPt1, TmpPt2, TmpTransf, VT);
        TmpPt1.X := TmpPt1.X + fDeltaX;
        TmpPt2.X := TmpPt2.X + fDeltaX;
        Cnv.Canvas.Pen.Color := fColor;
      end;
   end;
end;

procedure TPlanarFieldGrid3D._UpdateExtension;
begin
  inherited;
  WritableBox := Rect3D(-fFieldExt, -fFieldExt, -fFieldExt, fFieldExt, fFieldExt, fFieldExt);
end;

procedure TPlanarFieldGrid3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TPlanarFieldGrid3D then
   begin
     fDeltaX := TPlanarFieldGrid3D(Obj).fDeltaX;
     fDeltaY := TPlanarFieldGrid3D(Obj).fDeltaY;
     fFieldExt := TPlanarFieldGrid3D(Obj).fFieldExt;
     UpdateExtension(Self);
   end;
end;

constructor TPlanarFieldGrid3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  tmpS: TRealTypeSingle;
begin
  inherited;
  with Stream do
   begin
     if( Version >= 'CAD423' ) then
      begin
        Read(fDeltaX, SizeOf(fDeltaX));
        Read(fDeltaY, SizeOf(fDeltaY));
        Read(fFieldExt, SizeOf(fFieldExt));
      end
     else
      begin
        Read({%H-}tmpS, SizeOf(tmpS));
        fDeltaX := tmpS;
        Read(tmpS, SizeOf(tmpS));
        fDeltaY := tmpS;
        Read(tmpS, SizeOf(tmpS));
        fFieldExt := tmpS;
      end;
   end;
end;

procedure TPlanarFieldGrid3D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fDeltaX, SizeOf(fDeltaX));
     Write(fDeltaY, SizeOf(fDeltaY));
     Write(fFieldExt, SizeOf(fFieldExt));
   end;
end;

// =====================================================================
// TPrimitive3DHandler
// =====================================================================

procedure TPrimitive3DHandler.DrawControlPoints(const Sender: TObject3D; const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer);
var
  Cont: Integer;
  TmpPoint3D: TPoint3D;
  TmpPoint2D: TPoint2D;
begin
  if Sender is TPrimitive3D then
   with TPrimitive3D(Sender) do
    for Cont := 0 to Points.Count - 1 do
     begin
       if HasTransform then
        TmpPoint3D := TransformPoint3D(Points[Cont], MultiplyTransform3D(ModelTransform, NormTransf))
       else
        TmpPoint3D := TransformPoint3D(Points[Cont], NormTransf);
       if IsPointInBoxNRC3D(TmpPoint3D) then
        begin
          TmpPoint2D := TransformPoint2D(Point3DToPoint2D(TmpPoint3D), VT);
          DrawPlaceHolder(Cnv, Round(TmpPoint2D.X), Round(TmpPoint2D.Y), Width);
        end;
     end;
end;

function TPrimitive3DHandler.OnMe(const Sender: TObject3D; P: TPoint3D; const NormTransf: TTransf3D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  TmpTransf: TTransf3D;
  Cont: Integer;
  ResDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  if Sender is TPrimitive3D then
   with TPrimitive3D(Sender) do
    begin
      if HasTransform then
       TmpTransf := MultiplyTransform3D(ModelTransform, NormTransf)
      else
       TmpTransf := NormTransf;
      for Cont := 0 to Points.Count - 1 do
       if NearPoint3D(Points[Cont], P, Aperture, {%H-}ResDist, TmpTransf) and (ResDist <= Distance) then
        begin
          Result := Cont;
          Distance := ResDist;
        end;
    end;
end;

// =====================================================================
// TPrimitive3D
// =====================================================================

procedure TPrimitive3D._UpdateExtension;
begin
  { Change the extension. }
  if not Assigned(fPoints) then Exit;
  if HasTransform then
   WritableBox := TransformBoundingBox3D(fPoints.Extension, ModelTransform)
  else
   WritableBox := fPoints.Extension;
end;

function TPrimitive3D.CreateVect(const Size: Integer): TPointsSet3D;
begin
  Result := TPointsSet3D.Create(Size);
end;

constructor TPrimitive3D.Create(ID: LongInt; NPts: Integer);
begin
  inherited Create(ID);
  { Create the internal vector. }
  fPoints := CreateVect(NPts);
  fPoints.GrowingEnabled := True;
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler3D);
end;

procedure TPrimitive3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive3D) then
   begin
     if not Assigned(fPoints) then
      begin
        fPoints := CreateVect(0);
        fPoints.GrowingEnabled := True;
        fPoints.OnChange := UpdateExtension;
      end;
     fPoints.Clear;
   end;
end;

constructor TPrimitive3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint3D;
  TmpBoolean: Boolean;
  TmpPtS: TPoint3DSingle;
begin
  { Load the standard properties }
  inherited CreateFromStream(Stream, Version);
  with Stream do
   begin
     TmpWord := 0;
     Read(TmpWord, SizeOf(TmpWord));
     fPoints := CreateVect(TmpWord);
     { Read all the points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        if( Version >= 'CAD423' ) then
         begin
           Read({%H-}TmpPt, SizeOf(TmpPt));
         end
        else
         begin
           Read({%H-}TmpPtS, SizeOf(TmpPtS));
           TmpPt.X := TmpPtS.X;
           TmpPt.Y := TmpPtS.Y;
           TmpPt.Z := TmpPtS.Z;
           TmpPt.W := TmpPtS.W;
         end;
        fPoints.Points[Cont] := TmpPt;
      end;
     TmpBoolean := False;
     Read(TmpBoolean, SizeOf(TmpBoolean));
     fPoints.GrowingEnabled := TmpBoolean;
   end;
  fPoints.OnChange := UpdateExtension;
end;

procedure TPrimitive3D.SaveToStream(const Stream: TStream);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint3D;
  TmpBoolean: Boolean;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     TmpWord := fPoints.Count;
     Write(TmpWord, SizeOf(TmpWord));
     { Write all points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        TmpPt := fPoints.Points[Cont];
        Write(TmpPt, SizeOf(TmpPt));
      end;
     TmpBoolean := fPoints.GrowingEnabled;
     Write(TmpBoolean, SizeOf(TmpBoolean));
   end;
end;

destructor TPrimitive3D.Destroy;
begin
  if Assigned(fPoints) then fPoints.Free;
  inherited Destroy;
end;

// =====================================================================
// TPlanarPrimitive3D
// =====================================================================

{$WARNINGS OFF}
constructor TPlanarPrimitive3D.Create(ID: LongInt; NPts: Integer;const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
begin
  inherited Create(ID, NPts);
  fPlanarObj := TPlanarObject3D.Create(0, PlaneRef, PlaneNorm, PlUp);
end;
{$WARNINGS ON}

destructor TPlanarPrimitive3D.Destroy;
begin
  fPlanarObj.Free;
  fPlanarObj := nil;
  inherited;
end;

{$WARNINGS OFF}
procedure TPlanarPrimitive3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TPlanarPrimitive3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarPrimitive3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end
  else if (Obj is TPlanarOutline3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarOutline3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end
  else if (Obj is TPlanarCurve3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarCurve3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end;
end;

constructor TPlanarPrimitive3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  fPlanarObj := TPlanarObject3D.CreateFromStream(Stream, Version);
  fPlanarObj.UpdateExtension(Self);
  inherited;
end;
{$WARNINGS ON}

procedure TPlanarPrimitive3D.SaveToStream(const Stream: TStream);
begin
  fPlanarObj.SaveToStream(Stream);
  inherited;
end;

function  TPlanarPrimitive3D.HasTransform: Boolean;
begin
  Result := fPlanarObj.HasTransform;
end;

function  TPlanarPrimitive3D.GetModelTransform: TTransf3D;
begin
  Result := fPlanarObj.PlaneToWorldTransform;
end;

procedure TPlanarPrimitive3D.SetModelTransform(Transf: TTransf3D);
begin
  if not Assigned(fPlanarObj) then
   Exit;
  fPlanarObj.ModelTransform := Transf;
end;

procedure TPlanarPrimitive3D.ApplyTransform;
begin
  fPlanarObj.ApplyTransform;
  UpdateExtension(Self);
end;

// Forzo i punti di controllo a rimanere sul piano di definizione del profilo.
procedure TPlanarPrimitive3D._UpdateExtension;
begin
  if not Assigned(PlanarObject) then
   Exit;
  fPlanarObj.UpdateExtension(Self);
  inherited;
end;

procedure TPlanarPrimitive3D.SetPlaneParameters(const PlaneR: TPoint3D; const PlaneN, PlaneU: TVector3D);
begin
  if not Assigned(fPlanarObj) then
   Exit;
  // Trasformo il piano di definizione.
  fPlanarObj.PlaneReference := PlaneR;
  fPlanarObj.PlaneNormal := PlaneN;
  fPlanarObj.PlaneUP := PlaneU;
  UpdateExtension(Self);
end;

// =====================================================================
// TJustifiedVectText3D
// =====================================================================

procedure TJustifiedVectText3D.SetHeight(H: TRealType);
begin
  fHeight := H;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText3D.SetCharSpace(S: TRealType);
begin
  fCharSpace := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText3D.SetInterLine(S: TRealType);
begin
  fInterLine := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText3D.SetText(T: String);
begin
  fText := T;
  UpdateExtension(Self);
end;

function TJustifiedVectText3D.GetTextExtension: TRect2D;
var
  TmpRect: TRect2D;
  CurrRect: TRect2D;
  DX, TX, TY: TRealType;
begin
  if Assigned(fVectFont) then
   TmpRect := fVectFont.GetTextExtension(fText, fHeight, fCharSpace, fInterLine)
  else
   TmpRect := Rect2D(0, 0, 0, 0);
  CurrRect.FirstEdge := Point3DToPoint2D(Points[0]);
  CurrRect.SecondEdge := Point3DToPoint2D(Points[1]);
  CurrRect := ReorderRect2D(CurrRect);
  fBasePoint := Point2D(CurrRect.Left, CurrRect.Top - TmpRect.Top);
  DX := TmpRect.Right - TmpRect.Left;
  case fHJustification of
   jhLeft: TX := CurrRect.Left;
   jhRight: TX := CurrRect.Right - DX;
   jhCenter: TX := (CurrRect.Left + CurrRect.Right - DX) / 2.0;
  else
   TX := CurrRect.Left;
  end;
  case fVJustification of
   jvTop: TY := CurrRect.Top - TmpRect.Top;
   jvBottom: TY := CurrRect.Bottom - TmpRect.Bottom;
   jvCenter: TY := (CurrRect.Top + CurrRect.Bottom) / 2.0;
  else
   TY := CurrRect.Top - TmpRect.Top;
  end;
  Result := TransformRect2D(TmpRect, Translate2D(TX, TY));
end;

procedure TJustifiedVectText3D.DrawText(const NT: TTransf3D;
                                        const VT: TTransf2D;
                                        const Cnv: TDecorativeCanvas;
                                        const DrawMode: Integer);
  procedure DrawTextLine(BasePt: TPoint2D;
                         Str: String;
                         ObjTransf: TTransf3D);
  var
    Cont: Integer;
  begin
    for Cont := 1 to Length(Str) do
     // Disegno il carattere.
     fVectFont.DrawChar3D(Str[Cont],
                          BasePt, fHeight, fCharSpace,
                          ObjTransf, VT, Cnv);
  end;
var
  TmpTransf: TTransf3D;
  TmpStr, TmpRow: String;
  CurrBasePt: TPoint2D;
  TmpPos: Integer;
  TmpTExt: TRect2D;
begin
  if not Assigned(fVectFont) then
   Exit;
  // sposto il testo, applico la trasformazione oggetto al testo e trasformo nel viewport.
  TmpTransf := MultiplyTransform3D(ModelTransform, NT);
  try
    TmpTExt := GetTextExtension;
    if DrawMode and DRAWMODE_VECTTEXTONLYBOX = DRAWMODE_VECTTEXTONLYBOX then
     Exit;
    CurrBasePt.X := TmpTExt.Left;
    CurrBasePt.Y := TmpTExt.Top - fHeight;
    CurrBasePt.W := 1.0;
    // Estraggo le righe.
    TmpStr := fText;
    TmpPos := Pos(#13, TmpStr);
    while TmpPos > 0 do
     begin
       TmpRow := Copy(TmpStr, 1, TmpPos - 1);
       Delete(TmpStr, 1, TmpPos);
       if TmpStr[1] = #10 then
        Delete(TmpStr, 1, 1);
       TmpPos := Pos(#13, TmpStr);
       // Draw the string.
       TmpTExt := fVectFont.GetTextExtension(TmpRow, fHeight, fCharSpace, fInterLine);
       DrawTextLine(CurrBasePt, TmpRow, TmpTransf);
       CurrBasePt.Y := CurrBasePt.Y - (TmpTExt.Top - TmpTExt.Bottom) - fHeight * fInterLine;
     end;
    // Draw the string.
    DrawTextLine(CurrBasePt, TmpStr, TmpTransf);
  finally
  end;
end;

constructor TJustifiedVectText3D.Create(ID: LongInt;
                                        const PlaneRef: TPoint3D;
                                        const PlaneNorm, PlUp: TVector3D;
                                        const FontVect: TVectFont;
                                        const TextBox: TRect2D;
                                        const Height: TRealType;
                                        const Txt: AnsiString);
begin
  inherited Create(ID, 2, PlaneRef, PlaneNorm, PlUp);
  Points.DisableEvents := True;
  try
    with Rect2DToRect3D(TextBox) do
     begin
       Points.Add(FirstEdge);
       Points.Add(SecondEdge);
     end;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
  end;

  fHeight := Height;
  fCharSpace := 0.1;
  fInterLine := 0.02;
  fText := Txt;
  fDrawBox := False;
  fVectFont := FontVect;
  fHJustification := jhLeft;
  fVJustification := jvTop;
  UpdateExtension(Self);
end;

constructor TJustifiedVectText3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
  TmpS: TRealTypeSingle;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     TmpInt := 0;
     Read(TmpInt, SizeOf(TmpInt));
     SetString(fText, nil, TmpInt);
     Read(Pointer(fText)^, TmpInt);
     // Lettura indice font.
     Read(TmpInt, SizeOf(TmpInt));
     try
      fVectFont := CADSysFindFontByIndex(TmpInt);
     except
      on ECADObjClassNotFound do
       begin
         ShowMessage('Font class not found. Font not assigned');
         fVectFont := nil;
       end;
     end;
     Read(fHJustification, SizeOf(fHJustification));
     Read(fVJustification, SizeOf(fVJustification));
     Read(fDrawBox, SizeOf(fDrawBox));
     if( Version >= 'CAD423' ) then
      begin
        Read(fHeight, SizeOf(fHeight));
        Read(fInterLine, SizeOf(fInterLine));
        Read(fCharSpace, SizeOf(fCharSpace));
      end
     else
      begin
        Read({%H-}TmpS, SizeOf(TmpS));
        fHeight := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fInterLine := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fCharSpace := TmpS;
      end;
   end;
end;

procedure TJustifiedVectText3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TJustifiedVectText3D)then
   begin
     if not Assigned(Points) then
      begin
        Points := CreateVect(2);
        Points.GrowingEnabled := False;
        Points.OnChange := UpdateExtension;
      end;
     fText := (Obj as TJustifiedVectText3D).Text;
     fHeight := (Obj as TJustifiedVectText3D).Height;
     fInterLine := (Obj as TJustifiedVectText3D).InterLine;
     fCharSpace := (Obj as TJustifiedVectText3D).CharSpace;
     fDrawBox := (Obj as TJustifiedVectText3D).DrawBox;
     fVectFont := (Obj as TJustifiedVectText3D).fVectFont;
     fHJustification := (Obj as TJustifiedVectText3D).fHJustification;
     fVJustification := (Obj as TJustifiedVectText3D).fVJustification;
     Points.Clear;
     Points.Copy(TPrimitive3D(Obj).Points, 0, 1);
   end;
end;

procedure TJustifiedVectText3D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited;
  with Stream do
   begin
     TmpInt := Length(fText);
     Write(TmpInt, SizeOf(TmpInt));
     Write(Pointer(fText)^, TmpInt);
     // Scrittura indice font.
     TmpInt := CADSysFindFontIndex(fVectFont);
     Write(TmpInt, SizeOf(TmpInt));
     Write(fHJustification, SizeOf(fHJustification));
     Write(fVJustification, SizeOf(fVJustification));
     Write(fDrawBox, SizeOf(fDrawBox));
     Write(fHeight, SizeOf(fHeight));
     Write(fInterLine, SizeOf(fInterLine));
     Write(fCharSpace, SizeOf(fCharSpace));
   end;
end;

procedure TJustifiedVectText3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
begin
  DrawText(NormTransf, VT, Cnv, DrawMode);
end;

function TJustifiedVectText3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                                   var Distance: TRealType): Integer;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   Result := PICK_INOBJECT;
end;

procedure TJustifiedVectText3D._UpdateExtension;
begin
  inherited;
  WritableBox := TransformBoundingBox3D(Rect2DToRect3D(GetTextExtension), ModelTransform);
end;

// =====================================================================
// TLine3D
// =====================================================================

constructor TLine3D.Create(ID: LongInt; const P1, P2: TPoint3D);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TLine3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine3D) then
   begin
     Points.Copy(TPrimitive3D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

procedure TLine3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
begin
  if HasTransform then
   DrawLine3D(Cnv, Points[0], Points[1], MultiplyTransform3D(ModelTransform, NormTransf), VT)
  else
   DrawLine3D(Cnv, Points[0], Points[1], NormTransf, VT);
end;

function TLine3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                      var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if HasTransform then
      Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(Points.PointsReference, Points.Count, P, {%H-}TmpDist, Aperture, MultiplyTransform3D(ModelTransform, N), False)])
     else
      Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(Points.PointsReference, Points.Count, P, TmpDist, Aperture, N, False)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

// =====================================================================
// TOutline3D
// =====================================================================

function TOutline3D.GetIsClosed: Boolean;
var
  TmpVect: TPointsSet3D;
begin
  BeginUseProfilePoints;
  try
    TmpVect := ProfilePoints;
    if TmpVect <> nil then
     Result := (TmpVect.Count > 2) and IsSamePoint3D(TmpVect[0], TmpVect[TmpVect.Count - 1])
    else
     Result := False;
  finally
    EndUseProfilePoints;
  end;
end;

procedure TOutline3D.BeginUseProfilePoints;
begin
end;

procedure TOutline3D.EndUseProfilePoints;
begin
end;

// =====================================================================
// TPolyLine3D
// =====================================================================

constructor TPolyline3D.Create(ID: LongInt; const Pts: array of TPoint3D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1);
  Points.AddPoints(Pts);
  Points.GrowingEnabled := True;
end;

procedure TPolyLine3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine3D) or (Obj is TPolyline3D) then
   begin
     Points.Copy(TPrimitive3D(Obj).Points, 0, TPrimitive3D(Obj).Points.Count - 1);
     Points.GrowingEnabled := False;
   end;
end;

procedure TPolyLine3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end;
  if HasTransform then
   Points.DrawAsPolyline(Cnv, Box, MultiplyTransform3D(ModelTransform, NormTransf), VT)
  else
   Points.DrawAsPolyline(Cnv, Box, NormTransf, VT);
end;

function TPolyline3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                          var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if HasTransform then
      Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(Points.PointsReference, Points.Count, P, {%H-}TmpDist, Aperture, MultiplyTransform3D(ModelTransform, N), False)])
     else
      Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(Points.PointsReference, Points.Count, P, TmpDist, Aperture, N, False)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

function TPolyline3D.GetProfilePoints: TPointsSet3D;
begin
  Result := Points;
end;

function TPolyline3D.GetNPts: Integer;
begin
  Result := Points.Count;
end;

// =====================================================================
// TFace3D
// =====================================================================

function TFace3D.GetIsClosed: Boolean;
begin
  Result := True;
end;

constructor TFace3D.Create(ID: LongInt; const Pts: array of TPoint3D);
begin
  inherited Create(ID, Pts);
  fNormal := GetVectNormal(Points.PointsReference, Points.Count);
end;

procedure TFace3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  TmpTransf: TTransf3D;
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end
  else if (DrawMode and DRAWMODE_SHOWORIENTATION) = DRAWMODE_SHOWORIENTATION then
   begin
     if DotProduct3D(fNormal, Vector3D(VRP, Points[0])) < 0.0 then
      Cnv.Canvas.Pen.Color := clGreen
     else
      Cnv.Canvas.Pen.Color := clRed;
   end;
  if HasTransform then
   TmpTransf := MultiplyTransform3D(ModelTransform, NormTransf)
  else
   TmpTransf := NormTransf;
  Points.DrawSubSetAsPolyline(Cnv, Box, TmpTransf, VT, 0, Points.Count - 1, True);
end;

function TFace3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                                var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if HasTransform then
      Result := MaxIntValue([PICK_INBBOX, IsPointOnLine3D(Points[Points.Count - 1], Points[0], P, {%H-}TmpDist, Aperture, MultiplyTransform3D(ModelTransform, N))])
     else
      Result := MaxIntValue([PICK_INBBOX, IsPointOnLine3D(Points[Points.Count - 1], Points[0], P, TmpDist, Aperture, N)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

function TFace3D.IsVisible(const NormTransf: TTransf3D; const VRP: TPoint3D; const DrawMode: Integer): Boolean;
begin
  Result := inherited IsVisible(NormTransf, VRP, DrawMode);
  if Result and (DrawMode and DRAWMODE_BACKFACECULLING = DRAWMODE_BACKFACECULLING) then
   Result := DotProduct3D(fNormal, Vector3D(VRP, Points[0])) < 0.0;
end;

procedure TFace3D._UpdateExtension;
begin
  if Assigned(Points) then
   fNormal := NormalizeVector3D(TransformNormalVector3D(GetVectNormal(Points.PointsReference, Points.Count), ModelTransform));
  inherited;
end;

// =====================================================================
// TPlanarOutline3D
// =====================================================================

{$WARNINGS OFF}
constructor TPlanarOutline3D.Create(ID: LongInt; NPts: Integer;const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
begin
  inherited Create(ID, NPts);
  fPlanarObj := TPlanarObject3D.Create(0, PlaneRef, PlaneNorm, PlUp);
end;
{$WARNINGS ON}

destructor TPlanarOutline3D.Destroy;
begin
  fPlanarObj.Free;
  fPlanarObj := nil;
  inherited;
end;

{$WARNINGS OFF}
procedure TPlanarOutline3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TPlanarPrimitive3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarPrimitive3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end
  else if (Obj is TPlanarOutline3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarOutline3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end
  else if (Obj is TPlanarCurve3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarCurve3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end;
end;

constructor TPlanarOutline3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  fPlanarObj := TPlanarObject3D.CreateFromStream(Stream, Version);
  fPlanarObj.UpdateExtension(Self);
  inherited;
end;
{$WARNINGS ON}

procedure TPlanarOutline3D.SaveToStream(const Stream: TStream);
begin
  fPlanarObj.SaveToStream(Stream);
  inherited;
end;

function  TPlanarOutline3D.HasTransform: Boolean;
begin
  Result := fPlanarObj.HasTransform;
end;

function  TPlanarOutline3D.GetModelTransform: TTransf3D;
begin
  Result := fPlanarObj.PlaneToWorldTransform;
end;

procedure TPlanarOutline3D.SetModelTransform(Transf: TTransf3D);
begin
  if not Assigned(fPlanarObj) then
   Exit;
  fPlanarObj.ModelTransform := Transf;
end;

procedure TPlanarOutline3D.ApplyTransform;
begin
  fPlanarObj.ApplyTransform;
  UpdateExtension(Self);
end;

// Forzo i punti di controllo a rimanere sul piano di definizione del profilo.
procedure TPlanarOutline3D._UpdateExtension;
begin
  if not Assigned(PlanarObject) then
   Exit;
  fPlanarObj.UpdateTransforms;
  inherited;
end;

procedure TPlanarOutline3D.SetPlaneParameters(const PlaneR: TPoint3D; const PlaneN, PlaneU: TVector3D);
begin
  if not Assigned(fPlanarObj) then
   Exit;
  // Trasformo il piano di definizione.
  fPlanarObj.PlaneReference := PlaneR;
  fPlanarObj.PlaneNormal := PlaneN;
  fPlanarObj.PlaneUP := PlaneU;
  UpdateExtension(Self);
end;

function TPlanarOutline3D.IsVisible(const NormTransf: TTransf3D; const VRP: TPoint3D; const DrawMode: Integer): Boolean;
begin
  Result := inherited IsVisible(NormTransf, VRP, DrawMode);
  if Result and (DrawMode and DRAWMODE_BACKFACECULLING = DRAWMODE_BACKFACECULLING) then
   Result := DotProduct3D(fPlanarObj.fWorldPlaneNormal, Direction3D(VRP, Points[0])) < 0.0;
end;

function TPlanarOutline3D.GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean;
var
  Cont: Integer;
begin
  FacePts.Clear;
  if not IsClosed then
   begin
     Result := False;
     FaceID := -1;
     Exit;
   end;
  if I > 0 then
   begin
     Result := False;
     FaceID := -1;
     Exit;
   end;
  BeginUseProfilePoints;
  try
    for Cont := 0 to ProfilePoints.Count - 1 do
     FacePts.Add(ProfilePoints[Cont]);
  finally
   EndUseProfilePoints;
  end;
  if IsSamePoint3D(FacePts[0], FacePts[FacePts.Count - 1]) then
   FacePts.Delete(FacePts.Count - 1);
  FaceNormal := fPlanarObj.PlaneNormal;
  FaceID := 0;
  FacePts.FrontFace(FaceNormal);
  FacePts.TransformPoints(ModelTransform);
  Inc(I);
  Result := True;
end;

// =====================================================================
// TPlanarPolyLine3D
// =====================================================================

constructor TPlanarPolyline3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const Pts: array of TPoint3D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, PlaneRef, CrossProd3D(XDir, YDir), YDir);
  Points.AddPoints(Pts);
  Points.GrowingEnabled := True;
end;

procedure TPlanarPolyline3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine3D) or (Obj is TPolyline3D) or (Obj is TPlanarPolyline3D) then
   begin
     Points.Copy(TPrimitive3D(Obj).Points, 0, TPrimitive3D(Obj).Points.Count - 1);
     Points.GrowingEnabled := True;
   end;
end;

procedure TPlanarPolyline3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end
  else if DrawMode and DRAWMODE_SHOWORIENTATION = DRAWMODE_SHOWORIENTATION then
   begin
     if DotProduct3D(fPlanarObj.fWorldPlaneNormal, Direction3D(VRP, Points[0])) < 0.0 then
      Cnv.Canvas.Pen.Color := clGreen
     else
      Cnv.Canvas.Pen.Color := clRed;
   end;
  if HasTransform then
   Points.DrawAsPolyline(Cnv, Box, MultiplyTransform3D(ModelTransform, NormTransf), VT)
  else
   Points.DrawAsPolyline(Cnv, Box, NormTransf, VT);
end;

function TPlanarPolyline3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                                var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if HasTransform then
      Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(Points.PointsReference, Points.Count, P, {%H-}TmpDist, Aperture, MultiplyTransform3D(ModelTransform, N), False)])
     else
      Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(Points.PointsReference, Points.Count, P, TmpDist, Aperture, N, False)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

function TPlanarPolyline3D.GetProfilePoints: TPointsSet3D;
begin
  Result := Points;
end;

function TPlanarPolyline3D.GetNPts: Integer;
begin
  Result := Points.Count;
end;

// =====================================================================
// TPlanarFace3D
// =====================================================================

function TPlanarFace3D.GetIsClosed: Boolean;
begin
  Result := True;
end;

procedure TPlanarFace3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  TmpTransf: TTransf3D;
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end
  else if DrawMode and DRAWMODE_SHOWORIENTATION = DRAWMODE_SHOWORIENTATION then
   begin
     if DotProduct3D(fPlanarObj.fWorldPlaneNormal, Direction3D(VRP, Points[0])) < 0.0 then
      Cnv.Canvas.Pen.Color := clGreen
     else
      Cnv.Canvas.Pen.Color := clRed;
   end;
  if HasTransform then
   TmpTransf := MultiplyTransform3D(ModelTransform, NormTransf)
  else
   TmpTransf := NormTransf;
  Points.DrawSubSetAsPolyline(Cnv, Box, TmpTransf, VT, 0, Points.Count - 1, True);
end;

function TPlanarFace3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                                var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if HasTransform then
      Result := MaxIntValue([PICK_INBBOX, IsPointOnLine3D(Points[Points.Count - 1], Points[0], P, {%H-}TmpDist, Aperture, MultiplyTransform3D(ModelTransform, N))])
     else
      Result := MaxIntValue([PICK_INBBOX, IsPointOnLine3D(Points[Points.Count - 1], Points[0], P, TmpDist, Aperture, N)]);
     Distance := {%H-}MinValue([Aperture, TmpDist]);
   end;
end;

// =====================================================================
// TCurve3D
// =====================================================================

procedure TCurve3D.SetPrimitiveSavingType(S: TPrimitiveSavingType);
begin
  if S <> fSavingType then
   begin
     fSavingType := S;
     UpdateExtension(Self);
   end;
end;

procedure TCurve3D.SetCurvePrecision(N: Word);
begin
  if fCurvePrecision <> N then
   fCurvePrecision := N;
end;

function TCurve3D.PopulateCurvePoints(N: Word): TRect3D;
begin
  if not Assigned(fCurvePoints) then
   fCurvePoints := TPointsSet3D.Create(N)
  else
   fCurvePoints.Clear;
  Inc(fCountReference);
  fCurvePoints.GrowingEnabled := True;
  Result := Rect3D(0, 0, 0, 0, 0, 0);
end;

procedure TCurve3D.FreeCurvePoints;
begin
  Dec(fCountReference);
  if fCountReference <= 0 then
   begin
     fCurvePoints.Free;
     fCurvePoints := nil;
     fCountReference := 0;
   end;
end;

constructor TCurve3D.Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
begin
  inherited Create(ID, NPts);

  fCurvePrecision := CurvePrec;
  fCurvePoints := nil;
  fCountReference := 0;
  fSavingType := stSpace;
end;

destructor TCurve3D.Destroy;
begin
  fCountReference := 0;
  FreeCurvePoints;
  inherited;
end;

procedure TCurve3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TCurve3D then
   begin
     CurvePrecision := TCurve3D(Obj).fCurvePrecision;
     SavingType := TCurve3D(Obj).fSavingType;
   end;
end;

constructor TCurve3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  with Stream do
   begin
     Read(fCurvePrecision, SizeOf(fCurvePrecision));
     Read(fSavingType, SizeOf(fSavingType));
     fCountReference := 0;
   end;
end;

procedure TCurve3D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fCurvePrecision, SizeOf(fCurvePrecision));
     Write(fSavingType, SizeOf(fSavingType));
   end;
end;

procedure TCurve3D._UpdateExtension;
begin
  if not Assigned(Points) or (Points.Count = 0) then
   Exit;
  case fSavingType of
   stSpace: begin
     WritableBox := PopulateCurvePoints(0);
     FreeCurvePoints;
   end;
   stTime: begin
     FreeCurvePoints;
     WritableBox := PopulateCurvePoints(0);
   end;
  end;
end;

procedure TCurve3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end;
  BeginUseProfilePoints;
  try
    if Assigned(fCurvePoints) then
     begin
       if not HasTransform then
        fCurvePoints.DrawSubSetAsPolyline(Cnv, Box, NormTransf, VT, 0, fCurvePoints.Count - 1, False)
       else
        fCurvePoints.DrawSubSetAsPolyline(Cnv, Box, MultiplyTransform3D(ModelTransform, NormTransf), VT, 0, fCurvePoints.Count - 1, False)
     end;
  finally
   EndUseProfilePoints;
  end;
end;

function TCurve3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                       var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     BeginUseProfilePoints;
     try
       if not Assigned(fCurvePoints) then
        Exit;
       if HasTransform then
        Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(fCurvePoints.PointsReference, fCurvePoints.Count, P, {%H-}TmpDist, Aperture, MultiplyTransform3D(ModelTransform, N), False)])
       else
        Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(fCurvePoints.PointsReference, fCurvePoints.Count, P, TmpDist, Aperture, N, False)]);
       Distance := {%H-}MinValue([Aperture, TmpDist]);
     finally
       EndUseProfilePoints;
     end;
   end;
end;

function TCurve3D.GetProfilePoints: TPointsSet3D;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve3D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints;
end;

function TCurve3D.GetNPts: Integer;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve3D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints.Count;
end;

procedure TCurve3D.BeginUseProfilePoints;
begin
  if fSavingType = stSpace then
   WritableBox := PopulateCurvePoints(0);
end;

procedure TCurve3D.EndUseProfilePoints;
begin
  if fSavingType = stSpace then
   FreeCurvePoints;
end;

// =====================================================================
// TPlanarCurve3D
// =====================================================================

{$WARNINGS OFF}
constructor TPlanarCurve3D.Create(ID: LongInt; NPts: Integer; CurvePrec: Word; const PlaneRef: TPoint3D; const PlaneNorm, PlUp: TVector3D);
begin
  inherited Create(ID, NPts, CurvePrec);
  fPlanarObj := TPlanarObject3D.Create(0, PlaneRef, PlaneNorm, PlUp);
end;
{$WARNINGS ON}

destructor TPlanarCurve3D.Destroy;
begin
  fPlanarObj.Free;
  fPlanarObj := nil;
  inherited;
end;

{$WARNINGS OFF}
procedure TPlanarCurve3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TPlanarPrimitive3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarPrimitive3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end
  else if (Obj is TPlanarOutline3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarOutline3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end
  else if (Obj is TPlanarCurve3D) then
   begin
     if not Assigned(fPlanarObj) then
      fPlanarObj := TPlanarObject3D.Create(0, Point3D(0, 0, 0), Versor3D(0, 0, 1), Versor3D(0, 1, 0));
     fPlanarObj.Assign(TPlanarCurve3D(Obj).fPlanarObj);
     fPlanarObj.UpdateExtension(Self);
   end;
end;

constructor TPlanarCurve3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  fPlanarObj := TPlanarObject3D.CreateFromStream(Stream, Version);
  fPlanarObj.UpdateExtension(Self);
  inherited;
end;
{$WARNINGS ON}

procedure TPlanarCurve3D.SaveToStream(const Stream: TStream);
begin
  fPlanarObj.SaveToStream(Stream);
  inherited;
end;

function  TPlanarCurve3D.HasTransform: Boolean;
begin
  Result := fPlanarObj.HasTransform;
end;

function  TPlanarCurve3D.GetModelTransform: TTransf3D;
begin
  // I punti di controllo sono definiti sul piano e quindi la trasformazione modello
  // deve contenere la trasformazione da piano a mondo e la trasformazione
  // del piano stesso.
  Result := fPlanarObj.PlaneToWorldTransform;
end;

procedure TPlanarCurve3D.SetModelTransform(Transf: TTransf3D);
begin
  if not Assigned(fPlanarObj) then
   Exit;
  fPlanarObj.ModelTransform := Transf;
end;

procedure TPlanarCurve3D.ApplyTransform;
begin
  fPlanarObj.ApplyTransform;
  UpdateExtension(Self);
end;

// Forzo i punti di controllo a rimanere sul piano di definizione del profilo.
procedure TPlanarCurve3D._UpdateExtension;
begin
  if Assigned(fPlanarObj) then
   fPlanarObj.UpdateExtension(Self);
  inherited;
end;

function TPlanarCurve3D.IsVisible(const NormTransf: TTransf3D; const VRP: TPoint3D; const DrawMode: Integer): Boolean;
begin
  Result := inherited IsVisible(NormTransf, VRP, DrawMode);
  if Result and (DrawMode and DRAWMODE_BACKFACECULLING = DRAWMODE_BACKFACECULLING) then
   Result := DotProduct3D(fPlanarObj.fWorldPlaneNormal, Direction3D(VRP, Points[0])) < 0.0;
end;

procedure TPlanarCurve3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
begin
  if DrawMode and DRAWMODE_SHOWORIENTATION = DRAWMODE_SHOWORIENTATION then
   begin
     if DotProduct3D(fPlanarObj.fWorldPlaneNormal, Direction3D(VRP, Points[0])) < 0.0 then
      Cnv.Canvas.Pen.Color := clGreen
     else
      Cnv.Canvas.Pen.Color := clRed;
   end;
  inherited;
end;

procedure TPlanarCurve3D.SetPlaneParameters(const PlaneR: TPoint3D; const PlaneN, PlaneU: TVector3D);
begin
  if not Assigned(PlanarObject) then
   Exit;
  // Trasformo il piano di definizione.
  fPlanarObj.PlaneReference := PlaneR;
  fPlanarObj.PlaneNormal := PlaneN;
  fPlanarObj.PlaneUP := PlaneU;
  UpdateExtension(Self);
end;

function TPlanarCurve3D.GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean;
var
  Cont: Integer;
begin
  FacePts.Clear;
  if not IsClosed then
   begin
     Result := False;
     FaceID := -1;
     Exit;
   end;
  if I > 0 then
   begin
     Result := False;
     FaceID := -1;
     Exit;
   end;
  BeginUseProfilePoints;
  try
    for Cont := 0 to ProfilePoints.Count - 1 do
     FacePts.Add(ProfilePoints[Cont]);
  finally
   EndUseProfilePoints;
  end;
  if IsSamePoint3D(FacePts[0], FacePts[FacePts.Count - 1]) then
   FacePts.Delete(FacePts.Count - 1);
  FaceNormal := fPlanarObj.PlaneNormal;
  FacePts.FrontFace(FaceNormal);
  FacePts.TransformPoints(ModelTransform);
  FaceID := 0;
  Inc(I);
  Result := True;
end;

// =====================================================================
// TFrame3D
// =====================================================================

procedure TFrame3D.GetFramePoints2D(var P0, P1: TPoint2D);
begin
  P0 := Point3DToPoint2D(Points[0]);
  P1 := Point3DToPoint2D(Points[1]);
end;

function TFrame3D.PopulateCurvePoints(N: Word): TRect3D;
var
  P0, P1: TPoint2D;
begin
  GetFramePoints2D({%H-}P0, {%H-}P1);
  inherited PopulateCurvePoints(5);
  // Popola il vettore curvilineo.
  ProfilePoints.Add(Point2DToPoint3D(P0));
  ProfilePoints.Add(Point2DToPoint3D(Point2D(P0.X, P1.Y)));
  ProfilePoints.Add(Point2DToPoint3D(P1));
  ProfilePoints.Add(Point2DToPoint3D(Point2D(P1.X, P0.Y)));
  ProfilePoints.Add(ProfilePoints[0]);
  Result := TransformBoundingBox3D(ProfilePoints.Extension, ModelTransform);
end;

constructor TFrame3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const P1, P2: TPoint3D);
begin
  inherited Create(ID, 2, 5, PlaneRef, CrossProd3D(XDir, YDir), YDir);

  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TFrame3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TFrame3D) or (Obj is TEllipse3D) or (Obj is TArc3D) then
   begin
     Points.Copy(TPrimitive3D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TArc3D
// =====================================================================

procedure TArc3D.GetArcPoints2D(var P0, P1, P2, P3: TPoint2D);
begin
  P0 := Point3DToPoint2D(Points[0]);
  P1 := Point3DToPoint2D(Points[1]);
  if Points.Count < 3 then
   begin
     P2 := P0;
     P3 := P0;
   end
  else
   begin
     P2 := Point3DToPoint2D(Points[2]);
     P3 := Point3DToPoint2D(Points[3]);
   end;
end;

procedure TArc3D.GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
var
  P0, P1, P2, P3: TPoint2D;
begin
  GetArcPoints2D({%H-}P0, {%H-}P1, {%H-}P2, {%H-}P3);
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  RX := Abs(P1.X - P0.X) / 2.0;
  RY := Abs(P1.Y - P0.Y) / 2.0;
  case FDirection of
   adClockwise: begin
     SA := ArcTan2(CY - P2.Y, P2.X - CX);
     EA := ArcTan2(CY - P3.Y, P3.X - CX);
   end;
   adCounterClockwise: begin
     SA := ArcTan2(P2.Y - CY, P2.X - CX);
     EA := ArcTan2(P3.Y - CY, P3.X - CX);
   end;
  end;
end;

procedure TArc3D.SetStartAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  fStartAngle := A;
  GetArcParams({%H-}CX, {%H-}CY, {%H-}RX, {%H-}RY, {%H-}SA, {%H-}EA);
  Points[2] := Point3D(CX + RX * Cos(A), CY + RY * Sin(A), 0);
end;

procedure TArc3D.SetEndAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  fEndAngle := A;
  GetArcParams({%H-}CX, {%H-}CY, {%H-}RX, {%H-}RY, {%H-}SA, {%H-}EA);
  Points[3] := Point3D(CX + RX * Cos(A), CY + RY * Sin(A), 0);
end;

procedure TArc3D.SetArcDirection(D: TArcDirection);
begin
  if D <> FDirection then
   begin
     FDirection := D;
     UpdateExtension(Self);
   end;
end;

function TArc3D.PopulateCurvePoints(N: Word): TRect3D;
var
  Cont, NPts: Integer;
  CX, RX, CY, RY: TRealType;
  Delta, CurrAngle: TRealType;
  TmpPt: TPoint2D;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect3D(0, 0, 0, 0, 0, 0);
     Exit;
   end;
  GetArcParams({%H-}CX, {%H-}CY, {%H-}RX, {%H-}RY, {%H-}fStartAngle, {%H-}fEndAngle);
  if fEndAngle = fStartAngle then
   fEndAngle := fEndAngle + TWOPI;
  NPts := CurvePrecision;
  // Calcola il delta angolare tra due punti
  if fStartAngle < fEndAngle then
   Delta := (fEndAngle - fStartAngle) / (NPts + 1)
     else
   Delta := (TWOPI - fStartAngle + fEndAngle) / (NPts + 1);
  // Crea il vettore curvilineo.
  inherited PopulateCurvePoints(NPts + 1);
  // Popola il vettore curvilineo.
  if FDirection = adClockwise then
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts - 1 do
      begin
        TmpPt := Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle));
        ProfilePoints.Add(Point2DToPoint3D(TmpPt));
        CurrAngle := CurrAngle + Delta
      end;
     TmpPt := Point2D(CX + RX * Cos(fEndAngle), CY - RY * Sin(fEndAngle));
     ProfilePoints.Add(Point2DToPoint3D(TmpPt));
   end
  else
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts - 1 do
      begin
        TmpPt := Point2D(CX + RX * Cos(CurrAngle), CY + RY * Sin(CurrAngle));
        ProfilePoints.Add(Point2DToPoint3D(TmpPt));
        CurrAngle := CurrAngle + Delta
      end;
     TmpPt := Point2D(CX + RX * Cos(fEndAngle), CY + RY * Sin(fEndAngle));
     ProfilePoints.Add(Point2DToPoint3D(TmpPt));
   end;
  Result := TransformBoundingBox3D(ProfilePoints.Extension, ModelTransform);
end;

{ Angle in radiant. }
constructor TArc3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const P1, P2: TPoint3D; SA, EA: TRealType);
begin
  inherited Create(ID, 4, 50, PlaneRef, CrossProd3D(XDir, YDir), YDir);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.Add(Point3D(0, 0, 0));
    Points.Add(Point3D(0, 0, 0));
    Points.GrowingEnabled := False;
    fDirection := adClockwise;
    StartAngle := SA;
    EndAngle := EA;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TArc3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse3D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive3D(Obj).Points, 0, 1);
       Points.Add(Point3D(0, 0, 0));
       Points.Add(Point3D(0, 0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TArc3D then
   begin
     fStartAngle := (Obj as TArc3D).StartAngle;
     fEndAngle := (Obj as TArc3D).EndAngle;
     fDirection := (Obj as TArc3D).Direction;
     Points.Copy(TPrimitive3D(Obj).Points, 0, 3);
   end;
end;

constructor TArc3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited CreateFromStream(Stream, Version);
  with Stream do
   Read(FDirection, SizeOf(FDirection));
end;

procedure TArc3D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;

// =====================================================================
// TEllipse3D
// =====================================================================

procedure TEllipse3D.GetEllipsePoints2D(var P0, P1: TPoint2D);
begin
  P0 := Point3DToPoint2D(Points[0]);
  P1 := Point3DToPoint2D(Points[1]);
end;

procedure TEllipse3D.GetEllipseParams(var CX, CY, RX, RY: TRealType);
var
  P0, P1: TPoint2D;
begin
  GetEllipsePoints2D({%H-}P0, {%H-}P1);
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  RX := Abs(P1.X - P0.X) / 2.0;
  RY := Abs(P1.Y - P0.Y) / 2.0;
end;

function TEllipse3D.PopulateCurvePoints(N: Word): TRect3D;
var
  Cont: Integer;
  TmpPt: TPoint2D;
  Delta, CurrAngle, CX, RX, CY, RY: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect3D(0, 0, 0, 0, 0, 0);
     Exit;
   end;
  inherited PopulateCurvePoints(CurvePrecision + 1);

  GetEllipseParams({%H-}CX, {%H-}CY, {%H-}RX, {%H-}RY);
  Delta := TWOPI / CurvePrecision;
  TmpPt := Point2D(CX + RX, CY);
  ProfilePoints.Add(Point2DToPoint3D(TmpPt));
  CurrAngle := Delta;
  for Cont := 1 to CurvePrecision - 1 do
   begin
     TmpPt := Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle));
     ProfilePoints.Add(Point2DToPoint3D(TmpPt));
     CurrAngle := CurrAngle + Delta
   end;
  TmpPt := Point2D(CX + RX, CY);
  ProfilePoints.Add(Point2DToPoint3D(TmpPt));
  Result := TransformBoundingBox3D(ProfilePoints.Extension, ModelTransform);
end;

constructor TEllipse3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const P1, P2: TPoint3D);
begin
  inherited Create(ID, 2, 50, PlaneRef, CrossProd3D(XDir, YDir), YDir);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TEllipse3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse3D) or (Obj is TArc3D) then
   begin
     Points.Copy(TPrimitive3D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

function TEllipse3D.GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean;
var
  CX: TPoint3D;
begin
  FacePts.Clear;
  if I > NumberOfProfilePts - 2 then
   begin
     Result := False;
     FaceID := -1;
     Exit;
   end;
  BeginUseProfilePoints;
  try
    CX.X := (Points[0].X + Points[1].X) / 2.0;
    CX.Y := (Points[0].Y + Points[1].Y) / 2.0;
    CX.Z := (Points[0].Z + Points[1].Z) / 2.0;
    CX.W := 1.0;
    FacePts.Add(CX);
    FacePts.Add(ProfilePoints[I + 1]);
    FacePts.Add(ProfilePoints[I]);
  finally
   EndUseProfilePoints;
  end;
  FaceNormal := fPlanarObj.PlaneNormal;
  FacePts.FrontFace(FaceNormal);
  FacePts.TransformPoints(ModelTransform);
  FaceID := 0;
  Inc(I);
  Result := True;
end;

// =====================================================================
// TBSpline3D
// =====================================================================

procedure TPlanarSpline3D.GetSplineCtrlPoints2D(const V: TPointsSet2D);
var
  Cont: Integer;
begin
  for Cont := 0 to Points.Count - 1 do
   V.Add(Point3DToPoint2D(Points[Cont]));
end;

function TPlanarSpline3D.PopulateCurvePoints(N: Word): TRect3D;
var
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpVect: TPointsSet2D;
begin
  if (CurvePrecision = 0) or (Points.Count = 0) then
   begin
     Result := Rect3D(0, 0, 0, 0, 0, 0);
     Exit;
   end;
  if Points.Count < FOrder then
   begin
     inherited PopulateCurvePoints(Points.Count);
     ProfilePoints.Copy(Points, 0, Points.Count - 1);
   end
  else
   begin
     inherited PopulateCurvePoints(CurvePrecision + 1);
     TmpVect := TPointsSet2D.Create(Points.Count);
     try
      GetSplineCtrlPoints2D(TmpVect);
      for Cont := 0 to CurvePrecision - 1 do
       begin
         TmpPt := BSpline2D(Cont / CurvePrecision * (TmpVect.Count - 2), TmpVect.Count - 1, FOrder, TmpVect);
         ProfilePoints.Add(Point2DToPoint3D(TmpPt));
       end;
      ProfilePoints.Add(Points[Points.Count - 1]);
     finally
      TmpVect.Free;
     end;
   end;
  Result := TransformBoundingBox3D(ProfilePoints.Extension, ModelTransform);
end;

constructor TPlanarSpline3D.Create(ID: LongInt; const PlaneRef: TPoint3D; const XDir, YDir: TVector3D; const Pts: array of TPoint3D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, 50, PlaneRef, CrossProd3D(XDir, YDir), YDir);
  fOrder := 3; { The default spline is cubic. }
  Points.AddPoints(Pts);
  Points.GrowingEnabled := True;
end;

constructor TPlanarSpline3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited CreateFromStream(Stream, Version);
  with Stream do
   { Load the particular properties. }
   Read(fOrder, SizeOf(fOrder));
end;

procedure TPlanarSpline3D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fOrder, SizeOf(fOrder));
end;

procedure TPlanarSpline3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TPlanarSpline3D) then
   begin
     fOrder := TPlanarSpline3D(Obj).fOrder;
     Points.Copy(TPrimitive3D(Obj).Points, 0, TPrimitive3D(Obj).Points.Count - 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TMesh3D
// =====================================================================

function TMesh3D.GetMeshPt(M, N: Integer): TPoint3D;
begin
  Result := Points[fN * M + N];
end;

procedure TMesh3D.SetMeshPt(M, N: Integer; Pt: TPoint3D);
begin
  Points[fN * M + N] := Pt;
end;

// I punti sono ordinati per righe (M) e colonne (N). Prima ci sono
// i valori per la riga M=0 (N=0,1..), poi M=1 (N=0,1..) etc.
constructor TMesh3D.Create(ID: LongInt; M, N: Integer; const Pts: array of TPoint3D);
begin
  if M * N = 0 then
   Raise ECADSysException.Create('TMesh3D: Invalid size for a mesh.');
  inherited Create(ID, M * N);

  fN := N;
  fM := M;
  Points.DisableEvents := True;
  try
    Points.AddPoints(Pts);
    Points.GrowingEnabled := False;
  finally
   Points.DisableEvents := False;
  end;
end;

constructor TMesh3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  with Stream do
   begin
     Read(fM, sizeOf(fM));
     Read(fN, sizeOf(fN));
   end;
end;

procedure TMesh3D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fM, sizeOf(fM));
     Write(fN, sizeOf(fN));
   end;
end;

procedure TMesh3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TMesh3D) then
   begin
     Points.Copy(TPrimitive3D(Obj).Points, 0, TPrimitive3D(Obj).Points.Count - 1);
     Points.GrowingEnabled := False;
     fN := TMesh3D(Obj).fN;
     fM := TMesh3D(Obj).fM;
   end;
end;

procedure TMesh3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  TmpTransf: TTransf3D;
  RowCont, ColCont: Integer;
  TmpVect: TPointsSet3D;
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end;
  if HasTransform then
   TmpTransf := MultiplyTransform3D(ModelTransform, NormTransf)
  else
   TmpTransf := NormTransf;
  // Disegno le righe.
  for RowCont := 0 to fM - 1 do
   Points.DrawSubSetAsPolyline(Cnv, Box, TmpTransf, VT, RowCont * fN, (RowCont + 1) * fN - 1, False);
  // Disegno le colonne.
  TmpVect := TPointsSet3D.Create(fM);
  try
    for ColCont := 0 to fN - 1 do
     begin
       // creo il vettore temporaneo.
       for RowCont := 0 to fM - 1 do
        TmpVect[RowCont] := GetMeshPt(RowCont, ColCont);
       TmpVect.DrawAsPolyline(Cnv, Box, TmpTransf, VT);
     end;
  finally
    TmpVect.Free;
  end;
end;

function TMesh3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                  var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
  TmpTransf: TTransf3D;
  RowCont, ColCont: Integer;
  TmpVect: TPointsSet3D;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if HasTransform then
      TmpTransf := MultiplyTransform3D(ModelTransform, N)
     else
      TmpTransf := N;

     // Controllo le righe.
     TmpVect := TPointsSet3D.Create(fM);
     try
      for RowCont := 0 to fM - 1 do
       begin
         // creo il vettore temporaneo.
         for ColCont := 0 to fN - 1 do
          TmpVect[ColCont] := GetMeshPt(RowCont, ColCont);
         Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(TmpVect.PointsReference, TmpVect.Count, P, {%H-}TmpDist, Aperture, TmpTransf, False)]);
         if Result = PICK_ONOBJECT then
          begin
            Distance := {%H-}MinValue([Aperture, TmpDist]);
            Exit;
          end;
       end;
     finally
      TmpVect.Free;
     end;
     // Controllo le colonne.
     TmpVect := TPointsSet3D.Create(fM);
     try
      for ColCont := 0 to fN - 1 do
       begin
         // creo il vettore temporaneo.
         for RowCont := 0 to fM - 1 do
          TmpVect[RowCont] := GetMeshPt(RowCont, ColCont);
         Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine3D(TmpVect.PointsReference, TmpVect.Count, P, TmpDist, Aperture, TmpTransf, False)]);
         if Result = PICK_ONOBJECT then
          begin
            Distance := {%H-}MinValue([Aperture, TmpDist]);
            Exit;
          end;
       end;
     finally
      TmpVect.Free;
     end;
   end;
end;

function TMesh3D.GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean;
var
  FRow, FCol: Integer;
begin
  FacePts.Clear;
  if I >= (fM - 1) * (fN - 1) then
   begin
     Result := False;
     FaceID := -1;
     Exit;
   end;
  // Calcolo il numero della faccia.
  FRow := I mod (fM - 1);
  FCol := I div (fM - 1);
  // Creo la faccia
  FacePts.Add(GetMeshPt(FRow, FCol));
  FacePts.Add(GetMeshPt(FRow, FCol + 1));
  FacePts.Add(GetMeshPt(FRow + 1, FCol + 1));
  FacePts.Add(GetMeshPt(FRow + 1, FCol));
  FaceNormal := GetVectNormal(FacePts.PointsReference, FacePts.Count);
  FacePts.TransformPoints(ModelTransform);
  FaceID := 0;
  Inc(I);
  Result := True;
end;

// =====================================================================
// TPolyface3D
// =====================================================================

constructor TQuadEdge3D.Create(const Pts: TPointsSet3D; const P1, P2, P3, P4: SmallInt);
begin
  inherited Create;

  fPtsList := Pts;
  fV1 := P1;
  fV2 := P2;
  fV3 := P3;
  fV4 := P4;
  // Per ottimizzazione
  fFullQuad := (P1 > 0) and (P2 > 0) and (P3 > 0) and (P4 > 0);
  fFullTri := (P1 > 0) and (P2 > 0) and (P3 > 0) and (P4 = 0);
end;

procedure TQuadEdge3D.SetFace(const P1, P2, P3, P4: SmallInt);
begin
  fV1 := P1;
  fV2 := P2;
  fV3 := P3;
  fV4 := P4;
  // Per ottimizzazione
  fFullQuad := (P1 > 0) and (P2 > 0) and (P3 > 0) and (P4 > 0);
  fFullTri := (P1 > 0) and (P2 > 0) and (P3 > 0) and (P4 = 0);
end;

procedure TQuadEdge3D.DrawFace(const DrawMode: Integer; const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas);
begin
  // Disegno la faccia.
  if (DrawMode and DRAWMODE_SHOWORIENTATION = DRAWMODE_SHOWORIENTATION) and
     (fFullQuad or fFullTri) then
   begin
     if DotProduct3D(CrossProd3D(Direction3D(fPtsList[Abs(fV1) - 1], fPtsList[Abs(fV2) - 1]),
           Direction3D(fPtsList[Abs(fV2) - 1], fPtsList[Abs(fV3) - 1])),
           Direction3D(VRP, fPtsList[Abs(fV1) - 1])) < 0.0 then
      Cnv.Canvas.Pen.Color := clGreen
     else
      Cnv.Canvas.Pen.Color := clRed;
   end
  else if (DrawMode and DRAWMODE_BACKFACECULLING = DRAWMODE_BACKFACECULLING) and
          (fFullQuad or fFullTri) then
   begin
     if DotProduct3D(CrossProd3D(Direction3D(fPtsList[Abs(fV2) - 1], fPtsList[Abs(fV3) - 1]),
           Direction3D(fPtsList[Abs(fV2) - 1], fPtsList[Abs(fV1) - 1])),
           Direction3D(VRP, fPtsList[Abs(fV1) - 1])) > 0.0 then
      Exit;
   end;
  if fFullQuad then
   begin
     DrawLine3D(Cnv, fPtsList[fV1 - 1], fPtsList[Abs(fV2) - 1], NormTransf, VT);
     DrawLine3D(Cnv, fPtsList[fV2 - 1], fPtsList[Abs(fV3) - 1], NormTransf, VT);
     DrawLine3D(Cnv, fPtsList[fV3 - 1], fPtsList[Abs(fV4) - 1], NormTransf, VT);
     DrawLine3D(Cnv, fPtsList[fV4 - 1], fPtsList[Abs(fV1) - 1], NormTransf, VT);
   end
  else if fFullTri then
   begin
     DrawLine3D(Cnv, fPtsList[fV1 - 1], fPtsList[Abs(fV2) - 1], NormTransf, VT);
     DrawLine3D(Cnv, fPtsList[fV2 - 1], fPtsList[Abs(fV3) - 1], NormTransf, VT);
     DrawLine3D(Cnv, fPtsList[fV3 - 1], fPtsList[Abs(fV1) - 1], NormTransf, VT);
   end
  else
   begin
     if (fV1 > 0) and (fV2 <> 0) then
      DrawLine3D(Cnv, fPtsList[fV1 - 1], fPtsList[Abs(fV2) - 1], NormTransf, VT);
     if (fV2 > 0) and (fV3 <> 0) then
      DrawLine3D(Cnv, fPtsList[fV2 - 1], fPtsList[Abs(fV3) - 1], NormTransf, VT);
     if (fV3 > 0) and (fV4 <> 0) then
      DrawLine3D(Cnv, fPtsList[fV3 - 1], fPtsList[Abs(fV4) - 1], NormTransf, VT);
     if (fV3 > 0) and (fV4 = 0) then
      DrawLine3D(Cnv, fPtsList[fV3 - 1], fPtsList[Abs(fV1) - 1], NormTransf, VT)
     else if (fV3 > 0) and (fV4 > 0) then
      DrawLine3D(Cnv, fPtsList[fV4 - 1], fPtsList[Abs(fV1) - 1], NormTransf, VT);
   end;
end;

function TQuadEdge3D.OnFace(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                            var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  if (fV1 > 0) and (fV2 <> 0) then
   Result := IsPointOnLine3D(fPtsList[fV1 - 1], fPtsList[Abs(fV2) - 1], P, {%H-}TmpDist, Aperture, N);
  if (Result = PICK_NOOBJECT) and (fV2 > 0) and (fV3 <> 0) then
   Result := IsPointOnLine3D(fPtsList[fV2 - 1], fPtsList[Abs(fV3) - 1], P, TmpDist, Aperture, N);
  if (Result = PICK_NOOBJECT) and (fV3 > 0) and (fV4 <> 0) then
   Result := IsPointOnLine3D(fPtsList[fV3 - 1], fPtsList[Abs(fV4) - 1], P, TmpDist, Aperture, N);
  if (Result = PICK_NOOBJECT) and (fV3 > 0) and (fV4 = 0) then
   Result := IsPointOnLine3D(fPtsList[fV3 - 1], fPtsList[Abs(fV1) - 1], P, TmpDist, Aperture, N)
  else if (Result = PICK_NOOBJECT) and (fV3 > 0) and (fV4 > 0) then
   Result := IsPointOnLine3D(fPtsList[fV4 - 1], fPtsList[Abs(fV1) - 1], P, TmpDist, Aperture, N);
  Distance := {%H-}MinValue([Aperture, TmpDist]);
end;

procedure TQuadEdge3D.SaveToStream(const Stream: TStream);
begin
  with Stream do
   begin
     Write(fV1, Sizeof(fV1));
     Write(fV2, Sizeof(fV2));
     Write(fV3, Sizeof(fV3));
     Write(fV4, Sizeof(fV4));
   end;
end;

procedure TQuadEdge3D.LoadFromStream(const Stream: TStream);
begin
  with Stream do
   begin
     Read(fV1, Sizeof(fV1));
     Read(fV2, Sizeof(fV2));
     Read(fV3, Sizeof(fV3));
     Read(fV4, Sizeof(fV4));
   end;
end;


function TPolyface3D.GetNFaces: Integer;
begin
  Result := fFaces.NumberOfObjects;
end;

function TPolyface3D.GetFace(FI: Integer): TQuadEdge3D;
begin
  Result := fFaces.Objects[FI] as TQuadEdge3D;
end;

procedure TPolyface3D.SetFace(FI: Integer; Q: TQuadEdge3D);
begin
  if fFaces.Objects[FI] <> Q then
   fFaces.Objects[FI].Free;
  fFaces.Objects[FI] := Q;
end;

// I punti sono ordinati per righe (M) e colonne (N). Prima ci sono
// i valori per la riga M=0 (N=0,1..), poi M=1 (N=0,1..) etc.
constructor TPolyface3D.Create(ID: LongInt; NPts, NFaces: Integer; const Pts: array of TPoint3D);
begin
  inherited Create(ID, NPts);

  Points.AddPoints(Pts);
  Points.GrowingEnabled := True;
  // Di default non sono definite delle facce.
  fFaces := TIndexedObjectList.Create(NFaces);
  fFaces.FreeOnClear := True;
end;

destructor TPolyface3D.Destroy;
begin
  fFaces.Free;
  inherited;
end;

constructor TPolyface3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  Cont: Integer;
begin
  inherited;
  with Stream do
   begin
     Cont := 0;
     Read(Cont, SizeOf(Cont));
     fFaces := TIndexedObjectList.Create(Cont);
     fFaces.FreeOnClear := True;
     for Cont := 0 to fFaces.NumberOfObjects - 1 do
      begin
        fFaces[Cont] := TQuadEdge3D.Create(Points, 0, 0, 0, 0);
        TQuadEdge3D(fFaces[Cont]).LoadFromStream(Stream);
      end;
   end;
end;

procedure TPolyface3D.SaveToStream(const Stream: TStream);
var
  Cont: Integer;
begin
  inherited;
  with Stream do
   begin
     Cont := fFaces.NumberOfObjects;
     Write(Cont, SizeOf(Cont));
     for Cont := 0 to fFaces.NumberOfObjects - 1 do
      TQuadEdge3D(fFaces[Cont]).SaveToStream(Stream);
   end;
end;

procedure TPolyface3D.Assign(const Obj: TGraphicObject);
var
  Cont: Integer;
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if Obj is TPolyface3D then
   begin
     Points.Copy(TPrimitive3D(Obj).Points, 0, TPrimitive3D(Obj).Points.Count - 1);
     Points.GrowingEnabled := False;
     fFaces.Free;
     fFaces := TIndexedObjectList.Create(TPolyface3D(Obj).fFaces.NumberOfObjects);
     fFaces.FreeOnClear := True;
     for Cont := 0 to fFaces.NumberOfObjects - 1 do
      with TPolyface3D(Obj).Faces[Cont] do
       fFaces[Cont] := TQuadEdge3D.Create(Points, V1, V2, V3, V4);
   end;
end;

procedure TPolyface3D.AddFace(FI: Integer; P1, P2, P3, P4: Integer);
begin
  if not Assigned(fFaces.Objects[FI]) then
   fFaces.Objects[FI] := TQuadEdge3D.Create(Points, P1, P2, P3, P4)
  else
   TQuadEdge3D(fFaces.Objects[FI]).SetFace(P1, P2, P3, P4);
end;

procedure TPolyface3D.DeleteFace(FI: Integer);
begin
  if Assigned(fFaces.Objects[FI]) then
   begin
     fFaces.Objects[FI].Free;
     fFaces.Objects[FI] := nil;
   end;
end;

procedure TPolyface3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  TmpTransf: TTransf3D;
  Cont: Integer;
begin
  if (DrawMode and DRAWMODE_ONLYBOUNDINGBOX) = DRAWMODE_ONLYBOUNDINGBOX then
   begin
     DrawBoundingBox3D(Cnv, VRP, Box, NormTransf, VT);
     Exit;
   end;
  if HasTransform then
   TmpTransf := MultiplyTransform3D(ModelTransform, NormTransf)
  else
   TmpTransf := NormTransf;
  // Disegno le facce.
  for Cont := 0 to fFaces.NumberOfObjects - 1 do
   TQuadEdge3D(fFaces[Cont]).DrawFace(DrawMode, TmpTransf, VRP, VT, Cnv);
end;

function TPolyface3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                          var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
  TmpTransf: TTransf3D;
  Cont: Integer;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     Cont := 0;
     if HasTransform then
      TmpTransf := MultiplyTransform3D(ModelTransform, N)
     else
      TmpTransf := N;
     while (Cont < fFaces.NumberOfObjects) and (Result = PICK_INBBOX) do
      begin
        Result := MaxIntValue([Result, TQuadEdge3D(fFaces[Cont]).OnFace(P, TmpTransf, Aperture, {%H-}TmpDist)]);
        Inc(Cont);
      end;
     if Result > PICK_INBBOX then
      Distance := {%H-}MinValue([Distance, TmpDist]);
   end;
end;

function TPolyface3D.GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean;
begin
  FacePts.Clear;
  Result := False;
  while I < fFaces.NumberOfObjects do
   with TQuadEdge3D(fFaces.Objects[I]) do
    begin
      // Creo la faccia
      if fV1 <> 0 then
       FacePts.Add(fPtsList[Abs(fV1) - 1]);
      if fV2 <> 0 then
       FacePts.Add(fPtsList[Abs(fV2) - 1]);
      if fV3 <> 0 then
       FacePts.Add(fPtsList[Abs(fV3) - 1]);
      if fV4 <> 0 then
       FacePts.Add(fPtsList[Abs(fV4) - 1]);
      if FacePts.Count >= 3 then
       begin
         FaceNormal := GetVectNormal(FacePts.PointsReference, FacePts.Count);
         FacePts.TransformPoints(ModelTransform);
         FaceID := 0;
         Result := True;
         Inc(I);
         Break;
       end;
      FacePts.Clear;
      Inc(I);
    end;
end;

// =====================================================================
// TSweepedOutline3D
// =====================================================================

procedure TSweepedOutline3D.BaseOutlineChanged(O: TObject);
begin
  UpdateSolid;
  UpdateExtension(Self);
end;

procedure TSweepedOutline3D._UpdateExtension;
begin
  inherited;
  fBaseOutline.OnChange := nil;
  try
    fBaseOutline.UpdateExtension(Self);
    UpdateSolid;
  finally
    fBaseOutline.OnChange := BaseOutlineChanged;
  end;
end;

function  TSweepedOutline3D.HasTransform: Boolean;
begin
  if not Assigned(fBaseOutline) then
   Result := False
  else
   Result := fBaseOutline.HasTransform;
end;

function  TSweepedOutline3D.GetModelTransform: TTransf3D;
begin
  if not Assigned(fBaseOutline) then
   Result := NullTransf3D
  else
   Result := fBaseOutline.ModelTransform;
end;

procedure TSweepedOutline3D.SetModelTransform(Transf: TTransf3D);
begin
  if not Assigned(fBaseOutline) then
   Exit;
  fBaseOutline.ModelTransform := Transf;
end;

procedure TSweepedOutline3D.ApplyTransform;
begin
  fBaseOutline.ApplyTransform;
  UpdateExtension(Self);
end;

// In coordinate oggetto sia di BaseOutline che dell'oggetto stesso.
function TSweepedOutline3D.CreateSolidPolyface(PolyfaceClass: TPolyface3DClass): TRect3D;
var
  Iterations, NPts, NFaces, ProfPoints: Integer;
  Cont, ContFace, ContPoint: Integer;
  V1, V2: Integer;
  DS, NP: TVector3D;
  TmpS: TRealType;
  TmpProf: TPointsSet3D;
  TmpTransf: TTransf3D;
  TmpNeedToBeClosed: Boolean;
begin
  // Calcola i parametri di iterazione.
  fStartSweepDir := Versor3D(0, 0, 1);
  fEndSweepDir := Versor3D(0, 0, 1);
  Iterations := GetSweepIterations;
  // Comincia le iterazioni.
  fBaseOutline.BeginUseProfilePoints;
  try
    // Controllo se il profilo ha o meno il punto di chiusura esplicito (ossia il punto finale è lo stesso del punto iniziale)
    // Se il profilo è chiuso ma non ha il punto di chiusura esplicito, allora lo aggiungerò successivamente.
    with fBaseOutline do
     TmpNeedToBeClosed := IsClosed and
       not IsSamePoint3D(ProfilePoints[0], ProfilePoints[NumberOfProfilePts - 1]);
    if TmpNeedToBeClosed then
     ProfPoints := fBaseOutline.NumberOfProfilePts + 1
    else
     ProfPoints := fBaseOutline.NumberOfProfilePts;
    NFaces := (ProfPoints - 1) * Iterations;
    // Controlla la validità
    if ProfPoints < 2 then
     Exit;
    // Conta il numero di faccie aggiunte.
    ContFace := 0;
    // Calcola il numero totale di punti da aggiungere alla polyface.
    NPts := ProfPoints * (Iterations + 1);
    // Ricrea la polyface per sicurezza.
    fPolyface.Free;
    fPolyface := PolyfaceClass.Create(0, NPts, NFaces, [Point3D(0, 0, 0)]);
    // Crea il profilo delle iterazioni. Alla fine contiene il profilo finale.
    TmpProf := TPointsSet3D.Create(ProfPoints);
    // Crea il profilo iniziale della prima iterazione.
    TmpProf.Copy(fBaseOutline.ProfilePoints, 0, fBaseOutline.NumberOfProfilePts - 1);
    if TmpNeedToBeClosed then
     TmpProf.Add(fBaseOutline.ProfilePoints[0]);
    // Aggiunge i punti alla polyface.
    fPolyface.Points.DisableEvents := True;
    fFinalTransf := IdentityTransf3D;
    try
      fPolyface.Points.Clear;
      // Per prima cosa metto il profilo base.
      TmpTransf := GetSweepTransform(0);
      TmpProf.TransformPoints(TmpTransf);
      fFinalTransf := MultiplyTransform3D(fFinalTransf, TmpTransf);
      fPolyface.Points.Copy(TmpProf, 0, ProfPoints - 1);
      // Iterazioni.
      for Cont := 0 to Iterations - 1 do
       begin
         // Trasformo il profilo.
         TmpTransf := GetSweepTransform(Cont + 1);
         TmpProf.TransformPoints(TmpTransf);
         fFinalTransf := MultiplyTransform3D(fFinalTransf, TmpTransf);
         // Calcolo la normale del profilo.
         NP := GetVectNormal(TmpProf.PointsReference, ProfPoints);
         // Aggiungo i punti trasformati.
         fPolyface.Points.AddPoints(TmpProf);
         // Aggiungo le faccie.
         for ContPoint := 0 to ProfPoints - 2 do
          begin
            V1 := ContPoint;
            V2 := (ContPoint + 1) mod ProfPoints;
            // Calcolo la direzione di sweep per l'orientamento corretto della faccia.
            DS := Direction3D(fPolyface.Points[V1 + Cont * ProfPoints],
                              fPolyface.Points[V1 + (Cont + 1) * ProfPoints]);
            if VectorLength3D(DS) = 0 then
             DS := Direction3D(fPolyface.Points[V2 + Cont * ProfPoints],
                               fPolyface.Points[V2 + (Cont + 1) * ProfPoints])
            else
             begin
               if Cont = 0 then // La salvo per uso futuro.
                fStartSweepDir := DS;
               if Cont = Iterations - 1 then // La salvo per uso futuro.
                fEndSweepDir := DS;
             end;
            // Ora calcolo il parametro direzionale locale.
            TmpS := DotProduct3D(NP, DS);
            if ((TmpS > 0) and (GetHandleRuleForNormals = hrRightHand)) or
               ((TmpS < 0) and (GetHandleRuleForNormals = hrLeftHand)) then
             fPolyface.AddFace(ContFace,
                               V1 + Cont * ProfPoints + 1,
                               V2 + Cont * ProfPoints + 1,
                               V2 + (Cont + 1) * ProfPoints + 1,
                               V1 + (Cont + 1) * ProfPoints + 1)
            else
             fPolyface.AddFace(ContFace,
                               V1 + (Cont + 1) * ProfPoints + 1,
                               V2 + (Cont + 1) * ProfPoints + 1,
                               V2 + Cont * ProfPoints + 1,
                               V1 + Cont * ProfPoints + 1);
            Inc(ContFace);
          end;
      end;
    finally
      fPolyface.Points.DisableEvents := False;
      fPolyface.UpdateExtension(Self);
      Result := TransformBoundingBox3D(fPolyface.Box, ModelTransform);
    end;
  except
   fBaseOutline.EndUseProfilePoints;
   fPolyface.Free;
   fPolyface := nil;
  end;
end;

procedure TSweepedOutline3D.UpdateSolid;
begin
  WritableBox := CreateSolidPolyface(TPolyface3D);
end;

procedure TSweepedOutline3D.BeginUseProfilePoints;
begin
  fBaseOutline.BeginUseProfilePoints;
end;

procedure TSweepedOutline3D.EndUseProfilePoints;
begin
  fBaseOutline.EndUseProfilePoints;
end;

function TSweepedOutline3D.GetProfilePoints: TPointsSet3D;
begin
  Result := fBaseOutline.ProfilePoints;
end;

function TSweepedOutline3D.GetNPts: Integer;
begin
  Result := fBaseOutline.NumberOfProfilePts;
end;

function TSweepedOutline3D.GetIsClosed: Boolean;
begin
  Result := fBaseOutline.IsClosed;
end;

constructor TSweepedOutline3D.Create(ID: LongInt; const BaseProfile: TOutline3D);
begin
  if not Assigned(BaseProfile) then
   Raise ECADSysException.Create('TSweepedOutline3D: Invalid profile for TNewSweepedOutline3D');

  fBaseOutline := BaseProfile;
  inherited Create(ID, 0);
  fPoints.Free;
  fPoints := fBaseOutline.Points;
  fBaseOutline.OnChange := BaseOutlineChanged;
end;

destructor TSweepedOutline3D.Destroy;
begin
  fPoints := nil;
  fBaseOutline.Free;
  fPolyface.Free;
  inherited;
end;

procedure TSweepedOutline3D.Assign(const Obj: TGraphicObject);
var
  TmpClass: TGraphicObjectClass;
  TmpObj: TGraphicObject;
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if not (Obj is TSweepedOutline3D) then
   Exit;
  // Elimina il vecchio profilo
  fPoints := nil;
  fBaseOutline.Free;
  // Crea un nuovo profilo dello stesso tipo di quello di Obj.
  TmpClass := TGraphicObjectClass(TSweepedOutline3D(Obj).fBaseOutline.ClassType);
  TmpObj := TmpClass.Create(0);
  // Assegna il nuovo profilo
  fBaseOutline := TmpObj as TOutline3D;
  // Copia i dati per il nuovo profilo da Obj.
  fBaseOutline.Assign(TSweepedOutline3D(Obj).fBaseOutline);
  // Sistema il tutto
  fPoints := fBaseOutline.Points;
  fBaseOutline.OnChange := BaseOutlineChanged;
end;

constructor TSweepedOutline3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  OutlineBaseClass: TGraphicObjectClass;
  TmpWord: Word;
begin
  inherited;
  with Stream do
   begin
     TmpWord := 0;
     Read(TmpWord, sizeOf(TmpWord));
     OutlineBaseClass := CADSysFindClassByIndex(TmpWord);
     fBaseOutline := OutlineBaseClass.CreateFromStream(Stream, Version) as TOutline3D;
     fPoints := fBaseOutline.Points;
     fBaseOutline.UpdateExtension(Self);
     fBaseOutline.OnChange := BaseOutlineChanged;
   end;
end;

procedure TSweepedOutline3D.SaveToStream(const Stream: TStream);
var
  TmpWord: Word;
begin
  inherited;
  with Stream do
   begin
     TmpWord := CADSysFindClassIndex(fBaseOutline.ClassName);
     Write(TmpWord, sizeOf(TmpWord));
     fBaseOutline.SaveToStream(Stream);
   end;
end;

procedure TSweepedOutline3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  N: TVector3D;
begin
  if not Assigned(fBaseOutline) then
   Exit;
  // La polyface deve essere soggetta alla trasformazione modello
  if Assigned(fPolyface) then
   fPolyface.Draw(MultiplyTransform3D(ModelTransform, NormTransf), VRP, VT, Cnv, DrawMode);
  if Cnv.Canvas.Pen.Mode = pmXOr then
   Exit;
  // La baseOutline ha già la trasformazione modello
  if (DrawMode and DRAWMODE_BACKFACECULLING) = DRAWMODE_BACKFACECULLING then
   begin
     N := Vector3D(VRP, fBaseOutline.Points[0]);
     if DotProduct3D(N, fStartSweepDir) > 0 then
      // Disegno la faccia inferiore.
      fBaseOutline.Draw(NormTransf, VRP, VT, Cnv, DrawMode)
     else
      // Disegno la faccia superiore.
      fBaseOutline.Draw(MultiplyTransform3D(fFinalTransf, NormTransf), VRP, VT, Cnv, DrawMode);
   end
  else
   // Disegno la faccia inferiore.
   fBaseOutline.Draw(NormTransf, VRP, VT, Cnv, DrawMode);
end;

procedure TSweepedOutline3D.DrawControlPoints(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D;
                                                 const Cnv: TDecorativeCanvas; const Width: Integer);
begin
  // La baseOutline ha già la trasformazione modello
  if HasTransform then
   fBaseOutline.DrawControlPoints(NormTransf, VRP, VT, Cnv, Width)
  else
   fBaseOutline.DrawControlPoints(NormTransf, VRP, VT, Cnv, Width)
end;

function TSweepedOutline3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                                var Distance: TRealType): Integer;
begin
  Result := fBaseOutline.OnMe(P, N, Aperture, Distance);
  if (Result < PICK_ONOBJECT) and Assigned(fPolyface) then
   Result := MinIntValue([PICK_ONOBJECT, fPolyface.OnMe(P, MultiplyTransform3D(ModelTransform, N), Aperture, Distance)]);
end;

// FaceID è 1 per le faccie laterali e 0 per quelle di chiusura.
function TSweepedOutline3D.GetObjectFaces(var I: Integer; FacePts: TPointsSet3D; var FaceNormal: TVector3D; var FaceID: Integer): Boolean;
const
  ProfProcessed: Byte = 0;
var
  TmpI: Integer;
  N: TVector3D;
begin
  FacePts.Clear;
  if not Assigned(fPolyface) then
   begin
     FaceID := -1;
     Result := False;
     Exit;
   end;
  if I < fPolyface.NumOfFaces then
   begin
     Result := fPolyface.GetObjectFaces(I, FacePts, FaceNormal, FaceID);
     FaceID := 0;
   end
  else if ProfProcessed = 0 then // Basso
   begin
     TmpI := I - fPolyface.NumOfFaces;
     if fBaseOutline.GetObjectFaces(TmpI, FacePts, FaceNormal, FaceID) then
      begin
        // Controlla l'orientamento di fBaseOutline.
        N := GetVectNormal(FacePts.PointsReference, FacePts.Count);
        if ((DotProduct3D(N, fStartSweepDir) > 0) and (GetHandleRuleForNormals = hrRightHand)) or
           ((DotProduct3D(N, fStartSweepDir) < 0) and (GetHandleRuleForNormals = hrLeftHand)) then
         FaceNormal := Reflect3D(N)
        else
         FaceNormal := N;
        FacePts.FrontFace(FaceNormal);
        FacePts.TransformPoints(ModelTransform);
        Inc(I);
      end
     else
      begin
        I := fPolyface.NumOfFaces;
        Inc(ProfProcessed);
      end;
     FaceID := 1;
     Result := True;
   end
  else if ProfProcessed = 1 then // Alto
   begin
     TmpI := I - fPolyface.NumOfFaces;
     if fBaseOutline.GetObjectFaces(TmpI, FacePts, FaceNormal, FaceID) then
      begin
        FacePts.TransformPoints(fFinalTransf);
        // Controlla l'orientamento di fBaseOutline.
        N := GetVectNormal(FacePts.PointsReference, FacePts.Count);
        if ((DotProduct3D(N, fStartSweepDir) < 0) and (GetHandleRuleForNormals = hrRightHand)) or
           ((DotProduct3D(N, fStartSweepDir) > 0) and (GetHandleRuleForNormals = hrLeftHand)) then
         FaceNormal := Reflect3D(N)
        else
         FaceNormal := N;
        FacePts.FrontFace(FaceNormal);
        FacePts.TransformPoints(ModelTransform);
        Inc(I);
      end
     else
      begin
        I := fPolyface.NumOfFaces;
        Inc(ProfProcessed);
      end;
     FaceID := 2;
     Result := True;
   end
  else
   begin
     ProfProcessed := 0;
     FaceID := -1;
     Result := False;
   end;
end;

// =====================================================================
// TExtrudedOutline3D
// =====================================================================

function TExtrudedOutline3D.GetSweepIterations: Integer;
begin
  Result := 1;
end;

function TExtrudedOutline3D.GetSweepTransform(const NIter: Integer): TTransf3D;
begin
  if NIter = 0 then
   Result := IdentityTransf3D
  else
   Result := Translate3D(fExtrudeDir.X * fExtrudeLen, fExtrudeDir.Y * fExtrudeLen, fExtrudeDir.Z * fExtrudeLen);
end;

constructor TExtrudedOutline3D.Create(ID: LongInt; const Outline: TOutline3D; Len: TRealType; const ExtDir: TVector3D);
begin
  inherited Create(ID, Outline);
  fExtrudeDir := NormalizeVector3D(ExtDir);
  fExtrudeLen := Len;
  UpdateSolid;
end;

constructor TExtrudedOutline3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpVS: TVector3DSingle;
  TmpS: TRealTypeSingle;
begin
  inherited;
  with Stream do
   begin
     if( Version >= 'CAD423' ) then
      begin
        Read(fExtrudeDir, SizeOf(fExtrudeDir));
        Read(fExtrudeLen, SizeOf(fExtrudeLen));
      end
     else
      begin
        Read({%H-}TmpVS, SizeOf(TmpVS));
        fExtrudeDir.X := TmpVS.X;
        fExtrudeDir.Y := TmpVS.Y;
        fExtrudeDir.Z := TmpVS.Z;
        Read({%H-}TmpS, SizeOf(TmpS));
        fExtrudeLen := TmpS;
      end;
     UpdateSolid;
   end;
end;

procedure TExtrudedOutline3D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fExtrudeDir, SizeOf(fExtrudeDir));
     Write(fExtrudeLen, SizeOf(fExtrudeLen));
   end;
end;

procedure TExtrudedOutline3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TExtrudedOutline3D then
   begin
     fExtrudeDir := TExtrudedOutline3D(Obj).fExtrudeDir;
     fExtrudeLen := TExtrudedOutline3D(Obj).fExtrudeLen;
     UpdateSolid;
   end;
end;

// =====================================================================
// TRotationalOutline3D
// =====================================================================

function TRotationalOutline3D.GetSweepIterations: Integer;
begin
  if fDeltaAngle <> 0 then
   Result := Trunc((fEndAngle - fStartAngle) / fDeltaAngle)
  else
   Result := 0;
end;

function TRotationalOutline3D.GetSweepTransform(const NIter: Integer): TTransf3D;
begin
  if NIter = 0 then
   Result := IdentityTransf3D
  else
   Result := RotateOnAxis3D(fRotationCenter, fRotationAx, fDeltaAngle);
end;

constructor TRotationalOutline3D.Create(ID: LongInt; const Outline: TOutline3D; StartA, EndA, DA: TRealType; const RotAx: TVector3D; const RotC: TPoint3D);
begin
  inherited Create(ID, Outline);
  fRotationAx := NormalizeVector3D(RotAx);
  fRotationCenter := RotC;
  fStartAngle := StartA;
  fEndAngle := EndA;
  fDeltaAngle := DA;

  UpdateSolid;
end;

constructor TRotationalOutline3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpPtS: TPoint3DSingle;
  TmpVS: TVector3DSingle;
  TmpS: TRealTypeSingle;
begin
  inherited;
  with Stream do
   begin
     if( Version >= 'CAD423' ) then
      begin
        Read(fRotationCenter, SizeOf(fRotationCenter));
        Read(fRotationAx, SizeOf(fRotationAx));
        Read(fStartAngle, SizeOf(fStartAngle));
        Read(fEndAngle, SizeOf(fEndAngle));
        Read(fDeltaAngle, SizeOf(fDeltaAngle));
      end
     else
      begin
        Read({%H-}TmpPtS, SizeOf(TmpPtS));
        fRotationCenter.X := TmpPtS.X;
        fRotationCenter.Y := TmpPtS.Y;
        fRotationCenter.Z := TmpPtS.Z;
        fRotationCenter.W := TmpPtS.W;
        Read({%H-}TmpVS, SizeOf(TmpVS));
        fRotationAx.X := TmpVS.X;
        fRotationAx.Y := TmpVS.Y;
        fRotationAx.Z := TmpVS.Z;
        Read({%H-}TmpS, SizeOf(TmpS));
        fStartAngle := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fEndAngle := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fDeltaAngle := TmpS;
      end;
     UpdateSolid;
   end;
end;

procedure TRotationalOutline3D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fRotationCenter, SizeOf(fRotationCenter));
     Write(fRotationAx, SizeOf(fRotationAx));
     Write(fStartAngle, SizeOf(fStartAngle));
     Write(fEndAngle, SizeOf(fEndAngle));
     Write(fDeltaAngle, SizeOf(fDeltaAngle));
   end;
end;

procedure TRotationalOutline3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TRotationalOutline3D then
   begin
     fRotationAx := TRotationalOutline3D(Obj).fRotationAx;
     fRotationCenter := TRotationalOutline3D(Obj).fRotationCenter;
     fStartAngle := TRotationalOutline3D(Obj).fStartAngle;
     fEndAngle := TRotationalOutline3D(Obj).fEndAngle;
     fDeltaAngle := TRotationalOutline3D(Obj).fDeltaAngle;
     UpdateSolid;
   end;
end;

// =====================================================================
// TCameraObject3D
// =====================================================================

procedure SetCamerasViewport(V: TCADPerspectiveViewport3D);
begin
  fCamerasViewports := V;
end;

procedure TCameraObject3D.SetCameraPosition(P: TPoint3D);
begin
  fCameraPosition := P;
  UpdateExtension(Self);
end;

procedure TCameraObject3D.SetCameraViewPoint(P: TPoint3D);
begin
  fCameraViewPoint := P;
  UpdateExtension(Self);
end;

procedure TCameraObject3D.SetCameraUP(U: TVector3D);
begin
  fCameraUP := U;
  UpdateExtension(Self);
end;

procedure TCameraObject3D.SetAperture(A: TRealType);
begin
  fAperture := DegToRad(A);
  UpdateExtension(Self);
end;

function TCameraObject3D.GetAperture: TRealType;
begin
  Result := RadToDeg(fAperture);
end;

procedure TCameraObject3D.SetAspect(A: TRealType);
begin
  fAspect := A;
  UpdateExtension(Self);
end;

procedure TCameraObject3D.SetPlaneDistance(D: TRealType);
begin
  fPlaneDistance := D;
  UpdateExtension(Self);
end;

function TCameraObject3D.GetCameraPlanePosition: TPoint3D;
var
  BaseNorm: TVector3D;
begin
  BaseNorm := Direction3D(fCameraPosition, fCameraViewPoint);
  Result := ExtrudePoint3D(fCameraPosition, BaseNorm, fPlaneDistance);
end;

procedure TCameraObject3D.SetViewport(V: TCADViewport3D);
begin
  if V is TCADOrtogonalViewport3D then
   Raise ECADSysException.Create('TCameraObject3D: Ivalid viewport to link to a camera.');
  fViewport := V;
  if Assigned(fViewport) then
   UpdateExtension(Self);
end;

function TCameraObject3D.UpdateViewFrustum: TRect3D;
var
  XLen, YLen: TRealType;
  BaseNorm: TVector3D;
  BasePt: TPoint3D;
begin
  YLen := fPlaneDistance * Tan(fAperture / 2.0);
  XLen := YLen * fAspect;
  BaseNorm := Direction3D(fCameraPosition, fCameraViewPoint);
  BasePt := ExtrudePoint3D(fCameraPosition, BaseNorm, fPlaneDistance);
  fP1 := PlanePointToWorld3D(Point2D(-XLen, YLen), BaseNorm, fCameraUP, BasePt);
  fP2 := PlanePointToWorld3D(Point2D(XLen, YLen), BaseNorm, fCameraUP, BasePt);
  fP3 := PlanePointToWorld3D(Point2D(XLen, -YLen), BaseNorm, fCameraUP, BasePt);
  fP4 := PlanePointToWorld3D(Point2D(-XLen, -YLen), BaseNorm, fCameraUP, BasePt);
  Result.FirstEdge := fP1;
  Result.SecondEdge := fP1;
  Result := PointOutBox3D(fP2, Result);
  Result := PointOutBox3D(fP3, Result);
  Result := PointOutBox3D(fP4, Result);
  Result := PointOutBox3D(fCameraPosition, Result);
  if Assigned(fViewport) and (Abs(DotProduct3D(BaseNorm, fCameraUP)) <> 1.0) then
   begin
     fViewport.BeginUpdate;
     try
       if fViewport is TCADPerspectiveViewport3D then
        TCADPerspectiveViewport3D(fViewport).PlaneDistance := fPlaneDistance;
       // Leggermente dietro in modo da non vederlo nella vista prospettica.
       BasePt := ExtrudePoint3D(fCameraPosition, BaseNorm, fPlaneDistance * 1.01);
       fViewport.SetCamera(BasePt, fCameraViewPoint, fCameraUP);
       if fViewport is TCADPerspectiveViewport3D then
        TCADPerspectiveViewport3D(fViewport).SetFieldOfView(fAperture, 1);
     finally
       fViewport.EndUpdate;
     end;
   end;
end;

procedure TCameraObject3D.BeginUpdate;
begin
  fUpdating := True;
end;

procedure TCameraObject3D.EndUpdate;
begin
  fUpdating := False;
  UpdateExtension(Self);
end;

constructor TCameraObject3D.Create(ID: LongInt; const CamPos, CamView: TPoint3D; const CamUP: TVector3D; const Ap, PDist: TRealType);
begin
  inherited Create(ID, 2);

  fCameraPosition := CamPos;
  fCameraViewPoint := CamView;
  fCameraUP := CamUP;
  fUpdating := False;
  fAperture := DegToRad(Ap);
  fAspect := 1.0;
  fPlaneDistance := PDist;
  fViewport := fCamerasViewports;
  Points.DisableEvents := True;
  try
    Points[0] := CamPos;
    Points[1] := CamView;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
  end;
  UpdateExtension(Self);
end;

constructor TCameraObject3D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpPtS: TPoint3DSingle;
  TmpVS: TVector3DSingle;
  TmpS: TRealTypeSingle;
begin
  inherited;
  with Stream do
   begin
     if( Version >= 'CAD423' ) then
      begin
        Read(fCameraPosition, SizeOf(fCameraPosition));
        Read(fCameraViewPoint, SizeOf(fCameraViewPoint));
        Read(fCameraUP, SizeOf(fCameraUP));
        Read(fAperture, SizeOf(fAperture));
        Read(fPlaneDistance, SizeOf(fPlaneDistance));
        Read(fAspect, SizeOf(fAspect));
      end
     else
      begin
        Read({%H-}TmpPtS, SizeOf(TmpPtS));
        fCameraPosition.X := TmpPtS.X;
        fCameraPosition.Y := TmpPtS.Y;
        fCameraPosition.Z := TmpPtS.Z;
        fCameraPosition.W := TmpPtS.W;
        Read(TmpPtS, SizeOf(TmpPtS));
        fCameraViewPoint.X := TmpPtS.X;
        fCameraViewPoint.Y := TmpPtS.Y;
        fCameraViewPoint.Z := TmpPtS.Z;
        fCameraViewPoint.W := TmpPtS.W;
        Read({%H-}TmpVS, SizeOf(TmpVS));
        fCameraUP.X := TmpVS.X;
        fCameraUP.Y := TmpVS.Y;
        fCameraUP.Z := TmpVS.Z;
        Read({%H-}TmpS, SizeOf(TmpS));
        fAperture := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fPlaneDistance := TmpS;
        Read(TmpS, SizeOf(TmpS));
        fAspect := TmpS;
      end;
   end;
  fViewport := fCamerasViewports;
end;

procedure TCameraObject3D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fCameraPosition, SizeOf(fCameraPosition));
     Write(fCameraViewPoint, SizeOf(fCameraViewPoint));
     Write(fCameraUP, SizeOf(fCameraUP));
     Write(fAperture, SizeOf(fAperture));
     Write(fPlaneDistance, SizeOf(fPlaneDistance));
     Write(fAspect, SizeOf(fAspect));
   end;
end;

procedure TCameraObject3D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if Obj is TCameraObject3D then
   begin
     fCameraPosition := TCameraObject3D(Obj).fCameraPosition;
     fCameraViewPoint := TCameraObject3D(Obj).fCameraViewPoint;
     fCameraUP := TCameraObject3D(Obj).fCameraUP;
     fViewport := TCameraObject3D(Obj).fViewport;
     fAperture := TCameraObject3D(Obj).fAperture;
     fPlaneDistance := TCameraObject3D(Obj).fPlaneDistance;
     Points.DisableEvents := True;
     try
       Points[0] := TCameraObject3D(Obj).Points[0];
       Points[1] := TCameraObject3D(Obj).Points[1];
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
     end;
   end;
end;

procedure TCameraObject3D.Draw(const NormTransf: TTransf3D; const VRP: TPoint3D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
var
  ViewCPt: TPoint3D;
begin
  DrawLine3D(Cnv, fP1, fP2, NormTransf, VT);
  DrawLine3D(Cnv, fP2, fP3, NormTransf, VT);
  DrawLine3D(Cnv, fP3, fP4, NormTransf, VT);
  DrawLine3D(Cnv, fP4, fP1, NormTransf, VT);
  DrawLine3D(Cnv, fCameraPosition, fP1, NormTransf, VT);
  DrawLine3D(Cnv, fCameraPosition, fP2, NormTransf, VT);
  DrawLine3D(Cnv, fCameraPosition, fP3, NormTransf, VT);
  DrawLine3D(Cnv, fCameraPosition, fP4, NormTransf, VT);
  Cnv.Canvas.Pen.Style := psDot;
  SetBkMode(Cnv.Canvas.Handle, TRANSPARENT);
  ViewCPt := ExtrudePoint3D(fCameraPosition,
                            Direction3D(fCameraPosition, fCameraViewPoint),
                            fPlaneDistance * 2);
  DrawLine3D(Cnv, fCameraPosition, ViewCPt, NormTransf, VT);
end;

function TCameraObject3D.OnMe(P: TPoint3D; const N: TTransf3D; Aperture: TRealType;
                              var Distance: TRealType): Integer;
begin
  Result := inherited OnMe(P, N, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     if IsPointOnLine3D(fP1, fP2, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT
     else if IsPointOnLine3D(fP2, fP3, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT
     else if IsPointOnLine3D(fP3, fP4, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT
     else if IsPointOnLine3D(fP4, fP1, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT
     else if IsPointOnLine3D(fCameraPosition, fP1, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT
     else if IsPointOnLine3D(fCameraPosition, fP2, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT
     else if IsPointOnLine3D(fCameraPosition, fP3, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT
     else if IsPointOnLine3D(fCameraPosition, fP4, P, Distance, Aperture, N) > PICK_NOOBJECT then
      Result := PICK_ONOBJECT;
   end;
end;

procedure TCameraObject3D._UpdateExtension;
begin
  fCameraPosition := TransformPoint3D(Points[0], ModelTransform);
  fCameraViewPoint := TransformPoint3D(Points[1], ModelTransform);
  inherited;
  if not fUpdating then
   WritableBox := UpdateViewFrustum;
end;

initialization
  // Spostata inizializzazione da CADSys4 a CS4Shapes perchè pare che venga fatta prima questa inizializzazione e poi quella di CADSys4
  CADSysInitClassRegister;

  CADSysRegisterClass(0, TContainer2D);
  CADSysRegisterClass(1, TSourceBlock2D);
  CADSysRegisterClass(2, TBlock2D);
  CADSysRegisterClass(3, TLine2D);
  CADSysRegisterClass(4, TPolyline2D);
  CADSysRegisterClass(5, TPolygon2D);
  CADSysRegisterClass(6, TRectangle2D);
  CADSysRegisterClass(7, TArc2D);
  CADSysRegisterClass(8, TEllipse2D);
  CADSysRegisterClass(9, TFilledEllipse2D);
  CADSysRegisterClass(10, TText2D);
  CADSysRegisterClass(11, TFrame2D);
  CADSysRegisterClass(12, TBitmap2D);
  CADSysRegisterClass(13, TBSpline2D);
  CADSysRegisterClass(14, TJustifiedVectText2D);

  _DefaultHandler2D := TPrimitive2DHandler.Create(nil);
  CADSysRegisterClass(50, TContainer3D);
  CADSysRegisterClass(51, TSourceBlock3D);
  CADSysRegisterClass(52, TBlock3D);
  CADSysRegisterClass(53, TLine3D);
  CADSysRegisterClass(54, TPolyLine3D);
  CADSysRegisterClass(55, TFace3D);
  CADSysRegisterClass(56, TArc3D);
  CADSysRegisterClass(57, TEllipse3D);
  CADSysRegisterClass(58, TPlanar2DObject3D);
  CADSysRegisterClass(60, TPlanarPolyline3D);
  CADSysRegisterClass(61, TFrame3D);
  CADSysRegisterClass(62, TPlanarSpline3D);
  CADSysRegisterClass(63, TPlanarFace3D);
  CADSysRegisterClass(64, TExtrudedOutline3D);
  CADSysRegisterClass(65, TRotationalOutline3D);
  CADSysRegisterClass(66, TMesh3D);
  CADSysRegisterClass(67, TCameraObject3D);
  CADSysRegisterClass(68, TPolyface3D);
  CADSysRegisterClass(69, TJustifiedVectText3D);
  CADSysRegisterClass(70, TPlanarFieldGrid3D);

  _DefaultHandler3D := TPrimitive3DHandler.Create(nil);
  // Vectorial fonts
  CADSysInitFontList;

  _NullChar := TVectChar.Create(1);
  _NullChar.Vectors[0].Add(Point2D(0.0, 0.0));
  _NullChar.Vectors[0].Add(Point2D(0.8, 0.0));
  _NullChar.UpdateExtension(nil);
  _DefaultFont := nil;
finalization
  CADSysClearFontList;
  _NullChar.Free;
  _DefaultHandler2D.Free;
  _DefaultHandler3D.Free;
end.

