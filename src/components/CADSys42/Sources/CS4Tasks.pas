{: This help file explain all the interaction task classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Tasks unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.

   See also <See Class=TCADPrg><BR>

   The task classes defined here can be used in your program by
   adding a <See Class=TCADPrg> component of the desired type
   (2D or 3D) and using the following code to start a task:

   <CODE=
    ACADPrg.StartOperation(<<A task class>>, <<The required task parameter>>);
   >

   If you want to stop a task use the following code:

   <CODE=
    ACADPrg.SendUserEvent(CADPRG_CANCEL);
   >

   and to finish the current task use the code:

   <CODE=
    ACADPrg.SendUserEvent(CADPRG_ACCEPT);
   >

   You may also start another task by suspending the current
   one (if it can be suspended) with the code:

   <CODE=
    ACADPrg.SuspendOperation(<<A task class>>, <<The required task parameter>>);
   >

   <B=Note>: All the 3D tasks work on the active
   <See=working plane@WORKPLANE> of the <See Class=TCADPrg3D>.
}
unit CS4Tasks;

{$mode delphi}

Interface

uses SysUtils, Classes, Graphics, Dialogs, Types,
     CADSys4, CS4BaseTypes, CS4Shapes, LCLType;

type
  { ******************* Zooming states *********************** }

  {: This class rapresents the parameter for zooming tasks.

     All the zooming tasks may use an optional parameter that
     is useful only if you want to start an interaction task
     after the zooming task.
  }
  TCADPrgZoomParam = class(TCADPrgParam);

  {: This is the base class for all zooming and panning
     operations.

     Because it is an abstract class it cannot be used as an
     operation. It is only defined to give a
     common interface for zooming tasks.

     See also <See Class=TCADPrg>.
  }
  TCADPrgZoomState = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class can be used to perform a windowed zoom.

     The operation waits two points from the user that are
     the two window's corner of the area to be zoommed.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button
     is not used.

     To stop the operation you can use either the
     <See Method=TCADPrg@StopOperation> method or send the
     <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgZoomArea = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class can be used to perform a <I=continuos> zoom in/zoom out.

     The operation waits for the zooming center point. If the
     shift key is hold while pressing the mouse left button, a
     zoom out will be performed, otherwise a zoom in will be done.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button is
     not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot
     be suspended.
  }
  TCADPrgZoomInOut = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
  end;

  {: This class can be used to perform a single pan operation.

     The operation waits two points from the user that are the
     start and ending point of the panning. After the selection
     of the two points, the current <I=visual rect> will be
     translated from the start point to the end point.

     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button
     is not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgPan = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class can be used to perform a dynamic pan operation.

     The operation waits for the mouse to be pressed. Then,
     while holding the mouse left button, the current
     <I=visual rect> will follow the mouse position. If the
     draws is complex and the painting thread is not enabled the
     panning can be delayed with respect to the mouse movements.

     The right mouse button is not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message. The operation doesn't end
     by itself but you have to send a <I=CADPRG_ACCEPT> message.

     The operation doesn't require any parameter and cannot be
     suspended.
  }
  TCADPrgRealTimePan = class(TCADPrgZoomState)
  private
    fInPanning: Boolean;
    fLastPoint: TPoint2D;
    fOriginalRect: TRect2D;
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
  end;

  { ******************* Useful states *********************** }

  {: This is the base class for the selection operations.

     Because it is an abstract class it cannot be used as an
     operation.

     See also <See Class=TCADPrg>.
  }
  TCADPrgSelectionState = class(TCADState);

  {: This class defines the parameter needed by the
     <See Class=TCADPrgSelectArea> task.

     The parameter is used to store the area selected by the
     user.
  }
  TCADPrgSelectAreaParam = class(TCADPrgParam)
  private
    fFrame: TFrame2D;
    fCallerParam: TCADPrgParam;
    function GetArea: TRect2D;
  public
    {: This constructor creates the instance of the parameter
       to be passed to <See Method=TCADPrg@StartOperation> or
       <See Method=TCADPrg@SuspendOperation>.

       <I=AfterS> contains the starting state of the operation
       that can be started at the end of the selection. If it
       is <B=nil>, the CADPrg will returns into the default
       state.

       <I=CallerParam> may contains an optional parameter that
       can be used by the operation that will receive the selection.
       This parameter will be freed when the parameter will be
       deleted. If you need it after the deletion of the
       parameter set it to <B=nil> after you have retrieved it.
    }
    constructor Create(AfterS: TCADStateClass; CallerParam: TCADPrgParam);
    destructor Destroy; override;
    {: This property contains an optional parameter that can
       be used by the operation that will recive the selection made.

       This parameter will be freed when the parameter will be
       deleted. If you need it after the deletion of the
       parameter set it to <B=nil> after you have retrieved it.
    }
    property CallerParam: TCADPrgParam read fCallerParam write fCallerParam;
    {: It will contains the area selected by the user.
    }
    property Area: TRect2D read GetArea;
  end;

  {: This task class allows the user to select a 2D area defined
     in viewport coordinates.

     It can be used to obtain a rectangular parameter usable in
     selections or other operations.

     The operation waits for two points from the user that are
     the two area corner.
     The operation ends after the user has pressed the left
     mouse button for the second time. The right mouse button is
     not used.

     To stop the operation you can use either
     <See Method=TCADPrg@StopOperation> or
     send the <I=CADPRG_CANCEL> message.

     The operation requires a <See Class=TCADPrgSelectAreaParam>
     parameter.

     See also <See Class=TCADPrg>.
  }
  TCADPrgSelectArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD2DCommonParam = class(TCADPrgParam);

  {: This class defines the parameter used by the
     <See Class=TCAD2DPositionObject> task.

     The class is used to store the 2D object that must be
     positioned on the CAD. If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DPositionObjectParam = class(TCAD2DCommonParam)
  private
    fObject: TObject2D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=O> contains the object to be positioned with the
        <See Class=TCAD2DPositionObject> task.>

        <B=Note>: in the case <I=AfterS> is not <B=nil>, the
         object will not be added to the CAD.
    }
    constructor Create(AfterS: TCADStateClass; O: TObject2D);
    {: This property contains the object to be positioned
       (or already positioned when the task is finished) with the
       <See Class=TCAD2DPositionObject> task.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TCADCmp>,
       otherwise it will be added to it.
    }
    property Obj: TObject2D read fObject;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DDrawUnSizedPrimitive> to construct a 2D
     primitive with a variable number of <I=control points>,
     like a polyline.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DDrawUnSizedPrimitiveParam = class(TCAD2DCommonParam)
  private
    fPrimObject: TPrimitive2D;
    fCurrPoint: Word;
    fOrtoIsUsable: Boolean;
  protected
    {: This method updates the on-screen informations during
       the task.

       The method draws the current primitive with the rubber
       band (xor pen mode) mode. You will see your primitive
       growing as new points are added to it.
    }
    procedure DrawOSD(Viewport: TCADViewport2D);
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 2D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal to
        3, when the user click on the viewport, the fourth control
        point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any means with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
    {: This property contains the primitive being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TCADCmp>,
       otherwise it will be added to it.
    }
    property Primitive: TPrimitive2D read fPrimObject;
    {: This property indicates if the ortogonal constraint has
       any means with the primitive being defined.

       If it is <B=True>, the orto constraint will be used,
       otherwise it will not used.
    }
    property OrtoIsUsable: Boolean read fOrtoIsUsable write fOrtoIsUsable;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DDrawSizedPrimitive> to construct a 2D
     primitive with a fixed number of points, like an ellipse.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DDrawSizedPrimitiveParam = class(TCAD2DDrawUnSizedPrimitiveParam)
  private
    fnPoints: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 2D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal
        to 3 when the user click on the viewport, the fourth
        control point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any meanse with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
  end;

  {: This class implements the <I=object positioning task>.

     This task may be used to add an object to the linked CAD
     by firstly positioning it in the world.

     The operation wait for the user to do the following
     (in the order given here):

     <LI=move the object to the desired position using the
      mouse. You will see the object moving on the screen.>
     <LI=press the left mouse button on that point to lay down
      the object.>

     The object will be moved using its bottom-left bounding box
     corner as the base point. At the end of the task the object
     is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DPositionObjectParam> class. The task may
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DPositionObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a fixed number of control points.

     This task may be used to add an object to the linked CAD by
     firstly defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the
      parameter is greater than zero the control point to be
      positioned is not the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second step until no
      control points are left.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
     added to the CAD.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawSizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a variable number of control points.

     This task may be used to add an object to the linked CAD
     by firstly defining its control points.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the parameter
      is greater than zero the control point to be positioned is not
      the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.
     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD. Note that this is the only way to end
      the task.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawUnSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawUnSizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by the
     <See Class=TCAD2DDrawArcPrimitive> task to construct a
     2D arc segment.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD2DDrawArcPrimitiveParam = class(TCAD2DCommonParam)
  private
    fArcObject: TArc2D;
    fCurrPoint: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state. Note that in case
        <I=AfterS> is not <B=nil>, the object will not be added to
        the CAD.>
       <LI=<I=Arc> contains the object to be constructed with
        the <See Class=TCAD2DDrawArcPrimitive> task.>
    }
    constructor Create(AfterS: TCADStateClass; Arc: TArc2D);
    {: This property contains the 2D arc that is being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.
    }
    property Arc: TArc2D read fArcObject;
  end;

  {: This class implements the <I=arc constructing task>.

     This task may be used to add an arc to the linked CAD by
     defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the desired position for the first
      control point of the arc.>
     <LI=press the left mouse button to set the first control
      point on that point.>
     <LI=move the mouse on the desired position for the second
      control point. You will see an ellipse drawed on the
      viewport.>
     <LI=press the left mouse button to set the second control
      point.>
     <LI=move the mouse on the desired position for the third
      control point. You will modify the starting angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the third control
      point.>
     <LI=move the mouse on the desired position for the fourth
      control point. You will modify the ending angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the fourth control
      point.>

     At the end of the task the arc is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD.>
     <LI=CADPRG_CANCEL>. The task is aborted and the object
     destroyed. The CADPrgreturns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD2DDrawArcPrimitiveParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DDrawArcPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD2DSelectObjectsParam = class;

  {: This type defines the prototype for an event handler used
     to inform the application that an object was picked.

     See <See Class=TCAD2DSelectObject>,
     <See Class=TCAD2DSelectObjects> and
     <See Class=TCAD2DSelectObjectsInArea> for details.

     Parameters:

     <LI=<I=Sender> contains the instance of
      <See Class=TCAD2DSelectObjectsParam> (or
      <See Class=TCAD2DSelectObjectsInAreaParam>) of the task
      that had called the handler.>
     <LI=<I=Obj> contains the picked object, or <B=nil> if the
      selection task is <See Class=TCAD2DSelectObjectsInArea>.>
     <LI=<I=CtrlPt> contains the control point on which the
      mouse was picked. This is the same result of the
      <See Method=TCADViewport2D@PickObject> method. This
      parameter will be <I=PICK_NOOBJECT> in case the selection
      task is <See Class=TCAD2DSelectObjectsInArea>.>
     <LI=<I=Added> is <B=True> if Obj is added to the selected
      object list, or <B=False> if it is removed.>

     If a repaint event is raised, the handler is called for all
     the picked objects. In this case the <I=CtrlPt> is
     <I=PICK_NOOBJECT> and <I=Added> is <B=True> the selection
     task that fired the event is
     <See Class=TCAD2DSelectObjectsInArea>.
  }
  TSelection2DEvent = procedure(Sender: TCAD2DSelectObjectsParam; Obj: TObject2D; CtrlPt: Integer; Added: Boolean) of object;

  {: This class defines the parameter used by
     <See Class=TCAD2DSelectObject> and
     <See Class=TCAD2DSelectObjects> to pick objects
     interactively on the screen.
  }
  TCAD2DSelectObjectsParam = class(TCAD2DCommonParam)
  private
    fApertureSize: Word;
    fEndIfNoObject, fEndWithMouseDown: Boolean;
    fLastSelectedCtrlPoint: Integer;
    fLastPt: TPoint2D;
    fSelectedObjs: TGraphicObjList;
    fOnSelected: TSelection2DEvent;
    fSelectionFilter: TObject2DClass;
  protected
    {: This method draws the picking frame used to show the
       aperture of the picking.

       The frame is drawed in xor pen mode and centered at <I=Pt>.

       Parameters:

       <LI=<I=Viewport> is the viewport on which draw the frame.>
       <LI=<I=Pt> is the point at which draw the frame.>
    }
    procedure DrawOSD(Viewport: TCADViewport2D; const Pt: TPoint2D); virtual;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=ApertureSize> is the aperture used for the picking
        in pixels.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.
    }
    constructor Create(ApertureSize: Word; const AfterS: TCADStateClass);
    destructor Destroy; override;
    {: This property is the list that contains the picked
       objects.

       If you want to traverse the list ask for an iterator and
       remember to free it before to returns to the selection
       task.

       See also <See Class=TGraphicObjList>.
    }
    property SelectedObjects: TGraphicObjList read fSelectedObjs;
    {: This property contains the control point selected of the
       last picked object.
    }
    property LastSelectedCtrlPoint: Integer read fLastSelectedCtrlPoint;
    {: This property may contain a class reference type (deriving
       from <See Class=TObject2D>) used to filter the selection.

       If the picked object doesn't derive from that class, the
       object is ignored.

       By default it is <I=TObject2D>.
    }
    property SelectionFilter: TObject2DClass read fSelectionFilter write fSelectionFilter;
    {: This property specifies if the selection task must be ended
       when the <I=mouse down> event is received.

       If it is <I=True> the task is finished as soon as the
       user press the mouse. Otherwise the task will finish when
       the user release the mouse button.

       By default it is <B=False>.
    }
    property EndSelectionWithMouseDown: Boolean read fEndWithMouseDown write fEndWithMouseDown;
    {: This property specifies if the selection task must be
       ended when the user has pressed the mouse button but
       not object is under the mouse.

       By default it is <B=False>.

       <B=Note>: This property is meaningfull only with multiple
        selection.
    }
    property EndSelectionIfNoObject: Boolean read fEndIfNoObject write fEndIfNoObject;
    {: EVENTS}
    {: This property may contains an event handler that will be
      called when an object is picked (after it was added to the
      list).

      See Also <See Type=TSelection2DEvent>.
    }
    property OnObjectSelected: TSelection2DEvent read fOnSelected write fOnSelected;
  end;

  {: This class implements the <I=single object selection task>.

     This task may be used to select an object of the linked CAD.
     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>

     The object will be added to the
     <See Property=TCAD2DSelectObjectsParam@SelectedObjects> list
     of the task parameter. Normally you set <I=AfterS> of the
     task parameter to a state that will process the selected
     object by using the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=multiple object selection task>.

     This task may be used to select a set of object of the
     linked CAD.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object. If the object was already picked, it is removed
      from <See Property=TCAD2DSelectObjectsParam@SelectedObjects>
      list of the task parameter, otherwise it will be added.>
     <LI=continue with the first step.>

     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     Note that no visual feedback is given to the user. If you
     want to show the selected objects, you can use the
     <See Property=TCAD2DSelectObjectsParam@OnObjectSelected>
     handler of the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>
     <LI=<I=CADPRG_ACCEPT>. The task is ended. The CADPrg
     returns in the default state or in the state specified by
     <I=AfterS>. Note that this is the only way to finish the
     task.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines a special kind of selection task that
     is the combination of the
     <See Class=TCAD2DSelectObject> and
     <See Class=TCAD2DSelectObjects> tasks.

     If the user holds down the Shift key, the task behaves
     like the <I=TCAD2DSelectObjects> task, otherwise it
     behaves like the <I=TCAD2DSelectObject> task.

     See also <See Class=TCADPrg>.
  }
  TCAD2DExtendedSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD2DSelectObjectsInArea> task to select the
     objects contained in the specified window area.
  }
  TCAD2DSelectObjectsInAreaParam = class(TCAD2DSelectObjectsParam)
  private
    fAreaMode: TGroupMode;
    fArea: TRect2D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AreaMode> specify the type of selection. If it is
        <I=gmAllInside> only the objects fully contained in the
        area are selected; if it is <I=gmCrossFrame> all the
        objects that are contained or cross the area are selected.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is nil, the
        CADPrg will return to the default state.>
    }
    constructor Create(AreaMode: TGroupMode; const AfterS: TCADStateClass);
  end;

  {: This class implements <I=the 'area selection task>.

     This task may be used to select a set of object of
     the linked CAD by specify a rectangle frame.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the first corner of the area.>
     <LI=press the left mouse button to accept the point.>
     <LI=move the mouse on the second corner of the area. You
      will see the area being defined on the screen.>
     <LI=press the left mouse button to accept the point.>

     All the objects in the area are selected and stored in the
     <See Property=TCAD2DSelectObjectsParam@SelectedObjects>
     list of the task parameter.
     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD2DSelectObjectsInAreaParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DSelectObjectsInArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by the
     <I=object transformation task>.

     All the transformations of objects that may be described
     by a matrix transform that accept as parametesr a base
     point and a moving point can be modelled by this task.
     You only need to derive from this parameter and
     redefine the
     <See Method=TCAD2DTransformObjectsParam@GetTransform> method.
     Then you can pass the new parameter to
     <See Class=TCAD2DTransformObjects> task.
  }
  TCAD2DTransformObjectsParam = class(TCAD2DCommonParam)
  private
    fBasePt: TPoint2D;
    fNPoint: Integer;
    fBox: TRect2D;
    fObjs: TGraphicObjList;
    fUseFrame: Boolean;
    fCurrTransf: TTransf2D;

    procedure TransformObjs(CurrPt: TPoint2D; UseOrto: Boolean);
    procedure ConfirmTransform;
    procedure CancelTransform;
  protected
    {: This method draws the bounding box of the set of objects
       to be transformed.

       It is used by the <See Method=TCAD2DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithFrame(Viewport: TCADViewport2D); dynamic;
    {: This method draws the objects to be transformed in
       rubber band mode (xor pen mode).

       It is used by the <See Method=TCAD2DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithoutFrame(Viewport: TCADViewport2D); dynamic;
    {: This is the key method of the class.

       It must return the matrix transform that define the
       transformation of the objects. The returned matrix will
       override the current model transform for the selected
       objects.

       This method must be redefined in the derived classes
       for specific transformations.

       Parameters:

       <LI=<I=BasePt> is the base point for the transformation.
        For example to rotate an object you must give the center
        of rotation; to move an object you must give the first
        point of the translation.>
       <LI=<I=CurrPt> is the current point of the mouse. You
        may use this point to define the current transformation.
        For example to rotate an object you must give a second
        point, so you are able to find the angle of rotation
        with respect to the <I=BasePt>; to move an object this
        point is the second point of the translation.>
    }
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D; virtual; abstract;
    {: This method draws the on screen informations that informs
       the user of the result of the transformation.

       There are two modes of visualization:

       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawOSD(Viewport: TCADViewport2D);
    {: This property contains the base point for the
       transformation.
    }
    property BasePoint: TPoint2D read fBasePt;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=Objs> is a list that contains the objects to be
        transformed. The list must have the
        <See Property=TGraphicObjList@FreeOnClear> property set
        to <B=False>.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.>
    }
    constructor Create(Objs: TGraphicObjList; const AfterS: TCADStateClass);
    destructor Destroy; override;
    {: This property contains the list of the objects to be
       transformed.
    }
    property Objects: TGraphicObjList read fObjs;
    {: This property selects the visualization mode for the on
       screen informations:

       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD2DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       When the parameter is constructed, this property is set
       to <B=False> if the passed list of objects has only one
       object, it is set to <B=True> otherwise.>
    }
    property UseFrame: Boolean read fUseFrame write fUseFrame;
  end;

  {: This class defines the method
     <See Method=TCAD2DTransformObjectsParam@GetTransform> for
     the <I=move object task>.
  }
  TCAD2DMoveObjectsParam = class(TCAD2DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D; override;
  end;

  {: This class defines the method
     <See Method=TCAD2DTransformObjectsParam@GetTransform> for
     the <I=rotate object task>.
  }
  TCAD2DRotateObjectsParam = class(TCAD2DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D; override;
  end;

  {: This class implements the <I=transform objects task>.

     This task may be used to apply a transformation to a set
     of objects by specifing the appropriate parameter (see below).

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the transformation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      object transformed on the screen.>
     <LI=press the left mouse button to accept the current
      transformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of a class derived from
     <See Class=TCAD2DTransformObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DTransformObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=move a selection task>.

     This task may be used to select and move a set of objects
     by specifing the start point and end point of the
     translation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the translation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      objects moving on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD2DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DMoveSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=rotate a selection task>.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of rotation.>
     <LI=press the left mouse button to accept the center of
      rotation.>
     <LI=move the mouse on the second point. You will see the
      objects rotating on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD2DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DRotateSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=edit primitive task>.

     This task may be used to move the control points of a
     <See Class=TPrimitive2D> interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the new
      position of the control point.>
     <LI=continue from the first step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=CADPRG_CANCEL>. The task is aborted and the parameter
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the of the primitive
     incapsulated into the <See Property=TCADPrgParam@UserObject>
     property of a <See Class=TCADPrgParam> instance.

     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DEditPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=select and edit primitive task>.

     This task may be used to select a primitive and move its
     control points interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>
     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the
      new position of the control point.>
     <LI=continue from the third step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of
     <See Class=TCAD2DSelectObjectsParam>.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD2DEditSelectedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD3DCommonParam = class(TCADPrgParam);

  {: This class defines the parameter used by
     <See Class=TCAD3DSelectSegment>.

     It contains the 3D line segment defined by the user.
  }
  TCAD3DSelectSegmentParam = class(TCAD3DCommonParam)
  private
    fLine: TLine3D;
    fCurrPoint: Word;

    function GetStart: TPoint3D;
    function GetEnd: TPoint3D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters.>
    }
    constructor Create(AfterS: TCADStateClass);
    destructor Destroy; override;
    {: This property contains the starting point of the segment.
    }
    property StartPoint: TPoint3D read GetStart;
    {: This property contains the ending point of the segment.
    }
    property EndPoint: TPoint3D read GetEnd;
  end;

  {: This class implements the <I=select segment task>.

     This task may be used to define a 3D line segment on the
     screen, by specify the starting and ending points.

     The operation waits for the user to do the following
     (in the order given here):

     <LI=move the mouse to the desired starting position.>
     <LI=press the left mouse button on that point to confirm
      the starting point of the segment.>
     <LI=move the mouse to the desired ending position. You will
      see the segment on the screen.>
     <LI=press the left mouse button on that point to confirm
      the ending point of the segment.>

     At the end of the task the parameter will contains the
     segment. Normally you start another task (by setting <I=AfterS>
     of the task parameter) and use the defined segment.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      CADPrg returns in the default state.>

     The operation needs an instance of
     <See Class=TCAD3DSelectSegmentParam>.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DSelectSegment = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD3DRotationalDolly>.
  }
  TCAD3DRotationalDollyParam = class(TCAD3DCommonParam)
  private
    fOldMouse: TPoint;
    fCameraCenter, fCameraPos: TPoint3D;
    fOldCameraCenter, fOldCameraPos: TPoint3D;
    fInDollying, fOldShowCursor: Boolean;
    fDollyRadius: TRealType;
    fXAxis, fYAxis: TVector3D;
  public
    {: This is the constructor of the class.

       Parameters:

       <LI=<I=CameraPos> is the position of the camera around
        which rotate.>
       <LI=<I=CameraView> is the actual position of the camera.>
    }
    constructor Create(const CameraPos, CameraView: TPoint3D);
  end;

  {: This class implements the <I=rotational dolly task>.

     This task may be used to change the camera position around
     a sphere centered on the current view point.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse while holding the left mouse button. The
      viewport will change its viewing parameters.>
     <LI=continue from the first step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the actual
      viewport properties are accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of
     <See Class=TCAD3DRotationalDollyParam> class.
     The task cannot be suspended and cannot be used
     on ortogonal viewports.

     See also <See Class=TCADPrg>.
  }
  TCAD3DRotationalDolly = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD3DPositionalDolly>.
  }
  TCAD3DPositionalDollyParam = class(TCAD3DCommonParam)
  private
    fOldMouse: TPoint;
    fCameraCenter, fCameraPos: TPoint3D;
    fOldCameraCenter, fOldCameraPos: TPoint3D;
    fInDollying, fOldShowCursor: Boolean;
    fDollyRadius: TRealType;
  public
    {: This is the constructor of the class.

       Parameters:

       <LI=<I=CameraPos> is the position of the camera around
        which rotate.>
       <LI=<I=CameraView> is the actual position of the camera.>
    }
    constructor Create(const CameraPos, CameraView: TPoint3D);
  end;

  {: This class implements the <I=positional dolly task>.

     This task may be used to change the camera position by
     moving it on the current working plane.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse while holding the left mouse button. The
      viewport will change its viewing parameters.>
     <LI=continue from the first step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the actual
      viewport properties are accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of
     <See Class=TCAD3DPositionalDollyParam> class.
     The task cannot be suspended and cannot be used
     on ortogonal viewports.

     See also <See Class=TCADPrg>.
  }
  TCAD3DPositionalDolly = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD3DDeeptDolly>.
  }
  TCAD3DDeeptDollyParam = class(TCAD3DCommonParam)
  private
    fOldMouse: TPoint;
    fCameraCenter, fCameraPos: TPoint3D;
    fOldCameraCenter, fOldCameraPos: TPoint3D;
    fInDollying, fOldShowCursor: Boolean;
    fDollyRadius: TRealType;
  public
    {: This is the constructor of the class.

       Parameters:

       <LI=<I=CameraPos> is the position of the camera around
        which rotate.>
       <LI=<I=CameraView> is the actual position of the camera.>
    }
    constructor Create(const CameraPos, CameraView: TPoint3D);
  end;

  {: This class implements the <I=deep dolly task>.

     This task may be used to change the camera position on
     the line from the current camera position and the view
     point.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse while holding the left mouse button.
      The viewport will change its viewing parameters.>
     <LI=continue from the first step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the actual
      viewport properties are accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of
     <See Class=TCAD3DDeeptDollyParam> class.
     The task cannot be suspended and cannot be used on
     ortogonal viewports.

     See also <See Class=TCADPrg>.
  }
  TCAD3DDeeptDolly = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
  end;

  {: This class defines the parameter used by the
     <See Class=TCAD3DPositionObject> task.

     The class is used to store the 3D object that must be
     positioned on the CAD. If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD3DPositionObjectParam = class(TCAD3DCommonParam)
  private
    fObject: TObject3D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=O> contains the object to be positioned with the
        <See Class=TCAD3DPositionObject> task.>

        <B=Note>: in the case <I=AfterS> is not <B=nil>, the
         object will not be added to the CAD.
    }
    constructor Create(AfterS: TCADStateClass; O: TObject3D);
    {: This property contains the object to be positioned
       (or already positioned when the task is finished) with the
       <See Class=TCAD3DPositionObject> task.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TCADCmp>,
       otherwise it will be added to it.
    }
    property Obj: TObject3D read fObject;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD3DDrawUnSizedPrimitive> to construct a 3D
     primitive with a variable number of <I=control points>,
     like a polyline.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD3DDrawUnSizedPrimitiveParam = class(TCAD3DCommonParam)
  private
    fPrimObject: TPrimitive3D;
    fCurrPoint: Word;
    fOrtoIsUsable: Boolean;
  protected
    {: This method updates the on-screen informations during
       the task.

       The method draws the current primitive with the rubber
       band (xor pen mode) mode. You will see your primitive
       growing as new points are added to it.
    }
    procedure DrawOSD(Viewport: TCADViewport3D); virtual;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 3D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal to
        3, when the user click on the viewport, the fourth control
        point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any means with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive: TPrimitive3D; StartPointIdx: Integer; OrtoIsU: Boolean);
    {: This property contains the primitive being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.

       If you have assigned a state to the
       <See Property=TCADPrgParam@AfterState> property, the
       object is not added to the linked <See Class=TCADCmp>,
       otherwise it will be added to it.
    }
    property Primitive: TPrimitive3D read fPrimObject;
    {: This property indicates if the ortogonal constraint has
       any means with the primitive being defined.

       If it is <B=True>, the orto constraint will be used,
       otherwise it will not used.
    }
    property OrtoIsUsable: Boolean read fOrtoIsUsable write fOrtoIsUsable;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD3DDrawSizedPrimitive> to construct a 3D
     primitive with a fixed number of points, like an ellipse.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD3DDrawSizedPrimitiveParam = class(TCAD3DDrawUnSizedPrimitiveParam)
  private
    fnPoints: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        <See Class=TCADPrg> will return to the default state.>
       <LI=<I=Primitive> is the 3D primitive to be constructed.>
       <LI=<I=StartPointIdx> is the first control points that
        will be added. For instance if this parameter is equal
        to 3 when the user click on the viewport, the fourth
        control point will be added.>
       <LI=<I=OrtoIsU> indicate if the ortogonal constraint has
        any meanse with this primitive. If it is <B=True>, the
        orto constraint will be used, otherwise it will not used.>
    }
    constructor Create(AfterS: TCADStateClass; Primitive: TPrimitive3D; StartPointIdx: Integer; OrtoIsU: Boolean);
  end;

  {: This class implements the <I=object positioning task>.

     This task may be used to add an object to the linked CAD
     by firstly positioning it in the world.

     The operation wait for the user to do the following
     (in the order given here):

     <LI=move the object to the desired position using the
      mouse. You will see the object moving on the screen.>
     <LI=press the left mouse button on that point to lay down
      the object.>

     The object will be moved using its bottom-left bounding box
     corner as the base point. At the end of the task the object
     is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD3DPositionObjectParam> class. The task may
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DPositionObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a fixed number of control points.

     This task may be used to add an object to the linked CAD by
     firstly defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the
      parameter is greater than zero the control point to be
      positioned is not the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second step until no
      control points are left.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
     added to the CAD.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD3DDrawSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DDrawSizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=primitive constructing task>
     for primitives with a variable number of control points.

     This task may be used to add an object to the linked CAD
     by firstly defining its control points.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the position for the first control
      point of the primitive (if <I=StartPointIdx> of the parameter
      is greater than zero the control point to be positioned is not
      the first control point).>
     <LI=press the left mouse button to set the control point on
      that point.>
     <LI=move the mouse on the desired position for the next
      control points. Continue with the second.>

     During the second and third steps you will see the object
     on the screen.

     At the end of the task the object is added to the CAD.
     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD. Note that this is the only way to end
      the task.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the object
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD3DDrawUnSizedPrimitiveParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DDrawUnSizedPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by the
     <See Class=TCAD3DDrawArcPrimitive> task to construct a
     3D arc segment.

     If you assign a state to the
     <See Property=TCADPrgParam@AfterState> property, the
     object is not added to the linked <See Class=TCADCmp>,
     otherwise it will be added to it.
  }
  TCAD3DDrawArcPrimitiveParam = class(TCAD3DCommonParam)
  private
    fArcObject: TArc3D;
    fCurrPoint: Word;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state. Note that in case
        <I=AfterS> is not <B=nil>, the object will not be added to
        the CAD.>
       <LI=<I=Arc> contains the object to be constructed with
        the <See Class=TCAD3DDrawArcPrimitive> task.>
    }
    constructor Create(AfterS: TCADStateClass; Arc: TArc3D);
    {: This property contains the 3D arc that is being constructed.

       The object is deleted when the parameter is destroyed. If
       you want to keep the instance, set the property to <B=nil>
       before delete the parameter.
    }
    property Arc: TArc3D read fArcObject;
  end;

  {: This class implements the <I=arc constructing task>.

     This task may be used to add an arc to the linked CAD by
     defining its control points.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the desired position for the first
      control point of the arc.>
     <LI=press the left mouse button to set the first control
      point on that point.>
     <LI=move the mouse on the desired position for the second
      control point. You will see an ellipse drawed on the
      viewport.>
     <LI=press the left mouse button to set the second control
      point.>
     <LI=move the mouse on the desired position for the third
      control point. You will modify the starting angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the third control
      point.>
     <LI=move the mouse on the desired position for the fourth
      control point. You will modify the ending angle of the
      arc, and you will see the arc on the screen.>
     <LI=press the left mouse button to set the fourth control
      point.>

     At the end of the task the arc is added to the CAD.

     The operation understand the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the object
      added to the CAD.>
     <LI=CADPRG_CANCEL>. The task is aborted and the object
     destroyed. The CADPrgreturns in the default state.>

     The operation needs an instance of the
     <See Class=TCAD3DDrawArcPrimitiveParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DDrawArcPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This type is another name for <See Class=TCADPrgParam>.
  }
  TCAD3DSelectObjectsParam = class;

  {: This type defines the prototype for an event handler used
     to inform the application that an object was picked.

     See <See Class=TCAD3DSelectObject>,
     <See Class=TCAD3DSelectObjects> and
     <See Class=TCAD3DSelectObjectsInArea> for details.

     Parameters:

     <LI=<I=Sender> contains the instance of
      <See Class=TCAD3DSelectObjectsParam> (or
      <See Class=TCAD3DSelectObjectsInAreaParam>) of the task
      that had called the handler.>
     <LI=<I=Obj> contains the picked object, or <B=nil> if the
      selection task is <See Class=TCAD3DSelectObjectsInArea>.>
     <LI=<I=CtrlPt> contains the control point on which the
      mouse was picked. This is the same result of the
      <See Method=TCADViewport3D@PickObject> method. This
      parameter will be <I=PICK_NOOBJECT> in case the selection
      task is <See Class=TCAD3DSelectObjectsInArea>.>
     <LI=<I=Added> is <B=True> if Obj is added to the selected
      object list, or <B=False> if it is removed.>

     If a repaint event is raised, the handler is called for all
     the picked objects. In this case the <I=CtrlPt> is
     <I=PICK_NOOBJECT> and <I=Added> is <B=True> the selection
     task that fired the event is
     <See Class=TCAD3DSelectObjectsInArea>.
  }
  TSelection3DEvent = procedure(Sender: TCAD3DSelectObjectsParam; Obj: TObject3D; CtrlPt: Integer; Added: Boolean) of object;

  {: This class defines the parameter used by
     <See Class=TCAD3DSelectObject> and
     <See Class=TCAD3DSelectObjects> to pick objects
     interactively on the screen.
  }
  TCAD3DSelectObjectsParam = class(TCAD3DCommonParam)
  private
    fApertureSize: Word;
    fLastSelectedCtrlPoint: Integer;
    fLastPt: TPoint2D;
    fSelectedObjs: TGraphicObjList;
    fOnSelected: TSelection3DEvent;
    fEndIfNoObject, fEndWithMouseDown: Boolean;
    fSelectionFilter: TObject3DClass;
    fUserFlag: LongInt;
  protected
    {: This method draws the picking frame used to show the
       aperture of the picking.

       The frame is drawed in xor pen mode and centered at <I=Pt>.

       Parameters:

       <LI=<I=Viewport> is the viewport on which draw the frame.>
       <LI=<I=Pt> is the point at which draw the frame.>
    }
    procedure DrawOSD(Viewport: TCADViewport3D; const Pt: TPoint2D); virtual;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=ApertureSize> is the aperture used for the picking
        in pixels.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.
    }
    constructor Create(ApertureSize: Word; const AfterS: TCADStateClass; UserFlag: LongInt);
    destructor Destroy; override;
    {: This property is the list that contains the picked
       objects.

       If you want to traverse the list ask for an iterator and
       remember to free it before to returns to the selection
       task.

       See also <See Class=TGraphicObjList>.
    }
    property SelectedObjects: TGraphicObjList read fSelectedObjs;
    {: This property contains the control point selected of the
       last picked object.
    }
    property LastSelectedCtrlPoint: Integer read fLastSelectedCtrlPoint;
    {: This property may contain a class reference type (deriving
       from <See Class=TObject3D>) used to filter the selection.

       If the picked object doesn't derive from that class, the
       object is ignored.

       By default it is <I=TObject3D>.
    }
    property SelectionFilter: TObject3DClass read fSelectionFilter write fSelectionFilter;
    {: This property specifies if the selection task must be ended
       when the <I=mouse down> event is received.

       If it is <I=True> the task is finished as soon as the
       user press the mouse. Otherwise the task will finish when
       the user release the mouse button.

       By default it is <B=False>.
    }
    property EndSelectObjectWithMouseDown: Boolean read fEndWithMouseDown write fEndWithMouseDown;
    {: EVENTS}
    {: This property may contains an event handler that will be
      called when an object is picked (after it was added to the
      list).

      See Also <See Type=TSelection3DEvent>.
    }
    property OnObjectSelected: TSelection3DEvent read fOnSelected write fOnSelected;
  end;

  {: This class implements the <I=single object selection task>.

     This task may be used to select an object of the linked CAD.
     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>

     The object will be added to the
     <See Property=TCAD3DSelectObjectsParam@SelectedObjects> list
     of the task parameter. Normally you set <I=AfterS> of the
     task parameter to a state that will process the selected
     object by using the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD3DSelectObjectsParam> class. The task can
     be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DSelectObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=multiple object selection task>.

     This task may be used to select a set of object of the
     linked CAD.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object. If the object was already picked, it is removed
      from <See Property=TCAD3DSelectObjectsParam@SelectedObjects>
      list of the task parameter, otherwise it will be added.>
     <LI=continue with the first step.>

     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     Note that no visual feedback is given to the user. If you
     want to show the selected objects, you can use the
     <See Property=TCAD3DSelectObjectsParam@OnObjectSelected>
     handler of the task parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>
     <LI=<I=CADPRG_ACCEPT>. The task is ended. The CADPrg
     returns in the default state or in the state specified by
     <I=AfterS>. Note that this is the only way to finish the
     task.>

     The operation needs an instance of the
     <See Class=TCAD3DSelectObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines a special kind of selection task that
     is the combination of the
     <See Class=TCAD3DSelectObject> and
     <See Class=TCAD3DSelectObjects> tasks.

     If the user holds down the Shift key, the task behaves
     like the <I=TCAD3DSelectObjects> task, otherwise it
     behaves like the <I=TCAD3DSelectObject> task.

     See also <See Class=TCADPrg>.
  }
  TCAD3DExtendedSelectObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class defines the parameter used by
     <See Class=TCAD3DSelectObjectsInArea> task to select the
     objects contained in the specified window area.
  }
  TCAD3DSelectObjectsInAreaParam = class(TCAD3DSelectObjectsParam)
  private
    fAreaMode: TGroupMode;
    fArea: TRect2D;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=AreaMode> specify the type of selection. If it is
        <I=gmAllInside> only the objects fully contained in the
        area are selected; if it is <I=gmCrossFrame> all the
        objects that are contained or cross the area are selected.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is nil, the
        CADPrg will return to the default state.>
    }
    constructor Create(AreaMode: TGroupMode; const AfterS: TCADStateClass);
  end;

  {: This class implements <I=the 'area selection task>.

     This task may be used to select a set of object of
     the linked CAD by specify a rectangle frame.

     The operation wait for the user to do the following (in
     the order given here):

     <LI=move the mouse on the first corner of the area.>
     <LI=press the left mouse button to accept the point.>
     <LI=move the mouse on the second corner of the area. You
      will see the area being defined on the screen.>
     <LI=press the left mouse button to accept the point.>

     All the objects in the area are selected and stored in the
     <See Property=TCAD3DSelectObjectsParam@SelectedObjects>
     list of the task parameter.
     Normally you set <I=AfterS> of the task parameter to a
     state that will process the selected objects by using the
     passed parameter.

     The operation understand the following user commands:

     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of the
     <See Class=TCAD3DSelectObjectsInAreaParam> class. The task
     can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DSelectObjectsInArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    procedure OnStop; override;
  end;

  TCAD3DTransformObjectsParamClass = class of TCAD3DTransformObjectsParam;

  {: This class defines the parameter used by the
     <I=object transformation task>.

     All the transformations of objects that may be described
     by a matrix transform that accept as parametesr a base
     point and a moving point can be modelled by this task.
     You only need to derive from this parameter and
     redefine the
     <See Method=TCAD3DTransformObjectsParam@GetTransform> method.
     Then you can pass the new parameter to
     <See Class=TCAD3DTransformObjects> task.
  }
  TCAD3DTransformObjectsParam = class(TCAD3DCommonParam)
  private
    fBasePt: TPoint3D;
    fNPoint: Integer;
    fObjs: TGraphicObjList;
    fUseFrame: Boolean;
    fCurrTransf: TTransf3D;
    fObjectsBox: TRect3D;

    procedure TransformObjs(CurrPt: TPoint3D; CADPrg: TCADPrg3D);
    procedure ConfirmTransform;
    procedure CancelTransform;
  protected
    {: This method draws the bounding box of the set of objects
       to be transformed.

       It is used by the <See Method=TCAD3DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithFrame(Viewport: TCADViewport3D); dynamic;
    {: This method draws the objects to be transformed in
       rubber band mode (xor pen mode).

       It is used by the <See Method=TCAD3DTransformObjectsParam@DrawOSD>
       method.

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawWithoutFrame(Viewport: TCADViewport3D); dynamic;
    {: This is the key method of the class.

       It must return the matrix transform that define the
       transformation of the objects. The returned matrix will
       override the current model transform for the selected
       objects.

       This method must be redefined in the derived classes
       for specific transformations.

       Parameters:

       <LI=<I=BasePt> is the base point for the transformation.
        For example to rotate an object you must give the center
        of rotation; to move an object you must give the first
        point of the translation.>
       <LI=<I=CurrPt> is the current point of the mouse. You
        may use this point to define the current transformation.
        For example to rotate an object you must give a second
        point, so you are able to find the angle of rotation
        with respect to the <I=BasePt>; to move an object this
        point is the second point of the translation.>
    }
    function GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D; virtual; abstract;
    {: This method draws the on screen informations that informs
       the user of the result of the transformation.

       There are two modes of visualization:

       <LI=If <See Property=TCAD3DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD3DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       Parameters:

       <LI=<I=Viewport> is the viewport of which draw the
        information.>
    }
    procedure DrawOSD(Viewport: TCADViewport3D); virtual;
    {: This property contains the base point for the
       transformation.
    }
    property BasePt: TPoint3D read fBasePt;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=Objs> is a list that contains the objects to be
        transformed. The list must have the
        <See Property=TGraphicObjList@FreeOnClear> property set
        to <B=False>.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.>
    }
    constructor Create(Objs: TGraphicObjList; const AfterS: TCADStateClass);
    destructor Destroy; override;
    {: This property contains the list of the objects to be
       transformed.
    }
    property Objects: TGraphicObjList read fObjs;
    {: This property selects the visualization mode for the on
       screen informations:

       <LI=If <See Property=TCAD3DTransformObjectsParam@UseFrame>
        is <B=True> only the bounding box of the objects is showed>
       <LI=If <See Property=TCAD3DTransformObjectsParam@UseFrame>
        is <B=False> the transformed objects are showed in xor
        pen mode>

       When the parameter is constructed, this property is set
       to <B=False> if the passed list of objects has only one
       object, it is set to <B=True> otherwise.>
    }
    property UseFrame: Boolean read fUseFrame write fUseFrame;
  end;

  {: This class defines the method
     <See Method=TCAD3DTransformObjectsParam@GetTransform> for
     the <I=move object task>.
  }
  TCAD3DMoveObjectsParam = class(TCAD3DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D; override;
  end;

  {: This class defines the method
     <See Method=TCAD3DTransformObjectsParam@GetTransform> for
     the <I=rotate object on ZY plane task>.
  }
  TCAD3DRotateOnXObjectsParam = class(TCAD3DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D; override;
  end;

  {: This class defines the method
     <See Method=TCAD3DTransformObjectsParam@GetTransform> for
     the <I=rotate object on ZX plane task>.
  }
  TCAD3DRotateOnYObjectsParam = class(TCAD3DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D; override;
  end;

  {: This class defines the method
     <See Method=TCAD3DTransformObjectsParam@GetTransform> for
     the <I=rotate object on XY plane task>.
  }
  TCAD3DRotateOnZObjectsParam = class(TCAD3DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D; override;
  end;

  {: This class defines the method
     <See Method=TCAD3DTransformObjectsParam@GetTransform> for
     the <I=rotate object on a plane task>.
  }
  TCAD3DRotateOnPlaneObjectsParam = class(TCAD3DTransformObjectsParam)
  private
    fWX, fWY: TVector3D;
  protected
    function GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D; override;
  public
    {: This is the constructor of the class:

       Parameters:

       <LI=<I=Objs> is a list that contains the objects to be
        transformed. The list must have the
        <See Property=TGraphicObjList@FreeOnClear> property set
        to <B=False>.>
       <LI=<I=AfterS> may contains a <See Class=TCADState> class
        reference. If it is assigned that state will be started
        at the end of the current task. That state will receive
        the current parameters. If <I=AfterS> is <B=nil>, the
        CADPrg will return to the default state.>
       <LI=<I=WX> and <I=WY> are respectively the X and Y direction
        of plane on which rotate the objects.>
    }
    constructor Create(Objs: TGraphicObjList; const WX, WY: TVector3D; const AfterS: TCADStateClass);
  end;

  {: This class defines the method
     <See Method=TCAD3DTransformObjectsParam@GetTransform> for
     the <I=scale object task>.
  }
  TCAD3DScaleObjectsParam = class(TCAD3DTransformObjectsParam)
  protected
    function GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D; override;
  end;

  {: This class implements the <I=transform objects task>.

     This task may be used to apply a transformation to a set
     of objects by specifing the appropriate parameter (see below).

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the transformation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      object transformed on the screen.>
     <LI=press the left mouse button to accept the current
      transformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of a class derived from
     <See Class=TCAD3DTransformObjectsParam>. The task can be
     suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DTransformObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=move a selection task>.

     This task may be used to select and move a set of objects
     by specifing the start point and end point of the
     translation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the base point for the translation.>
     <LI=press the left mouse button to accept the base point.>
     <LI=move the mouse on the second point. You will see the
      objects moving on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD3DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DMoveSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=rotate on ZY plane a selection> task.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of rotation.>
     <LI=press the left mouse button to accept the center of
      rotation.>
     <LI=move the mouse on the second point. You will see the
      objects rotating on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD3DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DRotateOnXSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=rotate on ZX plane a selection> task.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of rotation.>
     <LI=press the left mouse button to accept the center of
      rotation.>
     <LI=move the mouse on the second point. You will see the
      objects rotating on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD3DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DRotateOnYSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=rotate on XY plane a selection> task.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of rotation.>
     <LI=press the left mouse button to accept the center of
      rotation.>
     <LI=move the mouse on the second point. You will see the
      objects rotating on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD3DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DRotateOnZSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=rotate on working plane a selection> task.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of rotation.>
     <LI=press the left mouse button to accept the center of
      rotation.>
     <LI=move the mouse on the second point. You will see the
      objects rotating on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD3DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DRotateOnWPSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This class implements the <I=scale a selection> task.

     This task may be used to select and rotate a set of
     objects by specifing the center of rotation and the angle
     of rotation.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on the center of scaling.>
     <LI=press the left mouse button to accept the center of
      scaling.>
     <LI=move the mouse on the second point. You will see the
      objects scaling on the screen.>
     <LI=press the left mouse button to accept the tranformation.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and the
      transformation is accepted.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
     parameter destroyed. The CADPrg returns in the default
     state.>

     The operation needs an instance of the
     <See Class=TCAD3DMoveObjectsParam> class.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DScaleSelectedObjects = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  {: This type defines the modes used to edit the control
     points of a <See Class=TPrimitive3D>.

     It may be one of the following mode:

     <LI=<I=emNone> (default). In this mode, if the primitive
      is a planar one the working plane is temporarily changed
      and set to the primitive plane; if the primitive is
      not a planar one, the control points are moved in
      a manner that preserve the distance of the control point
      from the current working plane.>
     <LI=<I=emChangeWP>. The same as <I=EmNone>.>
     <LI=<I=emProjectOnWP>. In this mode, if the primitive is
      a planar one the working plane is temporarily changed and
      setted to the primitive plane; if the primitive is not
      a planar one, the primitive is projected on the current
      working plane and then edited.>
  }
  TCAD3DEditPrimMode = (emNone, emChangeWP, emProjectOnWP);

  {: This class defines the parameter used by
     <See Class=TCAD3DEditPrimitive>. 
  }
  TCAD3DEditPrimitiveParam = class(TCAD3DCommonParam)
  private
    fPrimitive3D: TPrimitive3D;
    fEditMode: TCAD3DEditPrimMode;
  public
    {: This method creates a new instance of the parameter.

       Parameters:

       <LI=<I=Prim> is the primitive to be edited.>
       <LI=<I=EditMode> is the editing mode (see also
       <See Class=TCAD3DEditPrimMode>.>
    }
    constructor Create(Prim: TPrimitive3D; EditMode: TCAD3DEditPrimMode);
    {: This property contains the primitive being edited.

       The object is not deleted when the parameter is destroyed.
    }
    property Primitive3D: TPrimitive3D read fPrimitive3D;
  end;

  {: This class implements the <I=edit primitive task>.

     This task may be used to move the control points of a
     <See Class=TPrimitive3D> interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the new
      position of the control point.>
     <LI=continue from the first step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=CADPRG_CANCEL>. The task is aborted and the parameter
      destroyed. The CADPrg returns in the default state.>

     The operation needs an instance of the of the primitive
     incapsulated into the <See Property=TCADPrgParam@UserObject>
     property of a <See Class=TCADPrgParam> instance.

     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DEditPrimitive = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  {: This class implements the <I=select and edit primitive task>.

     This task may be used to select a primitive and move its
     control points interactively on the screen.

     The operation waits for the user to do the following (in
     the order given here):

     <LI=move the picking selection frame (showed on the screen
      as a small rectangle) on the object to be picked.>
     <LI=press the left mouse button on that point to pick the
      object.>
     <LI=move the mouse on one control point of the primitive.
      The primitive is showed in with the rubber pen of
      <See Class=TCADViewport>.>
     <LI=press and hold the left mouse button to pick the
      control point.>
     <LI=move the mouse to move the control point. You will see
      the primitive changing its shape.>
     <LI=release the left mouse button to accept the
      new position of the control point.>
     <LI=continue from the third step.>

     The operation understands the following user commands:

     <LI=<I=CADPRG_ACCEPT>. The task is ended and accept the
      new setting for the control point.>
     <LI=<I=CADPRG_CANCEL>. The task is aborted and the
      parameter destroyed. The CADPrg returns in the default
      state.>

     The operation needs an instance of
     <See Class=TCAD3DSelectObjectsParam>.
     The task can be suspended.

     See also <See Class=TCADPrg>.
  }
  TCAD3DEditSelectedObject = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

implementation

uses Math;

type
// -----===== Starting Cs4CADPrgTasks.pas =====-----
  TCADPrgEndZoomArea = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
  end;

  TCADPrgDragPan = class(TCADPrgZoomState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

  TCADPrgDragSelectArea = class(TCADState)
  public
    constructor Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass); override;
    function OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                     var NextState: TCADStateClass): Boolean; override;
    procedure OnStop; override;
  end;

// -----===== Starting Cs4CADPrgTasks2D.pas =====-----
  TCAD2DEditPrimitiveParam = class(TCAD2DCommonParam)
  private
    fCurrentPrimitive, fOriginalPrimitive: TPrimitive2D;
    fCurrentCtrlPt: Integer;
    fApertureSize: Word;
    fLastPt: TPoint2D;
  public
    constructor Create(Prim: TPrimitive2D; ApertureSize: Word);
    destructor Destroy; override;

    procedure SetCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
    procedure AddCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
    procedure UnSetCtrlPoint;
    procedure AcceptEdited;
    procedure MoveCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
    procedure DrawOSD(Viewport: TCADViewport2D; Pt: TPoint2D; FirstTime: Boolean);
    procedure DrawModifiedPrim(Viewport: TCADViewport2D);

    property CurrentCtrlPt: Integer read fCurrentCtrlPt;
  end;

// -----===== Starting Cs4CADPrgTasks3D.pas =====-----
  TCAD3DInternalEditPrimitiveParam = class(TCAD3DCommonParam)
  private
    fCurrentPrimitive, fOriginalPrimitive: TPrimitive3D;
    fOwnerPrg: TCADPrg3D;
    fCurrentCtrlPt: Integer;
    fCurrentCtrlPtDist: TRealType;
    fCurrentCtrlPtExDir: TVector3D;
    fApertureSize: Word;
    fLastPt: TPoint3D;
    fEditMode: TCAD3DEditPrimMode;
    fOldWPNorm, fOldWPUp: TVector3D;
    fOldWPRef: TPoint3D;
  public
    constructor Create(const CADPrg: TCADPrg; Prim: TPrimitive3D; ApertureSize: Word; EditMode: TCAD3DEditPrimMode);
    destructor Destroy; override;

    procedure SetCtrlPoint(Viewport: TCADViewport3D; Pt: TPoint3D);
    procedure AddCtrlPoint(Viewport: TCADViewport3D; Pt: TPoint3D);
    procedure UnSetCtrlPoint;
    procedure AcceptEdited;
    procedure MoveCtrlPoint(Viewport: TCADViewport3D; Pt: TPoint3D; {%$H-}CADPrg: TCADPrg3D);
    procedure DrawOSD(Viewport: TCADViewport3D; Pt: TPoint3D; FirstTime: Boolean);
    procedure DrawModifiedPrim(Viewport: TCADViewport3D);

    property CurrentCtrlPt: Integer read fCurrentCtrlPt;
    property EditMode: TCAD3DEditPrimMode read fEditMode;
  end;

// -----===== Starting Cs4CADPrgTasks.pas =====-----

{ -------------- TCADPrgSelectAreaParam -------------- }
constructor TCADPrgSelectAreaParam.Create(AfterS: TCADStateClass; CallerParam: TCADPrgParam);
begin
  inherited Create(AfterS);
  fFrame := TFrame2D.Create(0, Point2D(0, 0), Point2D(0, 0));
  fCallerParam := CallerParam;
end;

destructor TCADPrgSelectAreaParam.Destroy;
begin
  fFrame.Free;
  inherited Destroy;
end;

function TCADPrgSelectAreaParam.GetArea: TRect2D;
begin
  Result := fFrame.Box;
end;

{ ******************* Useful states *********************** }

{ ------------------ Select Area --------------------- }

constructor TCADPrgSelectArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the first point of the area.'
end;

{ Need TCADPrgSelectAreaParam. }
function TCADPrgSelectArea.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton;
                                  Shift: TShiftState; Key: Word;
                                  var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with Param as TCADPrgSelectAreaParam, CADPrg do
   case Event of
    ceUserDefined:
     if Key = CADPRG_CANCEL then
      begin
         Param.Free;
         Param := nil;
         NextState := CADPrg.DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     begin
      CurrPoint := CurrentViewportPoint;
      fFrame.Points[0] := CurrPoint;
      fFrame.Points[1] := CurrPoint;
      if Viewport is TCADViewport3D then
       TCADViewport3D(Viewport).DrawObject2DWithRubber(fFrame, False)
      else
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
      NextState := TCADPrgDragSelectArea;
      Result := True;
     end;
   end;
end;

procedure TCADPrgSelectArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCADPrgDragSelectArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the second point of the area.'
end;

function TCADPrgDragSelectArea.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton;
                                      Shift: TShiftState; Key: Word;
                                      var NextState: TCADStateClass): Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with Param as TCADPrgSelectAreaParam, CADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      if Assigned(AfterState) then
       NextState := AfterState
      else
       NextState := DefaultState;
      Result := True;
    end;
    ceMouseMove: begin
      if Viewport is TCADViewport3D then
       TCADViewport3D(Viewport).DrawObject2DWithRubber(fFrame, False)
      else
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
      fFrame.Points[1] := CurrentViewportPoint;
      if Viewport is TCADViewport3D then
       TCADViewport3D(Viewport).DrawObject2DWithRubber(fFrame, False)
      else
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
    end;
    cePaint:
    if Viewport is TCADViewport3D then
     TCADViewport3D(Viewport).DrawObject2DWithRubber(fFrame, False)
    else
    if Viewport is TCADViewport2D then
     TCADViewport2D(Viewport).DrawObject2DWithRubber(fFrame, False);
   end;
end;

procedure TCADPrgDragSelectArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ******************* Zooming states *********************** }

constructor TCADPrgZoomState.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  CanBeSuspended := False;
end;

{ ------------------ Zoom Area --------------------- }

{ No parameter. }
constructor TCADPrgZoomArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Param := TCADPrgSelectAreaParam.Create(TCADPrgEndZoomArea, StateParam);
  NextState := TCADPrgSelectArea;
end;

constructor TCADPrgEndZoomArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if Assigned(Param) then
   with CADPrg as TCADPrg, Param as TCADPrgSelectAreaParam do
    begin
      if not IsSamePoint2D(Area.FirstEdge, Area.SecondEdge) then
       Viewport.ZoomWindow(Area);
      if (CallerParam is TCADPrgZoomParam) and
         Assigned(TCADPrgParam(CallerParam).AfterState) then
       NextState := TCADPrgParam(CallerParam).AfterState
      else
       NextState := CADPrg.DefaultState;
      Param.Free;
      Param := nil;
    end;
end;

{ ------------------ ZoomInOut --------------------- }

constructor TCADPrgZoomInOut.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the center of the zoom. (Hold Shift key for zoom out)'
end;

{ No parameter. }
function TCADPrgZoomInOut.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                 var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  with CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      CurrPoint := CurrentViewportPoint;
      with Viewport do
       PanWindow(CurrPoint.X - (VisualRect.Right + VisualRect.Left) / 2.0,
               CurrPoint.Y - (VisualRect.Bottom + VisualRect.Top) / 2.0);
      if ssShift in Shift then
       Viewport.ZoomOut
      else
       Viewport.ZoomIn;
     end;
   end;
end;

{ ------------------ Pan --------------------- }

{ No parameter. }
constructor TCADPrgPan.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Param := TCADPrgParam.Create(StateParam.AfterState);
  Param.UserObject := TLine2D.Create(0, Point2D(0, 0), Point2D(0, 0));
  Description := 'Select the start point of the pan.'
end;

function TCADPrgPan.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                           var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with TCADPrgParam(Param), CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         TLine2D(UserObject).Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     begin
       CurrPoint := CurrentViewportPoint;
       TLine2D(UserObject).Points[0] := CurrPoint;
       TLine2D(UserObject).Points[1] := CurrPoint;
       if Viewport is TCADViewport3D then
        TCADViewport3D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False)
       else
       if Viewport is TCADViewport2D then
        TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
       NextState := TCADPrgDragPan;
       Result := True;
     end;
   end;
end;

procedure TCADPrgPan.OnStop;
begin
  TCADPrgParam(Param).UserObject.Free;
  Param.Free;
  Param := nil;
end;

constructor TCADPrgDragPan.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Select the end point of the pan.'
end;

function TCADPrgDragPan.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                               var NextState: TCADStateClass): Boolean;
var
  CurrPoint: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with TCADPrgParam(Param), CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         TLine2D(UserObject).Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     with TLine2D(UserObject) do
      begin
        IgnoreEvents := True;
        try
         if not IsSamePoint2D(Box.FirstEdge, Box.SecondEdge) then
         Viewport.PanWindow(Points[0].X - Points[1].X, Points[0].Y - Points[1].Y);
        finally
         IgnoreEvents := False;
        end;
        if Assigned(AfterState) then
         NextState := AfterState
        else
         NextState := DefaultState;
        Free;
        Param.Free;
        Param := nil;
        Result := True;
      end;
    ceMouseMove: begin
      if Viewport is TCADViewport3D then
       TCADViewport3D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False)
      else
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
      CurrPoint := CurrentViewportPoint;
      TLine2D(UserObject).Points[1] := CurrPoint;
      if Viewport is TCADViewport3D then
       TCADViewport3D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False)
      else
      if Viewport is TCADViewport2D then
       TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
    end;
    cePaint:
    if Viewport is TCADViewport3D then
     TCADViewport3D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False)
    else
    if Viewport is TCADViewport2D then
     TCADViewport2D(Viewport).DrawObject2DWithRubber(TLine2D(UserObject), False);
   end;
end;

procedure TCADPrgDragPan.OnStop;
begin
  TCADPrgParam(Param).UserObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------------ RealTimePan --------------------- }

{ No parameter. }
constructor TCADPrgRealTimePan.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Old the mouse and move it to pan.';
  fInPanning := False;
  fOriginalRect := CADPrg.Viewport.VisualRect;
end;

function TCADPrgRealTimePan.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  ScrPt, CurrPoint: TPoint2D;
  TmpDist, RefDist: TRealType;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  with CADPrg as TCADPrg do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         NextState := DefaultState;
         Result := True;
         Viewport.ZoomWindow(fOriginalRect);
       end
      else if Key = CADPRG_ACCEPT then
       begin
         if (Param is TCADPrgZoomParam) and Assigned(TCADPrgParam(Param).AfterState) then
          NextState := TCADPrgParam(Param).AfterState
         else
          NextState := DefaultState;
         RepaintAfterOperation;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then
     begin
       fInPanning := True;
       fLastPoint := CurrentViewportPoint;
       if MouseButton = cmbRight then
        begin
          NextState := DefaultState;
          Result := True;
        end;
     end;
    ceMouseDblClick,
    ceMouseUp: if MouseButton = cmbLeft then
     begin
       if Viewport.UsePaintingThread then
        Viewport.Repaint
       else
        Viewport.Refresh;
      fInPanning := False;
     end;
    ceMouseMove: if fInPanning then
     begin
       Viewport.StopRepaint;
       CurrPoint := CurrentViewportPoint;
       TmpDist := PointDistance2D(CurrPoint, fLastPoint);
       RefDist := PointDistance2D(Viewport.VisualRect.FirstEdge, Viewport.VisualRect.SecondEdge);
       if (TmpDist < RefDist * 0.0001) or (TmpDist > RefDist * 2) then
        begin
          fLastPoint := CurrPoint;
          Exit;
        end;
       ScrPt := Viewport.ViewportToScreen(CurrPoint);
       Viewport.PanWindow(fLastPoint.X - CurrPoint.X, fLastPoint.Y - CurrPoint.Y);
       fLastPoint := Viewport.ScreenToViewport(ScrPt);
     end;
   end;
end;

// -----===== Starting Cs4CADPrgTasks2D.pas =====-----

{ ******************* Drawing tasks *********************** }

constructor TCAD2DPositionObjectParam.Create(AfterS: TCADStateClass; O: TObject2D);
begin
  inherited Create(AfterS);
  fObject := O;
end;

constructor TCAD2DDrawUnSizedPrimitiveParam.Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
begin
  inherited Create(AfterS);
  fPrimObject := Primitive;
  fCurrPoint := StartPointIdx;
  fOrtoIsUsable := OrtoIsU;
end;

procedure TCAD2DDrawUnSizedPrimitiveParam.DrawOSD(Viewport: TCADViewport2D);
begin
  Viewport.DrawObject2DWithRubber(fPrimObject, True);
end;

constructor TCAD2DDrawSizedPrimitiveParam.Create(AfterS: TCADStateClass; Primitive: TPrimitive2D; StartPointIdx: Integer; OrtoIsU: Boolean);
begin
  inherited Create(AfterS, Primitive, StartPointIdx, OrtoIsU);
  fnPoints := Primitive.Points.Count;
end;

{ ------------- }

constructor TCAD2DPositionObject.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DPositionObjectParam) then
   Raise ECADSysException.Create('TCAD2DPositionObject: Invalid param');
  Description := 'Press the mouse on the desired insertion point.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DPositionObject.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                      var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DPositionObjectParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         fObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      if Assigned(AfterState) then
       begin
         NextState := AfterState;
         Result := True;
         Exit;
       end;
      IgnoreEvents := True;
      TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
      Viewport2D.CADCmp2D.DrawOnAdd := True;
      try
       Viewport2D.CADCmp2D.AddObject(fObject.ID, fObject);
      finally
        Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
        IgnoreEvents := False;
      end;
      fObject.UpdateExtension(Self);
      Param.Free;
      Param := nil;
      NextState := DefaultState;
      Result := True;
      Exit;
    end;
    ceMouseMove: begin
      CurrPoint2D := CurrentViewportSnappedPoint;
      if not IsSameTransform2D(fObject.ModelTransform, IdentityTransf2D) then
       Viewport2D.DrawObject2DWithRubber(fObject, True);
      fObject.MoveTo(CurrPoint2D, fObject.Box.FirstEdge);
      Viewport2D.DrawObject2DWithRubber(fObject, True);
    end;
    cePaint: if not IsSameTransform2D(fObject.ModelTransform, IdentityTransf2D) then
     Viewport2D.DrawObject2DWithRubber(fObject, True);
   end;
end;

procedure TCAD2DPositionObject.OnStop;
begin
  TCAD2DPositionObjectParam(Param).fObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawSizedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DDrawSizedPrimitiveParam) then
   Raise ECADSysException.Create('TCAD2DDrawSizedPrimitive: Invalid param');
  Description := 'Press the mouse on the desired points.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DDrawSizedPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                       var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  Cont: Integer;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DDrawSizedPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          Viewport2D.CADCmp2D.AddObject(fPrimObject.ID, fPrimObject);
         finally
          Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
          IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fPrimObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D);
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      SnapOriginPoint := CurrentViewportSnappedPoint;
      if fCurrPoint = 0 then
       for Cont := 0 to fnPoints - 1 do
        fPrimObject.Points[Cont] := CurrPoint2D
      else
       begin
         if fOrtoIsUsable and UseOrto then
          MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
         fPrimObject.Points[fCurrPoint] := CurrPoint2D;
       end;
      Inc(fCurrPoint);
      if fCurrPoint = fnPoints then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          Viewport2D.CADCmp2D.AddObject(fPrimObject.ID, fPrimObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
         Exit;
       end
      else
       fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      if fOrtoIsUsable and UseOrto then
       MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
      DrawOSD(Viewport2D);
      fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    cePaint: DrawOSD(Viewport2D);
   end;
end;

procedure TCAD2DDrawSizedPrimitive.OnStop;
begin
  TCAD2DDrawSizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawUnSizedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD2DDrawUnSizedPrimitiveParam) then
   Raise ECADSysException.Create('TCAD2DDrawUnSizedPrimitive: Invalid param');
  TCAD2DDrawUnSizedPrimitiveParam(StateParam).fPrimObject.Points.Clear;
  Description := 'Press the mouse on the desired points.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DDrawUnSizedPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                       var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DDrawUnSizedPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          Viewport2D.CADCmp2D.AddObject(fPrimObject.ID, fPrimObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fPrimObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D);
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      SnapOriginPoint := CurrentViewportSnappedPoint;
      if fCurrPoint = 0 then
       fPrimObject.Points.Add(CurrPoint2D)
      else
       begin
         if fOrtoIsUsable and UseOrto then
          MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
         fPrimObject.Points[fCurrPoint] := CurrPoint2D;
       end;
      if (fCurrPoint = 0) or not IsSamePoint2D(CurrPoint2D, fPrimObject.Points[fCurrPoint - 1]) then
       Inc(fCurrPoint);
      fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint2D := fPrimObject.WorldToObject(CurrentViewportSnappedPoint);
      if fOrtoIsUsable and UseOrto then
       MakeOrto2D(fPrimObject.Points[fCurrPoint - 1], CurrPoint2D);
      DrawOSD(Viewport2D);
      fPrimObject.Points[fCurrPoint] := CurrPoint2D;
      DrawOSD(Viewport2D);
    end;
    cePaint: DrawOSD(Viewport2D);
   end;
end;

procedure TCAD2DDrawUnSizedPrimitive.OnStop;
begin
  TCAD2DDrawUnSizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD2DDrawArcPrimitiveParam.Create(AfterS: TCADStateClass; Arc: TArc2D);
begin
  inherited Create(AfterS);
  fArcObject := Arc;
end;

constructor TCAD2DDrawArcPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DDrawArcPrimitiveParam) then
   Raise ECADSysException.Create('TCAD2DDrawArcPrimitive: Invalid param');
  Description := 'Drag the ellipse which contain the arc.';
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DDrawArcPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DDrawArcPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         try
          Viewport2D.CADCmp2D.AddObject(fArcObject.ID, fArcObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fArcObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
      CurrPoint2D := fArcObject.WorldToObject(CurrentViewportSnappedPoint);
      SnapOriginPoint := CurrentViewportSnappedPoint;
      fArcObject.Points[fCurrPoint] := CurrPoint2D;
      Inc(fCurrPoint);
      if fCurrPoint = 4 then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         TmpBool := Viewport2D.CADCmp2D.DrawOnAdd;
         Viewport2D.CADCmp2D.DrawOnAdd := True;
         IgnoreEvents := True;
         try
          Viewport2D.CADCmp2D.AddObject(fArcObject.ID, fArcObject);
         finally
           Viewport2D.CADCmp2D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
         Exit;
       end
      else if fCurrPoint = 0 then
       begin
         fArcObject.Points[0] := CurrPoint2D;
         fArcObject.Points[1] := CurrPoint2D;
         fArcObject.Points[2] := CurrPoint2D;
         fArcObject.Points[3] := CurrPoint2D;
       end
      else if fCurrPoint = 2 then
       begin
         fArcObject.StartAngle := 0;
         fArcObject.EndAngle := 0;
         Description := 'Select the start and end angle of the arc.'
       end;
      fArcObject.Points[fCurrPoint] := CurrPoint2D;
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint2D := fArcObject.WorldToObject(CurrentViewportSnappedPoint);
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
      fArcObject.Points[fCurrPoint] := CurrPoint2D;
      Viewport2D.DrawObject2DWithRubber(fArcObject, True);
    end;
    cePaint: Viewport2D.DrawObject2DWithRubber(fArcObject, True);
   end;
end;

procedure TCAD2DDrawArcPrimitive.OnStop;
begin
  TCAD2DDrawArcPrimitiveParam(Param).fArcObject.Free;
  Param.Free;
  Param := nil;
end;

{ ******************* Editing tasks *********************** }

constructor TCAD2DSelectObjectsParam.Create(ApertureSize: Word; const AfterS: TCADStateClass);
begin
  inherited Create(AfterS);

  fApertureSize := ApertureSize;
  fSelectedObjs := TGraphicObjList.Create;
  fSelectedObjs.FreeOnClear := False;
  fSelectionFilter := TObject2D;
  fEndWithMouseDown := False;
  fEndIfNoObject := False;
end;

destructor TCAD2DSelectObjectsParam.Destroy;
begin
  fSelectedObjs.Free;
  inherited;
end;

procedure TCAD2DSelectObjectsParam.DrawOSD(Viewport: TCADViewport2D; const Pt: TPoint2D);
var
  ScrPt: TPoint;
begin
  with Viewport do
   begin
     ScrPt := Point2DToPoint(ViewportToScreen(Pt));
     OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
     OnScreenCanvas.Canvas.Pen.Style := psSolid;
     OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
   end;
end;

constructor TCAD2DSelectObject.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DSelectObject: Invalid param');
  Description := 'Use the mouse to select an object.';
  with TCAD2DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DSelectObject.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if fEndWithMouseDown and (MouseButton = cmbLeft) then begin
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) then
       begin
         fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
           if Assigned(fOnSelected) then
            fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
           Viewport2D.Refresh;
         finally
           IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
            NextState := DefaultState;
          end;
         Result := True;
         Exit;
       end;
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) then
       begin
         fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
           if Assigned(fOnSelected) then
            fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
           Viewport2D.Refresh;
         finally
           IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
            NextState := DefaultState;
          end;
         Result := True;
       end;
    end;
    ceMouseMove: begin
      DrawOSD(Viewport2D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport2D, fLastPt);
    end;
    cePaint: DrawOSD(Viewport2D, fLastPt);
   end;
end;

procedure TCAD2DSelectObject.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD2DSelectObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DSelectObjects: Invalid param');
  Description := 'Use the mouse to select objects.';
  with TCAD2DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DSelectObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
  TmpExIter: TExclusiveGraphicObjIterator;
  TmpIter: TGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         IgnoreEvents := True;
         try
          Viewport2D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
            NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
          end;
         Result := True;
       end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D, fLastPt);
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         Removed := False;
         TmpExIter := fSelectedObjs.GetExclusiveIterator;
         try
           if TmpExIter.Search(TmpObj.ID) <> nil then
            begin
              TmpExIter.RemoveCurrent;
              Removed := True;
            end;
         finally
           TmpExIter.Free;
         end;
         if not Removed then
          fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
          if Assigned(fOnSelected) then
           fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, not Removed);
         finally
          IgnoreEvents := False;
         end;
       end;
      DrawOSD(Viewport2D, fLastPt);
    end;
    ceMouseMove: begin
      DrawOSD(Viewport2D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport2D, fLastPt);
    end;
    cePaint: begin
      DrawOSD(Viewport2D, fLastPt);
      if Assigned(fOnSelected) then
       begin
         IgnoreEvents := True;
         TmpIter := fSelectedObjs.GetIterator;
         try
           TmpIter.First;
           while TmpIter.Current <> nil do
            begin
              fOnSelected(TCAD2DSelectObjectsParam(Param), TObject2D(TmpIter.Current), PICK_NOOBJECT, True);
              TmpIter.Next;
            end;
         finally
          TmpIter.Free;
          IgnoreEvents := False;
         end;
       end;
    end;
   end;
end;

procedure TCAD2DSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD2DExtendedSelectObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DExtendedSelectObjects: Invalid param');
  Description := 'Use the mouse to select one object, hold shift key pressed to select more than one object.';
  with TCAD2DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg2D(CADPrg).Viewport2D, fLastPt);
end;

function TCAD2DExtendedSelectObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                             var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject2D;
  TmpExIter: TExclusiveGraphicObjIterator;
  TmpIter: TGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         IgnoreEvents := True;
         try
          Viewport2D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
            NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
          end;
         Result := True;
       end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport2D, fLastPt);
      TmpObj := Viewport2D.PickObject(CurrentViewportPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         Removed := False;
         TmpExIter := fSelectedObjs.GetExclusiveIterator;
         try
           if TmpExIter.Search(TmpObj.ID) <> nil then
            begin
              TmpExIter.RemoveCurrent;
              Removed := True;
            end;
         finally
           TmpExIter.Free;
         end;
         if not Removed then
          fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
          if Assigned(fOnSelected) then
           fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, not Removed);
         finally
          IgnoreEvents := False;
         end;
         // Controlla se il tasto del mouse è premuto.
         if Key <> VK_SHIFT then
          begin // No allora si comporta come selezione di un solo oggetto.
            if Removed then
             fSelectedObjs.Add(TmpObj);
            IgnoreEvents := True;
            try
              if Assigned(fOnSelected) then
               fOnSelected(TCAD2DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
              Viewport2D.Refresh;
            finally
             IgnoreEvents := False;
            end;
            if Assigned(AfterState) then
             NextState := AfterState
            else
             begin
               Param.Free;
               Param := nil;
               NextState := DefaultState;
             end;
            Result := True;
            Exit;
          end;
       end;
      DrawOSD(Viewport2D, fLastPt);
    end;
    ceMouseMove: begin
      DrawOSD(Viewport2D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport2D, fLastPt);
    end;
    cePaint: begin
      DrawOSD(Viewport2D, fLastPt);
      if Assigned(fOnSelected) then
       begin
         IgnoreEvents := True;
         TmpIter := fSelectedObjs.GetExclusiveIterator;
         try
           TmpIter.First;
           while TmpIter.Current <> nil do
            begin
              fOnSelected(TCAD2DSelectObjectsParam(Param), TObject2D(TmpIter.Current), PICK_NOOBJECT, True);
              TmpIter.Next;
            end;
         finally
          TmpIter.Free;
          IgnoreEvents := False;
         end;
       end;
    end;
   end;
end;

procedure TCAD2DExtendedSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;



constructor TCAD2DSelectObjectsInAreaParam.Create(AreaMode: TGroupMode; const AfterS: TCADStateClass);
begin
  inherited Create(0, AfterS);

  fAreaMode := AreaMode;
end;

constructor TCAD2DSelectObjectsInArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCADPrgParam;
  LastFilt: TObject2DClass;
begin
  inherited;
  if StateParam is TCADPrgSelectAreaParam then
   begin // Return from Select area.
     NewParam := TCADPrgSelectAreaParam(Param).CallerParam;
     TCADPrgSelectAreaParam(Param).CallerParam := nil;
     TCAD2DSelectObjectsInAreaParam(NewParam).fArea := TCADPrgSelectAreaParam(StateParam).Area;
     Param.Free;
     Param := NewParam; // Set the parameter back to the original.
     with CADPrg as TCADPrg2D, Param as TCAD2DSelectObjectsInAreaParam do
      begin
        LastFilt := Viewport2D.PickFilter;
        try
          Viewport2D.PickFilter := fSelectionFilter;
          Viewport2D.GroupObjects(fSelectedObjs, fArea, fAreaMode, False);
        finally
          Viewport2D.PickFilter := LastFilt;
        end;
        if Assigned(fOnSelected) then
         fOnSelected(TCAD2DSelectObjectsParam(Param), nil, PICK_NOOBJECT, True);
        IgnoreEvents := True;
        try
          Viewport2D.Refresh;
        finally
         IgnoreEvents := False;
        end;
        if Assigned(AfterState) then
         NextState := AfterState
        else
         begin
           Param.Free;
           Param := nil;
           NextState := DefaultState;
         end;
      end;
   end
  else if not (StateParam is TCAD2DSelectObjectsInAreaParam) then
   Raise ECADSysException.Create('TCAD2DSelectObjectsInArea: Invalid param')
  else
   begin
     NewParam := TCADPrgSelectAreaParam.Create(TCAD2DSelectObjectsInArea, Param);
     Param := NewParam;  // Set the parameter to the select area param.
     NextState := TCADPrgSelectArea;
   end;
end;

procedure TCAD2DSelectObjectsInArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ---------------------------- }

procedure TCAD2DTransformObjectsParam.TransformObjs(CurrPt: TPoint2D; UseOrto: Boolean);
begin
  if UseOrto then
   MakeOrto2D(fBasePt, CurrPt);
  fCurrTransf := GetTransform(fBasePt, CurrPt);
end;

procedure TCAD2DTransformObjectsParam.DrawWithFrame(Viewport: TCADViewport2D);
begin
  with Viewport do
   begin
     OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
     DrawBoundingBox2D(OnScreenCanvas, fBox,
                       RectToRect2D(OnScreenCanvas.Canvas.ClipRect),
                       MultiplyTransform2D(fCurrTransf, ViewportToScreenTransform));
   end;
end;

procedure TCAD2DTransformObjectsParam.DrawWithoutFrame(Viewport: TCADViewport2D);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := fObjs.GetIterator;
  with Viewport do
   try
     TmpObj := TmpIter.First as TObject2D;
     while TmpObj <> nil do
      begin
        TmpObj.ModelTransform := fCurrTransf;
        DrawObject2DWithRubber(TmpObj, False);
        TmpObj := TmpIter.Next as TObject2D;
      end;
   finally
     TmpIter.Free;
   end;
end;

procedure TCAD2DTransformObjectsParam.DrawOSD(Viewport: TCADViewport2D);
begin
  Viewport.OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
  if fUseFrame then
   DrawWithFrame(Viewport)
  else
   DrawWithoutFrame(Viewport);
end;

procedure TCAD2DTransformObjectsParam.ConfirmTransform;
var
  TmpObj: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fObjs.GetExclusiveIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       TmpObj.ModelTransform := fCurrTransf;
       TmpObj.ApplyTransform;
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
  end;
end;

procedure TCAD2DTransformObjectsParam.CancelTransform;
var
  TmpObj: TObject2D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fObjs.GetExclusiveIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       TmpObj.ModelTransform := IdentityTransf2D;
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
  end;
end;

constructor TCAD2DTransformObjectsParam.Create(Objs: TGraphicObjList; const AfterS: TCADStateClass);
var
  TmpObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  inherited Create(AfterS);

  fObjs := TGraphicObjList.Create;
  fObjs.FreeOnClear := False;
  fCurrTransf := IdentityTransf2D;
  if Objs.Count = 0 then
   Raise ECADSysException.Create('TCAD2DTransformObjectsParam: Invalid list');
  // Recupera il BBox a conferma la trasformazione Obj corrente.
  fUseFrame := Objs.Count > 1;
  TmpIter := Objs.GetIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    fBox := TmpObj.Box;
    while TmpObj <> nil do
     begin
       fObjs.Add(TmpObj);
       TmpObj.ApplyTransform;
       fBox := BoxOutBox2D(fBox, TmpObj.Box);
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
  end;
end;

destructor TCAD2DTransformObjectsParam.Destroy;
begin
  fObjs.Free;
  inherited;
end;


function TCAD2DMoveObjectsParam.GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D;
begin
  Result := Translate2D(CurrPt.X - BasePt.X, CurrPt.Y - BasePt.Y);
end;

function TCAD2DRotateObjectsParam.GetTransform(BasePt, CurrPt: TPoint2D): TTransf2D;
var
  A: TRealType;
begin
  A := ArcTan2(CurrPt.Y - BasePt.Y, CurrPt.X - BasePt.X);
  Result := Translate2D(-BasePt.X, -BasePt.Y);
  Result := MultiplyTransform2D(Result, Rotate2D(A));
  Result := MultiplyTransform2D(Result, Translate2D(BasePt.X, BasePt.Y));
end;


constructor TCAD2DTransformObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD2DTransformObjectsParam) then
   Raise ECADSysException.Create('TCAD2DTransformObjects: Invalid param');
  Description := 'Select the base point for the transformation.';
  with TCAD2DTransformObjectsParam(StateParam) do
   DrawWithFrame(TCADPrg2D(CADPrg).Viewport2D);
end;

function TCAD2DTransformObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DTransformObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         CancelTransform;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         ConfirmTransform;
         IgnoreEvents := True;
         try
          Viewport2D.Repaint;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      CurrPoint2D := CurrentViewportSnappedPoint;
      if fNPoint = 0 then
       begin
         fBasePt := CurrPoint2D;
         Description := 'Move the mouse to modify the transformation and press the mouse to apply it.';
       end
      else
       begin
         ConfirmTransform;
         IgnoreEvents := True;
         try
          Viewport2D.Repaint;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         Param.Free;
         Param := nil;
         Result := True;
         NextState := DefaultState;
         Exit;
       end;
      DrawOSD(Viewport2D);
      Inc(fNPoint);
    end;
    ceMouseMove: if fNPoint > 0 then begin
      DrawOSD(Viewport2D);
      CurrPoint2D := CurrentViewportSnappedPoint;
      TransformObjs(CurrPoint2D, UseOrto);
      DrawOSD(Viewport2D);
    end;
    cePaint: begin
      DrawOSD(Viewport2D);
    end;
   end;
end;

procedure TCAD2DTransformObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD2DMoveSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD2DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DMoveSelectedObjects: Invalid param');
  TmpList := TCAD2DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD2DMoveObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DMoveSelectedObjects;
  NextState := TCAD2DTransformObjects;
end;

constructor TCAD2DRotateSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD2DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DRotateSelectedObjects: Invalid param');
  TmpList := TCAD2DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD2DRotateObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DRotateSelectedObjects;
  NextState := TCAD2DTransformObjects;
end;


constructor TCAD2DEditPrimitiveParam.Create(Prim: TPrimitive2D; ApertureSize: Word);
begin
  inherited Create(nil);

  fOriginalPrimitive := Prim;
  fCurrentPrimitive := CADSysFindClassByName(fOriginalPrimitive.ClassName).Create(0) as TPrimitive2D;
  if not Assigned(fCurrentPrimitive) then
   Raise ECADSysException.Create('TCAD2DEditPrimitive: Only registered classes are allowed');
  fCurrentPrimitive.Assign(fOriginalPrimitive);
  fApertureSize := ApertureSize;
  fCurrentCtrlPt := -1;
end;

destructor TCAD2DEditPrimitiveParam.Destroy;
begin
  fCurrentPrimitive.Free;
  inherited;
end;

procedure TCAD2DEditPrimitiveParam.SetCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
var
  TmpDist: TRealType;
  TmpAp: TRealType;
begin
  with Viewport do
   begin
     TmpAp := GetAperture(fApertureSize);
     TmpDist := 0;
     fCurrentCtrlPt := fCurrentPrimitive.OnMe(Pt, TmpAp, TmpDist);
   end;
end;

procedure TCAD2DEditPrimitiveParam.AddCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
var
  TmpCPt: TPoint2D;
begin
  if fCurrentCtrlPt > -1 then
   begin
     DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
     TmpCPt := Viewport.WorldToObject(fCurrentPrimitive, Pt);
     fCurrentPrimitive.Points.Insert(fCurrentCtrlPt, TmpCPt);
     DrawModifiedPrim(Viewport);
   end;
end;

procedure TCAD2DEditPrimitiveParam.UnSetCtrlPoint;
begin
  fCurrentCtrlPt := -1;
end;

procedure TCAD2DEditPrimitiveParam.AcceptEdited;
begin
  fOriginalPrimitive.Assign(fCurrentPrimitive);
end;

procedure TCAD2DEditPrimitiveParam.MoveCtrlPoint(Viewport: TCADViewport2D; Pt: TPoint2D);
var
  TmpCPt: TPoint2D;
begin
  if fCurrentCtrlPt > -1 then
   begin
     DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
     TmpCPt := Viewport.WorldToObject(fCurrentPrimitive, Pt);
     fCurrentPrimitive.Points[fCurrentCtrlPt] := TmpCPt;
     DrawModifiedPrim(Viewport);
   end;
end;

procedure TCAD2DEditPrimitiveParam.DrawOSD(Viewport: TCADViewport2D; Pt: TPoint2D; FirstTime: Boolean);
var
  ScrPt: TPoint;
begin
  with Viewport do
   begin
     OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
     if not FirstTime then
      begin
        ScrPt := Point2DToPoint(ViewportToScreen(fLastPt));
        OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                        Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                        Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                        Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                        Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
      end;
     fLastPt := Pt;
     ScrPt := Point2DToPoint(ViewportToScreen(fLastPt));
     OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
   end;
end;

procedure TCAD2DEditPrimitiveParam.DrawModifiedPrim(Viewport: TCADViewport2D);
begin
  with Viewport do
   DrawObject2DWithRubber(fCurrentPrimitive, True);
end;

constructor TCAD2DEditPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD2DEditPrimitiveParam;
begin
  inherited;
  if not (StateParam is TCADPrgParam) or not (TCADPrgParam(StateParam).UserObject is TPrimitive2D) then
   Raise ECADSysException.Create('TCAD2DEditPrimitive: Invalid param');
  Description := 'Select a Control point of the primitive';
  NewParam := TCAD2DEditPrimitiveParam.Create(TPrimitive2D(TCADPrgParam(StateParam).UserObject), 5);
  with TCADPrg2D(CADPrg) do
   begin
     Viewport2D.Refresh;
     NewParam.DrawOSD(Viewport2D, Point2D(0, 0), True);
     NewParam.DrawModifiedPrim(Viewport2D);
   end;
  Param := NewParam;
  TCADPrg2D(CADPrg).SnapOriginPoint := Point2D(MaxCoord, MaxCoord);
end;

function TCAD2DEditPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint2D: TPoint2D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg2D, Param as TCAD2DEditPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         AcceptEdited;
         IgnoreEvents := True;
         try
          Viewport2D.Repaint;
         finally
          IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      SetCtrlPoint(Viewport2D, CurrentViewportPoint);
      if CurrentCtrlPt >= 0 then
       Description := 'Move the control point and release the mouse.';
    end;
    ceMouseDblClick: if (MouseButton = cmbLeft) and (fCurrentPrimitive.Points.GrowingEnabled) then begin
      SetCtrlPoint(Viewport2D, CurrentViewportPoint);
      if CurrentCtrlPt >= 0 then
       begin
         CurrPoint2D := CurrentViewportSnappedPoint;
         AddCtrlPoint(Viewport2D, CurrPoint2D);
       end;
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      UnSetCtrlPoint;
      Description := 'Select a Control point of the primitive';
    end;
    ceMouseMove: begin
      CurrPoint2D := CurrentViewportSnappedPoint;
      DrawOSD(Viewport2D, CurrentViewportPoint, False);
      MoveCtrlPoint(Viewport2D, CurrPoint2D);
      DrawOSD(Viewport2D, CurrentViewportPoint, False);
    end;
    cePaint: begin
      DrawOSD(Viewport2D, CurrentViewportPoint, True);
      DrawModifiedPrim(Viewport2D);
    end;
   end;
end;

procedure TCAD2DEditPrimitive.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD2DEditSelectedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCADPrgParam;
  NewObj: TObject2D;
  TmpIter: TGraphicObjIterator;
begin
  inherited;
  if not (StateParam is TCAD2DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD2DEditSelectedObjects: Invalid param');
  with TCAD2DSelectObjectsParam(StateParam) do
   begin
     TmpIter := SelectedObjects.GetIterator;
     try
      NewObj := TmpIter.First as TObject2D;
     finally
      TmpIter.Free;
     end;
     if not (NewObj is TPrimitive2D) then
      Raise ECADSysException.Create('TCAD2DEditSelectedObjects: Invalid param');
     NewParam := TCADPrgParam.Create(nil);
     NewParam.UserObject := NewObj;
   end;
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD2DEditSelectedPrimitive;
  NextState := TCAD2DEditPrimitive;
end;

// -----===== Starting Cs4CADPrgTasks3D.pas =====-----

{ ******************* General tasks *********************** }

function TCAD3DSelectSegmentParam.GetStart: TPoint3D;
begin
  Result := fLine.Points[0];
end;

function TCAD3DSelectSegmentParam.GetEnd: TPoint3D;
begin
  Result := fLine.Points[1];
end;

constructor TCAD3DSelectSegmentParam.Create(AfterS: TCADStateClass);
begin
  inherited Create(AfterS);
  fLine := TLine3D.Create(-1, Point3D(0, 0, 0), Point3D(0, 0, 0));
  fCurrPoint := 0;
end;

destructor TCAD3DSelectSegmentParam.Destroy;
begin
  fLine.Free;
  inherited;
end;

constructor TCAD3DSelectSegment.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  Description := 'Press the mouse on the start point of the segment.';
  TCADPrg3D(CADPrg).SnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
end;

function TCAD3DSelectSegment.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                     var NextState: TCADStateClass): Boolean;
var
  CurrPoint3D: TPoint3D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DSelectSegmentParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      Viewport3D.DrawObject3DWithRubber(fLine, True);
      CurrPoint3D := CurrentWorkingPlaneSnappedPoint;
      SnapOriginPoint := CurrPoint3D;
      if fCurrPoint = 0 then
       begin
         fLine.Points[0] := CurrPoint3D;
         fLine.Points[1] := CurrPoint3D;
         Description := 'Press the mouse on the end point of the segment.'
       end
      else
       fLine.Points[1] := CurrPoint3D;
      Inc(fCurrPoint);
      if fCurrPoint = 2 then
       begin
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
            NextState := DefaultState;
          end;
         Result := True;
         Exit;
       end;
      Viewport3D.DrawObject3DWithRubber(fLine, True);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      Viewport3D.DrawObject3DWithRubber(fLine, True);
      fLine.Points[fCurrPoint] := CurrentWorkingPlaneSnappedPoint;
      Viewport3D.DrawObject3DWithRubber(fLine, True);
    end;
    cePaint: Viewport3D.DrawObject3DWithRubber(fLine, True);
   end;
end;

procedure TCAD3DSelectSegment.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ******************* Drawing tasks *********************** }

constructor TCAD3DPositionObjectParam.Create(AfterS: TCADStateClass; O: TObject3D);
begin
  inherited Create(AfterS);
  fObject := O;
end;

constructor TCAD3DDrawUnSizedPrimitiveParam.Create(AfterS: TCADStateClass; Primitive: TPrimitive3D; StartPointIdx: Integer; OrtoIsU: Boolean);
begin
  inherited Create(AfterS);
  fPrimObject := Primitive;
  fCurrPoint := StartPointIdx;
  fOrtoIsUsable := OrtoIsU;
end;

procedure TCAD3DDrawUnSizedPrimitiveParam.DrawOSD(Viewport: TCADViewport3D);
begin
  Viewport.DrawObject3DWithRubber(fPrimObject, True);
end;

constructor TCAD3DDrawSizedPrimitiveParam.Create(AfterS: TCADStateClass; Primitive: TPrimitive3D; StartPointIdx: Integer; OrtoIsU: Boolean);
begin
  inherited Create(AfterS, Primitive, StartPointIdx, OrtoIsU);
  fnPoints := Primitive.Points.Count;
end;

{----------}

constructor TCAD3DPositionObject.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD3DPositionObjectParam) then
   Raise ECADSysException.Create('TCAD3DPositionObject: Invalid param');
  Description := 'Press the mouse on the desired insertion point.';
  TCADPrg3D(CADPrg).SnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
  with CADPrg as TCADPrg3D, Param as TCAD3DPositionObjectParam do
   Viewport3D.DrawObject3DWithRubber(fObject, True);
end;

function TCAD3DPositionObject.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                      var NextState: TCADStateClass): Boolean;
var
  CurrPoint3D: TPoint3D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DPositionObjectParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         fObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      if Assigned(AfterState) then
       begin
         NextState := AfterState;
         Result := True;
         Exit;
       end;
      IgnoreEvents := True;
      TmpBool := Viewport3D.CADCmp3D.DrawOnAdd;
      Viewport3D.CADCmp3D.DrawOnAdd := True;
      try
       Viewport3D.CADCmp3D.AddObject(fObject.ID, fObject);
      finally
        Viewport3D.CADCmp3D.DrawOnAdd := TmpBool;
        IgnoreEvents := False;
      end;
      fObject.UpdateExtension(Self);
      Param.Free;
      Param := nil;
      NextState := DefaultState;
      Result := True;
      Exit;
    end;
    ceMouseMove: begin
      Viewport3D.DrawObject3DWithRubber(fObject, True);
      CurrPoint3D := CurrentWorkingPlaneSnappedPoint;
      fObject.MoveTo(CurrPoint3D, fObject.Box.FirstEdge);
      Viewport3D.DrawObject3DWithRubber(fObject, True);
    end;
    cePaint: Viewport3D.DrawObject3DWithRubber(fObject, True);
   end;
end;

procedure TCAD3DPositionObject.OnStop;
begin
  TCAD3DPositionObjectParam(Param).fObject.Free;
  Param.Free;
  Param := nil;
end;

{----------}

constructor TCAD3DDrawSizedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD3DDrawSizedPrimitiveParam) then
   Raise ECADSysException.Create('TCAD3DDrawSizedPrimitive3D: Invalid param');
  Description := 'Press the mouse on the desired points.';
  TCADPrg3D(CADPrg).SnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
end;

function TCAD3DDrawSizedPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                          var NextState: TCADStateClass): Boolean;
var
  CurrPoint3D: TPoint3D;
  Cont: Integer;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DDrawSizedPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport3D.CADCmp3D.DrawOnAdd;
         Viewport3D.CADCmp3D.DrawOnAdd := True;
         try
          Viewport3D.CADCmp3D.AddObject(fPrimObject.ID, fPrimObject);
         finally
           Viewport3D.CADCmp3D.DrawOnAdd := TmpBool;
          IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fPrimObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport3D);
      CurrPoint3D := fPrimObject.WorldToObject(CurrentWorkingPlaneSnappedPoint);
      SnapOriginPoint := CurrentWorkingPlaneSnappedPoint;
      if fCurrPoint = 0 then
       for Cont := 0 to fnPoints - 1 do
        fPrimObject.Points[Cont] := CurrPoint3D
      else
       begin
         if fOrtoIsUsable and UseOrto then
          MakeOrto3D(fPrimObject.Points[fCurrPoint - 1], CurrPoint3D, WorkingPlaneOrigin, WorkingPlaneNormal, WorkingPlaneUP);
         fPrimObject.Points[fCurrPoint] := CurrPoint3D;
       end;
      Inc(fCurrPoint);
      if fCurrPoint = fnPoints then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport3D.CADCmp3D.DrawOnAdd;
         Viewport3D.CADCmp3D.DrawOnAdd := True;
         try
          Viewport3D.CADCmp3D.AddObject(fPrimObject.ID, fPrimObject);
         finally
           Viewport3D.CADCmp3D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         fPrimObject.UpdateExtension(Self);
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
         Exit;
       end;
      fPrimObject.Points[fCurrPoint] := CurrPoint3D;
      DrawOSD(Viewport3D);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint3D := fPrimObject.WorldToObject(CurrentWorkingPlaneSnappedPoint);
      if fOrtoIsUsable and UseOrto then
       MakeOrto3D(fPrimObject.Points[fCurrPoint - 1], CurrPoint3D, WorkingPlaneOrigin, WorkingPlaneNormal, WorkingPlaneUP);
      DrawOSD(Viewport3D);
      fPrimObject.Points[fCurrPoint] := CurrPoint3D;
      DrawOSD(Viewport3D);
    end;
    cePaint: DrawOSD(Viewport3D);
   end;
end;

procedure TCAD3DDrawSizedPrimitive.OnStop;
begin
  TCAD3DDrawSizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{----------}

constructor TCAD3DDrawUnSizedPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD3DDrawUnSizedPrimitiveParam) then
   Raise ECADSysException.Create('TCAD3DDrawUnSizedPrimitive3D: Invalid param');
  TCAD3DDrawUnSizedPrimitiveParam(StateParam).fPrimObject.Points.Clear;
  Description := 'Press the mouse on the desired points.';
  TCADPrg3D(CADPrg).SnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
end;

function TCAD3DDrawUnSizedPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                       var NextState: TCADStateClass): Boolean;
var
  CurrPoint3D: TPoint3D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DDrawUnSizedPrimitiveParam do
   case Event of
    ceUserDefined:
      if (Key = CADPRG_ACCEPT) and (fCurrPoint > 0) then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport3D.CADCmp3D.DrawOnAdd;
         Viewport3D.CADCmp3D.DrawOnAdd := True;
         try
          Viewport3D.CADCmp3D.AddObject(fPrimObject.ID, fPrimObject);
         finally
           Viewport3D.CADCmp3D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fPrimObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport3D);
      CurrPoint3D := fPrimObject.WorldToObject(CurrentWorkingPlaneSnappedPoint);
      SnapOriginPoint := CurrentWorkingPlaneSnappedPoint;
      if fCurrPoint = 0 then
       fPrimObject.Points.Add(CurrPoint3D)
      else
       begin
         if fOrtoIsUsable and UseOrto then
          MakeOrto3D(fPrimObject.Points[fCurrPoint - 1], CurrPoint3D, WorkingPlaneOrigin, WorkingPlaneNormal, WorkingPlaneUP);
         fPrimObject.Points[fCurrPoint] := CurrPoint3D;
       end;
      if (fCurrPoint = 0) or not IsSamePoint3D(CurrPoint3D, fPrimObject.Points[fCurrPoint - 1]) then
       Inc(fCurrPoint);
      fPrimObject.Points[fCurrPoint] := CurrPoint3D;
      DrawOSD(Viewport3D);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint3D := fPrimObject.WorldToObject(CurrentWorkingPlaneSnappedPoint);
      if fOrtoIsUsable and UseOrto then
       MakeOrto3D(fPrimObject.Points[fCurrPoint - 1], CurrPoint3D, WorkingPlaneOrigin, WorkingPlaneNormal, WorkingPlaneUP);
      DrawOSD(Viewport3D);
      fPrimObject.Points[fCurrPoint] := CurrPoint3D;
      DrawOSD(Viewport3D);
    end;
    cePaint: DrawOSD(Viewport3D);
   end;
end;

procedure TCAD3DDrawUnSizedPrimitive.OnStop;
begin
  TCAD3DDrawUnSizedPrimitiveParam(Param).fPrimObject.Free;
  Param.Free;
  Param := nil;
end;

{ ------------- }

constructor TCAD3DDrawArcPrimitiveParam.Create(AfterS: TCADStateClass; Arc: TArc3D);
begin
  inherited Create(AfterS);
  fArcObject := Arc;
end;

constructor TCAD3DDrawArcPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD3DDrawArcPrimitiveParam) then
   Raise ECADSysException.Create('TCAD3DDrawArcPrimitive: Invalid param');
  Description := 'Drag the ellipse which contain the arc.';
  TCADPrg3D(CADPrg).SnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
end;

function TCAD3DDrawArcPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint3D: TPoint3D;
  TmpBool: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DDrawArcPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_ACCEPT then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         IgnoreEvents := True;
         TmpBool := Viewport3D.CADCmp3D.DrawOnAdd;
         Viewport3D.CADCmp3D.DrawOnAdd := True;
         try
          Viewport3D.CADCmp3D.AddObject(fArcObject.ID, fArcObject);
         finally
           Viewport3D.CADCmp3D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_CANCEL then
       begin
         fArcObject.Free;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      Viewport3D.DrawObject3DWithRubber(fArcObject, True);
      CurrPoint3D := fArcObject.WorldToObject(CurrentWorkingPlaneSnappedPoint);
      SnapOriginPoint := CurrentWorkingPlaneSnappedPoint;
      fArcObject.Points[fCurrPoint] := CurrPoint3D;
      Inc(fCurrPoint);
      if fCurrPoint = 4 then
       begin
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         TmpBool := Viewport3D.CADCmp3D.DrawOnAdd;
         Viewport3D.CADCmp3D.DrawOnAdd := True;
         IgnoreEvents := True;
         try
          Viewport3D.CADCmp3D.AddObject(fArcObject.ID, fArcObject);
         finally
           Viewport3D.CADCmp3D.DrawOnAdd := TmpBool;
           IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
         Exit;
       end
      else if fCurrPoint = 0 then
       begin
         fArcObject.Points[0] := CurrPoint3D;
         fArcObject.Points[1] := CurrPoint3D;
         fArcObject.Points[2] := CurrPoint3D;
         fArcObject.Points[3] := CurrPoint3D;
       end
      else if fCurrPoint = 2 then
       begin
         fArcObject.StartAngle := 0;
         fArcObject.EndAngle := 0;
         Description := 'Select the start and end angle of the arc.'
       end;
      fArcObject.Points[fCurrPoint] := CurrPoint3D;
      Viewport3D.DrawObject3DWithRubber(fArcObject, True);
    end;
    ceMouseMove: if fCurrPoint > 0 then begin
      CurrPoint3D := fArcObject.WorldToObject(CurrentWorkingPlaneSnappedPoint);
      Viewport3D.DrawObject3DWithRubber(fArcObject, True);
      fArcObject.Points[fCurrPoint] := CurrPoint3D;
      Viewport3D.DrawObject3DWithRubber(fArcObject, True);
    end;
    cePaint: Viewport3D.DrawObject3DWithRubber(fArcObject, True);
   end;
end;

procedure TCAD3DDrawArcPrimitive.OnStop;
begin
  TCAD3DDrawArcPrimitiveParam(Param).fArcObject.Free;
  Param.Free;
  Param := nil;
end;

{ ******************* Editing tasks *********************** }

constructor TCAD3DSelectObjectsParam.Create(ApertureSize: Word; const AfterS: TCADStateClass; UserFlag: LongInt);
begin
  inherited Create(AfterS);

  fApertureSize := ApertureSize;
  fSelectedObjs := TGraphicObjList.Create;
  fSelectedObjs.FreeOnClear := False;
  fSelectionFilter := TObject3D;
  fUserFlag := UserFlag;
  fEndWithMouseDown := False;
  fEndIfNoObject := False;
end;

destructor TCAD3DSelectObjectsParam.Destroy;
begin
  fSelectedObjs.Free;
  inherited;
end;

procedure TCAD3DSelectObjectsParam.DrawOSD(Viewport: TCADViewport3D; const Pt: TPoint2D);
var
  ScrPt: TPoint;
begin
  with Viewport do
   begin
     ScrPt := Point2DToPoint(ViewportToScreen(Pt));
     OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
     OnScreenCanvas.Canvas.Pen.Style := psSolid;
     OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
   end;
end;

constructor TCAD3DSelectObject.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DSelectObject: Invalid param');
  Description := 'Use the mouse to select an object.';
  with TCAD3DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg3D(CADPrg).Viewport3D, fLastPt);
end;

function TCAD3DSelectObject.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject3D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if fEndWithMouseDown and (MouseButton = cmbLeft) then begin
      TmpObj := Viewport3D.PickObject(CurrentWorldPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
           if Assigned(fOnSelected) then
            fOnSelected(TCAD3DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
           Viewport3D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
            NextState := DefaultState;
          end;
         Result := True;
       end;
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      TmpObj := Viewport3D.PickObject(CurrentWorldPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
           if Assigned(fOnSelected) then
            fOnSelected(TCAD3DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
           Viewport3D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
            NextState := DefaultState;
          end;
         Result := True;
       end;
    end;
    ceMouseMove: begin
      DrawOSD(Viewport3D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport3D, fLastPt);
    end;
    cePaint: DrawOSD(Viewport3D, fLastPt);
   end;
end;

procedure TCAD3DSelectObject.OnStop;
begin
  Param.Free;
  Param := nil;
end;




constructor TCAD3DSelectObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DSelectObjects: Invalid param');
  Description := 'Use the mouse to select objects.';
  with TCAD3DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg3D(CADPrg).Viewport3D, fLastPt);
end;

function TCAD3DSelectObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                    var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject3D;
  TmpIter: TExclusiveGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         IgnoreEvents := True;
         try
          Viewport3D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
          end;
         Result := True;
       end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport3D, fLastPt);
      TmpObj := Viewport3D.PickObject(CurrentWorldPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         Removed := False;
         TmpIter := fSelectedObjs.GetExclusiveIterator;
         try
           if TmpIter.Search(TmpObj.ID) <> nil then
            begin
              TmpIter.RemoveCurrent;
              Removed := True;
            end;
         finally
           TmpIter.Free;
         end;
         if not Removed then
          fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
          if Assigned(fOnSelected) then
           fOnSelected(TCAD3DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, not Removed);
         finally
          IgnoreEvents := False;
         end;
       end;
      DrawOSD(Viewport3D, fLastPt);
    end;
    ceMouseMove: begin
      DrawOSD(Viewport3D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport3D, fLastPt);
    end;
    cePaint: begin
      DrawOSD(Viewport3D, fLastPt);
      if Assigned(fOnSelected) then
       begin
         IgnoreEvents := True;
         TmpIter := fSelectedObjs.GetExclusiveIterator;
         try
           TmpIter.First;
           while TmpIter.Current <> nil do
            begin
              fOnSelected(TCAD3DSelectObjectsParam(Param), TObject3D(TmpIter.Current), PICK_NOOBJECT, True);
              TmpIter.Next;
            end;
         finally
          TmpIter.Free;
          IgnoreEvents := False;
         end;
       end;
    end;
   end;
end;

procedure TCAD3DSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD3DExtendedSelectObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DSelectObjects: Invalid param');
  Description := 'Use the mouse to select one object, hold shift key pressed to select more than one object.';
  with TCAD3DSelectObjectsParam(StateParam) do
   DrawOSD(TCADPrg3D(CADPrg).Viewport3D, fLastPt);
end;

function TCAD3DExtendedSelectObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                             var NextState: TCADStateClass): Boolean;
var
  TmpObj: TObject3D;
  TmpExIter: TExclusiveGraphicObjIterator;
  TmpIter: TGraphicObjIterator;
  Removed: Boolean;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DSelectObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         IgnoreEvents := True;
         try
          Viewport3D.Refresh;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          NextState := AfterState
         else
          begin
            Param.Free;
            Param := nil;
          end;
         Result := True;
       end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      DrawOSD(Viewport3D, fLastPt);
      TmpObj := Viewport3D.PickObject(CurrentWorldPoint, fApertureSize, False, fLastSelectedCtrlPoint);
      if (fLastSelectedCtrlPoint > PICK_INBBOX) and Assigned(TmpObj) and (TmpObj is fSelectionFilter) and (fLastSelectedCtrlPoint > PICK_INBBOX) then
       begin
         Removed := False;
         TmpExIter := fSelectedObjs.GetExclusiveIterator;
         try
           if TmpExIter.Search(TmpObj.ID) <> nil then
            begin
              TmpExIter.RemoveCurrent;
              Removed := True;
            end;
         finally
           TmpExIter.Free;
         end;
         if not Removed then
          fSelectedObjs.Add(TmpObj);
         IgnoreEvents := True;
         try
          if Assigned(fOnSelected) then
           fOnSelected(TCAD3DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, not Removed);
         finally
          IgnoreEvents := False;
         end;
         // Controlla se il tasto del mouse è premuto.
         if Key <> VK_SHIFT then
          begin // No allora si comporta come selezione di un solo oggetto.
            if Removed then
             fSelectedObjs.Add(TmpObj);
            IgnoreEvents := True;
            try
              if Removed and Assigned(fOnSelected) then
               fOnSelected(TCAD3DSelectObjectsParam(Param), TmpObj, fLastSelectedCtrlPoint, True);
              Viewport3D.Refresh;
            finally
             IgnoreEvents := False;
            end;
            if Assigned(AfterState) then
             NextState := AfterState
            else
             begin
               Param.Free;
               Param := nil;
               NextState := DefaultState;
             end;
            Result := True;
            Exit;
          end;
       end;
      DrawOSD(Viewport3D, fLastPt);
    end;
    ceMouseMove: begin
      DrawOSD(Viewport3D, fLastPt);
      fLastPt := CurrentViewportPoint;
      DrawOSD(Viewport3D, fLastPt);
    end;
    cePaint: begin
      DrawOSD(Viewport3D, fLastPt);
      if Assigned(fOnSelected) then
       begin
         IgnoreEvents := True;
         TmpIter := fSelectedObjs.GetIterator;
         try
           TmpIter.First;
           while TmpIter.Current <> nil do
            begin
              fOnSelected(TCAD3DSelectObjectsParam(Param), TObject3D(TmpIter.Current), PICK_NOOBJECT, True);
              TmpIter.Next;
            end;
         finally
          TmpIter.Free;
          IgnoreEvents := False;
         end;
       end;
    end;
   end;
end;

procedure TCAD3DExtendedSelectObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;


constructor TCAD3DSelectObjectsInAreaParam.Create(AreaMode: TGroupMode; const AfterS: TCADStateClass);
begin
  inherited Create(0, AfterS, 0);

  fAreaMode := AreaMode;
end;

constructor TCAD3DSelectObjectsInArea.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCADPrgParam;
  LastFilt: TObject3DClass;
begin
  inherited;
  if StateParam is TCADPrgSelectAreaParam then
   begin // Return from Select area.
     NewParam := TCADPrgSelectAreaParam(Param).CallerParam;
     TCADPrgSelectAreaParam(Param).CallerParam := nil;
     TCAD3DSelectObjectsInAreaParam(NewParam).fArea := TCADPrgSelectAreaParam(StateParam).Area;
     Param.Free;
     Param := NewParam; // Set the parameter back to the original.
     with CADPrg as TCADPrg3D, Param as TCAD3DSelectObjectsInAreaParam do
      begin
        LastFilt := Viewport3D.PickFilter;
        try
          Viewport3D.PickFilter := fSelectionFilter;
          Viewport3D.GroupObjects(fSelectedObjs, fArea, fAreaMode, False);
        finally
          Viewport3D.PickFilter := LastFilt;
        end;
        IgnoreEvents := True;
        try
         if Assigned(fOnSelected) then
          fOnSelected(TCAD3DSelectObjectsParam(Param), nil, PICK_NOOBJECT, True);
         Viewport3D.Refresh;
        finally
         IgnoreEvents := False;
        end;
        if Assigned(AfterState) then
         NextState := AfterState
        else
         begin
           Param.Free;
           Param := nil;
           NextState := DefaultState;
         end;
      end;
   end
  else if not (StateParam is TCAD3DSelectObjectsInAreaParam) then
   Raise ECADSysException.Create('TCAD3DSelectObjectsInArea: Invalid param')
  else
   begin
     NewParam := TCADPrgSelectAreaParam.Create(TCAD3DSelectObjectsInArea, Param);
     Param := NewParam;  // Set the parameter to the select area param.
     NextState := TCADPrgSelectArea;
   end;
end;

procedure TCAD3DSelectObjectsInArea.OnStop;
begin
  Param.Free;
  Param := nil;
end;

{ ---------------------------- }

procedure TCAD3DTransformObjectsParam.TransformObjs(CurrPt: TPoint3D; CADPrg: TCADPrg3D);
begin
  if CADPrg.UseOrto then
   MakeOrto3D(fBasePt, CurrPt, CADPrg.WorkingPlaneOrigin, CADPrg.WorkingPlaneNormal, CADPrg.WorkingPlaneUP);
  fCurrTransf := GetTransform(fBasePt, CurrPt);
end;

procedure TCAD3DTransformObjectsParam.DrawWithFrame(Viewport: TCADViewport3D);
begin
  with Viewport do
   begin
     OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
     DrawBoundingBox3D(OnScreenCanvas,
        TransformPoint3D(VRP, InvertTransform3D(fCurrTransf)),
        fObjectsBox,
        MultiplyTransform3D(fCurrTransf, ViewNormalization),
        ViewMapping);
   end;
end;

procedure TCAD3DTransformObjectsParam.DrawWithoutFrame(Viewport: TCADViewport3D);
var
  TmpObj: TObject3D;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := fObjs.GetIterator;
  with Viewport do
   try
     TmpObj := TmpIter.First as TObject3D;
     while TmpObj <> nil do
      begin
        TmpObj.ModelTransform := fCurrTransf;
        DrawObject3DWithRubber(TmpObj, False);
        TmpObj := TmpIter.Next as TObject3D;
      end;
   finally
     TmpIter.Free;
   end;
end;

procedure TCAD3DTransformObjectsParam.DrawOSD(Viewport: TCADViewport3D);
begin
  Viewport.OnScreenCanvas.Canvas.Pen.Assign(Viewport.RubberPen);
  if fUseFrame then
   DrawWithFrame(Viewport)
  else
   DrawWithoutFrame(Viewport);
end;

procedure TCAD3DTransformObjectsParam.ConfirmTransform;
var
  TmpObj: TObject3D;
  TmpIter: TGraphicObjIterator;
begin
  TmpIter := fObjs.GetIterator;
  try
    TmpObj := TmpIter.First as TObject3D;
    while TmpObj <> nil do
     begin
       TmpObj.ModelTransform := fCurrTransf;
       TmpObj.ApplyTransform;
       TmpObj := TmpIter.Next as TObject3D;
     end;
  finally
    TmpIter.Free;
  end;
end;

procedure TCAD3DTransformObjectsParam.CancelTransform;
var
  TmpObj: TObject3D;
  TmpIter: TExclusiveGraphicObjIterator;
begin
  TmpIter := fObjs.GetExclusiveIterator;
  try
    TmpObj := TmpIter.First as TObject3D;
    while TmpObj <> nil do
     begin
       TmpObj.ModelTransform := IdentityTransf3D;
       TmpObj := TmpIter.Next as TObject3D;
     end;
  finally
    TmpIter.Free;
  end;
end;

constructor TCAD3DTransformObjectsParam.Create(Objs: TGraphicObjList; const AfterS: TCADStateClass);
var
  TmpObj: TObject3D;
  TmpIter: TGraphicObjIterator;
begin
  inherited Create(AfterS);

  fBasePt := Point3D(0, 0, 0);
  fObjs := TGraphicObjList.Create;
  fObjs.FreeOnClear := False;
  fCurrTransf := IdentityTransf3D;
  if Objs.Count = 0 then
   Raise ECADSysException.Create('TCAD3DTransformObjectsParam: Invalid list');
  // Recupera il BBox a conferma la trasformazione Obj corrente.
  TmpIter := Objs.GetIterator;
  try
    TmpObj := TmpIter.First as TObject3D;
    fUseFrame := (Objs.Count > 1) or (TmpObj is TExtrudedOutline3D);
    fObjectsBox := TmpObj.Box;
    while TmpObj <> nil do
     begin
       fObjs.Add(TmpObj);
       TmpObj.ApplyTransform;
       fObjectsBox := BoxOutBox3D(fObjectsBox, TmpObj.Box);
       TmpObj := TmpIter.Next as TObject3D;
     end;
  finally
    TmpIter.Free;
  end;
end;

destructor TCAD3DTransformObjectsParam.Destroy;
begin
  fObjs.Free;
  inherited;
end;

function TCAD3DScaleObjectsParam.GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D;
var
  ScaleFactor: TRealType;
begin
  // Calcola il fattore di scalatura.
  ScaleFactor := PointDistance3D(BasePt, CurrPt) /
                 PointDistance3D(fObjectsBox.FirstEdge, fObjectsBox.SecondEdge) * 4;
  if ScaleFactor = 0 then
   Result := IdentityTransf3D
  else
   begin
     Result := Translate3D(-BasePt.X, -BasePt.Y, -BasePt.Z);
     Result := MultiplyTransform3D(Result, Scale3D(ScaleFactor, ScaleFactor, ScaleFactor));
     Result := MultiplyTransform3D(Result, Translate3D(BasePt.X, BasePt.Y, BasePt.Z));
   end;
end;

function TCAD3DMoveObjectsParam.GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D;
begin
  Result := Translate3D(CurrPt.X - BasePt.X, CurrPt.Y - BasePt.Y, CurrPt.Z - BasePt.Z);
end;

function TCAD3DRotateOnXObjectsParam.GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D;
var
  A: TRealType;
begin
  A := ArcTan2(CurrPt.Z - BasePt.Z, CurrPt.Y - BasePt.Y);
  Result := Translate3D(-BasePt.X, -BasePt.Y, -BasePt.Z);
  Result := MultiplyTransform3D(Result, Rotate3DX(A));
  Result := MultiplyTransform3D(Result, Translate3D(BasePt.X, BasePt.Y, BasePt.Z));
end;

function TCAD3DRotateOnYObjectsParam.GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D;
var
  A: TRealType;
begin
  A := ArcTan2(CurrPt.X - BasePt.X, CurrPt.Z - BasePt.Z);
  Result := Translate3D(-BasePt.X, -BasePt.Y, -BasePt.Z);
  Result := MultiplyTransform3D(Result, Rotate3DY(A));
  Result := MultiplyTransform3D(Result, Translate3D(BasePt.X, BasePt.Y, BasePt.Z));
end;

function TCAD3DRotateOnZObjectsParam.GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D;
var
  A: TRealType;
begin
  A := ArcTan2(CurrPt.Y - BasePt.Y, CurrPt.X - BasePt.X);
  Result := Translate3D(-BasePt.X, -BasePt.Y, -BasePt.Z);
  Result := MultiplyTransform3D(Result, Rotate3DZ(A));
  Result := MultiplyTransform3D(Result, Translate3D(BasePt.X, BasePt.Y, BasePt.Z));
end;

constructor TCAD3DRotateOnPlaneObjectsParam.Create(Objs: TGraphicObjList; const WX, WY: TVector3D; const AfterS: TCADStateClass);
begin
  inherited Create(Objs, AfterS);

  fWX := WX;
  fWY := WY;
end;

function TCAD3DRotateOnPlaneObjectsParam.GetTransform(BasePt, CurrPt: TPoint3D): TTransf3D;
var
  A: TRealType;
  DX, DY: TRealType;
begin
  DX := DotProduct3D(Direction3D(BasePt, CurrPt), fWX);
  DY := DotProduct3D(Direction3D(BasePt, CurrPt), fWY);
  A := ArcTan2(DY, DX);
  Result := RotateOnAxis3D(BasePt, CrossProd3D(fWX, fWY), A);
end;

constructor TCAD3DTransformObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited;
  if not (StateParam is TCAD3DTransformObjectsParam) then
   Raise ECADSysException.Create('TCAD3DTransformObjects: Invalid param');
  Description := 'Select the base point for the transformation.';
  TCAD3DTransformObjectsParam(StateParam).DrawOSD(TCADPrg3D(CADPrg).Viewport3D);
  TCADPrg3D(CADPrg).SnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
end;

function TCAD3DTransformObjects.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint3D: TPoint3D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DTransformObjectsParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         CancelTransform;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         ConfirmTransform;
         IgnoreEvents := True;
         try
          Viewport3D.CADCmp3D.RepaintViewports;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      CurrPoint3D := CurrentWorkingPlaneSnappedPoint;
      SnapOriginPoint := CurrPoint3D;
      if fNPoint = 0 then
       begin
         fBasePt := CurrPoint3D;
         Description := 'Move the mouse to modify the transformation and press the mouse to apply it.';
       end
      else
       begin
         ConfirmTransform;
         IgnoreEvents := True;
         try
          Viewport3D.CADCmp3D.RepaintViewports;
         finally
          IgnoreEvents := False;
         end;
         if Assigned(AfterState) then
          begin
            NextState := AfterState;
            Result := True;
            Exit;
          end;
         Param.Free;
         Param := nil;
         Result := True;
         NextState := DefaultState;
         Exit;
       end;
      TransformObjs(CurrPoint3D, TCADPrg3D(CADPrg));
      DrawOSD(Viewport3D);
      Inc(fNPoint);
    end;
    ceMouseMove: if fNPoint > 0 then begin
      DrawOSD(Viewport3D);
      CurrPoint3D := CurrentWorkingPlaneSnappedPoint;
      TransformObjs(CurrPoint3D, TCADPrg3D(CADPrg));
      DrawOSD(Viewport3D);
    end;
    cePaint: begin
      DrawOSD(Viewport3D);
    end;
   end;
end;

procedure TCAD3DTransformObjects.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD3DMoveSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DMoveSelectedObjects: Invalid param');
  TmpList := TCAD3DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD3DMoveObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD3DMoveSelectedObjects;
  NextState := TCAD3DTransformObjects;
end;

constructor TCAD3DRotateOnXSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DRotateOnXSelectedObjects: Invalid param');
  TmpList := TCAD3DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD3DRotateOnXObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD3DRotateOnXSelectedObjects;
  NextState := TCAD3DTransformObjects;
end;

constructor TCAD3DRotateOnYSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DRotateOnXSelectedObjects: Invalid param');
  TmpList := TCAD3DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD3DRotateOnYObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD3DRotateOnYSelectedObjects;
  NextState := TCAD3DTransformObjects;
end;

constructor TCAD3DRotateOnZSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DRotateOnZSelectedObjects: Invalid param');
  TmpList := TCAD3DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  NewParam := TCAD3DRotateOnZObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD3DRotateOnZSelectedObjects;
  NextState := TCAD3DTransformObjects;
end;

constructor TCAD3DScaleSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DScaleSelectedObjects: Invalid param');
  TmpList := TCAD3DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  with CADPrg as TCADPrg3D do
   NewParam := TCAD3DScaleObjectsParam.Create(TmpList, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD3DScaleSelectedObjects;
  NextState := TCAD3DTransformObjects;
end;


constructor TCAD3DRotateOnWPSelectedObjects.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DTransformObjectsParam;
  TmpList: TGraphicObjList;
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DRotateOnWPSelectedObjects: Invalid param');
  TmpList := TCAD3DSelectObjectsParam(StateParam).SelectedObjects;
  if TmpList.Count = 0 then
   begin
     Param.Free;
     Param := nil;
     NextState := CADPrg.DefaultState;
     Exit;
   end;
  with CADPrg as TCADPrg3D do
   NewParam := TCAD3DRotateOnPlaneObjectsParam.Create(TmpList, WorkingPlaneXDir, WorkingPlaneUP, nil);
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD3DRotateOnWPSelectedObjects;
  NextState := TCAD3DTransformObjects;
end;

{ --------------------------------- }

constructor TCAD3DInternalEditPrimitiveParam.Create(const CADPrg: TCADPrg; Prim: TPrimitive3D; ApertureSize: Word; EditMode: TCAD3DEditPrimMode);
begin
  inherited Create(nil);

  fOriginalPrimitive := Prim;
  try
   fCurrentPrimitive := CADSysFindClassByName(fOriginalPrimitive.ClassName).Create(0) as TPrimitive3D;
  except
   Exit;
  end;
  if not Assigned(fCurrentPrimitive) then
   Raise ECADSysException.Create('TCAD3DEditPrimitive: Only registered classes are allowed');
  fCurrentPrimitive.Assign(fOriginalPrimitive);
  fCurrentPrimitive.ApplyTransform;
  fApertureSize := ApertureSize;
  fCurrentCtrlPt := -1;
  fEditMode := EditMode;
  fOwnerPrg := TCADPrg3D(CADPrg);
  fOldWPNorm := fOwnerPrg.WorkingPlaneNormal;
  fOldWPUp := fOwnerPrg.WorkingPlaneUP;
  fOldWPRef := fOwnerPrg.WorkingPlaneOrigin;
  if (fCurrentPrimitive is TPlanarOutline3D) then
   case fEditMode of
    emNone, emChangeWP: begin // Cambio il piano di lavoro con quello di definizione dell'oggetto planare.
      fOwnerPrg.WorkingPlaneNormal := TPlanarOutline3D(fCurrentPrimitive).PlanarObject.PlaneNormal;
      fOwnerPrg.WorkingPlaneUP := TPlanarOutline3D(fCurrentPrimitive).PlanarObject.PlaneUP;
      fOwnerPrg.WorkingPlaneOrigin := TPlanarOutline3D(fCurrentPrimitive).PlanarObject.PlaneReference;
    end;
    emProjectOnWP: begin // Cambio l'oggetto planare in modo da risiedere sul piano di lavoro.
      TPlanarOutline3D(fCurrentPrimitive).PlanarObject.PlaneNormal := fOldWPNorm;
      TPlanarOutline3D(fCurrentPrimitive).PlanarObject.PlaneUP := fOldWPUp;
      TPlanarOutline3D(fCurrentPrimitive).PlanarObject.PlaneReference := fOldWPRef;
    end;
   end
  else if (fCurrentPrimitive is TPlanarCurve3D) then
   case fEditMode of
    emNone, emChangeWP: begin // Cambio il piano di lavoro con quello di definizione dell'oggetto planare.
      fOwnerPrg.WorkingPlaneNormal := TPlanarCurve3D(fCurrentPrimitive).PlanarObject.PlaneNormal;
      fOwnerPrg.WorkingPlaneUP := TPlanarCurve3D(fCurrentPrimitive).PlanarObject.PlaneUP;
      fOwnerPrg.WorkingPlaneOrigin := TPlanarCurve3D(fCurrentPrimitive).PlanarObject.PlaneReference;
    end;
    emProjectOnWP: begin // Cambio l'oggetto planare in modo da risiedere sul piano di lavoro.
      TPlanarCurve3D(fCurrentPrimitive).PlanarObject.PlaneNormal := fOldWPNorm;
      TPlanarCurve3D(fCurrentPrimitive).PlanarObject.PlaneUP := fOldWPUp;
      TPlanarCurve3D(fCurrentPrimitive).PlanarObject.PlaneReference := fOldWPRef;
    end;
   end;
  fCurrentPrimitive.UpdateExtension(Self);
end;

destructor TCAD3DInternalEditPrimitiveParam.Destroy;
begin
  with fOwnerPrg do
   begin
     WorkingPlaneNormal := fOldWPNorm;
     WorkingPlaneUP := fOldWPUp;
     WorkingPlaneOrigin := fOldWPRef;
   end;
  fCurrentPrimitive.Free;
  inherited;
end;

procedure TCAD3DInternalEditPrimitiveParam.SetCtrlPoint(Viewport: TCADViewport3D; Pt: TPoint3D);
var
  TmpDist: TRealType;
  TmpAp: TRealType;
  TmpPt, TmpCPt: TPoint3D;
begin
  if not fOriginalPrimitive.Enabled then
   Exit;
  with Viewport do
   begin
     TmpAp := GetAperture(fApertureSize);
     Pt := TransformPoint3D(Pt, ViewNormalization);
     TmpDist := 0;
     fCurrentCtrlPt := fCurrentPrimitive.OnMe(Pt, ViewNormalization, TmpAp, TmpDist);
     if fCurrentCtrlPt >= 0 then
      begin
        TmpCPt := ObjectToWorld(fCurrentPrimitive, fCurrentPrimitive.Points[fCurrentCtrlPt]);
        TmpPt := fOwnerPrg.WorldToWorkingPlane(TmpCPt);
        // TmpPt è la proiezione del punto di controllo sul viewport.
        fCurrentCtrlPtDist := PointDistance3D(TmpPt, TmpCPt);
        fCurrentCtrlPtExDir := Direction3D(TmpPt, TmpCPt);
      end;
   end;
end;

procedure TCAD3DInternalEditPrimitiveParam.UnSetCtrlPoint;
begin
  fCurrentCtrlPt := -1;
end;

procedure TCAD3DInternalEditPrimitiveParam.AcceptEdited;
begin
  if (fEditMode = emNone) then
   begin
     if (fCurrentPrimitive is TPlanarOutline3D) then
      TPlanarOutline3D(fCurrentPrimitive).PlanarObject.Assign(TPlanarOutline3D(fOriginalPrimitive).PlanarObject)
     else if (fCurrentPrimitive is TPlanarCurve3D) then
      TPlanarCurve3D(fCurrentPrimitive).PlanarObject.Assign(TPlanarCurve3D(fOriginalPrimitive).PlanarObject);
   end;
  fOriginalPrimitive.Assign(fCurrentPrimitive);
  fOriginalPrimitive.UpdateExtension(Self);
end;

procedure TCAD3DInternalEditPrimitiveParam.AddCtrlPoint(Viewport: TCADViewport3D; Pt: TPoint3D);
var
  TmpCPt: TPoint3D;
begin
  if fCurrentCtrlPt > -1 then
   begin
     DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
     TmpCPt := Viewport.WorldToObject(fCurrentPrimitive, Pt);
     fCurrentPrimitive.Points.Insert(fCurrentCtrlPt, TmpCPt);
     DrawModifiedPrim(Viewport);
   end;
end;

procedure TCAD3DInternalEditPrimitiveParam.MoveCtrlPoint(Viewport: TCADViewport3D; Pt: TPoint3D; CADPrg: TCADPrg3D);
begin
  if (fCurrentCtrlPt > -1) and (fOriginalPrimitive.Enabled) then
   begin
     DrawModifiedPrim(Viewport);
     // Porto il punto da coordinate mondo a coordinate oggetto
     // perche' i punti di controllo sono in quest'ultimo sistema.
     case fEditMode of
      emNone, emChangeWP: Pt := ExtrudePoint3D(Pt, fCurrentCtrlPtExDir, fCurrentCtrlPtDist);
     end;
     Pt := fCurrentPrimitive.WorldToObject(Pt);
     fCurrentPrimitive.Points[fCurrentCtrlPt] := Pt;
     DrawModifiedPrim(Viewport);
   end;
end;

procedure TCAD3DInternalEditPrimitiveParam.DrawModifiedPrim(Viewport: TCADViewport3D);
begin
  with Viewport do
   DrawObject3DWithRubber(fCurrentPrimitive, True);
end;

procedure TCAD3DInternalEditPrimitiveParam.DrawOSD(Viewport: TCADViewport3D; Pt: TPoint3D; FirstTime: Boolean);
var
  ScrPt: TPoint;
begin
  with Viewport do
   begin
     OnScreenCanvas.Canvas.Pen.Assign(RubberPen);
     if not FirstTime then
      begin
        ScrPt := Point2DToPoint(ViewportToScreen(WorldToViewport(fLastPt)));
        OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                        Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                        Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                        Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                        Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
      end;
     fLastPt := Pt;
     ScrPt := Point2DToPoint(ViewportToScreen(WorldToViewport(fLastPt)));
     OnScreenCanvas.Canvas.Polyline([Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y - fApertureSize),
                                     Point(ScrPt.X + fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y + fApertureSize),
                                     Point(ScrPt.X - fApertureSize, ScrPt.Y - fApertureSize)]);
   end;
end;

constructor TCAD3DEditPrimitive.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DInternalEditPrimitiveParam;
begin
  inherited;
  if not (StateParam is TCAD3DEditPrimitiveParam) then
   Raise ECADSysException.Create('TCAD3DEditPrimitive: Invalid param');
  Description := 'Select a Control point of the primitive';
  try
   NewParam := TCAD3DInternalEditPrimitiveParam.Create(CADPrg, TCAD3DEditPrimitiveParam(StateParam).Primitive3D, 5, TCAD3DEditPrimitiveParam(StateParam).fEditMode);
  except
   NextState := CADPrg.DefaultState;
   Exit;
  end;
  with TCADPrg3D(CADPrg) do
   begin
     Viewport3D.Refresh;
     NewParam.DrawOSD(Viewport3D, Point3D(0, 0, 0), True);
     NewParam.DrawModifiedPrim(Viewport3D);
   end;
  Param := NewParam;
  TCADPrg3D(CADPrg).SnapOriginPoint := Point3D(MaxCoord, MaxCoord, MaxCoord);
end;

function TCAD3DEditPrimitive.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                        var NextState: TCADStateClass): Boolean;
var
  CurrPoint3D: TPoint3D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, Param as TCAD3DInternalEditPrimitiveParam do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         AcceptEdited;
         IgnoreEvents := True;
         try
          Viewport3D.CADCmp3D.RepaintViewports;
         finally
          IgnoreEvents := False;
         end;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end;
    ceMouseDblClick: if (MouseButton = cmbLeft) and (fCurrentPrimitive.Points.GrowingEnabled) then begin
      SetCtrlPoint(Viewport3D, CurrentWorldPoint);
      if CurrentCtrlPt >= 0 then
       begin
         CurrPoint3D := CurrentWorkingPlaneSnappedPoint;
         AddCtrlPoint(Viewport3D, CurrPoint3D);
       end;
    end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      SetCtrlPoint(Viewport3D, CurrentWorldPoint);
      if CurrentCtrlPt >= 0 then
       Description := 'Move the control point and release the mouse.';
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      UnSetCtrlPoint;
      Description := 'Select a Control point of the primitive';
    end;
    ceMouseMove: begin
      CurrPoint3D := CurrentWorkingPlaneSnappedPoint;
      DrawOSD(Viewport3D, CurrentWorldPoint, False);
      MoveCtrlPoint(Viewport3D, CurrPoint3D, TCADPrg3D(CADPrg));
      DrawOSD(Viewport3D, CurrentWorldPoint, False);
    end;
    cePaint: begin
      DrawOSD(Viewport3D, CurrentWorldPoint, True);
      DrawModifiedPrim(Viewport3D);
    end;
   end;
end;

procedure TCAD3DEditPrimitive.OnStop;
begin
  Param.Free;
  Param := nil;
end;

constructor TCAD3DEditPrimitiveParam.Create(Prim: TPrimitive3D; EditMode: TCAD3DEditPrimMode);
begin
  inherited Create(nil);
  fPrimitive3D := Prim;
  fEditMode := EditMode;
end;

constructor TCAD3DEditSelectedObject.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
var
  NewParam: TCAD3DEditPrimitiveParam;
  TmpObj: TObject3D;
  TmpIter: TGraphicObjIterator;
begin
  inherited;
  if not (StateParam is TCAD3DSelectObjectsParam) then
   Raise ECADSysException.Create('TCAD3DEditSelectedObject: Invalid param');
  with TCAD3DSelectObjectsParam(StateParam) do
   begin
     TmpIter := SelectedObjects.GetIterator;
     try
      TmpObj := TObject3D(TmpIter.First);
     finally
      TmpIter.Free;
     end;
     if TmpObj is TPrimitive3D then
      NewParam := TCAD3DEditPrimitiveParam.Create(TPrimitive3D(TmpObj), TCAD3DEditPrimMode(fUserFlag))
     else
      begin
        Param.Free;
        Param := nil;
        NextState := CADPrg.DefaultState;
        Exit;
      end;
   end;
  StateParam.Free;
  Param := NewParam;
  CADPrg.CurrentOperation := TCAD3DEditSelectedObject;
  NextState := TCAD3DEditPrimitive;
end;


{ ******************* Viewing tasks *********************** }

constructor TCAD3DRotationalDollyParam.Create(const CameraPos, CameraView: TPoint3D);
begin
  inherited Create(nil);

  fInDollying := False;
  fCameraCenter := CameraView;
  fCameraPos := CameraPos;
  fOldCameraCenter := CameraView;
  fOldCameraPos := CameraPos;
  fDollyRadius := PointDistance3D(fCameraPos, fCameraCenter);
end;

constructor TCAD3DRotationalDolly.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD3DRotationalDollyParam) then
   Raise ECADSysException.Create('TCAD3DRotationalDolly: Invalid param');
  Description := 'Old the mouse down for rotate the viewpoint.';
  CanBeSuspended := False;
  with TCAD3DRotationalDollyParam(StateParam) do
   fOldShowCursor := CADPrg.ShowCursorCross;
end;

function TCAD3DRotationalDolly.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                       var NextState: TCADStateClass): Boolean;
var
  NewCameraPt: TPoint3D;
  CurrScrPt: TPoint;
  RotTransfX, RotTransfY: TTransf3D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, TCAD3DRotationalDollyParam(Param) do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         ShowCursorCross := fOldShowCursor;
         Viewport3D.SetCamera(fOldCameraPos, fOldCameraCenter, Viewport3D.VUP);
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         ShowCursorCross := fOldShowCursor;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         RepaintAfterOperation;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      fInDollying := True;
      ShowCursorCross := False;
      fOldMouse := CurrentMousePoint;
      // Gli assi di rotazione sono due: Viewport3D.VUP, CrossProd(Viewport3D.VUP, Viewport3D.VPN)
      fYAxis := Viewport3D.VUP;
      fXAxis := CrossProd3D(Viewport3D.VUP, Viewport3D.VPN);
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      fInDollying := False;
      fCameraPos := Viewport3D.CameraProjectionPlanePosition;
      ShowCursorCross := fOldShowCursor;
      if Viewport.UsePaintingThread then
       Viewport.Repaint
      else
       Viewport.Refresh;
    end;
    ceMouseMove: if fInDollying then begin
      Viewport.StopRepaint;
      CurrScrPt := CurrentMousePoint;
      // Determino l'angolo di rotazione attorno a Y come ArcSin di delta X
      if (Abs((fOldMouse.X - CurrScrPt.X) / Viewport3D.Width) < 1.0) then
        RotTransfY := RotateOnAxis3D(fCameraCenter, fYAxis, ArcSin((fOldMouse.X - CurrScrPt.X) / Viewport3D.Width))
      else
        RotTransfY := IdentityTransf3D;
      // Determino l'angolo di rotazione attorno a X come ArcSin di delta Y
      if (Abs((fOldMouse.Y - CurrScrPt.Y) / Viewport3D.Height) < 1.0) then
        RotTransfX := RotateOnAxis3D(fCameraCenter, fXAxis, ArcSin((fOldMouse.Y - CurrScrPt.Y) / Viewport3D.Height))
      else
        RotTransfX := IdentityTransf3D;
      // Determino l'angolo di complessivo come combinazione delle due rotazioni
      NewCameraPt := TransformPoint3D(fCameraPos, MultiplyTransform3D(RotTransfX, RotTransfY));
      Viewport3D.SetCamera(NewCameraPt, fCameraCenter, Viewport3D.VUP);
    end;
   end;
end;

constructor TCAD3DPositionalDollyParam.Create(const CameraPos, CameraView: TPoint3D);
begin
  inherited Create(nil);

  fInDollying := False;
  fCameraCenter := CameraView;
  fCameraPos := CameraPos;
  fOldCameraCenter := CameraView;
  fOldCameraPos := CameraPos;
  fDollyRadius := PointDistance3D(fCameraPos, fCameraCenter);
end;

constructor TCAD3DPositionalDolly.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD3DPositionalDollyParam) then
   Raise ECADSysException.Create('TCAD3DPositionalDolly: Invalid param');
  Description := 'Old the mouse down for move the viewpoint.';
  CanBeSuspended := False;
  with TCAD3DPositionalDollyParam(StateParam) do
   fOldShowCursor := CADPrg.ShowCursorCross;
end;

function TCAD3DPositionalDolly.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                       var NextState: TCADStateClass): Boolean;
var
  DeltaRay3D: TVector3D;
  CurrScrPt: TPoint;
  NewCameraPt: TPoint3D;
  NewViewPt: TPoint3D;
  MoveTransf: TTransf3D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, TCAD3DPositionalDollyParam(Param) do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         ShowCursorCross := fOldShowCursor;
         Viewport3D.SetCamera(fOldCameraPos, fOldCameraCenter, Viewport3D.VUP);
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         ShowCursorCross := fOldShowCursor;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         RepaintAfterOperation;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      fInDollying := True;
      ShowCursorCross := False;
      fOldMouse := CurrentMousePoint;
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      fInDollying := False;
      ShowCursorCross := fOldShowCursor;
      if Viewport.UsePaintingThread then
       Viewport.Repaint
      else
       Viewport.Refresh;
    end;
    ceMouseMove: if fInDollying then begin
      Viewport.StopRepaint;
      CurrScrPt := CurrentMousePoint;
      DeltaRay3D.X := -(CurrScrPt.X - fOldMouse.X);
      DeltaRay3D.Y := (CurrScrPt.Y - fOldMouse.Y);
      DeltaRay3D.Z := 0;
      MoveTransf := Translate3D(DeltaRay3D.X, DeltaRay3D.Y, 0);
      NewCameraPt := TransformPoint3D(fCameraPos, MoveTransf);
      NewViewPt := TransformPoint3D(fCameraCenter, MoveTransf);
      Viewport3D.SetCamera(NewCameraPt, NewViewPt, Viewport3D.VUP);
    end;
   end;
end;

constructor TCAD3DDeeptDollyParam.Create(const CameraPos, CameraView: TPoint3D);
begin
  inherited Create(nil);

  fInDollying := False;
  fCameraCenter := CameraView;
  fCameraPos := CameraPos;
  fOldCameraCenter := CameraView;
  fOldCameraPos := CameraPos;
  fDollyRadius := PointDistance3D(CameraPos, CameraView);
end;

constructor TCAD3DDeeptDolly.Create(const CADPrg: TCADPrg; const StateParam: TCADPrgParam; var NextState: TCADStateClass);
begin
  inherited Create(CADPrg, StateParam, NextState);
  if not (StateParam is TCAD3DDeeptDollyParam) then
   Raise ECADSysException.Create('TCAD3DDeeptDolly: Invalid param');
  Description := 'Old the mouse down for move the viewpoint distance.';
  CanBeSuspended := False;
  with TCAD3DDeeptDollyParam(StateParam) do
   fOldShowCursor := CADPrg.ShowCursorCross;
end;

function TCAD3DDeeptDolly.OnEvent(Event: TCADPrgEvent; MouseButton: TCS4MouseButton; Shift: TShiftState; Key: Word;
                                  var NextState: TCADStateClass): Boolean;
var
  Delta: TRealType;
  CurrScrPt: TPoint;
  NewCameraPoint: TPoint3D;
  ViewDirection: TVector3D;
begin
  Result := inherited OnEvent(Event, MouseButton, Shift, Key, NextState);
  if not Assigned(Param) then
   Exit;
  with CADPrg as TCADPrg3D, TCAD3DDeeptDollyParam(Param) do
   case Event of
    ceUserDefined:
      if Key = CADPRG_CANCEL then
       begin
         ShowCursorCross := fOldShowCursor;
         Viewport3D.SetCamera(fOldCameraPos, fOldCameraCenter, Viewport3D.VUP);
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         Result := True;
       end
      else if Key = CADPRG_ACCEPT then
       begin
         ShowCursorCross := fOldShowCursor;
         Param.Free;
         Param := nil;
         NextState := DefaultState;
         RepaintAfterOperation;
         Result := True;
       end;
    ceMouseDown: if MouseButton = cmbLeft then begin
      fInDollying := True;
      ShowCursorCross := False;
      fOldMouse := CurrentMousePoint;
    end;
    ceMouseUp: if MouseButton = cmbLeft then begin
      fInDollying := False;
      fCameraPos := Viewport3D.CameraProjectionPlanePosition;
      ShowCursorCross := fOldShowCursor;
      if Viewport.UsePaintingThread then
       Viewport.Repaint
      else
       Viewport.Refresh;
    end;
    ceMouseMove: if fInDollying then begin
      Viewport.StopRepaint;
      CurrScrPt := CurrentMousePoint;
      Delta := (CurrScrPt.Y - fOldMouse.Y);
      NewCameraPoint := ExtrudePoint3D(fCameraPos, Viewport3D.VPN, Delta);
      ViewDirection := Direction3D(NewCameraPoint, fCameraCenter);
      if not IsSameVector3D(ViewDirection, Viewport3D.VPN, -3) and
         (PointDistance3D(NewCameraPoint, fCameraCenter) > fDollyRadius / 5) then
       begin
         Viewport3D.SetCamera(NewCameraPoint, fCameraCenter, Viewport3D.VUP);
       end;
    end;
   end;
end;

end.

