{ This demo shows you how to use all the methods of the TCADCmp2D.
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CADSys4, CS4BaseTypes,
  CS4Shapes, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    CSCAD: TCADCmp2D;
    CSViewport: TCADViewport2D;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CSViewportPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  { The first thing to do with TCADViewport2D (in the following
    text CSViewport) is to set up its defaults in the Object Inspector.
    The important properties to set are:
    - The linked CADCmp2D by setting CADCmp2D
    - if the control must be transparent setting the IsTransparent
      property
    - if you want to use a separate thread for all the repainting
      processes setting the UsePaintingThread.
    Some important properties are however not accessible from the
    object inspector as the starting viewport rectangle (the portion
    of the plane viewed in the control).
    So in the FormCreate you may want to set the starting viewport
    rectangle using the property VisualRect or using the
    ZoomWindow method.
  }
  CSViewport.VisualRect := Rect2D(-5, -5, 50, 50);
  { Another important property is the default color for the
    layers. By default it is black but in our case if we don't
    change it we will not see the shapes because the background
    is itself black.
  }
  CSCAD.DefaultLayersColor := clWhite;
  { For the other properties see the online help. }
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  TmpShape: TObject2D;
begin
  { Normally you use the CADCmp component to add the
    shapes to be viewed. You can add any shapes by using the
    AddObject method. Note that to add an object and give it a
    free ID you can pass -1 as the object ID.
  }
  CSCAD.AddObject(1, TEllipse2D.Create(1, Point2D(0, 0),
                                          Point2D(20, 20)));
  { When you add an object you have to repaint the CSViewport
    in order to see it on the screen. You can use the Repaint
    method of CSViewport or RepaintViewports of CSCad
  }
  CSCAD.RepaintViewports;
  { You can also set the DrawOnAdd property of CSCAD to automatically
    draw the new object on all the viewports.
  }
  CSCAD.DrawOnAdd := True;
  { Add another object.
  }
  CSCAD.AddObject(10, TEllipse2D.Create(10, Point2D(30, 0),
                                            Point2D(50, 20)));
  CSCAD.DrawOnAdd := False;
  { The object you've added are managed by the CAD and so you
    don't need to delete them at the end of the application,
    and you can repaint them when you want by using the method
    RepaintViewports.
  }
  CSCAD.RepaintViewports;
  { You can also refresh the viewports using the RefreshViewports
    method. By using this method instead of RepaintViewports the
    drawing is not actually redone but instead the off-screen copy
    of it is repainted on the screen. So if you have drawn an
    object manually on the viewport (and not added it to the
    CAD) you can restore the original drawing without redrawing
    it that is a time consuming work.
  }
  CSCAD.RefreshViewports;
  { You can get a shape in the CAD by means of GetObject function.
    This method needs an ID.
  }
  TmpShape := CSCAD.GetObject(10);
  { if the result is NIL then the object doesn't exist.
  }
  if TmpShape <> nil then
   ShowMessage('The object with ID=10 is in the CAD')
  else
   ShowMessage('The object with ID=10 is not in the CAD');
  { A shape can be redrawed using the RedrawObject method.
  }
  CSCAD.RedrawObject(TmpShape);
  { You can remove an object from a CAD without deleting it
    (remember that by default all objects in the CAD are
    under the responsability of the CAD).
    Again its need an object ID. If the object is not in the
    CAD a ECADListObjNotFound exception will be raised.
  }
  CSCAD.RemoveObject(10);
  { Here you can see the difference between RefreshViewports and
    RepaintViewports. Only when the viewport is repainted the
    right circle is deleted from the screen.
  }
  ShowMessage('The viewport will be refreshed');
  CSCAD.RefreshViewports;
  ShowMessage('The viewport will be repainted');
  CSCAD.RepaintViewports;
  ShowMessage('Press a key.');
  { You can re-insert the object in the CAD with a different ID.
  }
  CSCAD.AddObject(11, TmpShape);
  { You can remove the object and delete it using the DeleteObject
    method that behaves like RemoveObject but the shape will be
    also freed. So the TmpShape reference is now a dangling pointer.
    You can also delete all the objects in the CAD using the
    DeleteAllObjects method.
  }
  CSCAD.DeleteObject(11);
  CSCAD.AddObject(10, TEllipse2D.Create(10, Point2D(30, 0),
                                            Point2D(50, 20)));
  CSCAD.RepaintViewports;
  { The objects in the CAD are drawed in the insertion order, so
    an object that was added before another one will be covered
    by the second shape.
    You can however insert an object in a specified position
    using the InsertObject method. The first parameter is the
    ID of the object being inserted, the second parameter is
    the ID of an object in the CAD. The new shape will be inserted
    before the object with the specified ID.
  }
  CSCAD.Layers[0].Brush.Color := clRed; // Ignore this line for now.
  CSCAD.AddObject(11, TFilledEllipse2D.Create(0, Point2D(10, 0), Point2D(40, 20)));
  CSCAD.RepaintViewports;
  ShowMessage('The new object is drawed over the two circles with add');
  CSCAD.DeleteObject(11);
  CSCAD.InsertObject(11, 10, TFilledEllipse2D.Create(0, Point2D(10, 0), Point2D(40, 20)));
  CSCAD.RepaintViewports;
  ShowMessage('The new object is drawed over the left circles and under the right circle with insert');
  { However you can move an object before another one using the
    MoveObject method. The first parameter is the ID of the
    object before which the shape with the ID of the second parameter
    is to be moved.
    If either the reference object for the
    insertion or the object to be moved are not in the list
    an ECADListObjNotFound exception will be raised.
  }
  CSCAD.MoveObject(11, 1);
  CSCAD.RepaintViewports;
  ShowMessage('The new object is drawed under the two circles now');
  { When you have added objects to the CAD you can transform them
    using the TransformObjects method.
    It requires an ID with the ID of the shapes to be transformed
    and the transformation.
    If an ID is not found an ECADListObjNotFound exception will be raised.
    By seeting RepaintAfterTransform to True the viewports will
    be also repainted otherwise a manual repaint will be needed.
  }
  CSCAD.TransformObjects([1, 10, 11], Translate2D(0, 5));
  CSCAD.RepaintViewports;
  { You can get the drawing extension (the bounding box of the
    drawing) by query the DrawingExtensions property.
  }
  with CSCAD.DrawingExtension do
   ShowMessage(Format('(%6.3f, %6.3f)-(%6.3f, %6.3f)', [Left, Bottom, Top, Right]));
  { Delete all the objects now.
  }
  CSCAD.DeleteAllObjects;
  CSCAD.RepaintViewports;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  FS: TFileStream;
begin
  { The CADSys library is able to store drawings in a proprietary
    stream format. Because the persistance is made using the
    TStream object of Delphi, drawing can be stored in a file,
    in memory and event is a Database Blob. This is useful in
    GIS application where you need to store a path as a record
    in a database. You can also add application specific
    information in front or at the end of the stream, incorporating
    the draws in your own stream format.
    Now see all the methods to manage persistance in CADCmp.
  }
  { First of all we can save the drawing in a file by using the
    SaveToStream method with a TFileStream object.
  }
  // Create some shapes.
  CSCAD.AddObject(1, TFilledEllipse2D.Create(1, Point2D(0, 30),
                                                Point2D(20, 50)));
  CSCAD.AddObject(2, TFilledEllipse2D.Create(1, Point2D(30, 30),
                                                Point2D(50, 50)));
  CSCAD.AddObject(3, TEllipse2D.Create(1, Point2D(10, 0),
                                          Point2D(40, 35)));
  // Store them in a file.
  FS := TFileStream.Create('Test.drw', fmOpenWrite or fmCreate);
  try
    CSCAD.SaveToStream(FS);
  finally
    FS.Free;
  end;
  // Clear all shapes.
  CSCAD.DeleteAllObjects;
  { To retrieve a drawing you use the LoadFromStream method.
    This method clear the CAD before loading the new drawing.
    NOTE: The blocks in the CAD not yet saved are not cleared.
  }
  FS := TFileStream.Create('Test.drw', fmOpenRead);
  try
    CSCAD.LoadFromStream(FS);
  finally
    FS.Free;
    CSCAD.RepaintViewports;
  end;
  { All stream drawings are labed with the version of the library
    that made them (it is a six character string in the format
    CADxyz, where x is the major version, y the minor version and
    z the release number. At the moment it is CAD422. Note that
    this format is used from now on.). If the version of the
    library is different from the version of the drawing an
    exception will be raised if the user don't give an
    OnInvalidFileVersion handler. If this handler is given and
    it sets the Resume variable (see the on line help) to True,
    the drawing will be read as no differences in version was
    encountered. }
  { If you want to add a drawing to the current one you can
    use the MergeFromStream method that behaves as LoadFromStream,
    but no clearing is made. }
  { As a shortcut three other method are added: LoadFromFile, SaveToFile,
    MergeFromFile that accept the name of the file instead of a stream.
  }
  { There are also four other methods used for persistence of
    the shape blocks that will be described below.
  }
  { To work with persistance, there are two events that are usefull
    when you want to give a visual cue of the progress of the
    loading or saving. When a drawing is large, loading and saving
    it from a file or a database may be a slow process. In these
    cases it is useful to tell the user that the process is not
    blocked but it is slow working. To do that you can use two
    event handlers: OnLoadProgess and OnSaveProgress.
    Note however that these events are fired when a shape is loaded
    and not when a byte is read. So if a shape is a big one (as
    in the case of a shape block) these event get fired few times.
  }
  ShowMessage('End');
  CSCAD.DeleteAllObjects;
  CSCAD.RepaintViewports;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Brush: TBrush;
begin
  { The CADSys library use the concept of layers, a layer is
    a way to groups objects. A layer in CADSys can be
    switched on and off so that the shapes on that layer are not
    drawed, not picked or both. A layer cannot doesn't keep
    any z-order information however, so you cannot get the shapes
    in one layer drawed before the shapes in another layer.
    The most important use of the layers is to give special
    drawing properties to the shapes in that layer. In CADSys the
    color of a line, the patter of a fill or the fact that a
    shape is transparent or not, are given on a per layer basis.
    So the fact that a circle is red depend only on the layer
    on which the circle is drawed (there is a way to create shapes
    that contains the color information, and doesn't use the layer
    color, but you have to create these shapes by inheritance).
    There are 256 layers in CADSys, from Layer 0 to Layer 1.
    Any layer has a Name, a Pen object, a Brush object and other
    properties (see the help on TLayer class). The Layer 0 is the
    default one.
    You can access the layers using the Layers property.
  }
  { The following line of code sets the layer 0 lines color to
    red. }
  CSCAD.Layers[0].Pen.Color := clRed;
  // Now changes some other properties.
  CSCAD.Layers[0].Opaque := False;
  { When a shape is added to the CAD, it is placed of the layer
    given by the CurrentLayer property (by default 0).
  }
  // Add a circle to the zero layer (the default).
  CSCAD.AddObject(1, TFilledEllipse2D.Create(1, Point2D(0, 30),
                                                Point2D(20, 50)));
  // Now add a circle to the layer 1.
  CSCAD.CurrentLayer := 1;
  CSCAD.AddObject(2, TFilledEllipse2D.Create(1, Point2D(30, 30),
                                                Point2D(50, 50)));
  // Revert to layer 0.
  CSCAD.CurrentLayer := 0;
  CSCAD.AddObject(3, TEllipse2D.Create(1, Point2D(10, 0),
                                          Point2D(40, 35)));
  CSCAD.RepaintViewports;
  ShowMessage('Change layer');
  { If you want to change the layer to an object use the
    ChangeObjectLayer or set the Layer property of the shape. }
  CSCAD.ChangeObjectLayer(1, 1);
  CSCAD.RepaintViewports;
  ShowMessage('Set the defaults');
  { You can set the pen color of all the layers using the
    DefaultLayersColor property. You can also specify the
    default pen and brush of the layers using the
    SetDefaultBrush and SetDefaultPen methods.
    NOTE: These methods work only on the not modified layers.
    In our case on the first layer and the followings.
    NOTE: If you set a default, it will not be saved to a
    drawing stream, and so you must reset it once loaded. }
  CSCAD.DefaultLayersColor := clGreen;
  Brush := TBrush.Create;
  try
    Brush.Color := clPurple;
    Brush.Style := bsDiagCross;
    CSCAD.SetDefaultBrush(Brush);
  finally
    Brush.Free;
  end;
  CSCAD.RepaintViewports;
  ShowMessage('Set the decorative pen');
  { The CADSys library has now the ability to set a really
    user defined line pattern that has not the limits of
    Windows. To use it you have to set the DecorativePen
    pen property as in the following example. The new line
    pattern is used for all the widths of the pen.
    The style is given as a string where a 1 means a filled pixel
    and a 0 means a cleared pixel.
    Note that using a decorative pen slow down the repaint
    process.
    Note: It is better to start and end the pattern with 1s.
    Note: The pattern doesn't work with filled shapes
    Note: The patterned line is always transparent in the not
    set pixels.
  }
  CSCAD.Layers[0].DecorativePen.SetPenStyle('111111110000000011110001111');
  CSCAD.RepaintViewports;
  ShowMessage('End');
  CSCAD.DeleteAllObjects;
  CSCAD.RepaintViewports;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Src: TGraphicObjList;
begin
  { The library keep the objects to be drawed in lists of
    objects. These list derive from TGraphicObjList and
    are lists of indexed objects, where the index is the
    ID of the shape. A CADCmp contains two of these lists:
    one for the object to draw, and one for the blocks
    of shapes definition (see next).
    For further details on TGraphicObjList see the help,
    for the list is usefull by its own.
    The lists of a CADCmp are not accessible (they are
    protected) and you can only access them through an
    iterator (an iterator is a special class used to
    iterate the objects in a list). You can have multiple
    iterators active on a list. There are two kinds of
    avaiable iterators:
    - a read only iterator (TGraphicObjIterator), that is
      used to access the list but not to modify it
    - a read/write iterator (TExclusiveGraphicObjIterator),
      that is used to do all the function of a read only
      iterator but can also delete objects from the list.
      When there are read only iterators no read/write
      iterator is possible. Only one read/write iterator
      is possible at a time.
    To add objects you have to use the adding methods of
    CADCmp.
    NOTE: When there is at least an active iterator (that is
    an iterator created and not deleted) on a list, you cannot
    modify it. So free all the iterator when you have finished
    to use them, otherwise you will get a lot of ECADListBlocked
    exceptions.
  }
  // Now add some objects to the CAD.
  CSCAD.AddObject(1, TFilledEllipse2D.Create(1, Point2D(0, 30),
                                                Point2D(20, 50)));
  CSCAD.AddObject(2, TFilledEllipse2D.Create(1, Point2D(30, 30),
                                                Point2D(50, 50)));
  CSCAD.AddObject(3, TEllipse2D.Create(1, Point2D(10, 0),
                                          Point2D(40, 35)));
  CSCAD.RepaintViewports;
  { To get a read only iterator on the list of shapes of a
    CADCmp use the ObjectsIterator property. Whenever you
    call this property a new iterator is created and must
    be deleted by your code (use it in a try finally block).
  }
  with CSCAD.ObjectsIterator do
   try
  { All the objects in the list are of type TGraphicObject and
    you have to type-cast them. }
     // Show the type of all the objects in the list.
  { When an iterator is created it is positioned at the beginning
    of the list. You can use the Next, Prev, First, Last to
    move the position in the list. Use Current to access the
    current object in the iterator. Different iterators keep
    different position in the list.
  }
     repeat
       ShowMessage(Current.ClassName);
     until Next = nil;
     First;
  { You can also look for a particular object in the list using
    the Search method.
  }
     if Assigned(Search(3)) then
      ShowMessage('Search: ' + Current.ClassName)
     else
      ShowMessage('Object not found');
  { You can do the same using the array property. }
     ShowMessage('Items[1]: ' + Items[1].ClassName);
  { You can also access to the source list on which the
    iterator iterate using the SourceList property.
    When the iterator will be freed you can still use that
    list to get other iterators or to modify the list.
    In this case Src will be the same as the list of CADCmp.
  }
     Src := SourceList;
   finally
     Free;
   end;
  { If you want to iterate in a list and to delete some object
    from the list at the same time use a TExclusiveGraphicObjIterator
    iterator.
    Note: When there is an ExclusiveIterator on a list, the list
    is blocked (and so no repaints are possible for instance).
  }
  with CSCAD.ObjectsExclusiveIterator do
   try
     if Assigned(Search(3)) then
      DeleteCurrent;
   finally
     Free;
   end;
  // Do the repaint when the exclusive iterator is freed.
  CSCAD.RepaintViewports;
  { If you want to know if there are active iterators (read only
    or read/write) use the HasIterators property. When you want
    to know if there is an exclusive iterator use the IsBlocked
    property.
    To know how many objects are in the cad use the ObjectsCount
    property of CADCmp.
  }
  // End
  ShowMessage('End');
  CSCAD.DeleteAllObjects;
  CSCAD.RepaintViewports;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  SBlk: TSourceBlock2D;
  Src: TGraphicObjList;
  Strm: TFileStream;
begin
  { The library has the notion of block of shapes. A block
    is a collection of shapes that can be drawed in different
    position as a whole. An instance of a block is a reference
    to that block that is drawed. You can have instances of
    a block that all share the same shapes reducing the
    memory of the drawing. A set of blocks is called a library
    that can be saved and reused in different drawings.
    The library can be saved in the drawing or in a separate
    library file.
    A block is an instance of TSourceBlock2D, a reference to
    a block is an instance of TBlock2D.
    There is also a container of shapes that cannot be
    instantiated but can be used as an aggregate.
  }
  { To create a block you can create an instance of TSourceBlock2D
    and add to it shapes.
    A block has a name and other usefull properties, again see
    the Help for details.
  }
  // This will create an empty block.
  SBlk := TSourceBlock2D.Create(1, 'ABlock', [nil]);
  // Now add objects to it.
  SBlk.Objects.Add(TFilledEllipse2D.Create(1, Point2D(0, 30),
                                              Point2D(20, 50)));
  SBlk.Objects.Add(TFilledEllipse2D.Create(2, Point2D(30, 30),
                                              Point2D(50, 50)));
  SBlk.Objects.Add(TEllipse2D.Create(3, Point2D(10, 0),
                                        Point2D(40, 35)));
  // And finally add it to the list of source blocks.
  CSCAD.AddSourceBlock(SBlk);
  { To view a block you have to create an instance of it and add it
    to a CADCmp.
  }
  CSCAD.AddObject(1, TBlock2D.Create(1, SBlk));
  // Add another instance and translate it.
  CSCAD.AddObject(2, TBlock2D.Create(1, SBlk)).Transform(Translate2D(30, 30));
  CSViewport.ZoomToExtension;
  ShowMessage('Some blocks.');
  { Use this method to delete all source blocks. Remeber that
    a source block cannot be deleted if it has some instance.
    Use the NumOfReferences property to found how many instances
    it has.
    To delete a specific block use either DeleteSourceBlock or
    DeleteSourceBlockByID.
  }
  // Delete the instances first.
  CSCAD.DeleteAllObjects;
  CSCAD.DeleteAllSourceBlocks;
  CSViewport.ZoomWindow(Rect2D(-5, -5, 50, 50));
  { Another way to create a source block is to aggragate existing
    shapes using the BlockObjects method. This is useful when
    you have a list of object and want to transform it into a
    source block.
  }
  // Create a list of objects.
  Src := TGraphicObjList.Create;
  try
    // Remeber to set FreeOnClear to False.
    Src.FreeOnClear := False;
    Src.Add(TFilledEllipse2D.Create(1, Point2D(0, 30),
                                       Point2D(20, 50)));
    Src.Add(TFilledEllipse2D.Create(2, Point2D(30, 30),
                                       Point2D(50, 50)));
    Src.Add(TEllipse2D.Create(3, Point2D(10, 0),
                                 Point2D(40, 35)));

    // Create the source block.
    CSCAD.BlockObjects('ABlock', Src.GetIterator);
    // Remove the iterators.
    Src.RemoveAllIterators;
  finally
    // Destroy the list because we don't need it.
    Src.Free;
  end;
  // Add a block.
  CSCAD.AddBlock(1, 'ABlock');
  CSCAD.RepaintViewports;
  { You can also look for a source block using the
    FindSourceBlock method.
  }
  { When you have a set of source block you can save them to
    a library file. When a source block is saved to a library
    file it will not be saved with the drawing and so you have
    to load it before the drawing that need it.
    When you create a library only the source blocks marked
    as library block will be saved to it. The other source
    block will be saved with the drawing.
  }
  // Set the IsLibraryBlock property.
  CSCAD.FindSourceBlock('ABlock').IsLibraryBlock := True;
  // To create a library use the SaveLibrary method.
  Strm := TFileStream.Create('library.lib', fmCreate or fmOpenWrite);
  try
    CSCAD.SaveLibrary(Strm);
  finally
    Strm.Free;
  end;
  { When you have create a library, you can selectively delete
    all the source blocks that belong to the library (or to
    all the libraries) using the DeleteLibrarySourceBlocks
    method.
    When you want to delete all the source blocks that were
    saved and are not library source blocks use the
    DeleteLibrarySourceBlocks method.
  }
  CSCAD.DeleteObject(1);
  CSCAD.DeleteLibrarySourceBlocks;
  // To load a library use the LoadLibrary method.
  Strm := TFileStream.Create('library.lib', fmOpenRead);
  try
    CSCAD.LoadLibrary(Strm);
  finally
    Strm.Free;
  end;
  { You can also get the iterators for the source block list
    through the SourceBlocksIterator and SourceBlocksExclusiveIterator
    properties.
    Use the SourceBlocksCount property to get the number of
    source blocks defined.
  }
  ShowMessage('End');
  CSCAD.DeleteAllObjects;
  CSCAD.DeleteSourceBlock('ABlock');
  CSCAD.RepaintViewports;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  TmpBmp: TBitmap;
begin
  CSCAD.Layers[0].Pen.Color := clRed;
  CSCAD.Layers[0].Pen.Width := 2;
  CSCAD.Layers[0].Brush.Color := clGray;
  CSViewport.ShowControlPoints := True;
  CSViewport.ControlPointsWidth := 6;
  CSViewport.ControlPointsColor := clYellow;
  CSViewport.GridOnTop := False;
  { This section will show all the shapes that comes with
    the library.
  }
  { The basic shape is a line. }
  CSCAD.AddObject(0, TLine2D.Create(1, Point2D(5, 5), Point2D(30, 30)));
  CSCAD.RepaintViewports;
  ShowMessage('TLine2D');
  CSCAD.DeleteAllObjects;
  { Now an empty rectangle. }
  CSCAD.AddObject(0, TFrame2D.Create(1, Point2D(5, 5), Point2D(30, 30)));
  CSCAD.RepaintViewports;
  ShowMessage('TFrame2D');
  CSCAD.DeleteAllObjects;
  { Now a filled rectangle. }
  CSCAD.AddObject(0, TRectangle2D.Create(1, Point2D(5, 5), Point2D(30, 30)));
  CSCAD.RepaintViewports;
  ShowMessage('TRectangle2D');
  CSCAD.DeleteAllObjects;
  { A polyline. }
  CSCAD.AddObject(0, TPolyline2D.Create(1, [Point2D(5, 5), Point2D(20, 30), Point2D(30, 15)]));
  CSCAD.RepaintViewports;
  ShowMessage('TPolyline2D');
  CSCAD.DeleteAllObjects;
  { A polygon. }
  CSCAD.AddObject(0, TPolygon2D.Create(1, [Point2D(5, 5), Point2D(20, 30), Point2D(30, 15)]));
  CSCAD.RepaintViewports;
  ShowMessage('TPolygon2D');
  CSCAD.DeleteAllObjects;
  { An arc. }
  CSCAD.AddObject(0, TArc2D.Create(1, Point2D(5, 5), Point2D(30, 30), DegToRad(20), DegToRad(160)));
  CSCAD.RepaintViewports;
  ShowMessage('TArc2D');
  CSCAD.DeleteAllObjects;
  { An ellipse. }
  CSCAD.AddObject(0, TEllipse2D.Create(1, Point2D(5, 5), Point2D(30, 30)));
  CSCAD.RepaintViewports;
  ShowMessage('TEllipse2D');
  CSCAD.DeleteAllObjects;
  { A filled ellipse. }
  CSCAD.AddObject(0, TFilledEllipse2D.Create(1, Point2D(5, 5), Point2D(30, 30)));
  CSCAD.RepaintViewports;
  ShowMessage('TFilledEllipse2D');
  CSCAD.DeleteAllObjects;
  { A BSpline. }
  CSCAD.AddObject(0, TBSpline2D.Create(1, [Point2D(5, 5), Point2D(20, 30), Point2D(30, 15), Point2D(40, 20)]));
  CSCAD.RepaintViewports;
  ShowMessage('TBSpline2D');
  CSCAD.DeleteAllObjects;
  { A TTF text }
  with TText2D(CSCAD.AddObject(0, TText2D.Create(1, Rect2D(5, 5, 40, 30), 10, ' Text2D ! '))) do
   begin
     LogFont.FaceName := 'Arial';
     LogFont.Underline := 1;
     LogFont.Escapement := 3400;
   end;
  CSCAD.RepaintViewports;
  ShowMessage('TText2D');
  CSCAD.DeleteAllObjects;
  { A vectorial text }
  // First register a font.
  CSCAD.Layers[0].Pen.Width := 1;
  CADSysRegisterFontFromFile(0, 'Monotxt.fnt');
  with TJustifiedVectText2D(CSCAD.AddObject(0, TJustifiedVectText2D.Create(1, CADSysFindFontByIndex(0), Rect2D(5, 5, 40, 30), 6, 'JText2D'))) do
   begin
     HorizontalJust := jhCenter;
     VerticalJust := jvCenter;
   end;
  CSCAD.RepaintViewports;
  ShowMessage('TJustifiedVectText2D');
  CSCAD.DeleteAllObjects;
  // Unregister the font.
  CADSysUnregisterFont(0);
  { A Bitmap. }
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.LoadFromFile('Test.bmp');
    CSCAD.AddObject(0, TBitmap2D.Create(1, Point2D(5, 5), Point2D(30, 40), TmpBmp));
  finally
    TmpBmp.Free;
  end;
  CSCAD.RepaintViewports;
  ShowMessage('TBitmap2D');
  CSCAD.DeleteAllObjects;
  { END }
  CSCAD.RepaintViewports;
  CSCAD.Layers[0].Pen.Color := clWhite;
  CSCAD.Layers[0].Brush.Color := clWhite;
end;

procedure TForm1.CSViewportPaint(Sender: TObject);
begin
  Caption := IntToStr(CSViewport.Tag);
end;

end.

