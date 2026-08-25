{: This help file explain all the classes defined for DXF handling
   for the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4DXFModule unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.

   <B=Note>: The DXF module given here is not yet completed
   for the DXF documentation is lack to be completed and
   easy to understant (at least to me). Thanks to
   <Code=Vincenzo Siviero (esiviero@@tin.it)> for his help.
   Thanks also to <Code=Giuseppe Staltieri (sgs@@elios.net)>
   for his invaluable support and beta testing.
}
unit CS4DXFModule;

{$mode delphi}
{$WARN 6058 off}

Interface

uses SysUtils, Classes, Graphics, ComCtrls,
     CADSys4, CS4BaseTypes, CS4Shapes, LCLIntf;

type
  // -----===== Starting Cs4DXFReadWrite.pas =====-----
  TSections = (scHeader, scClasses, scObjects, scThumbinalImage, scTables, scBlocks, scEntities, scUnknow);

  { Used to store readed groups from 0 group to 0 group. }
  TGroupTable = array[0..512] of Variant;

  TDXFRead = class(TObject)
  private
    fStream: TextFile;
    fCurrentSection: TSections;
    fGroupCode: Word;
    fGroupValue: Variant;
    fProgressBar: TProgressBar;

    procedure SetProgressBar(PB: TProgressBar);
  public
    constructor Create(FileName: String);
    destructor Destroy; override;
    procedure Rewind;
    { Return False if EOF. }
    function ConsumeGroup: Boolean;
    procedure NextSection;
    function ReadAnEntry(GroupDel: Word; var Values: TGroupTable): Word;

    property GroupCode: Word read FGroupCode;
    property GroupValue: Variant read FGroupValue;
    property CurrentSection: TSections read FCurrentSection;
    property PositionBar: TProgressBar read fProgressBar write SetProgressBar;
  end;

  TDXFWrite = class(TObject)
  private
    FStream: TextFile;
  public
    constructor Create(FileName: String);
    destructor Destroy; override;

    procedure Reset;
    procedure BeginSection(Sect: TSections);
    procedure EndSection({%H-}Sect: TSections);
    procedure WriteGroup(GroupCode: Word; GroupValue: Variant);
    procedure WriteAnEntry(var Values: TGroupTable);
  end;

  EDXFException = Exception;
  EDXFEndOfFile = EAbort;
  EDXFInvalidDXF = EDXFException;

// -----===== Starting Cs4DXF2DConverter.pas =====-----

  TDXF2DImport = class(TObject)
  private
    fTextFont: TVectFont;
    fDXFRead: TDXFRead;
    fScale: TRealType;
    fHasExtension, fUnableToReadAll, fVerbose: Boolean;
    fSetLayers: Boolean; // Se True i layers letti modificano quelli del CAD in base all'ordine di recupero.
    fCADCmp2D: TCADCmp2D;
    fAngleDir: TArcDirection;
    fExtension: TRect2D;
    fLayerList: TStringList; { Contain the name of the layers and the layer itself. }
    fBlockList: TStringList; { Contain the name of the blocks and the block itself. }

    procedure ReadEntitiesAsContainer(const Container: TContainer2D);
    { Read the DXF blocks. }
    procedure ReadBlocks;
    { Read the DXF entities. }
    procedure ReadEntities;
  protected
    // Leggono le entità
    function ReadLine2D(Entry: TGroupTable): TLine2D;
    function ReadTrace2D(Entry: TGroupTable): TPolyline2D;
    function ReadSolid2D(Entry: TGroupTable): TPolyline2D;
    function ReadArc2D(Entry: TGroupTable): TArc2D;
    function ReadCircle2D(Entry: TGroupTable): TEllipse2D;
    function ReadEllipse2D(Entry: TGroupTable): TCurve2D;
    function ReadPolyline2D(Entry: TGroupTable): TPolyline2D;
    function ReadText2D(Entry: TGroupTable): TJustifiedVectText2D;
    function ReadSourceBlock(Entry: TGroupTable): TSourceBlock2D;
    function ReadBlock(Entry: TGroupTable): TBlock2D;
    function ReadEntity(IgnoreBlock: Boolean): TObject2D;

    function GoToSection(Sect: TSections): Boolean;

    property DXFRead: TDXFRead read FDXFRead;
  public
    constructor Create(const FileName: String; const CAD: TCADCmp2D);
    destructor Destroy; override;

    procedure SetTextFont(F: TVectFont);

    { Read the DXF header. }
    procedure ReadHeader;
    { Read the DXF tables. }
    procedure ReadTables;

    { Read the DXF informations. }
    procedure ReadDXF;
    { Read the DXF informations as a block. }
    function ReadDXFAsSourceBlock(const Name: TSourceBlockName): TSourceBlock2D;
    { Read the DXF informations as a container. }
    function ReadDXFAsContainer: TContainer2D;

    property HasExtension: Boolean read fHasExtension;
    property Extension: TRect2D read FExtension;
    property BlockList: TStringList read FBlockList;
    property LayerList: TStringList read FLayerList;
    property Scale: TRealType read fScale write fScale;
    property SetLayers: Boolean read fSetLayers write fSetLayers;
    property UnableToReadAllTheFile: Boolean read fUnableToReadAll;
    property Verbose: Boolean read fVerbose write fVerbose;
  end;

  TDXF2DExport = class(TObject)
  private
    FDXFWrite: TDXFWrite;
    FCADCmp2D: TCADCmp2D;
  protected
    procedure WriteLine2D(Line: TLine2D);
    procedure WriteFrame2D(Frm: TFrame2D);
    procedure WriteCurve2D(Curve: TCurve2D);
    procedure WriteOutline2D(Poly: TOutline2D);
    procedure WriteJVText2D(Text: TJustifiedVectText2D);
    procedure WriteText2D(Text: TText2D);
    procedure WriteBlock(Block: TBlock2D);
    procedure WriteEntity(Obj: TObject2D);
  public
    constructor Create(const FileName: String; const CAD: TCADCmp2D);
    destructor Destroy; override;

    { Write the DXF headers. }
    procedure WriteHeader;
    { Write the DXF tables. }
    procedure WriteTables;
    { Write the DXF blocks. }
    procedure WriteBlocks;
    { Write the DXF entities. }
    procedure WriteEntities;
    { Write the DXF informations. }
    procedure WriteDXF;
  end;

// -----===== Starting Cs4DXF3DConverter.pas =====-----

  TDXF3DImport = class(TObject)
  private
    fTextFont: TVectFont;
    fScale: TRealType;
    FDXFRead: TDXFRead;
    FCADCmp3D: TCADCmp3D;
    FAngleDir: TArcDirection;
    FExtension: TRect3D;
    fSetLayers: Boolean; // Se True i layers letti modificano quelli del CAD in base all'ordine di recupero.
    fHasExtension, fAllowEmptyBlocks, fUnableToReadAll, fVerbose: Boolean;
    FLayerList: TStringList; { Contain the name of the layers and the layer itself. }
    FBlockList: TStringList; { Contain the name of the blocks and the block itself. }

    procedure ReadEntitiesAsContainer(const Container: TContainer3D);
    { Read the DXF blocks. }
    procedure ReadBlocks;
    { Read the DXF entities. }
    procedure ReadEntities;
  protected
    function ReadLine3D(Entry: TGroupTable): TLine3D;
    function ReadCircle3D(Entry: TGroupTable): TEllipse3D;
    function ReadEllipse3D(Entry: TGroupTable): TPlanarCurve3D;
    function ReadArc3D(Entry: TGroupTable): TArc3D;
    function ReadTrace3D(Entry: TGroupTable): TPolyline3D;
    function ReadText3D(Entry: TGroupTable): TJustifiedVectText3D;
    function ReadPolyline3D(Entry: TGroupTable): TPrimitive3D;
    function ReadPlanarFace3D(Entry: TGroupTable): TPlanarFace3D;
    function ReadSourceBlock(Entry: TGroupTable): TSourceBlock3D;
    function ReadBlock(Entry: TGroupTable): TBlock3D; virtual;
    function ReadEntity(IgnoreBlock: Boolean): TObject3D; virtual;
  public
    constructor Create(const FileName: String; const CAD: TCADCmp3D);
    destructor Destroy; override;

    function GoToSection(Sect: TSections): Boolean;
    procedure SetTextFont(F: TVectFont);

    { Read the DXF headers. }
    procedure ReadHeader;
    { Read the DXF tables. }
    procedure ReadTables;
    { Read the DXF informations. }
    procedure ReadDXF;
    { Read the DXF informations as a block. }
    function ReadDXFAsSourceBlock(const Name: TSourceBlockName): TSourceBlock3D;
    { Read the DXF informations as a container. }
    function ReadDXFAsContainer: TContainer3D;

    property DXFRead: TDXFRead read FDXFRead;
    property Extension: TRect3D read FExtension;
    property HasExtension: Boolean read fHasExtension;
    property BlockList: TStringList read FBlockList;
    property LayerList: TStringList read FLayerList;
    property Scale: TRealType read fScale write fScale;
    property SetLayers: Boolean read fSetLayers write fSetLayers;
    property UnableToReadAllTheFile: Boolean read fUnableToReadAll;
    { If TRUE empty blocks are read anyway. Default is True. }
    property AllowEmptyBlocks: Boolean read fAllowEmptyBlocks write fAllowEmptyBlocks;
    property Verbose: Boolean read fVerbose write fVerbose;
  end;

var
  Colors : array[0..255] of TColor;

  Implementation

uses Math, Dialogs, Forms, Variants;

// function body

// -----===== Starting Cs4DXFReadWrite.pas =====-----

function ColorToIndex(Col: TColor; Active: Boolean): Integer;
begin
  Result := 7;
  case Col of
   clBlack,
   clWhite: Result := 7;
   clRed: Result := 1;
   clYellow: Result := 2;
   clLime: Result := 3;
   clAqua: Result := 4;
   clBlue: Result := 5;
   clFuchsia: Result := 6;
   clGray: Result := 8;
   clLtGray: Result := 9;
  end;
  if not Active then Result := -1 * Result;
end;

procedure TDXFRead.SetProgressBar(PB: TProgressBar);
begin
  fProgressBar := PB;
  if Assigned(fProgressBar) then
   begin
     fProgressBar.Min := 0;
     //fProgressBar.Max := FileSize(fStream);
     //fProgressBar.Position := FilePos(fStream);
   end;
end;

constructor TDXFRead.Create(FileName: String);
begin
  inherited Create;
  fProgressBar := nil;
  AssignFile(FStream, FileName);
  Reset(FStream);
  ConsumeGroup;
  NextSection;
end;

destructor TDXFRead.Destroy;
begin
  CloseFile(FStream);
  inherited Destroy;
end;

procedure TDXFRead.Rewind;
begin
  Reset(FStream);
  if Assigned(fProgressBar) then
   fProgressBar.Position := 0;
  ConsumeGroup;
  NextSection;
end;

function TDXFRead.ConsumeGroup;
var
  TxtLine: String;
  LastSep: Char;
begin
  LastSep := DefaultFormatSettings.DecimalSeparator;
  try
    ReadLn(FStream, FGroupCode);
    ReadLn(FStream, TxtLine);
    if EOF(FStream) then
     begin
       Result := False;
       Exit;
     end;
    case FGroupCode of
      0..9,
      999,
      1000..1009: FGroupValue := Trim(TxtLine);
      10..59,
      140..147,
      210..239,
      1010..1059: begin
        if Pos('.', TxtLine) > 0 then
         DefaultFormatSettings.DecimalSeparator := '.'
        else
         DefaultFormatSettings.DecimalSeparator := ',';
        try
         FGroupValue := StrToFloat(Trim(TxtLine));
        except
         FGroupValue := varEmpty;
        end;
      end;
      60..79,
      170..175,
      1060..1079 : begin
        if Pos('.', TxtLine) > 0 then
         DefaultFormatSettings.DecimalSeparator := '.'
        else
         DefaultFormatSettings.DecimalSeparator := ',';
        try
         FGroupValue := StrToInt(Trim(TxtLine));
        except
         FGroupValue := varEmpty;
        end;
      end;
    else
      FGroupValue := TxtLine;
    end;
    Result := True;
  finally
    DefaultFormatSettings.DecimalSeparator := LastSep;
  end;
//  if Assigned(fProgressBar) then
//    fProgressBar.Position := FilePos(fStream);
end;

procedure TDXFRead.NextSection;
begin
  FCurrentSection := scUnknow;
  while not ((FGroupCode = 0) and (FGroupValue = 'SECTION')) do
   if ConsumeGroup = False then Exit;
  ConsumeGroup;
  if FGroupValue = 'HEADER' then
   FCurrentSection := scHeader
  else if FGroupValue = 'CLASSES' then
   FCurrentSection := scClasses
  else if FGroupValue = 'OBJECTS' then
   FCurrentSection := scObjects
  else if FGroupValue = 'THUMBNAILIMAGE' then
   FCurrentSection := scThumbinalImage
  else if FGroupValue = 'TABLES' then
   FCurrentSection := scTables
  else if FGroupValue = 'BLOCKS' then
   FCurrentSection := scBlocks
  else if FGroupValue = 'ENTITIES' then
   FCurrentSection := scEntities;
  ConsumeGroup;
end;

{ GroupDel = Group delimiter }
function TDXFRead.ReadAnEntry(GroupDel: Word; var Values: TGroupTable): Word;
begin
  Result := 0;
  { Find the start of the entry. }
  while (FGroupCode <> GroupDel) and (FGroupCode <> 0) do
   if ConsumeGroup = False then Exit;
  if (FGroupCode = 0) and (FGroupCode <> GroupDel) then Exit;
  { Read all values. }
  repeat
   if FGroupCode < 256 then
    Values[FGroupCode] := FGroupValue
   else if FGroupCode > 999 then
    { The extended data types are remapped from 256=1000. }
    Values[FGroupCode - 744] := FGroupValue;
   ConsumeGroup;
  until (FGroupCode = GroupDel) or (FGroupCode = 0);
  Result := GroupDel;
end;

{ --================== DXFWriter ==================-- }

constructor TDXFWrite.Create(FileName: String);
begin
  inherited Create;
  AssignFile(FStream, FileName);
  Rewrite(FStream);
end;

destructor TDXFWrite.Destroy;
begin
  CloseFile(FStream);
  inherited Destroy;
end;

procedure TDXFWrite.Reset;
begin
  Rewrite(FStream);
end;

procedure TDXFWrite.WriteGroup(GroupCode: Word; GroupValue: Variant);
var
  TxtLine: String;
  LastSep: Char;
begin
  LastSep := DefaultFormatSettings.DecimalSeparator;
  try
    DefaultFormatSettings.DecimalSeparator := '.';
    WriteLn(FStream, Format('%3d', [GroupCode]));
    case GroupCode of
      0..9,
      999,
      1000..1009: TxtLine := Copy(GroupValue, 1, 255);
      10..59,
      140..147,
      210..239,
      1010..1059: TxtLine := Format('%.6f', [Double(GroupValue)]);
      60..79,
      170..175,
      1060..1079 : TxtLine := Format('%6d', [Integer(GroupValue)]);
    else
      TxtLine := '';
    end;
    WriteLn(FStream, TxtLine);
  finally
    DefaultFormatSettings.DecimalSeparator := LastSep;
  end;
end;

procedure TDXFWrite.BeginSection(Sect: TSections);
begin
  WriteGroup(0, 'SECTION');
  case Sect of
   scHeader: WriteGroup(2, 'HEADER');
   scTables: WriteGroup(2, 'TABLES');
   scBlocks: WriteGroup(2, 'BLOCKS');
   scEntities: WriteGroup(2, 'ENTITIES');
  end;
end;

procedure TDXFWrite.EndSection(Sect: TSections);
begin
  WriteGroup(0, 'ENDSEC');
end;

{ Table Index => DXF file group value
  0-255       => 0-255
  256-512     => 1000-1255 }
procedure TDXFWrite.WriteAnEntry(var Values: TGroupTable);
var
  Cont: Word;
begin
  for Cont := 0 to 255 do
   if VarType(Values[Cont]) > 0 then
    WriteGroup(Cont, Values[Cont]);
  for Cont := 1000 to 1255 do
   if VarType(Values[Cont - 744]) > 0 then
    WriteGroup(Cont, Values[Cont - 744]);
end;

// -----===== Starting Cs4DXF2DConverter.pas =====-----

{ --================ DXF2DImport ==================-- }

procedure TDXF2DImport.SetTextFont(F: TVectFont);
begin
  fTextFont := F;
end;

function TDXF2DImport.ReadLine2D(Entry: TGroupTable): TLine2D;
begin
  Result := TLine2D.Create(0, Point2D(Entry[10], Entry[20]),
                              Point2D(Entry[11], Entry[21]));
end;

function TDXF2DImport.ReadCircle2D(Entry: TGroupTable): TEllipse2D;
begin
  Result := TEllipse2D.Create(0, Point2D(Entry[10] - Entry[40], Entry[20] - Entry[40]),
                                 Point2D(Entry[10] + Entry[40], Entry[20] + Entry[40]));
end;

function TDXF2DImport.ReadEllipse2D(Entry: TGroupTable): TCurve2D;
var
  CenterPt, MajorAx: TPoint2D;
  MinorLen, MajorLen, SA, EA, RotA: TRealType;
begin
  CenterPt := Point2D(Entry[10], Entry[20]);
  MajorAx := Point2D(Entry[11], Entry[21]);
  MinorLen := Entry[40];
  MajorLen := PointDistance2D(CenterPt, MajorAx);
  SA := Entry[41];
  EA := Entry[42];
  RotA := ArcTan2(MajorAx.Y - CenterPt.Y, MajorAx.X - CenterPt.X);

  if (SA = 0.0) and (EA = 2 * Pi) then
   // Ellisse completa
   Result := TEllipse2D.Create(0, CenterPt, CenterPt)
  else
   // Arco di ellisse
   Result := TArc2D.Create(0, CenterPt, CenterPt, SA, EA);
  with Result do
   begin
     Points[0] := Point2D(CenterPt.X - MajorLen, CenterPt.X - MinorLen);
     Points[1] := Point2D(CenterPt.X + MajorLen, CenterPt.X + MinorLen);
     if RotA <> 0 then
      ModelTransform := Rotate2D(RotA);
   end;
end;

function TDXF2DImport.ReadArc2D(Entry: TGroupTable): TArc2D;
var
  SA, EA: TRealType;
begin
  SA := DegToRad(Entry[50]);
  EA := DegToRad(Entry[51]);
  Result := TArc2D.Create(0, Point2D(Entry[10] - Entry[40], Entry[20] - Entry[40]),
                             Point2D(Entry[10] + Entry[40], Entry[20] + Entry[40]),
                             SA, EA);
  Result.Direction := FAngleDir;
end;

function TDXF2DImport.ReadTrace2D(Entry: TGroupTable): TPolyline2D;
begin
  Result := TPolyline2D.Create(0, [Point2D(Entry[10], Entry[20]),
                                   Point2D(Entry[11], Entry[21]),
                                   Point2D(Entry[12], Entry[22]),
                                   Point2D(Entry[13], Entry[23])]);
end;

function TDXF2DImport.ReadSolid2D(Entry: TGroupTable): TPolyline2D;
begin
  if VarType(Entry[13]) = varEmpty then
   Result := TPolyline2D.Create(0, [Point2D(Entry[10], Entry[20]),
                                    Point2D(Entry[11], Entry[21]),
                                    Point2D(Entry[12], Entry[22]),
                                    Point2D(Entry[10], Entry[20])])
  else
   Result := TPolyline2D.Create(0, [Point2D(Entry[10], Entry[20]),
                                    Point2D(Entry[11], Entry[21]),
                                    Point2D(Entry[12], Entry[22]),
                                    Point2D(Entry[13], Entry[23]),
                                    Point2D(Entry[10], Entry[20])]);
end;

function TDXF2DImport.ReadText2D(Entry: TGroupTable): TJustifiedVectText2D;
var
  TmpRect: TRect2D;
begin
  Result := nil;
  if fTextFont = nil then
   Exit;
  TmpRect.FirstEdge := Point2D(Entry[10], Entry[20]);
  TmpRect.SecondEdge := Point2D(Entry[10], Entry[20] + Entry[40]);
  Result := TJustifiedVectText2D.Create(0, fTextFont, TmpRect, Entry[40], Entry[1]);
  if Entry[1] = 'F' then
  begin
   Result.DrawBox := True;
  end;
  if VarType(Entry[72]) <> 0 then
   begin
     case Entry[72] of
      1: Result.HorizontalJust := jhCenter;
      2: Result.HorizontalJust := jhRight;
     end;
     case Entry[73] of
      1: Result.VerticalJust := jvBottom;
      2: Result.VerticalJust := jvCenter;
     end;
     if (Entry[72] > 0) or (Entry[73] > 0) then
      Result.Points[1] := Point2D(Entry[11], Entry[21]);
   end;
  if VarType(Entry[50]) <> 0 then
   begin
     Result.Transform(Translate2D(-Result.Points[1].X, -Result.Points[1].Y));
     Result.Transform(Rotate2D(DegToRad(Entry[50])));
     Result.Transform(Translate2D(Result.Points[1].X, Result.Points[1].Y));
   end;
  Result.DrawBox := False;
end;

function TDXF2DImport.ReadPolyline2D(Entry: TGroupTable): TPolyline2D;
var
  LocalEntry: TGroupTable;
  IsClosedInM, IsSpline2D: Boolean;
begin
  // Considera le polilinee 2D e 3D alla stessa maniera.
  // Determina i flags della polilinea.
  if VarType(Entry[70]) <> varEmpty then
   begin
     IsClosedInM := Entry[70] and 1 = 1;
     IsSpline2D := (Entry[70] and 2 = 2) or (Entry[70] and 4 = 4);
   end
  else
   begin
     IsClosedInM := False;
     IsSpline2D := False;
   end;

  if IsSpline2D then
   begin // Leggo la spline2D
     Result := TPolyline2D.Create(0, [Point2D(0, 0)]);
     Result.Points.Delete(0);
     // Leggo soli punti aggiunti, quindi gruppo 70 con bit 8.
     fDXFRead.ReadAnEntry(0, {%H-}LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      begin
        if (VarType(LocalEntry[70]) <> varEmpty)
           and (LocalEntry[70] and 8 = 8) then
         Result.Points.Add(Point2D(LocalEntry[10], LocalEntry[20]));
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
     if Result.Points.Count = 0 then
      begin // La spline è stata creata senza i punti aggiuntivi.
        Result.Free;
        Result := nil;
        fUnableToReadAll := True;
        if fVerbose then
         ShowMessage('Spline without spline-fitting isn''t supported.');
      end;
   end
  else
   begin // Leggo la polilinea2D
     Result := TPolyline2D.Create(0, [Point2D(0, 0)]);
     Result.Points.Delete(0);
     // Leggo tutti i punti.
     fDXFRead.ReadAnEntry(0, LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      begin
        Result.Points.Add(Point2D(LocalEntry[10], LocalEntry[20]));
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
     if IsClosedInM then // la chiudo.
      Result.Points.Add(Result.Points[0]);
   end;
  if (LocalEntry[0] <> 'SEQEND') then
   begin
     fUnableToReadAll := True;
     Raise EDXFInvalidDXF.Create('Invalid DXF file.');
   end;
end;

function TDXF2DImport.ReadEntity(IgnoreBlock: Boolean): TObject2D;
var
  Entry: TGroupTable;
  NLayer: Integer;
begin
  Result := nil;
  FDXFRead.ReadAnEntry(0, {%H-}Entry);
  NLayer := FLayerList.IndexOf(Entry[8]);
  if NLayer > -1 then
   FCADCmp2D.CurrentLayer := NLayer;
  if Entry[0] = 'LINE' then
   Result := ReadLine2D(Entry)
  else if Entry[0] = 'ARC' then
   Result := ReadArc2D(Entry)
  else if Entry[0] = 'TRACE' then
   Result := ReadTrace2D(Entry)
  else if Entry[0] = 'SOLID' then
   Result := ReadSolid2D(Entry)
  else if Entry[0] = 'CIRCLE' then
   Result := ReadCircle2D(Entry)
  else if Entry[0] = 'ELLIPSE' then
   Result := ReadEllipse2D(Entry)
  else if Entry[0] = 'POLYLINE' then
   Result := ReadPolyline2D(Entry)
  else if Entry[0] = 'TEXT' then
   Result := ReadText2D(Entry)
  else if (not IgnoreBlock) and (Entry[0] = 'INSERT') then
   Result := ReadBlock(Entry);
end;

function TDXF2DImport.ReadBlock(Entry: TGroupTable): TBlock2D;
var
  TmpSource: TSourceBlock2D;
  TmpTransf, ScaleTransf, RotTransf: TTransf2D;
begin
  Result := nil;
  try
    TmpSource := FCADCmp2D.FindSourceBlock(StringToBlockName(Entry[2]));
  except
   fUnableToReadAll := True;
   if fVerbose then
    ShowMessage('Some blocks cannot be read.');
   Exit;
  end;
  if TmpSource <> nil then
   begin
     Result := TBlock2D.Create(0, TmpSource);
     // Rotation
     if VarType(Entry[50]) <> varEmpty then
      RotTransf := Rotate2D(DegToRad(Entry[50]))
     else
      RotTransf := IdentityTransf2D;
     // Scale.
     ScaleTransf := IdentityTransf2D;
     if VarType(Entry[41]) <> varEmpty then
      ScaleTransf[1, 1] := Entry[41];
     if VarType(Entry[42]) <> varEmpty then
      ScaleTransf[2, 2] := Entry[42];
     TmpTransf := MultiplyTransform2D(ScaleTransf, RotTransf);
     TmpTransf := MultiplyTransform2D(TmpTransf, Translate2D(Entry[10], Entry[20]));
     Result.ModelTransform := TmpTransf;
     Result.ApplyTransform;
     Result.UpdateExtension(Self);
   end;
end;

procedure TDXF2DImport.ReadEntitiesAsContainer(const Container: TContainer2D);
var
 Tmp: TObject2D;
 ID: Integer;
begin
  ID := 0;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     Tmp := ReadEntity(True);
     if Assigned(Tmp) then
      begin
        Tmp.Transform(Scale2D(fScale, fScale));
        Tmp.ApplyTransform;
        Tmp.ID := ID;
        Container.Objects.Add(Tmp);
        Inc(ID)
      end
     else
      fUnableToReadAll := True;
   end;
  Container.UpdateExtension(Self);
end;

constructor TDXF2DImport.Create(const FileName: String; const CAD: TCADCmp2D);
begin
  inherited Create;
  fDXFRead := TDXFRead.Create(FileName);
  fCADCmp2D := CAD;
  fLayerList := TStringList.Create;
  fBlockList := TStringList.Create;
  fSetLayers := True;
  fScale := 1.0;
  fHasExtension := False;
  fUnableToReadAll := False;
end;

destructor TDXF2DImport.Destroy;
begin
  fDXFRead.Free;
  fLayerList.Free;
  fBlockList.Free;
  inherited Destroy;
end;

function TDXF2DImport.GoToSection(Sect: TSections): Boolean;
begin
  FDXFRead.Rewind;
  while (FDXFRead.CurrentSection <> scUnknow) and
        (FDXFRead.CurrentSection <> Sect) do
   FDXFRead.NextSection;
  Result := FDXFRead.CurrentSection = Sect;
end;

procedure TDXF2DImport.ReadDXF;
begin
  if fCADCmp2D = nil then Exit;
  fUnableToReadAll := False;
  FDXFRead.Rewind;
  while (FDXFRead.CurrentSection <> scUnknow) do
   begin
     case FDXFRead.CurrentSection of
      scHeader: ReadHeader;
      scTables: ReadTables;
      scBlocks: ReadBlocks;
      scEntities: ReadEntities;
     end;
     FDXFRead.NextSection;
   end;
end;

function TDXF2DImport.ReadDXFAsSourceBlock(const Name: TSourceBlockName): TSourceBlock2D;
begin
  fUnableToReadAll := False;
  ReadHeader;
  ReadBlocks;
  Result := TSourceBlock2D.Create(0, Name, [nil]);
  try
   FDXFRead.Rewind;
   while (FDXFRead.CurrentSection <> scUnknow) do
    begin
      if FDXFRead.CurrentSection = scEntities then
       ReadEntitiesAsContainer(Result);
      FDXFRead.NextSection;
    end;
   Result.UpdateExtension(Self);
  except
   on Exception do
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

function TDXF2DImport.ReadDXFAsContainer: TContainer2D;
begin
  fUnableToReadAll := False;
  ReadHeader;
  ReadBlocks;
  Result := TContainer2D.Create(0, [nil]);
  try
   FDXFRead.Rewind;
   while (FDXFRead.CurrentSection <> scUnknow) do
    begin
      if FDXFRead.CurrentSection = scEntities then
       ReadEntitiesAsContainer(Result);
      FDXFRead.NextSection;
    end;
   Result.UpdateExtension(Self);
  except
   on Exception do
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

procedure TDXF2DImport.ReadHeader;
var
  Entry: TGroupTable;
begin
  fHasExtension := False;
  fExtension.Right := 1000;
  fExtension.Top := 1000;
  fExtension.W1 := 1.0;
  fExtension.Left := -1000;
  fExtension.Bottom := -1000;
  fExtension.W2 := 1.0;
  fAngleDir := adCounterClockwise;
  while fDXFRead.ReadAnEntry(9, {%H-}Entry) <> 0 do
   begin
     if Entry[9] = '$ANGDIR' then
      begin
        if Entry[70] = 1 then
         fAngleDir := adClockwise
        else
         fAngleDir := adCounterClockwise;
      end
     else if Entry[9] = '$EXTMAX' then
      begin
        fExtension.Right := Entry[10];
        fExtension.Top := Entry[20];
        fHasExtension := True;
      end
     else if Entry[9] = '$EXTMIN' then
      begin
        fExtension.Left := Entry[10];
        fExtension.Bottom := Entry[20];
        fHasExtension := True;
      end;
   end;
  if fHasExtension then
   FExtension := ReOrderRect2D(FExtension);
end;

procedure TDXF2DImport.ReadTables;
var
  Entry: TGroupTable;
begin
  if fCADCmp2D = nil then Exit;
  {%H-}fDXFRead.ReadAnEntry(0, {%H-}Entry);
  while fDXFRead.GroupValue <> 'ENDSEC' do
   begin
     if Entry[0] = 'LAYER' then
      begin
        if fSetLayers then
         with fCADCmp2D.Layers[fLayerList.Count] do
          begin
            Pen.Color := Colors[Abs(Round( Float(Entry[62]) ))];
            Brush.Style := bsClear;
            Active := Entry[62] >= 0;
            Name := Entry[2];
          end;
        fLayerList.AddObject(Entry[2], fCADCmp2D.Layers[fLayerList.Count]);
      end;
     fDXFRead.ReadAnEntry(0, Entry);
   end;
end;

procedure TDXF2DImport.ReadEntities;
var
 Tmp: TObject2D;
begin
  if fCADCmp2D = nil then Exit;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     Tmp := ReadEntity(False);
     if Assigned(Tmp) then
      begin
        Tmp.Transform(Scale2D(fScale, fScale));
        Tmp.ApplyTransform;
        FCADCmp2D.AddObject(-1, Tmp);
      end
     else
      fUnableToReadAll := True;
   end;
end;

procedure TDXF2DImport.ReadBlocks;
var
  Entry: TGroupTable;
  Tmp: TSourceBlock2D;
  NLayer: Integer;
begin
  if fCADCmp2D = nil then Exit;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     FDXFRead.ReadAnEntry(0, {%H-}Entry);
     NLayer := FLayerList.IndexOf(Entry[8]);
     if NLayer > -1 then
      FCADCmp2D.CurrentLayer := NLayer;
     if Entry[0] = 'BLOCK' then
      begin
        Tmp := ReadSourceBlock(Entry);
        if Assigned(Tmp) then
         begin
           FCADCmp2D.AddSourceBlock(Tmp);
           { Entry[2] contains the name of the block for future ref. }
           fBlockList.AddObject(Entry[2], Tmp);
         end;
      end;
   end;
end;

function TDXF2DImport.ReadSourceBlock(Entry: TGroupTable): TSourceBlock2D;
var
  BasePoint: TPoint2D;
  Tmp: TObject2D;
  TmpName: TSourceBlockName;
begin
  Result := nil;
  if (Entry[70] and $4) or (Entry[70] and $1) then
   { XRef and anonymous are not allowed. }
   Exit;
  BasePoint.X := Entry[10];
  BasePoint.Y := Entry[20];
  BasePoint.W := 1.0;
  if Entry[2] = '' then
   TmpName := StringToBlockName(Format('BLOCK%d', [FCADCmp2D.SourceBlocksCount]))
  else
   TmpName := StringToBlockName(Entry[2]);
  Result := TSourceBlock2D.Create(0, TmpName, [nil]);
  while FDXFRead.GroupValue <> 'ENDBLK' do
   begin
     Tmp := ReadEntity(False);
     if Assigned(Tmp) then
      Result.Objects.Add(Tmp);
   end;
  if Result.Objects.Count > 0 then
   begin
     Result.Transform(Translate2D(-BasePoint.X, -BasePoint.Y));
     Result.ApplyTransform;
     Result.UpdateExtension(Self);
   end
  else
   begin
     Result.Free;
     Result := nil;
   end;
end;

{ --================ DXF2DExport ==================-- }

constructor TDXF2DExport.Create(const FileName: String; const CAD: TCADCmp2D);
begin
  inherited Create;
  FDXFWrite := TDXFWrite.Create(FileName);
  FCADCmp2D := CAD;
end;

destructor TDXF2DExport.Destroy;
begin
  FDXFWrite.WriteGroup(0, 'EOF');
  FDXFWrite.Free;
  inherited Destroy;
end;

procedure TDXF2DExport.WriteDXF;
begin
  if fCADCmp2D = nil then Exit;
  FDXFWrite.Reset;
  WriteHeader;
  WriteTables;
  WriteBlocks;
  WriteEntities;
end;

procedure TDXF2DExport.WriteHeader;
var
  TmpInt: Integer;
begin
  if fCADCmp2D = nil then Exit;
  with FDXFWrite do
   begin
     BeginSection(scHeader);
     { Angle direction, Default CounterClockWise }
     WriteGroup(9, '$ANGDIR');
     TmpInt := 0;
     WriteGroup(70, TmpInt);
     { Extension of the drawing }
     WriteGroup(9, '$EXTMAX');
     WriteGroup(10, FCADCmp2D.DrawingExtension.Right);
     WriteGroup(20, FCADCmp2D.DrawingExtension.Top);
     WriteGroup(30, 0);
     WriteGroup(9, '$EXTMIN');
     WriteGroup(10, FCADCmp2D.DrawingExtension.Left);
     WriteGroup(20, FCADCmp2D.DrawingExtension.Bottom);
     WriteGroup(30, 0);
     WriteGroup(9, '$CECOLOR');
     WriteGroup(62, 256);
     WriteGroup(9, '$CELTYPE');
     WriteGroup(6, 'BYLAYER');
     { End Header Section }
     EndSection(scHeader);
   end;
end;

procedure TDXF2DExport.WriteTables;
var
  Count: Integer;
begin
  if fCADCmp2D = nil then Exit;
  with FDXFWrite do
   begin
     { Begin Tables Section }
     BeginSection(scTables);
     WriteGroup(0, 'TABLE');
     WriteGroup(2, 'LTYPE');
     WriteGroup(70, 1);
     WriteGroup(0, 'LTYPE');
     WriteGroup(2, 'CONTINUOUS');
     WriteGroup(70, 64);
     WriteGroup(3, 'Solid line');
     WriteGroup(72, 65);
     WriteGroup(73, 0);
     WriteGroup(40, 0.0);
     WriteGroup(0, 'ENDTAB');
     WriteGroup(0, 'TABLE');
     WriteGroup(2, 'LAYER');
     WriteGroup(70, 257);
     WriteGroup(0, 'LAYER');
     WriteGroup(2, '0');
     WriteGroup(70, 64);
     WriteGroup(62, 7);
     WriteGroup(6, 'CONTINUOUS');
     for Count := 0 to 255 do
      with FCADCmp2D.Layers[Count] do
       if Modified then
        begin
          WriteGroup(0, 'LAYER');
          WriteGroup(2, Name);
          WriteGroup(70, 64);
          WriteGroup(62, ColorToIndex(Pen.Color, Active));
          WriteGroup(6, 'CONTINUOUS');
        end;
     WriteGroup(0, 'ENDTAB');
     { End Tables Section }
     EndSection(scTables);
   end;
end;

procedure TDXF2DExport.WriteEntities;
var
  TmpIter: TGraphicObjIterator;
  TmpObj: TObject2D;
begin
  if fCADCmp2D = nil then Exit;
  FDXFWrite.BeginSection(scEntities);
  TmpIter := fCADCmp2D.ObjectsIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       WriteEntity(TmpObj);
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
    FDXFWrite.EndSection(scEntities);
  end;
end;

procedure TDXF2DExport.WriteBlocks;
var
  TmpIter: TGraphicObjIterator;
  TmpObj: TObject2D;
begin
  if fCADCmp2D = nil then Exit;
  FDXFWrite.BeginSection(scBlocks);
  TmpIter := fCADCmp2D.SourceBlocksIterator;
  try
    TmpObj := TmpIter.First as TObject2D;
    while TmpObj <> nil do
     begin
       WriteEntity(TmpObj);
       TmpObj := TmpIter.Next as TObject2D;
     end;
  finally
    TmpIter.Free;
    FDXFWrite.EndSection(scBlocks);
  end;
end;

procedure TDXF2DExport.WriteLine2D(Line: TLine2D);
var
 TmpPnt: TPoint2D;
begin
  with FDXFWrite, Line do
   begin
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     TmpPnt := TransformPoint2D(Points[1], ModelTransform);
     WriteGroup(11, TmpPnt.X);
     WriteGroup(21, TmpPnt.Y);
     WriteGroup(31, 0);
   end;
end;

procedure TDXF2DExport.WriteFrame2D(Frm: TFrame2D);
var
 TmpPnt: TPoint2D;
begin
  with FDXFWrite, Frm do
   begin
     WriteGroup(66, 1);
     WriteGroup(10, 0);
     WriteGroup(20, 0);
     WriteGroup(30, 0); // Questo setta l'elevazione.
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[Frm.Layer].Name);
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[Frm.Layer].Name);
     TmpPnt := TransformPoint2D(Point2D(Points[0].X, Points[1].Y), ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[Frm.Layer].Name);
     TmpPnt := TransformPoint2D(Points[1], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[Frm.Layer].Name);
     TmpPnt := TransformPoint2D(Point2D(Points[1].X, Points[0].Y), ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'VERTEX');
     WriteGroup(8, FCADCmp2D.Layers[Frm.Layer].Name);
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     WriteGroup(10, TmpPnt.X);
     WriteGroup(20, TmpPnt.Y);
     WriteGroup(30, 0);
     WriteGroup(0, 'SEQEND');
   end;
end;

procedure TDXF2DExport.WriteJVText2D(Text: TJustifiedVectText2D);
var
  TmpPnt, TmpPnt1: TPoint2D;
  InsPnt: TPoint2D;
  AllPnt: TPoint2D;
  TmpAngle: Single;
begin
  with FDXFWrite, Text do
   begin
     WriteGroup(1, Text);
     WriteGroup(40, Height);
     TmpPnt := TransformPoint2D(Points[0], ModelTransform);
     AllPnt := TransformPoint2D(Points[1], ModelTransform);
     InsPnt := TransformPoint2D(Point2D(Points[0].X, Points[1].Y - Height), ModelTransform);
     WriteGroup(10, InsPnt.X);
     WriteGroup(20, InsPnt.Y);
     WriteGroup(30, 0);
     // I compute the angle of the Text from the rotation of it.
     TmpPnt := TransformPoint2D(Point2D(0, 0), ModelTransform);
     TmpPnt1 := TransformPoint2D(Point2D(10, 0), ModelTransform);
     TmpAngle := ArcTan2(TmpPnt1.Y - TmpPnt.Y, TmpPnt1.X - TmpPnt.X);
     WriteGroup(50, RadToDeg(TmpAngle));
     case HorizontalJust of
       jhCenter: begin
         WriteGroup(11, AllPnt.X - InsPnt.X);
         WriteGroup(21, AllPnt.Y - InsPnt.Y);
         WriteGroup(31, 0);
         WriteGroup(72, 1);
       end;
       jhRight: begin
         WriteGroup(11, AllPnt.X);
         WriteGroup(21, AllPnt.Y);
         WriteGroup(31, 0);
         WriteGroup(72, 2);
       end;
     end;
  end;
end;

procedure TDXF2DExport.WriteText2D(Text: TText2D);
begin
  with FDXFWrite, Text do
   begin
     WriteGroup(1, Text);
     WriteGroup(40, Height);
     WriteGroup(10, Points[0].X);
     WriteGroup(20, Points[0].Y);
     WriteGroup(30, 0);
     WriteGroup(50, 0);
     WriteGroup(72, 0);
  end;
end;
procedure TDXF2DExport.WriteCurve2D(Curve: TCurve2D);
var
  Count: Integer;
  TmpPnt: TPoint2D;
begin
  with FDXFWrite, Curve do
   begin
     BeginUseProfilePoints;
     try
       WriteGroup(66, 1);
       WriteGroup(10, 0);
       WriteGroup(20, 0);
       WriteGroup(30, 0); // Questo setta l'elevazione.
       for Count := 0 to ProfilePoints.Count - 1 do
        begin
          WriteGroup(0, 'VERTEX');
          WriteGroup(8, FCADCmp2D.Layers[Curve.Layer].Name);
          TmpPnt := TransformPoint2D(ProfilePoints[Count], ModelTransform);
          WriteGroup(10, TmpPnt.X);
          WriteGroup(20, TmpPnt.Y);
          WriteGroup(30, 0);
        end;
       WriteGroup(0, 'SEQEND');
     finally
       EndUseProfilePoints;
     end;
   end;
end;

procedure TDXF2DExport.WriteOutline2D(Poly: TOutline2D);
var
  Count: Integer;
  TmpPnt: TPoint2D;
begin
  with FDXFWrite, Poly do
   begin
     WriteGroup(66, 1);
     WriteGroup(10, 0);
     WriteGroup(20, 0);
     WriteGroup(30, 0); // Questo setta l'elevazione.
     for Count := 0 to Points.Count - 1 do
      begin
        WriteGroup(0, 'VERTEX');
        WriteGroup(8, FCADCmp2D.Layers[Poly.Layer].Name);
        TmpPnt := TransformPoint2D(Points[Count], ModelTransform);
        WriteGroup(10, TmpPnt.X);
        WriteGroup(20, TmpPnt.Y);
        WriteGroup(30, 0);
      end;
     WriteGroup(0, 'SEQEND');
   end;
end;

procedure TDXF2DExport.WriteBlock(Block: TBlock2D);
begin
  with FDXFWrite, Block do
   begin
     WriteGroup(2, Format('%s', [SourceName]));
     WriteGroup(10, ModelTransform[3,1]);
     WriteGroup(20, ModelTransform[3,2]);
     WriteGroup(30, 0);
     WriteGroup(41, sqrt( sqr(ModelTransform[1,1]) + sqr(ModelTransform[1,2]) ));
     WriteGroup(42, sqrt( sqr(ModelTransform[2,1]) + sqr(ModelTransform[2,2]) ));
     WriteGroup(50, radtodeg(ArcTan2(ModelTransform[1,2], ModelTransform[1,1])) );
   end;
end;

procedure TDXF2DExport.WriteEntity(Obj: TObject2D);
var
  sb: TSourceBlock2D;
  tmpIter: TGraphicObjIterator;
  tmpObj: TObject2D;
begin
  with FDXFWrite do
   if Obj is TLine2D then
    begin
      WriteGroup(0, 'LINE');
      WriteGroup(8, FCADCmp2D.Layers[Obj.Layer].Name);
      WriteLine2D(Obj as TLine2D);
    end
   else if Obj is TFrame2D then
    begin
      WriteGroup(0, 'POLYLINE');
      WriteGroup(8, FCADCmp2D.Layers[Obj.Layer].Name);
      WriteFrame2D(Obj as TFrame2D);
    end
   else if Obj is TCurve2D then
    begin
      WriteGroup(0, 'POLYLINE');
      WriteGroup(8, FCADCmp2D.Layers[Obj.Layer].Name);
      WriteCurve2D(Obj as TCurve2D);
    end
   else if Obj is TOutline2D then
    begin
      WriteGroup(0, 'POLYLINE');
      WriteGroup(8, FCADCmp2D.Layers[Obj.Layer].Name);
      WriteOutline2D(Obj as TOutline2D);
    end
   else if Obj is TJustifiedVectText2D then
    begin
      WriteGroup(0, 'TEXT');
      WriteGroup(8, FCADCmp2D.Layers[Obj.Layer].Name);
      WriteJVText2D(Obj as TJustifiedVectText2D);
    end
   else if Obj is TBlock2D then
    begin
      WriteGroup(0, 'INSERT');
      WriteGroup(8, FCADCmp2D.Layers[Obj.Layer].Name);
      WriteBlock(Obj as TBlock2D);
    end
   else if Obj is TSourceBlock2D then
     begin
       sb:=TSourceBlock2D(obj);
       WriteGroup(0, 'BLOCK');
       WriteGroup(8,  FCADCmp2D.Layers[Obj.Layer].Name);
       WriteGroup(62, 0);
       WriteGroup(2, format('%s',[sb.Name]));
       WriteGroup(70, 0);
       WriteGroup(10, 0.0);
       WriteGroup(20, 0.0);
       WriteGroup(30, 0.0);
       WriteGroup(3, format('%s',[sb.Name]));
       WriteGroup(1, '');
       tmpiter:=sb.Objects.GetIterator;
       try
         tmpobj:=TmpIter.First as TObject2D;
         while tmpobj<> nil do begin
           WriteEntity(tmpobj);
           tmpobj:= tmpiter.Next as TObject2D;
         end;
       finally
         tmpiter.Free;
       end;
       WriteGroup(0, 'ENDBLK');
     end
   else if Obj is TText2D then
    begin
      WriteGroup(0, 'TEXT');
      WriteGroup(8, FCADCmp2D.Layers[Obj.Layer].Name);
      WriteText2D(Obj as TText2D);
    end;
end;

// -----===== Starting Cs4DXF3DConverter.pas =====-----

{ --================ DXF3DImport ==================-- }

constructor TDXF3DImport.Create(const FileName: String; const CAD: TCADCmp3D);
begin
  inherited Create;
  try
   fTextFont := CADSysFindFontByIndex(0);
  except
  end;
  FDXFRead := TDXFRead.Create(FileName);
  FCADCmp3D := CAD;
  FLayerList := TStringList.Create;
  FBlockList := TStringList.Create;
  fHasExtension := False;
  fScale := 1.0;
  fSetLayers := True;
  fUnableToReadAll := False;
end;

destructor TDXF3DImport.Destroy;
begin
  FDXFRead.Free;
  FLayerList.Free;
  FBlockList.Free;
  inherited Destroy;
end;

function TDXF3DImport.GoToSection(Sect: TSections): Boolean;
begin
  FDXFRead.Rewind;
  while (FDXFRead.CurrentSection <> scUnknow) and
        (FDXFRead.CurrentSection <> Sect) do
   FDXFRead.NextSection;
  Result := FDXFRead.CurrentSection = Sect;
end;

procedure TDXF3DImport.SetTextFont(F: TVectFont);
begin
  fTextFont := F;
end;

procedure TDXF3DImport.ReadDXF;
begin
  if fCADCmp3D = nil then Exit;
  FDXFRead.Rewind;
  while (FDXFRead.CurrentSection <> scUnknow) do
   begin
     case FDXFRead.CurrentSection of
      scHeader: ReadHeader;
      scTables: ReadTables;
      scBlocks: ReadBlocks;
      scEntities: ReadEntities;
     end;
     FDXFRead.NextSection;
   end;
end;

procedure TDXF3DImport.ReadEntitiesAsContainer(const Container: TContainer3D);
var
 Tmp: TObject3D;
 ID: Integer;
begin
  ID := 0;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     Tmp := ReadEntity(True);
     if Assigned(Tmp) then
      begin
        Tmp.Transform(Scale3D(fScale, fScale, fScale));
        Tmp.ApplyTransform;
        Tmp.ID := ID;
        Container.Objects.Add(Tmp);
        Inc(ID)
      end
     else
      fUnableToReadAll := True;
   end;
  Container.UpdateExtension(Self);
end;

function TDXF3DImport.ReadDXFAsSourceBlock(const Name: TSourceBlockName): TSourceBlock3D;
begin
  ReadHeader;
  Result := TSourceBlock3D.Create(0, Name, [nil]);
  try
   FDXFRead.Rewind;
   while (FDXFRead.CurrentSection <> scUnknow) do
    begin
      if FDXFRead.CurrentSection = scEntities then
       ReadEntitiesAsContainer(Result);
      FDXFRead.NextSection;
    end;
   Result.UpdateExtension(Self);
  except
   on Exception do
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

function TDXF3DImport.ReadDXFAsContainer: TContainer3D;
begin
  ReadHeader;
  Result := TContainer3D.Create(0, [nil]);
  try
   FDXFRead.Rewind;
   while (FDXFRead.CurrentSection <> scUnknow) do
    begin
      if FDXFRead.CurrentSection = scEntities then
       ReadEntitiesAsContainer(Result);
      FDXFRead.NextSection;
    end;
   Result.UpdateExtension(Self);
  except
   on Exception do
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

procedure TDXF3DImport.ReadHeader;
var
  Entry: TGroupTable;
begin
  fHasExtension := False;
  FAngleDir := adCounterClockwise;
  while FDXFRead.ReadAnEntry(9, {%H-}Entry) <> 0 do
   begin
     if Entry[9] = '$ANGDIR' then
      begin
        if Entry[70] = 1 then
         FAngleDir := adClockwise
        else
         FAngleDir := adCounterClockwise
      end
     else if Entry[9] = '$EXTMAX' then
      begin
        FExtension.Right := Entry[10];
        FExtension.Top := Entry[20];
        FExtension.Front := Entry[30];
        fHasExtension := True;
      end
     else if Entry[9] = '$EXTMIN' then
      begin
        FExtension.Left := Entry[10];
        FExtension.Bottom := Entry[20];
        FExtension.Back := Entry[30];
        fHasExtension := True;
      end;
   end;
  if fHasExtension then
   FExtension := ReOrderRect3D(FExtension);
end;

procedure TDXF3DImport.ReadTables;
var
  Entry: TGroupTable;
begin
  if fCADCmp3D = nil then Exit;
  FDXFRead.ReadAnEntry(0, {%H-}Entry);
  while FDXFRead.GroupValue <> 'ENDSEC' do
   begin
     if Entry[0] = 'LAYER' then
      begin
        if fSetLayers then
         with FCADCmp3D.Layers[FLayerList.Count] do
          begin
            Pen.Color := Colors[Abs(Round(Double(Entry[62])))];
            Brush.Style := bsClear;
            Active := Entry[62] >= 0;
            Name := Entry[2];
          end;
        FLayerList.AddObject(Entry[2], FCADCmp3D.Layers[FLayerList.Count]);
      end;
     FDXFRead.ReadAnEntry(0, Entry);
   end;
end;

procedure TDXF3DImport.ReadEntities;
var
 Tmp: TObject3D;
begin
  if fCADCmp3D = nil then Exit;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     Tmp := ReadEntity(False);
     if Assigned(Tmp) then
      begin
        Tmp.Transform(Scale3D(fScale, fScale, fScale));
        Tmp.ApplyTransform;
        FCADCmp3D.AddObject(-1, Tmp);
      end
     else
      fUnableToReadAll := True;
   end;
end;

procedure TDXF3DImport.ReadBlocks;
var
  Entry: TGroupTable;
  Tmp: TSourceBlock3D;
  NLayer: Integer;
begin
  if fCADCmp3D = nil then Exit;
  while DXFRead.GroupValue <> 'ENDSEC' do
   begin
     FDXFRead.ReadAnEntry(0, {%H-}Entry);
     NLayer := FLayerList.IndexOf(Entry[8]);
     if NLayer > -1 then
      FCADCmp3D.CurrentLayer := NLayer;
     if (Entry[0] = 'BLOCK') then
      begin
        Tmp := ReadSourceBlock(Entry);
        if Assigned(Tmp) then
         fCADCmp3D.AddSourceBlock(Tmp);
      end;
   end;
end;

function TDXF3DImport.ReadLine3D(Entry: TGroupTable): TLine3D;
begin
  Result := TLine3D.Create(-1, Point3D(Entry[10], Entry[20], Entry[30]),
                              Point3D(Entry[11], Entry[21], Entry[31]));
end;

function TDXF3DImport.ReadEllipse3D(Entry: TGroupTable): TPlanarCurve3D;
var
  CenterPt, MajorAx: TPoint3D;
  XAx, YAx, NAx: TVector3D;
  MinorLen, MajorLen, SA, EA: TRealType;
begin
  CenterPt := Point3D(Entry[10], Entry[20], Entry[30]);
  MajorAx := Point3D(Entry[11], Entry[21], Entry[31]);
  MinorLen := Entry[40];
  MajorLen := PointDistance3D(CenterPt, MajorAx);
  SA := Entry[41];
  EA := Entry[42];

  XAx := Direction3D(CenterPt, MajorAx);
  if VarType(Entry[210]) <> 0 then
   NAx := Versor3D(Entry[210], Entry[220], Entry[230])
  else
   NAx := Versor3D(0, 0, 1);
  YAx := CrossProd3D(NAx, XAx);

  if (SA = 0.0) and (EA = 2 * Pi) then
   // Ellisse completa
   Result := TEllipse3D.Create(0, CenterPt, XAx, YAx, CenterPt, CenterPt)
  else
   // Arco di ellisse
   Result := TArc3D.Create(0, CenterPt, XAx, YAx, CenterPt, CenterPt, SA, EA);
  with Result do
   begin
     MajorAx := ExtrudePoint3D(CenterPt, XAx, -MajorLen);
     MajorAx := ExtrudePoint3D(MajorAx, YAx, -MinorLen);
     Points[0] := Result.WorldToObject(MajorAx);
     MajorAx := ExtrudePoint3D(CenterPt, XAx, MajorLen);
     MajorAx := ExtrudePoint3D(MajorAx, YAx, MinorLen);
     Points[1] := Result.WorldToObject(MajorAx);
   end;
end;

function TDXF3DImport.ReadCircle3D(Entry: TGroupTable): TEllipse3D;
var
  CenterPt, P1, P2: TPoint3D;
  XAx, YAx, NAx: TVector3D;
  Radious: TRealType;
begin
  CenterPt := Point3D(Entry[10], Entry[20], Entry[30]);
  Radious := Entry[40];
  if VarType(Entry[210]) <> 0 then
   NAx := Versor3D(Entry[210], Entry[220], Entry[230])
  else
   NAx := Versor3D(0, 0, 1);
  XAx := Versor3D(1, 0, 0);
  YAx := CrossProd3D(NAx, XAx);

  P1 := ExtrudePoint3D(CenterPt, XAx, -Radious);
  P1 := ExtrudePoint3D(P1, YAx, -Radious);
  P2 := ExtrudePoint3D(CenterPt, XAx, Radious);
  P2 := ExtrudePoint3D(P2, YAx, Radious);
  Result := TEllipse3D.Create(-1, CenterPt, XAx, YAx, Point3D(0, 0, 0), Point3D(10, 10, 0));
  // I punti di controllo sono in coordinate oggetto.
  Result.Points[0] := Result.WorldToObject(Result.Points[0]);
  Result.Points[1] := Result.WorldToObject(Result.Points[1]);
end;

function TDXF3DImport.ReadArc3D(Entry: TGroupTable): TArc3D;
var
  CenterPt, P1, P2: TPoint3D;
  XAx, YAx, NAx: TVector3D;
  SA, EA, Radious: TRealType;
begin
  CenterPt := Point3D(Entry[10], Entry[20], Entry[30]);
  Radious := Entry[40];
  SA := Entry[50];
  EA := Entry[51];
  if VarType(Entry[210]) <> 0 then
   NAx := Versor3D(Entry[210], Entry[220], Entry[230])
  else
   NAx := Versor3D(0, 0, 1);
  XAx := Versor3D(1, 0, 0);
  YAx := CrossProd3D(NAx, XAx);

  P1 := ExtrudePoint3D(CenterPt, XAx, -Radious);
  P1 := ExtrudePoint3D(P1, YAx, -Radious);
  P2 := ExtrudePoint3D(CenterPt, XAx, Radious);
  P2 := ExtrudePoint3D(P2, YAx, Radious);

  Result := TArc3D.Create(-1, CenterPt, XAx, YAx, Point3D(0, 0, 0), Point3D(10, 10, 0), SA, EA);
  // I punti di controllo sono in coordinate oggetto.
  Result.Points[0] := Result.WorldToObject(Result.Points[0]);
  Result.Points[1] := Result.WorldToObject(Result.Points[1]);
  Result.Direction := fAngleDir;
end;

function TDXF3DImport.ReadTrace3D(Entry: TGroupTable): TPolyline3D;
var
  P1, P2, P3, P4: TPoint3D;
begin
  P1 := Point3D(Entry[10], Entry[20], Entry[30]);
  P2 := Point3D(Entry[11], Entry[21], Entry[31]);
  P3 := Point3D(Entry[12], Entry[22], Entry[32]);
  P4 := Point3D(Entry[13], Entry[23], Entry[33]);
  Result := TPolyline3D.Create(0, [P1, P2, P3, P4]);
end;

function TDXF3DImport.ReadText3D(Entry: TGroupTable): TJustifiedVectText3D;
var
  TmpRect: TRect2D;
  YAx, NAx: TVector3D;
  P: TPoint3D;
begin
  Result := nil;
  if fTextFont = nil then
   Exit;
  // Trova i parametri del piano.
  if VarType(Entry[210]) <> 0 then
   NAx := Versor3D(Entry[210], Entry[220], Entry[230])
  else
   NAx := Versor3D(0, 0, 1);
  if IsSameVector3D(CrossProd3D(NAx, Versor3D(0, 1, 0)), Versor3D(0, 0, 0)) then
   YAx := Versor3D(1, 0, 0)
  else
   YAx := Versor3D(0, 1, 0);
  P := Point3D(Entry[10], Entry[20], Entry[30]);
  TmpRect.FirstEdge := Point2D(0, 0);
  TmpRect.SecondEdge := Point2D(0, 0);
  Result := TJustifiedVectText3D.Create(-1,
                       P,
                       NAx, YAx,
                       fTextFont,
                       TmpRect,
                       Entry[40],
                       Entry[1]);
  if VarType(Entry[72]) <> 0 then
   begin
     case Entry[72] of
      1: Result.HorizontalJust := jhCenter;
      2: Result.HorizontalJust := jhRight;
     end;
     case Entry[73] of
      1: Result.VerticalJust := jvBottom;
      2: Result.VerticalJust := jvCenter;
     end;
   end;
  if VarType(Entry[50]) <> varEmpty then
   Result.Transform(RotateOnAxis3D(P, NAx, Entry[50]));
end;

function TDXF3DImport.ReadPolyline3D(Entry: TGroupTable): TPrimitive3D;
var
  LocalEntry: TGroupTable;
  IsClosedInM, IsMesh3D, IsPolyFace3D, IsSpline3D: Boolean;
  V1, V2, V3, V4, Cont: Integer;
begin
  // Considera le polilinee 2D e 3D alla stessa maniera.
  // Determina i flags della polilinea.
  if VarType(Entry[70]) <> varEmpty then
   begin
     IsClosedInM := (Entry[70] and 1) = 1;
     IsMesh3D := (Entry[70] and 16) = 16;
     IsPolyFace3D := (Entry[70] and 64) = 64;
     IsSpline3D := ((Entry[70] and 2) = 2) or ((Entry[70] and 4) = 4);
   end
  else
   begin
     IsClosedInM := False;
     IsMesh3D := False;
     IsSpline3D := False;
     IsPolyFace3D := False;
   end;

  if IsPolyFace3D then
   begin
     Result := TPolyface3D.Create(-1, Entry[71], Entry[72], [Point3D(0, 0, 0)]);
     Result.Points.Delete(0);
     Cont := 0;
     // Leggo i punti della polyface.
     fDXFRead.ReadAnEntry(0, {%H-}LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      with TPolyface3D(Result) do begin
        if (VarType(LocalEntry[70]) <> varEmpty) and
           ((LocalEntry[70] and 128) = 128) then
         begin
           if ((LocalEntry[70] and 64) = 64) then
            Points.Add(Point3D(LocalEntry[10], LocalEntry[20], LocalEntry[30]))
           else
            begin
              if VarType(LocalEntry[71]) <> varEmpty then
               V1 := LocalEntry[71]
              else
               V1 := 0;
              if VarType(LocalEntry[72]) <> varEmpty then
               V2 := LocalEntry[72]
              else
               V2 := 0;
              if VarType(LocalEntry[73]) <> varEmpty then
               V3 := LocalEntry[73]
              else
               V3 := 0;
              if VarType(LocalEntry[74]) <> varEmpty then
               V4 := LocalEntry[74]
              else
               V4 := 0;
              AddFace(Cont, V1, V2, V3, V4);
              Inc(Cont);
            end;
         end;
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
   end
  else if IsSpline3D then
   begin // Leggo la spline3D
     Result := TPolyline3D.Create(0, [Point3D(0, 0, 0)]);
     Result.Points.Delete(0);
     // Leggo soli punti aggiunti, quindi gruppo 70 con bit 8.
     fDXFRead.ReadAnEntry(0, LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      begin
        if (VarType(LocalEntry[70]) <> varEmpty)
           and ((LocalEntry[70] and 1) = 1)
           or ((LocalEntry[70] and 8) = 8) then
         Result.Points.Add(Point3D(LocalEntry[10], LocalEntry[20], LocalEntry[30]));
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
     if Result.Points.Count = 0 then
      begin // La spline è stata creata senza i punti aggiuntivi.
        Result.Free;
        Result := nil;
        if fVerbose then
         ShowMessage('Spline without spline-fitting isn''t supported.');
      end;
   end
  else if IsMesh3D then
   begin // Leggo la mesh3D
     Result := TMesh3D.Create(-1, Entry[71], Entry[72], [Point3D(0, 0, 0)]);
     Result.Points.Delete(0);
     // Leggo i punti della mesh.
     fDXFRead.ReadAnEntry(0, LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      begin
        if (VarType(LocalEntry[70]) <> varEmpty)
           and ((LocalEntry[70] and 64) = 64) then
         Result.Points.Add(Point3D(LocalEntry[10], LocalEntry[20], LocalEntry[30]));
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
   end
  else
   begin // Leggo la polilinea3D
     Result := TPolyline3D.Create(0, [Point3D(0, 0, 0)]);
     Result.Points.Delete(0);
     // Leggo tutti i punti.
     fDXFRead.ReadAnEntry(0, LocalEntry);
     while LocalEntry[0] = 'VERTEX' do
      begin
        Result.Points.Add(Point3D(LocalEntry[10], LocalEntry[20], LocalEntry[30]));
        FDXFRead.ReadAnEntry(0, LocalEntry);
      end;
     if IsClosedInM then // la chiudo.
      Result.Points.Add(Result.Points[0]);
   end;
  if (LocalEntry[0] <> 'SEQEND') then
   Raise EDXFInvalidDXF.Create('Invalid DXF file.');
end;

function TDXF3DImport.ReadPlanarFace3D(Entry: TGroupTable): TPlanarFace3D;
var
  TmpVect: TPointsSet3D;
  XDir, YDir: TVector3D;
begin
  TmpVect := TPointsSet3D.Create(4);
  try
    TmpVect.Add(Point3D(Entry[10], Entry[20], Entry[30]));
    if not IsSamePoint3D(TmpVect[0], Point3D(Entry[11], Entry[21], Entry[31])) then
     TmpVect.Add(Point3D(Entry[11], Entry[21], Entry[31]));
    if not IsSamePoint3D(TmpVect[1], Point3D(Entry[12], Entry[22], Entry[32])) then
     TmpVect.Add(Point3D(Entry[12], Entry[22], Entry[32]));
    if VarType(Entry[13]) <> varEmpty then
     begin
       if not IsSamePoint3D(TmpVect[2], Point3D(Entry[13], Entry[23], Entry[33])) then
        TmpVect.Add(Point3D(Entry[13], Entry[23], Entry[33]));
     end;
    case TmpVect.Count of
     1, 2: Result := nil;
     3: begin
       XDir := Direction3D(TmpVect[0], TmpVect[1]);
       YDir := CrossProd3D(GetVectNormal(TmpVect.PointsReference, TmpVect.Count), XDir);
       Result := TPlanarFace3D.Create(0, TmpVect[0], XDir, YDir,
            [TmpVect[0]]);
       // I punti di controllo sono in coordinate oggetto.
       Result.Points.Clear;
       Result.Points.Add(Result.WorldToObject(TmpVect[0]));
       Result.Points.Add(Result.WorldToObject(TmpVect[1]));
       Result.Points.Add(Result.WorldToObject(TmpVect[2]));
     end;
     4: begin
       XDir := Direction3D(TmpVect[0], TmpVect[1]);
       YDir := CrossProd3D(GetVectNormal(TmpVect.PointsReference, TmpVect.Count), XDir);
       Result := TPlanarFace3D.Create(0, TmpVect[0], XDir, YDir,
            [TmpVect[0]]);
       // I punti di controllo sono in coordinate oggetto.
       Result.Points.Clear;
       Result.Points.Add(Result.WorldToObject(TmpVect[0]));
       Result.Points.Add(Result.WorldToObject(TmpVect[1]));
       Result.Points.Add(Result.WorldToObject(TmpVect[2]));
       Result.Points.Add(Result.WorldToObject(TmpVect[3]));
     end;
    else
     Result := nil;
    end;
  finally
    TmpVect.Free;
  end;
end;

function TDXF3DImport.ReadBlock(Entry: TGroupTable): TBlock3D;
var
  TmpSource: TSourceBlock3D;
  TmpTransf: TTransf3D;
  InsertPt: TPoint3D;
  XScl, YScl, ZScl, RotAng, ColSpace, RowSpace: TRealType;
  XAx, YAx, NAx: TVector3D;
  ColCont, RowCont, C, R: Integer;
begin
  Result := nil;
  TmpSource := FCADCmp3D.FindSourceBlock(StringToBlockName(Entry[2]));
  if TmpSource <> nil then
   begin
     InsertPt := Point3D(Entry[10], Entry[20], Entry[30]);
     if VarType(Entry[41]) <> varEmpty then
      XScl := Entry[41]
     else
      XScl := 1.0;
     if VarType(Entry[41]) <> varEmpty then
      YScl := Entry[42]
     else
      YScl := 1.0;
     if VarType(Entry[41]) <> varEmpty then
      ZScl := Entry[43]
     else
      ZScl := 1.0;
     RotAng := Entry[50];
     if VarType(Entry[70]) <> varEmpty then
      ColCont := Entry[70]
     else
      ColCont := 1;
     if VarType(Entry[71]) <> varEmpty then
      RowCont := Entry[71]
     else
      RowCont := 1;
     ColSpace := Entry[44];
     RowSpace := Entry[45];
     if VarType(Entry[210]) <> 0 then
      NAx := Versor3D(Entry[210], Entry[220], Entry[230])
     else
      NAx := Versor3D(0, 0, 1);
     XAx := Versor3D(1, 0, 0);
     YAx := CrossProd3D(NAx, XAx);
     // Creazione trasformazione.
     TmpTransf := Scale3D(XScl, YScl, ZScl);
     TmpTransf := MultiplyTransform3D(TmpTransf, RotateOnAxis3D(InsertPt, NAx, RotAng));
     // Creazione oggetti.
     for R := 1 to RowCont do
      begin
        for C := 1 to ColCont do
         begin
           Result := TBlock3D.Create(0, TmpSource);
           Result.ModelTransform := TmpTransf;
           Result.ApplyTransform;
           Result.UpdateExtension(Self);
           TmpTransf := MultiplyTransform3D(TmpTransf,
              Translate3D(XAx.X * ColSpace, XAx.Y * ColSpace, XAx.Z * ColSpace));
         end;
        TmpTransf := MultiplyTransform3D(TmpTransf,
           Translate3D(YAx.X * RowSpace, YAx.Y * RowSpace, YAx.Z * RowSpace));
      end;
   end;
end;

function TDXF3DImport.ReadEntity(IgnoreBlock: Boolean): TObject3D;
var
  Entry: TGroupTable;
  NLayer: Integer;
begin
  Result := nil;
  FDXFRead.ReadAnEntry(0, {%H-}Entry);
  NLayer := FLayerList.IndexOf(Entry[8]);
  if NLayer > -1 then
   FCADCmp3D.CurrentLayer := NLayer;
  if Entry[0] = 'LINE' then
   Result := ReadLine3D(Entry)
  else if Entry[0] = 'ELLIPSE' then
   Result := ReadEllipse3D(Entry)
  else if Entry[0] = 'CIRCLE' then
   Result := ReadCircle3D(Entry)
  else if Entry[0] = 'ARC' then
   Result := ReadArc3D(Entry) 
  else if Entry[0] = 'TEXT' then
   Result := ReadText3D(Entry)
  else if Entry[0] = 'TRACE' then
   Result := ReadTrace3D(Entry)
  else if Entry[0] = 'POLYLINE' then
   Result := ReadPolyline3D(Entry)
  else if Entry[0] = '3DFACE' then
   Result := ReadPlanarFace3D(Entry)
  else if (not IgnoreBlock) and (Entry[0] = 'INSERT') then
   Result := ReadBlock(Entry);
end;

function TDXF3DImport.ReadSourceBlock(Entry: TGroupTable): TSourceBlock3D;
var
  BasePoint: TPoint3D;
  Tmp: TObject3D;
  TmpName: TSourceBlockName;
begin
  Result := nil;
  if (Entry[70] and $4 = $4) then
   Exit;
  BasePoint.X := Entry[10];
  BasePoint.Y := Entry[20];
  BasePoint.Z := Entry[30];
  BasePoint.W := 1.0;
  if Entry[2] = '' then
   TmpName := StringToBlockName(Format('BLOCK%d', [FCADCmp3D.SourceBlocksCount]))
  else
   TmpName := StringToBlockName(Entry[2]);
  Result := TSourceBlock3D.Create(0, TmpName, [nil]);
  while FDXFRead.GroupValue <> 'ENDBLK' do
   begin
     Tmp := ReadEntity(False);
     if Assigned(Tmp) then
      Result.Objects.Add(Tmp);
   end;
  if fAllowEmptyBlocks or (Result.Objects.Count > 0) then
   begin
     Result.Transform(Translate3D(-BasePoint.X, -BasePoint.Y, -BasePoint.Z));
     Result.ApplyTransform;
     Result.UpdateExtension(Self);
   end
  else
   begin
     Result.Free;
     Result := nil;
   end;
end;

const
 ColArray1: array[1..4] of Byte = (0, 63, 127, 191);
 ColArray2: array[1..4] of Byte = (127, 159, 191, 223);
var
 Cont: Byte;
initialization
// Settaggio colori per DXF.
  // Colori base.
  Colors[0] := RGB(255, 255, 255);
  Colors[1] := RGB(255, 0, 0);
  Colors[2] := RGB(255, 255, 0);
  Colors[3] := RGB(0, 255, 0);
  Colors[4] := RGB(0, 255, 255);
  Colors[5] := RGB(0, 0, 255);
  Colors[6] := RGB(255, 0, 255);
  Colors[7] := RGB(0, 0, 0);
  Colors[8] := RGB(134, 134, 134);
  Colors[9] := RGB(187, 187, 187);
  // Toni di grigio.
  Colors[250] := RGB(0, 0, 0);
  Colors[251] := RGB(45, 45, 45);
  Colors[252] := RGB(91, 91, 91);
  Colors[253] := RGB(137, 137, 137);
  Colors[254] := RGB(183, 183, 183);
  Colors[255] := RGB(179, 179, 179);
  // Altre tonalità
  for Cont := 1 to 4 do
   begin
     Colors[Cont * 10] := RGB(255, ColArray1[Cont], 0);
     Colors[40 + Cont * 10] := RGB(ColArray1[5 - Cont], 255, 0);
     Colors[80 + Cont * 10] := RGB(0, 255, ColArray1[Cont]);
     Colors[120 + Cont * 10] := RGB(0, ColArray1[5 - Cont], 255);
     Colors[160 + Cont * 10] := RGB(ColArray1[Cont], 0, 255);
   end;
  Colors[210] := RGB(255, 0, 255);
  Colors[220] := RGB(255, 0, 191);
  Colors[230] := RGB(255, 0, 127);
  Colors[240] := RGB(255, 0, 63);
  for Cont := 1 to 4 do
   begin
     Colors[Cont * 10 + 1] := RGB(255, ColArray2[Cont], 127);
     Colors[41 + Cont * 10] := RGB(ColArray2[5 - Cont], 255, 127);
     Colors[81 + Cont * 10] := RGB(127, 255, ColArray2[Cont]);
     Colors[121 + Cont * 10] := RGB(127, ColArray2[5 - Cont], 255);
     Colors[161 + Cont * 10] := RGB(ColArray2[Cont], 127, 255);
   end;
  Colors[211] := RGB(255, 127, 255);
  Colors[221] := RGB(255, 127, 223);
  Colors[231] := RGB(255, 127, 191);
  Colors[241] := RGB(255, 127, 159);
  // Gli altri sono tutti zero per ora.
end.

