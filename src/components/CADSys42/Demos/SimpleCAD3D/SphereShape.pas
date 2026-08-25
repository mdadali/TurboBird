unit SphereShape;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{: This is as example of how to define your own graphical shapes.
}

interface

uses CADSys4, CS4Shapes, CS4BaseTypes;

type
  {: A sphere is special kind of RotationalOutline3D object that
     is specialized to rotate a 3D planar ellipse.
  }
  TSphere3D = class(TRotationalOutline3D)
  private
    function GetDivisions: Integer;
    procedure SetDivisions(D: Integer);
  protected
    procedure UpdateSolid; override;
  public
    constructor Create(ID: LongInt;
                       const P1, P2: TPoint3D;
                       const Divis: Integer);

    property Divisions: Integer read GetDivisions write SetDivisions;
  end;

implementation

function MidPoint(const P1, P2: TPoint3D): TPoint3D;
begin
  Result.X := (P1.X + P2.X) / 2.0;
  Result.Y := (P1.Y + P2.Y) / 2.0;
  Result.Z := (P1.Z + P2.Z) / 2.0;
  Result.W := 1.0;
end;

function TSphere3D.GetDivisions: Integer;
begin
  Result := Trunc(TwoPi / DeltaAngle);
end;

procedure TSphere3D.SetDivisions(D: Integer);
begin
  DeltaAngle := TwoPi / D;
end;

procedure TSphere3D.UpdateSolid;
begin
  RotationCenter := MidPoint(Points[0], Points[1]);
  inherited;
end;

constructor TSphere3D.Create(ID: LongInt;
                             const P1, P2: TPoint3D;
                             const Divis: Integer);
var
  TmpEllipse: TEllipse3D;
begin
  TmpEllipse := TEllipse3D.Create(0, Point3D(0, 0, 0),
                                     Versor3D(1, 0, 0),
                                     Versor3D(0, 1, 0),
                                     P1, P2);
  inherited Create(ID, TmpEllipse, 0, Pi, TwoPi / Divis, Versor3D(1, 0, 0), MidPoint(P1, P2));
end;

initialization
  CADSysRegisterClass(105, TSphere3D);
end.
