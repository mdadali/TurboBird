unit uPSI_MathScriptinterface;

interface

uses
  math,
  SysUtils
 ,Classes
 ,uPSComponent
 ,uPSRuntime
 ,uPSCompiler,

 CADSys4,
 CS4BaseTypes;


procedure SIRegister_MathScriptInterface(CL: TPSPascalCompiler);
procedure RIRegister_MathScriptInterface_Routines(S: TPSExec);

implementation

function Factorial(N: Integer): Integer;
begin
  if N <= 1 then
    Result := 1
  else
    Result := N * Factorial(N - 1);
end;

function AngleBetweenPoints2D(P1, P2: TPoint2D): TRealType;
begin
  Result := ArcTan2(P2.Y - P1.Y, P2.X - P1.X);
end;

procedure SwapPoints2D(var P1, P2: TPoint2D);
var
  Temp: TPoint2D;
begin
  Temp := P1;
  P1 := P2;
  P2 := Temp;
end;

function DistanceBetweenPoints2D(P1, P2: TPoint2D): TRealType;
begin
  Result := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));
end;

function MidpointBetweenPoints2D(P1, P2: TPoint2D): TPoint2D;
begin
  Result.X := (P1.X + P2.X) / 2;
  Result.Y := (P1.Y + P2.Y) / 2;
end;

function ArcTanHelper(X: Extended): Extended;
begin
  Result := ArcTan(X);
end;

procedure SIRegister_MathScriptInterface(CL: TPSPascalCompiler);
begin
  CL.AddDelphiFunction('function Tan(X: TRealType): TRealType');
  CL.AddDelphiFunction('function ArcSin(X: TRealType): TRealType');
  CL.AddDelphiFunction('function ArcCos(X: TRealType): TRealType');
  //CL.AddDelphiFunction('function ArcTan(X: ValReal): ValReal');
  CL.AddDelphiFunction('function ArcTanHelper(X: Extended): Extended');
  CL.AddDelphiFunction('function ArcTan2(Y, X: TRealType): TRealType');

  CL.AddDelphiFunction('function IntPower(Base: Extended; Exponent: Integer): Extended');
  CL.AddDelphiFunction('function Ldexp(X: Extended; Exponent: Integer): Extended');
  CL.AddDelphiFunction('function LnXp1(X: Extended): Extended');
  CL.AddDelphiFunction('function Log10(X: Extended): Extended');
  CL.AddDelphiFunction('function Log2(X: Extended): Extended');
  CL.AddDelphiFunction('function LogN(Base: Extended; X: Extended): Extended');
  CL.AddDelphiFunction('function Power(Base: Extended; Exponent: Extended): Extended');

  CL.AddDelphiFunction('function DegToRad(Degrees: TRealType): TRealType');
  CL.AddDelphiFunction('function RadToDeg(Radians: TRealType): TRealType');

  CL.AddDelphiFunction('function Factorial(X: Integer): Integer');
  CL.AddDelphiFunction('function RoundTo(Value: TRealType; Decimals: Integer): TRealType');
  CL.AddDelphiFunction('function RandomRange(Min, Max: Integer): Integer');

  CL.AddDelphiFunction('function AngleBetweenPoints2D(P1, P2: TPoint2D): TRealType');
  CL.AddDelphiFunction('procedure SwapPoints2D(var P1, P2: TPoint2D)');
  CL.AddDelphiFunction('function DistanceBetweenPoints2D(P1, P2: TPoint2D): TRealType');
  CL.AddDelphiFunction('function MidpointBetweenPoints2D(P1, P2: TPoint2D): TPoint2D');
end;

procedure RIRegister_MathScriptInterface_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@Tan, 'Tan', cdRegister);
  S.RegisterDelphiFunction(@ArcSin, 'ArcSin', cdRegister);
  S.RegisterDelphiFunction(@ArcCos, 'ArcCos', cdRegister);
  //S.RegisterDelphiFunction(@ArcTan, 'ArcTan', cdRegister);
  S.RegisterDelphiFunction(@ArcTanHelper, 'ArcTan', cdRegister);
  S.RegisterDelphiFunction(@ArcTan2, 'ArcTan2', cdRegister);
  S.RegisterDelphiFunction(@IntPower, 'IntPower', cdRegister);
  S.RegisterDelphiFunction(@Ldexp, 'Ldexp', cdRegister);
  S.RegisterDelphiFunction(@LnXp1, 'LnXp1', cdRegister);
  S.RegisterDelphiFunction(@Log10, 'Log10', cdRegister);
  S.RegisterDelphiFunction(@Log2, 'Log2', cdRegister);
  S.RegisterDelphiFunction(@LogN, 'LogN', cdRegister);
  S.RegisterDelphiFunction(@Power, 'Power', cdRegister);
  S.RegisterDelphiFunction(@DegToRad, 'DegToRad', cdRegister);
  S.RegisterDelphiFunction(@RadToDeg, 'RadToDeg', cdRegister);

  S.RegisterDelphiFunction(@Factorial, 'Factorial', cdRegister);
  S.RegisterDelphiFunction(@RoundTo, 'RoundTo', cdRegister);
  S.RegisterDelphiFunction(@RandomRange, 'RandomRange', cdRegister);

  S.RegisterDelphiFunction(@AngleBetweenPoints2D, 'AngleBetweenPoints2D', cdRegister);
  S.RegisterDelphiFunction(@SwapPoints2D, 'SwapPoints2D', cdRegister);
  S.RegisterDelphiFunction(@DistanceBetweenPoints2D, 'DistanceBetweenPoints2D', cdRegister);
  S.RegisterDelphiFunction(@MidpointBetweenPoints2D, 'MidpointBetweenPoints2D', cdRegister);
end;

end.

