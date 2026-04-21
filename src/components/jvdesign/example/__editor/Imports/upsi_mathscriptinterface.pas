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

function ArcTanHelper(X: Extended): Extended;
begin
  Result := ArcTan(X);
end;

procedure SIRegister_MathScriptInterface(CL: TPSPascalCompiler);
begin
  CL.AddDelphiFunction('function Tan(X: Extended): Extended');
  CL.AddDelphiFunction('function ArcSin(X: Extended): Extended');
  CL.AddDelphiFunction('function ArcCos(X: Extended): Extended');
  //CL.AddDelphiFunction('function ArcTan(X: ValReal): ValReal');
  CL.AddDelphiFunction('function ArcTanHelper(X: Extended): Extended');
  CL.AddDelphiFunction('function ArcTan2(Y, X: Extended): Extended');

  CL.AddDelphiFunction('function IntPower(Base: Extended; Exponent: Integer): Extended');
  CL.AddDelphiFunction('function Ldexp(X: Extended; Exponent: Integer): Extended');
  CL.AddDelphiFunction('function LnXp1(X: Extended): Extended');
  CL.AddDelphiFunction('function Log10(X: Extended): Extended');
  CL.AddDelphiFunction('function Log2(X: Extended): Extended');
  CL.AddDelphiFunction('function LogN(Base: Extended; X: Extended): Extended');
  CL.AddDelphiFunction('function Power(Base: Extended; Exponent: Extended): Extended');

  CL.AddDelphiFunction('function DegToRad(Degrees: Extended): Extended');
  CL.AddDelphiFunction('function RadToDeg(Radians: Extended): Extended');

  CL.AddDelphiFunction('function Factorial(X: Integer): Integer');
  CL.AddDelphiFunction('function RoundTo(Value: Extended; Decimals: Integer): Extended');
  CL.AddDelphiFunction('function RandomRange(Min, Max: Integer): Integer');
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
end;

end.

