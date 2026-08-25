unit cadsysreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

uses
  CADSys4;

{$R cadsys.dcr}

procedure Register;
begin
  RegisterComponents('CADSys 4.2', [TRuler]);
  RegisterComponents('CADSys 4.2 2D', [TCADCmp2D, TCADViewport2D, TCADPrg2D]);
  RegisterComponents('CADSys 4.2 3D', [TCADCmp3D, TCADParallelViewport3D, TCADOrtogonalViewport3D, TCADPerspectiveViewport3D, TCADPrg3D]);
end;

end.

