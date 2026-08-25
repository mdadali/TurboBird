{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cadsys;

{$warn 5023 off : no warning about unused units}
interface

uses
  CADSys4, CS4BaseTypes, CS4DXFModule, CS4Shapes, CS4Tasks, cadsysreg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('cadsysreg', @cadsysreg.Register);
end;

initialization
  RegisterPackage('cadsys', @Register);
end.
