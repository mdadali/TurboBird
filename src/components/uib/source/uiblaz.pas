{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit UIBLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  registeruib, uib, uibdataset, uibavl, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registeruib', @registeruib.Register);
end;

initialization
  RegisterPackage('UIBLaz', @Register);
end.
