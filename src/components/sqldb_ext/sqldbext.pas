{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SQLDBExt;

{$warn 5023 off : no warning about unused units}
interface

uses
  usqlqueryext, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('usqlqueryext', @usqlqueryext.Register);
end;

initialization
  RegisterPackage('SQLDBExt', @Register);
end.
