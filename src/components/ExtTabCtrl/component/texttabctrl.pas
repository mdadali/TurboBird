{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TExtTabCtrl;

{$warn 5023 off : no warning about unused units}
interface

uses
  ExtTabCtrlReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ExtTabCtrlReg', @ExtTabCtrlReg.Register);
end;

initialization
  RegisterPackage('TExtTabCtrl', @Register);
end.
