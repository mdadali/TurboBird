{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TurboDataSets;

{$warn 5023 off : no warning about unused units}
interface

uses
  uCSVFastDataset, uJSONFastDataSet, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uCSVFastDataset', @uCSVFastDataset.Register);
  RegisterUnit('uJSONFastDataSet', @uJSONFastDataSet.Register);
end;

initialization
  RegisterPackage('TurboDataSets', @Register);
end.
