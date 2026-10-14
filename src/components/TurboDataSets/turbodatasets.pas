{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TurboDataSets;

{$warn 5023 off : no warning about unused units}
interface

uses
  c_json_dataset, uCSVFastDataset, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('c_json_dataset', @c_json_dataset.Register);
  RegisterUnit('uCSVFastDataset', @uCSVFastDataset.Register);
end;

initialization
  RegisterPackage('TurboDataSets', @Register);
end.
