{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JSONDataSet;

{$warn 5023 off : no warning about unused units}
interface

uses
  c_json_dataset, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('c_json_dataset', @c_json_dataset.Register);
end;

initialization
  RegisterPackage('JSONDataSet', @Register);
end.
