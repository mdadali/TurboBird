{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit csvBase; 

interface

uses
  jvCSVBase, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('jvCSVBase', @jvCSVBase.Register); 
end; 

initialization
  RegisterPackage('csvBase', @Register); 
end.
