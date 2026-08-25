{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit lazrichview; 

interface

uses
  RVFreeReg, RVLazIntf, PtblRV, RichView, RVScroll, RVStyle, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('RVFreeReg', @RVFreeReg.Register); 
end; 

initialization
  RegisterPackage('lazrichview', @Register); 
end.
