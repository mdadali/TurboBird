unit dmibx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, IBDatabase, IBQuery, IBTable, IBSQLMonitor,
  IBXServices;

type

  { TDataModuleIBX }

  TDataModuleIBX = class(TDataModule)
    IBDatabase1: TIBDatabase;
    IBQuery1: TIBQuery;
    IBTable1: TIBTable;
    IBTransaction1: TIBTransaction;
    IBXServicesConnection1: TIBXServicesConnection;
  private

  public

  end;

var
  DataModuleIBX: TDataModuleIBX;

implementation

{$R *.lfm}

{ TDataModuleIBX }


end.

