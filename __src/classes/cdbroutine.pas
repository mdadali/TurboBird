unit cDBRoutine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, cDBObject;

type
  TDBRoutine = class(TDBObject)
  public
    constructor Create(AConn: TSQLConnection; const AName: string; ADBObjectType: TDBObjectType); override;

    // Abstrakte Methoden zur Umsetzung in den Unterklassen
    function GetHeader: string; virtual; abstract;
    function GetBody: string; virtual; abstract;
    function GetDeclaration: string; virtual; abstract;
  end;

implementation

{ TDBRoutine }

constructor TDBRoutine.Create(AConn: TSQLConnection; const AName: string; ADBObjectType: TDBObjectType);
begin
  inherited Create(AConn, AName, ADBObjectType);
end;

end.

