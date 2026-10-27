unit uCrossRDBMSConnectionWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLDBLib;

type
  { ------------------------------------------------------------------ }
  { Führt Properties von TSQLConnection + TSQLDBLibraryLoader          }
  { in einer Klasse zusammen, damit das TTIPropertyGrid beide          }
  { gleichzeitig anzeigen kann.                                        }
  { ------------------------------------------------------------------ }
  TConnectionWrapper = class(TPersistent)
  private
    FConn   : TSQLConnection;
    FLoader : TSQLDBLibraryLoader;

    function  GetHostName: string;
    procedure SetHostName(const AValue: string);
    function  GetDatabaseName: string;
    procedure SetDatabaseName(const AValue: string);
    function  GetUserName: string;
    procedure SetUserName(const AValue: string);
    function  GetPassword: string;
    procedure SetPassword(const AValue: string);
    function  GetConnected: Boolean;
    procedure SetConnected(AValue: Boolean);

    function  GetLibLocation: string;
    procedure SetLibLocation(const AValue: string);

  public
    constructor Create(AConn: TSQLConnection; ALoader: TSQLDBLibraryLoader);

    property ActiveConnection: TSQLConnection read FConn;
    property ActiveLoader: TSQLDBLibraryLoader read FLoader;

  published
    property Connected: Boolean read GetConnected write SetConnected;
    property HostName: string read GetHostName write SetHostName;
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;

    property LibraryLocation: string read GetLibLocation write SetLibLocation;
  end;

implementation

constructor TConnectionWrapper.Create(AConn: TSQLConnection; ALoader: TSQLDBLibraryLoader);
begin
  inherited Create;
  FConn := AConn;
  FLoader := ALoader;
end;

function TConnectionWrapper.GetHostName: string;
begin
  if Assigned(FConn) then Result := FConn.HostName else Result := '';
end;

procedure TConnectionWrapper.SetHostName(const AValue: string);
begin
  if Assigned(FConn) then FConn.HostName := AValue;
end;

function TConnectionWrapper.GetDatabaseName: string;
begin
  if Assigned(FConn) then Result := FConn.DatabaseName else Result := '';
end;

procedure TConnectionWrapper.SetDatabaseName(const AValue: string);
begin
  if Assigned(FConn) then FConn.DatabaseName := AValue;
end;

function TConnectionWrapper.GetUserName: string;
begin
  if Assigned(FConn) then Result := FConn.UserName else Result := '';
end;

procedure TConnectionWrapper.SetUserName(const AValue: string);
begin
  if Assigned(FConn) then FConn.UserName := AValue;
end;

function TConnectionWrapper.GetPassword: string;
begin
  if Assigned(FConn) then Result := FConn.Password else Result := '';
end;

procedure TConnectionWrapper.SetPassword(const AValue: string);
begin
  if Assigned(FConn) then FConn.Password := AValue;
end;

function TConnectionWrapper.GetConnected: Boolean;
begin
  if Assigned(FConn) then Result := FConn.Connected else Result := False;
end;

procedure TConnectionWrapper.SetConnected(AValue: Boolean);
begin
  if not Assigned(FConn) then Exit;
  if FConn.Connected = AValue then Exit;

  try
    if AValue then
    begin
      if Assigned(FLoader) and (Trim(FLoader.ConnectionType) <> '') and (not FLoader.Enabled) then
        FLoader.Enabled := True;
      FConn.Open;
    end
    else
    begin
      FConn.Close;
      if Assigned(FLoader) then
        FLoader.Enabled := False;
    end;
  except
    raise;
  end;
end;

function TConnectionWrapper.GetLibLocation: string;
begin
  if Assigned(FLoader) then Result := FLoader.LibraryName else Result := '';
end;

procedure TConnectionWrapper.SetLibLocation(const AValue: string);
begin
  if Assigned(FLoader) then FLoader.LibraryName := AValue;
end;

end.
