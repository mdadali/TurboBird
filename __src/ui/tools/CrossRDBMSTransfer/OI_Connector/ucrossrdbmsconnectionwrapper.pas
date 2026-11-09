unit uCrossRDBMSConnectionWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,

  SQLDB, SQLDBLib, IB, IBDatabase;


type
  TConnectionWrapper = class(TPersistent)
  private
    FConn    : TSQLConnection;
    FLoader  : TSQLDBLibraryLoader;
    FIBXConn : TIBDatabase;
    FOnBeforeDisconnect: TNotifyEvent;

    { IBX-Zwischenspeicher }
    FIBXHostName: string;
    FIBXPort    : Integer;
    FIBXPath    : string;

    { SQLDB-Zwischenspeicher }
    FSQLDBPort  : Integer;

    function  IsIBX: Boolean;
    function  GetConnectorType: string;
    function  BuildIBXConnStr: string;
    procedure ApplyPortToSQLDB;

    function  GetHostName: string;
    procedure SetHostName(const AValue: string);
    function  GetPort: Integer;
    procedure SetPort(AValue: Integer);
    function  GetDatabaseName: string;
    procedure SetDatabaseName(const AValue: string);
    function  GetUserName: string;
    procedure SetUserName(const AValue: string);
    function  GetPassword: string;
    procedure SetPassword(const AValue: string);
    function  GetParams: TStrings;
    procedure SetParams(AValue: TStrings);
    function  GetConnected: Boolean;
    procedure SetConnected(AValue: Boolean);

    function  GetLibLocation: string;
    procedure SetLibLocation(const AValue: string);

    function  GetLoginPrompt: Boolean;
    procedure SetLoginPrompt(AValue: Boolean);

  public
    constructor Create(AConn: TSQLConnection; ALoader: TSQLDBLibraryLoader;
      AIBXConn: TIBDatabase = nil);

    property ActiveConnection: TSQLConnection read FConn;
    property ActiveIBXConnection: TIBDatabase read FIBXConn;
    property ActiveLoader: TSQLDBLibraryLoader read FLoader;

    property OnBeforeDisconnect: TNotifyEvent
      read FOnBeforeDisconnect write FOnBeforeDisconnect;

  published
    property Connected: Boolean read GetConnected write SetConnected;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetPort write SetPort;
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property Params: TStrings read GetParams write SetParams;

    property LibraryLocation: string read GetLibLocation write SetLibLocation;
  end;

implementation

constructor TConnectionWrapper.Create(AConn: TSQLConnection;
  ALoader: TSQLDBLibraryLoader; AIBXConn: TIBDatabase);
begin
  inherited Create;
  FConn := AConn;
  FLoader := ALoader;
  FIBXConn := AIBXConn;
  FOnBeforeDisconnect := nil;

  FIBXHostName := '';
  FIBXPort := 0;
  FIBXPath := '';
  FSQLDBPort := 0;

  if IsIBX and Assigned(FIBXConn) then
    FIBXPath := FIBXConn.DatabaseName;
end;

function TConnectionWrapper.IsIBX: Boolean;
begin
  Result := Assigned(FIBXConn);
end;

function TConnectionWrapper.GetConnectorType: string;
begin
  if FConn is TSQLConnector then
    Result := TSQLConnector(FConn).ConnectorType
  else
  begin
    Result := FConn.ClassName;
    if (Length(Result) > 1) and (Result[1] = 'T') then
      Delete(Result, 1, 1);
  end;

  if Trim(Result) = '' then
    Result := FConn.ClassName;
end;

{ ---- IBX-Connection-String bauen ---- }

function TConnectionWrapper.BuildIBXConnStr: string;
var
  HostPart, PortPart: string;
begin
  Result := '';
  if Trim(FIBXPath) = '' then
    Exit;

  HostPart := Trim(FIBXHostName);
  PortPart := '';
  if FIBXPort > 0 then
    PortPart := IntToStr(FIBXPort);

  if HostPart = '' then
    Result := FIBXPath
  else if PortPart = '' then
    Result := HostPart + ':' + FIBXPath
  else
    Result := HostPart + '/' + PortPart + ':' + FIBXPath;
end;

{ ---- SQLDB: Port je nach RDBMS anwenden ---- }

procedure TConnectionWrapper.ApplyPortToSQLDB;
var
  CT: string;
  H: string;
begin
  if FSQLDBPort <= 0 then Exit;
  if not Assigned(FConn) then Exit;

  CT := UpperCase(GetConnectorType);

  { PostgreSQL: Params 'port' }
  if Pos('POSTGRESQL', CT) > 0 then
  begin
    FConn.Params.Values['port'] := IntToStr(FSQLDBPort);
    Exit;
  end;

  { MySQL: Params 'Port' (published property greift auf denselben Wert zu) }
  if Pos('MYSQL', CT) > 0 then
  begin
    FConn.Params.Values['Port'] := IntToStr(FSQLDBPort);
    Exit;
  end;

  { Firebird (SQLDB): Port an HostName anhängen }
  if Pos('FIREBIRD', CT) > 0 then
  begin
    H := Trim(FConn.HostName);
    if (H <> '') and (Pos('/' + IntToStr(FSQLDBPort), H) = 0) then
      FConn.HostName := H + '/' + IntToStr(FSQLDBPort);
    Exit;
  end;

  { MSSQL: Port an HostName anhängen (Host,Port) }
  if Pos('MSSQL', CT) > 0 then
  begin
    H := Trim(FConn.HostName);
    if (H <> '') and (Pos(',' + IntToStr(FSQLDBPort), H) = 0) then
      FConn.HostName := H + ',' + IntToStr(FSQLDBPort);
    Exit;
  end;

  { Oracle: Port an HostName anhängen (Host:Port) }
  if Pos('ORACLE', CT) > 0 then
  begin
    H := Trim(FConn.HostName);
    if (H <> '') and (Pos(':' + IntToStr(FSQLDBPort), H) = 0) then
      FConn.HostName := H + ':' + IntToStr(FSQLDBPort);
    Exit;
  end;

  { Sybase: Params 'Port' }
  if Pos('SYBASE', CT) > 0 then
  begin
    FConn.Params.Values['Port'] := IntToStr(FSQLDBPort);
    Exit;
  end;

  { ODBC und Unknown: ignorieren — Port steckt im DSN }
end;

{ ---- HostName ---- }

function TConnectionWrapper.GetHostName: string;
begin
  if IsIBX then
    Result := FIBXHostName
  else if Assigned(FConn) then
    Result := FConn.HostName
  else
    Result := '';
end;

procedure TConnectionWrapper.SetHostName(const AValue: string);
begin
  if IsIBX then
    FIBXHostName := Trim(AValue)
  else if Assigned(FConn) then
    FConn.HostName := AValue;
end;

{ ---- Port ---- }

function TConnectionWrapper.GetPort: Integer;
begin
  if IsIBX then
    Result := FIBXPort
  else
    Result := FSQLDBPort;
end;

procedure TConnectionWrapper.SetPort(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if IsIBX then
    FIBXPort := AValue
  else
    FSQLDBPort := AValue;
end;

{ ---- DatabaseName ---- }

function TConnectionWrapper.GetDatabaseName: string;
begin
  if IsIBX then
    Result := FIBXPath
  else if Assigned(FConn) then
    Result := FConn.DatabaseName
  else
    Result := '';
end;

procedure TConnectionWrapper.SetDatabaseName(const AValue: string);
begin
  if IsIBX then
    FIBXPath := Trim(AValue)
  else if Assigned(FConn) then
    FConn.DatabaseName := AValue;
end;

{ ---- UserName / Password ---- }

function TConnectionWrapper.GetUserName: string;
begin
  if IsIBX then
    Result := FIBXConn.Params.Values['user_name']
  else if Assigned(FConn) then
    Result := FConn.UserName
  else
    Result := '';
end;

procedure TConnectionWrapper.SetUserName(const AValue: string);
begin
  if IsIBX then
    FIBXConn.Params.Values['user_name'] := AValue
  else if Assigned(FConn) then
    FConn.UserName := AValue;
end;

function TConnectionWrapper.GetPassword: string;
begin
  if IsIBX then
    Result := FIBXConn.Params.Values['password']
  else if Assigned(FConn) then
    Result := FConn.Password
  else
    Result := '';
end;

procedure TConnectionWrapper.SetPassword(const AValue: string);
begin
  if IsIBX then
    FIBXConn.Params.Values['password'] := AValue
  else if Assigned(FConn) then
    FConn.Password := AValue;
end;

{ ---- Params ---- }

function TConnectionWrapper.GetParams: TStrings;
begin
  if IsIBX then
    Result := FIBXConn.Params
  else if Assigned(FConn) then
    Result := FConn.Params
  else
    Result := nil;
end;

procedure TConnectionWrapper.SetParams(AValue: TStrings);
begin
  if not Assigned(AValue) then Exit;

  if IsIBX then
    FIBXConn.Params.Assign(AValue)
  else if Assigned(FConn) then
    FConn.Params.Assign(AValue);
end;

{ ---- Connected ---- }

function TConnectionWrapper.GetConnected: Boolean;
begin
  if IsIBX then
    Result := FIBXConn.Connected
  else if Assigned(FConn) then
    Result := FConn.Connected
  else
    Result := False;
end;

procedure TConnectionWrapper.SetConnected(AValue: Boolean);
begin
  if IsIBX then
  begin
    if FIBXConn.Connected = AValue then Exit;
    if AValue then
    begin
      FIBXConn.DatabaseName := BuildIBXConnStr;
      FIBXConn.Open;
    end
    else
    begin
      if Assigned(FOnBeforeDisconnect) then
      try FOnBeforeDisconnect(Self); except end;
      FIBXConn.Close;
    end;
  end
  else
  begin
    if not Assigned(FConn) then Exit;
    if FConn.Connected = AValue then Exit;

    try
      if AValue then
      begin
        { Port je nach RDBMS anwenden, BEVOR verbunden wird }
        ApplyPortToSQLDB;

        if Assigned(FLoader) and (Trim(FLoader.ConnectionType) <> '') and
           (not FLoader.Enabled) then
          FLoader.Enabled := True;
        FConn.Open;
      end
      else
      begin
        if Assigned(FOnBeforeDisconnect) then
        try FOnBeforeDisconnect(Self); except end;
        FConn.Close;
        if Assigned(FLoader) then
          FLoader.Enabled := False;
      end;
    except
      raise;
    end;
  end;
end;

{ ---- LibraryLocation ---- }
function TConnectionWrapper.GetLibLocation: string;
begin
  if IsIBX then
  begin
    { IBX: Pfad zur Client-Bibliothek aus FirebirdLibraryPathName }
    if Assigned(FIBXConn) then
      Result := FIBXConn.FirebirdLibraryPathName
    else
      Result := '';
  end
  else if Assigned(FLoader) then
    Result := FLoader.LibraryName
  else
    Result := '';
end;

procedure TConnectionWrapper.SetLibLocation(const AValue: string);
begin
  if IsIBX then
  begin
    { IBX: FirebirdLibraryPathName darf nur gesetzt werden,
      solange die Datenbank NICHT verbunden ist. }
    if not Assigned(FIBXConn) then
      raise Exception.Create(
        'Cannot set Firebird library path: IBX database is not assigned.');

    if FIBXConn.Connected then
      raise Exception.Create(
        'Cannot change the Firebird client library path while the ' +
        'connection is active.' + sLineBreak + sLineBreak +
        'Please disconnect first (Connected := False), change the ' +
        'library path, then reconnect.');

    FIBXConn.FirebirdLibraryPathName := Trim(AValue);
  end
  else if Assigned(FLoader) then
    FLoader.LibraryName := AValue;
end;

function TConnectionWrapper.GetLoginPrompt: Boolean;
begin
  if IsIBX then
  begin
    if Assigned(FIBXConn) then
      Result := FIBXConn.LoginPrompt
    else
      Result := False;
  end
  else if Assigned(FConn) then
    Result := FConn.LoginPrompt
  else
    Result := False;
end;

procedure TConnectionWrapper.SetLoginPrompt(AValue: Boolean);
begin
  if IsIBX then
  begin
    if Assigned(FIBXConn) then
      FIBXConn.LoginPrompt := AValue;
  end
  else if Assigned(FConn) then
    FConn.LoginPrompt := AValue;
end;

end.
