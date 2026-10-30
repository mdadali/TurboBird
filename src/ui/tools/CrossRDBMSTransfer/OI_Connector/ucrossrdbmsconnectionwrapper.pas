unit uCrossRDBMSConnectionWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLDBLib, IB, IBDatabase, Dialogs;

type
  { ------------------------------------------------------------------ }
  { Führt Properties von TSQLConnection + TSQLDBLibraryLoader          }
  { (SQLDB) oder von TIBDatabase (IBX) in einer Klasse zusammen,       }
  { damit das TTIPropertyGrid sie einheitlich anzeigen kann.          }
  { ------------------------------------------------------------------ }
  TConnectionWrapper = class(TPersistent)
  private
    FConn    : TSQLConnection;
    FLoader  : TSQLDBLibraryLoader;
    FIBXConn : TIBDatabase;
    FOnBeforeDisconnect: TNotifyEvent;

    function  IsIBX: Boolean;

    function  GetHostName: string;
    procedure SetHostName(const AValue: string);
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
    property HostName: string read GetHostName write SetHostName;
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
end;

function TConnectionWrapper.IsIBX: Boolean;
begin
  Result := Assigned(FIBXConn);
end;

function TConnectionWrapper.GetHostName: string;
begin
  if IsIBX then
    Result := ''
  else if Assigned(FConn) then
    Result := FConn.HostName
  else
    Result := '';
end;

procedure TConnectionWrapper.SetHostName(const AValue: string);
begin
  if IsIBX then
    { IBX: HostName wird ignoriert — steht in DatabaseName }
  else if Assigned(FConn) then
    FConn.HostName := AValue;
end;

function TConnectionWrapper.GetDatabaseName: string;
begin
  if IsIBX then
    Result := FIBXConn.DatabaseName
  else if Assigned(FConn) then
    Result := FConn.DatabaseName
  else
    Result := '';
end;

procedure TConnectionWrapper.SetDatabaseName(const AValue: string);
begin
  if IsIBX then
    FIBXConn.DatabaseName := AValue
  else if Assigned(FConn) then
    FConn.DatabaseName := AValue;
end;

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
      FIBXConn.Open
    else
    begin
      { Vor dem Schließen aufräumen — Query schließen, Transaktion rollback }
      if Assigned(FOnBeforeDisconnect) then
      begin
        try
          FOnBeforeDisconnect(Self);
        except
        end;
      end;
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
        if Assigned(FLoader) and (Trim(FLoader.ConnectionType) <> '') and
           (not FLoader.Enabled) then
          FLoader.Enabled := True;
        FConn.Open;
      end
      else
      begin
        { Vor dem Schließen aufräumen — gilt für ALLE SQLDB-Treiber.
          Besonders wichtig für SQLite (verweigert Close bei offenen
          Statements), aber auch für PostgreSQL/MySQL/MSSQL/Oracle. }
        if Assigned(FOnBeforeDisconnect) then
        begin
          try
            FOnBeforeDisconnect(Self);
          except
          end;
        end;
        FConn.Close;
        if Assigned(FLoader) then
          FLoader.Enabled := False;
      end;
    except
      raise;
    end;
  end;
end;

function TConnectionWrapper.GetLibLocation: string;
begin
  if IsIBX then
    Result := ''
  else if Assigned(FLoader) then
    Result := FLoader.LibraryName
  else
    Result := '';
end;

procedure TConnectionWrapper.SetLibLocation(const AValue: string);
begin
  if IsIBX then
    { IBX verwaltet die Bibliothek selbst — LibraryLocation ignorieren }
  else if Assigned(FLoader) then
    FLoader.LibraryName := AValue;
end;

end.
