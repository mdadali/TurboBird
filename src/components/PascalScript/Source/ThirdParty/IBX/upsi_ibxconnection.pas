unit uPSI_IBXConnection;

interface

uses
  Classes, SysUtils,
  Controls, ExtCtrls, Graphics,
  //LCLClasses,
  IBDatabase, uPSComponent, uPSRuntime, uPSCompiler;

type
  TPSIBXConnection = class(TPersistent)
  private
    FOwner: TPersistent;

    FDatabase: TIBDatabase;
    FTransaction: TIBTransaction;
    FDatabaseName: string;
    FUserName: string;
    FPassword: string;

    procedure SetConnected(AValue: Boolean);
    function GetConnected: Boolean;

    //procedure Paint; override;
  public
    constructor Create(AOwner: TComponent);
    function GetOwner: TPersistent; override;

    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    property Database: TIBDatabase read FDatabase;
    property Transaction: TIBTransaction read FTransaction;
  published
    //property Name;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Connected: Boolean read GetConnected write SetConnected;
  end;


  TIBXConnection = class(TPanel)
    private
      FPS: TPSIBXConnection;
      function GetDatabaseName: string;
      procedure SetDatabaseName(const Value: string);

      function GetUserName: string;
      procedure SetUserName(const Value: string);

      function GetPassword: string;
      procedure SetPassword(const Value: string);

      function GetConnected: Boolean;
      procedure SetConnected(Value: Boolean);

     function GetPSIBXConnection: TPSIBXConnection;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Loaded; override;

      property PSIBXConnection: TPSIBXConnection read GetPSIBXConnection;

      procedure Connect;
      procedure Disconnect;
      procedure StartTransaction;
      procedure Commit;
      procedure Rollback;
    published
      property DatabaseName: string read GetDatabaseName write SetDatabaseName;
      property UserName: string read GetUserName write SetUserName;
      property Password: string read GetPassword write SetPassword;
      property Connected: Boolean read GetConnected write SetConnected;
    end;

procedure Register;

procedure SIRegister_TPSIBXConnection(CL: TPSPascalCompiler);
procedure RIRegister_TPSIBXConnection(CL: TPSRuntimeClassImporter);
procedure SIRegister_TIBXConnection(CL: TPSPascalCompiler);
procedure RIRegister_TIBXConnection(CL: TPSRuntimeClassImporter);

implementation

procedure TIBXConnection.Loaded;
begin
  inherited;
  // Sicherstellen, dass FPS existiert (falls aus irgendeinem Grund nicht geladen)
  if FPS = nil then
  begin
    FPS := TPSIBXConnection.Create(Self);
    //FPS.SetSubComponent(True);
  end;
end;

function TIBXConnection.GetPSIBXConnection: TPSIBXConnection;
begin
  if FPS = nil then
    FPS := TPSIBXConnection.Create(Self);
  Result := FPS;
end;

constructor TIBXConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPS := TPSIBXConnection.Create(Self);
  //FPS.SetSubComponent(True);
end;

destructor TIBXConnection.Destroy;
begin
  FPS.Free;
  inherited;
end;

function TIBXConnection.GetDatabaseName: string;
begin
  Result := FPS.DatabaseName;
end;

procedure TIBXConnection.SetDatabaseName(const Value: string);
begin
  FPS.DatabaseName := Value;
end;

function TIBXConnection.GetUserName: string;
begin
  Result := FPS.UserName;
end;

procedure TIBXConnection.SetUserName(const Value: string);
begin
  FPS.UserName := Value;
end;

function TIBXConnection.GetPassword: string;
begin
  Result := FPS.Password;
end;

procedure TIBXConnection.SetPassword(const Value: string);
begin
  FPS.Password := Value;
end;

function TIBXConnection.GetConnected: Boolean;
begin
  Result := FPS.Connected;
end;

procedure TIBXConnection.SetConnected(Value: Boolean);
begin
  FPS.Connected := Value;
end;

procedure TIBXConnection.Connect;
begin
  FPS.Connect;
end;

procedure TIBXConnection.Disconnect;
begin
  FPS.Disconnect;
end;

procedure TIBXConnection.StartTransaction;
begin
  FPS.StartTransaction;
end;

procedure TIBXConnection.Commit;
begin
  FPS.Commit;
end;

procedure TIBXConnection.Rollback;
begin
  FPS.Rollback;
end;

// gleich für UserName, Password, Connected
{ TPSIBXConnection }

{procedure TPSIBXConnection.Paint;
begin
  Width  := 150;
  Height := 30;

  //Canvas.Brush.Color := clSilver;
  //Canvas.FillRect(ClientRect);

  //Canvas.Font.Color := clBlack;
  Canvas.TextOut(5, 5, Name);
end;
}

constructor TPSIBXConnection.Create(AOwner: TComponent);
begin
  inherited Create; //(AOwner);
  FOwner := AOwner;
  //Width  := 150;
  //Height := 30;

  FDatabase := TIBDatabase.Create(nil);
  FTransaction := TIBTransaction.Create(nil);

  FDatabase.DefaultTransaction := FTransaction;
  FTransaction.DefaultDatabase := FDatabase;
end;

function TPSIBXConnection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

destructor TPSIBXConnection.Destroy;
begin
  try
    if Assigned(FDatabase) then
    begin
      if FDatabase.Connected then
        FDatabase.Connected := False;
    end;
  except
    // schlucken – Script darf hier nicht crashen
  end;

  FreeAndNil(FTransaction);
  FreeAndNil(FDatabase);

  inherited Destroy;
end;

procedure TPSIBXConnection.Connect;
begin
  if not FDatabase.Connected then
  begin
    FDatabase.DatabaseName := FDatabaseName;
    FDatabase.Params.Values['user_name'] := FUserName;
    FDatabase.Params.Values['password'] := FPassword;
    FDatabase.LoginPrompt := False;

    FDatabase.Connected := True;
  end;
end;

procedure TPSIBXConnection.Disconnect;
begin
  if FDatabase.Connected then
    FDatabase.Connected := False;
end;

procedure TPSIBXConnection.StartTransaction;
begin
  if not FTransaction.InTransaction then
    FTransaction.StartTransaction;
end;

procedure TPSIBXConnection.Commit;
begin
  if FTransaction.InTransaction then
    FTransaction.Commit;
end;

procedure TPSIBXConnection.Rollback;
begin
  if FTransaction.InTransaction then
    FTransaction.Rollback;
end;

procedure TPSIBXConnection.SetConnected(AValue: Boolean);
begin
  if AValue then
    Connect
  else
    Disconnect;
end;

function TPSIBXConnection.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

{ === PascalScript Compile-Time Registration === }

procedure SIRegister_TPSIBXConnection(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TComponent'), 'TPSIBXConnection') do
  begin
    RegisterMethod('procedure Connect');
    RegisterMethod('procedure Disconnect');
    RegisterMethod('procedure StartTransaction');
    RegisterMethod('procedure Commit');
    RegisterMethod('procedure Rollback');

    RegisterProperty('DatabaseName', 'string', iptRW);
    RegisterProperty('UserName', 'string', iptRW);
    RegisterProperty('Password', 'string', iptRW);
    RegisterProperty('Connected', 'Boolean', iptRW);
  end;
end;

{ === PascalScript Runtime Registration === }

procedure TPSIBXConnection_SetDatabaseName(Self: TPSIBXConnection; const Value: string);
begin
  Self.DatabaseName := Value;
end;

function TPSIBXConnection_GetDatabaseName(Self: TPSIBXConnection): string;
begin
  Result := Self.DatabaseName;
end;

procedure TPSIBXConnection_SetUserName(Self: TPSIBXConnection; const Value: string);
begin
  Self.UserName := Value;
end;

function TPSIBXConnection_GetUserName(Self: TPSIBXConnection): string;
begin
  Result := Self.UserName;
end;

procedure TPSIBXConnection_SetPassword(Self: TPSIBXConnection; const Value: string);
begin
  Self.Password := Value;
end;

function TPSIBXConnection_GetPassword(Self: TPSIBXConnection): string;
begin
  Result := Self.Password;
end;

procedure TPSIBXConnection_SetConnected(Self: TPSIBXConnection; const Value: Boolean);
begin
  Self.Connected := Value;
end;

function TPSIBXConnection_GetConnected(Self: TPSIBXConnection): Boolean;
begin
  Result := Self.Connected;
end;

procedure RIRegister_TPSIBXConnection(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TPSIBXConnection) do
  begin
    RegisterConstructor(@TPSIBXConnection.Create, 'Create');

    RegisterMethod(@TPSIBXConnection.Connect, 'Connect');
    RegisterMethod(@TPSIBXConnection.Disconnect, 'Disconnect');
    RegisterMethod(@TPSIBXConnection.StartTransaction, 'StartTransaction');
    RegisterMethod(@TPSIBXConnection.Commit, 'Commit');
    RegisterMethod(@TPSIBXConnection.Rollback, 'Rollback');

    RegisterPropertyHelper(@TPSIBXConnection_GetDatabaseName, @TPSIBXConnection_SetDatabaseName, 'DatabaseName');
    RegisterPropertyHelper(@TPSIBXConnection_GetUserName, @TPSIBXConnection_SetUserName, 'UserName');
    RegisterPropertyHelper(@TPSIBXConnection_GetPassword, @TPSIBXConnection_SetPassword, 'Password');
    RegisterPropertyHelper(@TPSIBXConnection_GetConnected, @TPSIBXConnection_SetConnected, 'Connected');
  end;
end;

procedure SIRegister_TIBXConnection(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TControl'), 'TIBXConnection') do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent)');
    RegisterMethod('procedure Connect');
    RegisterMethod('procedure Disconnect');
    RegisterMethod('procedure StartTransaction');
    RegisterMethod('procedure Commit');
    RegisterMethod('procedure Rollback');

    RegisterProperty('PSIBXConnection', 'TPSIBXConnection', iptRW);
    RegisterProperty('DatabaseName', 'string', iptRW);
    RegisterProperty('UserName', 'string', iptRW);
    RegisterProperty('Password', 'string', iptRW);
    RegisterProperty('Connected', 'Boolean', iptRW);
  end;
end;

procedure TIBXConnection_SetDatabaseName(Self: TIBXConnection; const Value: string);
begin
  Self.DatabaseName := Value;
end;

function TIBXConnection_GetDatabaseName(Self: TIBXConnection): string;
begin
  Result := Self.DatabaseName;
end;

procedure TIBXConnection_SetUserName(Self: TIBXConnection; const Value: string);
begin
  Self.UserName := Value;
end;

function TIBXConnection_GetUserName(Self: TIBXConnection): string;
begin
  Result := Self.UserName;
end;

procedure TIBXConnection_SetPassword(Self: TIBXConnection; const Value: string);
begin
  Self.Password := Value;
end;

function TIBXConnection_GetPassword(Self: TIBXConnection): string;
begin
  Result := Self.Password;
end;

procedure TIBXConnection_SetConnected(Self: TIBXConnection; const Value: Boolean);
begin
  Self.Connected := Value;
end;

function TIBXConnection_GetConnected(Self: TIBXConnection): Boolean;
begin
  Result := Self.Connected;
end;

procedure RIRegister_TIBXConnection(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TIBXConnection) do
  begin
    RegisterConstructor(@TIBXConnection.Create, 'Create');

    RegisterMethod(@TIBXConnection.Connect, 'Connect');
    RegisterMethod(@TIBXConnection.Disconnect, 'Disconnect');
    RegisterMethod(@TIBXConnection.StartTransaction, 'StartTransaction');
    RegisterMethod(@TIBXConnection.Commit, 'Commit');
    RegisterMethod(@TIBXConnection.Rollback, 'Rollback');

    RegisterPropertyHelper(@TIBXConnection_GetDatabaseName, @TIBXConnection_SetDatabaseName, 'DatabaseName');
    RegisterPropertyHelper(@TIBXConnection_GetUserName, @TIBXConnection_SetUserName, 'UserName');
    RegisterPropertyHelper(@TIBXConnection_GetPassword, @TIBXConnection_SetPassword, 'Password');
    RegisterPropertyHelper(@TIBXConnection_GetConnected, @TIBXConnection_SetConnected, 'Connected');
  end;
end;

{ === Lazarus Component Registration === }

procedure Register;
begin
  //RegisterComponents('PascalScript', [TPSIBXConnection]);
end;

end.
