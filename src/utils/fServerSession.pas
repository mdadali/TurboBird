unit fServerSession;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ELSE}
    DynLibs,
  {$ENDIF}

  Forms, Classes, SysUtils, ComCtrls, ExtCtrls,
  Dialogs,

  //ibase60,
  //ibase60dyn,
  IBHeader,
  IB,
  FBIntf,
  IBXServices,
  IBDatabase,
  IBQuery,
  IBLocalDBSupport, IBXCustomIBLocalDBSupport,
  IBDatabaseInfo,

  fbcommon,

  tb_netutils;


type

  //ISC_STATUS_ARRAY = array[0..19] of ISC_STATUS;

  TServerSession = class
  private
    IBXServicesConnection: TIBXServicesConnection;
    FOwnerNode: TTreeNode;
    FServerName: string;
    FServerAlias: string;
    FUserName: string;
    FPassword: string;
    FRole: string;
    FProtocol: TProtocol;
    FPort: string;
    FCharset: string;
    FRootPath,
    FClientLibraryPath: string;

    FConfigFilePath: string;
    FLoadRegisteredClientLib: boolean;

    FIsEmbedded: boolean;

    FConnectTimeoutMS: LongInt;
    FRetryCount: SmallInt;
    FQueryTimeoutMS: LongInt;

    FConnected: Boolean;
    FServiceHandle: isc_svc_handle;
    //FStatus: ISC_STATUS_ARRAY;

    FFBVersionString: string;
    FFBVersionMajor: word;
    FFBVersionMinor: word;


    FErrorCode: TServerErrorCode;
    FErrorStr: string;

    TempEmbeddedDB: TIBDatabase; // for embedded connection

    procedure SaveCredentials;
    procedure LoadCredentials;

    function  GetConnected: boolean;
    procedure SetConnected(AValue: boolean);

    function  GetProtocol: TProtocol;
    procedure SetProtocol(AValue: TProtocol);
    function  GetPort: string;
    procedure SetPort(const AValue: string);
    procedure SetIsEmbedded(AValue: boolean);

    procedure ParseFBVersion;
    function  BuildServiceName: AnsiString;

    procedure IBLDBSupportOnGetDatabaseName(Sender: TObject; var DBName: string); //embedded connection
    function  CreateEmbeddedTestDB(FBAPI: IFirebirdAPI; const FileName: string): boolean;


  public
    constructor Create(const
                             AServerName,
                             AServerAlias,
                             AUserName,
                             APassword,
                             ARole: string;
                             AProtocol: TProtocol;
                             APort,
                             ACharset,
                             ARootPath,
                             AClientLibraryPath,
                             AConfigFilePath: string;
                             VersionMinor,
                             VersionMajor: word;
                             ALoadRegisteredClientLib,
                             AIsEmbedded: boolean;
                             AConnectTimeoutMS: LongInt;
                             ARetryCount: SmallInt;
                             AQueryTimeoutMS: LongInt
                             );

    destructor Destroy; override;

    function GetErrorText: string;

    function  LoadRegisteredClientLibrary: Boolean;

    function  __Connect: Boolean;
    function  ReConnect(AClientLibPath: string): Boolean;

    procedure Disconnect;

    procedure FetchVersion;
    procedure FetchEmbeddedVersion;


    procedure FillIBXServicesConnectionData;

    function IBXConnect: boolean;
    function ConnectServerIBX: boolean;
    function ConnectEmbeddedIBX: boolean;

    property ServerName: string read FServerName;
    property ServerAlias: string read FServerAlias write FServerAlias;

    property Protocol: TProtocol read FProtocol write FProtocol;
    property Port: string read GetPort write SetPort;

    property RootPath: string read FRootPath write FRootPath;
    property ClientLibraryPath: string read FClientLibraryPath write FClientLibraryPath;
    property ConfigFilePath: string read FConfigFilePath write FConfigFilePath;

    property LoadRegisteredClientLib: boolean read FLoadRegisteredClientLib write FLoadRegisteredClientLib;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Role: string read FRole write FRole;
    property Charset: string read FCharset write FCharset;

    property Connected: Boolean read GetConnected write SetConnected;

    property FBVersionString: string read FFBVersionString write FFBVersionString;
    property FBVersionMajor: word read FFBVersionMajor write FFBVersionMajor;
    property FBVersionMinor: word read FFBVersionMinor write FFBVersionMinor;

    property IsEmbedded: boolean read FIsEmbedded write SetIsEmbedded;

    property ConnectTimeout: LongInt read FConnectTimeoutMS write FConnectTimeoutMS;
    property RetryConnectCount: SmallInt read FRetryCount  write FRetryCount;
    property QueryTimeout: LongInt read FQueryTimeoutMS write FQueryTimeoutMS;

    property ErrorCode: TServerErrorCode read FErrorCode write FErrorCode;
    property ErrorStr: string read FErrorStr write FErrorStr;
  end;



{$IFDEF UNIX}
function dlerror: PChar; cdecl; external 'c';
{$ENDIF}

const
  DEFAULT_FIREBIRD_PORT = 3050;


implementation

uses turbocommon;

{ Helper functions }
function IsValidPortNr(AStr: string): boolean;
var
  PortNum: Integer;
begin
  Result := False;
  if TryStrToInt(Trim(AStr), PortNum) then
    Result := (PortNum >= 1) and (PortNum <= 65535);
end;

function ServerNameContainPortNr(AServerName: string): boolean;
var
  SlashPos: Integer;
  PortStr: string;
begin
  Result := False;
  SlashPos := Pos('/', AServerName);
  if SlashPos > 0 then
  begin
    PortStr := Copy(AServerName, SlashPos+1, MaxInt);
    Result := IsValidPortNr(PortStr);
  end;
end;

function GetPortNrFromServerName(AServerName: string): string;
var
  SlashPos: Integer;
  PortStr: string;
begin
  Result := '';
  SlashPos := Pos('/', AServerName);
  if SlashPos > 0 then
  begin
    PortStr := Copy(AServerName, SlashPos+1, MaxInt);
    if IsValidPortNr(PortStr) then
      Result := Trim(PortStr);
  end;
end;

function GetHostFromServerName(AServerName: string): string;
var
  SlashPos: Integer;
begin
  SlashPos := Pos('/', AServerName);
  if SlashPos > 0 then
    Result := Trim(Copy(AServerName, 1, SlashPos-1))
  else
    Result := Trim(AServerName);
end;

{ TServerSession }
constructor TServerSession.Create(const
                                        AServerName,
                                        AServerAlias,
                                        AUserName,
                                        APassword,
                                        ARole: string;
                                        AProtocol: TProtocol;
                                        APort,
                                        ACharset,
                                        ARootPath,
                                        AClientLibraryPath,
                                        AConfigFilePath: string;
                                        VersionMinor,
                                        VersionMajor: word;
                                        ALoadRegisteredClientLib,
                                        AIsEmbedded: boolean;
                                        AConnectTimeoutMS: LongInt;
                                        ARetryCount: SmallInt;
                                        AQueryTimeoutMS: LongInt
                                        );
begin
  inherited Create;
  FServerName          := AServerName;
  FServerAlias         := AServerAlias;
  FUserName            := AUserName;
  FPassword            := APassword;
  FRole                := ARole;
  FProtocol            := AProtocol;
  FPort                := APort;
  FCharset             := ACharset;
  FRootPath            := ARootPath;
  FClientLibraryPath   := AClientLibraryPath;
  FConfigFilePath      := AConfigFilePath;
  FBVersionMajor       := VersionMajor;
  self.FBVersionMinor  := VersionMinor;
  FLoadRegisteredClientLib := ALoadRegisteredClientLib;
  FIsEmbedded          := AIsEmbedded;

  FConnectTimeoutMS    := AConnectTimeoutMS;
  FRetryCount          := ARetryCount;
  FQueryTimeoutMS      := AQueryTimeoutMS;

  FConnected           := False;
  FServiceHandle       := 0;

  FErrorCode := seNone;
  FErrorStr := '';

  IBXServicesConnection := TIBXServicesConnection.Create(nil);
  FillIBXServicesConnectionData;
end;

destructor TServerSession.Destroy;
begin
  Disconnect;
  if IBXServicesConnection.Connected then
    IBXServicesConnection.Connected := false;
  FreeAndNil(IBXServicesConnection);
  inherited;
end;

function TServerSession.GetErrorText: string;
begin
  if FErrorCode = seNone then
    Result := 'No error'
  else
    Result := Format('%s (%s)', [ServerErrorToString(FErrorCode), FErrorStr]);
end;

function TServerSession.GetConnected: boolean;
begin
  Result := FConnected;
end;

procedure TServerSession.SetConnected(AValue: boolean);
begin
  if FProtocol = Local then
  begin
    FConnected := AValue;
    Exit;
  end;

  if AValue and (not FConnected) then
    IBXConnect
  else if (not AValue) and FConnected then
    Disconnect;
end;

function TServerSession.GetProtocol: TProtocol;
begin
  Result := FProtocol;
end;

procedure TServerSession.SetProtocol(AValue: TProtocol);
begin
  if FConnected then
    raise Exception.Create('Protocol cannot be changed while connected.');
  FProtocol := AValue;
end;

function TServerSession.GetPort: string;
begin
  Result := FPort;
end;

procedure TServerSession.SetPort(const AValue: string);
begin
  if FProtocol = Local then
  begin
    FPort := AValue;
    Exit;
  end;

  if FConnected then
    raise Exception.Create('Port cannot be changed while connected.');
  FPort := AValue;
end;

procedure TServerSession.SetIsEmbedded(AValue: boolean);
begin
  if FProtocol = Local then
  begin
    FIsEmbedded := true;
    Exit;
  end;

  if FConnected then
    raise Exception.Create('IsEmbedded cannot be changed while connected.');
  FIsEmbedded := AValue;
end;

function TServerSession.BuildServiceName: AnsiString;
begin
  case FProtocol of
    Local:
      Result := 'service_mgr';

    TCP:
      begin
        if FPort <> '' then
          Result := AnsiString(Format('%s/%s:service_mgr', [FServerName, FPort]))
        else
          Result := AnsiString(Format('%s:service_mgr', [FServerName]));
      end;

    XNET:
      Result := AnsiString(Format('\\%s\service_mgr', [FServerName]));
  else
    Result := 'service_mgr';
  end;
end;

function TServerSession.LoadRegisteredClientLibrary: Boolean;
begin
  result := IBXConnect;
end;

 function TServerSession.IBXConnect: boolean;
 begin
   if FConnected then
     Disconnect; // alte Verbindung trennen

   if FProtocol <> Local then
     Result := ConnectServerIBX
   else
     Result := ConnectEmbeddedIBX;

   FConnected := Result;
 end;

 function TServerSession.ConnectServerIBX: boolean;
 var
   ServerErrorCode: TServerErrorCode;
   IntPort: Word;
 begin
   Result := False;
   IntPort := StrToIntDef(FPort, 0);

   // Prüfen, ob Server erreichbar ist
   ServerErrorCode := tb_netutils.CheckServerStatus(
                        FServerName, IntPort,
                        FUserName, FPassword,
                        FConnectTimeoutMS, FErrorStr);

   if ServerErrorCode <> seNone then
   begin
     FErrorCode := ServerErrorCode;
     Exit;
   end;

   // IBX Verbindung aufbauen
   FillIBXServicesConnectionData;

   Result := ConnectFirebirdService(
               FServerName, IntPort,
               FUserName, FPassword,
               FProtocol,
               IBXServicesConnection,
               FFBVersionMajor,
               FFBVersionMinor,
               FFBVersionString,
               FErrorStr);

   if Result then
   begin
     ParseFBVersion;
   end
   else
     FErrorCode := seAttachFailed;
 end;


 procedure TServerSession.IBLDBSupportOnGetDatabaseName(Sender: TObject; var DBName: string);
 begin
   DBName :=  TempEmbeddedDB.DatabaseName;
 end;

function TServerSession.CreateEmbeddedTestDB(FBAPI: IFirebirdAPI; const FileName: string): boolean;
begin
  Result := False;
end;

 function TServerSession.ConnectEmbeddedIBX: boolean;
 var
   TempDBFile: string;
   IBLDBSupport1: TIBLocalDBSupport;
   FB: IFirebirdLibrary;
   FBAPI: IFirebirdAPI;
   Attachment: IAttachment;
   Version: TStringList;
   ImplementationVersion: string;
   ServerRec: TServerRecord;
   major, minor: integer;
   verstr: string;
 begin
   Result := False;
   try
     ServerRec := BuildServerRecordFromSession(Self, true);
     Result := TestEmbeddedConnection(ServerRec, major, minor, verstr);
     if result then
     begin
       FErrorStr :=  'Embedded Connection Succesful';
       FErrorCode := seNone;
       FConnected := Result;
     end;
   except
     on E: Exception do
     begin
       FErrorStr := 'Embedded connection test failed: ' + E.Message;
       FErrorCode := seAttachFailed;
       Result := False;
       FConnected := Result;
     end;
   end;

 end;


procedure TServerSession.FillIBXServicesConnectionData;
begin
  IBXServicesConnection.Connected := false;
  IBXServicesConnection.ServerName := FServerName;
  IBXServicesConnection.Protocol := FProtocol;
  IBXServicesConnection.PortNo := FPort;
  IBXServicesConnection.FirebirdLibraryPathName := FClientLibraryPath;
  IBXServicesConnection.Params.Clear;
  IBXServicesConnection.Params.Add('user_name=' + FUserName);
  IBXServicesConnection.Params.Add('password=' + FPassword);
  IBXServicesConnection.LoginPrompt := False;
end;

function TServerSession.__Connect: Boolean;
begin
  FConnected := IBXConnect;
  Result := FConnected;
end;

function TServerSession.ReConnect(AClientLibPath: string): Boolean;
begin
  Result := False;
  if FConnected then
    Disconnect;
  result := IBXConnect;
end;


procedure TServerSession.Disconnect;
begin
  if IBXServicesConnection.Connected then
    IBXServicesConnection.Connected := false;

  FConnected := False;
  FServiceHandle := 0;
end;

procedure TServerSession.FetchEmbeddedVersion;
var tmpDbInfo: TIBDatabaseInfo;
begin
  tmpDbInfo := TIBDatabaseInfo.Create(nil);
  try
    tmpDbInfo.Database := TempEmbeddedDB;
    FFBVersionString := tmpDbInfo.FirebirdVersion;
    ParseFBVersion;
  finally
    tmpDbInfo.Free;
  end;
end;

procedure TServerSession.FetchVersion;
begin
  FetchEmbeddedVersion;
{  if not FConnected then
    Exit;

  if not Assigned(IBXServicesConnection) then
    raise Exception.Create('IBX Services Connection is not initialized.');

  try
    // IBX liefert die Server-Version direkt als String
    FFBVersionString := IBXServicesConnection.ve

    // Optional: String in Major/Minor parsen
    ParseFBVersion;
  except
    on E: Exception do
    begin
      FFBVersionString := '';
      FFBVersionMajor := 0;
      FFBVersionMinor := 0;
      raise Exception.Create('Failed to fetch Firebird version via IBX: ' + E.Message);
    end;
  end;}
end;

procedure TServerSession.SaveCredentials;
begin
  // optional implementieren
end;

procedure TServerSession.LoadCredentials;
begin
  // optional implementieren
end;

procedure TServerSession.ParseFBVersion;
var
  S: string;
  DotPos: Integer;
  MajorStr, MinorStr: string;
begin
  FFBVersionMajor := 0;
  FFBVersionMinor := 0;

  S := FFBVersionString;
  if Pos('Firebird', S) > 0 then
    S := Trim(Copy(S, Pos('Firebird', S) + Length('Firebird'), MaxInt));

  DotPos := Pos('.', S);
  if DotPos > 0 then
  begin
    MajorStr := Copy(S, 1, DotPos-1);
    MinorStr := Copy(S, DotPos+1, MaxInt);
    FFBVersionMajor := StrToIntDef(MajorStr, 0);
    FFBVersionMinor := StrToIntDef(MinorStr, 0);
  end
  else
    FFBVersionMajor := StrToIntDef(S, 0);
end;

end.

