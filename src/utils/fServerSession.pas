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

  //ibase60;
  ibase60dyn,
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
  //TProtocol = (ptTCPIP, ptEmbedded, ptXNET); // Erweiterbar bei Bedarf

  ISC_STATUS_ARRAY = array[0..19] of ISC_STATUS;

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
    FClientLibraryPath: string;

    FConfigFilePath: string;
    FLoadRegisteredClientLib: boolean;

    FIsEmbedded: boolean;

    FConnectTimeoutMS: LongInt;
    FRetryCount: SmallInt;
    FQueryTimeoutMS: LongInt;

    FConnected: Boolean;
    FServiceHandle: isc_svc_handle;
    FStatus: ISC_STATUS_ARRAY;

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
                             AClientLibraryPath,
                             AConfigFilePath: string;
                             VersionMinor,
                             VersionMajor: word;
                             ALoadRegisteredClientLib,
                             AIsEmbedded: boolean;
                             AConnectTimeoutMS: LongInt;
                             ARetryCount: SmallInt;
                             AQueryTimeoutMS: LongInt);

    destructor Destroy; override;

    function GetErrorText: string;

    function  AnyClientLibraryLoaded: Boolean;
    function  UnloadClientLibrary: boolean;
    function  LoadAnyClientLibrary: Boolean;
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

function  TServerSession.AnyClientLibraryLoaded: Boolean;
begin
  result := (IBaseLibraryHandle <> 0);
end;

function  TServerSession.UnloadClientLibrary: boolean;
begin
  Disconnect;
  if IBaseLibraryHandle <> 0 then
    ReleaseIbase60;
  result := (IBaseLibraryHandle = 0);
end;

function TServerSession.LoadAnyClientLibrary: Boolean;
var LibHandle: TLibHandle;
begin
  Result := False;

  //ReleaseIbase60;
  //InitialiseIbase60;
  try
    // 1) Schon eine Library geladen? → egal welche, einfach True
    if IBaseLibraryHandle <> 0 then
    begin
      Exit(True);
    end;

    // 2) Keine Lib geladen → nur wenn Pfad gesetzt + existiert, versuchen zu laden
    if (Trim(FClientLibraryPath) = '') or (not FileExists(FClientLibraryPath)) then
      Exit(False);

    LibHandle := InitialiseIBase60(FClientLibraryPath);
    Result := (LibHandle > 0);
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

function TServerSession.LoadRegisteredClientLibrary: Boolean;
var
  LibHandle: Int64;
  LoadedLibName: string;
begin
  Result := False;
  LibHandle := 0;
  try
    // 1) Schon eine Library geladen?
    if IBaseLibraryHandle <> 0 then
    begin
      // Falls es die gleiche ist → alles ok
      LoadedLibName := Trim(IBaseLoadedLibrary);
      FClientLibraryPath := Trim(FClientLibraryPath);
      if LoadedLibName = FClientLibraryPath then
        Exit(True)
      else
      begin
        // Andere Lib → alte freigeben
        ReleaseIbase60;
        // Vor dem Laden prüfen, ob Datei existiert
        if (Trim(FClientLibraryPath) = '') or (not FileExists(FClientLibraryPath)) then
          Exit(False);

        LibHandle := InitialiseIBase60(FClientLibraryPath);
        Exit(LibHandle > 0);
      end;
    end;

    // 2) Bisher keine Library geladen → prüfen & laden
    if (Trim(FClientLibraryPath) = '') or (not FileExists(FClientLibraryPath)) then
      Exit(False);

    LibHandle := InitialiseIBase60(FClientLibraryPath);
    Result := (LibHandle > 0);

  except
    on E: Exception do
    begin
      // Optional: Exception weiterreichen oder nur False liefern
      Result := False;
    end;
  end;
end;

function GetFirebirdErrorText(const Status: ISC_STATUS_ARRAY): string;
var
  fbErrMsg: array[0..511] of AnsiChar;
  pStatus: PISC_STATUS;
begin
  Result := '';
  pStatus := @Status[0];
  while (isc_interprete(@fbErrMsg[0], @pStatus) > 0) do
    Result := Result + string(fbErrMsg) + sLineBreak;
end;

{function TServerSession.IBXConnect: boolean;
var ServerErrorCode: TServerErrorCode;
  IntPort: word;
begin
  result := false;
  Disconnect;

  ServerErrorCode := seNone;

  FillIBXServicesConnectionData;
  IntPort := StrToIntDef(FPort, 0);

  if self.FProtocol <> Local then
    ServerErrorCode :=  tb_netutils.CheckServerStatus(FServerName, StrToIntDef(FPort, 0), FUserName, FPassword, FConnectTimeoutMS, FErrorStr);

  //FErrorCode := ServerErrorCode;

  if ServerErrorCode = seNone then
      result := ConnectFirebirdService(FServerName,
                                   IntPort,
                                   FUserName,
                                   FPassword,
                                   FProtocol,
                                   IBXServicesConnection,
                                   FFBVersionMajor,
                                   FFBVersionMinor,
                                   FFBVersionString,
                                   FErrorStr);

  if result then
  begin

  end else
  begin

  end;
  FConnected := result;


 end; }

 function TServerSession.IBXConnect: boolean;
 begin
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
var
  Status: ISC_STATUS_ARRAY; //TISCStatusVector;
  DBHandle: TISC_DB_HANDLE;
  DPB: IDPB;
  DPBLen: LongWord;
begin
  Result := False;
end;

 function TServerSession.ConnectEmbeddedIBX: boolean;
 var
   TempDBFile: string;
   IBLDBSupport1: TIBLocalDBSupport;
   FB: IFirebirdLibrary;
   FBAPI: IFirebirdAPI;
   ImplementationVersion: string;
 begin
   Result := False;

   TempDBFile := ExtractFilePath(Application.ExeName) + TmpDir + '/embedded_test.fdb';
   if  FileExists(TempDBFile) then
      DeleteFile(PChar(TempDBFile));

   try
     FB := LoadFBLibrary(FClientLibraryPath);
     FBAPI := FB.GetFirebirdAPI;
     ImplementationVersion := FBAPI.GetImplementationVersion;
     FFBVersionMajor := StrToIntDef(ImplementationVersion[1], -1);
     FFBVersionMinor := StrToIntDef(ImplementationVersion[3], -1);
     FErrorStr :=  'Embedded Connection Succesful';
     FErrorCode := seNone;
     result := true;
     FConnected := Result;
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
var
  spb: array[0..255] of Byte;
  spb_len: SmallInt;
  svc_name: AnsiString;
  unameLen, pwdLen: Integer;
  Info: string;
  Status: TServerErrorCode;
  Attempt: Integer;
  fbErrMsg: array[0..255] of AnsiChar;
begin
  {Result := IBXConnect;
  self.FConnected := Result;
  Exit;}

  Result := False;
  FConnected := False;
  FErrorCode := seNone;
  FErrorStr := '';

  // Kein Connect bei Embedded
  if FProtocol = Local then
  begin
    FErrorCode := seAttachFailed;
    FErrorStr := 'Embedded mode: Service manager not available.';
    Exit(false);
  end;

  // Falls schon verbunden → fertig
  if FConnected then
    Exit(true);

  // Falls Client-Library erforderlich ist
  if FLoadRegisteredClientLib then
  begin
    if not LoadRegisteredClientLibrary then
    begin
      FErrorCode := seAttachFailed;
      FErrorStr := 'Failed to load registered Firebird client library.';
      Exit(False);
    end;
  end;

  // Verbindungsversuche mit Retry-Logik
  if not FIsEmbedded then
  for Attempt := 0 to FRetryCount -1  do
  begin
    Status := CheckServerStatus(FServerName, StrToIntDef(FPort, DEFAULT_FIREBIRD_PORT),
                                FUserName, FPassword, FConnectTimeoutMS, Info);

    FErrorCode := Status;
    FErrorStr := Info;

    if Status = seNone then
      Break
    else
    begin
      // Bei Timeout oder Refused ggf. warten und erneut versuchen
      if (Attempt < FRetryCount) then
        Sleep(500); // kleine Pause zwischen Retries (halbe Sekunde)
    end;
  end;

  // Wenn nach allen Versuchen noch kein Erfolg → abbrechen
  if FErrorCode <> seNone then
  begin
    FConnected := False;
    Exit(False);
  end;

  // SPB (Service Parameter Buffer) vorbereiten
  FillChar(spb, SizeOf(spb), 0);

  unameLen := Length(FUserName);
  pwdLen := Length(FPassword);

  if (4 + unameLen + 2 + pwdLen) > Length(spb) then
  begin
    FErrorCode := seUnknown;
    FErrorStr := 'Username/password too long.';
    Exit(False);
  end;

  spb[0] := isc_spb_version;
  spb[1] := isc_spb_current_version;

  if unameLen > 0 then
  begin
    spb[2] := isc_spb_user_name;
    spb[3] := Byte(unameLen);
    Move(PAnsiChar(AnsiString(FUserName))^, spb[4], unameLen);
  end
  else
  begin
    FErrorCode := seAuthenticationFailed;
    FErrorStr := 'Username is empty.';
    Exit(False);
  end;

  spb[4 + unameLen] := isc_spb_password;
  spb[5 + unameLen] := Byte(pwdLen);
  if pwdLen > 0 then
    Move(PAnsiChar(AnsiString(FPassword))^, spb[6 + unameLen], pwdLen);

  spb_len := 6 + unameLen + pwdLen;

  if FIsEmbedded then
    svc_name := '' //'service_mgr'
  else
    svc_name := BuildServiceName;

  // Firebird-Service verbinden
  if isc_service_attach(@FStatus, Length(svc_name), PAnsiChar(svc_name),
                        @FServiceHandle, spb_len, @spb) = 0 then
  begin
    FConnected := True;
    FErrorCode := seNone;
    FErrorStr := Format('Connected successfully to %s:%s.', [FServerName, FPort]);

    try
      FetchVersion;
    except
      // Version optional ignorieren
    end;

    Result := True;
  end
  else
  begin
    //isc_interprete(@fbErrMsg, @FStatus);
    FConnected := False;
    FErrorCode := seAttachFailed;
    //FErrorStr := Format('Failed to attach Firebird service %s:%s. %s',
                        //[FServerName, FPort, fbErrMsg]);
    FErrorStr := GetFirebirdErrorText(FStatus);
    Result := False;
  end;
end;

function TServerSession.ReConnect(AClientLibPath: string): Boolean;
begin
  Result := False;

  // 1) Falls verbunden, zuerst trennen
  if FConnected then
    Disconnect;

  // 2) Wenn ein neuer Client-Pfad übergeben wurde, diesen ggf. übernehmen
  if Trim(AClientLibPath) <> '' then
  begin
    // Nur neu setzen, wenn unterschiedlich
    if not SameText(FClientLibraryPath, AClientLibPath) then
    begin
      // Versuchen, alte Library zu entladen
      UnloadClientLibrary;
      FClientLibraryPath := AClientLibPath;

      // Neu laden
      if FLoadRegisteredClientLib then
      begin
        if not LoadRegisteredClientLibrary then
          Exit(False);
      end
      else
      begin
        if not LoadAnyClientLibrary then
          Exit(False);
      end;
    end;
  end
  else
  begin
    // Kein Pfad übergeben → prüfen, ob Library geladen ist
    if not AnyClientLibraryLoaded then
    begin
      if FLoadRegisteredClientLib then
      begin
        if not LoadRegisteredClientLibrary then
          Exit(False);
      end
      else
      begin
        if not LoadAnyClientLibrary then
          Exit(False);
      end;
    end;
  end;

  // 3) Jetzt erneut verbinden
  Result := IBXConnect;
end;


procedure TServerSession.Disconnect;
begin
  if IBXServicesConnection.Connected then
    IBXServicesConnection.Connected := false;

  FConnected := False;
  FServiceHandle := 0;

  {if FConnected and (FServiceHandle <> 0) then
  begin
    isc_service_detach(@FStatus, @FServiceHandle);
    FServiceHandle := 0;
    FConnected := False;
  end;}
end;

procedure TServerSession.FetchEmbeddedVersion;
var tmpDbInfo: TIBDatabaseInfo;
begin
  tmpDbInfo := TIBDatabaseInfo.Create(nil);
  tmpDbInfo.Database := TempEmbeddedDB;
  FFBVersionString := tmpDbInfo.FirebirdVersion;
  ParseFBVersion;
  tmpDbInfo.Free;
end;

procedure TServerSession.FetchVersion;
var
  request, buffer: array[0..511] of Byte;
  item_len: Word;
  p: PByte;
begin
  if not FConnected then Exit;

  request[0] := isc_info_svc_server_version;
  request[1] := isc_info_end;

  FillChar(buffer, SizeOf(buffer), 0);

  if isc_service_query(@FStatus, @FServiceHandle,
    nil, 0, nil, 2, @request, SizeOf(buffer), @buffer) <> 0 then
  begin
    raise Exception.Create('Service query failed: ' + IntToStr(FStatus[1]));
  end;

  p := @buffer[0];
  while (p^ <> isc_info_end) and (p^ <> 0) do
  begin
    case p^ of
      isc_info_svc_server_version:
        begin
          Inc(p);
          Move(p^, item_len, 2);
          Inc(p, 2);
          SetString(FFBVersionString, PAnsiChar(p), item_len);
          Inc(p, item_len);
        end;
    else
      Break;
    end;
  end;

  ParseFBVersion;
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

