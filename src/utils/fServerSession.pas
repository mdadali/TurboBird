unit fServerSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls,
  ibase60dyn, FBIntf;


type
  TProtocol = (ptLocal, ptTCPIP, ptXNET); // Erweiterbar bei Bedarf

  TServerSession = class
  private
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

    FConnected: Boolean;
    FServiceHandle: isc_svc_handle;
    FStatus: array[0..19] of ISC_STATUS;

    FFBVersionString: string;
    FFBVersionMajor: Integer;
    FFBVersionMinor: Integer;

    procedure SaveCredentials;
    procedure LoadCredentials;

    function  GetConnected: boolean;
    procedure SetConnected(AValue: boolean);

    function  GetProtocol: TProtocol;
    procedure SetProtocol(AValue: TProtocol);
    function  GetPort: string;
    procedure SetPort(const AValue: string);

    procedure ParseFBVersion;
    function  BuildServiceName: AnsiString;

  public
    constructor Create(const AServerName, AServerAlias, AUserName, APassword,
      ARole: string; AProtocol: TProtocol; const AClientLibraryPath, AConnfigFilePath, APort, ACharset: string);
    destructor Destroy; override;

    function  Connect: Boolean;
    procedure Disconnect;
    procedure FetchVersion;

    property ServerName: string read FServerName;
    property ServerAlias: string read FServerAlias write FServerAlias;

    property Protocol: TProtocol read FProtocol write FProtocol;
    property Port: string read GetPort write SetPort;
    property ClientLibraryPath: string read FClientLibraryPath write FClientLibraryPath;

    property ConfigFilePath: string read FConfigFilePath write FConfigFilePath;

    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Role: string read FRole write FRole;
    property Charset: string read FCharset write FCharset;

    property Connected: Boolean read GetConnected write SetConnected;

    property FBVersionString: string read FFBVersionString;
    property FBVersionMajor: Integer read FFBVersionMajor;
    property FBVersionMinor: Integer read FFBVersionMinor;
  end;

implementation

{ TServerSession }

constructor TServerSession.Create(const AServerName, AServerAlias, AUserName, APassword,
  ARole: string; AProtocol: TProtocol; const AClientLibraryPath, AConnfigFilePath, APort, ACharset: string);
begin
  inherited Create;
  FServerName        := AServerName;
  FServerAlias       := AServerAlias;
  FUserName          := AUserName;
  FPassword          := APassword;
  FRole              := ARole;
  FProtocol          := AProtocol;
  FClientLibraryPath := AClientLibraryPath;
  FPort              := APort;
  FCharset           := ACharset;

  FConnected         := False;
  FServiceHandle     := 0;
end;

destructor TServerSession.Destroy;
begin
  Disconnect;
  inherited;
end;

function TServerSession.GetConnected: boolean;
begin
  Result := FConnected;
end;

procedure TServerSession.SetConnected(AValue: boolean);
begin
  if AValue and (not FConnected) then
    Connect
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
  if FConnected then
    raise Exception.Create('Port cannot be changed while connected.');
  FPort := AValue;
end;

function TServerSession.BuildServiceName: AnsiString;
begin
  case FProtocol of
    ptLocal:
      Result := 'service_mgr';

    ptTCPIP:
      begin
        if FPort <> '' then
          Result := AnsiString(Format('%s/%s:service_mgr', [FServerName, FPort]))
        else
          Result := AnsiString(Format('%s:service_mgr', [FServerName]));
      end;

    ptXNET:
      Result := AnsiString(Format('\\%s\service_mgr', [FServerName]));
  else
    Result := 'service_mgr';
  end;
end;

function TServerSession.Connect: Boolean;
var
  spb: array[0..255] of Byte;
  spb_len: SmallInt;
  svc_name: AnsiString;
  unameLen, pwdLen: Integer;
begin
  Result := False;
  if FConnected then
    Exit(True);

  FillChar(spb, SizeOf(spb), 0);

  unameLen := Length(FUserName);
  pwdLen := Length(FPassword);

  if (4 + unameLen + 2 + pwdLen) > Length(spb) then
    Exit(False);

  spb[0] := isc_spb_version;
  spb[1] := isc_spb_current_version;

  if unameLen > 0 then
  begin
    spb[2] := isc_spb_user_name;
    spb[3] := Byte(unameLen);
    Move(PAnsiChar(AnsiString(FUserName))^, spb[4], unameLen);
  end
  else
    Exit(False);

  spb[4 + unameLen] := isc_spb_password;
  spb[5 + unameLen] := Byte(pwdLen);
  if pwdLen > 0 then
    Move(PAnsiChar(AnsiString(FPassword))^, spb[6 + unameLen], pwdLen);

  spb_len := 6 + unameLen + pwdLen;
  svc_name := BuildServiceName;

  if isc_service_attach(@FStatus, Length(svc_name), PAnsiChar(svc_name),
    @FServiceHandle, spb_len, @spb) = 0 then
  begin
    FConnected := True;
    try
      FetchVersion;
    except
    end;
    Result := True;
  end
  else
  begin
    FServiceHandle := 0;
    Result := False;
  end;
end;

procedure TServerSession.Disconnect;
begin
  if FConnected and (FServiceHandle <> 0) then
  begin
    isc_service_detach(@FStatus, @FServiceHandle);
    FServiceHandle := 0;
    FConnected := False;
  end;
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

