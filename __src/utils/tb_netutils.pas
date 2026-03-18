unit tb_netutils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  IdTCPClient, IdGlobal, IdException, IdStack;

type
  TServerErrorCode = (
    seNone,
    seHostUnreachable,
    sePortClosed,
    seAuthenticationFailed,
    seAttachFailed,
    seUnknown
  );

function ServerErrorToString(Code: TServerErrorCode): string;

// Prüft den Status eines Servers via übergebenem TCP-Client.
// Host, Port, UserName und Password werden übergeben.
function CheckServerStatus(const Host: string; Port: Word;
  const UserName, Password: string; TimeoutMS: Integer; out ErrorStr: string): TServerErrorCode;

implementation

{$IFDEF UNIX}
uses
  BaseUnix;
const
  ETIMEDOUT     = 110; // Zeitüberschreitung (timeout)
  ECONNREFUSED  = 111; // Verbindung abgelehnt
  TIMEOUT_ERROR = ETIMEDOUT;
  CONNREFUSED_ERROR = ECONNREFUSED;
{$ELSE}
uses
  WinSock;
const
  TIMEOUT_ERROR = WSAETIMEDOUT;
  CONNREFUSED_ERROR = WSAECONNREFUSED;
{$ENDIF}


function CheckServerStatus(const Host: string; Port: Word;
  const UserName, Password: string; TimeoutMS: Integer; out ErrorStr: string): TServerErrorCode;
var
  Client: TIdTCPClient;
  Connected: Boolean;
  E: Exception;
begin
  Result := seUnknown;
  ErrorStr := '';
  Connected := False;

  Client := TIdTCPClient.Create(nil);
  try
    // setze Zeitlimits VOR dem Connect
    Client.Host := Host;
    Client.Port := Port;
    Client.ConnectTimeout := TimeoutMS;
    Client.ReadTimeout := TimeoutMS;

    // Versuche zu verbinden — alle Exceptions abfangen und in Result/ ErrorStr mappen
    try
      Client.Connect;
      if Client.Connected then
      begin
        Connected := True;
        Result := seNone;
        ErrorStr := Format('Connected successfully to %s:%d.', [Host, Port]);
        // optional Auth: nur senden, nicht blockierend lesen
        if (UserName <> '') or (Password <> '') then
        begin
          try
            // Nur sendende Test-Auth — auf das konkrete Protokoll anpassen, falls nötig
            Client.IOHandler.WriteLn(UserName);
            Client.IOHandler.WriteLn(Password);
            // kein ReadLn, weil ReadLn ggf. blockiert (ReadTimeout gesetzt, aber EVtl. nicht zuverlässig)
          except
            on Ex: Exception do
            begin
              // Auth-Fehler als AuthenticationFailed markieren
              Result := seAuthenticationFailed;
              ErrorStr := Format('Authentication write error: %s', [Ex.Message]);
              // Wir lassen Connected wahr oder trennen: trennen ist sicherer
              try
                if Client.Connected then Client.Disconnect;
              except end;
              Exit;
            end;
          end;
        end;

        try
          if Client.Connected then Client.Disconnect;
        except end;
        Exit;
      end;
    except
      on EIdSock: EIdSocketError do
      begin
        // EIdSocketError liefert LastError (plattformabhängig)
        case EIdSock.LastError of
          TIMEOUT_ERROR:
            begin
              Result := seHostUnreachable;
              ErrorStr := Format('Connection to %s:%d timed out after %d ms (LastError=%d).',
                [Host, Port, TimeoutMS, EIdSock.LastError]);
            end;
          CONNREFUSED_ERROR:
            begin
              Result := sePortClosed;
              ErrorStr := Format('Connection refused by %s:%d (LastError=%d).',
                [Host, Port, EIdSock.LastError]);
            end;
        else
          begin
            Result := seUnknown;
            ErrorStr := Format('Socket error %d connecting to %s:%d: %s',
              [EIdSock.LastError, Host, Port, EIdSock.Message]);
          end;
        end;
      end;
      on Ex: EIdConnClosedGracefully do
      begin
        // Verbindung wurde sauber vom Remote beendet
        Result := seHostUnreachable;
        ErrorStr := Format('Connection closed gracefully by %s:%d: %s', [Host, Port, Ex.Message]);
      end;
      on Ex: Exception do
      begin
        // Alle sonstigen Exceptions unterdrücken und als Unknown melden
        Result := seUnknown;
        ErrorStr := Format('Unexpected exception connecting to %s:%d: %s', [Host, Port, Ex.Message]);
      end;
    end;

    // Falls wir hier landen: kein Connected
    if not Connected then
    begin
      if ErrorStr = '' then
        ErrorStr := Format('No connection established to %s:%d.', [Host, Port]);
    end;
  finally
    try
      Client.Free;
    except end;
  end;
end;

function ServerErrorToString(Code: TServerErrorCode): string;
begin
  case Code of
    seNone:                Result := 'No error';
    seHostUnreachable:     Result := 'Host unreachable';
    sePortClosed:          Result := 'Port is closed or not responding';
    seAuthenticationFailed:Result := 'Authentication failed';
    seUnknown:             Result := 'Unknown error';
  else
    Result := 'Undefined error';
  end;
end;

end.

