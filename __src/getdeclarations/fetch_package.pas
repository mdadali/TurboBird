unit fetch_package;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetPackageHeader(Conn: TIBDatabase; APackageName: string): TStringList;
function GetPackageBody(Conn: TIBDatabase; APackageName: string): TStringList;
function GetPackageDeclaration(Conn: TIBDatabase; APackageName: string): TStringList;

implementation

function GetPackageHeader(Conn: TIBDatabase; APackageName: string): TStringList;
var
  qry: TIBQuery; i: integer;
begin
  Result := TStringList.Create;
  qry := TIBQuery.Create(nil);
  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT RDB$PACKAGE_HEADER_SOURCE FROM RDB$PACKAGES ' +
      'WHERE RDB$PACKAGE_NAME = :PN';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    qry.ParamByName('PN').AsString := UpperCase(APackageName);
    qry.Open;
    if not qry.EOF and not qry.Fields[0].IsNull then
    begin
      Result.Text := qry.Fields[0].AsString;
      for i := Result.Count - 1 downto 0 do
      begin
        if Trim(Result[i]) = '' then         //or (Pos(';', Trim(Result[i - 1])) > 0)
        begin
          if (i > 0) and (i < Result.Count - 1) then
          begin
            if (
                 (Pos('END', UpperCase(Trim(Result[i - 1]))) = 1 ) or  (Pos(';', Trim(Result[i - 1])) > 0))
               and
               ((Pos('PROCEDURE ', UpperCase(Trim(Result[i + 1]))) = 1) or
                (Pos('FUNCTION ', UpperCase(Trim(Result[i + 1]))) = 1)) then
            begin
              // Leerzeile behalten, nicht löschen
              //Continue;
              Result[i] := '----------------------------------------------------';
            end else
              Result.Delete(i);
          end;
        end;
      end;
    end;
  finally
    qry.Free;
  end;
end;

function GetPackageBody(Conn: TIBDatabase; APackageName: string): TStringList;
var
  qry: TIBQuery; i: integer;
begin
  Result := TStringList.Create;
  qry := TIBQuery.Create(nil);
  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT RDB$PACKAGE_BODY_SOURCE FROM RDB$PACKAGES ' +
      'WHERE RDB$PACKAGE_NAME = :PN';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    qry.ParamByName('PN').AsString := UpperCase(APackageName);
    qry.Open;
    {if not qry.EOF and not qry.Fields[0].IsNull then
    begin
      Result.Text := qry.Fields[0].AsString;
      for i := Result.Count - 1 downto 0 do
        if Trim(Result[i]) = '' then
          Result.Delete(i);
    end;}
    if not qry.EOF and not qry.Fields[0].IsNull then
    begin
      Result.Text := qry.Fields[0].AsString;
      for i := Result.Count - 1 downto 0 do
      begin
        if Trim(Result[i]) = '' then         //or (Pos(';', Trim(Result[i - 1])) > 0)
        begin
          if (i > 0) and (i < Result.Count - 1) then
          begin
            if (
                 (Pos('END', UpperCase(Trim(Result[i - 1]))) = 1 ) or  (Pos(';', Trim(Result[i - 1])) > 0))
               and
               ((Pos('PROCEDURE ', UpperCase(Trim(Result[i + 1]))) = 1) or
                (Pos('FUNCTION ', UpperCase(Trim(Result[i + 1]))) = 1)) then
            begin
              // Leerzeile behalten, nicht löschen
              //Continue;
              Result[i] := '----------------------------------------------------';
            end else
              Result.Delete(i);
          end;
        end;
      end;
    end;
  finally
    qry.Free;
  end;
end;

function GetPackageDeclaration(Conn: TIBDatabase; APackageName: string): TStringList;
var
  Header, Body: TStringList;
begin
  Result := TStringList.Create;
  Header := GetPackageHeader(Conn, APackageName);
  Body := GetPackageBody(Conn, APackageName);

  if (Header.Count = 0) and (Body.Count = 0) then
  begin
    Result.Add(Format('-- Package "%s" not found or is empty.', [APackageName]));
    Header.Free;
    Body.Free;
    Exit;
  end;

  if Header.Count > 0 then
  begin
    Result.Add('/*');
    Result.Add('Package ' + APackageName + ' Header');
    Result.Add('*/');
    Result.Add('SET TERM ^;');
    Result.Add('');
    Result.Add(Format('-- DROP PACKAGE %s;', [UpperCase(APackageName)]));
    Result.Add(Format('CREATE OR ALTER PACKAGE %s', [UpperCase(APackageName)]));
    Result.Add('AS');
    Result.AddStrings(Header) ;
    Result[Result.Count -1] := Result[Result.Count -1] + '^';
    //Result.Add('^');
  end;

  if Body.Count > 0 then
  begin
    Result.Add('/*');
    Result.Add('Package ' + APackageName + ' Body');
    Result.Add('*/');
    Result.Add(Format('RECREATE PACKAGE BODY %s', [UpperCase(APackageName)]));
    Result.Add('AS');
    Result.AddStrings(Body);
    Result[Result.Count -1] := Result[Result.Count -1] + '^';
    //Result.Add('^');
  end;
  Result.Add('SET TERM ;^');

  Header.Free;
  Body.Free;
end;

end.

