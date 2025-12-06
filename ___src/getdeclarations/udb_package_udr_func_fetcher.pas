unit udb_package_udr_func_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetPackageUdrFunctionHeader(Conn: TIBDatabase; const FuncName: string; APackageName: string): string;
function GetPackageUdrFunctionBody(Conn: TIBDatabase; const FuncName: string; APackageName: string): string;
function GetPackageUdrFunctionDeclaration(Conn: TIBDatabase; const FuncName: string; APackageName: string): string;

implementation

function IsUdrFunction(Conn: TIBDatabase; const FuncName, PackageName: string): Boolean;
var
  qry: TIBQuery;
begin
  Result := False;
  qry := TIBQuery.Create(nil);
  qry.AllowAutoActivateTransaction := true;

  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT 1 FROM RDB$FUNCTIONS ' +
      'WHERE RDB$FUNCTION_NAME = :FN ' +
      'AND RDB$PACKAGE_NAME = :PN ' +
      'AND RDB$ENGINE_NAME IS NOT NULL';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    qry.ParamByName('FN').AsString := UpperCase(FuncName);
    qry.ParamByName('PN').AsString := UpperCase(PackageName);
    qry.Open;
    Result := not qry.EOF;
  finally
    qry.Free;
  end;
end;

function GetPackageUdrFunctionHeader(Conn: TIBDatabase; const FuncName: string; APackageName: string): string;
var
  qry: TIBQuery;
  src, uFuncName, uLine: string;
  lines: TStringList;
  i: Integer;
begin
  Result := '';
  if not IsUdrFunction(Conn, FuncName, APackageName) then
  begin
    Result := Format('-- Function "%s.%s" is not a UDR.', [APackageName, FuncName]);
    Exit;
  end;

  qry := TIBQuery.Create(nil);
  qry.AllowAutoActivateTransaction := true;

  lines := TStringList.Create;
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
      src := qry.Fields[0].AsString;
      lines.Text := src;

      // Optional: remove unwanted empty lines
      for i := lines.Count - 1 downto 0 do
      begin
        if Trim(lines[i]) = '' then
        begin
          if (i > 0) and (i < lines.Count - 1) then
          begin
            if (
                (Pos('END', UpperCase(Trim(lines[i - 1]))) = 1) or (Pos(';', Trim(lines[i - 1])) > 0)) and
               ((Pos('PROCEDURE ', UpperCase(Trim(lines[i + 1]))) = 1) or
                (Pos('FUNCTION ', UpperCase(Trim(lines[i + 1]))) = 1)) then
              Continue
            else
              lines.Delete(i);
          end;
        end;
      end;

      uFuncName := UpperCase(FuncName);
      for i := 0 to lines.Count - 1 do
      begin
        uLine := UpperCase(Trim(lines[i]));
        if (uLine.StartsWith('FUNCTION ' + uFuncName + '(') or
            uLine.StartsWith('FUNCTION ' + uFuncName + ' ')) then
        begin
          Result := Trim(lines[i]);
          Break;
        end;
      end;
    end;
  finally
    qry.Free;
    lines.Free;
  end;
end;

function GetPackageUdrFunctionBody(Conn: TIBDatabase; const FuncName: string; APackageName: string): string;
var
  qry: TIBQuery;
  src, uFuncName, line, block: string;
  lines: TStringList;
  i: Integer;
  inBlock: Boolean;
begin
  Result := '';
  if not IsUdrFunction(Conn, FuncName, APackageName) then
  begin
    Result := Format('-- Function "%s.%s" is not a UDR.', [APackageName, FuncName]);
    Exit;
  end;

  qry := TIBQuery.Create(nil);
  qry.AllowAutoActivateTransaction := true;

  lines := TStringList.Create;
  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT RDB$PACKAGE_BODY_SOURCE FROM RDB$PACKAGES ' +
      'WHERE RDB$PACKAGE_NAME = :PN';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    qry.ParamByName('PN').AsString := UpperCase(APackageName);
    qry.Open;
    if not qry.EOF and not qry.Fields[0].IsNull then
    begin
      src := qry.Fields[0].AsString;
      lines.Text := src;

      for i := lines.Count - 1 downto 0 do
      begin
        if Trim(lines[i]) = '' then
        begin
          if (i > 0) and (i < lines.Count - 1) then
          begin
            if ((Pos('END', UpperCase(Trim(lines[i - 1]))) = 1) or (Pos(';', Trim(lines[i - 1])) > 0)) and
               ((Pos('PROCEDURE ', UpperCase(Trim(lines[i + 1]))) = 1) or
                (Pos('FUNCTION ', UpperCase(Trim(lines[i + 1]))) = 1)) then
              Continue
            else
              lines.Delete(i);
          end;
        end;
      end;

      uFuncName := UpperCase(FuncName);
      inBlock := False;
      block := '';

      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);
        if not inBlock then
        begin
          if (UpperCase(line).StartsWith('FUNCTION ' + uFuncName + '(') or
              UpperCase(line).StartsWith('FUNCTION ' + uFuncName + ' ')) then
            inBlock := True;
        end
        else
        begin
          block := block + lines[i] + LineEnding;
          if line.EndsWith(';') then
          begin
            Result := Trim(block);
            Break;
          end;
        end;
      end;
    end;
  finally
    qry.Free;
    lines.Free;
  end;
end;

function GetPackageUdrFunctionDeclaration(Conn: TIBDatabase; const FuncName: string; APackageName: string): string;
var
  Header, Body, ParamAndReturn, FullName: string;
  P: Integer;
begin
  Header := GetPackageUdrFunctionHeader(Conn, FuncName, APackageName);
  Body := GetPackageUdrFunctionBody(Conn, FuncName, APackageName);

  if (Header = '') then
  begin
    Result := Format('-- UDR Function "%s.%s" not found or not valid.', [APackageName, FuncName]);
    Exit;
  end;

  P := Pos('(', Header);
  if P > 0 then
    ParamAndReturn := Copy(Header, P, MaxInt)
  else
    ParamAndReturn := '';

  FullName := Format('%s.%s', [UpperCase(APackageName), UpperCase(FuncName)]);
  Result :=
    'SET TERM ^;' + LineEnding + LineEnding +
    Format('-- DROP FUNCTION %s;%s', [FullName, LineEnding]) +
    'CREATE OR ALTER FUNCTION ' + FuncName + ParamAndReturn + LineEnding +
    Body + '^' + LineEnding +
    'SET TERM ;^';
end;

end.

