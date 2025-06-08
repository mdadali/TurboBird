unit udb_package_firebird_proc_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB;

function GetPackageFirebirdProcedureHeader(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
function GetPackageFirebirdProcedureBody(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
function GetPackageFirebirdProcedureDeclaration(Conn: TIBConnection; const ProcName: string; APackageName: string): string;

implementation

function IsNativeFirebirdProcedure(Conn: TIBConnection; const ProcName, PackageName: string): Boolean;
var
  qry: TSQLQuery;
begin
  Result := False;
  qry := TSQLQuery.Create(nil);
  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT 1 FROM RDB$PROCEDURES ' +
      'WHERE RDB$PROCEDURE_NAME = :PN ' +
      'AND RDB$PACKAGE_NAME = :PKG ' +
      'AND RDB$ENGINE_NAME IS NULL ';
    qry.ParamByName('PN').AsString := UpperCase(ProcName);
    qry.ParamByName('PKG').AsString := UpperCase(PackageName);
    qry.Open;
    Result := not qry.EOF;
  finally
    qry.Free;
  end;
end;

function GetPackageFirebirdProcedureHeader(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  qry: TSQLQuery;
  src, uProcName, uLine: string;
  lines: TStringList;
  i: Integer;
begin
  Result := '';
  if not IsNativeFirebirdProcedure(Conn, ProcName, APackageName) then
  begin
    Result := Format('-- Procedure "%s.%s" is not a native PSQL package procedure.', [APackageName, ProcName]);
    Exit;
  end;

  qry := TSQLQuery.Create(nil);
  lines := TStringList.Create;
  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT RDB$PACKAGE_HEADER_SOURCE FROM RDB$PACKAGES ' +
      'WHERE RDB$PACKAGE_NAME = :PN';
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

      uProcName := UpperCase(ProcName);
      for i := 0 to lines.Count - 1 do
      begin
        uLine := UpperCase(Trim(lines[i]));
        if uLine.StartsWith('PROCEDURE ' + uProcName + '(') or
           uLine.StartsWith('PROCEDURE ' + uProcName + ' ') then
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

function GetPackageFirebirdProcedureBody(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  qry: TSQLQuery;
  src, uProcName, line, block: string;
  lines: TStringList;
  i, beginCount, endCount: Integer;
  inBlock: Boolean;
begin
  Result := '';
  if not IsNativeFirebirdProcedure(Conn, ProcName, APackageName) then
  begin
    Result := Format('-- Procedure "%s.%s" is not a native PSQL package procedure.', [APackageName, ProcName]);
    Exit;
  end;

  qry := TSQLQuery.Create(nil);
  lines := TStringList.Create;
  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT RDB$PACKAGE_BODY_SOURCE FROM RDB$PACKAGES ' +
      'WHERE RDB$PACKAGE_NAME = :PN';
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

      uProcName := UpperCase(ProcName);
      inBlock := False;
      block := '';
      beginCount := 0;
      endCount := 0;

      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);

        if not inBlock then
        begin
          if (UpperCase(line).StartsWith('PROCEDURE ' + uProcName + '(') or
              UpperCase(line).StartsWith('PROCEDURE ' + uProcName + ' ')) then
            inBlock := True;
        end
        else
        begin
          block := block + lines[i] + LineEnding;
          if UpperCase(line) = 'BEGIN' then
            Inc(beginCount)
          else if UpperCase(line) = 'END' then
            Inc(endCount);

          if (beginCount > 0) and (beginCount = endCount) then
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

function GetPackageFirebirdProcedureDeclaration(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  Header, Body, Param, FullName: string;
  P: Integer;
begin
  Header := GetPackageFirebirdProcedureHeader(Conn, ProcName, APackageName);
  Body := GetPackageFirebirdProcedureBody(Conn, ProcName, APackageName);

  if (Header = '') or (Body = '') then
  begin
    Result := Format('-- Procedure "%s.%s" not found or not valid.', [APackageName, ProcName]);
    Exit;
  end;

  P := Pos('(', Header);
  if P > 0 then
    Param := Copy(Header, P, MaxInt)
  else
    Param := '';

  FullName := Format('%s.%s', [UpperCase(APackageName), UpperCase(ProcName)]);
  Result :=
    'SET TERM ^;' + LineEnding + LineEnding +
    Format('-- DROP PROCEDURE %s;%s', [FullName, LineEnding]) +
    'CREATE OR ALTER PROCEDURE ' + ProcName + Param + LineEnding +
    Body + '^' + LineEnding +
    'SET TERM ;^';
end;

end.

