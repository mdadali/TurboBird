unit udb_package_udr_proc_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetPackageUdrProcedureHeader(Conn: TIBDatabase; const ProcName: string; APackageName: string): string;
function GetPackageUdrProcedureBody(Conn: TIBDatabase; const ProcName: string; APackageName: string): string;
function GetPackageUdrProcedureDeclaration(Conn: TIBDatabase; const ProcName: string; APackageName: string): string;

implementation

function IsUdrProcedure(Conn: TIBDatabase; const ProcName, PackageName: string): Boolean;
var
  qry: TIBQuery;
begin
  Result := False;
  qry := TIBQuery.Create(nil);
  try
    qry.DataBase := Conn;
    qry.SQL.Text :=
      'SELECT 1 FROM RDB$PROCEDURES ' +
      'WHERE RDB$PROCEDURE_NAME = :PNAME ' +
      'AND RDB$PACKAGE_NAME = :PKG ' +
      'AND RDB$ENGINE_NAME IS NOT NULL';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    qry.ParamByName('PNAME').AsString := UpperCase(ProcName);
    qry.ParamByName('PKG').AsString := UpperCase(PackageName);
    qry.Open;
    Result := not qry.EOF;
  finally
    qry.Free;
  end;
end;

function GetPackageUdrProcedureHeader(Conn: TIBDatabase; const ProcName: string; APackageName: string): string;
var
  qry: TIBQuery;
  src, uProcName, uLine: string;
  lines: TStringList;
  i: Integer;
begin
  Result := '';
  if not IsUdrProcedure(Conn, ProcName, APackageName) then
  begin
    Result := Format('-- Procedure "%s.%s" is not a UDR.', [APackageName, ProcName]);
    Exit;
  end;

  qry := TIBQuery.Create(nil);
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

      uProcName := UpperCase(ProcName);
      for i := 0 to lines.Count - 1 do
      begin
        uLine := UpperCase(Trim(lines[i]));
        if (uLine.StartsWith('PROCEDURE ' + uProcName + '(') or
            uLine.StartsWith('PROCEDURE ' + uProcName + ' ')) then
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

function GetPackageUdrProcedureBody(Conn: TIBDatabase; const ProcName: string; APackageName: string): string;
var
  qry: TIBQuery;
  src, uProcName, line, block: string;
  lines: TStringList;
  i: Integer;
  inBlock: Boolean;
begin
  Result := '';
  if not IsUdrProcedure(Conn, ProcName, APackageName) then
  begin
    Result := Format('-- Procedure "%s.%s" is not a UDR.', [APackageName, ProcName]);
    Exit;
  end;

  qry := TIBQuery.Create(nil);
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

      uProcName := UpperCase(ProcName);
      inBlock := False;
      block := '';

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

function GetPackageUdrProcedureDeclaration(Conn: TIBDatabase; const ProcName: string; APackageName: string): string;
var
  Header, Body, ParamList, FullName: string;
  P: Integer;
begin
  Header := GetPackageUdrProcedureHeader(Conn, ProcName, APackageName);
  Body := GetPackageUdrProcedureBody(Conn, ProcName, APackageName);

  if (Header = '') then
  begin
    Result := Format('-- UDR Procedure "%s.%s" not found or not valid.', [APackageName, ProcName]);
    Exit;
  end;

  P := Pos('(', Header);
  if P > 0 then
    ParamList := Copy(Header, P, MaxInt)
  else
    ParamList := '';

  FullName := Format('%s.%s', [UpperCase(APackageName), UpperCase(ProcName)]);
  Result :=
    'SET TERM ^;' + LineEnding + LineEnding +
    Format('-- DROP PROCEDURE %s;%s', [FullName, LineEnding]) +
    'CREATE OR ALTER PROCEDURE ' + ProcName + ParamList + LineEnding +
    Body + '^' + LineEnding +
    'SET TERM ;^';
end;

end.

