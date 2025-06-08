unit udb_udr_proc_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB,
  udb_firebird_struct_helper;

function GetUDRProcedureHeader(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
function GetUDRProcedureDeclaration(Conn: TIBConnection; const ProcName: string; APackageName: string): string;

implementation

function GetUDRProcedureHeader(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  Q: TSQLQuery;
  Args, RetArgs: TStringList;
  ArgName, SourceName, ParamLine: string;
  IsReturning: Boolean;
  i: Integer;
  FullName: string;
begin
  Args := TStringList.Create;
  RetArgs := TStringList.Create;
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Conn;
    Q.SQL.Text :=
      'SELECT RDB$PARAMETER_NAME, RDB$FIELD_SOURCE, RDB$PARAMETER_TYPE ' +
      'FROM RDB$PROCEDURE_PARAMETERS ' +
      'WHERE UPPER(RDB$PROCEDURE_NAME) = :PROC ';

    if APackageName <> '' then
      Q.SQL.Text := Q.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      Q.SQL.Text := Q.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL ';

    Q.SQL.Text := Q.SQL.Text + 'ORDER BY RDB$PARAMETER_TYPE, RDB$PARAMETER_NUMBER';

    Q.Params.ParamByName('PROC').AsString := UpperCase(ProcName);
    if APackageName <> '' then
      Q.Params.ParamByName('PKG').AsString := UpperCase(APackageName);

    Q.Open;

    while not Q.EOF do
    begin
      ArgName := Trim(Q.FieldByName('RDB$PARAMETER_NAME').AsString);
      SourceName := Trim(Q.FieldByName('RDB$FIELD_SOURCE').AsString);
      IsReturning := Q.FieldByName('RDB$PARAMETER_TYPE').AsInteger = 1;

      if ArgName = '' then
        ParamLine := FieldSourceToStr(SourceName, Conn)
      else
        ParamLine := Format('%s %s', [ArgName, FieldSourceToStr(SourceName, Conn)]);

      if IsReturning then
        RetArgs.Add(ParamLine)
      else
        Args.Add(ParamLine);

      Q.Next;
    end;

    if APackageName <> '' then
      FullName := ProcName //Format('%s.%s', [APackageName, ProcName])
    else
      FullName := ProcName;

    Result := Format('CREATE OR ALTER PROCEDURE %s(', [FullName]) + LineEnding;
    for i := 0 to Args.Count - 1 do
    begin
      Result := Result + '  ' + Args[i];
      if i < Args.Count - 1 then
        Result := Result + ',';
      Result := Result + LineEnding;
    end;
    Result := Result + ')' + LineEnding;

    if RetArgs.Count > 0 then
    begin
      Result := Result + 'RETURNS (' + LineEnding;
      for i := 0 to RetArgs.Count - 1 do
      begin
        Result := Result + '  ' + RetArgs[i];
        if i < RetArgs.Count - 1 then
          Result := Result + ',';
        Result := Result + LineEnding;
      end;
      Result := Result + ')' + LineEnding;
    end;
  finally
    Args.Free;
    RetArgs.Free;
    Q.Free;
  end;
end;

function GetEntryPoint(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Conn;
    Q.SQL.Text :=
      'SELECT RDB$ENTRYPOINT FROM RDB$PROCEDURES ' +
      'WHERE UPPER(RDB$PROCEDURE_NAME) = :PROC ';

    if APackageName <> '' then
      Q.SQL.Text := Q.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      Q.SQL.Text := Q.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL ';

    Q.Params.ParamByName('PROC').AsString := UpperCase(ProcName);
    if APackageName <> '' then
      Q.Params.ParamByName('PKG').AsString := UpperCase(APackageName);

    Q.Open;
    Result := Trim(Q.FieldByName('RDB$ENTRYPOINT').AsString);
  finally
    Q.Free;
  end;
end;

function GetUDRProcedureDeclaration(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  FullName: string;
begin
  if APackageName <> '' then
    FullName := Format('%s.%s', [APackageName, ProcName])
  else
    FullName := ProcName;

  Result :=
    'SET TERM ^;' + LineEnding + LineEnding +
    Format('-- DROP PROCEDURE %s;%s', [FullName, LineEnding]) +
    GetUDRProcedureHeader(Conn, ProcName, APackageName) +
    Format('EXTERNAL NAME ''%s''%sENGINE UDR;%s^%s', [
      GetEntryPoint(Conn, ProcName, APackageName),
      LineEnding,
      LineEnding,
      LineEnding
    ]) +
    'SET TERM ;^';
end;

end.

