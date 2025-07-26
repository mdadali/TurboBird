unit udb_firebird_proc_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB,
  udb_firebird_struct_helper;

function GetFirebirdProcedureHeader(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
function GetFirebirdProcedureBody(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
function GetFirebirdProcedureDeclaration(Conn: TIBConnection; const ProcName: string; APackageName: string): string;

implementation

function CleanProcedureBody(const Body: string): string;
var
  Lines: TStringList;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Body;
    for i := Lines.Count - 1 downto 0 do
      if Trim(Lines[i]) = '' then
        Lines.Delete(i);
    for i := 0 to Lines.Count - 1 do
      Lines[i] := '  ' + Lines[i];
    Result := 'AS' + LineEnding + Lines.Text;
  finally
    Lines.Free;
  end;
end;

function FormatParamList(List: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
  begin
    if i < List.Count - 1 then
      Result := Result + '  ' + List[i] + ',' + LineEnding
    else
      Result := Result + '  ' + List[i] + LineEnding;
  end;
end;

function GetFirebirdProcedureHeader(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  Q: TSQLQuery;
  Args, RetArgs: TStringList;
  ArgName, SourceName, ArgStr: string;
  IsOutput: Boolean;
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
      IsOutput := Q.FieldByName('RDB$PARAMETER_TYPE').AsInteger = 1;

      if ArgName = '' then
        ArgStr := FieldSourceToStr(SourceName, Conn)
      else
        ArgStr := Format('%s %s', [ArgName, FieldSourceToStr(SourceName, Conn)]);

      if IsOutput then
        RetArgs.Add(ArgStr)
      else
        Args.Add(ArgStr);

      Q.Next;
    end;

    if APackageName = '' then
      FullName := ProcName
    else
      FullName := ProcName; //Format('%s.%s', [APackageName, ProcName]);

    Result := Format('CREATE OR ALTER PROCEDURE %s', [FullName]) + LineEnding;

    if Args.Count > 0 then
    begin
      Result := Result + '(' + LineEnding;
      Result := Result + FormatParamList(Args);
      Result := Result + ')' + LineEnding;
    end;

    if RetArgs.Count > 0 then
    begin
      Result := Result + 'RETURNS (' + LineEnding;
      Result := Result + FormatParamList(RetArgs);
      Result := Result + ')' + LineEnding;
    end;

  finally
    Args.Free;
    RetArgs.Free;
    Q.Free;
  end;
end;

function GetFirebirdProcedureBody(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  Q: TSQLQuery;
  ProcBody: string;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Conn;
    Q.SQL.Text :=
      'SELECT RDB$PROCEDURE_SOURCE FROM RDB$PROCEDURES ' +
      'WHERE RDB$ENGINE_NAME IS NULL AND UPPER(RDB$PROCEDURE_NAME) = :PROC ';

    if APackageName <> '' then
      Q.SQL.Text := Q.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      Q.SQL.Text := Q.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL ';

    Q.Params.ParamByName('PROC').AsString := UpperCase(ProcName);
    if APackageName <> '' then
      Q.Params.ParamByName('PKG').AsString := UpperCase(APackageName);

    Q.Open;

    ProcBody := Trim(Q.FieldByName('RDB$PROCEDURE_SOURCE').AsString);
    if ProcBody = '' then
      Result := 'AS' + LineEnding + 'BEGIN' + LineEnding + '  -- Original Procedure has no body!' + LineEnding + 'END'
    else
      Result := CleanProcedureBody(ProcBody);
  finally
    Q.Free;
  end;
end;

function GetFirebirdProcedureDeclaration(Conn: TIBConnection; const ProcName: string; APackageName: string): string;
var
  FullName: string;
begin
  if APackageName = '' then
    FullName := ProcName
  else
    FullName := Format('%s.%s', [APackageName, ProcName]);

  Result := 'SET TERM ^;' + LineEnding + LineEnding;
  Result := Result + Format('-- DROP PROCEDURE %s;%s%s', [FullName, LineEnding, LineEnding]);
  Result := Result + GetFirebirdProcedureHeader(Conn, ProcName, APackageName) + LineEnding;
  Result := Result + GetFirebirdProcedureBody(Conn, ProcName, APackageName) + '^' + LineEnding + LineEnding;
  Result := Result + 'SET TERM ;^' + LineEnding;
end;

end.

