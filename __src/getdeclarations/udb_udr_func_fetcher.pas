unit udb_udr_func_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB,
  udb_firebird_struct_helper;

function GetUDRFunctionHeader(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
function GetUDRFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;

implementation

function GetUDRFunctionHeader(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
var
  Q: TSQLQuery;
  Args: TStringList;
  ArgName, SourceName, ArgStr, RetStr: string;
  FullName: string;
begin
  Args := TStringList.Create;
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Conn;
    Q.SQL.Text :=
      'SELECT RDB$ARGUMENT_NAME, RDB$FIELD_SOURCE, RDB$ARGUMENT_POSITION ' +
      'FROM RDB$FUNCTION_ARGUMENTS ' +
      'WHERE UPPER(RDB$FUNCTION_NAME) = :FUNC ';

    if APackageName <> '' then
      Q.SQL.Text := Q.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      Q.SQL.Text := Q.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL ';

    Q.SQL.Text := Q.SQL.Text + 'ORDER BY RDB$ARGUMENT_POSITION NULLS FIRST';

    Q.Params.ParamByName('FUNC').AsString := UpperCase(FunctionName);
    if APackageName <> '' then
      Q.Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    Q.Open;

    RetStr := 'UNKNOWN';

    while not Q.EOF do
    begin
      ArgName := Trim(Q.FieldByName('RDB$ARGUMENT_NAME').AsString);
      SourceName := Trim(Q.FieldByName('RDB$FIELD_SOURCE').AsString);

      if (Q.FieldByName('RDB$ARGUMENT_NAME').IsNull) and
         (Q.FieldByName('RDB$ARGUMENT_POSITION').AsInteger = 0) then
      begin
        RetStr := FieldSourceToStr(SourceName, Conn);
      end
      else
      begin
        if ArgName = '' then
          ArgStr := FieldSourceToStr(SourceName, Conn)
        else
          ArgStr := Format('%s %s', [ArgName, FieldSourceToStr(SourceName, Conn)]);

        Args.Add(ArgStr);
      end;

      Q.Next;
    end;

    if APackageName = '' then
      FullName := FunctionName
    else
      FullName := FunctionName; //Format('%s.%s', [APackageName, FunctionName]);

    Result := Format('CREATE OR ALTER FUNCTION %s(%s)%sRETURNS %s',
      [FullName,
       LineEnding + '  ' + StringReplace(Trim(Args.Text), LineEnding, ',' + LineEnding + '  ', [rfReplaceAll]),
       LineEnding,
       RetStr]);
  finally
    Args.Free;
    Q.Free;
  end;
end;

function GetEntryPoint(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Conn;
    Q.SQL.Text := 'SELECT RDB$ENTRYPOINT FROM RDB$FUNCTIONS WHERE UPPER(RDB$FUNCTION_NAME) = :FUNC ';

    if APackageName <> '' then
      Q.SQL.Text := Q.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      Q.SQL.Text := Q.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL ';

    Q.Params.ParamByName('FUNC').AsString := UpperCase(FunctionName);
    if APackageName <> '' then
      Q.Params.ParamByName('PKG').AsString := UpperCase(APackageName);

    Q.Open;
    Result := Trim(Q.FieldByName('RDB$ENTRYPOINT').AsString);
  finally
    Q.Free;
  end;
end;

function GetUDRFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
var
  FullName: string;
begin
  if APackageName = '' then
    FullName := FunctionName
  else
    FullName := Format('%s.%s', [APackageName, FunctionName]);

  Result :=
    'SET TERM ^;' + LineEnding + LineEnding +
    Format('-- DROP FUNCTION %s;%s', [FullName, LineEnding]) +
    GetUDRFunctionHeader(Conn, FunctionName, APackageName) + LineEnding +
    Format('EXTERNAL NAME ''%s''%sENGINE UDR^', [GetEntryPoint(Conn, FunctionName, APackageName), LineEnding]) + LineEnding +
    'SET TERM ;^';
end;

end.

