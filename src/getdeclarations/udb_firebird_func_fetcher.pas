unit udb_firebird_func_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB, udb_firebird_struct_helper;

function GetFirebirdFunctionHeader(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
function GetFirebirdFunctionBody(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
function GetFirebirdFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;

implementation

function GetFirebirdFunctionHeader(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
var
  Q: TSQLQuery;
  Args: TStringList;
  ArgName, SourceName, ArgStr, RetStr: string;
  PosIndex: Integer;
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
      PosIndex := Q.FieldByName('RDB$ARGUMENT_POSITION').AsInteger;

      if (ArgName = '') and (PosIndex = 0) then
        RetStr := FieldSourceToStr(SourceName, Conn)
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

    // CREATE FUNCTION or CREATE FUNCTION <package>.<func>
    if APackageName = '' then
      Result := Format('CREATE OR ALTER FUNCTION %s(%s)%sRETURNS %s',
        [FunctionName,
         LineEnding + '  ' + StringReplace(Trim(Args.Text), LineEnding, ',' + LineEnding + '  ', [rfReplaceAll]),
         LineEnding,
         RetStr])
    else
      {Result := Format('CREATE OR ALTER FUNCTION %s.%s(%s)%sRETURNS %s',
        [APackageName, FunctionName,
         LineEnding + '  ' + StringReplace(Trim(Args.Text), LineEnding, ',' + LineEnding + '  ', [rfReplaceAll]),
         LineEnding,
         RetStr]);}  //Package.functionname
      Result := Format('CREATE OR ALTER FUNCTION %s(%s)%sRETURNS %s',
        [FunctionName,
        LineEnding + '  ' + StringReplace(Trim(Args.Text), LineEnding, ',' + LineEnding + '  ', [rfReplaceAll]),
        LineEnding,
        RetStr]);

  finally
    Args.Free;
    Q.Free;
  end;
end;

function GetFirebirdFunctionBody(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
var
  Q: TSQLQuery;
  BodyText: string;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Conn;

    Q.SQL.Text :=
      'SELECT RDB$FUNCTION_SOURCE FROM RDB$FUNCTIONS ' +
      'WHERE RDB$MODULE_NAME IS NULL AND RDB$ENGINE_NAME IS NULL AND UPPER(RDB$FUNCTION_NAME) = :FUNC ';

    if APackageName <> '' then
      Q.SQL.Text := Q.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      Q.SQL.Text := Q.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL ';

    Q.Params.ParamByName('FUNC').AsString := UpperCase(FunctionName);
    if APackageName <> '' then
      Q.Params.ParamByName('PKG').AsString := UpperCase(APackageName);

    Q.Open;

    BodyText := Trim(Q.FieldByName('RDB$FUNCTION_SOURCE').AsString);

    if BodyText = '' then
      BodyText := 'BEGIN' + LineEnding + '  -- Original function has no body!' + LineEnding + 'END'
    else
      BodyText := CleanupBodyText(BodyText);

    Result := 'AS' + LineEnding + BodyText;
  finally
    Q.Free;
  end;
end;

function GetFirebirdFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
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
    GetFirebirdFunctionHeader(Conn, FunctionName, APackageName) + LineEnding +
    GetFirebirdFunctionBody(Conn, FunctionName, APackageName) + '^' + LineEnding +
    'SET TERM ;^';
end;

end.

