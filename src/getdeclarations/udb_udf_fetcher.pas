unit udb_udf_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB,
  udb_firebird_struct_helper;

function GetUDFFunctionHeader(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
function GetUDFFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;

implementation

function GetUDFFunctionHeader(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
var
  QFunc, QArgs: TSQLQuery;
  Args: TStringList;
  ArgTypeStr, ArgName, RetStr, EntryPoint, ModuleName: string;
  FieldType, SubType, Precision, Scale, Length, CharsetId: Integer;
  FullName: string;
begin
  Args := TStringList.Create;
  QFunc := TSQLQuery.Create(nil);
  QArgs := TSQLQuery.Create(nil);
  try
    QFunc.DataBase := Conn;
    QArgs.DataBase := Conn;

    // Funktionsname inkl. optionalem Paketnamen
    if APackageName <> '' then
      FullName := FunctionName //Format('%s.%s', [APackageName, FunctionName])
    else
      FullName := FunctionName;

    // Funktion aus RDB$FUNCTIONS holen
    QFunc.SQL.Text :=
      'SELECT TRIM(RDB$FUNCTION_NAME), TRIM(RDB$ENTRYPOINT), TRIM(RDB$MODULE_NAME) ' +
      'FROM RDB$FUNCTIONS ' +
      'WHERE UPPER(TRIM(RDB$FUNCTION_NAME)) = :NAME ';
    if APackageName <> '' then
      QFunc.SQL.Text := QFunc.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      QFunc.SQL.Text := QFunc.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL';

    QFunc.Params.ParamByName('NAME').AsString := UpperCase(FunctionName);
    if APackageName <> '' then
      QFunc.Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    QFunc.Open;

    if QFunc.EOF then
      Exit('-- Funktion ' + FullName + ' nicht gefunden.');

    EntryPoint := QFunc.Fields[1].AsString;
    ModuleName := QFunc.Fields[2].AsString;

    // Parameter lesen
    QArgs.SQL.Text :=
      'SELECT RDB$ARGUMENT_POSITION, RDB$ARGUMENT_NAME, RDB$FIELD_TYPE, RDB$FIELD_SUB_TYPE, ' +
      'RDB$FIELD_LENGTH, RDB$FIELD_PRECISION, RDB$FIELD_SCALE, RDB$FIELD_SOURCE, RDB$CHARACTER_SET_ID ' +
      'FROM RDB$FUNCTION_ARGUMENTS ' +
      'WHERE UPPER(TRIM(RDB$FUNCTION_NAME)) = :NAME ';
    if APackageName <> '' then
      QArgs.SQL.Text := QArgs.SQL.Text + 'AND UPPER(RDB$PACKAGE_NAME) = :PKG '
    else
      QArgs.SQL.Text := QArgs.SQL.Text + 'AND RDB$PACKAGE_NAME IS NULL ';
    QArgs.SQL.Text := QArgs.SQL.Text + 'ORDER BY RDB$ARGUMENT_POSITION';

    QArgs.Params.ParamByName('NAME').AsString := UpperCase(FunctionName);
    if APackageName <> '' then
      QArgs.Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    QArgs.Open;

    RetStr := 'UNKNOWN';
    Args.Clear;

    while not QArgs.EOF do
    begin
      ArgName := Trim(QArgs.FieldByName('RDB$ARGUMENT_NAME').AsString);

      FieldType  := QArgs.FieldByName('RDB$FIELD_TYPE').AsInteger;
      SubType    := QArgs.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      Length     := QArgs.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      Precision  := QArgs.FieldByName('RDB$FIELD_PRECISION').AsInteger;
      Scale      := QArgs.FieldByName('RDB$FIELD_SCALE').AsInteger;
      CharsetId  := QArgs.FieldByName('RDB$CHARACTER_SET_ID').AsInteger;

      if FieldType = 0 then
        ArgTypeStr := FieldSourceToStr(Trim(QArgs.FieldByName('RDB$FIELD_SOURCE').AsString), Conn)
      else
        ArgTypeStr := FieldTypeToStr(FieldType, SubType, Precision, Scale, Length, CharsetId, Conn);

      if QArgs.FieldByName('RDB$ARGUMENT_POSITION').AsInteger = 0 then
        RetStr := ArgTypeStr
      else
        Args.Add(ArgTypeStr); // UDFs brauchen keine Namen
      QArgs.Next;
    end;

    Result := Format(
      'DECLARE EXTERNAL FUNCTION %s(%s)%sRETURNS %s%sENTRY_POINT ''%s''%sMODULE_NAME ''%s'';',
      [
        FullName,
        LineEnding + '  ' + StringReplace(Args.Text.Trim, LineEnding, ',' + LineEnding + '  ', [rfReplaceAll]),
        LineEnding,
        RetStr,
        LineEnding,
        EntryPoint,
        LineEnding,
        ModuleName
      ]);
  finally
    Args.Free;
    QFunc.Free;
    QArgs.Free;
  end;
end;

function GetUDFFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; APackageName: string): string;
var
  FullName: string;
begin
  if APackageName <> '' then
    FullName := Format('%s.%s', [APackageName, FunctionName])
  else
    FullName := FunctionName;

  Result :=
    Format('-- DROP EXTERNAL FUNCTION %s;%s', [FullName, LineEnding]) +
    GetUDFFunctionHeader(Conn, FunctionName, APackageName);
end;

end.

