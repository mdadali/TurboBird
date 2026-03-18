unit udb_udf_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fbcommon,
  IB,
  IBDatabase,
  IBQuery;


function GetUDFModuleNameAndEntryPoint(Conn: TIBDatabase; const FunctionName: string): string;
function GetUDFParams(Conn: TIBDatabase; const FunctionName: string): string;
function GetUDFInputParams(Conn: TIBDatabase; const FunctionName: string): string;
function GetUDFReturnParam(Conn: TIBDatabase; const FunctionName: string): string;
function GetUDFFunctionDeclaration(Conn: TIBDatabase; const FunctionName: string): string;

function GetUDFParamBeiPosition(Conn: TIBDatabase; const FunctionName: string; APosition: word): string;

implementation

function GetUDFModuleNameAndEntryPoint(Conn: TIBDatabase; const FunctionName: string): string;
var
  SQLQuery: TIBQuery;
  ModuleName, EntryPoint: string;
begin
  Result := '';
  SQLQuery := TIBQuery.Create(nil);
  try
    SQLQuery.DataBase := Conn;
    SQLQuery.SQL.Text :=
      'SELECT RDB$MODULE_NAME, RDB$ENTRYPOINT ' +
      'FROM RDB$FUNCTIONS ' +
      'WHERE RDB$FUNCTION_NAME = :FNAME';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    SQLQuery.ParamByName('FNAME').AsString := UpperCase(FunctionName);
    SQLQuery.Open;

    if not SQLQuery.EOF then
    begin
      ModuleName := Trim(SQLQuery.FieldByName('RDB$MODULE_NAME').AsString);
      EntryPoint := Trim(SQLQuery.FieldByName('RDB$ENTRYPOINT').AsString);
      Result := ModuleName + ',' + EntryPoint;
    end;
  finally
    SQLQuery.Free;
  end;
end;

function GetUDFParams(Conn: TIBDatabase; const FunctionName: string): string;
var
  SQLQuery: TIBQuery;
begin
  Result := '';
  SQLQuery := TIBQuery.Create(nil);
  try
    SQLQuery.DataBase := Conn;
    SQLQuery.SQL.Text :=
      'SELECT RDB$ARGUMENT_POSITION, ' +
      '       CASE ' +
      '           WHEN RDB$FIELD_TYPE = 7 THEN ''SMALLINT'' ' +
      '           WHEN RDB$FIELD_TYPE = 8 THEN ''INTEGER'' ' +
      '           WHEN RDB$FIELD_TYPE = 9 THEN ''QUAD'' ' +
      '           WHEN RDB$FIELD_TYPE = 10 THEN ''FLOAT'' ' +
      '           WHEN RDB$FIELD_TYPE = 11 THEN ''D_FLOAT'' ' +
      '           WHEN RDB$FIELD_TYPE = 12 AND RDB$FIELD_SUB_TYPE = 0 THEN ''DATE'' ' +
      '           WHEN RDB$FIELD_TYPE = 12 AND RDB$FIELD_SUB_TYPE = 1 THEN ''TIME WITH TIME ZONE'' ' +
      '           WHEN RDB$FIELD_TYPE = 13 THEN ''TIME'' ' +
      '           WHEN RDB$FIELD_TYPE = 14 THEN ''CHAR('' || RDB$CHARACTER_LENGTH || '')'' ' +
      '           WHEN RDB$FIELD_TYPE = 16 AND RDB$FIELD_SUB_TYPE = 0 THEN ''BIGINT'' ' +
      '           WHEN RDB$FIELD_TYPE = 16 AND RDB$FIELD_SUB_TYPE = 1 THEN ''NUMERIC('' || RDB$FIELD_PRECISION || '','' || (0 - RDB$FIELD_SCALE) || '')'' ' +
      '           WHEN RDB$FIELD_TYPE = 16 AND RDB$FIELD_SUB_TYPE = 2 THEN ''DECIMAL('' || RDB$FIELD_PRECISION || '','' || (0 - RDB$FIELD_SCALE) || '')'' ' +
      '           WHEN RDB$FIELD_TYPE = 23 THEN ''BOOLEAN'' ' +
      '           WHEN RDB$FIELD_TYPE = 27 THEN ''DOUBLE PRECISION'' ' +
      '           WHEN RDB$FIELD_TYPE = 35 AND RDB$FIELD_SUB_TYPE = 0 THEN ''TIMESTAMP'' ' +
      '           WHEN RDB$FIELD_TYPE = 35 AND RDB$FIELD_SUB_TYPE = 1 THEN ''TIMESTAMP WITH TIME ZONE'' ' +
      '           WHEN RDB$FIELD_TYPE = 37 THEN ''VARCHAR('' || RDB$CHARACTER_LENGTH || '')'' ' +
      '           WHEN RDB$FIELD_TYPE = 40 THEN ''CSTRING('' || RDB$FIELD_LENGTH || '')'' ' +
      '           WHEN RDB$FIELD_TYPE = 45 THEN ''BLOB_ID'' ' +
      '           WHEN RDB$FIELD_TYPE = 261 THEN ''BLOB'' ' + // immer nur BLOB, Subtype egal
      '           WHEN RDB$FIELD_TYPE = 32752 THEN ''SQL_ARRAY'' ' +
      '           ELSE ''UNKNOWN_TYPE_'' || RDB$FIELD_TYPE ' +
      '       END AS ARG_TYPE ' +
      'FROM RDB$FUNCTION_ARGUMENTS ' +
      'WHERE RDB$FUNCTION_NAME = :FNAME ' +
      'ORDER BY RDB$ARGUMENT_POSITION';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;
    SQLQuery.ParamByName('FNAME').AsString := UpperCase(FunctionName);
    SQLQuery.Open;

    while not SQLQuery.EOF do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + Trim(SQLQuery.FieldByName('ARG_TYPE').AsString);
      SQLQuery.Next;
    end;
  finally
    SQLQuery.Free;
  end;
end;

function GetUDFInputParams(Conn: TIBDatabase; const FunctionName: string): string;
var
  Params: string;
  P: SizeInt;
begin
  Result := '';
  Params := GetUDFParams(Conn, FunctionName);

  if Params = '' then Exit;

  // Erstes Komma+Space suchen
  P := Pos(', ', Params);
  if P > 0 then
    Result := Trim(Copy(Params, P + 2, MaxInt)); // +2 weil ', ' zwei Zeichen sind
end;

function GetUDFReturnParam(Conn: TIBDatabase; const FunctionName: string): string;
var
  Params: string;
  P: SizeInt;
begin
  Result := '';
  Params := GetUDFParams(Conn, FunctionName);

  if Params = '' then Exit;

  // Erstes Komma+Space suchen
  P := Pos(', ', Params);
  if P > 0 then
    Result := Trim(Copy(Params, 1, P - 1))
  else
    Result := Trim(Params);  // nur ein Parameter vorhanden
end;

function GetUDFFunctionDeclaration(Conn: TIBDatabase; const FunctionName: string): string;
var
  ModAndEntry, ModuleName, EntryPoint: string;
  InputParams, ReturnParam: string;
  Params: TStringList;
  ParamLines: string;
  I: Integer;
  SepPos: Integer;
begin
  Result := '';
  ModAndEntry := GetUDFModuleNameAndEntryPoint(Conn, FunctionName);
  InputParams := Trim(GetUDFInputParams(Conn, FunctionName));
  ReturnParam := Trim(GetUDFReturnParam(Conn, FunctionName));

  // ModuleName und EntryPoint aufsplitten
  SepPos := Pos(',', ModAndEntry);
  if SepPos > 0 then
  begin
    ModuleName := Trim(Copy(ModAndEntry, 1, SepPos - 1));
    EntryPoint := Trim(Copy(ModAndEntry, SepPos + 1, MaxInt));
  end
  else
  begin
    ModuleName := '';
    EntryPoint := '';
  end;

  // Fallbacks, damit die App nicht abstürzt
  if ModuleName = '' then
    ModuleName := '<UNKNOWN_MODULE>';
  if EntryPoint = '' then
    EntryPoint := '<UNKNOWN_ENTRYPOINT>';
  if ReturnParam = '' then
    ReturnParam := 'VOID';

  // Parameter formatieren
  ParamLines := '';
  if InputParams <> '' then
  begin
    Params := TStringList.Create;
    try
      Params.StrictDelimiter := True;
      Params.Delimiter := ',';
      Params.DelimitedText := InputParams;
      for I := 0 to Params.Count - 1 do
      begin
        if I < Params.Count - 1 then
          ParamLines := ParamLines + '  ' + Trim(Params[I]) + ',' + LineEnding
        else
          ParamLines := ParamLines + '  ' + Trim(Params[I]) + LineEnding;
      end;
    finally
      Params.Free;
    end;
  end;

  // Ausgabe bauen
  Result := '-- DROP EXTERNAL FUNCTION ' + UpperCase(FunctionName) + ';' + LineEnding +
            'DECLARE EXTERNAL FUNCTION ' + UpperCase(FunctionName) + LineEnding +
            '(' + LineEnding +
            ParamLines +
            ')' + LineEnding +
            'RETURNS ' + ReturnParam + LineEnding +
            'ENTRY_POINT ''' + EntryPoint + '''' + LineEnding +
            'MODULE_NAME ''' + ModuleName + ''';' + LineEnding;
end;

function GetUDFParamBeiPosition(Conn: TIBDatabase; const FunctionName: string; APosition: word): string;
var
  Params: string;
  StartPos, EndPos, CurrPos, CommaCount: SizeInt;
begin
  Result := '';
  Params := GetUDFParams(Conn, FunctionName);
  if Params = '' then Exit;

  StartPos := 1;
  CommaCount := 0; // 0 = RETURN

  for CurrPos := 1 to Length(Params) do
  begin
    if (Params[CurrPos] = ',') and (CurrPos < Length(Params)) and (Params[CurrPos+1] = ' ') then
    begin
      if CommaCount = APosition then
      begin
        EndPos := CurrPos - 1;
        Result := Trim(Copy(Params, StartPos, EndPos - StartPos + 1));
        Exit;
      end;
      Inc(CommaCount);
      StartPos := CurrPos + 2; // +2 wegen ", "
    end;
  end;

  // Falls APosition größer als Anzahl der Parameter
  if APosition >= CommaCount then
    Result := Trim(Copy(Params, StartPos, MaxInt));
end;

end.

