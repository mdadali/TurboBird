unit uCrossRDBMSTypeMapping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  uCrossRDBMSTypes;

function DBMSFromConnection(AConn: TSQLConnection): TCrossDBMS;
function DBMSName(ADBMS: TCrossDBMS): string;
function GetFieldTypeString(AField: TField): string;
function QuoteIdentifier(const AName: string; ADBMS: TCrossDBMS): string;
function GetTypeListForDBMS(ADBMS: TCrossDBMS): TStringList;
function AutoMapType(const ASourceType: string; ADestDBMS: TCrossDBMS): string;
function DetectBackendDBMS(AQuery: TSQLQuery; ATrans: TSQLTransaction): TCrossDBMS;

implementation

const
  DBMSNames: array[TCrossDBMS] of string = (
    'Unknown',
    'Firebird',
    'PostgreSQL',
    'MySQL / MariaDB',
    'SQLite',
    'Microsoft SQL Server',
    'Oracle',
    'Sybase',
    'ODBC'
  );

{ ------------------------------------------------------------------ }
{ DBMS-Erkennung                                                     }
{ ------------------------------------------------------------------ }

function DBMSFromConnection(AConn: TSQLConnection): TCrossDBMS;
var
  CT: string;
begin
  Result := cdUnknown;
  if AConn = nil then Exit;

  // Generischer TSQLConnector
  if AConn is TSQLConnector then
  begin
    CT := TSQLConnector(AConn).ConnectorType;

    if SameText(CT, 'Firebird') then
      Result := cdFirebird
    else if SameText(CT, 'SQLite3') then
      Result := cdSQLite
    else if SameText(CT, 'PostgreSQL') then
      Result := cdPostgreSQL
    else if Pos('MySQL', CT) > 0 then
      Result := cdMySQL
    else if SameText(CT, 'MSSQLServer') then
      Result := cdMSSQL
    else if SameText(CT, 'Oracle') then
      Result := cdOracle
    else if SameText(CT, 'Sybase') then
      Result := cdSybase
    else if SameText(CT, 'ODBC') then
      Result := cdODBC;

    Exit;
  end;

  // Spezifische Connector-Klassen (Fallback)
  if AConn.ClassNameIs('TIBConnection') then
    Result := cdFirebird
  else if AConn.ClassNameIs('TSQLite3Connection') then
    Result := cdSQLite
  else if AConn.ClassNameIs('TPQConnection') then
    Result := cdPostgreSQL
  else if (Pos('MySQL', AConn.ClassName) > 0) then
    Result := cdMySQL
  else if AConn.ClassNameIs('TMSSQLConnection') then
    Result := cdMSSQL
  else if AConn.ClassNameIs('TOracleConnection') then
    Result := cdOracle
  else if AConn.ClassNameIs('TSybaseConnection') then
    Result := cdSybase
  else if AConn.ClassNameIs('TODBCConnection') then
    Result := cdODBC;
end;

function DBMSName(ADBMS: TCrossDBMS): string;
begin
  Result := DBMSNames[ADBMS];
end;

{ ------------------------------------------------------------------ }
{ Feldtyp als String                                                 }
{ ------------------------------------------------------------------ }

function GetFieldTypeString(AField: TField): string;
begin
  if AField = nil then
    Exit('VARCHAR(255)');

  case AField.DataType of
    ftString:       Result := 'VARCHAR(' + IntToStr(AField.Size) + ')';
    ftFixedChar:    Result := 'CHAR(' + IntToStr(AField.Size) + ')';
    ftWideString:   Result := 'VARCHAR(' + IntToStr(AField.Size) + ')';
    ftMemo, ftWideMemo: Result := 'TEXT';
    ftSmallint:     Result := 'SMALLINT';
    ftInteger:      Result := 'INTEGER';
    ftLargeint, ftAutoInc: Result := 'BIGINT';
    ftWord:         Result := 'SMALLINT';
    ftFloat:        Result := 'DOUBLE PRECISION';
    ftCurrency:     Result := 'NUMERIC(18,4)';
    ftBCD, ftFMTBcd: Result := 'NUMERIC(18,4)';
    ftBoolean:      Result := 'BOOLEAN';
    ftDate:         Result := 'DATE';
    ftTime:         Result := 'TIME';
    ftDateTime:     Result := 'TIMESTAMP';
    ftTimeStamp:    Result := 'TIMESTAMP';
    ftBlob:         Result := 'BLOB';
    ftBytes, ftVarBytes: Result := 'BLOB';
    ftGraphic:      Result := 'BLOB';
  else
    Result := 'VARCHAR(255)';
  end;
end;

{ ------------------------------------------------------------------ }
{ Identifier quoten                                                  }
{ ------------------------------------------------------------------ }

function QuoteIdentifier(const AName: string; ADBMS: TCrossDBMS): string;
var
  Clean: string;
begin
  Clean := Trim(AName);
  if Clean = '' then
    Exit('');

  // Bereits gequotet?
  if (Length(Clean) >= 2) and
     (((Clean[1] = '"') and (Clean[Length(Clean)] = '"')) or
      ((Clean[1] = '`') and (Clean[Length(Clean)] = '`')) or
      ((Clean[1] = '[') and (Clean[Length(Clean)] = ']'))) then
    Exit(Clean);

  case ADBMS of
    cdMySQL:
      Result := '`' + StringReplace(Clean, '`', '``', [rfReplaceAll]) + '`';
    cdMSSQL, cdSybase:
      Result := '[' + StringReplace(Clean, ']', ']]', [rfReplaceAll]) + ']';
  else
    // Firebird, PostgreSQL, SQLite, Oracle, ODBC, Unknown → ANSI
    Result := '"' + StringReplace(Clean, '"', '""', [rfReplaceAll]) + '"';
  end;
end;

{ ------------------------------------------------------------------ }
{ Typ-Listen pro RDBMS (für ComboBox / PickList)                     }
{ ------------------------------------------------------------------ }

function GetTypeListForDBMS(ADBMS: TCrossDBMS): TStringList;
begin
  Result := TStringList.Create;

  case ADBMS of
    cdFirebird:
      begin
        Result.Add('SMALLINT'); Result.Add('INTEGER'); Result.Add('BIGINT');
        Result.Add('INT128');
        Result.Add('NUMERIC(18,2)'); Result.Add('NUMERIC(18,4)');
        Result.Add('DECIMAL(18,2)');
        Result.Add('FLOAT'); Result.Add('DOUBLE PRECISION');
        Result.Add('DECFLOAT(16)'); Result.Add('DECFLOAT(34)');
        Result.Add('CHAR(1)'); Result.Add('CHAR(10)');
        Result.Add('VARCHAR(50)'); Result.Add('VARCHAR(255)'); Result.Add('VARCHAR(1000)');
        Result.Add('BLOB SUB_TYPE TEXT'); Result.Add('BLOB SUB_TYPE BINARY');
        Result.Add('DATE'); Result.Add('TIME'); Result.Add('TIMESTAMP');
        Result.Add('TIME WITH TIME ZONE');
        Result.Add('TIMESTAMP WITH TIME ZONE');
        Result.Add('BOOLEAN');
      end;

    cdPostgreSQL:
      begin
        Result.Add('SMALLINT'); Result.Add('INTEGER'); Result.Add('BIGINT');
        Result.Add('NUMERIC(18,2)'); Result.Add('NUMERIC(18,4)');
        Result.Add('DECIMAL(18,2)');
        Result.Add('REAL'); Result.Add('DOUBLE PRECISION');
        Result.Add('CHAR(1)'); Result.Add('CHAR(10)');
        Result.Add('VARCHAR(50)'); Result.Add('VARCHAR(255)'); Result.Add('VARCHAR(1000)');
        Result.Add('TEXT'); Result.Add('BYTEA');
        Result.Add('DATE'); Result.Add('TIME'); Result.Add('TIMESTAMP');
        Result.Add('TIMESTAMPTZ');
        Result.Add('BOOLEAN');
        Result.Add('UUID'); Result.Add('JSON'); Result.Add('JSONB');
      end;

    cdMySQL:
      begin
        Result.Add('TINYINT'); Result.Add('SMALLINT'); Result.Add('INT'); Result.Add('BIGINT');
        Result.Add('DECIMAL(18,2)'); Result.Add('DECIMAL(18,4)');
        Result.Add('FLOAT'); Result.Add('DOUBLE');
        Result.Add('CHAR(1)'); Result.Add('CHAR(10)');
        Result.Add('VARCHAR(50)'); Result.Add('VARCHAR(255)'); Result.Add('VARCHAR(1000)');
        Result.Add('TEXT'); Result.Add('MEDIUMTEXT'); Result.Add('LONGTEXT');
        Result.Add('BLOB'); Result.Add('MEDIUMBLOB'); Result.Add('LONGBLOB');
        Result.Add('DATE'); Result.Add('TIME'); Result.Add('DATETIME'); Result.Add('TIMESTAMP');
        Result.Add('BOOLEAN');
        Result.Add('JSON');
      end;

    cdSQLite:
      begin
        Result.Add('INTEGER'); Result.Add('REAL');
        Result.Add('TEXT'); Result.Add('BLOB'); Result.Add('NUMERIC');
        Result.Add('BOOLEAN'); Result.Add('DATE'); Result.Add('DATETIME');
      end;

    cdMSSQL:
      begin
        Result.Add('INT'); Result.Add('BIGINT'); Result.Add('SMALLINT'); Result.Add('TINYINT');
        Result.Add('DECIMAL(18,2)'); Result.Add('FLOAT');
        Result.Add('CHAR(10)');
        Result.Add('VARCHAR(50)'); Result.Add('VARCHAR(255)'); Result.Add('VARCHAR(1000)');
        Result.Add('NVARCHAR(255)'); Result.Add('NVARCHAR(MAX)');
        Result.Add('VARBINARY(MAX)');
        Result.Add('DATE'); Result.Add('TIME'); Result.Add('DATETIME2');
        Result.Add('BIT');
      end;

    cdOracle:
      begin
        Result.Add('NUMBER(10)'); Result.Add('NUMBER(19)');
        Result.Add('NUMBER(18,2)');
        Result.Add('BINARY_FLOAT'); Result.Add('BINARY_DOUBLE');
        Result.Add('VARCHAR2(50)'); Result.Add('VARCHAR2(255)'); Result.Add('VARCHAR2(1000)');
        Result.Add('CLOB'); Result.Add('BLOB');
        Result.Add('DATE'); Result.Add('TIMESTAMP');
      end;

    cdSybase:
      begin
        Result.Add('INT'); Result.Add('BIGINT'); Result.Add('SMALLINT'); Result.Add('TINYINT');
        Result.Add('NUMERIC(18,2)'); Result.Add('REAL'); Result.Add('FLOAT');
        Result.Add('VARCHAR(50)'); Result.Add('VARCHAR(255)'); Result.Add('VARCHAR(1000)');
        Result.Add('TEXT'); Result.Add('IMAGE');
        Result.Add('DATETIME'); Result.Add('BIT');
      end;

    cdODBC:
      begin
        Result.Add('INTEGER'); Result.Add('BIGINT'); Result.Add('SMALLINT');
        Result.Add('DECIMAL(18,2)'); Result.Add('DOUBLE');
        Result.Add('VARCHAR(50)'); Result.Add('VARCHAR(255)'); Result.Add('VARCHAR(1000)');
        Result.Add('TEXT'); Result.Add('BLOB');
        Result.Add('DATE'); Result.Add('TIME'); Result.Add('TIMESTAMP');
        Result.Add('BOOLEAN');
      end;
  else
    Result.Add('VARCHAR(255)');
  end;
end;

{ ------------------------------------------------------------------ }
{ Automatisches Typ-Mapping                                          }
{ ------------------------------------------------------------------ }

function AutoMapType(const ASourceType: string; ADestDBMS: TCrossDBMS): string;
var
  S, Base, SizeStr: string;
  P1, P2, Size: Integer;
begin
  S := UpperCase(Trim(ASourceType));
  Result := '';

  Base := S;
  SizeStr := '';
  P1 := Pos('(', S);
  if P1 > 0 then
  begin
    P2 := Pos(')', S);
    if P2 > P1 then
    begin
      Base := Trim(Copy(S, 1, P1 - 1));
      SizeStr := Trim(Copy(S, P1 + 1, P2 - P1 - 1));
      P1 := Pos(',', SizeStr);
      if P1 > 0 then
        SizeStr := Trim(Copy(SizeStr, 1, P1 - 1));
    end;
  end;
  Size := StrToIntDef(SizeStr, 0);

  case ADestDBMS of
    cdPostgreSQL:
      begin
        if (Pos('SMALLINT', Base) > 0) or (Pos('TINYINT', Base) > 0) then
          Result := 'SMALLINT'
        else if Pos('BIGINT', Base) > 0 then
          Result := 'BIGINT'
        else if (Pos('INTEGER', Base) > 0) or (Pos('INT', Base) > 0) then
          Result := 'INTEGER'
        else if Pos('NUMERIC', Base) > 0 then
          Result := 'NUMERIC(' + SizeStr + ',2)'
        else if Pos('DECIMAL', Base) > 0 then
          Result := 'DECIMAL(' + SizeStr + ',2)'
        else if (Pos('FLOAT', Base) > 0) or (Pos('REAL', Base) > 0) then
          Result := 'REAL'
        else if Pos('DOUBLE', Base) > 0 then
          Result := 'DOUBLE PRECISION'
        else if Pos('CHAR', Base) > 0 then
          if Size > 0 then Result := 'CHAR(' + IntToStr(Size) + ')' else Result := 'CHAR(1)'
        else if Pos('VARCHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR(' + IntToStr(Size) + ')' else Result := 'VARCHAR(255)'
        else if (Pos('TEXT', Base) > 0) or (Pos('CLOB', Base) > 0) then
          Result := 'TEXT'
        else if (Pos('BLOB', Base) > 0) or (Pos('BINARY', Base) > 0) then
          Result := 'BYTEA'
        else if Pos('TIMESTAMP', Base) > 0 then Result := 'TIMESTAMP'
        else if Pos('DATE', Base) > 0 then Result := 'DATE'
        else if Pos('TIME', Base) > 0 then Result := 'TIME'
        else if Pos('BOOL', Base) > 0 then Result := 'BOOLEAN'
        else if Pos('UUID', Base) > 0 then Result := 'UUID'
        else if Pos('JSON', Base) > 0 then Result := 'JSONB'
        else Result := 'TEXT';
      end;

    cdMySQL:
      begin
        if Pos('TINYINT', Base) > 0 then Result := 'TINYINT'
        else if Pos('SMALLINT', Base) > 0 then Result := 'SMALLINT'
        else if Pos('BIGINT', Base) > 0 then Result := 'BIGINT'
        else if (Pos('INTEGER', Base) > 0) or (Pos('INT', Base) > 0) then Result := 'INT'
        else if Pos('NUMERIC', Base) > 0 then Result := 'DECIMAL(' + SizeStr + ',2)'
        else if Pos('DECIMAL', Base) > 0 then Result := 'DECIMAL(' + SizeStr + ',2)'
        else if Pos('FLOAT', Base) > 0 then Result := 'FLOAT'
        else if Pos('DOUBLE', Base) > 0 then Result := 'DOUBLE'
        else if Pos('CHAR', Base) > 0 then
          if Size > 0 then Result := 'CHAR(' + IntToStr(Size) + ')' else Result := 'CHAR(1)'
        else if Pos('VARCHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR(' + IntToStr(Size) + ')' else Result := 'VARCHAR(255)'
        else if Pos('TEXT', Base) > 0 then Result := 'TEXT'
        else if Pos('BLOB', Base) > 0 then Result := 'BLOB'
        else if Pos('TIMESTAMP', Base) > 0 then Result := 'DATETIME'
        else if Pos('DATE', Base) > 0 then Result := 'DATE'
        else if Pos('TIME', Base) > 0 then Result := 'TIME'
        else if Pos('BOOL', Base) > 0 then Result := 'BOOLEAN'
        else if Pos('JSON', Base) > 0 then Result := 'JSON'
        else Result := 'TEXT';
      end;

    cdSQLite:
      begin
        if (Pos('INT', Base) > 0) or (Pos('NUMERIC', Base) > 0) or (Pos('DECIMAL', Base) > 0) then
          Result := 'INTEGER'
        else if (Pos('FLOAT', Base) > 0) or (Pos('DOUBLE', Base) > 0) or (Pos('REAL', Base) > 0) then
          Result := 'REAL'
        else if (Pos('BLOB', Base) > 0) or (Pos('BINARY', Base) > 0) then
          Result := 'BLOB'
        else if Pos('BOOL', Base) > 0 then Result := 'BOOLEAN'
        else Result := 'TEXT';
      end;

    cdFirebird:
      begin
        if Pos('SMALLINT', Base) > 0 then Result := 'SMALLINT'
        else if Pos('BIGINT', Base) > 0 then Result := 'BIGINT'
        else if (Pos('INTEGER', Base) > 0) or (Pos('INT', Base) > 0) then Result := 'INTEGER'
        else if Pos('NUMERIC', Base) > 0 then Result := 'NUMERIC(' + SizeStr + ',2)'
        else if Pos('DECIMAL', Base) > 0 then Result := 'DECIMAL(' + SizeStr + ',2)'
        else if Pos('FLOAT', Base) > 0 then Result := 'FLOAT'
        else if Pos('DOUBLE', Base) > 0 then Result := 'DOUBLE PRECISION'
        else if Pos('CHAR', Base) > 0 then
          if Size > 0 then Result := 'CHAR(' + IntToStr(Size) + ')' else Result := 'CHAR(1)'
        else if Pos('VARCHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR(' + IntToStr(Size) + ')' else Result := 'VARCHAR(255)'
        else if Pos('TEXT', Base) > 0 then Result := 'BLOB SUB_TYPE TEXT'
        else if (Pos('BLOB', Base) > 0) or (Pos('BINARY', Base) > 0) then
          Result := 'BLOB SUB_TYPE BINARY'
        else if Pos('TIMESTAMP', Base) > 0 then Result := 'TIMESTAMP'
        else if Pos('DATE', Base) > 0 then Result := 'DATE'
        else if Pos('TIME', Base) > 0 then Result := 'TIME'
        else if Pos('BOOL', Base) > 0 then Result := 'BOOLEAN'
        else Result := 'VARCHAR(255)';
      end;

    cdMSSQL:
      begin
        if Pos('BIGINT', Base) > 0 then Result := 'BIGINT'
        else if Pos('SMALLINT', Base) > 0 then Result := 'SMALLINT'
        else if Pos('TINYINT', Base) > 0 then Result := 'TINYINT'
        else if (Pos('INTEGER', Base) > 0) or (Pos('INT', Base) > 0) then Result := 'INT'
        else if Pos('NUMERIC', Base) > 0 then Result := 'DECIMAL(18,2)'
        else if Pos('DECIMAL', Base) > 0 then Result := 'DECIMAL(18,2)'
        else if Pos('FLOAT', Base) > 0 then Result := 'FLOAT'
        else if Pos('DOUBLE', Base) > 0 then Result := 'FLOAT'
        else if Pos('CHAR', Base) > 0 then
          if Size > 0 then Result := 'NVARCHAR(' + IntToStr(Size) + ')' else Result := 'NVARCHAR(255)'
        else if Pos('VARCHAR', Base) > 0 then
          if Size > 0 then Result := 'NVARCHAR(' + IntToStr(Size) + ')' else Result := 'NVARCHAR(255)'
        else if (Pos('TEXT', Base) > 0) or (Pos('MEMO', Base) > 0) then Result := 'NVARCHAR(MAX)'
        else if Pos('BLOB', Base) > 0 then Result := 'VARBINARY(MAX)'
        else if Pos('TIMESTAMP', Base) > 0 then Result := 'DATETIME2'
        else if Pos('DATE', Base) > 0 then Result := 'DATE'
        else if Pos('TIME', Base) > 0 then Result := 'TIME'
        else if Pos('BOOL', Base) > 0 then Result := 'BIT'
        else Result := 'NVARCHAR(255)';
      end;

    cdOracle:
      begin
        if Pos('BIGINT', Base) > 0 then Result := 'NUMBER(19)'
        else if Pos('SMALLINT', Base) > 0 then Result := 'NUMBER(5)'
        else if (Pos('INTEGER', Base) > 0) or (Pos('INT', Base) > 0) then Result := 'NUMBER(10)'
        else if Pos('NUMERIC', Base) > 0 then Result := 'NUMBER(18,2)'
        else if Pos('DECIMAL', Base) > 0 then Result := 'NUMBER(18,2)'
        else if Pos('FLOAT', Base) > 0 then Result := 'BINARY_FLOAT'
        else if Pos('DOUBLE', Base) > 0 then Result := 'BINARY_DOUBLE'
        else if Pos('CHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR2(' + IntToStr(Size) + ')' else Result := 'VARCHAR2(255)'
        else if Pos('VARCHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR2(' + IntToStr(Size) + ')' else Result := 'VARCHAR2(255)'
        else if (Pos('TEXT', Base) > 0) or (Pos('CLOB', Base) > 0) then Result := 'CLOB'
        else if Pos('BLOB', Base) > 0 then Result := 'BLOB'
        else if Pos('TIMESTAMP', Base) > 0 then Result := 'TIMESTAMP'
        else if Pos('DATE', Base) > 0 then Result := 'DATE'
        else if Pos('TIME', Base) > 0 then Result := 'TIMESTAMP'
        else if Pos('BOOL', Base) > 0 then Result := 'NUMBER(1)'
        else Result := 'VARCHAR2(255)';
      end;

    cdSybase:
      begin
        if Pos('BIGINT', Base) > 0 then Result := 'BIGINT'
        else if Pos('SMALLINT', Base) > 0 then Result := 'SMALLINT'
        else if Pos('TINYINT', Base) > 0 then Result := 'TINYINT'
        else if (Pos('INTEGER', Base) > 0) or (Pos('INT', Base) > 0) then Result := 'INT'
        else if Pos('NUMERIC', Base) > 0 then Result := 'NUMERIC(18,2)'
        else if Pos('DECIMAL', Base) > 0 then Result := 'DECIMAL(18,2)'
        else if Pos('FLOAT', Base) > 0 then Result := 'REAL'
        else if Pos('DOUBLE', Base) > 0 then Result := 'FLOAT'
        else if Pos('CHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR(' + IntToStr(Size) + ')' else Result := 'VARCHAR(255)'
        else if Pos('VARCHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR(' + IntToStr(Size) + ')' else Result := 'VARCHAR(255)'
        else if (Pos('TEXT', Base) > 0) or (Pos('MEMO', Base) > 0) then Result := 'TEXT'
        else if Pos('BLOB', Base) > 0 then Result := 'IMAGE'
        else if Pos('TIMESTAMP', Base) > 0 then Result := 'DATETIME'
        else if Pos('DATE', Base) > 0 then Result := 'DATETIME'
        else if Pos('TIME', Base) > 0 then Result := 'DATETIME'
        else if Pos('BOOL', Base) > 0 then Result := 'BIT'
        else Result := 'VARCHAR(255)';
      end;

    cdODBC, cdUnknown:
      begin
        if Pos('BIGINT', Base) > 0 then Result := 'BIGINT'
        else if (Pos('INTEGER', Base) > 0) or (Pos('INT', Base) > 0) then Result := 'INTEGER'
        else if (Pos('NUMERIC', Base) > 0) or (Pos('DECIMAL', Base) > 0) then Result := 'DECIMAL(18,2)'
        else if (Pos('FLOAT', Base) > 0) or (Pos('DOUBLE', Base) > 0) then Result := 'DOUBLE'
        else if Pos('CHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR(' + IntToStr(Size) + ')' else Result := 'VARCHAR(255)'
        else if Pos('VARCHAR', Base) > 0 then
          if Size > 0 then Result := 'VARCHAR(' + IntToStr(Size) + ')' else Result := 'VARCHAR(255)'
        else if (Pos('TEXT', Base) > 0) or (Pos('CLOB', Base) > 0) then Result := 'TEXT'
        else if Pos('BLOB', Base) > 0 then Result := 'BLOB'
        else if Pos('TIMESTAMP', Base) > 0 then Result := 'TIMESTAMP'
        else if Pos('DATE', Base) > 0 then Result := 'DATE'
        else if Pos('TIME', Base) > 0 then Result := 'TIME'
        else if Pos('BOOL', Base) > 0 then Result := 'BOOLEAN'
        else Result := 'VARCHAR(255)';
      end;
  else
    Result := 'VARCHAR(255)';
  end;

  if Trim(Result) = '' then
    Result := 'VARCHAR(255)';
end;

// ============================================================================
// ODBC-Backend-Erkennung
// ============================================================================

function DetectBackendDBMS(AQuery: TSQLQuery; ATrans: TSQLTransaction): TCrossDBMS;
var
  V: string;
begin
  Result := cdUnknown;
  if not Assigned(AQuery) then Exit;

  { Test 1: VERSION() — MySQL, MariaDB, PostgreSQL }
  try
    AQuery.Close;
    AQuery.SQL.Text := 'SELECT VERSION()';
    if Assigned(ATrans) and not ATrans.Active then
      ATrans.StartTransaction;
    AQuery.Open;
    if not AQuery.EOF then
      V := UpperCase(AQuery.Fields[0].AsString);
    AQuery.Close;

    if Pos('MARIADB', V) > 0 then Exit(cdMySQL);
    if Pos('MYSQL', V) > 0 then Exit(cdMySQL);
    if Pos('POSTGRESQL', V) > 0 then Exit(cdPostgreSQL);
  except
    try AQuery.Close; except end;
  end;

  { Test 2: SQLite — sqlite_version() }
  try
    AQuery.SQL.Text := 'SELECT sqlite_version()';
    AQuery.Open;
    if not AQuery.EOF then
      V := Trim(AQuery.Fields[0].AsString);
    AQuery.Close;
    if V <> '' then Exit(cdSQLite);
  except
    try AQuery.Close; except end;
  end;

  { Test 3: Firebird — rdb$get_context }
  try
    AQuery.SQL.Text :=
      'SELECT rdb$get_context(''SYSTEM'', ''ENGINE_VERSION'') FROM rdb$database';
    AQuery.Open;
    if not AQuery.EOF then
      V := Trim(AQuery.Fields[0].AsString);
    AQuery.Close;
    if V <> '' then Exit(cdFirebird);
  except
    try AQuery.Close; except end;
  end;

  { Test 4: MSSQL — @@VERSION }
  try
    AQuery.SQL.Text := 'SELECT @@VERSION';
    AQuery.Open;
    if not AQuery.EOF then
      V := UpperCase(AQuery.Fields[0].AsString);
    AQuery.Close;
    if Pos('MICROSOFT', V) > 0 then Exit(cdMSSQL);
  except
    try AQuery.Close; except end;
  end;

  { Test 5: Oracle — v$version }
  try
    AQuery.SQL.Text := 'SELECT banner FROM v$version WHERE ROWNUM = 1';
    AQuery.Open;
    if not AQuery.EOF then
      V := UpperCase(AQuery.Fields[0].AsString);
    AQuery.Close;
    if Pos('ORACLE', V) > 0 then Exit(cdOracle);
  except
    try AQuery.Close; except end;
  end;

  { Test 6: Sybase — @@version }
  try
    AQuery.SQL.Text := 'SELECT @@version';
    AQuery.Open;
    if not AQuery.EOF then
      V := UpperCase(AQuery.Fields[0].AsString);
    AQuery.Close;
    if Pos('SYBASE', V) > 0 then Exit(cdSybase);
  except
    try AQuery.Close; except end;
  end;
end;

end.
