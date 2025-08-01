unit uDBObjectFetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, IBConnection,  Dialogs,
  turbocommon,
  fbcommon;

function GetDBObject(
  Conn: TIBConnection;
  AObjectType: TObjectType;
  AObjectName: string;
  APackageName: string = ''
): string;

implementation

function IsPackageObject(AObjectType: TObjectType): Boolean;
begin
  Result := AObjectType in [
    otPackageFunctions,
    otPackageProcedures,
    otPackageUDFFunctions,
    otPackageUDRFunctions,
    otPackageUDRProcedures
  ];
end;

function IsGlobalObject(AObjectType: TObjectType): Boolean;
begin
  Result := not IsPackageObject(AObjectType);
end;

function GetDBObject(
  Conn: TIBConnection;
  AObjectType: TObjectType;
  AObjectName: string;
  APackageName: string
): string;
var
  Q: TSQLQuery;
  SQL: string;
begin
  Result := '';
  AObjectName := UpperCase(Trim(AObjectName));
  APackageName := UpperCase(Trim(APackageName));

  if (APackageName = '') and IsPackageObject(AObjectType) then
    raise Exception.Create('Package object type requires a package name.');

  if (APackageName <> '') and IsGlobalObject(AObjectType) then
    raise Exception.Create('Global object type cannot be part of a package.');

  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := Conn;

    //if Q.DataBase.Connected then showmessage('connected')
    //else showmessage('not connected');
    //ShowMessage(Q.DataBase.DatabaseName);

    case AObjectType of
      otFBFunctions, otUDRFunctions:
        SQL := 'SELECT RDB$FUNCTION_SOURCE FROM RDB$FUNCTIONS WHERE RDB$FUNCTION_NAME = :NAME AND RDB$PACKAGE_NAME IS NULL';

      otFBProcedures, otUDRProcedures:
        SQL := 'SELECT RDB$PROCEDURE_SOURCE FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = :NAME AND RDB$PACKAGE_NAME IS NULL';

      otPackageFunctions, otPackageUDRFunctions, otPackageUDFFunctions:
        SQL := 'SELECT RDB$FUNCTION_SOURCE FROM RDB$FUNCTIONS WHERE RDB$FUNCTION_NAME = :NAME AND RDB$PACKAGE_NAME = :PKG';

      otPackageProcedures, otPackageUDRProcedures:
        SQL := 'SELECT RDB$PROCEDURE_SOURCE FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = :NAME AND RDB$PACKAGE_NAME = :PKG';

      otViews:
        SQL := 'SELECT RDB$VIEW_SOURCE FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = :NAME';

      otTriggers:
        SQL := 'SELECT RDB$TRIGGER_SOURCE FROM RDB$TRIGGERS WHERE RDB$TRIGGER_NAME = :NAME';

      otDomains:
        SQL := 'SELECT RDB$FIELD_NAME FROM RDB$FIELDS WHERE RDB$FIELD_NAME = :NAME';

      otExceptions:
        SQL := 'SELECT RDB$MESSAGE FROM RDB$EXCEPTIONS WHERE RDB$EXCEPTION_NAME = :NAME';

      otRoles:
        SQL := 'SELECT RDB$ROLE_NAME FROM RDB$ROLES WHERE RDB$ROLE_NAME = :NAME';

      otGenerators:
        SQL := 'SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME = :NAME';

      otStoredProcedures:
        SQL := 'SELECT RDB$PROCEDURE_SOURCE FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = :NAME AND RDB$PACKAGE_NAME IS NULL';

      otUDF:
      begin
        SQL :=
         'SELECT ' +
          QuotedStr('DECLARE EXTERNAL FUNCTION ') + ' || TRIM(F.RDB$FUNCTION_NAME) || ' +
          QuotedStr('(') + ' || ' +
          'LIST(DISTINCT TRIM(A.RDB$ARGUMENT_NAME) || '' '' || ' +
          'CASE FLD.RDB$FIELD_TYPE ' +
            'WHEN 7 THEN ''SMALLINT'' ' +
            'WHEN 8 THEN ''INTEGER'' ' +
            'WHEN 10 THEN ''FLOAT'' ' +
            'WHEN 12 THEN ''DATE'' ' +
            'WHEN 13 THEN ''TIME'' ' +
            'WHEN 14 THEN ''CHAR('' || FLD.RDB$FIELD_LENGTH || '')'' ' +
            'WHEN 16 THEN CASE FLD.RDB$FIELD_SUB_TYPE ' +
                          'WHEN 1 THEN ''NUMERIC('' || FLD.RDB$FIELD_PRECISION || '','' || ABS(FLD.RDB$FIELD_SCALE) || '')'' ' +
                          'WHEN 2 THEN ''DECIMAL('' || FLD.RDB$FIELD_PRECISION || '','' || ABS(FLD.RDB$FIELD_SCALE) || '')'' ' +
                          'ELSE ''BIGINT'' END ' +
            'WHEN 27 THEN ''DOUBLE PRECISION'' ' +
            'WHEN 35 THEN ''TIMESTAMP'' ' +
            'WHEN 37 THEN ''VARCHAR('' || FLD.RDB$FIELD_LENGTH || '')'' ' +
            'ELSE ''UNKNOWN'' ' +
          'END, '','') || ' +
          QuotedStr(') RETURNS ??? ENTRY_POINT ''') + ' || TRIM(F.RDB$ENTRYPOINT) || ' + QuotedStr(''' MODULE_NAME ''') + ' || TRIM(F.RDB$MODULE_NAME) ' +
          'FROM RDB$FUNCTIONS F ' +
          'LEFT JOIN RDB$FUNCTION_ARGUMENTS A ON A.RDB$FUNCTION_NAME = F.RDB$FUNCTION_NAME ' +
          'LEFT JOIN RDB$FIELDS FLD ON FLD.RDB$FIELD_NAME = A.RDB$FIELD_SOURCE ' +
          //'WHERE UPPER(TRIM(F.RDB$FUNCTION_NAME)) = UPPER(' + QuotedStr(AObjectName) + ') ' +
          'WHERE TRIM(F.RDB$FUNCTION_NAME) =  ' + QuotedStr('TEST_UDF')  + ' ' +
          'AND F.RDB$PACKAGE_NAME IS NULL ' +
          'GROUP BY F.RDB$FUNCTION_NAME, F.RDB$ENTRYPOINT, F.RDB$MODULE_NAME;';
      end;


      otPackages:
        SQL := 'SELECT RDB$PACKAGE_HEADER || ''\n'' || RDB$PACKAGE_BODY FROM RDB$PACKAGES WHERE RDB$PACKAGE_NAME = :NAME';

    else
      raise Exception.Create('Unsupported object type in GetDBObject');
    end;

    if SQL <> '' then
    begin
      Q.SQL.Text := SQL;
      if Pos(':PKG', SQL) > 0 then
        Q.Params.ParamByName('PKG').AsString := APackageName;

      Q.Open;
      if not Q.EOF then
        Result := Trim(Q.Fields[0].AsString);
    end;

  finally
    Q.Free;
  end;
end;

end.

