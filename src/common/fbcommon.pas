unit fbcommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB, FBAdmin, IniFiles,
  fSetFBClient, Controls, Dialogs,
  ibase60dyn,

  //IBX
  IBXServices;

type
  TRoutineType = (
    rtUDF, rtFBFunc, rtFBProc, rtUDRFunc, rtUDRProc,
    rtPackageFBFunc, rtPackageFBProc, rtPackageUDRFunc, rtPackageUDRProc,
    rtUnknown
  );

  TParamTypeFilter = (ptAll, ptInOnly, ptOutOnly);

  TRoutineInfo = record
    dbIndex: integer;
    RoutineType: TRoutineType;
    RoutineName: string;
    PackageName: string; // leer wenn kein Package
    Connection: TIBConnection;
  end;

function SetFBClient(Sender: word): boolean;
procedure DetectFBVersion(FBClientLib: string);

function NormalizeFloatForSQL(Value: Double): string;
function RoutineTypeToStr(ARoutineType: TRoutineType): string;
function StrToRoutineType(const AStr: string): TRoutineType;
function IsFunctionRoutine(RT: TRoutineType): Boolean;
function IsPackageRoutine(RT: TRoutineType): Boolean;
function IsCharType(FieldType: Integer): Boolean;
function GetRoutineListSQL(AConnection: TIBConnection; RT: TRoutineType): string;
function GetParamListSQL(AConnection: TIBConnection; const Info: TRoutineInfo; ParamTypeFilter: TParamTypeFilter = ptAll): string;

function GetAllRoutinesAsList(AConnection: TIBConnection; AFormat: boolean): TStringList;
function GetAllFunctionsAsQuery: string;
function GetAllProceduresAsQuery: string;
function GetAllRoutinesAsQuery: string;

var IBaseLibraryHandle : TLibHandle;
    IBaseLibrary: string;

    FBVersionString: string;
    FBVersionMajor: Integer = 0;
    FBVersionMinor: Integer = 0;
    FBVersionNumber: single = 0.0;

    IBXServicesConnection1: TIBXServicesConnection;
    IBXServerProperties1: TIBXServerProperties;

implementation

uses turbocommon;

function NormalizeFloatForSQL(Value: Double): string;
begin
  Result := FloatToStrF(Value, ffGeneral, 15, 0);
  // Sicherheitshalber: ersetze evtl. Komma durch Punkt
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
end;


function GetClientLibraryPath: string;
begin
  Result := fIniFile.ReadString('Firebird', 'ClientLib', '');
end;

procedure SaveClientLibraryPath(const path: string);
begin
  fIniFile.WriteString('Firebird', 'ClientLib', path);
end;

procedure DetectFBVersion(FBClientLib: string);
var
  S, VersionToken: string;
  StartPos, EndPos: Integer;
begin
  IBXServicesConnection1.Connected := false;
  IBXServicesConnection1.FirebirdLibraryPathName := FBClientLib;
  IBXServicesConnection1.Connected := true;
  FBVersionString := IBXServerProperties1.VersionInfo.ServerVersion;

  S := UpperCase(FBVersionString);
  // Beispiel: 'LI-V6.3.11.33637 FIREBIRD 3.0'

  StartPos := Pos('FIREBIRD', S);
  if StartPos > 0 then
  begin
    VersionToken := Trim(Copy(S, StartPos + 8, 10)); // 8 = length('FIREBIRD ')
    EndPos := Pos('.', VersionToken);
    if EndPos > 0 then
    begin
      FBVersionMajor := StrToIntDef(Copy(VersionToken, 1, EndPos - 1), 0);
      FBVersionMinor := StrToIntDef(Copy(VersionToken, EndPos + 1, 2), 0);
    end;
  end;
end;

function SetFBClient(Sender: Word): Boolean;
var
  fbclibOld, fbclibNew: string;
  caption0, caption1: string;
begin
  caption0 :=
    'FireBird client in ini-file does not exist or is invalid.' + sLineBreak +
    'Please select a version from the list below or' + sLineBreak +
    'use the "Browse" button to choose a valid version.' + sLineBreak +
    'Then test your selection with the "Test" button.';
  caption1 := 'Select a Firebird client library from the list below.';

  Result := false;
  // Wenn zur Laufzeit aufgerufen: alte Bibliothek entladen
  if Sender = 1 then
  begin
    if IBaseLibraryHandle <> 0 then
    begin
      //ReleaseIBase60;
      UnloadLibrary(IBaseLibraryHandle);
    end;
    IBaseLibraryHandle := 0;
    fbclibOld := ' ';
  end else
  begin
    // Aktuell gespeicherter Pfad zur Client-Bibliothek
    fbclibOld := GetClientLibraryPath;
    fbclibNew := fbclibOld;
  end;

  // Erster Ladeversuch mit fbclibOld

  IBaseLibraryHandle := LoadLibrary(PChar(fbclibOld));


  // Falls Laden fehlschlägt → Dialog zur Auswahl neuer Bibliothek
  if IBaseLibraryHandle = 0 then
  begin
    frmSetFBClient := TfrmSetFBClient.Create(nil);
    if Sender = 0 then frmSetFBClient.lblInfo.Caption :=  caption0
    else frmSetFBClient.lblInfo.Caption := caption1;
    try
      if frmSetFBClient.ShowModal = mrOk then
      begin
        fbclibNew := frmSetFBClient.FBClientLibPath;
        //IBaseLibraryHandle := InitialiseIBase60(fbclibNew);
        IBaseLibraryHandle := LoadLibrary(PChar(fbclibNew));
        if IBaseLibraryHandle = 0 then
        begin
          ShowMessage('An error occurred while loading the selected Firebird client library: ' + fbclibNew);
          Exit;
        end;
        // Falls neue Bibliothek abweicht, speichern
        if fbclibNew <> fbclibOld then
          SaveClientLibraryPath(fbclibNew);
      end
      else
      begin
        ShowMessage('Firebird client library was not set. The application will now close..');
        Exit;
      end;
    finally
      frmSetFBClient.Free;
    end;
  end;
  result := true;
  IBaseLibrary := fbclibNew;
  DetectFBVersion(IBaseLibrary);
end;

function RoutineTypeToStr(ARoutineType: TRoutineType): string;
begin
  case ARoutineType of
    rtUDF:              Result := 'UDF';
    rtFBFunc:           Result := 'FireBird Function';
    rtFBProc:           Result := 'FireBird Stored Procedure';
    rtUDRFunc:          Result := 'UDR Function';
    rtUDRProc:          Result := 'UDR Procedure';
    rtPackageFBFunc:    Result := 'Package-FireBird Function';
    rtPackageFBProc:    Result := 'Package-FireBird Stored Procedure';
    rtPackageUDRFunc:   Result := 'Package-UDR Function';
    rtPackageUDRProc:   Result := 'Package-UDR Procedure';
    rtUnknown:          Result := 'Unknown';
  else
    Result := 'Invalid';
  end;
end;

function StrToRoutineType(const AStr: string): TRoutineType;
var
  S: string;
begin
  S := UpperCase(Trim(AStr));
  if (S = 'RTUDF') or (S = 'UDF') then
    Result := rtUDF
  else if (S = 'RTFBFUNC') or (S = 'FIREBIRD FUNCTION') or (S = 'FBFUNC') then
    Result := rtFBFunc
  else if (S = 'RTFBPROC') or (S = 'FIREBIRD STORED PROCEDURE') or (S = 'FBPROC') then
    Result := rtFBProc
  else if (S = 'RTUDRFUNC') or (S = 'UDR FUNCTION') or (S = 'UDRFUNC') then
    Result := rtUDRFunc
  else if (S = 'RTUDRPROC') or (S = 'UDR PROCEDURE') or (S = 'UDRPROC') then
    Result := rtUDRProc
  else if (S = 'RTPACKAGEFBFUNC') or (S = 'PACKAGE-FIREBIRD FUNCTION') or (S = 'PACKAGEFBFUNC') then
    Result := rtPackageFBFunc
  else if (S = 'RTPACKAGEFBPROC') or (S = 'PACKAGE-FIREBIRD STORED PROCEDURE') or (S = 'PACKAGEFBPROC') then
    Result := rtPackageFBProc
  else if (S = 'RTPACKAGEUDRFUNC') or (S = 'PACKAGE-UDR FUNCTION') or (S = 'PACKAGEUDRFUNC') then
    Result := rtPackageUDRFunc
  else if (S = 'RTPACKAGEUDRPROC') or (S = 'PACKAGE-UDR PROCEDURE') or (S = 'PACKAGEUDRPROC') then
    Result := rtPackageUDRProc
  else if (S = 'RTUNKNOWN') or (S = 'UNKNOWN') then
    Result := rtUnknown
  else
    Result := rtUnknown;
end;


function IsPackageRoutine(RT: TRoutineType): Boolean;
begin
  case RT of
    rtPackageFBFunc,
    rtPackageFBProc,
    rtPackageUDRFunc,
    rtPackageUDRProc:
      Result := True;
  else
    Result := False;
  end;
end;

function IsFunctionRoutine(RT: TRoutineType): Boolean;
begin
  case RT of
    rtUDF,
    rtFBFunc,
    rtUDRFunc,
    rtPackageFBFunc,
    rtPackageUDRFunc:
      Result := True;
  else
    Result := False;
  end;
end;


function IsCharType(FieldType: Integer): Boolean;
begin
  Result := FieldType in [14, 37, 40]; // CHAR, VARCHAR, CSTRING
end;

function GetRoutineListSQL(AConnection: TIBConnection; RT: TRoutineType): string;
begin
  case RT of
    rtUDF:
      Result :=
        'SELECT ' +
        'RDB$FUNCTION_NAME AS NAME, ' +
        '  ''rtUDF'' AS ROUTINE_TYPE ' +
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$ENGINE_NAME IS NULL ' +
        '  AND RDB$PACKAGE_NAME IS NULL ' +
        '  AND RDB$MODULE_NAME IS NOT NULL ' +
        '  AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$FUNCTION_NAME';

    rtFBFunc:
      Result :=
        'SELECT RDB$FUNCTION_NAME AS NAME, ' +
        '  ''rtFBFunc'' AS ROUTINE_TYPE ' +      //synthetic field
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$ENGINE_NAME IS NULL AND RDB$PACKAGE_NAME IS NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$FUNCTION_NAME';

    rtFBProc:
      Result :=
        'SELECT RDB$PROCEDURE_NAME AS NAME ' +
      //  '  ''rtFBProc'' AS ROUTINE_TYPE ' +
        'FROM RDB$PROCEDURES ' +
        'WHERE RDB$PACKAGE_NAME IS NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$PROCEDURE_NAME';

    rtUDRFunc:
      Result :=
        'SELECT RDB$FUNCTION_NAME AS NAME ' +
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$ENGINE_NAME IS NOT NULL AND RDB$PACKAGE_NAME IS NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$FUNCTION_NAME';

    rtUDRProc:
      Result :=
        'SELECT RDB$PROCEDURE_NAME AS NAME ' +
        'FROM RDB$PROCEDURES ' +
        'WHERE RDB$ENGINE_NAME IS NOT NULL AND RDB$PACKAGE_NAME IS NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$PROCEDURE_NAME';

    rtPackageFBFunc:
      Result :=
        'SELECT ' +
        'RDB$FUNCTION_NAME AS NAME, ' +
        'RDB$PACKAGE_NAME  AS PACKAGE_NAME ' +
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$PACKAGE_NAME  = :PackageName AND RDB$ENGINE_NAME IS NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY NAME';

    rtPackageFBProc:
      Result :=
      'SELECT ' +
        'RDB$PROCEDURE_NAME AS NAME, ' +
        'RDB$PACKAGE_NAME  AS PACKAGE_NAME ' +
      'FROM RDB$PROCEDURES ' +
      'WHERE RDB$PACKAGE_NAME  = :PackageName AND RDB$ENGINE_NAME IS NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
      'ORDER BY NAME';

    rtPackageUDRFunc:
      Result :=
        'SELECT ' +
        'RDB$FUNCTION_NAME AS NAME, ' +
        'RDB$PACKAGE_NAME  AS PACKAGE_NAME ' +
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$PACKAGE_NAME  = :PackageName AND RDB$ENGINE_NAME IS NOT NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY NAME';

     rtPackageUDRProc:
      Result :=
      'SELECT ' +
        'RDB$PROCEDURE_NAME AS NAME, ' +
        'RDB$PACKAGE_NAME  AS PACKAGE_NAME ' +
      'FROM RDB$PROCEDURES ' +
      'WHERE RDB$PACKAGE_NAME  = :PackageName AND RDB$ENGINE_NAME IS NOT NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
      'ORDER BY NAME';

    else
      Result := '';
  end;
end;

function GetParamListSQL(AConnection: TIBConnection; const info: TRoutineInfo; ParamTypeFilter: TParamTypeFilter = ptAll): string;
var
  IsFunction: Boolean;
  IsUDR: Boolean;
  IsPackage: Boolean;
begin
  IsFunction := info.RoutineType in [rtUDF, rtFBFunc, rtUDRFunc, rtPackageFBFunc, rtPackageUDRFunc];
  IsUDR := info.RoutineType in [rtUDRFunc, rtPackageUDRFunc, rtUDRProc, rtPackageUDRProc];
  IsPackage := info.RoutineType in [rtPackageFBFunc, rtPackageUDRFunc, rtPackageFBProc, rtPackageUDRProc];

  // --- Sonderfall UDF ---
  if info.RoutineType = rtUDF then
  begin
    Result :=
      'SELECT' + LineEnding +
      '  CASE' + LineEnding +
      '    WHEN fa.RDB$ARGUMENT_POSITION = 0 THEN ''RETURN''' + LineEnding +
      '    ELSE ''ARG_'' || fa.RDB$ARGUMENT_POSITION' + LineEnding +
      '  END AS PARAM_NAME,' + LineEnding +
      '  FA.RDB$FIELD_SOURCE AS DATATYPE_SOURCE, ' +
      '  fa.RDB$FIELD_TYPE,' + LineEnding +
      '  fa.RDB$FIELD_SUB_TYPE,' + LineEnding +
      '  fa.RDB$FIELD_LENGTH,' + LineEnding +
      '  fa.RDB$FIELD_PRECISION,' + LineEnding +
      '  fa.RDB$FIELD_SCALE,' + LineEnding +
      '  fa.RDB$CHARACTER_SET_ID,' + LineEnding +
      '  fa.RDB$ARGUMENT_POSITION,' + LineEnding +
      '  NULL AS RDB$PACKAGE_NAME,' + LineEnding +
      '  f.RDB$FUNCTION_NAME,' + LineEnding +
      '  fa.RDB$FIELD_SOURCE,' + LineEnding +
      '  rf.RDB$CHARACTER_LENGTH' + LineEnding +
      'FROM RDB$FUNCTION_ARGUMENTS fa' + LineEnding +
      'JOIN RDB$FUNCTIONS f ON f.RDB$FUNCTION_NAME = fa.RDB$FUNCTION_NAME' + LineEnding +
      'LEFT JOIN RDB$FIELDS rf ON rf.RDB$FIELD_NAME = fa.RDB$FIELD_SOURCE' + LineEnding +
      'WHERE TRIM(UPPER(f.RDB$FUNCTION_NAME)) = :FUNCNAME' + LineEnding +
      'ORDER BY fa.RDB$ARGUMENT_POSITION';
    Exit;
  end;

  if IsFunction then
  begin
    Result :=
      'SELECT ' +
      '  FA.RDB$ARGUMENT_NAME AS PARAM_NAME, ' +
      '  FA.RDB$FIELD_SOURCE AS DATATYPE_SOURCE, ' +
      '  F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' +
      '  F.RDB$FIELD_PRECISION, F.RDB$FIELD_SCALE, ' +
      '  F.RDB$CHARACTER_LENGTH, F.RDB$CHARACTER_SET_ID ' +
      'FROM RDB$FUNCTION_ARGUMENTS FA ' +
      'LEFT JOIN RDB$FIELDS F ON F.RDB$FIELD_NAME = FA.RDB$FIELD_SOURCE ' +
      'WHERE FA.RDB$FUNCTION_NAME = :FUNCNAME ';

    if IsPackage then
      Result := Result + 'AND FA.RDB$PACKAGE_NAME = :PACKAGENAME '
    else
      Result := Result + 'AND FA.RDB$PACKAGE_NAME IS NULL ';

    if info.RoutineType in [rtFBFunc, rtPackageFBFunc] then
      Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$FUNCTIONS F2 WHERE F2.RDB$FUNCTION_NAME = FA.RDB$FUNCTION_NAME AND F2.RDB$ENGINE_NAME IS NULL) '
    else if info.RoutineType in [rtUDRFunc, rtPackageUDRFunc] then
      Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$FUNCTIONS F2 WHERE F2.RDB$FUNCTION_NAME = FA.RDB$FUNCTION_NAME AND F2.RDB$ENGINE_NAME IS NOT NULL) ';

    Result := Result + 'ORDER BY FA.RDB$ARGUMENT_POSITION';
  end
  else
  begin
    // Prozeduren
    Result :=
      'SELECT ' +
      '  P.RDB$PARAMETER_NAME AS PARAM_NAME, ' +
      '  P.RDB$FIELD_SOURCE AS DATATYPE_SOURCE, ' +
      '  F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' +
      '  F.RDB$FIELD_PRECISION, F.RDB$FIELD_SCALE, ' +
      '  F.RDB$CHARACTER_LENGTH, F.RDB$CHARACTER_SET_ID ' +
      'FROM RDB$PROCEDURE_PARAMETERS P ' +
      'LEFT JOIN RDB$FIELDS F ON F.RDB$FIELD_NAME = P.RDB$FIELD_SOURCE ' +
      'WHERE P.RDB$PROCEDURE_NAME = :PROCNAME ';

    if IsPackage then
      Result := Result + 'AND P.RDB$PACKAGE_NAME = :PACKAGENAME '
    else
      Result := Result + 'AND P.RDB$PACKAGE_NAME IS NULL ';

    if info.RoutineType in [rtFBProc, rtPackageFBProc] then
      Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME AND PR.RDB$ENGINE_NAME IS NULL) '
    else if info.RoutineType in [rtUDRProc, rtPackageUDRProc] then
      Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME AND PR.RDB$ENGINE_NAME IS NOT NULL) ';

    // 💡 Filter nach Parametertyp
    case ParamTypeFilter of
      ptInOnly:
        Result := Result + 'AND P.RDB$PARAMETER_TYPE = 0 ';
      ptOutOnly:
        Result := Result + 'AND P.RDB$PARAMETER_TYPE = 1 ';
    end;

    Result := Result + 'ORDER BY P.RDB$PARAMETER_TYPE, P.RDB$PARAMETER_NUMBER';
  end;
end;

function GetAllRoutinesAsList(AConnection: TIBConnection; AFormat: boolean): TStringList;
var
  RoutineList: TStringList;
  Query: TSQLQuery;
  RT: TRoutineType;
  SQL: string;
begin
  RoutineList := TStringList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := AConnection;

    for RT := Low(TRoutineType) to High(TRoutineType) do
    begin
      SQL := GetRoutineListSQL(AConnection, RT);
      if SQL = '' then
        Continue;

      Query.Close;
      Query.SQL.Text := SQL;
      try
        Query.Open;
      except
        Continue; // Überspringen falls Fehler (z. B. bei nicht unterstütztem Typ)
      end;

      if AFormat then
        RoutineList.Add('-- ' + RoutineTypeToStr(RT) + ' --');

      while not Query.EOF do
      begin
        RoutineList.Add('  ' + Trim(Query.FieldByName('NAME').AsString));
        Query.Next;
      end;
    end;
  finally
    Query.Free;
    Result := RoutineList;
  end;
end;

function GetAllFunctionsAsQuery: string;
begin
  Result :=
    'SELECT TRIM(RDB$FUNCTION_NAME) AS NAME, ''FUNCTION'' AS ROUTINE_TYPE ' +
    'FROM RDB$FUNCTIONS ' +
    'WHERE RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0';
end;

function GetAllProceduresAsQuery: string;
begin
  Result :=
    'SELECT TRIM(RDB$PROCEDURE_NAME) AS NAME, ''PROCEDURE'' AS ROUTINE_TYPE ' +
    'FROM RDB$PROCEDURES ' +
    'WHERE RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0';
end;

function GetAllRoutinesAsQuery: string;
begin
  Result :=
    GetAllFunctionsAsQuery + LineEnding +
    'UNION ALL' + LineEnding +
    GetAllProceduresAsQuery + LineEnding +
    'ORDER BY NAME';
end;

initialization
  IBaseLibraryHandle := 0;
  IBaseLibrary       := ' ';

  IBXServicesConnection1 := TIBXServicesConnection.Create(nil);
  IBXServicesConnection1.LoginPrompt:=true;
  IBXServerProperties1   := TIBXServerProperties.Create(nil);
  IBXServerProperties1.ServicesConnection := IBXServicesConnection1;

finalization
  IBXServerProperties1.ServicesConnection := nil;
  IBXServerProperties1.Free;
  IBXServicesConnection1.Connected := false;
  IBXServicesConnection1.Free;
end.


