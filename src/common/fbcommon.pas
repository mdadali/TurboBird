unit fbcommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB, FBAdmin, IniFiles,
  fSetFBClient, Controls, Dialogs,
  ibase60dyn, IB, SQLDBLib;


type

TObjectType = (
  otNone,
  otTables,
  otGenerators,
  otTriggers,
  otViews,
  otStoredProcedures,
  otUDF {User-Defined functions},
  otSystemTables,
  otDomains {excludes system domains},
  otRoles,
  otExceptions,
  otUsers,
  otIndexes,
  otConstraints,
  otFBFunctions,
  otFBProcedures,
  otUDRFunctions,
  otUDRProcedures,
  otPackages,
  otPackageFunctions,
  otPackageProcedures,
  otPackageUDFFunctions,
  otPackageUDRFunctions,
  otPackageUDRProcedures
  );

  TRoutineType = (
    rtUDF,  rtFBProc, rtFBFunc,rtUDRFunc, rtUDRProc,
    rtPackageUDF, rtPackageFBFunc, rtPackageFBProc, rtPackageUDRFunc, rtPackageUDRProc,
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

const

  //NumObjects = 13; //number of different objects in dbObjects array below
  //newlib
  NumObjects = 24;
  //end-newlib
  dbObjects: array [0 .. NumObjects-1] of string =
    ('None', 'Tables', 'Generators', 'Triggers',
    'Views', 'Stored Procedures', 'UDFs',
    'Sys Tables', 'Domains', 'Roles',
    'Exceptions', 'Users', 'Indices',
    'Constraints', 'FBFunctions', 'FBProcedures',
    'UDRFunctions', 'UDRProcedures',
    'Packages', 'PackageFunctions', 'PackageProcedures',
    'PackageUDFFunctions', 'PackageUDRFunctions', 'PackageUDRProcedures');


function ReadODSFromFile(const ADatabaseFile: string; out ODSMajor, ODSMinor: Integer): Boolean;
function ODSVersionToFBVersion(ODSMajor, ODSMinor: Integer): string;
function GetClientLibForODS(ODSMajor, ODSMinor: Integer): string;
function GetFBConffilePathFromFBClientlibPath(const ALibPath: string): string;

function EnsureCorrectClientLib(const DatabasePath: string): Boolean;

function SetFBClient(Sender: word): boolean;

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
function GetObjectOwner(AConnection: TIBConnection; const ObjectName: string; ObjType: TObjectType): string;

var
    SQLDBLibraryLoader1: TSQLDBLibraryLoader;
    ClientLibraryName: string;
    Port: string;

    FBVersionString: string;
    FBVersionMajor: Integer = 0;
    FBVersionMinor: Integer = 0;
    FBVersionNumber: single = 0.0;


implementation

uses turbocommon;

function ReadODSFromFile(const ADatabaseFile: string; out ODSMajor, ODSMinor: Integer): Boolean;
var
  fs: TFileStream;
  buf: array[0..63] of Byte;  // etwas mehr Puffer
  FileName: string;
  p: Integer;
begin
  Result := False;
  ODSMajor := 0;
  ODSMinor := 0;

  // 1) Serverangabe aus dem Pfad entfernen, falls vorhanden
  FileName := ADatabaseFile;
  p := LastDelimiter(':', FileName);
  if (p > 0) and (p < Length(FileName)) then
    FileName := Copy(FileName, p + 1, MaxInt);

  // 2) Datei existieren prüfen
  if not FileExists(FileName) then
    Exit;

  // 3) Datei öffnen & ersten Teil lesen
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    // Mindestens so viele Bytes wie der Header benötigt
    if fs.Read(buf, SizeOf(buf)) < 22 then
      Exit;  // nicht genug Bytes

    // 4) ODS Werte lesen: gemäß IB/Firebird Header Page
    // Offsets basierend auf Dokumentation:
    // ODS Major: Word an Offset 18 (Bytes 18-19)
    // ODS Minor: Word an Offset 20 (Bytes 20-21)

    ODSMajor := PWord(@buf[18])^;
    ODSMinor := PWord(@buf[20])^;

    // 5) Für FB >=3: oft ist ODSMajor "groß" (z. B. 32781) → ist wahrscheinlich Major + 32768?
    // Das hängt von deiner bisherigen Beobachtung. Wenn du regelmäßig 32781 siehst für FB4/5 etc.,
    // dann ziehen wir diesen Bias ab:

    if ODSMajor >= 32768 then
      ODSMajor := ODSMajor - 32768;

    Result := True;
  finally
    fs.Free;
  end;
end;

function ODSVersionToFBVersion(ODSMajor, ODSMinor: Integer): string;
begin
  //ShowMessage(IntToStr(ODSMajor) + '.' + IntToStr(ODSMinor));
  case ODSMajor of
    11:
      Result := '2.5';  // ODS 11.x
    12:
      Result := '3.0';  // ODS 12.x
    13:
      begin
        case ODSMinor of
          0: Result := '4.0';  // ODS 13.0 → Firebird 4.0
          1: Result := '5.0';  // ODS 13.1 → Firebird 5.0 initial
          2: Result := '5.0';  // ODS 13.2 → Firebird 5.0.x
          3: Result := '5.0';  // ODS 13.3 → Firebird 5.0.3 ff.
        else
          Result := Format('Firebird (ODS %d.%d, unbekannt)', [ODSMajor, ODSMinor]);
        end;
      end;
  else
    Result := Format('Unbekannte Firebird-Version (ODS %d.%d)', [ODSMajor, ODSMinor]);
  end;
end;

function GetClientLibForODS(ODSMajor, ODSMinor: Integer): string;
var
  RootDir, FBVersion, FBDir: string;
begin
  Result := '';

  // Root der mitgelieferten FB-Verzeichnisse
  RootDir := ExtractFilePath(ParamStr(0)) + 'system' + DirectorySeparator + 'firebird' + DirectorySeparator;

  // Mapping ODS → Firebird-Version (z.B. '2.5', '3.0', '4.0', '5.0')
  FBVersion := ODSVersionToFBVersion(ODSMajor, ODSMinor);

  if FBVersion = '' then
    Exit; // unbekannte ODS-Version

  {$IFDEF Windows}
  case FBVersion of
    '2.5': FBDir := 'FirebirdCS-2.5.9.27139-0.amd64\fbclient.dll'; // Classic als Default
    '3.0': FBDir := 'Firebird-3.0.13.33818-0.amd64\fbclient.dll';
    '4.0': FBDir := 'Firebird-4.0.6.3221-0.amd64\fbclient.dll';
    '5.0': FBDir := 'Firebird-5.0.3.1683-0-linux-x64\fbclient.dll';
  else
    FBDir := '';
  end;
  {$ELSE}
  case FBVersion of
    '2.5': FBDir := fIniFile.ReadString('Firebird', 'ClientLib25', '');
    '3.0': FBDir := fIniFile.ReadString('Firebird', 'ClientLib3', '');
    '4.0': FBDir := fIniFile.ReadString('Firebird', 'ClientLib4', '');
    '5.0': FBDir := fIniFile.ReadString('Firebird', 'ClientLib5', '');
  else
    FBDir := '';
  end;
  {$ENDIF}

  if FBDir <> '' then
    //Result := RootDir + FBDir;
    Result := FBDir;
end;

function EnsureCorrectClientLib(const DatabasePath: string): Boolean;
var
  ODSMajor, ODSMinor: Integer;
  TargetLibPath: string;

begin
  Result := False;
  try
    // 1) ODS aus DB-File lesen
    if not ReadODSFromFile(DatabasePath, ODSMajor, ODSMinor) then
      Exit;

    // 2) Richtige ClientLib anhand ODS ermitteln
    TargetLibPath := GetClientLibForODS(ODSMajor, ODSMinor);
    if TargetLibPath = '' then
      raise Exception.CreateFmt('No client library found for ODS %d.%d', [ODSMajor, ODSMinor]);

    // 3) Prüfen ob bereits die richtige Lib geladen ist
    if SQLDBLibraryLoader1.Enabled and SameFileName(SQLDBLibraryLoader1.LibraryName, TargetLibPath) then
    begin
      Result := True;
      Exit;
    end;

    // 4) Bibliothek wechseln
    if SQLDBLibraryLoader1.Enabled then
      SQLDBLibraryLoader1.UnloadLibrary;

    SQLDBLibraryLoader1.LibraryName := TargetLibPath;
    SQLDBLibraryLoader1.LoadLibrary;
    SQLDBLibraryLoader1.Enabled := True;

    Result := True;
  except
    on E: Exception do
    begin
      // Im Fehlerfall entladen
      if SQLDBLibraryLoader1.Enabled then
        SQLDBLibraryLoader1.UnloadLibrary;
      raise;
    end;
  end;
end;

function NormalizeFloatForSQL(Value: Double): string;
begin
  Result := FloatToStrF(Value, ffGeneral, 15, 0);
  // Sicherheitshalber: ersetze evtl. Komma durch Punkt
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
end;


procedure SaveClientLibraryPath(const path: string);
begin
  fIniFile.WriteString('Firebird', 'ClientLib', path);
end;

function GetFBConffilePathFromFBClientlibPath(const ALibPath: string): string;
var
  RootDir: string;
begin
  Result := '';
  if ALibPath = '' then
    Exit;

  // Verzeichnis vom Libpfad nehmen
  RootDir := ExtractFileDir(ALibPath);

  // Manche Installationen haben fbclient in ...\lib, andere in ...\bin
  // Also zwei Ebenen zurück probieren
  if SameText(ExtractFileName(RootDir), 'lib') or
     SameText(ExtractFileName(RootDir), 'bin') then
    RootDir := ExtractFileDir(RootDir);

  Result := IncludeTrailingPathDelimiter(RootDir) + 'firebird.conf';

  // Optional: prüfen ob Datei existiert, sonst leeren String zurückgeben
  if not FileExists(Result) then
    Result := '';
end;

procedure SetFBConfFilePathFromClientLibPath(AFBClientLibPath: string);
var FBConfFilePath: string;
begin
  FBConfFilePath := GetFBConffilePathFromFBClientlibPath(AFBClientLibPath);

  if (FBConfFilePath <> '') and FileExists(FBConfFilePath) then
    fIniFile.WriteString('FireBird', 'ConfPath', FBConfFilePath)
  else begin
    if MessageDlg('The file "firebird.conf" could not be found.' + sLineBreak +
                  'Do you want to locate it manually?', mtConfirmation,
                  [mbYes, mbNo], 0) = mrYes then
    begin
      with TOpenDialog.Create(nil) do
      try
        Title := 'Select firebird.conf';
        Filter := 'Firebird Config|firebird.conf|All Files|*.*';
        Options := [ofFileMustExist];
        if Execute then
        begin
          FBConfFilePath := FileName;
          fIniFile.WriteString('FireBird', 'ConfPath', FBConfFilePath);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

function GetClientLibraryPath: string;
begin
  ClientLibraryName := fIniFile.ReadString('Firebird', 'ClientLib', '');
  Port := fIniFile.ReadString('Firebird', 'Port', '');
  Result := ClientLibraryName;
end;

function SetFBClient(Sender: Word): Boolean;
var
  fbclibOld, fbclibNew: string;
  caption0, caption1: string;
begin
  Result := False;

  caption0 :=
    'FireBird client in ini-file does not exist or is invalid.' + sLineBreak +
    'Please select a version from the list below or' + sLineBreak +
    'use the "Browse" button to choose a valid version.' + sLineBreak +
    'Then test your selection with the "Test" button.';
  caption1 := 'Select a Firebird client library from the list below.';

  // Laufzeitwechsel: alte DLL entladen
  if Sender = 1 then
  begin
    if SQLDBLibraryLoader1.Enabled then
    begin
      SQLDBLibraryLoader1.UnloadLibrary;
      SQLDBLibraryLoader1.Enabled := false;
    end;
    fbclibOld := '';
  end
  else
  begin
    fbclibOld := GetClientLibraryPath;
  end;

  fbclibNew := fbclibOld;

  // Versuch, vorhandene DLL zu laden
  if fbclibOld <> '' then
  begin
    SQLDBLibraryLoader1.LibraryName := fbclibOld;
    try
      SQLDBLibraryLoader1.LoadLibrary;
    except
      //SQLDBLibraryLoader1.UnloadLibrary;
      SQLDBLibraryLoader1.Enabled := false;
    end;
  end;

  // Falls Laden fehlschlägt → Dialog öffnen
  if not SQLDBLibraryLoader1.Enabled then
  begin
    frmSetFBClient := TfrmSetFBClient.Create(nil);
    try
      if Sender = 0 then
        frmSetFBClient.lblInfo.Caption := caption0
      else
        frmSetFBClient.lblInfo.Caption := caption1;

      if frmSetFBClient.ShowModal = mrOk then
      begin
        fbclibNew := frmSetFBClient.FBClientLibPath;
        SQLDBLibraryLoader1.LibraryName := fbclibNew;

        try
          SQLDBLibraryLoader1.LoadLibrary;
        except
          on E: Exception do
          begin
            ShowMessage('Error while loading the selected Firebird client library:' +
                        sLineBreak + fbclibNew + sLineBreak + E.Message);
            Exit;
          end;
        end;

        // Pfad speichern, falls neu
        if fbclibNew <> fbclibOld then
        begin
          SaveClientLibraryPath(fbclibNew);
          SetFBConfFilePathFromClientLibPath(fbclibNew);
        end;

      end
      else
      begin
        ShowMessage('Firebird client library was not set. The application will now close.');
        Exit;
      end;
    finally
      frmSetFBClient.Free;
    end;
  end;
  Result := True;
end;


function RoutineTypeToStr(ARoutineType: TRoutineType): string;
begin
  case ARoutineType of
    rtUDF:              Result := 'UDF';
    rtFBFunc:           Result := 'FireBird Function';
    rtFBProc:           Result := 'FireBird Stored Procedure';
    rtUDRFunc:          Result := 'UDR Function';
    rtUDRProc:          Result := 'UDR Procedure';
    rtPackageUDF:       Result := 'Package UDF';
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
  else if (S = 'FUNCTIONS') or (S = 'RTFBFUNC') or (S = 'FIREBIRD FUNCTION') or (S = 'FBFUNC') then
    Result := rtFBFunc
  else if (S = 'PROCEDURES') or (S = 'RTFBPROC') or (S = 'FIREBIRD STORED PROCEDURE') or (S = 'FBPROC') then
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
    rtUDF: begin

      Result :=
        'SELECT ' +
        'RDB$FUNCTION_NAME AS NAME, ' +
        '  ''rtUDF'' AS ROUTINE_TYPE ' +
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$MODULE_NAME IS NOT NULL ';

        if FBVersionMajor >= 3 then
        begin
          Result := Result +
            'AND RDB$ENGINE_NAME IS NULL ' +
            'AND RDB$PACKAGE_NAME IS NULL ';
        end;
        Result := Result +
          'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$FUNCTION_NAME';
    end;

    rtFBFunc:
      Result :=
        'SELECT RDB$FUNCTION_NAME AS NAME, ' +
        '  ''rtFBFunc'' AS ROUTINE_TYPE ' +      //synthetic field
        'FROM RDB$FUNCTIONS ' +
        'WHERE RDB$ENGINE_NAME IS NULL AND RDB$PACKAGE_NAME IS NULL ' +
        'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
        'ORDER BY RDB$FUNCTION_NAME';

    rtFBProc:
     begin
       if FBVersionMajor >= 3 then
         Result :=
          'SELECT RDB$PROCEDURE_NAME AS NAME ' +
          'FROM RDB$PROCEDURES ' +
          'WHERE RDB$PACKAGE_NAME IS NULL ' +
          'AND (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$PROCEDURE_NAME'
       else
         Result :=
          'SELECT RDB$PROCEDURE_NAME AS NAME ' +
          'FROM RDB$PROCEDURES ' +
          'WHERE (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0) ' +
          'ORDER BY RDB$PROCEDURE_NAME';
     end;

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
  if FBVersionMajor >= 3 then
  begin
    IsFunction := info.RoutineType in [rtUDF, rtFBFunc, rtUDRFunc, rtPackageFBFunc, rtPackageUDRFunc];
    IsUDR := info.RoutineType in [rtUDRFunc, rtPackageUDRFunc, rtUDRProc, rtPackageUDRProc];
    IsPackage := info.RoutineType in [rtPackageFBFunc, rtPackageUDRFunc, rtPackageFBProc, rtPackageUDRProc];
  end else
  begin
    IsFunction := false;
    IsUDR :=      false;
    IsPackage :=  false;
  end;
  // --- Sonderfall UDF ---
  if info.RoutineType = rtUDF then
  begin
    if FBVersionMajor >= 3 then
    begin
      // Firebird 3+: volle Query mit FIELD_SOURCE und CHARACTER_LENGTH
      Result :=
        'SELECT' + LineEnding +
        '  CASE' + LineEnding +
        '    WHEN fa.RDB$ARGUMENT_POSITION = 0 THEN ''RETURN''' + LineEnding +
        '    ELSE ''ARG_'' || fa.RDB$ARGUMENT_POSITION' + LineEnding +
        '  END AS PARAM_NAME,' + LineEnding +
        '  fa.RDB$FIELD_SOURCE AS DATATYPE_SOURCE, ' +
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
    end
    else
    begin
      // Firebird 2.5: FIELD_SOURCE und CHARACTER_LENGTH existieren nicht
      Result :=
        'SELECT' + LineEnding +
        '  CASE' + LineEnding +
        '    WHEN fa.RDB$ARGUMENT_POSITION = 0 THEN ''RETURN''' + LineEnding +
        '    ELSE ''ARG_'' || fa.RDB$ARGUMENT_POSITION' + LineEnding +
        '  END AS PARAM_NAME,' + LineEnding +
        '  NULL AS DATATYPE_SOURCE,' + LineEnding +
        '  fa.RDB$FIELD_TYPE,' + LineEnding +
        '  fa.RDB$FIELD_SUB_TYPE,' + LineEnding +
        '  fa.RDB$FIELD_LENGTH,' + LineEnding +
        '  fa.RDB$FIELD_PRECISION,' + LineEnding +
        '  fa.RDB$FIELD_SCALE,' + LineEnding +
        '  fa.RDB$CHARACTER_SET_ID,' + LineEnding +
        '  fa.RDB$ARGUMENT_POSITION,' + LineEnding +
        '  NULL AS RDB$PACKAGE_NAME,' + LineEnding +
        '  f.RDB$FUNCTION_NAME,' + LineEnding +
        '  NULL AS FIELD_SOURCE,' + LineEnding +
        '  NULL AS ACHARACTER_LENGTH' + LineEnding +
        'FROM RDB$FUNCTION_ARGUMENTS fa' + LineEnding +
        'JOIN RDB$FUNCTIONS f ON f.RDB$FUNCTION_NAME = fa.RDB$FUNCTION_NAME' + LineEnding +
        'WHERE TRIM(UPPER(f.RDB$FUNCTION_NAME)) = :FUNCNAME' + LineEnding +
        'ORDER BY fa.RDB$ARGUMENT_POSITION';
    end;
    Exit; // Wichtig: verlässt die Funktion nach dem UDF-Block
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

    if FBVersionMajor >= 3 then
    begin
      if IsPackage then
        Result := Result + 'AND P.RDB$PACKAGE_NAME = :PACKAGENAME '
      else
        Result := Result + 'AND P.RDB$PACKAGE_NAME IS NULL ';
    end;

    {if info.RoutineType in [rtFBProc, rtPackageFBProc] then
      Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME AND PR.RDB$ENGINE_NAME IS NULL) '
    else if info.RoutineType in [rtUDRProc, rtPackageUDRProc] then
      Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME AND PR.RDB$ENGINE_NAME IS NOT NULL) ';
    }

    if info.RoutineType in [rtFBProc, rtPackageFBProc] then
      if FBVersionMajor >= 3 then
        Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME AND PR.RDB$ENGINE_NAME IS NULL) '
      else
        Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME) '
    else
      if info.RoutineType in [rtUDRProc, rtPackageUDRProc] then
        if FBVersionMajor >= 3 then
          Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME AND PR.RDB$ENGINE_NAME IS NOT NULL) '
        else
          Result := Result + 'AND EXISTS (SELECT 1 FROM RDB$PROCEDURES PR WHERE PR.RDB$PROCEDURE_NAME = P.RDB$PROCEDURE_NAME) ';

    //Filter nach Parametertyp
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

function GetObjectOwner(AConnection: TIBConnection; const ObjectName: string; ObjType: TObjectType): string;
var
  qry: TSQLQuery;
  SQLText, ParamName: string;
begin
  Result := '';
  qry := TSQLQuery.Create(nil);
  try
    qry.DataBase := AConnection;

    case ObjType of
      otTables, otSystemTables:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = :NAME';
        end;

      otViews:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = :NAME';
        end;

      otStoredProcedures, otFBProcedures, otUDRProcedures, otPackageProcedures, otPackageUDRProcedures:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = :NAME';
        end;

      otTriggers:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$TRIGGERS WHERE RDB$TRIGGER_NAME = :NAME';
        end;

      otDomains:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$FIELDS WHERE RDB$FIELD_NAME = :NAME';
        end;

      otGenerators:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME = :NAME';
        end;

      otRoles:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$ROLES WHERE RDB$ROLE_NAME = :NAME';
        end;

      otExceptions:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$EXCEPTIONS WHERE RDB$EXCEPTION_NAME = :NAME';
        end;

      otIndexes:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$INDICES WHERE RDB$INDEX_NAME = :NAME';
        end;

      otPackages, otPackageFunctions, otPackageUDFFunctions, otPackageUDRFunctions:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$PACKAGES WHERE RDB$PACKAGE_NAME = :NAME';
        end;

      otFBFunctions, otUDF, otUDRFunctions:
        begin
          SQLText := 'SELECT RDB$OWNER_NAME FROM RDB$FUNCTIONS WHERE RDB$FUNCTION_NAME = :NAME';
        end;

      else
        Exit; // Für Objekte ohne Owner
    end;

    qry.SQL.Text := SQLText;
    qry.Params[0].AsString := UpperCase(ObjectName);
    qry.Open;
    if not qry.EOF then
      Result := Trim(qry.Fields[0].AsString);
  finally
    qry.Free;
  end;
end;

initialization
  SQLDBLibraryLoader1 := TSQLDBLibraryLoader.Create(nil);
  SQLDBLibraryLoader1.ConnectionType := 'Firebird';

finalization
  if SQLDBLibraryLoader1.Enabled then
    SQLDBLibraryLoader1.UnloadLibrary;
  SQLDBLibraryLoader1.Free;
  SQLDBLibraryLoader1 := nil;

end.


