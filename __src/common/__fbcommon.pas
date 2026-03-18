unit fbcommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB, FBAdmin, IniFiles,
  fSetFBClient, Controls, Dialogs,
  ibase60dyn,

  //IBX
  IB,

  SQLDBLib;

type
  TRoutineType = (
    rtUDF, rtFBFunc, rtFBProc, rtUDRFunc, rtUDRProc,
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


function ReadODSFromFile(const ADatabaseFile: string; out ODSMajor, ODSMinor: Integer): Boolean;
function CompareODS(AFilePath1, AFilePath2: string): Boolean;
function ODSVersionToFBVersion(ODSMajor, ODSMinor: Integer): string;
function GetClientLibForODS(ODSMajor, ODSMinor: Integer): string;
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

var
    IBaseLibraryHandle : TLibHandle;
    IBaseLibraryName: string;

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

function CompareODS(AFilePath1, AFilePath2: string): Boolean;
var
  ODSMajor1, ODSMinor1: Integer;
  ODSMajor2, ODSMinor2: Integer;
begin
  Result := False;

  // ODS von beiden Dateien lesen
  if not ReadODSFromFile(AFilePath1, ODSMajor1, ODSMinor1) then
    Exit;
  if not ReadODSFromFile(AFilePath2, ODSMajor2, ODSMinor2) then
    Exit;

  // Vergleich: Major + Minor müssen identisch sein
  Result := (ODSMajor1 = ODSMajor2) and (ODSMinor1 = ODSMinor2);
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
    '2.5': FBDir := 'FirebirdCS-2.5.9.27139-0.amd64/lib/libfbclient.so.2.5.9';
    '3.0': FBDir := 'Firebird-3.0.13.33818-0.amd64/lib/libfbclient.so.3.0.13';
    '4.0': FBDir := 'Firebird-4.0.6.3221-0.amd64/lib/libfbclient.so.4.0.6';
    '5.0': FBDir := 'Firebird-5.0.3.1683-0-linux-x64/lib/libfbclient.so.5.0.3';
  else
    FBDir := '';
  end;
  {$ENDIF}

  if FBDir <> '' then
    Result := RootDir + FBDir;
end;

{function EnsureCorrectClientLib(const DatabasePath: string): Boolean;
var
  ODSMajor, ODSMinor: Integer;
  TargetLibPath: string;

begin
  //RegisteredDatabases[].RegRec.;
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
end; }

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
    if (IBaseLibraryHandle <> 0) and SameFileName(IBaseLibraryName, TargetLibPath) then
    begin
      Result := True;
      Exit;
    end;

    // 4) Alte Bibliothek entladen, falls geladen
    if IBaseLibraryHandle <> 0 then
    begin
      UnloadLibrary(IBaseLibraryHandle);
      IBaseLibraryHandle := 0;
      IBaseLibraryName := '';
    end;

    // 5) Neue Bibliothek laden
    IBaseLibraryHandle := LoadLibrary(PChar(TargetLibPath));
    if IBaseLibraryHandle = 0 then
      raise Exception.CreateFmt('Failed to load Firebird client library: %s', [TargetLibPath]);

    IBaseLibraryName := TargetLibPath;
    Result := True;

  except
    on E: Exception do
    begin
      // Im Fehlerfall alte Lib entladen, falls noch aktiv
      if IBaseLibraryHandle <> 0 then
      begin
        UnloadLibrary(IBaseLibraryHandle);
        IBaseLibraryHandle := 0;
        IBaseLibraryName := '';
      end;
      raise; // Exception weiterreichen
    end;
  end;
end;


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

  try
    // Alte Bibliothek entladen, falls zur Laufzeit gewechselt
    if (Sender = 1) and (IBaseLibraryHandle <> 0) then
    begin
      UnloadLibrary(IBaseLibraryHandle);
      IBaseLibraryHandle := 0;
      fbclibOld := '';
    end
    else
      fbclibOld := GetClientLibraryPath;

    fbclibNew := fbclibOld;

    // Erster Ladeversuch mit gespeicherter Bibliothek
    if fbclibOld <> '' then
      IBaseLibraryHandle := LoadLibrary(PChar(fbclibOld));

    // Falls Laden fehlschlägt → Dialog zur Auswahl neuer Bibliothek
    if IBaseLibraryHandle = 0 then
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

          if fbclibNew = '' then
            raise Exception.Create('No Firebird client library selected.');

          IBaseLibraryHandle := LoadLibrary(PChar(fbclibNew));
          if IBaseLibraryHandle = 0 then
            raise Exception.CreateFmt('Failed to load Firebird client library: %s', [fbclibNew]);

          // Pfad speichern, falls neu
          if fbclibNew <> fbclibOld then
          begin
            SaveClientLibraryPath(fbclibNew);
            SetFBConfFilePathFromClientLibPath(fbclibNew);
          end;
        end
        else
          raise Exception.Create('Firebird client library was not set. The application will now close.');
      finally
        frmSetFBClient.Free;
      end;
    end;

    // Alles ok
    IBaseLibraryName := fbclibNew;
    Result := True;

  except
    on E: Exception do
    begin
      ShowMessage('Error in SetFBClient: ' + E.Message);
      Result := False;
    end;
  end;
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

initialization
  IBaseLibraryHandle := 0;
  IBaseLibraryName       := '';

finalization

end.


