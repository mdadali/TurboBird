unit udb_package_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, Dialogs,
  udb_udf_fetcher,

  udb_udr_func_fetcher,
  udb_udr_proc_fetcher,

  udb_firebird_func_fetcher,
  udb_firebird_proc_fetcher,

  udb_package_firebird_func_fetcher,
  udb_package_firebird_proc_fetcher;

function GetPackageHeader(Conn: TIBConnection; const APackageName: string): string;
function GetPackageBody(Conn: TIBConnection; const APackageName: string): string;
function GetPackageDeclaration(Conn: TIBConnection; const APackageName: string): string;

// Firebird PSQL Function wrappers
function GetPackageFireBirdFunctionHeader(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
function GetPackageFireBirdFunctionBody(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
function GetPackageFireBirdFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;

// Firebird PSQL Procedure wrappers
function GetPackageFireBirdProcedureHeader(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
function GetPackageFireBirdProcedureBody(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
function GetPackageFireBirdProcedureDeclaration(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;

// UDR Function wrappers
function GetPackageUDRFunctionHeader(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
function GetPackageUDRFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;

// UDR Procedure wrappers
function GetPackageUDRProcedureHeader(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
function GetPackageUDRProcedureDeclaration(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;

// UDF wrappers
function GetPackageUDFHeader(Conn: TIBConnection; const UDFName: string; const APackageName: string): string;
function GetPackageUDFDeclaration(Conn: TIBConnection; const UDFName: string; const APackageName: string): string;

implementation

function GetAllPackageFireBirdFunctionNames(Conn: TIBConnection; const APackageName: string): TStringList;
begin
  Result := TStringList.Create;
  with TSQLQuery.Create(nil) do
  try
    Database := Conn;
    SQL.Text :=
      'SELECT RDB$FUNCTION_NAME FROM RDB$FUNCTIONS ' +
      'WHERE ' +
      'UPPER(RDB$PACKAGE_NAME) = :PKG  ' +
      'AND ' +
      'RDB$ENGINE_NAME IS NULL ' +
      'AND ' +
      'RDB$MODULE_NAME IS NULL '  +
      'ORDER BY RDB$FUNCTION_NAME';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    Open;
    while not EOF do
    begin
      Result.Add(Trim(FieldByName('RDB$FUNCTION_NAME').AsString));
      Next;
    end;
  finally
    Free;
  end;
end;

function GetAllPackageFireBirdProcedureNames(Conn: TIBConnection; const APackageName: string): TStringList;
begin
  Result := TStringList.Create;
  with TSQLQuery.Create(nil) do
  try
    Database := Conn;
    SQL.Text :=
    'SELECT RDB$PROCEDURE_NAME FROM RDB$PROCEDURES ' +
    'WHERE ' +
    'UPPER(RDB$PACKAGE_NAME) = :PKG  ' +
    'AND ' +
    'RDB$ENGINE_NAME IS NULL ' +
    'ORDER BY RDB$PROCEDURE_NAME';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    Open;
    while not EOF do
    begin
      Result.Add(Trim(FieldByName('RDB$PROCEDURE_NAME').AsString));
      Next;
    end;
  finally
    Free;
  end;
end;

function GetAllPackageUDRFunctionNames(Conn: TIBConnection; const APackageName: string): TStringList;
begin
  Result := TStringList.Create;
  with TSQLQuery.Create(nil) do
  try
    Database := Conn;
    SQL.Text :=
      'SELECT RDB$FUNCTION_NAME FROM RDB$FUNCTIONS ' +
      'WHERE RDB$ENGINE_NAME IS NOT NULL AND UPPER(RDB$PACKAGE_NAME) = :PKG ' +
      'ORDER BY RDB$FUNCTION_NAME';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    Open;
    while not EOF do
    begin
      Result.Add(Trim(FieldByName('RDB$FUNCTION_NAME').AsString));
      Next;
    end;
  finally
    Free;
  end;
end;

function GetAllPackageUDRProcedureNames(Conn: TIBConnection; const APackageName: string): TStringList;
begin
  Result := TStringList.Create;
  with TSQLQuery.Create(nil) do
  try
    Database := Conn;
    SQL.Text :=
      'SELECT RDB$PROCEDURE_NAME FROM RDB$PROCEDURES ' +
       'WHERE RDB$ENGINE_NAME IS NOT NULL AND UPPER(RDB$PACKAGE_NAME) = :PKG ' +
      'ORDER BY RDB$PROCEDURE_NAME';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    Open;
    while not EOF do
    begin
      Result.Add(Trim(FieldByName('RDB$PROCEDURE_NAME').AsString));
      Next;
    end;
  finally
    Free;
  end;
end;

function GetAllPackageUDFFunctionNames(Conn: TIBConnection; const APackageName: string): TStringList;
begin
  Result := TStringList.Create;
  with TSQLQuery.Create(nil) do
  try
    Database := Conn;
    SQL.Text :=
      'SELECT RDB$FUNCTION_NAME FROM RDB$FUNCTIONS ' +
      'WHERE RDB$MODULE_NAME IS NOT NULL AND UPPER(RDB$PACKAGE_NAME) = :PKG ' +
      'ORDER BY RDB$FUNCTION_NAME';

    if not Conn.DefaultTransaction.InTransaction then
      Conn.DefaultTransaction.StartTransaction;

    Params.ParamByName('PKG').AsString := UpperCase(APackageName);
    Open;
    while not EOF do
    begin
      Result.Add(Trim(FieldByName('RDB$FUNCTION_NAME').AsString));
      Next;
    end;
  finally
    Free;
  end;
end;

{ Utility to get all function names in a package }
function GetAllPackageFunctionNames(Conn: TIBConnection; const APackageName: string): TStringList;
var
  ListNative, ListUDR, ListUDF: TStringList;
begin
  Result := TStringList.Create;
  ListNative := GetAllPackageFireBirdFunctionNames(Conn, APackageName);
  ListUDR := GetAllPackageUDRFunctionNames(Conn, APackageName);
  ListUDF := GetAllPackageUDFFunctionNames(Conn, APackageName);
  try
    Result.AddStrings(ListNative);
    Result.AddStrings(ListUDR);
    Result.AddStrings(ListUDF);
  finally
    ListNative.Free;
    ListUDR.Free;
    ListUDF.Free;
  end;
end;

{ Utility to get all procedure names in a package }
function GetAllPackageProcedureNames(Conn: TIBConnection; const APackageName: string): TStringList;
var
  ListFireBird, ListUDR: TStringList;
begin
  Result := TStringList.Create;
  ListFireBird := GetAllPackageFireBirdProcedureNames(Conn, APackageName);
  ListUDR := GetAllPackageUDRProcedureNames(Conn, APackageName);
  try
    Result.AddStrings(ListFireBird);
    Result.AddStrings(ListUDR);
  finally
    ListFireBird.Free;
    ListUDR.Free;
  end;
end;

// ================== Firebird PSQL Function Wrappers ==================

function GetPackageFireBirdFunctionHeader(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
var
  Header: string;
begin
  Header := GetFirebirdFunctionHeader(Conn, FunctionName, APackageName);
  Header := StringReplace(Header, 'CREATE OR ALTER', '', [rfReplaceAll, rfIgnoreCase]);
  Header := Trim(Header); // Entfernt überflüssige Leerzeichen am Anfang/Ende
  Result := Header + ';';
end;

function GetPackageFireBirdFunctionBody(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
begin
  Result := GetFirebirdFunctionBody(Conn, FunctionName, APackageName);
end;

function GetPackageFireBirdFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
var BodyText, Header: string;
begin
  Header := GetPackageFireBirdFunctionHeader(Conn, FunctionName, APackageName);
  BodyText := GetPackageFirebirdFunctionBody(Conn, FunctionName, APackageName);
  Result := Header + sLineBreak + BodyText;
end;

// ================== Firebird PSQL Procedure Wrappers ==================

function GetPackageFireBirdProcedureHeader(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
var
  Header: string;
begin
  Header := GetFirebirdProcedureHeader(Conn, ProcedureName, APackageName);
  Header := StringReplace(Header, 'CREATE OR ALTER', '', [rfReplaceAll, rfIgnoreCase]);
  Header := Trim(Header); // Entfernt überflüssige Leerzeichen am Anfang/Ende
  Result := Header + ';';
end;

function GetPackageFireBirdProcedureBody(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
begin
  Result := GetFirebirdProcedureBody(Conn, ProcedureName, APackageName);
end;

function GetPackageFireBirdProcedureDeclaration(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
var BodyText, Header: string;
begin
  Header := GetPackageFireBirdProcedureHeader(Conn, ProcedureName, APackageName);
  BodyText := GetPackageFirebirdProcedureBody(Conn, ProcedureName, APackageName);
  Result := Header + sLineBreak + BodyText;
end;

// ================== UDR Function Wrappers ==================

function GetPackageUDRFunctionHeader(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
var Header: string;
begin
  Header := GetUDRFunctionHeader(Conn, FunctionName, APackageName);
  Header := StringReplace(Header, 'CREATE OR ALTER', '', [rfReplaceAll, rfIgnoreCase]);
  Header := Trim(Header); // Entfernt überflüssige Leerzeichen am Anfang/Ende

  Result := Header + ';';
end;

function GetPackageUDRFunctionDeclaration(Conn: TIBConnection; const FunctionName: string; const APackageName: string): string;
begin
  Result := GetUDRFunctionDeclaration(Conn, FunctionName, APackageName) + ';';
end;

// ================== UDR Procedure Wrappers ==================

function GetPackageUDRProcedureHeader(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
var Header: string;
begin
  Header := GetUDRProcedureHeader(Conn, ProcedureName, APackageName);
  Header := StringReplace(Header, 'CREATE OR ALTER', '', [rfReplaceAll, rfIgnoreCase]);
  Header := Trim(Header); // Entfernt überflüssige Leerzeichen am Anfang/Ende
  Result := Header + ';';
end;

function GetPackageUDRProcedureDeclaration(Conn: TIBConnection; const ProcedureName: string; const APackageName: string): string;
begin
  Result := GetUDRProcedureDeclaration(Conn, ProcedureName, APackageName) + ';';
end;

// ================== UDF Wrappers ==================

function GetPackageUDFHeader(Conn: TIBConnection; const UDFName: string; const APackageName: string): string;
var Header: string;
begin
  Header := GetUDFFunctionHeader(Conn, UDFName, APackageName);
  Header := StringReplace(Header, 'CREATE OR ALTER', '', [rfReplaceAll, rfIgnoreCase]);
  Header := Trim(Header); // Entfernt überflüssige Leerzeichen am Anfang/Ende
  Result := Result + Header + ';';
end;

function GetPackageUDFDeclaration(Conn: TIBConnection; const UDFName: string; const APackageName: string): string;
begin
  Result := GetUDFFunctionDeclaration(Conn, UDFName, APackageName) + ';';
end;

// ================== Hauptfunktionen ==================

function GetPackageHeader(Conn: TIBConnection; const APackageName: string): string;
var
  Routines: TStringList;
  I: Integer;
  S: TStringList;
begin
  //FuncNames := TStringList.Create;

  S := TStringList.Create;
  try
    S.Add('CREATE OR ALTER PACKAGE ' + APackageName + ' AS');
    S.Add('BEGIN');

    Routines := GetAllPackageFireBirdFunctionNames(Conn, APackageName);
    for I := 0 to Routines.Count - 1 do
      S.Add(udb_package_firebird_func_fetcher.GetPackageFireBirdFunctionHeader(Conn, Routines[I], APackageName));
    Routines.Free;

    Routines := GetAllPackageFireBirdProcedureNames(Conn, APackageName);
    for I := 0 to Routines.Count - 1 do
      S.Add(udb_package_firebird_proc_fetcher.GetPackageFireBirdProcedureHeader(Conn, Routines[I], APackageName));
    Routines.Free;

    {Routines := GetAllPackageUDRFunctionNames(Conn, APackageName);
    for I := 0 to Routines.Count - 1 do
      S.Add(GetPackageUDRFunctionHeader(Conn, Routines[I], APackageName));
    Routines.Free;

    Routines := GetAllPackageUDRProcedureNames(Conn, APackageName);
    for I := 0 to Routines.Count - 1 do
      S.Add(GetPackageUDRProcedureHeader(Conn, Routines[I], APackageName));
    Routines.Free;
    }
    S.Add('END;');
    Result := S.Text;
  finally
    S.Free;
  end;
end;

function GetPackageBody(Conn: TIBConnection; const APackageName: string): string;
var
  FuncNames, ProcNames: TStringList;
  I: Integer;
  S: TStringList;
begin
  FuncNames := GetAllPackageFunctionNames(Conn, APackageName);
  ProcNames := GetAllPackageProcedureNames(Conn, APackageName);
  S := TStringList.Create;
  try
    S.Add('CREATE OR ALTER PACKAGE BODY ' + APackageName + ' AS');
    S.Add('');

    for I := 0 to FuncNames.Count - 1 do
      S.Add(GetPackageFireBirdFunctionDeclaration(Conn, FuncNames[I], APackageName) + sLineBreak);
      //S.Add(GetPackageFireBirdFunctionBody(Conn, FuncNames[I], APackageName) + sLineBreak);

    for I := 0 to ProcNames.Count - 1 do
       S.Add(GetPackageFireBirdProcedureDeclaration(Conn, ProcNames[I], APackageName) + sLineBreak);
      //S.Add(GetPackageFireBirdProcedureBody(Conn, ProcNames[I], APackageName) + sLineBreak);

    // UDR / UDF bodies falls nötig hier ergänzen

    S.Add('END;');
    Result := S.Text;
  finally
    FuncNames.Free;
    ProcNames.Free;
    S.Free;
  end;
end;

function GetPackageDeclaration(Conn: TIBConnection; const APackageName: string): string;
begin
  // Alias zu Header
  Result := GetPackageHeader(Conn, APackageName) + sLineBreak +  GetPackageBody(Conn, APackageName)
end;

end.

