unit fb_proc_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetFBProcedureDeps(const AConn: TIBDatabase; const AProcName, APackageName: string): string;

implementation

function GetFBProcedureDeps(const AConn: TIBDatabase; const AProcName, APackageName: string): string;
var
  Q: TIBQuery;
  ProcNameFull, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  exit;

  if AProcName = '' then Exit;

  // Vollständiger Prozedurname, abhängig vom Package
  if APackageName <> '' then
    ProcNameFull := UpperCase(Trim(APackageName) + '.' + Trim(AProcName))
  else
    ProcNameFull := UpperCase(Trim(AProcName));

  Q := TIBQuery.Create(nil);
  Q.AllowAutoActivateTransaction := true;  
  
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    SetLength(ObjectTypes, 8);

    // Prozeduren
    ObjectTypes[0].Name := 'Procedures';
    ObjectTypes[0].ObjType := 'PROCEDURE';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + ProcNameFull + ''' ' +
      'AND (RDB$PACKAGE_NAME IS NULL OR RDB$PACKAGE_NAME <> ''' + UpperCase(APackageName) + ''')';

    // Funktionen
    ObjectTypes[1].Name := 'Functions';
    ObjectTypes[1].ObjType := 'FUNCTION';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$FUNCTION_NAME AS OBJ_NAME FROM RDB$FUNCTIONS ' +
      'WHERE UPPER(COALESCE(RDB$FUNCTION_SOURCE, '''')) CONTAINING ''' + ProcNameFull + '''';

    // Trigger
    ObjectTypes[2].Name := 'Triggers';
    ObjectTypes[2].ObjType := 'TRIGGER';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + ProcNameFull + '''';

    // Views
    ObjectTypes[3].Name := 'Views';
    ObjectTypes[3].ObjType := 'VIEW';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$RELATION_NAME AS OBJ_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_TYPE = 1 ' +
      'AND UPPER(COALESCE(RDB$VIEW_SOURCE, '''')) CONTAINING ''' + ProcNameFull + '''';

    // Package Header
    ObjectTypes[4].Name := 'Package Headers';
    ObjectTypes[4].ObjType := 'PACKAGE_HDR';
    ObjectTypes[4].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + ProcNameFull + '''';

    // Package Body
    ObjectTypes[5].Name := 'Package Bodies';
    ObjectTypes[5].ObjType := 'PACKAGE_BODY';
    ObjectTypes[5].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + ProcNameFull + '''';

    // Computed Fields
    ObjectTypes[6].Name := 'Computed Fields';
    ObjectTypes[6].ObjType := 'FIELD';
    ObjectTypes[6].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$COMPUTED_SOURCE, '''')) CONTAINING ''' + ProcNameFull + '''';

    // Constraints
    ObjectTypes[7].Name := 'Check Constraints';
    ObjectTypes[7].ObjType := 'CONSTRAINT';
    ObjectTypes[7].SQLText :=
      'SELECT RDB$CONSTRAINT_NAME AS OBJ_NAME FROM RDB$CHECK_CONSTRAINTS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + ProcNameFull + '''';

    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" uses procedure "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, ProcNameFull, ObjectTypes[i].Name]));
          Q.Next;
        end;
      except
        on E: Exception do
          ResultLines.Add(Format('Error checking %s: %s', [ObjectTypes[i].Name, E.Message]));
      end;
    end;

    Result := Trim(ResultLines.Text);
  finally
    Q.Free;
    ResultLines.Free;
  end;
end;

end.

