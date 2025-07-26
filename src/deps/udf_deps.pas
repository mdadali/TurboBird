unit udf_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection;

function GetUDFDeps(const AConn: TIBConnection; const AUDFName, APackageName: string): string;

implementation

function GetUDFDeps(const AConn: TIBConnection; const AUDFName, APackageName: string): string;
var
  Q: TSQLQuery;
  UDFNameFull, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  if AUDFName = '' then Exit;

  if APackageName <> '' then
    UDFNameFull := UpperCase(Trim(APackageName) + '.' + Trim(AUDFName))
  else
    UDFNameFull := UpperCase(Trim(AUDFName));

  Q := TSQLQuery.Create(nil);
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    SetLength(ObjectTypes, 8);

    // Andere UDFs (UDFs selbst)
    ObjectTypes[0].Name := 'UDF Definitions';
    ObjectTypes[0].ObjType := 'UDF_DEF';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$FUNCTION_NAME AS OBJ_NAME FROM RDB$FUNCTIONS ' +
      'WHERE UPPER(RDB$FUNCTION_NAME) = ''' + UpperCase(AUDFName) + '''';

    // Procedures
    ObjectTypes[1].Name := 'Procedures';
    ObjectTypes[1].ObjType := 'PROCEDURE';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + UDFNameFull + '''';

    // Triggers
    ObjectTypes[2].Name := 'Triggers';
    ObjectTypes[2].ObjType := 'TRIGGER';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + UDFNameFull + '''';

    // Views
    ObjectTypes[3].Name := 'Views';
    ObjectTypes[3].ObjType := 'VIEW';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$RELATION_NAME AS OBJ_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_TYPE = 1 ' +
      'AND UPPER(COALESCE(RDB$VIEW_SOURCE, '''')) CONTAINING ''' + UDFNameFull + '''';

    // Package Header
    ObjectTypes[4].Name := 'Package Headers';
    ObjectTypes[4].ObjType := 'PACKAGE_HDR';
    ObjectTypes[4].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + UDFNameFull + '''';

    // Package Body
    ObjectTypes[5].Name := 'Package Bodies';
    ObjectTypes[5].ObjType := 'PACKAGE_BODY';
    ObjectTypes[5].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + UDFNameFull + '''';

    // Computed Fields
    ObjectTypes[6].Name := 'Computed Fields';
    ObjectTypes[6].ObjType := 'FIELD';
    ObjectTypes[6].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$COMPUTED_SOURCE, '''')) CONTAINING ''' + UDFNameFull + '''';

    // Check Constraints
    ObjectTypes[7].Name := 'Check Constraints';
    ObjectTypes[7].ObjType := 'CONSTRAINT';
    ObjectTypes[7].SQLText :=
      'SELECT RDB$CONSTRAINT_NAME AS OBJ_NAME FROM RDB$CHECK_CONSTRAINTS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + UDFNameFull + '''';

    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" uses UDF "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, UDFNameFull, ObjectTypes[i].Name]));
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
