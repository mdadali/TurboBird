unit package_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection;

function GetPackageDependencies(const AConn: TIBConnection; const APackageName: string): string;

implementation

function GetPackageDependencies(const AConn: TIBConnection; const APackageName: string): string;
var
  Q: TSQLQuery;
  PkgNameUpper, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  if APackageName = '' then Exit;

  PkgNameUpper := UpperCase(Trim(APackageName));
  Q := TSQLQuery.Create(nil);
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    // Objektarten definieren
    SetLength(ObjectTypes, 6);

    ObjectTypes[0].Name := 'Procedures';
    ObjectTypes[0].ObjType := 'PROCEDURE';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + PkgNameUpper + '''';

    ObjectTypes[1].Name := 'Functions';
    ObjectTypes[1].ObjType := 'FUNCTION';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$FUNCTION_NAME AS OBJ_NAME FROM RDB$FUNCTIONS ' +
      'WHERE UPPER(COALESCE(RDB$FUNCTION_SOURCE, '''')) CONTAINING ''' + PkgNameUpper + '''';

    ObjectTypes[2].Name := 'Views';
    ObjectTypes[2].ObjType := 'VIEW';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$RELATION_NAME AS OBJ_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_TYPE = 1 ' +
      'AND UPPER(COALESCE(RDB$RELATION_SOURCE, '''')) CONTAINING ''' + PkgNameUpper + '''';

    ObjectTypes[3].Name := 'Triggers';
    ObjectTypes[3].ObjType := 'TRIGGER';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + PkgNameUpper + '''';

    ObjectTypes[4].Name := 'Other Packages';
    ObjectTypes[4].ObjType := 'PACKAGE';
    ObjectTypes[4].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + PkgNameUpper + ''' ' +
      'OR UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + PkgNameUpper + '''';

    ObjectTypes[5].Name := 'Computed Fields (from Relations)';
    ObjectTypes[5].ObjType := 'FIELD';
    ObjectTypes[5].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$COMPUTED_SOURCE, '''')) CONTAINING ''' + PkgNameUpper + '''';

    // Abfragen
    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" uses package "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, APackageName, ObjectTypes[i].Name]));
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
