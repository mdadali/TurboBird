unit role_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection;

function GetRoleDeps(const AConn: TIBConnection; const ARoleName: string): string;

implementation

function GetRoleDeps(const AConn: TIBConnection; const ARoleName: string): string;
var
  Q: TSQLQuery;
  RoleNameUpper, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  if ARoleName = '' then Exit;

  RoleNameUpper := UpperCase(Trim(ARoleName));

  Q := TSQLQuery.Create(nil);
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    SetLength(ObjectTypes, 4);

    // Rolle selbst existiert?
    ObjectTypes[0].Name := 'Roles';
    ObjectTypes[0].ObjType := 'ROLE';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$ROLE_NAME AS OBJ_NAME FROM RDB$ROLES ' +
      'WHERE UPPER(RDB$ROLE_NAME) = ''' + RoleNameUpper + '''';

    // Security Policies (Klassen)
    ObjectTypes[1].Name := 'Security Policies';
    ObjectTypes[1].ObjType := 'SECURITY_POLICY';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$SECURITY_CLASS AS OBJ_NAME FROM RDB$SECURITY_CLASSES ' +
      'WHERE UPPER(COALESCE(RDB$DESCRIPTION, '''')) CONTAINING ''' + RoleNameUpper + '''';

    // Benutzerrechte (User Privileges)
    ObjectTypes[2].Name := 'User Privileges';
    ObjectTypes[2].ObjType := 'USER_PRIVILEGE';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$USER AS OBJ_NAME FROM RDB$USER_PRIVILEGES ' +
      'WHERE UPPER(COALESCE(RDB$GRANTOR, '''')) = ''' + RoleNameUpper + ''' OR UPPER(COALESCE(RDB$USER, '''')) = ''' + RoleNameUpper + '''';

    // Metadaten Quellen: Procs, Triggers, Packages (kann Rolle als String vorkommen)
    ObjectTypes[3].Name := 'Metadata Sources';
    ObjectTypes[3].ObjType := 'METADATA';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + RoleNameUpper + ''' ' +
      'UNION ALL ' +
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + RoleNameUpper + ''' ' +
      'UNION ALL ' +
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + RoleNameUpper + ''' ' +
      'OR UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + RoleNameUpper + '''';

    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" references role "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, RoleNameUpper, ObjectTypes[i].Name]));
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
