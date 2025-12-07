unit user_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetUserDeps(const AConn: TIBDatabase; const AUserName: string): string;

implementation

function GetUserDeps(const AConn: TIBDatabase; const AUserName: string): string;
var
  Q: TIBQuery;
  UserNameUpper, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  if AUserName = '' then Exit;

  UserNameUpper := UpperCase(Trim(AUserName));

  Q := TIBQuery.Create(nil);
  Q.AllowAutoActivateTransaction := true;  
  
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    SetLength(ObjectTypes, 5);

    // User Privileges (RDB$USER_PRIVILEGES)
    ObjectTypes[0].Name := 'User Privileges';
    ObjectTypes[0].ObjType := 'USER_PRIVILEGE';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$USER AS OBJ_NAME FROM RDB$USER_PRIVILEGES ' +
      'WHERE UPPER(COALESCE(RDB$USER, '''')) = ''' + UserNameUpper + '''';

    // Grantor Privileges (RDB$USER_PRIVILEGES)
    ObjectTypes[1].Name := 'Grantor Privileges';
    ObjectTypes[1].ObjType := 'GRANTOR_PRIVILEGE';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$GRANTOR AS OBJ_NAME FROM RDB$USER_PRIVILEGES ' +
      'WHERE UPPER(COALESCE(RDB$GRANTOR, '''')) = ''' + UserNameUpper + '''';

    // Security Policies (RDB$SECURITY_CLASSES)
    ObjectTypes[2].Name := 'Security Policies';
    ObjectTypes[2].ObjType := 'SECURITY_POLICY';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$SECURITY_CLASS AS OBJ_NAME FROM RDB$SECURITY_CLASSES ' +
      'WHERE UPPER(COALESCE(RDB$DESCRIPTION, '''')) CONTAINING ''' + UserNameUpper + '''';

    // Metadata Sources (Procs, Triggers, Packages) mit Username als Text
    ObjectTypes[3].Name := 'Metadata Sources';
    ObjectTypes[3].ObjType := 'METADATA';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + UserNameUpper + ''' ' +
      'UNION ALL ' +
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + UserNameUpper + ''' ' +
      'UNION ALL ' +
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + UserNameUpper + ''' ' +
      'OR UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + UserNameUpper + '''';

    // Benutzer in Benutzer-Tabelle (Firebird 3+ SYSUSERS)
    ObjectTypes[4].Name := 'SYSUSERS';
    ObjectTypes[4].ObjType := 'SYSUSER';
    ObjectTypes[4].SQLText :=
      'SELECT USER_NAME AS OBJ_NAME FROM RDB$USERS ' +
      'WHERE UPPER(USER_NAME) = ''' + UserNameUpper + '''';

    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" references user "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, UserNameUpper, ObjectTypes[i].Name]));
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

