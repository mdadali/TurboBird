unit exception_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;

function GetExceptionDeps(const AConn: TIBDatabase; const AExceptionName: string): string;

implementation

function GetExceptionDeps(const AConn: TIBDatabase; const AExceptionName: string): string;
var
  Q: TIBQuery;
  ExceptionNameUpper, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  exit;
 
  if AExceptionName = '' then Exit;

  ExceptionNameUpper := UpperCase(Trim(AExceptionName));

  Q := TIBQuery.Create(nil);
  Q.AllowAutoActivateTransaction := true;  
  
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    SetLength(ObjectTypes, 7);

    // Existiert Exception?
    ObjectTypes[0].Name := 'Exceptions';
    ObjectTypes[0].ObjType := 'EXCEPTION';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$EXCEPTION_NAME AS OBJ_NAME FROM RDB$EXCEPTIONS ' +
      'WHERE UPPER(RDB$EXCEPTION_NAME) = ''' + ExceptionNameUpper + '''';

    // Procedures mit Exception-Aufruf
    ObjectTypes[1].Name := 'Procedures';
    ObjectTypes[1].ObjType := 'PROCEDURE';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + ExceptionNameUpper + '''';

    // Triggers mit Exception-Aufruf
    ObjectTypes[2].Name := 'Triggers';
    ObjectTypes[2].ObjType := 'TRIGGER';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + ExceptionNameUpper + '''';

    // Packages mit Exception-Aufruf (Header)
    ObjectTypes[3].Name := 'Package Headers';
    ObjectTypes[3].ObjType := 'PACKAGE_HEADER';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + ExceptionNameUpper + '''';

    // Packages mit Exception-Aufruf (Body)
    ObjectTypes[4].Name := 'Package Bodies';
    ObjectTypes[4].ObjType := 'PACKAGE_BODY';
    ObjectTypes[4].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + ExceptionNameUpper + '''';

    // Views mit Exception-Referenz (selten, aber möglich)
    ObjectTypes[5].Name := 'Views';
    ObjectTypes[5].ObjType := 'VIEW';
    ObjectTypes[5].SQLText :=
      'SELECT RDB$RELATION_NAME AS OBJ_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_TYPE = 1 ' +
      'AND UPPER(COALESCE(RDB$VIEW_SOURCE, '''')) CONTAINING ''' + ExceptionNameUpper + '''';

    // Check Constraints mit Exception-Referenz (sehr selten)
    ObjectTypes[6].Name := 'Check Constraints';
    ObjectTypes[6].ObjType := 'CHECK_CONSTRAINT';
    ObjectTypes[6].SQLText :=
      'SELECT RDB$CONSTRAINT_NAME AS OBJ_NAME FROM RDB$CHECK_CONSTRAINTS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + ExceptionNameUpper + '''';

    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" references exception "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, ExceptionNameUpper, ObjectTypes[i].Name]));
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
