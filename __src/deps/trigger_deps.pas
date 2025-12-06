unit trigger_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetTriggerDeps(const AConn: TIBDatabase; const ATriggerName: string): string;

implementation

function GetTriggerDeps(const AConn: TIBDatabase; const ATriggerName: string): string;
var
  Q: TIBQuery;
  TrigNameUpper, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  if ATriggerName = '' then Exit;

  TrigNameUpper := UpperCase(Trim(ATriggerName));
  Q := TIBQuery.Create(nil);
  Q.AllowAutoActivateTransaction := true;  
  
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    SetLength(ObjectTypes, 9);

    // Procedures
    ObjectTypes[0].Name := 'Procedures';
    ObjectTypes[0].ObjType := 'PROCEDURE';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Triggers
    ObjectTypes[1].Name := 'Other Triggers';
    ObjectTypes[1].ObjType := 'TRIGGER';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + ''' ' +
      'AND UPPER(RDB$TRIGGER_NAME) <> ''' + TrigNameUpper + '''';

    // Views
    ObjectTypes[2].Name := 'Views';
    ObjectTypes[2].ObjType := 'VIEW';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$RELATION_NAME AS OBJ_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_TYPE = 1 ' +
      'AND UPPER(COALESCE(RDB$VIEW_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Computed Fields
    ObjectTypes[3].Name := 'Computed Fields';
    ObjectTypes[3].ObjType := 'FIELD';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$COMPUTED_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Package Headers
    ObjectTypes[4].Name := 'Package Headers';
    ObjectTypes[4].ObjType := 'PACKAGE_HDR';
    ObjectTypes[4].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Package Bodies
    ObjectTypes[5].Name := 'Package Bodies';
    ObjectTypes[5].ObjType := 'PACKAGE_BODY';
    ObjectTypes[5].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Domains
    ObjectTypes[6].Name := 'Domains';
    ObjectTypes[6].ObjType := 'DOMAIN';
    ObjectTypes[6].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$DEFAULT_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Field Defaults
    ObjectTypes[7].Name := 'Field Defaults';
    ObjectTypes[7].ObjType := 'FIELD_DEFAULT';
    ObjectTypes[7].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$DEFAULT_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Check Constraints
    ObjectTypes[8].Name := 'Check Constraints';
    ObjectTypes[8].ObjType := 'CONSTRAINT';
    ObjectTypes[8].SQLText :=
      'SELECT RDB$CONSTRAINT_NAME AS OBJ_NAME FROM RDB$CHECK_CONSTRAINTS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + TrigNameUpper + '''';

    // Analyse durchführen
    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" refers to trigger "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, ATriggerName, ObjectTypes[i].Name]));
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
