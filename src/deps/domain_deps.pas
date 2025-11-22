unit domain_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetDomainDeps(const AConn: TIBDatabase; const ADomainName: string): string;

implementation

function GetDomainDeps(const AConn: TIBDatabase; const ADomainName: string): string;
var
  Q: TIBQuery;
  DomainNameUpper, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  if ADomainName = '' then Exit;

  DomainNameUpper := UpperCase(Trim(ADomainName));

  Q := TIBQuery.Create(nil);
  Q.AllowAutoActivateTransaction := true;  
  
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    SetLength(ObjectTypes, 9);

    // Tabellenfelder, die diese Domain verwenden
    ObjectTypes[0].Name := 'Table Fields';
    ObjectTypes[0].ObjType := 'TABLE_FIELD';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME, RDB$RELATION_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(RDB$FIELD_SOURCE) = ''' + DomainNameUpper + '''';

    // Computed Fields mit Domain im Source
    ObjectTypes[1].Name := 'Computed Fields';
    ObjectTypes[1].ObjType := 'COMPUTED_FIELD';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$COMPUTED_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    // Check Constraints, evtl. Domain benutzt
    ObjectTypes[2].Name := 'Check Constraints';
    ObjectTypes[2].ObjType := 'CHECK_CONSTRAINT';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$CONSTRAINT_NAME AS OBJ_NAME FROM RDB$CHECK_CONSTRAINTS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    // Views
    ObjectTypes[3].Name := 'Views';
    ObjectTypes[3].ObjType := 'VIEW';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$RELATION_NAME AS OBJ_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_TYPE = 1 ' +
      'AND UPPER(COALESCE(RDB$VIEW_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    // Procedures
    ObjectTypes[4].Name := 'Procedures';
    ObjectTypes[4].ObjType := 'PROCEDURE';
    ObjectTypes[4].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    // Triggers
    ObjectTypes[5].Name := 'Trigger';
    ObjectTypes[5].ObjType := 'TRIGGER';
    ObjectTypes[5].SQLText :=
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    // Package Headers
    ObjectTypes[6].Name := 'Package Headers';
    ObjectTypes[6].ObjType := 'PACKAGE_HEADER';
    ObjectTypes[6].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    // Package Bodies
    ObjectTypes[7].Name := 'Package Bodies';
    ObjectTypes[7].ObjType := 'PACKAGE_BODY';
    ObjectTypes[7].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    // Domains (recursive usage in domains themselves)
    ObjectTypes[8].Name := 'Domains';
    ObjectTypes[8].ObjType := 'DOMAIN';
    ObjectTypes[8].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$FIELD_SOURCE, '''')) CONTAINING ''' + DomainNameUpper + '''';

    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          if ObjectTypes[i].ObjType = 'TABLE_FIELD' then
            ObjectName := Format('%s (Table: %s)',
              [Trim(Q.FieldByName('OBJ_NAME').AsString), Trim(Q.FieldByName('RDB$RELATION_NAME').AsString)])
          else
            ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);

          ResultLines.Add(Format('%s "%s" uses domain "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, DomainNameUpper, ObjectTypes[i].Name]));
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
