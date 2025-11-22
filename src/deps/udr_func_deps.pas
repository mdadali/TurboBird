unit udr_func_deps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IB,
  IBDatabase,
  IBQuery;


function GetUDRFunctionDeps(const AConn: TIBDatabase; const AUDRFuncName, APackageName: string): string;

implementation

function GetUDRFunctionDeps(const AConn: TIBDatabase; const AUDRFuncName, APackageName: string): string;
var
  Q: TIBQuery;
  FuncNameFull, ObjectName: string;
  ResultLines: TStringList;
  ObjectTypes: array of record
    Name, SQLText, ObjType: string;
  end;
  i: Integer;
begin
  Result := '';
  if AUDRFuncName = '' then Exit;

  if APackageName <> '' then
    FuncNameFull := UpperCase(Trim(APackageName) + '.' + Trim(AUDRFuncName))
  else
    FuncNameFull := UpperCase(Trim(AUDRFuncName));

  Q := TIBQuery.Create(nil);
  Q.AllowAutoActivateTransaction := true;  
  
  ResultLines := TStringList.Create;
  try
    Q.DataBase := AConn;

    if not AConn.DefaultTransaction.InTransaction then
      AConn.DefaultTransaction.StartTransaction;

    SetLength(ObjectTypes, 8);

    // UDR Function definitions (RDB$FUNCTIONS where RDB$FUNCTION_TYPE=1 means external function, i.e. UDR)
    ObjectTypes[0].Name := 'UDR Function Definitions';
    ObjectTypes[0].ObjType := 'UDR_FUNC_DEF';
    ObjectTypes[0].SQLText :=
      'SELECT RDB$FUNCTION_NAME AS OBJ_NAME FROM RDB$FUNCTIONS ' +
      'WHERE RDB$FUNCTION_TYPE = 1 AND UPPER(RDB$FUNCTION_NAME) = ''' + UpperCase(AUDRFuncName) + '''';

    // Procedures
    ObjectTypes[1].Name := 'Procedures';
    ObjectTypes[1].ObjType := 'PROCEDURE';
    ObjectTypes[1].SQLText :=
      'SELECT RDB$PROCEDURE_NAME AS OBJ_NAME FROM RDB$PROCEDURES ' +
      'WHERE UPPER(COALESCE(RDB$PROCEDURE_SOURCE, '''')) CONTAINING ''' + FuncNameFull + '''';

    // Triggers
    ObjectTypes[2].Name := 'Triggers';
    ObjectTypes[2].ObjType := 'TRIGGER';
    ObjectTypes[2].SQLText :=
      'SELECT RDB$TRIGGER_NAME AS OBJ_NAME FROM RDB$TRIGGERS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + FuncNameFull + '''';

    // Views
    ObjectTypes[3].Name := 'Views';
    ObjectTypes[3].ObjType := 'VIEW';
    ObjectTypes[3].SQLText :=
      'SELECT RDB$RELATION_NAME AS OBJ_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_TYPE = 1 ' +
      'AND UPPER(COALESCE(RDB$VIEW_SOURCE, '''')) CONTAINING ''' + FuncNameFull + '''';

    // Package Headers
    ObjectTypes[4].Name := 'Package Headers';
    ObjectTypes[4].ObjType := 'PACKAGE_HDR';
    ObjectTypes[4].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_HEADER_SOURCE, '''')) CONTAINING ''' + FuncNameFull + '''';

    // Package Bodies
    ObjectTypes[5].Name := 'Package Bodies';
    ObjectTypes[5].ObjType := 'PACKAGE_BODY';
    ObjectTypes[5].SQLText :=
      'SELECT RDB$PACKAGE_NAME AS OBJ_NAME FROM RDB$PACKAGES ' +
      'WHERE UPPER(COALESCE(RDB$PACKAGE_BODY_SOURCE, '''')) CONTAINING ''' + FuncNameFull + '''';

    // Computed Fields
    ObjectTypes[6].Name := 'Computed Fields';
    ObjectTypes[6].ObjType := 'FIELD';
    ObjectTypes[6].SQLText :=
      'SELECT RDB$FIELD_NAME AS OBJ_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE UPPER(COALESCE(RDB$COMPUTED_SOURCE, '''')) CONTAINING ''' + FuncNameFull + '''';

    // Check Constraints
    ObjectTypes[7].Name := 'Check Constraints';
    ObjectTypes[7].ObjType := 'CONSTRAINT';
    ObjectTypes[7].SQLText :=
      'SELECT RDB$CONSTRAINT_NAME AS OBJ_NAME FROM RDB$CHECK_CONSTRAINTS ' +
      'WHERE UPPER(COALESCE(RDB$TRIGGER_SOURCE, '''')) CONTAINING ''' + FuncNameFull + '''';

    for i := 0 to High(ObjectTypes) do
    begin
      Q.Close;
      Q.SQL.Text := ObjectTypes[i].SQLText;
      try
        Q.Open;
        while not Q.EOF do
        begin
          ObjectName := Trim(Q.FieldByName('OBJ_NAME').AsString);
          ResultLines.Add(Format('%s "%s" uses UDR function "%s". [%s]',
            [ObjectTypes[i].ObjType, ObjectName, FuncNameFull, ObjectTypes[i].Name]));
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

