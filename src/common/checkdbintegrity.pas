unit CheckDBIntegrity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, IBDatabase, DB, IBQuery, DateUtils,
  turbocommon
  ;


procedure InitCheckResult(var Res: TDBCheckResult; const Title, DBName: string);
procedure CheckFieldsCharset(DB: TIBDatabase; CharsetIssues: TStringList);
procedure CheckFieldsLength(DB: TIBDatabase; LengthIssues: TStringList; MaxLength: Integer = 32765);
procedure CheckFieldsNotNull(DB: TIBDatabase; NotNullIssues: TStringList);
procedure CheckFieldsDataType(DB: TIBDatabase; DataTypeIssues: TStringList);
procedure CheckPrimaryKeys(DB: TIBDatabase; PKIssues: TStringList);
procedure CheckForeignKeys(DB: TIBDatabase; FKIssues: TStringList);
procedure CheckViews(DB: TIBDatabase; ViewsIssues: TStringList);
procedure CheckIndicesUnique(DB: TIBDatabase; IndexUniqueIssues: TStringList);
procedure CheckTriggerDefaults(DB: TIBDatabase; TriggerDefaultIssues: TStringList);

implementation

procedure InitCheckResult(var Res: TDBCheckResult; const Title, DBName: string);
begin
  Res.Title := Title;
  Res.DBName := DBName;
  Res.DateTimeChecked := Now;
  Res.CharsetIssues := TStringList.Create;
  Res.LengthIssues := TStringList.Create;
  Res.NotNullIssues := TStringList.Create;
  Res.DataTypeIssues := TStringList.Create;
  Res.PKIssues := TStringList.Create;
  Res.FKIssues := TStringList.Create;
  Res.ViewsIssues := TStringList.Create;
  Res.IndexUniqueIssues := TStringList.Create;
  Res.TriggerDefaultIssues := TStringList.Create;
end;

procedure CheckFieldsCharset(DB: TIBDatabase; CharsetIssues: TStringList);
var
  Q: TIBQuery;
  TableList, FieldList: TStringList;
  TableName, FieldName, ExpectedCharset, ActualCharset: string;
begin
  if not Assigned(DB) then Exit;
  if not DB.Connected then Exit;
  ExpectedCharset := UpperCase(DB.Params.Values['lc_ctype']);

  TableList := TStringList.Create;
  FieldList := TStringList.Create;
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    DB.GetTableNames(TableList, False);

    for TableName in TableList do
    begin
      DB.GetFieldNames(TableName, FieldList);

      for FieldName in FieldList do
      begin
        Q.SQL.Text :=
          'SELECT cs.RDB$CHARACTER_SET_NAME ' +
          'FROM RDB$FIELDS f ' +
          'JOIN RDB$RELATION_FIELDS rf ON f.RDB$FIELD_NAME = rf.RDB$FIELD_SOURCE ' +
          'JOIN RDB$CHARACTER_SETS cs ON f.RDB$CHARACTER_SET_ID = cs.RDB$CHARACTER_SET_ID ' +
          'WHERE rf.RDB$RELATION_NAME = :TableName AND rf.RDB$FIELD_NAME = :FieldName';
        Q.ParamByName('TableName').AsString := TableName;
        Q.ParamByName('FieldName').AsString := FieldName;
        Q.Open;
        if not Q.EOF then
        begin
          ActualCharset := UpperCase(Trim(Q.Fields[0].AsString));
          if ActualCharset <> ExpectedCharset then
            CharsetIssues.Add(
              Format('Table %s Field %s: Charset is %s, should be %s',
                [TableName, FieldName, ActualCharset, ExpectedCharset])
            );
        end;
        Q.Close;
      end;
    end;

  finally
    TableList.Free;
    FieldList.Free;
    Q.Free;
  end;
end;

procedure CheckFieldsLength(DB: TIBDatabase; LengthIssues: TStringList; MaxLength: Integer = 32765);
var
  Q: TIBQuery;
  TableName, FieldName: string;
  CharLen: Integer;
  FieldType: Integer;
begin
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.Transaction := DB.DefaultTransaction;

    Q.SQL.Text :=
      'SELECT rf.RDB$RELATION_NAME, rf.RDB$FIELD_NAME, ' +
      '       f.RDB$FIELD_TYPE, f.RDB$CHARACTER_LENGTH ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'WHERE rf.RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY rf.RDB$RELATION_NAME, rf.RDB$FIELD_POSITION';

    Q.Open;
    while not Q.EOF do
    begin
      TableName := Trim(Q.Fields[0].AsString);
      FieldName := Trim(Q.Fields[1].AsString);
      FieldType := Q.Fields[2].AsInteger;
      CharLen := Q.Fields[3].AsInteger;

      // only CHAR (14) and VARCHAR (37)
      if (FieldType in [14, 37]) then
      begin
        if (CharLen <= 0) or (CharLen > 32765) then
          LengthIssues.Add(Format('Table %s Field %s: Invalid length (%d)',
            [TableName, FieldName, CharLen]));
      end;

      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure CheckFieldsNotNull(DB: TIBDatabase; NotNullIssues: TStringList);
var
  Q: TIBQuery;
  TableList, FieldList: TStringList;
  TableName, FieldName: string;
  NullFlag: Integer;
begin
  if not Assigned(DB) then Exit;
  if not DB.Connected then Exit;

  TableList := TStringList.Create;
  FieldList := TStringList.Create;
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    DB.GetTableNames(TableList, False);

    for TableName in TableList do
    begin
      DB.GetFieldNames(TableName, FieldList);
      for FieldName in FieldList do
      begin
        Q.SQL.Text :=
          'SELECT RDB$NULL_FLAG FROM RDB$RELATION_FIELDS ' +
          'WHERE RDB$RELATION_NAME=:TableName AND RDB$FIELD_NAME=:FieldName';
        Q.ParamByName('TableName').AsString := TableName;
        Q.ParamByName('FieldName').AsString := FieldName;
        Q.Open;
        if not Q.EOF then
        begin
          if Q.Fields[0].IsNull then NullFlag := 0 else NullFlag := Q.Fields[0].AsInteger;
          if NullFlag = 0 then
            NotNullIssues.Add(Format('Table %s Field %s: Nullable', [TableName, FieldName]));
        end;
        Q.Close;
      end;
    end;
  finally
    TableList.Free;
    FieldList.Free;
    Q.Free;
  end;
end;

// ------------------------------
// Placeholders for remaining checks (no DB changes, only diagnostics)
// Implementing all checks in future iterations
procedure CheckFieldsDataType(DB: TIBDatabase; DataTypeIssues: TStringList);
begin
  DataTypeIssues.Add('DataType check not implemented yet.');
end;

procedure CheckPrimaryKeys(DB: TIBDatabase; PKIssues: TStringList);
begin
  PKIssues.Add('PK check not implemented yet.');
end;

procedure CheckForeignKeys(DB: TIBDatabase; FKIssues: TStringList);
begin
  FKIssues.Add('FK check not implemented yet.');
end;

procedure CheckViews(DB: TIBDatabase; ViewsIssues: TStringList);
begin
  ViewsIssues.Add('Views check not implemented yet.');
end;

procedure CheckIndicesUnique(DB: TIBDatabase; IndexUniqueIssues: TStringList);
begin
  IndexUniqueIssues.Add('Indices/Unique check not implemented yet.');
end;

procedure CheckTriggerDefaults(DB: TIBDatabase; TriggerDefaultIssues: TStringList);
begin
  TriggerDefaultIssues.Add('Trigger/Defaults check not implemented yet.');
end;

end.

