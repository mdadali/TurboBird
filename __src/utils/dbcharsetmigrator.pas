unit DBCharsetMigrator;

interface

uses
  SysUtils, Classes, IBDatabase, IBQuery;

type
  TDBCharsetMigrator = class
  public
    class procedure CheckCharsetCompatibility(
      const DB: TIBDatabase;
      const TargetCharset: string;
      Report: TStrings);
  end;

implementation

{ TDBCharsetMigrator }

class procedure TDBCharsetMigrator.CheckCharsetCompatibility(
  const DB: TIBDatabase; const TargetCharset: string; Report: TStrings);
var
  Q: TIBQuery;
  TableName, FieldName, FieldCharset: string;
begin
  if not Assigned(Report) then
    Exit;

  Report.Clear;
  Report.Add('--- Charset Compatibility Check ---');
  Report.Add('Database: ' + DB.DatabaseName);
  Report.Add('Target Charset: ' + TargetCharset);
  Report.Add('Date/Time: ' + DateTimeToStr(Now));
  Report.Add('');

  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;

    // Query all fields and their charset
    Q.SQL.Text :=
      'SELECT rf.RDB$RELATION_NAME, rf.RDB$FIELD_NAME, cs.RDB$CHARACTER_SET_NAME ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'LEFT JOIN RDB$CHARACTER_SETS cs ON f.RDB$CHARACTER_SET_ID = cs.RDB$CHARACTER_SET_ID ' +
      'WHERE rf.RDB$SYSTEM_FLAG = 0 ' +
      'ORDER BY rf.RDB$RELATION_NAME, rf.RDB$FIELD_NAME';

    Q.Open;
    while not Q.Eof do
    begin
      TableName   := Trim(Q.Fields[0].AsString);
      FieldName   := Trim(Q.Fields[1].AsString);
      FieldCharset:= Trim(Q.Fields[2].AsString);

      if (FieldCharset = '') then
        FieldCharset := 'NONE';

      if not SameText(FieldCharset, TargetCharset) then
        Report.Add(Format('Table %s Field %s: %s -> would be %s',
          [TableName, FieldName, FieldCharset, TargetCharset]));

      Q.Next;
    end;
    Q.Close;

    if Report.Count = 4 then // only header written
      Report.Add('All fields already match the target charset.');

  finally
    Q.Free;
  end;
end;

end.


