unit udb_firebird_struct_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  IB,
  IBDatabase,
  IBQuery;

function CleanupBodyText(const S: string): string;
function CharsetIdToStr(CharsetId: Integer; Conn: TIBDatabase): string;
function FieldTypeToStr(FieldType, SubType, Precision, Scale, Length, CharsetId: Integer; Conn: TIBDatabase): string;
function FieldSourceToStr(SourceName: string; Conn: TIBDatabase): string;
function ResolveFieldTypeAsString(SourceName: string; FieldType, SubType, Precision, Scale, CharLen, CharsetId: Integer; Conn: TIBDatabase): string;
function ResolveFieldTypeAsPascalFieldType(SourceName: string; FieldType, SubType, Precision, Scale, CharLen, CharsetId: Integer; Conn: TIBDatabase): TFieldType;
function GetValidInputChars(const FirebirdType: string): TFieldChars;

implementation


function CleanupBodyText(const S: string): string;
var
  Cleaned: string;
begin
  Cleaned := Trim(S);
  while Pos(LineEnding + LineEnding, Cleaned) > 0 do
    Cleaned := StringReplace(Cleaned, LineEnding + LineEnding, LineEnding, [rfReplaceAll]);
  Result := Cleaned;
end;

function CharsetIdToStr(CharsetId: Integer; Conn: TIBDatabase): string;
var
  Q: TIBQuery;
begin
  Result := 'UNKNOWN';
  Q := TIBQuery.Create(nil);
  try
    Q.DataBase := Conn;
    Q.SQL.Text :=
      'SELECT RDB$CHARACTER_SET_NAME ' +
      'FROM RDB$CHARACTER_SETS ' +
      'WHERE RDB$CHARACTER_SET_ID = :ID';
    Q.ParamByName('ID').AsInteger := CharsetId;
    Q.Open;
    if not Q.EOF then
      Result := Trim(Q.FieldByName('RDB$CHARACTER_SET_NAME').AsString);
  finally
    Q.Free;
  end;
end;

function FieldTypeToStr(FieldType, SubType, Precision, Scale, Length, CharsetId: Integer; Conn: TIBDatabase): string;
var
  CharSetName: string;
begin
  CharSetName := CharsetIdToStr(CharsetId, Conn);

  case FieldType of
    7:  Result := 'SMALLINT';
    8:  Result := 'INTEGER';
    9:  Result := 'QUAD';
    10: Result := 'FLOAT';
    11: Result := 'D_FLOAT';
    12: Result := 'DATE';
    13: Result := 'TIME';
    14: Result := Format('CHAR(%d) CHARACTER SET %s', [Length, CharSetName]);
    16:
      // Differenziere ggf. DECIMAL/NUMERIC mit Precision
      if (SubType = 1) or (SubType = 2) then
        Result := Format('NUMERIC(%d,%d)', [Precision, Abs(Scale)])
      else
        Result := 'BIGINT';
    27: Result := 'DOUBLE PRECISION';
    35: Result := 'TIMESTAMP';
    37: Result := Format('VARCHAR(%d) CHARACTER SET %s', [Length, CharSetName]);
    261:
      if SubType = 1 then
        Result := Format('BLOB SUB_TYPE TEXT CHARACTER SET %s', [CharSetName])
      else
        Result := 'BLOB';
  else
    Result := 'UNKNOWN';
  end;
end;
function FieldSourceToStr(SourceName: string; Conn: TIBDatabase): string;
var
  Q: TIBQuery;
begin
  Result := 'UNKNOWN';
  Q := TIBQuery.Create(nil);
  try
    Q.DataBase := Conn;
    Q.SQL.Text :=
      'SELECT RDB$FIELD_TYPE, RDB$FIELD_SUB_TYPE, RDB$FIELD_PRECISION, ' +
      'RDB$FIELD_SCALE, RDB$CHARACTER_LENGTH, RDB$CHARACTER_SET_ID ' +
      'FROM RDB$FIELDS ' +
      'WHERE RDB$FIELD_NAME = :SRC';
    Q.ParamByName('SRC').AsString := SourceName;
    Q.Open;
    if not Q.EOF then
    begin
      Result := FieldTypeToStr(
        Q.FieldByName('RDB$FIELD_TYPE').AsInteger,
        Q.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
        Q.FieldByName('RDB$FIELD_PRECISION').AsInteger,
        Q.FieldByName('RDB$FIELD_SCALE').AsInteger,
        Q.FieldByName('RDB$CHARACTER_LENGTH').AsInteger,
        Q.FieldByName('RDB$CHARACTER_SET_ID').AsInteger,
        Conn
      );
    end;
  finally
    Q.Free;
  end;
end;

function ResolveFieldTypeAsString(SourceName: string; FieldType, SubType, Precision, Scale, CharLen, CharsetId: Integer; Conn: TIBDatabase): string;
begin
  if Trim(SourceName) <> '' then
    Result := FieldSourceToStr(SourceName, Conn)
  else
    Result := FieldTypeToStr(FieldType, SubType, Precision, Scale, CharLen, CharsetId, Conn);
end;

function ResolveFieldTypeAsPascalFieldType(SourceName: string; FieldType, SubType, Precision, Scale, CharLen, CharsetId: Integer; Conn: TIBDatabase): TFieldType;
var
  FBTypeStr: string;
begin
  FBTypeStr := UpperCase(ResolveFieldTypeAsString(SourceName, FieldType, SubType, Precision, Scale, CharLen, CharsetId, Conn));

  if FBTypeStr = 'SMALLINT' then
    Result := ftSmallint
  else if FBTypeStr = 'INTEGER' then
    Result := ftInteger
  else if FBTypeStr = 'BIGINT' then
    Result := ftLargeint
  else if (FBTypeStr = 'FLOAT') or (FBTypeStr = 'D_FLOAT') or (FBTypeStr = 'DOUBLE PRECISION') or
          (FBTypeStr = 'DECIMAL') or (FBTypeStr = 'NUMERIC') then
    Result := ftFloat
  else if Pos('CHAR(', FBTypeStr) = 1 then
    Result := ftFixedChar
  else if Pos('VARCHAR(', FBTypeStr) = 1 then
    Result := ftString
  else if Pos('BLOB SUB_TYPE TEXT', FBTypeStr) = 1 then
    Result := ftMemo
  else if Pos('BLOB', FBTypeStr) = 1 then
    Result := ftBlob
  else if FBTypeStr = 'DATE' then
    Result := ftDate
  else if FBTypeStr = 'TIME' then
    Result := ftTime
  else if FBTypeStr = 'TIMESTAMP' then
    Result := ftDateTime
  else if FBTypeStr = 'QUAD' then
    Result := ftLargeint
  else
    Result := ftUnknown;
end;

function GetValidInputChars(const FirebirdType: string): TFieldChars;
var S: string;
const AllChars: set of char = [#0..#255];
begin
  S := UpperCase(FirebirdType);

  if (S = 'INTEGER') or (S = 'SMALLINT') or (S = 'BIGINT') or (S = 'QUAD') then
    Result := ['0'..'9', '-', '+']  // Integer Zahlen inkl. Vorzeichen
  else if (S = 'FLOAT') or (S = 'D_FLOAT') or (S = 'DOUBLE PRECISION') or (S = 'DECIMAL') or (S = 'NUMERIC') then
    Result := ['0'..'9', '-', '+', ',', '.']  // Gleitkommazahlen, Punkt oder Komma
  else if (S = 'DATE') then
    Result := ['0'..'9', '-','.','/']  // z.B. 2025-05-25 oder 25.05.2025 etc.
  else if (S = 'TIME') then
    Result := ['0'..'9', ':']  // z.B. 13:45:00
  else if (S = 'TIMESTAMP') then
    Result := ['0'..'9', '-', '.', '/', ':', ' ']  // Kombination aus Datum + Zeit
  {else if Pos('CHAR(', S) = 1 then
    Result := AllChars  // Freie Zeichen für Char
  else if Pos('VARCHAR(', S) = 1 then
    Result := AllChars  // Freie Zeichen für Varchar
  else if Pos('BLOB SUB_TYPE TEXT', S) = 1 then
    Result := AllChars  // Freie Zeichen für Textblob }
  else;
    //Result := [#0..#255];  // sonst keine Einschränkung
end;


end.

