unit DBReaderExternalWriter;

interface

uses
  SysUtils, Classes, Variants, DB,
  DBReaderBase, DBReaderFirebird;

type
  TExternalTableWriter = class
  private
    FFileName: string;
    FFieldsDef: array of TDbFieldDefRec;
    FRecordCount: Integer;
    FStream: TFileStream;

    // Für CHAR-Feld-Formatierung
    function FormatFieldForExternal(const AValue: Variant; AFieldType: TFieldType; ASize: Integer): string;
    function GetCharLength(AFieldType: TFieldType; ASize: Integer): Integer;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure AddFieldDef(const AFieldDef: TDbFieldDefRec);
    procedure AddRecord(ARow: TDbRowItem);
    procedure Close;

    function GenerateCreateTableSQL(const ATableName: string): string;

    property RecordCount: Integer read FRecordCount;
  end;

implementation


constructor TExternalTableWriter.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FRecordCount := 0;
  FStream := TFileStream.Create(AFileName, fmCreate);
end;

destructor TExternalTableWriter.Destroy;
begin
  Close;
  inherited;
end;

procedure TExternalTableWriter.AddFieldDef(const AFieldDef: TDbFieldDefRec);
var
  i: Integer;
begin
  i := Length(FFieldsDef);
  SetLength(FFieldsDef, i + 1);
  FFieldsDef[i] := AFieldDef;
end;

function TExternalTableWriter.GetCharLength(AFieldType: TFieldType; ASize: Integer): Integer;
begin
  case AFieldType of
    ftString, ftWideString, ftFixedChar, ftFixedWideChar, ftMemo:
      Result := ASize;
    ftInteger, ftSmallint, ftLargeint:
      Result := 20;  // 20 Stellen für Zahlen
    ftFloat, ftCurrency:
      Result := 30;  // 30 Stellen für Fließkomma
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := 24;  // 24 Stellen für Datum
    ftBoolean:
      Result := 1;   // 1 Stelle für Boolean
  else
    Result := 255;   // Fallback
  end;
end;

function TExternalTableWriter.FormatFieldForExternal(
  const AValue: Variant; AFieldType: TFieldType; ASize: Integer): string;
var
  CharLen: Integer;
begin
  CharLen := GetCharLength(AFieldType, ASize);

  if VarIsNull(AValue) then
  begin
    Result := StringOfChar(' ', CharLen);
    Exit;
  end;

  case AFieldType of
    ftString, ftWideString, ftFixedChar, ftFixedWideChar, ftMemo:
      begin
        Result := VarToStr(AValue);
        if Length(Result) > CharLen then
          Result := Copy(Result, 1, CharLen)
        else
          Result := Result + StringOfChar(' ', CharLen - Length(Result));
      end;

    ftInteger, ftSmallint, ftLargeint:
      begin
        Result := IntToStr(AValue);
        if Length(Result) > CharLen then
          Result := Copy(Result, 1, CharLen)
        else
          Result := StringOfChar(' ', CharLen - Length(Result)) + Result;  // Rechtsbündig
      end;

    ftFloat, ftCurrency:
      begin
        Result := VarToStr(AValue);
        if Length(Result) > CharLen then
          Result := Copy(Result, 1, CharLen)
        else
          Result := StringOfChar(' ', CharLen - Length(Result)) + Result;  // Rechtsbündig
      end;

    ftDate, ftTime, ftDateTime, ftTimeStamp:
      begin
        Result := VarToStr(AValue);
        if Length(Result) > CharLen then
          Result := Copy(Result, 1, CharLen)
        else
          Result := Result + StringOfChar(' ', CharLen - Length(Result));  // Linksbündig
      end;

    ftBoolean:
      begin
        if AValue then
          Result := '1'
        else
          Result := '0';
        // Kein Auffüllen nötig (CharLen = 1)
      end;
  else
    Result := VarToStr(AValue);
    if Length(Result) > CharLen then
      Result := Copy(Result, 1, CharLen)
    else
      Result := Result + StringOfChar(' ', CharLen - Length(Result));
  end;
end;

procedure TExternalTableWriter.AddRecord(ARow: TDbRowItem);
var
  i: Integer;
  Line: string;
  TotalLen: Integer;
begin
  // Zeile aus allen Feldern zusammensetzen
  Line := '';
  TotalLen := 0;

  for i := 0 to Length(FFieldsDef) - 1 do
  begin
    Line := Line + FormatFieldForExternal(
      ARow.Values[i],
      FFieldsDef[i].FieldType,
      FFieldsDef[i].Size
    );
    TotalLen := TotalLen + GetCharLength(FFieldsDef[i].FieldType, FFieldsDef[i].Size);
  end;

  // 👉 CRLF hinzufügen (Windows-Zeilenumbruch)
  Line := Line + #13#10;
  TotalLen := TotalLen + 2;

  // Schreiben
  FStream.Write(Line[1], TotalLen);
  Inc(FRecordCount);
end;

procedure TExternalTableWriter.Close;
begin
  if Assigned(FStream) then
  begin
    FStream.Free;
    FStream := nil;
  end;
end;

function TExternalTableWriter.GenerateCreateTableSQL(const ATableName: string): string;
var
  i: Integer;
  CharLen: Integer;
begin
  Result := 'CREATE TABLE "' + ATableName + '"' + sLineBreak +
            'EXTERNAL FILE ''' + FFileName + '''' + sLineBreak +
            '(' + sLineBreak;

  for i := 0 to Length(FFieldsDef) - 1 do
  begin
    CharLen := GetCharLength(FFieldsDef[i].FieldType, FFieldsDef[i].Size);
    Result := Result + '  "' + FFieldsDef[i].Name + '" CHAR(' + IntToStr(CharLen) + ')';
    if i < Length(FFieldsDef) - 1 then
      Result := Result + ',' + sLineBreak
    else
      Result := Result + sLineBreak;
  end;

  // 👉 CRLF-Spalte für Zeilenumbruch
  Result := Result + '  "CRLF" CHAR(2)' + sLineBreak +
            ');';
end;

end.
