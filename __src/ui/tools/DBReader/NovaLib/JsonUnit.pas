unit JsonUnit;
{ Author: Sergey Bodrov (serbod@gmail.com) 2016 }

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses SysUtils, Variants, VarLists, VarDicts, VarSerializeBase;

type
  {$ifndef UNICODE}
  RawByteString = AnsiString;
  {$endif}

  { TVariantSerializerJson }
  (*
  JSON serializer
  integers: string [-0123456789]
    0  123  -123
  numbers: string [-0123456789.E]
    0.0  123.0  1.23E+2  -1.6E-35
  strings: "<escaped UTF-8>"
    "test"  "some \"name\""
  lists: [<item>, <item>, ..]
    [1, "test"]
  dictionaries: {"name": <value>, "name2":<value2>, ..}
    { "age": 22, "name": "Mary", "childs": [] }
  value: <string> | <number> | <list> | <dict> | true | false | null
  *)
  TVariantSerializerJson = class(TVariantSerializer)
  private
    procedure WriteStorage(const AStorage: Variant; var AOutStr: RawByteString);
    procedure WriteJsonString(const AInStr: RawByteString; var AOutStr: RawByteString);
    function ReadJsonValue(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
    function ReadJsonString(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
    function ReadJsonList(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
    function ReadJsonDictionary(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
    function ReadJsonNumber(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
  public
    function GetName(): string; override;
    function VariantToString(const AStorage: Variant): RawByteString; override;
    function VariantFromString(const AString: RawByteString): Variant; override;
    function VariantToFile(const AStorage: Variant; AFileName: string): Boolean; override;
    function VariantFromFile(AFileName: string): Variant; override;
  end;

  function VariantToFileJson(const AValue: Variant; AFileName: string): Boolean;
  function VariantFromFileJson(AFileName: string): Variant;

implementation

function VariantToFileJson(const AValue: Variant; AFileName: string): Boolean;
var
  ser: TVariantSerializerJson;
begin
  ser := TVariantSerializerJson.Create();
  try
    Result := ser.VariantToFile(AValue, AFileName);
  finally
    ser.Free();
  end;
end;

function VariantFromFileJson(AFileName: string): Variant;
var
  ser: TVariantSerializerJson;
begin
  ser := TVariantSerializerJson.Create();
  try
    Result := ser.VariantFromFile(AFileName);
  finally
    ser.Free();
  end;
end;

{ TVariantSerializerJson }

function TVariantSerializerJson.GetName: string;
begin
  Result := 'JSON';
end;

function TVariantSerializerJson.ReadJsonDictionary(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): Variant;
var
  sName: RawByteString;
  bNamePart: Boolean;
  bValueReaded: Boolean;
  SubStorage: Variant;
begin
  if AString[APos] = '{' then
    Inc(APos)
  else
  begin
    Result := Null;
    Exit;
  end;

  Result := VarDictCreate();
  bNamePart := True;
  sName := '';
  bValueReaded := False;
  while APos <= ALen do
  begin
    case AString[APos] of
      '}':
      begin
        // end of dictionary
        Inc(APos);
        Exit;
      end;

      ':':
      begin
        // name-value separator
        bNamePart := False;
        Inc(APos);
      end;

      '"':
      begin
        // name begin
        if bNamePart then
          sName := ReadJsonString(AString, APos, ALen)
        else
        if (not bValueReaded) then
        begin
          SubStorage := ReadJsonValue(AString, APos, ALen);
          Result.SetValue(sName, SubStorage);
          bValueReaded := True;
        end
        else
          Inc(APos);
      end;

      ',':
      begin
        // next name-value pair
        bNamePart := True;
        bValueReaded := False;
        sName := '';
        Inc(APos);
      end;
    else
      if (not bNamePart) and (not bValueReaded) and (Pos(AString[APos], '{[0123456789+-.eE"') > 0) then
      begin
        // value
        SubStorage := ReadJsonValue(AString, APos, ALen);
        Result.SetValue(sName, SubStorage);
        bValueReaded := True;
      end
      else
        Inc(APos);
    end;
  end;
  Result := Null;
end;

function TVariantSerializerJson.ReadJsonList(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): Variant;
var
  bValueReaded: Boolean;
  SubStorage: Variant;
begin
  if AString[APos] = '[' then
    Inc(APos)
  else
  begin
    Result := Null;
    Exit;
  end;

  Result := VarListCreate();
  bValueReaded := False;
  while APos <= ALen do
  begin
    case AString[APos] of
      ']':
      begin
        // end of list
        Inc(APos);
        Exit;
      end;

      ',':
      begin
        // next name-value pair
        bValueReaded := False;
        Inc(APos);
      end;
    else
      if (not bValueReaded) and (Pos(AString[APos], '{[0123456789+-.eE"') > 0) then
      begin
        // value
        SubStorage := ReadJsonValue(AString, APos, ALen);
        Result.AddValue(SubStorage);
        bValueReaded := True;
      end
      else
        Inc(APos);
    end;
  end;
  Result := Null;
end;

function TVariantSerializerJson.ReadJsonNumber(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): Variant;
var
  s: RawByteString;
begin
  s := '';
  while (APos <= ALen)
  and (Pos(AString[APos], '+-0123456789.eE') > 0) do
  begin
    s := s + AString[APos];
    Inc(APos);
  end;

  if Pos('.', s) > 0 then
    Result := StrToFloatDef(s, 0)
  else
    Result := StrToInt64Def(s, 0);
end;

function TVariantSerializerJson.ReadJsonString(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): RawByteString;
var
  TmpStr: ShortString;
  sHex: String;

  function HexChar(c: Char): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      Result := 0;
    end;
  end;

  function HexToUtf8(const AHexStr: RawByteString): UTF8String;
  var
    i: Integer;
    n: Cardinal;
  begin
    // hex to int
    n := 0;
    for i := 0 to Length(AHexStr)-1 do
    begin
      n := n or (HexChar(AHexStr[i+1]) shl (4 * (4-i)));
    end;
    // int to utf-8
    if n < 128 then
      Result := Chr(n)
    else if n < 2048 then
    begin
      Result := Chr((n shr 6) + 192)
              + Chr((n and 63) + 128);
    end
    else if n < 65536 then
    begin
      Result := Chr((n shr 12) + 224)
              + Chr(((n shr 6) and 63) + 128)
              + Chr((n and 63) + 128);
    end
    else if n < 2097152 then
    begin
      Result := Chr((n shr 18) + 240)
              + Chr(((n shr 12) and 63) + 128)
              + Chr(((n shr 6) and 63) + 128)
              + Chr((n and 63) + 128);
    end;
  end;

var
  c: AnsiChar;
begin
  Result := '';
  if AString[APos] = '"' then
    Inc(APos)
  else
    Exit;

  TmpStr := '';
  while (APos <= ALen) do
  begin
    c := AString[APos];
    if c = '\' then
    begin
      Inc(APos);
      c := AString[APos];
      if APos >= ALen then Break;
      case c of
        'b':  TmpStr := TmpStr + #$08; // backspace
        't':  TmpStr := TmpStr + #$09; // tab
        'n':  TmpStr := TmpStr + #$0A; // line feed
        'f':  TmpStr := TmpStr + #$0C; // form feed
        'r':  TmpStr := TmpStr + #$0D; // carriage return
        'u':
        begin
          // utf-8 character
          sHex := Copy(AString, APos+1, 4);
          if Length(sHex) = 4 then
          begin
            TmpStr := TmpStr + HexToUtf8(sHex);
          end;
          Inc(APos, 4);
        end;
      else
        TmpStr := TmpStr + c;
      end;
    end
    else if c = '"' then
    begin
      Inc(APos);
      Break;
    end
    else
      TmpStr := TmpStr + c;

    Inc(APos);

    if Length(TmpStr) > 200 then
    begin
      Result := Result + TmpStr;
      TmpStr := '';
    end;
  end;

  Result := Result + TmpStr;
end;

function TVariantSerializerJson.ReadJsonValue(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): Variant;
var
  c: AnsiChar;
begin
  Result := Unassigned;
  while VarIsEmpty(Result) and (APos <= ALen) do
  begin
    c := AString[APos];
    case c of
      '"':
      begin
        // read string value
        Result := ReadJsonString(AString, APos, ALen);
      end;

      '[':
      begin
        // read list value
        Result := ReadJsonList(AString, APos, ALen);
      end;

      '{':
      begin
        // read dictionary value
        Result := ReadJsonDictionary(AString, APos, ALen);
      end;

      't':
      begin
        // true value
        if Copy(AString, APos, 4) = 'true' then
        begin
          Inc(APos, 3);
          Result := True;
        end;
      end;

      'f':
      begin
        // false value
        if Copy(AString, APos, 5) = 'false' then
        begin
          Inc(APos, 4);
          Result := False;
        end;
      end;

      'n':
      begin
        // null value
        if Copy(AString, APos, 4) = 'null' then
        begin
          Inc(APos, 3);
          Result := Null;
        end;
      end;

    else
      if Pos(c, '+-0123456789.eE') > 0 then
      begin
        // read number value
        Result := ReadJsonNumber(AString, APos, ALen);
      end
    end;

    if VarIsEmpty(Result) then
      Inc(APos);
  end;
end;

function TVariantSerializerJson.VariantFromFile(AFileName: string): Variant;
begin
  Result := Null;
  if Trim(AFileName) = '' then
    Exit;
  if Pos('.json', AFileName) < (Length(AFileName) - 2) then
    AFileName := AFileName + '.json';
  Result := Self.VariantFromString(FileToStr(AFileName));
end;

function TVariantSerializerJson.VariantFromString(const AString: RawByteString
  ): Variant;
var
  n: Cardinal;
begin
  n := 1;
  Result := ReadJsonValue(AString, n, Length(AString));
end;

function TVariantSerializerJson.VariantToFile(const AStorage: Variant;
  AFileName: string): Boolean;
begin
  Result := False;
  if Trim(AFileName) = '' then
    Exit;
  if Pos('.json', AFileName) < (Length(AFileName) - 2) then
    AFileName := AFileName + '.json';
  Result := StrToFile(AFileName, Self.VariantToString(AStorage));
end;

function TVariantSerializerJson.VariantToString(const AStorage: Variant): RawByteString;
begin
  Result := '';
  WriteStorage(AStorage, Result);
end;

procedure TVariantSerializerJson.WriteJsonString(const AInStr: RawByteString;
  var AOutStr: RawByteString);
var
  TmpStr: ShortString;
  c: AnsiChar;
  i: Integer;
begin
  TmpStr := '"';
  for i := 1 to Length(AInStr) do
  begin
    c := AInStr[i];
    case c of
      '\':  TmpStr := TmpStr + '\\';
      '/':  TmpStr := TmpStr + '\/';
      '"':  TmpStr := TmpStr + '\"';
      #$08: TmpStr := TmpStr + '\b';  // backspace
      #$09: TmpStr := TmpStr + '\t';  // horizontal tab
      #$0A: TmpStr := TmpStr + '\n';  // line feed
      #$0C: TmpStr := TmpStr + '\f';  // form feed
      #$0D: TmpStr := TmpStr + '\r';  // carriage return
    else
      if Ord(c) < 32 then
        // control characters
        TmpStr := TmpStr + '\u' + IntToHex(Ord(c), 4)
      else
        TmpStr := TmpStr + c;
    end;
    if Length(TmpStr) > 200 then
    begin
      AOutStr := AOutStr + TmpStr;
      TmpStr := '';
    end;
  end;
  TmpStr := TmpStr + '"';
  AOutStr := AOutStr + TmpStr;
end;

procedure TVariantSerializerJson.WriteStorage(const AStorage: Variant;
  var AOutStr: RawByteString);
var
  s, sName: RawByteString;
  SubItem: Variant;
  i: integer;
begin
  if VarIsOrdinal(AStorage) then
  begin
    AOutStr := AOutStr + VarToStrDef(AStorage, '0');
  end
  else
  if VarIsNumeric(AStorage) then
  begin
    AOutStr := AOutStr + VarToStrDef(AStorage, '0');
    if Pos('.', s) = 0 then
      AOutStr := AOutStr + '.0';
  end
  else
  if VarIsArray(AStorage) then
  begin
    AOutStr := AOutStr + '[';
    for i := VarArrayLowBound(AStorage, 1) to VarArrayHighBound(AStorage, 1) do
    begin
      if i > 0 then
        AOutStr := AOutStr + ',';
      SubItem := VarArrayGet(AStorage, [i]);
      // value
      WriteStorage(SubItem, AOutStr);
    end;
    AOutStr := AOutStr + ']';
  end
  else
  if VarIsType(AStorage, VarTypeDict) then
  begin
    AOutStr := AOutStr + '{';
    for i := 0 to AStorage.GetCount() - 1 do
    begin
      // item separator
      if i > 0 then
        AOutStr := AOutStr + ',';
      // name
      sName := AStorage.GetName(i);
      WriteJsonString(sName, AOutStr);
      // name-value separator
      AOutStr := AOutStr + ':';
      // value
      SubItem := AStorage.GetValue(i);
      WriteStorage(SubItem, AOutStr);
    end;
    AOutStr := AOutStr + '}';
  end
  else
  if VarIsType(AStorage, VarTypeList) then
  begin
    AOutStr := AOutStr + '[';
    for i := 0 to AStorage.GetCount() - 1 do
    begin
      if i > 0 then
        AOutStr := AOutStr + ',';
      SubItem := AStorage.GetValue(i);
      // value
      WriteStorage(SubItem, AOutStr);
    end;
    AOutStr := AOutStr + ']';
  end
  else
  begin
    // string
    WriteJsonString(VarToStr(AStorage), AOutStr);
  end;
end;

end.
