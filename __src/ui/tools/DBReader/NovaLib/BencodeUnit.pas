unit BencodeUnit;
{ Author: Sergey Bodrov (serbod@gmail.com) 2020 }

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Types, Variants, VarLists, VarDicts, VarSerializeBase;

type
  {$ifndef UNICODE}
  RawByteString = AnsiString;
  {$endif}

  { TVariantSerializerBencode }
  {
  Bencode serializer
  integers: i<value>e
    i0e  i42e  i-42e
  strings: <value_len>:<value>
    3:ben  4:code
  lists: l<items>e (without any spaces)
    l i42e 3:ben 4:code e
  dictionaries: d<items>e  where items is <string_name><value>
    d 4:name 3:ben  4:code i42e e
  }
  TVariantSerializerBencode = class(TVariantSerializer)
  private
    function VariantToBencode(const AStorage: Variant): RawByteString;
    function VariantToStream(const AStorage: Variant; AStream: TStream): Boolean;
    function ReadBencodeValue(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
    function ReadBencodeIntegerStr(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
    function ReadBencodeString(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
    function ReadBencodeList(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
    function ReadBencodeDictionary(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
  public
    function GetName(): string; override;
    function VariantToString(const AValue: Variant): RawByteString; override;
    function VariantFromString(const AString: RawByteString): Variant; override;
    function VariantToFile(const AStorage: Variant; AFileName: string): Boolean; override;
    function VariantFromFile(AFileName: string): Variant; override;
  end;

  function VariantToFileBencode(const AValue: Variant; AFileName: string): Boolean;
  function VariantFromFileBencode(AFileName: string): Variant;

implementation

function VariantToFileBencode(const AValue: Variant; AFileName: string): Boolean;
var
  ser: TVariantSerializerBencode;
begin
  ser := TVariantSerializerBencode.Create();
  try
    Result := ser.VariantToFile(AValue, AFileName);
  finally
    ser.Free();
  end;
end;

function VariantFromFileBencode(AFileName: string): Variant;
var
  ser: TVariantSerializerBencode;
begin
  ser := TVariantSerializerBencode.Create();
  try
    Result := ser.VariantFromFile(AFileName);
  finally
    ser.Free();
  end;
end;

{ TVariantSerializerBencode }

function TVariantSerializerBencode.GetName: string;
begin
  Result := 'BENCODE';
end;

function TVariantSerializerBencode.VariantToBencode(const AStorage: Variant): RawByteString;
var
  sName: RawByteString;
  SubItem: Variant;
  i: integer;
  s: RawByteString;
begin
  Result := '';
  if VarIsOrdinal(AStorage) then
  begin
    Result := Result + 'i' + VarToStrDef(AStorage, '0') + 'e';
  end
  else
  if VarIsArray(AStorage) then
  begin
    Result := Result + 'l';
    for i := VarArrayLowBound(AStorage, 1) to VarArrayHighBound(AStorage, 1) do
    begin
      SubItem := VarArrayGet(AStorage, [i]);
      // value
      Result := Result + VariantToBencode(SubItem);
    end;
    Result := Result + 'e';
  end
  else
  if VarIsType(AStorage, VarTypeDict) then
  begin
    Result := Result + 'd';
    for i := 0 to AStorage.GetCount() - 1 do
    begin
      sName := AStorage.GetName(i);
      SubItem := AStorage.GetValue(i);
      // name
      Result := Result + IntToStr(Length(sName)) + ':' + sName;
      // value
      Result := Result + VariantToBencode(SubItem);
    end;
    Result := Result + 'e';
  end
  else
  if VarIsType(AStorage, VarTypeList) then
  begin
    Result := Result + 'l';
    for i := 0 to AStorage.GetCount() do
    begin
      SubItem := AStorage.GetValue(i);
      // value
      Result := Result + VariantToBencode(SubItem);
    end;
    Result := Result + 'e';
  end
  else
  begin
    // string
    s := VarToStr(AStorage);
    Result := Result + IntToStr(Length(s)) + ':' + s;
  end;
end;

function TVariantSerializerBencode.VariantToStream(const AStorage: Variant;
  AStream: TStream): Boolean;

procedure WriteStr(const AStr: UTF8String);
begin
  if Length(AStr) > 0 then
    AStream.Write(AStr[1], Length(AStr));
end;

var
  sName: string;
  SubItem: Variant;
  i: integer;
  s: string;
begin
  if VarIsOrdinal(AStorage) then
  begin
    WriteStr('i' + VarToStrDef(AStorage, '0') + 'e');
  end
  else
  if VarIsArray(AStorage) then
  begin
    WriteStr('l');
    for i := VarArrayLowBound(AStorage, 1) to VarArrayHighBound(AStorage, 1) do
    begin
      SubItem := VarArrayGet(AStorage, [i]);
      // value
      VariantToStream(SubItem, AStream);
    end;
    WriteStr('e');
  end
  else
  if VarIsType(AStorage, VarTypeDict) then
  begin
    WriteStr('d');
    for i := 0 to AStorage.GetCount() - 1 do
    begin
      sName := AStorage.GetName(i);
      SubItem := AStorage.GetValue(i);
      // name
      WriteStr(IntToStr(Length(sName)) + ':' + sName);
      // value
      VariantToStream(SubItem, AStream);
    end;
    WriteStr('e');
  end
  else
  if VarIsType(AStorage, VarTypeList) then
  begin
    WriteStr('l');
    for i := 0 to AStorage.GetCount() do
    begin
      SubItem := AStorage.GetValue(i);
      // value
      VariantToStream(SubItem, AStream);
    end;
    WriteStr('e');
  end
  else
  begin
    // string
    s := VarToStr(AStorage);
    WriteStr(IntToStr(Length(s)) + ':' + s);
  end;

  Result := True;
end;

function TVariantSerializerBencode.ReadBencodeIntegerStr(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
begin
  Result := '';
  if AString[APos] = 'i' then
    Inc(APos)
  else
    Exit;
  while APos <= ALen do
  begin
    if AString[APos] = 'e' then
    begin
      Inc(APos);
      Break
    end;
    Result := Result + AString[APos];
    Inc(APos);
  end;
end;

function TVariantSerializerBencode.ReadBencodeString(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
var
  sValue: RawByteString;
  ValueLen: Cardinal;
begin
  Result := '';
  sValue := '';
  while APos <= ALen do
  begin
    if AString[APos] = ':' then
    begin
      ValueLen := StrToIntDef(sValue, 0);
      Result := Copy(AString, APos + 1, ValueLen);
      APos := APos + ValueLen + 1;
      Exit;
    end;
    sValue := sValue + AString[APos];
    Inc(APos);
  end;
end;

function TVariantSerializerBencode.ReadBencodeDictionary(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): Variant;
var
  sName: RawByteString;
  SubStorage: Variant;
begin
  Result := Null;
  if AString[APos] = 'd' then
    Inc(APos)
  else
    Exit;

  Result := VarDictCreate();
  while APos <= ALen do
  begin
    if AString[APos] = 'e' then
    begin
      Inc(APos);
      Exit;
    end;
    sName := ReadBencodeString(AString, APos, ALen);
    Assert(sName <> '', 'Empty name of dict value');
    SubStorage := ReadBencodeValue(AString, APos, ALen);
    Result.SetValue(sName, SubStorage);
  end;
  Assert(False, 'End of dict not found');
  Result := Null;
end;

function TVariantSerializerBencode.ReadBencodeList(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): Variant;
var
  SubStorage: Variant;
begin
  Result := Null;
  if AString[APos] = 'l' then
    Inc(APos)
  else
    Exit;

  Result := VarListCreate();
  while APos <= ALen do
  begin
    if AString[APos] = 'e' then
    begin
      Inc(APos);
      Exit;
    end;
    SubStorage := ReadBencodeValue(AString, APos, ALen);
    Result.AddValue(SubStorage);
  end;
  Assert(False, 'End of list not found');
  Result := Null;
end;

function TVariantSerializerBencode.ReadBencodeValue(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): Variant;
begin
  Result := Null;
  if APos <= ALen then
  begin
    if AString[APos] = 'i' then
    begin
      // read integer value
      Result := StrToInt64Def(ReadBencodeIntegerStr(AString, APos, ALen), 0);
    end

    else if Pos(AString[APos], '0123456789') > 0 then
    begin
      // read string value
      Result := ReadBencodeString(AString, APos, ALen);
    end

    else if AString[APos] = 'd' then
    begin
      // read dictionary value
      Result := ReadBencodeDictionary(AString, APos, ALen);
    end

    else if AString[APos] = 'l' then
    begin
      // read list value
      Result := ReadBencodeList(AString, APos, ALen);
    end

    else
    begin
      // error
      Assert(1=0, 'Bencode parsing error, index=' + IntToStr(APos) + ' char=' + AString[APos]);
      Exit;
    end;
  end;
end;

function TVariantSerializerBencode.VariantToString(const AValue: Variant): RawByteString;
begin
  Result := VariantToBencode(AValue);
end;

function TVariantSerializerBencode.VariantFromString(const AString: RawByteString): Variant;
var
  n: Cardinal;
begin
  n := 1;
  Result := ReadBencodeValue(AString, n, Length(AString));
  Assert(Result <> Null, 'Read value failed, len='+IntToStr(Length(AString)));
end;

function TVariantSerializerBencode.VariantToFile(const AStorage: Variant; AFileName: string): Boolean;
var
  ms: TMemoryStream;
begin
  Result := False;
  if Trim(AFileName) = '' then
    Exit;
  if Pos('.be', AFileName) < (Length(AFileName) - 2) then
    AFileName := AFileName + '.be';
  //Result := StrToFile(AFileName, Self.StorageToString(AStorage));
  ms := TMemoryStream.Create();
  try
    //ms.Size := Trunc(GetSize(AStorage) * 1.1);
    VariantToStream(AStorage, ms);
    ms.SaveToFile(AFileName);
  finally
    ms.Free();
  end;
end;

function TVariantSerializerBencode.VariantFromFile(AFileName: string): Variant;
begin
  Result := Null;
  if Trim(AFileName) = '' then
    Exit;
  if Pos('.be', AFileName) < (Length(AFileName) - 2) then
    AFileName := AFileName + '.be';
  Result := Self.VariantFromString(FileToStr(AFileName));
end;

initialization

DataFormatSettings.DecimalSeparator := '.';

end.
