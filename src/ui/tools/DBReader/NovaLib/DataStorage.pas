unit DataStorage;
{ Author: Sergey Bodrov (serbod@gmail.com) 2010-2020 }

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Types;

type
  TDataStorageType = (stUnknown, stString, stInteger, stNumber, stList, stDictionary);

  {$ifndef UNICODE}
  RawByteString = AnsiString;
  {$endif}

  { IDataStorage }

  IDataStorage = interface
    { Items count for (List, Dictionary) types }
    function GetCount(): integer;
    //procedure SetStorageType(const AValue: TDataStorageType);
    function GetStorageType(): TDataStorageType;
    { Set value, if storage type is Dictionary, then AName used }
    procedure SetValue(AValue: IDataStorage; const AName: string = ''); overload;
    procedure SetValue(AValue: RawByteString; const AName: string = ''); overload;
    procedure SetValue(AValue: TByteDynArray; const AName: string = ''); overload;
    procedure SetValue(AValue: Integer; const AName: string = ''); overload;
    procedure SetValue(AValue: Int64; const AName: string = ''); overload;
    procedure SetValue(AValue: Real; const AName: string = ''); overload;
    procedure SetValue(AValue: Boolean; const AName: string = ''); overload;

    { get storage value }
    function GetValue(): RawByteString;
    { Get storage item by name }
    function GetObject(const AName: string): IDataStorage; overload;
    { Get storage item by index }
    function GetObject(Index: integer): IDataStorage; overload;
    { Get name by index }
    function GetObjectName(Index: integer): string;
    { Get string by name (from dictionary). If name empty, get value }
    function GetString(const AName: string = ''): string;
    function GetBytes(const AName: string = ''): TByteDynArray;
    function GetInteger(const AName: string = ''): Integer;
    function GetInt64(const AName: string = ''): Int64;
    function GetCardinal(const AName: string = ''): Cardinal;
    function GetReal(const AName: string = ''): Real;
    function GetBool(const AName: string = ''): Boolean;
    function HaveName(const AName: string): Boolean;
    procedure Clear();
    { get length of all contained items and subitems, including dictionary names }
    function GetSize(): Int64;
  end;

  { TDataStorage }
  TDataStorage = class(TInterfacedObject, IDataStorage)
  private
    { stUnknown, stString, stInteger, stNumber, stList, stDictionary }
    FStorageType: TDataStorageType;
    { Value for (String, Integer, Number) types }
    FValue: RawByteString;
    FIntfList: TInterfaceList;
    { [name:object] items storage }
    FItems: TStringList;
    procedure AddValue(AStorageType: TDataStorageType; const AName: string; const AValue: RawByteString);
  public
    constructor Create(AStorageType: TDataStorageType);
    destructor Destroy(); override;
    { Items count for (List, Dictionary) types }
    function GetCount(): integer;
    //procedure SetStorageType(const AValue: TDataStorageType);
    function GetStorageType(): TDataStorageType;
    { Set value, if storage type is Dictionary, then AName used
      for (List) is add value}
    procedure SetValue(AValue: IDataStorage; const AName: string = ''); overload;
    procedure SetValue(AValue: RawByteString; const AName: string = ''); overload;
    procedure SetValue(AValue: TByteDynArray; const AName: string = ''); overload;
    procedure SetValue(AValue: Integer; const AName: string = ''); overload;
    procedure SetValue(AValue: Int64; const AName: string = ''); overload;
    procedure SetValue(AValue: Real; const AName: string = ''); overload;
    procedure SetValue(AValue: Boolean; const AName: string = ''); overload;
    { get storage value }
    function GetValue(): RawByteString;
    { set storage value }
    procedure SetValueStr(const AValue: RawByteString); overload;
    { Get storage item by name }
    function GetObject(const AName: string): IDataStorage; overload;
    { Get storage item by index }
    function GetObject(Index: integer): IDataStorage; overload;
    { Get name by index }
    function GetObjectName(Index: integer): string;
    { Get string by name (from dictionary). If name empty, get value }
    function GetString(const AName: string = ''): string;
    function GetBytes(const AName: string = ''): TByteDynArray;
    function GetInteger(const AName: string = ''): Integer;
    function GetInt64(const AName: string = ''): Int64;
    function GetCardinal(const AName: string = ''): Cardinal;
    function GetReal(const AName: string = ''): Real;
    function GetBool(const AName: string = ''): Boolean;
    function HaveName(const AName: string): Boolean;
    procedure Clear();
    { get size of all contained items and subitems, including dictionary names:
      numerics - SizeOf()
      strings - Length() }
    function GetSize(): Int64;

    property Count: Integer read GetCount;
    { stUnknown, stString, stInteger, stNumber, stList, stDictionary }
    property StorageType: TDataStorageType read GetStorageType;
    { Value for (String, Integer, Number) types }
    property Value: RawByteString read GetValue write SetValueStr;
  end;


  { TDataSerializer }

  TDataSerializer = class(TObject)
  public
    function GetName(): string; virtual;
    // Serialize storage to string
    function StorageToString(AStorage: IDataStorage): RawByteString; virtual; abstract;
    // De-serialize string into AStorage (not nil)
    function StorageFromString(const AString: RawByteString): IDataStorage; virtual; abstract;
    // Save storage to file. Filename must be without extension
    function StorageToFile(AStorage: IDataStorage; AFileName: string): Boolean; virtual; abstract;
    // Fill AStorage (not nil) from file. Filename must be without extension
    function StorageFromFile(AFileName: string): IDataStorage; virtual; abstract;
  end;

  { TDataSerializerBencode }
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
  TDataSerializerBencode = class(TDataSerializer)
  private
    function StorageToBencode(AStorage: IDataStorage): RawByteString;
    function StorageToStream(AStorage: IDataStorage; AStream: TStream): Boolean;
    function ReadBencodeValue(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): IDataStorage;
    function ReadBencodeIntegerStr(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
    function ReadBencodeString(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
    function ReadBencodeList(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): IDataStorage;
    function ReadBencodeDictionary(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): IDataStorage;
  public
    function GetName(): string; override;
    function StorageToString(AStorage: IDataStorage): RawByteString; override;
    function StorageFromString(const AString: RawByteString): IDataStorage; override;
    function StorageToFile(AStorage: IDataStorage; AFileName: string): Boolean; override;
    function StorageFromFile(AFileName: string): IDataStorage; override;
  end;


  // shared functions
function StrToFile(const FileName, Str: RawByteString): Boolean;

function FileToStr(const FileName: string): RawByteString;

var
  DataFormatSettings: TFormatSettings;

implementation

function StreamToStr(AStream: TStream): RawByteString;
var
  ss: TStringStream;
begin
  Result := '';
  ss := TStringStream.Create('');
  try
    AStream.Seek(0, soFromBeginning);
    ss.Size := AStream.Size;
    ss.Position := 0;
    ss.CopyFrom(AStream, AStream.Size);
    Result := ss.DataString;
  finally
    ss.Free();
  end;
end;

function StrToStream(const s: RawByteString; AStream: TStream): Boolean;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(s);
  try
    ss.Seek(0, soFromBeginning);
    AStream.Seek(0, soFromBeginning);
    AStream.CopyFrom(ss, ss.Size);
    Result := True;
  finally
    ss.Free();
  end;
end;

function StrToFile(const FileName, Str: RawByteString): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  try
    fs := TFileStream.Create(FileName, fmCreate);
  except
    fs := nil;
  end;

  if not Assigned(fs) then
    Exit;
  try
    StrToStream(Str, fs);
    Result := True;
  finally
    FreeAndNil(fs);
  end;
end;

function FileToStr(const FileName: string): RawByteString;
var
  fs: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  try
    fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    fs := nil;
  end;

  if not Assigned(fs) then
    Exit;
  try
    Result := StreamToStr(fs);
  finally
    fs.Free();
  end;
end;

function RawToBytes(const AValue: UTF8String): TByteDynArray;
var
  n: integer;
begin
  n := Length(AValue);
  SetLength(Result, n);
  if n > 0 then
    Move(AValue[1], Result[0], n);
end;

function BytesToRaw(const AValue: TByteDynArray): UTF8String;
var
  n: integer;
begin
  n := Length(AValue);
  SetLength(Result, n);
  if n > 0 then
    Move(AValue[0], Result[1], n);
end;

{ TDataStorage }

constructor TDataStorage.Create(AStorageType: TDataStorageType);
begin
  inherited Create();
  FStorageType := AStorageType;
  FValue := '';
  FItems := TStringList.Create();
  FIntfList := TInterfaceList.Create();
  //FItems.OwnsObjects := True;
end;

destructor TDataStorage.Destroy();
begin
  Clear();
  FreeAndNil(FIntfList);
  FreeAndNil(FItems);
  inherited Destroy();
end;

function TDataStorage.GetCount(): integer;
begin
  Result := FItems.Count;
end;

function TDataStorage.GetObject(const AName: string): IDataStorage;
var
  n: integer;
begin
  n := FItems.IndexOf(AName);
  if n >= 0 then
  begin
    Result := IDataStorage(FIntfList[n]);
    if Result.GetStorageType() in [stList, stDictionary] then
      Exit;
  end;
  Result := nil;
end;

function TDataStorage.GetObject(Index: integer): IDataStorage;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
  begin
    Result := IDataStorage(FIntfList[Index]);
  end;
end;

function TDataStorage.GetObjectName(Index: integer): string;
begin
  Result := '';
  if (Index >= 0) and (Index < FItems.Count) then
    Result := FItems[Index];
end;

function TDataStorage.GetString(const AName: string): string;
var
  n: integer;
  TmpItem: IDataStorage;
begin
  Result := '';
  if AName = '' then
    Result := FValue
  else
  begin
    n := FItems.IndexOf(AName);
    if n <> -1 then
    begin
      TmpItem := IDataStorage(FIntfList[n]);
      Result := TmpItem.GetValue();
      //if TmpItem.StorageType=stString then Result:=TmpItem.Value;
    end;
  end;
end;

function TDataStorage.GetBytes(const AName: string = ''): TByteDynArray;
var
  n: integer;
  TmpItem: IDataStorage;
begin
  if AName = '' then
  begin
    Result := RawToBytes(FValue);
  end
  else
  begin
    n := FItems.IndexOf(AName);
    if n <> -1 then
    begin
      TmpItem := IDataStorage(FIntfList[n]);
      Result := RawToBytes(TmpItem.GetValue());
      //if TmpItem.StorageType=stString then Result:=TmpItem.Value;
    end;
  end;
end;

function TDataStorage.GetInteger(const AName: string): Integer;
begin
  Result := StrToIntDef(GetString(AName), 0);
end;

function TDataStorage.GetInt64(const AName: string = ''): Int64;
begin
  Result := StrToInt64Def(GetString(AName), 0);
end;

function TDataStorage.GetCardinal(const AName: string): Cardinal;
begin
  Result := StrToInt64Def(GetString(AName), 0);
end;

function TDataStorage.GetReal(const AName: string): Real;
begin
  Result := StrToFloatDef(GetString(AName), 0, DataFormatSettings);
end;

function TDataStorage.GetBool(const AName: string): Boolean;
begin
  Result := (GetString(AName) = '1');
end;

function TDataStorage.HaveName(const AName: string): Boolean;
begin
  Result := (FItems.IndexOf(AName) <> -1);
end;

procedure TDataStorage.Clear();
{var
  i: Integer;
  TmpObj: TObject; }
begin
  {for i := Count-1 downto 0 do
  begin
    TmpObj := FItems.Objects[i];
    FItems.Objects[i] := nil;
    TmpObj.Free();
  end; }

  FItems.Clear();
  FIntfList.Clear();
  FValue := '';
end;

function TDataStorage.GetStorageType(): TDataStorageType;
begin
  Result := FStorageType;
end;

function TDataStorage.GetValue(): RawByteString;
begin
  Result := FValue;
end;

procedure TDataStorage.AddValue(AStorageType: TDataStorageType;
  const AName: string; const AValue: RawByteString);
var
  TmpItem: IDataStorage;
begin
  TmpItem := TDataStorage.Create(AStorageType);
  TmpItem.SetValue(AValue);

  FItems.Add(AName);
  FIntfList.Add(TmpItem);
end;

procedure TDataStorage.SetValue(AValue: RawByteString; const AName: string);
begin
  if (FStorageType = stDictionary) or (FStorageType = stList) then
  begin
    AddValue(stString, AName, AValue);
  end
  else
    FValue := AValue;
end;

procedure TDataStorage.SetValue(AValue: TByteDynArray; const AName: string = '');
begin
  if (FStorageType = stDictionary) or (FStorageType = stList) then
  begin
    AddValue(stString, AName, BytesToRaw(AValue));
  end
  else
    FValue := BytesToRaw(AValue);
end;

procedure TDataStorage.SetValue(AValue: IDataStorage; const AName: string);
begin
  if (FStorageType = stDictionary) or (FStorageType = stList) then
  begin
    if Assigned(AValue) then
    begin
      FIntfList.Add(AValue);
      FItems.Add(AName);
    end;
  end
  else
  begin
    // not valid for current storage type
  end;
end;

procedure TDataStorage.SetValue(AValue: Integer; const AName: string);
begin
  if (FStorageType = stDictionary) or (FStorageType = stList) then
  begin
    AddValue(stInteger, AName, IntToStr(AValue));
  end
  else
    FValue := IntToStr(AValue);
end;

procedure TDataStorage.SetValue(AValue: Int64; const AName: string);
begin
  if (FStorageType = stDictionary) or (FStorageType = stList) then
  begin
    AddValue(stInteger, AName, IntToStr(AValue));
  end
  else
    FValue := IntToStr(AValue);
end;

procedure TDataStorage.SetValue(AValue: Boolean; const AName: string);
begin
  if AValue then
    Self.SetValue('1', AName)
  else
    Self.SetValue('0', AName);
end;

procedure TDataStorage.SetValue(AValue: Real; const AName: string);
begin
  if (FStorageType = stDictionary) or (FStorageType = stList) then
  begin
    AddValue(stNumber, AName, FloatToStr(AValue, DataFormatSettings));
  end
  else
    FValue := FloatToStr(AValue, DataFormatSettings);
end;

procedure TDataStorage.SetValueStr(const AValue: RawByteString);
begin
  SetValue(AValue);
end;

function TDataStorage.GetSize(): Int64;
var
  i: Integer;
begin
  case FStorageType of
    stUnknown: Result := 0;
    stString, stInteger, stNumber: Result := Length(FValue);
    stList, stDictionary:
    begin
      Result := 0;
      for i := 0 to Count-1 do
      begin
        Result := Length(GetObjectName(i)) + GetObject(i).GetSize();
      end;
    end;
  else
    Result := 0;
  end;
end;

{ TDataSerializer }

function TDataSerializer.GetName: string;
begin
  Result := 'NONE';
end;

{ TDataSerializerBencode }

function TDataSerializerBencode.GetName: string;
begin
  Result := 'BENCODE';
end;

function TDataSerializerBencode.StorageToBencode(AStorage: IDataStorage): RawByteString;
var
  sName: RawByteString;
  SubItem: IDataStorage;
  i: integer;
  s: RawByteString;
begin
  Result := '';
  case AStorage.GetStorageType() of
    stString:
    begin
      s := AStorage.GetValue();
      Result := Result + IntToStr(Length(s)) + ':' + s;
    end;

    stNumber:
    begin
      s := AStorage.GetValue();
      Result := Result + IntToStr(Length(s)) + ':' + s;
    end;

    stInteger:
    begin
      Result := Result + 'i' + AStorage.GetValue() + 'e';
    end;

    stDictionary:
    begin
      Result := Result + 'd';
      for i := 0 to AStorage.GetCount() - 1 do
      begin
        sName := AStorage.GetObjectName(i);
        SubItem := AStorage.GetObject(i);
        // name
        Result := Result + IntToStr(Length(sName)) + ':' + sName;
        // value
        Result := Result + StorageToBencode(SubItem);
      end;
      Result := Result + 'e';
    end;

    stList:
    begin
      Result := Result + 'l';
      for i := 0 to AStorage.GetCount() - 1 do
      begin
        SubItem := AStorage.GetObject(i);
        // value
        Result := Result + StorageToBencode(SubItem);
      end;
      Result := Result + 'e';
    end;
  end;
end;

function TDataSerializerBencode.StorageToStream(AStorage: IDataStorage;
  AStream: TStream): Boolean;

procedure WriteStr(const AStr: RawByteString);
begin
  if Length(AStr) > 0 then
    AStream.Write(AStr[1], Length(AStr));
end;

var
  sName: RawByteString;
  SubItem: IDataStorage;
  i: integer;
  s: RawByteString;
begin
  case AStorage.GetStorageType() of
    stString:
    begin
      s := AStorage.GetValue();
      WriteStr(IntToStr(Length(s)) + ':' + s);
    end;

    stNumber:
    begin
      s := AStorage.GetValue();
      WriteStr(IntToStr(Length(s)) + ':' + s);
    end;

    stInteger:
    begin
      WriteStr('i' + AStorage.GetValue() + 'e');
    end;

    stDictionary:
    begin
      WriteStr('d');
      for i := 0 to AStorage.GetCount() - 1 do
      begin
        sName := AStorage.GetObjectName(i);
        SubItem := AStorage.GetObject(i);
        // name
        WriteStr(IntToStr(Length(sName)) + ':' + sName);
        // value
        StorageToStream(SubItem, AStream);
      end;
      WriteStr('e');
    end;

    stList:
    begin
      WriteStr('l');
      for i := 0 to AStorage.GetCount() - 1 do
      begin
        SubItem := AStorage.GetObject(i);
        // value
        StorageToStream(SubItem, AStream);
      end;
      WriteStr('e');
    end;
  end;
  Result := True;
end;

function TDataSerializerBencode.ReadBencodeIntegerStr(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
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

function TDataSerializerBencode.ReadBencodeString(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): RawByteString;
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

function TDataSerializerBencode.ReadBencodeDictionary(const AString: RawByteString; var APos: Cardinal; ALen: Cardinal): IDataStorage;
var
  sName: RawByteString;
  SubStorage: IDataStorage;
begin
  Result := nil;
  if AString[APos] = 'd' then
    Inc(APos)
  else
    Exit;

  Result := TDataStorage.Create(stDictionary);
  while APos <= ALen do
  begin
    if AString[APos] = 'e' then
    begin
      Inc(APos);
      Exit;
    end;
    sName := ReadBencodeString(AString, APos, ALen);
    SubStorage := ReadBencodeValue(AString, APos, ALen);
    if Assigned(SubStorage) then
      Result.SetValue(SubStorage, sName);
  end;
  Assert(False, 'End of dict not found');
  Result := nil;
end;

function TDataSerializerBencode.ReadBencodeList(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): IDataStorage;
var
  SubStorage: IDataStorage;
begin
  Result := nil;
  if AString[APos] = 'l' then
    Inc(APos)
  else
    Exit;

  Result := TDataStorage.Create(stList);
  while APos <= ALen do
  begin
    if AString[APos] = 'e' then
    begin
      Inc(APos);
      Exit;
    end;
    SubStorage := ReadBencodeValue(AString, APos, ALen);
    if Assigned(SubStorage) then
      Result.SetValue(SubStorage);
  end;
  Assert(False, 'End of list not found');
  Result := nil;
end;

function TDataSerializerBencode.ReadBencodeValue(const AString: RawByteString;
  var APos: Cardinal; ALen: Cardinal): IDataStorage;
begin
  Result := nil;
  if APos <= ALen then
  begin
    if AString[APos] = 'i' then
    begin
      // read integer value
      Result := TDataStorage.Create(stInteger);
      Result.SetValue(ReadBencodeIntegerStr(AString, APos, ALen));
    end

    else if Pos(AString[APos], '0123456789') > 0 then
    begin
      // read string value
      Result := TDataStorage.Create(stString);
      Result.SetValue(ReadBencodeString(AString, APos, ALen));
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

function TDataSerializerBencode.StorageToString(AStorage: IDataStorage): RawByteString;
begin
  Result := StorageToBencode(AStorage);
end;

function TDataSerializerBencode.StorageFromString(const AString: RawByteString): IDataStorage;
var
  n: Cardinal;
begin
  n := 1;
  Result := ReadBencodeValue(AString, n, Length(AString));
  Assert(Result <> nil, 'Read value failed, len='+IntToStr(Length(AString)));
end;

function TDataSerializerBencode.StorageToFile(AStorage: IDataStorage; AFileName: string): Boolean;
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
    ms.Size := Trunc(AStorage.GetSize() * 1.1);
    StorageToStream(AStorage, ms);
    ms.SaveToFile(AFileName);
  finally
    ms.Free();
  end;
end;

function TDataSerializerBencode.StorageFromFile(AFileName: string): IDataStorage;
begin
  Result := nil;
  if Trim(AFileName) = '' then
    Exit;
  if Pos('.be', AFileName) < (Length(AFileName) - 2) then
    AFileName := AFileName + '.be';
  Result := Self.StorageFromString(FileToStr(AFileName));
end;

initialization

DataFormatSettings.DecimalSeparator := '.';

end.

