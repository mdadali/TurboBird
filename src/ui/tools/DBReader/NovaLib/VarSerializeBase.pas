unit VarSerializeBase;
{ Author: Sergey Bodrov (serbod@gmail.com) 2020 }

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Types, Variants, VarLists, VarDicts;

type
  {$ifndef UNICODE}
  RawByteString = AnsiString;
  {$endif}

  { TVariantSerializer }

  TVariantSerializer = class(TObject)
  public
    function GetName(): string; virtual;
    // Get total bytes count of AValue string representation
    function GetSize(const AValue: Variant): Int64; virtual;
    // Serialize Variant to string
    function VariantToString(const AValue: Variant): RawByteString; virtual; abstract;
    // De-serialize string into AStorage (not nil)
    function VariantFromString(const AString: RawByteString): Variant; virtual; abstract;
    // Save storage to file. Filename must be without extension
    function VariantToFile(const AValue: Variant; AFileName: string): Boolean; virtual; abstract;
    // Fill AStorage (not nil) from file. Filename must be without extension
    function VariantFromFile(AFileName: string): Variant; virtual; abstract;
  end;

// shared functions
function StreamToStr(AStream: TStream): RawByteString;
function StrToStream(const s: RawByteString; AStream: TStream): Boolean;

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

function RawToBytes(const AValue: RawByteString): TByteDynArray;
var
  n: integer;
begin
  n := Length(AValue);
  SetLength(Result, n);
  if n > 0 then
    Move(AValue[1], Result[0], n);
end;

function BytesToRaw(const AValue: TByteDynArray): RawByteString;
var
  n: integer;
begin
  n := Length(AValue);
  SetLength(Result, n);
  if n > 0 then
    Move(AValue[0], Result[1], n);
end;

{ TVariantSerializer }

function TVariantSerializer.GetName: string;
begin
  Result := 'NONE';
end;

function TVariantSerializer.GetSize(const AValue: Variant): Int64;
var
  i: Integer;
begin
  Result := 0;
  if VarIsType(AValue, VarTypeDict) then
  begin
    for i := 0 to AValue.GetCount() - 1 do
    begin
      Result := Result + Length(AValue.GetName(i));
      Result := Result + GetSize(AValue.GetValue(i));
    end;
  end
  else
  if VarIsType(AValue, VarTypeList) then
  begin
    for i := 0 to AValue.GetCount() do
    begin
      Result := Result + GetSize(AValue.GetValue(i));
    end;
  end
  else
  begin
    // string
    Result := Result + Length(VarToStr(AValue));
  end;
end;

end.