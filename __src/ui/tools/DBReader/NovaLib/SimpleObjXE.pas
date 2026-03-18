{ Simple objects for Delphi XE

(C) Sergey Bodrov (serbod@gmail.com)
MIT license
}
unit SimpleObjXE;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef MSWINDOWS}Windows, {$endif}
  Classes, SysUtils;

type
  TSimpleLock = object
  private
    LockCount: Integer;
  public
    procedure Init();
    function Acquire(): Boolean;
    procedure Release();
  end;

  TSimpleRWLock = object
  private
    LockCount: Integer;
  public
    procedure Init();
    function BeginRead(ALockTryCount: Integer = 10000): Boolean;
    function BeginWrite(): Boolean;
    procedure EndRead();
    procedure EndWrite();
  end;

  { Simple thread-safe file writer for log files }
  TSimpleFileWriter = object
  private
    fh: THandle;
    FUnflushedCount: Integer;
    FLock: TSimpleLock;
  public
    FileName: string;
    procedure Init();
    procedure Term();
    { Append data to file. Note! Line ending chars not auto-added! }
    procedure Write(const AStr: string = '');
  end;

  { TSimpleDataQueue }
  { Simple thread-safe data queue }
  PSimpleDataQueue = ^TSimpleDataQueue;
  TSimpleDataQueue = object
  private
    FData: string;
    FLock: TSimpleLock;
  public
    procedure Init();
    procedure Push(const AData: string);
    function Pull(ASize: Integer = MaxInt): string;
    function Peek(ASize: Integer = MaxInt): string;
  end;

  PSimpleHashItem = ^TSimpleHashItem;
  TSimpleHashItem = record
    Next: PSimpleHashItem;
    Key: string;
    Value: Integer;
  end;

  { TStringHash }
  { Simple string-to-integer hashtable }
  TSimpleStringHash = object
  private
    Buckets: array of PSimpleHashItem;
    function Find(const Key: string): PSimpleHashItem;
    function HashOf(const Key: string): Cardinal;
  public
    procedure Init(Size: Cardinal = 256);
    procedure Add(const Key: string; Value: Integer);
    procedure Clear;
    function Modify(const Key: string; Value: Integer): Boolean;
    { return -1 if no value for key, suitable for list index }
    function ValueOf(const Key: string): Integer;
    function FindValue(const Key: string; out Value: Integer): Boolean;
  end;

implementation

const
  WR_LOCK = $10000;

{$ifndef FPC}
const
  fsFromEnd = 2;
{$endif}

{ TSimpleLock }

function TSimpleLock.Acquire: Boolean;
var
  LockTryCount: Integer;
begin
  // try to acquire exclusive lock
  LockTryCount := 10;
  while (AtomicIncrement(LockCount) > 1) and (LockTryCount > 0) do
  begin
    AtomicDecrement(LockCount);
    Dec(LockTryCount);
    if (LockTryCount = 0) then
    begin
      Result := False;
      Exit;
    end;
    Sleep(1);
  end;
  Result := True;
end;

procedure TSimpleLock.Init;
begin
  LockCount := 0;
end;

procedure TSimpleLock.Release;
begin
  // release exclusive lock
  AtomicDecrement(LockCount);
end;

{ TSimpleRWLock }

function TSimpleRWLock.BeginRead(ALockTryCount: Integer): Boolean;
var
  LockTryCount: Integer;
begin
  // try to acquire permissive lock
  LockTryCount := ALockTryCount;
  while (AtomicIncrement(LockCount, $1) >= WR_LOCK) and (LockTryCount > 0) do
  begin
    AtomicDecrement(LockCount, $1);
    Dec(LockTryCount);
    if (LockTryCount = 0) then
    begin
      Result := False;
      Exit;
    end;
    Sleep(1);
  end;
  Result := True;
end;

function TSimpleRWLock.BeginWrite: Boolean;
var
  LockTryCount: Integer;
begin
  // try to acquire exclusive lock
  LockTryCount := 10;
  while (AtomicIncrement(LockCount, WR_LOCK) > 0) and (LockTryCount > 0) do
  begin
    AtomicDecrement(LockCount, WR_LOCK);
    Dec(LockTryCount);
    if (LockTryCount = 0) then
    begin
      Result := False;
      Exit;
    end;
    Sleep(1);
  end;
  Result := True;
end;

procedure TSimpleRWLock.EndRead;
var
  n: Integer;
begin
  // release read lock
  n := AtomicDecrement(LockCount, $1);
  if (n < $0) or (n >= WR_LOCK) then
  begin
    // was not locked or was write-locked
    AtomicIncrement(LockCount, $1);
  end;
end;

procedure TSimpleRWLock.EndWrite;
begin
  // release exclusive write lock
  if AtomicDecrement(LockCount, WR_LOCK) < WR_LOCK then
  begin
    // was not locked
    AtomicIncrement(LockCount, WR_LOCK);
  end;
end;

procedure TSimpleRWLock.Init;
begin
  LockCount := 0;
end;

{ TSimpleFileWriter }
//{$ifndef ANDROID}
procedure TSimpleFileWriter.Init();
begin
  FUnflushedCount := 0;
  fh := 0;
  FLock.Init();
end;

procedure TSimpleFileWriter.Write(const AStr: string);
var
  {$IFDEF FPC}
  Res: THandle;
  {$ELSE}
  Res: Integer;
  {$ENDIF}
begin
  if not FLock.Acquire then Exit;
  if FUnflushedCount = 0 then
  begin
    Res := FileOpen(FileName, (fmOpenReadWrite or fmShareDenyNone));
    {$IFDEF FPC}
    if Res <> feInvalidHandle then
    {$ELSE}
    if Res > 0 then
    {$ENDIF}
    begin
      fh := Res;
      FileSeek(fh, 0, fsFromEnd);
    end
    else
    begin
      // file not exists? try to create new
      // Just created file not shared, despite to access flags
      Res := FileCreate(FileName, 438);
      {$IFDEF FPC}
      if Res <> feInvalidHandle then
      {$ELSE}
      if Res > 0 then
      {$ENDIF}
      begin
        FileClose(Res);
        Res := FileOpen(FileName, (fmOpenReadWrite or fmShareDenyNone));
        {$IFDEF FPC}
        if Res <> feInvalidHandle then
        {$ELSE}
        if Res > 0 then
        {$ENDIF}
          fh := Res;
      end;
    end;
  end;

  if fh <> 0 then
  begin
    if AStr <> '' then
    begin
      //FileWrite(fh, AStr[1], Length(AStr));
      System.Write(fh, AStr);
    end;
    //Flush(f); // forced flush data from buffer to file
    Inc(FUnflushedCount);
    if FUnflushedCount > 10 then
    begin
      // another forced flush
      FUnflushedCount := 0;
      //CloseFile(f);
      FileClose(fh);
      fh := 0;
    end;
  end;
  FLock.Release();
end;

procedure TSimpleFileWriter.Term();
begin
  if FUnflushedCount > 0 then
  begin
    FUnflushedCount := 0;
    FileClose(fh);
    fh := 0;
  end;
end;
//{$endif}

{ TSimpleDataQueue }

procedure TSimpleDataQueue.Init();
begin
  FData := '';
  FLock.Init();
end;

procedure TSimpleDataQueue.Push(const AData: string);
begin
  if FLock.Acquire() then
  begin
    FData := FData + AData;
    FLock.Release();
  end;
end;

function TSimpleDataQueue.Pull(ASize: Integer): string;
begin
  if FLock.Acquire() then
  begin
    Result := Copy(FData, 1, ASize);
    Delete(FData, 1, ASize);
    FLock.Release();
  end;
end;

function TSimpleDataQueue.Peek(ASize: Integer): string;
begin
  if FLock.Acquire() then
  begin
    Result := Copy(FData, 1, ASize);
    FLock.Release();
  end;
end;

{ TSimpleStringHash }

function TSimpleStringHash.Find(const Key: string): PSimpleHashItem;
var
  Hash: Cardinal;
begin
  Hash := (HashOf(Key)) mod Cardinal(Length(Buckets));
  Result := Buckets[Hash];
  while Result <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
    begin
      Result := Result^.Next;
    end;
  end;
end;

function TSimpleStringHash.HashOf(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Key[I]);
end;

procedure TSimpleStringHash.Init(Size: Cardinal);
var
  i: Integer;
begin
  if Length(Buckets) > 0 then
    Clear();
  SetLength(Buckets, Size);
  for i := 0 to Size-1 do
    Buckets[i] := nil;
end;

procedure TSimpleStringHash.Add(const Key: string; Value: Integer);
var
  Hash: Integer;
  Bucket: PSimpleHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TSimpleStringHash.Clear;
var
  I: Integer;
  P, N: PSimpleHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

function TSimpleStringHash.Modify(const Key: string; Value: Integer): Boolean;
var
  P: PSimpleHashItem;
begin
  P := Find(Key);
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

function TSimpleStringHash.ValueOf(const Key: string): Integer;
var
  P: PSimpleHashItem;
begin
  P := Find(Key);
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;

function TSimpleStringHash.FindValue(const Key: string; out Value: Integer): Boolean;
var
  P: PSimpleHashItem;
begin
  P := Find(Key);
  Result := (P <> nil);
  if Result then
    Value := P^.Value;
end;

end.

