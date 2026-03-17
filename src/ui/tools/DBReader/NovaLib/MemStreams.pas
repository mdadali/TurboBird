{
TMemStream
  TMemoryStream replacement with some improvements

  MEM_DEFAULT mode - same as TMemoryStream
  MEM_GLOBAL mode - can allocate memory outside default 2 Gb (for 32 bit)
  MEM_MSVCRT mode - use msvcrt.dll, with latest updates and fixes
  MEM_STRING mode - use AnsiString for memory buffer

  In MEM_GLOBAL mode, Size under MinGlobalMem (8Kb default) alocated in MEM_DEFAULT mode

TMemString
  TMemStream with some String methods (Pos, Copy, Delete..)

TTmpFileStream
  temporary file, that stays in memory, but swapped to disk when no free memory

Sergey Bodrov (serbod@gmail.com) 2016
}
unit MemStreams;

// memory allocation mode
{$ifdef ANDROID}
  {$define MEM_DEFAULT}   // default  GetMem/ReallocMem/FreeMem
{$else}
  {$ifdef CPU64}
    {$define MEM_DEFAULT}  // default  GetMem/ReallocMem/FreeMem
  {$else}
    {// $define MEM_DEFAULT}  // default  GetMem/ReallocMem/FreeMem
    {$define MEM_GLOBAL}  // GlobalAlloc  GlobalAllocPtr/GlobalReallocPtr/GlobalFreePtr
  {$endif}
  {// $define MEM_MSVCRT}  // msvcrt.dll  win_malloc/win_realloc/win_free
  {// $define MEM_STRING}  // AnsiString as memory buffer
{$endif}

interface

uses SysUtils,
{$ifdef MSWINDOWS}Windows,{$endif}
{$ifdef ANDROID}System.IOUtils, System.Generics.Collections,{$endif}
Classes;

type
  TMemStream = class(TStream)
  protected
    FMemory: Pointer;
    //FIsGlobal: Boolean;
    FSize, FPosition: Longint;
    FPrevCapacity: Longint;
    FCapacity: Longint;
    procedure SetPointer(APtr: Pointer; ASize: Longint);
    procedure SetCapacity(NewCapacity: Longint);
    function ReallocDefault(AMemory: Pointer; OldCapacity, NewCapacity: Longint): Pointer;
    {$ifdef MEM_GLOBAL}
    function ReallocGlobal(AMemory: Pointer; OldCapacity, NewCapacity: Longint): Pointer;
    {$endif}
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
  public
    constructor Create();
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(NewSize: Longint); override;
    property Memory: Pointer read FMemory;
    property Capacity: Longint read FCapacity write SetCapacity;
  end;

  TMemString = class(TMemStream)
  protected
    FLength: Integer;
    function GetChar(Index: Integer): Char;
    procedure SetChar(Index: Integer; Value: Char);
  public
    constructor Create(MaxLength: Integer); reintroduce;
    destructor Destroy; override;
    procedure Assign(const Value: string);
    procedure SetLength(NewLength: Integer);
    // delete part of buffer, shift remains to freed space
    // same as Delete(string)
    procedure Delete(Index, Count: Integer);
    // find substring position
    function Pos(const Substr: string): Integer;
    // returns substring
    function Copy(Index, Count: Integer): string;
    function CopyEx(Index, Count: Integer; var AStr: string): Boolean;
    property Chars[Index: Integer]: Char read GetChar write SetChar;
  end;

  TTmpFileStream = class(THandleStream)
  private
    FTmpFileName: string;
  public
    constructor Create();
    destructor Destroy; override;
  end;

  TRFObjCountItem = record
    Name: string;
    Count: Integer;
  end;
  PRFObjCountItem = ^TRFObjCountItem;

  TRFObjCount = class(TObject)
  private
    FItemList: TList{$ifdef ANDROID}<PRFObjCountItem>{$endif};
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Inc(const AClassName: string);
    procedure Dec(const AClassName: string);
    procedure FillStrings(AStrings: TStrings);
    function GetCount(): Integer;
    function GetItem(AIndex: Integer): TRFObjCountItem;
  end;

var
  GlobalMemStreamCount: Integer;
  GlobalMemStreamSize: Integer;
  RFObjCount: TRFObjCount;

implementation

type
  size_t = Cardinal;

const
  MemoryDelta = $400; { Must be a power of 2 }
  MaxMemGrowDelta = $100000; // 1 ฬแ
  MinGlobalMem = $2000; { 8สม }

{$ifdef MEM_MSVCRT}
const msvcrtDLL = 'msvcrt.dll';
function win_malloc(Size: size_t): Pointer; cdecl; external msvcrtDLL name 'malloc';
function win_realloc(P: Pointer; Size: size_t): Pointer; cdecl; external msvcrtDLL name 'realloc';
procedure win_free(P: Pointer); cdecl; external msvcrtDLL name 'free';
{$endif}

constructor TMemStream.Create;
begin
  {$ifdef ANDROID}
  AtomicIncrement(GlobalMemStreamCount);
  {$else}
  InterlockedIncrement(GlobalMemStreamCount);
  {$endif}
  inherited;
  //FString := '';
  FCapacity := 0;
  FPrevCapacity := 0;
  FMemory := nil;
  Clear();
end;

destructor TMemStream.Destroy;
begin
  Clear;
  inherited Destroy;
  {$ifdef ANDROID}
  AtomicDecrement(GlobalMemStreamCount);
  {$else}
  InterlockedDecrement(GlobalMemStreamCount);
  {$endif}
end;

procedure TMemStream.SetPointer(APtr: Pointer; ASize: Longint);
begin
  FMemory := APtr;
  FSize := ASize;
end;

function TMemStream.Read(var Buffer; Count: Longint): Longint;
var
  p: PByte;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      p := FMemory;
      Inc(p, FPosition);
      Move(p^, Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TMemStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TMemStream.SaveToStream(Stream: TStream);
begin
  if FSize <> 0 then Stream.WriteBuffer(FMemory^, FSize);
end;

procedure TMemStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMemStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
  FMemory := nil;
end;

procedure TMemStream.LoadFromStream(Stream: TStream);
var
  Count: Longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then Stream.ReadBuffer(FMemory^, Count);
end;

procedure TMemStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMemStream.SetCapacity(NewCapacity: Longint);
var
  GrowDelta, GrowDelta2: Longint;
begin
  if (FCapacity > FPrevCapacity) and (NewCapacity > FCapacity) then
  begin
    // growing, calculate optimal capacity ahead
    GrowDelta := Abs(FCapacity-FPrevCapacity);
    GrowDelta2 := Abs(NewCapacity-FCapacity);
    if GrowDelta > GrowDelta2 then
      GrowDelta := GrowDelta2;
    if GrowDelta > MaxMemGrowDelta then
      GrowDelta := MaxMemGrowDelta;
    NewCapacity := NewCapacity + GrowDelta;
  end;
  // align new capacity size up to 1 Kb
  if (NewCapacity > 0) and (NewCapacity <> FSize) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  if NewCapacity < 0 then
    NewCapacity := 0;

  {$ifdef ANDROID}
  AtomicDecrement(GlobalMemStreamSize, FCapacity);
  {$else}
  InterlockedExchangeAdd(GlobalMemStreamSize, -FCapacity);
  {$endif}
  SetPointer(Realloc(NewCapacity), FSize);
  FPrevCapacity := FCapacity;
  FCapacity := NewCapacity;
  {$ifdef ANDROID}
  AtomicIncrement(GlobalMemStreamSize, NewCapacity);
  {$else}
  InterlockedExchangeAdd(GlobalMemStreamSize, NewCapacity);
  {$endif}
end;

procedure TMemStream.SetSize(NewSize: Longint);
var
  OldPosition: Longint;
begin
  if NewSize < 0 then
    raise EStreamError.Create('SetSize(' + IntToStr(NewSize) + ')');
  if FSize <> NewSize then
  begin
    OldPosition := FPosition;
    SetCapacity(NewSize);
    FSize := NewSize;
    if OldPosition > NewSize then Seek(0, soFromEnd);
  end;
end;

function TMemStream.ReallocDefault(AMemory: Pointer; OldCapacity, NewCapacity: Integer): Pointer;
begin
  Result := AMemory;
  if NewCapacity <> OldCapacity then
  begin
    if (NewCapacity = 0) then
    begin
      FreeMem(AMemory);
      Result := nil;
    end
    else
    begin
      if (OldCapacity = 0) or (AMemory = nil) then
      begin
        GetMem(Result, NewCapacity);
        if Result = nil then
          raise EStreamError.Create('GetMem(' + IntToStr(NewCapacity) + ')=nil');
      end
      else
      begin
        ReallocMem(Result, NewCapacity);
        if Result = nil then
          raise EStreamError.Create('ReallocMem(' + IntToStr(NewCapacity) + ')=nil');
      end;
    end;
  end;
end;

{$ifdef MEM_DEFAULT}
function TMemStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  Result := ReallocDefault(Memory, FCapacity, NewCapacity);
end;
{$endif}

{$ifdef MEM_GLOBAL}
function TMemStream.ReallocGlobal(AMemory: Pointer; OldCapacity, NewCapacity: Longint): Pointer;
begin
  Result := AMemory;
  if NewCapacity <> OldCapacity then
  begin
    if NewCapacity = 0 then
    begin
      GlobalFreePtr(AMemory);
      Result := nil;
    end
    else
    begin
      // HeapAllocFlags = GMEM_MOVEABLE
      // GMEM_MOVEABLE - working, but useless for GlobalAllocPtr / GlobalReallocPtr
      // GMEM_FIXED - handle = pointer, but not working sometimes
      if (OldCapacity = 0) or (AMemory = nil) then
      begin
        Result := GlobalAllocPtr(GMEM_MOVEABLE, NewCapacity);
        if Result = nil then
          raise EStreamError.Create('GlobalAllocPtr(' + IntToStr(NewCapacity) + ')=nil');
      end
      else
      begin
        Result := GlobalReallocPtr(AMemory, NewCapacity, GMEM_MOVEABLE);
        if Result = nil then
          raise EStreamError.Create('GlobalReallocPtr(' + IntToStr(NewCapacity) + ')=nil');
      end;
    end;
  end;

end;

function TMemStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if (NewCapacity >= MinGlobalMem) then
  begin
    // use global alloc
    if (FCapacity >= MinGlobalMem) or (FCapacity = 0) then
      Result := ReallocGlobal(Memory, FCapacity, NewCapacity)
    else
    begin
      Result := ReallocGlobal(nil, 0, NewCapacity);
      if (NewCapacity > FCapacity) and (FCapacity > 0) then
        Move(Memory^, Result^, FCapacity)
      else if NewCapacity > 0 then
        Move(Memory^, Result^, NewCapacity);
      ReallocDefault(Memory, FCapacity, 0);
    end;
  end
  else
  begin
    // use default alloc
    if (FCapacity < MinGlobalMem) then
      Result := ReallocDefault(Memory, FCapacity, NewCapacity)
    else
    begin
      Result := ReallocDefault(nil, 0, NewCapacity);
      if (NewCapacity > FCapacity) and (FCapacity > 0) then
        Move(Memory^, Result^, FCapacity)
      else
        Move(Memory^, Result^, NewCapacity);
      ReallocGlobal(Memory, FCapacity, 0);
    end;

  end;
end;
{$endif}

{$ifdef MEM_MSVCRT}
function TMemStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  Result := Memory;
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
    begin
      win_free(Memory);
      Result := nil;
    end else
    begin
      if Capacity = 0 then
      begin
        Result := win_malloc(NewCapacity);
        if Result = nil then
          raise EStreamError.Create('win_malloc(' + IntToStr(NewCapacity) + ')=nil');
      end
      else
      begin
        Result := win_realloc(Result, NewCapacity);
        if Result = nil then
          raise EStreamError.Create('win_realloc(' + IntToStr(NewCapacity) + ')=nil');
      end;
    end;
  end;
end;
{$endif}

{$ifdef MEM_STRING}
function TMemStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if NewCapacity <> FCapacity then
  begin
    SetLength(FString, NewCapacity);
  end;
  if Length(FString) > 0 then
    Result := PChar(FString)
  else
    Result := nil;
end;
{$endif}

function TMemStream.Write(const Buffer; Count: Longint): Longint;
var
  iPos: Longint;
  p: PByte;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    iPos := FPosition + Count;
    if iPos > 0 then
    begin
      if iPos > FSize then
      begin
        if iPos > FCapacity then
          SetCapacity(iPos);
        FSize := iPos;
      end;
      p := FMemory;
      Inc(p, FPosition);
      System.Move(Buffer, p^, Count);
      FPosition := iPos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;


{ TTmpFileStream }

constructor TTmpFileStream.Create;
var
  TmpGUID: TGuid;
  FileName: string;
  tempFolder: array[0..MAX_PATH] of Char;
begin
  {$ifndef ANDROID}
  GetTempPath(MAX_PATH, @tempFolder);
  CreateGUID(TmpGuid);
  FileName := StrPas(tempFolder) + GUIDToString(TmpGuid);

  inherited Create(
    CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
    FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0)
  );
  {$else}
  // Android
  FileName := TPath.GetTempFileName();
  inherited Create(FileCreate(FileName));
  {$endif}

  if THandle(Handle) = INVALID_HANDLE_VALUE then
    raise EFCreateError.CreateFmt('Cannot create file "%s". %s', [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);

  FTmpFileName := FileName;
end;

destructor TTmpFileStream.Destroy;
begin
  if THandle(Handle) <> INVALID_HANDLE_VALUE then
    FileClose(Handle);

  inherited Destroy;
end;

{ TRFObjCount }

constructor TRFObjCount.Create;
begin
  FItemList := TList{$ifdef ANDROID}<PRFObjCountItem>{$endif}.Create();
  //FHashByName := TStringHash.Create();
end;

destructor TRFObjCount.Destroy;
var
  i: Integer;
  PItem: PRFObjCountItem;
begin
  //FreeAndNil(FHashByName);
  for i := FItemList.Count-1 downto 0 do
  begin
    PItem := FItemList[i];
    Dispose(PItem);
  end;
  FreeAndNil(FItemList);
  inherited;
end;

procedure TRFObjCount.Dec(const AClassName: string);
var
  i: Integer;
  PItem: PRFObjCountItem;
begin
  for i := 0 to FItemList.Count-1 do
  begin
    PItem := FItemList[i];
    if PItem^.Name = AClassName then
    begin
      {$ifdef ANDROID}
      AtomicDecrement(PItem^.Count);
      {$else}
      InterlockedDecrement(PItem^.Count);
      {$endif}
      Exit;
    end;
  end;

  New(PItem);
  PItem^.Name := AClassName;
  PItem^.Count := 1;
  FItemList.Add(PItem);
end;

procedure TRFObjCount.Inc(const AClassName: string);
var
  i: Integer;
  PItem: PRFObjCountItem;
begin
  {n := FHashByName.ValueOf(AClassName);
  if (n <> -1) and (n < Count) then
  begin
    Result := Items[n];
    if Result.Id = Id then
      Exit;
  end; }

  //FHashById.Add(AClassName, Result);

  for i := 0 to FItemList.Count-1 do
  begin
    PItem := FItemList[i];
    if PItem^.Name = AClassName then
    begin
      {$ifdef ANDROID}
      AtomicIncrement(PItem^.Count);
      {$else}
      InterlockedIncrement(PItem^.Count);
      {$endif}
      if i > 0 then
        FItemList.Exchange(i, 0);
      Exit;
    end;
  end;

  New(PItem);
  PItem^.Name := AClassName;
  PItem^.Count := 1;
  FItemList.Add(PItem);
end;

procedure TRFObjCount.FillStrings(AStrings: TStrings);
var
  i: Integer;
  PItem: PRFObjCountItem;
begin
  if not Assigned(AStrings) then Exit;
  AStrings.BeginUpdate();
  AStrings.Clear();
  for i := 0 to FItemList.Count-1 do
  begin
    PItem := FItemList[i];
    AStrings.Append(PItem^.Name + '=' + IntToStr(PItem^.Count));
  end;
  AStrings.EndUpdate();
end;

function TRFObjCount.GetCount(): Integer;
begin
  Result := FItemList.Count;
end;

function TRFObjCount.GetItem(AIndex: Integer): TRFObjCountItem;
var
  PItem: PRFObjCountItem;
begin
  PItem := FItemList[AIndex];
  Result := PItem^;
end;

{ TMemString }

constructor TMemString.Create(MaxLength: Integer);
begin
  inherited Create();
  FLength := 0;
  SetSize(MaxLength);
end;

destructor TMemString.Destroy;
begin
  inherited;
end;

procedure TMemString.Assign(const Value: string);
begin
  FLength := System.Length(Value);
  if FLength > Size then
    FLength := Size;
  if FLength > 0 then
    System.Move(Value[1], FMemory^, FLength);
end;

function TMemString.Copy(Index, Count: Integer): string;
var
  p: PByte;
begin
  if Index < 0 then
    Index := 0;
  if Index > FLength then
    Index := FLength;
  if Count > FLength-Index then
    Count := FLength-Index;
  if Count < 0 then
    Count := 0;

  System.SetLength(Result, Count);
  if Count > 0 then
  begin
    p := FMemory;
    Inc(p, Index);
    System.Move(p^, Result[1], Count);
  end;
end;

function TMemString.CopyEx(Index, Count: Integer;
  var AStr: string): Boolean;
var
  p: PByte;
begin
  if Index < 0 then
    Index := 0;
  if Index > FLength then
    Index := FLength;
  if Count > FLength-Index then
    Count := FLength-Index;
  if Count < 0 then
    Count := 0;

  System.SetLength(AStr, Count);
  if Count > 0 then
  begin
    p := FMemory;
    Inc(p, Index);
    System.Move(p^, AStr[1], Count);
    Result := True;
  end
  else
    Result := False;
end;

function TMemString.Pos(const Substr: string): Integer;
var
  n, len: Integer;
  c: Char;
begin
  Result := 1;
  len := System.Length(Substr);
  n := 1;
  while n < len do
  begin
    c := Substr[n];
    while (Result < FLength) do
    begin
      if (Chars[Result] <> c) then
      begin
        Inc(Result);
        if n > 1 then
        begin
          n := 1;
          Break;
        end;
      end
      else
      begin
        if n = len then
        begin
          Exit;
        end;
        Inc(n);
        Break;
      end;
    end;
  end;
  Result := 0;
end;

procedure TMemString.SetLength(NewLength: Integer);
begin
  //if NewLength > Size then
  //  Size := NewLength;
  if NewLength > Size then
    NewLength := Size;
  if NewLength < 0 then
    NewLength := 0;
  FLength := NewLength;
end;

procedure TMemString.Delete(Index, Count: Integer);
var
  ps, pd: PByte;
begin
  if Index+Count > FLength then
    Count := FLength - Index;
  if Count < 0 then
    Count := 0;
  if Count > 0 then
  begin
    ps := FMemory;
    Inc(ps, Index + Count);
    pd := FMemory;
    Inc(pd, Index);
    System.Move(ps^, pd^, Count);
    FLength := FLength - Count;
  end;
end;

function TMemString.GetChar(Index: Integer): Char;
var
  p: PByte;
begin
  if (Index < 0) or (Index > FLength) then
    Exit;

  p := FMemory;
  Inc(p, Index);
  System.Move(p^, Result, SizeOf(Char));
end;

procedure TMemString.SetChar(Index: Integer; Value: Char);
var
  p: PByte;
begin
  if (Index < 0) or (Index > FLength) then
    Exit;

  p := FMemory;
  Inc(p, Index);
  System.Move(Value, p^, SizeOf(Char));
end;

initialization

GlobalMemStreamCount := 0;
GlobalMemStreamSize := 0;
RFObjCount := TRFObjCount.Create();

finalization

FreeAndNil(RFObjCount);

end.
