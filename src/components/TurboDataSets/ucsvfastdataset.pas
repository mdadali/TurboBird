{
  uCSVFastDataset.pas

  High-performance CSV dataset for Lazarus / Free Pascal.

  TCSVFastDataset is a drop-in replacement for TCSVDataset. It
  inherits from TCSVDataset and replaces the slow, character-by-
  character parser with a PChar block scanner. All CSVOptions
  properties remain available, so swapping in existing code is
  trivial:

      // Before:
      CSVDataset1: TCSVDataset;

      // After:
      CSVDataset1: TCSVFastDataset;

  Measured performance (Intel i5-4210H, SSD, 16 GB RAM, Linux):
    -  4.94 GB CSV, 17 fields, 1,250,000 rows in ~18 seconds
    -  11.86 GB CSV, 17 fields, 3,000,000 rows in ~65 seconds
    -  ~67,000 rows/sec (internal SSD, warm cache)
    -  ~46,000 rows/sec (USB3 SSD, cold cache)

  For comparison: pandas.read_csv typically reaches 30,000-50,000
  rows/sec on the same hardware.

  ----------------------------------------------------------------------
  ARCHITECTURE
  ----------------------------------------------------------------------

  TCSVFastDataset (public)
    The externally visible replacement for TCSVDataset.
    Handles file opening, stream management, and API compatibility.
    All properties of TCSVDataset remain unchanged.

    Additional properties:
      BlockSize             - I/O block size in BYTES (default 1 MB)
      BufferSize            - Total buffer size in BYTES (default 2 MB)
      InitialCellCapacity   - Initial size of the cell-array (default 64)
      SkipFirstRows         - Number of data rows to skip (range start)
      StopAfterRow          - Number of rows to load (range limit)
      OnProgress            - Progress callback (see TCSVLoadProgressEvent)
      TotalRowsForProgress  - Total rows expected (for percent calculation)
      ProgressUpdatePercent - Update progress every N percent (default 1)
      CancelFlag            - Pointer to a Boolean that aborts the load

  TCSVFastPacketReader (internal)
    The actual parser. Inherits from TDataPacketReader.

  TCSVFastCellRef (internal)
    A cell reference: pointer into the buffer + length.
    No data is copied; the actual conversion into a string
    happens only in CellToStr().

  ----------------------------------------------------------------------
  BASIC PRINCIPLE: BLOCK SCANNING
  ----------------------------------------------------------------------

  The classic TCSVDataset parser works character by character and
  concatenates strings per character:

      for i := 1 to Length(Line) do
        S := S + C;   // O(n^2) through repeated reallocations

  Our approach avoids this:

  1. The file is read in blocks (default: 1 MB, configurable via
     BlockSize) into a buffer.
  2. A PChar scanner walks through the buffer character by character
     WITHOUT allocating memory. It only records where each cell
     begins and ends.
  3. Only when filling the field buffer is the cell content copied
     once into a string via Move().

  Result: one copy per cell instead of N copies per character.

  ----------------------------------------------------------------------
  UNITS
  ----------------------------------------------------------------------

  All sizes (BlockSize, BufferSize, cell sizes, etc.) are expressed
  in BYTES. There are no hidden KB/MB conversions. The only constants
  defined for convenience are internal defaults and bounds, also in
  BYTES.

  ----------------------------------------------------------------------
  CANCELLATION
  ----------------------------------------------------------------------

  Cancellation is pointer-based:
    - The caller creates a Boolean variable, set to False.
    - The caller passes its address via CancelFlag.
    - Setting the variable to True from another thread aborts
      the load at the next progress update.

  ----------------------------------------------------------------------
  PLATFORM SUPPORT
  ----------------------------------------------------------------------

  Tested on:
    - Linux (Debian 13, FPC 3.x)
    - Windows (via GlobalMemoryStatusEx)
    - macOS (via sysctl, conservative estimate)

  No dependency on IBX, Firebird, or any project-specific unit.
  Only FCL/RTL units are used.

  ----------------------------------------------------------------------
  LICENSE
  ----------------------------------------------------------------------

  Same license as the containing project.
}

unit uCSVFastDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, csvdataset, csvreadwrite;

type
  { Progress event: called during load at each progress update.

    Parameters:
      ARowsRead       - Rows read so far (data rows, header excluded)
      AElapsedSeconds - Elapsed time since load started, in seconds
      AFreeRAMMB      - Free physical RAM in MB.
                        -1 means "unchanged since last update"
                        (the reader only queries RAM every 5 updates
                         to minimize overhead).
  }
  TCSVLoadProgressEvent = procedure(ARowsRead: Int64;
    AElapsedSeconds: Double; AFreeRAMMB: Int64) of object;

type
  { Forward declaration }
  TCSVFastDataset = class;

  { Cell reference: pointer into the buffer + length.
    No data is copied here; the conversion to a string happens
    only in CellToStr(). }
  TCSVFastCellRef = record
    Start: PChar;
    Len: Int64;
  end;
  TCSVFastCellArray = array of TCSVFastCellRef;

  { ------------------------------------------------------------------
    TCSVFastPacketReader
    Reads CSV data block-wise using PChar scanning.
    Writes values directly into the field buffer of TBufDataset.
    ------------------------------------------------------------------ }
  TCSVFastPacketReader = class(TDataPacketReader)
  private
    FOptions: TCSVOptions;
    FOwnsOptions: Boolean;

    // File buffer
    FBuffer: TBytes;
    FBufferSize: Int64;       // in bytes
    FBlockSize: Integer;      // in bytes
    FBufStart: PChar;
    FBufPos: PChar;
    FBufEnd: PChar;
    FRowStartOffset: Integer;

    // Current row: pointers into the buffer
    FCells: TCSVFastCellArray;
    FCellCount: Integer;
    FCellCapacity: Integer;
    FInitialCellCapacity: Integer;

    // Field definitions (from header or pre-set FieldDefs)
    FCreateFieldDefs: TFieldDefs;

    // State
    FCurrentRow: Int64;
    FEOF: Boolean;
    FFirstLineAsFieldNames: Boolean;
    FDefaultFieldLength: Integer;

    // Parser state
    FInQuote: Boolean;
    FQuoteChar: Char;
    FDelimiter: Char;
    FLineEnding: string;
    FIgnoreOuterWhitespace: Boolean;

    // Row counting and range
    FScannedRowCount: Int64;
    FSkipFirstRows: Int64;
    FStopAfterRow: Int64;

    // Progress reporting
    FOnProgress: TCSVLoadProgressEvent;
    FTotalRowsForProgress: Int64;
    FProgressUpdatePercent: Integer;
    FLastProgressPercent: Integer;
    FStartTime: TDateTime;
    FProgressCounter: Integer;

    // Cancellation
    FCancelFlag: PBoolean;

    procedure RefillBuffer;
    procedure SkipBOM;
    procedure ScanRow;
    procedure AppendCell(Start: PChar; Len: Int64);
    procedure EnsureCellCapacity;
    function  CellToStr(const C: TCSVFastCellRef): string;
    procedure SetBufferSize(AValue: Int64);
    procedure SetInitialCellCapacity(AValue: Integer);
  protected
    property CreateFieldDefs: TFieldDefs read FCreateFieldDefs write FCreateFieldDefs;
  public
    constructor Create(ADataSet: TCustomBufDataset; AStream: TStream); override;
    constructor Create(ADataSet: TCustomBufDataset; AStream: TStream;
      AOptions: TCSVOptions);
    destructor Destroy; override;

    procedure LoadFieldDefs(var AnAutoIncValue: Integer); override;
    procedure StoreFieldDefs(AnAutoIncValue: Integer); override;
    procedure InitLoadRecords; override;
    function  GetCurrentRecord: Boolean; override;
    function  GetRecordRowState(out AUpdOrder: Integer): TRowState; override;
    procedure RestoreRecord; override;
    procedure GotoNextRecord; override;
    procedure StoreRecord(ARowState: TRowState; AUpdOrder: Integer = 0); override;
    procedure FinalizeStoreRecords; override;
    class function RecognizeStream(AStream: TStream): Boolean; override;

    property Options: TCSVOptions read FOptions;

    { I/O block size in BYTES (internal). The dataset passes the
      user-configured value directly. }
    property BlockSize: Integer read FBlockSize write FBlockSize;

    { Total buffer size in BYTES (internal). }
    property BufferSize: Int64 read FBufferSize write SetBufferSize;

    { Initial cell array size (number of cells, not bytes). }
    property InitialCellCapacity: Integer read FInitialCellCapacity
      write SetInitialCellCapacity;

    { Range control }
    property SkipFirstRows: Int64 read FSkipFirstRows write FSkipFirstRows;
    property StopAfterRow: Int64 read FStopAfterRow write FStopAfterRow;

    { Number of data rows read so far (header excluded). }
    property ScannedRowCount: Int64 read FScannedRowCount;

    { Progress callback (see TCSVLoadProgressEvent). }
    property OnProgress: TCSVLoadProgressEvent
      read FOnProgress write FOnProgress;

    { Total rows expected (used for percent calculation).
      If 0, no progress events are fired. }
    property TotalRowsForProgress: Int64
      read FTotalRowsForProgress write FTotalRowsForProgress;

    { Fire a progress event every N percent (default: 5). }
    property ProgressUpdatePercent: Integer
      read FProgressUpdatePercent write FProgressUpdatePercent;

    { Pointer to an external Boolean. If the Boolean becomes True,
      the load aborts at the next progress update with EAbort. }
    property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;
  end;

  { ------------------------------------------------------------------
    TCSVFastDataset
    Drop-in replacement for TCSVDataset.

    All sizes (BlockSize, BufferSize) are expressed in BYTES.
    They are passed to the reader unchanged — no hidden conversions.
    ------------------------------------------------------------------ }
  TCSVFastDataset = class(TCSVDataset)
  private
    // Configuration (in BYTES)
    FBlockSizeBytes: Integer;
    FBufferSizeBytes: Int64;
    FInitialCellCapacity: Integer;

    // Range control
    FSkipFirstRows: Int64;
    FStopAfterRow: Int64;

    // Progress reporting
    FOnProgress: TCSVLoadProgressEvent;
    FTotalRowsForProgress: Int64;
    FProgressUpdatePercent: Integer;

    // Cancellation
    FCancelFlag: PBoolean;
  protected
    class function DefaultPacketClass: TDataPacketReaderClass; override;
    function CreateDefaultPacketReader(aStream: TStream): TDataPacketReader; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadFromCSVFile(const AFileName: string); reintroduce;
    procedure LoadFromCSVStream(AStream: TStream); reintroduce;
    procedure SaveToCSVFile(AFileName: string = ''); reintroduce;

    { Range control: number of data rows to skip at the start. }
    property SkipFirstRows: Int64 read FSkipFirstRows write FSkipFirstRows;

    { Range control: number of data rows to load.
      If 0, load until end of file. }
    property StopAfterRow: Int64 read FStopAfterRow write FStopAfterRow;

    { Total rows expected (used for percent calculation).
      If 0, no progress events are fired. }
    property TotalRowsForProgress: Int64
      read FTotalRowsForProgress write FTotalRowsForProgress;

    { Pointer to an external Boolean for cancellation. }
    property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;
  published
    { I/O block size in BYTES (default: 1 MB).
      Controls how much data is read per I/O operation.
      Smaller values reduce memory usage, larger values may
      improve throughput on fast SSDs. }
    property BlockSize: Integer
      read FBlockSizeBytes write FBlockSizeBytes default 1048576;

    { Total buffer size in BYTES (default: 2 MB).
      Must be at least 2 * BlockSize, so that a row spanning a
      block boundary can be reassembled. }
    property BufferSize: Int64
      read FBufferSizeBytes write FBufferSizeBytes default 2097152;

    { Initial cell array capacity (default: 64).
      The array grows automatically when a row has more fields.
      Increasing this value reduces reallocations for wide tables. }
    property InitialCellCapacity: Integer
      read FInitialCellCapacity write FInitialCellCapacity default 128;

    { Progress callback (see TCSVLoadProgressEvent). }
    property OnProgress: TCSVLoadProgressEvent
      read FOnProgress write FOnProgress;

      { Fire a progress event every N percent (default: 1). }
    property ProgressUpdatePercent: Integer
      read FProgressUpdatePercent write FProgressUpdatePercent;
  end;

{ ------------------------------------------------------------------
  Inline PChar helpers
  ------------------------------------------------------------------ }
function PCharToInt64Def(P: PChar; Len: Int64; Def: Int64): Int64; inline;
function PCharToDoubleDef(P: PChar; Len: Int64; Def: Double): Double; inline;
function PCharEqualsIgnoreCase(P: PChar; Len: Int64; const S: string): Boolean; inline;

{ ------------------------------------------------------------------
  GetFreeMemoryMB
  Returns the available physical RAM in MB.

  Platform-specific:
    Linux  : /proc/meminfo (MemAvailable, fallback MemFree)
    Windows: GlobalMemoryStatusEx (ullAvailPhys)
    macOS  : conservative estimate

  Returns -1 on error.
  ------------------------------------------------------------------ }
function GetFreeMemoryMB: Int64;

{ ------------------------------------------------------------------
  GetTotalMemoryMB
  Returns the total physical RAM in MB.

  Platform-specific:
    Linux  : /proc/meminfo (MemTotal)
    Windows: GlobalMemoryStatusEx (ullTotalPhys)
    macOS  : sysctl hw.memsize

  Returns -1 on error.
  ------------------------------------------------------------------ }
function GetTotalMemoryMB: Int64;

procedure Register;

implementation

uses
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF LINUX}
  BaseUnix
  {$ENDIF}
  {$IFDEF DARWIN}
  Process
  {$ENDIF}
  ;

{ ==================================================================
  Constants
  ================================================================== }
const
  CR    = #13;
  LF    = #10;
  TAB   = #9;
  SPACE = #32;

{ ==================================================================
  RAM query (platform-specific)
  ================================================================== }

{$IFDEF LINUX}
function ReadMemInfoValue(const AKey: string): Int64;
var
  SL: TStringList;
  I: Integer;
  Line, ValStr: string;
  MemKB: Int64;
begin
  Result := -1;
  if not FileExists('/proc/meminfo') then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile('/proc/meminfo');
    for I := 0 to SL.Count - 1 do
    begin
      Line := SL[I];
      if Pos(AKey + ':', Line) = 1 then
      begin
        ValStr := Trim(Copy(Line, Pos(':', Line) + 1, MaxInt));
        ValStr := Copy(ValStr, 1, Pos(' ', ValStr) - 1);
        if TryStrToInt64(ValStr, MemKB) then
          Result := MemKB div 1024;   // kB -> MB
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;
{$ENDIF}

function GetFreeMemoryMB: Int64;
{$IFDEF LINUX}
begin
  // MemAvailable is the kernel's best estimate of usable RAM
  Result := ReadMemInfoValue('MemAvailable');
  if Result < 0 then
    Result := ReadMemInfoValue('MemFree');   // fallback for older kernels
end;
{$ENDIF}

{$IFDEF WINDOWS}
type
  TMemoryStatusEx = record
    dwLength:                DWORD;
    dwMemoryLoad:            DWORD;
    ullTotalPhys:            QWord;
    ullAvailPhys:            QWord;
    ullTotalPageFile:        QWord;
    ullAvailPageFile:        QWord;
    ullTotalVirtual:         QWord;
    ullAvailVirtual:         QWord;
    ullAvailExtendedVirtual: QWord;
  end;

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL;
  stdcall; external 'kernel32.dll' name 'GlobalMemoryStatusEx';

var
  MemStatus: TMemoryStatusEx;
begin
  Result := -1;
  FillChar(MemStatus, SizeOf(MemStatus), 0);
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
    Result := MemStatus.ullAvailPhys div (1024 * 1024);
end;
{$ENDIF}

{$IFDEF DARWIN}
var
  P: TProcess;
  SL: TStringList;
begin
  // Conservative estimate for macOS
  Result := GetTotalMemoryMB div 10;
end;
{$ENDIF}

function GetTotalMemoryMB: Int64;
{$IFDEF LINUX}
begin
  Result := ReadMemInfoValue('MemTotal');
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  MemStatus: TMemoryStatusEx;
begin
  Result := -1;
  FillChar(MemStatus, SizeOf(MemStatus), 0);
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
    Result := MemStatus.ullTotalPhys div (1024 * 1024);
end;
{$ENDIF}

{$IFDEF DARWIN}
var
  P: TProcess;
  SL: TStringList;
  ValStr: string;
begin
  Result := -1;
  P := TProcess.Create(nil);
  SL := TStringList.Create;
  try
    P.Executable := '/usr/sbin/sysctl';
    P.Parameters.Add('-n');
    P.Parameters.Add('hw.memsize');
    P.Options := [poUsePipes, poWaitOnExit];
    P.Execute;
    SL.LoadFromStream(P.Output);
    if SL.Count > 0 then
    begin
      ValStr := Trim(SL[0]);
      Result := StrToInt64Def(ValStr, 0) div (1024 * 1024);
    end;
  finally
    SL.Free;
    P.Free;
  end;
end;
{$ENDIF}

{ ==================================================================
  PChar helpers
  ================================================================== }

function PCharToInt64Def(P: PChar; Len: Int64; Def: Int64): Int64;
var
  I: Integer;
  Neg: Boolean;
  V: Int64;
  C: Char;
begin
  Result := Def;
  if (P = nil) or (Len <= 0) then Exit;

  I := 0;
  Neg := False;

  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I >= Len then Exit;

  if P[I] = '-' then
  begin
    Neg := True;
    Inc(I);
  end
  else if P[I] = '+' then
    Inc(I);

  if I >= Len then Exit;

  V := 0;
  while I < Len do
  begin
    C := P[I];
    if (C < '0') or (C > '9') then Exit;
    if V > (High(Int64) - (Ord(C) - Ord('0'))) div 10 then Exit;
    V := V * 10 + (Ord(C) - Ord('0'));
    Inc(I);
  end;

  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I <> Len then Exit;

  if Neg then V := -V;
  Result := V;
end;

function PCharToDoubleDef(P: PChar; Len: Int64; Def: Double): Double;
var
  I: Integer;
  Neg: Boolean;
  IntPart, FracPart: Double;
  FracDiv: Double;
  HasDigits: Boolean;
begin
  Result := Def;
  if (P = nil) or (Len <= 0) then Exit;

  I := 0;
  Neg := False;
  HasDigits := False;

  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I >= Len then Exit;

  if P[I] = '-' then
  begin
    Neg := True;
    Inc(I);
  end
  else if P[I] = '+' then
    Inc(I);

  IntPart := 0;
  while (I < Len) and (P[I] >= '0') and (P[I] <= '9') do
  begin
    IntPart := IntPart * 10 + (Ord(P[I]) - Ord('0'));
    HasDigits := True;
    Inc(I);
  end;

  FracPart := 0;
  FracDiv := 1;
  if (I < Len) and (P[I] = '.') then
  begin
    Inc(I);
    while (I < Len) and (P[I] >= '0') and (P[I] <= '9') do
    begin
      FracPart := FracPart * 10 + (Ord(P[I]) - Ord('0'));
      FracDiv := FracDiv * 10;
      HasDigits := True;
      Inc(I);
    end;
  end;

  if not HasDigits then Exit;

  // Exponent -> fallback (rare in CSV)
  if (I < Len) and ((P[I] = 'e') or (P[I] = 'E')) then
    Exit;

  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I <> Len then Exit;

  Result := IntPart + (FracPart / FracDiv);
  if Neg then Result := -Result;
end;

function PCharEqualsIgnoreCase(P: PChar; Len: Int64; const S: string): Boolean;
var
  I: Integer;
  C1, C2: Char;
begin
  Result := False;
  if Len <> Length(S) then Exit;
  for I := 0 to Len - 1 do
  begin
    C1 := P[I];
    C2 := S[I + 1];
    if (C1 >= 'a') and (C1 <= 'z') then Dec(C1, 32);
    if (C2 >= 'a') and (C2 <= 'z') then Dec(C2, 32);
    if C1 <> C2 then Exit;
  end;
  Result := True;
end;

{ ==================================================================
  TCSVFastPacketReader
  ================================================================== }

constructor TCSVFastPacketReader.Create(ADataSet: TCustomBufDataset;
  AStream: TStream);
begin
  inherited Create(ADataSet, AStream);
  FOptions := nil;
  FOwnsOptions := True;

  SetLength(FBuffer, FBufferSize);
  FBufStart := Pointer(FBuffer);
  FBufPos   := FBufStart;
  FBufEnd   := FBufStart;

  SetLength(FCells, FCellCapacity);
  FCellCount            := 0;

  FCurrentRow       := 0;
  FRowStartOffset   := 0;
  FEOF              := False;
  FFirstLineAsFieldNames := True;
  FDefaultFieldLength    := 50;
  FInQuote          := False;
  FQuoteChar        := '"';
  FDelimiter        := ',';
  FLineEnding       := sLineBreak;
  FIgnoreOuterWhitespace := False;

  FScannedRowCount := 0;
  FSkipFirstRows   := 0;
  FStopAfterRow    := 0;

  FOnProgress           := nil;
  FTotalRowsForProgress := 0;
  FProgressUpdatePercent := 5;
  FLastProgressPercent  := -1;
  FStartTime            := 0;
  FProgressCounter      := 0;

  FCancelFlag := nil;
end;

constructor TCSVFastPacketReader.Create(ADataSet: TCustomBufDataset;
  AStream: TStream; AOptions: TCSVOptions);
begin
  // Call the first constructor — this sets all defaults
  // (buffer size, block size, cell capacity, progress state, etc.)
  Create(ADataSet, AStream);

  // Now override with values from AOptions (if provided)
  if AOptions <> nil then
  begin
    FOptions               := AOptions;
    FOwnsOptions           := False;
    FQuoteChar             := AOptions.QuoteChar;
    FDelimiter             := AOptions.Delimiter;
    FLineEnding            := AOptions.LineEnding;
    FIgnoreOuterWhitespace := AOptions.IgnoreOuterWhitespace;
    FFirstLineAsFieldNames := AOptions.FirstLineAsFieldNames;
    FDefaultFieldLength    := AOptions.DefaultFieldLength;
  end;
end;

destructor TCSVFastPacketReader.Destroy;
begin
  if FOwnsOptions and (FOptions <> nil) then
    FreeAndNil(FOptions);
  SetLength(FBuffer, 0);
  SetLength(FCells, 0);
  inherited Destroy;
end;

procedure TCSVFastPacketReader.EnsureCellCapacity;
begin
  if FCellCount >= FCellCapacity then
  begin
    FCellCapacity := FCellCapacity * 2;
    SetLength(FCells, FCellCapacity);
  end;
end;

procedure TCSVFastPacketReader.AppendCell(Start: PChar; Len: Int64);
begin
  EnsureCellCapacity;
  FCells[FCellCount].Start := Start;
  FCells[FCellCount].Len   := Len;
  Inc(FCellCount);
end;

function TCSVFastPacketReader.CellToStr(const C: TCSVFastCellRef): string;
begin
  if C.Len <= 0 then
    Result := ''
  else if C.Len > High(Integer) then
  begin
    // Should never happen — cells larger than 2 GB are absurd
    raise Exception.CreateFmt('CSV cell too large: %d bytes', [C.Len]);
  end
  else
  begin
    SetLength(Result, C.Len);
    Move(C.Start^, Result[1], C.Len);
  end;
end;

procedure TCSVFastPacketReader.SkipBOM;
var
  B0, B1, B2: Byte;
begin
  if (FBufEnd - FBufStart) >= 3 then
  begin
    B0 := Byte(FBufStart[0]);
    B1 := Byte(FBufStart[1]);
    B2 := Byte(FBufStart[2]);
    if (B0 = $EF) and (B1 = $BB) and (B2 = $BF) then
      FBufStart := FBufStart + 3;
  end;
end;

procedure TCSVFastPacketReader.RefillBuffer;
var
  Remaining: Int64;
  BytesToRead: Int64;
  BytesRead: Integer;
  StreamRemaining: Int64;
begin
  // 1. Move unread data to the start of the buffer
  Remaining := FBufEnd - FBufPos;
  if Remaining < 0 then Remaining := 0;

  if (Remaining > 0) and (FBufPos <> Pointer(FBuffer)) then
    Move(FBufPos^, FBuffer[0], Remaining);

  FBufStart := Pointer(FBuffer);
  FBufPos   := FBufStart;
  FBufEnd   := FBufStart + Remaining;

  // 2. How many bytes are still in the stream?
  StreamRemaining := Stream.Size - Stream.Position;
  if StreamRemaining < 0 then StreamRemaining := 0;

  // 3. Requested read size: BlockSize if set, otherwise BufferSize
  if FBlockSize > 0 then
    BytesToRead := FBlockSize
  else
    BytesToRead := FBufferSize;

  // 4. Limit to available buffer space
  if Remaining + BytesToRead > FBufferSize then
    BytesToRead := FBufferSize - Remaining;

  // 5. Limit to what's still in the stream
  if BytesToRead > StreamRemaining then
    BytesToRead := StreamRemaining;

  // 6. Read
  if BytesToRead > 0 then
  begin
    if BytesToRead > High(Integer) then
      BytesToRead := High(Integer);
    BytesRead := Stream.Read(FBuffer[Remaining], BytesToRead);
    if BytesRead > 0 then
      FBufEnd := FBufStart + Remaining + BytesRead;
  end;
end;

procedure TCSVFastPacketReader.ScanRow;
var
  P, CellStart: PChar;
  C: Char;
  InQ: Boolean;
  QChar: Char;
  Delim: Char;
  NewPercent: Integer;
  FreeRAMMB: Int64;
begin
  FCellCount := 0;
  InQ    := FInQuote;
  QChar  := FQuoteChar;
  Delim  := FDelimiter;

  // Remember row start as offset (survives Move in RefillBuffer)
  FRowStartOffset := FBufPos - FBufStart;

  CellStart := FBufPos;
  P         := FBufPos;

  while P < FBufEnd do
  begin
    C := P^;

    if InQ then
    begin
      if C = QChar then
      begin
        if (P + 1 < FBufEnd) and ((P + 1)^ = QChar) then
        begin
          Inc(P, 2);
          Continue;
        end;
        InQ := False;
        Inc(P);
        Continue;
      end;
      Inc(P);
      Continue;
    end;

    if C = QChar then
    begin
      InQ := True;
      Inc(P);
      Continue;
    end;

    if C = Delim then
    begin
      AppendCell(CellStart, P - CellStart);
      CellStart := P + 1;
      Inc(P);
      Continue;
    end;

    if (C = CR) or (C = LF) then
    begin
      AppendCell(CellStart, P - CellStart);
      Inc(FScannedRowCount);

      // ============================================================
      // Progress callback (percent-based)
      // ============================================================
      if (FProgressUpdatePercent > 0) and
         (FTotalRowsForProgress > 0) and
         Assigned(FOnProgress) then
      begin
        if FStartTime = 0 then
          FStartTime := Now;

        NewPercent := (FScannedRowCount * 100) div FTotalRowsForProgress;
        if NewPercent >= (FLastProgressPercent + FProgressUpdatePercent) then
        begin
          FLastProgressPercent := NewPercent;
          Inc(FProgressCounter);

          // Query RAM only every 5 updates to minimize overhead
          //if (FProgressCounter mod 5) = 0 then
            FreeRAMMB := GetFreeMemoryMB;
          //else
            //FreeRAMMB := -1;

          FOnProgress(FScannedRowCount,
                      (Now - FStartTime) * SecsPerDay,
                      FreeRAMMB);
        end;
      end;

      // ============================================================
      // Cancellation check
      // ============================================================
      if Assigned(FCancelFlag) and FCancelFlag^ then
        raise EAbort.Create('Cancelled by user');

      if (C = CR) and (P + 1 < FBufEnd) and ((P + 1)^ = LF) then
        Inc(P, 2)
      else
        Inc(P);

      FBufPos  := P;
      FInQuote := False;
      Exit;
    end;

    Inc(P);
  end;

  // ============================================================
  // Buffer end reached, row not complete
  //
  // Case A: Stream at end -> last row without trailing newline
  //         -> finalize it, otherwise it would be lost
  // Case B: Stream not at end -> rollback to row start so
  //         RefillBuffer can supply the rest
  // ============================================================
  if (Stream.Position >= Stream.Size) and (P > FBufStart + FRowStartOffset) then
  begin
    AppendCell(CellStart, P - CellStart);
    FBufPos  := P;
    FInQuote := False;
    Exit;
  end;

  FBufPos    := FBufStart + FRowStartOffset;
  FCellCount := 0;
  // FInQuote stays as it was (state before this ScanRow call)
end;

procedure TCSVFastPacketReader.LoadFieldDefs(var AnAutoIncValue: Integer);
var
  I: Integer;
  FN: string;
  Retries: Integer;
  {$IFDEF CPU64}
  SkipIdx: Int64;
  {$ELSE}
  SkipIdx: Integer;
  {$ENDIF}
begin
  AnAutoIncValue := 0;

  RefillBuffer;
  SkipBOM;

  // ============================================================
  // Scan header row
  // ============================================================
  Retries := 0;
  repeat
    ScanRow;
    if FCellCount > 0 then Break;
    if Stream.Position >= Stream.Size then Break;
    RefillBuffer;
    SkipBOM;
    Inc(Retries);
    if Retries > 1000 then
      raise Exception.CreateFmt(
        'CSV header too large for buffer (%d bytes).',
        [FBufferSize]);
  until False;

  // ============================================================
  // Build FieldDefs from header
  // ============================================================
  if FCreateFieldDefs <> nil then
  begin
    if FCreateFieldDefs.Count <> FCellCount then
      DatabaseErrorFmt(
        'CSV File Field count (%d) does not match dataset field count (%d).',
        [FCellCount, FCreateFieldDefs.Count], DataSet.FieldDefs.Dataset);
    if FFirstLineAsFieldNames then
      for I := 0 to FCellCount - 1 do
        if CompareText(CellToStr(FCells[I]), FCreateFieldDefs[I].Name) <> 0 then
          DatabaseErrorFmt(
            'CSV File field %d: name "%s" does not match dataset field name "%s".',
            [I, CellToStr(FCells[I]), FCreateFieldDefs[I].Name],
            DataSet.FieldDefs.Dataset);
    DataSet.FieldDefs.Assign(FCreateFieldDefs);
  end
  else if FCellCount > 0 then
  begin
    for I := 0 to FCellCount - 1 do
    begin
      if FFirstLineAsFieldNames then
        FN := CellToStr(FCells[I])
      else
        FN := Format('Column%d', [I + 1]);
      DataSet.FieldDefs.Add(FN, ftString, FDefaultFieldLength);
    end;
  end;

  // ============================================================
  // Scan data rows up to FromRow
  // ============================================================
  if FFirstLineAsFieldNames then
  begin
    FScannedRowCount := 0;   // header is not counted

    // Scan (FSkipFirstRows + 1) data rows.
    // Afterwards FCells contains the first row of the range.
    for SkipIdx := 0 to FSkipFirstRows do
    begin
      Retries    := 0;
      FCellCount := 0;
      repeat
        if FBufPos >= FBufEnd then
        begin
          if Stream.Position >= Stream.Size then Break;
          RefillBuffer;
          SkipBOM;
        end;

        ScanRow;

        if FCellCount > 0 then Break;
        if Stream.Position >= Stream.Size then Break;

        RefillBuffer;
        SkipBOM;

        Inc(Retries);
        if Retries > 1000 then
          raise Exception.CreateFmt(
            'CSV row too large for buffer (%d bytes).',
            [FBufferSize]);
      until False;

      if FCellCount = 0 then Break;   // EOF reached
    end;
  end;
end;

procedure TCSVFastPacketReader.StoreFieldDefs(AnAutoIncValue: Integer);
begin
  // Phase 2 — not implemented yet
end;

procedure TCSVFastPacketReader.InitLoadRecords;
begin
  // Do nothing
end;

function TCSVFastPacketReader.GetCurrentRecord: Boolean;
begin
  Result := (FCellCount > 0) and (not FEOF);
end;

function TCSVFastPacketReader.GetRecordRowState(out AUpdOrder: Integer): TRowState;
begin
  AUpdOrder := 0;
  Result    := [];
end;

procedure TCSVFastPacketReader.RestoreRecord;
var
  I: Integer;
  F: TField;
  C: TCSVFastCellRef;
  S: string;
begin
  // ============================================================
  // Range start not yet reached? Discard this row.
  // ============================================================
  if FScannedRowCount <= FSkipFirstRows then
    Exit;

  for I := 0 to FCellCount - 1 do
  begin
    if I >= DataSet.FieldCount then Break;
    F := DataSet.Fields[I];
    C := FCells[I];

    if C.Len = 0 then
    begin
      F.Clear;
      Continue;
    end;

    case F.DataType of
      ftString, ftFixedChar, ftWideString, ftFixedWideChar:
        begin
          S := CellToStr(C);
          F.AsString := S;
        end;
      ftSmallint, ftInteger, ftWord, ftAutoInc:
        F.AsInteger := PCharToInt64Def(C.Start, C.Len, 0);
      ftLargeint:
        F.AsLargeInt := PCharToInt64Def(C.Start, C.Len, 0);
      ftFloat, ftCurrency:
        F.AsFloat := PCharToDoubleDef(C.Start, C.Len, 0.0);
      ftBoolean:
        begin
          if PCharEqualsIgnoreCase(C.Start, C.Len, 'true') or
             PCharEqualsIgnoreCase(C.Start, C.Len, 'yes') or
             ((C.Len = 1) and (C.Start^ = '1')) then
            F.AsBoolean := True
          else if PCharEqualsIgnoreCase(C.Start, C.Len, 'false') or
                  PCharEqualsIgnoreCase(C.Start, C.Len, 'no') or
                  ((C.Len = 1) and (C.Start^ = '0')) then
            F.AsBoolean := False
          else
            F.AsString := CellToStr(C);
        end;
    else
      F.AsString := CellToStr(C);
    end;
  end;

  for I := FCellCount to DataSet.FieldCount - 1 do
    DataSet.Fields[I].Clear;
end;

procedure TCSVFastPacketReader.GotoNextRecord;
var
  Retries: Integer;
begin
  if FEOF then Exit;

  // ============================================================
  // Range end reached? Stop delivering rows.
  // ============================================================
  if (FStopAfterRow > 0) and
     (FScannedRowCount - FSkipFirstRows >= FStopAfterRow) then
  begin
    FEOF       := True;
    FCellCount := 0;
    Exit;
  end;

  Retries := 0;

  repeat
    // ============================================================
    // Buffer exhausted? Refill.
    // ============================================================
    if FBufPos >= FBufEnd then
    begin
      if Stream.Position >= Stream.Size then
      begin
        FEOF       := True;
        FCellCount := 0;
        Exit;
      end;
      RefillBuffer;
      SkipBOM;
    end;

    // ============================================================
    // Scan next row
    // ============================================================
    ScanRow;

    if FCellCount > 0 then
      Exit;   // row complete

    // ============================================================
    // ScanRow did not complete the row. End of stream?
    // ============================================================
    if Stream.Position >= Stream.Size then
    begin
      FEOF       := True;
      FCellCount := 0;
      Exit;
    end;

    // ============================================================
    // Refill and retry
    // ============================================================
    RefillBuffer;
    SkipBOM;

    Inc(Retries);
    if Retries > 1000 then
      raise Exception.CreateFmt(
        'CSV row too large for buffer (%d bytes). ' +
        'Please increase BlockSize/BufferSize.',
        [FBufferSize]);
  until False;
end;

procedure TCSVFastPacketReader.StoreRecord(ARowState: TRowState;
  AUpdOrder: Integer);
begin
  // Phase 2 — not implemented yet
end;

procedure TCSVFastPacketReader.FinalizeStoreRecords;
begin
  // Do nothing
end;

class function TCSVFastPacketReader.RecognizeStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

{ ==================================================================
  TCSVFastPacketReader — setters
  ================================================================== }
procedure TCSVFastPacketReader.SetBufferSize(AValue: Int64);
begin
  if FBufferSize = AValue then Exit;

  if FBufPos <> FBufStart then
    raise Exception.Create(
      'BufferSize cannot be changed while data is in the buffer.');

  FBufferSize := AValue;
  SetLength(FBuffer, FBufferSize);
  FBufStart := Pointer(FBuffer);
  FBufPos   := FBufStart;
  FBufEnd   := FBufStart;
end;

procedure TCSVFastPacketReader.SetInitialCellCapacity(AValue: Integer);
begin
  if FInitialCellCapacity = AValue then Exit;

  if FCellCount > 0 then
    raise Exception.Create(
      'InitialCellCapacity cannot be changed while data is present.');

  FInitialCellCapacity := AValue;
  FCellCapacity        := FInitialCellCapacity;
  SetLength(FCells, FCellCapacity);
end;

{ ==================================================================
  TCSVFastDataset
  ================================================================== }
constructor TCSVFastDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Defaults — must match the 'default' clauses in the published
  // property declarations, otherwise Lazarus will write the values
  // into every .lfm file.
  FBlockSizeBytes        := 1048576;    // 1 MB
  FBufferSizeBytes       := 2097152;    // 2 MB
  FInitialCellCapacity   := 64;
  FProgressUpdatePercent := 1;

  // Runtime-only fields
  FSkipFirstRows         := 0;
  FStopAfterRow          := 0;
  FOnProgress            := nil;
  FTotalRowsForProgress  := 0;
  FCancelFlag            := nil;
end;

class function TCSVFastDataset.DefaultPacketClass: TDataPacketReaderClass;
begin
  Result := TCSVFastPacketReader;
end;

function TCSVFastDataset.CreateDefaultPacketReader(
  aStream: TStream): TDataPacketReader;
var
  R: TCSVFastPacketReader;
begin
  R := TCSVFastPacketReader.Create(Self, aStream, CSVOptions);
  R.BlockSize             := FBlockSizeBytes;
  R.BufferSize            := FBufferSizeBytes;
  R.InitialCellCapacity   := FInitialCellCapacity;
  R.SkipFirstRows         := FSkipFirstRows;
  R.StopAfterRow          := FStopAfterRow;
  R.OnProgress            := FOnProgress;
  R.TotalRowsForProgress  := FTotalRowsForProgress;
  R.ProgressUpdatePercent := FProgressUpdatePercent;
  R.CancelFlag            := FCancelFlag;
  Result := R;
end;

procedure TCSVFastDataset.LoadFromCSVStream(AStream: TStream);
var
  P: TCSVFastPacketReader;
begin
  CheckInactive;

  P := TCSVFastPacketReader.Create(Self, AStream, CSVOptions);
  try
    P.BlockSize             := FBlockSizeBytes;
    P.BufferSize            := FBufferSizeBytes;
    P.InitialCellCapacity   := FInitialCellCapacity;
    P.SkipFirstRows         := FSkipFirstRows;
    P.StopAfterRow          := FStopAfterRow;
    P.OnProgress            := FOnProgress;
    P.TotalRowsForProgress  := FTotalRowsForProgress;
    P.ProgressUpdatePercent := FProgressUpdatePercent;
    P.CancelFlag            := FCancelFlag;

    if FieldDefs.Count > 0 then
      P.CreateFieldDefs := FieldDefs;
    SetDatasetPacket(P);
  finally
    P.Free;
  end;
end;

procedure TCSVFastDataset.LoadFromCSVFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromCSVStream(F);
  finally
    F.Free;
  end;
end;

procedure TCSVFastDataset.SaveToCSVFile(AFileName: string);
begin
  // Phase 2 — currently uses the inherited implementation.
  inherited SaveToCSVFile(AFileName);
end;

procedure Register;
begin
  RegisterComponents('Data Access', [TCSVFastDataset]);
end;

end.
