{
  uCSVFastDataset.pas

  High-performance CSV dataset for Lazarus / Free Pascal.
  Drop-in replacement for TCSVDataset with a PChar block scanner.

  ----------------------------------------------------------------------
  CANCELLATION — BOTH MECHANISMS AVAILABLE
  ----------------------------------------------------------------------

  On cancel, the reader does TWO things:

    1. Sets its own Aborted flag (readable via the dataset property
       TCSVFastDataset.Aborted, which survives the load call).
    2. Raises EAbort so existing GUI code that catches EAbort keeps
       working.

  The caller can use either signal — or both.

  ----------------------------------------------------------------------
  MEMORY ACCOUNTING — DONE BY THE CALLER, NOT BY THIS UNIT
  ----------------------------------------------------------------------

  This unit does NOT query the OS for free RAM and does NOT read
  /proc/meminfo, GlobalMemoryStatusEx or sysctl.

  The caller may supply:
      InitialFreeRAM_MB  - free physical RAM in MB at load start
      MaxRowWidth        - max row width in bytes (from a pre-scan)
  When both are > 0, OnProgress receives a conservative estimate
  of memory used so far in AEstimatedUsageMB. The caller decides
  whether to warn, continue, or set CancelFlag.

  ----------------------------------------------------------------------
  RESPONSIBILITY
  ----------------------------------------------------------------------

  Fast, clean CSV loading. Nothing else.
}

unit uCSVFastDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, csvdataset, csvreadwrite;

type
  { Progress event.

    ARowsRead         - Data rows read so far (header excluded).
    AElapsedSeconds   - Elapsed seconds since load started.
    AFreeRAMMB        - Free physical RAM in MB at load start,
                        or -1 if the caller did not supply it.
    AEstimatedUsageMB - Conservative estimate of memory used so
                        far in MB, or -1 if not computable (needs
                        both InitialFreeRAM_MB and MaxRowWidth). }

  TCSVLoadProgressEvent = procedure(
    ARowsRead: Int64;
    ABytesRead: Int64;
    AElapsedSeconds: Double;
    AFreeRAMMB: Int64;
    AEstimatedUsageMB: Int64) of object;

type
  TCSVFastDataset = class;

  TCSVFastCellRef = record
    Start: PChar;
    Len: Int64;
  end;
  TCSVFastCellArray = array of TCSVFastCellRef;

  TCSVFastReaderState = (
    frsIdle,
    frsLoading,
    frsFinished,
    frsAborted
    );

  { ------------------------------------------------------------------
    TCSVFastPacketReader
    ------------------------------------------------------------------ }
  TCSVFastPacketReader = class(TDataPacketReader)
  private
  FOptions: TCSVOptions;
  FOwnsOptions: Boolean;

  FBuffer: TBytes;
  FBufferSize: Int64;
  FBlockSize: Integer;
  FBufStart: PChar;
  FBufPos: PChar;
  FBufEnd: PChar;
  FRowStartOffset: Integer;

  FCells: TCSVFastCellArray;
  FCellCount: Integer;
  FCellCapacity: Integer;
  FInitialCellCapacity: Integer;

  FCreateFieldDefs: TFieldDefs;

  FReaderState: TCSVFastReaderState;
  FFirstLineAsFieldNames: Boolean;
  FDefaultFieldLength: Integer;

  FStreamStartPosition: Int64;

  FQuoteChar: Char;
  FDelimiter: Char;
  FLineEnding: string;
  FIgnoreOuterWhitespace: Boolean;

  FBOMChecked: Boolean;
  FPendingLF: Boolean;

  FScannedRowCount: Int64;
  FSkipFirstRows: Int64;
  FStopAfterRow: Int64;

  FOnProgress: TCSVLoadProgressEvent;
  FTotalRowsForProgress: Int64;
  FProgressUpdatePercent: Integer;
  FLastProgressPercent: Integer;
  FStartTime: TDateTime;
  FProgressFiredOnce: Boolean;

  FInitialFreeRAM_MB: Int64;
  FMaxRowWidth: Int64;

  FCancelFlag: PBoolean;
  FAborted: Boolean;

procedure InitBuffer;
procedure RefillBuffer;
procedure SkipBOMIfNeeded;
procedure ScanRow;
function  ScanRowWithRefill(out ARowComplete: Boolean): Boolean;
procedure AppendCell(Start: PChar; Len: Int64);
procedure EnsureCellCapacity;
function  CellToStr(const C: TCSVFastCellRef): string;
procedure SetBufferSize(AValue: Int64);
procedure SetInitialCellCapacity(AValue: Integer);
procedure CheckCancellation;      // sets FAborted and raises EAbort
procedure FireProgress(AForce: Boolean);
procedure ResetProgress;
function  ComputeEstimatedUsageMB: Int64;
function  ComputeRowDisposition: Boolean;
protected
property CreateFieldDefs: TFieldDefs
read FCreateFieldDefs write FCreateFieldDefs;
public
constructor Create(ADataSet: TCustomBufDataset; AStream: TStream); override;
constructor Create(ADataSet: TCustomBufDataset; AStream: TStream;
                   AOptions: TCSVOptions);
destructor  Destroy; override;

procedure   LoadFieldDefs(var AnAutoIncValue: Integer); override;
procedure   StoreFieldDefs(AnAutoIncValue: Integer); override;
procedure   InitLoadRecords; override;
function    GetCurrentRecord: Boolean; override;
function    GetRecordRowState(out AUpdOrder: Integer): TRowState; override;
procedure   RestoreRecord; override;
procedure   GotoNextRecord; override;
procedure   StoreRecord(ARowState: TRowState;
                        AUpdOrder: Integer = 0); override;
procedure   FinalizeStoreRecords; override;
class function RecognizeStream(AStream: TStream): Boolean; override;

property Options: TCSVOptions read FOptions;
property BlockSize: Integer read FBlockSize write FBlockSize;
property BufferSize: Int64 read FBufferSize write SetBufferSize;
property InitialCellCapacity: Integer read FInitialCellCapacity
write SetInitialCellCapacity;

property SkipFirstRows: Int64 read FSkipFirstRows write FSkipFirstRows;
property StopAfterRow: Int64 read FStopAfterRow write FStopAfterRow;
property ScannedRowCount: Int64 read FScannedRowCount;

property OnProgress: TCSVLoadProgressEvent
read FOnProgress write FOnProgress;
property TotalRowsForProgress: Int64
read FTotalRowsForProgress write FTotalRowsForProgress;
property ProgressUpdatePercent: Integer
read FProgressUpdatePercent write FProgressUpdatePercent;

property InitialFreeRAM_MB: Int64
read FInitialFreeRAM_MB write FInitialFreeRAM_MB;
property MaxRowWidth: Int64
read FMaxRowWidth write FMaxRowWidth;

property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;
property Aborted: Boolean read FAborted;
end;

  { ------------------------------------------------------------------
    TCSVFastDataset
    ------------------------------------------------------------------ }
TCSVFastDataset = class(TCSVDataset)
private
FBlockSizeBytes: Integer;
FBufferSizeBytes: Int64;
FInitialCellCapacity: Integer;
FSkipFirstRows: Int64;
FStopAfterRow: Int64;
FOnProgress: TCSVLoadProgressEvent;
FTotalRowsForProgress: Int64;
FProgressUpdatePercent: Integer;
FInitialFreeRAM_MB: Int64;
FMaxRowWidth: Int64;
FCancelFlag: PBoolean;
FAborted: Boolean;
protected
class function DefaultPacketClass: TDataPacketReaderClass; override;
function  CreateDefaultPacketReader(aStream: TStream): TDataPacketReader; override;
procedure ApplyConfigToReader(R: TCSVFastPacketReader);
public
constructor Create(AOwner: TComponent); override;

procedure LoadFromCSVFile(const AFileName: string); reintroduce;
procedure LoadFromCSVStream(AStream: TStream); reintroduce;
procedure SaveToCSVFile(AFileName: string = ''); reintroduce;

property SkipFirstRows: Int64 read FSkipFirstRows write FSkipFirstRows;
property StopAfterRow: Int64 read FStopAfterRow write FStopAfterRow;
property TotalRowsForProgress: Int64
read FTotalRowsForProgress write FTotalRowsForProgress;
property InitialFreeRAM_MB: Int64
read FInitialFreeRAM_MB write FInitialFreeRAM_MB;
property MaxRowWidth: Int64
read FMaxRowWidth write FMaxRowWidth;
property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;

    { True after the last LoadFromCSVFile / LoadFromCSVStream if the
      load was cancelled via CancelFlag. Reset to False at the start
      of every new load. }
property Aborted: Boolean read FAborted;

published
property BlockSize: Integer
read FBlockSizeBytes write FBlockSizeBytes default 1048576;
property BufferSize: Int64
read FBufferSizeBytes write FBufferSizeBytes default 2097152;
property InitialCellCapacity: Integer
read FInitialCellCapacity write FInitialCellCapacity default 64;
property OnProgress: TCSVLoadProgressEvent
read FOnProgress write FOnProgress;
property ProgressUpdatePercent: Integer
read FProgressUpdatePercent write FProgressUpdatePercent default 1;
end;

function PCharToInt64Def(P: PChar; Len: Int64; Def: Int64): Int64; inline;
function PCharToDoubleDef(P: PChar; Len: Int64; Def: Double): Double; inline;
function PCharEqualsIgnoreCase(P: PChar; Len: Int64; const S: string): Boolean; inline;

function GetFreeMemoryMB: Int64;
function GetTotalMemoryMB: Int64;

procedure Register;

implementation

uses
  {$IFDEF WINDOWS} Windows {$ENDIF}
  {$IFDEF LINUX}   BaseUnix {$ENDIF}
  {$IFDEF DARWIN}  Process {$ENDIF}
  ;

const
  CR = #13;
  LF = #10;
  TAB = #9;
  SPACE = #32;
  UTF8_BOM_0 = $EF;
  UTF8_BOM_1 = $BB;
  UTF8_BOM_2 = $BF;

  DEFAULT_BLOCK_SIZE    = 1048576;
  DEFAULT_BUFFER_SIZE   = 2097152;
  DEFAULT_CELL_CAPACITY = 64;
  DEFAULT_FIELD_LENGTH  = 50;

  ROW_OVERHEAD_ESTIMATE = 64;   // conservative bytes/row for the estimator

{ ==================================================================
  RAM utilities (callable by the GUI; never called by the reader)
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
        Result := MemKB div 1024;
      Break;
    end;
  end;
  finally
  SL.Free;
end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
type
  TMemoryStatusEx = record
    dwLength: DWORD; dwMemoryLoad: DWORD;
    ullTotalPhys, ullAvailPhys: QWord;
    ullTotalPageFile, ullAvailPageFile: QWord;
    ullTotalVirtual, ullAvailVirtual: QWord;
    ullAvailExtendedVirtual: QWord;
  end;
function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL;
stdcall; external 'kernel32.dll' name 'GlobalMemoryStatusEx';
{$ENDIF}

function GetFreeMemoryMB: Int64;
{$IFDEF LINUX}
begin
  Result := ReadMemInfoValue('MemAvailable');
  if Result < 0 then Result := ReadMemInfoValue('MemFree');
end;
{$ENDIF}
{$IFDEF WINDOWS}
var M: TMemoryStatusEx;
begin
  Result := -1;
  FillChar(M, SizeOf(M), 0);
  M.dwLength := SizeOf(M);
  if GlobalMemoryStatusEx(M) then Result := M.ullAvailPhys div (1024*1024);
end;
{$ENDIF}
{$IFDEF DARWIN}
begin
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
var M: TMemoryStatusEx;
begin
  Result := -1;
  FillChar(M, SizeOf(M), 0);
  M.dwLength := SizeOf(M);
  if GlobalMemoryStatusEx(M) then Result := M.ullTotalPhys div (1024*1024);
end;
{$ENDIF}
{$IFDEF DARWIN}
var P: TProcess; SL: TStringList;
begin
  Result := -1;
  P := TProcess.Create(nil);
  SL := TStringList.Create;
  try
  P.Executable := '/usr/sbin/sysctl';
  P.Parameters.Add('-n'); P.Parameters.Add('hw.memsize');
  P.Options := [poUsePipes, poWaitOnExit];
  P.Execute;
  SL.LoadFromStream(P.Output);
  if SL.Count > 0 then
    Result := StrToInt64Def(Trim(SL[0]), 0) div (1024*1024);
  finally
  SL.Free; P.Free;
end;
end;
{$ENDIF}

{ ==================================================================
  PChar helpers
  ================================================================== }

function PCharToInt64Def(P: PChar; Len: Int64; Def: Int64): Int64;
var I: Integer; Neg: Boolean; V: Int64; C: Char;
begin
  Result := Def;
  if (P = nil) or (Len <= 0) then Exit;
  I := 0; Neg := False;
  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I >= Len then Exit;
  if P[I] = '-' then begin Neg := True; Inc(I); end
  else if P[I] = '+' then Inc(I);
  if I >= Len then Exit;
  V := 0;
  while I < Len do
  begin
    C := P[I];
    if (C < '0') or (C > '9') then Exit;
    if V > (High(Int64) - (Ord(C)-Ord('0'))) div 10 then Exit;
    V := V * 10 + (Ord(C)-Ord('0'));
    Inc(I);
  end;
  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I <> Len then Exit;
  if Neg then V := -V;
  Result := V;
end;

function PCharToDoubleDef(P: PChar; Len: Int64; Def: Double): Double;
var I: Integer; Neg, HasDigits: Boolean; IntPart, FracPart, FracDiv: Double;
begin
  Result := Def;
  if (P = nil) or (Len <= 0) then Exit;
  I := 0; Neg := False; HasDigits := False;
  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I >= Len then Exit;
  if P[I] = '-' then begin Neg := True; Inc(I); end
  else if P[I] = '+' then Inc(I);
  IntPart := 0;
  while (I < Len) and (P[I] >= '0') and (P[I] <= '9') do
  begin
    IntPart := IntPart * 10 + (Ord(P[I])-Ord('0'));
    HasDigits := True;
    Inc(I);
  end;
  FracPart := 0; FracDiv := 1;
  if (I < Len) and (P[I] = '.') then
  begin
    Inc(I);
    while (I < Len) and (P[I] >= '0') and (P[I] <= '9') do
    begin
      FracPart := FracPart * 10 + (Ord(P[I])-Ord('0'));
      FracDiv := FracDiv * 10;
      HasDigits := True;
      Inc(I);
    end;
  end;
  if not HasDigits then Exit;
  if (I < Len) and ((P[I] = 'e') or (P[I] = 'E')) then Exit;
  while (I < Len) and (P[I] in [SPACE, TAB]) do Inc(I);
  if I <> Len then Exit;
  Result := IntPart + (FracPart / FracDiv);
  if Neg then Result := -Result;
end;

function PCharEqualsIgnoreCase(P: PChar; Len: Int64; const S: string): Boolean;
var I: Integer; C1, C2: Char;
begin
  Result := False;
  if Len <> Length(S) then Exit;
  for I := 0 to Len - 1 do
  begin
    C1 := P[I]; C2 := S[I+1];
    if (C1 >= 'a') and (C1 <= 'z') then Dec(C1, 32);
    if (C2 >= 'a') and (C2 <= 'z') then Dec(C2, 32);
    if C1 <> C2 then Exit;
  end;
  Result := True;
end;

{ ==================================================================
  TCSVFastPacketReader
  ================================================================== }
constructor TCSVFastPacketReader.Create(ADataSet: TCustomBufDataset; AStream: TStream);
begin
  FStreamStartPosition := AStream.Position;
  inherited Create(ADataSet, AStream);
  FOptions               := nil;
  FOwnsOptions           := True;
  FBufferSize            := DEFAULT_BUFFER_SIZE;
  FBlockSize             := DEFAULT_BLOCK_SIZE;
  FInitialCellCapacity   := DEFAULT_CELL_CAPACITY;
  FCellCapacity          := FInitialCellCapacity;
  FCellCount             := 0;
  FRowStartOffset        := 0;
  FReaderState           := frsIdle;
  FFirstLineAsFieldNames := True;
  FDefaultFieldLength    := DEFAULT_FIELD_LENGTH;
  FQuoteChar             := '"';
  FDelimiter             := ',';
  FLineEnding            := sLineBreak;
  FIgnoreOuterWhitespace := False;
  FBOMChecked            := False;
  FPendingLF             := False;
  FScannedRowCount       := 0;
  FSkipFirstRows         := 0;
  FStopAfterRow          := 0;
  FOnProgress            := nil;
  FTotalRowsForProgress  := 0;
  FProgressUpdatePercent := 1;
  FLastProgressPercent   := -1;
  FStartTime             := 0;
  FProgressFiredOnce     := False;
  FInitialFreeRAM_MB     := 0;
  FMaxRowWidth           := 0;
  FCancelFlag            := nil;
  FAborted               := False;
  InitBuffer;
end;

constructor TCSVFastPacketReader.Create(ADataSet: TCustomBufDataset;
                                        AStream: TStream; AOptions: TCSVOptions);
begin
  Create(ADataSet, AStream);
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
  if FOwnsOptions and (FOptions <> nil) then FreeAndNil(FOptions);
  SetLength(FBuffer, 0);
  SetLength(FCells, 0);
  inherited Destroy;
end;

procedure TCSVFastPacketReader.InitBuffer;
begin
  if FBufferSize <= 0 then FBufferSize := DEFAULT_BUFFER_SIZE;
  SetLength(FBuffer, FBufferSize);
  FBufStart := Pointer(FBuffer);
  FBufPos   := FBufStart;
  FBufEnd   := FBufStart;
  SetLength(FCells, FCellCapacity);
end;

procedure TCSVFastPacketReader.EnsureCellCapacity;
begin
  if FCellCount >= FCellCapacity then
  begin
    if FCellCapacity < 16 then FCellCapacity := 16
    else FCellCapacity := FCellCapacity * 2;
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
  if C.Len <= 0 then Result := ''
  else if C.Len > High(Integer) then
    raise Exception.CreateFmt('CSV cell too large: %d bytes', [C.Len])
  else
  begin
    SetLength(Result, C.Len);
    Move(C.Start^, Result[1], C.Len);
  end;
end;

procedure TCSVFastPacketReader.SkipBOMIfNeeded;
var B0, B1, B2: Byte;
begin
  if FBOMChecked then Exit;
  FBOMChecked := True;
  if (FBufEnd - FBufStart) >= 3 then
  begin
    B0 := Byte(FBufStart[0]); B1 := Byte(FBufStart[1]); B2 := Byte(FBufStart[2]);
    if (B0 = UTF8_BOM_0) and (B1 = UTF8_BOM_1) and (B2 = UTF8_BOM_2) then
      FBufStart := FBufStart + 3;
  end;
  if FBufPos < FBufStart then FBufPos := FBufStart;
end;

procedure TCSVFastPacketReader.RefillBuffer;
var
  Remaining, BytesToRead, StreamRemaining: Int64;
  BytesRead: Integer;
begin
  Remaining := FBufEnd - FBufPos;
  if Remaining < 0 then Remaining := 0;
  if (Remaining > 0) and (FBufPos <> Pointer(FBuffer)) then
    Move(FBufPos^, FBuffer[0], Remaining);

  FBufStart := Pointer(FBuffer);
  FBufPos   := FBufStart;
  FBufEnd   := FBufStart + Remaining;

  StreamRemaining := Stream.Size - Stream.Position;
  if StreamRemaining < 0 then StreamRemaining := 0;

  if FBlockSize > 0 then BytesToRead := FBlockSize
  else BytesToRead := FBufferSize;

  if Remaining + BytesToRead > FBufferSize then
    BytesToRead := FBufferSize - Remaining;
  if BytesToRead > StreamRemaining then BytesToRead := StreamRemaining;

  if BytesToRead > 0 then
  begin
    if BytesToRead > High(Integer) then BytesToRead := High(Integer);
    BytesRead := Stream.Read(FBuffer[Remaining], BytesToRead);
    if BytesRead > 0 then
      FBufEnd := FBufStart + Remaining + BytesRead;
  end;
end;

procedure TCSVFastPacketReader.CheckCancellation;
begin
  if Assigned(FCancelFlag) and FCancelFlag^ then
  begin
    FAborted := True;
    FReaderState := frsAborted;
    raise EAbort.Create('Cancelled by user');
  end;
end;

procedure TCSVFastPacketReader.ResetProgress;
begin
  FStartTime           := Now;
  FLastProgressPercent := -1;
  FProgressFiredOnce   := False;
end;

function TCSVFastPacketReader.ComputeEstimatedUsageMB: Int64;
var Bytes: Int64;
begin
  if (FInitialFreeRAM_MB <= 0) or (FMaxRowWidth <= 0) then Exit(-1);
  Bytes := FScannedRowCount * (FMaxRowWidth + ROW_OVERHEAD_ESTIMATE);
  Result := Bytes div (1024 * 1024);
end;

procedure TCSVFastPacketReader.FireProgress(AForce: Boolean);
var
  NewPercent: Integer;
  FreeRAM, Estimated, BytesRead: Int64;
begin
  if not Assigned(FOnProgress) then Exit;

  if (not AForce) and (FTotalRowsForProgress > 0)
     and (FProgressUpdatePercent > 0) then
  begin
    NewPercent := (FScannedRowCount * 100) div FTotalRowsForProgress;
    if NewPercent > 100 then NewPercent := 100;
    if NewPercent < FLastProgressPercent + FProgressUpdatePercent then Exit;
    FLastProgressPercent := NewPercent;
  end
  else if (not AForce) and (FTotalRowsForProgress <= 0) then
    Exit;

  if FStartTime = 0 then FStartTime := Now;

  if FInitialFreeRAM_MB > 0 then FreeRAM := FInitialFreeRAM_MB
  else FreeRAM := -1;

  Estimated := ComputeEstimatedUsageMB;

  // Echte gelesene Bytes seit Load-Start.
  BytesRead := Stream.Position - FStreamStartPosition;
  if BytesRead < 0 then BytesRead := 0;

  FProgressFiredOnce := True;
  FOnProgress(FScannedRowCount,
              BytesRead,
              (Now - FStartTime) * SecsPerDay,
              FreeRAM,
              Estimated);
end;

procedure TCSVFastPacketReader.ScanRow;
var
  P, CellStart: PChar;
  C: Char;
  InQ: Boolean;
  QChar, Delim: Char;
begin
  FCellCount := 0;
  InQ    := False;
  QChar  := FQuoteChar;
  Delim  := FDelimiter;

  // Consume a pending LF from a previous CR at the buffer boundary.
  if FPendingLF then
  begin
    FPendingLF := False;
    if (FBufPos < FBufEnd) and (FBufPos^ = LF) then
      Inc(FBufPos);
  end;

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

    if C = QChar then begin InQ := True; Inc(P); Continue; end;

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

      FireProgress(False);
      CheckCancellation;   // raises EAbort if cancelled

      if C = CR then
      begin
        if (P + 1 < FBufEnd) and ((P + 1)^ = LF) then
        begin
          Inc(P, 2);
          FPendingLF := False;
        end
        else if P + 1 >= FBufEnd then
        begin
          Inc(P);
          FPendingLF := True;
        end
        else
        begin
          Inc(P);
          FPendingLF := False;
        end;
      end
      else
      begin
        Inc(P);
        FPendingLF := False;
      end;

      FBufPos  := P;
      Exit;
    end;

    Inc(P);
  end;

  // Buffer exhausted before row end.
  if (Stream.Position >= Stream.Size) and (P > FBufStart + FRowStartOffset) then
  begin
    AppendCell(CellStart, P - CellStart);
    Inc(FScannedRowCount);
    FBufPos := P;
    Exit;
  end;

  FBufPos    := FBufStart + FRowStartOffset;
  FCellCount := 0;
end;

function TCSVFastPacketReader.ScanRowWithRefill(out ARowComplete: Boolean): Boolean;
var BytesBefore: Int64;
begin
  Result := False;
  ARowComplete := False;

  repeat
    CheckCancellation;   // raises EAbort if cancelled

    if FBufPos >= FBufEnd then
    begin
      if Stream.Position >= Stream.Size then Exit;
      RefillBuffer;
      SkipBOMIfNeeded;
    end;

    ScanRow;

    if FCellCount > 0 then
    begin
      ARowComplete := True;
      Result := True;
      Exit;
    end;

    if Stream.Position >= Stream.Size then Exit;

    RefillBuffer;
    SkipBOMIfNeeded;

    // Die Zeile passt nur dann nicht in den Buffer, wenn der Buffer
    // bereits randvoll ist und nicht weiter wachsen kann.
    if (FBufEnd - FBufStart) >= FBufferSize then
      raise Exception.CreateFmt(
        'CSV row does not fit into buffer (%d bytes). ' +
        'Increase BufferSize or BlockSize.', [FBufferSize]);

  until False;
end;

function TCSVFastPacketReader.ComputeRowDisposition: Boolean;
begin
  Result := (FScannedRowCount > FSkipFirstRows);
  if Result and (FStopAfterRow > 0) then
    Result := (FScannedRowCount - FSkipFirstRows) <= FStopAfterRow;
end;

{ ---------------- TDataPacketReader overrides ---------------- }

procedure TCSVFastPacketReader.LoadFieldDefs(var AnAutoIncValue: Integer);
var
  I: Integer;
  FN: string;
  RowComplete: Boolean;
  SkipIdx: Int64;
begin
  AnAutoIncValue := 0;
  FReaderState := frsLoading;
  FBOMChecked  := False;
  FPendingLF   := False;
  FAborted     := False;
  ResetProgress;

  RefillBuffer;
  SkipBOMIfNeeded;

  if not ScanRowWithRefill(RowComplete) or (FCellCount = 0) then Exit;

  if FCreateFieldDefs <> nil then
  begin
    if FCreateFieldDefs.Count <> FCellCount then
      DatabaseErrorFmt(
        'CSV file field count (%d) does not match dataset field count (%d).',
        [FCellCount, FCreateFieldDefs.Count], DataSet.FieldDefs.Dataset);
    if FFirstLineAsFieldNames then
      for I := 0 to FCellCount - 1 do
        if CompareText(CellToStr(FCells[I]), FCreateFieldDefs[I].Name) <> 0 then
          DatabaseErrorFmt(
            'CSV file field %d: name "%s" does not match dataset field name "%s".',
            [I, CellToStr(FCells[I]), FCreateFieldDefs[I].Name],
            DataSet.FieldDefs.Dataset);
    DataSet.FieldDefs.Assign(FCreateFieldDefs);
  end
  else if FCellCount > 0 then
  begin
    for I := 0 to FCellCount - 1 do
    begin
      if FFirstLineAsFieldNames then FN := CellToStr(FCells[I])
      else FN := Format('Column%d', [I + 1]);
      DataSet.FieldDefs.Add(FN, ftString, FDefaultFieldLength);
    end;
  end;

  if FFirstLineAsFieldNames then
  begin
    FScannedRowCount := 0;
    for SkipIdx := 0 to FSkipFirstRows do
    begin
      FCellCount := 0;
      if not ScanRowWithRefill(RowComplete) then Break;
      if FCellCount = 0 then Break;
    end;
  end;
end;

procedure TCSVFastPacketReader.StoreFieldDefs(AnAutoIncValue: Integer);
begin
  raise ENotSupportedException.Create(
    'TCSVFastPacketReader is read-only. Writing CSV files is not ' +
    'implemented in this version.');
end;

procedure TCSVFastPacketReader.InitLoadRecords;
begin
  // buffer is primed in LoadFieldDefs
end;

function TCSVFastPacketReader.GetCurrentRecord: Boolean;
begin
  Result := (not FAborted)
        and (FReaderState <> frsFinished)
        and (FCellCount > 0);
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
begin
  if not ComputeRowDisposition then Exit;

  for I := 0 to FCellCount - 1 do
  begin
    if I >= DataSet.FieldCount then Break;
    F := DataSet.Fields[I];
    C := FCells[I];

    if C.Len = 0 then begin F.Clear; Continue; end;

    case F.DataType of
      ftString, ftFixedChar, ftWideString, ftFixedWideChar:
        F.AsString := CellToStr(C);
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
var RowComplete: Boolean;
begin
  if FReaderState = frsFinished then Exit;

  CheckCancellation;   // raises EAbort if cancelled

  if (FStopAfterRow > 0) and
     (FScannedRowCount - FSkipFirstRows >= FStopAfterRow) then
  begin
    FReaderState := frsFinished;
    FCellCount   := 0;
    if not FProgressFiredOnce then FireProgress(True);
    Exit;
  end;

  if not ScanRowWithRefill(RowComplete) or (FCellCount = 0) then
  begin
    FReaderState := frsFinished;
    FCellCount   := 0;
    if not FProgressFiredOnce then FireProgress(True);
    Exit;
  end;
end;

procedure TCSVFastPacketReader.StoreRecord(ARowState: TRowState;
                                           AUpdOrder: Integer);
begin
  raise ENotSupportedException.Create(
    'TCSVFastPacketReader is read-only. StoreRecord is not implemented.');
end;

procedure TCSVFastPacketReader.FinalizeStoreRecords;
begin
  raise ENotSupportedException.Create(
    'TCSVFastPacketReader is read-only. FinalizeStoreRecords is not implemented.');
end;

class function TCSVFastPacketReader.RecognizeStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

procedure TCSVFastPacketReader.SetBufferSize(AValue: Int64);
begin
  if FBufferSize = AValue then Exit;
  if AValue <= 0 then
    raise EArgumentException.CreateFmt('BufferSize must be > 0, got %d.', [AValue]);
  if FReaderState <> frsIdle then
    raise Exception.Create('BufferSize cannot be changed while loading is in progress.');
  if (FBufPos <> FBufStart) or (FCellCount > 0) then
    raise Exception.Create('BufferSize cannot be changed while data is in the buffer.');
  FBufferSize := AValue;
  InitBuffer;
end;

procedure TCSVFastPacketReader.SetInitialCellCapacity(AValue: Integer);
begin
  if FInitialCellCapacity = AValue then Exit;
  if AValue <= 0 then
    raise EArgumentException.CreateFmt('InitialCellCapacity must be > 0, got %d.', [AValue]);
  if FReaderState <> frsIdle then
    raise Exception.Create('InitialCellCapacity cannot be changed while loading is in progress.');
  FInitialCellCapacity := AValue;
  FCellCapacity        := AValue;
  if FCellCapacity < 16 then FCellCapacity := 16;
  SetLength(FCells, FCellCapacity);
end;

{ ==================================================================
  TCSVFastDataset
  ================================================================== }

constructor TCSVFastDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlockSizeBytes        := DEFAULT_BLOCK_SIZE;
  FBufferSizeBytes       := DEFAULT_BUFFER_SIZE;
  FInitialCellCapacity   := DEFAULT_CELL_CAPACITY;
  FProgressUpdatePercent := 1;
  FSkipFirstRows         := 0;
  FStopAfterRow          := 0;
  FOnProgress            := nil;
  FTotalRowsForProgress  := 0;
  FInitialFreeRAM_MB     := 0;
  FMaxRowWidth           := 0;
  FCancelFlag            := nil;
  FAborted               := False;
end;

class function TCSVFastDataset.DefaultPacketClass: TDataPacketReaderClass;
begin
  Result := TCSVFastPacketReader;
end;

procedure TCSVFastDataset.ApplyConfigToReader(R: TCSVFastPacketReader);
begin
  if R = nil then Exit;
  R.BlockSize             := FBlockSizeBytes;
  R.BufferSize            := FBufferSizeBytes;
  R.InitialCellCapacity   := FInitialCellCapacity;
  R.SkipFirstRows         := FSkipFirstRows;
  R.StopAfterRow          := FStopAfterRow;
  R.OnProgress            := FOnProgress;
  R.TotalRowsForProgress  := FTotalRowsForProgress;
  R.ProgressUpdatePercent := FProgressUpdatePercent;
  R.InitialFreeRAM_MB     := FInitialFreeRAM_MB;
  R.MaxRowWidth           := FMaxRowWidth;
  R.CancelFlag            := FCancelFlag;
end;

function TCSVFastDataset.CreateDefaultPacketReader(
  aStream: TStream): TDataPacketReader;
var R: TCSVFastPacketReader;
begin
  R := TCSVFastPacketReader.Create(Self, aStream, CSVOptions);
  ApplyConfigToReader(R);
  Result := R;
end;

procedure TCSVFastDataset.LoadFromCSVStream(AStream: TStream);
var P: TCSVFastPacketReader;
begin
  CheckInactive;
  FAborted := False;

  P := TCSVFastPacketReader.Create(Self, AStream, CSVOptions);
  try
  ApplyConfigToReader(P);
  if FieldDefs.Count > 0 then P.CreateFieldDefs := FieldDefs;
  SetDatasetPacket(P);
  finally
    // Capture the reader's abort state BEFORE destroying it, so
    // TCSVFastDataset.Aborted stays meaningful after EAbort has
    // propagated out of SetDatasetPacket.
  if P.Aborted then FAborted := True;
  P.Free;
end;
end;

procedure TCSVFastDataset.LoadFromCSVFile(const AFileName: string);
var F: TFileStream;
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
  // Write support: not implemented yet. Falls back to the slow
  // inherited implementation. When TCSVFastPacketWriter is added,
  // replace this with a fast path.
  inherited SaveToCSVFile(AFileName);
end;

procedure Register;
begin
  RegisterComponents('Data Access', [TCSVFastDataset]);
end;

end.
