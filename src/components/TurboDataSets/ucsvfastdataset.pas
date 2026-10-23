{
  uCSVFastDataset.pas

  High-performance CSV dataset for Lazarus / Free Pascal.
  Drop-in replacement for TCSVDataset with a PChar block scanner
  for reading and a PChar block writer for writing.

  ----------------------------------------------------------------------
  ARCHITECTURE
  ----------------------------------------------------------------------

  TCSVFastDataset (public)
    Drop-in replacement. Handles file opening, stream management,
    API compatibility.

  TCSVFastPacketReader (internal)
    The actual parser. Inherits from TDataPacketReader (FPC's
    buffered-read interface). Read-only.

  TCSVFastPacketWriter (internal)
    The actual formatter. Inherits from TObject (no FPC writer
    interface exists). Used by TCSVFastDataset.SaveToCSVFile.

  TCSVFastCellRef (internal)
    Cell reference: pointer into the buffer + length.

  ----------------------------------------------------------------------
  NAMING CONVENTIONS
  ----------------------------------------------------------------------

  Reader public properties : BlockSize, BufferSize, SkipFirstRows,
                             StopAfterRow, ProgressUpdatePercent
  Reader public event      : OnProgress
  Reader cancel            : CancelFlag, Aborted

  Writer public properties : SaveBufferSize, SaveSkipFirstRows,
                             SaveStopAfterRow, SaveProgressUpdatePercent,
                             SaveWriteBOM
  Writer public event      : OnSaveProgress
  Writer cancel            : SaveCancelFlag, SaveAborted

  All reader/writer internal methods are prefixed with a verb
  (Init, Flush, Write, Emit, Fire, Check, Compute, Scan, Refill,
  Append, Ensure, Set) so they never collide with properties.

  ----------------------------------------------------------------------
  RESPONSIBILITY: NO RAM QUERIES
  ----------------------------------------------------------------------

  This unit does NOT query the operating system for free RAM and
  does NOT read /proc/meminfo, GlobalMemoryStatusEx or sysctl.
  Memory accounting is the caller's job (e.g. the data editor GUI).

  ----------------------------------------------------------------------
  CANCELLATION
  ----------------------------------------------------------------------

  Both reader and writer raise EAbort when CancelFlag^ becomes True.
  The dataset exposes Aborted (reader) and SaveAborted (writer).

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
  { Reader progress event.

    ARowsRead       - Data rows read so far (header excluded).
    ABytesRead      - Bytes actually read from the stream so far.
    AElapsedSeconds - Elapsed seconds since load started. }
  TCSVLoadProgressEvent = procedure(
    ARowsRead: Int64;
    ABytesRead: Int64;
    AElapsedSeconds: Double) of object;

  { Writer progress event.

    ARowsWritten    - Data rows written so far (header excluded).
    ABytesWritten   - Bytes actually written to the stream so far.
    AElapsedSeconds - Elapsed seconds since save started. }
  TCSVSaveProgressEvent = procedure(
    ARowsWritten: Int64;
    ABytesWritten: Int64;
    AElapsedSeconds: Double) of object;

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
    TCSVFastPacketReader — reads CSV data block-wise via PChar scanning.
    ------------------------------------------------------------------ }
  TCSVFastPacketReader = class(TDataPacketReader)
  private
    FOptions: TCSVOptions;
    FOwnsOptions: Boolean;

    FStreamStartPosition: Int64;

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

    FCancelFlag: PBoolean;
    FAborted: Boolean;

    procedure InitBuffer;
    procedure RefillBuffer;
    procedure SkipBOMIfNeeded;
    procedure ScanRow;
    function  ScanRowWithRefill(out ARowComplete: Boolean): Boolean;
    procedure AppendCell(ACellStart: PChar; ACellLen: Int64);
    procedure EnsureCellCapacity;
    function  CellToStr(const ACell: TCSVFastCellRef): string;
    procedure SetBufferSize(AValue: Int64);
    procedure SetInitialCellCapacity(AValue: Integer);
    procedure CheckCancellation;
    procedure FireProgress(AForce: Boolean);
    procedure ResetProgress;
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

    property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;
    property Aborted: Boolean read FAborted;
  end;

  { ------------------------------------------------------------------
    TCSVFastPacketWriter — writes CSV data block-wise.
    Inherits from TObject because FPC has no TDataPacketWriter.
    ------------------------------------------------------------------ }
  TCSVFastPacketWriter = class(TObject)
  private
    FDataSet: TDataSet;
    FStream: TStream;
    FOptions: TCSVOptions;

    FBuffer: TBytes;
    FBufferSize: Integer;
    FBufPos: Integer;

    FSkipFirstRows: Int64;
    FStopAfterRow: Int64;

    FOnProgress: TCSVSaveProgressEvent;
    FProgressUpdatePercent: Integer;
    FTotalRows: Int64;
    FLastProgressPercent: Integer;
    FStartTime: TDateTime;
    FRowsWritten: Int64;
    FBytesWritten: Int64;
    FProgressFiredOnce: Boolean;

    FCancelFlag: PBoolean;
    FAborted: Boolean;

    FWriteBOM: Boolean;

    procedure InitBuffer;
    procedure FlushBuffer;
    procedure WriteByteRaw(AByte: Byte);
    procedure WriteStringRaw(const AStr: AnsiString);
    procedure WriteLineEnding;
    procedure EmitBOMBytes;                     // BOM-Bytes schreiben
    procedure WriteHeaderRow;
    procedure WriteDataRow;
    procedure WriteFieldValue(const AValue: string);
    function  NeedsQuoting(const AValue: string): Boolean;
    procedure WriteQuotedField(const AValue: string);
    procedure FireProgress(AForce: Boolean);
    procedure CheckCancellation;
  public
    constructor Create(ADataSet: TDataSet; AStream: TStream;
                       AOptions: TCSVOptions);
    destructor  Destroy; override;

    procedure SaveToStream;

    property SkipFirstRows: Int64 read FSkipFirstRows write FSkipFirstRows;
    property StopAfterRow: Int64 read FStopAfterRow write FStopAfterRow;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
    property ProgressUpdatePercent: Integer
      read FProgressUpdatePercent write FProgressUpdatePercent;
    property OnProgress: TCSVSaveProgressEvent
      read FOnProgress write FOnProgress;
    property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;
    property Aborted: Boolean read FAborted;
    property RowsWritten: Int64 read FRowsWritten;
    property BytesWritten: Int64 read FBytesWritten;
  end;

  { ------------------------------------------------------------------
    TCSVFastDataset — drop-in replacement for TCSVDataset.
    ------------------------------------------------------------------ }
  TCSVFastDataset = class(TCSVDataset)
  private
    // Reader
    FBlockSizeBytes: Integer;
    FBufferSizeBytes: Int64;
    FInitialCellCapacity: Integer;
    FSkipFirstRows: Int64;
    FStopAfterRow: Int64;
    FOnProgress: TCSVLoadProgressEvent;
    FTotalRowsForProgress: Int64;
    FProgressUpdatePercent: Integer;
    FCancelFlag: PBoolean;
    FAborted: Boolean;

    // Writer
    FSaveSkipFirstRows: Int64;
    FSaveStopAfterRow: Int64;
    FSaveProgressUpdatePercent: Integer;
    FSaveBufferSize: Integer;
    FSaveWriteBOM: Boolean;
    FOnSaveProgress: TCSVSaveProgressEvent;
    FSaveCancelFlag: PBoolean;
    FSaveAborted: Boolean;

  protected
    class function DefaultPacketClass: TDataPacketReaderClass; override;
    function  CreateDefaultPacketReader(aStream: TStream): TDataPacketReader; override;
    procedure ApplyConfigToReader(R: TCSVFastPacketReader);
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadFromCSVFile(const AFileName: string); reintroduce;
    procedure LoadFromCSVStream(AStream: TStream); reintroduce;
    procedure SaveToCSVFile(AFileName: string = ''); reintroduce;

    // --- Reader runtime ---
    property SkipFirstRows: Int64 read FSkipFirstRows write FSkipFirstRows;
    property StopAfterRow: Int64 read FStopAfterRow write FStopAfterRow;
    property TotalRowsForProgress: Int64
      read FTotalRowsForProgress write FTotalRowsForProgress;
    property CancelFlag: PBoolean read FCancelFlag write FCancelFlag;
    property Aborted: Boolean read FAborted;

    // --- Writer runtime ---
    property SaveSkipFirstRows: Int64
      read FSaveSkipFirstRows write FSaveSkipFirstRows;
    property SaveStopAfterRow: Int64
      read FSaveStopAfterRow write FSaveStopAfterRow;
    property SaveCancelFlag: PBoolean
      read FSaveCancelFlag write FSaveCancelFlag;
    property SaveAborted: Boolean read FSaveAborted;

  published
    // --- Reader ---
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

    // --- Writer ---
    property SaveBufferSize: Integer
      read FSaveBufferSize write FSaveBufferSize default 4194304;
    property SaveProgressUpdatePercent: Integer
      read FSaveProgressUpdatePercent write FSaveProgressUpdatePercent default 1;
    property SaveWriteBOM: Boolean
      read FSaveWriteBOM write FSaveWriteBOM default False;
    property OnSaveProgress: TCSVSaveProgressEvent
      read FOnSaveProgress write FOnSaveProgress;
  end;

{ Inline PChar helpers (reader) }
function PCharToInt64Def(P: PChar; Len: Int64; Def: Int64): Int64; inline;
function PCharToDoubleDef(P: PChar; Len: Int64; Def: Double): Double; inline;
function PCharEqualsIgnoreCase(P: PChar; Len: Int64; const S: string): Boolean; inline;

procedure Register;

implementation

const
  // Format definitions
  CR = #13;
  LF = #10;
  TAB = #9;
  SPACE = #32;
  UTF8_BOM_0 = $EF;
  UTF8_BOM_1 = $BB;
  UTF8_BOM_2 = $BF;

  // Reader defaults
  DEFAULT_BLOCK_SIZE    = 1048576;   // 1 MB
  DEFAULT_BUFFER_SIZE   = 2097152;   // 2 MB
  DEFAULT_CELL_CAPACITY = 64;
  DEFAULT_FIELD_LENGTH  = 50;

  // Writer defaults
  DEFAULT_SAVE_BUFFER_SIZE = 4194304;   // 4 MB
  MIN_SAVE_BUFFER_SIZE     = 65536;     // 64 KB

{ ==================================================================
  PChar helpers (reader)
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

constructor TCSVFastPacketReader.Create(ADataSet: TCustomBufDataset;
  AStream: TStream);
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

procedure TCSVFastPacketReader.AppendCell(ACellStart: PChar; ACellLen: Int64);
begin
  EnsureCellCapacity;
  FCells[FCellCount].Start := ACellStart;
  FCells[FCellCount].Len   := ACellLen;
  Inc(FCellCount);
end;

function TCSVFastPacketReader.CellToStr(const ACell: TCSVFastCellRef): string;
begin
  if ACell.Len <= 0 then Result := ''
  else if ACell.Len > High(Integer) then
    raise Exception.CreateFmt('CSV cell too large: %d bytes', [ACell.Len])
  else
  begin
    SetLength(Result, ACell.Len);
    Move(ACell.Start^, Result[1], ACell.Len);
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

procedure TCSVFastPacketReader.FireProgress(AForce: Boolean);
var
  NewPercent: Integer;
  BytesRead: Int64;
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

  BytesRead := Stream.Position - FStreamStartPosition;
  if BytesRead < 0 then BytesRead := 0;

  FProgressFiredOnce := True;
  FOnProgress(FScannedRowCount,
              BytesRead,
              (Now - FStartTime) * SecsPerDay);
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

  // Pending LF from previous row's CR at buffer boundary
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
      CheckCancellation;

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

      FBufPos := P;
      Exit;
    end;

    Inc(P);
  end;

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
begin
  Result := False;
  ARowComplete := False;

  repeat
    CheckCancellation;

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

  CheckCancellation;

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
  TCSVFastPacketWriter
  ================================================================== }

constructor TCSVFastPacketWriter.Create(ADataSet: TDataSet; AStream: TStream;
  AOptions: TCSVOptions);
begin
  inherited Create;

  if ADataSet = nil then
    raise EArgumentException.Create('ADataSet is nil');
  if AStream = nil then
    raise EArgumentException.Create('AStream is nil');
  if AOptions = nil then
    raise EArgumentException.Create('AOptions is nil');

  FDataSet := ADataSet;
  FStream  := AStream;
  FOptions := AOptions;

  FBufferSize            := DEFAULT_SAVE_BUFFER_SIZE;
  FBufPos                := 0;
  FSkipFirstRows         := 0;
  FStopAfterRow          := 0;
  FOnProgress            := nil;
  FProgressUpdatePercent := 1;
  FTotalRows             := 0;
  FLastProgressPercent   := -1;
  FStartTime             := 0;
  FRowsWritten           := 0;
  FBytesWritten          := 0;
  FProgressFiredOnce     := False;
  FCancelFlag            := nil;
  FAborted               := False;
  FWriteBOM              := False;
end;

destructor TCSVFastPacketWriter.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TCSVFastPacketWriter.InitBuffer;
begin
  if FBufferSize <= 0 then
    FBufferSize := DEFAULT_SAVE_BUFFER_SIZE;
  if FBufferSize < MIN_SAVE_BUFFER_SIZE then
    FBufferSize := MIN_SAVE_BUFFER_SIZE;

  SetLength(FBuffer, FBufferSize);
  FBufPos := 0;
end;

procedure TCSVFastPacketWriter.FlushBuffer;
begin
  if FBufPos > 0 then
  begin
    FStream.Write(FBuffer[0], FBufPos);
    FBufPos := 0;
  end;
end;

procedure TCSVFastPacketWriter.WriteByteRaw(AByte: Byte);
begin
  if FBufPos >= FBufferSize then
    FlushBuffer;
  FBuffer[FBufPos] := AByte;
  Inc(FBufPos);
  Inc(FBytesWritten);
end;

procedure TCSVFastPacketWriter.WriteStringRaw(const AStr: AnsiString);
var
  Len, Avail: Integer;
  P: PAnsiChar;
begin
  Len := Length(AStr);
  if Len = 0 then Exit;

  P := PAnsiChar(AStr);
  while Len > 0 do
  begin
    if FBufPos >= FBufferSize then
      FlushBuffer;
    Avail := FBufferSize - FBufPos;
    if Avail > Len then Avail := Len;
    Move(P^, FBuffer[FBufPos], Avail);
    Inc(FBufPos, Avail);
    Inc(FBytesWritten, Avail);
    Inc(P, Avail);
    Dec(Len, Avail);
  end;
end;

procedure TCSVFastPacketWriter.WriteLineEnding;
var
  LE: string;
begin
  LE := FOptions.LineEnding;
  if LE = '' then
    LE := sLineBreak;
  WriteStringRaw(LE);
end;

procedure TCSVFastPacketWriter.EmitBOMBytes;
begin
  WriteByteRaw(UTF8_BOM_0);
  WriteByteRaw(UTF8_BOM_1);
  WriteByteRaw(UTF8_BOM_2);
end;

procedure TCSVFastPacketWriter.WriteHeaderRow;
var
  I: Integer;
  F: TField;
begin
  for I := 0 to FDataSet.FieldCount - 1 do
  begin
    if I > 0 then
      WriteByteRaw(Ord(FOptions.Delimiter));

    F := FDataSet.Fields[I];
    if Assigned(F) then
      WriteFieldValue(F.FieldName);
  end;
  WriteLineEnding;
end;

procedure TCSVFastPacketWriter.WriteDataRow;
var
  I: Integer;
  F: TField;
begin
  for I := 0 to FDataSet.FieldCount - 1 do
  begin
    if I > 0 then
      WriteByteRaw(Ord(FOptions.Delimiter));

    F := FDataSet.Fields[I];
    if not Assigned(F) then Continue;
    if F.IsNull then Continue;

    WriteFieldValue(F.AsString);
  end;
  WriteLineEnding;
end;

function TCSVFastPacketWriter.NeedsQuoting(const AValue: string): Boolean;
var
  I: Integer;
  C: Char;
  FirstCh, LastCh: Char;
begin
  Result := False;
  if AValue = '' then Exit;

  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    if (C = FOptions.Delimiter) or
       (C = FOptions.QuoteChar) or
       (C = CR) or (C = LF) then
      Exit(True);
  end;

  if FOptions.QuoteOuterWhitespace then
  begin
    FirstCh := AValue[1];
    LastCh  := AValue[Length(AValue)];
    if (FirstCh in [SPACE, TAB]) or (LastCh in [SPACE, TAB]) then
      Exit(True);
  end;
end;

procedure TCSVFastPacketWriter.WriteQuotedField(const AValue: string);
var
  I: Integer;
  C: Char;
begin
  if FOptions.QuoteChar = #0 then
    raise EConvertError.CreateFmt(
      'Field needs quoting but QuoteChar is #0. ' +
      'Content starts with: "%s"', [Copy(AValue, 1, 80)]);

  WriteByteRaw(Ord(FOptions.QuoteChar));
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    if C = FOptions.QuoteChar then
      WriteByteRaw(Ord(C));   // doubled
    WriteByteRaw(Ord(C));
  end;
  WriteByteRaw(Ord(FOptions.QuoteChar));
end;

procedure TCSVFastPacketWriter.WriteFieldValue(const AValue: string);
begin
  if AValue = '' then Exit;
  if NeedsQuoting(AValue) then
    WriteQuotedField(AValue)
  else
    WriteStringRaw(AValue);
end;

procedure TCSVFastPacketWriter.FireProgress(AForce: Boolean);
var
  NewPercent: Integer;
begin
  if not Assigned(FOnProgress) then Exit;

  if (not AForce) and (FTotalRows > 0) and (FProgressUpdatePercent > 0) then
  begin
    NewPercent := (FRowsWritten * 100) div FTotalRows;
    if NewPercent > 100 then NewPercent := 100;
    if NewPercent < FLastProgressPercent + FProgressUpdatePercent then Exit;
    FLastProgressPercent := NewPercent;
  end
  else if (not AForce) and (FTotalRows <= 0) then
    Exit;

  if FStartTime = 0 then FStartTime := Now;

  FProgressFiredOnce := True;
  FOnProgress(FRowsWritten,
              FBytesWritten,
              (Now - FStartTime) * SecsPerDay);
end;

procedure TCSVFastPacketWriter.CheckCancellation;
begin
  if Assigned(FCancelFlag) and FCancelFlag^ then
  begin
    FAborted := True;
    FlushBuffer;
    raise EAbort.Create('Cancelled by user');
  end;
end;

procedure TCSVFastPacketWriter.SaveToStream;
var
  SkipCount: Int64;
begin
  FStartTime           := Now;
  FAborted             := False;
  FRowsWritten         := 0;
  FBytesWritten        := 0;
  FLastProgressPercent := -1;
  FProgressFiredOnce   := False;

  InitBuffer;

  // ---- BOM ----
  if FWriteBOM then
    EmitBOMBytes;

  // ---- Header ----
  if FOptions.FirstLineAsFieldNames then
    WriteHeaderRow;

  // ---- Total Rows ----
  if FStopAfterRow > 0 then
    FTotalRows := FStopAfterRow
  else
  begin
    FTotalRows := FDataSet.RecordCount;
    if FTotalRows < 0 then
      FTotalRows := 0;
  end;

  FDataSet.DisableControls;
  try
    FDataSet.First;

    // ---- Skip ----
    SkipCount := 0;
    while (SkipCount < FSkipFirstRows) and (not FDataSet.EOF) do
    begin
      CheckCancellation;
      FDataSet.Next;
      Inc(SkipCount);
    end;

    // ---- Write Loop ----
    while not FDataSet.EOF do
    begin
      CheckCancellation;

      if (FStopAfterRow > 0) and (FRowsWritten >= FStopAfterRow) then
        Break;

      WriteDataRow;
      Inc(FRowsWritten);

      FireProgress(False);

      FDataSet.Next;
    end;
  finally
    FDataSet.EnableControls;
  end;

  FireProgress(True);
  FlushBuffer;
end;

{ ==================================================================
  TCSVFastDataset
  ================================================================== }

constructor TCSVFastDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Reader defaults (match published 'default' clauses)
  FBlockSizeBytes        := DEFAULT_BLOCK_SIZE;
  FBufferSizeBytes       := DEFAULT_BUFFER_SIZE;
  FInitialCellCapacity   := DEFAULT_CELL_CAPACITY;
  FProgressUpdatePercent := 1;

  FSkipFirstRows         := 0;
  FStopAfterRow          := 0;
  FOnProgress            := nil;
  FTotalRowsForProgress  := 0;
  FCancelFlag            := nil;
  FAborted               := False;

  // Writer defaults
  FSaveSkipFirstRows         := 0;
  FSaveStopAfterRow          := 0;
  FSaveProgressUpdatePercent := 1;
  FSaveBufferSize            := DEFAULT_SAVE_BUFFER_SIZE;
  FSaveWriteBOM              := False;
  FOnSaveProgress            := nil;
  FSaveCancelFlag            := nil;
  FSaveAborted               := False;
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
  R.CancelFlag            := FCancelFlag;
end;

function TCSVFastDataset.CreateDefaultPacketReader(
  aStream: TStream): TDataPacketReader;
var
  R: TCSVFastPacketReader;
begin
  R := TCSVFastPacketReader.Create(Self, aStream, CSVOptions);
  ApplyConfigToReader(R);
  Result := R;
end;

procedure TCSVFastDataset.LoadFromCSVStream(AStream: TStream);
var
  P: TCSVFastPacketReader;
begin
  CheckInactive;
  FAborted := False;

  P := TCSVFastPacketReader.Create(Self, AStream, CSVOptions);
  try
    ApplyConfigToReader(P);
    if FieldDefs.Count > 0 then
      P.CreateFieldDefs := FieldDefs;
    SetDatasetPacket(P);
  finally
    if P.Aborted then FAborted := True;
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
var
  W: TCSVFastPacketWriter;
  Stream: TStream;
begin
  if AFileName = '' then
    raise EArgumentException.Create('AFileName is empty');
  if not Active then
    raise Exception.Create('Cannot save: dataset is not active');

  FSaveAborted := False;

  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    W := TCSVFastPacketWriter.Create(Self, Stream, CSVOptions);
    try
      W.SkipFirstRows         := FSaveSkipFirstRows;
      W.StopAfterRow          := FSaveStopAfterRow;
      W.BufferSize            := FSaveBufferSize;
      W.WriteBOM              := FSaveWriteBOM;
      W.ProgressUpdatePercent := FSaveProgressUpdatePercent;
      W.OnProgress            := FOnSaveProgress;
      W.CancelFlag            := FSaveCancelFlag;

      W.SaveToStream;

      FSaveAborted := W.Aborted;
    finally
      W.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('Data Access', [TCSVFastDataset]);
end;

end.
