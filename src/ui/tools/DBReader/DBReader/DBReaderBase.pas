unit DBReaderBase;

(*
Database file reader base classes and defines

Author: Sergey Bodrov, 2024 Minsk
License: MIT

*)

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
  SysUtils, Classes, Variants, DB;

type
  //TDbReader = class;
  TDbRowsList = class;

  TDbFieldDefRec = record
    Name: string;
    TypeName: string;        // field type name
    FieldType: TFieldType;
    Size: Integer;
    RawOffset: Cardinal;     // raw data offset (0..Length(RawData)-1)
  end;

  TDbRowItem = class(TObject)
  protected
    FOwner: TDbRowsList;
  public
    Values: array of Variant;
    RawData: AnsiString;
    RawOffs: array of Integer;
    constructor Create(AOwner: TDbRowsList);
    function GetFieldAsStr(AFieldIndex: Integer): string; virtual;
    procedure Assign(ASourceItem: TDbRowItem); virtual;
    property Owner: TDbRowsList read FOwner;
  end;

  TDbRowsList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    // set FieldsInfo[AIndex] properties for system tables
    //procedure SetFieldInfo(var AIndex: Integer; AName: string; AType, ASize: Integer);
  public
    FieldsDef: array of TDbFieldDefRec;
    TableName: string;
    function GetItem(AIndex: Integer): TDbRowItem;
    // contain no rows
    function IsEmpty(): Boolean; virtual;
    // predefined table
    function IsSystem(): Boolean; virtual;
    // not defined in metadata
    function IsGhost(): Boolean; virtual;
  end;

  { DB reader base class }

  TDBReader = class(TComponent)
  protected
    FFile: TStream;
    FIsSingleTable: Boolean;
    FOnLog: TGetStrProc;
    FOnPageReaded: TNotifyEvent;

  public
    IsDebugPages: Boolean;     // debug messages for pages
    IsDebugRows: Boolean;      // debug messages for rows
    IsUseRowRawData: Boolean;  // fill TDbRowItem.RawData
    FileName: string;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    procedure LogInfo(AStr: string); virtual;
    function OpenFile(AFileName: string; AStream: TStream = nil): Boolean; virtual;
    // Read table data from DB to AList
    // AName - table name
    // ACount - how many items read
    procedure ReadTable(AName: string; ACount: Int64 = MaxInt; AList: TDbRowsList = nil); virtual; abstract;
    // get detailed multi-line description of table
    function FillTableInfoText(ATableName: string; ALines: TStrings): Boolean; virtual;
    // get progress value 0..1000
    function GetProgress(): Integer; virtual;

    // get tables count
    function GetTablesCount(): Integer; virtual;
    // get table by index 0..GetTablesCount()-1
    function GetTableByIndex(AIndex: Integer): TDbRowsList; virtual;

    // detabase file contain single table
    property IsSingleTable: Boolean read FIsSingleTable;
    // messages from reader
    property OnLog: TGetStrProc read FOnLog write FOnLog;
    // after portion of data readed, for progress update
    property OnPageReaded: TNotifyEvent read FOnPageReaded write FOnPageReaded;
  end;

  TDBReaderClass = class of TDBReader;

  TDbReaderDataSet = class(TDataSet)
  private
    FIsOpen: Boolean;
    FCursor: Integer;
    FRowsList: TDbRowsList;
  protected
    procedure InternalHandleException; override;
    // Cursor init/close
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    procedure InternalClose; override;
    // Record init/fill/close
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean):
      TGetResult; override;
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    // Internal Navigation
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    // Records count
    function GetRecordCount: Integer; override;
    // External navigation (1-based)
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;
    // Access
    function GetCanModify: Boolean; override;
  public
    // read field data
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    procedure AssignRowsList(ARowsList: TDbRowsList);
  end;

  { TRawDataReader }

  TRawDataReader = record
    Data: AnsiString;
    nPos: Integer;
    StartPtr: PByte;
    DataPtr: PByte;
    IsBigEndian: Boolean; // Most significant byte first, 00 01 = 1

    procedure InitBuf(AData: AnsiString; AStartPos: Integer = 0; AIsBigEndian: Boolean = False); overload;
    procedure Init(const AData; AIsBigEndian: Boolean = False); overload;
    procedure SetPosition(AOffset: Integer); // 0-based from initial position
    function GetPosition(): Integer; // 0-based from initial position
    function ReadUInt8: Byte;
    function ReadUInt16: Word;
    function ReadUInt32: Cardinal;
    function ReadUInt64: UInt64;
    function ReadInt8: ShortInt;
    function ReadInt16: SmallInt;
    function ReadInt24: Integer; // 3-byte Integer
    function ReadInt32: Integer;
    function ReadInt48: Int64;   // 6-byte Integer
    function ReadInt64: Int64;
    function ReadCurrency: Currency;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadBytes(ASize: Integer): AnsiString;
    // zero-terminated string
    // do not read more than AMaxSize
    // if AFixed = True, set position up to AMaxSize anyways
    function ReadCString(AMaxSize: Integer; AFixed: Boolean = False): AnsiString;
    procedure ReadToBuffer(var ABuf; ASize: Integer);
  end;

  TInnerFileStream = class(TFileStream)
  public
    InnerFilePos: Int64;
    InnerFileSize: Int64;
    InnerFileName: string;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    constructor Create(AFileName, AInnerFileName: string; APos, ASize: Int64);
  end;

procedure BufToFile(const ABuf; ABufSize: Integer; AFileName: string);
procedure StrToFile(AStr: AnsiString; AFileName: string);
procedure StrToStream(AStr: AnsiString; AStream: TStream; AAddLineEnd: Boolean = True);
// Returns Buffer content in HEX, capital letters, without spaces
// Example: 010ABC
function BufToHex(const Buffer; BufferSize: Integer): string;
// Example: [01 0A BC]
function BufferToHex(const Buffer; BufferSize: Integer): string;
function VarToInt(const AValue: Variant): Integer;
function VarToInt64(const AValue: Variant): Int64;
// Raw data as printable text, non-printable chars replaced by dots
function DataAsStr(const AData; ALen: Integer): string;
// 2-byte data to string
function WideDataToStr(AData: AnsiString): string;
// reverse bytes order
procedure ReverseBytes(const AData; ASize: Integer);
// AQuotes = '""', '[]'
function RemoveQuotes(AStr: string; AQuotes: string): string;

implementation

type
  TRecordBuffer = record
    RecordNum: Integer; // 1-based
  end;
  PRecordBuffer = ^TRecordBuffer;

procedure BufToFile(const ABuf; ABufSize: Integer; AFileName: string);
var
  fs: TFileStream;
begin
  if ABufSize = 0 then Exit;
  if FileExists(AFileName) then
    DeleteFile(AFileName);

  fs := TFileStream.Create(AFileName, fmCreate);
  try
    fs.Write(ABuf, ABufSize);
  finally
    fs.Free();
  end;
end;

procedure StrToFile(AStr: AnsiString; AFileName: string);
begin
  if AStr = '' then Exit;
  BufToFile(AStr[1], Length(AStr), AFileName);
end;

procedure StrToStream(AStr: AnsiString; AStream: TStream; AAddLineEnd: Boolean);
begin
  if AAddLineEnd then
    AStr := AStr + sLineBreak;
  if AStr = '' then Exit;
  AStream.Write(AStr[1], Length(AStr));
end;

// Returns Buffer content in HEX, capital letters, without spaces
// Example: 010ABC
function BufToHex(const Buffer; BufferSize: Integer): string;
var
  i: Integer;
  pb: PByte;
begin
  Result := '';
  pb := @Buffer;
  for i := 0 to BufferSize - 1 do
  begin
    Result := Result + IntToHex(pb^, 2);
    Inc(pb);
  end;
end;

// Example: [01 0A BC]
function BufferToHex(const Buffer; BufferSize: Integer): string;
var
  i: Integer;
  pb: PByte;
begin
  Result := '[';
  pb := @Buffer;
  for i := 0 to BufferSize - 1 do
  begin
    if i > 0 then Result := Result + ' ';
    Result := Result + IntToHex(pb^, 2);
    Inc(pb);
  end;
  Result := Result + ']';
end;

function VarToInt(const AValue: Variant): Integer;
begin
  if VarIsOrdinal(AValue) then
    Result := AValue
  else
    Result := 0;
end;

function VarToInt64(const AValue: Variant): Int64;
begin
  if VarIsOrdinal(AValue) then
    Result := AValue
  else
    Result := 0;
end;

function DataAsStr(const AData; ALen: Integer): string;
var
  i: Integer;
begin
  Result := '';
  if ALen = 0 then Exit;

  SetLength(Result, ALen);
  Move(AData, Result[1], ALen);
  for i := 1 to ALen do
  begin
    if Ord(Result[i]) < 32 then
      Result[i] := '.';
  end;
end;

function WideDataToStr(AData: AnsiString): string;
var
  ws: WideString;
begin
  if Length(AData) > 0 then
  begin
    SetLength(ws, Length(AData) div 2);
    System.Move(AData[1], ws[1], Length(AData));
    Result := ws;
  end
  else
    Result := '';
end;

procedure ReverseBytes(const AData; ASize: Integer);
var
  i, bt: Byte;
  bp1, bp2: PByte;
begin
  bp1 := @AData;
  bp2 := @AData;
  Inc(bp2, ASize-1);
  for i := 0 to (ASize div 2)-1 do
  begin
    bt := bp1^;
    bp1^ := bp2^;
    bp2^ := bt;
    Inc(bp1);
    Dec(bp2);
  end;
end;

// AQuotes = '""', '[]'
function RemoveQuotes(AStr: string; AQuotes: string): string;
begin
  if Copy(AStr, 1, 1) <> Copy(AQuotes, 1, 1) then
    Result := AStr
  else
  if Copy(AStr, Length(AStr), 1) <> Copy(AQuotes, 2, 1) then
    Result := AStr
  else
    Result := Copy(AStr, 2, Length(AStr)-2);
end;

{ TDbRowsList }

function TDbRowsList.GetItem(AIndex: Integer): TDbRowItem;
begin
  Result := TDbRowItem(Get(AIndex));
end;

function TDbRowsList.IsEmpty: Boolean;
begin
  Result := (Count > 0);
end;

function TDbRowsList.IsGhost: Boolean;
begin
  Result := False;
end;

function TDbRowsList.IsSystem: Boolean;
begin
  Result := False;
end;

procedure TDbRowsList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TDbRowItem(Ptr).Free;
end;

{procedure TDbRowsList.SetFieldInfo(var AIndex: Integer; AName: string; AType, ASize: Integer);
begin
  if AIndex >= Length(FieldsInfo) then
    SetLength(FieldsInfo, AIndex+1);
  FieldsInfo[AIndex].Name := AName;
  FieldsInfo[AIndex].VType := Byte(AType);
  FieldsInfo[AIndex].Size := ASize;
end; }

{ TDbRowItem }

procedure TDbRowItem.Assign(ASourceItem: TDbRowItem);
begin
  Values := ASourceItem.Values;
  RawData := ASourceItem.RawData;
  RawOffs := ASourceItem.RawOffs;
end;

constructor TDbRowItem.Create(AOwner: TDbRowsList);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TDbRowItem.GetFieldAsStr(AFieldIndex: Integer): string;
var
  dt: TDateTime;
begin

  if VarIsNull(Values[AFieldIndex]) then
    Result := '<null>'
  else
  if VarIsType(Values[AFieldIndex], varDate) then
  begin
    dt := Values[AFieldIndex];
    if dt > MaxDateTime then
      dt := MaxDateTime;
    if dt < MinDateTime then
      dt := MinDateTime;
    if Trunc(dt) = 0 then
      Result := FormatDateTime('HH:NN:SS', dt)
    else if Frac(dt) = 0 then
      Result := FormatDateTime('YYYY-MM-DD', dt)
    else
      Result := FormatDateTime('YYYY-MM-DD HH:NN:SS', dt);
  end
  else
  if Assigned(Owner) and (Owner.FieldsDef[AFieldIndex].FieldType = ftBytes) then
  begin
    Result := VarToStrDef(Values[AFieldIndex], '');
    if Result <> '' then
      Result := '0x' + BufToHex(Result[1], Length(Result));
  end
  else
  try
    Result := VarToStrDef(Values[AFieldIndex], '');
  except
    Result := '<error>';
  end;
end;

{ TDBReader }

procedure TDBReader.AfterConstruction;
begin
  inherited;
end;

procedure TDBReader.BeforeDestruction;
begin
  FreeAndNil(FFile);
  inherited;
end;

function TDBReader.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
begin
  Result := False;
end;

function TDBReader.GetProgress: Integer;
begin
  Result := Trunc(FFile.Position / (FFile.Size + 1) * 1000);
end;

function TDBReader.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := nil;
end;

function TDBReader.GetTablesCount: Integer;
begin
  Result := 0;
end;

procedure TDBReader.LogInfo(AStr: string);
begin
  if Assigned(OnLog) then OnLog(AStr);
end;

function TDBReader.OpenFile(AFileName: string; AStream: TStream): Boolean;
begin
  Result := False;
  FreeAndNil(FFile);
  if not FileExists(AFileName) and (not Assigned(AStream)) then Exit;
  if Assigned(AStream) then
    FFile := AStream
  else
    FFile := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  FileName := AFileName;
  Result := True;
end;

{ TDbReaderDataSet }

constructor TDbReaderDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FIsOpen := False;
end;

function TDbReaderDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  RecItem: TDbRowItem;
  n: Integer;
  sData: AnsiString;
begin
  Result := TMemoryStream.Create();
  if (Mode <> bmRead) then
    Exit;
  if not Assigned(FRowsList) or (Length(FRowsList.FieldsDef) < Field.FieldNo) then
    Exit;

  RecItem := FRowsList.GetItem(PRecordBuffer(ActiveBuffer)^.RecordNum-1);

  n := Field.FieldNo-1;
  sData := RecItem.Values[n];
  if sData <> '' then
    Result.Write(sData[1], Length(sData));

  Result.Position := 0;
end;

procedure TDbReaderDataSet.InternalHandleException;
begin
  //raise Exception.Create('TDbReaderDataSet');
end;

procedure TDbReaderDataSet.InternalInitFieldDefs;
var
  i: Integer;
begin
  FieldDefs.Clear;
  for i := 0 to Length(FRowsList.FieldsDef) - 1 do
  begin
    with FieldDefs.AddFieldDef do
    begin
      DataType := FRowsList.FieldsDef[i].FieldType;
      {$ifndef FPC}
      FieldNo := i+1;
      {$endif}
      Name := FRowsList.FieldsDef[i].Name;
      //Size := FRowsList.FieldsDef[i].Size;
    end;
  end;
end;

procedure TDbReaderDataSet.InternalOpen;
begin
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(True);
  FIsOpen := True;
  FCursor := 0;
end;

function TDbReaderDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

procedure TDbReaderDataSet.InternalClose;
begin
  BindFields(False); // unbind fields
  if DefaultFields then
    DestroyFields;
  FIsOpen := False;
end;

function TDbReaderDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK;
  case GetMode of
    gmPrior:
      if FCursor <= 1 then
        Result := grBOF
      else
        Dec(FCursor);
    gmNext:
      if FCursor >= RecordCount then
        Result := grEOF
      else
        Inc(FCursor);
    gmCurrent: if (FCursor < 1) or (FCursor > RecordCount) then
        Result := grError;
  end;
  if Result = grOK then
    PRecordBuffer(Buffer).RecordNum := FCursor;
  if (Result = grError) and DoCheck then
    DatabaseError('Error in GetRecord()');
end;

function TDbReaderDataSet.AllocRecordBuffer: PChar;
begin
  GetMem(Result, SizeOf(TRecordBuffer));
end;

procedure TDbReaderDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer, SizeOf(TRecordBuffer));
end;

procedure TDbReaderDataSet.InternalInitRecord(Buffer: PChar);
begin
  inherited;
end;

procedure TDbReaderDataSet.InternalFirst;
begin
  FCursor := 0;
end;

procedure TDbReaderDataSet.InternalLast;
begin
  FCursor := RecordCount + 1;
end;

procedure TDbReaderDataSet.InternalSetToRecord(Buffer: PChar);
begin
  FCursor := PRecordBuffer(Buffer)^.RecordNum;
end;

procedure TDbReaderDataSet.AssignRowsList(ARowsList: TDbRowsList);
begin
  FRowsList := ARowsList;
  Open();
end;

function TDbReaderDataSet.GetCanModify: Boolean;
begin
  Result := False;
end;

function TDbReaderDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecItem: TDbRowItem;
  n: Integer;
  s: string;
begin
  Result := False;
  if not Assigned(FRowsList) or (Length(FRowsList.FieldsDef) < Field.FieldNo) then
    Exit;

  RecItem := FRowsList.GetItem(PRecordBuffer(ActiveBuffer)^.RecordNum-1);

  Result := True;
  n := Field.FieldNo-1;
  case FRowsList.FieldsDef[n].FieldType of
    ftString:
    begin
      //PAnsiString(Buffer)^ := RecItem.Values[n];
      s := RecItem.Values[n];
      StrLCopy(Buffer, PChar(s), Length(s));
    end;
    ftSmallint: PSmallInt(Buffer)^ := RecItem.Values[n];
    ftInteger: PInteger(Buffer)^ := RecItem.Values[n];
    ftWord: PWord(Buffer)^ := RecItem.Values[n];
    ftBoolean: PBoolean(Buffer)^ := RecItem.Values[n];
    ftFloat: PDouble(Buffer)^ := RecItem.Values[n];
    ftCurrency: PCurrency(Buffer)^ := RecItem.Values[n];
    ftDate: PDate(Buffer)^ := RecItem.Values[n];
    ftTime: PDateTime(Buffer)^ := RecItem.Values[n];
    ftDateTime: PDateTime(Buffer)^ := RecItem.Values[n];
    {todo: blobs}
  else
    Result := False;
  end;
end;

function TDbReaderDataSet.GetRecNo: Integer;
begin
  Result := PRecordBuffer(ActiveBuffer)^.RecordNum;
end;

function TDbReaderDataSet.GetRecordCount: Integer;
begin
  Result := FRowsList.Count;
end;

procedure TDbReaderDataSet.SetRecNo(Value: Integer);
begin
  if (Value < 1) or (Value >= RecordCount + 1) then
    exit;
  FCursor := Value;
  Resync([]);
end;

{ TRawDataReader }

procedure TRawDataReader.Init(const AData; AIsBigEndian: Boolean);
begin
  Data := '';
  StartPtr := @AData;
  DataPtr := @AData;
  IsBigEndian := AIsBigEndian;
end;

procedure TRawDataReader.InitBuf(AData: AnsiString; AStartPos: Integer; AIsBigEndian: Boolean);
begin
  Data := AData;
  if AStartPos = 0 then
    nPos := 1
  else
    nPos := AStartPos;
  StartPtr := @AData[nPos];
  DataPtr := @AData[nPos];
  IsBigEndian := AIsBigEndian;
end;

function TRawDataReader.ReadBytes(ASize: Integer): AnsiString;
begin
  Result := '';
  if ASize > 0 then
  begin
    SetLength(Result, ASize);
    Move(DataPtr^, Result[1], ASize);
    Inc(DataPtr, ASize);
  end;
end;

function TRawDataReader.ReadCString(AMaxSize: Integer; AFixed: Boolean): AnsiString;
var
  bt: Byte;
  nPos: Integer;
begin
  Result := '';
  nPos := GetPosition();
  bt := ReadUInt8;
  while (bt <> 0) and (Length(Result) < AMaxSize) do
  begin
    Result := Result + Chr(bt);
    bt := ReadUInt8;
  end;
  if AFixed then
    SetPosition(nPos + AMaxSize);
end;

function TRawDataReader.ReadInt16: SmallInt;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  // swap endiness
  Result := Swap(Result);
  {Result := (((Result shr 0) and $FF) shl 8)
         or (((Result shr 8) and $FF) shl 0); }
end;

function TRawDataReader.ReadInt24: Integer;
begin
  Result := 0;
  Move(DataPtr^, Result, 3);
  Inc(DataPtr, 3);
  // upscale to 4 bytes
  Result := Result shr 8;
  if Result and ($800000) <> 0 then
    Result := Result or ($FF000000);

  if not IsBigEndian then Exit;
  Result := (((Result shr  0) and $FF) shl 24)
         or (((Result shr  8) and $FF) shl 16)
         or (((Result shr 16) and $FF) shl  8)
         or (((Result shr 24) and $FF) shl  0);
end;

function TRawDataReader.ReadInt32: Integer;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  Result := (((Result shr  0) and $FF) shl 24)
         or (((Result shr  8) and $FF) shl 16)
         or (((Result shr 16) and $FF) shl  8)
         or (((Result shr 24) and $FF) shl  0);
end;

function TRawDataReader.ReadInt48: Int64;
begin
  Result := 0;
  Move(DataPtr^, Result, 6);
  Inc(DataPtr, 6);
  // upscale to 8 bytes
  Result := Result shr 16;
  if Result and ($800000000000) <> 0 then
    Result := Result or ($FFFF000000000000);

  if not IsBigEndian then Exit;
  ReverseBytes(Result, SizeOf(Result));
end;

function TRawDataReader.ReadInt64: Int64;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  ReverseBytes(Result, SizeOf(Result));
end;

function TRawDataReader.ReadInt8: ShortInt;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
end;

function TRawDataReader.ReadUInt16: Word;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  // swap endiness
  Result := (((Result shr 0) and $FF) shl 8)
         or (((Result shr 8) and $FF) shl 0);
end;

function TRawDataReader.ReadUInt32: Cardinal;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  // swap endiness
  Result := (((Result shr  0) and $FF) shl 24)
         or (((Result shr  8) and $FF) shl 16)
         or (((Result shr 16) and $FF) shl  8)
         or (((Result shr 24) and $FF) shl  0);
end;

function TRawDataReader.ReadUInt64: UInt64;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  ReverseBytes(Result, SizeOf(Result));
end;

function TRawDataReader.ReadUInt8: Byte;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
end;

function TRawDataReader.ReadCurrency: Currency;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  ReverseBytes(Result, SizeOf(Result));
end;

function TRawDataReader.ReadSingle: Single;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  ReverseBytes(Result, SizeOf(Result));
end;

function TRawDataReader.ReadDouble: Double;
begin
  Result := 0;
  Move(DataPtr^, Result, SizeOf(Result));
  Inc(DataPtr, SizeOf(Result));
  if not IsBigEndian then Exit;
  ReverseBytes(Result, SizeOf(Result));
end;

procedure TRawDataReader.ReadToBuffer(var ABuf; ASize: Integer);
begin
  Move(DataPtr^, ABuf, ASize);
  Inc(DataPtr, ASize);
end;

procedure TRawDataReader.SetPosition(AOffset: Integer);
begin
  DataPtr := StartPtr;
  Inc(DataPtr, AOffset);
end;

function TRawDataReader.GetPosition: Integer;
begin
  // todo: native
  Result := Integer(DataPtr) - Integer(StartPtr);
end;


{ TInnerFileStream }

constructor TInnerFileStream.Create(AFileName, AInnerFileName: string; APos, ASize: Int64);
begin
  inherited Create(AFileName, fmOpenRead or fmShareDenyNone);
  InnerFilePos := APos;
  InnerFileSize := ASize;
  InnerFileName := AInnerFileName;
  inherited Seek(InnerFilePos, soBeginning);
end;

function TInnerFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
  case Origin of
    soBeginning: Result := inherited Seek(InnerFilePos + Offset, Origin);
    soCurrent: Result := inherited Seek(Offset, Origin);
    soEnd: Result := inherited Seek(InnerFilePos + InnerFileSize - Offset, soBeginning);
  end;
  Dec(Result, InnerFilePos);
end;

end.
