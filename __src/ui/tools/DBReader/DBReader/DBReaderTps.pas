unit DBReaderTps;

(*
Clarion TopSpeed database file reader (.tps)

Author: Sergey Bodrov, 2025 Minsk
License: MIT

* TODO: memo fields

*)

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
  {$ifdef FPC}
  LazUTF8, {LConvEncoding,}
  {$else}
  Windows, {for OemToChar}
  {$endif}
  SysUtils, Classes, Variants, DBReaderBase, DB, Types;

type
  TTpsFieldRec = record
    FieldType: Byte;   // u8  field type
    Offset: Word;      // u16 offset
    Name: string;      // cs  name (zero-terminated)
    Count: Word;       // u16 items count
    Size: Word;        // u16 items total size
    Over: Word;        // u16 is overlay (=1)
    Order: Word;       // u16 field order num
    Sub: Word;         // (optional) for some field types
    Sub2: Word;        // (optional) for some field types
    Template: string;  // (optional) for strings
  end;

type
  { TTpsTableInfo }

  TTpsTableInfo = class(TDbRowsList)
  public
    Fields: array of TTpsFieldRec;

    procedure AfterConstruction; override;
    // predefined table
    function IsSystem(): Boolean; override;

    procedure AddFieldDef(AName: string; AType, ASize: Integer);
  end;

  TTpsBlockRangeRec = record
    From: Cardinal;
    Next: Cardinal;
  end;

  TTpsMetadataRec = record
    BlockNum: Integer;     // u16 definitions 512b block num
    MinVer: Integer;       // u16 min version
    SomeSize: Integer;     // u16 definitions data size ?
    FieldsCount: Integer;  // u16 fields count
    MemoCount: Integer;    // u16 memo count
    KeyCount: Integer;     // u16 index/keys count

    Data: TByteDynArray;
    DataPos: Integer;
  end;

  { TDBReaderTps }

  TDBReaderTps = class(TDBReader)
  private
    FPagesList: TTpsTableInfo;
    FTableInfo: TTpsTableInfo;
    FIsMetadataLoaded: Boolean;
    FBlockMapArr: array of TTpsBlockRangeRec;
    FMetaData: TTpsMetadataRec;
    FFileHeaderSize: Integer;

    // APagePos contain offset to next page after reading
    function ReadDataPage(var APageBuf: TByteDynArray; var APagePos: Int64;
      ATableInfo: TTpsTableInfo; AList: TDbRowsList): Boolean;

    function ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
      ATableInfo: TTpsTableInfo; AList: TDbRowsList): Boolean;

    procedure FillTablesList();

  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    function OpenFile(AFileName: string; AStream: TStream = nil): Boolean; override;
    // Read table data from DB to AList
    // AName - table name
    // ACount - how many items read
    procedure ReadTable(AName: string; ACount: Int64 = MaxInt; AList: TDbRowsList = nil); override;
    // get detailed multi-line description of table
    function FillTableInfoText(ATableName: string; ALines: TStrings): Boolean; override;

    // get tables count
    function GetTablesCount(): Integer; override;
    // get table by index 0..GetTablesCount()-1
    function GetTableByIndex(AIndex: Integer): TDbRowsList; override;

  end;

implementation

const
  TPS_FILE_HEADER_SIZE = $200;
  TPS_MIN_PAGE_SIZE    = $100;

  TPS_REC_TYPE_DATA    = $F3;   // data record
  TPS_REC_TYPE_INFO    = $F6;   // count of records of same type
  TPS_REC_TYPE_META    = $FA;   // table definition
  TPS_REC_TYPE_MEMO    = $FC;   // memo field data
  TPS_REC_TYPE_NAME    = $FE;   // table name

  TPS_REC_FLAG_LEN      = $80;  // length field present
  TPS_REC_FLAG_LEN_TREE = $40;  // length field present in tree page
  TPS_REC_FLAG_LEN_MASK = $3F;  // length from previous record

const
  TPS_FIELD_TYPE_BYTE         = $01; // Byte (1 byte) UInt8
  TPS_FIELD_TYPE_SHORT        = $02; // Short (2 byte) Int16
  TPS_FIELD_TYPE_USHORT       = $03; // Unsigned Short (2 byte) UInt16
  TPS_FIELD_TYPE_LONG         = $06; // Long (4 byte) Int32
  TPS_FIELD_TYPE_ULONG        = $07; // Unsigned Long (4 byte) UInt32
  TPS_FIELD_TYPE_SREAL        = $08; // Single (4 byte) Real32
  TPS_FIELD_TYPE_REAL         = $09; // Double (8 byte) Real64
  TPS_FIELD_TYPE_DECIMAL      = $0A; // Decimal (<16 byte) BCD
  TPS_FIELD_TYPE_STRING       = $12; // String
  TPS_FIELD_TYPE_CSTRING      = $13; // CString zero-terminated
  TPS_FIELD_TYPE_PSTRING      = $14; // PString size byte + data
  TPS_FIELD_TYPE_GROUP        = $16; // Group


type
  // file header
  TTpsFileHeaderRec = record
    HeadOffset: Cardinal;    // 4 Header offset
    HeaderSize: Word;        // 2 Header size
    FileSize: Cardinal;      // 4 File size
    FileSignature: Cardinal; // 4 File signature [74 4F 70 53]
    Unknown_12: Word;        // 2 00
    LastNum: Cardinal;       // 4 Last number
    UpdCount: Cardinal;      // 4 File changes count
  end;

  // page header
  TTpsPageHeadRec = record
    PageOffset: Cardinal;    // 4 Page offset
    DataSize: Word;          // 2 Data size (in file) can be same as unpacked
    DataSizeUnPacked: Word;  // 2 Data size (unpacked)
    DataSizeRaw: Word;       // 2 Data size (unpacked, unwrapped)
    PageRecCount: Word;      // 2 Records count
    PageLevel: Byte;         // 1 Page level
    PackOffset: Byte;        // 1 Pack offset (only packed pages)
  end;

  // row header structure
  TTpsRowHeadRec = record
    Flags: Byte;  // flags + prev size
    Size: Word;     // row size
    HeadSize: Word; // row header size (TabNum, RecType, RecNum)
    TableNum: Cardinal;
    RecType: Byte;
    RecNum: Cardinal;
  end;

function TpsFieldTypeToDbFieldType(AValue: Integer): TFieldType;
begin
  case AValue of
    TPS_FIELD_TYPE_BYTE:     Result := ftInteger;
    TPS_FIELD_TYPE_SHORT:    Result := ftInteger;
    TPS_FIELD_TYPE_USHORT:   Result := ftInteger;
    TPS_FIELD_TYPE_LONG:     Result := ftInteger;
    TPS_FIELD_TYPE_ULONG:    Result := ftInteger;
    TPS_FIELD_TYPE_SREAL:    Result := ftFloat;
    TPS_FIELD_TYPE_REAL:     Result := ftFloat;
    TPS_FIELD_TYPE_DECIMAL:  Result := ftBCD;
    TPS_FIELD_TYPE_STRING:   Result := ftString;
    TPS_FIELD_TYPE_CSTRING:  Result := ftString;
    TPS_FIELD_TYPE_PSTRING:  Result := ftString;
    //TPS_FIELD_TYPE_GROUP:    Result := ftUnknown;
  else
    Result := ftUnknown;
  end;
end;

function TpsFieldTypeName(AValue, ASize: Integer): string;
begin
  case AValue of
    TPS_FIELD_TYPE_BYTE:     Result := 'BYTE';
    TPS_FIELD_TYPE_SHORT:    Result := 'SHORT';
    TPS_FIELD_TYPE_USHORT:   Result := 'USHORT';
    TPS_FIELD_TYPE_LONG:     Result := 'LONG';
    TPS_FIELD_TYPE_ULONG:    Result := 'ULONG';
    TPS_FIELD_TYPE_SREAL:    Result := 'SREAL';
    TPS_FIELD_TYPE_REAL:     Result := 'REAL';
    TPS_FIELD_TYPE_DECIMAL:  Result := 'DECIMAL';
    TPS_FIELD_TYPE_STRING:   Result := 'STRING';
    TPS_FIELD_TYPE_CSTRING:  Result := 'CSTRING';
    TPS_FIELD_TYPE_PSTRING:  Result := 'PSTRING';
    TPS_FIELD_TYPE_GROUP:    Result := 'GROUP';
  else
    Result := 'Unknown';
  end;
end;

{ TTpsTableInfo }

procedure TTpsTableInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  Fields := [];
  //SetLength(FieldsDef, 4);
end;

function TTpsTableInfo.IsSystem(): Boolean;
begin
  Result := (TableName = 'sys_pages');
end;

procedure TTpsTableInfo.AddFieldDef(AName: string; AType, ASize: Integer);
var
  n: Integer;
begin
  n := Length(FieldsDef);
  SetLength(FieldsDef, n+1);
  FieldsDef[n].Name := AName;
  FieldsDef[n].FieldType := TpsFieldTypeToDbFieldType(AType);
  FieldsDef[n].Size := ASize;
end;

{ TDBReaderTps }

procedure TDBReaderTps.AfterConstruction;
begin
  inherited;
  //FIsSingleTable := True;
  FPagesList := TTpsTableInfo.Create();
  FPagesList.TableName := 'sys_pages';
  FPagesList.AddFieldDef('Offs', TPS_FIELD_TYPE_ULONG, 4);
  FPagesList.AddFieldDef('Type', TPS_FIELD_TYPE_STRING, 40);
  FPagesList.AddFieldDef('Size', TPS_FIELD_TYPE_USHORT, 2);
  FPagesList.AddFieldDef('SizeUnpack', TPS_FIELD_TYPE_USHORT, 2);
  FPagesList.AddFieldDef('SizeRaw', TPS_FIELD_TYPE_USHORT, 2);
  FPagesList.AddFieldDef('RecCount', TPS_FIELD_TYPE_USHORT, 2);

  FTableInfo := TTpsTableInfo.Create();
  FTableInfo.TableName := 'data';

  FMetaData.SomeSize := 0;
  FMetaData.DataPos := 0;
  FMetaData.Data := [];
end;

procedure TDBReaderTps.BeforeDestruction;
begin
  FreeAndNil(FTableInfo);
  FreeAndNil(FPagesList);
  inherited;
end;

function TDBReaderTps.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  s: string;
  TmpTable: TTpsTableInfo;
begin
  Result := False;
  TmpTable := FTableInfo;

  with TmpTable do
  begin
    ALines.Add(Format('== Table Name=%s', [TableName]));
    //ALines.Add(Format('LastUpdate=%.2d-%.2d-%.2d', [yy + FileHeader.LastUpdate[0], DbfHeader.LastUpdate[1], DbfHeader.LastUpdate[2]]));
    //ALines.Add(Format('Description=%s', [FileHeader.Description]));
    ALines.Add(Format('== Fields  Count=%d', [Length(Fields)]));
    for i := Low(Fields) to High(Fields) do
    begin
      s := Format('%.2d Order=%d  Name=%s  Type=%d %s  Size=%d  Offs=%d',
        [
          i,
          Fields[i].Order,
          Fields[i].Name,
          Fields[i].FieldType,
          TpsFieldTypeName(Fields[i].FieldType, Fields[i].Size),
          Fields[i].Size,
          Fields[i].Offset
        ]);
      if Fields[i].Over <> 0 then
        s := s + ' OVER';
      if Fields[i].Count > 1 then
        s := s + ' Count=' + IntToStr(Fields[i].Count);
      ALines.Add(s);
    end;

  end;
end;

function TDBReaderTps.GetTablesCount(): Integer;
begin
  Result := 2;
end;

function TDBReaderTps.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  case AIndex of
    0: Result := FPagesList;
    1: Result := FTableInfo;
  end;
end;

function TDBReaderTps.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  FileHeader: TTpsFileHeaderRec;
  rdr: TRawDataReader;
  iPagePos: Int64;
  i, n, iPageSize: Integer;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  FTableInfo.TableName := ExtractFileName(AFileName);

  // init reader
  PageBuf := [];
  SetLength(PageBuf, TPS_FILE_HEADER_SIZE);
  FFile.Position := 0;
  FFile.Read(PageBuf[0], TPS_FILE_HEADER_SIZE);
  rdr.Init(PageBuf[0], False);

  // read header
  FileHeader.HeadOffset := rdr.ReadUInt32;
  FileHeader.HeaderSize := rdr.ReadUInt16;
  FileHeader.FileSize := rdr.ReadUInt32;
  FileHeader.FileSignature := rdr.ReadUInt32;
  FileHeader.Unknown_12 := rdr.ReadUInt8;
  FileHeader.LastNum := rdr.ReadUInt32;
  FileHeader.UpdCount := rdr.ReadUInt32;

  FFileHeaderSize := FileHeader.HeaderSize;

  // read block map
  n := (FileHeader.HeaderSize - $20) div (2 * 4); // offset pair count
  SetLength(FBlockMapArr, n);
  rdr.SetPosition($20);
  for i := 0 to n-1 do
    FBlockMapArr[i].From := rdr.ReadUInt32;
  for i := 0 to n-1 do
    FBlockMapArr[i].Next := rdr.ReadUInt32;

  // scan pages
  iPageSize := TPS_MIN_PAGE_SIZE;
  SetLength(PageBuf, $10000);
  iPagePos := FFileHeaderSize;
  while iPagePos + iPageSize <= FFile.Size do
  begin
    FFile.Position := iPagePos;
    FFile.Read(PageBuf[0], iPageSize);

    ReadDataPage(PageBuf, iPagePos, nil, nil);

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

  FillTablesList();
end;

function TDBReaderTps.ReadDataPage(var APageBuf: TByteDynArray;
  var APagePos: Int64; ATableInfo: TTpsTableInfo; AList: TDbRowsList): Boolean;
var
  PageHead: TTpsPageHeadRec;
  rdr, rdrPage: TRawDataReader;
  i, iRowPos, n: Integer;
  iStartPos, iRawPos, iCopyCount, iRepCount: Integer;
  btRepByte: Byte;
  nPageOffs: Cardinal;
  isDebugPage: Boolean;
  TmpRow: TDbRowItem;
  RowHead: TTpsRowHeadRec;
  RawData: TByteDynArray;
  RawRec, PrevRecHead: TByteDynArray;
begin
  Result := False;
  rdr.Init(APageBuf[0], False);
  RawData := [];
  isDebugPage := False;
  //isDebugPage := (APagePos = $F00);

  PageHead.PageOffset := rdr.ReadUInt32;
  PageHead.DataSize := rdr.ReadUInt16;
  PageHead.DataSizeUnPacked := rdr.ReadUInt16;
  PageHead.DataSizeRaw := rdr.ReadUInt16;
  PageHead.PageRecCount := rdr.ReadUInt16;
  PageHead.PageLevel := rdr.ReadUInt8;

  // skip wrong header
  if (PageHead.PageOffset <> APagePos) then
  begin
    Inc(APagePos, TPS_MIN_PAGE_SIZE);
    Exit;
  end;

  // next page offset
  if (PageHead.DataSize = 0) then
    n := 1
  else
    n := ((PageHead.DataSize - 1) div TPS_MIN_PAGE_SIZE) + 1;
  // re-read page, if size larger, than minimal
  if n > 1 then
  begin
    FFile.Position := APagePos;
    FFile.Read(APageBuf[0], n * TPS_MIN_PAGE_SIZE);
  end;
  iStartPos := rdr.GetPosition(); // data start (header size)

  // check for mid-page header
  for i := 1 to n do
  begin
    rdr.SetPosition(i * TPS_MIN_PAGE_SIZE);
    nPageOffs := rdr.ReadUInt32;
    if nPageOffs = APagePos + (i * TPS_MIN_PAGE_SIZE) then
    begin
      LogInfo('Incomplete page: ' + IntToHex(APagePos, 8));
      APagePos := nPageOffs;
      Exit;
    end;
  end;

  Inc(APagePos, n * TPS_MIN_PAGE_SIZE); // next page

  if (not FIsMetadataLoaded) and IsDebugPages then
  begin
    // page info
    TmpRow := TDbRowItem.Create(FPagesList);
    FPagesList.Add(TmpRow);
    SetLength(TmpRow.Values, 6);
    TmpRow.Values[0] := PageHead.PageOffset; // Offs
    TmpRow.Values[1] := 'Page L' + IntToStr(PageHead.PageLevel); // Type
    TmpRow.Values[2] := PageHead.DataSize; // Size
    TmpRow.Values[3] := PageHead.DataSizeUnPacked; // Size Unpacked
    TmpRow.Values[4] := PageHead.DataSizeRaw; // Size Raw
    TmpRow.Values[5] := PageHead.PageRecCount; // RecCount
  end;

  if PageHead.PageLevel > 0 then
  begin
    // tree page
    Result := True;
    Exit;
  end;

  SetLength(RawData, PageHead.DataSizeUnPacked);
  // unpack
  if PageHead.DataSize <> PageHead.DataSizeUnPacked then
  begin
    // debug
    if isDebugPage then
      BufToFile(APageBuf[0], PageHead.DataSize, 'packed_page.raw');

    iRawPos := 0;
    // copy header to raw
    rdr.SetPosition(0);
    rdr.ReadToBuffer(RawData[iRawPos], iStartPos);
    Inc(iRawPos, iStartPos);

    // copy bytes before repeated item
    iCopyCount := rdr.ReadUInt8;
    if iCopyCount > $7F then // extended
    begin
      iCopyCount := (iCopyCount and $7F) shl 1;
      iCopyCount := iCopyCount + (rdr.ReadUInt8 shl 8);
      iCopyCount := iCopyCount shr 1;
    end;

    while (rdr.GetPosition() < (PageHead.DataSize - 2)) do
    begin
      if (rdr.GetPosition() + iCopyCount) > PageHead.DataSize then
        Break;
      Assert(iRawPos + iCopyCount <= PageHead.DataSizeUnPacked);
      // copy raw data up to packed item offset
      rdr.ReadToBuffer(RawData[iRawPos], iCopyCount);
      Inc(iRawPos, iCopyCount);
      if (rdr.GetPosition() + 2) >= PageHead.DataSize then
        Break;
      if iRawPos >= PageHead.DataSizeUnPacked then
        Break;

      if iCopyCount = 0 then
      begin
        LogInfo('CopyCount=0 on page: ' + IntToHex(PageHead.PageOffset, 8));
        Exit;
      end
      else
        btRepByte := RawData[iRawPos-1]; // last copy byte is repeated

      // read repeat count and next copy count
      iRepCount := rdr.ReadUInt8;
      if iRepCount > 127 then // extended
      begin
        iRepCount := (iRepCount and $7F) shl 1;
        iRepCount := iRepCount + (rdr.ReadUInt8 shl 8);
        iRepCount := iRepCount shr 1;
      end;
      iCopyCount := rdr.ReadUInt8;
      if iCopyCount > $7F then // extended
      begin
        iCopyCount := (iCopyCount and $7F) shl 1;
        iCopyCount := iCopyCount + (rdr.ReadUInt8 shl 8);
        iCopyCount := iCopyCount shr 1;
      end;

      if (iRawPos + iRepCount) >= PageHead.DataSizeUnPacked then
        iRepCount := PageHead.DataSizeUnPacked - iRawPos - 1;
      if (iRepCount > 0) then
      begin
        // unpack item to raw
        FillChar(RawData[iRawPos], iRepCount, btRepByte);
        Inc(iRawPos, iRepCount);
      end;
      // debug
      if isDebugPage then
        BufToFile(RawData[0], PageHead.DataSizeUnPacked, 'page.raw');

      if iRawPos = PageHead.DataSizeUnPacked-1 then
        Break
    end;
    // debug
    if isDebugPage then
      BufToFile(RawData[0], PageHead.DataSizeUnPacked, 'page.raw');

    //iStartPos := 0;
  end
  else
  begin
    // copy page to raw
    rdr.SetPosition(0);
    rdr.ReadToBuffer(RawData[0], PageHead.DataSizeUnPacked);
  end;

  if isDebugPage then
    BufToFile(RawData[0], PageHead.DataSizeUnPacked, 'page.raw');  // !!

  rdrPage.Init(RawData[0], False);
  rdrPage.SetPosition(iStartPos);

  RawRec := [];
  PrevRecHead := [];
  SetLength(PrevRecHead, 64);

  // scan rows
  RowHead.RecType := 0;
  for i := 0 to PageHead.PageRecCount-1 do
  begin
    RowHead.Flags := rdrPage.ReadUInt8;
    // optional sizes
    if (RowHead.Flags and $80) <> 0 then
      RowHead.Size := rdrPage.ReadUInt16;
    if (RowHead.Flags and $40) <> 0 then
      RowHead.HeadSize := rdrPage.ReadUInt16;
    iCopyCount := RowHead.Flags and $3F;

    iRowPos := rdrPage.GetPosition();
    SetLength(RawRec, RowHead.Size);
    //FillChar(RawRec[0], RowHead.Size, 0); // !!

    if RowHead.Size = 0 then
      Continue;

    // copy begining from previous row
    if iCopyCount > 0 then
    begin
      rdr.Init(PrevRecHead[0], False);
      rdr.ReadToBuffer(RawRec[0], iCopyCount);
    end;

    // copy row data from page
    rdrPage.ReadToBuffer(RawRec[iCopyCount], RowHead.Size-iCopyCount);

    // read row data
    rdr.Init(RawRec[0], False);
    RowHead.TableNum := rdr.ReadUInt32;
    ReverseBytes(RowHead.TableNum, SizeOf(RowHead.TableNum));
    RowHead.RecType := rdr.ReadUInt8;
    case RowHead.RecType of
      TPS_REC_TYPE_DATA:
      begin
        if FIsMetadataLoaded then
          ReadRowData(RawRec, 0, Length(RawRec), ATableInfo, AList);
        //RowHead.RecNum := rdr.ReadUInt32;
        //ReverseBytes(RowHead.RecNum, SizeOf(RowHead.RecNum));
      end;
      TPS_REC_TYPE_INFO:
      begin
        // u8  Record type
        // u32 Records count
        // u32 First record
      end;
      TPS_REC_TYPE_META:
      begin
        if not FIsMetadataLoaded then
        begin
          // --- metadata block (up to 512 bytes)
          // u16 block num
          // next fields only for first block, num=0
          // u16 min version
          // u16 definitions data size
          // u16 fields count
          // u16 memo count
          // u16 index/keys count
          FMetaData.BlockNum := rdr.ReadUInt16;
          if FMetaData.BlockNum = 0 then
          begin
            FMetaData.MinVer := rdr.ReadUInt16;
            FMetaData.SomeSize := rdr.ReadUInt16;  // can be larger, than record size!
            FMetaData.FieldsCount := rdr.ReadUInt16;
            FMetaData.MemoCount := rdr.ReadUInt16;
            FMetaData.KeyCount := rdr.ReadUInt16;
          end;

          //BufToFile(RawRec[0], Length(RawRec), 'rec.raw');  // !!

          // copy metadata
          n := Length(RawRec) - rdr.GetPosition(); // metadata block size
          SetLength(FMetaData.Data, Length(FMetaData.Data) + n);
          rdr.ReadToBuffer(FMetaData.Data[FMetaData.DataPos], n);
          Inc(FMetaData.DataPos, n);
        end;
      end;
    end;
    // copy row header as prev
    //FillChar(PrevRecHead[0], Length(PrevRecHead), 0); // !!
    iRepCount := Length(PrevRecHead);
    if iRepCount > RowHead.Size then
      iRepCount := RowHead.Size;
    rdr.SetPosition(0);
    rdr.ReadToBuffer(PrevRecHead[0], iRepCount);

    // next row
    if (iRowPos + RowHead.Size - iCopyCount) > (Length(RawData) - 3) then
      Break;
    rdrPage.SetPosition(iRowPos + RowHead.Size - iCopyCount);
  end;

  Result := True;
end;

function TDBReaderTps.ReadRowData(const APageBuf: TByteDynArray; ARowOffs,
  ARowSize: Integer; ATableInfo: TTpsTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  nRecNum: Cardinal;
  TmpRow: TDbRowItem;
  i, iFieldSize: Integer;
  btFieldType: Byte;
  v: Variant;
  s: string;
begin
  Result := False;
  if (not FIsMetadataLoaded)
  or (not Assigned(ATableInfo))
  or (not Assigned(AList)) then
    Exit;

  rdr.Init(APageBuf[0], False);

  nRecNum := rdr.ReadUInt32;
  ReverseBytes(nRecNum, SizeOf(nRecNum));

  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);
  if IsDebugRows then
  begin
    rdr.SetPosition(0);
    TmpRow.RawData := rdr.ReadBytes(ARowSize);
  end;

  SetLength(TmpRow.Values, Length(ATableInfo.Fields));

  for i := 0 to Length(ATableInfo.Fields)-1 do
  begin
    btFieldType := ATableInfo.Fields[i].FieldType;
    iFieldSize := ATableInfo.Fields[i].Size;

    rdr.SetPosition(ATableInfo.Fields[i].Offset + 4+1+4); // skip TabNum + RecType + RecNum
    v := Null;
    case btFieldType of
      TPS_FIELD_TYPE_BYTE:     v := rdr.ReadUInt8;
      TPS_FIELD_TYPE_SHORT:    v := rdr.ReadInt16;
      TPS_FIELD_TYPE_USHORT:   v := rdr.ReadUInt16;
      TPS_FIELD_TYPE_LONG:     v := rdr.ReadInt32;
      TPS_FIELD_TYPE_ULONG:    v := rdr.ReadUInt32;
      TPS_FIELD_TYPE_SREAL:    v := rdr.ReadSingle;
      TPS_FIELD_TYPE_REAL:     v := rdr.ReadDouble;
      TPS_FIELD_TYPE_DECIMAL:
      begin
        s := rdr.ReadBytes(iFieldSize);
        if s <> '' then
          v := BufferToHex(s[1], Length(s));
      end;
      TPS_FIELD_TYPE_STRING:
      begin
        s := rdr.ReadBytes(iFieldSize);
        s := WinCPToUTF8(s);
        v := s;
      end;
      TPS_FIELD_TYPE_CSTRING:
      begin
        s := rdr.ReadCString(iFieldSize);
        s := WinCPToUTF8(s);
        v := s;
      end;
      TPS_FIELD_TYPE_PSTRING:
      begin
        s := rdr.ReadCString(iFieldSize);
        s := WinCPToUTF8(s);
        v := s;
      end;
      TPS_FIELD_TYPE_GROUP:
      begin
        s := rdr.ReadBytes(iFieldSize);
        if s <> '' then
          v := BufferToHex(s[1], Length(s));
      end;

    end;

    TmpRow.Values[i] := v;
  end;

end;

procedure TDBReaderTps.FillTablesList();
var
  rdr: TRawDataReader;
  i, ii, iii, n: Integer;
  FieldRec: TTpsFieldRec;
begin
  if Length(FMetaData.Data) = 0 then
    Exit;
  // metadata
  //BufToFile(FMetaData.Data[0], Length(FMetaData.Data), 'metadata.raw');  // !!

  rdr.Init(FMetaData.Data[0], False);

  for ii := 0 to FMetaData.FieldsCount-1 do
  begin
    // === field definition ===
    FieldRec.FieldType := rdr.ReadUInt8;
    FieldRec.Offset := rdr.ReadUInt16;
    FieldRec.Name := rdr.ReadCString(32);
    FieldRec.Count := rdr.ReadUInt16;
    FieldRec.Size := rdr.ReadUInt16;
    FieldRec.Over := rdr.ReadUInt16;
    FieldRec.Order := rdr.ReadUInt16;
    case FieldRec.FieldType of
      TPS_FIELD_TYPE_DECIMAL:
      begin
        FieldRec.Sub := rdr.ReadUInt8;  // precision
        FieldRec.Sub2 := rdr.ReadUInt8; // size
      end;
      TPS_FIELD_TYPE_STRING,
      TPS_FIELD_TYPE_CSTRING,
      TPS_FIELD_TYPE_PSTRING:
      begin
        FieldRec.Sub := rdr.ReadUInt16;   // array item size
        FieldRec.Template := rdr.ReadCString(32);
        if FieldRec.Template = '' then
          FieldRec.Sub2 := rdr.ReadUInt8;
      end;
    end;

    n := Length(FTableInfo.Fields);
    SetLength(FTableInfo.Fields, n+1);
    FTableInfo.Fields[n] := FieldRec;
  end;

  for ii := 0 to FMetaData.MemoCount-1 do
  begin
    // === memo definition ===
    FieldRec.Template := rdr.ReadCString(32); // blob file name
    if FieldRec.Template = '' then
      FieldRec.Sub2 := rdr.ReadUInt8;
    FieldRec.Name := rdr.ReadCString(32);
    FieldRec.Size := rdr.ReadUInt16; // blob size
    FieldRec.Sub := rdr.ReadUInt16;  // blob attrs
    // blob attrs:
    // bit0 - 0-text
    // bit1 - 1-binary
    // bit2 - 1-text
  end;

  for ii := 0 to FMetaData.KeyCount-1 do
  begin
    // === key definition ===
    FieldRec.Template := rdr.ReadCString(32); // key file name
    if FieldRec.Template = '' then
      FieldRec.Sub2 := rdr.ReadUInt8;
    FieldRec.Name := rdr.ReadCString(32);
    FieldRec.Sub := rdr.ReadUInt8;  // key attrs byte
    FieldRec.Count := rdr.ReadUInt16; // key fields count
    // key attrs:
    // bit0 - DUP
    // bit1 - OPT
    // bit2 - NOCASE
    // bit5..bit6 - 0=KEY, 1=INDEX, 2=DYNAMIC INDEX
    for iii := 1 to FieldRec.Count do
    begin
      n := rdr.ReadUInt16; // key field num
      n := rdr.ReadUInt16; // key field attrs (= 0 ASCENDING)
    end;
  end;

  // update table fields definitions
  SetLength(FTableInfo.FieldsDef, Length(FTableInfo.Fields));
  for i := 0 to Length(FTableInfo.Fields) - 1 do
  begin
    FTableInfo.FieldsDef[i].FieldType := TpsFieldTypeToDbFieldType(FTableInfo.Fields[i].FieldType);
    FTableInfo.FieldsDef[i].Name := FTableInfo.Fields[i].Name;
    FTableInfo.FieldsDef[i].TypeName := TpsFieldTypeName(FTableInfo.Fields[i].FieldType, FTableInfo.Fields[i].Size);
    FTableInfo.FieldsDef[i].Size := FTableInfo.Fields[i].Size;
    FTableInfo.FieldsDef[i].RawOffset := FTableInfo.Fields[i].Offset + 9; // include TabNum, RecType, RecNum
  end;

  FIsMetadataLoaded := True;
end;

procedure TDBReaderTps.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i: Integer;
  TmpRow: TDbRowItem;
  TmpTable: TTpsTableInfo;
  iPageSize: Integer;
  iPagePos: Int64;
  PageBuf: TByteDynArray;
begin
  AList.Clear;
  AList.TableName := AName;
  if AName = 'sys_pages' then
    TmpTable := FPagesList
  else
    TmpTable := FTableInfo;
  AList.FieldsDef := TmpTable.FieldsDef;

  if TmpTable.Count <> 0 then
  begin
    // copy data
    for i := 0 to TmpTable.Count - 1 do
    begin
      TmpRow := TDbRowItem.Create(AList);
      TmpRow.Assign(TmpTable.GetItem(i));
      AList.Add(TmpRow);
    end;
  end
  else
  begin
    // scan pages
    iPageSize := TPS_MIN_PAGE_SIZE;
    PageBuf := [];
    SetLength(PageBuf, $10000);
    iPagePos := FFileHeaderSize;
    while iPagePos + iPageSize <= FFile.Size do
    begin
      FFile.Position := iPagePos;
      FFile.Read(PageBuf[0], iPageSize);

      ReadDataPage(PageBuf, iPagePos, TmpTable, AList);

      if Assigned(OnPageReaded) then
        OnPageReaded(Self);
    end;
  end;

end;

end.
