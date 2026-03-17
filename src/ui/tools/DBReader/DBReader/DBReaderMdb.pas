unit DBReaderMdb;

(*
MS Access/MS Jet/Ace database reader

Author: Sergey Bodrov, 2024 Minsk
License: MIT

https://web.archive.org/web/20220711181532/http://fileformats.archiveteam.org/wiki/Access
https://web.archive.org/web/20220825195028/https://www.loc.gov/preservation/digital/formats/fdd/fdd000462.shtml
*)

interface

uses
  SysUtils, Classes, Variants, DBReaderBase, DB, Types;

type
  TMdbFileInfo = record
    MagicNumber: Cardinal;  // 0x100
    FileFormatID: AnsiString;
    JetVersion: Cardinal;
    SystemCollation: Word;
    SystemCodePage: Word;
    DatabaseKey: Cardinal;
    DatabasePassword: AnsiString;
    CreationDate: TDateTime;
  end;

  TMdbFieldDefRec = record
    ColType: Byte;
    ColID: Word;
    OffsetV: Word; // offset to variable part
    OffsetF: Word; // offset to fixed part
    ColNum: Word;
    SortOrder: Word;
    Misc: Word;
    Flags: Word;
    ColLen: Word;
    ColName: AnsiString;
  end;
  TMdbFieldDefRecArr = array of TMdbFieldDefRec;

  TMdbTableInfo = class(TDbRowsList)
  public
    TableID: Integer;  // PageID
    RowCount: Integer;
    VarColCount: Integer;
    PageIdArr: array of Integer;
    PageIdCount: Integer;
    FieldInfoArr: TMdbFieldDefRecArr;
    TableType: AnsiChar; // N ок S
    // contain no rows
    function IsEmpty(): Boolean; override;
    // predefined table
    function IsSystem(): Boolean; override;
    // not defined in metadata
    function IsGhost(): Boolean; override;
  end;

  TMdbTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TMdbTableInfo;
    function GetByName(AName: string): TMdbTableInfo;
    function GetByID(ATableID: Integer): TMdbTableInfo;
    procedure SortByName();
  end;

  TDBReaderMdb = class(TDBReader)
  private
    FTableList: TMdbTableInfoList;
    FFileInfo: TMdbFileInfo;
    FPageSize: Integer;
    FIsMetadataLoaded: Boolean;

    // raw page for blob reader
    FBlobRawPage: TByteDynArray;
    FBlobRawPageID: Integer;

    procedure ReadTabDefJet3(rdr: TRawDataReader; ATableInfo: TMdbTableInfo);
    procedure ReadTabDefJet4(rdr: TRawDataReader; ATableInfo: TMdbTableInfo);

    function ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
      ATableInfo: TMdbTableInfo; AList: TDbRowsList): Boolean;
    function ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
      ATableInfo: TMdbTableInfo; AList: TDbRowsList): Boolean;
    // fill schema tables from initial tables
    procedure FillTablesList();
    // for prefix $FFFE
    function GetDecompressedText(AStr: AnsiString): string;
    // Return blob content for PageID/RowID
    // ASize contain flags bits
    // if False, then AData contain error description
    // if another chunk present, set APageID to non-zero
    function GetBlobData(var APageID, ARowID, ASize: Integer; var AData: AnsiString): Boolean;
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

    property TableList: TMdbTableInfoList read FTableList;
    property FileInfo: TMdbFileInfo read FFileInfo;
  end;

implementation

{ TDBReaderMdf }

const
  MDB_PAGE_TYPE_BOOT       = 0; // Database definition page.  (Always page 0)
  MDB_PAGE_TYPE_DATA       = 1; // Data page
  MDB_PAGE_TYPE_TABLE_DEF  = 2; // Table definition
  MDB_PAGE_TYPE_INDEX      = 3; // Intermediate Index pages
  MDB_PAGE_TYPE_LEAF_INDEX = 4; // Leaf Index pages
  MDB_PAGE_TYPE_USAGE_BMP  = 5; // Page Usage Bitmaps (extended page usage)

  MDB_TABLE_LVAL           = $4C41564C; // 'LVAL' Long VALues table

  MDB_COL_FLAG_FIXED_LEN   = $0001; // fixed length column
  MDB_COL_FLAG_CAN_BE_NULL = $0002; // can be null (possibly related to joins?)
  MDB_COL_FLAG_AUTO_LONG   = $0004; // is auto long
  MDB_COL_FLAG_REPLICA     = $0010; // replication related field (or hidden?)
  MDB_COL_FLAG_AUTO_GUID   = $0020; // is auto guid
  MDB_COL_FLAG_HYPERLINK   = $0080; // hyperlink. Syntax is "Link Title#http://example.com/somepage.html#" or "#PAGE.HTM#"
  MDB_COL_FLAG_COMPRESSED  = $0100; // Compressed Unicode (Note: Memo is always compressed unicode!)
  MDB_COL_FLAG_MODERN      = $1000; // Marks the modern package type for OLE fields

  MDB_COL_TYPE_BOOL        = $01; // 1 bit
  MDB_COL_TYPE_BYTE        = $02; // 8 bit
  MDB_COL_TYPE_INT         = $03; // 16 bit
  MDB_COL_TYPE_LONGINT     = $04; // 32 bit
  MDB_COL_TYPE_MONEY       = $05; // 64 bit
  MDB_COL_TYPE_FLOAT       = $06; // 32 bit
  MDB_COL_TYPE_DOUBLE      = $07; // 64 bit
  MDB_COL_TYPE_DATETIME    = $08; // 64 bit
  MDB_COL_TYPE_BINARY      = $09; // 255 bytes
  MDB_COL_TYPE_TEXT        = $0A; // 255 bytes
  MDB_COL_TYPE_OLE         = $0B; // long binary
  MDB_COL_TYPE_MEMO        = $0C; // long text
  MDB_COL_TYPE_REPID       = $0F; // GUID
  MDB_COL_TYPE_NUMERIC     = $10; // Scaled decimal  (17 bytes)
  MDB_COL_TYPE_VARBIN      = $11; // Variable binary data
  MDB_COL_TYPE_COMPLEX     = $12; // 32 bit integer key

type
  TMdbVarLenRec = record
    Offs: Integer;
    Size: Integer;
    Data: AnsiString;
  end;

  TMdbRecPointer = record
    RecNum: Byte;     // 8 bit
    PageID: Integer;  // 24 bit
    Size: Integer;    // for Memo
    Flags: Byte;      // part of Size
  end;

function MdbColTypeToStr(AVal: Byte): string;
begin
  case AVal of
    MDB_COL_TYPE_BOOL:     Result := 'Boolean';
    MDB_COL_TYPE_BYTE:     Result := 'Byte';
    MDB_COL_TYPE_INT:      Result := 'ShortInt';
    MDB_COL_TYPE_LONGINT:  Result := 'Integer';
    MDB_COL_TYPE_MONEY:    Result := 'Currency';
    MDB_COL_TYPE_FLOAT:    Result := 'Single';
    MDB_COL_TYPE_DOUBLE:   Result := 'Double';
    MDB_COL_TYPE_DATETIME: Result := 'DateTime';
    MDB_COL_TYPE_BINARY:   Result := 'Binary';
    MDB_COL_TYPE_TEXT:     Result := 'Text';
    MDB_COL_TYPE_OLE:      Result := 'LongBinary';
    MDB_COL_TYPE_MEMO:     Result := 'LongText';
    MDB_COL_TYPE_REPID:    Result := 'GUID';
    MDB_COL_TYPE_NUMERIC:  Result := 'Decimal';
    MDB_COL_TYPE_VARBIN:   Result := 'VarBin';
  else
    Result := 'Unknown_' + IntToStr(AVal);
  end;
end;

function MdbColTypeToDbFieldType(AVal: Byte): TFieldType;
begin
  case AVal of
    MDB_COL_TYPE_BOOL:     Result := ftBoolean;
    MDB_COL_TYPE_BYTE:     Result := ftSmallint;
    MDB_COL_TYPE_INT:      Result := ftInteger;
    MDB_COL_TYPE_LONGINT:  Result := ftInteger;
    MDB_COL_TYPE_MONEY:    Result := ftCurrency;
    MDB_COL_TYPE_FLOAT:    Result := ftFloat;
    MDB_COL_TYPE_DOUBLE:   Result := ftFloat;
    MDB_COL_TYPE_DATETIME: Result := ftDateTime;
    MDB_COL_TYPE_BINARY:   Result := ftBytes;
    MDB_COL_TYPE_TEXT:     Result := ftString;
    MDB_COL_TYPE_OLE:      Result := ftBlob;
    MDB_COL_TYPE_MEMO:     Result := ftMemo;
    MDB_COL_TYPE_REPID:    Result := ftGuid;
    MDB_COL_TYPE_NUMERIC:  Result := ftBCD;
    MDB_COL_TYPE_VARBIN:   Result := ftVarBytes;
  else
    Result := ftUnknown;
  end;
end;

function GetNullTerminatedText(AData: AnsiString): AnsiString;
var
  i, iLen: Integer;
begin
  Result := AData;
  iLen := Length(AData);
  i := 0;
  while i < iLen do
  begin
    Inc(i);
    if AData[i] = #0 then
    begin
      if (i < iLen) and (AData[i+1] = #0) then
      begin
        Result := Copy(AData, 1, i-1);
        Exit;
      end;
    end;
  end;
end;

procedure TDBReaderMdb.AfterConstruction;
begin
  inherited;
  FTableList := TMdbTableInfoList.Create();
end;

procedure TDBReaderMdb.BeforeDestruction;
begin
  FreeAndNil(FTableList);
  inherited;
end;

function TDBReaderMdb.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TMdbTableInfo;
  s: string;
begin
  Result := False;
  TmpTable := FTableList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  ALines.Add(Format('== Table Name=%s  RowCount=%d  Type=%s', [TmpTable.TableName, TmpTable.RowCount, TmpTable.TableType]));
  ALines.Add(Format('PageID=%s', [IntToHex(TmpTable.TableID, 16)]));
  ALines.Add(Format('VarColCount=%d', [TmpTable.VarColCount]));
  ALines.Add(Format('== Fields  Count=%d', [Length(TmpTable.FieldsDef)]));
  for i := Low(TmpTable.FieldsDef) to High(TmpTable.FieldsDef) do
  begin
    s := Format('%.2d Name=%s  Type=%s', [i, TmpTable.FieldsDef[i].Name, TmpTable.FieldsDef[i].TypeName]);
    ALines.Add(s);
  end;

  ALines.Add(Format('== FieldInfo  Count=%d', [Length(TmpTable.FieldInfoArr)]));
  for i := Low(TmpTable.FieldInfoArr) to High(TmpTable.FieldInfoArr) do
  begin
    s := Format('%.2d Name=%s  Type=%s  Length=%d  Misc=$%x  ID=%d  Num=%d  OffsF=%d  OffsV=%d  Flags=$%x',
      [i,
        TmpTable.FieldInfoArr[i].ColName,
        MdbColTypeToStr(TmpTable.FieldInfoArr[i].ColType),
        TmpTable.FieldInfoArr[i].ColLen,
        TmpTable.FieldInfoArr[i].Misc,
        TmpTable.FieldInfoArr[i].ColID,
        TmpTable.FieldInfoArr[i].ColNum,
        TmpTable.FieldInfoArr[i].OffsetF,
        TmpTable.FieldInfoArr[i].OffsetV,
        TmpTable.FieldInfoArr[i].Flags
      ]);
    ALines.Add(s);
  end;
end;

procedure TDBReaderMdb.FillTablesList;
var
  i, ii, id: Integer;
  TmpTable, SysObjTable: TMdbTableInfo;
  TmpRow: TDbRowItem;
begin
  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);

    // -- pages list
    SetLength(TmpTable.PageIdArr, TmpTable.PageIdCount);
    // -- fields defs
    SetLength(TmpTable.FieldsDef, Length(TmpTable.FieldInfoArr));
    for ii := 0 to Length(TmpTable.FieldInfoArr) - 1 do
    begin
      // order by id
      id := TmpTable.FieldInfoArr[ii].ColID;
      if (id >= Low(TmpTable.FieldsDef)) and (id <= High(TmpTable.FieldsDef)) then
      begin
        TmpTable.FieldsDef[id].Name := TmpTable.FieldInfoArr[ii].ColName;
        TmpTable.FieldsDef[id].TypeName := MdbColTypeToStr(TmpTable.FieldInfoArr[ii].ColType);
        TmpTable.FieldsDef[id].FieldType := MdbColTypeToDbFieldType(TmpTable.FieldInfoArr[ii].ColType);
        TmpTable.FieldsDef[id].Size := TmpTable.FieldInfoArr[ii].ColLen;
        TmpTable.FieldsDef[id].RawOffset := TmpTable.FieldInfoArr[ii].OffsetF;
      end;
    end;
  end;

  FIsMetadataLoaded := True; // allow reading tables data

  // read MSysObjects table
  SysObjTable := TableList.GetByID(2);
  ReadTable(SysObjTable.TableName, MaxInt, SysObjTable);

  // fill tables names
  for i := 0 to SysObjTable.Count - 1 do
  begin
    TmpRow := SysObjTable.GetItem(i);
    TmpTable := nil;
    if VarIsOrdinal(TmpRow.Values[0]) then
      TmpTable := TableList.GetByID(TmpRow.Values[0]);
    if Assigned(TmpTable) then
      TmpTable.TableName := VarToStrDef(TmpRow.Values[2], TmpTable.TableName);
  end;
end;

function TDBReaderMdb.GetBlobData(var APageID, ARowID, ASize: Integer; var AData: AnsiString): Boolean;
var
  rdr: TRawDataReader;
  iPageType, iPageID, iRecCount, iPrevRecOffs: Integer;
  i, iRecOffs, iRecSize: Integer;
  //RecPtr: TMdbRecPointer;
  //sData: AnsiString;
begin
  Result := False;
  if FBlobRawPageID <> APageID then
  begin
    SetLength(FBlobRawPage, FPageSize);
    if (APageID * FPageSize) + FPageSize <= FFile.Size then
    begin
      FFile.Position := APageID * FPageSize;
      FFile.Read(FBlobRawPage[0], FPageSize);
      FBlobRawPageID := APageID;
    end
    else
    begin
      AData := '<Blob page out of file>';
      Exit;
    end;
  end;

  //
  rdr.Init(FBlobRawPage[0]);
  iPageType := rdr.ReadUInt16; // Page type
  rdr.ReadUInt16; // Free space in this page
  iPageID := rdr.ReadInt32; // Page pointer to table definition
  if Byte(iPageType) <> MDB_PAGE_TYPE_DATA then
  begin
    Assert(False, 'Page not DATA: ' + IntToStr(iPageID));
    AData := 'Page not DATA: ' + IntToStr(iPageID);
    Exit;
  end;
  Assert(iPageID = MDB_TABLE_LVAL, 'Page not LVAL: ' + IntToStr(iPageID));
  //if nPageID <> MDB_TABLE_LVAL then
  //  Exit;

  if FFileInfo.JetVersion >= 1 then
    rdr.ReadUInt32;
  iRecCount := rdr.ReadUInt16; // number of records on this page
  iPrevRecOffs := FPageSize;

  for i := 0 to iRecCount - 1 do
  begin
    iRecOffs := rdr.ReadUInt16; // The record's location on this page
    iRecSize := iPrevRecOffs - iRecOffs;
    iPrevRecOffs := iRecOffs;

    if i <> ARowID then
      Continue;

    if (iRecOffs and $4000) <> 0 then // Data Pointer (4 bytes) to another data page
    else
    if (iRecOffs and $8000) <> 0 then // Deleted row or referenced from Data Pointer
      Exit
    else
    begin
      rdr.SetPosition(iRecOffs);
      if ((ASize and $40000000) <> 0) or (ASize = iRecSize) then  // type 1 (pure data)
      begin
        APageID := 0;
        ARowID := 0;
        if iRecOffs + iRecSize < FPageSize then
          AData := rdr.ReadBytes(iRecSize)
        else
        begin
          AData := Format('Blob offs:size=%d:%d > page size=%d', [iRecOffs, iRecSize, FPageSize]);
          Exit;
        end;
      end
      else  // type 2 (32bit pointer + data)
      begin
        APageID := rdr.ReadInt32();
        ARowID := Byte(APageID);
        APageID := APageID shr 8;

        AData := rdr.ReadBytes(iRecSize - 4);
      end;
      Result := True;
      Exit;
    end;
  end;
end;

function TDBReaderMdb.GetDecompressedText(AStr: AnsiString): string;
var
  nPos, nStart: Integer;
  IsCompressed: Boolean;
begin
  if Copy(AStr, 1, 2) <> #$FF#$FE then
  begin
    Result := AStr;
    Exit;
  end;
  nPos := 3;
  nStart := nPos;
  IsCompressed := True;
  Result := '';
  while nPos < Length(AStr) do
  begin
    if AStr[nPos] = #0 then
    begin
      if IsCompressed then
      begin
        Result := Result + Copy(AStr, nStart, nPos-nStart);
        nStart := nPos + 1;
      end
      else if AStr[nPos+1] = #0 then
      begin
        Result := Result + WideDataToStr(Copy(AStr, nStart, nPos-nStart));
        nStart := nPos + 2;
    end;
      IsCompressed := not IsCompressed;
    end;
    Inc(nPos);
  end;
  // rest of string
  if IsCompressed then
    Result := Result + Copy(AStr, nStart, MaxInt)
  else
    Result := Result + WideDataToStr(Copy(AStr, nStart, MaxInt));
end;

function TDBReaderMdb.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList.GetItem(AIndex);
end;

function TDBReaderMdb.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

function TDBReaderMdb.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  rdr: TRawDataReader;
  iPageType: Word;
  iPagePos: Int64;
  iCurPageID: Cardinal;
  TableInfo: TMdbTableInfo;
  s: string;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;

  FIsMetadataLoaded := False;
  FBlobRawPageID := 0;
  TableList.Clear();

  // read file info
  FPageSize := $400; // 1K
  SetLength(PageBuf, FPageSize);
  FFIle.Read(PageBuf[0], FPageSize);
  rdr.Init(PageBuf[0]);

  with FFileInfo do
  begin
    MagicNumber := rdr.ReadUInt32();
    FileFormatID := rdr.ReadBytes(16);
    JetVersion := rdr.ReadUInt32();
    rdr.ReadBytes(34); // skip 34 bytes
    SystemCollation := rdr.ReadUInt16();
    SystemCodePage := rdr.ReadUInt16();
    DatabaseKey := rdr.ReadUInt32();
    if JetVersion = 0 then
      DatabasePassword := rdr.ReadBytes(20)
    else
      DatabasePassword := rdr.ReadBytes(40);
  end;

  case FFileInfo.JetVersion of
    0: FPageSize := $0800; // JET3 / 2K
  else
    FPageSize := $1000; // JET4 / 4K
  end;
  SetLength(PageBuf, FPageSize);

  // version info
  s := 'unknown';
  case FFileInfo.JetVersion of
    0: s := 'Jet 3';
    1: s := 'Jet 4';
    2: s := 'Jet 5';
    3: s := 'Access 2010';
    4: s := 'Access 2013';
    5: s := 'Access 2016';
    6: s := 'Access 2019';
  end;
  LogInfo('File Version: ' + s);

  // scan pages
  iCurPageID := 0;
  iPagePos := 0;
  while iPagePos + FPageSize <= FFile.Size do
  begin
    FFile.Position := iPagePos;
    FFile.Read(PageBuf[0], FPageSize);
    rdr.Init(PageBuf[0]);
    // header
    iPageType := rdr.ReadUInt8;

    if iPageType = MDB_PAGE_TYPE_DATA then
    begin
      ReadDataPage(PageBuf, iPagePos, nil, nil);
    end
    else
    if iPageType = MDB_PAGE_TYPE_TABLE_DEF then
    begin
      // table definition page
      rdr.ReadUInt8; // unknown
      rdr.ReadUInt16; // Free space in this page minus 8
      rdr.ReadUInt32; // Next tdef page pointer (0 if none)

      TableInfo := TableList.GetByID(iCurPageID);
      if not Assigned(TableInfo) then
      begin
        TableInfo := TMdbTableInfo.Create();
        TableInfo.TableID := iCurPageID;
        TableInfo.TableName := Format('TabID_%x', [iCurPageID]);
        if iCurPageID = MDB_TABLE_LVAL then
          TableInfo.TableName := 'LVAL';
        TableList.Add(TableInfo);
      end;

      if FFileInfo.JetVersion = 0 then // JET3
        ReadTabDefJet3(rdr, TableInfo)
      else
      //if FFileInfo.JetVersion = 1 then // JET4
        ReadTabDefJet4(rdr, TableInfo);

    end;

    Inc(iCurPageID);
    iPagePos := iCurPageID * FPageSize;

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

  SetLength(PageBuf, 0);

  FillTablesList();
end;

function TDBReaderMdb.ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
  ATableInfo: TMdbTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  iPageID, iCurPageID, iPageType: Cardinal;
  TableInfo: TMdbTableInfo;
  i, iRecCount, iRecOffs, iPrevRecOffs, iRecSize: Integer;
begin
  Result := False;
  iCurPageID := APagePos div FPageSize;
  rdr.Init(APageBuf[0]);
  iPageType := rdr.ReadUInt16; // Page type
  if Byte(iPageType) <> MDB_PAGE_TYPE_DATA then
  begin
    Assert(False, 'Page not DATA: ' + IntToStr(iCurPageID));
    Exit;
  end;
  rdr.ReadUInt16; // Free space in this page
  iPageID := rdr.ReadUInt32; // Page pointer to table definition
  if FFileInfo.JetVersion >= 1 then
    rdr.ReadUInt32;
  iRecCount := rdr.ReadUInt16; // number of records on this page
  iPrevRecOffs := FPageSize;

  TableInfo := TableList.GetByID(iPageID);
  if not Assigned(TableInfo) then
  begin
    TableInfo := TMdbTableInfo.Create();
    TableInfo.TableID := iPageID;
    TableInfo.TableName := Format('TabID_%x', [iPageID]);
    TableList.Add(TableInfo);
  end;
  if not FIsMetadataLoaded then
  begin
    // store PageID
    Inc(TableInfo.PageIdCount);
    if TableInfo.PageIdCount >= Length(TableInfo.PageIdArr) then
      SetLength(TableInfo.PageIdArr, Length(TableInfo.PageIdArr) + 32);
    TableInfo.PageIdArr[TableInfo.PageIdCount-1] := iCurPageID;
  end
  else
  begin
    for i := 0 to iRecCount - 1 do
    begin
      iRecOffs := rdr.ReadUInt16; // The record's location on this page
      iRecSize := iPrevRecOffs - iRecOffs;
      iPrevRecOffs := iRecOffs;

      if (iRecOffs and $4000) <> 0 then // Data Pointer (4 bytes) to another data page
      else
      if (iRecOffs and $8000) <> 0 then // Deleted row or referenced from Data Pointer
        Continue
      else
      begin
        ReadRowData(APageBuf, iRecOffs, iRecSize, TableInfo, AList);
      end;
    end;
  end;
  Result := True;
end;

function IsNullValue(const ANullBmp: TByteDynArray; AIndex: Integer): Boolean;
var
  NullFlagIndex, NullFlagOffs: Integer;
begin
  NullFlagIndex := (AIndex div 8);
  NullFlagOffs := AIndex mod 8;
  if NullFlagIndex < Length(ANullBmp) then
    Result := (ANullBmp[NullFlagIndex] and (Byte(1) shl NullFlagOffs)) = 0
  else
    Result := False;
end;

function TDBReaderMdb.ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
  ATableInfo: TMdbTableInfo; AList: TDbRowsList): Boolean;
var
  rdr, rdr2: TRawDataReader;
  nFieldCount, nVarLenCount, nVarLenIndex, nPrevOffs, nBaseOffs, nFixedSize: Integer;
  i, iBitmapLen, iColId, iColOffs, iColLen: Integer;
  nPageID, nRecID, nSize: Integer;
  TmpRow: TDbRowItem;
  NullBmp: TByteDynArray;
  VarLenArr: array of TMdbVarLenRec;
  TmpGuid: TGUID;
  sData, sData2: AnsiString;
  //RecPtr: TMdbRecPointer;
  IsNull: Boolean;
begin
  Result := False;
  Assert(Assigned(ATableInfo), 'TableInfo not assigned');
  Assert(Assigned(AList), 'RowsList not assigned');
  rdr.Init(APageBuf[ARowOffs]);

  // -- Record structure --   Jet3  Jet4
  // Field Count              1     2
  // Fixed Length fields      n     n
  // Variable length fields   n     n
  // Total data length        1     2    size of above
  // VarLen fields offsets    1     2    per field, in reverse order
  // VarLen jump table        1     -    Jet3 only
  // VarLen field count       1     2
  // Null bitmap              n     n    0-NULL

  if FFileInfo.JetVersion = 0 then
    nFieldCount := rdr.ReadUInt8
  else
    nFieldCount := rdr.ReadUInt16;

  if nFieldCount <> Length(ATableInfo.FieldInfoArr) then
    LogInfo(Format('!FieldCount=%d, FieldInfoCount=%d', [nFieldCount, Length(ATableInfo.FieldInfoArr)]));

  // Null bitmap
  iBitmapLen := ((nFieldCount + 7) div 8); // bytes count
  SetLength(NullBmp, iBitmapLen);
  if iBitmapLen > 0 then
  begin
    rdr.SetPosition(ARowSize - iBitmapLen);
    rdr.ReadToBuffer(NullBmp[0], iBitmapLen);
  end;
  // VarLen field count
  if FFileInfo.JetVersion = 0 then // Jet3
  begin
    rdr.SetPosition(ARowSize - iBitmapLen - 1);
    nVarLenCount := rdr.ReadUInt8;
  end
  else // Jet4
  begin
    rdr.SetPosition(ARowSize - iBitmapLen - 2);
    nVarLenCount := rdr.ReadUInt16;
  end;
  if nVarLenCount > ATableInfo.VarColCount then
  begin
    LogInfo(Format('!VarLenCount=%d  TableInfo.VarLenCount=%d', [nVarLenCount, ATableInfo.VarColCount]));
    Exit;
  end;
  // -- VarLen offsets
  SetLength(VarLenArr, nVarLenCount);
  if FFileInfo.JetVersion = 0 then  // Jet3
  begin
    rdr.SetPosition(ARowSize - iBitmapLen - 1 - nVarLenCount - 1);
    nFixedSize := rdr.ReadUInt8;
    nPrevOffs := nFixedSize;
    for i := nVarLenCount - 1 downto 0 do
    begin
      VarLenArr[i].Offs := rdr.ReadUInt8;
      VarLenArr[i].Size := nPrevOffs - VarLenArr[i].Offs;
      nPrevOffs := VarLenArr[i].Offs;
    end;
  end
  else  // Jet4
  begin
    rdr.SetPosition(ARowSize - iBitmapLen - 2 - (nVarLenCount * 2) - 2);
    nFixedSize := rdr.ReadUInt16;
    nPrevOffs := nFixedSize;
    for i := nVarLenCount - 1 downto 0 do
    begin
      VarLenArr[i].Offs := rdr.ReadUInt16;
      VarLenArr[i].Size := nPrevOffs - VarLenArr[i].Offs;
      nPrevOffs := VarLenArr[i].Offs;
    end;
  end;
  // -- VarLen data
  for i := 0 to nVarLenCount - 1 do
  begin
    Assert(VarLenArr[i].Offs < FPageSize, 'VarLenOffs > PageSize');
    rdr.SetPosition(VarLenArr[i].Offs);
    VarLenArr[i].Data := rdr.ReadBytes(VarLenArr[i].Size);
    if IsDebugRows then
      LogInfo(Format('VarLen[%d] Offs=%d Size=%d Data=%s', [i, VarLenArr[i].Offs, VarLenArr[i].Size, DataAsStr(PChar(VarLenArr[i].Data)^, VarLenArr[i].Size)]));
  end;

  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);
  SetLength(TmpRow.Values, Length(ATableInfo.FieldInfoArr));
  if IsDebugRows then
  begin
    rdr.SetPosition(0);
    TmpRow.RawData := rdr.ReadBytes(ARowSize);
  end;
  //nVarLenIndex := 0;
  if FFileInfo.JetVersion = 0 then  // Jet3
    nBaseOffs := 1
  else
    nBaseOffs := 2;
  nPrevOffs := nBaseOffs;

  // -- fixed length fields
  for i := 0 to Length(ATableInfo.FieldInfoArr) - 1 do
  begin
    iColId := ATableInfo.FieldInfoArr[i].ColID;
    iColLen := ATableInfo.FieldInfoArr[i].ColLen;
    iColOffs := ATableInfo.FieldInfoArr[i].OffsetF + nBaseOffs;
    if (iColOffs > FPageSize) then
      iColOffs := nPrevOffs;

    //IsNull := IsNullValue(NullBmp, i);
    IsNull := IsNullValue(NullBmp, iColId);
    if IsNull and (ATableInfo.FieldInfoArr[i].ColType <> MDB_COL_TYPE_BOOL) then
    begin
      TmpRow.Values[iColId] := Null;
      Continue;
    end
    else
      TmpRow.Values[iColId] := '<undef>';

    rdr.SetPosition(iColOffs);
    //rdr.SetPosition(ATableInfo.FieldInfoArr[i].OffsetF);
    //nColNum := ATableInfo.FieldInfoArr[i].ColNum;

    // fixed types can be in variable sizes
    if (ATableInfo.FieldInfoArr[i].Flags and MDB_COL_FLAG_FIXED_LEN = 0) then
    begin
      nVarLenIndex := ATableInfo.FieldInfoArr[i].OffsetV;
      if (nVarLenIndex >= 0) and (nVarLenIndex < nVarLenCount) then
        rdr.SetPosition(VarLenArr[nVarLenIndex].Offs);
    end;

    case ATableInfo.FieldInfoArr[i].ColType of
      MDB_COL_TYPE_BOOL:   // encoded in Null bitmap
        TmpRow.Values[iColId] := IsNull;
      MDB_COL_TYPE_BYTE:
        TmpRow.Values[iColId] := rdr.ReadUInt8();
      MDB_COL_TYPE_INT:
        TmpRow.Values[iColId] := rdr.ReadInt16();
      MDB_COL_TYPE_LONGINT:
        TmpRow.Values[iColId] := rdr.ReadInt32();
      MDB_COL_TYPE_MONEY:
        TmpRow.Values[iColId] := rdr.ReadCurrency();
      MDB_COL_TYPE_FLOAT:
        TmpRow.Values[iColId] := rdr.ReadSingle();
      MDB_COL_TYPE_DOUBLE:
        TmpRow.Values[iColId] := rdr.ReadDouble();
      MDB_COL_TYPE_DATETIME:
        TmpRow.Values[iColId] := TDateTime(rdr.ReadDouble());
      MDB_COL_TYPE_BINARY,
      MDB_COL_TYPE_TEXT:
      begin
        if (ATableInfo.FieldInfoArr[i].Flags and MDB_COL_FLAG_FIXED_LEN = 0)
        or ((iColOffs + iColLen) > nFixedSize) then
        begin
          // VarLen data
          nVarLenIndex := ATableInfo.FieldInfoArr[i].OffsetV;
          if (nVarLenIndex >= 0) and (nVarLenIndex < nVarLenCount) then
            sData := VarLenArr[nVarLenIndex].Data
          else
            sData := '';
        end
        else
          sData := rdr.ReadBytes(iColLen);

        if (ATableInfo.FieldInfoArr[i].ColType = MDB_COL_TYPE_TEXT) then
        begin
          if Copy(sData, 1, 2) = #$FF#$FE then  // compressed
          begin
            sData := GetDecompressedText(sData);  // UTF8Decode(sData)
            {$ifdef FPC}
            sData := AnsiToUtf8(sData);
            {$endif}
          end
          else if FFileInfo.JetVersion > 0 then
            sData := WideDataToStr(sData);
        end;
        TmpRow.Values[iColId] := sData;
      end;
      MDB_COL_TYPE_OLE,
      MDB_COL_TYPE_MEMO:
      begin
        if iColLen = 0 then
        begin
          // VarLen data
          sData := '';
          nVarLenIndex := ATableInfo.FieldInfoArr[i].OffsetV;
          if (nVarLenIndex >= 0) and (nVarLenIndex < nVarLenCount) then
          begin
            sData := VarLenArr[nVarLenIndex].Data;
            Inc(nVarLenIndex);
          end;
          nPageID := 0;
          if Length(sData) >= 12 then
          begin
            rdr2.Init(sData[1]);
            // -- Blob pointer --
            // Field Length   32 bit    plus flags
            // Data Location  32 bit    pointer to VLAV page
            // Reserved       32 bit
            nSize := rdr2.ReadInt32;
            nPageID := rdr2.ReadInt32;
            nRecID := Byte(nPageID);
            nPageID := nPageID shr 8;
            rdr2.ReadInt32;

            // rest of data
            sData := Copy(sData, 13, MaxInt);
          end;

          //if sData = '' then
          //  sData := Format('<blob page=$%x rec=%d size=%d>', [RecPtr.PageID, RecPtr.RecNum, (RecPtr.Size and $0FFFFFFF)])
          //else
          while nPageID <> 0 do
          begin
            sData2 := '';
            if not GetBlobData(nPageID, nRecID, nSize, sData2) then
            begin
              LogInfo('!' + sData2);
            end;
            sData := sData + sData2;
          end;

          if (ATableInfo.FieldInfoArr[i].ColType = MDB_COL_TYPE_MEMO) then
          begin
            if Copy(sData, 1, 2) = #$FF#$FE then  // compressed
              sData := GetDecompressedText(sData)  // UTF8Decode(sData)
            else if FFileInfo.JetVersion > 0 then
              sData := WideDataToStr(sData);
          end;

          TmpRow.Values[iColId] := sData;

        end
        else
        begin
          // Fixed len data
          TmpRow.Values[iColId] := Format('<blob fix len=%d>', [iColLen]);
        end;

      end;
      MDB_COL_TYPE_VARBIN:
      begin
        if (ATableInfo.FieldInfoArr[i].Flags and MDB_COL_FLAG_FIXED_LEN = 0)
        or ((iColOffs + iColLen) > nFixedSize) then
        begin
          Assert(False, 'VarBin not fit in fixed size');
          Continue;
        end;
        sData := rdr.ReadBytes(iColLen);
        TmpRow.Values[iColId] := sData;
        {rdr2.Init(sData[1]);
        TmpRow.Values[nColId] := rdr2.ReadBytes(RecPtr.Size);
        // <data>
        rdr2.SetPosition(nColLen-3);
        RecPtr.Size := rdr2.ReadUInt16;
        if RecPtr.Size < nColLen-2 then
          TmpRow.Values[nColId] := rdr2.ReadBytes(RecPtr.Size); }
      end;
      MDB_COL_TYPE_REPID:
      begin
        rdr.ReadToBuffer(TmpGuid, SizeOf(TmpGuid));
        TmpRow.Values[iColId] := GUIDToString(TmpGuid);
      end;
      MDB_COL_TYPE_NUMERIC:
      begin
        TmpRow.Values[iColId] := rdr.ReadBytes(17);
      end;
      //MDB_COL_TYPE_COMPLEX     = $12; // 32 bit integer key
    else
      rdr.ReadBytes(iColLen);
    end;
    nPrevOffs := rdr.GetPosition();
  end;

end;

procedure TDBReaderMdb.ReadTabDefJet3(rdr: TRawDataReader; ATableInfo: TMdbTableInfo);
var
  i, nIdxCount, nIdxRealCount, nColCount, nNameLen, nColNum: Integer;
begin
  // JET3
  rdr.ReadInt32;  // Length of the data for this page
  ATableInfo.RowCount := rdr.ReadInt32;  // Number of records in this table
  rdr.ReadInt32;  // Value for the next autonumber
  ATableInfo.TableType := Chr(rdr.ReadUInt8);  // 0x4e: user table, 0x53: system table
  rdr.ReadUInt16; // Max columns a row will have (deletions)
  ATableInfo.VarColCount := rdr.ReadUInt16; // Number of variable columns in table
  nColCount := rdr.ReadUInt16; // Number of columns in table (repeat)
  nIdxCount := rdr.ReadInt32;  // Number of logical indexes in table
  nIdxRealCount := rdr.ReadInt32;  // Number of index entries
  rdr.ReadInt32;  // Points to a record containing the usage bitmask for this table.
  rdr.ReadInt32;  // Points to a similar record as above, listing pages which contain free space.
  // -- indexes
  for i := 0 to nIdxRealCount - 1 do
    rdr.ReadInt64; // 32bit + 32bit

  // -- column definitions
  SetLength(ATableInfo.FieldInfoArr, nColCount);
  for i := 0 to nColCount - 1 do
  begin
    ATableInfo.FieldInfoArr[i].ColType := rdr.ReadUInt8;  // Column Type
    ATableInfo.FieldInfoArr[i].ColID   := rdr.ReadUInt16; // Column Number (includes deleted columns)
    ATableInfo.FieldInfoArr[i].OffsetV := rdr.ReadUInt16; // Offset for variable length columns
    ATableInfo.FieldInfoArr[i].ColNum  := rdr.ReadUInt16; // Column Number
    ATableInfo.FieldInfoArr[i].SortOrder := rdr.ReadUInt16; // textual column sort order(0x409=General)
    ATableInfo.FieldInfoArr[i].Misc    := rdr.ReadUInt16; // prec/scale (1 byte each), or code page for textual columns (0x4E4=cp1252)
    rdr.ReadUInt16; //
    ATableInfo.FieldInfoArr[i].Flags   := rdr.ReadUInt8;  // Column flags
    ATableInfo.FieldInfoArr[i].OffsetF := rdr.ReadUInt16; // Offset for fixed length columns
    ATableInfo.FieldInfoArr[i].ColLen  := rdr.ReadUInt16; // Length of the column (0 if memo)
  end;
  // -- column names
  for i := 0 to nColCount - 1 do
  begin
    nNameLen := rdr.ReadUInt8;  // len of the name of the column
    ATableInfo.FieldInfoArr[i].ColName := rdr.ReadBytes(nNameLen); // Name of the column
  end;
  // -- indexes
  for i := 0 to nIdxRealCount - 1 do
  begin
    rdr.ReadBytes(30); // Iterate 10 times for 10 possible columns
    rdr.ReadInt32;     // Points to usage bitmap for index
    rdr.ReadInt32;     // Data pointer of the index page
    rdr.ReadUInt8;     // flags
  end;
  // -- logical indexes
  for i := 0 to nIdxCount - 1 do
  begin
    rdr.ReadInt32;     // Number of the index (warn: not always in the sequential order)
    rdr.ReadInt32;     // Index into index cols list
    rdr.ReadUInt8;     // type of the other table in this fk (same values as index_type)
    rdr.ReadInt32;     // index number of other index in fk
    rdr.ReadInt32;     // page number of other table in fk
    rdr.ReadUInt8;     // flag indicating if updates are cascaded
    rdr.ReadUInt8;     // flag indicating if deletes are cascaded
    rdr.ReadUInt8;     // 0x01 if index is primary, 0x02 if foreign
  end;
  // -- logical indexes names
  for i := 0 to nIdxCount - 1 do
  begin
    nNameLen := rdr.ReadUInt8;  // len of the name of the index
    rdr.ReadBytes(nNameLen);    // Name of the index
  end;
  // -- column usage bitmaps
  repeat
    nColNum := rdr.ReadUInt16; // Column number with variable length
    rdr.ReadInt32;     // Points to a record containing the usage bitmask for this column
    rdr.ReadInt32;     // Points to a similar record as above, listing pages which contain free space.
  until nColNum = $FFFF;

end;

procedure TDBReaderMdb.ReadTabDefJet4(rdr: TRawDataReader; ATableInfo: TMdbTableInfo);
var
  i, nIdxCount, nIdxRealCount, nColCount, nNameLen: Integer;
begin
  // JET4
  rdr.ReadInt32;  // Length of the data for this page
  rdr.ReadInt32;  // unknown
  ATableInfo.RowCount := rdr.ReadInt32;  // Number of records in this table
  rdr.ReadInt32;  // Value for the next autonumber
  rdr.ReadInt32;  // 0x01 makes autonumbers work in access
  rdr.ReadInt32;  // autonumber value for complex type column(s)  (shared across all columns in the table)
  rdr.ReadInt64;  // unknown
  ATableInfo.TableType := Chr(rdr.ReadUInt8);  // 0x4e: user table, 0x53: system table
  rdr.ReadUInt16; // The Column Id that the next column to be created will have.
  ATableInfo.VarColCount := rdr.ReadUInt16; // Number of variable columns in table
  nColCount := rdr.ReadUInt16; // Number of columns in table (repeat)
  nIdxCount := rdr.ReadInt32;  // Number of logical indexes in table
  nIdxRealCount := rdr.ReadInt32;  // Number of index entries
  {used_pages} rdr.ReadInt32;  // Points to a record containing the usage bitmask for this table.
  {free_pages} rdr.ReadInt32;  // Points to a similar record as above, listing pages which contain free space.
  // -- indexes
  for i := 0 to nIdxRealCount - 1 do
  begin
    rdr.ReadInt32; //
    rdr.ReadInt32; // num_idx_rows
    rdr.ReadInt32; // 0
  end;

  // -- column definitions
  SetLength(ATableInfo.FieldInfoArr, nColCount);
  for i := 0 to nColCount - 1 do
  begin
    ATableInfo.FieldInfoArr[i].ColType := rdr.ReadUInt8;  // Column Type
    rdr.ReadInt32;                                        // matches first unknown definition block
    ATableInfo.FieldInfoArr[i].ColID   := rdr.ReadUInt16; // Column Number (includes deleted columns)
    ATableInfo.FieldInfoArr[i].OffsetV := rdr.ReadUInt16; // Offset for variable length columns
    ATableInfo.FieldInfoArr[i].ColNum  := rdr.ReadUInt16; // Column Number
    ATableInfo.FieldInfoArr[i].SortOrder := rdr.ReadUInt16; // textual column sort order(0x409=General)
    ATableInfo.FieldInfoArr[i].Misc    := rdr.ReadUInt16; // prec/scale (1 byte each), or code page for textual columns (0x4E4=cp1252)
    ATableInfo.FieldInfoArr[i].Flags   := rdr.ReadUInt16;  // Column flags
    rdr.ReadUInt32; // 0000
    ATableInfo.FieldInfoArr[i].OffsetF := rdr.ReadUInt16; // Offset for fixed length columns
    ATableInfo.FieldInfoArr[i].ColLen  := rdr.ReadUInt16; // Length of the column (0 if memo)
  end;
  // -- column names
  for i := 0 to nColCount - 1 do
  begin
    nNameLen := rdr.ReadUInt16;  // len of the name of the column
    ATableInfo.FieldInfoArr[i].ColName := WideDataToStr(rdr.ReadBytes(nNameLen)); // Name of the column (UCS-2 format)
  end;
  {
  // -- indexes
  for i := 0 to nIdxRealCount - 1 do
  begin
    rdr.ReadInt32;     // 0 or 1923
    for ii := 0 to 9 do
    begin
      rdr.ReadUInt16; // The Column Id of the indexed column, or 0xFFFF if the index doesn't have so many columns
      rdr.ReadUInt8;  // Index column flags. Only known flag is 0x01 for ascending order.
    end;
    rdr.ReadUInt32;     //
    rdr.ReadUInt32;     // Points to the first index page
    rdr.ReadUInt16;     // flags
    rdr.ReadUInt32;
    rdr.ReadUInt32;     // 0
  end;
  // -- logical indexes
  for i := 0 to nIdxCount - 1 do
  begin
    rdr.ReadInt32;     // 0 or 1625  first unknown definition block
    rdr.ReadInt32;     // The number of the index (not necessarily sequential)
    rdr.ReadInt32;     // Index into index cols list
    rdr.ReadUInt8;     // type of the other table in this fk (same values as index_type)
    rdr.ReadInt32;     // index number of other index in fk
    rdr.ReadInt32;     // Always 0 for real indices
    rdr.ReadUInt16;    // flags
    rdr.ReadUInt8;     // 0x01 if index is primary, 0x02 if foreign\
    rdr.ReadInt32;     // 0
  end;
  // -- logical indexes names
  for i := 0 to nIdxCount - 1 do
  begin
    nNameLen := rdr.ReadUInt16; // len of the name of the index
    WideDataToStr(rdr.ReadBytes(nNameLen)); // Name of the index
  end;
  // -- column usage bitmaps
  repeat
    nColID := rdr.ReadUInt16; // Column ID of variable length column
    rdr.ReadInt32;     // Points to a record containing the usage bitmask for this column
    rdr.ReadInt32;     // Points to a similar record as above, listing pages which contain free space.
  until nColNum = $FFFF;
  }
end;

procedure TDBReaderMdb.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  TmpTable: TMdbTableInfo;
  i: Integer;
  PageBuf: TByteDynArray;
  iPagePos: Int64;
begin
  TmpTable := FTableList.GetByName(AName);
  if not Assigned(TmpTable) then
  begin
    //Assert(False, 'Table not found: ' + AName);
    LogInfo('!Table not found: ' + AName);
    Exit;
  end;
  if not Assigned(AList) then
    AList := TmpTable;
  AList.Clear();
  AList.FieldsDef := TmpTable.FieldsDef;
  SetLength(PageBuf, FPageSize);
  // read pages
  for i := Low(TmpTable.PageIdArr) to High(TmpTable.PageIdArr) do
  begin
    iPagePos := TmpTable.PageIdArr[i] * FPageSize;
    if iPagePos + FPageSize <= FFile.Size then
    begin
      FFile.Position := iPagePos;
      FFile.Read(PageBuf[0], FPageSize);

      ReadDataPage(PageBuf, iPagePos, TmpTable, AList);
    end
    else
      Assert(False, 'Page out of file: ' + IntToStr(TmpTable.PageIdArr[i]));

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;
end;

{ TMdfTableInfoList }

function TMdbTableInfoList.GetByID(ATableID: Integer): TMdbTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.TableID = ATableID then
      Exit;
  end;
  Result := nil;
end;

function TMdbTableInfoList.GetByName(AName: string): TMdbTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.TableName = AName then
      Exit;
  end;
  Result := nil;
end;

function TMdbTableInfoList.GetItem(AIndex: Integer): TMdbTableInfo;
begin
  Result := TMdbTableInfo(Get(AIndex));
end;

procedure TMdbTableInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TMdbTableInfo(Ptr).Free;
end;

procedure TMdbTableInfoList.SortByName;
begin

end;

{ TMdbTableInfo }

function TMdbTableInfo.IsEmpty: Boolean;
begin
  Result := (RowCount = 0);
end;

function TMdbTableInfo.IsGhost: Boolean;
begin
  Result := (Length(FieldsDef) = 0);
end;

function TMdbTableInfo.IsSystem: Boolean;
begin
  Result := (TableType = 'S');
end;

end.
